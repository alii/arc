import arc/compiler
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/common.{type Builtins}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, ArrayObject, DataProperty, FunctionObject,
  GeneratorObject, NativeFunction, ObjectSlot, OrdinaryObject, PromiseObject,
}
import arc/vm/vm
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleam/string

// -- FFI: read a line from stdin ---------------------------------------------

@external(erlang, "arc_vm_ffi", "read_line")
fn read_line(prompt: String) -> Result(String, Nil)

// -- REPL state --------------------------------------------------------------

type ReplState {
  ReplState(heap: Heap, builtins: Builtins, env: vm.ReplEnv)
}

// -- Inspect -----------------------------------------------------------------

/// Console-style inspect for REPL output (like Chrome DevTools, not toString).
fn inspect(h: Heap, val: JsValue) -> String {
  inspect_inner(h, val, 0, set.new())
}

/// REPL/debug value inspector (not a spec operation). Recursively renders
/// a JsValue as a human-readable string similar to Chrome DevTools output.
/// Tracks `seen` refs for cycle detection ([Circular]).
fn inspect_inner(
  h: Heap,
  val: JsValue,
  depth: Int,
  seen: set.Set(Int),
) -> String {
  case val {
    value.JsUndefined -> "undefined"
    value.JsNull -> "null"
    value.JsBool(True) -> "true"
    value.JsBool(False) -> "false"
    value.JsNumber(value.Finite(n)) -> value.js_format_number(n)
    value.JsNumber(value.NaN) -> "NaN"
    value.JsNumber(value.Infinity) -> "Infinity"
    value.JsNumber(value.NegInfinity) -> "-Infinity"
    value.JsString(s) -> "'" <> escape_string(s) <> "'"
    value.JsSymbol(sym_id) ->
      case value.well_known_symbol_description(sym_id) {
        Some(desc) -> "Symbol(" <> desc <> ")"
        None -> "Symbol()"
      }
    value.JsBigInt(value.BigInt(n)) -> int.to_string(n) <> "n"
    value.JsUninitialized -> "undefined"
    value.JsObject(ref) -> inspect_object(h, ref, depth, seen)
  }
}

fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("'", "\\'")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

/// REPL/debug helper: render an object (array, function, or plain object)
/// as a human-readable string. Dispatches on ObjectKind for format selection.
fn inspect_object(h: Heap, ref: Ref, depth: Int, seen: set.Set(Int)) -> String {
  // Cycle detection
  case set.contains(seen, ref.id) {
    True -> "[Circular]"
    False -> {
      let seen = set.insert(seen, ref.id)
      case heap.read(h, ref) {
        Some(ObjectSlot(kind:, properties:, elements:, symbol_properties:, ..)) ->
          case kind {
            ArrayObject(length:) ->
              inspect_array(h, elements, length, depth, seen)
            FunctionObject(..) -> inspect_function(properties)
            NativeFunction(_) -> inspect_function(properties)
            OrdinaryObject ->
              inspect_tagged_object(
                h,
                properties,
                symbol_properties,
                depth,
                seen,
              )
            PromiseObject(_) -> "Promise {}"
            GeneratorObject(_) ->
              inspect_tagged_object(
                h,
                properties,
                symbol_properties,
                depth,
                seen,
              )
            value.ArgumentsObject(length:) ->
              "[Arguments] " <> inspect_array(h, elements, length, depth, seen)
            value.StringObject(value: s) ->
              "[String: '" <> escape_string(s) <> "']"
            value.NumberObject(value: n) ->
              "[Number: "
              <> inspect_inner(h, value.JsNumber(n), depth, seen)
              <> "]"
            value.BooleanObject(value: True) -> "[Boolean: true]"
            value.BooleanObject(value: False) -> "[Boolean: false]"
            value.SymbolObject(value: sym) ->
              "[Symbol: "
              <> inspect_inner(h, value.JsSymbol(sym), depth, seen)
              <> "]"
          }
        _ -> "[Object]"
      }
    }
  }
}

fn inspect_array(
  h: Heap,
  elements: value.JsElements,
  length: Int,
  depth: Int,
  seen: set.Set(Int),
) -> String {
  case depth > 2 {
    True -> "[Array]"
    False -> {
      let items =
        int.range(from: 0, to: length, with: [], run: fn(acc, i) {
          let s = case js_elements.get_option(elements, i) {
            Some(v) -> inspect_inner(h, v, depth + 1, seen)
            None -> "<empty>"
          }
          list.append(acc, [s])
        })
      "[ " <> string.join(items, ", ") <> " ]"
    }
  }
}

fn inspect_function(properties: dict.Dict(String, value.Property)) -> String {
  let name = case dict.get(properties, "name") {
    Ok(DataProperty(value: value.JsString(n), ..)) -> n
    _ -> ""
  }
  case name {
    "" -> "[Function (anonymous)]"
    n -> "[Function: " <> n <> "]"
  }
}

/// Inspect an object, checking for Symbol.toStringTag to add a tag prefix.
fn inspect_tagged_object(
  h: Heap,
  properties: dict.Dict(String, value.Property),
  symbol_properties: dict.Dict(value.SymbolId, value.Property),
  depth: Int,
  seen: set.Set(Int),
) -> String {
  let tag = case dict.get(symbol_properties, value.symbol_to_string_tag) {
    Ok(DataProperty(value: value.JsString(t), ..)) -> Some(t)
    _ -> None
  }
  let body = inspect_plain_object(h, properties, depth, seen)
  case tag {
    Some(t) -> "Object [" <> t <> "] " <> body
    None -> body
  }
}

fn inspect_plain_object(
  h: Heap,
  properties: dict.Dict(String, value.Property),
  depth: Int,
  seen: set.Set(Int),
) -> String {
  case depth > 2 {
    True -> "[Object]"
    False -> {
      let entries =
        dict.to_list(properties)
        |> list.filter_map(fn(pair) {
          let #(key, prop) = pair
          case prop {
            DataProperty(value: v, enumerable: True, ..) ->
              Ok(key <> ": " <> inspect_inner(h, v, depth + 1, seen))
            _ -> Error(Nil)
          }
        })
      case entries {
        [] -> "{}"
        _ -> "{ " <> string.join(entries, ", ") <> " }"
      }
    }
  }
}

// -- VM error formatting -----------------------------------------------------

fn inspect_vm_error(vm_err: vm.VmError) -> String {
  case vm_err {
    vm.PcOutOfBounds(pc) -> "PC out of bounds: " <> int.to_string(pc)
    vm.StackUnderflow(op) -> "stack underflow at " <> op
    vm.LocalIndexOutOfBounds(idx) ->
      "local index out of bounds: " <> int.to_string(idx)
    vm.Unimplemented(op) -> "unimplemented: " <> op
  }
}

// -- Eval one line -----------------------------------------------------------

fn eval(
  state: ReplState,
  source: String,
) -> #(ReplState, Result(JsValue, String)) {
  case parser.parse(source, parser.Script) {
    Error(err) -> #(
      state,
      Error("SyntaxError: " <> parser.parse_error_to_string(err)),
    )
    Ok(program) ->
      case compiler.compile_repl(program) {
        Error(compiler.Unsupported(desc)) -> #(
          state,
          Error("compile error: unsupported " <> desc),
        )
        Error(compiler.BreakOutsideLoop) -> #(
          state,
          Error("compile error: break outside loop"),
        )
        Error(compiler.ContinueOutsideLoop) -> #(
          state,
          Error("compile error: continue outside loop"),
        )
        Ok(template) ->
          case
            vm.run_and_drain_repl(
              template,
              state.heap,
              state.builtins,
              state.env,
            )
          {
            Ok(#(vm.NormalCompletion(val, heap), env)) -> #(
              ReplState(..state, heap:, env:),
              Ok(val),
            )
            Ok(#(vm.ThrowCompletion(val, heap), env)) -> #(
              ReplState(..state, heap:, env:),
              Error("Uncaught " <> inspect(heap, val)),
            )
            Ok(#(vm.YieldCompletion(_, _), _)) ->
              panic as "YieldCompletion should not appear at REPL level"
            Error(vm_err) -> #(
              state,
              Error("InternalError: " <> inspect_vm_error(vm_err)),
            )
          }
      }
  }
}

// -- REPL loop ---------------------------------------------------------------

fn clear() -> Nil {
  io.println("\u{1b}[2J\u{1b}[H")
}

fn banner() -> Nil {
  io.println("arc -- JavaScript on the BEAM")
  io.println("Run /help for commands, Ctrl+C to exit.")
  io.println("")
}

fn repl_loop(state: ReplState) -> Nil {
  case read_line("> ") {
    Error(Nil) -> {
      io.println("")
      Nil
    }
    Ok(line) -> {
      let source = string.trim(line)
      case source {
        "/clear" -> {
          clear()
          repl_loop(state)
        }

        "/exit" -> {
          io.println("Goodbye!")
          Nil
        }

        "/reset" -> {
          let h = heap.new()
          let #(h, b) = builtins.init(h)
          let #(h, globals) = builtins.globals(b, h)
          let env =
            vm.ReplEnv(
              globals:,
              closure_templates: dict.new(),
              const_globals: set.new(),
              next_symbol_id: 100,
              symbol_descriptions: dict.new(),
            )
          let state = ReplState(heap: h, builtins: b, env:)
          clear()
          banner()
          repl_loop(state)
        }

        "/help" -> {
          io.println("    /clear - clear the console")
          io.println("    /help  - show this message")
          io.println("    /reset - reset the REPL state")
          io.println("    /exit  - exit the REPL")
          repl_loop(state)
        }

        "" -> repl_loop(state)

        _ -> {
          let #(new_state, result) = eval(state, source)
          case result {
            Ok(val) -> io.println(inspect(new_state.heap, val))
            Error(err) -> io.println(err)
          }
          repl_loop(new_state)
        }
      }
    }
  }
}

pub fn main() -> Nil {
  banner()
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, globals) = builtins.globals(b, h)

  repl_loop(ReplState(
    heap: h,
    builtins: b,
    env: vm.ReplEnv(
      globals:,
      closure_templates: dict.new(),
      const_globals: set.new(),
      next_symbol_id: 100,
      symbol_descriptions: dict.new(),
    ),
  ))
}
