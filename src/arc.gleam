import arc/compiler
import arc/engine
import arc/internal/path
import arc/parser
import arc/repl/examples
import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{NormalCompletion, ThrowCompletion, YieldCompletion}
import arc/vm/exec/entry
import arc/vm/exec/event_loop
import arc/vm/heap
import arc/vm/ops/object
import arc/vm/state.{type Heap}
import arc/vm/value.{type JsValue}
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/string

// -- FFI: read a line from stdin ---------------------------------------------

@external(erlang, "arc_vm_ffi", "read_line")
fn read_line(prompt: String) -> Result(String, Nil)

// -- REPL state --------------------------------------------------------------

type ReplState(host) {
  ReplState(heap: Heap(host), builtins: Builtins, env: entry.ReplEnv)
}

// -- VM error formatting -----------------------------------------------------

fn inspect_vm_error(vm_err: state.VmError) -> String {
  case vm_err {
    state.PcOutOfBounds(pc) -> "PC out of bounds: " <> int.to_string(pc)
    state.StackUnderflow(op) -> "stack underflow at " <> op
    state.Unimplemented(op) -> "unimplemented: " <> op
  }
}

// -- Eval one line -----------------------------------------------------------

fn eval(
  state: ReplState(host),
  source: String,
) -> #(ReplState(host), Result(JsValue, String)) {
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
            entry.run_and_drain_repl(
              template,
              state.heap,
              state.builtins,
              state.env,
            )
          {
            Ok(#(NormalCompletion(val, heap), env)) -> #(
              ReplState(..state, heap:, env:),
              Ok(val),
            )
            Ok(#(ThrowCompletion(val, heap), env)) -> #(
              ReplState(..state, heap:, env:),
              Error("Uncaught " <> object.format_error(val, heap)),
            )
            Ok(#(YieldCompletion(_, _), _)) ->
              panic as "YieldCompletion should not appear at REPL level"
            Ok(#(completion.AwaitCompletion(_, _), _)) ->
              panic as "AwaitCompletion should not appear at REPL level"
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

fn handle_repl_line(
  state: ReplState(host),
  line: String,
) -> option.Option(ReplState(host)) {
  let source = string.trim(line)
  case source {
    "/clear" -> {
      clear()
      Some(state)
    }

    "/heap" -> {
      io.println("Usage: `/heap <expression>`")
      Some(state)
    }

    "/heap " <> source -> {
      let #(new_state, result) = eval(state, source)

      case result {
        Ok(val) -> {
          heap.info_about_jsvalue(new_state.heap, val)
          |> option.map(value.heap_slot_to_string)
          |> option.unwrap("none")
          |> io.println
        }
        Error(err) -> io.println(err)
      }

      Some(new_state)
    }

    "/exit" -> {
      io.println("Goodbye!")
      None
    }

    "/reset" -> {
      clear()
      banner()
      Some(new_repl_state())
    }

    "/help" -> {
      io.println("    /clear          - clear the console")
      io.println("    /help           - show this message")
      io.println("    /reset          - reset the REPL state")
      io.println("    /examples [n]   - list or run built-in demos")
      io.println("    /exit           - exit the REPL")
      Some(state)
    }

    "/examples" -> {
      examples.print_list()
      Some(state)
    }

    "/examples " <> arg ->
      case int.parse(string.trim(arg)) {
        Error(Nil) -> {
          io.println("Usage: `/examples <n>` (try `/examples` for the list)")
          Some(state)
        }
        Ok(n) ->
          case examples.get(n) {
            None -> {
              io.println(
                "No example " <> int.to_string(n) <> ". Try `/examples`.",
              )
              Some(state)
            }
            Some(ex) -> {
              examples.print_source(ex)
              let #(new_state, result) = eval(state, ex.source)
              case result {
                Ok(_) -> Nil
                Error(err) -> io.println(err)
              }
              io.println("")
              Some(new_state)
            }
          }
      }

    "" -> Some(state)

    _ -> {
      let #(new_state, result) = eval(state, source)
      case result {
        Ok(val) -> io.println(object.inspect(val, new_state.heap))
        Error(err) -> io.println(err)
      }
      Some(new_state)
    }
  }
}

fn repl_loop(state: ReplState(host)) -> Nil {
  case read_line("> ") {
    Error(Nil) -> {
      io.println("")
      Nil
    }

    Ok(line) -> {
      case handle_repl_line(state, line) {
        Some(next) -> repl_loop(next)
        None -> Nil
      }
    }
  }
}

@external(erlang, "arc_vm_ffi", "get_script_args")
fn get_script_args() -> List(String)

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(String, FileError)

type FileError

/// Run a JS source file and print the result (or error). `prepare` is applied
/// to each freshly booted State before its top level executes; `finish` drains
/// microtasks (and any embedder macrotask loop) after it returns.
fn run_file(
  path: String,
  prepare: fn(state.State(host)) -> state.State(host),
  finish: fn(state.State(host)) -> state.State(host),
) -> Nil {
  case read_file(path) {
    Error(err) ->
      io.println("Error reading " <> path <> ": " <> string.inspect(err))
    Ok(source) -> {
      let is_module = !string.ends_with(path, ".cjs")
      case is_module {
        True -> run_module_file(path, source, prepare, finish)
        False -> run_script_file(source, prepare, finish)
      }
    }
  }
}

/// Run a file as an ES module using the bundle lifecycle.
fn run_module_file(
  path: String,
  source: String,
  prepare: fn(state.State(host)) -> state.State(host),
  finish: fn(state.State(host)) -> state.State(host),
) -> Nil {
  let eng = engine.new()
  case
    engine.eval_module_prepared_with(
      eng,
      path,
      source,
      resolve_dep,
      load_dep,
      prepare,
      finish,
    )
  {
    Ok(_) -> Nil
    Error(err) -> io.println(engine.eval_error_message(err))
  }
}

/// Resolve a dependency specifier: relative paths (./foo, ../bar) against
/// the parent module's directory.
fn resolve_dep(
  raw_specifier: String,
  parent_specifier: String,
) -> Result(String, String) {
  Ok(path.resolve_specifier(raw_specifier, parent_specifier))
}

/// Read a resolved dependency's source from disk.
fn load_dep(resolved: String) -> Result(String, String) {
  case read_file(resolved) {
    Ok(source) -> Ok(source)
    Error(err) ->
      Error(
        "file not found: " <> resolved <> " (" <> string.inspect(err) <> ")",
      )
  }
}

/// Run a file as a script (only for .cjs files).
fn run_script_file(
  source: String,
  prepare: fn(state.State(host)) -> state.State(host),
  finish: fn(state.State(host)) -> state.State(host),
) -> Nil {
  let eng = engine.new()
  case engine.eval_prepared_with(eng, source, prepare, finish) {
    Ok(#(ThrowCompletion(val, new_heap), _)) ->
      io.println("Uncaught " <> object.format_error(val, new_heap))
    Ok(_) -> Nil
    Error(err) -> io.println(engine.eval_error_message(err))
  }
}

fn new_repl_state() -> ReplState(host) {
  let eng = engine.new()
  ReplState(
    heap: engine.heap(eng),
    builtins: engine.builtins(eng),
    env: entry.new_repl_env(engine.global(eng)),
  )
}

pub fn main() -> Nil {
  case get_script_args() {
    ["-p", ..rest] -> {
      let rest = string.join(rest, " ")
      new_repl_state() |> handle_repl_line(rest)
      Nil
    }

    [path, ..] -> run_file(path, fn(s) { s }, event_loop.finish)

    [] -> {
      banner()
      new_repl_state() |> repl_loop()
    }
  }
}
