import arc/compiler
import arc/engine.{Threw}
import arc/internal/path
import arc/parser
import arc/repl/examples
import arc/vm/builtins/common.{type Builtins}
import arc/vm/exec/entry
import arc/vm/exec/event_loop
import arc/vm/heap
import arc/vm/ops/object
import arc/vm/state.{type Heap, vm_error_message}
import arc/vm/value.{type JsValue}
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import simplifile

// -- FFI: read a line from stdin ---------------------------------------------

@external(erlang, "arc_vm_ffi", "read_line")
fn read_line(prompt: String) -> Result(String, Nil)

// -- REPL state --------------------------------------------------------------

type ReplState(host) {
  ReplState(heap: Heap(host), builtins: Builtins, env: entry.ReplEnv)
}

/// Everything that can go wrong evaluating one REPL line, still carrying its
/// original typed cause. Rendered for humans only at `format_repl_error`.
type ReplError {
  ReplSyntax(parser.ParseError)
  ReplCompile(compiler.CompileError)
  ReplUncaught(value.JsValue)
  ReplInternal(state.VmError)
}

/// Render a `ReplError` at the print site. `heap` must be the heap returned by
/// the `eval` that produced the error, so a thrown value can be inspected.
fn format_repl_error(err: ReplError, heap: Heap(host)) -> String {
  case err {
    ReplSyntax(parse_err) ->
      "SyntaxError: " <> parser.parse_error_to_string(parse_err)
    ReplCompile(compile_err) ->
      "compile error: " <> compiler.error_message(compile_err)
    ReplUncaught(thrown) -> "Uncaught " <> object.format_error(thrown, heap)
    ReplInternal(vm_err) -> "InternalError: " <> vm_error_message(vm_err)
  }
}

// -- Eval one line -----------------------------------------------------------

fn eval(
  state: ReplState(host),
  source: String,
) -> #(ReplState(host), Result(JsValue, ReplError)) {
  let template = {
    use #(program, sb) <- result.try(
      parser.parse(source, parser.Script)
      |> result.map_error(ReplSyntax),
    )
    compiler.compile_repl(program, sb)
    |> result.map_error(ReplCompile)
  }

  case template {
    Error(err) -> #(state, Error(err))
    Ok(template) ->
      case
        entry.run_and_drain_repl(template, state.heap, state.builtins, state.env)
      {
        Ok(#(Ok(val), heap, env)) -> #(ReplState(..state, heap:, env:), Ok(val))
        Ok(#(Error(val), heap, env)) -> #(
          ReplState(..state, heap:, env:),
          Error(ReplUncaught(val)),
        )
        Error(vm_err) -> #(state, Error(ReplInternal(vm_err)))
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

/// What the REPL loop should do after one line: keep going with the (possibly
/// updated) state, or stop because the user asked to leave.
type ReplStep(host) {
  Continue(ReplState(host))
  Quit
}

fn handle_repl_line(state: ReplState(host), line: String) -> ReplStep(host) {
  let source = string.trim(line)
  case source {
    "/clear" -> {
      clear()
      Continue(state)
    }

    "/heap" -> {
      io.println("Usage: `/heap <expression>`")
      Continue(state)
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
        Error(err) -> io.println(format_repl_error(err, new_state.heap))
      }

      Continue(new_state)
    }

    "/exit" -> {
      io.println("Goodbye!")
      Quit
    }

    "/reset" -> {
      clear()
      banner()
      Continue(new_repl_state())
    }

    "/help" -> {
      io.println("    /clear          - clear the console")
      io.println("    /help           - show this message")
      io.println("    /reset          - reset the REPL state")
      io.println("    /examples [n]   - list or run built-in demos")
      io.println("    /exit           - exit the REPL")
      Continue(state)
    }

    "/examples" -> {
      examples.print_list()
      Continue(state)
    }

    "/examples " <> arg ->
      case int.parse(string.trim(arg)) {
        Error(Nil) -> {
          io.println("Usage: `/examples <n>` (try `/examples` for the list)")
          Continue(state)
        }
        Ok(n) ->
          case examples.get(n) {
            None -> {
              io.println(
                "No example " <> int.to_string(n) <> ". Try `/examples`.",
              )
              Continue(state)
            }
            Some(ex) -> {
              examples.print_source(ex)
              let #(new_state, result) = eval(state, ex.source)
              case result {
                Ok(_) -> Nil
                Error(err) ->
                  io.println(format_repl_error(err, new_state.heap))
              }
              io.println("")
              Continue(new_state)
            }
          }
      }

    "" -> Continue(state)

    _ -> {
      let #(new_state, result) = eval(state, source)
      case result {
        Ok(val) -> io.println(object.inspect(val, new_state.heap))
        Error(err) -> io.println(format_repl_error(err, new_state.heap))
      }
      Continue(new_state)
    }
  }
}

fn repl_loop(state: ReplState(host)) -> Nil {
  case read_line("> ") {
    Error(Nil) -> {
      io.println("")
      Nil
    }

    Ok(line) ->
      case handle_repl_line(state, line) {
        Continue(next) -> repl_loop(next)
        Quit -> Nil
      }
  }
}

@external(erlang, "arc_vm_ffi", "get_script_args")
fn get_script_args() -> List(String)

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> a

/// Run a JS source file. `Ok(Nil)` means it ran to completion with nothing
/// uncaught; every failure prints its diagnostic to stderr and returns
/// `Error(Nil)` so `main` can exit non-zero. The engine boots with its default
/// host hooks (`engine.new()`); `finish` drains microtasks (and any embedder
/// macrotask loop) after the top level returns.
fn run_file(
  path: String,
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(Nil, Nil) {
  case simplifile.read(path) {
    Error(err) -> {
      io.println_error(
        "Error reading " <> path <> ": " <> simplifile.describe_error(err),
      )
      Error(Nil)
    }
    Ok(source) -> {
      let is_module = !string.ends_with(path, ".cjs")
      case is_module {
        True -> run_module_file(path, source, finish)
        False -> run_script_file(source, finish)
      }
    }
  }
}

/// Run a file as an ES module using the bundle lifecycle.
fn run_module_file(
  path: String,
  source: String,
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(Nil, Nil) {
  let eng = engine.new()
  case
    engine.eval_module_with(eng, path, source, resolve_dep, load_dep, finish)
  {
    Ok(_) -> Ok(Nil)
    Error(err) -> {
      io.println_error(engine.eval_error_message(err))
      Error(Nil)
    }
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
  case simplifile.read(resolved) {
    Ok(source) -> Ok(source)
    Error(err) ->
      Error(
        "file not found: "
        <> resolved
        <> " ("
        <> simplifile.describe_error(err)
        <> ")",
      )
  }
}

/// Run a file as a script (only for .cjs files).
fn run_script_file(
  source: String,
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(Nil, Nil) {
  let eng = engine.new()
  case engine.eval_with(eng, source, finish) {
    Ok(#(Threw(val), eng)) -> {
      io.println_error("Uncaught " <> engine.format_error(eng, val))
      Error(Nil)
    }
    Ok(_) -> Ok(Nil)
    Error(err) -> {
      io.println_error(engine.eval_error_message(err))
      Error(Nil)
    }
  }
}

/// `arc -p <expr>`: evaluate one expression in a fresh REPL state and print
/// the result. `Error(Nil)` (and a stderr diagnostic) if the eval failed.
fn run_print(source: String) -> Result(Nil, Nil) {
  let #(new_state, result) = eval(new_repl_state(), source)
  case result {
    Ok(val) -> {
      io.println(object.inspect(val, new_state.heap))
      Ok(Nil)
    }
    Error(err) -> {
      io.println_error(format_repl_error(err, new_state.heap))
      Error(Nil)
    }
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
  let outcome = case get_script_args() {
    ["-p", ..rest] -> run_print(string.join(rest, " "))

    [path, ..] -> run_file(path, event_loop.finish)

    [] -> {
      banner()
      new_repl_state() |> repl_loop()
      Ok(Nil)
    }
  }

  case outcome {
    Ok(Nil) -> Nil
    Error(Nil) -> halt(1)
  }
}
