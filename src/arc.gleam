import arc/compiler
import arc/dis
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
import gleam/dynamic
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import simplifile

// -- FFI: read a line from stdin ---------------------------------------------

/// One `read_line` outcome. `Eof` (Ctrl-D / closed stdin) is a normal way to
/// leave the REPL; `ReadError` is a real I/O failure and is reported, not
/// silently conflated with end-of-input.
type ReadLine {
  Line(String)
  Eof
  ReadError(reason: dynamic.Dynamic)
}

@external(erlang, "arc_cli_ffi", "read_line")
fn read_line(prompt: String) -> ReadLine

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

/// Parse + compile one REPL line to a template, without running it. Shared
/// by `eval` and the `/dis` command so both report syntax/compile errors the
/// same way.
fn compile_line(source: String) -> Result(value.FuncTemplate, ReplError) {
  use #(program, sb) <- result.try(
    parser.parse(source, parser.Script)
    |> result.map_error(ReplSyntax),
  )
  compiler.compile_repl(program, sb)
  |> result.map_error(ReplCompile)
}

fn eval(
  state: ReplState(host),
  source: String,
) -> #(ReplState(host), Result(JsValue, ReplError)) {
  case compile_line(source) {
    Error(err) -> #(state, Error(err))
    Ok(template) ->
      case
        entry.run_and_drain_repl(
          template,
          state.heap,
          state.builtins,
          state.env,
        )
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

    "/dis" -> {
      io.println("Usage: `/dis <source>`")
      Continue(state)
    }

    "/dis " <> source -> {
      case compile_line(source) {
        Ok(template) -> io.print(dis.disassemble(template))
        Error(err) -> io.println(format_repl_error(err, state.heap))
      }
      Continue(state)
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
      io.println("    /dis <source>   - show the bytecode <source> compiles to")
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
                Error(err) -> io.println(format_repl_error(err, new_state.heap))
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
    Eof -> {
      io.println("")
      Nil
    }

    ReadError(reason) -> {
      io.println_error("Error reading stdin: " <> string.inspect(reason))
      Nil
    }

    Line(line) ->
      case handle_repl_line(state, line) {
        Continue(next) -> repl_loop(next)
        Quit -> Nil
      }
  }
}

@external(erlang, "arc_cli_ffi", "get_script_args")
fn get_script_args() -> List(String)

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> a

/// Everything that can go wrong on a non-interactive CLI path (`arc <file>`,
/// `arc -p <expr>`), still carrying its original typed cause plus whatever
/// context (engine, heap) is needed to render it. The runners never print —
/// they return one of these and `main` renders it once via `format_cli_error`,
/// mirroring `ReplError` / `format_repl_error`.
type CliError(host) {
  /// The entry file could not be read from disk.
  ReadFailed(path: String, error: simplifile.FileError)
  /// `arc --dis <file>`: the disassembly output file could not be written.
  WriteFailed(path: String, error: simplifile.FileError)
  /// The parse → compile → run pipeline failed (or an ES module bundle
  /// failed to link/evaluate).
  EvalFailed(error: engine.EvalError(host))
  /// The script ran but threw an uncaught exception. The engine that produced
  /// it is kept so the thrown value can be inspected at the print site.
  Uncaught(engine: engine.Engine(host), thrown: JsValue)
  /// `arc -p <expr>` failed. The heap that produced the error is kept so a
  /// thrown value can be inspected at the print site.
  PrintFailed(heap: Heap(host), error: ReplError)
}

/// Render a `CliError` at the print site (in `main`, just before exiting
/// non-zero).
fn format_cli_error(err: CliError(host)) -> String {
  case err {
    ReadFailed(path, file_err) ->
      "Error reading " <> path <> ": " <> simplifile.describe_error(file_err)
    WriteFailed(path, file_err) ->
      "Error writing " <> path <> ": " <> simplifile.describe_error(file_err)
    EvalFailed(eval_err) -> engine.eval_error_message(eval_err)
    Uncaught(eng, thrown) -> "Uncaught " <> engine.format_error(eng, thrown)
    PrintFailed(heap, repl_err) -> format_repl_error(repl_err, heap)
  }
}

/// Run a JS source file. `Ok(Nil)` means it ran to completion with nothing
/// uncaught; every failure is returned as a typed `CliError` for `main` to
/// render and exit non-zero on. The engine boots with its default host hooks
/// (`engine.new()`); `finish` drains microtasks (and any embedder macrotask
/// loop) after the top level returns.
fn run_file(
  path: String,
  finish: fn(state.State(host)) -> state.State(host),
) -> Result(Nil, CliError(host)) {
  case simplifile.read(path) {
    Error(err) -> Error(ReadFailed(path:, error: err))
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
) -> Result(Nil, CliError(host)) {
  let eng = engine.new()
  engine.eval_module_with(eng, path, source, resolve_dep, load_dep, finish)
  |> result.replace(Nil)
  |> result.map_error(EvalFailed)
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
) -> Result(Nil, CliError(host)) {
  let eng = engine.new()
  case engine.eval_with(eng, source, finish) {
    Ok(#(Threw(val), eng)) -> Error(Uncaught(engine: eng, thrown: val))
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(EvalFailed(err))
  }
}

/// `arc --dis <file>`: parse and compile <file> WITHOUT running it, and write
/// the disassembled bytecode next to it as `<file>.dis.txt`. `.cjs` files
/// compile as classic scripts, everything else as an ES module — the same
/// rule `run_file` uses to pick an execution path. Parse/compile failures are
/// reported through the same `engine.EvalError` shapes a normal run would use.
fn run_dis(path: String) -> Result(Nil, CliError(host)) {
  use source <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(err) { ReadFailed(path:, error: err) }),
  )
  let mode = case string.ends_with(path, ".cjs") {
    True -> parser.Script
    False -> parser.Module
  }
  use #(program, sb) <- result.try(
    parser.parse(source, mode)
    |> result.map_error(fn(err) { EvalFailed(engine.ParseError(err)) }),
  )
  use template <- result.try(
    compiler.compile(program, sb)
    |> result.map_error(fn(err) { EvalFailed(engine.CompileError(err)) }),
  )
  let out_path = path <> ".dis.txt"
  use Nil <- result.map(
    simplifile.write(out_path, dis.disassemble(template))
    |> result.map_error(fn(err) { WriteFailed(path: out_path, error: err) }),
  )
  io.println("wrote " <> out_path)
}

/// `arc -p <expr>`: evaluate one expression in a fresh REPL state and print
/// the result. A failed eval comes back as a `CliError` for `main` to render.
fn run_print(source: String) -> Result(Nil, CliError(host)) {
  let #(new_state, result) = eval(new_repl_state(), source)
  case result {
    Ok(val) -> {
      io.println(object.inspect(val, new_state.heap))
      Ok(Nil)
    }
    Error(err) -> Error(PrintFailed(heap: new_state.heap, error: err))
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

    ["--dis", path, ..] -> run_dis(path)

    ["--dis"] -> {
      io.println_error("Usage: arc --dis <file>")
      Ok(Nil)
    }

    [path, ..] -> run_file(path, event_loop.finish)

    [] -> {
      banner()
      new_repl_state() |> repl_loop()
      Ok(Nil)
    }
  }

  case outcome {
    Ok(Nil) -> Nil
    Error(err) -> {
      io.println_error(format_cli_error(err))
      halt(1)
    }
  }
}
