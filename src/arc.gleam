import arc/compiler
import arc/dis
import arc/engine.{Returned, Threw}
import arc/esm
import arc/internal/path
import arc/module/load_error
import arc/module_host.{type ModuleLoadError}
import arc/parser
import arc/repl/examples
import arc/vm/builtins/common.{type Builtins}
import arc/vm/exec/entry
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

/// A source string that never made it to bytecode. Renderable with NO heap —
/// nothing has been allocated yet — which is what lets `/dis` (which never
/// runs the code) report failures without holding one.
type CompileError {
  Syntax(parser.ParseError)
  Compile(compiler.CompileError)
}

fn format_compile_error(err: CompileError) -> String {
  case err {
    Syntax(parse_err) ->
      "SyntaxError: " <> parser.parse_error_to_string(parse_err)
    Compile(compile_err) ->
      "compile error: " <> compiler.error_message(compile_err)
  }
}

/// Everything that can go wrong evaluating one REPL line, still carrying its
/// original typed cause. Rendered for humans only at `format_repl_error`.
type ReplError {
  CompileFailed(CompileError)
  Uncaught(value.JsValue)
  Internal(state.VmError)
}

/// Render a `ReplError` at the print site. `heap` must be the heap returned by
/// the `eval` that produced the error, so a thrown value can be inspected.
fn format_repl_error(err: ReplError, heap: Heap(host)) -> String {
  case err {
    CompileFailed(compile_err) -> format_compile_error(compile_err)
    Uncaught(thrown) -> "Uncaught " <> object.format_error(thrown, heap)
    Internal(vm_err) -> "InternalError: " <> vm_error_message(vm_err)
  }
}

// -- Eval one line -----------------------------------------------------------

/// Parse + compile one REPL line to a template, without running it. Shared
/// by `eval` and the `/dis` command so both report syntax/compile errors the
/// same way — and it can only fail BEFORE anything runs, so its error type
/// carries no thrown value and needs no heap to render.
fn compile_line(source: String) -> Result(value.FuncTemplate, CompileError) {
  use #(body, sb) <- result.try(
    parser.parse_script(source)
    |> result.map_error(Syntax),
  )
  compiler.compile_repl(body, sb)
  |> result.map_error(Compile)
}

fn eval(
  state: ReplState(host),
  source: String,
) -> #(ReplState(host), Result(JsValue, ReplError)) {
  case compile_line(source) {
    Error(err) -> #(state, Error(CompileFailed(err)))
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
          Error(Uncaught(val)),
        )
        Error(vm_err) -> #(state, Error(Internal(vm_err)))
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
        Ok(value.JsObject(ref)) ->
          heap.read(new_state.heap, ref)
          |> option.map(value.heap_slot_to_string)
          |> option.unwrap("<collected>")
          |> io.println
        Ok(_) -> io.println("not an object")
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
        Error(err) -> io.println(format_compile_error(err))
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
  /// argv did not name a runnable command.
  BadUsage(reason: UsageError)
  /// The entry file could not be read from disk.
  ReadFailed(path: String, error: simplifile.FileError)
  /// `arc --dis <file>`: the disassembly output file could not be written.
  WriteFailed(path: String, error: simplifile.FileError)
  /// The parse → compile → run pipeline failed (or an ES module bundle
  /// failed to link/evaluate).
  EvalFailed(error: engine.EvalError(host))
  /// `arc --dis <file>`: the file did not parse/compile. Nothing ran, so no
  /// heap is needed to render it.
  DisFailed(error: CompileError)
  /// The script (or module) ran but threw an uncaught exception. The engine
  /// that produced it is kept so the thrown value can be inspected at the
  /// print site.
  ScriptThrew(engine: engine.Engine(host), thrown: JsValue)
  /// `arc -p <expr>` failed. The heap that produced the error is kept so a
  /// thrown value can be inspected at the print site.
  PrintFailed(heap: Heap(host), error: ReplError)
}

/// Render a `CliError` at the print site (in `main`, just before exiting
/// non-zero).
fn format_cli_error(err: CliError(host)) -> String {
  case err {
    BadUsage(reason) -> format_usage_error(reason)
    ReadFailed(path, file_err) ->
      "Error reading " <> path <> ": " <> simplifile.describe_error(file_err)
    WriteFailed(path, file_err) ->
      "Error writing " <> path <> ": " <> simplifile.describe_error(file_err)
    EvalFailed(eval_err) -> engine.eval_error_message(eval_err)
    DisFailed(compile_err) -> format_compile_error(compile_err)
    ScriptThrew(eng, thrown) -> "Uncaught " <> engine.format_error(eng, thrown)
    PrintFailed(heap, repl_err) -> format_repl_error(repl_err, heap)
  }
}

/// Which goal symbol a file is parsed and compiled under. `.cjs` is a classic
/// script; everything else is an ES module. Written ONCE, so `run_file` and
/// `run_dis` cannot disagree about how a given path is treated.
type GoalSymbol {
  Script
  Module
}

fn goal_symbol(path: String) -> GoalSymbol {
  case string.ends_with(path, ".cjs") {
    True -> Script
    False -> Module
  }
}

/// Run a JS source file. `Ok(Nil)` means it ran to completion with nothing
/// uncaught; every failure is returned as a typed `CliError` for `main` to
/// render and exit non-zero on. The engine boots with its default host hooks
/// (`engine.new()`) and its default post-script driver, which drains
/// microtasks after the top level returns.
fn run_file(path: String) -> Result(Nil, CliError(host)) {
  use source <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(err) { ReadFailed(path:, error: err) }),
  )
  case goal_symbol(path) {
    Module -> run_module_file(path, source)
    Script -> run_script_file(source)
  }
}

/// Run a file as an ES module using the bundle lifecycle.
fn run_module_file(
  entry_path: String,
  source: String,
) -> Result(Nil, CliError(host)) {
  let eng = engine.new()
  // The entry specifier is a module IDENTITY, and it comes straight from argv.
  // Normalize it, or `arc ./a.js` names a different module than the `a.js` a
  // dependency's `import "./a.js"` resolves to — one file, two module records.
  let entry = path.normalize(entry_path)
  case engine.eval_module(eng, entry, source, resolve_dep, load_dep) {
    Ok(#(evaluated, eng)) ->
      case evaluated.outcome {
        Returned(_) -> Ok(Nil)
        Threw(val) -> Error(ScriptThrew(engine: eng, thrown: val))
      }
    Error(err) -> Error(EvalFailed(err))
  }
}

/// Resolve a dependency specifier: relative paths (./foo, ../bar) against
/// the parent module's directory. The CLI is a filesystem loader, so a bare
/// specifier ("fs", a URL) has no path meaning here — it is rejected as such,
/// never probed as if it were a file.
fn resolve_dep(
  raw_specifier: String,
  parent_specifier: String,
) -> Result(String, ModuleLoadError) {
  case path.resolve_specifier(raw_specifier, parent_specifier) {
    path.PathSpecifier(resolved) -> Ok(resolved)
    path.BareSpecifier(bare) -> Error(load_error.UnsupportedBareSpecifier(bare))
  }
}

/// Read a resolved dependency's source from disk. Only a genuinely absent
/// file is `NotFound`; a directory, a permissions failure or an I/O error is a
/// `ReadFailed` carrying simplifile's own description.
fn load_dep(resolved: String) -> Result(String, ModuleLoadError) {
  case simplifile.read(resolved) {
    Ok(source) -> Ok(source)
    Error(simplifile.Enoent) -> Error(load_error.NotFound(resolved))
    Error(err) ->
      Error(load_error.ReadFailed(resolved, simplifile.describe_error(err)))
  }
}

/// Run a file as a script (only for .cjs files).
fn run_script_file(source: String) -> Result(Nil, CliError(host)) {
  let eng = engine.new()
  case engine.eval(eng, source) {
    Ok(#(Threw(val), eng)) -> Error(ScriptThrew(engine: eng, thrown: val))
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(EvalFailed(err))
  }
}

/// Parse + compile a whole file under its goal symbol, without running it.
/// Each goal symbol has its own parse + compile entry point, so the two paths
/// never share a `Program` wrapper one of them would have to reject.
fn compile_file(
  goal: GoalSymbol,
  source: String,
) -> Result(value.FuncTemplate, CompileError) {
  case goal {
    Script -> {
      use #(body, sb) <- result.try(
        parser.parse_script(source)
        |> result.map_error(Syntax),
      )
      compiler.compile(body, sb)
      |> result.map_error(Compile)
    }
    Module -> {
      use #(items, sb) <- result.try(
        parser.parse_module(source)
        |> result.map_error(Syntax),
      )
      use compiled <- result.map(
        compiler.compile_module(items, sb, esm.analyze(items))
        |> result.map_error(Compile),
      )
      compiled.template
    }
  }
}

/// `arc --dis <file>`: parse and compile <file> WITHOUT running it, and write
/// the disassembled bytecode next to it as `<file>.dis.txt`. The goal symbol
/// (`.cjs` ⇒ script, else module) is the same one `run_file` picks its
/// execution path with.
fn run_dis(path: String) -> Result(Nil, CliError(host)) {
  use source <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(err) { ReadFailed(path:, error: err) }),
  )
  use template <- result.try(
    compile_file(goal_symbol(path), source)
    |> result.map_error(DisFailed),
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

/// Why argv did not name a runnable command. Rendered once, by
/// `format_usage_error`.
pub type UsageError {
  MissingDisPath
  MissingPrintExpr
  UnknownFlag(String)
}

/// What argv asked arc to do. Parsing argv into one of these keeps the flag
/// list in a single place, and makes `Usage(_)` a value the caller must handle
/// — the old inline match printed usage and still returned `Ok(Nil)`, so a
/// usage error exited 0.
pub type Command {
  Repl
  RunFile(String)
  Print(String)
  Dis(String)
  Usage(reason: UsageError)
}

/// Pure argv → `Command`. No IO, no exits.
fn parse_args(args: List(String)) -> Command {
  case args {
    [] -> Repl
    ["-p"] -> Usage(MissingPrintExpr)
    ["-p", ..rest] -> Print(string.join(rest, " "))
    ["--dis"] -> Usage(MissingDisPath)
    ["--dis", path, ..] -> Dis(path)
    ["-" <> _ as flag, ..] -> Usage(UnknownFlag(flag))
    [path, ..] -> RunFile(path)
  }
}

fn format_usage_error(reason: UsageError) -> String {
  let detail = case reason {
    MissingDisPath -> "arc --dis: missing <file>"
    MissingPrintExpr -> "arc -p: missing <expr>"
    UnknownFlag(flag) -> "arc: unknown flag " <> flag
  }
  detail
  <> "\n\nUsage:\n"
  <> "  arc                start the REPL\n"
  <> "  arc <file>         run a file (.cjs as a script, else as an ES module)\n"
  <> "  arc -p <expr>      evaluate <expr> and print the result\n"
  <> "  arc --dis <file>   write <file>.dis.txt with the compiled bytecode"
}

fn run(command: Command) -> Result(Nil, CliError(host)) {
  case command {
    Repl -> {
      banner()
      new_repl_state() |> repl_loop()
      Ok(Nil)
    }
    RunFile(path) -> run_file(path)
    Print(source) -> run_print(source)
    Dis(path) -> run_dis(path)
    Usage(reason) -> Error(BadUsage(reason))
  }
}

pub fn main() -> Nil {
  case run(parse_args(get_script_args())) {
    Ok(Nil) -> Nil
    Error(err) -> {
      io.println_error(format_cli_error(err))
      halt(1)
    }
  }
}
