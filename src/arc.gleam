import arc/compiler
import arc/dis
import arc/engine.{
  type Engine, type Outcome, type Repl, ModuleReturned, ModuleThrew, Returned,
  Threw,
}
import arc/esm
import arc/internal/path
import arc/module/load_error
import arc/module_host.{type LoadError, type ResolveError}
import arc/parser
import arc/repl/examples
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

// -- Eval one line -----------------------------------------------------------

/// Render an `EvalError` for the terminal. Parse and compile failures get the
/// prefix a JS user expects; everything else is the engine's own message.
fn format_eval_error(err: engine.EvalError(host)) -> String {
  case err {
    engine.ParseError(parse_err) ->
      "SyntaxError: " <> parser.parse_error_to_string(parse_err)
    engine.CompileError(compile_err) ->
      "compile error: " <> compiler.error_message(compile_err)
    engine.VmError(_) -> "InternalError: " <> engine.eval_error_message(err)
    engine.ModuleCompileError(_) | engine.ModuleError(..) ->
      engine.eval_error_message(err)
  }
}

/// Evaluate one REPL input. The session comes back on every path: advanced
/// when the input ran (returned or threw), unchanged when it never made it
/// to bytecode or the engine itself failed.
fn eval(
  repl: Repl(host),
  source: String,
) -> #(Repl(host), Result(Outcome, engine.EvalError(host))) {
  case engine.repl_eval(repl, source) {
    Ok(#(outcome, repl)) -> #(repl, Ok(outcome))
    Error(err) -> #(repl, Error(err))
  }
}

/// `Uncaught <error>` for a value a run threw, rendered against the engine
/// that produced it.
fn format_uncaught(eng: Engine(host), thrown) -> String {
  "Uncaught " <> engine.format_error(eng, thrown)
}

/// Print whatever one REPL input produced: the inspected completion value,
/// the uncaught error, or the reason it never ran.
fn print_result(
  repl: Repl(host),
  result: Result(Outcome, engine.EvalError(host)),
) -> Nil {
  let eng = engine.repl_engine(repl)
  case result {
    Ok(Returned(val)) -> io.println(engine.inspect(eng, val))
    Ok(Threw(thrown)) -> io.println(format_uncaught(eng, thrown))
    Error(err) -> io.println(format_eval_error(err))
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
/// updated) session, or stop because the user asked to leave.
type ReplStep(host) {
  Continue(Repl(host))
  Quit
}

fn handle_repl_line(repl: Repl(host), line: String) -> ReplStep(host) {
  let source = string.trim(line)
  case source {
    "/clear" -> {
      clear()
      Continue(repl)
    }

    "/heap" -> {
      io.println("Usage: `/heap <expression>`")
      Continue(repl)
    }

    "/heap " <> source -> {
      let #(repl, result) = eval(repl, source)
      case result {
        Ok(Returned(val)) ->
          engine.dump_object(engine.repl_engine(repl), val)
          |> option.unwrap("not an object")
          |> io.println
        Ok(Threw(_)) | Error(_) -> print_result(repl, result)
      }
      Continue(repl)
    }

    "/dis" -> {
      io.println("Usage: `/dis <source>`")
      Continue(repl)
    }

    "/dis " <> source -> {
      case dis.source(dis.ReplInput, source) {
        Ok(text) -> io.print(text)
        Error(err) -> io.println(dis.format_source_error(err))
      }
      Continue(repl)
    }

    "/exit" -> {
      io.println("Goodbye!")
      Quit
    }

    "/reset" -> {
      clear()
      banner()
      Continue(new_repl())
    }

    "/help" -> {
      io.println("    /clear          - clear the console")
      io.println("    /dis <source>   - show the bytecode <source> compiles to")
      io.println("    /help           - show this message")
      io.println("    /reset          - reset the REPL state")
      io.println("    /examples [n]   - list or run built-in demos")
      io.println("    /exit           - exit the REPL")
      Continue(repl)
    }

    "/examples" -> {
      examples.print_list()
      Continue(repl)
    }

    "/examples " <> arg ->
      case int.parse(string.trim(arg)) {
        Error(Nil) -> {
          io.println("Usage: `/examples <n>` (try `/examples` for the list)")
          Continue(repl)
        }
        Ok(n) ->
          case examples.get(n) {
            None -> {
              io.println(
                "No example " <> int.to_string(n) <> ". Try `/examples`.",
              )
              Continue(repl)
            }
            Some(ex) -> {
              examples.print_source(ex)
              let #(repl, result) = eval(repl, ex.source)
              case result {
                Ok(Returned(_)) -> Nil
                Ok(Threw(_)) | Error(_) -> print_result(repl, result)
              }
              io.println("")
              Continue(repl)
            }
          }
      }

    "" -> Continue(repl)

    _ -> {
      let #(repl, result) = eval(repl, source)
      print_result(repl, result)
      Continue(repl)
    }
  }
}

fn repl_loop(repl: Repl(host)) -> Nil {
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
      case handle_repl_line(repl, line) {
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
/// `arc -p <expr>`, `arc --dis <file>`), still carrying its original typed
/// cause. The runners never print — they return one of these and `main`
/// renders it once via `format_cli_error`. A thrown JS value is the one cause
/// rendered where it happens: it only means anything against the engine that
/// produced it, so the runner formats it there and carries the text.
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
  /// `arc --dis <file>`: the file did not parse/compile. Nothing ran.
  DisFailed(error: dis.SourceError)
  /// The script, module or `-p` expression ran but threw an uncaught
  /// exception; `report` is the rendered `Uncaught ...` line.
  ScriptThrew(report: String)
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
    EvalFailed(eval_err) -> format_eval_error(eval_err)
    DisFailed(source_err) -> dis.format_source_error(source_err)
    ScriptThrew(report) -> report
  }
}

/// Which goal symbol a file is parsed and compiled under. `.cjs` is a classic
/// script; everything else is an ES module. Written ONCE, so `run_file` and
/// `run_dis` cannot disagree about how a given path is treated.
fn goal_symbol(path: String) -> dis.Goal {
  case string.ends_with(path, ".cjs") {
    True -> dis.Script
    False -> dis.Module
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
    dis.Script | dis.ReplInput -> run_script_file(source)
    dis.Module -> run_module_file(path, source)
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
    Ok(#(ModuleReturned(..), _eng)) -> Ok(Nil)
    Ok(#(ModuleThrew(error:), eng)) ->
      Error(ScriptThrew(format_uncaught(eng, error)))
    Error(err) -> Error(EvalFailed(err))
  }
}

/// Resolve a dependency specifier: relative paths (./foo, ../bar) against
/// the parent module's directory. The CLI is a filesystem loader, so a bare
/// specifier ("fs", a URL) has no path meaning here — it is rejected as such,
/// never probed as if it were a file.
///
/// `module_host.ResolveFn` is a stringly host boundary; the Raw/Resolved
/// distinction is put back on at this edge and taken off again on the way out.
fn resolve_dep(
  raw_specifier: String,
  parent_specifier: String,
) -> Result(String, ResolveError) {
  let raw = esm.raw(raw_specifier)
  let parent = esm.resolved_unchecked(parent_specifier)
  case path.resolve_specifier(raw, parent) {
    path.PathSpecifier(resolved) -> Ok(esm.resolved_text(resolved))
    path.BareSpecifier(_bare) -> Error(load_error.UnsupportedBareSpecifier)
  }
}

/// Read a resolved dependency's source from disk. Only a genuinely absent
/// file is `NotFound`; a directory, a permissions failure or an I/O error is a
/// `ReadFailed` carrying simplifile's own description.
fn load_dep(resolved: String) -> Result(String, LoadError) {
  case simplifile.read(resolved) {
    Ok(source) -> Ok(source)
    Error(simplifile.Enoent) -> Error(load_error.LoadNotFound)
    Error(err) -> Error(load_error.ReadFailed(simplifile.describe_error(err)))
  }
}

/// Run a file as a script (only for .cjs files).
fn run_script_file(source: String) -> Result(Nil, CliError(host)) {
  let eng = engine.new()
  case engine.eval(eng, source) {
    Ok(#(Threw(thrown), eng)) ->
      Error(ScriptThrew(format_uncaught(eng, thrown)))
    Ok(#(Returned(_), _eng)) -> Ok(Nil)
    Error(err) -> Error(EvalFailed(err))
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
  use text <- result.try(
    dis.source(goal_symbol(path), source) |> result.map_error(DisFailed),
  )
  let out_path = path <> ".dis.txt"
  use Nil <- result.map(
    simplifile.write(out_path, text)
    |> result.map_error(fn(err) { WriteFailed(path: out_path, error: err) }),
  )
  io.println("wrote " <> out_path)
}

/// `arc -p <expr>`: evaluate one expression in a fresh REPL session and
/// print the result. A failed eval comes back as a `CliError` for `main` to
/// render.
fn run_print(source: String) -> Result(Nil, CliError(host)) {
  case engine.repl_eval(new_repl(), source) {
    Ok(#(Returned(val), repl)) -> {
      io.println(engine.inspect(engine.repl_engine(repl), val))
      Ok(Nil)
    }
    Ok(#(Threw(thrown), repl)) ->
      Error(ScriptThrew(format_uncaught(engine.repl_engine(repl), thrown)))
    Error(err) -> Error(EvalFailed(err))
  }
}

fn new_repl() -> Repl(host) {
  engine.repl(engine.new())
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
      new_repl() |> repl_loop()
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
