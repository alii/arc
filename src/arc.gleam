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

type ReadLine {
  Line(String)
  Eof
  ReadError(reason: dynamic.Dynamic)
}

@external(erlang, "arc_cli_ffi", "read_line")
fn read_line(prompt: String) -> ReadLine

fn format_eval_error(err: engine.EvalError(host)) -> String {
  case err {
    engine.ParseError(parse_err) ->
      "SyntaxError: " <> parser.parse_error_to_string(parse_err)
    engine.CompileError(compile_err) ->
      "compile error: " <> compiler.error_message(compile_err)
    engine.ModuleCompileError(_) | engine.ModuleError(..) ->
      engine.eval_error_message(err)
  }
}

fn eval(
  repl: Repl(host),
  source: String,
) -> #(Repl(host), Result(Outcome, engine.EvalError(host))) {
  case engine.repl_eval(repl, source) {
    Ok(#(outcome, repl)) -> #(repl, Ok(outcome))
    Error(err) -> #(repl, Error(err))
  }
}

fn format_uncaught(eng: Engine(host), thrown) -> String {
  "Uncaught " <> engine.format_error(eng, thrown)
}

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

fn clear() -> Nil {
  io.println("\u{1b}[2J\u{1b}[H")
}

fn banner() -> Nil {
  io.println("arc -- JavaScript on the BEAM")
  io.println("Run /help for commands, Ctrl+C to exit.")
  io.println("")
}

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

type CliError(host) {
  BadUsage(reason: UsageError)
  ReadFailed(path: String, error: simplifile.FileError)
  WriteFailed(path: String, error: simplifile.FileError)
  EvalFailed(error: engine.EvalError(host))
  DisFailed(error: dis.SourceError)
  ScriptThrew(report: String)
}

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

fn goal_symbol(path: String) -> dis.Goal {
  case string.ends_with(path, ".cjs") {
    True -> dis.Script
    False -> dis.Module
  }
}

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

fn run_module_file(
  entry_path: String,
  source: String,
) -> Result(Nil, CliError(host)) {
  let eng = engine.new()
  let entry = path.normalize(entry_path)
  case engine.eval_module(eng, entry, source, resolve_dep, load_dep) {
    Ok(#(ModuleReturned(..), _eng)) -> Ok(Nil)
    Ok(#(ModuleThrew(error:), eng)) ->
      Error(ScriptThrew(format_uncaught(eng, error)))
    Error(err) -> Error(EvalFailed(err))
  }
}

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

fn load_dep(resolved: String) -> Result(String, LoadError) {
  case simplifile.read(resolved) {
    Ok(source) -> Ok(source)
    Error(simplifile.Enoent) -> Error(load_error.LoadNotFound)
    Error(err) -> Error(load_error.ReadFailed(simplifile.describe_error(err)))
  }
}

fn run_script_file(source: String) -> Result(Nil, CliError(host)) {
  let eng = engine.new()
  case engine.eval(eng, source) {
    Ok(#(Threw(thrown), eng)) ->
      Error(ScriptThrew(format_uncaught(eng, thrown)))
    Ok(#(Returned(_), _eng)) -> Ok(Nil)
    Error(err) -> Error(EvalFailed(err))
  }
}

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

pub type UsageError {
  MissingDisPath
  MissingPrintExpr
  UnknownFlag(String)
}

pub type Command {
  Repl
  RunFile(String)
  Print(String)
  Dis(String)
  Usage(reason: UsageError)
}

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
