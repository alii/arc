import arc/host_hooks.{type HostHooks}
import arc/rt/inspect as rt_inspect
import arc_aot/compile
import arc_aot/run
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

@external(erlang, "arc_aot_cli_ffi", "get_script_args")
fn get_script_args() -> List(String)

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> a

/// Why argv did not name a runnable command.
pub type UsageError {
  MissingFile(command: String)
  MissingOutputPath
  UnknownFlag(String)
  UnknownCommand(String)
}

/// What argv asked for.
pub type Command {
  Run(path: String)
  Build(path: String, out: Option(String), core: Bool, ir: Bool)
  Help
  Usage(reason: UsageError)
}

/// Everything a non-interactive command can fail with. `main` renders it once
/// and picks the exit code from it.
pub type CliError {
  BadUsage(reason: UsageError)
  ReadFailed(path: String, error: simplifile.FileError)
  WriteFailed(path: String, error: simplifile.FileError)
  CompileFailed(path: String, error: compile.CompileError)
  LoadFailed(reason: String)
  /// The script ran but threw; `report` is the rendered `Uncaught ...` line.
  ScriptThrew(report: String)
  /// The compiled module raised a non-JS error.
  Crashed(reason: String)
}

/// Pure argv → `Command`.
pub fn parse_args(args: List(String)) -> Command {
  case args {
    [] | ["help", ..] | ["--help", ..] | ["-h", ..] -> Help
    ["run"] -> Usage(MissingFile("run"))
    ["run", path, ..] -> Run(path)
    ["build"] -> Usage(MissingFile("build"))
    ["build", path, ..flags] ->
      parse_build_flags(flags, Build(path, None, core: False, ir: False))
    ["-" <> _ as flag, ..] -> Usage(UnknownFlag(flag))
    [other, ..] -> Usage(UnknownCommand(other))
  }
}

fn parse_build_flags(flags: List(String), command: Command) -> Command {
  case flags, command {
    [], _ -> command
    ["-o", out, ..rest], Build(path, _, core, ir) ->
      parse_build_flags(rest, Build(path, Some(out), core:, ir:))
    ["-o"], _ -> Usage(MissingOutputPath)
    ["--core", ..rest], Build(path, out, _, ir) ->
      parse_build_flags(rest, Build(path, out, core: True, ir:))
    ["--ir", ..rest], Build(path, out, core, _) ->
      parse_build_flags(rest, Build(path, out, core:, ir: True))
    [flag, ..], _ -> Usage(UnknownFlag(flag))
  }
}

pub const usage_text = "Usage:
  arc_aot run <file>              compile <file> to BEAM in memory and run it
  arc_aot build <file> [-o out]   compile <file> and write the .beam (default <file>.beam)
            [--core] [--ir]       also write the Core Erlang / 2core IR text next to it
  arc_aot help                    show this message

Scripts only: a file using import/export exits with status 2."

fn format_usage_error(reason: UsageError) -> String {
  let detail = case reason {
    MissingFile(command) -> "arc_aot " <> command <> ": missing <file>"
    MissingOutputPath -> "arc_aot build: -o needs a path"
    UnknownFlag(flag) -> "arc_aot: unknown flag " <> flag
    UnknownCommand(command) -> "arc_aot: unknown command " <> command
  }
  detail <> "\n\n" <> usage_text
}

pub fn format_cli_error(err: CliError) -> String {
  case err {
    BadUsage(reason) -> format_usage_error(reason)
    ReadFailed(path, file_err) ->
      "Error reading " <> path <> ": " <> simplifile.describe_error(file_err)
    WriteFailed(path, file_err) ->
      "Error writing " <> path <> ": " <> simplifile.describe_error(file_err)
    CompileFailed(path, compile_err) ->
      path <> ": " <> compile.describe(compile_err)
    LoadFailed(reason) -> "load failed: " <> reason
    ScriptThrew(report) -> report
    Crashed(reason) -> "internal error: " <> reason
  }
}

/// Usage errors and module-goal sources exit 2; everything else 1.
pub fn exit_code(err: CliError) -> Int {
  case err {
    BadUsage(_) | CompileFailed(_, compile.ModuleGoalUnsupported) -> 2
    _ -> 1
  }
}

/// The BEAM module name a file compiles to: its basename without extension,
/// with anything outside [A-Za-z0-9_] replaced by `_`.
pub fn module_name_for(path: String) -> String {
  let base = case list.last(string.split(path, "/")) {
    Ok(base) -> base
    Error(Nil) -> path
  }
  let stem = case string.split(base, ".") {
    [stem, ..] if stem != "" -> stem
    _ -> base
  }
  stem
  |> string.to_graphemes
  |> list.map(fn(g) {
    case is_name_char(g) {
      True -> g
      False -> "_"
    }
  })
  |> string.concat
}

fn is_name_char(g: String) -> Bool {
  case g {
    "_" -> True
    _ ->
      case string.to_utf_codepoints(g) {
        [cp] -> {
          let c = string.utf_codepoint_to_int(cp)
          { c >= 48 && c <= 57 }
          || { c >= 65 && c <= 90 }
          || { c >= 97 && c <= 122 }
        }
        _ -> False
      }
  }
}

fn read_source(path: String) -> Result(String, CliError) {
  simplifile.read(path)
  |> result.map_error(fn(err) { ReadFailed(path:, error: err) })
}

/// `arc_aot run <file>`: compile, load, run `js_main`, drain microtasks.
fn run_file(path: String, hooks: HostHooks) -> Result(Nil, CliError) {
  use source <- result.try(read_source(path))
  let name = "arc_aot_js_" <> module_name_for(path)
  use beam <- result.try(
    compile.to_beam(source, name)
    |> result.map_error(fn(err) { CompileFailed(path:, error: err) }),
  )
  use module <- result.try(run.load(beam, name) |> result.map_error(LoadFailed))
  let #(outcome, st) = run.apply_main(module, run.seed(hooks))
  run.unload(module)
  case outcome {
    run.JsReturned(_) -> Ok(Nil)
    run.JsThrew(thrown) ->
      Error(ScriptThrew("Uncaught " <> rt_inspect.format_error(st, thrown)))
    run.JsCrashed(reason) -> Error(Crashed(reason))
  }
}

/// `arc_aot build <file>`: write the .beam (and optionally .core / .ir).
fn build_file(
  path: String,
  out: Option(String),
  core: Bool,
  ir: Bool,
) -> Result(Nil, CliError) {
  use source <- result.try(read_source(path))
  let out = option.lazy_unwrap(out, fn() { strip_js(path) <> ".beam" })
  let name = module_name_for(out)
  let compile_failed = fn(err) { CompileFailed(path:, error: err) }
  use module <- result.try(
    compile.to_ir(source, name) |> result.map_error(compile_failed),
  )
  let stem = strip_suffix(out, ".beam")
  use Nil <- result.try(case ir {
    True -> write(stem <> ".ir", compile.ir_to_text(module))
    False -> Ok(Nil)
  })
  use Nil <- result.try(case core {
    True -> {
      use text <- result.try(
        compile.ir_to_core(module) |> result.map_error(compile_failed),
      )
      write(stem <> ".core", text)
    }
    False -> Ok(Nil)
  })
  use beam <- result.try(
    compile.ir_to_beam(module) |> result.map_error(compile_failed),
  )
  use Nil <- result.map(
    simplifile.write_bits(out, beam)
    |> result.map_error(fn(err) { WriteFailed(path: out, error: err) }),
  )
  io.println("wrote " <> out <> " (module " <> name <> ")")
}

fn write(path: String, text: String) -> Result(Nil, CliError) {
  use Nil <- result.map(
    simplifile.write(path, text)
    |> result.map_error(fn(err) { WriteFailed(path:, error: err) }),
  )
  io.println("wrote " <> path)
}

fn strip_js(path: String) -> String {
  strip_suffix(path, ".js")
}

fn strip_suffix(path: String, suffix: String) -> String {
  case string.ends_with(path, suffix) {
    True -> string.drop_end(path, string.length(suffix))
    False -> path
  }
}

/// Run one command against `hooks` (the `console` sink and clocks the
/// script sees). `Help` prints usage and succeeds.
pub fn execute(command: Command, hooks: HostHooks) -> Result(Nil, CliError) {
  case command {
    Help -> {
      io.println(usage_text)
      Ok(Nil)
    }
    Run(path) -> run_file(path, hooks)
    Build(path, out, core:, ir:) -> build_file(path, out, core, ir)
    Usage(reason) -> Error(BadUsage(reason))
  }
}

pub fn main() -> Nil {
  let command = parse_args(get_script_args())
  case execute(command, host_hooks.default_host_hooks()) {
    Ok(Nil) -> Nil
    Error(err) -> {
      io.println_error(format_cli_error(err))
      halt(exit_code(err))
    }
  }
}
