import arc_aot.{Build, Help, Run, Usage}
import arc_aot/compile
import emit_2core_harness as harness
import gleam/option.{None, Some}
import gleam/string
import simplifile

pub fn parse_args_test() {
  assert arc_aot.parse_args([]) == Help
  assert arc_aot.parse_args(["help"]) == Help
  assert arc_aot.parse_args(["run", "a.js"]) == Run("a.js")
  assert arc_aot.parse_args(["run"]) == Usage(arc_aot.MissingFile("run"))
  assert arc_aot.parse_args(["build", "a.js"])
    == Build("a.js", None, core: False, ir: False)
  assert arc_aot.parse_args(["build", "a.js", "-o", "x.beam", "--core", "--ir"])
    == Build("a.js", Some("x.beam"), core: True, ir: True)
  assert arc_aot.parse_args(["build", "a.js", "--nope"])
    == Usage(arc_aot.UnknownFlag("--nope"))
  assert arc_aot.parse_args(["frob"]) == Usage(arc_aot.UnknownCommand("frob"))
}

pub fn module_name_for_test() {
  assert arc_aot.module_name_for("some/dir/my-file.test.js") == "my_file"
  assert arc_aot.module_name_for("out.beam") == "out"
}

pub fn run_prints_and_drains_test() {
  harness.buf_reset()
  let result =
    arc_aot.execute(Run("test/fixtures/hello.js"), harness.test_hooks())
  assert result == Ok(Nil)
  assert harness.buf_read() == <<"hello aot\nlater\n":utf8>>
}

pub fn run_uncaught_test() {
  let assert Error(err) =
    arc_aot.execute(Run("test/fixtures/throws.js"), harness.test_hooks())
  let assert arc_aot.ScriptThrew(report) = err
  assert string.starts_with(report, "Uncaught RangeError: boom")
  assert arc_aot.exit_code(err) == 1
}

pub fn run_module_goal_test() {
  let assert Error(err) =
    arc_aot.execute(Run("test/fixtures/esm.js"), harness.test_hooks())
  assert err
    == arc_aot.CompileFailed(
      "test/fixtures/esm.js",
      compile.ModuleGoalUnsupported,
    )
  assert arc_aot.exit_code(err) == 2
  assert string.contains(arc_aot.format_cli_error(err), "not supported")
}

pub fn run_missing_file_test() {
  let assert Error(arc_aot.ReadFailed(path: "nope.js", ..) as err) =
    arc_aot.execute(Run("nope.js"), harness.test_hooks())
  assert arc_aot.exit_code(err) == 1
}

pub fn usage_error_test() {
  let assert Error(err) =
    arc_aot.execute(arc_aot.parse_args(["build"]), harness.test_hooks())
  assert arc_aot.exit_code(err) == 2
  assert string.contains(arc_aot.format_cli_error(err), "Usage:")
}

pub fn build_writes_beam_core_ir_test() {
  let dir = "../build/aot_cli_test"
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let out = dir <> "/hello_built.beam"
  let result =
    arc_aot.execute(
      Build("test/fixtures/hello.js", Some(out), core: True, ir: True),
      harness.test_hooks(),
    )
  assert result == Ok(Nil)
  let assert Ok(beam) = simplifile.read_bits(out)
  assert beam != <<>>
  let assert Ok(core) = simplifile.read(dir <> "/hello_built.core")
  assert string.contains(core, "'hello_built'")
  let assert Ok(ir) = simplifile.read(dir <> "/hello_built.ir")
  assert string.starts_with(ir, "module @hello_built")
}
