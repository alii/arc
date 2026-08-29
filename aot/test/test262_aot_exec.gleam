import arc/host
import arc/host_hooks.{type HostHooks, HostHooks}
import arc/parser
import arc/rt/inspect as rt_inspect
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, DataProperty, KHandle, KStr, KUndef,
  ProxyObj, SObject, SShapedObject, StringKey, classify, mk_object, mk_string,
  mk_undefined,
}
import arc/rt/val as rt_val
import arc_aot/compile
import arc_aot/emit/state as emit_state
import arc_aot/run
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string
import simplifile
import test262_metadata.{type TestMetadata, Parse, Resolution, Runtime}
import test262_suite.{type StrictnessVariant}
import test_runner

const test_dir: String = "../vendor/test262/test"

const harness_dir: String = "../vendor/test262/harness"

const snapshot_path: String = "../.github/test262/pass-aot.txt"

const print_output: String = "__print_output__"

pub type Outcome {
  Pass
  Fail(reason: String)
  Skip(category: String)
}

pub type HarnessEntry {
  Loaded(module: Atom)
  Unsupported(feature: String)
  Broken(reason: String)
}

pub type Ctx {
  Ctx(
    harness: Dict(String, HarnessEntry),
    update_mode: Bool,
    has_snapshot: Bool,
    fail_log: Option(String),
  )
}

pub type Setup {
  Setup(ctx: Ctx, entries: List(#(String, Bool)))
}

pub type TestResult {
  TestResult(path: String, expected_pass: Bool, outcome: Outcome)
}

pub fn setup() -> Setup {
  let update_mode = test_runner.get_env_is_truthy("UPDATE_SNAPSHOT")
  let fail_log = test_runner.get_env("FAIL_LOG") |> option.from_result
  let snapshot = test262_suite.load_pass_list(snapshot_path)
  let files =
    test262_suite.list_test_files(test_dir) |> test262_suite.select_files
  check_atom_headroom(list.length(files))
  let harness = compile_harness(harness_needed(files))
  let ctx =
    Ctx(harness:, update_mode:, has_snapshot: set.size(snapshot) > 0, fail_log:)
  Setup(
    ctx:,
    entries: list.map(files, fn(f) { #(f, set.contains(snapshot, f)) }),
  )
}

// compiled var names intern as atoms, default table is 1M
const atoms_per_file: Int = 400

fn check_atom_headroom(file_count: Int) -> Nil {
  let needed = file_count * atoms_per_file
  let available = atom_limit() - atom_count()
  case available < needed {
    False -> Nil
    True ->
      panic as {
        "test262 aot: "
        <> int.to_string(file_count)
        <> " files need ~"
        <> int.to_string(needed)
        <> " atoms but only "
        <> int.to_string(available)
        <> " are left; run with ERL_FLAGS=\"+t "
        <> int.to_string(needed + atom_count() + 1_000_000)
        <> "\""
      }
  }
}

fn harness_needed(files: List(String)) -> List(String) {
  let always = ["assert.js", "sta.js", "doneprintHandle.js"]
  let named = case list.length(files) > 2000 {
    True ->
      case simplifile.get_files(harness_dir) {
        Ok(paths) ->
          list.filter(paths, string.ends_with(_, ".js"))
          |> list.map(string.replace(_, harness_dir <> "/", ""))
        Error(err) ->
          panic as {
            "cannot list " <> harness_dir <> ": " <> string.inspect(err)
          }
      }
    False ->
      list.flat_map(files, fn(f) {
        case simplifile.read(test_dir <> "/" <> f) {
          Ok(source) -> test262_metadata.parse_metadata(source).includes
          Error(_unreadable) -> []
        }
      })
  }
  list.append(always, named) |> list.unique
}

fn compile_harness(names: List(String)) -> Dict(String, HarnessEntry) {
  io.println(
    "test262 aot: compiling "
    <> int.to_string(list.length(names))
    <> " harness files",
  )
  pmap(names, fn(name) { #(name, compile_harness_file(name)) })
  |> dict.from_list
}

fn compile_harness_file(name: String) -> HarnessEntry {
  let module_name = "arc_aot_t262h_" <> sanitize(name)
  case simplifile.read(harness_dir <> "/" <> name) {
    Error(err) -> Broken("read: " <> string.inspect(err))
    Ok(source) ->
      case compile.script_to_ir(globalize_lexicals(source), module_name) {
        Error(emit_state.UnsupportedFeature(feature)) -> Unsupported(feature)
        Error(err) -> Broken(compile.describe_emit_error(err))
        Ok(module) ->
          case compile.ir_to_beam(module) {
            Error(err) -> Broken(compile.describe(err))
            Ok(beam) ->
              case run.load(beam, module_name) {
                Ok(loaded) -> Loaded(loaded)
                Error(reason) -> Broken("load: " <> reason)
              }
          }
      }
  }
}

// harness top-level let/const must land on the global object
fn globalize_lexicals(source: String) -> String {
  string.split(source, "\n")
  |> list.map(fn(line) {
    case line {
      "const " <> rest | "let " <> rest -> "var " <> rest
      _ -> line
    }
  })
  |> string.join("\n")
}

fn sanitize(name: String) -> String {
  string.replace(name, ".js", "")
  |> string.replace(".", "_")
  |> string.replace("-", "_")
  |> string.replace("/", "_")
}

pub fn run_file(ctx: Ctx, relative: String, module_base: String) -> Outcome {
  case simplifile.read(test_dir <> "/" <> relative) {
    Error(err) -> Fail("could not read file: " <> string.inspect(err))
    Ok(source) -> {
      let metadata = test262_metadata.parse_metadata(source)
      case list.contains(metadata.flags, "module") {
        True -> Skip("module")
        False -> run_variants(ctx, metadata, source, module_base)
      }
    }
  }
}

fn run_variants(
  ctx: Ctx,
  metadata: TestMetadata,
  source: String,
  module_base: String,
) -> Outcome {
  test262_suite.variants_for_test(metadata)
  |> list.index_map(fn(variant, i) { #(variant, i) })
  |> list.fold_until(Pass, fn(_acc, entry) {
    let #(variant, i) = entry
    let module_name = module_base <> "_" <> int.to_string(i)
    let outcome = case metadata.negative_phase {
      Some(Parse) -> run_parse_negative(source, variant)
      Some(Resolution) -> Fail("resolution-phase test without the module flag")
      Some(Runtime) | None ->
        run_compiled(ctx, metadata, source, variant, module_name)
    }
    case outcome {
      Pass -> list.Continue(Pass)
      Skip(category) -> list.Stop(Skip(category))
      Fail(reason) ->
        list.Stop(Fail(reason <> test262_suite.variant_label(variant)))
    }
  })
}

fn run_parse_negative(source: String, variant: StrictnessVariant) -> Outcome {
  case
    parser.parse(test262_suite.variant_source(source, variant), parser.Script)
  {
    Error(_expected) -> Pass
    Ok(_) -> Fail("expected parse error but parsed successfully")
  }
}

fn run_compiled(
  ctx: Ctx,
  metadata: TestMetadata,
  source: String,
  variant: StrictnessVariant,
  module_name: String,
) -> Outcome {
  let is_async = list.contains(metadata.flags, "async")
  let prepared = {
    use st <- result.try(prepare_agent(ctx, metadata, is_async))
    use module <- result.map(compile_test(
      test262_suite.variant_source(source, variant),
      module_name,
    ))
    #(st, module)
  }
  case prepared {
    Error(outcome) -> outcome
    Ok(#(st, module)) -> {
      let #(exec, st) = run.apply_main(module, st)
      run.unload(module)
      judge(metadata, is_async, exec, st)
    }
  }
}

fn prepare_agent(
  ctx: Ctx,
  metadata: TestMetadata,
  is_async: Bool,
) -> Result(Agent, Outcome) {
  let st = run.seed(hooks_for(metadata))
  case list.contains(metadata.flags, "raw") {
    True -> Ok(st)
    False -> {
      let st = install_host_api(st)
      test262_suite.harness_files(metadata, is_async)
      |> list.try_fold(st, fn(st, name) { run_harness(ctx, st, name) })
    }
  }
}

fn run_harness(ctx: Ctx, st: Agent, name: String) -> Result(Agent, Outcome) {
  case dict.get(ctx.harness, name) {
    Error(Nil) -> Error(Fail("harness " <> name <> " was not compiled"))
    Ok(Unsupported(feature)) ->
      Error(Skip("harness " <> name <> " unsupported: " <> feature))
    Ok(Broken(reason)) -> Error(Fail("harness " <> name <> ": " <> reason))
    Ok(Loaded(module)) ->
      case run.apply_main(module, st) {
        #(run.JsReturned(_), st) -> Ok(st)
        #(run.JsThrew(thrown), st) ->
          case emitter_rejection(thrown, st) {
            Some(feature) ->
              Error(Skip("harness " <> name <> " unsupported: " <> feature))
            None ->
              Error(Fail(
                "harness " <> name <> " threw: " <> inspect_thrown(thrown, st),
              ))
          }
        #(run.JsCrashed(reason), _st) ->
          Error(Fail("harness " <> name <> " crashed: " <> reason))
      }
  }
}

fn compile_test(source: String, module_name: String) -> Result(Atom, Outcome) {
  case compile.script_to_ir(source, module_name) {
    Error(emit_state.UnsupportedFeature(feature)) -> Error(Skip(feature))
    Error(err) -> Error(Fail(compile.describe_emit_error(err)))
    Ok(module) ->
      case compile.ir_to_beam(module) {
        Error(err) -> Error(Fail(compile.describe(err)))
        Ok(beam) ->
          run.load(beam, module_name)
          |> result.map_error(fn(reason) { Fail("load: " <> reason) })
      }
  }
}

fn judge(
  metadata: TestMetadata,
  is_async: Bool,
  exec: run.JsExecOutcome,
  st: Agent,
) -> Outcome {
  let rejected = case exec {
    run.JsThrew(thrown) -> emitter_rejection(thrown, st)
    run.JsReturned(_) if is_async ->
      case check_async_completion(st) {
        Error("async failure: TypeError: unsupported: " <> feature) ->
          Some(feature)
        _ -> None
      }
    _ -> None
  }
  case rejected {
    Some(feature) -> Skip(feature)
    None -> judge_completion(metadata, is_async, exec, st)
  }
}

// runtime "unsupported:" TypeError is the same static rejection
fn emitter_rejection(thrown: JsVal, st: Agent) -> Option(String) {
  use h <- option.then(as_handle(thrown))
  use name <- option.then(get_data(st, h, "name"))
  use message <- option.then(get_data(st, h, "message"))
  case classify(name), classify(message) {
    KStr("TypeError"), KStr("unsupported: " <> feature) -> Some(feature)
    _, _ -> None
  }
}

fn judge_completion(
  metadata: TestMetadata,
  is_async: Bool,
  exec: run.JsExecOutcome,
  st: Agent,
) -> Outcome {
  case metadata.negative_phase, exec {
    _, run.JsCrashed(reason) -> Fail("crashed: " <> reason)
    Some(_), run.JsThrew(thrown) -> verify_negative_type(metadata, thrown, st)
    Some(_), run.JsReturned(_) ->
      case is_async {
        False -> Fail("expected runtime throw but completed normally")
        True ->
          case check_async_completion(st) {
            Ok(Nil) -> Fail("expected runtime throw but async test completed")
            Error(msg) ->
              case
                string.contains(msg, option.unwrap(metadata.negative_type, ""))
              {
                True -> Pass
                False -> Fail("wrong async error: " <> msg)
              }
          }
      }
    None, run.JsThrew(thrown) ->
      Fail("unexpected throw: " <> inspect_thrown(thrown, st))
    None, run.JsReturned(_) ->
      case is_async {
        False -> Pass
        True ->
          case check_async_completion(st) {
            Ok(Nil) -> Pass
            Error(reason) -> Fail(reason)
          }
      }
  }
}

fn check_async_completion(st: Agent) -> Result(Nil, String) {
  case get_data(st, st.realm.global_object, print_output) {
    None -> Error("async test did not call $DONE (no __print_output__)")
    Some(output) ->
      case classify(output) {
        KStr("Test262:AsyncTestComplete") -> Ok(Nil)
        KStr("Test262:AsyncTestFailure:" <> msg) ->
          Error("async failure: " <> msg)
        KStr(other) -> Error("unexpected print output: " <> other)
        KUndef -> Error("async test did not call $DONE")
        _ ->
          Error(
            "unexpected __print_output__: " <> rt_inspect.inspect(st, output),
          )
      }
  }
}

fn verify_negative_type(
  metadata: TestMetadata,
  thrown: JsVal,
  st: Agent,
) -> Outcome {
  case metadata.negative_type {
    None -> Pass
    Some(expected) -> {
      let actual = {
        use h <- option.then(as_handle(thrown))
        use ctor <- option.then(get_data(st, h, "constructor"))
        use ctor <- option.then(as_handle(ctor))
        use name <- option.then(get_data(st, ctor, "name"))
        case classify(name) {
          KStr(n) -> Some(n)
          _ -> None
        }
      }
      case actual {
        Some(name) if name == expected -> Pass
        Some(name) ->
          Fail(
            "expected "
            <> expected
            <> " but got "
            <> name
            <> ": "
            <> inspect_thrown(thrown, st),
          )
        None ->
          Fail(
            "expected "
            <> expected
            <> " but got: "
            <> inspect_thrown(thrown, st),
          )
      }
    }
  }
}

fn hooks_for(metadata: TestMetadata) -> HostHooks {
  HostHooks(
    ..host_hooks.default_host_hooks(),
    can_block: !list.contains(metadata.flags, "CanBlockIsFalse"),
    report_uncaught: fn(_report) { Nil },
    print: fn(_level, _line) { Nil },
  )
}

fn install_host_api(st: Agent) -> Agent {
  let #(_dollar_262, st) = rt_realm.install_262(st, st.realm)
  let s: host.State(Nil) = host.from_agent(st, host.new_key())
  let s = host.define_global(s, print_output, mk_undefined())
  let s = host.define_fn(s, "print", 1, print_native)
  s.agent
}

fn print_native(
  args: List(JsVal),
  _this: JsVal,
  s: host.State(Nil),
) -> #(host.State(Nil), Result(JsVal, JsVal)) {
  let #(str, st) = rt_val.t_to_string(s.agent, host.first_arg(args))
  let #(k, st) = rt_store.t_key(st, print_output)
  let #(_ok, st) =
    rt_obj.t_set_prop(
      st,
      mk_object(st.realm.global_object),
      StringKey(k),
      mk_string(str),
    )
  #(host.State(..s, agent: st), Ok(mk_undefined()))
}

fn as_handle(v: JsVal) -> Option(Handle) {
  case classify(v) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}

fn ordinary_proto(st: Agent, h: Handle) -> Option(Handle) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind: ProxyObj(..), ..) -> None
    SObject(proto:, ..) | SShapedObject(proto:, ..) -> proto
    _ -> None
  }
}

fn get_data(st: Agent, h: Handle, key: String) -> Option(JsVal) {
  use k <- option.then(rt_store.t_find_key(st, key))
  case rt_obj.t_ordinary_own_property(st, h, StringKey(k)) {
    Some(DataProperty(value: val, ..)) -> Some(val)
    Some(_) -> None
    None -> option.then(ordinary_proto(st, h), get_data(st, _, key))
  }
}

fn inspect_thrown(val: JsVal, st: Agent) -> String {
  let described = {
    use h <- option.then(as_handle(val))
    use message <- option.then(get_data(st, h, "message"))
    case classify(message) {
      KStr(msg) -> {
        let name = case option.map(get_data(st, h, "name"), classify) {
          Some(KStr(n)) -> n
          _ -> "Error"
        }
        Some(name <> ": " <> msg)
      }
      _ -> None
    }
  }
  option.lazy_unwrap(described, fn() { rt_inspect.inspect(st, val) })
}

pub fn finish(ctx: Ctx, results: List(TestResult)) -> Int {
  let passes =
    list.filter(results, fn(r) { r.outcome == Pass })
    |> list.map(fn(r) { r.path })
    |> list.sort(string.compare)
  let fails =
    list.filter_map(results, fn(r) {
      case r.outcome {
        Fail(reason) -> Ok(#(r.path, reason))
        _ -> Error(Nil)
      }
    })
    |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
  let skips =
    list.fold(results, dict.new(), fn(acc, r) {
      case r.outcome {
        Skip(category) ->
          dict.upsert(acc, category, fn(n) { option.unwrap(n, 0) + 1 })
        _ -> acc
      }
    })
  let pass_count = list.length(passes)
  let fail_count = list.length(fails)
  let skip_count = dict.fold(skips, 0, fn(acc, _cat, n) { acc + n })

  io.println(
    "\n"
    <> test262_suite.summary_line(
      "test262 aot",
      pass_count,
      fail_count,
      skip_count,
    ),
  )
  io.println(
    "  atoms: "
    <> int.to_string(atom_count())
    <> " of "
    <> int.to_string(atom_limit()),
  )
  dict.to_list(skips)
  |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
  |> list.each(fn(entry) {
    io.println("  skip " <> int.to_string(entry.1) <> "  " <> entry.0)
  })

  write_fail_log(ctx, fails)
  write_snapshot(ctx, passes)
  write_results(pass_count, fail_count, skip_count)
  count_mismatches(ctx, results)
}

fn write_fail_log(ctx: Ctx, fails: List(#(String, String))) -> Nil {
  case ctx.fail_log {
    None -> Nil
    Some(path) -> {
      let lines = list.map(fails, fn(f) { f.0 <> "\t" <> f.1 <> "\n" })
      case simplifile.write(to: path, contents: string.concat(lines)) {
        Ok(Nil) -> io.println("Failures written to " <> path)
        Error(err) ->
          io.println(
            "Warning: could not write fail log: " <> string.inspect(err),
          )
      }
    }
  }
}

fn write_snapshot(ctx: Ctx, passes: List(String)) -> Nil {
  case ctx.update_mode {
    False -> Nil
    True ->
      case test262_suite.write_pass_list(snapshot_path, passes) {
        Ok(Nil) -> {
          io.println(
            "Snapshot updated: "
            <> snapshot_path
            <> " ("
            <> int.to_string(list.length(passes))
            <> " passing tests)",
          )
          case test262_suite.partial_run_env() {
            Some(name) ->
              io.println(
                "Warning: "
                <> name
                <> " is set, so "
                <> snapshot_path
                <> " now covers only that subset — do not commit it",
              )
            None -> Nil
          }
        }
        Error(err) ->
          io.println(
            "Warning: could not write snapshot: " <> string.inspect(err),
          )
      }
  }
}

fn write_results(pass: Int, fail: Int, skip: Int) -> Nil {
  case test_runner.get_env("RESULTS_FILE") {
    Error(Nil) -> Nil
    Ok(path) ->
      case
        simplifile.write(path, test262_suite.results_json(pass, fail, skip))
      {
        Ok(Nil) -> io.println("Results written to " <> path)
        Error(err) ->
          io.println(
            "Warning: could not write results: " <> string.inspect(err),
          )
      }
  }
}

pub fn is_mismatch(ctx: Ctx, result: TestResult) -> Bool {
  case ctx.update_mode || !ctx.has_snapshot {
    True -> False
    False ->
      case result.outcome {
        Pass -> !result.expected_pass
        Fail(_) -> result.expected_pass
        Skip(_) -> False
      }
  }
}

fn count_mismatches(ctx: Ctx, results: List(TestResult)) -> Int {
  list.count(results, is_mismatch(ctx, _))
}

@external(erlang, "arc_aot_test262_ffi", "pmap")
fn pmap(items: List(a), f: fn(a) -> b) -> List(b)

@external(erlang, "arc_aot_test262_ffi", "atom_count")
fn atom_count() -> Int

@external(erlang, "arc_aot_test262_ffi", "atom_limit")
fn atom_limit() -> Int
