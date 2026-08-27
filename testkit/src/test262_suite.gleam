import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/set.{type Set}
import gleam/string
import simplifile
import test262_metadata.{type TestMetadata}
import test_runner

pub type StrictnessVariant {
  NonStrict
  Strict
}

pub fn variants_for_test(metadata: TestMetadata) -> List(StrictnessVariant) {
  let has = fn(flag) { list.contains(metadata.flags, flag) }
  case has("onlyStrict") {
    True -> [Strict]
    False ->
      case has("noStrict") || has("raw") || has("module") {
        True -> [NonStrict]
        False -> [NonStrict, Strict]
      }
  }
}

pub fn variant_source(source: String, variant: StrictnessVariant) -> String {
  case variant {
    Strict -> "\"use strict\";\n" <> source
    NonStrict -> source
  }
}

pub fn variant_label(variant: StrictnessVariant) -> String {
  case variant {
    Strict -> " (strict)"
    NonStrict -> " (non-strict)"
  }
}

pub fn harness_files(metadata: TestMetadata, is_async: Bool) -> List(String) {
  let default_harness = ["assert.js", "sta.js"]
  let async_harness = case is_async {
    True -> ["doneprintHandle.js"]
    False -> []
  }
  let extra =
    list.filter(metadata.includes, fn(f) {
      !list.contains(default_harness, f) && !list.contains(async_harness, f)
    })
  list.flatten([default_harness, async_harness, extra])
}

@external(erlang, "test_runner_ffi", "list_test_files")
pub fn list_test_files(dir: String) -> List(String)

pub fn select_files(files: List(String)) -> List(String) {
  let listed = case test_runner.get_env("TEST262_LIST") {
    Ok("") | Error(Nil) -> files
    Ok(path) -> {
      let wanted = load_pass_list(path)
      list.filter(files, set.contains(wanted, _))
    }
  }
  let filtered = case test_runner.get_env("TEST262_FILTER") {
    Ok("") | Error(Nil) -> listed
    Ok(filter) -> list.filter(listed, string.contains(_, filter))
  }
  case test_runner.get_env("TEST262_SHARD") {
    Ok("") | Error(Nil) -> filtered
    Ok(spec) -> {
      let #(k, n) = parse_shard(spec)
      list.filter(filtered, fn(f) { phash2(f, n) == k })
    }
  }
}

fn parse_shard(spec: String) -> #(Int, Int) {
  let parsed = case string.split(spec, "/") {
    [k, n] ->
      case int.parse(k), int.parse(n) {
        Ok(k), Ok(n) if n > 0 && k >= 0 && k < n -> Ok(#(k, n))
        _, _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
  case parsed {
    Ok(shard) -> shard
    Error(Nil) -> panic as { "bad TEST262_SHARD: " <> spec }
  }
}

@external(erlang, "erlang", "phash2")
fn phash2(term: String, range: Int) -> Int

pub fn partial_run_env() -> Option(String) {
  ["TEST262_SHARD", "TEST262_FILTER", "TEST262_LIST"]
  |> list.find(fn(name) {
    case test_runner.get_env(name) {
      Ok("") -> False
      Ok(_) -> True
      Error(Nil) -> False
    }
  })
  |> option.from_result
}

pub fn load_pass_list(path: String) -> Set(String) {
  case simplifile.read(path) {
    Ok(content) ->
      content
      |> string.split("\n")
      |> list.filter(fn(line) { line != "" })
      |> set.from_list
    Error(simplifile.Enoent) -> set.new()
    Error(err) ->
      panic as { "cannot read " <> path <> ": " <> string.inspect(err) }
  }
}

pub fn write_pass_list(
  path: String,
  paths: List(String),
) -> Result(Nil, simplifile.FileError) {
  simplifile.write(to: path, contents: string.join(paths, "\n") <> "\n")
}

pub fn format_percent(pass: Int, tested: Int) -> String {
  case tested > 0 {
    True -> {
      let pct_x100 = { pass * 10_000 } / tested
      let whole = pct_x100 / 100
      let frac = pct_x100 % 100
      int.to_string(whole)
      <> "."
      <> case frac < 10 {
        True -> "0" <> int.to_string(frac)
        False -> int.to_string(frac)
      }
    }
    False -> "0.00"
  }
}

pub fn results_json(pass: Int, fail: Int, skip: Int) -> String {
  let tested = pass + fail
  "{\"pass\":"
  <> int.to_string(pass)
  <> ",\"fail\":"
  <> int.to_string(fail)
  <> ",\"skip\":"
  <> int.to_string(skip)
  <> ",\"total\":"
  <> int.to_string(tested + skip)
  <> ",\"tested\":"
  <> int.to_string(tested)
  <> ",\"percent\":"
  <> format_percent(pass, tested)
  <> "}"
}

pub fn summary_line(label: String, pass: Int, fail: Int, skip: Int) -> String {
  let tested = pass + fail
  label
  <> ": "
  <> int.to_string(pass)
  <> " pass, "
  <> int.to_string(fail)
  <> " fail, "
  <> int.to_string(skip)
  <> " skip ("
  <> format_percent(pass, tested)
  <> "% of "
  <> int.to_string(tested)
  <> " tested)"
}
