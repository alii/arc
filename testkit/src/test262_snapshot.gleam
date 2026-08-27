import gleam/list
import gleam/set.{type Set}
import gleam/string
import simplifile

pub fn load_snapshot(path: String) -> Set(String) {
  case simplifile.read(path) {
    Error(_) -> set.new()
    Ok(contents) ->
      string.split(contents, "\n")
      |> list.filter(fn(line) {
        let trimmed = string.trim(line)
        trimmed != "" && !string.starts_with(trimmed, "#")
      })
      |> list.map(string.trim)
      |> set.from_list
  }
}

pub fn write_snapshot(
  path: String,
  failures: List(String),
) -> Result(Nil, String) {
  let sorted = list.sort(failures, string.compare)
  let contents =
    "# test262 expected failures snapshot\n# Auto-generated. One relative path per line.\n"
    <> string.join(sorted, "\n")
    <> "\n"
  case simplifile.write(path, contents) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error("Failed to write snapshot: " <> string.inspect(err))
  }
}

pub fn is_expected_failure(snapshot: Set(String), path: String) -> Bool {
  set.contains(snapshot, path)
}
