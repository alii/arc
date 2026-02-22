/// Snapshot-based expected failure tracking for test262.
/// A snapshot file lists test paths that are expected to fail.
import gleam/list
import gleam/set.{type Set}
import gleam/string
import simplifile

/// Load a snapshot file, returning a set of paths expected to fail.
/// Returns an empty set if the file doesn't exist.
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

/// Write a snapshot file from a list of failing paths.
/// Paths are sorted alphabetically.
pub fn write_snapshot(path: String, failures: List(String)) -> Result(Nil, String) {
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

/// Check if a test path is in the snapshot (expected to fail).
pub fn is_expected_failure(snapshot: Set(String), path: String) -> Bool {
  set.contains(snapshot, path)
}
