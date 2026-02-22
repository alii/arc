/// Parse test262 YAML frontmatter from test files.
/// Extracts negative phase/type and flags for test execution.
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type NegativePhase {
  Parse
  Resolution
  Runtime
}

pub type TestMetadata {
  TestMetadata(
    negative_phase: Option(NegativePhase),
    negative_type: Option(String),
    flags: List(String),
    includes: List(String),
    features: List(String),
  )
}

/// Parse metadata from a test262 source file.
/// Returns default metadata if no frontmatter found.
pub fn parse_metadata(source: String) -> TestMetadata {
  case string.split_once(source, "/*---") {
    Error(_) -> default_metadata()
    Ok(#(_, rest)) ->
      case string.split_once(rest, "---*/") {
        Error(_) -> default_metadata()
        Ok(#(yaml, _)) -> parse_yaml_block(yaml)
      }
  }
}

fn default_metadata() -> TestMetadata {
  TestMetadata(
    negative_phase: None,
    negative_type: None,
    flags: [],
    includes: [],
    features: [],
  )
}

fn parse_yaml_block(yaml: String) -> TestMetadata {
  let lines = string.split(yaml, "\n")
  parse_yaml_lines(lines, default_metadata(), False)
}

/// Walk YAML lines, tracking whether we're inside a `negative:` block.
fn parse_yaml_lines(
  lines: List(String),
  meta: TestMetadata,
  in_negative: Bool,
) -> TestMetadata {
  case lines {
    [] -> meta
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case trimmed {
        // Skip empty lines and comments
        "" -> parse_yaml_lines(rest, meta, in_negative)
        "#" <> _ -> parse_yaml_lines(rest, meta, in_negative)
        _ -> {
          // Check if this is an indented line (part of a block)
          let is_indented =
            string.starts_with(line, "  ") || string.starts_with(line, "\t")
          case in_negative, is_indented {
            // Inside negative block, reading indented fields
            True, True -> {
              let meta = case string.split_once(trimmed, ":") {
                Ok(#("phase", value)) -> {
                  let phase = case string.trim(value) {
                    "parse" -> Some(Parse)
                    "resolution" -> Some(Resolution)
                    "runtime" -> Some(Runtime)
                    _ -> None
                  }
                  TestMetadata(..meta, negative_phase: phase)
                }
                Ok(#("type", value)) ->
                  TestMetadata(..meta, negative_type: Some(string.trim(value)))
                _ -> meta
              }
              parse_yaml_lines(rest, meta, True)
            }
            // Was in negative block but hit non-indented line — exit block
            True, False -> parse_yaml_lines([line, ..rest], meta, False)
            // Not in negative block
            False, _ -> {
              case string.starts_with(trimmed, "negative:") {
                True -> parse_yaml_lines(rest, meta, True)
                False -> {
                  let meta = parse_top_level_field(trimmed, meta)
                  parse_yaml_lines(rest, meta, False)
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Parse a top-level YAML field that isn't `negative:`.
fn parse_top_level_field(trimmed: String, meta: TestMetadata) -> TestMetadata {
  case string.split_once(trimmed, ":") {
    Ok(#("flags", rest)) ->
      TestMetadata(..meta, flags: parse_inline_array(rest))
    Ok(#("includes", rest)) ->
      TestMetadata(..meta, includes: parse_inline_array(rest))
    Ok(#("features", rest)) ->
      TestMetadata(..meta, features: parse_inline_array(rest))
    _ -> meta
  }
}

/// Parse a YAML inline array from the value portion (after the colon).
/// e.g. " [module, onlyStrict]" → ["module", "onlyStrict"]
fn parse_inline_array(value: String) -> List(String) {
  case string.split_once(value, "[") {
    Error(_) -> []
    Ok(#(_, rest)) ->
      case string.split_once(rest, "]") {
        Error(_) -> []
        Ok(#(items, _)) ->
          string.split(items, ",")
          |> list.map(string.trim)
          |> list.filter(fn(s) { s != "" })
      }
  }
}
