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

/// Which multi-line block we're currently inside.
type BlockState {
  TopLevel
  InNegative
  InList(field: ListField)
}

type ListField {
  FlagsList
  IncludesList
  FeaturesList
}

fn parse_yaml_block(yaml: String) -> TestMetadata {
  let lines = string.split(yaml, "\n")
  parse_yaml_lines(lines, default_metadata(), TopLevel)
}

fn parse_yaml_lines(
  lines: List(String),
  meta: TestMetadata,
  block: BlockState,
) -> TestMetadata {
  case lines {
    [] -> meta
    [line, ..rest] -> {
      let trimmed = string.trim(line)
      case trimmed {
        "" -> parse_yaml_lines(rest, meta, block)
        "#" <> _ -> parse_yaml_lines(rest, meta, block)
        _ -> {
          let is_indented =
            string.starts_with(line, "  ") || string.starts_with(line, "\t")
          case block, is_indented {
            InNegative, True -> {
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
              parse_yaml_lines(rest, meta, InNegative)
            }
            InList(field), True ->
              case trimmed {
                "- " <> item -> {
                  let meta = append_list_item(meta, field, string.trim(item))
                  parse_yaml_lines(rest, meta, block)
                }
                _ -> parse_yaml_lines(rest, meta, block)
              }
            // In a block but hit non-indented line — exit and reprocess.
            _, False if block != TopLevel ->
              parse_yaml_lines([line, ..rest], meta, TopLevel)
            _, _ -> {
              let #(meta, next_block) = parse_top_level_field(trimmed, meta)
              parse_yaml_lines(rest, meta, next_block)
            }
          }
        }
      }
    }
  }
}

fn append_list_item(
  meta: TestMetadata,
  field: ListField,
  item: String,
) -> TestMetadata {
  case field {
    FlagsList -> TestMetadata(..meta, flags: list.append(meta.flags, [item]))
    IncludesList ->
      TestMetadata(..meta, includes: list.append(meta.includes, [item]))
    FeaturesList ->
      TestMetadata(..meta, features: list.append(meta.features, [item]))
  }
}

/// Parse a top-level YAML field. Returns updated meta and the next block
/// state — `InList(field)` when the value is empty (YAML list follows),
/// `InNegative` for `negative:`, `TopLevel` otherwise.
fn parse_top_level_field(
  trimmed: String,
  meta: TestMetadata,
) -> #(TestMetadata, BlockState) {
  case string.split_once(trimmed, ":") {
    Ok(#("negative", _)) -> #(meta, InNegative)
    Ok(#("flags", rest)) -> parse_array_field(meta, rest, FlagsList)
    Ok(#("includes", rest)) -> parse_array_field(meta, rest, IncludesList)
    Ok(#("features", rest)) -> parse_array_field(meta, rest, FeaturesList)
    _ -> #(meta, TopLevel)
  }
}

fn parse_array_field(
  meta: TestMetadata,
  rest: String,
  field: ListField,
) -> #(TestMetadata, BlockState) {
  case string.trim(rest) {
    "" -> #(meta, InList(field))
    _ -> {
      let items = parse_inline_array(rest)
      let meta = case field {
        FlagsList -> TestMetadata(..meta, flags: items)
        IncludesList -> TestMetadata(..meta, includes: items)
        FeaturesList -> TestMetadata(..meta, features: items)
      }
      #(meta, TopLevel)
    }
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
