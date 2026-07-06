//// Intl.Segmenter segmentation (root rules, approximate).
////
//// Pure `String`/`Granularity` → `List(Segment)` — no `State`. Indices are
//// UTF-16 code units, matching what JS length/indexing sees.

import arc/internal/digits
import arc/vm/value.{
  type Granularity, type Segment, GGrapheme, GSentence, GWord, Segment,
}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// The segments covering the string, in order.
pub fn segment_string(s: String, granularity: Granularity) -> List(Segment) {
  case granularity {
    GWord -> segment_words(s)
    GSentence -> segment_sentences(s)
    GGrapheme -> segment_graphemes(s)
  }
}

fn segment_graphemes(s: String) -> List(Segment) {
  string.to_graphemes(s)
  |> list.fold(#([], 0), fn(acc, g) {
    let #(parts, idx) = acc
    #([Segment(g, idx, False), ..parts], idx + utf16_len(g))
  })
  |> fn(acc) { list.reverse(acc.0) }
}

pub fn utf16_len(s: String) -> Int {
  string.to_utf_codepoints(s)
  |> list.fold(0, fn(n, cp) {
    case string.utf_codepoint_to_int(cp) > 0xffff {
      True -> n + 2
      False -> n + 1
    }
  })
}

fn is_word_char(g: String) -> Bool {
  case string.to_utf_codepoints(g) {
    [cp, ..] -> {
      let c = string.utf_codepoint_to_int(cp)
      digits.is_ascii_alnum_code(c) || c == 0x27 || c > 0x7f
    }
    [] -> False
  }
}

fn segment_words(s: String) -> List(Segment) {
  let graphemes = string.to_graphemes(s)
  segment_words_loop(graphemes, 0, [], "", 0, None)
}

fn segment_words_loop(
  rest: List(String),
  idx: Int,
  acc: List(Segment),
  current: String,
  current_start: Int,
  current_kind: Option(Bool),
) -> List(Segment) {
  case rest {
    [] ->
      case current {
        "" -> list.reverse(acc)
        _ ->
          list.reverse([
            Segment(current, current_start, option.unwrap(current_kind, False)),
            ..acc
          ])
      }
    [g, ..gs] -> {
      let kind = is_word_char(g)
      case current_kind {
        Some(k) if k == kind ->
          segment_words_loop(
            gs,
            idx + utf16_len(g),
            acc,
            current <> g,
            current_start,
            current_kind,
          )
        Some(k) ->
          segment_words_loop(
            gs,
            idx + utf16_len(g),
            [Segment(current, current_start, k), ..acc],
            g,
            idx,
            Some(kind),
          )
        None ->
          segment_words_loop(gs, idx + utf16_len(g), acc, g, idx, Some(kind))
      }
    }
  }
}

fn segment_sentences(s: String) -> List(Segment) {
  case s {
    "" -> []
    _ -> segment_sentences_loop(string.to_graphemes(s), 0, [], "", 0, False)
  }
}

fn segment_sentences_loop(
  rest: List(String),
  idx: Int,
  acc: List(Segment),
  current: String,
  current_start: Int,
  after_terminator: Bool,
) -> List(Segment) {
  case rest {
    [] ->
      case current {
        "" -> list.reverse(acc)
        _ -> list.reverse([Segment(current, current_start, False), ..acc])
      }
    [g, ..gs] -> {
      let next_idx = idx + utf16_len(g)
      let is_term = g == "." || g == "!" || g == "?"
      case after_terminator && !is_term && g != " " && g != "\n" {
        True ->
          // Start a new sentence at this grapheme.
          segment_sentences_loop(
            gs,
            next_idx,
            [Segment(current, current_start, False), ..acc],
            g,
            idx,
            False,
          )
        False ->
          segment_sentences_loop(
            gs,
            next_idx,
            acc,
            current <> g,
            current_start,
            after_terminator || is_term,
          )
      }
    }
  }
}
