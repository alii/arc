// date string parsing, same rules as v8's dateparser

import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type ParsedDate {
  ParsedDate(
    year: Int,
    // zero based
    month: Int,
    day: Int,
    hours: Int,
    minutes: Int,
    seconds: Int,
    ms: Int,
    // local minus utc, none means local time
    offset_minutes: Option(Int),
  )
}

type Keyword {
  MonthName
  TimeZoneName
  TimeSeparator
  AmPm
  OtherWord
}

type Token {
  // length counts leading zeros too
  Number(value: Int, length: Int)
  Colon
  Minus
  Plus
  Dot
  CloseParen
  Word(kind: Keyword, value: Int, length: Int)
  WhiteSpace
  Unknown
  End
}

type Day {
  Day(parts: List(Int), named_month: Option(Int), iso: Bool)
}

type Time {
  Time(parts: List(Int), hour_offset: Option(Int))
}

type Zone {
  Zone(sign: Option(Int), hour: Option(Int), minute: Option(Int))
}

type State {
  State(day: Day, time: Time, zone: Zone, has_read_number: Bool)
}

type IsoResult {
  IsoInvalid
  IsoComplete(State)
  // the legacy rules take over from this token
  IsoUnhandled(token: Token, rest: BitArray, state: State)
}

const max_significant_digits = 9

const max_offset_seconds = 2_147_483_647

pub fn parse(text: String) -> Option(ParsedDate) {
  case parse_iso(bit_array.from_string(text)) {
    IsoInvalid -> None
    IsoComplete(st) -> finish(st)
    IsoUnhandled(token:, rest:, state: st) ->
      legacy(token, rest, State(..st, has_read_number: st.day.parts != []))
      |> option.then(finish)
  }
}

fn scan(input: BitArray) -> #(Token, BitArray) {
  case input {
    <<>> | <<0, _:bits>> -> #(End, input)
    <<c, _:bits>> if c >= 0x30 && c <= 0x39 -> scan_number(input, 0, 0, 0)
    <<":":utf8, rest:bits>> -> #(Colon, rest)
    <<"-":utf8, rest:bits>> -> #(Minus, rest)
    <<"+":utf8, rest:bits>> -> #(Plus, rest)
    <<".":utf8, rest:bits>> -> #(Dot, rest)
    <<")":utf8, rest:bits>> -> #(CloseParen, rest)
    <<"(":utf8, _:bits>> -> #(Unknown, skip_parentheses(input, 0))
    <<cp:utf8_codepoint, rest:bits>> -> {
      let code = string.utf_codepoint_to_int(cp)
      let space = is_unicode_space(code)
      case code >= 0x41 && !space, space || is_ascii_space(code) {
        True, _ -> scan_word(input, <<>>, 0)
        False, True -> #(WhiteSpace, rest)
        False, False -> #(Unknown, rest)
      }
    }
    _ -> #(End, input)
  }
}

fn scan_number(
  input: BitArray,
  value: Int,
  significant: Int,
  length: Int,
) -> #(Token, BitArray) {
  case input {
    <<c, rest:bits>> if c >= 0x30 && c <= 0x39 ->
      case c == 0x30 && significant == 0, significant < max_significant_digits {
        True, _ -> scan_number(rest, 0, 0, length + 1)
        False, True ->
          scan_number(rest, value * 10 + c - 0x30, significant + 1, length + 1)
        False, False -> scan_number(rest, value, significant + 1, length + 1)
      }
    _ -> #(Number(value:, length:), input)
  }
}

fn scan_word(
  input: BitArray,
  prefix: BitArray,
  length: Int,
) -> #(Token, BitArray) {
  case input {
    <<cp:utf8_codepoint, rest:bits>> -> {
      let code = string.utf_codepoint_to_int(cp)
      case code >= 0x41 && !is_unicode_space(code) {
        True -> scan_word(rest, extend_prefix(prefix, code, length), length + 1)
        False -> #(keyword(prefix, length), input)
      }
    }
    _ -> #(keyword(prefix, length), input)
  }
}

fn extend_prefix(prefix: BitArray, code: Int, length: Int) -> BitArray {
  case length < 3 {
    False -> prefix
    True -> {
      let byte = case code {
        _ if code >= 0x41 && code <= 0x5A -> code + 0x20
        _ if code < 0x80 -> code
        // never part of a keyword
        _ -> 0xFF
      }
      <<prefix:bits, byte>>
    }
  }
}

fn keyword(prefix: BitArray, length: Int) -> Token {
  case prefix {
    <<"jan":utf8>> -> Word(MonthName, 1, length)
    <<"feb":utf8>> -> Word(MonthName, 2, length)
    <<"mar":utf8>> -> Word(MonthName, 3, length)
    <<"apr":utf8>> -> Word(MonthName, 4, length)
    <<"may":utf8>> -> Word(MonthName, 5, length)
    <<"jun":utf8>> -> Word(MonthName, 6, length)
    <<"jul":utf8>> -> Word(MonthName, 7, length)
    <<"aug":utf8>> -> Word(MonthName, 8, length)
    <<"sep":utf8>> -> Word(MonthName, 9, length)
    <<"oct":utf8>> -> Word(MonthName, 10, length)
    <<"nov":utf8>> -> Word(MonthName, 11, length)
    <<"dec":utf8>> -> Word(MonthName, 12, length)
    // only month names may run past three letters
    _ if length > 3 -> Word(OtherWord, 0, length)
    <<"am":utf8>> -> Word(AmPm, 0, length)
    <<"pm":utf8>> -> Word(AmPm, 12, length)
    <<"ut":utf8>> | <<"utc":utf8>> | <<"gmt":utf8>> | <<"z":utf8>> ->
      Word(TimeZoneName, 0, length)
    <<"cdt":utf8>> -> Word(TimeZoneName, -5, length)
    <<"cst":utf8>> -> Word(TimeZoneName, -6, length)
    <<"edt":utf8>> -> Word(TimeZoneName, -4, length)
    <<"est":utf8>> -> Word(TimeZoneName, -5, length)
    <<"mdt":utf8>> -> Word(TimeZoneName, -6, length)
    <<"mst":utf8>> -> Word(TimeZoneName, -7, length)
    <<"pdt":utf8>> -> Word(TimeZoneName, -7, length)
    <<"pst":utf8>> -> Word(TimeZoneName, -8, length)
    <<"t":utf8>> -> Word(TimeSeparator, 0, length)
    _ -> Word(OtherWord, 0, length)
  }
}

// an unclosed comment runs to the end
fn skip_parentheses(input: BitArray, balance: Int) -> BitArray {
  case input {
    <<>> | <<0, _:bits>> -> input
    <<"(":utf8, rest:bits>> -> skip_parentheses(rest, balance + 1)
    <<")":utf8, rest:bits>> ->
      case balance > 1 {
        True -> skip_parentheses(rest, balance - 1)
        False -> rest
      }
    <<_, rest:bits>> -> skip_parentheses(rest, balance)
    _ -> input
  }
}

fn is_ascii_space(code: Int) -> Bool {
  code == 0x20 || { code >= 0x09 && code <= 0x0D }
}

fn is_unicode_space(code: Int) -> Bool {
  code == 0xA0
  || code == 0x1680
  || { code >= 0x2000 && code <= 0x200A }
  || code == 0x202F
  || code == 0x205F
  || code == 0x3000
  || code == 0xFEFF
}

fn peek(input: BitArray) -> Token {
  scan(input).0
}

fn skip(input: BitArray, symbol: Token) -> Option(BitArray) {
  let #(token, rest) = scan(input)
  case token == symbol {
    True -> Some(rest)
    False -> None
  }
}

fn skip_if_present(input: BitArray, symbol: Token) -> BitArray {
  skip(input, symbol) |> option.unwrap(input)
}

fn is_zulu(token: Token) -> Bool {
  token == Word(TimeZoneName, 0, 1)
}

fn between(n: Int, low: Int, high: Int) -> Bool {
  n >= low && n <= high
}

fn is_day(n: Int) -> Bool {
  between(n, 1, 31)
}

fn add_day(st: State, n: Int) -> Option(State) {
  case st.day.parts {
    [_, _, _, ..] -> None
    parts ->
      Some(State(..st, day: Day(..st.day, parts: list.append(parts, [n]))))
  }
}

fn add_time(time: Time, n: Int) -> Option(Time) {
  case time.parts {
    [_, _, _, _, ..] -> None
    parts -> Some(Time(..time, parts: list.append(parts, [n])))
  }
}

// a full clock ignores it
fn add_final_time(time: Time, n: Int) -> Time {
  case add_time(time, n) {
    None -> time
    Some(Time(parts:, hour_offset:)) ->
      Time(parts: pad(parts, 4, 0), hour_offset:)
  }
}

fn pad(parts: List(Int), size: Int, with: Int) -> List(Int) {
  list.append(parts, list.repeat(with, size - list.length(parts)))
}

fn time_expects(time: Time, n: Int) -> Bool {
  case time.parts {
    [_] | [_, _] -> between(n, 0, 59)
    [_, _, _] -> between(n, 0, 999)
    _ -> False
  }
}

fn zone_expects(zone: Zone, n: Int) -> Bool {
  option.is_some(zone.hour) && option.is_none(zone.minute) && between(n, 0, 59)
}

fn named_zone(hours: Int) -> Zone {
  case hours < 0 {
    True -> Zone(sign: Some(-1), hour: Some(0 - hours), minute: Some(0))
    False -> Zone(sign: Some(1), hour: Some(hours), minute: Some(0))
  }
}

// first three digits as written, leading zeros count
fn milliseconds(value: Int, length: Int) -> Int {
  case length {
    1 -> value * 100
    2 -> value * 10
    3 -> value
    _ -> shrink(value, int_min(length, max_significant_digits))
  }
}

fn shrink(value: Int, length: Int) -> Int {
  case length > 3 {
    True -> shrink(value / 10, length - 1)
    False -> value
  }
}

fn int_min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
  }
}

fn parse_iso(input: BitArray) -> IsoResult {
  let st =
    State(
      day: Day(parts: [], named_month: None, iso: False),
      time: Time(parts: [], hour_offset: None),
      zone: Zone(sign: None, hour: None, minute: None),
      has_read_number: False,
    )
  let #(first, rest) = scan(input)
  case first, scan(rest) {
    // year zero has no minus sign, the digits are dropped
    Minus, #(Number(value: 0, length: 6), after) ->
      IsoUnhandled(first, after, st)
    Minus, #(Number(value: year, length: 6), after) ->
      iso_month(after, with_day(st, 0 - year))
    Plus, #(Number(value: year, length: 6), after) ->
      iso_month(after, with_day(st, year))
    Number(value: year, length: 4), _ -> iso_month(rest, with_day(st, year))
    _, _ -> IsoUnhandled(first, rest, st)
  }
}

fn with_day(st: State, n: Int) -> State {
  State(..st, day: Day(..st.day, parts: list.append(st.day.parts, [n])))
}

fn iso_month(input: BitArray, st: State) -> IsoResult {
  case skip(input, Minus) {
    None -> iso_time(input, st)
    Some(rest) ->
      case scan(rest) {
        #(Number(value: month, length: 2), after) if month >= 1 && month <= 12 ->
          iso_day(after, with_day(st, month))
        #(token, after) -> IsoUnhandled(token, after, st)
      }
  }
}

fn iso_day(input: BitArray, st: State) -> IsoResult {
  case skip(input, Minus) {
    None -> iso_time(input, st)
    Some(rest) ->
      case scan(rest) {
        #(Number(value: day, length: 2), after) if day >= 1 && day <= 31 ->
          iso_time(after, with_day(st, day))
        #(token, after) -> IsoUnhandled(token, after, st)
      }
  }
}

fn iso_time(input: BitArray, st: State) -> IsoResult {
  case scan(input) {
    #(Word(kind: TimeSeparator, ..), rest) ->
      iso_clock(rest, st)
      |> option.map(iso_complete)
      |> option.unwrap(IsoInvalid)
    #(End, _) -> iso_complete(st)
    #(token, rest) -> IsoUnhandled(token, rest, st)
  }
}

// date only is utc, date and time is local
fn iso_complete(st: State) -> IsoResult {
  let zone = case st.zone.hour, st.time.parts {
    None, [] -> named_zone(0)
    _, _ -> st.zone
  }
  IsoComplete(State(..st, zone:, day: Day(..st.day, iso: True)))
}

fn two_digits(input: BitArray, high: Int) -> Option(#(Int, BitArray)) {
  case scan(input) {
    #(Number(value:, length: 2), rest) if value <= high -> Some(#(value, rest))
    _ -> None
  }
}

fn iso_clock(input: BitArray, st: State) -> Option(State) {
  use #(hour, rest) <- option.then(two_digits(input, 24))
  // 24 is midnight, so nothing after it may be set
  let high = fn(limit) {
    case hour {
      24 -> 0
      _ -> limit
    }
  }
  use rest <- option.then(skip(rest, Colon))
  use #(minute, rest) <- option.then(two_digits(rest, high(59)))
  use #(parts, rest) <- option.then(case skip(rest, Colon) {
    None -> Some(#([hour, minute], rest))
    Some(rest) -> {
      use #(second, rest) <- option.then(two_digits(rest, high(59)))
      case skip(rest, Dot) {
        None -> Some(#([hour, minute, second], rest))
        Some(rest) ->
          case scan(rest), hour {
            #(Number(value:, ..), _), 24 if value > 0 -> None
            #(Number(value:, length:), rest), _ ->
              Some(#([hour, minute, second, milliseconds(value, length)], rest))
            _, _ -> None
          }
      }
    }
  })
  use #(zone, rest) <- option.then(iso_zone(rest, st.zone))
  case peek(rest) {
    End -> Some(State(..st, time: Time(..st.time, parts:), zone:))
    _ -> None
  }
}

fn iso_zone(input: BitArray, zone: Zone) -> Option(#(Zone, BitArray)) {
  let #(token, rest) = scan(input)
  case token, is_zulu(token) {
    _, True -> Some(#(named_zone(0), rest))
    Plus, _ -> iso_offset(rest, 1)
    Minus, _ -> iso_offset(rest, -1)
    _, _ -> Some(#(zone, input))
  }
}

fn iso_offset(input: BitArray, sign: Int) -> Option(#(Zone, BitArray)) {
  let offset = fn(hour, minute, rest) {
    case hour <= 23 && minute <= 59 {
      True -> Some(#(Zone(Some(sign), Some(hour), Some(minute)), rest))
      False -> None
    }
  }
  case scan(input) {
    #(Number(value:, length: 4), rest) -> offset(value / 100, value % 100, rest)
    #(Number(value: hour, length: 2), rest) -> {
      use rest <- option.then(skip(rest, Colon))
      use #(minute, rest) <- option.then(two_digits(rest, 59))
      offset(hour, minute, rest)
    }
    _ -> None
  }
}

fn legacy(token: Token, rest: BitArray, st: State) -> Option(State) {
  let reads_offset =
    st.zone.hour == Some(0) && st.zone.minute == Some(0) || st.time.parts != []
  case token {
    End -> Some(st)
    Number(value:, ..) ->
      legacy_number(value, rest, State(..st, has_read_number: True))
    Word(kind:, value:, ..) -> legacy_word(kind, value, rest, st)
    Plus if reads_offset -> legacy_offset(1, rest, st)
    Minus if reads_offset -> legacy_offset(-1, rest, st)
    Plus | Minus | CloseParen if st.has_read_number -> None
    _ -> legacy_next(rest, st)
  }
}

fn legacy_next(input: BitArray, st: State) -> Option(State) {
  let #(token, rest) = scan(input)
  legacy(token, rest, st)
}

fn legacy_number(n: Int, input: BitArray, st: State) -> Option(State) {
  case skip(input, Colon) {
    Some(rest) -> legacy_clock_part(n, rest, st)
    None -> {
      let dotted = skip(input, Dot)
      let rest = option.unwrap(dotted, input)
      let timed = time_expects(st.time, n)
      case option.is_some(dotted) && timed, zone_expects(st.zone, n), timed {
        True, _, _ ->
          case scan(rest) {
            #(Number(value:, length:), rest) -> {
              use time <- option.then(add_time(st.time, n))
              let time = add_final_time(time, milliseconds(value, length))
              legacy_next(rest, State(..st, time:))
            }
            _ -> None
          }
        False, True, _ ->
          legacy_next(rest, State(..st, zone: Zone(..st.zone, minute: Some(n))))
        False, False, True -> {
          let st = State(..st, time: add_final_time(st.time, n))
          let next = peek(rest)
          case next, is_zulu(next) {
            End, _ | WhiteSpace, _ | Plus, _ | Minus, _ | _, True ->
              legacy_next(rest, st)
            _, False -> None
          }
        }
        False, False, False -> {
          use st <- option.then(add_day(st, n))
          legacy_next(skip_if_present(rest, Minus), st)
        }
      }
    }
  }
}

// the number before a colon
fn legacy_clock_part(n: Int, input: BitArray, st: State) -> Option(State) {
  case skip(input, Colon), st.time.parts {
    Some(rest), [] ->
      legacy_next(rest, State(..st, time: Time(..st.time, parts: [n, 0])))
    Some(_), _ -> None
    None, _ -> {
      use time <- option.then(add_time(st.time, n))
      legacy_next(skip_if_present(input, Dot), State(..st, time:))
    }
  }
}

fn legacy_word(
  kind: Keyword,
  value: Int,
  input: BitArray,
  st: State,
) -> Option(State) {
  case kind, st.time.parts, st.has_read_number {
    AmPm, [_, ..], _ ->
      legacy_next(
        input,
        State(..st, time: Time(..st.time, hour_offset: Some(value))),
      )
    MonthName, _, _ ->
      legacy_next(
        skip_if_present(input, Minus),
        State(..st, day: Day(..st.day, named_month: Some(value))),
      )
    TimeZoneName, _, True ->
      legacy_next(input, State(..st, zone: named_zone(value)))
    _, _, True -> None
    _, _, False ->
      case peek(input) {
        Number(..) -> None
        _ -> legacy_next(input, st)
      }
  }
}

fn legacy_offset(sign: Int, input: BitArray, st: State) -> Option(State) {
  let #(n, length, rest) = case scan(input) {
    #(Number(value:, length:), rest) -> #(value, length, rest)
    _ -> #(0, 0, input)
  }
  use zone <- option.then(case peek(rest), length {
    Colon, _ -> Some(Zone(sign: Some(sign), hour: Some(n), minute: None))
    _, 1 | _, 2 -> Some(Zone(sign: Some(sign), hour: Some(n), minute: Some(0)))
    _, 3 | _, 4 ->
      Some(Zone(sign: Some(sign), hour: Some(n / 100), minute: Some(n % 100)))
    _, _ -> None
  })
  legacy_next(rest, State(..st, zone:, has_read_number: True))
}

fn finish(st: State) -> Option(ParsedDate) {
  use #(year, month, day) <- option.then(finish_day(st.day))
  use #(hours, minutes, seconds, ms) <- option.then(finish_time(st.time))
  use offset_minutes <- option.map(finish_zone(st.zone))
  ParsedDate(
    year:,
    month:,
    day:,
    hours:,
    minutes:,
    seconds:,
    ms:,
    offset_minutes:,
  )
}

fn finish_day(day: Day) -> Option(#(Int, Int, Int)) {
  case pad(day.parts, 3, 1), day.parts {
    _, [] -> None
    [a, b, c], _ -> {
      let #(year, month, date) = case day.named_month, day.iso || !is_day(a) {
        None, True -> #(a, b, c)
        None, False -> #(c, a, b)
        Some(month), _ ->
          case is_day(a) {
            True -> #(b, month, a)
            False -> #(a, month, b)
          }
      }
      let year = case day.iso {
        False if year >= 0 && year <= 49 -> year + 2000
        False if year >= 50 && year <= 99 -> year + 1900
        _ -> year
      }
      case between(month, 1, 12) && is_day(date) {
        True -> Some(#(year, month - 1, date))
        False -> None
      }
    }
    _, _ -> None
  }
}

fn finish_time(time: Time) -> Option(#(Int, Int, Int, Int)) {
  case pad(time.parts, 4, 0) {
    [hour, minute, second, ms] -> {
      use hour <- option.then(case time.hour_offset {
        None -> Some(hour)
        Some(offset) if hour >= 0 && hour <= 12 -> Some(hour % 12 + offset)
        Some(_) -> None
      })
      let in_range =
        between(hour, 0, 23)
        && between(minute, 0, 59)
        && between(second, 0, 59)
        && between(ms, 0, 999)
      let midnight = hour == 24 && minute == 0 && second == 0 && ms == 0
      case in_range || midnight {
        True -> Some(#(hour, minute, second, ms))
        False -> None
      }
    }
    _ -> None
  }
}

fn finish_zone(zone: Zone) -> Option(Option(Int)) {
  case zone.sign {
    None -> Some(None)
    Some(sign) -> {
      let hour = option.unwrap(zone.hour, 0)
      let minute = option.unwrap(zone.minute, 0)
      case hour * 3600 + minute * 60 > max_offset_seconds {
        True -> None
        False -> Some(Some(sign * { hour * 60 + minute }))
      }
    }
  }
}
