//// Calendar-aware pieces shared by the calendared Temporal types
//// (PlainDate, PlainDateTime, PlainYearMonth, PlainMonthDay, ZonedDateTime):
//// calendar identifiers, property-bag date-field readers, CalendarResolveFields
//// / CalendarDateToISO, CalendarDateAdd / CalendarDateUntil, the ISO
//// year-month and month-day string forms, and NudgeToCalendarUnit's
//// `round_between`.
////
//// The unit/rounding/duration layer is temporal_common.gleam; pure ISO math
//// is temporal_iso.gleam; calendars are temporal_calendar.

import arc/internal/digits.{take_digits}
import arc/internal/gregorian.{days_in_month}
import arc/internal/int_math.{floor_div, floor_mod as math_mod, trunc_div}
import arc/internal/temporal_calendar as tcal
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  type RoundingMode, RHalfEven, RHalfInfinity, RHalfZero, RInfinity, RZero,
  negate_dur, read_int_field, read_pos_int_field, temporal_data_of, terr,
  time_only_ns, to_temporal_duration, unsigned_rounding_mode,
}
import arc/rt/builtins/temporal_iso.{
  type DurRec, type IsoDate, type Overflow, type ParsedIso, type TErr, Constrain,
  NoOffset, NumericOffset, RangeE, Reject, TypeE, Zulu, check_date_limits,
  epoch_days, int_sign, is_valid_iso_date, iso_date_from_epoch_days,
  iso_date_within_limits, iso_year_month_within_limits, pad2, parse_annotations,
  parse_iso_datetime_string, parse_offset_part, parse_time_part, parse_year_part,
  regulate_iso_date,
}
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type TemporalData, HintString, JInt,
  KHandle, KStr, KUndef, Named, StringKey, TemporalDate, TemporalDateTime,
  TemporalDuration, TemporalInstant, TemporalMonthDay, TemporalTime,
  TemporalYearMonth, TemporalZonedDateTime, classify, mk_number, mk_object,
  mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub fn int_val(i: Int) -> JsVal {
  mk_number(JInt(i))
}

/// Get(O, key) for a string key.
pub fn get_named(st: Agent, h: Handle, key: String) -> #(JsVal, Agent) {
  rt_obj.t_get_prop(st, mk_object(h), StringKey(Named(key)))
}

// ============================================================================
// Slots
// ============================================================================

/// The [[Calendar]] of a Temporal object that has one.
pub fn calendar_slot_of(data: TemporalData) -> Option(tcal.Calendar) {
  case data {
    TemporalDate(calendar:, ..)
    | TemporalDateTime(calendar:, ..)
    | TemporalYearMonth(calendar:, ..)
    | TemporalMonthDay(calendar:, ..)
    | TemporalZonedDateTime(calendar:, ..) -> Some(calendar)
    TemporalTime(..) | TemporalDuration(..) | TemporalInstant(..) -> None
  }
}

// ============================================================================
// Options
// ============================================================================

/// Validate options object + read overflow (result often unused for
/// instance copies, but the validation is observable).
pub fn validated_overflow(st: Agent, options: JsVal) -> #(Overflow, Agent) {
  let #(opts, st) = temporal_common.get_options_object(st, options)
  temporal_common.get_overflow_option(st, opts)
}

/// Shared preamble for the `add`/`subtract` instance methods: read the duration
/// argument, validate the options bag's `overflow`, and negate the duration
/// when the method is `subtract`.
pub fn add_sub_args(
  st: Agent,
  args: List(JsVal),
  is_subtract: Bool,
) -> #(DurRec, Overflow, Agent) {
  let #(dur, st) = to_temporal_duration(st, helpers.arg_at(args, 0))
  let #(overflow, st) = validated_overflow(st, helpers.arg_at(args, 1))
  let dur = case is_subtract {
    True -> negate_dur(dur)
    False -> dur
  }
  #(dur, overflow, st)
}

// ============================================================================
// Calendar identifiers (delegates to temporal_calendar's CLDR calendar set)
// ============================================================================

/// ToTemporalCalendarIdentifier / CanonicalizeCalendar for a bare identifier:
/// case-insensitive lookup + alias resolution via tcal.canonicalize.
/// RangeError for identifiers outside temporal_calendar's supported set.
pub fn canonicalize_calendar(id: String) -> Result(tcal.Calendar, TErr) {
  case tcal.canonicalize(id) {
    Ok(c) -> Ok(c)
    Error(Nil) -> Error(RangeE("calendar " <> id <> " is not supported"))
  }
}

/// ToTemporalCalendarIdentifier for string inputs: either a bare calendar id,
/// or an ISO date/date-time/year-month/month-day/time string whose [u-ca=]
/// annotation (default iso8601) supplies the calendar.
pub fn calendar_from_string(s: String) -> Result(tcal.Calendar, TErr) {
  case canonicalize_calendar(s) {
    Ok(c) -> Ok(c)
    Error(e) ->
      case extract_calendar_annotation(s) {
        Some(cal) -> canonicalize_calendar(cal)
        None -> Error(e)
      }
  }
}

/// If `s` parses as some ISO 8601 Temporal string, return its calendar
/// annotation value (or "iso8601" when absent). None when not parseable.
fn extract_calendar_annotation(s: String) -> Option(String) {
  case parse_iso_datetime_string(s) {
    Some(p) -> Some(option.unwrap(p.calendar, "iso8601"))
    None -> {
      // Try time-only / year-month / month-day forms.
      let body = case s {
        "T" <> r | "t" <> r -> r
        _ -> s
      }
      case parse_time_part(body) {
        Some(#(_, rest)) -> {
          let rest = case parse_offset_part(rest) {
            Some(#(NumericOffset(_, _), r)) -> r
            _ -> rest
          }
          case parse_annotations(rest, None, None, False) {
            Some(#(_, cal, "")) -> Some(option.unwrap(cal, "iso8601"))
            _ -> try_ym_md_calendar(s)
          }
        }
        None -> try_ym_md_calendar(s)
      }
    }
  }
}

fn try_ym_md_calendar(s: String) -> Option(String) {
  case
    result.is_ok(parse_year_month_string(s))
    || result.is_ok(parse_month_day_string(s))
  {
    True -> Some("iso8601")
    False -> None
  }
}

/// Calendar argument of constructors: undefined → iso8601; string → must be
/// supported; anything else → TypeError.
pub fn to_calendar_arg(v: JsVal) -> Result(tcal.Calendar, TErr) {
  case classify(v) {
    KUndef -> Ok(tcal.Iso8601)
    KStr(s) -> canonicalize_calendar(s)
    _ -> Error(TypeE("calendar must be a string"))
  }
}

/// Validate that the string's [u-ca=] calendar annotation (if any) names a
/// supported calendar. RangeError otherwise. No annotation is always valid.
pub fn check_parsed_calendar(p: ParsedIso) -> Result(Nil, TErr) {
  case p.calendar {
    None -> Ok(Nil)
    Some(c) -> {
      use _cal <- result.map(canonicalize_calendar(c))
      Nil
    }
  }
}

/// Parse a string for ToTemporalDate/DateTime — Z designator is rejected.
pub fn parse_plain_datetime_string(s: String) -> Result(ParsedIso, TErr) {
  case parse_iso_datetime_string(s) {
    None -> Error(RangeE("invalid ISO 8601 string: " <> s))
    Some(p) ->
      case p.offset {
        Zulu ->
          Error(RangeE("Z designator not supported for plain Temporal types"))
        NoOffset | NumericOffset(_, _) -> {
          use Nil <- result.map(check_parsed_calendar(p))
          p
        }
      }
  }
}

/// Canonical calendar id from a parsed ISO string's annotation.
pub fn parsed_calendar_id(p: ParsedIso) -> Result(tcal.Calendar, TErr) {
  case p.calendar {
    None -> Ok(tcal.Iso8601)
    Some(c) -> canonicalize_calendar(c)
  }
}

pub fn month_code_str(m: Int) -> String {
  "M" <> pad2(m)
}

// ============================================================================
// Property-bag field readers
// ============================================================================

/// Read the "monthCode" field: must be a String primitive "M01".."M13"
/// optionally with an "L" suffix (leap month).
pub fn read_month_code(
  st: Agent,
  h: Handle,
) -> #(Option(tcal.MonthCode), Agent) {
  let #(v, st) = get_named(st, h, "monthCode")
  case classify(v) {
    KUndef -> #(None, st)
    _ -> {
      // ToPrimitive(string) then require a String type.
      let #(prim, st) = rt_val.t_to_primitive(st, v, HintString)
      case classify(prim) {
        KStr(s) ->
          // Only the ToMonthCode GRAMMAR is checked at read time; whether
          // the code suits the calendar is validated in ResolveFields,
          // after the required-field TypeError checks.
          case parse_month_code_grammar(s) {
            Ok(mc) -> #(Some(mc), st)
            Error(Nil) ->
              rt_val.t_throw_range_error(st, "invalid monthCode: " <> s)
          }
        _ -> rt_val.t_throw_type_error(st, "monthCode must be a string")
      }
    }
  }
}

/// ToMonthCode grammar: "M" + two digits + optional "L"; "M00" needs "L".
/// Whether the code suits the calendar is `tcal.month_for_code`'s answer;
/// this is the only String -> `tcal.MonthCode` site.
fn parse_month_code_grammar(s: String) -> Result(tcal.MonthCode, Nil) {
  case s {
    "M" <> rest -> {
      let #(ds, leap) = case string.ends_with(rest, "L") {
        True -> #(string.drop_end(rest, 1), True)
        False -> #(rest, False)
      }
      case two_decimal_digits(ds) {
        // "M00" is only meaningful as the leap month "M00L".
        Ok(n) if n >= 1 || leap -> Ok(tcal.MonthCode(number: n, leap:))
        Ok(_) | Error(Nil) -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Exactly two ASCII decimal digits, so signed forms ("M-1L", "M+1") are
/// rejected before `int.parse` — which would happily accept them.
fn two_decimal_digits(s: String) -> Result(Int, Nil) {
  case string.to_graphemes(s) {
    [a, b] ->
      case is_ascii_digit(a) && is_ascii_digit(b) {
        True -> int.parse(s)
        False -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn is_ascii_digit(g: String) -> Bool {
  case g {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

/// Read the "era" field: must be a String when present.
pub fn read_bag_era(st: Agent, h: Handle) -> #(Option(String), Agent) {
  let #(v, st) = get_named(st, h, "era")
  case classify(v) {
    KUndef -> #(None, st)
    _ -> {
      let #(prim, st) = rt_val.t_to_primitive(st, v, HintString)
      case classify(prim) {
        KStr(s) -> #(Some(s), st)
        _ -> rt_val.t_throw_type_error(st, "era must be a string")
      }
    }
  }
}

/// era then eraYear, read only for calendars with eras.
pub fn read_era_fields(
  st: Agent,
  h: Handle,
  cal: tcal.Calendar,
) -> #(Option(String), Option(Int), Agent) {
  case tcal.has_eras(cal) {
    False -> #(None, None, st)
    True -> {
      let #(era, st) = read_bag_era(st, h)
      let #(era_year, st) = read_int_field(st, h, "eraYear")
      #(era, era_year, st)
    }
  }
}

/// Read a property bag's "calendar" field; returns the calendar (iso8601
/// when absent).
pub fn read_bag_calendar(st: Agent, h: Handle) -> #(tcal.Calendar, Agent) {
  let #(v, st) = get_named(st, h, "calendar")
  case classify(v) {
    KUndef -> #(tcal.Iso8601, st)
    KStr(s) -> #(terr(st, calendar_from_string(s)), st)
    KHandle(_) ->
      // A Temporal object with a calendar slot acts as its calendar.
      case temporal_data_of(st, v) |> option.then(calendar_slot_of) {
        Some(calendar) -> #(calendar, st)
        None -> rt_val.t_throw_type_error(st, "invalid calendar")
      }
    _ -> rt_val.t_throw_type_error(st, "invalid calendar")
  }
}

/// ToTemporalCalendarIdentifier(calendarLike) — string ids, ISO strings with
/// annotations, and Temporal objects carrying a calendar slot.
pub fn to_temporal_calendar_identifier(
  st: Agent,
  v: JsVal,
) -> #(tcal.Calendar, Agent) {
  case classify(v) {
    KStr(s) -> #(terr(st, calendar_from_string(s)), st)
    KHandle(_) ->
      case temporal_data_of(st, v) |> option.then(calendar_slot_of) {
        Some(calendar) -> #(calendar, st)
        None -> rt_val.t_throw_type_error(st, "not a valid calendar")
      }
    _ -> rt_val.t_throw_type_error(st, "not a valid calendar")
  }
}

// ============================================================================
// Calendar-aware field resolution (CalendarResolveFields + CalendarDateToISO)
// ============================================================================

/// Calendar date fields read from a property bag (all optional here;
/// requiredness is checked in resolve_calendar_date).
pub type DateFields {
  DateFields(
    day: Option(Int),
    era: Option(String),
    era_year: Option(Int),
    month: Option(Int),
    month_code: Option(tcal.MonthCode),
    year: Option(Int),
  )
}

pub const no_date_fields = DateFields(None, None, None, None, None, None)

/// Read date fields from a bag in spec (alphabetical) order:
/// day, era, eraYear, month, monthCode, year. era/eraYear are only read for
/// calendars with eras.
pub fn read_date_fields(
  st: Agent,
  h: Handle,
  cal: tcal.Calendar,
) -> #(DateFields, Agent) {
  let #(day, st) = read_pos_int_field(st, h, "day")
  let #(era, era_year, st) = read_era_fields(st, h, cal)
  let #(month, st) = read_pos_int_field(st, h, "month")
  let #(month_code, st) = read_month_code(st, h)
  let #(year, st) = read_int_field(st, h, "year")
  #(DateFields(day:, era:, era_year:, month:, month_code:, year:), st)
}

/// The year-month subset (no `day`): era, eraYear, month, monthCode, year.
pub fn read_year_month_fields(
  st: Agent,
  h: Handle,
  cal: tcal.Calendar,
) -> #(DateFields, Agent) {
  let #(era, era_year, st) = read_era_fields(st, h, cal)
  let #(month, st) = read_pos_int_field(st, h, "month")
  let #(month_code, st) = read_month_code(st, h)
  let #(year, st) = read_int_field(st, h, "year")
  #(DateFields(day: None, era:, era_year:, month:, month_code:, year:), st)
}

/// `with()` argument: must be an object with no calendar/timeZone properties
/// and not a Temporal instance.
pub fn require_partial_bag(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case classify(v) {
    KHandle(h) ->
      case temporal_data_of(st, v) {
        Some(_) ->
          rt_val.t_throw_type_error(
            st,
            "with() argument must be a plain object, not a Temporal instance",
          )
        None -> {
          let #(cal, st) = get_named(st, h, "calendar")
          case classify(cal) {
            KUndef -> {
              let #(tz, st) = get_named(st, h, "timeZone")
              case classify(tz) {
                KUndef -> #(h, st)
                _ ->
                  rt_val.t_throw_type_error(
                    st,
                    "with() argument must not have a timeZone property",
                  )
              }
            }
            _ ->
              rt_val.t_throw_type_error(
                st,
                "with() argument must not have a calendar property",
              )
          }
        }
      }
    _ -> rt_val.t_throw_type_error(st, "with() argument must be an object")
  }
}

/// `with()` throws when the bag carries none of the recognized fields.
pub fn require_nonempty_fields(st: Agent, is_empty: Bool) -> Nil {
  case is_empty {
    True -> rt_val.t_throw_type_error(st, "with() requires at least one field")
    False -> Nil
  }
}

/// Resolve the arithmetic year from year/era/eraYear fields. The fields must
/// contain a year (checked by the caller for TypeError ordering).
pub fn resolve_calendar_year(
  cal: tcal.Calendar,
  f: DateFields,
) -> Result(Int, TErr) {
  // era and eraYear must come as a pair.
  use Nil <- result.try(case f.era, f.era_year {
    Some(_), None | None, Some(_) ->
      Error(TypeE("era and eraYear must both be provided"))
    _, _ -> Ok(Nil)
  })
  case f.year, f.era, f.era_year {
    _, Some(era), Some(ey) ->
      // The era code is free-form user input; `parse_era_code` closes it, and
      // `year_for_era` says whether this calendar uses it. Both failures are
      // the same RangeError, raised here rather than at read time so the
      // era/eraYear TypeError above still wins.
      case
        tcal.parse_era_code(era)
        |> result.try(tcal.year_for_era(cal, _, ey))
      {
        Error(Nil) ->
          Error(RangeE(
            era <> " is not a valid era for calendar " <> tcal.identifier(cal),
          ))
        Ok(y2) ->
          case f.year {
            Some(y) if y != y2 ->
              Error(RangeE("era/eraYear inconsistent with year"))
            _ -> Ok(y2)
          }
      }
    Some(y), _, _ -> Ok(y)
    None, _, _ -> Error(TypeE("year is required"))
  }
}

/// Resolve the ordinal month within `year` from month/monthCode fields.
pub fn resolve_calendar_month(
  cal: tcal.Calendar,
  year: Int,
  f: DateFields,
  overflow: Overflow,
) -> Result(Int, TErr) {
  case f.month_code {
    Some(mc) -> {
      use ordinal <- result.try(case tcal.month_for_code(cal, year, mc) {
        Ok(o) -> Ok(o)
        Error(tcal.NeverValid) ->
          Error(RangeE(
            "monthCode is not valid for calendar " <> tcal.identifier(cal),
          ))
        Error(tcal.NotInThisYear(skip_to)) ->
          case overflow {
            Reject -> Error(RangeE("monthCode not present in year"))
            Constrain -> Ok(skip_to)
          }
      })
      case f.month {
        Some(m) if m != ordinal ->
          Error(RangeE("month and monthCode must agree"))
        _ -> Ok(ordinal)
      }
    }
    None ->
      case f.month {
        None -> Error(TypeE("month or monthCode is required"))
        Some(m) -> {
          let max = tcal.months_in_year(cal, year)
          case m > max {
            True ->
              case overflow {
                Reject -> Error(RangeE("month out of range"))
                Constrain -> Ok(max)
              }
            False -> Ok(m)
          }
        }
      }
  }
}

/// Full date resolution: fields -> ISO date (CalendarDateToISO).
pub fn resolve_calendar_date(
  cal: tcal.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  // Required-field (TypeError) checks come before all RangeError checks.
  use Nil <- result.try(case f.year, f.era, f.era_year {
    None, None, None -> Error(TypeE("year is required"))
    _, _, _ -> Ok(Nil)
  })
  use Nil <- result.try(case f.month, f.month_code {
    None, None -> Error(TypeE("month or monthCode is required"))
    _, _ -> Ok(Nil)
  })
  use day <- result.try(case f.day {
    None -> Error(TypeE("day is required"))
    Some(d) -> Ok(d)
  })
  use y <- result.try(resolve_calendar_year(cal, f))
  case cal {
    tcal.Iso8601 -> {
      use m <- result.try(resolve_iso_month(f))
      regulate_iso_date(y, m, day, overflow)
    }
    _ -> {
      use m <- result.try(resolve_calendar_month(cal, y, f, overflow))
      use d <- result.try(regulate_calendar_day(cal, y, m, day, overflow))
      Ok(iso_date_from_epoch_days(tcal.date_to_epoch_days(cal, y, m, d)))
    }
  }
}

/// month/monthCode resolution for iso8601 (codes are plain ordinals).
pub fn resolve_iso_month(f: DateFields) -> Result(Int, TErr) {
  case f.month_code {
    Some(tcal.MonthCode(number: num, leap:)) ->
      case leap || num > 12 {
        True -> Error(RangeE("monthCode is not valid for calendar iso8601"))
        False ->
          case f.month {
            Some(m) if m != num ->
              Error(RangeE("month and monthCode must agree"))
            _ -> Ok(num)
          }
      }
    None ->
      case f.month {
        None -> Error(TypeE("month or monthCode is required"))
        Some(m) -> Ok(m)
      }
  }
}

pub fn regulate_calendar_day(
  cal: tcal.Calendar,
  year: Int,
  month: Int,
  day: Int,
  overflow: Overflow,
) -> Result(Int, TErr) {
  let max = tcal.days_in_month(cal, year, month)
  case day >= 1 && day <= max {
    True -> Ok(day)
    False ->
      case overflow {
        Reject -> Error(RangeE("day out of range"))
        Constrain -> Ok(int.clamp(day, 1, max))
      }
  }
}

/// Merge `with()` fields onto an existing calendar date and resolve.
/// Existing date contributes year, monthCode (not ordinal month), day —
/// matching CalendarMergeFields/ISODateToFields.
pub fn calendar_with_fields(
  cal: tcal.Calendar,
  d: IsoDate,
  f: DateFields,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
  let f = merge_year_month_code(cal, cd, f)
  let f = case f.day {
    Some(_) -> f
    None -> DateFields(..f, day: Some(cd.day))
  }
  resolve_calendar_date(cal, f, overflow)
}

/// CalendarMergeFields for the year and month keys: an absent year (no
/// year/era/eraYear) is taken from `cd`; an absent month (no
/// month/monthCode) is taken from `cd` as its month CODE, not its ordinal.
pub fn merge_year_month_code(
  cal: tcal.Calendar,
  cd: tcal.CalDate,
  f: DateFields,
) -> DateFields {
  let has_year = f.year != None || f.era != None || f.era_year != None
  let f = case has_year {
    True -> f
    False -> DateFields(..f, year: Some(cd.year))
  }
  case f.month != None || f.month_code != None {
    True -> f
    False ->
      DateFields(
        ..f,
        month_code: Some(tcal.month_code_of(cal, cd.year, cd.month)),
      )
  }
}

// ============================================================================
// Calendar date arithmetic (CalendarDateAdd / CalendarDateUntil)
// ============================================================================

/// Balance year-month after adding months (1-based months).
pub fn balance_year_month(y: Int, m: Int) -> #(Int, Int) {
  let total = y * 12 + m - 1
  #(floor_div(total, 12), math_mod(total, 12) + 1)
}

/// ISODateAdd: add a duration to an ISO date (calendar part y/m regulated
/// by overflow, then weeks/days/time-as-days exact).
pub fn add_duration_to_date(
  d: IsoDate,
  dur: DurRec,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  let #(y2, m2) = balance_year_month(d.year + dur.years, d.month + dur.months)
  use intermediate <- result.try(regulate_iso_date(y2, m2, d.day, overflow))
  let extra_days =
    dur.weeks
    * 7
    + dur.days
    + trunc_div(time_only_ns(dur), temporal_iso.ns_per_day)
  let final_days = epoch_days(intermediate) + extra_days
  let final = iso_date_from_epoch_days(final_days)
  check_date_limits(final)
}

/// CalendarDateAdd: add a duration's years/months/weeks/days to an ISO date
/// interpreted in `cal`.
pub fn calendar_date_add(
  cal: tcal.Calendar,
  d: IsoDate,
  dur: DurRec,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  case cal {
    tcal.Iso8601 -> add_duration_to_date(d, dur, overflow)
    _ -> {
      let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
      // Add years keeping the month code (leap months constrain forward).
      let y1 = cd.year + dur.years
      use m1 <- result.try(case dur.years == 0 {
        True -> Ok(cd.month)
        False -> {
          let mc = tcal.month_code_of(cal, cd.year, cd.month)
          case tcal.carry_month_code(cal, y1, mc) {
            Ok(o) -> Ok(o)
            Error(skip_to) ->
              case overflow {
                Reject -> Error(RangeE("month not present in year"))
                Constrain -> Ok(skip_to)
              }
          }
        }
      })
      // Add months ordinally, balancing across variable-length years.
      let #(y2, m2) = balance_calendar_month(cal, y1, m1 + dur.months)
      // Regulate the day, convert back to ISO, then add weeks/days.
      use d2 <- result.try(regulate_calendar_day(cal, y2, m2, cd.day, overflow))
      let days = tcal.date_to_epoch_days(cal, y2, m2, d2)
      let extra =
        dur.weeks
        * 7
        + dur.days
        + trunc_div(time_only_ns(dur), temporal_iso.ns_per_day)
      let final = iso_date_from_epoch_days(days + extra)
      check_date_limits(final)
    }
  }
}

pub fn balance_calendar_month(
  cal: tcal.Calendar,
  year: Int,
  month: Int,
) -> #(Int, Int) {
  case month < 1 {
    True ->
      balance_calendar_month(
        cal,
        year - 1,
        month + tcal.months_in_year(cal, year - 1),
      )
    False -> {
      let max = tcal.months_in_year(cal, year)
      case month > max {
        True -> balance_calendar_month(cal, year + 1, month - max)
        False -> #(year, month)
      }
    }
  }
}

/// CalendarDateUntil for years/months in calendar space. Returns
/// #(years, months, day_remainder) — weeks/days handled by the caller from
/// the day remainder. `whole_years` selects whether whole years are counted
/// (largestUnit "year") or folded into the month count.
pub fn calendar_date_until(
  cal: tcal.Calendar,
  from: IsoDate,
  to: IsoDate,
  whole_years whole_years: Bool,
) -> #(Int, Int, Int) {
  let from_days = epoch_days(from)
  let to_days = epoch_days(to)
  let sign = case to_days >= from_days {
    True -> 1
    False -> -1
  }
  let cd1 = tcal.date_from_epoch_days(cal, from_days)
  let cd2 = tcal.date_from_epoch_days(cal, to_days)
  // Count whole years (only when largestUnit is years).
  let years = case whole_years {
    True -> count_calendar_years(cal, cd1, cd2, cd2.year - cd1.year, sign)
    False -> 0
  }
  let after_years = add_calendar_years_constrain(cal, cd1, years)
  // Count whole months. Comparisons use the receiver's original (possibly
  // unconstrained) day — a month only counts once the same day-of-month is
  // reached, per DifferenceISODate / CalendarDateUntil.
  let months = count_calendar_months(cal, after_years, cd1.day, cd2, sign, 0)
  let #(ym, mm) =
    balance_calendar_month(cal, after_years.year, after_years.month + months)
  let dmax = tcal.days_in_month(cal, ym, mm)
  // Constrain the receiver's ORIGINAL day into the final month — the day
  // constrain happens once from the original fields, not cascaded through
  // the intermediate year step (30th Esfand minus 60 years lands on the
  // 30th of the target month, even when the stepped year's Esfand has 29).
  let dd = int.min(cd1.day, dmax)
  let intermediate = tcal.date_to_epoch_days(cal, ym, mm, dd)
  #(years, months, to_days - intermediate)
}

/// Lexicographic comparison of two (year-ish, month-ish, day) triples.
pub fn compare_triple(a: #(Int, Int, Int), b: #(Int, Int, Int)) -> Int {
  let #(a1, a2, a3) = a
  let #(b1, b2, b3) = b
  case a1 == b1, a2 == b2 {
    False, _ -> int_sign(a1 - b1)
    True, False -> int_sign(a2 - b2)
    True, True -> int_sign(a3 - b3)
  }
}

/// Sort position of a month within a year that is comparable across years
/// of the same calendar: a leap month sorts between its base month and the
/// next one (M05 < M05L < M06).
fn month_code_pos(cal: tcal.Calendar, year: Int, month: Int) -> Int {
  let tcal.MonthCode(number: num, leap:) = tcal.month_code_of(cal, year, month)
  case leap {
    True -> num * 2 + 1
    False -> num * 2
  }
}

/// Add years to a calendar date keeping month code (constrain semantics).
fn add_calendar_years_constrain(
  cal: tcal.Calendar,
  cd: tcal.CalDate,
  years: Int,
) -> tcal.CalDate {
  let y = cd.year + years
  let mc = tcal.month_code_of(cal, cd.year, cd.month)
  let m = case tcal.carry_month_code(cal, y, mc) {
    Ok(o) -> o
    Error(skip_to) -> skip_to
  }
  let d = int.min(cd.day, tcal.days_in_month(cal, y, m))
  tcal.CalDate(y, m, d)
}

/// Count whole years from cd1 toward cd2. A year only counts when the
/// stepped (year, monthCode position, day) triple does not surpass the
/// target — the day is compared WITHOUT constraining into the stepped year
/// (e.g. Iyyar 30 of an islamic leap year to Iyyar 29 a year later is
/// 11 months and days, not one year), while a leap month code missing from
/// the stepped year constrains in the direction of travel.
fn count_calendar_years(
  cal: tcal.Calendar,
  cd1: tcal.CalDate,
  cd2: tcal.CalDate,
  candidate: Int,
  sign: Int,
) -> Int {
  case candidate * sign < 0 {
    True -> 0
    False -> {
      let pos1 = stepped_month_pos(cal, cd1, cd1.year + candidate, sign)
      let pos2 = month_code_pos(cal, cd2.year, cd2.month)
      let cmp =
        compare_triple(#(cd1.year + candidate, pos1, cd1.day), #(
          cd2.year,
          pos2,
          cd2.day,
        ))
      case cmp * sign > 0 {
        True -> count_calendar_years(cal, cd1, cd2, candidate - sign, sign)
        False -> candidate
      }
    }
  }
}

/// Month position of cd1's month code carried into `target_year`. When a
/// leap month (e.g. hebrew M05L) does not exist in the stepped year, it
/// constrains in the direction of travel: forward to the following month
/// (skip-forward), backward to the preceding base month.
fn stepped_month_pos(
  cal: tcal.Calendar,
  cd1: tcal.CalDate,
  target_year: Int,
  sign: Int,
) -> Int {
  let mc = tcal.month_code_of(cal, cd1.year, cd1.month)
  let tcal.MonthCode(number: num, leap:) = mc
  case leap {
    False -> num * 2
    True ->
      case tcal.carry_month_code(cal, target_year, mc) {
        Ok(_) -> num * 2 + 1
        Error(_) ->
          case sign > 0 {
            True -> num * 2 + 2
            False -> num * 2
          }
      }
  }
}

/// Count whole months from cd toward cd2; `day_cmp` is the original
/// (unconstrained) day-of-month used for the surpass comparison.
/// Carries the current (year, month) position through the walk so each
/// step is O(1).
fn count_calendar_months(
  cal: tcal.Calendar,
  cd: tcal.CalDate,
  day_cmp: Int,
  cd2: tcal.CalDate,
  sign: Int,
  acc: Int,
) -> Int {
  let #(y, m) = balance_calendar_month(cal, cd.year, cd.month + acc)
  count_calendar_months_loop(cal, y, m, day_cmp, cd2, sign, acc)
}

fn count_calendar_months_loop(
  cal: tcal.Calendar,
  y: Int,
  m: Int,
  day_cmp: Int,
  cd2: tcal.CalDate,
  sign: Int,
  acc: Int,
) -> Int {
  let #(ny, nm) = step_calendar_month(cal, y, m, sign)
  let cmp = compare_triple(#(ny, nm, day_cmp), #(cd2.year, cd2.month, cd2.day))
  case cmp * sign <= 0 {
    True ->
      count_calendar_months_loop(cal, ny, nm, day_cmp, cd2, sign, acc + sign)
    False -> acc
  }
}

/// Step a valid (year, month) pair by exactly one month in `sign` direction,
/// wrapping across variable-length years.
fn step_calendar_month(
  cal: tcal.Calendar,
  y: Int,
  m: Int,
  sign: Int,
) -> #(Int, Int) {
  case sign > 0 {
    True ->
      case m >= tcal.months_in_year(cal, y) {
        True -> #(y + 1, 1)
        False -> #(y, m + 1)
      }
    False ->
      case m <= 1 {
        True -> #(y - 1, tcal.months_in_year(cal, y - 1))
        False -> #(y, m - 1)
      }
  }
}

pub fn compare_iso_date(a: IsoDate, b: IsoDate) -> Int {
  int_sign(epoch_days(a) - epoch_days(b))
}

/// Round a value that lies at `num`/`den` progress between the bounding
/// unsigned candidates `abs_r1` and `abs_r2` (multiples of `inc`), per `mode`
/// applied to a value of the given `sign`.
pub fn round_between(
  abs_r1: Int,
  abs_r2: Int,
  num: Int,
  den: Int,
  inc: Int,
  mode: RoundingMode,
  sign: Int,
) -> Int {
  case num == 0, num == den {
    True, _ -> abs_r1
    _, True -> abs_r2
    _, _ -> {
      // ApplyUnsignedRoundingMode for a value strictly between r1 and r2:
      // `cmp` is the sign of (2*|numerator| - |denominator|).
      let cmp = int_sign(2 * int.absolute_value(num) - int.absolute_value(den))
      let r1_even = math_mod(abs_r1 / inc, 2) == 0
      let umode = unsigned_rounding_mode(mode, sign < 0)
      case umode {
        RZero -> abs_r1
        RInfinity -> abs_r2
        RHalfZero | RHalfInfinity | RHalfEven ->
          case cmp {
            -1 -> abs_r1
            1 -> abs_r2
            _ ->
              case umode {
                RHalfInfinity -> abs_r2
                RHalfEven ->
                  case r1_even {
                    True -> abs_r1
                    False -> abs_r2
                  }
                // A half-zero tie truncates.
                RZero | RInfinity | RHalfZero -> abs_r1
              }
          }
      }
    }
  }
}

// ============================================================================
// Year-month / month-day ISO strings (also probed by calendar_from_string)
// ============================================================================

/// ParseTemporalYearMonthString → #(iso_year, iso_month, reference_day,
/// calendar).
pub fn parse_year_month_string(
  s: String,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  // YYYY-MM or YYYYMM (+ annotations), or any full date-time string.
  let ym = case parse_year_part(s) {
    Some(#(y, rest)) -> {
      let mm = case rest {
        "-" <> r -> take_digits(r, 2)
        _ -> take_digits(rest, 2)
      }
      case mm {
        Some(#(m, rest2)) ->
          case parse_annotations(rest2, None, None, False) {
            Some(#(_, cal, "")) -> Some(#(y, m, cal))
            _ -> None
          }
        None -> None
      }
    }
    None -> None
  }
  case ym {
    Some(#(y, m, cal)) ->
      case m >= 1 && m <= 12 {
        False -> Error(RangeE("invalid year-month string"))
        True ->
          case cal {
            None -> check_ym_limits(y, m, 1, tcal.Iso8601)
            Some(c) -> {
              use canon <- result.try(canonicalize_calendar(c))
              // Year-month-only strings are only valid for iso8601.
              case canon {
                tcal.Iso8601 -> check_ym_limits(y, m, 1, tcal.Iso8601)
                _ ->
                  Error(RangeE(
                    "year-month string requires a day for non-ISO calendars",
                  ))
              }
            }
          }
      }
    None -> {
      use p <- result.try(parse_plain_datetime_string(s))
      let d = p.date
      use cal_id <- result.try(parsed_calendar_id(p))
      case cal_id {
        tcal.Iso8601 -> check_ym_limits(d.year, d.month, d.day, tcal.Iso8601)
        cal -> {
          // Reference day: first day of the calendar month
          // containing the parsed date.
          let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
          let first =
            iso_date_from_epoch_days(tcal.date_to_epoch_days(
              cal,
              cd.year,
              cd.month,
              1,
            ))
          check_ym_limits(first.year, first.month, first.day, cal)
        }
      }
    }
  }
}

pub fn check_ym_limits(
  y: Int,
  m: Int,
  rd: Int,
  cal: tcal.Calendar,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  case iso_year_month_within_limits(y, m) {
    True -> Ok(#(y, m, rd, cal))
    False -> Error(RangeE("year-month outside of supported range"))
  }
}

/// ParseTemporalMonthDayString → #(iso_month, iso_day, reference_year,
/// calendar).
pub fn parse_month_day_string(
  s: String,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  // --MM-DD / --MMDD / MM-DD / MMDD (+ annotations), or full date-time.
  let body = case s {
    "--" <> r -> r
    _ -> s
  }
  let md = case take_digits(body, 2) {
    Some(#(m, rest)) -> {
      let dd = case rest {
        "-" <> r -> take_digits(r, 2)
        _ -> take_digits(rest, 2)
      }
      case dd {
        Some(#(d, rest2)) ->
          case parse_annotations(rest2, None, None, False) {
            Some(#(_, cal, "")) -> Some(#(m, d, cal))
            _ -> None
          }
        None -> None
      }
    }
    None -> None
  }
  case md {
    Some(#(m, d, cal)) ->
      // Use a leap reference year so Feb 29 is valid.
      case is_valid_iso_date(1972, m, d) {
        False -> try_month_day_as_datetime(s)
        True ->
          case cal {
            None -> Ok(#(m, d, 1972, tcal.Iso8601))
            Some(c) -> {
              use canon <- result.try(canonicalize_calendar(c))
              // Month-day-only strings are only valid for iso8601.
              case canon {
                tcal.Iso8601 -> Ok(#(m, d, 1972, tcal.Iso8601))
                _ ->
                  Error(RangeE(
                    "month-day string requires a year for non-ISO calendars",
                  ))
              }
            }
          }
      }
    None -> try_month_day_as_datetime(s)
  }
}

fn try_month_day_as_datetime(
  s: String,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  use p <- result.try(parse_plain_datetime_string(s))
  let d = p.date
  use cal_id <- result.try(parsed_calendar_id(p))
  case cal_id {
    tcal.Iso8601 -> Ok(#(d.month, d.day, 1972, tcal.Iso8601))
    cal -> {
      // ISODateWithinLimits before converting to calendar space:
      // e.g. -999999-01-01[u-ca=gregory] must throw RangeError.
      use Nil <- result.try(case iso_date_within_limits(d) {
        False -> Error(RangeE("date outside of supported range"))
        True -> Ok(Nil)
      })
      let cd = tcal.date_from_epoch_days(cal, epoch_days(d))
      let mc = tcal.month_code_of(cal, cd.year, cd.month)
      use iso <- result.try(month_day_reference_iso(cal, mc, cd.day, Constrain))
      Ok(#(iso.month, iso.day, iso.year, cal))
    }
  }
}

/// ISO epoch days of 1972-12-31 — the month-day reference-year boundary.
pub const md_reference_boundary = 1095

/// Find the ISO date of the latest calendar month-day on or before
/// 1972-12-31 with the given month code and day.
pub fn month_day_reference_iso(
  cal: tcal.Calendar,
  mc: tcal.MonthCode,
  day: Int,
  overflow: Overflow,
) -> Result(IsoDate, TErr) {
  let boundary_cd = tcal.date_from_epoch_days(cal, md_reference_boundary)
  case md_search(cal, mc, day, boundary_cd.year, 300) {
    Ok(iso) -> Ok(iso)
    Error(Nil) ->
      // No year in the window has this exact day. Constrain clamps to the
      // largest day the month ever has; reject throws.
      case overflow {
        Reject -> Error(RangeE("day out of range for month"))
        Constrain -> {
          let dmax = md_max_day(cal, mc, boundary_cd.year, 300, 0)
          case dmax > 0 {
            True ->
              md_search(cal, mc, dmax, boundary_cd.year, 300)
              |> result.replace_error(RangeE("invalid month-day"))
            False -> Error(RangeE("invalid month-day"))
          }
        }
      }
  }
}

fn md_search(
  cal: tcal.Calendar,
  mc: tcal.MonthCode,
  day: Int,
  year: Int,
  tries: Int,
) -> Result(IsoDate, Nil) {
  case tries <= 0 {
    True -> Error(Nil)
    False ->
      case tcal.month_for_code(cal, year, mc) {
        // NeverValid depends only on the code, not the year: no earlier year
        // can produce this month either, so stop rather than spin.
        Error(tcal.NeverValid) -> Error(Nil)
        Error(tcal.NotInThisYear(_)) ->
          md_search(cal, mc, day, year - 1, tries - 1)
        Ok(m) ->
          case day <= tcal.days_in_month(cal, year, m) {
            False -> md_search(cal, mc, day, year - 1, tries - 1)
            True -> {
              let days = tcal.date_to_epoch_days(cal, year, m, day)
              case days <= md_reference_boundary {
                True -> Ok(iso_date_from_epoch_days(days))
                False -> md_search(cal, mc, day, year - 1, tries - 1)
              }
            }
          }
      }
  }
}

/// Largest day the month with this code reaches in the search window.
fn md_max_day(
  cal: tcal.Calendar,
  mc: tcal.MonthCode,
  year: Int,
  tries: Int,
  best: Int,
) -> Int {
  case tries <= 0 {
    True -> best
    False ->
      case tcal.month_for_code(cal, year, mc) {
        // As in `md_search`: NeverValid is year-independent, so `best` is
        // already final.
        Error(tcal.NeverValid) -> best
        Error(tcal.NotInThisYear(_)) ->
          md_max_day(cal, mc, year - 1, tries - 1, best)
        Ok(m) ->
          md_max_day(
            cal,
            mc,
            year - 1,
            tries - 1,
            int.max(best, tcal.days_in_month(cal, year, m)),
          )
      }
  }
}

/// A "MM-DD"-shaped string valid in the leap reference year 1972 ("0229" is
/// a possible month-day but "0230" is not, so the latter is unambiguously a
/// time).
pub fn is_month_day_like(s: String) -> Bool {
  let s = case s {
    "--" <> r -> r
    _ -> s
  }
  case take_digits(s, 2) {
    Some(#(m, rest)) -> {
      let rest = case rest {
        "-" <> r -> r
        _ -> rest
      }
      case take_digits(rest, 2) {
        Some(#(d, "")) ->
          m >= 1 && m <= 12 && d >= 1 && d <= days_in_month(1972, m)
        _ -> False
      }
    }
    None -> False
  }
}

/// A "YYYY-MM"-shaped string.
pub fn is_year_month_like(s: String) -> Bool {
  case take_digits(s, 4) {
    Some(#(_, "")) -> False
    Some(#(_, rest)) ->
      case rest {
        "-" <> r ->
          case take_digits(r, 2) {
            Some(#(m, "")) -> m >= 1 && m <= 12
            _ -> False
          }
        _ ->
          case take_digits(rest, 2) {
            Some(#(m, "")) -> m >= 1 && m <= 12
            _ -> False
          }
      }
    None -> False
  }
}

// ============================================================================
// Getter field helpers
// ============================================================================

pub fn era_field(cal: tcal.Calendar, cd: tcal.CalDate) -> JsVal {
  tcal.era_for(cal, cd.year, cd.month, cd.day)
  |> option.map(fn(e: tcal.Era) { mk_string(tcal.era_code_string(e.code)) })
  |> option.unwrap(mk_undefined())
}

pub fn era_year_field(cal: tcal.Calendar, cd: tcal.CalDate) -> JsVal {
  tcal.era_for(cal, cd.year, cd.month, cd.day)
  |> option.map(fn(e: tcal.Era) { int_val(e.year) })
  |> option.unwrap(mk_undefined())
}
