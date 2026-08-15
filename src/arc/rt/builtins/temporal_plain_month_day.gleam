//// Temporal.PlainMonthDay (proposal-temporal §10): constructor, from, the
//// prototype getters and methods, and ToTemporalMonthDay.
////
//// The slot holds the ISO date of the reference day (in the ISO reference
//// year 1972, or the latest year on or before it in which the calendar
//// month-day exists) plus the calendar. PlainMonthDay has no `compare`.

import arc/internal/gregorian.{days_in_month}
import arc/rt/builtins/helpers
import arc/rt/builtins/temporal_common.{
  type CalendarNameMode, CalAlways, CalAuto, CalCritical, CalNever,
  arg_trunc_int, arg_trunc_int_or, calendar_suffix, get_calendar_name_option,
  make_date_cal, make_month_day_cal, month_day_slot_of, read_int_field,
  require_temporal, terr,
}
import arc/rt/builtins/temporal_fields.{
  type DateFields, DateFields, int_val, md_reference_boundary, month_code_str,
  month_day_reference_iso, no_date_fields, parse_month_day_string,
  read_bag_calendar, read_date_fields, read_era_fields, regulate_calendar_day,
  require_nonempty_fields, require_partial_bag, resolve_calendar_date,
  resolve_calendar_month, resolve_calendar_year, resolve_iso_month,
  to_calendar_arg, validated_overflow,
}
import arc/rt/builtins/temporal_iso.{
  type Overflow, type TErr, Constrain, IsoDate, RangeE, Reject, TypeE,
  check_date_limits, epoch_days, format_iso_date, is_valid_iso_date,
  max_epoch_days, min_epoch_days, pad2, regulate_iso_date,
}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, type NativeToken, type PlainMonthDayMethod,
  type TemporalMonthDayGetter, type TemporalProtos, type TemporalStaticName,
  KHandle, KStr, MdCalendarId, MdDay, MdMonthCode, PmdEquals, PmdToJson,
  PmdToLocaleString, PmdToPlainDate, PmdToString, PmdValueOf, PmdWith, SObject,
  TemporalN, TemporalPlainMonthDayCtor, TemporalPlainMonthDayGetter,
  TemporalPlainMonthDayMethod, TemporalPlainMonthDayStatic, TsCompare, TsFrom,
  classify, mk_bool, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import arc/vm/internal/temporal_calendar as tcal
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

// ============================================================================
// Registration tables — the only place the JS-facing names are written down
// ============================================================================

const all_getters = [MdCalendarId, MdMonthCode, MdDay]

const all_methods = [
  #(PmdWith, 1),
  #(PmdEquals, 1),
  #(PmdToString, 0),
  #(PmdToLocaleString, 0),
  #(PmdToJson, 0),
  #(PmdValueOf, 0),
  #(PmdToPlainDate, 1),
]

/// Registration specs for `temporal.init_temporal_type`: the constructor
/// token, `from` (no `compare`), the getters and the prototype methods, in
/// prototype-registration order.
pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainMonthDayCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  [#("from", TemporalN(TemporalPlainMonthDayStatic(TsFrom, protos)), 1)]
}

pub fn getters() -> List(#(String, NativeToken)) {
  list.map(all_getters, fn(g) {
    #(getter_name(g), TemporalN(TemporalPlainMonthDayGetter(g)))
  })
}

pub fn methods(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  list.map(all_methods, fn(m) {
    #(
      method_name(m.0),
      TemporalN(TemporalPlainMonthDayMethod(m.0, protos)),
      m.1,
    )
  })
}

pub fn getter_name(g: TemporalMonthDayGetter) -> String {
  case g {
    MdCalendarId -> "calendarId"
    MdMonthCode -> "monthCode"
    MdDay -> "day"
  }
}

pub fn method_name(m: PlainMonthDayMethod) -> String {
  case m {
    PmdWith -> "with"
    PmdEquals -> "equals"
    PmdToString -> "toString"
    PmdToLocaleString -> "toLocaleString"
    PmdToJson -> "toJSON"
    PmdValueOf -> "valueOf"
    PmdToPlainDate -> "toPlainDate"
  }
}

// ============================================================================
// Constructor and statics
// ============================================================================

/// new Temporal.PlainMonthDay(month, day [, calendar [, referenceISOYear]])
/// — allocated on the intrinsic prototype; the caller applies NewTarget's.
pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(m, st) = arg_trunc_int(st, args, 0)
  let #(d, st) = arg_trunc_int(st, args, 1)
  let cal = terr(st, to_calendar_arg(helpers.arg_at(args, 2)))
  let #(y, st) = arg_trunc_int_or(st, args, 3, 1972)
  case is_valid_iso_date(y, m, d) {
    False -> rt_val.t_throw_range_error(st, "invalid ISO month-day")
    True -> make_month_day_cal(st, protos, m, d, y, cal)
  }
}

/// Temporal.PlainMonthDay.from
pub fn static(
  st: Agent,
  name: TemporalStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    TsFrom -> {
      let #(#(m, d, ry, cal), st) =
        to_temporal_month_day(
          st,
          helpers.arg_at(args, 0),
          helpers.arg_at(args, 1),
        )
      make_month_day_cal(st, protos, m, d, ry, cal)
    }
    // PlainMonthDay has no `compare` per spec; init never registers this
    // pair, so this arm is structurally unreachable — kept only because
    // `TemporalStaticName` is exhaustive here.
    TsCompare ->
      rt_val.t_throw_type_error(st, "Temporal.PlainMonthDay has no compare")
  }
}

// ============================================================================
// ToTemporalMonthDay
// ============================================================================

/// ToTemporalMonthDay(item [, options]) → #(iso_month, iso_day,
/// reference_year, calendar).
pub fn to_temporal_month_day(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(#(Int, Int, Int, tcal.Calendar), Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind:, ..) ->
          case month_day_slot_of(kind) {
            Some(md) -> {
              let #(_o, st) = validated_overflow(st, options)
              #(md, st)
            }
            None -> month_day_from_bag(st, h, options)
          }
        _ -> month_day_from_bag(st, h, options)
      }
    KStr(s) -> {
      let md = terr(st, parse_month_day_string(s))
      let #(_o, st) = validated_overflow(st, options)
      #(md, st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "cannot convert to a Temporal.PlainMonthDay",
      )
  }
}

/// Property bag → month-day. Fields: calendar, then day, era, eraYear,
/// month, monthCode, year.
fn month_day_from_bag(
  st: Agent,
  h: types.Handle,
  options: JsVal,
) -> #(#(Int, Int, Int, tcal.Calendar), Agent) {
  let #(cal, st) = read_bag_calendar(st, h)
  let #(fields, st) = read_date_fields(st, h, cal)
  let #(overflow, st) = validated_overflow(st, options)
  #(terr(st, resolve_calendar_month_day(cal, fields, overflow)), st)
}

/// What pins the month-day reference-year search to a specific year: an
/// explicit calendar year in the fields, or — when there is none — the bare
/// month code, which is then the only thing that can identify the month.
type MdAnchor {
  AnchorFromYear
  AnchorFromCode(tcal.MonthCode)
}

/// Resolve month-day fields to #(iso_month, iso_day, iso_ref_year, calendar).
pub fn resolve_calendar_month_day(
  cal: tcal.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
  // Required fields (TypeError) first.
  use day <- result.try(case f.day {
    None -> Error(TypeE("day is required"))
    Some(d) -> Ok(d)
  })
  use Nil <- result.try(case f.month, f.month_code {
    None, None -> Error(TypeE("month or monthCode is required"))
    _, _ -> Ok(Nil)
  })
  let has_year = f.year != None || { f.era != None && f.era_year != None }
  case cal {
    tcal.Iso8601 -> {
      use m <- result.try(resolve_iso_month(f))
      let ref_year = case f.month_code {
        Some(_) -> 1972
        None -> option.unwrap(f.year, 1972)
      }
      use date <- result.try(regulate_iso_date(ref_year, m, day, overflow))
      // Clamp day to the leap reference year's month length.
      let d2 = int.min(date.day, days_in_month(1972, date.month))
      Ok(#(date.month, d2, 1972, cal))
    }
    _ -> {
      // What pins down the reference-year search. Absent a year, only a bare
      // month code can: producing it here is what makes it available below,
      // rather than re-deriving it from `f.month_code` and asserting.
      // (For iso8601 the month maps straight to a code, so this is non-ISO
      // only.)
      use anchor <- result.try(case has_year, f.month_code {
        True, _ -> Ok(AnchorFromYear)
        False, Some(mc) -> Ok(AnchorFromCode(mc))
        False, None ->
          Error(TypeE("either year or monthCode required with month"))
      })
      // Determine the month code (and day) to anchor the reference search.
      use #(mc, day) <- result.try(case anchor {
        AnchorFromYear -> {
          use y <- result.try(resolve_calendar_year(cal, f))
          // Bail out before any month-info computation when no date in the
          // calendar year is within the representable ISO range.
          let year_first = tcal.date_to_epoch_days(cal, y, 1, 1)
          let year_last = tcal.date_to_epoch_days(cal, y + 1, 1, 1) - 1
          use Nil <- result.try(
            case year_first > max_epoch_days || year_last < min_epoch_days {
              True -> Error(RangeE("year outside of supported range"))
              False -> Ok(Nil)
            },
          )
          use m <- result.try(resolve_calendar_month(cal, y, f, overflow))
          use d <- result.try(regulate_calendar_day(cal, y, m, day, overflow))
          Ok(#(tcal.month_code_of(cal, y, m), d))
        }
        AnchorFromCode(mc) -> {
          // Validate the code can ever occur in this calendar.
          use Nil <- result.try(
            case tcal.month_for_code(cal, md_probe_year(cal, mc.leap), mc) {
              Error(tcal.NeverValid) ->
                Error(RangeE(
                  "monthCode is not valid for calendar " <> tcal.identifier(cal),
                ))
              _ -> Ok(Nil)
            },
          )
          case f.month {
            Some(_) -> Error(TypeE("year is required when month is present"))
            None -> Ok(#(mc, day))
          }
        }
      })
      // chinese/dangi leap month-day pairs with no ISO reference year in the
      // spec's reference-year table throw under reject; constrain falls back
      // to the non-leap month (keeping the day).
      use mc <- result.try(
        case
          { cal == tcal.Chinese || cal == tcal.Dangi }
          && mc.leap
          && chinese_ref_year_missing(mc.number, day)
        {
          True ->
            case overflow {
              Reject -> Error(RangeE("no reference year for monthCode and day"))
              Constrain -> Ok(tcal.MonthCode(number: mc.number, leap: False))
            }
          False -> Ok(mc)
        },
      )
      use iso <- result.try(month_day_reference_iso(cal, mc, day, overflow))
      Ok(#(iso.month, iso.day, iso.year, cal))
    }
  }
}

/// chinese/dangi leap-month + day combinations that have no ISO reference
/// year (the "—" cells of the spec's chinese/dangi reference-year table):
/// such dates are not known to occur between ISO years 1900 and 2035.
fn chinese_ref_year_missing(num: Int, day: Int) -> Bool {
  case num {
    1 | 12 -> True
    2 | 8 | 9 | 10 | 11 -> day == 30
    _ -> False
  }
}

/// A year in which a leap/normal month code can plausibly occur, used only
/// for NeverValid validation of bare month codes.
fn md_probe_year(cal: tcal.Calendar, leap: Bool) -> Int {
  case cal == tcal.Hebrew && leap {
    True -> 5779
    False -> {
      let cd = tcal.date_from_epoch_days(cal, md_reference_boundary)
      cd.year
    }
  }
}

// ============================================================================
// Getters
// ============================================================================

pub fn getter(
  st: Agent,
  g: TemporalMonthDayGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let #(m, d, ry, cal) =
    require_temporal(
      st,
      this,
      "PlainMonthDay",
      getter_name(g),
      month_day_slot_of,
    )
  #(month_day_field_cal(cal, m, d, ry, g), st)
}

/// Calendar-aware month-day field getter. m/d/ry are the slot's ISO date.
fn month_day_field_cal(
  cal: tcal.Calendar,
  m: Int,
  d: Int,
  ry: Int,
  g: TemporalMonthDayGetter,
) -> JsVal {
  case g {
    MdCalendarId -> mk_string(tcal.identifier(cal))
    MdMonthCode ->
      case cal {
        tcal.Iso8601 -> mk_string(month_code_str(m))
        _ -> {
          let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(ry, m, d)))
          mk_string(tcal.month_code(cal, cd.year, cd.month))
        }
      }
    MdDay ->
      case cal {
        tcal.Iso8601 -> int_val(d)
        _ -> {
          let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(ry, m, d)))
          int_val(cd.day)
        }
      }
  }
}

// ============================================================================
// Methods
// ============================================================================

pub fn method(
  st: Agent,
  meth: PlainMonthDayMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(m, d, ry, cal) =
    require_temporal(
      st,
      this,
      "PlainMonthDay",
      method_name(meth),
      month_day_slot_of,
    )
  case meth {
    PmdToJson | PmdToLocaleString -> #(
      mk_string(format_md_cal(m, d, ry, cal, CalAuto)),
      st,
    )
    PmdToString -> {
      let #(#(cal_name, _), st) =
        get_calendar_name_option(st, helpers.arg_at(args, 0))
      #(mk_string(format_md_cal(m, d, ry, cal, cal_name)), st)
    }
    PmdValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainMonthDay cannot be converted with valueOf",
      )
    PmdEquals -> {
      let #(other, st) =
        to_temporal_month_day(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(#(m, d, ry, cal) == other), st)
    }
    PmdWith -> with(st, protos, m, d, ry, cal, args)
    PmdToPlainDate -> to_plain_date(st, protos, m, d, ry, cal, args)
  }
}

/// Temporal.PlainMonthDay.prototype.with ( temporalMonthDayLike [, options] )
fn with(
  st: Agent,
  protos: TemporalProtos,
  m: Int,
  d: Int,
  ry: Int,
  cal: tcal.Calendar,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
  let #(fields, st) = read_date_fields(st, bag, cal)
  require_nonempty_fields(st, fields == no_date_fields)
  let #(overflow, st) = validated_overflow(st, helpers.arg_at(args, 1))
  // Merge with the existing month-day's calendar fields.
  let cd = tcal.date_from_epoch_days(cal, epoch_days(IsoDate(ry, m, d)))
  let f = fields
  let f = case f.month != None || f.month_code != None {
    True -> f
    False ->
      DateFields(
        ..f,
        month_code: Some(tcal.month_code_of(cal, cd.year, cd.month)),
      )
  }
  let f = case f.day {
    Some(_) -> f
    None -> DateFields(..f, day: Some(cd.day))
  }
  let md = terr(st, resolve_calendar_month_day(cal, f, overflow))
  make_month_day_cal(st, protos, md.0, md.1, md.2, md.3)
}

/// Temporal.PlainMonthDay.prototype.toPlainDate ( item )
fn to_plain_date(
  st: Agent,
  protos: TemporalProtos,
  m: Int,
  d: Int,
  ry: Int,
  cal: tcal.Calendar,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(helpers.arg_at(args, 0)) {
    KHandle(h) -> {
      let #(era, era_year, st) = read_era_fields(st, h, cal)
      let #(year, st) = read_int_field(st, h, "year")
      // iso8601 has no eras, so an explicit `year` is the only thing
      // that can satisfy the "year is required" rule for it — matching
      // on the pair produces the year rather than asserting it later.
      case cal, year {
        tcal.Iso8601, Some(y) -> {
          let date = terr(st, regulate_iso_date(y, m, d, Constrain))
          let date = terr(st, check_date_limits(date))
          make_date_cal(st, protos, date, cal)
        }
        tcal.Iso8601, None -> rt_val.t_throw_type_error(st, "year is required")
        _, _ ->
          case year != None || { era != None && era_year != None } {
            True -> {
              let cd =
                tcal.date_from_epoch_days(cal, epoch_days(IsoDate(ry, m, d)))
              let mc = tcal.month_code_of(cal, cd.year, cd.month)
              let f =
                DateFields(
                  day: Some(cd.day),
                  era:,
                  era_year:,
                  month: None,
                  month_code: Some(mc),
                  year:,
                )
              let date = terr(st, resolve_calendar_date(cal, f, Constrain))
              let date = terr(st, check_date_limits(date))
              make_date_cal(st, protos, date, cal)
            }
            False -> rt_val.t_throw_type_error(st, "year is required")
          }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "argument must be an object")
  }
}

/// Format a PlainMonthDay: non-ISO calendars always include the reference
/// year and the calendar annotation.
fn format_md_cal(
  m: Int,
  d: Int,
  ry: Int,
  cal: tcal.Calendar,
  mode: CalendarNameMode,
) -> String {
  case cal {
    tcal.Iso8601 ->
      case mode {
        CalAlways | CalCritical ->
          format_iso_date(IsoDate(ry, m, d)) <> calendar_suffix(mode, cal)
        CalAuto | CalNever -> pad2(m) <> "-" <> pad2(d)
      }
    _ ->
      case mode {
        CalNever -> format_iso_date(IsoDate(ry, m, d))
        CalAuto | CalAlways | CalCritical ->
          format_iso_date(IsoDate(ry, m, d)) <> calendar_suffix(mode, cal)
      }
  }
}
