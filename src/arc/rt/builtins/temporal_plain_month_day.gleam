import arc/bytecode/error_kind.{type JsError, JsError, RangeError, TypeError}
import arc/internal/gregorian.{days_in_month}
import arc/internal/temporal_calendar
import arc/rt/builtins/helpers
import arc/rt/builtins/options.{get_options_object}
import arc/rt/builtins/temporal_common.{
  make_date_cal, make_month_day_cal, month_day_slot_of, read_int_field,
  require_temporal, truncated_int_arg, truncated_int_arg_or,
}
import arc/rt/builtins/temporal_fields.{
  type DateFields, DateFields, max_reference_epoch_days, month_code_text,
  month_day_reference_iso, no_date_fields, parse_month_day_string,
  read_bag_calendar, read_date_fields, read_era_fields, regulate_calendar_day,
  require_nonempty_fields, require_partial_bag, resolve_calendar_date,
  resolve_calendar_month, resolve_calendar_year, resolve_iso_month,
  to_calendar_arg,
}
import arc/rt/builtins/temporal_iso.{
  type IsoDateSlots, type Overflow, Constrain, IsoDate, IsoDateSlots, Reject,
  check_date_limits, epoch_days, is_valid_iso_date, max_epoch_days,
  min_epoch_days, pad2, regulate_iso_date,
}
import arc/rt/builtins/temporal_options.{
  type CalendarNameMode, CalendarNameAuto, format_with_reference,
  get_calendar_name_option, get_overflow_option_from_value,
}
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type JsVal, type NativeToken, type PlainMonthDayMethod,
  type TemporalMonthDayGetter, type TemporalProtos, type TemporalStaticName,
  CompareStatic, FromStatic, KHandle, KStr, MonthDayCalendarId, MonthDayDay,
  MonthDayMonthCode, PlainMonthDayEquals, PlainMonthDayToJson,
  PlainMonthDayToLocaleString, PlainMonthDayToPlainDate, PlainMonthDayToString,
  PlainMonthDayValueOf, PlainMonthDayWith, SObject, TemporalN,
  TemporalPlainMonthDayCtor, TemporalPlainMonthDayGetter,
  TemporalPlainMonthDayMethod, TemporalPlainMonthDayStatic, classify, mk_bool,
  mk_int, mk_string, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

const all_getters = [MonthDayCalendarId, MonthDayMonthCode, MonthDayDay]

const all_methods = [
  #(PlainMonthDayWith, 1),
  #(PlainMonthDayEquals, 1),
  #(PlainMonthDayToString, 0),
  #(PlainMonthDayToLocaleString, 0),
  #(PlainMonthDayToJson, 0),
  #(PlainMonthDayValueOf, 0),
  #(PlainMonthDayToPlainDate, 1),
]

pub fn ctor_token(protos: TemporalProtos) -> NativeToken {
  TemporalN(TemporalPlainMonthDayCtor(protos:))
}

pub fn statics(protos: TemporalProtos) -> List(#(String, NativeToken, Int)) {
  [#("from", TemporalN(TemporalPlainMonthDayStatic(FromStatic, protos)), 1)]
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
    MonthDayCalendarId -> "calendarId"
    MonthDayMonthCode -> "monthCode"
    MonthDayDay -> "day"
  }
}

pub fn method_name(m: PlainMonthDayMethod) -> String {
  case m {
    PlainMonthDayWith -> "with"
    PlainMonthDayEquals -> "equals"
    PlainMonthDayToString -> "toString"
    PlainMonthDayToLocaleString -> "toLocaleString"
    PlainMonthDayToJson -> "toJSON"
    PlainMonthDayValueOf -> "valueOf"
    PlainMonthDayToPlainDate -> "toPlainDate"
  }
}

pub fn ctor(
  st: Agent,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(m, st) = truncated_int_arg(st, args, 0)
  let #(d, st) = truncated_int_arg(st, args, 1)
  let cal = rt_val.or_throw(st, to_calendar_arg(helpers.arg_at(args, 2)))
  let #(y, st) = truncated_int_arg_or(st, args, 3, 1972)
  case is_valid_iso_date(y, m, d) {
    False -> rt_val.t_throw_range_error(st, "invalid ISO month-day")
    True -> make_month_day_cal(st, protos, m, d, y, cal)
  }
}

pub fn static(
  st: Agent,
  name: TemporalStaticName,
  protos: TemporalProtos,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case name {
    FromStatic -> {
      let #(IsoDateSlots(IsoDate(ry, m, d), cal), st) =
        to_temporal_month_day(
          st,
          helpers.arg_at(args, 0),
          helpers.arg_at(args, 1),
        )
      make_month_day_cal(st, protos, m, d, ry, cal)
    }
    // unreachable, plainmonthday has no compare
    CompareStatic ->
      rt_val.t_throw_type_error(st, "Temporal.PlainMonthDay has no compare")
  }
}

pub fn to_temporal_month_day(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(IsoDateSlots, Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind:, ..) ->
          case month_day_slot_of(kind) {
            Some(md) -> {
              let #(_o, st) = get_overflow_option_from_value(st, options)
              #(md, st)
            }
            None -> month_day_from_bag(st, h, options)
          }
        _ -> month_day_from_bag(st, h, options)
      }
    KStr(s) -> {
      let md = rt_val.or_throw(st, parse_month_day_string(s))
      let #(_o, st) = get_overflow_option_from_value(st, options)
      #(md, st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "cannot convert to a Temporal.PlainMonthDay",
      )
  }
}

fn month_day_from_bag(
  st: Agent,
  h: types.Handle,
  options: JsVal,
) -> #(IsoDateSlots, Agent) {
  let #(cal, st) = read_bag_calendar(st, h)
  let #(fields, st) = read_date_fields(st, h, cal)
  let #(overflow, st) = get_overflow_option_from_value(st, options)
  #(rt_val.or_throw(st, resolve_calendar_month_day(cal, fields, overflow)), st)
}

type MonthDayAnchor {
  AnchorFromYear
  AnchorFromCode(temporal_calendar.MonthCode)
}

pub fn resolve_calendar_month_day(
  cal: temporal_calendar.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(IsoDateSlots, JsError) {
  use day <- result.try(case f.day {
    None -> Error(JsError(TypeError, "day is required"))
    Some(d) -> Ok(d)
  })
  use Nil <- result.try(case f.month, f.month_code {
    None, None -> Error(JsError(TypeError, "month or monthCode is required"))
    _, _ -> Ok(Nil)
  })
  let has_year = f.year != None || { f.era != None && f.era_year != None }
  case cal {
    temporal_calendar.Iso8601 -> {
      use m <- result.try(resolve_iso_month(f))
      let ref_year = case f.month_code {
        Some(_) -> 1972
        None -> option.unwrap(f.year, 1972)
      }
      use date <- result.try(regulate_iso_date(ref_year, m, day, overflow))
      let d2 = int.min(date.day, days_in_month(1972, date.month))
      Ok(IsoDateSlots(IsoDate(1972, date.month, d2), cal))
    }
    _ -> {
      use anchor <- result.try(case has_year, f.month_code {
        True, _ -> Ok(AnchorFromYear)
        False, Some(mc) -> Ok(AnchorFromCode(mc))
        False, None ->
          Error(JsError(
            TypeError,
            "either year or monthCode required with month",
          ))
      })
      use #(mc, day) <- result.try(case anchor {
        AnchorFromYear -> {
          use y <- result.try(resolve_calendar_year(cal, f))
          let year_first = temporal_calendar.date_to_epoch_days(cal, y, 1, 1)
          let year_last =
            temporal_calendar.date_to_epoch_days(cal, y + 1, 1, 1) - 1
          use Nil <- result.try(
            case year_first > max_epoch_days || year_last < min_epoch_days {
              True ->
                Error(JsError(RangeError, "year outside of supported range"))
              False -> Ok(Nil)
            },
          )
          use m <- result.try(resolve_calendar_month(cal, y, f, overflow))
          use d <- result.try(regulate_calendar_day(cal, y, m, day, overflow))
          Ok(#(temporal_calendar.month_code_of(cal, y, m), d))
        }
        AnchorFromCode(mc) -> {
          use Nil <- result.try(
            case
              temporal_calendar.month_for_code(
                cal,
                probe_year_for_month_code(cal, mc.leap),
                mc,
              )
            {
              Error(temporal_calendar.NeverValid) ->
                Error(JsError(
                  RangeError,
                  "monthCode is not valid for calendar "
                    <> temporal_calendar.identifier(cal),
                ))
              _ -> Ok(Nil)
            },
          )
          case f.month {
            Some(_) ->
              Error(JsError(TypeError, "year is required when month is present"))
            None -> Ok(#(mc, day))
          }
        }
      })
      // no iso reference year: reject throws, constrain uses non-leap month
      use mc <- result.try(
        case
          { cal == temporal_calendar.Chinese || cal == temporal_calendar.Dangi }
          && mc.leap
          && chinese_ref_year_missing(mc.number, day)
        {
          True ->
            case overflow {
              Reject ->
                Error(JsError(
                  RangeError,
                  "no reference year for monthCode and day",
                ))
              Constrain ->
                Ok(temporal_calendar.MonthCode(number: mc.number, leap: False))
            }
          False -> Ok(mc)
        },
      )
      use iso <- result.try(month_day_reference_iso(cal, mc, day, overflow))
      Ok(IsoDateSlots(iso, cal))
    }
  }
}

// chinese/dangi leap month-days with no iso reference year
fn chinese_ref_year_missing(num: Int, day: Int) -> Bool {
  case num {
    1 | 12 -> True
    2 | 8 | 9 | 10 | 11 -> day == 30
    _ -> False
  }
}

fn probe_year_for_month_code(
  cal: temporal_calendar.Calendar,
  leap leap: Bool,
) -> Int {
  case cal == temporal_calendar.Hebrew && leap {
    True -> 5779
    False -> {
      let cd =
        temporal_calendar.date_from_epoch_days(cal, max_reference_epoch_days)
      cd.year
    }
  }
}

pub fn getter(
  st: Agent,
  g: TemporalMonthDayGetter,
  this: JsVal,
) -> #(JsVal, Agent) {
  let IsoDateSlots(IsoDate(ry, m, d), cal) =
    require_temporal(
      st,
      this,
      "PlainMonthDay",
      getter_name(g),
      month_day_slot_of,
    )
  #(month_day_field_cal(cal, m, d, ry, g), st)
}

fn month_day_field_cal(
  cal: temporal_calendar.Calendar,
  m: Int,
  d: Int,
  ry: Int,
  g: TemporalMonthDayGetter,
) -> JsVal {
  case g {
    MonthDayCalendarId -> mk_string(temporal_calendar.identifier(cal))
    MonthDayMonthCode ->
      case cal {
        temporal_calendar.Iso8601 -> mk_string(month_code_text(m))
        _ -> {
          let cd =
            temporal_calendar.date_from_epoch_days(
              cal,
              epoch_days(IsoDate(ry, m, d)),
            )
          mk_string(temporal_calendar.month_code(cal, cd.year, cd.month))
        }
      }
    MonthDayDay ->
      case cal {
        temporal_calendar.Iso8601 -> mk_int(d)
        _ -> {
          let cd =
            temporal_calendar.date_from_epoch_days(
              cal,
              epoch_days(IsoDate(ry, m, d)),
            )
          mk_int(cd.day)
        }
      }
  }
}

pub fn method(
  st: Agent,
  meth: PlainMonthDayMethod,
  protos: TemporalProtos,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let IsoDateSlots(IsoDate(ry, m, d), cal) =
    require_temporal(
      st,
      this,
      "PlainMonthDay",
      method_name(meth),
      month_day_slot_of,
    )
  case meth {
    PlainMonthDayToJson | PlainMonthDayToLocaleString -> #(
      mk_string(format_md_cal(m, d, ry, cal, CalendarNameAuto)),
      st,
    )
    PlainMonthDayToString -> {
      let #(opts, st) = get_options_object(st, helpers.arg_at(args, 0))
      let #(cal_name, st) = get_calendar_name_option(st, opts)
      #(mk_string(format_md_cal(m, d, ry, cal, cal_name)), st)
    }
    PlainMonthDayValueOf ->
      rt_val.t_throw_type_error(
        st,
        "Temporal.PlainMonthDay cannot be converted with valueOf",
      )
    PlainMonthDayEquals -> {
      let #(other, st) =
        to_temporal_month_day(st, helpers.arg_at(args, 0), mk_undefined())
      #(mk_bool(IsoDateSlots(IsoDate(ry, m, d), cal) == other), st)
    }
    PlainMonthDayWith -> with(st, protos, m, d, ry, cal, args)
    PlainMonthDayToPlainDate -> to_plain_date(st, protos, m, d, ry, cal, args)
  }
}

fn with(
  st: Agent,
  protos: TemporalProtos,
  m: Int,
  d: Int,
  ry: Int,
  cal: temporal_calendar.Calendar,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(bag, st) = require_partial_bag(st, helpers.arg_at(args, 0))
  let #(fields, st) = read_date_fields(st, bag, cal)
  let Nil = require_nonempty_fields(st, fields == no_date_fields)
  let #(overflow, st) =
    get_overflow_option_from_value(st, helpers.arg_at(args, 1))
  let cd =
    temporal_calendar.date_from_epoch_days(cal, epoch_days(IsoDate(ry, m, d)))
  let f = case fields.month != None || fields.month_code != None {
    True -> fields
    False ->
      DateFields(
        ..fields,
        month_code: Some(temporal_calendar.month_code_of(cal, cd.year, cd.month)),
      )
  }
  let f = case f.day {
    Some(_) -> f
    None -> DateFields(..f, day: Some(cd.day))
  }
  let md = rt_val.or_throw(st, resolve_calendar_month_day(cal, f, overflow))
  let IsoDateSlots(IsoDate(year: ry2, month: m2, day: d2), cal2) = md
  make_month_day_cal(st, protos, m2, d2, ry2, cal2)
}

fn to_plain_date(
  st: Agent,
  protos: TemporalProtos,
  m: Int,
  d: Int,
  ry: Int,
  cal: temporal_calendar.Calendar,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(helpers.arg_at(args, 0)) {
    KHandle(h) -> {
      let #(era, era_year, st) = read_era_fields(st, h, cal)
      let #(year, st) = read_int_field(st, h, "year")
      case cal, year {
        temporal_calendar.Iso8601, Some(y) -> {
          let date = rt_val.or_throw(st, regulate_iso_date(y, m, d, Constrain))
          let date = rt_val.or_throw(st, check_date_limits(date))
          make_date_cal(st, protos, date, cal)
        }
        temporal_calendar.Iso8601, None ->
          rt_val.t_throw_type_error(st, "year is required")
        _, _ ->
          case year != None || { era != None && era_year != None } {
            True -> {
              let cd =
                temporal_calendar.date_from_epoch_days(
                  cal,
                  epoch_days(IsoDate(ry, m, d)),
                )
              let mc = temporal_calendar.month_code_of(cal, cd.year, cd.month)
              let f =
                DateFields(
                  day: Some(cd.day),
                  era:,
                  era_year:,
                  month: None,
                  month_code: Some(mc),
                  year:,
                )
              let date =
                rt_val.or_throw(st, resolve_calendar_date(cal, f, Constrain))
              let date = rt_val.or_throw(st, check_date_limits(date))
              make_date_cal(st, protos, date, cal)
            }
            False -> rt_val.t_throw_type_error(st, "year is required")
          }
      }
    }
    _ -> rt_val.t_throw_type_error(st, "argument must be an object")
  }
}

fn format_md_cal(
  m: Int,
  d: Int,
  ry: Int,
  cal: temporal_calendar.Calendar,
  mode: CalendarNameMode,
) -> String {
  format_with_reference(
    IsoDate(ry, m, d),
    cal,
    mode,
    short: pad2(m) <> "-" <> pad2(d),
  )
}
