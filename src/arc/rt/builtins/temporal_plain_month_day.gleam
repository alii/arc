import arc/internal/gregorian.{days_in_month}
import arc/internal/temporal_calendar as tcal
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
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

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
    // unreachable, plainmonthday has no compare
    TsCompare ->
      rt_val.t_throw_type_error(st, "Temporal.PlainMonthDay has no compare")
  }
}

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

type MdAnchor {
  AnchorFromYear
  AnchorFromCode(tcal.MonthCode)
}

pub fn resolve_calendar_month_day(
  cal: tcal.Calendar,
  f: DateFields,
  overflow: Overflow,
) -> Result(#(Int, Int, Int, tcal.Calendar), TErr) {
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
      let d2 = int.min(date.day, days_in_month(1972, date.month))
      Ok(#(date.month, d2, 1972, cal))
    }
    _ -> {
      use anchor <- result.try(case has_year, f.month_code {
        True, _ -> Ok(AnchorFromYear)
        False, Some(mc) -> Ok(AnchorFromCode(mc))
        False, None ->
          Error(TypeE("either year or monthCode required with month"))
      })
      use #(mc, day) <- result.try(case anchor {
        AnchorFromYear -> {
          use y <- result.try(resolve_calendar_year(cal, f))
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
      // no iso reference year: reject throws, constrain uses non-leap month
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

// chinese/dangi leap month-days with no iso reference year
fn chinese_ref_year_missing(num: Int, day: Int) -> Bool {
  case num {
    1 | 12 -> True
    2 | 8 | 9 | 10 | 11 -> day == 30
    _ -> False
  }
}

fn md_probe_year(cal: tcal.Calendar, leap: Bool) -> Int {
  case cal == tcal.Hebrew && leap {
    True -> 5779
    False -> {
      let cd = tcal.date_from_epoch_days(cal, md_reference_boundary)
      cd.year
    }
  }
}

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
