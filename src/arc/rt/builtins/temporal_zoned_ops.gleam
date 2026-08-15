//// Time-zone-aware Temporal abstract operations shared by every type that
//// converts between wall-clock and exact time: GetPossibleEpochNanoseconds,
//// DisambiguatePossibleEpochNanoseconds, GetEpochNanosecondsFor,
//// GetStartOfDay, InterpretISODateTimeOffset, ToTemporalZonedDateTime (string
//// and property-bag forms), GetTemporalRelativeToOption, AddZonedDateTime,
//// plus the date-time property-bag field record they read.
////
//// Builds on temporal_common (options, units, zone identifiers),
//// temporal_fields (calendar fields) and temporal_plain_time (time fields).

import arc/internal/int_math.{floor_div}
import arc/rt/builtins/temporal_common.{
  type Disambiguation, type OffsetOption, Compatible, Earlier, HalfExpand,
  IgnoreOffset, Later, RejectDisambiguation, RejectOffset, UseOffset,
  epoch_ns_to_iso_in, get_disambiguation_option, get_offset_option,
  get_options_object, get_overflow_option, parse_time_zone_id, read_int_field,
  read_pos_int_field, round_to_increment, terr, time_only_ns,
  to_temporal_time_zone, tz_offset_ns_at, unloadable_tz, validate_epoch_ns,
}
import arc/rt/builtins/temporal_fields.{
  type DateFields, DateFields, calendar_date_add, check_parsed_calendar,
  get_named, no_date_fields, parsed_calendar_id, read_bag_calendar, read_bag_era,
  read_month_code, resolve_calendar_date,
}
import arc/rt/builtins/temporal_iso.{
  type DurRec, type IsoDate, type Overflow, type ParsedOffset, type TErr,
  type TimeRec, Constrain, DurRec, IsoDate, NoOffset, NumericOffset, RangeE,
  Zulu, epoch_days, epoch_ns_to_iso, iso_date_from_epoch_days,
  iso_date_within_limits, midnight, ns_per_day, ns_per_minute,
  parse_iso_datetime_string, parse_offset_part, utc_epoch_ns, zero_dur,
}
import arc/rt/builtins/temporal_plain_time.{
  type TimeFields, TimeFields, no_time_fields, regulate_time, time_fields_apply,
}
import arc/rt/builtins/temporal_tz
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type TimeZone, HintString, KHandle, KStr,
  KUndef, SObject, TemporalDate, TemporalDateTime, TemporalObj,
  TemporalZonedDateTime, TzNamed, TzOffset, TzUtc, classify, mk_undefined,
}
import arc/rt/val as rt_val
import arc/vm/internal/temporal_calendar as tcal
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// ============================================================================
// Wall-clock ⇄ exact time
// ============================================================================

/// CheckISODaysRange — the wall-clock date must be within ±10^8 days.
pub fn check_iso_days_range(d: IsoDate) -> Result(Nil, TErr) {
  case int.absolute_value(epoch_days(d)) <= 100_000_000 {
    True -> Ok(Nil)
    False -> Error(RangeE("date outside of supported range"))
  }
}

/// GetPossibleEpochNanoseconds — ascending epoch instants whose local time
/// in `tz` reads as the given wall-clock date-time. Empty for times skipped
/// by an offset transition; two entries for repeated times.
/// RangeError when the (offset-shifted) date is outside the supported range.
pub fn get_possible_epoch_ns(
  tz: TimeZone,
  d: IsoDate,
  t: TimeRec,
) -> Result(List(Int), TErr) {
  let utc = utc_epoch_ns(d, t)
  case tz {
    TzUtc -> {
      use Nil <- result.try(check_iso_days_range(d))
      use ns <- result.map(validate_epoch_ns(utc))
      [ns]
    }
    TzOffset(ns: off) -> {
      let shifted_day = floor_div(utc - off, ns_per_day)
      use Nil <- result.try(
        check_iso_days_range(iso_date_from_epoch_days(shifted_day)),
      )
      use ns <- result.map(validate_epoch_ns(utc - off))
      [ns]
    }
    TzNamed(zone:) -> {
      use Nil <- result.try(check_iso_days_range(d))
      let named_offset = fn(at) {
        temporal_tz.offset_ns_at(zone, at)
        |> result.map_error(unloadable_tz(tz, _))
      }
      use before <- result.try(named_offset(utc - ns_per_day))
      use after <- result.try(named_offset(utc + ns_per_day))
      let candidates = case before == after {
        True -> [before]
        False -> [before, after]
      }
      Ok(
        list.filter_map(candidates, fn(off) {
          let ens = utc - off
          case temporal_tz.offset_ns_at(zone, ens) == Ok(off) {
            True -> Ok(ens)
            False -> Error(Nil)
          }
        }),
      )
    }
  }
}

/// DisambiguatePossibleEpochNanoseconds.
pub fn disambiguate_epoch_ns(
  possible: List(Int),
  tz: TimeZone,
  d: IsoDate,
  t: TimeRec,
  dis: Disambiguation,
) -> Result(Int, TErr) {
  case possible {
    [one] -> validate_epoch_ns(one)
    [first, ..rest] ->
      case dis {
        Compatible | Earlier -> validate_epoch_ns(first)
        Later ->
          case list.last(rest) {
            Ok(l) -> validate_epoch_ns(l)
            Error(Nil) -> validate_epoch_ns(first)
          }
        RejectDisambiguation -> Error(RangeE("ambiguous wall-clock time"))
      }
    [] ->
      case dis {
        RejectDisambiguation -> Error(RangeE("no such wall-clock time"))
        Compatible | Earlier | Later -> {
          // Skipped (gap) time: shift by the size of the gap and retry.
          let utc = utc_epoch_ns(d, t)
          use before <- result.try(tz_offset_ns_at(tz, utc - ns_per_day))
          use after <- result.try(tz_offset_ns_at(tz, utc + ns_per_day))
          let gap = after - before
          let shifted = case dis {
            Earlier -> utc - gap
            Compatible | Later | RejectDisambiguation -> utc + gap
          }
          let #(d2, t2) = epoch_ns_to_iso(shifted, 0)
          use possible2 <- result.try(get_possible_epoch_ns(tz, d2, t2))
          case dis, possible2 {
            _, [] -> Error(RangeE("no such wall-clock time"))
            Earlier, [f, ..] -> validate_epoch_ns(f)
            _, [f, ..rest2] ->
              case list.last(rest2) {
                Ok(la) -> validate_epoch_ns(la)
                Error(Nil) -> validate_epoch_ns(f)
              }
          }
        }
      }
  }
}

/// GetEpochNanosecondsFor.
pub fn get_epoch_ns_for(
  tz: TimeZone,
  d: IsoDate,
  t: TimeRec,
  dis: Disambiguation,
) -> Result(Int, TErr) {
  use possible <- result.try(get_possible_epoch_ns(tz, d, t))
  disambiguate_epoch_ns(possible, tz, d, t, dis)
}

/// GetStartOfDay.
pub fn start_of_day_ns(tz: TimeZone, d: IsoDate) -> Result(Int, TErr) {
  use possible <- result.try(get_possible_epoch_ns(tz, d, midnight))
  case possible {
    [first, ..] -> validate_epoch_ns(first)
    [] ->
      // Midnight lies in a DST gap; only named zones can reach here. The day
      // starts at the instant the gap ends: the next transition after a
      // point guaranteed to be before it (one day earlier).
      case tz {
        TzUtc | TzOffset(_) ->
          // Unreachable: fixed-offset zones never skip a wall-clock time.
          Error(RangeE("no start of day for skipped midnight"))
        TzNamed(zone:) -> {
          use day_before <- result.try(validate_epoch_ns(
            utc_epoch_ns(d, midnight) - ns_per_day,
          ))
          case temporal_tz.next_transition_ns(zone, day_before) {
            Ok(Some(transition)) -> validate_epoch_ns(transition)
            Ok(None) -> Error(RangeE("no start of day for skipped midnight"))
            Error(err) -> Error(unloadable_tz(tz, err))
          }
        }
      }
  }
}

/// Whether the source of a wall-clock date-time also supplied a UTC offset.
/// The offset lives inside `OptionOffset`, so it cannot be read — nor
/// defaulted to a meaningless 0 — when the source had none.
pub type OffsetBehaviour {
  WallOffset
  OptionOffset(offset_ns: Int)
}

/// InterpretISODateTimeOffset. `match_minutes` allows minute-truncated
/// offsets (ISO strings).
pub fn interpret_offset(
  d: IsoDate,
  t: TimeRec,
  behaviour: OffsetBehaviour,
  tz: TimeZone,
  dis: Disambiguation,
  offset_opt: OffsetOption,
  match_minutes: Bool,
) -> Result(Int, TErr) {
  case behaviour {
    WallOffset -> get_epoch_ns_for(tz, d, t, dis)
    OptionOffset(_) if offset_opt == IgnoreOffset ->
      get_epoch_ns_for(tz, d, t, dis)
    OptionOffset(offset_ns) if offset_opt == UseOffset -> {
      let ns = utc_epoch_ns(d, t) - offset_ns
      use Nil <- result.try(
        check_iso_days_range(
          iso_date_from_epoch_days(floor_div(ns, ns_per_day)),
        ),
      )
      validate_epoch_ns(ns)
    }
    OptionOffset(offset_ns) -> {
      let utc = utc_epoch_ns(d, t)
      use Nil <- result.try(check_iso_days_range(d))
      use possible <- result.try(get_possible_epoch_ns(tz, d, t))
      let matched =
        list.find(possible, fn(candidate) {
          let cand_off = utc - candidate
          let rounded = round_to_increment(cand_off, ns_per_minute, HalfExpand)
          cand_off == offset_ns || { match_minutes && rounded == offset_ns }
        })
      case matched {
        Ok(c) -> validate_epoch_ns(c)
        Error(Nil) ->
          case offset_opt == RejectOffset {
            True -> Error(RangeE("offset does not match time zone"))
            False -> disambiguate_epoch_ns(possible, tz, d, t, dis)
          }
      }
    }
  }
}

// ============================================================================
// Date-time property bags
// ============================================================================

/// Date-time fields read from a property bag (all optional). `tz` is the raw
/// `timeZone` value (undefined when absent or not requested).
pub type DateTimeFields {
  DateTimeFields(
    date: DateFields,
    time: TimeFields,
    offset: Option(Int),
    tz: JsVal,
  )
}

pub fn date_time_fields_all_none(f: DateTimeFields) -> Bool {
  f.date == no_date_fields && f.time == no_time_fields && f.offset == None
}

/// Read date-time fields from a bag in spec (alphabetical) order: day, era,
/// eraYear, hour, microsecond, millisecond, minute, month, monthCode,
/// nanosecond, [offset], second, [timeZone], year. era/eraYear are read only
/// for calendars with eras; offset and timeZone only when requested.
pub fn read_date_time_fields(
  st: Agent,
  bag: Handle,
  cal: tcal.Calendar,
  read_offset read_offset: Bool,
  read_tz read_tz: Bool,
) -> #(DateTimeFields, Agent) {
  let #(day, st) = read_pos_int_field(st, bag, "day")
  let #(era, st) = case tcal.has_eras(cal) {
    True -> read_bag_era(st, bag)
    False -> #(None, st)
  }
  let #(era_year, st) = case tcal.has_eras(cal) {
    True -> read_int_field(st, bag, "eraYear")
    False -> #(None, st)
  }
  let #(hour, st) = read_int_field(st, bag, "hour")
  let #(us, st) = read_int_field(st, bag, "microsecond")
  let #(ms, st) = read_int_field(st, bag, "millisecond")
  let #(minute, st) = read_int_field(st, bag, "minute")
  let #(month, st) = read_pos_int_field(st, bag, "month")
  let #(month_code, st) = read_month_code(st, bag)
  let #(ns, st) = read_int_field(st, bag, "nanosecond")
  let #(offset, st) = case read_offset {
    True -> read_bag_offset(st, bag)
    False -> #(None, st)
  }
  let #(second, st) = read_int_field(st, bag, "second")
  let #(tz, st) = case read_tz {
    True -> get_named(st, bag, "timeZone")
    False -> #(mk_undefined(), st)
  }
  let #(year, st) = read_int_field(st, bag, "year")
  #(
    DateTimeFields(
      date: DateFields(day:, era:, era_year:, month:, month_code:, year:),
      time: TimeFields(hour:, minute:, second:, ms:, us:, ns:),
      offset:,
      tz:,
    ),
    st,
  )
}

/// Read + validate an `offset` field from a property bag (ToOffsetString):
/// ToPrimitive with string hint, require a String, then parse.
pub fn read_bag_offset(st: Agent, bag: Handle) -> #(Option(Int), Agent) {
  let #(v, st) = get_named(st, bag, "offset")
  case classify(v) {
    KUndef -> #(None, st)
    _ -> {
      let #(prim, st) = rt_val.t_to_primitive(st, v, HintString)
      case classify(prim) {
        KStr(s) ->
          case parse_offset_part(s) {
            Some(#(NumericOffset(off, _), "")) -> #(Some(off), st)
            _ -> rt_val.t_throw_range_error(st, "invalid offset string: " <> s)
          }
        _ -> rt_val.t_throw_type_error(st, "offset must be a string")
      }
    }
  }
}

// ============================================================================
// ToTemporalZonedDateTime
// ============================================================================

/// ToTemporalZonedDateTime(item [, options]) → #(epoch_ns, tz, calendar).
pub fn to_temporal_zoned(
  st: Agent,
  item: JsVal,
  options: JsVal,
) -> #(#(Int, TimeZone, tcal.Calendar), Agent) {
  case classify(item) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(
          kind: TemporalObj(TemporalZonedDateTime(
            epoch_ns:,
            time_zone:,
            calendar:,
          )),
          ..,
        ) -> {
          let #(_o, st) = validated_zdt_options(st, options)
          #(#(epoch_ns, time_zone, calendar), st)
        }
        _ -> zoned_from_bag(st, h, options)
      }
    KStr(s) -> {
      let #(d, t_opt, offset, tz, cal) = terr(st, parse_zoned_string(s))
      let #(#(dis, offset_opt, _ov), st) = validated_zdt_options(st, options)
      let ns =
        terr(st, zoned_string_epoch_ns(d, t_opt, offset, tz, dis, offset_opt))
      #(#(ns, tz, cal), st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "cannot convert to a Temporal.ZonedDateTime",
      )
  }
}

/// ZonedDateTime options: disambiguation, offset, overflow (alphabetical).
pub fn validated_zdt_options(
  st: Agent,
  options: JsVal,
) -> #(#(Disambiguation, OffsetOption, Overflow), Agent) {
  let #(opts, st) = get_options_object(st, options)
  let #(d, st) = get_disambiguation_option(st, opts)
  let #(of, st) = get_offset_option(st, opts, RejectOffset)
  let #(ov, st) = get_overflow_option(st, opts)
  #(#(d, of, ov), st)
}

/// ParseTemporalZonedDateTimeString: date, optional time, UTC offset, the
/// required bracketed zone, and the [u-ca=] calendar (iso8601 default).
pub fn parse_zoned_string(
  s: String,
) -> Result(
  #(IsoDate, Option(TimeRec), ParsedOffset, TimeZone, tcal.Calendar),
  TErr,
) {
  case parse_iso_datetime_string(s) {
    None -> Error(RangeE("invalid ZonedDateTime string: " <> s))
    Some(p) -> {
      use Nil <- result.try(check_parsed_calendar(p))
      case p.tz {
        None -> Error(RangeE("ZonedDateTime string requires a [TimeZone]"))
        Some(tz_str) -> {
          use tz <- result.try(parse_time_zone_id(tz_str))
          use cal <- result.map(parsed_calendar_id(p))
          #(p.date, p.time, p.offset, tz, cal)
        }
      }
    }
  }
}

/// Epoch ns for a parsed ZonedDateTime string: Z → exact instant; explicit
/// offset → interpreted per the offset option (match-minutes); no time →
/// start of day; otherwise wall-clock with disambiguation.
pub fn zoned_string_epoch_ns(
  d: IsoDate,
  t_opt: Option(TimeRec),
  offset: ParsedOffset,
  tz: TimeZone,
  dis: Disambiguation,
  offset_opt: OffsetOption,
) -> Result(Int, TErr) {
  case t_opt, offset {
    None, NoOffset -> start_of_day_ns(tz, d)
    _, _ -> {
      let t = option.unwrap(t_opt, midnight)
      case offset {
        Zulu -> validate_epoch_ns(utc_epoch_ns(d, t))
        NumericOffset(off, sub_minute) ->
          // Match-minutes only when the offset lacks a seconds component.
          interpret_offset(
            d,
            t,
            OptionOffset(off),
            tz,
            dis,
            offset_opt,
            !sub_minute,
          )
        NoOffset ->
          interpret_offset(d, t, WallOffset, tz, dis, offset_opt, True)
      }
    }
  }
}

/// ZonedDateTime property bag: calendar, day, era, eraYear, hour,
/// microsecond, millisecond, minute, month, monthCode, nanosecond, offset,
/// second, timeZone, year.
pub fn zoned_from_bag(
  st: Agent,
  bag: Handle,
  options: JsVal,
) -> #(#(Int, TimeZone, tcal.Calendar), Agent) {
  let #(cal, st) = read_bag_calendar(st, bag)
  let #(f, st) =
    read_date_time_fields(st, bag, cal, read_offset: True, read_tz: True)
  // timeZone is required.
  case classify(f.tz) {
    KUndef -> rt_val.t_throw_type_error(st, "timeZone is required")
    _ -> {
      let #(tz, st) = to_temporal_time_zone(st, f.tz)
      let #(#(dis, offset_opt, ov), st) = validated_zdt_options(st, options)
      let date = terr(st, resolve_calendar_date(cal, f.date, ov))
      let t0 = time_fields_apply(f.time, midnight)
      let t = terr(st, regulate_time(t0, ov))
      let behaviour = case f.offset {
        Some(o) -> OptionOffset(o)
        None -> WallOffset
      }
      let ens =
        terr(
          st,
          interpret_offset(date, t, behaviour, tz, dis, offset_opt, False),
        )
      #(#(ens, tz, cal), st)
    }
  }
}

// ============================================================================
// relativeTo (Duration.compare / round / total)
// ============================================================================

/// GetTemporalRelativeToOption's result: nothing, a plain date, or an exact
/// zoned instant.
pub type RelTo {
  RelNone
  RelPlain(date: IsoDate, cal: tcal.Calendar)
  RelZoned(epoch_ns: Int, tz: TimeZone, cal: tcal.Calendar)
}

/// GetTemporalRelativeToOption, after the `relativeTo` value itself has been
/// read from the options bag.
pub fn convert_relative_to(st: Agent, v: JsVal) -> #(RelTo, Agent) {
  case classify(v) {
    KUndef -> #(RelNone, st)
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(
          kind: TemporalObj(TemporalZonedDateTime(
            epoch_ns:,
            time_zone:,
            calendar:,
          )),
          ..,
        ) -> #(RelZoned(epoch_ns, time_zone, calendar), st)
        SObject(
          kind: TemporalObj(TemporalDate(year:, month:, day:, calendar:)),
          ..,
        ) -> #(RelPlain(IsoDate(year, month, day), calendar), st)
        SObject(
          kind: TemporalObj(TemporalDateTime(year:, month:, day:, calendar:, ..)),
          ..,
        ) -> #(RelPlain(IsoDate(year, month, day), calendar), st)
        _ -> relative_from_bag(st, h)
      }
    KStr(s) ->
      case parse_iso_datetime_string(s) {
        None -> rt_val.t_throw_range_error(st, "invalid ISO 8601 string: " <> s)
        Some(p) -> {
          let Nil = terr(st, check_parsed_calendar(p))
          let cal = terr(st, parsed_calendar_id(p))
          let d = p.date
          case p.tz {
            Some(tz_str) -> {
              let tz = terr(st, parse_time_zone_id(tz_str))
              let ens =
                terr(
                  st,
                  zoned_string_epoch_ns(
                    d,
                    p.time,
                    p.offset,
                    tz,
                    Compatible,
                    RejectOffset,
                  ),
                )
              #(RelZoned(ens, tz, cal), st)
            }
            None ->
              case p.offset {
                Zulu ->
                  rt_val.t_throw_range_error(
                    st,
                    "Z designator requires a bracketed time zone in relativeTo",
                  )
                NoOffset | NumericOffset(_, _) ->
                  case iso_date_within_limits(d) {
                    True -> #(RelPlain(d, cal), st)
                    False ->
                      rt_val.t_throw_range_error(
                        st,
                        "date outside of supported range",
                      )
                  }
              }
          }
        }
      }
    _ ->
      rt_val.t_throw_type_error(st, "relativeTo must be a string or an object")
  }
}

/// ToRelativeTemporalObject's property-bag path: like zoned_from_bag but
/// timeZone is optional (absent → plain date) and the options are fixed
/// (overflow constrain, disambiguation compatible, offset reject).
fn relative_from_bag(st: Agent, bag: Handle) -> #(RelTo, Agent) {
  let #(cal, st) = read_bag_calendar(st, bag)
  let #(f, st) =
    read_date_time_fields(st, bag, cal, read_offset: True, read_tz: True)
  let date = terr(st, resolve_calendar_date(cal, f.date, Constrain))
  let t0 = time_fields_apply(f.time, midnight)
  let t = terr(st, regulate_time(t0, Constrain))
  case classify(f.tz) {
    KUndef ->
      case iso_date_within_limits(date) {
        True -> #(RelPlain(date, cal), st)
        False ->
          rt_val.t_throw_range_error(st, "date outside of supported range")
      }
    _ -> {
      let #(tz, st) = to_temporal_time_zone(st, f.tz)
      let behaviour = case f.offset {
        Some(o) -> OptionOffset(o)
        None -> WallOffset
      }
      let ens =
        terr(
          st,
          interpret_offset(
            date,
            t,
            behaviour,
            tz,
            Compatible,
            RejectOffset,
            False,
          ),
        )
      #(RelZoned(ens, tz, cal), st)
    }
  }
}

/// AddZonedDateTime: calendar part added in wall-clock space, time part
/// added exactly to the epoch instant.
pub fn add_zoned_ns(
  ns: Int,
  tz: TimeZone,
  cal: tcal.Calendar,
  dur: DurRec,
) -> Result(Int, TErr) {
  use base <- result.try(
    case dur.years == 0 && dur.months == 0 && dur.weeks == 0 && dur.days == 0 {
      True -> Ok(ns)
      False -> {
        use #(d0, t0) <- result.try(epoch_ns_to_iso_in(tz, ns))
        let date_dur =
          DurRec(
            ..zero_dur,
            years: dur.years,
            months: dur.months,
            weeks: dur.weeks,
            days: dur.days,
          )
        use d2 <- result.try(calendar_date_add(cal, d0, date_dur, Constrain))
        get_epoch_ns_for(tz, d2, t0, Compatible)
      }
    },
  )
  validate_epoch_ns(base + time_only_ns(dur))
}

/// DateDurationDays: a date duration's length in days anchored at a plain
/// date (years/months/weeks resolved through the calendar).
pub fn date_duration_days(
  dur: DurRec,
  rel: IsoDate,
  cal: tcal.Calendar,
) -> Result(Int, TErr) {
  case dur.years == 0 && dur.months == 0 && dur.weeks == 0 {
    True -> Ok(dur.days)
    False -> {
      let ymw =
        DurRec(
          ..zero_dur,
          years: dur.years,
          months: dur.months,
          weeks: dur.weeks,
        )
      use later <- result.map(calendar_date_add(cal, rel, ymw, Constrain))
      epoch_days(later) - epoch_days(rel) + dur.days
    }
  }
}
