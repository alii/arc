import arc/bytecode/error_kind.{type JsError, JsError, RangeError}
import arc/internal/digits
import arc/rt/builtins/temporal_iso.{
  type IsoDate, type IsoTime, NoOffset, NumericOffset, Zulu, epoch_ns_to_iso,
  format_offset_minutes, is_tz_annotation, ns_max_instant, ns_per_day,
  ns_per_minute, ns_per_second, parse_iso_datetime_string, parse_offset_part,
}
import arc/rt/store as rt_store
import arc/rt/temporal_data.{
  type TemporalZone, IanaZone, OffsetZone, TemporalZonedDateTime, UtcZone,
}
import arc/rt/types.{
  type Agent, type JsVal, Agent, KHandle, KStr, SObject, TemporalObj, classify,
}
import arc/rt/val as rt_val
import arc/time_zone
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string

import arc/rt/builtins/temporal_rounding.{HalfExpand, round_to_increment}

pub fn time_zone_from_string(st: Agent, id: String) -> #(TemporalZone, Agent) {
  case parse_time_zone_identifier(st, id) {
    #(Ok(tz), st) -> #(tz, st)
    #(Error(UnknownIdentifier), st) -> {
      let #(tz, st) = tz_from_datetime_string(st, id)
      #(rt_val.or_throw(st, tz), st)
    }
    #(Error(InvalidIdentifier(e)), st) -> rt_val.throw(st, e)
  }
}

pub type TimeZoneIdError {
  UnknownIdentifier
  InvalidIdentifier(JsError)
}

pub fn parse_time_zone_identifier(
  st: Agent,
  id: String,
) -> #(Result(TemporalZone, TimeZoneIdError), Agent) {
  case string.uppercase(id) == "UTC" {
    True -> #(Ok(UtcZone), st)
    False ->
      case parse_offset_tz_id(id) {
        Some(ns) -> #(Ok(OffsetZone(ns:)), st)
        None ->
          case lookup_zone(st, id) {
            #(Ok(zone), st) -> #(Ok(IanaZone(zone:)), st)
            #(Error(time_zone.LoadFailed(id:, error:)), st) -> #(
              Error(InvalidIdentifier(unloadable_tz(id, error))),
              st,
            )
            #(Error(time_zone.UnknownZone), st) ->
              case is_tz_annotation(id) {
                True -> #(Error(InvalidIdentifier(unsupported_tz(id))), st)
                False -> #(Error(UnknownIdentifier), st)
              }
          }
      }
  }
}

// the agent keeps each zone the host loaded, see HostHooks.load_time_zone
pub fn lookup_zone(
  st: Agent,
  name: String,
) -> #(Result(time_zone.Zone, time_zone.ZoneLookupError), Agent) {
  case time_zone.lookup(name, st.tz_zones, st.hooks.load_time_zone) {
    Ok(#(zone, tz_zones)) -> #(Ok(zone), Agent(..st, tz_zones:))
    Error(err) -> #(Error(err), st)
  }
}

fn tz_from_datetime_string(
  st: Agent,
  s: String,
) -> #(Result(TemporalZone, JsError), Agent) {
  case parse_iso_datetime_string(s) {
    None -> #(Error(JsError(RangeError, "invalid time zone: " <> s)), st)
    Some(p) ->
      case p.tz {
        Some(tz_text) ->
          case parse_time_zone_identifier(st, tz_text) {
            #(Ok(tz), st) -> #(Ok(tz), st)
            #(Error(InvalidIdentifier(e)), st) -> #(Error(e), st)
            #(Error(UnknownIdentifier), st) -> #(
              Error(unsupported_tz(tz_text)),
              st,
            )
          }
        None ->
          case p.offset {
            Zulu -> #(Ok(UtcZone), st)
            NumericOffset(off, sub_minute) ->
              // seconds component not allowed, even ":00"
              case !sub_minute && off % ns_per_minute == 0 {
                True -> #(Ok(OffsetZone(ns: off)), st)
                False -> #(
                  Error(JsError(
                    RangeError,
                    "sub-minute offset not valid as a time zone",
                  )),
                  st,
                )
              }
            NoOffset -> #(
              Error(JsError(RangeError, "no time zone found in string: " <> s)),
              st,
            )
          }
      }
  }
}

pub fn parse_offset_tz_id(id: String) -> Option(Int) {
  let signed = case id {
    "+" <> _ | "-" <> _ -> True
    _ -> False
  }
  case signed {
    False -> None
    True ->
      case parse_offset_part(id) {
        // seconds component not allowed, even ":00"
        Some(#(NumericOffset(ns, False), "")) ->
          case ns % ns_per_minute == 0 && int.absolute_value(ns) < ns_per_day {
            True -> Some(ns)
            False -> None
          }
        _ -> None
      }
  }
}

pub fn time_zone_id(tz: TemporalZone) -> String {
  case tz {
    UtcZone -> "UTC"
    OffsetZone(ns:) -> format_offset_minutes(ns)
    IanaZone(zone:) -> time_zone.zone_id(zone)
  }
}

fn unsupported_tz(tz: String) -> JsError {
  JsError(RangeError, "time zone " <> tz <> " is not supported")
}

fn unloadable_tz(id: String, error: time_zone.TzError) -> JsError {
  JsError(
    RangeError,
    "time zone " <> id <> " cannot be loaded: " <> time_zone.describe(error),
  )
}

pub fn tz_offset_ns_at(tz: TemporalZone, epoch_ns: Int) -> Int {
  case tz {
    UtcZone -> 0
    OffsetZone(ns:) -> ns
    IanaZone(zone:) -> time_zone.offset_ns_at(zone, epoch_ns)
  }
}

pub fn epoch_ns_to_iso_in(
  tz: TemporalZone,
  epoch_ns: Int,
) -> #(IsoDate, IsoTime) {
  epoch_ns_to_iso(epoch_ns, tz_offset_ns_at(tz, epoch_ns))
}

pub fn is_valid_epoch_ns(ns: Int) -> Bool {
  int.absolute_value(ns) <= ns_max_instant
}

pub fn validate_epoch_ns(ns: Int) -> Result(Int, JsError) {
  case is_valid_epoch_ns(ns) {
    True -> Ok(ns)
    False -> Error(JsError(RangeError, "instant outside valid range"))
  }
}

pub fn format_offset_full(offset_ns: Int) -> String {
  let sign = case offset_ns < 0 {
    True -> "-"
    False -> "+"
  }
  let total_sec = int.absolute_value(offset_ns) / ns_per_second
  let base =
    sign
    <> digits.pad2(total_sec / 3600)
    <> ":"
    <> digits.pad2({ total_sec / 60 } % 60)
  case total_sec % 60 {
    0 -> base
    s -> base <> ":" <> digits.pad2(s)
  }
}

pub fn format_offset_rounded(offset_ns: Int) -> String {
  format_offset_minutes(round_to_increment(offset_ns, ns_per_minute, HalfExpand))
}

pub fn time_zone_equals(a: TemporalZone, b: TemporalZone) -> Bool {
  a == b
  || case a, b {
    OffsetZone(_), _ | _, OffsetZone(_) -> False
    UtcZone, UtcZone -> True
    UtcZone, IanaZone(z) | IanaZone(z), UtcZone ->
      time_zone.primary_identifier_of(z) == "UTC"
    IanaZone(za), IanaZone(zb) ->
      time_zone.primary_identifier_of(za) == time_zone.primary_identifier_of(zb)
  }
}

pub fn to_temporal_time_zone(st: Agent, v: JsVal) -> #(TemporalZone, Agent) {
  case classify(v) {
    KStr(s) -> time_zone_from_string(st, s)
    KHandle(h) ->
      case rt_store.cell_get(st, h) {
        SObject(
          kind: TemporalObj(data: TemporalZonedDateTime(time_zone:, ..)),
          ..,
        ) -> #(time_zone, st)
        _ -> rt_val.throw_type_error(st, "timeZone must be a string")
      }
    _ -> rt_val.throw_type_error(st, "timeZone must be a string")
  }
}

pub fn system_time_zone(st: Agent) -> #(TemporalZone, Agent) {
  case time_zone.time_zone_id(st.hooks.time_zone) {
    Some(id) ->
      case parse_time_zone_identifier(st, id) {
        #(Ok(tz), st) -> #(tz, st)
        #(Error(UnknownIdentifier), st) | #(Error(InvalidIdentifier(_)), st) -> #(
          UtcZone,
          st,
        )
      }
    None -> #(UtcZone, st)
  }
}
