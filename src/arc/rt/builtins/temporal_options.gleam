import arc/internal/temporal_calendar
import arc/rt/builtins/options.{get_options_object, opt_get}
import arc/rt/builtins/temporal_iso.{
  type IsoDate, type Overflow, Constrain, Reject, format_iso_date,
}
import arc/rt/types.{type Agent, type Handle, type JsVal, KUndef, classify}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{type Option}

pub fn get_enum_option(
  st: Agent,
  opts: Option(Handle),
  key: String,
  allowed: List(#(String, a)),
  default: a,
) -> #(a, Agent) {
  let #(v, st) = opt_get(st, opts, key)
  case classify(v) {
    KUndef -> #(default, st)
    _ -> {
      let #(s, st) = rt_val.t_to_string(st, v)
      case list.key_find(allowed, s) {
        Ok(parsed) -> #(parsed, st)
        Error(Nil) ->
          rt_val.t_throw_range_error(
            st,
            s <> " is not a valid value for option " <> key,
          )
      }
    }
  }
}

pub type Disambiguation {
  Compatible
  Earlier
  Later
  RejectDisambiguation
}

pub type OffsetOption {
  PreferOffset
  UseOffset
  IgnoreOffset
  RejectOffset
}

pub fn get_overflow_option(
  st: Agent,
  opts: Option(Handle),
) -> #(Overflow, Agent) {
  get_enum_option(
    st,
    opts,
    "overflow",
    [#("constrain", Constrain), #("reject", Reject)],
    Constrain,
  )
}

pub fn get_overflow_option_from_value(
  st: Agent,
  options: JsVal,
) -> #(Overflow, Agent) {
  let #(opts, st) = get_options_object(st, options)
  get_overflow_option(st, opts)
}

pub fn get_disambiguation_option(
  st: Agent,
  opts: Option(Handle),
) -> #(Disambiguation, Agent) {
  get_enum_option(
    st,
    opts,
    "disambiguation",
    [
      #("compatible", Compatible),
      #("earlier", Earlier),
      #("later", Later),
      #("reject", RejectDisambiguation),
    ],
    Compatible,
  )
}

pub fn get_offset_option(
  st: Agent,
  opts: Option(Handle),
  default: OffsetOption,
) -> #(OffsetOption, Agent) {
  get_enum_option(
    st,
    opts,
    "offset",
    [
      #("prefer", PreferOffset),
      #("use", UseOffset),
      #("ignore", IgnoreOffset),
      #("reject", RejectOffset),
    ],
    default,
  )
}

pub type CalendarNameMode {
  CalendarNameAuto
  CalendarNameAlways
  CalendarNameNever
  CalendarNameCritical
}

pub type ShowOffset {
  OffsetShowAuto
  OffsetShowNever
}

pub type TimeZoneNameMode {
  ZoneNameAuto
  ZoneNameNever
  ZoneNameCritical
}

pub fn get_calendar_name_option(
  st: Agent,
  opts: Option(Handle),
) -> #(CalendarNameMode, Agent) {
  get_enum_option(
    st,
    opts,
    "calendarName",
    [
      #("auto", CalendarNameAuto),
      #("always", CalendarNameAlways),
      #("never", CalendarNameNever),
      #("critical", CalendarNameCritical),
    ],
    CalendarNameAuto,
  )
}

pub fn get_show_offset_option(
  st: Agent,
  opts: Option(Handle),
) -> #(ShowOffset, Agent) {
  get_enum_option(
    st,
    opts,
    "offset",
    [#("auto", OffsetShowAuto), #("never", OffsetShowNever)],
    OffsetShowAuto,
  )
}

pub fn get_time_zone_name_option(
  st: Agent,
  opts: Option(Handle),
) -> #(TimeZoneNameMode, Agent) {
  get_enum_option(
    st,
    opts,
    "timeZoneName",
    [
      #("auto", ZoneNameAuto),
      #("never", ZoneNameNever),
      #("critical", ZoneNameCritical),
    ],
    ZoneNameAuto,
  )
}

pub fn calendar_suffix(
  mode: CalendarNameMode,
  cal: temporal_calendar.Calendar,
) -> String {
  let id = temporal_calendar.identifier(cal)
  case mode {
    CalendarNameNever -> ""
    CalendarNameAuto ->
      case cal {
        temporal_calendar.Iso8601 -> ""
        _ -> "[u-ca=" <> id <> "]"
      }
    CalendarNameAlways -> "[u-ca=" <> id <> "]"
    CalendarNameCritical -> "[!u-ca=" <> id <> "]"
  }
}

// year-month and month-day print the reference date unless iso omits it
pub fn format_with_reference(
  iso: IsoDate,
  cal: temporal_calendar.Calendar,
  mode: CalendarNameMode,
  short short: String,
) -> String {
  case cal, mode {
    temporal_calendar.Iso8601, CalendarNameAuto
    | temporal_calendar.Iso8601, CalendarNameNever
    -> short
    _, _ -> format_iso_date(iso) <> calendar_suffix(mode, cal)
  }
}
