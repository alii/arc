import arc/internal/temporal_calendar.{type Calendar}
import arc/time_zone

pub type TemporalZone {
  UtcZone
  OffsetZone(ns: Int)
  IanaZone(zone: time_zone.Zone)
}

pub type TemporalData {
  TemporalInstant(epoch_ns: Int)
  TemporalDate(year: Int, month: Int, day: Int, calendar: Calendar)
  TemporalTime(
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    microsecond: Int,
    nanosecond: Int,
  )
  TemporalDateTime(
    year: Int,
    month: Int,
    day: Int,
    hour: Int,
    minute: Int,
    second: Int,
    millisecond: Int,
    microsecond: Int,
    nanosecond: Int,
    calendar: Calendar,
  )
  TemporalYearMonth(year: Int, month: Int, day: Int, calendar: Calendar)
  TemporalMonthDay(month: Int, day: Int, ref_year: Int, calendar: Calendar)
  TemporalDuration(
    years: Int,
    months: Int,
    weeks: Int,
    days: Int,
    hours: Int,
    minutes: Int,
    seconds: Int,
    milliseconds: Int,
    microseconds: Int,
    nanoseconds: Int,
  )
  TemporalZonedDateTime(
    epoch_ns: Int,
    time_zone: TemporalZone,
    calendar: Calendar,
  )
}
