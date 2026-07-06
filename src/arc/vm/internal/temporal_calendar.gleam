//// Non-ISO calendar arithmetic for Temporal (Intl era/monthCode semantics).
////
//// Supports the arithmetic (deterministic) calendars — gregory, buddhist,
//// japanese, roc, coptic, ethiopic, ethioaa, hebrew, islamic-civil (alias
//// islamicc), islamic-tbla, persian, indian — plus the table-driven
//// observation-based ones (islamic-umalqura, chinese, dangi), whose packed
//// year tables live in `temporal_calendar_data`.
////
//// All conversions are in "epoch days" — days since 1970-01-01 (matching
//// temporal.gleam's epoch_days). Algorithms follow Calendrical Calculations
//// (Reingold & Dershowitz) and ICU, which is what test262 expectations
//// are based on.

import arc/internal/gregorian.{
  civil_from_days, days_from_civil, days_in_month as gregorian_days_in_month,
  is_leap_year as is_gregorian_leap,
}
import arc/internal/int_math.{floor_div, floor_mod}
import arc/vm/internal/temporal_calendar_data.{
  chinese_data, dangi_data, umalqura_month_length, umalqura_year_start_fix,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// A date in a calendar: arithmetic year, ordinal month (1-based), day.
pub type CalDate {
  CalDate(year: Int, month: Int, day: Int)
}

/// Why a month code doesn't resolve in a given year.
pub type MonthCodeIssue {
  /// The code never occurs in this calendar (e.g. "M13" in gregory).
  NeverValid
  /// The code exists in some years but not this one (e.g. "M05L" in a
  /// non-leap hebrew year). `skip_to` is the ordinal month to constrain to.
  NotInThisYear(skip_to: Int)
}

// ============================================================================
// Identifier handling
// ============================================================================

/// A supported calendar, as a closed set. The ONLY way to obtain one from a
/// string is `canonicalize`, so holding a `Calendar` is proof the identifier
/// was validated and aliases resolved. Render it back with `identifier`.
pub type Calendar {
  Iso8601
  Gregory
  Buddhist
  Japanese
  Roc
  Coptic
  Ethiopic
  Ethioaa
  Hebrew
  IslamicCivil
  IslamicTbla
  IslamicUmalqura
  Persian
  Indian
  Chinese
  Dangi
}

/// Canonicalize a calendar identifier (case-insensitive, resolves aliases).
/// Error(Nil) for unsupported identifiers. This is the only String ->
/// Calendar site.
pub fn canonicalize(id: String) -> Result(Calendar, Nil) {
  case string.lowercase(id) {
    "iso8601" -> Ok(Iso8601)
    "gregory" | "gregorian" -> Ok(Gregory)
    "buddhist" -> Ok(Buddhist)
    "japanese" -> Ok(Japanese)
    "roc" -> Ok(Roc)
    "coptic" -> Ok(Coptic)
    "ethiopic" -> Ok(Ethiopic)
    "ethioaa" | "ethiopic-amete-alem" -> Ok(Ethioaa)
    "hebrew" -> Ok(Hebrew)
    "islamic-civil" | "islamicc" -> Ok(IslamicCivil)
    "islamic-tbla" -> Ok(IslamicTbla)
    "islamic-umalqura" -> Ok(IslamicUmalqura)
    "persian" -> Ok(Persian)
    "indian" -> Ok(Indian)
    "chinese" -> Ok(Chinese)
    "dangi" -> Ok(Dangi)
    _ -> Error(Nil)
  }
}

/// Canonical Temporal identifier for a calendar (inverse of `canonicalize`).
pub fn identifier(cal: Calendar) -> String {
  case cal {
    Iso8601 -> "iso8601"
    Gregory -> "gregory"
    Buddhist -> "buddhist"
    Japanese -> "japanese"
    Roc -> "roc"
    Coptic -> "coptic"
    Ethiopic -> "ethiopic"
    Ethioaa -> "ethioaa"
    Hebrew -> "hebrew"
    IslamicCivil -> "islamic-civil"
    IslamicTbla -> "islamic-tbla"
    IslamicUmalqura -> "islamic-umalqura"
    Persian -> "persian"
    Indian -> "indian"
    Chinese -> "chinese"
    Dangi -> "dangi"
  }
}

/// The date-arithmetic family a calendar belongs to, carrying the family's
/// parameters. `arithmetic` derives it exhaustively from `Calendar`, so the
/// per-family algorithms below never dispatch on (or default over) calendar
/// identifiers themselves.
type Arithmetic {
  /// Same months/days as ISO 8601; calendar year = ISO year + `year_offset`.
  IsoLike(year_offset: Int)
  /// Coptic family: 12 thirty-day months plus an epagomenal 13th.
  /// `year_shift` maps the arithmetic year onto the coptic year count
  /// (ethioaa's amete alem epoch is 5500 years before amete mihret).
  CopticLike(epoch: Int, year_shift: Int)
  /// 30-year-cycle tabular Islamic.
  TabularIslamic(epoch: Int)
  /// Islamic Umm al-Qura (ICU table for AH 1300-1600, civil fallback).
  UmmAlQura
  PersianArith
  IndianArith
  HebrewArith
  /// Chinese/Dangi table-driven lunisolar; `data` is the packed year table.
  LunisolarArith(data: fn(Int) -> Result(Int, Nil))
}

fn arithmetic(cal: Calendar) -> Arithmetic {
  case cal {
    Iso8601 | Gregory | Japanese -> IsoLike(0)
    Buddhist -> IsoLike(543)
    Roc -> IsoLike(-1911)
    Coptic -> CopticLike(epoch: coptic_epoch, year_shift: 0)
    Ethiopic -> CopticLike(epoch: ethiopic_epoch, year_shift: 0)
    Ethioaa -> CopticLike(epoch: ethiopic_epoch, year_shift: 5500)
    IslamicCivil -> TabularIslamic(islamic_civil_epoch)
    IslamicTbla -> TabularIslamic(islamic_tbla_epoch)
    IslamicUmalqura -> UmmAlQura
    Persian -> PersianArith
    Indian -> IndianArith
    Hebrew -> HebrewArith
    Chinese -> LunisolarArith(chinese_data)
    Dangi -> LunisolarArith(dangi_data)
  }
}

// ============================================================================
// Coptic / Ethiopic family
// ============================================================================

// Rata Die epochs converted to days-since-1970 (RD - 719163).
const coptic_epoch = -615_558

const ethiopic_epoch = -716_367

/// `epoch`/`shift` come from the calendar's `CopticLike` arithmetic record.
fn coptic_to_days(
  epoch: Int,
  shift: Int,
  year: Int,
  month: Int,
  day: Int,
) -> Int {
  let y = year - shift
  epoch - 1 + 365 * { y - 1 } + floor_div(y, 4) + 30 * { month - 1 } + day
}

fn coptic_from_days(epoch: Int, shift: Int, date: Int) -> CalDate {
  let y = floor_div(4 * { date - epoch } + 1463, 1461)
  let m =
    floor_div(date - coptic_to_days(epoch, shift, y + shift, 1, 1), 30) + 1
  let d = date - coptic_to_days(epoch, shift, y + shift, m, 1) + 1
  CalDate(y + shift, m, d)
}

fn coptic_is_leap(shift: Int, year: Int) -> Bool {
  floor_mod(year - shift, 4) == 3
}

fn coptic_days_in_month(shift: Int, year: Int, month: Int) -> Int {
  case month {
    13 ->
      case coptic_is_leap(shift, year) {
        True -> 6
        False -> 5
      }
    _ -> 30
  }
}

// ============================================================================
// Tabular Islamic (civil and tbla)
// ============================================================================

const islamic_civil_epoch = -492_148

const islamic_tbla_epoch = -492_149

fn islamic_is_leap(year: Int) -> Bool {
  floor_mod(14 + 11 * year, 30) < 11
}

/// `epoch` comes from the calendar's `TabularIslamic` arithmetic record.
fn islamic_to_days(epoch: Int, year: Int, month: Int, day: Int) -> Int {
  epoch
  - 1
  + 354
  * { year - 1 }
  + floor_div(3 + 11 * year, 30)
  + 29
  * { month - 1 }
  + floor_div(month, 2)
  + day
}

fn islamic_days_in_month(year: Int, month: Int) -> Int {
  case month == 12 && islamic_is_leap(year) {
    True -> 30
    False ->
      case floor_mod(month, 2) == 1 {
        True -> 30
        False -> 29
      }
  }
}

fn islamic_from_days(epoch: Int, date: Int) -> CalDate {
  let y0 = floor_div(30 * { date - epoch } + 10_646, 10_631)
  let y = adjust_year(date, y0, fn(yy) { islamic_to_days(epoch, yy, 1, 1) })
  let #(m, d) =
    scan_months(date, y, 1, 12, fn(yy, mm) { islamic_to_days(epoch, yy, mm, 1) })
  CalDate(y, m, d)
}

// ============================================================================
// Islamic Umm al-Qura (ICU table for AH 1300–1600, civil fallback outside)
// ============================================================================

/// A umm al-Qura year is either inside the ICU tables (its packed month
/// lengths and its year-start correction unpacked here, once) or outside them,
/// where the tabular Islamic civil calendar takes over. The tables themselves
/// answer "is this year covered?" — there is no separately-maintained range
/// test that could drift out of step with them.
type UmalquraYear {
  UmTabulated(month_bits: Int, year_start_fix: Int)
  UmCivil
}

fn umalqura_year(year: Int) -> UmalquraYear {
  let tabulated = {
    use month_bits <- result.try(umalqura_month_length(year))
    use year_start_fix <- result.map(umalqura_year_start_fix(year))
    UmTabulated(month_bits:, year_start_fix:)
  }
  result.unwrap(tabulated, UmCivil)
}

/// Day count of the start of an AH year, origin 0 at civil AH 1-01-01.
fn umalqura_year_start(year: Int) -> Int {
  case umalqura_year(year) {
    UmTabulated(year_start_fix:, ..) ->
      floor_div(35_436_720 * { year - 1300 } + 46_032_255_000, 100_000)
      + year_start_fix
    UmCivil -> 354 * { year - 1 } + floor_div(3 + 11 * year, 30)
  }
}

fn umalqura_days_in_month(year: Int, month: Int) -> Int {
  case umalqura_year(year) {
    UmTabulated(month_bits:, ..) ->
      29 + int.bitwise_and(int.bitwise_shift_right(month_bits, 12 - month), 1)
    UmCivil -> islamic_days_in_month(year, month)
  }
}

fn umalqura_days_before_month(year: Int, month: Int) -> Int {
  case month <= 1 {
    True -> 0
    False ->
      umalqura_days_before_month(year, month - 1)
      + umalqura_days_in_month(year, month - 1)
  }
}

fn umalqura_to_days(year: Int, month: Int, day: Int) -> Int {
  islamic_civil_epoch
  + umalqura_year_start(year)
  + umalqura_days_before_month(year, month)
  + day
  - 1
}

fn umalqura_from_days(date: Int) -> CalDate {
  let y0 = floor_div(30 * { date - islamic_civil_epoch } + 10_646, 10_631)
  let y = adjust_year(date, y0, fn(yy) { umalqura_to_days(yy, 1, 1) })
  let #(m, d) =
    scan_months(date, y, 1, 12, fn(yy, mm) { umalqura_to_days(yy, mm, 1) })
  CalDate(y, m, d)
}

// ============================================================================
// Persian (arithmetic, ICU algorithm)
// ============================================================================

const persian_epoch = -492_268

fn persian_is_leap(year: Int) -> Bool {
  floor_mod(25 * year + 11, 33) < 8
}

fn persian_to_days(year: Int, month: Int, day: Int) -> Int {
  let mo = month - 1
  let offset = case mo <= 6 {
    True -> 31 * mo
    False -> 30 * mo + 6
  }
  persian_epoch
  - 1
  + 365
  * { year - 1 }
  + floor_div(8 * year + 21, 33)
  + offset
  + day
}

fn persian_days_in_month(year: Int, month: Int) -> Int {
  case month <= 6 {
    True -> 31
    False ->
      case month <= 11 {
        True -> 30
        False ->
          case persian_is_leap(year) {
            True -> 30
            False -> 29
          }
      }
  }
}

fn persian_from_days(date: Int) -> CalDate {
  let y0 = 1 + floor_div(33 * { date - persian_epoch } + 3, 12_053)
  let y = adjust_year(date, y0, fn(yy) { persian_to_days(yy, 1, 1) })
  let #(m, d) =
    scan_months(date, y, 1, 12, fn(yy, mm) { persian_to_days(yy, mm, 1) })
  CalDate(y, m, d)
}

// ============================================================================
// Indian national (Saka) calendar, ICU algorithm
// ============================================================================

fn indian_year_start(eyear: Int) -> Int {
  let gyear = eyear + 78
  case is_gregorian_leap(gyear) {
    True -> days_from_civil(gyear, 3, 21)
    False -> days_from_civil(gyear, 3, 22)
  }
}

fn indian_days_in_month(eyear: Int, month: Int) -> Int {
  case month {
    1 ->
      case is_gregorian_leap(eyear + 78) {
        True -> 31
        False -> 30
      }
    _ ->
      case month <= 6 {
        True -> 31
        False -> 30
      }
  }
}

fn indian_to_days(eyear: Int, month: Int, day: Int) -> Int {
  let first_len = indian_days_in_month(eyear, 1)
  let offset = case month {
    1 -> 0
    _ ->
      case month <= 6 {
        True -> first_len + 31 * { month - 2 }
        False -> first_len + 31 * 5 + 30 * { month - 7 }
      }
  }
  indian_year_start(eyear) + offset + day - 1
}

fn indian_from_days(date: Int) -> CalDate {
  let #(gy, _, _) = civil_from_days(date)
  let ey0 = gy - 78
  let ey = case date < indian_year_start(ey0) {
    True -> ey0 - 1
    False -> ey0
  }
  let #(m, d) =
    scan_months(date, ey, 1, 12, fn(yy, mm) { indian_to_days(yy, mm, 1) })
  CalDate(ey, m, d)
}

// ============================================================================
// Hebrew (arithmetic, molad-based — Calendrical Calculations)
// ============================================================================

const hebrew_epoch = -2_092_590

pub fn hebrew_is_leap(year: Int) -> Bool {
  floor_mod(7 * year + 1, 19) < 7
}

fn hebrew_elapsed_days(year: Int) -> Int {
  let months_elapsed = floor_div(235 * year - 234, 19)
  let parts_elapsed = 12_084 + 13_753 * months_elapsed
  let days = 29 * months_elapsed + floor_div(parts_elapsed, 25_920)
  case floor_mod(3 * { days + 1 }, 7) < 3 {
    True -> days + 1
    False -> days
  }
}

fn hebrew_year_length_correction(e0: Int, e1: Int, e2: Int) -> Int {
  case e2 - e1 == 356 {
    True -> 2
    False ->
      case e1 - e0 == 382 {
        True -> 1
        False -> 0
      }
  }
}

/// Epoch days of Tishri 1 in the given hebrew year.
fn hebrew_new_year(year: Int) -> Int {
  let e0 = hebrew_elapsed_days(year - 1)
  let e1 = hebrew_elapsed_days(year)
  let e2 = hebrew_elapsed_days(year + 1)
  hebrew_epoch + e1 + hebrew_year_length_correction(e0, e1, e2)
}

/// Per-year shape, computed once and threaded through month arithmetic so a
/// date conversion needs only a handful of hebrew_elapsed_days calls instead
/// of several per month touched.
type HebrewYearShape {
  HebrewYearShape(new_year: Int, length: Int, leap: Bool)
}

fn hebrew_year_shape(year: Int) -> HebrewYearShape {
  let e0 = hebrew_elapsed_days(year - 1)
  let e1 = hebrew_elapsed_days(year)
  let e2 = hebrew_elapsed_days(year + 1)
  let e3 = hebrew_elapsed_days(year + 2)
  let ny = hebrew_epoch + e1 + hebrew_year_length_correction(e0, e1, e2)
  let ny_next = hebrew_epoch + e2 + hebrew_year_length_correction(e1, e2, e3)
  HebrewYearShape(
    new_year: ny,
    length: ny_next - ny,
    leap: hebrew_is_leap(year),
  )
}

fn hebrew_year_length(year: Int) -> Int {
  hebrew_year_shape(year).length
}

/// Days in ordinal month (civil order: 1 = Tishri).
fn hebrew_days_in_month(year: Int, month: Int) -> Int {
  hebrew_shape_days_in_month(hebrew_year_shape(year), month)
}

fn hebrew_shape_days_in_month(shape: HebrewYearShape, month: Int) -> Int {
  let HebrewYearShape(length: ylen, leap:, ..) = shape
  case month {
    1 -> 30
    2 ->
      // Heshvan: long in 355/385-day years
      case ylen == 355 || ylen == 385 {
        True -> 30
        False -> 29
      }
    3 ->
      // Kislev: short in 353/383-day years
      case ylen == 353 || ylen == 383 {
        True -> 29
        False -> 30
      }
    4 -> 29
    5 -> 30
    _ ->
      case leap {
        True ->
          case month {
            6 -> 30
            // Adar I
            7 -> 29
            // Adar II
            _ ->
              // Nisan(8)=30, Iyar(9)=29, Sivan(10)=30, Tammuz(11)=29,
              // Av(12)=30, Elul(13)=29
              case floor_mod(month, 2) == 0 {
                True -> 30
                False -> 29
              }
          }
        False ->
          case month {
            6 -> 29
            // Adar
            _ ->
              // Nisan(7)=30, Iyar(8)=29, ... Elul(12)=29
              case floor_mod(month, 2) == 1 {
                True -> 30
                False -> 29
              }
          }
      }
  }
}

fn hebrew_months_in_year(year: Int) -> Int {
  case hebrew_is_leap(year) {
    True -> 13
    False -> 12
  }
}

fn hebrew_to_days(year: Int, month: Int, day: Int) -> Int {
  let shape = hebrew_year_shape(year)
  shape.new_year + hebrew_days_before_month(shape, month) + day - 1
}

fn hebrew_days_before_month(shape: HebrewYearShape, month: Int) -> Int {
  sum_months(shape, 1, month, 0)
}

fn sum_months(shape: HebrewYearShape, m: Int, until: Int, acc: Int) -> Int {
  case m < until {
    True ->
      sum_months(
        shape,
        m + 1,
        until,
        acc + hebrew_shape_days_in_month(shape, m),
      )
    False -> acc
  }
}

fn hebrew_from_days(date: Int) -> CalDate {
  // Approximate year, then adjust.
  let approx = floor_div(98_496 * { date - hebrew_epoch }, 35_975_351) + 1
  let y = adjust_year(date, approx, hebrew_new_year)
  let shape = hebrew_year_shape(y)
  let months = case shape.leap {
    True -> 13
    False -> 12
  }
  let #(m, d) = hebrew_scan_months(date, shape, 1, months, shape.new_year)
  CalDate(y, m, d)
}

/// Find the month containing `date` by walking month starts incrementally;
/// returns #(month, day).
fn hebrew_scan_months(
  date: Int,
  shape: HebrewYearShape,
  m: Int,
  max: Int,
  start: Int,
) -> #(Int, Int) {
  let next = start + hebrew_shape_days_in_month(shape, m)
  case m < max && date >= next {
    True -> hebrew_scan_months(date, shape, m + 1, max, next)
    False -> #(m, date - start + 1)
  }
}

// ============================================================================
// Generic year/month search helpers
// ============================================================================

/// Adjust an approximate year so that year_start(y) <= date < year_start(y+1).
fn adjust_year(date: Int, y: Int, year_start: fn(Int) -> Int) -> Int {
  case date < year_start(y) {
    True -> adjust_year(date, y - 1, year_start)
    False ->
      case date >= year_start(y + 1) {
        True -> adjust_year(date, y + 1, year_start)
        False -> y
      }
  }
}

/// Find the month containing `date` by scanning month starts; returns
/// #(month, day).
fn scan_months(
  date: Int,
  year: Int,
  m: Int,
  max: Int,
  month_start: fn(Int, Int) -> Int,
) -> #(Int, Int) {
  case m < max && date >= month_start(year, m + 1) {
    True -> scan_months(date, year, m + 1, max, month_start)
    False -> #(m, date - month_start(year, m) + 1)
  }
}

// ============================================================================
// Public conversion API
// ============================================================================

/// Calendar date for an epoch-days value.
pub fn date_from_epoch_days(cal: Calendar, days: Int) -> CalDate {
  case arithmetic(cal) {
    IsoLike(offset) -> {
      let #(y, m, d) = civil_from_days(days)
      CalDate(y + offset, m, d)
    }
    CopticLike(epoch:, year_shift:) -> coptic_from_days(epoch, year_shift, days)
    TabularIslamic(epoch) -> islamic_from_days(epoch, days)
    UmmAlQura -> umalqura_from_days(days)
    PersianArith -> persian_from_days(days)
    IndianArith -> indian_from_days(days)
    HebrewArith -> hebrew_from_days(days)
    LunisolarArith(data) -> lunisolar_from_days(data, days)
  }
}

/// Epoch days for a (valid) calendar date.
pub fn date_to_epoch_days(
  cal: Calendar,
  year: Int,
  month: Int,
  day: Int,
) -> Int {
  case arithmetic(cal) {
    IsoLike(offset) -> days_from_civil(year - offset, month, day)
    CopticLike(epoch:, year_shift:) ->
      coptic_to_days(epoch, year_shift, year, month, day)
    TabularIslamic(epoch) -> islamic_to_days(epoch, year, month, day)
    UmmAlQura -> umalqura_to_days(year, month, day)
    PersianArith -> persian_to_days(year, month, day)
    IndianArith -> indian_to_days(year, month, day)
    HebrewArith -> hebrew_to_days(year, month, day)
    LunisolarArith(data) -> lunisolar_to_days(data, year, month, day)
  }
}

pub fn months_in_year(cal: Calendar, year: Int) -> Int {
  case arithmetic(cal) {
    CopticLike(..) -> 13
    HebrewArith -> hebrew_months_in_year(year)
    LunisolarArith(data) ->
      case lunisolar_leap_num(data, year) == 0 {
        True -> 12
        False -> 13
      }
    IsoLike(_) | TabularIslamic(_) | UmmAlQura | PersianArith | IndianArith ->
      12
  }
}

pub fn days_in_month(cal: Calendar, year: Int, month: Int) -> Int {
  case arithmetic(cal) {
    IsoLike(offset) -> gregorian_days_in_month(year - offset, month)
    CopticLike(epoch: _, year_shift:) ->
      coptic_days_in_month(year_shift, year, month)
    TabularIslamic(_) -> islamic_days_in_month(year, month)
    UmmAlQura -> umalqura_days_in_month(year, month)
    PersianArith -> persian_days_in_month(year, month)
    IndianArith -> indian_days_in_month(year, month)
    HebrewArith -> hebrew_days_in_month(year, month)
    LunisolarArith(data) -> lunisolar_month_len(data, year, month)
  }
}

pub fn days_in_year(cal: Calendar, year: Int) -> Int {
  case arithmetic(cal) {
    HebrewArith -> hebrew_year_length(year)
    IsoLike(_)
    | CopticLike(..)
    | TabularIslamic(_)
    | UmmAlQura
    | PersianArith
    | IndianArith
    | LunisolarArith(_) ->
      date_to_epoch_days(cal, year + 1, 1, 1)
      - date_to_epoch_days(cal, year, 1, 1)
  }
}

pub fn in_leap_year(cal: Calendar, year: Int) -> Bool {
  case arithmetic(cal) {
    IsoLike(offset) -> is_gregorian_leap(year - offset)
    CopticLike(epoch: _, year_shift:) -> coptic_is_leap(year_shift, year)
    TabularIslamic(_) -> islamic_is_leap(year)
    UmmAlQura ->
      umalqura_to_days(year + 1, 1, 1) - umalqura_to_days(year, 1, 1) > 354
    PersianArith -> persian_is_leap(year)
    IndianArith -> is_gregorian_leap(year + 78)
    HebrewArith -> hebrew_is_leap(year)
    LunisolarArith(data) -> lunisolar_leap_num(data, year) != 0
  }
}

/// 1-based day of year for a calendar date.
pub fn day_of_year(cal: Calendar, year: Int, month: Int, day: Int) -> Int {
  date_to_epoch_days(cal, year, month, day)
  - date_to_epoch_days(cal, year, 1, 1)
  + 1
}

// ============================================================================
// Month codes
// ============================================================================

fn pad2(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

/// A month code: the CLDR month number plus whether it names the leap month
/// that follows it. This is the value `month_code` renders and
/// `month_for_code` consumes — callers that need both never have to render a
/// string and parse it back.
pub type MonthCode {
  MonthCode(number: Int, leap: Bool)
}

/// Month code of an ordinal month in a year.
pub fn month_code_of(cal: Calendar, year: Int, month: Int) -> MonthCode {
  case arithmetic(cal) {
    HebrewArith ->
      case hebrew_is_leap(year) {
        True ->
          case month == 6 {
            True -> MonthCode(number: 5, leap: True)
            False ->
              case month > 6 {
                True -> MonthCode(number: month - 1, leap: False)
                False -> MonthCode(number: month, leap: False)
              }
          }
        False -> MonthCode(number: month, leap: False)
      }
    LunisolarArith(data) -> {
      let leap = lunisolar_leap_num(data, year)
      case leap > 0 && month == leap + 1 {
        True -> MonthCode(number: leap, leap: True)
        False ->
          case leap > 0 && month > leap + 1 {
            True -> MonthCode(number: month - 1, leap: False)
            False -> MonthCode(number: month, leap: False)
          }
      }
    }
    IsoLike(_)
    | CopticLike(..)
    | TabularIslamic(_)
    | UmmAlQura
    | PersianArith
    | IndianArith -> MonthCode(number: month, leap: False)
  }
}

/// Render a month code the way JavaScript sees it: "M01".."M13", "M05L".
pub fn month_code_string(mc: MonthCode) -> String {
  case mc.leap {
    True -> "M" <> pad2(mc.number) <> "L"
    False -> "M" <> pad2(mc.number)
  }
}

/// Month code string for an ordinal month in a year.
pub fn month_code(cal: Calendar, year: Int, month: Int) -> String {
  month_code_string(month_code_of(cal, year, month))
}

/// Resolve a USER-supplied month code to an ordinal month within `year`.
/// `NeverValid` is only reachable for codes that came in from JavaScript, so
/// this is the parse-boundary entry point; code minted by `month_code_of` and
/// carried into another year goes through `carry_month_code` instead.
pub fn month_for_code(
  cal: Calendar,
  year: Int,
  mc: MonthCode,
) -> Result(Int, MonthCodeIssue) {
  let MonthCode(number: num, leap:) = mc
  case arithmetic(cal), leap {
    HebrewArith, True ->
      case num == 5 {
        True ->
          case hebrew_is_leap(year) {
            True -> Ok(6)
            // Skip-forward: constrain M05L to M06 (Adar II = ordinal 6).
            False -> Error(NotInThisYear(6))
          }
        False -> Error(NeverValid)
      }
    LunisolarArith(data), True ->
      case num >= 1 && num <= 12 {
        False -> Error(NeverValid)
        True -> {
          let leap_month = lunisolar_leap_num(data, year)
          case leap_month == num {
            True -> Ok(num + 1)
            // Constrain MxxL to the regular month Mxx of this year.
            False ->
              Error(
                NotInThisYear(case leap_month > 0 && num > leap_month {
                  True -> num + 1
                  False -> num
                }),
              )
          }
        }
      }
    IsoLike(_), True
    | CopticLike(..), True
    | TabularIslamic(_), True
    | UmmAlQura, True
    | PersianArith, True
    | IndianArith, True
    -> Error(NeverValid)
    HebrewArith, False ->
      case num >= 1 && num <= 12 {
        True ->
          case hebrew_is_leap(year) && num >= 6 {
            True -> Ok(num + 1)
            False -> Ok(num)
          }
        False -> Error(NeverValid)
      }
    CopticLike(..), False ->
      case num >= 1 && num <= 13 {
        True -> Ok(num)
        False -> Error(NeverValid)
      }
    LunisolarArith(data), False ->
      case num >= 1 && num <= 12 {
        True -> {
          let leap_month = lunisolar_leap_num(data, year)
          case leap_month > 0 && num > leap_month {
            True -> Ok(num + 1)
            False -> Ok(num)
          }
        }
        False -> Error(NeverValid)
      }
    IsoLike(_), False
    | TabularIslamic(_), False
    | UmmAlQura, False
    | PersianArith, False
    | IndianArith, False
    ->
      case num >= 1 && num <= 12 {
        True -> Ok(num)
        False -> Error(NeverValid)
      }
  }
}

/// Carry a month code MINTED BY `month_code_of` (so it is a code the calendar
/// really has) into `target_year`. `Ok(ordinal)` when the code occurs there;
/// `Error(skip_to)` — the ordinal to constrain to — when it does not, which
/// only ever happens for a leap month absent from that year.
///
/// This is `month_for_code` minus its `NeverValid` variant: minted codes
/// cannot be invalid for their own calendar, so callers that carry a code
/// forward do not have to invent an answer for a case that cannot occur.
pub fn carry_month_code(
  cal: Calendar,
  target_year: Int,
  mc: MonthCode,
) -> Result(Int, Int) {
  case month_for_code(cal, target_year, mc) {
    Ok(ordinal) -> Ok(ordinal)
    Error(NotInThisYear(skip_to)) -> Error(skip_to)
    // Unreachable for a minted code. Absorb it here, once, rather than at
    // every call site: clamp into the target year like any other overflow.
    Error(NeverValid) ->
      Error(int.min(mc.number, months_in_year(cal, target_year)))
  }
}

// ============================================================================
// Eras
// ============================================================================

/// True when the calendar's dates carry era/eraYear. The iso8601/chinese/dangi
/// calendars have no eras (era/eraYear fields are ignored entirely). Derived
/// from `eras_of`, so the two can never disagree.
pub fn has_eras(cal: Calendar) -> Bool {
  eras_of(cal) != []
}

/// The closed set of era codes any supported calendar can name. Obtaining one
/// from a string goes through `parse_era_code`, so an `EraCode` in hand is
/// proof the code exists (whether it suits a *given* calendar is
/// `year_for_era`'s answer). Render it back with `era_code_string`.
pub type EraCode {
  Ce
  Bce
  Be
  /// "roc" — the Minguo era of the roc calendar. Named for the era rather
  /// than the calendar so it does not collide with the `Roc` calendar.
  Minguo
  /// "broc" — years before the Minguo era.
  BeforeMinguo
  Am
  Aa
  Ah
  Bh
  Ap
  Shaka
  Reiwa
  Heisei
  Showa
  Taisho
  Meiji
}

/// Parse an era code, resolving the "ad"/"bc" aliases of "ce"/"bce".
/// Error(Nil) for codes no calendar uses. This is the only String -> EraCode
/// site.
pub fn parse_era_code(s: String) -> Result(EraCode, Nil) {
  case s {
    "ce" | "ad" -> Ok(Ce)
    "bce" | "bc" -> Ok(Bce)
    "be" -> Ok(Be)
    "roc" -> Ok(Minguo)
    "broc" -> Ok(BeforeMinguo)
    "am" -> Ok(Am)
    "aa" -> Ok(Aa)
    "ah" -> Ok(Ah)
    "bh" -> Ok(Bh)
    "ap" -> Ok(Ap)
    "shaka" -> Ok(Shaka)
    "reiwa" -> Ok(Reiwa)
    "heisei" -> Ok(Heisei)
    "showa" -> Ok(Showa)
    "taisho" -> Ok(Taisho)
    "meiji" -> Ok(Meiji)
    _ -> Error(Nil)
  }
}

/// Canonical era code as JavaScript sees it (inverse of `parse_era_code` on
/// its non-alias inputs).
pub fn era_code_string(code: EraCode) -> String {
  case code {
    Ce -> "ce"
    Bce -> "bce"
    Be -> "be"
    Minguo -> "roc"
    BeforeMinguo -> "broc"
    Am -> "am"
    Aa -> "aa"
    Ah -> "ah"
    Bh -> "bh"
    Ap -> "ap"
    Shaka -> "shaka"
    Reiwa -> "reiwa"
    Heisei -> "heisei"
    Showa -> "showa"
    Taisho -> "taisho"
    Meiji -> "meiji"
  }
}

/// An era code paired with the year within it. The two always travel
/// together — a calendar either has eras (both present) or does not (neither),
/// so `Option(Era)` is the only shape callers ever see.
pub type Era {
  Era(code: EraCode, year: Int)
}

/// era + eraYear for a calendar date. `None` for era-less calendars.
///
/// Only decides WHICH era code applies; the eraYear then falls out of the
/// shift `eras_of` already declares for that code, so the two directions of
/// the mapping cannot drift apart.
pub fn era_for(cal: Calendar, year: Int, month: Int, day: Int) -> Option(Era) {
  use code <- option.map(era_code_for(cal, year, month, day))
  // Invariant: every code `era_code_for` can hand back for `cal` is declared
  // in `eras_of(cal)`. A miss means the two tables have drifted apart, which
  // is a bug — not a date that legitimately has no era. Fail loudly rather
  // than silently reporting `None`.
  let assert Ok(shift) = list.key_find(eras_of(cal), code)
  Era(code, era_year(shift, year))
}

/// The era code a calendar date falls in. `None` for era-less calendars.
fn era_code_for(
  cal: Calendar,
  year: Int,
  month: Int,
  day: Int,
) -> Option(EraCode) {
  case cal {
    Iso8601 | Chinese | Dangi -> None
    Gregory ->
      Some(case year >= 1 {
        True -> Ce
        False -> Bce
      })
    Buddhist -> Some(Be)
    Japanese -> Some(japanese_era_code(year, month, day))
    Roc ->
      Some(case year >= 1 {
        True -> Minguo
        False -> BeforeMinguo
      })
    Coptic -> Some(Am)
    Ethiopic ->
      Some(case year >= 1 {
        True -> Am
        False -> Aa
      })
    Ethioaa -> Some(Aa)
    Hebrew -> Some(Am)
    IslamicCivil | IslamicTbla | IslamicUmalqura ->
      Some(case year >= 1 {
        True -> Ah
        False -> Bh
      })
    Persian -> Some(Ap)
    Indian -> Some(Shaka)
  }
}

/// Japanese era for an ISO date (japanese arithmetic year == ISO year).
/// Modern named eras start: meiji 6 = 1873-01-01 (output cutoff), taisho
/// 1912-07-30, showa 1926-12-25, heisei 1989-01-08, reiwa 2019-05-01.
fn japanese_era_code(y: Int, m: Int, d: Int) -> EraCode {
  let after = fn(ey: Int, em: Int, ed: Int) {
    y > ey || { y == ey && { m > em || { m == em && d >= ed } } }
  }
  case after(2019, 5, 1) {
    True -> Reiwa
    False ->
      case after(1989, 1, 8) {
        True -> Heisei
        False ->
          case after(1926, 12, 25) {
            True -> Showa
            False ->
              case after(1912, 7, 30) {
                True -> Taisho
                False ->
                  case after(1873, 1, 1) {
                    True -> Meiji
                    False ->
                      case y >= 1 {
                        True -> Ce
                        False -> Bce
                      }
                  }
              }
          }
      }
  }
}

/// How an era code maps onto the arithmetic year. `Forward(k)` means
/// year = k + eraYear (eras that count up with the arithmetic year);
/// `Backward(k)` means year = k - eraYear (eras that count backwards from it,
/// like bce).
type EraShift {
  Forward(Int)
  Backward(Int)
}

/// The era codes a calendar accepts, and how each maps onto its arithmetic
/// year. Exhaustive over `Calendar`, so a new calendar has to declare its eras
/// (`[]` for none) rather than silently inheriting a fallback.
fn eras_of(cal: Calendar) -> List(#(EraCode, EraShift)) {
  case cal {
    Iso8601 | Chinese | Dangi -> []
    Gregory -> [#(Ce, Forward(0)), #(Bce, Backward(1))]
    Buddhist -> [#(Be, Forward(0))]
    Japanese -> [
      #(Reiwa, Forward(2018)),
      #(Heisei, Forward(1988)),
      #(Showa, Forward(1925)),
      #(Taisho, Forward(1911)),
      #(Meiji, Forward(1867)),
      #(Ce, Forward(0)),
      #(Bce, Backward(1)),
    ]
    Roc -> [#(Minguo, Forward(0)), #(BeforeMinguo, Backward(1))]
    Coptic -> [#(Am, Forward(0))]
    Ethiopic -> [#(Am, Forward(0)), #(Aa, Forward(-5500))]
    Ethioaa -> [#(Aa, Forward(0))]
    Hebrew -> [#(Am, Forward(0))]
    IslamicCivil | IslamicTbla | IslamicUmalqura -> [
      #(Ah, Forward(0)),
      #(Bh, Backward(1)),
    ]
    Persian -> [#(Ap, Forward(0))]
    Indian -> [#(Shaka, Forward(0))]
  }
}

/// Arithmetic year for era + eraYear. Error(Nil) when the calendar does not
/// use that era. Both arguments are closed sets, so there is nothing left to
/// reject with a wildcard: the answer is a lookup in `eras_of`.
pub fn year_for_era(cal: Calendar, era: EraCode, ey: Int) -> Result(Int, Nil) {
  use shift <- result.map(list.key_find(eras_of(cal), era))
  case shift {
    Forward(k) -> k + ey
    Backward(k) -> k - ey
  }
}

/// eraYear for an arithmetic year under a shift — the exact inverse of the
/// arithmetic `year_for_era` performs, so `era_for` never restates an offset.
fn era_year(shift: EraShift, year: Int) -> Int {
  case shift {
    Forward(k) -> year - k
    Backward(k) -> k - year
  }
}

// ============================================================================
// Chinese / Dangi lunisolar calendars (table-driven)
// ============================================================================
//
// Month structure is precomputed for the years whose new year falls in ISO
// 1700..2300 using the Calendrical Calculations astronomical algorithms
// (Dershowitz & Reingold) — the same algorithms ICU4X's chinese/dangi
// calendars are built on. Each packed entry holds, for one lunisolar year:
//   bits 0..12   month-length bitmap (bit m-1 set -> ordinal month m has 30
//                days; 29 otherwise)
//   bits 13..16  leap month number (0 = common year; e.g. 4 means the leap
//                month follows month 4, i.e. monthCode M04L at ordinal 5)
//   bits 17..22  offset of the lunisolar new year from January 1 of the ISO
//                year that shares its number
//
// The Temporal arithmetic year of a chinese/dangi date is the ISO year in
// which its new year falls. Outside the table range (no test262 coverage —
// astronomy is not predictable there) we fall back to a mean-motion metonic
// approximation that tiles exactly against both table edges, keeping all
// conversions total and monotonic.

// The per-calendar packed year table (`chinese_data` / `dangi_data`) is
// carried in the `LunisolarArith` arithmetic record as `data`.

/// Months elapsed before lunisolar year y in the metonic fallback (235
/// synodic months per 19 years).
fn lunisolar_eb(y: Int) -> Int {
  floor_div(235 * y - 234, 19)
}

/// Day position of the start of fallback month index t (mean synodic month
/// = 1447/49 days ~ 29.5306).
fn lunisolar_fb_pos(t: Int) -> Int {
  floor_div(1447 * t, 49)
}

/// First and last year covered by the packed `chinese_data`/`dangi_data`
/// tables. Only the mean-motion fallback needs them, to anchor itself against
/// the table edges — whether a year is *in* the table is decided by the table.
const lunisolar_first_year = 1700

const lunisolar_last_year = 2300

/// A lunisolar year is either inside the packed table — in which case its
/// three bit-fields are unpacked here, once — or outside it, where the
/// mean-motion metonic approximation takes over. The shifts and masks live
/// here and nowhere else.
type LunisolarYear {
  Tabulated(month_bits: Int, leap_month: Int, new_year_offset: Int)
  MeanMotion
}

fn lunisolar_year(data: fn(Int) -> Result(Int, Nil), y: Int) -> LunisolarYear {
  case data(y) {
    Ok(v) ->
      Tabulated(
        month_bits: int.bitwise_and(v, 0x1fff),
        leap_month: int.bitwise_and(int.bitwise_shift_right(v, 13), 15),
        new_year_offset: int.bitwise_shift_right(v, 17),
      )
    Error(Nil) -> MeanMotion
  }
}

fn lunisolar_leap_num(data: fn(Int) -> Result(Int, Nil), y: Int) -> Int {
  case lunisolar_year(data, y) {
    Tabulated(leap_month:, ..) -> leap_month
    MeanMotion ->
      // 13-month fallback years get their leap month after month 6.
      case lunisolar_eb(y + 1) - lunisolar_eb(y) == 13 {
        True -> 6
        False -> 0
      }
  }
}

fn count_bits(n: Int) -> Int {
  case n == 0 {
    True -> 0
    False -> int.bitwise_and(n, 1) + count_bits(int.bitwise_shift_right(n, 1))
  }
}

fn lunisolar_year_len(data: fn(Int) -> Result(Int, Nil), y: Int) -> Int {
  case lunisolar_year(data, y) {
    Tabulated(month_bits:, leap_month:, ..) -> {
      let months = case leap_month {
        0 -> 12
        _ -> 13
      }
      29 * months + count_bits(month_bits)
    }
    MeanMotion ->
      lunisolar_fb_pos(lunisolar_eb(y + 1)) - lunisolar_fb_pos(lunisolar_eb(y))
  }
}

/// Epoch days of the first day of lunisolar year y. The fallback regions are
/// anchored so they tile exactly against the table at both edges.
fn lunisolar_year_start(data: fn(Int) -> Result(Int, Nil), y: Int) -> Int {
  case lunisolar_year(data, y) {
    Tabulated(new_year_offset:, ..) ->
      days_from_civil(y, 1, 1) + new_year_offset
    MeanMotion -> {
      // Anchor: the table edge this fallback region abuts, and the month
      // count the mean-motion series must line up with there.
      let #(edge_days, edge_months) = case y < lunisolar_first_year {
        True -> #(
          lunisolar_year_start(data, lunisolar_first_year),
          lunisolar_eb(lunisolar_first_year),
        )
        False -> #(
          lunisolar_year_start(data, lunisolar_last_year)
            + lunisolar_year_len(data, lunisolar_last_year),
          lunisolar_eb(lunisolar_last_year + 1),
        )
      }
      edge_days
      - lunisolar_fb_pos(edge_months)
      + lunisolar_fb_pos(lunisolar_eb(y))
    }
  }
}

fn lunisolar_month_len(
  data: fn(Int) -> Result(Int, Nil),
  y: Int,
  m: Int,
) -> Int {
  case lunisolar_year(data, y) {
    Tabulated(month_bits:, ..) ->
      29 + int.bitwise_and(int.bitwise_shift_right(month_bits, m - 1), 1)
    MeanMotion -> {
      let t = lunisolar_eb(y) + m - 1
      lunisolar_fb_pos(t + 1) - lunisolar_fb_pos(t)
    }
  }
}

fn lunisolar_days_before_month(
  data: fn(Int) -> Result(Int, Nil),
  y: Int,
  m: Int,
) -> Int {
  case m <= 1 {
    True -> 0
    False ->
      lunisolar_days_before_month(data, y, m - 1)
      + lunisolar_month_len(data, y, m - 1)
  }
}

fn lunisolar_to_days(
  data: fn(Int) -> Result(Int, Nil),
  y: Int,
  m: Int,
  d: Int,
) -> Int {
  lunisolar_year_start(data, y)
  + lunisolar_days_before_month(data, y, m)
  + d
  - 1
}

fn lunisolar_from_days(
  data: fn(Int) -> Result(Int, Nil),
  date: Int,
) -> CalDate {
  let #(y0, _, _) = civil_from_days(date)
  let y = adjust_year(date, y0, fn(yy) { lunisolar_year_start(data, yy) })
  let max = case lunisolar_leap_num(data, y) {
    0 -> 12
    _ -> 13
  }
  let #(m, d) =
    scan_months(date, y, 1, max, fn(yy, mm) {
      lunisolar_to_days(data, yy, mm, 1)
    })
  CalDate(y, m, d)
}
