//// Non-ISO calendar arithmetic for Temporal (Intl era/monthCode semantics).
////
//// Supports the arithmetic (deterministic) calendars: gregory, buddhist,
//// japanese, roc, coptic, ethiopic, ethioaa, hebrew, islamic-civil
//// (alias islamicc), islamic-tbla, persian, indian. Observation-based
//// calendars (chinese, dangi, islamic-umalqura, islamic, islamic-rgsa)
//// are not supported.
////
//// All conversions are in "epoch days" — days since 1970-01-01 (matching
//// temporal.gleam's epoch_days). Algorithms follow Calendrical Calculations
//// (Reingold & Dershowitz) and ICU, which is what test262 expectations
//// are based on.

import gleam/int
import gleam/option.{type Option, None, Some}
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

/// Canonicalize a calendar identifier (case-insensitive, resolves aliases).
/// Error(Nil) for unsupported identifiers.
pub fn canonicalize(id: String) -> Result(String, Nil) {
  case string.lowercase(id) {
    "iso8601" -> Ok("iso8601")
    "gregory" | "gregorian" -> Ok("gregory")
    "buddhist" -> Ok("buddhist")
    "japanese" -> Ok("japanese")
    "roc" -> Ok("roc")
    "coptic" -> Ok("coptic")
    "ethiopic" -> Ok("ethiopic")
    "ethioaa" | "ethiopic-amete-alem" -> Ok("ethioaa")
    "hebrew" -> Ok("hebrew")
    "islamic-civil" | "islamicc" -> Ok("islamic-civil")
    "islamic-tbla" -> Ok("islamic-tbla")
    "islamic-umalqura" -> Ok("islamic-umalqura")
    "persian" -> Ok("persian")
    "indian" -> Ok("indian")
    "chinese" -> Ok("chinese")
    "dangi" -> Ok("dangi")
    _ -> Error(Nil)
  }
}

/// Calendars that are ISO-like (same months/days as ISO 8601, only year
/// numbering and eras differ).
fn iso_like_year_offset(cal: String) -> Option(Int) {
  // calendar year = iso year + offset
  case cal {
    "gregory" | "japanese" -> Some(0)
    "buddhist" -> Some(543)
    "roc" -> Some(-1911)
    _ -> None
  }
}

// ============================================================================
// Math helpers
// ============================================================================

fn floor_div(a: Int, b: Int) -> Int {
  let q = a / b
  case a % b != 0 && { a < 0 } != { b < 0 } {
    True -> q - 1
    False -> q
  }
}

fn floor_mod(a: Int, b: Int) -> Int {
  a - floor_div(a, b) * b
}

// ============================================================================
// Proleptic Gregorian (epoch days <-> y/m/d), Hinnant's algorithms
// ============================================================================

fn is_gregorian_leap(y: Int) -> Bool {
  floor_mod(y, 4) == 0 && { floor_mod(y, 100) != 0 || floor_mod(y, 400) == 0 }
}

fn gregorian_days_in_month(y: Int, m: Int) -> Int {
  case m {
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    4 | 6 | 9 | 11 -> 30
    _ ->
      case is_gregorian_leap(y) {
        True -> 29
        False -> 28
      }
  }
}

/// Days since 1970-01-01 for a proleptic Gregorian date.
fn days_from_civil(y: Int, m: Int, d: Int) -> Int {
  let y = case m <= 2 {
    True -> y - 1
    False -> y
  }
  let era = floor_div(y, 400)
  let yoe = y - era * 400
  let mp = floor_mod(m + 9, 12)
  let doy = { 153 * mp + 2 } / 5 + d - 1
  let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
  era * 146_097 + doe - 719_468
}

/// Proleptic Gregorian date from days since 1970-01-01.
fn civil_from_days(z: Int) -> #(Int, Int, Int) {
  let z = z + 719_468
  let era = floor_div(z, 146_097)
  let doe = z - era * 146_097
  let yoe = { doe - doe / 1460 + doe / 36_524 - doe / 146_096 } / 365
  let y = yoe + era * 400
  let doy = doe - { 365 * yoe + yoe / 4 - yoe / 100 }
  let mp = { 5 * doy + 2 } / 153
  let d = doy - { 153 * mp + 2 } / 5 + 1
  let m = case mp < 10 {
    True -> mp + 3
    False -> mp - 9
  }
  case m <= 2 {
    True -> #(y + 1, m, d)
    False -> #(y, m, d)
  }
}

// ============================================================================
// Coptic / Ethiopic family
// ============================================================================

// Rata Die epochs converted to days-since-1970 (RD - 719163).
const coptic_epoch = -615_558

const ethiopic_epoch = -716_367

fn coptic_family_epoch(cal: String) -> Int {
  case cal {
    "coptic" -> coptic_epoch
    _ -> ethiopic_epoch
  }
}

/// ethioaa year = ethiopic year + 5500 (amete alem vs amete mihret).
fn coptic_family_year_shift(cal: String) -> Int {
  case cal {
    "ethioaa" -> 5500
    _ -> 0
  }
}

fn coptic_to_days(cal: String, year: Int, month: Int, day: Int) -> Int {
  let y = year - coptic_family_year_shift(cal)
  coptic_family_epoch(cal)
  - 1
  + 365
  * { y - 1 }
  + floor_div(y, 4)
  + 30
  * { month - 1 }
  + day
}

fn coptic_from_days(cal: String, date: Int) -> CalDate {
  let epoch = coptic_family_epoch(cal)
  let y = floor_div(4 * { date - epoch } + 1463, 1461)
  let shift = coptic_family_year_shift(cal)
  let m = floor_div(date - coptic_to_days(cal, y + shift, 1, 1), 30) + 1
  let d = date - coptic_to_days(cal, y + shift, m, 1) + 1
  CalDate(y + shift, m, d)
}

fn coptic_is_leap(cal: String, year: Int) -> Bool {
  floor_mod(year - coptic_family_year_shift(cal), 4) == 3
}

fn coptic_days_in_month(cal: String, year: Int, month: Int) -> Int {
  case month {
    13 ->
      case coptic_is_leap(cal, year) {
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

fn islamic_epoch(cal: String) -> Int {
  case cal {
    "islamic-tbla" -> islamic_tbla_epoch
    _ -> islamic_civil_epoch
  }
}

fn islamic_is_leap(year: Int) -> Bool {
  floor_mod(14 + 11 * year, 30) < 11
}

fn islamic_to_days(cal: String, year: Int, month: Int, day: Int) -> Int {
  islamic_epoch(cal)
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

fn islamic_from_days(cal: String, date: Int) -> CalDate {
  let y0 = floor_div(30 * { date - islamic_epoch(cal) } + 10_646, 10_631)
  let y = adjust_year(date, y0, fn(yy) { islamic_to_days(cal, yy, 1, 1) })
  let #(m, d) =
    scan_months(date, y, 1, 12, fn(yy, mm) { islamic_to_days(cal, yy, mm, 1) })
  CalDate(y, m, d)
}

// ============================================================================
// Islamic Umm al-Qura (ICU table for AH 1300–1600, civil fallback outside)
// ============================================================================

/// Packed month lengths for AH 1300..1600 (bit 11-(month-1) set → 30 days),
/// from ICU4C islamcal.cpp UMALQURA_MONTHLENGTH.
const umalqura_month_length = [
  2730, 3412, 3785, 1748, 1770, 876, 2733, 1365, 1705, 1938, 2985, 1492, 2778,
  1372, 3373, 1685, 1866, 2900, 2922, 1453, 1198, 2639, 1303, 1675, 1701, 2773,
  726, 2395, 1181, 2637, 3366, 3477, 1452, 2486, 698, 2651, 1323, 2709, 1738,
  2793, 756, 2422, 694, 2390, 2762, 2980, 3026, 1497, 732, 2413, 1357, 2725,
  2898, 2981, 1460, 2486, 1367, 663, 1355, 1699, 1874, 2917, 1386, 2731, 1323,
  3221, 3402, 3493, 1482, 2774, 2391, 1195, 2379, 2725, 2898, 2922, 1397, 630,
  2231, 1115, 1365, 1449, 1460, 2522, 1245, 622, 2358, 2730, 3412, 3506, 1493,
  730, 2395, 1195, 2645, 2889, 2916, 2929, 1460, 2741, 2645, 3365, 3730, 3785,
  1748, 2793, 2411, 1195, 2707, 3401, 3492, 3506, 2745, 1210, 2651, 1323, 2709,
  2858, 2901, 1372, 1213, 573, 2333, 2709, 2890, 2906, 1389, 694, 2363, 1179,
  1621, 1705, 1876, 2922, 1388, 2733, 1365, 2857, 2962, 2985, 1492, 2778, 1370,
  2731, 1429, 1865, 1892, 2986, 1461, 694, 2646, 3661, 2853, 2898, 2922, 1453,
  686, 2351, 1175, 1611, 1701, 1708, 2774, 1373, 1181, 2637, 3350, 3477, 1450,
  1461, 730, 2395, 1197, 1429, 1738, 1764, 2794, 1269, 694, 2390, 2730, 2900,
  3026, 1497, 746, 2413, 1197, 2709, 2890, 2981, 1458, 2485, 1238, 2711, 1351,
  1683, 1865, 2901, 1386, 2667, 1323, 2699, 3398, 3491, 1482, 2774, 1243, 619,
  2379, 2725, 2898, 2921, 1397, 374, 2231, 603, 1323, 1381, 1460, 2522, 1261,
  365, 2230, 2726, 3410, 3497, 1492, 2778, 2395, 1195, 1619, 1833, 1890, 2985,
  1458, 2741, 1365, 2853, 3474, 3785, 1746, 2793, 1387, 1195, 2645, 3369, 3412,
  3498, 2485, 1210, 2619, 1179, 2637, 2730, 2773, 730, 2397, 1118, 2606, 3226,
  3413, 1714, 1721, 1210, 2653, 1325, 2709, 2898, 2984, 2996, 1465, 730, 2394,
  2890, 3492, 3793, 1768, 2922, 1389, 1333, 1685, 3402, 3496, 3540, 1754, 1371,
  669, 1579, 2837, 2890, 2965, 1450, 2734, 2350, 3215, 1319, 1685, 1706, 2774,
  1373, 669,
]

/// Corrections to the linear year-start estimate for AH 1300..1600, from
/// ICU4C islamcal.cpp umAlQuraYrStartEstimateFix.
const umalqura_year_start_fix = [
  0, 0, -1, 0, -1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, -1, 0, 1, 0, 1, 1, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, -1, -1, 0, 0, 0, 1,
  0, 0, -1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 1, 1, 0, 0, -1,
  0, 1, 0, 1, 1, 0, 0, -1, 0, 1, 0, 0, 0, -1, 0, 1, 0, 1, 0, 0, 0, -1, 0, 0, 0,
  0, -1, -1, 0, -1, 0, 1, 0, 0, 0, -1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, -1,
  -1, 0, 0, 0, 1, 0, 0, -1, -1, 0, -1, 0, 0, -1, -1, 0, -1, 0, -1, 0, 0, -1, -1,
  0, 0, 0, 0, 0, 0, -1, 0, 1, 0, 1, 1, 0, 0, -1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0,
  0, 0, -1, 0, 1, 0, 0, -1, -1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 1, 0, 0, -1, 0, 0, 0, 1, 1, 0, 0, -1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0,
  -1, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, -1, 0, -1, 0, 1, 0, 0, 0, -1, 0, 1,
  0, 1, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, -1,
  -1, 0, -1, 0, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  1,
]

fn list_at(l: List(Int), i: Int) -> Int {
  case l {
    [x, ..rest] ->
      case i <= 0 {
        True -> x
        False -> list_at(rest, i - 1)
      }
    [] -> 0
  }
}

/// Day count of the start of an AH year, origin 0 at civil AH 1-01-01.
fn umalqura_year_start(year: Int) -> Int {
  case year >= 1300 && year <= 1600 {
    True -> {
      let dy = year - 1300
      floor_div(35_436_720 * dy + 46_032_255_000, 100_000)
      + list_at(umalqura_year_start_fix, dy)
    }
    False -> 354 * { year - 1 } + floor_div(3 + 11 * year, 30)
  }
}

fn umalqura_days_in_month(year: Int, month: Int) -> Int {
  case year >= 1300 && year <= 1600 {
    True ->
      29
      + int.bitwise_and(
        int.bitwise_shift_right(
          list_at(umalqura_month_length, year - 1300),
          12 - month,
        ),
        1,
      )
    False -> islamic_days_in_month(year, month)
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

fn hebrew_year_length_correction(year: Int) -> Int {
  let ny0 = hebrew_elapsed_days(year - 1)
  let ny1 = hebrew_elapsed_days(year)
  let ny2 = hebrew_elapsed_days(year + 1)
  case ny2 - ny1 == 356 {
    True -> 2
    False ->
      case ny1 - ny0 == 382 {
        True -> 1
        False -> 0
      }
  }
}

/// Epoch days of Tishri 1 in the given hebrew year.
fn hebrew_new_year(year: Int) -> Int {
  hebrew_epoch + hebrew_elapsed_days(year) + hebrew_year_length_correction(year)
}

fn hebrew_year_length(year: Int) -> Int {
  hebrew_new_year(year + 1) - hebrew_new_year(year)
}

/// Days in ordinal month (civil order: 1 = Tishri).
fn hebrew_days_in_month(year: Int, month: Int) -> Int {
  let leap = hebrew_is_leap(year)
  let ylen = hebrew_year_length(year)
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
  hebrew_new_year(year) + hebrew_days_before_month(year, month) + day - 1
}

fn hebrew_days_before_month(year: Int, month: Int) -> Int {
  sum_months(year, 1, month, 0)
}

fn sum_months(year: Int, m: Int, until: Int, acc: Int) -> Int {
  case m < until {
    True -> sum_months(year, m + 1, until, acc + hebrew_days_in_month(year, m))
    False -> acc
  }
}

fn hebrew_from_days(date: Int) -> CalDate {
  // Approximate year, then adjust.
  let approx = floor_div(98_496 * { date - hebrew_epoch }, 35_975_351) + 1
  let y = adjust_year(date, approx, hebrew_new_year)
  let months = hebrew_months_in_year(y)
  let #(m, d) =
    scan_months(date, y, 1, months, fn(yy, mm) { hebrew_to_days(yy, mm, 1) })
  CalDate(y, m, d)
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
pub fn date_from_epoch_days(cal: String, days: Int) -> CalDate {
  case iso_like_year_offset(cal) {
    Some(offset) -> {
      let #(y, m, d) = civil_from_days(days)
      CalDate(y + offset, m, d)
    }
    None ->
      case cal {
        "coptic" | "ethiopic" | "ethioaa" -> coptic_from_days(cal, days)
        "islamic-civil" | "islamic-tbla" -> islamic_from_days(cal, days)
        "islamic-umalqura" -> umalqura_from_days(days)
        "persian" -> persian_from_days(days)
        "indian" -> indian_from_days(days)
        "hebrew" -> hebrew_from_days(days)
        "chinese" | "dangi" -> lunisolar_from_days(cal, days)
        _ -> {
          // iso8601
          let #(y, m, d) = civil_from_days(days)
          CalDate(y, m, d)
        }
      }
  }
}

/// Epoch days for a (valid) calendar date.
pub fn date_to_epoch_days(cal: String, year: Int, month: Int, day: Int) -> Int {
  case iso_like_year_offset(cal) {
    Some(offset) -> days_from_civil(year - offset, month, day)
    None ->
      case cal {
        "coptic" | "ethiopic" | "ethioaa" ->
          coptic_to_days(cal, year, month, day)
        "islamic-civil" | "islamic-tbla" ->
          islamic_to_days(cal, year, month, day)
        "islamic-umalqura" -> umalqura_to_days(year, month, day)
        "persian" -> persian_to_days(year, month, day)
        "indian" -> indian_to_days(year, month, day)
        "hebrew" -> hebrew_to_days(year, month, day)
        "chinese" | "dangi" -> lunisolar_to_days(cal, year, month, day)
        _ -> days_from_civil(year, month, day)
      }
  }
}

pub fn months_in_year(cal: String, year: Int) -> Int {
  case cal {
    "coptic" | "ethiopic" | "ethioaa" -> 13
    "hebrew" -> hebrew_months_in_year(year)
    "chinese" | "dangi" ->
      case lunisolar_leap_num(cal, year) == 0 {
        True -> 12
        False -> 13
      }
    _ -> 12
  }
}

pub fn days_in_month(cal: String, year: Int, month: Int) -> Int {
  case iso_like_year_offset(cal) {
    Some(offset) -> gregorian_days_in_month(year - offset, month)
    None ->
      case cal {
        "coptic" | "ethiopic" | "ethioaa" ->
          coptic_days_in_month(cal, year, month)
        "islamic-civil" | "islamic-tbla" -> islamic_days_in_month(year, month)
        "islamic-umalqura" -> umalqura_days_in_month(year, month)
        "persian" -> persian_days_in_month(year, month)
        "indian" -> indian_days_in_month(year, month)
        "hebrew" -> hebrew_days_in_month(year, month)
        "chinese" | "dangi" -> lunisolar_month_len(cal, year, month)
        _ -> gregorian_days_in_month(year, month)
      }
  }
}

pub fn days_in_year(cal: String, year: Int) -> Int {
  case cal {
    "hebrew" -> hebrew_year_length(year)
    _ ->
      date_to_epoch_days(cal, year + 1, 1, 1)
      - date_to_epoch_days(cal, year, 1, 1)
  }
}

pub fn in_leap_year(cal: String, year: Int) -> Bool {
  case iso_like_year_offset(cal) {
    Some(offset) -> is_gregorian_leap(year - offset)
    None ->
      case cal {
        "coptic" | "ethiopic" | "ethioaa" -> coptic_is_leap(cal, year)
        "islamic-civil" | "islamic-tbla" -> islamic_is_leap(year)
        "islamic-umalqura" ->
          umalqura_to_days(year + 1, 1, 1) - umalqura_to_days(year, 1, 1) > 354
        "persian" -> persian_is_leap(year)
        "indian" -> is_gregorian_leap(year + 78)
        "hebrew" -> hebrew_is_leap(year)
        "chinese" | "dangi" -> lunisolar_leap_num(cal, year) != 0
        _ -> is_gregorian_leap(year)
      }
  }
}

/// 1-based day of year for a calendar date.
pub fn day_of_year(cal: String, year: Int, month: Int, day: Int) -> Int {
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

/// Month code string for an ordinal month in a year.
pub fn month_code(cal: String, year: Int, month: Int) -> String {
  case cal {
    "hebrew" ->
      case hebrew_is_leap(year) {
        True ->
          case month == 6 {
            True -> "M05L"
            False ->
              case month > 6 {
                True -> "M" <> pad2(month - 1)
                False -> "M" <> pad2(month)
              }
          }
        False -> "M" <> pad2(month)
      }
    "chinese" | "dangi" -> {
      let leap = lunisolar_leap_num(cal, year)
      case leap > 0 && month == leap + 1 {
        True -> "M" <> pad2(leap) <> "L"
        False ->
          case leap > 0 && month > leap + 1 {
            True -> "M" <> pad2(month - 1)
            False -> "M" <> pad2(month)
          }
      }
    }
    _ -> "M" <> pad2(month)
  }
}

/// Parse a month code string into #(number, is_leap). Error(Nil) on syntax.
pub fn parse_month_code(s: String) -> Result(#(Int, Bool), Nil) {
  case s {
    "M" <> rest -> {
      let #(digits, leap) = case string.ends_with(rest, "L") {
        True -> #(string.drop_end(rest, 1), True)
        False -> #(rest, False)
      }
      case string.length(digits) == 2, int.parse(digits) {
        True, Ok(n) ->
          case n >= 0 && n <= 13 && { n >= 1 || leap } {
            True -> Ok(#(n, leap))
            False -> Error(Nil)
          }
        _, _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Resolve a month code to an ordinal month within `year`.
pub fn month_for_code(
  cal: String,
  year: Int,
  num: Int,
  leap: Bool,
) -> Result(Int, MonthCodeIssue) {
  case cal, leap {
    "hebrew", True ->
      case num == 5 {
        True ->
          case hebrew_is_leap(year) {
            True -> Ok(6)
            // Skip-forward: constrain M05L to M06 (Adar II = ordinal 6).
            False -> Error(NotInThisYear(6))
          }
        False -> Error(NeverValid)
      }
    "chinese", True | "dangi", True ->
      case num >= 1 && num <= 12 {
        False -> Error(NeverValid)
        True -> {
          let leap = lunisolar_leap_num(cal, year)
          case leap == num {
            True -> Ok(num + 1)
            // Constrain MxxL to the regular month Mxx of this year.
            False ->
              Error(
                NotInThisYear(case leap > 0 && num > leap {
                  True -> num + 1
                  False -> num
                }),
              )
          }
        }
      }
    _, True -> Error(NeverValid)
    "hebrew", False ->
      case num >= 1 && num <= 12 {
        True ->
          case hebrew_is_leap(year) && num >= 6 {
            True -> Ok(num + 1)
            False -> Ok(num)
          }
        False -> Error(NeverValid)
      }
    "coptic", False | "ethiopic", False | "ethioaa", False ->
      case num >= 1 && num <= 13 {
        True -> Ok(num)
        False -> Error(NeverValid)
      }
    "chinese", False | "dangi", False ->
      case num >= 1 && num <= 12 {
        True -> {
          let leap = lunisolar_leap_num(cal, year)
          case leap > 0 && num > leap {
            True -> Ok(num + 1)
            False -> Ok(num)
          }
        }
        False -> Error(NeverValid)
      }
    _, False ->
      case num >= 1 && num <= 12 {
        True -> Ok(num)
        False -> Error(NeverValid)
      }
  }
}

// ============================================================================
// Eras
// ============================================================================

/// True when the calendar's dates carry era/eraYear. The chinese/dangi
/// calendars have no eras (era/eraYear fields are ignored entirely).
pub fn has_eras(cal: String) -> Bool {
  cal != "iso8601" && cal != "chinese" && cal != "dangi"
}

/// era + eraYear for a calendar date.
pub fn era_for(
  cal: String,
  year: Int,
  month: Int,
  day: Int,
) -> #(Option(String), Option(Int)) {
  case cal {
    "iso8601" -> #(None, None)
    "gregory" ->
      case year >= 1 {
        True -> #(Some("ce"), Some(year))
        False -> #(Some("bce"), Some(1 - year))
      }
    "buddhist" -> #(Some("be"), Some(year))
    "japanese" -> japanese_era(year, month, day)
    "roc" ->
      case year >= 1 {
        True -> #(Some("roc"), Some(year))
        False -> #(Some("broc"), Some(1 - year))
      }
    "coptic" -> #(Some("am"), Some(year))
    "ethiopic" ->
      case year >= 1 {
        True -> #(Some("am"), Some(year))
        False -> #(Some("aa"), Some(year + 5500))
      }
    "ethioaa" -> #(Some("aa"), Some(year))
    "hebrew" -> #(Some("am"), Some(year))
    "islamic-civil" | "islamic-tbla" | "islamic-umalqura" ->
      case year >= 1 {
        True -> #(Some("ah"), Some(year))
        False -> #(Some("bh"), Some(1 - year))
      }
    "persian" -> #(Some("ap"), Some(year))
    "indian" -> #(Some("shaka"), Some(year))
    _ -> #(None, None)
  }
}

/// Japanese era for an ISO date (japanese arithmetic year == ISO year).
/// Modern named eras start: meiji 6 = 1873-01-01 (output cutoff), taisho
/// 1912-07-30, showa 1926-12-25, heisei 1989-01-08, reiwa 2019-05-01.
fn japanese_era(y: Int, m: Int, d: Int) -> #(Option(String), Option(Int)) {
  let after = fn(ey: Int, em: Int, ed: Int) {
    y > ey || { y == ey && { m > em || { m == em && d >= ed } } }
  }
  case after(2019, 5, 1) {
    True -> #(Some("reiwa"), Some(y - 2018))
    False ->
      case after(1989, 1, 8) {
        True -> #(Some("heisei"), Some(y - 1988))
        False ->
          case after(1926, 12, 25) {
            True -> #(Some("showa"), Some(y - 1925))
            False ->
              case after(1912, 7, 30) {
                True -> #(Some("taisho"), Some(y - 1911))
                False ->
                  case after(1873, 1, 1) {
                    True -> #(Some("meiji"), Some(y - 1867))
                    False ->
                      case y >= 1 {
                        True -> #(Some("ce"), Some(y))
                        False -> #(Some("bce"), Some(1 - y))
                      }
                  }
              }
          }
      }
  }
}

/// Arithmetic year for era + eraYear. Error(Nil) when the era code is not
/// valid for the calendar.
pub fn year_for_era(cal: String, era: String, ey: Int) -> Result(Int, Nil) {
  case cal, era {
    "gregory", "ce" | "gregory", "ad" -> Ok(ey)
    "gregory", "bce" | "gregory", "bc" -> Ok(1 - ey)
    "buddhist", "be" -> Ok(ey)
    "japanese", "reiwa" -> Ok(2018 + ey)
    "japanese", "heisei" -> Ok(1988 + ey)
    "japanese", "showa" -> Ok(1925 + ey)
    "japanese", "taisho" -> Ok(1911 + ey)
    "japanese", "meiji" -> Ok(1867 + ey)
    "japanese", "ce" | "japanese", "ad" -> Ok(ey)
    "japanese", "bce" | "japanese", "bc" -> Ok(1 - ey)
    "roc", "roc" -> Ok(ey)
    "roc", "broc" -> Ok(1 - ey)
    "coptic", "am" -> Ok(ey)
    "ethiopic", "am" -> Ok(ey)
    "ethiopic", "aa" -> Ok(ey - 5500)
    "ethioaa", "aa" -> Ok(ey)
    "hebrew", "am" -> Ok(ey)
    "islamic-civil", "ah" | "islamic-tbla", "ah" | "islamic-umalqura", "ah" ->
      Ok(ey)
    "islamic-civil", "bh" | "islamic-tbla", "bh" | "islamic-umalqura", "bh" ->
      Ok(1 - ey)
    "persian", "ap" -> Ok(ey)
    "indian", "shaka" -> Ok(ey)
    _, _ -> Error(Nil)
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

fn lunisolar_data(cal: String, y: Int) -> Int {
  case cal {
    "dangi" -> dangi_data(y)
    _ -> chinese_data(y)
  }
}

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

fn lunisolar_leap_num(cal: String, y: Int) -> Int {
  case y >= 1700 && y <= 2300 {
    True ->
      int.bitwise_and(int.bitwise_shift_right(lunisolar_data(cal, y), 13), 15)
    False ->
      // 13-month fallback years get their leap month after month 6.
      case lunisolar_eb(y + 1) - lunisolar_eb(y) == 13 {
        True -> 6
        False -> 0
      }
  }
}

fn lunisolar_table_start(cal: String, y: Int) -> Int {
  days_from_civil(y, 1, 1) + int.bitwise_shift_right(lunisolar_data(cal, y), 17)
}

fn count_bits(n: Int) -> Int {
  case n == 0 {
    True -> 0
    False -> int.bitwise_and(n, 1) + count_bits(int.bitwise_shift_right(n, 1))
  }
}

fn lunisolar_table_year_len(cal: String, y: Int) -> Int {
  let v = lunisolar_data(cal, y)
  let months = case int.bitwise_and(int.bitwise_shift_right(v, 13), 15) {
    0 -> 12
    _ -> 13
  }
  29 * months + count_bits(int.bitwise_and(v, 0x1fff))
}

/// Epoch days of the first day of lunisolar year y. The fallback regions are
/// anchored so they tile exactly against the table at both edges.
fn lunisolar_year_start(cal: String, y: Int) -> Int {
  case y < 1700 {
    True ->
      lunisolar_table_start(cal, 1700)
      - lunisolar_fb_pos(lunisolar_eb(1700))
      + lunisolar_fb_pos(lunisolar_eb(y))
    False ->
      case y > 2300 {
        True ->
          lunisolar_table_start(cal, 2300)
          + lunisolar_table_year_len(cal, 2300)
          - lunisolar_fb_pos(lunisolar_eb(2301))
          + lunisolar_fb_pos(lunisolar_eb(y))
        False -> lunisolar_table_start(cal, y)
      }
  }
}

fn lunisolar_month_len(cal: String, y: Int, m: Int) -> Int {
  case y >= 1700 && y <= 2300 {
    True ->
      29
      + int.bitwise_and(
        int.bitwise_shift_right(lunisolar_data(cal, y), m - 1),
        1,
      )
    False -> {
      let t = lunisolar_eb(y) + m - 1
      lunisolar_fb_pos(t + 1) - lunisolar_fb_pos(t)
    }
  }
}

fn lunisolar_days_before_month(cal: String, y: Int, m: Int) -> Int {
  case m <= 1 {
    True -> 0
    False ->
      lunisolar_days_before_month(cal, y, m - 1)
      + lunisolar_month_len(cal, y, m - 1)
  }
}

fn lunisolar_to_days(cal: String, y: Int, m: Int, d: Int) -> Int {
  lunisolar_year_start(cal, y) + lunisolar_days_before_month(cal, y, m) + d - 1
}

fn lunisolar_from_days(cal: String, date: Int) -> CalDate {
  let #(y0, _, _) = civil_from_days(date)
  let y = adjust_year(date, y0, fn(yy) { lunisolar_year_start(cal, yy) })
  let max = case lunisolar_leap_num(cal, y) {
    0 -> 12
    _ -> 13
  }
  let #(m, d) =
    scan_months(date, y, 1, max, fn(yy, mm) {
      lunisolar_to_days(cal, yy, mm, 1)
    })
  CalDate(y, m, d)
}

fn chinese_data(y: Int) -> Int {
  case y {
    1700 -> 6_425_893
    1701 -> 4_983_381
    1702 -> 3_593_389
    1703 -> 6_030_518
    1704 -> 4_589_237
    1705 -> 3_181_994
    1706 -> 5_639_881
    1707 -> 4_329_106
    1708 -> 2_915_621
    1709 -> 5_246_246
    1710 -> 3_861_078
    1711 -> 6_163_035
    1712 -> 4_851_034
    1713 -> 3_320_533
    1714 -> 5_769_045
    1715 -> 4_458_314
    1716 -> 3_042_963
    1717 -> 5_375_635
    1718 -> 4_003_115
    1719 -> 6_423_851
    1720 -> 4_983_467
    1721 -> 3_593_562
    1722 -> 6_030_698
    1723 -> 4_590_437
    1724 -> 3_315_530
    1725 -> 5_638_986
    1726 -> 4_197_013
    1727 -> 2_774_315
    1728 -> 5_244_237
    1729 -> 3_730_093
    1730 -> 6_163_125
    1731 -> 4_851_122
    1732 -> 3_451_813
    1733 -> 5_770_661
    1734 -> 4_459_850
    1735 -> 3_054_869
    1736 -> 5_508_246
    1737 -> 4_012_374
    1738 -> 6_423_894
    1739 -> 4_983_509
    1740 -> 3_724_722
    1741 -> 6_031_058
    1742 -> 4_591_269
    1743 -> 3_313_290
    1744 -> 5_637_771
    1745 -> 4_066_455
    1746 -> 2_779_478
    1747 -> 5_113_179
    1748 -> 3_861_210
    1749 -> 6_163_306
    1750 -> 4_851_538
    1751 -> 3_454_757
    1752 -> 5_901_125
    1753 -> 4_328_075
    1754 -> 2_921_771
    1755 -> 5_375_149
    1756 -> 4_008_299
    1757 -> 6_292_917
    1758 -> 4_983_722
    1759 -> 3_726_164
    1760 -> 6_163_874
    1761 -> 4_590_917
    1762 -> 3_193_485
    1763 -> 5_638_805
    1764 -> 4_195_501
    1765 -> 2_640_301
    1766 -> 5_113_525
    1767 -> 3_861_930
    1768 -> 6_295_242
    1769 -> 4_853_410
    1770 -> 3_456_326
    1771 -> 5_901_642
    1772 -> 4_459_158
    1773 -> 2_913_590
    1774 -> 5_375_322
    1775 -> 4_016_853
    1776 -> 6_425_445
    1777 -> 4_982_610
    1778 -> 3_591_845
    1779 -> 6_031_013
    1780 -> 4_588_875
    1781 -> 3_058_327
    1782 -> 5_507_755
    1783 -> 4_195_674
    1784 -> 2_779_861
    1785 -> 5_114_725
    1786 -> 3_864_402
    1787 -> 6_294_866
    1788 -> 4_852_501
    1789 -> 3_323_467
    1790 -> 5_768_525
    1791 -> 4_328_109
    1792 -> 3_052_906
    1793 -> 5_375_410
    1794 -> 3_935_657
    1795 -> 2_645_330
    1796 -> 5_115_282
    1797 -> 3_595_541
    1798 -> 6_032_678
    1799 -> 4_589_910
    1800 -> 3_181_229
    1801 -> 5_638_870
    1802 -> 4_326_868
    1803 -> 2_903_465
    1804 -> 5_377_737
    1805 -> 3_993_226
    1806 -> 6_293_131
    1807 -> 4_853_031
    1808 -> 3_582_294
    1809 -> 5_769_563
    1810 -> 4_459_226
    1811 -> 3_176_148
    1812 -> 5_637_972
    1813 -> 4_065_097
    1814 -> 2_643_595
    1815 -> 5_114_515
    1816 -> 3_724_587
    1817 -> 6_030_637
    1818 -> 4_589_933
    1819 -> 3_312_490
    1820 -> 5_770_666
    1821 -> 4_328_356
    1822 -> 2_915_141
    1823 -> 5_377_353
    1824 -> 3_996_309
    1825 -> 6_294_165
    1826 -> 4_850_989
    1827 -> 3_451_565
    1828 -> 5_900_981
    1829 -> 4_459_946
    1830 -> 3_186_084
    1831 -> 5_639_844
    1832 -> 4_275_530
    1833 -> 6_557_002
    1834 -> 5_114_518
    1835 -> 3_724_598
    1836 -> 6_161_754
    1837 -> 4_590_293
    1838 -> 3_315_402
    1839 -> 5_769_042
    1840 -> 4_329_125
    1841 -> 2_911_562
    1842 -> 5_244_491
    1843 -> 3_861_143
    1844 -> 6_294_187
    1845 -> 4_851_034
    1846 -> 3_451_733
    1847 -> 5_901_225
    1848 -> 4_589_394
    1849 -> 3_054_373
    1850 -> 5_507_877
    1851 -> 4_135_499
    1852 -> 6_555_981
    1853 -> 4_983_469
    1854 -> 3_732_842
    1855 -> 6_161_844
    1856 -> 4_722_089
    1857 -> 3_325_266
    1858 -> 5_770_642
    1859 -> 4_328_741
    1860 -> 2_914_893
    1861 -> 5_245_526
    1862 -> 3_871_413
    1863 -> 6_294_230
    1864 -> 4_982_484
    1865 -> 3_452_329
    1866 -> 5_902_025
    1867 -> 4_591_250
    1868 -> 3_181_862
    1869 -> 5_375_275
    1870 -> 4_016_727
    1871 -> 6_424_923
    1872 -> 5_114_714
    1873 -> 3_725_012
    1874 -> 6_162_260
    1875 -> 4_720_457
    1876 -> 3_323_539
    1877 -> 5_638_803
    1878 -> 4_195_627
    1879 -> 2_779_739
    1880 -> 5_245_549
    1881 -> 3_861_354
    1882 -> 6_294_954
    1883 -> 4_983_716
    1884 -> 3_586_889
    1885 -> 5_901_641
    1886 -> 4_459_157
    1887 -> 3_052_845
    1888 -> 5_506_349
    1889 -> 3_934_893
    1890 -> 2_643_306
    1891 -> 5_115_306
    1892 -> 3_857_828
    1893 -> 6_164_132
    1894 -> 4_721_994
    1895 -> 3_320_469
    1896 -> 5_638_807
    1897 -> 4_195_670
    1898 -> 2_779_829
    1899 -> 5_245_653
    1900 -> 4_003_538
    1901 -> 6_424_402
    1902 -> 4_984_485
    1903 -> 3_716_682
    1904 -> 6_030_923
    1905 -> 4_459_163
    1906 -> 3_183_962
    1907 -> 5_637_482
    1908 -> 4_197_209
    1909 -> 2_774_866
    1910 -> 5_244_754
    1911 -> 3_857_189
    1912 -> 6_294_309
    1913 -> 4_721_227
    1914 -> 3_323_051
    1915 -> 5_767_853
    1916 -> 4_326_763
    1917 -> 2_902_889
    1918 -> 5_377_449
    1919 -> 4_128_146
    1920 -> 6_557_330
    1921 -> 4_984_101
    1922 -> 3_586_637
    1923 -> 6_031_958
    1924 -> 4_588_214
    1925 -> 3_052_981
    1926 -> 5_637_844
    1927 -> 4_198_057
    1928 -> 2_907_794
    1929 -> 5_246_610
    1930 -> 3_853_606
    1931 -> 6_161_707
    1932 -> 4_721_239
    1933 -> 3_322_550
    1934 -> 5_770_074
    1935 -> 4_458_196
    1936 -> 3_043_017
    1937 -> 5_375_817
    1938 -> 3_995_283
    1939 -> 6_425_235
    1940 -> 4_982_059
    1941 -> 3_459_675
    1942 -> 5_900_973
    1943 -> 4_588_906
    1944 -> 3_185_493
    1945 -> 5_639_076
    1946 -> 4_197_193
    1947 -> 2_775_699
    1948 -> 5_245_589
    1949 -> 3_732_781
    1950 -> 6_161_718
    1951 -> 4_721_325
    1952 -> 3_454_378
    1953 -> 5_768_626
    1954 -> 4_328_869
    1955 -> 3_046_730
    1956 -> 5_508_426
    1957 -> 4_000_405
    1958 -> 6_294_167
    1959 -> 4_982_102
    1960 -> 3_590_837
    1961 -> 5_901_013
    1962 -> 4_589_266
    1963 -> 3_182_245
    1964 -> 5_639_845
    1965 -> 4_195_914
    1966 -> 2_649_239
    1967 -> 5_114_523
    1968 -> 3_863_898
    1969 -> 6_161_770
    1970 -> 4_721_513
    1971 -> 3_454_802
    1972 -> 5_901_138
    1973 -> 4_328_229
    1974 -> 2_922_059
    1975 -> 5_376_587
    1976 -> 4_002_987
    1977 -> 6_292_141
    1978 -> 4_851_053
    1979 -> 3_591_017
    1980 -> 6_032_809
    1981 -> 4_590_994
    1982 -> 3_185_957
    1983 -> 5_639_461
    1984 -> 4_282_957
    1985 -> 6_556_246
    1986 -> 5_112_502
    1987 -> 3_720_629
    1988 -> 6_162_133
    1989 -> 4_722_345
    1990 -> 3_456_658
    1991 -> 5_901_970
    1992 -> 4_459_814
    1993 -> 2_910_806
    1994 -> 5_245_527
    1995 -> 4_003_030
    1996 -> 6_423_386
    1997 -> 4_851_413
    1998 -> 3_585_737
    1999 -> 6_031_177
    2000 -> 4_589_203
    2001 -> 3_052_843
    2002 -> 5_506_347
    2003 -> 4_065_883
    2004 -> 2_774_362
    2005 -> 5_113_194
    2006 -> 3_734_357
    2007 -> 6_294_436
    2008 -> 4_852_553
    2009 -> 3_324_563
    2010 -> 5_769_877
    2011 -> 4_326_701
    2012 -> 2_919_085
    2013 -> 5_245_621
    2014 -> 4_011_434
    2015 -> 6_424_018
    2016 -> 4_984_229
    2017 -> 3_595_594
    2018 -> 6_032_714
    2019 -> 4_590_741
    2020 -> 3_183_918
    2021 -> 5_506_390
    2022 -> 4_065_973
    2023 -> 2_774_450
    2024 -> 5_244_626
    2025 -> 3_722_917
    2026 -> 6_162_213
    2027 -> 4_720_203
    2028 -> 3_320_983
    2029 -> 5_639_339
    2030 -> 4_326_746
    2031 -> 2_910_934
    2032 -> 5_376_873
    2033 -> 4_028_242
    2034 -> 6_425_426
    2035 -> 4_983_589
    2036 -> 3_594_827
    2037 -> 5_900_875
    2038 -> 4_457_643
    2039 -> 3_056_987
    2040 -> 5_506_477
    2041 -> 4_066_154
    2042 -> 2_775_890
    2043 -> 5_246_354
    2044 -> 3_865_893
    2045 -> 6_163_749
    2046 -> 4_721_237
    2047 -> 3_323_053
    2048 -> 5_768_374
    2049 -> 4_195_765
    2050 -> 2_911_658
    2051 -> 5_377_737
    2052 -> 4_136_594
    2053 -> 6_426_258
    2054 -> 4_984_102
    2055 -> 3_590_742
    2056 -> 5_900_887
    2057 -> 4_457_814
    2058 -> 3_049_173
    2059 -> 5_506_901
    2060 -> 4_196_169
    2061 -> 2_649_747
    2062 -> 5_113_491
    2063 -> 3_732_779
    2064 -> 6_161_707
    2065 -> 4_590_171
    2066 -> 3_323_226
    2067 -> 5_768_554
    2068 -> 4_328_293
    2069 -> 2_922_314
    2070 -> 5_376_842
    2071 -> 4_004_501
    2072 -> 6_425_237
    2073 -> 4_850_989
    2074 -> 3_459_757
    2075 -> 5_900_981
    2076 -> 4_588_970
    2077 -> 3_050_405
    2078 -> 5_508_517
    2079 -> 4_197_706
    2080 -> 2_784_405
    2081 -> 5_115_030
    2082 -> 3_733_838
    2083 -> 6_161_750
    2084 -> 4_721_333
    2085 -> 3_323_314
    2086 -> 5_768_914
    2087 -> 4_329_125
    2088 -> 3_051_082
    2089 -> 5_244_555
    2090 -> 3_869_847
    2091 -> 6_292_651
    2092 -> 4_851_035
    2093 -> 3_459_798
    2094 -> 5_901_162
    2095 -> 4_589_394
    2096 -> 3_184_421
    2097 -> 5_507_909
    2098 -> 4_065_931
    2099 -> 2_643_099
    2100 -> 5_113_003
    2101 -> 3_729_755
    2102 -> 6_161_837
    2103 -> 4_852_650
    2104 -> 3_586_898
    2105 -> 5_901_714
    2106 -> 4_459_813
    2107 -> 3_054_155
    2108 -> 5_507_669
    2109 -> 4_011_181
    2110 -> 6_423_734
    2111 -> 4_982_453
    2112 -> 3_722_666
    2113 -> 6_033_097
    2114 -> 4_722_322
    2115 -> 3_317_030
    2116 -> 5_770_538
    2117 -> 4_196_950
    2118 -> 2_782_390
    2119 -> 5_244_246
    2120 -> 3_861_205
    2121 -> 6_163_285
    2122 -> 4_851_530
    2123 -> 3_452_563
    2124 -> 5_899_925
    2125 -> 4_326_699
    2126 -> 2_918_999
    2127 -> 5_376_667
    2128 -> 4_158_810
    2129 -> 6_423_914
    2130 -> 4_983_653
    2131 -> 3_725_130
    2132 -> 6_163_274
    2133 -> 4_590_357
    2134 -> 3_192_107
    2135 -> 5_637_453
    2136 -> 4_197_037
    2137 -> 2_774_378
    2138 -> 5_244_330
    2139 -> 3_861_413
    2140 -> 6_294_949
    2141 -> 4_853_066
    2142 -> 3_456_277
    2143 -> 5_901_590
    2144 -> 4_458_830
    2145 -> 2_919_085
    2146 -> 5_376_726
    2147 -> 4_158_900
    2148 -> 6_555_346
    2149 -> 4_984_485
    2150 -> 3_722_890
    2151 -> 6_030_987
    2152 -> 4_590_871
    2153 -> 3_189_078
    2154 -> 5_507_419
    2155 -> 4_197_082
    2156 -> 2_914_004
    2157 -> 5_244_756
    2158 -> 3_864_389
    2159 -> 6_294_341
    2160 -> 4_852_363
    2161 -> 3_331_371
    2162 -> 5_768_365
    2163 -> 4_327_787
    2164 -> 3_050_330
    2165 -> 5_377_450
    2166 -> 4_152_148
    2167 -> 6_557_090
    2168 -> 5_115_205
    2169 -> 3_594_901
    2170 -> 6_032_021
    2171 -> 4_588_845
    2172 -> 3_189_421
    2173 -> 5_507_765
    2174 -> 4_197_802
    2175 -> 2_915_748
    2176 -> 5_377_698
    2177 -> 3_865_926
    2178 -> 6_294_858
    2179 -> 4_852_374
    2180 -> 3_462_454
    2181 -> 5_768_538
    2182 -> 4_328_149
    2183 -> 3_053_258
    2184 -> 5_506_898
    2185 -> 3_935_909
    2186 -> 2_641_226
    2187 -> 4_982_091
    2188 -> 3_590_807
    2189 -> 5_900_971
    2190 -> 4_588_890
    2191 -> 3_189_461
    2192 -> 5_639_013
    2193 -> 4_196_178
    2194 -> 2_783_909
    2195 -> 5_245_733
    2196 -> 3_865_163
    2197 -> 6_162_765
    2198 -> 4_721_325
    2199 -> 3_462_506
    2200 -> 5_899_700
    2201 -> 4_459_433
    2202 -> 3_185_490
    2203 -> 5_639_570
    2204 -> 4_275_493
    2205 -> 6_556_966
    2206 -> 5_114_198
    2207 -> 3_723_949
    2208 -> 6_163_158
    2209 -> 4_720_340
    2210 -> 3_313_065
    2211 -> 5_770_953
    2212 -> 4_460_178
    2213 -> 2_911_526
    2214 -> 5_244_199
    2215 -> 3_861_079
    2216 -> 6_293_851
    2217 -> 4_852_442
    2218 -> 3_585_748
    2219 -> 6_031_188
    2220 -> 4_589_385
    2221 -> 3_053_203
    2222 -> 5_507_731
    2223 -> 4_142_379
    2224 -> 6_554_925
    2225 -> 4_983_149
    2226 -> 3_730_282
    2227 -> 6_163_882
    2228 -> 4_852_644
    2229 -> 3_324_745
    2230 -> 5_770_569
    2231 -> 4_328_085
    2232 -> 2_913_579
    2233 -> 5_244_205
    2234 -> 3_869_357
    2235 -> 6_294_197
    2236 -> 4_984_234
    2237 -> 3_587_492
    2238 -> 6_033_060
    2239 -> 4_590_922
    2240 -> 3_185_301
    2241 -> 5_507_734
    2242 -> 4_158_774
    2243 -> 6_554_970
    2244 -> 5_114_581
    2245 -> 3_725_010
    2246 -> 6_162_258
    2247 -> 4_722_341
    2248 -> 3_454_538
    2249 -> 5_637_707
    2250 -> 4_197_015
    2251 -> 2_913_622
    2252 -> 5_375_322
    2253 -> 3_861_333
    2254 -> 6_294_441
    2255 -> 4_982_610
    2256 -> 3_595_045
    2257 -> 5_901_093
    2258 -> 4_459_083
    2259 -> 3_060_891
    2260 -> 5_505_709
    2261 -> 3_933_547
    2262 -> 2_632_553
    2263 -> 5_115_305
    2264 -> 3_865_938
    2265 -> 6_163_858
    2266 -> 4_721_957
    2267 -> 3_324_493
    2268 -> 5_769_814
    2269 -> 4_194_997
    2270 -> 2_782_637
    2271 -> 5_375_700
    2272 -> 4_001_193
    2273 -> 6_295_241
    2274 -> 4_984_466
    2275 -> 3_591_462
    2276 -> 5_899_559
    2277 -> 4_328_023
    2278 -> 3_052_214
    2279 -> 5_507_930
    2280 -> 4_196_052
    2281 -> 2_641_577
    2282 -> 5_113_673
    2283 -> 3_724_947
    2284 -> 6_163_091
    2285 -> 4_588_843
    2286 -> 3_189_339
    2287 -> 5_638_765
    2288 -> 4_326_762
    2289 -> 2_784_085
    2290 -> 5_376_932
    2291 -> 3_996_489
    2292 -> 6_425_929
    2293 -> 4_852_373
    2294 -> 3_462_445
    2295 -> 5_899_566
    2296 -> 4_459_181
    2297 -> 3_052_906
    2298 -> 5_506_482
    2299 -> 4_066_725
    2300 -> 2_776_394
    _ -> 0
  }
}

fn dangi_data(y: Int) -> Int {
  case y {
    1700 -> 6_425_893
    1701 -> 4_983_381
    1702 -> 3_593_389
    1703 -> 6_030_518
    1704 -> 4_588_981
    1705 -> 3_181_994
    1706 -> 5_639_881
    1707 -> 4_329_106
    1708 -> 2_915_621
    1709 -> 5_246_246
    1710 -> 3_861_078
    1711 -> 6_163_035
    1712 -> 4_850_902
    1713 -> 3_319_509
    1714 -> 5_769_045
    1715 -> 4_458_313
    1716 -> 3_042_963
    1717 -> 5_375_635
    1718 -> 4_003_115
    1719 -> 6_423_851
    1720 -> 4_983_387
    1721 -> 3_593_562
    1722 -> 6_030_698
    1723 -> 4_590_437
    1724 -> 3_315_530
    1725 -> 5_638_985
    1726 -> 4_197_013
    1727 -> 2_774_315
    1728 -> 5_244_205
    1729 -> 3_730_093
    1730 -> 6_163_125
    1731 -> 4_851_114
    1732 -> 3_451_813
    1733 -> 5_770_661
    1734 -> 4_459_850
    1735 -> 3_046_549
    1736 -> 5_508_246
    1737 -> 4_012_366
    1738 -> 6_423_894
    1739 -> 4_983_477
    1740 -> 3_724_722
    1741 -> 6_031_058
    1742 -> 4_591_269
    1743 -> 3_313_226
    1744 -> 5_637_771
    1745 -> 4_066_455
    1746 -> 2_779_478
    1747 -> 5_113_179
    1748 -> 3_861_206
    1749 -> 6_163_305
    1750 -> 4_851_538
    1751 -> 3_454_757
    1752 -> 5_901_125
    1753 -> 4_328_075
    1754 -> 2_921_643
    1755 -> 5_375_147
    1756 -> 4_008_299
    1757 -> 6_292_909
    1758 -> 4_983_722
    1759 -> 3_726_162
    1760 -> 6_163_858
    1761 -> 4_590_917
    1762 -> 3_193_483
    1763 -> 5_638_741
    1764 -> 4_195_501
    1765 -> 2_640_237
    1766 -> 5_113_525
    1767 -> 3_861_930
    1768 -> 6_295_241
    1769 -> 4_853_394
    1770 -> 3_456_293
    1771 -> 5_901_610
    1772 -> 4_459_094
    1773 -> 2_913_462
    1774 -> 5_375_322
    1775 -> 4_016_853
    1776 -> 6_424_421
    1777 -> 4_982_602
    1778 -> 3_591_827
    1779 -> 6_030_997
    1780 -> 4_588_843
    1781 -> 3_058_327
    1782 -> 5_507_755
    1783 -> 4_195_674
    1784 -> 2_779_861
    1785 -> 5_114_725
    1786 -> 3_864_394
    1787 -> 6_294_346
    1788 -> 4_852_373
    1789 -> 3_331_371
    1790 -> 5_768_525
    1791 -> 4_328_109
    1792 -> 3_052_906
    1793 -> 5_375_410
    1794 -> 3_935_141
    1795 -> 2_644_810
    1796 -> 5_115_210
    1797 -> 3_595_541
    1798 -> 6_032_662
    1799 -> 4_589_910
    1800 -> 3_181_229
    1801 -> 5_638_870
    1802 -> 4_326_868
    1803 -> 2_911_653
    1804 -> 5_377_701
    1805 -> 3_985_034
    1806 -> 6_293_131
    1807 -> 4_853_031
    1808 -> 3_582_294
    1809 -> 5_768_539
    1810 -> 4_459_226
    1811 -> 3_176_148
    1812 -> 5_637_972
    1813 -> 4_065_093
    1814 -> 2_643_595
    1815 -> 5_114_507
    1816 -> 3_724_587
    1817 -> 6_030_509
    1818 -> 4_589_931
    1819 -> 3_312_490
    1820 -> 5_770_154
    1821 -> 4_328_340
    1822 -> 2_915_141
    1823 -> 5_377_349
    1824 -> 3_996_309
    1825 -> 6_294_165
    1826 -> 4_850_989
    1827 -> 3_451_309
    1828 -> 5_899_957
    1829 -> 4_459_946
    1830 -> 3_186_084
    1831 -> 5_639_842
    1832 -> 4_275_526
    1833 -> 6_557_002
    1834 -> 5_114_518
    1835 -> 3_724_598
    1836 -> 6_161_754
    1837 -> 4_590_293
    1838 -> 3_315_402
    1839 -> 5_769_042
    1840 -> 4_329_125
    1841 -> 2_911_562
    1842 -> 5_244_235
    1843 -> 3_861_143
    1844 -> 6_294_187
    1845 -> 4_851_034
    1846 -> 3_451_605
    1847 -> 5_901_161
    1848 -> 4_589_394
    1849 -> 3_053_221
    1850 -> 5_507_877
    1851 -> 4_159_051
    1852 -> 6_554_957
    1853 -> 4_983_469
    1854 -> 3_732_842
    1855 -> 6_161_844
    1856 -> 4_721_577
    1857 -> 3_324_754
    1858 -> 5_770_642
    1859 -> 4_328_741
    1860 -> 2_914_893
    1861 -> 5_245_270
    1862 -> 3_869_365
    1863 -> 6_294_230
    1864 -> 4_982_484
    1865 -> 3_452_329
    1866 -> 5_902_025
    1867 -> 4_591_250
    1868 -> 3_181_862
    1869 -> 5_377_319
    1870 -> 4_147_542
    1871 -> 6_424_923
    1872 -> 5_114_714
    1873 -> 3_725_012
    1874 -> 6_162_260
    1875 -> 4_720_457
    1876 -> 3_323_539
    1877 -> 5_638_803
    1878 -> 4_195_627
    1879 -> 2_779_739
    1880 -> 5_245_293
    1881 -> 3_861_354
    1882 -> 6_294_442
    1883 -> 4_983_716
    1884 -> 3_586_889
    1885 -> 5_901_641
    1886 -> 4_459_157
    1887 -> 3_052_843
    1888 -> 5_506_349
    1889 -> 4_033_197
    1890 -> 6_425_269
    1891 -> 5_115_306
    1892 -> 3_857_828
    1893 -> 6_164_132
    1894 -> 4_721_994
    1895 -> 3_324_565
    1896 -> 5_769_878
    1897 -> 4_195_638
    1898 -> 2_779_829
    1899 -> 5_245_653
    1900 -> 4_003_538
    1901 -> 6_424_402
    1902 -> 4_984_485
    1903 -> 3_714_634
    1904 -> 6_030_667
    1905 -> 4_459_159
    1906 -> 3_183_958
    1907 -> 5_637_482
    1908 -> 4_197_205
    1909 -> 2_774_866
    1910 -> 5_244_754
    1911 -> 3_856_165
    1912 -> 6_294_309
    1913 -> 4_721_227
    1914 -> 3_322_523
    1915 -> 5_769_901
    1916 -> 4_457_834
    1917 -> 2_902_889
    1918 -> 5_376_937
    1919 -> 4_127_570
    1920 -> 6_557_074
    1921 -> 4_984_101
    1922 -> 3_586_637
    1923 -> 6_031_702
    1924 -> 4_588_213
    1925 -> 3_052_973
    1926 -> 5_637_844
    1927 -> 4_197_801
    1928 -> 2_907_538
    1929 -> 5_246_610
    1930 -> 3_853_606
    1931 -> 6_161_703
    1932 -> 4_721_239
    1933 -> 3_322_550
    1934 -> 5_769_946
    1935 -> 4_458_196
    1936 -> 3_042_985
    1937 -> 5_375_817
    1938 -> 3_995_283
    1939 -> 6_425_235
    1940 -> 4_982_059
    1941 -> 3_459_675
    1942 -> 5_900_653
    1943 -> 4_590_442
    1944 -> 3_316_564
    1945 -> 5_639_076
    1946 -> 4_197_193
    1947 -> 2_775_699
    1948 -> 5_245_589
    1949 -> 3_732_779
    1950 -> 6_161_709
    1951 -> 4_721_325
    1952 -> 3_454_314
    1953 -> 5_770_674
    1954 -> 4_459_940
    1955 -> 3_046_729
    1956 -> 5_508_426
    1957 -> 4_004_501
    1958 -> 6_425_238
    1959 -> 4_982_102
    1960 -> 3_590_837
    1961 -> 5_901_013
    1962 -> 4_589_266
    1963 -> 3_182_245
    1964 -> 5_639_845
    1965 -> 4_197_962
    1966 -> 2_780_310
    1967 -> 5_114_523
    1968 -> 3_863_894
    1969 -> 6_161_770
    1970 -> 4_721_497
    1971 -> 3_454_802
    1972 -> 5_900_114
    1973 -> 4_327_205
    1974 -> 2_922_059
    1975 -> 5_376_587
    1976 -> 4_002_475
    1977 -> 6_292_141
    1978 -> 4_851_051
    1979 -> 3_591_017
    1980 -> 6_032_809
    1981 -> 4_590_994
    1982 -> 3_185_445
    1983 -> 5_639_461
    1984 -> 4_282_957
    1985 -> 6_556_246
    1986 -> 5_112_502
    1987 -> 3_724_717
    1988 -> 6_293_204
    1989 -> 4_722_089
    1990 -> 3_456_402
    1991 -> 5_901_970
    1992 -> 4_459_814
    1993 -> 2_910_806
    1994 -> 5_245_527
    1995 -> 4_002_486
    1996 -> 6_425_434
    1997 -> 4_982_484
    1998 -> 3_583_689
    1999 -> 6_031_177
    2000 -> 4_589_203
    2001 -> 3_052_839
    2002 -> 5_506_347
    2003 -> 4_065_883
    2004 -> 2_774_362
    2005 -> 5_112_682
    2006 -> 3_734_357
    2007 -> 6_294_436
    2008 -> 4_852_553
    2009 -> 3_324_563
    2010 -> 5_769_877
    2011 -> 4_326_701
    2012 -> 2_910_813
    2013 -> 5_245_613
    2014 -> 4_011_434
    2015 -> 6_424_018
    2016 -> 4_984_229
    2017 -> 3_587_402
    2018 -> 6_032_714
    2019 -> 4_590_229
    2020 -> 3_183_917
    2021 -> 5_506_390
    2022 -> 4_065_973
    2023 -> 2_774_442
    2024 -> 5_244_626
    2025 -> 3_722_917
    2026 -> 6_164_133
    2027 -> 4_853_322
    2028 -> 3_452_054
    2029 -> 5_639_323
    2030 -> 4_326_746
    2031 -> 2_910_933
    2032 -> 5_376_873
    2033 -> 4_028_242
    2034 -> 6_424_402
    2035 -> 4_983_589
    2036 -> 3_593_803
    2037 -> 5_900_875
    2038 -> 4_457_643
    2039 -> 3_056_987
    2040 -> 5_506_413
    2041 -> 4_066_153
    2042 -> 2_775_890
    2043 -> 5_246_354
    2044 -> 3_865_893
    2045 -> 6_163_749
    2046 -> 4_721_229
    2047 -> 3_323_053
    2048 -> 5_767_862
    2049 -> 4_195_765
    2050 -> 2_911_657
    2051 -> 5_377_705
    2052 -> 4_136_338
    2053 -> 6_426_258
    2054 -> 4_984_102
    2055 -> 3_590_742
    2056 -> 5_900_887
    2057 -> 4_457_686
    2058 -> 3_049_141
    2059 -> 5_506_773
    2060 -> 4_198_089
    2061 -> 2_780_818
    2062 -> 5_113_491
    2063 -> 3_732_779
    2064 -> 6_161_707
    2065 -> 4_590_171
    2066 -> 3_323_226
    2067 -> 5_768_554
    2068 -> 4_328_277
    2069 -> 2_922_313
    2070 -> 5_376_841
    2071 -> 4_004_499
    2072 -> 6_425_237
    2073 -> 4_850_989
    2074 -> 3_459_757
    2075 -> 5_900_981
    2076 -> 4_588_970
    2077 -> 3_050_405
    2078 -> 5_508_517
    2079 -> 4_197_706
    2080 -> 2_783_893
    2081 -> 5_115_029
    2082 -> 3_732_782
    2083 -> 6_161_750
    2084 -> 4_721_333
    2085 -> 3_323_314
    2086 -> 5_768_914
    2087 -> 4_329_125
    2088 -> 3_055_178
    2089 -> 5_375_562
    2090 -> 3_869_847
    2091 -> 6_294_699
    2092 -> 4_982_106
    2093 -> 3_459_797
    2094 -> 5_901_161
    2095 -> 4_589_394
    2096 -> 3_184_293
    2097 -> 5_507_877
    2098 -> 4_064_843
    2099 -> 2_651_287
    2100 -> 5_113_003
    2101 -> 3_728_731
    2102 -> 6_161_837
    2103 -> 4_852_585
    2104 -> 3_586_898
    2105 -> 5_901_714
    2106 -> 4_459_813
    2107 -> 3_054_155
    2108 -> 5_507_669
    2109 -> 4_011_181
    2110 -> 6_423_734
    2111 -> 4_982_197
    2112 -> 3_722_666
    2113 -> 6_033_097
    2114 -> 4_722_322
    2115 -> 3_317_029
    2116 -> 5_770_534
    2117 -> 4_196_950
    2118 -> 2_782_382
    2119 -> 5_244_118
    2120 -> 3_861_205
    2121 -> 6_162_133
    2122 -> 4_851_401
    2123 -> 3_452_563
    2124 -> 5_899_923
    2125 -> 4_326_699
    2126 -> 2_918_999
    2127 -> 5_376_603
    2128 -> 4_142_426
    2129 -> 6_423_914
    2130 -> 4_983_653
    2131 -> 3_725_130
    2132 -> 6_163_273
    2133 -> 4_590_229
    2134 -> 3_192_107
    2135 -> 5_637_421
    2136 -> 4_197_037
    2137 -> 2_774_378
    2138 -> 5_244_330
    2139 -> 3_861_413
    2140 -> 6_294_949
    2141 -> 4_853_066
    2142 -> 3_456_149
    2143 -> 5_901_462
    2144 -> 4_458_830
    2145 -> 2_919_085
    2146 -> 5_376_693
    2147 -> 4_158_898
    2148 -> 6_555_346
    2149 -> 4_984_485
    2150 -> 3_726_922
    2151 -> 6_162_058
    2152 -> 4_590_743
    2153 -> 3_189_078
    2154 -> 5_506_395
    2155 -> 4_197_078
    2156 -> 2_914_004
    2157 -> 5_244_754
    2158 -> 3_864_357
    2159 -> 6_294_341
    2160 -> 4_852_363
    2161 -> 3_331_227
    2162 -> 5_768_363
    2163 -> 4_327_771
    2164 -> 3_050_330
    2165 -> 5_376_938
    2166 -> 4_152_146
    2167 -> 6_557_074
    2168 -> 5_115_205
    2169 -> 3_594_827
    2170 -> 6_031_957
    2171 -> 4_588_717
    2172 -> 3_180_909
    2173 -> 5_506_741
    2174 -> 4_197_802
    2175 -> 2_915_730
    2176 -> 5_377_698
    2177 -> 3_865_925
    2178 -> 6_294_826
    2179 -> 4_852_310
    2180 -> 3_462_454
    2181 -> 5_768_534
    2182 -> 4_328_149
    2183 -> 3_053_226
    2184 -> 5_506_890
    2185 -> 3_935_907
    2186 -> 2_641_194
    2187 -> 4_982_059
    2188 -> 3_598_935
    2189 -> 5_900_955
    2190 -> 4_588_890
    2191 -> 3_189_461
    2192 -> 5_639_013
    2193 -> 4_196_170
    2194 -> 2_782_869
    2195 -> 5_245_589
    2196 -> 3_863_851
    2197 -> 6_161_741
    2198 -> 4_721_325
    2199 -> 3_462_506
    2200 -> 5_899_690
    2201 -> 4_459_429
    2202 -> 3_185_482
    2203 -> 5_639_562
    2204 -> 4_275_477
    2205 -> 6_556_950
    2206 -> 5_114_198
    2207 -> 3_721_901
    2208 -> 6_163_158
    2209 -> 4_720_052
    2210 -> 3_313_065
    2211 -> 5_770_917
    2212 -> 4_460_170
    2213 -> 2_911_510
    2214 -> 5_246_247
    2215 -> 3_991_894
    2216 -> 6_293_851
    2217 -> 4_852_442
    2218 -> 3_585_748
    2219 -> 6_031_188
    2220 -> 4_589_381
    2221 -> 3_053_195
    2222 -> 5_507_723
    2223 -> 4_142_379
    2224 -> 6_554_797
    2225 -> 4_983_147
    2226 -> 3_730_266
    2227 -> 6_163_370
    2228 -> 4_852_564
    2229 -> 3_324_741
    2230 -> 5_770_565
    2231 -> 4_328_085
    2232 -> 2_913_579
    2233 -> 5_244_205
    2234 -> 3_869_357
    2235 -> 6_293_173
    2236 -> 4_984_234
    2237 -> 3_587_492
    2238 -> 6_033_060
    2239 -> 4_590_918
    2240 -> 3_185_301
    2241 -> 5_507_734
    2242 -> 4_158_774
    2243 -> 6_554_970
    2244 -> 5_114_581
    2245 -> 3_725_002
    2246 -> 6_162_258
    2247 -> 4_722_341
    2248 -> 3_452_234
    2249 -> 5_637_451
    2250 -> 4_197_015
    2251 -> 2_913_622
    2252 -> 5_375_322
    2253 -> 3_869_397
    2254 -> 6_294_377
    2255 -> 4_982_610
    2256 -> 3_593_893
    2257 -> 5_901_093
    2258 -> 4_459_083
    2259 -> 3_052_187
    2260 -> 5_507_757
    2261 -> 4_064_618
    2262 -> 2_632_553
    2263 -> 5_114_793
    2264 -> 3_857_234
    2265 -> 6_163_858
    2266 -> 4_721_957
    2267 -> 3_324_493
    2268 -> 5_769_558
    2269 -> 4_194_989
    2270 -> 2_782_637
    2271 -> 5_375_700
    2272 -> 4_001_193
    2273 -> 6_295_241
    2274 -> 4_984_466
    2275 -> 3_591_462
    2276 -> 5_899_559
    2277 -> 4_328_023
    2278 -> 3_052_214
    2279 -> 5_507_802
    2280 -> 4_196_052
    2281 -> 2_641_577
    2282 -> 5_113_673
    2283 -> 3_724_947
    2284 -> 6_163_091
    2285 -> 4_588_843
    2286 -> 3_189_339
    2287 -> 5_638_507
    2288 -> 4_328_298
    2289 -> 2_915_156
    2290 -> 5_376_932
    2291 -> 3_996_489
    2292 -> 6_425_929
    2293 -> 4_852_373
    2294 -> 3_462_443
    2295 -> 5_899_565
    2296 -> 4_459_181
    2297 -> 3_052_906
    2298 -> 5_508_522
    2299 -> 4_197_796
    2300 -> 2_776_393
    _ -> 0
  }
}
