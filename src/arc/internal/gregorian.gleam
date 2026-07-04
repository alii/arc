//// Shared proleptic-Gregorian calendar math.
////
//// Every date-shaped builtin (`Date`, `Temporal`, `Intl.DateTimeFormat`) needs
//// the same handful of primitives: floor division/modulo, leap years, month
//// lengths and Howard Hinnant's `days_from_civil`/`civil_from_days` pair.
//// They live here so a fix lands once instead of drifting across five files.
////
//// Months are 1-based (January = 1). Callers with a 0-based month convention
//// keep their own thin adapter.

/// Integer division flooring toward -infinity. `b` must be non-zero.
pub fn floor_div(a: Int, b: Int) -> Int {
  let q = a / b
  case a % b != 0 && { a < 0 } != { b < 0 } {
    True -> q - 1
    False -> q
  }
}

/// Modulo whose result carries the sign of the divisor. `b` must be non-zero.
pub fn floor_mod(a: Int, b: Int) -> Int {
  a - floor_div(a, b) * b
}

pub fn is_leap_year(y: Int) -> Bool {
  floor_mod(y, 4) == 0 && { floor_mod(y, 100) != 0 || floor_mod(y, 400) == 0 }
}

pub fn days_in_year(y: Int) -> Int {
  case is_leap_year(y) {
    True -> 366
    False -> 365
  }
}

/// Length of 1-based month `m` in year `y`.
pub fn days_in_month(y: Int, m: Int) -> Int {
  case m {
    2 ->
      case is_leap_year(y) {
        True -> 29
        False -> 28
      }
    4 | 6 | 9 | 11 -> 30
    _ -> 31
  }
}

/// Days since 1970-01-01 for a proleptic Gregorian date (1-based month).
pub fn days_from_civil(y: Int, m: Int, d: Int) -> Int {
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

/// Proleptic Gregorian `#(year, month, day)` (1-based month) from days since
/// 1970-01-01.
pub fn civil_from_days(z: Int) -> #(Int, Int, Int) {
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
