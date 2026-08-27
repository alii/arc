// months are 1-based

import arc/internal/int_math.{floor_div, floor_mod}

pub fn is_leap_year(y: Int) -> Bool {
  floor_mod(y, 4) == 0 && { floor_mod(y, 100) != 0 || floor_mod(y, 400) == 0 }
}

pub fn days_in_year(y: Int) -> Int {
  case is_leap_year(y) {
    True -> 366
    False -> 365
  }
}

// §21.4.1.3 dayfromyear
pub fn days_from_year(y: Int) -> Int {
  365
  * { y - 1970 }
  + floor_div(y - 1969, 4)
  - floor_div(y - 1901, 100)
  + floor_div(y - 1601, 400)
}

// never return 0 here, month scan loops would spin
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

// 0 = sunday, epoch day 0 was a thursday
pub fn weekday_from_days(z: Int) -> Int {
  floor_mod(z + 4, 7)
}

// 1 = monday .. 7 = sunday
pub fn iso_weekday_from_days(z: Int) -> Int {
  floor_mod(z + 3, 7) + 1
}

// hinnant days_from_civil
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

// hinnant civil_from_days
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
