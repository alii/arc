import arc/internal/gregorian.{
  civil_from_days, days_from_civil, days_in_month as gregorian_days_in_month,
  is_leap_year as is_gregorian_leap,
}
import arc/internal/int_math.{floor_div, floor_mod}
import arc/internal/temporal_calendar_data.{
  chinese_data, dangi_data, umalqura_month_length, umalqura_year_start_fix,
}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type CalDate {
  CalDate(year: Int, month: Int, day: Int)
}

pub type MonthCodeIssue {
  NeverValid
  NotInThisYear(skip_to: Int)
}

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

type Arithmetic {
  IsoLike(year_offset: Int)
  CopticLike(epoch: Int, year_shift: Int)
  TabularIslamic(epoch: Int)
  UmmAlQura
  PersianArith
  IndianArith
  HebrewArith
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

// rata die epochs minus 719163
const coptic_epoch = -615_558

const ethiopic_epoch = -716_367

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

const islamic_civil_epoch = -492_148

const islamic_tbla_epoch = -492_149

fn islamic_is_leap(year: Int) -> Bool {
  floor_mod(14 + 11 * year, 30) < 11
}

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

fn hebrew_new_year(year: Int) -> Int {
  let e0 = hebrew_elapsed_days(year - 1)
  let e1 = hebrew_elapsed_days(year)
  let e2 = hebrew_elapsed_days(year + 1)
  hebrew_epoch + e1 + hebrew_year_length_correction(e0, e1, e2)
}

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

fn hebrew_days_in_month(year: Int, month: Int) -> Int {
  hebrew_shape_days_in_month(hebrew_year_shape(year), month)
}

fn hebrew_shape_days_in_month(shape: HebrewYearShape, month: Int) -> Int {
  let HebrewYearShape(length: ylen, leap:, ..) = shape
  case month {
    1 -> 30
    // heshvan
    2 ->
      case ylen == 355 || ylen == 385 {
        True -> 30
        False -> 29
      }
    // kislev
    3 ->
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
            7 -> 29
            _ ->
              case floor_mod(month, 2) == 0 {
                True -> 30
                False -> 29
              }
          }
        False ->
          case month {
            6 -> 29
            _ ->
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

pub fn day_of_year(cal: Calendar, year: Int, month: Int, day: Int) -> Int {
  date_to_epoch_days(cal, year, month, day)
  - date_to_epoch_days(cal, year, 1, 1)
  + 1
}

fn pad2(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

pub type MonthCode {
  MonthCode(number: Int, leap: Bool)
}

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

pub fn month_code_string(mc: MonthCode) -> String {
  case mc.leap {
    True -> "M" <> pad2(mc.number) <> "L"
    False -> "M" <> pad2(mc.number)
  }
}

pub fn month_code(cal: Calendar, year: Int, month: Int) -> String {
  month_code_string(month_code_of(cal, year, month))
}

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

pub fn carry_month_code(
  cal: Calendar,
  target_year: Int,
  mc: MonthCode,
) -> Result(Int, Int) {
  case month_for_code(cal, target_year, mc) {
    Ok(ordinal) -> Ok(ordinal)
    Error(NotInThisYear(skip_to)) -> Error(skip_to)
    // unreachable for a minted code
    Error(NeverValid) ->
      Error(int.min(mc.number, months_in_year(cal, target_year)))
  }
}

pub fn has_eras(cal: Calendar) -> Bool {
  eras_of(cal) != []
}

pub type EraCode {
  Ce
  Bce
  Be
  Minguo
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

pub type Era {
  Era(code: EraCode, year: Int)
}

pub fn era_for(cal: Calendar, year: Int, month: Int, day: Int) -> Option(Era) {
  use code <- option.map(era_code_for(cal, year, month, day))
  let assert Ok(shift) = list.key_find(eras_of(cal), code)
  Era(code, era_year(shift, year))
}

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

type EraShift {
  Forward(Int)
  Backward(Int)
}

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

pub fn year_for_era(cal: Calendar, era: EraCode, ey: Int) -> Result(Int, Nil) {
  use shift <- result.map(list.key_find(eras_of(cal), era))
  case shift {
    Forward(k) -> k + ey
    Backward(k) -> k - ey
  }
}

fn era_year(shift: EraShift, year: Int) -> Int {
  case shift {
    Forward(k) -> year - k
    Backward(k) -> k - year
  }
}

// metonic fallback: 235 months per 19 years
fn lunisolar_eb(y: Int) -> Int {
  floor_div(235 * y - 234, 19)
}

// mean synodic month = 1447/49 days
fn lunisolar_fb_pos(t: Int) -> Int {
  floor_div(1447 * t, 49)
}

const lunisolar_first_year = 1700

const lunisolar_last_year = 2300

// packed: bits 0-12 month lengths, 13-16 leap month, 17-22 new year offset
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

fn lunisolar_year_start(data: fn(Int) -> Result(Int, Nil), y: Int) -> Int {
  case lunisolar_year(data, y) {
    Tabulated(new_year_offset:, ..) ->
      days_from_civil(y, 1, 1) + new_year_offset
    MeanMotion -> {
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
