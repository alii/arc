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
  NotInThisYear(constrain_to: Int)
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

type YearTable =
  fn(Int) -> Option(Int)

type Arithmetic {
  IsoArith(year_offset: Int)
  CopticArith(epoch: Int, year_shift: Int)
  TabularIslamicArith(epoch: Int)
  UmalquraArith
  PersianArith
  IndianArith
  HebrewArith
  LunisolarArith(year_table: YearTable)
}

fn arithmetic(cal: Calendar) -> Arithmetic {
  case cal {
    Iso8601 | Gregory | Japanese -> IsoArith(0)
    Buddhist -> IsoArith(543)
    Roc -> IsoArith(-1911)
    Coptic -> CopticArith(epoch: coptic_epoch, year_shift: 0)
    Ethiopic -> CopticArith(epoch: ethiopic_epoch, year_shift: 0)
    Ethioaa -> CopticArith(epoch: ethiopic_epoch, year_shift: 5500)
    IslamicCivil -> TabularIslamicArith(islamic_civil_epoch)
    IslamicTbla -> TabularIslamicArith(islamic_tbla_epoch)
    IslamicUmalqura -> UmalquraArith
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

fn coptic_from_days(epoch: Int, shift: Int, days: Int) -> CalDate {
  let y = floor_div(4 * { days - epoch } + 1463, 1461)
  let m =
    floor_div(days - coptic_to_days(epoch, shift, y + shift, 1, 1), 30) + 1
  let d = days - coptic_to_days(epoch, shift, y + shift, m, 1) + 1
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
  case floor_mod(month, 2) == 1 || { month == 12 && islamic_is_leap(year) } {
    True -> 30
    False -> 29
  }
}

fn islamic_from_days(epoch: Int, days: Int) -> CalDate {
  let y0 = floor_div(30 * { days - epoch } + 10_646, 10_631)
  let y = adjust_year(days, y0, fn(yy) { islamic_to_days(epoch, yy, 1, 1) })
  let #(m, d) =
    scan_months(days, y, 1, 12, fn(yy, mm) { islamic_to_days(epoch, yy, mm, 1) })
  CalDate(y, m, d)
}

// month_bits: bit (12 - month) set = 30 days
type UmalquraYear {
  UmalquraTabulated(month_bits: Int, year_start_fix: Int)
  UmalquraCivilFallback
}

fn umalqura_year(year: Int) -> UmalquraYear {
  let tabulated = {
    use month_bits <- option.then(umalqura_month_length(year))
    use year_start_fix <- option.map(umalqura_year_start_fix(year))
    UmalquraTabulated(month_bits:, year_start_fix:)
  }
  option.unwrap(tabulated, UmalquraCivilFallback)
}

fn umalqura_year_start(year: Int) -> Int {
  case umalqura_year(year) {
    UmalquraTabulated(year_start_fix:, ..) ->
      floor_div(35_436_720 * { year - 1300 } + 46_032_255_000, 100_000)
      + year_start_fix
    UmalquraCivilFallback -> 354 * { year - 1 } + floor_div(3 + 11 * year, 30)
  }
}

fn umalqura_days_in_month(year: Int, month: Int) -> Int {
  case umalqura_year(year) {
    UmalquraTabulated(month_bits:, ..) ->
      29 + int.bitwise_and(int.bitwise_shift_right(month_bits, 12 - month), 1)
    UmalquraCivilFallback -> islamic_days_in_month(year, month)
  }
}

fn umalqura_to_days(year: Int, month: Int, day: Int) -> Int {
  islamic_civil_epoch
  + umalqura_year_start(year)
  + days_before_month(month, umalqura_days_in_month(year, _))
  + day
  - 1
}

fn umalqura_from_days(days: Int) -> CalDate {
  let y0 = floor_div(30 * { days - islamic_civil_epoch } + 10_646, 10_631)
  let y = adjust_year(days, y0, fn(yy) { umalqura_to_days(yy, 1, 1) })
  let #(m, d) =
    scan_months(days, y, 1, 12, fn(yy, mm) { umalqura_to_days(yy, mm, 1) })
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
  case month <= 6, month <= 11 || persian_is_leap(year) {
    True, _ -> 31
    False, True -> 30
    False, False -> 29
  }
}

fn persian_from_days(days: Int) -> CalDate {
  let y0 = 1 + floor_div(33 * { days - persian_epoch } + 3, 12_053)
  let y = adjust_year(days, y0, fn(yy) { persian_to_days(yy, 1, 1) })
  let #(m, d) =
    scan_months(days, y, 1, 12, fn(yy, mm) { persian_to_days(yy, mm, 1) })
  CalDate(y, m, d)
}

fn indian_year_start(year: Int) -> Int {
  let gregorian_year = year + 78
  case is_gregorian_leap(gregorian_year) {
    True -> days_from_civil(gregorian_year, 3, 21)
    False -> days_from_civil(gregorian_year, 3, 22)
  }
}

fn indian_days_in_month(year: Int, month: Int) -> Int {
  case month {
    1 ->
      case is_gregorian_leap(year + 78) {
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

fn indian_to_days(year: Int, month: Int, day: Int) -> Int {
  let first_len = indian_days_in_month(year, 1)
  let offset = case month {
    1 -> 0
    _ ->
      case month <= 6 {
        True -> first_len + 31 * { month - 2 }
        False -> first_len + 31 * 5 + 30 * { month - 7 }
      }
  }
  indian_year_start(year) + offset + day - 1
}

fn indian_from_days(days: Int) -> CalDate {
  let #(gregorian_year, _, _) = civil_from_days(days)
  let y0 = gregorian_year - 78
  let y = case days < indian_year_start(y0) {
    True -> y0 - 1
    False -> y0
  }
  let #(m, d) =
    scan_months(days, y, 1, 12, fn(yy, mm) { indian_to_days(yy, mm, 1) })
  CalDate(y, m, d)
}

const hebrew_epoch = -2_092_590

fn hebrew_is_leap(year: Int) -> Bool {
  floor_mod(7 * year + 1, 19) < 7
}

fn hebrew_months_in_year(year: Int) -> Int {
  case hebrew_is_leap(year) {
    True -> 13
    False -> 12
  }
}

// adar i (M05L) follows month 5 in a leap year
fn hebrew_leap_month(year: Int) -> Int {
  case hebrew_is_leap(year) {
    True -> 5
    False -> 0
  }
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
  case e2 - e1 == 356, e1 - e0 == 382 {
    True, _ -> 2
    False, True -> 1
    False, False -> 0
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
  let new_year = hebrew_new_year(year)
  HebrewYearShape(
    new_year:,
    length: hebrew_new_year(year + 1) - new_year,
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

fn hebrew_to_days(year: Int, month: Int, day: Int) -> Int {
  let shape = hebrew_year_shape(year)
  shape.new_year
  + days_before_month(month, hebrew_shape_days_in_month(shape, _))
  + day
  - 1
}

fn hebrew_from_days(days: Int) -> CalDate {
  let approx = floor_div(98_496 * { days - hebrew_epoch }, 35_975_351) + 1
  let y = adjust_year(days, approx, hebrew_new_year)
  let shape = hebrew_year_shape(y)
  let #(m, d) =
    hebrew_scan_months(days, shape, 1, hebrew_months_in_year(y), shape.new_year)
  CalDate(y, m, d)
}

fn hebrew_scan_months(
  days: Int,
  shape: HebrewYearShape,
  m: Int,
  max: Int,
  start: Int,
) -> #(Int, Int) {
  let next = start + hebrew_shape_days_in_month(shape, m)
  case m < max && days >= next {
    True -> hebrew_scan_months(days, shape, m + 1, max, next)
    False -> #(m, days - start + 1)
  }
}

// metonic fallback: 235 months per 19 years
fn mean_months_before_year(year: Int) -> Int {
  floor_div(235 * year - 234, 19)
}

// mean synodic month = 1447/49 days
fn mean_new_moon_day(months: Int) -> Int {
  floor_div(1447 * months, 49)
}

const lunisolar_first_year = 1700

const lunisolar_last_year = 2300

// packed: bits 0-12 month lengths, 13-16 leap month, 17-22 new year offset
type LunisolarYear {
  Tabulated(month_bits: Int, leap_month: Int, new_year_offset: Int)
  MeanMotion
}

fn lunisolar_year(year_table: YearTable, year: Int) -> LunisolarYear {
  case year_table(year) {
    Some(v) ->
      Tabulated(
        month_bits: int.bitwise_and(v, 0x1fff),
        leap_month: int.bitwise_and(int.bitwise_shift_right(v, 13), 15),
        new_year_offset: int.bitwise_shift_right(v, 17),
      )
    None -> MeanMotion
  }
}

fn lunisolar_leap_month(year_table: YearTable, year: Int) -> Int {
  case lunisolar_year(year_table, year) {
    Tabulated(leap_month:, ..) -> leap_month
    MeanMotion ->
      case mean_months_before_year(year + 1) - mean_months_before_year(year) {
        13 -> 6
        _ -> 0
      }
  }
}

fn lunisolar_months_in_year(year_table: YearTable, year: Int) -> Int {
  months_given_leap_month(lunisolar_leap_month(year_table, year))
}

fn months_given_leap_month(leap_month: Int) -> Int {
  case leap_month {
    0 -> 12
    _ -> 13
  }
}

fn count_bits(n: Int) -> Int {
  case n == 0 {
    True -> 0
    False -> int.bitwise_and(n, 1) + count_bits(int.bitwise_shift_right(n, 1))
  }
}

fn lunisolar_year_length(year_table: YearTable, year: Int) -> Int {
  case lunisolar_year(year_table, year) {
    Tabulated(month_bits:, leap_month:, ..) ->
      29 * months_given_leap_month(leap_month) + count_bits(month_bits)
    MeanMotion ->
      mean_new_moon_day(mean_months_before_year(year + 1))
      - mean_new_moon_day(mean_months_before_year(year))
  }
}

fn lunisolar_year_start(year_table: YearTable, year: Int) -> Int {
  case lunisolar_year(year_table, year) {
    Tabulated(new_year_offset:, ..) ->
      days_from_civil(year, 1, 1) + new_year_offset
    MeanMotion -> {
      let #(edge_days, edge_months) = case year < lunisolar_first_year {
        True -> #(
          lunisolar_year_start(year_table, lunisolar_first_year),
          mean_months_before_year(lunisolar_first_year),
        )
        False -> #(
          lunisolar_year_start(year_table, lunisolar_last_year)
            + lunisolar_year_length(year_table, lunisolar_last_year),
          mean_months_before_year(lunisolar_last_year + 1),
        )
      }
      edge_days
      - mean_new_moon_day(edge_months)
      + mean_new_moon_day(mean_months_before_year(year))
    }
  }
}

fn lunisolar_days_in_month(
  year_table: YearTable,
  year: Int,
  month: Int,
) -> Int {
  case lunisolar_year(year_table, year) {
    Tabulated(month_bits:, ..) ->
      29 + int.bitwise_and(int.bitwise_shift_right(month_bits, month - 1), 1)
    MeanMotion -> {
      let months = mean_months_before_year(year) + month - 1
      mean_new_moon_day(months + 1) - mean_new_moon_day(months)
    }
  }
}

fn lunisolar_to_days(
  year_table: YearTable,
  year: Int,
  month: Int,
  day: Int,
) -> Int {
  lunisolar_year_start(year_table, year)
  + days_before_month(month, lunisolar_days_in_month(year_table, year, _))
  + day
  - 1
}

fn lunisolar_from_days(year_table: YearTable, days: Int) -> CalDate {
  let #(y0, _, _) = civil_from_days(days)
  let y = adjust_year(days, y0, fn(yy) { lunisolar_year_start(year_table, yy) })
  let #(m, d) =
    scan_months(days, y, 1, lunisolar_months_in_year(year_table, y), fn(yy, mm) {
      lunisolar_to_days(year_table, yy, mm, 1)
    })
  CalDate(y, m, d)
}

fn days_before_month(month: Int, month_length: fn(Int) -> Int) -> Int {
  case month <= 1 {
    True -> 0
    False ->
      days_before_month(month - 1, month_length) + month_length(month - 1)
  }
}

fn adjust_year(days: Int, y: Int, year_start: fn(Int) -> Int) -> Int {
  case days < year_start(y) {
    True -> adjust_year(days, y - 1, year_start)
    False ->
      case days >= year_start(y + 1) {
        True -> adjust_year(days, y + 1, year_start)
        False -> y
      }
  }
}

fn scan_months(
  days: Int,
  year: Int,
  m: Int,
  max: Int,
  month_start: fn(Int, Int) -> Int,
) -> #(Int, Int) {
  case m < max && days >= month_start(year, m + 1) {
    True -> scan_months(days, year, m + 1, max, month_start)
    False -> #(m, days - month_start(year, m) + 1)
  }
}

pub fn date_from_epoch_days(cal: Calendar, days: Int) -> CalDate {
  case arithmetic(cal) {
    IsoArith(offset) -> {
      let #(y, m, d) = civil_from_days(days)
      CalDate(y + offset, m, d)
    }
    CopticArith(epoch:, year_shift:) ->
      coptic_from_days(epoch, year_shift, days)
    TabularIslamicArith(epoch) -> islamic_from_days(epoch, days)
    UmalquraArith -> umalqura_from_days(days)
    PersianArith -> persian_from_days(days)
    IndianArith -> indian_from_days(days)
    HebrewArith -> hebrew_from_days(days)
    LunisolarArith(year_table) -> lunisolar_from_days(year_table, days)
  }
}

pub fn date_to_epoch_days(
  cal: Calendar,
  year: Int,
  month: Int,
  day: Int,
) -> Int {
  case arithmetic(cal) {
    IsoArith(offset) -> days_from_civil(year - offset, month, day)
    CopticArith(epoch:, year_shift:) ->
      coptic_to_days(epoch, year_shift, year, month, day)
    TabularIslamicArith(epoch) -> islamic_to_days(epoch, year, month, day)
    UmalquraArith -> umalqura_to_days(year, month, day)
    PersianArith -> persian_to_days(year, month, day)
    IndianArith -> indian_to_days(year, month, day)
    HebrewArith -> hebrew_to_days(year, month, day)
    LunisolarArith(year_table) ->
      lunisolar_to_days(year_table, year, month, day)
  }
}

pub fn months_in_year(cal: Calendar, year: Int) -> Int {
  case arithmetic(cal) {
    CopticArith(..) -> 13
    HebrewArith -> hebrew_months_in_year(year)
    LunisolarArith(year_table) -> lunisolar_months_in_year(year_table, year)
    IsoArith(_)
    | TabularIslamicArith(_)
    | UmalquraArith
    | PersianArith
    | IndianArith -> 12
  }
}

pub fn days_in_month(cal: Calendar, year: Int, month: Int) -> Int {
  case arithmetic(cal) {
    IsoArith(offset) -> gregorian_days_in_month(year - offset, month)
    CopticArith(epoch: _, year_shift:) ->
      coptic_days_in_month(year_shift, year, month)
    TabularIslamicArith(_) -> islamic_days_in_month(year, month)
    UmalquraArith -> umalqura_days_in_month(year, month)
    PersianArith -> persian_days_in_month(year, month)
    IndianArith -> indian_days_in_month(year, month)
    HebrewArith -> hebrew_days_in_month(year, month)
    LunisolarArith(year_table) ->
      lunisolar_days_in_month(year_table, year, month)
  }
}

pub fn days_in_year(cal: Calendar, year: Int) -> Int {
  case arithmetic(cal) {
    HebrewArith -> hebrew_year_length(year)
    IsoArith(_)
    | CopticArith(..)
    | TabularIslamicArith(_)
    | UmalquraArith
    | PersianArith
    | IndianArith
    | LunisolarArith(_) ->
      date_to_epoch_days(cal, year + 1, 1, 1)
      - date_to_epoch_days(cal, year, 1, 1)
  }
}

pub fn in_leap_year(cal: Calendar, year: Int) -> Bool {
  case arithmetic(cal) {
    IsoArith(offset) -> is_gregorian_leap(year - offset)
    CopticArith(epoch: _, year_shift:) -> coptic_is_leap(year_shift, year)
    TabularIslamicArith(_) -> islamic_is_leap(year)
    UmalquraArith -> days_in_year(cal, year) > 354
    PersianArith -> persian_is_leap(year)
    IndianArith -> is_gregorian_leap(year + 78)
    HebrewArith -> hebrew_is_leap(year)
    LunisolarArith(year_table) -> lunisolar_leap_month(year_table, year) != 0
  }
}

pub fn day_of_year(cal: Calendar, year: Int, month: Int, day: Int) -> Int {
  date_to_epoch_days(cal, year, month, day)
  - date_to_epoch_days(cal, year, 1, 1)
  + 1
}

pub type MonthCode {
  MonthCode(number: Int, leap: Bool)
}

fn pad2(n: Int) -> String {
  case n < 10 {
    True -> "0" <> int.to_string(n)
    False -> int.to_string(n)
  }
}

fn month_code_string(mc: MonthCode) -> String {
  case mc.leap {
    True -> "M" <> pad2(mc.number) <> "L"
    False -> "M" <> pad2(mc.number)
  }
}

pub fn month_code(cal: Calendar, year: Int, month: Int) -> String {
  month_code_string(month_code_of(cal, year, month))
}

// 0 if none; M{n}L is ordinal n + 1
fn leap_month_of(cal: Calendar, year: Int) -> Int {
  case arithmetic(cal) {
    HebrewArith -> hebrew_leap_month(year)
    LunisolarArith(year_table) -> lunisolar_leap_month(year_table, year)
    IsoArith(_)
    | CopticArith(..)
    | TabularIslamicArith(_)
    | UmalquraArith
    | PersianArith
    | IndianArith -> 0
  }
}

pub fn month_code_of(cal: Calendar, year: Int, month: Int) -> MonthCode {
  let leap_month = leap_month_of(cal, year)
  case leap_month > 0 && month > leap_month, month == leap_month + 1 {
    True, True -> MonthCode(number: leap_month, leap: True)
    True, False -> MonthCode(number: month - 1, leap: False)
    False, _ -> MonthCode(number: month, leap: False)
  }
}

fn valid_month_number(num: Int, max: Int) -> Result(Int, MonthCodeIssue) {
  case num >= 1 && num <= max {
    True -> Ok(num)
    False -> Error(NeverValid)
  }
}

fn ordinal_past_leap_month(num: Int, leap_month: Int) -> Int {
  case leap_month > 0 && num > leap_month {
    True -> num + 1
    False -> num
  }
}

pub fn month_for_code(
  cal: Calendar,
  year: Int,
  mc: MonthCode,
) -> Result(Int, MonthCodeIssue) {
  let MonthCode(number: num, leap:) = mc
  case arithmetic(cal), leap {
    HebrewArith, True ->
      case num == 5, hebrew_is_leap(year) {
        True, True -> Ok(6)
        True, False -> Error(NotInThisYear(6))
        False, _ -> Error(NeverValid)
      }
    HebrewArith, False -> {
      use num <- result.map(valid_month_number(num, 12))
      ordinal_past_leap_month(num, hebrew_leap_month(year))
    }
    LunisolarArith(year_table), _ -> {
      use num <- result.try(valid_month_number(num, 12))
      let leap_month = lunisolar_leap_month(year_table, year)
      let ordinal = ordinal_past_leap_month(num, leap_month)
      case leap, num == leap_month {
        False, _ -> Ok(ordinal)
        True, True -> Ok(num + 1)
        True, False -> Error(NotInThisYear(ordinal))
      }
    }
    CopticArith(..), False -> valid_month_number(num, 13)
    _, True -> Error(NeverValid)
    _, False -> valid_month_number(num, 12)
  }
}

pub fn carry_month_code(
  cal: Calendar,
  target_year: Int,
  mc: MonthCode,
) -> Result(Int, Int) {
  use issue <- result.map_error(month_for_code(cal, target_year, mc))
  case issue {
    NotInThisYear(constrain_to) -> constrain_to
    // unreachable for a minted code
    NeverValid -> int.min(mc.number, months_in_year(cal, target_year))
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
    as "era_code_for gave a code that eras_of does not list"
  Era(code, era_year(shift, year))
}

fn era_by_sign(year: Int, from_year_one: EraCode, before: EraCode) -> EraCode {
  case year >= 1 {
    True -> from_year_one
    False -> before
  }
}

fn era_code_for(
  cal: Calendar,
  year: Int,
  month: Int,
  day: Int,
) -> Option(EraCode) {
  case cal {
    Iso8601 | Chinese | Dangi -> None
    Gregory -> Some(era_by_sign(year, Ce, Bce))
    Buddhist -> Some(Be)
    Japanese -> Some(japanese_era_code(year, month, day))
    Roc -> Some(era_by_sign(year, Minguo, BeforeMinguo))
    Coptic -> Some(Am)
    Ethiopic -> Some(era_by_sign(year, Am, Aa))
    Ethioaa -> Some(Aa)
    Hebrew -> Some(Am)
    IslamicCivil | IslamicTbla | IslamicUmalqura ->
      Some(era_by_sign(year, Ah, Bh))
    Persian -> Some(Ap)
    Indian -> Some(Shaka)
  }
}

type JapaneseEra {
  JapaneseEra(code: EraCode, year_offset: Int, start: CalDate)
}

// newest first
const japanese_eras = [
  JapaneseEra(code: Reiwa, year_offset: 2018, start: CalDate(2019, 5, 1)),
  JapaneseEra(code: Heisei, year_offset: 1988, start: CalDate(1989, 1, 8)),
  JapaneseEra(code: Showa, year_offset: 1925, start: CalDate(1926, 12, 25)),
  JapaneseEra(code: Taisho, year_offset: 1911, start: CalDate(1912, 7, 30)),
  JapaneseEra(code: Meiji, year_offset: 1867, start: CalDate(1873, 1, 1)),
]

fn japanese_era_code(year: Int, month: Int, day: Int) -> EraCode {
  let started = fn(era: JapaneseEra) {
    let CalDate(y, m, d) = era.start
    year > y || { year == y && { month > m || { month == m && day >= d } } }
  }
  case list.find(japanese_eras, started) {
    Ok(era) -> era.code
    Error(Nil) -> era_by_sign(year, Ce, Bce)
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
    Japanese ->
      list.map(japanese_eras, fn(era) { #(era.code, Forward(era.year_offset)) })
      |> list.append(eras_of(Gregory))
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

pub fn year_for_era(
  cal: Calendar,
  era: EraCode,
  year_of_era: Int,
) -> Result(Int, Nil) {
  use shift <- result.map(list.key_find(eras_of(cal), era))
  case shift {
    Forward(k) -> k + year_of_era
    Backward(k) -> k - year_of_era
  }
}

fn era_year(shift: EraShift, year: Int) -> Int {
  case shift {
    Forward(k) -> year - k
    Backward(k) -> k - year
  }
}
