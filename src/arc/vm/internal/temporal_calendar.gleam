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

import arc/internal/gregorian.{
  civil_from_days, days_from_civil, days_in_month as gregorian_days_in_month,
  floor_div, floor_mod, is_leap_year as is_gregorian_leap,
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

/// Packed month lengths for AH 1300..1600 (bit 11-(month-1) set → 30 days),
/// from ICU4C islamcal.cpp UMALQURA_MONTHLENGTH. `Error(Nil)` outside the
/// table — a year the ICU data does not cover, where the civil fallback runs.
fn umalqura_month_length(y: Int) -> Result(Int, Nil) {
  case y {
    1300 -> Ok(2730)
    1301 -> Ok(3412)
    1302 -> Ok(3785)
    1303 -> Ok(1748)
    1304 -> Ok(1770)
    1305 -> Ok(876)
    1306 -> Ok(2733)
    1307 -> Ok(1365)
    1308 -> Ok(1705)
    1309 -> Ok(1938)
    1310 -> Ok(2985)
    1311 -> Ok(1492)
    1312 -> Ok(2778)
    1313 -> Ok(1372)
    1314 -> Ok(3373)
    1315 -> Ok(1685)
    1316 -> Ok(1866)
    1317 -> Ok(2900)
    1318 -> Ok(2922)
    1319 -> Ok(1453)
    1320 -> Ok(1198)
    1321 -> Ok(2639)
    1322 -> Ok(1303)
    1323 -> Ok(1675)
    1324 -> Ok(1701)
    1325 -> Ok(2773)
    1326 -> Ok(726)
    1327 -> Ok(2395)
    1328 -> Ok(1181)
    1329 -> Ok(2637)
    1330 -> Ok(3366)
    1331 -> Ok(3477)
    1332 -> Ok(1452)
    1333 -> Ok(2486)
    1334 -> Ok(698)
    1335 -> Ok(2651)
    1336 -> Ok(1323)
    1337 -> Ok(2709)
    1338 -> Ok(1738)
    1339 -> Ok(2793)
    1340 -> Ok(756)
    1341 -> Ok(2422)
    1342 -> Ok(694)
    1343 -> Ok(2390)
    1344 -> Ok(2762)
    1345 -> Ok(2980)
    1346 -> Ok(3026)
    1347 -> Ok(1497)
    1348 -> Ok(732)
    1349 -> Ok(2413)
    1350 -> Ok(1357)
    1351 -> Ok(2725)
    1352 -> Ok(2898)
    1353 -> Ok(2981)
    1354 -> Ok(1460)
    1355 -> Ok(2486)
    1356 -> Ok(1367)
    1357 -> Ok(663)
    1358 -> Ok(1355)
    1359 -> Ok(1699)
    1360 -> Ok(1874)
    1361 -> Ok(2917)
    1362 -> Ok(1386)
    1363 -> Ok(2731)
    1364 -> Ok(1323)
    1365 -> Ok(3221)
    1366 -> Ok(3402)
    1367 -> Ok(3493)
    1368 -> Ok(1482)
    1369 -> Ok(2774)
    1370 -> Ok(2391)
    1371 -> Ok(1195)
    1372 -> Ok(2379)
    1373 -> Ok(2725)
    1374 -> Ok(2898)
    1375 -> Ok(2922)
    1376 -> Ok(1397)
    1377 -> Ok(630)
    1378 -> Ok(2231)
    1379 -> Ok(1115)
    1380 -> Ok(1365)
    1381 -> Ok(1449)
    1382 -> Ok(1460)
    1383 -> Ok(2522)
    1384 -> Ok(1245)
    1385 -> Ok(622)
    1386 -> Ok(2358)
    1387 -> Ok(2730)
    1388 -> Ok(3412)
    1389 -> Ok(3506)
    1390 -> Ok(1493)
    1391 -> Ok(730)
    1392 -> Ok(2395)
    1393 -> Ok(1195)
    1394 -> Ok(2645)
    1395 -> Ok(2889)
    1396 -> Ok(2916)
    1397 -> Ok(2929)
    1398 -> Ok(1460)
    1399 -> Ok(2741)
    1400 -> Ok(2645)
    1401 -> Ok(3365)
    1402 -> Ok(3730)
    1403 -> Ok(3785)
    1404 -> Ok(1748)
    1405 -> Ok(2793)
    1406 -> Ok(2411)
    1407 -> Ok(1195)
    1408 -> Ok(2707)
    1409 -> Ok(3401)
    1410 -> Ok(3492)
    1411 -> Ok(3506)
    1412 -> Ok(2745)
    1413 -> Ok(1210)
    1414 -> Ok(2651)
    1415 -> Ok(1323)
    1416 -> Ok(2709)
    1417 -> Ok(2858)
    1418 -> Ok(2901)
    1419 -> Ok(1372)
    1420 -> Ok(1213)
    1421 -> Ok(573)
    1422 -> Ok(2333)
    1423 -> Ok(2709)
    1424 -> Ok(2890)
    1425 -> Ok(2906)
    1426 -> Ok(1389)
    1427 -> Ok(694)
    1428 -> Ok(2363)
    1429 -> Ok(1179)
    1430 -> Ok(1621)
    1431 -> Ok(1705)
    1432 -> Ok(1876)
    1433 -> Ok(2922)
    1434 -> Ok(1388)
    1435 -> Ok(2733)
    1436 -> Ok(1365)
    1437 -> Ok(2857)
    1438 -> Ok(2962)
    1439 -> Ok(2985)
    1440 -> Ok(1492)
    1441 -> Ok(2778)
    1442 -> Ok(1370)
    1443 -> Ok(2731)
    1444 -> Ok(1429)
    1445 -> Ok(1865)
    1446 -> Ok(1892)
    1447 -> Ok(2986)
    1448 -> Ok(1461)
    1449 -> Ok(694)
    1450 -> Ok(2646)
    1451 -> Ok(3661)
    1452 -> Ok(2853)
    1453 -> Ok(2898)
    1454 -> Ok(2922)
    1455 -> Ok(1453)
    1456 -> Ok(686)
    1457 -> Ok(2351)
    1458 -> Ok(1175)
    1459 -> Ok(1611)
    1460 -> Ok(1701)
    1461 -> Ok(1708)
    1462 -> Ok(2774)
    1463 -> Ok(1373)
    1464 -> Ok(1181)
    1465 -> Ok(2637)
    1466 -> Ok(3350)
    1467 -> Ok(3477)
    1468 -> Ok(1450)
    1469 -> Ok(1461)
    1470 -> Ok(730)
    1471 -> Ok(2395)
    1472 -> Ok(1197)
    1473 -> Ok(1429)
    1474 -> Ok(1738)
    1475 -> Ok(1764)
    1476 -> Ok(2794)
    1477 -> Ok(1269)
    1478 -> Ok(694)
    1479 -> Ok(2390)
    1480 -> Ok(2730)
    1481 -> Ok(2900)
    1482 -> Ok(3026)
    1483 -> Ok(1497)
    1484 -> Ok(746)
    1485 -> Ok(2413)
    1486 -> Ok(1197)
    1487 -> Ok(2709)
    1488 -> Ok(2890)
    1489 -> Ok(2981)
    1490 -> Ok(1458)
    1491 -> Ok(2485)
    1492 -> Ok(1238)
    1493 -> Ok(2711)
    1494 -> Ok(1351)
    1495 -> Ok(1683)
    1496 -> Ok(1865)
    1497 -> Ok(2901)
    1498 -> Ok(1386)
    1499 -> Ok(2667)
    1500 -> Ok(1323)
    1501 -> Ok(2699)
    1502 -> Ok(3398)
    1503 -> Ok(3491)
    1504 -> Ok(1482)
    1505 -> Ok(2774)
    1506 -> Ok(1243)
    1507 -> Ok(619)
    1508 -> Ok(2379)
    1509 -> Ok(2725)
    1510 -> Ok(2898)
    1511 -> Ok(2921)
    1512 -> Ok(1397)
    1513 -> Ok(374)
    1514 -> Ok(2231)
    1515 -> Ok(603)
    1516 -> Ok(1323)
    1517 -> Ok(1381)
    1518 -> Ok(1460)
    1519 -> Ok(2522)
    1520 -> Ok(1261)
    1521 -> Ok(365)
    1522 -> Ok(2230)
    1523 -> Ok(2726)
    1524 -> Ok(3410)
    1525 -> Ok(3497)
    1526 -> Ok(1492)
    1527 -> Ok(2778)
    1528 -> Ok(2395)
    1529 -> Ok(1195)
    1530 -> Ok(1619)
    1531 -> Ok(1833)
    1532 -> Ok(1890)
    1533 -> Ok(2985)
    1534 -> Ok(1458)
    1535 -> Ok(2741)
    1536 -> Ok(1365)
    1537 -> Ok(2853)
    1538 -> Ok(3474)
    1539 -> Ok(3785)
    1540 -> Ok(1746)
    1541 -> Ok(2793)
    1542 -> Ok(1387)
    1543 -> Ok(1195)
    1544 -> Ok(2645)
    1545 -> Ok(3369)
    1546 -> Ok(3412)
    1547 -> Ok(3498)
    1548 -> Ok(2485)
    1549 -> Ok(1210)
    1550 -> Ok(2619)
    1551 -> Ok(1179)
    1552 -> Ok(2637)
    1553 -> Ok(2730)
    1554 -> Ok(2773)
    1555 -> Ok(730)
    1556 -> Ok(2397)
    1557 -> Ok(1118)
    1558 -> Ok(2606)
    1559 -> Ok(3226)
    1560 -> Ok(3413)
    1561 -> Ok(1714)
    1562 -> Ok(1721)
    1563 -> Ok(1210)
    1564 -> Ok(2653)
    1565 -> Ok(1325)
    1566 -> Ok(2709)
    1567 -> Ok(2898)
    1568 -> Ok(2984)
    1569 -> Ok(2996)
    1570 -> Ok(1465)
    1571 -> Ok(730)
    1572 -> Ok(2394)
    1573 -> Ok(2890)
    1574 -> Ok(3492)
    1575 -> Ok(3793)
    1576 -> Ok(1768)
    1577 -> Ok(2922)
    1578 -> Ok(1389)
    1579 -> Ok(1333)
    1580 -> Ok(1685)
    1581 -> Ok(3402)
    1582 -> Ok(3496)
    1583 -> Ok(3540)
    1584 -> Ok(1754)
    1585 -> Ok(1371)
    1586 -> Ok(669)
    1587 -> Ok(1579)
    1588 -> Ok(2837)
    1589 -> Ok(2890)
    1590 -> Ok(2965)
    1591 -> Ok(1450)
    1592 -> Ok(2734)
    1593 -> Ok(2350)
    1594 -> Ok(3215)
    1595 -> Ok(1319)
    1596 -> Ok(1685)
    1597 -> Ok(1706)
    1598 -> Ok(2774)
    1599 -> Ok(1373)
    1600 -> Ok(669)
    _ -> Error(Nil)
  }
}

/// Corrections to the linear year-start estimate for AH 1300..1600, from
/// ICU4C islamcal.cpp umAlQuraYrStartEstimateFix. `Error(Nil)` outside the
/// table, exactly like `umalqura_month_length`.
fn umalqura_year_start_fix(y: Int) -> Result(Int, Nil) {
  case y {
    1300 -> Ok(0)
    1301 -> Ok(0)
    1302 -> Ok(-1)
    1303 -> Ok(0)
    1304 -> Ok(-1)
    1305 -> Ok(0)
    1306 -> Ok(0)
    1307 -> Ok(0)
    1308 -> Ok(0)
    1309 -> Ok(0)
    1310 -> Ok(-1)
    1311 -> Ok(0)
    1312 -> Ok(0)
    1313 -> Ok(0)
    1314 -> Ok(0)
    1315 -> Ok(0)
    1316 -> Ok(0)
    1317 -> Ok(0)
    1318 -> Ok(-1)
    1319 -> Ok(0)
    1320 -> Ok(1)
    1321 -> Ok(0)
    1322 -> Ok(1)
    1323 -> Ok(1)
    1324 -> Ok(0)
    1325 -> Ok(0)
    1326 -> Ok(0)
    1327 -> Ok(0)
    1328 -> Ok(1)
    1329 -> Ok(0)
    1330 -> Ok(0)
    1331 -> Ok(0)
    1332 -> Ok(0)
    1333 -> Ok(0)
    1334 -> Ok(0)
    1335 -> Ok(0)
    1336 -> Ok(1)
    1337 -> Ok(0)
    1338 -> Ok(0)
    1339 -> Ok(0)
    1340 -> Ok(0)
    1341 -> Ok(0)
    1342 -> Ok(1)
    1343 -> Ok(0)
    1344 -> Ok(0)
    1345 -> Ok(-1)
    1346 -> Ok(-1)
    1347 -> Ok(0)
    1348 -> Ok(0)
    1349 -> Ok(0)
    1350 -> Ok(1)
    1351 -> Ok(0)
    1352 -> Ok(0)
    1353 -> Ok(-1)
    1354 -> Ok(0)
    1355 -> Ok(0)
    1356 -> Ok(0)
    1357 -> Ok(1)
    1358 -> Ok(1)
    1359 -> Ok(0)
    1360 -> Ok(0)
    1361 -> Ok(0)
    1362 -> Ok(0)
    1363 -> Ok(0)
    1364 -> Ok(0)
    1365 -> Ok(0)
    1366 -> Ok(0)
    1367 -> Ok(-1)
    1368 -> Ok(0)
    1369 -> Ok(0)
    1370 -> Ok(0)
    1371 -> Ok(1)
    1372 -> Ok(1)
    1373 -> Ok(0)
    1374 -> Ok(0)
    1375 -> Ok(-1)
    1376 -> Ok(0)
    1377 -> Ok(1)
    1378 -> Ok(0)
    1379 -> Ok(1)
    1380 -> Ok(1)
    1381 -> Ok(0)
    1382 -> Ok(0)
    1383 -> Ok(-1)
    1384 -> Ok(0)
    1385 -> Ok(1)
    1386 -> Ok(0)
    1387 -> Ok(0)
    1388 -> Ok(0)
    1389 -> Ok(-1)
    1390 -> Ok(0)
    1391 -> Ok(1)
    1392 -> Ok(0)
    1393 -> Ok(1)
    1394 -> Ok(0)
    1395 -> Ok(0)
    1396 -> Ok(0)
    1397 -> Ok(-1)
    1398 -> Ok(0)
    1399 -> Ok(0)
    1400 -> Ok(0)
    1401 -> Ok(0)
    1402 -> Ok(-1)
    1403 -> Ok(-1)
    1404 -> Ok(0)
    1405 -> Ok(-1)
    1406 -> Ok(0)
    1407 -> Ok(1)
    1408 -> Ok(0)
    1409 -> Ok(0)
    1410 -> Ok(0)
    1411 -> Ok(-1)
    1412 -> Ok(0)
    1413 -> Ok(0)
    1414 -> Ok(0)
    1415 -> Ok(1)
    1416 -> Ok(0)
    1417 -> Ok(0)
    1418 -> Ok(0)
    1419 -> Ok(0)
    1420 -> Ok(0)
    1421 -> Ok(1)
    1422 -> Ok(0)
    1423 -> Ok(0)
    1424 -> Ok(-1)
    1425 -> Ok(-1)
    1426 -> Ok(0)
    1427 -> Ok(0)
    1428 -> Ok(0)
    1429 -> Ok(1)
    1430 -> Ok(0)
    1431 -> Ok(0)
    1432 -> Ok(-1)
    1433 -> Ok(-1)
    1434 -> Ok(0)
    1435 -> Ok(-1)
    1436 -> Ok(0)
    1437 -> Ok(0)
    1438 -> Ok(-1)
    1439 -> Ok(-1)
    1440 -> Ok(0)
    1441 -> Ok(-1)
    1442 -> Ok(0)
    1443 -> Ok(-1)
    1444 -> Ok(0)
    1445 -> Ok(0)
    1446 -> Ok(-1)
    1447 -> Ok(-1)
    1448 -> Ok(0)
    1449 -> Ok(0)
    1450 -> Ok(0)
    1451 -> Ok(0)
    1452 -> Ok(0)
    1453 -> Ok(0)
    1454 -> Ok(-1)
    1455 -> Ok(0)
    1456 -> Ok(1)
    1457 -> Ok(0)
    1458 -> Ok(1)
    1459 -> Ok(1)
    1460 -> Ok(0)
    1461 -> Ok(0)
    1462 -> Ok(-1)
    1463 -> Ok(0)
    1464 -> Ok(1)
    1465 -> Ok(0)
    1466 -> Ok(0)
    1467 -> Ok(0)
    1468 -> Ok(0)
    1469 -> Ok(0)
    1470 -> Ok(1)
    1471 -> Ok(0)
    1472 -> Ok(1)
    1473 -> Ok(0)
    1474 -> Ok(0)
    1475 -> Ok(0)
    1476 -> Ok(-1)
    1477 -> Ok(0)
    1478 -> Ok(1)
    1479 -> Ok(0)
    1480 -> Ok(0)
    1481 -> Ok(-1)
    1482 -> Ok(-1)
    1483 -> Ok(0)
    1484 -> Ok(0)
    1485 -> Ok(0)
    1486 -> Ok(1)
    1487 -> Ok(0)
    1488 -> Ok(0)
    1489 -> Ok(0)
    1490 -> Ok(0)
    1491 -> Ok(0)
    1492 -> Ok(0)
    1493 -> Ok(0)
    1494 -> Ok(1)
    1495 -> Ok(0)
    1496 -> Ok(0)
    1497 -> Ok(0)
    1498 -> Ok(0)
    1499 -> Ok(0)
    1500 -> Ok(1)
    1501 -> Ok(0)
    1502 -> Ok(0)
    1503 -> Ok(-1)
    1504 -> Ok(0)
    1505 -> Ok(0)
    1506 -> Ok(0)
    1507 -> Ok(1)
    1508 -> Ok(1)
    1509 -> Ok(0)
    1510 -> Ok(0)
    1511 -> Ok(-1)
    1512 -> Ok(0)
    1513 -> Ok(1)
    1514 -> Ok(0)
    1515 -> Ok(1)
    1516 -> Ok(1)
    1517 -> Ok(0)
    1518 -> Ok(0)
    1519 -> Ok(0)
    1520 -> Ok(0)
    1521 -> Ok(1)
    1522 -> Ok(0)
    1523 -> Ok(0)
    1524 -> Ok(0)
    1525 -> Ok(-1)
    1526 -> Ok(0)
    1527 -> Ok(0)
    1528 -> Ok(0)
    1529 -> Ok(1)
    1530 -> Ok(0)
    1531 -> Ok(0)
    1532 -> Ok(0)
    1533 -> Ok(-1)
    1534 -> Ok(0)
    1535 -> Ok(0)
    1536 -> Ok(0)
    1537 -> Ok(0)
    1538 -> Ok(0)
    1539 -> Ok(-1)
    1540 -> Ok(0)
    1541 -> Ok(-1)
    1542 -> Ok(0)
    1543 -> Ok(1)
    1544 -> Ok(0)
    1545 -> Ok(0)
    1546 -> Ok(0)
    1547 -> Ok(-1)
    1548 -> Ok(0)
    1549 -> Ok(1)
    1550 -> Ok(0)
    1551 -> Ok(1)
    1552 -> Ok(0)
    1553 -> Ok(0)
    1554 -> Ok(0)
    1555 -> Ok(0)
    1556 -> Ok(0)
    1557 -> Ok(1)
    1558 -> Ok(0)
    1559 -> Ok(0)
    1560 -> Ok(-1)
    1561 -> Ok(0)
    1562 -> Ok(0)
    1563 -> Ok(0)
    1564 -> Ok(0)
    1565 -> Ok(1)
    1566 -> Ok(0)
    1567 -> Ok(0)
    1568 -> Ok(0)
    1569 -> Ok(-1)
    1570 -> Ok(0)
    1571 -> Ok(0)
    1572 -> Ok(0)
    1573 -> Ok(0)
    1574 -> Ok(-1)
    1575 -> Ok(-1)
    1576 -> Ok(0)
    1577 -> Ok(-1)
    1578 -> Ok(0)
    1579 -> Ok(1)
    1580 -> Ok(0)
    1581 -> Ok(0)
    1582 -> Ok(-1)
    1583 -> Ok(-1)
    1584 -> Ok(0)
    1585 -> Ok(0)
    1586 -> Ok(1)
    1587 -> Ok(1)
    1588 -> Ok(0)
    1589 -> Ok(0)
    1590 -> Ok(-1)
    1591 -> Ok(0)
    1592 -> Ok(0)
    1593 -> Ok(0)
    1594 -> Ok(0)
    1595 -> Ok(1)
    1596 -> Ok(0)
    1597 -> Ok(0)
    1598 -> Ok(0)
    1599 -> Ok(0)
    1600 -> Ok(1)
    _ -> Error(Nil)
  }
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
  use code <- option.then(era_code_for(cal, year, month, day))
  use shift <- option.map(option.from_result(list.key_find(eras_of(cal), code)))
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

fn chinese_data(y: Int) -> Result(Int, Nil) {
  case y {
    1700 -> Ok(6_425_893)
    1701 -> Ok(4_983_381)
    1702 -> Ok(3_593_389)
    1703 -> Ok(6_030_518)
    1704 -> Ok(4_589_237)
    1705 -> Ok(3_181_994)
    1706 -> Ok(5_639_881)
    1707 -> Ok(4_329_106)
    1708 -> Ok(2_915_621)
    1709 -> Ok(5_246_246)
    1710 -> Ok(3_861_078)
    1711 -> Ok(6_163_035)
    1712 -> Ok(4_851_034)
    1713 -> Ok(3_320_533)
    1714 -> Ok(5_769_045)
    1715 -> Ok(4_458_314)
    1716 -> Ok(3_042_963)
    1717 -> Ok(5_375_635)
    1718 -> Ok(4_003_115)
    1719 -> Ok(6_423_851)
    1720 -> Ok(4_983_467)
    1721 -> Ok(3_593_562)
    1722 -> Ok(6_030_698)
    1723 -> Ok(4_590_437)
    1724 -> Ok(3_315_530)
    1725 -> Ok(5_638_986)
    1726 -> Ok(4_197_013)
    1727 -> Ok(2_774_315)
    1728 -> Ok(5_244_237)
    1729 -> Ok(3_730_093)
    1730 -> Ok(6_163_125)
    1731 -> Ok(4_851_122)
    1732 -> Ok(3_451_813)
    1733 -> Ok(5_770_661)
    1734 -> Ok(4_459_850)
    1735 -> Ok(3_054_869)
    1736 -> Ok(5_508_246)
    1737 -> Ok(4_012_374)
    1738 -> Ok(6_423_894)
    1739 -> Ok(4_983_509)
    1740 -> Ok(3_724_722)
    1741 -> Ok(6_031_058)
    1742 -> Ok(4_591_269)
    1743 -> Ok(3_313_290)
    1744 -> Ok(5_637_771)
    1745 -> Ok(4_066_455)
    1746 -> Ok(2_779_478)
    1747 -> Ok(5_113_179)
    1748 -> Ok(3_861_210)
    1749 -> Ok(6_163_306)
    1750 -> Ok(4_851_538)
    1751 -> Ok(3_454_757)
    1752 -> Ok(5_901_125)
    1753 -> Ok(4_328_075)
    1754 -> Ok(2_921_771)
    1755 -> Ok(5_375_149)
    1756 -> Ok(4_008_299)
    1757 -> Ok(6_292_917)
    1758 -> Ok(4_983_722)
    1759 -> Ok(3_726_164)
    1760 -> Ok(6_163_874)
    1761 -> Ok(4_590_917)
    1762 -> Ok(3_193_485)
    1763 -> Ok(5_638_805)
    1764 -> Ok(4_195_501)
    1765 -> Ok(2_640_301)
    1766 -> Ok(5_113_525)
    1767 -> Ok(3_861_930)
    1768 -> Ok(6_295_242)
    1769 -> Ok(4_853_410)
    1770 -> Ok(3_456_326)
    1771 -> Ok(5_901_642)
    1772 -> Ok(4_459_158)
    1773 -> Ok(2_913_590)
    1774 -> Ok(5_375_322)
    1775 -> Ok(4_016_853)
    1776 -> Ok(6_425_445)
    1777 -> Ok(4_982_610)
    1778 -> Ok(3_591_845)
    1779 -> Ok(6_031_013)
    1780 -> Ok(4_588_875)
    1781 -> Ok(3_058_327)
    1782 -> Ok(5_507_755)
    1783 -> Ok(4_195_674)
    1784 -> Ok(2_779_861)
    1785 -> Ok(5_114_725)
    1786 -> Ok(3_864_402)
    1787 -> Ok(6_294_866)
    1788 -> Ok(4_852_501)
    1789 -> Ok(3_323_467)
    1790 -> Ok(5_768_525)
    1791 -> Ok(4_328_109)
    1792 -> Ok(3_052_906)
    1793 -> Ok(5_375_410)
    1794 -> Ok(3_935_657)
    1795 -> Ok(2_645_330)
    1796 -> Ok(5_115_282)
    1797 -> Ok(3_595_541)
    1798 -> Ok(6_032_678)
    1799 -> Ok(4_589_910)
    1800 -> Ok(3_181_229)
    1801 -> Ok(5_638_870)
    1802 -> Ok(4_326_868)
    1803 -> Ok(2_903_465)
    1804 -> Ok(5_377_737)
    1805 -> Ok(3_993_226)
    1806 -> Ok(6_293_131)
    1807 -> Ok(4_853_031)
    1808 -> Ok(3_582_294)
    1809 -> Ok(5_769_563)
    1810 -> Ok(4_459_226)
    1811 -> Ok(3_176_148)
    1812 -> Ok(5_637_972)
    1813 -> Ok(4_065_097)
    1814 -> Ok(2_643_595)
    1815 -> Ok(5_114_515)
    1816 -> Ok(3_724_587)
    1817 -> Ok(6_030_637)
    1818 -> Ok(4_589_933)
    1819 -> Ok(3_312_490)
    1820 -> Ok(5_770_666)
    1821 -> Ok(4_328_356)
    1822 -> Ok(2_915_141)
    1823 -> Ok(5_377_353)
    1824 -> Ok(3_996_309)
    1825 -> Ok(6_294_165)
    1826 -> Ok(4_850_989)
    1827 -> Ok(3_451_565)
    1828 -> Ok(5_900_981)
    1829 -> Ok(4_459_946)
    1830 -> Ok(3_186_084)
    1831 -> Ok(5_639_844)
    1832 -> Ok(4_275_530)
    1833 -> Ok(6_557_002)
    1834 -> Ok(5_114_518)
    1835 -> Ok(3_724_598)
    1836 -> Ok(6_161_754)
    1837 -> Ok(4_590_293)
    1838 -> Ok(3_315_402)
    1839 -> Ok(5_769_042)
    1840 -> Ok(4_329_125)
    1841 -> Ok(2_911_562)
    1842 -> Ok(5_244_491)
    1843 -> Ok(3_861_143)
    1844 -> Ok(6_294_187)
    1845 -> Ok(4_851_034)
    1846 -> Ok(3_451_733)
    1847 -> Ok(5_901_225)
    1848 -> Ok(4_589_394)
    1849 -> Ok(3_054_373)
    1850 -> Ok(5_507_877)
    1851 -> Ok(4_135_499)
    1852 -> Ok(6_555_981)
    1853 -> Ok(4_983_469)
    1854 -> Ok(3_732_842)
    1855 -> Ok(6_161_844)
    1856 -> Ok(4_722_089)
    1857 -> Ok(3_325_266)
    1858 -> Ok(5_770_642)
    1859 -> Ok(4_328_741)
    1860 -> Ok(2_914_893)
    1861 -> Ok(5_245_526)
    1862 -> Ok(3_871_413)
    1863 -> Ok(6_294_230)
    1864 -> Ok(4_982_484)
    1865 -> Ok(3_452_329)
    1866 -> Ok(5_902_025)
    1867 -> Ok(4_591_250)
    1868 -> Ok(3_181_862)
    1869 -> Ok(5_375_275)
    1870 -> Ok(4_016_727)
    1871 -> Ok(6_424_923)
    1872 -> Ok(5_114_714)
    1873 -> Ok(3_725_012)
    1874 -> Ok(6_162_260)
    1875 -> Ok(4_720_457)
    1876 -> Ok(3_323_539)
    1877 -> Ok(5_638_803)
    1878 -> Ok(4_195_627)
    1879 -> Ok(2_779_739)
    1880 -> Ok(5_245_549)
    1881 -> Ok(3_861_354)
    1882 -> Ok(6_294_954)
    1883 -> Ok(4_983_716)
    1884 -> Ok(3_586_889)
    1885 -> Ok(5_901_641)
    1886 -> Ok(4_459_157)
    1887 -> Ok(3_052_845)
    1888 -> Ok(5_506_349)
    1889 -> Ok(3_934_893)
    1890 -> Ok(2_643_306)
    1891 -> Ok(5_115_306)
    1892 -> Ok(3_857_828)
    1893 -> Ok(6_164_132)
    1894 -> Ok(4_721_994)
    1895 -> Ok(3_320_469)
    1896 -> Ok(5_638_807)
    1897 -> Ok(4_195_670)
    1898 -> Ok(2_779_829)
    1899 -> Ok(5_245_653)
    1900 -> Ok(4_003_538)
    1901 -> Ok(6_424_402)
    1902 -> Ok(4_984_485)
    1903 -> Ok(3_716_682)
    1904 -> Ok(6_030_923)
    1905 -> Ok(4_459_163)
    1906 -> Ok(3_183_962)
    1907 -> Ok(5_637_482)
    1908 -> Ok(4_197_209)
    1909 -> Ok(2_774_866)
    1910 -> Ok(5_244_754)
    1911 -> Ok(3_857_189)
    1912 -> Ok(6_294_309)
    1913 -> Ok(4_721_227)
    1914 -> Ok(3_323_051)
    1915 -> Ok(5_767_853)
    1916 -> Ok(4_326_763)
    1917 -> Ok(2_902_889)
    1918 -> Ok(5_377_449)
    1919 -> Ok(4_128_146)
    1920 -> Ok(6_557_330)
    1921 -> Ok(4_984_101)
    1922 -> Ok(3_586_637)
    1923 -> Ok(6_031_958)
    1924 -> Ok(4_588_214)
    1925 -> Ok(3_052_981)
    1926 -> Ok(5_637_844)
    1927 -> Ok(4_198_057)
    1928 -> Ok(2_907_794)
    1929 -> Ok(5_246_610)
    1930 -> Ok(3_853_606)
    1931 -> Ok(6_161_707)
    1932 -> Ok(4_721_239)
    1933 -> Ok(3_322_550)
    1934 -> Ok(5_770_074)
    1935 -> Ok(4_458_196)
    1936 -> Ok(3_043_017)
    1937 -> Ok(5_375_817)
    1938 -> Ok(3_995_283)
    1939 -> Ok(6_425_235)
    1940 -> Ok(4_982_059)
    1941 -> Ok(3_459_675)
    1942 -> Ok(5_900_973)
    1943 -> Ok(4_588_906)
    1944 -> Ok(3_185_493)
    1945 -> Ok(5_639_076)
    1946 -> Ok(4_197_193)
    1947 -> Ok(2_775_699)
    1948 -> Ok(5_245_589)
    1949 -> Ok(3_732_781)
    1950 -> Ok(6_161_718)
    1951 -> Ok(4_721_325)
    1952 -> Ok(3_454_378)
    1953 -> Ok(5_768_626)
    1954 -> Ok(4_328_869)
    1955 -> Ok(3_046_730)
    1956 -> Ok(5_508_426)
    1957 -> Ok(4_000_405)
    1958 -> Ok(6_294_167)
    1959 -> Ok(4_982_102)
    1960 -> Ok(3_590_837)
    1961 -> Ok(5_901_013)
    1962 -> Ok(4_589_266)
    1963 -> Ok(3_182_245)
    1964 -> Ok(5_639_845)
    1965 -> Ok(4_195_914)
    1966 -> Ok(2_649_239)
    1967 -> Ok(5_114_523)
    1968 -> Ok(3_863_898)
    1969 -> Ok(6_161_770)
    1970 -> Ok(4_721_513)
    1971 -> Ok(3_454_802)
    1972 -> Ok(5_901_138)
    1973 -> Ok(4_328_229)
    1974 -> Ok(2_922_059)
    1975 -> Ok(5_376_587)
    1976 -> Ok(4_002_987)
    1977 -> Ok(6_292_141)
    1978 -> Ok(4_851_053)
    1979 -> Ok(3_591_017)
    1980 -> Ok(6_032_809)
    1981 -> Ok(4_590_994)
    1982 -> Ok(3_185_957)
    1983 -> Ok(5_639_461)
    1984 -> Ok(4_282_957)
    1985 -> Ok(6_556_246)
    1986 -> Ok(5_112_502)
    1987 -> Ok(3_720_629)
    1988 -> Ok(6_162_133)
    1989 -> Ok(4_722_345)
    1990 -> Ok(3_456_658)
    1991 -> Ok(5_901_970)
    1992 -> Ok(4_459_814)
    1993 -> Ok(2_910_806)
    1994 -> Ok(5_245_527)
    1995 -> Ok(4_003_030)
    1996 -> Ok(6_423_386)
    1997 -> Ok(4_851_413)
    1998 -> Ok(3_585_737)
    1999 -> Ok(6_031_177)
    2000 -> Ok(4_589_203)
    2001 -> Ok(3_052_843)
    2002 -> Ok(5_506_347)
    2003 -> Ok(4_065_883)
    2004 -> Ok(2_774_362)
    2005 -> Ok(5_113_194)
    2006 -> Ok(3_734_357)
    2007 -> Ok(6_294_436)
    2008 -> Ok(4_852_553)
    2009 -> Ok(3_324_563)
    2010 -> Ok(5_769_877)
    2011 -> Ok(4_326_701)
    2012 -> Ok(2_919_085)
    2013 -> Ok(5_245_621)
    2014 -> Ok(4_011_434)
    2015 -> Ok(6_424_018)
    2016 -> Ok(4_984_229)
    2017 -> Ok(3_595_594)
    2018 -> Ok(6_032_714)
    2019 -> Ok(4_590_741)
    2020 -> Ok(3_183_918)
    2021 -> Ok(5_506_390)
    2022 -> Ok(4_065_973)
    2023 -> Ok(2_774_450)
    2024 -> Ok(5_244_626)
    2025 -> Ok(3_722_917)
    2026 -> Ok(6_162_213)
    2027 -> Ok(4_720_203)
    2028 -> Ok(3_320_983)
    2029 -> Ok(5_639_339)
    2030 -> Ok(4_326_746)
    2031 -> Ok(2_910_934)
    2032 -> Ok(5_376_873)
    2033 -> Ok(4_028_242)
    2034 -> Ok(6_425_426)
    2035 -> Ok(4_983_589)
    2036 -> Ok(3_594_827)
    2037 -> Ok(5_900_875)
    2038 -> Ok(4_457_643)
    2039 -> Ok(3_056_987)
    2040 -> Ok(5_506_477)
    2041 -> Ok(4_066_154)
    2042 -> Ok(2_775_890)
    2043 -> Ok(5_246_354)
    2044 -> Ok(3_865_893)
    2045 -> Ok(6_163_749)
    2046 -> Ok(4_721_237)
    2047 -> Ok(3_323_053)
    2048 -> Ok(5_768_374)
    2049 -> Ok(4_195_765)
    2050 -> Ok(2_911_658)
    2051 -> Ok(5_377_737)
    2052 -> Ok(4_136_594)
    2053 -> Ok(6_426_258)
    2054 -> Ok(4_984_102)
    2055 -> Ok(3_590_742)
    2056 -> Ok(5_900_887)
    2057 -> Ok(4_457_814)
    2058 -> Ok(3_049_173)
    2059 -> Ok(5_506_901)
    2060 -> Ok(4_196_169)
    2061 -> Ok(2_649_747)
    2062 -> Ok(5_113_491)
    2063 -> Ok(3_732_779)
    2064 -> Ok(6_161_707)
    2065 -> Ok(4_590_171)
    2066 -> Ok(3_323_226)
    2067 -> Ok(5_768_554)
    2068 -> Ok(4_328_293)
    2069 -> Ok(2_922_314)
    2070 -> Ok(5_376_842)
    2071 -> Ok(4_004_501)
    2072 -> Ok(6_425_237)
    2073 -> Ok(4_850_989)
    2074 -> Ok(3_459_757)
    2075 -> Ok(5_900_981)
    2076 -> Ok(4_588_970)
    2077 -> Ok(3_050_405)
    2078 -> Ok(5_508_517)
    2079 -> Ok(4_197_706)
    2080 -> Ok(2_784_405)
    2081 -> Ok(5_115_030)
    2082 -> Ok(3_733_838)
    2083 -> Ok(6_161_750)
    2084 -> Ok(4_721_333)
    2085 -> Ok(3_323_314)
    2086 -> Ok(5_768_914)
    2087 -> Ok(4_329_125)
    2088 -> Ok(3_051_082)
    2089 -> Ok(5_244_555)
    2090 -> Ok(3_869_847)
    2091 -> Ok(6_292_651)
    2092 -> Ok(4_851_035)
    2093 -> Ok(3_459_798)
    2094 -> Ok(5_901_162)
    2095 -> Ok(4_589_394)
    2096 -> Ok(3_184_421)
    2097 -> Ok(5_507_909)
    2098 -> Ok(4_065_931)
    2099 -> Ok(2_643_099)
    2100 -> Ok(5_113_003)
    2101 -> Ok(3_729_755)
    2102 -> Ok(6_161_837)
    2103 -> Ok(4_852_650)
    2104 -> Ok(3_586_898)
    2105 -> Ok(5_901_714)
    2106 -> Ok(4_459_813)
    2107 -> Ok(3_054_155)
    2108 -> Ok(5_507_669)
    2109 -> Ok(4_011_181)
    2110 -> Ok(6_423_734)
    2111 -> Ok(4_982_453)
    2112 -> Ok(3_722_666)
    2113 -> Ok(6_033_097)
    2114 -> Ok(4_722_322)
    2115 -> Ok(3_317_030)
    2116 -> Ok(5_770_538)
    2117 -> Ok(4_196_950)
    2118 -> Ok(2_782_390)
    2119 -> Ok(5_244_246)
    2120 -> Ok(3_861_205)
    2121 -> Ok(6_163_285)
    2122 -> Ok(4_851_530)
    2123 -> Ok(3_452_563)
    2124 -> Ok(5_899_925)
    2125 -> Ok(4_326_699)
    2126 -> Ok(2_918_999)
    2127 -> Ok(5_376_667)
    2128 -> Ok(4_158_810)
    2129 -> Ok(6_423_914)
    2130 -> Ok(4_983_653)
    2131 -> Ok(3_725_130)
    2132 -> Ok(6_163_274)
    2133 -> Ok(4_590_357)
    2134 -> Ok(3_192_107)
    2135 -> Ok(5_637_453)
    2136 -> Ok(4_197_037)
    2137 -> Ok(2_774_378)
    2138 -> Ok(5_244_330)
    2139 -> Ok(3_861_413)
    2140 -> Ok(6_294_949)
    2141 -> Ok(4_853_066)
    2142 -> Ok(3_456_277)
    2143 -> Ok(5_901_590)
    2144 -> Ok(4_458_830)
    2145 -> Ok(2_919_085)
    2146 -> Ok(5_376_726)
    2147 -> Ok(4_158_900)
    2148 -> Ok(6_555_346)
    2149 -> Ok(4_984_485)
    2150 -> Ok(3_722_890)
    2151 -> Ok(6_030_987)
    2152 -> Ok(4_590_871)
    2153 -> Ok(3_189_078)
    2154 -> Ok(5_507_419)
    2155 -> Ok(4_197_082)
    2156 -> Ok(2_914_004)
    2157 -> Ok(5_244_756)
    2158 -> Ok(3_864_389)
    2159 -> Ok(6_294_341)
    2160 -> Ok(4_852_363)
    2161 -> Ok(3_331_371)
    2162 -> Ok(5_768_365)
    2163 -> Ok(4_327_787)
    2164 -> Ok(3_050_330)
    2165 -> Ok(5_377_450)
    2166 -> Ok(4_152_148)
    2167 -> Ok(6_557_090)
    2168 -> Ok(5_115_205)
    2169 -> Ok(3_594_901)
    2170 -> Ok(6_032_021)
    2171 -> Ok(4_588_845)
    2172 -> Ok(3_189_421)
    2173 -> Ok(5_507_765)
    2174 -> Ok(4_197_802)
    2175 -> Ok(2_915_748)
    2176 -> Ok(5_377_698)
    2177 -> Ok(3_865_926)
    2178 -> Ok(6_294_858)
    2179 -> Ok(4_852_374)
    2180 -> Ok(3_462_454)
    2181 -> Ok(5_768_538)
    2182 -> Ok(4_328_149)
    2183 -> Ok(3_053_258)
    2184 -> Ok(5_506_898)
    2185 -> Ok(3_935_909)
    2186 -> Ok(2_641_226)
    2187 -> Ok(4_982_091)
    2188 -> Ok(3_590_807)
    2189 -> Ok(5_900_971)
    2190 -> Ok(4_588_890)
    2191 -> Ok(3_189_461)
    2192 -> Ok(5_639_013)
    2193 -> Ok(4_196_178)
    2194 -> Ok(2_783_909)
    2195 -> Ok(5_245_733)
    2196 -> Ok(3_865_163)
    2197 -> Ok(6_162_765)
    2198 -> Ok(4_721_325)
    2199 -> Ok(3_462_506)
    2200 -> Ok(5_899_700)
    2201 -> Ok(4_459_433)
    2202 -> Ok(3_185_490)
    2203 -> Ok(5_639_570)
    2204 -> Ok(4_275_493)
    2205 -> Ok(6_556_966)
    2206 -> Ok(5_114_198)
    2207 -> Ok(3_723_949)
    2208 -> Ok(6_163_158)
    2209 -> Ok(4_720_340)
    2210 -> Ok(3_313_065)
    2211 -> Ok(5_770_953)
    2212 -> Ok(4_460_178)
    2213 -> Ok(2_911_526)
    2214 -> Ok(5_244_199)
    2215 -> Ok(3_861_079)
    2216 -> Ok(6_293_851)
    2217 -> Ok(4_852_442)
    2218 -> Ok(3_585_748)
    2219 -> Ok(6_031_188)
    2220 -> Ok(4_589_385)
    2221 -> Ok(3_053_203)
    2222 -> Ok(5_507_731)
    2223 -> Ok(4_142_379)
    2224 -> Ok(6_554_925)
    2225 -> Ok(4_983_149)
    2226 -> Ok(3_730_282)
    2227 -> Ok(6_163_882)
    2228 -> Ok(4_852_644)
    2229 -> Ok(3_324_745)
    2230 -> Ok(5_770_569)
    2231 -> Ok(4_328_085)
    2232 -> Ok(2_913_579)
    2233 -> Ok(5_244_205)
    2234 -> Ok(3_869_357)
    2235 -> Ok(6_294_197)
    2236 -> Ok(4_984_234)
    2237 -> Ok(3_587_492)
    2238 -> Ok(6_033_060)
    2239 -> Ok(4_590_922)
    2240 -> Ok(3_185_301)
    2241 -> Ok(5_507_734)
    2242 -> Ok(4_158_774)
    2243 -> Ok(6_554_970)
    2244 -> Ok(5_114_581)
    2245 -> Ok(3_725_010)
    2246 -> Ok(6_162_258)
    2247 -> Ok(4_722_341)
    2248 -> Ok(3_454_538)
    2249 -> Ok(5_637_707)
    2250 -> Ok(4_197_015)
    2251 -> Ok(2_913_622)
    2252 -> Ok(5_375_322)
    2253 -> Ok(3_861_333)
    2254 -> Ok(6_294_441)
    2255 -> Ok(4_982_610)
    2256 -> Ok(3_595_045)
    2257 -> Ok(5_901_093)
    2258 -> Ok(4_459_083)
    2259 -> Ok(3_060_891)
    2260 -> Ok(5_505_709)
    2261 -> Ok(3_933_547)
    2262 -> Ok(2_632_553)
    2263 -> Ok(5_115_305)
    2264 -> Ok(3_865_938)
    2265 -> Ok(6_163_858)
    2266 -> Ok(4_721_957)
    2267 -> Ok(3_324_493)
    2268 -> Ok(5_769_814)
    2269 -> Ok(4_194_997)
    2270 -> Ok(2_782_637)
    2271 -> Ok(5_375_700)
    2272 -> Ok(4_001_193)
    2273 -> Ok(6_295_241)
    2274 -> Ok(4_984_466)
    2275 -> Ok(3_591_462)
    2276 -> Ok(5_899_559)
    2277 -> Ok(4_328_023)
    2278 -> Ok(3_052_214)
    2279 -> Ok(5_507_930)
    2280 -> Ok(4_196_052)
    2281 -> Ok(2_641_577)
    2282 -> Ok(5_113_673)
    2283 -> Ok(3_724_947)
    2284 -> Ok(6_163_091)
    2285 -> Ok(4_588_843)
    2286 -> Ok(3_189_339)
    2287 -> Ok(5_638_765)
    2288 -> Ok(4_326_762)
    2289 -> Ok(2_784_085)
    2290 -> Ok(5_376_932)
    2291 -> Ok(3_996_489)
    2292 -> Ok(6_425_929)
    2293 -> Ok(4_852_373)
    2294 -> Ok(3_462_445)
    2295 -> Ok(5_899_566)
    2296 -> Ok(4_459_181)
    2297 -> Ok(3_052_906)
    2298 -> Ok(5_506_482)
    2299 -> Ok(4_066_725)
    2300 -> Ok(2_776_394)
    _ -> Error(Nil)
  }
}

fn dangi_data(y: Int) -> Result(Int, Nil) {
  case y {
    1700 -> Ok(6_425_893)
    1701 -> Ok(4_983_381)
    1702 -> Ok(3_593_389)
    1703 -> Ok(6_030_518)
    1704 -> Ok(4_588_981)
    1705 -> Ok(3_181_994)
    1706 -> Ok(5_639_881)
    1707 -> Ok(4_329_106)
    1708 -> Ok(2_915_621)
    1709 -> Ok(5_246_246)
    1710 -> Ok(3_861_078)
    1711 -> Ok(6_163_035)
    1712 -> Ok(4_850_902)
    1713 -> Ok(3_319_509)
    1714 -> Ok(5_769_045)
    1715 -> Ok(4_458_313)
    1716 -> Ok(3_042_963)
    1717 -> Ok(5_375_635)
    1718 -> Ok(4_003_115)
    1719 -> Ok(6_423_851)
    1720 -> Ok(4_983_387)
    1721 -> Ok(3_593_562)
    1722 -> Ok(6_030_698)
    1723 -> Ok(4_590_437)
    1724 -> Ok(3_315_530)
    1725 -> Ok(5_638_985)
    1726 -> Ok(4_197_013)
    1727 -> Ok(2_774_315)
    1728 -> Ok(5_244_205)
    1729 -> Ok(3_730_093)
    1730 -> Ok(6_163_125)
    1731 -> Ok(4_851_114)
    1732 -> Ok(3_451_813)
    1733 -> Ok(5_770_661)
    1734 -> Ok(4_459_850)
    1735 -> Ok(3_046_549)
    1736 -> Ok(5_508_246)
    1737 -> Ok(4_012_366)
    1738 -> Ok(6_423_894)
    1739 -> Ok(4_983_477)
    1740 -> Ok(3_724_722)
    1741 -> Ok(6_031_058)
    1742 -> Ok(4_591_269)
    1743 -> Ok(3_313_226)
    1744 -> Ok(5_637_771)
    1745 -> Ok(4_066_455)
    1746 -> Ok(2_779_478)
    1747 -> Ok(5_113_179)
    1748 -> Ok(3_861_206)
    1749 -> Ok(6_163_305)
    1750 -> Ok(4_851_538)
    1751 -> Ok(3_454_757)
    1752 -> Ok(5_901_125)
    1753 -> Ok(4_328_075)
    1754 -> Ok(2_921_643)
    1755 -> Ok(5_375_147)
    1756 -> Ok(4_008_299)
    1757 -> Ok(6_292_909)
    1758 -> Ok(4_983_722)
    1759 -> Ok(3_726_162)
    1760 -> Ok(6_163_858)
    1761 -> Ok(4_590_917)
    1762 -> Ok(3_193_483)
    1763 -> Ok(5_638_741)
    1764 -> Ok(4_195_501)
    1765 -> Ok(2_640_237)
    1766 -> Ok(5_113_525)
    1767 -> Ok(3_861_930)
    1768 -> Ok(6_295_241)
    1769 -> Ok(4_853_394)
    1770 -> Ok(3_456_293)
    1771 -> Ok(5_901_610)
    1772 -> Ok(4_459_094)
    1773 -> Ok(2_913_462)
    1774 -> Ok(5_375_322)
    1775 -> Ok(4_016_853)
    1776 -> Ok(6_424_421)
    1777 -> Ok(4_982_602)
    1778 -> Ok(3_591_827)
    1779 -> Ok(6_030_997)
    1780 -> Ok(4_588_843)
    1781 -> Ok(3_058_327)
    1782 -> Ok(5_507_755)
    1783 -> Ok(4_195_674)
    1784 -> Ok(2_779_861)
    1785 -> Ok(5_114_725)
    1786 -> Ok(3_864_394)
    1787 -> Ok(6_294_346)
    1788 -> Ok(4_852_373)
    1789 -> Ok(3_331_371)
    1790 -> Ok(5_768_525)
    1791 -> Ok(4_328_109)
    1792 -> Ok(3_052_906)
    1793 -> Ok(5_375_410)
    1794 -> Ok(3_935_141)
    1795 -> Ok(2_644_810)
    1796 -> Ok(5_115_210)
    1797 -> Ok(3_595_541)
    1798 -> Ok(6_032_662)
    1799 -> Ok(4_589_910)
    1800 -> Ok(3_181_229)
    1801 -> Ok(5_638_870)
    1802 -> Ok(4_326_868)
    1803 -> Ok(2_911_653)
    1804 -> Ok(5_377_701)
    1805 -> Ok(3_985_034)
    1806 -> Ok(6_293_131)
    1807 -> Ok(4_853_031)
    1808 -> Ok(3_582_294)
    1809 -> Ok(5_768_539)
    1810 -> Ok(4_459_226)
    1811 -> Ok(3_176_148)
    1812 -> Ok(5_637_972)
    1813 -> Ok(4_065_093)
    1814 -> Ok(2_643_595)
    1815 -> Ok(5_114_507)
    1816 -> Ok(3_724_587)
    1817 -> Ok(6_030_509)
    1818 -> Ok(4_589_931)
    1819 -> Ok(3_312_490)
    1820 -> Ok(5_770_154)
    1821 -> Ok(4_328_340)
    1822 -> Ok(2_915_141)
    1823 -> Ok(5_377_349)
    1824 -> Ok(3_996_309)
    1825 -> Ok(6_294_165)
    1826 -> Ok(4_850_989)
    1827 -> Ok(3_451_309)
    1828 -> Ok(5_899_957)
    1829 -> Ok(4_459_946)
    1830 -> Ok(3_186_084)
    1831 -> Ok(5_639_842)
    1832 -> Ok(4_275_526)
    1833 -> Ok(6_557_002)
    1834 -> Ok(5_114_518)
    1835 -> Ok(3_724_598)
    1836 -> Ok(6_161_754)
    1837 -> Ok(4_590_293)
    1838 -> Ok(3_315_402)
    1839 -> Ok(5_769_042)
    1840 -> Ok(4_329_125)
    1841 -> Ok(2_911_562)
    1842 -> Ok(5_244_235)
    1843 -> Ok(3_861_143)
    1844 -> Ok(6_294_187)
    1845 -> Ok(4_851_034)
    1846 -> Ok(3_451_605)
    1847 -> Ok(5_901_161)
    1848 -> Ok(4_589_394)
    1849 -> Ok(3_053_221)
    1850 -> Ok(5_507_877)
    1851 -> Ok(4_159_051)
    1852 -> Ok(6_554_957)
    1853 -> Ok(4_983_469)
    1854 -> Ok(3_732_842)
    1855 -> Ok(6_161_844)
    1856 -> Ok(4_721_577)
    1857 -> Ok(3_324_754)
    1858 -> Ok(5_770_642)
    1859 -> Ok(4_328_741)
    1860 -> Ok(2_914_893)
    1861 -> Ok(5_245_270)
    1862 -> Ok(3_869_365)
    1863 -> Ok(6_294_230)
    1864 -> Ok(4_982_484)
    1865 -> Ok(3_452_329)
    1866 -> Ok(5_902_025)
    1867 -> Ok(4_591_250)
    1868 -> Ok(3_181_862)
    1869 -> Ok(5_377_319)
    1870 -> Ok(4_147_542)
    1871 -> Ok(6_424_923)
    1872 -> Ok(5_114_714)
    1873 -> Ok(3_725_012)
    1874 -> Ok(6_162_260)
    1875 -> Ok(4_720_457)
    1876 -> Ok(3_323_539)
    1877 -> Ok(5_638_803)
    1878 -> Ok(4_195_627)
    1879 -> Ok(2_779_739)
    1880 -> Ok(5_245_293)
    1881 -> Ok(3_861_354)
    1882 -> Ok(6_294_442)
    1883 -> Ok(4_983_716)
    1884 -> Ok(3_586_889)
    1885 -> Ok(5_901_641)
    1886 -> Ok(4_459_157)
    1887 -> Ok(3_052_843)
    1888 -> Ok(5_506_349)
    1889 -> Ok(4_033_197)
    1890 -> Ok(6_425_269)
    1891 -> Ok(5_115_306)
    1892 -> Ok(3_857_828)
    1893 -> Ok(6_164_132)
    1894 -> Ok(4_721_994)
    1895 -> Ok(3_324_565)
    1896 -> Ok(5_769_878)
    1897 -> Ok(4_195_638)
    1898 -> Ok(2_779_829)
    1899 -> Ok(5_245_653)
    1900 -> Ok(4_003_538)
    1901 -> Ok(6_424_402)
    1902 -> Ok(4_984_485)
    1903 -> Ok(3_714_634)
    1904 -> Ok(6_030_667)
    1905 -> Ok(4_459_159)
    1906 -> Ok(3_183_958)
    1907 -> Ok(5_637_482)
    1908 -> Ok(4_197_205)
    1909 -> Ok(2_774_866)
    1910 -> Ok(5_244_754)
    1911 -> Ok(3_856_165)
    1912 -> Ok(6_294_309)
    1913 -> Ok(4_721_227)
    1914 -> Ok(3_322_523)
    1915 -> Ok(5_769_901)
    1916 -> Ok(4_457_834)
    1917 -> Ok(2_902_889)
    1918 -> Ok(5_376_937)
    1919 -> Ok(4_127_570)
    1920 -> Ok(6_557_074)
    1921 -> Ok(4_984_101)
    1922 -> Ok(3_586_637)
    1923 -> Ok(6_031_702)
    1924 -> Ok(4_588_213)
    1925 -> Ok(3_052_973)
    1926 -> Ok(5_637_844)
    1927 -> Ok(4_197_801)
    1928 -> Ok(2_907_538)
    1929 -> Ok(5_246_610)
    1930 -> Ok(3_853_606)
    1931 -> Ok(6_161_703)
    1932 -> Ok(4_721_239)
    1933 -> Ok(3_322_550)
    1934 -> Ok(5_769_946)
    1935 -> Ok(4_458_196)
    1936 -> Ok(3_042_985)
    1937 -> Ok(5_375_817)
    1938 -> Ok(3_995_283)
    1939 -> Ok(6_425_235)
    1940 -> Ok(4_982_059)
    1941 -> Ok(3_459_675)
    1942 -> Ok(5_900_653)
    1943 -> Ok(4_590_442)
    1944 -> Ok(3_316_564)
    1945 -> Ok(5_639_076)
    1946 -> Ok(4_197_193)
    1947 -> Ok(2_775_699)
    1948 -> Ok(5_245_589)
    1949 -> Ok(3_732_779)
    1950 -> Ok(6_161_709)
    1951 -> Ok(4_721_325)
    1952 -> Ok(3_454_314)
    1953 -> Ok(5_770_674)
    1954 -> Ok(4_459_940)
    1955 -> Ok(3_046_729)
    1956 -> Ok(5_508_426)
    1957 -> Ok(4_004_501)
    1958 -> Ok(6_425_238)
    1959 -> Ok(4_982_102)
    1960 -> Ok(3_590_837)
    1961 -> Ok(5_901_013)
    1962 -> Ok(4_589_266)
    1963 -> Ok(3_182_245)
    1964 -> Ok(5_639_845)
    1965 -> Ok(4_197_962)
    1966 -> Ok(2_780_310)
    1967 -> Ok(5_114_523)
    1968 -> Ok(3_863_894)
    1969 -> Ok(6_161_770)
    1970 -> Ok(4_721_497)
    1971 -> Ok(3_454_802)
    1972 -> Ok(5_900_114)
    1973 -> Ok(4_327_205)
    1974 -> Ok(2_922_059)
    1975 -> Ok(5_376_587)
    1976 -> Ok(4_002_475)
    1977 -> Ok(6_292_141)
    1978 -> Ok(4_851_051)
    1979 -> Ok(3_591_017)
    1980 -> Ok(6_032_809)
    1981 -> Ok(4_590_994)
    1982 -> Ok(3_185_445)
    1983 -> Ok(5_639_461)
    1984 -> Ok(4_282_957)
    1985 -> Ok(6_556_246)
    1986 -> Ok(5_112_502)
    1987 -> Ok(3_724_717)
    1988 -> Ok(6_293_204)
    1989 -> Ok(4_722_089)
    1990 -> Ok(3_456_402)
    1991 -> Ok(5_901_970)
    1992 -> Ok(4_459_814)
    1993 -> Ok(2_910_806)
    1994 -> Ok(5_245_527)
    1995 -> Ok(4_002_486)
    1996 -> Ok(6_425_434)
    1997 -> Ok(4_982_484)
    1998 -> Ok(3_583_689)
    1999 -> Ok(6_031_177)
    2000 -> Ok(4_589_203)
    2001 -> Ok(3_052_839)
    2002 -> Ok(5_506_347)
    2003 -> Ok(4_065_883)
    2004 -> Ok(2_774_362)
    2005 -> Ok(5_112_682)
    2006 -> Ok(3_734_357)
    2007 -> Ok(6_294_436)
    2008 -> Ok(4_852_553)
    2009 -> Ok(3_324_563)
    2010 -> Ok(5_769_877)
    2011 -> Ok(4_326_701)
    2012 -> Ok(2_910_813)
    2013 -> Ok(5_245_613)
    2014 -> Ok(4_011_434)
    2015 -> Ok(6_424_018)
    2016 -> Ok(4_984_229)
    2017 -> Ok(3_587_402)
    2018 -> Ok(6_032_714)
    2019 -> Ok(4_590_229)
    2020 -> Ok(3_183_917)
    2021 -> Ok(5_506_390)
    2022 -> Ok(4_065_973)
    2023 -> Ok(2_774_442)
    2024 -> Ok(5_244_626)
    2025 -> Ok(3_722_917)
    2026 -> Ok(6_164_133)
    2027 -> Ok(4_853_322)
    2028 -> Ok(3_452_054)
    2029 -> Ok(5_639_323)
    2030 -> Ok(4_326_746)
    2031 -> Ok(2_910_933)
    2032 -> Ok(5_376_873)
    2033 -> Ok(4_028_242)
    2034 -> Ok(6_424_402)
    2035 -> Ok(4_983_589)
    2036 -> Ok(3_593_803)
    2037 -> Ok(5_900_875)
    2038 -> Ok(4_457_643)
    2039 -> Ok(3_056_987)
    2040 -> Ok(5_506_413)
    2041 -> Ok(4_066_153)
    2042 -> Ok(2_775_890)
    2043 -> Ok(5_246_354)
    2044 -> Ok(3_865_893)
    2045 -> Ok(6_163_749)
    2046 -> Ok(4_721_229)
    2047 -> Ok(3_323_053)
    2048 -> Ok(5_767_862)
    2049 -> Ok(4_195_765)
    2050 -> Ok(2_911_657)
    2051 -> Ok(5_377_705)
    2052 -> Ok(4_136_338)
    2053 -> Ok(6_426_258)
    2054 -> Ok(4_984_102)
    2055 -> Ok(3_590_742)
    2056 -> Ok(5_900_887)
    2057 -> Ok(4_457_686)
    2058 -> Ok(3_049_141)
    2059 -> Ok(5_506_773)
    2060 -> Ok(4_198_089)
    2061 -> Ok(2_780_818)
    2062 -> Ok(5_113_491)
    2063 -> Ok(3_732_779)
    2064 -> Ok(6_161_707)
    2065 -> Ok(4_590_171)
    2066 -> Ok(3_323_226)
    2067 -> Ok(5_768_554)
    2068 -> Ok(4_328_277)
    2069 -> Ok(2_922_313)
    2070 -> Ok(5_376_841)
    2071 -> Ok(4_004_499)
    2072 -> Ok(6_425_237)
    2073 -> Ok(4_850_989)
    2074 -> Ok(3_459_757)
    2075 -> Ok(5_900_981)
    2076 -> Ok(4_588_970)
    2077 -> Ok(3_050_405)
    2078 -> Ok(5_508_517)
    2079 -> Ok(4_197_706)
    2080 -> Ok(2_783_893)
    2081 -> Ok(5_115_029)
    2082 -> Ok(3_732_782)
    2083 -> Ok(6_161_750)
    2084 -> Ok(4_721_333)
    2085 -> Ok(3_323_314)
    2086 -> Ok(5_768_914)
    2087 -> Ok(4_329_125)
    2088 -> Ok(3_055_178)
    2089 -> Ok(5_375_562)
    2090 -> Ok(3_869_847)
    2091 -> Ok(6_294_699)
    2092 -> Ok(4_982_106)
    2093 -> Ok(3_459_797)
    2094 -> Ok(5_901_161)
    2095 -> Ok(4_589_394)
    2096 -> Ok(3_184_293)
    2097 -> Ok(5_507_877)
    2098 -> Ok(4_064_843)
    2099 -> Ok(2_651_287)
    2100 -> Ok(5_113_003)
    2101 -> Ok(3_728_731)
    2102 -> Ok(6_161_837)
    2103 -> Ok(4_852_585)
    2104 -> Ok(3_586_898)
    2105 -> Ok(5_901_714)
    2106 -> Ok(4_459_813)
    2107 -> Ok(3_054_155)
    2108 -> Ok(5_507_669)
    2109 -> Ok(4_011_181)
    2110 -> Ok(6_423_734)
    2111 -> Ok(4_982_197)
    2112 -> Ok(3_722_666)
    2113 -> Ok(6_033_097)
    2114 -> Ok(4_722_322)
    2115 -> Ok(3_317_029)
    2116 -> Ok(5_770_534)
    2117 -> Ok(4_196_950)
    2118 -> Ok(2_782_382)
    2119 -> Ok(5_244_118)
    2120 -> Ok(3_861_205)
    2121 -> Ok(6_162_133)
    2122 -> Ok(4_851_401)
    2123 -> Ok(3_452_563)
    2124 -> Ok(5_899_923)
    2125 -> Ok(4_326_699)
    2126 -> Ok(2_918_999)
    2127 -> Ok(5_376_603)
    2128 -> Ok(4_142_426)
    2129 -> Ok(6_423_914)
    2130 -> Ok(4_983_653)
    2131 -> Ok(3_725_130)
    2132 -> Ok(6_163_273)
    2133 -> Ok(4_590_229)
    2134 -> Ok(3_192_107)
    2135 -> Ok(5_637_421)
    2136 -> Ok(4_197_037)
    2137 -> Ok(2_774_378)
    2138 -> Ok(5_244_330)
    2139 -> Ok(3_861_413)
    2140 -> Ok(6_294_949)
    2141 -> Ok(4_853_066)
    2142 -> Ok(3_456_149)
    2143 -> Ok(5_901_462)
    2144 -> Ok(4_458_830)
    2145 -> Ok(2_919_085)
    2146 -> Ok(5_376_693)
    2147 -> Ok(4_158_898)
    2148 -> Ok(6_555_346)
    2149 -> Ok(4_984_485)
    2150 -> Ok(3_726_922)
    2151 -> Ok(6_162_058)
    2152 -> Ok(4_590_743)
    2153 -> Ok(3_189_078)
    2154 -> Ok(5_506_395)
    2155 -> Ok(4_197_078)
    2156 -> Ok(2_914_004)
    2157 -> Ok(5_244_754)
    2158 -> Ok(3_864_357)
    2159 -> Ok(6_294_341)
    2160 -> Ok(4_852_363)
    2161 -> Ok(3_331_227)
    2162 -> Ok(5_768_363)
    2163 -> Ok(4_327_771)
    2164 -> Ok(3_050_330)
    2165 -> Ok(5_376_938)
    2166 -> Ok(4_152_146)
    2167 -> Ok(6_557_074)
    2168 -> Ok(5_115_205)
    2169 -> Ok(3_594_827)
    2170 -> Ok(6_031_957)
    2171 -> Ok(4_588_717)
    2172 -> Ok(3_180_909)
    2173 -> Ok(5_506_741)
    2174 -> Ok(4_197_802)
    2175 -> Ok(2_915_730)
    2176 -> Ok(5_377_698)
    2177 -> Ok(3_865_925)
    2178 -> Ok(6_294_826)
    2179 -> Ok(4_852_310)
    2180 -> Ok(3_462_454)
    2181 -> Ok(5_768_534)
    2182 -> Ok(4_328_149)
    2183 -> Ok(3_053_226)
    2184 -> Ok(5_506_890)
    2185 -> Ok(3_935_907)
    2186 -> Ok(2_641_194)
    2187 -> Ok(4_982_059)
    2188 -> Ok(3_598_935)
    2189 -> Ok(5_900_955)
    2190 -> Ok(4_588_890)
    2191 -> Ok(3_189_461)
    2192 -> Ok(5_639_013)
    2193 -> Ok(4_196_170)
    2194 -> Ok(2_782_869)
    2195 -> Ok(5_245_589)
    2196 -> Ok(3_863_851)
    2197 -> Ok(6_161_741)
    2198 -> Ok(4_721_325)
    2199 -> Ok(3_462_506)
    2200 -> Ok(5_899_690)
    2201 -> Ok(4_459_429)
    2202 -> Ok(3_185_482)
    2203 -> Ok(5_639_562)
    2204 -> Ok(4_275_477)
    2205 -> Ok(6_556_950)
    2206 -> Ok(5_114_198)
    2207 -> Ok(3_721_901)
    2208 -> Ok(6_163_158)
    2209 -> Ok(4_720_052)
    2210 -> Ok(3_313_065)
    2211 -> Ok(5_770_917)
    2212 -> Ok(4_460_170)
    2213 -> Ok(2_911_510)
    2214 -> Ok(5_246_247)
    2215 -> Ok(3_991_894)
    2216 -> Ok(6_293_851)
    2217 -> Ok(4_852_442)
    2218 -> Ok(3_585_748)
    2219 -> Ok(6_031_188)
    2220 -> Ok(4_589_381)
    2221 -> Ok(3_053_195)
    2222 -> Ok(5_507_723)
    2223 -> Ok(4_142_379)
    2224 -> Ok(6_554_797)
    2225 -> Ok(4_983_147)
    2226 -> Ok(3_730_266)
    2227 -> Ok(6_163_370)
    2228 -> Ok(4_852_564)
    2229 -> Ok(3_324_741)
    2230 -> Ok(5_770_565)
    2231 -> Ok(4_328_085)
    2232 -> Ok(2_913_579)
    2233 -> Ok(5_244_205)
    2234 -> Ok(3_869_357)
    2235 -> Ok(6_293_173)
    2236 -> Ok(4_984_234)
    2237 -> Ok(3_587_492)
    2238 -> Ok(6_033_060)
    2239 -> Ok(4_590_918)
    2240 -> Ok(3_185_301)
    2241 -> Ok(5_507_734)
    2242 -> Ok(4_158_774)
    2243 -> Ok(6_554_970)
    2244 -> Ok(5_114_581)
    2245 -> Ok(3_725_002)
    2246 -> Ok(6_162_258)
    2247 -> Ok(4_722_341)
    2248 -> Ok(3_452_234)
    2249 -> Ok(5_637_451)
    2250 -> Ok(4_197_015)
    2251 -> Ok(2_913_622)
    2252 -> Ok(5_375_322)
    2253 -> Ok(3_869_397)
    2254 -> Ok(6_294_377)
    2255 -> Ok(4_982_610)
    2256 -> Ok(3_593_893)
    2257 -> Ok(5_901_093)
    2258 -> Ok(4_459_083)
    2259 -> Ok(3_052_187)
    2260 -> Ok(5_507_757)
    2261 -> Ok(4_064_618)
    2262 -> Ok(2_632_553)
    2263 -> Ok(5_114_793)
    2264 -> Ok(3_857_234)
    2265 -> Ok(6_163_858)
    2266 -> Ok(4_721_957)
    2267 -> Ok(3_324_493)
    2268 -> Ok(5_769_558)
    2269 -> Ok(4_194_989)
    2270 -> Ok(2_782_637)
    2271 -> Ok(5_375_700)
    2272 -> Ok(4_001_193)
    2273 -> Ok(6_295_241)
    2274 -> Ok(4_984_466)
    2275 -> Ok(3_591_462)
    2276 -> Ok(5_899_559)
    2277 -> Ok(4_328_023)
    2278 -> Ok(3_052_214)
    2279 -> Ok(5_507_802)
    2280 -> Ok(4_196_052)
    2281 -> Ok(2_641_577)
    2282 -> Ok(5_113_673)
    2283 -> Ok(3_724_947)
    2284 -> Ok(6_163_091)
    2285 -> Ok(4_588_843)
    2286 -> Ok(3_189_339)
    2287 -> Ok(5_638_507)
    2288 -> Ok(4_328_298)
    2289 -> Ok(2_915_156)
    2290 -> Ok(5_376_932)
    2291 -> Ok(3_996_489)
    2292 -> Ok(6_425_929)
    2293 -> Ok(4_852_373)
    2294 -> Ok(3_462_443)
    2295 -> Ok(5_899_565)
    2296 -> Ok(4_459_181)
    2297 -> Ok(3_052_906)
    2298 -> Ok(5_508_522)
    2299 -> Ok(4_197_796)
    2300 -> Ok(2_776_393)
    _ -> Error(Nil)
  }
}
