//// Integer floor division/modulo — the pair every calendar and clock module
//// reaches for. Lives here so time-zone / Intl code doesn't import a
//// Gregorian-calendar module just to divide two integers.

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
