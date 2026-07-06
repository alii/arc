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

/// Integer division truncating toward zero (Gleam's `/`, named for symmetry
/// with `floor_div` at call sites that mix both flavours).
pub fn trunc_div(a: Int, b: Int) -> Int {
  a / b
}

/// Modulo whose result carries the sign of the dividend. `b` must be non-zero.
pub fn trunc_mod(a: Int, b: Int) -> Int {
  a - trunc_div(a, b) * b
}
