pub fn floor_div(a: Int, b: Int) -> Int {
  let q = a / b
  case a % b != 0 && { a < 0 } != { b < 0 } {
    True -> q - 1
    False -> q
  }
}

// sign of divisor
pub fn floor_mod(a: Int, b: Int) -> Int {
  a - floor_div(a, b) * b
}

pub fn trunc_div(a: Int, b: Int) -> Int {
  a / b
}

// sign of dividend
pub fn trunc_mod(a: Int, b: Int) -> Int {
  a - trunc_div(a, b) * b
}
