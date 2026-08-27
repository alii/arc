pub type SurrogateKind {
  High
  Low
  Scalar
}

pub fn classify(cu: Int) -> SurrogateKind {
  case cu {
    _ if cu >= 0xD800 && cu <= 0xDBFF -> High
    _ if cu >= 0xDC00 && cu <= 0xDFFF -> Low
    _ -> Scalar
  }
}

pub fn is_high(cu: Int) -> Bool {
  classify(cu) == High
}

pub fn is_low(cu: Int) -> Bool {
  classify(cu) == Low
}

pub fn is_surrogate(cu: Int) -> Bool {
  classify(cu) != Scalar
}

// §11.1.3, caller already checked is_high and is_low
pub fn combine(high: Int, low: Int) -> Int {
  0x10000 + { high - 0xD800 } * 0x400 + { low - 0xDC00 }
}

// cp must be > 0xffff
pub fn split(cp: Int) -> #(Int, Int) {
  let offset = cp - 0x10000
  #(0xD800 + offset / 0x400, 0xDC00 + offset % 0x400)
}
