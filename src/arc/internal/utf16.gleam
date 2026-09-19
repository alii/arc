pub fn is_high(cu: Int) -> Bool {
  cu >= 0xD800 && cu <= 0xDBFF
}

pub fn is_low(cu: Int) -> Bool {
  cu >= 0xDC00 && cu <= 0xDFFF
}

// §11.1.3, caller already checked is_high and is_low
pub fn combine(high: Int, low: Int) -> Int {
  0x10000 + { high - 0xD800 } * 0x400 + { low - 0xDC00 }
}
