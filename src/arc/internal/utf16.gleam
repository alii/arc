//// UTF-16 surrogate-pair predicates and code-point arithmetic.
////
//// JS strings are sequences of UTF-16 code units, so the parser and the
//// String/JSON/URI/RegExp builtins all need to recognise surrogate pairs and
//// map them to and from Unicode scalar values. Keeping one copy of the
//// combine/split arithmetic here means a future off-by-one has exactly one
//// place to be wrong (and to be tested).
////
//// Terminology follows the ECMAScript spec: a *high* (leading) surrogate is
//// U+D800..U+DBFF, a *low* (trailing) surrogate is U+DC00..U+DFFF.

/// A UTF-16 code unit's role in surrogate-pair encoding: leading half,
/// trailing half, or a non-surrogate BMP scalar. Every code unit is exactly
/// one of these — callers that branch on all three match on `classify`
/// rather than a Bool product with a structurally-dead `(True, True)` cell.
pub type SurrogateKind {
  High
  Low
  Scalar
}

/// The three-way partition of a UTF-16 code unit. Match on this when high,
/// low and scalar are handled distinctly; use the Bool predicates below for
/// single-branch tests (the "is this a low surrogate?" peek after a high).
pub fn classify(cu: Int) -> SurrogateKind {
  case cu {
    _ if cu >= 0xD800 && cu <= 0xDBFF -> High
    _ if cu >= 0xDC00 && cu <= 0xDFFF -> Low
    _ -> Scalar
  }
}

/// True when `cu` is a high (leading) surrogate code unit.
pub fn is_high(cu: Int) -> Bool {
  classify(cu) == High
}

/// True when `cu` is a low (trailing) surrogate code unit.
pub fn is_low(cu: Int) -> Bool {
  classify(cu) == Low
}

/// True when `cu` is any surrogate code unit (high or low).
pub fn is_surrogate(cu: Int) -> Bool {
  classify(cu) != Scalar
}

/// UTF16SurrogatePairToCodePoint (§11.1.3): the supplementary-plane code
/// point encoded by a high/low surrogate pair. Callers must have already
/// established `is_high(high) && is_low(low)`.
pub fn combine(high: Int, low: Int) -> Int {
  0x10000 + { high - 0xD800 } * 0x400 + { low - 0xDC00 }
}

/// CodePointToUTF16CodeUnits for a supplementary-plane code point
/// (`cp > 0xFFFF`): the high/low surrogate pair that encodes it.
pub fn split(cp: Int) -> #(Int, Int) {
  let offset = cp - 0x10000
  #(0xD800 + offset / 0x400, 0xDC00 + offset % 0x400)
}
