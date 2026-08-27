import arc/rt/types.{
  JFloat, JInt, JNan, JNegInf, JPosInf, JsCell, KBig, KBool, KHandle, KNull,
  KNum, KStr, KSym, KTdz, KUndef, SymIterator, UserSymbol, WellKnownSymbol,
  classify, mk_bigint, mk_bool, mk_null, mk_number, mk_object, mk_string,
  mk_symbol, mk_tdz, mk_undefined,
}
import gleam/option.{None, Some}

pub fn undefined_round_trips_test() {
  assert classify(mk_undefined()) == KUndef
}

pub fn null_round_trips_test() {
  assert classify(mk_null()) == KNull
}

pub fn bool_true_round_trips_test() {
  assert classify(mk_bool(True)) == KBool(True)
}

pub fn bool_false_round_trips_test() {
  assert classify(mk_bool(False)) == KBool(False)
}

pub fn number_jint_round_trips_test() {
  assert classify(mk_number(JInt(42))) == KNum(JInt(42))
}

pub fn number_jint_zero_round_trips_test() {
  assert classify(mk_number(JInt(0))) == KNum(JInt(0))
}

pub fn number_jint_negative_round_trips_test() {
  assert classify(mk_number(JInt(-17))) == KNum(JInt(-17))
}

pub fn number_jfloat_round_trips_test() {
  assert classify(mk_number(JFloat(3.5))) == KNum(JFloat(3.5))
}

pub fn number_jfloat_negative_round_trips_test() {
  assert classify(mk_number(JFloat(-0.25))) == KNum(JFloat(-0.25))
}

pub fn number_jnan_round_trips_test() {
  assert classify(mk_number(JNan)) == KNum(JNan)
}

pub fn number_jposinf_round_trips_test() {
  assert classify(mk_number(JPosInf)) == KNum(JPosInf)
}

pub fn number_jneginf_round_trips_test() {
  assert classify(mk_number(JNegInf)) == KNum(JNegInf)
}

pub fn string_round_trips_test() {
  assert classify(mk_string("hello")) == KStr("hello")
}

pub fn string_empty_round_trips_test() {
  assert classify(mk_string("")) == KStr("")
}

pub fn string_utf8_round_trips_test() {
  assert classify(mk_string("naïve — 🎉")) == KStr("naïve — 🎉")
}

pub fn bigint_round_trips_test() {
  assert classify(mk_bigint(7)) == KBig(7)
}

pub fn bigint_large_round_trips_test() {
  let big = 9_007_199_254_740_993
  assert classify(mk_bigint(big)) == KBig(big)
}

pub fn symbol_user_round_trips_test() {
  let sym = UserSymbol(uid: 3, description: Some("tag"))
  assert classify(mk_symbol(sym)) == KSym(sym)
}

pub fn symbol_user_no_description_round_trips_test() {
  let sym = UserSymbol(uid: 0, description: None)
  assert classify(mk_symbol(sym)) == KSym(sym)
}

pub fn symbol_well_known_round_trips_test() {
  let sym = WellKnownSymbol(SymIterator)
  assert classify(mk_symbol(sym)) == KSym(sym)
}

pub fn handle_round_trips_test() {
  let h = JsCell(id: 5)
  assert classify(mk_object(h)) == KHandle(h)
}

pub fn handle_zero_round_trips_test() {
  let h = JsCell(id: 0)
  assert classify(mk_object(h)) == KHandle(h)
}

pub fn tdz_round_trips_test() {
  assert classify(mk_tdz()) == KTdz
}

pub fn bigint_is_not_number_test() {
  assert classify(mk_bigint(1)) == KBig(1)
  assert classify(mk_number(JInt(1))) == KNum(JInt(1))
}

pub fn bool_is_not_undefined_or_null_test() {
  let assert KBool(_) = classify(mk_bool(False))
  let assert KUndef = classify(mk_undefined())
  let assert KNull = classify(mk_null())
}
