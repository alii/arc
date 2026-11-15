import arc/bytecode/key.{Named}
import arc/engine.{Returned}
import gleam/int
import gleam/list

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// these names differ but are filed under one number, and nothing a script can
// see may depend on that
pub fn these_names_do_share_a_number_test() {
  let assert Named(id: a, ..) = key.named("ksjeg0Fdv")
  let assert Named(id: b, ..) = key.named("blgsn3988")
  assert a == b
  let assert Named(id: c, ..) = key.named("adnpu4BaA")
  let assert Named(id: d, ..) = key.named("cheao1x9S")
  assert c == d
}

const prelude =
  "
  var out = [];
  function attempt(f) { try { out.push(JSON.stringify(f())); } catch (e) { out.push(e.constructor.name); } }
"

pub fn one_name_never_answers_for_the_other_test() {
  assert run(prelude <> "
  attempt(() => ({ ksjeg0Fdv: 1 }).blgsn3988);
  attempt(() => 'blgsn3988' in { ksjeg0Fdv: 1 });
  attempt(() => ({ ksjeg0Fdv: 1 }).hasOwnProperty('blgsn3988'));
  attempt(() => { var o = { ksjeg0Fdv: 1 }; return delete o.blgsn3988 && o.ksjeg0Fdv; });
  attempt(() => { var o = Object.create({ blgsn3988: 'up' }); o.ksjeg0Fdv = 'own'; return [o.blgsn3988, o.ksjeg0Fdv]; });
  attempt(() => { var o = Object.create({ blgsn3988: 'up' }); o.x = 1; o.ksjeg0Fdv = 'own'; var k = 'blgsn3988'; return [o[k], k in o]; });
  attempt(() => { var o = Object.create({ get blgsn3988() { return 'getter'; } }); o.ksjeg0Fdv = 'own'; return o.blgsn3988; });
  attempt(() => { var o = Object.create({ blgsn3988() { return 'method'; } }); o.ksjeg0Fdv = 'own'; return o.blgsn3988(); });
  attempt(() => { var o = { ksjeg0Fdv() { return 'a'; } }; return o.blgsn3988(); });
  attempt(() => { function F() { this.ksjeg0Fdv = 1; } return new F().blgsn3988; });
  attempt(() => { var d = {}; d['k' + 1] = 0; d.ksjeg0Fdv = 1; return [d.blgsn3988, 'blgsn3988' in d]; });
  out.join(' ')
  ") == "' false false 1 [\"up\",\"own\"] [\"up\",true] \"getter\" \"method\" TypeError  [null,false]'"
}

pub fn both_names_live_side_by_side_test() {
  assert run(prelude <> "
  attempt(() => ({ ksjeg0Fdv: 1, blgsn3988: 2 }));
  attempt(() => ({ blgsn3988: 2, ksjeg0Fdv: 1, z: 3 }));
  attempt(() => { var o = {}; o.ksjeg0Fdv = 1; o.blgsn3988 = 2; o.ksjeg0Fdv = 10; return [o, o.ksjeg0Fdv, o.blgsn3988]; });
  attempt(() => { var o = { a: 0 }; o.ksjeg0Fdv = 1; o.b = 5; o.blgsn3988 = 2; return [Object.keys(o), Object.values(o)]; });
  attempt(() => { var o = { ksjeg0Fdv: 1, blgsn3988: 2 }; delete o.ksjeg0Fdv; return [o, o.ksjeg0Fdv, o.blgsn3988, 'blgsn3988' in o]; });
  attempt(() => { var o = { ksjeg0Fdv: 1, blgsn3988: 2 }; delete o.blgsn3988; return [o, o.ksjeg0Fdv, o.blgsn3988]; });
  attempt(() => { var o = { ksjeg0Fdv: 1, blgsn3988: 2 }; delete o.ksjeg0Fdv; o.ksjeg0Fdv = 3; return o; });
  attempt(() => { var o = { ksjeg0Fdv: 1, blgsn3988: 2 }; return { ...o, adnpu4BaA: 3, cheao1x9S: 4 }; });
  attempt(() => { var a = { ksjeg0Fdv: 1 }, b = { blgsn3988: 2 }; return [{ ...a, ...b }, Object.assign({}, b, a)]; });
  attempt(() => JSON.parse('{\"ksjeg0Fdv\":1,\"blgsn3988\":2,\"ksjeg0Fdv\":3}'));
  attempt(() => { var ks = []; for (var k in { ksjeg0Fdv: 1, blgsn3988: 2 }) ks.push(k); return ks; });
  attempt(() => { var { ksjeg0Fdv, blgsn3988, ...rest } = { ksjeg0Fdv: 1, blgsn3988: 2, q: 3 }; return [ksjeg0Fdv, blgsn3988, rest]; });
  attempt(() => { var { ksjeg0Fdv, ...rest } = { ksjeg0Fdv: 1, blgsn3988: 2 }; return rest; });
  attempt(() => { class C { ksjeg0Fdv = 1; blgsn3988 = 2; static adnpu4BaA = 3; static cheao1x9S = 4; } return [new C(), C.adnpu4BaA, C.cheao1x9S]; });
  attempt(() => { class C { ksjeg0Fdv() { return 'a'; } blgsn3988() { return 'b'; } } var c = new C(); return c.ksjeg0Fdv() + c.blgsn3988(); });
  attempt(() => { var o = { get ksjeg0Fdv() { return 'ga'; }, blgsn3988: 'vb' }; return [o.ksjeg0Fdv, o.blgsn3988]; });
  attempt(() => { var o = {}; Object.defineProperty(o, 'ksjeg0Fdv', { value: 1 }); Object.defineProperty(o, 'blgsn3988', { value: 2, enumerable: true }); o.ksjeg0Fdv = 9; return [o, o.ksjeg0Fdv, Object.getOwnPropertyNames(o)]; });
  attempt(() => { var o = Object.freeze({ ksjeg0Fdv: 1, blgsn3988: 2 }); return [Object.isFrozen(o), o.blgsn3988]; });
  attempt(() => { globalThis.ksjeg0Fdv = 'g1'; globalThis.blgsn3988 = 'g2'; return [ksjeg0Fdv, blgsn3988, typeof adnpu4BaA]; });
  attempt(() => { function f() {} f.ksjeg0Fdv = 1; f.blgsn3988 = 2; var a = []; a.ksjeg0Fdv = 3; a.blgsn3988 = 4; return [f.ksjeg0Fdv, f.blgsn3988, a.ksjeg0Fdv, a.blgsn3988]; });
  out.join(' ')
  ") == "'{\"ksjeg0Fdv\":1,\"blgsn3988\":2} {\"blgsn3988\":2,\"ksjeg0Fdv\":1,\"z\":3} [{\"ksjeg0Fdv\":10,\"blgsn3988\":2},10,2] [[\"a\",\"ksjeg0Fdv\",\"b\",\"blgsn3988\"],[0,1,5,2]] [{\"blgsn3988\":2},null,2,true] [{\"ksjeg0Fdv\":1},1,null] {\"blgsn3988\":2,\"ksjeg0Fdv\":3} {\"ksjeg0Fdv\":1,\"blgsn3988\":2,\"adnpu4BaA\":3,\"cheao1x9S\":4} [{\"ksjeg0Fdv\":1,\"blgsn3988\":2},{\"blgsn3988\":2,\"ksjeg0Fdv\":1}] {\"ksjeg0Fdv\":3,\"blgsn3988\":2} [\"ksjeg0Fdv\",\"blgsn3988\"] [1,2,{\"q\":3}] {\"blgsn3988\":2} [{\"ksjeg0Fdv\":1,\"blgsn3988\":2},3,4] \"ab\" [\"ga\",\"vb\"] [{\"blgsn3988\":2},1,[\"ksjeg0Fdv\",\"blgsn3988\"]] [true,2] [\"g1\",\"g2\",\"undefined\"] [1,2,3,4]'"
}

// up to seven bytes, or nine letters, the id is the text itself, so these must
// all differ
pub fn short_names_never_share_a_number_test() {
  let texts = [
    "", "\u{0}", "\u{0}\u{0}", "a", "\u{0}a", "a\u{0}", "aa", "b", "abcdefg",
    "\u{0}bcdefg", "abcdef\u{0}", "abcdef", "\u{1}", "\u{1}\u{0}", "abcdefgh",
    "abcdefgH", "@bcdefgh", "@abcdefgh", "@@@@@@@@", "@@@@@@@@@", "abcdefghi",
    "__proto__",
  ]
  let ids = list.map(texts, fn(text) { key.id(key.named(text)) })
  assert list.length(list.unique(ids)) == list.length(texts)
  let private_ids = list.map(texts, fn(text) { key.id(key.private(text)) })
  assert list.length(list.unique(list.append(ids, private_ids)))
    == 2 * list.length(texts)
  // the second bit tells the two kinds of id apart
  assert list.all(ids, fn(id) { int.bitwise_and(id, 2) == 2 })
  assert int.bitwise_and(key.id(key.named("abcdefg8")), 2) == 0
  assert int.bitwise_and(key.id(key.private("abcdefghij")), 2) == 0
}

pub fn a_setter_filed_past_its_number_still_runs_test() {
  assert run(prelude <> "
  attempt(() => {
    var seen = [], proto = { ksjeg0Fdv: 1 };
    Object.defineProperty(proto, 'blgsn3988', { set(v) { seen.push(v); } });
    var o = Object.create(proto); o.z = 1; o.blgsn3988 = 5;
    return [seen, Object.keys(o)];
  });
  attempt(() => {
    var proto = { ksjeg0Fdv: 1 };
    Object.defineProperty(proto, 'blgsn3988', { value: 1, writable: false });
    var o = Object.create(proto); o.a = 1; o.blgsn3988 = 7;
    return [o.blgsn3988, Object.keys(o)];
  });
  out.join(' ')
  ") == "'[[5],[\"z\"]] [1,[\"a\"]]'"
}
