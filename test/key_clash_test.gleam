import arc/bytecode/key.{Named}
import arc/engine.{Returned}

fn run(source: String) -> String {
  let eng = engine.new()
  let assert Ok(#(Returned(value), eng)) = engine.eval(eng, source)
  engine.inspect(eng, value)
}

// different names that are filed under one number. Nothing a script can see
// may depend on that
pub fn these_names_do_share_a_number_test() {
  let assert Named(id: a, ..) = key.named("miefzxAba")
  let assert Named(id: b, ..) = key.named("dlffym3X_")
  assert a == b
  let assert Named(id: c, ..) = key.named("obfleoaaB")
  let assert Named(id: d, ..) = key.named("scdedfTU8")
  assert c == d
}

const prelude = "
  var out = [];
  function attempt(f) { try { out.push(JSON.stringify(f())); } catch (e) { out.push(e.constructor.name); } }
"

pub fn one_name_never_answers_for_the_other_test() {
  assert run(prelude <> "
  attempt(() => ({ miefzxAba: 1 }).dlffym3X_);
  attempt(() => 'dlffym3X_' in { miefzxAba: 1 });
  attempt(() => ({ miefzxAba: 1 }).hasOwnProperty('dlffym3X_'));
  attempt(() => { var o = { miefzxAba: 1 }; return delete o.dlffym3X_ && o.miefzxAba; });
  attempt(() => { var o = Object.create({ dlffym3X_: 'up' }); o.miefzxAba = 'own'; return [o.dlffym3X_, o.miefzxAba]; });
  attempt(() => { var o = Object.create({ dlffym3X_: 'up' }); o.x = 1; o.miefzxAba = 'own'; var k = 'dlffym3X_'; return [o[k], k in o]; });
  attempt(() => { var o = Object.create({ get dlffym3X_() { return 'getter'; } }); o.miefzxAba = 'own'; return o.dlffym3X_; });
  attempt(() => { var o = Object.create({ dlffym3X_() { return 'method'; } }); o.miefzxAba = 'own'; return o.dlffym3X_(); });
  attempt(() => { var o = { miefzxAba() { return 'a'; } }; return o.dlffym3X_(); });
  attempt(() => { function F() { this.miefzxAba = 1; } return new F().dlffym3X_; });
  attempt(() => { var d = {}; d['k' + 1] = 0; d.miefzxAba = 1; return [d.dlffym3X_, 'dlffym3X_' in d]; });
  out.join(' ')
  ") == "' false false 1 [\"up\",\"own\"] [\"up\",true] \"getter\" \"method\" TypeError  [null,false]'"
}

pub fn both_names_live_side_by_side_test() {
  assert run(prelude <> "
  attempt(() => ({ miefzxAba: 1, dlffym3X_: 2 }));
  attempt(() => ({ dlffym3X_: 2, miefzxAba: 1, z: 3 }));
  attempt(() => { var o = {}; o.miefzxAba = 1; o.dlffym3X_ = 2; o.miefzxAba = 10; return [o, o.miefzxAba, o.dlffym3X_]; });
  attempt(() => { var o = { a: 0 }; o.miefzxAba = 1; o.b = 5; o.dlffym3X_ = 2; return [Object.keys(o), Object.values(o)]; });
  attempt(() => { var o = { miefzxAba: 1, dlffym3X_: 2 }; delete o.miefzxAba; return [o, o.miefzxAba, o.dlffym3X_, 'dlffym3X_' in o]; });
  attempt(() => { var o = { miefzxAba: 1, dlffym3X_: 2 }; delete o.dlffym3X_; return [o, o.miefzxAba, o.dlffym3X_]; });
  attempt(() => { var o = { miefzxAba: 1, dlffym3X_: 2 }; delete o.miefzxAba; o.miefzxAba = 3; return o; });
  attempt(() => { var o = { miefzxAba: 1, dlffym3X_: 2 }; return { ...o, obfleoaaB: 3, scdedfTU8: 4 }; });
  attempt(() => { var a = { miefzxAba: 1 }, b = { dlffym3X_: 2 }; return [{ ...a, ...b }, Object.assign({}, b, a)]; });
  attempt(() => JSON.parse('{\"miefzxAba\":1,\"dlffym3X_\":2,\"miefzxAba\":3}'));
  attempt(() => { var ks = []; for (var k in { miefzxAba: 1, dlffym3X_: 2 }) ks.push(k); return ks; });
  attempt(() => { var { miefzxAba, dlffym3X_, ...rest } = { miefzxAba: 1, dlffym3X_: 2, q: 3 }; return [miefzxAba, dlffym3X_, rest]; });
  attempt(() => { var { miefzxAba, ...rest } = { miefzxAba: 1, dlffym3X_: 2 }; return rest; });
  attempt(() => { class C { miefzxAba = 1; dlffym3X_ = 2; static obfleoaaB = 3; static scdedfTU8 = 4; } return [new C(), C.obfleoaaB, C.scdedfTU8]; });
  attempt(() => { class C { miefzxAba() { return 'a'; } dlffym3X_() { return 'b'; } } var c = new C(); return c.miefzxAba() + c.dlffym3X_(); });
  attempt(() => { var o = { get miefzxAba() { return 'ga'; }, dlffym3X_: 'vb' }; return [o.miefzxAba, o.dlffym3X_]; });
  attempt(() => { var o = {}; Object.defineProperty(o, 'miefzxAba', { value: 1 }); Object.defineProperty(o, 'dlffym3X_', { value: 2, enumerable: true }); o.miefzxAba = 9; return [o, o.miefzxAba, Object.getOwnPropertyNames(o)]; });
  attempt(() => { var o = Object.freeze({ miefzxAba: 1, dlffym3X_: 2 }); return [Object.isFrozen(o), o.dlffym3X_]; });
  attempt(() => { globalThis.miefzxAba = 'g1'; globalThis.dlffym3X_ = 'g2'; return [miefzxAba, dlffym3X_, typeof obfleoaaB]; });
  attempt(() => { function f() {} f.miefzxAba = 1; f.dlffym3X_ = 2; var a = []; a.miefzxAba = 3; a.dlffym3X_ = 4; return [f.miefzxAba, f.dlffym3X_, a.miefzxAba, a.dlffym3X_]; });
  out.join(' ')
  ") == "'{\"miefzxAba\":1,\"dlffym3X_\":2} {\"dlffym3X_\":2,\"miefzxAba\":1,\"z\":3} [{\"miefzxAba\":10,\"dlffym3X_\":2},10,2] [[\"a\",\"miefzxAba\",\"b\",\"dlffym3X_\"],[0,1,5,2]] [{\"dlffym3X_\":2},null,2,true] [{\"miefzxAba\":1},1,null] {\"dlffym3X_\":2,\"miefzxAba\":3} {\"miefzxAba\":1,\"dlffym3X_\":2,\"obfleoaaB\":3,\"scdedfTU8\":4} [{\"miefzxAba\":1,\"dlffym3X_\":2},{\"dlffym3X_\":2,\"miefzxAba\":1}] {\"miefzxAba\":3,\"dlffym3X_\":2} [\"miefzxAba\",\"dlffym3X_\"] [1,2,{\"q\":3}] {\"dlffym3X_\":2} [{\"miefzxAba\":1,\"dlffym3X_\":2},3,4] \"ab\" [\"ga\",\"vb\"] [{\"dlffym3X_\":2},1,[\"miefzxAba\",\"dlffym3X_\"]] [true,2] [\"g1\",\"g2\",\"undefined\"] [1,2,3,4]'"
}
