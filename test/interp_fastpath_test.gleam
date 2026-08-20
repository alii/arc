//// The fast loop's fused kernels must agree with the guarded slow path on
//// every observable outcome: each program here drives an opcode through a
//// shape its kernel answers for and one it must decline (`miss`), and
//// checks the JS-visible result is the full MOP's.

import arc/compiler
import arc/interp/entry
import arc/parser
import arc/rt/builtins as rt_builtins
import arc/rt/call.{NormalCompletion, ThrowCompletion}
import arc/rt/inspect as rt_inspect
import arc/rt/types.{KStr, classify}
import rt_helpers

fn run_string(source: String) -> String {
  let st = rt_builtins.new_agent(rt_helpers.quiet_hooks()) |> entry.link
  let assert Ok(#(body, sb)) = parser.parse_script(source)
    as { "parse failed: " <> source }
  let assert Ok(template) = compiler.compile(body, sb)
    as { "compile failed: " <> source }
  case entry.run_script(st, template) {
    #(NormalCompletion(v), st) ->
      case classify(v) {
        KStr(s) -> s
        _ -> panic as { source <> " gave " <> rt_inspect.inspect(st, v) }
      }
    #(ThrowCompletion(e), st) ->
      panic as { source <> " threw " <> rt_inspect.inspect(st, e) }
  }
}

// -- PutElem -------------------------------------------------------------------

pub fn put_elem_overwrite_fill_append_test() {
  assert run_string(
      "var a = []; for (var i = 0; i < 5; i++) a[i] = i * i; a[2] = 'x'; a.join() + '/' + a.length",
    )
    == "0,1,x,9,16/5"
  assert run_string("var h = [1,,3]; h[1] = 2; h.join() + '/' + h.length")
    == "1,2,3/3"
  assert run_string("var f = [1,2,3]; f[5] = 1; f.length + '/' + (4 in f)")
    == "6/false"
  assert run_string(
      "var s = []; s[2000] = 1; s[2001] = 2; s.length + '/' + (1999 in s) + '/' + s[2001]",
    )
    == "2002/false/2"
}

/// §10.1.9.2 step 2: an inherited setter at the index takes the store, both
/// for an append and for a hole fill.
pub fn put_elem_inherited_setter_test() {
  assert run_string(
      "var hit = 'no';
       Object.defineProperty(Array.prototype, '3', { set: function (v) { hit = v }, configurable: true });
       var a = [1,2,3]; a[3] = 9;
       [hit, a.length, a[3]].join()",
    )
    == "9,3,"
  assert run_string(
      "var hit = 'no';
       Object.defineProperty(Object.prototype, '1', { set: function (v) { hit = v }, configurable: true });
       var a = [1,,3]; a[1] = 9;
       [hit, a.length, a.hasOwnProperty(1)].join()",
    )
    == "9,3,false"
}

/// An inherited read-only data property at the index rejects the store.
pub fn put_elem_inherited_readonly_test() {
  assert run_string(
      "Object.defineProperty(Array.prototype, '2', { value: 7, writable: false, configurable: true });
       var a = [1,2]; a[2] = 9;
       [a.length, a[2], a.hasOwnProperty(2)].join()",
    )
    == "2,7,false"
  assert run_string(
      "'use strict';
       Object.defineProperty(Array.prototype, '2', { value: 7, writable: false, configurable: true });
       var a = [1,2];
       try { a[2] = 9; 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
}

/// §10.4.2.1 step 2.h: no growth past a non-writable length; no new
/// element on a non-extensible array.
pub fn put_elem_frozen_length_test() {
  assert run_string(
      "var a = [1,2]; Object.defineProperty(a, 'length', { writable: false });
       a[2] = 5; [a.length, a[2]].join()",
    )
    == "2,"
  assert run_string(
      "'use strict';
       var a = [1,2]; Object.defineProperty(a, 'length', { writable: false });
       try { a[2] = 5; 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
  assert run_string(
      "var e = Object.preventExtensions([1,,3]); e[1] = 5; [e.length, 1 in e].join()",
    )
    == "3,false"
}

/// A defineProperty override at the index keeps its attributes.
pub fn put_elem_index_override_test() {
  assert run_string(
      "var a = [1,2,3];
       Object.defineProperty(a, '1', { value: 'ro', writable: false });
       a[1] = 'w';
       var g = 0; Object.defineProperty(a, '2', { get: function () { return 'g' }, set: function (v) { g = v } });
       a[2] = 'set';
       [a[1], a[2], g].join()",
    )
    == "ro,g,set"
}

// -- GetElem -------------------------------------------------------------------

pub fn get_elem_holes_and_keys_test() {
  assert run_string(
      "Array.prototype[1] = 'p'; var a = [1,,3]; var r = a[1]; delete Array.prototype[1]; r",
    )
    == "p"
  assert run_string(
      "var a = ['x','y']; var o = { k: 'v', 3: 'three' };
       [a['1'], a[1.0], String(a[-1]), String(a[1.5]), o['k'], o[3], o['3']].join()",
    )
    == "y,y,undefined,undefined,v,three,three"
  assert run_string(
      "var a = [1,2]; Object.defineProperty(a, '0', { get: function () { return 'got' } }); a[0]",
    )
    == "got"
}

// -- GetField / PutField -------------------------------------------------------

pub fn get_field_chain_test() {
  assert run_string(
      "class A { get x() { return 'ax' } m() { return 'm' } }
       class B extends A {}
       var b = new B(); b.own = 'o';
       [b.x, b.m(), b.own, String(b.nope), [1,2,3].length, 'str'.length, (function f(a, b) {}).length].join()",
    )
    == "ax,m,o,undefined,3,3,2"
  assert run_string(
      "var t = { get v() { return this.w * 2 }, w: 21 }; var p = new Proxy({}, { get: function (_, k) { return 'P' + String(k) } });
       [t.v, p.any].join()",
    )
    == "42,Pany"
}

pub fn put_field_test() {
  assert run_string(
      "var o = { x: 1 }; o.x = 2; o.y = 3;
       var log = [];
       var s = { set v(n) { log.push(n) } }; s.v = 7;
       var c = Object.create(s); c.v = 8;
       Object.defineProperty(o, 'ro', { value: 'r', writable: false }); o.ro = 'changed';
       [o.x, o.y, log.join('+'), c.hasOwnProperty('v'), o.ro, Object.keys(o).join('')].join()",
    )
    == "2,3,7+8,false,r,xy"
  assert run_string(
      "'use strict'; var o = Object.freeze({ x: 1 }); try { o.x = 2; 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
  assert run_string(
      "var a = [1,2,3]; a.length = 1; var s = new String('abc');
       [a.join(), a.length, s.length].join()",
    )
    == "1,1,3"
}

// -- DefineField ---------------------------------------------------------------

pub fn define_field_test() {
  assert run_string("var o = { a: 1, b: 2, a: 3 }; Object.keys(o).join() + o.a")
    == "a,b3"
  // §7.3.7 CreateDataPropertyOrThrow over a non-configurable own property.
  assert run_string(
      "class A { constructor() { Object.defineProperty(this, 'x', { value: 1, configurable: false }) } }
       class B extends A { x = 2 }
       try { String(new B().x) } catch (e) { e.constructor.name }",
    )
    == "TypeError"
  // A configurable accessor is replaced by the data property outright.
  assert run_string(
      "class A { constructor() { Object.defineProperty(this, 'x', { get() { return 1 }, configurable: true }) } }
       class B extends A { x = 2 }
       JSON.stringify(Object.getOwnPropertyDescriptor(new B(), 'x'))",
    )
    == "{\"value\":2,\"writable\":true,\"enumerable\":true,\"configurable\":true}"
}

// -- Arithmetic / compare kernels ---------------------------------------------

pub fn add_kernel_test() {
  assert run_string(
      "[1+2, 1.5+1, 'a'+1, 1+'a', 1+null, null+1, true+1, 1+undefined, 0.1+0.2, 9007199254740991+1, 1e308+1e308, 'a'+1n, [1]+[2]].join()",
    )
    == "3,2.5,a1,1a,1,1,2,NaN,0.30000000000000004,9007199254740992,Infinity,a1,12"
  assert run_string(
      "try { 1 + 1n; 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
  assert run_string(
      "try { 'a' + Symbol(); 'nothrow' } catch (e) { e.constructor.name }",
    )
    == "TypeError"
}

pub fn negative_zero_kernel_test() {
  assert run_string(
      "[1/(-0), 1/(0*-1), 1/(-1%1), 1/(-4%2), 1/(0/-3), Object.is(0, -(0*-1)), 1/-(0)].join()",
    )
    == "-Infinity,-Infinity,-Infinity,-Infinity,-Infinity,true,-Infinity"
}

pub fn div_mod_kernel_test() {
  assert run_string(
      "[7/2, 6/3, 1/0, -1/0, 0/0, 5%0, -5%5, 5.5%2, Infinity/Infinity, 2**10].map(String).join()",
    )
    == "3.5,2,Infinity,-Infinity,NaN,NaN,0,1.5,NaN,1024"
}

pub fn compare_kernel_test() {
  assert run_string(
      "[1<2, 'a'<'b', 'B'<'a', 1<NaN, NaN>=NaN, Infinity>1e308, -Infinity<-1e308, 1n<2n, null<1, undefined<1, '10'<'9', 10<'9', [2]<3].join()",
    )
    == "true,true,true,false,false,true,true,true,true,false,true,false,true"
  assert run_string(
      "[null==undefined, null==0, 1=='1', NaN==NaN, 0==-0, 'a'=='a', 1n==1, Infinity=='Infinity', true==1, [1]==1, ({})=='[object Object]', null===undefined, 1===1.0].join()",
    )
    == "true,false,true,false,true,true,true,true,true,true,true,false,true"
}

pub fn bitwise_kernel_test() {
  assert run_string(
      "[5&3, 5|3, 5^3, 1<<31, -1>>>0, -8>>1, ~5, (2147483647+1)|0, 1.5|0, '3'|0, ~'x'].join()",
    )
    == "1,7,6,-2147483648,4294967295,-4,-6,-2147483648,1,3,-1"
}

// -- Fused local superinstructions --------------------------------------------

pub fn inc_dec_local_test() {
  assert run_string(
      "(function () {
         var s = '1'; s++; var t = '1'; t--; var f = 0.5; f++; var m = 9007199254740991; m++;
         var u; u++; var o = { valueOf() { return 41 } }; o++;
         return [typeof s, s, t, f, m, u, o].join()
       })()",
    )
    == "number,2,0,1.5,9007199254740992,NaN,42"
}

pub fn cmp_local_jump_test() {
  // Numbers, strings (lexicographic), and an object operand (valueOf runs).
  assert run_string(
      "(function () {
         var n = 0; for (var i = 0; i < 10; i++) n++;
         var m = 0; for (var j = 'a'; j < 'aaaa'; j += 'a') m++;
         var calls = 0; var lim = { valueOf() { calls++; return 3 } };
         var k = 0; for (var x = 0; x < lim; x++) k++;
         var big = 0; for (var y = 0n; y < 3n; y = y + 1n) big++;
         return [n, m, k, calls, big].join()
       })()",
    )
    == "10,3,3,4,3"
  // A TDZ local read inside the fused compare is still a ReferenceError.
  assert run_string(
      "(function () { try { for (let i = 0; i < 1; i++) { let z = z < 1; } return 'nothrow' } catch (e) { return e.constructor.name } })()",
    )
    == "ReferenceError"
}

/// Prefix `++i`/`--i` on a plain local fuse to IncLocal/DecLocal (plus a
/// read when the value is used): same coercions, same TDZ error.
pub fn prefix_inc_dec_local_test() {
  assert run_string(
      "(function () {
         var s = '1'; var a = ++s; var n = 3; var seen = []; while (--n >= 0) seen.push(n);
         var o = { valueOf() { return 41 } }; var b = --o;
         var t; try { ++z; let z = 0; t = 'nothrow' } catch (e) { t = e.constructor.name }
         return [typeof s, s, a, seen.join(''), o, b, t].join()
       })()",
    )
    == "number,2,2,210,40,40,ReferenceError"
}

/// Conditions lowered in branch context (`!`, `&&`, `||`, `== null`,
/// literal tests, inverted fused compares) keep evaluation order and
/// short-circuiting.
pub fn branch_lowering_test() {
  assert run_string(
      "(function () {
         var log = ''; function t(x) { log += x; return x }
         var r = [];
         if (t(1) && t(0) && t(2)) r.push('a'); else r.push('b');
         if (!(t(0) || !t(3))) r.push('c');
         if (!t('')) r.push('d');
         var i = 5; do { r.push(i) } while (--i > 3);
         var j = 0; do { j++ } while (j < 2 || t(0));
         while (true) { if (null == t(null) && t(4) != undefined) break }
         for (; 0;) r.push('never');
         r.push(t({ valueOf() { return null } }) == null ? 'obj' : 'ok');
         return r.join() + '/' + log + '/' + j
       })()",
    )
    == "b,c,d,5,4,ok/10030null4null/2"
}

// -- typeof / truthiness --------------------------------------------------------

pub fn typeof_kernel_test() {
  assert run_string(
      "[typeof class {}, typeof function () {}, typeof new Proxy(function () {}, {}), typeof new Proxy({}, {}), typeof null, typeof undefined, typeof 1n, typeof Symbol(), typeof 'a'.at, typeof Math].join()",
    )
    == "function,function,function,object,object,undefined,bigint,symbol,function,object"
}

pub fn truthy_kernel_test() {
  assert run_string(
      "[0, -0, NaN, '', null, undefined, 0n, false, 1, 'a', {}, [], 1n, Symbol(), Infinity].map(function (v) { return v ? 1 : 0 }).join('')",
    )
    == "000000001111111"
}

// -- Globals -------------------------------------------------------------------

pub fn get_global_kernel_test() {
  // Lexical (let/const), var, an accessor on the global object, a name
  // resolved through the global's prototype chain, and typeof of each.
  assert run_string(
      "let lx = 1; const cx = 2; var vx = 3;
       Object.defineProperty(globalThis, 'acc', { get: function () { return 4 }, configurable: true });
       [lx + cx + vx + acc, typeof nope, typeof vx, typeof lx, typeof Math, toString === Object.prototype.toString].join()",
    )
    == "10,undefined,number,number,object,true"
  // TDZ and unresolvable names still throw.
  assert run_string(
      "var out = [];
       try { tdz } catch (e) { out.push(e.constructor.name) }
       try { typeof tdz } catch (e) { out.push(e.constructor.name) }
       let tdz = 0;
       try { nope } catch (e) { out.push(e.constructor.name) }
       out.join()",
    )
    == "ReferenceError,ReferenceError,ReferenceError"
}

pub fn put_global_kernel_test() {
  // Sloppy creation, replacement of a var, a lexical let, a strict miss on
  // an undeclared name, and a read-only global left alone.
  assert run_string(
      "var v = 1; let l = 2; v = 10; l = 20; fresh = 30;
       var d = Object.getOwnPropertyDescriptor(globalThis, 'fresh');
       var strict = (function () { 'use strict'; try { undeclared = 1; return 'set' } catch (e) { return e.constructor.name } })();
       Object.defineProperty(globalThis, 'ro', { value: 1, writable: false }); ro = 5;
       [v, l, fresh, d.writable && d.enumerable && d.configurable, strict, ro, 'l' in globalThis].join()",
    )
    == "10,20,30,true,ReferenceError,1,false"
}

// -- new / constructor return ----------------------------------------------------

pub fn construct_fast_path_test() {
  assert run_string(
      "function K(a) { this.a = a } K.prototype = { z: 1 };
       var k = new K(5);
       function R() { this.q = 1; return { r: 2 } }
       function S() { this.q = 1; return 5 }
       var out = [k.a, k.z, Object.getPrototypeOf(k) === K.prototype, new R().r, new R().q, new S().q];
       K.prototype = null; out.push(Object.getPrototypeOf(new K(1)) === Object.prototype);
       class B { constructor() { this.b = 1 } } class D extends B { constructor() { super(); this.d = 2 } }
       var dd = new D(); out.push(dd.b + dd.d, dd instanceof B);
       try { new (() => 1)() } catch (e) { out.push(e.constructor.name) }
       var P = new Proxy(function () {}, { get: function (t, k) { return k === 'prototype' ? { viaProxy: true } : t[k] } });
       out.push(Reflect.construct(K, [], P).viaProxy);
       out.join()",
    )
    == "5,1,true,2,,1,true,3,true,TypeError,true"
}

// -- instanceof ---------------------------------------------------------------

pub fn instanceof_kernel_test() {
  assert run_string(
      "function G() {} var g = new G(); var out = [];
       out.push(g instanceof G, ({}) instanceof G, 5 instanceof G, [] instanceof Array, new Map instanceof Map, G instanceof Function, g instanceof Object);
       class B {} class D extends B {} out.push(new D instanceof B, new B instanceof D);
       out.push(g instanceof G.bind(null));
       var H = function () {}; Object.defineProperty(H, Symbol.hasInstance, { value: function () { return true } });
       out.push(1 instanceof H);
       function F() {} Object.setPrototypeOf(F, Object.create(Function.prototype, { [Symbol.hasInstance]: { value: function () { return true } } }));
       out.push(({}) instanceof F);
       try { g instanceof {} } catch (e) { out.push(e.constructor.name) }
       var A = () => 1; try { g instanceof A } catch (e) { out.push(e.constructor.name) }
       out.push(g instanceof new Proxy(G, {}));
       out.join()",
    )
    == "true,false,false,true,true,true,true,true,false,true,true,true,TypeError,TypeError,true"
}

// -- Computed access on ordinary objects / arguments / string keys ------------

pub fn elem_kernels_ordinary_and_arguments_test() {
  assert run_string(
      "var o = {}; o[3] = 'x'; o[4294967295] = 'big'; o['7'] = 's'; var k = 'name'; o[k] = 'n';
       var out = [o[3], o['3'], o[4294967295], o[7], o.name, o[9], Object.keys(o).join('|')];
       var P = { set 5(v) { out.push('setter' + v) } }; var c = Object.create(P); c[5] = 1; out.push(Object.keys(c).length);
       var ro = Object.create(Object.defineProperty({}, '6', { value: 0, writable: false })); ro[6] = 1; out.push(ro[6]);
       var fz = Object.freeze({}); fz[1] = 2; out.push(fz[1]);
       function A(a, b) { arguments[0] = 9; return [arguments[0], arguments[1], arguments[2], arguments.length].join('/') }
       out.push(A(1, 2));
       var arr = [1, 2, 3]; out.push(arr.length, arr['length']); arr.length = 1; out.push(arr.length, arr[1]);
       var xs = [1, 2]; xs[1] += 5; var oo = { q: 1 }; var qq = 'q'; oo[qq] += 1; out.push(xs[1], oo.q);
       out.join()",
    )
    == "x,x,big,s,n,,3|7|4294967295|name,setter1,0,0,,9/2//2,3,3,1,,7,2"
}

// -- Object literal fields, closures, try, generators in the loop -------------

pub fn literal_closure_try_yield_arms_test() {
  assert run_string(
      "var out = [];
       var lit = { a: 1, b: 2, a: 3 }; out.push(Object.keys(lit).join('|'), lit.a);
       var fs = []; for (var i = 0; i < 3; i++) fs.push((function (x) { return function () { return x } })(i));
       out.push(fs.map(function (f) { return f() }).join('|'));
       function T() { try { throw 1 } catch (e) { return e + 1 } finally { out.push('fin') } } out.push(T());
       function* gen(k) { for (var j = 0; j < k; j++) yield j; return 'done' }
       var s = 0; for (var v of gen(4)) s += v; out.push(s);
       var it = gen(1); out.push(it.next().value, it.next().value, it.next().done);
       async function af() { var x = await 5; return x + 1 } var r; af().then(function (v) { r = v });
       out.push(typeof r);
       out.join()",
    )
    == "a|b,3,0|1|2,fin,2,6,0,done,true,undefined"
}
