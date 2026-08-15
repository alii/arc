//// M20 Milestone-0 differential tests: prove that a JS source compiled via
//// emit_2core→ir_to_core→BEAM produces byte-identical console output to the
//// same source run through the arc interpreter. sum(n) exercises loops +
//// mutable locals + arithmetic; makeAdder exercises closure capture + nested
//// call. Each test asserts BOTH the differential (compiled == interpreted)
//// AND the interpreter oracle against the known answer, so a bug that breaks
//// both paths identically still fails.

import emit_2core_harness as harness

const sum_src = "function sum(n){let s=0;for(let i=1;i<=n;i++)s+=i;return s} console.log(sum(10))"

const make_adder_src = "function makeAdder(x){return function(y){return x+y}} console.log(makeAdder(3)(4))"

pub fn sum_n_diff_test() {
  let i = harness.run_interpreted(sum_src)
  let c = harness.run_compiled(sum_src)
  assert i.stdout == <<"55\n":utf8>>
  assert c.stdout == i.stdout
}

pub fn make_adder_diff_test() {
  let i = harness.run_interpreted(make_adder_src)
  let c = harness.run_compiled(make_adder_src)
  assert i.stdout == <<"7\n":utf8>>
  assert c.stdout == i.stdout
}

const object_literal_src = "let o={x:5};console.log(o.x)"

const obj_prop_src = "let o={x:0};for(let i=1;i<=4;i++)o.x=o.x+i;console.log(o.x)"

pub fn object_literal_diff_test() {
  let i = harness.run_interpreted(object_literal_src)
  let c = harness.run_compiled(object_literal_src)
  assert i.stdout == <<"5\n":utf8>>
  assert c.stdout == i.stdout
}

pub fn obj_prop_diff_test() {
  let i = harness.run_interpreted(obj_prop_src)
  let c = harness.run_compiled(obj_prop_src)
  assert i.stdout == <<"10\n":utf8>>
  assert c.stdout == i.stdout
}

// ── Fast-path miss-branch differential tests ────────────────────────────────
// Each source below is crafted so that (a) the emit_2core fast-path probes
// (call_method_mono / own_data_poly / new_simple / instanceof_fast) hit on
// SOME lines and MISS→fallback on others, and (b) the interpreter oracle is
// a fixed byte string. A regression in either the hit or the miss arm shows
// up as a stdout diff.

const method_call_proto_own_src = "function A(){};A.prototype.mp=function(){return 'proto'};var a=new A();a.mo=function(){return 'own'};console.log(a.mp());console.log(a.mo())"

pub fn method_call_proto_own_diff_test() {
  let i = harness.run_interpreted(method_call_proto_own_src)
  let c = harness.run_compiled(method_call_proto_own_src)
  assert i.stdout == <<"proto\nown\n":utf8>>
  assert c.stdout == i.stdout
}

const method_call_proto_chain_src = "function A(){};A.prototype.m=function(){return 'A'};function B(){};B.prototype=new A();var b=new B();console.log(b.m());B.prototype.m=function(){return 'B'};console.log(b.m())"

pub fn method_call_proto_chain_diff_test() {
  let i = harness.run_interpreted(method_call_proto_chain_src)
  let c = harness.run_compiled(method_call_proto_chain_src)
  assert i.stdout == <<"A\nB\n":utf8>>
  assert c.stdout == i.stdout
}

const method_call_miss_src = "var o={};try{o.nope()}catch(e){console.log('miss:'+e.name)};Object.defineProperty(o,'g',{get:function(){return function(){return 'getter'}}});console.log(o.g())"

pub fn method_call_miss_diff_test() {
  let i = harness.run_interpreted(method_call_miss_src)
  let c = harness.run_compiled(method_call_miss_src)
  assert i.stdout == <<"miss:TypeError\ngetter\n":utf8>>
  assert c.stdout == i.stdout
}

const this_multi_field_src = "function T(){this.a=1;this.b=2;this.c=3};T.prototype.p=9;Object.defineProperty(T.prototype,'g',{get:function(){return this.a+this.b}});T.prototype.rd=function(){var s='';s+=this.a;s+=this.b;s+=this.c;s+=this.g;s+=this.p;return s};console.log(new T().rd())"

pub fn this_multi_field_diff_test() {
  let i = harness.run_interpreted(this_multi_field_src)
  let c = harness.run_compiled(this_multi_field_src)
  assert i.stdout == <<"12339\n":utf8>>
  assert c.stdout == i.stdout
}

const set_read_coherence_src = "function C(){this.x=0;this.y=0};C.prototype.w=function(a,b){this.x=a;this.y=b};var c=new C();c.w(4,5);var k='x';console.log(''+c[k]+c['y']+c.x)"

pub fn set_read_coherence_diff_test() {
  let i = harness.run_interpreted(set_read_coherence_src)
  let c = harness.run_compiled(set_read_coherence_src)
  assert i.stdout == <<"454\n":utf8>>
  assert c.stdout == i.stdout
}

const new_return_shape_src = "function Fo(){this.v=1;return {v:2}};function Fp(){this.v=1;return 5};console.log(''+new Fo().v);console.log(''+new Fp().v)"

pub fn new_return_shape_diff_test() {
  let i = harness.run_interpreted(new_return_shape_src)
  let c = harness.run_compiled(new_return_shape_src)
  assert i.stdout == <<"2\n1\n":utf8>>
  assert c.stdout == i.stdout
}

const new_nonctor_src = "var A=function(){return 1};A=()=>{};try{new A()}catch(e){console.log('arrow:'+e.name)};function F(x){this.v=x};var B=F.bind(null,7);console.log(''+new B().v)"

pub fn new_nonctor_diff_test() {
  let i = harness.run_interpreted(new_nonctor_src)
  let c = harness.run_compiled(new_nonctor_src)
  assert i.stdout == <<"arrow:TypeError\n7\n":utf8>>
  assert c.stdout == i.stdout
}

// Truth-value RESULTS, not their truthiness. Every other operator test here
// funnels the result through a ternary, where the number 1 and the boolean
// `true` are indistinguishable — which is exactly how `instanceof`/`in`/`==`
// shipped returning i32 `0|1` instead of Booleans. String-coerce and `typeof`
// the result so the value itself is asserted (§13.10.1, §13.10.2, §7.2.14).
const truth_value_src = "function A(){};function B(){};var a=new A();var o={p:1};var s='';s+=(a instanceof A)+','+(a instanceof B)+',';s+=('p' in o)+','+('zz' in o)+',';s+=(1==1)+','+(1==2)+',';s+=('a'<'b')+','+('b'<'a')+',';s+=typeof (a instanceof A)+','+typeof ('p' in o)+','+typeof ('a'<'b');console.log(s)"

pub fn truth_value_results_are_booleans_diff_test() {
  let i = harness.run_interpreted(truth_value_src)
  let c = harness.run_compiled(truth_value_src)
  assert i.stdout
    == <<
      "true,false,true,false,true,false,true,false,boolean,boolean,boolean\n":utf8,
    >>
  assert c.stdout == i.stdout
}

const instanceof_chain_src = "function A(){};function B(){};B.prototype=new A();var b=new B();var s='';s+=(b instanceof B)?'y':'n';s+=(b instanceof A)?'y':'n';s+=({} instanceof A)?'y':'n';console.log(s)"

pub fn instanceof_chain_diff_test() {
  let i = harness.run_interpreted(instanceof_chain_src)
  let c = harness.run_compiled(instanceof_chain_src)
  assert i.stdout == <<"yyn\n":utf8>>
  assert c.stdout == i.stdout
}

const instanceof_bound_src = "function A(){};var a=new A();var Ab=A.bind(null);var s='';s+=(a instanceof Ab)?'y':'n';s+=({} instanceof Ab)?'y':'n';console.log(s)"

pub fn instanceof_bound_diff_test() {
  let i = harness.run_interpreted(instanceof_bound_src)
  let c = harness.run_compiled(instanceof_bound_src)
  assert i.stdout == <<"yn\n":utf8>>
  assert c.stdout == i.stdout
}

const instanceof_has_instance_src = "function H(){};Object.defineProperty(H,Symbol.hasInstance,{value:function(v){return v===42}});var s='';s+=(42 instanceof H)?'y':'n';s+=({} instanceof H)?'y':'n';console.log(s)"

pub fn instanceof_has_instance_diff_test() {
  let i = harness.run_interpreted(instanceof_has_instance_src)
  let c = harness.run_compiled(instanceof_has_instance_src)
  assert i.stdout == <<"yn\n":utf8>>
  assert c.stdout == i.stdout
}

const cond_eq_src = "var a=1,b=1,c=2,o={};function f(x,y){if(x==y)return 'y';else return 'n'}console.log(f(a,b),f(a,c),f(null,undefined),f(o,o),f('1',1));if(a!=c)console.log('ne');if(a!=b)console.log('bad');var i=0;while(i!=3)i++;console.log(i);for(var j=5;j==5;j++)console.log('once')"

pub fn cond_eq_diff_test() {
  let i = harness.run_interpreted(cond_eq_src)
  let c = harness.run_compiled(cond_eq_src)
  assert i.stdout == <<"y n y y y\nne\n3\nonce\n":utf8>>
  assert c.stdout == i.stdout
}

// ── Language ops served by arc/rt/lang ─────────────────────────────────────

fn diff(src: String, want: String) {
  let i = harness.run_interpreted(src)
  let c = harness.run_compiled(src)
  assert i.stdout == <<want:utf8>>
  assert c.stdout == i.stdout
}

pub fn for_of_array_diff_test() {
  diff("var s='';for(const x of [1,2,3])s+=x;console.log(s)", "123\n")
}

const user_iter_src = "var log=[];function it(n){var i=0;return {[Symbol.iterator](){return this},next(){i++;return {done:i>n,value:i}},return(){log.push('ret'+i);return {}}}}
for(var a of it(3)){log.push(a)}
for(var b of it(5)){if(b==2)break;log.push(b)}
try{for(var c of it(5)){if(c==2)throw 'boom';log.push(c)}}catch(e){log.push(e)}
console.log(log.join())"

pub fn for_of_user_iterator_return_diff_test() {
  diff(user_iter_src, "1,2,3,1,ret2,1,ret2,boom\n")
}

pub fn array_spread_diff_test() {
  diff(
    "var a=[1,2,3];var b=[0,...a,4,...'xy'];console.log(b.length,b.join())",
    "7 0,1,2,3,4,x,y\n",
  )
}

pub fn call_spread_diff_test() {
  diff(
    "function f(){return Array.prototype.join.call(arguments)}var a=[2,3];console.log(f(1,...a,4),Math.max(...a))",
    "1,2,3,4 3\n",
  )
}

pub fn object_spread_diff_test() {
  diff(
    "var o={a:1,b:2};Object.defineProperty(o,'h',{value:9,enumerable:false});var p={...o,c:3,...null};console.log(Object.keys(p).join(),p.a+p.b+p.c,p.h)",
    "a,b,c 6 undefined\n",
  )
}

pub fn object_rest_diff_test() {
  diff(
    "var o={a:1,b:2,c:3};var {a,...r}=o;var k='b';var {[k]:bb,...r2}=o;console.log(a,Object.keys(r).join(),bb,Object.keys(r2).join())",
    "1 b,c 2 a,c\n",
  )
}

pub fn array_pattern_rest_diff_test() {
  diff(
    "var [x,,y,...zs]=[1,2,3,4,5];var [p,...ps]='ab';var h,t;[h,...t]=[7,8,9];var [q,...qs]=[];console.log(x,y,zs.join(),p,ps.join(),h+':'+t.length,q,qs.length)",
    "1 3 4,5 a b 7:2 undefined 0\n",
  )
}

pub fn tagged_template_identity_diff_test() {
  diff(
    "function t(s){return s}function f(x){return t`a${x}\\n`}var s1=f(1),s2=f(2);console.log(s1===s2,s1.length,s1[1]==='\\n',s1.raw[1],Object.isFrozen(s1),t`z`===s1)",
    "true 2 true \\n true false\n",
  )
}

pub fn regexp_literal_diff_test() {
  diff(
    "var r=/a+(b)?/g;var m=r.exec('caaab');console.log(m[0],m[1],m.index,r.lastIndex,/x/===/x/)",
    "aaab b 1 5 false\n",
  )
}

pub fn global_delete_diff_test() {
  diff(
    "globalThis.gx=1;Object.defineProperty(globalThis,'gy',{value:2,configurable:false});let lz=3;console.log(delete gx,typeof gx,delete gy,typeof gy,delete lz,lz)",
    "true undefined false number false 3\n",
  )
}

// Counts iterations only: the for-await lowering does not yet rebind the
// loop variable per iteration, but get_iterator(async) / async_iter_next /
// the done check are what this covers.
pub fn for_await_step_diff_test() {
  diff(
    "var it={i:0,[Symbol.asyncIterator](){return this},next(){this.i++;return Promise.resolve({done:this.i>3,value:this.i})}};var n=0;var main=async function(){for await(const v of it)n++;for await(const w of [1,2])n+=10;console.log(n)};main()",
    "23\n",
  )
}

pub fn microtasks_drain_after_main_diff_test() {
  diff(
    "Promise.resolve(1).then(v=>console.log('then',v));console.log('sync')",
    "sync\nthen 1\n",
  )
}

pub fn unsupported_import_call_is_compile_error_test() {
  let c = harness.run_compiled("var p=import('x');console.log('no')")
  assert c.stdout == <<>>
  let assert Error(msg) = c.result
  assert msg == "UnsupportedFeature(\"import()\")"
}

pub fn unsupported_using_is_compile_error_test() {
  let c =
    harness.run_compiled("{using r={[Symbol.dispose](){}};console.log('no')}")
  assert c.stdout == <<>>
  let assert Error(msg) = c.result
  assert msg == "UnsupportedFeature(\"using declaration\")"
}

const iter_close_protocol_src = "var log=[];function it(n){var i=0;return {[Symbol.iterator](){return this},next(){i++;log.push('n'+i);return {done:i>n,value:i}},return(){log.push('r');return {}}}}
var [a]=it(3);var [b,c,d,e]=it(2);var [...all]=it(2);var [f,...g]=it(3);
var t={[Symbol.iterator](){return this},next(){throw 'nx'},return(){log.push('BAD')}};try{for(const x of t){}}catch(ex){log.push(ex)}
console.log(log.join(),a,e,all.join(),g.join())"

pub fn iterator_close_protocol_diff_test() {
  diff(
    iter_close_protocol_src,
    "n1,r,n1,n2,n3,n1,n2,n3,n1,n2,n3,n4,nx 1 undefined 1,2 2,3\n",
  )
}

pub fn generator_and_collection_iteration_diff_test() {
  diff(
    "function* gen(){yield 1;yield 2;return 3}var s=[];for(const v of gen())s.push(v);s.push([...gen()].length);var m=new Map([[1,'a'],[2,'b']]);for(const [k,v] of m)s.push(k+v);var closed=0;var inf={[Symbol.iterator](){return this},next(){return {done:false,value:1}},return(){closed++;return {}}};lab:for(const x of inf){for(const y of inf){break lab}}console.log(s.join(),Math.max(...new Set([5,1,5])),closed)",
    "1,2,2,1a,2b 5 2\n",
  )
}

// ── Buffer family: ArrayBuffer / TypedArray / DataView / Atomics ───────────

pub fn typed_array_index_exotic_diff_test() {
  diff(
    "var a=new Uint8Array([1,2,300]);a[1]=7;a[9]=5;a['1.5']=5;var o={};console.log(a[0],a[1],a[2],a[9],a.length,Object.keys(a).join(),'1' in a,'3' in a,'-0' in a,delete a[0],delete a[9],JSON.stringify(Object.getOwnPropertyDescriptor(a,'0')),Reflect.set(a,0,4,o),o[0],a[0],JSON.stringify({...a}))",
    "1 7 44 undefined 3 0,1,2 true false false false true {\"value\":1,\"writable\":true,\"enumerable\":true,\"configurable\":true} true 4 1 {\"0\":1,\"1\":7,\"2\":44}\n",
  )
}

pub fn typed_array_over_buffer_and_resizable_diff_test() {
  diff(
    "var b=new ArrayBuffer(16);var f=new Float64Array(b,8);f[0]=1.5;console.log(f.length,f.byteOffset,f[0],new Uint8Array(b)[15]);var r=new ArrayBuffer(4,{maxByteLength:16});var v=new Uint8Array(r);var s=v.subarray(1);r.resize(12);console.log(v.length,s.length,r.resizable,r.maxByteLength);r.resize(2);console.log(v.length,s.length);var t=r.transfer(8);console.log(r.detached,v.length,t.byteLength,t.resizable);try{v.fill(1)}catch(e){console.log(e.name)}",
    "1 8 1.5 63\n12 11 true 16\n2 1\ntrue 0 8 true\nTypeError\n",
  )
}

pub fn typed_array_methods_diff_test() {
  diff(
    "var x=new Float32Array([5,1,4]);x.set([9],2);console.log(x.join(),x.subarray(1).join(),x.slice(0,2).join(),x.toSorted().join(),x.sort(function(a,b){return b-a}).join(),x.at(-1),x.indexOf(9),x.includes(NaN),x.map(function(v){return v*2}).join(),x.filter(function(v){return v>4}).length,x.reduce(function(a,b){return a+b}),x.findLast(function(v){return v<9}),x.with(0,7).join(),x.toReversed().join(),x.copyWithin(1,0,2).join(),[...x.entries()].join(';'),Uint8Array.from('123',function(c){return c*2}).join(),Int16Array.of(-1,'2').join(),new Uint8ClampedArray([1.5,2.5,-1,999]).join(),new Int8Array(new Float64Array([1.9,-2.9,130])).join())",
    "5,1,9 1,9 5,1 1,5,9 9,5,1 1 0 false 18,10,2 2 15 1 7,5,1 1,5,9 9,9,5 0,9;1,9;2,5 2,4,6 -1,2 2,2,0,255 1,-2,-126\n",
  )
}

pub fn data_view_and_bigint_diff_test() {
  diff(
    "var d=new DataView(new ArrayBuffer(16));d.setInt16(0,-2,true);d.setFloat16(2,1.5);d.setBigUint64(8,2n**64n-1n);console.log(d.getInt16(0,true),d.getInt16(0),d.getFloat16(2),d.getUint16(2),d.getBigInt64(8),d.getUint32(8,true),d.byteLength);try{d.getFloat64(12)}catch(e){console.log(e.name)}var g=new BigInt64Array(2);g[0]=-5n;g[1]=2n**63n;console.log(g[0],g[1],g.join());try{g[0]=1}catch(e){console.log(e.name)}",
    "-2 -257 1.5 15872 -1n 4294967295 16\nRangeError\n-5n -9223372036854775808n -5,-9223372036854775808\nTypeError\n",
  )
}

pub fn shared_buffer_and_atomics_diff_test() {
  diff(
    "var sab=new SharedArrayBuffer(8,{maxByteLength:16});var i=new Int32Array(sab);console.log(Atomics.add(i,0,5),Atomics.load(i,0),Atomics.compareExchange(i,0,5,9),Atomics.compareExchange(i,0,5,1),i[0],Atomics.exchange(i,1,-1),Atomics.and(i,1,12),Atomics.sub(i,1,2),Atomics.xor(i,1,3),Atomics.or(i,1,16),i[1],Atomics.store(i,0,-1/0),Atomics.notify(i,0),Atomics.isLockFree(8));sab.grow(16);console.log(sab.byteLength,i.length,sab.growable);try{Atomics.wait(i,0,0,0)}catch(e){console.log(e.name)}try{Atomics.load(i,9)}catch(e){console.log(e.name)}try{Atomics.add(new Float64Array(1),0,1)}catch(e){console.log(e.name)}",
    "0 5 5 9 9 0 -1 12 10 9 25 -Infinity 0 true\n16 4 true\nTypeError\nRangeError\nTypeError\n",
  )
}

// ── Meta-object protocol: Proxy / Reflect / String exotic / JSON ───────────

pub fn proxy_traps_logging_diff_test() {
  diff(
    "var log=[];var t={a:1,b:2};var h={};['get','set','has','deleteProperty','ownKeys','getOwnPropertyDescriptor','defineProperty','getPrototypeOf'].forEach(function(n){h[n]=function(){log.push(n);return Reflect[n].apply(null,arguments)}});var p=new Proxy(t,h);console.log(p.a);p.c=3;console.log('a' in p,delete p.b,Object.keys(p).join(),t.c,p instanceof Object);var ks=[];for(var k in p)ks.push(k);console.log(ks.join(),log.join())",
    "1\ntrue true a,c 3 true\na,c get,set,getOwnPropertyDescriptor,defineProperty,has,deleteProperty,ownKeys,getOwnPropertyDescriptor,getOwnPropertyDescriptor,getPrototypeOf,ownKeys,getOwnPropertyDescriptor,getOwnPropertyDescriptor,getPrototypeOf\n",
  )
}

pub fn proxy_invariants_and_revocation_diff_test() {
  diff(
    "function tr(f){try{return String(f())}catch(e){return e.constructor.name}}var t={};Object.defineProperty(t,'k',{value:1});var p=new Proxy(t,{get:function(){return 2},has:function(){return false},ownKeys:function(){return []},getOwnPropertyDescriptor:function(){return undefined},deleteProperty:function(){return true},isExtensible:function(){return false},defineProperty:function(){return true},getPrototypeOf:function(){return 1}});console.log(tr(function(){return p.k}),tr(function(){return 'k' in p}),tr(function(){return Object.keys(p)}),tr(function(){return Object.getOwnPropertyDescriptor(p,'k')}),tr(function(){return Reflect.deleteProperty(p,'k')}),tr(function(){return Object.isExtensible(p)}),tr(function(){return Object.defineProperty(p,'z',{configurable:false})}),tr(function(){return Object.getPrototypeOf(p)}),Reflect.defineProperty(new Proxy({},{defineProperty:function(){return false}}),'x',{}),tr(function(){return new Proxy({},{get:1}).x}));var r=Proxy.revocable(function(){},{});r.revoke();r.revoke();console.log(tr(function(){return r.proxy.x}),tr(function(){return Object.keys(r.proxy)}),tr(function(){return r.proxy()}),typeof r.proxy,tr(function(){return Array.isArray(r.proxy)}),Array.isArray(new Proxy(new Proxy([],{}),{})))",
    "TypeError TypeError TypeError TypeError TypeError TypeError TypeError TypeError false TypeError\nTypeError TypeError TypeError function TypeError true\n",
  )
}

pub fn reflect_through_proxy_diff_test() {
  diff(
    "var t={x:1};var p=new Proxy(t,{});Reflect.set(p,'y',2);console.log(Reflect.get(p,'x'),Reflect.has(p,'y'),t.y,Reflect.ownKeys(p).join(),Reflect.getPrototypeOf(p)===Object.prototype,Reflect.setPrototypeOf(p,null),Object.getPrototypeOf(t),Reflect.isExtensible(p),Reflect.preventExtensions(p),Object.isExtensible(t),Reflect.defineProperty(p,'z',{value:3}),Reflect.deleteProperty(p,'x'),JSON.stringify(Reflect.getOwnPropertyDescriptor(p,'y')),JSON.stringify(Object.getOwnPropertyDescriptors(p)));var c=Object.create(new Proxy({inh:7},{}));console.log(c.inh,'inh' in c,Object.keys(Object.assign({},p)).join())",
    "1 true 2 x,y true true null true true false false true {\"value\":2,\"writable\":true,\"enumerable\":true,\"configurable\":true} {\"y\":{\"value\":2,\"writable\":true,\"enumerable\":true,\"configurable\":true}}\n7 true y\n",
  )
}

pub fn string_exotic_keys_diff_test() {
  diff(
    "var s=new String('abc');s.x=1;s[5]='q';console.log(s[0],s[3],s.length,Object.keys(s).join(),Object.getOwnPropertyNames(s).join(),JSON.stringify(Object.getOwnPropertyDescriptor(s,'1')),delete s[5],'2' in s,'3' in s,Reflect.set(s,'0','z'),s[0],Reflect.defineProperty(s,'0',{value:'a'}),Reflect.defineProperty(s,'0',{value:'z'}),Reflect.defineProperty(s,'length',{value:3}),Reflect.deleteProperty(s,'length'),Object.isFrozen(Object.freeze(new String('hi'))));var r=[];for(var k in s)r.push(k);console.log(r.join(),JSON.stringify(s),JSON.stringify({...new String('ok')}))",
    "a undefined 3 0,1,2,5,x 0,1,2,5,length,x {\"value\":\"b\",\"writable\":false,\"enumerable\":true,\"configurable\":false} true true false false a true false true false true\n0,1,2,x \"abc\" {\"0\":\"o\",\"1\":\"k\"}\n",
  )
}

pub fn json_through_proxy_and_raw_json_diff_test() {
  diff(
    "var p=new Proxy({a:[1,{b:2}],c:'x'},{});console.log(JSON.stringify(p),JSON.stringify({r:JSON.rawJSON('99')}),JSON.isRawJSON(JSON.rawJSON('1')),JSON.isRawJSON(Object.freeze({rawJSON:'1'})),JSON.stringify(new Proxy([1,2],{})),Object.prototype.toString.call(new Proxy(function(){},{})),JSON.stringify(p,['c']))",
    "{\"a\":[1,{\"b\":2}],\"c\":\"x\"} {\"r\":99} true false [1,2] [object Function] {\"c\":\"x\"}\n",
  )
}

// ── §10.2.1.2 OrdinaryCallBindThis: strict vs sloppy `this` ─────────────────

pub fn strict_and_sloppy_this_diff_test() {
  diff(
    "function sl(){return typeof this}function st(){'use strict';return typeof this}function slg(){return this===globalThis}function stu(){'use strict';return this}console.log(sl.call(5),st.call(5),sl.call('s'),st.call(true),slg(),stu(),slg.call(null),stu.call(null),sl.call(undefined));var o={m:sl,n:st};console.log(o.m(),o.n(),(0,o.m)(),(0,o.n)())",
    "object number object boolean true undefined true null object\nobject object object undefined\n",
  )
}

// ── One Number type: 2^53 widening and -0 (N38) ────────────────────────────

pub fn big_integers_widen_to_double_diff_test() {
  diff(
    "var m=9007199254740991;console.log(m+1===m+2,9007199254740992+1,m+2,2**53+2,m*3,m*m,-m-2,(m+1)-(-3));var x=1;for(var i=0;i<60;i++)x=x+x;console.log(x,x+1===x,x*2);var c=m;c++;c++;console.log(c,String(m*1000).length);console.log(123456789*987654321,99999999999*99999999999,parseInt('9007199254740993'),Number('18014398509481985'))",
    "true 9007199254740992 9007199254740992 9007199254740994 27021597764222972 8.112963841460666e+31 -9007199254740992 9007199254740996\n1152921504606847000 true 2305843009213694000\n9007199254740992 19\n121932631112635260 9.9999999998e+21 9007199254740992 18014398509481984\n",
  )
}

pub fn minus_zero_is_preserved_diff_test() {
  diff(
    "function d(v){return 1/v===-Infinity?'-0':String(v)}var z=0,n=-1,p=5;console.log(d(-0),d(0*-1),d(z*n),d(n*z),d(z*p),d(-z),d(-0+-0),d(-0+0),d(0-0),d(-4%2),d(4%-2),d(z/-5),d(Math.round(-0.4)),d(-p*0));console.log(Object.is(-0,0),Object.is(z*n,-0),1/-0,(-0).toString(),JSON.stringify(-0),JSON.stringify([z*n]),String(-0),-0===0,[-0].includes(0),Math.max(-0,0)===0&&1/Math.max(-0,0));var o={};o[-0]='k';console.log(Object.keys(o).join());var q=0;q*= -1;console.log(d(q),d(q+1-1));var ng=-1,nz=-0;console.log(ng,ng==-1,5&ng,d(nz),d(nz*ng))",
    "-0 -0 -0 -0 0 -0 -0 0 0 -0 0 -0 -0 -0\nfalse true -Infinity 0 0 [0] 0 true true Infinity\n0\n-0 0\n-1 true 5 -0 0\n",
  )
}

/// A user species constructor can hand the reaction job a throwing
/// `resolve`; the job has no caller, so the throw is reported to the host
/// sink instead of vanishing.
pub fn throwing_species_resolve_is_reported_test() {
  let c =
    harness.run_compiled(
      "var p=Promise.resolve(1);function C(ex){ex(function(){throw new Error('boom')},function(){})}C[Symbol.species]=C;Promise.prototype.constructor=C;p.then(function(v){console.log('ran',v);return v})",
    )
  assert c.stdout == <<"ran 1\n":utf8>>
  assert harness.err_read()
    == <<"Uncaught (in promise job) Error: boom\n":utf8>>
}

// ── Host hooks ──────────────────────────────────────────────────────────────

/// console.log/info/debug reach the stdout level of the print hook and
/// console.warn/error the stderr level, so only the former are diffed.
pub fn console_levels_split_test() {
  let c =
    harness.run_compiled(
      "console.log('a');console.warn('b');console.error('c',1);console.info('d');console.debug('e')",
    )
  assert c.stdout == <<"a\nd\ne\n":utf8>>
  assert harness.err_read() == <<"b\nc 1\n":utf8>>
}

/// `Date.now()` and `new Date()` read the wall-clock hook, which the harness
/// pins to `fixed_now_ms`.
pub fn date_now_reads_wall_clock_hook_test() {
  let c =
    harness.run_compiled(
      "console.log(Date.now(), new Date().getTime()===Date.now())",
    )
  assert c.stdout == <<"1700000000000 true\n":utf8>>
}
