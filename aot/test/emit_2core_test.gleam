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

// Constructor field adds after the shape transition is cached: a setter, a
// non-writable data property and a Proxy on the proto chain must all still
// intercept `this.k = v`; a same-named writable slot on a shaped proto must
// not.
const ctor_add_setter_src = "function P(){this.x=1;this.y=2};new P();new P();Object.defineProperty(P.prototype,'y',{set:function(v){this.z=v*10}});var p=new P();console.log(''+p.x+p.y+p.z)"

pub fn ctor_add_setter_diff_test() {
  let i = harness.run_interpreted(ctor_add_setter_src)
  let c = harness.run_compiled(ctor_add_setter_src)
  assert i.stdout == <<"1undefined20\n":utf8>>
  assert c.stdout == i.stdout
}

const ctor_add_readonly_src = "function P(){this.x=1;this.y=2};new P();new P();Object.defineProperty(P.prototype,'x',{value:7,writable:false});var p=new P();console.log(''+p.x+p.y+Object.keys(p))"

pub fn ctor_add_readonly_diff_test() {
  let i = harness.run_interpreted(ctor_add_readonly_src)
  let c = harness.run_compiled(ctor_add_readonly_src)
  assert i.stdout == <<"72y\n":utf8>>
  assert c.stdout == i.stdout
}

const ctor_add_proxy_src = "function P(){this.x=1};new P();new P();P.prototype=new Proxy({},{set:function(t,k,v,r){console.log('trap:'+k);return Reflect.set(t,k,v,r)}});var p=new P();console.log(''+p.x+Object.keys(p))"

pub fn ctor_add_proxy_diff_test() {
  let i = harness.run_interpreted(ctor_add_proxy_src)
  let c = harness.run_compiled(ctor_add_proxy_src)
  assert i.stdout == <<"trap:x\n1x\n":utf8>>
  assert c.stdout == i.stdout
}

const ctor_add_shaped_proto_src = "function A(){this.m=1};var proto=new A();function B(){this.m=2};B.prototype=proto;new B();var b=new B();console.log(''+b.m+proto.m+Object.keys(b))"

pub fn ctor_add_shaped_proto_diff_test() {
  let i = harness.run_interpreted(ctor_add_shaped_proto_src)
  let c = harness.run_compiled(ctor_add_shaped_proto_src)
  assert i.stdout == <<"21m\n":utf8>>
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

/// Code after a try/catch runs outside it: a later throw in the same function
/// must not land in the earlier handler, and a break out of a try body must
/// leave it.
pub fn throw_after_try_diff_test() {
  diff(
    "function f(){try{}catch(e){return 'wrong'}throw 1}try{console.log(f())}catch(e){console.log('ok',e)}function g(){for(;;){try{break}catch(e){return 'wrong'}}throw 2}try{console.log(g())}catch(e){console.log('ok',e)}",
    "ok 1\nok 2\n",
  )
}

/// A finally block that throws while a return/break crosses it runs once, and
/// its throw is not seen by that try's own catch; an iterator close throwing
/// on `return` inside for-of is not caught by an inner try/catch either.
pub fn finally_throws_once_diff_test() {
  diff(
    "var n=0;function f(){try{return 1}finally{n++;throw 2}}try{f()}catch(e){console.log('A',e,n)}n=0;function g(){try{return 1}catch(e){console.log('inner',e)}finally{n++;throw 2}}try{g()}catch(e){console.log('B',e,n)}n=0;function h(){for(var i=0;i<1;i++){try{break}finally{n++;throw 3}}}try{h()}catch(e){console.log('C',e,n)}n=0;function k(){try{try{return 1}finally{n++;throw 4}}catch(e){console.log('k',e,n);return 9}}console.log('D',k(),n);var rc=0,fe=0,ce=0;var it={};it[Symbol.iterator]=function(){return{next(){return{done:false}},return(){rc++;throw 42}}};function m(){for(var x of it){try{return}catch(e){ce++}finally{fe++}}}try{m()}catch(e){console.log('E',e,rc,ce,fe)}",
    "A 2 1\nB 2 1\nC 3 1\nk 4 1\nD 9 1\nE 42 1 0 1\n",
  )
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

pub fn uint8_base64_hex_diff_test() {
  diff(
    "var u=new Uint8Array([251,255,191,1,2]);var t=new Uint8Array(4);var r=t.setFromBase64('AQID BAU=',{lastChunkHandling:'stop-before-partial'});var h=new Uint8Array(3);var w=h.setFromHex('a0b1c2');console.log(u.toBase64(),u.toBase64({alphabet:'base64url',omitPadding:true}),u.toHex(),Uint8Array.fromHex('DEADbeef').join(),Uint8Array.fromBase64(' +/8 = ',{lastChunkHandling:'loose'}).join(),Uint8Array.fromBase64('-_8',{alphabet:'base64url'}).join(),r.read,r.written,t.join(),w.read,w.written,h.join());try{Uint8Array.fromHex('abc')}catch(e){console.log(e.name,e.message)}try{Uint8Array.fromBase64('QQ',{lastChunkHandling:'strict'})}catch(e){console.log(e.name)}try{u.toBase64({alphabet:'nope'})}catch(e){console.log(e.name,e.message)}try{Uint8Array.fromHex(1)}catch(e){console.log(e.name,e.message)}",
    "+/+/AQI= -_-_AQI fbffbf0102 222,173,190,239 251,255 251,255 4 3 1,2,3,0 6 3 160,177,194\nSyntaxError unable to decode hex string\nSyntaxError\nTypeError \"nope\" is not a valid value for option alphabet\nTypeError expected input to be a string, got number\n",
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

pub fn json_parse_reviver_context_source_diff_test() {
  diff(
    "var l=[];JSON.parse('{\"a\":[1.5,\"s\\\\n\",{\"b\":null}],\"c\":true}',function(k,v,c){l.push(k+':'+(c&&c.source));return v});console.log(l.join());var m=[];JSON.parse('[1,2,{\"p\":3}]',function(k,v,c){m.push(k+':'+(c&&c.source));if(k==='0')this[1]=9;if(k==='1')this[2]={q:4};return this[k]});console.log(m.join())",
    "0:1.5,1:\"s\\n\",b:null,2:undefined,a:undefined,c:true,:undefined\n0:1,1:undefined,q:undefined,2:undefined,:undefined\n",
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

/// Float arithmetic whose true result passes 1.8e308 is ±Infinity on both
/// paths (native `+ - * /`, `**`, Math), never a `badarith`.
pub fn float_overflow_is_infinity_diff_test() {
  diff(
    "var b=1e308,m=Number.MAX_VALUE;function f(x,y){return [x*10,-x*10,x+x,-x-x,y*2,x/1e-10,x/-1e-10,2**1024,(-10)**401,Math.pow(10,400),Math.exp(1000),x*x-x*x]}console.log(f(b,m).join());var x=b;x*=10;var y=-b;y-=b;console.log(x,y,x===Infinity,1e309,-1e309,parseFloat('1e400'),+'-1e400',isFinite(b*10))",
    "Infinity,-Infinity,Infinity,-Infinity,Infinity,Infinity,-Infinity,Infinity,-Infinity,Infinity,Infinity,NaN\nInfinity -Infinity true Infinity -Infinity Infinity -Infinity false\n",
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

// ── console.log rendering (arc/rt/inspect) ─────────────────────────────────

pub fn console_renders_values_diff_test() {
  diff(
    "console.log({a:1,b:[1,2]}, new Map([[1,2]]), function f(){}, Symbol('s'), 12n, -0, 'str', [ , 1])",
    "{ a: 1, b: [ 1, 2 ] } Map(1) [Function: f] Symbol(s) 12n 0 str [ <empty>, 1 ]\n",
  )
}

pub fn console_renders_nested_and_cyclic_diff_test() {
  diff(
    "var o={x:1};o.self=o;var a=[1];a.push(a);console.log(o, a, {n:{m:{k:{j:1}}}}, [[[[1]]]], {s:'q\\'x\\n'}, ['t'], null, undefined, true, 1.5, NaN, -Infinity)",
    "{ x: 1, self: [Circular] } [ 1, [Circular] ] { n: { m: { k: [Object] } } } [ [ [ [Array] ] ] ] { s: 'q\\'x\\n' } [ 't' ] null undefined true 1.5 NaN -Infinity\n",
  )
}

pub fn console_renders_exotics_diff_test() {
  diff(
    "console.log(new Set([1,2,3]), new WeakMap, /a/g, new RegExp(''), new Date(0), new Date(NaN), ()=>1, async function af(){}, function*gg(){}, (function*(){})(), Promise.resolve(1));console.log([1,2,3].values(), new Map().entries(), 'ab'[Symbol.iterator](), new Set().values(), Object.create(null), {[Symbol.toStringTag]:'Tag',v:1}, {get g(){return 1},e:2}, Object.defineProperty({},'h',{value:1}), new Proxy({},{}), new Proxy(function(){},{}))",
    "Set(3) WeakMap {} /a/g /(?:)/ Date(0) Invalid Date [Function (anonymous)] [Function: af] [Function: gg] Object [Generator] {} Promise {}\nObject [Array Iterator] {} Object [Map Iterator] {} Object [String Iterator] {} Object [Set Iterator] {} {} Object [Tag] { v: 1 } { e: 2 } {} Proxy {} [Function (Proxy)]\n",
  )
  diff(
    "console.log(new Number(3), new String('s'), new Boolean(false), Object(1n), Object(Symbol('q')), new ArrayBuffer(8), new Uint8Array(4), new DataView(new ArrayBuffer(2)), new SharedArrayBuffer(4), (function(){return arguments})(1,2), JSON.rawJSON('7'), {0:'z',b:1,1:'y'}, [1,,3], new Array(5), Object.assign([1],{p:2}));console.log('%o and %O and %s', 'q', [1], {a:'b'}, {c:1});console.log([function(){}, Math.max], {['k y']:1, 3:2})",
    "[Number: 3] [String: 's'] [Boolean: false] [BigInt: 1n] [Symbol: Symbol(q)] ArrayBuffer { byteLength: 8 } Uint8Array(4) DataView {} SharedArrayBuffer { byteLength: 4 } [Arguments] [ 1, 2 ] [RawJSON 7] { 0: 'z', 1: 'y', b: 1 } [ 1, <empty>, 3 ] [ <empty>, <empty>, <empty>, <empty>, <empty> ] [ 1 ]\n'q' and [ 1 ] and [object Object] { c: 1 }\n[ [Function (anonymous)], [Function: max] ] { 3: 2, k y: 1 }\n",
  )
}

/// Errors render as their captured stack. Compiled code records no frames,
/// so only the `Name: message` header appears (the interpreter adds
/// `at script:N` lines, hence no differential here).
pub fn console_renders_errors_test() {
  let c =
    harness.run_compiled(
      "var e=new Error('m');e.name='';console.log(new Error('e'), new TypeError, e, Object.assign(new Error('x'),{name:'N'}), new AggregateError([],'agg'), [new RangeError('r')]);Promise.reject({code:4})",
    )
  assert c.stdout
    == <<
      "Error: e TypeError Error: m Error: x AggregateError: agg [ RangeError: r ]\n":utf8,
    >>
  assert harness.err_read() == <<"Uncaught (in promise) { code: 4 }\n":utf8>>
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

/// Local-time Date methods under the harness's UTC zone: component
/// construction, setters, string forms, parse round-trips and the Annex B
/// year accessors.
pub fn date_local_time_diff_test() {
  diff(
    "var d=new Date(2021,0,15,12,5,9,7);console.log(d.getTime(),d.getDay(),d.getHours(),d.getTimezoneOffset(),d.toString(),d.toISOString(),JSON.stringify(d),d.toUTCString(),d.toLocaleString());d.setMonth(13,29);console.log(d.toDateString(),d.getYear(),d.setYear(99),d.getFullYear());console.log(Date.parse('2021-01-15T12:00'),Date.parse(d.toISOString())===d.getTime(),Date.parse('2021-02-29'),new Date(NaN)+'',Date.UTC(99,11,31,23,59,59,999),new Date(0,0).getFullYear(),String(Date()).length===String(new Date()).length,typeof Date.now())",
    "1610712309007 5 12 0 Fri Jan 15 2021 12:05:09 GMT+0000 2021-01-15T12:05:09.007Z \"2021-01-15T12:05:09.007Z\" Fri, 15 Jan 2021 12:05:09 GMT 1/15/2021, 12:05:09 PM\nTue Mar 01 2022 122 920289909007 1999\n1610712000000 true NaN Invalid Date 946684799999 1900 true number\n",
  )
  diff(
    "var d=new Date(8.64e15);console.log(d.toISOString(),new Date(8.64e15+1).getTime(),d.setMilliseconds(1));try{d.toISOString()}catch(e){console.log(e.name)}var o={valueOf:function(){d2.setTime(5);return 1}};var d2=new Date(NaN);console.log(d2.setDate(o),d2.getTime(),d2.setFullYear(2020),new Date(2020,0).getTime());console.log(d[Symbol.toPrimitive]('number'),Object.prototype.toString.call(d),d.toJSON())",
    "+275760-09-13T00:00:00.000Z NaN NaN\nRangeError\nNaN 5 1577836800005 1577836800000\nNaN [object Date] null\n",
  )
}

/// A promise left rejected at the end of the drain is reported once through
/// `report_uncaught`, with the same text on both paths (an Error reason would
/// differ only by the interpreter's `at script:N` trace lines).
pub fn unhandled_rejection_report_diff_test() {
  let src =
    "Promise.reject('nobody');var p=Promise.reject('later');Promise.resolve().then(function(){p.catch(function(){})});console.log('main')"
  harness.buf_reset()
  let i = harness.run_interpreted(src)
  let i_err = harness.err_read()
  let c = harness.run_compiled(src)
  assert i.stdout == <<"main\n":utf8>>
  assert c.stdout == i.stdout
  assert i_err == <<"Uncaught (in promise) nobody\n":utf8>>
  assert harness.err_read() == i_err
  let c = harness.run_compiled("Promise.reject(new RangeError('e'))")
  let assert Ok(_) = c.result
  assert harness.err_read() == <<"Uncaught (in promise) RangeError: e\n":utf8>>
}

// ── Promises and generators are ordinary objects (N21) ─────────────────────

pub fn promise_and_generator_take_own_properties_diff_test() {
  diff(
    "var p=Promise.resolve(1);p.tag='t';function* g(){yield 1;yield 2}var it=g();it.n=9;console.log(p.tag,Object.isExtensible(p),Object.keys(p).join(),it.n,Object.isExtensible(it),it.next().value,it.n,Object.keys(it).join());Object.preventExtensions(p);console.log(Object.isExtensible(p),p instanceof Promise,Object.getPrototypeOf(it)===Object.getPrototypeOf(g()))",
    "t true tag 9 true 1 9 n\nfalse true true\n",
  )
}

/// A promise subclass built with `Reflect.construct`: instances get the
/// subclass prototype from `new.target`, and `then` goes through the species
/// constructor so its result is a subclass instance too.
pub fn promise_subclass_via_new_target_diff_test() {
  diff(
    "function P(ex){return Reflect.construct(Promise,[ex],P)}Object.setPrototypeOf(P,Promise);P.prototype=Object.create(Promise.prototype);P.prototype.constructor=P;var q=new P(function(r){r(7)});var c=q.then(function(v){console.log('v',v)});console.log(q instanceof P,Object.getPrototypeOf(q)===P.prototype,c instanceof P,q instanceof Promise)",
    "true true true true\nv 7\n",
  )
}

// ── Classes, function prologues and NamedEvaluation ────────────────────────

/// Reads of `let`/`const`/class bindings before their declaration ran throw; a
/// named function expression sees itself.
pub fn lexical_tdz_reads_diff_test() {
  diff(
    "try { console.log(x); } catch (e) { console.log('tdz:', e.constructor.name) }\nlet x = 1;\n{ try { z } catch (e) { console.log('tdz3:', e.constructor.name) } const z = 3; console.log(z) }\nfunction g(){ try { w } catch (e) { console.log('tdz4:', e.constructor.name) } let w = 4; return w } console.log(g());\ntry { new K() } catch (e) { console.log('class:', e.constructor.name) } class K {}\nvar f = function self(){ return typeof self }; console.log(f());",
    "tdz: ReferenceError\ntdz3: ReferenceError\n3\ntdz4: ReferenceError\n4\nclass: ReferenceError\nfunction\n",
  )
}

/// Methods, accessors, statics, `name`/`length`, the non-writable `prototype`,
/// non-enumerable members and the no-`new` TypeError.
pub fn class_basics_diff_test() {
  diff(
    "class A { constructor(x){ this.x = x } get(){ return this.x } static s(){ return 's' } get g(){ return this.x*2 } set g(v){ this.x = v } }\nvar a = new A(3);\nconsole.log(a.get(), A.s(), a.g, typeof A, A.name, A.length, a instanceof A, Object.getPrototypeOf(a) === A.prototype);\na.g = 10; console.log(a.x, a.get());\nconsole.log(Object.keys(A.prototype).length, Object.getOwnPropertyNames(A.prototype).join(), JSON.stringify(Object.getOwnPropertyDescriptor(A,'prototype')));\nconsole.log(A.prototype.get.name, A.s.name, Object.getOwnPropertyDescriptor(A.prototype,'g').get.name, A.prototype.get.length);\ntry { A() } catch (e) { console.log(e.constructor.name) }\ntry { new a.get() } catch (e) { console.log(e.constructor.name) }\nclass B {}\nconsole.log(new B() instanceof B, B.name, B.length, typeof B.prototype.constructor, B.prototype.constructor === B);",
    "3 s 6 function A 1 true true\n10 10\n0 constructor,get,g {\"value\":{},\"writable\":false,\"enumerable\":false,\"configurable\":false}\nget s get g 0\nTypeError\nTypeError\ntrue B 0 function true\n",
  )
}

/// NamedEvaluation for class/function/arrow expressions in declarators,
/// assignments and object literals; object literal members are enumerable.
pub fn named_evaluation_diff_test() {
  diff(
    "let C = class {};\nvar D = class Inner { static n(){ return Inner.name } };\nconst E = class { static m(){} };\nvar o = { F: class {} };\nconsole.log(C.name, D.name, D.n(), E.name, o.F.name, (class {}).name, (class Z {}).name);\nvar f = function(){}; let g = () => 1; const h = function named(){};\nvar o2 = { i: function(){}, j: () => 0, k(){}, ['l' + 1]: function(){} };\nconsole.log(f.name, g.name, h.name, o2.i.name, o2.j.name, o2.k.name, o2.l1.name);\nvar m; m = function(){}; var n2; n2 = () => {}; var p; p = class {};\nconsole.log(m.name, n2.name, p.name);\nconsole.log(Object.keys(o2).join(), JSON.stringify(Object.getOwnPropertyDescriptor(o2,'k'), ['writable','enumerable','configurable']));",
    "C Inner Inner E F  Z\nf g named i j k \nm n2 p\ni,j,k,l1 {\"writable\":true,\"enumerable\":true,\"configurable\":true}\n",
  )
}

/// `extends` a class, a plain function and null; `super.m()` in instance and
/// static methods; heritage validation.
pub fn class_extends_diff_test() {
  diff(
    "class A { constructor(x){ this.x = x; this.tag = 'A' } m(){ return 'A.m:' + this.x } static sm(){ return 'A.sm' } }\nclass B extends A { constructor(x, y){ super(x); this.y = y } m(){ return 'B>' + super.m() + ':' + this.y } static sm(){ return 'B>' + super.sm() } }\nvar b = new B(1, 2);\nconsole.log(b.m(), B.sm(), b instanceof A, b instanceof B, b.tag, Object.getPrototypeOf(B) === A, Object.getPrototypeOf(B.prototype) === A.prototype);\nclass C extends A {}\nvar c = new C(7); console.log(c.x, c.m(), C.length);\nclass N extends null { constructor(){ return Object.create(N.prototype) } }\nconsole.log(Object.getPrototypeOf(N.prototype), new N() instanceof N, Object.getPrototypeOf(N) === Function.prototype);\ntry { class X extends 3 {} } catch (e) { console.log(e.constructor.name) }\nfunction F(){ this.f = 1 } F.prototype.fm = function(){ return 'fm' };\nclass G extends F { constructor(){ super(); this.g = 2 } }\nvar g = new G(); console.log(g.f, g.g, g.fm());",
    "B>A.m:1:2 B>A.sm true true A true true\n7 A.m:7 0\nnull true true\nTypeError\n1 2 fm\n",
  )
}

/// `this` before `super()`, double `super()`, missing `super()`, and derived
/// return-override rules.
pub fn derived_constructor_this_diff_test() {
  diff(
    "class A { constructor(){ this.a = 1 } }\nclass B extends A { constructor(){ try { this.x = 1 } catch (e) { console.log('before:', e.constructor.name) } super(); console.log('after', this.a); try { super() } catch (e) { console.log('double:', e.constructor.name) } } }\nnew B();\nclass C extends A { constructor(){ } }\ntry { new C() } catch (e) { console.log('nosuper:', e.constructor.name) }\nclass D extends A { constructor(){ return 5 } }\ntry { new D() } catch (e) { console.log('prim:', e.constructor.name) }\nclass E extends A { constructor(){ return {z:1} } }\nconsole.log(new E().z, new E() instanceof E);\nclass F2 extends A { constructor(){ super(); return undefined } }\nconsole.log(new F2().a);\nclass G extends A { constructor(){ var f = () => this; var s = () => super(); s(); console.log('arrow', f().a) } }\nnew G();",
    "before: ReferenceError\nafter 1\ndouble: ReferenceError\nnosuper: ReferenceError\nprim: TypeError\n1 false\n1\narrow 1\n",
  )
}

/// Subclassing Array, Error, Map, Promise, Uint8Array, TypeError and Object.
pub fn class_extends_natives_diff_test() {
  diff(
    "class MyArr extends Array { sum(){ return this.reduce((a,b)=>a+b, 0) } }\nvar m = new MyArr(); m.push(1,2,3);\nconsole.log(m.length, m.sum(), Array.isArray(m), m instanceof MyArr, m instanceof Array, MyArr.from([1]) instanceof MyArr);\nclass MyErr extends Error { constructor(msg){ super(msg); this.name = 'MyErr' } }\nvar e = new MyErr('boom');\nconsole.log(e.message, e.name, e instanceof Error, e instanceof MyErr, String(e), Object.prototype.toString.call(e));\ntry { throw new MyErr('t') } catch (x) { console.log(x instanceof MyErr, x.message) }\nclass MyMap extends Map { setx(k,v){ return super.set(k, v*2) } }\nvar mm = new MyMap([[1,1]]); mm.setx(2, 5); console.log(mm.get(1), mm.get(2), mm.size, mm instanceof Map);\nclass P extends Promise { }\nvar p = new P(function(r){ r(3) }); console.log(p instanceof P, p instanceof Promise, p.then(function(){}) instanceof P);\np.then(function(v){ console.log('v', v) });\nclass U extends Uint8Array { }\nvar u = new U(3); u[0] = 300; console.log(u.length, u[0], u instanceof U, u instanceof Uint8Array);\nclass TE extends TypeError {}\nconsole.log(new TE('x').name, new TE('x') instanceof TypeError, Object.getPrototypeOf(TE) === TypeError);\nclass O extends Object { constructor(){ super(); this.q = 1 } }\nconsole.log(new O().q);",
    "3 6 true true true true\nboom MyErr true true MyErr: boom [object Error]\ntrue t\n1 10 2 true\ntrue true true\n3 44 true true\nTypeError true true\n1\nv 3\n",
  )
}

/// Instance and static public fields: computed keys, `this` in initializers,
/// arrows capturing `this`, evaluation order, initializer names.
pub fn class_fields_diff_test() {
  diff(
    "var k = 'comp';\nclass A { x = 1; y = this.x + 1; [k] = 3; [k + '2'] = this.y; static s = 10; static t = A.s + 1; static u = this.s + 2; f = () => this.x; 'q r' = 5; 42 = 'n'; z; }\nvar a = new A();\nconsole.log(a.x, a.y, a.comp, a.comp2, A.s, A.t, A.u, a.f(), a['q r'], a[42], 'z' in a, a.z, Object.keys(a).join());\nclass B extends A { w = this.x + 100; constructor(){ super(); this.after = this.w } }\nvar b = new B(); console.log(b.w, b.after, b.x);\nclass C { fn = function(){}; arrow = () => {}; cls = class {}; static sf = function(){}; }\nvar c = new C(); console.log(c.fn.name, c.arrow.name, c.cls.name, C.sf.name);\nvar i = 0; class D { [i++] = i++; [i++] = i++; } var d = new D(); console.log(Object.keys(d).join(), d[0], d[1], i);",
    "1 2 3 2 10 11 12 1 5 n true undefined 42,x,y,comp,comp2,f,q r,z\n101 101 1\nfn arrow cls sf\n0,1 2 3 4\n",
  )
}

/// Private fields, methods, accessors and statics; `#x in o` brand checks;
/// wrong-receiver TypeErrors; fresh names per class evaluation.
pub fn class_private_members_diff_test() {
  diff(
    "class A {\n  #x = 1; #y; static #s = 's';\n  #m(){ return '#m' + this.#x }\n  get #g(){ return this.#x * 10 } set #g(v){ this.#x = v }\n  static #sm(){ return A.#s }\n  read(){ return [this.#x, this.#y, this.#m(), this.#g, A.#sm()].join() }\n  write(){ this.#g = 5; this.#y = 'y'; this.#x++; return this.read() }\n  static has(o){ return #x in o }\n  static readOther(o){ return o.#x }\n}\nvar a = new A();\nconsole.log(a.read()); console.log(a.write());\nconsole.log(A.has(a), A.has({}), Object.keys(a).length, Object.getOwnPropertyNames(a).length);\ntry { A.readOther({}) } catch (e) { console.log(e.constructor.name) }\ntry { A.has(1) } catch (e) { console.log(e.constructor.name) }\nclass B { #v; constructor(v){ this.#v = v } static eq(a, b){ return a.#v === b.#v } }\nconsole.log(B.eq(new B(1), new B(1)), B.eq(new B(1), new B(2)));\nfunction mk(){ return class { #p = 1; static has(o){ return #p in o } } }\nvar K1 = mk(), K2 = mk();\nconsole.log(K1.has(new K1()), K1.has(new K2()), K2.has(new K2()));\nclass M { #meth(){} static test(o){ try { o.#meth; return true } catch (e) { return e.constructor.name } } }\nconsole.log(M.test(new M()), M.test({}));\nclass W { #w = 1; static setOn(o){ try { o.#w = 2; return 'ok' } catch(e){ return e.constructor.name } } #ro(){} static callSet(o){ try { o.#ro = 1; return 'ok' } catch(e){ return e.constructor.name } } }\nconsole.log(W.setOn(new W()), W.setOn({}), W.callSet(new W()));",
    "1,,#m1,10,s\n6,y,#m6,60,s\ntrue false 0 0\nTypeError\nTypeError\ntrue false\ntrue false true\ntrue TypeError\nok TypeError TypeError\n",
  )
}

/// Static blocks interleaved with static fields, `this` and private access
/// inside them.
pub fn class_static_blocks_diff_test() {
  diff(
    "var log = [];\nclass A { static x = 1; static { log.push('blk1:' + this.x); this.y = 2 } static z = 3; static { log.push('blk2:' + this.y + this.z); A.w = 4 } }\nconsole.log(log.join(), A.w, A.y);\nclass B { static #p = 5; static { log.push(B.#p) } }\nconsole.log(log.join());\nclass C { static { var inner = 1; log.push(typeof inner) } }\nconsole.log(log.join(), typeof inner);",
    "blk1:1,blk2:23 4 2\nblk1:1,blk2:23,5\nblk1:1,blk2:23,5,number undefined\n",
  )
}

/// `new.target` in constructors, functions and arrows; `Symbol.hasInstance`;
/// `Reflect.construct` with a foreign new.target.
pub fn new_target_and_has_instance_diff_test() {
  diff(
    "class A { constructor(){ this.nt = new.target.name } }\nclass B extends A {}\nconsole.log(new A().nt, new B().nt);\nfunction F(){ return new.target } console.log(F() === undefined, new F() instanceof F ? 'obj' : 'nt');\nfunction G(){ this.v = new.target === G } console.log(new G().v);\nclass H { static [Symbol.hasInstance](v){ return v === 1 } }\nconsole.log(1 instanceof H, {} instanceof H, new H() instanceof H);\nclass I {} console.log(I[Symbol.hasInstance] === Function.prototype[Symbol.hasInstance]);\nvar R = Reflect.construct(A, [], B); console.log(R.nt, R instanceof B);\nclass J { constructor(){ this.t = typeof new.target; var f = () => new.target; this.a = f() === J } }\nconsole.log(new J().t, new J().a);",
    "A B\ntrue nt\ntrue\ntrue false false\ntrue\nB true\nfunction true\n",
  )
}

/// Computed method/accessor keys and their names, accessor pair merging across
/// a hierarchy, `static prototype` rejection.
pub fn class_computed_keys_and_accessors_diff_test() {
  diff(
    "var s = Symbol('sym'); var n = 0;\nclass A { ['a' + 'b'](){ return 'ab' } get [s](){ return 'gs' } static ['st' + (++n)](){ return n } [1 + 1](){ return 2 } get x(){ return this._x } set x(v){ this._x = v } static get y(){ return 'Y' } static set y(v){ A._y = v } }\nvar a = new A();\nconsole.log(a.ab(), a[s], A.st1(), a[2](), A.prototype.ab.name, Object.getOwnPropertyDescriptor(A.prototype, s).get.name, A.st1.name, A.prototype[2].name);\na.x = 4; console.log(a.x, A.y); A.y = 9; console.log(A._y);\nvar d = Object.getOwnPropertyDescriptor(A.prototype, 'x'); console.log(typeof d.get, typeof d.set, d.enumerable, d.configurable);\nclass B { get v(){ return 1 } } class C extends B { set v(x){ this._v = x } }\nvar c = new C(); c.v = 3; console.log(c.v, c._v);\nvar key = { toString(){ return 'dyn' } };\nclass D { [key](){ return 'd' } static [key] = 1; [key] = 2 }\nconsole.log(new D().dyn, D.dyn, typeof D.prototype.dyn);\ntry { class E { static ['prototype'](){} } } catch (e) { console.log(e.constructor.name) }\nclass F { static ['constructor'](){ return 'sc' } ['constructor'](){ return 'pc' } }\nconsole.log(F.constructor(), new F().constructor === F, F.prototype.hasOwnProperty('constructor'));",
    "ab gs 1 2 ab get [sym] st1 2\n4 Y\n9\nfunction function false true\nundefined 3\n2 1 function\nTypeError\nsc false true\n",
  )
}

/// Generator, async and async-generator methods; generator functions get their
/// own `prototype` object.
pub fn class_generator_and_async_methods_diff_test() {
  diff(
    "class A { *g(){ yield 1; yield this.k } async am(){ return this.k } static *sg(){ yield 's' } static async sa(){ return 'sa' } async *ag(){ yield 1 } constructor(){ this.k = 'k' } }\nvar a = new A();\nconsole.log([...a.g()].join(), [...A.sg()].join(), typeof a.am().then, Object.getPrototypeOf(a.g()) === a.g.prototype, typeof A.prototype.g.prototype, A.prototype.am.prototype);\na.am().then(v => console.log('am', v)); A.sa().then(v => console.log('sa', v));\na.ag().next().then(r => console.log('ag', r.value, r.done));\nfunction* gf(){} var ge = function*(){}; async function af(){}\nconsole.log(Object.getPrototypeOf(gf()) === gf.prototype, Object.getPrototypeOf(gf.prototype) === Object.getPrototypeOf(function*(){}).prototype, gf.prototype !== ge.prototype, af.prototype, Object.getPrototypeOf(ge()) === ge.prototype, gf.hasOwnProperty('prototype'), Object.keys(gf).length);\nconsole.log(Object.getPrototypeOf(gf.prototype) === Object.getPrototypeOf(gf).prototype);\ntry { new a.g() } catch (e) { console.log(e.constructor.name) }",
    "1,k s function true object undefined\ntrue true true undefined true true 0\ntrue\nTypeError\nam k\nsa sa\nag 1 false\n",
  )
}

/// Class `toString`, per-iteration class evaluation, `new this()`, the
/// immutable inner name binding.
pub fn class_identity_and_scoping_diff_test() {
  diff(
    "class A { m(){} }\nconsole.log(String(A).slice(0, 9), typeof A.toString(), Object.prototype.toString.call(A), Object.prototype.toString.call(new A()));\nclass B { constructor(a, b = 1, ...c){} }\nconsole.log(B.length);\nfor (var i = 0, cs = []; i < 3; i++) cs.push(class { static i = i; #p = i; static get(o){ return o.#p } });\nconsole.log(cs[0].i, cs[2].i, cs[1].get(new cs[1]()), cs[0] === cs[1]);\ntry { cs[0].get(new cs[1]()) } catch (e) { console.log(e.constructor.name) }\nclass C { static create(){ return new this() } who(){ return 'C' } }\nclass D extends C { who(){ return 'D' } }\nconsole.log(D.create().who(), C.create().who());\nvar E = class Named { self(){ return Named } };\nconsole.log(new E().self() === E, typeof Named);\ntry { E = class { [E2](){} }; var E2 } catch(e){ console.log(e.constructor.name) }\nclass F { static m(){ F = null } }\ntry { F.m() } catch (e) { console.log('inner-const:', e.constructor.name) }\nclass G {} G = 1; console.log(G);\nconsole.log(typeof class {}, (class { static x(){ return this } }).x().name);",
    "function  string [object Function] [object Object]\n1\n0 2 3 false\nTypeError\nD C\ntrue undefined\ninner-const: TypeError\n1\nfunction \n",
  )
}

/// The default derived constructor forwards arguments without iterating them;
/// `super.x` reads and writes; object literal `super`.
pub fn derived_default_constructor_and_super_property_diff_test() {
  diff(
    "class A { constructor(...args){ this.args = args } }\nclass B extends A {}\nconsole.log(new B(1,2,3).args.join(), B.length);\nvar log = [];\nvar iter = Array.prototype[Symbol.iterator];\nArray.prototype[Symbol.iterator] = function(){ log.push('iter'); return iter.call(this) };\nnew B(4,5);\nArray.prototype[Symbol.iterator] = iter;\nconsole.log(log.length);\nclass C extends B { constructor(){ super(...[7,8]) } }\nconsole.log(new C().args.join());\nclass M { m(){ return 'm' } }\nclass N extends M { m(){ var f = () => super.m(); return 'n' + f() } set p(v){ super.p = v } get hp(){ return super.hasOwnProperty('p') } }\nvar n = new N(); n.p = 3; console.log(n.m(), n.p, n.hp, Object.keys(n).join());\nvar o = { __proto__: { base(){ return 'b' } }, m(){ return super.base() } };\nconsole.log(o.m());\nclass S { static sm(){ return 'S' } } class T extends S { static sm(){ return super.sm() + 'T' } static f = super.sm() }\nconsole.log(T.sm(), T.f);",
    "1,2,3 0\n0\n7,8\nnm 3 true p\nb\nST S\n",
  )
}

/// Which binding positions name an anonymous function: parameter and
/// destructuring defaults, private and string-keyed fields, logical assignment.
pub fn named_evaluation_positions_diff_test() {
  diff(
    "function d(a = function(){}, [b = () => 1] = [], {c = class {}} = {}){ return [a.name, b.name, c.name].join('|') }\nconsole.log(d());\nvar {x = function(){}} = {}; var [y = () => 0] = []; console.log(x.name, y.name);\nvar z; ({z = function(){}} = {}); console.log(z.name);\nclass K { #p = function(){}; 42 = function(){}; 'str' = () => 1; ['comp'] = function(){}; static #sp = class {}; names(){ return [this.#p.name, this[42].name, this.str.name, this.comp.name, K.#sp.name].join('|') } }\nconsole.log(new K().names());\nvar o = { 5: function(){}, 'a b': () => 1, get g(){ return 1 }, [Symbol.iterator]: function(){}, [Symbol('d')]() {} };\nconsole.log(o[5].name, o['a b'].name, Object.getOwnPropertyDescriptor(o, 'g').get.name, o[Symbol.iterator].name);\nvar w = (1, function(){}); var v = (function(){}); console.log('seq:', w.name, 'paren:', v.name);\nvar l1 = function(){} || 1; console.log('log:', typeof l1, l1.name);\nx ||= function(){}; var u; u ??= () => 1; console.log(u.name);",
    "a|b|c\nx y\nz\n#p||str||#sp\n a b get g \nseq:  paren: v\nlog: function \nu\n",
  )
}

/// `arguments` is iterable with a `callee`; rest parameters, destructuring and
/// defaults in functions; `??` and `?.`.
pub fn parameters_and_arguments_diff_test() {
  diff(
    "function f(){ return [...arguments].join() } console.log(f(1,2));\nfunction g(){ 'use strict'; return [...arguments].join() + typeof arguments[Symbol.iterator] } console.log(g(3,4));\nfunction h(a,b){ return arguments.callee === h } console.log(h(1)); try { (function(){ 'use strict'; return arguments.callee })() } catch (e) { console.log(e.constructor.name) }\nfunction r(...a){ return a.length } console.log(r(1,2));\nvar r2 = function(x, ...rest){ return x + ':' + rest.join() }; console.log(r2(1,2,3));\nfunction d(){ var [a,b] = [1,2]; let {c} = {c:3}; return a+b+c } console.log(d());\nfunction d2([a,b],{c}){ return a+b+c } console.log(d2([1,2],{c:3}));\nfunction d3(x=5){ return x } console.log(d3(), d3(1));\nfunction d4(x = 1, {y} = {y:2}){ function inner(){ return x + y } return inner() } console.log(d4());\nvar o=null; console.log(o?.x, o ?? 1, 0 ?? 2, ({a:1})?.a, JSON.stringify(({a:1}) ?? 3));\nvar [q=7] = []; var [w=7] = [1]; console.log(q, w);\nvar s, t; [s=8] = []; [t=8] = [2]; console.log(s, t);",
    "1,2\n3,4function\ntrue\nTypeError\n2\n1:2,3\n6\n6\n5 1\n3\nundefined 1 0 1 {\"a\":1}\n7 1\n8 2\n",
  )
}

/// Generators and async functions bind parameters, `this`, `arguments`, hoisted
/// declarations and closed-over locals at call time.
pub fn coroutine_prologue_diff_test() {
  diff(
    "function* g(a, b){ yield a + b; var t = this; yield typeof t } var it = g.call({}, 1, 2); console.log(it.next().value, it.next().value);\nasync function af(x){ await null; return x * 2 } af(4).then(v => console.log('af', v));\nasync function f0(x){ return x } f0(2).then(v => console.log('f0', v));\nasync function h(x){ var y = x + 1; return this.k + y } h.call({k:10}, 2).then(v => console.log('h', v));\nasync function f(x = 1, {y} = {y:2}, ...r) { var z = 3; function inner(){ return z } await null; return x + y + r.length + inner() + arguments.length }\nf(undefined, undefined, 9, 9).then(v => console.log('f', v));\nfunction* g2(){ var v = 1; var arrow = () => v; v = 5; yield arrow(); let w = yield 2; yield w }\nvar it2 = g2(); console.log(it2.next().value, it2.next().value, it2.next(7).value);\nvar o = { async *ag(a){ yield a; yield this.k }, k: 'k' };\nvar ai = o.ag(1); ai.next().then(r => { console.log(r.value); return ai.next() }).then(r => console.log(r.value));\nvar named = function* self(n){ yield typeof self; if (n) yield* self(0) }; console.log([...named(1)].join());\nclass C { static async s(){ return this.name } *gm(p){ yield p; yield this instanceof C } }\nC.s().then(v => console.log(v)); console.log([...new C().gm('p')].join());",
    "3 object\n5 2 7\nfunction,function\np,true\nf0 2\nh 13\nC\naf 8\nf 12\n1\nk\n",
  )
}

/// Class-valued default parameters and heritage expressions, static privates on
/// a subclass receiver, setter-only and static accessors, defaults reading
/// `this`, `super` inside async and generator methods, field initialization
/// order.
pub fn class_heritage_and_members_edge_cases_diff_test() {
  diff(
    "function mk(Base = class { who(){ return 'base' } }){ return class extends Base { who(){ return 'd>' + super.who() } } }\nconsole.log(new (mk())().who(), new (mk(class { who(){ return 'x' } }))().who());\nclass B extends (class { static s = 1; v(){ return 2 } }) { }\nconsole.log(B.s, new B().v(), Object.getOwnPropertyNames(B).join(), Object.getOwnPropertyNames(class { static z(){} static y = 1 }).join());\nclass P { static #count = 0; static inc(){ return ++this.#count } }\nclass Q extends P {}\nconsole.log(P.inc(), P.inc()); try { Q.inc() } catch (e) { console.log(e.constructor.name) }\nclass S { set only(v){ this._o = v } static get sg(){ return 'sg' } m(a = this.k, b = a + 1){ return a + b } k = 5 }\nvar s = new S(); s.only = 3; console.log(s._o, s.only, S.sg, s.m(), s.m(1));\nclass AM { m(){ return 'am' } *g(){ yield 'ag' } }\nclass AD extends AM { async m(){ await null; return 'd:' + super.m() } *g(){ yield* super.g(); yield 'dg' } }\nnew AD().m().then(v => console.log(v)); console.log([...new AD().g()].join());\nclass F { a = this.init(); init(){ return 'i' } b = this.a + '2' } console.log(new F().b);\nclass G { x = 1 } class H extends G { x = this.x + 1; y = super.x } var h = new H(); console.log(h.x, h.y, Object.keys(h).join());\nvar arrowCls = () => class { static n = this === undefined }; console.log(arrowCls().n);\nclass I { static make(){ return () => new this() } } class J extends I {} console.log(I.make.call(J)() instanceof J);\nconsole.log(typeof class {}.prototype.constructor, (class { constructor(){ return } }).length);",
    "d>base d>x\n1 2 length,name,prototype length,name,prototype,z,y\n1 2\nTypeError\n3 undefined sg 11 3\nag,dg\ni2\n2 undefined x,y\nfalse\ntrue\nfunction 0\nd:am\n",
  )
}

/// `super` accessors and static blocks, `Reflect.construct` with a foreign
/// new.target, a Proxy heritage, well-known-symbol members, Error and Promise
/// subclasses with species, arrows reading `this` before `super()`.
pub fn class_super_and_protocols_diff_test() {
  diff(
    "var log = [];\nclass A { constructor(){ log.push('A:' + new.target.name) } set sx(v){ log.push('set:' + v) } get gx(){ return 'gx:' + this.tag } }\nclass B extends A { tag = 'b'; constructor(){ var early = () => this; try { early() } catch (e) { log.push(e.constructor.name) } super(); super.sx = 5; log.push(super.gx) } static { log.push('static:' + (super.constructor === Function.prototype.constructor)) } }\nnew B(); console.log(log.join());\nvar R = Reflect.construct(B, [], function NT(){}); console.log(Object.getPrototypeOf(R) === Object.prototype || Object.getPrototypeOf(R).constructor.name);\nvar px = new Proxy(A, {}); class C extends px { } console.log(new C() instanceof A, log.pop());\nclass D { static get [Symbol.toStringTag](){ return 'DD' } get [Symbol.toStringTag](){ return 'dd' } static [Symbol.iterator] = function*(){ yield 1 } }\nconsole.log(String(new D()), Object.prototype.toString.call(D), [...D].join());\nclass E extends Error { constructor(m){ super(m); this.extra = 1 } get name(){ return 'EE' } }\nvar e = new E('msg'); console.log(String(e), e.extra, e instanceof E, Object.prototype.hasOwnProperty.call(e, 'message'));\nclass Pr extends Promise { static get [Symbol.species](){ return Promise } }\nvar pr = Pr.resolve(1); console.log(pr instanceof Pr, pr.then(() => {}) instanceof Pr, Pr.all([pr]) instanceof Pr);\nclass St { static a = 1; static b = St.a + this.a; static c = () => this.a } console.log(St.b, St.c());\ntry { class X extends A { constructor(){ super(); super() } } new X() } catch (e2) { console.log('dbl', e2.constructor.name, log.length) }\nclass Sym { [Symbol.hasInstance](v){ return 'inst' } static [Symbol.hasInstance](v){ return v === 7 } } console.log(7 instanceof Sym, new Sym() instanceof Sym);",
    "static:true,ReferenceError,A:B,set:5,gx:b\nNT\ntrue A:C\n[object dd] [object DD] 1\nEE: msg 1 true true\ntrue false true\n2 1\ndbl ReferenceError 11\ntrue false\n",
  )
}

/// Generator and async-generator objects inherit from their function's own
/// `prototype` (declarations, expressions, methods), falling back to the realm
/// prototype when that is not an object.
pub fn generator_object_prototypes_diff_test() {
  diff(
    "function* gf(){} var ge = function*(){}; async function* ag(){}\nconsole.log(Object.getPrototypeOf(gf()) === gf.prototype, Object.getPrototypeOf(ge()) === ge.prototype, gf.prototype !== ge.prototype, Object.getPrototypeOf(ag()) === ag.prototype);\nclass A { *g(){} static async *sg(){} } var a = new A();\nconsole.log(Object.getPrototypeOf(a.g()) === A.prototype.g.prototype, Object.getPrototypeOf(A.sg()) === A.sg.prototype);\ngf.prototype = 5; console.log(Object.getPrototypeOf(gf()) === Object.getPrototypeOf(ge.prototype));\nvar o = { *m(){} }; console.log(Object.getPrototypeOf(o.m()) === o.m.prototype, Object.getPrototypeOf(gf).prototype === Object.getPrototypeOf(ge.prototype));",
    "true true true true\ntrue true\ntrue\ntrue true\n",
  )
}
