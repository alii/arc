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
