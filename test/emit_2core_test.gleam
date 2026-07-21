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
