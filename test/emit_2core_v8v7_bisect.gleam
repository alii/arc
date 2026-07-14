//// Bisect which JS construct in the V8-v7 benches breaks the compiled path.
//// Each snippet prints "ok" on success; a `RUN FAIL` names the pattern.
////
////     gleam run -m emit_2core_v8v7_bisect

import emit_2core_harness as harness
import gleam/io
import gleam/string
import twocore/pipeline

fn t(name: String, source: String) {
  let src = source <> ";console.log(\"ok\")"
  case harness.run_compiled(src) {
    pipeline.DiffRun(result: Ok(_), stdout:) ->
      case stdout {
        <<"ok\n":utf8>> -> io.println("  pass  " <> name)
        _ ->
          io.println(
            "  BAD   " <> name <> " → stdout=" <> string.inspect(stdout),
          )
      }
    pipeline.DiffRun(result: Error(e), stdout:) ->
      io.println(
        "  FAIL  " <> name <> " → " <> string.slice(e, 0, 200) <> " | stdout="
        <> string.inspect(stdout),
      )
  }
}

pub fn main() {
  io.println("emit_2core V8-v7 bisect — construct isolation")
  t("proto_method", "function F(){this.x=5};F.prototype.m=function(){return this.x};var o=new F();if(o.m()!==5)throw 1")
  t("proto_chain", "function A(){};A.prototype.v=7;function B(){};B.prototype=new A();var o=new B();if(o.v!==7)throw 1")
  t("new_array_n", "var a=new Array(4);if(a.length!==4)throw 1")
  t("new_array_0", "var a=new Array();a.push(1);a.push(2);if(a.length!==2)throw 1")
  t("throw_error", "try{throw new Error('x')}catch(e){if(e.message!=='x')throw 1}")
  t("null_eq", "var x=null;if(x==null){}else throw 1")
  t("bitwise", "var x=0xff;if((x&7)!==7)throw 1;if((1<<4)!==16)throw 1")
  t("div_mod", "if(10/4!==2.5)throw 1;if(10%3!==1)throw 2")
  t("this_in_fn", "function F(){this.a=1;this.b=2};var o=new F();if(o.a+o.b!==3)throw 1")
  t("arr_index_set", "var a=new Array(3);a[0]=10;a[1]=20;if(a[0]+a[1]!==30)throw 1")
  t("hoisted_var", "if(true){var x=5};if(x!==5)throw 1")
  t("ctor_call_method", "function F(){this.x=0;this.set(5)};F.prototype.set=function(v){this.x=v};var o=new F();if(o.x!==5)throw 1")
  t("proto_method_call_proto", "function F(){this.x=1};F.prototype.a=function(){return this.b()};F.prototype.b=function(){return this.x};var o=new F();if(o.a()!==1)throw 1")
  t("nested_new", "function A(x){this.x=x};function B(){this.a=new A(3)};var o=new B();if(o.a.x!==3)throw 1")
  t("global_typeof", "if(typeof undeclared==='undefined'){}else throw 1")
  t("fn_proto_assign", "Function.prototype.foo=function(){return 9};function F(){};if(F.foo()!==9)throw 1")
  t("string_char_code", "var s='A';if(s.charCodeAt(0)!==65)throw 1")
  t("error_in_correctness", "var q=1;var h=2;if(q!=1||h!=2){throw new Error('x')}")

  t("read_fn_proto", "function F(){};if(typeof F.prototype!=='object')throw 1")
  t("set_fn_proto_prop", "function F(){};F.prototype.v=7")
  t("set_fn_proto_prop_decl", "var F=function(){};F.prototype.v=7")
  t("read_fn_prop", "function F(){};if(F.length!==0)throw 1")
  t("read_fn_name", "function F(){};if(F.name!=='F')throw 1")
  // Distinguish "GET doesn't walk proto" vs "new doesn't link proto":
  t("obj_literal_proto", "var p={m:5};var o=Object.create(p);if(o.m!==5)throw 1")
  t("get_proto_of_new", "function F(){};var o=new F();if(Object.getPrototypeOf(o)!==F.prototype)throw new Error('proto not linked')")
  t("read_proto_prop_val", "function F(){};F.prototype.v=7;var o=new F();if(o.v!==7)throw 1")
  t("read_fn_proto_direct", "function F(){};F.prototype.v=7;if(F.prototype.v!==7)throw 1")

  // Negative: MakeConstructor must NOT run for arrows/methods (§10.2.5 only
  // applies to FunctionDeclaration/FunctionExpression evaluation).
  t("arrow_no_proto", "var f=()=>{};if(f.prototype!==undefined)throw 1")
  t("method_no_proto", "var o={m(){}};if(o.m.prototype!==undefined)throw 1")

  // §14.7.5.9 for-in must walk the prototype chain (raytrace Object.extend
  // copies from a replaced-prototype instance; own-keys-only would copy zero).
  t("for_in_proto", "var p={a:1};var o=Object.create(p);var k='';for(var x in o){k+=x};if(k!=='a')throw 1")

  // §10.4.4.6 arguments.length (raytrace reads it).
  t("arguments_len", "function f(){return arguments.length};if(f(1,2,3)!==3)throw 1")

  // raytrace Class.create idiom — `.apply(this, arguments)` fast-path forwards
  // the raw _args cons-list directly (elides arguments-object + apply reflect).
  t("class_create_apply", "function F(){this.init.apply(this,arguments)};F.prototype.init=function(a,b){this.s=a+b};var o=new F(3,4);if(o.s!==7)throw 1")

  // deltablue: `Function.prototype.inheritsFrom` — nested fn decl inside a
  // proto method, `this.prototype = new Inner()` (deltablue_run.js:6-11).
  t("deltablue_inherits", "Function.prototype.inh=function(sup){function I(){};I.prototype=sup.prototype;this.prototype=new I();this.sup=sup};function A(){};A.prototype.v=7;function B(){};B.inh(A);var o=new B();if(o.v!==7)throw 1;if(B.sup!==A)throw 2")

  // raytrace: `Object.extend` — for-in copy of own props (raytrace.js:43-47).
  t("raytrace_extend", "Object.extend=function(d,s){for(var p in s)d[p]=s[p];return d};var d={};Object.extend(d,{a:1,b:2});if(d.a!==1||d.b!==2)throw 1")

  // crypto isolation — bignum arithmetic core ops (crypto.js am3 kernel).
  t("crypto_urshift", "if((0x100000000>>>30)!==0)throw 1;if((-1>>>0)!==4294967295)throw 2;if((0xdeadbeef>>>16)!==0xdead)throw 3")
  t("crypto_rshift", "if((0x100>>4)!==16)throw 1;if((-1>>1)!==-1)throw 2")
  t("crypto_compound_or", "var x=1;x|=6;if(x!==7)throw 1")
  t("crypto_char_at", "var s='abc';if(s.charAt(1)!=='b')throw 1")
  t("crypto_mathpow", "if(Math.pow(2,10)!==1024)throw 1")
  t("crypto_mathfloor", "if(Math.floor(3.7)!==3)throw 1")
  t("crypto_str_concat_num", "var r='';r+=5;if(r!=='5')throw new Error(r)")
  t("crypto_prefix_inc", "var i=0;var a=[];a[i++]=10;a[i++]=20;if(a[0]!==10||a[1]!==20)throw 1")
  t("crypto_arr_append", "var a=[];for(var i=0;i<10;i++)a[i]=i*2;if(a.length!==10||a[9]!==18)throw 0")
  t(
    "arr_pdict_writeback",
    "var a=[1,2,3];a[0];a[1]=9;var k='len'+'gth';if(a[k]!==3)throw 1;if(a[1]!==9)throw 2;a[2]=7;if(a[2]!==7)throw 3",
  )
  t(
    "arr_pdict_no_extend",
    "var a=[1];Object.preventExtensions(a);a[0];a[1]=9;if(a.length!==1)throw 1;if(a[1]!==undefined)throw 2",
  )
  t("crypto_am3_step", "var xl=5&0x1fff,xh=5>>13;var l=7;var h=l>>13;var m=xh*l+h*xl;l=xl*l+((m&0x1fff)<<13)+3+0;var c=(l>>26)+(m>>13)+xh*h;if(l!==38)throw new Error('l='+l);if(c!==0)throw new Error('c='+c)")
  t("crypto_large_and", "var canary=0xdeadbeefcafe;if((canary&0xffffff)!==0xefcafe)throw 1")
  t("crypto_substring", "var s='hello';if(s.substring(1,3)!=='el')throw 1")
  t("crypto_while_dec", "var n=5;var r='';while(--n>=0)r+='x';if(r!=='xxxxx')throw new Error(r)")
  t("crypto_null_eq_undef", "var c;if(c==null){}else throw 1")
  t("crypto_idx_compound", "var a=[1,2,3];a[1]|=8;if(a[1]!==10)throw new Error(a[1])")
  t("crypto_str_len_lit", "if('ff'.length!==2)throw 1")
  t("crypto_str_len_var", "var s='ff';if(s.length!==2)throw 1")
  t("crypto_this_array", "function BI(){this.arr=new Array()};BI.prototype.put=function(i,v){this.arr[i]=v};var b=new BI();b.put(0,5);if(b.arr[0]!==5)throw 1")
  t("crypto_shift_assign", "var x=1;x<<=4;if(x!==16)throw 1;x>>=2;if(x!==4)throw 2")
  t("crypto_obj_array_idx", "var a=new Array();a[48]=0;a[49]=1;if(a[48]!==0||a[49]!==1)throw 1")
  t("crypto_int_key_read", "var a=new Array();a['0'.charCodeAt(0)]=5;if(a[48]!==5)throw 1")
  t("crypto_nbi_from_int", "function BI(){this.arr=new Array()};function nbi(){return new BI()};BI.prototype.fromInt=function(x){this.t=1;this.s=x<0?-1:0;if(x>0)this.arr[0]=x;else if(x<-1)this.arr[0]=x+0x10000000;else this.t=0};var r=nbi();r.fromInt(5);if(r.arr[0]!==5)throw 1;if(r.t!==1)throw 2")
  t("crypto_for_no_body_semi", "var s=0;for(var i=0;i<3;i++)s+=i;if(s!==3)throw 1")
  t("crypto_mathmax", "if(Math.max(1,2)!==2)throw 1;if(Math.min(1,2)!==1)throw 2")
  t("crypto_unary_neg", "var x=5;if(-x!==-5)throw 1")
  t("crypto_proto_this_arr", "function F(){this.a=new Array();this.t=0};F.prototype.set=function(i,v){var arr=this.a;arr[this.t++]=v};var f=new F();f.set(0,7);if(f.a[0]!==7)throw new Error(f.a[0]);if(f.t!==1)throw 2")

  // richards prefix — everything up to the first Scheduler method call
  t("richards_prefix", "var COUNT=1000;var ID_IDLE=0;var NUMBER_OF_IDS=6;function Scheduler(){this.queueCount=0;this.holdCount=0;this.blocks=new Array(NUMBER_OF_IDS);this.list=null;this.currentTcb=null;this.currentId=null};var s=new Scheduler();if(s.queueCount!==0)throw 1")
  t("richards_addtask", "function Sch(){this.list=null};Sch.prototype.addTask=function(id,pri,q,task){this.list={id:id,pri:pri,q:q,task:task,link:this.list}};var s=new Sch();s.addTask(1,2,3,4);if(s.list.id!==1)throw 1")
  // perf6 slot -2 (`_this_sid`) loop-carry: a `_t` body with a while whose
  // cond/body touches `this.*` (rebinds slot -2 via refresh_this_c/share)
  // then reads `this.*` AFTER the loop. Was: unbound `V_5ft<N>` in jsf_*_t.
  t("t_while_this_after", "function F(){this.n=0;this.v=7};F.prototype.a=function(){while(this.n>0){var q=1};return this.v};var o=new F();if(o.a()!==7)throw 1")

  // §14.12 switch dispatch: strict_eq→truthy→i32 for If.cond (was: bare
  // atom cond → every case matched). deltablue nextWeaker shape.
  t("switch_return_plain", "function f(n){switch(n){case 0:return 10;case 1:return 20}};if(f(0)!==10)throw 1;if(f(1)!==20)throw 2")
  t("switch_return_member", "var G={a:1,b:2,c:3};function f(n){switch(n){case 0:return G.a;case 1:return G.b;case 2:return G.c}};if(f(0)!==1)throw 1;if(f(1)!==2)throw 2;if(f(2)!==3)throw 3")
  t("switch_default", "function f(n){switch(n){case 0:return 10;default:return 99}};if(f(5)!==99)throw 1;if(f(0)!==10)throw 2")
  t("switch_break", "function f(n){var r;switch(n){case 0:r=10;break;case 1:r=20;break};return r};if(f(1)!==20)throw 1")
  t("switch_fallthrough", "function f(n){var r=0;switch(n){case 0:r+=1;case 1:r+=10};return r};if(f(0)!==11)throw 1;if(f(1)!==10)throw 2")
  // perf7 cold-outline: aux_fn added mid-block; a following bare `break`
  // diverges — rk_checkpoint threads fns_acc/next_fn back to run_rk (was:
  // UnknownFunction jsf_cold_ps_*).
  t("cold_for_break", "var G={a:0};for(;;){G.a=1;break};if(G.a!==1)throw 1")
  t("cold_nested_for", "var G={a:0};for(;;){for(;;){G.a=1;break};break};if(G.a!==1)throw 1")
  t("cold_while_break", "var G={a:0};while(1){G.a=1;break};if(G.a!==1)throw 1")
  t("cold_for_continue", "var G={a:0};for(var i=0;i<1;i++){G.a=1;continue};if(G.a!==1)throw 1")
  // perf8 ic_proto_get: warm-cache correctness. Loop warms the {PId,V}
  // SiteKey; second read must be identical (raytrace `.prototype` on
  // KFunction — peek_get's ordinary gate rejects k_function).
  t(
    "pg_proto_warm_kfn",
    "function F(){};F.prototype.v=7;var s=0;for(var i=0;i<3;i++)s+=F.prototype.v;if(s!==21)throw s",
  )
  // Own-shadow AFTER cache install: t_ic_get precedes t_ic_proto_get, so
  // once own is added it must win (§9.1.8.1 own-before-proto).
  t(
    "pg_proto_own_shadow",
    "function F(){};F.prototype.v=7;var o=new F();var a=o.v;o.v=99;var b=o.v;if(a!==7||b!==99)throw a*100+b",
  )
  // Proto MUTATION after cache install: tc_mc_evict on the proto write must
  // sweep the @pgp SiteKey so the next read sees the new value.
  t(
    "pg_proto_mutate",
    "function F(){};F.prototype.v=7;var o=new F();var a=o.v;F.prototype.v=99;var b=o.v;if(a!==7||b!==99)throw a*100+b",
  )

  // perf8_arr_c_hoist: hoisted `{tc_arr,Id}` overlay must not go stale on
  // writeback / length-growth. `a` sees ≥2 bracket reads AND a bracket write
  // → hoist skipped (write veto); reads see the write. Length ≥8 so overlay
  // installs.
  t(
    "arr_c_writeback",
    "var a=new Array(10);for(var i=0;i<10;i++)a[i]=i;var s=0;for(var i=0;i<9;i++){s+=a[i];a[i+1]=a[i]+a[i+1]};if(s!==120)throw s;if(a[9]!==45)throw a[9]",
  )
  // Length-growth mid-loop: `a[a.length]=v` grows from 8→12. Write veto keeps
  // `a` un-hoisted; `b` (read-only, ≥2 reads) IS hoisted.
  t(
    "arr_c_grow",
    "var a=new Array(8);var b=new Array(8);for(var i=0;i<8;i++){a[i]=i;b[i]=i*10};for(var i=0;i<4;i++){a[a.length]=b[i]+b[i+1]};if(a.length!==12)throw a.length;if(a[11]!==70)throw a[11]",
  )
  // Read-only base hoist: crypto am3 shape — `t` has ≥2 reads, 0 writes →
  // arr_c_load fires; sum must match un-hoisted result.
  t(
    "arr_c_readonly",
    "var t=new Array(10);for(var i=0;i<10;i++)t[i]=i+1;var s=0;var i=0;while(i<9){s+=t[i]+t[i+1];i++};if(s!==99)throw s",
  )
  // Not-yet-installed at hoist time: arr_c_load()=undefined → `_c` tail-calls
  // `_p` for cold install on first iter; subsequent iters `_p`-warm.
  t(
    "arr_c_cold_undef",
    "function f(){var t=new Array(10);for(var i=0;i<10;i++)t[i]=i;var s=0;for(var i=0;i<10;i++){s+=t[i];s+=t[i]};return s};if(f()!==90)throw f()",
  )
}
