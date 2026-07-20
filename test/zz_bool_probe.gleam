//// Temp probe: cond-position `==`/`!=`/`in`/`instanceof` after i32_to_js_bool.
//// Run: gleam run -m zz_bool_probe

import emit_2core_harness as harness
import gleam/io
import gleam/string

fn check(label: String, src: String) -> Nil {
  let i = harness.run_interpreted(src)
  let c = harness.run_compiled(src)
  io.println(
    label
    <> " interp=" <> string.inspect(i.stdout)
    <> " compiled=" <> string.inspect(c.stdout)
    <> case i.stdout == c.stdout {
      True -> "  OK"
      False -> "  *** MISMATCH ***"
    },
  )
  Nil
}

pub fn main() -> Nil {
  check("if(a==b)", "var a=1,b=2;if(a==b){console.log('eq')}else{console.log('ne')}")
  check("if(a!=b)", "var a=1,b=2;if(a!=b){console.log('ne')}else{console.log('eq')}")
  check("while(i==0)", "var i=0,n=0;while(i==0){n++;if(n>3)i=1}console.log(n)")
  check("if(!(a==b))", "var a=1,b=1;if(!(a==b)){console.log('t')}else{console.log('f')}")
  check("if(a in o)", "var o={p:1};if('zz' in o){console.log('t')}else{console.log('f')}")
  check("if(x instanceof A)", "function A(){};function B(){};var x=new B();if(x instanceof A){console.log('t')}else{console.log('f')}")
  check("if(a<b)", "var a=3,b=2;if(a<b){console.log('t')}else{console.log('f')}")
  check("str<str cond", "var a='b',b='a';if(a<b){console.log('t')}else{console.log('f')}")
  check("ternary ==", "var a=1,b=2;console.log((a==b)?'t':'f')")
  check("&& with ==", "var a=1,b=2;console.log(((a==b)&&1)||'f')")
  check("value ==", "var a=1;console.log(''+(a==1)+typeof(a==1))")
  check("do-while !=", "var i=0;do{i++}while(i!=3);console.log(i)")
  check("for cond ==", "var s='';for(var i=0;i!=3;i++){s+=i}console.log(s)")
}
