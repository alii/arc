// user formals spelled like the old $param_N shim names

import arc/engine.{type JsValueKind, JsString, Returned}

fn assert_eval(source: String) -> JsValueKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  engine.classify(value)
}

pub fn user_param_named_like_shim_test() {
  assert assert_eval(
      "function f($param_1, $param_0 = 1) {
         return '' + $param_0 + ',' + $param_1;
       }
       f(7)",
    )
    == JsString("1,7")
}

pub fn user_param_named_param_0_test() {
  assert assert_eval(
      "function f($param_0 = 'd', [x] = ['y']) {
         return $param_0 + ':' + x;
       }
       f() + '|' + f('a', ['b'])",
    )
    == JsString("d:y|a:b")
}
