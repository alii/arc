import arc/engine.{Returned, Threw}
import arc/rt/types.{type JsValKind, KBool, KStr}
import rt_helpers

fn eval(source: String) -> JsValKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  rt_helpers.classify(value)
}

fn eval_throw(source: String) -> JsValKind {
  let assert Ok(#(Threw(error:), _)) = engine.eval(engine.new(), source)
  rt_helpers.classify(error)
}

fn eval_error_name(source: String) -> JsValKind {
  eval("try { " <> source <> "; 'no throw'; } catch (e) { e.constructor.name }")
}

pub fn reviver_transforms_values_test() {
  assert eval(
      "JSON.stringify(JSON.parse('{\"a\":1,\"b\":{\"c\":2}}',
         function (k, v) { return typeof v === 'number' ? v * 2 : v; }))",
    )
    == KStr("{\"a\":2,\"b\":{\"c\":4}}")
}

pub fn reviver_undefined_deletes_key_test() {
  assert eval(
      "JSON.stringify(JSON.parse('{\"a\":1,\"b\":2}',
         function (k, v) { return k === 'a' ? undefined : v; }))",
    )
    == KStr("{\"b\":2}")
}

pub fn reviver_undefined_deletes_array_element_test() {
  assert eval(
      "var a = JSON.parse('[1,2,3]',
         function (k, v) { return v === 2 ? undefined : v; });
       a.length + '/' + (1 in a) + '/' + JSON.stringify(a)",
    )
    == KStr("3/false/[1,null,3]")
}

pub fn reviver_receives_holder_as_this_test() {
  assert eval(
      "var seen = [];
       JSON.parse('{\"a\":1}', function (k, v) {
         seen.push(k + ':' + JSON.stringify(this));
         return v;
       });
       seen.join('|')",
    )
    == KStr("a:{\"a\":1}|:{\"\":{\"a\":1}}")
}

pub fn reviver_root_key_is_empty_string_test() {
  assert eval(
      "JSON.parse('7', function (k, v) { return k === '' ? 'root' : 'nested'; })",
    )
    == KStr("root")
}

pub fn reviver_throw_propagates_test() {
  assert eval_throw("JSON.parse('{\"a\":1}', function () { throw 'boom'; })")
    == KStr("boom")
}

pub fn non_callable_reviver_is_ignored_test() {
  assert eval("JSON.stringify(JSON.parse('{\"a\":1}', 42))")
    == KStr("{\"a\":1}")
  assert eval("JSON.stringify(JSON.parse('{\"a\":1}', {}))")
    == KStr("{\"a\":1}")
}

pub fn reviver_can_replace_with_object_test() {
  assert eval(
      "JSON.stringify(JSON.parse('[1]',
         function (k, v) { return k === '0' ? { n: v } : v; }))",
    )
    == KStr("[{\"n\":1}]")
}

pub fn reviver_replace_of_non_configurable_key_does_not_throw_test() {
  assert eval(
      "JSON.stringify(JSON.parse('{\"a\":1,\"b\":2}', function (k, v) {
         if (k === 'a') Object.defineProperty(this, 'b', { configurable: false });
         if (k === 'b') return 22;
         return v;
       }))",
    )
    == KStr("{\"a\":1,\"b\":2}")
}

pub fn reviver_replace_of_non_configurable_index_does_not_throw_test() {
  assert eval(
      "JSON.stringify(JSON.parse('[1,2]', function (k, v) {
         if (k === '0') Object.defineProperty(this, '1', { configurable: false });
         if (k === '1') return 22;
         return v;
       }))",
    )
    == KStr("[1,2]")
}

pub fn reviver_context_source_at_nesting_depths_test() {
  assert eval(
      "var out = [];
       JSON.parse('{\"a\":1,\"b\":{\"c\":[true,\"x\"]}}', function (k, v, ctx) {
         out.push(k + '=' + ctx.source);
         return v;
       });
       out.join('|')",
    )
    == KStr("a=1|0=true|1=\"x\"|c=undefined|b=undefined|=undefined")
}

pub fn reviver_context_source_is_verbatim_literal_test() {
  assert eval(
      "var out = [];
       JSON.parse('[1.1e+1,-0,\"a\\\\nb\",null,false]', function (k, v, ctx) {
         if (k !== '') out.push(String(ctx.source));
         return v;
       });
       out.join('|')",
    )
    == KStr("1.1e+1|-0|\"a\\nb\"|null|false")
}

pub fn reviver_context_is_empty_for_object_and_array_literals_test() {
  assert eval(
      "var out = [];
       JSON.parse('{\"a\":[1]}', function (k, v, ctx) {
         out.push(k
           + ':' + Object.getOwnPropertyNames(ctx).length
           + ':' + Object.getOwnPropertySymbols(ctx).length
           + ':' + (Object.getPrototypeOf(ctx) === Object.prototype));
         return v;
       });
       out.join('|')",
    )
    == KStr("0:1:0:true|a:0:0:true|:0:0:true")
}

pub fn reviver_context_source_property_descriptor_test() {
  assert eval(
      "var d;
       JSON.parse('1', function (k, v, ctx) {
         d = Object.getOwnPropertyDescriptor(ctx, 'source');
         return v;
       });
       [d.value, d.writable, d.enumerable, d.configurable].join(',')",
    )
    == KStr("1,true,true,true")
}

pub fn reviver_source_absent_after_forward_append_test() {
  assert eval(
      "var log = [];
       JSON.parse('[1,[]]', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (v === 1) this[1].push('barf');
         return this[k];
       });
       log.join('|')",
    )
    == KStr("0:1|0:undefined|1:undefined|:undefined")
}

pub fn reviver_source_absent_after_forward_replacement_test() {
  assert eval(
      "var log = [];
       JSON.parse('{\"p\":1,\"q\":2}', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (k === 'p') this.q = 42;
         return this[k];
       });
       log.join('|')",
    )
    == KStr("p:1|q:undefined|:undefined")
}

pub fn reviver_source_absent_after_forward_array_replacement_test() {
  assert eval(
      "var log = [];
       JSON.parse('{\"a\":1,\"b\":[2]}', function (k, v, ctx) {
         if (k === 'a') this.b = [2];
         log.push(k + ':' + String(ctx.source));
         return v;
       });
       log.join('|')",
    )
    == KStr("a:1|0:undefined|b:undefined|:undefined")
}

pub fn reviver_source_absent_for_replaced_array_elements_test() {
  assert eval(
      "var log = [];
       JSON.parse('[1,[5]]', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (v === 1) this[1] = [5];
         return this[k];
       });
       log.join('|')",
    )
    == KStr("0:1|0:undefined|1:undefined|:undefined")
}

pub fn reviver_source_absent_for_replaced_object_members_test() {
  assert eval(
      "var log = [];
       JSON.parse('{\"p\":1,\"q\":{\"x\":2}}', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (v === 1) this.q = { x: 2 };
         return this[k];
       });
       log.join('|')",
    )
    == KStr("p:1|x:undefined|q:undefined|:undefined")
}

pub fn reviver_source_absent_for_added_object_key_test() {
  assert eval(
      "var log = [];
       JSON.parse('{\"p\":1,\"q\":{}}', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (v === 1) this.q.added = 'barf';
         return this[k];
       });
       log.join('|')",
    )
    == KStr("p:1|added:undefined|q:undefined|:undefined")
}

pub fn raw_json_returns_frozen_null_prototype_object_test() {
  assert eval(
      "var r = JSON.rawJSON('1');
       [String(Object.getPrototypeOf(r)),
        String(Object.isFrozen(r)),
        r.rawJSON,
        Object.getOwnPropertyNames(r).join(','),
        String(Object.getOwnPropertySymbols(r).length)].join('|')",
    )
    == KStr("null|true|1|rawJSON|0")
}

pub fn raw_json_stringify_round_trip_test() {
  assert eval("JSON.stringify(JSON.rawJSON(1.1))") == KStr("1.1")
  assert eval("JSON.stringify(JSON.rawJSON(null))") == KStr("null")
  assert eval("JSON.stringify(JSON.rawJSON('\"foo\"'))") == KStr("\"foo\"")
  assert eval("JSON.stringify({ x: JSON.rawJSON(1), y: JSON.rawJSON(2) })")
    == KStr("{\"x\":1,\"y\":2}")
  assert eval("JSON.stringify([JSON.rawJSON('null'), JSON.rawJSON(true)])")
    == KStr("[null,true]")
  assert eval("JSON.stringify([{ x: JSON.rawJSON(1) }])") == KStr("[{\"x\":1}]")
}

pub fn raw_json_stringify_preserves_precision_test() {
  assert eval("JSON.stringify({ big: JSON.rawJSON('12345678901234567890') })")
    == KStr("{\"big\":12345678901234567890}")
}

pub fn raw_json_stringify_honours_gap_test() {
  assert eval("JSON.stringify({ x: JSON.rawJSON(1) }, null, 2)")
    == KStr("{\n  \"x\": 1\n}")
}

pub fn is_raw_json_true_only_for_boxes_test() {
  assert eval("JSON.isRawJSON(JSON.rawJSON(1))") == KBool(True)
  assert eval("JSON.isRawJSON(JSON.rawJSON('\"s\"'))") == KBool(True)
  assert eval("JSON.isRawJSON(1)") == KBool(False)
  assert eval("JSON.isRawJSON({ rawJSON: '123' })") == KBool(False)
  assert eval(
      "[JSON.isRawJSON(1.1), JSON.isRawJSON(null), JSON.isRawJSON(false),
        JSON.isRawJSON('123'), JSON.isRawJSON(undefined), JSON.isRawJSON([]),
        JSON.isRawJSON({}), JSON.isRawJSON(Symbol('123'))].join(',')",
    )
    == KStr("false,false,false,false,false,false,false,false")
}

pub fn raw_json_rejects_illegal_text_test() {
  assert eval_error_name("JSON.rawJSON('')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON(' 1')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON('1 ')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON('\\t1')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON('1\\n')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON('1\\r')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON('{}')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON('[]')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON('garbage')") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON(undefined)") == KStr("SyntaxError")
  assert eval_error_name("JSON.rawJSON(Symbol('123'))") == KStr("TypeError")
}
