/// JSON.parse reviver — §25.5.1.1 InternalizeJSONProperty — plus the ES2025
/// json-parse-with-source proposal (reviver `context`, JSON.rawJSON,
/// JSON.isRawJSON, verbatim rawJSON serialization).
import arc/engine.{Returned, Threw}
import arc/vm/value.{type JsValue, JsBool, JsString}

/// Helper: eval source on a fresh engine, assert normal completion, return
/// the completion value.
fn eval(source: String) -> JsValue {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  value
}

/// Helper: eval source, assert it threw, return the thrown value.
fn eval_throw(source: String) -> JsValue {
  let assert Ok(#(Threw(error:), _)) = engine.eval(engine.new(), source)
  error
}

/// Helper: eval an expression that is expected to throw, return the thrown
/// error's constructor name (or "no throw" if it completed normally).
fn eval_error_name(source: String) -> JsValue {
  eval("try { " <> source <> "; 'no throw'; } catch (e) { e.constructor.name }")
}

pub fn reviver_transforms_values_test() {
  // The reviver visits every value bottom-up; numbers get doubled.
  assert eval(
      "JSON.stringify(JSON.parse('{\"a\":1,\"b\":{\"c\":2}}',
         function (k, v) { return typeof v === 'number' ? v * 2 : v; }))",
    )
    == JsString("{\"a\":2,\"b\":{\"c\":4}}")
}

pub fn reviver_undefined_deletes_key_test() {
  assert eval(
      "JSON.stringify(JSON.parse('{\"a\":1,\"b\":2}',
         function (k, v) { return k === 'a' ? undefined : v; }))",
    )
    == JsString("{\"b\":2}")
}

pub fn reviver_undefined_deletes_array_element_test() {
  // A deleted array index becomes a hole — length is unchanged and the hole
  // stringifies as null.
  assert eval(
      "var a = JSON.parse('[1,2,3]',
         function (k, v) { return v === 2 ? undefined : v; });
       a.length + '/' + (1 in a) + '/' + JSON.stringify(a)",
    )
    == JsString("3/false/[1,null,3]")
}

pub fn reviver_receives_holder_as_this_test() {
  // The root call's holder is { "": parsedValue }; nested calls get the
  // containing object/array.
  assert eval(
      "var seen = [];
       JSON.parse('{\"a\":1}', function (k, v) {
         seen.push(k + ':' + JSON.stringify(this));
         return v;
       });
       seen.join('|')",
    )
    == JsString("a:{\"a\":1}|:{\"\":{\"a\":1}}")
}

pub fn reviver_root_key_is_empty_string_test() {
  assert eval(
      "JSON.parse('7', function (k, v) { return k === '' ? 'root' : 'nested'; })",
    )
    == JsString("root")
}

pub fn reviver_throw_propagates_test() {
  assert eval_throw("JSON.parse('{\"a\":1}', function () { throw 'boom'; })")
    == JsString("boom")
}

pub fn non_callable_reviver_is_ignored_test() {
  // §25.5.1 step 7 gates on IsCallable — a non-callable second argument is
  // simply not a reviver.
  assert eval("JSON.stringify(JSON.parse('{\"a\":1}', 42))")
    == JsString("{\"a\":1}")
  assert eval("JSON.stringify(JSON.parse('{\"a\":1}', {}))")
    == JsString("{\"a\":1}")
}

pub fn reviver_can_replace_with_object_test() {
  assert eval(
      "JSON.stringify(JSON.parse('[1]',
         function (k, v) { return k === '0' ? { n: v } : v; }))",
    )
    == JsString("[{\"n\":1}]")
}

pub fn reviver_replace_of_non_configurable_key_does_not_throw_test() {
  // §25.5.1.1 step 2.c.ii.3 is a bare `Perform ? CreateDataProperty(...)` —
  // the Boolean is discarded, so a rejected define keeps the old value rather
  // than throwing (test262 reviver-object-non-configurable-prop-create.js).
  assert eval(
      "JSON.stringify(JSON.parse('{\"a\":1,\"b\":2}', function (k, v) {
         if (k === 'a') Object.defineProperty(this, 'b', { configurable: false });
         if (k === 'b') return 22;
         return v;
       }))",
    )
    == JsString("{\"a\":1,\"b\":2}")
}

pub fn reviver_replace_of_non_configurable_index_does_not_throw_test() {
  // Same for the array walk, §25.5.1.1 step 2.b.iii.4
  // (test262 reviver-array-non-configurable-prop-create.js).
  assert eval(
      "JSON.stringify(JSON.parse('[1,2]', function (k, v) {
         if (k === '0') Object.defineProperty(this, '1', { configurable: false });
         if (k === '1') return 22;
         return v;
       }))",
    )
    == JsString("[1,2]")
}

// ---------------------------------------------------------------------------
// ES2025 json-parse-with-source: reviver `context` third argument
// ---------------------------------------------------------------------------

pub fn reviver_context_source_at_nesting_depths_test() {
  // `source` is present for every primitive literal, at every depth, and
  // absent (undefined) for the object/array literals that contain them.
  assert eval(
      "var out = [];
       JSON.parse('{\"a\":1,\"b\":{\"c\":[true,\"x\"]}}', function (k, v, ctx) {
         out.push(k + '=' + ctx.source);
         return v;
       });
       out.join('|')",
    )
    == JsString("a=1|0=true|1=\"x\"|c=undefined|b=undefined|=undefined")
}

pub fn reviver_context_source_is_verbatim_literal_test() {
  // `source` is the exact source text of the literal — quotes and escape
  // sequences preserved, number formatting not normalized.
  assert eval(
      "var out = [];
       JSON.parse('[1.1e+1,-0,\"a\\\\nb\",null,false]', function (k, v, ctx) {
         if (k !== '') out.push(String(ctx.source));
         return v;
       });
       out.join('|')",
    )
    == JsString("1.1e+1|-0|\"a\\nb\"|null|false")
}

pub fn reviver_context_is_empty_for_object_and_array_literals_test() {
  // For an Array or Object the context object has no own properties at all;
  // it is always an ordinary object with %Object.prototype% and no symbols.
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
    == JsString("0:1:0:true|a:0:0:true|:0:0:true")
}

pub fn reviver_context_source_property_descriptor_test() {
  // `source` is a plain data property: writable, enumerable, configurable.
  assert eval(
      "var d;
       JSON.parse('1', function (k, v, ctx) {
         d = Object.getOwnPropertyDescriptor(ctx, 'source');
         return v;
       });
       [d.value, d.writable, d.enumerable, d.configurable].join(',')",
    )
    == JsString("1,true,true,true")
}

pub fn reviver_source_absent_after_forward_append_test() {
  // Once a reviver mutates a not-yet-visited part of the tree, the values it
  // finds there no longer correspond to a source literal — no `source`.
  assert eval(
      "var log = [];
       JSON.parse('[1,[]]', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (v === 1) this[1].push('barf');
         return this[k];
       });
       log.join('|')",
    )
    == JsString("0:1|0:undefined|1:undefined|:undefined")
}

pub fn reviver_source_absent_after_forward_replacement_test() {
  // A forward-modified primitive is no longer SameValue with what the parse
  // node produced, so its context carries no `source`.
  assert eval(
      "var log = [];
       JSON.parse('{\"p\":1,\"q\":2}', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (k === 'p') this.q = 42;
         return this[k];
       });
       log.join('|')",
    )
    == JsString("p:1|q:undefined|:undefined")
}

pub fn reviver_source_absent_after_forward_array_replacement_test() {
  // Replacing an array slot with a *structurally identical* array still drops
  // the whole parse record: the record's [[Value]] is not the new array, so
  // neither its `source` nor its element records reach the children.
  assert eval(
      "var log = [];
       JSON.parse('{\"a\":1,\"b\":[2]}', function (k, v, ctx) {
         if (k === 'a') this.b = [2];
         log.push(k + ':' + String(ctx.source));
         return v;
       });
       log.join('|')",
    )
    == JsString("a:1|0:undefined|b:undefined|:undefined")
}

pub fn reviver_source_absent_for_replaced_array_elements_test() {
  // Same for a nested array inside an array literal.
  assert eval(
      "var log = [];
       JSON.parse('[1,[5]]', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (v === 1) this[1] = [5];
         return this[k];
       });
       log.join('|')",
    )
    == JsString("0:1|0:undefined|1:undefined|:undefined")
}

pub fn reviver_source_absent_for_replaced_object_members_test() {
  // And for an object slot replaced with an identical-looking object.
  assert eval(
      "var log = [];
       JSON.parse('{\"p\":1,\"q\":{\"x\":2}}', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (v === 1) this.q = { x: 2 };
         return this[k];
       });
       log.join('|')",
    )
    == JsString("p:1|x:undefined|q:undefined|:undefined")
}

pub fn reviver_source_absent_for_added_object_key_test() {
  // A key the reviver adds has no parse node at all.
  assert eval(
      "var log = [];
       JSON.parse('{\"p\":1,\"q\":{}}', function (k, v, ctx) {
         log.push(k + ':' + String(ctx.source));
         if (v === 1) this.q.added = 'barf';
         return this[k];
       });
       log.join('|')",
    )
    == JsString("p:1|added:undefined|q:undefined|:undefined")
}

// ---------------------------------------------------------------------------
// ES2025 json-parse-with-source: JSON.rawJSON / JSON.isRawJSON
// ---------------------------------------------------------------------------

pub fn raw_json_returns_frozen_null_prototype_object_test() {
  assert eval(
      "var r = JSON.rawJSON('1');
       [String(Object.getPrototypeOf(r)),
        String(Object.isFrozen(r)),
        r.rawJSON,
        Object.getOwnPropertyNames(r).join(','),
        String(Object.getOwnPropertySymbols(r).length)].join('|')",
    )
    == JsString("null|true|1|rawJSON|0")
}

pub fn raw_json_stringify_round_trip_test() {
  // A rawJSON box serializes verbatim, wherever it appears.
  assert eval("JSON.stringify(JSON.rawJSON(1.1))") == JsString("1.1")
  assert eval("JSON.stringify(JSON.rawJSON(null))") == JsString("null")
  assert eval("JSON.stringify(JSON.rawJSON('\"foo\"'))") == JsString("\"foo\"")
  assert eval("JSON.stringify({ x: JSON.rawJSON(1), y: JSON.rawJSON(2) })")
    == JsString("{\"x\":1,\"y\":2}")
  assert eval("JSON.stringify([JSON.rawJSON('null'), JSON.rawJSON(true)])")
    == JsString("[null,true]")
  assert eval("JSON.stringify([{ x: JSON.rawJSON(1) }])")
    == JsString("[{\"x\":1}]")
}

pub fn raw_json_stringify_preserves_precision_test() {
  // The point of the proposal: bytes in, bytes out — no float round-trip.
  assert eval("JSON.stringify({ big: JSON.rawJSON('12345678901234567890') })")
    == JsString("{\"big\":12345678901234567890}")
}

pub fn raw_json_stringify_honours_gap_test() {
  assert eval("JSON.stringify({ x: JSON.rawJSON(1) }, null, 2)")
    == JsString("{\n  \"x\": 1\n}")
}

pub fn is_raw_json_true_only_for_boxes_test() {
  assert eval("JSON.isRawJSON(JSON.rawJSON(1))") == JsBool(True)
  assert eval("JSON.isRawJSON(JSON.rawJSON('\"s\"'))") == JsBool(True)
  assert eval("JSON.isRawJSON(1)") == JsBool(False)
  assert eval("JSON.isRawJSON({ rawJSON: '123' })") == JsBool(False)
  assert eval(
      "[JSON.isRawJSON(1.1), JSON.isRawJSON(null), JSON.isRawJSON(false),
        JSON.isRawJSON('123'), JSON.isRawJSON(undefined), JSON.isRawJSON([]),
        JSON.isRawJSON({}), JSON.isRawJSON(Symbol('123'))].join(',')",
    )
    == JsString("false,false,false,false,false,false,false,false")
}

pub fn raw_json_rejects_illegal_text_test() {
  // Empty string, leading/trailing JSON whitespace, non-primitive outermost
  // value, and plain garbage all throw SyntaxError.
  assert eval_error_name("JSON.rawJSON('')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON(' 1')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON('1 ')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON('\\t1')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON('1\\n')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON('1\\r')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON('{}')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON('[]')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON('garbage')") == JsString("SyntaxError")
  assert eval_error_name("JSON.rawJSON(undefined)") == JsString("SyntaxError")
  // ToString runs first, so a Symbol is a TypeError, not a SyntaxError.
  assert eval_error_name("JSON.rawJSON(Symbol('123'))") == JsString("TypeError")
}
