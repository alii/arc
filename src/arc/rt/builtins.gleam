//// `rt_builtins` — realm bootstrap + native-method dispatch (SPEC §7.M6).
////
//// Port of `arc/vm/builtins.gleam:54-635` (`init` + `globals`) over the
//// threaded `Agent` model. `init_realm` allocates every intrinsic
//// prototype/constructor into the store, seeds the concrete `JsOps` upcall
//// table (D17), pins every realm handle as a permanent GC root, allocates
//// `globalThis`, and returns the populated `Realm` record + updated state.
////
//// `dispatch_native` / `dispatch_native_construct` are the M4→M6 seam:
//// `rt_call.gleam:83-98` forward-declares them via
//// `@external(erlang, "arc_rt_builtins_ffi", ...)`; the FFI shim
//// forwards straight to this module. Return-tuple order `#(V, St')` (R1).

import arc/host_hooks.{type HostHooks}
import arc/rt/async as rt_async
import arc/rt/builtins/array as b_array
import arc/rt/builtins/array_buffer as b_array_buffer
import arc/rt/builtins/atomics as b_atomics
import arc/rt/builtins/bigint as b_bigint
import arc/rt/builtins/boolean as b_boolean
import arc/rt/builtins/common
import arc/rt/builtins/console as b_console
import arc/rt/builtins/data_view as b_data_view
import arc/rt/builtins/date as b_date
import arc/rt/builtins/disposable_stack as b_disposable_stack
import arc/rt/builtins/dom_exception as b_dom_exception
import arc/rt/builtins/error as b_error
import arc/rt/builtins/function as b_function
import arc/rt/builtins/generator as b_generator
import arc/rt/builtins/global_fns as b_global_fns
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/intl as b_intl
import arc/rt/builtins/iterator as b_iterator
import arc/rt/builtins/json as b_json
import arc/rt/builtins/map as b_map
import arc/rt/builtins/math as b_math
import arc/rt/builtins/number as b_number
import arc/rt/builtins/object as b_object
import arc/rt/builtins/promise as b_promise
import arc/rt/builtins/proxy as b_proxy
import arc/rt/builtins/realm_ops
import arc/rt/builtins/reflect as b_reflect
import arc/rt/builtins/regexp as b_regexp
import arc/rt/builtins/set as b_set
import arc/rt/builtins/string as b_string
import arc/rt/builtins/symbol as b_symbol
import arc/rt/builtins/temporal as b_temporal
import arc/rt/builtins/typed_array as b_typed_array
import arc/rt/builtins/weak as b_weak
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type NativeToken,
  type Realm, Agent, ArrayBufferN, ArrayN, AsyncGenResume, AsyncResume, AtomicsN,
  BigIntN, BooleanConstructor, BooleanN, BooleanObj, ConsoleN, DataProperty,
  DataViewN, DateN, DisposableStackN, DomExceptionN, ErrorN, FunctionN,
  GeneratorN, GlobalN, HostFn, HostFnEntry, IntlN, IteratorN, JInt, JNan,
  JPosInf, JsOps, JsStore, JsonN, KHandle, MapN, MathN, Named, NativeUnseeded,
  NoElements, NumberConstructor, NumberN, NumberObj, ObjectN, Ordinary, PromiseN,
  PromiseRejectFn, PromiseResolveFn, ProxyN, Realm, ReflectN, RegExpN,
  ReturnThis, SObject, SetN, StringConstructor, StringKey, StringN, StringObj,
  SymbolConstructor, SymbolN, TemporalN, Test262N, ThrowTypeErrorPoison,
  TypedArrayN, WeakN, classify, mk_number, mk_object, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}

// ───────────────────────────────── init_realm ───────────────────────────────

/// A fresh `Agent` with an empty store, the embedder's `hooks`, `JsOps`
/// seeded, and a fully initialised realm 0 as the current realm.
pub fn new_agent(hooks: HostHooks) -> Agent {
  let st =
    Agent(
      store: rt_store.t_store_new(),
      realm: rt_types.unset_realm(),
      template_objects: dict.new(),
      frames: [],
      hooks:,
      host_fns: dict.new(),
      realms: dict.new(),
      import_hook: None,
    )
  let #(_realm, st) = init_realm(seed_ops(st))
  st
}

/// Allocate and root every built-in intrinsic of a NEW realm into the store,
/// build its `globalThis`, register it in `st.realms` under the next free id
/// and make it the current realm (SPEC §7.M6 / §2.5, §9.3.3 CreateRealm +
/// SetRealmGlobalObject + SetDefaultGlobalBindings).
///
/// Works on any agent whose `JsOps` are seeded: `new_agent` calls it for
/// realm 0, `create_realm` (with `st.realm` reset to `unset_realm()`,
/// reproducing the bootstrap state) for every later one.
///
/// Allocation order mirrors arc `builtins.gleam:54-456` — prototype-chain
/// wiring depends on it (Object.prototype first, then Function.prototype,
/// then everything else). Deterministic: same handle ids for realm 0 every
/// run.
pub fn init_realm(st: Agent) -> #(Realm, Agent) {
  // One past the highest id in use, counting the current realm even if the
  // registry has no entry for it (`unset_realm` is -1, so realm 0 first).
  let id =
    dict.fold(st.realms, st.realm.id + 1, fn(next, known, _realm) {
      int.max(next, known + 1)
    })
  // 1. Object.prototype — the root of all prototype chains (proto: None).
  let #(object_proto, st) = common.alloc_proto(st, None, dict.new())
  // 2. Function.prototype + %Function% + %ThrowTypeError%.
  let #(#(function, throw_type_error), st) = b_function.init(st, object_proto)
  let fn_proto = function.prototype
  let fn_ctor = function.constructor
  // 3. Object constructor + Object.prototype methods (fills object_proto).
  let #(object, st) = b_object.init(st, object_proto, fn_proto)
  // 4. Array.
  let #(array, st) = b_array.init(st, object_proto, fn_proto)
  // 5. Error family (Error + 7 NativeError subclasses).
  let #(errors, st) = b_error.init(st, object_proto, fn_proto, id)
  let #(dom_exception, st) =
    b_dom_exception.init(st, fn_proto, errors.error.prototype)
  // 6. Namespace objects (Math, JSON, Reflect, console, Atomics).
  let #(math, st) = b_math.init(st, object_proto, fn_proto)
  let #(json, st) = b_json.init(st, object_proto, fn_proto, id)
  let #(reflect, st) = b_reflect.init(st, object_proto, fn_proto)
  let #(console, st) = b_console.init(st, object_proto, fn_proto)
  let #(atomics, st) = b_atomics.init(st, object_proto, fn_proto)
  // 7. Primitive wrapper types.
  let #(string, st) = b_string.init(st, object_proto, fn_proto)
  let #(nb, st) = b_number.init(st, object_proto, fn_proto)
  let number = nb.pair
  let #(boolean, st) = b_boolean.init(st, object_proto, fn_proto)
  let #(symbol, st) = b_symbol.init(st, object_proto, fn_proto)
  let #(bigint, st) = b_bigint.init(st, object_proto, fn_proto)
  // 8. RegExp, Date.
  let #(regexp, st) = b_regexp.init(st, object_proto, fn_proto)
  let #(date, st) = b_date.init(st, object_proto, fn_proto)
  // 9. Promise.
  let #(promise, st) = b_promise.init(st, object_proto, fn_proto)
  // 10. Iterator prototypes (%IteratorPrototype% + per-kind + async).
  let #(iters, st) = b_iterator.init(st, object_proto, fn_proto)
  // 11. Generator / AsyncGenerator / AsyncFunction intrinsics.
  let #(#(generator, generator_fn), st) =
    b_generator.init(st, iters.iterator_proto, fn_proto, fn_ctor)
  let #(#(async_gen, _async_gen_fn), st) =
    b_generator.init_async(st, iters.async_iterator_proto, fn_proto, fn_ctor)
  let #(async_fn, st) = b_generator.init_async_function(st, fn_proto, fn_ctor)
  // 12. Collections.
  let #(map, st) = b_map.init(st, object_proto, fn_proto)
  let #(set, st) = b_set.init(st, object_proto, fn_proto)
  let #(#(weak_map, weak_set), st) = b_weak.init(st, object_proto, fn_proto)
  // DisposableStack / AsyncDisposableStack constructors + prototypes.
  let #(disposable_stack, st) =
    b_disposable_stack.init(st, object_proto, fn_proto)
  let #(async_disposable_stack, st) =
    b_disposable_stack.init_async(st, object_proto, fn_proto)
  // 13. Proxy.
  let #(proxy, st) = b_proxy.init(st, object_proto, fn_proto)
  // 14. Binary data.
  let #(#(array_buffer, shared_array_buffer), st) =
    b_array_buffer.init(st, object_proto, fn_proto)
  let #(data_view, st) = b_data_view.init(st, object_proto, fn_proto)
  let #(#(_ta_base, typed_arrays), st) =
    b_typed_array.init(st, object_proto, fn_proto, array)
  // 15. Global functions (eval, URI codecs). §21.1.2.12/.13:
  // `Number.parseInt === parseInt` etc — the four handles allocated by
  // `b_number.init` are reused rather than allocating twins.
  let #(gfns, st) =
    b_global_fns.init(
      st,
      fn_proto,
      id,
      parse_int: nb.parse_int,
      parse_float: nb.parse_float,
      is_nan: nb.is_nan,
      is_finite: nb.is_finite,
    )
  // 16. Intl (+ the ECMA-402 overrides on Number/BigInt prototypes) and
  // Temporal.
  let #(intl, st) =
    b_intl.init(st, object_proto, fn_proto, number.prototype, bigint.prototype)
  let #(temporal, st) = b_temporal.init(st, object_proto, fn_proto)
  // 17. globalThis — allocated last so it can reference every constructor.
  let #(global_object, st) =
    alloc_global_object(
      st,
      object_proto,
      gfns,
      GlobalRefs(
        object:,
        function:,
        array:,
        string:,
        number:,
        boolean:,
        symbol:,
        bigint:,
        errors:,
        dom_exception:,
        map:,
        set:,
        weak_map:,
        weak_set:,
        disposable_stack:,
        async_disposable_stack:,
        date:,
        regexp:,
        promise:,
        iterator: iters.iterator,
        proxy:,
        array_buffer:,
        shared_array_buffer:,
        data_view:,
        typed_arrays:,
        math:,
        json:,
        reflect:,
        console:,
        atomics:,
        intl:,
        temporal:,
      ),
    )
  // Assemble the Realm record — every field populated, no Options.
  let realm =
    Realm(
      object:,
      function:,
      array:,
      string:,
      number:,
      boolean:,
      symbol:,
      bigint:,
      error: errors.error,
      type_error: errors.type_error,
      reference_error: errors.reference_error,
      range_error: errors.range_error,
      syntax_error: errors.syntax_error,
      eval_error: errors.eval_error,
      uri_error: errors.uri_error,
      aggregate_error: errors.aggregate_error,
      map:,
      set:,
      weak_map:,
      weak_set:,
      date:,
      regexp:,
      promise:,
      proxy:,
      array_buffer:,
      data_view:,
      typed_arrays:,
      math:,
      json:,
      reflect:,
      console:,
      atomics:,
      iterator_proto: iters.iterator_proto,
      array_iter_proto: iters.array_iter_proto,
      string_iter_proto: iters.string_iter_proto,
      map_iter_proto: iters.map_iter_proto,
      set_iter_proto: iters.set_iter_proto,
      async_iterator_proto: iters.async_iterator_proto,
      async_from_sync_proto: iters.async_from_sync_proto,
      iterator: iters.iterator,
      iterator_helper_proto: iters.iterator_helper_proto,
      wrap_for_valid_proto: iters.wrap_for_valid_proto,
      generator:,
      generator_fn:,
      async_fn:,
      async_gen:,
      throw_type_error:,
      global_object:,
      shared_array_buffer:,
      id:,
      lexical_globals: dict.new(),
      suppressed_error: errors.suppressed_error,
    )
  // 18. Pin every realm handle (idempotent — most are already pinned by
  // alloc_proto/init_type, this catches any that arrived by another route).
  let st =
    list.fold(realm_ops.realm_handles(realm), st, fn(st, h) {
      rt_store.t_pin_root(st, h)
    })
  // 19. Register the realm and make it current so `st.realm` reads succeed
  // from here on (the JsOps bodies + every native call rely on it).
  let st = Agent(..st, realm:, realms: dict.insert(st.realms, id, realm))
  #(realm, st)
}

/// §9.6 InitializeHostDefinedRealm for an agent that already runs one: a
/// fresh realm with its own intrinsics and global object, registered in
/// `st.realms`. The current realm is unchanged on return.
pub fn create_realm(st: Agent) -> #(Realm, Agent) {
  let origin = st.realm
  let #(realm, st) = init_realm(Agent(..st, realm: rt_types.unset_realm()))
  #(realm, Agent(..st, realm: origin))
}

/// Rebind `st.store.ops` to the runtime's own `JsOps` bodies; the bytecode
/// entries stay unlinked stubs until the interpreter links them. Runs when
/// an agent is built, and again in `snapshot.deserialize`, whose decoded
/// store carries no ops.
pub fn seed_ops(st: Agent) -> Agent {
  let js = st.store
  Agent(
    ..st,
    store: JsStore(
      ..js,
      ops: JsOps(
        get_prop: rt_obj.t_get_prop,
        call: rt_call.t_call_checked,
        to_object: realm_ops.t_box_primitive,
        new_error: realm_ops.t_new_error,
        eval_hook: no_eval,
        call_bytecode: fn(_, _, _, _, _) {
          interpreter_not_linked("call_bytecode")
        },
        construct_bytecode: fn(_, _, _, _) {
          interpreter_not_linked("construct_bytecode")
        },
        resume_frame: fn(_, _, _) { interpreter_not_linked("resume_frame") },
      ),
    ),
  )
}

/// `eval_hook` seed for an agent with no interpreter linked. Unlike the
/// bytecode entries this IS reachable from user code (`Function("...")`,
/// `(0, eval)("...")` in compiled code), so it is a JS-level TypeError, not
/// a wiring panic. `interp/entry.link` replaces it.
fn no_eval(st: Agent, _source: String, _kind: rt_types.EvalKind) -> a {
  rt_val.t_throw_type_error(
    st,
    "eval is not supported in this environment: no interpreter linked",
  )
}

/// The bytecode `JsOps` entries are seeded by the interpreter when it builds
/// an engine; a bare runtime agent has none, and reaching one is a wiring
/// bug rather than a JS error.
fn interpreter_not_linked(op: String) -> a {
  panic as { "JsOps." <> op <> ": interpreter not linked" }
}

// ── globalThis (arc builtins.gleam:489-635) ─────────────────────────────────

/// The constructor/namespace handles `alloc_global_object` binds — internal
/// bundle so `init_realm` doesn't pass 25 positional args.
type GlobalRefs {
  GlobalRefs(
    object: BuiltinPair,
    function: BuiltinPair,
    array: BuiltinPair,
    string: BuiltinPair,
    number: BuiltinPair,
    boolean: BuiltinPair,
    symbol: BuiltinPair,
    bigint: BuiltinPair,
    errors: b_error.ErrorFamily,
    dom_exception: BuiltinPair,
    map: BuiltinPair,
    set: BuiltinPair,
    weak_map: BuiltinPair,
    weak_set: BuiltinPair,
    disposable_stack: BuiltinPair,
    async_disposable_stack: BuiltinPair,
    date: BuiltinPair,
    regexp: BuiltinPair,
    promise: BuiltinPair,
    iterator: BuiltinPair,
    proxy: BuiltinPair,
    array_buffer: BuiltinPair,
    shared_array_buffer: BuiltinPair,
    data_view: BuiltinPair,
    typed_arrays: rt_types.TypedArrays,
    math: Handle,
    json: Handle,
    reflect: Handle,
    console: Handle,
    atomics: Handle,
    intl: Handle,
    temporal: Handle,
  )
}

/// A global entry: name + value + descriptor shape.
type GlobalEntry {
  /// §19.1: NaN, Infinity, undefined — {W:F, E:F, C:F}.
  Immutable(name: String, val: JsVal)
  /// Normal builtin — {W:T, E:F, C:T}.
  Builtin(name: String, val: JsVal)
}

/// Allocate the `globalThis` object with every §19.1-§19.3 binding installed.
/// Port of arc `builtins.gleam:489-635`.
fn alloc_global_object(
  st: Agent,
  object_proto: Handle,
  gfns: b_global_fns.GlobalFns,
  r: GlobalRefs,
) -> #(Handle, Agent) {
  let ctor = fn(bt: BuiltinPair) { mk_object(bt.constructor) }
  let ns = fn(h: Handle) { mk_object(h) }
  let entries = [
    // §19.1: {W:F, E:F, C:F}.
    Immutable("NaN", mk_number(JNan)),
    Immutable("Infinity", mk_number(JPosInf)),
    Immutable("undefined", mk_undefined()),
    // Constructors.
    Builtin("Object", ctor(r.object)),
    Builtin("Function", ctor(r.function)),
    Builtin("Array", ctor(r.array)),
    Builtin("String", ctor(r.string)),
    Builtin("Number", ctor(r.number)),
    Builtin("Boolean", ctor(r.boolean)),
    Builtin("Symbol", ctor(r.symbol)),
    Builtin("BigInt", ctor(r.bigint)),
    Builtin("Error", ctor(r.errors.error)),
    Builtin("TypeError", ctor(r.errors.type_error)),
    Builtin("ReferenceError", ctor(r.errors.reference_error)),
    Builtin("RangeError", ctor(r.errors.range_error)),
    Builtin("SyntaxError", ctor(r.errors.syntax_error)),
    Builtin("EvalError", ctor(r.errors.eval_error)),
    Builtin("URIError", ctor(r.errors.uri_error)),
    Builtin("AggregateError", ctor(r.errors.aggregate_error)),
    Builtin("SuppressedError", ctor(r.errors.suppressed_error)),
    Builtin("DOMException", ctor(r.dom_exception)),
    Builtin("Map", ctor(r.map)),
    Builtin("Set", ctor(r.set)),
    Builtin("WeakMap", ctor(r.weak_map)),
    Builtin("WeakSet", ctor(r.weak_set)),
    Builtin("DisposableStack", ctor(r.disposable_stack)),
    Builtin("AsyncDisposableStack", ctor(r.async_disposable_stack)),
    Builtin("Date", ctor(r.date)),
    Builtin("RegExp", ctor(r.regexp)),
    Builtin("Promise", ctor(r.promise)),
    Builtin("Iterator", ctor(r.iterator)),
    Builtin("Proxy", ctor(r.proxy)),
    Builtin("ArrayBuffer", ctor(r.array_buffer)),
    Builtin("SharedArrayBuffer", ctor(r.shared_array_buffer)),
    Builtin("DataView", ctor(r.data_view)),
    // Namespace objects.
    Builtin("Math", ns(r.math)),
    Builtin("JSON", ns(r.json)),
    Builtin("Reflect", ns(r.reflect)),
    Builtin("console", ns(r.console)),
    Builtin("Atomics", ns(r.atomics)),
    Builtin("Intl", ns(r.intl)),
    Builtin("Temporal", ns(r.temporal)),
    // Global functions (§19.2).
    Builtin("eval", ns(gfns.eval)),
    Builtin("parseInt", ns(gfns.parse_int)),
    Builtin("parseFloat", ns(gfns.parse_float)),
    Builtin("isNaN", ns(gfns.is_nan)),
    Builtin("isFinite", ns(gfns.is_finite)),
    Builtin("decodeURI", ns(gfns.decode_uri)),
    Builtin("encodeURI", ns(gfns.encode_uri)),
    Builtin("decodeURIComponent", ns(gfns.decode_uri_component)),
    Builtin("encodeURIComponent", ns(gfns.encode_uri_component)),
    Builtin("escape", ns(gfns.escape)),
    Builtin("unescape", ns(gfns.unescape)),
  ]
  // The 11 TypedArray constructors (Int8Array .. BigUint64Array), in
  // `all_typed_array_kinds` order so the global's key order is stable.
  let entries =
    list.append(
      entries,
      list.filter_map(rt_types.all_typed_array_kinds, fn(kind) {
        case dict.get(r.typed_arrays.by_kind, kind) {
          Ok(bt) -> Ok(Builtin(b_typed_array.kind_name(kind), ctor(bt)))
          Error(Nil) -> Error(Nil)
        }
      }),
    )
  // Materialise property descriptors with threaded seq stamps.
  let #(props, st) =
    list.fold(entries, #([], st), fn(acc, e) {
      let #(props, st) = acc
      case e {
        Immutable(name:, val:) -> {
          let #(p, st) = common.data_prop(st, val)
          #([#(name, p), ..props], st)
        }
        Builtin(name:, val:) -> {
          let #(p, st) = common.builtin_property(st, val)
          #([#(name, p), ..props], st)
        }
      }
    })
  let #(global_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: Ordinary,
        proto: Some(object_proto),
        props: common.named_props(list.reverse(props)),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = rt_store.t_pin_root(st, global_h)
  // globalThis self-reference {W:T, E:F, C:T}.
  let #(self_prop, st) = common.builtin_property(st, mk_object(global_h))
  let st = common.add_named_property(st, global_h, "globalThis", self_prop)
  #(global_h, st)
}

// ───────────────────────── dispatch_native (M4→M6 seam) ─────────────────────

/// The single native-method dispatcher — port of arc's per-module `dispatch`
/// fan-out. Called by `rt_call.do_call` for `KNative(tag:)` cells via the
/// `arc_rt_builtins_ffi` shim. D7: throws RAISE via `t_throw` (never
/// `Result`); the caller wraps in `t_apply_protected`.
///
/// Exhaustive over the CURRENT `NativeToken` variant set — a new wrapper
/// variant added by the native-tokens unit is a compile error here by design.
pub fn dispatch_native(
  st: Agent,
  tag: NativeToken,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case tag {
    // ── async closures (bodies live in rt_async) ─────────────────────────
    PromiseResolveFn(promise:, already_resolved:) ->
      rt_async.do_resolve_fn(st, promise, already_resolved, args)
    PromiseRejectFn(promise:, already_resolved:) ->
      rt_async.do_reject_fn(st, promise, already_resolved, args)
    AsyncResume(gen:, is_throw:) ->
      rt_async.do_async_resume(st, gen, is_throw, args)
    AsyncGenResume(gen:, is_throw:, kind:) -> #(
      mk_undefined(),
      rt_async.t_asyncgen_resume(
        st,
        gen,
        is_throw,
        kind,
        first_arg_or_undefined(args),
      ),
    )
    // ── shared helpers ──────────────────────────────────────────────────────
    ReturnThis -> #(this, st)
    ThrowTypeErrorPoison ->
      b_function.dispatch(st, rt_types.ThrowTypeErrorFn, this, args)
    NativeUnseeded ->
      panic as "dispatch_native: NativeUnseeded token reached (unimplemented builtin)"
    // ── embedder natives: plain [[Call]], NewTarget undefined (§10.2.1) ─────
    HostFn(id:) -> call_host_fn(st, id, this, args, mk_undefined())
    // ── per-module wrapper variants ─────────────────────────────────────────
    ObjectN(n) -> b_object.dispatch(st, n, this, args)
    FunctionN(n) -> b_function.dispatch(st, n, this, args)
    ErrorN(n) -> b_error.dispatch(st, n, this, args, mk_undefined())
    DomExceptionN(n) ->
      b_dom_exception.dispatch(st, n, this, args, mk_undefined())
    ArrayN(n) -> b_array.dispatch(st, n, this, args)
    StringN(n) -> b_string.dispatch(st, n, this, args)
    NumberN(n) -> b_number.dispatch(st, n, this, args)
    BooleanN(n) -> b_boolean.dispatch(st, n, this, args)
    SymbolN(n) -> b_symbol.dispatch(st, n, this, args)
    BigIntN(n) -> b_bigint.dispatch(st, n, this, args)
    MathN(n) -> b_math.dispatch(st, n, this, args)
    JsonN(n) -> b_json.dispatch(st, n, this, args)
    ReflectN(n) -> b_reflect.dispatch(st, n, this, args)
    ConsoleN(n) -> b_console.dispatch(st, n, this, args)
    GlobalN(n) -> b_global_fns.dispatch(st, n, this, args)
    DateN(n) -> b_date.dispatch(st, n, this, args)
    RegExpN(n) -> b_regexp.dispatch(st, n, this, args)
    PromiseN(n) -> b_promise.dispatch(st, n, this, args)
    ProxyN(n) -> b_proxy.dispatch(st, n, this, args)
    IteratorN(n) -> b_iterator.dispatch(st, n, this, args)
    GeneratorN(n) -> b_generator.dispatch(st, n, this, args)
    MapN(n) -> b_map.dispatch(st, n, this, args)
    SetN(n) -> b_set.dispatch(st, n, this, args)
    WeakN(n) -> b_weak.dispatch(st, n, this, args)
    DisposableStackN(n) -> b_disposable_stack.dispatch(st, n, this, args)
    ArrayBufferN(n) -> b_array_buffer.dispatch(st, n, this, args)
    DataViewN(n) -> b_data_view.dispatch(st, n, this, args)
    TypedArrayN(n) -> b_typed_array.dispatch(st, n, this, args)
    AtomicsN(n) -> b_atomics.dispatch(st, n, this, args)
    Test262N(n) -> rt_realm.dispatch_262(st, n, this, args, create_realm)
    IntlN(n) -> b_intl.dispatch(st, n, this, args)
    TemporalN(n) -> b_temporal.dispatch(st, n, this, args)
  }
}

/// Native-constructor dispatch — port of arc's `[[Construct]]` fan-out. Called
/// by `rt_call.construct_by_kind` for `KNative(constructible: True)` cells.
/// `new_target` is the original `new.target` (may differ from callee under
/// `Reflect.construct` / `super`). Returns the allocated instance handle.
///
/// arc unifies call/construct via an `Option(new_target)` param; 2core's
/// rt_call splits them (rt_call.gleam:83-98 forward-decls) so this
/// routes to per-module `dispatch_construct` (or an inline
/// OrdinaryCreateFromConstructor) for every constructor that needs
/// `new_target`. Exhaustive: every `constructible: True` token has an arm.
pub fn dispatch_native_construct(
  st: Agent,
  tag: NativeToken,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case tag {
    ObjectN(n) -> b_object.dispatch_construct(st, n, args, new_target)
    ErrorN(n) -> {
      let #(v, st) = b_error.dispatch(st, n, mk_undefined(), args, new_target)
      require_handle(st, v)
    }
    DomExceptionN(n) -> {
      let #(v, st) =
        b_dom_exception.dispatch(st, n, mk_undefined(), args, new_target)
      require_handle(st, v)
    }
    HostFn(id:) -> construct_host_fn(st, id, args, new_target)
    MapN(n) -> b_map.dispatch_construct(st, n, args, new_target)
    SetN(n) -> b_set.dispatch_construct(st, n, args, new_target)
    WeakN(n) -> b_weak.dispatch_construct(st, n, args, new_target)
    DisposableStackN(n) ->
      b_disposable_stack.dispatch_construct(st, n, args, new_target)
    DateN(n) -> b_date.dispatch_construct(st, n, args, new_target)
    RegExpN(n) -> b_regexp.dispatch_construct(st, n, args, new_target)
    ProxyN(n) -> b_proxy.dispatch_construct(st, n, args, new_target)
    PromiseN(_) -> b_promise.dispatch_construct(st, args, new_target)
    ArrayBufferN(n) ->
      b_array_buffer.dispatch_construct(st, n, args, new_target)
    DataViewN(n) -> b_data_view.dispatch_construct(st, n, args, new_target)
    TypedArrayN(n) -> b_typed_array.dispatch_construct(st, n, args, new_target)
    IntlN(n) -> b_intl.dispatch_construct(st, n, args, new_target)
    TemporalN(n) -> b_temporal.dispatch_construct(st, n, args, new_target)
    // §22.1.1 Array — proto derived from new.target, then ArrayCreate.
    // b_array has no dispatch_construct yet (out-of-scope file), so allocate
    // via its call path then fix up [[Prototype]] before returning.
    ArrayN(n) -> {
      let r = st.realm
      let #(proto, st) =
        proto_from_new_target(st, new_target, r.array.prototype)
      let #(v, st) = b_array.dispatch(st, n, mk_undefined(), args)
      let #(h, st) = require_handle(st, v)
      let #(_ok, st) = rt_obj.t_set_prototype(st, h, Some(proto))
      #(h, st)
    }
    // §22.1.1.1 String — s = args ? ToString(value) : "" (no symbol special
    // case under [[Construct]]); StringCreate(s, proto-from-new.target).
    StringN(StringConstructor) -> {
      let r = st.realm
      let #(s, st) = case args {
        [] -> #("", st)
        [v, ..] -> rt_val.t_to_string(st, v)
      }
      let #(proto, st) =
        proto_from_new_target(st, new_target, r.string.prototype)
      realm_ops.alloc_wrapper(st, StringObj(s), proto)
    }
    // §21.1.1.1 Number — n = args ? ToNumeric (BigInt→𝔽) : +0; wrap.
    NumberN(NumberConstructor) -> {
      let r = st.realm
      let #(v, st) =
        b_number.dispatch(st, NumberConstructor, mk_undefined(), args)
      let n = case classify(v) {
        rt_types.KNum(n) -> n
        _ -> JInt(0)
      }
      let #(proto, st) =
        proto_from_new_target(st, new_target, r.number.prototype)
      realm_ops.alloc_wrapper(st, NumberObj(n), proto)
    }
    // §20.3.1.1 Boolean — b = ToBoolean(value); wrap.
    BooleanN(BooleanConstructor) -> {
      let r = st.realm
      let b = case args {
        [] -> False
        [v, ..] -> rt_val.to_boolean(v)
      }
      let #(proto, st) =
        proto_from_new_target(st, new_target, r.boolean.prototype)
      realm_ops.alloc_wrapper(st, BooleanObj(b), proto)
    }
    // §20.4.1.1 step 1: NewTarget defined → TypeError.
    SymbolN(SymbolConstructor) ->
      rt_val.t_throw_type_error(st, "Symbol is not a constructor")
    // §21.2.1.1 step 1: NewTarget defined → TypeError. Unreachable in
    // practice (`constructible: False`) but explicit so `require_handle`
    // never sees a primitive BigInt.
    BigIntN(_) -> rt_val.t_throw_type_error(st, "BigInt is not a constructor")
    // §20.2.1.1 / §27.3.1.1 dynamic Function-family constructors:
    // CreateDynamicFunction(C, NewTarget, kind, args).
    FunctionN(n) -> {
      let #(v, st) = b_function.dispatch_construct(st, n, args, new_target)
      require_handle(st, v)
    }
    GeneratorN(n) -> {
      let #(v, st) = b_generator.dispatch_construct(st, n, args, new_target)
      require_handle(st, v)
    }
    // Non-constructor method tokens on constructible types.
    StringN(_) | NumberN(_) | BooleanN(_) | SymbolN(_) ->
      rt_val.t_throw_type_error(st, "not a constructor")
    // Every remaining token is `constructible: False` — construct_by_kind
    // never routes it here. Reaching this arm is an engine bug.
    // §27.1.1.1 Iterator — abstract constructor (only IteratorConstructor is
    // constructible; every other IteratorN token is `constructible: False`).
    IteratorN(n) -> b_iterator.dispatch_construct(st, n, args, new_target)
    PromiseResolveFn(..)
    | PromiseRejectFn(..)
    | AsyncResume(..)
    | AsyncGenResume(..)
    | ReturnThis
    | ThrowTypeErrorPoison
    | NativeUnseeded
    | MathN(_)
    | JsonN(_)
    | ReflectN(_)
    | ConsoleN(_)
    | GlobalN(_)
    | AtomicsN(_)
    | Test262N(_) ->
      panic as "dispatch_native_construct: non-constructible token reached [[Construct]]"
  }
}

/// §10.1.13.2 GetPrototypeFromConstructor with a per-type intrinsic fallback.
fn proto_from_new_target(
  st: Agent,
  new_target: JsVal,
  fallback: Handle,
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_obj.t_get_prop(st, new_target, StringKey(Named("prototype")))
  case classify(proto) {
    KHandle(h) -> #(h, st)
    _ -> #(fallback, st)
  }
}

fn require_handle(st: Agent, v: JsVal) -> #(Handle, Agent) {
  case classify(v) {
    KHandle(h) -> #(h, st)
    _ ->
      panic as "dispatch_native_construct: native constructor returned non-object"
  }
}

// ───────────────────────────── embedder natives ─────────────────────────────

/// Run the closure registered under `HostFn(id)`. The one site where the
/// embedder's `Result` contract meets the runtime's raise contract:
/// `Error(thrown)` becomes `t_throw`. An id with no entry (a deserialized
/// engine whose natives were not re-registered) is a TypeError.
fn call_host_fn(
  st: Agent,
  id: Int,
  this: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  case dict.get(st.host_fns, id) {
    Ok(HostFnEntry(call:, ..)) ->
      case call(st, args, this, new_target) {
        #(st, Ok(v)) -> #(v, st)
        #(st, Error(thrown)) -> rt_store.t_throw(st, thrown)
      }
    Error(Nil) ->
      rt_val.t_throw_type_error(
        st,
        "host function #" <> int.to_string(id) <> " is not registered",
      )
  }
}

/// [[Construct]] of a host class. The closure sees `new_target`, `this` is
/// undefined, and it must return an object. That object is re-prototyped to
/// `new_target.prototype` when it is an own data property holding an object,
/// so `class Sub extends HostClass {}` yields `Sub.prototype` instances even
/// when the closure allocated a plain object (port of arc `exec/call.gleam`
/// do_construct, native arm).
fn construct_host_fn(
  st: Agent,
  id: Int,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(v, st) = call_host_fn(st, id, mk_undefined(), args, new_target)
  case classify(v), own_data_prototype(st, new_target) {
    KHandle(h), Some(proto) -> {
      let #(_changed, st) = rt_obj.t_set_prototype(st, h, Some(proto))
      #(h, st)
    }
    KHandle(h), None -> #(h, st)
    _, _ ->
      rt_val.t_throw_type_error(st, "host constructor must return an object")
  }
}

/// `ctor.prototype` iff it is an own DATA property holding an object: no
/// getter runs and no proxy trap fires.
fn own_data_prototype(st: Agent, ctor: JsVal) -> option.Option(Handle) {
  use h <- option.then(as_handle(ctor))
  case rt_obj.t_ordinary_own_property(st, h, StringKey(Named("prototype"))) {
    Some(DataProperty(value:, ..)) -> as_handle(value)
    _ -> None
  }
}

fn as_handle(v: JsVal) -> option.Option(Handle) {
  case classify(v) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}
