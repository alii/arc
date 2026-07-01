import arc/vm/builtins/common.{type Builtins}
import arc/vm/builtins/disposable_stack
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/helpers
import arc/vm/builtins/iterator as builtins_iterator
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/regexp as builtins_regexp
import arc/vm/completion.{
  type Completion, AwaitCompletion, NormalCompletion, ThrowCompletion,
  YieldCompletion,
}
import arc/vm/exec/call
import arc/vm/exec/dynamic_import
import arc/vm/exec/generators
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/internal/job_queue
import arc/vm/internal/tuple_array
import arc/vm/opcode.{
  type Op, Add, ArrayFrom, ArrayFromWithHoles, ArrayPush, ArrayPushHole,
  ArraySpread, AsyncYieldStarNext, AsyncYieldStarResume, Await, BinOp, BoxLocal,
  Call, CallApply, CallConstructor, CallConstructorApply, CallEval, CallMethod,
  CallMethodApply, CmpLocalConstJump, CmpLocalLocalJump, CreateArguments,
  CreateRestArray, DecLocal, DeclareEvalVar, DeclareGlobalLex, DeclareGlobalVar,
  DefineAccessor, DefineAccessorComputed, DefineField, DefineFieldComputed,
  DefineMethod, DefineMethodComputed, DefinePrivateAccessor, DefinePrivateField,
  DefinePrivateMethod, DeleteElem, DeleteField, Dup, ForInNext, ForInStart,
  GetAsyncIterator, GetBoxed, GetElem, GetElem2, GetEvalVar, GetField, GetField2,
  GetGlobal, GetIterator, GetLocal, GetPrivateField, GetPrivateField2,
  GetPrivateFieldDyn, GetPrivateFieldDyn2, GetPrototypeOf, GetSuperValue,
  GetSuperValue2, IncLocal, InitGlobalLex, InitialYield, IteratorCheckObject,
  IteratorClose, IteratorCloseThrow, IteratorNext, IteratorRecord, IteratorRest,
  Jump, JumpIfFalse, JumpIfNullish, JumpIfTrue, MakeClosure, MakeMethod,
  NewObject, NewPrivateName, NewRegExp, ObjectRestCopy, ObjectSpread, Pop,
  PrivateIn, PrivateInDyn, PushConst, PushTry, PutBoxed, PutBoxedCheckInit,
  PutElem, PutEvalVar, PutField, PutGlobal, PutLocal, PutLocalCheckInit,
  PutPrivateField, PutPrivateFieldDyn, PutSuperValue, Return, Rot3, SetLine,
  SetProto, SetupDerivedClass, Swap, TypeOf, TypeofEvalVar, TypeofGlobal,
  UnaryOp, Unrot4, Yield, YieldStar,
}
import arc/vm/ops/array as array_ops
import arc/vm/ops/coerce
import arc/vm/ops/object
import arc/vm/ops/operators
import arc/vm/ops/property
import arc/vm/realm
import arc/vm/state.{
  type Heap, type NativeFnSlot, type State, type StepResult, type VmError,
  Awaited, Done, SavedFrame, StackUnderflow, State, StepVmError, Thrown,
  TryFrame, Unimplemented, Yielded,
}
import arc/vm/value.{
  type FuncTemplate, type JsValue, type Ref, ArrayIteratorObject, ArrayObject,
  DataProperty, EvalEnvSlot, ForInIteratorSlot, FunctionObject, GeneratorObject,
  JsBool, JsNull, JsObject, JsString, JsUndefined, JsUninitialized, Named,
  NativeFunction, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string

/// Single-traversal `obj.x = v` overwrite of an existing own writable data
/// property, preserving its attribute flags and creation seq (§10.1.11 —
/// the key keeps its enumeration position). Error(Nil) when the key is
/// absent, non-writable, or an accessor — callers take the full [[Set]]
/// path. FFI mirrors the DataProperty tuple layout; see arc_vm_ffi.erl.
@external(erlang, "arc_vm_ffi", "put_existing_writable_data")
fn put_existing_writable_data(
  properties: dict.Dict(value.PropertyKey, value.Property),
  key: value.PropertyKey,
  val: JsValue,
) -> Result(dict.Dict(value.PropertyKey, value.Property), Nil)

// ============================================================================
// Internal state (types defined in state.gleam for cross-module access)
// ============================================================================

/// The call_fn callback stored in State, backing `state.call` — the re-entrant
/// JS call made from native code (e.g. Array.prototype.map's callback). The
/// caller already holds a State, so it drives the lossless
/// `call_value_to_completion` directly and narrows it to the host-fn
/// `Ok(value)`/`Error(thrown)` contract; a `VmError` or a stray Yield/Await is an
/// engine bug here (no channel to surface it through, mid-execution) → panic.
fn call_fn_callback(
  state: State(host),
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case call_value_to_completion(state, callee, this_val, args) {
    Ok(#(NormalCompletion(val), s)) -> Ok(#(val, s))
    Ok(#(ThrowCompletion(thrown), s)) -> Error(#(thrown, s))
    Ok(#(YieldCompletion(_), _)) | Ok(#(AwaitCompletion(_), _)) ->
      panic as "Yield/Await completion should not appear in a re-entrant call"
    Error(vm_err) ->
      panic as { "VM error in re-entrant call: " <> string.inspect(vm_err) }
  }
}

/// Drive one function-value call to completion from outside the normal frame
/// flow (promise jobs, native re-entry, embedder exports). Routes through
/// `call_value`, so generator / async / async-generator callees get their
/// proper [[Call]] semantics — a generator object or promise — instead of
/// having their bodies executed inline. (An earlier re-entry path ran callee
/// bodies inline, which leaked Yield/Await completions into contexts that
/// cannot resume them.)
///
/// A non-callable callee is the legacy pass-through `undefined` (no throw)
/// that promise reactions and friends rely on.
fn call_value_to_completion(
  state: State(host),
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
) -> Result(#(Completion, State(host)), VmError) {
  case helpers.is_callable(state.heap, callee) {
    False -> Ok(#(NormalCompletion(JsUndefined), state))
    True -> {
      // Sentinel frame, same trick as construct_fn_callback: the call
      // machinery returns to pc+1 = 1, which is Return, so execute_inner
      // finishes with the call result on top of the isolated stack.
      // Pre-built at init (ctx.callback_sentinel) — this path runs once per
      // element in Array.prototype.map and friends.
      let sentinel = state.ctx.callback_sentinel
      let isolated =
        State(
          ..state,
          stack: [],
          pc: 0,
          func: sentinel,
          code: sentinel.bytecode,
          constants: sentinel.constants,
          call_stack: [],
          try_stack: [],
        )
      case call_value(isolated, callee, args, this_val) {
        Ok(entered) ->
          case execute_inner(entered) {
            Ok(#(comp, final_state)) ->
              Ok(#(comp, merge_back(state, final_state)))
            Error(vm_err) -> Error(vm_err)
          }
        Error(#(Thrown, thrown, post)) ->
          Ok(#(ThrowCompletion(thrown), merge_back(state, post)))
        Error(#(StepVmError(vm_err), _, _)) -> Error(vm_err)
        Error(#(step, _, _)) ->
          Error(Unimplemented(
            "unexpected step result entering a re-entrant call: "
            <> string.inspect(step),
          ))
      }
    }
  }
}

/// Build the ObjectSlot for a frozen Array used by GetTemplateObject
/// (§13.2.8.4 steps 6-15): every index property is a dict override
/// { writable: false, enumerable: true, configurable: false } (dense
/// elements can't carry attributes), "length" is { writable: false,
/// enumerable: false, configurable: false }, and the slot is non-extensible
/// — together exactly SetIntegrityLevel(A, frozen).
fn frozen_array_slot(
  values: List(JsValue),
  count: Int,
  extra_props: List(#(value.PropertyKey, value.Property)),
  array_proto: Ref,
) {
  let index_props =
    list.index_map(values, fn(v, i) {
      #(
        value.Index(i),
        // seq: 0 — Index keys enumerate numerically, never by seq.
        DataProperty(
          value: v,
          writable: False,
          enumerable: True,
          configurable: False,
          seq: 0,
        ),
      )
    })
  let length_prop = #(
    Named("length"),
    // seq: 0 — array "length" never enumerates through the seq-ordered
    // named-key path.
    DataProperty(
      value: value.from_int(count),
      writable: False,
      enumerable: False,
      configurable: False,
      seq: 0,
    ),
  )
  ObjectSlot(
    kind: ArrayObject(count),
    properties: dict.from_list([
      length_prop,
      ..list.append(index_props, extra_props)
    ]),
    elements: elements.new(),
    prototype: Some(array_proto),
    symbol_properties: [],
    extensible: False,
  )
}

/// Single-record-update equivalent of
/// `State(..state.merge_globals(parent, child, []), heap:)` — thread VM-global
/// state (lexical globals, job queue, event-loop counters) from a child
/// execution back to the parent plus the child's heap in ONE State copy
/// instead of two. Hot: runs once per re-entrant callback (Array.prototype.map
/// element calls, promise jobs, ...).
fn merge_back(parent: State(host), child: State(host)) -> State(host) {
  let heap = child.heap
  // Fast path: the typical callback (Array.prototype.map element call, ...)
  // touches none of the shared fields, so the child's terms are the exact
  // terms the isolated state was built from — `==` hits BEAM's
  // pointer-equality shortcut and each check is O(1). Skips the RealmCtx
  // rebuild plus its extra State-field copies per re-entrant call.
  case
    child.ctx == parent.ctx
    && child.job_queue == parent.job_queue
    && child.outstanding == parent.outstanding
    && child.atomics_waiters == parent.atomics_waiters
  {
    True -> State(..parent, heap:)
    False ->
      State(
        ..parent,
        heap:,
        ctx: state.RealmCtx(
          ..parent.ctx,
          lexical_globals: child.ctx.lexical_globals,
          template_objects: child.ctx.template_objects,
          // Realms registered during the re-entrant call (ShadowRealm /
          // $262.createRealm constructors) must survive the merge.
          realms: child.ctx.realms,
        ),
        job_queue: child.job_queue,
        outstanding: child.outstanding,
        atomics_waiters: child.atomics_waiters,
      )
  }
}

/// The construct_fn callback that gets stored in State.
/// Wraps do_construct for re-entrant `new target(...args)` from native code
/// (e.g. Reflect.construct).
///
/// Sets up an isolated frame with a sentinel empty-bytecode func so that when
/// the constructor body returns, execute_inner hits end-of-code and yields
/// NormalCompletion with the constructed object on top of stack. The sentinel
/// func is required because Return restores `code` from SavedFrame.func.bytecode,
/// not from the state's code field directly.
fn construct_fn_callback(
  state: State(host),
  target: JsValue,
  args: List(JsValue),
  new_target: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case target, new_target {
    JsObject(ref), JsObject(nt_ref) -> {
      // Sentinel bytecode: do_construct saves pc+1 into the SavedFrame (or
      // advances pc+1 for native constructors), so we need Return at index 1.
      // Index 0 is never dispatched but present for belt-and-braces.
      // Pre-built at init (ctx.callback_sentinel).
      let sentinel = state.ctx.callback_sentinel
      let isolated =
        State(
          ..state,
          stack: [],
          pc: 0,
          func: sentinel,
          code: sentinel.bytecode,
          call_stack: [],
          try_stack: [],
        )
      // do_construct either:
      //  - pushes a SavedFrame and switches to the constructor's bytecode
      //    (regular function path), or
      //  - runs synchronously and leaves the result on stack at pc+1
      //    (native constructor path).
      // Either way, execute_inner drives to completion.
      case do_construct(isolated, ref, args, [], nt_ref) {
        Ok(entered) ->
          case execute_inner(entered) {
            Ok(#(NormalCompletion(val), final_state)) ->
              Ok(#(val, merge_back(state, final_state)))
            Ok(#(ThrowCompletion(thrown), final_state)) ->
              Error(#(thrown, merge_back(state, final_state)))
            Ok(#(YieldCompletion(_), _)) | Ok(#(AwaitCompletion(_), _)) ->
              panic as "Yield/Await completion during construct"
            Error(vm_err) ->
              panic as {
                "VM error during construct: " <> string.inspect(vm_err)
              }
          }
        Error(#(Thrown, thrown, post)) ->
          Error(#(thrown, State(..state, heap: post.heap)))
        Error(#(StepVmError(vm_err), _, _)) ->
          panic as { "VM error in do_construct: " <> string.inspect(vm_err) }
        Error(#(other, _, _post)) ->
          panic as {
            "Unexpected step result from do_construct: "
            <> string.inspect(other)
          }
      }
    }
    _, _ ->
      coerce.thrown_type_error(
        state,
        object.inspect(target, state.heap) <> " is not a constructor",
      )
  }
}

/// Create a fresh VM state from a function template.
/// Most callers can use this directly; override fields with `State(..new_state(...), ...)`
/// for cases that need non-default symbol_descriptions.
pub fn new_state(
  func: FuncTemplate,
  locals: tuple_array.TupleArray(JsValue),
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  lexical_globals: dict.Dict(String, value.LexicalGlobal),
  symbol_descriptions: dict.Dict(value.SymbolId, String),
  symbol_registry: dict.Dict(String, value.SymbolId),
  hooks: state.HostHooks,
) -> State(host) {
  State(
    stack: [],
    locals:,
    constants: func.constants,
    func:,
    code: func.bytecode,
    heap:,
    pc: 0,
    call_stack: [],
    try_stack: [],
    builtins:,
    ctx: state.RealmCtx(
      lexical_globals:,
      global_object:,
      symbol_descriptions:,
      symbol_registry:,
      template_objects: dict.new(),
      realms: dict.new(),
      call_fn: call_fn_callback,
      construct_fn: construct_fn_callback,
      callback_sentinel: value.FuncTemplate(
        ..empty_template(),
        bytecode: tuple_array.from_list([Return, Return]),
      ),
      // Embedder host capabilities (Atomics blocking wait / wake delivery),
      // supplied exactly once at engine/realm construction and inherited by
      // every derived State via `..ctx` spreads.
      host_hooks: hooks,
    ),
    new_target: JsUndefined,
    call_args: [],
    job_queue: job_queue.new(),
    unhandled_rejections: [],
    atomics_waiters: [],
    outstanding: 0,
    call_depth: 0,
    eval_env: None,
    current_line: 0,
    // Agent [[CanBlock]]: read once at state init from the process-local
    // flag (defaults to True). The test262 runner clears the flag in the
    // test's worker process before booting a realm for CanBlockIsFalse
    // tests; fresh agent processes start with it unset, i.e. True.
    can_block: host_can_block(),
  )
}

/// Agent Record [[CanBlock]] for the booting agent — process-local, default
/// True. See arc_atomics_ffi.erl; cleared by the test262 runner for tests
/// flagged CanBlockIsFalse.
@external(erlang, "arc_atomics_ffi", "can_block")
fn host_can_block() -> Bool

pub fn init_state(
  func: FuncTemplate,
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  is_module: Bool,
  hooks: state.HostHooks,
) -> State(host) {
  // ES §16.2.1.5.2 ModuleEvaluation: module `this` is undefined.
  // ES §16.1.6 ScriptEvaluation: the script's this is the global object,
  // regardless of strict mode. (Strict only affects function-body this.)
  let this_val = case is_module {
    True -> JsUndefined
    False -> JsObject(global_object)
  }
  let locals = init_top_level_locals(func, this_val)
  new_state(
    func,
    locals,
    heap,
    builtins,
    global_object,
    dict.new(),
    dict.new(),
    dict.new(),
    hooks,
  )
}

/// Build the locals array for a top-level (script/module/eval/REPL) template:
/// JsUndefined everywhere, then seed the lexical-`this` slot if present.
/// Function bodies use call.setup_locals instead — this is only for entries
/// that don't go through the call protocol.
pub fn init_top_level_locals(
  func: FuncTemplate,
  this_val: JsValue,
) -> tuple_array.TupleArray(JsValue) {
  let locals = tuple_array.repeat(JsUndefined, func.local_count)
  case func.lexical.this {
    Some(idx) -> tuple_array.set_unchecked(idx, this_val, locals)
    None -> locals
  }
}

/// Build the locals array for a module body. Module `this` is undefined
/// (§16.2.1.5.2). `seeds` places pre-allocated BoxSlot refs into specific local
/// slots: import bindings into capture slots 0..N-1 (each the exporting
/// module's live cell), plus this module's own export cells into their declared
/// slots. The body reads/writes both through GetBoxed/PutBoxed.
pub fn init_module_locals(
  func: FuncTemplate,
  seeds: List(#(Int, JsValue)),
) -> tuple_array.TupleArray(JsValue) {
  let locals = tuple_array.repeat(JsUndefined, func.local_count)
  list.fold(seeds, locals, fn(acc, seed) {
    let #(index, box) = seed
    tuple_array.set_unchecked(index, box, acc)
  })
}

/// A function template with no body — every flag off, empty bytecode. Lets the
/// few places that need a structurally-valid frame they never actually run
/// (`call_root`, and `construct_fn_callback` with a `Return` spliced in) avoid
/// hand-rolling the full record.
fn empty_template() -> FuncTemplate {
  value.FuncTemplate(
    name: None,
    arity: 0,
    length: 0,
    local_count: 0,
    bytecode: tuple_array.from_list([]),
    constants: tuple_array.from_list([]),
    functions: tuple_array.from_list([]),
    env_descriptors: [],
    is_strict: True,
    is_arrow: False,
    is_derived_constructor: False,
    is_generator: False,
    is_async: False,
    is_constructor: False,
    is_class_constructor: False,
    local_names: None,
    lexical: opcode.no_lexical_slots,
    syntax_perms: opcode.script_perms,
  )
}

/// Call a function value to completion from outside the VM — the cold-start
/// counterpart to the re-entrant `state.call`, and the engine half of
/// `entry.run_export`. Stands up a fresh root State (no enclosing frame, no
/// loaded code) on the given heap/builtins/global and invokes `callee` on it,
/// losslessly: the full `Completion`, or a `VmError`. Callers already inside the
/// VM hold a State and use `state.call` instead.
pub fn call_root(
  callee: JsValue,
  this_val: JsValue,
  args: List(JsValue),
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  hooks: state.HostHooks,
) -> Result(#(Completion, State(host)), VmError) {
  let state =
    init_state(empty_template(), heap, builtins, global_object, False, hooks)
  call_value_to_completion(state, callee, this_val, args)
}

/// Stand up a fresh root State (no enclosing frame, no loaded code) on the given
/// heap/builtins/global, for an embedder that wants to run host-side work against
/// a live State — allocate JS values, invoke held functions via `state.call`,
/// marshal data in and out — WITHOUT installing and calling a global shim.
/// The engine half of `engine.with_state`; it carries the same empty top-level
/// frame `call_root` uses, so `state.call`/`state.construct` (which push their
/// own frames) work against it.
pub fn root_state(
  heap: Heap(host),
  builtins: Builtins,
  global_object: Ref,
  hooks: state.HostHooks,
) -> State(host) {
  init_state(empty_template(), heap, builtins, global_object, False, hooks)
}

/// Allocate a closure (FunctionObject) for `child_template`, capturing
/// `captured_values` (gathered per the template's env_descriptors). Builds the
/// env, the `name`/`length` props, the `.prototype` object (+ `.constructor`)
/// for non-arrows, and links the function prototype. Shared by the MakeClosure
/// op and module link-time hoisting (so cyclic function exports are callable
/// before bodies run).
pub fn make_closure(
  heap: Heap(host),
  builtins: Builtins,
  child_template: FuncTemplate,
  captured_values: List(JsValue),
) -> #(Heap(host), Ref) {
  let #(heap, env_ref) = heap.alloc_env(heap, captured_values)
  // For non-arrow functions, pre-populate .prototype with a fresh object so
  // `Foo.prototype.bar = ...` and `new Foo()` work.
  // Build "length" BEFORE "name": §10.2.9 SetFunctionLength runs before
  // §10.2.10 SetFunctionName, and [[OwnPropertyKeys]] orders named keys by
  // creation seq (test262: built-ins/*/property-order.js).
  let length_prop = common.fn_length_property(child_template.length)
  let name_prop =
    common.fn_name_property(option.unwrap(child_template.name, ""))
  // §10.2.5 MakeConstructor / §27.3.3: only constructible functions and
  // (async) generators get an own "prototype" property. Arrows, methods,
  // accessors, and async functions have none (test262:
  // Function/prototype/Symbol.hasInstance/this-val-poisoned-prototype.js).
  let has_prototype =
    child_template.is_constructor || child_template.is_generator
  let #(heap, fn_properties, proto_ref, closure_ref) = case has_prototype {
    False -> {
      let #(heap, closure_ref) = heap.reserve(heap)
      #(
        heap,
        dict.from_list([
          #(value.Named("name"), name_prop),
          #(value.Named("length"), length_prop),
        ]),
        None,
        closure_ref,
      )
    }
    True -> {
      // Reserve the closure's ref up front so the prototype's "constructor"
      // back-pointer (§10.2.5 MakeConstructor) can reference it, then register
      // the prototype object LAZILY (QuickJS-style autoinit,
      // JS_AUTOINIT_ID_PROTOTYPE): most closures never have their `.prototype`
      // object touched, so heap.read synthesises the `{constructor: fn}` slot
      // on demand and the first write materialises it.
      // A generator's "prototype" object has no "constructor" (§27.3.3.1).
      let #(h, closure_ref) = heap.reserve(heap)
      // §27.3.3/§27.6.3: a generator function's "prototype" object inherits
      // from %GeneratorPrototype% (resp. %AsyncGeneratorPrototype%), not
      // Object.prototype.
      let proto_parent = case
        child_template.is_generator,
        child_template.is_async
      {
        True, False -> builtins.generator.prototype
        True, True -> builtins.async_generator.prototype
        False, _ -> builtins.object.prototype
      }
      let #(h, proto_obj_ref) =
        heap.alloc_lazy_proto(
          h,
          closure_ref,
          child_template.is_constructor,
          proto_parent,
        )
      // §15.7.14 ClassDefinitionEvaluation step 16: a class constructor's
      // "prototype" is non-writable; ordinary functions/generators get the
      // writable §10.2.5/§27.3.3 form.
      // seq: 2 — birth-time "prototype", after the constant-seq "length" (0)
      // and "name" (1), before any later next_prop_seq() value (see
      // common.fn_name_property). Avoids a global counter read per closure.
      let proto_prop = case child_template.is_class_constructor {
        True ->
          value.DataProperty(
            value: JsObject(proto_obj_ref),
            writable: False,
            enumerable: False,
            configurable: False,
            seq: 2,
          )
        False ->
          value.DataProperty(
            value: JsObject(proto_obj_ref),
            writable: True,
            enumerable: False,
            configurable: False,
            seq: 2,
          )
      }
      #(
        h,
        dict.from_list([
          #(value.Named("prototype"), proto_prop),
          #(value.Named("name"), name_prop),
          #(value.Named("length"), length_prop),
        ]),
        Some(proto_obj_ref),
        closure_ref,
      )
    }
  }
  let heap =
    heap.fill(
      heap,
      closure_ref,
      ObjectSlot(
        // A class constructor's [[HomeObject]] is its own .prototype, so
        // `super.x` inside the constructor resolves against the parent
        // prototype. Concise methods/accessors get their home re-set by
        // DefineMethod/DefineAccessor; for plain functions `super` is a syntax
        // error so this is never read.
        kind: FunctionObject(
          func_template: child_template,
          env: env_ref,
          home_object: proto_ref,
        ),
        properties: fn_properties,
        elements: elements.new(),
        // §27.3.3/§27.4.3/§27.7.3: generator function objects' [[Prototype]]
        // is %GeneratorFunction.prototype% (resp. %AsyncGeneratorFunction
        // .prototype%) — so Object.getPrototypeOf(function*(){}).constructor
        // is the GeneratorFunction dynamic constructor. Async functions
        // (including async arrows and methods, §15.9.3) likewise get
        // %AsyncFunction.prototype%.
        prototype: Some(
          case child_template.is_generator, child_template.is_async {
            True, False -> builtins.generator.fn_proto
            True, True -> builtins.async_generator.fn_proto
            False, True -> builtins.async_function_proto
            False, False -> builtins.function.prototype
          },
        ),
        symbol_properties: [],
        extensible: True,
      ),
    )
  #(heap, closure_ref)
}

/// Read one of the current frame's lexical pseudo-bindings. Unboxes when the
/// slot is boxed (captured by an inner arrow, or the frame IS an arrow reading
/// its capture). Returns JsUndefined when the slot is None.
pub fn read_lexical_local(
  state: State(host),
  ref: opcode.LexicalRef,
) -> JsValue {
  case opcode.lexical_slot(state.func.lexical, ref) {
    None -> JsUndefined
    Some(idx) ->
      case tuple_array.unsafe_get(idx, state.locals) {
        JsObject(r) as raw ->
          case heap.read_box(state.heap, r) {
            Some(boxed) -> boxed
            None -> raw
          }
        raw -> raw
      }
  }
}

/// Shorthand for `read_lexical_local(state, RefThis)`.
pub fn read_this_local(state: State(host)) -> JsValue {
  read_lexical_local(state, opcode.RefThis)
}

// ============================================================================
// Execution loop
// ============================================================================

/// Main execution loop. Tail-recursive.
/// Returns the completion and the final state (for job queue access).
///
/// Every bytecode stream ends with a sentinel Return (appended by
/// resolve.gleam), so fetch uses unchecked element/2 — no Option box,
/// no bounds check. Termination flows through the Return handler.
pub fn execute_inner(
  state: State(host),
) -> Result(#(Completion, State(host)), VmError) {
  fast_loop(
    state,
    state.pc,
    state.stack,
    state.locals,
    state.heap,
    state.code,
    state.constants,
    state.current_line,
  )
}

/// Hot inner loop. Carries the per-instruction-mutable hot fields (pc, stack,
/// locals, heap, current_line) as bare arguments so the common opcodes run
/// without allocating a fresh 21-field State record plus an Ok Result box on
/// every bytecode step. code/constants are loop-invariant within a frame and
/// carried to skip a field load per iteration. Any opcode not handled here —
/// and every error/throw path of the ones that are — materializes the full
/// State once via `dispatch_slow` and falls back to the general `step`
/// dispatcher, which re-executes the instruction from scratch (all fast paths
/// below are effect-free before bailing, so re-execution is safe).
fn fast_loop(
  state: State(host),
  pc: Int,
  stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  hp: Heap(host),
  code: tuple_array.TupleArray(Op),
  constants: tuple_array.TupleArray(JsValue),
  line: Int,
) -> Result(#(Completion, State(host)), VmError) {
  case tuple_array.unsafe_get(pc, code) {
    SetLine(l) ->
      fast_loop(state, pc + 1, stack, locals, hp, code, constants, l)

    PushConst(index) -> {
      let v = tuple_array.unsafe_get(index, constants)
      fast_loop(state, pc + 1, [v, ..stack], locals, hp, code, constants, line)
    }

    Pop ->
      case stack {
        [_, ..rest] ->
          fast_loop(state, pc + 1, rest, locals, hp, code, constants, line)
        [] -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    Dup ->
      case stack {
        [top, ..] ->
          fast_loop(
            state,
            pc + 1,
            [top, ..stack],
            locals,
            hp,
            code,
            constants,
            line,
          )
        [] -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    Swap ->
      case stack {
        [a, b, ..rest] ->
          fast_loop(
            state,
            pc + 1,
            [b, a, ..rest],
            locals,
            hp,
            code,
            constants,
            line,
          )
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    GetLocal(index) ->
      case tuple_array.unsafe_get(index, locals) {
        // TDZ — slow path rebuilds State and throws the ReferenceError.
        JsUninitialized -> dispatch_slow(state, pc, stack, locals, hp, line)
        v ->
          fast_loop(
            state,
            pc + 1,
            [v, ..stack],
            locals,
            hp,
            code,
            constants,
            line,
          )
      }

    PutLocal(index) ->
      case stack {
        [v, ..rest] ->
          fast_loop(
            state,
            pc + 1,
            rest,
            tuple_array.set_unchecked(index, v, locals),
            hp,
            code,
            constants,
            line,
          )
        [] -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    GetBoxed(index) ->
      case tuple_array.unsafe_get(index, locals) {
        JsObject(box_ref) ->
          case heap.read_box(hp, box_ref) {
            // TDZ / corrupt box — slow path throws.
            Some(JsUninitialized) | None ->
              dispatch_slow(state, pc, stack, locals, hp, line)
            Some(val) ->
              fast_loop(
                state,
                pc + 1,
                [val, ..stack],
                locals,
                hp,
                code,
                constants,
                line,
              )
          }
        _not_a_box -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    PutBoxed(index) ->
      case stack {
        [new_value, ..rest] ->
          case tuple_array.unsafe_get(index, locals) {
            JsObject(box_ref) ->
              fast_loop(
                state,
                pc + 1,
                rest,
                locals,
                heap.write(hp, box_ref, value.BoxSlot(new_value)),
                code,
                constants,
                line,
              )
            _not_a_box -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
        [] -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    Jump(target) ->
      fast_loop(state, target, stack, locals, hp, code, constants, line)

    JumpIfFalse(target) ->
      case stack {
        [top, ..rest] ->
          case value.is_truthy(top) {
            False ->
              fast_loop(state, target, rest, locals, hp, code, constants, line)
            True ->
              fast_loop(state, pc + 1, rest, locals, hp, code, constants, line)
          }
        [] -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    JumpIfTrue(target) ->
      case stack {
        [top, ..rest] ->
          case value.is_truthy(top) {
            True ->
              fast_loop(state, target, rest, locals, hp, code, constants, line)
            False ->
              fast_loop(state, pc + 1, rest, locals, hp, code, constants, line)
          }
        [] -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    JumpIfNullish(target) ->
      case stack {
        [JsNull, ..rest] | [JsUndefined, ..rest] ->
          fast_loop(state, target, rest, locals, hp, code, constants, line)
        [_, ..rest] ->
          fast_loop(state, pc + 1, rest, locals, hp, code, constants, line)
        [] -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    BinOp(kind) ->
      case stack {
        [right, left, ..rest] ->
          case kind {
            // §13.15.3: objects need ToPrimitive (stateful), string×non-string
            // needs js_to_string (stateful), BigInt mixes throw — all slow.
            // string×string and number×number are pure.
            Add ->
              case left, right {
                JsString(a), JsString(b) ->
                  fast_loop(
                    state,
                    pc + 1,
                    [JsString(a <> b), ..rest],
                    locals,
                    hp,
                    code,
                    constants,
                    line,
                  )
                JsObject(_), _
                | _, JsObject(_)
                | JsString(_), _
                | _, JsString(_)
                | value.JsBigInt(_), _
                | _, value.JsBigInt(_)
                -> dispatch_slow(state, pc, stack, locals, hp, line)
                _, _ ->
                  case operators.num_binop(left, right, operators.num_add) {
                    Ok(result) ->
                      fast_loop(
                        state,
                        pc + 1,
                        [result, ..rest],
                        locals,
                        hp,
                        code,
                        constants,
                        line,
                      )
                    // Slow path re-runs the op and throws the same error.
                    Error(_err) ->
                      dispatch_slow(state, pc, stack, locals, hp, line)
                  }
              }
            // Strict equality compares references — never coerces, pure.
            opcode.StrictEq | opcode.StrictNotEq ->
              case operators.exec_binop(kind, left, right) {
                Ok(result) ->
                  fast_loop(
                    state,
                    pc + 1,
                    [result, ..rest],
                    locals,
                    hp,
                    code,
                    constants,
                    line,
                  )
                // Slow path re-runs the op and throws the same error.
                Error(_err) -> dispatch_slow(state, pc, stack, locals, hp, line)
              }
            // instanceof / in need heap + can run user code — always slow.
            opcode.InstanceOf | opcode.In ->
              dispatch_slow(state, pc, stack, locals, hp, line)
            // Remaining numeric/relational/bitwise/loose-eq ops: pure when
            // neither operand is an object (no ToPrimitive can fire —
            // mirrors step's is_eq_coercible / object checks).
            _other_kind ->
              case left, right {
                JsObject(_), _ | _, JsObject(_) ->
                  dispatch_slow(state, pc, stack, locals, hp, line)
                _, _ ->
                  case operators.exec_binop(kind, left, right) {
                    Ok(result) ->
                      fast_loop(
                        state,
                        pc + 1,
                        [result, ..rest],
                        locals,
                        hp,
                        code,
                        constants,
                        line,
                      )
                    // Slow path re-runs the op and throws the same error.
                    Error(_err) ->
                      dispatch_slow(state, pc, stack, locals, hp, line)
                  }
              }
          }
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    UnaryOp(kind) ->
      case stack {
        // Objects may need ToPrimitive (Neg/Pos/BitNot) — slow path decides.
        [JsObject(_), ..] -> dispatch_slow(state, pc, stack, locals, hp, line)
        [operand, ..rest] ->
          case operators.exec_unaryop(kind, operand) {
            Ok(result) ->
              fast_loop(
                state,
                pc + 1,
                [result, ..rest],
                locals,
                hp,
                code,
                constants,
                line,
              )
            // Slow path re-runs the op and throws the same error.
            Error(_err) -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
        [] -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    // -- Fused superinstructions (resolver peephole) ---------------------
    // Statement-position `i++;` / `i--;` on a numeric local: one locals
    // write, no stack traffic. UnaryOp(Pos) is the identity on every
    // JsNumber, so only the Add/Sub needs to run. Non-numbers (objects,
    // strings, BigInt, TDZ) take the slow path's full coercion chain.
    IncLocal(index) ->
      case tuple_array.unsafe_get(index, locals) {
        value.JsNumber(_) as v ->
          case operators.num_binop(v, number_one, operators.num_add) {
            Ok(result) ->
              fast_loop(
                state,
                pc + 1,
                stack,
                tuple_array.set_unchecked(index, result, locals),
                hp,
                code,
                constants,
                line,
              )
            Error(_err) -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    DecLocal(index) ->
      case tuple_array.unsafe_get(index, locals) {
        value.JsNumber(_) as v ->
          case operators.exec_binop(opcode.Sub, v, number_one) {
            Ok(result) ->
              fast_loop(
                state,
                pc + 1,
                stack,
                tuple_array.set_unchecked(index, result, locals),
                hp,
                code,
                constants,
                line,
              )
            Error(_err) -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    // Fused loop-condition compare-and-branch. Mirrors the BinOp fast
    // path's purity rules: objects (ToPrimitive can run user code) and
    // TDZ sentinels bail to the slow path.
    CmpLocalLocalJump(left_idx, right_idx, kind, target) -> {
      let left = tuple_array.unsafe_get(left_idx, locals)
      let right = tuple_array.unsafe_get(right_idx, locals)
      case left, right {
        JsObject(_), _
        | _, JsObject(_)
        | JsUninitialized, _
        | _, JsUninitialized
        -> dispatch_slow(state, pc, stack, locals, hp, line)
        _, _ ->
          case operators.exec_binop(kind, left, right) {
            Ok(result) ->
              case value.is_truthy(result) {
                True ->
                  fast_loop(
                    state,
                    pc + 1,
                    stack,
                    locals,
                    hp,
                    code,
                    constants,
                    line,
                  )
                False ->
                  fast_loop(
                    state,
                    target,
                    stack,
                    locals,
                    hp,
                    code,
                    constants,
                    line,
                  )
              }
            Error(_err) -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
      }
    }

    CmpLocalConstJump(left_idx, const_index, kind, target) -> {
      let left = tuple_array.unsafe_get(left_idx, locals)
      let right = tuple_array.unsafe_get(const_index, constants)
      case left, right {
        JsObject(_), _
        | _, JsObject(_)
        | JsUninitialized, _
        | _, JsUninitialized
        -> dispatch_slow(state, pc, stack, locals, hp, line)
        _, _ ->
          case operators.exec_binop(kind, left, right) {
            Ok(result) ->
              case value.is_truthy(result) {
                True ->
                  fast_loop(
                    state,
                    pc + 1,
                    stack,
                    locals,
                    hp,
                    code,
                    constants,
                    line,
                  )
                False ->
                  fast_loop(
                    state,
                    target,
                    stack,
                    locals,
                    hp,
                    code,
                    constants,
                    line,
                  )
              }
            Error(_err) -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
      }
    }

    // -- Dense-array computed access -------------------------------------
    // `a[i]` reads on Array/Arguments with an integral number key: pure
    // heap probe, no State materialization. Mirrors to_property_key's
    // number arm + get_value's ArrayObject/ArgumentsObject Index fast path.
    // A dict override at the index (defineProperty accessor/attributes) or
    // a hole (prototype walk) bails to the generic slow path.
    GetElem ->
      case stack {
        [value.JsNumber(value.Finite(f)), JsObject(ref), ..rest] -> {
          // +. 0.0 normalizes -0.0 → +0.0, same as to_property_key.
          let n = f +. 0.0
          let idx = float.truncate(n)
          case int.to_float(idx) == n && idx >= 0 && idx <= 4_294_967_294 {
            False -> dispatch_slow(state, pc, stack, locals, hp, line)
            True ->
              case heap.read(hp, ref) {
                Some(ObjectSlot(
                  kind: ArrayObject(_),
                  properties:,
                  elements: els,
                  ..,
                ))
                | Some(ObjectSlot(
                    kind: value.ArgumentsObject(_),
                    properties:,
                    elements: els,
                    ..,
                  )) ->
                  case dict.has_key(properties, value.Index(idx)) {
                    True -> dispatch_slow(state, pc, stack, locals, hp, line)
                    False ->
                      case elements.get_option(els, idx) {
                        Some(v) ->
                          fast_loop(
                            state,
                            pc + 1,
                            [v, ..rest],
                            locals,
                            hp,
                            code,
                            constants,
                            line,
                          )
                        None ->
                          dispatch_slow(state, pc, stack, locals, hp, line)
                      }
                  }
                _ -> dispatch_slow(state, pc, stack, locals, hp, line)
              }
          }
        }
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    // `a[i] = v` on a plain Array with an integral number key. Two shapes:
    //   - overwrite of a present dense element: an own writable data
    //     property by construction — write elements in place;
    //   - hole/append: allowed when the prototype chain has no property at
    //     the index (a setter or non-writable data prop up the chain would
    //     hijack the store — §10.1.9.2 step 2) and growth passes the
    //     §10.4.2.1 step 2.h extensible + writable-length check.
    // Everything else (dict override, sparse proto, exotic kinds, length
    // overflow, primitives) bails to the generic set_value chain.
    PutElem ->
      case stack {
        [val, value.JsNumber(value.Finite(f)), JsObject(ref), ..rest] -> {
          let n = f +. 0.0
          let idx = float.truncate(n)
          case int.to_float(idx) == n && idx >= 0 && idx <= 4_294_967_294 {
            False -> dispatch_slow(state, pc, stack, locals, hp, line)
            True ->
              case heap.read(hp, ref) {
                Some(
                  ObjectSlot(
                    kind: ArrayObject(length),
                    properties:,
                    elements: els,
                    prototype:,
                    extensible:,
                    ..,
                  ) as slot,
                ) ->
                  case dict.has_key(properties, value.Index(idx)) {
                    True -> dispatch_slow(state, pc, stack, locals, hp, line)
                    False ->
                      case elements.has(els, idx) {
                        True -> {
                          let hp =
                            heap.write(
                              hp,
                              ref,
                              ObjectSlot(
                                ..slot,
                                elements: elements.set(els, idx, val),
                              ),
                            )
                          fast_loop(
                            state,
                            pc + 1,
                            [val, ..rest],
                            locals,
                            hp,
                            code,
                            constants,
                            line,
                          )
                        }
                        False -> {
                          let growable =
                            idx < length
                            || {
                              extensible && array_length_writable(properties)
                            }
                          case
                            growable
                            && proto_chain_index_free(hp, prototype, idx)
                          {
                            False ->
                              dispatch_slow(state, pc, stack, locals, hp, line)
                            True -> {
                              let hp =
                                heap.write(
                                  hp,
                                  ref,
                                  ObjectSlot(
                                    ..slot,
                                    kind: ArrayObject(int.max(length, idx + 1)),
                                    elements: elements.set(els, idx, val),
                                  ),
                                )
                              fast_loop(
                                state,
                                pc + 1,
                                [val, ..rest],
                                locals,
                                hp,
                                code,
                                constants,
                                line,
                              )
                            }
                          }
                        }
                      }
                  }
                _ -> dispatch_slow(state, pc, stack, locals, hp, line)
              }
          }
        }
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    // -- Named own data property on plain objects -------------------------
    // `obj.x` read: own data-property hit on an OrdinaryObject is a pure
    // dict probe. Misses (proto walk), accessors, and every exotic kind
    // (Array length, String indices, Proxy, Namespace, ...) bail.
    GetField(opcode.OpNamed(name)) ->
      case stack {
        [JsObject(ref), ..rest] ->
          case heap.read(hp, ref) {
            Some(ObjectSlot(kind: OrdinaryObject, properties:, ..)) ->
              case dict.get(properties, Named(name)) {
                Ok(DataProperty(value: v, ..)) ->
                  fast_loop(
                    state,
                    pc + 1,
                    [v, ..rest],
                    locals,
                    hp,
                    code,
                    constants,
                    line,
                  )
                _ -> dispatch_slow(state, pc, stack, locals, hp, line)
              }
            _ -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    // `obj.x = v` overwrite of an existing own writable data property on an
    // OrdinaryObject — mirrors set_value's fused-update branch (receiver is
    // the object itself for PutField). The FFI updates the property's value
    // in one map traversal, preserving its flags and creation seq (§10.1.11
    // — the key keeps its enumeration position). Creation (needs the
    // proto-chain setter walk), non-writable, accessors, and exotic kinds
    // bail to the slow path.
    PutField(opcode.OpNamed(name)) ->
      case stack {
        [val, JsObject(ref), ..rest] ->
          case heap.read(hp, ref) {
            Some(ObjectSlot(kind: OrdinaryObject, properties:, ..) as slot) ->
              case put_existing_writable_data(properties, Named(name), val) {
                Ok(new_props) -> {
                  let hp =
                    heap.write(
                      hp,
                      ref,
                      ObjectSlot(..slot, properties: new_props),
                    )
                  fast_loop(
                    state,
                    pc + 1,
                    [val, ..rest],
                    locals,
                    hp,
                    code,
                    constants,
                    line,
                  )
                }
                Error(Nil) -> dispatch_slow(state, pc, stack, locals, hp, line)
              }
            _ -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    // -- Object literal construction ---------------------------------------
    // `{}` allocation: a pure heap alloc with %Object.prototype% — mirrors
    // the step arm exactly (state.builtins is set once per realm at startup
    // and never reassigned, so reading it off the stale State is safe).
    NewObject -> {
      let #(hp, ref) =
        common.alloc_wrapper(
          hp,
          OrdinaryObject,
          state.builtins.object.prototype,
        )
      fast_loop(
        state,
        pc + 1,
        [JsObject(ref), ..stack],
        locals,
        hp,
        code,
        constants,
        line,
      )
    }

    // `{name: v}` static-key property definition — pops the value, keeps the
    // object. OrdinaryObject + extensible is exactly the step arm's
    // define_needs_full_semantics == False region, where a raw
    // object.set_property insert is spec-equivalent (CreateDataProperty on a
    // fresh literal). Proxies and non-extensible receivers bail to step.
    DefineField(opcode.OpNamed(name)) ->
      case stack {
        [val, JsObject(ref) as obj, ..rest] ->
          case heap.read(hp, ref) {
            Some(ObjectSlot(kind: OrdinaryObject, extensible: True, ..)) -> {
              let #(hp, _) = object.set_property(hp, ref, Named(name), val)
              fast_loop(
                state,
                pc + 1,
                [obj, ..rest],
                locals,
                hp,
                code,
                constants,
                line,
              )
            }
            _ -> dispatch_slow(state, pc, stack, locals, hp, line)
          }
        _ -> dispatch_slow(state, pc, stack, locals, hp, line)
      }

    _other_op -> dispatch_slow(state, pc, stack, locals, hp, line)
  }
}

/// Shared 1.0 for the fused IncLocal/DecLocal ops.
const number_one = value.JsNumber(value.Finite(1.0))

/// §10.4.2.4 step 12 precondition for growing an array via an index store:
/// the virtual "length" must still be writable (default True; a
/// defineProperty override in the dict is authoritative). Mirrors
/// object.set_property_on_slot's length_writable probe.
fn array_length_writable(
  properties: dict.Dict(value.PropertyKey, value.Property),
) -> Bool {
  case dict.get(properties, Named("length")) {
    Ok(DataProperty(writable: writable, ..)) -> writable
    _ -> True
  }
}

/// True when no object on the prototype chain has an own property at `idx` —
/// the precondition for a hole/append element store to go straight to the
/// receiver (OrdinarySetWithOwnDescriptor §10.1.9.2 step 2 would otherwise
/// find a setter / non-writable property up the chain). Conservatively bails
/// (False) on any proto kind whose index lookup is not a pure dict/elements
/// probe (Proxy traps, TypedArray indices, String chars, ...).
fn proto_chain_index_free(
  hp: Heap(host),
  proto: Option(Ref),
  idx: Int,
) -> Bool {
  case proto {
    None -> True
    Some(ref) ->
      case heap.read(hp, ref) {
        Some(ObjectSlot(kind: OrdinaryObject, properties:, prototype:, ..))
        | Some(ObjectSlot(
            kind: value.ErrorObject(_),
            properties:,
            prototype:,
            ..,
          )) ->
          case dict.has_key(properties, value.Index(idx)) {
            True -> False
            False -> proto_chain_index_free(hp, prototype, idx)
          }
        Some(ObjectSlot(
          kind: ArrayObject(_),
          properties:,
          elements: els,
          prototype:,
          ..,
        ))
        | Some(ObjectSlot(
            kind: value.ArgumentsObject(_),
            properties:,
            elements: els,
            prototype:,
            ..,
          )) ->
          case
            dict.has_key(properties, value.Index(idx)) || elements.has(els, idx)
          {
            True -> False
            False -> proto_chain_index_free(hp, prototype, idx)
          }
        _ -> False
      }
  }
}

/// Materialize the full State from the fast loop's bare arguments and run one
/// instruction through the general `step` dispatcher.
fn dispatch_slow(
  state: State(host),
  pc: Int,
  stack: List(JsValue),
  locals: tuple_array.TupleArray(JsValue),
  hp: Heap(host),
  line: Int,
) -> Result(#(Completion, State(host)), VmError) {
  let state = State(..state, pc:, stack:, locals:, heap: hp, current_line: line)
  let op = tuple_array.unsafe_get(state.pc, state.code)
  case step(state, op) {
    Ok(new_state) -> execute_inner(new_state)
    Error(#(Done, result, post)) -> Ok(#(NormalCompletion(result), post))
    Error(#(StepVmError(err), _, _)) -> Error(err)
    Error(#(Yielded, yielded_value, post)) -> {
      // Generator yielded — build suspended state.
      // For Yield: pop the yielded value from stack, advance pc.
      // For YieldStar: pop arg (keep iter), DON'T advance pc — resume
      //   re-executes YieldStar with [resume_val, iter, ..].
      // For InitialYield: stack unchanged, just advance pc.
      // Yield/Await handlers don't mutate stack before erroring; pc/stack
      // surgery here is written against pre-step state.
      let heap = post.heap
      let suspended_state = case op {
        Yield ->
          State(
            ..state,
            heap:,
            stack: case state.stack {
              [_, ..rest] -> rest
              [] -> []
            },
            pc: state.pc + 1,
          )
        YieldStar ->
          // Strip arg; also resolve an internal Iterator Record in the iter
          // slot (pre-step stack still has it on the FIRST YieldStar after
          // GetIterator) so gen.return/.throw forwarding off the saved stack
          // sees the real iterator.
          State(..state, heap:, stack: case state.stack {
            [_arg, iter, ..rest] -> [
              unwrap_iterator_record(state, iter),
              ..rest
            ]
            [_arg] | [] -> []
          })
        AsyncYieldStarResume(next_pc:) ->
          // Pre-step stack is [result_obj, iter, ..]. result_obj was fully
          // consumed by step_generators (its .value is yielded_value, .done
          // was false). Drop it so saved stack = [iter, ..]; resume pushes
          // the .next(v) arg → [v, iter, ..] and pc jumps back to Next.
          State(..state, heap:, pc: next_pc, stack: case state.stack {
            [_result_obj, ..rest] -> rest
            [] -> []
          })
        _ -> State(..state, heap:, pc: state.pc + 1)
      }
      Ok(#(YieldCompletion(yielded_value), suspended_state))
    }
    Error(#(Awaited, awaited_value, post)) -> {
      // Async function/generator hit await — pop value, advance pc.
      let heap = post.heap
      let suspended_state =
        State(
          ..state,
          heap:,
          stack: case state.stack {
            [_, ..rest] -> rest
            [] -> []
          },
          pc: state.pc + 1,
        )
      Ok(#(AwaitCompletion(awaited_value), suspended_state))
    }
    Error(#(Thrown, thrown_value, post)) -> {
      // Try to unwind to a catch handler. The handler's full post-step state
      // (stack/pc included) is threaded here so opcodes can mutate stack
      // before throwing (e.g., undef the iter slot then propagate).
      case unwind_to_catch(post, thrown_value) {
        Some(caught_state) -> execute_inner(caught_state)
        None -> Ok(#(ThrowCompletion(thrown_value), post))
      }
    }
  }
}

/// Try to find a catch handler for a thrown value. Walks up call_stack when
/// the current frame's try_stack is exhausted, so throws from a callee can be
/// caught by a try/catch in the caller.
fn unwind_to_catch(
  state: State(host),
  thrown_value: JsValue,
) -> Option(State(host)) {
  case state.try_stack {
    [TryFrame(catch_target:, stack_depth:), ..rest_try] -> {
      let restored_stack = truncate_stack(state.stack, stack_depth)
      Some(
        State(
          ..state,
          stack: [thrown_value, ..restored_stack],
          try_stack: rest_try,
          pc: catch_target,
        ),
      )
    }
    [] ->
      case state.call_stack {
        [] -> None
        [
          SavedFrame(
            func:,
            locals:,
            stack:,
            pc:,
            try_stack:,
            new_target:,
            call_args:,
            eval_env:,
            current_line:,
            ..,
          ),
          ..rest_frames
        ] ->
          unwind_to_catch(
            State(
              ..state,
              func:,
              code: func.bytecode,
              constants: func.constants,
              locals:,
              stack:,
              pc:,
              try_stack:,
              new_target:,
              call_args:,
              eval_env:,
              current_line:,
              call_stack: rest_frames,
              call_depth: state.call_depth - 1,
            ),
            thrown_value,
          )
      }
  }
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  let excess = list.length(stack) - depth
  case excess > 0 {
    True -> list.drop(stack, excess)
    False -> stack
  }
}

fn underflow(
  state: State(host),
  op: String,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  Error(#(StepVmError(StackUnderflow(op)), JsUndefined, state))
}

/// Pop top of stack and jump to `target` if `condition(value)` is true,
/// otherwise advance to next instruction.
fn conditional_jump(
  state: State(host),
  target: Int,
  condition: fn(JsValue) -> Bool,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case state.stack {
    [top, ..rest] ->
      case condition(top) {
        True -> Ok(State(..state, stack: rest, pc: target))
        False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
      }
    [] -> underflow(state, "ConditionalJump")
  }
}

/// Shared body of GetSuperValue / GetSuperValue2: [key, base, this, ..] →
/// ToPropertyKey, then OrdinaryGet on base with receiver=this. When
/// `keep_base` is True the coerced key + base + this stay under the value
/// for the trailing PutSuperValue; the coerced key is written back as a
/// primitive JsValue so PutSuperValue's own to_property_key fast-paths
/// with no observable side effects.
fn get_super_value(
  state: State(host),
  keep_base: Bool,
  op: String,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case state.stack {
    // Symbol keys are valid property keys (§7.1.19 ToPropertyKey step 3) —
    // e.g. super[Symbol.iterator]. Must come before the generic arm whose
    // to_property_key would throw on symbols, and the symbol stays on the
    // stack as-is for the trailing PutSuperValue (no string round-trip).
    [value.JsSymbol(sym) as key, JsObject(base_ref), this_val, ..rest] -> {
      use #(val, state) <- result.map(
        state.rethrow(object.get_symbol_value(state, base_ref, sym, this_val)),
      )
      let stack = case keep_base {
        True -> [val, key, JsObject(base_ref), this_val, ..rest]
        False -> [val, ..rest]
      }
      State(..state, stack:, pc: state.pc + 1)
    }
    [key, JsObject(base_ref), this_val, ..rest] -> {
      use #(pk, state) <- result.try(
        state.rethrow(property.to_property_key(state, key)),
      )
      use #(val, state) <- result.map(
        state.rethrow(object.get_value(state, base_ref, pk, this_val)),
      )
      let stack = case keep_base {
        True -> [
          val,
          JsString(value.key_to_string(pk)),
          JsObject(base_ref),
          this_val,
          ..rest
        ]
        False -> [val, ..rest]
      }
      State(..state, stack:, pc: state.pc + 1)
    }
    // §12.3.5.3 step 5 RequireObjectCoercible — base is null when
    // home_object's prototype is null (e.g. `class C extends null`).
    [_key, _base, _this, ..] ->
      state.throw_type_error(
        state,
        "Cannot read super property when prototype is null",
      )
    _ -> underflow(state, op)
  }
}

// ============================================================================
// Step — single instruction dispatch
// ============================================================================

/// Execute a single instruction. Returns Ok(new_state) to continue,
/// or Error(#(signal, value, heap)) to stop.
fn step(
  state: State(host),
  op: Op,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case op {
    // ---- Source mapping ----------------------------------------------
    SetLine(line) -> Ok(State(..state, current_line: line, pc: state.pc + 1))

    // ---- Stack operations --------------------------------------------
    PushConst(index) -> {
      let value = tuple_array.unsafe_get(index, state.constants)
      Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
    }

    Pop -> {
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "Pop")
      }
    }

    Dup -> {
      case state.stack {
        [top, ..] ->
          Ok(State(..state, stack: [top, ..state.stack], pc: state.pc + 1))
        [] -> underflow(state, "Dup")
      }
    }

    Swap -> {
      case state.stack {
        [a, b, ..rest] ->
          Ok(State(..state, stack: [b, a, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Swap")
      }
    }

    // [a, b, c, ..] → [c, a, b, ..] — bring the 3rd element to the top.
    Rot3 -> {
      case state.stack {
        [a, b, c, ..rest] ->
          Ok(State(..state, stack: [c, a, b, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Rot3")
      }
    }

    // [a, b, c, d, ..] → [b, c, d, a, ..] — bury the top under the next three.
    Unrot4 -> {
      case state.stack {
        [a, b, c, d, ..rest] ->
          Ok(State(..state, stack: [b, c, d, a, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Unrot4")
      }
    }

    // ---- Local variable access ---------------------------------------
    GetLocal(index) -> {
      case tuple_array.unsafe_get(index, state.locals) {
        JsUninitialized ->
          state.throw_reference_error(
            state,
            "Cannot access variable before initialization (TDZ)",
          )
        value ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
      }
    }

    PutLocal(index) -> {
      case state.stack {
        [value, ..rest] -> {
          let locals = tuple_array.set_unchecked(index, value, state.locals)
          Ok(State(..state, stack: rest, locals:, pc: state.pc + 1))
        }
        [] -> underflow(state, "PutLocal")
      }
    }

    // §9.1.1.3.1 BindThisValue — derived ctor `this` may be bound exactly once.
    PutLocalCheckInit(index) ->
      case state.stack {
        [value, ..rest] ->
          case tuple_array.unsafe_get(index, state.locals) {
            JsUninitialized -> {
              let locals = tuple_array.set_unchecked(index, value, state.locals)
              Ok(State(..state, stack: rest, locals:, pc: state.pc + 1))
            }
            _ ->
              state.throw_reference_error(
                state,
                "'this' is already initialized",
              )
          }
        [] -> underflow(state, "PutLocalCheckInit")
      }

    BoxLocal(index) -> {
      let current_value = tuple_array.unsafe_get(index, state.locals)
      let #(heap, box_ref) =
        heap.alloc(state.heap, value.BoxSlot(current_value))
      let locals =
        tuple_array.set_unchecked(index, JsObject(box_ref), state.locals)
      Ok(State(..state, heap:, locals:, pc: state.pc + 1))
    }

    GetBoxed(index) -> {
      case tuple_array.unsafe_get(index, state.locals) {
        JsObject(box_ref) ->
          case heap.read_box(state.heap, box_ref) {
            Some(JsUninitialized) ->
              state.throw_reference_error(
                state,
                "Cannot access variable before initialization (TDZ)",
              )
            Some(val) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            None ->
              Error(#(
                StepVmError(Unimplemented("GetBoxed: not a BoxSlot")),
                JsUndefined,
                state,
              ))
          }
        _ ->
          Error(#(
            StepVmError(Unimplemented("GetBoxed: local is not a box ref")),
            JsUndefined,
            state,
          ))
      }
    }

    PutBoxed(index) -> {
      case state.stack {
        [new_value, ..rest_stack] -> {
          case tuple_array.unsafe_get(index, state.locals) {
            JsObject(box_ref) -> {
              let heap =
                heap.write(state.heap, box_ref, value.BoxSlot(new_value))
              Ok(State(..state, heap:, stack: rest_stack, pc: state.pc + 1))
            }
            _ ->
              Error(#(
                StepVmError(Unimplemented("PutBoxed: local is not a box ref")),
                JsUndefined,
                state,
              ))
          }
        }
        [] -> underflow(state, "PutBoxed")
      }
    }

    // BindThisValue when `this` is captured by an arrow inside the ctor.
    PutBoxedCheckInit(index) ->
      case state.stack {
        [new_value, ..rest_stack] ->
          case tuple_array.unsafe_get(index, state.locals) {
            JsObject(box_ref) ->
              case heap.read_box(state.heap, box_ref) {
                Some(JsUninitialized) -> {
                  let heap =
                    heap.write(state.heap, box_ref, value.BoxSlot(new_value))
                  Ok(State(..state, heap:, stack: rest_stack, pc: state.pc + 1))
                }
                _ ->
                  state.throw_reference_error(
                    state,
                    "'this' is already initialized",
                  )
              }
            other ->
              Error(#(
                StepVmError(Unimplemented(
                  "PutBoxedCheckInit: local is not a box ref: "
                  <> string.inspect(other),
                )),
                JsUndefined,
                state,
              ))
          }
        [] -> underflow(state, "PutBoxedCheckInit")
      }

    // ---- Global variable access --------------------------------------
    // §9.1.1.4.4 GetBindingValue — two-phase: declarative then object record
    GetGlobal(name) -> {
      case dict.get(state.ctx.lexical_globals, name) {
        // Lexical binding exists — check for TDZ
        Ok(value.Let(JsUninitialized)) | Ok(value.Const(JsUninitialized)) ->
          state.throw_reference_error(
            state,
            "Cannot access '" <> name <> "' before initialization",
          )
        Ok(binding) -> {
          let val = value.lexical_global_value(binding)
          Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
        }
        // Not in lexical → try object record (globalThis)
        Error(_) -> {
          let key = Named(name)
          case
            object.get_own_property(state.heap, state.ctx.global_object, key)
          {
            Some(DataProperty(value: val, ..)) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            Some(value.AccessorProperty(get: Some(getter), ..)) ->
              case
                state.call(state, getter, JsObject(state.ctx.global_object), [])
              {
                Ok(#(val, state)) ->
                  Ok(
                    State(
                      ..state,
                      stack: [val, ..state.stack],
                      pc: state.pc + 1,
                    ),
                  )
                Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
              }
            Some(value.AccessorProperty(get: None, ..)) ->
              Ok(
                State(
                  ..state,
                  stack: [JsUndefined, ..state.stack],
                  pc: state.pc + 1,
                ),
              )
            None ->
              // Check prototype chain — stateful: §9.1.1.4.1 HasBinding goes
              // through [[HasProperty]], so a Proxy on the global's prototype
              // chain must have its "has" trap run (it can execute user code).
              case
                object.has_property_stateful(
                  state,
                  state.ctx.global_object,
                  object.PkString(key),
                )
              {
                Ok(#(True, state)) ->
                  case
                    object.get_value_of(
                      state,
                      JsObject(state.ctx.global_object),
                      key,
                    )
                  {
                    Ok(#(val, state)) ->
                      Ok(
                        State(
                          ..state,
                          stack: [val, ..state.stack],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
                  }
                Ok(#(False, state)) ->
                  state.throw_reference_error(state, name <> " is not defined")
                Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
              }
          }
        }
      }
    }

    // §9.1.1.4.5 SetMutableBinding — two-phase: declarative then object record
    PutGlobal(name) -> {
      case state.stack {
        [val, ..rest] -> {
          case dict.get(state.ctx.lexical_globals, name) {
            // const → assignment rejected (even in TDZ, per spec ordering)
            Ok(value.Const(_)) ->
              state.throw_type_error(state, "Assignment to constant variable.")
            // let in TDZ → reference error
            Ok(value.Let(JsUninitialized)) ->
              state.throw_reference_error(
                state,
                "Cannot access '" <> name <> "' before initialization",
              )
            // initialized let → rebind
            Ok(value.Let(_)) ->
              Ok(
                State(
                  ..state,
                  stack: rest,
                  ctx: state.RealmCtx(
                    ..state.ctx,
                    lexical_globals: dict.insert(
                      state.ctx.lexical_globals,
                      name,
                      value.Let(val),
                    ),
                  ),
                  pc: state.pc + 1,
                ),
              )
            // Not in lexical → object record path
            Error(_) -> {
              let key = Named(name)
              case state.func.is_strict {
                True ->
                  // Strict mode: must exist on globalThis or throw. Stateful
                  // — a Proxy on the global's prototype chain runs its "has"
                  // trap here (§9.1.1.4.5 SetMutableBinding → HasBinding).
                  case
                    object.has_property_stateful(
                      state,
                      state.ctx.global_object,
                      object.PkString(key),
                    )
                  {
                    Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
                    Ok(#(False, state)) ->
                      state.throw_reference_error(
                        state,
                        name <> " is not defined",
                      )
                    Ok(#(True, state)) ->
                      case
                        object.set_value(
                          State(..state, stack: rest),
                          state.ctx.global_object,
                          key,
                          val,
                          JsObject(state.ctx.global_object),
                        )
                      {
                        Ok(#(state, True)) ->
                          Ok(State(..state, pc: state.pc + 1))
                        Ok(#(state, False)) ->
                          state.throw_type_error(
                            state,
                            "Cannot assign to read only property '"
                              <> name
                              <> "' of object '#<Object>'",
                          )
                        Error(#(thrown, state)) ->
                          Error(#(Thrown, thrown, state))
                      }
                  }
                False ->
                  // Sloppy mode: set on globalThis (creates if needed,
                  // returns False for non-writable → silently ignore)
                  case
                    object.set_value(
                      State(..state, stack: rest),
                      state.ctx.global_object,
                      key,
                      val,
                      JsObject(state.ctx.global_object),
                    )
                  {
                    Ok(#(state, _)) -> Ok(State(..state, pc: state.pc + 1))
                    Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
                  }
              }
            }
          }
        }
        [] -> underflow(state, "PutGlobal")
      }
    }

    // §9.1.1.4.17 CreateGlobalVarBinding — create var on globalThis
    DeclareGlobalVar(name) -> {
      let key = Named(name)
      // §9.1.1.4.17 CreateGlobalVarBinding: hasProperty = ? HasOwnProperty(
      // globalObject, N) — own properties only, NOT the prototype chain.
      // A name that only exists as an inherited accessor (e.g. `__proto__`
      // from Object.prototype) must still get an own data binding, so the
      // later PutGlobal writes the binding instead of invoking the setter
      // (test262: language/expressions/object/__proto__-permitted-dup-
      // shorthand.js via eval'd `var __proto__`).
      let has_own =
        object.get_own_property(state.heap, state.ctx.global_object, key)
        |> option.is_some
      case has_own {
        True ->
          // Already exists — no-op
          Ok(State(..state, pc: state.pc + 1))
        False -> {
          let #(heap, _) =
            object.set_property(
              state.heap,
              state.ctx.global_object,
              key,
              JsUndefined,
            )
          Ok(State(..state, heap:, pc: state.pc + 1))
        }
      }
    }

    // Sloppy direct-eval var access: check eval_env dict, fall through to globals.
    GetEvalVar(name) -> {
      case lookup_eval_env(state, name) {
        Some(v) ->
          Ok(State(..state, stack: [v, ..state.stack], pc: state.pc + 1))
        None -> step(state, GetGlobal(name))
      }
    }

    // typeof on a name that might live in eval_env.
    TypeofEvalVar(name) -> {
      case lookup_eval_env(state, name) {
        Some(v) ->
          Ok(
            State(
              ..state,
              stack: [
                JsString(common.typeof_value(v, state.heap)),
                ..state.stack
              ],
              pc: state.pc + 1,
            ),
          )
        None -> step(state, TypeofGlobal(name))
      }
    }

    // Sloppy direct-eval var write: update eval_env if key exists, else PutGlobal.
    PutEvalVar(name) -> {
      case state.eval_env, state.stack {
        Some(ref), [v, ..rest] -> {
          let vars =
            heap.read_eval_env(state.heap, ref) |> option.unwrap(dict.new())
          case dict.has_key(vars, name) {
            False -> step(state, PutGlobal(name))
            True -> {
              let heap =
                heap.write(
                  state.heap,
                  ref,
                  EvalEnvSlot(dict.insert(vars, name, v)),
                )
              Ok(State(..state, heap:, stack: rest, pc: state.pc + 1))
            }
          }
        }
        _, _ -> step(state, PutGlobal(name))
      }
    }

    // Sloppy direct-eval var declaration: seed name=undefined into eval_env.
    DeclareEvalVar(name) -> {
      case state.eval_env {
        None -> step(state, DeclareGlobalVar(name))
        Some(ref) -> {
          let vars =
            heap.read_eval_env(state.heap, ref) |> option.unwrap(dict.new())
          let heap = case dict.has_key(vars, name) {
            True -> state.heap
            False ->
              heap.write(
                state.heap,
                ref,
                EvalEnvSlot(dict.insert(vars, name, JsUndefined)),
              )
          }
          Ok(State(..state, heap:, pc: state.pc + 1))
        }
      }
    }

    // §7.1.17 ToString for template literal substitutions: ToPrimitive with
    // string hint (toString before valueOf), then primitive → string.
    opcode.ToStringVal -> {
      case state.stack {
        [JsString(_), ..] -> Ok(State(..state, pc: state.pc + 1))
        [val, ..rest] -> {
          use #(s, state) <- result.try(
            state.rethrow(coerce.js_to_string(state, val)),
          )
          Ok(State(..state, stack: [JsString(s), ..rest], pc: state.pc + 1))
        }
        [] -> underflow(state, "ToStringVal")
      }
    }

    // §13.2.8.4 GetTemplateObject — push the per-site cached template object
    // for a tagged template, creating it on first evaluation. The template
    // object and its .raw array are frozen arrays whose index properties are
    // { writable: false, enumerable: true, configurable: false }; both are
    // stored as dict overrides so freeze semantics (writes reject, deletes
    // reject, isFrozen reports true) hold.
    opcode.GetTemplateObject(site, cooked, raw) -> {
      case dict.get(state.ctx.template_objects, site) {
        Ok(ref) ->
          Ok(
            State(
              ..state,
              stack: [JsObject(ref), ..state.stack],
              pc: state.pc + 1,
            ),
          )
        Error(Nil) -> {
          let count = list.length(raw)
          // Step 10: raw array — frozen Array of the verbatim quasi strings.
          let #(heap, raw_ref) =
            heap.alloc(
              state.heap,
              frozen_array_slot(
                list.map(raw, JsString),
                count,
                [],
                state.builtins.array.prototype,
              ),
            )
          // Steps 6-9, 11-13: template array — frozen Array of the cooked
          // values (undefined for invalid escapes), with a non-enumerable,
          // non-writable, non-configurable "raw" property.
          let cooked_values =
            list.map(cooked, fn(c) {
              case c {
                Some(s) -> JsString(s)
                None -> JsUndefined
              }
            })
          let raw_prop = #(
            Named("raw"),
            DataProperty(
              value: JsObject(raw_ref),
              writable: False,
              enumerable: False,
              configurable: False,
              seq: value.next_prop_seq(),
            ),
          )
          let #(heap, tpl_ref) =
            heap.alloc(
              heap,
              frozen_array_slot(
                cooked_values,
                count,
                [raw_prop],
                state.builtins.array.prototype,
              ),
            )
          // Root the template object so GC keeps it alive for the cache's
          // lifetime (the raw array is reachable through its "raw" property).
          let heap = heap.root(heap, tpl_ref)
          Ok(
            State(
              ..state,
              heap:,
              ctx: state.RealmCtx(
                ..state.ctx,
                template_objects: dict.insert(
                  state.ctx.template_objects,
                  site,
                  tpl_ref,
                ),
              ),
              stack: [JsObject(tpl_ref), ..state.stack],
              pc: state.pc + 1,
            ),
          )
        }
      }
    }

    // §7.1.19 ToPropertyKey — class-definition-time coercion of computed
    // field names (§15.7.14 ClassFieldDefinitionEvaluation step 1).
    // ToPrimitive(key, string): Symbols pass through unchanged, everything
    // else is ToString'd. Abrupt completions (throwing @@toPrimitive /
    // toString / valueOf) propagate at class-evaluation time.
    opcode.ToPropertyKey -> {
      case state.stack {
        [JsString(_), ..] | [value.JsSymbol(_), ..] ->
          Ok(State(..state, pc: state.pc + 1))
        [val, ..rest] -> {
          use #(prim, state) <- result.try(
            state.rethrow(coerce.to_primitive(state, val, coerce.StringHint)),
          )
          case prim {
            value.JsSymbol(_) ->
              Ok(State(..state, stack: [prim, ..rest], pc: state.pc + 1))
            _ -> {
              use #(s, state) <- result.try(
                state.rethrow(coerce.js_to_string(state, prim)),
              )
              Ok(State(..state, stack: [JsString(s), ..rest], pc: state.pc + 1))
            }
          }
        }
        [] -> underflow(state, "ToPropertyKey")
      }
    }

    // §7.1.18 ToObject for the `with (expr)` head.
    opcode.ToObject -> {
      case state.stack {
        [val, ..rest] ->
          case common.to_object(state.heap, state.builtins, val) {
            Some(#(heap, ref)) ->
              Ok(
                State(
                  ..state,
                  heap:,
                  stack: [JsObject(ref), ..rest],
                  pc: state.pc + 1,
                ),
              )
            None ->
              state.throw_type_error(
                state,
                "Cannot convert undefined or null to object",
              )
          }
        [] -> underflow(state, "ToObject")
      }
    }

    // §9.1.1.2.1 HasBinding + §9.1.1.2.6 GetBindingValue against a with
    // object. Found: replace obj with the value and jump. Not found
    // (or @@unscopables-blocked): pop obj, fall through.
    opcode.WithGetVar(name, target) -> {
      case state.stack {
        [JsObject(ref) as obj, ..rest] -> {
          use #(bound, state) <- result.try(with_has_binding(state, ref, name))
          case bound {
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            True -> {
              // GetBindingValue re-checks HasProperty: the @@unscopables
              // getter may have deleted the property. Strict referencing
              // code (a strict closure resolving through a sloppy with)
              // throws ReferenceError; sloppy reads undefined.
              use #(still, state) <- result.try(
                state.rethrow(object.has_property_stateful(
                  state,
                  ref,
                  object.PkString(Named(name)),
                )),
              )
              case still {
                False ->
                  case state.func.is_strict {
                    True ->
                      state.throw_reference_error(
                        state,
                        name <> " is not defined",
                      )
                    False ->
                      Ok(
                        State(..state, stack: [JsUndefined, ..rest], pc: target),
                      )
                  }
                True -> {
                  use #(val, state) <- result.map(
                    state.rethrow(object.get_value_of(state, obj, Named(name))),
                  )
                  State(..state, stack: [val, ..rest], pc: target)
                }
              }
            }
          }
        }
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "WithGetVar")
      }
    }

    // Like WithGetVar, but keeps the with object beneath the value as the
    // call receiver (§13.3.6.2 EvaluateCall step 1.b.ii — thisValue is the
    // env record's WithBaseObject). Found: [obj, ..] → [value, obj, ..],
    // jump. Not found (or @@unscopables-blocked): pop obj, fall through to
    // the static path (which pushes undefined as receiver).
    opcode.WithGetVarThis(name, target) -> {
      case state.stack {
        [JsObject(ref) as obj, ..rest] -> {
          use #(bound, state) <- result.try(with_has_binding(state, ref, name))
          case bound {
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            True -> {
              // Same HasProperty re-check as WithGetVar: the @@unscopables
              // getter may have deleted the property.
              use #(still, state) <- result.try(
                state.rethrow(object.has_property_stateful(
                  state,
                  ref,
                  object.PkString(Named(name)),
                )),
              )
              case still {
                False ->
                  case state.func.is_strict {
                    True ->
                      state.throw_reference_error(
                        state,
                        name <> " is not defined",
                      )
                    False ->
                      Ok(
                        State(
                          ..state,
                          stack: [JsUndefined, obj, ..rest],
                          pc: target,
                        ),
                      )
                  }
                True -> {
                  use #(val, state) <- result.map(
                    state.rethrow(object.get_value_of(state, obj, Named(name))),
                  )
                  State(..state, stack: [val, obj, ..rest], pc: target)
                }
              }
            }
          }
        }
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "WithGetVarThis")
      }
    }

    // §9.1.1.2.5 SetMutableBinding against a with object. Stack:
    // [obj, value, ..]. Found: Set(obj, name, value), pop both, jump.
    // Not found: pop obj, fall through to the ordinary store.
    opcode.WithPutVar(name, target) -> {
      case state.stack {
        [JsObject(ref) as obj, val, ..rest] -> {
          use #(bound, state) <- result.try(with_has_binding(state, ref, name))
          case bound {
            False -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            True -> {
              // SetMutableBinding step 2: re-check HasProperty — the
              // @@unscopables getter may have deleted the binding. If gone
              // and the assigning code is strict (a strict closure can
              // resolve through an enclosing sloppy with), ReferenceError.
              use #(still, state) <- result.try(
                state.rethrow(object.has_property_stateful(
                  state,
                  ref,
                  object.PkString(Named(name)),
                )),
              )
              use Nil <- result.try(case still, state.func.is_strict {
                False, True ->
                  state.throw_reference_error(state, name <> " is not defined")
                _, _ -> Ok(Nil)
              })
              use #(state, ok) <- result.try(
                state.rethrow(object.set_value(
                  state,
                  ref,
                  Named(name),
                  val,
                  obj,
                )),
              )
              // §13.15.2 PutValue 6.b.iv: failed [[Set]] throws TypeError in
              // strict code only.
              case ok, state.func.is_strict {
                False, True ->
                  state.throw_type_error(
                    state,
                    "Cannot assign to read only property '"
                      <> name
                      <> "' of object",
                  )
                _, _ -> Ok(State(..state, stack: rest, pc: target))
              }
            }
          }
        }
        [_, val, ..rest] ->
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "WithPutVar")
      }
    }

    // §9.1.1.2.7 DeleteBinding against a with object. Found: replace obj
    // with the [[Delete]] result and jump. Not found: pop obj, fall through.
    opcode.WithDeleteVar(name, target) -> {
      case state.stack {
        [JsObject(ref), ..rest] -> {
          use #(bound, state) <- result.try(with_has_binding(state, ref, name))
          case bound {
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            True -> {
              use #(state, success) <- result.map(
                state.rethrow(object.delete_property_stateful(
                  state,
                  ref,
                  object.PkString(Named(name)),
                )),
              )
              State(..state, stack: [JsBool(success), ..rest], pc: target)
            }
          }
        }
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "WithDeleteVar")
      }
    }

    // §9.1.2.1 GetIdentifierReference at a with object — HasBinding only.
    // Bound: KEEP obj (it becomes the reference base) and jump. Not bound:
    // pop obj, fall through (next check or the undefined static sentinel).
    opcode.WithMakeRef(name, target) -> {
      case state.stack {
        [JsObject(ref) as obj, ..rest] -> {
          use #(bound, state) <- result.map(with_has_binding(state, ref, name))
          case bound {
            // Keep obj on the stack — it is the reference base.
            True -> State(..state, stack: [obj, ..rest], pc: target)
            False -> State(..state, stack: rest, pc: state.pc + 1)
          }
        }
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "WithMakeRef")
      }
    }

    // §9.1.1.2.6 GetBindingValue on a made reference base. Object base:
    // HasProperty re-check (binding may have vanished) then Get; undefined
    // sentinel: pop, fall through to the static read.
    opcode.WithGetRefValue(name, target) -> {
      case state.stack {
        [JsObject(ref) as obj, ..rest] -> {
          use #(still, state) <- result.try(
            state.rethrow(object.has_property_stateful(
              state,
              ref,
              object.PkString(Named(name)),
            )),
          )
          case still, state.func.is_strict {
            False, True ->
              state.throw_reference_error(state, name <> " is not defined")
            False, False ->
              Ok(State(..state, stack: [JsUndefined, ..rest], pc: target))
            True, _ -> {
              use #(val, state) <- result.map(
                state.rethrow(object.get_value_of(state, obj, Named(name))),
              )
              State(..state, stack: [val, ..rest], pc: target)
            }
          }
        }
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "WithGetRefValue")
      }
    }

    // §9.1.1.2.5 SetMutableBinding on a made reference base. Stack:
    // [base, value, ..]. Object base: stillExists re-check then Set (the
    // ORIGINAL base — even if the binding was deleted during the RHS, the
    // store recreates it there, §13.15.2 note). Undefined sentinel: pop,
    // fall through to the static store.
    opcode.WithPutRefValue(name, target) -> {
      case state.stack {
        [JsObject(ref) as obj, val, ..rest] -> {
          use #(still, state) <- result.try(
            state.rethrow(object.has_property_stateful(
              state,
              ref,
              object.PkString(Named(name)),
            )),
          )
          use Nil <- result.try(case still, state.func.is_strict {
            False, True ->
              state.throw_reference_error(state, name <> " is not defined")
            _, _ -> Ok(Nil)
          })
          use #(state, ok) <- result.try(
            state.rethrow(object.set_value(state, ref, Named(name), val, obj)),
          )
          case ok, state.func.is_strict {
            False, True ->
              state.throw_type_error(
                state,
                "Cannot assign to read only property '" <> name <> "' of object",
              )
            _, _ -> Ok(State(..state, stack: rest, pc: target))
          }
        }
        [_, val, ..rest] ->
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "WithPutRefValue")
      }
    }

    // §9.1.1.4.16 CreateGlobalLexBinding — create let/const in lexical record
    DeclareGlobalLex(name, is_const) -> {
      // TDZ slot, tagged const/let so PutGlobal and InitGlobalLex can tell them apart.
      let binding = case is_const {
        True -> value.Const(JsUninitialized)
        False -> value.Let(JsUninitialized)
      }
      Ok(
        State(
          ..state,
          ctx: state.RealmCtx(
            ..state.ctx,
            lexical_globals: dict.insert(
              state.ctx.lexical_globals,
              name,
              binding,
            ),
          ),
          pc: state.pc + 1,
        ),
      )
    }

    // Initialize a lexical global (TDZ → value), preserving its const/let tag.
    InitGlobalLex(name) -> {
      case state.stack {
        [val, ..rest] -> {
          let binding = case dict.get(state.ctx.lexical_globals, name) {
            Ok(existing) -> value.set_lexical_global_value(existing, val)
            // No prior DeclareGlobalLex — default to let.
            Error(Nil) -> value.Let(val)
          }
          Ok(
            State(
              ..state,
              stack: rest,
              ctx: state.RealmCtx(
                ..state.ctx,
                lexical_globals: dict.insert(
                  state.ctx.lexical_globals,
                  name,
                  binding,
                ),
              ),
              pc: state.pc + 1,
            ),
          )
        }
        [] -> underflow(state, "InitGlobalLex")
      }
    }

    // `using` / `await using` desugar: CreateDisposableResource(V, hint) —
    // pop the resource value, push its disposer callable (or undefined for
    // null/undefined). TypeError for non-disposable values.
    opcode.GetDisposer(is_async:) -> {
      case state.stack {
        [val, ..rest] -> {
          let state = State(..state, stack: rest, pc: state.pc + 1)
          case disposable_stack.make_using_disposer(state, val, is_async:) {
            #(state, Ok(disposer)) ->
              Ok(State(..state, stack: [disposer, ..state.stack]))
            #(state, Error(thrown)) -> Error(#(Thrown, thrown, state))
          }
        }
        [] -> underflow(state, "GetDisposer")
      }
    }

    // `using` / `await using` desugar: DisposeResources error folding — pop
    // suppressed, pop error, push new SuppressedError(error, suppressed).
    opcode.MakeSuppressed -> {
      case state.stack {
        [suppressed, err, ..rest] -> {
          let #(state, suppressed_error) =
            builtins_error.make_suppressed_error(state, err, suppressed)
          Ok(
            State(..state, stack: [suppressed_error, ..rest], pc: state.pc + 1),
          )
        }
        _ -> underflow(state, "MakeSuppressed")
      }
    }

    TypeOf -> {
      case state.stack {
        [val, ..rest] -> {
          Ok(
            State(
              ..state,
              stack: [JsString(common.typeof_value(val, state.heap)), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        [] -> underflow(state, "TypeOf")
      }
    }

    // §9.1.1.4: typeof on globals — TDZ throws, undeclared returns "undefined"
    TypeofGlobal(name) -> {
      case dict.get(state.ctx.lexical_globals, name) {
        // TDZ — typeof on uninitialized lexical still throws per spec
        Ok(value.Let(JsUninitialized)) | Ok(value.Const(JsUninitialized)) ->
          state.throw_reference_error(
            state,
            "Cannot access '" <> name <> "' before initialization",
          )
        Ok(binding) -> {
          let val = value.lexical_global_value(binding)
          Ok(
            State(
              ..state,
              stack: [
                JsString(common.typeof_value(val, state.heap)),
                ..state.stack
              ],
              pc: state.pc + 1,
            ),
          )
        }
        Error(_) -> {
          // Object record: try globalThis, return "undefined" if not found
          let key = Named(name)
          let val = case
            object.get_own_property(state.heap, state.ctx.global_object, key)
          {
            Some(DataProperty(value: v, ..)) -> v
            _ ->
              case
                object.has_property(state.heap, state.ctx.global_object, key)
              {
                True ->
                  // Property exists on proto chain — use get_value_of for correct result
                  case
                    object.get_value_of(
                      state,
                      JsObject(state.ctx.global_object),
                      key,
                    )
                  {
                    Ok(#(v, _)) -> v
                    Error(_) -> JsUndefined
                  }
                False -> JsUndefined
              }
          }
          Ok(
            State(
              ..state,
              stack: [
                JsString(common.typeof_value(val, state.heap)),
                ..state.stack
              ],
              pc: state.pc + 1,
            ),
          )
        }
      }
    }

    // ---- Operators ---------------------------------------------------
    BinOp(kind) -> {
      case state.stack {
        [right, left, ..rest] -> {
          // instanceof and in need heap access
          case kind {
            opcode.InstanceOf -> {
              use #(result, state) <- result.map(
                state.rethrow(coerce.js_instanceof(state, left, right)),
              )
              State(..state, stack: [JsBool(result), ..rest], pc: state.pc + 1)
            }
            opcode.In -> {
              // left = key, right = object
              case right {
                JsObject(ref) -> {
                  use #(result, state) <- result.map(case left {
                    value.JsSymbol(sym) ->
                      state.rethrow(object.has_property_stateful(
                        state,
                        ref,
                        object.PkSymbol(sym),
                      ))
                    _ ->
                      case property.to_property_key(state, left) {
                        Ok(#(pk, state)) ->
                          state.rethrow(object.has_property_stateful(
                            state,
                            ref,
                            object.PkString(pk),
                          ))
                        Error(#(thrown, state)) ->
                          Error(#(Thrown, thrown, state))
                      }
                  })
                  State(
                    ..state,
                    stack: [JsBool(result), ..rest],
                    pc: state.pc + 1,
                  )
                }
                _ ->
                  state.throw_type_error(
                    state,
                    "Cannot use 'in' operator to search for '"
                      <> object.inspect(left, state.heap)
                      <> "' in "
                      <> object.inspect(right, state.heap),
                  )
              }
            }
            // Add needs ToPrimitive for object operands (ES2024 §13.15.3)
            Add ->
              case left, right {
                JsObject(_), _ | _, JsObject(_) ->
                  binop_add_with_to_primitive(state, left, right, rest)
                _, _ -> add_primitives(state, left, right, rest)
              }
            // Strict equality compares object references — never coerce.
            opcode.StrictEq | opcode.StrictNotEq ->
              binop_direct(state, kind, left, right, rest)
            // Loose equality: §7.2.14 step 12 only ToPrimitives the object
            // side when the other is Number/String/BigInt/Symbol. Bool is
            // first ToNumber'd (step 10) so it ends up here too. For
            // object×object (reference equality) and object×nullish (always
            // false) we stay on the direct path.
            opcode.Eq | opcode.NotEq ->
              case is_eq_coercible(left, right) {
                True -> binop_with_to_primitive(state, kind, left, right, rest)
                False -> binop_direct(state, kind, left, right, rest)
              }
            // All remaining ops are numeric/relational/bitwise: ToNumeric →
            // ToPrimitive(number) on both operands (§13.15.4).
            _ ->
              case left, right {
                JsObject(_), _ | _, JsObject(_) ->
                  binop_with_to_primitive(state, kind, left, right, rest)
                _, _ -> binop_direct(state, kind, left, right, rest)
              }
          }
        }
        _ -> underflow(state, "BinOp")
      }
    }

    UnaryOp(kind) -> {
      case state.stack {
        [operand, ..rest] ->
          case operand, kind {
            JsObject(_), opcode.Neg
            | JsObject(_), opcode.Pos
            | JsObject(_), opcode.BitNot
            -> unaryop_with_to_primitive(state, kind, operand, rest)
            _, _ ->
              case operators.exec_unaryop(kind, operand) {
                Ok(result) ->
                  Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
                Error(err) -> throw_operator_error(state, err)
              }
          }
        [] -> underflow(state, "UnaryOp")
      }
    }

    // ---- Fused superinstructions (resolver peephole) -------------------
    IncLocal(index) -> fused_update_local(state, index, Add)
    DecLocal(index) -> fused_update_local(state, index, opcode.Sub)

    CmpLocalLocalJump(left_idx, right_idx, kind, target) ->
      case tuple_array.unsafe_get(left_idx, state.locals) {
        JsUninitialized -> tdz_reference_error(state)
        left ->
          case tuple_array.unsafe_get(right_idx, state.locals) {
            JsUninitialized -> tdz_reference_error(state)
            right -> fused_cmp_jump(state, kind, left, right, target)
          }
      }

    CmpLocalConstJump(left_idx, const_index, kind, target) ->
      case tuple_array.unsafe_get(left_idx, state.locals) {
        JsUninitialized -> tdz_reference_error(state)
        left ->
          fused_cmp_jump(
            state,
            kind,
            left,
            tuple_array.unsafe_get(const_index, state.constants),
            target,
          )
      }

    // ---- Control flow ------------------------------------------------
    Return -> {
      let return_value = case state.stack {
        [value, ..] -> value
        [] -> JsUndefined
      }
      case state.call_stack {
        // No caller — top-level return, we're done
        [] -> Error(#(Done, return_value, state))
        // Pop call frame, restore caller, push return value onto caller's stack
        [
          SavedFrame(
            func:,
            locals:,
            stack:,
            pc:,
            try_stack:,
            constructor_this:,
            new_target: saved_new_target,
            call_args: saved_call_args,
            eval_env: saved_eval_env,
            current_line: saved_current_line,
          ),
          ..rest_frames
        ] -> {
          // Restore the caller's source line before any reconstruction below
          // (they all spread `..state`), so a throw in the remainder of the
          // caller's statement reports the caller's line, not the callee's.
          let state = State(..state, current_line: saved_current_line)
          // Constructor return semantics: resolve the value to push onto the
          // caller's stack. Error(..) carries an already-thrown completion.
          let resolved = case constructor_this {
            Some(constructed_obj) ->
              // Base constructor: use the constructed object unless the
              // function explicitly returned an object.
              // §13.3.7.1 SuperCall step 8 BindThisValue(result) is handled at
              // the call site: emit's `CallConstructor; Dup; set_this` writes
              // effective_return into the caller's lexical-`this` slot.
              case return_value {
                JsObject(_) -> Ok(return_value)
                _ -> Ok(constructed_obj)
              }
            None ->
              case state.func.is_derived_constructor {
                True ->
                  case return_value {
                    JsObject(_) -> Ok(return_value)
                    JsUndefined ->
                      case read_this_local(state) {
                        JsUninitialized ->
                          Error(state.throw_reference_error(
                            state,
                            "Must call super constructor in derived class before returning from derived constructor",
                          ))
                        this_val -> Ok(this_val)
                      }
                    _ ->
                      Error(state.throw_type_error(
                        state,
                        "Derived constructors may only return object or undefined",
                      ))
                  }
                False ->
                  // Regular function return
                  Ok(return_value)
              }
          }
          case resolved {
            Error(thrown) -> thrown
            Ok(pushed) ->
              // Restore the caller's frame. This includes call_args: the
              // caller may still need its own args after this call returns
              // (e.g. IrCreateRestArray runs after default-value expressions,
              // which can themselves contain calls).
              Ok(maybe_collect_at_toplevel(
                State(
                  ..state,
                  stack: [pushed, ..stack],
                  locals:,
                  func:,
                  code: func.bytecode,
                  constants: func.constants,
                  pc:,
                  call_stack: rest_frames,
                  call_depth: state.call_depth - 1,
                  try_stack:,
                  new_target: saved_new_target,
                  call_args: saved_call_args,
                  eval_env: saved_eval_env,
                ),
              ))
          }
        }
      }
    }

    Jump(target) -> Ok(State(..state, pc: target))

    JumpIfFalse(target) -> {
      use v <- conditional_jump(state, target)
      !value.is_truthy(v)
    }

    JumpIfTrue(target) -> conditional_jump(state, target, value.is_truthy)

    JumpIfNullish(target) -> {
      use v <- conditional_jump(state, target)
      case v {
        JsNull | JsUndefined -> True
        _ -> False
      }
    }

    // QuickJS OP_gosub: push return-PC as a tagged number, jump to finally body.
    opcode.Gosub(target) ->
      Ok(
        State(
          ..state,
          stack: [value.from_int(state.pc + 1), ..state.stack],
          pc: target,
        ),
      )

    // QuickJS OP_ret: pop return-PC, jump back to it. A negative retpc is the
    // sentinel pushed by generator .return() finally-unwinding (see
    // process_generator_return) — it means "the slot below me is a return
    // value, complete the frame with it".
    opcode.Ret ->
      case state.stack {
        [value.JsNumber(value.Finite(f)), slot, ..rest] if f <. 0.0 ->
          Error(#(Done, slot, State(..state, stack: rest)))
        [value.JsNumber(value.Finite(f)), ..rest] ->
          Ok(State(..state, stack: rest, pc: float.truncate(f)))
        _ -> underflow(state, "Ret")
      }

    // -- Exception handling --
    PushTry(catch_target) -> {
      let frame = TryFrame(catch_target:, stack_depth: list.length(state.stack))
      Ok(
        State(..state, try_stack: [frame, ..state.try_stack], pc: state.pc + 1),
      )
    }

    opcode.PopTry -> {
      case state.try_stack {
        [_, ..rest] -> Ok(State(..state, try_stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "PopTry: empty try_stack")
      }
    }

    opcode.Throw -> {
      case state.stack {
        [value, ..] -> Error(#(Thrown, value, state))
        [] -> underflow(state, "Throw")
      }
    }

    opcode.ThrowConstAssign(_name) ->
      state.throw_type_error(state, "Assignment to constant variable.")

    opcode.ThrowError(kind, msg) ->
      case kind {
        opcode.ReferenceErrorKind -> state.throw_reference_error(state, msg)
        opcode.TypeErrorKind -> state.throw_type_error(state, msg)
      }

    // ---- Object property access --------------------------------------
    NewObject -> {
      let #(heap, ref) =
        common.alloc_wrapper(
          state.heap,
          OrdinaryObject,
          state.builtins.object.prototype,
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    GetField(op_key) -> {
      let key = value.from_op_key(op_key)
      case state.stack {
        [JsNull as v, ..] | [JsUndefined as v, ..] ->
          state.throw_type_error(
            state,
            "Cannot read properties of "
              <> value.nullish_label(v)
              <> " (reading '"
              <> opcode.key_name(op_key)
              <> "')",
          )
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(object.get_value_of(state, receiver, key)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField")
      }
    }

    GetField2(op_key) -> {
      // Like GetField but keeps the object on the stack for CallMethod.
      // Stack: [obj, ..rest] → [prop_value, obj, ..rest]
      let key = value.from_op_key(op_key)
      case state.stack {
        [JsNull as v, ..] | [JsUndefined as v, ..] ->
          state.throw_type_error(
            state,
            "Cannot read properties of "
              <> value.nullish_label(v)
              <> " (reading '"
              <> opcode.key_name(op_key)
              <> "')",
          )
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(object.get_value_of(state, receiver, key)),
          )
          State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField2")
      }
    }

    PutField(op_key) -> {
      // Consumes [value, obj] and pushes value back (assignment is an expression).
      // Consistent with PutElem which also leaves the value on the stack.
      let key = value.from_op_key(op_key)
      case state.stack {
        [value, JsObject(ref) as receiver, ..rest] -> {
          // set_value walks proto chain, calls setters, handles non-writable.
          use #(state, ok) <- result.try(
            state.rethrow(object.set_value(state, ref, key, value, receiver)),
          )
          // §13.15.2 PutValue step 6.b.iv: failed [[Set]] throws TypeError
          // in strict mode; sloppy mode ignores the failure.
          case ok, state.func.is_strict {
            False, True ->
              state.throw_type_error(
                state,
                "Cannot assign to read only property '"
                  <> opcode.key_name(op_key)
                  <> "' of object",
              )
            _, _ -> Ok(State(..state, stack: [value, ..rest], pc: state.pc + 1))
          }
        }
        // §6.2.5.6 PutValue step 5.a: ToObject(undefined|null) throws
        // TypeError in BOTH modes — sloppy-ignore only applies to boxed
        // primitives below.
        [_, JsUndefined, ..] | [_, JsNull, ..] ->
          state.throw_type_error(
            state,
            "Cannot set properties of undefined or null (setting '"
              <> opcode.key_name(op_key)
              <> "')",
          )
        [value, _, ..rest] -> {
          // PutField on non-object base (primitive): §13.15.2 PutValue 6.b.iv —
          // strict mode throws TypeError, sloppy mode silently ignores.
          case state.func.is_strict {
            True ->
              state.throw_type_error(
                state,
                "Cannot create property '"
                  <> opcode.key_name(op_key)
                  <> "' on primitive value",
              )
            False ->
              Ok(State(..state, stack: [value, ..rest], pc: state.pc + 1))
          }
        }
        _ -> underflow(state, "PutField")
      }
    }

    GetPrivateField(op_key) -> {
      let key = value.private_from_op_key(op_key)
      case state.stack {
        [JsObject(ref) as receiver, ..rest] ->
          // Single chain walk: find the descriptor (brand check), then apply
          // OrdinaryGet steps 3-7 to it — no second walk via get_value_of.
          case object.find_property(state.heap, ref, key) {
            Some(prop) -> {
              use #(val, state) <- result.map(
                state.rethrow(object.property_get_value(state, prop, receiver)),
              )
              State(..state, stack: [val, ..rest], pc: state.pc + 1)
            }
            None ->
              state.throw_type_error(
                state,
                "Cannot read private member "
                  <> opcode.key_name(op_key)
                  <> " from an object whose class did not declare it",
              )
          }
        [_, ..] ->
          state.throw_type_error(
            state,
            "Cannot read private member "
              <> opcode.key_name(op_key)
              <> " on non-object",
          )
        [] -> underflow(state, "GetPrivateField")
      }
    }

    GetPrivateField2(op_key) -> {
      let key = value.private_from_op_key(op_key)
      case state.stack {
        [JsObject(ref) as receiver, ..rest] ->
          // Single chain walk — see GetPrivateField.
          case object.find_property(state.heap, ref, key) {
            Some(prop) -> {
              use #(val, state) <- result.map(
                state.rethrow(object.property_get_value(state, prop, receiver)),
              )
              State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
            }
            None ->
              state.throw_type_error(
                state,
                "Cannot read private member "
                  <> opcode.key_name(op_key)
                  <> " from an object whose class did not declare it",
              )
          }
        [_, ..] ->
          state.throw_type_error(
            state,
            "Cannot read private member "
              <> opcode.key_name(op_key)
              <> " on non-object",
          )
        [] -> underflow(state, "GetPrivateField2")
      }
    }

    PutPrivateField(op_key) -> {
      let key = value.private_from_op_key(op_key)
      case state.stack {
        [val, JsObject(ref) as receiver, ..rest] ->
          // Single chain walk: find the descriptor (brand check), then apply
          // OrdinarySetWithOwnDescriptor to it — no second walk via set_value.
          case object.find_property(state.heap, ref, key) {
            Some(prop) -> {
              use #(state, ok) <- result.try(
                state.rethrow(object.set_found_value(
                  state,
                  receiver,
                  prop,
                  key,
                  val,
                )),
              )
              // §7.3.32 PrivateSet: kind ~method~ or an accessor without a
              // setter throws TypeError (regardless of strict mode).
              case ok {
                True ->
                  Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
                False ->
                  state.throw_type_error(
                    state,
                    "Cannot write private member "
                      <> opcode.key_name(op_key)
                      <> ": it is a method or has no setter",
                  )
              }
            }
            None ->
              state.throw_type_error(
                state,
                "Cannot write private member "
                  <> opcode.key_name(op_key)
                  <> " to an object whose class did not declare it",
              )
          }
        [_, _, ..] ->
          state.throw_type_error(
            state,
            "Cannot write private member "
              <> opcode.key_name(op_key)
              <> " on non-object",
          )
        _ -> underflow(state, "PutPrivateField")
      }
    }

    PrivateIn(op_key) -> {
      let key = value.private_from_op_key(op_key)
      case state.stack {
        [JsObject(ref), ..rest] -> {
          // Brand check via find_property (chain walk) — has_property hides
          // private-name keys from ordinary [[HasProperty]], so it can't be
          // used here.
          let found = option.is_some(object.find_property(state.heap, ref, key))
          Ok(State(..state, stack: [JsBool(found), ..rest], pc: state.pc + 1))
        }
        [_, ..] ->
          state.throw_type_error(
            state,
            "Cannot use 'in' operator to search for private name "
              <> opcode.key_name(op_key)
              <> " in non-object",
          )
        [] -> underflow(state, "PrivateIn")
      }
    }

    NewPrivateName(name) -> {
      // §15.7.14 step 5/6: mint a fresh PrivateName for this class
      // evaluation. The minted storage-key text travels as a JsString.
      let key_text = value.mint_private_key(name)
      Ok(
        State(
          ..state,
          stack: [JsString(key_text), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    GetPrivateFieldDyn -> {
      // §7.3.30 PrivateGet. [key, obj, ..] → [val, ..]. Own-only lookup —
      // spec [[PrivateElements]] never inherit through the prototype chain.
      case state.stack {
        [JsString(key_text), JsObject(ref) as receiver, ..rest] ->
          case object.get_own_property(state.heap, ref, Named(key_text)) {
            Some(value.AccessorProperty(get: None, ..)) ->
              state.throw_type_error(
                state,
                "'"
                  <> value.private_display_name(key_text)
                  <> "' was defined without a getter",
              )
            Some(prop) -> {
              use #(val, state) <- result.map(
                state.rethrow(object.property_get_value(state, prop, receiver)),
              )
              State(..state, stack: [val, ..rest], pc: state.pc + 1)
            }
            None ->
              state.throw_type_error(
                state,
                "Cannot read private member "
                  <> value.private_display_name(key_text)
                  <> " from an object whose class did not declare it",
              )
          }
        [JsString(key_text), _, ..] ->
          state.throw_type_error(
            state,
            "Cannot read private member "
              <> value.private_display_name(key_text)
              <> " on non-object",
          )
        [_, _, ..] | [_] | [] -> underflow(state, "GetPrivateFieldDyn")
      }
    }

    GetPrivateFieldDyn2 -> {
      // Like GetPrivateFieldDyn but keeps obj: [key, obj, ..] → [val, obj, ..]
      case state.stack {
        [JsString(key_text), JsObject(ref) as receiver, ..rest] ->
          case object.get_own_property(state.heap, ref, Named(key_text)) {
            Some(value.AccessorProperty(get: None, ..)) ->
              state.throw_type_error(
                state,
                "'"
                  <> value.private_display_name(key_text)
                  <> "' was defined without a getter",
              )
            Some(prop) -> {
              use #(val, state) <- result.map(
                state.rethrow(object.property_get_value(state, prop, receiver)),
              )
              State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
            }
            None ->
              state.throw_type_error(
                state,
                "Cannot read private member "
                  <> value.private_display_name(key_text)
                  <> " from an object whose class did not declare it",
              )
          }
        [JsString(key_text), _, ..] ->
          state.throw_type_error(
            state,
            "Cannot read private member "
              <> value.private_display_name(key_text)
              <> " on non-object",
          )
        [_, _, ..] | [_] | [] -> underflow(state, "GetPrivateFieldDyn2")
      }
    }

    PutPrivateFieldDyn -> {
      // §7.3.31 PrivateSet. [key, val, obj, ..] → [val, ..]. Own-only.
      case state.stack {
        [JsString(key_text), val, JsObject(ref) as receiver, ..rest] -> {
          let key = Named(key_text)
          case object.get_own_property(state.heap, ref, key) {
            Some(prop) -> {
              use #(state, ok) <- result.try(
                state.rethrow(object.set_found_value(
                  state,
                  receiver,
                  prop,
                  key,
                  val,
                )),
              )
              // kind ~method~ (non-writable data) or an accessor without a
              // setter throws TypeError regardless of strict mode.
              case ok {
                True ->
                  Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
                False ->
                  state.throw_type_error(
                    state,
                    "Cannot write private member "
                      <> value.private_display_name(key_text)
                      <> ": it is a method or has no setter",
                  )
              }
            }
            None ->
              state.throw_type_error(
                state,
                "Cannot write private member "
                  <> value.private_display_name(key_text)
                  <> " to an object whose class did not declare it",
              )
          }
        }
        [JsString(key_text), _, _, ..] ->
          state.throw_type_error(
            state,
            "Cannot write private member "
              <> value.private_display_name(key_text)
              <> " on non-object",
          )
        [_, _, _, ..] | [_, _] | [_] | [] ->
          underflow(state, "PutPrivateFieldDyn")
      }
    }

    PrivateInDyn -> {
      // §13.10.1 `#x in obj`. [key, obj, ..] → [bool, ..]. Own-only check.
      case state.stack {
        [JsString(key_text), JsObject(ref), ..rest] -> {
          let found =
            option.is_some(object.get_own_property(
              state.heap,
              ref,
              Named(key_text),
            ))
          Ok(State(..state, stack: [JsBool(found), ..rest], pc: state.pc + 1))
        }
        [JsString(key_text), _, ..] ->
          state.throw_type_error(
            state,
            "Cannot use 'in' operator to search for private name "
              <> value.private_display_name(key_text)
              <> " in non-object",
          )
        [_, _, ..] | [_] | [] -> underflow(state, "PrivateInDyn")
      }
    }

    DefinePrivateField -> {
      // §7.3.28 PrivateFieldAdd. [val, key, obj, ..] → [obj, ..].
      case state.stack {
        [val, JsString(key_text), JsObject(ref) as obj, ..rest] -> {
          let key = Named(key_text)
          use Nil <- result.try(check_private_add(state, ref, key_text))
          let heap = object.define_private_data(state.heap, ref, key, val, True)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, _, ..] | [_, _] | [_] | [] ->
          underflow(state, "DefinePrivateField")
      }
    }

    DefinePrivateMethod -> {
      // §7.3.29 PrivateMethodOrAccessorAdd (method). [fn, key, obj, ..] →
      // [obj, ..]. Non-writable so PrivateSet's method check trips.
      case state.stack {
        [func, JsString(key_text), JsObject(ref) as obj, ..rest] -> {
          let key = Named(key_text)
          use Nil <- result.try(check_private_add(state, ref, key_text))
          let heap =
            object.define_private_data(state.heap, ref, key, func, False)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, _, ..] | [_, _] | [_] | [] ->
          underflow(state, "DefinePrivateMethod")
      }
    }

    DefinePrivateAccessor(kind) -> {
      // §7.3.29 for one accessor half. [fn, key, obj, ..] → [obj, ..].
      // get+set of one class evaluation merge; a half that is already
      // present means double initialization → TypeError.
      case state.stack {
        [func, JsString(key_text), JsObject(ref) as obj, ..rest] -> {
          let key = Named(key_text)
          let existing = object.get_own_property(state.heap, ref, key)
          let half_present = case existing, kind {
            Some(value.AccessorProperty(get: Some(_), ..)), opcode.Getter ->
              True
            Some(value.AccessorProperty(set: Some(_), ..)), opcode.Setter ->
              True
            Some(value.DataProperty(..)), _ -> True
            _, _ -> False
          }
          use Nil <- result.try(case existing {
            None -> check_private_add(state, ref, key_text)
            Some(_) ->
              case half_present {
                True ->
                  state.throw_type_error(
                    state,
                    "Cannot initialize private accessor "
                      <> value.private_display_name(key_text)
                      <> " twice on the same object",
                  )
                False -> Ok(Nil)
              }
          })
          // Private accessor: enumerability is never observable (private
          // names don't appear in any enumeration).
          let heap =
            object.define_accessor(
              state.heap,
              ref,
              key,
              func,
              kind,
              enumerable: False,
            )
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, _, ..] | [_, _] | [_] | [] ->
          underflow(state, "DefinePrivateAccessor")
      }
    }

    DefineField(op_key) -> {
      // Like PutField but keeps the object on the stack (for object literal
      // construction). "#"-prefixed static keys come from class private field
      // syntax and land in the private namespace (value.from_op_key_define).
      let key = value.from_op_key_define(op_key)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] ->
          case define_needs_full_semantics(state, ref, key) {
            // Fast path: ordinary extensible target (fresh object literals,
            // normal class instances) — a raw insert is spec-equivalent.
            False -> {
              let #(heap, _) = object.set_property(state.heap, ref, key, value)
              Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
            }
            // §7.3.32 DefineField → CreateDataPropertyOrThrow: a proxy
            // receiver fires its defineProperty trap; a frozen /
            // non-extensible receiver throws TypeError.
            True -> {
              use state <- result.map(define_field_full(
                state,
                ref,
                JsString(opcode.key_name(op_key)),
                value,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
          }
        [_, _, ..] -> {
          // DefineField on non-object: no-op, keep object on stack
          Ok(State(..state, pc: state.pc + 1))
        }
        _ -> underflow(state, "DefineField")
      }
    }

    DefineMethod(op_key) -> {
      // Like DefineField but creates a non-enumerable property (for class
      // methods). §15.4.4 MakeMethod: also set [[HomeObject]] = target on the
      // function so super.prop inside it finds the right base. "#"-prefixed
      // static keys (private methods) land in the private namespace.
      let key = value.from_op_key_define(op_key)
      case state.stack {
        [v, JsObject(ref) as obj, ..rest] -> {
          let heap = make_method(state.heap, v, ref)
          let heap = object.define_method_property(heap, ref, key, v)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineMethod")
      }
    }

    DefineMethodComputed -> {
      // Computed class method: class { [expr]() {} }
      // Stack: [fn, key, obj, ...] → [obj, ...]
      // Non-enumerable data property (writable, configurable).
      case state.stack {
        [func, value.JsSymbol(sym), JsObject(ref) as obj, ..rest] -> {
          let heap =
            set_computed_fn_name(
              state.heap,
              func,
              "",
              symbol_fn_name(state, sym),
            )
          let heap = make_method(heap, func, ref)
          let heap =
            object.define_symbol_property(
              heap,
              ref,
              sym,
              value.builtin_property(func),
            )
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [func, key, JsObject(ref) as obj, ..rest] -> {
          use #(pk, state) <- result.try(
            state.rethrow(property.to_property_key(state, key)),
          )
          // DefinePropertyOrThrow (§14.3.9 step 11): redefining an existing
          // non-configurable own property — e.g. static ['prototype']() on
          // the constructor — throws TypeError.
          use Nil <- result.map(check_define_nonconfigurable(state, ref, pk))
          let heap =
            set_computed_fn_name(state.heap, func, "", value.key_to_string(pk))
          let heap = make_method(heap, func, ref)
          let heap = object.define_method_property(heap, ref, pk, func)
          State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineMethodComputed")
      }
    }

    DefineAccessor(op_key, kind, enumerable) -> {
      // Object literal getter/setter: { get x() {}, set x(v) {} }
      // Stack: [fn, obj, ...] → [obj, ...]
      // Defines or updates an AccessorProperty on the object. "#"-prefixed
      // static keys (private accessors) land in the private namespace.
      let key = value.from_op_key_define(op_key)
      case state.stack {
        [func, JsObject(ref) as obj, ..rest] -> {
          let heap = make_method(state.heap, func, ref)
          let heap =
            object.define_accessor(heap, ref, key, func, kind, enumerable:)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineAccessor")
      }
    }

    DefineAccessorComputed(kind, enumerable) -> {
      // Computed getter/setter: { get [expr]() {} }
      // Stack: [fn, key, obj, ...] → [obj, ...]
      case state.stack {
        [func, value.JsSymbol(sym), JsObject(ref) as obj, ..rest] -> {
          let heap =
            set_computed_fn_name(
              state.heap,
              func,
              accessor_name_prefix(kind),
              symbol_fn_name(state, sym),
            )
          let heap = make_method(heap, func, ref)
          let heap =
            object.define_symbol_accessor(
              heap,
              ref,
              sym,
              func,
              kind,
              enumerable:,
            )
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [func, key, JsObject(ref) as obj, ..rest] -> {
          use #(pk, state) <- result.try(
            state.rethrow(property.to_property_key(state, key)),
          )
          // DefinePropertyOrThrow (§14.3.9): an accessor cannot replace an
          // existing non-configurable own property (static get/set
          // ['prototype'] on the constructor) — TypeError.
          use Nil <- result.map(check_define_nonconfigurable(state, ref, pk))
          let heap =
            set_computed_fn_name(
              state.heap,
              func,
              accessor_name_prefix(kind),
              value.key_to_string(pk),
            )
          let heap = make_method(heap, func, ref)
          let heap =
            object.define_accessor(heap, ref, pk, func, kind, enumerable:)
          State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "DefineAccessorComputed")
      }
    }

    MakeMethod ->
      // §15.4.4 — set top-of-stack closure's [[HomeObject]] to the object
      // directly beneath it. Stack-neutral; DefineField/Computed follows.
      case state.stack {
        [func, JsObject(ref), ..] -> {
          let heap = make_method(state.heap, func, ref)
          Ok(State(..state, heap:, pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ -> underflow(state, "MakeMethod")
      }

    DefineFieldComputed -> {
      // Object literal computed key: {[key]: value}
      // Stack: [value, key, obj, ...] → [obj, ...]
      // Key goes through ToPropertyKey (Symbol preserved, else ToString).
      // put_elem_value already implements this (symbol → symbol_properties,
      // array index → elements, else → js_to_string → properties).
      case state.stack {
        // Proxy target (class field init where the constructor returned a
        // proxy): §7.3.7 CreateDataPropertyOrThrow goes through the proxy's
        // [[DefineOwnProperty]] (defineProperty trap), NOT [[Set]]. A
        // deferred module namespace relies on this to trigger evaluation.
        [val, key, JsObject(ref) as obj, ..rest] -> {
          // Proxies AND frozen/non-extensible targets take the full
          // [[DefineOwnProperty]] path (trap / TypeError respectively).
          let needs_full = case object.as_proxy(state.heap, ref) {
            Some(_) -> True
            None ->
              case heap.read(state.heap, ref) {
                Some(ObjectSlot(extensible:, ..)) -> !extensible
                _ -> False
              }
          }
          case needs_full {
            True -> {
              use state <- result.map(define_field_full(state, ref, key, val))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            False -> {
              // CreateDataPropertyOrThrow (§7.3.7): defining over an existing
              // non-configurable own property → TypeError. Reachable via a
              // static class field with computed name "prototype" (class
              // field keys are already ToPropertyKey'd primitives here);
              // fresh object literals have no non-configurable properties.
              use Nil <- result.try(case key {
                JsString("prototype") ->
                  check_define_nonconfigurable(state, ref, Named("prototype"))
                _ -> Ok(Nil)
              })
              // §7.3.7 CreateDataProperty defines an OWN property — it must
              // NOT walk the prototype chain or invoke inherited setters
              // (e.g. `{0: v}` with an accessor "0" on Object.prototype),
              // so use the raw own-define, not [[Set]].
              case key {
                value.JsSymbol(sym) -> {
                  let heap =
                    object.define_symbol_property(
                      state.heap,
                      ref,
                      sym,
                      value.data_property(val),
                    )
                  Ok(
                    State(
                      ..state,
                      heap:,
                      stack: [obj, ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                }
                _ -> {
                  use #(pk, state) <- result.map(
                    state.rethrow(property.to_property_key(state, key)),
                  )
                  let #(heap, _ok) =
                    object.set_property(state.heap, ref, pk, val)
                  State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1)
                }
              }
            }
          }
        }
        [_, _, _, ..rest] ->
          // Non-object target: shouldn't happen for literals, but pop and keep going.
          Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ -> underflow(state, "DefineFieldComputed")
      }
    }

    // Annex B §B.3.1 — `{__proto__: v}` literal sets [[Prototype]].
    // Stack: [val, obj, ...] → [obj, ...]. If val is Object or null, set
    // obj.[[Prototype]]; else no-op (no error). The target is a fresh literal
    // so always extensible & cycle-free — direct slot write is spec-equivalent.
    SetProto ->
      case state.stack {
        [val, JsObject(ref) as obj, ..rest] -> {
          let heap = case val {
            JsObject(proto_ref) ->
              set_slot_prototype(state.heap, ref, Some(proto_ref))
            JsNull -> set_slot_prototype(state.heap, ref, None)
            _ -> state.heap
          }
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        _ -> underflow(state, "SetProto")
      }

    ObjectSpread -> {
      // Object spread: {...source}
      // Stack: [source, obj, ...] → [obj, ...]
      // CopyDataProperties: own enumerable props of source → target.
      // null/undefined/primitives → no-op per spec (unlike assign target).
      case state.stack {
        [source, JsObject(ref) as obj, ..rest] -> {
          use state <- result.map(
            state.rethrow(builtins_object.copy_data_properties_stateful(
              state,
              ref,
              source,
              set.new(),
              set.new(),
            )),
          )
          State(..state, stack: [obj, ..rest], pc: state.pc + 1)
        }
        [_, _, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ -> underflow(state, "ObjectSpread")
      }
    }

    // Destructuring rest: `let {a, b, ...rest} = src`
    // Stack: [src, key_n, ..., key_1, ...] → [rest_obj, ...]
    // §13.15.5.3 RestBindingInitialization → CopyDataProperties with
    // excludedNames = the n keys already bound.
    ObjectRestCopy(excluded_count) ->
      case state.stack {
        [source, ..below] ->
          case pop_n(below, excluded_count) {
            Some(#(raw_keys, rest)) -> {
              let state = State(..state, stack: rest)
              // §8.6.2 RequireObjectCoercible — unlike object-spread,
              // `let {...x} = null` MUST throw TypeError.
              case source {
                JsNull ->
                  state.throw_type_error(
                    state,
                    "Cannot destructure 'null' as it is null.",
                  )
                JsUndefined ->
                  state.throw_type_error(
                    state,
                    "Cannot destructure 'undefined' as it is undefined.",
                  )
                _ -> {
                  // ToPropertyKey each excluded key (computed keys arrive
                  // as raw JsValue; static keys are already JsString).
                  use #(ex_keys, ex_syms, state) <- result.try(
                    state.rethrow(build_exclusion_sets(state, raw_keys)),
                  )
                  let #(heap, ref) =
                    common.alloc_wrapper(
                      state.heap,
                      OrdinaryObject,
                      state.builtins.object.prototype,
                    )
                  let state = State(..state, heap:)
                  use state <- result.map(
                    state.rethrow(builtins_object.copy_data_properties_stateful(
                      state,
                      ref,
                      source,
                      ex_keys,
                      ex_syms,
                    )),
                  )
                  State(
                    ..state,
                    stack: [JsObject(ref), ..state.stack],
                    pc: state.pc + 1,
                  )
                }
              }
            }
            None -> underflow(state, "ObjectRestCopy")
          }
        _ -> underflow(state, "ObjectRestCopy")
      }

    // -- Delete operator --
    DeleteField(op_key) -> {
      let key = value.from_op_key(op_key)
      case state.stack {
        [obj, ..rest] ->
          case obj {
            JsObject(ref) -> {
              use #(state, success) <- result.try(
                state.rethrow(object.delete_property_stateful(
                  state,
                  ref,
                  object.PkString(key),
                )),
              )
              // §13.5.1.2 step 5.b.i: strict-mode delete of a
              // non-configurable property throws TypeError.
              case success, state.func.is_strict {
                False, True ->
                  state.throw_type_error(
                    state,
                    "Cannot delete property '" <> opcode.key_name(op_key) <> "'",
                  )
                _, _ ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(success), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
              }
            }
            // delete on non-object returns true
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ -> underflow(state, "DeleteField")
      }
    }

    DeleteElem -> {
      case state.stack {
        [key, obj, ..rest] ->
          case obj {
            JsObject(ref) ->
              case key {
                value.JsSymbol(sym) -> {
                  use #(state, success) <- result.try(
                    state.rethrow(object.delete_property_stateful(
                      state,
                      ref,
                      object.PkSymbol(sym),
                    )),
                  )
                  // §13.5.1.2 step 5.b.i: strict delete failure throws.
                  case success, state.func.is_strict {
                    False, True ->
                      state.throw_type_error(state, "Cannot delete property")
                    _, _ ->
                      Ok(
                        State(
                          ..state,
                          stack: [JsBool(success), ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                  }
                }
                _ -> {
                  use #(pk, state) <- result.try(
                    state.rethrow(property.to_property_key(state, key)),
                  )
                  use #(state, success) <- result.try(
                    state.rethrow(object.delete_property_stateful(
                      state,
                      ref,
                      object.PkString(pk),
                    )),
                  )
                  // §13.5.1.2 step 5.b.i: strict delete failure throws.
                  case success, state.func.is_strict {
                    False, True ->
                      state.throw_type_error(state, "Cannot delete property")
                    _, _ ->
                      Ok(
                        State(
                          ..state,
                          stack: [JsBool(success), ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                  }
                }
              }
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ -> underflow(state, "DeleteElem")
      }
    }

    // -- Class Inheritance --
    SetupDerivedClass -> {
      // Stack: [ctor, parent, ..rest] → [ctor, ..rest]
      // Wire ctor.prototype.__proto__ = parent.prototype
      // Wire ctor.__proto__ = parent (for static inheritance)
      case state.stack {
        [JsObject(ctor_ref) as ctor, JsObject(parent_ref) as parent, ..rest] ->
          // §15.7.14 step 5.f — IsConstructor BEFORE Get(superclass,"prototype")
          // so an arrow/generator/async heritage throws without touching its
          // (possibly trapped) .prototype.
          case object.is_constructor(state.heap, parent) {
            False ->
              state.throw_type_error(
                state,
                "Class extends value is not a constructor or null",
              )
            True -> {
              // §15.7.14 step 5.g: protoParent = ? Get(superclass,
              // "prototype") — observable; neither Object nor null (e.g. a
              // bound function's missing prototype) → TypeError.
              use #(parent_proto_val, state) <- result.try(
                state.rethrow(object.get_value(
                  state,
                  parent_ref,
                  Named("prototype"),
                  parent,
                )),
              )
              use parent_proto <- result.try(case parent_proto_val {
                JsObject(p) -> Ok(Some(p))
                JsNull -> Ok(None)
                _ ->
                  state.throw_type_error(
                    state,
                    "Class extends value does not have valid prototype property "
                      <> object.inspect(parent_proto_val, state.heap),
                  )
              })
              let ctor_proto = get_field_ref(state.heap, ctor_ref, "prototype")
              // §15.7.14 step 12: ctor.[[HomeObject]] = ctor.prototype
              let heap =
                option.map(ctor_proto, make_method(state.heap, ctor, _))
                |> option.unwrap(state.heap)
              // Set ctor.prototype.__proto__ = protoParent (object or null)
              let heap =
                option.map(ctor_proto, set_slot_prototype(heap, _, parent_proto))
                |> option.unwrap(heap)
              // Set ctor.__proto__ = parent (for static inheritance)
              let heap = set_slot_prototype(heap, ctor_ref, Some(parent_ref))
              Ok(
                State(
                  ..state,
                  heap:,
                  stack: [JsObject(ctor_ref), ..rest],
                  pc: state.pc + 1,
                ),
              )
            }
          }
        [JsObject(ctor_ref) as ctor, JsNull, ..rest] -> {
          // extends null — ctor.prototype.__proto__ = null,
          // ctor.[[HomeObject]] = ctor.prototype
          let ctor_proto = get_field_ref(state.heap, ctor_ref, "prototype")
          let heap =
            option.map(ctor_proto, make_method(state.heap, ctor, _))
            |> option.unwrap(state.heap)
          let heap =
            option.map(ctor_proto, set_slot_prototype(heap, _, None))
            |> option.unwrap(heap)
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ctor_ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        _ -> {
          state.throw_type_error(
            state,
            "Class extends value is not a constructor or null",
          )
        }
      }
    }

    // ---- Array operations --------------------------------------------
    // -- Array construction --
    ArrayFrom(count) -> {
      case pop_n(state.stack, count) {
        Some(#(elements, rest)) -> {
          // elements are in order [first, ..., last]
          let #(heap, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: ArrayObject(count),
                properties: dict.new(),
                elements: elements.from_list(elements),
                prototype: Some(state.builtins.array.prototype),
                symbol_properties: [],
                extensible: True,
              ),
            )
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        None -> underflow(state, "ArrayFrom")
      }
    }

    ArrayFromWithHoles(count, holes) -> {
      // Pop only the non-hole values (count - len(holes)), then zip them with
      // the non-hole indices and build a SparseElements-backed tuple_array.
      // The emitter guarantees `holes` is non-empty (empty → ArrayFrom used),
      // sorted ascending, and all indices are in [0, count).
      let value_count = count - list.length(holes)
      case pop_n(state.stack, value_count) {
        Some(#(values, rest)) -> {
          // values are in order [first_non_hole, ..., last_non_hole]
          let indexed = array_ops.assign_non_hole_indices(values, holes, 0, [])
          let #(heap, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: ArrayObject(count),
                properties: dict.new(),
                elements: elements.from_indexed(indexed),
                prototype: Some(state.builtins.array.prototype),
                symbol_properties: [],
                extensible: True,
              ),
            )
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        None -> underflow(state, "ArrayFromWithHoles")
      }
    }

    // -- Computed property access --
    GetElem -> {
      case state.stack {
        [key, JsObject(ref), ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(property.get_elem_value(state, ref, key)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [_, JsNull as v, ..] | [_, JsUndefined as v, ..] ->
          state.throw_type_error(
            state,
            "Cannot read properties of " <> value.nullish_label(v),
          )
        [key, receiver, ..rest] -> {
          // Primitive receiver: canonicalize key, delegate to get_value_of
          use #(val, state) <- result.map(
            state.rethrow(get_elem_on_primitive(state, receiver, key)),
          )
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetElem")
      }
    }

    GetElem2 -> {
      // Like GetElem but keeps obj+key on stack: [key, obj, ...] -> [value, key, obj, ...]
      // Used by compound assignment / update on computed members, where the
      // spec evaluates ToPropertyKey exactly ONCE (§13.15.2 + §13.3.3): the
      // key left on the stack for the later PutElem is the already-converted
      // property key, so a stateful toString isn't called a second time.
      // RequireObjectCoercible on the base comes BEFORE ToPropertyKey
      // (GetValue → §6.2.5.5), so a nullish base throws TypeError without
      // touching the key.
      case state.stack {
        [_, JsNull as v, ..] | [_, JsUndefined as v, ..] ->
          state.throw_type_error(
            state,
            "Cannot read properties of " <> value.nullish_label(v),
          )
        [value.JsSymbol(sym) as key, JsObject(ref) as obj, ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(object.get_symbol_value(state, ref, sym, obj)),
          )
          State(..state, stack: [val, key, obj, ..rest], pc: state.pc + 1)
        }
        [key, JsObject(ref) as obj, ..rest] -> {
          use #(pk, state) <- result.try(
            state.rethrow(property.to_property_key(state, key)),
          )
          use #(val, state) <- result.map(
            state.rethrow(object.get_value(state, ref, pk, obj)),
          )
          State(
            ..state,
            stack: [val, property_key_value(pk), obj, ..rest],
            pc: state.pc + 1,
          )
        }
        [value.JsSymbol(sym) as key, receiver, ..rest] -> {
          use #(val, state) <- result.map(
            state.rethrow(object.get_symbol_value_of(state, receiver, sym)),
          )
          State(..state, stack: [val, key, receiver, ..rest], pc: state.pc + 1)
        }
        [key, receiver, ..rest] -> {
          use #(pk, state) <- result.try(
            state.rethrow(property.to_property_key(state, key)),
          )
          use #(val, state) <- result.map(
            state.rethrow(object.get_value_of(state, receiver, pk)),
          )
          State(
            ..state,
            stack: [val, property_key_value(pk), receiver, ..rest],
            pc: state.pc + 1,
          )
        }
        _ -> underflow(state, "GetElem2")
      }
    }

    PutElem -> {
      // Stack: [value, key, obj, ...rest]
      case state.stack {
        [val, key, JsObject(ref), ..rest] -> {
          use #(state, ok) <- result.try(
            state.rethrow(property.put_elem_value(state, ref, key, val)),
          )
          // §13.15.2 PutValue step 6.b.iv: failed [[Set]] throws TypeError
          // in strict mode; sloppy mode ignores the failure.
          case ok, state.func.is_strict {
            False, True ->
              state.throw_type_error(
                state,
                "Cannot assign to read only property of object",
              )
            _, _ -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
          }
        }
        // §6.2.5.6 PutValue step 5.a: ToObject(undefined|null) throws
        // TypeError in BOTH modes — sloppy-ignore only applies to boxed
        // primitives below.
        [_, _, JsUndefined, ..] | [_, _, JsNull, ..] ->
          state.throw_type_error(
            state,
            "Cannot set properties of undefined or null",
          )
        [val, _, _, ..rest] -> {
          // PutElem on non-object base (primitive): §13.15.2 PutValue 6.b.iv —
          // strict mode throws TypeError, sloppy mode silently ignores.
          // Stack effect must match the object arm: value stays on the stack.
          case state.func.is_strict {
            True ->
              state.throw_type_error(
                state,
                "Cannot create property on primitive value",
              )
            False -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
          }
        }
        _ -> underflow(state, "PutElem")
      }
    }

    // -- Spread element support (array literals + calls) --
    ArrayPush -> {
      // [val, arr] → [arr]; arr[arr.length] = val, length++.
      case state.stack {
        [val, JsObject(ref) as arr, ..rest] -> {
          let heap = array_ops.push_onto_array(state.heap, ref, val)
          Ok(State(..state, heap:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        _ -> underflow(state, "ArrayPush")
      }
    }

    ArrayPushHole -> {
      // [arr] → [arr]; length++ WITHOUT setting any element.
      case state.stack {
        [JsObject(ref) as arr, ..rest] -> {
          let heap = array_ops.grow_array_length(state.heap, ref)
          Ok(State(..state, heap:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        _ -> underflow(state, "ArrayPushHole")
      }
    }

    ArraySpread -> {
      // [iterable, arr] → [arr]; drain iterable via the iterator protocol.
      case state.stack {
        [iterable, JsObject(arr_ref) as arr, ..rest] -> {
          use state <- result.map(spread_into_array(state, arr_ref, iterable))
          State(..state, stack: [arr, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "ArraySpread")
      }
    }

    // ---- Function calls ----------------------------------------------
    CallEval(arity, param_scope_names, with_names, private_names) -> {
      // Syntactic `eval(...)` call. Runtime identity check: if the callee
      // resolves to the intrinsic eval function, do a DIRECT eval (sees
      // caller's locals via state.func.local_names + boxed slots). If eval
      // was shadowed/rebound, fall through to regular Call semantics.
      case pop_n(state.stack, arity) {
        Some(#(args, [JsObject(callee_ref), ..rest_stack]))
          if callee_ref == state.builtins.eval
        -> {
          let #(new_state, result) =
            realm.direct_eval_native(
              args,
              param_scope_names,
              with_names,
              private_names,
              State(..state, stack: rest_stack),
              execute_inner,
              new_state,
            )
          case result {
            Ok(val) ->
              Ok(
                State(
                  ..new_state,
                  stack: [val, ..new_state.stack],
                  pc: state.pc + 1,
                ),
              )
            // Unwind directly with new_state so eval_env (possibly just
            // lazy-allocated in run_direct_eval) threads through. The step
            // error return #(Thrown, val, Heap) can't carry it.
            Error(thrown) ->
              case unwind_to_catch(new_state, thrown) {
                Some(caught) -> Ok(caught)
                None -> Error(#(Thrown, thrown, new_state))
              }
          }
        }
        // Not the intrinsic eval — regular call semantics.
        _ -> step(state, Call(arity))
      }
    }

    Call(arity) -> {
      // Stack layout: [arg_n, ..., arg_1, callee, ...rest]
      // Pop arity args, then callee
      case pop_n(state.stack, arity) {
        Some(#(args, after_args)) -> {
          case after_args {
            [JsObject(obj_ref), ..rest_stack] -> {
              case heap.read(state.heap, obj_ref) {
                Some(ObjectSlot(
                  kind: FunctionObject(
                    func_template:,
                    env: env_ref,
                    home_object:,
                  ),
                  ..,
                )) -> {
                  let result =
                    call_function(
                      state,
                      obj_ref,
                      env_ref,
                      home_object,
                      func_template,
                      args,
                      rest_stack,
                      JsUndefined,
                      None,
                      JsUndefined,
                    )
                  case is_tail_call(state, func_template) {
                    True -> elide_tail_frame(result)
                    False -> result
                  }
                }
                Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) ->
                  call_native(state, native, args, rest_stack, JsUndefined)
                // Proxy [[Call]] (§10.5.12) — route through call.call_value,
                // which holds the trap machinery.
                Some(ObjectSlot(kind: value.ProxyObject(..), ..)) ->
                  call_value(
                    State(..state, stack: rest_stack),
                    JsObject(obj_ref),
                    args,
                    JsUndefined,
                  )
                _ ->
                  state.throw_type_error(
                    state,
                    object.inspect(JsObject(obj_ref), state.heap)
                      <> " is not a function",
                  )
              }
            }
            [non_func, ..] ->
              state.throw_type_error(
                state,
                object.inspect(non_func, state.heap) <> " is not a function",
              )
            [] -> underflow(state, "Call: no callee")
          }
        }
        None -> underflow(state, "Call: not enough args")
      }
    }

    CallMethod(_name, arity) -> {
      // Stack: [arg_n, ..., arg_1, method, receiver, ...rest]
      // Pop arity args, then method, then receiver
      case pop_n(state.stack, arity) {
        Some(#(args, after_args)) -> {
          case after_args {
            [JsObject(method_ref), receiver, ..rest_stack] -> {
              case heap.read(state.heap, method_ref) {
                Some(ObjectSlot(
                  kind: FunctionObject(
                    func_template:,
                    env: env_ref,
                    home_object:,
                  ),
                  ..,
                )) -> {
                  let result =
                    call_function(
                      state,
                      method_ref,
                      env_ref,
                      home_object,
                      func_template,
                      args,
                      rest_stack,
                      // Method call: this = receiver
                      receiver,
                      None,
                      JsUndefined,
                    )
                  case is_tail_call(state, func_template) {
                    True -> elide_tail_frame(result)
                    False -> result
                  }
                }
                Some(ObjectSlot(kind: NativeFunction(native, ..), ..)) ->
                  call_native(state, native, args, rest_stack, receiver)
                // Proxy [[Call]] (§10.5.12) — this = receiver.
                Some(ObjectSlot(kind: value.ProxyObject(..), ..)) ->
                  call_value(
                    State(..state, stack: rest_stack),
                    JsObject(method_ref),
                    args,
                    receiver,
                  )
                _ ->
                  state.throw_type_error(
                    state,
                    object.inspect(JsObject(method_ref), state.heap)
                      <> " is not a function",
                  )
              }
            }
            [non_func, _, ..] ->
              state.throw_type_error(
                state,
                object.inspect(non_func, state.heap) <> " is not a function",
              )
            _ -> underflow(state, "CallMethod")
          }
        }
        None -> underflow(state, "CallMethod: not enough args")
      }
    }

    CallConstructor(arity) ->
      // Stack: [arg_n, ..., arg_1, new_target, ctor, ...rest]
      case pop_n(state.stack, arity) {
        Some(#(args, [JsObject(nt_ref), JsObject(ctor_ref), ..rest_stack])) ->
          do_construct(state, ctor_ref, args, rest_stack, nt_ref)
        Some(#(_, [_, non_func, ..])) ->
          state.throw_type_error(
            state,
            object.inspect(non_func, state.heap) <> " is not a constructor",
          )
        Some(#(_, _)) -> underflow(state, "CallConstructor")
        None -> underflow(state, "CallConstructor: not enough args")
      }

    CallApply -> {
      // [args_array, callee] → [result]; this=undefined.
      case state.stack {
        [JsObject(args_ref), callee, ..rest] -> {
          let args = call.extract_array_args(state.heap, args_ref)
          call_value(State(..state, stack: rest), callee, args, JsUndefined)
        }
        [_, callee, ..] -> {
          state.throw_type_error(
            state,
            object.inspect(callee, state.heap) <> " is not a function",
          )
        }
        _ -> underflow(state, "CallApply")
      }
    }

    CallMethodApply -> {
      // [args_array, method, receiver] → [result]; this=receiver.
      case state.stack {
        [JsObject(args_ref), method, receiver, ..rest] -> {
          let args = call.extract_array_args(state.heap, args_ref)
          call_value(State(..state, stack: rest), method, args, receiver)
        }
        _ -> underflow(state, "CallMethodApply")
      }
    }

    CallConstructorApply ->
      // [args_array, new_target, ctor] → [new instance]. Spread-new path.
      case state.stack {
        [JsObject(args_ref), JsObject(nt_ref), JsObject(ctor_ref), ..rest] -> {
          let args = call.extract_array_args(state.heap, args_ref)
          do_construct(state, ctor_ref, args, rest, nt_ref)
        }
        [_, _, non_ctor, ..] ->
          state.throw_type_error(
            state,
            object.inspect(non_ctor, state.heap) <> " is not a constructor",
          )
        _ -> underflow(state, "CallConstructorApply")
      }

    // Generic [[GetPrototypeOf]]: [obj] → [proto|null]. Used as the second
    // hop for both `super.x` (home_object → proto) and `super()` (active_func
    // → parent ctor) — QuickJS OP_get_super.
    GetPrototypeOf ->
      case state.stack {
        [JsObject(ref), ..rest] -> {
          let proto = case heap.read(state.heap, ref) {
            Some(ObjectSlot(prototype: Some(p), ..)) -> JsObject(p)
            _ -> JsNull
          }
          Ok(State(..state, stack: [proto, ..rest], pc: state.pc + 1))
        }
        [_, ..rest] ->
          Ok(State(..state, stack: [JsNull, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "GetPrototypeOf")
      }

    // [key, base, this, ..] → [val, ..]. OrdinaryGet on base, receiver=this.
    GetSuperValue -> get_super_value(state, False, "GetSuperValue")

    // [key, base, this, ..] → [val, pk, base, this, ..]. Read-under for
    // compound/update super: ToPropertyKey ONCE (so e.g. key.toString runs
    // once per §13.15.2), Get with receiver=this, leave coerced key + base
    // + this for the trailing PutSuperValue.
    GetSuperValue2 -> get_super_value(state, True, "GetSuperValue2")

    PutSuperValue ->
      // [val, key, base, this, ..] → [val, ..]. OrdinarySet, receiver=this.
      case state.stack {
        // Symbol-keyed super assignment: super[Symbol.x] = v.
        [val, value.JsSymbol(sym), JsObject(base_ref), this_val, ..rest] -> {
          use #(state, ok) <- result.try(
            state.rethrow(object.set_symbol_value(
              state,
              base_ref,
              sym,
              val,
              this_val,
            )),
          )
          case ok, state.func.is_strict {
            False, True ->
              state.throw_type_error(
                state,
                "Cannot assign to read-only super property",
              )
            _, _ -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
          }
        }
        [val, key, JsObject(base_ref), this_val, ..rest] -> {
          use #(pk, state) <- result.try(
            state.rethrow(property.to_property_key(state, key)),
          )
          use #(state, ok) <- result.try(
            state.rethrow(object.set_value(state, base_ref, pk, val, this_val)),
          )
          // §6.2.5.6 PutValue step 5.c — gate on caller strictness so sloppy
          // object-literal methods stay non-throwing (QuickJS JS_PROP_THROW_STRICT).
          case ok, state.func.is_strict {
            False, True ->
              state.throw_type_error(
                state,
                "Cannot assign to read-only super property",
              )
            _, _ -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
          }
        }
        [_val, _key, _base, _this, ..] ->
          state.throw_type_error(
            state,
            "Cannot write super property when prototype is null",
          )
        _ -> underflow(state, "PutSuperValue")
      }

    MakeClosure(func_index) -> {
      // Compiler-generated index into the function table — always in bounds.
      let child_template =
        tuple_array.unsafe_get(func_index, state.func.functions)
      // Capture values from current frame according to env_descriptors.
      // For boxed captured vars, the local holds a JsObject(box_ref) —
      // copying that ref means the closure shares the same BoxSlot.
      let captured_values =
        list.map(child_template.env_descriptors, fn(desc) {
          case desc {
            value.CaptureLocal(parent_index) ->
              tuple_array.unsafe_get(parent_index, state.locals)
            value.CaptureEnv(_parent_env_index) ->
              // Transitive capture not yet implemented
              JsUndefined
          }
        })
      let #(heap, closure_ref) =
        make_closure(
          state.heap,
          state.builtins,
          child_template,
          captured_values,
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(closure_ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    // ---- Iteration ---------------------------------------------------
    ForInStart -> {
      case state.stack {
        [obj, ..rest] -> {
          // Collect enumerable keys from the object (or empty for non-objects)
          use #(keys, state) <- result.try(
            state.rethrow(case obj {
              JsObject(ref) ->
                case object.as_proxy(state.heap, ref) {
                  // Proxy: EnumerateObjectProperties via the ownKeys /
                  // getOwnPropertyDescriptor / getPrototypeOf traps.
                  Some(_) -> builtins_object.enumerate_keys_stateful(state, ref)
                  None -> Ok(#(object.enumerate_keys(state.heap, ref), state))
                }
              // for-in on null/undefined produces no iterations
              JsNull | JsUndefined -> Ok(#([], state))
              // Primitives: no enumerable properties
              _ -> Ok(#([], state))
            }),
          )
          // EnumerateObjectProperties checks each key via [[GetOwnProperty]], so
          // a TDZ namespace binding throws ReferenceError before iteration.
          let guard = case obj {
            JsObject(ref) -> object.namespace_tdz_guard(state, ref, keys)
            _ -> Ok(state)
          }
          use state <- result.try(state.rethrow(guard))
          // Wrap string keys as JsString values for ForInIteratorSlot
          let key_values = list.map(keys, JsString)
          let #(heap, iter_ref) =
            heap.alloc(state.heap, ForInIteratorSlot(keys: key_values))
          Ok(
            State(
              ..state,
              stack: [JsObject(iter_ref), ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
        _ -> underflow(state, "ForInStart")
      }
    }

    ForInNext -> {
      case state.stack {
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            Some(ForInIteratorSlot(keys:)) ->
              case keys {
                [val, ..remaining] -> {
                  // Advance the iterator
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ForInIteratorSlot(keys: remaining),
                    )
                  // Push: iterator stays, key, done=false
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
                [] -> {
                  // No more keys — push undefined + done=true
                  Ok(
                    State(
                      ..state,
                      stack: [
                        JsBool(True),
                        JsUndefined,
                        JsObject(iter_ref),
                        ..rest
                      ],
                      pc: state.pc + 1,
                    ),
                  )
                }
              }
            _ ->
              Error(#(
                StepVmError(Unimplemented("ForInNext: not a ForInIteratorSlot")),
                JsUndefined,
                state,
              ))
          }
        _ -> underflow(state, "ForInNext")
      }
    }

    GetIterator -> {
      case state.stack {
        [iterable, ..rest] ->
          case iterable {
            JsObject(ref) ->
              case heap.read(state.heap, ref) {
                // Iterators are their own iterator — [Symbol.iterator]() on
                // %IteratorPrototype% returns `this`. Skip the proto walk.
                // Generator/Array/String iterators are stepped in-VM by
                // IteratorNext (`.next` is never read), so push them raw.
                Some(ObjectSlot(kind: GeneratorObject(_), ..))
                | Some(ObjectSlot(kind: ArrayIteratorObject(..), ..))
                | Some(ObjectSlot(kind: value.StringIteratorObject(..), ..)) ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(ref), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                // Set/Map iterators are their own iterator too, but they step
                // through native %SetIteratorPrototype%.next — build the
                // Iterator Record so `.next` is fetched once, not per step.
                Some(ObjectSlot(kind: value.SetIteratorObject(..), ..))
                | Some(ObjectSlot(kind: value.MapIteratorObject(..), ..)) ->
                  push_iterator_record(state, ref, rest)
                // All other objects: look up Symbol.iterator per §7.4.1.
                // No array fast path — must honor deleted/overridden
                // Symbol.iterator (test262 destructuring tests rely on this).
                Some(ObjectSlot(..)) ->
                  get_iterator_via_symbol(state, ref, iterable, rest)
                _ ->
                  state.throw_type_error(
                    state,
                    object.inspect(iterable, state.heap) <> " is not iterable",
                  )
              }
            // String primitive: iterate code points — ES §22.1.5.
            // Snapshot the codepoints up front: O(n) total, O(1) per next,
            // vs O(n²) for per-index UTF-8 walks through a wrapper object.
            JsString(s) -> {
              let #(h, iter_ref) =
                common.alloc_wrapper(
                  state.heap,
                  value.StringIteratorObject(
                    remaining: string.to_utf_codepoints(s),
                  ),
                  state.builtins.array_iterator_proto,
                )
              Ok(
                State(
                  ..state,
                  stack: [JsObject(iter_ref), ..rest],
                  heap: h,
                  pc: state.pc + 1,
                ),
              )
            }
            _ ->
              state.throw_type_error(
                state,
                object.inspect(iterable, state.heap) <> " is not iterable",
              )
          }
        _ -> underflow(state, "GetIterator")
      }
    }

    GetAsyncIterator -> {
      case state.stack {
        [iterable, ..rest] ->
          case iterable {
            JsObject(ref) ->
              get_async_iterator_via_symbol(state, ref, iterable, rest)
            _ ->
              state.throw_type_error(
                state,
                object.inspect(iterable, state.heap) <> " is not async iterable",
              )
          }
        _ -> underflow(state, "GetAsyncIterator")
      }
    }

    // GetIteratorFromMethod step 4 (§7.4.4): cache the iterator's `next` in
    // an internal Iterator Record. The Get is observable and abrupt
    // completions propagate. Emitted after GetAsyncIterator for async yield*.
    IteratorRecord -> {
      case state.stack {
        [JsObject(iter_ref), ..rest] ->
          push_iterator_record(state, iter_ref, rest)
        [other, ..] ->
          state.throw_type_error(
            state,
            object.inspect(other, state.heap) <> " is not an object",
          )
        _ -> underflow(state, "IteratorRecord")
      }
    }

    IteratorNext -> {
      // [[Done]] tracking (QuickJS-style): on done=true OR .next() abrupt,
      // the iter slot becomes JsUndefined so subsequent IteratorNext
      // short-circuits and IteratorClose/CloseThrow no-op (§7.4.11/.6).
      let mark_done = fn(rest) {
        fn(e: #(StepResult, JsValue, State(host))) {
          let #(r, v, s) = e
          #(r, v, State(..s, stack: [JsUndefined, ..rest]))
        }
      }
      case state.stack {
        // Iter already exhausted/aborted — short-circuit to done.
        [JsUndefined, ..rest] ->
          Ok(
            State(
              ..state,
              stack: [JsBool(True), JsUndefined, JsUndefined, ..rest],
              pc: state.pc + 1,
            ),
          )
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            // index < 0 latches exhaustion: once the iterator reported done
            // it stays done even if the source later grows or its buffer is
            // resized — no witness re-validation (spec sets
            // [[IteratedObject]] to undefined, "generator already returned").
            Some(ObjectSlot(kind: ArrayIteratorObject(index:, ..), ..))
              if index < 0
            ->
              Ok(
                State(
                  ..state,
                  stack: [JsBool(True), JsUndefined, JsUndefined, ..rest],
                  pc: state.pc + 1,
                ),
              )
            Some(
              ObjectSlot(
                kind: ArrayIteratorObject(source:, index:, iter_kind:),
                ..,
              ) as slot,
            ) ->
              case heap.read(state.heap, source) {
                // Typed-array source — §23.1.5.1: re-validate the buffer
                // witness and re-read the element through the live backing
                // store each step, so mutation during iteration is observed
                // and detached/out-of-bounds views throw TypeError.
                Some(ObjectSlot(
                  kind: value.TypedArrayObject(
                    buffer:,
                    elem_kind:,
                    byte_offset:,
                    length:,
                  ),
                  ..,
                )) ->
                  case
                    object.typed_array_iter_length(
                      state.heap,
                      buffer,
                      elem_kind,
                      byte_offset,
                      length,
                    )
                  {
                    // The throw comes from `.next()` itself — ForIn/Of body
                    // evaluation must NOT close the iterator (§14.7.5.6
                    // step 6.a), so mark the iter slot done.
                    Error(msg) ->
                      state.throw_type_error(state, msg)
                      |> result.map_error(mark_done(rest))
                    Ok(len) ->
                      case index >= len {
                        True -> {
                          let heap =
                            advance_array_iter(state.heap, iter_ref, source, -1)
                          Ok(
                            State(
                              ..state,
                              stack: [
                                JsBool(True),
                                JsUndefined,
                                JsUndefined,
                                ..rest
                              ],
                              heap:,
                              pc: state.pc + 1,
                            ),
                          )
                        }
                        False -> {
                          let elem =
                            object.typed_array_element(
                              state.heap,
                              buffer,
                              elem_kind,
                              byte_offset,
                              len,
                              index,
                            )
                            |> option.unwrap(JsUndefined)
                          let heap =
                            heap.write(
                              state.heap,
                              iter_ref,
                              ObjectSlot(
                                ..slot,
                                kind: ArrayIteratorObject(
                                  source:,
                                  index: index + 1,
                                  iter_kind:,
                                ),
                              ),
                            )
                          let #(heap, val) =
                            array_iter_shape(
                              heap,
                              state,
                              iter_kind,
                              index,
                              elem,
                            )
                          Ok(
                            State(
                              ..state,
                              stack: [
                                JsBool(False),
                                val,
                                JsObject(iter_ref),
                                ..rest
                              ],
                              heap:,
                              pc: state.pc + 1,
                            ),
                          )
                        }
                      }
                  }
                // A throw from `.next()` itself (proxy trap / index accessor)
                // must NOT close the iterator (§14.7.5.6 step 6.a) — mark
                // the iter slot done so IteratorCloseThrow no-ops.
                _ ->
                  step_array_iterator_source(
                    state,
                    iter_ref,
                    source,
                    index,
                    iter_kind,
                    rest,
                  )
                  |> result.map_error(mark_done(rest))
              }
            Some(
              ObjectSlot(kind: value.StringIteratorObject(remaining:), ..) as slot,
            ) ->
              case remaining {
                [] ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(True), JsUndefined, JsUndefined, ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                [cp, ..remaining] -> {
                  let val = JsString(string.from_utf_codepoints([cp]))
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ObjectSlot(
                        ..slot,
                        kind: value.StringIteratorObject(remaining:),
                      ),
                    )
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
              }
            Some(ObjectSlot(kind: GeneratorObject(_), ..)) -> {
              // Generator iterator: internal resume returning #(done, value)
              // directly — no {value, done} result object is allocated.
              use #(done, val, next_state) <- result.map(
                generators.resume_generator_next(
                  state,
                  JsObject(iter_ref),
                  JsUndefined,
                  execute_inner,
                )
                |> result.map_error(mark_done(rest)),
              )
              let iter_slot = case done {
                True -> JsUndefined
                False -> JsObject(iter_ref)
              }
              State(
                ..next_state,
                stack: [JsBool(done), val, iter_slot, ..rest],
                pc: state.pc + 1,
              )
            }
            // Iterator Record (§7.4.1): `next` was cached at GetIterator —
            // call it directly, no per-iteration property re-resolution.
            Some(ObjectSlot(
              kind: value.IteratorRecordObject(iterated:, next_method:),
              ..,
            )) ->
              step_iterator_record(state, iter_ref, iterated, next_method, rest)
              |> result.map_error(mark_done(rest))
            // Generic iterator: any object with .next(). Call it, extract {value, done}.
            Some(ObjectSlot(..)) ->
              step_generic_iterator(state, iter_ref, rest)
              |> result.map_error(mark_done(rest))
            _ ->
              state.throw_type_error(
                state,
                object.inspect(JsObject(iter_ref), state.heap)
                  <> " is not an iterator",
              )
          }
        _ -> underflow(state, "IteratorNext")
      }
    }

    IteratorClose -> {
      // §7.4.11 normal-completion close. Stack: [iter, ..rest] → [..rest].
      // iter slot is JsUndefined when [[Done]] (set by IteratorNext) → no-op.
      case state.stack {
        [JsObject(_) as iter, ..rest] -> {
          // Resolve internal Iterator Records so GetMethod(iter, "return")
          // hits the real iterator object.
          let iter = unwrap_iterator_record(state, iter)
          let state = State(..state, stack: rest, pc: state.pc + 1)
          case builtins_iterator.iterator_close_normal(state, iter) {
            #(state, Ok(Nil)) -> Ok(state)
            #(state, Error(thrown)) -> Error(#(Thrown, thrown, state))
          }
        }
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "IteratorClose")
      }
    }

    IteratorCloseThrow -> {
      // §7.4.11 throw-completion close. Stack: [thrown, iter, ..] → rethrows.
      // unwind_to_catch truncated to PushTry-time depth (iter on top) then
      // pushed thrown, so layout is guaranteed. iter slot is JsUndefined when
      // [[Done]] (set by IteratorNext on done/abrupt) → skip .return().
      case state.stack {
        [thrown, JsObject(_) as iter, ..rest] -> {
          let iter = unwrap_iterator_record(state, iter)
          let state = State(..state, stack: rest)
          // Original error wins regardless of what .return() does.
          let #(state, _inner) = builtins_iterator.call_return(state, iter)
          Error(#(Thrown, thrown, state))
        }
        [thrown, _, ..rest] ->
          Error(#(Thrown, thrown, State(..state, stack: rest)))
        _ -> underflow(state, "IteratorCloseThrow")
      }
    }

    IteratorRest -> {
      // §13.15.5.3 / §14.3.3 rest element. [iter, ..rest] → [arr, ..rest].
      // Drains via .next() loop without re-GetIterator. Emitter pops the
      // close-guard try frame before this op so a .next() throw propagates
      // without IteratorClose (spec: [[Done]]=true → no close on rest abrupt).
      case state.stack {
        [JsObject(_) as iter, ..rest] -> {
          let iter = unwrap_iterator_record(state, iter)
          let state = State(..state, stack: rest, pc: state.pc + 1)
          case builtins_iterator.iterator_rest(state, iter) {
            #(state, Ok(arr)) -> Ok(State(..state, stack: [arr, ..rest]))
            #(state, Error(thrown)) -> Error(#(Thrown, thrown, state))
          }
        }
        // [[Done]] sentinel — a prior IteratorNext exhausted/aborted the
        // iterator and undef'd the slot. Rest result is an empty array.
        [_, ..rest] -> {
          let #(heap, ref) =
            common.alloc_array(state.heap, [], state.builtins.array.prototype)
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        [] -> underflow(state, "IteratorRest")
      }
    }

    IteratorCheckObject ->
      // §7.4.12 step 6 / §14.7.5.6 step 6.c: awaited iterator result must be
      // an Object. Peeks; leaves the value on the stack on success.
      case state.stack {
        [JsObject(_), ..] -> Ok(State(..state, pc: state.pc + 1))
        [_, ..] ->
          state.throw_type_error(state, "Iterator result is not an object")
        [] -> underflow(state, "IteratorCheckObject")
      }

    // ---- Generator/async ---------------------------------------------
    InitialYield ->
      // Suspend immediately at start of generator body.
      // PC advances past InitialYield so resumption starts at the next op.
      Error(#(Yielded, JsUndefined, state))

    Yield -> {
      // Pop value from stack and suspend the generator.
      // On resume, .next(arg) value will be pushed onto the stack.
      case state.stack {
        [yielded_value, ..] -> Error(#(Yielded, yielded_value, state))
        [] -> Error(#(Yielded, JsUndefined, state))
      }
    }

    YieldStar -> {
      // Self-looping delegate: [arg, iter, ..rest]. Calls iter.next(arg).
      // done → push value, pc+1. !done → yield value; execute_inner keeps pc
      // here so next resume re-enters with [resume_val, iter].
      case state.stack {
        [arg, JsObject(orig_ref), ..rest] -> {
          // GetIterator may have pushed an internal Iterator Record (cached
          // `next`). Delegation forwards next/throw/return dynamically off
          // the saved stack slot, so swap in the real iterator up front.
          let #(iter_ref, state) = case heap.read(state.heap, orig_ref) {
            Some(ObjectSlot(
              kind: value.IteratorRecordObject(iterated: JsObject(real), ..),
              ..,
            )) -> #(real, State(..state, stack: [arg, JsObject(real), ..rest]))
            _ -> #(orig_ref, state)
          }
          let iter = JsObject(iter_ref)
          use #(next_fn, state) <- result.try(
            state.rethrow(object.get_value(state, iter_ref, Named("next"), iter)),
          )
          use #(done, val, state) <- result.try(
            case is_native_generator_next(state.heap, next_fn) {
              // Fast path: unmodified native generator .next — resume the inner
              // generator directly, skipping the {value, done} result object
              // that would be allocated and immediately discarded per step.
              // resume advances pc past YieldStar; restore it so the !done
              // branch suspends at this op (resume loops back here).
              True -> {
                let pc = state.pc
                use #(done, val, st) <- result.map(
                  generators.resume_generator_next(
                    state,
                    iter,
                    arg,
                    execute_inner,
                  ),
                )
                #(done, val, State(..st, pc:))
              }
              False -> {
                use #(res, state) <- result.try(
                  state.rethrow(state.call(state, next_fn, iter, [arg])),
                )
                builtins_iterator.read_iter_result(state, res)
              }
            },
          )
          case done {
            True -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            False ->
              // execute_inner's YieldStar arm strips arg from the
              // original stack and keeps pc here, so resume loops back
              // with [resume_val, iter, ..rest].
              Error(#(Yielded, val, state))
          }
        }
        _ -> underflow(state, "YieldStar")
      }
    }

    AsyncYieldStarNext ->
      // [arg, iter, ..rest]. Call iter.next(arg), replace arg with result →
      // [result, iter, ..rest], pc+1. The following Await op suspends on it.
      // The iter slot is an internal Iterator Record (cached [[NextMethod]],
      // §7.4.3 IteratorNext) when emitted via IrIteratorRecord; a bare
      // iterator object otherwise.
      case state.stack {
        [arg, JsObject(iter_ref) as iter, ..rest] -> {
          use #(next_fn, this, state) <- result.try(
            case heap.read(state.heap, iter_ref) {
              Some(ObjectSlot(
                kind: value.IteratorRecordObject(iterated:, next_method:),
                ..,
              )) -> Ok(#(next_method, iterated, state))
              _ -> {
                use #(next_fn, state) <- result.map(
                  state.rethrow(object.get_value(
                    state,
                    iter_ref,
                    Named("next"),
                    iter,
                  )),
                )
                #(next_fn, iter, state)
              }
            },
          )
          use #(res, state) <- result.try(
            state.rethrow(state.call(state, next_fn, this, [arg])),
          )
          Ok(State(..state, stack: [res, iter, ..rest], pc: state.pc + 1))
        }
        _ -> underflow(state, "AsyncYieldStarNext")
      }

    AsyncYieldStarResume(_) ->
      // [result_obj, iter, ..rest]. done? → push value, pc+1 : Yielded(value).
      case state.stack {
        [res, _iter, ..rest] -> {
          use #(done, val, state) <- result.try(
            builtins_iterator.read_iter_result(state, res),
          )
          case done {
            True -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            False -> Error(#(Yielded, val, state))
          }
        }
        _ -> underflow(state, "AsyncYieldStarResume")
      }

    Await -> {
      // Pop the awaited value from the stack and suspend the async function.
      case state.stack {
        [awaited_value, ..] -> Error(#(Awaited, awaited_value, state))
        [] -> Error(#(Awaited, JsUndefined, state))
      }
    }

    // ---- Special -----------------------------------------------------
    CreateArguments(simple_params:) -> {
      // Allocate an unmapped arguments object from state.call_args.
      let args = state.call_args
      let length = list.length(args)
      let callee = read_lexical_local(state, opcode.RefActiveFunc)
      // §10.2.11 step 20: an UNMAPPED arguments object is created when the
      // function is strict OR its parameter list is non-simple (defaults,
      // destructuring, rest). §10.4.4.7 CreateUnmappedArgumentsObject step 8:
      // its "callee" is the %ThrowTypeError% accessor (non-enumerable,
      // non-configurable) — reuse the restricted accessor's getter installed
      // on Function.prototype ("arguments"), same function identity
      // (test262: built-ins/ThrowTypeError/unique-per-realm-non-simple.js).
      // Sloppy functions with simple parameter lists get the MAPPED form,
      // whose "callee" is a writable data property holding the function.
      // Build "length" BEFORE "callee": §10.4.4.6/§10.4.4.7 define length
      // first, and [[OwnPropertyKeys]] orders named keys by creation seq.
      let length_prop =
        value.data(value.from_int(length))
        |> value.writable
        |> value.configurable
      let callee_prop = case state.func.is_strict || !simple_params {
        True ->
          case
            object.get_own_property(
              state.heap,
              state.builtins.function.prototype,
              Named("arguments"),
            )
          {
            Some(value.AccessorProperty(get:, set:, ..)) ->
              value.accessor(get:, set:, enumerable: False, configurable: False)
            _ -> value.data(callee) |> value.writable |> value.configurable
          }
        False -> value.data(callee) |> value.writable |> value.configurable
      }
      let props =
        dict.from_list([
          #(Named("length"), length_prop),
          #(Named("callee"), callee_prop),
        ])
      // §10.4.4.6: [@@iterator] = %Array.prototype.values%
      let sym_props = case
        heap.read(state.heap, state.builtins.array.prototype)
      {
        Some(ObjectSlot(symbol_properties: arr_syms, ..)) ->
          case list.key_find(arr_syms, value.symbol_iterator) {
            Ok(values_fn) -> [#(value.symbol_iterator, values_fn)]
            Error(Nil) -> []
          }
        _ -> []
      }
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: value.ArgumentsObject(length:),
            properties: props,
            elements: elements.from_list(args),
            prototype: Some(state.builtins.object.prototype),
            symbol_properties: sym_props,
            extensible: True,
          ),
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    CreateRestArray(from_index) -> {
      // §10.4.4 rest parameter: a plain Array of the call args from
      // `from_index` onward (the params before the rest are bound positionally).
      let rest_args = list.drop(state.call_args, from_index)
      let #(heap, ref) =
        common.alloc_array(
          state.heap,
          rest_args,
          state.builtins.array.prototype,
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    // -- RegExp literal --
    NewRegExp -> {
      case state.stack {
        [JsString(flags), JsString(pattern), ..rest] -> {
          let #(heap, ref) =
            builtins_regexp.alloc_regexp(
              state.heap,
              state.builtins.regexp.prototype,
              pattern,
              flags,
            )
          Ok(
            State(
              ..state,
              stack: [JsObject(ref), ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
        _ -> underflow(state, "NewRegExp")
      }
    }

    // -- Dynamic import (§13.3.10 ImportCall) --
    opcode.DynamicImport ->
      case state.stack {
        [options, specifier, ..rest] ->
          dynamic_import.evaluate_import_call(state, specifier, options, rest)
        _ -> underflow(state, "DynamicImport")
      }

    // import.source(specifier): Source Text Module Records have no source
    // phase representation, so a coercible specifier rejects with SyntaxError.
    opcode.DynamicImportSource ->
      case state.stack {
        [specifier, ..rest] ->
          dynamic_import.evaluate_source_import_call(state, specifier, rest)
        _ -> underflow(state, "DynamicImportSource")
      }

    // import.defer(specifier): load + link the requested graph without
    // evaluating it; the promise resolves with the Deferred Module Namespace,
    // whose first relevant access triggers evaluation.
    opcode.DynamicImportDefer ->
      case state.stack {
        [specifier, ..rest] ->
          dynamic_import.evaluate_defer_import_call(state, specifier, rest)
        _ -> underflow(state, "DynamicImportDefer")
      }
  }
}

fn lookup_eval_env(state: State(host), name: String) -> Option(JsValue) {
  option.then(state.eval_env, heap.read_eval_env(state.heap, _))
  |> option.then(fn(vars) { dict.get(vars, name) |> option.from_result })
}

/// §9.1.1.2.1 Object Environment Record HasBinding(N) for a `with` scope:
/// HasProperty(obj, N), then — since with-environments always have
/// [[IsWithEnvironment]] = true — Get(obj, @@unscopables) and treat a truthy
/// Get(unscopables, N) as "not bound". Both Gets can run user code (getters,
/// proxy traps); errors propagate as thrown completions.
fn with_has_binding(
  state: State(host),
  ref: value.Ref,
  name: String,
) -> Result(#(Bool, State(host)), #(state.StepResult, JsValue, State(host))) {
  use #(found, state) <- result.try(
    state.rethrow(object.has_property_stateful(
      state,
      ref,
      object.PkString(Named(name)),
    )),
  )
  case found {
    False -> Ok(#(False, state))
    True -> {
      use #(unscopables, state) <- result.try(
        state.rethrow(object.get_symbol_value_of(
          state,
          JsObject(ref),
          value.symbol_unscopables,
        )),
      )
      case unscopables {
        JsObject(_) -> {
          use #(blocked, state) <- result.map(
            state.rethrow(object.get_value_of(state, unscopables, Named(name))),
          )
          #(!value.is_truthy(blocked), state)
        }
        _ -> Ok(#(True, state))
      }
    }
  }
}

/// Re-materialize an already-converted PropertyKey as a JsValue whose
/// re-conversion through to_property_key is side-effect-free and yields the
/// same key. GetElem2 leaves this on the stack so the later PutElem does not
/// re-run a user-observable ToPropertyKey (§13.15.2: ToPropertyKey once).
fn property_key_value(pk: value.PropertyKey) -> JsValue {
  case pk {
    value.Index(n) -> value.from_int(n)
    value.Named(s) -> JsString(s)
  }
}

/// GetElem on a primitive receiver — ToPropertyKey (Symbol → symbol lookup
/// on prototype, else ToString → string lookup) then delegate to get_value_of.
fn get_elem_on_primitive(
  state: State(host),
  receiver: JsValue,
  key: JsValue,
) -> Result(#(JsValue, State(host)), #(JsValue, State(host))) {
  case key {
    value.JsSymbol(sym) -> object.get_symbol_value_of(state, receiver, sym)
    _ -> {
      use #(pk, state) <- result.try(property.to_property_key(state, key))
      object.get_value_of(state, receiver, pk)
    }
  }
}

// ============================================================================
// Call helpers
// ============================================================================

/// ES2024 §10.2.1.2 OrdinaryCallBindThis ( F, thisArgument )
///
/// The abstract operation OrdinaryCallBindThis binds the `this` value for an
/// ordinary function call based on the function's [[ThisMode]] internal slot.
///
/// Spec steps:
///   1. Let thisMode be F.[[ThisMode]].
///   2. If thisMode is LEXICAL, return unused.
///   3. Let calleeRealm be F.[[Realm]].
///   4. Let localEnv be the LexicalEnvironment of calleeContext.
///   5. If thisMode is STRICT, let thisValue be thisArgument.
///   6. Else (sloppy),
///      a. If thisArgument is undefined or null, then
///         i. Let globalEnv be calleeRealm.[[GlobalEnv]].
///         ii. Let thisValue be globalEnv.[[GlobalThisValue]].
///      b. Else, let thisValue be ! ToObject(thisArgument).
///   7-8. (Assertions about localEnv — not applicable here.)
///   9. Perform ! localEnv.BindThisValue(thisValue).
///   10. Return unused.
///
/// Our implementation threads the returned thisValue into the new call frame
/// directly via call_function. Arrow functions use is_arrow instead of
/// [[ThisMode]] = LEXICAL, capturing the enclosing frame's `this` slot.
/// Thin wrapper: delegates to call.call_function with execute_inner/unwind_to_catch.
fn call_function(
  state: State(host),
  fn_ref: value.Ref,
  env_ref: value.Ref,
  home_object: option.Option(value.Ref),
  callee_template: FuncTemplate,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  constructor_this: option.Option(JsValue),
  new_target: JsValue,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  call.call_function(
    state,
    fn_ref,
    env_ref,
    home_object,
    callee_template,
    args,
    rest_stack,
    this_val,
    constructor_this,
    new_target,
    execute_inner,
    unwind_to_catch,
  )
}

/// Thin wrapper: delegates to call.call_native with execute_inner/unwind_to_catch/dispatch_native.
fn call_native(
  state: State(host),
  native: NativeFnSlot(host),
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  call.call_native(
    state,
    native,
    args,
    rest_stack,
    this,
    execute_inner,
    unwind_to_catch,
    dispatch_native,
  )
}

/// §15.4.4 MakeMethod(F, homeObject): if `func` is a JS closure, set its
/// [[HomeObject]] = `target` so `super.x` inside it resolves via the
/// home object's prototype. No-op for non-closures (native, bound, etc.).
/// DefinePropertyOrThrow guard for class element definition: §10.1.6.3
/// ValidateAndApplyPropertyDescriptor rejects redefining an existing
/// non-configurable own property (the descriptors class bodies produce
/// always differ from it — fresh closure values / accessor-vs-data). The
/// only collision reachable from a class body is the constructor's own
/// "prototype" via a computed key.
/// DefineField receivers that need the full [[DefineOwnProperty]] path:
/// proxies (defineProperty trap, §10.5.6) and non-extensible/frozen objects
/// (CreateDataPropertyOrThrow → TypeError). Private-namespace keys never go
/// through traps (§7.3.28 PrivateFieldAdd is not an ordinary define).
fn define_needs_full_semantics(
  state: State(host),
  ref: value.Ref,
  key: value.PropertyKey,
) -> Bool {
  case value.is_private_name(key) {
    True -> False
    False ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.ProxyObject(..), ..)) -> True
        Some(ObjectSlot(extensible:, ..)) -> !extensible
        _ -> False
      }
  }
}

/// §7.3.32 DefineField slow path — CreateDataPropertyOrThrow through the
/// real [[DefineOwnProperty]]: proxy traps fire; a false result (frozen /
/// non-extensible receiver, trap refusal) throws TypeError.
fn define_field_full(
  state: State(host),
  ref: value.Ref,
  key: JsValue,
  val: JsValue,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let #(heap, desc_ref) =
    common.alloc_pojo(state.heap, state.builtins.object.prototype, [
      #("value", value.data_property(val)),
      #("writable", value.data_property(value.JsBool(True))),
      #("enumerable", value.data_property(value.JsBool(True))),
      #("configurable", value.data_property(value.JsBool(True))),
    ])
  let state = State(..state, heap:)
  use #(state, ok) <- result.try(
    state.rethrow(builtins_object.define_property_bool(
      state,
      ref,
      key,
      desc_ref,
    )),
  )
  case ok {
    True -> Ok(state)
    False -> {
      let name = case key {
        JsString(n) -> n
        _ -> "[computed]"
      }
      state.throw_type_error(state, "Cannot define property " <> name)
    }
  }
}

fn check_define_nonconfigurable(
  state: State(host),
  ref: value.Ref,
  pk: value.PropertyKey,
) -> Result(Nil, #(StepResult, JsValue, State(host))) {
  case heap.read(state.heap, ref) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.get(properties, pk) {
        Ok(value.DataProperty(configurable: False, ..))
        | Ok(value.AccessorProperty(configurable: False, ..)) ->
          state.throw_type_error(
            state,
            "Cannot redefine property: " <> value.key_to_string(pk),
          )
        _ -> Ok(Nil)
      }
    _ -> Ok(Nil)
  }
}

/// Shared guard for the DefinePrivate* ops (§7.3.28 PrivateFieldAdd /
/// §7.3.29 PrivateMethodOrAccessorAdd): TypeError if the receiver already
/// has the private element (return-override double initialization) or is
/// non-extensible (proposal nonextensible-applies-to-private).
fn check_private_add(
  state: State(host),
  ref: value.Ref,
  key_text: String,
) -> Result(Nil, #(state.StepResult, JsValue, State(host))) {
  case object.get_own_property(state.heap, ref, Named(key_text)) {
    Some(_) ->
      state.throw_type_error(
        state,
        "Cannot initialize "
          <> value.private_display_name(key_text)
          <> " twice on the same object",
      )
    None ->
      case object.slot_extensible(state.heap, ref) {
        False ->
          state.throw_type_error(
            state,
            "Cannot define private member "
              <> value.private_display_name(key_text)
              <> " on a non-extensible object",
          )
        True -> Ok(Nil)
      }
  }
}

fn make_method(h: Heap(host), func: JsValue, target: Ref) -> Heap(host) {
  case func {
    JsObject(fn_ref) -> {
      use slot <- heap.update(h, fn_ref)
      case slot {
        ObjectSlot(kind: FunctionObject(..) as fo, ..) ->
          ObjectSlot(
            ..slot,
            kind: FunctionObject(..fo, home_object: Some(target)),
          )
        _ -> slot
      }
    }
    _ -> h
  }
}

/// §10.2.9 SetFunctionName for runtime-computed method/accessor keys: the
/// closure was compiled anonymous (key unknown at compile time), so its
/// "name" is set here from the evaluated propKey, with the accessor
/// "get "/"set " prefix where applicable.
fn set_computed_fn_name(
  h: Heap(host),
  func: JsValue,
  prefix: String,
  name: String,
) -> Heap(host) {
  case func {
    JsObject(fn_ref) -> {
      use slot <- heap.update(h, fn_ref)
      case slot {
        ObjectSlot(kind: FunctionObject(..), properties:, ..) ->
          ObjectSlot(
            ..slot,
            properties: dict.insert(
              properties,
              Named("name"),
              // Redefining an existing "name" keeps its creation seq
              // (§10.1.11 — the key keeps its enumeration position).
              case dict.get(properties, Named("name")) {
                Ok(old) ->
                  value.with_seq_of(
                    common.fn_name_property(prefix <> name),
                    old,
                  )
                Error(Nil) -> common.fn_name_property(prefix <> name)
              },
            ),
          )
        _ -> slot
      }
    }
    _ -> h
  }
}

/// SetFunctionName step 4: a Symbol key names the function "[description]",
/// or "" when the symbol has no description.
fn symbol_fn_name(state: State(host), sym: value.SymbolId) -> String {
  let desc = case value.well_known_symbol_description(sym) {
    Some(d) -> Ok(d)
    None -> dict.get(state.ctx.symbol_descriptions, sym)
  }
  case desc {
    Ok(d) -> "[" <> d <> "]"
    Error(Nil) -> ""
  }
}

/// The accessor-name prefix for SetFunctionName ("get "/"set ").
fn accessor_name_prefix(kind: opcode.AccessorKind) -> String {
  case kind {
    opcode.Getter -> "get "
    opcode.Setter -> "set "
  }
}

/// Thin wrapper: delegates to call.do_construct with execute_inner/unwind_to_catch/dispatch_native.
fn do_construct(
  state: State(host),
  ctor_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
  new_target_ref: Ref,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  call.do_construct(
    state,
    ctor_ref,
    args,
    rest_stack,
    new_target_ref,
    execute_inner,
    unwind_to_catch,
    dispatch_native,
  )
}

/// Thin wrapper: delegates to call.call_value with execute_inner/unwind_to_catch/dispatch_native.
fn call_value(
  state: State(host),
  callee: JsValue,
  args: List(JsValue),
  this_val: JsValue,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  call.call_value(
    state,
    callee,
    args,
    this_val,
    execute_inner,
    unwind_to_catch,
    dispatch_native,
  )
}

/// Thin wrapper: delegates to array_ops.spread_into_array with execute_inner.
fn spread_into_array(
  state: State(host),
  target_ref: Ref,
  iterable: JsValue,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  array_ops.spread_into_array(state, target_ref, iterable, execute_inner)
}

fn dispatch_native(
  native: value.NativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State(host),
) -> #(State(host), Result(JsValue, JsValue)) {
  call.dispatch_native(native, args, this, state, execute_inner, new_state)
}

/// Get the Ref of a named property's JsObject value from a heap object.
/// Returns Error(Nil) if the object doesn't exist, the property is missing,
/// or the property value is not a JsObject.
fn get_field_ref(
  h: Heap(host),
  obj_ref: value.Ref,
  name: String,
) -> Option(value.Ref) {
  use slot <- option.then(heap.read(h, obj_ref))
  case slot {
    ObjectSlot(properties: props, ..) ->
      case dict.get(props, Named(name)) {
        Ok(DataProperty(value: JsObject(ref), ..)) -> Some(ref)
        _ -> None
      }
    _ -> None
  }
}

/// Update the prototype of a heap object in-place, returning the new heap.
/// If the ref doesn't point to an ObjectSlot, returns the heap unchanged.
fn set_slot_prototype(
  h: Heap(host),
  ref: value.Ref,
  new_proto: option.Option(value.Ref),
) -> Heap(host) {
  use slot <- heap.update(h, ref)
  case slot {
    ObjectSlot(..) -> ObjectSlot(..slot, prototype: new_proto)
    _ -> slot
  }
}

/// Partition raw JsValue keys (from ObjectRestCopy stack) into PropertyKey
/// (string/index) and SymbolId sets for CopyDataProperties exclusion.
/// Non-symbol keys go through ToPropertyKey (§7.1.19) for canonical form.
fn build_exclusion_sets(
  state: State(host),
  keys: List(JsValue),
) -> Result(
  #(set.Set(value.PropertyKey), set.Set(value.SymbolId), State(host)),
  #(JsValue, State(host)),
) {
  use #(pks, syms, state), key <- list.try_fold(keys, #(
    set.new(),
    set.new(),
    state,
  ))
  case key {
    value.JsSymbol(id) -> Ok(#(pks, set.insert(syms, id), state))
    _ -> {
      use #(pk, state) <- result.map(property.to_property_key(state, key))
      #(set.insert(pks, pk), syms, state)
    }
  }
}

/// §15.10 Tail Position Calls (IsInTailPosition). A Call/CallMethod is a
/// tail call when:
///   - the very next opcode is Return (the shape `return f(...)` compiles
///     to; tagged templates lower to CallExpression so they qualify too,
///     per HasCallInTailPosition §15.10.2),
///   - the caller is strict code (§15.10.1 step 2),
///   - no try handlers are active in the caller (HasCallInTailPosition
///     returns false inside `try` and `finally` blocks; an active handler
///     means the callee must unwind through this frame),
///   - the current frame is a plain [[Call]] invocation — constructor
///     frames (new_target set) carry return-value fixup that reads
///     state.func/locals of the *returning* function, so they can't be
///     elided (Return's derived-constructor checks),
///   - the callee runs on the VM frame stack: generators/async functions
///     allocate isolated states instead of pushing a frame, so there is
///     no frame to elide. (Generator/async *caller* bodies execute with
///     call_stack == [] at body level, so the non-empty-call-stack guard
///     also implements §15.10.1 steps 5-7 — their returns never elide.)
fn is_tail_call(state: State(host), callee: FuncTemplate) -> Bool {
  let frame_eligible = case state.try_stack, state.call_stack {
    [], [_, ..] ->
      state.func.is_strict
      && !callee.is_generator
      && !callee.is_async
      && state.new_target == JsUndefined
    _, _ -> False
  }
  case frame_eligible {
    False -> False
    True ->
      case tuple_array.unsafe_get(state.pc + 1, state.code) {
        Return -> True
        _ -> False
      }
  }
}

/// §15.10.3 PrepareForTailCall: pop the running execution context before
/// the call. call_regular_function has just pushed the caller's SavedFrame;
/// discard it so the callee's Return goes straight to the caller's caller
/// and tail recursion runs at constant call depth. Only reached when
/// is_tail_call held, which guarantees the Ok state came from
/// call_regular_function (plain function callee), which always pushes
/// exactly one frame.
fn elide_tail_frame(
  result: Result(State(host), #(StepResult, JsValue, State(host))),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  use new_state <- result.map(result)
  case new_state.call_stack {
    [_caller_frame, ..rest_frames] ->
      State(
        ..new_state,
        call_stack: rest_frames,
        call_depth: new_state.call_depth - 1,
      )
    [] -> new_state
  }
}

/// Pop n items from stack. Returns #(popped_items_in_order, remaining_stack).
fn pop_n(
  stack: List(JsValue),
  n: Int,
) -> Option(#(List(JsValue), List(JsValue))) {
  pop_n_loop(stack, n, [])
}

fn pop_n_loop(
  stack: List(JsValue),
  remaining: Int,
  acc: List(JsValue),
) -> Option(#(List(JsValue), List(JsValue))) {
  case remaining {
    0 -> Some(#(acc, stack))
    _ ->
      case stack {
        [top, ..rest] -> pop_n_loop(rest, remaining - 1, [top, ..acc])
        [] -> None
      }
  }
}

/// BinOp Add with ToPrimitive for object operands.
/// ES2024 §13.15.3: ToPrimitive(default) both sides, then string-concat or numeric-add.
fn binop_direct(
  state: State(host),
  kind: opcode.BinOpKind,
  left: JsValue,
  right: JsValue,
  rest: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case operators.exec_binop(kind, left, right) {
    Ok(result) -> Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
    Error(err) -> throw_operator_error(state, err)
  }
}

/// In-run heap GC trigger threshold: fresh slot ids allocated since the last
/// collection. High enough that short programs (unit tests, test262 cases)
/// never trigger it; long allocation-heavy scripts collect at top-level
/// returns so dead objects stop inflating the BEAM process's live set (which
/// every major Erlang GC must copy).
const gc_growth_threshold = 65_536

/// Mark-and-sweep the JS heap when returning to the outermost frame of an
/// allocation-heavy script. Gated very conservatively — every VM-global
/// channel that could hold heap refs outside this State (pending jobs,
/// timers, atomics waiters, extra realms, in-flight host promises, recorded
/// unhandled rejections, re-entrant drives) disables collection, so the root
/// set is exactly: this frame's stack/locals/args + lexical globals + the
/// persistent root set (builtins, global object, module cells, template
/// objects — all rooted at creation).
fn maybe_collect_at_toplevel(state: State(host)) -> State(host) {
  let eligible =
    state.call_stack == []
    // call_depth > 0 here means this is the bottom frame of a re-entrant
    // drive (callback / generator body), where the SUSPENDED PARENT
    // interpreter holds heap refs invisible to this State — never collect.
    && state.call_depth == 0
    && heap.grown_since_collect(state.heap) > gc_growth_threshold
    && dict.size(state.ctx.realms) == 0
    && state.atomics_waiters == []
    && state.outstanding == 0
    && state.unhandled_rejections == []
    && option.is_none(job_queue.pop(state.job_queue))
  case eligible {
    False -> state
    True ->
      State(..state, heap: heap.compact(state.heap, state_root_ids(state)))
  }
}

/// Heap refs reachable from this State but not in the heap's persistent root
/// set: operand stack, locals, current args/new.target, the frame's sloppy
/// direct-eval env, and global let/const bindings. call_stack is empty at the
/// (gated) call site, so saved frames need no scan.
fn state_root_ids(state: State(host)) -> set.Set(Int) {
  let acc = value_root_ids(state.stack, [state.ctx.global_object.id])
  let acc = value_root_ids(tuple_array.to_list(state.locals), acc)
  let acc = value_root_ids([state.new_target, ..state.call_args], acc)
  let acc = case state.eval_env {
    Some(ref) -> [ref.id, ..acc]
    None -> acc
  }
  let acc =
    dict.fold(state.ctx.lexical_globals, acc, fn(a, _name, global) {
      case value.lexical_global_value(global) {
        JsObject(ref) -> [ref.id, ..a]
        _ -> a
      }
    })
  set.from_list(acc)
}

fn value_root_ids(values: List(JsValue), acc: List(Int)) -> List(Int) {
  case values {
    [] -> acc
    [JsObject(ref), ..rest] -> value_root_ids(rest, [ref.id, ..acc])
    [_, ..rest] -> value_root_ids(rest, acc)
  }
}

/// Same ReferenceError the GetLocal step arm throws for a TDZ read — the
/// fused ops fold a GetLocal, so their TDZ path must be indistinguishable.
fn tdz_reference_error(
  state: State(host),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  state.throw_reference_error(
    state,
    "Cannot access variable before initialization (TDZ)",
  )
}

/// Unwrap a step-helper result that pushed exactly one value onto the stack
/// passed to it: return that value and the state with it popped again.
fn pop_top(
  r: Result(State(host), #(StepResult, JsValue, State(host))),
  op: String,
) -> Result(#(JsValue, State(host)), #(StepResult, JsValue, State(host))) {
  use state <- result.try(r)
  case state.stack {
    [top, ..rest] -> Ok(#(top, State(..state, stack: rest)))
    [] -> Error(#(StepVmError(StackUnderflow(op)), JsUndefined, state))
  }
}

/// Fused statement-position postfix update (IncLocal/DecLocal) — semantics
/// are exactly the folded sequence GetLocal; UnaryOp(Pos); Dup; PushConst(1);
/// BinOp(Add|Sub); PutLocal; Pop. Reuses the same coercion helpers as the
/// unfused ops so every ToPrimitive call and thrown error is identical.
fn fused_update_local(
  state: State(host),
  index: Int,
  kind: opcode.BinOpKind,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let next_pc = state.pc + 1
  case tuple_array.unsafe_get(index, state.locals) {
    JsUninitialized -> tdz_reference_error(state)
    v -> {
      // UnaryOp(Pos): ToNumber, via ToPrimitive for objects — mirrors the
      // UnaryOp step arm's operand split.
      use #(n, state) <- result.try(case v {
        JsObject(_) ->
          pop_top(
            unaryop_with_to_primitive(state, opcode.Pos, v, state.stack),
            "IncLocal",
          )
        _ ->
          case operators.exec_unaryop(opcode.Pos, v) {
            Ok(n) -> Ok(#(n, state))
            Error(err) -> throw_operator_error(state, err)
          }
      })
      // BinOp(Add|Sub) with the constant 1 — n is already a primitive, so
      // Add goes through add_primitives and Sub through binop_direct,
      // exactly like the BinOp step arm would route them.
      use #(result, state) <- result.try(case kind {
        Add ->
          pop_top(add_primitives(state, n, number_one, state.stack), "IncLocal")
        _ ->
          pop_top(
            binop_direct(state, kind, n, number_one, state.stack),
            "IncLocal",
          )
      })
      let locals = tuple_array.set_unchecked(index, result, state.locals)
      Ok(State(..state, locals:, pc: next_pc))
    }
  }
}

/// Fused compare-and-branch (CmpLocal*Jump) — semantics are exactly the
/// folded sequence GetLocal(s)/PushConst; BinOp(kind); JumpIfFalse(target)
/// for the pure relational kinds (Lt/LtEq/Gt/GtEq): binop_direct for
/// primitives, binop_with_to_primitive when an operand is an object.
fn fused_cmp_jump(
  state: State(host),
  kind: opcode.BinOpKind,
  left: JsValue,
  right: JsValue,
  target: Int,
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let next_pc = state.pc + 1
  use #(result, state) <- result.try(case left, right {
    JsObject(_), _ | _, JsObject(_) ->
      pop_top(
        binop_with_to_primitive(state, kind, left, right, state.stack),
        "CmpLocalJump",
      )
    _, _ ->
      pop_top(
        binop_direct(state, kind, left, right, state.stack),
        "CmpLocalJump",
      )
  })
  case value.is_truthy(result) {
    True -> Ok(State(..state, pc: next_pc))
    False -> Ok(State(..state, pc: target))
  }
}

/// Throw the error class an operator/coercion failure names. The constructor
/// is chosen at the throw site in ops/operators, so the mapping is exhaustive:
/// a new OpError variant is a compile error here, not a silently-wrong throw.
fn throw_operator_error(
  state: State(host),
  err: operators.OpError,
) -> Result(a, #(StepResult, JsValue, State(host))) {
  case err {
    operators.OpRangeError(msg) -> state.throw_range_error(state, msg)
    operators.OpTypeError(msg) -> state.throw_type_error(state, msg)
  }
}

/// §7.2.14 IsLooselyEqual: ToPrimitive fires only for object × {Number,
/// String, BigInt, Symbol, Bool}. Object×object and object×nullish go
/// straight to abstract_equal.
fn is_eq_coercible(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    JsObject(_), JsObject(_) -> False
    JsObject(_), JsNull | JsObject(_), JsUndefined -> False
    JsNull, JsObject(_) | JsUndefined, JsObject(_) -> False
    JsObject(_), _ | _, JsObject(_) -> True
    _, _ -> False
  }
}

/// ES2024 §13.15.4 / §7.2.14: ToPrimitive both operands before delegating
/// to the pure operator. The fast path already short-circuits on
/// primitive×primitive. Relational/numeric ops use number hint; loose
/// equality uses default hint (matters for Date @@toPrimitive).
fn binop_with_to_primitive(
  state: State(host),
  kind: opcode.BinOpKind,
  left: JsValue,
  right: JsValue,
  rest: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let hint = case kind {
    opcode.Eq | opcode.NotEq -> coerce.DefaultHint
    _ -> coerce.NumberHint
  }
  use #(lprim, s1) <- result.try(
    state.rethrow(coerce.to_primitive(state, left, hint)),
  )
  use #(rprim, s2) <- result.try(
    state.rethrow(coerce.to_primitive(s1, right, hint)),
  )
  case operators.exec_binop(kind, lprim, rprim) {
    Ok(result) -> Ok(State(..s2, stack: [result, ..rest], pc: state.pc + 1))
    Error(err) -> throw_operator_error(s2, err)
  }
}

/// ES2024 §13.5.4/5/6: numeric unary ops call ToNumber → ToPrimitive on
/// object operands. LogicalNot/Void are handled in the fast path (they do
/// not coerce).
fn unaryop_with_to_primitive(
  state: State(host),
  kind: opcode.UnaryOpKind,
  operand: JsValue,
  rest: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  use #(prim, s1) <- result.try(
    state.rethrow(coerce.to_primitive(state, operand, coerce.NumberHint)),
  )
  case operators.exec_unaryop(kind, prim) {
    Ok(result) -> Ok(State(..s1, stack: [result, ..rest], pc: state.pc + 1))
    Error(err) -> throw_operator_error(s1, err)
  }
}

/// ES2024 §13.15.4 step 2–7: apply `+` to two already-primitive operands.
fn add_primitives(
  state: State(host),
  lprim: JsValue,
  rprim: JsValue,
  rest: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case lprim, rprim {
    JsString(a), JsString(b) ->
      Ok(State(..state, stack: [JsString(a <> b), ..rest], pc: state.pc + 1))
    JsString(a), _ -> {
      use #(b, state) <- result.map(
        state.rethrow(coerce.js_to_string(state, rprim)),
      )
      State(..state, stack: [JsString(a <> b), ..rest], pc: state.pc + 1)
    }
    _, JsString(b) -> {
      use #(a, state) <- result.map(
        state.rethrow(coerce.js_to_string(state, lprim)),
      )
      State(..state, stack: [JsString(a <> b), ..rest], pc: state.pc + 1)
    }
    // §13.15.3 step 3: both operands BigInt → BigInt::add; one BigInt and
    // one non-string other type → TypeError (§13.15.4 step 7.a).
    value.JsBigInt(value.BigInt(a)), value.JsBigInt(value.BigInt(b)) ->
      Ok(
        State(
          ..state,
          stack: [value.JsBigInt(value.BigInt(a + b)), ..rest],
          pc: state.pc + 1,
        ),
      )
    value.JsBigInt(_), _ | _, value.JsBigInt(_) ->
      state.throw_type_error(
        state,
        "Cannot mix BigInt and other types, use explicit conversions",
      )
    _, _ ->
      case operators.num_binop(lprim, rprim, operators.num_add) {
        Ok(result) ->
          Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
        Error(err) -> throw_operator_error(state, err)
      }
  }
}

fn binop_add_with_to_primitive(
  state: State(host),
  left: JsValue,
  right: JsValue,
  rest: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  use #(lprim, s1) <- result.try(
    state.rethrow(coerce.to_primitive(state, left, coerce.DefaultHint)),
  )
  use #(rprim, s2) <- result.try(
    state.rethrow(coerce.to_primitive(s1, right, coerce.DefaultHint)),
  )
  add_primitives(s2, lprim, rprim, rest)
}

/// True when `next_fn` is the unmodified native Generator.prototype.next.
/// Lets yield* resume the inner generator via the internal no-alloc API
/// instead of a full native call that allocates a discarded result object.
fn is_native_generator_next(h: Heap(host), next_fn: JsValue) -> Bool {
  case next_fn {
    JsObject(fn_ref) ->
      case heap.read(h, fn_ref) {
        Some(ObjectSlot(
          kind: value.NativeFunction(value.Call(value.GeneratorNext), ..),
          ..,
        )) -> True
        _ -> False
      }
    _ -> False
  }
}

/// §7.4.8 IteratorStep / §7.4.9 IteratorStepValue: IteratorComplete reads
/// `done` first; when done is true, `value` is NOT read (observable — the
/// result object's value getter must not fire). yield* keeps the plain
/// `builtins_iterator.read_iter_result` because §27.5.3.2 DOES read value
/// when done.
fn read_iter_step_result(
  state: State(host),
  res: JsValue,
) -> Result(#(Bool, JsValue, State(host)), #(StepResult, JsValue, State(host))) {
  case res {
    JsObject(rref) -> {
      use #(done, state) <- result.try(
        state.rethrow(object.get_value(state, rref, Named("done"), res)),
      )
      case value.is_truthy(done) {
        True -> Ok(#(True, JsUndefined, state))
        False -> {
          use #(val, state) <- result.map(
            state.rethrow(object.get_value(state, rref, Named("value"), res)),
          )
          #(False, val, state)
        }
      }
    }
    _ -> state.throw_type_error(state, "Iterator result is not an object")
  }
}

/// §7.4.1 steps 4-6: finish GetIterator by building the Iterator Record —
/// `next` is fetched ONCE here so each IteratorNext is just a call, not a
/// fresh own-property miss + prototype-chain walk per iteration. Only
/// Generator/Array/String iterators are pushed raw: IteratorNext steps
/// those in-VM and never reads `.next`, so a record would only add an
/// alloc. Set/Map iterators step through their native `.next`, so they
/// get a record like any other iterator.
fn push_iterator_record(
  state: State(host),
  iter_ref: Ref,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case heap.read(state.heap, iter_ref) {
    Some(ObjectSlot(kind: GeneratorObject(_), ..))
    | Some(ObjectSlot(kind: ArrayIteratorObject(..), ..))
    | Some(ObjectSlot(kind: value.StringIteratorObject(..), ..)) ->
      Ok(
        State(
          ..state,
          stack: [JsObject(iter_ref), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    _ -> {
      let iter = JsObject(iter_ref)
      use #(next_method, state) <- result.map(
        state.rethrow(object.get_value(state, iter_ref, Named("next"), iter)),
      )
      let #(heap, rec_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: value.IteratorRecordObject(iterated: iter, next_method:),
            properties: dict.new(),
            elements: elements.new(),
            prototype: None,
            symbol_properties: [],
            extensible: False,
          ),
        )
      State(
        ..state,
        heap:,
        stack: [JsObject(rec_ref), ..rest_stack],
        pc: state.pc + 1,
      )
    }
  }
}

/// Resolve a GetIterator stack slot to the real iterator object: internal
/// IteratorRecordObject wrappers (cached `next`) must be transparent to
/// .return()/.throw() lookups and to yield* delegation.
fn unwrap_iterator_record(state: State(host), v: JsValue) -> JsValue {
  case v {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Some(ObjectSlot(kind: value.IteratorRecordObject(iterated:, ..), ..)) ->
          iterated
        _ -> v
      }
    _ -> v
  }
}

/// IteratorNext for an internal Iterator Record: call the `next_method`
/// cached at GetIterator (§7.4.1) with the real iterator as `this`. Same
/// stack contract as step_generic_iterator.
/// IteratorNext step for an ArrayIteratorObject whose source is a plain
/// array/arguments object or a Proxy. Pushes [done, value, iter|undef]
/// like the inline IteratorNext paths.
fn step_array_iterator_source(
  state: State(host),
  iter_ref: value.Ref,
  source: value.Ref,
  index: Int,
  iter_kind: value.ArrayIterKind,
  rest: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  case object.as_proxy(state.heap, source) {
    // Proxy source — §23.1.5.1 generic path: ? Get(array,
    // "length") / ? Get(array, ToString(index)) fire get traps.
    Some(_) -> {
      use #(len_val, state) <- result.try(
        state.rethrow(object.get_value(
          state,
          source,
          Named("length"),
          JsObject(source),
        )),
      )
      // §7.1.20 LengthOfArrayLike: ? ToLength(len_val). The trap result may
      // be an object — ToNumber must run ToPrimitive on it (user valueOf,
      // which can throw), so go through coerce.js_to_number.
      use #(len_num, state) <- result.try(
        state.rethrow(coerce.js_to_number(state, len_val)),
      )
      let length = case len_num {
        value.Finite(f) -> int.max(0, value.float_to_int(f))
        _ -> 0
      }
      case index >= length {
        True -> {
          let heap = advance_array_iter(state.heap, iter_ref, source, -1)
          Ok(
            State(
              ..state,
              stack: [JsBool(True), JsUndefined, JsUndefined, ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
        False -> {
          // §23.1.5.1 step 8.b.iii: "key" iterators yield the index WITHOUT
          // Get(array, elementKey) — no get trap fires.
          use #(val, state) <- result.try(case iter_kind {
            value.ArrayIterKeys -> Ok(#(value.from_int(index), state))
            _ -> {
              use #(elem, state) <- result.try(
                state.rethrow(object.get_value(
                  state,
                  source,
                  value.Index(index),
                  JsObject(source),
                )),
              )
              let #(h, out) =
                array_iter_shape(state.heap, state, iter_kind, index, elem)
              Ok(#(out, State(..state, heap: h)))
            }
          })
          let heap = advance_array_iter(state.heap, iter_ref, source, index + 1)
          Ok(
            State(
              ..state,
              stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
      }
    }
    None -> {
      // Re-read the source length each time (handles mutations
      // during iteration)
      let #(length, elements) =
        heap.read_array_like(state.heap, source)
        |> option.unwrap(#(0, elements.new()))
      case index >= length {
        True -> {
          let heap = advance_array_iter(state.heap, iter_ref, source, -1)
          Ok(
            State(
              ..state,
              stack: [JsBool(True), JsUndefined, JsUndefined, ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
        False -> {
          // §23.1.5.1: Let elementValue be ? Get(array, elementKey). The
          // elements store is only a fast path for plain data values —
          // defineProperty can install accessor/attribute overrides at an
          // index (kept in the properties dict), and holes consult the
          // prototype chain, so fall back to the generic [[Get]] when the
          // element store has no entry or an override exists. "key"
          // iterators skip the Get entirely (§23.1.5.1 step 8.b.iii).
          use #(val, state) <- result.try(case iter_kind {
            value.ArrayIterKeys -> Ok(#(value.from_int(index), state))
            _ -> {
              use #(elem, state) <- result.try(
                case
                  array_elem_without_override(
                    state.heap,
                    source,
                    elements,
                    index,
                  )
                {
                  Some(v) -> Ok(#(v, state))
                  None ->
                    state.rethrow(object.get_value(
                      state,
                      source,
                      value.Index(index),
                      JsObject(source),
                    ))
                },
              )
              let #(h, out) =
                array_iter_shape(state.heap, state, iter_kind, index, elem)
              Ok(#(out, State(..state, heap: h)))
            }
          })
          let heap = advance_array_iter(state.heap, iter_ref, source, index + 1)
          Ok(
            State(
              ..state,
              stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
      }
    }
  }
}

/// §23.1.5.1: shape one iteration result for the iterator's kind — the
/// index ("key"), the element ("value"), or a fresh [index, element] pair
/// array ("key+value").
fn array_iter_shape(
  h: state.Heap(host),
  state: State(host),
  iter_kind: value.ArrayIterKind,
  index: Int,
  elem: JsValue,
) -> #(state.Heap(host), JsValue) {
  case iter_kind {
    value.ArrayIterKeys -> #(h, value.from_int(index))
    value.ArrayIterValues -> #(h, elem)
    value.ArrayIterEntries -> {
      let #(h, pair_ref) =
        common.alloc_array(
          h,
          [value.from_int(index), elem],
          state.builtins.array.prototype,
        )
      #(h, JsObject(pair_ref))
    }
  }
}

/// Fast-path element read for the array iterator: Some(value) only when the
/// index is a plain data value in the element store with no properties-dict
/// override (defineProperty can install accessor/attribute overrides at an
/// index). None → caller takes the generic [[Get]] path.
fn array_elem_without_override(
  h: state.Heap(host),
  source: value.Ref,
  elems: value.JsElements,
  index: Int,
) -> Option(JsValue) {
  case heap.read(h, source) {
    Some(ObjectSlot(properties:, ..)) ->
      case dict.has_key(properties, value.Index(index)) {
        True -> None
        False -> elements.get_option(elems, index)
      }
    _ -> None
  }
}

/// Bump an ArrayIteratorObject's index in place (preserving the iteration
/// kind). No-op if the ref no longer holds an array-iterator slot.
fn advance_array_iter(
  h: state.Heap(host),
  iter_ref: value.Ref,
  source: value.Ref,
  index: Int,
) -> state.Heap(host) {
  case heap.read(h, iter_ref) {
    Some(ObjectSlot(kind: ArrayIteratorObject(iter_kind:, ..), ..) as slot) ->
      heap.write(
        h,
        iter_ref,
        ObjectSlot(
          ..slot,
          kind: ArrayIteratorObject(source:, index:, iter_kind:),
        ),
      )
    _ -> h
  }
}

fn step_iterator_record(
  state: State(host),
  rec_ref: Ref,
  iterated: JsValue,
  next_method: JsValue,
  rest: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  use #(result_obj, state) <- result.try(
    state.rethrow(state.call(state, next_method, iterated, [])),
  )
  use #(done, val, state) <- result.map(read_iter_step_result(state, result_obj))
  let iter_slot = case done {
    True -> JsUndefined
    False -> JsObject(rec_ref)
  }
  State(
    ..state,
    stack: [JsBool(done), val, iter_slot, ..rest],
    pc: state.pc + 1,
  )
}

/// IteratorNext fallback for user-defined iterators: call .next(), extract
/// {value, done}, push [done, value, iter] onto stack. On done=true the iter
/// slot becomes JsUndefined ([[Done]] tracking — see IteratorNext); abrupt
/// completion is undef'd by the caller's mark_done wrapper.
fn step_generic_iterator(
  state: State(host),
  iter_ref: Ref,
  rest: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  let iter = JsObject(iter_ref)
  use #(next_fn, state) <- result.try(
    state.rethrow(object.get_value(state, iter_ref, Named("next"), iter)),
  )
  use #(result_obj, state) <- result.try(
    state.rethrow(state.call(state, next_fn, iter, [])),
  )
  use #(done, val, state) <- result.map(read_iter_step_result(state, result_obj))
  let iter_slot = case done {
    True -> JsUndefined
    False -> iter
  }
  State(
    ..state,
    stack: [JsBool(done), val, iter_slot, ..rest],
    pc: state.pc + 1,
  )
}

/// ES2024 §7.4.1 GetIterator(obj, kind) — look up Symbol.iterator and call it.
/// Used when the fast path (ArrayObject without overridden Symbol.iterator) doesn't apply.
fn get_iterator_via_symbol(
  state: State(host),
  ref: value.Ref,
  iterable: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  // Step 1: Let method be ? GetMethod(obj, @@iterator)
  case object.get_symbol_value(state, ref, value.symbol_iterator, iterable) {
    Ok(#(method, state)) ->
      case helpers.is_callable(state.heap, method) {
        True ->
          // Step 2: Let iterator be ? Call(method, obj)
          case state.call(state, method, iterable, []) {
            Ok(#(iterator, state)) ->
              case iterator {
                JsObject(iter_ref) ->
                  push_iterator_record(state, iter_ref, rest_stack)
                _ ->
                  state.throw_type_error(
                    state,
                    "Iterator result is not an object",
                  )
              }
            Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
          }
        False ->
          state.throw_type_error(
            state,
            object.inspect(iterable, state.heap) <> " is not iterable",
          )
      }
    // §7.3.10 GetMethod step 1: a throwing getter propagates — it must not
    // be masked by the "not iterable" TypeError (test262 get-abrupt cases).
    Error(#(thrown, state)) -> Error(#(Thrown, thrown, state))
  }
}

/// ES §7.4.3 GetIterator(obj, async). Tries Symbol.asyncIterator, falls back
/// to Symbol.iterator wrapped via CreateAsyncFromSyncIterator (§27.1.6.1).
///
/// Spec-ordered (engine262 GetIterator/GetIteratorFromMethod):
/// - GetMethod abrupt completions propagate (getter throw, call throw)
/// - method undefined/null → fall back to @@iterator
/// - method present but not callable → TypeError (GetMethod step 3)
/// - call result not an object → TypeError (GetIteratorFromMethod step 2)
fn get_async_iterator_via_symbol(
  state: State(host),
  ref: Ref,
  iterable: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  // Step 1.a: Let method be ? GetMethod(obj, @@asyncIterator).
  use #(method, state) <- result.try(
    state.rethrow(object.get_symbol_value(
      state,
      ref,
      value.symbol_async_iterator,
      iterable,
    )),
  )
  case method {
    // Step 1.b: method undefined → sync fallback via @@iterator.
    JsUndefined | JsNull ->
      get_async_from_sync_fallback(state, ref, iterable, rest_stack)
    _ -> {
      use #(iter, state) <- result.try(call_iterator_method(
        state,
        method,
        iterable,
      ))
      Ok(State(..state, stack: [iter, ..rest_stack], pc: state.pc + 1))
    }
  }
}

/// §7.4.3 step 1.b: @@asyncIterator is undefined — GetMethod(obj, @@iterator),
/// call it, and wrap the sync iterator via CreateAsyncFromSyncIterator.
fn get_async_from_sync_fallback(
  state: State(host),
  ref: Ref,
  iterable: JsValue,
  rest_stack: List(JsValue),
) -> Result(State(host), #(StepResult, JsValue, State(host))) {
  use #(sync_method, state) <- result.try(
    state.rethrow(object.get_symbol_value(
      state,
      ref,
      value.symbol_iterator,
      iterable,
    )),
  )
  case sync_method {
    JsUndefined | JsNull ->
      state.throw_type_error(
        state,
        object.inspect(iterable, state.heap) <> " is not async iterable",
      )
    _ -> {
      use #(sync_iter_val, state) <- result.try(call_iterator_method(
        state,
        sync_method,
        iterable,
      ))
      let assert JsObject(sync_iter) = sync_iter_val
      // GetIteratorFromMethod step 4: cache the sync iterator record's
      // [[NextMethod]] now — observable Get, abrupt propagates. The wrapper's
      // .next() reuses it instead of re-Getting per call (§27.1.6.2.1).
      use #(sync_next, state) <- result.try(
        state.rethrow(object.get_value(
          state,
          sync_iter,
          Named("next"),
          sync_iter_val,
        )),
      )
      let #(h, wrapped) =
        common.alloc_wrapper(
          state.heap,
          value.AsyncFromSyncIteratorObject(sync_iter:, sync_next:),
          state.builtins.async_from_sync_iterator_proto,
        )
      Ok(
        State(
          ..state,
          heap: h,
          stack: [JsObject(wrapped), ..rest_stack],
          pc: state.pc + 1,
        ),
      )
    }
  }
}

/// GetMethod step 3 + GetIteratorFromMethod steps 1-2: the looked-up method
/// must be callable, its call result must be an object; both call abrupt
/// completions propagate.
fn call_iterator_method(
  state: State(host),
  method: JsValue,
  iterable: JsValue,
) -> Result(#(JsValue, State(host)), #(StepResult, JsValue, State(host))) {
  case helpers.is_callable(state.heap, method) {
    False ->
      state.throw_type_error(
        state,
        object.inspect(method, state.heap) <> " is not a function",
      )
    True -> {
      use #(iter, state) <- result.try(
        state.rethrow(state.call(state, method, iterable, [])),
      )
      case iter {
        JsObject(_) -> Ok(#(iter, state))
        _ -> state.throw_type_error(state, "Iterator result is not an object")
      }
    }
  }
}
