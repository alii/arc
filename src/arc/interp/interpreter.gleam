//// The interpreter's dispatch loop. `fast_loop` runs the two dozen common
//// opcodes on bare registers (pc, stack, locals, agent) over the total
//// `arc_interp_ffi` kernels; a kernel `miss`, an empty stack, or any other
//// opcode materialises the full `State` once and goes through `step`, the
//// one big instruction dispatcher. `step` is Result-based: `Ok(state)`
//// continues, `Error(StepExit)` leaves the loop with a throw, the frame's
//// return, a coroutine suspension, or a broken engine invariant. Runtime
//// calls that can raise a JS exception go through the `guardN` shims so a
//// raise comes back as `Error(Threw(..))` carrying the agent it raised
//// with; throws the interpreter originates itself allocate the error with
//// the non-raising `JsOps.new_error` and return `Error(Threw(..))` too.

import arc/bytecode/binop
import arc/bytecode/key
import arc/bytecode/lexical
import arc/bytecode/opcode.{
  type Op, ArrayFrom, ArrayFromWithHoles, ArrayPush, ArrayPushHole, ArraySpread,
  AsyncYieldStarNext, AsyncYieldStarResume, Await, BinOp, BoxLocal, Call,
  CallApply, CallConstructor, CallConstructorApply, CallEval, CallMethod,
  CallMethodApply, CmpLocalConstJump, CmpLocalLocalJump, CreateArguments,
  CreateRestArray, DecLocal, DeclareEvalVar, DeclareGlobalFn, DeclareGlobalLex,
  DeclareGlobalVar, DefineAccessor, DefineAccessorComputed, DefineField,
  DefineFieldComputed, DefineMethod, DefineMethodComputed, DefinePrivateAccessor,
  DefinePrivateField, DefinePrivateMethod, DeleteElem, DeleteField,
  DeleteGlobalVar, Dup, ForInNext, ForInStart, GetAsyncIterator, GetBoxed,
  GetElem, GetElem2, GetEvalVar, GetField, GetField2, GetGlobal, GetIterator,
  GetLocal, GetPrivateFieldDyn, GetPrivateFieldDyn2, GetPrototypeOf,
  GetSuperValue, GetSuperValue2, IncLocal, InitGlobalLex, InitialYield,
  IteratorCheckObject, IteratorClose, IteratorCloseThrow, IteratorNext,
  IteratorRecord, IteratorRest, Jump, JumpIfFalse, JumpIfNullish, JumpIfTrue,
  MakeClosure, MakeMethod, NewObject, NewPrivateName, NewRegExp, ObjectRestCopy,
  ObjectSpread, Pc, Pop, PrivateInDyn, PushConst, PushTry, PutBoxed,
  PutBoxedCheckInit, PutElem, PutEvalVar, PutField, PutGlobal, PutLocal,
  PutLocalCheckInit, PutPrivateFieldDyn, PutSuperValue, Return, Rot3, SetLine,
  SetProto, SetupDerivedClass, Swap, TypeOf, TypeofEvalVar, TypeofGlobal,
  UnaryOp, Unrot4, Yield, YieldStar,
}
import arc/internal/tree_array
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/call.{type Drive}
import arc/interp/dynamic_import
import arc/interp/eval
import arc/interp/ffi
import arc/interp/park
import arc/interp/state.{
  type State, type StepExit, type VmError, AsyncDelegateResume, Awaited,
  DelegateYield, InitialSuspend, InternalError, PlainYield, Returned, SavedFrame,
  StackUnderflow, State, SuspensionLeak, Threw, VmFailed, Yielded,
}
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/builtins/disposable_stack
import arc/rt/builtins/error as rt_error
import arc/rt/builtins/global_fns
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/regexp as b_regexp
import arc/rt/bytecode.{
  type FuncTemplate, type SuspendedFrame, ParkedOp, ParkedStart, TryFrame,
}
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/class as rt_class
import arc/rt/closure
import arc/rt/elements as rt_elements
import arc/rt/env as rt_env
import arc/rt/inspect as rt_inspect
import arc/rt/lang as rt_lang
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type LexicalGlobal, type ObjectKey,
  AccessorProperty, Agent, DataProperty, ForInIterator, FunctionApply,
  FunctionCall, FunctionN, HintString, Index, JsStore, KBytecode, KCompiled,
  KHandle, KNative, KNull, KNum, KStr, KSym, KUndef, Named, NoElements, Realm,
  ReflectApply, ReflectN, SBox, SObject, SShapedObject, StringKey, SymbolKey,
  classify, mk_bool, mk_number, mk_object, mk_string, mk_tdz, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// ============================================================================
// Outcome
// ============================================================================

/// How one run of the loop ended: the activation completed (normally or
/// with an uncaught throw), or a coroutine body parked at `yield`/`await`.
/// Only coroutine drivers may see `Suspended`; every other caller narrows
/// through `execute_to_completion`.
pub type Outcome {
  Completed(Completion)
  Suspended(state.SuspendKind, JsVal)
}

// ============================================================================
// Wire-level probes for the fast loop
// ============================================================================
// The few tests `fast_loop` needs beyond the `arc_interp_ffi` kernels, kept
// as total term probes so a hit never goes through `classify`.

/// `v` is the Handle wire form `{js_cell, N}`.
@external(erlang, "arc_rt_store_ffi", "is_handle")
fn is_handle(v: JsVal) -> Bool

/// A value already proven a handle by `is_handle` (the object wire form IS
/// the Handle record).
@external(erlang, "arc_rt_store_ffi", "identity")
fn as_handle(v: JsVal) -> Handle

/// The kernels' `miss` answer, for fast-loop arms that decide to bail before
/// calling one. `Miss` lowers to the same atom.
type KernelMiss {
  Miss
}

@external(erlang, "arc_rt_store_ffi", "identity")
fn miss_value(m: KernelMiss) -> JsVal

/// §13.12 bitwise operators on two integer-valued Numbers (ToInt32 wrap
/// inline); anything else answers `miss`.
@external(erlang, "arc_rt_ops_ffi", "t_bitand_fast")
fn k_bitand(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "t_bitor_fast")
fn k_bitor(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "t_bitxor_fast")
fn k_bitxor(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "t_shl_fast")
fn k_shl(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "t_shr_fast")
fn k_shr(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "t_ushr_fast")
fn k_ushr(a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "t_bitnot_fast")
fn k_bitnot(a: JsVal) -> JsVal

/// A compare/equality kernel's `true | false | miss` answer as a value: the
/// boolean atoms ARE the boolean wire terms, and `miss` passes through for
/// the caller's `is_miss` test.
@external(erlang, "arc_rt_store_ffi", "identity")
fn tri(b: Bool) -> JsVal

// ============================================================================
// Guarded runtime calls
// ============================================================================
// `rtN(state, f, ..)` applies the raise-capable, value-first runtime function
// `f(agent, ..) -> #(v, Agent)` under the `guardN` shim and adopts whichever
// agent comes back. `f` is always a module function, so the shim receives a
// literal remote fun and no closure is built per call.

fn rt2(
  state: State,
  f: fn(Agent, a) -> #(v, Agent),
  a: a,
) -> Result(#(v, State), StepExit) {
  ffi.guarded(ffi.guard2(f, state.agent, a), state)
}

fn rt3(
  state: State,
  f: fn(Agent, a, b) -> #(v, Agent),
  a: a,
  b: b,
) -> Result(#(v, State), StepExit) {
  ffi.guarded(ffi.guard3(f, state.agent, a, b), state)
}

fn rt4(
  state: State,
  f: fn(Agent, a, b, c) -> #(v, Agent),
  a: a,
  b: b,
  c: c,
) -> Result(#(v, State), StepExit) {
  ffi.guarded(ffi.guard4(f, state.agent, a, b, c), state)
}

fn rt5(
  state: State,
  f: fn(Agent, a, b, c, d) -> #(v, Agent),
  a: a,
  b: b,
  c: c,
  d: d,
) -> Result(#(v, State), StepExit) {
  ffi.guarded(ffi.guard5(f, state.agent, a, b, c, d), state)
}

fn rt7(
  state: State,
  f: fn(Agent, a, b, c, d, e, g) -> #(v, Agent),
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
  g: g,
) -> Result(#(v, State), StepExit) {
  ffi.guarded(ffi.guard7(f, state.agent, a, b, c, d, e, g), state)
}

fn drop_nil(r: Result(#(Nil, State), StepExit)) -> Result(State, StepExit) {
  use #(_nil, state) <- result.map(r)
  state
}

fn rt_unit3(
  state: State,
  f: fn(Agent, a, b) -> Agent,
  a: a,
  b: b,
) -> Result(State, StepExit) {
  ffi.guarded(ffi.guard_unit3(f, state.agent, a, b), state) |> drop_nil
}

fn rt_unit4(
  state: State,
  f: fn(Agent, a, b, c) -> Agent,
  a: a,
  b: b,
  c: c,
) -> Result(State, StepExit) {
  ffi.guarded(ffi.guard_unit4(f, state.agent, a, b, c), state) |> drop_nil
}

fn rt_unit5(
  state: State,
  f: fn(Agent, a, b, c, d) -> Agent,
  a: a,
  b: b,
  c: c,
  d: d,
) -> Result(State, StepExit) {
  ffi.guarded(ffi.guard_unit5(f, state.agent, a, b, c, d), state) |> drop_nil
}

fn rt_unit6(
  state: State,
  f: fn(Agent, a, b, c, d, e) -> Agent,
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
) -> Result(State, StepExit) {
  ffi.guarded(ffi.guard_unit6(f, state.agent, a, b, c, d, e), state)
  |> drop_nil
}

// ============================================================================
// Small value helpers
// ============================================================================

/// The runtime key of a compile-time canonical key carried by an opcode.
fn okey(k: key.PropertyKey) -> ObjectKey {
  case k {
    key.Named(name) -> StringKey(Named(name))
    key.Index(i) -> StringKey(Index(i))
    key.Private(text) -> StringKey(rt_types.private_key(text))
  }
}

fn named(name: String) -> ObjectKey {
  StringKey(Named(name))
}

fn is_undef(v: JsVal) -> Bool {
  case classify(v) {
    KUndef -> True
    _ -> False
  }
}

fn is_object(v: JsVal) -> Bool {
  case classify(v) {
    KHandle(_) -> True
    _ -> False
  }
}

fn handle_of(v: JsVal) -> Option(Handle) {
  case classify(v) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}

fn int_val(n: Int) -> JsVal {
  mk_number(rt_types.JInt(n))
}

fn inspect(state: State, v: JsVal) -> String {
  rt_inspect.inspect(state.agent, v)
}

// ============================================================================
// Closures
// ============================================================================

/// Allocate the function object for `template` closed over `captured`
/// (gathered per the template's `env_descriptors`), created by code of parse
/// `unit`. Shared by the MakeClosure opcode and module link-time hoisting.
pub fn make_closure(
  agent: Agent,
  template: FuncTemplate,
  captured: List(JsVal),
  unit: Int,
) -> #(Handle, Agent) {
  closure.t_new_bytecode_function(
    agent,
    template,
    bytecode.env_from_list(captured),
    unit,
  )
}

/// §15.4.4 MakeMethod(F, homeObject) on a function cell: bytecode and
/// compiled closures carry [[HomeObject]] in their kind. No-op otherwise.
fn set_home_object(agent: Agent, fn_h: Handle, home: Handle) -> Agent {
  rt_store.t_cell_update(agent, fn_h, fn(slot) {
    case slot {
      SObject(kind: KBytecode(..) as k, ..) ->
        SObject(..slot, kind: KBytecode(..k, home_object: Some(home)))
      SObject(kind: KCompiled(..) as k, ..) ->
        SObject(..slot, kind: KCompiled(..k, home_object: Some(home)))
      _ -> slot
    }
  })
}

/// MakeMethod when the function is a stack value: only object values can be
/// closures.
fn make_method(agent: Agent, func: JsVal, target: Handle) -> Agent {
  case classify(func) {
    KHandle(fn_h) -> set_home_object(agent, fn_h, target)
    _ -> agent
  }
}

// ============================================================================
// Explicit resource management (`using` / `await using`)
// ============================================================================
// The compiler lowers a using-scope to anonymous disposer slots plus an
// inline DisposeResources sequence; the interpreter supplies
// CreateDisposableResource. A disposer never reaches user code, so it is
// whichever callable performs Dispose(V, hint, method) exactly.

/// CreateDisposableResource(V, hint): undefined for a null/undefined
/// resource, else the 0-argument callable the lowered dispose sequence
/// invokes. GetDisposeMethod(V, hint) reads the method once, here, never
/// again at dispose time. Raises TypeError for a primitive or method-less
/// resource. `unit` is the running activation's, for the fallback closure.
fn using_disposer(
  agent: Agent,
  val: JsVal,
  is_async: Bool,
  unit: Int,
) -> #(JsVal, Agent) {
  case classify(val) {
    // Step 1.a: V is null or undefined → method undefined.
    KUndef | KNull -> #(mk_undefined(), agent)
    // GetDisposeMethod(V, hint): sync-dispose reads @@dispose; async-dispose
    // reads @@asyncDispose, falling back to the step 1.b.ii wrapper around
    // @@dispose. A missing method is a TypeError.
    KHandle(_) -> {
      let #(method, agent) =
        disposable_stack.get_dispose_method(agent, val, is_async:)
      case method {
        disposable_stack.DirectDispose(m) -> direct_disposer(agent, m, val)
        disposable_stack.SyncFallbackDispose(m) ->
          sync_fallback_disposer(agent, m, val, unit)
      }
    }
    // Step 1.b.i: a primitive resource is a TypeError.
    _ ->
      rt_val.t_throw_type_error(
        agent,
        "using declaration initializer is not an object, null, or undefined",
      )
  }
}

/// Dispose(V, hint, method) is Call(method, V): a bound function of `method`
/// with `this` = V and no arguments. Built directly rather than through
/// Function.prototype.bind so the method's own `length`/`name` are not read.
fn direct_disposer(
  agent: Agent,
  method: Handle,
  val: JsVal,
) -> #(JsVal, Agent) {
  let #(h, agent) =
    rt_store.t_cell_new(
      agent,
      SObject(
        kind: rt_types.KBound(target: method, bound_this: val, bound_args: []),
        proto: Some(agent.realm.function.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(h), agent)
}

/// GetDisposeMethod step 1.b.ii: the async-dispose fallback onto a sync
/// @@dispose calls the method, discards its result and settles a fresh
/// promise with undefined (rejecting it instead if the call threw). That is
/// an async function whose body is `Call(method, V)`, so it is one: an
/// async arrow closed over [method, V].
fn sync_fallback_disposer(
  agent: Agent,
  method: Handle,
  val: JsVal,
  unit: Int,
) -> #(JsVal, Agent) {
  let #(h, agent) =
    make_closure(
      agent,
      sync_fallback_template(),
      [mk_object(method), val],
      unit,
    )
  #(mk_object(h), agent)
}

/// `async () => { Call(locals[0], locals[1]) }` over a two-value env.
fn sync_fallback_template() -> FuncTemplate {
  bytecode.FuncTemplate(
    name: None,
    arity: 0,
    length: 0,
    local_count: 2,
    // [V] → [method, V] → CallMethod(0) → drop → undefined → Return.
    bytecode: tuple_array.from_list([
      GetLocal(1),
      GetLocal(0),
      CallMethod(0),
      Pop,
      PushConst(0),
      Return,
    ]),
    constants: tuple_array.from_list([mk_undefined()]),
    functions: tuple_array.from_list([]),
    env_descriptors: [bytecode.CaptureLocal(0), bytecode.CaptureLocal(1)],
    is_strict: True,
    is_arrow: True,
    is_derived_constructor: False,
    is_generator: False,
    is_async: True,
    is_constructor: False,
    is_class_constructor: False,
    local_names: None,
    lexical: lexical.NoLexicalSlots,
    code_kind: lexical.FunctionCode,
  )
}

// ============================================================================
// Global lexical bindings (§9.1.1.4 declarative half)
// ============================================================================
// A realm's global `let`/`const`/`class` bindings live on its Realm Record
// (`Realm.lexical_globals`), never on the global object. A binding in its
// temporal dead zone holds the TDZ sentinel.

fn lex_lookup(agent: Agent, name: String) -> Option(LexicalGlobal) {
  dict.get(agent.realm.lexical_globals, name) |> option.from_result
}

fn lex_write(agent: Agent, name: String, binding: LexicalGlobal) -> Agent {
  let realm = agent.realm
  Agent(
    ..agent,
    realm: Realm(
      ..realm,
      lexical_globals: dict.insert(realm.lexical_globals, name, binding),
    ),
  )
}

/// `v` is the TDZ sentinel.
@external(erlang, "arc_interp_ffi", "is_tdz")
fn is_tdz(v: JsVal) -> Bool

// ============================================================================
// Execution loop
// ============================================================================

/// Main execution loop. Tail-recursive. Returns the outcome (completion or
/// suspension) and the final state. Every bytecode stream ends with a
/// sentinel Return (appended by resolve.gleam), so fetch uses unchecked
/// element/2. Termination flows through the Return handler.
pub fn execute_inner(
  state: State,
  drive: Drive,
) -> Result(#(Outcome, State), VmError) {
  fast_loop(
    state,
    drive,
    state.pc,
    state.stack,
    state.locals,
    state.agent,
    state.code,
    state.constants,
    call.current_line(state.agent),
  )
}

/// Run the loop over a frame that cannot resume a suspension: top-level
/// scripts, eval frames, re-entrant calls from natives. Narrows `Completed`
/// to its `Completion`; a `Suspended` escaping such a frame is an engine bug
/// reported as a `VmError` at `site`.
pub fn execute_to_completion(
  state: State,
  drive: Drive,
  site: String,
) -> Result(#(Completion, State), VmError) {
  case execute_inner(state, drive) {
    Ok(#(Completed(comp), final_state)) -> Ok(#(comp, final_state))
    Ok(#(Suspended(kind, _), _)) -> Error(SuspensionLeak(site:, kind:))
    Error(vm_err) -> Error(vm_err)
  }
}

/// Hot inner loop. Carries the per-instruction registers (pc, stack, locals,
/// agent, current line) as bare arguments so the common opcodes run without
/// rebuilding the State record and an Ok box per step; code/constants are
/// loop-invariant within a frame and ride along to save a field load. Every
/// value test is a total FFI kernel over the wire term: no `classify`, no
/// allocation on a hit. Anything not handled here, and every miss / throw /
/// underflow path of what is, materialises the State once in `dispatch_slow`
/// and re-executes the instruction through `step` (all fast paths are
/// effect-free before they bail, so re-execution is safe).
fn fast_loop(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  line: Int,
) -> Result(#(Outcome, State), VmError) {
  case tuple_array.get_unchecked(pc, code) {
    SetLine(l) ->
      fast_loop(state, drive, pc + 1, stack, locals, agent, code, constants, l)

    PushConst(index) -> {
      let v = tuple_array.get_unchecked(index, constants)
      fast_loop(
        state,
        drive,
        pc + 1,
        [v, ..stack],
        locals,
        agent,
        code,
        constants,
        line,
      )
    }

    Pop ->
      case stack {
        [_, ..rest] ->
          fast_loop(
            state,
            drive,
            pc + 1,
            rest,
            locals,
            agent,
            code,
            constants,
            line,
          )
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    Dup ->
      case stack {
        [top, ..] ->
          fast_loop(
            state,
            drive,
            pc + 1,
            [top, ..stack],
            locals,
            agent,
            code,
            constants,
            line,
          )
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    Swap ->
      case stack {
        [a, b, ..rest] ->
          fast_loop(
            state,
            drive,
            pc + 1,
            [b, a, ..rest],
            locals,
            agent,
            code,
            constants,
            line,
          )
        _ -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    GetLocal(index) -> {
      let v = tuple_array.get_unchecked(index, locals)
      // TDZ: the slow path rebuilds State and throws the ReferenceError.
      case is_tdz(v) {
        True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
        False ->
          fast_loop(
            state,
            drive,
            pc + 1,
            [v, ..stack],
            locals,
            agent,
            code,
            constants,
            line,
          )
      }
    }

    PutLocal(index) ->
      case stack {
        [v, ..rest] ->
          fast_loop(
            state,
            drive,
            pc + 1,
            rest,
            tuple_array.set_unchecked(index, v, locals),
            agent,
            code,
            constants,
            line,
          )
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    GetBoxed(index) ->
      case box_value(agent, tuple_array.get_unchecked(index, locals)) {
        // TDZ / not a box: the slow path throws.
        Error(Nil) ->
          dispatch_slow(state, drive, pc, stack, locals, agent, line)
        Ok(v) ->
          fast_loop(
            state,
            drive,
            pc + 1,
            [v, ..stack],
            locals,
            agent,
            code,
            constants,
            line,
          )
      }

    PutBoxed(index) ->
      case stack {
        [v, ..rest] -> {
          let slot = tuple_array.get_unchecked(index, locals)
          case is_handle(slot) {
            False -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            True ->
              fast_loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                rt_store.t_cell_set(agent, as_handle(slot), SBox(v)),
                code,
                constants,
                line,
              )
          }
        }
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    Jump(Pc(target)) ->
      fast_loop(
        state,
        drive,
        target,
        stack,
        locals,
        agent,
        code,
        constants,
        line,
      )

    JumpIfFalse(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case ffi.truthy(top) {
            False ->
              fast_loop(
                state,
                drive,
                target,
                rest,
                locals,
                agent,
                code,
                constants,
                line,
              )
            True ->
              fast_loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    JumpIfTrue(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case ffi.truthy(top) {
            True ->
              fast_loop(
                state,
                drive,
                target,
                rest,
                locals,
                agent,
                code,
                constants,
                line,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    JumpIfNullish(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case ffi.nullish(top) {
            True ->
              fast_loop(
                state,
                drive,
                target,
                rest,
                locals,
                agent,
                code,
                constants,
                line,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // `kind` arrives already classified by the resolver. Objects
    // (ToPrimitive), Symbols (TypeError), string relational compares and
    // every other observable case make the kernel answer `miss`.
    BinOp(kind) ->
      case stack {
        [right, left, ..rest] -> {
          let r = case kind {
            opcode.AddOp -> ffi.add(left, right)
            opcode.PureOp(op) -> pure_binop_kernel(op, left, right)
            // instanceof / in read the heap and can run user code.
            opcode.InstanceOfOp | opcode.InOp -> miss_value(Miss)
          }
          case ffi.is_miss(r) {
            True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [r, ..rest],
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        }
        _ -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    UnaryOp(kind) ->
      case stack {
        [operand, ..rest] -> {
          let r = case kind {
            opcode.Neg -> ffi.neg(operand)
            opcode.Pos -> ffi.plus(operand)
            opcode.LogicalNot -> mk_bool(!ffi.truthy(operand))
            opcode.Void -> mk_undefined()
            opcode.BitNot -> k_bitnot(operand)
          }
          case ffi.is_miss(r) {
            True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [r, ..rest],
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        }
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // -- Fused superinstructions (resolver peephole) ---------------------
    // Statement-position `i++` / `i--` on a numeric local: one locals write,
    // no stack traffic. Non-numbers (objects, strings, BigInt, TDZ) take the
    // slow path's full coercion chain.
    IncLocal(index) -> {
      let r = number_step(tuple_array.get_unchecked(index, locals), 1)
      case ffi.is_miss(r) {
        True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
        False ->
          fast_loop(
            state,
            drive,
            pc + 1,
            stack,
            tuple_array.set_unchecked(index, r, locals),
            agent,
            code,
            constants,
            line,
          )
      }
    }

    DecLocal(index) -> {
      let r = number_step(tuple_array.get_unchecked(index, locals), -1)
      case ffi.is_miss(r) {
        True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
        False ->
          fast_loop(
            state,
            drive,
            pc + 1,
            stack,
            tuple_array.set_unchecked(index, r, locals),
            agent,
            code,
            constants,
            line,
          )
      }
    }

    // Fused loop-condition compare-and-branch. Objects and TDZ sentinels
    // miss (the compare kernels only answer for number/string/bigint pairs).
    CmpLocalLocalJump(left_idx, right_idx, kind, Pc(target)) -> {
      let r =
        pure_binop_kernel(
          kind,
          tuple_array.get_unchecked(left_idx, locals),
          tuple_array.get_unchecked(right_idx, locals),
        )
      case ffi.is_miss(r) {
        True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
        False ->
          case ffi.truthy(r) {
            True ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                line,
              )
            False ->
              fast_loop(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
      }
    }

    CmpLocalConstJump(left_idx, const_index, kind, Pc(target)) -> {
      let r =
        pure_binop_kernel(
          kind,
          tuple_array.get_unchecked(left_idx, locals),
          tuple_array.get_unchecked(const_index, constants),
        )
      case ffi.is_miss(r) {
        True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
        False ->
          case ffi.truthy(r) {
            True ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                line,
              )
            False ->
              fast_loop(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
      }
    }

    // -- Dense-array computed access -------------------------------------
    // `a[i]` with an own element present on an Array cell (or a plain
    // string key on an ordinary chain). Holes, accessors, exotic receivers
    // and non-canonical keys miss to the full [[Get]].
    GetElem ->
      case stack {
        [k, recv, ..rest] -> {
          let v = ffi.get_elem(agent.store, recv, k)
          case ffi.is_miss(v) {
            True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [v, ..rest],
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        }
        _ -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // `a[i] = v` on an extensible Array cell: overwrite, hole-fill inside
    // the allocated dense size, or append at `length`.
    PutElem ->
      case stack {
        [val, k, recv, ..rest] -> {
          let store = ffi.put_elem(agent.store, recv, k, val)
          case ffi.is_miss(store) {
            True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [val, ..rest],
                locals,
                Agent(..agent, store:),
                code,
                constants,
                line,
              )
          }
        }
        _ -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // -- Named data property ------------------------------------------------
    // `obj.x`: own or inherited plain data property along an all-ordinary
    // chain (`undefined` when absent on the whole chain, as OrdinaryGet
    // answers). A string or number receiver reads its wrapper prototype
    // (String "length" is answered directly). Accessors, proxies,
    // namespaces, an object cell's virtual `length` miss.
    GetField(key.Named(name)) ->
      case stack {
        [recv, ..rest] -> {
          let v = ffi.get_field(agent, recv, name)
          case ffi.is_miss(v) {
            True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [v, ..rest],
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        }
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // Like GetField but keeps the receiver beneath the value for CallMethod.
    GetField2(key.Named(name)) ->
      case stack {
        [recv, ..rest] -> {
          let v = ffi.get_field(agent, recv, name)
          case ffi.is_miss(v) {
            True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [v, recv, ..rest],
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        }
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // `obj.x = v` on an existing own writable data property replaces the
    // value inside the descriptor, keeping attributes and creation order
    // (§10.1.11); on an extensible ordinary receiver whose prototype chain
    // holds nothing but writable data at the key it creates the property
    // (fresh seq from the store). Setters and read-only props up the chain,
    // non-writable, accessors and exotic receivers miss.
    PutField(key.Named(name)) ->
      case stack {
        [val, recv, ..rest] -> {
          let store = ffi.put_field(agent.store, recv, name, val)
          case ffi.is_miss(store) {
            True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [val, ..rest],
                locals,
                Agent(..agent, store:),
                code,
                constants,
                line,
              )
          }
        }
        _ -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // -- Object literal construction ---------------------------------------
    // `{}`: a plain allocation with %Object.prototype%, never observable.
    NewObject -> {
      let #(h, agent) =
        rt_obj.t_new_object(agent, Some(agent.realm.object.prototype))
      fast_loop(
        state,
        drive,
        pc + 1,
        [mk_object(h), ..stack],
        locals,
        agent,
        code,
        constants,
        line,
      )
    }

    TypeOf ->
      case stack {
        [v, ..rest] -> {
          let t = ffi.type_of_in(agent.store, v)
          case ffi.is_miss(t) {
            True -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [mk_string(t), ..rest],
                locals,
                agent,
                code,
                constants,
                line,
              )
          }
        }
        [] -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // `{name: v}` static-key definition on a fresh literal: an ordinary,
    // extensible SObject takes a raw own insert (CreateDataProperty on such a
    // target can neither trap nor fail). Anything else goes through step's
    // full [[DefineOwnProperty]].
    DefineField(key.Named(name)) ->
      case stack {
        [val, obj, ..rest] ->
          case define_plain(agent, obj, Named(name), val) {
            Ok(agent) ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [obj, ..rest],
                locals,
                agent,
                code,
                constants,
                line,
              )
            Error(Nil) ->
              dispatch_slow(state, drive, pc, stack, locals, agent, line)
          }
        _ -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // [arg_n, .., arg_1, callee, ..] → the callee's frame or [result, ..].
    Call(arity) ->
      case pop_n(stack, arity) {
        Some(#(args, [callee, ..rest])) ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            line,
            callee,
            mk_undefined(),
            args,
            rest,
          )
        _ -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // [arg_n, .., arg_1, method, receiver, ..]: this = receiver.
    CallMethod(arity) ->
      case pop_n(stack, arity) {
        Some(#(args, [method, receiver, ..rest])) ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            line,
            method,
            receiver,
            args,
            rest,
          )
        _ -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
      }

    // A plain call frame returning to a caller: no return override, so the
    // caller is restored straight from the registers. Constructor frames and
    // the activation root take the general path.
    Return ->
      case state.call_stack {
        [SavedFrame(constructor_this: None, ..) as saved, ..rest_frames]
          if !state.func.is_derived_constructor
        -> {
          let value = case stack {
            [v, ..] -> v
            [] -> mk_undefined()
          }
          let new_state =
            call.return_to(agent, state.outer_depth, saved, rest_frames, value)
          fast_loop(
            new_state,
            drive,
            new_state.pc,
            new_state.stack,
            new_state.locals,
            new_state.agent,
            new_state.code,
            new_state.constants,
            call.current_line(new_state.agent),
          )
        }
        _ ->
          after_step(
            call.return_op(
              State(
                ..state,
                pc:,
                stack:,
                locals:,
                agent: call.set_line(agent, line),
              ),
            ),
            drive,
          )
      }

    _other -> dispatch_slow(state, drive, pc, stack, locals, agent, line)
  }
}

/// Call/CallMethod from the loop's registers. A plain native callee (not
/// call/apply/Reflect.apply, which re-dispatch) runs under the loop's own
/// guard and depth bracket and the loop continues with its result; the
/// State is only materialised for a throw. A plain same-realm bytecode
/// callee has its frame built from the registers (`call.call_plain`) and
/// the loop enters it directly. Every other callee takes the general call
/// path with the cell already read.
fn fast_call(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  line: Int,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest: List(JsVal),
) -> Result(#(Outcome, State), VmError) {
  let agent = call.set_line(agent, line)
  case classify(callee) {
    KHandle(h) ->
      case rt_store.t_cell_get(agent, h) {
        SObject(kind: KNative(tag:, ..), ..)
          if tag != function_call
          && tag != function_apply
          && tag != reflect_apply
          && agent.store.call_depth < limits.max_call_depth
        -> {
          let agent = rt_store.t_enter_call(agent)
          case ffi.guard4(rt_builtins.dispatch_native, agent, tag, this, args) {
            ffi.Ok(value: v, agent:) ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [v, ..rest],
                locals,
                rt_store.t_leave_call(agent),
                code,
                constants,
                line,
              )
            ffi.Threw(agent:, thrown:) ->
              after_step(
                Error(Threw(
                  thrown,
                  State(
                    ..state,
                    pc:,
                    stack: rest,
                    locals:,
                    agent: rt_store.t_leave_call(agent),
                  ),
                )),
                drive,
              )
          }
        }
        SObject(
          kind: KBytecode(
            template:,
            env:,
            home_object:,
            flags:,
            realm:,
            unit:,
            ..,
          ),
          ..,
        ) as slot ->
          case
            realm == agent.realm.id
            && !template.is_class_constructor
            && !template.is_generator
            && !template.is_async
          {
            True ->
              case
                call.call_plain(
                  state,
                  pc,
                  locals,
                  agent,
                  h,
                  template,
                  unit,
                  env,
                  home_object,
                  flags,
                  args,
                  rest,
                  this,
                )
              {
                Ok(callee) ->
                  fast_loop(
                    callee,
                    drive,
                    0,
                    [],
                    callee.locals,
                    callee.agent,
                    callee.code,
                    callee.constants,
                    0,
                  )
                Error(exit) -> after_step(Error(exit), drive)
              }
            False ->
              after_step(
                call.call_cell(
                  State(..state, pc:, stack:, locals:, agent:),
                  h,
                  slot,
                  this,
                  args,
                  rest,
                  drive,
                ),
                drive,
              )
          }
        slot ->
          after_step(
            call.call_cell(
              State(..state, pc:, stack:, locals:, agent:),
              h,
              slot,
              this,
              args,
              rest,
              drive,
            ),
            drive,
          )
      }
    _ ->
      after_step(
        call.call(
          State(..state, pc:, stack:, locals:, agent:),
          callee,
          this,
          args,
          rest,
          drive,
        ),
        drive,
      )
  }
}

const function_call = FunctionN(FunctionCall)

const function_apply = FunctionN(FunctionApply)

const reflect_apply = ReflectN(ReflectApply)

/// The kernel for a resolver-classified pure binary operator: the result
/// value, or `miss` when the operands need coercion the kernel cannot see
/// through.
fn pure_binop_kernel(op: binop.PureBinOp, left: JsVal, right: JsVal) -> JsVal {
  case op {
    binop.Arith(binop.ArithSub) -> ffi.sub(left, right)
    binop.Arith(binop.ArithMul) -> ffi.mul(left, right)
    binop.Arith(binop.ArithDiv) -> ffi.div(left, right)
    binop.Arith(binop.ArithMod) -> ffi.mod(left, right)
    binop.Arith(binop.ArithExp) -> miss_value(Miss)
    binop.Bitwise(binop.AndOp) -> k_bitand(left, right)
    binop.Bitwise(binop.OrOp) -> k_bitor(left, right)
    binop.Bitwise(binop.XorOp) -> k_bitxor(left, right)
    binop.Bitwise(binop.ShlOp) -> k_shl(left, right)
    binop.Bitwise(binop.ShrOp) -> k_shr(left, right)
    binop.Bitwise(binop.UShrOp) -> k_ushr(left, right)
    binop.Compare(binop.LtCmp) -> tri(ffi.lt(left, right))
    binop.Compare(binop.LtEqCmp) -> tri(ffi.le(left, right))
    binop.Compare(binop.GtCmp) -> tri(ffi.gt(left, right))
    binop.Compare(binop.GtEqCmp) -> tri(ffi.ge(left, right))
    binop.Equality(binop.StrictEqOp) -> mk_bool(ffi.strict_eq(left, right))
    binop.Equality(binop.StrictNotEqOp) -> mk_bool(!ffi.strict_eq(left, right))
    binop.Equality(binop.EqOp) -> tri(ffi.eq(left, right))
    binop.Equality(binop.NotEqOp) -> {
      let r = tri(ffi.eq(left, right))
      case ffi.is_miss(r) {
        True -> r
        False -> mk_bool(!ffi.truthy(r))
      }
    }
  }
}

/// §7.3.5 CreateDataProperty on an ordinary, extensible `SObject` under a
/// string key: a fresh `{W, E, C: true}` data property is inserted, or
/// replaces a configurable one in place (§10.1.6.3: any configurable
/// current accepts the new descriptor; the key keeps its creation order,
/// §10.1.11). `Error(Nil)` when the receiver is anything else (array,
/// proxy, non-extensible, shaped or not an object) or the current property
/// is non-configurable, so the caller takes the full [[DefineOwnProperty]].
fn define_plain(
  agent: Agent,
  obj: JsVal,
  pk: rt_types.PropertyKey,
  val: JsVal,
) -> Result(Agent, Nil) {
  use <- bool.guard(!is_handle(obj), Error(Nil))
  let h = as_handle(obj)
  case rt_store.t_cell_get(agent, h) {
    SObject(kind: rt_types.Ordinary, extensible: True, props:, ..) as slot -> {
      let current = case dict.get(props, pk) {
        Ok(old) ->
          case rt_types.prop_configurable(old) {
            True -> Ok(#(rt_types.prop_seq(old), agent))
            False -> Error(Nil)
          }
        Error(Nil) -> Ok(rt_store.t_next_prop_seq(agent))
      }
      use #(seq, agent) <- result.map(current)
      let prop =
        DataProperty(
          value: val,
          writable: True,
          enumerable: True,
          configurable: True,
          seq:,
        )
      rt_store.t_cell_set(
        agent,
        h,
        SObject(..slot, props: dict.insert(props, pk, prop)),
      )
    }
    _ -> Error(Nil)
  }
}

/// The value in the box cell a captured local holds, `Error(Nil)` for the
/// TDZ sentinel or a local that is not a box handle.
fn box_value(agent: Agent, slot: JsVal) -> Result(JsVal, Nil) {
  case is_handle(slot) {
    False -> Error(Nil)
    True ->
      case rt_store.t_cell_get(agent, as_handle(slot)) {
        SBox(value:) ->
          case is_tdz(value) {
            True -> Error(Nil)
            False -> Ok(value)
          }
        _ -> Error(Nil)
      }
  }
}

/// `v + delta` for a Number local (the IncLocal/DecLocal kernel): the unary
/// plus kernel gates on Number so a string local never concatenates.
fn number_step(v: JsVal, delta: Int) -> JsVal {
  let n = ffi.plus(v)
  case ffi.is_miss(n) {
    True -> n
    False -> ffi.add(n, int_val(delta))
  }
}

/// Materialise the State from the fast loop's registers, record the current
/// source line on the innermost `Agent.frames` entry, and run one instruction
/// through the general `step` dispatcher.
fn dispatch_slow(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  line: Int,
) -> Result(#(Outcome, State), VmError) {
  let state =
    State(..state, pc:, stack:, locals:, agent: call.set_line(agent, line))
  after_step(
    step(state, drive, tuple_array.get_unchecked(state.pc, state.code)),
    drive,
  )
}

/// Continue the loop after one stepped instruction: re-enter it on the new
/// State, finish on Return, park a coroutine, or unwind a throw to its
/// handler.
fn after_step(
  stepped: Result(State, StepExit),
  drive: Drive,
) -> Result(#(Outcome, State), VmError) {
  case stepped {
    Ok(new_state) -> execute_inner(new_state, drive)
    Error(Returned(value, post)) ->
      Ok(#(Completed(NormalCompletion(value)), post))
    Error(VmFailed(err, _)) -> Error(err)
    Error(Yielded(kind, yielded_value, post)) -> {
      // Build the parked state. It MUST spread from `post`, not the
      // pre-step `state`: user code run by the step (iter.next, a `done` /
      // `value` getter) may have changed the agent. Yield/Await handlers
      // keep pc and the caller's stack shape, so the fixups below are valid
      // against `post`.
      let parked = case kind {
        // InitialYield: stack unchanged, just advance pc.
        InitialSuspend -> State(..post, pc: post.pc + 1)
        // Yield: pop the yielded value, advance pc.
        PlainYield ->
          State(
            ..post,
            stack: case post.stack {
              [_, ..rest] -> rest
              [] -> []
            },
            pc: post.pc + 1,
          )
        // YieldStar: pop the arg (keep the iterator), keep pc here so the
        // resume re-executes YieldStar with [resume_val, iter, ..].
        DelegateYield ->
          State(..post, stack: case post.stack {
            [_arg, ..rest] -> rest
            [] -> []
          })
        // AsyncYieldStarResume: [result_obj, iter, ..] with result_obj fully
        // consumed. Drop it so the parked stack is [iter, ..]; the resume
        // pushes the .next(v) arg and pc jumps back to Next.
        AsyncDelegateResume(next_pc:) ->
          State(..post, pc: next_pc, stack: case post.stack {
            [_result_obj, ..rest] -> rest
            [] -> []
          })
      }
      Ok(#(Suspended(state.Yield, yielded_value), parked))
    }
    Error(Awaited(awaited_value, post)) -> {
      // Async body hit await: pop the operand, advance pc.
      let parked =
        State(
          ..post,
          stack: case post.stack {
            [_, ..rest] -> rest
            [] -> []
          },
          pc: post.pc + 1,
        )
      Ok(#(Suspended(state.Await, awaited_value), parked))
    }
    Error(Threw(thrown, post)) ->
      // Try to land on a catch handler. The full post-step state (stack/pc
      // included) is threaded so opcodes can mutate the stack before
      // throwing (undef the iter slot, then propagate).
      case unwind_to_catch(post, thrown) {
        Some(caught) -> execute_inner(caught, drive)
        None -> Ok(#(Completed(ThrowCompletion(thrown)), post))
      }
  }
}

/// Truncate the operand stack down to `depth` elements: the try-frame
/// unwinder's primitive (the depth was recorded at PushTry time).
pub fn truncate_stack(stack: List(JsVal), depth: Int) -> List(JsVal) {
  let excess = list.length(stack) - depth
  case excess > 0 {
    True -> list.drop(stack, excess)
    False -> stack
  }
}

/// Find a catch handler for a thrown value. Walks up the call stack when the
/// current frame's try_stack is exhausted (restoring each caller frame and
/// its depth / stack-frame bookkeeping through `call.unwind_frame`), so a
/// throw from a callee can be caught by a try/catch in the caller.
pub fn unwind_to_catch(state: State, thrown: JsVal) -> Option(State) {
  case state.try_stack {
    // `kind` only matters to the return-completion unwinder; a *throw* lands
    // at catch_target no matter what the frame guards.
    [TryFrame(catch_target:, stack_depth:, kind: _), ..rest_try] ->
      Some(
        State(
          ..state,
          stack: [thrown, ..truncate_stack(state.stack, stack_depth)],
          try_stack: rest_try,
          pc: catch_target,
        ),
      )
    [] -> option.then(call.unwind_frame(state), unwind_to_catch(_, thrown))
  }
}

fn underflow(state: State, op: String) -> Result(a, StepExit) {
  Error(VmFailed(StackUnderflow(op), state))
}

/// Pop top of stack and jump to `target` if `condition(value)` holds,
/// otherwise advance to the next instruction.
fn conditional_jump(
  state: State,
  target: Int,
  condition: fn(JsVal) -> Bool,
) -> Result(State, StepExit) {
  case state.stack {
    [top, ..rest] ->
      case condition(top) {
        True -> Ok(State(..state, stack: rest, pc: target))
        False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
      }
    [] -> underflow(state, "ConditionalJump")
  }
}

// ============================================================================
// Step — single instruction dispatch
// ============================================================================

/// Execute a single instruction. `Ok(new_state)` continues the loop; a
/// `StepExit` leaves it: a thrown value (`Threw`), the frame's normal
/// completion (`Returned`), a suspension (`Yielded` / `Awaited`), or a broken
/// engine invariant (`VmFailed`). Every exit carries the state to resume or
/// unwind from.
fn step(state: State, drive: Drive, op: Op) -> Result(State, StepExit) {
  case op {
    // ---- Source mapping ----------------------------------------------
    SetLine(line) ->
      Ok(
        State(
          ..state,
          agent: call.set_line(state.agent, line),
          pc: state.pc + 1,
        ),
      )

    // ---- Stack operations --------------------------------------------
    PushConst(index) -> {
      let value = tuple_array.get_unchecked(index, state.constants)
      Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
    }

    Pop ->
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "Pop")
      }

    Dup ->
      case state.stack {
        [top, ..] ->
          Ok(State(..state, stack: [top, ..state.stack], pc: state.pc + 1))
        [] -> underflow(state, "Dup")
      }

    Swap ->
      case state.stack {
        [a, b, ..rest] ->
          Ok(State(..state, stack: [b, a, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Swap")
      }

    // [a, b, c, ..] → [c, a, b, ..]: bring the 3rd element to the top.
    Rot3 ->
      case state.stack {
        [a, b, c, ..rest] ->
          Ok(State(..state, stack: [c, a, b, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Rot3")
      }

    // [a, b, c, d, ..] → [b, c, d, a, ..]: bury the top under the next three.
    Unrot4 ->
      case state.stack {
        [a, b, c, d, ..rest] ->
          Ok(State(..state, stack: [b, c, d, a, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Unrot4")
      }

    // ---- Local variable access ---------------------------------------
    GetLocal(index) -> {
      let value = tuple_array.get_unchecked(index, state.locals)
      case is_tdz(value) {
        True -> tdz_reference_error(state)
        False ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
      }
    }

    PutLocal(index) ->
      case state.stack {
        [value, ..rest] -> {
          let locals = tuple_array.set_unchecked(index, value, state.locals)
          Ok(State(..state, stack: rest, locals:, pc: state.pc + 1))
        }
        [] -> underflow(state, "PutLocal")
      }

    // §9.1.1.3.1 BindThisValue: derived-ctor `this` may be bound exactly once.
    PutLocalCheckInit(index) ->
      case state.stack {
        [value, ..rest] ->
          case is_tdz(tuple_array.get_unchecked(index, state.locals)) {
            True -> {
              let locals = tuple_array.set_unchecked(index, value, state.locals)
              Ok(
                State(
                  ..state,
                  stack: rest,
                  locals:,
                  this: value,
                  pc: state.pc + 1,
                ),
              )
            }
            False ->
              state.throw_reference_error(
                state,
                "'this' is already initialized",
              )
          }
        [] -> underflow(state, "PutLocalCheckInit")
      }

    BoxLocal(index) -> {
      let current = tuple_array.get_unchecked(index, state.locals)
      let #(box, agent) = rt_store.t_cell_new(state.agent, SBox(current))
      let locals =
        tuple_array.set_unchecked(index, mk_object(box), state.locals)
      Ok(State(..state, agent:, locals:, pc: state.pc + 1))
    }

    GetBoxed(index) ->
      case read_box(state, tuple_array.get_unchecked(index, state.locals)) {
        Some(value) ->
          case is_tdz(value) {
            True -> tdz_reference_error(state)
            False ->
              Ok(
                State(..state, stack: [value, ..state.stack], pc: state.pc + 1),
              )
          }
        None ->
          Error(VmFailed(InternalError("GetBoxed", "local is not a box"), state))
      }

    PutBoxed(index) ->
      case state.stack {
        [new_value, ..rest] ->
          case handle_of(tuple_array.get_unchecked(index, state.locals)) {
            Some(box) -> {
              let agent = rt_store.t_cell_set(state.agent, box, SBox(new_value))
              Ok(State(..state, agent:, stack: rest, pc: state.pc + 1))
            }
            None ->
              Error(VmFailed(
                InternalError("PutBoxed", "local is not a box"),
                state,
              ))
          }
        [] -> underflow(state, "PutBoxed")
      }

    // BindThisValue when `this` is captured by an arrow inside the ctor.
    PutBoxedCheckInit(index) ->
      case state.stack {
        [new_value, ..rest] -> {
          let slot = tuple_array.get_unchecked(index, state.locals)
          case handle_of(slot), read_box(state, slot) {
            Some(box), Some(current) ->
              case is_tdz(current) {
                True -> {
                  let agent =
                    rt_store.t_cell_set(state.agent, box, SBox(new_value))
                  Ok(
                    State(
                      ..state,
                      agent:,
                      stack: rest,
                      this: new_value,
                      pc: state.pc + 1,
                    ),
                  )
                }
                False ->
                  state.throw_reference_error(
                    state,
                    "'this' is already initialized",
                  )
              }
            _, _ ->
              Error(VmFailed(
                InternalError("PutBoxedCheckInit", "local is not a box"),
                state,
              ))
          }
        }
        [] -> underflow(state, "PutBoxedCheckInit")
      }

    // ---- Global variable access --------------------------------------
    // §9.1.1.4.4 GetBindingValue: declarative record, then object record.
    GetGlobal(name) ->
      case lex_lookup(state.agent, name) {
        Some(binding) -> {
          let value = rt_types.lexical_global_value(binding)
          case is_tdz(value) {
            True ->
              state.throw_reference_error(
                state,
                "Cannot access '" <> name <> "' before initialization",
              )
            False ->
              Ok(
                State(..state, stack: [value, ..state.stack], pc: state.pc + 1),
              )
          }
        }
        None -> {
          use #(value, state) <- result.map(global_object_get(state, name))
          State(..state, stack: [value, ..state.stack], pc: state.pc + 1)
        }
      }

    // §9.1.1.4.5 SetMutableBinding: declarative record, then object record.
    PutGlobal(name) ->
      case state.stack {
        [value, ..rest] ->
          case lex_lookup(state.agent, name) {
            // const → assignment rejected (even in TDZ, per spec ordering).
            Some(rt_types.Const(_)) ->
              state.throw_type_error(state, "Assignment to constant variable.")
            Some(rt_types.Let(current)) ->
              case is_tdz(current) {
                True ->
                  state.throw_reference_error(
                    state,
                    "Cannot access '" <> name <> "' before initialization",
                  )
                False ->
                  Ok(
                    State(
                      ..state,
                      agent: lex_write(state.agent, name, rt_types.Let(value)),
                      stack: rest,
                      pc: state.pc + 1,
                    ),
                  )
              }
            None -> {
              use state <- result.map(global_object_put(
                State(..state, stack: rest),
                name,
                value,
              ))
              State(..state, pc: state.pc + 1)
            }
          }
        [] -> underflow(state, "PutGlobal")
      }

    // §9.1.1.4.7 DeleteBinding on the global record: the static fallback of
    // a sloppy `delete identifier`. Lexical bindings are never deletable
    // (false without touching the object record); otherwise a real
    // [[Delete]] on the global object.
    DeleteGlobalVar(name) ->
      case lex_lookup(state.agent, name) {
        Some(_) ->
          Ok(
            State(
              ..state,
              stack: [mk_bool(False), ..state.stack],
              pc: state.pc + 1,
            ),
          )
        None -> {
          use #(deleted, state) <- result.map(rt2(
            state,
            rt_env.t_delete_global_var,
            name,
          ))
          State(
            ..state,
            stack: [mk_bool(deleted), ..state.stack],
            pc: state.pc + 1,
          )
        }
      }

    // §9.1.1.4.17 CreateGlobalVarBinding(N, D). Scripts pass D = false
    // (bindings survive `delete`); eval code passes D = true (§19.2.1.3).
    DeclareGlobalVar(name, deletable) -> {
      use state <- result.map(rt_unit3(
        state,
        rt_env.t_create_global_var_binding,
        name,
        deletable,
      ))
      State(..state, pc: state.pc + 1)
    }

    DeclareGlobalFn(name, deletable) -> {
      use state <- result.map(rt_unit3(
        state,
        rt_env.t_create_global_fn_binding,
        name,
        deletable,
      ))
      State(..state, pc: state.pc + 1)
    }

    // Sloppy direct-eval var access: the frame's eval scope, then globals.
    GetEvalVar(name) ->
      case lookup_eval_env(state, name) {
        Some(v) ->
          Ok(State(..state, stack: [v, ..state.stack], pc: state.pc + 1))
        None -> step(state, drive, GetGlobal(name))
      }

    TypeofEvalVar(name) ->
      case lookup_eval_env(state, name) {
        Some(v) -> {
          let #(t, _) = rt_val.t_type_of(state.agent, v)
          Ok(
            State(
              ..state,
              stack: [mk_string(t), ..state.stack],
              pc: state.pc + 1,
            ),
          )
        }
        None -> step(state, drive, TypeofGlobal(name))
      }

    // Sloppy direct-eval var write: the eval scope if it declares the name,
    // else PutGlobal.
    PutEvalVar(name) ->
      case state.eval_env, state.stack {
        Some(env), [v, ..rest] ->
          case rt_env.eval_env_has(state.agent, env, name) {
            False -> step(state, drive, PutGlobal(name))
            True ->
              Ok(
                State(
                  ..state,
                  agent: rt_env.t_eval_env_set(state.agent, env, name, v),
                  stack: rest,
                  pc: state.pc + 1,
                ),
              )
          }
        _, _ -> step(state, drive, PutGlobal(name))
      }

    // Sloppy direct-eval var declaration: seed name = undefined into the
    // eval scope. With no scope allocated for this frame the var falls
    // through to the global object; §19.2.1.3 EvalDeclarationInstantiation
    // uses D = true for eval code, so such globals ARE deletable.
    DeclareEvalVar(name) ->
      case state.eval_env {
        None -> step(state, drive, DeclareGlobalVar(name, deletable: True))
        Some(env) ->
          Ok(
            State(
              ..state,
              agent: rt_env.t_eval_env_declare(state.agent, env, name),
              pc: state.pc + 1,
            ),
          )
      }

    // §7.1.17 ToString for template substitutions (string hint).
    opcode.ToStringVal ->
      case state.stack {
        [val, ..rest] ->
          case classify(val) {
            KStr(_) -> Ok(State(..state, pc: state.pc + 1))
            _ -> {
              use #(s, state) <- result.map(rt2(state, rt_val.t_to_string, val))
              State(..state, stack: [mk_string(s), ..rest], pc: state.pc + 1)
            }
          }
        [] -> underflow(state, "ToStringVal")
      }

    // §13.2.8.4 GetTemplateObject: the per-site cached frozen template
    // array (with its frozen `raw`), created on first evaluation. The site
    // key is `"<unit>#<site>"`, the shape compiled modules use with their
    // module name as the unit.
    opcode.GetTemplateObject(site, quasis) -> {
      let cooked =
        list.map(quasis, fn(q) {
          option.map(q.cooked, mk_string) |> option.unwrap(mk_undefined())
        })
      let raw = list.map(quasis, fn(q) { q.raw })
      let #(tpl, agent) =
        rt_lang.t_get_template_object(
          state.agent,
          int.to_string(state.unit) <> "#" <> int.to_string(site),
          cooked,
          raw,
        )
      Ok(State(..state, agent:, stack: [tpl, ..state.stack], pc: state.pc + 1))
    }

    // §7.1.19 ToPropertyKey: class-definition-time coercion of computed
    // field names (§15.7.14 ClassFieldDefinitionEvaluation step 1).
    // Symbols pass through, everything else is ToString'd via
    // ToPrimitive(string).
    opcode.ToPropertyKey ->
      case state.stack {
        [val, ..rest] ->
          case classify(val) {
            KStr(_) | KSym(_) -> Ok(State(..state, pc: state.pc + 1))
            _ -> {
              use #(prim, state) <- result.try(rt3(
                state,
                rt_val.t_to_primitive,
                val,
                HintString,
              ))
              case classify(prim) {
                KSym(_) ->
                  Ok(State(..state, stack: [prim, ..rest], pc: state.pc + 1))
                _ -> {
                  use #(s, state) <- result.map(rt2(
                    state,
                    rt_val.t_to_string,
                    prim,
                  ))
                  State(
                    ..state,
                    stack: [mk_string(s), ..rest],
                    pc: state.pc + 1,
                  )
                }
              }
            }
          }
        [] -> underflow(state, "ToPropertyKey")
      }

    // §7.1.18 ToObject for the `with (expr)` head.
    opcode.ToObject ->
      case state.stack {
        [val, ..rest] -> {
          use #(h, state) <- result.map(rt2(state, rt_val.t_to_object, val))
          State(..state, stack: [mk_object(h), ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "ToObject")
      }

    // §9.1.1.2.1 HasBinding + §9.1.1.2.6 GetBindingValue against a with
    // object. Found: replace obj with the value and jump. Not found (or
    // @@unscopables-blocked): pop obj, fall through.
    opcode.WithGetVar(name, Pc(target)) ->
      with_get_var(state, name, target, keep_this: False, op: "WithGetVar")

    // Like WithGetVar, keeping the with object beneath the value as the call
    // receiver (§13.3.6.2 EvaluateCall step 1.b.ii).
    opcode.WithGetVarThis(name, Pc(target)) ->
      with_get_var(state, name, target, keep_this: True, op: "WithGetVarThis")

    // §9.1.1.2.5 SetMutableBinding against a with object. Stack:
    // [obj, value, ..]. Found: Set(obj, name, value), pop both, jump.
    // Not found: pop obj, fall through to the ordinary store.
    opcode.WithPutVar(name, Pc(target)) ->
      case state.stack {
        [obj, val, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            Some(h) -> {
              use #(bound, state) <- result.try(rt3(
                state,
                rt_env.t_with_has_binding,
                h,
                name,
              ))
              case bound {
                False ->
                  Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
                True -> {
                  use state <- result.map(rt_unit5(
                    state,
                    rt_env.t_with_set_mutable_binding,
                    h,
                    name,
                    val,
                    state.func.is_strict,
                  ))
                  State(..state, stack: rest, pc: target)
                }
              }
            }
          }
        _ -> underflow(state, "WithPutVar")
      }

    // §9.1.1.2.7 DeleteBinding against a with object. Found: replace obj
    // with the [[Delete]] result and jump. Not found: pop obj, fall through.
    opcode.WithDeleteVar(name, Pc(target)) ->
      case state.stack {
        [obj, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            Some(h) -> {
              use #(bound, state) <- result.try(rt3(
                state,
                rt_env.t_with_has_binding,
                h,
                name,
              ))
              case bound {
                False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
                True -> {
                  use #(deleted, state) <- result.map(rt3(
                    state,
                    rt_env.t_with_delete_binding,
                    h,
                    name,
                  ))
                  State(..state, stack: [mk_bool(deleted), ..rest], pc: target)
                }
              }
            }
          }
        [] -> underflow(state, "WithDeleteVar")
      }

    // §9.1.2.1 GetIdentifierReference at a with object: HasBinding only.
    // Bound: KEEP obj (the reference base) and jump. Not bound: pop obj.
    opcode.WithMakeRef(name, Pc(target)) ->
      case state.stack {
        [obj, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            Some(h) -> {
              use #(bound, state) <- result.map(rt3(
                state,
                rt_env.t_with_has_binding,
                h,
                name,
              ))
              case bound {
                True -> State(..state, stack: [obj, ..rest], pc: target)
                False -> State(..state, stack: rest, pc: state.pc + 1)
              }
            }
          }
        [] -> underflow(state, "WithMakeRef")
      }

    // §9.1.1.2.6 GetBindingValue on a made reference base. Object base:
    // HasProperty re-check then Get; undefined sentinel: pop, fall through.
    opcode.WithGetRefValue(name, Pc(target)) ->
      case state.stack {
        [obj, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            Some(h) -> {
              use #(val, state) <- result.map(rt4(
                state,
                rt_env.t_with_get_binding_value,
                h,
                name,
                state.func.is_strict,
              ))
              State(..state, stack: [val, ..rest], pc: target)
            }
          }
        [] -> underflow(state, "WithGetRefValue")
      }

    // §9.1.1.2.5 SetMutableBinding on a made reference base. Stack:
    // [base, value, ..]. Object base: still-exists re-check then Set on the
    // ORIGINAL base; undefined sentinel: pop, fall through.
    opcode.WithPutRefValue(name, Pc(target)) ->
      case state.stack {
        [obj, val, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            Some(h) -> {
              use state <- result.map(rt_unit5(
                state,
                rt_env.t_with_set_mutable_binding,
                h,
                name,
                val,
                state.func.is_strict,
              ))
              State(..state, stack: rest, pc: target)
            }
          }
        _ -> underflow(state, "WithPutRefValue")
      }

    // §9.1.1.4.16 CreateGlobalLexBinding: a TDZ slot tagged const/let.
    DeclareGlobalLex(name, is_const) ->
      Ok(
        State(
          ..state,
          agent: lex_write(state.agent, name, case is_const {
            True -> rt_types.Const(mk_tdz())
            False -> rt_types.Let(mk_tdz())
          }),
          pc: state.pc + 1,
        ),
      )

    // Initialise a lexical global (TDZ → value), keeping its const/let tag.
    InitGlobalLex(name) ->
      case state.stack {
        [val, ..rest] -> {
          // No prior DeclareGlobalLex: default to let.
          let binding = case lex_lookup(state.agent, name) {
            Some(existing) -> rt_types.lexical_global_with_value(existing, val)
            None -> rt_types.Let(val)
          }
          Ok(
            State(
              ..state,
              agent: lex_write(state.agent, name, binding),
              stack: rest,
              pc: state.pc + 1,
            ),
          )
        }
        [] -> underflow(state, "InitGlobalLex")
      }

    // `using` / `await using` desugar: CreateDisposableResource(V, hint) —
    // pop the resource value, push its disposer callable (or undefined for
    // null/undefined). TypeError for non-disposable values.
    opcode.GetDisposer(is_async:) ->
      case state.stack {
        [val, ..rest] -> {
          let state = State(..state, stack: rest, pc: state.pc + 1)
          use #(disposer, state) <- result.map(rt4(
            state,
            using_disposer,
            val,
            is_async,
            state.unit,
          ))
          State(..state, stack: [disposer, ..state.stack])
        }
        [] -> underflow(state, "GetDisposer")
      }

    // `using` / `await using` desugar: DisposeResources error folding — pop
    // suppressed, pop error, push new SuppressedError(error, suppressed).
    opcode.MakeSuppressed ->
      case state.stack {
        [suppressed, err, ..rest] -> {
          let #(suppressed_error, agent) =
            rt_error.make_suppressed_error(state.agent, err, suppressed)
          Ok(
            State(
              ..state,
              agent:,
              stack: [suppressed_error, ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        _ -> underflow(state, "MakeSuppressed")
      }

    TypeOf ->
      case state.stack {
        [val, ..rest] -> {
          let #(t, _) = rt_val.t_type_of(state.agent, val)
          Ok(State(..state, stack: [mk_string(t), ..rest], pc: state.pc + 1))
        }
        [] -> underflow(state, "TypeOf")
      }

    // §9.1.1.4: typeof on globals: TDZ throws, undeclared is "undefined".
    TypeofGlobal(name) ->
      case lex_lookup(state.agent, name) {
        Some(binding) -> {
          let value = rt_types.lexical_global_value(binding)
          case is_tdz(value) {
            True ->
              state.throw_reference_error(
                state,
                "Cannot access '" <> name <> "' before initialization",
              )
            False -> {
              let #(t, _) = rt_val.t_type_of(state.agent, value)
              Ok(
                State(
                  ..state,
                  stack: [mk_string(t), ..state.stack],
                  pc: state.pc + 1,
                ),
              )
            }
          }
        }
        None -> {
          use #(t, state) <- result.map(rt2(
            state,
            rt_obj.t_global_typeof,
            bit_array.from_string(name),
          ))
          State(..state, stack: [mk_string(t), ..state.stack], pc: state.pc + 1)
        }
      }

    // ---- Operators ---------------------------------------------------
    BinOp(kind) ->
      case state.stack {
        [right, left, ..rest] ->
          case kind {
            opcode.InstanceOfOp -> {
              use #(r, state) <- result.map(rt3(
                state,
                rt_ops.t_instance_of,
                left,
                right,
              ))
              State(..state, stack: [mk_bool(r == 1), ..rest], pc: state.pc + 1)
            }
            // left = key, right = object. §13.10.1: ToPropertyKey(lval),
            // then HasProperty(rval, key); a non-object rval is a TypeError.
            opcode.InOp ->
              case is_object(right) {
                True -> {
                  use #(r, state) <- result.map(rt3(
                    state,
                    rt_ops.t_in,
                    left,
                    right,
                  ))
                  State(
                    ..state,
                    stack: [mk_bool(r == 1), ..rest],
                    pc: state.pc + 1,
                  )
                }
                False ->
                  state.throw_type_error(
                    state,
                    "Cannot use 'in' operator to search for '"
                      <> inspect(state, left)
                      <> "' in "
                      <> inspect(state, right),
                  )
              }
            // §13.15.3: ToPrimitive(default) both sides, then string-concat
            // or numeric add.
            opcode.AddOp -> {
              use #(r, state) <- result.map(rt3(
                state,
                rt_ops.t_add,
                left,
                right,
              ))
              State(..state, stack: [r, ..rest], pc: state.pc + 1)
            }
            opcode.PureOp(op) -> {
              use #(r, state) <- result.map(pure_binop_slow(
                state,
                op,
                left,
                right,
              ))
              State(..state, stack: [r, ..rest], pc: state.pc + 1)
            }
          }
        _ -> underflow(state, "BinOp")
      }

    UnaryOp(kind) ->
      case state.stack {
        [operand, ..rest] -> {
          use #(r, state) <- result.map(unaryop_slow(state, kind, operand))
          State(..state, stack: [r, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "UnaryOp")
      }

    // ---- Fused superinstructions (resolver peephole) -------------------
    IncLocal(index) -> fused_update_local(state, index, True)
    DecLocal(index) -> fused_update_local(state, index, False)

    CmpLocalLocalJump(left_idx, right_idx, kind, Pc(target)) -> {
      let left = tuple_array.get_unchecked(left_idx, state.locals)
      let right = tuple_array.get_unchecked(right_idx, state.locals)
      case is_tdz(left) || is_tdz(right) {
        True -> tdz_reference_error(state)
        False -> fused_cmp_jump(state, kind, left, right, target)
      }
    }

    CmpLocalConstJump(left_idx, const_index, kind, Pc(target)) -> {
      let left = tuple_array.get_unchecked(left_idx, state.locals)
      case is_tdz(left) {
        True -> tdz_reference_error(state)
        False ->
          fused_cmp_jump(
            state,
            kind,
            left,
            tuple_array.get_unchecked(const_index, state.constants),
            target,
          )
      }
    }

    // ---- Control flow ------------------------------------------------
    Return -> call.return_op(state)

    Jump(Pc(target)) -> Ok(State(..state, pc: target))

    JumpIfFalse(Pc(target)) -> {
      use v <- conditional_jump(state, target)
      !ffi.truthy(v)
    }

    JumpIfTrue(Pc(target)) -> conditional_jump(state, target, ffi.truthy)

    JumpIfNullish(Pc(target)) -> conditional_jump(state, target, ffi.nullish)

    // QuickJS OP_gosub: push return-PC as a number, jump to the finally body.
    opcode.Gosub(Pc(target)) ->
      Ok(
        State(
          ..state,
          stack: [int_val(state.pc + 1), ..state.stack],
          pc: target,
        ),
      )

    // QuickJS OP_ret: pop return-PC, jump back to it. A negative retpc is the
    // sentinel pushed by generator .return() finally-unwinding: "the slot
    // below me is a return value, complete the frame with it".
    opcode.Ret ->
      case state.stack {
        [ret_pc, ..rest] ->
          case classify(ret_pc), rest {
            KNum(rt_types.JInt(n)), [slot, ..below] if n < 0 ->
              Error(Returned(slot, State(..state, stack: below)))
            KNum(rt_types.JInt(n)), _ -> Ok(State(..state, stack: rest, pc: n))
            KNum(rt_types.JFloat(f)), [slot, ..below] if f <. 0.0 ->
              Error(Returned(slot, State(..state, stack: below)))
            KNum(rt_types.JFloat(f)), _ ->
              Ok(State(..state, stack: rest, pc: rt_val.float_to_int(f)))
            _, _ -> underflow(state, "Ret")
          }
        [] -> underflow(state, "Ret")
      }

    // -- Exception handling --
    PushTry(catch_target: Pc(catch_target), kind:) -> {
      let frame =
        TryFrame(catch_target:, stack_depth: list.length(state.stack), kind:)
      Ok(
        State(..state, try_stack: [frame, ..state.try_stack], pc: state.pc + 1),
      )
    }

    opcode.PopTry ->
      case state.try_stack {
        [_, ..rest] -> Ok(State(..state, try_stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "PopTry: empty try_stack")
      }

    opcode.Throw ->
      case state.stack {
        [value, ..] -> Error(Threw(value, state))
        [] -> underflow(state, "Throw")
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
      let #(h, agent) =
        rt_obj.t_new_object(
          state.agent,
          Some(state.agent.realm.object.prototype),
        )
      Ok(
        State(
          ..state,
          agent:,
          stack: [mk_object(h), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    GetField(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(get_field(state, receiver, k))
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField")
      }

    // Like GetField but keeps the object beneath the value for CallMethod.
    GetField2(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(get_field(state, receiver, k))
          State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField2")
      }

    // Consumes [value, obj] and pushes value back (assignment is an
    // expression).
    PutField(k) ->
      case state.stack {
        [value, receiver, ..rest] ->
          case classify(receiver) {
            KHandle(_) -> {
              use #(ok, state) <- result.try(rt4(
                state,
                rt_obj.t_set_prop,
                receiver,
                okey(k),
                value,
              ))
              // §13.15.2 PutValue step 6.b.iv: a failed [[Set]] throws
              // TypeError in strict mode; sloppy mode ignores the failure.
              case ok, state.func.is_strict {
                False, True ->
                  state.throw_type_error(
                    state,
                    "Cannot assign to read only property '"
                      <> key.key_display_string(k)
                      <> "' of object",
                  )
                _, _ ->
                  Ok(State(..state, stack: [value, ..rest], pc: state.pc + 1))
              }
            }
            // §6.2.5.6 PutValue step 5.a: ToObject(undefined|null) throws in
            // BOTH modes.
            KUndef | KNull ->
              state.throw_type_error(
                state,
                "Cannot set properties of "
                  <> rt_val.nullish_label(receiver)
                  <> " (setting '"
                  <> key.key_display_string(k)
                  <> "')",
              )
            // Primitive base: §13.15.2 PutValue 6.b.iv, strict throws
            // TypeError, sloppy silently ignores.
            _ ->
              case state.func.is_strict {
                True ->
                  state.throw_type_error(
                    state,
                    "Cannot create property '"
                      <> key.key_display_string(k)
                      <> "' on primitive value",
                  )
                False ->
                  Ok(State(..state, stack: [value, ..rest], pc: state.pc + 1))
              }
          }
        _ -> underflow(state, "PutField")
      }

    // §15.7.14 step 5/6: mint a fresh PrivateName for this class evaluation.
    // The minted storage-key text travels as a string value.
    NewPrivateName(name) -> {
      let #(k, agent) = rt_class_new_private_name(state.agent, name)
      Ok(State(..state, agent:, stack: [k, ..state.stack], pc: state.pc + 1))
    }

    // §7.3.30 PrivateGet: [key, obj, ..] → [val, ..]. Own-only lookup.
    GetPrivateFieldDyn ->
      case state.stack {
        [k, obj, ..rest] -> {
          use #(val, state) <- result.map(private_get(state, obj, k))
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetPrivateFieldDyn")
      }

    // As GetPrivateFieldDyn, keeping the receiver beneath the value.
    GetPrivateFieldDyn2 ->
      case state.stack {
        [k, obj, ..rest] -> {
          use #(val, state) <- result.map(private_get(state, obj, k))
          State(..state, stack: [val, obj, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetPrivateFieldDyn2")
      }

    // §7.3.31 PrivateSet. [key, val, obj, ..] → [val, ..]. Own-only.
    PutPrivateFieldDyn ->
      case state.stack {
        [k, val, obj, ..rest] -> {
          use #(v, state) <- result.map(private_set(state, obj, k, val))
          State(..state, stack: [v, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "PutPrivateFieldDyn")
      }

    // §13.10.1 `#x in obj`. [key, obj, ..] → [bool, ..]. Own-only check.
    PrivateInDyn ->
      case state.stack {
        [k, obj, ..rest] -> {
          use #(found, state) <- result.map(
            call.guarded(state, fn(agent) {
              #(private_in(agent, obj, k), agent)
            }),
          )
          State(..state, stack: [mk_bool(found), ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "PrivateInDyn")
      }

    // §7.3.28 PrivateFieldAdd. [val, key, obj, ..] → [obj, ..].
    DefinePrivateField ->
      case state.stack {
        [val, k, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use state <- result.map(private_define_field(state, h, k, val))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            None -> Ok(State(..state, stack: [obj, ..rest], pc: state.pc + 1))
          }
        _ -> underflow(state, "DefinePrivateField")
      }

    // §7.3.29 PrivateMethodOrAccessorAdd (method). [fn, key, obj, ..] →
    // [obj, ..]. Non-writable so PrivateSet's method check trips.
    DefinePrivateMethod ->
      case state.stack {
        [func, k, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use state <- result.map(private_define_method(
                state,
                h,
                k,
                func,
                rt_types.MIMethod,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            None -> Ok(State(..state, stack: [obj, ..rest], pc: state.pc + 1))
          }
        _ -> underflow(state, "DefinePrivateMethod")
      }

    // §7.3.29 for one accessor half. [fn, key, obj, ..] → [obj, ..]. The
    // get and set halves of one class evaluation merge; a half already
    // present is double initialisation → TypeError.
    DefinePrivateAccessor(kind) ->
      case state.stack {
        [func, k, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              let install = case kind {
                opcode.Getter -> rt_types.MIGetter
                opcode.Setter -> rt_types.MISetter
              }
              use state <- result.map(private_define_method(
                state,
                h,
                k,
                func,
                install,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            None -> Ok(State(..state, stack: [obj, ..rest], pc: state.pc + 1))
          }
        _ -> underflow(state, "DefinePrivateAccessor")
      }

    // Like PutField but keeps the object on the stack (object literal
    // construction, class fields). §7.3.7 CreateDataPropertyOrThrow: an OWN
    // define that never walks the prototype chain or invokes inherited
    // setters; a proxy receiver fires its defineProperty trap and a
    // frozen / non-extensible receiver throws TypeError.
    DefineField(k) ->
      case state.stack {
        [value, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) ->
              case okey(k) {
                StringKey(pk) ->
                  case define_plain(state.agent, obj, pk, value) {
                    Ok(agent) ->
                      Ok(
                        State(
                          ..state,
                          agent:,
                          stack: [obj, ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    Error(Nil) -> {
                      use state <- result.map(create_data_property_or_throw(
                        state,
                        h,
                        StringKey(pk),
                        value,
                      ))
                      State(..state, stack: [obj, ..rest], pc: state.pc + 1)
                    }
                  }
                symbol_key -> {
                  use state <- result.map(create_data_property_or_throw(
                    state,
                    h,
                    symbol_key,
                    value,
                  ))
                  State(..state, stack: [obj, ..rest], pc: state.pc + 1)
                }
              }
            // DefineField on non-object: no-op, keep object on stack.
            None -> Ok(State(..state, pc: state.pc + 1))
          }
        _ -> underflow(state, "DefineField")
      }

    // Class method: a non-enumerable, writable, configurable data property
    // with [[HomeObject]] = target (§15.4.4 MakeMethod).
    DefineMethod(k) ->
      case state.stack {
        [func, obj, ..rest] ->
          case handle_of(obj), handle_of(func) {
            Some(target), Some(fn_h) -> {
              use state <- result.map(rt_unit6(
                state,
                rt_class_define_method,
                target,
                okey(k),
                fn_h,
                rt_types.MIMethod,
                False,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            _, _ -> Ok(State(..state, stack: [obj, ..rest], pc: state.pc + 1))
          }
        _ -> underflow(state, "DefineMethod")
      }

    // Computed class method: [fn, key, obj, ..] → [obj, ..].
    DefineMethodComputed ->
      case state.stack {
        [func, k, obj, ..rest] ->
          case handle_of(obj), handle_of(func) {
            Some(target), Some(fn_h) -> {
              use #(pk, state) <- result.try(rt2(
                state,
                rt_val.t_to_property_key,
                k,
              ))
              use state <- result.map(rt_unit6(
                state,
                rt_class_define_method,
                target,
                pk,
                fn_h,
                rt_types.MIMethod,
                False,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            _, _ -> Ok(State(..state, stack: [obj, ..rest], pc: state.pc + 1))
          }
        _ -> underflow(state, "DefineMethodComputed")
      }

    // Object literal / class getter or setter: [fn, obj, ..] → [obj, ..].
    DefineAccessor(k, kind, enumerable) ->
      case state.stack {
        [func, obj, ..rest] ->
          case handle_of(obj), handle_of(func) {
            Some(target), Some(fn_h) -> {
              use state <- result.map(rt_unit6(
                state,
                rt_class_define_method,
                target,
                okey(k),
                fn_h,
                accessor_install_kind(kind),
                enumerable,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            _, _ -> Ok(State(..state, stack: [obj, ..rest], pc: state.pc + 1))
          }
        _ -> underflow(state, "DefineAccessor")
      }

    // Computed getter/setter: [fn, key, obj, ..] → [obj, ..].
    DefineAccessorComputed(kind, enumerable) ->
      case state.stack {
        [func, k, obj, ..rest] ->
          case handle_of(obj), handle_of(func) {
            Some(target), Some(fn_h) -> {
              use #(pk, state) <- result.try(rt2(
                state,
                rt_val.t_to_property_key,
                k,
              ))
              use state <- result.map(rt_unit6(
                state,
                rt_class_define_method,
                target,
                pk,
                fn_h,
                accessor_install_kind(kind),
                enumerable,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            _, _ -> Ok(State(..state, stack: [obj, ..rest], pc: state.pc + 1))
          }
        _ -> underflow(state, "DefineAccessorComputed")
      }

    // §15.4.4: set top-of-stack closure's [[HomeObject]] to the object
    // directly beneath it. Stack-neutral; DefineField/Computed follows.
    MakeMethod ->
      case state.stack {
        [func, obj, ..] ->
          case handle_of(obj) {
            Some(target) ->
              Ok(
                State(
                  ..state,
                  agent: make_method(state.agent, func, target),
                  pc: state.pc + 1,
                ),
              )
            None -> Ok(State(..state, pc: state.pc + 1))
          }
        _ -> underflow(state, "MakeMethod")
      }

    // Object literal computed key {[key]: value}: [value, key, obj, ..] →
    // [obj, ..]. ToPropertyKey, then CreateDataPropertyOrThrow.
    DefineFieldComputed ->
      case state.stack {
        [val, k, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use #(pk, state) <- result.try(rt2(
                state,
                rt_val.t_to_property_key,
                k,
              ))
              use state <- result.map(create_data_property_or_throw(
                state,
                h,
                pk,
                val,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            // Non-object target: pop and keep going.
            None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
          }
        _ -> underflow(state, "DefineFieldComputed")
      }

    // Annex B §B.3.1 `{__proto__: v}`: [val, obj, ..] → [obj, ..]. Object or
    // null sets [[Prototype]]; anything else is ignored. The target is a
    // fresh literal, so the set never fails.
    SetProto ->
      case state.stack {
        [val, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use #(_, state) <- result.map(rt3(
                state,
                rt_obj.t_set_proto,
                h,
                val,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            None -> Ok(State(..state, stack: [obj, ..rest], pc: state.pc + 1))
          }
        _ -> underflow(state, "SetProto")
      }

    // Object spread {...source}: [source, obj, ..] → [obj, ..].
    // CopyDataProperties; null/undefined/primitives are a no-op.
    ObjectSpread ->
      case state.stack {
        [source, obj, ..rest] ->
          case is_object(obj) {
            True -> {
              use #(_, state) <- result.map(rt3(
                state,
                rt_lang.t_copy_data_props,
                obj,
                source,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
          }
        _ -> underflow(state, "ObjectSpread")
      }

    // Destructuring rest `let {a, b, ...rest} = src`: [src, key_n, ..,
    // key_1, ..] → [rest_obj, ..]. §13.15.5.3: CopyDataProperties with
    // excludedNames = the n keys already bound.
    ObjectRestCopy(excluded_count) ->
      case state.stack {
        [source, ..below] ->
          case pop_n(below, excluded_count) {
            Some(#(raw_keys, rest)) -> {
              let state = State(..state, stack: rest)
              // §8.6.2 RequireObjectCoercible: unlike spread, `let {...x} =
              // null` MUST throw.
              case classify(source) {
                KNull ->
                  state.throw_type_error(
                    state,
                    "Cannot destructure 'null' as it is null.",
                  )
                KUndef ->
                  state.throw_type_error(
                    state,
                    "Cannot destructure 'undefined' as it is undefined.",
                  )
                _ -> {
                  // ToPropertyKey each excluded key (computed keys arrive as
                  // raw values; static keys are already strings).
                  use #(keys, state) <- result.try(
                    to_property_keys(state, raw_keys, []),
                  )
                  use #(obj, state) <- result.map(rt3(
                    state,
                    rt_lang.t_object_rest,
                    source,
                    keys,
                  ))
                  State(..state, stack: [obj, ..state.stack], pc: state.pc + 1)
                }
              }
            }
            None -> underflow(state, "ObjectRestCopy")
          }
        _ -> underflow(state, "ObjectRestCopy")
      }

    // -- Delete operator --
    DeleteField(k) ->
      case state.stack {
        [obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use #(deleted, state) <- result.try(rt3(
                state,
                rt_obj.t_delete_prop,
                h,
                okey(k),
              ))
              // §13.5.1.2 step 5.b.i: strict delete of a non-configurable
              // property throws TypeError.
              case deleted, state.func.is_strict {
                False, True ->
                  state.throw_type_error(
                    state,
                    "Cannot delete property '"
                      <> key.key_display_string(k)
                      <> "'",
                  )
                _, _ ->
                  Ok(
                    State(
                      ..state,
                      stack: [mk_bool(deleted), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
              }
            }
            // delete on non-object returns true.
            None ->
              Ok(
                State(..state, stack: [mk_bool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ -> underflow(state, "DeleteField")
      }

    DeleteElem ->
      case state.stack {
        [k, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use #(pk, state) <- result.try(rt2(
                state,
                rt_val.t_to_property_key,
                k,
              ))
              use #(deleted, state) <- result.try(rt3(
                state,
                rt_obj.t_delete_prop,
                h,
                pk,
              ))
              case deleted, state.func.is_strict {
                False, True ->
                  state.throw_type_error(state, "Cannot delete property")
                _, _ ->
                  Ok(
                    State(
                      ..state,
                      stack: [mk_bool(deleted), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
              }
            }
            None ->
              Ok(
                State(..state, stack: [mk_bool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ -> underflow(state, "DeleteElem")
      }

    // -- Class inheritance --
    // [ctor, parent, ..] → [ctor, ..]. §15.7.14 step 5: IsConstructor before
    // Get(superclass, "prototype"); wire ctor.prototype.[[Prototype]] =
    // protoParent, ctor.[[HomeObject]] = ctor.prototype and ctor.[[Prototype]]
    // = parent (static inheritance).
    SetupDerivedClass ->
      case state.stack {
        [ctor, parent, ..rest] ->
          case handle_of(ctor) {
            None ->
              state.throw_type_error(
                state,
                "Class extends value is not a constructor or null",
              )
            Some(ctor_h) -> {
              use #(proto_parent, state) <- result.try(class_proto_parent(
                state,
                parent,
              ))
              let agent = state.agent
              let ctor_proto = own_prototype_handle(agent, ctor_h)
              let agent =
                option.map(ctor_proto, set_home_object(agent, ctor_h, _))
                |> option.unwrap(agent)
              let agent =
                option.map(ctor_proto, set_slot_prototype(
                  agent,
                  _,
                  proto_parent,
                ))
                |> option.unwrap(agent)
              let agent = case handle_of(parent) {
                Some(parent_h) ->
                  set_slot_prototype(agent, ctor_h, Some(parent_h))
                None -> agent
              }
              Ok(
                State(..state, agent:, stack: [ctor, ..rest], pc: state.pc + 1),
              )
            }
          }
        _ ->
          state.throw_type_error(
            state,
            "Class extends value is not a constructor or null",
          )
      }

    // ---- Array operations --------------------------------------------
    ArrayFrom(count) ->
      case pop_n(state.stack, count) {
        Some(#(items, rest)) -> {
          let #(arr, agent) = rt_obj.t_new_array(state.agent, items)
          Ok(State(..state, agent:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        None -> underflow(state, "ArrayFrom")
      }

    // Pop only the non-hole values, then lay them out at their non-hole
    // indices. The emitter guarantees `holes` is non-empty, ascending, and
    // within [0, count).
    ArrayFromWithHoles(count, holes) ->
      case pop_n(state.stack, count - list.length(holes)) {
        Some(#(values, rest)) -> {
          let items = fill_holes(values, holes, 0, count, [])
          let #(arr, agent) = rt_obj.t_new_array(state.agent, items)
          Ok(State(..state, agent:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        None -> underflow(state, "ArrayFromWithHoles")
      }

    // -- Computed property access --
    GetElem ->
      case state.stack {
        [k, receiver, ..rest] ->
          case classify(receiver) {
            KUndef | KNull ->
              state.throw_type_error(
                state,
                "Cannot read properties of " <> rt_val.nullish_label(receiver),
              )
            _ -> {
              use #(pk, state) <- result.try(rt2(
                state,
                rt_val.t_to_property_key,
                k,
              ))
              use #(val, state) <- result.map(rt3(
                state,
                rt_obj.t_get_prop,
                receiver,
                pk,
              ))
              State(..state, stack: [val, ..rest], pc: state.pc + 1)
            }
          }
        _ -> underflow(state, "GetElem")
      }

    // Like GetElem but keeps obj+key: [key, obj, ..] → [value, key, obj, ..]
    // for compound assignment, where ToPropertyKey runs exactly ONCE
    // (§13.15.2): the key left for the later PutElem is the converted key.
    // RequireObjectCoercible on the base comes first.
    GetElem2 ->
      case state.stack {
        [k, receiver, ..rest] ->
          case classify(receiver) {
            KUndef | KNull ->
              state.throw_type_error(
                state,
                "Cannot read properties of " <> rt_val.nullish_label(receiver),
              )
            _ -> {
              use #(pk, state) <- result.try(rt2(
                state,
                rt_val.t_to_property_key,
                k,
              ))
              use #(val, state) <- result.map(rt3(
                state,
                rt_obj.t_get_prop,
                receiver,
                pk,
              ))
              State(
                ..state,
                stack: [val, prop_key_value(pk), receiver, ..rest],
                pc: state.pc + 1,
              )
            }
          }
        _ -> underflow(state, "GetElem2")
      }

    // [value, key, obj, ..] → [value, ..].
    PutElem ->
      case state.stack {
        [val, k, receiver, ..rest] ->
          case classify(receiver) {
            KHandle(_) -> {
              use #(pk, state) <- result.try(rt2(
                state,
                rt_val.t_to_property_key,
                k,
              ))
              use #(ok, state) <- result.try(rt4(
                state,
                rt_obj.t_set_prop,
                receiver,
                pk,
                val,
              ))
              case ok, state.func.is_strict {
                False, True ->
                  state.throw_type_error(
                    state,
                    "Cannot assign to read only property of object",
                  )
                _, _ ->
                  Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
              }
            }
            KUndef | KNull ->
              state.throw_type_error(
                state,
                "Cannot set properties of undefined or null",
              )
            _ ->
              case state.func.is_strict {
                True ->
                  state.throw_type_error(
                    state,
                    "Cannot create property on primitive value",
                  )
                False ->
                  Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
              }
          }
        _ -> underflow(state, "PutElem")
      }

    // -- Spread element support (array literals + calls) --
    // [val, arr] → [arr]; arr[arr.length] = val.
    ArrayPush ->
      case state.stack {
        [val, arr, ..rest] ->
          case handle_of(arr) {
            Some(h) ->
              Ok(
                State(
                  ..state,
                  agent: array_push(state.agent, h, Some(val)),
                  stack: [arr, ..rest],
                  pc: state.pc + 1,
                ),
              )
            None -> underflow(state, "ArrayPush")
          }
        _ -> underflow(state, "ArrayPush")
      }

    // [arr] → [arr]; length++ without setting any element.
    ArrayPushHole ->
      case state.stack {
        [arr, ..rest] ->
          case handle_of(arr) {
            Some(h) ->
              Ok(
                State(
                  ..state,
                  agent: array_push(state.agent, h, None),
                  stack: [arr, ..rest],
                  pc: state.pc + 1,
                ),
              )
            None -> underflow(state, "ArrayPushHole")
          }
        _ -> underflow(state, "ArrayPushHole")
      }

    // [iterable, arr] → [arr]; drain the iterable through the protocol.
    ArraySpread ->
      case state.stack {
        [iterable, arr, ..rest] ->
          case handle_of(arr) {
            Some(h) -> {
              use #(items, state) <- result.map(rt3(
                state,
                rt_lang.t_spread_into_list,
                [],
                iterable,
              ))
              let agent = array_append(state.agent, h, items)
              State(..state, agent:, stack: [arr, ..rest], pc: state.pc + 1)
            }
            None -> underflow(state, "ArraySpread")
          }
        _ -> underflow(state, "ArraySpread")
      }

    // ---- Function calls ----------------------------------------------
    // Syntactic `eval(...)`. If the callee IS the intrinsic %eval%, run a
    // DIRECT eval (sees the caller's locals through their boxes); if eval
    // was shadowed or rebound, ordinary Call semantics.
    CallEval(arity, param_scope_names, with_names, private_names) ->
      case pop_n(state.stack, arity) {
        Some(#(args, [callee, ..rest_stack])) ->
          case global_fns.is_intrinsic_eval(state.agent, callee) {
            False -> step(state, drive, Call(arity))
            True -> {
              let #(res, new_state) =
                eval.direct_eval(
                  State(..state, stack: rest_stack),
                  args,
                  param_scope_names,
                  with_names,
                  private_names,
                  run_activation(_, drive),
                )
              // Continue (or unwind) from `new_state`: it carries the agent
              // the eval ran with and any eval scope it had to allocate.
              case res {
                Ok(val) ->
                  Ok(
                    State(
                      ..new_state,
                      stack: [val, ..new_state.stack],
                      pc: state.pc + 1,
                    ),
                  )
                Error(thrown) -> Error(Threw(thrown, new_state))
              }
            }
          }
        _ -> step(state, drive, Call(arity))
      }

    // [arg_n, .., arg_1, callee, ..] → the callee's frame or [result, ..].
    Call(arity) ->
      case pop_n(state.stack, arity) {
        Some(#(args, [callee, ..rest_stack])) ->
          call.call(state, callee, mk_undefined(), args, rest_stack, drive)
        Some(#(_, [])) -> underflow(state, "Call: no callee")
        None -> underflow(state, "Call: not enough args")
      }

    // [arg_n, .., arg_1, method, receiver, ..]: this = receiver.
    CallMethod(arity) ->
      case pop_n(state.stack, arity) {
        Some(#(args, [method, receiver, ..rest_stack])) ->
          call.call(state, method, receiver, args, rest_stack, drive)
        Some(#(_, _)) -> underflow(state, "CallMethod")
        None -> underflow(state, "CallMethod: not enough args")
      }

    // [arg_n, .., arg_1, new_target, ctor, ..].
    CallConstructor(arity) ->
      case pop_n(state.stack, arity) {
        Some(#(args, [new_target, ctor, ..rest_stack])) ->
          call.construct(state, ctor, args, rest_stack, new_target, drive)
        Some(#(_, _)) -> underflow(state, "CallConstructor")
        None -> underflow(state, "CallConstructor: not enough args")
      }

    // [args_array, callee] → [result]; this = undefined.
    CallApply ->
      case state.stack {
        [args_arr, callee, ..rest] ->
          call.call(
            state,
            callee,
            mk_undefined(),
            call.array_values(state.agent, args_arr),
            rest,
            drive,
          )
        _ -> underflow(state, "CallApply")
      }

    // [args_array, method, receiver] → [result]; this = receiver.
    CallMethodApply ->
      case state.stack {
        [args_arr, method, receiver, ..rest] ->
          call.call(
            state,
            method,
            receiver,
            call.array_values(state.agent, args_arr),
            rest,
            drive,
          )
        _ -> underflow(state, "CallMethodApply")
      }

    // [args_array, new_target, ctor] → [instance]. Spread-new path.
    CallConstructorApply ->
      case state.stack {
        [args_arr, new_target, ctor, ..rest] ->
          call.construct(
            state,
            ctor,
            call.array_values(state.agent, args_arr),
            rest,
            new_target,
            drive,
          )
        _ -> underflow(state, "CallConstructorApply")
      }

    // Ordinary [[GetPrototypeOf]] read: [obj] → [proto|null]. The second hop
    // for both `super.x` (home_object → proto) and `super()` (active_func →
    // parent ctor), whose bases are never proxies.
    GetPrototypeOf ->
      case state.stack {
        [obj, ..rest] -> {
          let proto = case handle_of(obj) {
            Some(h) ->
              slot_prototype(state.agent, h)
              |> option.map(mk_object)
              |> option.unwrap(rt_types.mk_null())
            None -> rt_types.mk_null()
          }
          Ok(State(..state, stack: [proto, ..rest], pc: state.pc + 1))
        }
        [] -> underflow(state, "GetPrototypeOf")
      }

    // [key, base, this, ..] → [val, ..]. OrdinaryGet on base, receiver=this.
    GetSuperValue -> get_super_value(state, False, "GetSuperValue")

    // [key, base, this, ..] → [val, pk, base, this, ..]: ToPropertyKey ONCE,
    // Get with receiver = this, leaving the coerced key + base + this for
    // the trailing PutSuperValue.
    GetSuperValue2 -> get_super_value(state, True, "GetSuperValue2")

    // [val, key, base, this, ..] → [val, ..]. OrdinarySet, receiver = this.
    PutSuperValue ->
      case state.stack {
        [val, k, base, this_val, ..rest] ->
          case handle_of(base) {
            Some(base_h) -> {
              use #(pk, state) <- result.try(rt2(
                state,
                rt_val.t_to_property_key,
                k,
              ))
              use #(ok, state) <- result.try(rt5(
                state,
                rt_obj.t_set_prop_with_receiver,
                base_h,
                pk,
                val,
                this_val,
              ))
              // §6.2.5.6 PutValue step 5.c: gated on caller strictness so
              // sloppy object-literal methods stay non-throwing.
              case ok, state.func.is_strict {
                False, True ->
                  state.throw_type_error(
                    state,
                    "Cannot assign to read-only super property",
                  )
                _, _ ->
                  Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
              }
            }
            None ->
              state.throw_type_error(
                state,
                "Cannot write super property when prototype is null",
              )
          }
        _ -> underflow(state, "PutSuperValue")
      }

    // Capture values from the current frame according to env_descriptors.
    // For boxed captured vars the local holds the box handle: copying it
    // shares the cell.
    MakeClosure(func_index) -> {
      let template = tuple_array.get_unchecked(func_index, state.func.functions)
      let captured =
        list.map(template.env_descriptors, fn(desc) {
          tuple_array.get_unchecked(desc.parent_index, state.locals)
        })
      let #(fn_h, agent) =
        make_closure(state.agent, template, captured, state.unit)
      Ok(
        State(
          ..state,
          agent:,
          stack: [mk_object(fn_h), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    // ---- Iteration ---------------------------------------------------
    // EnumerateObjectProperties (§14.7.5.6): the key list is computed up
    // front (own keys, enumerability and each level's [[GetPrototypeOf]] all
    // trap for proxies; a namespace TDZ export throws before iteration) and
    // parked in a ForInIterator cell.
    ForInStart ->
      case state.stack {
        [obj, ..rest] -> {
          use #(keys, state) <- result.map(rt2(state, rt_obj.t_for_in_keys, obj))
          let names =
            list.filter_map(keys, fn(k) {
              case classify(k) {
                KStr(s) -> Ok(s)
                _ -> Error(Nil)
              }
            })
          let #(iter, agent) =
            rt_store.t_cell_new(
              state.agent,
              SObject(
                kind: ForInIterator(remaining: names),
                proto: None,
                props: dict.new(),
                symbol_props: [],
                elements: NoElements,
                extensible: False,
              ),
            )
          State(
            ..state,
            agent:,
            stack: [mk_object(iter), ..rest],
            pc: state.pc + 1,
          )
        }
        _ -> underflow(state, "ForInStart")
      }

    // [iter, ..] → [done, key, iter, ..].
    ForInNext ->
      case state.stack {
        [iter, ..rest] ->
          case for_in_remaining(state.agent, iter) {
            Some(#(h, [name, ..remaining])) -> {
              let agent =
                rt_store.t_cell_update(state.agent, h, fn(slot) {
                  case slot {
                    SObject(..) ->
                      SObject(..slot, kind: ForInIterator(remaining:))
                    _ -> slot
                  }
                })
              Ok(
                State(
                  ..state,
                  agent:,
                  stack: [mk_bool(False), mk_string(name), iter, ..rest],
                  pc: state.pc + 1,
                ),
              )
            }
            Some(#(_, [])) ->
              Ok(
                State(
                  ..state,
                  stack: [mk_bool(True), mk_undefined(), iter, ..rest],
                  pc: state.pc + 1,
                ),
              )
            None ->
              Error(VmFailed(
                InternalError("ForInNext", "not a ForInIterator"),
                state,
              ))
          }
        _ -> underflow(state, "ForInNext")
      }

    // §7.4.1 GetIterator(obj, sync): look up and call @@iterator, cache
    // `next` in an Iterator Record. [iterable] → [record].
    GetIterator ->
      case state.stack {
        [iterable, ..rest] -> {
          use #(rec, state) <- result.map(rt3(
            state,
            rt_lang.t_get_iterator,
            iterable,
            rt_lang.Sync,
          ))
          State(..state, stack: [rec, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetIterator")
      }

    // §7.4.3 GetIterator(obj, async): @@asyncIterator, else @@iterator
    // wrapped by CreateAsyncFromSyncIterator. Pushes the ITERATOR object;
    // `next` is read by what follows (IteratorRecord for `yield*`, each
    // step of a for-await loop), not here.
    GetAsyncIterator ->
      case state.stack {
        [iterable, ..rest] -> {
          use #(iterator, state) <- result.map(rt2(
            state,
            async_iterator_object,
            iterable,
          ))
          State(..state, stack: [iterator, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetAsyncIterator")
      }

    // §7.4.4 GetIteratorFromMethod step 4: cache the iterator's `next` in an
    // Iterator Record. The Get is observable; abrupt completions propagate.
    IteratorRecord ->
      case state.stack {
        [iterator, ..rest] ->
          case is_object(iterator) {
            True -> {
              use #(rec, state) <- result.map(
                call.guarded(state, fn(agent) {
                  let #(record, agent) =
                    iter_protocol.get_iterator_direct(
                      agent,
                      iterator,
                      "Iterator is not an object",
                    )
                  rt_lang.t_alloc_record(agent, record)
                }),
              )
              State(..state, stack: [rec, ..rest], pc: state.pc + 1)
            }
            False ->
              state.throw_type_error(
                state,
                inspect(state, iterator) <> " is not an object",
              )
          }
        _ -> underflow(state, "IteratorRecord")
      }

    // [rec, ..] → [done, value, rec', ..]. [[Done]] tracking (QuickJS-
    // style): on done OR an abrupt .next(), the record slot becomes
    // undefined so later IteratorNext short-circuits and IteratorClose /
    // CloseThrow no-op (§7.4.11 / §7.4.6). §7.4.8: `value` is not read
    // when done.
    IteratorNext ->
      case state.stack {
        [rec, ..rest] ->
          case is_undef(rec) {
            True ->
              Ok(
                State(
                  ..state,
                  stack: [mk_bool(True), mk_undefined(), rec, ..rest],
                  pc: state.pc + 1,
                ),
              )
            False -> {
              let record = rt_lang.record_parts(state.agent, rec)
              case fast_iter_step(state.agent, record) {
                ArrayStep(done, val, agent) -> {
                  let slot = case done {
                    True -> mk_undefined()
                    False -> rec
                  }
                  Ok(
                    State(
                      ..state,
                      agent:,
                      stack: [mk_bool(done), val, slot, ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                }
                fast ->
                  case iter_step(state, drive, rec, fast) {
                    Ok(#(#(done, val), state)) -> {
                      let slot = case done {
                        True -> mk_undefined()
                        False -> rec
                      }
                      Ok(
                        State(
                          ..state,
                          stack: [mk_bool(done), val, slot, ..rest],
                          pc: state.pc + 1,
                        ),
                      )
                    }
                    Error(exit) ->
                      Error(
                        state.map_exit_state(exit, fn(s) {
                          State(..s, stack: [mk_undefined(), ..rest])
                        }),
                      )
                  }
              }
            }
          }
        _ -> underflow(state, "IteratorNext")
      }

    // §7.4.11 normal-completion close. [rec, ..] → [..]. An undefined slot
    // ([[Done]]) is a no-op. Only ever sees an Iterator Record: a for-await
    // loop's bare async iterator is closed by open-coded bytecode.
    IteratorClose ->
      case state.stack {
        [rec, ..rest] -> {
          let state = State(..state, stack: rest, pc: state.pc + 1)
          case is_undef(rec) {
            True -> Ok(state)
            False -> rt_unit3(state, rt_lang.t_iter_close, rec, False)
          }
        }
        [] -> underflow(state, "IteratorClose")
      }

    // §7.4.11 throw-completion close. [thrown, rec, ..] → rethrows. The
    // original error wins whatever .return() does.
    IteratorCloseThrow ->
      case state.stack {
        [thrown, rec, ..rest] -> {
          let state = State(..state, stack: rest)
          case is_undef(rec) {
            True -> Error(Threw(thrown, state))
            False ->
              case rt_unit3(state, rt_lang.t_iter_close, rec, True) {
                Ok(state) -> Error(Threw(thrown, state))
                Error(Threw(_, state)) -> Error(Threw(thrown, state))
                Error(other) -> Error(other)
              }
          }
        }
        _ -> underflow(state, "IteratorCloseThrow")
      }

    // §13.15.5.3 / §14.3.3 rest element. [rec, ..] → [arr, ..]. Drains via
    // .next() without re-GetIterator; the emitter popped the close guard so
    // a .next() throw propagates without IteratorClose. An undefined slot
    // ([[Done]]) yields an empty array.
    IteratorRest ->
      case state.stack {
        [rec, ..rest] -> {
          let state = State(..state, stack: rest, pc: state.pc + 1)
          case is_undef(rec) {
            True -> {
              let #(arr, agent) = rt_obj.t_new_array(state.agent, [])
              Ok(State(..state, agent:, stack: [arr, ..rest]))
            }
            False -> {
              use #(arr, state) <- result.map(rt2(
                state,
                rt_lang.t_iter_rest,
                rec,
              ))
              State(..state, stack: [arr, ..rest])
            }
          }
        }
        [] -> underflow(state, "IteratorRest")
      }

    // §7.4.12 step 6 / §14.7.5.6 step 6.c: an awaited iterator result must
    // be an Object. Peeks.
    IteratorCheckObject ->
      case state.stack {
        [v, ..] ->
          case is_object(v) {
            True -> Ok(State(..state, pc: state.pc + 1))
            False ->
              state.throw_type_error(state, "Iterator result is not an object")
          }
        [] -> underflow(state, "IteratorCheckObject")
      }

    // ---- Generator/async ---------------------------------------------
    // Suspend immediately at the start of a generator body; the driver
    // advances pc past this op.
    InitialYield -> Error(Yielded(InitialSuspend, mk_undefined(), state))

    // Pop the value and suspend; on resume the sent value is pushed.
    Yield ->
      case state.stack {
        [yielded, ..] -> Error(Yielded(PlainYield, yielded, state))
        [] -> Error(Yielded(PlainYield, mk_undefined(), state))
      }

    // Self-looping delegate: [arg, rec, ..]. Calls the record's cached
    // `next` (§27.5.3.8 step 7.a.i); done → push value, pc+1; !done → yield
    // the value with pc kept HERE so the resume re-enters with
    // [resume_val, rec, ..]. `.throw`/`.return` reach the delegate through
    // `entry.resume_frame`, which finds the record on the parked stack.
    YieldStar ->
      case state.stack {
        [arg, slot, ..rest] -> {
          use #(iterator, next_fn, state) <- result.try(delegate_target(
            state,
            slot,
          ))
          use #(#(done, val), state) <- result.try(delegate_step(
            state,
            drive,
            iterator,
            next_fn,
            arg,
          ))
          case done {
            True -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            False -> Error(Yielded(DelegateYield, val, state))
          }
        }
        _ -> underflow(state, "YieldStar")
      }

    // [arg, rec, ..]: Call(rec.[[NextMethod]], rec.[[Iterator]], «arg»),
    // replace arg with the result → [result, rec, ..], pc+1. The following
    // Await suspends on it. A `.throw`/`.return` arriving while parked here
    // is forwarded to the delegate by `entry.resume_frame`.
    AsyncYieldStarNext(after_pc: _) ->
      case state.stack {
        [arg, slot, ..rest] -> {
          use #(iterator, next_fn, state) <- result.try(delegate_target(
            state,
            slot,
          ))
          use #(res, state) <- result.map(
            rt4(state, rt_call.t_call_checked, next_fn, iterator, [arg]),
          )
          State(..state, stack: [res, slot, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "AsyncYieldStarNext")
      }

    // [result_obj, iter, ..]: done → push value, pc+1; else yield the value.
    AsyncYieldStarResume(next_pc: Pc(next_pc)) ->
      case state.stack {
        [res, _iter, ..rest] -> {
          use #(#(done, val), state) <- result.try(rt2(
            state,
            iter_protocol.read_iter_result,
            res,
          ))
          case done {
            True -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            False -> Error(Yielded(AsyncDelegateResume(next_pc:), val, state))
          }
        }
        _ -> underflow(state, "AsyncYieldStarResume")
      }

    // Pop the awaited value and suspend the async body.
    Await ->
      case state.stack {
        [awaited, ..] -> Error(Awaited(awaited, state))
        [] -> Error(Awaited(mk_undefined(), state))
      }

    // ---- Special -----------------------------------------------------
    CreateArguments(simple_params:) ->
      Ok(call.create_arguments(state, simple_params))

    CreateRestArray(from_index) -> Ok(call.create_rest_array(state, from_index))

    // §13.2.7.3: a fresh RegExp per evaluation of the literal.
    NewRegExp ->
      case state.stack {
        [flags, pattern, ..rest] ->
          case classify(flags), classify(pattern) {
            KStr(f), KStr(p) -> {
              use #(re, state) <- result.map(rt3(
                state,
                b_regexp.regexp_create_literal,
                p,
                f,
              ))
              State(..state, stack: [re, ..rest], pc: state.pc + 1)
            }
            _, _ -> underflow(state, "NewRegExp")
          }
        _ -> underflow(state, "NewRegExp")
      }

    // -- Dynamic import (§13.3.10 ImportCall) --
    // Every failure after argument evaluation rejects the returned promise;
    // the guard only adopts the agent.
    opcode.DynamicImport ->
      case state.stack {
        [options, specifier, ..rest] -> {
          use #(promise, state) <- result.map(rt3(
            state,
            dynamic_import.import_call,
            specifier,
            options,
          ))
          State(..state, stack: [promise, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "DynamicImport")
      }

    opcode.DynamicImportSource ->
      case state.stack {
        [specifier, ..rest] -> {
          use #(promise, state) <- result.map(rt2(
            state,
            dynamic_import.source_import_call,
            specifier,
          ))
          State(..state, stack: [promise, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "DynamicImportSource")
      }

    opcode.DynamicImportDefer ->
      case state.stack {
        [specifier, ..rest] -> {
          use #(promise, state) <- result.map(rt2(
            state,
            dynamic_import.defer_import_call,
            specifier,
          ))
          State(..state, stack: [promise, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "DynamicImportDefer")
      }
  }
}

// ============================================================================
// Step helpers
// ============================================================================

/// The runtime's class ops take a bare Agent and raise; bind them by name so
/// the guard shims receive remote fun references.
fn rt_class_new_private_name(agent: Agent, name: String) -> #(JsVal, Agent) {
  rt_class.t_new_private_name(agent, name)
}

fn rt_class_define_method(
  agent: Agent,
  target: Handle,
  k: ObjectKey,
  fn_h: Handle,
  kind: rt_types.MethodInstallKind,
  enumerable: Bool,
) -> Agent {
  // MakeMethod for bytecode closures (the runtime op only knows compiled
  // cells), then the shared define.
  let agent = set_home_object(agent, fn_h, target)
  let agent = set_fn_name_if_empty(agent, fn_h, kind, k)
  rt_class.t_define_method(agent, target, k, fn_h, kind, enumerable)
}

/// §10.2.9 SetFunctionName for a computed method/accessor key: the closure
/// was compiled anonymous (its `name` is ""), so name it from the evaluated
/// key with the accessor prefix, keeping the property's creation seq.
fn set_fn_name_if_empty(
  agent: Agent,
  fn_h: Handle,
  kind: rt_types.MethodInstallKind,
  k: ObjectKey,
) -> Agent {
  let prefix = case kind {
    rt_types.MIGetter | rt_types.MIStaticGetter -> "get "
    rt_types.MISetter | rt_types.MIStaticSetter -> "set "
    rt_types.MIMethod | rt_types.MIStatic -> ""
  }
  let name = case k {
    StringKey(pk) -> rt_types.key_display_string(pk)
    SymbolKey(sym) ->
      case rt_types.symbol_description(sym) {
        Some(d) -> "[" <> d <> "]"
        None -> ""
      }
  }
  rt_store.t_cell_update(agent, fn_h, fn(slot) {
    case slot {
      SObject(kind: KBytecode(..), props:, ..) ->
        case dict.get(props, Named("name")) {
          Ok(DataProperty(value: v, seq:, ..)) ->
            case classify(v) {
              KStr("") ->
                SObject(
                  ..slot,
                  props: dict.insert(
                    props,
                    Named("name"),
                    DataProperty(
                      value: mk_string(prefix <> name),
                      writable: False,
                      enumerable: False,
                      configurable: True,
                      seq:,
                    ),
                  ),
                )
              _ -> slot
            }
          _ -> slot
        }
      _ -> slot
    }
  })
}

fn accessor_install_kind(
  kind: opcode.AccessorKind,
) -> rt_types.MethodInstallKind {
  case kind {
    opcode.Getter -> rt_types.MIGetter
    opcode.Setter -> rt_types.MISetter
  }
}

/// Same ReferenceError the GetLocal arm throws for a TDZ read; the fused
/// ops fold a GetLocal, so their TDZ path must be indistinguishable.
fn tdz_reference_error(state: State) -> Result(State, StepExit) {
  state.throw_reference_error(
    state,
    "Cannot access variable before initialization (TDZ)",
  )
}

/// The value inside a box cell held in a local, `None` if the local is not
/// a box handle.
fn read_box(state: State, slot: JsVal) -> Option(JsVal) {
  use h <- option.then(handle_of(slot))
  case rt_store.t_cell_get(state.agent, h) {
    SBox(value:) -> Some(value)
    _ -> None
  }
}

fn lookup_eval_env(state: State, name: String) -> Option(JsVal) {
  option.then(state.eval_env, rt_env.eval_env_lookup(state.agent, _, name))
}

/// GetField / GetField2 body: RequireObjectCoercible with the property name
/// in the message, then [[Get]] (primitives box, accessors run).
fn get_field(
  state: State,
  receiver: JsVal,
  k: key.PropertyKey,
) -> Result(#(JsVal, State), StepExit) {
  case classify(receiver) {
    KUndef | KNull ->
      state.throw_type_error(
        state,
        "Cannot read properties of "
          <> rt_val.nullish_label(receiver)
          <> " (reading '"
          <> key.key_display_string(k)
          <> "')",
      )
    _ -> rt3(state, rt_obj.t_get_prop, receiver, okey(k))
  }
}

/// §9.1.1.4.4 object-record half of a global read: an own data property is
/// a plain read; otherwise HasProperty (a proxy on the global's prototype
/// chain runs its `has` trap) decides between Get and ReferenceError.
fn global_object_get(
  state: State,
  name: String,
) -> Result(#(JsVal, State), StepExit) {
  let agent = state.agent
  let global = agent.realm.global_object
  let k = named(name)
  case rt_obj.t_ordinary_own_property(agent, global, k) {
    Some(DataProperty(value:, ..)) -> Ok(#(value, state))
    Some(AccessorProperty(..)) ->
      rt3(state, rt_obj.t_get_prop, mk_object(global), k)
    None -> {
      use #(has, state) <- result.try(rt3(
        state,
        rt_obj.t_has_prop,
        mk_object(global),
        k,
      ))
      case has {
        True -> rt3(state, rt_obj.t_get_prop, mk_object(global), k)
        False -> state.throw_reference_error(state, name <> " is not defined")
      }
    }
  }
}

/// §9.1.1.4.5 object-record half of a global write. Strict: the binding must
/// exist (HasProperty, trap-aware) or ReferenceError, and a rejected [[Set]]
/// is a TypeError. Sloppy: set on the global object (creates if needed;
/// a rejected set is silently ignored).
fn global_object_put(
  state: State,
  name: String,
  value: JsVal,
) -> Result(State, StepExit) {
  let global = mk_object(state.agent.realm.global_object)
  let k = named(name)
  case state.func.is_strict {
    True -> {
      use #(has, state) <- result.try(rt3(state, rt_obj.t_has_prop, global, k))
      case has {
        False -> state.throw_reference_error(state, name <> " is not defined")
        True -> {
          use #(ok, state) <- result.try(rt4(
            state,
            rt_obj.t_set_prop,
            global,
            k,
            value,
          ))
          case ok {
            True -> Ok(state)
            False ->
              state.throw_type_error(
                state,
                "Cannot assign to read only property '"
                  <> name
                  <> "' of object '#<Object>'",
              )
          }
        }
      }
    }
    False -> {
      use #(_, state) <- result.map(rt4(
        state,
        rt_obj.t_set_prop,
        global,
        k,
        value,
      ))
      state
    }
  }
}

/// §9.1.1.2.1 HasBinding + §9.1.1.2.6 GetBindingValue against a with object,
/// shared by WithGetVar and WithGetVarThis. Found: replace obj with the value
/// (keeping obj beneath as the call receiver when `keep_this`) and jump. Not
/// found (or @@unscopables-blocked): pop obj, fall through.
fn with_get_var(
  state: State,
  name: String,
  target: Int,
  keep_this keep_this: Bool,
  op op: String,
) -> Result(State, StepExit) {
  case state.stack {
    [obj, ..rest] ->
      case handle_of(obj) {
        None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        Some(h) -> {
          use #(bound, state) <- result.try(rt3(
            state,
            rt_env.t_with_has_binding,
            h,
            name,
          ))
          case bound {
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            True -> {
              use #(val, state) <- result.map(rt4(
                state,
                rt_env.t_with_get_binding_value,
                h,
                name,
                state.func.is_strict,
              ))
              let below = case keep_this {
                True -> [obj, ..rest]
                False -> rest
              }
              State(..state, stack: [val, ..below], pc: target)
            }
          }
        }
      }
    [] -> underflow(state, op)
  }
}

/// A resolver-classified pure operator on operands the kernel missed: the
/// runtime op does ToPrimitive/ToNumeric (running user code) and throws
/// the operator's TypeErrors itself.
fn pure_binop_slow(
  state: State,
  op: binop.PureBinOp,
  left: JsVal,
  right: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  let cmp = fn(f) {
    use #(r, state) <- result.map(rt3(state, f, left, right))
    #(mk_bool(r == 1), state)
  }
  case op {
    binop.Arith(binop.ArithSub) -> rt3(state, rt_ops.t_sub, left, right)
    binop.Arith(binop.ArithMul) -> rt3(state, rt_ops.t_mul, left, right)
    binop.Arith(binop.ArithDiv) -> rt3(state, rt_ops.t_div, left, right)
    binop.Arith(binop.ArithMod) -> rt3(state, rt_ops.t_mod, left, right)
    binop.Arith(binop.ArithExp) -> rt3(state, rt_ops.t_pow, left, right)
    binop.Bitwise(binop.AndOp) -> rt3(state, rt_ops.t_bitand, left, right)
    binop.Bitwise(binop.OrOp) -> rt3(state, rt_ops.t_bitor, left, right)
    binop.Bitwise(binop.XorOp) -> rt3(state, rt_ops.t_bitxor, left, right)
    binop.Bitwise(binop.ShlOp) -> rt3(state, rt_ops.t_shl, left, right)
    binop.Bitwise(binop.ShrOp) -> rt3(state, rt_ops.t_shr, left, right)
    binop.Bitwise(binop.UShrOp) -> rt3(state, rt_ops.t_ushr, left, right)
    binop.Compare(binop.LtCmp) -> cmp(rt_ops.t_lt)
    binop.Compare(binop.LtEqCmp) -> cmp(rt_ops.t_le)
    binop.Compare(binop.GtCmp) -> cmp(rt_ops.t_gt)
    binop.Compare(binop.GtEqCmp) -> cmp(rt_ops.t_ge)
    binop.Equality(binop.EqOp) -> cmp(rt_ops.t_eq)
    binop.Equality(binop.NotEqOp) -> cmp(rt_ops.t_neq)
    binop.Equality(binop.StrictEqOp) ->
      Ok(#(mk_bool(rt_ops.strict_eq(left, right)), state))
    binop.Equality(binop.StrictNotEqOp) ->
      Ok(#(mk_bool(!rt_ops.strict_eq(left, right)), state))
  }
}

/// ES2024 §13.5.4/5/6: numeric unary ops call ToNumber → ToPrimitive on
/// object operands; LogicalNot/Void never coerce.
fn unaryop_slow(
  state: State,
  kind: opcode.UnaryOpKind,
  operand: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  case kind {
    opcode.Neg -> rt2(state, rt_ops.t_neg, operand)
    opcode.Pos -> rt2(state, rt_ops.t_plus, operand)
    opcode.BitNot -> rt2(state, rt_ops.t_bitnot, operand)
    opcode.LogicalNot -> Ok(#(mk_bool(!ffi.truthy(operand)), state))
    opcode.Void -> Ok(#(mk_undefined(), state))
  }
}

/// Fused statement-position postfix update (IncLocal/DecLocal): exactly the
/// folded sequence GetLocal; UnaryOp(Pos); PushConst(1); BinOp(Add|Sub);
/// PutLocal; Pop, through the same coercions so every ToPrimitive call and
/// thrown error is identical.
fn fused_update_local(
  state: State,
  index: Int,
  increment: Bool,
) -> Result(State, StepExit) {
  let next_pc = state.pc + 1
  let v = tuple_array.get_unchecked(index, state.locals)
  case is_tdz(v) {
    True -> tdz_reference_error(state)
    False -> {
      use #(n, state) <- result.try(rt2(state, rt_ops.t_plus, v))
      let one = int_val(1)
      use #(r, state) <- result.map(case increment {
        True -> rt3(state, rt_ops.t_add, n, one)
        False -> rt3(state, rt_ops.t_sub, n, one)
      })
      let locals = tuple_array.set_unchecked(index, r, state.locals)
      State(..state, locals:, pc: next_pc)
    }
  }
}

/// Fused compare-and-branch (CmpLocal*Jump): the folded sequence
/// GetLocal(s)/PushConst; BinOp(kind); JumpIfFalse(target).
fn fused_cmp_jump(
  state: State,
  kind: binop.PureBinOp,
  left: JsVal,
  right: JsVal,
  target: Int,
) -> Result(State, StepExit) {
  let next_pc = state.pc + 1
  use #(r, state) <- result.map(pure_binop_slow(state, kind, left, right))
  case ffi.truthy(r) {
    True -> State(..state, pc: next_pc)
    False -> State(..state, pc: target)
  }
}

/// §7.3.30 PrivateGet with a minted key.
fn private_get(
  state: State,
  obj: JsVal,
  k: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  rt3(state, rt_class.t_private_get, obj, k)
}

/// §7.3.31 PrivateSet with a minted key; returns the written value.
fn private_set(
  state: State,
  obj: JsVal,
  k: JsVal,
  val: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  rt4(state, rt_class.t_private_set, obj, k, val)
}

fn private_in(agent: Agent, obj: JsVal, k: JsVal) -> Bool {
  rt_class.t_private_in(agent, obj, k)
}

fn private_define_field(
  state: State,
  h: Handle,
  k: JsVal,
  val: JsVal,
) -> Result(State, StepExit) {
  rt_unit4(state, rt_class.t_private_define, h, k, val)
}

fn private_define_method(
  state: State,
  h: Handle,
  k: JsVal,
  func: JsVal,
  kind: rt_types.MethodInstallKind,
) -> Result(State, StepExit) {
  rt_unit5(state, rt_class.t_define_private, h, k, func, kind)
}

/// §7.3.7 CreateDataPropertyOrThrow through the real [[DefineOwnProperty]]:
/// proxy traps fire; a false result (frozen / non-extensible receiver, trap
/// refusal, an exotic array's failing length write) throws TypeError.
fn create_data_property_or_throw(
  state: State,
  h: Handle,
  k: ObjectKey,
  val: JsVal,
) -> Result(State, StepExit) {
  use #(ok, state) <- result.try(rt7(
    state,
    rt_obj.t_define_own_data,
    h,
    k,
    val,
    True,
    True,
    True,
  ))
  case ok {
    True -> Ok(state)
    False ->
      state.throw_type_error(
        state,
        "Cannot define property " <> object_key_display(k),
      )
  }
}

fn object_key_display(k: ObjectKey) -> String {
  case k {
    StringKey(pk) -> rt_types.key_display_string(pk)
    SymbolKey(sym) -> rt_types.symbol_descriptive_string(sym)
  }
}

/// ToPropertyKey every excluded key of an object-rest pattern, in order.
fn to_property_keys(
  state: State,
  raw: List(JsVal),
  acc: List(ObjectKey),
) -> Result(#(List(ObjectKey), State), StepExit) {
  case raw {
    [] -> Ok(#(list.reverse(acc), state))
    [k, ..rest] -> {
      use #(pk, state) <- result.try(rt2(state, rt_val.t_to_property_key, k))
      to_property_keys(state, rest, [pk, ..acc])
    }
  }
}

/// §15.7.14 steps 5.f-g for `class extends parent`: IsConstructor BEFORE
/// Get(superclass, "prototype") (so an arrow/generator heritage throws
/// without touching its possibly trapped .prototype); protoParent must be an
/// object or null. `null` heritage is `extends null`.
fn class_proto_parent(
  state: State,
  parent: JsVal,
) -> Result(#(Option(Handle), State), StepExit) {
  case classify(parent) {
    KNull -> Ok(#(None, state))
    KHandle(_) ->
      case rt_call.is_constructor(state.agent, parent) {
        False ->
          state.throw_type_error(
            state,
            "Class extends value is not a constructor or null",
          )
        True -> {
          use #(pp, state) <- result.try(rt3(
            state,
            rt_obj.t_get_prop,
            parent,
            named("prototype"),
          ))
          case classify(pp) {
            KHandle(p) -> Ok(#(Some(p), state))
            KNull -> Ok(#(None, state))
            _ ->
              state.throw_type_error(
                state,
                "Class extends value does not have valid prototype property "
                  <> inspect(state, pp),
              )
          }
        }
      }
    _ ->
      state.throw_type_error(
        state,
        "Class extends value is not a constructor or null",
      )
  }
}

/// The handle in an object's OWN `prototype` data property, if any.
fn own_prototype_handle(agent: Agent, h: Handle) -> Option(Handle) {
  case rt_obj.t_ordinary_own_property(agent, h, named("prototype")) {
    Some(DataProperty(value:, ..)) -> handle_of(value)
    _ -> None
  }
}

/// The ordinary [[Prototype]] slot of a cell (no trap dispatch).
fn slot_prototype(agent: Agent, h: Handle) -> Option(Handle) {
  case rt_store.t_cell_get(agent, h) {
    SObject(proto:, ..) | SShapedObject(proto:, ..) -> proto
    _ -> None
  }
}

/// Overwrite a cell's [[Prototype]] slot directly. Class setup targets are
/// fresh closures / prototype objects: extensible and cycle-free, so the
/// direct write is spec-equivalent to OrdinarySetPrototypeOf.
fn set_slot_prototype(agent: Agent, h: Handle, proto: Option(Handle)) -> Agent {
  rt_store.t_cell_update(agent, h, fn(slot) {
    case slot {
      SObject(..) -> SObject(..slot, proto:)
      SShapedObject(..) -> SShapedObject(..slot, proto:)
      _ -> slot
    }
  })
}

/// Append to an Array cell (or bump its length past a hole) during array
/// literal construction. The array is a fresh literal: extensible, no
/// index overrides, writable length.
fn array_push(agent: Agent, h: Handle, value: Option(JsVal)) -> Agent {
  rt_store.t_cell_update(agent, h, fn(slot) {
    case slot {
      SObject(kind: rt_types.ArrayObj(length:), elements:, ..) ->
        SObject(
          ..slot,
          kind: rt_types.ArrayObj(length: length + 1),
          elements: case value {
            Some(v) -> rt_elements.set(elements, length, v)
            None -> elements
          },
        )
      _ -> slot
    }
  })
}

/// Append every value of `items` to an Array cell in one write (spread into
/// an array literal); same invariants as `array_push`.
fn array_append(agent: Agent, h: Handle, items: List(JsVal)) -> Agent {
  rt_store.t_cell_update(agent, h, fn(slot) {
    case slot {
      SObject(kind: rt_types.ArrayObj(length:), elements:, ..) ->
        SObject(
          ..slot,
          kind: rt_types.ArrayObj(length: length + list.length(items)),
          elements: rt_elements.write_list(elements, length, items),
        )
      _ -> slot
    }
  })
}

/// Rebuild the full element list of an ArrayFromWithHoles literal: `values`
/// are the non-hole items in order, `holes` the ascending hole indices.
fn fill_holes(
  values: List(JsVal),
  holes: List(Int),
  index: Int,
  count: Int,
  acc: List(JsVal),
) -> List(JsVal) {
  case index >= count {
    True -> list.reverse(acc)
    False ->
      case holes {
        [hole, ..rest_holes] if hole == index ->
          fill_holes(values, rest_holes, index + 1, count, [
            rt_types.mk_hole(),
            ..acc
          ])
        _ ->
          case values {
            [v, ..rest_values] ->
              fill_holes(rest_values, holes, index + 1, count, [v, ..acc])
            [] ->
              fill_holes([], holes, index + 1, count, [
                rt_types.mk_hole(),
                ..acc
              ])
          }
      }
  }
}

/// IteratorNext for a record `fast_iter_step` did not step itself, as
/// `#(done, value)`. A generator whose `next` is the intrinsic
/// %GeneratorPrototype%.next is resumed for one turn and answers the pair
/// itself: no `{value, done}` object is built per step only to be read
/// straight back. Anything else runs the protocol call.
fn iter_step(
  state: State,
  drive: Drive,
  rec: JsVal,
  fast: FastIter,
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  case fast {
    GenStep(data) -> gen_step(state, drive, data, mk_undefined())
    ArrayStep(..) | Protocol -> rt2(state, rt_lang.t_iter_next, rec)
  }
}

/// §27.5.3.3 GeneratorResume of the generator behind `data` with `sent`, as
/// `#(done, value)`. A body parked at a yield (or its InitialYield) in the
/// running realm is resumed right here, on this loop's stack: the frame is
/// unparked, run for one turn and parked again with one store write each
/// way. Everything else (`.throw`/`.return` parks, a delegate mid-flight,
/// another realm, a compiled body, the depth limit, running/completed
/// states) takes the shared driver, which does the same in general form.
fn gen_step(
  state: State,
  drive: Drive,
  data: Handle,
  sent: JsVal,
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  let agent = state.agent
  case rt_store.t_cell_get(agent, data) {
    rt_types.SGenerator(
      state: rt_types.GenSuspendedYield,
      resume: rt_types.ResumeFrame(frame:) as resume,
    )
      | rt_types.SGenerator(
        state: rt_types.GenSuspendedStart,
        resume: rt_types.ResumeFrame(frame:) as resume,
      )
      if frame.realm == agent.realm.id
      && agent.store.call_depth < limits.max_call_depth
    ->
      case frame.parked {
        ParkedOp ->
          resume_here(state, drive, data, resume, frame, [sent, ..frame.stack])
        ParkedStart ->
          resume_here(state, drive, data, resume, frame, frame.stack)
        _ -> rt3(state, rt_async.t_gen_step, data, sent)
      }
    _ -> rt3(state, rt_async.t_gen_step, data, sent)
  }
}

/// One turn of the parked body `frame` of generator `data`, delivered
/// `stack`. Marks the generator running and enters its depth and
/// `Error.stack` frame in one agent, runs the body under this loop's guard,
/// then trues the depth and frames back up and writes the generator's next
/// state: parked at the yield, or completed. The body's own uncaught throw
/// (and a fault, surfaced as one, as `entry` does) is this step's throw.
fn resume_here(
  state: State,
  drive: Drive,
  data: Handle,
  resume: rt_types.Resume,
  frame: SuspendedFrame,
  stack: List(JsVal),
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  let agent = state.agent
  let store = agent.store
  let depth = store.call_depth
  let frames = agent.frames
  let running =
    Agent(
      ..agent,
      store: JsStore(
        ..store,
        data: tree_array.set(
          data.id,
          rt_types.SGenerator(state: rt_types.GenExecuting, resume:),
          store.data,
        ),
        call_depth: depth + 1,
      ),
      frames: [call.frame_info_at(frame.template, frame.line), ..frames],
    )
  let body = park.unpark_with(running, frame, stack)
  let completed = rt_types.SGenerator(state: rt_types.GenCompleted, resume:)
  case ffi.guard_state2(resumed_turn, body, drive) {
    ffi.Ok(value: Ok(#(Suspended(state.Yield, v), post)), ..) -> {
      let parked = rt_types.ResumeFrame(park.park(post, ParkedOp))
      let gen =
        rt_types.SGenerator(state: rt_types.GenSuspendedYield, resume: parked)
      Ok(#(
        #(False, v),
        State(..state, agent: settle_gen(post.agent, data, depth, frames, gen)),
      ))
    }
    ffi.Ok(value: Ok(#(Completed(NormalCompletion(v)), post)), ..) ->
      Ok(#(
        #(True, v),
        State(
          ..state,
          agent: settle_gen(post.agent, data, depth, frames, completed),
        ),
      ))
    ffi.Ok(value: Ok(#(Completed(ThrowCompletion(e)), post)), ..) ->
      Error(Threw(
        e,
        State(
          ..state,
          agent: settle_gen(post.agent, data, depth, frames, completed),
        ),
      ))
    ffi.Ok(value: Ok(#(Suspended(state.Await, _), post)), ..) ->
      Error(VmFailed(
        SuspensionLeak(site: "gen_step", kind: state.Await),
        State(
          ..state,
          agent: settle_gen(post.agent, data, depth, frames, completed),
        ),
      ))
    ffi.Ok(value: Error(err), agent:) -> {
      let #(e, s) =
        state.new_error(
          State(..state, agent:),
          rt_types.TypeErr,
          "internal error: " <> state.vm_error_message(err),
        )
      Error(Threw(
        e,
        State(..s, agent: settle_gen(s.agent, data, depth, frames, completed)),
      ))
    }
    ffi.Threw(agent:, thrown:) ->
      Error(Threw(
        thrown,
        State(..state, agent: settle_gen(agent, data, depth, frames, completed)),
      ))
  }
}

/// True `agent` back up after a turn of generator `data`'s body: its depth
/// and `Error.stack` frame gone, the generator now `gen`.
fn settle_gen(
  agent: Agent,
  data: Handle,
  depth: Int,
  frames: List(rt_types.FrameInfo),
  gen: rt_types.JsSlot,
) -> Agent {
  let store = agent.store
  Agent(
    ..agent,
    store: JsStore(
      ..store,
      data: tree_array.set(data.id, gen, store.data),
      call_depth: depth,
    ),
    frames:,
  )
}

/// The guarded body of `resume_here`: run the unparked activation for one
/// turn and hand back the agent it ended in (the entry agent for a fault,
/// which carries none).
fn resumed_turn(
  body: State,
  drive: Drive,
) -> #(Result(#(Outcome, State), VmError), Agent) {
  case execute_inner(body, drive) {
    Ok(#(_, post)) as res -> #(res, post.agent)
    Error(_) as res -> #(res, body.agent)
  }
}

/// One `next(arg)` turn of a yield* delegate as `#(done, value)` (§27.5.3.8
/// step 7.a: `value` IS read when done). A native generator is resumed
/// directly, skipping the result object; any other iterator is called and
/// its result read.
fn delegate_step(
  state: State,
  drive: Drive,
  iterator: JsVal,
  next_fn: JsVal,
  arg: JsVal,
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  case native_generator(state.agent, iterator, next_fn) {
    Some(data) -> gen_step(state, drive, data, arg)
    None -> {
      use #(res, state) <- result.try(
        rt4(state, rt_call.t_call_checked, next_fn, iterator, [arg]),
      )
      rt2(state, iter_protocol.read_iter_result, res)
    }
  }
}

/// The `SGenerator` data cell behind `iterator` when it is a generator
/// object and `next_fn` the unmodified %GeneratorPrototype%.next, i.e. when
/// calling `next_fn` could do nothing but resume that generator.
fn native_generator(
  agent: Agent,
  iterator: JsVal,
  next_fn: JsVal,
) -> Option(Handle) {
  use next_h <- option.then(handle_of(next_fn))
  use iter_h <- option.then(handle_of(iterator))
  case rt_store.t_cell_get(agent, next_h), rt_store.t_cell_get(agent, iter_h) {
    SObject(
      kind: rt_types.KNative(
        tag: rt_types.GeneratorN(rt_types.GeneratorNext),
        ..,
      ),
      ..,
    ),
      SObject(kind: rt_types.GeneratorObj(data:), ..)
    -> Some(data)
    _, _ -> None
  }
}

/// What IteratorNext learns from one read of a record's [[Iterator]] and
/// [[NextMethod]] cells.
type FastIter {
  /// The step was taken here.
  ArrayStep(done: Bool, value: JsVal, agent: Agent)
  /// A native generator to resume for one turn.
  GenStep(data: Handle)
  /// Run the protocol call.
  Protocol
}

/// IteratorNext for the record of an unmodified Array values iteration
/// (`for (x of array)`, spread of an array): the record's [[NextMethod]] IS
/// %ArrayIteratorPrototype%.next and its [[Iterator]] an ArrayIterator over
/// a plain Array cell, so §23.1.5.2.1 is stepped here without allocating the
/// `{value, done}` object the native would build and this opcode would
/// immediately take apart. Only the shapes whose element read observes
/// nothing are handled (a present own element, no index override); a hole,
/// an exhausted-but-not-yet-marked source of another kind, or anything else
/// answers `Protocol` and the generic protocol call runs instead. The same
/// cell reads spot a native generator (see `native_generator`).
fn fast_iter_step(
  agent: Agent,
  record: Option(rt_types.IteratorRecord),
) -> FastIter {
  case record {
    None -> Protocol
    Some(record) ->
      case handle_of(record.next_method), handle_of(record.iterator) {
        Some(next_h), Some(iter_h) ->
          case
            rt_store.t_cell_get(agent, next_h),
            rt_store.t_cell_get(agent, iter_h)
          {
            SObject(
              kind: rt_types.KNative(
                tag: rt_types.IteratorN(rt_types.ArrayIteratorNext),
                ..,
              ),
              ..,
            ),
              SObject(
                kind: rt_types.ArrayIterator(
                  target:,
                  index:,
                  kind: rt_types.ArrayIterValues as kind,
                ),
                ..,
              ) as iter_slot
            ->
              case index < 0 {
                // Already exhausted: done, nothing to write.
                True -> ArrayStep(True, mk_undefined(), agent)
                False ->
                  case rt_store.t_cell_get(agent, target) {
                    SObject(
                      kind: rt_types.ArrayObj(length:),
                      elements:,
                      props:,
                      ..,
                    ) ->
                      case index >= length {
                        True ->
                          ArrayStep(
                            True,
                            mk_undefined(),
                            rt_store.t_cell_set(
                              agent,
                              iter_h,
                              SObject(
                                ..iter_slot,
                                kind: rt_types.ArrayIterator(
                                  target:,
                                  index: -1,
                                  kind:,
                                ),
                              ),
                            ),
                          )
                        False ->
                          case
                            dict.has_key(props, Index(index)),
                            rt_elements.get_option(elements, index)
                          {
                            False, Some(v) ->
                              ArrayStep(
                                False,
                                v,
                                rt_store.t_cell_set(
                                  agent,
                                  iter_h,
                                  SObject(
                                    ..iter_slot,
                                    kind: rt_types.ArrayIterator(
                                      target:,
                                      index: index + 1,
                                      kind:,
                                    ),
                                  ),
                                ),
                              )
                            _, _ -> Protocol
                          }
                      }
                    _ -> Protocol
                  }
              }
            SObject(
              kind: rt_types.KNative(
                tag: rt_types.GeneratorN(rt_types.GeneratorNext),
                ..,
              ),
              ..,
            ),
              SObject(kind: rt_types.GeneratorObj(data:), ..)
            -> GenStep(data)
            _, _ -> Protocol
          }
        _, _ -> Protocol
      }
  }
}

/// The ForInIterator cell behind a for-in slot and its remaining keys.
fn for_in_remaining(
  agent: Agent,
  iter: JsVal,
) -> Option(#(Handle, List(String))) {
  use h <- option.then(handle_of(iter))
  case rt_store.t_cell_get(agent, h) {
    SObject(kind: ForInIterator(remaining:), ..) -> Some(#(h, remaining))
    _ -> None
  }
}

/// Re-materialise an already-converted key as a value whose re-conversion
/// through ToPropertyKey is side-effect-free and yields the same key.
/// GetElem2 / GetSuperValue2 leave this on the stack so the later PutElem /
/// PutSuperValue does not re-run a user-observable ToPropertyKey (§13.15.2:
/// ToPropertyKey once). Index keys round-trip as numbers.
fn prop_key_value(pk: ObjectKey) -> JsVal {
  case pk {
    SymbolKey(sym) -> rt_types.mk_symbol(sym)
    StringKey(Index(n)) -> int_val(n)
    StringKey(other) -> mk_string(rt_types.key_to_text(other))
  }
}

/// Shared body of GetSuperValue / GetSuperValue2: [key, base, this, ..] →
/// ToPropertyKey, then OrdinaryGet on base with receiver = this. With
/// `keep_base` the coerced key + base + this stay under the value.
fn get_super_value(
  state: State,
  keep_base: Bool,
  op: String,
) -> Result(State, StepExit) {
  case state.stack {
    [k, base, this_val, ..rest] ->
      case handle_of(base) {
        Some(base_h) -> {
          use #(pk, state) <- result.try(rt2(state, rt_val.t_to_property_key, k))
          use #(val, state) <- result.map(rt4(
            state,
            rt_obj.t_get_prop_with_receiver,
            base_h,
            pk,
            this_val,
          ))
          let stack = case keep_base {
            True -> [val, prop_key_value(pk), base, this_val, ..rest]
            False -> [val, ..rest]
          }
          State(..state, stack:, pc: state.pc + 1)
        }
        // §12.3.5.3 step 5 RequireObjectCoercible: base is null when the
        // home object's prototype is null (`class C extends null`).
        None ->
          state.throw_type_error(
            state,
            "Cannot read super property when prototype is null",
          )
      }
    _ -> underflow(state, op)
  }
}

/// §7.4.3 GetIterator(obj, async) up to the iterator OBJECT: the
/// `@@asyncIterator` method's result (which must be an Object), or for a
/// sync-only iterable the CreateAsyncFromSyncIterator wrapper (whose sync
/// record does cache `next`, §7.4.3 step 1.b.ii). The async iterator's own
/// `next` is left unread for the consumer.
fn async_iterator_object(agent: Agent, iterable: JsVal) -> #(JsVal, Agent) {
  let #(method, agent) =
    rt_obj.t_get_prop(
      agent,
      iterable,
      SymbolKey(rt_types.symbol_async_iterator),
    )
  case classify(method) {
    // Step 1.b: GetMethod(obj, @@iterator), GetIteratorFromMethod, wrap.
    KUndef | KNull -> {
      let #(sync_method, agent) =
        rt_obj.t_get_prop(agent, iterable, SymbolKey(rt_types.symbol_iterator))
      case rt_call.is_callable(agent, sync_method) {
        False -> {
          let #(ty, agent) = rt_val.t_type_of(agent, iterable)
          let #(err, agent) =
            agent.store.ops.new_error(
              agent,
              rt_types.TypeErr,
              ty <> " is not async iterable",
            )
          rt_store.t_throw(agent, err)
        }
        True -> {
          let #(sync, agent) =
            iter_protocol.get_iterator_from_method(agent, iterable, sync_method)
          let #(record, agent) =
            iter_protocol.create_async_from_sync(agent, sync)
          #(record.iterator, agent)
        }
      }
    }
    _ -> {
      let #(iterator, agent) =
        rt_call.t_call_checked(agent, method, iterable, [])
      case is_object(iterator) {
        True -> #(iterator, agent)
        False -> {
          let #(err, agent) =
            agent.store.ops.new_error(
              agent,
              rt_types.TypeErr,
              "Result of the Symbol.asyncIterator method is not an object",
            )
          rt_store.t_throw(agent, err)
        }
      }
    }
  }
}

/// The iterator object and its `next` for a yield* delegation slot: the
/// Iterator Record on the stack, or (defensively) a bare iterator whose
/// `next` is read now.
fn delegate_target(
  state: State,
  slot: JsVal,
) -> Result(#(JsVal, JsVal, State), StepExit) {
  case rt_lang.record_parts(state.agent, slot) {
    Some(record) -> Ok(#(record.iterator, record.next_method, state))
    None -> {
      use #(next_fn, state) <- result.map(rt3(
        state,
        rt_obj.t_get_prop,
        slot,
        named("next"),
      ))
      #(slot, next_fn, state)
    }
  }
}

/// Pop n items. Returns #(popped_in_order, remaining_stack).
fn pop_n(stack: List(JsVal), n: Int) -> Option(#(List(JsVal), List(JsVal))) {
  case n, stack {
    0, _ -> Some(#([], stack))
    1, [a, ..rest] -> Some(#([a], rest))
    2, [b, a, ..rest] -> Some(#([a, b], rest))
    3, [c, b, a, ..rest] -> Some(#([a, b, c], rest))
    _, _ -> pop_n_loop(stack, n, [])
  }
}

fn pop_n_loop(
  stack: List(JsVal),
  remaining: Int,
  acc: List(JsVal),
) -> Option(#(List(JsVal), List(JsVal))) {
  case remaining {
    0 -> Some(#(acc, stack))
    _ ->
      case stack {
        [top, ..rest] -> pop_n_loop(rest, remaining - 1, [top, ..acc])
        [] -> None
      }
  }
}

// ============================================================================
// Nested activations
// ============================================================================

/// Run a freshly prepared root activation (eval code) until its own call
/// stack empties: `Ok(value)` / `Error(thrown)` and the agent it finished
/// in, with its `Error.stack` frame pushed for the duration. This is the
/// `Run` the eval machinery is handed from inside the loop; the engine's
/// entry points supply their own, backstopped, equivalent.
fn run_activation(
  activation: State,
  drive: Drive,
) -> #(Result(JsVal, JsVal), Agent) {
  let agent = call.push_frame_info(activation.agent, activation.func)
  let #(res, s) = case execute_inner(State(..activation, agent:), drive) {
    Ok(#(Completed(NormalCompletion(v)), s)) -> #(Ok(v), s)
    Ok(#(Completed(ThrowCompletion(e)), s)) -> #(Error(e), s)
    Ok(#(Suspended(kind, _), s)) ->
      fault(s, SuspensionLeak(site: "eval", kind:))
    Error(err) -> fault(State(..activation, agent:), err)
  }
  #(res, call.pop_frame_info(s.agent))
}

/// An engine fault surfacing where only a JS completion fits: a thrown
/// TypeError naming it.
fn fault(s: State, err: VmError) -> #(Result(JsVal, JsVal), State) {
  let #(e, s) =
    state.new_error(
      s,
      rt_types.TypeErr,
      "internal error: " <> state.vm_error_message(err),
    )
  #(Error(e), s)
}
