import arc/bytecode/binop
import arc/bytecode/key.{
  type PropertyKey, Index, Named, key_display_string, key_to_text,
}
import arc/bytecode/lexical
import arc/bytecode/opcode.{
  type Op, ApplyArguments, ArrayFrom, ArrayFromWithHoles, ArrayPush,
  ArrayPushHole, ArraySpread, AsyncYieldStarNext, AsyncYieldStarResume, Await,
  BinOp, BinOpConst, BinOpConstPut, BinOpLocal, BinOpLocalConst, BinOpLocalField,
  BinOpLocalLocal, BinOpLocalLocalPut, BinOpLocalPut, BinOpPut, BoxLocal, Call,
  CallApply, CallConstructor, CallConstructorApply, CallEval, CallMethod,
  CallMethodApply, CallNew, CmpConstJump, CmpJump, CmpLocalConstJump,
  CmpLocalLocalJump, CreateArguments, CreateRestArray, DecLocal, DeclareEvalVar,
  DeclareGlobalFn, DeclareGlobalLex, DeclareGlobalVar, DefineAccessor,
  DefineAccessorComputed, DefineField, DefineFieldComputed, DefineMethod,
  DefineMethodComputed, DefinePrivateAccessor, DefinePrivateField,
  DefinePrivateMethod, DeleteElem, DeleteField, DeleteGlobalVar, Dup, ForInNext,
  ForInStart, GetAsyncIterator, GetBoxed, GetElem, GetElem2, GetElemLocals,
  GetElemPostInc, GetEvalVar, GetField, GetField2, GetFieldCall, GetFieldCall1,
  GetGlobal, GetIterator, GetLocal, GetLocalField, GetLocalField2,
  GetLocalFieldCall, GetPrivateFieldDyn, GetPrivateFieldDyn2, GetPrototypeOf,
  GetSuperValue, GetSuperValue2, IncLocal, IncLocalCmpConstJump,
  IncLocalCmpLocalJump, IncLocalJump, InitGlobalLex, InitialYield,
  IteratorCheckObject, IteratorClose, IteratorCloseThrow, IteratorNext,
  IteratorRecord, IteratorRest, Jump, JumpIfFalse, JumpIfLocal, JumpIfNotNullish,
  JumpIfNullish, JumpIfTrue, MakeClosure, MakeMethod, NewObject, NewObjectWith,
  NewPrivateName, NewRegExp, ObjectRestCopy, ObjectSpread, Pc, Pop, PostDecLocal,
  PostIncLocal, PrivateInDyn, PushConst, PushTry, PutBoxed, PutBoxedCheckInit,
  PutElem, PutElemPop, PutEvalVar, PutField, PutFieldPop, PutGlobal, PutLocal,
  PutLocalCheckInit, PutLocalConstField, PutLocalLocalField, PutPrivateFieldDyn,
  PutSuperValue, Return, Rot3, SetProto, SetupDerivedClass, Swap, TypeOf,
  TypeofEvalVar, TypeofGlobal, UnaryOp, Unrot4, Yield, YieldStar,
}
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/call.{type Drive}
import arc/interp/dynamic_import
import arc/interp/eval
import arc/interp/ffi
import arc/interp/park
import arc/interp/safepoint
import arc/interp/state.{
  type State, type StepExit, type VmError, AsyncDelegateResume, Awaited,
  DelegateYield, InitialSuspend, InternalError, PlainYield, Returned, SavedFrame,
  SavedRegFrame, StackUnderflow, State, SuspensionLeak, Threw, VmFailed, Yielded,
}
import arc/rt/arena
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
  FunctionCall, FunctionN, HintString, JsStore, KBytecode, KCompiled, KHandle,
  KNative, KNull, KNum, KStr, KSym, KUndef, NoElements, Realm, ReflectApply,
  ReflectN, SBox, SObject, SShapedObject, StringKey, SymbolKey, classify,
  mk_bool, mk_number, mk_object, mk_string, mk_tdz, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Outcome {
  Completed(Completion)
  Suspended(state.SuspendKind, JsVal)
}

@external(erlang, "arc_rt_store_ffi", "is_handle")
fn is_handle(v: JsVal) -> Bool

fn as_handle(v: JsVal) -> Handle {
  ffi.handle([v])
}

@external(erlang, "arc_rt_ops_ffi", "binop")
fn k_binop(kind: opcode.Classified, a: JsVal, b: JsVal) -> JsVal

@external(erlang, "arc_rt_ops_ffi", "t_bitnot_fast")
fn k_bitnot(a: JsVal) -> JsVal

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

fn make_method(agent: Agent, func: JsVal, target: Handle) -> Agent {
  case classify(func) {
    KHandle(fn_h) -> set_home_object(agent, fn_h, target)
    _ -> agent
  }
}

fn using_disposer(
  agent: Agent,
  val: JsVal,
  is_async: Bool,
  unit: Int,
) -> #(JsVal, Agent) {
  case classify(val) {
    KUndef | KNull -> #(mk_undefined(), agent)
    KHandle(_) -> {
      let #(method, agent) =
        disposable_stack.get_dispose_method(agent, val, is_async:)
      case method {
        disposable_stack.DirectDispose(m) -> direct_disposer(agent, m, val)
        disposable_stack.SyncFallbackDispose(m) ->
          sync_fallback_disposer(agent, m, val, unit)
      }
    }
    _ ->
      rt_val.t_throw_type_error(
        agent,
        "using declaration initializer is not an object, null, or undefined",
      )
  }
}

// built directly so the method's length/name are never read
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

fn sync_fallback_template() -> FuncTemplate {
  bytecode.FuncTemplate(
    name: None,
    arity: 0,
    length: 0,
    local_count: 2,
    bytecode: tuple_array.from_list([
      GetLocal(1),
      GetLocal(0),
      CallMethod(0),
      Pop,
      PushConst(0),
      Return,
    ]),
    constants: tuple_array.from_list([mk_undefined()]),
    keys: tuple_array.from_list([]),
    lines: tuple_array.from_list([0, 0, 0, 0, 0, 0]),
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
    regs: bytecode.NoRegs,
  )
}

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

@external(erlang, "arc_interp_ffi", "is_tdz")
fn is_tdz(v: JsVal) -> Bool

// every stream ends in a sentinel return, so fetch is unchecked
pub fn execute_inner(
  state: State,
  drive: Drive,
) -> Result(#(Outcome, State), VmError) {
  let func = state.func
  let locals = state.locals
  let code = func.bytecode
  let constants = func.constants
  let keys = func.keys
  let _ = tuple_array.size(locals)
  let _ = tuple_array.size(code)
  let _ = tuple_array.size(constants)
  let _ = tuple_array.size(keys)
  case func.regs {
    bytecode.NoRegs -> {
      let u = ffi.val([ffi.Undefined])
      fast_loop(
        state,
        drive,
        state.pc,
        state.stack,
        locals,
        state.agent,
        code,
        constants,
        keys,
        u,
        u,
      )
    }
    bytecode.Regs(a, b) ->
      fast_loop(
        state,
        drive,
        state.pc,
        state.stack,
        locals,
        state.agent,
        code,
        constants,
        keys,
        ld(locals, a),
        ld(locals, b),
      )
  }
}

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

// tuple_size tells the erlang compiler these are tuples so element inlines
fn enter(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  keys: TupleArray(PropertyKey),
) -> Result(#(Outcome, State), VmError) {
  let _ = tuple_array.size(locals)
  let _ = tuple_array.size(code)
  let _ = tuple_array.size(constants)
  let _ = tuple_array.size(keys)
  case state.func.regs {
    bytecode.NoRegs -> {
      let u = ffi.val([ffi.Undefined])
      fast_loop(
        state,
        drive,
        pc,
        stack,
        locals,
        agent,
        code,
        constants,
        keys,
        u,
        u,
      )
    }
    bytecode.Regs(a, b) ->
      fast_loop(
        state,
        drive,
        pc,
        stack,
        locals,
        agent,
        code,
        constants,
        keys,
        ld(locals, a),
        ld(locals, b),
      )
  }
}

fn key_at(state: State, slot: Int) -> PropertyKey {
  tuple_array.element(slot + 1, state.func.keys)
}

fn ld(locals: TupleArray(JsVal), slot: Int) -> JsVal {
  case slot < 0 {
    True -> ffi.val([ffi.Undefined])
    False -> tuple_array.element(slot + 1, locals)
  }
}

// negative slots name a register
fn wreg(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  keys: TupleArray(PropertyKey),
  r0: JsVal,
  r1: JsVal,
  slot: Int,
  v: JsVal,
) -> Result(#(Outcome, State), VmError) {
  case slot {
    -1 ->
      fast_loop(
        state,
        drive,
        pc,
        stack,
        locals,
        agent,
        code,
        constants,
        keys,
        v,
        r1,
      )
    _ ->
      fast_loop(
        state,
        drive,
        pc,
        stack,
        locals,
        agent,
        code,
        constants,
        keys,
        r0,
        v,
      )
  }
}

// registers written back so the tuple can leave the loop
fn fl(
  state: State,
  locals: TupleArray(JsVal),
  r0: JsVal,
  r1: JsVal,
) -> TupleArray(JsVal) {
  case state.func.regs {
    bytecode.NoRegs -> locals
    bytecode.Regs(a, b) -> ffi.flush_regs(locals, a, b, r0, r1)
  }
}

fn slow(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  r0: JsVal,
  r1: JsVal,
) -> Result(#(Outcome, State), VmError) {
  dispatch_slow(state, drive, pc, stack, fl(state, locals, r0, r1), agent)
}

// fast paths do nothing observable before a miss, so step re-runs the op.
// state.pc/stack/locals/agent are stale here, the loop args win
fn fast_loop(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  keys: TupleArray(PropertyKey),
  r0: JsVal,
  r1: JsVal,
) -> Result(#(Outcome, State), VmError) {
  case tuple_array.element(pc + 1, code) {
    PushConst(index) -> {
      let v = tuple_array.element(index + 1, constants)
      fast_loop(
        state,
        drive,
        pc + 1,
        [v, ..stack],
        locals,
        agent,
        code,
        constants,
        keys,
        r0,
        r1,
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
            keys,
            r0,
            r1,
          )
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
            keys,
            r0,
            r1,
          )
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
            keys,
            r0,
            r1,
          )
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetLocal(index) -> {
      let v = case index < 0 {
        True ->
          case index {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(index + 1, locals)
      }
      case ffi.is(v, ffi.JsTdz) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
            keys,
            r0,
            r1,
          )
      }
    }

    PutLocal(index) ->
      case stack {
        [v, ..rest] ->
          case index < 0 {
            True ->
              wreg(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                index,
                v,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                rest,
                tuple_array.set_element(index + 1, locals, v),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetBoxed(index) -> {
      let v =
        ffi.box_get(agent, case index < 0 {
          True ->
            case index {
              -1 -> r0
              _ -> r1
            }
          False -> tuple_array.element(index + 1, locals)
        })
      case ffi.is(v, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
            keys,
            r0,
            r1,
          )
      }
    }

    PutBoxed(index) ->
      case stack {
        [v, ..rest] -> {
          let slot = case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          }
          case is_handle(slot) {
            False -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
        keys,
        r0,
        r1,
      )

    JumpIfFalse(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case ffi.is_bool(top, True) {
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
                keys,
                r0,
                r1,
              )
            False ->
              case ffi.is_bool(top, False) {
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
                    keys,
                    r0,
                    r1,
                  )
                False ->
                  case ffi.truthy(top) {
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
                        keys,
                        r0,
                        r1,
                      )
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
                        keys,
                        r0,
                        r1,
                      )
                  }
              }
          }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    JumpIfTrue(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case ffi.is_bool(top, True) {
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
                keys,
                r0,
                r1,
              )
            False ->
              case ffi.is_bool(top, False) {
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
                    keys,
                    r0,
                    r1,
                  )
                False ->
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
                        keys,
                        r0,
                        r1,
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
                        keys,
                        r0,
                        r1,
                      )
                  }
              }
          }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    JumpIfNullish(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case ffi.is(top, ffi.Undefined) || ffi.is(top, ffi.Null) {
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
                keys,
                r0,
                r1,
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
                keys,
                r0,
                r1,
              )
          }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    JumpIfNotNullish(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case ffi.is(top, ffi.Undefined) || ffi.is(top, ffi.Null) {
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
                keys,
                r0,
                r1,
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
                keys,
                r0,
                r1,
              )
          }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOp(kind) ->
      case stack {
        [right, left, ..rest] -> {
          let r = case kind {
            opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> k_binop(kind, left, right)
          }
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpConst(kind, const_index) ->
      case stack {
        [left, ..rest] -> {
          let right = tuple_array.element(const_index + 1, constants)
          let r = case kind {
            opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> k_binop(kind, left, right)
          }
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpLocal(kind, index) ->
      case stack {
        [left, ..rest] -> {
          let right = case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          }
          let r = case kind {
            opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> k_binop(kind, left, right)
          }
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpLocalLocal(kind, left_idx, right_idx) -> {
      let left = case left_idx < 0 {
        True ->
          case left_idx {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(left_idx + 1, locals)
      }
      let right = case right_idx < 0 {
        True ->
          case right_idx {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(right_idx + 1, locals)
      }
      let r = case kind {
        opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
        _ -> k_binop(kind, left, right)
      }
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          fast_loop(
            state,
            drive,
            pc + 1,
            [r, ..stack],
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
          )
      }
    }

    BinOpLocalConst(kind, left_idx, const_index) -> {
      let left = case left_idx < 0 {
        True ->
          case left_idx {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(left_idx + 1, locals)
      }
      let right = tuple_array.element(const_index + 1, constants)
      let r = case kind {
        opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
        _ -> k_binop(kind, left, right)
      }
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          fast_loop(
            state,
            drive,
            pc + 1,
            [r, ..stack],
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
          )
      }
    }

    BinOpPut(kind, dst) ->
      case stack {
        [right, left, ..rest] -> {
          let r = case kind {
            opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> k_binop(kind, left, right)
          }
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case dst < 0 {
                True ->
                  wreg(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    keys,
                    r0,
                    r1,
                    dst,
                    r,
                  )
                False ->
                  fast_loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    tuple_array.set_element(dst + 1, locals, r),
                    agent,
                    code,
                    constants,
                    keys,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpConstPut(kind, const_index, dst) ->
      case stack {
        [left, ..rest] -> {
          let right = tuple_array.element(const_index + 1, constants)
          let r = case kind {
            opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> k_binop(kind, left, right)
          }
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case dst < 0 {
                True ->
                  wreg(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    keys,
                    r0,
                    r1,
                    dst,
                    r,
                  )
                False ->
                  fast_loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    tuple_array.set_element(dst + 1, locals, r),
                    agent,
                    code,
                    constants,
                    keys,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpLocalPut(kind, index, dst) ->
      case stack {
        [left, ..rest] -> {
          let right = case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          }
          let r = case kind {
            opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> k_binop(kind, left, right)
          }
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case dst < 0 {
                True ->
                  wreg(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    keys,
                    r0,
                    r1,
                    dst,
                    r,
                  )
                False ->
                  fast_loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    tuple_array.set_element(dst + 1, locals, r),
                    agent,
                    code,
                    constants,
                    keys,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpLocalField(kind, index, k) ->
      case tuple_array.element(k + 1, keys), stack {
        Named(_) as k, [left, ..rest] -> {
          let right =
            ffi.get_field(
              agent,
              case index < 0 {
                True ->
                  case index {
                    -1 -> r0
                    _ -> r1
                  }
                False -> tuple_array.element(index + 1, locals)
              },
              k,
            )
          case ffi.is(right, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False -> {
              let r = case kind {
                opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
                _ -> k_binop(kind, left, right)
              }
              case ffi.is(r, ffi.Miss) {
                True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                    keys,
                    r0,
                    r1,
                  )
              }
            }
          }
        }
        _, _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpLocalLocalPut(kind, left_idx, right_idx, dst) -> {
      let left = case left_idx < 0 {
        True ->
          case left_idx {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(left_idx + 1, locals)
      }
      let right = case right_idx < 0 {
        True ->
          case right_idx {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(right_idx + 1, locals)
      }
      let r = case kind {
        opcode.InstanceOfOp -> instance_of_kernel(agent, left, right)
        _ -> k_binop(kind, left, right)
      }
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case dst < 0 {
            True ->
              wreg(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                dst,
                r,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                tuple_array.set_element(dst + 1, locals, r),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    UnaryOp(kind) ->
      case stack {
        [operand, ..rest] -> {
          let r = case kind {
            opcode.Neg -> ffi.neg(operand)
            opcode.Pos -> ffi.plus(operand)
            opcode.LogicalNot -> ffi.lnot(operand)
            opcode.Void -> mk_undefined()
            opcode.BitNot -> k_bitnot(operand)
          }
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    IncLocal(index) -> {
      let r =
        ffi.step(
          case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          },
          1,
        )
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              wreg(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                index,
                r,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    DecLocal(index) -> {
      let r =
        ffi.step(
          case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          },
          -1,
        )
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              wreg(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                index,
                r,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    JumpIfLocal(index, Pc(target), when) -> {
      let v = case index < 0 {
        True ->
          case index {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(index + 1, locals)
      }
      case ffi.is(v, ffi.JsTdz) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case ffi.truthy(v) == when {
            True ->
              fast_loop(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    IncLocalJump(index, Pc(target)) -> {
      let r =
        ffi.step(
          case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          },
          1,
        )
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              wreg(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                index,
                r,
              )
            False ->
              fast_loop(
                state,
                drive,
                target,
                stack,
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    IncLocalCmpConstJump(index, by, const_index, kind, Pc(target), when) -> {
      let n =
        ffi.step(
          case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          },
          by,
        )
      let r = case ffi.is(n, ffi.Miss) {
        True -> n
        False ->
          pure_binop_kernel(
            kind,
            n,
            tuple_array.element(const_index + 1, constants),
          )
      }
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False -> {
          let next = case ffi.is_bool(r, when) {
            True -> target
            False -> pc + 1
          }
          case index < 0 {
            True ->
              wreg(
                state,
                drive,
                next,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                index,
                n,
              )
            False ->
              fast_loop(
                state,
                drive,
                next,
                stack,
                tuple_array.set_element(index + 1, locals, n),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
      }
    }

    IncLocalCmpLocalJump(index, by, right_idx, kind, Pc(target), when) -> {
      let n =
        ffi.step(
          case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          },
          by,
        )
      let r = case ffi.is(n, ffi.Miss) {
        True -> n
        False ->
          pure_binop_kernel(kind, n, case right_idx < 0 {
            True ->
              case right_idx {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(right_idx + 1, locals)
          })
      }
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False -> {
          let next = case ffi.is_bool(r, when) {
            True -> target
            False -> pc + 1
          }
          case index < 0 {
            True ->
              wreg(
                state,
                drive,
                next,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                index,
                n,
              )
            False ->
              fast_loop(
                state,
                drive,
                next,
                stack,
                tuple_array.set_element(index + 1, locals, n),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
      }
    }

    PostIncLocal(index) -> {
      let old = case index < 0 {
        True ->
          case index {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(index + 1, locals)
      }
      let r = ffi.step(old, 1)
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              wreg(
                state,
                drive,
                pc + 1,
                [old, ..stack],
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                index,
                r,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [old, ..stack],
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    PostDecLocal(index) -> {
      let old = case index < 0 {
        True ->
          case index {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(index + 1, locals)
      }
      let r = ffi.step(old, -1)
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              wreg(
                state,
                drive,
                pc + 1,
                [old, ..stack],
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                index,
                r,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [old, ..stack],
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    CmpLocalLocalJump(left_idx, right_idx, kind, Pc(target), when) -> {
      let r =
        pure_binop_kernel(
          kind,
          case left_idx < 0 {
            True ->
              case left_idx {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(left_idx + 1, locals)
          },
          case right_idx < 0 {
            True ->
              case right_idx {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(right_idx + 1, locals)
          },
        )
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case ffi.is_bool(r, when) {
            True ->
              fast_loop(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    CmpLocalConstJump(left_idx, const_index, kind, Pc(target), when) -> {
      let r =
        pure_binop_kernel(
          kind,
          case left_idx < 0 {
            True ->
              case left_idx {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(left_idx + 1, locals)
          },
          tuple_array.element(const_index + 1, constants),
        )
      case ffi.is(r, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case ffi.is_bool(r, when) {
            True ->
              fast_loop(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    CmpJump(kind, Pc(target), when) ->
      case stack {
        [right, left, ..rest] -> {
          let r = pure_binop_kernel(kind, left, right)
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case ffi.is_bool(r, when) {
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
                    keys,
                    r0,
                    r1,
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
                    keys,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    CmpConstJump(const_index, kind, Pc(target), when) ->
      case stack {
        [left, ..rest] -> {
          let r =
            pure_binop_kernel(
              kind,
              left,
              tuple_array.element(const_index + 1, constants),
            )
          case ffi.is(r, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case ffi.is_bool(r, when) {
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
                    keys,
                    r0,
                    r1,
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
                    keys,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetElem ->
      case stack {
        [k, recv, ..rest] -> {
          let v = ffi.get_elem(agent.store, recv, k)
          case ffi.is(v, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetElem2 ->
      case stack {
        [k, recv, ..] -> {
          let v = ffi.get_elem2(agent.store, recv, k)
          case ffi.is(v, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    PutElem ->
      case stack {
        [val, k, recv, ..rest] -> {
          let store = ffi.put_elem(agent.store, recv, k, val)
          case ffi.is(store, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetElemLocals(obj, key_idx) -> {
      let v =
        ffi.get_elem(
          agent.store,
          case obj < 0 {
            True ->
              case obj {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(obj + 1, locals)
          },
          case key_idx < 0 {
            True ->
              case key_idx {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(key_idx + 1, locals)
          },
        )
      case ffi.is(v, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
            keys,
            r0,
            r1,
          )
      }
    }

    GetElemPostInc(obj, key_idx) -> {
      let old = case key_idx < 0 {
        True ->
          case key_idx {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(key_idx + 1, locals)
      }
      let r = ffi.step(old, 1)
      let v = case ffi.is(r, ffi.Miss) {
        True -> r
        False ->
          ffi.get_elem(
            agent.store,
            case obj < 0 {
              True ->
                case obj {
                  -1 -> r0
                  _ -> r1
                }
              False -> tuple_array.element(obj + 1, locals)
            },
            old,
          )
      }
      case ffi.is(v, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case key_idx < 0 {
            True ->
              wreg(
                state,
                drive,
                pc + 1,
                [v, ..stack],
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                key_idx,
                r,
              )
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [v, ..stack],
                tuple_array.set_element(key_idx + 1, locals, r),
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
      }
    }

    PutElemPop ->
      case stack {
        [val, k, recv, ..rest] -> {
          let store = ffi.put_elem(agent.store, recv, k, val)
          case ffi.is(store, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                Agent(..agent, store:),
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetField(k) ->
      case tuple_array.element(k + 1, keys), stack {
        Named(_) as k, [recv, ..rest] -> {
          let v = ffi.get_field(agent, recv, k)
          case ffi.is(v, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _, _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetField2(k) ->
      case tuple_array.element(k + 1, keys), stack {
        Named(_) as k, [recv, ..rest] -> {
          let v = ffi.get_field(agent, recv, k)
          case ffi.is(v, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _, _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    PutField(k) ->
      case tuple_array.element(k + 1, keys), stack {
        Named(_) as k, [val, recv, ..rest] -> {
          let store = ffi.put_field(agent.store, recv, k, val, True)
          case ffi.is(store, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _, _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    PutFieldPop(k) ->
      case tuple_array.element(k + 1, keys), stack {
        Named(_) as k, [val, recv, ..rest] -> {
          let store = ffi.put_field(agent.store, recv, k, val, True)
          case ffi.is(store, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                Agent(..agent, store:),
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
        _, _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    PutLocalLocalField(obj, value, k) -> {
      let val = case value < 0 {
        True ->
          case value {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(value + 1, locals)
      }
      case ffi.is(val, ffi.JsTdz) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False -> {
          let store =
            ffi.put_field(
              agent.store,
              case obj < 0 {
                True ->
                  case obj {
                    -1 -> r0
                    _ -> r1
                  }
                False -> tuple_array.element(obj + 1, locals)
              },
              tuple_array.element(k + 1, keys),
              val,
              True,
            )
          case ffi.is(store, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                Agent(..agent, store:),
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
      }
    }

    PutLocalConstField(obj, const_index, k) -> {
      let store =
        ffi.put_field(
          agent.store,
          case obj < 0 {
            True ->
              case obj {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(obj + 1, locals)
          },
          tuple_array.element(k + 1, keys),
          tuple_array.element(const_index + 1, constants),
          True,
        )
      case ffi.is(store, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          fast_loop(
            state,
            drive,
            pc + 1,
            stack,
            locals,
            Agent(..agent, store:),
            code,
            constants,
            keys,
            r0,
            r1,
          )
      }
    }

    GetLocalField(index, k) ->
      case tuple_array.element(k + 1, keys) {
        Named(_) as k -> {
          let v =
            ffi.get_field(
              agent,
              case index < 0 {
                True ->
                  case index {
                    -1 -> r0
                    _ -> r1
                  }
                False -> tuple_array.element(index + 1, locals)
              },
              k,
            )
          case ffi.is(v, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetLocalField2(index, k) ->
      case tuple_array.element(k + 1, keys) {
        Named(_) as k -> {
          let recv = case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          }
          let v = ffi.get_field(agent, recv, k)
          case ffi.is(v, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [v, recv, ..stack],
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetGlobal(name) -> {
      let v = ffi.get_global(agent, agent.realm.lexical_globals, name)
      case ffi.is(v, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
            keys,
            r0,
            r1,
          )
      }
    }

    TypeofGlobal(name) -> {
      let v = ffi.get_global(agent, agent.realm.lexical_globals, name)
      case ffi.is(v, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False -> {
          let t = ffi.type_of_in(agent.store, v)
          case ffi.is(t, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [mk_string(t), ..stack],
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
      }
    }

    PutGlobal(name) ->
      case stack {
        [val, ..rest] -> {
          let realm = agent.realm
          let store =
            ffi.put_global(
              agent.store,
              realm.lexical_globals,
              realm.global_object,
              name,
              val,
              state.func.is_strict,
            )
          case ffi.is(store, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                Agent(..agent, store:),
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    NewObject -> {
      let #(obj, stack, store) =
        ffi.new_object(
          agent.store,
          agent.realm.object.prototype,
          keys,
          [],
          0,
          stack,
        )
      fast_loop(
        state,
        drive,
        pc + 1,
        [obj, ..stack],
        locals,
        Agent(..agent, store:),
        code,
        constants,
        keys,
        r0,
        r1,
      )
    }

    NewObjectWith(slots, count) -> {
      let #(obj, stack, store) =
        ffi.new_object(
          agent.store,
          agent.realm.object.prototype,
          keys,
          slots,
          count,
          stack,
        )
      fast_loop(
        state,
        drive,
        pc + 1,
        [obj, ..stack],
        locals,
        Agent(..agent, store:),
        code,
        constants,
        keys,
        r0,
        r1,
      )
    }

    TypeOf ->
      case stack {
        [v, ..rest] -> {
          let t = ffi.type_of_in(agent.store, v)
          case ffi.is(t, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
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
                keys,
                r0,
                r1,
              )
          }
        }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    DefineField(k) ->
      case tuple_array.element(k + 1, keys), stack {
        Named(_) as k, [val, obj, ..rest] -> {
          let store = ffi.define_field(agent.store, obj, k, val)
          case ffi.is(store, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [obj, ..rest],
                locals,
                Agent(..agent, store:),
                code,
                constants,
                keys,
                r0,
                r1,
              )
          }
        }
        _, _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    MakeClosure(func_index) -> {
      let template = tuple_array.element(func_index + 1, state.func.functions)
      let #(fn_h, agent) =
        closure.t_new_bytecode_function(
          agent,
          template,
          ffi.capture_env(template.env_descriptors, locals),
          state.unit,
        )
      fast_loop(
        state,
        drive,
        pc + 1,
        [mk_object(fn_h), ..stack],
        locals,
        agent,
        code,
        constants,
        keys,
        r0,
        r1,
      )
    }

    IteratorNext ->
      case stack {
        [rec, ..rest] ->
          case ffi.is(rec, ffi.Undefined) {
            True ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [mk_bool(True), mk_undefined(), rec, ..rest],
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
              )
            False ->
              case fast_iter_step(agent.store, rec) {
                ArrayStep(done, val, store) -> {
                  let agent = Agent(..agent, store:)
                  let slot = case done {
                    True -> mk_undefined()
                    False -> rec
                  }
                  fast_loop(
                    state,
                    drive,
                    pc + 1,
                    [mk_bool(done), val, slot, ..rest],
                    locals,
                    agent,
                    code,
                    constants,
                    keys,
                    r0,
                    r1,
                  )
                }
                // registers stay live across the step, flushed only on exits
                fast -> {
                  let state =
                    State(
                      ..state,
                      pc:,
                      stack:,
                      agent: call.sync(state, agent, pc, 0),
                    )
                  case iterator_next_slow(state, drive, rec, rest, fast) {
                    Ok(s) ->
                      fast_loop(
                        s,
                        drive,
                        s.pc,
                        s.stack,
                        locals,
                        s.agent,
                        code,
                        constants,
                        keys,
                        r0,
                        r1,
                      )
                    Error(exit) ->
                      after_step(
                        Error(
                          state.map_exit_state(exit, fn(s) {
                            State(..s, locals: fl(s, locals, r0, r1))
                          }),
                        ),
                        drive,
                      )
                  }
                }
              }
          }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    // coroutine frames never get registers, see compile_child
    Yield -> {
      let #(v, rest) = top_or_undefined(stack)
      let parked =
        State(
          ..state,
          pc: pc + 1,
          stack: rest,
          locals:,
          agent: call.sync(state, agent, pc, 0),
        )
      Ok(#(Suspended(state.Yield, v), parked))
    }

    Await -> {
      let #(v, rest) = top_or_undefined(stack)
      let parked =
        State(
          ..state,
          pc: pc + 1,
          stack: rest,
          locals:,
          agent: call.sync(state, agent, pc, 0),
        )
      Ok(#(Suspended(state.Await, v), parked))
    }

    InitialYield ->
      Ok(#(
        Suspended(state.Yield, mk_undefined()),
        State(
          ..state,
          pc: pc + 1,
          stack:,
          locals:,
          agent: call.sync(state, agent, pc, 0),
        ),
      ))

    PushTry(catch_target: Pc(catch_target), kind:) -> {
      let frame =
        TryFrame(catch_target:, stack_depth: list.length(stack), kind:)
      fast_loop(
        State(..state, try_stack: [frame, ..state.try_stack]),
        drive,
        pc + 1,
        stack,
        locals,
        agent,
        code,
        constants,
        keys,
        r0,
        r1,
      )
    }

    opcode.PopTry ->
      case state.try_stack {
        [_, ..try_rest] ->
          fast_loop(
            State(..state, try_stack: try_rest),
            drive,
            pc + 1,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
          )
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    CreateArguments(simple_params:) -> {
      let s =
        call.create_arguments(
          State(..state, pc:, stack:, locals: fl(state, locals, r0, r1), agent:),
          simple_params,
        )
      fast_loop(
        s,
        drive,
        s.pc,
        s.stack,
        locals,
        s.agent,
        code,
        constants,
        keys,
        r0,
        r1,
      )
    }

    Call(arity) ->
      case arity, stack {
        0, [callee, ..rest] ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ffi.cell_of(agent, callee),
            callee,
            ffi.val([ffi.Undefined]),
            [],
            rest,
            None,
            ffi.val([ffi.Undefined]),
          )
        1, [a, callee, ..rest] ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ffi.cell_of(agent, callee),
            callee,
            ffi.val([ffi.Undefined]),
            [a],
            rest,
            None,
            ffi.val([ffi.Undefined]),
          )
        2, [b, a, callee, ..rest] ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ffi.cell_of(agent, callee),
            callee,
            ffi.val([ffi.Undefined]),
            [a, b],
            rest,
            None,
            ffi.val([ffi.Undefined]),
          )
        _, _ ->
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
                keys,
                r0,
                r1,
                ffi.cell_of(agent, callee),
                callee,
                ffi.val([ffi.Undefined]),
                args,
                rest,
                None,
                ffi.val([ffi.Undefined]),
              )
            _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
          }
      }

    CallMethod(arity) ->
      case arity, stack {
        0, [method, receiver, ..rest] ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ffi.cell_of(agent, method),
            method,
            receiver,
            [],
            rest,
            None,
            ffi.val([ffi.Undefined]),
          )
        1, [a, method, receiver, ..rest] ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ffi.cell_of(agent, method),
            method,
            receiver,
            [a],
            rest,
            None,
            ffi.val([ffi.Undefined]),
          )
        2, [b, a, method, receiver, ..rest] ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ffi.cell_of(agent, method),
            method,
            receiver,
            [a, b],
            rest,
            None,
            ffi.val([ffi.Undefined]),
          )
        _, _ ->
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
                keys,
                r0,
                r1,
                ffi.cell_of(agent, method),
                method,
                receiver,
                args,
                rest,
                None,
                ffi.val([ffi.Undefined]),
              )
            _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
          }
      }

    GetFieldCall1(k, arg_idx) ->
      case stack {
        [recv, ..rest] -> {
          let method =
            ffi.get_field(agent, recv, tuple_array.element(k + 1, keys))
          let arg = case arg_idx < 0 {
            True ->
              case arg_idx {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(arg_idx + 1, locals)
          }
          case ffi.is(method, ffi.Miss) || ffi.is(arg, ffi.JsTdz) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                ffi.cell_of(agent, method),
                method,
                recv,
                [arg],
                rest,
                None,
                ffi.val([ffi.Undefined]),
              )
          }
        }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetFieldCall(k) ->
      case stack {
        [recv, ..rest] -> {
          let method =
            ffi.get_field(agent, recv, tuple_array.element(k + 1, keys))
          case ffi.is(method, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              fast_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                ffi.cell_of(agent, method),
                method,
                recv,
                [],
                rest,
                None,
                ffi.val([ffi.Undefined]),
              )
          }
        }
        [] -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetLocalFieldCall(index, k) -> {
      let recv = case index < 0 {
        True ->
          case index {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(index + 1, locals)
      }
      let method = ffi.get_field(agent, recv, tuple_array.element(k + 1, keys))
      case ffi.is(method, ffi.Miss) {
        True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ffi.cell_of(agent, method),
            method,
            recv,
            [],
            stack,
            None,
            ffi.val([ffi.Undefined]),
          )
      }
    }

    CallNew(arity) ->
      case pop_n(stack, arity) {
        Some(#(args, [ctor, ..rest])) ->
          fast_construct(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ctor,
            ctor,
            args,
            rest,
          )
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    ApplyArguments(slot:, ..) ->
      case stack {
        [this_arg, apply_fn, target, ..rest] ->
          case
            !is_handle(case slot < 0 {
              True ->
                case slot {
                  -1 -> r0
                  _ -> r1
                }
              False -> tuple_array.element(slot + 1, locals)
            })
            && is_intrinsic_apply(agent, apply_fn)
          {
            True ->
              fast_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                ffi.cell_of(agent, target),
                target,
                this_arg,
                state.call_args,
                rest,
                None,
                ffi.val([ffi.Undefined]),
              )
            False -> slow(state, drive, pc, stack, locals, agent, r0, r1)
          }
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    CallConstructor(arity) ->
      case pop_n(stack, arity) {
        Some(#(args, [new_target, ctor, ..rest])) ->
          fast_construct(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            ctor,
            new_target,
            args,
            rest,
          )
        _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
      }

    Return ->
      case state.call_stack {
        [saved, ..] ->
          case saved.constructor_this, state.func.is_derived_constructor {
            None, True ->
              after_step(
                call.return_op(
                  State(
                    ..state,
                    pc:,
                    stack:,
                    locals: fl(state, locals, r0, r1),
                    agent: call.sync(state, agent, pc, 0),
                  ),
                ),
                drive,
              )
            constructor_this, _ -> {
              let value = case constructor_this, stack {
                None, [v, ..] -> v
                None, [] -> ffi.val([ffi.Undefined])
                Some(receiver), [v, ..] ->
                  case !ffi.is(v, ffi.Undefined) && is_handle(v) {
                    True -> v
                    False -> receiver
                  }
                Some(receiver), [] -> receiver
              }
              let caller = saved.caller
              let caller_pc = saved.pc
              let caller_stack = saved.stack
              let caller_locals = saved.locals
              let depth = state.depth
              let agent = case agent.call_depth == depth {
                False -> agent
                True ->
                  Agent(
                    ..agent,
                    call_depth: depth - 1,
                    frames: case agent.frames {
                      [_, ..rest] -> rest
                      [] -> []
                    },
                  )
              }
              let caller_stack = [value, ..caller_stack]
              let store = agent.store
              case
                state.outer_depth == 0
                && store.alloc_since_gc >= store.gc_threshold
              {
                True -> {
                  let caller =
                    safepoint.maybe_collect_at_return(call.restore_frame(
                      call.sync(caller, agent, caller_pc, 0),
                      saved,
                      caller_stack,
                    ))
                  enter(
                    caller,
                    drive,
                    caller_pc,
                    caller.stack,
                    caller.locals,
                    caller.agent,
                    caller.func.bytecode,
                    caller.func.constants,
                    caller.func.keys,
                  )
                }
                False ->
                  case saved {
                    SavedRegFrame(r0:, r1:, ..) -> {
                      let caller_func = caller.func
                      let code = caller_func.bytecode
                      let constants = caller_func.constants
                      let keys = caller_func.keys
                      let _ = tuple_array.size(caller_locals)
                      let _ = tuple_array.size(code)
                      let _ = tuple_array.size(constants)
                      let _ = tuple_array.size(keys)
                      fast_loop(
                        caller,
                        drive,
                        caller_pc,
                        caller_stack,
                        caller_locals,
                        agent,
                        code,
                        constants,
                        keys,
                        r0,
                        r1,
                      )
                    }
                    SavedFrame(..) -> {
                      let caller_func = caller.func
                      case caller_func.regs {
                        bytecode.NoRegs -> {
                          let code = caller_func.bytecode
                          let constants = caller_func.constants
                          let keys = caller_func.keys
                          let _ = tuple_array.size(caller_locals)
                          let _ = tuple_array.size(code)
                          let _ = tuple_array.size(constants)
                          let _ = tuple_array.size(keys)
                          let u = ffi.val([ffi.Undefined])
                          fast_loop(
                            caller,
                            drive,
                            caller_pc,
                            caller_stack,
                            caller_locals,
                            agent,
                            code,
                            constants,
                            keys,
                            u,
                            u,
                          )
                        }
                        bytecode.Regs(..) ->
                          enter(
                            caller,
                            drive,
                            caller_pc,
                            caller_stack,
                            caller_locals,
                            agent,
                            caller_func.bytecode,
                            caller_func.constants,
                            caller_func.keys,
                          )
                      }
                    }
                  }
              }
            }
          }
        [] -> {
          let value = case stack {
            [v, ..] -> v
            [] -> ffi.val([ffi.Undefined])
          }
          // a finished frame's locals are never read, so no flush
          Ok(#(
            Completed(NormalCompletion(value)),
            State(..state, pc:, stack:, locals:, agent:),
          ))
        }
      }

    _other -> slow(state, drive, pc, stack, locals, agent, r0, r1)
  }
}

// new_target undefined = plain call; otherwise entered from fast_construct
fn fast_call(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  keys: TupleArray(PropertyKey),
  r0: JsVal,
  r1: JsVal,
  slot: rt_types.JsSlot,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest: List(JsVal),
  constructor_this: Option(JsVal),
  new_target: JsVal,
) -> Result(#(Outcome, State), VmError) {
  let depth = state.depth
  case ffi.is(slot, ffi.Miss) {
    False ->
      case slot {
        SObject(kind: KNative(tag:, ..), ..)
          if tag != function_call
          && tag != function_apply
          && tag != reflect_apply
          && depth < limits.max_call_depth
        -> {
          let agent = case agent.call_depth == depth {
            False -> call.sync(state, agent, pc, 1)
            True -> {
              let line = tuple_array.element(pc + 1, state.func.lines)
              let frames = case agent.frames {
                [rt_types.FrameInfo(line: l, ..), ..] as frames if l == line ->
                  frames
                [top, ..rest] -> [rt_types.FrameInfo(..top, line:), ..rest]
                [] -> [rt_types.FrameInfo("", call.stack_source, line)]
              }
              Agent(..agent, frames:, call_depth: depth + 1)
            }
          }
          case ffi.guard4(rt_builtins.dispatch_native, agent, tag, this, args) {
            ffi.Ok(value: v, agent:) ->
              fast_loop(
                state,
                drive,
                pc + 1,
                [v, ..rest],
                locals,
                Agent(..agent, call_depth: agent.call_depth - 1),
                code,
                constants,
                keys,
                r0,
                r1,
              )
            ffi.Threw(agent:, thrown:) ->
              after_step(
                Error(Threw(
                  thrown,
                  State(
                    ..state,
                    pc:,
                    stack: rest,
                    locals: fl(state, locals, r0, r1),
                    agent: Agent(..agent, call_depth: agent.call_depth - 1),
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
            && depth < limits.max_call_depth
            && case ffi.is(new_target, ffi.Undefined) {
              True ->
                !template.is_class_constructor
                && !template.is_generator
                && !template.is_async
              False -> True
            }
          {
            True -> {
              let home = case home_object {
                Some(h) -> ffi.object([h])
                None -> ffi.val([ffi.Undefined])
              }
              let #(this_val, agent) = case
                template.is_arrow || flags.is_strict
              {
                True -> #(this, agent)
                False ->
                  case ffi.is(this, ffi.Undefined) {
                    True -> #(ffi.object([agent.realm.global_object]), agent)
                    False -> {
                      let bound = ffi.bind_this(this, agent.realm.global_object)
                      case ffi.is(bound, ffi.Miss) {
                        False -> #(bound, agent)
                        True -> rt_call.resolve_this(agent, flags, this)
                      }
                    }
                  }
              }
              let callee_locals =
                ffi.frame_locals(
                  env,
                  template.lexical,
                  this_val,
                  callee,
                  home,
                  new_target,
                  args,
                  template.arity,
                  template.local_count,
                )
              let saved = case state.func.regs {
                bytecode.NoRegs ->
                  SavedFrame(
                    caller: state,
                    pc: pc + 1,
                    stack: rest,
                    locals:,
                    constructor_this:,
                  )
                bytecode.Regs(..) ->
                  SavedRegFrame(
                    caller: state,
                    pc: pc + 1,
                    stack: rest,
                    locals:,
                    constructor_this:,
                    r0:,
                    r1:,
                  )
              }
              let new_state =
                State(
                  agent:,
                  stack: [],
                  locals: callee_locals,
                  func: template,
                  unit:,
                  pc: 0,
                  call_stack: [saved, ..state.call_stack],
                  outer_depth: state.outer_depth,
                  depth: depth + 1,
                  try_stack: [],
                  this: this_val,
                  new_target:,
                  home_object: home,
                  call_args: args,
                  eval_env: None,
                )
              // §15.10 tail call elision
              let new_state = case
                state.func.is_strict
                && same_op(tuple_array.element(pc + 2, code), Return)
                && ffi.is(new_target, ffi.Undefined)
                && call.is_tail_call(state, pc, template)
              {
                True -> call.elide_tail_frame(new_state)
                False -> new_state
              }
              case template.regs {
                bytecode.NoRegs -> {
                  let code = template.bytecode
                  let constants = template.constants
                  let keys = template.keys
                  let _ = tuple_array.size(callee_locals)
                  let _ = tuple_array.size(code)
                  let _ = tuple_array.size(constants)
                  let _ = tuple_array.size(keys)
                  let u = ffi.val([ffi.Undefined])
                  fast_loop(
                    new_state,
                    drive,
                    0,
                    [],
                    callee_locals,
                    new_state.agent,
                    code,
                    constants,
                    keys,
                    u,
                    u,
                  )
                }
                bytecode.Regs(..) ->
                  enter(
                    new_state,
                    drive,
                    0,
                    [],
                    callee_locals,
                    new_state.agent,
                    template.bytecode,
                    template.constants,
                    template.keys,
                  )
              }
            }
            False ->
              case ffi.is(new_target, ffi.Undefined) {
                True ->
                  after_step(
                    call.call_cell(
                      State(
                        ..state,
                        pc:,
                        stack:,
                        locals: fl(state, locals, r0, r1),
                        agent: call.sync(state, agent, pc, 0),
                      ),
                      ffi.handle([callee]),
                      slot,
                      this,
                      args,
                      rest,
                      drive,
                    ),
                    drive,
                  )
                False -> slow(state, drive, pc, stack, locals, agent, r0, r1)
              }
          }
        slot ->
          after_step(
            call.call_cell(
              State(
                ..state,
                pc:,
                stack:,
                locals: fl(state, locals, r0, r1),
                agent: call.sync(state, agent, pc, 0),
              ),
              ffi.handle([callee]),
              slot,
              this,
              args,
              rest,
              drive,
            ),
            drive,
          )
      }
    True ->
      after_step(
        call.call(
          State(
            ..state,
            pc:,
            stack:,
            locals: fl(state, locals, r0, r1),
            agent: call.sync(state, agent, pc, 0),
          ),
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

fn fast_construct(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  keys: TupleArray(PropertyKey),
  r0: JsVal,
  r1: JsVal,
  ctor: JsVal,
  new_target: JsVal,
  args: List(JsVal),
  rest: List(JsVal),
) -> Result(#(Outcome, State), VmError) {
  case ffi.cell_of(agent, ctor) {
    SObject(kind: KBytecode(template:, flags:, realm:, ..), props:, ..) as slot
      if flags.is_constructor
      && realm == agent.realm.id
      && state.depth < limits.max_call_depth
    ->
      case template.is_derived_constructor {
        True ->
          fast_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            keys,
            r0,
            r1,
            slot,
            ctor,
            ffi.val([ffi.JsTdz]),
            args,
            rest,
            None,
            new_target,
          )
        False -> {
          let proto = case ffi.same(new_target, ctor) {
            True -> ffi.own_data(props, prototype_key)
            False -> ffi.get_field(agent, new_target, prototype_key)
          }
          let made = ffi.new_receiver(agent, proto)
          case ffi.is(made, ffi.Miss) {
            True -> slow(state, drive, pc, stack, locals, agent, r0, r1)
            False -> {
              let #(receiver, agent) = made
              fast_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                keys,
                r0,
                r1,
                slot,
                ctor,
                receiver,
                args,
                rest,
                Some(receiver),
                new_target,
              )
            }
          }
        }
      }
    _ -> slow(state, drive, pc, stack, locals, agent, r0, r1)
  }
}

const prototype_key = Named("prototype")

@external(erlang, "erlang", "=:=")
fn same_op(a: Op, b: Op) -> Bool

const function_call = FunctionN(FunctionCall)

fn is_intrinsic_apply(agent: Agent, v: JsVal) -> Bool {
  case ffi.cell_of(agent, v) {
    SObject(kind: KNative(tag:, ..), ..) -> tag == function_apply
    _ -> False
  }
}

const function_apply = FunctionN(FunctionApply)

const reflect_apply = ReflectN(ReflectApply)

fn instance_of_kernel(agent: Agent, left: JsVal, right: JsVal) -> JsVal {
  ffi.instance_of(agent, left, right, rt_types.symbol_has_instance)
}

@external(erlang, "arc_rt_ops_ffi", "pure_binop")
fn pure_binop_kernel(op: binop.PureBinOp, left: JsVal, right: JsVal) -> JsVal

fn top_or_undefined(stack: List(JsVal)) -> #(JsVal, List(JsVal)) {
  case stack {
    [v, ..rest] -> #(v, rest)
    [] -> #(mk_undefined(), [])
  }
}

fn dispatch_slow(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
) -> Result(#(Outcome, State), VmError) {
  let state =
    State(..state, pc:, stack:, locals:, agent: call.sync(state, agent, pc, 0))
  let func = state.func
  let op = tuple_array.element(pc + 1, func.bytecode)
  let op = case func.regs {
    bytecode.NoRegs -> op
    bytecode.Regs(a, b) ->
      opcode.map_slots(op, fn(i) {
        case i {
          -1 -> a
          -2 -> b
          _ -> i
        }
      })
  }
  case step(state, drive, op) {
    Ok(s) ->
      enter(
        s,
        drive,
        s.pc,
        s.stack,
        s.locals,
        s.agent,
        s.func.bytecode,
        s.func.constants,
        s.func.keys,
      )
    exit -> after_step(exit, drive)
  }
}

fn after_step(
  stepped: Result(State, StepExit),
  drive: Drive,
) -> Result(#(Outcome, State), VmError) {
  case stepped {
    Ok(s) ->
      enter(
        s,
        drive,
        s.pc,
        s.stack,
        s.locals,
        s.agent,
        s.func.bytecode,
        s.func.constants,
        s.func.keys,
      )
    Error(Returned(value, post)) ->
      Ok(#(Completed(NormalCompletion(value)), post))
    Error(VmFailed(err, _)) -> Error(err)
    Error(Yielded(kind, yielded_value, post)) -> {
      // must spread from post: the step may have run user code
      let parked = case kind {
        InitialSuspend -> State(..post, pc: post.pc + 1)
        PlainYield ->
          State(
            ..post,
            stack: case post.stack {
              [_, ..rest] -> rest
              [] -> []
            },
            pc: post.pc + 1,
          )
        // keep pc so the resume re-executes yieldstar
        DelegateYield ->
          State(..post, stack: case post.stack {
            [_arg, ..rest] -> rest
            [] -> []
          })
        AsyncDelegateResume(next_pc:) ->
          State(..post, pc: next_pc, stack: case post.stack {
            [_result_obj, ..rest] -> rest
            [] -> []
          })
      }
      Ok(#(Suspended(state.Yield, yielded_value), parked))
    }
    Error(Awaited(awaited_value, post)) -> {
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
      case unwind_to_catch(post, thrown) {
        Some(caught) -> execute_inner(caught, drive)
        None -> Ok(#(Completed(ThrowCompletion(thrown)), post))
      }
  }
}

pub fn truncate_stack(stack: List(JsVal), depth: Int) -> List(JsVal) {
  let excess = list.length(stack) - depth
  case excess > 0 {
    True -> list.drop(stack, excess)
    False -> stack
  }
}

// walks up caller frames when this frame has no handler
pub fn unwind_to_catch(state: State, thrown: JsVal) -> Option(State) {
  case state.try_stack {
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

fn step(state: State, drive: Drive, op: Op) -> Result(State, StepExit) {
  case op {
    PushConst(index) -> {
      let value = tuple_array.get_unchecked(index, state.func.constants)
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

    Rot3 ->
      case state.stack {
        [a, b, c, ..rest] ->
          Ok(State(..state, stack: [c, a, b, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Rot3")
      }

    Unrot4 ->
      case state.stack {
        [a, b, c, d, ..rest] ->
          Ok(State(..state, stack: [b, c, d, a, ..rest], pc: state.pc + 1))
        _ -> underflow(state, "Unrot4")
      }

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

    // §9.1.1.3.1 bindthisvalue: bound exactly once
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

    // §9.1.1.4.4 getbindingvalue
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

    // §9.1.1.4.5 setmutablebinding
    PutGlobal(name) ->
      case state.stack {
        [value, ..rest] ->
          case lex_lookup(state.agent, name) {
            // const rejects assignment even in tdz
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

    // §9.1.1.4.7 deletebinding, lexical bindings never deletable
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

    // §9.1.1.4.17 createglobalvarbinding, d = true only for eval
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

    // §19.2.1.3 no eval scope: global var, deletable
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

    // §13.2.8.4 gettemplateobject, cached per site
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

    // §7.1.19 topropertykey
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

    opcode.ToObject ->
      case state.stack {
        [val, ..rest] -> {
          use #(h, state) <- result.map(rt2(state, rt_val.t_to_object, val))
          State(..state, stack: [mk_object(h), ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "ToObject")
      }

    opcode.WithGetVar(name, Pc(target)) ->
      with_get_var(state, name, target, keep_this: False, op: "WithGetVar")

    opcode.WithGetVarThis(name, Pc(target)) ->
      with_get_var(state, name, target, keep_this: True, op: "WithGetVarThis")

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

    // §9.1.1.4.16 creategloballexbinding
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

    InitGlobalLex(name) ->
      case state.stack {
        [val, ..rest] -> {
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

    // tdz throws, undeclared is "undefined"
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

    BinOp(kind) ->
      case state.stack {
        [right, left, ..rest] -> binop_step(state, kind, left, right, rest)
        _ -> underflow(state, "BinOp")
      }

    BinOpConst(kind, const_index) ->
      case state.stack {
        [left, ..rest] ->
          binop_step(
            state,
            kind,
            left,
            tuple_array.get_unchecked(const_index, state.func.constants),
            rest,
          )
        _ -> underflow(state, "BinOpConst")
      }

    BinOpLocal(kind, index) ->
      case state.stack {
        [left, ..rest] -> {
          use right <- local_or_tdz(state, index)
          binop_step(state, kind, left, right, rest)
        }
        _ -> underflow(state, "BinOpLocal")
      }

    BinOpLocalLocal(kind, left_idx, right_idx) -> {
      use left <- local_or_tdz(state, left_idx)
      use right <- local_or_tdz(state, right_idx)
      binop_step(state, kind, left, right, state.stack)
    }

    BinOpLocalConst(kind, left_idx, const_index) -> {
      use left <- local_or_tdz(state, left_idx)
      binop_step(
        state,
        kind,
        left,
        tuple_array.get_unchecked(const_index, state.func.constants),
        state.stack,
      )
    }

    PostIncLocal(index) -> fused_postfix_local(state, index, True)
    PostDecLocal(index) -> fused_postfix_local(state, index, False)

    BinOpPut(kind, dst) ->
      case state.stack {
        [right, left, ..rest] ->
          binop_put_step(state, kind, left, right, rest, dst)
        _ -> underflow(state, "BinOpPut")
      }

    BinOpConstPut(kind, const_index, dst) ->
      case state.stack {
        [left, ..rest] ->
          binop_put_step(
            state,
            kind,
            left,
            tuple_array.get_unchecked(const_index, state.func.constants),
            rest,
            dst,
          )
        _ -> underflow(state, "BinOpConstPut")
      }

    BinOpLocalPut(kind, index, dst) ->
      case state.stack {
        [left, ..rest] -> {
          use right <- local_or_tdz(state, index)
          binop_put_step(state, kind, left, right, rest, dst)
        }
        _ -> underflow(state, "BinOpLocalPut")
      }

    BinOpLocalField(kind, index, k) ->
      case state.stack {
        [left, ..rest] -> {
          use receiver <- local_or_tdz(state, index)
          use #(right, state) <- result.try(get_field(
            state,
            receiver,
            key_at(state, k),
          ))
          binop_step(state, kind, left, right, rest)
        }
        _ -> underflow(state, "BinOpLocalField")
      }

    BinOpLocalLocalPut(kind, left_idx, right_idx, dst) -> {
      use left <- local_or_tdz(state, left_idx)
      use right <- local_or_tdz(state, right_idx)
      binop_put_step(state, kind, left, right, state.stack, dst)
    }

    UnaryOp(kind) ->
      case state.stack {
        [operand, ..rest] -> {
          use #(r, state) <- result.map(unaryop_slow(state, kind, operand))
          State(..state, stack: [r, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "UnaryOp")
      }

    IncLocal(index) -> fused_update_local(state, index, True)
    DecLocal(index) -> fused_update_local(state, index, False)
    IncLocalJump(index, Pc(target)) -> {
      use state <- result.map(fused_update_local(state, index, True))
      State(..state, pc: target)
    }
    IncLocalCmpConstJump(index, by, const_index, kind, Pc(target), when) -> {
      use stepped <- result.try(fused_update_local(state, index, by == 1))
      fused_cmp_jump(
        State(..stepped, pc: state.pc),
        kind,
        tuple_array.get_unchecked(index, stepped.locals),
        tuple_array.get_unchecked(const_index, state.func.constants),
        target,
        when,
      )
    }
    IncLocalCmpLocalJump(index, by, right_idx, kind, Pc(target), when) -> {
      use stepped <- result.try(fused_update_local(state, index, by == 1))
      let right = tuple_array.get_unchecked(right_idx, stepped.locals)
      case is_tdz(right) {
        True -> tdz_reference_error(stepped)
        False ->
          fused_cmp_jump(
            State(..stepped, pc: state.pc),
            kind,
            tuple_array.get_unchecked(index, stepped.locals),
            right,
            target,
            when,
          )
      }
    }

    CmpLocalLocalJump(left_idx, right_idx, kind, Pc(target), when) -> {
      let left = tuple_array.get_unchecked(left_idx, state.locals)
      let right = tuple_array.get_unchecked(right_idx, state.locals)
      case is_tdz(left) || is_tdz(right) {
        True -> tdz_reference_error(state)
        False -> fused_cmp_jump(state, kind, left, right, target, when)
      }
    }

    CmpLocalConstJump(left_idx, const_index, kind, Pc(target), when) -> {
      let left = tuple_array.get_unchecked(left_idx, state.locals)
      case is_tdz(left) {
        True -> tdz_reference_error(state)
        False ->
          fused_cmp_jump(
            state,
            kind,
            left,
            tuple_array.get_unchecked(const_index, state.func.constants),
            target,
            when,
          )
      }
    }

    CmpJump(kind, Pc(target), when) ->
      case state.stack {
        [right, left, ..rest] ->
          fused_cmp_jump(
            State(..state, stack: rest),
            kind,
            left,
            right,
            target,
            when,
          )
        _ -> underflow(state, "CmpJump")
      }

    CmpConstJump(const_index, kind, Pc(target), when) ->
      case state.stack {
        [left, ..rest] ->
          fused_cmp_jump(
            State(..state, stack: rest),
            kind,
            left,
            tuple_array.get_unchecked(const_index, state.func.constants),
            target,
            when,
          )
        _ -> underflow(state, "CmpConstJump")
      }

    Return -> call.return_op(state)

    Jump(Pc(target)) -> Ok(State(..state, pc: target))

    JumpIfFalse(Pc(target)) -> {
      use v <- conditional_jump(state, target)
      !ffi.truthy(v)
    }

    JumpIfTrue(Pc(target)) -> conditional_jump(state, target, ffi.truthy)

    JumpIfLocal(index, Pc(target), when) -> {
      let v = tuple_array.get_unchecked(index, state.locals)
      case is_tdz(v) {
        True -> tdz_reference_error(state)
        False ->
          case ffi.truthy(v) == when {
            True -> Ok(State(..state, pc: target))
            False -> Ok(State(..state, pc: state.pc + 1))
          }
      }
    }

    JumpIfNullish(Pc(target)) -> conditional_jump(state, target, ffi.nullish)
    JumpIfNotNullish(Pc(target)) -> {
      use v <- conditional_jump(state, target)
      !ffi.nullish(v)
    }

    // quickjs op_gosub
    opcode.Gosub(Pc(target)) ->
      Ok(
        State(
          ..state,
          stack: [int_val(state.pc + 1), ..state.stack],
          pc: target,
        ),
      )

    // quickjs op_ret; negative retpc: slot below is the return value
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

    NewObjectWith(slots, count) -> {
      let agent = state.agent
      let #(obj, stack, store) =
        ffi.new_object(
          agent.store,
          agent.realm.object.prototype,
          state.func.keys,
          slots,
          count,
          state.stack,
        )
      Ok(
        State(
          ..state,
          agent: Agent(..agent, store:),
          stack: [obj, ..stack],
          pc: state.pc + 1,
        ),
      )
    }

    GetField(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(get_field(
            state,
            receiver,
            key_at(state, k),
          ))
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField")
      }

    GetField2(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(val, state) <- result.map(get_field(
            state,
            receiver,
            key_at(state, k),
          ))
          State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField2")
      }

    GetLocalField(index, k) -> {
      let receiver = tuple_array.get_unchecked(index, state.locals)
      case is_tdz(receiver) {
        True -> tdz_reference_error(state)
        False -> {
          use #(val, state) <- result.map(get_field(
            state,
            receiver,
            key_at(state, k),
          ))
          State(..state, stack: [val, ..state.stack], pc: state.pc + 1)
        }
      }
    }

    GetFieldCall(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(method, state) <- result.try(get_field(
            state,
            receiver,
            key_at(state, k),
          ))
          call.call(state, method, receiver, [], rest, drive)
        }
        [] -> underflow(state, "GetFieldCall")
      }

    GetFieldCall1(k, arg_idx) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(method, state) <- result.try(get_field(
            state,
            receiver,
            key_at(state, k),
          ))
          let arg = tuple_array.get_unchecked(arg_idx, state.locals)
          case is_tdz(arg) {
            True -> tdz_reference_error(state)
            False -> call.call(state, method, receiver, [arg], rest, drive)
          }
        }
        [] -> underflow(state, "GetFieldCall1")
      }

    GetLocalFieldCall(index, k) -> {
      let receiver = tuple_array.get_unchecked(index, state.locals)
      case is_tdz(receiver) {
        True -> tdz_reference_error(state)
        False -> {
          use #(method, state) <- result.try(get_field(
            state,
            receiver,
            key_at(state, k),
          ))
          call.call(state, method, receiver, [], state.stack, drive)
        }
      }
    }

    GetLocalField2(index, k) -> {
      let receiver = tuple_array.get_unchecked(index, state.locals)
      case is_tdz(receiver) {
        True -> tdz_reference_error(state)
        False -> {
          use #(val, state) <- result.map(get_field(
            state,
            receiver,
            key_at(state, k),
          ))
          State(
            ..state,
            stack: [val, receiver, ..state.stack],
            pc: state.pc + 1,
          )
        }
      }
    }

    PutLocalLocalField(obj, value, k) -> {
      let receiver = tuple_array.get_unchecked(obj, state.locals)
      let val = tuple_array.get_unchecked(value, state.locals)
      case is_tdz(receiver) || is_tdz(val) {
        True -> tdz_reference_error(state)
        False ->
          put_field_step(state, key_at(state, k), val, receiver, state.stack)
      }
    }

    PutLocalConstField(obj, const_index, k) -> {
      let receiver = tuple_array.get_unchecked(obj, state.locals)
      case is_tdz(receiver) {
        True -> tdz_reference_error(state)
        False ->
          put_field_step(
            state,
            key_at(state, k),
            tuple_array.get_unchecked(const_index, state.func.constants),
            receiver,
            state.stack,
          )
      }
    }

    PutField(k) ->
      case state.stack {
        [value, receiver, ..rest] ->
          put_field_step(state, key_at(state, k), value, receiver, [
            value,
            ..rest
          ])
        _ -> underflow(state, "PutField")
      }

    PutFieldPop(k) ->
      case state.stack {
        [value, receiver, ..rest] ->
          put_field_step(state, key_at(state, k), value, receiver, rest)
        _ -> underflow(state, "PutFieldPop")
      }

    // §15.7.14 step 5/6 fresh privatename per class evaluation
    NewPrivateName(name) -> {
      let #(k, agent) = rt_class_new_private_name(state.agent, name)
      Ok(State(..state, agent:, stack: [k, ..state.stack], pc: state.pc + 1))
    }

    // §7.3.30 privateget
    GetPrivateFieldDyn ->
      case state.stack {
        [k, obj, ..rest] -> {
          use #(val, state) <- result.map(private_get(state, obj, k))
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetPrivateFieldDyn")
      }

    GetPrivateFieldDyn2 ->
      case state.stack {
        [k, obj, ..rest] -> {
          use #(val, state) <- result.map(private_get(state, obj, k))
          State(..state, stack: [val, obj, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetPrivateFieldDyn2")
      }

    // §7.3.31 privateset
    PutPrivateFieldDyn ->
      case state.stack {
        [k, val, obj, ..rest] -> {
          use #(v, state) <- result.map(private_set(state, obj, k, val))
          State(..state, stack: [v, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "PutPrivateFieldDyn")
      }

    // §13.10.1 #x in obj
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

    // §7.3.28 privatefieldadd
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

    // §7.3.29; non-writable so privateset's method check trips
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

    // §7.3.29 accessor half; a half already present is a typeerror
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

    // §7.3.7 own define, never walks the chain
    DefineField(k) ->
      case state.stack {
        [value, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use state <- result.map(create_data_property_or_throw(
                state,
                h,
                StringKey(key_at(state, k)),
                value,
              ))
              State(..state, stack: [obj, ..rest], pc: state.pc + 1)
            }
            None -> Ok(State(..state, pc: state.pc + 1))
          }
        _ -> underflow(state, "DefineField")
      }

    DefineMethod(k) ->
      case state.stack {
        [func, obj, ..rest] ->
          case handle_of(obj), handle_of(func) {
            Some(target), Some(fn_h) -> {
              use state <- result.map(rt_unit6(
                state,
                rt_class_define_method,
                target,
                StringKey(key_at(state, k)),
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

    DefineAccessor(k, kind, enumerable) ->
      case state.stack {
        [func, obj, ..rest] ->
          case handle_of(obj), handle_of(func) {
            Some(target), Some(fn_h) -> {
              use state <- result.map(rt_unit6(
                state,
                rt_class_define_method,
                target,
                StringKey(key_at(state, k)),
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
            None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
          }
        _ -> underflow(state, "DefineFieldComputed")
      }

    // annex b §b.3.1 __proto__ literal
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

    // §13.15.5.3 copydataproperties minus the bound keys
    ObjectRestCopy(excluded_count) ->
      case state.stack {
        [source, ..below] ->
          case pop_n(below, excluded_count) {
            Some(#(raw_keys, rest)) -> {
              let state = State(..state, stack: rest)
              // unlike spread, rest of null must throw
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

    DeleteField(k) ->
      case state.stack {
        [obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use #(deleted, state) <- result.try(rt3(
                state,
                rt_obj.t_delete_prop,
                h,
                StringKey(key_at(state, k)),
              ))
              // §13.5.1.2 step 5.b.i
              case deleted, state.func.is_strict {
                False, True ->
                  state.throw_type_error(
                    state,
                    "Cannot delete property '"
                      <> key_display_string(key_at(state, k))
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

    // §15.7.14 step 5: isconstructor before reading .prototype
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

    ArrayFrom(count) ->
      case pop_n(state.stack, count) {
        Some(#(items, rest)) -> {
          let #(arr, agent) = rt_obj.t_new_array(state.agent, items)
          Ok(State(..state, agent:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        None -> underflow(state, "ArrayFrom")
      }

    // emitter guarantees holes non-empty, ascending, within count
    ArrayFromWithHoles(count, holes) ->
      case pop_n(state.stack, count - list.length(holes)) {
        Some(#(values, rest)) -> {
          let items = fill_holes(values, holes, 0, count, [])
          let #(arr, agent) = rt_obj.t_new_array(state.agent, items)
          Ok(State(..state, agent:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        None -> underflow(state, "ArrayFromWithHoles")
      }

    GetElem ->
      case state.stack {
        [k, receiver, ..rest] -> get_elem_step(state, receiver, k, rest)
        _ -> underflow(state, "GetElem")
      }

    GetElemLocals(obj, key_idx) -> {
      use receiver <- local_or_tdz(state, obj)
      use k <- local_or_tdz(state, key_idx)
      get_elem_step(state, receiver, k, state.stack)
    }
    GetElemPostInc(obj, key_idx) -> {
      use receiver <- local_or_tdz(state, obj)
      use stepped <- result.try(fused_postfix_local(state, key_idx, True))
      case stepped.stack {
        [k, ..rest] ->
          get_elem_step(State(..stepped, pc: state.pc), receiver, k, rest)
        [] -> underflow(stepped, "GetElemPostInc")
      }
    }

    // §13.15.2 topropertykey runs once; converted key is left for putelem
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

    PutElem ->
      case state.stack {
        [val, k, receiver, ..rest] ->
          put_elem_step(state, val, k, receiver, [val, ..rest])
        _ -> underflow(state, "PutElem")
      }

    PutElemPop ->
      case state.stack {
        [val, k, receiver, ..rest] ->
          put_elem_step(state, val, k, receiver, rest)
        _ -> underflow(state, "PutElemPop")
      }

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

    // direct eval only if the callee is the intrinsic %eval%
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

    Call(arity) ->
      case pop_n(state.stack, arity) {
        Some(#(args, [callee, ..rest_stack])) ->
          call.call(state, callee, mk_undefined(), args, rest_stack, drive)
        Some(#(_, [])) -> underflow(state, "Call: no callee")
        None -> underflow(state, "Call: not enough args")
      }

    CallMethod(arity) ->
      case pop_n(state.stack, arity) {
        Some(#(args, [method, receiver, ..rest_stack])) ->
          call.call(state, method, receiver, args, rest_stack, drive)
        Some(#(_, _)) -> underflow(state, "CallMethod")
        None -> underflow(state, "CallMethod: not enough args")
      }

    CallNew(arity) ->
      case pop_n(state.stack, arity) {
        Some(#(args, [ctor, ..rest_stack])) ->
          call.construct(state, ctor, args, rest_stack, ctor, drive)
        Some(#(_, _)) -> underflow(state, "CallNew")
        None -> underflow(state, "CallNew: not enough args")
      }

    CallConstructor(arity) ->
      case pop_n(state.stack, arity) {
        Some(#(args, [new_target, ctor, ..rest_stack])) ->
          call.construct(state, ctor, args, rest_stack, new_target, drive)
        Some(#(_, _)) -> underflow(state, "CallConstructor")
        None -> underflow(state, "CallConstructor: not enough args")
      }

    ApplyArguments(slot:, simple_params:) ->
      case state.stack {
        [this_arg, apply_fn, target, ..rest] -> {
          let cached = tuple_array.get_unchecked(slot, state.locals)
          case is_object(cached), is_intrinsic_apply(state.agent, apply_fn) {
            False, True ->
              call.call(state, target, this_arg, state.call_args, rest, drive)
            False, False -> {
              let #(obj, agent) = call.arguments_object(state, simple_params)
              let locals = tuple_array.set_unchecked(slot, obj, state.locals)
              call.call(
                State(..state, agent:, locals:),
                apply_fn,
                target,
                [this_arg, obj],
                rest,
                drive,
              )
            }
            True, _ ->
              call.call(
                state,
                apply_fn,
                target,
                [this_arg, cached],
                rest,
                drive,
              )
          }
        }
        _ -> underflow(state, "ApplyArguments")
      }

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

    // bases here are never proxies, so no trap dispatch
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

    GetSuperValue -> get_super_value(state, False, "GetSuperValue")

    GetSuperValue2 -> get_super_value(state, True, "GetSuperValue2")

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
              // §6.2.5.6 putvalue step 5.c
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

    MakeClosure(func_index) -> {
      let template = tuple_array.get_unchecked(func_index, state.func.functions)
      let #(fn_h, agent) =
        closure.t_new_bytecode_function(
          state.agent,
          template,
          ffi.capture_env(template.env_descriptors, state.locals),
          state.unit,
        )
      Ok(
        State(
          ..state,
          agent:,
          stack: [mk_object(fn_h), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    // §14.7.5.6 key list computed up front
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

    // §7.4.1 getiterator sync
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

    // §7.4.3 getiterator async; next is read by what follows
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

    // §7.4.4 step 4
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

    // done or abrupt next: slot becomes undefined so later ops no-op
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
              case fast_iter_step(state.agent.store, rec) {
                ArrayStep(done, val, store) -> {
                  let agent = Agent(..state.agent, store:)
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
                fast -> iterator_next_slow(state, drive, rec, rest, fast)
              }
            }
          }
        _ -> underflow(state, "IteratorNext")
      }

    // §7.4.11 normal close
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

    // §7.4.11 throw close; the original error wins
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

    // §13.15.5.3 rest element, drains without close
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

    // §7.4.12 step 6
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

    InitialYield -> Error(Yielded(InitialSuspend, mk_undefined(), state))

    Yield ->
      case state.stack {
        [yielded, ..] -> Error(Yielded(PlainYield, yielded, state))
        [] -> Error(Yielded(PlainYield, mk_undefined(), state))
      }

    // §27.5.3.8; pc kept here so the resume re-enters
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

    Await ->
      case state.stack {
        [awaited, ..] -> Error(Awaited(awaited, state))
        [] -> Error(Awaited(mk_undefined(), state))
      }

    CreateArguments(simple_params:) ->
      Ok(call.create_arguments(state, simple_params))

    CreateRestArray(from_index) -> Ok(call.create_rest_array(state, from_index))

    // §13.2.7.3 fresh regexp per evaluation
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

    // §13.3.10 failures after arg evaluation reject the promise
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
  let agent = set_home_object(agent, fn_h, target)
  let agent = set_fn_name_if_empty(agent, fn_h, kind, k)
  rt_class.t_define_method(agent, target, k, fn_h, kind, enumerable)
}

// §10.2.9 setfunctionname from the computed key
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
    StringKey(pk) -> key_display_string(pk)
    SymbolKey(sym) ->
      case rt_types.symbol_description(sym) {
        Some(d) -> "[" <> d <> "]"
        None -> ""
      }
  }
  rt_obj.t_name_if_anonymous(agent, fn_h, prefix <> name)
}

fn accessor_install_kind(
  kind: opcode.AccessorKind,
) -> rt_types.MethodInstallKind {
  case kind {
    opcode.Getter -> rt_types.MIGetter
    opcode.Setter -> rt_types.MISetter
  }
}

fn tdz_reference_error(state: State) -> Result(State, StepExit) {
  state.throw_reference_error(
    state,
    "Cannot access variable before initialization (TDZ)",
  )
}

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

// §6.2.5.6 putvalue, static key
fn put_field_step(
  state: State,
  k: PropertyKey,
  value: JsVal,
  receiver: JsVal,
  stack: List(JsVal),
) -> Result(State, StepExit) {
  case classify(receiver) {
    KHandle(_) -> {
      use #(ok, state) <- result.try(rt4(
        state,
        rt_obj.t_set_prop,
        receiver,
        StringKey(k),
        value,
      ))
      case ok, state.func.is_strict {
        False, True ->
          state.throw_type_error(
            state,
            "Cannot assign to read only property '"
              <> key_display_string(k)
              <> "' of object",
          )
        _, _ -> Ok(State(..state, stack:, pc: state.pc + 1))
      }
    }
    KUndef | KNull ->
      state.throw_type_error(
        state,
        "Cannot set properties of "
          <> rt_val.nullish_label(receiver)
          <> " (setting '"
          <> key_display_string(k)
          <> "')",
      )
    _ ->
      case state.func.is_strict {
        True ->
          state.throw_type_error(
            state,
            "Cannot create property '"
              <> key_display_string(k)
              <> "' on primitive value",
          )
        False -> Ok(State(..state, stack:, pc: state.pc + 1))
      }
  }
}

fn get_field(
  state: State,
  receiver: JsVal,
  k: PropertyKey,
) -> Result(#(JsVal, State), StepExit) {
  case classify(receiver) {
    KUndef | KNull ->
      state.throw_type_error(
        state,
        "Cannot read properties of "
          <> rt_val.nullish_label(receiver)
          <> " (reading '"
          <> key_display_string(k)
          <> "')",
      )
    _ -> rt3(state, rt_obj.t_get_prop, receiver, StringKey(k))
  }
}

// §9.1.1.4.4 object record half
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

// §9.1.1.4.5 object record half
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

// §9.1.1.2.1 + §9.1.1.2.6 against a with object
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

// must match the unfused sequence's coercions exactly
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

fn get_elem_step(
  state: State,
  receiver: JsVal,
  k: JsVal,
  rest: List(JsVal),
) -> Result(State, StepExit) {
  case classify(receiver) {
    KUndef | KNull ->
      state.throw_type_error(
        state,
        "Cannot read properties of " <> rt_val.nullish_label(receiver),
      )
    _ -> {
      use #(pk, state) <- result.try(rt2(state, rt_val.t_to_property_key, k))
      use #(val, state) <- result.map(rt3(
        state,
        rt_obj.t_get_prop,
        receiver,
        pk,
      ))
      State(..state, stack: [val, ..rest], pc: state.pc + 1)
    }
  }
}

fn put_elem_step(
  state: State,
  val: JsVal,
  k: JsVal,
  receiver: JsVal,
  stack: List(JsVal),
) -> Result(State, StepExit) {
  case classify(receiver) {
    KHandle(_) -> {
      use #(pk, state) <- result.try(rt2(state, rt_val.t_to_property_key, k))
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
        _, _ -> Ok(State(..state, stack:, pc: state.pc + 1))
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
        False -> Ok(State(..state, stack:, pc: state.pc + 1))
      }
  }
}

fn fused_postfix_local(
  state: State,
  index: Int,
  increment: Bool,
) -> Result(State, StepExit) {
  let next_pc = state.pc + 1
  use v <- local_or_tdz(state, index)
  use #(n, state) <- result.try(rt2(state, rt_ops.t_plus, v))
  let one = int_val(1)
  use #(r, state) <- result.map(case increment {
    True -> rt3(state, rt_ops.t_add, n, one)
    False -> rt3(state, rt_ops.t_sub, n, one)
  })
  let locals = tuple_array.set_unchecked(index, r, state.locals)
  State(..state, stack: [n, ..state.stack], locals:, pc: next_pc)
}

fn local_or_tdz(
  state: State,
  index: Int,
  k: fn(JsVal) -> Result(State, StepExit),
) -> Result(State, StepExit) {
  let v = tuple_array.get_unchecked(index, state.locals)
  case is_tdz(v) {
    True -> tdz_reference_error(state)
    False -> k(v)
  }
}

fn binop_step(
  state: State,
  kind: opcode.Classified,
  left: JsVal,
  right: JsVal,
  rest: List(JsVal),
) -> Result(State, StepExit) {
  use #(r, state) <- result.map(binop_value(state, kind, left, right))
  State(..state, stack: [r, ..rest], pc: state.pc + 1)
}

fn binop_put_step(
  state: State,
  kind: opcode.Classified,
  left: JsVal,
  right: JsVal,
  rest: List(JsVal),
  dst: Int,
) -> Result(State, StepExit) {
  use #(r, state) <- result.map(binop_value(state, kind, left, right))
  let locals = tuple_array.set_unchecked(dst, r, state.locals)
  State(..state, stack: rest, locals:, pc: state.pc + 1)
}

fn binop_value(
  state: State,
  kind: opcode.Classified,
  left: JsVal,
  right: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  case kind {
    opcode.InstanceOfOp -> {
      use #(r, state) <- result.map(rt3(
        state,
        rt_ops.t_instance_of,
        left,
        right,
      ))
      #(mk_bool(r == 1), state)
    }
    opcode.InOp ->
      case is_object(right) {
        True -> {
          use #(r, state) <- result.map(rt3(state, rt_ops.t_in, left, right))
          #(mk_bool(r == 1), state)
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
    opcode.AddOp -> rt3(state, rt_ops.t_add, left, right)
    opcode.PureOp(op) -> pure_binop_slow(state, op, left, right)
  }
}

fn fused_cmp_jump(
  state: State,
  kind: binop.PureBinOp,
  left: JsVal,
  right: JsVal,
  target: Int,
  when: Bool,
) -> Result(State, StepExit) {
  let next_pc = state.pc + 1
  use #(r, state) <- result.map(pure_binop_slow(state, kind, left, right))
  case ffi.truthy(r) == when {
    True -> State(..state, pc: target)
    False -> State(..state, pc: next_pc)
  }
}

fn private_get(
  state: State,
  obj: JsVal,
  k: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  rt3(state, rt_class.t_private_get, obj, k)
}

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

// §7.3.7 via the real [[defineownproperty]]; false throws
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
    StringKey(pk) -> key_display_string(pk)
    SymbolKey(sym) -> rt_types.symbol_descriptive_string(sym)
  }
}

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

// §15.7.14 5.f-g: isconstructor before reading .prototype
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

fn own_prototype_handle(agent: Agent, h: Handle) -> Option(Handle) {
  case rt_obj.t_ordinary_own_property(agent, h, named("prototype")) {
    Some(DataProperty(value:, ..)) -> handle_of(value)
    _ -> None
  }
}

fn slot_prototype(agent: Agent, h: Handle) -> Option(Handle) {
  case rt_store.t_cell_get(agent, h) {
    SObject(proto:, ..) | SShapedObject(proto:, ..) -> proto
    _ -> None
  }
}

// targets are fresh objects, so a direct write is safe
fn set_slot_prototype(agent: Agent, h: Handle, proto: Option(Handle)) -> Agent {
  rt_store.t_cell_update(agent, h, fn(slot) {
    case slot {
      SObject(..) -> SObject(..slot, proto:)
      SShapedObject(..) -> SShapedObject(..slot, proto:)
      _ -> slot
    }
  })
}

// fresh literal: extensible, writable length
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

fn iterator_next_slow(
  state: State,
  drive: Drive,
  rec: JsVal,
  rest: List(JsVal),
  fast: FastIter,
) -> Result(State, StepExit) {
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

// §27.5.3.3; same-realm parked body resumes on this stack
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
      && agent.call_depth < limits.max_call_depth
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
  let depth = agent.call_depth
  let frames = agent.frames
  let running =
    Agent(
      ..agent,
      store: JsStore(
        ..store,
        data: arena.set(
          data.id,
          rt_types.SGenerator(state: rt_types.GenExecuting, resume:),
          store.data,
        ),
      ),
      call_depth: depth + 1,
      frames: [
        call.frame_info_at(
          frame.template,
          bytecode.line_at(frame.template, frame.pc),
        ),
        ..frames
      ],
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
    store: JsStore(..store, data: arena.set(data.id, gen, store.data)),
    call_depth: depth,
    frames:,
  )
}

fn resumed_turn(
  body: State,
  drive: Drive,
) -> #(Result(#(Outcome, State), VmError), Agent) {
  case execute_inner(body, drive) {
    Ok(#(_, post)) as res -> #(res, post.agent)
    Error(_) as res -> #(res, body.agent)
  }
}

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

type FastIter {
  ArrayStep(done: Bool, value: JsVal, store: rt_types.JsStore(Agent))
  GenStep(data: Handle)
  Protocol
}

// §23.1.5.2.1 in the kernel only when the read observes nothing
@external(erlang, "arc_interp_ffi", "iter_step")
fn fast_iter_step(store: rt_types.JsStore(Agent), rec: JsVal) -> FastIter

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

// §13.15.2 re-conversion must be side-effect free
fn prop_key_value(pk: ObjectKey) -> JsVal {
  case pk {
    SymbolKey(sym) -> rt_types.mk_symbol(sym)
    StringKey(Index(n)) -> int_val(n)
    StringKey(other) -> mk_string(key_to_text(other))
  }
}

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
        // base is null for class extends null
        None ->
          state.throw_type_error(
            state,
            "Cannot read super property when prototype is null",
          )
      }
    _ -> underflow(state, op)
  }
}

fn async_iterator_object(agent: Agent, iterable: JsVal) -> #(JsVal, Agent) {
  let #(method, agent) =
    rt_obj.t_get_prop(
      agent,
      iterable,
      SymbolKey(rt_types.symbol_async_iterator),
    )
  case classify(method) {
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

fn fault(s: State, err: VmError) -> #(Result(JsVal, JsVal), State) {
  let #(e, s) =
    state.new_error(
      s,
      rt_types.TypeErr,
      "internal error: " <> state.vm_error_message(err),
    )
  #(Error(e), s)
}
