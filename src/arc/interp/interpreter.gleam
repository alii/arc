import arc/bytecode/binop
import arc/bytecode/error_kind.{TypeError}
import arc/bytecode/key.{Index, Named}
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
  DefinePrivateMethod, DeleteElem, DeleteField, DeleteGlobalVar, Dup,
  DynamicImport, DynamicImportDefer, DynamicImportSource, ForInNext, ForInStart,
  GetAsyncIterator, GetBoxed, GetDisposer, GetElem, GetElemKeep, GetElemLocals,
  GetElemPostInc, GetEvalVar, GetField, GetFieldCall, GetFieldCall1,
  GetFieldKeep, GetGlobal, GetIterator, GetLocal, GetLocalField,
  GetLocalFieldCall, GetLocalFieldKeep, GetPrivateFieldDyn,
  GetPrivateFieldDynKeep, GetPrototypeOf, GetSuperValue, GetSuperValueKeep,
  GetTemplateObject, Gosub, IncLocal, IncLocalCmpConstJump, IncLocalCmpLocalJump,
  IncLocalJump, InitGlobalLex, InitialYield, IteratorCheckObject, IteratorClose,
  IteratorCloseThrow, IteratorNext, IteratorRecord, IteratorRest, Jump,
  JumpIfFalse, JumpIfLocal, JumpIfNotNullish, JumpIfNullish, JumpIfTrue,
  MakeClosure, MakeMethod, MakeSuppressed, NewObject, NewObjectWith,
  NewPrivateName, NewRegExp, ObjectRestCopy, ObjectSpread, Pc, Pop, PopTry,
  PostDecLocal, PostIncLocal, PrivateInDyn, PushConst, PushTry, PutBoxed,
  PutBoxedCheckInit, PutElem, PutElemPop, PutEvalVar, PutField, PutFieldPop,
  PutGlobal, PutLocal, PutLocalCheckInit, PutLocalConstField, PutLocalLocalField,
  PutPrivateFieldDyn, PutSuperValue, Ret, Return, Rot3, Safepoint, SetProto,
  SetupDerivedClass, Swap, Throw, ThrowConstAssign, ThrowError, ToObject,
  ToPropertyKey, ToStringVal, TypeOf, TypeofEvalVar, TypeofGlobal, UnaryOp,
  Unrot4, WithDeleteVar, WithGetRefValue, WithGetVar, WithGetVarThis,
  WithMakeRef, WithPutRefValue, WithPutVar, Yield, YieldStar,
}
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/call.{type Drive}
import arc/interp/eval
import arc/interp/frames
import arc/interp/guard.{
  Thrown, Value, guarded2, guarded3, guarded4, guarded5, guarded7, guarded_unit3,
  guarded_unit4, guarded_unit5, guarded_unit6,
}
import arc/interp/kernel
import arc/interp/park
import arc/interp/safepoint
import arc/interp/state.{
  type State, type StepExit, type VmError, AsyncDelegateResume, Awaited,
  DelegateYield, InitialSuspend, InternalError, PlainYield, Returned, SavedCont,
  SavedFrame, SavedRegFrame, StackUnderflow, State, SuspensionLeak, Threw,
  VmFailed, Yielded,
}
import arc/interp/using
import arc/module/dynamic_import
import arc/rt/arena
import arc/rt/async as rt_async
import arc/rt/builtins as rt_builtins
import arc/rt/builtins/error as b_error
import arc/rt/builtins/global_fns
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/regexp as b_regexp
import arc/rt/bytecode.{type SuspendedFrame, ParkedOp, ParkedStart, TryFrame}
import arc/rt/call.{type Completion, NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/class as rt_class
import arc/rt/closure as rt_closure
import arc/rt/elements
import arc/rt/env as rt_env
import arc/rt/inspect as rt_inspect
import arc/rt/js_string
import arc/rt/lang as rt_lang
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type LexicalGlobal, type ObjectKey,
  AccessorProperty, Agent, BytecodeFn, DataProperty, FunctionApply, FunctionCall,
  FunctionN, HintString, KHandle, KNull, KNum, KStr, KSym, KUndef, NativeFn,
  Realm, ReflectApply, ReflectN, SBox, SObject, SShapedObject, Store, StringKey,
  SymbolKey, classify, mk_bool, mk_int, mk_object, mk_string, mk_tdz,
  mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Outcome {
  Completed(Completion(JsVal))
  Suspended(state.SuspendKind, JsVal)
}

const prototype_key = key.Named("prototype")

const return_key = key.Named("return")

const function_call = FunctionN(FunctionCall)

const function_apply = FunctionN(FunctionApply)

const reflect_apply = ReflectN(ReflectApply)

fn is_undef(v: JsVal) -> Bool {
  kernel.is(v, kernel.Undefined)
}

fn handle_of(v: JsVal) -> Option(Handle) {
  case classify(v) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}

fn inspect(state: State, v: JsVal) -> String {
  rt_inspect.describe(state.agent, v)
}

fn lexical_global(agent: Agent, name: String) -> Option(LexicalGlobal) {
  dict.get(agent.realm.lexical_globals, name) |> option.from_result
}

fn put_lexical_global(
  agent: Agent,
  name: String,
  binding: LexicalGlobal,
) -> Agent {
  let realm = agent.realm
  Agent(
    ..agent,
    realm: Realm(
      ..realm,
      lexical_globals: dict.insert(realm.lexical_globals, name, binding),
    ),
  )
}

// loop tier: registers live in loop arguments
pub fn execute(
  state: State,
  drive: Drive,
) -> Result(#(Outcome, State), VmError) {
  let func = state.func
  let locals = state.locals
  let code = func.bytecode
  let constants = func.constants
  let _ = tuple_array.size(locals)
  let _ = tuple_array.size(code)
  let _ = tuple_array.size(constants)
  case func.regs {
    bytecode.NoRegs -> {
      let u = kernel.literal([kernel.Undefined])
      loop(
        state,
        drive,
        state.pc,
        state.stack,
        locals,
        state.agent,
        code,
        constants,
        u,
        u,
      )
    }
    bytecode.Regs(a, b) ->
      loop(
        state,
        drive,
        state.pc,
        state.stack,
        locals,
        state.agent,
        code,
        constants,
        load_register(locals, a),
        load_register(locals, b),
      )
  }
}

pub fn execute_to_completion(
  state: State,
  drive: Drive,
  site: String,
) -> #(Result(JsVal, JsVal), State) {
  case execute(state, drive) {
    Ok(#(Completed(NormalCompletion(v)), state)) -> #(Ok(v), state)
    Ok(#(Completed(ThrowCompletion(e)), state)) -> #(Error(e), state)
    Ok(#(Suspended(kind, _), state)) ->
      state.internal_fault(state, SuspensionLeak(site:, kind:))
    Error(err) -> state.internal_fault(state, err)
  }
}

// tuple_size tells the erlang compiler these are tuples so element inlines
fn enter_loop(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
) -> Result(#(Outcome, State), VmError) {
  let _ = tuple_array.size(locals)
  let _ = tuple_array.size(code)
  let _ = tuple_array.size(constants)
  case state.func.regs {
    bytecode.NoRegs -> {
      let u = kernel.literal([kernel.Undefined])
      loop(state, drive, pc, stack, locals, agent, code, constants, u, u)
    }
    bytecode.Regs(a, b) ->
      loop(
        state,
        drive,
        pc,
        stack,
        locals,
        agent,
        code,
        constants,
        load_register(locals, a),
        load_register(locals, b),
      )
  }
}

fn load_register(locals: TupleArray(JsVal), local: Int) -> JsVal {
  case local < 0 {
    True -> kernel.literal([kernel.Undefined])
    False -> tuple_array.element(local + 1, locals)
  }
}

// -1 is reg_a_slot; the literal keeps these arms inlinable
fn continue_with_register(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  r0: JsVal,
  r1: JsVal,
  register: Int,
  v: JsVal,
) -> Result(#(Outcome, State), VmError) {
  case register {
    -1 -> loop(state, drive, pc, stack, locals, agent, code, constants, v, r1)
    _ -> loop(state, drive, pc, stack, locals, agent, code, constants, r0, v)
  }
}

// registers written back so the tuple can leave the loop
fn flush_registers(
  state: State,
  locals: TupleArray(JsVal),
  r0: JsVal,
  r1: JsVal,
) -> TupleArray(JsVal) {
  case state.func.regs {
    bytecode.NoRegs -> locals
    bytecode.Regs(a, b) -> kernel.flush_registers(locals, a, b, r0, r1)
  }
}

fn via_step(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  r0: JsVal,
  r1: JsVal,
) -> Result(#(Outcome, State), VmError) {
  step_from_loop(
    state,
    drive,
    pc,
    stack,
    flush_registers(state, locals, r0, r1),
    agent,
  )
}

// nothing ran before the miss, so step re-runs with the loop args
fn loop(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  r0: JsVal,
  r1: JsVal,
) -> Result(#(Outcome, State), VmError) {
  // every stream ends in a sentinel return, so fetch is unchecked
  case tuple_array.element(pc + 1, code) {
    PushConst(index) -> {
      let v = tuple_array.element(index + 1, constants)
      loop(
        state,
        drive,
        pc + 1,
        [v, ..stack],
        locals,
        agent,
        code,
        constants,
        r0,
        r1,
      )
    }

    Pop ->
      case stack {
        [_, ..rest] ->
          loop(
            state,
            drive,
            pc + 1,
            rest,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    Dup ->
      case stack {
        [top, ..] ->
          loop(
            state,
            drive,
            pc + 1,
            [top, ..stack],
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    Swap ->
      case stack {
        [a, b, ..rest] ->
          loop(
            state,
            drive,
            pc + 1,
            [b, a, ..rest],
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
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
      case kernel.is(v, kernel.JsTdz) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            [v, ..stack],
            locals,
            agent,
            code,
            constants,
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
              continue_with_register(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                index,
                v,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                tuple_array.set_element(index + 1, locals, v),
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetBoxed(index) -> {
      let v =
        kernel.box_get(agent, case index < 0 {
          True ->
            case index {
              -1 -> r0
              _ -> r1
            }
          False -> tuple_array.element(index + 1, locals)
        })
      case kernel.is(v, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            [v, ..stack],
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
      }
    }

    PutBoxed(index) ->
      case stack {
        [v, ..rest] -> {
          let local = case index < 0 {
            True ->
              case index {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(index + 1, locals)
          }
          case rt_store.is_handle(local) {
            False -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            True ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                rt_store.cell_set(
                  agent,
                  kernel.to_handle_unchecked([local]),
                  SBox(v),
                ),
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    Safepoint ->
      case agent.store.alloc_since_gc < agent.store.gc_threshold {
        True ->
          loop(
            state,
            drive,
            pc + 1,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
        False -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    Jump(Pc(target)) ->
      loop(state, drive, target, stack, locals, agent, code, constants, r0, r1)

    JumpIfFalse(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case kernel.is_bool(top, expected: True) {
            True ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            False ->
              case kernel.is_bool(top, expected: False) {
                True ->
                  loop(
                    state,
                    drive,
                    target,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
                False ->
                  case rt_val.to_boolean(top) {
                    True ->
                      loop(
                        state,
                        drive,
                        pc + 1,
                        rest,
                        locals,
                        agent,
                        code,
                        constants,
                        r0,
                        r1,
                      )
                    False ->
                      loop(
                        state,
                        drive,
                        target,
                        rest,
                        locals,
                        agent,
                        code,
                        constants,
                        r0,
                        r1,
                      )
                  }
              }
          }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    JumpIfTrue(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case kernel.is_bool(top, expected: True) {
            True ->
              loop(
                state,
                drive,
                target,
                rest,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            False ->
              case kernel.is_bool(top, expected: False) {
                True ->
                  loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
                False ->
                  case rt_val.to_boolean(top) {
                    True ->
                      loop(
                        state,
                        drive,
                        target,
                        rest,
                        locals,
                        agent,
                        code,
                        constants,
                        r0,
                        r1,
                      )
                    False ->
                      loop(
                        state,
                        drive,
                        pc + 1,
                        rest,
                        locals,
                        agent,
                        code,
                        constants,
                        r0,
                        r1,
                      )
                  }
              }
          }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    JumpIfNullish(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case kernel.is(top, kernel.Undefined) || kernel.is(top, kernel.Null) {
            True ->
              loop(
                state,
                drive,
                target,
                rest,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    JumpIfNotNullish(Pc(target)) ->
      case stack {
        [top, ..rest] ->
          case kernel.is(top, kernel.Undefined) || kernel.is(top, kernel.Null) {
            False ->
              loop(
                state,
                drive,
                target,
                rest,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            True ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOp(kind) ->
      case stack {
        [right, left, ..rest] -> {
          let r = case kind {
            binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> kernel.classified_binop(kind, left, right)
          }
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [r, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpConst(kind, const_index) ->
      case stack {
        [left, ..rest] -> {
          let right = tuple_array.element(const_index + 1, constants)
          let r = case kind {
            binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> kernel.classified_binop(kind, left, right)
          }
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [r, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
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
            binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> kernel.classified_binop(kind, left, right)
          }
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [r, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
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
        binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
        _ -> kernel.classified_binop(kind, left, right)
      }
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            [r, ..stack],
            locals,
            agent,
            code,
            constants,
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
        binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
        _ -> kernel.classified_binop(kind, left, right)
      }
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            [r, ..stack],
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
      }
    }

    BinOpPut(kind, dst) ->
      case stack {
        [right, left, ..rest] -> {
          let r = case kind {
            binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> kernel.classified_binop(kind, left, right)
          }
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case dst < 0 {
                True ->
                  continue_with_register(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                    dst,
                    r,
                  )
                False ->
                  loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    tuple_array.set_element(dst + 1, locals, r),
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpConstPut(kind, const_index, dst) ->
      case stack {
        [left, ..rest] -> {
          let right = tuple_array.element(const_index + 1, constants)
          let r = case kind {
            binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> kernel.classified_binop(kind, left, right)
          }
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case dst < 0 {
                True ->
                  continue_with_register(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                    dst,
                    r,
                  )
                False ->
                  loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    tuple_array.set_element(dst + 1, locals, r),
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
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
            binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
            _ -> kernel.classified_binop(kind, left, right)
          }
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case dst < 0 {
                True ->
                  continue_with_register(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                    dst,
                    r,
                  )
                False ->
                  loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    tuple_array.set_element(dst + 1, locals, r),
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    BinOpLocalField(kind, index, key.Named(_) as k) ->
      case stack {
        [left, ..rest] -> {
          let right =
            kernel.get_field(
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
          case kernel.is(right, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False -> {
              let r = case kind {
                binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
                _ -> kernel.classified_binop(kind, left, right)
              }
              case kernel.is(r, kernel.Miss) {
                True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
                False ->
                  loop(
                    state,
                    drive,
                    pc + 1,
                    [r, ..rest],
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
              }
            }
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
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
        binop.InstanceOfOp -> instance_of_kernel(agent, left, right)
        _ -> kernel.classified_binop(kind, left, right)
      }
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case dst < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                dst,
                r,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                stack,
                tuple_array.set_element(dst + 1, locals, r),
                agent,
                code,
                constants,
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
            opcode.Neg -> kernel.neg(operand)
            opcode.Pos -> kernel.plus(operand)
            opcode.LogicalNot -> rt_val.logical_not(operand)
            opcode.Void -> mk_undefined()
            opcode.BitNot -> kernel.bitnot(operand)
          }
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [r, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    IncLocal(index) -> {
      let r =
        kernel.step(
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
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                index,
                r,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                stack,
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
      }
    }

    DecLocal(index) -> {
      let r =
        kernel.step(
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
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                index,
                r,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                stack,
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
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
      case kernel.is(v, kernel.JsTdz) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case rt_val.to_boolean(v) == when {
            True ->
              loop(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
      }
    }

    IncLocalJump(index, Pc(target)) -> {
      let r =
        kernel.step(
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
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                index,
                r,
              )
            False ->
              loop(
                state,
                drive,
                target,
                stack,
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
      }
    }

    IncLocalCmpConstJump(index, by, const_index, kind, Pc(target), when) -> {
      let n =
        kernel.step(
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
      let r = case kernel.is(n, kernel.Miss) {
        True -> n
        False ->
          kernel.pure_binop(
            kind,
            n,
            tuple_array.element(const_index + 1, constants),
          )
      }
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False -> {
          let next = case kernel.is_bool(r, when) {
            True -> target
            False -> pc + 1
          }
          case index < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                next,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                index,
                n,
              )
            False ->
              loop(
                state,
                drive,
                next,
                stack,
                tuple_array.set_element(index + 1, locals, n),
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
      }
    }

    IncLocalCmpLocalJump(index, by, right_idx, kind, Pc(target), when) -> {
      let n =
        kernel.step(
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
      let r = case kernel.is(n, kernel.Miss) {
        True -> n
        False ->
          kernel.pure_binop(kind, n, case right_idx < 0 {
            True ->
              case right_idx {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(right_idx + 1, locals)
          })
      }
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False -> {
          let next = case kernel.is_bool(r, when) {
            True -> target
            False -> pc + 1
          }
          case index < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                next,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                index,
                n,
              )
            False ->
              loop(
                state,
                drive,
                next,
                stack,
                tuple_array.set_element(index + 1, locals, n),
                agent,
                code,
                constants,
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
      let r = kernel.step(old, 1)
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                pc + 1,
                [old, ..stack],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                index,
                r,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [old, ..stack],
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
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
      let r = kernel.step(old, -1)
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case index < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                pc + 1,
                [old, ..stack],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                index,
                r,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [old, ..stack],
                tuple_array.set_element(index + 1, locals, r),
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
      }
    }

    CmpLocalLocalJump(left_idx, right_idx, kind, Pc(target), when) -> {
      let r =
        kernel.pure_binop(
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
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case kernel.is_bool(r, when) {
            True ->
              loop(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
      }
    }

    CmpLocalConstJump(left_idx, const_index, kind, Pc(target), when) -> {
      let r =
        kernel.pure_binop(
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
      case kernel.is(r, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case kernel.is_bool(r, when) {
            True ->
              loop(
                state,
                drive,
                target,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
      }
    }

    CmpJump(kind, Pc(target), when) ->
      case stack {
        [right, left, ..rest] -> {
          let r = kernel.pure_binop(kind, left, right)
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case kernel.is_bool(r, when) {
                True ->
                  loop(
                    state,
                    drive,
                    target,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
                False ->
                  loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    CmpConstJump(const_index, kind, Pc(target), when) ->
      case stack {
        [left, ..rest] -> {
          let r =
            kernel.pure_binop(
              kind,
              left,
              tuple_array.element(const_index + 1, constants),
            )
          case kernel.is(r, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              case kernel.is_bool(r, when) {
                True ->
                  loop(
                    state,
                    drive,
                    target,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
                False ->
                  loop(
                    state,
                    drive,
                    pc + 1,
                    rest,
                    locals,
                    agent,
                    code,
                    constants,
                    r0,
                    r1,
                  )
              }
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetElem ->
      case stack {
        [k, recv, ..rest] -> {
          let v = kernel.get_elem(agent.store, recv, k)
          case kernel.is(v, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [v, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetElemKeep ->
      case stack {
        [k, recv, ..] -> {
          let v = kernel.get_elem_keep(agent.store, recv, k)
          case kernel.is(v, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [v, ..stack],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    PutElem ->
      case stack {
        [val, k, recv, ..rest] -> {
          let store = kernel.put_elem(agent.store, recv, k, val)
          case kernel.is(store, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [val, ..rest],
                locals,
                Agent(..agent, store:),
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetElemLocals(obj, key_idx) -> {
      let v =
        kernel.get_elem(
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
      case kernel.is(v, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            [v, ..stack],
            locals,
            agent,
            code,
            constants,
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
      let r = kernel.step(old, 1)
      let v = case kernel.is(r, kernel.Miss) {
        True -> r
        False ->
          kernel.get_elem(
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
      case kernel.is(v, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          case key_idx < 0 {
            True ->
              continue_with_register(
                state,
                drive,
                pc + 1,
                [v, ..stack],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                key_idx,
                r,
              )
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [v, ..stack],
                tuple_array.set_element(key_idx + 1, locals, r),
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
      }
    }

    PutElemPop ->
      case stack {
        [val, k, recv, ..rest] -> {
          let store = kernel.put_elem(agent.store, recv, k, val)
          case kernel.is(store, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                Agent(..agent, store:),
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetField(key.Named(_) as k) ->
      case stack {
        [recv, ..rest] -> {
          let v = kernel.get_field(agent, recv, k)
          case kernel.is(v, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [v, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetFieldKeep(key.Named(_) as k) ->
      case stack {
        [recv, ..rest] -> {
          let v = kernel.get_field(agent, recv, k)
          case kernel.is(v, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [v, recv, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    PutField(key.Named(_) as k) ->
      case stack {
        [val, recv, ..rest] -> {
          let store = kernel.put_field(agent.store, recv, k, val, create: True)
          case kernel.is(store, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [val, ..rest],
                locals,
                Agent(..agent, store:),
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    PutFieldPop(key.Named(_) as k) ->
      case stack {
        [val, recv, ..rest] -> {
          let store = kernel.put_field(agent.store, recv, k, val, create: True)
          case kernel.is(store, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                Agent(..agent, store:),
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
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
      case kernel.is(val, kernel.JsTdz) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False -> {
          let store =
            kernel.put_field(
              agent.store,
              case obj < 0 {
                True ->
                  case obj {
                    -1 -> r0
                    _ -> r1
                  }
                False -> tuple_array.element(obj + 1, locals)
              },
              k,
              val,
              create: True,
            )
          case kernel.is(store, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                stack,
                locals,
                Agent(..agent, store:),
                code,
                constants,
                r0,
                r1,
              )
          }
        }
      }
    }

    PutLocalConstField(obj, const_index, k) -> {
      let store =
        kernel.put_field(
          agent.store,
          case obj < 0 {
            True ->
              case obj {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(obj + 1, locals)
          },
          k,
          tuple_array.element(const_index + 1, constants),
          create: True,
        )
      case kernel.is(store, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            stack,
            locals,
            Agent(..agent, store:),
            code,
            constants,
            r0,
            r1,
          )
      }
    }

    GetLocalField(index, key.Named(_) as k) -> {
      let v =
        kernel.get_field(
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
      case kernel.is(v, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            [v, ..stack],
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
      }
    }

    GetLocalFieldKeep(index, key.Named(_) as k) -> {
      let recv = case index < 0 {
        True ->
          case index {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(index + 1, locals)
      }
      let v = kernel.get_field(agent, recv, k)
      case kernel.is(v, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            [v, recv, ..stack],
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
      }
    }

    GetGlobal(name) -> {
      let v = kernel.get_global(agent, agent.realm.lexical_globals, name)
      case kernel.is(v, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop(
            state,
            drive,
            pc + 1,
            [v, ..stack],
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
      }
    }

    TypeofGlobal(name) -> {
      let v = kernel.get_global(agent, agent.realm.lexical_globals, name)
      case kernel.is(v, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False -> {
          let t = kernel.type_of(agent.store, v)
          case kernel.is(t, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [mk_string(t), ..stack],
                locals,
                agent,
                code,
                constants,
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
            kernel.put_global(
              agent.store,
              realm.lexical_globals,
              realm.global_object,
              name,
              val,
              state.func.is_strict,
            )
          case kernel.is(store, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                Agent(..agent, store:),
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    NewObject -> {
      let #(obj, stack, store) =
        kernel.new_object(
          agent.store,
          agent.realm.object.prototype,
          [],
          0,
          stack,
        )
      loop(
        state,
        drive,
        pc + 1,
        [obj, ..stack],
        locals,
        Agent(..agent, store:),
        code,
        constants,
        r0,
        r1,
      )
    }

    NewObjectWith(keys, count) -> {
      let #(obj, stack, store) =
        kernel.new_object(
          agent.store,
          agent.realm.object.prototype,
          keys,
          count,
          stack,
        )
      loop(
        state,
        drive,
        pc + 1,
        [obj, ..stack],
        locals,
        Agent(..agent, store:),
        code,
        constants,
        r0,
        r1,
      )
    }

    TypeOf ->
      case stack {
        [v, ..rest] -> {
          let t = kernel.type_of(agent.store, v)
          case kernel.is(t, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [mk_string(t), ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    DefineField(key.Named(_) as k) ->
      case stack {
        [val, obj, ..rest] -> {
          let store = kernel.define_field(agent.store, obj, k, val)
          case kernel.is(store, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [obj, ..rest],
                locals,
                Agent(..agent, store:),
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    MakeClosure(func_index) -> {
      let template = tuple_array.element(func_index + 1, state.func.functions)
      let #(fn_h, agent) =
        rt_closure.new_bytecode_function(
          agent,
          template,
          kernel.capture_env(template.env_descriptors, locals),
          state.unit_id,
        )
      loop(
        state,
        drive,
        pc + 1,
        [mk_object(fn_h), ..stack],
        locals,
        agent,
        code,
        constants,
        r0,
        r1,
      )
    }

    GetIterator ->
      case stack {
        [iterable, ..rest] -> {
          let rec = rt_lang.array_iter_start(agent, iterable)
          case kernel.is(rec, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop(
                state,
                drive,
                pc + 1,
                [rec, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    IteratorClose ->
      case stack {
        [rec, ..rest] ->
          case
            kernel.is(rec, kernel.Undefined)
            || {
              rt_lang.is_array_iter(rec)
              && kernel.is(
                kernel.get_field(
                  agent,
                  mk_object(rt_lang.array_iter_proto(agent, rec)),
                  return_key,
                ),
                kernel.Undefined,
              )
            }
          {
            True ->
              loop(
                state,
                drive,
                pc + 1,
                rest,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            False -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
          }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    ForInNext ->
      case stack {
        [iter, ..rest] ->
          case kernel.for_in_next(iter) {
            kernel.ForInKey(key:, rest: iter) ->
              loop(
                state,
                drive,
                pc + 1,
                [mk_bool(False), key, iter, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            kernel.ForInEnd ->
              loop(
                state,
                drive,
                pc + 1,
                [mk_bool(True), mk_undefined(), iter, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
          }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    IteratorNext ->
      case stack {
        [rec, ..rest] ->
          case kernel.is(rec, kernel.Undefined) {
            True ->
              loop(
                state,
                drive,
                pc + 1,
                [mk_bool(True), mk_undefined(), rec, ..rest],
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
              )
            False ->
              case rt_lang.is_array_iter(rec) {
                True ->
                  case rt_lang.array_iter_next(agent.store, rec) {
                    rt_lang.IterStep(done:, value:, rec:) ->
                      loop(
                        state,
                        drive,
                        pc + 1,
                        [mk_bool(done), value, rec, ..rest],
                        locals,
                        agent,
                        code,
                        constants,
                        r0,
                        r1,
                      )
                    rt_lang.IterPair(key:, value:, rec:) -> {
                      let #(pair, agent) = rt_obj.new_array(agent, [key, value])
                      loop(
                        state,
                        drive,
                        pc + 1,
                        [mk_bool(False), pair, rec, ..rest],
                        locals,
                        agent,
                        code,
                        constants,
                        r0,
                        r1,
                      )
                    }
                    rt_lang.IterMiss ->
                      via_step(state, drive, pc, stack, locals, agent, r0, r1)
                  }
                False ->
                  case kernel.iter_step(agent.store, rec) {
                    kernel.ArrayAdvanced(done, val, store) -> {
                      let agent = Agent(..agent, store:)
                      let record = case done {
                        True -> mk_undefined()
                        False -> rec
                      }
                      loop(
                        state,
                        drive,
                        pc + 1,
                        [mk_bool(done), val, record, ..rest],
                        locals,
                        agent,
                        code,
                        constants,
                        r0,
                        r1,
                      )
                    }
                    // registers stay live across the step, flushed only on exits
                    plan -> {
                      let state =
                        State(
                          ..state,
                          pc:,
                          stack:,
                          agent: frames.sync(state, agent, pc),
                        )
                      case
                        iterator_next_general(state, drive, rec, rest, plan)
                      {
                        Ok(state) ->
                          loop(
                            state,
                            drive,
                            state.pc,
                            state.stack,
                            locals,
                            state.agent,
                            code,
                            constants,
                            r0,
                            r1,
                          )
                        Error(exit) ->
                          after_step(
                            Error(
                              state.map_exit(exit, fn(state) {
                                State(
                                  ..state,
                                  locals: flush_registers(state, locals, r0, r1),
                                )
                              }),
                            ),
                            drive,
                          )
                      }
                    }
                  }
              }
          }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
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
          agent: frames.sync(state, agent, pc),
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
          agent: frames.sync(state, agent, pc),
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
          agent: frames.sync(state, agent, pc),
        ),
      ))

    PushTry(catch_target: Pc(catch_target), kind:) -> {
      let frame =
        TryFrame(catch_target:, stack_depth: list.length(stack), kind:)
      loop(
        State(..state, try_stack: [frame, ..state.try_stack]),
        drive,
        pc + 1,
        stack,
        locals,
        agent,
        code,
        constants,
        r0,
        r1,
      )
    }

    PopTry ->
      case state.try_stack {
        [_, ..try_rest] ->
          loop(
            State(..state, try_stack: try_rest),
            drive,
            pc + 1,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
          )
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    CreateArguments(simple_params:) -> {
      let state =
        call.create_arguments(
          State(
            ..state,
            pc:,
            stack:,
            locals: flush_registers(state, locals, r0, r1),
            agent:,
          ),
          simple_params,
        )
      loop(
        state,
        drive,
        state.pc,
        state.stack,
        locals,
        state.agent,
        code,
        constants,
        r0,
        r1,
      )
    }

    // arity 0-2 unrolled to skip pop_n's tuple
    Call(arity) ->
      case arity, stack {
        0, [callee, ..rest] ->
          loop_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            kernel.cell_of(agent, callee),
            callee,
            kernel.literal([kernel.Undefined]),
            [],
            rest,
            None,
            kernel.literal([kernel.Undefined]),
          )
        1, [a, callee, ..rest] ->
          loop_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            kernel.cell_of(agent, callee),
            callee,
            kernel.literal([kernel.Undefined]),
            [a],
            rest,
            None,
            kernel.literal([kernel.Undefined]),
          )
        2, [b, a, callee, ..rest] ->
          loop_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            kernel.cell_of(agent, callee),
            callee,
            kernel.literal([kernel.Undefined]),
            [a, b],
            rest,
            None,
            kernel.literal([kernel.Undefined]),
          )
        _, _ ->
          case pop_n(stack, arity) {
            Some(#(args, [callee, ..rest])) ->
              loop_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                kernel.cell_of(agent, callee),
                callee,
                kernel.literal([kernel.Undefined]),
                args,
                rest,
                None,
                kernel.literal([kernel.Undefined]),
              )
            _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
          }
      }

    CallMethod(arity) ->
      case arity, stack {
        0, [method, receiver, ..rest] ->
          loop_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            kernel.cell_of(agent, method),
            method,
            receiver,
            [],
            rest,
            None,
            kernel.literal([kernel.Undefined]),
          )
        1, [a, method, receiver, ..rest] ->
          loop_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            kernel.cell_of(agent, method),
            method,
            receiver,
            [a],
            rest,
            None,
            kernel.literal([kernel.Undefined]),
          )
        2, [b, a, method, receiver, ..rest] ->
          loop_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            kernel.cell_of(agent, method),
            method,
            receiver,
            [a, b],
            rest,
            None,
            kernel.literal([kernel.Undefined]),
          )
        _, _ ->
          case pop_n(stack, arity) {
            Some(#(args, [method, receiver, ..rest])) ->
              loop_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                kernel.cell_of(agent, method),
                method,
                receiver,
                args,
                rest,
                None,
                kernel.literal([kernel.Undefined]),
              )
            _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
          }
      }

    GetFieldCall1(key.Named(_) as k, arg_idx) ->
      case stack {
        [recv, ..rest] -> {
          let method = kernel.get_field(agent, recv, k)
          let arg = case arg_idx < 0 {
            True ->
              case arg_idx {
                -1 -> r0
                _ -> r1
              }
            False -> tuple_array.element(arg_idx + 1, locals)
          }
          case kernel.is(method, kernel.Miss) || kernel.is(arg, kernel.JsTdz) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                kernel.cell_of(agent, method),
                method,
                recv,
                [arg],
                rest,
                None,
                kernel.literal([kernel.Undefined]),
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetFieldCall(key.Named(_) as k) ->
      case stack {
        [recv, ..rest] -> {
          let method = kernel.get_field(agent, recv, k)
          case kernel.is(method, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False ->
              loop_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                kernel.cell_of(agent, method),
                method,
                recv,
                [],
                rest,
                None,
                kernel.literal([kernel.Undefined]),
              )
          }
        }
        [] -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    GetLocalFieldCall(index, key.Named(_) as k) -> {
      let recv = case index < 0 {
        True ->
          case index {
            -1 -> r0
            _ -> r1
          }
        False -> tuple_array.element(index + 1, locals)
      }
      let method = kernel.get_field(agent, recv, k)
      case kernel.is(method, kernel.Miss) {
        True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
        False ->
          loop_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            kernel.cell_of(agent, method),
            method,
            recv,
            [],
            stack,
            None,
            kernel.literal([kernel.Undefined]),
          )
      }
    }

    CallNew(arity) ->
      case pop_n(stack, arity) {
        Some(#(args, [ctor, ..rest])) ->
          loop_construct(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            ctor,
            ctor,
            args,
            rest,
          )
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    ApplyArguments(slot:, ..) ->
      case stack {
        [this_arg, apply_fn, target, ..rest] ->
          case
            !rt_store.is_handle(case slot < 0 {
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
              loop_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                kernel.cell_of(agent, target),
                target,
                this_arg,
                state.call_args,
                rest,
                None,
                kernel.literal([kernel.Undefined]),
              )
            False -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
          }
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    CallConstructor(arity) ->
      case pop_n(stack, arity) {
        Some(#(args, [new_target, ctor, ..rest])) ->
          loop_construct(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            ctor,
            new_target,
            args,
            rest,
          )
        _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
      }

    Return ->
      case state.call_stack {
        [SavedCont(..) as saved, ..] -> {
          let value = case stack {
            [v, ..] -> v
            [] -> kernel.literal([kernel.Undefined])
          }
          after_step(call.cont_return(agent, state.depth, saved, value), drive)
        }
        [saved, ..] ->
          case saved.constructor_this, state.func.is_derived_constructor {
            None, True ->
              after_step(
                call.return_op(
                  State(
                    ..state,
                    pc:,
                    stack:,
                    locals: flush_registers(state, locals, r0, r1),
                    agent: frames.sync(state, agent, pc),
                  ),
                ),
                drive,
              )
            constructor_this, _ -> {
              let value = case constructor_this, stack {
                None, [v, ..] -> v
                None, [] -> kernel.literal([kernel.Undefined])
                Some(receiver), [v, ..] ->
                  case
                    !kernel.is(v, kernel.Undefined) && rt_store.is_handle(v)
                  {
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
              // call.leave_frame inlined
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
                      frames.sync(caller, agent, caller_pc),
                      saved,
                      caller_stack,
                    ))
                  enter_loop(
                    caller,
                    drive,
                    caller_pc,
                    caller.stack,
                    caller.locals,
                    caller.agent,
                    caller.func.bytecode,
                    caller.func.constants,
                  )
                }
                // enter_loop inlined
                False ->
                  case saved {
                    SavedRegFrame(r0:, r1:, ..) -> {
                      let caller_func = caller.func
                      let code = caller_func.bytecode
                      let constants = caller_func.constants
                      let _ = tuple_array.size(caller_locals)
                      let _ = tuple_array.size(code)
                      let _ = tuple_array.size(constants)
                      loop(
                        caller,
                        drive,
                        caller_pc,
                        caller_stack,
                        caller_locals,
                        agent,
                        code,
                        constants,
                        r0,
                        r1,
                      )
                    }
                    SavedFrame(..) | SavedCont(..) -> {
                      let caller_func = caller.func
                      case caller_func.regs {
                        bytecode.NoRegs -> {
                          let code = caller_func.bytecode
                          let constants = caller_func.constants
                          let _ = tuple_array.size(caller_locals)
                          let _ = tuple_array.size(code)
                          let _ = tuple_array.size(constants)
                          let u = kernel.literal([kernel.Undefined])
                          loop(
                            caller,
                            drive,
                            caller_pc,
                            caller_stack,
                            caller_locals,
                            agent,
                            code,
                            constants,
                            u,
                            u,
                          )
                        }
                        bytecode.Regs(..) ->
                          enter_loop(
                            caller,
                            drive,
                            caller_pc,
                            caller_stack,
                            caller_locals,
                            agent,
                            caller_func.bytecode,
                            caller_func.constants,
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
            [] -> kernel.literal([kernel.Undefined])
          }
          // a finished frame's locals are never read, so no flush
          Ok(#(
            Completed(NormalCompletion(value)),
            State(..state, pc:, stack:, locals:, agent:),
          ))
        }
      }

    _other -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
  }
}

// new_target undefined = plain call; otherwise entered from loop_construct
fn loop_call(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  r0: JsVal,
  r1: JsVal,
  callee_cell: types.Cell,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest: List(JsVal),
  constructor_this: Option(JsVal),
  new_target: JsVal,
) -> Result(#(Outcome, State), VmError) {
  let depth = state.depth
  case kernel.is(callee_cell, kernel.Miss) {
    False ->
      case callee_cell {
        SObject(kind: NativeFn(token:, ..), ..)
          if token != function_call
          && token != function_apply
          && token != reflect_apply
          && depth < limits.max_call_depth
        -> {
          let agent = case agent.call_depth == depth {
            False -> frames.sync_entering(state, agent, pc)
            True -> {
              let line = tuple_array.element(pc + 1, state.func.lines)
              // frames.set_top_line inlined
              let frames = case agent.frames {
                [types.FrameInfo(line: l, ..), ..] as frames if l == line ->
                  frames
                [top, ..rest] -> [types.FrameInfo(..top, line:), ..rest]
                [] -> [types.FrameInfo("", frames.stack_source, line)]
              }
              Agent(..agent, frames:, call_depth: depth + 1)
            }
          }
          case
            guard.guard4(rt_builtins.dispatch_native, agent, token, this, args)
          {
            Value(value: v, agent:) ->
              loop(
                state,
                drive,
                pc + 1,
                [v, ..rest],
                locals,
                Agent(..agent, call_depth: agent.call_depth - 1),
                code,
                constants,
                r0,
                r1,
              )
            Thrown(agent:, thrown:) ->
              after_step(
                Error(Threw(
                  thrown,
                  State(
                    ..state,
                    pc:,
                    stack: rest,
                    locals: flush_registers(state, locals, r0, r1),
                    agent: Agent(..agent, call_depth: agent.call_depth - 1),
                  ),
                )),
                drive,
              )
          }
        }
        SObject(
          kind: BytecodeFn(
            template:,
            env:,
            home_object:,
            flags:,
            realm:,
            unit_id:,
            ..,
          ),
          ..,
        ) as callee_cell ->
          case
            realm == agent.realm.id
            && depth < limits.max_call_depth
            && case kernel.is(new_target, kernel.Undefined) {
              True ->
                !template.is_class_constructor
                && !template.is_generator
                && !template.is_async
              False -> True
            }
          {
            True -> {
              let home = case home_object {
                Some(h) -> kernel.object_val([h])
                None -> kernel.literal([kernel.Undefined])
              }
              // keep in step with call.setup_frame
              let #(this_val, agent) = case
                template.is_arrow || flags.is_strict
              {
                True -> #(this, agent)
                False ->
                  case kernel.is(this, kernel.Undefined) {
                    True -> #(
                      kernel.object_val([agent.realm.global_object]),
                      agent,
                    )
                    False -> {
                      let coerced =
                        kernel.sloppy_this(this, agent.realm.global_object)
                      case kernel.is(coerced, kernel.Miss) {
                        False -> #(coerced, agent)
                        True -> rt_call.callee_this(agent, flags, this)
                      }
                    }
                  }
              }
              let callee_locals =
                kernel.frame_locals(
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
                  unit_id:,
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
                && tuple_array.element(pc + 2, code) == Return
                && kernel.is(new_target, kernel.Undefined)
                && call.is_tail_call(state, pc, template)
              {
                True -> call.elide_tail_frame(new_state)
                False -> new_state
              }
              // enter_loop inlined
              case template.regs {
                bytecode.NoRegs -> {
                  let code = template.bytecode
                  let constants = template.constants
                  let _ = tuple_array.size(callee_locals)
                  let _ = tuple_array.size(code)
                  let _ = tuple_array.size(constants)
                  let u = kernel.literal([kernel.Undefined])
                  loop(
                    new_state,
                    drive,
                    0,
                    [],
                    callee_locals,
                    new_state.agent,
                    code,
                    constants,
                    u,
                    u,
                  )
                }
                bytecode.Regs(..) ->
                  enter_loop(
                    new_state,
                    drive,
                    0,
                    [],
                    callee_locals,
                    new_state.agent,
                    template.bytecode,
                    template.constants,
                  )
              }
            }
            False ->
              case kernel.is(new_target, kernel.Undefined) {
                True ->
                  after_step(
                    call.call_cell(
                      State(
                        ..state,
                        pc:,
                        stack:,
                        locals: flush_registers(state, locals, r0, r1),
                        agent: frames.sync(state, agent, pc),
                      ),
                      kernel.to_handle_unchecked([callee]),
                      callee_cell,
                      this,
                      args,
                      rest,
                      drive,
                    ),
                    drive,
                  )
                False ->
                  via_step(state, drive, pc, stack, locals, agent, r0, r1)
              }
          }
        callee_cell ->
          after_step(
            call.call_cell(
              State(
                ..state,
                pc:,
                stack:,
                locals: flush_registers(state, locals, r0, r1),
                agent: frames.sync(state, agent, pc),
              ),
              kernel.to_handle_unchecked([callee]),
              callee_cell,
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
            locals: flush_registers(state, locals, r0, r1),
            agent: frames.sync(state, agent, pc),
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

fn loop_construct(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
  code: TupleArray(Op),
  constants: TupleArray(JsVal),
  r0: JsVal,
  r1: JsVal,
  ctor: JsVal,
  new_target: JsVal,
  args: List(JsVal),
  rest: List(JsVal),
) -> Result(#(Outcome, State), VmError) {
  case kernel.cell_of(agent, ctor) {
    SObject(kind: BytecodeFn(template:, flags:, realm:, ..), props:, ..) as callee_cell
      if flags.is_constructor
      && realm == agent.realm.id
      && state.depth < limits.max_call_depth
    ->
      case template.is_derived_constructor {
        True ->
          loop_call(
            state,
            drive,
            pc,
            stack,
            locals,
            agent,
            code,
            constants,
            r0,
            r1,
            callee_cell,
            ctor,
            kernel.literal([kernel.JsTdz]),
            args,
            rest,
            None,
            new_target,
          )
        False -> {
          let proto = case kernel.same(new_target, ctor) {
            True -> kernel.own_data(props, prototype_key)
            False -> kernel.get_field(agent, new_target, prototype_key)
          }
          let made = kernel.new_receiver(agent, proto)
          case kernel.is(made, kernel.Miss) {
            True -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
            False -> {
              let #(receiver, agent) = made
              loop_call(
                state,
                drive,
                pc,
                stack,
                locals,
                agent,
                code,
                constants,
                r0,
                r1,
                callee_cell,
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
    _ -> via_step(state, drive, pc, stack, locals, agent, r0, r1)
  }
}

fn is_intrinsic_apply(agent: Agent, v: JsVal) -> Bool {
  case kernel.cell_of(agent, v) {
    SObject(kind: NativeFn(token:, ..), ..) -> token == function_apply
    _ -> False
  }
}

fn instance_of_kernel(agent: Agent, left: JsVal, right: JsVal) -> JsVal {
  kernel.instance_of(agent, left, right, types.symbol_has_instance)
}

// the stack-only array record, spelled out with full get semantics
fn array_iter_next_general(
  state: State,
  rec: JsVal,
  rest: List(JsVal),
) -> Result(State, StepExit) {
  use <- iter_next_kernel(state, rec, rest)
  let #(target, index, next_fn) = rt_lang.array_iter_parts(rec)
  let len = case classify(target) {
    KHandle(h) ->
      case rt_store.cell_get(state.agent, h) {
        SObject(kind: types.ArrayObj(length:), ..) -> length
        _ -> 0
      }
    _ -> 0
  }
  case index >= len {
    True ->
      Ok(
        State(
          ..state,
          stack: [mk_bool(True), mk_undefined(), mk_undefined(), ..rest],
          pc: state.pc + 1,
        ),
      )
    False -> {
      use #(v, state) <- result.map(guarded3(
        state,
        rt_obj.get_prop,
        target,
        StringKey(Index(index)),
      ))
      let rec = rt_lang.array_iter_record(target, index + 1, next_fn)
      State(..state, stack: [mk_bool(False), v, rec, ..rest], pc: state.pc + 1)
    }
  }
}

// what the loop does, before the array hole path
fn iter_next_kernel(
  state: State,
  rec: JsVal,
  rest: List(JsVal),
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case rt_lang.array_iter_next(state.agent.store, rec) {
    rt_lang.IterStep(done:, value:, rec:) ->
      Ok(
        State(
          ..state,
          stack: [mk_bool(done), value, rec, ..rest],
          pc: state.pc + 1,
        ),
      )
    rt_lang.IterPair(key:, value:, rec:) -> {
      let #(pair, agent) = rt_obj.new_array(state.agent, [key, value])
      Ok(
        State(
          ..state,
          agent:,
          stack: [mk_bool(False), pair, rec, ..rest],
          pc: state.pc + 1,
        ),
      )
    }
    rt_lang.IterMiss -> otherwise()
  }
}

// gives the record real iterator objects once something may observe them
fn materialize_record(
  state: State,
  rec: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  case rt_lang.is_array_iter(rec) {
    False -> Ok(#(rec, state))
    True -> {
      let #(target, index, next_fn) = rt_lang.array_iter_parts(rec)
      let kind = case classify(target) {
        KHandle(h) ->
          case rt_store.cell_get(state.agent, h) {
            SObject(kind: types.MapObj(_), ..) ->
              types.MapIterator(target: h, index:, kind: types.MapIterEntries)
            SObject(kind: types.SetObj(_), ..) ->
              types.SetIterator(target: h, index:, kind: types.SetIterValues)
            _ ->
              types.ArrayIterator(
                target: h,
                index:,
                kind: types.ArrayIterValues,
              )
          }
        _ -> types.StringIterator(source: js_string.text(target), index:)
      }
      guarded2(
        state,
        fn(agent, _) {
          let proto = Some(rt_lang.array_iter_proto(agent, rec))
          let #(iter, agent) =
            rt_store.cell_new(
              agent,
              types.SObject(
                kind: kind,
                proto: proto,
                props: dict.new(),
                symbol_props: [],
                elements: types.NoElements,
                extensible: True,
              ),
            )
          rt_lang.alloc_record(
            agent,
            types.IteratorRecord(
              iterator: mk_object(iter),
              next_method: next_fn,
            ),
          )
        },
        Nil,
      )
    }
  }
}

// §7.4.11 only needs the objects when a return method exists
pub fn closable_record(
  state: State,
  rec: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  case rt_lang.is_array_iter(rec) {
    False -> Ok(#(rec, state))
    True -> {
      use #(ret, state) <- result.try(guarded3(
        state,
        rt_obj.get_prop,
        mk_object(rt_lang.array_iter_proto(state.agent, rec)),
        StringKey(Named("return")),
      ))
      case classify(ret) {
        KUndef | KNull -> Ok(#(mk_undefined(), state))
        _ -> materialize_record(state, rec)
      }
    }
  }
}

fn top_or_undefined(stack: List(JsVal)) -> #(JsVal, List(JsVal)) {
  case stack {
    [v, ..rest] -> #(v, rest)
    [] -> #(mk_undefined(), [])
  }
}

fn step_from_loop(
  state: State,
  drive: Drive,
  pc: Int,
  stack: List(JsVal),
  locals: TupleArray(JsVal),
  agent: Agent,
) -> Result(#(Outcome, State), VmError) {
  let state =
    State(..state, pc:, stack:, locals:, agent: frames.sync(state, agent, pc))
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
    Ok(state) ->
      enter_loop(
        state,
        drive,
        state.pc,
        state.stack,
        state.locals,
        state.agent,
        state.func.bytecode,
        state.func.constants,
      )
    exit -> after_step(exit, drive)
  }
}

fn after_step(
  stepped: Result(State, StepExit),
  drive: Drive,
) -> Result(#(Outcome, State), VmError) {
  case stepped {
    Ok(state) ->
      enter_loop(
        state,
        drive,
        state.pc,
        state.stack,
        state.locals,
        state.agent,
        state.func.bytecode,
        state.func.constants,
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
        Some(caught) -> execute(caught, drive)
        None -> Ok(#(Completed(ThrowCompletion(thrown)), post))
      }
  }
}

// walks up caller frames when this frame has no handler
pub fn unwind_to_catch(state: State, thrown: JsVal) -> Option(State) {
  case state.try_stack {
    [TryFrame(catch_target:, stack_depth:, kind: _), ..rest_try] ->
      Some(
        State(
          ..state,
          stack: [thrown, ..state.truncate_stack(state.stack, stack_depth)],
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
      case kernel.is(value, kernel.JsTdz) {
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
          case
            kernel.is(
              tuple_array.get_unchecked(index, state.locals),
              kernel.JsTdz,
            )
          {
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
      let #(box, agent) = rt_store.cell_new(state.agent, SBox(current))
      let locals =
        tuple_array.set_unchecked(index, mk_object(box), state.locals)
      Ok(State(..state, agent:, locals:, pc: state.pc + 1))
    }

    GetBoxed(index) ->
      case read_box(state, tuple_array.get_unchecked(index, state.locals)) {
        Some(value) ->
          case kernel.is(value, kernel.JsTdz) {
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
              let agent = rt_store.cell_set(state.agent, box, SBox(new_value))
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
          let local = tuple_array.get_unchecked(index, state.locals)
          case handle_of(local), read_box(state, local) {
            Some(box), Some(current) ->
              case kernel.is(current, kernel.JsTdz) {
                True -> {
                  let agent =
                    rt_store.cell_set(state.agent, box, SBox(new_value))
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
      case lexical_global(state.agent, name) {
        Some(binding) -> {
          let value = types.lexical_global_value(binding)
          case kernel.is(value, kernel.JsTdz) {
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
          case lexical_global(state.agent, name) {
            // const rejects assignment even in tdz
            Some(types.Const(_)) ->
              state.throw_type_error(state, "Assignment to constant variable.")
            Some(types.Let(current)) ->
              case kernel.is(current, kernel.JsTdz) {
                True ->
                  state.throw_reference_error(
                    state,
                    "Cannot access '" <> name <> "' before initialization",
                  )
                False ->
                  Ok(
                    State(
                      ..state,
                      agent: put_lexical_global(
                        state.agent,
                        name,
                        types.Let(value),
                      ),
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
      case lexical_global(state.agent, name) {
        Some(_) ->
          Ok(
            State(
              ..state,
              stack: [mk_bool(False), ..state.stack],
              pc: state.pc + 1,
            ),
          )
        None -> {
          use #(deleted, state) <- result.map(guarded2(
            state,
            rt_env.delete_global_var,
            name,
          ))
          State(
            ..state,
            stack: [mk_bool(deleted), ..state.stack],
            pc: state.pc + 1,
          )
        }
      }

    // §9.1.1.4.17 createglobalvarbinding, deletable only for eval
    DeclareGlobalVar(name, deletable) -> {
      use state <- result.map(guarded_unit3(
        state,
        rt_env.create_global_var_binding,
        name,
        deletable,
      ))
      State(..state, pc: state.pc + 1)
    }

    DeclareGlobalFn(name, deletable) -> {
      use state <- result.map(guarded_unit3(
        state,
        rt_env.create_global_fn_binding,
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
          let t = rt_val.type_of(state.agent, v)
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
                  agent: rt_env.eval_env_set(state.agent, env, name, v),
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
              agent: rt_env.eval_env_declare(state.agent, env, name),
              pc: state.pc + 1,
            ),
          )
      }

    ToStringVal ->
      case state.stack {
        [val, ..rest] ->
          case classify(val) {
            KStr(_) -> Ok(State(..state, pc: state.pc + 1))
            _ -> {
              use #(s, state) <- result.map(guarded2(
                state,
                rt_val.to_string,
                val,
              ))
              State(..state, stack: [mk_string(s), ..rest], pc: state.pc + 1)
            }
          }
        [] -> underflow(state, "ToStringVal")
      }

    // §13.2.8.4 gettemplateobject, cached per site
    GetTemplateObject(site, quasis) -> {
      let cooked =
        list.map(quasis, fn(q) {
          option.map(q.cooked, mk_string) |> option.unwrap(mk_undefined())
        })
      let raw = list.map(quasis, fn(q) { q.raw })
      let #(tpl, agent) =
        rt_lang.get_template_object(
          state.agent,
          int.to_string(state.unit_id) <> "#" <> int.to_string(site),
          cooked,
          raw,
        )
      Ok(State(..state, agent:, stack: [tpl, ..state.stack], pc: state.pc + 1))
    }

    // §7.1.19 topropertykey
    ToPropertyKey ->
      case state.stack {
        [val, ..rest] ->
          case classify(val) {
            KStr(_) | KSym(_) -> Ok(State(..state, pc: state.pc + 1))
            _ -> {
              use #(prim, state) <- result.try(guarded3(
                state,
                rt_val.to_primitive,
                val,
                HintString,
              ))
              case classify(prim) {
                KSym(_) ->
                  Ok(State(..state, stack: [prim, ..rest], pc: state.pc + 1))
                _ -> {
                  use #(s, state) <- result.map(guarded2(
                    state,
                    rt_val.to_string,
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

    ToObject ->
      case state.stack {
        [val, ..rest] -> {
          use #(h, state) <- result.map(guarded2(state, rt_val.to_object, val))
          State(..state, stack: [mk_object(h), ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "ToObject")
      }

    WithGetVar(name, Pc(target)) ->
      with_get_var(state, name, target, keep_this: False, op: "WithGetVar")

    WithGetVarThis(name, Pc(target)) ->
      with_get_var(state, name, target, keep_this: True, op: "WithGetVarThis")

    WithPutVar(name, Pc(target)) ->
      case state.stack {
        [obj, val, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            Some(h) -> {
              use #(bound, state) <- result.try(guarded3(
                state,
                rt_env.with_has_binding,
                h,
                name,
              ))
              case bound {
                False ->
                  Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
                True -> {
                  use state <- result.map(guarded_unit5(
                    state,
                    rt_env.with_set_mutable_binding,
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

    WithDeleteVar(name, Pc(target)) ->
      case state.stack {
        [obj, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            Some(h) -> {
              use #(bound, state) <- result.try(guarded3(
                state,
                rt_env.with_has_binding,
                h,
                name,
              ))
              case bound {
                False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
                True -> {
                  use #(deleted, state) <- result.map(guarded3(
                    state,
                    rt_env.with_delete_binding,
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

    WithMakeRef(name, Pc(target)) ->
      case state.stack {
        [obj, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            Some(h) -> {
              use #(bound, state) <- result.map(guarded3(
                state,
                rt_env.with_has_binding,
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

    WithGetRefValue(name, Pc(target)) ->
      case state.stack {
        [obj, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            Some(h) -> {
              use #(val, state) <- result.map(guarded4(
                state,
                rt_env.with_get_binding_value,
                h,
                name,
                state.func.is_strict,
              ))
              State(..state, stack: [val, ..rest], pc: target)
            }
          }
        [] -> underflow(state, "WithGetRefValue")
      }

    WithPutRefValue(name, Pc(target)) ->
      case state.stack {
        [obj, val, ..rest] ->
          case handle_of(obj) {
            None -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            Some(h) -> {
              use state <- result.map(guarded_unit5(
                state,
                rt_env.with_set_mutable_binding,
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
          agent: put_lexical_global(state.agent, name, case is_const {
            True -> types.Const(mk_tdz())
            False -> types.Let(mk_tdz())
          }),
          pc: state.pc + 1,
        ),
      )

    InitGlobalLex(name) ->
      case state.stack {
        [val, ..rest] -> {
          let binding = case lexical_global(state.agent, name) {
            Some(existing) -> types.lexical_global_with_value(existing, val)
            None -> types.Let(val)
          }
          Ok(
            State(
              ..state,
              agent: put_lexical_global(state.agent, name, binding),
              stack: rest,
              pc: state.pc + 1,
            ),
          )
        }
        [] -> underflow(state, "InitGlobalLex")
      }

    GetDisposer(is_async:) ->
      case state.stack {
        [val, ..rest] -> {
          let state = State(..state, stack: rest, pc: state.pc + 1)
          use #(disposer, state) <- result.map(guarded4(
            state,
            using.using_disposer,
            val,
            is_async,
            state.unit_id,
          ))
          State(..state, stack: [disposer, ..state.stack])
        }
        [] -> underflow(state, "GetDisposer")
      }

    MakeSuppressed ->
      case state.stack {
        [suppressed, err, ..rest] -> {
          let #(suppressed_error, agent) =
            b_error.make_suppressed(state.agent, err, suppressed)
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
          let t = rt_val.type_of(state.agent, val)
          Ok(State(..state, stack: [mk_string(t), ..rest], pc: state.pc + 1))
        }
        [] -> underflow(state, "TypeOf")
      }

    // tdz throws, undeclared is "undefined"
    TypeofGlobal(name) ->
      case lexical_global(state.agent, name) {
        Some(binding) -> {
          let value = types.lexical_global_value(binding)
          case kernel.is(value, kernel.JsTdz) {
            True ->
              state.throw_reference_error(
                state,
                "Cannot access '" <> name <> "' before initialization",
              )
            False -> {
              let t = rt_val.type_of(state.agent, value)
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
          use #(t, state) <- result.map(guarded2(
            state,
            rt_lang.global_typeof,
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

    PostIncLocal(index) -> fused_postfix_local(state, index, increment: True)
    PostDecLocal(index) -> fused_postfix_local(state, index, increment: False)

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
          let finish = fn(right, state) {
            binop_step(state, kind, left, right, rest)
          }
          use <-
            accessor_as_frame(state, receiver, k, rest, drive, Some(finish), _)
          use #(right, state) <- result.try(get_field(state, receiver, k))
          finish(right, state)
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
          use #(r, state) <- result.map(unaryop_general(state, kind, operand))
          State(..state, stack: [r, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "UnaryOp")
      }

    IncLocal(index) -> fused_update_local(state, index, increment: True)
    DecLocal(index) -> fused_update_local(state, index, increment: False)
    IncLocalJump(index, Pc(target)) -> {
      use state <- result.map(fused_update_local(state, index, increment: True))
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
      case kernel.is(right, kernel.JsTdz) {
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
      case kernel.is(left, kernel.JsTdz) || kernel.is(right, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False -> fused_cmp_jump(state, kind, left, right, target, when)
      }
    }

    CmpLocalConstJump(left_idx, const_index, kind, Pc(target), when) -> {
      let left = tuple_array.get_unchecked(left_idx, state.locals)
      case kernel.is(left, kernel.JsTdz) {
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

    Safepoint ->
      Ok(safepoint.maybe_collect_at_return(State(..state, pc: state.pc + 1)))

    Jump(Pc(target)) -> Ok(State(..state, pc: target))

    JumpIfFalse(Pc(target)) -> {
      use v <- conditional_jump(state, target)
      !rt_val.to_boolean(v)
    }

    JumpIfTrue(Pc(target)) -> conditional_jump(state, target, rt_val.to_boolean)

    JumpIfLocal(index, Pc(target), when) -> {
      let v = tuple_array.get_unchecked(index, state.locals)
      case kernel.is(v, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False ->
          case rt_val.to_boolean(v) == when {
            True -> Ok(State(..state, pc: target))
            False -> Ok(State(..state, pc: state.pc + 1))
          }
      }
    }

    JumpIfNullish(Pc(target)) ->
      conditional_jump(state, target, rt_val.is_nullish)
    JumpIfNotNullish(Pc(target)) -> {
      use v <- conditional_jump(state, target)
      !rt_val.is_nullish(v)
    }

    // quickjs op_gosub
    Gosub(Pc(target)) ->
      Ok(
        State(..state, stack: [mk_int(state.pc + 1), ..state.stack], pc: target),
      )

    // quickjs op_ret; negative retpc: value below is the return value
    Ret ->
      case state.stack {
        [ret_pc, ..rest] ->
          case classify(ret_pc), rest {
            KNum(types.JInt(n)), [v, ..below] if n < 0 ->
              Error(Returned(v, State(..state, stack: below)))
            KNum(types.JInt(n)), _ -> Ok(State(..state, stack: rest, pc: n))
            KNum(types.JFloat(f)), [v, ..below] if f <. 0.0 ->
              Error(Returned(v, State(..state, stack: below)))
            KNum(types.JFloat(f)), _ ->
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

    PopTry ->
      case state.try_stack {
        [_, ..rest] -> Ok(State(..state, try_stack: rest, pc: state.pc + 1))
        [] -> underflow(state, "PopTry: empty try_stack")
      }

    Throw ->
      case state.stack {
        [value, ..] -> Error(Threw(value, state))
        [] -> underflow(state, "Throw")
      }

    ThrowConstAssign(_name) ->
      state.throw_type_error(state, "Assignment to constant variable.")

    ThrowError(kind, msg) -> state.throw_error(state, kind, msg)

    NewObject -> {
      let #(h, agent) =
        rt_obj.new_object(state.agent, Some(state.agent.realm.object.prototype))
      Ok(
        State(
          ..state,
          agent:,
          stack: [mk_object(h), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    NewObjectWith(keys, count) -> {
      let agent = state.agent
      let #(obj, stack, store) =
        kernel.new_object(
          agent.store,
          agent.realm.object.prototype,
          keys,
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
          use <- getter_as_frame(state, receiver, k, rest, drive)
          use #(val, state) <- result.map(get_field(state, receiver, k))
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField")
      }

    GetFieldKeep(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use <- getter_as_frame(state, receiver, k, state.stack, drive)
          use #(val, state) <- result.map(get_field(state, receiver, k))
          State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetFieldKeep")
      }

    GetLocalField(index, k) -> {
      let receiver = tuple_array.get_unchecked(index, state.locals)
      case kernel.is(receiver, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False -> {
          use <- getter_as_frame(state, receiver, k, state.stack, drive)
          use #(val, state) <- result.map(get_field(state, receiver, k))
          State(..state, stack: [val, ..state.stack], pc: state.pc + 1)
        }
      }
    }

    GetFieldCall(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(method, state) <- result.try(get_field(state, receiver, k))
          call.call(state, method, receiver, [], rest, drive)
        }
        [] -> underflow(state, "GetFieldCall")
      }

    GetFieldCall1(k, arg_idx) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(method, state) <- result.try(get_field(state, receiver, k))
          let arg = tuple_array.get_unchecked(arg_idx, state.locals)
          case kernel.is(arg, kernel.JsTdz) {
            True -> tdz_reference_error(state)
            False -> call.call(state, method, receiver, [arg], rest, drive)
          }
        }
        [] -> underflow(state, "GetFieldCall1")
      }

    GetLocalFieldCall(index, k) -> {
      let receiver = tuple_array.get_unchecked(index, state.locals)
      case kernel.is(receiver, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False -> {
          use #(method, state) <- result.try(get_field(state, receiver, k))
          call.call(state, method, receiver, [], state.stack, drive)
        }
      }
    }

    GetLocalFieldKeep(index, k) -> {
      let receiver = tuple_array.get_unchecked(index, state.locals)
      case kernel.is(receiver, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False -> {
          use #(val, state) <- result.map(get_field(state, receiver, k))
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
      case kernel.is(receiver, kernel.JsTdz) || kernel.is(val, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False -> put_field_step(state, k, val, receiver, state.stack)
      }
    }

    PutLocalConstField(obj, const_index, k) -> {
      let receiver = tuple_array.get_unchecked(obj, state.locals)
      case kernel.is(receiver, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False ->
          put_field_step(
            state,
            k,
            tuple_array.get_unchecked(const_index, state.func.constants),
            receiver,
            state.stack,
          )
      }
    }

    PutField(k) ->
      case state.stack {
        [value, receiver, ..rest] -> {
          let after = [value, ..rest]
          use <- setter_as_frame(state, receiver, k, value, after, drive)
          put_field_step(state, k, value, receiver, after)
        }
        _ -> underflow(state, "PutField")
      }

    PutFieldPop(k) ->
      case state.stack {
        [value, receiver, ..rest] -> {
          use <- setter_as_frame(state, receiver, k, value, rest, drive)
          put_field_step(state, k, value, receiver, rest)
        }
        _ -> underflow(state, "PutFieldPop")
      }

    // §15.7.14 step 5/6 fresh privatename per class evaluation
    NewPrivateName(name) -> {
      let #(k, agent) = rt_class.new_private_name(state.agent, name)
      Ok(State(..state, agent:, stack: [k, ..state.stack], pc: state.pc + 1))
    }

    // §7.3.30 privateget
    GetPrivateFieldDyn ->
      case state.stack {
        [k, obj, ..rest] -> {
          use #(val, state) <- result.map(guarded3(
            state,
            rt_class.private_get,
            obj,
            k,
          ))
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetPrivateFieldDyn")
      }

    GetPrivateFieldDynKeep ->
      case state.stack {
        [k, obj, ..rest] -> {
          use #(val, state) <- result.map(guarded3(
            state,
            rt_class.private_get,
            obj,
            k,
          ))
          State(..state, stack: [val, obj, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "GetPrivateFieldDynKeep")
      }

    // §7.3.31 privateset
    PutPrivateFieldDyn ->
      case state.stack {
        [k, val, obj, ..rest] -> {
          use #(v, state) <- result.map(guarded4(
            state,
            rt_class.private_set,
            obj,
            k,
            val,
          ))
          State(..state, stack: [v, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "PutPrivateFieldDyn")
      }

    // §13.10.1 #x in obj
    PrivateInDyn ->
      case state.stack {
        [k, obj, ..rest] -> {
          use #(found, state) <- result.map(
            guard.guarded(state, fn(agent) {
              #(rt_class.private_in(agent, obj, k), agent)
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
              use state <- result.map(guarded_unit4(
                state,
                rt_class.private_field_add,
                h,
                k,
                val,
              ))
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
              use state <- result.map(guarded_unit5(
                state,
                rt_class.private_method_add,
                h,
                k,
                func,
                types.InstallMethod,
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
                opcode.Getter -> types.InstallGetter
                opcode.Setter -> types.InstallSetter
              }
              use state <- result.map(guarded_unit5(
                state,
                rt_class.private_method_add,
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
                StringKey(k),
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
              use state <- result.map(guarded_unit6(
                state,
                rt_class.define_method,
                target,
                StringKey(k),
                fn_h,
                types.InstallMethod,
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
              use #(pk, state) <- result.try(guarded2(
                state,
                rt_val.to_property_key,
                k,
              ))
              use state <- result.map(guarded_unit6(
                state,
                rt_class.define_method,
                target,
                pk,
                fn_h,
                types.InstallMethod,
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
              use state <- result.map(guarded_unit6(
                state,
                rt_class.define_method,
                target,
                StringKey(k),
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
              use #(pk, state) <- result.try(guarded2(
                state,
                rt_val.to_property_key,
                k,
              ))
              use state <- result.map(guarded_unit6(
                state,
                rt_class.define_method,
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
          case handle_of(obj), handle_of(func) {
            Some(target), Some(fn_h) ->
              Ok(
                State(
                  ..state,
                  agent: rt_class.make_method(state.agent, fn_h, target),
                  pc: state.pc + 1,
                ),
              )
            _, _ -> Ok(State(..state, pc: state.pc + 1))
          }
        _ -> underflow(state, "MakeMethod")
      }

    DefineFieldComputed ->
      case state.stack {
        [val, k, obj, ..rest] ->
          case handle_of(obj) {
            Some(h) -> {
              use #(pk, state) <- result.try(guarded2(
                state,
                rt_val.to_property_key,
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
              use #(_, state) <- result.map(guarded3(
                state,
                rt_obj.set_proto,
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
          case rt_val.is_object(obj) {
            True -> {
              use #(_, state) <- result.map(guarded3(
                state,
                rt_lang.copy_data_props,
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
                  use #(obj, state) <- result.map(guarded3(
                    state,
                    rt_lang.object_rest,
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
              use #(deleted, state) <- result.try(guarded3(
                state,
                rt_obj.delete_prop,
                h,
                StringKey(k),
              ))
              // §13.5.1.2 step 5.b.i
              case deleted, state.func.is_strict {
                False, True ->
                  state.throw_type_error(
                    state,
                    "Cannot delete property '" <> key.display_text(k) <> "'",
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
              use #(pk, state) <- result.try(guarded2(
                state,
                rt_val.to_property_key,
                k,
              ))
              use #(deleted, state) <- result.try(guarded3(
                state,
                rt_obj.delete_prop,
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
                option.map(ctor_proto, rt_class.make_method(agent, ctor_h, _))
                |> option.unwrap(agent)
              let agent =
                option.map(ctor_proto, set_cell_prototype(
                  agent,
                  _,
                  proto_parent,
                ))
                |> option.unwrap(agent)
              let agent = case handle_of(parent) {
                Some(parent_h) ->
                  set_cell_prototype(agent, ctor_h, Some(parent_h))
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
          let #(arr, agent) = rt_obj.new_array(state.agent, items)
          Ok(State(..state, agent:, stack: [arr, ..rest], pc: state.pc + 1))
        }
        None -> underflow(state, "ArrayFrom")
      }

    // emitter guarantees holes non-empty, ascending, within count
    ArrayFromWithHoles(count, holes) ->
      case pop_n(state.stack, count - list.length(holes)) {
        Some(#(values, rest)) -> {
          let items = fill_holes(values, holes, 0, count, [])
          let #(arr, agent) = rt_obj.new_array(state.agent, items)
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
      use stepped <- result.try(fused_postfix_local(
        state,
        key_idx,
        increment: True,
      ))
      case stepped.stack {
        [k, ..rest] ->
          get_elem_step(State(..stepped, pc: state.pc), receiver, k, rest)
        [] -> underflow(stepped, "GetElemPostInc")
      }
    }

    // §13.15.2 topropertykey runs once; converted key is left for putelem
    GetElemKeep ->
      case state.stack {
        [k, receiver, ..rest] ->
          case classify(receiver) {
            KUndef | KNull ->
              state.throw_type_error(
                state,
                "Cannot read properties of " <> rt_val.nullish_label(receiver),
              )
            _ -> {
              use #(pk, state) <- result.try(guarded2(
                state,
                rt_val.to_property_key,
                k,
              ))
              use #(val, state) <- result.map(guarded3(
                state,
                rt_obj.get_prop,
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
        _ -> underflow(state, "GetElemKeep")
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
              use #(items, state) <- result.map(guarded3(
                state,
                rt_lang.spread_into_list,
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
                eval.direct(
                  State(..state, stack: rest_stack),
                  args,
                  param_scope_names,
                  with_names,
                  private_names,
                  run_eval_body(_, drive),
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
          case
            rt_val.is_object(cached),
            is_intrinsic_apply(state.agent, apply_fn)
          {
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
              cell_prototype(state.agent, h)
              |> option.map(mk_object)
              |> option.unwrap(types.mk_null())
            None -> types.mk_null()
          }
          Ok(State(..state, stack: [proto, ..rest], pc: state.pc + 1))
        }
        [] -> underflow(state, "GetPrototypeOf")
      }

    GetSuperValue ->
      get_super_value(state, keep_base: False, op: "GetSuperValue")

    GetSuperValueKeep ->
      get_super_value(state, keep_base: True, op: "GetSuperValueKeep")

    PutSuperValue ->
      case state.stack {
        [val, k, base, this_val, ..rest] ->
          case handle_of(base) {
            Some(base_h) -> {
              use #(pk, state) <- result.try(guarded2(
                state,
                rt_val.to_property_key,
                k,
              ))
              use #(ok, state) <- result.try(guarded5(
                state,
                rt_obj.set_prop_with_receiver,
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
        rt_closure.new_bytecode_function(
          state.agent,
          template,
          kernel.capture_env(template.env_descriptors, state.locals),
          state.unit_id,
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
          use #(keys, state) <- result.map(guarded2(
            state,
            rt_obj.for_in_keys,
            obj,
          ))
          State(
            ..state,
            stack: [kernel.for_in_list(keys), ..rest],
            pc: state.pc + 1,
          )
        }
        _ -> underflow(state, "ForInStart")
      }

    ForInNext ->
      case state.stack {
        [iter, ..rest] ->
          case kernel.for_in_next(iter) {
            kernel.ForInKey(key:, rest: iter) ->
              Ok(
                State(
                  ..state,
                  stack: [mk_bool(False), key, iter, ..rest],
                  pc: state.pc + 1,
                ),
              )
            kernel.ForInEnd ->
              Ok(
                State(
                  ..state,
                  stack: [mk_bool(True), mk_undefined(), iter, ..rest],
                  pc: state.pc + 1,
                ),
              )
          }
        _ -> underflow(state, "ForInNext")
      }

    // §7.4.1 getiterator sync
    GetIterator ->
      case state.stack {
        [iterable, ..rest] -> {
          let rec = rt_lang.array_iter_start(state.agent, iterable)
          case kernel.is(rec, kernel.Miss) {
            False -> Ok(State(..state, stack: [rec, ..rest], pc: state.pc + 1))
            True -> {
              use #(rec, state) <- result.map(guarded3(
                state,
                rt_lang.get_iterator,
                iterable,
                rt_lang.Sync,
              ))
              State(..state, stack: [rec, ..rest], pc: state.pc + 1)
            }
          }
        }
        _ -> underflow(state, "GetIterator")
      }

    // §7.4.3 getiterator async; next is read by what follows
    GetAsyncIterator ->
      case state.stack {
        [iterable, ..rest] -> {
          use #(iterator, state) <- result.map(guarded2(
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
          case rt_val.is_object(iterator) {
            True -> {
              use #(rec, state) <- result.map(
                guard.guarded(state, fn(agent) {
                  let #(record, agent) =
                    iter_protocol.get_iterator_direct(
                      agent,
                      iterator,
                      "Iterator is not an object",
                    )
                  rt_lang.alloc_record(agent, record)
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

    // done or abrupt next: the record becomes undefined so later ops no-op
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
              use <- bool.lazy_guard(rt_lang.is_array_iter(rec), fn() {
                array_iter_next_general(state, rec, rest)
              })
              case kernel.iter_step(state.agent.store, rec) {
                kernel.ArrayAdvanced(done, val, store) -> {
                  let agent = Agent(..state.agent, store:)
                  let record = case done {
                    True -> mk_undefined()
                    False -> rec
                  }
                  Ok(
                    State(
                      ..state,
                      agent:,
                      stack: [mk_bool(done), val, record, ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                }
                plan -> iterator_next_general(state, drive, rec, rest, plan)
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
            False -> {
              use #(rec, state) <- result.try(closable_record(state, rec))
              case is_undef(rec) {
                True -> Ok(state)
                False -> guarded_unit3(state, rt_lang.iter_close, rec, False)
              }
            }
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
              case closable_record(state, rec) {
                Ok(#(rec, state)) ->
                  case is_undef(rec) {
                    True -> Error(Threw(thrown, state))
                    False ->
                      case guarded_unit3(state, rt_lang.iter_close, rec, True) {
                        Ok(state) -> Error(Threw(thrown, state))
                        Error(Threw(_, state)) -> Error(Threw(thrown, state))
                        Error(other) -> Error(other)
                      }
                  }
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
              let #(arr, agent) = rt_obj.new_array(state.agent, [])
              Ok(State(..state, agent:, stack: [arr, ..rest]))
            }
            False -> {
              use #(rec, state) <- result.try(materialize_record(state, rec))
              use #(arr, state) <- result.map(guarded2(
                state,
                rt_lang.iter_rest,
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
          case rt_val.is_object(v) {
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
        [arg, record, ..rest] -> {
          use #(record, state) <- result.try(materialize_record(state, record))
          let state = State(..state, stack: [arg, record, ..rest])
          use #(types.IteratorRecord(iterator, next_fn), state) <- result.try(
            delegate_target(state, record),
          )
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
        [arg, record, ..rest] -> {
          use #(types.IteratorRecord(iterator, next_fn), state) <- result.try(
            delegate_target(state, record),
          )
          use #(res, state) <- result.map(
            guarded4(state, rt_call.call, next_fn, iterator, [arg]),
          )
          State(..state, stack: [res, record, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "AsyncYieldStarNext")
      }

    AsyncYieldStarResume(next_pc: Pc(next_pc)) ->
      case state.stack {
        [res, _iter, ..rest] -> {
          use #(#(done, val), state) <- result.try(guarded2(
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
              use #(re, state) <- result.map(guarded3(
                state,
                b_regexp.create_literal,
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
    DynamicImport ->
      case state.stack {
        [options, specifier, ..rest] -> {
          use #(promise, state) <- result.map(guarded3(
            state,
            dynamic_import.import_call,
            specifier,
            options,
          ))
          State(..state, stack: [promise, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "DynamicImport")
      }

    DynamicImportSource ->
      case state.stack {
        [specifier, ..rest] -> {
          use #(promise, state) <- result.map(guarded2(
            state,
            dynamic_import.source_import_call,
            specifier,
          ))
          State(..state, stack: [promise, ..rest], pc: state.pc + 1)
        }
        _ -> underflow(state, "DynamicImportSource")
      }

    DynamicImportDefer ->
      case state.stack {
        [specifier, ..rest] -> {
          use #(promise, state) <- result.map(guarded2(
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

fn accessor_install_kind(kind: opcode.AccessorKind) -> types.MethodInstallKind {
  case kind {
    opcode.Getter -> types.InstallGetter
    opcode.Setter -> types.InstallSetter
  }
}

fn tdz_reference_error(state: State) -> Result(State, StepExit) {
  state.throw_reference_error(
    state,
    "Cannot access variable before initialization (TDZ)",
  )
}

fn read_box(state: State, local: JsVal) -> Option(JsVal) {
  use h <- option.then(handle_of(local))
  case rt_store.cell_get(state.agent, h) {
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
  k: key.PropertyKey,
  value: JsVal,
  receiver: JsVal,
  stack: List(JsVal),
) -> Result(State, StepExit) {
  case classify(receiver) {
    KHandle(_) -> {
      use #(ok, state) <- result.try(guarded4(
        state,
        rt_obj.set_prop,
        receiver,
        StringKey(k),
        value,
      ))
      case ok, state.func.is_strict {
        False, True ->
          state.throw_type_error(
            state,
            "Cannot assign to read only property '"
              <> key.display_text(k)
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
          <> key.display_text(k)
          <> "')",
      )
    _ ->
      case state.func.is_strict {
        True ->
          state.throw_type_error(
            state,
            "Cannot create property '"
              <> key.display_text(k)
              <> "' on primitive value",
          )
        False -> Ok(State(..state, stack:, pc: state.pc + 1))
      }
  }
}

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
          <> key.display_text(k)
          <> "')",
      )
    _ -> guarded3(state, rt_obj.get_prop, receiver, StringKey(k))
  }
}

// §9.1.1.4.4 object record half
fn global_object_get(
  state: State,
  name: String,
) -> Result(#(JsVal, State), StepExit) {
  let agent = state.agent
  let global = agent.realm.global_object
  let k = StringKey(Named(name))
  case rt_obj.ordinary_own_property(agent, global, k) {
    Some(DataProperty(value:, ..)) -> Ok(#(value, state))
    Some(AccessorProperty(..)) ->
      guarded3(state, rt_obj.get_prop, mk_object(global), k)
    None -> {
      use #(has, state) <- result.try(guarded3(
        state,
        rt_obj.has_prop,
        mk_object(global),
        k,
      ))
      case has {
        True -> guarded3(state, rt_obj.get_prop, mk_object(global), k)
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
  let k = StringKey(Named(name))
  case state.func.is_strict {
    True -> {
      use #(has, state) <- result.try(guarded3(
        state,
        rt_obj.has_prop,
        global,
        k,
      ))
      case has {
        False -> state.throw_reference_error(state, name <> " is not defined")
        True -> {
          use #(ok, state) <- result.try(guarded4(
            state,
            rt_obj.set_prop,
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
      use #(_, state) <- result.map(guarded4(
        state,
        rt_obj.set_prop,
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
          use #(bound, state) <- result.try(guarded3(
            state,
            rt_env.with_has_binding,
            h,
            name,
          ))
          case bound {
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            True -> {
              use #(val, state) <- result.map(guarded4(
                state,
                rt_env.with_get_binding_value,
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

fn pure_binop_general(
  state: State,
  op: binop.PureBinOp,
  left: JsVal,
  right: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  let cmp = fn(f) {
    use #(r, state) <- result.map(guarded3(state, f, left, right))
    #(mk_bool(r == 1), state)
  }
  case op {
    binop.Arith(binop.Sub) -> guarded3(state, rt_ops.sub, left, right)
    binop.Arith(binop.Mul) -> guarded3(state, rt_ops.mul, left, right)
    binop.Arith(binop.Div) -> guarded3(state, rt_ops.div, left, right)
    binop.Arith(binop.Mod) -> guarded3(state, rt_ops.mod, left, right)
    binop.Arith(binop.Exp) -> guarded3(state, rt_ops.pow, left, right)
    binop.Bitwise(binop.BitAnd) ->
      guarded3(state, rt_ops.bitand_general, left, right)
    binop.Bitwise(binop.BitOr) ->
      guarded3(state, rt_ops.bitor_general, left, right)
    binop.Bitwise(binop.BitXor) ->
      guarded3(state, rt_ops.bitxor_general, left, right)
    binop.Bitwise(binop.ShiftLeft) ->
      guarded3(state, rt_ops.shl_general, left, right)
    binop.Bitwise(binop.ShiftRight) ->
      guarded3(state, rt_ops.shr_general, left, right)
    binop.Bitwise(binop.ShiftRightUnsigned) ->
      guarded3(state, rt_ops.ushr_general, left, right)
    binop.Compare(binop.Less) -> cmp(rt_ops.lt_i32)
    binop.Compare(binop.LessEq) -> cmp(rt_ops.le_i32)
    binop.Compare(binop.Greater) -> cmp(rt_ops.gt_i32)
    binop.Compare(binop.GreaterEq) -> cmp(rt_ops.ge_i32)
    binop.Equality(binop.LooseEq) -> cmp(rt_ops.eq_i32_general)
    binop.Equality(binop.LooseNotEq) -> cmp(rt_ops.neq_i32)
    binop.Equality(binop.StrictEq) ->
      Ok(#(mk_bool(rt_ops.strict_eq(left, right)), state))
    binop.Equality(binop.StrictNotEq) ->
      Ok(#(mk_bool(!rt_ops.strict_eq(left, right)), state))
  }
}

fn unaryop_general(
  state: State,
  kind: opcode.UnaryOpKind,
  operand: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  case kind {
    opcode.Neg -> guarded2(state, rt_ops.neg, operand)
    opcode.Pos -> guarded2(state, rt_ops.plus, operand)
    opcode.BitNot -> guarded2(state, rt_ops.bitnot_general, operand)
    opcode.LogicalNot -> Ok(#(mk_bool(!rt_val.to_boolean(operand)), state))
    opcode.Void -> Ok(#(mk_undefined(), state))
  }
}

// must match the unfused sequence's coercions exactly
fn fused_update_local(
  state: State,
  index: Int,
  increment increment: Bool,
) -> Result(State, StepExit) {
  let next_pc = state.pc + 1
  let v = tuple_array.get_unchecked(index, state.locals)
  case kernel.is(v, kernel.JsTdz) {
    True -> tdz_reference_error(state)
    False -> {
      use #(n, state) <- result.try(guarded2(state, rt_ops.plus, v))
      let one = mk_int(1)
      use #(r, state) <- result.map(case increment {
        True -> guarded3(state, rt_ops.add, n, one)
        False -> guarded3(state, rt_ops.sub, n, one)
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
      use #(pk, state) <- result.try(guarded2(state, rt_val.to_property_key, k))
      use #(val, state) <- result.map(guarded3(
        state,
        rt_obj.get_prop,
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
      use #(pk, state) <- result.try(guarded2(state, rt_val.to_property_key, k))
      use #(ok, state) <- result.try(guarded4(
        state,
        rt_obj.set_prop,
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
  increment increment: Bool,
) -> Result(State, StepExit) {
  let next_pc = state.pc + 1
  use v <- local_or_tdz(state, index)
  use #(n, state) <- result.try(guarded2(state, rt_ops.plus, v))
  let one = mk_int(1)
  use #(r, state) <- result.map(case increment {
    True -> guarded3(state, rt_ops.add, n, one)
    False -> guarded3(state, rt_ops.sub, n, one)
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
  case kernel.is(v, kernel.JsTdz) {
    True -> tdz_reference_error(state)
    False -> k(v)
  }
}

fn binop_step(
  state: State,
  kind: binop.ClassifiedBinOp,
  left: JsVal,
  right: JsVal,
  rest: List(JsVal),
) -> Result(State, StepExit) {
  use #(r, state) <- result.map(binop_value(state, kind, left, right))
  State(..state, stack: [r, ..rest], pc: state.pc + 1)
}

fn binop_put_step(
  state: State,
  kind: binop.ClassifiedBinOp,
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
  kind: binop.ClassifiedBinOp,
  left: JsVal,
  right: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  case kind {
    binop.InstanceOfOp -> {
      use #(r, state) <- result.map(guarded3(
        state,
        rt_ops.instance_of,
        left,
        right,
      ))
      #(mk_bool(r), state)
    }
    binop.InOp ->
      case rt_val.is_object(right) {
        True -> {
          use #(r, state) <- result.map(guarded3(state, rt_ops.in, left, right))
          #(mk_bool(r), state)
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
    binop.AddOp -> guarded3(state, rt_ops.add, left, right)
    binop.PureOp(op) -> pure_binop_general(state, op, left, right)
  }
}

fn fused_cmp_jump(
  state: State,
  kind: binop.PureBinOp,
  left: JsVal,
  right: JsVal,
  target: Int,
  when when: Bool,
) -> Result(State, StepExit) {
  let next_pc = state.pc + 1
  use #(r, state) <- result.map(pure_binop_general(state, kind, left, right))
  case rt_val.to_boolean(r) == when {
    True -> State(..state, pc: target)
    False -> State(..state, pc: next_pc)
  }
}

// §7.3.7 via the real [[defineownproperty]]; false throws
fn create_data_property_or_throw(
  state: State,
  h: Handle,
  k: ObjectKey,
  val: JsVal,
) -> Result(State, StepExit) {
  use #(ok, state) <- result.try(guarded7(
    state,
    rt_obj.define_own_data,
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
    StringKey(pk) -> key.display_text(pk)
    SymbolKey(sym) -> types.symbol_descriptive_string(sym)
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
      use #(pk, state) <- result.try(guarded2(state, rt_val.to_property_key, k))
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
          use #(pp, state) <- result.try(guarded3(
            state,
            rt_obj.get_prop,
            parent,
            StringKey(Named("prototype")),
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
  case rt_obj.ordinary_own_property(agent, h, StringKey(Named("prototype"))) {
    Some(DataProperty(value:, ..)) -> handle_of(value)
    _ -> None
  }
}

fn cell_prototype(agent: Agent, h: Handle) -> Option(Handle) {
  case rt_store.cell_get(agent, h) {
    SObject(proto:, ..) | SShapedObject(proto:, ..) -> proto
    _ -> None
  }
}

// targets are fresh objects, so a direct write is safe
fn set_cell_prototype(agent: Agent, h: Handle, proto: Option(Handle)) -> Agent {
  rt_store.cell_update(agent, h, fn(cell) {
    case cell {
      SObject(..) -> SObject(..cell, proto:)
      SShapedObject(..) -> SShapedObject(..cell, proto:)
      _ -> cell
    }
  })
}

// fresh literal: extensible, writable length
fn array_push(agent: Agent, h: Handle, value: Option(JsVal)) -> Agent {
  rt_store.cell_update(agent, h, fn(cell) {
    case cell {
      SObject(kind: types.ArrayObj(length:), elements:, ..) ->
        SObject(
          ..cell,
          kind: types.ArrayObj(length: length + 1),
          elements: case value {
            Some(v) -> elements.set(elements, length, v)
            None -> elements
          },
        )
      _ -> cell
    }
  })
}

fn array_append(agent: Agent, h: Handle, items: List(JsVal)) -> Agent {
  rt_store.cell_update(agent, h, fn(cell) {
    case cell {
      SObject(kind: types.ArrayObj(length:), elements:, ..) ->
        SObject(
          ..cell,
          kind: types.ArrayObj(length: length + list.length(items)),
          elements: elements.write_list(elements, length, items),
        )
      _ -> cell
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
            types.mk_hole(),
            ..acc
          ])
        _ ->
          case values {
            [v, ..rest_values] ->
              fill_holes(rest_values, holes, index + 1, count, [v, ..acc])
            [] ->
              fill_holes([], holes, index + 1, count, [types.mk_hole(), ..acc])
          }
      }
  }
}

fn iterator_next_general(
  state: State,
  drive: Drive,
  rec: JsVal,
  rest: List(JsVal),
  plan: kernel.IterPlan,
) -> Result(State, StepExit) {
  case next_by_plan(state, drive, rec, plan) {
    Ok(#(#(done, val), state)) -> {
      let record = case done {
        True -> mk_undefined()
        False -> rec
      }
      Ok(
        State(
          ..state,
          stack: [mk_bool(done), val, record, ..rest],
          pc: state.pc + 1,
        ),
      )
    }
    Error(exit) ->
      Error(
        state.map_exit(exit, fn(state) {
          State(..state, stack: [mk_undefined(), ..rest])
        }),
      )
  }
}

fn next_by_plan(
  state: State,
  drive: Drive,
  rec: JsVal,
  plan: kernel.IterPlan,
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  case plan {
    kernel.ResumeGenerator(gen_h) ->
      gen_step(state, drive, gen_h, mk_undefined())
    kernel.ArrayAdvanced(..) | kernel.IterMiss ->
      guarded2(state, rt_lang.iter_next, rec)
  }
}

// §27.5.3.3; same-realm parked body resumes on this stack
fn gen_step(
  state: State,
  drive: Drive,
  gen_h: Handle,
  sent: JsVal,
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  let agent = state.agent
  case rt_store.cell_get(agent, gen_h) {
    types.SGenerator(
      state: types.GenSuspendedYield,
      resume: types.ResumeFrame(frame:) as resume,
    )
      | types.SGenerator(
        state: types.GenSuspendedStart,
        resume: types.ResumeFrame(frame:) as resume,
      )
      if frame.realm == agent.realm.id
      && agent.call_depth < limits.max_call_depth
    ->
      case frame.parked {
        ParkedOp ->
          resume_inline(state, drive, gen_h, resume, frame, [
            sent,
            ..frame.stack
          ])
        ParkedStart ->
          resume_inline(state, drive, gen_h, resume, frame, frame.stack)
        _ -> guarded3(state, rt_async.gen_step, gen_h, sent)
      }
    _ -> guarded3(state, rt_async.gen_step, gen_h, sent)
  }
}

fn resume_inline(
  state: State,
  drive: Drive,
  gen_h: Handle,
  resume: types.Resume,
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
      store: Store(
        ..store,
        cells: arena.set(
          gen_h.id,
          types.SGenerator(state: types.GenExecuting, resume:),
          store.cells,
        ),
      ),
      call_depth: depth + 1,
      frames: [
        frames.frame_info_at(
          frame.template,
          bytecode.line_at(frame.template, frame.pc),
        ),
        ..frames
      ],
    )
  let body = park.unpark_with(running, frame, stack)
  let completed = types.SGenerator(state: types.GenCompleted, resume:)
  case guard.guard2(run_resumed, body, drive) {
    Value(value: Ok(#(Suspended(state.Yield, v), post)), ..) -> {
      let parked = types.ResumeFrame(park.park(post, ParkedOp))
      let gen = types.SGenerator(state: types.GenSuspendedYield, resume: parked)
      Ok(#(
        #(False, v),
        State(
          ..state,
          agent: settle_generator(post.agent, gen_h, depth, frames, gen),
        ),
      ))
    }
    Value(value: Ok(#(Completed(NormalCompletion(v)), post)), ..) ->
      Ok(#(
        #(True, v),
        State(
          ..state,
          agent: settle_generator(post.agent, gen_h, depth, frames, completed),
        ),
      ))
    Value(value: Ok(#(Completed(ThrowCompletion(e)), post)), ..) ->
      Error(Threw(
        e,
        State(
          ..state,
          agent: settle_generator(post.agent, gen_h, depth, frames, completed),
        ),
      ))
    Value(value: Ok(#(Suspended(state.Await, _), post)), ..) ->
      Error(VmFailed(
        SuspensionLeak(site: "gen_step", kind: state.Await),
        State(
          ..state,
          agent: settle_generator(post.agent, gen_h, depth, frames, completed),
        ),
      ))
    Value(value: Error(err), agent:) -> {
      let #(e, state) =
        state.new_error(
          State(..state, agent:),
          TypeError,
          "internal error: " <> state.vm_error_message(err),
        )
      Error(Threw(
        e,
        State(
          ..state,
          agent: settle_generator(state.agent, gen_h, depth, frames, completed),
        ),
      ))
    }
    Thrown(agent:, thrown:) ->
      Error(Threw(
        thrown,
        State(
          ..state,
          agent: settle_generator(agent, gen_h, depth, frames, completed),
        ),
      ))
  }
}

fn settle_generator(
  agent: Agent,
  gen_h: Handle,
  depth: Int,
  frames: List(types.FrameInfo),
  cell: types.Cell,
) -> Agent {
  let store = agent.store
  Agent(
    ..agent,
    store: Store(..store, cells: arena.set(gen_h.id, cell, store.cells)),
    call_depth: depth,
    frames:,
  )
}

fn run_resumed(
  body: State,
  drive: Drive,
) -> #(Result(#(Outcome, State), VmError), Agent) {
  case execute(body, drive) {
    Ok(#(_, post)) as res -> #(res, post.agent)
    Error(err) -> #(Error(err), body.agent)
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
    Some(gen_h) -> gen_step(state, drive, gen_h, arg)
    None -> {
      use #(res, state) <- result.try(
        guarded4(state, rt_call.call, next_fn, iterator, [arg]),
      )
      guarded2(state, iter_protocol.read_iter_result, res)
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
  case rt_store.cell_get(agent, next_h), rt_store.cell_get(agent, iter_h) {
    SObject(
      kind: types.NativeFn(token: types.GeneratorN(types.GeneratorNext), ..),
      ..,
    ),
      SObject(kind: types.GeneratorObj(data: gen_h), ..)
    -> Some(gen_h)
    _, _ -> None
  }
}

// §13.15.2 re-conversion must be side-effect free
fn prop_key_value(pk: ObjectKey) -> JsVal {
  case pk {
    SymbolKey(sym) -> types.mk_symbol(sym)
    StringKey(Index(n)) -> mk_int(n)
    StringKey(other) -> mk_string(key.to_text(other))
  }
}

fn get_super_value(
  state: State,
  keep_base keep_base: Bool,
  op op: String,
) -> Result(State, StepExit) {
  case state.stack {
    [k, base, this_val, ..rest] ->
      case handle_of(base) {
        Some(base_h) -> {
          use #(pk, state) <- result.try(guarded2(
            state,
            rt_val.to_property_key,
            k,
          ))
          use #(val, state) <- result.map(guarded4(
            state,
            rt_obj.get_prop_with_receiver,
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
    rt_obj.get_prop(agent, iterable, SymbolKey(types.symbol_async_iterator))
  case classify(method) {
    KUndef | KNull -> {
      let #(sync_method, agent) =
        rt_obj.get_prop(agent, iterable, SymbolKey(types.symbol_iterator))
      case rt_val.is_callable(agent, sync_method) {
        False -> {
          let ty = rt_val.type_of(agent, iterable)
          let #(err, agent) =
            rt_val.new_error(agent, TypeError, ty <> " is not async iterable")
          rt_store.throw(agent, err)
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
      let #(iterator, agent) = rt_call.call(agent, method, iterable, [])
      case rt_val.is_object(iterator) {
        True -> #(iterator, agent)
        False -> {
          let #(err, agent) =
            rt_val.new_error(
              agent,
              TypeError,
              "Result of the Symbol.asyncIterator method is not an object",
            )
          rt_store.throw(agent, err)
        }
      }
    }
  }
}

fn delegate_target(
  state: State,
  record: JsVal,
) -> Result(#(types.IteratorRecord, State), StepExit) {
  case rt_lang.record_parts(state.agent, record) {
    Some(parts) -> Ok(#(parts, state))
    None -> {
      use #(next_method, state) <- result.map(guarded3(
        state,
        rt_obj.get_prop,
        record,
        StringKey(Named("next")),
      ))
      #(types.IteratorRecord(iterator: record, next_method:), state)
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

fn run_eval_body(
  activation: State,
  drive: Drive,
) -> #(Result(JsVal, JsVal), Agent) {
  let agent = frames.push_frame_info(activation.agent, activation.func)
  let #(res, state) =
    execute_to_completion(State(..activation, agent:), drive, "eval")
  #(res, frames.pop_frame_info(state.agent))
}

// a bytecode getter runs as an ordinary frame that returns onto rest
fn getter_as_frame(
  state: State,
  receiver: JsVal,
  k: key.PropertyKey,
  rest: List(JsVal),
  drive: Drive,
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  accessor_as_frame(state, receiver, k, rest, drive, None, otherwise)
}

// then, when given, finishes the op with the getter's result
fn accessor_as_frame(
  state: State,
  receiver: JsVal,
  k: key.PropertyKey,
  rest: List(JsVal),
  drive: Drive,
  then: Option(fn(JsVal, State) -> Result(State, StepExit)),
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case k {
    key.Named(_) ->
      case kernel.find_accessor(state.agent, receiver, k) {
        kernel.Accessor(get: Some(f), ..) ->
          call_as_frame(state, f, receiver, [], rest, drive, then, otherwise)
        _ -> otherwise()
      }
    _ -> otherwise()
  }
}

// a bytecode setter runs as a frame whose result is dropped for stack_after
fn setter_as_frame(
  state: State,
  receiver: JsVal,
  k: key.PropertyKey,
  value: JsVal,
  stack_after: List(JsVal),
  drive: Drive,
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case k {
    key.Named(_) ->
      case kernel.find_accessor(state.agent, receiver, k) {
        kernel.Accessor(set: Some(f), ..) -> {
          let finish = fn(_, state: State) {
            Ok(State(..state, stack: stack_after, pc: state.pc + 1))
          }
          call_as_frame(
            state,
            f,
            receiver,
            [value],
            stack_after,
            drive,
            Some(finish),
            otherwise,
          )
        }
        _ -> otherwise()
      }
    _ -> otherwise()
  }
}

// pushes f as a frame when it is same-realm bytecode, else otherwise
fn call_as_frame(
  state: State,
  f: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest: List(JsVal),
  drive: Drive,
  then: Option(fn(JsVal, State) -> Result(State, StepExit)),
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case kernel.cell_of(state.agent, f) {
    SObject(
      kind: BytecodeFn(
        template:,
        env:,
        home_object:,
        flags:,
        realm:,
        unit_id:,
        ..,
      ),
      ..,
    )
      if realm == state.agent.realm.id
      && !template.is_generator
      && !template.is_async
    -> {
      let assert KHandle(fn_h) = classify(f)
      call.call_function_then(
        state,
        fn_h,
        template,
        unit_id,
        env,
        home_object,
        flags,
        args,
        rest,
        this,
        None,
        mk_undefined(),
        drive,
        then,
      )
    }
    _ -> otherwise()
  }
}
