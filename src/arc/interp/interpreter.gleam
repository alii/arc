import arc/bytecode/binop
import arc/bytecode/error_kind.{TypeError}
import arc/bytecode/key.{Index, named}
import arc/bytecode/names
import arc/bytecode/opcode.{
  type Op, ApplyArguments, ArrayFrom, ArrayFromWithHoles, ArrayPush,
  ArrayPushHole, ArraySpread, AsyncYieldStarNext, AsyncYieldStarResume, Await,
  BinOp, BinOpConst, BinOpConstPut, BinOpLocal, BinOpLocalConst, BinOpLocalField,
  BinOpLocalLocal, BinOpLocalLocalPut, BinOpLocalPut, BinOpPut, BoxLocal,
  BoxLocals, Call, CallApply, CallConstructor, CallConstructorApply, CallEval,
  CallMethod, CallMethodApply, CallNew, CmpConstJump, CmpJump, CmpLocalConstJump,
  CmpLocalLocalJump, CompileSelf, CreateArguments, CreateRestArray, DecLocal,
  DeclareEvalVar, DeclareGlobalFn, DeclareGlobalLex, DeclareGlobalVar,
  DefineAccessor, DefineAccessorComputed, DefineField, DefineFieldComputed,
  DefineMethod, DefineMethodComputed, DefinePrivateAccessor, DefinePrivateField,
  DefinePrivateMethod, DeleteElem, DeleteField, DeleteGlobalVar, Dup,
  DynamicImport, DynamicImportDefer, DynamicImportSource, ForInNext, ForInStart,
  GetAsyncIterator, GetBoxed, GetDisposer, GetElem, GetElemKeep, GetElemLocals,
  GetElemPostInc, GetEnv, GetEnvBoxed, GetEvalVar, GetField, GetFieldCall,
  GetFieldCall1, GetFieldKeep, GetGlobal, GetIterator, GetLocal, GetLocalField,
  GetLocalFieldCall, GetLocalFieldKeep, GetPrivateFieldDyn,
  GetPrivateFieldDynKeep, GetPrototypeOf, GetSuperValue, GetSuperValueKeep,
  GetTemplateObject, Gosub, IncLocal, IncLocalCmpConstJump, IncLocalCmpLocalJump,
  IncLocalJump, InitGlobalLex, InitialYield, IterateView, IteratorCheckObject,
  IteratorClose, IteratorCloseThrow, IteratorNext, IteratorRecord, IteratorRest,
  Jump, JumpIfFalse, JumpIfFalseOrPop, JumpIfLocal, JumpIfNotNullish,
  JumpIfNotNullishOrPop, JumpIfNullish, JumpIfTrue, JumpIfTrueOrPop,
  JumpUndefinedIfNullish, KeepUnitText, MakeClosure, MakeLazyClosure, MakeMethod,
  MakeSuppressed, NewObject, NewObjectShaped, NewObjectWith, NewPrivateName,
  NewRegExp, ObjectRestCopy, ObjectSpread, Pop, PopTry, PostDecLocal,
  PostIncLocal, PrivateInDyn, PushConst, PushTry, PutBoxed, PutBoxedCheckInit,
  PutElem, PutElemPop, PutEnvBoxed, PutEnvBoxedCheckInit, PutEvalVar, PutField,
  PutFieldPop, PutGlobal, PutLocal, PutLocalCheckInit, PutLocalConstField,
  PutLocalLocalField, PutPrivateFieldDyn, PutSuperValue, Ret, Return, Rot3,
  Safepoint, SetProto, SetupDerivedClass, Swap, Throw, ThrowConstAssign,
  ThrowReferenceError, ToObject, ToPropertyKey, ToStringVal, TypeOf,
  TypeofEvalVar, TypeofGlobal, UnaryOp, UnpackArray, Unrot4, WithDeleteVar,
  WithGetRefValue, WithGetVar, WithGetVarThis, WithMakeRef, WithPutRefValue,
  WithPutVar, Yield, YieldStar,
}
import arc/internal/tuple_array
import arc/interp/call
import arc/interp/eval
import arc/interp/frames
import arc/interp/guard.{
  Thrown, Value, guarded2, guarded3, guarded4, guarded5, guarded7, guarded_unit3,
  guarded_unit4, guarded_unit5, guarded_unit6,
}
import arc/interp/kernel
import arc/interp/lazy
import arc/interp/park
import arc/interp/state.{
  type Outcome, type State, type StepExit, type VmError, AsyncDelegateResume,
  Completed, DelegateYield, Frame, InternalError, Returned, StackUnderflow,
  State, Suspended, SuspensionLeak, Threw, VmFailed, Yielded,
}
import arc/interp/thread
import arc/interp/using
import arc/module/dynamic_import
import arc/rt/arena
import arc/rt/async as rt_async
import arc/rt/builtins/error as b_error
import arc/rt/builtins/global_fns
import arc/rt/builtins/iter_protocol
import arc/rt/builtins/regexp as b_regexp
import arc/rt/bytecode.{type SuspendedFrame, ParkedOp, ParkedStart, TryFrame}
import arc/rt/call.{NormalCompletion, ThrowCompletion} as rt_call
import arc/rt/class as rt_class
import arc/rt/elements
import arc/rt/env as rt_env
import arc/rt/inspect as rt_inspect
import arc/rt/lang as rt_lang
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/ops as rt_ops
import arc/rt/prop_map
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type LexicalGlobal, type ObjectKey,
  AccessorProperty, Agent, BytecodeFn, DataProperty, FunctionApply, FunctionN,
  HintString, KHandle, KNull, KNum, KStr, KSym, KUndef, NativeFn, Realm, SBox,
  SObject, SShapedObject, StringKey, SymbolKey, classify, mk_bool, mk_int,
  mk_object, mk_string, mk_tdz, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

const function_apply = FunctionN(FunctionApply)

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
  prop_map.get(agent.realm.lexical_globals, key.named(name))
  |> option.from_result
}

fn put_lexical_global(
  agent: Agent,
  name: String,
  binding: LexicalGlobal,
) -> Agent {
  let realm = agent.realm
  Agent(
    ..agent,
    globals: dict.new(),
    realm: Realm(
      ..realm,
      lexical_globals: prop_map.insert(
        realm.lexical_globals,
        key.named(name),
        binding,
      ),
    ),
  )
}

pub fn execute(state: State) -> Result(#(Outcome, State), VmError) {
  thread.enter(state)
}

fn execute_to_completion(state: State) -> #(Result(JsVal, JsVal), State) {
  case execute(state) {
    Ok(#(Completed(NormalCompletion(v)), state)) -> #(Ok(v), state)
    Ok(#(Completed(ThrowCompletion(e)), state)) -> #(Error(e), state)
    Ok(#(Suspended(kind, _), state)) ->
      state.internal_fault(state, SuspensionLeak(site: "eval", kind:))
    Error(err) -> state.internal_fault(state, err)
  }
}

fn is_intrinsic_apply(agent: Agent, v: JsVal) -> Bool {
  case kernel.cell_of(agent, v) {
    SObject(kind: NativeFn(token:, ..), ..) -> token == function_apply
    _ -> False
  }
}

fn array_iter_next_general(
  state: State,
  rec: JsVal,
  rest: List(JsVal),
) -> Result(State, StepExit) {
  use #(#(done, value, rec), state) <- result.map(guarded2(
    state,
    rt_lang.for_of_next,
    rec,
  ))
  State(..state, stack: [mk_bool(done), value, rec, ..rest], pc: state.pc + 1)
}

fn materialize_record(
  state: State,
  rec: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  guarded2(state, rt_lang.materialize_record, rec)
}

pub fn closable_record(
  state: State,
  rec: JsVal,
) -> Result(#(JsVal, State), StepExit) {
  guarded2(state, rt_lang.closable_record, rec)
}

// called by name from threaded code, for a step that did not go on
pub fn after_exit(exit: StepExit) -> Result(#(Outcome, State), VmError) {
  case exit {
    Returned(value, post) -> Ok(#(Completed(NormalCompletion(value)), post))
    VmFailed(err, _) -> Error(err)
    Yielded(kind, yielded_value, post) -> {
      // must spread from post: the step may have run user code
      let parked = case kind {
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
    Threw(thrown, post) ->
      case unwind_to_catch(post, thrown) {
        Some(caught) -> execute(caught)
        None -> Ok(#(Completed(ThrowCompletion(thrown)), post))
      }
  }
}

// walks up caller frames when this frame has no handler
pub fn unwind_to_catch(state: State, thrown: JsVal) -> Option(State) {
  case state.frame.try_stack {
    [TryFrame(catch_target:, stack_depth:, kind: _), ..rest_try] ->
      Some(
        State(
          ..state,
          stack: [thrown, ..state.truncate_stack(state.stack, stack_depth)],
          frame: Frame(..state.frame, try_stack: rest_try),
          pc: catch_target,
        ),
      )
    [] -> option.then(call.unwind_frame(state), unwind_to_catch(_, thrown))
  }
}

fn underflow(state: State, op: String) -> Result(a, StepExit) {
  Error(VmFailed(StackUnderflow(op), state))
}

// called by name from threaded code, for whatever it does not run itself
pub fn step(state: State, op: Op) -> Result(State, StepExit) {
  case op {
    CompileSelf -> lazy.first_call(state)
    KeepUnitText(bytes, is_module) ->
      Ok(
        State(
          ..state,
          agent: lazy.keep_unit_text(
            state.agent,
            state.frame.unit_id,
            types.UnitText(bytes:, is_module:),
          ),
          pc: state.pc + 1,
        ),
      )

    // threaded code runs these without ever falling back here
    PushConst(_)
    | Jump(_)
    | PushTry(..)
    | NewObject
    | NewObjectWith(..)
    | NewObjectShaped(..)
    | MakeClosure(_)
    | MakeLazyClosure(..)
    | InitialYield
    | Yield
    | Await
    | CreateArguments(..)
    | Pop
    | Dup
    | Swap
    | Rot3
    | Unrot4
    | GetLocal(..)
    | PutLocal(..)
    | BoxLocal(..)
    | GetEnv(..)
    | BinOpConst(..)
    | BinOpLocal(..)
    | BinOpLocalLocal(..)
    | BinOpLocalConst(..)
    | BinOpPut(..)
    | BinOpConstPut(..)
    | BinOpLocalPut(..)
    | BinOpLocalLocalPut(..)
    | IncLocalJump(..)
    | IncLocalCmpConstJump(..)
    | IncLocalCmpLocalJump(..)
    | CmpLocalLocalJump(..)
    | CmpLocalConstJump(..)
    | CmpJump(..)
    | CmpConstJump(..)
    | Safepoint
    | JumpIfFalse(..)
    | JumpIfTrue(..)
    | JumpIfLocal(..)
    | IterateView(..)
    | UnpackArray(..)
    | JumpIfNullish(..)
    | JumpIfNotNullish(..)
    | JumpIfFalseOrPop(..)
    | JumpIfTrueOrPop(..)
    | JumpIfNotNullishOrPop(..)
    | JumpUndefinedIfNullish(..)
    | Gosub(..)
    | PopTry
    | ArrayFrom(..)
    | GetElemLocals(..)
    | Call(..)
    | CallMethod(..)
    | CallNew(..)
    | CallConstructor(..)
    | ForInNext
    | CreateRestArray(..)
    | GetFieldCall(..)
    | GetLocalFieldCall(..)
    | PutLocalLocalField(..)
    | PutLocalConstField(..)
    | Return ->
      Error(VmFailed(InternalError("step", "threaded-only op"), state))

    // §9.1.1.3.1 bindthisvalue: bound exactly once
    PutLocalCheckInit(index) ->
      case state.stack {
        [value, ..rest] ->
          case
            kernel.is(tuple_array.element(index, state.locals), kernel.JsTdz)
          {
            True -> {
              let locals = tuple_array.set_element(index, state.locals, value)
              Ok(State(..state, stack: rest, locals:, pc: state.pc + 1))
            }
            False ->
              state.throw_reference_error(
                state,
                "'this' is already initialized",
              )
          }
        [] -> underflow(state, "PutLocalCheckInit")
      }

    BoxLocals(slots, uninit) -> {
      let #(items, agent) =
        box_slots(
          tuple_array.to_list(state.locals),
          1,
          slots,
          uninit,
          state.agent,
          [],
        )
      let locals = tuple_array.from_list(items)
      Ok(State(..state, agent:, locals:, pc: state.pc + 1))
    }

    GetBoxed(index) ->
      get_boxed(state, tuple_array.element(index, state.locals))
    GetEnvBoxed(index) ->
      get_boxed(
        state,
        tuple_array.element(index, kernel.nested_captures(1, state.locals)),
      )

    PutBoxed(index) ->
      put_boxed(state, tuple_array.element(index, state.locals))
    PutEnvBoxed(index) ->
      put_boxed(
        state,
        tuple_array.element(index, kernel.nested_captures(1, state.locals)),
      )

    PutBoxedCheckInit(index) ->
      put_boxed_check_init(state, tuple_array.element(index, state.locals))
    PutEnvBoxedCheckInit(index) ->
      put_boxed_check_init(
        state,
        tuple_array.element(index, kernel.nested_captures(1, state.locals)),
      )

    // §9.1.1.4.4 getbindingvalue
    GetGlobal(k) -> {
      let name = key.to_text(k)
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
    }

    // §9.1.1.4.5 setmutablebinding
    PutGlobal(k) -> {
      let name = key.to_text(k)
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
        None -> step(state, GetGlobal(key.named(name)))
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
        None -> step(state, TypeofGlobal(key.named(name)))
      }

    PutEvalVar(name) ->
      case state.frame.eval_env, state.stack {
        Some(env), [v, ..rest] ->
          case rt_env.eval_env_has(state.agent, env, name) {
            False -> step(state, PutGlobal(key.named(name)))
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
        _, _ -> step(state, PutGlobal(key.named(name)))
      }

    // §19.2.1.3 no eval scope: global var, deletable
    DeclareEvalVar(name) ->
      case state.frame.eval_env {
        None -> step(state, DeclareGlobalVar(name, deletable: True))
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
          int.to_string(state.frame.unit_id) <> "#" <> int.to_string(site),
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

    WithGetVar(name, target) ->
      with_get_var(state, name, target, keep_this: False, op: "WithGetVar")

    WithGetVarThis(name, target) ->
      with_get_var(state, name, target, keep_this: True, op: "WithGetVarThis")

    WithPutVar(name, target) ->
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
                    state.frame.func.is_strict,
                  ))
                  State(..state, stack: rest, pc: target)
                }
              }
            }
          }
        _ -> underflow(state, "WithPutVar")
      }

    WithDeleteVar(name, target) ->
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

    WithMakeRef(name, target) ->
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

    WithGetRefValue(name, target) ->
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
                state.frame.func.is_strict,
              ))
              State(..state, stack: [val, ..rest], pc: target)
            }
          }
        [] -> underflow(state, "WithGetRefValue")
      }

    WithPutRefValue(name, target) ->
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
                state.frame.func.is_strict,
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
            state.frame.unit_id,
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
    TypeofGlobal(k) -> {
      let name = key.to_text(k)
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
    }

    BinOp(kind) ->
      case state.stack {
        [right, left, ..rest] -> binop_step(state, kind, left, right, rest)
        _ -> underflow(state, "BinOp")
      }

    PostIncLocal(index) -> fused_postfix_local(state, index, increment: True)
    PostDecLocal(index) -> fused_postfix_local(state, index, increment: False)

    BinOpLocalField(kind, index, k) ->
      case state.stack {
        [left, ..rest] -> {
          use receiver <- local_or_tdz(state, index)
          let finish = fn(right, state) {
            binop_step(state, kind, left, right, rest)
          }
          use <- accessor_as_frame(state, receiver, k, rest, Some(finish), _)
          use #(right, state) <- result.try(get_field(state, receiver, k))
          finish(right, state)
        }
        _ -> underflow(state, "BinOpLocalField")
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
    // quickjs op_ret; negative retpc: value below is the return value
    Ret ->
      case state.stack {
        [ret_pc, ..rest] ->
          case classify(ret_pc), rest {
            KNum(types.JInt(n)), [v, ..below] if n < 0 ->
              Error(Returned(v, State(..state, stack: below)))
            KNum(types.JInt(n)), _ -> Ok(State(..state, stack: rest, pc: n))
            _, _ -> underflow(state, "Ret")
          }
        [] -> underflow(state, "Ret")
      }

    Throw ->
      case state.stack {
        [value, ..] -> Error(Threw(value, state))
        [] -> underflow(state, "Throw")
      }

    ThrowConstAssign(_name) ->
      state.throw_type_error(state, "Assignment to constant variable.")

    ThrowReferenceError(msg) -> state.throw_reference_error(state, msg)

    GetField(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use <- getter_as_frame(state, receiver, k, rest)
          use #(val, state) <- result.map(get_field(state, receiver, k))
          State(..state, stack: [val, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetField")
      }

    GetFieldKeep(k) ->
      case state.stack {
        [receiver, ..rest] -> {
          use <- getter_as_frame(state, receiver, k, state.stack)
          use #(val, state) <- result.map(get_field(state, receiver, k))
          State(..state, stack: [val, receiver, ..rest], pc: state.pc + 1)
        }
        [] -> underflow(state, "GetFieldKeep")
      }

    GetLocalField(index, k) -> {
      let receiver = tuple_array.element(index, state.locals)
      case kernel.is(receiver, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False -> {
          use <- getter_as_frame(state, receiver, k, state.stack)
          use #(val, state) <- result.map(get_field(state, receiver, k))
          State(..state, stack: [val, ..state.stack], pc: state.pc + 1)
        }
      }
    }

    GetFieldCall1(k, arg_idx) ->
      case state.stack {
        [receiver, ..rest] -> {
          use #(method, state) <- result.try(get_field(state, receiver, k))
          let arg = tuple_array.element(arg_idx, state.locals)
          case kernel.is(arg, kernel.JsTdz) {
            True -> tdz_reference_error(state)
            False -> call.call(state, method, receiver, [arg], rest)
          }
        }
        [] -> underflow(state, "GetFieldCall1")
      }

    GetLocalFieldKeep(index, k) -> {
      let receiver = tuple_array.element(index, state.locals)
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

    PutField(k) ->
      case state.stack {
        [value, receiver, ..rest] -> {
          let after = [value, ..rest]
          use <- setter_as_frame(state, receiver, k, value, after)
          put_field_step(state, k, value, receiver, after)
        }
        _ -> underflow(state, "PutField")
      }

    PutFieldPop(k) ->
      case state.stack {
        [value, receiver, ..rest] -> {
          use <- setter_as_frame(state, receiver, k, value, rest)
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
              let install = accessor_install_kind(kind)
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
              case deleted, state.frame.func.is_strict {
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
              case deleted, state.frame.func.is_strict {
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
            False -> call.call(state, callee, mk_undefined(), args, rest_stack)
            True -> {
              let #(res, new_state) =
                eval.direct(
                  State(..state, stack: rest_stack),
                  args,
                  param_scope_names,
                  with_names,
                  private_names,
                  run_eval_body,
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
        _ -> underflow(state, "CallEval")
      }

    ApplyArguments(slot:, simple_params:) ->
      case state.stack {
        [this_arg, apply_fn, target, ..rest] -> {
          let cached = tuple_array.element(slot, state.locals)
          case
            rt_val.is_object(cached),
            is_intrinsic_apply(state.agent, apply_fn)
          {
            False, True ->
              call.call(state, target, this_arg, state.frame.call_args, rest)
            False, False -> {
              let #(obj, agent) = call.arguments_object(state, simple_params)
              let locals = tuple_array.set_element(slot, state.locals, obj)
              call.call(
                State(..state, agent:, locals:),
                apply_fn,
                target,
                [this_arg, obj],
                rest,
              )
            }
            True, _ ->
              call.call(state, apply_fn, target, [this_arg, cached], rest)
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
              case ok, state.frame.func.is_strict {
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

    // §7.4.1 getiterator sync
    GetIterator ->
      case state.stack {
        [iterable, ..rest] -> {
          use #(rec, state) <- result.map(guarded2(
            state,
            rt_lang.for_of_start,
            iterable,
          ))
          State(..state, stack: [rec, ..rest], pc: state.pc + 1)
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
              case kernel.iter_step(state.agent, rec) {
                kernel.Advanced(done, val, agent) -> {
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
                kernel.PairAdvanced(key, value, agent) -> {
                  let #(pair, agent) = rt_obj.new_array(agent, [key, value])
                  Ok(
                    State(
                      ..state,
                      agent:,
                      stack: [mk_bool(False), pair, rec, ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                }
                plan -> iterator_next_general(state, rec, rest, plan)
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

    AsyncYieldStarNext ->
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

    AsyncYieldStarResume(next_pc:) ->
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
        [options, specifier, ..rest] -> {
          use #(promise, state) <- result.map(guarded3(
            state,
            dynamic_import.defer_import_call,
            specifier,
            options,
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

fn box_slots(
  items: List(JsVal),
  index: Int,
  slots: List(Int),
  uninit: Bool,
  agent: Agent,
  done: List(JsVal),
) -> #(List(JsVal), Agent) {
  case slots, items {
    [], _ -> #(list.append(list.reverse(done), items), agent)
    [slot, ..later], [item, ..rest] if slot == index -> {
      let held = case uninit {
        True -> mk_tdz()
        False -> item
      }
      let #(box, agent) = rt_store.cell_new(agent, SBox(held))
      box_slots(rest, index + 1, later, uninit, agent, [mk_object(box), ..done])
    }
    [_, ..], [item, ..rest] ->
      box_slots(rest, index + 1, slots, uninit, agent, [item, ..done])
    [_, ..], [] -> panic as "BoxLocals: slot outside the frame"
  }
}

fn read_box(state: State, local: JsVal) -> Option(JsVal) {
  use h <- option.then(handle_of(local))
  case rt_store.cell_get(state.agent, h) {
    SBox(value:) -> Some(value)
    _ -> None
  }
}

fn lookup_eval_env(state: State, name: String) -> Option(JsVal) {
  option.then(state.frame.eval_env, rt_env.eval_env_lookup(state.agent, _, name))
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
      case ok, state.frame.func.is_strict {
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
      case state.frame.func.is_strict {
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
  let k = StringKey(named(name))
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
  let k = StringKey(named(name))
  case state.frame.func.is_strict {
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
                state.frame.func.is_strict,
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
  let v = tuple_array.element(index, state.locals)
  case kernel.is(v, kernel.JsTdz) {
    True -> tdz_reference_error(state)
    False -> {
      use #(n, state) <- result.try(guarded2(state, rt_ops.plus, v))
      let one = mk_int(1)
      use #(r, state) <- result.map(case increment {
        True -> guarded3(state, rt_ops.add, n, one)
        False -> guarded3(state, rt_ops.sub, n, one)
      })
      let locals = tuple_array.set_element(index, state.locals, r)
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
      use pk, agent <- guard.then2(
        state,
        state.agent,
        rt_val.to_property_key,
        k,
      )
      use val, agent <- guard.then3(state, agent, rt_obj.get_prop, receiver, pk)
      Ok(State(..state, agent:, stack: [val, ..rest], pc: state.pc + 1))
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
      let state =
        State(
          ..state,
          agent: rt_obj.devolve_dictionary(state.agent, receiver, pk),
        )
      use #(ok, state) <- result.try(guarded4(
        state,
        rt_obj.set_prop,
        receiver,
        pk,
        val,
      ))
      case ok, state.frame.func.is_strict {
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
      case state.frame.func.is_strict {
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
  let locals = tuple_array.set_element(index, state.locals, r)
  State(..state, stack: [n, ..state.stack], locals:, pc: next_pc)
}

fn local_or_tdz(
  state: State,
  index: Int,
  k: fn(JsVal) -> Result(State, StepExit),
) -> Result(State, StepExit) {
  let v = tuple_array.element(index, state.locals)
  case kernel.is(v, kernel.JsTdz) {
    True -> tdz_reference_error(state)
    False -> k(v)
  }
}

fn binop_step(
  state: State,
  kind: binop.BinOp,
  left: JsVal,
  right: JsVal,
  rest: List(JsVal),
) -> Result(State, StepExit) {
  use r, agent <- binop_value(state, kind, left, right)
  Ok(State(..state, agent:, stack: [r, ..rest], pc: state.pc + 1))
}

fn binop_value(
  state: State,
  kind: binop.BinOp,
  left: JsVal,
  right: JsVal,
  then: fn(JsVal, Agent) -> Result(State, StepExit),
) -> Result(State, StepExit) {
  let cmp = fn(f) {
    use r, agent <- guard.then3(state, state.agent, f, left, right)
    then(mk_bool(r == 1), agent)
  }
  case kind {
    binop.InstanceOf -> {
      use r, agent <- guard.then3(
        state,
        state.agent,
        rt_ops.instance_of,
        left,
        right,
      )
      then(mk_bool(r), agent)
    }
    binop.In ->
      case rt_val.is_object(right) {
        True -> {
          use r, agent <- guard.then3(
            state,
            state.agent,
            rt_ops.in,
            left,
            right,
          )
          then(mk_bool(r), agent)
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
    binop.Add -> guard.then3(state, state.agent, rt_ops.add, left, right, then)
    binop.Sub -> guard.then3(state, state.agent, rt_ops.sub, left, right, then)
    binop.Mul -> guard.then3(state, state.agent, rt_ops.mul, left, right, then)
    binop.Div -> guard.then3(state, state.agent, rt_ops.div, left, right, then)
    binop.Mod -> guard.then3(state, state.agent, rt_ops.mod, left, right, then)
    binop.Exp -> guard.then3(state, state.agent, rt_ops.pow, left, right, then)
    binop.BitAnd ->
      guard.then3(state, state.agent, rt_ops.bitand_general, left, right, then)
    binop.BitOr ->
      guard.then3(state, state.agent, rt_ops.bitor_general, left, right, then)
    binop.BitXor ->
      guard.then3(state, state.agent, rt_ops.bitxor_general, left, right, then)
    binop.ShiftLeft ->
      guard.then3(state, state.agent, rt_ops.shl_general, left, right, then)
    binop.ShiftRight ->
      guard.then3(state, state.agent, rt_ops.shr_general, left, right, then)
    binop.ShiftRightUnsigned ->
      guard.then3(state, state.agent, rt_ops.ushr_general, left, right, then)
    binop.Less -> cmp(rt_ops.lt_i32)
    binop.LessEq -> cmp(rt_ops.le_i32)
    binop.Greater -> cmp(rt_ops.gt_i32)
    binop.GreaterEq -> cmp(rt_ops.ge_i32)
    binop.LooseEq -> cmp(rt_ops.eq_i32_general)
    binop.LooseNotEq -> cmp(rt_ops.neq_i32)
    binop.StrictEq -> then(mk_bool(rt_val.strict_eq(left, right)), state.agent)
    binop.StrictNotEq ->
      then(mk_bool(!rt_val.strict_eq(left, right)), state.agent)
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
            StringKey(names.prototype),
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
  case rt_obj.ordinary_own_property(agent, h, StringKey(names.prototype)) {
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

// called by name from threaded code
pub fn iterator_next_general(
  state: State,
  rec: JsVal,
  rest: List(JsVal),
  plan: kernel.IterPlan,
) -> Result(State, StepExit) {
  case next_by_plan(state, rec, plan) {
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
  rec: JsVal,
  plan: kernel.IterPlan,
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  case plan {
    kernel.ResumeGenerator(gen_h) -> gen_step(state, gen_h, mk_undefined())
    kernel.Advanced(..) | kernel.PairAdvanced(..) | kernel.IterMiss ->
      guarded2(state, rt_lang.iter_next, rec)
  }
}

// §27.5.3.3; same-realm parked body resumes on this stack
fn gen_step(
  state: State,
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
          resume_inline(state, gen_h, resume, frame, [sent, ..frame.stack])
        ParkedStart -> resume_inline(state, gen_h, resume, frame, frame.stack)
        _ -> guarded3(state, rt_async.gen_step, gen_h, sent)
      }
    _ -> guarded3(state, rt_async.gen_step, gen_h, sent)
  }
}

fn resume_inline(
  state: State,
  gen_h: Handle,
  resume: types.Resume,
  frame: SuspendedFrame,
  stack: List(JsVal),
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  let agent = state.agent
  let depth = agent.call_depth
  let frames = agent.frames
  let running =
    Agent(
      ..agent,
      cells: arena.set(
        gen_h.id,
        types.SGenerator(state: types.GenExecuting, resume:),
        agent.cells,
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
  generator_stopped(
    state,
    gen_h,
    resume,
    depth,
    frames,
    guard.guard1(run_resumed, body),
  )
}

// what a run of a generator's body came to, back in the state that resumed it;
// also called by name from threaded code
pub fn generator_stopped(
  state: State,
  gen_h: Handle,
  resume: types.Resume,
  depth: Int,
  frames: List(types.FrameInfo),
  outcome: guard.Guarded(Result(#(Outcome, State), VmError)),
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  let completed = types.SGenerator(state: types.GenCompleted, resume:)
  case outcome {
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
  Agent(
    ..agent,
    cells: arena.set(gen_h.id, cell, agent.cells),
    call_depth: depth,
    frames:,
  )
}

fn run_resumed(body: State) -> #(Result(#(Outcome, State), VmError), Agent) {
  case execute(body) {
    Ok(#(_, post)) as res -> #(res, post.agent)
    Error(err) -> #(Error(err), body.agent)
  }
}

fn delegate_step(
  state: State,
  iterator: JsVal,
  next_fn: JsVal,
  arg: JsVal,
) -> Result(#(#(Bool, JsVal), State), StepExit) {
  case native_generator(state.agent, iterator, next_fn) {
    Some(gen_h) -> gen_step(state, gen_h, arg)
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
        StringKey(names.next),
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

fn run_eval_body(activation: State) -> #(Result(JsVal, JsVal), Agent) {
  let agent = frames.push_frame_info(activation.agent, activation.frame.func)
  let #(res, state) = execute_to_completion(State(..activation, agent:))
  #(res, frames.pop_frame_info(state.agent))
}

// a bytecode getter runs as an ordinary frame that returns onto rest
fn getter_as_frame(
  state: State,
  receiver: JsVal,
  k: key.PropertyKey,
  rest: List(JsVal),
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  accessor_as_frame(state, receiver, k, rest, None, otherwise)
}

// then, when given, finishes the op with the getter's result
fn accessor_as_frame(
  state: State,
  receiver: JsVal,
  k: key.PropertyKey,
  rest: List(JsVal),
  then: Option(fn(JsVal, State) -> Result(State, StepExit)),
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case k {
    key.Named(..) ->
      case kernel.find_accessor(state.agent, receiver, k) {
        kernel.Accessor(get: Some(f), ..) ->
          call_as_frame(state, f, receiver, [], rest, then, otherwise)
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
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case k {
    key.Named(..) ->
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
            Some(finish),
            otherwise,
          )
        }
        _ -> otherwise()
      }
    _ -> otherwise()
  }
}

// pushes f as a frame when it is same-realm bytecode, else calls otherwise
fn call_as_frame(
  state: State,
  f: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest: List(JsVal),
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
        then,
      )
    }
    _ -> otherwise()
  }
}

fn get_boxed(state: State, local: JsVal) -> Result(State, StepExit) {
  case read_box(state, local) {
    Some(value) ->
      case kernel.is(value, kernel.JsTdz) {
        True -> tdz_reference_error(state)
        False ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
      }
    None ->
      Error(VmFailed(InternalError("GetBoxed", "local is not a box"), state))
  }
}

fn put_boxed(state: State, local: JsVal) -> Result(State, StepExit) {
  case state.stack {
    [new_value, ..rest] ->
      case handle_of(local) {
        Some(box) -> {
          let agent = rt_store.cell_set(state.agent, box, SBox(new_value))
          Ok(State(..state, agent:, stack: rest, pc: state.pc + 1))
        }
        None ->
          Error(VmFailed(InternalError("PutBoxed", "local is not a box"), state))
      }
    [] -> underflow(state, "PutBoxed")
  }
}

fn put_boxed_check_init(state: State, local: JsVal) -> Result(State, StepExit) {
  case state.stack {
    [new_value, ..rest] ->
      case handle_of(local), read_box(state, local) {
        Some(box), Some(current) ->
          case kernel.is(current, kernel.JsTdz) {
            True -> {
              let agent = rt_store.cell_set(state.agent, box, SBox(new_value))
              Ok(State(..state, agent:, stack: rest, pc: state.pc + 1))
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
    [] -> underflow(state, "PutBoxedCheckInit")
  }
}
