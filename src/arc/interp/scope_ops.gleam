//// Opcode bodies for the dynamically scoped corners of the language: the
//// `with` statement's object environment (§9.1.1.2), the var scope a
//// sloppy direct eval shares with its calling function (§19.2.1.3), the
//// object half of the global environment record (§9.1.1.4), and the
//// `eval(...)` call site itself. Each takes the running `State` with its
//// operands on the stack and returns the next state or a throw; the
//// environment semantics live in `rt/env`, the evaluation machinery in
//// `interp/eval`.

import arc/interp/eval.{type Run}
import arc/interp/ffi
import arc/interp/state.{
  type State, type StepExit, StackUnderflow, State, Threw, VmFailed,
}
import arc/rt/builtins/global_fns
import arc/rt/env as rt_env
import arc/rt/types.{
  type Handle, type JsVal, KHandle, classify, mk_bool, mk_object,
}
import arc/rt/val as rt_val
import gleam/list
import gleam/option.{None, Some}
import gleam/result

fn underflow(state: State, op: String) -> Result(a, StepExit) {
  Error(VmFailed(StackUnderflow(op), state))
}

fn as_handle(v: JsVal) -> Result(Handle, Nil) {
  case classify(v) {
    KHandle(h) -> Ok(h)
    _ -> Error(Nil)
  }
}

fn has_binding(
  state: State,
  obj: Handle,
  name: String,
) -> Result(#(Bool, State), StepExit) {
  ffi.guarded(
    ffi.guard3(rt_env.t_with_has_binding, state.agent, obj, name),
    state,
  )
}

// -- `with` (§14.11) -----------------------------------------------------------

/// ToObject on the stack top, for the `with (expr)` head (§14.11.2 step 2).
pub fn to_object(state: State) -> Result(State, StepExit) {
  case state.stack {
    [val, ..rest] -> {
      use #(h, state) <- result.map(ffi.guarded(
        ffi.guard2(rt_val.t_to_object, state.agent, val),
        state,
      ))
      State(..state, stack: [mk_object(h), ..rest], pc: state.pc + 1)
    }
    [] -> underflow(state, "ToObject")
  }
}

/// WithGetVar / WithGetVarThis: HasBinding then GetBindingValue against the
/// with object on the stack top. Bound: replace it with the value (keeping
/// the object beneath as the call receiver when `keep_this`, §13.3.6.2 step
/// 1.b.ii) and jump to `target`. Unbound: pop it and fall through to the
/// next scope's check.
pub fn with_get_var(
  state: State,
  name: String,
  target: Int,
  keep_this keep_this: Bool,
) -> Result(State, StepExit) {
  case state.stack {
    [obj_v, ..rest] ->
      case as_handle(obj_v) {
        Error(Nil) -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        Ok(obj) -> {
          use #(bound, state) <- result.try(has_binding(state, obj, name))
          case bound {
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            True -> {
              use #(val, state) <- result.map(ffi.guarded(
                ffi.guard4(
                  rt_env.t_with_get_binding_value,
                  state.agent,
                  obj,
                  name,
                  state.func.is_strict,
                ),
                state,
              ))
              let below = case keep_this {
                True -> [obj_v, ..rest]
                False -> rest
              }
              State(..state, stack: [val, ..below], pc: target)
            }
          }
        }
      }
    [] ->
      underflow(state, case keep_this {
        True -> "WithGetVarThis"
        False -> "WithGetVar"
      })
  }
}

/// WithPutVar: `[obj, value, ..]`. Bound: SetMutableBinding, pop both, jump.
/// Unbound: pop the object, fall through to the static store.
pub fn with_put_var(
  state: State,
  name: String,
  target: Int,
) -> Result(State, StepExit) {
  case state.stack {
    [obj_v, val, ..rest] ->
      case as_handle(obj_v) {
        Error(Nil) -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        Ok(obj) -> {
          use #(bound, state) <- result.try(has_binding(state, obj, name))
          case bound {
            False -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
            True -> set_binding(state, obj, name, val, rest, target)
          }
        }
      }
    _ -> underflow(state, "WithPutVar")
  }
}

fn set_binding(
  state: State,
  obj: Handle,
  name: String,
  val: JsVal,
  rest: List(JsVal),
  target: Int,
) -> Result(State, StepExit) {
  use #(_, state) <- result.map(ffi.guarded(
    ffi.guard_unit5(
      rt_env.t_with_set_mutable_binding,
      state.agent,
      obj,
      name,
      val,
      state.func.is_strict,
    ),
    state,
  ))
  State(..state, stack: rest, pc: target)
}

/// WithDeleteVar: `[obj, ..]`. Bound: replace the object with the boolean
/// [[Delete]] result and jump. Unbound: pop it, fall through.
pub fn with_delete_var(
  state: State,
  name: String,
  target: Int,
) -> Result(State, StepExit) {
  case state.stack {
    [obj_v, ..rest] ->
      case as_handle(obj_v) {
        Error(Nil) -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        Ok(obj) -> {
          use #(bound, state) <- result.try(has_binding(state, obj, name))
          case bound {
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            True -> {
              use #(deleted, state) <- result.map(ffi.guarded(
                ffi.guard3(rt_env.t_with_delete_binding, state.agent, obj, name),
                state,
              ))
              State(..state, stack: [mk_bool(deleted), ..rest], pc: target)
            }
          }
        }
      }
    [] -> underflow(state, "WithDeleteVar")
  }
}

/// WithMakeRef (§9.1.2.1 GetIdentifierReference): HasBinding only. Bound:
/// KEEP the object, it is the reference base, and jump. Unbound: pop it and
/// fall through (to the next with object or the `undefined` static sentinel).
pub fn with_make_ref(
  state: State,
  name: String,
  target: Int,
) -> Result(State, StepExit) {
  case state.stack {
    [obj_v, ..rest] ->
      case as_handle(obj_v) {
        Error(Nil) -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        Ok(obj) -> {
          use #(bound, state) <- result.map(has_binding(state, obj, name))
          case bound {
            True -> State(..state, stack: [obj_v, ..rest], pc: target)
            False -> State(..state, stack: rest, pc: state.pc + 1)
          }
        }
      }
    [] -> underflow(state, "WithMakeRef")
  }
}

/// WithGetRefValue: GetBindingValue on a base WithMakeRef left. Object base:
/// read (with the still-exists re-check) and jump. `undefined` sentinel:
/// pop it and fall through to the static read.
pub fn with_get_ref_value(
  state: State,
  name: String,
  target: Int,
) -> Result(State, StepExit) {
  case state.stack {
    [base, ..rest] ->
      case as_handle(base) {
        Error(Nil) -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        Ok(obj) -> {
          use #(val, state) <- result.map(ffi.guarded(
            ffi.guard4(
              rt_env.t_with_get_binding_value,
              state.agent,
              obj,
              name,
              state.func.is_strict,
            ),
            state,
          ))
          State(..state, stack: [val, ..rest], pc: target)
        }
      }
    [] -> underflow(state, "WithGetRefValue")
  }
}

/// WithPutRefValue: `[base, value, ..]`. Object base: SetMutableBinding on
/// the ORIGINAL base (§13.15.2 note: a binding deleted while the right-hand
/// side ran is recreated there), pop both, jump. Sentinel: pop it, fall
/// through to the static store.
pub fn with_put_ref_value(
  state: State,
  name: String,
  target: Int,
) -> Result(State, StepExit) {
  case state.stack {
    [base, val, ..rest] ->
      case as_handle(base) {
        Error(Nil) -> Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        Ok(obj) -> set_binding(state, obj, name, val, rest, target)
      }
    _ -> underflow(state, "WithPutRefValue")
  }
}

// -- Global environment record, object half (§9.1.1.4) --------------------------

/// DeclareGlobalVar: CreateGlobalVarBinding(name, deletable).
pub fn declare_global_var(
  state: State,
  name: String,
  deletable: Bool,
) -> Result(State, StepExit) {
  use #(_, state) <- result.map(ffi.guarded(
    ffi.guard_unit3(
      rt_env.t_create_global_var_binding,
      state.agent,
      name,
      deletable,
    ),
    state,
  ))
  State(..state, pc: state.pc + 1)
}

/// DeleteGlobalVar, object-record half: `[[Delete]]` on the global object,
/// pushing the result. The step arm answers `false` itself for a name the
/// declarative record (let/const/class) holds, without coming here.
pub fn delete_global_var(
  state: State,
  name: String,
) -> Result(State, StepExit) {
  use #(deleted, state) <- result.map(ffi.guarded(
    ffi.guard2(rt_env.t_delete_global_var, state.agent, name),
    state,
  ))
  State(..state, stack: [mk_bool(deleted), ..state.stack], pc: state.pc + 1)
}

// -- Sloppy direct eval var scope (§19.2.1.3) -------------------------------------
// The four EvalVar opcodes consult the frame's eval env first and otherwise
// behave exactly like their Global counterparts, which the step function
// owns; `otherwise` is that arm.

fn eval_env_value(state: State, name: String) -> option.Option(JsVal) {
  option.then(state.eval_env, rt_env.eval_env_lookup(state.agent, _, name))
}

/// GetEvalVar: the eval scope's value for `name`, else GetGlobal.
pub fn get_eval_var(
  state: State,
  name: String,
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case eval_env_value(state, name) {
    Some(v) -> Ok(State(..state, stack: [v, ..state.stack], pc: state.pc + 1))
    None -> otherwise()
  }
}

/// TypeofEvalVar: `typeof` the eval scope's value for `name`, else
/// TypeofGlobal.
pub fn typeof_eval_var(
  state: State,
  name: String,
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case eval_env_value(state, name) {
    Some(v) -> {
      use #(t, state) <- result.map(ffi.guarded(
        ffi.guard2(rt_val.t_type_of, state.agent, v),
        state,
      ))
      State(
        ..state,
        stack: [types.mk_string(t), ..state.stack],
        pc: state.pc + 1,
      )
    }
    None -> otherwise()
  }
}

/// PutEvalVar: `[value, ..]`. Overwrite `name` in the eval scope if it is
/// declared there, else PutGlobal.
pub fn put_eval_var(
  state: State,
  name: String,
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case state.eval_env, state.stack {
    Some(env), [v, ..rest] ->
      case rt_env.eval_env_has(state.agent, env, name) {
        False -> otherwise()
        True -> {
          let agent = rt_env.t_eval_env_set(state.agent, env, name, v)
          Ok(State(..state, agent:, stack: rest, pc: state.pc + 1))
        }
      }
    Some(_), [] -> underflow(state, "PutEvalVar")
    None, _ -> otherwise()
  }
}

/// DeclareEvalVar: bind `name` = undefined in the eval scope unless already
/// declared. With no eval scope on the frame (the caller's variable
/// environment is the global one) it is CreateGlobalVarBinding with D = true:
/// eval-introduced globals are deletable (§19.2.1.3), unlike a script's own.
pub fn declare_eval_var(state: State, name: String) -> Result(State, StepExit) {
  case state.eval_env {
    None -> declare_global_var(state, name, True)
    Some(env) -> {
      let agent = rt_env.t_eval_env_declare(state.agent, env, name)
      Ok(State(..state, agent:, pc: state.pc + 1))
    }
  }
}

// -- eval(...) call site ---------------------------------------------------------

/// CallEval: a syntactic `eval(...)`. Stack `[arg_n, .., arg_1, callee, ..]`.
/// If the callee IS %eval% this is a direct eval (§13.3.6.1 step 6), run
/// against this frame through `interp/eval`; a throw it produces unwinds
/// from the state it hands back, which may carry a newly allocated eval
/// scope. Any other callee (eval was shadowed or rebound) is `otherwise`:
/// the plain `Call(arity)` arm over the untouched stack.
pub fn call_eval(
  state: State,
  arity: Int,
  param_scope_names: List(String),
  with_names: List(String),
  private_names: List(String),
  run: Run,
  otherwise: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  let #(rev_args, after) = list.split(state.stack, arity)
  case list.length(rev_args) == arity, after {
    True, [callee, ..rest] ->
      case global_fns.is_intrinsic_eval(state.agent, callee) {
        False -> otherwise()
        True -> {
          let #(res, state) =
            eval.direct_eval(
              State(..state, stack: rest),
              list.reverse(rev_args),
              param_scope_names,
              with_names,
              private_names,
              run,
            )
          case res {
            Ok(v) ->
              Ok(State(..state, stack: [v, ..state.stack], pc: state.pc + 1))
            Error(thrown) -> Error(Threw(thrown, state))
          }
        }
      }
    _, _ -> underflow(state, "CallEval")
  }
}
