//// [[Call]] / [[Construct]] / Return for the bytecode interpreter.
////
//// A bytecode callee runs FLAT: the caller's registers are parked in a
//// `SavedFrame`, the callee's locals tuple is built and the loop carries on
//// in the same BEAM frame; `Return` pops the frame back. Bound functions
//// whose target is bytecode and `Function.prototype.call/apply` /
//// `Reflect.apply` are unwrapped here so they stay flat too. Every other
//// callee (native, compiled, proxy) is ONE nested runtime call through
//// `rt/call.t_call` / `t_construct`, which re-enters the interpreter via
//// `JsOps.call_bytecode` if it calls back into bytecode.
////
//// Depth: a flat frame push bumps `Agent.call_depth` and pops it on
//// Return/unwind, the same counter `t_enter_call` bumps for nested calls,
//// so `limits.max_call_depth` bounds both. `Agent.frames` is pushed and
//// popped in step so `Error.stack` names the live bytecode frames.

import arc/bytecode/lexical
import arc/bytecode/opcode
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/ffi
import arc/interp/safepoint
import arc/interp/state.{
  type SavedFrame, type State, type StepExit, Returned, SavedFrame, State, Threw,
}
import arc/rt/builtins as rt_builtins
import arc/rt/builtins/function as b_function
import arc/rt/bytecode.{type EnvTuple, type FuncTemplate}
import arc/rt/call as rt_call
import arc/rt/elements as rt_elements
import arc/rt/inspect as rt_inspect
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type FnFlags, type Handle, type JsSlot, type JsVal,
  type NativeToken, Agent, ArgumentsObj, ArrayObj, FrameInfo, FunctionApply,
  FunctionCall, FunctionN, KBound, KBytecode, KCompiled, KHandle, KNative, KNull,
  KTdz, KUndef, ProxyObj, ReflectApply, ReflectN, SBox, SObject, classify,
  mk_object, mk_tdz, mk_undefined,
}
import gleam/bool
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// -- Raise adapter -------------------------------------------------------------

/// Run a raise-capable runtime body against this state's agent, folding a
/// JS throw into `Error(Threw(e, state'))` where `state'` has adopted the
/// agent the throw carried. For composite slow-path bodies; single runtime
/// calls use `ffi.guardN` with the function reference directly.
pub fn guarded(
  state: State,
  body: fn(Agent) -> #(a, Agent),
) -> Result(#(a, State), StepExit) {
  ffi.guarded(ffi.guard1(body, state.agent), state)
}

/// `guarded` for a body that returns the bare agent.
pub fn guarded_unit(
  state: State,
  body: fn(Agent) -> Agent,
) -> Result(State, StepExit) {
  use #(_, state) <- result.map(ffi.guarded(
    ffi.guard_unit1(body, state.agent),
    state,
  ))
  state
}

// -- Agent.frames / call depth ------------------------------------------

/// Script label in `Error.stack` frames.
const stack_source = "script"

fn template_frame(template: FuncTemplate) -> types.FrameInfo {
  frame_info_at(template, 0)
}

/// The `Error.stack` frame of an activation of `template` at `line`.
pub fn frame_info_at(template: FuncTemplate, line: Int) -> types.FrameInfo {
  FrameInfo(name: option.unwrap(template.name, ""), script: stack_source, line:)
}

/// Push the `Error.stack` frame for an activation of `template` without
/// touching the depth counter: for root activations, whose depth the
/// enclosing `t_call` (or the script driver) already accounts for.
pub fn push_frame_info(agent: Agent, template: FuncTemplate) -> Agent {
  Agent(..agent, frames: [template_frame(template), ..agent.frames])
}

/// Pop the innermost `Error.stack` frame (root activation exit).
pub fn pop_frame_info(agent: Agent) -> Agent {
  case agent.frames {
    [_, ..rest] -> Agent(..agent, frames: rest)
    [] -> agent
  }
}

/// Record the source line the innermost frame is executing (SetLine).
pub fn set_line(agent: Agent, line: Int) -> Agent {
  case agent.frames {
    [FrameInfo(line: l, ..), ..] if l == line -> agent
    [top, ..rest] -> Agent(..agent, frames: [FrameInfo(..top, line:), ..rest])
    [] -> Agent(..agent, frames: [FrameInfo("", stack_source, line)])
  }
}

/// The line the innermost frame last recorded, 0 if none.
pub fn current_line(agent: Agent) -> Int {
  case agent.frames {
    [FrameInfo(line:, ..), ..] -> line
    [] -> 0
  }
}

/// Enter a flat bytecode frame: `++call_depth` and push its stack frame.
/// `Error(Nil)` at `limits.max_call_depth` (the same limit
/// `rt/store.t_enter_call` enforces for nested calls); the caller throws the
/// RangeError in its own frame.
fn enter_frame(agent: Agent, template: FuncTemplate) -> Result(Agent, Nil) {
  let depth = agent.call_depth
  case depth >= limits.max_call_depth {
    True -> Error(Nil)
    False -> {
      let name = case template.name {
        Some(name) -> name
        None -> ""
      }
      Ok(
        Agent(..agent, call_depth: depth + 1, frames: [
          FrameInfo(name:, script: stack_source, line: 0),
          ..agent.frames
        ]),
      )
    }
  }
}

/// Leave a flat bytecode frame: `--call_depth` and pop its stack frame.
fn leave_frame(agent: Agent) -> Agent {
  let frames = case agent.frames {
    [_, ..rest] -> rest
    [] -> []
  }
  Agent(..agent, call_depth: agent.call_depth - 1, frames:)
}

// -- Coroutine hand-off ---------------------------------------------------

/// A generator / async function / async generator call whose frame is
/// already laid out (`this` bound, locals seeded). The coroutine driver
/// turns it into a generator object or a promise and pushes that.
pub type CoroutineCall {
  CoroutineCall(
    fn_h: Handle,
    template: FuncTemplate,
    /// The closure's parse id, the body activation's `unit`.
    unit: Int,
    locals: TupleArray(JsVal),
    this: JsVal,
    home_object: JsVal,
    args: List(JsVal),
    rest_stack: List(JsVal),
  )
}

/// Callbacks into the parts of the interpreter this module cannot import.
pub type Drive {
  Drive(start_coroutine: fn(State, CoroutineCall) -> Result(State, StepExit))
}

// -- Frame setup (§10.2.1.1 PrepareForOrdinaryCall) --------------------------

fn home_value(home_object: Option(Handle)) -> JsVal {
  case home_object {
    Some(h) -> mk_object(h)
    None -> mk_undefined()
  }
}

/// Build the callee's locals tuple: bind `this` per §10.2.1.2
/// OrdinaryCallBindThis (`rt/call.resolve_this`), then lay out
/// `[env.., lexical seeds.., args (padded to arity).., undefined..]`.
/// Arrows own no lexical slots and bind nothing. Returns the bound `this`.
fn setup_frame(
  agent: Agent,
  env: EnvTuple,
  fn_h: Handle,
  home: JsVal,
  template: FuncTemplate,
  flags: FnFlags,
  args: List(JsVal),
  this_arg: JsVal,
  new_target: JsVal,
) -> #(TupleArray(JsVal), JsVal, Agent) {
  case template.is_arrow {
    True -> #(
      ffi.setup_locals_tuple(
        env,
        [],
        args,
        template.arity,
        template.local_count,
        mk_undefined(),
      ),
      this_arg,
      agent,
    )
    False -> {
      // Strict [[ThisMode]] passes `this` through uncoerced (and a derived
      // constructor enters with it in TDZ: nothing to bind either way).
      let #(this_val, agent) = case flags.is_strict {
        True -> #(this_arg, agent)
        False ->
          case classify(this_arg) {
            KTdz -> #(this_arg, agent)
            _ -> rt_call.resolve_this(agent, flags, this_arg)
          }
      }
      #(
        ffi.setup_locals_seeded(
          env,
          template.lexical,
          this_val,
          mk_object(fn_h),
          home,
          new_target,
          args,
          template.arity,
          template.local_count,
          mk_undefined(),
        ),
        this_val,
        agent,
      )
    }
  }
}

// -- Flat bytecode call -------------------------------------------------------

/// Enter a bytecode function in-loop. Shared by Call/CallMethod (plain
/// [[Call]]: `constructor_this` None, `new_target` undefined) and the
/// bytecode arm of [[Construct]]. Class constructors refuse a plain call
/// (§10.2.1 step 2); coroutine bodies are handed to `drive`; everything else
/// parks the caller in a `SavedFrame` and switches registers to the callee.
pub fn call_function(
  state: State,
  fn_h: Handle,
  template: FuncTemplate,
  unit: Int,
  env: EnvTuple,
  home_object: Option(Handle),
  flags: FnFlags,
  args: List(JsVal),
  rest_stack: List(JsVal),
  this_arg: JsVal,
  constructor_this: Option(JsVal),
  new_target: JsVal,
  drive: Drive,
) -> Result(State, StepExit) {
  let plain_call = case classify(new_target) {
    KUndef -> True
    _ -> False
  }
  case template.is_class_constructor && plain_call {
    True ->
      state.throw_type_error(
        State(..state, stack: rest_stack),
        "Class constructor "
          <> option.unwrap(template.name, "")
          <> " cannot be invoked without 'new'",
      )
    False -> {
      let home = home_value(home_object)
      let #(locals, this_val, agent) =
        setup_frame(
          state.agent,
          env,
          fn_h,
          home,
          template,
          flags,
          args,
          this_arg,
          new_target,
        )
      let state = State(..state, agent:)
      case template.is_generator || template.is_async {
        True ->
          drive.start_coroutine(
            state,
            CoroutineCall(
              fn_h:,
              template:,
              unit:,
              locals:,
              this: this_val,
              home_object: home,
              args:,
              rest_stack:,
            ),
          )
        False ->
          call_regular_function(
            state,
            template,
            unit,
            args,
            rest_stack,
            locals,
            this_val,
            home,
            constructor_this,
            new_target,
          )
      }
    }
  }
}

fn call_regular_function(
  state: State,
  template: FuncTemplate,
  unit: Int,
  args: List(JsVal),
  rest_stack: List(JsVal),
  locals: TupleArray(JsVal),
  this_val: JsVal,
  home: JsVal,
  constructor_this: Option(JsVal),
  new_target: JsVal,
) -> Result(State, StepExit) {
  case enter_frame(state.agent, template) {
    Error(Nil) ->
      state.throw_range_error(
        State(..state, stack: rest_stack),
        "Maximum call stack size exceeded",
      )
    Ok(agent) -> {
      let saved =
        SavedFrame(
          func: state.func,
          unit: state.unit,
          locals: state.locals,
          stack: rest_stack,
          pc: state.pc + 1,
          try_stack: state.try_stack,
          constructor_this:,
          this: state.this,
          new_target: state.new_target,
          home_object: state.home_object,
          call_args: state.call_args,
          eval_env: state.eval_env,
        )
      Ok(State(
        agent:,
        stack: [],
        locals:,
        func: template,
        unit:,
        code: template.bytecode,
        constants: template.constants,
        pc: 0,
        call_stack: [saved, ..state.call_stack],
        outer_depth: state.outer_depth,
        try_stack: [],
        this: this_val,
        new_target:,
        home_object: home,
        call_args: args,
        eval_env: None,
      ))
    }
  }
}

/// Plain [[Call]] of a same-realm bytecode function that is neither a class
/// constructor nor a coroutine, straight from the loop's registers (`pc`,
/// `locals`, `agent`; `state` carries the rest of the caller's frame). The
/// callee frame is built without materialising the caller's State first.
pub fn call_plain(
  state: State,
  pc: Int,
  locals: TupleArray(JsVal),
  agent: Agent,
  fn_h: Handle,
  template: FuncTemplate,
  unit: Int,
  env: EnvTuple,
  home_object: Option(Handle),
  flags: FnFlags,
  args: List(JsVal),
  rest_stack: List(JsVal),
  this_arg: JsVal,
) -> Result(State, StepExit) {
  let undefined = mk_undefined()
  let home = case home_object {
    Some(h) -> mk_object(h)
    None -> undefined
  }
  let #(callee_locals, this_val, agent) =
    setup_frame(
      agent,
      env,
      fn_h,
      home,
      template,
      flags,
      args,
      this_arg,
      undefined,
    )
  case enter_frame(agent, template) {
    Error(Nil) ->
      state.throw_range_error(
        State(..state, pc:, stack: rest_stack, locals:, agent:),
        "Maximum call stack size exceeded",
      )
    Ok(callee_agent) -> {
      let saved =
        SavedFrame(
          func: state.func,
          unit: state.unit,
          locals:,
          stack: rest_stack,
          pc: pc + 1,
          try_stack: state.try_stack,
          constructor_this: None,
          this: state.this,
          new_target: state.new_target,
          home_object: state.home_object,
          call_args: state.call_args,
          eval_env: state.eval_env,
        )
      let callee =
        Ok(State(
          agent: callee_agent,
          stack: [],
          locals: callee_locals,
          func: template,
          unit:,
          code: template.bytecode,
          constants: template.constants,
          pc: 0,
          call_stack: [saved, ..state.call_stack],
          outer_depth: state.outer_depth,
          try_stack: [],
          this: this_val,
          new_target: undefined,
          home_object: home,
          call_args: args,
          eval_env: None,
        ))
      case is_tail_call(state, pc, template) {
        True -> elide_tail_frame(callee)
        False -> callee
      }
    }
  }
}

// -- §15.10 tail calls --------------------------------------------------------

/// §15.10 IsInTailPosition for a Call/CallMethod: the next opcode is Return,
/// the caller is strict, no try handler is active in the caller, the caller
/// is a plain [[Call]] frame (constructor frames need Return's fixups) and
/// there IS a caller frame to return to (coroutine bodies run at
/// call_stack == [] and never elide, §15.10.1 steps 5-7).
fn is_tail_call(state: State, pc: Int, callee: FuncTemplate) -> Bool {
  let frame_eligible = case state.try_stack, state.call_stack {
    [], [_, ..] ->
      state.func.is_strict
      && !callee.is_generator
      && !callee.is_async
      && is_undefined(state.new_target)
    _, _ -> False
  }
  case frame_eligible {
    False -> False
    True ->
      case tuple_array.get_unchecked(pc + 1, state.code) {
        opcode.Return -> True
        _ -> False
      }
  }
}

/// §15.10.3 PrepareForTailCall: `call_regular_function` has just parked the
/// caller; discard that frame (and its depth/stack-frame entry) so the
/// callee returns straight to the caller's caller.
fn elide_tail_frame(res: Result(State, StepExit)) -> Result(State, StepExit) {
  use new_state <- result.map(res)
  case new_state.call_stack {
    [_caller, ..rest_frames] -> {
      let agent = new_state.agent
      let frames = case agent.frames {
        [callee, _caller, ..rest] -> [callee, ..rest]
        other -> other
      }
      State(
        ..new_state,
        call_stack: rest_frames,
        agent: Agent(..agent, frames:, call_depth: agent.call_depth - 1),
      )
    }
    [] -> new_state
  }
}

// -- [[Call]] dispatch (Call / CallMethod / CallApply / CallMethodApply) ------

@external(erlang, "arc_interp_ffi", "is_undefined")
fn is_undefined(v: JsVal) -> Bool

/// Call `callee` with `this` and `args`; the result lands on `rest_stack`.
/// Bytecode callees, bound chains and call/apply/Reflect.apply stay in the
/// loop; anything else is one nested `rt/call.t_call`.
pub fn call(
  state: State,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest_stack: List(JsVal),
  drive: Drive,
) -> Result(State, StepExit) {
  case classify(callee) {
    KHandle(h) ->
      call_cell(
        state,
        h,
        rt_store.t_cell_get(state.agent, h),
        this,
        args,
        rest_stack,
        drive,
      )
    _ -> not_a_function(state, callee)
  }
}

/// `call` for a callee handle whose cell the caller has already read.
pub fn call_cell(
  state: State,
  h: Handle,
  slot: JsSlot,
  this: JsVal,
  args: List(JsVal),
  rest_stack: List(JsVal),
  drive: Drive,
) -> Result(State, StepExit) {
  case slot {
    SObject(
      kind: KBytecode(template:, env:, home_object:, flags:, realm:, unit:, ..),
      ..,
    )
      if realm == state.agent.realm.id
    -> {
      let res =
        call_function(
          state,
          h,
          template,
          unit,
          env,
          home_object,
          flags,
          args,
          rest_stack,
          this,
          None,
          mk_undefined(),
          drive,
        )
      case is_tail_call(state, state.pc, template) {
        True -> elide_tail_frame(res)
        False -> res
      }
    }
    // §10.4.1.1: [[BoundThis]] replaces `this`; bound args prepend.
    SObject(kind: KBound(target:, bound_this:, bound_args:), ..) ->
      call(
        state,
        mk_object(target),
        bound_this,
        list.append(bound_args, args),
        rest_stack,
        drive,
      )
    // §20.2.3.3 Function.prototype.call(thisArg, ...args): `this` is
    // the target function.
    SObject(kind: KNative(tag: FunctionN(FunctionCall), ..), ..) -> {
      let #(this_arg, call_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(mk_undefined(), [])
      }
      call(state, this, this_arg, call_args, rest_stack, drive)
    }
    // §20.2.3.1 Function.prototype.apply(thisArg, argArray).
    SObject(kind: KNative(tag: FunctionN(FunctionApply), ..), ..) -> {
      let #(this_arg, arg_array) = case args {
        [t, a, ..] -> #(t, a)
        [t] -> #(t, mk_undefined())
        [] -> #(mk_undefined(), mk_undefined())
      }
      // Step 1: If IsCallable(func) is false, throw a TypeError.
      use <- require_callable(state, this)
      // Step 3: undefined/null argArray → no args. Step 4:
      // ? CreateListFromArrayLike(argArray).
      use #(call_args, state) <- result.try(case classify(arg_array) {
        KUndef | KNull -> Ok(#([], state))
        _ ->
          guarded(State(..state, stack: rest_stack), fn(agent) {
            b_function.create_list_from_array_like(agent, arg_array)
          })
      })
      call(state, this, this_arg, call_args, rest_stack, drive)
    }
    // §28.1.1 Reflect.apply(target, thisArgument, argumentsList).
    SObject(kind: KNative(tag: ReflectN(ReflectApply), ..), ..) -> {
      let #(target, this_arg, args_list) = case args {
        [t, a, l, ..] -> #(t, a, l)
        [t, a] -> #(t, a, mk_undefined())
        [t] -> #(t, mk_undefined(), mk_undefined())
        [] -> #(mk_undefined(), mk_undefined(), mk_undefined())
      }
      use <- require_callable(state, target)
      use #(call_args, state) <- result.try(
        guarded(State(..state, stack: rest_stack), fn(agent) {
          b_function.create_list_from_array_like(agent, args_list)
        }),
      )
      call(state, target, this_arg, call_args, rest_stack, drive)
    }
    // A closure from another realm runs with that realm current
    // (§10.2.1.1 PrepareForOrdinaryCall step 5): one nested root
    // activation, which `entry.run_root` enters the realm around.
    SObject(kind: KNative(tag:, ..), ..) ->
      call_native(state, tag, mk_object(h), this, args, rest_stack)
    SObject(kind: KBytecode(..), ..)
    | SObject(kind: KCompiled(..), ..)
    | SObject(kind: ProxyObj(..), ..) ->
      call_nested(state, mk_object(h), this, args, rest_stack)
    _ -> not_a_function(state, mk_object(h))
  }
}

/// A native method straight from the loop: the depth bracket `t_call` would
/// take, around one guarded `dispatch_native`. At the depth limit the
/// nested `t_call` raises the RangeError.
fn call_native(
  state: State,
  tag: NativeToken,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest_stack: List(JsVal),
) -> Result(State, StepExit) {
  case state.agent.call_depth >= limits.max_call_depth {
    True -> call_nested(state, callee, this, args, rest_stack)
    False -> {
      let agent = rt_store.t_enter_call(state.agent)
      case ffi.guard4(rt_builtins.dispatch_native, agent, tag, this, args) {
        ffi.Ok(value: v, agent:) ->
          Ok(
            State(
              ..state,
              agent: rt_store.t_leave_call(agent),
              stack: [v, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        ffi.Threw(agent:, thrown:) ->
          Error(Threw(
            thrown,
            State(
              ..state,
              agent: rt_store.t_leave_call(agent),
              stack: rest_stack,
            ),
          ))
      }
    }
  }
}

fn require_callable(
  state: State,
  v: JsVal,
  k: fn() -> Result(State, StepExit),
) -> Result(State, StepExit) {
  case rt_call.is_callable(state.agent, v) {
    True -> k()
    False -> not_a_function(state, v)
  }
}

/// The not-callable TypeError. The operand stack is left as-is:
/// `unwind_to_catch` truncates to the handler's recorded depth, and no
/// handler records a depth above a call's operands.
fn not_a_function(state: State, callee: JsVal) -> Result(State, StepExit) {
  state.throw_type_error(
    state,
    rt_inspect.inspect(state.agent, callee) <> " is not a function",
  )
}

/// One nested runtime call: natives, compiled functions, proxies (a revoked
/// or non-callable proxy throws in there). `t_call` owns the depth bracket
/// and the catch.
fn call_nested(
  state: State,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest_stack: List(JsVal),
) -> Result(State, StepExit) {
  case rt_call.t_call(state.agent, callee, this, args) {
    #(rt_call.NormalCompletion(v), agent) ->
      Ok(State(..state, agent:, stack: [v, ..rest_stack], pc: state.pc + 1))
    #(rt_call.ThrowCompletion(thrown), agent) ->
      Error(Threw(thrown, State(..state, agent:, stack: rest_stack)))
  }
}

/// Elements `0..length-1` of an Array (or Arguments) cell as a list, holes
/// as `undefined`. The spread-call opcodes build their argument array with
/// ArrayFrom/ArraySpread, so this is a plain element read.
pub fn array_values(agent: Agent, v: JsVal) -> List(JsVal) {
  case classify(v) {
    KHandle(h) ->
      case rt_obj.as_sobject(agent, rt_store.t_cell_get(agent, h)) {
        SObject(kind: ArrayObj(length:), elements:, ..)
        | SObject(kind: ArgumentsObj(length:, ..), elements:, ..) ->
          padded_elements(elements, length - 1, [])
        _ -> []
      }
    _ -> []
  }
}

fn padded_elements(
  elements: types.JsElements,
  i: Int,
  acc: List(JsVal),
) -> List(JsVal) {
  case i < 0 {
    True -> acc
    False ->
      padded_elements(elements, i - 1, [rt_elements.get(elements, i), ..acc])
  }
}

// -- [[Construct]] (CallConstructor / CallConstructorApply) --------------------

/// §10.1.13 OrdinaryCreateFromConstructor(newTarget, %Object.prototype%):
/// the fresh receiver for a base-class / plain-function [[Construct]].
/// `? Get(newTarget, "prototype")` is observable and may raise.
fn new_base_this(agent: Agent, new_target: JsVal) -> #(Handle, Agent) {
  let #(proto, agent) =
    rt_call.get_prototype_from_constructor(
      agent,
      new_target,
      rt_call.object_prototype,
    )
  rt_obj.t_new_object(agent, Some(proto))
}

/// §10.2.2 [[Construct]] of `ctor` with `new_target` (== `ctor` for plain
/// `new X()`, the derived class for `super()`, argv[2] for
/// Reflect.construct). Bytecode constructors run flat; bound constructors
/// unwrap; natives, compiled functions and proxies are one nested
/// `rt/call.t_construct`.
pub fn construct(
  state: State,
  ctor: JsVal,
  args: List(JsVal),
  rest_stack: List(JsVal),
  new_target: JsVal,
  drive: Drive,
) -> Result(State, StepExit) {
  // §7.2.4 IsConstructor gate — after ArgumentListEvaluation (§13.3.7.2
  // step 5), so `super(sideEffect())` on a non-ctor parent still ran args.
  case rt_call.is_constructor(state.agent, ctor), classify(ctor) {
    True, KHandle(ctor_h) ->
      construct_handle(state, ctor_h, args, rest_stack, new_target, drive)
    _, _ ->
      state.throw_type_error(
        State(..state, stack: rest_stack),
        rt_inspect.inspect(state.agent, ctor) <> " is not a constructor",
      )
  }
}

fn construct_handle(
  state: State,
  ctor_h: Handle,
  args: List(JsVal),
  rest_stack: List(JsVal),
  new_target: JsVal,
  drive: Drive,
) -> Result(State, StepExit) {
  case rt_store.t_cell_get(state.agent, ctor_h) {
    SObject(
      kind: KBytecode(template:, env:, home_object:, flags:, realm:, unit:, ..),
      ..,
    )
      if realm == state.agent.realm.id
    ->
      case template.is_derived_constructor {
        // Derived: `this` starts in TDZ; `super()` writes it. No
        // constructor_this signals derived mode to Return.
        True ->
          call_function(
            state,
            ctor_h,
            template,
            unit,
            env,
            home_object,
            flags,
            args,
            rest_stack,
            mk_tdz(),
            None,
            new_target,
            drive,
          )
        // Base: §10.1.13 OrdinaryCreateFromConstructor — proto comes from
        // ? Get(newTarget, "prototype"), observable for proxy newTargets.
        False -> {
          use #(new_obj, state) <- result.try(ffi.guarded(
            ffi.guard2(new_base_this, state.agent, new_target),
            State(..state, stack: rest_stack),
          ))
          let this_val = mk_object(new_obj)
          call_function(
            state,
            ctor_h,
            template,
            unit,
            env,
            home_object,
            flags,
            args,
            rest_stack,
            this_val,
            Some(this_val),
            new_target,
            drive,
          )
        }
      }
    // §10.4.1.2 BoundFunction [[Construct]]: prepend bound args; if
    // SameValue(F, newTarget) then newTarget ← target.
    SObject(kind: KBound(target:, bound_args:, ..), ..) -> {
      let nt = case classify(new_target) {
        KHandle(nt_h) if nt_h == ctor_h -> mk_object(target)
        _ -> new_target
      }
      construct(
        state,
        mk_object(target),
        list.append(bound_args, args),
        rest_stack,
        nt,
        drive,
      )
    }
    _ -> {
      use #(h, state) <- result.map(
        guarded(State(..state, stack: rest_stack), fn(agent) {
          rt_call.t_construct(agent, mk_object(ctor_h), args, new_target)
        }),
      )
      State(..state, stack: [mk_object(h), ..rest_stack], pc: state.pc + 1)
    }
  }
}

// -- Return -----------------------------------------------------------------

/// Read one of the frame's lexical pseudo-bindings, unboxing a captured
/// (boxed) slot. `undefined` when the body owns no such slot.
fn read_lexical_local(state: State, ref: lexical.LexicalRef) -> JsVal {
  case lexical.lexical_slot(state.func.lexical, ref) {
    None -> mk_undefined()
    Some(idx) -> {
      let raw = tuple_array.get_unchecked(idx, state.locals)
      case classify(raw) {
        KHandle(h) ->
          case rt_store.t_cell_get(state.agent, h) {
            SBox(value:) -> value
            _ -> raw
          }
        _ -> raw
      }
    }
  }
}

/// The frame's current `this` binding (after any `super()` write).
fn read_this_local(state: State) -> JsVal {
  read_lexical_local(state, lexical.RefThis)
}

/// §10.2.2 steps 10-12 return override for a frame that is unwinding with
/// `return_value`: what the caller receives, or the error to throw (with the
/// state holding it). `constructor_this` is the base-constructor receiver
/// (`Some`) or `None` for plain calls and derived constructors (told apart by
/// the returning template).
fn resolve_return(
  state: State,
  return_value: JsVal,
  constructor_this: Option(JsVal),
) -> Result(JsVal, #(JsVal, State)) {
  case constructor_this {
    // Base constructor: an object result overrides `this`; anything else
    // yields the constructed object. §13.3.7.1 SuperCall step 8
    // BindThisValue is done at the call site by the emitted code.
    Some(constructed) ->
      case classify(return_value) {
        KHandle(_) -> Ok(return_value)
        _ -> Ok(constructed)
      }
    None ->
      case state.func.is_derived_constructor {
        False -> Ok(return_value)
        True ->
          case classify(return_value) {
            KHandle(_) -> Ok(return_value)
            KUndef -> {
              let this_val = read_this_local(state)
              case classify(this_val) {
                KTdz ->
                  Error(state.new_error(
                    state,
                    types.ReferenceErr,
                    "Must call super constructor in derived class before returning from derived constructor",
                  ))
                _ -> Ok(this_val)
              }
            }
            _ ->
              Error(state.new_error(
                state,
                types.TypeErr,
                "Derived constructors may only return object or undefined",
              ))
          }
      }
  }
}

/// The Return opcode. Top of stack (or `undefined`) is the completion
/// value. With no caller frame this activation is done: `Returned` carries
/// the raw value and the final state to the driver. Otherwise apply the
/// constructor return rules, restore the caller, push the value, and give
/// the collector its chance if this landed back in the root activation.
pub fn return_op(state: State) -> Result(State, StepExit) {
  let return_value = case state.stack {
    [v, ..] -> v
    [] -> mk_undefined()
  }
  case state.call_stack {
    [] -> Error(Returned(return_value, state))
    [saved, ..rest_frames] ->
      case resolve_return(state, return_value, saved.constructor_this) {
        Error(#(thrown, state)) -> Error(Threw(thrown, state))
        Ok(pushed) ->
          Ok(return_to(
            state.agent,
            state.outer_depth,
            saved,
            rest_frames,
            pushed,
          ))
      }
  }
}

/// Return from a plain-call frame straight from the loop's registers:
/// `saved` is the caller (`constructor_this` None, callee not a derived
/// constructor, so `resolve_return` is the identity), `value` the completion.
/// The callee's frame is popped whole, so its line is never recorded.
pub fn return_to(
  agent: Agent,
  outer_depth: Int,
  saved: SavedFrame,
  rest_frames: List(SavedFrame),
  value: JsVal,
) -> State {
  safepoint.maybe_collect_at_return(restore_frame(
    leave_frame(agent),
    outer_depth,
    saved,
    [value, ..saved.stack],
    rest_frames,
  ))
}

/// Reinstate `saved` as the running frame (registers, depth, stack frame)
/// with `stack` as its operand stack, under `agent` (the callee's frame
/// already left).
fn restore_frame(
  agent: Agent,
  outer_depth: Int,
  saved: SavedFrame,
  stack: List(JsVal),
  rest_frames: List(SavedFrame),
) -> State {
  let SavedFrame(
    func:,
    unit:,
    locals:,
    stack: _,
    pc:,
    try_stack:,
    constructor_this: _,
    this:,
    new_target:,
    home_object:,
    call_args:,
    eval_env:,
  ) = saved
  State(
    agent:,
    stack:,
    locals:,
    func:,
    unit:,
    code: func.bytecode,
    constants: func.constants,
    pc:,
    call_stack: rest_frames,
    outer_depth:,
    try_stack:,
    this:,
    new_target:,
    home_object:,
    call_args:,
    eval_env:,
  )
}

/// Throw unwinding ran out of try handlers in this frame: drop back into
/// the caller frame (its own try_stack intact) so the search continues
/// there. `None` at the root of the activation.
pub fn unwind_frame(state: State) -> Option(State) {
  case state.call_stack {
    [] -> None
    [saved, ..rest_frames] ->
      Some(restore_frame(
        leave_frame(state.agent),
        state.outer_depth,
        saved,
        saved.stack,
        rest_frames,
      ))
  }
}

// -- arguments / rest ---------------------------------------------------------

/// CreateArguments: §10.4.4.7 unmapped when strict or the parameter list is
/// non-simple (callee = %ThrowTypeError% accessor), else the sloppy form
/// with a data `callee`. Pushes the object.
pub fn create_arguments(state: State, simple_params: Bool) -> State {
  let callee = read_lexical_local(state, lexical.RefActiveFunc)
  let #(obj, agent) = case state.func.is_strict || !simple_params {
    True ->
      rt_obj.t_new_arguments(
        state.agent,
        state.call_args,
        mk_undefined(),
        callee,
      )
    False -> {
      let no_cells: List(Handle) = []
      rt_obj.t_new_arguments(state.agent, state.call_args, no_cells, callee)
    }
  }
  State(..state, agent:, stack: [obj, ..state.stack], pc: state.pc + 1)
}

/// CreateRestArray(from): a plain Array of the call args from `from` on.
pub fn create_rest_array(state: State, from_index: Int) -> State {
  let #(arr, agent) =
    rt_obj.t_new_array(state.agent, list.drop(state.call_args, from_index))
  State(..state, agent:, stack: [arr, ..state.stack], pc: state.pc + 1)
}

// -- Root activations (JsOps.call_bytecode / construct_bytecode) ---------------

/// How a root [[Construct]] entered through `JsOps` must treat its
/// completion value once its own call stack has emptied.
pub type RootKind {
  RootBaseConstruct(this: Handle)
  RootDerivedConstruct
}

/// The receiver a root [[Construct]] of a bytecode function starts with: TDZ
/// for a derived constructor, a fresh object for a base one. §10.2.2 steps
/// 1-3 run in the CALLER's context, before PrepareForOrdinaryCall switches
/// realms, so this is taken before `entry.run_construct` enters the callee's
/// realm: `Error` carries a throwing `newTarget.prototype` with the agent to
/// re-raise it under, its error created from the caller's intrinsics.
pub fn root_this(
  agent: Agent,
  template: FuncTemplate,
  new_target: JsVal,
) -> Result(#(JsVal, RootKind, Agent), #(JsVal, Agent)) {
  use <- bool.guard(
    template.is_derived_constructor,
    Ok(#(mk_tdz(), RootDerivedConstruct, agent)),
  )
  case ffi.guard1(new_base_this(_, new_target), agent) {
    ffi.Ok(value: h, agent:) -> Ok(#(mk_object(h), RootBaseConstruct(h), agent))
    ffi.Threw(agent:, thrown:) -> Error(#(thrown, agent))
  }
}

/// Lay out a fresh root activation of the bytecode cell `fn_h` (its fields
/// already read) for a nested [[Call]] (`new_target` undefined) or
/// [[Construct]] arriving from a builtin or compiled frame, over the receiver
/// prepared for it. The enclosing `t_call`/`apply_ctor` owns the depth
/// bracket; this pushes the stack frame only. `Error` carries a throw raised
/// before the body could start (class-ctor-without-new, created in the
/// callee's realm as §10.2.1 step 2's calleeContext has it) with the agent to
/// re-raise it under.
pub fn enter_root(
  agent: Agent,
  fn_h: Handle,
  template: FuncTemplate,
  env: EnvTuple,
  home_object: Option(Handle),
  flags: FnFlags,
  unit: Int,
  this_arg: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> Result(State, #(JsVal, Agent)) {
  case template.is_class_constructor && is_undefined(new_target) {
    True -> {
      let #(err, agent) =
        agent.store.ops.new_error(
          agent,
          types.TypeErr,
          "Class constructor "
            <> option.unwrap(template.name, "")
            <> " cannot be invoked without 'new'",
        )
      Error(#(err, agent))
    }
    False -> {
      let home = home_value(home_object)
      let #(locals, this_val, agent) =
        setup_frame(
          agent,
          env,
          fn_h,
          home,
          template,
          flags,
          args,
          this_arg,
          new_target,
        )
      Ok(State(
        agent: push_frame_info(agent, template),
        pc: 0,
        stack: [],
        locals:,
        code: template.bytecode,
        constants: template.constants,
        func: template,
        unit:,
        call_stack: [],
        outer_depth: agent.call_depth,
        try_stack: [],
        this: this_val,
        new_target:,
        home_object: home,
        call_args: args,
        eval_env: None,
      ))
    }
  }
}

/// The coroutine hand-off for a root activation `enter_root` laid out.
pub fn root_coroutine(state: State, fn_h: Handle) -> CoroutineCall {
  CoroutineCall(
    fn_h:,
    template: state.func,
    unit: state.unit,
    locals: state.locals,
    this: state.this,
    home_object: state.home_object,
    args: state.call_args,
    rest_stack: [],
  )
}

/// A root [[Construct]]'s `Returned(value, final_state)` folded through the
/// constructor return rules for `kind` (§10.2.2 steps 10-13). `final_state`
/// is the activation's last state over the agent the caller resumed with:
/// its `Error.stack` frame already popped and the caller's realm current.
pub fn finish_root(
  kind: RootKind,
  value: JsVal,
  final_state: State,
) -> Result(#(JsVal, Agent), #(JsVal, Agent)) {
  let constructor_this = case kind {
    RootBaseConstruct(h) -> Some(mk_object(h))
    RootDerivedConstruct -> None
  }
  case resolve_return(final_state, value, constructor_this) {
    Ok(v) -> Ok(#(v, final_state.agent))
    Error(#(thrown, s)) -> Error(#(thrown, s.agent))
  }
}
