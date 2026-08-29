//// flat bytecode calls; other callees nest via rt/call

import arc/bytecode/key.{type Key}
import arc/bytecode/lexical
import arc/bytecode/opcode
import arc/internal/tuple_array.{type TupleArray}
import arc/interp/ffi
import arc/interp/safepoint
import arc/interp/state.{
  type SavedFrame, type State, type StepExit, Returned, SavedFrame,
  SavedRegFrame, State, Threw,
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

pub fn guarded(
  state: State,
  body: fn(Agent) -> #(a, Agent),
) -> Result(#(a, State), StepExit) {
  ffi.guarded(ffi.guard1(body, state.agent), state)
}

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

pub const stack_source = "script"

pub fn frame_info_at(
  template: FuncTemplate(Key),
  line: Int,
) -> types.FrameInfo {
  FrameInfo(name: option.unwrap(template.name, ""), script: stack_source, line:)
}

/// no depth bump, caller already counted it
pub fn push_frame_info(agent: Agent, template: FuncTemplate(Key)) -> Agent {
  Agent(..agent, frames: [frame_info_at(template, 0), ..agent.frames])
}

pub fn pop_frame_info(agent: Agent) -> Agent {
  case agent.frames {
    [_, ..rest] -> Agent(..agent, frames: rest)
    [] -> agent
  }
}

pub fn set_line(agent: Agent, line: Int) -> Agent {
  case agent.frames {
    [FrameInfo(line: l, ..), ..] if l == line -> agent
    [top, ..rest] -> Agent(..agent, frames: [FrameInfo(..top, line:), ..rest])
    [] -> Agent(..agent, frames: [FrameInfo("", stack_source, line)])
  }
}

pub fn current_line(agent: Agent) -> Int {
  case agent.frames {
    [FrameInfo(line:, ..), ..] -> line
    [] -> 0
  }
}

/// catch frames and call_depth up with the loop's fast calls
pub fn sync(state: State, agent: Agent, pc: Int, bump: Int) -> Agent {
  let depth = state.depth
  let line = tuple_array.element(pc + 1, state.func.lines)
  case depth - agent.call_depth, bump {
    0, 0 ->
      case agent.frames {
        [FrameInfo(line: l, ..), ..] if l == line -> agent
        frames -> Agent(..agent, frames: set_top_line(frames, line))
      }
    behind, _ if behind <= 0 ->
      Agent(
        ..agent,
        call_depth: depth + bump,
        frames: set_top_line(agent.frames, line),
      )
    behind, _ ->
      Agent(
        ..agent,
        call_depth: depth + bump,
        frames: pending_frames(agent.frames, state, line, behind),
      )
  }
}

fn set_top_line(frames: List(types.FrameInfo), line: Int) {
  case frames {
    [FrameInfo(line: l, ..), ..] if l == line -> frames
    [top, ..rest] -> [FrameInfo(..top, line:), ..rest]
    [] -> [FrameInfo("", stack_source, line)]
  }
}

fn pending_frames(
  frames: List(types.FrameInfo),
  state: State,
  line: Int,
  behind: Int,
) -> List(types.FrameInfo) {
  case behind, state.call_stack {
    0, _ -> set_top_line(frames, line)
    _, [saved, ..] -> [
      frame_info_at(state.func, line),
      ..pending_frames(
        frames,
        saved.caller,
        tuple_array.element(saved.pc, saved.caller.func.lines),
        behind - 1,
      )
    ]
    _, [] -> [frame_info_at(state.func, line), ..frames]
  }
}

// frames above agent.call_depth were never pushed
fn leave_frame(agent: Agent, depth: Int) -> Agent {
  case agent.call_depth == depth {
    False -> agent
    True -> {
      let frames = case agent.frames {
        [_, ..rest] -> rest
        [] -> []
      }
      Agent(..agent, call_depth: depth - 1, frames:)
    }
  }
}

pub type CoroutineCall {
  CoroutineCall(
    fn_h: Handle,
    template: FuncTemplate(Key),
    unit: Int,
    locals: TupleArray(JsVal),
    this: JsVal,
    home_object: JsVal,
    args: List(JsVal),
    rest_stack: List(JsVal),
  )
}

/// callbacks into interpreter parts this module cannot import
pub type Drive {
  Drive(start_coroutine: fn(State, CoroutineCall) -> Result(State, StepExit))
}

/// §10.2.1.2 bind this, then lay out locals
fn setup_frame(
  agent: Agent,
  env: EnvTuple,
  callee: JsVal,
  home: JsVal,
  template: FuncTemplate(Key),
  flags: FnFlags,
  args: List(JsVal),
  this_arg: JsVal,
  new_target: JsVal,
) -> #(TupleArray(JsVal), JsVal, Agent) {
  let #(this_val, agent) = case template.is_arrow || flags.is_strict {
    True -> #(this_arg, agent)
    False ->
      case ffi.is(this_arg, ffi.Undefined) {
        True -> #(ffi.object([agent.realm.global_object]), agent)
        False -> {
          let bound = ffi.bind_this(this_arg, agent.realm.global_object)
          case ffi.is(bound, ffi.Miss) {
            False -> #(bound, agent)
            True -> rt_call.resolve_this(agent, flags, this_arg)
          }
        }
      }
  }
  #(
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
    ),
    this_val,
    agent,
  )
}

/// §10.2.1 flat entry: park caller, switch to callee
pub fn call_function(
  state: State,
  fn_h: Handle,
  template: FuncTemplate(Key),
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
  case template.is_class_constructor && ffi.is(new_target, ffi.Undefined) {
    True ->
      state.throw_type_error(
        State(..state, stack: rest_stack),
        "Class constructor "
          <> option.unwrap(template.name, "")
          <> " cannot be invoked without 'new'",
      )
    False -> {
      let home = case home_object {
        Some(h) -> ffi.object([h])
        None -> ffi.val([ffi.Undefined])
      }
      let #(locals, this_val, agent) =
        setup_frame(
          state.agent,
          env,
          ffi.object([fn_h]),
          home,
          template,
          flags,
          args,
          this_arg,
          new_target,
        )
      case template.is_generator || template.is_async {
        True ->
          drive.start_coroutine(
            State(..state, agent:),
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
        False -> {
          let depth = state.depth
          case depth >= limits.max_call_depth {
            True ->
              state.throw_range_error(
                State(..state, agent:, stack: rest_stack),
                "Maximum call stack size exceeded",
              )
            False -> {
              let saved =
                SavedFrame(
                  caller: state,
                  pc: state.pc + 1,
                  stack: rest_stack,
                  locals: state.locals,
                  constructor_this:,
                )
              Ok(State(
                agent:,
                stack: [],
                locals:,
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
              ))
            }
          }
        }
      }
    }
  }
}

/// §15.10 isintailposition
pub fn is_tail_call(state: State, pc: Int, callee: FuncTemplate(Key)) -> Bool {
  let frame_eligible = case state.try_stack, state.call_stack {
    [], [_, ..] ->
      state.func.is_strict
      && !callee.is_generator
      && !callee.is_async
      && ffi.is(state.new_target, ffi.Undefined)
    _, _ -> False
  }
  case frame_eligible {
    False -> False
    True ->
      case tuple_array.element(pc + 2, state.func.bytecode) {
        opcode.Return -> True
        _ -> False
      }
  }
}

/// §15.10.3 drop the just-parked caller frame
pub fn elide_tail_frame(new_state: State) -> State {
  case new_state.call_stack {
    [saved, ..rest_frames] ->
      State(
        ..new_state,
        call_stack: rest_frames,
        depth: saved.caller.depth,
        agent: leave_frame(new_state.agent, saved.caller.depth),
      )
    [] -> new_state
  }
}

pub fn call(
  state: State,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
  rest_stack: List(JsVal),
  drive: Drive,
) -> Result(State, StepExit) {
  let slot = ffi.cell_of(state.agent, callee)
  case ffi.is(slot, ffi.Miss) {
    False ->
      call_cell(
        state,
        ffi.handle([callee]),
        slot,
        this,
        args,
        rest_stack,
        drive,
      )
    True ->
      case classify(callee) {
        // dangling handle: t_cell_get names the use-after-free
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
}

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
          ffi.val([ffi.Undefined]),
          drive,
        )
      case state.func.is_strict && is_tail_call(state, state.pc, template) {
        True -> result.map(res, elide_tail_frame)
        False -> res
      }
    }
    // §10.4.1.1 bound call
    SObject(kind: KBound(target:, bound_this:, bound_args:), ..) ->
      call(
        state,
        mk_object(target),
        bound_this,
        list.append(bound_args, args),
        rest_stack,
        drive,
      )
    // §20.2.3.3 function.prototype.call
    SObject(kind: KNative(tag: FunctionN(FunctionCall), ..), ..) -> {
      let #(this_arg, call_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(mk_undefined(), [])
      }
      call(state, this, this_arg, call_args, rest_stack, drive)
    }
    // §20.2.3.1 function.prototype.apply
    SObject(kind: KNative(tag: FunctionN(FunctionApply), ..), ..) -> {
      let #(this_arg, arg_array) = case args {
        [t, a, ..] -> #(t, a)
        [t] -> #(t, mk_undefined())
        [] -> #(mk_undefined(), mk_undefined())
      }
      use <- require_callable(state, this)
      use #(call_args, state) <- result.try(case classify(arg_array) {
        KUndef | KNull -> Ok(#([], state))
        _ -> list_from_array_like(state, arg_array, rest_stack)
      })
      call(state, this, this_arg, call_args, rest_stack, drive)
    }
    // §28.1.1 reflect.apply
    SObject(kind: KNative(tag: ReflectN(ReflectApply), ..), ..) -> {
      let #(target, this_arg, args_list) = case args {
        [t, a, l, ..] -> #(t, a, l)
        [t, a] -> #(t, a, mk_undefined())
        [t] -> #(t, mk_undefined(), mk_undefined())
        [] -> #(mk_undefined(), mk_undefined(), mk_undefined())
      }
      use <- require_callable(state, target)
      use #(call_args, state) <- result.try(list_from_array_like(
        state,
        args_list,
        rest_stack,
      ))
      call(state, target, this_arg, call_args, rest_stack, drive)
    }
    // other-realm closure runs as a nested root activation
    SObject(kind: KNative(tag:, ..), ..) ->
      call_native(state, tag, mk_object(h), this, args, rest_stack)
    SObject(kind: KBytecode(..), ..)
    | SObject(kind: KCompiled(..), ..)
    | SObject(kind: ProxyObj(..), ..) ->
      call_nested(state, mk_object(h), this, args, rest_stack)
    _ -> not_a_function(state, mk_object(h))
  }
}

/// §7.3.20 createlistfromarraylike
fn list_from_array_like(
  state: State,
  array_like: JsVal,
  rest_stack: List(JsVal),
) -> Result(#(List(JsVal), State), StepExit) {
  let args = ffi.list_of(state.agent, array_like)
  case ffi.is(args, ffi.Miss) {
    False -> Ok(#(args, state))
    True ->
      guarded(State(..state, stack: rest_stack), fn(agent) {
        b_function.create_list_from_array_like(agent, array_like)
      })
  }
}

/// at the depth limit the nested t_call raises rangeerror
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

/// stack left as is; unwind truncates it
fn not_a_function(state: State, callee: JsVal) -> Result(State, StepExit) {
  state.throw_type_error(
    state,
    rt_inspect.inspect(state.agent, callee) <> " is not a function",
  )
}

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

/// holes as undefined
pub fn array_values(agent: Agent, v: JsVal) -> List(JsVal) {
  case classify(v) {
    KHandle(h) ->
      case rt_obj.as_sobject(rt_store.t_cell_get(agent, h)) {
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

/// §10.1.13, reading prototype may raise
fn new_base_this(agent: Agent, new_target: JsVal) -> #(Handle, Agent) {
  let #(proto, agent) =
    rt_call.get_prototype_from_constructor(
      agent,
      new_target,
      rt_call.object_prototype,
    )
  rt_obj.t_new_receiver(agent, proto)
}

/// §10.2.2 construct
pub fn construct(
  state: State,
  ctor: JsVal,
  args: List(JsVal),
  rest_stack: List(JsVal),
  new_target: JsVal,
  drive: Drive,
) -> Result(State, StepExit) {
  // §7.2.4 gate runs after argument evaluation
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
        // derived: this starts in tdz, super() writes it
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
    // §10.4.1.2 bound construct
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
    _ ->
      case
        ffi.guard4(
          rt_call.t_construct,
          state.agent,
          ffi.object([ctor_h]),
          args,
          new_target,
        )
      {
        ffi.Ok(value: h, agent:) ->
          Ok(
            State(
              ..state,
              agent:,
              stack: [ffi.object([h]), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        ffi.Threw(agent:, thrown:) ->
          Error(Threw(thrown, State(..state, agent:, stack: rest_stack)))
      }
  }
}

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

fn read_this_local(state: State) -> JsVal {
  read_lexical_local(state, lexical.RefThis)
}

/// §10.2.2 steps 10-12 constructor return override
fn resolve_return(
  state: State,
  return_value: JsVal,
  constructor_this: Option(JsVal),
) -> Result(JsVal, #(JsVal, State)) {
  case constructor_this {
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

pub fn return_op(state: State) -> Result(State, StepExit) {
  let return_value = case state.stack {
    [v, ..] -> v
    [] -> mk_undefined()
  }
  case state.call_stack {
    [] -> Error(Returned(return_value, state))
    [saved, ..] ->
      case resolve_return(state, return_value, saved.constructor_this) {
        Error(#(thrown, state)) -> Error(Threw(thrown, state))
        Ok(pushed) -> Ok(return_to(state, saved, pushed))
      }
  }
}

fn return_to(state: State, saved: SavedFrame, value: JsVal) -> State {
  safepoint.maybe_collect_at_return(
    restore_frame(leave_frame(state.agent, state.depth), saved, [
      value,
      ..saved.stack
    ]),
  )
}

pub fn restore_frame(
  agent: Agent,
  saved: SavedFrame,
  stack: List(JsVal),
) -> State {
  let caller = saved.caller
  let locals = case saved, caller.func.regs {
    SavedRegFrame(locals:, r0:, r1:, ..), bytecode.Regs(a, b) ->
      ffi.flush_regs(locals, a, b, r0, r1)
    _, _ -> saved.locals
  }
  State(..caller, agent:, stack:, locals:, pc: saved.pc)
}

/// out of handlers here; continue the search in the caller
pub fn unwind_frame(state: State) -> Option(State) {
  case state.call_stack {
    [] -> None
    [saved, ..] ->
      Some(restore_frame(
        leave_frame(state.agent, state.depth),
        saved,
        saved.stack,
      ))
  }
}

/// §10.4.4.7 unmapped when strict or non-simple params
pub fn create_arguments(state: State, simple_params: Bool) -> State {
  let #(obj, agent) = arguments_object(state, simple_params)
  State(..state, agent:, stack: [obj, ..state.stack], pc: state.pc + 1)
}

pub fn arguments_object(state: State, simple_params: Bool) -> #(JsVal, Agent) {
  let callee = read_lexical_local(state, lexical.RefActiveFunc)
  case state.func.is_strict || !simple_params {
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
}

pub fn create_rest_array(state: State, from_index: Int) -> State {
  let #(arr, agent) =
    rt_obj.t_new_array(state.agent, list.drop(state.call_args, from_index))
  State(..state, agent:, stack: [arr, ..state.stack], pc: state.pc + 1)
}

pub type RootKind {
  RootBaseConstruct(this: Handle)
  RootDerivedConstruct
}

/// §10.2.2 steps 1-3, taken in the caller's realm
pub fn root_this(
  agent: Agent,
  template: FuncTemplate(Key),
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

/// same per cell, so callers making many build it once
pub type RootCallee {
  RootCallee(
    callee: JsVal,
    template: FuncTemplate(Key),
    env: EnvTuple,
    home: JsVal,
    flags: FnFlags,
    unit: Int,
  )
}

pub fn root_callee(
  fn_h: Handle,
  template: FuncTemplate(Key),
  env: EnvTuple,
  home_object: Option(Handle),
  flags: FnFlags,
  unit: Int,
) -> RootCallee {
  RootCallee(
    callee: mk_object(fn_h),
    template:,
    env:,
    home: case home_object {
      Some(h) -> ffi.object([h])
      None -> ffi.val([ffi.Undefined])
    },
    flags:,
    unit:,
  )
}

/// caller has already checked the depth limit
pub fn enter_root(
  agent: Agent,
  callee: RootCallee,
  this_arg: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> Result(State, #(JsVal, Agent)) {
  let template = callee.template
  case template.is_class_constructor && ffi.is(new_target, ffi.Undefined) {
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
    False -> Ok(root_state(agent, callee, this_arg, args, new_target))
  }
}

pub fn root_state(
  agent: Agent,
  callee: RootCallee,
  this_arg: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> State {
  let RootCallee(callee:, template:, env:, home:, flags:, unit:) = callee
  let #(locals, this_val, agent) =
    setup_frame(
      agent,
      env,
      callee,
      home,
      template,
      flags,
      args,
      this_arg,
      new_target,
    )
  // frame push and depth bump happen lazily at the first sync
  let depth = agent.call_depth + 1
  State(
    agent:,
    pc: 0,
    stack: [],
    locals:,
    func: template,
    unit:,
    call_stack: [],
    outer_depth: depth,
    depth:,
    try_stack: [],
    this: this_val,
    new_target:,
    home_object: home,
    call_args: args,
    eval_env: None,
  )
}

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

/// §10.2.2 steps 10-13 for a root construct
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
