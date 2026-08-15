//// The interpreter's GC safepoints: a collection triggered from the root
//// activation keeps what the frame holds and drops the rest, memory stays
//// bounded under a small threshold, the turn-end drain keeps the completion
//// value, and parked coroutine frames / closure environments are traced.

import arc/bytecode/lexical
import arc/internal/tuple_array
import arc/interp/safepoint
import arc/interp/state.{type State, SavedFrame, State}
import arc/rt/async as rt_async
import arc/rt/bytecode.{
  type FuncTemplate, FuncTemplate, ParkedOp, SuspendedFrame,
}
import arc/rt/gc as rt_gc
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, Agent, FnFlags, GenSuspendedYield,
  HostJob, JsStore, KBytecode, KHandle, NoElements, ResumeFrame, SGenerator,
  SObject, classify, mk_object, mk_undefined,
}
import gleam/dict
import gleam/option.{None, Some}
import gleam/set
import rt_helpers

const threshold = 64

/// A realm-initialised agent, collected once so the counter starts at zero,
/// with a threshold small enough that a few dozen allocations trip it.
fn small_agent() -> Agent {
  let st = rt_gc.t_collect(rt_helpers.agent(), [])
  Agent(..st, store: JsStore(..st.store, gc_threshold: threshold))
}

fn new_object(st: Agent) -> #(Handle, JsVal, Agent) {
  let #(v, st) = rt_obj.t_new_object_literal(st)
  let assert KHandle(h) = classify(v)
  #(h, v, st)
}

/// Allocate `n` objects nothing refers to.
fn churn(st: Agent, n: Int) -> Agent {
  case n {
    0 -> st
    _ -> {
      let #(_, _, st) = new_object(st)
      churn(st, n - 1)
    }
  }
}

fn empty_template() -> FuncTemplate {
  FuncTemplate(
    name: None,
    arity: 0,
    length: 0,
    local_count: 1,
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
    lexical: lexical.NoLexicalSlots,
    code_kind: lexical.ScriptCode,
  )
}

/// A root activation over `agent` holding `locals` and `stack`.
fn root_state(agent: Agent, locals: List(JsVal), stack: List(JsVal)) -> State {
  let func = empty_template()
  State(
    agent:,
    pc: 0,
    stack:,
    locals: tuple_array.from_list(locals),
    code: func.bytecode,
    constants: func.constants,
    func:,
    unit: 0,
    call_stack: [],
    try_stack: [],
    this: mk_undefined(),
    new_target: mk_undefined(),
    home_object: mk_undefined(),
    call_args: [],
    eval_env: None,
  )
}

pub fn toplevel_return_collects_and_keeps_frame_values_test() {
  let st = small_agent()
  let #(local_h, local, st) = new_object(st)
  let #(stacked_h, stacked, st) = new_object(st)
  let #(dead_h, _, st) = new_object(st)
  let st = churn(st, threshold)
  let s =
    safepoint.maybe_collect_at_toplevel(root_state(st, [local], [stacked]))
  assert rt_gc.t_is_live(s.agent, local_h)
  assert rt_gc.t_is_live(s.agent, stacked_h)
  assert !rt_gc.t_is_live(s.agent, dead_h)
  assert rt_gc.stats(s.agent).since_gc == 0
}

pub fn below_threshold_does_not_collect_test() {
  let st = small_agent()
  let #(dead_h, _, st) = new_object(st)
  let s = safepoint.maybe_collect_at_toplevel(root_state(st, [], []))
  assert rt_gc.t_is_live(s.agent, dead_h)
}

pub fn nested_activation_never_collects_test() {
  // A native above us entered a call: its registers are invisible here.
  let st = rt_store.t_enter_call(small_agent())
  let #(dead_h, _, st) = new_object(st)
  let st = churn(st, threshold)
  let s = safepoint.maybe_collect_at_toplevel(root_state(st, [], []))
  assert rt_gc.t_is_live(s.agent, dead_h)
}

pub fn inner_frame_return_never_collects_test() {
  let st = small_agent()
  let #(dead_h, _, st) = new_object(st)
  let st = churn(st, threshold)
  let s = root_state(st, [], [])
  let caller =
    SavedFrame(
      func: s.func,
      unit: s.unit,
      locals: s.locals,
      stack: [],
      pc: 0,
      try_stack: [],
      constructor_this: None,
      this: mk_undefined(),
      new_target: mk_undefined(),
      home_object: mk_undefined(),
      call_args: [],
      eval_env: None,
    )
  let s = safepoint.maybe_collect_at_toplevel(State(..s, call_stack: [caller]))
  assert rt_gc.t_is_live(s.agent, dead_h)
}

pub fn allocation_loop_stays_bounded_test() {
  let st = small_agent()
  let #(kept_h, kept, st) = new_object(st)
  let base = rt_gc.stats(st).live
  let s = stress(root_state(st, [kept], []), 200, base)
  assert rt_gc.t_is_live(s.agent, kept_h)
  assert rt_gc.stats(s.agent).live <= base + 2 * threshold
}

/// `rounds` iterations of: allocate past the threshold, hit the safepoint,
/// check the live set never exceeds the baseline by more than one round.
fn stress(s: State, rounds: Int, base: Int) -> State {
  case rounds {
    0 -> s
    _ -> {
      let s = State(..s, agent: churn(s.agent, 2 * threshold))
      let s = safepoint.maybe_collect_at_toplevel(s)
      assert rt_gc.stats(s.agent).live <= base + 2 * threshold
      stress(s, rounds - 1, base)
    }
  }
}

pub fn end_turn_keeps_completion_value_across_drain_test() {
  let st = small_agent()
  let #(kept_h, kept, st) = new_object(st)
  let #(dead_h, _, st) = new_object(st)
  let base = rt_gc.stats(st).live
  let alloc_job = HostJob(churn(_, 2 * threshold))
  let st =
    st
    |> rt_async.t_enqueue_job(alloc_job)
    |> rt_async.t_enqueue_job(alloc_job)
    |> rt_async.t_enqueue_job(alloc_job)
  let st = safepoint.end_turn(st, [kept])
  assert rt_gc.t_is_live(st, kept_h)
  assert !rt_gc.t_is_live(st, dead_h)
  assert rt_gc.stats(st).live <= base + 2 * threshold
  // The hold is scoped to the turn end: nothing stays pinned.
  assert !set.contains(st.store.pinned_roots, kept_h.id)
}

pub fn end_turn_leaves_permanent_pins_alone_test() {
  let st = small_agent()
  let #(pinned_h, pinned, st) = new_object(st)
  let st = rt_store.t_pin_root(st, pinned_h)
  let st = safepoint.end_turn(st, [pinned])
  assert set.contains(st.store.pinned_roots, pinned_h.id)
}

pub fn parked_frame_roots_its_registers_test() {
  let st = small_agent()
  let #(local_h, local, st) = new_object(st)
  let #(stacked_h, stacked, st) = new_object(st)
  let #(this_h, this, st) = new_object(st)
  let #(env_h, _, st) = new_object(st)
  let #(dead_h, _, st) = new_object(st)
  let frame =
    SuspendedFrame(
      template: empty_template(),
      pc: 3,
      locals: tuple_array.from_list([mk_undefined(), local]),
      stack: [stacked],
      try_stack: [],
      this:,
      home_object: mk_undefined(),
      eval_env: Some(env_h.id),
      line: 1,
      parked: ParkedOp,
      call_args: [],
      realm: st.realm.id,
      unit: 0,
    )
  let #(gen_h, st) =
    rt_store.t_cell_new(
      st,
      SGenerator(state: GenSuspendedYield, resume: ResumeFrame(frame)),
    )
  let st = rt_gc.t_collect(st, [gen_h])
  assert rt_gc.t_is_live(st, local_h)
  assert rt_gc.t_is_live(st, stacked_h)
  assert rt_gc.t_is_live(st, this_h)
  assert rt_gc.t_is_live(st, env_h)
  assert !rt_gc.t_is_live(st, dead_h)
  // And once nothing holds the generator, the whole frame goes.
  let st = rt_gc.t_collect(st, [])
  assert !rt_gc.t_is_live(st, local_h)
  assert !rt_gc.t_is_live(st, gen_h)
}

pub fn closure_environment_and_constants_are_traced_test() {
  let st = small_agent()
  let #(captured_h, captured, st) = new_object(st)
  let #(pooled_h, pooled, st) = new_object(st)
  let #(dead_h, _, st) = new_object(st)
  let template =
    FuncTemplate(..empty_template(), constants: tuple_array.from_list([pooled]))
  let flags =
    FnFlags(
      is_constructor: False,
      is_class_constructor: False,
      is_derived_constructor: False,
      is_arrow: True,
      is_method: False,
      is_generator: False,
      is_async: False,
      is_strict: True,
    )
  let #(fn_h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: KBytecode(
          template:,
          env: bytecode.env_from_list([captured]),
          home_object: None,
          flags:,
          fields_init: None,
          realm: 0,
          unit: 0,
        ),
        proto: None,
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = rt_obj.t_global_set(st, <<"f">>, mk_object(fn_h))
  let st = rt_gc.t_collect(st, [])
  assert rt_gc.t_is_live(st, captured_h)
  assert rt_gc.t_is_live(st, pooled_h)
  assert !rt_gc.t_is_live(st, dead_h)
}
