import arc/interp/state.{type State, type StepExit}
import arc/rt/types.{type Agent, type JsVal}
import gleam/result

pub type Guarded(v) {
  Value(value: v, agent: Agent)
  Thrown(thrown: JsVal, agent: Agent)
}

pub fn guard_state(
  outcome: Guarded(v),
  state: State,
) -> Result(#(v, State), StepExit) {
  case outcome {
    Value(value:, agent:) -> Ok(#(value, state.with_agent(state, agent)))
    Thrown(thrown:, agent:) ->
      Error(state.Threw(thrown, state.with_agent(state, agent)))
  }
}

// pass a module function, never a fresh closure; st is the agent or state
@external(erlang, "arc_interp_guard_ffi", "guard1")
pub fn guard1(f: fn(st) -> #(v, Agent), st: st) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard2")
pub fn guard2(f: fn(st, a) -> #(v, Agent), st: st, a: a) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard3")
pub fn guard3(
  f: fn(Agent, a, b) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard4")
pub fn guard4(
  f: fn(Agent, a, b, c) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
  c: c,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard5")
pub fn guard5(
  f: fn(Agent, a, b, c, d) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard7")
pub fn guard7(
  f: fn(Agent, a, b, c, d, e, g) -> #(v, Agent),
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
  g: g,
) -> Guarded(v)

@external(erlang, "arc_interp_guard_ffi", "guard_unit1")
pub fn guard_unit1(f: fn(Agent) -> Agent, agent: Agent) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit3")
pub fn guard_unit3(
  f: fn(Agent, a, b) -> Agent,
  agent: Agent,
  a: a,
  b: b,
) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit4")
pub fn guard_unit4(
  f: fn(Agent, a, b, c) -> Agent,
  agent: Agent,
  a: a,
  b: b,
  c: c,
) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit5")
pub fn guard_unit5(
  f: fn(Agent, a, b, c, d) -> Agent,
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
) -> Guarded(Nil)

@external(erlang, "arc_interp_guard_ffi", "guard_unit6")
pub fn guard_unit6(
  f: fn(Agent, a, b, c, d, e) -> Agent,
  agent: Agent,
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
) -> Guarded(Nil)

pub fn guarded(
  state: State,
  body: fn(Agent) -> #(a, Agent),
) -> Result(#(a, State), StepExit) {
  guard_state(guard1(body, state.agent), state)
}

pub fn guarded2(
  state: State,
  f: fn(Agent, a) -> #(v, Agent),
  a: a,
) -> Result(#(v, State), StepExit) {
  guard_state(guard2(f, state.agent, a), state)
}

pub fn guarded3(
  state: State,
  f: fn(Agent, a, b) -> #(v, Agent),
  a: a,
  b: b,
) -> Result(#(v, State), StepExit) {
  guard_state(guard3(f, state.agent, a, b), state)
}

pub fn guarded4(
  state: State,
  f: fn(Agent, a, b, c) -> #(v, Agent),
  a: a,
  b: b,
  c: c,
) -> Result(#(v, State), StepExit) {
  guard_state(guard4(f, state.agent, a, b, c), state)
}

pub fn guarded5(
  state: State,
  f: fn(Agent, a, b, c, d) -> #(v, Agent),
  a: a,
  b: b,
  c: c,
  d: d,
) -> Result(#(v, State), StepExit) {
  guard_state(guard5(f, state.agent, a, b, c, d), state)
}

pub fn guarded7(
  state: State,
  f: fn(Agent, a, b, c, d, e, g) -> #(v, Agent),
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
  g: g,
) -> Result(#(v, State), StepExit) {
  guard_state(guard7(f, state.agent, a, b, c, d, e, g), state)
}

fn drop_nil(r: Result(#(Nil, State), StepExit)) -> Result(State, StepExit) {
  use #(_nil, state) <- result.map(r)
  state
}

pub fn guarded_unit(
  state: State,
  body: fn(Agent) -> Agent,
) -> Result(State, StepExit) {
  guard_state(guard_unit1(body, state.agent), state) |> drop_nil
}

pub fn guarded_unit3(
  state: State,
  f: fn(Agent, a, b) -> Agent,
  a: a,
  b: b,
) -> Result(State, StepExit) {
  guard_state(guard_unit3(f, state.agent, a, b), state) |> drop_nil
}

pub fn guarded_unit4(
  state: State,
  f: fn(Agent, a, b, c) -> Agent,
  a: a,
  b: b,
  c: c,
) -> Result(State, StepExit) {
  guard_state(guard_unit4(f, state.agent, a, b, c), state) |> drop_nil
}

pub fn guarded_unit5(
  state: State,
  f: fn(Agent, a, b, c, d) -> Agent,
  a: a,
  b: b,
  c: c,
  d: d,
) -> Result(State, StepExit) {
  guard_state(guard_unit5(f, state.agent, a, b, c, d), state) |> drop_nil
}

pub fn guarded_unit6(
  state: State,
  f: fn(Agent, a, b, c, d, e) -> Agent,
  a: a,
  b: b,
  c: c,
  d: d,
  e: e,
) -> Result(State, StepExit) {
  guard_state(guard_unit6(f, state.agent, a, b, c, d, e), state) |> drop_nil
}
