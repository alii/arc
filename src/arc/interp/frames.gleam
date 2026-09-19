import arc/internal/tuple_array
import arc/interp/state.{type State}
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/types.{type Agent, type FrameInfo, Agent, FrameInfo}
import gleam/option

pub const stack_source = "script"

pub fn frame_info_at(template: FuncTemplate, line: Int) -> FrameInfo {
  FrameInfo(name: option.unwrap(template.name, ""), script: stack_source, line:)
}

// no depth bump, caller already counted it
pub fn push_frame_info(agent: Agent, template: FuncTemplate) -> Agent {
  Agent(..agent, frames: [frame_info_at(template, 0), ..agent.frames])
}

pub fn pop_frame_info(agent: Agent) -> Agent {
  case agent.frames {
    [_, ..rest] -> Agent(..agent, frames: rest)
    [] -> agent
  }
}

// catch frames and call_depth up with the in-loop calls
pub fn sync(state: State, agent: Agent, pc: Int) -> Agent {
  let depth = state.depth
  let line = tuple_array.element(pc + 1, state.func.lines)
  case depth - agent.call_depth {
    0 ->
      case agent.frames {
        [FrameInfo(line: l, ..), ..] if l == line -> agent
        frames -> Agent(..agent, frames: set_top_line(frames, line))
      }
    behind if behind < 0 ->
      Agent(
        ..agent,
        call_depth: depth,
        frames: set_top_line(agent.frames, line),
      )
    behind ->
      Agent(
        ..agent,
        call_depth: depth,
        frames: pending_frames(agent.frames, state, line, behind),
      )
  }
}

// same, then count the frame being entered
pub fn sync_entering(state: State, agent: Agent, pc: Int) -> Agent {
  let depth = state.depth
  let line = tuple_array.element(pc + 1, state.func.lines)
  case depth - agent.call_depth {
    behind if behind <= 0 ->
      Agent(
        ..agent,
        call_depth: depth + 1,
        frames: set_top_line(agent.frames, line),
      )
    behind ->
      Agent(
        ..agent,
        call_depth: depth + 1,
        frames: pending_frames(agent.frames, state, line, behind),
      )
  }
}

fn set_top_line(frames: List(FrameInfo), line: Int) -> List(FrameInfo) {
  case frames {
    [FrameInfo(line: l, ..), ..] if l == line -> frames
    [top, ..rest] -> [FrameInfo(..top, line:), ..rest]
    [] -> [FrameInfo("", stack_source, line)]
  }
}

fn pending_frames(
  frames: List(FrameInfo),
  state: State,
  line: Int,
  behind: Int,
) -> List(FrameInfo) {
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

pub type EntryMark {
  EntryMark(frames: List(FrameInfo), call_depth: Int)
}

pub fn mark(agent: Agent) -> EntryMark {
  EntryMark(frames: agent.frames, call_depth: agent.call_depth)
}

// restore entry frames and depth
pub fn settle(agent: Agent, m: EntryMark) -> Agent {
  resettle(agent, m.frames, m.call_depth)
}

// a callee that never synced left frames and depth untouched
pub fn resettle(agent: Agent, frames: List(FrameInfo), depth: Int) -> Agent {
  case agent.call_depth == depth && agent.frames == frames {
    True -> agent
    False -> Agent(..agent, frames:, call_depth: depth)
  }
}
