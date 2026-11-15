import arc/internal/tuple_array
import arc/interp/state.{
  type Frame, Direct, Outermost, SavedCont, SavedFrame, SavedRegs, SavedThread,
}
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/types.{type Agent, type FrameInfo, Agent, FrameInfo, Unlisted}
import gleam/option

pub const stack_source = "script"

// more than can be pending
const all_frames = 1_000_000

pub fn frame_info_at(template: FuncTemplate, line: Int) -> FrameInfo {
  FrameInfo(name: option.unwrap(template.name, ""), script: stack_source, line:)
}

// doesn't bump the depth because the caller already counted it
pub fn push_frame_info(agent: Agent, template: FuncTemplate) -> Agent {
  Agent(..agent, frames: [frame_info_at(template, 0), ..agent.frames])
}

pub fn pop_frame_info(agent: Agent) -> Agent {
  case agent.frames {
    [_, ..rest] -> Agent(..agent, frames: rest)
    [] -> agent
  }
}

// catch frames and call_depth up with the calls threaded code made
pub fn sync(frame: Frame, agent: Agent, pc: Int) -> Agent {
  let depth = frame.depth
  let line = tuple_array.element(pc, frame.func.lines)
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
        frames: pending_frames(agent.frames, frame, line, behind, all_frames),
      )
  }
}

pub fn set_top_line(frames: List(FrameInfo), line: Int) -> List(FrameInfo) {
  case frames {
    [FrameInfo(line: l, ..), ..] if l == line -> frames
    [FrameInfo(..) as top, ..rest] -> [FrameInfo(..top, line:), ..rest]
    [Unlisted(innermost:, line: at, behind:), ..rest] ->
      pending_frames(rest, running(innermost), at, behind, all_frames)
      |> set_top_line(line)
    [] -> [FrameInfo("", stack_source, line)]
  }
}

@external(erlang, "gleam_stdlib", "identity")
fn running(frame: types.RunningFrame) -> Frame

// called by name from rt/builtins/error
pub fn pending_frames(
  frames: List(FrameInfo),
  frame: Frame,
  line: Int,
  behind: Int,
  wanted: Int,
) -> List(FrameInfo) {
  case behind, frame.return_to {
    _, _ if wanted <= 0 -> []
    _, _ if behind <= 0 -> set_top_line(frames, line)
    _, Outermost | _, Direct -> [frame_info_at(frame.func, line), ..frames]
    _, SavedFrame(caller:, pc:, ..)
    | _, SavedThread(caller:, pc:, ..)
    | _, SavedRegs(caller:, pc:, ..)
    | _, SavedCont(caller:, pc:, ..)
    -> [
      frame_info_at(frame.func, line),
      ..pending_frames(
        frames,
        caller,
        tuple_array.element(pc - 1, caller.func.lines),
        behind - 1,
        wanted - 1,
      )
    ]
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
