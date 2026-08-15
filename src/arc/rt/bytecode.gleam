//// The interpreter's code and activation types as the shared runtime sees
//// them: opaque payloads it stores in `KBytecode` cells and generator slots
//// and hands back to the interpreter through `JsOps`. The interpreter owns
//// their representation.

import arc/vm/opcode.{type Pc, type TryKind}

/// One compiled function body: bytecode, constant pool, arity, scope data.
pub type FuncTemplate

/// The captured environment a bytecode closure was created with.
pub type EnvTuple

/// An activation parked at a `yield`/`await`, resumable by the interpreter.
pub type SuspendedFrame

/// Exception handler frame, pushed by PushTry. `kind` is copied straight off
/// the opcode: it says whether unwinding a *return* completion past this frame
/// must run a finally subroutine, close a live iterator, or just skip it
/// (see `opcode.TryKind`). Lives here so `SuspendedFrame` can carry the live
/// try-stack verbatim.
pub type TryFrame {
  TryFrame(catch_target: Int, stack_depth: Int, kind: TryKind(Pc))
}
