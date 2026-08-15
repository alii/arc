//// The interpreter's code and activation types as the shared runtime sees
//// them: opaque payloads it stores in `KBytecode` cells and generator slots
//// and hands back to the interpreter through `JsOps`. The interpreter owns
//// their representation.

/// One compiled function body: bytecode, constant pool, arity, scope data.
pub type FuncTemplate

/// The captured environment a bytecode closure was created with.
pub type EnvTuple

/// An activation parked at a `yield`/`await`, resumable by the interpreter.
pub type SuspendedFrame

/// What resuming a `SuspendedFrame` produced: a return, a throw, or the
/// next suspension.
pub type FrameStep
