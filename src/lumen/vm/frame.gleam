import gleam/option.{type Option}
import lumen/vm/opcode.{type FuncTemplate, type Op}
import lumen/vm/value.{type JsValue, type Ref}

/// A single call frame on the VM call stack.
pub type CallFrame {
  CallFrame(
    func: FuncTemplate,
    locals: List(JsValue),
    this: JsValue,
    env: Option(Ref),
    pc: Int,
    try_stack: List(TryFrame),
  )
}

/// Exception handler frame, pushed by PushTry.
pub type TryFrame {
  TryFrame(catch_target: Int, stack_depth: Int)
}

/// Why we entered a finally block. Saved by EnterFinally, consumed by LeaveFinally.
pub type FinallyCompletion {
  /// Normal completion — continue after finally.
  NormalCompletion
  /// An exception was thrown — re-throw after finally.
  ThrowCompletion(value: JsValue)
  /// A return was interrupted by finally — resume return after.
  ReturnCompletion(value: JsValue)
}

/// The full VM state.
pub type Vm {
  Vm(
    stack: List(JsValue),
    call_stack: List(CallFrame),
    code: List(Op),
    pc: Int,
    finally_stack: List(FinallyCompletion),
  )
}
