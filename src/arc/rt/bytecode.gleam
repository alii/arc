//// The interpreter's code and activation types as the shared runtime sees
//// them: the payloads it stores in `KBytecode` cells and generator slots and
//// hands back to the interpreter through `JsOps`. Leaf module: imports only
//// the compiler-side opcode/lexical definitions and the wire value type, so
//// `arc/rt/types` can import it without a cycle.

import arc/rt/wire.{type JsVal}
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/lexical.{type CodeKind, type LexicalSlots}
import arc/vm/opcode.{type Op, type Pc, type TryKind}
import gleam/option.{type Option}

/// Whether a frame's VariableEnvironment is the GLOBAL environment
/// (script/REPL top level) or the frame's own eval env. Sloppy direct eval
/// sends `var` declarations to one or the other.
pub type VarEnvKind {
  GlobalVarEnv
  FrameVarEnv
}

/// The name table a direct eval in this body needs: every local binding's
/// name → local slot index (all such locals are boxed so direct eval can
/// read/write them by index), plus the kind of VariableEnvironment the frame
/// runs in.
pub type EvalNameTable {
  EvalNameTable(var_env: VarEnvKind, names: List(#(String, Int)))
}

/// How to capture one variable from the enclosing scope when creating a
/// closure: copy the parent frame's local at `parent_index`. The only capture
/// mode the compiler emits; transitive captures are flattened by scope
/// analysis so each MakeClosure reaches exactly one frame up. A boxed
/// (mutated-after-capture) variable's parent local already holds the box
/// cell, so copying it shares the cell.
pub type EnvCapture {
  CaptureLocal(parent_index: Int)
}

/// Compiled function definition (§10.2 [[ECMAScriptCode]] and friends).
/// The constant pool holds wire values built at compile time: primitives and
/// the TDZ sentinel only, never a heap handle.
pub type FuncTemplate {
  FuncTemplate(
    name: Option(String),
    /// Fixed formal parameters bound positionally into local slots.
    arity: Int,
    /// §15.1.5 ExpectedArgumentCount: the `length` property value.
    length: Int,
    local_count: Int,
    bytecode: TupleArray(Op),
    constants: TupleArray(JsVal),
    functions: TupleArray(FuncTemplate),
    env_descriptors: List(EnvCapture),
    is_strict: Bool,
    is_arrow: Bool,
    is_derived_constructor: Bool,
    is_generator: Bool,
    is_async: Bool,
    /// §7.2.4 [[Construct]] capability, decided from the syntactic kind.
    is_constructor: Bool,
    /// §10.2.1 step 2 [[IsClassConstructor]].
    is_class_constructor: Bool,
    /// Present only for functions that contain a direct eval call.
    local_names: Option(EvalNameTable),
    /// Where the §9.1.1.3 `this`/function/home/new.target quartet lives.
    lexical: LexicalSlots,
    /// §19.2.1.1 PerformEval step 6 legality bits for a direct eval here.
    code_kind: CodeKind,
  )
}

/// The captured environment a bytecode closure was created with: the values
/// copied out of the parent frame per `env_descriptors`, in order, as one
/// Erlang tuple. Frame setup lays it out as the leading run of the callee's
/// locals. The GC walks it as a plain term.
pub type EnvTuple

/// Pack captured values (in `env_descriptors` order) into an environment.
@external(erlang, "erlang", "list_to_tuple")
pub fn env_from_list(values: List(JsVal)) -> EnvTuple

/// The captured values back as a list, in order.
@external(erlang, "erlang", "tuple_to_list")
pub fn env_to_list(env: EnvTuple) -> List(JsVal)

/// Number of captured values.
@external(erlang, "erlang", "tuple_size")
pub fn env_size(env: EnvTuple) -> Int

/// Exception handler frame, pushed by PushTry. `kind` is copied straight off
/// the opcode: it says whether unwinding a *return* completion past this frame
/// must run a finally subroutine, close a live iterator, or just skip it
/// (see `opcode.TryKind`). Lives here so `SuspendedFrame` can carry the live
/// try-stack verbatim.
pub type TryFrame {
  TryFrame(catch_target: Int, stack_depth: Int, kind: TryKind(Pc))
}

/// A coroutine body parked at `InitialYield`/`yield`/`await`: everything
/// `JsOps.resume_frame` needs to rebuild the activation from the `Agent`
/// alone. The parking opcode has already applied its pc/stack fixup, so a
/// resume restores these fields verbatim and, unless `at_start`, pushes the
/// sent value for the resumed instruction to consume.
pub type SuspendedFrame {
  SuspendedFrame(
    template: FuncTemplate,
    pc: Int,
    locals: TupleArray(JsVal),
    stack: List(JsVal),
    try_stack: List(TryFrame),
    this: JsVal,
    home_object: JsVal,
    /// Cell id of the frame's sloppy-direct-eval var object, if one exists.
    eval_env: Option(Int),
    /// Source line the body was on when it parked.
    line: Int,
    /// Parked by `InitialYield` before the body proper ran: the first
    /// resumption's sent value is not delivered to the operand stack.
    at_start: Bool,
    /// The activation's argument list. An async function parks at pc 0
    /// before its prologue, so `arguments` / rest are built on resume.
    call_args: List(JsVal),
  )
}
