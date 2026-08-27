import arc/bytecode/lexical.{type CodeKind, type LexicalSlots}
import arc/bytecode/opcode.{type Op, type Pc, type TryKind}
import arc/internal/tuple_array.{type TupleArray}
import arc/rt/wire.{type JsVal}
import gleam/option.{type Option}

pub type VarEnvKind {
  GlobalVarEnv
  FrameVarEnv
}

pub type EvalNameTable {
  EvalNameTable(var_env: VarEnvKind, names: List(#(String, Int)))
}

pub type EnvCapture {
  CaptureLocal(parent_index: Int)
}

pub fn line_at(template: FuncTemplate, pc: Int) -> Int {
  tuple_array.element(pc + 1, template.lines)
}

pub type FuncTemplate {
  FuncTemplate(
    name: Option(String),
    arity: Int,
    // §15.1.5 the length property value
    length: Int,
    local_count: Int,
    bytecode: TupleArray(Op),
    constants: TupleArray(JsVal),
    // source line per pc, for error.stack
    lines: TupleArray(Int),
    functions: TupleArray(FuncTemplate),
    env_descriptors: List(EnvCapture),
    is_strict: Bool,
    is_arrow: Bool,
    is_derived_constructor: Bool,
    is_generator: Bool,
    is_async: Bool,
    is_constructor: Bool,
    is_class_constructor: Bool,
    // only for functions containing direct eval
    local_names: Option(EvalNameTable),
    lexical: LexicalSlots,
    code_kind: CodeKind,
  )
}

// erlang tuple of captured values, leading run of callee locals
pub type EnvTuple

@external(erlang, "erlang", "list_to_tuple")
pub fn env_from_list(values: List(JsVal)) -> EnvTuple

@external(erlang, "erlang", "tuple_to_list")
pub fn env_to_list(env: EnvTuple) -> List(JsVal)

@external(erlang, "erlang", "tuple_size")
pub fn env_size(env: EnvTuple) -> Int

pub type TryFrame {
  TryFrame(catch_target: Int, stack_depth: Int, kind: TryKind(Pc))
}

pub type ParkedAt {
  // first sent value is not delivered
  ParkedStart
  ParkedOp
  // §27.5.3.8 step 7.c.v
  ParkedDelegateReturn
  // §27.5.3.8 step 7.c.iii.2
  ParkedReturnValue
  // §27.5.3.8 step 7.b.iii
  ParkedDelegateClose
}

pub type SuspendedFrame {
  SuspendedFrame(
    template: FuncTemplate,
    pc: Int,
    locals: TupleArray(JsVal),
    stack: List(JsVal),
    try_stack: List(TryFrame),
    this: JsVal,
    home_object: JsVal,
    eval_env: Option(Int),
    parked: ParkedAt,
    call_args: List(JsVal),
    realm: Int,
    unit: Int,
  )
}
