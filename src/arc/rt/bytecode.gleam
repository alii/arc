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

pub type FnFlags {
  FnFlags(
    is_constructor: Bool,
    is_class_constructor: Bool,
    is_derived_constructor: Bool,
    is_arrow: Bool,
    is_generator: Bool,
    is_async: Bool,
    is_strict: Bool,
  )
}

pub const function_sloppy = FnFlags(
  is_constructor: True,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: False,
  is_async: False,
  is_strict: False,
)

pub const function_strict = FnFlags(
  is_constructor: True,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: False,
  is_async: False,
  is_strict: True,
)

pub const method_sloppy = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: False,
  is_async: False,
  is_strict: False,
)

pub const method_strict = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: False,
  is_async: False,
  is_strict: True,
)

pub const arrow_sloppy = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: True,
  is_generator: False,
  is_async: False,
  is_strict: False,
)

pub const arrow_strict = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: True,
  is_generator: False,
  is_async: False,
  is_strict: True,
)

pub const async_arrow_sloppy = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: True,
  is_generator: False,
  is_async: True,
  is_strict: False,
)

pub const async_arrow_strict = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: True,
  is_generator: False,
  is_async: True,
  is_strict: True,
)

pub const async_function_sloppy = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: False,
  is_async: True,
  is_strict: False,
)

pub const async_function_strict = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: False,
  is_async: True,
  is_strict: True,
)

pub const generator_sloppy = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: True,
  is_async: False,
  is_strict: False,
)

pub const generator_strict = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: True,
  is_async: False,
  is_strict: True,
)

pub const async_generator_sloppy = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: True,
  is_async: True,
  is_strict: False,
)

pub const async_generator_strict = FnFlags(
  is_constructor: False,
  is_class_constructor: False,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: True,
  is_async: True,
  is_strict: True,
)

pub const class_constructor = FnFlags(
  is_constructor: True,
  is_class_constructor: True,
  is_derived_constructor: False,
  is_arrow: False,
  is_generator: False,
  is_async: False,
  is_strict: True,
)

pub const derived_class_constructor = FnFlags(
  is_constructor: True,
  is_class_constructor: True,
  is_derived_constructor: True,
  is_arrow: False,
  is_generator: False,
  is_async: False,
  is_strict: True,
)

// the shared constant for these flags
pub fn flags(
  is_constructor is_constructor: Bool,
  is_class_constructor is_class_constructor: Bool,
  is_derived_constructor is_derived_constructor: Bool,
  is_arrow is_arrow: Bool,
  is_generator is_generator: Bool,
  is_async is_async: Bool,
  is_strict is_strict: Bool,
) -> FnFlags {
  case
    is_constructor,
    is_class_constructor,
    is_derived_constructor,
    is_arrow,
    is_generator,
    is_async,
    is_strict
  {
    True, False, False, False, False, False, False -> function_sloppy
    True, False, False, False, False, False, True -> function_strict
    False, False, False, False, False, False, False -> method_sloppy
    False, False, False, False, False, False, True -> method_strict
    False, False, False, True, False, False, False -> arrow_sloppy
    False, False, False, True, False, False, True -> arrow_strict
    False, False, False, True, False, True, False -> async_arrow_sloppy
    False, False, False, True, False, True, True -> async_arrow_strict
    False, False, False, False, False, True, False -> async_function_sloppy
    False, False, False, False, False, True, True -> async_function_strict
    False, False, False, False, True, False, False -> generator_sloppy
    False, False, False, False, True, False, True -> generator_strict
    False, False, False, False, True, True, False -> async_generator_sloppy
    False, False, False, False, True, True, True -> async_generator_strict
    True, True, False, False, False, False, True -> class_constructor
    True, True, True, False, False, False, True -> derived_class_constructor
    _, _, _, _, _, _, _ ->
      FnFlags(
        is_constructor:,
        is_class_constructor:,
        is_derived_constructor:,
        is_arrow:,
        is_generator:,
        is_async:,
        is_strict:,
      )
  }
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
    // for each captured value, in env order, the local it has in the parent
    env_descriptors: List(Int),
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
    regs: Regs,
  )
}

// erlang tuple of captured values, leading run of callee locals
pub type EnvTuple

@external(erlang, "erlang", "list_to_tuple")
pub fn env_from_list(values: List(JsVal)) -> EnvTuple

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
    unit_id: Int,
  )
}

// loop-hot slots the interpreter keeps in loop arguments
pub type Regs {
  NoRegs
  Regs(a: Int, b: Int)
}

// Regs.b when only one slot was picked
pub const no_register = -1

// negative op slot operands name a register instead of a local
pub const reg_a_slot = -1

pub const reg_b_slot = -2

// a negative retpc makes Ret return the value under it
pub const return_retpc = -1
