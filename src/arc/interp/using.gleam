import arc/bytecode/lexical
import arc/bytecode/opcode.{CallMethod, GetLocal, Pop, PushConst, Return}
import arc/internal/tuple_array
import arc/rt/builtins/disposable_stack as b_disposable_stack
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/closure as rt_closure
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, KHandle, KNull, KUndef, NoElements,
  SObject, classify, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{None, Some}

pub fn using_disposer(
  agent: Agent,
  val: JsVal,
  is_async is_async: Bool,
  unit_id unit_id: Int,
) -> #(JsVal, Agent) {
  case classify(val) {
    KUndef | KNull -> #(mk_undefined(), agent)
    KHandle(_) -> {
      let #(method, agent) =
        b_disposable_stack.get_dispose_method(agent, val, is_async:)
      case method {
        b_disposable_stack.DirectDispose(m) -> direct_disposer(agent, m, val)
        b_disposable_stack.SyncFallbackDispose(m) ->
          sync_fallback_disposer(agent, m, val, unit_id)
      }
    }
    _ ->
      rt_val.throw_type_error(
        agent,
        "using declaration initializer is not an object, null, or undefined",
      )
  }
}

// built directly so the method's length/name are never read
fn direct_disposer(
  agent: Agent,
  method: Handle,
  val: JsVal,
) -> #(JsVal, Agent) {
  let #(h, agent) =
    rt_store.cell_new(
      agent,
      SObject(
        kind: types.BoundFn(target: method, bound_this: val, bound_args: []),
        proto: Some(agent.realm.function.prototype),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  #(mk_object(h), agent)
}

fn sync_fallback_disposer(
  agent: Agent,
  method: Handle,
  val: JsVal,
  unit_id: Int,
) -> #(JsVal, Agent) {
  let #(h, agent) =
    rt_closure.new_bytecode_function(
      agent,
      sync_fallback_template(),
      bytecode.env_from_list([mk_object(method), val]),
      unit_id,
    )
  #(mk_object(h), agent)
}

fn sync_fallback_template() -> FuncTemplate {
  bytecode.FuncTemplate(
    name: None,
    arity: 0,
    length: 0,
    local_count: 2,
    bytecode: tuple_array.from_list([
      GetLocal(1),
      GetLocal(0),
      CallMethod(0),
      Pop,
      PushConst(0),
      Return,
    ]),
    constants: tuple_array.from_list([mk_undefined()]),
    lines: tuple_array.from_list([0, 0, 0, 0, 0, 0]),
    functions: tuple_array.from_list([]),
    env_descriptors: [bytecode.CaptureLocal(0), bytecode.CaptureLocal(1)],
    is_strict: True,
    is_arrow: True,
    is_derived_constructor: False,
    is_generator: False,
    is_async: True,
    is_constructor: False,
    is_class_constructor: False,
    local_names: None,
    lexical: lexical.NoLexicalSlots,
    code_kind: lexical.FunctionCode,
    regs: bytecode.NoRegs,
  )
}
