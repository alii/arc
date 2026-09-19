import arc/bytecode/lexical
import arc/bytecode/opcode.{CallMethod, GetLocal, Pop, PushConst, Return}
import arc/internal/tuple_array
import arc/rt/builtins/disposable_stack as b_disposable_stack
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/closure as rt_closure
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, KHandle, KNull, KUndef, classify,
  mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/option.{None, Some}

pub fn using_disposer(
  st: Agent,
  val: JsVal,
  is_async is_async: Bool,
  unit_id unit_id: Int,
) -> #(JsVal, Agent) {
  case classify(val) {
    KUndef | KNull -> #(mk_undefined(), st)
    KHandle(_) -> {
      let #(method, st) =
        b_disposable_stack.get_dispose_method(st, val, is_async:)
      case method {
        b_disposable_stack.DirectDispose(m) -> bound_disposer(st, m, val)
        b_disposable_stack.SyncFallbackDispose(m) ->
          sync_fallback_disposer(st, m, val, unit_id)
      }
    }
    _ ->
      rt_val.throw_type_error(
        st,
        "using declaration initializer is not an object, null, or undefined",
      )
  }
}

// built directly so the method's length/name are never read
fn bound_disposer(st: Agent, method: Handle, val: JsVal) -> #(JsVal, Agent) {
  let kind = types.BoundFn(target: method, bound_this: val, bound_args: [])
  let proto = Some(st.realm.function.prototype)
  let #(h, st) =
    rt_store.cell_new(st, types.plain_object(kind, proto, dict.new()))
  #(mk_object(h), st)
}

fn sync_fallback_disposer(
  st: Agent,
  method: Handle,
  val: JsVal,
  unit_id: Int,
) -> #(JsVal, Agent) {
  let #(h, st) =
    rt_closure.new_bytecode_function(
      st,
      sync_fallback_template(),
      bytecode.env_from_list([mk_object(method), val]),
      unit_id,
    )
  #(mk_object(h), st)
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
