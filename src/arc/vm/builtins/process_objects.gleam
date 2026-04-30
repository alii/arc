//// Low-level Pid object allocation.
////
//// Kept separate from `arc/beam` and `structured_clone` so both can depend
//// on it without forming an import cycle (deserialize → alloc_pid_object,
//// beam.spawn/self → alloc_pid_object, beam → structured_clone).

import arc/vm/builtins/common
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type JsValue, type Ref, JsObject, JsString, ObjectSlot, PidObject,
}
import gleam/option.{None, Some}

/// Returns the pid in the format `<x.x.x>
@external(erlang, "arc_vm_ffi", "pid_to_string")
pub fn ffi_pid_to_string(pid: value.ErlangPid) -> String

/// Allocate a PidObject on the heap wrapping an Erlang PID.
pub fn alloc_pid_object(
  heap: Heap,
  object_proto: Ref,
  function_proto: Ref,
  pid: value.ErlangPid,
) -> #(Heap, JsValue) {
  let #(heap, to_string_ref) =
    common.alloc_host_fn(heap, function_proto, pid_to_string, "toString", 0)
  let #(heap, ref) =
    heap.alloc(
      heap,
      ObjectSlot(
        kind: PidObject(pid:),
        properties: common.named_props([
          #("toString", value.builtin_property(JsObject(to_string_ref))),
        ]),
        elements: elements.new(),
        prototype: Some(object_proto),
        symbol_properties: [common.to_string_tag("Pid")],
        extensible: True,
      ),
    )
  #(heap, JsObject(ref))
}

/// Pid toString — returns "Pid<0.83.0>" when called on a PidObject.
fn pid_to_string(
  _args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(State, Result(JsValue, JsValue)) {
  case this {
    JsObject(ref) ->
      case heap.read_pid(state.heap, ref) {
        Some(pid) -> #(state, Ok(JsString("Pid" <> ffi_pid_to_string(pid))))
        None -> state.type_error(state, "Dead Pid")
      }
    _ -> state.type_error(state, "Invalid Pid object")
  }
}
