import arc/vm/builtins/common
import arc/vm/frame.{type State, State}
import arc/vm/heap.{type Heap}
import arc/vm/js_elements
import arc/vm/value.{
  type JsValue, type Ref, JsObject, JsString, JsUndefined, NativeArcPeek,
  ObjectSlot, OrdinaryObject, PromiseFulfilled, PromiseObject, PromisePending,
  PromiseRejected, PromiseSlot,
}
import gleam/dict
import gleam/option.{None, Some}

/// Non-standard: Set up the Arc global namespace object.
/// Arc is an engine-specific namespace (like Math) with debugging utilities.
/// Not part of any ECMAScript specification.
pub fn init(h: Heap, object_proto: Ref, function_proto: Ref) -> #(Heap, Ref) {
  let #(h, methods) =
    common.alloc_methods(h, function_proto, [#("peek", NativeArcPeek, 1)])

  let properties = dict.from_list(methods)
  let symbol_properties =
    dict.from_list([
      #(
        value.symbol_to_string_tag,
        value.data(JsString("Arc")) |> value.configurable(),
      ),
    ])

  let #(h, arc_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties:,
        elements: js_elements.new(),
        prototype: Some(object_proto),
        symbol_properties:,
        extensible: True,
      ),
    )
  let h = heap.root(h, arc_ref)

  #(h, arc_ref)
}

/// Non-standard: Arc.peek(promise)
/// Returns {type: 'pending'} | {type: 'resolved', value} | {type: 'rejected', reason}
///
/// Synchronously inspects a promise's internal [[PromiseState]] and
/// [[PromiseResult]] slots without awaiting. Similar in spirit to V8's
/// %PromiseStatus/%PromiseResult debug intrinsics or Node's
/// `util.inspect` promise output, but exposed as a first-class API.
///
/// Throws TypeError if the argument is not a Promise.
pub fn peek(
  args: List(JsValue),
  state: State,
  object_proto: Ref,
) -> #(State, Result(JsValue, JsValue)) {
  let arg = case args {
    [a, ..] -> a
    [] -> JsUndefined
  }

  case read_promise_state(state.heap, arg) {
    Some(promise_state) -> {
      let props = case promise_state {
        PromisePending -> [#("type", value.data_property(JsString("pending")))]
        PromiseFulfilled(value:) -> [
          #("type", value.data_property(JsString("resolved"))),
          #("value", value.data_property(value)),
        ]
        PromiseRejected(reason:) -> [
          #("type", value.data_property(JsString("rejected"))),
          #("reason", value.data_property(reason)),
        ]
      }
      let #(heap, result_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.from_list(props),
            symbol_properties: dict.new(),
            elements: js_elements.new(),
            prototype: Some(object_proto),
            extensible: True,
          ),
        )
      #(State(..state, heap:), Ok(JsObject(result_ref)))
    }
    None -> {
      let #(heap, err) =
        common.make_type_error(
          state.heap,
          state.builtins,
          "Arc.peek: argument is not a Promise",
        )
      #(State(..state, heap:), Error(err))
    }
  }
}

/// Non-standard: Read the internal [[PromiseState]] of a JS value if it's a
/// promise. Traverses the PromiseObject kind to find the PromiseSlot data.
fn read_promise_state(
  h: Heap,
  val: JsValue,
) -> option.Option(value.PromiseState) {
  use ref <- option.then(case val {
    JsObject(r) -> Some(r)
    _ -> None
  })
  use data_ref <- option.then(case heap.read(h, ref) {
    Some(ObjectSlot(kind: PromiseObject(promise_data:), ..)) ->
      Some(promise_data)
    _ -> None
  })
  case heap.read(h, data_ref) {
    Some(PromiseSlot(state:, ..)) -> Some(state)
    _ -> None
  }
}
