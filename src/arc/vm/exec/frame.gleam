//// Frame-setup helpers shared by `call.gleam` and `event_loop.gleam`.
//// Lives in its own leaf module so both can import it without creating an
//// import cycle (call → realm → event_loop).

import arc/vm/builtins/common
import arc/vm/heap
import arc/vm/internal/tuple_array
import arc/vm/opcode
import arc/vm/state.{type Heap, type State}
import arc/vm/value.{
  type FuncTemplate, type JsValue, JsNull, JsObject, JsUndefined,
}
import gleam/option.{type Option, None, Some}

/// Resolve `this` for a function call per ES2024 §10.2.1.2 OrdinaryCallBindThis.
/// Computes the value to write into the callee's lexical-`this` slot
/// (FuncTemplate.this_slot). Arrows don't own a slot — the value returned
/// for them is unused; they read `this` via a capture from the enclosing
/// non-arrow.
pub fn bind_this(
  state: State,
  callee: FuncTemplate,
  this_arg: JsValue,
) -> #(Heap, JsValue) {
  case callee.is_arrow {
    // Step 2: thisMode is LEXICAL → arrows have no own `this` binding.
    // Their `this` reads resolve to a capture from the enclosing non-arrow,
    // so the value computed here is never written anywhere.
    True -> #(state.heap, JsUndefined)
    False ->
      case callee.is_strict {
        // Step 5: thisMode is STRICT -> thisValue = thisArgument (no coercion).
        True -> #(state.heap, this_arg)
        // Step 6: Sloppy mode coercion.
        False ->
          case this_arg {
            // Step 6a: undefined/null -> globalThis.
            JsUndefined | JsNull -> #(
              state.heap,
              JsObject(state.ctx.global_object),
            )
            // Step 6b: Objects pass through (ToObject is identity for objects).
            JsObject(_) -> #(state.heap, this_arg)
            _ ->
              // Step 6b: Primitives -> ToObject wrapper (boxing).
              // to_object only errors on null/undefined which we handled above.
              case common.to_object(state.heap, state.builtins, this_arg) {
                Some(#(heap, ref)) -> #(heap, JsObject(ref))
                None -> #(state.heap, this_arg)
              }
          }
      }
  }
}

/// Set up locals for a function call:
/// [env_values, lexical_seeds, args(padded to arity), undefined×remaining].
/// Non-arrows own slots for the lexical pseudo-bindings immediately after
/// captures, in canonical `all_lexical_refs` order; write the seed values
/// there. Arrows inherit lexicals via env_values (their lexical slots, if
/// Some, point at captures) so skip.
///
/// Hot path: every JS call goes through here. The tuple is built in one
/// forward pass via FFI (no list.append/reverse/intermediate accumulator).
pub fn setup_locals(
  h: Heap,
  env_ref: value.Ref,
  fn_ref: value.Ref,
  home_object: Option(value.Ref),
  callee_template: FuncTemplate,
  args: List(JsValue),
  this_val: JsValue,
  new_target: JsValue,
) -> tuple_array.TupleArray(JsValue) {
  // Capture-less templates always point at an empty EnvSlot — skip the
  // heap dict lookup entirely (the common case for top-level functions
  // and tiny callbacks).
  let env_values = case callee_template.env_descriptors {
    [] -> []
    _has_captures -> heap.read_env(h, env_ref) |> option.unwrap([])
  }
  case callee_template.is_arrow {
    // Arrows own no lexical slots — their `this`/`new.target` reads resolve
    // to captures, so there are no seeds to write.
    True ->
      setup_locals_tuple(
        env_values,
        [],
        args,
        callee_template.arity,
        callee_template.local_count,
        JsUndefined,
      )
    False -> {
      let home = option.map(home_object, JsObject) |> option.unwrap(JsUndefined)
      setup_locals_seeded(
        env_values,
        callee_template.lexical,
        this_val,
        JsObject(fn_ref),
        home,
        new_target,
        args,
        callee_template.arity,
        callee_template.local_count,
        JsUndefined,
      )
    }
  }
}

/// FFI: build the locals tuple in one forward pass — see
/// arc_vm_ffi:setup_locals_tuple/6. Body-recursive on the Erlang side so
/// the result list is built in order (no reverse) then list_to_tuple'd.
@external(erlang, "arc_vm_ffi", "setup_locals_tuple")
fn setup_locals_tuple(
  env: List(JsValue),
  seeds: List(JsValue),
  args: List(JsValue),
  arity: Int,
  local_count: Int,
  undef: JsValue,
) -> tuple_array.TupleArray(JsValue)

/// FFI: non-arrow locals build — see arc_vm_ffi:setup_locals_seeded/10.
/// Seed values for the owned lexical slots are written in canonical
/// `all_lexical_refs` order ([this, active_func, home_object, new_target]),
/// one per Some entry in `lexical`. The emitter guarantees slot indices
/// start at len(captures) and run contiguously in this order. Passing the
/// LexicalSlots record plus the four values lets the FFI write the common
/// all-four-owned case inline with no intermediate seeds list.
@external(erlang, "arc_vm_ffi", "setup_locals_seeded")
fn setup_locals_seeded(
  env: List(JsValue),
  lexical: opcode.LexicalSlots,
  this_val: JsValue,
  fn_obj: JsValue,
  home: JsValue,
  new_target: JsValue,
  args: List(JsValue),
  arity: Int,
  local_count: Int,
  undef: JsValue,
) -> tuple_array.TupleArray(JsValue)

/// FFI: a one-instruction `Return` code array used as a sentinel frame for
/// natives driven outside the interpreter loop — see
/// arc_vm_ffi:return_code_sentinel/0.
@external(erlang, "arc_vm_ffi", "return_code_sentinel")
pub fn return_code_sentinel() -> tuple_array.TupleArray(opcode.Op)
