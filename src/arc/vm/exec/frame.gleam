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
  JsUninitialized,
}
import gleam/option.{type Option, None, Some}

/// Build the callee's frame: bind `this` per ES2024 §10.2.1.2
/// OrdinaryCallBindThis, then lay out its locals tuple
/// [env_values, lexical_seeds, args(padded to arity), undefined×remaining].
///
/// One entry point rather than the old `bind_this` + `setup_locals` pair:
/// those had an unenforceable ordering contract (the raw `this_arg` had to be
/// run through `bind_this` before reaching `setup_locals`) and each branched
/// on `is_arrow` separately. Here `is_arrow` is decided ONCE and the bound
/// `this` never escapes, so "called setup_locals with an unbound this" is not
/// something a caller can express.
///
/// Non-arrows own slots for the lexical pseudo-bindings immediately after
/// captures, in canonical `all_lexical_refs` order; the seed values are
/// written there. Arrows own no lexical slots (their `this` / `new.target`
/// reads resolve to captures already present in env_values), so nothing is
/// bound or seeded for them.
///
/// Hot path: every JS call goes through here. The tuple is built in one
/// forward pass via FFI (no list.append/reverse/intermediate accumulator).
pub fn setup_frame(
  state: State(host),
  env_ref: value.Ref,
  fn_ref: value.Ref,
  home_object: Option(value.Ref),
  callee_template: FuncTemplate,
  args: List(JsValue),
  this_arg: JsValue,
  new_target: JsValue,
) -> #(Heap(host), tuple_array.TupleArray(JsValue)) {
  // Capture-less templates always point at an empty EnvSlot — skip the
  // heap dict lookup entirely (the common case for top-level functions
  // and tiny callbacks).
  let env_values = case callee_template.env_descriptors {
    [] -> []
    _has_captures -> heap.read_env(state.heap, env_ref) |> option.unwrap([])
  }
  case callee_template.is_arrow {
    // §10.2.1.2 step 2: thisMode is LEXICAL → arrows have no own `this`
    // binding, so there is nothing to bind and no seed to write.
    True -> #(
      state.heap,
      setup_locals_tuple(
        env_values,
        [],
        args,
        callee_template.arity,
        callee_template.local_count,
        JsUndefined,
      ),
    )
    False -> {
      let #(heap, this_val) = bind_this(state, callee_template, this_arg)
      // Plain `case` (not option.map |> option.unwrap) — avoids allocating
      // a fun for the constructor per call on this hot path.
      let home = case home_object {
        Some(ref) -> JsObject(ref)
        None -> JsUndefined
      }
      #(
        heap,
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
        ),
      )
    }
  }
}

/// §10.2.1.2 OrdinaryCallBindThis for a NON-arrow callee: the value to write
/// into the callee's lexical-`this` slot. Private — only `setup_frame` may
/// call it, and only on the non-arrow path.
fn bind_this(
  state: State(host),
  callee: FuncTemplate,
  this_arg: JsValue,
) -> #(Heap(host), JsValue) {
  case callee.is_strict {
    // Step 5: thisMode is STRICT -> thisValue = thisArgument (no coercion).
    True -> #(state.heap, this_arg)
    // Step 6: Sloppy mode coercion.
    False ->
      case this_arg {
        // Step 6a: undefined/null -> globalThis.
        JsUndefined | JsNull -> #(state.heap, JsObject(state.ctx.global_object))
        // Step 6b: Objects pass through (ToObject is identity for objects).
        JsObject(_) -> #(state.heap, this_arg)
        // `JsUninitialized` is the TDZ sentinel, never a JS value: it is
        // the OTHER input `common.to_object` rejects, and it must be
        // matched here rather than falling into the `_` arm below — where
        // the None branch would silently hand the sentinel back as `this`
        // and let it escape into user code.
        JsUninitialized ->
          panic as "TDZ sentinel escaped as `this` in bind_this"
        // Step 6b: Primitives -> ToObject wrapper (boxing). Every
        // remaining variant (string/number/bool/symbol/bigint) boxes, so
        // `to_object` cannot fail — a None here would be an engine bug.
        _ -> {
          let assert Some(#(heap, ref)) =
            common.to_object(state.heap, state.builtins, this_arg)
            as "to_object failed on a boxable primitive"
          #(heap, JsObject(ref))
        }
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
/// `all_lexical_refs` order ([this, active_func, home_object, new_target]).
/// `opcode.OwnedLexicalSlots` makes that contiguous canonical layout a fact
/// of the type, so the FFI's fast clause writes the four seeds inline right
/// after the env values with no intermediate seeds list.
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
