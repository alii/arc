//// Operations on `JsElements`, the tri-representation array element store
//// (port of `arc/vm/internal/elements.gleam`, the subset the MOP and the
//// buffer family need). The type itself lives in `arc/rt/types`.

import arc/rt/types.{type JsElements, type JsVal, Dense, NoElements, Sparse}
import arc/vm/internal/tree_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option}

/// After this many empty slots between the current dense end and a new
/// index, promote to sparse (arc `elements.gleam:25`).
const max_gap = 1024

/// The FFI `:array` backing tops out here (arc `limits.gleam:27`).
const max_dense_index = 10_000_000

/// Empty elements. Zero allocation — every non-array object starts here.
pub fn new() -> JsElements {
  NoElements
}

/// Dense elements from a list of values; `[]` stays `NoElements`.
pub fn from_list(items: List(JsVal)) -> JsElements {
  case items {
    [] -> NoElements
    _ -> Dense(tree_array.from_list(items, types.mk_undefined()))
  }
}

/// Read element at `i`, `undefined` for a hole or absent index.
pub fn get(elements: JsElements, i: Int) -> JsVal {
  get_option(elements, i) |> option.unwrap(types.mk_undefined())
}

/// Read element at `i`. `None` for a hole or absent index.
pub fn get_option(elements: JsElements, i: Int) -> Option(JsVal) {
  case elements {
    NoElements -> option.None
    Dense(data) -> tree_array.get_option(i, data)
    Sparse(data) -> dict.get(data, i) |> option.from_result
  }
}

/// True when index `i` holds a present element.
pub fn has(elements: JsElements, i: Int) -> Bool {
  option.is_some(get_option(elements, i))
}

/// True when no element is present at any index. O(k) worst case, O(1) for
/// the common NoElements case. Used by the array-mutator fast path to verify
/// prototype-chain objects carry no indexed elements.
pub fn is_empty(elements: JsElements) -> Bool {
  case elements {
    NoElements -> True
    Dense(data) ->
      tree_array.sparse_fold(fn(_i, _v, _acc) { False }, True, data)
    Sparse(data) -> dict.size(data) == 0
  }
}

/// Write `v` at `i`, promoting NoElements to Dense or Dense to Sparse as
/// needed.
pub fn set(elements: JsElements, i: Int, v: JsVal) -> JsElements {
  case elements {
    NoElements -> set(Dense(tree_array.new(types.mk_undefined())), i, v)
    Dense(data) -> {
      let size = tree_array.size(data)
      case i - size > max_gap || i >= max_dense_index {
        True -> Sparse(dense_to_sparse(data) |> dict.insert(i, v))
        False -> Dense(tree_array.set(i, v, data))
      }
    }
    Sparse(data) -> Sparse(dict.insert(data, i, v))
  }
}

/// Delete element at `i` (creates a hole). Stays dense.
pub fn delete(elements: JsElements, i: Int) -> JsElements {
  case elements {
    NoElements -> NoElements
    Dense(data) -> Dense(tree_array.reset(i, data))
    Sparse(data) -> Sparse(dict.delete(data, i))
  }
}

/// Write Some(val) as a present element, None as a hole. Internal helper for
/// the bulk move/reverse operations below, which must preserve holes.
fn put_option(elements: JsElements, i: Int, val: Option(JsVal)) -> JsElements {
  case val {
    option.Some(v) -> set(elements, i, v)
    option.None -> delete(elements, i)
  }
}

/// Move elements in [from, len) by a SIGNED `delta`: negative shifts them
/// toward index 0, positive away from it. Holes are preserved (a hole source
/// deletes the target slot). This is `copy_within` with a computed
/// destination, so the overlapping in-place move picks its own iteration
/// direction from the sign of `delta`. The vacated slots are left untouched;
/// callers truncate to the new length afterwards. Used by the
/// shift/unshift/splice fast paths so the whole move is one heap read + one
/// heap write instead of 3-4 heap ops per element.
pub fn move_range(
  elements: JsElements,
  from: Int,
  len: Int,
  delta: Int,
) -> JsElements {
  copy_within(elements, from, from + delta, len - from)
}

/// Reverse elements [0, len) in place, holes included.
pub fn reverse_range(elements: JsElements, len: Int) -> JsElements {
  reverse_loop(elements, 0, len - 1)
}

fn reverse_loop(elements: JsElements, lo: Int, hi: Int) -> JsElements {
  case lo >= hi {
    True -> elements
    False -> {
      let lo_val = get_option(elements, lo)
      let hi_val = get_option(elements, hi)
      let elements = put_option(elements, lo, hi_val) |> put_option(hi, lo_val)
      reverse_loop(elements, lo + 1, hi - 1)
    }
  }
}

/// Set every index in [start, end) to `val` (fills holes with own elements,
/// matching the spec's per-index Set on the no-overrides fast path).
pub fn fill_range(
  elements: JsElements,
  start: Int,
  end: Int,
  val: JsVal,
) -> JsElements {
  case start >= end {
    True -> elements
    False -> fill_range(set(elements, start, val), start + 1, end, val)
  }
}

/// Copy [from, from + count) onto [to, to + count), holes preserved. Picks
/// the iteration direction so overlapping ranges copy correctly (same trick
/// as memmove). Used by the copyWithin fast path.
pub fn copy_within(
  elements: JsElements,
  from: Int,
  to: Int,
  count: Int,
) -> JsElements {
  case from < to {
    True -> copy_backward(elements, from + count - 1, to + count - 1, count)
    False -> copy_forward(elements, from, to, count)
  }
}

fn copy_forward(
  elements: JsElements,
  from: Int,
  to: Int,
  remaining: Int,
) -> JsElements {
  case remaining <= 0 {
    True -> elements
    False ->
      copy_forward(
        put_option(elements, to, get_option(elements, from)),
        from + 1,
        to + 1,
        remaining - 1,
      )
  }
}

fn copy_backward(
  elements: JsElements,
  from: Int,
  to: Int,
  remaining: Int,
) -> JsElements {
  case remaining <= 0 {
    True -> elements
    False ->
      copy_backward(
        put_option(elements, to, get_option(elements, from)),
        from - 1,
        to - 1,
        remaining - 1,
      )
  }
}

/// Write `vals` at consecutive indices starting at `i`.
pub fn write_list(
  elements: JsElements,
  i: Int,
  vals: List(JsVal),
) -> JsElements {
  case vals {
    [] -> elements
    [v, ..rest] -> write_list(set(elements, i, v), i + 1, rest)
  }
}

/// Present indices in ascending order. Skips holes.
pub fn indices(elements: JsElements) -> List(Int) {
  case elements {
    NoElements -> []
    Dense(data) ->
      tree_array.sparse_fold(fn(i, _v, acc) { [i, ..acc] }, [], data)
      |> list.reverse
    Sparse(data) -> dict.keys(data) |> list.sort(int.compare)
  }
}

/// Drop every element at index >= `new_len`.
pub fn truncate(elements: JsElements, new_len: Int) -> JsElements {
  case elements {
    NoElements -> NoElements
    Dense(data) ->
      case new_len >= tree_array.size(data) {
        True -> elements
        False -> Dense(tree_array.resize(data, new_len))
      }
    Sparse(data) -> Sparse(dict.filter(data, fn(idx, _v) { idx < new_len }))
  }
}

fn dense_to_sparse(data: tree_array.TreeArray(JsVal)) -> Dict(Int, JsVal) {
  tree_array.sparse_fold(
    fn(i, v, acc) { dict.insert(acc, i, v) },
    dict.new(),
    data,
  )
}
