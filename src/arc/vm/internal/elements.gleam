/// Operations on `JsElements` — tri-representation JS array elements.
///
/// The type itself is defined in `arc/vm/value` (to avoid import cycles).
/// This module provides all operations: new, from_list, get, set, delete, etc.
///
/// DenseElements uses JsUninitialized as its tree_array default so holes
/// (deleted/unset slots) are distinguishable from explicit `arr[i]=undefined`.
/// The sentinel never leaks: get/get_option convert it to JsUndefined/None.
import arc/vm/internal/tree_array
import arc/vm/limits
import arc/vm/value.{
  type JsElements, type JsValue, DenseElements, JsUndefined, JsUninitialized,
  NoElements, SparseElements,
}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

const max_gap = 1024

/// Empty elements. Zero allocation — every non-array object starts here.
pub fn new() -> JsElements {
  NoElements
}

/// Build dense elements from a list of values.
pub fn from_list(items: List(JsValue)) -> JsElements {
  DenseElements(tree_array.from_list(items, JsUninitialized))
}

/// Build sparse elements from #(index, value) pairs. Produces a dict-backed
/// representation where missing indices are treated as holes (return undefined
/// on access). Used for array literals containing elisions (e.g. `[1,,3]`).
pub fn from_indexed(items: List(#(Int, JsValue))) -> JsElements {
  SparseElements(dict.from_list(items))
}

/// Get element at index. Returns JsUndefined for missing/out-of-bounds.
pub fn get(elements: JsElements, index: Int) -> JsValue {
  get_option(elements, index) |> option.unwrap(JsUndefined)
}

/// Get element as Option (for has_key semantics and property descriptors).
pub fn get_option(elements: JsElements, index: Int) -> Option(JsValue) {
  case elements {
    NoElements -> None
    DenseElements(data) -> tree_array.get_option(index, data)
    SparseElements(data) ->
      case dict.get(data, index) {
        Ok(val) -> Some(val)
        Error(Nil) -> None
      }
  }
}

/// Check if an element exists at index.
pub fn has(elements: JsElements, index: Int) -> Bool {
  case elements {
    NoElements -> False
    DenseElements(data) -> option.is_some(tree_array.get_option(index, data))
    SparseElements(data) -> dict.has_key(data, index)
  }
}

/// Set element at index. May promote NoElements→Dense or Dense→Sparse.
pub fn set(elements: JsElements, index: Int, val: JsValue) -> JsElements {
  case elements {
    NoElements ->
      // Promote to dense and re-dispatch so large-gap check applies.
      set(DenseElements(tree_array.new(JsUninitialized)), index, val)
    DenseElements(data) -> {
      let size = tree_array.size(data)
      case index - size > max_gap || index >= limits.max_dense_index {
        True ->
          // Large gap (dense-with-holes would waste memory) or index past
          // the dense cap (the FFI's :array backing tops out at
          // limits.max_dense_index) → sparse. This is the ONLY gate in
          // front of tree_array.set, so every index that reaches it is
          // < max_dense_index by construction — the FFI never has to
          // discard an out-of-range write.
          SparseElements(dense_to_sparse(data) |> dict.insert(index, val))
        False ->
          // tree_array.set is O(log n) and auto-grows.
          DenseElements(tree_array.set(index, val, data))
      }
    }
    SparseElements(data) -> SparseElements(dict.insert(data, index, val))
  }
}

/// Delete element at index (creates hole). Stays dense — :array natively
/// supports holes via reset. O(log n) vs the old O(n) dense→sparse copy.
pub fn delete(elements: JsElements, index: Int) -> JsElements {
  case elements {
    NoElements -> NoElements
    DenseElements(data) -> DenseElements(tree_array.reset(index, data))
    SparseElements(data) -> SparseElements(dict.delete(data, index))
  }
}

/// Read elements 0..length into a list. Holes/missing slots become
/// JsUndefined (via `get`), so the result always has exactly `length` items.
pub fn to_list_padded(elements: JsElements, length: Int) -> List(JsValue) {
  to_list_padded_loop(elements, 0, length, [])
}

fn to_list_padded_loop(
  elements: JsElements,
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False ->
      to_list_padded_loop(elements, idx + 1, length, [get(elements, idx), ..acc])
  }
}

/// Get all values as a list (for GC ref tracing).
pub fn values(elements: JsElements) -> List(JsValue) {
  case elements {
    NoElements -> []
    DenseElements(data) -> tree_array.to_list(data)
    SparseElements(data) -> dict.values(data)
  }
}

/// Present indices in ascending order. Skips holes. O(k) for dense (via
/// sparse_fold), O(k log k) for sparse (dict.keys + sort). Use this instead
/// of probing 0..length when iterating — critical for sparse arrays where
/// length can be billions but k is small.
pub fn indices(elements: JsElements) -> List(Int) {
  case elements {
    NoElements -> []
    DenseElements(data) ->
      tree_array.sparse_fold(fn(i, _v, acc) { [i, ..acc] }, [], data)
      |> list.reverse
    SparseElements(data) -> dict.keys(data) |> list.sort(int.compare)
  }
}

/// True when no element is present at any index. O(k) worst case, O(1) for
/// the common NoElements case. Used by the array-mutator fast path to verify
/// prototype-chain objects carry no indexed elements.
pub fn is_empty(elements: JsElements) -> Bool {
  case elements {
    NoElements -> True
    DenseElements(data) ->
      tree_array.sparse_fold(fn(_i, _v, _acc) { False }, True, data)
    SparseElements(data) -> dict.size(data) == 0
  }
}

/// Write Some(val) as a present element, None as a hole. Internal helper for
/// the bulk move/reverse operations below, which must preserve holes.
fn put_option(
  elements: JsElements,
  index: Int,
  val: Option(JsValue),
) -> JsElements {
  case val {
    Some(v) -> set(elements, index, v)
    None -> delete(elements, index)
  }
}

/// Move elements in [from, len) down by `delta` positions (toward index 0),
/// preserving holes (a hole source deletes the target slot). Iterates
/// ascending so the overlapping in-place move is safe (delta > 0). The
/// vacated trailing slots [len - delta, len) are left untouched — callers
/// truncate to the new length afterwards. Pure JsElements transformation:
/// used by the Array.prototype shift/splice fast path so the whole move is
/// one heap read + one heap write instead of 3-4 heap ops per element.
pub fn move_down(
  elements: JsElements,
  from: Int,
  len: Int,
  delta: Int,
) -> JsElements {
  case from >= len {
    True -> elements
    False ->
      move_down(
        put_option(elements, from - delta, get_option(elements, from)),
        from + 1,
        len,
        delta,
      )
  }
}

/// Move elements in [from, len) up by `delta` positions (away from index 0),
/// preserving holes. Iterates descending so the overlapping in-place move is
/// safe (delta > 0). Used by the unshift/splice-insert fast path.
pub fn move_up(
  elements: JsElements,
  from: Int,
  len: Int,
  delta: Int,
) -> JsElements {
  move_up_loop(elements, len - 1, from, delta)
}

fn move_up_loop(
  elements: JsElements,
  idx: Int,
  from: Int,
  delta: Int,
) -> JsElements {
  case idx < from {
    True -> elements
    False ->
      move_up_loop(
        put_option(elements, idx + delta, get_option(elements, idx)),
        idx - 1,
        from,
        delta,
      )
  }
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
  val: JsValue,
) -> JsElements {
  case start >= end {
    True -> elements
    False -> fill_range(set(elements, start, val), start + 1, end, val)
  }
}

/// Copy [from, from + count) onto [to, to + count), holes preserved.
/// Picks the iteration direction so overlapping ranges copy correctly
/// (same trick as memmove). Used by the copyWithin fast path.
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

/// Write `vals` at consecutive indices starting at `idx`.
pub fn write_list(
  elements: JsElements,
  idx: Int,
  vals: List(JsValue),
) -> JsElements {
  case vals {
    [] -> elements
    [v, ..rest] -> write_list(set(elements, idx, v), idx + 1, rest)
  }
}

/// Remove all elements at indices >= new_len. O(log n).
pub fn truncate(elements: JsElements, new_len: Int) -> JsElements {
  case elements {
    NoElements -> NoElements
    DenseElements(data) ->
      case new_len >= tree_array.size(data) {
        True -> elements
        False -> DenseElements(tree_array.resize(data, new_len))
      }
    SparseElements(data) ->
      SparseElements(dict.filter(data, fn(idx, _val) { idx < new_len }))
  }
}

fn dense_to_sparse(
  data: tree_array.TreeArray(JsValue),
) -> dict.Dict(Int, JsValue) {
  // sparse_fold skips hole slots (JsUninitialized default), so holes are
  // dropped during promotion — they become missing dict keys, as intended.
  tree_array.sparse_fold(
    fn(i, v, acc) { dict.insert(acc, i, v) },
    dict.new(),
    data,
  )
}
