/// Operations on `JsElements` — tri-representation JS array elements.
///
/// The type itself is defined in `arc/vm/value` (to avoid import cycles).
/// This module provides all operations: new, from_list, get, set, delete, etc.
///
/// DenseElements uses JsUninitialized as its tree_array default so holes
/// (deleted/unset slots) are distinguishable from explicit `arr[i]=undefined`.
/// The sentinel never leaks: get/get_option convert it to JsUndefined/None.
import arc/vm/internal/tree_array
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
      case index - size > max_gap {
        True ->
          // Large gap → sparse. Dense-with-holes would waste memory.
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

/// Number of stored entries. NOT JS .length — use ArrayObject(length:) for that.
pub fn stored_count(elements: JsElements) -> Int {
  case elements {
    NoElements -> 0
    DenseElements(data) -> tree_array.size(data)
    SparseElements(data) -> dict.size(data)
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
