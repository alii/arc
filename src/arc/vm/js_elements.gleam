/// Operations on `JsElements` — dual-representation JS array elements.
///
/// The type itself is defined in `arc/vm/value` (to avoid import cycles).
/// This module provides all operations: new, from_list, get, set, delete, etc.
import arc/vm/array
import arc/vm/value.{
  type JsElements, type JsValue, DenseElements, JsUndefined, SparseElements,
}
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

const max_gap = 1024

/// Empty elements (for non-array objects and empty arrays).
pub fn new() -> JsElements {
  DenseElements(array.from_list([]))
}

/// Build dense elements from a list of values.
pub fn from_list(items: List(JsValue)) -> JsElements {
  DenseElements(array.from_list(items))
}

/// Build sparse elements from #(index, value) pairs. Produces a dict-backed
/// representation where missing indices are treated as holes (return undefined
/// on access). Used for array literals containing elisions (e.g. `[1,,3]`).
pub fn from_indexed(items: List(#(Int, JsValue))) -> JsElements {
  SparseElements(dict.from_list(items))
}

/// Get element at index. Returns JsUndefined for missing/out-of-bounds.
pub fn get(elements: JsElements, index: Int) -> JsValue {
  case elements {
    DenseElements(data) -> array.get(index, data) |> option.unwrap(JsUndefined)
    SparseElements(data) -> dict.get(data, index) |> result.unwrap(JsUndefined)
  }
}

/// Get element as Option (for has_key semantics and property descriptors).
pub fn get_option(elements: JsElements, index: Int) -> Option(JsValue) {
  case elements {
    DenseElements(data) ->
      case index >= 0 && index < array.size(data) {
        True -> array.get(index, data)
        False -> None
      }
    SparseElements(data) ->
      case dict.get(data, index) {
        Ok(val) -> Some(val)
        Error(_) -> None
      }
  }
}

/// Check if an element exists at index.
pub fn has(elements: JsElements, index: Int) -> Bool {
  case elements {
    DenseElements(data) -> index >= 0 && index < array.size(data)
    SparseElements(data) -> dict.has_key(data, index)
  }
}

/// Set element at index. May trigger dense->sparse transition.
pub fn set(elements: JsElements, index: Int, val: JsValue) -> JsElements {
  case elements {
    DenseElements(data) -> {
      let size = array.size(data)
      case index < size {
        True ->
          // In-bounds write
          case array.set(index, val, data) {
            Ok(new_data) -> DenseElements(new_data)
            Error(_) -> elements
          }
        False ->
          // Out-of-bounds — check gap
          case index - size > max_gap {
            True ->
              // Large gap -> convert to sparse
              SparseElements(dense_to_sparse(data) |> dict.insert(index, val))
            False -> {
              // Small gap -> grow tuple, fill with JsUndefined
              let grown = array.grow(data, index + 1, JsUndefined)
              DenseElements(
                array.set(index, val, grown) |> result.unwrap(grown),
              )
            }
          }
      }
    }
    SparseElements(data) -> SparseElements(dict.insert(data, index, val))
  }
}

/// Delete element at index (creates hole).
/// For dense arrays, converts to sparse (delete is rare in normal JS code).
pub fn delete(elements: JsElements, index: Int) -> JsElements {
  case elements {
    DenseElements(data) ->
      SparseElements(dense_to_sparse(data) |> dict.delete(index))
    SparseElements(data) -> SparseElements(dict.delete(data, index))
  }
}

/// Get all values as a list (for GC ref tracing).
pub fn values(elements: JsElements) -> List(JsValue) {
  case elements {
    DenseElements(data) -> array.to_list(data)
    SparseElements(data) -> dict.values(data)
  }
}

/// Number of stored entries. NOT JS .length — use ArrayObject(length:) for that.
pub fn stored_count(elements: JsElements) -> Int {
  case elements {
    DenseElements(data) -> array.size(data)
    SparseElements(data) -> dict.size(data)
  }
}

fn dense_to_sparse(data: array.Array(JsValue)) -> dict.Dict(Int, JsValue) {
  array.to_list(data)
  |> list.index_map(fn(val, idx) { #(idx, val) })
  |> dict.from_list()
}
