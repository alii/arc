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
