import arc/internal/tree_array
import arc/rt/types.{type JsElements, type JsVal, Dense, NoElements, Sparse}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option}

// max hole run before dense promotes to sparse
const max_gap = 1024

// ffi :array backing tops out here
const max_dense_index = 10_000_000

pub fn new() -> JsElements {
  NoElements
}

pub fn from_list(items: List(JsVal)) -> JsElements {
  case items {
    [] -> NoElements
    _ -> Dense(tree_array.from_list(items))
  }
}

pub fn get(elements: JsElements, i: Int) -> JsVal {
  get_option(elements, i) |> option.unwrap(types.mk_undefined())
}

pub fn get_option(elements: JsElements, i: Int) -> Option(JsVal) {
  case elements {
    NoElements -> option.None
    Dense(data) -> tree_array.get_option(i, data)
    Sparse(data) -> dict.get(data, i) |> option.from_result
  }
}

pub fn has(elements: JsElements, i: Int) -> Bool {
  option.is_some(get_option(elements, i))
}

pub fn is_empty(elements: JsElements) -> Bool {
  case elements {
    NoElements -> True
    Dense(data) ->
      tree_array.sparse_fold(fn(_i, _v, _acc) { False }, True, data)
    Sparse(data) -> dict.size(data) == 0
  }
}

pub fn set(elements: JsElements, i: Int, v: JsVal) -> JsElements {
  case elements {
    NoElements -> set(Dense(tree_array.new()), i, v)
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

pub fn delete(elements: JsElements, i: Int) -> JsElements {
  case elements {
    NoElements -> NoElements
    Dense(data) -> Dense(tree_array.reset(i, data))
    Sparse(data) -> Sparse(dict.delete(data, i))
  }
}

fn put_option(elements: JsElements, i: Int, val: Option(JsVal)) -> JsElements {
  case val {
    option.Some(v) -> set(elements, i, v)
    option.None -> delete(elements, i)
  }
}

// signed delta, holes preserved, caller truncates after
pub fn move_range(
  elements: JsElements,
  from: Int,
  len: Int,
  delta: Int,
) -> JsElements {
  copy_within(elements, from, from + delta, len - from)
}

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

pub fn indices(elements: JsElements) -> List(Int) {
  case elements {
    NoElements -> []
    Dense(data) ->
      tree_array.sparse_fold(fn(i, _v, acc) { [i, ..acc] }, [], data)
      |> list.reverse
    Sparse(data) -> dict.keys(data) |> list.sort(int.compare)
  }
}

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
