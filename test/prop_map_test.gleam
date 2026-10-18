import arc/bytecode/key.{type PropertyKey, Index, Named, Private}
import arc/rt/prop_map.{type PropMap}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string

// keys made by hand, so that many of them want the same few slots: five names
// on id 8, names whose own ids are the very slots those spill into, #names
// next door on the odd numbers, and some indices
fn crowded() -> List(PropertyKey) {
  [
    Named(8, "a"),
    Named(8, "b"),
    Named(8, "c"),
    Named(12, "d"),
    Named(8, "e"),
    Named(16, "f"),
    Named(16, "g"),
    Named(20, "h"),
    Named(8, "i"),
    Named(40, "far"),
    Private(9, "#a"),
    Private(9, "#b"),
    Private(13, "#c"),
    Index(0),
    Index(7),
  ]
}

type Step {
  Put(PropertyKey, Int)
  Drop(PropertyKey)
}

fn agrees(props: PropMap(Int), model: Dict(PropertyKey, Int)) -> Nil {
  list.each(crowded(), fn(k) {
    assert prop_map.get(props, k) == dict.get(model, k)
    assert prop_map.has_key(props, k) == dict.has_key(model, k)
  })
  assert prop_map.size(props) == dict.size(model)
  let by_text = fn(a: #(PropertyKey, Int), b: #(PropertyKey, Int)) {
    string.compare(string.inspect(a.0), string.inspect(b.0))
  }
  assert list.sort(prop_map.to_list(props), by_text)
    == list.sort(dict.to_list(model), by_text)
  assert list.sort(prop_map.values(props), int.compare)
    == list.sort(dict.values(model), int.compare)
  assert prop_map.fold(props, 0, fn(sum, _k, v) { sum + v })
    == dict.fold(model, 0, fn(sum, _k, v) { sum + v })
}

fn play(steps: List(Step)) -> #(PropMap(Int), Dict(PropertyKey, Int)) {
  use #(props, model), step <- list.fold(steps, #(prop_map.new(), dict.new()))
  let next = case step {
    Put(k, v) -> #(prop_map.insert(props, k, v), dict.insert(model, k, v))
    Drop(k) -> #(prop_map.delete(props, k), dict.delete(model, k))
  }
  agrees(next.0, next.1)
  next
}

// a fixed shuffle: the same steps every run
fn scrambled(seed: Int, count: Int) -> List(Step) {
  let keys = crowded()
  let size = list.length(keys)
  let #(_, steps) =
    int.range(from: 0, to: count, with: #(seed, []), run: fn(acc, i) {
      let #(x, steps) = acc
      let x = { x * 1_103_515_245 + 12_345 } % 2_147_483_648
      let assert Ok(k) = list.first(list.drop(keys, { x / 65_536 } % size))
      let step = case { x / 7 } % 3 {
        0 -> Drop(k)
        _ -> Put(k, i)
      }
      #(x, [step, ..steps])
    })
  steps
}

pub fn names_that_want_one_slot_all_keep_their_own_values_test() {
  let #(props, _) = play(list.index_map(crowded(), Put))
  assert prop_map.get(props, Named(8, "zz")) == Error(Nil)
  assert prop_map.get(props, Named(24, "zz")) == Error(Nil)
  assert prop_map.get(props, Named(100, "a")) == Error(Nil)
}

pub fn removing_one_leaves_the_rest_findable_test() {
  let all = list.index_map(crowded(), Put)
  list.each(crowded(), fn(gone) { play(list.append(all, [Drop(gone)])) })
  // and in every order of arrival that a rotation gives
  list.each(list.index_map(crowded(), fn(_, i) { i }), fn(turn) {
    let rotated = list.append(list.drop(all, turn), list.take(all, turn))
    play(list.append(rotated, list.map(crowded(), Drop)))
  })
}

pub fn any_mix_of_writes_and_removals_matches_a_plain_dictionary_test() {
  list.each([1, 2, 3, 4, 5, 6, 7, 8], fn(seed) { play(scrambled(seed, 400)) })
}

pub fn filter_and_map_keep_everything_findable_test() {
  let #(props, model) = play(list.index_map(crowded(), Put))
  let keep = fn(_k, v) { v % 2 == 0 }
  agrees(prop_map.filter(props, keep), dict.filter(model, keep))
  let double = fn(_k, v) { v * 2 }
  agrees(prop_map.map_values(props, double), dict.map_values(model, double))
  agrees(prop_map.from_list(dict.to_list(model)), model)
}
