import arc/rt/arena
import arc/rt/types.{type Cell, SBox, mk_int}
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}

fn check(a: arena.Arena(Int), model: dict.Dict(Int, Int), upto: Int) -> Nil {
  int.range(from: -1, to: upto, with: Nil, run: fn(nil, id) {
    let want = case dict.get(model, id) {
      Ok(v) -> Some(v)
      Error(Nil) -> None
    }
    assert arena.get_option(id, a) == want
    nil
  })
}

pub fn set_get_across_levels_test() {
  let ids = [0, 15, 16, 17, 255, 256, 4095, 4096, 70_000, 3, 4097]
  let #(a, model) =
    list.fold(ids, #(arena.new(), dict.new()), fn(acc, id) {
      let #(a, m) = acc
      #(arena.set(id, id * 7, a), dict.insert(m, id, id * 7))
    })
  check(a, model, 70_100)
  list.each(ids, fn(id) {
    assert arena.get(id, a) == id * 7
  })
  assert arena.count(a) == list.length(ids)
}

pub fn overwrite_and_free_test() {
  let a =
    int.range(from: 0, to: 300, with: arena.new(), run: fn(a, id) {
      arena.set(id, id, a)
    })
  let a = arena.set(5, -5, a)
  let a = arena.set(299, -299, a)
  let a = arena.free(6, a)
  let a = arena.free(298, a)
  let a = arena.free(10_000, a)
  assert arena.get(5, a) == -5
  assert arena.get(299, a) == -299
  assert arena.get_option(6, a) == None
  assert arena.get_option(298, a) == None
  assert arena.get(7, a) == 7
  assert arena.count(a) == 298
}

@external(erlang, "arc_rt_gc_ffi", "mark")
fn mark(
  cells: arena.Arena(Cell),
  roots: List(Int),
  watermark: Int,
  weak: List(Int),
) -> #(arena.Marks, Int, List(Int), Int)

pub fn keep_marked_test() {
  let live = [2, 3, 40, 41, 42, 1000, 1001, 5000, 20_001]
  let cell = fn(id) { SBox(mk_int(id)) }
  let a =
    int.range(from: 0, to: 600_000, with: arena.new(), run: fn(a, id) {
      arena.set(id, cell(id), a)
    })
  use watermark <- list.each([0, 41, 2047, 2048, 4099])
  let #(marks, count, weak, past) = mark(a, live, watermark, [])
  let young = list.filter(live, fn(id) { id >= watermark })
  assert count == list.length(young)
  assert weak == []
  assert past == 20_002
  let b = arena.keep_marked(watermark, marks, past, a)
  let kept = arena.fold(fn(id, _, acc) { [id, ..acc] }, [], b)
  let old = int.range(from: 0, to: watermark, with: [], run: list.prepend)
  assert kept == list.append(list.reverse(young), old)
  assert arena.diff_below(watermark, a, b) == []
  let b = arena.set(20_002, cell(9), arena.set(700_000, cell(8), b))
  assert arena.get(20_002, b) == cell(9)
  assert arena.get(700_000, b) == cell(8)
  assert arena.get(5000, b) == cell(5000)
}

pub fn ascending_fold_order_test() {
  let a =
    [900, 1, 300, 16, 0]
    |> list.fold(arena.new(), fn(a, id) { arena.set(id, id, a) })
  let seen = arena.fold(fn(id, _, acc) { [id, ..acc] }, [], a)
  assert seen == [900, 300, 16, 1, 0]
  assert int.sum(seen) == 1217
}

// five and six levels deep, where whole branches are still missing
pub fn sparse_deep_arena_test() {
  let ids = [
    0, 17, 4096, 65_535, 65_536, 65_553, 70_000, 1_000_000, 1_048_575, 1_048_576,
    1_048_593, 1_114_112, 3_000_000, 16_777_215, 5, 65_537, 1_048_577,
  ]
  let near = fn(id) {
    [
      id - 4096,
      id - 256,
      id - 16,
      id - 1,
      id,
      id + 1,
      id + 16,
      id + 256,
      id + 4096,
    ]
  }
  list.fold(ids, #(arena.new(), dict.new()), fn(acc, id) {
    let #(a, m) = acc
    let a = arena.set(id, id * 3, a)
    let m = dict.insert(m, id, id * 3)
    list.each(list.flat_map(ids, near), fn(probe) {
      assert arena.get_option(probe, a)
        == option.from_result(dict.get(m, probe))
    })
    #(a, m)
  })
  Nil
}
