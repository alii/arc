import gleam/option.{type Option}

pub type Arena(a)

@external(erlang, "arc_rt_arena_ffi", "new")
pub fn new() -> Arena(a)

// freed ids answer the untyped free marker, ids never set crash
@external(erlang, "arc_rt_arena_ffi", "get")
pub fn get(id: Int, arena: Arena(a)) -> a

@external(erlang, "arc_rt_arena_ffi", "get_option")
pub fn get_option(id: Int, arena: Arena(a)) -> Option(a)

@external(erlang, "arc_rt_arena_ffi", "set")
pub fn set(id: Int, value: a, arena: Arena(a)) -> Arena(a)

@external(erlang, "arc_rt_arena_ffi", "free")
pub fn free(id: Int, arena: Arena(a)) -> Arena(a)

@external(erlang, "arc_rt_arena_ffi", "fold")
pub fn fold(f: fn(Int, a, b) -> b, initial: b, arena: Arena(a)) -> b

@external(erlang, "arc_rt_arena_ffi", "count")
pub fn count(arena: Arena(a)) -> Int

/// which ids a collection found alive
pub type Marks

/// frees the unmarked ids from `watermark` up; `next` is where writes go next
@external(erlang, "arc_rt_arena_ffi", "keep_marked")
pub fn keep_marked(
  watermark: Int,
  marks: Marks,
  next: Int,
  arena: Arena(a),
) -> Arena(a)

@external(erlang, "arc_rt_arena_ffi", "diff_below")
pub fn diff_below(watermark: Int, old: Arena(a), new: Arena(a)) -> List(Int)
