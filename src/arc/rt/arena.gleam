import gleam/option.{type Option}

pub type Arena(a)

@external(erlang, "arc_rt_arena_ffi", "new")
pub fn new() -> Arena(a)

@external(erlang, "arc_rt_arena_ffi", "get")
pub fn get(id: Int, arena: Arena(a)) -> a

@external(erlang, "arc_rt_arena_ffi", "get_option")
pub fn get_option(id: Int, arena: Arena(a)) -> Option(a)

@external(erlang, "arc_rt_arena_ffi", "set")
pub fn set(id: Int, value: a, arena: Arena(a)) -> Arena(a)

@external(erlang, "arc_rt_arena_ffi", "reset")
pub fn reset(id: Int, arena: Arena(a)) -> Arena(a)

@external(erlang, "arc_rt_arena_ffi", "fold")
pub fn fold(f: fn(Int, a, b) -> b, initial: b, arena: Arena(a)) -> b

@external(erlang, "arc_rt_arena_ffi", "count")
pub fn count(arena: Arena(a)) -> Int

@external(erlang, "arc_rt_arena_ffi", "from_descending")
pub fn from_descending(cells: List(#(Int, a))) -> Arena(a)
