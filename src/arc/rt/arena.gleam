//// The heap's cell arena: a persistent 16-way trie from cell id to slot,
//// as shallow as the highest id allows, with the last-written leaf kept
//// aside so runs of writes to neighbouring ids skip the path copy
//// (`arc_rt_arena_ffi`).

import gleam/option.{type Option}

pub type Arena(a)

/// The empty arena.
@external(erlang, "arc_rt_arena_ffi", "new")
pub fn new() -> Arena(a)

/// Read slot `id`. Only defined for a taken id (set, and not since reset or
/// dropped by `from_descending`); any other id is a caller bug and either
/// crashes or reads the FFI's free sentinel.
@external(erlang, "arc_rt_arena_ffi", "get")
pub fn get(id: Int, arena: Arena(a)) -> a

/// Read slot `id`, None when it is free or was never set. Total.
@external(erlang, "arc_rt_arena_ffi", "get_option")
pub fn get_option(id: Int, arena: Arena(a)) -> Option(a)

/// Write slot `id` (>= 0), growing the arena to reach it.
@external(erlang, "arc_rt_arena_ffi", "set")
pub fn set(id: Int, value: a, arena: Arena(a)) -> Arena(a)

/// Free slot `id`; an id the arena never reached is a no-op.
@external(erlang, "arc_rt_arena_ffi", "reset")
pub fn reset(id: Int, arena: Arena(a)) -> Arena(a)

/// Fold over the taken slots in ascending id order.
@external(erlang, "arc_rt_arena_ffi", "fold")
pub fn fold(f: fn(Int, a, b) -> b, initial: b, arena: Arena(a)) -> b

/// Number of taken slots. O(n).
@external(erlang, "arc_rt_arena_ffi", "count")
pub fn count(arena: Arena(a)) -> Int

/// The arena holding exactly `cells`, given as `#(id, slot)` pairs in
/// DESCENDING id order (a `fold` accumulator's shape). O(n).
@external(erlang, "arc_rt_arena_ffi", "from_descending")
pub fn from_descending(cells: List(#(Int, a))) -> Arena(a)
