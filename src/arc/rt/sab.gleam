import arc/rt/async.{type WaitResult} as rt_async
import arc/rt/buffer
import arc/rt/types.{
  type Agent, type Handle, type SabOwner, type WaiterRef, LocalBlock, OwnerBlock,
  Shared,
}
import gleam/option.{type Option, None, Some}

pub fn share(st: Agent, buffer_h: Handle) -> #(Option(SabOwner), Agent) {
  case buffer.buffer_storage(st, buffer_h) {
    Some(Shared(block: OwnerBlock(owner:, ..), ..)) -> #(Some(owner), st)
    Some(Shared(block: LocalBlock(bytes:), max_byte_length:)) -> {
      let owner = spawn_owner(bytes)
      let storage =
        Shared(
          block: OwnerBlock(
            owner:,
            byte_length: types.buffer_byte_size(Shared(
              block: LocalBlock(bytes:),
              max_byte_length:,
            )),
          ),
          max_byte_length:,
        )
      #(Some(owner), buffer.set_storage(st, buffer_h, storage))
    }
    Some(_) | None -> #(None, st)
  }
}

@external(erlang, "arc_rt_sab_ffi", "spawn_owner")
fn spawn_owner(bytes: BitArray) -> SabOwner

@external(erlang, "arc_rt_sab_ffi", "read_part")
pub fn read_part(owner: SabOwner, byte_offset: Int, size: Int) -> BitArray

@external(erlang, "arc_rt_sab_ffi", "write")
pub fn write(owner: SabOwner, byte_offset: Int, chunk: BitArray) -> Nil

// f runs inside the owner process, must be pure
@external(erlang, "arc_rt_sab_ffi", "update")
pub fn update(
  owner: SabOwner,
  byte_offset: Int,
  size: Int,
  f: fn(BitArray) -> #(a, BitArray),
) -> a

@external(erlang, "arc_rt_sab_ffi", "grow")
pub fn grow(owner: SabOwner, new_byte_length: Int) -> Result(Nil, Nil)

// §25.4.3.14 dowait sync, negative timeout waits forever
@external(erlang, "arc_rt_sab_ffi", "wait_sync")
pub fn wait_sync(
  owner: SabOwner,
  byte_offset: Int,
  expected: BitArray,
  timeout_ms: Int,
) -> WaitResult

@external(erlang, "arc_rt_sab_ffi", "notify")
pub fn notify(owner: SabOwner, byte_offset: Int, count: Int) -> Int

@external(erlang, "arc_rt_sab_ffi", "make_waiter_ref")
fn make_waiter_ref() -> WaiterRef

type Registration {
  Waiting
  NotEqualNow
}

@external(erlang, "arc_rt_sab_ffi", "wait_async")
fn wait_async(
  owner: SabOwner,
  ref: WaiterRef,
  byte_offset: Int,
  expected: BitArray,
) -> Registration

// §25.4.3.14 dowait async, none means not-equal
pub fn register_async(
  st: Agent,
  owner: SabOwner,
  byte_offset: Int,
  expected: BitArray,
  deadline: Option(Int),
) -> #(Option(Handle), Agent) {
  let ref = make_waiter_ref()
  case wait_async(owner, ref, byte_offset, expected) {
    Waiting -> {
      let #(promise, st) = rt_async.t_add_waiter(st, owner, ref, deadline)
      #(Some(promise), st)
    }
    NotEqualNow -> #(None, st)
  }
}
