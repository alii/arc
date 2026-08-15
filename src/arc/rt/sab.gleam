//// Shared Data Blocks that more than one agent can see (ES2024 §6.2.9,
//// §9.7 Agents) and their WaiterList (§25.4.1). A SharedArrayBuffer starts
//// with its bytes in the creating agent's store (`types.LocalBlock`); the
//// first time it can be observed by another agent, or a waiter is registered
//// on it, `share` moves the block into an owner PROCESS
//// (`arc_rt_sab_ffi`) and the buffer's storage becomes
//// `Shared(OwnerBlock(pid, ..))`. Every agent handed the buffer holds that
//// same pid, and each read / write / read-modify-write / wait / notify is a
//// synchronous message to the owner, whose mailbox order is the memory
//// model's total order. No tables, no atomics refs: processes and messages.
////
//// Waiters. A sync `Atomics.wait` compares-and-registers in the owner and
//// then blocks the calling BEAM process in a selective receive
//// (`wait_sync`). An `Atomics.waitAsync` compares-and-registers the same
//// way (`register_async`) but returns at once: the registration joins the
//// agent's own `Agent.waiters` (`rt/async.t_add_waiter`) with its promise
//// capability and deadline, and from then on the agent's microtask drain
//// drives it — it takes the owner's wake message from this process's
//// mailbox and queues the resolve job, and it runs the timeout job at the
//// deadline (`rt/async.drain`). Deadlines are the WAITER's job, so nothing
//// in the owner ever races a timer: an expiring waiter withdraws with
//// `cancel`, and `already_woken` means the wake is already in its mailbox.

import arc/rt/async.{type WaitResult} as rt_async
import arc/rt/buffer
import arc/rt/types.{
  type Agent, type Handle, type SabOwner, type WaiterRef, LocalBlock, OwnerBlock,
  Shared,
}
import gleam/option.{type Option, None, Some}

// ── the block ───────────────────────────────────────────────────────────────

/// Hand the SharedArrayBuffer at `buffer_h` to an owner process (once): a
/// `LocalBlock` is moved out of the store, an `OwnerBlock` is returned as
/// is. None when the handle is not a SharedArrayBuffer.
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

/// `size` bytes at `byte_offset` of the owner's block.
@external(erlang, "arc_rt_sab_ffi", "read_part")
pub fn read_part(owner: SabOwner, byte_offset: Int, size: Int) -> BitArray

/// Overwrite `byte_size(chunk)` bytes at `byte_offset`.
@external(erlang, "arc_rt_sab_ffi", "write")
pub fn write(owner: SabOwner, byte_offset: Int, chunk: BitArray) -> Nil

/// One read-modify-write of the `size` bytes at `byte_offset`, performed
/// inside the owner: `f` maps the old bytes to `#(reply, new bytes)` and
/// runs with no other access interleaved (§25.4.3.17 AtomicReadModifyWrite,
/// §25.4.6 compareExchange). `f` must be total and pure — it executes in the
/// owner process.
@external(erlang, "arc_rt_sab_ffi", "update")
pub fn update(
  owner: SabOwner,
  byte_offset: Int,
  size: Int,
  f: fn(BitArray) -> #(a, BitArray),
) -> a

/// §25.2.2.3 GrowSharedArrayBuffer against the live length: Error when the
/// block is already longer than `new_byte_length` (another agent grew it).
@external(erlang, "arc_rt_sab_ffi", "grow")
pub fn grow(owner: SabOwner, new_byte_length: Int) -> Result(Nil, Nil)

// ── waiters ─────────────────────────────────────────────────────────────────

/// §25.4.3.14 DoWait, sync mode: if the `byte_size(expected)` bytes at
/// `byte_offset` still equal `expected`, join the WaiterList and BLOCK this
/// process until notified or `timeout_ms` elapses (negative = forever).
/// The owner's reply atoms are `rt_async.WaitResult`'s constructors.
@external(erlang, "arc_rt_sab_ffi", "wait_sync")
pub fn wait_sync(
  owner: SabOwner,
  byte_offset: Int,
  expected: BitArray,
  timeout_ms: Int,
) -> WaitResult

/// §25.4.3.11 / Atomics.notify: wake up to `count` waiters at `byte_offset`,
/// FIFO. Returns how many were woken.
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

/// §25.4.3.14 DoWait, async mode, steps 16-30: compare-and-AddWaiter in the
/// owner's critical section. `Some(promise)` — the fresh capability's
/// promise, now one of this agent's `waiters` for the drain to wake or time
/// out at `deadline` — when the registration joined the WaiterList; None
/// when the bytes no longer equal `expected` ("not-equal", nothing
/// registered).
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
