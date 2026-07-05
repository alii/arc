//// Heap-level ArrayBuffer storage helpers, shared by every write path
//// (DataView.prototype.set*, TypedArray element stores, ArrayBuffer.slice).
//// The storage rewrite lives here exactly once, so no call site can rebuild
//// an ArrayBufferObject slot and lose part of its state on the way back in.

import arc/vm/heap
import arc/vm/state.{type Heap}
import arc/vm/value.{type Ref, ArrayBufferObject, ObjectSlot}

/// Persist `new_bits` (a whole new image of the buffer's bytes) into the
/// ArrayBuffer slot at `buffer`, writing back only the `count` bytes at
/// `byte_offset`: shared (atomics-backed) storage lets other agent processes
/// concurrently mutate the regions this caller did NOT touch, so pushing the
/// whole snapshot back would clobber their updates. Non-shared storage just
/// swaps in the full image; a caller that owns the whole buffer (a freshly
/// allocated result) passes the full range.
///
/// Detached and immutable buffers keep whatever they had — every write path
/// rejects them before getting here (see `value.buffer_store_region`).
///
/// `buffer` is always a validated ArrayBuffer ref (a TypedArray/DataView's
/// [[ViewedArrayBuffer]], or a buffer this module's caller just allocated),
/// so a slot of any other shape is a wiring bug: crash rather than silently
/// drop the store while still reporting success to JS.
pub fn store_region(
  h: Heap(host),
  buffer: Ref,
  new_bits: BitArray,
  byte_offset: Int,
  count: Int,
) -> Heap(host) {
  use slot <- heap.update(h, buffer)
  let assert ObjectSlot(kind: ArrayBufferObject(storage:), ..) = slot
    as "buffer.store_region: ref does not hold an ArrayBuffer"
  ObjectSlot(
    ..slot,
    kind: ArrayBufferObject(storage: value.buffer_store_region(
      storage,
      new_bits,
      byte_offset,
      count,
    )),
  )
}
