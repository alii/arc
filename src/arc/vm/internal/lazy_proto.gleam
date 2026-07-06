//// The heap's slot-id number line, in one place.
////
//// Slot ids fall in three disjoint regions:
////
////   * `id >= 0`   — a real heap slot, handed out by `heap.alloc`/`heap.reserve`.
////   * `id == -1`  — the sentinel (`heap.sentinel_ref`): reads miss, writes no-op.
////   * `id <= -2`  — a tagged *lazy function prototype* (QuickJS-style autoinit),
////                   encoded entirely in the id: no heap entry exists until the
////                   first write materialises one under the tagged id.
////
//// The lazy tag is `-2 - payload`, where
//// `payload = (fn_id * 2^30 + object_proto_id) * 2 + has_constructor`.
//// `fn_id` is the owning closure (the `.constructor` backref, and the identity
//// discriminator — distinct closures get distinct proto refs) and
//// `object_proto_id` is the realm's %Object.prototype% (or the generator
//// prototype) captured at closure creation.
////
//// This module owns the arithmetic; nothing else in the VM should compare a
//// slot id against a magic number.

import gleam/option.{type Option, None, Some}

/// Radix used to pack `object_proto_id` alongside `fn_id` in one payload int.
const shift = 1_073_741_824

/// The one id that is neither a real slot nor a lazy proto: reads miss and
/// writes are dropped. See `heap.sentinel_ref`.
pub const sentinel_id = -1

/// A decoded lazy-proto tag.
pub type LazyProto {
  LazyProto(fn_id: Int, object_proto_id: Int, has_constructor: Bool)
}

/// Is `id` a real heap slot (i.e. one `alloc` may hand out and recycle)?
pub fn is_real_slot(id: Int) -> Bool {
  id >= 0
}

/// Is `id` the sentinel?
pub fn is_sentinel(id: Int) -> Bool {
  id == sentinel_id
}

/// Pack a lazy-proto tag. `None` when either input id doesn't fit the encoding
/// (practically unreachable) — the caller must then allocate the prototype
/// eagerly instead, since an out-of-range payload would alias another
/// closure's proto ref.
pub fn encode_lazy_proto(
  fn_id: Int,
  object_proto_id: Int,
  has_constructor: Bool,
) -> Option(Int) {
  case fn_id >= 0 && object_proto_id >= 0 && object_proto_id < shift {
    False -> None
    True -> {
      let flag = case has_constructor {
        True -> 1
        False -> 0
      }
      let payload = { fn_id * shift + object_proto_id } * 2 + flag
      Some(-2 - payload)
    }
  }
}

/// Unpack a tagged lazy-proto id. `None` when `id` is not in the lazy-proto
/// region (i.e. a real slot or the sentinel) — a non-lazy id has no valid
/// decoding.
pub fn decode_lazy_proto(id: Int) -> Option(LazyProto) {
  case id < sentinel_id {
    False -> None
    True -> {
      let payload = -2 - id
      let rest = payload / 2
      Some(LazyProto(
        fn_id: rest / shift,
        object_proto_id: rest % shift,
        has_constructor: payload % 2 == 1,
      ))
    }
  }
}
