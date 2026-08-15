//// The one type both `arc/rt/types` and `arc/rt/bytecode` must name: the
//// opaque JS wire value. Declared in this leaf so the bytecode types can
//// carry constants and parked operand stacks without importing `types`
//// (which imports them). `arc_rt_val_ffi` is still the only decode point.

/// Opaque JS value — the wire term. Gleam never matches on its shape.
pub type JsVal
