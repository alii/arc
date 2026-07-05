//// Realm-wide module registry — the heap-resident caches the module system
//// keeps on PRIVATE-keyed global-object properties, invisible to guest JS.
////
//// Module evaluation state must be observable by three parties that cannot
//// share Gleam data (it's immutable): the link-time DFS evaluator
//// (`arc/module`), a deferred namespace's proxy traps (which fire while some
//// other module's body is mid-execution), and the dynamic-import host hook
//// (`arc/module_host`). So that state lives in the heap: one hidden object
//// per cache on the realm's global object, keyed by resolved specifier.
////
//// This module is the ONLY reader and writer of those private properties.
//// Each cache has a typed accessor pair, so the heap encoding of an entry —
//// and what "absent" means for it — is decided in exactly one place:
////
////   - status:    specifier → `ModuleStatus` (JsString-encoded).
////   - errors:    specifier → the value the module's evaluation threw.
////                Sticky — later imports / deferred triggers rethrow it
////                (§16.2.1.5.3). Presence of the KEY marks a cached error,
////                so a legal `throw undefined` is cached and rethrown like
////                any other value; the stored value is never compared to
////                `JsUndefined` to decide presence.
////   - namespace: specifier → Module Namespace Exotic Object.
////   - deferred:  specifier → Deferred Module Namespace ([[DeferredNamespace]]
////                is per module record, so identity must be cached).
////   - pending:   specifier → in-flight namespace promise for a module
////                parked on top-level await ([[TopLevelCapability]]).

import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/key.{type PropertyKey, Named}
import arc/vm/ops/object
import arc/vm/state.{type Heap}
import arc/vm/value.{
  type JsValue, type Ref, DataProperty, JsObject, JsString, ObjectSlot,
  OrdinaryObject,
}
import gleam/dict
import gleam/option.{type Option, None, Some}

// =============================================================================
// Hidden global-object property keys — one per cache, private to this module.
//
// Each cache hangs off the realm's global object under a NUL-marker PRIVATE
// key (`key.private_key`, the same hidden namespace class private elements
// live in). Every reflection surface — ownKeys, spread, `in`, has_property,
// for-in, JSON, freeze — filters those out (`value.is_private_name`), so guest
// JS can neither enumerate, read, overwrite nor delete the module registry.
// =============================================================================

/// Resolved specifier → the module's `ModuleStatus`.
fn status_property() -> PropertyKey {
  key.private_key("arc_module_status")
}

/// Resolved specifier → the value the module's evaluation threw.
fn error_cache_property() -> PropertyKey {
  key.private_key("arc_module_errors")
}

/// Resolved specifier → the module's Module Namespace Exotic Object.
fn namespace_cache_property() -> PropertyKey {
  key.private_key("arc_module_cache")
}

/// Resolved specifier → the module's Deferred Module Namespace.
fn deferred_cache_property() -> PropertyKey {
  key.private_key("arc_module_deferred")
}

/// Resolved specifier → in-flight namespace promise (top-level await).
fn pending_cache_property() -> PropertyKey {
  key.private_key("arc_module_pending")
}

// =============================================================================
// Status — [[Status]] of a module record (§16.2.1.5)
// =============================================================================

/// A module's evaluation status in the registry. The absence of a status
/// (`None` from `read_module_status`) means the body has not started
/// ([[Status]] ~linked~).
pub type ModuleStatus {
  /// The body is running or is parked on top-level await.
  Evaluating
  /// The body completed.
  Evaluated
}

// The JsString encoding of `ModuleStatus` is private to the next two
// functions: `write_module_status` is the only writer, and a cleared status is
// a DELETED key, so the catch-all arm of `read_module_status` is unreachable —
// it is there to keep the match total.

/// The module's evaluation status: `Some(Evaluated)` once its body completed,
/// `Some(Evaluating)` while it runs (or is parked on top-level await), `None`
/// when it has not started.
pub fn read_module_status(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Option(ModuleStatus) {
  case read_entry(h, global_object, status_property(), spec) {
    Some(JsString("evaluating")) -> Some(Evaluating)
    Some(JsString("evaluated")) -> Some(Evaluated)
    Some(_) | None -> None
  }
}

pub fn write_module_status(
  h: Heap(host),
  global_object: Ref,
  spec: String,
  status: ModuleStatus,
) -> Heap(host) {
  let encoded = case status {
    Evaluating -> "evaluating"
    Evaluated -> "evaluated"
  }
  write_entry(h, global_object, status_property(), spec, JsString(encoded))
}

/// Forget the module's status (back to "not started") — a failed body clears
/// its ~evaluating~ mark. The error cache, not the status, is what makes the
/// failure sticky.
pub fn clear_module_status(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Heap(host) {
  clear_entry(h, global_object, status_property(), spec)
}

// =============================================================================
// Errors — the sticky evaluation error of a module record
// =============================================================================

/// The value the module's evaluation threw, if it threw. Presence is decided
/// by the cache KEY existing, never by inspecting the stored value — so a
/// module whose top level did `throw undefined` reads back as
/// `Some(JsUndefined)`, a cached error to rethrow, not as "no error".
pub fn read_module_error(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Option(JsValue) {
  read_entry(h, global_object, error_cache_property(), spec)
}

/// Record the value the module's evaluation threw. Every later import or
/// deferred-namespace trigger of `spec` rethrows exactly this value.
pub fn write_module_error(
  h: Heap(host),
  global_object: Ref,
  spec: String,
  err: JsValue,
) -> Heap(host) {
  write_entry(h, global_object, error_cache_property(), spec, err)
}

// =============================================================================
// Namespaces — Module Namespace Exotic Objects (§10.4.6)
// =============================================================================

/// The module's registered Module Namespace Exotic Object, if any.
pub fn read_namespace(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Option(Ref) {
  read_object_entry(h, global_object, namespace_cache_property(), spec)
}

/// Register the module's Module Namespace Exotic Object so later imports
/// (static or dynamic) resolve to the same module record (§16.2.1.8).
pub fn write_namespace(
  h: Heap(host),
  global_object: Ref,
  spec: String,
  namespace: Ref,
) -> Heap(host) {
  write_entry(
    h,
    global_object,
    namespace_cache_property(),
    spec,
    JsObject(namespace),
  )
}

/// Roll back a namespace registration for a module whose body never
/// completed — it may be (re-)evaluated by a later import. Private: the only
/// blessed rollback is `clear_module_registrations`, which drops every
/// registration together.
fn clear_namespace(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Heap(host) {
  clear_entry(h, global_object, namespace_cache_property(), spec)
}

/// The module's registered Deferred Module Namespace, if any.
pub fn read_deferred_namespace(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Option(Ref) {
  read_object_entry(h, global_object, deferred_cache_property(), spec)
}

/// Register the module's Deferred Module Namespace: `import defer` /
/// `import.defer()` of the same module must yield the identical object.
pub fn write_deferred_namespace(
  h: Heap(host),
  global_object: Ref,
  spec: String,
  namespace: Ref,
) -> Heap(host) {
  write_entry(
    h,
    global_object,
    deferred_cache_property(),
    spec,
    JsObject(namespace),
  )
}

/// Roll back a deferred-namespace registration for a module whose body never
/// completed — its exports would read uninitialized cells, and a later
/// `import defer` must build a fresh namespace over a re-evaluated module.
/// Private for the same reason as `clear_namespace`.
fn clear_deferred_namespace(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Heap(host) {
  clear_entry(h, global_object, deferred_cache_property(), spec)
}

// =============================================================================
// Pending — in-flight top-level-await namespace promises
// =============================================================================

/// The in-flight namespace promise of a module parked on top-level await, if
/// one is registered. A re-import of an ~evaluating-async~ module returns
/// this same promise (Evaluate() step 4 — [[TopLevelCapability]]).
pub fn read_pending_promise(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Option(Ref) {
  read_object_entry(h, global_object, pending_cache_property(), spec)
}

pub fn write_pending_promise(
  h: Heap(host),
  global_object: Ref,
  spec: String,
  promise_ref: Ref,
) -> Heap(host) {
  write_entry(
    h,
    global_object,
    pending_cache_property(),
    spec,
    JsObject(promise_ref),
  )
}

/// Drop the in-flight promise once the module's evaluation settled — future
/// imports read the namespace or error cache instead.
pub fn clear_pending_promise(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Heap(host) {
  clear_entry(h, global_object, pending_cache_property(), spec)
}

// =============================================================================
// The combined view — one precedence ladder over all five caches
// =============================================================================

/// Everything the caches say about one module, as ONE value with the
/// precedence rules baked in. Both dynamic-import hook arms (`import()` and
/// `import.defer()`) read the registry through this, so they cannot grow two
/// subtly different ladders over the same five caches.
///
/// Precedence, highest first:
///   1. the sticky evaluation error (§16.2.1.5.3) — a namespace may have been
///      pre-published before the body threw, so the error must win;
///   2. the in-flight top-level-await promise (Evaluate() step 4) — a re-import
///      chains onto the same evaluation instead of re-running the body;
///   3. the module namespace, split by whether the body has started.
///
/// The Deferred Module Namespace rides along on every state that can have one:
/// its identity is per module record ([[DeferredNamespace]]) and is valid
/// whatever the body's status, so an `import.defer()` can hand it back without
/// re-deriving the ladder.
pub type CacheState {
  /// The module's evaluation threw; every later import rethrows this value.
  Failed(error: JsValue)
  /// The body is parked on top-level await; `promise` is its in-flight
  /// namespace promise.
  Pending(promise: Ref, deferred: Option(Ref))
  /// The body has started (it is running, parked, or completed) — an eager
  /// import must NOT run it again and resolves with `namespace`.
  Started(namespace: Ref, deferred: Option(Ref))
  /// Linked (so the namespace exists) but the body never started — an eager
  /// import still has to evaluate it. This is what an earlier `import.defer()`
  /// leaves behind.
  LinkedOnly(namespace: Ref, deferred: Option(Ref))
  /// No namespace registered. `deferred` is `None` in every reachable case (a
  /// deferred namespace is only ever registered alongside a namespace, and
  /// `clear_module_registrations` drops both together); it is carried anyway so
  /// this type is a LOSSLESS view of the caches rather than one that quietly
  /// forgets an entry.
  Absent(deferred: Option(Ref))
}

/// Everything the five caches say about `spec`, collapsed to one `CacheState`.
pub fn read_cache_state(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> CacheState {
  case read_module_error(h, global_object, spec) {
    Some(error) -> Failed(error:)
    None -> {
      let deferred = read_deferred_namespace(h, global_object, spec)
      case read_pending_promise(h, global_object, spec) {
        Some(promise) -> Pending(promise:, deferred:)
        None ->
          case read_namespace(h, global_object, spec) {
            None -> Absent(deferred:)
            Some(namespace) ->
              case read_module_status(h, global_object, spec) {
                Some(Evaluating) | Some(Evaluated) ->
                  Started(namespace:, deferred:)
                None -> LinkedOnly(namespace:, deferred:)
              }
          }
      }
    }
  }
}

// =============================================================================
// Whole-module rollback
// =============================================================================

/// Un-register a module whose body never completed: the registrations a bundle
/// publishes for it (status, namespace, deferred namespace) are dropped
/// together, so a later import re-links and re-evaluates it from scratch. The
/// single counterpart to registration — a partially rolled-back module (say,
/// namespace cleared but the deferred namespace still live over uninitialized
/// cells) is not expressible.
///
/// The error cache is deliberately untouched: an evaluation error is sticky
/// (§16.2.1.5.3) and must be rethrown, not re-run.
///
/// The in-flight promise is deliberately untouched too: a module parked on top-
/// level await must keep handing back the same promise (§16.2.1.8), and its
/// %FinishDynamicImport% continuation owns the settle-time cleanup. Dropping it
/// here would let a later `import()` re-link and re-run a body that is still
/// running, executing its side effects twice.
pub fn clear_module_registrations(
  h: Heap(host),
  global_object: Ref,
  spec: String,
) -> Heap(host) {
  h
  |> clear_module_status(global_object, spec)
  |> clear_namespace(global_object, spec)
  |> clear_deferred_namespace(global_object, spec)
}

// =============================================================================
// The hidden-cache protocol (private)
// =============================================================================

/// Read `key` off the hidden cache object `property` on the global. `Some` iff
/// the cache object exists AND owns the key (whatever value it holds) — which
/// is exactly "registered", because `clear_entry` DELETES the key rather than
/// overwriting it with a sentinel.
fn read_entry(
  h: Heap(host),
  global_object: Ref,
  property: PropertyKey,
  key: String,
) -> Option(JsValue) {
  case object.get_own_property(h, global_object, property) {
    Some(DataProperty(value: JsObject(cache_ref), ..)) ->
      case object.get_own_property(h, cache_ref, Named(key)) {
        Some(DataProperty(value: cached, ..)) -> Some(cached)
        _ -> None
      }
    _ -> None
  }
}

/// `read_entry` narrowed to entries holding an object — the shape of the
/// namespace / deferred / pending caches. Only `write_*` fills these caches and
/// each writes a `JsObject`, so the non-object arm is unreachable; it exists to
/// keep the match total, not to tolerate a sentinel.
fn read_object_entry(
  h: Heap(host),
  global_object: Ref,
  property: PropertyKey,
  key: String,
) -> Option(Ref) {
  case read_entry(h, global_object, property, key) {
    Some(JsObject(ref)) -> Some(ref)
    Some(_) | None -> None
  }
}

/// Write `key` → `val` into the hidden cache object `property` on the global,
/// creating the cache object on first use.
///
/// Both writes go through the TOTAL define path (`define_method_property`),
/// never `set_property`: an ordinary [[Set]] returns a success Bool that a
/// caller can drop on the floor, and a refused registry write must not be able
/// to pass silently.
fn write_entry(
  h: Heap(host),
  global_object: Ref,
  property: PropertyKey,
  key: String,
  val: JsValue,
) -> Heap(host) {
  let #(h, cache_ref) = case
    object.get_own_property(h, global_object, property)
  {
    Some(DataProperty(value: JsObject(cache_ref), ..)) -> #(h, cache_ref)
    _ -> {
      let #(h, cache_ref) =
        heap.alloc(
          h,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: elements.new(),
            prototype: None,
            symbol_properties: [],
            extensible: True,
          ),
        )
      let h =
        object.define_method_property(
          h,
          global_object,
          property,
          JsObject(cache_ref),
        )
      #(h, cache_ref)
    }
  }
  object.define_method_property(h, cache_ref, Named(key), val)
}

/// Un-register `key` from the hidden cache object `property` on the global.
///
/// Every cache clears the SAME way — by deleting the key — so "absent" has one
/// meaning (`read_entry` returns `None`) rather than two, and no reader has to
/// recognise a sentinel value as secretly meaning "not registered". `write_entry`
/// defines cache entries as configurable data properties on an ordinary object,
/// so [[Delete]] cannot refuse: a `False` here is a broken invariant, not a case
/// to swallow.
fn clear_entry(
  h: Heap(host),
  global_object: Ref,
  property: PropertyKey,
  key: String,
) -> Heap(host) {
  case object.get_own_property(h, global_object, property) {
    Some(DataProperty(value: JsObject(cache_ref), ..)) -> {
      let #(h, deleted) = object.delete_property(h, cache_ref, Named(key))
      case deleted {
        True -> h
        False -> panic as "arc/module/registry: cache entry refused deletion"
      }
    }
    // The cache object does not exist yet, so nothing is registered under it.
    _ -> h
  }
}
