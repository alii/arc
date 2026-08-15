//// Realm-wide module registry: the caches the module system keeps on
//// PRIVATE-keyed global-object properties, invisible to guest JS.
////
//// Module evaluation state must be observable by the link-time DFS evaluator
//// (`arc/module`), a deferred namespace's proxy traps (which fire
//// while some other module's body is mid-execution), and the dynamic-import
//// continuation (`arc/module_host`). All three thread the same
//// `Agent`, so the state lives in its heap: one hidden object per cache on
//// the realm's global object, keyed by resolved specifier. Being heap data it
//// is per-realm, GC-traced and serialized with the store.
////
//// This module is the ONLY reader and writer of those private properties.
////
////   - status:    specifier → `ModuleStatus` (string-encoded).
////   - errors:    specifier → the value the module's evaluation threw.
////                Sticky (§16.2.1.5.3). Presence of the KEY marks a cached
////                error, so a legal `throw undefined` is cached and rethrown
////                like any other value.
////   - namespace: specifier → Module Namespace Exotic Object.
////   - deferred:  specifier → Deferred Module Namespace ([[DeferredNamespace]]
////                is per module record, so identity must be cached).
////   - pending:   specifier → in-flight namespace promise for a module
////                parked on top-level await ([[TopLevelCapability]]).
////   - referrer:  the resolved specifier of the module whose body is running
////                right now (§16.2.1.8 referencingScriptOrModule), one slot.

import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type PropertyKey, DataProperty, KHandle,
  KStr, Named, SObject, StringKey, classify, mk_object, mk_string,
}
import gleam/dict
import gleam/option.{type Option, None, Some}

// =============================================================================
// Hidden global-object property keys, one per cache.
//
// Each cache hangs off the realm's global object under a PRIVATE key (the
// same hidden namespace class private elements live in). Every reflection
// surface filters those out (`types.is_private_key`), so guest JS can neither
// enumerate, read, overwrite nor delete the module registry.
// =============================================================================

fn status_property() -> PropertyKey {
  types.private_key("arc_module_status")
}

fn error_cache_property() -> PropertyKey {
  types.private_key("arc_module_errors")
}

fn namespace_cache_property() -> PropertyKey {
  types.private_key("arc_module_cache")
}

fn deferred_cache_property() -> PropertyKey {
  types.private_key("arc_module_deferred")
}

fn pending_cache_property() -> PropertyKey {
  types.private_key("arc_module_pending")
}

fn referrer_property() -> PropertyKey {
  types.private_key("arc_module_referrer")
}

/// The one key of the referrer cache object.
const referrer_key = "active"

// =============================================================================
// Referrer: §16.2.1.8 referencingScriptOrModule of the running module body
// =============================================================================

/// The resolved specifier of the module whose body is executing, if one is:
/// what an ImportCall captures so a nested `import()` resolves relative to
/// the importing MODULE. `None` is script code (the host falls back to its
/// entry referrer). It lives in the heap rather than on an activation so a
/// bytecode callback re-entered from a builtin during the body still sees it.
pub fn read_active_referrer(st: Agent) -> Option(String) {
  use v <- option.then(read_entry(st, referrer_property(), referrer_key))
  case classify(v) {
    KStr(spec) -> Some(spec)
    _ -> None
  }
}

/// Set (or with `None`, clear) the running module body's specifier. The
/// evaluator brackets each body with the previous value so nested
/// evaluations (a deferred-namespace trigger mid-body) restore correctly.
pub fn write_active_referrer(st: Agent, referrer: Option(String)) -> Agent {
  case referrer {
    Some(spec) ->
      write_entry(st, referrer_property(), referrer_key, mk_string(spec))
    None -> clear_entry(st, referrer_property(), referrer_key)
  }
}

// =============================================================================
// Status: [[Status]] of a module record (§16.2.1.5)
// =============================================================================

/// A module's evaluation status in the registry. No status (`None` from
/// `read_module_status`) means the body has not started ([[Status]] ~linked~).
pub type ModuleStatus {
  /// The body is running or is parked on top-level await.
  Evaluating
  /// The body completed.
  Evaluated
}

/// `Some(Evaluated)` once the body completed, `Some(Evaluating)` while it
/// runs (or is parked on top-level await), `None` when it has not started.
pub fn read_module_status(st: Agent, spec: String) -> Option(ModuleStatus) {
  use v <- option.then(read_entry(st, status_property(), spec))
  case classify(v) {
    KStr("evaluating") -> Some(Evaluating)
    KStr("evaluated") -> Some(Evaluated)
    _ -> None
  }
}

pub fn write_module_status(
  st: Agent,
  spec: String,
  status: ModuleStatus,
) -> Agent {
  let encoded = case status {
    Evaluating -> "evaluating"
    Evaluated -> "evaluated"
  }
  write_entry(st, status_property(), spec, mk_string(encoded))
}

/// Forget the module's status (back to "not started"): a failed body clears
/// its ~evaluating~ mark. The error cache, not the status, is what makes the
/// failure sticky.
pub fn clear_module_status(st: Agent, spec: String) -> Agent {
  clear_entry(st, status_property(), spec)
}

// =============================================================================
// Errors: the sticky evaluation error of a module record
// =============================================================================

/// The value the module's evaluation threw, if it threw. Presence is decided
/// by the cache KEY existing, never by inspecting the stored value.
pub fn read_module_error(st: Agent, spec: String) -> Option(JsVal) {
  read_entry(st, error_cache_property(), spec)
}

/// Record the value the module's evaluation threw. Every later import or
/// deferred-namespace trigger of `spec` rethrows exactly this value.
pub fn write_module_error(st: Agent, spec: String, err: JsVal) -> Agent {
  write_entry(st, error_cache_property(), spec, err)
}

// =============================================================================
// Namespaces: Module Namespace Exotic Objects (§10.4.6)
// =============================================================================

/// The module's registered Module Namespace Exotic Object, if any.
pub fn read_namespace(st: Agent, spec: String) -> Option(Handle) {
  read_object_entry(st, namespace_cache_property(), spec)
}

/// Register the module's namespace so later imports (static or dynamic)
/// resolve to the same module record (§16.2.1.8).
pub fn write_namespace(st: Agent, spec: String, namespace: Handle) -> Agent {
  write_entry(st, namespace_cache_property(), spec, mk_object(namespace))
}

fn clear_namespace(st: Agent, spec: String) -> Agent {
  clear_entry(st, namespace_cache_property(), spec)
}

/// The module's registered Deferred Module Namespace, if any.
pub fn read_deferred_namespace(st: Agent, spec: String) -> Option(Handle) {
  read_object_entry(st, deferred_cache_property(), spec)
}

/// Register the module's Deferred Module Namespace: `import defer` /
/// `import.defer()` of the same module must yield the identical object.
pub fn write_deferred_namespace(
  st: Agent,
  spec: String,
  namespace: Handle,
) -> Agent {
  write_entry(st, deferred_cache_property(), spec, mk_object(namespace))
}

fn clear_deferred_namespace(st: Agent, spec: String) -> Agent {
  clear_entry(st, deferred_cache_property(), spec)
}

// =============================================================================
// Pending: in-flight top-level-await namespace promises
// =============================================================================

/// The in-flight namespace promise of a module parked on top-level await. A
/// re-import of an ~evaluating-async~ module returns this same promise
/// (Evaluate() step 4, [[TopLevelCapability]]).
pub fn read_pending_promise(st: Agent, spec: String) -> Option(Handle) {
  read_object_entry(st, pending_cache_property(), spec)
}

pub fn write_pending_promise(
  st: Agent,
  spec: String,
  promise: Handle,
) -> Agent {
  write_entry(st, pending_cache_property(), spec, mk_object(promise))
}

/// Drop the in-flight promise once the module's evaluation settled.
pub fn clear_pending_promise(st: Agent, spec: String) -> Agent {
  clear_entry(st, pending_cache_property(), spec)
}

// =============================================================================
// The combined view: one precedence ladder over all five caches
// =============================================================================

/// Everything the caches say about one module, with the precedence rules
/// baked in. Both dynamic-import arms (`import()` and `import.defer()`) read
/// the registry through this.
///
/// Precedence, highest first:
///   1. the sticky evaluation error (§16.2.1.5.3): a namespace may have been
///      pre-published before the body threw, so the error must win;
///   2. the in-flight top-level-await promise (Evaluate() step 4): a re-import
///      chains onto the same evaluation instead of re-running the body;
///   3. the module namespace, split by whether the body has started.
///
/// The Deferred Module Namespace rides along on every state that can have one:
/// its identity is per module record and valid whatever the body's status.
pub type CacheState {
  /// The module's evaluation threw; every later import rethrows this value.
  Failed(error: JsVal)
  /// The body is parked on top-level await; `promise` is its in-flight
  /// namespace promise.
  Pending(promise: Handle, deferred: Option(Handle))
  /// The body has started (running, parked, or completed): an eager import
  /// must NOT run it again and resolves with `namespace`.
  Started(namespace: Handle, deferred: Option(Handle))
  /// Linked (so the namespace exists) but the body never started: an eager
  /// import still has to evaluate it. What an earlier `import.defer()` leaves.
  LinkedOnly(namespace: Handle, deferred: Option(Handle))
  /// No namespace registered.
  Absent(deferred: Option(Handle))
}

pub fn read_cache_state(st: Agent, spec: String) -> CacheState {
  case read_module_error(st, spec) {
    Some(error) -> Failed(error:)
    None -> {
      let deferred = read_deferred_namespace(st, spec)
      case read_pending_promise(st, spec), read_namespace(st, spec) {
        Some(promise), _ -> Pending(promise:, deferred:)
        None, None -> Absent(deferred:)
        None, Some(namespace) ->
          case read_module_status(st, spec) {
            Some(Evaluating) | Some(Evaluated) -> Started(namespace:, deferred:)
            None -> LinkedOnly(namespace:, deferred:)
          }
      }
    }
  }
}

// =============================================================================
// Whole-module rollback
// =============================================================================

/// Un-register a module whose body never completed: status, namespace and
/// deferred namespace are dropped together, so a later import re-links and
/// re-evaluates it from scratch.
///
/// The error cache is untouched: an evaluation error is sticky (§16.2.1.5.3).
/// The in-flight promise is untouched too: a module parked on top-level await
/// keeps handing back the same promise (§16.2.1.8), and its
/// %FinishDynamicImport% continuation owns the settle-time cleanup.
pub fn clear_module_registrations(st: Agent, spec: String) -> Agent {
  st
  |> clear_module_status(spec)
  |> clear_namespace(spec)
  |> clear_deferred_namespace(spec)
}

// =============================================================================
// The hidden-cache protocol (private)
// =============================================================================

/// The hidden cache object `property` on the global, if it exists yet.
fn cache_object(st: Agent, property: PropertyKey) -> Option(Handle) {
  case
    rt_obj.t_ordinary_own_property(
      st,
      st.realm.global_object,
      StringKey(property),
    )
  {
    Some(DataProperty(value:, ..)) ->
      case classify(value) {
        KHandle(h) -> Some(h)
        _ -> None
      }
    _ -> None
  }
}

/// Read `key` off the hidden cache object `property`. `Some` iff the cache
/// object exists AND owns the key (whatever value it holds), which is exactly
/// "registered": `clear_entry` DELETES the key rather than overwriting it.
fn read_entry(st: Agent, property: PropertyKey, key: String) -> Option(JsVal) {
  use cache <- option.then(cache_object(st, property))
  case rt_obj.t_ordinary_own_property(st, cache, StringKey(Named(key))) {
    Some(DataProperty(value:, ..)) -> Some(value)
    _ -> None
  }
}

/// `read_entry` narrowed to entries holding an object: the shape of the
/// namespace / deferred / pending caches. Only `write_*` fills these caches
/// and each writes an object, so the non-object arm is unreachable; it keeps
/// the match total.
fn read_object_entry(
  st: Agent,
  property: PropertyKey,
  key: String,
) -> Option(Handle) {
  use v <- option.then(read_entry(st, property, key))
  case classify(v) {
    KHandle(h) -> Some(h)
    _ -> None
  }
}

/// Write `key` → `val` into the hidden cache object `property` on the global,
/// creating the (null-prototype) cache object on first use.
fn write_entry(
  st: Agent,
  property: PropertyKey,
  key: String,
  val: JsVal,
) -> Agent {
  let #(cache, st) = case cache_object(st, property) {
    Some(cache) -> #(cache, st)
    None -> {
      let #(cache, st) = rt_obj.t_new_object(st, None)
      let st =
        put_hidden_slot(st, st.realm.global_object, property, mk_object(cache))
      #(cache, st)
    }
  }
  put_hidden_slot(st, cache, Named(key), val)
}

/// Install (or replace) `key` as a raw own data slot of `target`, bypassing
/// [[DefineOwnProperty]]: like a class private element, a registry slot is
/// engine bookkeeping invisible to integrity levels, so a guest that froze or
/// sealed `globalThis` cannot make the loader refuse to record its state.
fn put_hidden_slot(
  st: Agent,
  target: Handle,
  key: PropertyKey,
  val: JsVal,
) -> Agent {
  let st = rt_obj.devolve(st, target)
  let #(seq, st) = rt_store.t_next_prop_seq(st)
  use slot <- rt_store.t_cell_update(st, target)
  case slot {
    SObject(props:, ..) ->
      SObject(
        ..slot,
        props: dict.insert(
          props,
          key,
          DataProperty(
            value: val,
            writable: True,
            enumerable: False,
            configurable: True,
            seq:,
          ),
        ),
      )
    // `devolve` left an object cell an `SObject`; the global and the cache
    // objects are never data cells.
    _ -> panic as "arc/module/registry: hidden slot target is not an object"
  }
}

/// Un-register `key` from the hidden cache object `property`. Every cache
/// clears by deleting the key, so "absent" has one meaning. Entries are
/// configurable data properties on an ordinary object, so [[Delete]] cannot
/// refuse: `False` is a broken invariant.
fn clear_entry(st: Agent, property: PropertyKey, key: String) -> Agent {
  case cache_object(st, property) {
    // The cache object does not exist yet, so nothing is registered under it.
    None -> st
    Some(cache) -> {
      let #(deleted, st) =
        rt_obj.t_delete_prop(st, cache, StringKey(Named(key)))
      case deleted {
        True -> st
        False -> panic as "arc/module/registry: cache entry refused deletion"
      }
    }
  }
}
