// module registry caches live as private-keyed props on the global

import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type Handle, type JsVal, type PropertyKey, DataProperty, KHandle,
  KStr, Named, SObject, StringKey, classify, mk_object, mk_string,
}
import gleam/dict
import gleam/option.{type Option, None, Some}

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

const referrer_key = "active"

pub fn read_active_referrer(st: Agent) -> Option(String) {
  use v <- option.then(read_entry(st, referrer_property(), referrer_key))
  case classify(v) {
    KStr(spec) -> Some(spec)
    _ -> None
  }
}

pub fn write_active_referrer(st: Agent, referrer: Option(String)) -> Agent {
  case referrer {
    Some(spec) ->
      write_entry(st, referrer_property(), referrer_key, mk_string(spec))
    None -> clear_entry(st, referrer_property(), referrer_key)
  }
}

pub type ModuleStatus {
  Evaluating
  Evaluated
}

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

pub fn clear_module_status(st: Agent, spec: String) -> Agent {
  clear_entry(st, status_property(), spec)
}

// §16.2.1.5.3 sticky; key presence, not value, marks a cached error
pub fn read_module_error(st: Agent, spec: String) -> Option(JsVal) {
  read_entry(st, error_cache_property(), spec)
}

pub fn write_module_error(st: Agent, spec: String, err: JsVal) -> Agent {
  write_entry(st, error_cache_property(), spec, err)
}

pub fn read_namespace(st: Agent, spec: String) -> Option(Handle) {
  read_object_entry(st, namespace_cache_property(), spec)
}

pub fn write_namespace(st: Agent, spec: String, namespace: Handle) -> Agent {
  write_entry(st, namespace_cache_property(), spec, mk_object(namespace))
}

fn clear_namespace(st: Agent, spec: String) -> Agent {
  clear_entry(st, namespace_cache_property(), spec)
}

pub fn read_deferred_namespace(st: Agent, spec: String) -> Option(Handle) {
  read_object_entry(st, deferred_cache_property(), spec)
}

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

pub fn clear_pending_promise(st: Agent, spec: String) -> Agent {
  clear_entry(st, pending_cache_property(), spec)
}

pub type CacheState {
  Failed(error: JsVal)
  Pending(promise: Handle, deferred: Option(Handle))
  Started(namespace: Handle, deferred: Option(Handle))
  LinkedOnly(namespace: Handle, deferred: Option(Handle))
  Absent(deferred: Option(Handle))
}

// precedence: sticky error, then tla promise, then namespace
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

pub fn clear_module_registrations(st: Agent, spec: String) -> Agent {
  st
  |> clear_module_status(spec)
  |> clear_namespace(spec)
  |> clear_deferred_namespace(spec)
}

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

fn read_entry(st: Agent, property: PropertyKey, key: String) -> Option(JsVal) {
  use cache <- option.then(cache_object(st, property))
  case rt_obj.t_ordinary_own_property(st, cache, StringKey(Named(key))) {
    Some(DataProperty(value:, ..)) -> Some(value)
    _ -> None
  }
}

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

// bypasses [[DefineOwnProperty]] so a frozen global cannot block it
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
    _ -> panic as "arc/module/registry: hidden slot target is not an object"
  }
}

fn clear_entry(st: Agent, property: PropertyKey, key: String) -> Agent {
  case cache_object(st, property) {
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
