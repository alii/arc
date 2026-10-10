//// what node's util.inspect and util.types ask of the engine: a look inside
//// values that javascript itself cannot take

import arc/host.{type Context, type HostMethod, HostMethod}
import arc/internal/ordered_entries
import arc/rt/bytecode.{type FnFlags}
import arc/rt/store as rt_store
import arc/rt/types.{
  type JsVal, type WeakKey, ArgumentsObj, ArrayBufferObj, ArrayObj,
  AsyncGeneratorObj, BigIntObj, BooleanObj, BoundFn, BytecodeFn, CompiledFn,
  DataViewObj, DateObj, ErrorObj, GeneratorObj, Handle, MapIterEntries,
  MapIterKeys, MapIterValues, MapIterator, MapObj, ModuleNamespace, NativeFn,
  NumberObj, PromiseFulfilled, PromiseObj, PromisePending, PromiseRejected,
  ProxyObj, RegExpObj, SObject, SPromiseData, SetIterEntries, SetIterValues,
  SetIterator, SetObj, Shared, StringObj, SymbolObj, TypedArrayObj, WeakMapObj,
  WeakObjKey, WeakSetObj, WeakSymKey, map_key_to_js, mk_bool, mk_int, mk_null,
  mk_object, mk_string, mk_symbol, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/set

type Reply(host) =
  #(Result(JsVal, JsVal), Context(host))

pub fn natives() -> List(HostMethod(host)) {
  [
    // the engine's name for what a value is, "" for a primitive
    HostMethod("kind", 1, fn(ctx, args, _this) {
      #(Ok(mk_string(kind(ctx, host.first_arg(args)))), ctx)
    }),
    // [0 pending | 1 fulfilled | 2 rejected, result], undefined if no promise
    HostMethod("promiseDetails", 1, fn(ctx, args, _this) {
      case promise_state(ctx, host.first_arg(args)) {
        Some(PromisePending(_)) -> pair(ctx, mk_int(0), mk_undefined())
        Some(PromiseFulfilled(value)) -> pair(ctx, mk_int(1), value)
        Some(PromiseRejected(reason)) -> pair(ctx, mk_int(2), reason)
        None -> #(Ok(mk_undefined()), ctx)
      }
    }),
    // [target, handler], both null once revoked; undefined if no proxy
    HostMethod("proxyDetails", 1, fn(ctx, args, _this) {
      case object_kind(ctx, host.first_arg(args)) {
        Some(ProxyObj(revoked: True, ..)) -> pair(ctx, mk_null(), mk_null())
        Some(ProxyObj(target:, handler:, revoked: False)) ->
          pair(ctx, mk_object(target), mk_object(handler))
        _ -> #(Ok(mk_undefined()), ctx)
      }
    }),
    // [what is left to visit as a flat list, whether it runs key, value, ...]
    // for a map or set iterator, and all of a weak map or weak set
    HostMethod("previewEntries", 1, fn(ctx, args, _this) {
      case preview(ctx, host.first_arg(args)) {
        None -> #(Ok(mk_undefined()), ctx)
        Some(#(entries, keyed)) -> {
          let #(entries, ctx) = host.array(ctx, entries)
          pair(ctx, entries, mk_bool(keyed))
        }
      }
    }),
  ]
}

fn pair(ctx: Context(host), a: JsVal, b: JsVal) -> Reply(host) {
  let #(array, ctx) = host.array(ctx, [a, b])
  #(Ok(array), ctx)
}

fn object_kind(
  ctx: Context(host),
  value: JsVal,
) -> option.Option(types.ObjKind) {
  use h <- option.then(rt_val.handle_of(value))
  case rt_store.cell_get(ctx.agent, h) {
    SObject(kind:, ..) -> Some(kind)
    _ -> None
  }
}

fn promise_state(
  ctx: Context(host),
  value: JsVal,
) -> option.Option(types.PromiseState) {
  case object_kind(ctx, value) {
    Some(PromiseObj(data:)) ->
      case rt_store.cell_get(ctx.agent, data) {
        SPromiseData(state:, ..) -> Some(state)
        _ -> None
      }
    _ -> None
  }
}

fn kind(ctx: Context(host), value: JsVal) -> String {
  case rt_val.handle_of(value) {
    None -> ""
    Some(_) ->
      case object_kind(ctx, value) {
        None -> "object"
        Some(kind) -> kind_name(kind)
      }
  }
}

fn kind_name(kind: types.ObjKind) -> String {
  case kind {
    ArrayObj(..) -> "array"
    ArgumentsObj(..) -> "arguments"
    StringObj(..) -> "string_object"
    NumberObj(..) -> "number_object"
    BooleanObj(..) -> "boolean_object"
    BigIntObj(..) -> "bigint_object"
    SymbolObj(..) -> "symbol_object"
    BytecodeFn(flags:, ..) | CompiledFn(flags:, ..) -> function_name(flags)
    NativeFn(..) | BoundFn(..) -> "function"
    ErrorObj(..) -> "error"
    MapObj(..) -> "map"
    SetObj(..) -> "set"
    WeakMapObj(..) -> "weak_map"
    WeakSetObj(..) -> "weak_set"
    DateObj(..) -> "date"
    RegExpObj(..) -> "regexp"
    ArrayBufferObj(storage: Shared(..)) -> "shared_array_buffer"
    ArrayBufferObj(..) -> "array_buffer"
    TypedArrayObj(..) -> "typed_array"
    DataViewObj(..) -> "data_view"
    ModuleNamespace(..) -> "module_namespace"
    ProxyObj(..) -> "proxy"
    MapIterator(..) -> "map_iterator"
    SetIterator(..) -> "set_iterator"
    PromiseObj(..) -> "promise"
    GeneratorObj(..) | AsyncGeneratorObj(..) -> "generator_object"
    _ -> "object"
  }
}

fn function_name(flags: FnFlags) -> String {
  case flags.is_async, flags.is_generator {
    True, True -> "async_generator_function"
    True, False -> "async_function"
    False, True -> "generator_function"
    False, False -> "function"
  }
}

fn preview(
  ctx: Context(host),
  value: JsVal,
) -> option.Option(#(List(JsVal), Bool)) {
  case object_kind(ctx, value) {
    Some(MapIterator(target:, index:, kind:)) -> {
      let left = case rt_store.cell_get(ctx.agent, target) {
        SObject(kind: MapObj(entries:), ..) -> rest(entries, index, [])
        _ -> []
      }
      Some(case kind {
        MapIterKeys -> #(list.map(left, fn(e) { map_key_to_js(e.0) }), False)
        MapIterValues -> #(list.map(left, fn(e) { e.1 }), False)
        MapIterEntries -> #(
          list.flat_map(left, fn(e) { [map_key_to_js(e.0), e.1] }),
          True,
        )
      })
    }
    Some(SetIterator(target:, index:, kind:)) -> {
      let left = case rt_store.cell_get(ctx.agent, target) {
        SObject(kind: SetObj(entries:), ..) -> rest(entries, index, [])
        _ -> []
      }
      Some(case kind {
        SetIterValues -> #(list.map(left, fn(e) { e.1 }), False)
        SetIterEntries -> #(list.flat_map(left, fn(e) { [e.1, e.1] }), True)
      })
    }
    Some(WeakMapObj(entries:)) ->
      Some(#(
        dict.to_list(entries)
          |> list.flat_map(fn(e) { [weak_key_value(e.0), e.1] }),
        True,
      ))
    Some(WeakSetObj(entries:)) ->
      Some(#(set.to_list(entries) |> list.map(weak_key_value), False))
    _ -> None
  }
}

// a finished iterator sits at -1
fn rest(
  entries: ordered_entries.OrderedEntries(k, v),
  cursor: Int,
  found: List(#(k, v)),
) -> List(#(k, v)) {
  case cursor < 0 {
    True -> []
    False ->
      case ordered_entries.next_from(entries, cursor) {
        None -> list.reverse(found)
        Some(#(next, key, value)) ->
          rest(entries, next, [#(key, value), ..found])
      }
  }
}

fn weak_key_value(key: WeakKey) -> JsVal {
  case key {
    WeakObjKey(id:) -> mk_object(Handle(id))
    WeakSymKey(id:) -> mk_symbol(id)
  }
}
