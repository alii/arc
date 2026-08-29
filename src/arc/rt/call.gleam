import arc/bytecode/key.{type PropertyKey, Named}
import arc/internal/tree_array
import arc/rt/limits
import arc/rt/obj as rt_obj
import arc/rt/store as rt_store
import arc/rt/types.{
  type Agent, type CompiledFn, type FnFlags, type Handle, type JsOps, type JsVal,
  type NativeToken, type ObjKind, type Property, type Realm, Agent, ArrayObj,
  BirthPending, BirthSettled, DataProperty, Dense, JInt, JPosInf, KBound,
  KBytecode, KCompiled, KHandle, KNative, KNull, KNum, KStr, KTdz, KUndef,
  NoElements, ProxyObj, ReferenceErr, SObject, StringKey, TypeErr, classify,
  mk_number, mk_object, mk_tdz, mk_undefined,
} as rt_types
import arc/rt/val as rt_val
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// plain erlang 4-tuple {this, func, home, new_target}, not a record
pub type Frame

@external(erlang, "arc_rt_call_ffi", "mk_frame")
fn mk_frame(
  this: JsVal,
  active_func: JsVal,
  home_object: JsVal,
  new_target: JsVal,
) -> Frame

@external(erlang, "erlang", "element")
fn frame_element(n: Int, frame: Frame) -> JsVal

pub fn frame_active_func(frame: Frame) -> JsVal {
  frame_element(2, frame)
}

pub type Completion {
  NormalCompletion(JsVal)
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_call_protected")
fn t_call_protected(
  st: Agent,
  code: CompiledFn,
  frame: Frame,
  args: List(JsVal),
) -> #(Completion, Agent)

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn t_apply_protected(
  st: Agent,
  body: fn(Agent) -> #(JsVal, Agent),
) -> #(Completion, Agent)

@external(erlang, "arc_rt_call_ffi", "t_native_protected")
fn t_native_protected(
  st: Agent,
  tag: NativeToken,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent)

@external(erlang, "arc_rt_builtins_ffi", "dispatch_native_construct")
fn dispatch_native_construct(
  st: Agent,
  tag: NativeToken,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent)

fn js_ops(st: Agent) -> JsOps(Agent) {
  st.store.ops
}

fn throw_error(st: Agent, kind: rt_types.ErrorKind, msg: String) -> a {
  let #(e, st) = js_ops(st).new_error(st, kind, msg)
  rt_store.t_throw(st, e)
}

fn read_obj_kind(st: Agent, h: Handle) -> Option(ObjKind) {
  case rt_store.t_cell_get(st, h) {
    SObject(kind:, ..) -> Some(kind)
    rt_types.SShapedObject(..) -> Some(rt_types.Ordinary)
    _ -> None
  }
}

pub fn is_callable(st: Agent, v: JsVal) -> Bool {
  let #(b, _) = rt_val.t_is_callable(st, v)
  b
}

// §7.2.4 isconstructor
pub fn is_constructor(st: Agent, v: JsVal) -> Bool {
  case classify(v) {
    KHandle(h) -> handle_is_constructor(st, h)
    _ -> False
  }
}

fn handle_is_constructor(st: Agent, h: Handle) -> Bool {
  case read_obj_kind(st, h) {
    Some(KCompiled(flags:, ..)) | Some(KBytecode(flags:, ..)) ->
      flags.is_constructor
    Some(KNative(constructible:, ..)) -> constructible
    Some(KBound(target:, ..)) -> handle_is_constructor(st, target)
    // §10.5.15 step 7: stays installed after revocation
    Some(ProxyObj(target:, ..)) -> handle_is_constructor(st, target)
    _ -> False
  }
}

// fast path probe: ordinary compiled fn only, else undefined
@external(erlang, "arc_rt_call_ffi", "t_kfn_code")
pub fn t_kfn_code(st: Agent, callee: JsVal, this: JsVal) -> JsVal

// §10.2.1 [[call]], catches a throw into a completion
pub fn t_call(
  st: Agent,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent) {
  case classify(callee) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: KBytecode(..) as kind, ..) -> {
          let #(res, st) = js_ops(st).call_bytecode(st, h, kind, this, args)
          case res {
            Ok(v) -> #(NormalCompletion(v), st)
            Error(e) -> #(ThrowCompletion(e), st)
          }
        }
        slot -> call_slot(st, callee, h, slot, this, args)
      }
    _ -> bracketed(st, fn(st) { not_a_function(st, callee) })
  }
}

fn call_slot(
  st: Agent,
  callee: JsVal,
  h: Handle,
  slot: rt_types.JsSlot,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent) {
  case slot {
    SObject(kind: KCompiled(code:, home_object:, flags:, ..), ..) -> {
      use st <- bracketed(st)
      call_kfunction(st, h, code, home_object, flags, this, args)
    }
    SObject(kind: KNative(tag:, ..), ..) -> {
      use st <- bracketed(st)
      t_native_protected(st, tag, this, args)
    }
    SObject(kind: KBound(target:, bound_this:, bound_args:), ..) ->
      t_call(st, mk_object(target), bound_this, list.append(bound_args, args))
    SObject(kind: ProxyObj(target:, handler:, revoked:), ..) -> {
      use st <- bracketed(st)
      call_proxy(st, callee, target, handler, revoked, this, args)
    }
    _ -> bracketed(st, fn(st) { not_a_function(st, callee) })
  }
}

fn bracketed(
  st: Agent,
  body: fn(Agent) -> #(Completion, Agent),
) -> #(Completion, Agent) {
  let depth = st.call_depth
  case depth >= limits.max_call_depth {
    True -> t_apply_protected(st, rt_store.stack_overflow)
    False -> {
      let #(c, st) = body(Agent(..st, call_depth: depth + 1))
      #(c, Agent(..st, call_depth: st.call_depth - 1))
    }
  }
}

fn call_kfunction(
  st: Agent,
  callee_h: Handle,
  code: CompiledFn,
  home_object: Option(Handle),
  flags: FnFlags,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent) {
  case flags.is_class_constructor {
    True ->
      t_apply_protected(st, fn(st) {
        throw_error(
          st,
          TypeErr,
          "Class constructor cannot be invoked without 'new'",
        )
      })
    False -> {
      let home = case home_object {
        Some(h) -> mk_object(h)
        None -> mk_undefined()
      }
      let #(this_resolved, st) = resolve_this(st, flags, this)
      let frame =
        mk_frame(this_resolved, mk_object(callee_h), home, mk_undefined())
      t_call_protected(st, code, frame, args)
    }
  }
}

// §10.2.1.2 ordinarycallbindthis, arrows keep lexical this
pub fn resolve_this(st: Agent, flags: FnFlags, this: JsVal) -> #(JsVal, Agent) {
  case flags.is_arrow || flags.is_strict {
    True -> #(this, st)
    False ->
      case classify(this) {
        KUndef | KNull -> #(mk_object(st.realm.global_object), st)
        KHandle(_) -> #(this, st)
        // tdz sentinel must not escape as this
        KTdz -> panic as "TDZ sentinel escaped as `this` in resolve_this"
        _ -> {
          let #(h, st) = js_ops(st).to_object(st, this)
          #(mk_object(h), st)
        }
      }
  }
}

// §10.5.12 proxy [[call]]
fn call_proxy(
  st: Agent,
  callee: JsVal,
  target: Handle,
  handler: Handle,
  revoked: Bool,
  this: JsVal,
  args: List(JsVal),
) -> #(Completion, Agent) {
  t_apply_protected(st, fn(st) {
    use <- bool.lazy_guard(!is_callable(st, mk_object(target)), fn() {
      not_a_function_raise(st, callee)
    })
    let #(trap, st) = proxy_trap(st, handler, revoked, "apply")
    case trap {
      None -> t_call_checked(st, mk_object(target), this, args)
      Some(trap_fn) -> {
        let #(args_arr, st) = alloc_args_array(st, args)
        t_call_checked(st, trap_fn, mk_object(handler), [
          mk_object(target),
          this,
          mk_object(args_arr),
        ])
      }
    }
  })
}

// §10.5.14 + getmethod; none means forward to target
fn proxy_trap(
  st: Agent,
  handler: Handle,
  revoked: Bool,
  name: String,
) -> #(Option(JsVal), Agent) {
  use <- bool.lazy_guard(revoked, fn() {
    throw_error(
      st,
      TypeErr,
      "Cannot perform '" <> name <> "' on a proxy that has been revoked",
    )
  })
  let #(trap, st) =
    rt_obj.t_get_prop(st, mk_object(handler), StringKey(Named(name)))
  case classify(trap) {
    KUndef | KNull -> #(None, st)
    _ ->
      case is_callable(st, trap) {
        True -> #(Some(trap), st)
        False ->
          throw_error(
            st,
            TypeErr,
            "'" <> name <> "' trap of proxy handler is not a function",
          )
      }
  }
}

fn not_a_function(st: Agent, callee: JsVal) -> #(Completion, Agent) {
  t_apply_protected(st, fn(st) { not_a_function_raise(st, callee) })
}

fn not_a_function_raise(st: Agent, callee: JsVal) -> a {
  let #(ty, _) = rt_val.t_type_of(st, callee)
  throw_error(st, TypeErr, ty <> " is not a function")
}

// rethrows; the fn seeded into jsops.call
pub fn t_call_checked(
  st: Agent,
  callee: JsVal,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case classify(callee) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: KBytecode(..) as kind, ..) ->
          case js_ops(st).call_bytecode(st, h, kind, this, args) {
            #(Ok(v), st) -> #(v, st)
            #(Error(e), st) -> rt_store.t_throw(st, e)
          }
        slot -> rethrown(call_slot(st, callee, h, slot, this, args))
      }
    _ -> rethrown(bracketed(st, fn(st) { not_a_function(st, callee) }))
  }
}

pub fn t_bind_call(
  st: Agent,
  callee: JsVal,
  this: JsVal,
) -> fn(Agent, List(JsVal)) -> #(JsVal, Agent) {
  let generic = fn(st, args) { t_call_checked(st, callee, this, args) }
  case classify(callee) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: KBytecode(..) as kind, ..) ->
          js_ops(st).bind_call(st, h, kind, this)
        SObject(kind: KNative(tag:, ..), ..) -> fn(st, args) {
          call_native(st, tag, this, args)
        }
        SObject(kind: KCompiled(code:, home_object:, flags:, ..), ..) -> fn(
          st,
          args,
        ) {
          call_compiled(st, h, code, home_object, flags, this, args)
        }
        _ -> generic
      }
    _ -> generic
  }
}

fn call_compiled(
  st: Agent,
  h: Handle,
  code: CompiledFn,
  home_object: Option(Handle),
  flags: FnFlags,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st <- rethrown_bracket(st)
  call_kfunction(st, h, code, home_object, flags, this, args)
}

fn call_native(
  st: Agent,
  tag: NativeToken,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st <- rethrown_bracket(st)
  t_native_protected(st, tag, this, args)
}

fn rethrown_bracket(
  st: Agent,
  body: fn(Agent) -> #(Completion, Agent),
) -> #(JsVal, Agent) {
  rethrown(bracketed(st, body))
}

pub fn t_bind_callable(
  st: Agent,
  callee: JsVal,
  this: JsVal,
) -> Option(fn(Agent, List(JsVal)) -> #(JsVal, Agent)) {
  let generic = fn(st, args) { t_call_checked(st, callee, this, args) }
  case classify(callee) {
    KHandle(h) ->
      case rt_store.t_cell_get(st, h) {
        SObject(kind: KBytecode(..) as kind, ..) ->
          Some(js_ops(st).bind_call(st, h, kind, this))
        SObject(kind: KNative(tag:, ..), ..) ->
          Some(fn(st, args) { call_native(st, tag, this, args) })
        SObject(kind: KCompiled(code:, home_object:, flags:, ..) as kind, ..) ->
          bind_compiled(st, callee, kind, this)
          |> option.or(
            Some(fn(st, args) {
              call_compiled(st, h, code, home_object, flags, this, args)
            }),
          )
        SObject(kind: KBound(..), ..) -> Some(generic)
        SObject(kind: ProxyObj(target:, ..), ..) ->
          case is_callable(st, mk_object(target)) {
            True -> Some(generic)
            False -> None
          }
        _ -> None
      }
    _ -> None
  }
}

@external(erlang, "arc_rt_call_fast_ffi", "t_bind_compiled")
fn bind_compiled(
  st: Agent,
  callee: JsVal,
  kind: ObjKind,
  this: JsVal,
) -> Option(fn(Agent, List(JsVal)) -> #(JsVal, Agent))

fn rethrown(outcome: #(Completion, Agent)) -> #(JsVal, Agent) {
  case outcome {
    #(NormalCompletion(v), st) -> #(v, st)
    #(ThrowCompletion(e), st) -> rt_store.t_throw(st, e)
  }
}

pub fn t_call_method(
  st: Agent,
  recv: JsVal,
  key: rt_types.ObjectKey,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let #(callee, st) = rt_obj.t_get_prop(st, recv, key)
  t_call_checked(st, callee, recv, args)
}

// §10.2.2 [[construct]], isconstructor gate first
pub fn t_construct(
  st: Agent,
  callee: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case classify(callee) {
    KHandle(callee_h) ->
      case handle_is_constructor(st, callee_h) {
        False -> not_a_constructor(st, callee)
        True -> construct_by_kind(st, callee_h, args, new_target)
      }
    _ -> not_a_constructor(st, callee)
  }
}

fn not_a_constructor(st: Agent, callee: JsVal) -> a {
  let #(ty, _) = rt_val.t_type_of(st, callee)
  throw_error(st, TypeErr, ty <> " is not a constructor")
}

fn construct_by_kind(
  st: Agent,
  callee_h: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  case read_obj_kind(st, callee_h) {
    Some(KCompiled(code:, home_object:, flags:, fields_init:, ..)) ->
      construct_kfunction(
        st,
        callee_h,
        code,
        home_object,
        flags,
        fields_init,
        args,
        new_target,
      )
    Some(KBytecode(..)) ->
      js_ops(st).construct_bytecode(st, callee_h, args, new_target)
    Some(KNative(tag:, ..)) ->
      dispatch_native_construct(st, tag, args, new_target)
    Some(KBound(target:, bound_args:, ..)) -> {
      let nt = case classify(new_target) {
        KHandle(nt_h) if nt_h == callee_h -> mk_object(target)
        _ -> new_target
      }
      t_construct(st, mk_object(target), list.append(bound_args, args), nt)
    }
    Some(ProxyObj(target:, handler:, revoked:)) ->
      construct_proxy(st, target, handler, revoked, args, new_target)
    _ ->
      panic as "t_construct: IsConstructor passed but ObjKind not constructible"
  }
}

fn construct_kfunction(
  st: Agent,
  callee_h: Handle,
  code: CompiledFn,
  home_object: Option(Handle),
  flags: FnFlags,
  fields_init: Option(Handle),
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let callee_v = mk_object(callee_h)
  let home = case home_object {
    Some(h) -> mk_object(h)
    None -> mk_undefined()
  }
  case flags.is_derived_constructor {
    True -> {
      let frame = mk_frame(mk_tdz(), callee_v, home, new_target)
      let #(c, st) = apply_ctor(st, code, frame, args)
      derived_return_override(st, c)
    }
    False -> {
      let #(proto, st) =
        get_prototype_from_constructor(st, new_target, object_prototype)
      let #(new_this, st) = rt_obj.t_new_receiver(st, proto)
      let st = run_fields_init(st, fields_init, new_this)
      let frame = mk_frame(mk_object(new_this), callee_v, home, new_target)
      let #(c, st) = apply_ctor(st, code, frame, args)
      base_return_override(st, c, new_this)
    }
  }
}

fn apply_ctor(
  st: Agent,
  code: CompiledFn,
  frame: Frame,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  let st = rt_store.t_enter_call(st)
  let #(c, st) = t_call_protected(st, code, frame, args)
  let st = rt_store.t_leave_call(st)
  case c {
    NormalCompletion(v) -> #(v, st)
    ThrowCompletion(e) -> rt_store.t_throw(st, e)
  }
}

fn base_return_override(
  st: Agent,
  result: JsVal,
  new_this: Handle,
) -> #(Handle, Agent) {
  case classify(result) {
    KHandle(h) -> #(h, st)
    _ -> #(new_this, st)
  }
}

// emit rewrites undefined returns to this_local
fn derived_return_override(st: Agent, result: JsVal) -> #(Handle, Agent) {
  case classify(result) {
    KHandle(h) -> #(h, st)
    KTdz ->
      throw_error(
        st,
        ReferenceErr,
        "Must call super constructor in derived class before returning from derived constructor",
      )
    KUndef ->
      panic as "derived ctor returned KUndef — M18 return-lowering contract violated"
    _ ->
      throw_error(
        st,
        TypeErr,
        "Derived constructors may only return object or undefined",
      )
  }
}

// §10.1.14 getprototypefromconstructor, undefined newtarget uses current realm
pub fn get_prototype_from_constructor(
  st: Agent,
  constructor: JsVal,
  intrinsic: fn(Realm) -> Handle,
) -> #(Handle, Agent) {
  case classify(constructor) {
    KHandle(ctor_h) -> {
      let #(proto, st) =
        rt_obj.t_get_prop(st, constructor, StringKey(Named("prototype")))
      case classify(proto) {
        KHandle(h) -> #(h, st)
        _ -> #(intrinsic(function_realm(st, ctor_h)), st)
      }
    }
    _ -> #(intrinsic(st.realm), st)
  }
}

pub fn object_prototype(realm: Realm) -> Handle {
  realm.object.prototype
}

pub fn realm_by_id(st: Agent, id: Int) -> Realm {
  use <- bool.guard(id == st.realm.id, st.realm)
  case dict.get(st.realms, id) {
    Ok(r) -> r
    Error(Nil) ->
      panic as { "rt/call.realm_by_id: no realm with id " <> int.to_string(id) }
  }
}

pub fn function_realm(st: Agent, obj: Handle) -> Realm {
  realm_by_id(st, get_function_realm(st, obj))
}

// §7.3.24 getfunctionrealm as a realm id
pub fn get_function_realm(st: Agent, obj: Handle) -> Int {
  case rt_store.t_cell_get(st, obj) {
    SObject(kind: KBytecode(realm:, ..), ..) -> realm
    SObject(kind: KBound(target:, ..), ..) -> get_function_realm(st, target)
    SObject(kind: ProxyObj(revoked: True, ..), ..) ->
      throw_error(
        st,
        TypeErr,
        "Cannot perform 'getFunctionRealm' on a proxy that has been revoked",
      )
    SObject(kind: ProxyObj(target:, ..), ..) -> get_function_realm(st, target)
    SObject(kind: KNative(tag:, ..), proto:, ..) ->
      case native_realm(tag) {
        Some(id) -> id
        None -> realm_of_function_proto(st, proto)
      }
    SObject(proto:, ..) -> realm_of_function_proto(st, proto)
    _ -> st.realm.id
  }
}

fn native_realm(tag: NativeToken) -> Option(Int) {
  case tag {
    rt_types.GlobalN(rt_types.GlobalEval(realm:))
    | rt_types.FunctionN(rt_types.FunctionConstructor(realm:))
    | rt_types.GeneratorN(rt_types.GeneratorFunctionCtor(realm:))
    | rt_types.GeneratorN(rt_types.AsyncFunctionCtor(realm:))
    | rt_types.GeneratorN(rt_types.AsyncGeneratorFunctionCtor(realm:))
    | rt_types.JsonN(rt_types.JsonParse(realm:))
    | rt_types.JsonN(rt_types.JsonStringify(realm:))
    | rt_types.JsonN(rt_types.JsonRawJson(realm:))
    | rt_types.JsonN(rt_types.JsonIsRawJson(realm:))
    | rt_types.ErrorN(rt_types.ErrorStackSetter(realm:))
    | rt_types.Test262N(rt_types.Test262EvalScript(realm:))
    | rt_types.Test262N(rt_types.Test262CreateRealm(realm:))
    | rt_types.ShadowRealmN(rt_types.ShadowRealmEvaluate(realm:))
    | rt_types.ShadowRealmN(rt_types.ShadowRealmImportValue(realm:))
    | rt_types.ShadowRealmN(rt_types.WrappedFunctionCall(
        caller_realm: realm,
        ..,
      )) -> Some(realm)
    _ -> None
  }
}

fn realm_of_function_proto(st: Agent, proto: Option(Handle)) -> Int {
  let current = st.realm
  case proto {
    None -> current.id
    Some(p) -> {
      use <- bool.guard(is_function_proto_of(st, current, p), current.id)
      let found =
        list.find(dict.values(st.realms), fn(r) {
          r.id != current.id && is_function_proto_of(st, r, p)
        })
      case found {
        Ok(r) -> r.id
        Error(Nil) -> current.id
      }
    }
  }
}

fn is_function_proto_of(st: Agent, realm: rt_types.Realm, p: Handle) -> Bool {
  p == realm.function.prototype
  || p == realm.generator_fn.prototype
  || p == realm.async_fn.prototype
  || p == async_generator_fn_prototype(st, realm)
}

pub fn async_generator_fn_prototype(
  st: Agent,
  realm: rt_types.Realm,
) -> Handle {
  case
    rt_obj.t_ordinary_own_property(
      st,
      realm.async_gen.constructor,
      StringKey(Named("prototype")),
    )
  {
    Some(DataProperty(value:, ..)) ->
      case classify(value) {
        KHandle(p) -> p
        _ -> realm.function.prototype
      }
    _ -> realm.function.prototype
  }
}

// §7.3.33 initializeinstanceelements
fn run_fields_init(
  st: Agent,
  fields_init: Option(Handle),
  new_this: Handle,
) -> Agent {
  case fields_init {
    None -> st
    Some(init_h) -> {
      let #(_, st) =
        t_call_checked(st, mk_object(init_h), mk_object(new_this), [])
      st
    }
  }
}

// §10.5.13 proxy [[construct]]
fn construct_proxy(
  st: Agent,
  target: Handle,
  handler: Handle,
  revoked: Bool,
  args: List(JsVal),
  new_target: JsVal,
) -> #(Handle, Agent) {
  let #(trap, st) = proxy_trap(st, handler, revoked, "construct")
  case trap {
    None -> t_construct(st, mk_object(target), args, new_target)
    Some(trap_fn) -> {
      let #(args_arr, st) = alloc_args_array(st, args)
      let #(res, st) =
        t_call_checked(st, trap_fn, mk_object(handler), [
          mk_object(target),
          mk_object(args_arr),
          new_target,
        ])
      case classify(res) {
        KHandle(h) -> #(h, st)
        _ ->
          throw_error(
            st,
            TypeErr,
            "'construct' on proxy: trap returned non-object",
          )
      }
    }
  }
}

fn alloc_args_array(st: Agent, items: List(JsVal)) -> #(Handle, Agent) {
  let len = list.length(items)
  let elements = case items {
    [] -> NoElements
    _ -> Dense(tree_array.from_list(items))
  }
  rt_store.t_cell_new(
    st,
    SObject(
      kind: ArrayObj(length: len),
      proto: Some(st.realm.array.prototype),
      props: dict.new(),
      symbol_props: [],
      elements:,
      extensible: True,
    ),
  )
}

// birth props take seq 0,1,2; prop_seq starts past them
// length is a jsval so bind can install +infinity
pub fn fn_own_prop(value: JsVal, seq: Int) -> Property {
  DataProperty(
    value:,
    writable: False,
    enumerable: False,
    configurable: True,
    seq:,
  )
}

@external(erlang, "arc_rt_call_ffi", "birth_props")
pub fn birth_props(length_v: JsVal, name: String) -> Dict(PropertyKey, Property)

fn alloc_fn_cell(
  st: Agent,
  proto: Option(Handle),
  kind: ObjKind,
  length_v: JsVal,
  name: String,
) -> #(Handle, Agent) {
  rt_store.t_cell_new(
    st,
    SObject(
      kind:,
      proto:,
      props: birth_props(length_v, name),
      symbol_props: [],
      elements: NoElements,
      extensible: True,
    ),
  )
}

// no .prototype here, makeconstructor is separate
pub fn t_fn_new(
  st: Agent,
  code: CompiledFn,
  flags: FnFlags,
  name: String,
  len: Int,
  home: Option(Handle),
  simple: Option(#(CompiledFn, Int, Bool)),
) -> #(Handle, Agent) {
  alloc_fn_cell(
    st,
    Some(st.realm.function.prototype),
    KCompiled(
      code:,
      home_object: home,
      flags:,
      fields_init: None,
      simple:,
      name:,
      length: len,
      birth: BirthSettled,
    ),
    mk_number(JInt(len)),
    name,
  )
}

// closure site of every compiled function
pub fn t_new_function(
  st: Agent,
  code: CompiledFn,
  flags: FnFlags,
  name: String,
  len: Int,
  simple: Option(#(CompiledFn, Int, Bool)),
) -> #(JsVal, Agent) {
  let realm = st.realm
  let proto = case flags.is_generator, flags.is_async {
    True, False -> realm.generator_fn.prototype
    True, True -> async_generator_fn_prototype(st, realm)
    False, True -> realm.async_fn.prototype
    False, False -> realm.function.prototype
  }
  let prototype_parent = case
    flags.is_constructor && !flags.is_class_constructor && !flags.is_generator
  {
    True -> Some(realm.object.prototype)
    False -> None
  }
  let #(h, st) =
    rt_store.t_cell_new(
      st,
      SObject(
        kind: KCompiled(
          code:,
          home_object: None,
          flags:,
          fields_init: None,
          simple:,
          name:,
          length: len,
          birth: BirthPending(prototype_parent),
        ),
        proto: Some(proto),
        props: dict.new(),
        symbol_props: [],
        elements: NoElements,
        extensible: True,
      ),
    )
  let st = case flags.is_generator {
    False -> st
    True -> {
      let gen_proto = case flags.is_async {
        True -> realm.async_gen.prototype
        False -> realm.generator.prototype
      }
      let #(own_proto, st) = rt_obj.t_new_object(st, Some(gen_proto))
      let #(_, st) =
        rt_obj.t_define_own_data(
          st,
          h,
          StringKey(Named("prototype")),
          mk_object(own_proto),
          True,
          False,
          False,
        )
      st
    }
  }
  #(mk_object(h), st)
}

// proto is explicit: called during bootstrap
pub fn t_native_new(
  st: Agent,
  proto: Option(Handle),
  tag: NativeToken,
  name: String,
  len: Int,
  constructible: Bool,
) -> #(Handle, Agent) {
  alloc_fn_cell(
    st,
    proto,
    KNative(tag:, name:, length: len, constructible:),
    mk_number(JInt(len)),
    name,
  )
}

// length may be +infinity (wrappedfunctioncreate)
pub fn t_native_new_computed_length(
  st: Agent,
  proto: Option(Handle),
  tag: NativeToken,
  name: String,
  length_v: JsVal,
) -> #(Handle, Agent) {
  let length = case classify(length_v) {
    KNum(JInt(n)) -> n
    _ -> 0
  }
  alloc_fn_cell(
    st,
    proto,
    KNative(tag:, name:, length:, constructible: False),
    length_v,
    name,
  )
}

// §20.2.3.2 bind steps 3-10; caller checks iscallable
pub fn t_bound_new(
  st: Agent,
  target: Handle,
  bound_this: JsVal,
  bound_args: List(JsVal),
) -> #(Handle, Agent) {
  let target_v = mk_object(target)
  let #(own_length, st) =
    rt_obj.t_get_own_property(st, target, StringKey(Named("length")))
  let #(target_len, st) = case own_length {
    Some(_) -> rt_obj.t_get_prop(st, target_v, StringKey(Named("length")))
    None -> #(mk_undefined(), st)
  }
  let n_args = list.length(bound_args)
  let length_v = case classify(target_len) {
    KNum(JPosInf) -> mk_number(JPosInf)
    KNum(n) ->
      mk_number(
        JInt(int.max(rt_val.jsnum_to_integer_or_infinity(n) - n_args, 0)),
      )
    _ -> mk_number(JInt(0))
  }
  let #(target_name, st) =
    rt_obj.t_get_prop(st, target_v, StringKey(Named("name")))
  let bound_name = case classify(target_name) {
    KStr(s) -> "bound " <> s
    _ -> "bound "
  }
  alloc_fn_cell(
    st,
    Some(st.realm.function.prototype),
    KBound(target:, bound_this:, bound_args:),
    length_v,
    bound_name,
  )
}
