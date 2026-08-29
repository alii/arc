import arc/bytecode/key.{Named}
import arc/compiler
import arc/parser
import arc/rt/async as rt_async
import arc/rt/builtins/common
import arc/rt/builtins/helpers.{first_arg_or_undefined}
import arc/rt/builtins/realm_ops
import arc/rt/call as rt_call
import arc/rt/inspect
import arc/rt/obj as rt_obj
import arc/rt/realm as rt_realm
import arc/rt/types.{
  type Agent, type BuiltinPair, type Handle, type JsVal, type Realm,
  type ShadowRealmNative, IndirectEval, JInt, JPosInf, KHandle, KNum, KStr,
  ShadowRealmConstructor, ShadowRealmEvaluate, ShadowRealmImportValue,
  ShadowRealmN, ShadowRealmObj, StringKey, TypeErr, WrappedFunctionCall,
  classify, mk_number, mk_object, mk_undefined,
}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

pub fn init(
  st: Agent,
  object_proto: Handle,
  fn_proto: Handle,
  realm: Int,
) -> #(BuiltinPair, Agent) {
  let #(methods, st) =
    common.alloc_methods(st, fn_proto, [
      #("evaluate", ShadowRealmN(ShadowRealmEvaluate(realm:)), 1),
      #("importValue", ShadowRealmN(ShadowRealmImportValue(realm:)), 2),
    ])
  let #(shadow_realm, st) =
    common.init_type(
      st,
      object_proto,
      fn_proto,
      methods,
      fn(proto) { ShadowRealmN(ShadowRealmConstructor(proto:)) },
      "ShadowRealm",
      0,
      [],
    )
  let st = common.add_to_string_tag(st, shadow_realm.prototype, "ShadowRealm")
  #(shadow_realm, st)
}

pub fn dispatch(
  st: Agent,
  n: ShadowRealmNative,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  case n {
    ShadowRealmConstructor(..) ->
      rt_val.t_throw_type_error(st, "Constructor ShadowRealm requires 'new'")
    ShadowRealmEvaluate(realm:) -> evaluate(st, realm, this, args)
    ShadowRealmImportValue(realm:) -> import_value(st, realm, this, args)
    WrappedFunctionCall(target:, caller_realm:, target_realm:) ->
      wrapped_function_call(st, target, caller_realm, target_realm, this, args)
  }
}

/// create_realm passed in to avoid an import cycle
pub fn dispatch_construct(
  st: Agent,
  n: ShadowRealmNative,
  new_target: JsVal,
  create_realm: fn(Agent) -> #(Realm, Agent),
) -> #(Handle, Agent) {
  case n {
    ShadowRealmConstructor(proto:) ->
      construct(st, proto, new_target, create_realm)
    ShadowRealmEvaluate(..)
    | ShadowRealmImportValue(..)
    | WrappedFunctionCall(..) ->
      rt_val.t_throw_type_error(st, "not a constructor")
  }
}

// §3.2.1 shadowrealm ( )
fn construct(
  st: Agent,
  fallback_proto: Handle,
  new_target: JsVal,
  create_realm: fn(Agent) -> #(Realm, Agent),
) -> #(Handle, Agent) {
  let #(proto, st) =
    rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) {
      fallback_proto
    })
  let #(realm, st) = create_realm(st)
  realm_ops.alloc_wrapper(st, ShadowRealmObj(realm: realm.id), proto)
}

// §3.1.1 validateshadowrealmobject
fn require_shadow_realm(st: Agent, this: JsVal, method: String) -> Int {
  let brand =
    helpers.brand_of(st, this, fn(kind) {
      case kind {
        ShadowRealmObj(realm:) -> Some(realm)
        _ -> None
      }
    })
  case brand {
    Some(#(realm, _)) -> realm
    None ->
      rt_val.t_throw_type_error(
        st,
        "ShadowRealm.prototype." <> method <> " called on incompatible receiver",
      )
  }
}

type Outcome(a) {
  NormalCompletion(a)
  ThrowCompletion(JsVal)
}

@external(erlang, "arc_rt_call_ffi", "t_apply_protected")
fn protected(st: Agent, body: fn(Agent) -> #(a, Agent)) -> #(Outcome(a), Agent)

fn protected_in_realm(
  st: Agent,
  id: Int,
  body: fn(Agent) -> #(a, Agent),
) -> #(Outcome(a), Agent) {
  use st <- protected(st)
  rt_realm.with_realm(st, id, body)
}

// §2.3 getwrappedvalue, from source realm into target realm
fn get_wrapped_value(
  st: Agent,
  from: Int,
  into: Int,
  val: JsVal,
) -> #(JsVal, Agent) {
  case classify(val) {
    KHandle(h) ->
      case rt_call.is_callable(st, val) {
        True -> wrapped_function_create(st, from, into, h)
        False ->
          rt_val.t_throw_type_error(
            st,
            "value crossing the ShadowRealm boundary must be callable or primitive",
          )
      }
    _ -> #(val, st)
  }
}

fn wrap_all(
  st: Agent,
  from: Int,
  into: Int,
  vals: List(JsVal),
) -> #(List(JsVal), Agent) {
  let #(wrapped, st) =
    list.fold(vals, #([], st), fn(acc, v) {
      let #(done, st) = acc
      let #(w, st) = get_wrapped_value(st, from, into, v)
      #([w, ..done], st)
    })
  #(list.reverse(wrapped), st)
}

// §2.2 wrappedfunctioncreate, into is callerrealm
fn wrapped_function_create(
  st: Agent,
  from: Int,
  into: Int,
  target: Handle,
) -> #(JsVal, Agent) {
  // run the observable gets in the target's realm
  let #(copied, st) =
    protected_in_realm(st, from, copy_name_and_length(_, target))
  case copied {
    ThrowCompletion(_thrown) ->
      rt_val.t_throw_type_error(
        st,
        "wrapped function could not copy target name and length",
      )
    NormalCompletion(#(name, length)) -> {
      let fn_proto = rt_call.realm_by_id(st, into).function.prototype
      let tag =
        ShadowRealmN(WrappedFunctionCall(
          target:,
          caller_realm: into,
          target_realm: from,
        ))
      let #(h, st) =
        rt_call.t_native_new_computed_length(
          st,
          Some(fn_proto),
          tag,
          name,
          length,
        )
      #(mk_object(h), st)
    }
  }
}

// §2.4 copynameandlength steps 2-7
fn copy_name_and_length(
  st: Agent,
  target: Handle,
) -> #(#(String, JsVal), Agent) {
  let target_v = mk_object(target)
  let #(len_desc, st) =
    rt_obj.t_get_own_property(st, target, StringKey(Named("length")))
  let #(len_val, st) = case len_desc {
    Some(_) -> rt_obj.t_get_prop(st, target_v, StringKey(Named("length")))
    None -> #(mk_undefined(), st)
  }
  let length = case classify(len_val) {
    KNum(JPosInf) -> mk_number(JPosInf)
    KNum(n) ->
      mk_number(JInt(int.max(rt_val.jsnum_to_integer_or_infinity(n), 0)))
    _ -> mk_number(JInt(0))
  }
  let #(name_val, st) =
    rt_obj.t_get_prop(st, target_v, StringKey(Named("name")))
  let name = case classify(name_val) {
    KStr(s) -> s
    _ -> ""
  }
  #(#(name, length), st)
}

// §3.4.1 shadowrealm.prototype.evaluate
fn evaluate(
  st: Agent,
  own_realm: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  // own realm is callerrealm and brands errors
  use st <- rt_realm.with_realm(st, own_realm)
  let eval_realm = require_shadow_realm(st, this, "evaluate")
  case classify(first_arg_or_undefined(args)) {
    KStr(source) -> perform_shadow_realm_eval(st, source, own_realm, eval_realm)
    _ ->
      rt_val.t_throw_type_error(
        st,
        "ShadowRealm.prototype.evaluate expects a string",
      )
  }
}

// §3.1.3 performshadowrealmeval with callerrealm current
fn perform_shadow_realm_eval(
  st: Agent,
  source: String,
  caller_realm: Int,
  eval_realm: Int,
) -> #(JsVal, Agent) {
  let early = {
    use #(body, sb) <- result.try(
      parser.parse_script(source)
      |> result.map_error(parser.parse_error_to_string),
    )
    compiler.compile_eval(body, sb) |> result.map_error(compiler.error_message)
  }
  let st = case early {
    Ok(_template) -> st
    Error(message) -> rt_val.t_throw_syntax_error(st, message)
  }
  let #(outcome, st) =
    protected_in_realm(st, eval_realm, fn(st) {
      st.store.ops.eval_hook(st, source, IndirectEval)
    })
  case outcome {
    NormalCompletion(v) -> get_wrapped_value(st, eval_realm, caller_realm, v)
    // original error must not cross the boundary
    ThrowCompletion(thrown) ->
      rt_val.t_throw_type_error(
        st,
        "ShadowRealm.prototype.evaluate threw: "
          <> inspect.format_error(st, thrown),
      )
  }
}

// §2.1 wrapped function [[call]]
fn wrapped_function_call(
  st: Agent,
  target: Handle,
  caller_realm: Int,
  target_realm: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st <- rt_realm.with_realm(st, caller_realm)
  let #(wrapped_args, st) = wrap_all(st, caller_realm, target_realm, args)
  let #(wrapped_this, st) =
    get_wrapped_value(st, caller_realm, target_realm, this)
  let #(outcome, st) =
    protected_in_realm(st, target_realm, fn(st) {
      rt_call.t_call_checked(st, mk_object(target), wrapped_this, wrapped_args)
    })
  case outcome {
    NormalCompletion(v) -> get_wrapped_value(st, target_realm, caller_realm, v)
    ThrowCompletion(thrown) ->
      rt_val.t_throw_type_error(
        st,
        "wrapped function threw: " <> inspect.format_error(st, thrown),
      )
  }
}

// §3.4.2 importvalue: validates, then always rejects (no module loader)
fn import_value(
  st: Agent,
  own_realm: Int,
  this: JsVal,
  args: List(JsVal),
) -> #(JsVal, Agent) {
  use st <- rt_realm.with_realm(st, own_realm)
  let _eval_realm = require_shadow_realm(st, this, "importValue")
  let #(specifier, export_name) = helpers.two_args_or_undefined(args)
  let #(_specifier, st) = rt_val.t_to_string(st, specifier)
  case classify(export_name) {
    KStr(_) -> {
      let #(err, st) =
        st.store.ops.new_error(
          st,
          TypeErr,
          "ShadowRealm.prototype.importValue: module loading is not "
            <> "available in this host",
        )
      let #(promise, st) = rt_async.t_new_promise(st)
      let st = rt_async.t_promise_reject(st, promise, err)
      #(mk_object(promise), st)
    }
    _ ->
      rt_val.t_throw_type_error(
        st,
        "ShadowRealm.prototype.importValue: exportName must be a string",
      )
  }
}
