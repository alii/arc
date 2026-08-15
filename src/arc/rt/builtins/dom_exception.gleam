//// WebIDL §2.8.1 DOMException: Error-like with a `name` drawn from a fixed
//// table that maps to a legacy integer `code`. The prototype chain goes
//// through %Error.prototype% so `instanceof Error` holds. Port of
//// `arc/vm/builtins/dom_exception.gleam` onto the threaded `Agent`.

import arc/rt/builtins/common
import arc/rt/builtins/error as b_error
import arc/rt/builtins/helpers
import arc/rt/call as rt_call
import arc/rt/obj as rt_obj
import arc/rt/types.{
  type Agent, type BuiltinPair, type DomExceptionNative, type Handle, type JsVal,
  DomExceptionConstructor, DomExceptionGetCode, DomExceptionN, JInt, KHandle,
  KUndef, Named, StringKey, classify, mk_number, mk_object, mk_string,
}
import arc/rt/val as rt_val

/// Set up %DOMException% and %DOMException.prototype%: the prototype
/// inherits from `error_proto`, carries the `code` getter and
/// `@@toStringTag = "DOMException"`; the constructor's [[Prototype]] is
/// `fn_proto` and its `length` is 2.
pub fn init(
  st: Agent,
  fn_proto: Handle,
  error_proto: Handle,
) -> #(BuiltinPair, Agent) {
  let #(getters, st) =
    common.alloc_getters(st, fn_proto, [
      #("code", DomExceptionN(DomExceptionGetCode)),
    ])
  let #(pair, st) =
    common.init_type(
      st,
      error_proto,
      fn_proto,
      getters,
      fn(proto) { DomExceptionN(DomExceptionConstructor(proto:)) },
      "DOMException",
      2,
      [],
    )
  let st = common.add_to_string_tag(st, pair.prototype, "DOMException")
  #(pair, st)
}

/// Per-module dispatch. `new_target` is `undefined` for a plain [[Call]];
/// `dispatch_native_construct` re-enters with it set.
pub fn dispatch(
  st: Agent,
  native: DomExceptionNative,
  this: JsVal,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  case native {
    DomExceptionConstructor(proto:) -> construct(st, proto, args, new_target)
    DomExceptionGetCode -> get_code(st, this)
  }
}

/// new DOMException(message = "", name = "Error")
///
/// WebIDL interface objects throw a TypeError when [[Call]]ed without `new`
/// and honour `new.target.prototype` for subclasses. Always installs own
/// `name` and `message` (writable+configurable, not enumerable) so the
/// prototype `code` getter and Error.prototype.toString resolve via
/// ordinary Get.
fn construct(
  st: Agent,
  fallback_proto: Handle,
  args: List(JsVal),
  new_target: JsVal,
) -> #(JsVal, Agent) {
  case classify(new_target) {
    KUndef ->
      rt_val.t_throw_type_error(st, "Constructor DOMException requires 'new'")
    _ -> {
      // The realm record has no %DOMException% slot, so the intrinsic
      // default is the constructor's own.
      let #(proto, st) =
        rt_call.get_prototype_from_constructor(st, new_target, fn(_realm) {
          fallback_proto
        })
      let #(msg_arg, name_arg) = helpers.two_args_or_undefined(args)
      let #(message, st) = arg_string(st, msg_arg, "")
      let #(name, st) = arg_string(st, name_arg, "Error")
      let #(msg_prop, st) = common.builtin_property(st, mk_string(message))
      let #(name_prop, st) = common.builtin_property(st, mk_string(name))
      // Allocated through `alloc_error_slot`, so the kind is `ErrorObj`: the
      // [[ErrorData]] internal slot (§20.5.4). Instances hold a stack trace
      // and satisfy Error.isError, exactly like the native Error types.
      let #(h, st) =
        common.alloc_error_slot(st, proto, [
          #("message", msg_prop),
          #("name", name_prop),
        ])
      let st = b_error.attach_stack(st, h, name, message)
      #(mk_object(h), st)
    }
  }
}

fn arg_string(st: Agent, arg: JsVal, default: String) -> #(String, Agent) {
  case classify(arg) {
    KUndef -> #(default, st)
    _ -> rt_val.t_to_string(st, arg)
  }
}

/// get DOMException.prototype.code: reads `this.name` and maps it through
/// the WebIDL legacy code table; unknown names yield 0.
fn get_code(st: Agent, this: JsVal) -> #(JsVal, Agent) {
  case classify(this) {
    KHandle(_) -> {
      let #(name_val, st) =
        rt_obj.t_get_prop(st, this, StringKey(Named("name")))
      let #(name, st) = rt_val.t_to_string(st, name_val)
      #(mk_number(JInt(legacy_code(name))), st)
    }
    _ -> #(mk_number(JInt(0)), st)
  }
}

/// WebIDL DOMException names table: `name` to legacy numeric `code`.
fn legacy_code(name: String) -> Int {
  case name {
    "IndexSizeError" -> 1
    "HierarchyRequestError" -> 3
    "WrongDocumentError" -> 4
    "InvalidCharacterError" -> 5
    "NoModificationAllowedError" -> 7
    "NotFoundError" -> 8
    "NotSupportedError" -> 9
    "InUseAttributeError" -> 10
    "InvalidStateError" -> 11
    "SyntaxError" -> 12
    "InvalidModificationError" -> 13
    "NamespaceError" -> 14
    "InvalidAccessError" -> 15
    "TypeMismatchError" -> 17
    "SecurityError" -> 18
    "NetworkError" -> 19
    "AbortError" -> 20
    "URLMismatchError" -> 21
    "QuotaExceededError" -> 22
    "TimeoutError" -> 23
    "InvalidNodeTypeError" -> 24
    "DataCloneError" -> 25
    _ -> 0
  }
}
