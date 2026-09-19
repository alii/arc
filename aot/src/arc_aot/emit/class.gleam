import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope.{type Binding}
import arc/parser/ast
import arc_aot/emit/anf
import arc_aot/emit/cps
import arc_aot/emit/func
import arc_aot/emit/state.{
  type EmitResult, type Emitter, type Next, type NextWith, Emitter,
}
import carder/ir
import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

fn class_scope_binding(e: Emitter, name: String) -> Binding {
  let assert Ok(b) =
    dict.get(scope.get(e.scope_tree, e.cur_scope).bindings, name)
    as "emit/class: name missing from ClassBody-scope bindings"
  b
}

fn store_class_const(
  e: Emitter,
  name: String,
  v: ir.Value,
  k: Next,
) -> EmitResult {
  let b = class_scope_binding(e, name)
  let e =
    state.Emitter(
      ..e,
      initialized_slots: set.insert(e.initialized_slots, b.slot),
    )
  case b.boxed {
    True ->
      cps.host_unit(e, "box_set", [ir.Var(state.get_slot_var(e, b.slot)), v], k)
    False -> {
      let vn = state.slot_base_name(e, b.slot)
      use body <- state.map_tree(k(state.set_slot_var(e, b.slot, vn)))
      ir.Let([vn], ir.Values([v]), body)
    }
  }
}

fn read_class_const(
  e: Emitter,
  name: String,
  k: NextWith(ir.Value),
) -> EmitResult {
  let b = class_scope_binding(e, name)
  let v = ir.Var(state.get_slot_var(e, b.slot))
  case b.boxed {
    True -> cps.host(e, "box_get", [v], k)
    False -> k(v, e)
  }
}

// §15.7.14 step 27, each computed key evaluated once
fn emit_computed_keys(
  e: Emitter,
  body: List(ast.ClassElement),
  k: Next,
) -> EmitResult {
  use e, pair, next <- cps.each(
    e,
    ast_util.computed_element_keys(body),
    then: k,
  )
  let #(idx, key_expr) = pair
  use #(tree, e) <- result.try(e.dispatch.emit_expr(e, key_expr))
  use key_value, e <- cps.let_(e, tree)
  use pk, e <- cps.host(e, "to_property_key", [key_value])
  store_class_const(e, ast_util.computed_field_const(idx), pk, next)
}

// atoms must match gleam's erlang spelling of InstallMethod etc
fn method_install_atom(kind: ast.MethodKind) -> ir.Value {
  ir.ConstAtom(case kind {
    ast.GetterMethod -> "install_getter"
    ast.SetterMethod -> "install_setter"
    ast.PlainMethod | ast.ConstructorMethod -> "install_method"
  })
}

fn method_fn_name(
  key: ast.PropertyName,
  kind: ast.MethodKind,
) -> Option(String) {
  let base = case key {
    ast.IdentifierName(name:, ..) | ast.StringName(value: name, ..) ->
      Some(name)
    ast.PrivateName(name:, ..) -> Some(name)
    ast.NumberName(..) | ast.BigIntName(..) | ast.ComputedName(..) -> None
  }
  case kind, base {
    ast.GetterMethod, Some(n) -> Some("get " <> n)
    ast.SetterMethod, Some(n) -> Some("set " <> n)
    _, _ -> base
  }
}

fn method_key_value(
  e: Emitter,
  key: ast.PropertyName,
  body_index: Int,
  k: NextWith(ir.Value),
) -> EmitResult {
  case key {
    ast.ComputedName(..) ->
      read_class_const(e, ast_util.computed_field_const(body_index), k)
    ast.PrivateName(name:, ..) -> read_class_const(e, name, k)
    ast.IdentifierName(..)
    | ast.StringName(..)
    | ast.NumberName(..)
    | ast.BigIntName(..) -> {
      let #(tree, e) = anf.run(anf.object_key_lit(key), e)
      cps.let_(e, tree, k)
    }
  }
}

fn emit_methods(
  e: Emitter,
  methods: List(ast_util.ClassMethodElement),
  target: ir.Value,
  is_static is_static: Bool,
  k k: Next,
) -> EmitResult {
  use e, method, next <- cps.each(e, methods, then: k)
  let ast_util.ClassMethodElement(body_index:, key:, kind:, fun:) = method
  let ast.FunctionLiteral(params:, body:, is_generator:, is_async:, ..) = fun
  let #(child_id, e) = state.pop_child_fn(e)
  use #(ctree, e) <- result.try(e.dispatch.emit_function(
    e,
    state.Method(is_generator:, is_async:),
    method_fn_name(key, kind),
    params,
    state.StmtBody(body),
    child_id,
  ))
  use method_fn, e <- cps.let_(e, ctree)
  case key {
    ast.PrivateName(name:, ..) if !is_static -> {
      use e <- cps.host_unit(e, "make_method", [method_fn, target])
      store_class_const(
        e,
        ast_util.private_fn_const(kind, name),
        method_fn,
        next,
      )
    }
    ast.PrivateName(name:, ..) -> {
      use e <- cps.host_unit(e, "make_method", [method_fn, target])
      use pk, e <- read_class_const(e, name)
      cps.host_unit(
        e,
        "private_method_add",
        [target, pk, method_fn, method_install_atom(kind)],
        next,
      )
    }
    _ -> {
      use key_value, e <- method_key_value(e, key, body_index)
      cps.host_unit(
        e,
        "define_method",
        [
          target,
          key_value,
          method_fn,
          method_install_atom(kind),
          e.consts.false_,
        ],
        next,
      )
    }
  }
}

pub fn has_instance_field_init(parts: ast_util.ClassBodyParts) -> Bool {
  parts.instance_fields != []
  || list.any(parts.instance_methods, fn(m) {
    case m.key {
      ast.PrivateName(..) -> True
      _ -> False
    }
  })
}

fn emit_ctor_and_create(
  e: Emitter,
  parts: ast_util.ClassBodyParts,
  display_name: Option(String),
  parent_class: ir.Value,
  is_derived is_derived: Bool,
  has_field_init has_field_init: Bool,
  ctor_child_id ctor_child_id: scope.ScopeId,
  k k: NextWith(CtorProto),
) -> EmitResult {
  let #(ctor_params, ctor_body, default) = case parts.constructor {
    Some(ast_util.ClassMethodElement(
      fun: ast.FunctionLiteral(params:, body:, ..),
      ..,
    )) -> #(params, body, False)
    None -> #([], default_ctor_body(is_derived), True)
  }
  use #(ctor_tree, e) <- result.try(e.dispatch.emit_function(
    e,
    state.ClassCtor(is_derived:, has_field_init:, default:),
    display_name,
    ctor_params,
    state.StmtBody(ctor_body),
    ctor_child_id,
  ))
  use ctor, e <- cps.let_(e, ctor_tree)
  use proto, e <- cps.host(e, "setup", [ctor, parent_class])
  k(CtorProto(ctor:, proto:), e)
}

type CtorProto {
  CtorProto(ctor: ir.Value, proto: ir.Value)
}

// §15.7.14 step 14.a default constructor
fn default_ctor_body(is_derived: Bool) -> List(ast.StmtWithLine) {
  case is_derived {
    False -> []
    True -> {
      let span = ast.Span(0, 0)
      [
        ast.StmtWithLine(
          0,
          ast.ExpressionStatement(
            expression: ast.CallExpression(
              span:,
              callee: ast.SuperExpression(span:),
              arguments: [],
            ),
            directive: None,
          ),
        ),
      ]
    }
  }
}

// §15.7.14 class definition evaluation, step order matters
pub fn emit(
  e: Emitter,
  binding_name: Option(String),
  display_name: Option(String),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> EmitResult {
  use done, e <- cps.with_done(e)
  let saved_strict = e.strict
  let private_names = ast_util.class_private_names(body)
  let e = Emitter(..e, strict: True)
  let #(save, e) = state.enter_scope(e, in_block: e.in_block)
  use e <- func.binding_prologue(e, e.cur_scope)
  use e <- cps.each(e, private_names, with: fn(e, pname, next) {
    use key, e <- cps.host(e, "new_private_name", [
      ir.ConstBinary(bit_array.from_string(pname)),
    ])
    store_class_const(e, pname, key, next)
  })
  let is_derived = option.is_some(super_class)
  // analyzer registers init then ctor shells first, so pop them first
  let parts = ast_util.classify_class_body(body)
  let has_field_init = has_instance_field_init(parts)
  let #(init_child_id, e) = case has_field_init {
    True -> {
      let #(id, e) = state.pop_child_fn(e)
      #(Some(id), e)
    }
    False -> #(None, e)
  }
  let #(ctor_child_id, e) = state.pop_child_fn(e)
  let with_super = fn(e: Emitter, k: NextWith(ir.Value)) {
    case super_class {
      Some(h) -> {
        use #(tree, e) <- result.try(e.dispatch.emit_expr(e, h))
        cps.let_(e, tree, k)
      }
      None -> k(e.consts.tdz, e)
    }
  }
  use parent_class, e <- with_super(e)
  use CtorProto(ctor:, proto:), e <- emit_ctor_and_create(
    e,
    parts,
    display_name,
    parent_class,
    is_derived,
    has_field_init,
    ctor_child_id,
  )
  use e <- emit_computed_keys(e, body)
  use e <- emit_methods(e, parts.instance_methods, proto, is_static: False)
  use e <- emit_methods(e, parts.static_methods, ctor, is_static: True)
  use init_fn, e <- emit_field_init_fn(e, parts, proto, init_child_id)
  // inner name bound after elements but before statics
  let with_inner_name = fn(e, then: Next) {
    case binding_name {
      Some(n) -> store_class_const(e, n, ctor, then)
      None -> then(e)
    }
  }
  use e <- with_inner_name(e)
  let with_fields_init = fn(e, then) {
    case init_fn {
      Some(v) -> cps.host_unit(e, "set_fields_init", [ctor, v], then)
      None -> then(e)
    }
  }
  use e <- with_fields_init(e)
  use e <- emit_static_init(e, parts, ctor)
  let e = Emitter(..state.leave_scope(e, save), strict: saved_strict)
  done(ir.Values([ctor]), e)
}

type FieldInit {
  PrivateMethodInit(name: String, closure_const: String, kind: ast.MethodKind)
  PrivateFieldInit(name: String, init: ast.Expression)
  NamedFieldInit(name: String, init: ast.Expression)
  NumericFieldInit(value: ast.LiteralNumber, init: ast.Expression)
  ComputedFieldInit(key_const: String, init: ast.Expression)
  BigIntFieldInit(value: Int, init: ast.Expression)
  StaticBlockInit(body: List(ast.StmtWithLine))
}

// §7.3.31 private methods install before fields
fn private_method_inits(
  methods: List(ast_util.ClassMethodElement),
) -> List(FieldInit) {
  use m <- list.filter_map(methods)
  case m.key {
    ast.PrivateName(name:, ..) ->
      Ok(PrivateMethodInit(
        name:,
        closure_const: ast_util.private_fn_const(m.kind, name),
        kind: m.kind,
      ))
    ast.IdentifierName(..)
    | ast.StringName(..)
    | ast.NumberName(..)
    | ast.BigIntName(..)
    | ast.ComputedName(..) -> Error(Nil)
  }
}

fn field_init_of(field: ast_util.ClassFieldElement) -> FieldInit {
  let ast_util.ClassFieldElement(body_index:, key:, value:) = field
  let init =
    option.unwrap(value, ast.UndefinedExpression(ast.property_name_span(key)))
  case key {
    ast.PrivateName(name:, ..) -> PrivateFieldInit(name:, init:)
    ast.IdentifierName(name:, ..) | ast.StringName(value: name, ..) ->
      NamedFieldInit(name:, init:)
    ast.NumberName(value: n, ..) -> NumericFieldInit(value: n, init:)
    ast.BigIntName(value: i, ..) -> BigIntFieldInit(value: i, init:)
    ast.ComputedName(..) ->
      ComputedFieldInit(
        key_const: ast_util.computed_field_const(body_index),
        init:,
      )
  }
}

fn field_inits(fields: List(ast_util.ClassFieldElement)) -> List(FieldInit) {
  list.map(fields, field_init_of)
}

fn static_inits(elements: List(ast_util.StaticElement)) -> List(FieldInit) {
  use elem <- list.map(elements)
  case elem {
    ast_util.StaticFieldElement(field) -> field_init_of(field)
    ast_util.StaticBlockElement(body) -> StaticBlockInit(body)
  }
}

fn static_block_iife(body: List(ast.StmtWithLine)) -> ast.Expression {
  ast.CallExpression(
    span: ast.Span(0, 0),
    callee: ast.ArrowFunctionExpression(
      span: ast.Span(0, 0),
      params: [],
      body: ast.ArrowBodyBlock(body),
      is_async: False,
    ),
    arguments: [],
  )
}

// must match arc/rt/types FnFlags field order exactly
fn init_fn_flags(consts: state.IrConsts) -> List(ir.Value) {
  [
    ir.ConstAtom("fn_flags"),
    consts.false_,
    consts.false_,
    consts.false_,
    consts.false_,
    consts.false_,
    consts.false_,
    consts.true_,
  ]
}

fn read_captured_const(
  e: Emitter,
  name: String,
  k: NextWith(ir.Value),
) -> EmitResult {
  case state.resolve(e, name) {
    scope.Plain(scope.Local(slot:, boxed:, ..)) -> {
      let v = ir.Var(state.get_slot_var(e, slot))
      case boxed {
        True -> cps.host(e, "box_get", [v], k)
        False -> k(v, e)
      }
    }
    scope.Plain(scope.Global(_))
    | scope.Plain(scope.EvalEnv(_))
    | scope.WithChain(..) ->
      cps.host(
        e,
        "throw_reference_error",
        [
          ir.ConstBinary(bit_array.from_string(
            "emit/class: init-fn const resolve miss: " <> name,
          )),
        ],
        k,
      )
  }
}

fn emit_one_init(
  e: Emitter,
  this: ir.Value,
  fi: FieldInit,
  next: Next,
) -> EmitResult {
  case fi {
    StaticBlockInit(body:) -> {
      use #(tree, e) <- result.try(e.dispatch.emit_expr(
        e,
        static_block_iife(body),
      ))
      use _, e <- cps.let_(e, tree)
      next(e)
    }
    PrivateMethodInit(name:, closure_const:, kind:) -> {
      use pk, e <- read_captured_const(e, name)
      use closure, e <- read_captured_const(e, closure_const)
      cps.host_unit(
        e,
        "private_method_add",
        [this, pk, closure, method_install_atom(kind)],
        next,
      )
    }
    PrivateFieldInit(name:, init:) -> {
      use pk, e <- read_captured_const(e, name)
      use #(tree, e) <- result.try(e.dispatch.emit_expr_named(
        e,
        init,
        Some(name),
      ))
      use v, e <- cps.let_(e, tree)
      cps.host_unit(e, "private_field_add", [this, pk, v], next)
    }
    NamedFieldInit(name:, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.IdentifierName(name:, span: ast.Span(0, 0))),
          e,
        )
      use key_value, e <- cps.let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr_named(
        e,
        init,
        Some(name),
      ))
      use v, e <- cps.let_(e, tree)
      cps.host_unit(e, "create_data_prop", [this, key_value, v], next)
    }
    NumericFieldInit(value: n, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.NumberName(value: n, span: ast.Span(0, 0))),
          e,
        )
      use key_value, e <- cps.let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use v, e <- cps.let_(e, tree)
      cps.host_unit(e, "create_data_prop", [this, key_value, v], next)
    }
    BigIntFieldInit(value: i, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.BigIntName(value: i, span: ast.Span(0, 0))),
          e,
        )
      use key_value, e <- cps.let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use v, e <- cps.let_(e, tree)
      cps.host_unit(e, "create_data_prop", [this, key_value, v], next)
    }
    ComputedFieldInit(key_const:, init:) -> {
      use key_value, e <- read_captured_const(e, key_const)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use v, e <- cps.let_(e, tree)
      cps.host_unit(e, "create_data_prop", [this, key_value, v], next)
    }
  }
}

fn build_class_init_closure(
  e: Emitter,
  child_id: scope.ScopeId,
  inits: List(FieldInit),
  home: ir.Value,
  k: NextWith(ir.Value),
) -> EmitResult {
  let child_info = scope.function_info(e.scope_tree, child_id)
  let capture_vals = func.build_capture_values(e, child_info)
  let #(fn_name, e) = state.fresh_fn_name(e, None)
  let #(save, e_child) =
    state.enter_function(
      e,
      child_id,
      strict: True,
      is_async: False,
      is_arrow: False,
    )
  let e_child = func.seed_capture_slots(e_child, child_info)
  use #(body_expr, e_child) <- result.try(
    cps.with_done(e_child, fn(done, ec) {
      use ec <- func.unpack_frame(ec, is_arrow: False, info: child_info)
      use ec <- func.binding_prologue(ec, ec.fn_scope)
      let with_this = fn(ec, k) {
        case lexical.slot_of(child_info.lexical, lexical.ThisRef) {
          Some(slot) -> {
            let v = ir.Var(state.get_slot_var(ec, slot))
            case state.lexical_is_boxed(ec, child_info, lexical.ThisRef) {
              True -> cps.host(ec, "box_get", [v], k)
              False -> k(v, ec)
            }
          }
          None -> k(ec.consts.undef, ec)
        }
      }
      use this, ec <- with_this(ec)
      cps.each(
        ec,
        inits,
        then: fn(ef) { done(ir.Return([ef.consts.undef]), ef) },
        with: fn(ec, fi, next) { emit_one_init(ec, this, fi, next) },
      )
    }),
  )
  let ncap =
    list.length(child_info.captures) + dict.size(child_info.lexical_captures)
  let e_child =
    state.add_function(
      e_child,
      ir.Function(
        name: fn_name,
        params: func.build_ir_params(e_child, 0, ncap),
        result: [ir.TTerm],
        locals: [],
        body: body_expr,
      ),
    )
  let e = state.leave_function(e_child, save)
  use fun, e <- cps.let_(e, ir.MakeClosure(fn_name, capture_vals, 2))
  use flags_t, e <- cps.let_(
    e,
    ir.TermOp(ir.MakeTuple, init_fn_flags(e.consts)),
  )
  use init_fn, e <- cps.host(e, "new_closure", [
    fun,
    flags_t,
    e.consts.empty_bin,
    ir.ConstI32(0),
    ir.ConstAtom("none"),
  ])
  use e <- cps.host_unit(e, "make_method", [init_fn, home])
  k(init_fn, e)
}

fn emit_field_init_fn(
  e: Emitter,
  parts: ast_util.ClassBodyParts,
  proto: ir.Value,
  init_child_id: Option(scope.ScopeId),
  k: NextWith(Option(ir.Value)),
) -> EmitResult {
  let inits =
    list.append(
      private_method_inits(parts.instance_methods),
      field_inits(parts.instance_fields),
    )
  case inits {
    [] -> {
      use e <- store_class_const(e, ast_util.class_fields_init, e.consts.undef)
      k(None, e)
    }
    _ -> {
      let assert Some(child_id) = init_child_id
        as "emit/class: has_instance_field_init/parser needs_instance_init desync"
      use init_fn, e <- build_class_init_closure(e, child_id, inits, proto)
      use e <- store_class_const(e, ast_util.class_fields_init, init_fn)
      k(Some(init_fn), e)
    }
  }
}

fn emit_static_init(
  e: Emitter,
  parts: ast_util.ClassBodyParts,
  ctor: ir.Value,
  k: Next,
) -> EmitResult {
  let inits = static_inits(parts.static_elements)
  case inits {
    [] -> k(e)
    _ -> {
      let #(child_id, e) = state.pop_child_fn(e)
      use static_init, e <- build_class_init_closure(e, child_id, inits, ctor)
      use empty, e <- cps.host(e, "empty_list", [])
      cps.host_unit(e, "call", [static_init, ctor, empty], k)
    }
  }
}
