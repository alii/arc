import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope.{type Binding}
import arc/parser/ast
import arc_aot/emit/anf
import arc_aot/emit/func
import arc_aot/emit/state.{
  type EmitResult, type Emitter, type Next, type NextWith, ClassContext, Emitter,
}
import carder/ir
import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

fn let_(e: Emitter, rhs: ir.Expr, k: NextWith(ir.Value)) -> EmitResult {
  state.let_(e, rhs, k)
}

fn host_(
  e: Emitter,
  op: String,
  args: List(ir.Value),
  k: NextWith(ir.Value),
) -> EmitResult {
  let_(e, ir.CallHost("js", op, args), k)
}

fn host_unit_(
  e: Emitter,
  op: String,
  args: List(ir.Value),
  k: Next,
) -> EmitResult {
  use _, e <- host_(e, op, args)
  k(e)
}

fn each_(
  e: Emitter,
  items: List(a),
  then k: Next,
  with step: fn(Emitter, a, Next) -> EmitResult,
) -> EmitResult {
  case items {
    [] -> k(e)
    [x, ..rest] -> step(e, x, fn(e) { each_(e, rest, k, step) })
  }
}

fn with_done(
  e: Emitter,
  f: fn(NextWith(ir.Expr), Emitter) -> EmitResult,
) -> EmitResult {
  f(fn(tree, ef) { Ok(#(tree, ef)) }, e)
}

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
      host_unit_(e, "box_set", [ir.Var(state.get_slot_var(e, b.slot)), v], k)
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
    True -> host_(e, "box_get", [v], k)
    False -> k(v, e)
  }
}

// §15.7.14 step 27, each computed key evaluated once
fn emit_computed_keys(
  e: Emitter,
  body: List(ast.ClassElement),
  k: Next,
) -> EmitResult {
  use e, pair, next <- each_(e, ast_util.computed_element_keys(body), then: k)
  let #(idx, key_expr) = pair
  use #(tree, e) <- result.try(e.dispatch.emit_expr(e, key_expr))
  use key_value, e <- let_(e, tree)
  use pk, e <- host_(e, "to_property_key", [key_value])
  store_class_const(e, ast_util.computed_field_const(idx), pk, next)
}

// atoms must match gleam's erlang spelling of InstallMethod etc
fn method_install_atom(
  kind: ast.MethodKind,
  is_static is_static: Bool,
) -> ir.Value {
  ir.ConstAtom(case kind, is_static {
    ast.GetterMethod, False -> "install_getter"
    ast.SetterMethod, False -> "install_setter"
    ast.PlainMethod, False | ast.ConstructorMethod, False -> "install_method"
    ast.GetterMethod, True -> "install_static_getter"
    ast.SetterMethod, True -> "install_static_setter"
    ast.PlainMethod, True | ast.ConstructorMethod, True -> "install_static"
  })
}

fn method_fn_name(
  key: ast.PropertyKey,
  kind: ast.MethodKind,
) -> Option(String) {
  let base = case key {
    ast.KeyIdentifier(name:, ..) | ast.KeyString(value: name, ..) -> Some(name)
    ast.KeyPrivate(name:, ..) -> Some(name)
    ast.KeyNumber(..) | ast.KeyBigInt(..) | ast.KeyComputed(..) -> None
  }
  case kind, base {
    ast.GetterMethod, Some(n) -> Some("get " <> n)
    ast.SetterMethod, Some(n) -> Some("set " <> n)
    _, _ -> base
  }
}

fn resolve_method_key(
  e: Emitter,
  key: ast.PropertyKey,
  body_index: Int,
  k: NextWith(ir.Value),
) -> EmitResult {
  case key {
    ast.KeyComputed(..) ->
      read_class_const(e, ast_util.computed_field_const(body_index), k)
    ast.KeyPrivate(name:, ..) -> read_class_const(e, name, k)
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..) -> {
      let #(tree, e) = anf.run(anf.object_key_lit(key), e)
      let_(e, tree, k)
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
  use e, method, next <- each_(e, methods, then: k)
  let ast_util.ClassMethodElement(body_index:, key:, kind:, fun:) = method
  let ast.FunctionLiteral(params:, body:, is_generator: is_gen, is_async:, ..) =
    fun
  let #(child_id, e) = state.pop_child_fn(e)
  use #(ctree, e) <- result.try(e.dispatch.emit_function(
    e,
    state.Method(is_gen:, is_async:),
    method_fn_name(key, kind),
    params,
    state.StmtBody(body),
    child_id,
  ))
  use method_fn, e <- let_(e, ctree)
  case key {
    ast.KeyPrivate(name:, ..) if !is_static -> {
      use e <- host_unit_(e, "make_method", [method_fn, target])
      store_class_const(
        e,
        ast_util.private_fn_const(kind, name),
        method_fn,
        next,
      )
    }
    ast.KeyPrivate(name:, ..) -> {
      use e <- host_unit_(e, "make_method", [method_fn, target])
      use pk, e <- read_class_const(e, name)
      host_unit_(
        e,
        "define_private",
        [target, pk, method_fn, method_install_atom(kind, is_static: False)],
        next,
      )
    }
    _ -> {
      use key_value, e <- resolve_method_key(e, key, body_index)
      host_unit_(
        e,
        "define_method",
        [
          target,
          key_value,
          method_fn,
          method_install_atom(kind, is_static),
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
      ast.KeyPrivate(..) -> True
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
  k k: NextWith(#(ir.Value, ir.Value)),
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
    state.ClassCtor(derived: is_derived, has_field_init:, default:),
    display_name,
    ctor_params,
    state.StmtBody(ctor_body),
    ctor_child_id,
  ))
  use ctor, e <- let_(e, ctor_tree)
  use proto, e <- host_(e, "class_setup", [ctor, parent_class])
  let assert [ctx, ..] = e.class_stack
    as "emit/class: emit_ctor_and_create with empty class_stack"
  use e <- host_unit_(e, "box_set", [ctx.proto_home_box, proto])
  use e <- host_unit_(e, "box_set", [ctx.static_home_box, ctor])
  use e <- host_unit_(e, "box_set", [ctx.ctor_self_box, ctor])
  k(#(ctor, proto), e)
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
  use done, e <- with_done(e)
  let saved_strict = e.strict
  let saved_private_env = e.private_env
  let private_names = ast_util.class_private_names(body)
  let e =
    Emitter(
      ..e,
      strict: True,
      private_env: list.append(private_names, e.private_env),
    )
  let #(save, e) = state.enter_scope(e, in_block: e.in_block)
  use e <- func.binding_prologue(e, e.cur_scope)
  use e <- each_(e, private_names, with: fn(e, pname, next) {
    use key, e <- host_(e, "new_private_name", [
      ir.ConstBinary(bit_array.from_string(pname)),
    ])
    store_class_const(e, pname, key, next)
  })
  let is_derived = option.is_some(super_class)
  use proto_home_box, e <- host_(e, "box_new", [e.consts.undef])
  use static_home_box, e <- host_(e, "box_new", [e.consts.undef])
  use ctor_self_box, e <- host_(e, "box_new", [e.consts.undef])
  let with_inner_box = fn(e: Emitter, then: NextWith(Option(ir.Value))) {
    case binding_name {
      None -> then(None, e)
      Some(_) -> {
        use box, e <- host_(e, "box_new", [e.consts.undef])
        then(Some(box), e)
      }
    }
  }
  use inner_name_box, e <- with_inner_box(e)
  let brand_vars =
    list.fold(private_names, dict.new(), fn(acc, pname) {
      dict.insert(
        acc,
        pname,
        ir.Var(state.get_slot_var(e, class_scope_binding(e, pname).slot)),
      )
    })
  let ctx =
    ClassContext(
      brand_vars:,
      proto_home_box:,
      static_home_box:,
      ctor_self_box:,
      inner_name_box:,
      is_derived:,
    )
  let e = Emitter(..e, class_stack: [ctx, ..e.class_stack])
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
        let_(e, tree, k)
      }
      None -> k(e.consts.tdz, e)
    }
  }
  use parent_class, e <- with_super(e)
  use #(ctor, proto), e <- emit_ctor_and_create(
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
      Some(v) -> host_unit_(e, "set_fields_init", [ctor, v], then)
      None -> then(e)
    }
  }
  use e <- with_fields_init(e)
  use e <- emit_static_init(e, parts, ctor)
  let assert [_, ..outer_class_stack] = e.class_stack
  let e =
    Emitter(
      ..state.leave_scope(e, save),
      class_stack: outer_class_stack,
      strict: saved_strict,
      private_env: saved_private_env,
    )
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
    ast.KeyPrivate(name:, ..) ->
      Ok(PrivateMethodInit(
        name:,
        closure_const: ast_util.private_fn_const(m.kind, name),
        kind: m.kind,
      ))
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..)
    | ast.KeyComputed(..) -> Error(Nil)
  }
}

fn field_init_of(field: ast_util.ClassFieldElement) -> FieldInit {
  let ast_util.ClassFieldElement(body_index:, key:, value:) = field
  let init =
    option.unwrap(value, ast.UndefinedExpression(ast.property_key_span(key)))
  case key {
    ast.KeyPrivate(name:, ..) -> PrivateFieldInit(name:, init:)
    ast.KeyIdentifier(name:, ..) | ast.KeyString(value: name, ..) ->
      NamedFieldInit(name:, init:)
    ast.KeyNumber(value: n, ..) -> NumericFieldInit(value: n, init:)
    ast.KeyBigInt(value: i, ..) -> BigIntFieldInit(value: i, init:)
    ast.KeyComputed(..) ->
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
fn init_fn_flags(rc: state.IrConsts) -> List(ir.Value) {
  [
    ir.ConstAtom("fn_flags"),
    rc.false_,
    rc.false_,
    rc.false_,
    rc.false_,
    rc.false_,
    rc.false_,
    rc.false_,
    rc.true_,
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
        True -> host_(e, "box_get", [v], k)
        False -> k(v, e)
      }
    }
    scope.Plain(scope.Global(_))
    | scope.Plain(scope.EvalEnv(_))
    | scope.WithChain(..) ->
      host_(
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
      use _, e <- let_(e, tree)
      next(e)
    }
    PrivateMethodInit(name:, closure_const:, kind:) -> {
      use pk, e <- read_captured_const(e, name)
      use closure, e <- read_captured_const(e, closure_const)
      host_unit_(
        e,
        "define_private",
        [this, pk, closure, method_install_atom(kind, is_static: False)],
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
      use v, e <- let_(e, tree)
      host_unit_(e, "private_define", [this, pk, v], next)
    }
    NamedFieldInit(name:, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.KeyIdentifier(name:, span: ast.Span(0, 0))),
          e,
        )
      use key_value, e <- let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr_named(
        e,
        init,
        Some(name),
      ))
      use v, e <- let_(e, tree)
      host_unit_(e, "create_data_prop", [this, key_value, v], next)
    }
    NumericFieldInit(value: n, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.KeyNumber(value: n, span: ast.Span(0, 0))),
          e,
        )
      use key_value, e <- let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use v, e <- let_(e, tree)
      host_unit_(e, "create_data_prop", [this, key_value, v], next)
    }
    BigIntFieldInit(value: i, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.KeyBigInt(value: i, span: ast.Span(0, 0))),
          e,
        )
      use key_value, e <- let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use v, e <- let_(e, tree)
      host_unit_(e, "create_data_prop", [this, key_value, v], next)
    }
    ComputedFieldInit(key_const:, init:) -> {
      use key_value, e <- read_captured_const(e, key_const)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use v, e <- let_(e, tree)
      host_unit_(e, "create_data_prop", [this, key_value, v], next)
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
    with_done(e_child, fn(done, ec) {
      use ec <- func.unpack_frame(ec, is_arrow: False, info: child_info)
      use ec <- func.binding_prologue(ec, ec.fn_scope)
      let with_this = fn(ec, k) {
        case lexical.slot_of(child_info.lexical, lexical.RefThis) {
          Some(slot) -> {
            let v = ir.Var(state.get_slot_var(ec, slot))
            case state.lexical_is_boxed(ec, child_info, lexical.RefThis) {
              True -> host_(ec, "box_get", [v], k)
              False -> k(v, ec)
            }
          }
          None -> k(ec.consts.undef, ec)
        }
      }
      use this, ec <- with_this(ec)
      each_(
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
  use fun, e <- let_(e, ir.MakeClosure(fn_name, capture_vals, 2))
  use flags_t, e <- let_(e, ir.TermOp(ir.MakeTuple, init_fn_flags(e.consts)))
  use init_fn, e <- host_(e, "new_function", [
    fun,
    flags_t,
    e.consts.empty_bin,
    ir.ConstI32(0),
    ir.ConstAtom("none"),
  ])
  use e <- host_unit_(e, "make_method", [init_fn, home])
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
      use empty, e <- host_(e, "empty_list", [])
      host_unit_(e, "call", [static_init, ctor, empty], k)
    }
  }
}
