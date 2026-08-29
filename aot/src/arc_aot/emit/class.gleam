import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope.{type Binding}
import arc/parser/ast
import arc_aot/emit/anf
import arc_aot/emit/func
import arc_aot/emit/state.{type EmitError, type Emitter2, ClassCtx, Emitter2}
import carder/ir
import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set

type Rk(a) =
  fn(Emitter2, a) -> Result(#(ir.Expr, Emitter2), EmitError)

fn let_(
  e: Emitter2,
  rhs: ir.Expr,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  state.let_(e, rhs, k)
}

fn host_(
  e: Emitter2,
  op: String,
  args: List(ir.Value),
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let_(e, ir.CallHost("js", op, args), k)
}

fn host_unit_(
  e: Emitter2,
  op: String,
  args: List(ir.Value),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, _ <- host_(e, op, args)
  k(e)
}

fn each_(
  e: Emitter2,
  items: List(a),
  then k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  with step: fn(
    Emitter2,
    a,
    fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  ) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case items {
    [] -> k(e)
    [x, ..rest] -> step(e, x, fn(e) { each_(e, rest, k, step) })
  }
}

fn run_rk(
  e: Emitter2,
  f: fn(
    Emitter2,
    fn(Emitter2, ir.Expr) -> Result(#(ir.Expr, Emitter2), EmitError),
  ) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  f(e, fn(ef, tree) { Ok(#(tree, ef)) })
}

fn class_scope_binding(e: Emitter2, name: String) -> Binding {
  let assert Ok(b) =
    dict.get(scope.get_scope(e.tree, e.cur_scope).bindings, name)
    as "emit_2core/class: name missing from ClassBody-scope bindings"
  b
}

fn store_class_const(
  e: Emitter2,
  name: String,
  v: ir.Value,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let b = class_scope_binding(e, name)
  let e = state.Emitter2(..e, initialized: set.insert(e.initialized, b.slot))
  case b.is_boxed {
    True ->
      host_unit_(e, "cell_set", [ir.Var(state.get_slot_var(e, b.slot)), v], k)
    False -> {
      let vn = state.slot_var_name(e, b.slot)
      use body <- state.map_tree(k(state.set_slot_var(e, b.slot, vn)))
      ir.Let([vn], ir.Values([v]), body)
    }
  }
}

fn read_class_const(
  e: Emitter2,
  name: String,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let b = class_scope_binding(e, name)
  let v = ir.Var(state.get_slot_var(e, b.slot))
  case b.is_boxed {
    True -> host_(e, "cell_get", [v], k)
    False -> k(e, v)
  }
}

// §15.7.14 step 27, each computed key evaluated once
fn emit_computed_keys(
  e: Emitter2,
  body: List(ast.ClassElement),
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, pair, next <- each_(e, ast_util.computed_element_keys(body), then: k)
  let #(idx, key_expr) = pair
  use #(tree, e) <- result.try(e.dispatch.emit_expr(e, key_expr))
  use e, kv <- let_(e, tree)
  use e, pk <- host_(e, "to_property_key", [kv])
  store_class_const(e, ast_util.computed_field_const(idx), pk, next)
}

// atoms must match gleam's erlang spelling of MIMethod etc
fn method_install_atom(kind: ast.MethodKind, is_static: Bool) -> ir.Value {
  ir.ConstAtom(case kind, is_static {
    ast.MethodGet, False -> "m_i_getter"
    ast.MethodSet, False -> "m_i_setter"
    ast.MethodMethod, False | ast.MethodConstructor, False -> "m_i_method"
    ast.MethodGet, True -> "m_i_static_getter"
    ast.MethodSet, True -> "m_i_static_setter"
    ast.MethodMethod, True | ast.MethodConstructor, True -> "m_i_static"
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
    ast.MethodGet, Some(n) -> Some("get " <> n)
    ast.MethodSet, Some(n) -> Some("set " <> n)
    _, _ -> base
  }
}

fn resolve_method_key(
  e: Emitter2,
  key: ast.PropertyKey,
  body_index: Int,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
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
  e: Emitter2,
  methods: List(ast_util.ClassMethodEl),
  target_h: ir.Value,
  is_static: Bool,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, method, next <- each_(e, methods, then: k)
  let ast_util.ClassMethodEl(body_index:, key:, kind:, fun:) = method
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
  use e, fn_h <- let_(e, ctree)
  case key {
    ast.KeyPrivate(name:, ..) if !is_static -> {
      use e <- host_unit_(e, "make_method", [fn_h, target_h])
      store_class_const(e, ast_util.private_fn_const(kind, name), fn_h, next)
    }
    ast.KeyPrivate(name:, ..) -> {
      use e <- host_unit_(e, "make_method", [fn_h, target_h])
      use e, pk <- read_class_const(e, name)
      host_unit_(
        e,
        "define_private",
        [target_h, pk, fn_h, method_install_atom(kind, False)],
        next,
      )
    }
    _ -> {
      use e, kv <- resolve_method_key(e, key, body_index)
      host_unit_(
        e,
        "define_method",
        [
          target_h,
          kv,
          fn_h,
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
  e: Emitter2,
  parts: ast_util.ClassBodyParts,
  display_name: Option(String),
  super_v: ir.Value,
  is_derived: Bool,
  has_field_init: Bool,
  ctor_child_id: scope.ScopeId,
  k: Rk(#(ir.Value, ir.Value)),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let #(ctor_params, ctor_body, default) = case parts.constructor {
    Some(ast_util.ClassMethodEl(
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
  use e, ctor_h <- let_(e, ctor_tree)
  use e, proto_h <- host_(e, "class_setup", [ctor_h, super_v])
  let assert [ctx, ..] = e.class_stack
    as "emit_2core/class: emit_ctor_and_create with empty class_stack"
  use e <- host_unit_(e, "cell_set", [ctx.proto_home_cell, proto_h])
  use e <- host_unit_(e, "cell_set", [ctx.static_home_cell, ctor_h])
  use e <- host_unit_(e, "cell_set", [ctx.ctor_self_cell, ctor_h])
  k(e, #(ctor_h, proto_h))
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
pub fn emit_class(
  e: Emitter2,
  binding_name: Option(String),
  display_name: Option(String),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, done <- run_rk(e)
  let saved_strict = e.strict
  let saved_private_env = e.private_env
  let private_names = ast_util.class_private_names(body)
  let e =
    Emitter2(
      ..e,
      strict: True,
      private_env: list.append(private_names, e.private_env),
    )
  let #(e, save) = state.enter_scope(e, in_block: e.in_block)
  use e <- func.binding_prologue(e, e.cur_scope)
  use e <- each_(e, private_names, with: fn(e, pname, next) {
    use e, key <- host_(e, "new_private_name", [
      ir.ConstBinary(bit_array.from_string(pname)),
    ])
    store_class_const(e, pname, key, next)
  })
  let is_derived = option.is_some(super_class)
  use e, proto_home_cell <- host_(e, "cell_new", [e.consts.undef])
  use e, static_home_cell <- host_(e, "cell_new", [e.consts.undef])
  use e, ctor_self_cell <- host_(e, "cell_new", [e.consts.undef])
  let with_inner_cell = fn(e: Emitter2, then: Rk(Option(ir.Value))) {
    case binding_name {
      None -> then(e, None)
      Some(_) -> {
        use e, cell <- host_(e, "cell_new", [e.consts.undef])
        then(e, Some(cell))
      }
    }
  }
  use e, inner_name_cell <- with_inner_cell(e)
  let brand_vars =
    list.fold(private_names, dict.new(), fn(acc, pname) {
      dict.insert(
        acc,
        pname,
        ir.Var(state.get_slot_var(e, class_scope_binding(e, pname).slot)),
      )
    })
  let ctx =
    ClassCtx(
      brand_vars:,
      proto_home_cell:,
      static_home_cell:,
      ctor_self_cell:,
      inner_name_cell:,
      is_derived:,
    )
  let e = Emitter2(..e, class_stack: [ctx, ..e.class_stack])
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
  let with_super = fn(e: Emitter2, k: Rk(ir.Value)) {
    case super_class {
      Some(h) -> {
        use #(tree, e) <- result.try(e.dispatch.emit_expr(e, h))
        let_(e, tree, k)
      }
      None -> k(e, e.consts.tdz)
    }
  }
  use e, super_v <- with_super(e)
  use e, #(ctor_h, proto_h) <- emit_ctor_and_create(
    e,
    parts,
    display_name,
    super_v,
    is_derived,
    has_field_init,
    ctor_child_id,
  )
  use e <- emit_computed_keys(e, body)
  use e <- emit_methods(e, parts.instance_methods, proto_h, False)
  use e <- emit_methods(e, parts.static_methods, ctor_h, True)
  use e, init_h <- emit_field_init_fn(e, parts, proto_h, init_child_id)
  // inner name bound after elements but before statics
  let with_inner_name = fn(
    e,
    then: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
  ) {
    case binding_name {
      Some(n) -> store_class_const(e, n, ctor_h, then)
      None -> then(e)
    }
  }
  use e <- with_inner_name(e)
  let with_fields_init = fn(e, then) {
    case init_h {
      Some(v) -> host_unit_(e, "set_fields_init", [ctor_h, v], then)
      None -> then(e)
    }
  }
  use e <- with_fields_init(e)
  use e <- emit_static_init(e, parts, ctor_h)
  let assert [_, ..outer_class_stack] = e.class_stack
  let e =
    Emitter2(
      ..state.leave_scope(e, save),
      class_stack: outer_class_stack,
      strict: saved_strict,
      private_env: saved_private_env,
    )
  done(e, ir.Values([ctor_h]))
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
  methods: List(ast_util.ClassMethodEl),
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

fn field_init_of(field: ast_util.ClassFieldEl) -> FieldInit {
  let ast_util.ClassFieldEl(body_index:, key:, value:) = field
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

fn field_inits(fields: List(ast_util.ClassFieldEl)) -> List(FieldInit) {
  list.map(fields, field_init_of)
}

fn static_inits(elements: List(ast_util.StaticEl)) -> List(FieldInit) {
  use elem <- list.map(elements)
  case elem {
    ast_util.StaticField(field) -> field_init_of(field)
    ast_util.StaticBlockEl(body) -> StaticBlockInit(body)
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
fn init_fn_flags(rc: state.RealmConsts) -> List(ir.Value) {
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
  e: Emitter2,
  name: String,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case state.resolve(e, name) {
    scope.Plain(scope.Local(slot:, boxed:, ..)) -> {
      let v = ir.Var(state.get_slot_var(e, slot))
      case boxed {
        True -> host_(e, "cell_get", [v], k)
        False -> k(e, v)
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
            "emit_2core/class: init-fn const resolve miss: " <> name,
          )),
        ],
        k,
      )
  }
}

fn emit_one_init(
  e: Emitter2,
  this_v: ir.Value,
  fi: FieldInit,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case fi {
    StaticBlockInit(body:) -> {
      use #(tree, e) <- result.try(e.dispatch.emit_expr(
        e,
        static_block_iife(body),
      ))
      use e, _ <- let_(e, tree)
      next(e)
    }
    PrivateMethodInit(name:, closure_const:, kind:) -> {
      use e, pk <- read_captured_const(e, name)
      use e, closure <- read_captured_const(e, closure_const)
      host_unit_(
        e,
        "define_private",
        [this_v, pk, closure, method_install_atom(kind, False)],
        next,
      )
    }
    PrivateFieldInit(name:, init:) -> {
      use e, pk <- read_captured_const(e, name)
      use #(tree, e) <- result.try(e.dispatch.emit_expr_named(
        e,
        init,
        Some(name),
      ))
      use e, v <- let_(e, tree)
      host_unit_(e, "private_define", [this_v, pk, v], next)
    }
    NamedFieldInit(name:, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.KeyIdentifier(name:, span: ast.Span(0, 0))),
          e,
        )
      use e, kv <- let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr_named(
        e,
        init,
        Some(name),
      ))
      use e, v <- let_(e, tree)
      host_unit_(e, "define_prop", [this_v, kv, v], next)
    }
    NumericFieldInit(value: n, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.KeyNumber(value: n, span: ast.Span(0, 0))),
          e,
        )
      use e, kv <- let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use e, v <- let_(e, tree)
      host_unit_(e, "define_prop", [this_v, kv, v], next)
    }
    BigIntFieldInit(value: i, init:) -> {
      let #(ktree, e) =
        anf.run(
          anf.object_key_lit(ast.KeyBigInt(value: i, span: ast.Span(0, 0))),
          e,
        )
      use e, kv <- let_(e, ktree)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use e, v <- let_(e, tree)
      host_unit_(e, "define_prop", [this_v, kv, v], next)
    }
    ComputedFieldInit(key_const:, init:) -> {
      use e, kv <- read_captured_const(e, key_const)
      use #(tree, e) <- result.try(e.dispatch.emit_expr(e, init))
      use e, v <- let_(e, tree)
      host_unit_(e, "define_prop", [this_v, kv, v], next)
    }
  }
}

fn build_class_init_closure(
  e: Emitter2,
  child_id: scope.ScopeId,
  inits: List(FieldInit),
  home_h: ir.Value,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let child_info = scope.function_info(e.tree, child_id)
  let capture_vals = func.build_capture_values(e, child_info)
  let #(fn_name, e) = state.fresh_fn_name(e, None)
  let #(e_child, save) =
    state.enter_function(
      e,
      child_id,
      strict: True,
      is_async: False,
      is_generator: False,
      is_arrow: False,
    )
  let e_child = func.seed_capture_slots(e_child, child_info)
  use #(body_expr, e_child) <- result.try(
    run_rk(e_child, fn(ec, done) {
      use ec <- func.unpack_frame(ec, False, child_info)
      use ec <- func.binding_prologue(ec, ec.fn_scope)
      let with_this = fn(ec, k) {
        case lexical.lexical_slot(child_info.lexical, lexical.RefThis) {
          Some(slot) -> {
            let v = ir.Var(state.get_slot_var(ec, slot))
            case state.lexical_is_boxed(ec, child_info, lexical.RefThis) {
              True -> host_(ec, "cell_get", [v], k)
              False -> k(ec, v)
            }
          }
          None -> k(ec, ec.consts.undef)
        }
      }
      use ec, this_v <- with_this(ec)
      each_(
        ec,
        inits,
        then: fn(ef) { done(ef, ir.Return([ef.consts.undef])) },
        with: fn(ec, fi, next) { emit_one_init(ec, this_v, fi, next) },
      )
    }),
  )
  let ncap = func.capture_count(child_info)
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
  let capture_vals =
    list.append(capture_vals, state.keys_args(e_child.uses_keys))
  let e = state.leave_function(e_child, save)
  use e, fun <- let_(e, ir.MakeClosure(fn_name, capture_vals, 2))
  use e, flags_t <- let_(e, ir.TermOp(ir.MakeTuple, init_fn_flags(e.consts)))
  use e, fn_h <- host_(e, "fn_new", [
    fun,
    flags_t,
    e.consts.empty_bin,
    ir.ConstI32(0),
    ir.ConstAtom("none"),
  ])
  use e <- host_unit_(e, "make_method", [fn_h, home_h])
  k(e, fn_h)
}

fn emit_field_init_fn(
  e: Emitter2,
  parts: ast_util.ClassBodyParts,
  proto_h: ir.Value,
  init_child_id: Option(scope.ScopeId),
  k: Rk(Option(ir.Value)),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let inits =
    list.append(
      private_method_inits(parts.instance_methods),
      field_inits(parts.instance_fields),
    )
  case inits {
    [] -> {
      use e <- store_class_const(e, ast_util.class_fields_init, e.consts.undef)
      k(e, None)
    }
    _ -> {
      let assert Some(child_id) = init_child_id
        as "emit_2core/class: has_instance_field_init/parser needs_instance_init desync"
      use e, init_h <- build_class_init_closure(e, child_id, inits, proto_h)
      use e <- store_class_const(e, ast_util.class_fields_init, init_h)
      k(e, Some(init_h))
    }
  }
}

fn emit_static_init(
  e: Emitter2,
  parts: ast_util.ClassBodyParts,
  ctor_h: ir.Value,
  k: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let inits = static_inits(parts.static_elements)
  case inits {
    [] -> k(e)
    _ -> {
      let #(child_id, e) = state.pop_child_fn(e)
      use e, static_h <- build_class_init_closure(e, child_id, inits, ctor_h)
      use e, empty <- host_(e, "empty_list", [])
      host_unit_(e, "call", [static_h, ctor_h, empty], k)
    }
  }
}
