//// M16: JS ClassDefinitionEvaluation §15.7.14 → twocore IR via M7
//// rt_js_class host ops. Faithful port of arc/compiler/emit.gleam:7085-7715
//// into the Result-aware Rk-CPS style. D2 (Arch-A): NO St threading.
//// R11: MethodInstallKind wire-encoded as ConstAtom("m_i_method"/…).
//// R7: 0-based TupleGet. R14: emit_function returns #(ir.Expr, Emitter2).

import arc/compiler/ast_util
import arc/compiler/scope.{type Binding}
import arc/parser/ast
import arc/vm/lexical
import arc_aot/emit/anf
import arc_aot/emit/func
import arc_aot/emit/state.{type EmitError, type Emitter2, ClassCtx, Emitter2}
import gleam/bit_array
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import twocore/ir

// ── Result-aware CPS (Rk chain) — mirrors func.gleam ────────────────────────
// Rk returns the tree paired with the emitter the chain finished with.
// R12 Result channel; host_ is a sanctioned CallHost("js",..) site alongside
// anf.host.

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

/// Right-fold `step` over `items`, threading e, building nested Lets.
/// `then` is labelled so `use e, x, next <- each_(…, then: k)` reads well.
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

/// Run an Rk chain whose leaf calls `done(ef, tree)`.
fn run_rk(
  e: Emitter2,
  f: fn(
    Emitter2,
    fn(Emitter2, ir.Expr) -> Result(#(ir.Expr, Emitter2), EmitError),
  ) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  f(e, fn(ef, tree) { Ok(#(tree, ef)) })
}

// ── class-scope const slot access (shared by scaffold/methods/init-fns) ─────
// Every synthetic class-scope const (inner name binding, #priv names,
// private_fn_const stashes, computed_field_const stashes, class_fields_init)
// is a ConstBinding declared by ast_util.class_body_bindings into the
// per-class body scope. These helpers resolve the name in `e.cur_scope` and
// read/write the slot directly (no scope.lookup with-chain — ClassBody scopes
// never sit under `with`). Port of emit.gleam emit_var_get/emit_var_init.

fn class_scope_binding(e: Emitter2, name: String) -> Binding {
  let assert Ok(b) =
    dict.get(scope.get_scope(e.tree, e.cur_scope).bindings, name)
    as "emit_2core/class: name missing from ClassBody-scope bindings"
  b
}

/// Store `v` into class-scope const `name` and mark its slot initialized (TDZ
/// exit). Boxed → cell_set on the seeded cell; unboxed → Let-rebind + slot_var
/// remap. Port of emit.gleam emit_var_init (via stmt.store_slot).
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
      let vn = state.slot_var_name(b.slot)
      use body <- state.map_tree(k(state.set_slot_var(e, b.slot, vn)))
      ir.Let([vn], ir.Values([v]), body)
    }
  }
}

/// Read class-scope const `name`. Boxed → cell_get; unboxed → Var(slot_var).
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

// ── §15.7.14 step 27: computed element keys (port emit.gleam:7253-7268) ─────

/// Evaluate + ToPropertyKey each computed element name (fields AND methods,
/// instance AND static) in one source-order pass, into its
/// `computed_field_const(idx)` stash slot. Runs after heritage, before method
/// definitions read the stashed keys, and before the inner class-name binding
/// is initialized. Abrupt completions (throwing getters, ToPrimitive) surface
/// here at class evaluation.
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

// ── §15.7.14 steps 28-29: method definitions (port emit.gleam:7388-7536) ────

/// R11: MethodInstallKind wire-encoded as ConstAtom — Gleam `MIMethod` mangles
/// to Erlang atom `m_i_method` (each capital splits), so the wire atoms must
/// match that exact spelling for rt_js_class.t_define_method's case to hit.
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

/// SetFunctionName-derived closure name: `"get "|"set " <> name` for
/// accessors, `name` for plain methods, `None` for computed/numeric/bigint
/// keys (runtime SetFunctionName from key not implemented — matches
/// object-literal computed methods, emit.gleam:7502).
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

/// Resolve a method's property key to an ir.Value: KeyComputed → read the
/// pre-evaluated stash const (never re-evaluate); KeyPrivate → read the
/// #name class-scope slot (the {js_private,uid} minted by scaffold step 3);
/// literal keys → compile-time-canonical wire tuple via anf.object_key_lit.
/// Port of emit.gleam:7543-7557 emit_class_element_key.
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

/// Define a list of class methods on `target_h` (ctor.prototype when
/// `is_static: False`, ctor itself when `True`). Port of emit.gleam:7388-7536
/// emit_class_methods. For each method: pop its analyzer-assigned child fn
/// scope; build the closure via dispatch.emit_function(Method(..)); wrap with
/// make_method to record [[HomeObject]]=target (M7 C4); then install:
///  - private + instance → stash closure in private_fn_const slot (field-init
///    fn installs it per-instance per §7.3.29 PrivateMethodOrAccessorAdd)
///  - private + static  → define_private on ctor now
///  - public            → define_method on target now
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
  // M7 C4 t_make_method is JMutUnit — mutates fn_h in place; result is fn_h.
  use e <- host_unit_(e, "make_method", [fn_h, target_h])
  case key {
    ast.KeyPrivate(name:, ..) if !is_static ->
      // Instance private method/accessor: closure built once here at class
      // definition time with [[HomeObject]] set; stash it in the hidden
      // class-scope const for the field-init fn to install per-instance.
      store_class_const(e, ast_util.private_fn_const(kind, name), fn_h, next)
    ast.KeyPrivate(name:, ..) -> {
      // Static private: install on the constructor right now.
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

// ── §15.7.14 steps 8-14: constructor + class_setup (port emit.gleam:7159-7290) ──

/// True when `parts` will yield a non-empty instance field-initializer fn —
/// instance fields OR any private instance method (installed per-instance per
/// §7.3.29). Gates ClassCtor(has_field_init:) so a derived ctor emits the
/// field-init call after `super()`. Port of emit.gleam:7205-7209.
fn has_instance_field_init(parts: ast_util.ClassBodyParts) -> Bool {
  parts.instance_fields != []
  || list.any(parts.instance_methods, fn(m) {
    case m.key {
      ast.KeyPrivate(..) -> True
      _ -> False
    }
  })
}

/// Build the constructor closure (explicit or spec-default synthesized), then
/// call `class_setup` to validate the heritage, allocate the prototype and
/// wire it to the constructor, and populate the ClassCtx home-object cells.
/// The heritage expression is evaluated by the scaffold BEFORE this call
/// (into `super_v`, the `tdz` sentinel when there is no `extends`). `k`
/// receives `#(ctor_h, proto_h)`.
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
  // Populate ClassCtx cells NOW so method/field-init closures capturing them
  // (via home-object / ctor-self) see the allocated proto/ctor.
  let assert [ctx, ..] = e.class_stack
    as "emit_2core/class: emit_ctor_and_create with empty class_stack"
  use e <- host_unit_(e, "cell_set", [ctx.proto_home_cell, proto_h])
  use e <- host_unit_(e, "cell_set", [ctx.static_home_cell, ctor_h])
  use e <- host_unit_(e, "cell_set", [ctx.ctor_self_cell, ctor_h])
  k(e, #(ctor_h, proto_h))
}

/// §15.7.14 step 14.a default constructor body: empty for a base class; for
/// a derived class a `super()` call which, under `default_ctor`, forwards the
/// incoming argument list without iterating it.
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

// ── §15.7.14 ClassDefinitionEvaluation spine (port emit.gleam:7085-7155) ─────

/// state.EmitDispatch.emit_class (state.gleam:246-252). `binding_name` is the
/// class's own BindingIdentifier (drives the immutable inner binding + TDZ);
/// `display_name` is the NamedEvaluation-inferred `.name` (may differ for
/// `const X = class {}`). Result IR value is the constructor handle.
/// Step ordering EXACTLY matches emit.gleam:7085-7155: private-names bound
/// BEFORE any child closure; inner-name bound AFTER computed-keys/methods but
/// BEFORE the static-init call.
pub fn emit_class(
  e: Emitter2,
  binding_name: Option(String),
  display_name: Option(String),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  use e, done <- run_rk(e)
  // (1) §15.7.1: class bodies are always strict. §15.7.14 step 6.b/c: extend
  // PrivateEnvironment for the class body's duration. Restored at exit.
  let saved_strict = e.strict
  let saved_private_env = e.private_env
  let private_names = ast_util.class_private_names(body)
  let e =
    Emitter2(
      ..e,
      strict: True,
      private_env: list.append(private_names, e.private_env),
    )
  // (2) §15.7.14 step 4/5: per-class ClassBody scope. binding_prologue seeds
  // every ast_util.class_body_bindings const to tdz BEFORE heritage emit so
  // `class C extends C {}` TDZs on the inner C, and so every child closure's
  // MakeClosure snapshot sees the slots for capture.
  let #(e, save) = state.enter_scope(e, in_block: e.in_block)
  use e <- func.binding_prologue(e, e.cur_scope)
  // (3) §15.7.14 steps 5/6: mint each PrivateName NOW (after declaration,
  // before any child closure) so each class evaluation gets distinct keys and
  // methods/field-inits capture them via ordinary lexical scoping.
  use e <- each_(e, private_names, with: fn(e, pname, next) {
    use e, key <- host_(e, "new_private_name", [
      ir.ConstBinary(bit_array.from_string(pname)),
    ])
    store_class_const(e, pname, key, next)
  })
  // (4) HomeObject/self cells for ClassCtx — allocated before any child
  // closure so ctor/method bodies capture them; filled by emit_ctor_and_create.
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
  // Pre-pop init/ctor shell ids in analyzer registration order — parser.gleam
  // class_scope_finalize:3903 fixes children_at[class_id] to [init?, ctor,
  // heritage, keys, imethods, smethods, static?]; heritage/key exprs may nest
  // fn-exprs whose pop_child_fn must land at step 3/4. Port emit.gleam:7197→
  // 7210→7231 pops init→ctor→heritage.
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
  // (5) Heritage evaluation (§15.7.14 step 7) — before ctor closure creation
  // so an abrupt heritage aborts before any child fn is compiled/installed.
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
  // (6-10) Child compilation + installation, in §15.7.14 order.
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
  // (11) §15.7.14: bind inner name to F AFTER all element evaluation (computed
  // keys/methods can't see it) but BEFORE static element evaluation, so
  // `class C { static x = C }` sees the constructor.
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
  // (12) §15.7.14 step 25: attach [[Fields]] initializer to ctor; then run
  // static elements (fields + blocks) with `this = ctor`.
  let with_fields_init = fn(e, then) {
    case init_h {
      Some(v) -> host_unit_(e, "set_fields_init", [ctor_h, v], then)
      None -> then(e)
    }
  }
  use e <- with_fields_init(e)
  use e <- emit_static_init(e, parts, ctor_h)
  // (13) Pop ClassCtx, leave ClassBody scope, restore enclosing strict/private.
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

// ── §15.7.14 [[Fields]] / static-init closures (port emit.gleam:7310-7726) ──
// A field-initializer body is a list of FieldInits, not statements, so these
// build the ir.Function directly from func's prologue pieces rather than
// routing through dispatch.emit_function.

/// One element of a class-init function body. Verbatim port of arc
/// emit.gleam:331-358 FieldInit.
type FieldInit {
  /// §7.3.29 PrivateMethodOrAccessorAdd — closure was built at class-def time
  /// into `closure_const`; the init fn reads it back and installs per-instance.
  PrivateMethodInit(name: String, closure_const: String, kind: ast.MethodKind)
  /// §7.3.28 PrivateFieldAdd — the PrivateName lives in class-scope const `#x`.
  PrivateFieldInit(name: String, init: ast.Expression)
  /// §7.3.33 DefineField step 7: anonymous fn initializers get NamedEvaluation.
  NamedFieldInit(name: String, init: ast.Expression)
  NumericFieldInit(value: ast.LiteralNumber, init: ast.Expression)
  /// The key was evaluated + ToPropertyKey'd once at class-definition time
  /// into `key_const` (emit_computed_keys).
  ComputedFieldInit(key_const: String, init: ast.Expression)
  BigIntFieldInit(value: Int, init: ast.Expression)
  /// `static { … }` — lowered to an arrow IIFE so its body's scope (which the
  /// analyzer created as a Function-kind child of the static-init shell) is
  /// consumed via the ordinary pop_child_fn path. Port of emit.gleam:7654.
  StaticBlockInit(body: List(ast.StmtWithLine))
}

/// §7.3.29: per-instance install elements for every instance private
/// method/accessor half. Spec order: all private methods install BEFORE field
/// initializers (InitializeInstanceElements steps 5 then 6). Port of
/// emit.gleam:7583-7598.
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
  // §15.7.14: absent Initializer → undefined; carries key's span for errors.
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

/// Map every instance ClassField → its FieldInit. Port of emit.gleam:7603-7606.
fn field_inits(fields: List(ast_util.ClassFieldEl)) -> List(FieldInit) {
  list.map(fields, field_init_of)
}

/// Map static elements → FieldInits of the static-init wrapper body:
/// `static x = v` defines on `this` = ctor; `static { … }` → StaticBlockInit.
/// Port of emit.gleam:7635-7641.
fn static_inits(elements: List(ast_util.StaticEl)) -> List(FieldInit) {
  use elem <- list.map(elements)
  case elem {
    ast_util.StaticField(field) -> field_init_of(field)
    ast_util.StaticBlockEl(body) -> StaticBlockInit(body)
  }
}

/// `(() => { …body })()` — synthetic static-block lowering so its analyzer
/// scope (Function-kind child of the static-init shell) is popped by the
/// ordinary Arrow path in dispatch.emit_expr. Port of emit.gleam:7716-7726.
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

/// FnFlags wire tuple for a ClassInitFn — strict (§15.7.1: class bodies are
/// strict code), every other flag False. MUST match arc/rt/types.FnFlags
/// field order + tag exactly (func.gleam emit_closure_site).
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

/// Read a class-scope const from INSIDE the init-fn body — the const is a
/// capture of the enclosing ClassBody scope, so resolve via the ordinary
/// scope-tree lookup (state.resolve) rather than cur_scope's own bindings.
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
    // Class-scope synthetic consts are always Local; anything else is an
    // analyzer/emit desync — surface as a runtime ReferenceError (R12).
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

/// Emit one element of a class-init function body. `this_v` is the instance
/// (or the ctor for static elements). Port of emit.gleam:7646-7706.
fn emit_one_init(
  e: Emitter2,
  this_v: ir.Value,
  fi: FieldInit,
  next: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case fi {
    StaticBlockInit(body:) -> {
      // Arrow IIFE so `this` inside the block reads the enclosing wrapper's,
      // and the block's own analyzer scope is consumed by pop_child_fn.
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

/// Compile `inits` into a synthetic ClassInitFn ir.Function, register it, and
/// build the parent-frame closure (MakeClosure → fn_new → make_method(home_h)).
/// Port of emit.gleam:7330-7348 + emit_attach_field_init/
/// emit_call_static_init.
fn build_class_init_closure(
  e: Emitter2,
  child_id: scope.ScopeId,
  inits: List(FieldInit),
  home_h: ir.Value,
  k: Rk(ir.Value),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  let child_info = scope.function_info(e.tree, child_id)
  // Capture values are read from the PARENT frame BEFORE enter_function.
  let capture_vals = func.build_capture_values(e, child_info)
  let #(fn_name, e) = state.fresh_fn_name(e)
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
  // Body: unpack _frame → seed lexical slots (this at idx 0, R7) → prologue for
  // any own bindings of the shell scope → each_ emit_one_init → Return([undef]).
  use #(body_expr, e_child) <- result.try(
    run_rk(e_child, fn(ec, done) {
      use ec <- func.unpack_frame(ec, False, child_info)
      use ec <- func.binding_prologue(ec, ec.fn_scope)
      // this_v is the define_prop/private_define target. When RefThis is boxed
      // (an arrow initializer captures `this`) unpack_frame maps the slot to
      // the CELL — unbox so this_v is the raw instance/ctor (emit.gleam:2601).
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
  let ncap =
    list.length(child_info.captures) + dict.size(child_info.lexical_captures)
  let e_child =
    state.add_function(
      e_child,
      ir.Function(
        name: fn_name,
        params: func.build_ir_params(0, ncap),
        result: [ir.TTerm],
        locals: [],
        body: body_expr,
      ),
    )
  let e = state.leave_function(e_child, save)
  // Closure site (SPEC.md:1388): MakeClosure → fn_new → make_method(home_h).
  use e, fun <- let_(e, ir.MakeClosure(fn_name, capture_vals, 2))
  use e, flags_t <- let_(e, ir.TermOp(ir.MakeTuple, init_fn_flags(e.consts)))
  use e, fn_h <- host_(e, "fn_new", [
    fun,
    flags_t,
    e.consts.empty_bin,
    ir.ConstI32(0),
    ir.ConstAtom("none"),
  ])
  // M7 C4 t_make_method is JMutUnit — mutates fn_h in place; result is fn_h.
  use e <- host_unit_(e, "make_method", [fn_h, home_h])
  k(e, fn_h)
}

/// §15.7.14 step 25 [[Fields]] initializer. Port of emit.gleam:7330-7348 +
/// emit_attach_field_init:7357-7371. Instance private methods install BEFORE
/// fields (InitializeInstanceElements §7.3.31 steps 5 then 6). No-field case
/// stores `undefined` into `class_fields_init` so the derived-ctor call site's
/// truthy-gate skips and the const isn't TDZ. `k` receives Some(init_h)/None.
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
      // Invariant: inits ≠ [] ⟺ has_instance_field_init(parts) ⟺ parser
      // needs_instance_init kept the shell ⟺ emit_class pre-popped Some(id).
      let assert Some(child_id) = init_child_id
        as "emit_2core/class: has_instance_field_init/parser needs_instance_init desync"
      use e, init_h <- build_class_init_closure(e, child_id, inits, proto_h)
      use e <- store_class_const(e, ast_util.class_fields_init, init_h)
      k(e, Some(init_h))
    }
  }
}

/// §15.7.14 step 31: static fields + `static {}` blocks in source order. Port
/// of emit.gleam:7282-7288 + emit_call_static_init:7560-7574. Builds the
/// static-initializer closure with [[HomeObject]] = ctor and immediately
/// [[Call]]s it with `this` = ctor. No-op when the class has no static
/// elements (parser discarded the shell scope so no pop_child_fn).
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
      // Analyzer step 7: static shell is last in children_at, so popping here
      // (after methods + field-init) is already in position.
      let #(child_id, e) = state.pop_child_fn(e)
      use e, static_h <- build_class_init_closure(e, child_id, inits, ctor_h)
      use e, empty <- host_(e, "empty_list", [])
      // D4: JS invocation via host("call",[f, this, argsL]).
      host_unit_(e, "call", [static_h, ctor_h, empty], k)
    }
  }
}
