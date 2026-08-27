import arc/bytecode/lexical
import arc/compiler/scope.{type ScopeId, type ScopeTree}
import arc/parser/ast
import arc_aot/emit/split
import carder/ir
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/set.{type Set}
import gleam/string

pub type RealmConsts {
  RealmConsts(
    undef: ir.Value,
    null: ir.Value,
    true_: ir.Value,
    false_: ir.Value,
    nan: ir.Value,
    pos_inf: ir.Value,
    neg_inf: ir.Value,
    tdz: ir.Value,
    empty_bin: ir.Value,
    js_tag: String,
  )
}

pub fn realm_consts() -> RealmConsts {
  RealmConsts(
    undef: ir.ConstAtom("undefined"),
    null: ir.ConstAtom("null"),
    true_: ir.ConstAtom("true"),
    false_: ir.ConstAtom("false"),
    nan: ir.ConstAtom("js_nan"),
    pos_inf: ir.ConstAtom("js_inf"),
    neg_inf: ir.ConstAtom("js_neg_inf"),
    tdz: ir.ConstAtom("js_tdz"),
    empty_bin: ir.ConstBinary(<<>>),
    js_tag: "js_exn",
  )
}

pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  EarlySyntaxError(message: String)
  UnsupportedFeature(feature: String)
  ScopeCursorDesync(at: ScopeId)
}

pub type FieldInitMode {
  NoFieldInit
  FieldInitAtStart
  FieldInitAfterSuper
}

pub type ClassCtx {
  ClassCtx(
    brand_vars: Dict(String, ir.Value),
    proto_home_cell: ir.Value,
    static_home_cell: ir.Value,
    ctor_self_cell: ir.Value,
    inner_name_cell: Option(ir.Value),
    is_derived: Bool,
  )
}

pub type ScopeSave2 {
  ScopeSave2(
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    slot_vars: Dict(Int, String),
    in_block: Bool,
  )
}

pub type FnSave {
  FnSave(
    fn_scope: ScopeId,
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    child_fn_cursor: List(ScopeId),
    in_block: Bool,
    frame_stack: List(Frame2),
    pending_label: Option(String),
    strict: Bool,
    is_async: Bool,
    is_generator: Bool,
    is_arrow: Bool,
    with_stack: List(String),
    private_env: List(String),
    field_init: FieldInitMode,
    derived_ctor: Bool,
    default_ctor: Bool,
    this_tdz: Bool,
    class_stack: List(ClassCtx),
    slot_vars: Dict(Int, String),
    cap_names: List(String),
    initialized: Set(Int),
    hoisted_kfn: Dict(Int, ir.Value),
    sm_abrupt: Option(SmAbrupt),
    raw_args_var: Option(String),
  )
}

pub type Frame2 {
  Loop2(
    ir_break: String,
    ir_continue: String,
    js_label: Option(String),
    carried: List(Int),
    iter_close: Option(#(String, Escape)),
  )
  Switch2(ir_break: String, js_label: Option(String), carried: List(Int))
  Labeled2(ir_break: String, js_label: String, carried: List(Int))
  Barrier2(
    finally_body: Option(#(List(ast.StmtWithLine), ScopeSave2)),
    iter_close: Option(String),
    escape: Option(Escape),
  )
}

pub type Escape {
  Escape(label: String, arity: Int)
}

pub type BarrierCleanup {
  FinallyBlock(
    body: List(ast.StmtWithLine),
    saved_scope: ScopeSave2,
    escape: Option(Escape),
  )
  IterClose(iter_var: String, is_async: Bool, escape: Option(Escape))
  CatchOnly
}

pub type K =
  fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError)

pub fn map_tree(
  r: Result(#(ir.Expr, Emitter2), EmitError),
  f: fn(ir.Expr) -> ir.Expr,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case r {
    Ok(#(tree, e)) -> Ok(#(f(tree), e))
    Error(err) -> Error(err)
  }
}

pub type BindMode {
  BindLet
  BindConst
  BindVar
  BindAssign
}

pub type FnShape {
  FnDecl(is_gen: Bool, is_async: Bool)
  FnExpr(self_name: Option(String), is_gen: Bool, is_async: Bool)
  Arrow(is_async: Bool)
  Method(is_gen: Bool, is_async: Bool)
  ClassCtor(derived: Bool, has_field_init: Bool, default: Bool)
  ClassInitFn
}

pub type FnBody {
  StmtBody(List(ast.StmtWithLine))
  ExprBody(ast.Expression)
}

pub type FnSite {
  DirectFn(
    name: String,
    captures: List(ir.Value),
    arity: Int,
    needs_this: Bool,
    strict: Bool,
  )
  ClosureSite(tree: ir.Expr)
}

pub type CoroutineKind {
  CorGenerator
  CorAsync
  CorAsyncGen
}

pub type SmAbrupt {
  SmAbrupt(
    on_return: fn(Emitter2, ir.Value) -> Result(#(ir.Expr, Emitter2), EmitError),
    on_goto: fn(Emitter2, String) ->
      Option(Result(#(ir.Expr, Emitter2), EmitError)),
  )
}

pub type EmitDispatch {
  EmitDispatch(
    emit_expr: fn(Emitter2, ast.Expression) ->
      Result(#(ir.Expr, Emitter2), EmitError),
    emit_expr_named: fn(Emitter2, ast.Expression, Option(String)) ->
      Result(#(ir.Expr, Emitter2), EmitError),
    emit_stmts: fn(Emitter2, List(ast.StmtWithLine), K) ->
      Result(#(ir.Expr, Emitter2), EmitError),
    emit_pattern: fn(Emitter2, ast.Pattern, ir.Value, BindMode) ->
      Result(#(ir.Expr, Emitter2), EmitError),
    emit_function: fn(
      Emitter2,
      FnShape,
      Option(String),
      List(ast.Pattern),
      FnBody,
      ScopeId,
    ) -> Result(#(ir.Expr, Emitter2), EmitError),
    emit_function_site: fn(
      Emitter2,
      FnShape,
      Option(String),
      List(ast.Pattern),
      FnBody,
      ScopeId,
    ) -> Result(#(FnSite, Emitter2), EmitError),
    emit_class: fn(
      Emitter2,
      Option(String),
      Option(String),
      Option(ast.Expression),
      List(ast.ClassElement),
    ) -> Result(#(ir.Expr, Emitter2), EmitError),
    emit_async_body: fn(
      Emitter2,
      FnShape,
      Option(String),
      List(ast.Pattern),
      FnBody,
      ScopeId,
      List(ir.Value),
    ) -> Result(#(ir.Expr, Emitter2), EmitError),
    emit_destructure: fn(Emitter2, ast.Pattern, ir.Value, BindMode) ->
      Result(#(ir.Expr, Emitter2), EmitError),
  )
}

pub type Emitter2 {
  Emitter2(
    tree: ScopeTree,
    fn_scope: ScopeId,
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    child_fn_cursor: List(ScopeId),
    in_block: Bool,
    slot_names: Dict(#(ScopeId, Int), String),
    cap_names: List(String),
    next_var: Int,
    next_label: Int,
    next_fn: Int,
    fn_names: Set(String),
    next_site: Int,
    module_name: String,
    frame_stack: List(Frame2),
    pending_label: Option(String),
    fns_acc: List(ir.Function),
    unsupported: List(String),
    strict: Bool,
    is_async: Bool,
    is_generator: Bool,
    is_arrow: Bool,
    with_stack: List(String),
    private_env: List(String),
    field_init: FieldInitMode,
    derived_ctor: Bool,
    default_ctor: Bool,
    this_tdz: Bool,
    slot_vars: Dict(Int, String),
    initialized: Set(Int),
    known_numbers: Set(String),
    known_strings: Set(String),
    hoisted_kfn: Dict(Int, ir.Value),
    const_globals: Dict(String, ir.Value),
    slotted_globals: Dict(String, Int),
    class_stack: List(ClassCtx),
    sm_abrupt: Option(SmAbrupt),
    raw_args_var: Option(String),
    dispatch: EmitDispatch,
    consts: RealmConsts,
  )
}

// never cleared; var names are module-unique
pub fn mark_known_number(e: Emitter2, name: String) -> Emitter2 {
  Emitter2(..e, known_numbers: set.insert(e.known_numbers, name))
}

pub fn is_known_number(e: Emitter2, name: String) -> Bool {
  set.contains(e.known_numbers, name)
}

pub fn mark_known_string(e: Emitter2, name: String) -> Emitter2 {
  Emitter2(..e, known_strings: set.insert(e.known_strings, name))
}

pub fn is_known_string(e: Emitter2, name: String) -> Bool {
  set.contains(e.known_strings, name)
}

pub fn set_const_globals(e: Emitter2, d: Dict(String, ir.Value)) -> Emitter2 {
  Emitter2(..e, const_globals: d)
}

pub fn set_slotted_globals(e: Emitter2, d: Dict(String, Int)) -> Emitter2 {
  Emitter2(..e, slotted_globals: d)
}

pub fn lookup_slotted_global(e: Emitter2, name: String) -> Option(Int) {
  case dict.get(e.slotted_globals, name) {
    Ok(slot) -> Some(slot)
    Error(_) -> None
  }
}

pub fn fresh_var(e: Emitter2) -> #(String, Emitter2) {
  #("_t" <> int_to_string(e.next_var), Emitter2(..e, next_var: e.next_var + 1))
}

pub fn let_tail_value(rhs: ir.Expr) -> Option(ir.Value) {
  case rhs {
    ir.Values([v]) -> Some(v)
    ir.Let(_, _, body) -> let_tail_value(body)
    _ -> None
  }
}

pub fn splice_let(rhs: ir.Expr, drop: String, body: ir.Expr) -> ir.Expr {
  case rhs {
    ir.Let(names, inner, rest) ->
      ir.Let(names, inner, splice_let(rest, drop, body))
    ir.Values([_]) -> body
    _ -> ir.Let([drop], rhs, body)
  }
}

// splices the let spine so rhs names stay in scope for k
pub fn let_(
  e: Emitter2,
  rhs: ir.Expr,
  k: fn(Emitter2, ir.Value) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  case rhs {
    ir.Let(names, inner_rhs, inner_body) -> {
      use tail <- map_tree(let_(e, inner_body, k))
      ir.Let(names, inner_rhs, tail)
    }
    ir.Values([v]) -> k(e, v)
    _ -> {
      let #(n, e) = fresh_var(e)
      let e = case let_tail_value(rhs) {
        Some(ir.Var(vn)) ->
          case is_known_number(e, vn) {
            True -> mark_known_number(e, n)
            False -> e
          }
        _ -> e
      }
      use body <- map_tree(k(e, ir.Var(n)))
      ir.Let([n], rhs, body)
    }
  }
}

pub fn fresh_label(e: Emitter2) -> #(String, Emitter2) {
  #(
    "_L" <> int_to_string(e.next_label),
    Emitter2(..e, next_label: e.next_label + 1),
  )
}

pub fn fresh_fn_name(
  e: Emitter2,
  js_name: Option(String),
) -> #(String, Emitter2) {
  let base = case option.then(js_name, fn_base) {
    Some(name) -> name
    None -> "fn_" <> int_to_string(e.next_fn)
  }
  let name = unique_fn_name(base, e.fn_names, 2)
  #(
    name,
    Emitter2(
      ..e,
      next_fn: e.next_fn + 1,
      fn_names: set.insert(e.fn_names, name),
    ),
  )
}

fn fn_base(js_name: String) -> Option(String) {
  let name =
    string.to_graphemes(js_name)
    |> list.map(fn(g) {
      case g {
        "_" -> "_"
        _ ->
          case is_ascii_digit(g) || is_ascii_letter(g) {
            True -> string.lowercase(g)
            False -> "_"
          }
      }
    })
    |> string.concat
  let all_underscores = string.replace(name, "_", "") == ""
  case name, all_underscores {
    "", _ -> None
    _, True -> None
    "js_main", _ | "instantiate", _ | "module_info", _ -> Some(name <> "_")
    _, _ ->
      case string.first(name) {
        Ok(first) ->
          case is_ascii_digit(first) {
            True -> None
            False -> Some(name)
          }
        Error(Nil) -> None
      }
  }
}

fn is_ascii_digit(g: String) -> Bool {
  case g {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_ascii_letter(g: String) -> Bool {
  case string.to_utf_codepoints(g) {
    [cp] -> {
      let c = string.utf_codepoint_to_int(cp)
      { c >= 65 && c <= 90 } || { c >= 97 && c <= 122 }
    }
    _ -> False
  }
}

fn unique_fn_name(base: String, taken: Set(String), n: Int) -> String {
  case fn_name_free(base, taken) {
    True -> base
    False -> {
      let cand = base <> "_" <> int_to_string(n)
      case fn_name_free(cand, taken) {
        True -> cand
        False -> unique_fn_name(base, taken, n + 1)
      }
    }
  }
}

fn fn_name_free(cand: String, taken: Set(String)) -> Bool {
  !set.contains(taken, cand)
  && !set.contains(taken, cand <> "_s")
  && !set.contains(taken, cand <> "_t")
  && !set.contains(taken, cand <> "__sm")
  && list.all(["_s", "_t", "__sm"], fn(suffix) {
    case strip_suffix(cand, suffix) {
      Some(stem) -> !set.contains(taken, stem)
      None -> True
    }
  })
  && case strip_chunk_suffix(cand) {
    Some(stem) -> !set.contains(taken, stem)
    None -> True
  }
}

fn strip_suffix(s: String, suffix: String) -> Option(String) {
  case string.ends_with(s, suffix) {
    True -> Some(string.drop_end(s, string.length(suffix)))
    False -> None
  }
}

fn strip_chunk_suffix(s: String) -> Option(String) {
  case string.split(s, "_c") {
    [_, _, ..] -> {
      let assert Ok(last) = list.last(string.split(s, "_c"))
      case int.parse(last) {
        Ok(_) -> Some(string.drop_end(s, string.length(last) + 2))
        Error(Nil) -> None
      }
    }
    _ -> None
  }
}

pub fn add_function(e: Emitter2, f: ir.Function) -> Emitter2 {
  let fs = list.reverse(split.function(f))
  Emitter2(..e, fns_acc: list.append(fs, e.fns_acc))
}

pub fn take_functions(e: Emitter2) -> List(ir.Function) {
  list.reverse(e.fns_acc)
}

pub fn mark_unsupported(e: Emitter2, feature: String) -> Emitter2 {
  Emitter2(..e, unsupported: [feature, ..e.unsupported])
}

pub fn slot_var_name(e: Emitter2, slot: Int) -> String {
  case dict.get(e.slot_names, #(e.fn_scope, slot)) {
    Ok(name) -> name
    Error(Nil) -> "js_local_" <> int_to_string(slot)
  }
}

pub fn get_slot_var(e: Emitter2, slot: Int) -> String {
  case dict.get(e.slot_vars, slot) {
    Ok(name) -> name
    Error(_) -> slot_var_name(e, slot)
  }
}

fn slot_names(tree: ScopeTree) -> Dict(#(ScopeId, Int), String) {
  let by_frame =
    dict.fold(tree.scopes, dict.new(), fn(acc, _id, sc) {
      dict.fold(sc.bindings, acc, fn(acc, js_name, b) {
        dict.upsert(acc, sc.function_scope, fn(existing) {
          [#(b.slot, js_name), ..option.unwrap(existing, [])]
        })
      })
    })
  dict.fold(by_frame, dict.new(), fn(acc, frame, entries) {
    let sorted =
      list.sort(entries, fn(a, b) {
        case int.compare(a.0, b.0) {
          order.Eq -> string.compare(a.1, b.1)
          other -> other
        }
      })
    let #(acc, _taken) =
      list.fold(sorted, #(acc, set.new()), fn(st, entry) {
        let #(acc, taken) = st
        let #(slot, js_name) = entry
        let key = #(frame, slot)
        case dict.has_key(acc, key) {
          // capture redeclares the origin binding; first name wins
          True -> #(acc, taken)
          False -> {
            let name = unique_name(ir_name(js_name), taken, 2)
            #(dict.insert(acc, key, name), set.insert(taken, name))
          }
        }
      })
    acc
  })
}

fn ir_name(js_name: String) -> String {
  case js_name {
    "#" <> rest -> "priv_" <> rest
    "_" <> _ -> "u" <> js_name
    _ -> js_name
  }
}

fn unique_name(base: String, taken: Set(String), n: Int) -> String {
  case set.contains(taken, base) {
    False -> base
    True -> {
      let cand = base <> "__" <> int_to_string(n)
      case set.contains(taken, cand) {
        False -> cand
        True -> unique_name(base, taken, n + 1)
      }
    }
  }
}

pub fn fresh_slot_var(e: Emitter2, slot: Int) -> #(String, Emitter2) {
  #(
    slot_var_name(e, slot) <> "_" <> int_to_string(e.next_var),
    Emitter2(..e, next_var: e.next_var + 1),
  )
}

pub fn cap_param_name(e: Emitter2, i: Int) -> String {
  case list_at(e.cap_names, i) {
    Some(name) -> name
    None -> "cap_" <> int_to_string(i)
  }
}

fn list_at(xs: List(a), i: Int) -> Option(a) {
  case xs, i {
    [], _ -> None
    [x, ..], 0 -> Some(x)
    [_, ..rest], n -> list_at(rest, n - 1)
  }
}

pub fn set_slot_var(e: Emitter2, slot: Int, name: String) -> Emitter2 {
  Emitter2(..e, slot_vars: dict.insert(e.slot_vars, slot, name))
}

pub fn set_hoisted_kfn(e: Emitter2, slot: Int, pair_var: ir.Value) -> Emitter2 {
  Emitter2(..e, hoisted_kfn: dict.insert(e.hoisted_kfn, slot, pair_var))
}

pub fn lookup_hoisted_kfn(e: Emitter2, slot: Int) -> Option(ir.Value) {
  case dict.get(e.hoisted_kfn, slot) {
    Ok(v) -> Some(v)
    Error(_) -> None
  }
}

pub fn clear_hoisted_kfn(e: Emitter2) -> Emitter2 {
  Emitter2(..e, hoisted_kfn: dict.new())
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(i: Int) -> String

pub fn push_frame(e: Emitter2, frame: Frame2) -> Emitter2 {
  Emitter2(..e, frame_stack: [frame, ..e.frame_stack], pending_label: None)
}

pub fn push_loop(
  e: Emitter2,
  ir_break: String,
  ir_continue: String,
  carried: List(Int),
  iter_close: Option(#(String, Escape)),
) -> Emitter2 {
  push_frame(
    e,
    Loop2(
      ir_break:,
      ir_continue:,
      js_label: e.pending_label,
      carried:,
      iter_close:,
    ),
  )
}

pub fn push_switch(
  e: Emitter2,
  ir_break: String,
  carried: List(Int),
) -> Emitter2 {
  push_frame(e, Switch2(ir_break:, js_label: e.pending_label, carried:))
}

pub fn push_labeled(
  e: Emitter2,
  ir_break: String,
  js_label: String,
  carried: List(Int),
) -> Emitter2 {
  push_frame(e, Labeled2(ir_break:, js_label:, carried:))
}

// not via push_frame: pending_label must survive a barrier
pub fn push_barrier(
  e: Emitter2,
  finally_body: Option(#(List(ast.StmtWithLine), ScopeSave2)),
  iter_close: Option(String),
  escape: Option(Escape),
) -> Emitter2 {
  Emitter2(..e, frame_stack: [
    Barrier2(finally_body:, iter_close:, escape:),
    ..e.frame_stack
  ])
}

pub fn fresh_escape(e: Emitter2, arity: Int) -> #(Escape, Emitter2) {
  let #(label, e) = fresh_label(e)
  #(Escape(label:, arity:), e)
}

fn fresh_vars(e: Emitter2, n: Int) -> #(List(String), Emitter2) {
  let #(e, names) = {
    use #(e, acc), _ <- list.fold(list.repeat(Nil, n), #(e, []))
    let #(v, e) = fresh_var(e)
    #(e, [v, ..acc])
  }
  #(list.reverse(names), e)
}

pub fn escape_handler(
  e: Emitter2,
  esc: Escape,
) -> #(ir.CatchHandler, Emitter2) {
  let #(x, e) = fresh_var(e)
  let dummies = list.repeat(e.consts.undef, esc.arity)
  #(
    ir.CatchHandler(
      on: ir.OnTag(e.consts.js_tag),
      payload: [x],
      exnref: None,
      handler: ir.Break(esc.label, [ir.ConstI32(1), ir.Var(x), ..dummies]),
    ),
    e,
  )
}

pub fn land_escapes(
  e: Emitter2,
  esc: Escape,
  region: ir.Expr,
) -> #(ir.Expr, Emitter2) {
  let #(code, e) = fresh_var(e)
  let #(exn, e) = fresh_var(e)
  let #(inner, e) = fresh_vars(e, esc.arity)
  let #(outer, e) = fresh_vars(e, esc.arity)
  let tys = list.repeat(ir.TTerm, esc.arity)
  let block =
    ir.Block(
      esc.label,
      [ir.TI32, ir.TTerm, ..tys],
      ir.Let(
        inner,
        region,
        ir.Values([ir.ConstI32(0), e.consts.undef, ..list.map(inner, ir.Var)]),
      ),
    )
  let tree =
    ir.Let(
      [code, exn, ..outer],
      block,
      ir.Let(
        [],
        ir.If(
          ir.Var(code),
          [],
          ir.Throw(e.consts.js_tag, [ir.Var(exn)]),
          ir.Values([]),
        ),
        ir.Values(list.map(outer, ir.Var)),
      ),
    )
  #(tree, e)
}

pub fn pop_frame(e: Emitter2) -> Emitter2 {
  let assert [_, ..rest] = e.frame_stack
  Emitter2(..e, frame_stack: rest)
}

pub fn set_pending_label(e: Emitter2, label: String) -> Emitter2 {
  Emitter2(..e, pending_label: Some(label))
}

fn break_target_of(frame: Frame2, name: Option(String)) -> Option(String) {
  case frame {
    Loop2(ir_break:, js_label:, ..) | Switch2(ir_break:, js_label:, ..) ->
      case name {
        None -> Some(ir_break)
        Some(_) ->
          case js_label == name {
            True -> Some(ir_break)
            False -> None
          }
      }
    Labeled2(ir_break:, js_label:, ..) ->
      // §14.8 unlabeled break skips a labeled block
      case name {
        Some(n) if n == js_label -> Some(ir_break)
        _ -> None
      }
    Barrier2(..) -> None
  }
}

fn continue_target_of(frame: Frame2, name: Option(String)) -> Option(String) {
  case frame {
    Loop2(ir_continue:, js_label:, ..) ->
      case name {
        None -> Some(ir_continue)
        Some(_) ->
          case js_label == name {
            True -> Some(ir_continue)
            False -> None
          }
      }
    Switch2(..) | Labeled2(..) | Barrier2(..) -> None
  }
}

fn cross_cleanups(frame: Frame2) -> List(BarrierCleanup) {
  case frame {
    Loop2(iter_close: Some(#(iv, esc)), ..) -> [IterClose(iv, False, Some(esc))]
    Loop2(..) | Switch2(..) | Labeled2(..) -> []
    Barrier2(finally_body:, iter_close:, escape:) -> {
      let acc = case finally_body {
        Some(#(body, save)) -> [FinallyBlock(body, save, escape)]
        None -> []
      }
      case iter_close {
        Some(iv) -> [IterClose(iv, False, escape), ..acc]
        None ->
          case acc {
            [] -> [CatchOnly]
            _ -> acc
          }
      }
    }
  }
}

fn find_target(
  frames: List(Frame2),
  name: Option(String),
  target_of: fn(Frame2, Option(String)) -> Option(String),
  miss: EmitError,
  crossed: List(BarrierCleanup),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  case frames {
    [] -> Error(miss)
    [frame, ..rest] ->
      case target_of(frame, name) {
        Some(label) -> Ok(#(label, list.reverse(crossed)))
        None -> {
          let crossed =
            list.fold(cross_cleanups(frame), crossed, fn(acc, c) { [c, ..acc] })
          find_target(rest, name, target_of, miss, crossed)
        }
      }
  }
}

pub fn find_break_target(
  e: Emitter2,
  name: Option(String),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  find_target(e.frame_stack, name, break_target_of, BreakOutsideLoop, [])
}

pub fn find_continue_target(
  e: Emitter2,
  name: Option(String),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  find_target(e.frame_stack, name, continue_target_of, ContinueOutsideLoop, [])
}

pub fn block_child_scopes(tree: ScopeTree, id: ScopeId) -> List(ScopeId) {
  use c <- list.filter(scope.child_scopes(tree, id))
  !scope.is_function_kind(scope.get_scope(tree, c).kind)
}

pub fn new_emitter(
  tree: ScopeTree,
  root: ScopeId,
  strict: Bool,
  module_name: String,
  dispatch: EmitDispatch,
) -> Emitter2 {
  Emitter2(
    tree:,
    fn_scope: root,
    cur_scope: root,
    scope_cursor: block_child_scopes(tree, root),
    child_fn_cursor: scope.child_function_scopes(tree, root),
    in_block: False,
    slot_names: slot_names(tree),
    cap_names: [],
    next_var: 0,
    next_label: 0,
    next_fn: 0,
    fn_names: set.new(),
    next_site: 0,
    module_name:,
    frame_stack: [],
    pending_label: None,
    fns_acc: [],
    unsupported: [],
    strict:,
    is_async: False,
    is_generator: False,
    is_arrow: False,
    with_stack: [],
    private_env: [],
    field_init: NoFieldInit,
    derived_ctor: False,
    default_ctor: False,
    this_tdz: False,
    slot_vars: dict.new(),
    initialized: set.new(),
    known_numbers: set.new(),
    known_strings: set.new(),
    hoisted_kfn: dict.new(),
    const_globals: dict.new(),
    slotted_globals: dict.new(),
    class_stack: [],
    sm_abrupt: None,
    raw_args_var: None,
    dispatch:,
    consts: realm_consts(),
  )
}

pub fn fn_info(e: Emitter2) -> scope.FunctionInfo {
  scope.function_info(e.tree, e.fn_scope)
}

pub fn lexical_is_boxed(
  e: Emitter2,
  info: scope.FunctionInfo,
  ref: lexical.LexicalRef,
) -> Bool {
  lexical.lexical_refs_get(info.lexical_boxed, ref)
  || { ref == lexical.RefThis && e.derived_ctor }
}

pub fn resolve(e: Emitter2, name: String) -> scope.Resolution {
  scope.lookup(e.tree, e.cur_scope, name)
}

pub fn arguments_is_implicit(e: Emitter2) -> Bool {
  case dict.get(scope.get_scope(e.tree, e.fn_scope).bindings, "arguments") {
    Ok(scope.Binding(slot: fs, kind: scope.VarBinding, ..)) ->
      case resolve(e, "arguments") {
        scope.Plain(scope.Local(slot:, kind: scope.VarBinding, ..)) ->
          slot == fs
        _ -> False
      }
    _ -> False
  }
}

pub fn pop_child_fn(e: Emitter2) -> #(ScopeId, Emitter2) {
  let assert [fn_id, ..rest] = e.child_fn_cursor
    as "emit_2core.pop_child_fn: cursor exhausted (analyzer/emit walk desync)"
  #(fn_id, Emitter2(..e, child_fn_cursor: rest))
}

// empty cursor stays put; never re-read consumed children
pub fn enter_scope(
  e: Emitter2,
  in_block in_block: Bool,
) -> #(Emitter2, ScopeSave2) {
  case e.scope_cursor {
    [child_id, ..parent_rest] -> {
      let save =
        ScopeSave2(
          cur_scope: e.cur_scope,
          scope_cursor: parent_rest,
          slot_vars: e.slot_vars,
          in_block: e.in_block,
        )
      let e =
        Emitter2(
          ..e,
          cur_scope: child_id,
          scope_cursor: block_child_scopes(e.tree, child_id),
          in_block:,
        )
      #(e, save)
    }
    [] -> {
      let save =
        ScopeSave2(
          cur_scope: e.cur_scope,
          scope_cursor: [],
          slot_vars: e.slot_vars,
          in_block: e.in_block,
        )
      #(Emitter2(..e, in_block:), save)
    }
  }
}

pub fn leave_scope(e: Emitter2, save: ScopeSave2) -> Emitter2 {
  Emitter2(
    ..e,
    cur_scope: save.cur_scope,
    scope_cursor: save.scope_cursor,
    slot_vars: save.slot_vars,
    in_block: save.in_block,
  )
}

pub fn leave_scope_if_inside(
  e: Emitter2,
  entered: ScopeId,
  save: ScopeSave2,
) -> Emitter2 {
  case entered != save.cur_scope && scope_within(e.tree, e.cur_scope, entered) {
    True -> leave_scope(e, save)
    False -> e
  }
}

fn scope_within(tree: ScopeTree, id: ScopeId, ancestor: ScopeId) -> Bool {
  case id == ancestor {
    True -> True
    False ->
      case scope.get_scope(tree, id).parent {
        Some(parent) -> scope_within(tree, parent, ancestor)
        None -> False
      }
  }
}

pub fn enter_for_scope(
  e: Emitter2,
  has_lex_head: Bool,
) -> #(Emitter2, Option(ScopeSave2)) {
  case has_lex_head {
    True -> {
      let #(e, save) = enter_scope(e, in_block: e.in_block)
      #(e, Some(save))
    }
    False -> #(e, None)
  }
}

pub fn leave_for_scope(e: Emitter2, save: Option(ScopeSave2)) -> Emitter2 {
  case save {
    Some(s) -> leave_scope(e, s)
    None -> e
  }
}

// counters and fns_acc are module-wide and not saved
pub fn enter_function(
  e: Emitter2,
  child_id: ScopeId,
  strict strict: Bool,
  is_async is_async: Bool,
  is_generator is_generator: Bool,
  is_arrow is_arrow: Bool,
) -> #(Emitter2, FnSave) {
  let save =
    FnSave(
      fn_scope: e.fn_scope,
      cur_scope: e.cur_scope,
      scope_cursor: e.scope_cursor,
      child_fn_cursor: e.child_fn_cursor,
      in_block: e.in_block,
      frame_stack: e.frame_stack,
      pending_label: e.pending_label,
      strict: e.strict,
      is_async: e.is_async,
      is_generator: e.is_generator,
      is_arrow: e.is_arrow,
      with_stack: e.with_stack,
      private_env: e.private_env,
      field_init: e.field_init,
      derived_ctor: e.derived_ctor,
      default_ctor: e.default_ctor,
      this_tdz: e.this_tdz,
      class_stack: e.class_stack,
      slot_vars: e.slot_vars,
      cap_names: e.cap_names,
      initialized: e.initialized,
      hoisted_kfn: e.hoisted_kfn,
      sm_abrupt: e.sm_abrupt,
      raw_args_var: e.raw_args_var,
    )
  let child =
    Emitter2(
      ..e,
      fn_scope: child_id,
      cur_scope: child_id,
      scope_cursor: block_child_scopes(e.tree, child_id),
      child_fn_cursor: scope.child_function_scopes(e.tree, child_id),
      in_block: False,
      frame_stack: [],
      pending_label: None,
      strict:,
      is_async:,
      is_generator:,
      is_arrow:,
      with_stack: [],
      private_env: e.private_env,
      field_init: NoFieldInit,
      derived_ctor: False,
      default_ctor: False,
      this_tdz: is_arrow && e.this_tdz,
      class_stack: e.class_stack,
      slot_vars: dict.new(),
      cap_names: [],
      initialized: set.new(),
      hoisted_kfn: dict.new(),
      sm_abrupt: None,
      raw_args_var: None,
    )
  #(child, save)
}

pub fn leave_function(e: Emitter2, save: FnSave) -> Emitter2 {
  Emitter2(
    ..e,
    fn_scope: save.fn_scope,
    cur_scope: save.cur_scope,
    scope_cursor: save.scope_cursor,
    child_fn_cursor: save.child_fn_cursor,
    in_block: save.in_block,
    frame_stack: save.frame_stack,
    pending_label: save.pending_label,
    strict: save.strict,
    is_async: save.is_async,
    is_generator: save.is_generator,
    is_arrow: save.is_arrow,
    with_stack: save.with_stack,
    private_env: save.private_env,
    field_init: save.field_init,
    derived_ctor: save.derived_ctor,
    default_ctor: save.default_ctor,
    this_tdz: save.this_tdz,
    class_stack: save.class_stack,
    slot_vars: save.slot_vars,
    cap_names: save.cap_names,
    initialized: save.initialized,
    hoisted_kfn: save.hoisted_kfn,
    sm_abrupt: save.sm_abrupt,
    raw_args_var: save.raw_args_var,
  )
}

pub fn set_sm_abrupt(e: Emitter2, hooks: SmAbrupt) -> Emitter2 {
  Emitter2(..e, sm_abrupt: Some(hooks))
}

pub fn clear_sm_abrupt(e: Emitter2) -> Emitter2 {
  Emitter2(..e, sm_abrupt: None)
}
