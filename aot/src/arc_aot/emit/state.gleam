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

pub type IrConsts {
  IrConsts(
    undef: ir.Value,
    null: ir.Value,
    true_: ir.Value,
    false_: ir.Value,
    nan: ir.Value,
    pos_inf: ir.Value,
    neg_inf: ir.Value,
    tdz: ir.Value,
    empty_bin: ir.Value,
    exn_tag: String,
  )
}

fn ir_consts() -> IrConsts {
  IrConsts(
    undef: ir.ConstAtom("undefined"),
    null: ir.ConstAtom("null"),
    true_: ir.ConstAtom("true"),
    false_: ir.ConstAtom("false"),
    nan: ir.ConstAtom("js_nan"),
    pos_inf: ir.ConstAtom("js_inf"),
    neg_inf: ir.ConstAtom("js_neg_inf"),
    tdz: ir.ConstAtom("js_tdz"),
    empty_bin: ir.ConstBinary(<<>>),
    exn_tag: "js_exn",
  )
}

pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  EarlySyntaxError(message: String)
  UnsupportedFeature(feature: String)
  ScopeCursorDesync(at: ScopeId)
}

pub fn describe_error(err: EmitError) -> String {
  case err {
    BreakOutsideLoop -> "break outside loop"
    ContinueOutsideLoop -> "continue outside loop"
    EarlySyntaxError(message:) -> message
    UnsupportedFeature(feature:) -> "unsupported: " <> feature
    ScopeCursorDesync(..) -> "scope cursor desync"
  }
}

pub type FieldInitMode {
  NoFieldInit
  FieldInitAfterSuper
}

pub type ClassContext {
  ClassContext(
    brand_vars: Dict(String, ir.Value),
    proto_home_box: ir.Value,
    static_home_box: ir.Value,
    ctor_self_box: ir.Value,
    inner_name_box: Option(ir.Value),
    is_derived: Bool,
  )
}

// a loop-invariant callee whose direct entry is loaded before the loop
pub type InvariantCallee {
  PlainSlot(slot: Int)
  BoxedSlot(slot: Int)
}

pub type ScopeSnapshot {
  ScopeSnapshot(
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
    frame_stack: List(Frame),
    pending_label: Option(String),
    strict: Bool,
    is_async: Bool,
    private_env: List(String),
    field_init: FieldInitMode,
    derived_ctor: Bool,
    default_ctor: Bool,
    this_tdz: Bool,
    class_stack: List(ClassContext),
    slot_vars: Dict(Int, String),
    cap_names: List(String),
    initialized_slots: Set(Int),
    invariant_callees: Dict(InvariantCallee, ir.Value),
    machine_abrupt: Option(MachineAbrupt),
    raw_args_var: Option(String),
  )
}

pub type Frame {
  LoopFrame(
    ir_break: String,
    ir_continue: String,
    js_label: Option(String),
    carried: List(Int),
    iter_close: Option(#(String, Escape)),
  )
  SwitchFrame(ir_break: String, js_label: Option(String), carried: List(Int))
  LabeledBlockFrame(ir_break: String, js_label: String, carried: List(Int))
  BarrierFrame(
    finally_body: Option(#(List(ast.StmtWithLine), ScopeSnapshot)),
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
    saved_scope: ScopeSnapshot,
    escape: Option(Escape),
  )
  IterClose(iter_var: String, escape: Option(Escape))
  CatchOnly
}

pub type EmitResult =
  Result(#(ir.Expr, Emitter), EmitError)

pub type Next =
  fn(Emitter) -> EmitResult

pub type NextWith(a) =
  fn(a, Emitter) -> EmitResult

pub fn map_tree(r: EmitResult, f: fn(ir.Expr) -> ir.Expr) -> EmitResult {
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
  FnDecl(is_generator: Bool, is_async: Bool)
  FnExpr(self_name: Option(String), is_generator: Bool, is_async: Bool)
  Arrow(is_async: Bool)
  Method(is_generator: Bool, is_async: Bool)
  ClassCtor(is_derived: Bool, has_field_init: Bool, default: Bool)
}

pub type FnBody {
  StmtBody(List(ast.StmtWithLine))
  ExprBody(ast.Expression)
}

pub type EmittedFn {
  DirectCallable(
    name: String,
    captures: List(ir.Value),
    arity: Int,
    takes_this: Bool,
    strict: Bool,
  )
  ClosureExpr(ir.Expr)
}

pub type CoroutineKind {
  Generator
  AsyncFunction
  AsyncGenerator
}

pub type MachineAbrupt {
  MachineAbrupt(
    on_return: NextWith(ir.Value),
    on_goto: fn(Emitter, String) -> Option(EmitResult),
  )
}

pub type EmitDispatch {
  EmitDispatch(
    emit_expr: fn(Emitter, ast.Expression) -> EmitResult,
    emit_expr_named: fn(Emitter, ast.Expression, Option(String)) -> EmitResult,
    emit_stmts: fn(Emitter, List(ast.StmtWithLine), Next) -> EmitResult,
    emit_function: fn(
      Emitter,
      FnShape,
      Option(String),
      List(ast.Pattern),
      FnBody,
      ScopeId,
    ) -> EmitResult,
    emit_function_callable: fn(
      Emitter,
      FnShape,
      Option(String),
      List(ast.Pattern),
      FnBody,
      ScopeId,
    ) -> Result(#(EmittedFn, Emitter), EmitError),
    emit_class: fn(
      Emitter,
      Option(String),
      Option(String),
      Option(ast.Expression),
      List(ast.ClassElement),
    ) -> EmitResult,
    emit_coroutine_fn: fn(
      Emitter,
      FnShape,
      Option(String),
      List(ast.Pattern),
      FnBody,
      ScopeId,
      List(ir.Value),
    ) -> EmitResult,
    emit_destructure: fn(Emitter, ast.Pattern, ir.Value, BindMode) -> EmitResult,
  )
}

pub type Emitter {
  Emitter(
    scope_tree: ScopeTree,
    fn_scope: ScopeId,
    cur_scope: ScopeId,
    scope_cursor: List(ScopeId),
    child_fn_cursor: List(ScopeId),
    in_block: Bool,
    slot_base_names: Dict(#(ScopeId, Int), String),
    cap_names: List(String),
    next_var: Int,
    next_label: Int,
    next_fn: Int,
    fn_names: Set(String),
    next_ic_site: Int,
    module_name: String,
    frame_stack: List(Frame),
    pending_label: Option(String),
    fns_acc: List(ir.Function),
    unsupported: List(String),
    strict: Bool,
    is_async: Bool,
    private_env: List(String),
    field_init: FieldInitMode,
    derived_ctor: Bool,
    default_ctor: Bool,
    this_tdz: Bool,
    // current ssa version per slot
    slot_vars: Dict(Int, String),
    initialized_slots: Set(Int),
    known_numbers: Set(String),
    known_strings: Set(String),
    invariant_callees: Dict(InvariantCallee, ir.Value),
    const_globals: Dict(String, ir.Value),
    slotted_globals: Dict(String, Int),
    class_stack: List(ClassContext),
    machine_abrupt: Option(MachineAbrupt),
    raw_args_var: Option(String),
    dispatch: EmitDispatch,
    consts: IrConsts,
  )
}

// never cleared; var names are module-unique
pub fn mark_known_number(e: Emitter, name: String) -> Emitter {
  Emitter(..e, known_numbers: set.insert(e.known_numbers, name))
}

pub fn is_known_number(e: Emitter, name: String) -> Bool {
  set.contains(e.known_numbers, name)
}

pub fn mark_known_string(e: Emitter, name: String) -> Emitter {
  Emitter(..e, known_strings: set.insert(e.known_strings, name))
}

pub fn is_known_string(e: Emitter, name: String) -> Bool {
  set.contains(e.known_strings, name)
}

pub fn set_const_globals(e: Emitter, d: Dict(String, ir.Value)) -> Emitter {
  Emitter(..e, const_globals: d)
}

pub fn set_slotted_globals(e: Emitter, d: Dict(String, Int)) -> Emitter {
  Emitter(..e, slotted_globals: d)
}

pub fn lookup_slotted_global(e: Emitter, name: String) -> Option(Int) {
  option.from_result(dict.get(e.slotted_globals, name))
}

pub fn fresh_var(e: Emitter) -> #(String, Emitter) {
  #("_t" <> int.to_string(e.next_var), Emitter(..e, next_var: e.next_var + 1))
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
pub fn let_(e: Emitter, rhs: ir.Expr, k: NextWith(ir.Value)) -> EmitResult {
  case rhs {
    ir.Let(names, inner_rhs, inner_body) -> {
      use tail <- map_tree(let_(e, inner_body, k))
      ir.Let(names, inner_rhs, tail)
    }
    ir.Values([v]) -> k(v, e)
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
      use body <- map_tree(k(ir.Var(n), e))
      ir.Let([n], rhs, body)
    }
  }
}

pub fn fresh_label(e: Emitter) -> #(String, Emitter) {
  #(
    "_L" <> int.to_string(e.next_label),
    Emitter(..e, next_label: e.next_label + 1),
  )
}

pub fn fresh_fn_name(
  e: Emitter,
  js_name: Option(String),
) -> #(String, Emitter) {
  let base = case option.then(js_name, fn_base) {
    Some(name) -> name
    None -> "fn_" <> int.to_string(e.next_fn)
  }
  let name = unique_fn_name(base, e.fn_names, 2)
  #(
    name,
    Emitter(..e, next_fn: e.next_fn + 1, fn_names: set.insert(e.fn_names, name)),
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
      let cand = base <> "_" <> int.to_string(n)
      case fn_name_free(cand, taken) {
        True -> cand
        False -> unique_fn_name(base, taken, n + 1)
      }
    }
  }
}

fn fn_name_free(cand: String, taken: Set(String)) -> Bool {
  !set.contains(taken, cand)
  && !set.contains(taken, cand <> "_direct")
  && !set.contains(taken, cand <> "_direct_this")
  && !set.contains(taken, cand <> "__sm")
  && list.all(["_direct", "_direct_this", "__sm"], fn(suffix) {
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
  case list.reverse(string.split(s, "_c")) {
    [last, _, ..] ->
      case int.parse(last) {
        Ok(_) -> Some(string.drop_end(s, string.length(last) + 2))
        Error(Nil) -> None
      }
    _ -> None
  }
}

pub fn add_function(e: Emitter, f: ir.Function) -> Emitter {
  let fs = list.reverse(split.function(f))
  Emitter(..e, fns_acc: list.append(fs, e.fns_acc))
}

pub fn take_functions(e: Emitter) -> List(ir.Function) {
  list.reverse(e.fns_acc)
}

pub fn mark_unsupported(e: Emitter, feature: String) -> Emitter {
  Emitter(..e, unsupported: [feature, ..e.unsupported])
}

pub fn slot_base_name(e: Emitter, slot: Int) -> String {
  case dict.get(e.slot_base_names, #(e.fn_scope, slot)) {
    Ok(name) -> name
    Error(Nil) -> "js_local_" <> int.to_string(slot)
  }
}

pub fn get_slot_var(e: Emitter, slot: Int) -> String {
  case dict.get(e.slot_vars, slot) {
    Ok(name) -> name
    Error(Nil) -> slot_base_name(e, slot)
  }
}

fn slot_base_names(tree: ScopeTree) -> Dict(#(ScopeId, Int), String) {
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
      list.fold(sorted, #(acc, set.new()), fn(names, entry) {
        let #(acc, taken) = names
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
      let cand = base <> "__" <> int.to_string(n)
      case set.contains(taken, cand) {
        False -> cand
        True -> unique_name(base, taken, n + 1)
      }
    }
  }
}

pub fn fresh_slot_var(e: Emitter, slot: Int) -> #(String, Emitter) {
  #(
    slot_base_name(e, slot) <> "_" <> int.to_string(e.next_var),
    Emitter(..e, next_var: e.next_var + 1),
  )
}

pub fn cap_param_name(e: Emitter, i: Int) -> String {
  list_at(e.cap_names, i) |> option.unwrap("cap_" <> int.to_string(i))
}

fn list_at(xs: List(a), i: Int) -> Option(a) {
  case xs, i {
    [], _ -> None
    [x, ..], 0 -> Some(x)
    [_, ..rest], n -> list_at(rest, n - 1)
  }
}

pub fn set_slot_var(e: Emitter, slot: Int, name: String) -> Emitter {
  Emitter(..e, slot_vars: dict.insert(e.slot_vars, slot, name))
}

pub fn set_invariant_callee(
  e: Emitter,
  callee: InvariantCallee,
  direct_callee: ir.Value,
) -> Emitter {
  Emitter(
    ..e,
    invariant_callees: dict.insert(e.invariant_callees, callee, direct_callee),
  )
}

pub fn lookup_invariant_callee(
  e: Emitter,
  callee: InvariantCallee,
) -> Option(ir.Value) {
  option.from_result(dict.get(e.invariant_callees, callee))
}

// one ic map per agent, so each module numbers sites from a name-derived base
fn site_base(module_name: String) -> Int {
  phash2(module_name, 1_073_741_824) * 16_777_216
}

@external(erlang, "erlang", "phash2")
fn phash2(term: String, range: Int) -> Int

pub fn push_frame(e: Emitter, frame: Frame) -> Emitter {
  Emitter(..e, frame_stack: [frame, ..e.frame_stack], pending_label: None)
}

pub fn push_loop(
  e: Emitter,
  ir_break: String,
  ir_continue: String,
  carried: List(Int),
  iter_close: Option(#(String, Escape)),
) -> Emitter {
  push_frame(
    e,
    LoopFrame(
      ir_break:,
      ir_continue:,
      js_label: e.pending_label,
      carried:,
      iter_close:,
    ),
  )
}

pub fn push_switch(
  e: Emitter,
  ir_break: String,
  carried: List(Int),
) -> Emitter {
  push_frame(e, SwitchFrame(ir_break:, js_label: e.pending_label, carried:))
}

pub fn push_labeled(
  e: Emitter,
  ir_break: String,
  js_label: String,
  carried: List(Int),
) -> Emitter {
  push_frame(e, LabeledBlockFrame(ir_break:, js_label:, carried:))
}

// not via push_frame: pending_label must survive a barrier
pub fn push_barrier(
  e: Emitter,
  finally_body: Option(#(List(ast.StmtWithLine), ScopeSnapshot)),
  iter_close: Option(String),
  escape: Option(Escape),
) -> Emitter {
  Emitter(..e, frame_stack: [
    BarrierFrame(finally_body:, iter_close:, escape:),
    ..e.frame_stack
  ])
}

pub fn fresh_escape(e: Emitter, arity: Int) -> #(Escape, Emitter) {
  let #(label, e) = fresh_label(e)
  #(Escape(label:, arity:), e)
}

fn fresh_vars(e: Emitter, n: Int) -> #(List(String), Emitter) {
  let #(names, e) = {
    use #(acc, e), _ <- list.fold(list.repeat(Nil, n), #([], e))
    let #(v, e) = fresh_var(e)
    #([v, ..acc], e)
  }
  #(list.reverse(names), e)
}

pub fn escape_handler(e: Emitter, esc: Escape) -> #(ir.CatchHandler, Emitter) {
  let #(x, e) = fresh_var(e)
  let dummies = list.repeat(e.consts.undef, esc.arity)
  #(
    ir.CatchHandler(
      on: ir.OnTag(e.consts.exn_tag),
      payload: [x],
      exnref: None,
      handler: ir.Break(esc.label, [ir.ConstI32(1), ir.Var(x), ..dummies]),
    ),
    e,
  )
}

pub fn land_escapes(
  e: Emitter,
  esc: Escape,
  region: ir.Expr,
) -> #(ir.Expr, Emitter) {
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
          ir.Throw(e.consts.exn_tag, [ir.Var(exn)]),
          ir.Values([]),
        ),
        ir.Values(list.map(outer, ir.Var)),
      ),
    )
  #(tree, e)
}

pub fn pop_frame(e: Emitter) -> Emitter {
  let assert [_, ..rest] = e.frame_stack
  Emitter(..e, frame_stack: rest)
}

pub fn set_pending_label(e: Emitter, label: String) -> Emitter {
  Emitter(..e, pending_label: Some(label))
}

fn break_target_of(frame: Frame, name: Option(String)) -> Option(String) {
  case frame {
    LoopFrame(ir_break:, js_label:, ..)
    | SwitchFrame(ir_break:, js_label:, ..) ->
      case name {
        None -> Some(ir_break)
        Some(_) ->
          case js_label == name {
            True -> Some(ir_break)
            False -> None
          }
      }
    LabeledBlockFrame(ir_break:, js_label:, ..) ->
      // §14.8 unlabeled break skips a labeled block
      case name {
        Some(n) if n == js_label -> Some(ir_break)
        _ -> None
      }
    BarrierFrame(..) -> None
  }
}

fn continue_target_of(frame: Frame, name: Option(String)) -> Option(String) {
  case frame {
    LoopFrame(ir_continue:, js_label:, ..) ->
      case name {
        None -> Some(ir_continue)
        Some(_) ->
          case js_label == name {
            True -> Some(ir_continue)
            False -> None
          }
      }
    SwitchFrame(..) | LabeledBlockFrame(..) | BarrierFrame(..) -> None
  }
}

pub fn cross_cleanups(frame: Frame) -> List(BarrierCleanup) {
  case frame {
    LoopFrame(iter_close: Some(#(iv, esc)), ..) -> [
      IterClose(iv, Some(esc)),
    ]
    LoopFrame(..) | SwitchFrame(..) | LabeledBlockFrame(..) -> []
    BarrierFrame(finally_body:, iter_close:, escape:) -> {
      let acc = case finally_body {
        Some(#(body, save)) -> [FinallyBlock(body, save, escape)]
        None -> []
      }
      case iter_close {
        Some(iv) -> [IterClose(iv, escape), ..acc]
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
  frames: List(Frame),
  name: Option(String),
  target_of: fn(Frame, Option(String)) -> Option(String),
  not_found: EmitError,
  crossed: List(BarrierCleanup),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  case frames {
    [] -> Error(not_found)
    [frame, ..rest] ->
      case target_of(frame, name) {
        Some(label) -> Ok(#(label, list.reverse(crossed)))
        None -> {
          let crossed =
            list.fold(cross_cleanups(frame), crossed, fn(acc, c) { [c, ..acc] })
          find_target(rest, name, target_of, not_found, crossed)
        }
      }
  }
}

pub fn find_break_target(
  e: Emitter,
  name: Option(String),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  find_target(e.frame_stack, name, break_target_of, BreakOutsideLoop, [])
}

pub fn find_continue_target(
  e: Emitter,
  name: Option(String),
) -> Result(#(String, List(BarrierCleanup)), EmitError) {
  find_target(e.frame_stack, name, continue_target_of, ContinueOutsideLoop, [])
}

pub fn block_child_scopes(tree: ScopeTree, id: ScopeId) -> List(ScopeId) {
  use c <- list.filter(scope.child_scopes(tree, id))
  !scope.is_function_kind(scope.get(tree, c).kind)
}

pub fn new_emitter(
  tree: ScopeTree,
  root: ScopeId,
  strict strict: Bool,
  module_name module_name: String,
  dispatch dispatch: EmitDispatch,
) -> Emitter {
  Emitter(
    scope_tree: tree,
    fn_scope: root,
    cur_scope: root,
    scope_cursor: block_child_scopes(tree, root),
    child_fn_cursor: scope.child_function_scopes(tree, root),
    in_block: False,
    slot_base_names: slot_base_names(tree),
    cap_names: [],
    next_var: 0,
    next_label: 0,
    next_fn: 0,
    fn_names: set.new(),
    next_ic_site: site_base(module_name),
    module_name:,
    frame_stack: [],
    pending_label: None,
    fns_acc: [],
    unsupported: [],
    strict:,
    is_async: False,
    private_env: [],
    field_init: NoFieldInit,
    derived_ctor: False,
    default_ctor: False,
    this_tdz: False,
    slot_vars: dict.new(),
    initialized_slots: set.new(),
    known_numbers: set.new(),
    known_strings: set.new(),
    invariant_callees: dict.new(),
    const_globals: dict.new(),
    slotted_globals: dict.new(),
    class_stack: [],
    machine_abrupt: None,
    raw_args_var: None,
    dispatch:,
    consts: ir_consts(),
  )
}

pub fn fn_info(e: Emitter) -> scope.FunctionInfo {
  scope.function_info(e.scope_tree, e.fn_scope)
}

pub fn lexical_is_boxed(
  e: Emitter,
  info: scope.FunctionInfo,
  ref: lexical.LexicalRef,
) -> Bool {
  lexical.refs_get(info.lexical_boxed, ref)
  || { ref == lexical.ThisRef && e.derived_ctor }
}

pub fn resolve(e: Emitter, name: String) -> scope.Resolution {
  scope.lookup(e.scope_tree, e.cur_scope, name)
}

pub fn arguments_is_implicit(e: Emitter) -> Bool {
  case dict.get(scope.get(e.scope_tree, e.fn_scope).bindings, "arguments") {
    Ok(scope.Binding(slot: fs, kind: scope.VarBinding, ..)) ->
      case resolve(e, "arguments") {
        scope.Plain(scope.Local(slot:, kind: scope.VarBinding, ..)) ->
          slot == fs
        _ -> False
      }
    _ -> False
  }
}

pub fn pop_child_fn(e: Emitter) -> #(ScopeId, Emitter) {
  let assert [fn_id, ..rest] = e.child_fn_cursor
    as "aot/state: child fn cursor exhausted (analyzer/emit walk desync)"
  #(fn_id, Emitter(..e, child_fn_cursor: rest))
}

// empty cursor stays put; never re-read consumed children
pub fn enter_scope(
  e: Emitter,
  in_block in_block: Bool,
) -> #(ScopeSnapshot, Emitter) {
  case e.scope_cursor {
    [child_id, ..parent_rest] -> {
      let save =
        ScopeSnapshot(
          cur_scope: e.cur_scope,
          scope_cursor: parent_rest,
          slot_vars: e.slot_vars,
          in_block: e.in_block,
        )
      let e =
        Emitter(
          ..e,
          cur_scope: child_id,
          scope_cursor: block_child_scopes(e.scope_tree, child_id),
          in_block:,
        )
      #(save, e)
    }
    [] -> {
      let save =
        ScopeSnapshot(
          cur_scope: e.cur_scope,
          scope_cursor: [],
          slot_vars: e.slot_vars,
          in_block: e.in_block,
        )
      #(save, Emitter(..e, in_block:))
    }
  }
}

pub fn leave_scope(e: Emitter, save: ScopeSnapshot) -> Emitter {
  Emitter(
    ..e,
    cur_scope: save.cur_scope,
    scope_cursor: save.scope_cursor,
    slot_vars: save.slot_vars,
    in_block: save.in_block,
  )
}

pub fn leave_scope_if_inside(
  e: Emitter,
  entered: ScopeId,
  save: ScopeSnapshot,
) -> Emitter {
  case
    entered != save.cur_scope
    && scope_within(e.scope_tree, e.cur_scope, entered)
  {
    True -> leave_scope(e, save)
    False -> e
  }
}

fn scope_within(tree: ScopeTree, id: ScopeId, ancestor: ScopeId) -> Bool {
  case id == ancestor {
    True -> True
    False ->
      case scope.get(tree, id).parent {
        Some(parent) -> scope_within(tree, parent, ancestor)
        None -> False
      }
  }
}

pub fn enter_for_scope(
  e: Emitter,
  has_lex_head has_lex_head: Bool,
) -> #(Option(ScopeSnapshot), Emitter) {
  case has_lex_head {
    True -> {
      let #(save, e) = enter_scope(e, in_block: e.in_block)
      #(Some(save), e)
    }
    False -> #(None, e)
  }
}

pub fn leave_for_scope(e: Emitter, save: Option(ScopeSnapshot)) -> Emitter {
  case save {
    Some(s) -> leave_scope(e, s)
    None -> e
  }
}

// counters and fns_acc are module-wide and not saved
pub fn enter_function(
  e: Emitter,
  child_id: ScopeId,
  strict strict: Bool,
  is_async is_async: Bool,
  is_arrow is_arrow: Bool,
) -> #(FnSave, Emitter) {
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
      private_env: e.private_env,
      field_init: e.field_init,
      derived_ctor: e.derived_ctor,
      default_ctor: e.default_ctor,
      this_tdz: e.this_tdz,
      class_stack: e.class_stack,
      slot_vars: e.slot_vars,
      cap_names: e.cap_names,
      initialized_slots: e.initialized_slots,
      invariant_callees: e.invariant_callees,
      machine_abrupt: e.machine_abrupt,
      raw_args_var: e.raw_args_var,
    )
  let child =
    Emitter(
      ..e,
      fn_scope: child_id,
      cur_scope: child_id,
      scope_cursor: block_child_scopes(e.scope_tree, child_id),
      child_fn_cursor: scope.child_function_scopes(e.scope_tree, child_id),
      in_block: False,
      frame_stack: [],
      pending_label: None,
      strict:,
      is_async:,
      private_env: e.private_env,
      field_init: NoFieldInit,
      derived_ctor: False,
      default_ctor: False,
      this_tdz: is_arrow && e.this_tdz,
      class_stack: e.class_stack,
      slot_vars: dict.new(),
      cap_names: [],
      initialized_slots: set.new(),
      invariant_callees: dict.new(),
      machine_abrupt: None,
      raw_args_var: None,
    )
  #(save, child)
}

pub fn leave_function(e: Emitter, save: FnSave) -> Emitter {
  Emitter(
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
    private_env: save.private_env,
    field_init: save.field_init,
    derived_ctor: save.derived_ctor,
    default_ctor: save.default_ctor,
    this_tdz: save.this_tdz,
    class_stack: save.class_stack,
    slot_vars: save.slot_vars,
    cap_names: save.cap_names,
    initialized_slots: save.initialized_slots,
    invariant_callees: save.invariant_callees,
    machine_abrupt: save.machine_abrupt,
    raw_args_var: save.raw_args_var,
  )
}

pub fn set_machine_abrupt(e: Emitter, hooks: MachineAbrupt) -> Emitter {
  Emitter(..e, machine_abrupt: Some(hooks))
}

pub fn clear_machine_abrupt(e: Emitter) -> Emitter {
  Emitter(..e, machine_abrupt: None)
}
