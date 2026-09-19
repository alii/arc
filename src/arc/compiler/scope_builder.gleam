import arc/bytecode/lexical.{
  type LexicalRef, type LexicalRefs, ActiveFuncRef, HomeObjectRef, NewTargetRef,
  ThisRef,
}
import arc/compiler/scope.{
  type BindingKind, type ScopeId, type ScopeKind, Block, CaptureBinding, Catch,
  CatchBinding, ClassBody, ClassStaticBlock, ConstBinding, FnNameBinding,
  Function, LetBinding, Module, ParamBinding, Script, VarBinding, With,
  is_function_kind, param_shim, root_scope_id, with_object_name,
}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub type RawBinding {
  RawBinding(kind: BindingKind, synthetic: Bool, decl_order: Int)
}

pub type SourceTag {
  FnDeclSource
  SwitchTestSource
  OtherSource
}

pub type RawScope {
  RawScope(
    id: ScopeId,
    parent: Option(ScopeId),
    function_scope: ScopeId,
    kind: ScopeKind,
    bindings: Dict(String, RawBinding),
    next_decl_order: Int,
    contains_direct_eval: Bool,
    annexb_blocked: Set(String),
    is_strict: Bool,
    catch_param_simple: Bool,
    source_tag: SourceTag,
    // vars hoisting through here; early errors only, not bindings
    hoisted_vars: Set(String),
    // <paramN> shim count for a non-simple parameter list
    non_simple_shim_count: Int,
    // §10.2.11 step 28 var sink body block for non-simple params
    is_var_boundary: Bool,
  )
}

pub type RawFunctionInfo {
  RawFunctionInfo(
    is_arrow: Bool,
    is_derived_constructor: Bool,
    annexb_candidates: List(#(ScopeId, String)),
  )
}

const blank_raw_fn_info = RawFunctionInfo(
  is_arrow: False,
  is_derived_constructor: False,
  annexb_candidates: [],
)

fn new_raw_scope(
  id: ScopeId,
  parent: Option(ScopeId),
  function_scope: ScopeId,
  kind: ScopeKind,
  is_strict is_strict: Bool,
) -> RawScope {
  RawScope(
    id:,
    parent:,
    function_scope:,
    kind:,
    bindings: dict.new(),
    next_decl_order: 0,
    contains_direct_eval: False,
    annexb_blocked: set.new(),
    is_strict:,
    catch_param_simple: True,
    source_tag: OtherSource,
    hoisted_vars: set.new(),
    non_simple_shim_count: 0,
    is_var_boundary: False,
  )
}

pub type NameRef {
  NameRef(scope: ScopeId, name: String)
}

// order_stamp is next_id at the write, so later scopes compare greater
pub type AssignRef {
  AssignRef(scope: ScopeId, name: String, order_stamp: ScopeId)
}

pub type TryAssignRef {
  TryAssignRef(scope: ScopeId, name: String, try_scope: ScopeId)
}

pub type ScopeBuilder {
  ScopeBuilder(
    scopes: Dict(ScopeId, RawScope),
    functions: Dict(ScopeId, RawFunctionInfo),
    // newest first while open, then emit's consumption order, not source order
    children_at: Dict(ScopeId, List(ScopeId)),
    next_id: Int,
    current: ScopeId,
    current_fn: ScopeId,
    raw_refs: List(NameRef),
    assign_refs: List(AssignRef),
    try_scopes: List(ScopeId),
    try_assign_refs: List(TryAssignRef),
    own_lexical_refs: Dict(ScopeId, LexicalRefs),
  )
}

pub fn init(root_kind: ScopeKind, strict strict: Bool) -> ScopeBuilder {
  let root =
    new_raw_scope(
      root_scope_id,
      None,
      root_scope_id,
      root_kind,
      strict || root_kind == Module,
    )
  ScopeBuilder(
    scopes: dict.from_list([#(root_scope_id, root)]),
    functions: dict.from_list([#(root_scope_id, blank_raw_fn_info)]),
    children_at: dict.from_list([#(root_scope_id, [])]),
    next_id: 1,
    current: root_scope_id,
    current_fn: root_scope_id,
    raw_refs: [],
    assign_refs: [],
    try_scopes: [],
    try_assign_refs: [],
    own_lexical_refs: dict.new(),
  )
}

pub fn scope_at(sb: ScopeBuilder, id: ScopeId) -> RawScope {
  let assert Ok(s) = dict.get(sb.scopes, id)
    as "scope_builder.scope_at: unknown ScopeId"
  s
}

pub fn fn_info_at(sb: ScopeBuilder, fn_id: ScopeId) -> RawFunctionInfo {
  let assert Ok(info) = dict.get(sb.functions, fn_id)
    as "scope_builder.fn_info_at: unknown function scope"
  info
}

pub fn push(sb: ScopeBuilder, kind: ScopeKind) -> #(ScopeBuilder, ScopeId) {
  let id = sb.next_id
  let parent = scope_at(sb, sb.current)
  let is_fn = is_function_kind(kind)
  let function_scope = case is_fn {
    True -> id
    False -> parent.function_scope
  }
  let is_strict = case kind {
    Module | ClassBody | ClassStaticBlock -> True
    Script | Function | Block | Catch | With(_) -> parent.is_strict
  }
  let node =
    new_raw_scope(id, Some(sb.current), function_scope, kind, is_strict)
  let functions = case is_fn {
    False -> sb.functions
    True -> dict.insert(sb.functions, id, blank_raw_fn_info)
  }
  let children_at =
    dict.upsert(sb.children_at, sb.current, fn(prev) {
      [id, ..option.unwrap(prev, [])]
    })
    |> dict.insert(id, [])
  #(
    ScopeBuilder(
      ..sb,
      scopes: dict.insert(sb.scopes, id, node),
      functions:,
      children_at:,
      next_id: id + 1,
      current: id,
      current_fn: function_scope,
    ),
    id,
  )
}

// §14.11 the only way to build a with scope; declares its holder
pub fn push_with(sb: ScopeBuilder) -> #(ScopeBuilder, ScopeId) {
  let holder = with_object_name(with_depth(sb), sb.next_id)
  let #(sb, id) = push(sb, With(holder:))
  #(declare(sb, holder, LetBinding, synthetic: True), id)
}

// §10.2.11 step 28 body scope for a non-simple parameter list
pub fn push_var_boundary(sb: ScopeBuilder) -> #(ScopeBuilder, ScopeId) {
  let #(sb, id) = push(sb, Block)
  let scope = scope_at(sb, id)
  #(
    ScopeBuilder(
      ..sb,
      scopes: dict.insert(
        sb.scopes,
        id,
        RawScope(..scope, is_var_boundary: True),
      ),
    ),
    id,
  )
}

// first declaration wins
pub fn declare(
  sb: ScopeBuilder,
  name: String,
  kind: BindingKind,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let target_id = case kind {
    VarBinding -> var_target(sb)
    LetBinding
    | ConstBinding
    | ParamBinding
    | CatchBinding
    | CaptureBinding
    | FnNameBinding -> sb.current
  }
  declare_in(sb, target_id, name, kind, synthetic:)
}

fn var_target(sb: ScopeBuilder) -> ScopeId {
  use id, scope, acc <- fold_up(
    sb,
    from: sb.current,
    stop_at_fn: True,
    init: sb.current_fn,
  )
  case is_function_kind(scope.kind) || scope.is_var_boundary {
    True -> list.Stop(id)
    False -> list.Continue(acc)
  }
}

pub fn declare_var(
  sb: ScopeBuilder,
  name: String,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let sb = mark_hoisted_var(sb, sb.current, name)
  declare(sb, name, VarBinding, synthetic:)
}

fn fold_up(
  sb: ScopeBuilder,
  from at: ScopeId,
  stop_at_fn stop_at_fn: Bool,
  init acc: a,
  step step: fn(ScopeId, RawScope, a) -> list.ContinueOrStop(a),
) -> a {
  let scope = scope_at(sb, at)
  case step(at, scope, acc) {
    list.Stop(acc) -> acc
    list.Continue(acc) ->
      case stop_at_fn && at == sb.current_fn, scope.parent {
        False, Some(pid) -> fold_up(sb, pid, stop_at_fn, acc, step)
        _, _ -> acc
      }
  }
}

fn mark_hoisted_var(
  sb: ScopeBuilder,
  at: ScopeId,
  name: String,
) -> ScopeBuilder {
  use id, scope, sb <- fold_up(sb, from: at, stop_at_fn: True, init: sb)
  let updated =
    ScopeBuilder(
      ..sb,
      scopes: dict.insert(
        sb.scopes,
        id,
        RawScope(..scope, hoisted_vars: set.insert(scope.hoisted_vars, name)),
      ),
    )
  case is_function_kind(scope.kind) || scope.is_var_boundary {
    True -> list.Stop(updated)
    False -> list.Continue(updated)
  }
}

pub fn ref(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  ScopeBuilder(..sb, raw_refs: [NameRef(sb.current, name), ..sb.raw_refs])
}

pub fn assign_ref(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  let try_assign_refs = case sb.try_scopes {
    [try_scope, ..] -> [
      TryAssignRef(scope: sb.current, name:, try_scope:),
      ..sb.try_assign_refs
    ]
    [] -> sb.try_assign_refs
  }
  let assign_ref = AssignRef(scope: sb.current, name:, order_stamp: sb.next_id)
  ScopeBuilder(
    ..sb,
    assign_refs: [assign_ref, ..sb.assign_refs],
    try_assign_refs:,
  )
}

pub fn enter_try(sb: ScopeBuilder) -> ScopeBuilder {
  ScopeBuilder(..sb, try_scopes: [sb.current, ..sb.try_scopes])
}

pub fn leave_try(sb: ScopeBuilder) -> ScopeBuilder {
  ScopeBuilder(..sb, try_scopes: list.drop(sb.try_scopes, 1))
}

pub fn lexical_ref(sb: ScopeBuilder, ref: LexicalRef) -> ScopeBuilder {
  let own_lexical_refs =
    dict.upsert(sb.own_lexical_refs, sb.current_fn, fn(prev) {
      let prev = option.unwrap(prev, lexical.no_lexical_refs)
      case ref {
        ThisRef -> lexical.LexicalRefs(..prev, this: True)
        ActiveFuncRef -> lexical.LexicalRefs(..prev, active_func: True)
        HomeObjectRef -> lexical.LexicalRefs(..prev, home_object: True)
        NewTargetRef -> lexical.LexicalRefs(..prev, new_target: True)
      }
    })
  ScopeBuilder(..sb, own_lexical_refs:)
}

pub fn mark_eval(sb: ScopeBuilder) -> ScopeBuilder {
  update_current(sb, fn(s) { RawScope(..s, contains_direct_eval: True) })
}

pub fn set_children(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  ordered: List(ScopeId),
) -> ScopeBuilder {
  ScopeBuilder(
    ..sb,
    children_at: dict.insert(sb.children_at, parent_id, ordered),
  )
}

pub fn enter(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = scope_at(sb, id)
  ScopeBuilder(..sb, current: id, current_fn: scope.function_scope)
}

pub fn children_newest_first(sb: ScopeBuilder, id: ScopeId) -> List(ScopeId) {
  dict.get(sb.children_at, id) |> result.unwrap([])
}

pub fn declare_in(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  name: String,
  kind: BindingKind,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let scope = scope_at(sb, scope_id)
  case dict.has_key(scope.bindings, name) {
    True -> sb
    False -> {
      let decl_order = scope.next_decl_order
      let updated =
        RawScope(
          ..scope,
          bindings: dict.insert(
            scope.bindings,
            name,
            RawBinding(kind:, synthetic:, decl_order:),
          ),
          next_decl_order: decl_order + 1,
        )
      ScopeBuilder(..sb, scopes: dict.insert(sb.scopes, scope_id, updated))
    }
  }
}

pub fn insert_param_shims(sb: ScopeBuilder, count: Int) -> ScopeBuilder {
  use <- bool.guard(count <= 0, sb)
  let fn_id = sb.current_fn
  let scope = scope_at(sb, fn_id)
  // stays param kind here, finalize_scope rekinds to let
  let shifted =
    dict.map_values(scope.bindings, fn(_name, rb) {
      RawBinding(..rb, decl_order: rb.decl_order + count)
    })
  let with_shims = insert_param_shims_loop(shifted, 0, count)
  let scope =
    RawScope(
      ..scope,
      bindings: with_shims,
      next_decl_order: scope.next_decl_order + count,
      non_simple_shim_count: count,
    )
  ScopeBuilder(..sb, scopes: dict.insert(sb.scopes, fn_id, scope))
}

fn insert_param_shims_loop(
  bindings: Dict(String, RawBinding),
  i: Int,
  count: Int,
) -> Dict(String, RawBinding) {
  case i >= count {
    True -> bindings
    False ->
      insert_param_shims_loop(
        dict.insert(
          bindings,
          param_shim(i),
          RawBinding(kind: ParamBinding, synthetic: True, decl_order: i),
        ),
        i + 1,
        count,
      )
  }
}

pub fn discard(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = scope_at(sb, id)
  let children_at = case scope.parent {
    Some(pid) -> {
      let siblings = children_newest_first(sb, pid)
      dict.insert(sb.children_at, pid, list.filter(siblings, fn(c) { c != id }))
    }
    None -> sb.children_at
  }
  ScopeBuilder(
    ..sb,
    functions: dict.delete(sb.functions, id),
    children_at: dict.delete(children_at, id),
    own_lexical_refs: dict.delete(sb.own_lexical_refs, id),
  )
}

fn block_prunable(scope: RawScope) -> Bool {
  scope.kind == Block && dict.is_empty(scope.bindings) && !scope.is_var_boundary
}

// v8 finalize_block_scope: splice out a block with no bindings
fn prune_empty_block(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = scope_at(sb, id)
  case block_prunable(scope), scope.parent {
    True, Some(parent_id) -> {
      // keep the eval flag or eval() silently goes indirect
      let sb = case scope.contains_direct_eval {
        False -> sb
        True -> {
          let parent = scope_at(sb, parent_id)
          ScopeBuilder(
            ..sb,
            scopes: dict.insert(
              sb.scopes,
              parent_id,
              RawScope(..parent, contains_direct_eval: True),
            ),
          )
        }
      }
      let own_children = children_newest_first(sb, id)
      let spliced = case children_newest_first(sb, parent_id) {
        [head, ..rest] if head == id -> list.append(own_children, rest)
        parent_children ->
          list.flat_map(parent_children, fn(c) {
            case c == id {
              True -> own_children
              False -> [c]
            }
          })
      }
      // perf: tombstone stays in sb.scopes, never remap raw_refs (quadratic)
      let scopes =
        list.fold(own_children, sb.scopes, fn(acc, child_id) {
          let child = scope_at(sb, child_id)
          dict.insert(acc, child_id, RawScope(..child, parent: Some(parent_id)))
        })
      ScopeBuilder(
        ..sb,
        scopes:,
        children_at: sb.children_at
          |> dict.insert(parent_id, spliced)
          |> dict.delete(id),
      )
    }
    _, _ -> sb
  }
}

pub fn set_source_tag(
  sb: ScopeBuilder,
  id: ScopeId,
  tag: SourceTag,
) -> ScopeBuilder {
  let scope = scope_at(sb, id)
  ScopeBuilder(
    ..sb,
    scopes: dict.insert(sb.scopes, id, RawScope(..scope, source_tag: tag)),
  )
}

// children opened under parent_id after the before snapshot, newest first
pub fn children_since(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  before: List(ScopeId),
) -> List(ScopeId) {
  let now = children_newest_first(sb, parent_id)
  list.take(now, list.length(now) - list.length(before))
}

pub fn tag_children_since(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  before: List(ScopeId),
  tag: SourceTag,
) -> ScopeBuilder {
  use sb, id <- list.fold(children_since(sb, parent_id, before), sb)
  set_source_tag(sb, id, tag)
}

fn tag_of(sb: ScopeBuilder, id: ScopeId) -> SourceTag {
  scope_at(sb, id).source_tag
}

pub fn reorder_block_children(
  sb: ScopeBuilder,
  scope_id: ScopeId,
) -> ScopeBuilder {
  reorder_body_children(sb, scope_id, [])
}

// scopes opened before the body keep their place ahead of it
pub fn reorder_body_children(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  before: List(ScopeId),
) -> ScopeBuilder {
  use <- bool.guard(children_newest_first(sb, scope_id) == [], sb)
  let body_src = children_since(sb, scope_id, before) |> list.reverse
  let #(fn_decls, rest) =
    list.partition(body_src, fn(id) { tag_of(sb, id) == FnDeclSource })
  set_children(
    sb,
    scope_id,
    list.flatten([list.reverse(before), fn_decls, rest]),
  )
}

// order: case-body fn decls, case-test scopes, other case-body scopes
pub fn reorder_switch_children(
  sb: ScopeBuilder,
  switch_id: ScopeId,
) -> ScopeBuilder {
  let rev = children_newest_first(sb, switch_id)
  use <- bool.guard(rev == [], sb)
  let src_order = list.reverse(rev)
  let #(fn_decls, non_decl) =
    list.partition(src_order, fn(id) { tag_of(sb, id) == FnDeclSource })
  let #(tests, rest) =
    list.partition(non_decl, fn(id) { tag_of(sb, id) == SwitchTestSource })
  set_children(sb, switch_id, list.flatten([fn_decls, tests, rest]))
}

pub fn close_block(sb: ScopeBuilder, block_id: ScopeId) -> ScopeBuilder {
  case block_prunable(scope_at(sb, block_id)) {
    True -> prune_empty_block(sb, block_id)
    False -> reorder_block_children(sb, block_id)
  }
}

pub fn update_current(
  sb: ScopeBuilder,
  f: fn(RawScope) -> RawScope,
) -> ScopeBuilder {
  let scope = scope_at(sb, sb.current)
  ScopeBuilder(..sb, scopes: dict.insert(sb.scopes, sb.current, f(scope)))
}

pub fn update_current_fn(
  sb: ScopeBuilder,
  f: fn(RawFunctionInfo) -> RawFunctionInfo,
) -> ScopeBuilder {
  ScopeBuilder(
    ..sb,
    functions: dict.insert(
      sb.functions,
      sb.current_fn,
      f(fn_info_at(sb, sb.current_fn)),
    ),
  )
}

pub fn annexb_candidate(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  update_current_fn(sb, fn(fi) {
    RawFunctionInfo(..fi, annexb_candidates: [
      #(sb.current, name),
      ..fi.annexb_candidates
    ])
  })
}

// nfe self name excluded, a var of that name is legal
fn is_lexical_kind(kind: BindingKind) -> Bool {
  case kind {
    LetBinding | ConstBinding -> True
    VarBinding | ParamBinding | CatchBinding | CaptureBinding | FnNameBinding ->
      False
  }
}

pub fn raw_binding_kind(scope: RawScope, name: String) -> Option(BindingKind) {
  case dict.get(scope.bindings, name) {
    Ok(rb) -> Some(rb.kind)
    Error(Nil) -> None
  }
}

// §14.2.1 duplicate lexical declaration check
pub fn lexical_conflict(sb: ScopeBuilder, name: String) -> Bool {
  let scope = scope_at(sb, sb.current)
  dict.has_key(scope.bindings, name)
  || set.contains(scope.hoisted_vars, name)
  || boundary_param_conflict(sb, scope, name)
}

// §15.2.1 / §14.15.1 body lexical names vs param or catch names
fn boundary_param_conflict(
  sb: ScopeBuilder,
  scope: RawScope,
  name: String,
) -> Bool {
  case scope.kind, scope.parent {
    Block, Some(parent_id) -> {
      let parent = scope_at(sb, parent_id)
      use <- bool.guard(!scope.is_var_boundary && parent.kind != Catch, False)
      case raw_binding_kind(parent, name) {
        Some(ParamBinding) | Some(CatchBinding) -> True
        Some(_) | None -> False
      }
    }
    _, _ -> False
  }
}

// only the implicit arguments placeholder blocks a let arguments
pub fn only_implicit_arguments(sb: ScopeBuilder, name: String) -> Bool {
  use <- bool.guard(name != "arguments", False)
  let scope = scope_at(sb, sb.current)
  use <- bool.guard(set.contains(scope.hoisted_vars, name), False)
  case dict.get(scope.bindings, name) {
    Ok(RawBinding(kind: VarBinding, synthetic: True, ..)) -> True
    Ok(_) | Error(Nil) -> False
  }
}

pub fn current_has_kind(
  sb: ScopeBuilder,
  name: String,
  kind: BindingKind,
) -> Bool {
  raw_binding_kind(scope_at(sb, sb.current), name) == Some(kind)
}

pub fn var_conflicts_lexical(sb: ScopeBuilder, name: String) -> Bool {
  use _id, scope, _acc <- fold_up(
    sb,
    from: sb.current,
    stop_at_fn: True,
    init: False,
  )
  case raw_binding_kind(scope, name) |> option.map(is_lexical_kind) {
    Some(True) -> list.Stop(True)
    Some(False) | None -> list.Continue(False)
  }
}

// §16.2.1.1 module top fn decls are lexical, so var conflicts
pub fn var_conflicts_module_fn(sb: ScopeBuilder, name: String) -> Bool {
  use <- bool.guard(sb.current_fn != root_scope_id, False)
  let root = scope_at(sb, root_scope_id)
  use <- bool.guard(root.kind != Module, False)
  dict.has_key(root.bindings, name) && !set.contains(root.hoisted_vars, name)
}

pub fn root_has(sb: ScopeBuilder, name: String) -> Bool {
  dict.has_key(scope_at(sb, root_scope_id).bindings, name)
}

pub fn nearest_catch_params(sb: ScopeBuilder) -> List(String) {
  use _id, scope, _acc <- fold_up(
    sb,
    from: sb.current,
    stop_at_fn: True,
    init: [],
  )
  case scope.kind {
    Catch ->
      list.Stop({
        use #(name, rb) <- list.filter_map(dict.to_list(scope.bindings))
        case rb.kind {
          ParamBinding | CatchBinding -> Ok(name)
          _ -> Error(Nil)
        }
      })
    _ -> list.Continue([])
  }
}

fn with_depth(sb: ScopeBuilder) -> Int {
  use _id, scope, acc <- fold_up(
    sb,
    from: sb.current,
    stop_at_fn: False,
    init: 0,
  )
  list.Continue(case scope.kind {
    With(_) -> acc + 1
    _ -> acc
  })
}
