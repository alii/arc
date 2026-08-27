import arc/bytecode/lexical.{
  type LexicalRef, type LexicalRefs, type LexicalSlots, RefActiveFunc,
  RefHomeObject, RefNewTarget, RefThis,
}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub type GlobalFallthrough {
  ToGlobal
  ToEvalEnv
}

pub type BindingKind {
  VarBinding
  LetBinding
  ConstBinding
  ParamBinding
  CatchBinding
  CaptureBinding
  /// §13.2.5.5 nfe self name; sloppy writes are silently dropped
  FnNameBinding
}

pub type ScopeId =
  Int

pub const root_scope_id: ScopeId = 0

pub type ScopeKind {
  Module
  Script
  Function
  Block
  Catch
  With(holder: String)
  ClassBody
  ClassStaticBlock
}

pub fn is_with_kind(kind: ScopeKind) -> Bool {
  case kind {
    With(_) -> True
    _ -> False
  }
}

pub fn is_function_kind(kind: ScopeKind) -> Bool {
  case kind {
    Module | Script | Function | ClassStaticBlock -> True
    Block | Catch | With(_) | ClassBody -> False
  }
}

pub type TopLevelLex {
  LexGlobal
  LexLocal
}

pub type Binding {
  Binding(
    slot: Int,
    kind: BindingKind,
    is_boxed: Bool,
    origin_kind_for_capture: BindingKind,
  )
}

pub type Scope {
  Scope(
    id: ScopeId,
    parent: Option(ScopeId),
    function_scope: ScopeId,
    kind: ScopeKind,
    bindings: Dict(String, Binding),
    contains_direct_eval: Bool,
    annexb_blocked: Set(String),
    is_strict: Bool,
    /// §10.2.11 step 28 body block of a non-simple-params function
    is_var_boundary: Bool,
  )
}

/// captures pair each name with the parent's slot
pub type FunctionInfo {
  FunctionInfo(
    local_count: Int,
    lexical: LexicalSlots,
    lexical_boxed: LexicalRefs,
    captures: List(#(String, Int)),
    lexical_captures: Dict(LexicalRef, Int),
    names: Dict(String, Int),
    fallthrough: GlobalFallthrough,
    contains_direct_eval: Bool,
    eval_in_subtree: Bool,
    annexb_candidates: List(String),
    is_arrow: Bool,
    is_derived_constructor: Bool,
  )
}

pub type ScopeTree {
  ScopeTree(
    scopes: Dict(ScopeId, Scope),
    functions: Dict(ScopeId, FunctionInfo),
    children_at: Dict(ScopeId, List(ScopeId)),
    top_lex: TopLevelLex,
    linker_seeded: Set(String),
    /// direct eval caller's with holders as root slots, innermost first
    inherited_with_stack: List(Int),
  )
}

pub type Direct {
  Local(slot: Int, boxed: Bool, kind: BindingKind, origin_kind: BindingKind)
  Global(name: String)
  EvalEnv(name: String)
}

pub type SlotRef {
  SlotRef(slot: Int, boxed: Bool)
}

pub type Resolution {
  Plain(direct: Direct)
  WithChain(crossed_slots: List(SlotRef), fallback: Direct)
}

pub type AnalyzeOpts {
  AnalyzeOpts(
    top_lex: TopLevelLex,
    fallthrough: GlobalFallthrough,
    strict: Bool,
    parent_names: Dict(String, Int),
    lexical_captures: Dict(LexicalRef, Int),
    linker_seeded: Set(String),
    /// sloppy script root vars get real slots; off by default
    module_slot_globals: Bool,
    /// box bindings declared outside a try and written inside it
    box_try_writes: Bool,
    /// slot indices into parent_names, innermost first
    with_stack: List(Int),
  )
}

pub fn default_analyze_opts() -> AnalyzeOpts {
  AnalyzeOpts(
    top_lex: LexLocal,
    fallthrough: ToGlobal,
    strict: False,
    parent_names: dict.new(),
    lexical_captures: dict.new(),
    linker_seeded: set.new(),
    module_slot_globals: False,
    box_try_writes: False,
    with_stack: [],
  )
}

pub type RawBinding {
  RawBinding(kind: BindingKind, synthetic: Bool, index: Int)
}

pub type SourceTag {
  TagFnDecl
  TagSwitchTest
  TagOther
}

pub type RawScope {
  RawScope(
    id: ScopeId,
    parent: Option(ScopeId),
    function_scope: ScopeId,
    kind: ScopeKind,
    bindings: Dict(String, RawBinding),
    next_binding_index: Int,
    contains_direct_eval: Bool,
    annexb_blocked: Set(String),
    is_strict: Bool,
    catch_param_simple: Bool,
    source_tag: SourceTag,
    /// vars hoisting through here; early errors only, not bindings
    hoisted_vars: Set(String),
    /// <paramN> shim count for a non-simple parameter list
    non_simple_shim_count: Int,
    /// §10.2.11 step 28 var sink body block for non-simple params
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
  is_strict: Bool,
) -> RawScope {
  RawScope(
    id:,
    parent:,
    function_scope:,
    kind:,
    bindings: dict.new(),
    next_binding_index: 0,
    contains_direct_eval: False,
    annexb_blocked: set.new(),
    is_strict:,
    catch_param_simple: True,
    source_tag: TagOther,
    hoisted_vars: set.new(),
    non_simple_shim_count: 0,
    is_var_boundary: False,
  )
}

pub type ScopeBuilder {
  ScopeBuilder(
    scopes: Dict(ScopeId, RawScope),
    functions: Dict(ScopeId, RawFunctionInfo),
    children_at: Dict(ScopeId, List(ScopeId)),
    next_id: Int,
    current: ScopeId,
    current_fn: ScopeId,
    raw_refs: List(#(ScopeId, String)),
    /// assignment targets stamped with next_id at the write
    assign_refs: List(#(ScopeId, String, ScopeId)),
    try_scopes: List(ScopeId),
    try_assign_refs: List(#(ScopeId, ScopeId, String)),
    own_lexical_refs: Dict(ScopeId, LexicalRefs),
  )
}

pub fn sb_init(root_kind: ScopeKind, strict: Bool) -> ScopeBuilder {
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

fn sb_scope(sb: ScopeBuilder, id: ScopeId) -> RawScope {
  let assert Ok(s) = dict.get(sb.scopes, id)
    as "scope.sb_scope: unknown ScopeId"
  s
}

fn sb_fn_info(sb: ScopeBuilder, fn_id: ScopeId) -> RawFunctionInfo {
  let assert Ok(info) = dict.get(sb.functions, fn_id)
    as "scope.sb_fn_info: unknown function scope"
  info
}

pub fn sb_push(sb: ScopeBuilder, kind: ScopeKind) -> #(ScopeBuilder, ScopeId) {
  let id = sb.next_id
  let parent = sb_scope(sb, sb.current)
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
      case prev {
        Some(l) -> [id, ..l]
        None -> [id]
      }
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

/// §14.11 the only way to build a with scope; declares its holder
pub fn sb_push_with(sb: ScopeBuilder) -> #(ScopeBuilder, ScopeId) {
  let holder = with_object_name(sb_with_depth(sb), sb.next_id)
  let #(sb, id) = sb_push(sb, With(holder:))
  #(sb_declare(sb, holder, LetBinding, synthetic: True), id)
}

/// §10.2.11 step 28 body scope for a non-simple parameter list
pub fn sb_push_var_boundary(sb: ScopeBuilder) -> #(ScopeBuilder, ScopeId) {
  let #(sb, id) = sb_push(sb, Block)
  let scope = sb_scope(sb, id)
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

/// first declaration wins
pub fn sb_declare(
  sb: ScopeBuilder,
  name: String,
  kind: BindingKind,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let target_id = case kind {
    VarBinding -> sb_var_target(sb)
    LetBinding
    | ConstBinding
    | ParamBinding
    | CatchBinding
    | CaptureBinding
    | FnNameBinding -> sb.current
  }
  sb_declare_in(sb, target_id, name, kind, synthetic:)
}

fn sb_var_target(sb: ScopeBuilder) -> ScopeId {
  use id, scope, acc <- sb_fold_up(
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

pub fn sb_declare_var(
  sb: ScopeBuilder,
  name: String,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let sb = sb_mark_hoisted_var(sb, sb.current, name)
  sb_declare(sb, name, VarBinding, synthetic:)
}

fn sb_fold_up(
  sb: ScopeBuilder,
  from at: ScopeId,
  stop_at_fn stop_at_fn: Bool,
  init acc: a,
  step step: fn(ScopeId, RawScope, a) -> list.ContinueOrStop(a),
) -> a {
  let scope = sb_scope(sb, at)
  case step(at, scope, acc) {
    list.Stop(acc) -> acc
    list.Continue(acc) ->
      case stop_at_fn && at == sb.current_fn, scope.parent {
        False, Some(pid) -> sb_fold_up(sb, pid, stop_at_fn, acc, step)
        _, _ -> acc
      }
  }
}

fn sb_mark_hoisted_var(
  sb: ScopeBuilder,
  at: ScopeId,
  name: String,
) -> ScopeBuilder {
  use id, scope, sb <- sb_fold_up(sb, from: at, stop_at_fn: True, init: sb)
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

pub fn sb_ref(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  ScopeBuilder(..sb, raw_refs: [#(sb.current, name), ..sb.raw_refs])
}

pub fn sb_assign_ref(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  let try_assign_refs = case sb.try_scopes {
    [enclosing, ..] -> [#(enclosing, sb.current, name), ..sb.try_assign_refs]
    [] -> sb.try_assign_refs
  }
  ScopeBuilder(
    ..sb,
    assign_refs: [#(sb.current, name, sb.next_id), ..sb.assign_refs],
    try_assign_refs:,
  )
}

pub fn sb_enter_try(sb: ScopeBuilder) -> ScopeBuilder {
  ScopeBuilder(..sb, try_scopes: [sb.current, ..sb.try_scopes])
}

pub fn sb_leave_try(sb: ScopeBuilder) -> ScopeBuilder {
  ScopeBuilder(..sb, try_scopes: list.drop(sb.try_scopes, 1))
}

pub fn sb_lexical_ref(sb: ScopeBuilder, ref: LexicalRef) -> ScopeBuilder {
  let own_lexical_refs =
    dict.upsert(sb.own_lexical_refs, sb.current_fn, fn(prev) {
      let prev = option.unwrap(prev, lexical.no_lexical_refs)
      case ref {
        RefThis -> lexical.LexicalRefs(..prev, this: True)
        RefActiveFunc -> lexical.LexicalRefs(..prev, active_func: True)
        RefHomeObject -> lexical.LexicalRefs(..prev, home_object: True)
        RefNewTarget -> lexical.LexicalRefs(..prev, new_target: True)
      }
    })
  ScopeBuilder(..sb, own_lexical_refs:)
}

pub fn sb_mark_eval(sb: ScopeBuilder) -> ScopeBuilder {
  sb_update_current(sb, fn(s) { RawScope(..s, contains_direct_eval: True) })
}

pub fn sb_set_children(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  ordered: List(ScopeId),
) -> ScopeBuilder {
  ScopeBuilder(
    ..sb,
    children_at: dict.insert(sb.children_at, parent_id, ordered),
  )
}

pub fn sb_enter(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  ScopeBuilder(..sb, current: id, current_fn: scope.function_scope)
}

pub fn sb_children_raw(sb: ScopeBuilder, id: ScopeId) -> List(ScopeId) {
  dict.get(sb.children_at, id) |> result.unwrap([])
}

pub fn sb_declare_in(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  name: String,
  kind: BindingKind,
  synthetic synthetic: Bool,
) -> ScopeBuilder {
  let scope = sb_scope(sb, scope_id)
  case dict.has_key(scope.bindings, name) {
    True -> sb
    False -> {
      let idx = scope.next_binding_index
      let updated =
        RawScope(
          ..scope,
          bindings: dict.insert(
            scope.bindings,
            name,
            RawBinding(kind:, synthetic:, index: idx),
          ),
          next_binding_index: idx + 1,
        )
      ScopeBuilder(..sb, scopes: dict.insert(sb.scopes, scope_id, updated))
    }
  }
}

pub fn sb_insert_param_shims(sb: ScopeBuilder, count: Int) -> ScopeBuilder {
  use <- bool.guard(count <= 0, sb)
  let fn_id = sb.current_fn
  let scope = sb_scope(sb, fn_id)
  // stays param kind here, finalize_scope rekinds to let
  let shifted =
    dict.map_values(scope.bindings, fn(_name, rb) {
      RawBinding(..rb, index: rb.index + count)
    })
  let with_shims = insert_param_shims_loop(shifted, 0, count)
  let scope =
    RawScope(
      ..scope,
      bindings: with_shims,
      next_binding_index: scope.next_binding_index + count,
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
          RawBinding(kind: ParamBinding, synthetic: True, index: i),
        ),
        i + 1,
        count,
      )
  }
}

pub fn sb_discard(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  let children_at = case scope.parent {
    Some(pid) -> {
      let siblings = sb_children_raw(sb, pid)
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

fn sb_block_prunable(scope: RawScope) -> Bool {
  scope.kind == Block && dict.is_empty(scope.bindings) && !scope.is_var_boundary
}

/// v8 finalize_block_scope: splice out a block with no bindings
pub fn sb_prune_empty_block(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  case sb_block_prunable(scope), scope.parent {
    True, Some(parent_id) -> {
      // keep the eval flag or eval() silently goes indirect
      let sb = case scope.contains_direct_eval {
        False -> sb
        True -> {
          let parent = sb_scope(sb, parent_id)
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
      let own_children = sb_children_raw(sb, id)
      let spliced = case sb_children_raw(sb, parent_id) {
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
          let child = sb_scope(sb, child_id)
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

// children_at must end up in emit's consumption order, not source order

pub fn sb_set_source_tag(
  sb: ScopeBuilder,
  id: ScopeId,
  tag: SourceTag,
) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  ScopeBuilder(
    ..sb,
    scopes: dict.insert(sb.scopes, id, RawScope(..scope, source_tag: tag)),
  )
}

pub fn sb_tag_children_since(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  marker: List(ScopeId),
  tag: SourceTag,
) -> ScopeBuilder {
  let now = sb_children_raw(sb, parent_id)
  let new_count = list.length(now) - list.length(marker)
  use <- bool.guard(new_count <= 0, sb)
  let new_ids = list.take(now, new_count)
  list.fold(new_ids, sb, fn(sb, id) { sb_set_source_tag(sb, id, tag) })
}

fn sb_tag_of(sb: ScopeBuilder, id: ScopeId) -> SourceTag {
  sb_scope(sb, id).source_tag
}

pub fn sb_reorder_block_children(
  sb: ScopeBuilder,
  scope_id: ScopeId,
) -> ScopeBuilder {
  sb_reorder_body_children(sb, scope_id, [])
}

pub fn sb_reorder_body_children(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  marker: List(ScopeId),
) -> ScopeBuilder {
  let rev = sb_children_raw(sb, scope_id)
  use <- bool.guard(rev == [], sb)
  let body_count = list.length(rev) - list.length(marker)
  let body_src = list.take(rev, body_count) |> list.reverse
  let pre_body = list.reverse(marker)
  let #(fn_decls, rest) =
    list.partition(body_src, fn(id) { sb_tag_of(sb, id) == TagFnDecl })
  sb_set_children(
    sb,
    scope_id,
    list.append(pre_body, list.append(fn_decls, rest)),
  )
}

/// order: case-body fn decls, case-test scopes, other case-body scopes
pub fn sb_reorder_switch_children(
  sb: ScopeBuilder,
  switch_id: ScopeId,
) -> ScopeBuilder {
  let rev = sb_children_raw(sb, switch_id)
  use <- bool.guard(rev == [], sb)
  let src_order = list.reverse(rev)
  let #(fn_decls, non_decl) =
    list.partition(src_order, fn(id) { sb_tag_of(sb, id) == TagFnDecl })
  let #(tests, rest) =
    list.partition(non_decl, fn(id) { sb_tag_of(sb, id) == TagSwitchTest })
  sb_set_children(
    sb,
    switch_id,
    list.append(fn_decls, list.append(tests, rest)),
  )
}

pub fn sb_close_block(sb: ScopeBuilder, block_id: ScopeId) -> ScopeBuilder {
  case sb_block_prunable(sb_scope(sb, block_id)) {
    True -> sb_prune_empty_block(sb, block_id)
    False -> sb_reorder_block_children(sb, block_id)
  }
}

pub fn sb_update_current(
  sb: ScopeBuilder,
  f: fn(RawScope) -> RawScope,
) -> ScopeBuilder {
  let scope = sb_scope(sb, sb.current)
  ScopeBuilder(..sb, scopes: dict.insert(sb.scopes, sb.current, f(scope)))
}

pub fn sb_update_current_fn(
  sb: ScopeBuilder,
  f: fn(RawFunctionInfo) -> RawFunctionInfo,
) -> ScopeBuilder {
  ScopeBuilder(
    ..sb,
    functions: dict.insert(
      sb.functions,
      sb.current_fn,
      f(sb_fn_info(sb, sb.current_fn)),
    ),
  )
}

pub fn sb_annexb_candidate(sb: ScopeBuilder, name: String) -> ScopeBuilder {
  sb_update_current_fn(sb, fn(fi) {
    RawFunctionInfo(..fi, annexb_candidates: [
      #(sb.current, name),
      ..fi.annexb_candidates
    ])
  })
}

/// nfe self name excluded, a var of that name is legal
fn is_lexical_kind(kind: BindingKind) -> Bool {
  case kind {
    LetBinding | ConstBinding -> True
    VarBinding | ParamBinding | CatchBinding | CaptureBinding | FnNameBinding ->
      False
  }
}

/// §14.2.1 duplicate lexical declaration check
pub fn sb_lexical_conflict(sb: ScopeBuilder, name: String) -> Bool {
  let scope = sb_scope(sb, sb.current)
  dict.has_key(scope.bindings, name)
  || set.contains(scope.hoisted_vars, name)
  || sb_boundary_param_conflict(sb, scope, name)
}

/// §15.2.1 / §14.15.1 body lexical names vs param or catch names
fn sb_boundary_param_conflict(
  sb: ScopeBuilder,
  scope: RawScope,
  name: String,
) -> Bool {
  let param_scope = case scope.kind, scope.parent {
    Block, Some(parent_id) ->
      case scope.is_var_boundary || sb_scope(sb, parent_id).kind == Catch {
        True -> Some(parent_id)
        False -> None
      }
    _, _ -> None
  }
  case param_scope {
    None -> False
    Some(parent_id) ->
      case dict.get(sb_scope(sb, parent_id).bindings, name) {
        Ok(rb) -> rb.kind == ParamBinding || rb.kind == CatchBinding
        Error(Nil) -> False
      }
  }
}

/// only the implicit arguments placeholder blocks a let arguments
pub fn sb_only_implicit_arguments(sb: ScopeBuilder, name: String) -> Bool {
  use <- bool.guard(name != "arguments", False)
  let scope = sb_scope(sb, sb.current)
  use <- bool.guard(set.contains(scope.hoisted_vars, name), False)
  case dict.get(scope.bindings, name) {
    Ok(RawBinding(kind: VarBinding, synthetic: True, ..)) -> True
    Ok(_) | Error(Nil) -> False
  }
}

pub fn sb_current_has_kind(
  sb: ScopeBuilder,
  name: String,
  kind: BindingKind,
) -> Bool {
  case dict.get(sb_scope(sb, sb.current).bindings, name) {
    Ok(rb) -> rb.kind == kind
    Error(Nil) -> False
  }
}

pub fn sb_var_conflicts_lexical(sb: ScopeBuilder, name: String) -> Bool {
  use _id, scope, _acc <- sb_fold_up(sb, sb.current, True, False)
  let hit = case dict.get(scope.bindings, name) {
    Ok(rb) -> is_lexical_kind(rb.kind)
    Error(Nil) -> False
  }
  case hit {
    True -> list.Stop(True)
    False -> list.Continue(False)
  }
}

/// §16.2.1.1 module top fn decls are lexical, so var conflicts
pub fn sb_var_conflicts_module_fn(sb: ScopeBuilder, name: String) -> Bool {
  use <- bool.guard(sb.current_fn != root_scope_id, False)
  let root = sb_scope(sb, root_scope_id)
  use <- bool.guard(root.kind != Module, False)
  dict.has_key(root.bindings, name) && !set.contains(root.hoisted_vars, name)
}

pub fn sb_root_has(sb: ScopeBuilder, name: String) -> Bool {
  dict.has_key(sb_scope(sb, root_scope_id).bindings, name)
}

pub fn sb_nearest_catch_params(sb: ScopeBuilder) -> List(String) {
  use _id, scope, _acc <- sb_fold_up(sb, sb.current, True, [])
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

pub fn sb_with_depth(sb: ScopeBuilder) -> Int {
  use _id, scope, acc <- sb_fold_up(sb, sb.current, False, 0)
  list.Continue(case scope.kind {
    With(_) -> acc + 1
    _ -> acc
  })
}

type FinSt {
  FinSt(scopes: Dict(ScopeId, Scope), functions: Dict(ScopeId, FunctionInfo))
}

fn blank_function_info(
  raw: RawFunctionInfo,
  fallthrough: GlobalFallthrough,
) -> FunctionInfo {
  FunctionInfo(
    local_count: 0,
    lexical: lexical.NoLexicalSlots,
    lexical_boxed: lexical.no_lexical_refs,
    captures: [],
    lexical_captures: dict.new(),
    names: dict.new(),
    fallthrough:,
    contains_direct_eval: False,
    eval_in_subtree: False,
    annexb_candidates: [],
    is_arrow: raw.is_arrow,
    is_derived_constructor: raw.is_derived_constructor,
  )
}

pub fn finalize(sb: ScopeBuilder, opts: AnalyzeOpts) -> ScopeTree {
  let root_raw = sb_scope(sb, root_scope_id)
  let #(parent_kind, parent_origin) = case root_raw.kind {
    Module -> #(CaptureBinding, ConstBinding)
    Script
    | Function
    | Block
    | Catch
    | With(_)
    | ClassBody
    | ClassStaticBlock -> #(CaptureBinding, CaptureBinding)
  }
  let parent_bindings =
    dict.map_values(opts.parent_names, fn(_name, slot) {
      Binding(
        slot:,
        kind: parent_kind,
        is_boxed: True,
        origin_kind_for_capture: parent_origin,
      )
    })
  let root_raw_fn = sb_fn_info(sb, root_scope_id)
  // with_stack indexes parent_names so it adds no slots
  let root_base =
    dict.size(opts.parent_names) + dict.size(opts.lexical_captures)
  // owner iff root_base == 0, not merely empty lexical_captures
  let script_root_owns_lexical = root_raw.kind == Script && root_base == 0
  let #(root_local_count, root_lexical) = case script_root_owns_lexical {
    True -> #(
      root_base + lexical.owned_lexical_slot_count,
      lexical.OwnedLexicalSlots(base: root_base),
    )
    False -> #(root_base, lexical.NoLexicalSlots)
  }
  let root_fn =
    FunctionInfo(
      ..blank_function_info(root_raw_fn, opts.fallthrough),
      local_count: root_local_count,
      lexical: root_lexical,
      lexical_captures: opts.lexical_captures,
      names: opts.parent_names,
    )
  let st =
    FinSt(
      scopes: dict.new(),
      functions: dict.from_list([#(root_scope_id, root_fn)]),
    )
  let st =
    finalize_scope(sb, opts, st, root_scope_id, parent_bindings, opts.strict)
  let st = hoist_annexb_block_functions(sb, st, opts)
  let tree =
    ScopeTree(
      scopes: st.scopes,
      functions: st.functions,
      children_at: sb.children_at,
      top_lex: opts.top_lex,
      linker_seeded: opts.linker_seeded,
      inherited_with_stack: opts.with_stack,
    )
  let captured = resolve_raw_refs(tree, sb)
  let assigned = resolve_assign_refs(tree, sb)
  let try_assigned = case opts.box_try_writes {
    True -> resolve_try_assign_refs(tree, sb)
    False -> dict.new()
  }
  let refs_args = resolve_arguments_refs(tree, sb)
  let fn_decls =
    dict.fold(sb.scopes, set.new(), fn(acc, id, raw) {
      case raw.source_tag {
        TagFnDecl -> set.insert(acc, id)
        TagSwitchTest | TagOther -> acc
      }
    })
  analyze_captures(
    tree,
    captured,
    assigned,
    try_assigned,
    refs_args,
    fn_decls,
    sb.own_lexical_refs,
  )
}

fn finalize_scope(
  sb: ScopeBuilder,
  opts: AnalyzeOpts,
  st: FinSt,
  scope_id: ScopeId,
  seed_bindings: Dict(String, Binding),
  inherited_strict: Bool,
) -> FinSt {
  let raw = sb_scope(sb, scope_id)
  let is_strict = raw.is_strict || inherited_strict
  let st = case
    is_function_kind(raw.kind) && !dict.has_key(st.functions, scope_id)
  {
    False -> st
    True -> {
      let raw_fn = sb_fn_info(sb, scope_id)
      let info = blank_function_info(raw_fn, opts.fallthrough)
      FinSt(..st, functions: dict.insert(st.functions, scope_id, info))
    }
  }
  let fn_id = raw.function_scope
  let assert Ok(info) = dict.get(st.functions, fn_id)
    as "scope.finalize_scope: function_scope FunctionInfo missing (pre-order invariant violated)"
  let sorted =
    raw.bindings
    |> dict.to_list
    |> list.filter(fn(entry) {
      let #(_name, rb) = entry
      root_binding_is_local(raw.kind, scope_id, opts, rb.kind)
    })
    |> list.sort(fn(a, b) {
      let #(_, ra) = a
      let #(_, rb) = b
      int.compare(ra.index, rb.index)
    })
  let #(bindings, info) =
    list.fold(sorted, #(seed_bindings, info), fn(acc, entry) {
      let #(bindings, info) = acc
      let #(name, rb) = entry
      // seeds win, except names a strict direct eval declares itself
      let strict_eval_root = opts.strict && raw.kind == Script
      let keep_seeded =
        dict.has_key(bindings, name)
        && !{ strict_eval_root && dict.has_key(seed_bindings, name) }
      use <- bool.guard(keep_seeded, acc)
      let slot = info.local_count
      // §10.2.11 step 28: user formals past the shims become let (tdz)
      let kind = case rb.kind {
        ParamBinding ->
          case
            raw.non_simple_shim_count > 0
            && rb.index >= raw.non_simple_shim_count
          {
            True -> LetBinding
            False -> ParamBinding
          }
        k -> k
      }
      let binding =
        Binding(slot:, kind:, is_boxed: False, origin_kind_for_capture: kind)
      let names = case dict.has_key(info.names, name) {
        True -> info.names
        False -> dict.insert(info.names, name, slot)
      }
      #(
        dict.insert(bindings, name, binding),
        FunctionInfo(..info, local_count: slot + 1, names:),
      )
    })
  let scope =
    Scope(
      id: raw.id,
      parent: raw.parent,
      function_scope: raw.function_scope,
      kind: raw.kind,
      bindings:,
      contains_direct_eval: raw.contains_direct_eval,
      annexb_blocked: raw.annexb_blocked,
      is_strict:,
      is_var_boundary: raw.is_var_boundary,
    )
  let st =
    FinSt(
      scopes: dict.insert(st.scopes, scope_id, scope),
      functions: dict.insert(st.functions, fn_id, info),
    )
  let children = sb_children_raw(sb, scope_id)
  list.fold(children, st, fn(st, child_id) {
    finalize_scope(sb, opts, st, child_id, dict.new(), is_strict)
  })
}

/// sloppy script root vars and repl lexicals get no local slot
fn root_binding_is_local(
  scope_kind: ScopeKind,
  scope_id: ScopeId,
  opts: AnalyzeOpts,
  kind: BindingKind,
) -> Bool {
  use <- bool.guard(scope_id != root_scope_id, True)
  case scope_kind {
    Script ->
      case kind {
        VarBinding -> opts.strict || opts.module_slot_globals
        LetBinding | ConstBinding -> opts.top_lex == LexLocal
        ParamBinding | CatchBinding | CaptureBinding | FnNameBinding -> True
      }
    Module
    | Function
    | Block
    | Catch
    | With(_)
    | ClassBody
    | ClassStaticBlock -> True
  }
}

/// §B.3.2-6 annex b var twins, decided once the whole body is known
fn hoist_annexb_block_functions(
  sb: ScopeBuilder,
  st: FinSt,
  opts: AnalyzeOpts,
) -> FinSt {
  dict.fold(sb.functions, st, fn(st, fn_id, raw_fi) {
    use <- bool.guard(raw_fi.annexb_candidates == [], st)
    let fn_raw = sb_scope(sb, fn_id)
    let var_is_local =
      root_binding_is_local(fn_raw.kind, fn_id, opts, VarBinding)
    list.fold(raw_fi.annexb_candidates, st, fn(st, cand) {
      let #(block_id, name) = cand
      case annexb_walk_blocked(sb, block_id, fn_id, name) {
        True -> {
          let assert Ok(bs) = dict.get(st.scopes, block_id)
            as "scope: Annex-B block absent from finalized scopes"
          FinSt(
            ..st,
            scopes: dict.insert(
              st.scopes,
              block_id,
              Scope(..bs, annexb_blocked: set.insert(bs.annexb_blocked, name)),
            ),
          )
        }
        False -> {
          let assert Ok(info) = dict.get(st.functions, fn_id)
            as "scope.hoist_annexb_block_functions: FunctionInfo missing"
          let assert Ok(fn_scope) = dict.get(st.scopes, fn_id)
            as "scope.hoist_annexb_block_functions: fn-root Scope missing"
          let already = dict.has_key(fn_scope.bindings, name)
          let #(fn_scope, info) = case var_is_local && !already {
            False -> #(fn_scope, info)
            True -> {
              let slot = info.local_count
              let binding =
                Binding(
                  slot:,
                  kind: VarBinding,
                  is_boxed: False,
                  origin_kind_for_capture: VarBinding,
                )
              let names = case dict.has_key(info.names, name) {
                True -> info.names
                False -> dict.insert(info.names, name, slot)
              }
              #(
                Scope(
                  ..fn_scope,
                  bindings: dict.insert(fn_scope.bindings, name, binding),
                ),
                FunctionInfo(..info, local_count: slot + 1, names:),
              )
            }
          }
          let info =
            FunctionInfo(..info, annexb_candidates: [
              name,
              ..info.annexb_candidates
            ])
          FinSt(
            scopes: dict.insert(st.scopes, fn_id, fn_scope),
            functions: dict.insert(st.functions, fn_id, info),
          )
        }
      }
    })
  })
}

/// would var name be an early error between the block and fn_id
fn annexb_walk_blocked(
  sb: ScopeBuilder,
  from_block: ScopeId,
  fn_id: ScopeId,
  name: String,
) -> Bool {
  case sb_scope(sb, from_block).parent {
    None -> False
    Some(parent_id) -> annexb_check_chain(sb, parent_id, fn_id, name)
  }
}

fn annexb_check_chain(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  fn_id: ScopeId,
  name: String,
) -> Bool {
  let raw = sb_scope(sb, scope_id)
  let blocked_here = case dict.get(raw.bindings, name) {
    Error(Nil) -> False
    Ok(rb) ->
      case raw.kind {
        // §B.3.4 only a simple catch param is var-transparent
        Catch -> !raw.catch_param_simple
        Module
        | Script
        | Function
        | Block
        | With(_)
        | ClassBody
        | ClassStaticBlock ->
          case rb.kind {
            LetBinding | ConstBinding | FnNameBinding -> True
            // §B.3.2.1 a same-named formal suppresses the twin
            ParamBinding -> True
            // unreachable, catch params are recorded as param kind
            CatchBinding -> False
            VarBinding | CaptureBinding -> False
          }
      }
  }
  use <- bool.guard(blocked_here, True)
  use <- bool.guard(scope_id == fn_id, False)
  case raw.parent {
    None -> False
    Some(parent_id) -> annexb_check_chain(sb, parent_id, fn_id, name)
  }
}

fn resolve_raw_refs(
  tree: ScopeTree,
  sb: ScopeBuilder,
) -> Set(#(ScopeId, String)) {
  let #(_seen, captured) =
    list.fold(sb.raw_refs, #(set.new(), set.new()), fn(acc, ref) {
      let #(seen, captured) = acc
      let #(scope_id, name) = ref
      // scope may be a pruned tombstone; start from a live ancestor
      let assert Ok(raw) = dict.get(sb.scopes, scope_id)
        as "scope.resolve_raw_refs: dangling raw_ref"
      use <- bool.guard(set.contains(seen, ref), acc)
      let seen = set.insert(seen, ref)
      let ref_fn = raw.function_scope
      let is_free = case nearest_finalized(tree, sb, scope_id) {
        None -> True
        Some(start) ->
          case find_declaring_scope(tree, start, name) {
            None -> True
            Some(decl) -> decl.function_scope != ref_fn
          }
      }
      case is_free {
        False -> #(seen, captured)
        True -> #(seen, set.insert(captured, #(ref_fn, name)))
      }
    })
  captured
}

fn resolve_assign_refs(
  tree: ScopeTree,
  sb: ScopeBuilder,
) -> Dict(ScopeId, Dict(String, ScopeId)) {
  use acc, ref <- list.fold(sb.assign_refs, dict.new())
  let #(scope_id, name, stamp) = ref
  case nearest_finalized(tree, sb, scope_id) {
    None -> acc
    Some(start) ->
      case find_declaring_scope(tree, start, name) {
        None -> acc
        Some(decl) ->
          dict.upsert(acc, decl.function_scope, fn(prev) {
            option.unwrap(prev, dict.new())
            |> dict.upsert(name, fn(last) {
              int.max(stamp, option.unwrap(last, 0))
            })
          })
      }
  }
}

fn resolve_try_assign_refs(
  tree: ScopeTree,
  sb: ScopeBuilder,
) -> Dict(ScopeId, Set(String)) {
  use acc, ref <- list.fold(sb.try_assign_refs, dict.new())
  let #(enclosing, scope_id, name) = ref
  let declaring = fn(id) {
    nearest_finalized(tree, sb, id)
    |> option.then(find_declaring_scope(tree, _, name))
  }
  case declaring(scope_id), declaring(enclosing) {
    Some(decl), Some(outer) if decl.id == outer.id ->
      dict.upsert(acc, decl.function_scope, fn(prev) {
        case prev {
          Some(s) -> set.insert(s, name)
          None -> set.from_list([name])
        }
      })
    _, _ -> acc
  }
}

fn resolve_arguments_refs(tree: ScopeTree, sb: ScopeBuilder) -> Set(ScopeId) {
  use acc, ref <- list.fold(sb.raw_refs, set.new())
  case ref {
    #(scope_id, "arguments") ->
      case nearest_finalized(tree, sb, scope_id) {
        None -> acc
        Some(start) ->
          case find_declaring_scope(tree, start, "arguments") {
            Some(decl) -> set.insert(acc, decl.function_scope)
            None -> acc
          }
      }
    _ -> acc
  }
}

fn find_declaring_scope(
  tree: ScopeTree,
  scope_id: ScopeId,
  name: String,
) -> Option(Scope) {
  let scope = get_scope(tree, scope_id)
  case dict.has_key(scope.bindings, name) {
    True -> Some(scope)
    False ->
      case scope.parent {
        Some(parent) -> find_declaring_scope(tree, parent, name)
        None -> None
      }
  }
}

fn nearest_finalized(
  tree: ScopeTree,
  sb: ScopeBuilder,
  scope_id: ScopeId,
) -> Option(ScopeId) {
  case dict.has_key(tree.scopes, scope_id) {
    True -> Some(scope_id)
    False ->
      case dict.get(sb.scopes, scope_id) {
        Error(Nil) -> None
        Ok(raw) ->
          case raw.parent {
            Some(parent_id) -> nearest_finalized(tree, sb, parent_id)
            None -> None
          }
      }
  }
}

pub const default_export = "*default*"

fn with_object_name(depth: Int, with_id: ScopeId) -> String {
  "<with" <> int.to_string(depth) <> "_" <> int.to_string(with_id) <> ">"
}

pub fn param_shim(idx: Int) -> String {
  "<param" <> int.to_string(idx) <> ">"
}

pub fn lookup(tree: ScopeTree, scope_id: ScopeId, name: String) -> Resolution {
  do_lookup(tree, scope_id, name, [])
}

fn do_lookup(
  tree: ScopeTree,
  scope_id: ScopeId,
  name: String,
  crossed: List(SlotRef),
) -> Resolution {
  let scope = get_scope(tree, scope_id)
  case dict.get(scope.bindings, name) {
    Ok(Binding(slot:, kind:, is_boxed:, origin_kind_for_capture:)) -> {
      // §9.1.2.1 inherited withs are probed before the closure env
      let crossed = case kind {
        CaptureBinding ->
          list.append(inherited_with_slots(tree, scope), crossed)
        VarBinding
        | LetBinding
        | ConstBinding
        | ParamBinding
        | CatchBinding
        | FnNameBinding -> crossed
      }
      wrap_with_chain(
        crossed,
        Local(
          slot:,
          boxed: is_boxed,
          kind:,
          origin_kind: origin_kind_for_capture,
        ),
      )
    }
    Error(Nil) -> {
      let crossed = case scope.kind {
        With(holder:) -> [own_holder_ref(scope, holder), ..crossed]
        _ -> crossed
      }
      // stop at the function boundary but still probe inherited withs
      case is_function_kind(scope.kind), scope.parent {
        False, Some(parent_id) -> do_lookup(tree, parent_id, name, crossed)
        True, _ | False, None -> {
          let crossed = list.append(inherited_with_slots(tree, scope), crossed)
          let info = function_info(tree, scope.function_scope)
          let fallback = case info.fallthrough {
            ToGlobal -> Global(name)
            ToEvalEnv -> EvalEnv(name)
          }
          wrap_with_chain(crossed, fallback)
        }
      }
    }
  }
}

fn own_holder_ref(with_scope: Scope, holder: String) -> SlotRef {
  let assert Ok(b) = dict.get(with_scope.bindings, holder)
    as "scope: With(holder) whose holder binding is not in its own scope"
  SlotRef(slot: b.slot, boxed: b.is_boxed)
}

fn wrap_with_chain(crossed: List(SlotRef), fallback: Direct) -> Resolution {
  case crossed {
    [] -> Plain(fallback)
    _ -> WithChain(crossed_slots: list.reverse(crossed), fallback:)
  }
}

fn inherited_with_slots(tree: ScopeTree, fn_root: Scope) -> List(SlotRef) {
  case fn_root.parent {
    None ->
      tree.inherited_with_stack
      |> list.map(SlotRef(slot: _, boxed: True))
      |> list.reverse
    Some(_) -> {
      use acc, holder <- fold_enclosing_withs(tree, fn_root.parent, [])
      case dict.get(fn_root.bindings, holder) {
        Ok(b) -> [SlotRef(slot: b.slot, boxed: True), ..acc]
        Error(Nil) -> acc
      }
    }
  }
}

fn fold_enclosing_withs(
  tree: ScopeTree,
  scope_id: Option(ScopeId),
  acc: a,
  f: fn(a, String) -> a,
) -> a {
  case scope_id {
    None -> acc
    Some(id) -> {
      let scope = get_scope(tree, id)
      let acc = case scope.kind {
        With(holder:) -> f(acc, holder)
        _ -> acc
      }
      fold_enclosing_withs(tree, scope.parent, acc, f)
    }
  }
}

pub fn lookup_lexical(
  tree: ScopeTree,
  scope_id: ScopeId,
  ref: LexicalRef,
) -> SlotRef {
  let scope = get_scope(tree, scope_id)
  let info = function_info(tree, scope.function_scope)
  let boxed = lexical.lexical_refs_get(info.lexical_boxed, ref)
  case lexical.lexical_slot(info.lexical, ref) {
    Some(slot) -> SlotRef(slot:, boxed:)
    None ->
      case dict.get(info.lexical_captures, ref) {
        Ok(slot) -> SlotRef(slot:, boxed:)
        Error(Nil) -> panic as "scope.lookup_lexical: no slot for lexical ref"
      }
  }
}

pub fn alloc_scratch(
  tree: ScopeTree,
  function_scope_id: ScopeId,
) -> #(ScopeTree, Int) {
  let info = function_info(tree, function_scope_id)
  let slot = info.local_count
  let info = FunctionInfo(..info, local_count: slot + 1)
  let tree =
    ScopeTree(
      ..tree,
      functions: dict.insert(tree.functions, function_scope_id, info),
    )
  #(tree, slot)
}

pub fn function_info(tree: ScopeTree, scope_id: ScopeId) -> FunctionInfo {
  let assert Ok(info) = dict.get(tree.functions, scope_id)
    as "scope.function_info: not a function scope"
  info
}

pub fn get_scope(tree: ScopeTree, scope_id: ScopeId) -> Scope {
  let assert Ok(scope) = dict.get(tree.scopes, scope_id)
    as "scope.get_scope: unknown ScopeId"
  scope
}

pub fn child_scopes(tree: ScopeTree, scope_id: ScopeId) -> List(ScopeId) {
  dict.get(tree.children_at, scope_id) |> result.unwrap([])
}

pub fn child_function_scopes(
  tree: ScopeTree,
  parent_fn_scope_id: ScopeId,
) -> List(ScopeId) {
  collect_child_fns(tree, parent_fn_scope_id, [])
  |> list.reverse
}

fn collect_child_fns(
  tree: ScopeTree,
  scope_id: ScopeId,
  acc: List(ScopeId),
) -> List(ScopeId) {
  use acc, child_id <- list.fold(child_scopes(tree, scope_id), acc)
  let child = get_scope(tree, child_id)
  case is_function_kind(child.kind) {
    True -> [child_id, ..acc]
    False -> collect_child_fns(tree, child_id, acc)
  }
}

type FnAnalysisInput {
  FnAnalysisInput(
    is_arrow: Bool,
    is_strict: Bool,
    lexical_refs: LexicalRefs,
    free_own: Set(String),
  )
}

type ParentView {
  ParentView(
    names: Dict(String, Int),
    name_set: Set(String),
    consts: Set(String),
    fn_names: Set(String),
    lets: Set(String),
    boxed: Set(String),
    lexical_available: LexicalRefs,
    lexical_boxed: LexicalRefs,
  )
}

fn build_capture_inputs(
  tree: ScopeTree,
  captured: Set(#(ScopeId, String)),
  own_lexical_refs: Dict(ScopeId, LexicalRefs),
) -> Dict(ScopeId, FnAnalysisInput) {
  let free_by_fn = derive_free_own(captured)
  build_inputs_rec(
    tree,
    free_by_fn,
    own_lexical_refs,
    root_scope_id,
    dict.new(),
  )
}

fn derive_free_own(
  captured: Set(#(ScopeId, String)),
) -> Dict(ScopeId, Set(String)) {
  use d, entry <- set.fold(captured, dict.new())
  let #(ref_fn, name) = entry
  dict.upsert(d, ref_fn, fn(prev) {
    case prev {
      Some(s) -> set.insert(s, name)
      None -> set.from_list([name])
    }
  })
}

fn build_inputs_rec(
  tree: ScopeTree,
  free_by_fn: Dict(ScopeId, Set(String)),
  own_lexical_refs: Dict(ScopeId, LexicalRefs),
  fn_id: ScopeId,
  acc: Dict(ScopeId, FnAnalysisInput),
) -> Dict(ScopeId, FnAnalysisInput) {
  let children = child_function_scopes(tree, fn_id)
  let acc =
    list.fold(children, acc, fn(acc, cid) {
      build_inputs_rec(tree, free_by_fn, own_lexical_refs, cid, acc)
    })
  let info = function_info(tree, fn_id)
  let scope = get_scope(tree, fn_id)
  let own =
    dict.get(own_lexical_refs, fn_id)
    |> result.unwrap(lexical.no_lexical_refs)
  let lexical_refs =
    list.fold(children, own, fn(refs, cid) {
      let assert Ok(cinp) = dict.get(acc, cid)
        as "build_inputs_rec: child not in acc after post-order recursion"
      case cinp.is_arrow {
        True -> lexical.lexical_refs_or(refs, cinp.lexical_refs)
        False -> refs
      }
    })
  let free_own = dict.get(free_by_fn, fn_id) |> result.unwrap(set.new())
  dict.insert(
    acc,
    fn_id,
    FnAnalysisInput(
      is_arrow: info.is_arrow,
      is_strict: scope.is_strict,
      lexical_refs:,
      free_own:,
    ),
  )
}

fn analyze_captures(
  tree: ScopeTree,
  captured: Set(#(ScopeId, String)),
  assigned: Dict(ScopeId, Dict(String, ScopeId)),
  try_assigned: Dict(ScopeId, Set(String)),
  refs_args: Set(ScopeId),
  fn_decls: Set(ScopeId),
  own_lexical_refs: Dict(ScopeId, LexicalRefs),
) -> ScopeTree {
  let inputs = build_capture_inputs(tree, captured, own_lexical_refs)
  let by_fn = scopes_by_function(tree)
  let up = compute_up(tree, inputs, by_fn, root_scope_id, dict.new())
  let root_parent =
    ParentView(
      names: dict.new(),
      name_set: set.new(),
      consts: set.new(),
      fn_names: set.new(),
      lets: set.new(),
      boxed: set.new(),
      lexical_available: lexical.no_lexical_refs,
      lexical_boxed: lexical.every_lexical_ref,
    )
  compute_down(
    tree,
    inputs,
    by_fn,
    up,
    assigned,
    try_assigned,
    refs_args,
    fn_decls,
    root_scope_id,
    root_parent,
  )
}

fn scopes_by_function(tree: ScopeTree) -> Dict(ScopeId, List(ScopeId)) {
  use d, id, scope <- dict.fold(tree.scopes, dict.new())
  dict.upsert(d, scope.function_scope, fn(prev) {
    case prev {
      Some(l) -> [id, ..l]
      None -> [id]
    }
  })
}

fn fn_member_scopes(
  by_fn: Dict(ScopeId, List(ScopeId)),
  fn_id: ScopeId,
) -> List(ScopeId) {
  dict.get(by_fn, fn_id) |> result.unwrap([])
}

type Up {
  Up(own_eval: Bool, eval_in_subtree: Bool, transitive_free: Set(String))
}

fn compute_up(
  tree: ScopeTree,
  inputs: Dict(ScopeId, FnAnalysisInput),
  by_fn: Dict(ScopeId, List(ScopeId)),
  fn_id: ScopeId,
  acc: Dict(ScopeId, Up),
) -> Dict(ScopeId, Up) {
  let children = child_function_scopes(tree, fn_id)
  let acc =
    list.fold(children, acc, fn(acc, child_id) {
      compute_up(tree, inputs, by_fn, child_id, acc)
    })
  let inp = get_input(inputs, fn_id)
  let own_scope_ids = fn_member_scopes(by_fn, fn_id)
  let own_scopes = list.map(own_scope_ids, get_scope(tree, _))
  let own_eval = list.any(own_scopes, fn(s) { s.contains_direct_eval })
  let eval_in_subtree =
    own_eval
    || list.any(children, fn(cid) { { get_up(acc, cid) }.eval_in_subtree })
  let declared = declared_in(own_scopes)
  let #(from_children, _) =
    list.fold(children, #(set.new(), dict.new()), fn(st, cid) {
      let #(s, memo) = st
      let creation = { get_scope(tree, cid) }.parent
      let #(visible_names, memo) = case dict.get(memo, creation) {
        Ok(names) -> #(names, memo)
        Error(Nil) -> {
          let names = set.from_list(dict.keys(visible_at_creation(tree, cid)))
          #(names, dict.insert(memo, creation, names))
        }
      }
      let child_free = { get_up(acc, cid) }.transitive_free
      #(set.union(s, set.difference(child_free, visible_names)), memo)
    })
  let with_free = fn_with_stack_free(tree, fn_id, declared)
  let transitive_free =
    inp.free_own |> set.union(from_children) |> set.union(with_free)
  dict.insert(acc, fn_id, Up(own_eval:, eval_in_subtree:, transitive_free:))
}

type LexLayout {
  LexLayout(
    lexical: LexicalSlots,
    lexical_captures: Dict(LexicalRef, Int),
    lexical_boxed: LexicalRefs,
    cap_count: Int,
    available: LexicalRefs,
    script_root_owns: Bool,
  )
}

fn compute_down(
  tree: ScopeTree,
  inputs: Dict(ScopeId, FnAnalysisInput),
  by_fn: Dict(ScopeId, List(ScopeId)),
  ups: Dict(ScopeId, Up),
  assigned: Dict(ScopeId, Dict(String, ScopeId)),
  try_assigned: Dict(ScopeId, Set(String)),
  refs_args: Set(ScopeId),
  fn_decls: Set(ScopeId),
  fn_id: ScopeId,
  parent: ParentView,
) -> ScopeTree {
  let inp = get_input(inputs, fn_id)
  let up = get_up(ups, fn_id)
  let is_root = fn_id == root_scope_id
  let children = child_function_scopes(tree, fn_id)
  let own_scope_ids = fn_member_scopes(by_fn, fn_id)
  let own_scopes = list.map(own_scope_ids, get_scope(tree, _))
  let declared = declared_in(own_scopes)
  let seeded_info = function_info(tree, fn_id)
  let kind = { get_scope(tree, fn_id) }.kind

  let #(captures, const_captures, fn_name_captures, let_captures) =
    derive_name_captures(up, parent)

  let lex =
    derive_lexical_layout(
      is_root,
      kind,
      inp,
      up,
      seeded_info,
      parent,
      list.length(captures),
      children,
      inputs,
    )

  let forced_box =
    case is_root {
      True -> tree.linker_seeded
      False -> set.new()
    }
    |> set.union(dict.get(try_assigned, fn_id) |> result.unwrap(set.new()))
  let assigned_here = dict.get(assigned, fn_id) |> result.unwrap(dict.new())
  // sloppy + arguments referenced: arguments[i]=v may write params
  let may_map_args = !inp.is_strict && set.contains(refs_args, fn_id)
  // const by value only if every capturer opens after its write
  let const_settled = fn(name) {
    case dict.get(assigned_here, name) {
      Error(Nil) -> False
      Ok(last_write) ->
        list.all(children, fn(cid) {
          !set.contains({ get_up(ups, cid) }.transitive_free, name)
          || { cid >= last_write && !set.contains(fn_decls, cid) }
        })
    }
  }
  let never_box =
    never_box_names(own_scopes, assigned_here, may_map_args, const_settled)
  let vars_to_box =
    derive_vars_to_box(up, ups, children, declared, never_box, forced_box)

  let tree = case is_root || lex.cap_count == 0 {
    True -> tree
    False ->
      insert_captures(
        tree,
        fn_id,
        own_scope_ids,
        lex.cap_count,
        captures,
        const_captures,
        fn_name_captures,
        let_captures,
        parent.boxed,
      )
  }

  let fallthrough =
    derive_fallthrough(is_root, seeded_info.fallthrough, up, inp)

  let tree = apply_boxing(tree, own_scope_ids, vars_to_box)
  let tree =
    update_function_info(tree, fn_id, fn(info) {
      // an owning script root keeps the lexical finalize seeded
      let lexical = case lex.script_root_owns {
        True -> info.lexical
        False -> lex.lexical
      }
      FunctionInfo(
        ..info,
        captures:,
        lexical:,
        lexical_captures: lex.lexical_captures,
        lexical_boxed: lex.lexical_boxed,
        fallthrough:,
        contains_direct_eval: up.own_eval,
        eval_in_subtree: up.eval_in_subtree,
      )
    })

  let #(tree, _) =
    list.fold(children, #(tree, dict.new()), fn(st, cid) {
      let #(tree, memo) = st
      let creation = { get_scope(tree, cid) }.parent
      let #(view, memo) = case dict.get(memo, creation) {
        Ok(view) -> #(view, memo)
        Error(Nil) -> {
          let view =
            child_parent_view(
              tree,
              cid,
              captures,
              const_captures,
              fn_name_captures,
              let_captures,
              lex.available,
              lex.lexical_boxed,
            )
          #(view, dict.insert(memo, creation, view))
        }
      }
      let tree =
        compute_down(
          tree,
          inputs,
          by_fn,
          ups,
          assigned,
          try_assigned,
          refs_args,
          fn_decls,
          cid,
          view,
        )
      #(tree, memo)
    })
  tree
}

fn derive_name_captures(
  up: Up,
  parent: ParentView,
) -> #(List(#(String, Int)), Set(String), Set(String), Set(String)) {
  let captured_names = case up.eval_in_subtree {
    True -> parent.name_set
    False -> set.intersection(up.transitive_free, parent.name_set)
  }
  let captures =
    captured_names
    |> set.to_list
    |> list.sort(string.compare)
    |> list.map(fn(name) {
      let assert Ok(parent_slot) = dict.get(parent.names, name)
        as "scope.captures: captured name absent from parent view"
      #(name, parent_slot)
    })
  let const_captures = set.intersection(parent.consts, captured_names)
  let fn_name_captures = set.intersection(parent.fn_names, captured_names)
  let let_captures = set.intersection(parent.lets, captured_names)
  #(captures, const_captures, fn_name_captures, let_captures)
}

fn derive_lexical_layout(
  is_root: Bool,
  kind: ScopeKind,
  inp: FnAnalysisInput,
  up: Up,
  seeded: FunctionInfo,
  parent: ParentView,
  name_cap_count: Int,
  children: List(ScopeId),
  inputs: Dict(ScopeId, FnAnalysisInput),
) -> LexLayout {
  let seeded_root_owns_lexical = case seeded.lexical {
    lexical.OwnedLexicalSlots(_) -> True
    lexical.CapturedLexicalSlots(..) | lexical.NoLexicalSlots -> False
  }
  let script_root_owns = is_root && kind == Script && seeded_root_owns_lexical
  let #(lexical_captures, available) = case is_root, inp.is_arrow {
    True, _ -> {
      let seeded_caps = seeded.lexical_captures
      let available = case script_root_owns {
        True -> lexical.every_lexical_ref
        False -> lexical_refs_present(seeded_caps)
      }
      #(seeded_caps, available)
    }
    False, False -> #(dict.new(), lexical.every_lexical_ref)
    False, True -> {
      let #(m, _next) =
        list.fold(
          lexical.all_lexical_refs,
          #(dict.new(), name_cap_count),
          fn(st, ref) {
            let #(m, i) = st
            let referenced =
              up.eval_in_subtree
              || lexical.lexical_refs_get(inp.lexical_refs, ref)
            let available =
              lexical.lexical_refs_get(parent.lexical_available, ref)
            case referenced && available {
              True -> #(dict.insert(m, ref, i), i + 1)
              False -> st
            }
          },
        )
      #(m, lexical_refs_present(m))
    }
  }

  // non-arrow functions and a plain script root own all four slots
  let lex_base = name_cap_count + dict.size(lexical_captures)
  let owns_lexical = case kind {
    Function -> !inp.is_arrow
    ClassStaticBlock -> True
    Script -> script_root_owns
    Module | Block | Catch | With(_) | ClassBody -> False
  }
  let #(slots, own_lexical_count) = case owns_lexical {
    False -> #(
      lexical.captured_lexical_slots(
        this: dict.get(lexical_captures, RefThis) |> option.from_result,
        active_func: dict.get(lexical_captures, RefActiveFunc)
          |> option.from_result,
        home_object: dict.get(lexical_captures, RefHomeObject)
          |> option.from_result,
        new_target: dict.get(lexical_captures, RefNewTarget)
          |> option.from_result,
      ),
      0,
    )
    True -> #(
      lexical.OwnedLexicalSlots(base: lex_base),
      lexical.owned_lexical_slot_count,
    )
  }

  // eval boxes all; derived ctor this boxed if an arrow reads it
  let lexical_boxed = case owns_lexical, up.eval_in_subtree {
    _, True -> lexical.every_lexical_ref
    True, False -> {
      let this_captured =
        seeded.is_derived_constructor
        && list.any(children, fn(cid) {
          let cinp = get_input(inputs, cid)
          cinp.is_arrow && cinp.lexical_refs.this
        })
      lexical.LexicalRefs(..lexical.no_lexical_refs, this: this_captured)
    }
    False, False ->
      lexical_refs_and(
        lexical_refs_present(lexical_captures),
        parent.lexical_boxed,
      )
  }

  LexLayout(
    lexical: slots,
    lexical_captures:,
    lexical_boxed:,
    cap_count: lex_base + own_lexical_count,
    available:,
    script_root_owns:,
  )
}

fn derive_vars_to_box(
  up: Up,
  ups: Dict(ScopeId, Up),
  children: List(ScopeId),
  declared: Set(String),
  never_box: Set(String),
  forced_box: Set(String),
) -> Set(String) {
  let vars_to_box = case up.eval_in_subtree {
    True -> declared
    False ->
      list.fold(children, set.new(), fn(s, cid) {
        let cu = get_up(ups, cid)
        case cu.eval_in_subtree {
          True -> set.union(s, declared)
          False ->
            set.intersection(cu.transitive_free, declared) |> set.union(s)
        }
      })
      |> set.difference(never_box)
  }
  set.union(vars_to_box, forced_box)
}

/// names safe to capture by value, deliberately conservative
fn never_box_names(
  own_scopes: List(Scope),
  assigned_here: Dict(String, ScopeId),
  may_map_args: Bool,
  const_settled: fn(String) -> Bool,
) -> Set(String) {
  let #(safe, consts, others, poison) = {
    use acc, scope <- list.fold(own_scopes, #(
      set.new(),
      set.new(),
      set.new(),
      set.new(),
    ))
    use acc, name, b <- dict.fold(scope.bindings, acc)
    let #(safe, consts, others, poison) = acc
    let assigned = dict.has_key(assigned_here, name)
    case b.kind {
      ParamBinding ->
        case may_map_args || assigned {
          False -> #(set.insert(safe, name), consts, others, poison)
          True -> #(safe, consts, set.insert(others, name), poison)
        }
      CatchBinding | FnNameBinding ->
        case assigned {
          False -> #(set.insert(safe, name), consts, others, poison)
          True -> #(safe, consts, set.insert(others, name), poison)
        }
      ConstBinding ->
        case const_settled(name) {
          True -> #(safe, set.insert(consts, name), others, poison)
          False -> #(safe, consts, others, set.insert(poison, name))
        }
      VarBinding | LetBinding | CaptureBinding -> #(
        safe,
        consts,
        others,
        set.insert(poison, name),
      )
    }
  }
  let consts = set.difference(consts, others) |> set.difference(safe)
  set.union(safe, consts) |> set.difference(poison)
}

fn derive_fallthrough(
  is_root: Bool,
  seeded: GlobalFallthrough,
  up: Up,
  inp: FnAnalysisInput,
) -> GlobalFallthrough {
  use <- bool.guard(is_root, seeded)
  case up.eval_in_subtree && !inp.is_strict {
    True -> ToEvalEnv
    False -> ToGlobal
  }
}

fn insert_captures(
  tree: ScopeTree,
  fn_id: ScopeId,
  own_scope_ids: List(ScopeId),
  cap_count: Int,
  captures: List(#(String, Int)),
  const_captures: Set(String),
  fn_name_captures: Set(String),
  let_captures: Set(String),
  parent_boxed: Set(String),
) -> ScopeTree {
  let scopes =
    list.fold(own_scope_ids, tree.scopes, fn(scopes, sid) {
      let scope = scopes_get_or_panic(scopes, sid)
      let bindings =
        dict.map_values(scope.bindings, fn(_name, b) {
          Binding(..b, slot: b.slot + cap_count)
        })
      dict.insert(scopes, sid, Scope(..scope, bindings:))
    })
  // a capture also declared here is shadowed by the own binding
  // names_shadowed also counts the var-boundary body block
  let scope_bindings = fn(sid) { scopes_get_or_panic(scopes, sid).bindings }
  let root_bindings = scope_bindings(fn_id)
  let root_shadowed = fn(name) { dict.has_key(root_bindings, name) }
  let var_boundary_names =
    own_scope_ids
    |> list.filter(fn(sid) { scopes_get_or_panic(scopes, sid).is_var_boundary })
    |> list.flat_map(fn(sid) { dict.keys(scope_bindings(sid)) })
    |> set.from_list
  let names_shadowed = fn(name) {
    root_shadowed(name) || set.contains(var_boundary_names, name)
  }
  let root = scopes_get_or_panic(scopes, fn_id)
  let scopes = {
    let bindings =
      list.index_fold(captures, root.bindings, fn(bs, cap, i) {
        let #(name, _parent_slot) = cap
        use <- bool.guard(root_shadowed(name), bs)
        let origin = case
          set.contains(const_captures, name),
          set.contains(fn_name_captures, name),
          set.contains(let_captures, name)
        {
          True, _, _ -> ConstBinding
          False, True, _ -> FnNameBinding
          False, False, True -> LetBinding
          False, False, False -> CaptureBinding
        }
        dict.insert(
          bs,
          name,
          Binding(
            slot: i,
            kind: CaptureBinding,
            is_boxed: set.contains(parent_boxed, name),
            origin_kind_for_capture: origin,
          ),
        )
      })
    dict.insert(scopes, fn_id, Scope(..root, bindings:))
  }
  let info = function_info(tree, fn_id)
  let names =
    dict.map_values(info.names, fn(_n, slot) { slot + cap_count })
    |> list.index_fold(captures, _, fn(d, cap, i) {
      case names_shadowed(cap.0) {
        True -> d
        False -> dict.insert(d, cap.0, i)
      }
    })
  let functions =
    dict.insert(
      tree.functions,
      fn_id,
      FunctionInfo(..info, local_count: info.local_count + cap_count, names:),
    )
  ScopeTree(..tree, scopes:, functions:)
}

fn child_parent_view(
  tree: ScopeTree,
  child_fn_id: ScopeId,
  our_captures: List(#(String, Int)),
  our_const_captures: Set(String),
  our_fn_name_captures: Set(String),
  our_let_captures: Set(String),
  lexical_available: LexicalRefs,
  lexical_boxed: LexicalRefs,
) -> ParentView {
  let cap_names =
    list.index_map(our_captures, fn(c, i) { #(c.0, i) }) |> dict.from_list
  let own_visible = visible_at_creation(tree, child_fn_id)
  let names =
    dict.fold(own_visible, cap_names, fn(d, name, b) {
      dict.insert(d, name, b.slot)
    })
  let own_names = set.from_list(dict.keys(own_visible))
  // origin kind, not kind, so a captured const stays const
  let origin_names = fn(kind, inherited) {
    let own =
      dict.fold(own_visible, set.new(), fn(s, name, b) {
        case b.origin_kind_for_capture == kind {
          True -> set.insert(s, name)
          False -> s
        }
      })
    set.union(own, set.difference(inherited, own_names))
  }
  let consts = origin_names(ConstBinding, our_const_captures)
  let fn_names = origin_names(FnNameBinding, our_fn_name_captures)
  let lets = origin_names(LetBinding, our_let_captures)
  let boxed =
    dict.fold(own_visible, set.new(), fn(s, name, b) {
      case b.is_boxed {
        True -> set.insert(s, name)
        False -> s
      }
    })
  ParentView(
    names:,
    name_set: set.from_list(dict.keys(names)),
    consts:,
    fn_names:,
    lets:,
    boxed:,
    lexical_available:,
    lexical_boxed:,
  )
}

fn apply_boxing(
  tree: ScopeTree,
  own_scope_ids: List(ScopeId),
  vars_to_box: Set(String),
) -> ScopeTree {
  use <- bool.guard(set.is_empty(vars_to_box), tree)
  let scopes =
    list.fold(own_scope_ids, tree.scopes, fn(scopes, sid) {
      let scope = scopes_get_or_panic(scopes, sid)
      let bindings =
        dict.map_values(scope.bindings, fn(name, b) {
          case set.contains(vars_to_box, name) {
            True -> Binding(..b, is_boxed: True)
            False -> b
          }
        })
      dict.insert(scopes, sid, Scope(..scope, bindings:))
    })
  ScopeTree(..tree, scopes:)
}

fn scopes_get_or_panic(scopes: Dict(ScopeId, Scope), sid: ScopeId) -> Scope {
  let assert Ok(scope) = dict.get(scopes, sid)
    as "scope.scopes_get_or_panic: unknown ScopeId"
  scope
}

fn update_function_info(
  tree: ScopeTree,
  fn_id: ScopeId,
  f: fn(FunctionInfo) -> FunctionInfo,
) -> ScopeTree {
  let info = function_info(tree, fn_id)
  ScopeTree(..tree, functions: dict.insert(tree.functions, fn_id, f(info)))
}

fn lexical_refs_and(a: LexicalRefs, b: LexicalRefs) -> LexicalRefs {
  lexical.LexicalRefs(
    this: a.this && b.this,
    active_func: a.active_func && b.active_func,
    home_object: a.home_object && b.home_object,
    new_target: a.new_target && b.new_target,
  )
}

fn lexical_refs_present(d: Dict(LexicalRef, a)) -> LexicalRefs {
  lexical.LexicalRefs(
    this: dict.has_key(d, RefThis),
    active_func: dict.has_key(d, RefActiveFunc),
    home_object: dict.has_key(d, RefHomeObject),
    new_target: dict.has_key(d, RefNewTarget),
  )
}

fn get_input(
  inputs: Dict(ScopeId, FnAnalysisInput),
  fn_id: ScopeId,
) -> FnAnalysisInput {
  let assert Ok(i) = dict.get(inputs, fn_id)
    as "scope.analyze_captures: build_capture_inputs missed a function scope"
  i
}

fn get_up(ups: Dict(ScopeId, Up), fn_id: ScopeId) -> Up {
  let assert Ok(u) = dict.get(ups, fn_id)
    as "scope.analyze_captures: missing bottom-up result"
  u
}

fn declared_in(scopes: List(Scope)) -> Set(String) {
  use s, scope <- list.fold(scopes, set.new())
  dict.fold(scope.bindings, s, fn(s, name, _b) { set.insert(s, name) })
}

fn fn_with_stack_free(
  tree: ScopeTree,
  fn_id: ScopeId,
  declared: Set(String),
) -> Set(String) {
  let start = { get_scope(tree, fn_id) }.parent
  { fold_enclosing_withs(tree, start, set.new(), set.insert) }
  |> set.difference(declared)
}

fn visible_at_creation(
  tree: ScopeTree,
  child_fn_id: ScopeId,
) -> Dict(String, Binding) {
  let child = get_scope(tree, child_fn_id)
  case child.parent {
    None -> dict.new()
    Some(parent_id) -> {
      let parent_fn = { get_scope(tree, parent_id) }.function_scope
      collect_visible(tree, parent_id, parent_fn, dict.new())
    }
  }
}

fn collect_visible(
  tree: ScopeTree,
  scope_id: ScopeId,
  stop_at_fn: ScopeId,
  acc: Dict(String, Binding),
) -> Dict(String, Binding) {
  let scope = get_scope(tree, scope_id)
  let acc =
    dict.fold(scope.bindings, acc, fn(d, name, b) {
      case dict.has_key(d, name) {
        True -> d
        False -> dict.insert(d, name, b)
      }
    })
  case scope_id == stop_at_fn, scope.parent {
    True, _ -> acc
    False, None -> acc
    False, Some(p) ->
      case { get_scope(tree, p) }.function_scope == stop_at_fn {
        True -> collect_visible(tree, p, stop_at_fn, acc)
        False -> acc
      }
  }
}
