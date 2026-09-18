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
  // §13.2.5.5 nfe self name; sloppy writes are silently dropped
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

// declared_kind survives capture so a captured const stays const
pub type Binding {
  Binding(slot: Int, kind: BindingKind, boxed: Bool, declared_kind: BindingKind)
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
    // §10.2.11 step 28 body block of a non-simple-params function
    is_var_boundary: Bool,
  )
}

pub type NameCapture {
  NameCapture(name: String, parent_slot: Int)
}

pub type FunctionInfo {
  FunctionInfo(
    local_count: Int,
    lexical: LexicalSlots,
    lexical_boxed: LexicalRefs,
    captures: List(NameCapture),
    lexical_captures: Dict(LexicalRef, Int),
    slot_by_name: Dict(String, Int),
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
    linker_seeded_exports: Set(String),
    // direct eval caller's with holders as root slots, innermost first
    inherited_with_stack: List(Int),
  )
}

pub type Direct {
  Local(slot: Int, boxed: Bool, kind: BindingKind, declared_kind: BindingKind)
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
    linker_seeded_exports: Set(String),
    // sloppy script root vars get real slots; measured slower, off by default
    module_slot_globals: Bool,
    // box bindings declared outside a try and written inside it
    box_try_writes: Bool,
    // slot indices into parent_names, innermost first
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
    linker_seeded_exports: set.new(),
    module_slot_globals: False,
    box_try_writes: False,
    with_stack: [],
  )
}

pub type RawBinding {
  RawBinding(kind: BindingKind, synthetic: Bool, decl_order: Int)
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
  is_strict: Bool,
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
    source_tag: TagOther,
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
pub fn sb_push_with(sb: ScopeBuilder) -> #(ScopeBuilder, ScopeId) {
  let holder = with_object_name(sb_with_depth(sb), sb.next_id)
  let #(sb, id) = sb_push(sb, With(holder:))
  #(sb_declare(sb, holder, LetBinding, synthetic: True), id)
}

// §10.2.11 step 28 body scope for a non-simple parameter list
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

// first declaration wins
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
  ScopeBuilder(..sb, raw_refs: [NameRef(sb.current, name), ..sb.raw_refs])
}

pub fn sb_assign_ref(sb: ScopeBuilder, name: String) -> ScopeBuilder {
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

pub fn sb_children_newest_first(
  sb: ScopeBuilder,
  id: ScopeId,
) -> List(ScopeId) {
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

pub fn sb_insert_param_shims(sb: ScopeBuilder, count: Int) -> ScopeBuilder {
  use <- bool.guard(count <= 0, sb)
  let fn_id = sb.current_fn
  let scope = sb_scope(sb, fn_id)
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

pub fn sb_discard(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
  let scope = sb_scope(sb, id)
  let children_at = case scope.parent {
    Some(pid) -> {
      let siblings = sb_children_newest_first(sb, pid)
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

// v8 finalize_block_scope: splice out a block with no bindings
fn sb_prune_empty_block(sb: ScopeBuilder, id: ScopeId) -> ScopeBuilder {
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
      let own_children = sb_children_newest_first(sb, id)
      let spliced = case sb_children_newest_first(sb, parent_id) {
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

// children opened under parent_id after the before snapshot, newest first
pub fn sb_children_since(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  before: List(ScopeId),
) -> List(ScopeId) {
  let now = sb_children_newest_first(sb, parent_id)
  list.take(now, list.length(now) - list.length(before))
}

pub fn sb_tag_children_since(
  sb: ScopeBuilder,
  parent_id: ScopeId,
  before: List(ScopeId),
  tag: SourceTag,
) -> ScopeBuilder {
  use sb, id <- list.fold(sb_children_since(sb, parent_id, before), sb)
  sb_set_source_tag(sb, id, tag)
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

// scopes opened before the body keep their place ahead of it
pub fn sb_reorder_body_children(
  sb: ScopeBuilder,
  scope_id: ScopeId,
  before: List(ScopeId),
) -> ScopeBuilder {
  use <- bool.guard(sb_children_newest_first(sb, scope_id) == [], sb)
  let body_src = sb_children_since(sb, scope_id, before) |> list.reverse
  let #(fn_decls, rest) =
    list.partition(body_src, fn(id) { sb_tag_of(sb, id) == TagFnDecl })
  sb_set_children(
    sb,
    scope_id,
    list.flatten([list.reverse(before), fn_decls, rest]),
  )
}

// order: case-body fn decls, case-test scopes, other case-body scopes
pub fn sb_reorder_switch_children(
  sb: ScopeBuilder,
  switch_id: ScopeId,
) -> ScopeBuilder {
  let rev = sb_children_newest_first(sb, switch_id)
  use <- bool.guard(rev == [], sb)
  let src_order = list.reverse(rev)
  let #(fn_decls, non_decl) =
    list.partition(src_order, fn(id) { sb_tag_of(sb, id) == TagFnDecl })
  let #(tests, rest) =
    list.partition(non_decl, fn(id) { sb_tag_of(sb, id) == TagSwitchTest })
  sb_set_children(sb, switch_id, list.flatten([fn_decls, tests, rest]))
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

// nfe self name excluded, a var of that name is legal
fn is_lexical_kind(kind: BindingKind) -> Bool {
  case kind {
    LetBinding | ConstBinding -> True
    VarBinding | ParamBinding | CatchBinding | CaptureBinding | FnNameBinding ->
      False
  }
}

fn raw_binding_kind(scope: RawScope, name: String) -> Option(BindingKind) {
  case dict.get(scope.bindings, name) {
    Ok(rb) -> Some(rb.kind)
    Error(Nil) -> None
  }
}

// §14.2.1 duplicate lexical declaration check
pub fn sb_lexical_conflict(sb: ScopeBuilder, name: String) -> Bool {
  let scope = sb_scope(sb, sb.current)
  dict.has_key(scope.bindings, name)
  || set.contains(scope.hoisted_vars, name)
  || sb_boundary_param_conflict(sb, scope, name)
}

// §15.2.1 / §14.15.1 body lexical names vs param or catch names
fn sb_boundary_param_conflict(
  sb: ScopeBuilder,
  scope: RawScope,
  name: String,
) -> Bool {
  case scope.kind, scope.parent {
    Block, Some(parent_id) -> {
      let parent = sb_scope(sb, parent_id)
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
  raw_binding_kind(sb_scope(sb, sb.current), name) == Some(kind)
}

pub fn sb_var_conflicts_lexical(sb: ScopeBuilder, name: String) -> Bool {
  use _id, scope, _acc <- sb_fold_up(
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
  use _id, scope, _acc <- sb_fold_up(
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

fn sb_with_depth(sb: ScopeBuilder) -> Int {
  use _id, scope, acc <- sb_fold_up(
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

type FinalizeState {
  FinalizeState(
    scopes: Dict(ScopeId, Scope),
    functions: Dict(ScopeId, FunctionInfo),
  )
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
    slot_by_name: dict.new(),
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
  let parent_declared_kind = case root_raw.kind {
    Module -> ConstBinding
    Script
    | Function
    | Block
    | Catch
    | With(_)
    | ClassBody
    | ClassStaticBlock -> CaptureBinding
  }
  let parent_bindings =
    dict.map_values(opts.parent_names, fn(_name, slot) {
      Binding(
        slot:,
        kind: CaptureBinding,
        boxed: True,
        declared_kind: parent_declared_kind,
      )
    })
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
      ..blank_function_info(sb_fn_info(sb, root_scope_id), opts.fallthrough),
      local_count: root_local_count,
      lexical: root_lexical,
      lexical_captures: opts.lexical_captures,
      slot_by_name: opts.parent_names,
    )
  let st =
    FinalizeState(
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
      linker_seeded_exports: opts.linker_seeded_exports,
      inherited_with_stack: opts.with_stack,
    )
  analyze_captures(tree, sb, box_try_writes: opts.box_try_writes)
}

fn finalize_scope(
  sb: ScopeBuilder,
  opts: AnalyzeOpts,
  st: FinalizeState,
  scope_id: ScopeId,
  seed_bindings: Dict(String, Binding),
  inherited_strict: Bool,
) -> FinalizeState {
  let raw = sb_scope(sb, scope_id)
  let is_strict = raw.is_strict || inherited_strict
  let fn_id = raw.function_scope
  let info = case dict.get(st.functions, fn_id), is_function_kind(raw.kind) {
    Ok(info), _ -> info
    Error(Nil), True ->
      blank_function_info(sb_fn_info(sb, scope_id), opts.fallthrough)
    Error(Nil), False ->
      panic as "scope.finalize_scope: function_scope FunctionInfo missing (pre-order invariant violated)"
  }
  let in_decl_order =
    raw.bindings
    |> dict.to_list
    |> list.filter(fn(entry) {
      root_binding_is_local(raw.kind, scope_id, opts, entry.1.kind)
    })
    |> list.sort(fn(a, b) { int.compare(a.1.decl_order, b.1.decl_order) })
  // seeds win, except names a strict direct eval declares itself
  let strict_eval_root = opts.strict && raw.kind == Script
  let #(bindings, info) = {
    use #(bindings, info) as acc, #(name, rb) <- list.fold(in_decl_order, #(
      seed_bindings,
      info,
    ))
    let keep_seeded =
      dict.has_key(bindings, name)
      && !{ strict_eval_root && dict.has_key(seed_bindings, name) }
    use <- bool.guard(keep_seeded, acc)
    // §10.2.11 step 28: user formals past the shims become let (tdz)
    let past_shims =
      raw.non_simple_shim_count > 0
      && rb.decl_order >= raw.non_simple_shim_count
    let kind = case rb.kind, past_shims {
      ParamBinding, True -> LetBinding
      kind, _ -> kind
    }
    let #(binding, info) = push_named_slot(info, name, kind)
    #(dict.insert(bindings, name, binding), info)
  }
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
    FinalizeState(
      scopes: dict.insert(st.scopes, scope_id, scope),
      functions: dict.insert(st.functions, fn_id, info),
    )
  use st, child_id <- list.fold(sb_children_newest_first(sb, scope_id), st)
  finalize_scope(sb, opts, st, child_id, dict.new(), is_strict)
}

fn push_named_slot(
  info: FunctionInfo,
  name: String,
  kind: BindingKind,
) -> #(Binding, FunctionInfo) {
  let slot = info.local_count
  let slot_by_name = case dict.has_key(info.slot_by_name, name) {
    True -> info.slot_by_name
    False -> dict.insert(info.slot_by_name, name, slot)
  }
  #(
    Binding(slot:, kind:, boxed: False, declared_kind: kind),
    FunctionInfo(..info, local_count: slot + 1, slot_by_name:),
  )
}

// sloppy script root vars and repl lexicals get no local slot
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

// §B.3.2-6 annex b var twins, decided once the whole body is known
fn hoist_annexb_block_functions(
  sb: ScopeBuilder,
  st: FinalizeState,
  opts: AnalyzeOpts,
) -> FinalizeState {
  use st, fn_id, raw_fi <- dict.fold(sb.functions, st)
  use <- bool.guard(raw_fi.annexb_candidates == [], st)
  let var_is_local =
    root_binding_is_local(sb_scope(sb, fn_id).kind, fn_id, opts, VarBinding)
  use st, #(block_id, name) <- list.fold(raw_fi.annexb_candidates, st)
  case annexb_walk_blocked(sb, block_id, fn_id, name) {
    True -> mark_annexb_blocked(st, block_id, name)
    False -> hoist_annexb_twin(st, fn_id, name, declare: var_is_local)
  }
}

fn mark_annexb_blocked(
  st: FinalizeState,
  block_id: ScopeId,
  name: String,
) -> FinalizeState {
  let assert Ok(block) = dict.get(st.scopes, block_id)
    as "scope: Annex-B block absent from finalized scopes"
  let block =
    Scope(..block, annexb_blocked: set.insert(block.annexb_blocked, name))
  FinalizeState(..st, scopes: dict.insert(st.scopes, block_id, block))
}

fn hoist_annexb_twin(
  st: FinalizeState,
  fn_id: ScopeId,
  name: String,
  declare declare: Bool,
) -> FinalizeState {
  let assert Ok(info) = dict.get(st.functions, fn_id)
    as "scope.hoist_annexb_block_functions: FunctionInfo missing"
  let assert Ok(fn_scope) = dict.get(st.scopes, fn_id)
    as "scope.hoist_annexb_block_functions: fn-root Scope missing"
  let #(fn_scope, info) = case
    declare && !dict.has_key(fn_scope.bindings, name)
  {
    False -> #(fn_scope, info)
    True -> {
      let #(binding, info) = push_named_slot(info, name, VarBinding)
      let bindings = dict.insert(fn_scope.bindings, name, binding)
      #(Scope(..fn_scope, bindings:), info)
    }
  }
  let info =
    FunctionInfo(..info, annexb_candidates: [name, ..info.annexb_candidates])
  FinalizeState(
    scopes: dict.insert(st.scopes, fn_id, fn_scope),
    functions: dict.insert(st.functions, fn_id, info),
  )
}

// would var name be an early error between the block and fn_id
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
  use <- bool.guard(annexb_blocked_by(raw, name), True)
  use <- bool.guard(scope_id == fn_id, False)
  case raw.parent {
    None -> False
    Some(parent_id) -> annexb_check_chain(sb, parent_id, fn_id, name)
  }
}

fn annexb_blocked_by(raw: RawScope, name: String) -> Bool {
  case raw_binding_kind(raw, name), raw.kind {
    None, _ -> False
    // §B.3.4 only a simple catch param is var-transparent
    Some(_), Catch -> !raw.catch_param_simple
    Some(LetBinding), _ | Some(ConstBinding), _ | Some(FnNameBinding), _ -> True
    // §B.3.2.1 a same-named formal suppresses the twin
    Some(ParamBinding), _ -> True
    // unreachable, catch params are recorded as param kind
    Some(CatchBinding), _ -> False
    Some(VarBinding), _ | Some(CaptureBinding), _ -> False
  }
}

fn resolve_raw_refs(
  tree: ScopeTree,
  sb: ScopeBuilder,
) -> Set(#(ScopeId, String)) {
  let #(_seen, captured) = {
    use #(seen, captured) as acc, ref <- list.fold(sb.raw_refs, #(
      set.new(),
      set.new(),
    ))
    // ref.scope may be a pruned tombstone; start from a live ancestor
    let assert Ok(raw) = dict.get(sb.scopes, ref.scope)
      as "scope.resolve_raw_refs: dangling raw_ref"
    use <- bool.guard(set.contains(seen, ref), acc)
    let seen = set.insert(seen, ref)
    let ref_fn = raw.function_scope
    case declaring_scope(tree, sb, ref.scope, ref.name) {
      Some(decl) if decl.function_scope == ref_fn -> #(seen, captured)
      Some(_) | None -> #(seen, set.insert(captured, #(ref_fn, ref.name)))
    }
  }
  captured
}

fn resolve_assign_refs(
  tree: ScopeTree,
  sb: ScopeBuilder,
) -> Dict(ScopeId, Dict(String, ScopeId)) {
  use acc, ref <- list.fold(sb.assign_refs, dict.new())
  case declaring_scope(tree, sb, ref.scope, ref.name) {
    None -> acc
    Some(decl) ->
      dict.upsert(acc, decl.function_scope, fn(prev) {
        option.unwrap(prev, dict.new())
        |> dict.upsert(ref.name, fn(last) {
          int.max(ref.order_stamp, option.unwrap(last, 0))
        })
      })
  }
}

fn resolve_try_assign_refs(
  tree: ScopeTree,
  sb: ScopeBuilder,
) -> Dict(ScopeId, Set(String)) {
  use acc, ref <- list.fold(sb.try_assign_refs, dict.new())
  case
    declaring_scope(tree, sb, ref.scope, ref.name),
    declaring_scope(tree, sb, ref.try_scope, ref.name)
  {
    Some(decl), Some(outer) if decl.id == outer.id ->
      dict.upsert(acc, decl.function_scope, fn(prev) {
        option.unwrap(prev, set.new()) |> set.insert(ref.name)
      })
    _, _ -> acc
  }
}

fn resolve_arguments_refs(tree: ScopeTree, sb: ScopeBuilder) -> Set(ScopeId) {
  use acc, ref <- list.fold(sb.raw_refs, set.new())
  use <- bool.guard(ref.name != "arguments", acc)
  case declaring_scope(tree, sb, ref.scope, ref.name) {
    Some(decl) -> set.insert(acc, decl.function_scope)
    None -> acc
  }
}

// from may be a raw scope pruned before finalize
fn declaring_scope(
  tree: ScopeTree,
  sb: ScopeBuilder,
  from: ScopeId,
  name: String,
) -> Option(Scope) {
  nearest_finalized(tree, sb, from)
  |> option.then(find_declaring_scope(tree, _, name))
}

fn find_declaring_scope(
  tree: ScopeTree,
  scope_id: ScopeId,
  name: String,
) -> Option(Scope) {
  let scope = get_scope(tree, scope_id)
  case dict.has_key(scope.bindings, name), scope.parent {
    True, _ -> Some(scope)
    False, Some(parent) -> find_declaring_scope(tree, parent, name)
    False, None -> None
  }
}

fn nearest_finalized(
  tree: ScopeTree,
  sb: ScopeBuilder,
  scope_id: ScopeId,
) -> Option(ScopeId) {
  use <- bool.guard(dict.has_key(tree.scopes, scope_id), Some(scope_id))
  case dict.get(sb.scopes, scope_id) {
    Ok(RawScope(parent: Some(parent_id), ..)) ->
      nearest_finalized(tree, sb, parent_id)
    Ok(RawScope(parent: None, ..)) | Error(Nil) -> None
  }
}

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
    Ok(Binding(slot:, kind:, boxed:, declared_kind:)) -> {
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
      wrap_with_chain(crossed, Local(slot:, boxed:, kind:, declared_kind:))
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
  binding_ref(b)
}

pub fn binding_ref(b: Binding) -> SlotRef {
  SlotRef(slot: b.slot, boxed: b.boxed)
}

// owned slot first, else the capture slot; None at a script or module root
pub fn lexical_slot_in(info: FunctionInfo, ref: LexicalRef) -> Option(Int) {
  case lexical.lexical_slot(info.lexical, ref) {
    Some(slot) -> Some(slot)
    None -> dict.get(info.lexical_captures, ref) |> option.from_result
  }
}

// parent slots for the child's lexical captures, in interp/call.setup_frame order
pub fn lexical_capture_parent_slots(
  child: FunctionInfo,
  parent: FunctionInfo,
) -> List(Int) {
  use ref <- list.filter_map(lexical.all_lexical_refs)
  use <- bool.guard(!dict.has_key(child.lexical_captures, ref), Error(Nil))
  case lexical.lexical_slot(parent.lexical, ref) {
    Some(parent_slot) -> Ok(parent_slot)
    None ->
      panic as "scope analyzer recorded a lexical capture the parent has no slot for"
  }
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

type OwnFacts {
  OwnFacts(
    is_arrow: Bool,
    is_strict: Bool,
    // own refs plus those of nested arrows, which this function serves
    lexical_refs: LexicalRefs,
    free_own: Set(String),
  )
}

type SubtreeFacts {
  SubtreeFacts(
    own_eval: Bool,
    eval_in_subtree: Bool,
    transitive_free: Set(String),
  )
}

type ParentView {
  ParentView(
    slot_by_name: Dict(String, Int),
    name_set: Set(String),
    consts: Set(String),
    fn_names: Set(String),
    lets: Set(String),
    boxed: Set(String),
    lexical_available: LexicalRefs,
    lexical_boxed: LexicalRefs,
  )
}

type CaptureAnalysis {
  CaptureAnalysis(
    own_by_fn: Dict(ScopeId, OwnFacts),
    subtree_by_fn: Dict(ScopeId, SubtreeFacts),
    scopes_by_fn: Dict(ScopeId, List(ScopeId)),
    // per function and name, the order stamp of the latest write
    assigned: Dict(ScopeId, Dict(String, ScopeId)),
    try_assigned: Dict(ScopeId, Set(String)),
    refs_arguments: Set(ScopeId),
    fn_decls: Set(ScopeId),
  )
}

// names captured from the parent; the index in ordered is the capture slot
type NameCaptures {
  NameCaptures(
    ordered: List(NameCapture),
    consts: Set(String),
    fn_names: Set(String),
    lets: Set(String),
  )
}

type LexicalLayout {
  LexicalLayout(
    lexical: LexicalSlots,
    lexical_captures: Dict(LexicalRef, Int),
    lexical_boxed: LexicalRefs,
    // name captures, lexical captures and owned lexical slots, in that order
    capture_slot_count: Int,
    available: LexicalRefs,
    script_root_owns: Bool,
  )
}

type CapturePlan {
  CapturePlan(
    name_captures: NameCaptures,
    layout: LexicalLayout,
    vars_to_box: Set(String),
    fallthrough: GlobalFallthrough,
  )
}

fn analyze_captures(
  tree: ScopeTree,
  sb: ScopeBuilder,
  box_try_writes box_try_writes: Bool,
) -> ScopeTree {
  let captured = resolve_raw_refs(tree, sb)
  let own_by_fn =
    collect_own_facts(
      tree,
      free_names_by_fn(captured),
      sb.own_lexical_refs,
      root_scope_id,
      dict.new(),
    )
  let scopes_by_fn = scopes_by_function(tree)
  let subtree_by_fn =
    collect_subtree_facts(
      tree,
      own_by_fn,
      scopes_by_fn,
      root_scope_id,
      dict.new(),
    )
  let try_assigned = case box_try_writes {
    True -> resolve_try_assign_refs(tree, sb)
    False -> dict.new()
  }
  let analysis =
    CaptureAnalysis(
      own_by_fn:,
      subtree_by_fn:,
      scopes_by_fn:,
      assigned: resolve_assign_refs(tree, sb),
      try_assigned:,
      refs_arguments: resolve_arguments_refs(tree, sb),
      fn_decls: fn_decl_scopes(sb),
    )
  let root_parent =
    ParentView(
      slot_by_name: dict.new(),
      name_set: set.new(),
      consts: set.new(),
      fn_names: set.new(),
      lets: set.new(),
      boxed: set.new(),
      lexical_available: lexical.no_lexical_refs,
      lexical_boxed: lexical.every_lexical_ref,
    )
  assign_captures(tree, analysis, root_scope_id, root_parent)
}

fn fn_decl_scopes(sb: ScopeBuilder) -> Set(ScopeId) {
  use acc, id, raw <- dict.fold(sb.scopes, set.new())
  case raw.source_tag {
    TagFnDecl -> set.insert(acc, id)
    TagSwitchTest | TagOther -> acc
  }
}

fn scopes_by_function(tree: ScopeTree) -> Dict(ScopeId, List(ScopeId)) {
  use d, id, scope <- dict.fold(tree.scopes, dict.new())
  use prev <- dict.upsert(d, scope.function_scope)
  [id, ..option.unwrap(prev, [])]
}

fn fn_member_scopes(
  scopes_by_fn: Dict(ScopeId, List(ScopeId)),
  fn_id: ScopeId,
) -> List(ScopeId) {
  dict.get(scopes_by_fn, fn_id) |> result.unwrap([])
}

fn free_names_by_fn(
  captured: Set(#(ScopeId, String)),
) -> Dict(ScopeId, Set(String)) {
  use d, #(ref_fn, name) <- set.fold(captured, dict.new())
  use prev <- dict.upsert(d, ref_fn)
  option.unwrap(prev, set.new()) |> set.insert(name)
}

fn collect_own_facts(
  tree: ScopeTree,
  free_by_fn: Dict(ScopeId, Set(String)),
  own_lexical_refs: Dict(ScopeId, LexicalRefs),
  fn_id: ScopeId,
  acc: Dict(ScopeId, OwnFacts),
) -> Dict(ScopeId, OwnFacts) {
  let children = child_function_scopes(tree, fn_id)
  let acc =
    list.fold(children, acc, fn(acc, child_id) {
      collect_own_facts(tree, free_by_fn, own_lexical_refs, child_id, acc)
    })
  let own_refs =
    dict.get(own_lexical_refs, fn_id)
    |> result.unwrap(lexical.no_lexical_refs)
  let lexical_refs =
    list.fold(children, own_refs, fn(refs, child_id) {
      let child_own = own_facts(acc, child_id)
      case child_own.is_arrow {
        True -> lexical.lexical_refs_or(refs, child_own.lexical_refs)
        False -> refs
      }
    })
  dict.insert(
    acc,
    fn_id,
    OwnFacts(
      is_arrow: function_info(tree, fn_id).is_arrow,
      is_strict: get_scope(tree, fn_id).is_strict,
      lexical_refs:,
      free_own: dict.get(free_by_fn, fn_id) |> result.unwrap(set.new()),
    ),
  )
}

fn collect_subtree_facts(
  tree: ScopeTree,
  own_by_fn: Dict(ScopeId, OwnFacts),
  scopes_by_fn: Dict(ScopeId, List(ScopeId)),
  fn_id: ScopeId,
  acc: Dict(ScopeId, SubtreeFacts),
) -> Dict(ScopeId, SubtreeFacts) {
  let children = child_function_scopes(tree, fn_id)
  let acc =
    list.fold(children, acc, fn(acc, child_id) {
      collect_subtree_facts(tree, own_by_fn, scopes_by_fn, child_id, acc)
    })
  let own_scopes =
    fn_member_scopes(scopes_by_fn, fn_id) |> list.map(get_scope(tree, _))
  let own_eval = list.any(own_scopes, fn(s) { s.contains_direct_eval })
  let eval_in_subtree =
    own_eval
    || list.any(children, fn(cid) { subtree_facts(acc, cid).eval_in_subtree })
  let declared = declared_in(own_scopes)
  // a child's free name declared where the child opens is not free here
  let free_in_children = {
    use free, child_id, visible_names <- fold_child_functions(
      tree,
      children,
      init: set.new(),
      per_site: fn(_free, child_id) {
        visible_at_creation(tree, child_id) |> dict.keys |> set.from_list
      },
    )
    subtree_facts(acc, child_id).transitive_free
    |> set.difference(visible_names)
    |> set.union(free, _)
  }
  let transitive_free =
    own_facts(own_by_fn, fn_id).free_own
    |> set.union(free_in_children)
    |> set.union(fn_with_stack_free(tree, fn_id, declared))
  dict.insert(
    acc,
    fn_id,
    SubtreeFacts(own_eval:, eval_in_subtree:, transitive_free:),
  )
}

// per_site runs once per creation scope and is shared by siblings opened there
fn fold_child_functions(
  tree: ScopeTree,
  children: List(ScopeId),
  init acc: a,
  per_site per_site: fn(a, ScopeId) -> v,
  with step: fn(a, ScopeId, v) -> a,
) -> a {
  let #(acc, _memo) = {
    use #(acc, memo), child_id <- list.fold(children, #(acc, dict.new()))
    let site = get_scope(tree, child_id).parent
    let #(value, memo) = case dict.get(memo, site) {
      Ok(value) -> #(value, memo)
      Error(Nil) -> {
        let value = per_site(acc, child_id)
        #(value, dict.insert(memo, site, value))
      }
    }
    #(step(acc, child_id, value), memo)
  }
  acc
}

fn assign_captures(
  tree: ScopeTree,
  analysis: CaptureAnalysis,
  fn_id: ScopeId,
  parent: ParentView,
) -> ScopeTree {
  let plan = plan_captures(tree, analysis, fn_id, parent)
  let tree = write_captures(tree, analysis, fn_id, parent, plan)
  use tree, child_id, view <- fold_child_functions(
    tree,
    child_function_scopes(tree, fn_id),
    init: tree,
    per_site: fn(tree, child_id) {
      child_parent_view(
        tree,
        child_id,
        plan.name_captures,
        plan.layout.available,
        plan.layout.lexical_boxed,
      )
    },
  )
  assign_captures(tree, analysis, child_id, view)
}

fn plan_captures(
  tree: ScopeTree,
  analysis: CaptureAnalysis,
  fn_id: ScopeId,
  parent: ParentView,
) -> CapturePlan {
  let own = own_facts(analysis.own_by_fn, fn_id)
  let subtree = subtree_facts(analysis.subtree_by_fn, fn_id)
  let name_captures = derive_name_captures(subtree, parent)
  let layout =
    derive_lexical_layout(
      tree,
      analysis,
      fn_id,
      parent,
      list.length(name_captures.ordered),
    )
  let vars_to_box = derive_vars_to_box(tree, analysis, fn_id)
  let fallthrough =
    derive_fallthrough(
      fn_id == root_scope_id,
      function_info(tree, fn_id).fallthrough,
      subtree,
      own,
    )
  CapturePlan(name_captures:, layout:, vars_to_box:, fallthrough:)
}

fn write_captures(
  tree: ScopeTree,
  analysis: CaptureAnalysis,
  fn_id: ScopeId,
  parent: ParentView,
  plan: CapturePlan,
) -> ScopeTree {
  let CapturePlan(name_captures:, layout:, vars_to_box:, fallthrough:) = plan
  let subtree = subtree_facts(analysis.subtree_by_fn, fn_id)
  let own_scope_ids = fn_member_scopes(analysis.scopes_by_fn, fn_id)
  let tree = case fn_id == root_scope_id || layout.capture_slot_count == 0 {
    True -> tree
    False ->
      insert_captures(
        tree,
        fn_id,
        own_scope_ids,
        layout.capture_slot_count,
        name_captures,
        parent.boxed,
      )
  }
  let tree = apply_boxing(tree, own_scope_ids, vars_to_box)
  use info <- update_function_info(tree, fn_id)
  // an owning script root keeps the lexical finalize seeded
  let lexical = case layout.script_root_owns {
    True -> info.lexical
    False -> layout.lexical
  }
  FunctionInfo(
    ..info,
    captures: name_captures.ordered,
    lexical:,
    lexical_captures: layout.lexical_captures,
    lexical_boxed: layout.lexical_boxed,
    fallthrough:,
    contains_direct_eval: subtree.own_eval,
    eval_in_subtree: subtree.eval_in_subtree,
  )
}

fn derive_name_captures(
  subtree: SubtreeFacts,
  parent: ParentView,
) -> NameCaptures {
  let captured_names = case subtree.eval_in_subtree {
    True -> parent.name_set
    False -> set.intersection(subtree.transitive_free, parent.name_set)
  }
  let ordered = {
    use name <- list.map(
      set.to_list(captured_names) |> list.sort(string.compare),
    )
    let assert Ok(parent_slot) = dict.get(parent.slot_by_name, name)
      as "scope.captures: captured name absent from parent view"
    NameCapture(name:, parent_slot:)
  }
  NameCaptures(
    ordered:,
    consts: set.intersection(parent.consts, captured_names),
    fn_names: set.intersection(parent.fn_names, captured_names),
    lets: set.intersection(parent.lets, captured_names),
  )
}

fn derive_lexical_layout(
  tree: ScopeTree,
  analysis: CaptureAnalysis,
  fn_id: ScopeId,
  parent: ParentView,
  name_capture_count: Int,
) -> LexicalLayout {
  let own = own_facts(analysis.own_by_fn, fn_id)
  let subtree = subtree_facts(analysis.subtree_by_fn, fn_id)
  let is_root = fn_id == root_scope_id
  let kind = get_scope(tree, fn_id).kind
  let seeded = function_info(tree, fn_id)
  let seeded_root_owns_lexical = case seeded.lexical {
    lexical.OwnedLexicalSlots(_) -> True
    lexical.CapturedLexicalSlots(..) | lexical.NoLexicalSlots -> False
  }
  let script_root_owns = is_root && kind == Script && seeded_root_owns_lexical
  let #(lexical_captures, available) = case is_root, own.is_arrow {
    True, _ -> {
      let seeded_captures = seeded.lexical_captures
      let available = case script_root_owns {
        True -> lexical.every_lexical_ref
        False -> lexical.lexical_refs_present(seeded_captures)
      }
      #(seeded_captures, available)
    }
    False, False -> #(dict.new(), lexical.every_lexical_ref)
    False, True -> {
      // one slot per lexical ref this arrow needs and the parent can serve
      let slot_by_ref = {
        use ref <- lexical.number_refs(from: name_capture_count)
        let needed =
          subtree.eval_in_subtree
          || lexical.lexical_refs_get(own.lexical_refs, ref)
        needed && lexical.lexical_refs_get(parent.lexical_available, ref)
      }
      #(slot_by_ref, lexical.lexical_refs_present(slot_by_ref))
    }
  }

  // non-arrow functions and a plain script root own all four slots
  let lexical_base = name_capture_count + dict.size(lexical_captures)
  let owns_lexical = case kind {
    Function -> !own.is_arrow
    ClassStaticBlock -> True
    Script -> script_root_owns
    Module | Block | Catch | With(_) | ClassBody -> False
  }
  let #(slots, own_lexical_count) = case owns_lexical {
    False -> {
      let captured = fn(ref) {
        dict.get(lexical_captures, ref) |> option.from_result
      }
      #(
        lexical.captured_lexical_slots(
          this: captured(RefThis),
          active_func: captured(RefActiveFunc),
          home_object: captured(RefHomeObject),
          new_target: captured(RefNewTarget),
        ),
        0,
      )
    }
    True -> #(
      lexical.OwnedLexicalSlots(base: lexical_base),
      lexical.owned_lexical_slot_count,
    )
  }

  // eval boxes all; derived ctor this boxed if an arrow reads it
  let lexical_boxed = case owns_lexical, subtree.eval_in_subtree {
    _, True -> lexical.every_lexical_ref
    True, False -> {
      let this_captured =
        seeded.is_derived_constructor
        && list.any(child_function_scopes(tree, fn_id), fn(child_id) {
          let child_own = own_facts(analysis.own_by_fn, child_id)
          child_own.is_arrow && child_own.lexical_refs.this
        })
      lexical.LexicalRefs(..lexical.no_lexical_refs, this: this_captured)
    }
    False, False ->
      lexical.lexical_refs_and(
        lexical.lexical_refs_present(lexical_captures),
        parent.lexical_boxed,
      )
  }

  LexicalLayout(
    lexical: slots,
    lexical_captures:,
    lexical_boxed:,
    capture_slot_count: lexical_base + own_lexical_count,
    available:,
    script_root_owns:,
  )
}

fn derive_vars_to_box(
  tree: ScopeTree,
  analysis: CaptureAnalysis,
  fn_id: ScopeId,
) -> Set(String) {
  let subtree = subtree_facts(analysis.subtree_by_fn, fn_id)
  let children = child_function_scopes(tree, fn_id)
  let own_scopes =
    fn_member_scopes(analysis.scopes_by_fn, fn_id)
    |> list.map(get_scope(tree, _))
  let declared = declared_in(own_scopes)
  let forced_box =
    case fn_id == root_scope_id {
      True -> tree.linker_seeded_exports
      False -> set.new()
    }
    |> set.union(
      dict.get(analysis.try_assigned, fn_id) |> result.unwrap(set.new()),
    )
  let never_box = never_box_names(analysis, fn_id, own_scopes, children)
  let vars_to_box = case subtree.eval_in_subtree {
    True -> declared
    False -> {
      let free_in_children = {
        use free, child_id <- list.fold(children, set.new())
        let child = subtree_facts(analysis.subtree_by_fn, child_id)
        case child.eval_in_subtree {
          True -> set.union(free, declared)
          False ->
            set.intersection(child.transitive_free, declared)
            |> set.union(free)
        }
      }
      set.difference(free_in_children, never_box)
    }
  }
  set.union(vars_to_box, forced_box)
}

type ByValueClass {
  NeverWritten
  SettledConst
  Rewritten
  MustBox
}

// names safe to capture by value, deliberately conservative
fn never_box_names(
  analysis: CaptureAnalysis,
  fn_id: ScopeId,
  own_scopes: List(Scope),
  children: List(ScopeId),
) -> Set(String) {
  let own = own_facts(analysis.own_by_fn, fn_id)
  let assigned_here =
    dict.get(analysis.assigned, fn_id) |> result.unwrap(dict.new())
  // sloppy + arguments referenced: arguments[i]=v may write params
  let may_map_args =
    !own.is_strict && set.contains(analysis.refs_arguments, fn_id)
  // const by value only if every capturer opens after its write
  let const_settled = fn(name) {
    case dict.get(assigned_here, name) {
      Error(Nil) -> False
      Ok(last_write) ->
        list.all(children, fn(child_id) {
          let child = subtree_facts(analysis.subtree_by_fn, child_id)
          !set.contains(child.transitive_free, name)
          || {
            child_id >= last_write && !set.contains(analysis.fn_decls, child_id)
          }
        })
    }
  }
  let classified = {
    use acc, scope <- list.fold(own_scopes, [])
    use acc, name, b <- dict.fold(scope.bindings, acc)
    let assigned = dict.has_key(assigned_here, name)
    let class = case b.kind {
      ParamBinding ->
        case may_map_args || assigned {
          False -> NeverWritten
          True -> Rewritten
        }
      CatchBinding | FnNameBinding ->
        case assigned {
          False -> NeverWritten
          True -> Rewritten
        }
      ConstBinding ->
        case const_settled(name) {
          True -> SettledConst
          False -> MustBox
        }
      VarBinding | LetBinding | CaptureBinding -> MustBox
    }
    [#(name, class), ..acc]
  }
  let names = fn(class: ByValueClass) {
    list.filter(classified, fn(entry) { entry.1 == class })
    |> list.map(fn(entry) { entry.0 })
    |> set.from_list
  }
  set.union(
    names(NeverWritten),
    set.difference(names(SettledConst), names(Rewritten)),
  )
  |> set.difference(names(MustBox))
}

fn derive_fallthrough(
  is_root: Bool,
  seeded: GlobalFallthrough,
  subtree: SubtreeFacts,
  own: OwnFacts,
) -> GlobalFallthrough {
  use <- bool.guard(is_root, seeded)
  case subtree.eval_in_subtree && !own.is_strict {
    True -> ToEvalEnv
    False -> ToGlobal
  }
}

fn insert_captures(
  tree: ScopeTree,
  fn_id: ScopeId,
  own_scope_ids: List(ScopeId),
  capture_slot_count: Int,
  name_captures: NameCaptures,
  parent_boxed: Set(String),
) -> ScopeTree {
  let scopes =
    list.fold(own_scope_ids, tree.scopes, fn(scopes, sid) {
      let scope = scopes_get_or_panic(scopes, sid)
      let bindings =
        dict.map_values(scope.bindings, fn(_name, b) {
          Binding(..b, slot: b.slot + capture_slot_count)
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
    let bindings = {
      use bs, NameCapture(name:, ..), i <- list.index_fold(
        name_captures.ordered,
        root.bindings,
      )
      use <- bool.guard(root_shadowed(name), bs)
      dict.insert(
        bs,
        name,
        Binding(
          slot: i,
          kind: CaptureBinding,
          boxed: set.contains(parent_boxed, name),
          declared_kind: captured_declared_kind(name_captures, name),
        ),
      )
    }
    dict.insert(scopes, fn_id, Scope(..root, bindings:))
  }
  let info = function_info(tree, fn_id)
  let slot_by_name = {
    let shifted =
      dict.map_values(info.slot_by_name, fn(_n, slot) {
        slot + capture_slot_count
      })
    use d, NameCapture(name:, ..), i <- list.index_fold(
      name_captures.ordered,
      shifted,
    )
    use <- bool.guard(names_shadowed(name), d)
    dict.insert(d, name, i)
  }
  let functions =
    dict.insert(
      tree.functions,
      fn_id,
      FunctionInfo(
        ..info,
        local_count: info.local_count + capture_slot_count,
        slot_by_name:,
      ),
    )
  ScopeTree(..tree, scopes:, functions:)
}

fn captured_declared_kind(
  name_captures: NameCaptures,
  name: String,
) -> BindingKind {
  use <- bool.guard(set.contains(name_captures.consts, name), ConstBinding)
  use <- bool.guard(set.contains(name_captures.fn_names, name), FnNameBinding)
  case set.contains(name_captures.lets, name) {
    True -> LetBinding
    False -> CaptureBinding
  }
}

fn child_parent_view(
  tree: ScopeTree,
  child_fn_id: ScopeId,
  ours: NameCaptures,
  lexical_available: LexicalRefs,
  lexical_boxed: LexicalRefs,
) -> ParentView {
  let capture_slots =
    list.index_map(ours.ordered, fn(c, i) { #(c.name, i) }) |> dict.from_list
  let own_visible = visible_at_creation(tree, child_fn_id)
  let slot_by_name =
    dict.fold(own_visible, capture_slots, fn(d, name, b) {
      dict.insert(d, name, b.slot)
    })
  let own_names = set.from_list(dict.keys(own_visible))
  // by declared kind, not kind, so a captured const stays const
  let declared_as = fn(kind, inherited) {
    let own =
      dict.fold(own_visible, set.new(), fn(s, name, b) {
        case b.declared_kind == kind {
          True -> set.insert(s, name)
          False -> s
        }
      })
    set.union(own, set.difference(inherited, own_names))
  }
  let boxed =
    dict.fold(own_visible, set.new(), fn(s, name, b) {
      case b.boxed {
        True -> set.insert(s, name)
        False -> s
      }
    })
  ParentView(
    slot_by_name:,
    name_set: set.from_list(dict.keys(slot_by_name)),
    consts: declared_as(ConstBinding, ours.consts),
    fn_names: declared_as(FnNameBinding, ours.fn_names),
    lets: declared_as(LetBinding, ours.lets),
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
            True -> Binding(..b, boxed: True)
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

fn own_facts(own_by_fn: Dict(ScopeId, OwnFacts), fn_id: ScopeId) -> OwnFacts {
  let assert Ok(own) = dict.get(own_by_fn, fn_id)
    as "scope.analyze_captures: no OwnFacts for function scope"
  own
}

fn subtree_facts(
  subtree_by_fn: Dict(ScopeId, SubtreeFacts),
  fn_id: ScopeId,
) -> SubtreeFacts {
  let assert Ok(facts) = dict.get(subtree_by_fn, fn_id)
    as "scope.analyze_captures: no SubtreeFacts for function scope"
  facts
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
  fold_enclosing_withs(
    tree,
    get_scope(tree, fn_id).parent,
    set.new(),
    set.insert,
  )
  |> set.difference(declared)
}

fn visible_at_creation(
  tree: ScopeTree,
  child_fn_id: ScopeId,
) -> Dict(String, Binding) {
  case get_scope(tree, child_fn_id).parent {
    None -> dict.new()
    Some(parent_id) -> {
      let parent_fn = get_scope(tree, parent_id).function_scope
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
    True, _ | False, None -> acc
    False, Some(p) ->
      case get_scope(tree, p).function_scope == stop_at_fn {
        True -> collect_visible(tree, p, stop_at_fn, acc)
        False -> acc
      }
  }
}
