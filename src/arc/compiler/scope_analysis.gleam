import arc/bytecode/lexical.{
  type LexicalRef, type LexicalRefs, type LexicalSlots, ActiveFuncRef,
  HomeObjectRef, NewTargetRef, ThisRef,
}
import arc/compiler/scope.{
  type AnalyzeOpts, type Binding, type BindingKind, type FunctionInfo,
  type GlobalFallthrough, type NameCapture, type Scope, type ScopeId,
  type ScopeKind, type ScopeTree, Binding, Block, CaptureBinding, Catch,
  ClassBody, ClassStaticBlock, ConstBinding, FnNameBinding, Function,
  FunctionInfo, LetBinding, LocalLexical, Module, NameCapture, ParamBinding,
  Scope, ScopeTree, Script, ToEvalEnv, ToGlobal, VarBinding, With,
  child_function_scopes, function_info, get, is_function_kind, root_scope_id,
}
import arc/compiler/scope_builder.{
  type RawFunctionInfo, type RawScope, type ScopeBuilder, FnDeclSource,
  OtherSource, RawScope, SwitchTestSource,
}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

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

pub fn finalize(builder: ScopeBuilder, opts: AnalyzeOpts) -> ScopeTree {
  let root_raw = scope_builder.scope_at(builder, root_scope_id)
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
      ..blank_function_info(
        scope_builder.fn_info_at(builder, root_scope_id),
        opts.fallthrough,
      ),
      local_count: root_local_count,
      lexical: root_lexical,
      lexical_captures: opts.lexical_captures,
      slot_by_name: opts.parent_names,
    )
  let acc =
    FinalizeState(
      scopes: dict.new(),
      functions: dict.from_list([#(root_scope_id, root_fn)]),
    )
  let acc =
    finalize_scope(
      builder,
      opts,
      acc,
      root_scope_id,
      parent_bindings,
      inherited_strict: opts.strict,
    )
  let acc = hoist_annexb_block_functions(builder, acc, opts)
  let tree =
    ScopeTree(
      scopes: acc.scopes,
      functions: acc.functions,
      children_at: builder.children_at,
      top_lex: opts.top_lex,
      linker_seeded_exports: opts.linker_seeded_exports,
      inherited_with_stack: opts.with_stack,
    )
  analyze_captures(tree, builder, box_try_writes: opts.box_try_writes)
}

fn finalize_scope(
  builder: ScopeBuilder,
  opts: AnalyzeOpts,
  acc: FinalizeState,
  scope_id: ScopeId,
  seed_bindings: Dict(String, Binding),
  inherited_strict inherited_strict: Bool,
) -> FinalizeState {
  let raw = scope_builder.scope_at(builder, scope_id)
  let is_strict = raw.is_strict || inherited_strict
  let fn_id = raw.function_scope
  let info = case dict.get(acc.functions, fn_id), is_function_kind(raw.kind) {
    Ok(info), _ -> info
    Error(Nil), True ->
      blank_function_info(
        scope_builder.fn_info_at(builder, scope_id),
        opts.fallthrough,
      )
    Error(Nil), False ->
      panic as "scope_analysis.finalize_scope: function_scope FunctionInfo missing (pre-order invariant violated)"
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
    use #(bindings, info) as folded, #(name, rb) <- list.fold(in_decl_order, #(
      seed_bindings,
      info,
    ))
    let keep_seeded =
      dict.has_key(bindings, name)
      && !{ strict_eval_root && dict.has_key(seed_bindings, name) }
    use <- bool.guard(keep_seeded, folded)
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
      annexb_blocked: set.new(),
      is_strict:,
      is_var_boundary: raw.is_var_boundary,
    )
  let acc =
    FinalizeState(
      scopes: dict.insert(acc.scopes, scope_id, scope),
      functions: dict.insert(acc.functions, fn_id, info),
    )
  use acc, child_id <- list.fold(
    scope_builder.children_newest_first(builder, scope_id),
    acc,
  )
  finalize_scope(
    builder,
    opts,
    acc,
    child_id,
    dict.new(),
    inherited_strict: is_strict,
  )
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
        LetBinding | ConstBinding -> opts.top_lex == LocalLexical
        ParamBinding | CaptureBinding | FnNameBinding -> True
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
  builder: ScopeBuilder,
  acc: FinalizeState,
  opts: AnalyzeOpts,
) -> FinalizeState {
  use acc, fn_id, raw_fi <- dict.fold(builder.functions, acc)
  use <- bool.guard(raw_fi.annexb_candidates == [], acc)
  let var_is_local =
    root_binding_is_local(
      scope_builder.scope_at(builder, fn_id).kind,
      fn_id,
      opts,
      VarBinding,
    )
  use acc, #(block_id, name) <- list.fold(raw_fi.annexb_candidates, acc)
  case annexb_walk_blocked(builder, block_id, fn_id, name) {
    True -> mark_annexb_blocked(acc, block_id, name)
    False -> hoist_annexb_twin(acc, fn_id, name, declare: var_is_local)
  }
}

fn mark_annexb_blocked(
  acc: FinalizeState,
  block_id: ScopeId,
  name: String,
) -> FinalizeState {
  let assert Ok(block) = dict.get(acc.scopes, block_id)
    as "scope_analysis: Annex-B block absent from finalized scopes"
  let block =
    Scope(..block, annexb_blocked: set.insert(block.annexb_blocked, name))
  FinalizeState(..acc, scopes: dict.insert(acc.scopes, block_id, block))
}

fn hoist_annexb_twin(
  acc: FinalizeState,
  fn_id: ScopeId,
  name: String,
  declare declare: Bool,
) -> FinalizeState {
  let assert Ok(info) = dict.get(acc.functions, fn_id)
    as "scope_analysis.hoist_annexb_block_functions: FunctionInfo missing"
  let assert Ok(fn_scope) = dict.get(acc.scopes, fn_id)
    as "scope_analysis.hoist_annexb_block_functions: fn-root Scope missing"
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
    scopes: dict.insert(acc.scopes, fn_id, fn_scope),
    functions: dict.insert(acc.functions, fn_id, info),
  )
}

// would var name be an early error between the block and fn_id
fn annexb_walk_blocked(
  builder: ScopeBuilder,
  from_block: ScopeId,
  fn_id: ScopeId,
  name: String,
) -> Bool {
  case scope_builder.scope_at(builder, from_block).parent {
    None -> False
    Some(parent_id) -> annexb_check_chain(builder, parent_id, fn_id, name)
  }
}

fn annexb_check_chain(
  builder: ScopeBuilder,
  scope_id: ScopeId,
  fn_id: ScopeId,
  name: String,
) -> Bool {
  let raw = scope_builder.scope_at(builder, scope_id)
  use <- bool.guard(annexb_blocked_by(raw, name), True)
  use <- bool.guard(scope_id == fn_id, False)
  case raw.parent {
    None -> False
    Some(parent_id) -> annexb_check_chain(builder, parent_id, fn_id, name)
  }
}

fn annexb_blocked_by(raw: RawScope, name: String) -> Bool {
  case scope_builder.raw_binding_kind(raw, name), raw.kind {
    None, _ -> False
    // §B.3.4 only a simple catch param is var-transparent
    Some(_), Catch -> !raw.catch_param_simple
    Some(LetBinding), _ | Some(ConstBinding), _ | Some(FnNameBinding), _ -> True
    // §B.3.2.1 a same-named formal suppresses the twin
    Some(ParamBinding), _ -> True
    Some(VarBinding), _ | Some(CaptureBinding), _ -> False
  }
}

fn resolve_raw_refs(
  tree: ScopeTree,
  builder: ScopeBuilder,
) -> Set(#(ScopeId, String)) {
  let #(_seen, captured) = {
    use #(seen, captured) as acc, ref <- list.fold(builder.raw_refs, #(
      set.new(),
      set.new(),
    ))
    // ref.scope may be a pruned tombstone; start from a live ancestor
    let assert Ok(raw) = dict.get(builder.scopes, ref.scope)
      as "scope_analysis.resolve_raw_refs: dangling raw_ref"
    use <- bool.guard(set.contains(seen, ref), acc)
    let seen = set.insert(seen, ref)
    let ref_fn = raw.function_scope
    case declaring_scope(tree, builder, ref.scope, ref.name) {
      Some(decl) if decl.function_scope == ref_fn -> #(seen, captured)
      Some(_) | None -> #(seen, set.insert(captured, #(ref_fn, ref.name)))
    }
  }
  captured
}

fn resolve_assign_refs(
  tree: ScopeTree,
  builder: ScopeBuilder,
) -> Dict(ScopeId, Dict(String, ScopeId)) {
  use acc, ref <- list.fold(builder.assign_refs, dict.new())
  case declaring_scope(tree, builder, ref.scope, ref.name) {
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
  builder: ScopeBuilder,
) -> Dict(ScopeId, Set(String)) {
  use acc, ref <- list.fold(builder.try_assign_refs, dict.new())
  case
    declaring_scope(tree, builder, ref.scope, ref.name),
    declaring_scope(tree, builder, ref.try_scope, ref.name)
  {
    Some(decl), Some(outer) if decl.id == outer.id ->
      dict.upsert(acc, decl.function_scope, fn(prev) {
        option.unwrap(prev, set.new()) |> set.insert(ref.name)
      })
    _, _ -> acc
  }
}

fn resolve_arguments_refs(
  tree: ScopeTree,
  builder: ScopeBuilder,
) -> Set(ScopeId) {
  use acc, ref <- list.fold(builder.raw_refs, set.new())
  use <- bool.guard(ref.name != "arguments", acc)
  case declaring_scope(tree, builder, ref.scope, ref.name) {
    Some(decl) -> set.insert(acc, decl.function_scope)
    None -> acc
  }
}

// from may be a raw scope pruned before finalize
fn declaring_scope(
  tree: ScopeTree,
  builder: ScopeBuilder,
  from: ScopeId,
  name: String,
) -> Option(Scope) {
  nearest_finalized(tree, builder, from)
  |> option.then(find_declaring_scope(tree, _, name))
}

fn find_declaring_scope(
  tree: ScopeTree,
  scope_id: ScopeId,
  name: String,
) -> Option(Scope) {
  let scope = get(tree, scope_id)
  case dict.has_key(scope.bindings, name), scope.parent {
    True, _ -> Some(scope)
    False, Some(parent) -> find_declaring_scope(tree, parent, name)
    False, None -> None
  }
}

fn nearest_finalized(
  tree: ScopeTree,
  builder: ScopeBuilder,
  scope_id: ScopeId,
) -> Option(ScopeId) {
  use <- bool.guard(dict.has_key(tree.scopes, scope_id), Some(scope_id))
  case dict.get(builder.scopes, scope_id) {
    Ok(RawScope(parent: Some(parent_id), ..)) ->
      nearest_finalized(tree, builder, parent_id)
    Ok(RawScope(parent: None, ..)) | Error(Nil) -> None
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
  builder: ScopeBuilder,
  box_try_writes box_try_writes: Bool,
) -> ScopeTree {
  let captured = resolve_raw_refs(tree, builder)
  let own_by_fn =
    collect_own_facts(
      tree,
      free_names_by_fn(captured),
      builder.own_lexical_refs,
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
    True -> resolve_try_assign_refs(tree, builder)
    False -> dict.new()
  }
  let analysis =
    CaptureAnalysis(
      own_by_fn:,
      subtree_by_fn:,
      scopes_by_fn:,
      assigned: resolve_assign_refs(tree, builder),
      try_assigned:,
      refs_arguments: resolve_arguments_refs(tree, builder),
      fn_decls: fn_decl_scopes(builder),
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

fn fn_decl_scopes(builder: ScopeBuilder) -> Set(ScopeId) {
  use acc, id, raw <- dict.fold(builder.scopes, set.new())
  case raw.source_tag {
    FnDeclSource -> set.insert(acc, id)
    SwitchTestSource | OtherSource -> acc
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
        True -> lexical.refs_or(refs, child_own.lexical_refs)
        False -> refs
      }
    })
  dict.insert(
    acc,
    fn_id,
    OwnFacts(
      is_arrow: function_info(tree, fn_id).is_arrow,
      is_strict: get(tree, fn_id).is_strict,
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
    fn_member_scopes(scopes_by_fn, fn_id) |> list.map(get(tree, _))
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
    let site = get(tree, child_id).parent
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
      as "scope_analysis.captures: captured name absent from parent view"
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
  let kind = get(tree, fn_id).kind
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
        False -> lexical.refs_present(seeded_captures)
      }
      #(seeded_captures, available)
    }
    False, False -> #(dict.new(), lexical.every_lexical_ref)
    False, True -> {
      // one slot per lexical ref this arrow needs and the parent can serve
      let slot_by_ref = {
        use ref <- lexical.number_refs(from: name_capture_count)
        let needed =
          subtree.eval_in_subtree || lexical.refs_get(own.lexical_refs, ref)
        needed && lexical.refs_get(parent.lexical_available, ref)
      }
      #(slot_by_ref, lexical.refs_present(slot_by_ref))
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
        lexical.captured_slots(
          this: captured(ThisRef),
          active_func: captured(ActiveFuncRef),
          home_object: captured(HomeObjectRef),
          new_target: captured(NewTargetRef),
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
      lexical.refs_and(
        lexical.refs_present(lexical_captures),
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
    |> list.map(get(tree, _))
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
      FnNameBinding ->
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
  // an own binding shadows a capture; counts the var-boundary body block too
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
    as "scope_analysis.scopes_get_or_panic: unknown ScopeId"
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
    as "scope_analysis.analyze_captures: no OwnFacts for function scope"
  own
}

fn subtree_facts(
  subtree_by_fn: Dict(ScopeId, SubtreeFacts),
  fn_id: ScopeId,
) -> SubtreeFacts {
  let assert Ok(facts) = dict.get(subtree_by_fn, fn_id)
    as "scope_analysis.analyze_captures: no SubtreeFacts for function scope"
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
  scope.fold_enclosing_withs(
    tree,
    get(tree, fn_id).parent,
    set.new(),
    set.insert,
  )
  |> set.difference(declared)
}

fn visible_at_creation(
  tree: ScopeTree,
  child_fn_id: ScopeId,
) -> Dict(String, Binding) {
  case get(tree, child_fn_id).parent {
    None -> dict.new()
    Some(parent_id) -> {
      let parent_fn = get(tree, parent_id).function_scope
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
  let scope = get(tree, scope_id)
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
      case get(tree, p).function_scope == stop_at_fn {
        True -> collect_visible(tree, p, stop_at_fn, acc)
        False -> acc
      }
  }
}
