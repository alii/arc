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
    source: raw.source,
    waits: raw.waits,
    free: set.new(),
  )
}

/// the outer names the one function under this root uses, in the order its
/// captures are laid out in. For a function parsed alone, before it is known
/// which of them the code around it supplies
pub fn lone_free_names(
  builder: ScopeBuilder,
  opts: AnalyzeOpts,
) -> List(String) {
  let tree = finalize(builder, opts)
  child_function_scopes(tree, root_scope_id)
  |> list.flat_map(fn(id) { set.to_list(function_info(tree, id).free) })
  |> list.sort(string.compare)
}

/// Forgets everything inside the function `fn_id`, whose parse began at the
/// reading `before` of the builder and has just ended, keeping only what the
/// code around it can tell: the outer names it reads and writes, and its use
/// of this, new.target and the like. The analysis of the code around it comes
/// out as it would with the function whole. Not for a function that holds a
/// direct eval.
pub fn forget_inside(
  builder: ScopeBuilder,
  fn_id: ScopeId,
  before before: ScopeBuilder,
) -> ScopeBuilder {
  let opts = scope.default_analyze_opts()
  let acc =
    finalize_scope(
      builder,
      opts,
      FinalizeState(scopes: dict.new(), functions: dict.new()),
      fn_id,
      dict.new(),
      inherited_strict: False,
    )
  let acc = hoist_annexb_block_functions(builder, acc, opts)
  // nothing outside the function is in this tree, so it is the top of it
  let assert Ok(top) = dict.get(acc.scopes, fn_id)
    as "scope_analysis.forget_inside: the function was not finalized"
  let tree =
    ScopeTree(
      scopes: dict.insert(acc.scopes, fn_id, Scope(..top, parent: None)),
      functions: acc.functions,
      children_at: builder.children_at,
      top_lex: opts.top_lex,
      linker_seeded_exports: opts.linker_seeded_exports,
      inherited_with_stack: opts.with_stack,
    )
  let #(refs, writes, try_writes) =
    scope_builder.recorded_since(builder, before)
  let reads = resolve_own_reads(tree, builder, refs)
  let own_by_fn =
    collect_own_facts(
      tree,
      reads.free,
      builder.own_lexical_refs,
      fn_id,
      dict.new(),
    )
  let subtree_by_fn =
    collect_subtree_facts(
      tree,
      own_by_fn,
      scopes_by_function(tree),
      fn_id,
      dict.new(),
    )
  let facts =
    collect_write_facts(
      tree,
      builder,
      subtree_by_fn,
      reads.arguments,
      writes_by_function(tree, builder, writes, fn(ref) { ref.scope }),
      writes_by_function(tree, builder, try_writes, fn(ref) { ref.scope }),
      fn_id,
      WriteFacts(
        assigned: dict.new(),
        try_assigned: dict.new(),
        refs_arguments: set.new(),
        outward: dict.new(),
      ),
    )
  let outward =
    dict.get(facts.outward, fn_id)
    |> result.unwrap(Outward(writes: dict.new(), try_writes: []))
  scope_builder.forget_inside(
    builder,
    fn_id,
    before:,
    free: set.to_list(subtree_facts(subtree_by_fn, fn_id).transitive_free),
    writes: dict.to_list(outward.writes),
    try_writes: list.map(outward.try_writes, fn(write) {
      case write.1 {
        TryAbove(try_scope) -> #(write.0, Some(try_scope))
        TryHere(_) | TryBelow -> #(write.0, None)
      }
    }),
    lexical_refs: own_facts(own_by_fn, fn_id).lexical_refs,
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
    dict.map_values(opts.parent_names, fn(name, slot) {
      let scope.CaptureSeed(boxed:, declared_kind:) =
        dict.get(opts.parent_seeds, name)
        |> result.unwrap(scope.CaptureSeed(True, parent_declared_kind))
      Binding(slot:, kind: CaptureBinding, boxed:, declared_kind:)
    })
  // with_stack indexes parent_names so it adds no slots
  let root_base =
    dict.size(opts.parent_names) + dict.size(opts.lexical_captures)
  // owner iff root_base == 0, not merely empty lexical_captures
  let script_root_owns_lexical =
    opts.root_may_own_lexical && root_raw.kind == Script && root_base == 0
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
  analyze_captures(
    tree,
    builder,
    box_try_writes: opts.box_try_writes,
    parent_lexical_boxed: opts.lexical_captures_boxed,
  )
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
  use acc, fn_id <- list.fold(dict.keys(acc.functions), acc)
  let raw_fi = scope_builder.fn_info_at(builder, fn_id)
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

// the scope of fn_id that declares name, looking outward from scope_id and
// never past the function's own root
fn declared_within(
  tree: ScopeTree,
  scope_id: ScopeId,
  name: String,
  fn_id: ScopeId,
) -> Option(Scope) {
  let scope = get(tree, scope_id)
  use <- bool.guard(scope.function_scope != fn_id, None)
  case dict.has_key(scope.bindings, name), scope.parent {
    True, _ -> Some(scope)
    False, Some(parent) -> declared_within(tree, parent, name, fn_id)
    False, None -> None
  }
}

type OwnReads {
  OwnReads(
    // per function, the names it reads itself and does not declare
    free: Dict(ScopeId, Set(String)),
    // functions that read an `arguments` they declare themselves
    arguments: Set(ScopeId),
  )
}

// each read is settled inside the function it is written in: either that
// function declares the name or it is free there
fn resolve_own_reads(
  tree: ScopeTree,
  builder: ScopeBuilder,
  refs: List(scope_builder.NameRef),
) -> OwnReads {
  let #(_seen, reads) = {
    use #(seen, reads) as acc, ref <- list.fold(refs, #(
      set.new(),
      OwnReads(free: dict.new(), arguments: set.new()),
    ))
    let seen_before = set.size(seen)
    let seen = set.insert(seen, ref)
    use <- bool.guard(set.size(seen) == seen_before, acc)
    // ref.scope may be a pruned tombstone; start from a live ancestor
    let assert Ok(raw) = dict.get(builder.scopes, ref.scope)
      as "scope_analysis.resolve_own_reads: dangling raw_ref"
    let ref_fn = raw.function_scope
    let declared =
      nearest_finalized(tree, builder, ref.scope)
      |> option.then(declared_within(tree, _, ref.name, ref_fn))
    let reads = case declared, ref.name {
      Some(_), "arguments" ->
        OwnReads(..reads, arguments: set.insert(reads.arguments, ref_fn))
      Some(_), _ -> reads
      None, _ ->
        OwnReads(
          ..reads,
          free: dict.upsert(reads.free, ref_fn, fn(prev) {
            option.unwrap(prev, set.new()) |> set.insert(ref.name)
          }),
        )
    }
    #(seen, reads)
  }
  reads
}

// where the try block of a guarded write is, seen from one function
type TrySide {
  TryHere(ScopeId)
  // in a function further out
  TryAbove(ScopeId)
  // in a function further in, so the write and its try look outward alike
  TryBelow
}

// what a function, with everything inside it, writes that it does not declare
type Outward {
  Outward(
    // the latest order stamp of each name
    writes: Dict(String, ScopeId),
    try_writes: List(#(String, TrySide)),
  )
}

type WriteFacts {
  WriteFacts(
    assigned: Dict(ScopeId, Dict(String, ScopeId)),
    try_assigned: Dict(ScopeId, Set(String)),
    refs_arguments: Set(ScopeId),
    outward: Dict(ScopeId, Outward),
  )
}

// a write, and the live scope the search for its name starts from
type OwnWrite(ref) {
  OwnWrite(start: ScopeId, ref: ref)
}

// writes filed under the function they are written in
fn writes_by_function(
  tree: ScopeTree,
  builder: ScopeBuilder,
  refs: List(ref),
  scope_of: fn(ref) -> ScopeId,
) -> Dict(ScopeId, List(OwnWrite(ref))) {
  use by_fn, ref <- list.fold(refs, dict.new())
  case nearest_finalized(tree, builder, scope_of(ref)) {
    None -> by_fn
    Some(start) -> {
      use prev <- dict.upsert(by_fn, get(tree, start).function_scope)
      [OwnWrite(start:, ref:), ..option.unwrap(prev, [])]
    }
  }
}

// children first: a function settles the writes whose name it declares and
// passes the rest outward, exactly as its own writes
fn collect_write_facts(
  tree: ScopeTree,
  builder: ScopeBuilder,
  subtree_by_fn: Dict(ScopeId, SubtreeFacts),
  own_arguments: Set(ScopeId),
  writes_by_fn: Dict(ScopeId, List(OwnWrite(scope_builder.AssignRef))),
  try_writes_by_fn: Dict(ScopeId, List(OwnWrite(scope_builder.TryAssignRef))),
  fn_id: ScopeId,
  acc: WriteFacts,
) -> WriteFacts {
  let children = child_function_scopes(tree, fn_id)
  let acc =
    list.fold(children, acc, fn(acc, child_id) {
      collect_write_facts(
        tree,
        builder,
        subtree_by_fn,
        own_arguments,
        writes_by_fn,
        try_writes_by_fn,
        child_id,
        acc,
      )
    })
  let outward_of = fn(child_id) {
    dict.get(acc.outward, child_id)
    |> result.unwrap(Outward(writes: dict.new(), try_writes: []))
  }

  let writes = {
    use filed, OwnWrite(start:, ref:) <- list.fold(
      dict.get(writes_by_fn, fn_id) |> result.unwrap([]),
      #(dict.new(), dict.new()),
    )
    file_write(tree, fn_id, start, ref.name, ref.order_stamp, filed)
  }
  let #(assigned_here, writes_out) = {
    use filed, child_id <- list.fold(children, writes)
    case get(tree, child_id).parent {
      None -> filed
      Some(site) -> {
        use filed, name, stamp <- dict.fold(outward_of(child_id).writes, filed)
        file_write(tree, fn_id, site, name, stamp, filed)
      }
    }
  }

  let try_side = fn(try_scope) {
    case dict.get(builder.scopes, try_scope) {
      Ok(raw) if raw.function_scope == fn_id -> TryHere(try_scope)
      Ok(_) | Error(Nil) -> TryAbove(try_scope)
    }
  }
  let try_writes = {
    use filed, OwnWrite(start:, ref:) <- list.fold(
      dict.get(try_writes_by_fn, fn_id) |> result.unwrap([]),
      #(set.new(), []),
    )
    let side = try_side(ref.try_scope)
    file_try_write(tree, builder, fn_id, start, ref.name, side, filed)
  }
  let #(try_assigned_here, try_writes_out) = {
    use filed, child_id <- list.fold(children, try_writes)
    case get(tree, child_id).parent {
      None -> filed
      Some(site) -> {
        use filed, #(name, side) <- list.fold(
          outward_of(child_id).try_writes,
          filed,
        )
        let side = case side {
          TryAbove(try_scope) -> try_side(try_scope)
          TryHere(_) | TryBelow -> TryBelow
        }
        file_try_write(tree, builder, fn_id, site, name, side, filed)
      }
    }
  }

  let reads_arguments =
    set.contains(own_arguments, fn_id)
    || list.any(children, fn(child_id) {
      let free = subtree_facts(subtree_by_fn, child_id).transitive_free
      case set.contains(free, "arguments"), get(tree, child_id).parent {
        True, Some(site) ->
          option.is_some(declared_within(tree, site, "arguments", fn_id))
        _, _ -> False
      }
    })

  WriteFacts(
    assigned: case dict.is_empty(assigned_here) {
      True -> acc.assigned
      False -> dict.insert(acc.assigned, fn_id, assigned_here)
    },
    try_assigned: case set.is_empty(try_assigned_here) {
      True -> acc.try_assigned
      False -> dict.insert(acc.try_assigned, fn_id, try_assigned_here)
    },
    refs_arguments: case reads_arguments {
      True -> set.insert(acc.refs_arguments, fn_id)
      False -> acc.refs_arguments
    },
    outward: dict.insert(
      acc.outward,
      fn_id,
      Outward(writes: writes_out, try_writes: try_writes_out),
    ),
  )
}

// filed is #(names this function declares, names it passes outward), each
// with the latest order stamp
fn file_write(
  tree: ScopeTree,
  fn_id: ScopeId,
  start: ScopeId,
  name: String,
  stamp: ScopeId,
  filed: #(Dict(String, ScopeId), Dict(String, ScopeId)),
) -> #(Dict(String, ScopeId), Dict(String, ScopeId)) {
  let #(here, out) = filed
  let latest = fn(prev) { int.max(stamp, option.unwrap(prev, 0)) }
  case declared_within(tree, start, name, fn_id) {
    Some(_) -> #(dict.upsert(here, name, latest), out)
    None -> #(here, dict.upsert(out, name, latest))
  }
}

// a guarded write counts when the name's declaration is outside the try: the
// same scope answers for the write and for the try block
fn file_try_write(
  tree: ScopeTree,
  builder: ScopeBuilder,
  fn_id: ScopeId,
  start: ScopeId,
  name: String,
  side: TrySide,
  filed: #(Set(String), List(#(String, TrySide))),
) -> #(Set(String), List(#(String, TrySide))) {
  let #(here, out) = filed
  case declared_within(tree, start, name, fn_id), side {
    Some(_), TryBelow -> #(set.insert(here, name), out)
    Some(decl), TryHere(try_scope) -> {
      let outer =
        nearest_finalized(tree, builder, try_scope)
        |> option.then(declared_within(tree, _, name, fn_id))
      case outer {
        Some(outer) if outer.id == decl.id -> #(set.insert(here, name), out)
        Some(_) | None -> filed
      }
    }
    Some(_), TryAbove(_) -> filed
    None, TryAbove(_) -> #(here, [#(name, side), ..out])
    None, TryHere(_) | None, TryBelow -> #(here, [#(name, TryBelow), ..out])
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
    // bindings of the parent's scopes open at the creation site, innermost first
    visible: List(Dict(String, Binding)),
    capture_slots: Dict(String, Int),
    inherited: NameCaptures,
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
  parent_lexical_boxed parent_lexical_boxed: lexical.LexicalRefs,
) -> ScopeTree {
  let reads = resolve_own_reads(tree, builder, builder.raw_refs)
  let own_by_fn =
    collect_own_facts(
      tree,
      reads.free,
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
  let writes =
    collect_write_facts(
      tree,
      builder,
      subtree_by_fn,
      reads.arguments,
      writes_by_function(tree, builder, builder.assign_refs, fn(ref) {
        ref.scope
      }),
      case box_try_writes {
        True ->
          writes_by_function(tree, builder, builder.try_assign_refs, fn(ref) {
            ref.scope
          })
        False -> dict.new()
      },
      root_scope_id,
      WriteFacts(
        assigned: dict.new(),
        try_assigned: dict.new(),
        refs_arguments: set.new(),
        outward: dict.new(),
      ),
    )
  let analysis =
    CaptureAnalysis(
      own_by_fn:,
      subtree_by_fn:,
      scopes_by_fn:,
      assigned: writes.assigned,
      try_assigned: writes.try_assigned,
      refs_arguments: writes.refs_arguments,
      fn_decls: fn_decl_scopes(builder),
    )
  let root_parent =
    ParentView(
      visible: [],
      capture_slots: dict.new(),
      inherited: NameCaptures(
        ordered: [],
        consts: set.new(),
        fn_names: set.new(),
        lets: set.new(),
      ),
      lexical_available: lexical.no_lexical_refs,
      lexical_boxed: parent_lexical_boxed,
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
  // a child's free name declared where the child opens is not free here
  let free_in_children = {
    use free, child_id, visible <- fold_child_functions(
      tree,
      children,
      init: set.new(),
      per_site: fn(_free, child_id) { visible_at_creation(tree, child_id) },
    )
    subtree_facts(acc, child_id).transitive_free
    |> set.filter(fn(name) { option.is_none(visible_binding(visible, name)) })
    |> set.union(free, _)
  }
  let transitive_free =
    own_facts(own_by_fn, fn_id).free_own
    |> set.union(free_in_children)
    |> set.union(fn_with_stack_free(tree, fn_id, own_scopes))
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
  let children = child_function_scopes(tree, fn_id)
  use <- bool.guard(children == [], tree)
  let capture_slots =
    list.index_map(plan.name_captures.ordered, fn(c, i) { #(c.name, i) })
    |> dict.from_list
  use tree, child_id, view <- fold_child_functions(
    tree,
    children,
    init: tree,
    per_site: fn(tree, child_id) {
      ParentView(
        visible: visible_at_creation(tree, child_id),
        capture_slots:,
        inherited: plan.name_captures,
        lexical_available: plan.layout.available,
        lexical_boxed: plan.layout.lexical_boxed,
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
        parent,
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
    free: subtree.transitive_free,
  )
}

fn derive_name_captures(
  subtree: SubtreeFacts,
  parent: ParentView,
) -> NameCaptures {
  let slots = case subtree.eval_in_subtree {
    True -> {
      use name <- list.map(set.to_list(parent_names(parent)))
      let assert Some(slot) = parent_slot(parent, name)
        as "scope_analysis.captures: captured name absent from parent view"
      #(name, slot)
    }
    False -> {
      use slots, name <- set.fold(subtree.transitive_free, [])
      case parent_slot(parent, name) {
        Some(slot) -> [#(name, slot), ..slots]
        None -> slots
      }
    }
  }
  let ordered =
    list.sort(slots, fn(a, b) { string.compare(a.0, b.0) })
    |> list.map(fn(slot) { NameCapture(name: slot.0, parent_slot: slot.1) })
  // by declared kind, not kind, so a captured const stays const
  let none = set.new()
  let #(consts, fn_names, lets) = {
    use #(consts, fn_names, lets) as kinds, #(name, _slot) <- list.fold(
      slots,
      #(none, none, none),
    )
    case parent_declared_kind(parent, name) {
      ConstBinding -> #(set.insert(consts, name), fn_names, lets)
      FnNameBinding -> #(consts, set.insert(fn_names, name), lets)
      LetBinding -> #(consts, fn_names, set.insert(lets, name))
      VarBinding | ParamBinding | CaptureBinding -> kinds
    }
  }
  NameCaptures(ordered:, consts:, fn_names:, lets:)
}

fn visible_binding(
  visible: List(Dict(String, Binding)),
  name: String,
) -> Option(Binding) {
  case visible {
    [] -> None
    [bindings, ..outer] ->
      case dict.get(bindings, name) {
        Ok(b) -> Some(b)
        Error(Nil) -> visible_binding(outer, name)
      }
  }
}

fn parent_slot(parent: ParentView, name: String) -> Option(Int) {
  case visible_binding(parent.visible, name) {
    Some(b) -> Some(b.slot)
    None -> dict.get(parent.capture_slots, name) |> option.from_result
  }
}

fn parent_declared_kind(parent: ParentView, name: String) -> BindingKind {
  case visible_binding(parent.visible, name) {
    Some(b) -> b.declared_kind
    None -> captured_declared_kind(parent.inherited, name)
  }
}

fn parent_boxes(parent: ParentView, name: String) -> Bool {
  visible_binding(parent.visible, name)
  |> option.map(fn(b) { b.boxed })
  |> option.unwrap(False)
}

fn parent_names(parent: ParentView) -> Set(String) {
  use names, bindings <- list.fold(
    parent.visible,
    set.from_list(dict.keys(parent.capture_slots)),
  )
  dict.fold(bindings, names, fn(names, name, _b) { set.insert(names, name) })
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
  let own_scopes = fn() {
    fn_member_scopes(analysis.scopes_by_fn, fn_id)
    |> list.map(get(tree, _))
  }
  let forced_box =
    case fn_id == root_scope_id {
      True -> tree.linker_seeded_exports
      False -> set.new()
    }
    |> set.union(
      dict.get(analysis.try_assigned, fn_id) |> result.unwrap(set.new()),
    )
  let vars_to_box = case subtree.eval_in_subtree, children {
    True, _ -> declared_in(own_scopes())
    // nothing below to capture anything
    False, [] -> set.new()
    False, _ -> {
      let own_scopes = own_scopes()
      let declared = declared_in(own_scopes)
      let never_box = never_box_names(analysis, fn_id, own_scopes, children)
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
  let #(hoisted_free, first_capturer) = {
    use #(hoisted, first), child_id <- list.fold(children, #(
      set.new(),
      dict.new(),
    ))
    let free = subtree_facts(analysis.subtree_by_fn, child_id).transitive_free
    case set.contains(analysis.fn_decls, child_id) {
      True -> #(set.union(hoisted, free), first)
      False -> {
        let first = {
          use first, name <- set.fold(free, first)
          use <- bool.guard(!dict.has_key(assigned_here, name), first)
          use prev <- dict.upsert(first, name)
          option.map(prev, int.min(_, child_id)) |> option.unwrap(child_id)
        }
        #(hoisted, first)
      }
    }
  }
  // const by value only if every capturer opens after its write
  let const_settled = fn(name) {
    case dict.get(assigned_here, name) {
      Error(Nil) -> False
      Ok(last_write) ->
        !set.contains(hoisted_free, name)
        && dict.get(first_capturer, name)
        |> result.map(fn(first) { first >= last_write })
        |> result.unwrap(True)
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
  parent: ParentView,
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
  let var_boundary_bindings =
    own_scope_ids
    |> list.filter(fn(sid) { scopes_get_or_panic(scopes, sid).is_var_boundary })
    |> list.map(scope_bindings)
  let names_shadowed = fn(name) {
    root_shadowed(name)
    || list.any(var_boundary_bindings, dict.has_key(_, name))
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
          boxed: parent_boxes(parent, name),
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
          // a captured name stays held the way it arrived
          case b.kind != CaptureBinding && set.contains(vars_to_box, name) {
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
  own_scopes: List(Scope),
) -> Set(String) {
  let withs =
    scope.fold_enclosing_withs(
      tree,
      get(tree, fn_id).parent,
      set.new(),
      set.insert,
    )
  use <- bool.guard(set.is_empty(withs), withs)
  set.difference(withs, declared_in(own_scopes))
}

fn visible_at_creation(
  tree: ScopeTree,
  child_fn_id: ScopeId,
) -> List(Dict(String, Binding)) {
  case get(tree, child_fn_id).parent {
    None -> []
    Some(parent_id) ->
      collect_visible(tree, parent_id, get(tree, parent_id).function_scope)
  }
}

fn collect_visible(
  tree: ScopeTree,
  scope_id: ScopeId,
  stop_at_fn: ScopeId,
) -> List(Dict(String, Binding)) {
  let scope = get(tree, scope_id)
  let outer = case scope_id == stop_at_fn, scope.parent {
    True, _ | False, None -> []
    False, Some(p) ->
      case get(tree, p).function_scope == stop_at_fn {
        True -> collect_visible(tree, p, stop_at_fn)
        False -> []
      }
  }
  [scope.bindings, ..outer]
}
