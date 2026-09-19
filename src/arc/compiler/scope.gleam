import arc/bytecode/lexical.{
  type LexicalRef, type LexicalRefs, type LexicalSlots,
}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

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
  GlobalLexical
  LocalLexical
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

pub type BindingTarget {
  Local(slot: Int, boxed: Bool, kind: BindingKind, declared_kind: BindingKind)
  Global(name: String)
  EvalEnv(name: String)
}

pub type SlotRef {
  SlotRef(slot: Int, boxed: Bool)
}

pub type Resolution {
  Plain(target: BindingTarget)
  WithChain(crossed_slots: List(SlotRef), fallback: BindingTarget)
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
    top_lex: LocalLexical,
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

pub fn with_object_name(depth: Int, with_id: ScopeId) -> String {
  "<with" <> int.to_string(depth) <> "_" <> int.to_string(with_id) <> ">"
}

pub fn param_shim(idx: Int) -> String {
  "<param" <> int.to_string(idx) <> ">"
}

pub fn lookup(tree: ScopeTree, scope_id: ScopeId, name: String) -> Resolution {
  lookup_crossing(tree, scope_id, name, [])
}

fn lookup_crossing(
  tree: ScopeTree,
  scope_id: ScopeId,
  name: String,
  crossed: List(SlotRef),
) -> Resolution {
  let scope = get(tree, scope_id)
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
        False, Some(parent_id) ->
          lookup_crossing(tree, parent_id, name, crossed)
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
  case lexical.slot_of(info.lexical, ref) {
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
  case lexical.slot_of(parent.lexical, ref) {
    Some(parent_slot) -> Ok(parent_slot)
    None ->
      panic as "scope analyzer recorded a lexical capture the parent has no slot for"
  }
}

fn wrap_with_chain(
  crossed: List(SlotRef),
  fallback: BindingTarget,
) -> Resolution {
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

pub fn fold_enclosing_withs(
  tree: ScopeTree,
  scope_id: Option(ScopeId),
  acc: a,
  f: fn(a, String) -> a,
) -> a {
  case scope_id {
    None -> acc
    Some(id) -> {
      let scope = get(tree, id)
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
) -> #(Int, ScopeTree) {
  let info = function_info(tree, function_scope_id)
  let slot = info.local_count
  let info = FunctionInfo(..info, local_count: slot + 1)
  let tree =
    ScopeTree(
      ..tree,
      functions: dict.insert(tree.functions, function_scope_id, info),
    )
  #(slot, tree)
}

pub fn function_info(tree: ScopeTree, scope_id: ScopeId) -> FunctionInfo {
  let assert Ok(info) = dict.get(tree.functions, scope_id)
    as "scope.function_info: not a function scope"
  info
}

pub fn get(tree: ScopeTree, scope_id: ScopeId) -> Scope {
  let assert Ok(scope) = dict.get(tree.scopes, scope_id)
    as "scope.get: unknown ScopeId"
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
  let child = get(tree, child_id)
  case is_function_kind(child.kind) {
    True -> [child_id, ..acc]
    False -> collect_child_fns(tree, child_id, acc)
  }
}
