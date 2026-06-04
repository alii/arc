/// Phase 2: Scope Resolution
///
/// Walks the EmitterOp list from Phase 1 and resolves symbolic variable names
/// to local slot indices. Consumes scope markers (EnterScope/LeaveScope/DeclareVar)
/// and replaces IrScopeGetVar/IrScopePutVar/IrScopeTypeofVar with concrete
/// GetLocal/PutLocal/GetGlobal/PutGlobal/TypeofGlobal ops.
///
/// Variables captured by child closures are "boxed" — stored in a heap-allocated
/// BoxSlot. Both the parent and child dereference through the same box, so
/// mutations are visible in both directions (true JS closure semantics).
import arc/compiler/emit.{
  type BindingKind, type EmitterOp, BlockScope, CaptureBinding, CatchBinding,
  ConstBinding, DeclareLexical, DeclareVar, EnterScope, FunctionScope, Ir,
  LeaveScope, LetBinding, ParamBinding, VarBinding,
}
import arc/vm/opcode.{
  type IrOp, type LexicalRef, type LexicalRefs, type LexicalSlots, IrBoxLocal,
  IrDeclareEvalVar, IrDeclareGlobalVar, IrGetBoxed, IrGetEvalVar, IrGetGlobal,
  IrGetLexical, IrGetLocal, IrMakeClosure, IrPushConst, IrPutBoxed,
  IrPutBoxedCheckInit, IrPutEvalVar, IrPutGlobal, IrPutLocal,
  IrPutLocalCheckInit, IrScopeGetVar, IrScopeInitVar, IrScopePutVar,
  IrScopeReboxVar, IrScopeTypeofVar, IrSetThis, IrThrowConstAssign, IrTypeOf,
  IrTypeofEvalVar, IrTypeofGlobal, LexicalSlots, RefActiveFunc, RefHomeObject,
  RefNewTarget, RefThis,
}
import arc/vm/value.{type JsValue, JsUndefined, JsUninitialized}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

// ============================================================================
// Types
// ============================================================================

/// Where unresolved names fall through to. ToGlobal is the normal case.
/// ToEvalEnv is used when compiling sloppy direct-eval code OR sloppy
/// functions that contain a direct eval — those frames carry an eval_env
/// dict that GetEvalVar/PutEvalVar check before the global object.
pub type GlobalFallthrough {
  ToGlobal
  ToEvalEnv
}

/// A binding in a scope — maps name to local slot index.
type Binding {
  Binding(index: Int, kind: BindingKind, is_boxed: Bool)
}

/// A single scope level.
type Scope {
  Scope(kind: emit.ScopeKind, bindings: Dict(String, Binding))
}

/// The scope resolver state.
type Resolver {
  Resolver(
    scopes: List(Scope),
    next_local: Int,
    max_locals: Int,
    output: List(IrOp),
    constants: List(JsValue),
    constants_map: Dict(JsValue, Int),
    next_const: Int,
    /// Names of variables that are captured by child closures.
    /// Variables in this set will be boxed (stored via BoxSlot indirection).
    captured_vars: Set(String),
    /// Module exports the linker pre-allocates a BoxSlot for and seeds into the
    /// slot before the body runs (§16.2 instantiation). Their DeclareVar
    /// reserves the slot and a boxed binding but emits NO init/box op — the
    /// linker owns the cell so it can be shared with importers (incl. cyclic).
    linker_seeded: Set(String),
    fallthrough: GlobalFallthrough,
    /// Bindings for the lexical pseudo-slots (this/active_func/new.target).
    /// Populated by DeclareLexical (owned slot) or pre-seeded as captures
    /// (arrows, direct-eval).
    lexical_bindings: Dict(LexicalRef, Binding),
    /// Lexical refs that an inner closure (or direct eval in this subtree)
    /// captures — DeclareLexical will box those slots so writes (super())
    /// and reads through arrows alias the same cell.
    lexical_captured: LexicalRefs,
    /// Snapshot of name→slot visible at each IrMakeClosure(idx). The single
    /// source of truth for child capture indices — replaces compiler.gleam's
    /// build_scope_dict, which diverged on block-scoped shadows.
    closure_scopes: Dict(Int, Dict(String, Int)),
    /// Every name ever allocated a slot in this body (cumulative, across
    /// closed block scopes). Used for module-export lookup and direct-eval
    /// local_names.
    names: Dict(String, Int),
  )
}

/// Result of scope resolution.
pub type Resolved {
  Resolved(
    code: List(IrOp),
    local_count: Int,
    constants: List(JsValue),
    constants_map: Dict(JsValue, Int),
    /// Local-slot indices for the lexical pseudo-bindings — owned
    /// (DeclareLexical) or inherited as captures.
    lexical: LexicalSlots,
    /// name→slot visible at each `IrMakeClosure(i)` — keyed by func_index.
    closure_scopes: Dict(Int, Dict(String, Int)),
    /// All names ever allocated a slot in this body.
    names: Dict(String, Int),
  )
}

// ============================================================================
// Public API
// ============================================================================

/// Resolve scopes in a list of EmitterOps. Top-level (no captures) entry.
pub fn resolve(
  code: List(EmitterOp),
  constants: List(JsValue),
  constants_map: Dict(JsValue, Int),
  captured_vars: Set(String),
  lexical_captured: LexicalRefs,
  fallthrough: GlobalFallthrough,
) -> Resolved {
  resolve_with_captures(
    code,
    constants,
    constants_map,
    [],
    dict.new(),
    captured_vars,
    set.new(),
    lexical_captured,
    fallthrough,
  )
}

/// Resolve scopes with pre-populated capture bindings.
/// Named captures occupy local slots 0..len-1. `lexical_captures` maps each
/// inherited lexical binding (arrows, direct-eval) to its capture-slot index;
/// caller must size env_descriptors/seed-locals to match. Absent refs come
/// from a `DeclareLexical` in `code` (or are unreferenced).
pub fn resolve_with_captures(
  code: List(EmitterOp),
  constants: List(JsValue),
  constants_map: Dict(JsValue, Int),
  captures: List(String),
  lexical_captures: Dict(LexicalRef, Int),
  captured_vars: Set(String),
  linker_seeded: Set(String),
  lexical_captured: LexicalRefs,
  fallthrough: GlobalFallthrough,
) -> Resolved {
  let capture_bindings =
    list.index_map(captures, fn(name, idx) {
      #(name, Binding(index: idx, kind: CaptureBinding, is_boxed: True))
    })
    |> dict.from_list()
  let lexical_bindings =
    dict.map_values(lexical_captures, fn(_ref, idx) {
      Binding(idx, CaptureBinding, True)
    })
  // Captures occupy slots 0..N-1 (named + lexical captures).
  let max_lex_idx =
    dict.fold(lexical_captures, list.length(captures) - 1, fn(m, _r, i) {
      int.max(m, i)
    })
  let capture_count = max_lex_idx + 1
  let scopes = case capture_count {
    0 -> []
    _ -> [Scope(kind: FunctionScope, bindings: capture_bindings)]
  }
  let names =
    list.index_map(captures, fn(name, idx) { #(name, idx) }) |> dict.from_list
  let r =
    Resolver(
      scopes:,
      next_local: capture_count,
      max_locals: capture_count,
      output: [],
      constants:,
      constants_map:,
      next_const: list.length(constants),
      captured_vars:,
      linker_seeded:,
      fallthrough:,
      lexical_bindings:,
      lexical_captured:,
      closure_scopes: dict.new(),
      names:,
    )
  let r = resolve_ops(r, code)
  Resolved(
    code: list.reverse(r.output),
    local_count: r.max_locals,
    constants: r.constants,
    constants_map: r.constants_map,
    lexical: lexical_slots_from(r.lexical_bindings),
    closure_scopes: r.closure_scopes,
    names: r.names,
  )
}

fn lexical_slots_from(bindings: Dict(LexicalRef, Binding)) -> LexicalSlots {
  let idx = fn(ref) {
    dict.get(bindings, ref)
    |> option.from_result
    |> option.map(fn(b) { b.index })
  }
  LexicalSlots(
    this: idx(RefThis),
    active_func: idx(RefActiveFunc),
    home_object: idx(RefHomeObject),
    new_target: idx(RefNewTarget),
  )
}

// ============================================================================
// Resolution loop
// ============================================================================

fn resolve_ops(r: Resolver, ops: List(EmitterOp)) -> Resolver {
  case ops {
    [] -> r
    [op, ..rest] -> {
      let r = resolve_one(r, op)
      resolve_ops(r, rest)
    }
  }
}

fn resolve_one(r: Resolver, op: EmitterOp) -> Resolver {
  case op {
    EnterScope(kind) -> {
      let scope = Scope(kind:, bindings: dict.new())
      Resolver(..r, scopes: [scope, ..r.scopes])
    }

    LeaveScope -> {
      case r.scopes {
        [_, ..rest] -> Resolver(..r, scopes: rest)
        [] -> r
      }
    }

    DeclareLexical(ref) -> {
      // Allocate an owned slot for this lexical binding. Non-arrows emit these
      // first after EnterScope(FunctionScope) so indices start at len(captures),
      // matching what setup_locals expects. Box if any inner closure captures
      // this ref (so arrow→super() write-backs alias the same cell).
      use <- on_some(dict.get(r.lexical_bindings, ref) |> option.from_result, r)
      let index = r.next_local
      let boxed = opcode.lexical_refs_get(r.lexical_captured, ref)
      let binding = Binding(index:, kind: ParamBinding, is_boxed: boxed)
      let r =
        Resolver(
          ..r,
          next_local: index + 1,
          max_locals: int.max(r.max_locals, index + 1),
          lexical_bindings: dict.insert(r.lexical_bindings, ref, binding),
        )
      case boxed {
        True -> emit(r, IrBoxLocal(index))
        False -> r
      }
    }

    Ir(IrGetLexical(ref)) ->
      case dict.get(r.lexical_bindings, ref) {
        Ok(Binding(index:, is_boxed: True, ..)) -> emit(r, IrGetBoxed(index))
        Ok(Binding(index:, is_boxed: False, ..)) -> emit(r, IrGetLocal(index))
        // Unreachable for well-formed input: every body that references a
        // lexical binding either DeclareLexical'd or got a capture slot.
        // Mirror read_lexical_local's defensive JsUndefined.
        Error(Nil) -> {
          let #(r, idx) = ensure_constant(r, JsUndefined)
          emit(r, IrPushConst(idx))
        }
      }

    // §9.1.1.3.1 BindThisValue — only emitted in derived ctors after super(),
    // where the slot was seeded JsUninitialized. CheckInit throws ReferenceError
    // on double-super().
    Ir(IrSetThis) ->
      case dict.get(r.lexical_bindings, RefThis) {
        Ok(Binding(index:, is_boxed: True, ..)) ->
          emit(r, IrPutBoxedCheckInit(index))
        Ok(Binding(index:, is_boxed: False, ..)) ->
          emit(r, IrPutLocalCheckInit(index))
        Error(Nil) -> r
      }

    Ir(IrMakeClosure(idx) as op) ->
      // Snapshot name→slot visible RIGHT NOW so compile_child captures the
      // correct slot for block-scoped shadows. children[i] ⟷ IrMakeClosure(i).
      Resolver(
        ..r,
        closure_scopes: dict.insert(
          r.closure_scopes,
          idx,
          flatten_scopes(r.scopes),
        ),
      )
      |> emit(op)

    DeclareVar(name, kind) -> {
      // If already declared in the target scope, skip entirely (no new slot,
      // no IR). Lets the emitter hoist let/const DeclareVar before hoisted
      // function MakeClosure without the inline DeclareVar double-boxing.
      let already = case kind {
        VarBinding | ParamBinding | CaptureBinding ->
          lookup_in_function_scope(r.scopes, name)
        LetBinding | ConstBinding | CatchBinding ->
          lookup_in_current_scope(r.scopes, name)
      }
      use <- on_some(already, r)
      let index = r.next_local
      // Linker-seeded module exports are always boxed: the linker pre-allocates
      // the cell and seeds it into this slot, so no init/box op is emitted.
      // Only module TOP-LEVEL declarations are linker cells — a same-named
      // binding inside a block scope (e.g. a class's inner-name const) is an
      // ordinary local and must get its normal init/boxing.
      let at_top_level =
        list.all(r.scopes, fn(s) { s.kind == emit.FunctionScope })
      let linker_seeded = at_top_level && set.contains(r.linker_seeded, name)
      let boxed = linker_seeded || set.contains(r.captured_vars, name)
      let binding = Binding(index:, kind:, is_boxed: boxed)
      let new_max = int.max(r.max_locals, index + 1)
      // Record name→slot for module-export lookup and direct-eval
      // local_names. First declaration wins: top-level let/const/class
      // DeclareVars are hoisted ahead of body emission, so a later same-named
      // binding in a block scope (e.g. a class's inner-name const) must not
      // steal the export's slot mapping.
      let names = case dict.has_key(r.names, name) {
        True -> r.names
        False -> dict.insert(r.names, name, index)
      }
      let r =
        Resolver(..r, next_local: index + 1, max_locals: new_max, names:)

      // Add binding to the appropriate scope
      let r = case kind {
        VarBinding | ParamBinding | CaptureBinding ->
          add_to_function_scope(r, name, binding)
        LetBinding | ConstBinding | CatchBinding ->
          add_to_current_scope(r, name, binding)
      }

      // Emit initialization + boxing — skipped entirely for linker-seeded
      // exports (the linker writes the initial value into the pre-made box).
      use <- bool.guard(linker_seeded, r)
      case kind {
        VarBinding -> {
          let #(r, idx) = ensure_constant(r, JsUndefined)
          let r = emit(emit(r, IrPushConst(idx)), IrPutLocal(index))
          // Box the local if it's captured by a child closure
          case boxed {
            True -> emit(r, IrBoxLocal(index))
            False -> r
          }
        }
        LetBinding | ConstBinding -> {
          let #(r, idx) = ensure_constant(r, JsUninitialized)
          let r = emit(emit(r, IrPushConst(idx)), IrPutLocal(index))
          case boxed {
            True -> emit(r, IrBoxLocal(index))
            False -> r
          }
        }
        ParamBinding | CatchBinding -> {
          // Params: set by call convention. Catch: set by unwind.
          // Both need BoxLocal if captured (or if eval is present).
          case boxed {
            True -> emit(r, IrBoxLocal(index))
            False -> r
          }
        }
        CaptureBinding -> r
        // Captures: already boxed refs from parent, never re-box.
      }
    }

    Ir(IrScopeGetVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrGetBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrGetLocal(index))
        None ->
          case r.fallthrough {
            ToGlobal -> emit(r, IrGetGlobal(name))
            ToEvalEnv -> emit(r, IrGetEvalVar(name))
          }
      }
    }

    // Assignment (`x = …`, `x += …`, `x++`, destructuring-assign). A store to a
    // const local binding is a runtime TypeError; the local store ops carry no
    // const flag, so — like QuickJS's resolve_scope_var — we resolve it here and
    // emit a throw. Initialization goes through IrScopeInitVar, never here.
    Ir(IrScopePutVar(name)) -> {
      case lookup(r.scopes, name) {
        // §9.1.1.1.5 SetMutableBinding step 6 — const bindings are always
        // strict (§14.3.1.3), so reassignment unconditionally throws TypeError.
        // RHS is already evaluated (on stack); throw discards it via unwind.
        Some(Binding(kind: ConstBinding, ..)) ->
          emit(r, IrThrowConstAssign(name))
        Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrPutBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrPutLocal(index))
        None ->
          case r.fallthrough {
            ToGlobal -> emit(r, IrPutGlobal(name))
            ToEvalEnv -> emit(r, IrPutEvalVar(name))
          }
      }
    }

    // One-time init write that bypasses the const-reassign check above. Used
    // for `const x = v` / inner-class-name / `<class_fields_init>` — mirrors
    // QuickJS OP_scope_put_var_init.
    Ir(IrScopeInitVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrPutBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrPutLocal(index))
        None ->
          case r.fallthrough {
            ToGlobal -> emit(r, IrPutGlobal(name))
            ToEvalEnv -> emit(r, IrPutEvalVar(name))
          }
      }
    }

    Ir(IrDeclareGlobalVar(name)) ->
      case r.fallthrough {
        ToGlobal -> emit(r, IrDeclareGlobalVar(name))
        ToEvalEnv -> emit(r, IrDeclareEvalVar(name))
      }

    Ir(IrScopeReboxVar(name)) ->
      case lookup(r.scopes, name) {
        Some(Binding(index:, is_boxed: True, ..)) ->
          emit(r, IrGetBoxed(index))
          |> emit(IrPutLocal(index))
          |> emit(IrBoxLocal(index))
        _ -> r
      }

    Ir(IrScopeTypeofVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, is_boxed: True, ..)) -> {
          let r = emit(r, IrGetBoxed(index))
          emit(r, IrTypeOf)
        }
        Some(Binding(index:, is_boxed: False, ..)) -> {
          let r = emit(r, IrGetLocal(index))
          emit(r, IrTypeOf)
        }
        None ->
          case r.fallthrough {
            ToGlobal -> emit(r, IrTypeofGlobal(name))
            ToEvalEnv -> emit(r, IrTypeofEvalVar(name))
          }
      }
    }

    // All other IR ops: pass through
    Ir(ir_op) -> emit(r, ir_op)
  }
}

// ============================================================================
// Scope helpers
// ============================================================================

fn add_to_current_scope(
  r: Resolver,
  name: String,
  binding: Binding,
) -> Resolver {
  case r.scopes {
    [scope, ..rest] -> {
      let scope =
        Scope(..scope, bindings: dict.insert(scope.bindings, name, binding))
      Resolver(..r, scopes: [scope, ..rest])
    }
    [] -> r
  }
}

fn add_to_function_scope(
  r: Resolver,
  name: String,
  binding: Binding,
) -> Resolver {
  let scopes = add_to_func_scope_inner(r.scopes, name, binding)
  Resolver(..r, scopes:)
}

fn add_to_func_scope_inner(
  scopes: List(Scope),
  name: String,
  binding: Binding,
) -> List(Scope) {
  case scopes {
    [] -> []
    [scope, ..rest] ->
      case scope.kind {
        FunctionScope -> {
          // Check if already declared (var can be declared multiple times)
          case dict.get(scope.bindings, name) {
            Ok(_) -> [scope, ..rest]
            // Already exists, reuse
            Error(Nil) -> {
              let scope =
                Scope(
                  ..scope,
                  bindings: dict.insert(scope.bindings, name, binding),
                )
              [scope, ..rest]
            }
          }
        }
        BlockScope -> [scope, ..add_to_func_scope_inner(rest, name, binding)]
      }
  }
}

/// Flatten the scope chain into name→index. `scopes` is innermost-first;
/// fold outermost→innermost so the innermost binding for a shadowed name wins.
fn flatten_scopes(scopes: List(Scope)) -> Dict(String, Int) {
  use acc, scope <- list.fold(list.reverse(scopes), dict.new())
  use a, name, b <- dict.fold(scope.bindings, acc)
  dict.insert(a, name, b.index)
}

fn lookup(scopes: List(Scope), name: String) -> Option(Binding) {
  case scopes {
    [] -> None
    [scope, ..rest] ->
      case dict.get(scope.bindings, name) {
        Ok(binding) -> Some(binding)
        Error(_) -> lookup(rest, name)
      }
  }
}

fn lookup_in_current_scope(
  scopes: List(Scope),
  name: String,
) -> Option(Binding) {
  case scopes {
    [] -> None
    [scope, ..] -> dict.get(scope.bindings, name) |> option.from_result
  }
}

fn lookup_in_function_scope(
  scopes: List(Scope),
  name: String,
) -> Option(Binding) {
  case scopes {
    [] -> None
    [Scope(kind: FunctionScope, bindings:), ..] ->
      dict.get(bindings, name) |> option.from_result
    [Scope(kind: BlockScope, ..), ..rest] ->
      lookup_in_function_scope(rest, name)
  }
}

fn on_some(opt: Option(a), if_some: b, cont: fn() -> b) -> b {
  case opt {
    Some(_) -> if_some
    None -> cont()
  }
}

fn emit(r: Resolver, op: IrOp) -> Resolver {
  Resolver(..r, output: [op, ..r.output])
}

fn ensure_constant(r: Resolver, val: JsValue) -> #(Resolver, Int) {
  case dict.get(r.constants_map, val) {
    Ok(idx) -> #(r, idx)
    Error(_) -> {
      let idx = r.next_const
      let r =
        Resolver(
          ..r,
          constants: list.append(r.constants, [val]),
          constants_map: dict.insert(r.constants_map, val, idx),
          next_const: idx + 1,
        )
      #(r, idx)
    }
  }
}
