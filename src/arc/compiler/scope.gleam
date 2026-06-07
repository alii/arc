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
  type BindingKind, type EmitterOp, AnnexBPromote, BlockScope, CaptureBinding,
  CatchBinding, ConstBinding, DeclareAnnexBVar, DeclareLexical, DeclareVar,
  EnterScope, EnterWith, FnNameBinding, FunctionScope, Ir, LeaveScope, LeaveWith,
  LetBinding, ParamBinding, VarBinding,
}
import arc/vm/opcode.{
  type IrOp, type LexicalRef, type LexicalRefs, type LexicalSlots, IrBoxLocal,
  IrDeclareEvalVar, IrDeclareGlobalVar, IrGetBoxed, IrGetEvalVar, IrGetGlobal,
  IrGetLexical, IrGetLocal, IrJump, IrLabel, IrMakeClosure, IrPop, IrPushConst,
  IrPutBoxed, IrPutBoxedCheckInit, IrPutEvalVar, IrPutGlobal, IrPutLocal,
  IrPutLocalCheckInit, IrScopeDeleteVar, IrScopeGetRef, IrScopeGetVar,
  IrScopeGetVarThis, IrScopeInitVar, IrScopeMakeRef, IrScopePutRef,
  IrScopePutVar, IrScopeReboxVar, IrScopeTypeofVar, IrSetThis,
  IrThrowConstAssign, IrTypeOf, IrTypeofEvalVar, IrTypeofGlobal, IrWithDeleteVar,
  IrWithGetRefValue, IrWithGetVar, IrWithGetVarThis, IrWithMakeRef,
  IrWithPutRefValue, IrWithPutVar, LexicalSlots, RefActiveFunc, RefHomeObject,
  RefNewTarget, RefThis,
}
import arc/vm/value.{type JsValue, JsBool, JsUndefined, JsUninitialized}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
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

/// A single scope level. `with_obj` is Some for the marker scope pushed by
/// EnterWith: any name resolution that walks PAST this scope must first check
/// the with object held in that binding's local slot at runtime.
type Scope {
  Scope(
    kind: emit.ScopeKind,
    bindings: Dict(String, Binding),
    with_obj: Option(Binding),
  )
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
    /// Capture names whose ORIGIN binding is const (e.g. the inner class-name
    /// binding captured by a method). §9.1.1.1.5 SetMutableBinding: writes
    /// through them throw TypeError, same as a locally-declared const.
    const_captures: Set(String),
    /// Capture names whose ORIGIN binding is a named-function-expression
    /// self name (FnNameBinding). Writes through them follow the same
    /// strict-aware rule as a local FnNameBinding: strict → TypeError,
    /// sloppy → silently dropped.
    fn_name_captures: Set(String),
    /// Const-binding names visible at each IrMakeClosure(idx) — the
    /// const-capture seed for that child (parallel to closure_scopes).
    closure_consts: Dict(Int, Set(String)),
    /// FnNameBinding names visible at each IrMakeClosure(idx) — the
    /// fn-name-capture seed for that child (parallel to closure_consts).
    closure_fn_names: Dict(Int, Set(String)),
    /// Every name ever allocated a slot in this body (cumulative, across
    /// closed block scopes). Used for module-export lookup and direct-eval
    /// local_names.
    names: Dict(String, Int),
    /// Next fresh label ID for the with-check jump targets Phase 2 emits.
    /// Seeded past the max label ID present in the Phase 1 ops.
    next_label: Int,
    /// Scratch local slots holding active reference bases (IrScopeMakeRef
    /// … IrScopePutRef pairs that cross `with` markers). Head = innermost.
    ref_active: List(Int),
    /// Scratch slots returned by closed refs, reused by later MakeRefs.
    ref_free: List(Int),
    /// Whether the code being resolved is strict mode. Decides what a write
    /// to a FnNameBinding (named-function-expression self name) compiles to:
    /// strict → TypeError throw, sloppy → silent drop (§9.1.1.1.5
    /// SetMutableBinding on an immutable binding).
    strict: Bool,
    /// Slot indices of let bindings whose IrScopeInitVar has already been
    /// resolved (linearly earlier in the op stream). A store to a let slot
    /// NOT in this set may execute while the binding is still in TDZ —
    /// §9.1.1.1.5 SetMutableBinding step 5 — so it gets a checked store
    /// (TDZ-check read before the put). Slots are never reused across
    /// bindings (next_local only grows), so a plain Int set is sound.
    initialized: Set(Int),
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
    /// Const-binding names visible at each `IrMakeClosure(i)` — the child's
    /// const-capture seed (writes through such captures throw TypeError).
    closure_consts: Dict(Int, Set(String)),
    /// FnNameBinding names visible at each `IrMakeClosure(i)` — the child's
    /// fn-name-capture seed (writes through such captures throw in strict
    /// code, are silently dropped in sloppy code).
    closure_fn_names: Dict(Int, Set(String)),
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
  strict: Bool,
) -> Resolved {
  resolve_with_captures(
    code,
    constants,
    constants_map,
    [],
    dict.new(),
    set.new(),
    set.new(),
    captured_vars,
    set.new(),
    lexical_captured,
    fallthrough,
    [],
    strict,
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
  const_captures: Set(String),
  fn_name_captures: Set(String),
  captured_vars: Set(String),
  linker_seeded: Set(String),
  lexical_captured: LexicalRefs,
  fallthrough: GlobalFallthrough,
  with_stack: List(String),
  strict: Bool,
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
    _ -> [
      Scope(kind: FunctionScope, bindings: capture_bindings, with_obj: None),
    ]
  }
  // Enclosing `with` scopes (innermost first) sit between this body's own
  // scopes and the capture scope: a free name (or captured parent local)
  // must check the with objects before reaching outer bindings/globals.
  // Each with object is itself a captured (boxed) parent local.
  let scopes =
    list.filter_map(with_stack, fn(name) {
      dict.get(capture_bindings, name)
      |> result.map(fn(b) {
        Scope(kind: BlockScope, bindings: dict.new(), with_obj: Some(b))
      })
    })
    |> list.append(scopes)
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
      const_captures:,
      fn_name_captures:,
      closure_consts: dict.new(),
      closure_fn_names: dict.new(),
      names:,
      next_label: max_label_id(code) + 1,
      ref_active: [],
      ref_free: [],
      strict:,
      initialized: set.new(),
    )
  let r = resolve_ops(r, code)
  Resolved(
    code: list.reverse(r.output),
    local_count: r.max_locals,
    constants: r.constants,
    constants_map: r.constants_map,
    lexical: lexical_slots_from(r.lexical_bindings),
    closure_scopes: r.closure_scopes,
    closure_consts: r.closure_consts,
    closure_fn_names: r.closure_fn_names,
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
      let scope = Scope(kind:, bindings: dict.new(), with_obj: None)
      Resolver(..r, scopes: [scope, ..r.scopes])
    }

    LeaveScope -> {
      case r.scopes {
        [_, ..rest] -> Resolver(..r, scopes: rest)
        [] -> r
      }
    }

    // §14.11 with: push a marker scope holding the binding of the synthetic
    // local that stores the with object (declared just before this marker).
    // Name resolutions that walk past the marker emit IrWith*Var checks.
    EnterWith(name) ->
      case lookup(r.scopes, name) {
        Some(binding) -> {
          let scope =
            Scope(
              kind: BlockScope,
              bindings: dict.new(),
              with_obj: Some(binding),
            )
          Resolver(..r, scopes: [scope, ..r.scopes])
        }
        // Unreachable for well-formed input: the emitter declares the
        // synthetic immediately before EnterWith.
        None -> r
      }

    LeaveWith ->
      case r.scopes {
        [Scope(with_obj: Some(_), ..), ..rest] -> Resolver(..r, scopes: rest)
        _ -> r
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
      // Also snapshot which visible names are const so the child rejects
      // writes through those captures (class inner-name binding, etc.).
      Resolver(
        ..r,
        closure_scopes: dict.insert(
          r.closure_scopes,
          idx,
          flatten_scopes(r.scopes),
        ),
        closure_consts: dict.insert(r.closure_consts, idx, flatten_consts(r)),
        closure_fn_names: dict.insert(
          r.closure_fn_names,
          idx,
          flatten_fn_names(r),
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
        LetBinding | ConstBinding | CatchBinding | FnNameBinding ->
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
      let r = Resolver(..r, next_local: index + 1, max_locals: new_max, names:)

      // Add binding to the appropriate scope
      let r = case kind {
        VarBinding | ParamBinding | CaptureBinding ->
          add_to_function_scope(r, name, binding)
        LetBinding | ConstBinding | CatchBinding | FnNameBinding ->
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
        LetBinding | ConstBinding | FnNameBinding -> {
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
      let #(found, crossed) = lookup_crossing(r.scopes, name)
      let fallback = emit_static_get(_, found, name)
      case crossed {
        [] -> fallback(r)
        _ -> {
          // §14.11: check each crossed with object (innermost first); the
          // first that has the (unscopables-visible) property supplies the
          // value and jumps to `done`. Otherwise fall back to the binding.
          let #(r, done) = fresh_label(r)
          let r =
            list.fold(crossed, r, fn(r, w) {
              r |> emit_with_obj(w) |> emit(IrWithGetVar(name, done))
            })
          let r = fallback(r)
          emit(r, IrLabel(done))
        }
      }
    }

    // Callee identifier of `f(args)` inside a `with` body — push the call
    // receiver beneath the callee value. §13.3.6.2 EvaluateCall step 1.b.ii:
    // when the name resolves through a with object, thisValue is that object
    // (WithBaseObject); otherwise undefined. Result stack: [value, this, ..].
    Ir(IrScopeGetVarThis(name)) -> {
      let #(found, crossed) = lookup_crossing(r.scopes, name)
      let fallback = fn(r) {
        let #(r, uidx) = ensure_constant(r, JsUndefined)
        let r = emit(r, IrPushConst(uidx))
        emit_static_get(r, found, name)
      }
      case crossed {
        [] -> fallback(r)
        _ -> {
          // Check each crossed with object (innermost first); the first that
          // has the (unscopables-visible) property supplies BOTH the value
          // and the receiver, then jumps to `done`. Otherwise fall back to
          // the static binding with receiver = undefined.
          let #(r, done) = fresh_label(r)
          let r =
            list.fold(crossed, r, fn(r, w) {
              r |> emit_with_obj(w) |> emit(IrWithGetVarThis(name, done))
            })
          let r = fallback(r)
          emit(r, IrLabel(done))
        }
      }
    }

    // Assignment (`x = …`, `x += …`, `x++`, destructuring-assign). A store to a
    // const local binding is a runtime TypeError; the local store ops carry no
    // const flag, so — like QuickJS's resolve_scope_var — we resolve it here and
    // emit a throw. Initialization goes through IrScopeInitVar, never here.
    Ir(IrScopePutVar(name)) -> {
      let #(found, crossed) = lookup_crossing(r.scopes, name)
      let fallback = emit_static_put(_, found, name)
      case crossed {
        [] -> fallback(r)
        _ -> {
          let #(r, done) = fresh_label(r)
          let r =
            list.fold(crossed, r, fn(r, w) {
              r |> emit_with_obj(w) |> emit(IrWithPutVar(name, done))
            })
          let r = fallback(r)
          emit(r, IrLabel(done))
        }
      }
    }

    // §13.15.2 step 1a — ResolveBinding for an assignment-like target before
    // the RHS runs. Only matters when the resolution crosses with markers:
    // store the matched with object (or undefined = static) in a scratch
    // local so the paired GetRef/PutRef hit the ORIGINAL reference base.
    Ir(IrScopeMakeRef(name)) -> {
      let #(_found, crossed) = lookup_crossing(r.scopes, name)
      case crossed {
        [] -> r
        _ -> {
          let #(r, slot) = acquire_ref_slot(r)
          let #(r, lref) = fresh_label(r)
          let r =
            list.fold(crossed, r, fn(r, w) {
              r |> emit_with_obj(w) |> emit(IrWithMakeRef(name, lref))
            })
          let #(r, uidx) = ensure_constant(r, JsUndefined)
          let r = emit(r, IrPushConst(uidx))
          let r = emit(r, IrLabel(lref))
          emit(r, IrPutLocal(slot))
        }
      }
    }

    // GetValue on the innermost active reference (§9.1.1.2.6).
    Ir(IrScopeGetRef(name)) -> {
      let #(found, crossed) = lookup_crossing(r.scopes, name)
      let fallback = emit_static_get(_, found, name)
      case crossed, r.ref_active {
        [], _ | _, [] -> fallback(r)
        _, [slot, ..] -> {
          let #(r, lg) = fresh_label(r)
          let r = emit(r, IrGetLocal(slot))
          let r = emit(r, IrWithGetRefValue(name, lg))
          let r = fallback(r)
          emit(r, IrLabel(lg))
        }
      }
    }

    // PutValue on the innermost active reference (§9.1.1.2.5); closes it.
    Ir(IrScopePutRef(name)) -> {
      let #(found, crossed) = lookup_crossing(r.scopes, name)
      let fallback = emit_static_put(_, found, name)
      case crossed, r.ref_active {
        [], _ | _, [] -> fallback(r)
        _, [slot, ..rest_active] -> {
          let r =
            Resolver(..r, ref_active: rest_active, ref_free: [
              slot,
              ..r.ref_free
            ])
          let #(r, ld) = fresh_label(r)
          let r = emit(r, IrGetLocal(slot))
          let r = emit(r, IrWithPutRefValue(name, ld))
          let r = fallback(r)
          emit(r, IrLabel(ld))
        }
      }
    }

    // `delete Identifier` (sloppy mode). If an enclosing with object has the
    // property, [[Delete]] it and push the result; otherwise keep the legacy
    // `true` result for plain variables.
    Ir(IrScopeDeleteVar(name)) -> {
      let #(_found, crossed) = lookup_crossing(r.scopes, name)
      let push_true = fn(r) {
        let #(r, idx) = ensure_constant(r, JsBool(True))
        emit(r, IrPushConst(idx))
      }
      case crossed {
        [] -> push_true(r)
        _ -> {
          let #(r, done) = fresh_label(r)
          let r =
            list.fold(crossed, r, fn(r, w) {
              r |> emit_with_obj(w) |> emit(IrWithDeleteVar(name, done))
            })
          let r = push_true(r)
          emit(r, IrLabel(done))
        }
      }
    }

    // One-time init write that bypasses the const-reassign check above. Used
    // for `const x = v` / inner-class-name / `<class_fields_init>` — mirrors
    // QuickJS OP_scope_put_var_init.
    Ir(IrScopeInitVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, is_boxed: True, ..)) ->
          Resolver(..r, initialized: set.insert(r.initialized, index))
          |> emit(IrPutBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) ->
          Resolver(..r, initialized: set.insert(r.initialized, index))
          |> emit(IrPutLocal(index))
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

    // Annex B §B.3.2.2/.2.6: var binding (undefined) for a sloppy-mode
    // function-in-block name, created before the body runs. Same routing as
    // IrDeclareGlobalVar; kept as a distinct emitter op so compile_eval_direct
    // can drop it when a direct eval is strict via its caller.
    DeclareAnnexBVar(name) ->
      case r.fallthrough {
        ToGlobal -> emit(r, IrDeclareGlobalVar(name))
        ToEvalEnv -> emit(r, IrDeclareEvalVar(name))
      }

    // Annex B §B.3.2.6 runtime step: when a block-level FunctionDeclaration
    // is evaluated, copy the block-scoped binding's value into the enclosing
    // VariableEnvironment binding — skipping intermediate block scopes (and
    // simple catch-param bindings, §B.3.4). Skipped entirely when an
    // intermediate let/const (or a top-level lexical) shadows the name: the
    // emitter's AST-side analysis prevents most of these, and this check
    // covers bindings already declared at resolution time.
    AnnexBPromote(name) ->
      case annexb_promote_plan(r.scopes, name) {
        None -> r
        Some(#(source, target)) -> {
          let r = case source.is_boxed {
            True -> emit(r, IrGetBoxed(source.index))
            False -> emit(r, IrGetLocal(source.index))
          }
          case target {
            Some(Binding(index:, is_boxed: True, ..)) ->
              emit(r, IrPutBoxed(index))
            Some(Binding(index:, is_boxed: False, ..)) ->
              emit(r, IrPutLocal(index))
            None ->
              case r.fallthrough {
                ToGlobal -> emit(r, IrPutGlobal(name))
                ToEvalEnv -> emit(r, IrPutEvalVar(name))
              }
          }
        }
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
      let #(found, crossed) = lookup_crossing(r.scopes, name)
      let fallback = fn(r) {
        case found {
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
      case crossed {
        [] -> fallback(r)
        _ -> {
          // A with object that has the property supplies the value (jump to
          // `found_l`, then TypeOf); otherwise the fallback computes typeof.
          let #(r, found_l) = fresh_label(r)
          let #(r, end_l) = fresh_label(r)
          let r =
            list.fold(crossed, r, fn(r, w) {
              r |> emit_with_obj(w) |> emit(IrWithGetVar(name, found_l))
            })
          let r = fallback(r)
          let r = emit(r, IrJump(end_l))
          let r = emit(r, IrLabel(found_l))
          let r = emit(r, IrTypeOf)
          emit(r, IrLabel(end_l))
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

/// Names whose VISIBLE binding (innermost shadow wins, like flatten_scopes)
/// is const — either declared const here, or a capture whose origin binding
/// was const in an enclosing body.
fn flatten_consts(r: Resolver) -> Set(String) {
  use acc, name, kind <- dict.fold(flatten_kinds(r), set.new())
  case kind {
    ConstBinding -> set.insert(acc, name)
    CaptureBinding ->
      case set.contains(r.const_captures, name) {
        True -> set.insert(acc, name)
        False -> acc
      }
    _ -> acc
  }
}

/// Names whose VISIBLE binding is a named-function-expression self name —
/// declared here, or a capture whose origin was one in an enclosing body.
fn flatten_fn_names(r: Resolver) -> Set(String) {
  use acc, name, kind <- dict.fold(flatten_kinds(r), set.new())
  case kind {
    FnNameBinding -> set.insert(acc, name)
    CaptureBinding ->
      case set.contains(r.fn_name_captures, name) {
        True -> set.insert(acc, name)
        False -> acc
      }
    _ -> acc
  }
}

/// name → kind of the VISIBLE binding (innermost shadow wins).
fn flatten_kinds(r: Resolver) -> Dict(String, BindingKind) {
  use acc, scope <- list.fold(list.reverse(r.scopes), dict.new())
  use a, name, b <- dict.fold(scope.bindings, acc)
  dict.insert(a, name, b.kind)
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

/// Like `lookup`, but also returns the with-object bindings of every
/// EnterWith marker scope crossed BEFORE the binding was found (innermost
/// first). Names found inside the with body (e.g. its block's lexicals)
/// cross nothing; names resolving outside — or falling through to
/// globals/eval — must check every crossed with object at runtime.
fn lookup_crossing(
  scopes: List(Scope),
  name: String,
) -> #(Option(Binding), List(Binding)) {
  do_lookup_crossing(scopes, name, [])
}

fn do_lookup_crossing(
  scopes: List(Scope),
  name: String,
  acc: List(Binding),
) -> #(Option(Binding), List(Binding)) {
  case scopes {
    [] -> #(None, list.reverse(acc))
    [scope, ..rest] ->
      case dict.get(scope.bindings, name) {
        Ok(binding) -> #(Some(binding), list.reverse(acc))
        Error(Nil) -> {
          let acc = case scope.with_obj {
            Some(w) -> [w, ..acc]
            None -> acc
          }
          do_lookup_crossing(rest, name, acc)
        }
      }
  }
}

/// Push the with object held in `binding`'s slot onto the runtime stack.
fn emit_with_obj(r: Resolver, binding: Binding) -> Resolver {
  case binding.is_boxed {
    True -> emit(r, IrGetBoxed(binding.index))
    False -> emit(r, IrGetLocal(binding.index))
  }
}

/// The non-with ("static") read of a resolved binding: local/boxed slot, or
/// global/eval-env fallthrough.
fn emit_static_get(
  r: Resolver,
  found: Option(Binding),
  name: String,
) -> Resolver {
  case found {
    Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrGetBoxed(index))
    Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrGetLocal(index))
    None ->
      case r.fallthrough {
        ToGlobal -> emit(r, IrGetGlobal(name))
        ToEvalEnv -> emit(r, IrGetEvalVar(name))
      }
  }
}

/// The non-with ("static") store to a resolved binding. §9.1.1.1.5
/// SetMutableBinding step 6 — const bindings are always strict (§14.3.1.3),
/// so reassignment unconditionally throws TypeError. RHS is already
/// evaluated (on stack); throw discards it via unwind.
fn emit_static_put(
  r: Resolver,
  found: Option(Binding),
  name: String,
) -> Resolver {
  case found {
    Some(Binding(kind: ConstBinding, ..)) -> emit(r, IrThrowConstAssign(name))
    // §13.2.5.5 named-function-expression self name: an immutable binding
    // created with strict=false, so SetMutableBinding (§9.1.1.1.5 step 4)
    // throws only when the WRITE site is strict code; sloppy writes are
    // silently dropped (value popped to keep the stack balanced).
    Some(Binding(kind: FnNameBinding, ..)) ->
      case r.strict {
        True -> emit(r, IrThrowConstAssign(name))
        False -> emit(r, IrPop)
      }
    // A capture whose origin binding is const (class inner-name binding seen
    // from a method) — same SetMutableBinding step 6 TypeError. A capture of
    // an NFE self name gets the strict-aware FnNameBinding treatment instead.
    Some(Binding(kind: CaptureBinding, index:, is_boxed:)) ->
      case set.contains(r.fn_name_captures, name) {
        True ->
          case r.strict {
            True -> emit(r, IrThrowConstAssign(name))
            False -> emit(r, IrPop)
          }
        False ->
          case set.contains(r.const_captures, name) {
            True -> emit(r, IrThrowConstAssign(name))
            // The origin binding may be a let still in TDZ when this closure
            // runs (`(function() { x = 1; })(); let x;`) — §9.1.1.1.5
            // SetMutableBinding step 5: store to an uninitialized binding is
            // a ReferenceError. The resolver can't see the origin's kind, so
            // every capture store is checked: a TDZ-checking read (throws on
            // JsUninitialized) before the put. Var/param origin cells are
            // never uninitialized, so for them this is only a wasted read.
            False -> emit_checked_put(r, index, is_boxed)
          }
      }
    // A let binding whose initialization has NOT been resolved yet (linearly)
    // — the store may run during TDZ (`{ x = 1; let x; }`), so check first.
    Some(Binding(kind: LetBinding, index:, is_boxed:)) ->
      case set.contains(r.initialized, index) {
        True ->
          case is_boxed {
            True -> emit(r, IrPutBoxed(index))
            False -> emit(r, IrPutLocal(index))
          }
        False -> emit_checked_put(r, index, is_boxed)
      }
    Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrPutBoxed(index))
    Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrPutLocal(index))
    None ->
      case r.fallthrough {
        ToGlobal -> emit(r, IrPutGlobal(name))
        ToEvalEnv -> emit(r, IrPutEvalVar(name))
      }
  }
}

/// A store that must respect TDZ (§9.1.1.1.5 SetMutableBinding step 5):
/// re-use the TDZ-checking read ops (GetLocal/GetBoxed throw ReferenceError
/// on JsUninitialized), drop the read value, then store.
fn emit_checked_put(r: Resolver, index: Int, is_boxed: Bool) -> Resolver {
  case is_boxed {
    True ->
      r
      |> emit(IrGetBoxed(index))
      |> emit(IrPop)
      |> emit(IrPutBoxed(index))
    False ->
      r
      |> emit(IrGetLocal(index))
      |> emit(IrPop)
      |> emit(IrPutLocal(index))
  }
}

/// Acquire a scratch local for a reference base — reuse a freed one or
/// allocate a fresh slot.
fn acquire_ref_slot(r: Resolver) -> #(Resolver, Int) {
  case r.ref_free {
    [slot, ..rest] -> #(
      Resolver(..r, ref_free: rest, ref_active: [slot, ..r.ref_active]),
      slot,
    )
    [] -> {
      let slot = r.next_local
      #(
        Resolver(
          ..r,
          next_local: slot + 1,
          max_locals: int.max(r.max_locals, slot + 1),
          ref_active: [slot, ..r.ref_active],
        ),
        slot,
      )
    }
  }
}

/// Mint a fresh Phase-3 label ID (seeded past all Phase-1 labels).
fn fresh_label(r: Resolver) -> #(Resolver, Int) {
  #(Resolver(..r, next_label: r.next_label + 1), r.next_label)
}

/// Largest label ID placed in the Phase-1 ops (-1 if none). Every label the
/// emitter references is also placed, so scanning IrLabel is sufficient.
fn max_label_id(code: List(EmitterOp)) -> Int {
  use acc, op <- list.fold(code, -1)
  case op {
    Ir(IrLabel(id)) -> int.max(acc, id)
    _ -> acc
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

/// Annex B promotion plan: the SOURCE is the innermost binding of `name`
/// (the block-scoped function binding just initialized by
/// BlockDeclarationInstantiation). The TARGET is the var-scope binding —
/// found by walking outward past block scopes: catch-param bindings are
/// stepped over (§B.3.4), let/const bindings abort the promotion, and a
/// function-scope var/param/capture binding (or fallthrough) receives the
/// value. Returns None when there is no source or the promotion is blocked.
fn annexb_promote_plan(
  scopes: List(Scope),
  name: String,
) -> Option(#(Binding, Option(Binding))) {
  use #(source, rest) <- option.then(find_innermost(scopes, name))
  use target <- option.map(annexb_target(rest, name))
  #(source, target)
}

/// Innermost binding of `name` plus the scopes OUTSIDE the scope it was
/// found in.
fn find_innermost(
  scopes: List(Scope),
  name: String,
) -> Option(#(Binding, List(Scope))) {
  case scopes {
    [] -> None
    [scope, ..rest] ->
      case dict.get(scope.bindings, name) {
        Ok(binding) -> Some(#(binding, rest))
        Error(Nil) -> find_innermost(rest, name)
      }
  }
}

/// Walk outward from the source's enclosing scopes to the var-scope target.
/// Some(Some(b)) = write to local binding b; Some(None) = write through the
/// fallthrough (global / eval env); None = promotion blocked.
fn annexb_target(scopes: List(Scope), name: String) -> Option(Option(Binding)) {
  case scopes {
    [] -> Some(None)
    [scope, ..rest] ->
      case dict.get(scope.bindings, name), scope.kind {
        // Lexical shadow between block and var scope — not promotable.
        // (FnNameBinding can never collide here — compile_function_body
        // skips the self-name binding when an Annex B candidate shadows it —
        // but block immutable bindings would also be non-promotable.)
        Ok(Binding(kind: LetBinding, ..)), _
        | Ok(Binding(kind: ConstBinding, ..)), _
        | Ok(Binding(kind: FnNameBinding, ..)), _
        -> None
        // Simple catch param: §B.3.4 lets the promotion pass through it.
        Ok(Binding(kind: CatchBinding, ..)), _ -> annexb_target(rest, name)
        // Var/param/capture binding: this is the VariableEnvironment binding.
        Ok(binding), _ -> Some(Some(binding))
        Error(Nil), FunctionScope ->
          case rest {
            // Captures live in the outermost FunctionScope frame; keep
            // walking if there are more scopes (nested resolution never
            // stacks FunctionScopes, so this only matters for safety).
            [] -> Some(None)
            _ -> annexb_target(rest, name)
          }
        Error(Nil), BlockScope -> annexb_target(rest, name)
      }
  }
}

fn lookup_in_function_scope(
  scopes: List(Scope),
  name: String,
) -> Option(Binding) {
  case scopes {
    [] -> None
    [Scope(kind: FunctionScope, bindings:, ..), ..] ->
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
