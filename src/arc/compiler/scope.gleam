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
  ConstBinding, DeclareThis, DeclareVar, EnterScope, FunctionScope, Ir,
  LeaveScope, LetBinding, ParamBinding, VarBinding,
}
import arc/vm/opcode.{
  type IrOp, ConstAssignment, IrBoxLocal, IrDeclareEvalVar, IrDeclareGlobalVar,
  IrGetBoxed, IrGetEvalVar, IrGetGlobal, IrGetLocal, IrGetThis, IrPushConst,
  IrPutBoxed, IrPutEvalVar, IrPutGlobal, IrPutLocal, IrScopeGetVar,
  IrScopeInitVar, IrScopePutVar, IrScopeReboxVar, IrScopeTypeofVar, IrSetThis,
  IrThrowError, IrTypeOf, IrTypeofEvalVar, IrTypeofGlobal,
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
    /// Binding for the lexical-`this` slot. Some when DeclareThis has been
    /// processed (owned slot) or when this body inherits `this` as a capture
    /// (arrows, direct-eval); None until then.
    this_binding: Option(Binding),
    /// True when an inner closure (or direct eval in this subtree) captures
    /// `this` — DeclareThis will box the slot so writes (super()) propagate.
    this_is_captured: Bool,
  )
}

/// Result of scope resolution.
pub type Resolved {
  Resolved(
    code: List(IrOp),
    local_count: Int,
    constants: List(JsValue),
    constants_map: Dict(JsValue, Int),
    /// Local slot index where lexical `this` lives in this body — owned
    /// (DeclareThis) or inherited as a capture. None if `this` is unreferenced.
    this_slot: Option(Int),
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
  this_is_captured: Bool,
  fallthrough: GlobalFallthrough,
) -> Resolved {
  resolve_with_captures(
    code,
    constants,
    constants_map,
    [],
    None,
    captured_vars,
    set.new(),
    this_is_captured,
    fallthrough,
  )
}

/// Resolve scopes with pre-populated capture bindings.
/// Named captures occupy local slots 0..len-1. When `this_capture_slot` is
/// Some(idx) (arrows, direct-eval), `this` is a boxed CaptureBinding at idx
/// — caller must size env_descriptors/seed-locals accordingly. When None,
/// `this` comes from a `DeclareThis` in `code` (or is unreferenced).
pub fn resolve_with_captures(
  code: List(EmitterOp),
  constants: List(JsValue),
  constants_map: Dict(JsValue, Int),
  captures: List(String),
  this_capture_slot: Option(Int),
  captured_vars: Set(String),
  linker_seeded: Set(String),
  this_is_captured: Bool,
  fallthrough: GlobalFallthrough,
) -> Resolved {
  let capture_bindings =
    list.index_map(captures, fn(name, idx) {
      #(name, Binding(index: idx, kind: CaptureBinding, is_boxed: True))
    })
    |> dict.from_list()
  let this_binding =
    option.map(this_capture_slot, Binding(_, CaptureBinding, True))
  // Captures occupy slots 0..N-1 (named + this-capture if present).
  let capture_count = case this_capture_slot {
    Some(idx) -> int.max(list.length(captures), idx + 1)
    None -> list.length(captures)
  }
  let scopes = case captures, this_capture_slot {
    [], None -> []
    _, _ -> [Scope(kind: FunctionScope, bindings: capture_bindings)]
  }
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
      this_binding:,
      this_is_captured:,
    )
  let r = resolve_ops(r, code)
  Resolved(
    code: list.reverse(r.output),
    local_count: r.max_locals,
    constants: r.constants,
    constants_map: r.constants_map,
    this_slot: option.map(r.this_binding, fn(b) { b.index }),
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

    DeclareThis -> {
      // Allocate the lexical-`this` slot. Non-arrows emit this first after
      // EnterScope(FunctionScope) so index == len(captures), matching what
      // setup_locals expects. Box if any inner closure captures `this` (so
      // arrow→super() write-backs alias the same cell).
      use <- on_some(r.this_binding, r)
      let index = r.next_local
      let boxed = r.this_is_captured
      let binding = Binding(index:, kind: ParamBinding, is_boxed: boxed)
      let r =
        Resolver(
          ..r,
          next_local: index + 1,
          max_locals: int.max(r.max_locals, index + 1),
          this_binding: Some(binding),
        )
      case boxed {
        True -> emit(r, IrBoxLocal(index))
        False -> r
      }
    }

    Ir(IrGetThis) ->
      case r.this_binding {
        Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrGetBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrGetLocal(index))
        // Unreachable for well-formed input: every body that references `this`
        // either DeclareThis'd or got a this_capture_slot. Mirror
        // read_this_local's defensive JsUndefined.
        None -> {
          let #(r, idx) = ensure_constant(r, JsUndefined)
          emit(r, IrPushConst(idx))
        }
      }

    Ir(IrSetThis) ->
      case r.this_binding {
        Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrPutBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrPutLocal(index))
        None -> r
      }

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
      let linker_seeded = set.contains(r.linker_seeded, name)
      let boxed = linker_seeded || set.contains(r.captured_vars, name)
      let binding = Binding(index:, kind:, is_boxed: boxed)
      let new_max = case index + 1 > r.max_locals {
        True -> index + 1
        False -> r.max_locals
      }
      let r = Resolver(..r, next_local: index + 1, max_locals: new_max)

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
        Some(Binding(kind: ConstBinding, ..)) ->
          emit(r, IrThrowError(ConstAssignment))
        Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrPutBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrPutLocal(index))
        None ->
          case r.fallthrough {
            ToGlobal -> emit(r, IrPutGlobal(name))
            ToEvalEnv -> emit(r, IrPutEvalVar(name))
          }
      }
    }

    // Initialization of a let/const binding — same lowering as IrScopePutVar
    // but never const-checked (binding a const for the first time is allowed).
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
