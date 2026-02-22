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
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import lumen/compiler/emit.{
  type BindingKind, type EmitterOp, BlockScope, CaptureBinding, CatchBinding,
  ConstBinding, DeclareVar, EnterScope, FunctionScope, Ir, LeaveScope,
  LetBinding, ParamBinding, VarBinding,
}
import lumen/vm/opcode.{
  type IrOp, IrBoxLocal, IrGetBoxed, IrGetGlobal, IrGetLocal, IrPushConst,
  IrPutBoxed, IrPutGlobal, IrPutLocal, IrScopeGetVar, IrScopePutVar,
  IrScopeTypeofVar, IrTypeOf, IrTypeofGlobal,
}
import lumen/vm/value.{type JsValue, JsUndefined, JsUninitialized}

// ============================================================================
// Types
// ============================================================================

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
  )
}

// ============================================================================
// Public API
// ============================================================================

/// Resolve scopes in a list of EmitterOps.
/// Returns resolved IrOps (no scope markers), local_count, and updated constants.
pub fn resolve(
  code: List(EmitterOp),
  constants: List(JsValue),
  constants_map: Dict(JsValue, Int),
  captured_vars: Set(String),
) -> #(List(IrOp), Int, List(JsValue), Dict(JsValue, Int)) {
  let r =
    Resolver(
      scopes: [],
      next_local: 0,
      max_locals: 0,
      output: [],
      constants:,
      constants_map:,
      next_const: list.length(constants),
      captured_vars:,
    )
  let r = resolve_ops(r, code)
  #(list.reverse(r.output), r.max_locals, r.constants, r.constants_map)
}

/// Resolve scopes with pre-populated capture bindings.
/// Captures occupy local slots 0..len-1, before any params or body vars.
/// Capture bindings are always boxed (they hold refs to parent's BoxSlots).
/// Returns resolved IrOps, local_count, and updated constants.
pub fn resolve_with_captures(
  code: List(EmitterOp),
  constants: List(JsValue),
  constants_map: Dict(JsValue, Int),
  captures: List(String),
  captured_vars: Set(String),
) -> #(List(IrOp), Int, List(JsValue), Dict(JsValue, Int)) {
  let capture_count = list.length(captures)
  // Pre-populate a function scope with capture bindings (always boxed)
  let capture_bindings =
    list.index_map(captures, fn(name, idx) {
      #(name, Binding(index: idx, kind: CaptureBinding, is_boxed: True))
    })
    |> dict.from_list()
  let initial_scope = Scope(kind: FunctionScope, bindings: capture_bindings)
  let r =
    Resolver(
      scopes: [initial_scope],
      next_local: capture_count,
      max_locals: capture_count,
      output: [],
      constants:,
      constants_map:,
      next_const: list.length(constants),
      captured_vars:,
    )
  let r = resolve_ops(r, code)
  #(list.reverse(r.output), r.max_locals, r.constants, r.constants_map)
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

    DeclareVar(name, kind) -> {
      let index = r.next_local
      let boxed = set.contains(r.captured_vars, name)
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

      // Emit initialization + boxing
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
        ParamBinding -> {
          // Params are set by call convention. Box if captured.
          case boxed {
            True -> emit(r, IrBoxLocal(index))
            False -> r
          }
        }
        CatchBinding | CaptureBinding -> r
        // Catch: set by unwind. Captures: already boxed refs from parent.
      }
    }

    Ir(IrScopeGetVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrGetBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrGetLocal(index))
        None -> emit(r, IrGetGlobal(name))
      }
    }

    Ir(IrScopePutVar(name)) -> {
      case lookup(r.scopes, name) {
        Some(Binding(index:, is_boxed: True, ..)) -> emit(r, IrPutBoxed(index))
        Some(Binding(index:, is_boxed: False, ..)) -> emit(r, IrPutLocal(index))
        None -> emit(r, IrPutGlobal(name))
      }
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
        None -> emit(r, IrTypeofGlobal(name))
      }
    }

    // All other IR ops: pass through
    Ir(ir_op) -> emit(r, ir_op)
  }
}

// ============================================================================
// Scope helpers
// ============================================================================

fn add_to_current_scope(r: Resolver, name: String, binding: Binding) -> Resolver {
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
            Error(_) -> {
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
