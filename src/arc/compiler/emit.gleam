/// Phase 1: AST Emission
///
/// Walks the AST and produces a list of EmitterOps — symbolic IR instructions
/// mixed with scope markers. Variable references use string names (IrScopeGetVar),
/// jump targets use integer label IDs (IrJump). These are resolved in Phase 2 and 3.
import arc/ast
import arc/vm/opcode.{
  type IrOp, IrArrayFrom, IrArrayFromWithHoles, IrArrayPush, IrArrayPushHole,
  IrArraySpread, IrAwait, IrBinOp, IrCallApply, IrCallConstructor,
  IrCallConstructorApply, IrCallMethod, IrCallMethodApply, IrCallSuper,
  IrCreateArguments, IrDefineAccessor, IrDefineAccessorComputed, IrDefineField,
  IrDefineFieldComputed, IrDefineMethod, IrDeleteElem, IrDeleteField, IrDup,
  IrEnterFinally, IrEnterFinallyThrow, IrForInNext, IrForInStart, IrGetElem,
  IrGetElem2, IrGetField, IrGetField2, IrGetIterator, IrGetThis, IrInitialYield,
  IrIteratorNext, IrJump, IrJumpIfFalse, IrJumpIfNullish, IrJumpIfTrue, IrLabel,
  IrLeaveFinally, IrMakeClosure, IrMarkGlobalConst, IrNewObject, IrObjectSpread,
  IrPop, IrPopTry, IrPushConst, IrPushTry, IrPutElem, IrPutField, IrReturn,
  IrScopeGetVar, IrScopePutVar, IrScopeTypeofVar, IrSetupDerivedClass, IrSwap,
  IrThrow, IrTypeOf, IrUnaryOp, IrUnmarkGlobalConst, IrYield,
}
import arc/vm/value.{
  type JsValue, Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined,
}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// Types
// ============================================================================

/// An instruction in the emitter output — either a real IR op or a scope marker.
pub type EmitterOp {
  /// A real IR instruction (passed to Phase 2 → Phase 3)
  Ir(IrOp)
  /// Open a new scope
  EnterScope(kind: ScopeKind)
  /// Close the current scope
  LeaveScope
  /// Declare a variable in the current scope
  DeclareVar(name: String, kind: BindingKind)
}

pub type ScopeKind {
  FunctionScope
  BlockScope
}

pub type BindingKind {
  VarBinding
  LetBinding
  ConstBinding
  ParamBinding
  CatchBinding
  CaptureBinding
}

/// A compiled child function (before Phase 2/3).
pub type CompiledChild {
  CompiledChild(
    name: Option(String),
    arity: Int,
    code: List(EmitterOp),
    constants: List(JsValue),
    constants_map: Dict(JsValue, Int),
    functions: List(CompiledChild),
    is_strict: Bool,
    is_arrow: Bool,
    is_derived_constructor: Bool,
    is_generator: Bool,
    is_async: Bool,
  )
}

/// Loop context for break/continue targets.
pub type LoopContext {
  LoopContext(break_label: Int, continue_label: Int, label: Option(String))
}

/// The emitter state, threaded through all emit functions.
pub opaque type Emitter {
  Emitter(
    code: List(EmitterOp),
    constants_map: Dict(JsValue, Int),
    constants_list: List(JsValue),
    next_const: Int,
    next_label: Int,
    loop_stack: List(LoopContext),
    functions: List(CompiledChild),
    next_func: Int,
    /// Set by LabeledStatement before emitting a loop body.
    /// Consumed by push_loop to attach the label to the LoopContext.
    pending_label: Option(String),
    /// True if the current compilation unit is strict. Inherited by child
    /// functions; can be upgraded (never downgraded) by a "use strict"
    /// directive in the function body prologue. Classes force strict.
    strict: Bool,
  )
}

/// Compile error from the emitter.
pub type EmitError {
  BreakOutsideLoop
  ContinueOutsideLoop
  Unsupported(description: String)
}

// ============================================================================
// Public API
// ============================================================================

/// Emit IR for a list of top-level statements (script body).
/// Returns the emitter ops, constants, child functions, and script strictness.
pub fn emit_program(
  stmts: List(ast.Statement),
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
  ),
  EmitError,
) {
  emit_program_common(stmts, True, False, emit_stmt, emit_stmt_tail)
}

/// Emit IR for REPL mode: top-level var/let/const skip DeclareVar so they
/// resolve to globals (which persist across REPL evaluations).
/// Nested scopes (blocks, functions) use normal emit_stmt.
pub fn emit_program_repl(
  stmts: List(ast.Statement),
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
  ),
  EmitError,
) {
  emit_program_common(stmts, False, False, emit_stmt_repl, emit_stmt_tail_repl)
}

/// Emit IR for a module body. Always strict mode.
/// Accepts raw module items and handles export default internally
/// by declaring a `*default*` local binding (per ES spec §16.2.1.6.2).
pub fn emit_module(
  items: List(ast.ModuleItem),
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
  ),
  EmitError,
) {
  let has_default_export =
    list.any(items, fn(item) {
      case item {
        ast.ExportDefaultDeclaration(_) -> True
        _ -> False
      }
    })
  let stmts = module_items_to_stmts(items)
  emit_module_common(stmts, has_default_export)
}

/// Module emission: sets up scope, hoists, handles *default* binding, emits body.
fn emit_module_common(
  stmts: List(ast.Statement),
  has_default_export: Bool,
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
  ),
  EmitError,
) {
  let e = Emitter(..new_emitter(), strict: True)
  let e = emit_op(e, EnterScope(FunctionScope))

  // Hoist var declarations
  let hoisted_vars = collect_hoisted_vars(stmts)
  let e =
    list.fold(hoisted_vars, e, fn(e, name) {
      emit_op(e, DeclareVar(name, VarBinding))
    })

  // Declare *default* binding if module has a default export
  let e = case has_default_export {
    True -> emit_op(e, DeclareVar("*default*", ConstBinding))
    False -> e
  }

  // Collect and emit hoisted function declarations
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)
  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(name, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(name))
      e
    })

  // Emit body with module-aware tail emitter
  use e <- result.try(emit_program_body_with(
    stmts,
    e,
    emit_stmt_module,
    emit_stmt_tail,
  ))

  let e = emit_op(e, LeaveScope)
  let #(code, constants, constants_map, children) = finish(e)
  Ok(#(code, constants, constants_map, children, True))
}

/// Convert module items to statements, stripping import/export wrappers.
/// ExportDefaultDeclaration becomes an assignment to *default* (the binding
/// is declared separately during module emission).
fn module_items_to_stmts(items: List(ast.ModuleItem)) -> List(ast.Statement) {
  list.filter_map(items, fn(item) {
    case item {
      ast.StatementItem(stmt) -> Ok(stmt)
      ast.ExportNamedDeclaration(option.Some(decl), _, _) -> Ok(decl)
      ast.ExportDefaultDeclaration(expr) ->
        // Emit as: *default* = expr;
        // The *default* local is declared during module hoisting.
        Ok(
          ast.ExpressionStatement(ast.AssignmentExpression(
            operator: ast.Assign,
            left: ast.Identifier("*default*"),
            right: expr,
          )),
        )
      ast.ImportDeclaration(..) -> Error(Nil)
      ast.ExportNamedDeclaration(None, _, _) -> Error(Nil)
      ast.ExportAllDeclaration(..) -> Error(Nil)
    }
  })
}

/// Module-mode statement emitter: identical to emit_stmt but handles
/// the *default* assignment correctly (no special casing needed since
/// it's a normal assignment to a declared local).
fn emit_stmt_module(
  e: Emitter,
  stmt: ast.Statement,
) -> Result(Emitter, EmitError) {
  emit_stmt(e, stmt)
}

/// Common program emission: sets up scope, hoists, emits body, tears down.
/// When hoist_vars is True, collects and emits DeclareVar for var declarations.
/// When False (REPL mode), skips DeclareVar so vars resolve to globals.
/// When force_strict is True, the program is always strict (modules).
fn emit_program_common(
  stmts: List(ast.Statement),
  hoist_vars: Bool,
  force_strict: Bool,
  emit_non_tail: fn(Emitter, ast.Statement) -> Result(Emitter, EmitError),
  emit_tail: fn(Emitter, ast.Statement) -> Result(Emitter, EmitError),
) -> Result(
  #(
    List(EmitterOp),
    List(JsValue),
    Dict(JsValue, Int),
    List(CompiledChild),
    Bool,
  ),
  EmitError,
) {
  // Detect top-level strict directive so child functions inherit.
  // Modules are always strict regardless of directives.
  let script_strict = force_strict || has_use_strict_directive(stmts)
  let e = Emitter(..new_emitter(), strict: script_strict)

  // Wrap in function scope
  let e = emit_op(e, EnterScope(FunctionScope))

  // Hoisting pre-pass: optionally collect var declarations
  let e = case hoist_vars {
    True -> {
      let hoisted_vars = collect_hoisted_vars(stmts)
      list.fold(hoisted_vars, e, fn(e, name) {
        emit_op(e, DeclareVar(name, VarBinding))
      })
    }
    False -> e
  }

  // Collect and emit hoisted function declarations
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)
  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(name, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(name))
      e
    })

  // Emit body — last statement in tail position keeps its value on stack
  use e <- result.try(emit_program_body_with(stmts, e, emit_non_tail, emit_tail))

  let e = emit_op(e, LeaveScope)
  let #(code, constants, constants_map, children) = finish(e)
  Ok(#(code, constants, constants_map, children, script_strict))
}

/// Emit program body using provided statement emitters.
/// The last statement is emitted with emit_tail (keeps value on stack),
/// all others use emit_non_tail (which pops the value).
fn emit_program_body_with(
  stmts: List(ast.Statement),
  e: Emitter,
  emit_non_tail: fn(Emitter, ast.Statement) -> Result(Emitter, EmitError),
  emit_tail: fn(Emitter, ast.Statement) -> Result(Emitter, EmitError),
) -> Result(Emitter, EmitError) {
  case stmts {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_tail(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_non_tail(e, first))
      emit_program_body_with(rest, e, emit_non_tail, emit_tail)
    }
  }
}

/// REPL statement emit: only differs from emit_stmt for VariableDeclaration
/// and FunctionDeclaration (skips DeclareVar so they resolve to globals).
/// For const declarations, emits MarkGlobalConst/UnmarkGlobalConst for enforcement.
/// All other statements delegate to normal emit_stmt.
fn emit_stmt_repl(e: Emitter, stmt: ast.Statement) -> Result(Emitter, EmitError) {
  case stmt {
    ast.VariableDeclaration(kind, declarators) -> {
      // Skip DeclareVar entirely — IrScopePutVar will resolve to PutGlobal
      list.try_fold(declarators, e, fn(e, decl) {
        case decl {
          ast.VariableDeclarator(ast.IdentifierPattern(name), init) -> {
            // Remove const protection to allow redeclaration
            let e = emit_ir(e, IrUnmarkGlobalConst(name))
            case init {
              Some(init_expr) -> {
                use e <- result.map(emit_expr(e, init_expr))
                let e = emit_ir(e, IrScopePutVar(name))
                // Mark as const if this is a const declaration
                case kind {
                  ast.Const -> emit_ir(e, IrMarkGlobalConst(name))
                  _ -> e
                }
              }
              None -> Ok(e)
            }
          }
          // Destructuring patterns — use VarBinding which skips DeclareVar
          ast.VariableDeclarator(pattern, init) -> {
            // For destructuring, unmark all bound names first
            let names = collect_pattern_names(pattern)
            let e =
              list.fold(names, e, fn(e, name) {
                emit_ir(e, IrUnmarkGlobalConst(name))
              })
            use e <- result.try(case init {
              Some(init_expr) -> emit_expr(e, init_expr)
              None -> Ok(push_const(e, JsUndefined))
            })
            use e <- result.map(emit_destructuring_bind(e, pattern, VarBinding))
            // Mark all as const if this is a const declaration
            case kind {
              ast.Const ->
                list.fold(names, e, fn(e, name) {
                  emit_ir(e, IrMarkGlobalConst(name))
                })
              _ -> e
            }
          }
        }
      })
    }

    // Function declarations are already handled by hoisting — skip
    ast.FunctionDeclaration(..) -> Ok(e)

    // Everything else delegates to normal emit_stmt
    _ -> emit_stmt(e, stmt)
  }
}

/// REPL tail position: handles the last statement in the REPL input.
fn emit_stmt_tail_repl(
  e: Emitter,
  stmt: ast.Statement,
) -> Result(Emitter, EmitError) {
  case stmt {
    ast.ExpressionStatement(expr) ->
      // Tail position: keep value on stack (no IrPop)
      emit_expr(e, expr)

    ast.VariableDeclaration(..) -> {
      // Emit the declaration via REPL path, then push undefined as completion value
      use e <- result.map(emit_stmt_repl(e, stmt))
      push_const(e, JsUndefined)
    }

    // All other statements: delegate to normal emit_stmt_tail
    // (which recurses into normal emit_stmt for nested blocks — correct
    // since those aren't top-level REPL declarations)
    _ -> emit_stmt_tail(e, stmt)
  }
}

// ============================================================================
// Emitter helpers
// ============================================================================

fn new_emitter() -> Emitter {
  Emitter(
    code: [],
    constants_map: dict.new(),
    constants_list: [],
    next_const: 0,
    next_label: 0,
    loop_stack: [],
    functions: [],
    next_func: 0,
    pending_label: None,
    strict: False,
  )
}

/// Check if a statement list begins with a Use Strict Directive.
/// ES2024 section 11.2.1 "Directive Prologues and the Use Strict Directive":
/// the directive prologue is the leading run of ExpressionStatements whose
/// expression is a string literal. "use strict" anywhere in that run makes
/// the function strict. We stop at the first non-string ExpressionStatement.
fn has_use_strict_directive(stmts: List(ast.Statement)) -> Bool {
  case stmts {
    [ast.ExpressionStatement(ast.StringExpression("use strict")), ..] -> True
    [ast.ExpressionStatement(ast.StringExpression(_)), ..rest] ->
      has_use_strict_directive(rest)
    _ -> False
  }
}

fn emit_op(e: Emitter, op: EmitterOp) -> Emitter {
  Emitter(..e, code: [op, ..e.code])
}

fn emit_ir(e: Emitter, op: IrOp) -> Emitter {
  emit_op(e, Ir(op))
}

fn add_constant(e: Emitter, val: JsValue) -> #(Emitter, Int) {
  case dict.get(e.constants_map, val) {
    Ok(idx) -> #(e, idx)
    Error(Nil) -> {
      let idx = e.next_const
      let e =
        Emitter(
          ..e,
          constants_map: dict.insert(e.constants_map, val, idx),
          constants_list: [val, ..e.constants_list],
          next_const: idx + 1,
        )
      #(e, idx)
    }
  }
}

fn push_const(e: Emitter, val: JsValue) -> Emitter {
  let #(e, idx) = add_constant(e, val)
  emit_ir(e, IrPushConst(idx))
}

fn fresh_label(e: Emitter) -> #(Emitter, Int) {
  let label = e.next_label
  #(Emitter(..e, next_label: label + 1), label)
}

fn push_loop(e: Emitter, break_label: Int, continue_label: Int) -> Emitter {
  let label = e.pending_label
  Emitter(
    ..e,
    loop_stack: [
      LoopContext(break_label:, continue_label:, label:),
      ..e.loop_stack
    ],
    pending_label: None,
  )
}

fn pop_loop(e: Emitter) -> Emitter {
  case e.loop_stack {
    [_, ..rest] -> Emitter(..e, loop_stack: rest)
    [] -> e
  }
}

fn find_label(
  stack: List(LoopContext),
  target: String,
) -> Result(LoopContext, Nil) {
  case stack {
    [] -> Error(Nil)
    [ctx, ..rest] ->
      case ctx.label {
        Some(l) if l == target -> Ok(ctx)
        _ -> find_label(rest, target)
      }
  }
}

fn add_child_function(e: Emitter, child: CompiledChild) -> #(Emitter, Int) {
  let idx = e.next_func
  #(
    Emitter(
      ..e,
      functions: list.append(e.functions, [child]),
      next_func: idx + 1,
    ),
    idx,
  )
}

/// Extract final results from the emitter.
fn finish(
  e: Emitter,
) -> #(List(EmitterOp), List(JsValue), Dict(JsValue, Int), List(CompiledChild)) {
  #(
    list.reverse(e.code),
    list.reverse(e.constants_list),
    e.constants_map,
    e.functions,
  )
}

/// Emit a statement in "tail" position — its completion value stays on stack.
/// For expression statements, this means NOT emitting IrPop.
/// For compound statements (blocks, if/else, try/catch), propagates tail into
/// the inner last statement.
fn emit_stmt_tail(e: Emitter, stmt: ast.Statement) -> Result(Emitter, EmitError) {
  case stmt {
    ast.ExpressionStatement(expr) ->
      // Tail position: keep value on stack (no IrPop)
      emit_expr(e, expr)

    ast.BlockStatement(body) -> {
      let e = emit_op(e, EnterScope(BlockScope))
      use e <- result.map(emit_stmts_tail(e, body))
      emit_op(e, LeaveScope)
    }

    ast.IfStatement(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_stmt_tail(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      let e = case alternate {
        Some(alt) -> {
          case emit_stmt_tail(e, alt) {
            Ok(e) -> e
            Error(_) -> e
          }
        }
        None -> push_const(e, JsUndefined)
      }
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    ast.TryStatement(block, handler, _finalizer) -> {
      case handler {
        Some(ast.CatchClause(param, catch_body)) -> {
          let #(e, catch_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(catch_label, -1))
          use e <- result.try(emit_stmt_tail(e, block))
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrJump(end_label))

          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))

          let e = case param {
            Some(pattern) -> {
              case emit_destructuring_bind(e, pattern, CatchBinding) {
                Ok(e) -> e
                Error(_) -> e
              }
            }
            None -> emit_ir(e, IrPop)
          }

          use e <- result.try(emit_stmt_tail(e, catch_body))
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }
        None -> emit_stmt_tail(e, block)
      }
    }

    // All other statements: delegate to regular emit_stmt, then push undefined
    // as the completion value
    _ -> {
      use e <- result.map(emit_stmt(e, stmt))
      push_const(e, JsUndefined)
    }
  }
}

/// Like emit_stmts but the last statement is emitted in tail position.
fn emit_stmts_tail(
  e: Emitter,
  stmts: List(ast.Statement),
) -> Result(Emitter, EmitError) {
  case stmts {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_stmt_tail(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_stmt(e, first))
      emit_stmts_tail(e, rest)
    }
  }
}

// ============================================================================
// Hoisting
// ============================================================================

/// Collect all var-declared names in a function body (not entering nested functions).
fn collect_hoisted_vars(stmts: List(ast.Statement)) -> List(String) {
  list.flat_map(stmts, collect_vars_stmt)
  |> list.unique()
}

/// Recursively extract all bound variable names from a pattern.
fn collect_pattern_names(pattern: ast.Pattern) -> List(String) {
  case pattern {
    ast.IdentifierPattern(name) -> [name]
    ast.ArrayPattern(elements) ->
      list.flat_map(elements, fn(elem) {
        case elem {
          Some(p) -> collect_pattern_names(p)
          None -> []
        }
      })
    ast.ObjectPattern(properties) ->
      list.flat_map(properties, fn(prop) {
        case prop {
          ast.PatternProperty(_, value:, ..) -> collect_pattern_names(value)
          ast.RestProperty(argument) -> collect_pattern_names(argument)
        }
      })
    ast.AssignmentPattern(left, _) -> collect_pattern_names(left)
    ast.RestElement(argument) -> collect_pattern_names(argument)
  }
}

fn collect_vars_stmt(stmt: ast.Statement) -> List(String) {
  case stmt {
    ast.VariableDeclaration(ast.Var, declarators) ->
      list.flat_map(declarators, fn(d) {
        case d {
          ast.VariableDeclarator(pattern, _) -> collect_pattern_names(pattern)
        }
      })
    ast.BlockStatement(body) -> list.flat_map(body, collect_vars_stmt)
    ast.IfStatement(_, consequent, alternate) -> {
      let then_vars = collect_vars_stmt(consequent)
      let else_vars = case alternate {
        Some(alt) -> collect_vars_stmt(alt)
        None -> []
      }
      list.append(then_vars, else_vars)
    }
    ast.WhileStatement(_, body) -> collect_vars_stmt(body)
    ast.DoWhileStatement(_, body) -> collect_vars_stmt(body)
    ast.ForStatement(init, _, _, body) -> {
      let init_vars = case init {
        Some(ast.ForInitDeclaration(ast.VariableDeclaration(ast.Var, decls))) ->
          list.flat_map(decls, fn(d) {
            case d {
              ast.VariableDeclarator(pattern, _) ->
                collect_pattern_names(pattern)
            }
          })
        _ -> []
      }
      list.append(init_vars, collect_vars_stmt(body))
    }
    ast.TryStatement(block, handler, finalizer) -> {
      let block_vars = case block {
        ast.BlockStatement(body) -> list.flat_map(body, collect_vars_stmt)
        _ -> collect_vars_stmt(block)
      }
      let handler_vars = case handler {
        Some(ast.CatchClause(_, body)) ->
          case body {
            ast.BlockStatement(b) -> list.flat_map(b, collect_vars_stmt)
            _ -> collect_vars_stmt(body)
          }
        None -> []
      }
      let finally_vars = case finalizer {
        Some(f) ->
          case f {
            ast.BlockStatement(b) -> list.flat_map(b, collect_vars_stmt)
            _ -> collect_vars_stmt(f)
          }
        None -> []
      }
      list.flatten([block_vars, handler_vars, finally_vars])
    }
    ast.ForInStatement(left, _, body) | ast.ForOfStatement(left, _, body, ..) -> {
      let left_vars = case left {
        ast.ForInitDeclaration(ast.VariableDeclaration(ast.Var, decls)) ->
          list.flat_map(decls, fn(d) {
            case d {
              ast.VariableDeclarator(pattern, _) ->
                collect_pattern_names(pattern)
            }
          })
        _ -> []
      }
      list.append(left_vars, collect_vars_stmt(body))
    }
    ast.LabeledStatement(_, body) -> collect_vars_stmt(body)
    ast.SwitchStatement(_, cases) ->
      list.flat_map(cases, fn(c) {
        case c {
          ast.SwitchCase(_, consequent) ->
            list.flat_map(consequent, collect_vars_stmt)
        }
      })
    // Function declarations: include the name for hoisting (DeclareVar)
    // but don't recurse into the body (nested scope).
    ast.FunctionDeclaration(Some(name), ..) -> [name]
    ast.FunctionDeclaration(None, ..) -> []
    _ -> []
  }
}

/// Collect and compile hoisted function declarations.
/// Returns updated emitter + list of (name, func_index) pairs.
fn collect_hoisted_funcs(
  e: Emitter,
  stmts: List(ast.Statement),
) -> #(Emitter, List(#(String, Int))) {
  list.fold(stmts, #(e, []), fn(acc, stmt) {
    let #(e, funcs) = acc
    case stmt {
      ast.FunctionDeclaration(Some(name), params, body, is_gen, is_async) -> {
        let child =
          compile_function_body(
            e,
            Some(name),
            params,
            body,
            False,
            is_gen,
            is_async,
          )
        let #(e, idx) = add_child_function(e, child)
        #(e, list.append(funcs, [#(name, idx)]))
      }
      _ -> #(e, funcs)
    }
  })
}

// ============================================================================
// `arguments` usage detection
// ============================================================================
//
// We walk the AST looking for `Identifier("arguments")`. The walk recurses
// into arrow function bodies (arrows inherit the enclosing `arguments`
// binding, just like `this`) but does NOT recurse into non-arrow function
// bodies, which have their own separate `arguments` binding.
//
// This is a compile-time scan so functions that never reference `arguments`
// pay zero allocation cost.

fn stmts_reference_arguments(stmts: List(ast.Statement)) -> Bool {
  list.any(stmts, stmt_references_arguments)
}

fn stmt_references_arguments(stmt: ast.Statement) -> Bool {
  case stmt {
    ast.EmptyStatement
    | ast.DebuggerStatement
    | ast.BreakStatement(_)
    | ast.ContinueStatement(_) -> False

    ast.ExpressionStatement(expr) -> expr_references_arguments(expr)
    ast.BlockStatement(body) -> stmts_reference_arguments(body)
    ast.ReturnStatement(arg) -> opt_expr_references_arguments(arg)
    ast.ThrowStatement(arg) -> expr_references_arguments(arg)

    ast.IfStatement(cond, cons, alt) ->
      expr_references_arguments(cond)
      || stmt_references_arguments(cons)
      || opt_stmt_references_arguments(alt)

    ast.WhileStatement(cond, body) ->
      expr_references_arguments(cond) || stmt_references_arguments(body)
    ast.DoWhileStatement(cond, body) ->
      expr_references_arguments(cond) || stmt_references_arguments(body)

    ast.ForStatement(init, cond, upd, body) ->
      opt_for_init_references_arguments(init)
      || opt_expr_references_arguments(cond)
      || opt_expr_references_arguments(upd)
      || stmt_references_arguments(body)

    ast.ForInStatement(left, right, body)
    | ast.ForOfStatement(left, right, body, ..) ->
      for_init_references_arguments(left)
      || expr_references_arguments(right)
      || stmt_references_arguments(body)

    ast.SwitchStatement(disc, cases) ->
      expr_references_arguments(disc)
      || list.any(cases, fn(c) {
        let ast.SwitchCase(cond, cons) = c
        opt_expr_references_arguments(cond) || stmts_reference_arguments(cons)
      })

    ast.TryStatement(block, handler, finalizer) ->
      stmt_references_arguments(block)
      || case handler {
        Some(ast.CatchClause(_, body)) -> stmt_references_arguments(body)
        None -> False
      }
      || opt_stmt_references_arguments(finalizer)

    ast.LabeledStatement(_, body) -> stmt_references_arguments(body)
    ast.WithStatement(obj, body) ->
      expr_references_arguments(obj) || stmt_references_arguments(body)

    ast.VariableDeclaration(_, decls) ->
      list.any(decls, fn(d) {
        let ast.VariableDeclarator(id, init) = d
        pattern_references_arguments(id) || opt_expr_references_arguments(init)
      })

    // Non-arrow function declaration: has its own `arguments` binding, do NOT
    // recurse into body. But DO check default param expressions (they run in
    // the enclosing scope before the new function's arguments is created).
    // Actually — spec-wise, default param exprs of a nested function have
    // access to the NESTED function's arguments, not the enclosing one.
    // So fully skip.
    ast.FunctionDeclaration(_, _, _, _, _) -> False

    ast.ClassDeclaration(_, super_class, body) ->
      opt_expr_references_arguments(super_class)
      || class_body_references_arguments(body)
  }
}

fn expr_references_arguments(expr: ast.Expression) -> Bool {
  case expr {
    ast.Identifier("arguments") -> True
    ast.Identifier(_) -> False

    ast.NumberLiteral(_)
    | ast.StringExpression(_)
    | ast.BooleanLiteral(_)
    | ast.NullLiteral
    | ast.UndefinedExpression
    | ast.ThisExpression
    | ast.SuperExpression
    | ast.MetaProperty(_, _)
    | ast.RegExpLiteral(_, _) -> False

    ast.BinaryExpression(_, l, r) | ast.LogicalExpression(_, l, r) ->
      expr_references_arguments(l) || expr_references_arguments(r)

    ast.UnaryExpression(_, _, arg)
    | ast.UpdateExpression(_, _, arg)
    | ast.AwaitExpression(arg)
    | ast.SpreadElement(arg)
    | ast.ImportExpression(arg) -> expr_references_arguments(arg)

    ast.YieldExpression(arg, _) -> opt_expr_references_arguments(arg)

    ast.AssignmentExpression(_, l, r) ->
      expr_references_arguments(l) || expr_references_arguments(r)

    ast.CallExpression(callee, args)
    | ast.OptionalCallExpression(callee, args)
    | ast.NewExpression(callee, args) ->
      expr_references_arguments(callee)
      || list.any(args, expr_references_arguments)

    ast.MemberExpression(obj, prop, computed)
    | ast.OptionalMemberExpression(obj, prop, computed) ->
      expr_references_arguments(obj)
      || case computed {
        True -> expr_references_arguments(prop)
        False -> False
      }

    ast.ConditionalExpression(c, t, a) ->
      expr_references_arguments(c)
      || expr_references_arguments(t)
      || expr_references_arguments(a)

    ast.ArrayExpression(elems) ->
      list.any(elems, fn(e) {
        case e {
          Some(ex) -> expr_references_arguments(ex)
          None -> False
        }
      })

    ast.ObjectExpression(props) ->
      list.any(props, fn(p) {
        case p {
          ast.Property(key, value, _, computed, _, _) ->
            case computed {
              True -> expr_references_arguments(key)
              False -> False
            }
            || expr_references_arguments(value)
          ast.SpreadProperty(arg) -> expr_references_arguments(arg)
        }
      })

    ast.SequenceExpression(exprs) -> list.any(exprs, expr_references_arguments)

    ast.TemplateLiteral(_, exprs) -> list.any(exprs, expr_references_arguments)

    ast.TaggedTemplateExpression(tag, quasi) ->
      expr_references_arguments(tag) || expr_references_arguments(quasi)

    // Non-arrow function expression: has its own `arguments`, skip entirely.
    ast.FunctionExpression(_, _, _, _, _) -> False

    // Arrow: inherits enclosing `arguments`, recurse into body AND default
    // param values (arrows have no own binding so `arguments` in defaults
    // also refers to the enclosing scope).
    ast.ArrowFunctionExpression(params, arrow_body, _) ->
      list.any(params, pattern_references_arguments)
      || case arrow_body {
        ast.ArrowBodyExpression(e) -> expr_references_arguments(e)
        ast.ArrowBodyBlock(s) -> stmt_references_arguments(s)
      }

    ast.ClassExpression(_, super_class, body) ->
      opt_expr_references_arguments(super_class)
      || class_body_references_arguments(body)
  }
}

fn opt_expr_references_arguments(e: Option(ast.Expression)) -> Bool {
  case e {
    Some(ex) -> expr_references_arguments(ex)
    None -> False
  }
}

fn opt_stmt_references_arguments(s: Option(ast.Statement)) -> Bool {
  case s {
    Some(stmt) -> stmt_references_arguments(stmt)
    None -> False
  }
}

fn for_init_references_arguments(init: ast.ForInit) -> Bool {
  case init {
    ast.ForInitExpression(e) -> expr_references_arguments(e)
    ast.ForInitDeclaration(s) -> stmt_references_arguments(s)
    ast.ForInitPattern(p) -> pattern_references_arguments(p)
  }
}

fn opt_for_init_references_arguments(init: Option(ast.ForInit)) -> Bool {
  case init {
    Some(i) -> for_init_references_arguments(i)
    None -> False
  }
}

fn pattern_references_arguments(p: ast.Pattern) -> Bool {
  // Patterns only contain expressions in default-value positions (AssignmentPattern)
  // and in computed object-pattern keys.
  case p {
    ast.IdentifierPattern(_) -> False
    ast.RestElement(inner) -> pattern_references_arguments(inner)
    ast.AssignmentPattern(left, right) ->
      pattern_references_arguments(left) || expr_references_arguments(right)
    ast.ArrayPattern(elems) ->
      list.any(elems, fn(e) {
        case e {
          Some(pat) -> pattern_references_arguments(pat)
          None -> False
        }
      })
    ast.ObjectPattern(props) ->
      list.any(props, fn(prop) {
        case prop {
          ast.PatternProperty(key, value, computed, _) ->
            case computed {
              True -> expr_references_arguments(key)
              False -> False
            }
            || pattern_references_arguments(value)
          ast.RestProperty(inner) -> pattern_references_arguments(inner)
        }
      })
  }
}

fn class_body_references_arguments(body: List(ast.ClassElement)) -> Bool {
  // Class methods are non-arrow functions — they have their own `arguments`.
  // We only need to scan: computed keys, field initialisers (which spec-wise
  // run in a scope where `arguments` from enclosing is NOT visible — but in
  // practice they run as method bodies on the instance; skip for safety),
  // and static blocks (which DO have their own `arguments` forbidden… skip).
  // For the detector's purposes, only computed keys matter here.
  list.any(body, fn(el) {
    case el {
      ast.ClassMethod(key, _, _, _, computed) ->
        case computed {
          True -> expr_references_arguments(key)
          False -> False
        }
      ast.ClassField(key, _, _, computed) ->
        case computed {
          True -> expr_references_arguments(key)
          False -> False
        }
      ast.StaticBlock(_) -> False
    }
  })
}

/// Compile a function body into a CompiledChild.
fn compile_function_body(
  parent: Emitter,
  name: Option(String),
  params: List(ast.Pattern),
  body: ast.Statement,
  is_arrow: Bool,
  is_generator: Bool,
  is_async: Bool,
) -> CompiledChild {
  let stmts = case body {
    ast.BlockStatement(s) -> s
    other -> [other]
  }

  // Strictness: inherit from parent, upgrade if body prologue has "use strict".
  // (Classes force strict at the call site by passing a strict parent emitter.)
  let child_strict = parent.strict || has_use_strict_directive(stmts)

  // Use a fresh emitter inheriting nothing from parent (except label counter
  // for uniqueness, and strictness).
  let e =
    Emitter(
      ..new_emitter(),
      next_label: parent.next_label,
      strict: child_strict,
    )

  let e = emit_op(e, EnterScope(FunctionScope))

  // Phase 1: Declare parameters (identifier or synthetic for destructuring)
  let #(e, destructured_params) =
    list.index_fold(params, #(e, []), fn(acc, param, idx) {
      let #(e, destr) = acc
      case param {
        ast.IdentifierPattern(pname) -> #(
          emit_op(e, DeclareVar(pname, ParamBinding)),
          destr,
        )
        _ -> {
          let synthetic = "$param_" <> int.to_string(idx)
          let e = emit_op(e, DeclareVar(synthetic, ParamBinding))
          #(e, list.append(destr, [#(synthetic, param)]))
        }
      }
    })

  // Detect whether the function body references `arguments`. We scan the body
  // AND the parameter patterns (default-value expressions can reference
  // `arguments`), recursing into arrow functions (which inherit the enclosing
  // arguments binding) but NOT into non-arrow nested functions (which have
  // their own). Only non-arrow functions get the binding — arrows resolve
  // `arguments` as a free variable captured from the enclosing scope.
  let uses_args = case is_arrow {
    True -> False
    False ->
      list.any(params, pattern_references_arguments)
      || stmts_reference_arguments(stmts)
  }
  // Declare `arguments` immediately after params so it's local; emit
  // IrCreateArguments to build the object at runtime from state.call_args,
  // then store it into the local slot. This must happen before parameter
  // destructuring so default-value expressions can use `arguments`.
  let e = case uses_args {
    True -> {
      let e = emit_op(e, DeclareVar("arguments", VarBinding))
      let e = emit_ir(e, IrCreateArguments)
      emit_ir(e, IrScopePutVar("arguments"))
    }
    False -> e
  }

  // Phase 2: Emit destructuring for non-identifier params
  let e =
    list.fold(destructured_params, e, fn(e, dp) {
      let #(synthetic, pattern) = dp
      let e = emit_ir(e, IrScopeGetVar(synthetic))
      case emit_destructuring_bind(e, pattern, LetBinding) {
        Ok(e) -> e
        Error(_) -> e
      }
    })

  // Hoisting for the function body
  let hoisted_vars = collect_hoisted_vars(stmts)
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)

  let e =
    list.fold(hoisted_vars, e, fn(e, vname) {
      emit_op(e, DeclareVar(vname, VarBinding))
    })

  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(fname, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(fname))
      e
    })

  // For generators, emit IrInitialYield after parameter setup and hoisting,
  // but before the function body. This suspends execution so the generator
  // returns the iterator object (caller must call .next() to start).
  // Async functions do NOT get InitialYield — they run eagerly until the first
  // await or completion.
  let e = case is_generator {
    True -> emit_ir(e, IrInitialYield)
    False -> e
  }

  // Emit body statements
  let e = case emit_stmts(e, stmts) {
    Ok(e) -> e
    Error(_) -> e
    // For MVP, compilation errors in function bodies are ignored
  }

  // Implicit return undefined at end
  let e = push_const(e, JsUndefined)
  let e = emit_ir(e, IrReturn)

  let e = emit_op(e, LeaveScope)
  let #(code, constants, constants_map, children) = finish(e)

  CompiledChild(
    name:,
    arity: list.length(params),
    code:,
    constants:,
    constants_map:,
    functions: children,
    is_strict: child_strict,
    is_arrow:,
    is_derived_constructor: False,
    is_generator:,
    is_async:,
  )
}

// ============================================================================
// Statement emission
// ============================================================================

fn emit_stmts(
  e: Emitter,
  stmts: List(ast.Statement),
) -> Result(Emitter, EmitError) {
  list.try_fold(stmts, e, emit_stmt)
}

fn emit_stmt(e: Emitter, stmt: ast.Statement) -> Result(Emitter, EmitError) {
  case stmt {
    ast.EmptyStatement | ast.DebuggerStatement -> Ok(e)

    ast.ExpressionStatement(expr) -> {
      use e <- result.map(emit_expr(e, expr))
      emit_ir(e, IrPop)
    }

    ast.BlockStatement(body) -> {
      let e = emit_op(e, EnterScope(BlockScope))
      use e <- result.map(emit_stmts(e, body))
      emit_op(e, LeaveScope)
    }

    ast.VariableDeclaration(kind, declarators) -> {
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        ast.Const -> ConstBinding
      }
      list.try_fold(declarators, e, fn(e, decl) {
        case decl {
          ast.VariableDeclarator(ast.IdentifierPattern(name), init) -> {
            // For let/const, emit declaration marker (var already hoisted)
            let e = case kind {
              ast.Let | ast.Const -> emit_op(e, DeclareVar(name, binding_kind))
              ast.Var -> e
            }
            // Emit initializer if present
            case init {
              Some(init_expr) -> {
                use e <- result.map(emit_expr(e, init_expr))
                emit_ir(e, IrScopePutVar(name))
              }
              None -> Ok(e)
            }
          }
          // Destructuring patterns
          ast.VariableDeclarator(pattern, init) -> {
            use e <- result.try(case init {
              Some(init_expr) -> emit_expr(e, init_expr)
              None -> Ok(push_const(e, JsUndefined))
            })
            emit_destructuring_bind(e, pattern, binding_kind)
          }
        }
      })
    }

    ast.IfStatement(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_stmt(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      let e = case alternate {
        Some(alt) -> {
          case emit_stmt(e, alt) {
            Ok(e) -> e
            Error(_) -> e
          }
        }
        None -> e
      }
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    ast.WhileStatement(condition, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)
      let e = push_loop(e, loop_end, loop_start)
      let e = emit_ir(e, IrLabel(loop_start))
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(loop_end))
      use e <- result.try(emit_stmt(e, body))
      let e = emit_ir(e, IrJump(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      Ok(e)
    }

    ast.DoWhileStatement(condition, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_cond) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)
      let e = push_loop(e, loop_end, loop_cond)
      let e = emit_ir(e, IrLabel(loop_start))
      use e <- result.try(emit_stmt(e, body))
      let e = emit_ir(e, IrLabel(loop_cond))
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfTrue(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      Ok(e)
    }

    ast.ForStatement(init, condition, update, body) -> {
      let #(e, loop_start) = fresh_label(e)
      let #(e, loop_continue) = fresh_label(e)
      let #(e, loop_end) = fresh_label(e)

      // For-init may create a block scope (for let/const)
      let e = emit_op(e, EnterScope(BlockScope))

      // Init
      let e = case init {
        Some(ast.ForInitExpression(expr)) -> {
          case emit_expr(e, expr) {
            Ok(e) -> emit_ir(e, IrPop)
            Error(_) -> e
          }
        }
        Some(ast.ForInitDeclaration(decl)) -> {
          case emit_stmt(e, decl) {
            Ok(e) -> e
            Error(_) -> e
          }
        }
        _ -> e
      }

      let e = push_loop(e, loop_end, loop_continue)
      let e = emit_ir(e, IrLabel(loop_start))

      // Condition
      let e = case condition {
        Some(cond) -> {
          case emit_expr(e, cond) {
            Ok(e) -> emit_ir(e, IrJumpIfFalse(loop_end))
            Error(_) -> e
          }
        }
        None -> e
      }

      // Body
      let e = case emit_stmt(e, body) {
        Ok(e) -> e
        Error(_) -> e
      }

      // Continue target
      let e = emit_ir(e, IrLabel(loop_continue))

      // Update
      let e = case update {
        Some(upd) -> {
          case emit_expr(e, upd) {
            Ok(e) -> emit_ir(e, IrPop)
            Error(_) -> e
          }
        }
        None -> e
      }

      let e = emit_ir(e, IrJump(loop_start))
      let e = emit_ir(e, IrLabel(loop_end))
      let e = pop_loop(e)
      let e = emit_op(e, LeaveScope)
      Ok(e)
    }

    ast.ReturnStatement(arg) -> {
      let e = case arg {
        Some(expr) -> {
          case emit_expr(e, expr) {
            Ok(e) -> e
            Error(_) -> push_const(e, JsUndefined)
          }
        }
        None -> push_const(e, JsUndefined)
      }
      let e = emit_ir(e, IrReturn)
      Ok(e)
    }

    ast.ThrowStatement(arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_ir(e, IrThrow)
    }

    ast.TryStatement(block, handler, finalizer) -> {
      case handler, finalizer {
        // try/catch (no finally)
        Some(ast.CatchClause(param, catch_body)), None -> {
          let #(e, catch_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(catch_label, -1))
          use e <- result.try(emit_stmt(e, block))
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrJump(end_label))

          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))
          let e = case param {
            Some(pattern) -> {
              case emit_destructuring_bind(e, pattern, CatchBinding) {
                Ok(e) -> e
                Error(_) -> e
              }
            }
            None -> emit_ir(e, IrPop)
          }

          use e <- result.try(emit_stmt(e, catch_body))
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }

        // try/finally (no catch)
        None, Some(finally_body) -> {
          let #(e, finally_throw_label) = fresh_label(e)
          let #(e, finally_body_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(finally_throw_label, -1))
          use e <- result.try(emit_stmt(e, block))
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrEnterFinally)
          let e = emit_ir(e, IrJump(finally_body_label))

          // Exception path: thrown value on stack from unwind_to_catch
          let e = emit_ir(e, IrLabel(finally_throw_label))
          let e = emit_ir(e, IrEnterFinallyThrow)

          // Finally body
          let e = emit_ir(e, IrLabel(finally_body_label))
          use e <- result.try(emit_stmt(e, finally_body))
          let e = emit_ir(e, IrLeaveFinally)
          Ok(e)
        }

        // try/catch/finally
        Some(ast.CatchClause(param, catch_body)), Some(finally_body) -> {
          let #(e, finally_throw_label) = fresh_label(e)
          let #(e, catch_label) = fresh_label(e)
          let #(e, finally_body_label) = fresh_label(e)

          // Outer try: catches exceptions from catch body → finally with ThrowCompletion
          let e = emit_ir(e, IrPushTry(finally_throw_label, -1))
          // Inner try: catches exceptions from try body → catch handler
          let e = emit_ir(e, IrPushTry(catch_label, -1))
          use e <- result.try(emit_stmt(e, block))
          let e = emit_ir(e, IrPopTry)
          // Pop inner try
          let e = emit_ir(e, IrPopTry)
          // Pop outer try
          let e = emit_ir(e, IrEnterFinally)
          let e = emit_ir(e, IrJump(finally_body_label))

          // Catch handler
          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))
          let e = case param {
            Some(pattern) -> {
              case emit_destructuring_bind(e, pattern, CatchBinding) {
                Ok(e) -> e
                Error(_) -> e
              }
            }
            None -> emit_ir(e, IrPop)
          }
          use e <- result.try(emit_stmt(e, catch_body))
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrPopTry)
          // Pop outer try
          let e = emit_ir(e, IrEnterFinally)
          let e = emit_ir(e, IrJump(finally_body_label))

          // Exception path (from catch body re-throwing)
          let e = emit_ir(e, IrLabel(finally_throw_label))
          let e = emit_ir(e, IrEnterFinallyThrow)

          // Finally body (shared by all paths)
          let e = emit_ir(e, IrLabel(finally_body_label))
          use e <- result.try(emit_stmt(e, finally_body))
          let e = emit_ir(e, IrLeaveFinally)
          Ok(e)
        }

        // try with neither catch nor finally (shouldn't happen per spec, but handle gracefully)
        None, None -> emit_stmt(e, block)
      }
    }

    ast.SwitchStatement(discriminant, cases) -> {
      emit_switch(e, discriminant, cases)
    }

    ast.BreakStatement(None) -> {
      case e.loop_stack {
        [ctx, ..] -> {
          let e = emit_ir(e, IrJump(ctx.break_label))
          Ok(e)
        }
        [] -> Error(BreakOutsideLoop)
      }
    }

    ast.BreakStatement(Some(label)) -> {
      case find_label(e.loop_stack, label) {
        Ok(ctx) -> Ok(emit_ir(e, IrJump(ctx.break_label)))
        Error(_) -> Error(BreakOutsideLoop)
      }
    }

    ast.ContinueStatement(None) -> {
      case e.loop_stack {
        [ctx, ..] -> {
          let e = emit_ir(e, IrJump(ctx.continue_label))
          Ok(e)
        }
        [] -> Error(ContinueOutsideLoop)
      }
    }

    ast.ContinueStatement(Some(label)) -> {
      case find_label(e.loop_stack, label) {
        Ok(ctx) -> Ok(emit_ir(e, IrJump(ctx.continue_label)))
        Error(_) -> Error(ContinueOutsideLoop)
      }
    }

    ast.LabeledStatement(label, body) -> {
      case body {
        // Labeled loop: set pending_label so the loop picks it up
        ast.WhileStatement(..)
        | ast.DoWhileStatement(..)
        | ast.ForStatement(..)
        | ast.ForInStatement(..)
        | ast.ForOfStatement(..) -> {
          let e = Emitter(..e, pending_label: Some(label))
          emit_stmt(e, body)
        }
        // Labeled non-loop: create a break-only target
        _ -> {
          let #(e, break_target) = fresh_label(e)
          let e =
            Emitter(..e, loop_stack: [
              LoopContext(
                break_label: break_target,
                continue_label: -1,
                label: Some(label),
              ),
              ..e.loop_stack
            ])
          use e <- result.map(emit_stmt(e, body))
          let e = pop_loop(e)
          emit_ir(e, IrLabel(break_target))
        }
      }
    }

    ast.FunctionDeclaration(..) -> {
      // Already hoisted — nothing to emit here
      Ok(e)
    }

    ast.ClassDeclaration(name, super_class, body) -> {
      case name {
        Some(n) -> {
          // Class names are block-scoped (like let)
          let e = emit_op(e, DeclareVar(n, LetBinding))
          use e <- result.map(compile_class(e, name, super_class, body))
          // compile_class leaves [ctor] on stack; PutVar pops it
          emit_ir(e, IrScopePutVar(n))
        }
        None -> Error(Unsupported("anonymous class declaration"))
      }
    }

    ast.ForInStatement(left, right, body) -> emit_for_in(e, left, right, body)

    ast.ForOfStatement(left, right, body, _is_await) ->
      emit_for_of(e, left, right, body)

    _ -> Error(Unsupported("statement: " <> string_inspect_stmt_kind(stmt)))
  }
}

// ============================================================================
// Expression emission
// ============================================================================

fn emit_expr(e: Emitter, expr: ast.Expression) -> Result(Emitter, EmitError) {
  case expr {
    // Literals
    ast.NumberLiteral(value) -> Ok(push_const(e, JsNumber(Finite(value))))
    ast.StringExpression(value) -> Ok(push_const(e, JsString(value)))
    ast.BooleanLiteral(value) -> Ok(push_const(e, JsBool(value)))
    ast.NullLiteral -> Ok(push_const(e, JsNull))
    ast.UndefinedExpression -> Ok(push_const(e, JsUndefined))

    // Identifier
    ast.Identifier("undefined") -> Ok(push_const(e, JsUndefined))
    ast.Identifier(name) -> Ok(emit_ir(e, IrScopeGetVar(name)))

    // Binary expressions
    ast.BinaryExpression(op, left, right) -> {
      use e <- result.try(emit_expr(e, left))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrBinOp(translate_binop(op)))
    }

    // Logical expressions (short-circuit)
    ast.LogicalExpression(ast.LogicalAnd, left, right) -> {
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfFalse(end_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    ast.LogicalExpression(ast.LogicalOr, left, right) -> {
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfTrue(end_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    ast.LogicalExpression(ast.NullishCoalescing, left, right) -> {
      let #(e, use_right_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, left))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(use_right_label))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(use_right_label))
      let e = emit_ir(e, IrPop)
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrLabel(end_label))
    }

    // Other logical ops are just binary ops
    ast.LogicalExpression(op, left, right) -> {
      use e <- result.try(emit_expr(e, left))
      use e <- result.map(emit_expr(e, right))
      emit_ir(e, IrBinOp(translate_binop(op)))
    }

    // Unary expressions
    ast.UnaryExpression(ast.TypeOf, _, ast.Identifier(name)) -> {
      // typeof x must NOT throw for undeclared variables
      Ok(emit_ir(e, IrScopeTypeofVar(name)))
    }

    ast.UnaryExpression(ast.TypeOf, _, arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_ir(e, IrTypeOf)
    }

    // delete expression — special handling per argument type
    ast.UnaryExpression(
      ast.Delete,
      _,
      ast.MemberExpression(obj, ast.Identifier(prop), False),
    ) -> {
      // delete obj.prop → emit obj, DeleteField(prop)
      use e <- result.map(emit_expr(e, obj))
      emit_ir(e, IrDeleteField(prop))
    }
    ast.UnaryExpression(
      ast.Delete,
      _,
      ast.MemberExpression(obj, key_expr, True),
    ) -> {
      // delete obj[key] → emit obj, emit key, DeleteElem
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, key_expr))
      emit_ir(e, IrDeleteElem)
    }
    ast.UnaryExpression(ast.Delete, _, ast.Identifier(_)) -> {
      // delete x → always true in sloppy mode (can't delete plain vars)
      Ok(push_const(e, JsBool(True)))
    }
    ast.UnaryExpression(ast.Delete, _, arg) -> {
      // delete <other expr> → evaluate for side effects, discard, push true
      use e <- result.map(emit_expr(e, arg))
      let e = emit_ir(e, IrPop)
      push_const(e, JsBool(True))
    }

    ast.UnaryExpression(op, _, arg) -> {
      use e <- result.map(emit_expr(e, arg))
      emit_ir(e, IrUnaryOp(translate_unaryop(op)))
    }

    // Update expressions (++/--)
    ast.UpdateExpression(op, prefix, ast.Identifier(name)) -> {
      let one = JsNumber(Finite(1.0))
      let bin_kind = case op {
        ast.Increment -> opcode.Add
        ast.Decrement -> opcode.Sub
      }
      case prefix {
        True -> {
          // ++x: get, add 1, dup (keep result), store
          let e = emit_ir(e, IrScopeGetVar(name))
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrScopePutVar(name))
          Ok(e)
        }
        False -> {
          // x++: get, dup (old value stays as result), add 1, store
          let e = emit_ir(e, IrScopeGetVar(name))
          let e = emit_ir(e, IrDup)
          let e = push_const(e, one)
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrScopePutVar(name))
          Ok(e)
        }
      }
    }

    // Assignment to identifier
    ast.AssignmentExpression(ast.Assign, ast.Identifier(name), right) -> {
      use e <- result.map(emit_expr(e, right))
      let e = emit_ir(e, IrDup)
      emit_ir(e, IrScopePutVar(name))
    }

    // Compound assignment to identifier
    ast.AssignmentExpression(op, ast.Identifier(name), right) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          let e = emit_ir(e, IrScopeGetVar(name))
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          let e = emit_ir(e, IrDup)
          emit_ir(e, IrScopePutVar(name))
        }
        Error(_) -> Error(Unsupported("assignment op"))
      }
    }

    // Assignment to dot member expression (obj.prop = val)
    ast.AssignmentExpression(
      ast.Assign,
      ast.MemberExpression(obj, ast.Identifier(prop), False),
      right,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.map(emit_expr(e, right))
      // Stack: [val, obj, ...] — PutField pops both, leaves val
      emit_ir(e, IrPutField(prop))
    }

    // Compound assignment to dot member (obj.prop += val)
    ast.AssignmentExpression(
      op,
      ast.MemberExpression(obj, ast.Identifier(prop), False),
      right,
    ) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          use e <- result.try(emit_expr(e, obj))
          let e = emit_ir(e, IrGetField2(prop))
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          emit_ir(e, IrPutField(prop))
        }
        Error(_) -> Error(Unsupported("assignment op"))
      }
    }

    // Assignment to computed member expression (obj[key] = val)
    ast.AssignmentExpression(
      ast.Assign,
      ast.MemberExpression(obj, key, True),
      right,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, right))
      // Stack: [obj, key, val] — PutElem expects [val, key, obj]
      emit_ir(e, IrPutElem)
    }

    // Compound assignment to computed member (obj[key] += val)
    ast.AssignmentExpression(op, ast.MemberExpression(obj, key, True), right) -> {
      case compound_to_binop(op) {
        Ok(bin_kind) -> {
          use e <- result.try(emit_expr(e, obj))
          use e <- result.try(emit_expr(e, key))
          // GetElem2 reads obj[key] but keeps obj+key on stack
          let e = emit_ir(e, IrGetElem2)
          use e <- result.map(emit_expr(e, right))
          let e = emit_ir(e, IrBinOp(bin_kind))
          // Stack: [obj, key, result] — PutElem consumes all three
          emit_ir(e, IrPutElem)
        }
        Error(_) -> Error(Unsupported("assignment op"))
      }
    }

    // super(args) — call parent constructor
    ast.CallExpression(ast.SuperExpression, args) ->
      // super(...) spread is rare and CallSuper's logic is complex (derived
      // constructor chain, this_binding TDZ). Defer spread-super; for now,
      // detect and error cleanly rather than miscompiling.
      case has_spread_arg(args) {
        True -> Error(Unsupported("spread in super() call"))
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, IrCallSuper(list.length(args)))
        }
      }

    // Method call: obj.method(args) — emits GetField2 + CallMethod for this binding.
    // Spread path: build args array after GetField2, then IrCallMethodApply.
    ast.CallExpression(
      ast.MemberExpression(obj, ast.Identifier(method_name), False),
      args,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, IrGetField2(method_name))
      case has_spread_arg(args) {
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, IrCallMethod(method_name, list.length(args)))
        }
        True -> {
          // Stack after GetField2: [method, receiver, ...]
          // Build args array on top, then CallMethodApply pops [args, method, receiver].
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallMethodApply)
        }
      }
    }
    // Computed method call: obj[key](args) — must bind `this` to obj.
    // GetElem2 leaves [method, key, receiver]; we shuffle to [method, receiver]
    // via Swap+Pop so CallMethod sees the same shape as the dot-access path.
    ast.CallExpression(ast.MemberExpression(obj, key, True), args) -> {
      use e <- result.try(emit_expr(e, obj))
      use e <- result.try(emit_expr(e, key))
      let e = emit_ir(e, IrGetElem2)
      // [method, key, receiver] → Swap → [key, method, receiver] → Pop → [method, receiver]
      let e = emit_ir(e, IrSwap)
      let e = emit_ir(e, IrPop)
      case has_spread_arg(args) {
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          // Static name unknown for computed access; CallMethod ignores name
          // at runtime anyway — it's informational only.
          emit_ir(e, IrCallMethod("[computed]", list.length(args)))
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallMethodApply)
        }
      }
    }
    // Regular call expression
    ast.CallExpression(callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      case has_spread_arg(args) {
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, opcode.IrCall(list.length(args)))
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallApply)
        }
      }
    }

    // Conditional (ternary)
    ast.ConditionalExpression(condition, consequent, alternate) -> {
      let #(e, else_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, condition))
      let e = emit_ir(e, IrJumpIfFalse(else_label))
      use e <- result.try(emit_expr(e, consequent))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(else_label))
      use e <- result.map(emit_expr(e, alternate))
      emit_ir(e, IrLabel(end_label))
    }

    // Sequence expression (comma operator)
    ast.SequenceExpression(exprs) -> emit_sequence(e, exprs)

    // Object literal
    ast.ObjectExpression(properties) -> {
      let e = emit_ir(e, IrNewObject)
      list.try_fold(properties, e, emit_object_property)
    }

    // Member expression (dot access)
    ast.MemberExpression(object, ast.Identifier(prop), False) -> {
      use e <- result.map(emit_expr(e, object))
      emit_ir(e, IrGetField(prop))
    }

    // Computed member expression (obj[key])
    ast.MemberExpression(object, property, True) -> {
      use e <- result.try(emit_expr(e, object))
      use e <- result.map(emit_expr(e, property))
      emit_ir(e, IrGetElem)
    }

    // Optional member expression (obj?.prop)
    ast.OptionalMemberExpression(object, ast.Identifier(prop), False) -> {
      let #(e, nullish_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, object))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(nullish_label))
      let e = emit_ir(e, IrGetField(prop))
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(nullish_label))
      let e = emit_ir(e, IrPop)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    // Optional computed member expression (obj?.[key])
    ast.OptionalMemberExpression(object, property, True) -> {
      let #(e, nullish_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, object))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(nullish_label))
      use e <- result.try(emit_expr(e, property))
      let e = emit_ir(e, IrGetElem)
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(nullish_label))
      let e = emit_ir(e, IrPop)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    // Optional call expression (fn?.())
    ast.OptionalCallExpression(callee, args) -> {
      let #(e, nullish_label) = fresh_label(e)
      let #(e, end_label) = fresh_label(e)
      use e <- result.try(emit_expr(e, callee))
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrJumpIfNullish(nullish_label))
      use e <- result.try(case has_spread_arg(args) {
        False -> {
          let arity = list.length(args)
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, opcode.IrCall(arity))
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallApply)
        }
      })
      let e = emit_ir(e, IrJump(end_label))
      let e = emit_ir(e, IrLabel(nullish_label))
      let e = emit_ir(e, IrPop)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrLabel(end_label))
      Ok(e)
    }

    // Array literal
    // Fast path (no spread, no holes): push N elements, IrArrayFrom(N).
    // Hole path (no spread, has holes): push only non-hole values,
    //   IrArrayFromWithHoles(N, hole_indices) builds a sparse array.
    // Slow path (any spread): push prefix, then incrementally
    //   IrArrayPush / IrArrayPushHole / IrArraySpread the rest.
    //   Mirrors QuickJS's OP_append approach.
    ast.ArrayExpression(elements) ->
      case has_spread_element(elements) {
        False -> emit_array_no_spread(e, elements)
        True -> emit_array_with_spread(e, elements)
      }

    // Function expression
    ast.FunctionExpression(name, params, body, is_gen, is_async) -> {
      let child =
        compile_function_body(e, name, params, body, False, is_gen, is_async)
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }

    // Arrow function expression
    ast.ArrowFunctionExpression(params, body, is_async) -> {
      let body_stmt = case body {
        ast.ArrowBodyExpression(expr) ->
          ast.BlockStatement([ast.ReturnStatement(Some(expr))])
        ast.ArrowBodyBlock(stmt) -> stmt
      }
      let child =
        compile_function_body(e, None, params, body_stmt, True, False, is_async)
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }

    // This expression
    ast.ThisExpression -> Ok(emit_ir(e, IrGetThis))

    // New expression: new Foo(args)
    ast.NewExpression(callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      case has_spread_arg(args) {
        False -> {
          use e <- result.map(list.try_fold(args, e, emit_expr))
          emit_ir(e, IrCallConstructor(list.length(args)))
        }
        True -> {
          use e <- result.map(emit_args_array_with_spread(e, args))
          emit_ir(e, IrCallConstructorApply)
        }
      }
    }

    // Template literal: `text ${expr} more`
    // Desugar to string concatenation: "" + "text " + expr + " more"
    ast.TemplateLiteral(quasis, expressions) ->
      emit_template_literal(e, quasis, expressions)

    // Class expression
    ast.ClassExpression(name, super_class, body) ->
      compile_class(e, name, super_class, body)

    // Yield expression (inside generator functions)
    ast.YieldExpression(argument, is_delegate) -> {
      case is_delegate {
        True -> Error(Unsupported("yield* (delegate yield)"))
        False -> {
          let e = case argument {
            Some(arg) -> {
              use e <- result.try(emit_expr(e, arg))
              Ok(e)
            }
            None -> Ok(push_const(e, JsUndefined))
          }
          use e <- result.map(e)
          emit_ir(e, IrYield)
        }
      }
    }

    ast.AwaitExpression(argument) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_ir(e, IrAwait)
    }

    _ -> Error(Unsupported("expression: " <> string_inspect_expr_kind(expr)))
  }
}

fn emit_template_literal(
  e: Emitter,
  quasis: List(String),
  expressions: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  // Template literal `a${x}b${y}c` has quasis=["a","b","c"], expressions=[x,y]
  // Desugar to: "a" + x + "b" + y + "c"
  case quasis {
    [] -> Ok(push_const(e, JsString("")))
    [first, ..rest_quasis] -> {
      // Start with the first quasi string
      let e = push_const(e, JsString(first))
      // Interleave: for each expression, Add it, then Add the next quasi
      emit_template_parts(e, expressions, rest_quasis)
    }
  }
}

fn emit_template_parts(
  e: Emitter,
  expressions: List(ast.Expression),
  quasis: List(String),
) -> Result(Emitter, EmitError) {
  case expressions, quasis {
    [expr, ..rest_exprs], [quasi, ..rest_quasis] -> {
      // Emit expression, concat with accumulator
      use e <- result.try(emit_expr(e, expr))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      // Emit next quasi string, concat
      let e = push_const(e, JsString(quasi))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      emit_template_parts(e, rest_exprs, rest_quasis)
    }
    // If there are trailing expressions without quasis (shouldn't happen but safe)
    [expr, ..rest_exprs], [] -> {
      use e <- result.try(emit_expr(e, expr))
      let e = emit_ir(e, IrBinOp(opcode.Add))
      emit_template_parts(e, rest_exprs, [])
    }
    // Done
    [], _ -> Ok(e)
  }
}

fn emit_switch(
  e: Emitter,
  discriminant: ast.Expression,
  cases: List(ast.SwitchCase),
) -> Result(Emitter, EmitError) {
  let #(e, end_label) = fresh_label(e)

  // Push break context for switch (break; exits the switch)
  // Preserve parent continue_label if inside a loop
  let parent_continue = case e.loop_stack {
    [ctx, ..] -> ctx.continue_label
    [] -> -1
  }
  let e = push_loop(e, end_label, parent_continue)

  // Emit discriminant — stays on stack through comparison phase
  use e <- result.try(emit_expr(e, discriminant))

  // Allocate labels: each non-default case gets a "found" trampoline label
  // and a "body" label. Default cases only get a "body" label.
  // The trampoline pops the discriminant then jumps to the body label.
  // This ensures the discriminant is off the stack for all body code,
  // allowing fall-through between case bodies to work correctly.
  let #(e, body_labels) =
    list.fold(cases, #(e, []), fn(acc, _case) {
      let #(e, labels) = acc
      let #(e, label) = fresh_label(e)
      #(e, list.append(labels, [label]))
    })

  // Allocate found (trampoline) labels for non-default cases
  let #(e, found_labels) =
    list.fold(cases, #(e, []), fn(acc, c) {
      let #(e, labels) = acc
      case c {
        ast.SwitchCase(Some(_), _) -> {
          let #(e, label) = fresh_label(e)
          #(e, list.append(labels, [Some(label)]))
        }
        ast.SwitchCase(None, _) -> #(e, list.append(labels, [None]))
      }
    })

  // Phase 1: Emit comparison jumps
  // For each case with a test: Dup discriminant, emit test, StrictEq, JumpIfTrue(found_N)
  let #(e, default_body_label) =
    list.index_fold(cases, #(e, option.None), fn(acc, c, idx) {
      let #(e, default_lbl) = acc
      case c {
        ast.SwitchCase(Some(test_expr), _) -> {
          let e = emit_ir(e, IrDup)
          case emit_expr(e, test_expr) {
            Ok(e) -> {
              let e = emit_ir(e, IrBinOp(opcode.StrictEq))
              let found_lbl = case list.drop(found_labels, idx) {
                [Some(l), ..] -> l
                _ -> end_label
              }
              let e = emit_ir(e, IrJumpIfTrue(found_lbl))
              #(e, default_lbl)
            }
            Error(_) -> #(e, default_lbl)
          }
        }
        ast.SwitchCase(None, _) -> {
          // Default case — record its body label
          let body_lbl = case list.drop(body_labels, idx) {
            [l, ..] -> Some(l)
            [] -> Some(end_label)
          }
          #(e, body_lbl)
        }
      }
    })

  // No match: pop discriminant and jump to default body or end
  let e = emit_ir(e, IrPop)
  let e = case default_body_label {
    Some(dl) -> emit_ir(e, IrJump(dl))
    None -> emit_ir(e, IrJump(end_label))
  }

  // Phase 2: Emit trampolines — each pops discriminant and jumps to body
  let e =
    list.index_fold(cases, e, fn(e, _c, idx) {
      case list.drop(found_labels, idx) {
        [Some(found_lbl), ..] -> {
          let e = emit_ir(e, IrLabel(found_lbl))
          let e = emit_ir(e, IrPop)
          let body_lbl = case list.drop(body_labels, idx) {
            [l, ..] -> l
            [] -> end_label
          }
          emit_ir(e, IrJump(body_lbl))
        }
        _ -> e
      }
    })

  // Phase 3: Emit case bodies (fall-through between them)
  let e =
    list.index_fold(cases, e, fn(e, c, idx) {
      let label = case list.drop(body_labels, idx) {
        [l, ..] -> l
        [] -> end_label
      }
      let e = emit_ir(e, IrLabel(label))
      case c {
        ast.SwitchCase(_, consequent) -> {
          case list.try_fold(consequent, e, emit_stmt) {
            Ok(e) -> e
            Error(_) -> e
          }
        }
      }
    })

  let e = emit_ir(e, IrLabel(end_label))
  let e = pop_loop(e)
  Ok(e)
}

fn emit_sequence(
  e: Emitter,
  exprs: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  case exprs {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_expr(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_expr(e, first))
      let e = emit_ir(e, IrPop)
      emit_sequence(e, rest)
    }
  }
}

/// Emit one property in an object literal. Object is already on the stack.
/// All handlers leave the object on the stack for the next property.
fn emit_object_property(
  e: Emitter,
  prop: ast.Property,
) -> Result(Emitter, EmitError) {
  case prop {
    // Static key: {name: value} or {"name": value}
    // → IrDefineField(name) — pops value, keeps obj.
    ast.Property(
      key: ast.Identifier(name),
      value:,
      kind: ast.Init,
      computed: False,
      ..,
    )
    | ast.Property(
        key: ast.StringExpression(name),
        value:,
        kind: ast.Init,
        computed: False,
        ..,
      ) -> {
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineField(name))
    }

    // Numeric literal key: {1: "a"} — not computed in the AST, but needs
    // ToPropertyKey at runtime to get the canonical string form ("1" not "1.0").
    // Route through IrDefineFieldComputed which calls put_elem_value → js_to_string.
    ast.Property(
      key: ast.NumberLiteral(n),
      value:,
      kind: ast.Init,
      computed: False,
      ..,
    ) -> {
      let e = push_const(e, JsNumber(Finite(n)))
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineFieldComputed)
    }

    // Computed key: {[expr]: value}
    // Emit key, emit value, IrDefineFieldComputed — pops both, keeps obj.
    // The VM handles ToPropertyKey (Symbol preserved, else ToString).
    ast.Property(key:, value:, kind: ast.Init, computed: True, ..) -> {
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineFieldComputed)
    }

    // Spread: {...source}
    // IrObjectSpread pops source, copies own enumerable props, keeps obj.
    // null/undefined sources are no-ops per CopyDataProperties spec.
    ast.SpreadProperty(argument:) -> {
      use e <- result.map(emit_expr(e, argument))
      emit_ir(e, IrObjectSpread)
    }

    // Getter: { get name() { ... } }
    // Emit the function, then DefineAccessor(name, Getter).
    ast.Property(
      key: ast.Identifier(name),
      value:,
      kind: ast.Get,
      computed: False,
      ..,
    )
    | ast.Property(
        key: ast.StringExpression(name),
        value:,
        kind: ast.Get,
        computed: False,
        ..,
      ) -> {
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineAccessor(name, opcode.Getter))
    }

    // Setter: { set name(v) { ... } }
    ast.Property(
      key: ast.Identifier(name),
      value:,
      kind: ast.Set,
      computed: False,
      ..,
    )
    | ast.Property(
        key: ast.StringExpression(name),
        value:,
        kind: ast.Set,
        computed: False,
        ..,
      ) -> {
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineAccessor(name, opcode.Setter))
    }

    // Computed or exotic-key getter/setter: { get [expr]() {} }
    // Stack: emit key, emit fn → DefineAccessorComputed
    ast.Property(key:, value:, kind: ast.Get, ..) -> {
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineAccessorComputed(opcode.Getter))
    }
    ast.Property(key:, value:, kind: ast.Set, ..) -> {
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineAccessorComputed(opcode.Setter))
    }

    // Remaining case: non-computed Init with an exotic key expression
    // (shouldn't happen — parser only produces Identifier/StringExpression/
    // NumberLiteral for non-computed keys). Route through computed path anyway.
    ast.Property(key:, value:, kind: ast.Init, computed: False, ..) -> {
      use e <- result.try(emit_expr(e, key))
      use e <- result.map(emit_expr(e, value))
      emit_ir(e, IrDefineFieldComputed)
    }
  }
}

// ============================================================================
// Spread element support — array literals and call argument lists
// ============================================================================

/// Emit an array literal that contains no SpreadElement (ES2024 section
/// 13.2.4 "Array Initializer" — the non-spread case). Decides between the
/// dense fast path (IrArrayFrom) and the sparse path (IrArrayFromWithHoles)
/// based on whether any element is an Elision (None in the AST).
///
/// Single pass over elements: push non-hole values onto the stack, collect
/// hole indices. Accumulator threads #(emitter, index, holes_rev).
fn emit_array_no_spread(
  e: Emitter,
  elements: List(Option(ast.Expression)),
) -> Result(Emitter, EmitError) {
  let count = list.length(elements)
  use #(e, _idx, holes_rev) <- result.map(
    list.try_fold(elements, #(e, 0, []), fn(acc, elem) {
      let #(e, idx, holes_rev) = acc
      case elem {
        Some(expr) -> {
          use e <- result.map(emit_expr(e, expr))
          #(e, idx + 1, holes_rev)
        }
        None -> Ok(#(e, idx + 1, [idx, ..holes_rev]))
      }
    }),
  )
  case holes_rev {
    [] -> emit_ir(e, IrArrayFrom(count))
    _ -> emit_ir(e, IrArrayFromWithHoles(count, list.reverse(holes_rev)))
  }
}

/// Emit the prefix of a spread-mode array literal (elements before the first
/// SpreadElement). Delegates to emit_array_no_spread; factored out so the
/// spread path can build the initial array then append spread elements.
fn emit_array_prefix(
  e: Emitter,
  prefix: List(Option(ast.Expression)),
) -> Result(Emitter, EmitError) {
  emit_array_no_spread(e, prefix)
}

/// True if any element is Some(SpreadElement(_)). Used to choose the
/// fast static-arity path vs the incremental-build spread path.
fn has_spread_element(elements: List(Option(ast.Expression))) -> Bool {
  list.any(elements, fn(el) {
    case el {
      Some(ast.SpreadElement(_)) -> True
      _ -> False
    }
  })
}

/// True if any arg is SpreadElement(_). Call argument lists use plain
/// List(Expression), not List(Option(Expression)), so no hole case here.
fn has_spread_arg(args: List(ast.Expression)) -> Bool {
  list.any(args, fn(a) {
    case a {
      ast.SpreadElement(_) -> True
      _ -> False
    }
  })
}

/// Emit an array literal that contains at least one SpreadElement.
///
/// Strategy (QuickJS-style):
///   1. Peel off the leading non-spread run (prefix), push those elements,
///      then IrArrayFrom / IrArrayFromWithHoles to pack them. This handles
///      the common `[a, b, ...rest]` shape in one opcode.
///   2. For each remaining element, emit:
///      - IrArraySpread (drain iterator into array) for spread elements
///      - IrArrayPush (single append) for regular elements
///      - IrArrayPushHole (increment length, no element) for holes
///
/// Stack invariant throughout step 2: array is on top; each IrArrayPush /
/// IrArraySpread consumes [val-or-iter, arr] → [arr]; IrArrayPushHole
/// consumes [arr] → [arr].
///
/// Holes in the *source* of a spread become undefined per the array
/// iterator spec — that's a different thing from holes in the literal.
fn emit_array_with_spread(
  e: Emitter,
  elements: List(Option(ast.Expression)),
) -> Result(Emitter, EmitError) {
  // Split at first spread: prefix has no spreads, tail starts at first spread.
  let #(prefix, tail) = split_at_first_spread_element(elements)

  // Pack the prefix (handles holes via IrArrayFromWithHoles if needed).
  use e <- result.try(emit_array_prefix(e, prefix))

  // Incrementally append the tail.
  list.try_fold(tail, e, fn(e, elem) {
    case elem {
      Some(ast.SpreadElement(argument:)) -> {
        use e <- result.map(emit_expr(e, argument))
        emit_ir(e, IrArraySpread)
      }
      Some(expr) -> {
        use e <- result.map(emit_expr(e, expr))
        emit_ir(e, IrArrayPush)
      }
      None ->
        // Hole after a spread — increment length without setting element.
        Ok(emit_ir(e, IrArrayPushHole))
    }
  })
}

/// Build an args array for a spread-call (f(a, ...b, c) etc).
/// Same algorithm as emit_array_with_spread but over List(Expression)
/// (call args have no holes — the parser doesn't produce them in arglists).
/// Leaves the args array on top of the stack; caller follows with an
/// IrCallApply / IrCallMethodApply / IrCallConstructorApply.
fn emit_args_array_with_spread(
  e: Emitter,
  args: List(ast.Expression),
) -> Result(Emitter, EmitError) {
  let #(prefix, tail) = split_at_first_spread_arg(args)

  let prefix_count = list.length(prefix)
  use e <- result.try(list.try_fold(prefix, e, emit_expr))
  let e = emit_ir(e, IrArrayFrom(prefix_count))

  list.try_fold(tail, e, fn(e, arg) {
    case arg {
      ast.SpreadElement(argument:) -> {
        use e <- result.map(emit_expr(e, argument))
        emit_ir(e, IrArraySpread)
      }
      _ -> {
        use e <- result.map(emit_expr(e, arg))
        emit_ir(e, IrArrayPush)
      }
    }
  })
}

/// Split an array-literal element list at the first SpreadElement.
/// Returns (prefix_with_no_spreads, tail_starting_at_first_spread).
/// If no spread exists, tail is [] — but callers have already checked
/// has_spread_element so tail is always non-empty in practice.
fn split_at_first_spread_element(
  elements: List(Option(ast.Expression)),
) -> #(List(Option(ast.Expression)), List(Option(ast.Expression))) {
  split_at_first_spread_element_loop(elements, [])
}

fn split_at_first_spread_element_loop(
  remaining: List(Option(ast.Expression)),
  acc: List(Option(ast.Expression)),
) -> #(List(Option(ast.Expression)), List(Option(ast.Expression))) {
  case remaining {
    [] -> #(list.reverse(acc), [])
    [Some(ast.SpreadElement(_)), ..] -> #(list.reverse(acc), remaining)
    [el, ..rest] -> split_at_first_spread_element_loop(rest, [el, ..acc])
  }
}

fn split_at_first_spread_arg(
  args: List(ast.Expression),
) -> #(List(ast.Expression), List(ast.Expression)) {
  split_at_first_spread_arg_loop(args, [])
}

fn split_at_first_spread_arg_loop(
  remaining: List(ast.Expression),
  acc: List(ast.Expression),
) -> #(List(ast.Expression), List(ast.Expression)) {
  case remaining {
    [] -> #(list.reverse(acc), [])
    [ast.SpreadElement(_), ..] -> #(list.reverse(acc), remaining)
    [a, ..rest] -> split_at_first_spread_arg_loop(rest, [a, ..acc])
  }
}

// ============================================================================
// For-in / for-of loops
// ============================================================================

/// Emit a for-in loop: `for (lhs in rhs) body`
///
/// Stack pattern:
///   [obj] → ForInStart → [iterator]
///   loop: ForInNext → [iterator, key, done]
///   JumpIfTrue(cleanup) → [iterator, key]
///   bind key → [iterator]
///   body
///   Jump(loop) → cleanup: Pop key → loop_end: Pop iterator
fn emit_for_in(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, cleanup) = fresh_label(e)
  let #(e, loop_end) = fresh_label(e)

  // Block scope for let/const
  let e = emit_op(e, EnterScope(BlockScope))

  // Evaluate the right-hand side (object to iterate)
  use e <- result.try(emit_expr(e, right))
  // ForInStart: pops object, pushes iterator ref
  let e = emit_ir(e, IrForInStart)

  let e = push_loop(e, loop_end, loop_continue)
  let e = emit_ir(e, IrLabel(loop_start))

  // ForInNext: peeks iterator, pushes key + done
  let e = emit_ir(e, IrForInNext)
  // If done, jump to cleanup (where we pop the unused key)
  let e = emit_ir(e, IrJumpIfTrue(cleanup))

  // Bind the key to the left-hand side variable
  use e <- result.try(emit_for_lhs_bind(e, left))

  // Body
  use e <- result.try(emit_stmt(e, body))

  // Continue point
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // cleanup: pop the key (done=true left it on stack)
  let e = emit_ir(e, IrLabel(cleanup))
  let e = emit_ir(e, IrPop)

  // loop_end: pop the iterator
  let e = emit_ir(e, IrLabel(loop_end))
  let e = emit_ir(e, IrPop)

  let e = pop_loop(e)
  let e = emit_op(e, LeaveScope)
  Ok(e)
}

/// Emit a for-of loop: `for (lhs of rhs) body`
///
/// Same stack pattern as for-in but uses GetIterator/IteratorNext.
fn emit_for_of(
  e: Emitter,
  left: ast.ForInit,
  right: ast.Expression,
  body: ast.Statement,
) -> Result(Emitter, EmitError) {
  let #(e, loop_start) = fresh_label(e)
  let #(e, loop_continue) = fresh_label(e)
  let #(e, cleanup) = fresh_label(e)
  let #(e, loop_end) = fresh_label(e)

  // Block scope for let/const
  let e = emit_op(e, EnterScope(BlockScope))

  // Evaluate the iterable
  use e <- result.try(emit_expr(e, right))
  // GetIterator: pops iterable, pushes iterator ref
  let e = emit_ir(e, IrGetIterator)

  let e = push_loop(e, loop_end, loop_continue)
  let e = emit_ir(e, IrLabel(loop_start))

  // IteratorNext: peeks iterator, pushes value + done
  let e = emit_ir(e, IrIteratorNext)
  // If done, jump to cleanup (where we pop the unused value)
  let e = emit_ir(e, IrJumpIfTrue(cleanup))

  // Bind the value to the left-hand side
  use e <- result.try(emit_for_lhs_bind(e, left))

  // Body
  use e <- result.try(emit_stmt(e, body))

  // Continue point
  let e = emit_ir(e, IrLabel(loop_continue))
  let e = emit_ir(e, IrJump(loop_start))

  // cleanup: pop the value (done=true left it on stack)
  let e = emit_ir(e, IrLabel(cleanup))
  let e = emit_ir(e, IrPop)

  // loop_end: pop the iterator
  let e = emit_ir(e, IrLabel(loop_end))
  let e = emit_ir(e, IrPop)

  let e = pop_loop(e)
  let e = emit_op(e, LeaveScope)
  Ok(e)
}

/// Bind the current value (on top of stack) to the for-in/for-of LHS.
/// The LHS can be:
///   - ForInitDeclaration(VariableDeclaration(...)) e.g. `for (let x ...)`
///   - ForInitExpression(Identifier(name)) e.g. `for (x ...)`
///   - ForInitPattern(pattern) e.g. `for ({a, b} ...)`
/// Consumes the value on top of stack.
fn emit_for_lhs_bind(
  e: Emitter,
  left: ast.ForInit,
) -> Result(Emitter, EmitError) {
  case left {
    ast.ForInitDeclaration(ast.VariableDeclaration(kind, declarators)) -> {
      let binding_kind = case kind {
        ast.Var -> VarBinding
        ast.Let -> LetBinding
        ast.Const -> ConstBinding
      }
      case declarators {
        [ast.VariableDeclarator(pattern, _)] ->
          emit_destructuring_bind(e, pattern, binding_kind)
        _ -> Error(Unsupported("for-in/of with multiple declarators"))
      }
    }
    ast.ForInitExpression(ast.Identifier(name)) -> {
      Ok(emit_ir(e, IrScopePutVar(name)))
    }
    ast.ForInitExpression(ast.MemberExpression(obj, ast.Identifier(prop), False)) -> {
      // e.g. for (obj.prop in ...) — rare but valid
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, IrSwap)
      Ok(emit_ir(e, IrPutField(prop)))
    }
    ast.ForInitPattern(pattern) ->
      emit_destructuring_bind(e, pattern, VarBinding)
    _ -> Error(Unsupported("for-in/of left-hand side"))
  }
}

// ============================================================================
// Destructuring patterns
// ============================================================================

/// Emit code to destructure a value on top of stack into a pattern.
/// Consumes the value (pops it when done).
fn emit_destructuring_bind(
  e: Emitter,
  pattern: ast.Pattern,
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case pattern {
    ast.IdentifierPattern(name) -> {
      let e = case binding_kind {
        LetBinding | ConstBinding | ParamBinding | CatchBinding ->
          emit_op(e, DeclareVar(name, binding_kind))
        VarBinding | CaptureBinding -> e
      }
      Ok(emit_ir(e, IrScopePutVar(name)))
    }

    ast.ObjectPattern(properties) ->
      emit_object_destructure(e, properties, binding_kind)

    ast.ArrayPattern(elements) ->
      emit_array_destructure(e, elements, binding_kind)

    ast.AssignmentPattern(left, default_expr) -> {
      // Check if value === undefined, if so use default
      let #(e, has_val) = fresh_label(e)
      let e = emit_ir(e, IrDup)
      let e = push_const(e, JsUndefined)
      let e = emit_ir(e, IrBinOp(opcode.StrictEq))
      let e = emit_ir(e, IrJumpIfFalse(has_val))
      // Value is undefined — pop it and use default
      let e = emit_ir(e, IrPop)
      use e <- result.try(emit_expr(e, default_expr))
      let e = emit_ir(e, IrLabel(has_val))
      // Now the value (original or default) is on stack
      emit_destructuring_bind(e, left, binding_kind)
    }

    ast.RestElement(_) -> {
      let _e = emit_ir(e, IrPop)
      Error(Unsupported("rest element in destructuring"))
    }
  }
}

/// Destructure an object: for each property, Dup obj, GetField, recurse; then Pop obj.
fn emit_object_destructure(
  e: Emitter,
  properties: List(ast.PatternProperty),
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_object_props(e, properties, binding_kind))
  emit_ir(e, IrPop)
}

fn emit_object_props(
  e: Emitter,
  properties: List(ast.PatternProperty),
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case properties {
    [] -> Ok(e)
    [prop, ..rest] -> {
      use e <- result.try(emit_single_object_prop(e, prop, binding_kind))
      emit_object_props(e, rest, binding_kind)
    }
  }
}

fn emit_single_object_prop(
  e: Emitter,
  prop: ast.PatternProperty,
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case prop {
    ast.PatternProperty(key:, value:, computed: False, ..) -> {
      let field_name = case key {
        ast.Identifier(name) -> Ok(name)
        ast.StringExpression(name) -> Ok(name)
        _ -> Error(Unsupported("computed property key in destructuring"))
      }
      use name <- result.try(field_name)
      let e = emit_ir(e, IrDup)
      let e = emit_ir(e, IrGetField(name))
      emit_destructuring_bind(e, value, binding_kind)
    }
    ast.PatternProperty(computed: True, ..) ->
      Error(Unsupported("computed property in destructuring"))
    ast.RestProperty(_) -> Error(Unsupported("rest property in destructuring"))
  }
}

/// Destructure an array: for each element, Dup arr, PushConst(index), GetElem, recurse; then Pop arr.
fn emit_array_destructure(
  e: Emitter,
  elements: List(Option(ast.Pattern)),
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  use e <- result.map(emit_array_elements(e, elements, 0, binding_kind))
  emit_ir(e, IrPop)
}

fn emit_array_elements(
  e: Emitter,
  elements: List(Option(ast.Pattern)),
  index: Int,
  binding_kind: BindingKind,
) -> Result(Emitter, EmitError) {
  case elements {
    [] -> Ok(e)
    [None, ..rest] -> emit_array_elements(e, rest, index + 1, binding_kind)
    [Some(pattern), ..rest] -> {
      let e = emit_ir(e, IrDup)
      let e = push_const(e, JsNumber(Finite(int.to_float(index))))
      let e = emit_ir(e, IrGetElem)
      use e <- result.try(emit_destructuring_bind(e, pattern, binding_kind))
      emit_array_elements(e, rest, index + 1, binding_kind)
    }
  }
}

// ============================================================================
// Operator translation
// ============================================================================

fn translate_binop(op: ast.BinaryOp) -> opcode.BinOpKind {
  case op {
    ast.Add -> opcode.Add
    ast.Subtract -> opcode.Sub
    ast.Multiply -> opcode.Mul
    ast.Divide -> opcode.Div
    ast.Modulo -> opcode.Mod
    ast.Exponentiation -> opcode.Exp
    ast.StrictEqual -> opcode.StrictEq
    ast.StrictNotEqual -> opcode.StrictNotEq
    ast.Equal -> opcode.Eq
    ast.NotEqual -> opcode.NotEq
    ast.LessThan -> opcode.Lt
    ast.GreaterThan -> opcode.Gt
    ast.LessThanEqual -> opcode.LtEq
    ast.GreaterThanEqual -> opcode.GtEq
    ast.LeftShift -> opcode.ShiftLeft
    ast.RightShift -> opcode.ShiftRight
    ast.UnsignedRightShift -> opcode.UShiftRight
    ast.BitwiseAnd -> opcode.BitAnd
    ast.BitwiseOr -> opcode.BitOr
    ast.BitwiseXor -> opcode.BitXor
    ast.In -> opcode.In
    ast.InstanceOf -> opcode.InstanceOf
    // Logical ops should not reach here (handled separately)
    ast.LogicalAnd | ast.LogicalOr | ast.NullishCoalescing -> opcode.Add
  }
}

fn translate_unaryop(op: ast.UnaryOp) -> opcode.UnaryOpKind {
  case op {
    ast.Negate -> opcode.Neg
    ast.UnaryPlus -> opcode.Pos
    ast.LogicalNot -> opcode.LogicalNot
    ast.BitwiseNot -> opcode.BitNot
    ast.Void -> opcode.Void
    // TypeOf handled separately, Delete not in MVP
    ast.TypeOf -> opcode.Void
    ast.Delete -> opcode.Void
  }
}

fn compound_to_binop(op: ast.AssignmentOp) -> Result(opcode.BinOpKind, Nil) {
  case op {
    ast.AddAssign -> Ok(opcode.Add)
    ast.SubtractAssign -> Ok(opcode.Sub)
    ast.MultiplyAssign -> Ok(opcode.Mul)
    ast.DivideAssign -> Ok(opcode.Div)
    ast.ModuloAssign -> Ok(opcode.Mod)
    ast.ExponentiationAssign -> Ok(opcode.Exp)
    ast.LeftShiftAssign -> Ok(opcode.ShiftLeft)
    ast.RightShiftAssign -> Ok(opcode.ShiftRight)
    ast.UnsignedRightShiftAssign -> Ok(opcode.UShiftRight)
    ast.BitwiseAndAssign -> Ok(opcode.BitAnd)
    ast.BitwiseOrAssign -> Ok(opcode.BitOr)
    ast.BitwiseXorAssign -> Ok(opcode.BitXor)
    ast.Assign -> Error(Nil)
    ast.LogicalAndAssign | ast.LogicalOrAssign | ast.NullishCoalesceAssign ->
      Error(Nil)
  }
}

// ============================================================================
// Class compilation
// ============================================================================

/// Compile a class body. Leaves the constructor function on the stack.
///
/// Strategy:
///   1. Extract or synthesize constructor
///   2. Inject field initializer code into constructor body
///   3. MakeClosure for the constructor
///   4. Define instance methods on ctor.prototype (non-enumerable)
///   5. Define static methods on ctor (non-enumerable)
fn compile_class(
  e: Emitter,
  name: Option(String),
  super_class: Option(ast.Expression),
  body: List(ast.ClassElement),
) -> Result(Emitter, EmitError) {
  // ES spec: class bodies are always strict (§15.7.1 "A class body is always
  // strict mode code."). Force strict on the emitter so all compile_function_body
  // calls for methods/constructor inherit it. Restore enclosing strictness on
  // exit so a sloppy-mode caller isn't polluted.
  let saved_strict = e.strict
  let e = Emitter(..e, strict: True)
  use e <- result.map(case super_class {
    Some(parent_expr) -> compile_derived_class(e, name, parent_expr, body)
    None -> compile_base_class(e, name, body)
  })
  Emitter(..e, strict: saved_strict)
}

fn compile_derived_class(
  e: Emitter,
  name: Option(String),
  parent_expr: ast.Expression,
  body: List(ast.ClassElement),
) -> Result(Emitter, EmitError) {
  let #(ctor_method, instance_methods, static_methods, instance_fields) =
    classify_class_body(body)

  // Build constructor: if none provided, synthesize default derived constructor
  // Default: constructor(...args) { super(...args); }
  // Simplified: constructor() { super(); }
  let #(ctor_params, ctor_body) = case ctor_method {
    Some(ast.ClassMethod(value: ast.FunctionExpression(_, params, body, ..), ..)) -> #(
      params,
      body,
    )
    _ -> #(
      [],
      ast.BlockStatement([
        ast.ExpressionStatement(ast.CallExpression(ast.SuperExpression, [])),
      ]),
    )
  }

  // Compile constructor with field initializer preamble
  // For derived classes, field inits go AFTER super() call
  // (but for simplicity, if user wrote explicit ctor we trust them;
  //  for default ctor, fields need to go after super())
  let ctor_body_with_fields = case ctor_method {
    Some(_) -> inject_field_inits(instance_fields, ctor_body)
    None ->
      // For default derived constructor: super() first, then fields
      inject_field_inits_after(instance_fields, ctor_body)
  }

  // Constructors cannot be generators or async (spec forbids it)
  let child =
    compile_function_body(
      e,
      name,
      ctor_params,
      ctor_body_with_fields,
      False,
      False,
      False,
    )
  // Mark as derived constructor
  let child = CompiledChild(..child, is_derived_constructor: True)
  let #(e, ctor_idx) = add_child_function(e, child)

  // Step 1: Emit parent expression → [parent]
  use e <- result.try(emit_expr(e, parent_expr))

  // Step 2: MakeClosure for the derived constructor → [parent, ctor]
  let e = emit_ir(e, IrMakeClosure(ctor_idx))

  // Step 3: SetupDerivedClass → [ctor] (wires prototype chain)
  let e = emit_ir(e, IrSetupDerivedClass)

  // Step 4: Define instance methods on ctor.prototype (same as base class)
  use e <- result.try(
    list.try_fold(instance_methods, e, fn(e, method) {
      case method {
        ast.ClassMethod(
          key: ast.Identifier(method_name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodMethod,
          computed: False,
          ..,
        ) -> {
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrGetField("prototype"))
          let method_child =
            compile_function_body(
              e,
              Some(method_name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, method_idx) = add_child_function(e, method_child)
          let e = emit_ir(e, IrMakeClosure(method_idx))
          let e = emit_ir(e, IrDefineMethod(method_name))
          Ok(emit_ir(e, IrPop))
        }
        ast.ClassMethod(
          key: ast.StringExpression(method_name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodMethod,
          computed: False,
          ..,
        ) -> {
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrGetField("prototype"))
          let method_child =
            compile_function_body(
              e,
              Some(method_name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, method_idx) = add_child_function(e, method_child)
          let e = emit_ir(e, IrMakeClosure(method_idx))
          let e = emit_ir(e, IrDefineMethod(method_name))
          Ok(emit_ir(e, IrPop))
        }
        // Getter: get name() { ... }
        ast.ClassMethod(
          key: ast.Identifier(name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodGet,
          computed: False,
          ..,
        )
        | ast.ClassMethod(
            key: ast.StringExpression(name),
            value: ast.FunctionExpression(_, params, body, is_gen, is_async),
            kind: ast.MethodGet,
            computed: False,
            ..,
          ) -> {
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrGetField("prototype"))
          let getter_child =
            compile_function_body(
              e,
              Some("get " <> name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, getter_idx) = add_child_function(e, getter_child)
          let e = emit_ir(e, IrMakeClosure(getter_idx))
          let e = emit_ir(e, IrDefineAccessor(name, opcode.Getter))
          Ok(emit_ir(e, IrPop))
        }
        // Setter: set name(v) { ... }
        ast.ClassMethod(
          key: ast.Identifier(name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodSet,
          computed: False,
          ..,
        )
        | ast.ClassMethod(
            key: ast.StringExpression(name),
            value: ast.FunctionExpression(_, params, body, is_gen, is_async),
            kind: ast.MethodSet,
            computed: False,
            ..,
          ) -> {
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrGetField("prototype"))
          let setter_child =
            compile_function_body(
              e,
              Some("set " <> name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, setter_idx) = add_child_function(e, setter_child)
          let e = emit_ir(e, IrMakeClosure(setter_idx))
          let e = emit_ir(e, IrDefineAccessor(name, opcode.Setter))
          Ok(emit_ir(e, IrPop))
        }
        _ -> Error(Unsupported("computed class method"))
      }
    }),
  )

  // Step 5: Define static methods on ctor
  use e <- result.try(
    list.try_fold(static_methods, e, fn(e, method) {
      case method {
        ast.ClassMethod(
          key: ast.Identifier(method_name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodMethod,
          computed: False,
          ..,
        ) -> {
          let e = emit_ir(e, IrDup)
          let method_child =
            compile_function_body(
              e,
              Some(method_name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, method_idx) = add_child_function(e, method_child)
          let e = emit_ir(e, IrMakeClosure(method_idx))
          let e = emit_ir(e, IrDefineMethod(method_name))
          Ok(emit_ir(e, IrPop))
        }
        ast.ClassMethod(
          key: ast.StringExpression(method_name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodMethod,
          computed: False,
          ..,
        ) -> {
          let e = emit_ir(e, IrDup)
          let method_child =
            compile_function_body(
              e,
              Some(method_name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, method_idx) = add_child_function(e, method_child)
          let e = emit_ir(e, IrMakeClosure(method_idx))
          let e = emit_ir(e, IrDefineMethod(method_name))
          Ok(emit_ir(e, IrPop))
        }
        // Static getter: static get name() { ... }
        ast.ClassMethod(
          key: ast.Identifier(name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodGet,
          computed: False,
          ..,
        )
        | ast.ClassMethod(
            key: ast.StringExpression(name),
            value: ast.FunctionExpression(_, params, body, is_gen, is_async),
            kind: ast.MethodGet,
            computed: False,
            ..,
          ) -> {
          let e = emit_ir(e, IrDup)
          let getter_child =
            compile_function_body(
              e,
              Some("get " <> name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, getter_idx) = add_child_function(e, getter_child)
          let e = emit_ir(e, IrMakeClosure(getter_idx))
          let e = emit_ir(e, IrDefineAccessor(name, opcode.Getter))
          Ok(emit_ir(e, IrPop))
        }
        // Static setter: static set name(v) { ... }
        ast.ClassMethod(
          key: ast.Identifier(name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodSet,
          computed: False,
          ..,
        )
        | ast.ClassMethod(
            key: ast.StringExpression(name),
            value: ast.FunctionExpression(_, params, body, is_gen, is_async),
            kind: ast.MethodSet,
            computed: False,
            ..,
          ) -> {
          let e = emit_ir(e, IrDup)
          let setter_child =
            compile_function_body(
              e,
              Some("set " <> name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, setter_idx) = add_child_function(e, setter_child)
          let e = emit_ir(e, IrMakeClosure(setter_idx))
          let e = emit_ir(e, IrDefineAccessor(name, opcode.Setter))
          Ok(emit_ir(e, IrPop))
        }
        _ -> Error(Unsupported("computed static method"))
      }
    }),
  )

  // Stack: [ctor]
  Ok(e)
}

/// Inject field initializers after existing statements (for default derived constructor).
fn inject_field_inits_after(
  fields: List(ast.ClassElement),
  body: ast.Statement,
) -> ast.Statement {
  case fields {
    [] -> body
    _ -> {
      let field_stmts =
        list.filter_map(fields, fn(field) {
          case field {
            ast.ClassField(key: ast.Identifier(name), value: Some(init), ..) ->
              Ok(
                ast.ExpressionStatement(ast.AssignmentExpression(
                  operator: ast.Assign,
                  left: ast.MemberExpression(
                    ast.ThisExpression,
                    ast.Identifier(name),
                    False,
                  ),
                  right: init,
                )),
              )
            _ -> Error(Nil)
          }
        })
      let existing_stmts = case body {
        ast.BlockStatement(stmts) -> stmts
        other -> [other]
      }
      ast.BlockStatement(list.append(existing_stmts, field_stmts))
    }
  }
}

fn compile_base_class(
  e: Emitter,
  name: Option(String),
  body: List(ast.ClassElement),
) -> Result(Emitter, EmitError) {
  // Separate class elements into categories
  let #(ctor_method, instance_methods, static_methods, instance_fields) =
    classify_class_body(body)

  // Build the constructor body statement, injecting field initializers at the top
  let #(ctor_params, ctor_body) = case ctor_method {
    Some(ast.ClassMethod(value: ast.FunctionExpression(_, params, body, ..), ..)) -> #(
      params,
      body,
    )
    _ -> #([], ast.BlockStatement([]))
  }

  // Compile constructor: wrap the body with field initializer preamble
  // Constructors cannot be generators or async (spec forbids it)
  let ctor_body_with_fields = inject_field_inits(instance_fields, ctor_body)
  let child =
    compile_function_body(
      e,
      name,
      ctor_params,
      ctor_body_with_fields,
      False,
      False,
      False,
    )
  let #(e, ctor_idx) = add_child_function(e, child)

  // Step 1: MakeClosure for the constructor (creates .prototype + .prototype.constructor)
  let e = emit_ir(e, IrMakeClosure(ctor_idx))
  // Stack: [ctor]

  // Step 2: Define instance methods on ctor.prototype
  use e <- result.try(
    list.try_fold(instance_methods, e, fn(e, method) {
      case method {
        ast.ClassMethod(
          key: ast.Identifier(method_name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodMethod,
          computed: False,
          ..,
        ) -> {
          let e = emit_ir(e, IrDup)
          // Stack: [ctor, ctor]
          let e = emit_ir(e, IrGetField("prototype"))
          // Stack: [ctor, proto]
          let method_child =
            compile_function_body(
              e,
              Some(method_name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, method_idx) = add_child_function(e, method_child)
          let e = emit_ir(e, IrMakeClosure(method_idx))
          // Stack: [ctor, proto, method_fn]
          let e = emit_ir(e, IrDefineMethod(method_name))
          // Stack: [ctor, proto]
          Ok(emit_ir(e, IrPop))
          // Stack: [ctor]
        }
        ast.ClassMethod(
          key: ast.StringExpression(method_name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodMethod,
          computed: False,
          ..,
        ) -> {
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrGetField("prototype"))
          let method_child =
            compile_function_body(
              e,
              Some(method_name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, method_idx) = add_child_function(e, method_child)
          let e = emit_ir(e, IrMakeClosure(method_idx))
          let e = emit_ir(e, IrDefineMethod(method_name))
          Ok(emit_ir(e, IrPop))
        }
        // Getter: get name() { ... }
        ast.ClassMethod(
          key: ast.Identifier(name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodGet,
          computed: False,
          ..,
        )
        | ast.ClassMethod(
            key: ast.StringExpression(name),
            value: ast.FunctionExpression(_, params, body, is_gen, is_async),
            kind: ast.MethodGet,
            computed: False,
            ..,
          ) -> {
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrGetField("prototype"))
          let getter_child =
            compile_function_body(
              e,
              Some("get " <> name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, getter_idx) = add_child_function(e, getter_child)
          let e = emit_ir(e, IrMakeClosure(getter_idx))
          let e = emit_ir(e, IrDefineAccessor(name, opcode.Getter))
          Ok(emit_ir(e, IrPop))
        }
        // Setter: set name(v) { ... }
        ast.ClassMethod(
          key: ast.Identifier(name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodSet,
          computed: False,
          ..,
        )
        | ast.ClassMethod(
            key: ast.StringExpression(name),
            value: ast.FunctionExpression(_, params, body, is_gen, is_async),
            kind: ast.MethodSet,
            computed: False,
            ..,
          ) -> {
          let e = emit_ir(e, IrDup)
          let e = emit_ir(e, IrGetField("prototype"))
          let setter_child =
            compile_function_body(
              e,
              Some("set " <> name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, setter_idx) = add_child_function(e, setter_child)
          let e = emit_ir(e, IrMakeClosure(setter_idx))
          let e = emit_ir(e, IrDefineAccessor(name, opcode.Setter))
          Ok(emit_ir(e, IrPop))
        }
        _ -> Error(Unsupported("computed class method"))
      }
    }),
  )

  // Step 3: Define static methods on ctor
  use e <- result.try(
    list.try_fold(static_methods, e, fn(e, method) {
      case method {
        ast.ClassMethod(
          key: ast.Identifier(method_name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodMethod,
          computed: False,
          ..,
        ) -> {
          let e = emit_ir(e, IrDup)
          // Stack: [ctor, ctor]
          let method_child =
            compile_function_body(
              e,
              Some(method_name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, method_idx) = add_child_function(e, method_child)
          let e = emit_ir(e, IrMakeClosure(method_idx))
          // Stack: [ctor, ctor, method_fn]
          let e = emit_ir(e, IrDefineMethod(method_name))
          // Stack: [ctor, ctor]
          Ok(emit_ir(e, IrPop))
          // Stack: [ctor]
        }
        ast.ClassMethod(
          key: ast.StringExpression(method_name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodMethod,
          computed: False,
          ..,
        ) -> {
          let e = emit_ir(e, IrDup)
          let method_child =
            compile_function_body(
              e,
              Some(method_name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, method_idx) = add_child_function(e, method_child)
          let e = emit_ir(e, IrMakeClosure(method_idx))
          let e = emit_ir(e, IrDefineMethod(method_name))
          Ok(emit_ir(e, IrPop))
        }
        // Static getter: static get name() { ... }
        ast.ClassMethod(
          key: ast.Identifier(name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodGet,
          computed: False,
          ..,
        )
        | ast.ClassMethod(
            key: ast.StringExpression(name),
            value: ast.FunctionExpression(_, params, body, is_gen, is_async),
            kind: ast.MethodGet,
            computed: False,
            ..,
          ) -> {
          let e = emit_ir(e, IrDup)
          let getter_child =
            compile_function_body(
              e,
              Some("get " <> name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, getter_idx) = add_child_function(e, getter_child)
          let e = emit_ir(e, IrMakeClosure(getter_idx))
          let e = emit_ir(e, IrDefineAccessor(name, opcode.Getter))
          Ok(emit_ir(e, IrPop))
        }
        // Static setter: static set name(v) { ... }
        ast.ClassMethod(
          key: ast.Identifier(name),
          value: ast.FunctionExpression(_, params, body, is_gen, is_async),
          kind: ast.MethodSet,
          computed: False,
          ..,
        )
        | ast.ClassMethod(
            key: ast.StringExpression(name),
            value: ast.FunctionExpression(_, params, body, is_gen, is_async),
            kind: ast.MethodSet,
            computed: False,
            ..,
          ) -> {
          let e = emit_ir(e, IrDup)
          let setter_child =
            compile_function_body(
              e,
              Some("set " <> name),
              params,
              body,
              False,
              is_gen,
              is_async,
            )
          let #(e, setter_idx) = add_child_function(e, setter_child)
          let e = emit_ir(e, IrMakeClosure(setter_idx))
          let e = emit_ir(e, IrDefineAccessor(name, opcode.Setter))
          Ok(emit_ir(e, IrPop))
        }
        _ -> Error(Unsupported("computed static method"))
      }
    }),
  )

  // Stack: [ctor]
  Ok(e)
}

/// Classify class body elements into constructor, instance methods,
/// static methods, and instance fields.
fn classify_class_body(
  body: List(ast.ClassElement),
) -> #(
  Option(ast.ClassElement),
  List(ast.ClassElement),
  List(ast.ClassElement),
  List(ast.ClassElement),
) {
  list.fold(body, #(None, [], [], []), fn(acc, elem) {
    let #(ctor, instance_methods, static_methods, instance_fields) = acc
    case elem {
      // Constructor
      ast.ClassMethod(kind: ast.MethodConstructor, ..) -> #(
        Some(elem),
        instance_methods,
        static_methods,
        instance_fields,
      )
      // Instance method (non-static, non-constructor)
      ast.ClassMethod(is_static: False, kind: ast.MethodMethod, ..) -> #(
        ctor,
        list.append(instance_methods, [elem]),
        static_methods,
        instance_fields,
      )
      // Static method
      ast.ClassMethod(is_static: True, ..) -> #(
        ctor,
        instance_methods,
        list.append(static_methods, [elem]),
        instance_fields,
      )
      // Instance field (non-static)
      ast.ClassField(is_static: False, ..) -> #(
        ctor,
        instance_methods,
        static_methods,
        list.append(instance_fields, [elem]),
      )
      // Getter/setter on instance
      ast.ClassMethod(is_static: False, ..) -> #(
        ctor,
        list.append(instance_methods, [elem]),
        static_methods,
        instance_fields,
      )
      // Skip static fields and static blocks for now
      _ -> acc
    }
  })
}

/// Inject field initializer code at the start of a constructor body.
/// Each field `x = expr` becomes: `this.x = expr;` prepended to the body.
fn inject_field_inits(
  fields: List(ast.ClassElement),
  body: ast.Statement,
) -> ast.Statement {
  case fields {
    [] -> body
    _ -> {
      let init_stmts =
        list.filter_map(fields, fn(field) {
          case field {
            ast.ClassField(
              key: ast.Identifier(name),
              value: Some(init_expr),
              computed: False,
              ..,
            ) ->
              Ok(
                ast.ExpressionStatement(ast.AssignmentExpression(
                  ast.Assign,
                  ast.MemberExpression(
                    ast.ThisExpression,
                    ast.Identifier(name),
                    False,
                  ),
                  init_expr,
                )),
              )
            ast.ClassField(
              key: ast.Identifier(_name),
              value: None,
              computed: False,
              ..,
            ) ->
              // Field with no initializer: this.x = undefined
              Error(Nil)
            _ -> Error(Nil)
          }
        })
      let body_stmts = case body {
        ast.BlockStatement(stmts) -> stmts
        other -> [other]
      }
      ast.BlockStatement(list.append(init_stmts, body_stmts))
    }
  }
}

// ============================================================================
// Debug helpers
// ============================================================================

fn string_inspect_stmt_kind(stmt: ast.Statement) -> String {
  case stmt {
    ast.EmptyStatement -> "EmptyStatement"
    ast.ExpressionStatement(_) -> "ExpressionStatement"
    ast.BlockStatement(_) -> "BlockStatement"
    ast.VariableDeclaration(..) -> "VariableDeclaration"
    ast.ReturnStatement(_) -> "ReturnStatement"
    ast.IfStatement(..) -> "IfStatement"
    ast.ThrowStatement(_) -> "ThrowStatement"
    ast.WhileStatement(..) -> "WhileStatement"
    ast.DoWhileStatement(..) -> "DoWhileStatement"
    ast.ForStatement(..) -> "ForStatement"
    ast.ForInStatement(..) -> "ForInStatement"
    ast.ForOfStatement(..) -> "ForOfStatement"
    ast.SwitchStatement(..) -> "SwitchStatement"
    ast.TryStatement(..) -> "TryStatement"
    ast.BreakStatement(_) -> "BreakStatement"
    ast.ContinueStatement(_) -> "ContinueStatement"
    ast.DebuggerStatement -> "DebuggerStatement"
    ast.LabeledStatement(..) -> "LabeledStatement"
    ast.WithStatement(..) -> "WithStatement"
    ast.FunctionDeclaration(..) -> "FunctionDeclaration"
    ast.ClassDeclaration(..) -> "ClassDeclaration"
  }
}

fn string_inspect_expr_kind(expr: ast.Expression) -> String {
  case expr {
    ast.Identifier(_) -> "Identifier"
    ast.NumberLiteral(_) -> "NumberLiteral"
    ast.StringExpression(_) -> "StringExpression"
    ast.BooleanLiteral(_) -> "BooleanLiteral"
    ast.NullLiteral -> "NullLiteral"
    ast.UndefinedExpression -> "UndefinedExpression"
    ast.BinaryExpression(..) -> "BinaryExpression"
    ast.LogicalExpression(..) -> "LogicalExpression"
    ast.UnaryExpression(..) -> "UnaryExpression"
    ast.UpdateExpression(..) -> "UpdateExpression"
    ast.AssignmentExpression(..) -> "AssignmentExpression"
    ast.CallExpression(..) -> "CallExpression"
    ast.MemberExpression(..) -> "MemberExpression"
    ast.OptionalMemberExpression(..) -> "OptionalMemberExpression"
    ast.OptionalCallExpression(..) -> "OptionalCallExpression"
    ast.ConditionalExpression(..) -> "ConditionalExpression"
    ast.NewExpression(..) -> "NewExpression"
    ast.ThisExpression -> "ThisExpression"
    ast.SuperExpression -> "SuperExpression"
    ast.ArrayExpression(_) -> "ArrayExpression"
    ast.ObjectExpression(_) -> "ObjectExpression"
    ast.FunctionExpression(..) -> "FunctionExpression"
    ast.ArrowFunctionExpression(..) -> "ArrowFunctionExpression"
    ast.ClassExpression(..) -> "ClassExpression"
    ast.YieldExpression(..) -> "YieldExpression"
    ast.AwaitExpression(_) -> "AwaitExpression"
    ast.SequenceExpression(_) -> "SequenceExpression"
    ast.SpreadElement(_) -> "SpreadElement"
    ast.TemplateLiteral(..) -> "TemplateLiteral"
    ast.TaggedTemplateExpression(..) -> "TaggedTemplateExpression"
    ast.MetaProperty(..) -> "MetaProperty"
    ast.ImportExpression(_) -> "ImportExpression"
    ast.RegExpLiteral(..) -> "RegExpLiteral"
  }
}

// Need to import result for map/try
import gleam/result
