/// Phase 1: AST Emission
///
/// Walks the AST and produces a list of EmitterOps — symbolic IR instructions
/// mixed with scope markers. Variable references use string names (IrScopeGetVar),
/// jump targets use integer label IDs (IrJump). These are resolved in Phase 2 and 3.
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import lumen/ast
import lumen/vm/opcode.{
  type IrOp, IrArrayFrom, IrBinOp, IrCallConstructor, IrCallMethod,
  IrDefineField, IrDup, IrGetElem, IrGetElem2, IrGetField, IrGetField2,
  IrGetThis, IrJump, IrJumpIfFalse, IrJumpIfNullish, IrJumpIfTrue, IrLabel,
  IrMakeClosure, IrNewObject, IrPop, IrPopTry, IrPushConst, IrPushTry, IrPutElem,
  IrPutField, IrReturn, IrScopeGetVar, IrScopePutVar, IrScopeTypeofVar, IrSwap,
  IrThrow, IrTypeOf, IrUnaryOp,
}
import lumen/vm/value.{
  type JsValue, Finite, JsBool, JsNull, JsNumber, JsString, JsUndefined,
}

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
  )
}

/// Loop context for break/continue targets.
pub type LoopContext {
  LoopContext(break_label: Int, continue_label: Int)
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
/// Returns the emitter ops, constants, and child functions.
pub fn emit_program(
  stmts: List(ast.Statement),
) -> Result(
  #(List(EmitterOp), List(JsValue), Dict(JsValue, Int), List(CompiledChild)),
  EmitError,
) {
  let e = new_emitter()

  // Wrap in function scope
  let e = emit_op(e, EnterScope(FunctionScope))

  // Hoisting pre-pass: collect var declarations and function declarations
  let hoisted_vars = collect_hoisted_vars(stmts)
  let #(e, hoisted_funcs) = collect_hoisted_funcs(e, stmts)

  // Emit var declarations at top
  let e =
    list.fold(hoisted_vars, e, fn(e, name) {
      emit_op(e, DeclareVar(name, VarBinding))
    })

  // Emit hoisted function declarations
  let e =
    list.fold(hoisted_funcs, e, fn(e, hf) {
      let #(name, func_idx) = hf
      let e = emit_ir(e, IrMakeClosure(func_idx))
      let e = emit_ir(e, IrScopePutVar(name))
      e
    })

  // Emit body — last expression statement's value is the program result
  use e <- result.try(emit_program_body(stmts, e))

  let e = emit_op(e, LeaveScope)
  Ok(finish(e))
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
  )
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
    Error(_) -> {
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
  Emitter(..e, loop_stack: [
    LoopContext(break_label:, continue_label:),
    ..e.loop_stack
  ])
}

fn pop_loop(e: Emitter) -> Emitter {
  case e.loop_stack {
    [_, ..rest] -> Emitter(..e, loop_stack: rest)
    [] -> e
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

/// Emit program body: all statements, but the last statement is emitted in
/// "tail" position — its completion value stays on the stack as the program result.
fn emit_program_body(
  stmts: List(ast.Statement),
  e: Emitter,
) -> Result(Emitter, EmitError) {
  case stmts {
    [] -> Ok(push_const(e, JsUndefined))
    [only] -> emit_stmt_tail(e, only)
    [first, ..rest] -> {
      use e <- result.try(emit_stmt(e, first))
      emit_program_body(rest, e)
    }
  }
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
            Some(ast.IdentifierPattern(name)) -> {
              let e = emit_op(e, DeclareVar(name, CatchBinding))
              emit_ir(e, IrScopePutVar(name))
            }
            Some(_) -> e
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

fn collect_vars_stmt(stmt: ast.Statement) -> List(String) {
  case stmt {
    ast.VariableDeclaration(ast.Var, declarators) ->
      list.filter_map(declarators, fn(d) {
        case d {
          ast.VariableDeclarator(ast.IdentifierPattern(name), _) -> Ok(name)
          _ -> Error(Nil)
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
          list.filter_map(decls, fn(d) {
            case d {
              ast.VariableDeclarator(ast.IdentifierPattern(name), _) -> Ok(name)
              _ -> Error(Nil)
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
    ast.LabeledStatement(_, body) -> collect_vars_stmt(body)
    ast.SwitchStatement(_, cases) ->
      list.flat_map(cases, fn(c) {
        case c {
          ast.SwitchCase(_, consequent) ->
            list.flat_map(consequent, collect_vars_stmt)
        }
      })
    // Don't enter nested functions
    ast.FunctionDeclaration(..) -> []
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
      ast.FunctionDeclaration(Some(name), params, body, _is_gen, _is_async) -> {
        let child = compile_function_body(e, Some(name), params, body, False)
        let #(e, idx) = add_child_function(e, child)
        #(e, list.append(funcs, [#(name, idx)]))
      }
      _ -> #(e, funcs)
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
) -> CompiledChild {
  // Use a fresh emitter inheriting nothing from parent (except label counter for uniqueness)
  let e = Emitter(..new_emitter(), next_label: parent.next_label)

  let e = emit_op(e, EnterScope(FunctionScope))

  // Declare parameters
  let e =
    list.fold(params, e, fn(e, param) {
      case param {
        ast.IdentifierPattern(pname) ->
          emit_op(e, DeclareVar(pname, ParamBinding))
        _ -> e
        // Non-identifier params not in MVP
      }
    })

  let stmts = case body {
    ast.BlockStatement(s) -> s
    other -> [other]
  }

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
    is_strict: False,
    is_arrow:,
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
    ast.EmptyStatement -> Ok(e)

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
          _ -> Error(Unsupported("destructuring patterns"))
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

    ast.TryStatement(block, handler, _finalizer) -> {
      // MVP: try-catch only (no finally)
      case handler {
        Some(ast.CatchClause(param, catch_body)) -> {
          let #(e, catch_label) = fresh_label(e)
          let #(e, end_label) = fresh_label(e)

          let e = emit_ir(e, IrPushTry(catch_label, -1))
          use e <- result.try(emit_stmt(e, block))
          let e = emit_ir(e, IrPopTry)
          let e = emit_ir(e, IrJump(end_label))

          let e = emit_ir(e, IrLabel(catch_label))
          let e = emit_op(e, EnterScope(BlockScope))

          // Bind catch parameter (thrown value is on stack from unwind)
          let e = case param {
            Some(ast.IdentifierPattern(name)) -> {
              let e = emit_op(e, DeclareVar(name, CatchBinding))
              emit_ir(e, IrScopePutVar(name))
            }
            Some(_) -> e
            // Non-identifier catch param not in MVP
            None -> emit_ir(e, IrPop)
            // No param, discard thrown value
          }

          use e <- result.try(emit_stmt(e, catch_body))
          let e = emit_op(e, LeaveScope)
          let e = emit_ir(e, IrLabel(end_label))
          Ok(e)
        }
        None ->
          // try-finally without catch: just emit the block for MVP
          emit_stmt(e, block)
      }
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

    ast.ContinueStatement(None) -> {
      case e.loop_stack {
        [ctx, ..] -> {
          let e = emit_ir(e, IrJump(ctx.continue_label))
          Ok(e)
        }
        [] -> Error(ContinueOutsideLoop)
      }
    }

    ast.FunctionDeclaration(..) -> {
      // Already hoisted — nothing to emit here
      Ok(e)
    }

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

    // Method call: obj.method(args) — emits GetField2 + CallMethod for this binding
    ast.CallExpression(
      ast.MemberExpression(obj, ast.Identifier(method_name), False),
      args,
    ) -> {
      use e <- result.try(emit_expr(e, obj))
      let e = emit_ir(e, IrGetField2(method_name))
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, IrCallMethod(method_name, list.length(args)))
    }
    // Regular call expression
    ast.CallExpression(callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, opcode.IrCall(list.length(args)))
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
      list.try_fold(properties, e, fn(e, prop) {
        case prop {
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
          _ -> Error(Unsupported("computed/getter/setter property"))
        }
      })
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

    // Array literal
    ast.ArrayExpression(elements) -> {
      let count = list.length(elements)
      use e <- result.map(
        list.try_fold(elements, e, fn(e, elem) {
          case elem {
            Some(expr) -> emit_expr(e, expr)
            // Hole in array — push undefined
            None -> Ok(push_const(e, JsUndefined))
          }
        }),
      )
      emit_ir(e, IrArrayFrom(count))
    }

    // Function expression
    ast.FunctionExpression(name, params, body, _is_gen, _is_async) -> {
      let child = compile_function_body(e, name, params, body, False)
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }

    // Arrow function expression
    ast.ArrowFunctionExpression(params, body, _is_async) -> {
      let body_stmt = case body {
        ast.ArrowBodyExpression(expr) ->
          ast.BlockStatement([ast.ReturnStatement(Some(expr))])
        ast.ArrowBodyBlock(stmt) -> stmt
      }
      let child = compile_function_body(e, None, params, body_stmt, True)
      let #(e, idx) = add_child_function(e, child)
      Ok(emit_ir(e, IrMakeClosure(idx)))
    }

    // This expression
    ast.ThisExpression -> Ok(emit_ir(e, IrGetThis))

    // New expression: new Foo(args)
    ast.NewExpression(callee, args) -> {
      use e <- result.try(emit_expr(e, callee))
      use e <- result.map(list.try_fold(args, e, emit_expr))
      emit_ir(e, IrCallConstructor(list.length(args)))
    }

    _ -> Error(Unsupported("expression: " <> string_inspect_expr_kind(expr)))
  }
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
