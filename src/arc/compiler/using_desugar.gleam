//// Desugar `using` / `await using` declarations (Explicit Resource
//// Management, ES2026) into plain AST the emitter already understands:
//// const bindings + try/catch/finally + two intrinsic expressions
//// (ast.IntrinsicGetDisposer / ast.IntrinsicMakeSuppressed).
////
//// A statement list containing using declarations
////
////   { using x = a; stmt; await using y = b; }
////
//// becomes (names with a `%u:` prefix cannot collide with user bindings —
//// `%` is not legal in an IdentifierName):
////
////   {
////     let %u:err = undefined;       // DisposeResources `completion`
////     let %u:hasErr = false;
////     let %u:d1 = null;             // null = declaration not reached
////     let %u:d2 = null;
////     let %u:needsAwait = false;    // async lists only
////     let %u:hasAwaited = false;
////     let %u:t = undefined;
////     let %u:ok = false;
////     try {
////       const x = a;
////       %u:d1 = IntrinsicGetDisposer(x, sync);
////       stmt;
////       const y = b;
////       %u:d2 = IntrinsicGetDisposer(y, async);
////     } catch (%u:e) {
////       %u:hasErr = true; %u:err = %u:e;
////     } finally {
////       ...dispose %u:d2 then %u:d1 (reverse order), folding errors into
////       %u:err via IntrinsicMakeSuppressed, awaiting async disposals...
////       if (%u:hasErr) throw %u:err;
////     }
////   }
////
//// This mirrors DisposeResources(disposeCapability, completion) from the
//// proposal (https://tc39.es/proposal-explicit-resource-management/),
//// including the needsAwait/hasAwaited coalescing of Await(undefined) for
//// method-less async resources.
////
//// The pass is SHALLOW: emit_block / function-body / switch-case emission
//// calls `desugar_list` on each statement list right before scanning it, so
//// nested blocks are handled when they are themselves emitted. for/for-of
//// statements with using heads are rewritten by `rewrite_for_using` from
//// emit_stmt (they can also sit in single-statement positions).

import arc/parser/ast
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

const n_err = "%u:err"

const n_has_err = "%u:hasErr"

const n_needs_await = "%u:needsAwait"

const n_has_awaited = "%u:hasAwaited"

const n_tmp = "%u:t"

const n_ok = "%u:ok"

const n_exc = "%u:e"

const n_exc2 = "%u:e2"

fn slot_name(k: Int) -> String {
  "%u:d" <> int.to_string(k)
}

fn ident(name: String) -> ast.Expression {
  ast.Identifier(name)
}

fn assign(name: String, value: ast.Expression) -> ast.Statement {
  ast.ExpressionStatement(
    ast.AssignmentExpression(
      operator: ast.Assign,
      left: ident(name),
      right: value,
    ),
    None,
  )
}

fn let_decl(name: String, init: ast.Expression) -> ast.Statement {
  ast.VariableDeclaration(kind: ast.Let, declarations: [
    ast.VariableDeclarator(id: ast.IdentifierPattern(name), init: Some(init)),
  ])
}

fn stmt(s: ast.Statement) -> ast.StmtWithLine {
  ast.StmtWithLine(0, s)
}

fn not_null(name: String) -> ast.Expression {
  ast.BinaryExpression(ast.StrictNotEqual, ident(name), ast.NullLiteral)
}

fn not_undefined(name: String) -> ast.Expression {
  ast.BinaryExpression(ast.StrictNotEqual, ident(name), ast.UndefinedExpression)
}

/// `%u:needsAwait && !%u:hasAwaited` — the DisposeResources guard for the
/// coalesced Await(undefined).
fn pending_await_check() -> ast.Expression {
  ast.LogicalExpression(
    ast.LogicalAnd,
    ident(n_needs_await),
    ast.UnaryExpression(ast.LogicalNot, True, ident(n_has_awaited)),
  )
}

/// DisposeResources error folding (step 3.e.iii): fold a caught disposal
/// error `exc` into the pending completion:
///   %u:err = %u:hasErr ? IntrinsicMakeSuppressed(exc, %u:err) : exc;
///   %u:hasErr = true;
fn merge_error_stmts(exc: String) -> List(ast.StmtWithLine) {
  [
    stmt(assign(
      n_err,
      ast.ConditionalExpression(
        condition: ident(n_has_err),
        consequent: ast.IntrinsicMakeSuppressed(
          error: ident(exc),
          suppressed: ident(n_err),
        ),
        alternate: ident(exc),
      ),
    )),
    stmt(assign(n_has_err, ast.BooleanLiteral(True))),
  ]
}

/// `try { <body> } catch (%u:e2) { <merge into %u:err> }`
fn try_merge(body: List(ast.StmtWithLine)) -> ast.Statement {
  ast.TryStatement(
    block: ast.BlockStatement(body),
    handler: Some(ast.CatchClause(
      param: Some(ast.IdentifierPattern(n_exc2)),
      body: ast.BlockStatement(merge_error_stmts(n_exc2)),
    )),
    finalizer: None,
  )
}

/// One resource slot created by the desugar, in declaration order.
type Slot {
  Slot(index: Int, is_async: Bool)
}

/// True if the list directly contains a using/await-using declaration.
pub fn has_using_decl(stmts: List(ast.StmtWithLine)) -> Bool {
  list.any(stmts, fn(s) {
    case s.statement {
      ast.VariableDeclaration(kind: ast.Using, ..)
      | ast.VariableDeclaration(kind: ast.AwaitUsing, ..) -> True
      _ -> False
    }
  })
}

/// Desugar a block-like statement list (block body, function body, switch
/// case list). No-op when the list has no direct using declarations.
pub fn desugar_list(stmts: List(ast.StmtWithLine)) -> List(ast.StmtWithLine) {
  case has_using_decl(stmts) {
    False -> stmts
    True -> {
      let #(directives, rest) = split_directives(stmts)
      let #(body, slots) = replace_using_decls(rest)
      let has_async = list.any(slots, fn(s) { s.is_async })
      list.flatten([
        directives,
        prelude(slots, has_async),
        [
          stmt(ast.TryStatement(
            block: ast.BlockStatement(body),
            handler: Some(ast.CatchClause(
              param: Some(ast.IdentifierPattern(n_exc)),
              body: ast.BlockStatement([
                stmt(assign(n_has_err, ast.BooleanLiteral(True))),
                stmt(assign(n_err, ident(n_exc))),
              ]),
            )),
            finalizer: Some(ast.BlockStatement(dispose_stmts(slots, has_async))),
          )),
        ],
      ])
    }
  }
}

/// Pieces of a desugared module top level with using declarations. Exports
/// must stay module-scoped bindings, so the body cannot be wrapped in an AST
/// try block (its BlockStatement would re-scope the const bindings away from
/// the module environment). Instead the module emitter installs a scope-free
/// try frame: `prelude; PushTry(h); body; PopTry; goto dispose;
/// h: catch_body; dispose;` — so DisposeResources runs on normal AND abrupt
/// module-body completion (proposal: Module Evaluation performs
/// DisposeResources(completion) for normal and throw completions alike).
pub type ModuleTop {
  ModuleTop(
    /// Completion-state and disposer-slot `let` declarations.
    prelude: List(ast.StmtWithLine),
    /// The module body with using declarations rewritten to const+register.
    body: List(ast.StmtWithLine),
    /// Catch-parameter name for the emitter's handler block scope.
    catch_param: String,
    /// Handler statements: fold the thrown value into the completion state.
    catch_body: List(ast.StmtWithLine),
    /// DisposeResources over the slots, ending in the conditional rethrow.
    dispose: List(ast.StmtWithLine),
  )
}

/// Module top level: returns None when the body has no direct using
/// declarations; otherwise the pieces for the emitter's scope-free
/// try-frame lowering (see ModuleTop).
pub fn desugar_module_top(stmts: List(ast.StmtWithLine)) -> Option(ModuleTop) {
  case has_using_decl(stmts) {
    False -> None
    True -> {
      let #(body, slots) = replace_using_decls(stmts)
      let has_async = list.any(slots, fn(s) { s.is_async })
      Some(ModuleTop(
        prelude: prelude(slots, has_async),
        body:,
        catch_param: n_exc,
        catch_body: [
          stmt(assign(n_has_err, ast.BooleanLiteral(True))),
          stmt(assign(n_err, ident(n_exc))),
        ],
        dispose: dispose_stmts(slots, has_async),
      ))
    }
  }
}

/// Directive prologue must stay ahead of the synthesized prelude.
fn split_directives(
  stmts: List(ast.StmtWithLine),
) -> #(List(ast.StmtWithLine), List(ast.StmtWithLine)) {
  list.split_while(stmts, fn(s) {
    case s.statement {
      ast.ExpressionStatement(directive: Some(_), ..) -> True
      _ -> False
    }
  })
}

/// The `let` declarations ahead of the try statement: completion state, one
/// disposer slot per resource, and (for async lists) the await-coalescing
/// flags plus scratch slots.
fn prelude(slots: List(Slot), has_async: Bool) -> List(ast.StmtWithLine) {
  let head = [
    stmt(let_decl(n_err, ast.UndefinedExpression)),
    stmt(let_decl(n_has_err, ast.BooleanLiteral(False))),
  ]
  let slot_decls =
    list.map(slots, fn(s) {
      stmt(let_decl(slot_name(s.index), ast.NullLiteral))
    })
  let async_decls = case has_async {
    False -> []
    True -> [
      stmt(let_decl(n_needs_await, ast.BooleanLiteral(False))),
      stmt(let_decl(n_has_awaited, ast.BooleanLiteral(False))),
      stmt(let_decl(n_tmp, ast.UndefinedExpression)),
      stmt(let_decl(n_ok, ast.BooleanLiteral(False))),
    ]
  }
  list.flatten([head, slot_decls, async_decls])
}

/// Replace each `using` declaration statement with per-declarator
/// `const x = init; %u:dK = IntrinsicGetDisposer(x, hint);` pairs.
/// Returns the rewritten list plus the slots in declaration order.
fn replace_using_decls(
  stmts: List(ast.StmtWithLine),
) -> #(List(ast.StmtWithLine), List(Slot)) {
  let #(rev_stmts, rev_slots, _next) =
    list.fold(stmts, #([], [], 1), fn(acc, s) {
      let #(out, slots, next) = acc
      case s.statement {
        ast.VariableDeclaration(kind: ast.Using, declarations:) ->
          replace_one_decl(out, slots, next, s.line, declarations, False)
        ast.VariableDeclaration(kind: ast.AwaitUsing, declarations:) ->
          replace_one_decl(out, slots, next, s.line, declarations, True)
        _ -> #([s, ..out], slots, next)
      }
    })
  #(list.reverse(rev_stmts), list.reverse(rev_slots))
}

fn replace_one_decl(
  out: List(ast.StmtWithLine),
  slots: List(Slot),
  next: Int,
  line: Int,
  declarations: List(ast.VariableDeclarator),
  is_async: Bool,
) -> #(List(ast.StmtWithLine), List(Slot), Int) {
  list.fold(declarations, #(out, slots, next), fn(acc, decl) {
    let #(out, slots, k) = acc
    let ast.VariableDeclarator(id:, init:) = decl
    // The parser only produces identifier patterns with initializers for
    // using declarations; for-of rewrites supply the iteration value.
    let init_expr = option.unwrap(init, ast.UndefinedExpression)
    let const_stmt =
      ast.StmtWithLine(
        line,
        ast.VariableDeclaration(kind: ast.Const, declarations: [
          ast.VariableDeclarator(id:, init: Some(init_expr)),
        ]),
      )
    let bound_expr = case id {
      ast.IdentifierPattern(name) -> ident(name)
      // Unreachable (parser rejects destructuring in using declarations) —
      // fall back to registering an undisposable slot.
      _ -> ast.UndefinedExpression
    }
    let disposer_stmt =
      ast.StmtWithLine(
        line,
        assign(
          slot_name(k),
          ast.IntrinsicGetDisposer(argument: bound_expr, is_async:),
        ),
      )
    #(
      [disposer_stmt, const_stmt, ..out],
      [Slot(index: k, is_async:), ..slots],
      k + 1,
    )
  })
}

/// The finally-block body: DisposeResources over the slots in reverse
/// order, then rethrow of the folded completion.
fn dispose_stmts(slots: List(Slot), has_async: Bool) -> List(ast.StmtWithLine) {
  let per_resource =
    slots
    |> list.reverse
    |> list.flat_map(fn(s) {
      case s.is_async {
        True -> dispose_async_resource(s.index)
        False -> dispose_sync_resource(s.index, has_async)
      }
    })
  let tail_await = case has_async {
    False -> []
    True -> [
      // DisposeResources step 4: trailing coalesced Await(undefined).
      stmt(ast.IfStatement(
        condition: pending_await_check(),
        consequent: ast.BlockStatement([
          stmt(ast.ExpressionStatement(
            ast.AwaitExpression(ast.UndefinedExpression),
            None,
          )),
        ]),
        alternate: None,
      )),
    ]
  }
  let rethrow = [
    stmt(ast.IfStatement(
      condition: ident(n_has_err),
      consequent: ast.ThrowStatement(ident(n_err)),
      alternate: None,
    )),
  ]
  list.flatten([per_resource, tail_await, rethrow])
}

/// Sync resource K (hint sync-dispose):
///   if (%u:dK !== null && %u:dK !== undefined) {
///     [async lists only — DisposeResources step 3.b:]
///     if (%u:needsAwait && !%u:hasAwaited) { await undefined; %u:needsAwait = false; }
///     try { %u:dK(); } catch (%u:e2) { <merge> }
///   }
/// (A sync resource with a null/undefined initializer is never added to the
/// stack — %u:dK stays undefined — so the whole step is skipped, including
/// the coalesced await, matching AddDisposableResource step 1.a.)
fn dispose_sync_resource(k: Int, has_async: Bool) -> List(ast.StmtWithLine) {
  let dk = slot_name(k)
  let flush_await = case has_async {
    False -> []
    True -> [
      stmt(ast.IfStatement(
        condition: pending_await_check(),
        consequent: ast.BlockStatement([
          stmt(ast.ExpressionStatement(
            ast.AwaitExpression(ast.UndefinedExpression),
            None,
          )),
          stmt(assign(n_needs_await, ast.BooleanLiteral(False))),
        ]),
        alternate: None,
      )),
    ]
  }
  let call_stmts = [
    stmt(
      try_merge([
        stmt(ast.ExpressionStatement(
          ast.CallExpression(callee: ident(dk), arguments: []),
          None,
        )),
      ]),
    ),
  ]
  [
    stmt(ast.IfStatement(
      condition: ast.LogicalExpression(
        ast.LogicalAnd,
        not_null(dk),
        not_undefined(dk),
      ),
      consequent: ast.BlockStatement(list.append(flush_await, call_stmts)),
      alternate: None,
    )),
  ]
}

/// Async resource K (hint async-dispose):
///   if (%u:dK !== null) {                       // declaration was reached
///     if (%u:dK !== undefined) {
///       %u:ok = false;
///       try { %u:t = %u:dK(); %u:ok = true; } catch (%u:e2) { <merge> }
///       if (%u:ok) {
///         try { await %u:t; } catch (%u:e2) { <merge> }
///         %u:hasAwaited = true;
///       }
///     } else {
///       %u:needsAwait = true;                   // method-less: coalesce
///     }
///   }
fn dispose_async_resource(k: Int) -> List(ast.StmtWithLine) {
  let dk = slot_name(k)
  let call_and_await = [
    stmt(assign(n_ok, ast.BooleanLiteral(False))),
    stmt(
      try_merge([
        stmt(assign(n_tmp, ast.CallExpression(callee: ident(dk), arguments: []))),
        stmt(assign(n_ok, ast.BooleanLiteral(True))),
      ]),
    ),
    stmt(ast.IfStatement(
      condition: ident(n_ok),
      consequent: ast.BlockStatement([
        stmt(
          try_merge([
            stmt(ast.ExpressionStatement(
              ast.AwaitExpression(ident(n_tmp)),
              None,
            )),
          ]),
        ),
        stmt(assign(n_has_awaited, ast.BooleanLiteral(True))),
      ]),
      alternate: None,
    )),
  ]
  [
    stmt(ast.IfStatement(
      condition: not_null(dk),
      consequent: ast.BlockStatement([
        stmt(ast.IfStatement(
          condition: not_undefined(dk),
          consequent: ast.BlockStatement(call_and_await),
          alternate: Some(
            ast.BlockStatement([
              stmt(assign(n_needs_await, ast.BooleanLiteral(True))),
            ]),
          ),
        )),
      ]),
      alternate: None,
    )),
  ]
}

/// Rewrite a for / for-of statement whose head declares using bindings (the
/// statement may sit under a chain of labels). Returns None when no rewrite
/// is needed.
///
///   for (using x = a; c; u) body
///     → { using x = a; for (; c; u) body }      (labels stay on the for)
///   for (using x of xs) body
///     → for (const x of xs) { <prelude>; try { %u:d1 = GetDisposer(x); body }
///         catch … finally { <dispose> } }
///
/// The for-of form keeps the ORIGINAL binding in the head so the RHS sees
/// its TDZ (`for (using x of [x])` must throw ReferenceError) and the
/// binding stays per-iteration and const (assignment → TypeError).
/// The classic-for form's produced using declaration is handled by
/// desugar_list when the wrapper block is emitted.
pub fn rewrite_for_using(statement: ast.Statement) -> Option(ast.Statement) {
  rewrite_for_labeled(statement, [])
}

fn rewrite_for_labeled(
  statement: ast.Statement,
  labels: List(String),
) -> Option(ast.Statement) {
  case statement {
    ast.LabeledStatement(label:, body:) ->
      rewrite_for_labeled(body, [label, ..labels])
    ast.ForStatement(
      init: Some(ast.ForInitDeclaration(
        ast.VariableDeclaration(kind:, ..) as decl,
      )),
      condition:,
      update:,
      body:,
    ) ->
      case kind {
        ast.Using | ast.AwaitUsing -> {
          let inner =
            apply_labels(
              labels,
              ast.ForStatement(init: None, condition:, update:, body:),
            )
          Some(ast.BlockStatement([stmt(decl), stmt(inner)]))
        }
        _ -> None
      }
    ast.ForOfStatement(
      left: ast.ForInitDeclaration(ast.VariableDeclaration(kind:, declarations:)),
      right:,
      body:,
      is_await:,
    ) ->
      case kind, declarations {
        ast.Using, [ast.VariableDeclarator(id:, ..)] ->
          Some(apply_labels(
            labels,
            rewrite_for_of(kind, id, right, body, is_await),
          ))
        ast.AwaitUsing, [ast.VariableDeclarator(id:, ..)] ->
          Some(apply_labels(
            labels,
            rewrite_for_of(kind, id, right, body, is_await),
          ))
        _, _ -> None
      }
    _ -> None
  }
}

/// `for (using x of xs) body`
///   → `for (const x of xs) { <dispose-wrapped body> }`
fn rewrite_for_of(
  kind: ast.VariableKind,
  id: ast.Pattern,
  right: ast.Expression,
  body: ast.Statement,
  is_await: Bool,
) -> ast.Statement {
  let is_async = kind == ast.AwaitUsing
  ast.ForOfStatement(
    left: ast.ForInitDeclaration(
      ast.VariableDeclaration(kind: ast.Const, declarations: [
        ast.VariableDeclarator(id:, init: None),
      ]),
    ),
    right:,
    body: ast.BlockStatement(wrap_body_with_dispose(id, is_async, body)),
    is_await:,
  )
}

/// Wrap a loop body so an EXISTING (head-bound) binding is registered as a
/// disposable resource at iteration start and disposed at iteration end:
///   <prelude>;
///   try { %u:d1 = IntrinsicGetDisposer(x, hint); body }
///   catch (%u:e) { %u:hasErr = true; %u:err = %u:e; }
///   finally { <dispose>; if (%u:hasErr) throw %u:err; }
fn wrap_body_with_dispose(
  id: ast.Pattern,
  is_async: Bool,
  body: ast.Statement,
) -> List(ast.StmtWithLine) {
  let slots = [Slot(index: 1, is_async:)]
  let resource = case id {
    ast.IdentifierPattern(name) -> ident(name)
    // Unreachable — the parser only allows identifier bindings in using
    // for-of heads.
    _ -> ast.UndefinedExpression
  }
  let try_body = [
    stmt(assign(
      slot_name(1),
      ast.IntrinsicGetDisposer(argument: resource, is_async:),
    )),
    stmt(body),
  ]
  list.flatten([
    prelude(slots, is_async),
    [
      stmt(ast.TryStatement(
        block: ast.BlockStatement(try_body),
        handler: Some(ast.CatchClause(
          param: Some(ast.IdentifierPattern(n_exc)),
          body: ast.BlockStatement([
            stmt(assign(n_has_err, ast.BooleanLiteral(True))),
            stmt(assign(n_err, ident(n_exc))),
          ]),
        )),
        finalizer: Some(ast.BlockStatement(dispose_stmts(slots, is_async))),
      )),
    ],
  ])
}

fn apply_labels(
  labels: List(String),
  statement: ast.Statement,
) -> ast.Statement {
  list.fold(labels, statement, fn(s, label) {
    ast.LabeledStatement(label:, body: s)
  })
}
