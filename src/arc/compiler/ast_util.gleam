//// Pure-AST query helpers shared by the PARSER (arc/parser.gleam, which
//// builds the scope tree via `scope.ScopeBuilder` as it parses) and the
//// EMITTER (compiler/emit.gleam), plus the compiler driver.
////
//// The parser runs BEFORE emission and cannot import compiler/emit.gleam,
//// so every helper here depends ONLY on arc/parser/ast and the Gleam
//// stdlib — no Emitter, no IrOp, no BindingKind. emit.gleam wraps these
//// where it needs an emit-local type (e.g. collect_top_lex_names below
//// returns `is_const: Bool`; emit's thin wrapper maps that to
//// LetBinding/ConstBinding).
////
//// The parser and the emitter must make IDENTICAL structural decisions
//// (which parameter lists are "simple", which statements are hoisted
//// FunctionDeclarations, the class-body element order): the parser
//// allocates scopes/slots from them and the emitter consumes child scopes
//// with positional cursors. Keeping ONE copy of each predicate here is
//// what keeps the two in lockstep.
////
//// Everything in this module is a pure function over AST nodes: no I/O, no
//// state, no side effects.

import arc/parser/ast
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}

// ============================================================================
// Pattern / declarator name extraction
// ============================================================================

/// Split off a trailing `...rest` parameter. Returns the fixed params (in
/// order) and the rest target pattern (the binding inside `...`), if
/// present. A rest element is only valid as the last formal, so checking
/// the tail is sufficient.
pub fn split_trailing_rest(
  params: List(ast.Pattern),
) -> #(List(ast.Pattern), Option(ast.Pattern)) {
  case list.reverse(params) {
    [ast.RestElement(inner), ..rev_fixed] -> #(
      list.reverse(rev_fixed),
      Some(inner),
    )
    _ -> #(params, None)
  }
}

/// True when every FIXED formal (after `split_trailing_rest`) is a bare
/// `IdentifierPattern` (no default, no destructuring). §10.2.11 step 22 —
/// only a "simple parameter list" gets ParamBinding semantics; otherwise
/// `<paramN>` shims receive the raw positional args and the user names are
/// TDZ-declared LetBindings. Single source of truth: the PARSER and the
/// EMITTER MUST agree on this predicate or the parser declares
/// shims+LetBindings while the emitter takes the simple positional path,
/// leaving the user names seeded JsUninitialized → TDZ on first read.
pub fn all_simple_params(fixed: List(ast.Pattern)) -> Bool {
  use p <- list.all(fixed)
  case p {
    ast.IdentifierPattern(..) -> True
    _ -> False
  }
}

/// All pattern-bound names across a list of declarators.
fn declarator_names(declarators: List(ast.VariableDeclarator)) -> List(String) {
  list.flat_map(declarators, fn(d) {
    let ast.VariableDeclarator(pattern, _) = d
    ast.pattern_bound_names(pattern)
  })
}

// ============================================================================
// var hoisting (VarDeclaredNames)
// ============================================================================

/// Collect all var-declared names in a function body (not entering nested
/// functions). Spec VarDeclaredNames.
pub fn collect_hoisted_vars(stmts: List(ast.StmtWithLine)) -> List(String) {
  list.flat_map(stmts, collect_vars_located)
  |> list.unique()
}

/// Peel a StmtWithLine for the recursive var-collection walk.
fn collect_vars_located(s: ast.StmtWithLine) -> List(String) {
  collect_vars_stmt(s.statement)
}

/// VarDeclaredNames of a single statement (recurses into nested blocks /
/// loops / try, NOT into nested function bodies).
fn collect_vars_stmt(stmt: ast.Statement) -> List(String) {
  case stmt {
    ast.VariableDeclaration(ast.Var, declarators) ->
      declarator_names(declarators)
    ast.BlockStatement(body) -> list.flat_map(body, collect_vars_located)
    ast.IfStatement(_, consequent, alternate) ->
      list.append(
        collect_vars_stmt(consequent),
        alternate |> option.map(collect_vars_stmt) |> option.unwrap([]),
      )
    ast.WhileStatement(_, body) -> collect_vars_stmt(body)
    ast.DoWhileStatement(_, body) -> collect_vars_stmt(body)
    ast.ForStatement(init, _, _, body) -> {
      let init_vars = case init {
        Some(ast.ForInitDeclaration(ast.Var, decls)) -> declarator_names(decls)
        _ -> []
      }
      list.append(init_vars, collect_vars_stmt(body))
    }
    ast.TryStatement(block, handler, finalizer) -> {
      let handler_vars = case handler {
        Some(ast.CatchClause(_, body)) -> collect_vars_stmt(body)
        None -> []
      }
      let finally_vars =
        finalizer |> option.map(collect_vars_stmt) |> option.unwrap([])
      list.flatten([collect_vars_stmt(block), handler_vars, finally_vars])
    }
    ast.ForInStatement(left, _, body) | ast.ForOfStatement(left, _, body, ..) -> {
      let left_vars = case left {
        ast.ForInitDeclaration(ast.Var, decls) -> declarator_names(decls)
        _ -> []
      }
      list.append(left_vars, collect_vars_stmt(body))
    }
    ast.LabeledStatement(_, body) -> collect_vars_stmt(body)
    ast.WithStatement(_, body) -> collect_vars_stmt(body)
    ast.SwitchStatement(_, cases) ->
      list.flat_map(cases, fn(c) {
        case c {
          ast.SwitchCase(_, consequent) ->
            list.flat_map(consequent, collect_vars_located)
        }
      })
    // Function declarations are NOT VarDeclaredNames: at the body top level
    // their names are declared alongside collect_hoisted_funcs, and inside
    // blocks they are block-scoped lexical bindings (§14.2.3). Sloppy-mode
    // Annex B var promotion is handled separately via the parser-populated
    // RawFunctionInfo.annexb_candidates.
    ast.FunctionDeclaration(..) -> []
    _ -> []
  }
}

// ============================================================================
// Lexical (let/const/class) name collection
// ============================================================================

/// FunctionDeclarations may sit under labels (`l: function f() {}`, sloppy
/// mode only); labels are transparent for declaration instantiation.
pub fn peel_labels(stmt: ast.Statement) -> ast.Statement {
  case stmt {
    ast.LabeledStatement(_, body) -> peel_labels(body)
    _ -> stmt
  }
}

/// Names of FunctionDeclarations sitting directly in a statement list.
pub fn direct_fn_names(stmts: List(ast.StmtWithLine)) -> List(String) {
  list.filter_map(stmts, fn(located) {
    case peel_labels(located.statement) {
      ast.FunctionDeclaration(Some(ast.NamedBinding(name:, ..)), ..) -> Ok(name)
      _ -> Error(Nil)
    }
  })
}

/// §14.12.4 step 3-5: a switch's CaseBlock is ONE block scope, so its
/// BlockDeclarationInstantiation runs over every case body concatenated in
/// source order. Returns that single flat statement list — the input to
/// `emit_switch`'s `emit_block_declarations`. The parser's
/// `scope.sb_reorder_switch_children` puts the corresponding CHILD SCOPES
/// into the same emission order (fn-decls, then case tests, then the rest)
/// so emit's positional child-scope cursor stays in lockstep.
pub fn switch_case_stmts(
  cases: List(ast.SwitchCase),
) -> List(ast.StmtWithLine) {
  list.flat_map(cases, fn(c) { c.consequent })
}

/// Collect let/const names declared directly in the given statement list (NOT
/// recursing into nested blocks). Returned tuple is `(name, is_const)` —
/// `is_const = True` for const/using/await using, `False` for let/class.
/// emit.gleam's same-named `collect_top_lex_names` wraps this, mapping
/// `is_const` onto its own `LetBinding`/`ConstBinding` type.
pub fn collect_top_lex_names(
  stmts: List(ast.StmtWithLine),
) -> List(#(String, Bool)) {
  list.flat_map(stmts, fn(located) {
    case located.statement {
      ast.VariableDeclaration(ast.Let, declarators) ->
        declarator_names(declarators) |> list.map(fn(n) { #(n, False) })
      ast.VariableDeclaration(ast.Const, declarators)
      | ast.VariableDeclaration(ast.Using, declarators)
      | ast.VariableDeclaration(ast.AwaitUsing, declarators) ->
        declarator_names(declarators) |> list.map(fn(n) { #(n, True) })
      ast.ClassDeclaration(name: Some(ast.NamedBinding(name:, ..)), ..) -> [
        #(name, False),
      ]
      _ -> []
    }
  })
}

/// True when a block body declares anything block-scoped: let/const/class or
/// a (possibly labeled) function/generator/async declaration. Blocks without
/// such declarations create no bindings and emit_block elides their scope.
pub fn block_has_declarations(body: List(ast.StmtWithLine)) -> Bool {
  list.any(body, fn(located) {
    case peel_labels(located.statement) {
      ast.VariableDeclaration(ast.Let, _)
      | ast.VariableDeclaration(ast.Const, _)
      | ast.VariableDeclaration(ast.Using, _)
      | ast.VariableDeclaration(ast.AwaitUsing, _)
      | ast.ClassDeclaration(..)
      | ast.FunctionDeclaration(..) -> True
      _ -> False
    }
  })
}

/// let names of a classic-for head `for (let x = ...; ...)`.
pub fn for_let_names(
  kind: ast.VariableKind,
  declarations: List(ast.VariableDeclarator),
) -> List(String) {
  case kind {
    ast.Let -> declarator_names(declarations)
    ast.Var | ast.Const | ast.Using | ast.AwaitUsing -> []
  }
}

/// True when a classic `for (init; ...; ...)` head declares lexical
/// (let/const/using) bindings — i.e. the parser pushed a Block scope for
/// the head and emit must consume it. var/expr/empty heads push no scope.
pub fn for_classic_init_is_lex(init: Option(ast.ForInit)) -> Bool {
  case init {
    Some(ast.ForInitDeclaration(ast.Let, _))
    | Some(ast.ForInitDeclaration(ast.Const, _))
    | Some(ast.ForInitDeclaration(ast.Using, _))
    | Some(ast.ForInitDeclaration(ast.AwaitUsing, _)) -> True
    _ -> False
  }
}

// ============================================================================
// Directives / strictness / using
// ============================================================================

/// True when the directive prologue of a statement list contains a Use
/// Strict Directive (§11.2.2).
pub fn has_use_strict_directive(stmts: List(ast.StmtWithLine)) -> Bool {
  case stmts {
    [
      ast.StmtWithLine(
        statement: ast.ExpressionStatement(
          expression: ast.StringExpression(_, _),
          directive: directive,
        ),
        ..,
      ),
      ..rest
    ] ->
      case directive {
        Some("use strict") -> True
        _ -> has_use_strict_directive(rest)
      }
    _ -> False
  }
}

/// Split off the directive prologue (leading string-literal expression
/// statements with a recorded directive token) from a statement list.
pub fn split_directives(
  stmts: List(ast.StmtWithLine),
) -> #(List(ast.StmtWithLine), List(ast.StmtWithLine)) {
  list.split_while(stmts, fn(s) {
    case s.statement {
      ast.ExpressionStatement(directive: Some(_), ..) -> True
      _ -> False
    }
  })
}

/// Lower a Module body to the statement list `emit_module` compiles.
/// Imports and re-export-only items are dropped; an `export <decl>` is
/// unwrapped to its inner declaration; a NAMED
/// `export default function foo() {}` / `export default class Foo {}`
/// is lowered to the ordinary FunctionDeclaration / ClassDeclaration so
/// the name is hoisted as a module-level binding (§16.2.3.7 BoundNames).
/// An ANONYMOUS `export default <expr>` becomes the ExpressionStatement
/// `*default* = expr;` — the `*default*` local was declared during module
/// hoisting, and the synthesized assignment carries the declaration span
/// so errors point at real source.
pub fn module_items_to_stmts(
  items: List(ast.ModuleItem),
) -> List(ast.StmtWithLine) {
  list.filter_map(items, fn(item) {
    case item {
      ast.StatementItem(s) -> Ok(s)
      ast.ExportNamedDeclaration(declaration: Some(decl), ..) ->
        Ok(ast.StmtWithLine(0, decl))
      ast.ExportDefaultDeclaration(
        declaration: ast.FunctionExpression(
          name: Some(_) as name,
          params:,
          body:,
          is_generator:,
          is_async:,
          span: _,
        ),
        ..,
      ) ->
        Ok(ast.StmtWithLine(
          0,
          ast.FunctionDeclaration(
            name:,
            params:,
            body:,
            is_generator:,
            is_async:,
          ),
        ))
      ast.ExportDefaultDeclaration(
        declaration: ast.ClassExpression(
          name: Some(_) as name,
          super_class:,
          body:,
          span: _,
        ),
        ..,
      ) ->
        Ok(ast.StmtWithLine(0, ast.ClassDeclaration(name:, super_class:, body:)))
      ast.ExportDefaultDeclaration(declaration: expr, span:) ->
        Ok(ast.StmtWithLine(
          0,
          ast.ExpressionStatement(
            expression: ast.AssignmentExpression(
              operator: ast.Assign,
              left: ast.Identifier(name: "*default*", span:),
              right: expr,
              span:,
            ),
            directive: None,
          ),
        ))
      ast.ImportDeclaration(..)
      | ast.ExportNamedDeclaration(declaration: None, ..)
      | ast.ExportAllDeclaration(..) -> Error(Nil)
    }
  })
}

/// True if a statement list contains a `using` / `await using` declaration
/// at its top level (no recursion into nested blocks).
pub fn has_using_decl(stmts: List(ast.StmtWithLine)) -> Bool {
  list.any(stmts, fn(s) {
    case s.statement {
      ast.VariableDeclaration(kind: ast.Using, ..)
      | ast.VariableDeclaration(kind: ast.AwaitUsing, ..) -> True
      _ -> False
    }
  })
}

// ============================================================================
// Expression shape predicates
// ============================================================================

/// If `expr` is a non-computed `obj.prop` MemberExpression, return
/// `Some(#(obj, "prop"))`. Folds the Identifier / defensive-StringExpression
/// OR-pattern that the emitter's assignment-target arms otherwise repeat to
/// pick the PutField fast path. (The parser emits computed=True for
/// `obj["s"]`, so the StringExpression case is belt-and-braces.)
pub fn member_static_prop(
  expr: ast.Expression,
) -> Option(#(ast.Expression, String)) {
  case expr {
    ast.MemberExpression(_, obj, ast.Identifier(name:, ..), False)
    | ast.MemberExpression(_, obj, ast.StringExpression(_, name), False) ->
      Some(#(obj, name))
    _ -> None
  }
}

/// Strip ParenthesizedExpression wrappers (possibly nested).
/// Used to look through parens when the spec says they're transparent.
pub fn unwrap_parens(expr: ast.Expression) -> ast.Expression {
  case expr {
    ast.ParenthesizedExpression(_, inner) -> unwrap_parens(inner)
    _ -> expr
  }
}

/// §13.3.9 OptionalExpression: does this member/call/tagged-template chain
/// contain a `?.` link? Recursion stops at any non-chain node — in
/// particular a ParenthesizedExpression is a chain BOUNDARY (parentheses
/// re-enter the grammar via PrimaryExpression): `(a?.b).c` must NOT
/// short-circuit the outer `.c`, and `(a?.b)() = 1` stays web-compat.
pub fn chain_has_optional(expr: ast.Expression) -> Bool {
  case expr {
    ast.OptionalMemberExpression(..) | ast.OptionalCallExpression(..) -> True
    ast.MemberExpression(object:, ..) -> chain_has_optional(object)
    ast.CallExpression(callee:, ..) -> chain_has_optional(callee)
    ast.TaggedTemplateExpression(tag:, ..) -> chain_has_optional(tag)
    _ -> False
  }
}

/// True if any call argument is a SpreadElement.
pub fn has_spread_arg(args: List(ast.Expression)) -> Bool {
  list.any(args, fn(a) {
    case a {
      ast.SpreadElement(_, _) -> True
      _ -> False
    }
  })
}

/// True if any array-literal element is Some(SpreadElement(_)). Used to
/// choose the fast static-arity path vs the incremental-build spread path.
pub fn has_spread_element(elements: List(Option(ast.Expression))) -> Bool {
  list.any(elements, fn(el) {
    case el {
      Some(ast.SpreadElement(_, _)) -> True
      _ -> False
    }
  })
}

// ============================================================================
// Class-scope synthetic binding names
//
// A class body opens a per-class lexical scope holding hidden const bindings
// (private-name keys, the [[Fields]] initializer closure, stashed computed
// element keys, stashed instance private-method closures). These names are
// minted by the EMITTER but the PARSER must pre-register them in the
// ClassBody scope (see parser.gleam's class-body handling, which folds
// `class_body_bindings` into `sb_declare`) so the emitter's later
// (scope_id, name) lookups resolve. Both sides therefore call the same
// naming functions below — one source of truth for the slot layout.
// ============================================================================

/// Partition a class body into (constructor, instance methods, static
/// methods, instance fields, static elements). Static elements are static
/// fields + static blocks, interleaved in source order — §15.7.14 step 31
/// requires textual order. Class bodies are tiny (≪50 elements) and this
/// runs once per class, so five filter passes are no worse than one fold +
/// five reverses, and avoid 5-tuple unpack/repack boilerplate in every arm.
///
/// The PARSER (which orders the ClassBody scope's children at the closing
/// `}` — see parser.gleam's class-body scope handling) and emit.gleam's
/// `compile_class_body` MUST partition identically so child scopes are
/// created/consumed in the EXACT order `child_fn_cursor` entries are
/// popped — hence one shared copy.
pub fn classify_class_body(
  body: List(ast.ClassElement),
) -> #(
  Option(ast.ClassElement),
  List(ast.ClassElement),
  List(ast.ClassElement),
  List(ast.ClassElement),
  List(ast.ClassElement),
) {
  let ctor =
    list.find(body, fn(el) {
      case el {
        ast.ClassMethod(kind: ast.MethodConstructor, ..) -> True
        _ -> False
      }
    })
    |> option.from_result
  let instance_methods =
    list.filter(body, fn(el) {
      case el {
        ast.ClassMethod(kind: ast.MethodConstructor, ..) -> False
        ast.ClassMethod(is_static: False, ..) -> True
        _ -> False
      }
    })
  let static_methods =
    list.filter(body, fn(el) {
      case el {
        ast.ClassMethod(kind: ast.MethodConstructor, ..) -> False
        ast.ClassMethod(is_static: True, ..) -> True
        _ -> False
      }
    })
  let instance_fields =
    list.filter(body, fn(el) {
      case el {
        ast.ClassField(is_static: False, ..) -> True
        _ -> False
      }
    })
  let static_elements =
    list.filter(body, fn(el) {
      case el {
        ast.ClassField(is_static: True, ..) | ast.StaticBlock(..) -> True
        _ -> False
      }
    })
  #(ctor, instance_methods, static_methods, instance_fields, static_elements)
}

/// QuickJS JS_ATOM_class_fields_init. Declared as a const in the per-class
/// block scope and captured via the ordinary closure path — `<...>` is
/// outside §12.7 IdentifierName grammar so it can't collide with user code.
pub const class_fields_init = "<class_fields_init>"

/// All private element names ("#m") declared by a class body, deduped
/// (get/set pairs share one PrivateName), in source order. These become
/// class-scope consts bound to freshly minted PrivateName keys — the spec
/// PrivateEnvironment (§15.7.14 steps 5/6) mapped onto lexical scoping.
pub fn class_private_names(body: List(ast.ClassElement)) -> List(String) {
  list.fold(body, [], fn(acc, elem) {
    let name = case elem {
      ast.ClassMethod(key: ast.Identifier(name: "#" <> rest, ..), ..)
      | ast.ClassField(key: ast.Identifier(name: "#" <> rest, ..), ..) ->
        Some("#" <> rest)
      _ -> None
    }
    case name {
      Some(n) ->
        case list.contains(acc, n) {
          True -> acc
          False -> [n, ..acc]
        }
      None -> acc
    }
  })
  |> list.reverse
}

/// Hidden class-scope const name holding the closure of an instance private
/// method/accessor half. NUL-prefixed so source code can never name it.
pub fn private_fn_const(kind: ast.MethodKind, name: String) -> String {
  case kind {
    ast.MethodGet -> "\u{0}pg:" <> name
    ast.MethodSet -> "\u{0}ps:" <> name
    ast.MethodMethod | ast.MethodConstructor -> "\u{0}pm:" <> name
  }
}

/// Hidden class-scope const holding the stashed property key of the Nth
/// class element's computed name (field or method). §15.7.14: each
/// ClassElementName is evaluated ONCE at class-definition time, in source
/// order; method definitions and the instance/static init fns read the
/// captured key, never re-evaluate the expression. NUL-prefixed so source
/// code can never name it.
pub fn computed_field_const(idx: Int) -> String {
  "\u{0}ck:" <> int.to_string(idx)
}

/// All computed element names — fields AND methods, instance AND static, in
/// source order — paired with their body index. This is the order keys are
/// evaluated in at class definition time: §15.7.14 evaluates each
/// ClassElementName in one source-order pass over the class body, so
/// intercalated method/field and static/non-static keys all share one pass.
pub fn computed_element_keys(
  body: List(ast.ClassElement),
) -> List(#(Int, ast.Expression)) {
  list.index_map(body, fn(elem, idx) { #(idx, elem) })
  |> list.filter_map(fn(pair) {
    let #(idx, elem) = pair
    case elem {
      ast.ClassField(key:, computed: True, ..) -> Ok(#(idx, key))
      ast.ClassMethod(key:, computed: True, ..) -> Ok(#(idx, key))
      _ -> Error(Nil)
    }
  })
}

/// Ordered list of every synthetic const the per-class scope (V8/SM
/// ClassBody scope) declares — the EXACT slot-allocation order the PARSER
/// must `sb_declare` into the ClassBody scope for a ClassExpression /
/// ClassDeclaration (its sole caller: parser.gleam folds this list through
/// `scope.sb_declare` when it opens a class body) so that the emitter's
/// later (scope_id, name) lookups (emit_var_init / emit_var_get in
/// compile_class / compile_class_body / emit_class_methods /
/// emit_field_init_call) hit the same slot indices. Every entry is a
/// const binding. Order:
///   1. inner class-name binding (only if the class has a BindingIdentifier)
///   2. <class_fields_init> — the [[Fields]] initializer-closure const
///   3. each private name "#x" in source order (deduped get/set pairs)
///   4. each instance private-method closure stash "\u{0}pm:/pg:/ps:#x"
///   5. each computed-element-key stash "\u{0}ck:N" in body-index order
/// The emitter re-derives the same names via the naming helpers above
/// (`class_fields_init`, `private_fn_const`, `computed_field_const`) when
/// it emits each element, so this list is the ONE source of truth for the
/// class-scope slot layout.
pub fn class_body_bindings(
  binding_name: Option(String),
  body: List(ast.ClassElement),
) -> List(String) {
  let inner = case binding_name {
    Some(n) -> [n]
    None -> []
  }
  // Hidden const names for every instance private method/accessor half
  // (declared up front in compile_class so child-fn closure snapshots see
  // the slots).
  let private_fn_consts =
    list.filter_map(body, fn(elem) {
      case elem {
        ast.ClassMethod(
          key: ast.Identifier(name: "#" <> rest, ..),
          kind:,
          is_static: False,
          ..,
        ) -> Ok(private_fn_const(kind, "#" <> rest))
        _ -> Error(Nil)
      }
    })
  let computed =
    list.map(computed_element_keys(body), fn(pair) {
      computed_field_const(pair.0)
    })
  list.flatten([
    inner,
    [class_fields_init],
    class_private_names(body),
    private_fn_consts,
    computed,
  ])
}
