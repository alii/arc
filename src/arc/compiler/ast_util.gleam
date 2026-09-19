import arc/compiler/scope.{type BindingKind}
import arc/module/summary
import arc/parser/ast
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

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

// §10.2.11 step 22, parser and emitter must agree
pub fn all_simple_params(fixed: List(ast.Pattern)) -> Bool {
  use p <- list.all(fixed)
  case p {
    ast.IdentifierPattern(..) -> True
    _ -> False
  }
}

fn declarator_names(declarators: List(ast.VariableDeclarator)) -> List(String) {
  list.flat_map(declarators, fn(d) {
    let ast.VariableDeclarator(pattern, _) = d
    ast.pattern_bound_names(pattern)
  })
}

// §8.2.6 vardeclarednames, does not enter nested functions
pub fn var_declared_names(stmts: List(ast.StmtWithLine)) -> List(String) {
  list.flat_map(stmts, collect_vars_located)
  |> list.unique()
}

fn collect_vars_located(s: ast.StmtWithLine) -> List(String) {
  collect_vars_stmt(s.statement)
}

fn collect_vars_stmts(stmts: List(ast.StmtWithLine)) -> List(String) {
  list.flat_map(stmts, collect_vars_located)
}

fn collect_vars_stmt(stmt: ast.Statement) -> List(String) {
  case stmt {
    ast.VariableDeclaration(ast.Var, declarators) ->
      declarator_names(declarators)
    ast.BlockStatement(body) -> collect_vars_stmts(body)
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
    ast.TryStatement(block, tail) -> {
      let tail_vars = case tail {
        ast.TryCatch(ast.CatchClause(_, body)) -> collect_vars_stmts(body)
        ast.TryFinally(finalizer) -> collect_vars_stmts(finalizer)
        ast.TryCatchFinally(ast.CatchClause(_, body), finalizer) ->
          list.append(collect_vars_stmts(body), collect_vars_stmts(finalizer))
      }
      list.append(collect_vars_stmts(block), tail_vars)
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
    ast.FunctionDeclaration(..) -> []
    // leaves spelled out on purpose, no catch-all
    ast.EmptyStatement
    | ast.ExpressionStatement(..)
    | ast.VariableDeclaration(ast.Let, _)
    | ast.VariableDeclaration(ast.Const, _)
    | ast.VariableDeclaration(ast.Using, _)
    | ast.VariableDeclaration(ast.AwaitUsing, _)
    | ast.ReturnStatement(..)
    | ast.ThrowStatement(..)
    | ast.BreakStatement(..)
    | ast.ContinueStatement(..)
    | ast.DebuggerStatement
    | ast.ClassDeclaration(..) -> []
  }
}

pub fn binding_kind_of(kind: ast.VariableKind) -> BindingKind {
  case kind {
    ast.Var -> scope.VarBinding
    ast.Let -> scope.LetBinding
    ast.Const | ast.Using | ast.AwaitUsing -> scope.ConstBinding
  }
}

pub fn is_lexical(kind: ast.VariableKind) -> Bool {
  case kind {
    ast.Var -> False
    ast.Let | ast.Const | ast.Using | ast.AwaitUsing -> True
  }
}

pub fn peel_labels(stmt: ast.Statement) -> ast.Statement {
  case stmt {
    ast.LabeledStatement(_, body) -> peel_labels(body)
    _ -> stmt
  }
}

pub fn top_level_function_names(stmts: List(ast.StmtWithLine)) -> List(String) {
  list.filter_map(stmts, fn(located) {
    case peel_labels(located.statement) {
      ast.FunctionDeclaration(Some(ast.NamedBinding(name:, ..)), ..) -> Ok(name)
      _ -> Error(Nil)
    }
  })
}

// §8.2.7 varscopeddeclarations by name: vars plus top-level functions
pub fn var_scoped_names(stmts: List(ast.StmtWithLine)) -> List(String) {
  list.append(var_declared_names(stmts), top_level_function_names(stmts))
}

// §14.12.4: the whole caseblock is one block scope
pub fn switch_case_stmts(
  cases: List(ast.SwitchCase),
) -> List(ast.StmtWithLine) {
  list.flat_map(cases, fn(c) { c.consequent })
}

// §8.2.4 lexicallydeclarednames paired with is_const
pub fn lexically_declared_names(
  stmts: List(ast.StmtWithLine),
) -> List(#(String, Bool)) {
  list.flat_map(stmts, fn(located) {
    case located.statement {
      ast.VariableDeclaration(kind, declarators) ->
        case kind {
          ast.Var -> []
          ast.Let ->
            declarator_names(declarators) |> list.map(fn(n) { #(n, False) })
          ast.Const | ast.Using | ast.AwaitUsing ->
            declarator_names(declarators) |> list.map(fn(n) { #(n, True) })
        }
      ast.ClassDeclaration(name: Some(ast.NamedBinding(name:, ..)), ..) -> [
        #(name, False),
      ]
      _ -> []
    }
  })
}

pub fn block_has_declarations(body: List(ast.StmtWithLine)) -> Bool {
  list.any(body, fn(located) {
    case peel_labels(located.statement) {
      ast.VariableDeclaration(kind, _) -> is_lexical(kind)
      ast.ClassDeclaration(..) | ast.FunctionDeclaration(..) -> True
      _ -> False
    }
  })
}

pub fn for_let_names(
  kind: ast.VariableKind,
  declarations: List(ast.VariableDeclarator),
) -> List(String) {
  case kind {
    ast.Let -> declarator_names(declarations)
    ast.Var | ast.Const | ast.Using | ast.AwaitUsing -> []
  }
}

pub fn for_classic_init_is_lex(init: Option(ast.ForInit)) -> Bool {
  case init {
    Some(ast.ForInitDeclaration(kind, _)) -> is_lexical(kind)
    _ -> False
  }
}

pub fn for_head_lex_names(left: ast.ForInit) -> List(String) {
  case left {
    ast.ForInitDeclaration(kind, declarators) ->
      case is_lexical(kind) {
        True -> declarator_names(declarators)
        False -> []
      }
    ast.ForInitPattern(_) | ast.ForInitExpression(_) -> []
  }
}

pub fn has_use_strict_directive(stmts: List(ast.StmtWithLine)) -> Bool {
  case stmts {
    [
      ast.StmtWithLine(
        statement: ast.ExpressionStatement(
          expression: ast.StringLiteral(_, _),
          directive:,
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

// lower module items to plain statements for emit
pub fn module_items_to_stmts(
  items: List(ast.ModuleItem),
) -> List(ast.StmtWithLine) {
  list.filter_map(items, fn(item) {
    case item {
      ast.StatementItem(s) -> Ok(s)
      ast.ExportDeclaration(declaration:, line:, ..) ->
        Ok(ast.StmtWithLine(line, ast.declaration_to_statement(declaration)))
      ast.ExportDefaultDeclaration(
        declaration: ast.FunctionExpression(
          name: Some(_) as name,
          params:,
          body:,
          is_generator:,
          is_async:,
          span: _,
        ),
        line:,
        ..,
      ) ->
        Ok(ast.StmtWithLine(
          line,
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
        line:,
        ..,
      ) ->
        Ok(ast.StmtWithLine(
          line,
          ast.ClassDeclaration(name:, super_class:, body:),
        ))
      ast.ExportDefaultDeclaration(declaration: expr, line:, span:) ->
        Ok(ast.StmtWithLine(
          line,
          ast.ExpressionStatement(
            expression: ast.AssignmentExpression(
              operator: ast.Assign,
              left: ast.Identifier(
                name: summary.default_export_local_name,
                span:,
              ),
              right: expr,
              span:,
            ),
            directive: None,
          ),
        ))
      ast.ImportDeclaration(..)
      | ast.ExportNamed(..)
      | ast.ExportAllDeclaration(..) -> Error(Nil)
    }
  })
}

pub fn has_using_decl(stmts: List(ast.StmtWithLine)) -> Bool {
  list.any(stmts, fn(s) {
    case s.statement {
      ast.VariableDeclaration(kind: ast.Using, ..)
      | ast.VariableDeclaration(kind: ast.AwaitUsing, ..) -> True
      _ -> False
    }
  })
}

pub fn unwrap_parens(expr: ast.Expression) -> ast.Expression {
  case expr {
    ast.ParenthesizedExpression(_, inner) -> unwrap_parens(inner)
    _ -> expr
  }
}

pub fn is_bare_identifier(expr: ast.Expression) -> Bool {
  case expr {
    ast.Identifier(..) -> True
    _ -> False
  }
}

pub fn is_bare_private_name(expr: ast.Expression) -> Bool {
  case expr {
    ast.Identifier(name: "#" <> _, ..) -> True
    _ -> False
  }
}

pub fn is_private_name_access(expr: ast.Expression) -> Bool {
  case expr {
    ast.MemberExpression(property: ast.Dot(name: "#" <> _, ..), ..)
    | ast.OptionalMemberExpression(property: ast.Dot(name: "#" <> _, ..), ..) ->
      True
    _ -> False
  }
}

// §13.3.9, parens end the chain: (a?.b).c
pub fn chain_has_optional(expr: ast.Expression) -> Bool {
  case expr {
    ast.OptionalMemberExpression(..) | ast.OptionalCallExpression(..) -> True
    ast.MemberExpression(object:, ..) -> chain_has_optional(object)
    ast.CallExpression(callee:, ..) -> chain_has_optional(callee)
    ast.TaggedTemplateExpression(tag:, ..) -> chain_has_optional(tag)
    _ -> False
  }
}

pub fn has_spread_arg(args: List(ast.Expression)) -> Bool {
  list.any(args, fn(a) {
    case a {
      ast.SpreadElement(_, _) -> True
      _ -> False
    }
  })
}

pub fn has_spread_element(elements: List(Option(ast.Expression))) -> Bool {
  list.any(elements, fn(el) {
    case el {
      Some(ast.SpreadElement(_, _)) -> True
      _ -> False
    }
  })
}

pub type ClassMethodElement {
  ClassMethodElement(
    body_index: Int,
    key: ast.PropertyName,
    kind: ast.MethodKind,
    fun: ast.FunctionLiteral,
  )
}

pub type ClassFieldElement {
  ClassFieldElement(
    body_index: Int,
    key: ast.PropertyName,
    value: Option(ast.Expression),
  )
}

pub type StaticElement {
  StaticFieldElement(ClassFieldElement)
  StaticBlockElement(List(ast.StmtWithLine))
}

pub type ClassBodyParts {
  ClassBodyParts(
    constructor: Option(ClassMethodElement),
    instance_methods: List(ClassMethodElement),
    static_methods: List(ClassMethodElement),
    instance_fields: List(ClassFieldElement),
    static_elements: List(StaticElement),
  )
}

pub type ClassElementBucket {
  ConstructorBucket
  InstanceMethodBucket
  StaticMethodBucket
  InstanceFieldBucket
  StaticElementBucket
}

// the one partition, parser and emitter share it
pub fn class_element_bucket(el: ast.ClassElement) -> ClassElementBucket {
  case el {
    ast.ClassMethod(kind: ast.ConstructorMethod, ..) -> ConstructorBucket
    ast.ClassMethod(is_static: False, ..) -> InstanceMethodBucket
    ast.ClassMethod(is_static: True, ..) -> StaticMethodBucket
    ast.ClassField(is_static: False, ..) -> InstanceFieldBucket
    ast.ClassField(is_static: True, ..) | ast.StaticBlock(..) ->
      StaticElementBucket
  }
}

pub fn is_instance_field(el: ast.ClassElement) -> Bool {
  class_element_bucket(el) == InstanceFieldBucket
}

pub fn is_static_element(el: ast.ClassElement) -> Bool {
  class_element_bucket(el) == StaticElementBucket
}

fn as_method_element(
  entry: #(Int, ast.ClassElement),
) -> Result(ClassMethodElement, Nil) {
  let #(body_index, el) = entry
  case el {
    ast.ClassMethod(key:, value:, kind:, ..) ->
      Ok(ClassMethodElement(body_index:, key:, kind:, fun: value))
    ast.ClassField(..) | ast.StaticBlock(..) -> Error(Nil)
  }
}

fn as_field_element(
  entry: #(Int, ast.ClassElement),
) -> Result(ClassFieldElement, Nil) {
  let #(body_index, el) = entry
  case el {
    ast.ClassField(key:, value:, ..) ->
      Ok(ClassFieldElement(body_index:, key:, value:))
    ast.ClassMethod(..) | ast.StaticBlock(..) -> Error(Nil)
  }
}

fn as_static_element(
  entry: #(Int, ast.ClassElement),
) -> Result(StaticElement, Nil) {
  case entry.1 {
    ast.ClassField(..) ->
      as_field_element(entry) |> result.map(StaticFieldElement)
    ast.StaticBlock(body:) -> Ok(StaticBlockElement(body))
    ast.ClassMethod(..) -> Error(Nil)
  }
}

pub fn classify_class_body(body: List(ast.ClassElement)) -> ClassBodyParts {
  let indexed = list.index_map(body, fn(el, idx) { #(idx, el) })
  let of_bucket = fn(bucket: ClassElementBucket) {
    list.filter(indexed, fn(entry) { class_element_bucket(entry.1) == bucket })
  }
  ClassBodyParts(
    constructor: of_bucket(ConstructorBucket)
      |> list.first
      |> result.try(as_method_element)
      |> option.from_result,
    instance_methods: of_bucket(InstanceMethodBucket)
      |> list.filter_map(as_method_element),
    static_methods: of_bucket(StaticMethodBucket)
      |> list.filter_map(as_method_element),
    instance_fields: of_bucket(InstanceFieldBucket)
      |> list.filter_map(as_field_element),
    static_elements: of_bucket(StaticElementBucket)
      |> list.filter_map(as_static_element),
  )
}

// not an identifiername so user code cannot collide
pub const class_fields_init = "<class_fields_init>"

pub fn class_private_names(body: List(ast.ClassElement)) -> List(String) {
  list.fold(body, [], fn(acc, elem) {
    let name = case elem {
      ast.ClassMethod(key: ast.PrivateName(name:, ..), ..)
      | ast.ClassField(key: ast.PrivateName(name:, ..), ..) -> Some(name)
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

// nul prefix so source can never name it
pub fn private_fn_const(kind: ast.MethodKind, name: String) -> String {
  case kind {
    ast.GetterMethod -> "\u{0}pg:" <> name
    ast.SetterMethod -> "\u{0}ps:" <> name
    ast.PlainMethod | ast.ConstructorMethod -> "\u{0}pm:" <> name
  }
}

// nul prefix so source can never name it
pub fn computed_field_const(idx: Int) -> String {
  "\u{0}ck:" <> int.to_string(idx)
}

pub fn computed_element_keys(
  body: List(ast.ClassElement),
) -> List(#(Int, ast.Expression)) {
  list.index_map(body, fn(elem, idx) { #(idx, elem) })
  |> list.filter_map(fn(pair) {
    let #(idx, elem) = pair
    case elem {
      ast.ClassField(key: ast.ComputedName(expression:), ..)
      | ast.ClassMethod(key: ast.ComputedName(expression:), ..) ->
        Ok(#(idx, expression))
      _ -> Error(Nil)
    }
  })
}

// exact class scope slot order, parser and emitter must match
pub fn class_body_bindings(
  binding_name: Option(String),
  body: List(ast.ClassElement),
) -> List(String) {
  let inner = case binding_name {
    Some(n) -> [n]
    None -> []
  }
  let private_fn_consts =
    list.filter_map(body, fn(elem) {
      case elem {
        ast.ClassMethod(
          key: ast.PrivateName(name:, ..),
          kind:,
          is_static: False,
          ..,
        ) -> Ok(private_fn_const(kind, name))
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

// §10.2.11; must match emit's non_simple_fixed
pub fn fixed_params_non_simple(params: List(ast.Pattern)) -> Bool {
  let #(fixed, _rest) = split_trailing_rest(params)
  !all_simple_params(fixed)
}
