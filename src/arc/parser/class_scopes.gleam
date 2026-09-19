import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope
import arc/compiler/scope_builder
import arc/parser/ast
import gleam/bool
import gleam/list
import gleam/option.{type Option, None, Some}

pub fn mark_assign_targets(
  scopes: scope_builder.ScopeBuilder,
  lhs: ast.Expression,
) -> scope_builder.ScopeBuilder {
  case lhs {
    ast.Identifier(name:, ..) -> scope_builder.assign_ref(scopes, name)
    ast.ParenthesizedExpression(expression:, ..) ->
      mark_assign_targets(scopes, expression)
    ast.ArrayExpression(elements:, ..) ->
      list.fold(elements, scopes, fn(scopes, elem) {
        case elem {
          None -> scopes
          Some(ast.SpreadElement(argument:, ..)) ->
            mark_assign_targets(scopes, argument)
          Some(e) -> mark_assign_element(scopes, e)
        }
      })
    ast.ObjectExpression(properties:, ..) ->
      list.fold(properties, scopes, fn(scopes, prop) {
        case prop {
          ast.InitProperty(value:, ..) -> mark_assign_element(scopes, value)
          ast.SpreadProperty(argument:) -> mark_assign_targets(scopes, argument)
          ast.MethodProperty(..) | ast.AccessorProperty(..) -> scopes
        }
      })
    _ -> scopes
  }
}

fn mark_assign_element(
  scopes: scope_builder.ScopeBuilder,
  expr: ast.Expression,
) -> scope_builder.ScopeBuilder {
  case expr {
    ast.AssignmentExpression(operator: ast.Assign, left:, ..) ->
      mark_assign_targets(scopes, left)
    _ -> mark_assign_targets(scopes, expr)
  }
}

// the only signal never_box_names sees for var/param collisions
pub fn mark_pattern_assigned(
  scopes: scope_builder.ScopeBuilder,
  pattern: ast.Pattern,
) -> scope_builder.ScopeBuilder {
  list.fold(ast.pattern_bound_names(pattern), scopes, scope_builder.assign_ref)
}

// must agree with emit.compile_function_body
pub fn declare_param_shims(
  scopes: scope_builder.ScopeBuilder,
  params: List(ast.Pattern),
) -> scope_builder.ScopeBuilder {
  let #(fixed, _rest) = ast_util.split_trailing_rest(params)
  case ast_util.all_simple_params(fixed) {
    True -> scopes
    False -> scope_builder.insert_param_shims(scopes, list.length(fixed))
  }
}

// matches emit.compile_class_body order
pub type ClassScopeIds {
  ClassScopeIds(
    class_id: scope.ScopeId,
    init_id: scope.ScopeId,
    static_id: scope.ScopeId,
  )
}

// key_scopes: scopes pushed while parsing a computed key
pub type ClassElementScopes {
  MethodScopes(key_scopes: List(scope.ScopeId), method_fn_id: scope.ScopeId)
  NonMethodScopes(key_scopes: List(scope.ScopeId))
}

pub type ParsedClassElement {
  ParsedClassElement(element: ast.ClassElement, scopes: ClassElementScopes)
}

// children_at is newest-first, so new ids are the prefix
pub fn class_new_children(
  scopes: scope_builder.ScopeBuilder,
  parent_id: scope.ScopeId,
  before: List(scope.ScopeId),
) -> List(scope.ScopeId) {
  scope_builder.children_since(scopes, parent_id, before) |> list.reverse
}

// 7-step child order of class_scope_finalize; emit reads it positionally
pub fn class_scope_finalize(
  scopes: scope_builder.ScopeBuilder,
  ids: ClassScopeIds,
  name: Option(String),
  has_super_class has_super_class: Bool,
  heritage_scopes heritage_scopes: List(scope.ScopeId),
  parsed parsed: List(ParsedClassElement),
) -> scope_builder.ScopeBuilder {
  let elements = list.map(parsed, fn(el) { el.element })
  // same slot order the emitter looks up
  let scopes =
    list.fold(
      ast_util.class_body_bindings(name, elements),
      scopes,
      fn(scopes, n) {
        scope_builder.declare_in(
          scopes,
          ids.class_id,
          n,
          scope.ConstBinding,
          synthetic: True,
        )
      },
    )
  // (4) computed keys, source order
  let key_scopes = list.flat_map(parsed, fn(el) { el.scopes.key_scopes })
  let MethodScopeBuckets(constructor:, instance_methods:, static_methods:) =
    method_scope_buckets(parsed)
  // (1) instance init needed (§7.3.29)
  let needs_instance_init =
    list.any(elements, fn(el) {
      case el {
        ast.ClassMethod(key: ast.PrivateName(..), is_static: False, ..) -> True
        _ -> ast_util.is_instance_field(el)
      }
    })
  // (7) static init needed
  let needs_static_init = list.any(elements, ast_util.is_static_element)
  let scopes =
    finalize_field_shell(
      scopes,
      ids.init_id,
      elements,
      is_static: False,
      needed: needs_instance_init,
    )
  let scopes =
    finalize_field_shell(
      scopes,
      ids.static_id,
      elements,
      is_static: True,
      needed: needs_static_init,
    )
  // (2) constructor: always one function child
  let #(scopes, ctor_id) = case constructor {
    Some(id) -> #(scopes, id)
    None ->
      scope_builder.push(
        scope_builder.enter(scopes, ids.class_id),
        scope.Function,
      )
  }
  let scopes =
    class_seed_ctor_shell(
      scopes,
      ctor_id,
      needs_instance_init:,
      is_synthetic: option.is_none(constructor),
      has_super_class:,
    )
  let init_part = case needs_instance_init {
    True -> [ids.init_id]
    False -> []
  }
  let static_part = case needs_static_init {
    True -> [ids.static_id]
    False -> []
  }
  let ordered =
    list.flatten([
      init_part,
      [ctor_id],
      heritage_scopes,
      key_scopes,
      instance_methods,
      static_methods,
      static_part,
    ])
  scope_builder.set_children(scopes, ids.class_id, ordered)
  |> scope_builder.enter(ids.class_id)
}

type MethodScopeBuckets {
  MethodScopeBuckets(
    constructor: Option(scope.ScopeId),
    instance_methods: List(scope.ScopeId),
    static_methods: List(scope.ScopeId),
  )
}

// (2)(5)(6) buckets must match ast_util.classify_class_body
fn method_scope_buckets(
  parsed: List(ParsedClassElement),
) -> MethodScopeBuckets {
  let empty =
    MethodScopeBuckets(
      constructor: None,
      instance_methods: [],
      static_methods: [],
    )
  // parsed is in source order, so fold from the right to keep it
  use buckets, ParsedClassElement(element:, scopes:) <- list.fold_right(
    parsed,
    empty,
  )
  case scopes {
    NonMethodScopes(..) -> buckets
    MethodScopes(method_fn_id: id, ..) ->
      case ast_util.class_element_bucket(element) {
        ast_util.ConstructorBucket ->
          MethodScopeBuckets(..buckets, constructor: Some(id))
        ast_util.InstanceMethodBucket ->
          MethodScopeBuckets(..buckets, instance_methods: [
            id,
            ..buckets.instance_methods
          ])
        ast_util.StaticMethodBucket ->
          MethodScopeBuckets(..buckets, static_methods: [
            id,
            ..buckets.static_methods
          ])
        // unreachable in practice
        ast_util.InstanceFieldBucket | ast_util.StaticElementBucket -> buckets
      }
  }
}

// drop an unneeded shell, else flip its children and seed refs
fn finalize_field_shell(
  scopes: scope_builder.ScopeBuilder,
  shell_id: scope.ScopeId,
  elements: List(ast.ClassElement),
  is_static is_static: Bool,
  needed needed: Bool,
) -> scope_builder.ScopeBuilder {
  use <- bool.lazy_guard(!needed, fn() {
    scope_builder.discard(scopes, shell_id)
  })
  let children =
    scope_builder.children_newest_first(scopes, shell_id) |> list.reverse
  class_seed_field_shell(scopes, shell_id, elements, is_static:)
  |> scope_builder.set_children(shell_id, children)
}

// ref to the field-key stash const emit reads
fn class_ref_field_key(
  scopes: scope_builder.ScopeBuilder,
  key: ast.PropertyName,
  idx: Int,
) -> scope_builder.ScopeBuilder {
  case key {
    ast.ComputedName(..) ->
      scope_builder.ref(scopes, ast_util.computed_field_const(idx))
    ast.PrivateName(name:, ..) -> scope_builder.ref(scopes, name)
    ast.IdentifierName(..) | ast.StringName(..) | ast.NumberName(..) -> scopes
    ast.BigIntName(..) -> scopes
  }
}

// seed synthetic refs emit's compile_class_init_fn reads
fn class_seed_field_shell(
  scopes: scope_builder.ScopeBuilder,
  shell_id: scope.ScopeId,
  elements: List(ast.ClassElement),
  is_static is_static: Bool,
) -> scope_builder.ScopeBuilder {
  let scopes = scope_builder.enter(scopes, shell_id)
  // §10.2.11 step 22
  let scopes =
    scope_builder.declare_in(
      scopes,
      shell_id,
      "arguments",
      scope.VarBinding,
      synthetic: True,
    )
  let scopes = scope_builder.lexical_ref(scopes, lexical.ThisRef)
  // §7.3.29 private methods read #x and its stash
  let scopes = case is_static {
    True -> scopes
    False ->
      list.fold(elements, scopes, fn(scopes, element) {
        case element {
          ast.ClassMethod(
            key: ast.PrivateName(name:, ..),
            kind:,
            is_static: False,
            ..,
          ) ->
            scopes
            |> scope_builder.ref(name)
            |> scope_builder.ref(ast_util.private_fn_const(kind, name))
          _ -> scopes
        }
      })
  }
  list.index_fold(elements, scopes, fn(scopes, element, idx) {
    case element {
      ast.ClassField(key:, is_static: s, ..) if s == is_static ->
        class_ref_field_key(scopes, key, idx)
      _ -> scopes
    }
  })
}

// what super(...) reads: the active function, new.target and this
pub fn super_call_refs(
  scopes: scope_builder.ScopeBuilder,
) -> scope_builder.ScopeBuilder {
  scopes
  |> scope_builder.lexical_ref(lexical.ActiveFuncRef)
  |> scope_builder.lexical_ref(lexical.NewTargetRef)
  |> scope_builder.lexical_ref(lexical.ThisRef)
}

// synthetic refs the emitter adds to the constructor
fn class_seed_ctor_shell(
  scopes: scope_builder.ScopeBuilder,
  ctor_id: scope.ScopeId,
  needs_instance_init needs_instance_init: Bool,
  is_synthetic is_synthetic: Bool,
  has_super_class has_super_class: Bool,
) -> scope_builder.ScopeBuilder {
  let scopes =
    scope_builder.enter(scopes, ctor_id)
    |> scope_builder.update_current_fn(fn(fi) {
      scope_builder.RawFunctionInfo(
        ..fi,
        is_derived_constructor: has_super_class,
      )
    })
  // synthetic ctor never declared arguments
  let scopes = case is_synthetic {
    True ->
      scope_builder.declare_in(
        scopes,
        ctor_id,
        "arguments",
        scope.VarBinding,
        synthetic: True,
      )
    False -> scopes
  }
  let scopes = case needs_instance_init {
    True ->
      scopes
      |> scope_builder.ref(ast_util.class_fields_init)
      |> scope_builder.lexical_ref(lexical.ThisRef)
    False -> scopes
  }
  case is_synthetic && has_super_class {
    True -> super_call_refs(scopes) |> scope_builder.ref("arguments")
    False -> scopes
  }
}
