import arc/bytecode/key
import arc/bytecode/lexical
import arc/compiler/ast_util
import arc/compiler/scope
import arc/parser/ast
import arc/rt/val as rt_val
import arc_aot/emit/anf.{type Build}
import arc_aot/emit/state.{type EmitError, type Emitter2}
import carder/ir
import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

pub const perf5_to_property_key_split: Bool = True

pub const perf8_int_const_shift: Bool = True

pub fn ask(
  e: Emitter2,
  k: fn(Emitter2, Emitter2) -> #(ir.Expr, Emitter2),
) -> #(ir.Expr, Emitter2) {
  k(e, e)
}

pub fn modify(f: fn(Emitter2) -> Emitter2) -> Build(Nil) {
  fn(e, k) { k(f(e), Nil) }
}

pub fn consts() -> Build(state.RealmConsts) {
  fn(e: Emitter2, k) { k(e, e.consts) }
}

// never panic: emit a runtime throw, yield undef so k still runs
pub fn throw_at_rt(op: String, msg: String) -> Build(ir.Value) {
  use _ <- anf.then(anf.host(op, [ir.ConstBinary(bit_array.from_string(msg))]))
  use rc <- anf.then(consts())
  anf.pure(rc.undef)
}

// parser-unreachable shapes throw at runtime too
pub fn unreachable(why: String) -> Build(ir.Value) {
  throw_at_rt("throw_type_error", "emit_2core/expr: unreachable: " <> why)
}

fn describe_error(err: EmitError) -> String {
  case err {
    state.BreakOutsideLoop -> "break outside loop"
    state.ContinueOutsideLoop -> "continue outside loop"
    state.EarlySyntaxError(message:) -> message
    state.UnsupportedFeature(feature:) -> "unsupported: " <> feature
    state.ScopeCursorDesync(..) -> "scope cursor desync"
  }
}

pub fn bridge_value(
  call: fn(Emitter2) -> Result(#(Emitter2, ir.Value), EmitError),
) -> Build(ir.Value) {
  fn(e, k) {
    case call(e) {
      Ok(#(e, v)) -> k(e, v)
      Error(err) -> throw_at_rt("throw_type_error", describe_error(err))(e, k)
    }
  }
}

pub fn bridge_expr(
  call: fn(Emitter2) -> Result(#(ir.Expr, Emitter2), EmitError),
) -> Build(ir.Value) {
  fn(e, k) {
    case call(e) {
      Ok(#(tree, e)) -> {
        let #(name, e) = state.fresh_var(e)
        anf.wrap(k(e, ir.Var(name)), ir.Let([name], tree, _))
      }
      Error(err) -> throw_at_rt("throw_type_error", describe_error(err))(e, k)
    }
  }
}

// named is the namedevaluation hint for anonymous fn/class
fn emit(ex: ast.Expression, named: Option(String)) -> Build(ir.Value) {
  case ex {
    ast.NumberLiteral(_, value) -> number_literal(value)
    ast.BigIntLiteral(_, n) -> {
      use boxed <- anf.then(
        anf.bind(ir.Convert(ir.BoxInt(ir.W64), ir.ConstI64(n))),
      )
      anf.make_tuple([ir.ConstAtom("js_bigint"), boxed])
    }
    ast.StringExpression(_, s) ->
      anf.pure(ir.ConstBinary(bit_array.from_string(s)))
    ast.BooleanLiteral(_, b) -> {
      use rc <- anf.then(consts())
      anf.pure(case b {
        True -> rc.true_
        False -> rc.false_
      })
    }
    ast.NullLiteral(_) -> anf.then(consts(), fn(rc) { anf.pure(rc.null) })
    ast.UndefinedExpression(_) ->
      anf.then(consts(), fn(rc) { anf.pure(rc.undef) })
    ast.RegExpLiteral(_, pattern, flags) ->
      anf.host("regexp_new", [
        ir.ConstBinary(bit_array.from_string(pattern)),
        ir.ConstBinary(bit_array.from_string(flags)),
      ])
    // named evaluation looks through parens
    ast.ParenthesizedExpression(_, inner) -> emit(inner, named)

    // §13.10.1 #x in obj, lhs is a name not a value
    ast.BinaryExpression(
      operator: ast.In,
      left: ast.Identifier(name: "#" <> rest, ..),
      right:,
      ..,
    ) -> {
      use r <- anf.then(expr(right))
      use k <- anf.then(emit_identifier("#" <> rest))
      anf.host("private_in", [r, k])
    }
    ast.BinaryExpression(operator: op, left:, right:, ..) -> {
      use l <- anf.then(expr_operand(left))
      use r <- anf.then(expr_operand(right))
      binop(op, l, r)
    }

    ast.TemplateLiteral(parts:, ..) -> emit_template_literal(parts)

    // §13.3.11.1 a tagged template is never direct eval
    ast.TaggedTemplateExpression(tag:, parts:, span:) -> {
      use site <- anf.then(next_site())
      let template =
        ast.IntrinsicTemplateObject(
          span:,
          site:,
          quasis: ast.template_quasis(parts),
        )
      let tag = case tag {
        ast.Identifier(span: tag_span, name: "eval") ->
          ast.ParenthesizedExpression(span: tag_span, expression: tag)
        _ -> tag
      }
      emit(
        ast.CallExpression(span:, callee: tag, arguments: [
          template,
          ..ast.template_expressions(parts)
        ]),
        None,
      )
    }

    ast.IntrinsicTemplateObject(site:, quasis:, ..) ->
      emit_template_object(site, quasis)

    ast.LogicalExpression(operator: op, left:, right:, ..) ->
      case op, is_boolean_expr(left) {
        ast.LogicalAnd, True -> {
          use c <- anf.then(cond_i32(left))
          use rc <- anf.then(consts())
          anf.bind_if(c, expr(right), anf.pure(rc.false_))
        }
        ast.LogicalOr, True -> {
          use c <- anf.then(cond_i32(left))
          use rc <- anf.then(consts())
          anf.bind_if(c, anf.pure(rc.true_), expr(right))
        }
        _, _ -> {
          use l <- anf.then(expr(left))
          case op {
            ast.LogicalAnd -> anf.truthy_if(l, expr(right), anf.pure(l))
            ast.LogicalOr -> anf.truthy_if(l, anf.pure(l), expr(right))
            ast.NullishCoalescing -> anf.nullish_if(l, expr(right), anf.pure(l))
          }
        }
      }
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) -> {
      use c <- anf.then(cond_i32(condition))
      anf.bind_if(c, expr(consequent), expr(alternate))
    }
    ast.SequenceExpression(expressions:, ..) -> emit_sequence(expressions)

    ast.UnaryExpression(_, op, arg) -> emit_unary(op, arg)

    ast.MemberExpression(_, ast.SuperExpression(_), property) ->
      emit_super_get(property)
    ast.MemberExpression(_, object, property) ->
      case ast_util.chain_has_optional(object) {
        True -> emit_chain_root(ex)
        False -> {
          use ov <- anf.then(expr(object))
          case object, static_dot_key(property) {
            ast.ThisExpression(_), Some(kb) -> get_prop_this(ov, kb)
            _, _ -> emit_member_get(ov, property)
          }
        }
      }
    ast.OptionalMemberExpression(..) -> emit_chain_root(ex)

    ast.CallExpression(..) ->
      case ast_util.chain_has_optional(ex) {
        True -> emit_chain_root(ex)
        False -> emit_plain_call(ex)
      }
    ast.OptionalCallExpression(..) -> emit_chain_root(ex)

    ast.NewExpression(_, callee, args) -> {
      use c <- anf.then(expr(callee))
      use args_l <- anf.then(emit_args_list(args))
      case ast_util.has_spread_arg(args) {
        True -> anf.host("construct", [c, args_l, c])
        False -> {
          use r <- anf.then(anf.host("new_simple", [c, args_l]))
          use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, r)))
          anf.bind_if(
            is_miss,
            anf.host("construct", [c, args_l, c]),
            anf.pure(r),
          )
        }
      }
    }

    ast.Identifier(name: "undefined", ..) ->
      anf.then(consts(), fn(rc) { anf.pure(rc.undef) })
    ast.Identifier(name: "#" <> _, ..) ->
      throw_at_rt(
        "throw_syntax_error",
        "private field must be declared in an enclosing class",
      )
    ast.Identifier(name:, ..) -> emit_identifier(name)
    ast.ThisExpression(_) -> emit_lexical(lexical.RefThis)
    ast.SuperExpression(_) -> unreachable("bare super")
    ast.SpreadElement(..) -> unreachable("bare spread")
    ast.MetaProperty(_, ast.NewTarget) -> emit_lexical(lexical.RefNewTarget)
    ast.MetaProperty(_, ast.ImportMeta) ->
      throw_at_rt("throw_type_error", "unsupported: import.meta")

    ast.UpdateExpression(_, op, prefix, target) ->
      emit_update(op, prefix, target)
    ast.AssignmentExpression(_, op, left, right) ->
      emit_assignment(op, left, right)

    ast.ObjectExpression(_, properties) -> emit_object(properties, named)
    ast.ArrayExpression(_, elements) -> emit_array(elements)

    ast.FunctionExpression(_, self_name, params, body, is_gen, is_async) -> {
      let self = ast.binding_name(self_name)
      let inferred = case self {
        Some(_) -> self
        None -> named
      }
      emit_function_expr(
        state.FnExpr(self_name: self, is_gen:, is_async:),
        inferred,
        params,
        state.StmtBody(body),
      )
    }
    ast.ArrowFunctionExpression(_, params, body, is_async) -> {
      let fn_body = case body {
        ast.ArrowBodyBlock(stmts) -> state.StmtBody(stmts)
        ast.ArrowBodyExpression(e) -> state.ExprBody(e)
      }
      emit_function_expr(state.Arrow(is_async:), named, params, fn_body)
    }
    ast.ClassExpression(_, self_name, super_class, body) -> {
      let self = ast.binding_name(self_name)
      let inferred = case self {
        Some(_) -> self
        None -> named
      }
      bridge_expr(fn(e) {
        e.dispatch.emit_class(e, self, inferred, super_class, body)
      })
    }

    ast.AwaitExpression(_, argument) -> {
      use v <- anf.then(expr(argument))
      anf.host("await", [v])
    }
    ast.YieldExpression(_, argument, is_delegate) -> {
      use rc <- anf.then(consts())
      use v <- anf.then(case argument {
        Some(a) -> expr(a)
        None -> anf.pure(rc.undef)
      })
      case is_delegate {
        True -> anf.host("yield_star", [v])
        False -> {
          use e <- anf.then(ask)
          case e.is_async {
            True -> {
              use awaited <- anf.then(anf.host("await", [v]))
              anf.host("yield", [awaited])
            }
            False -> anf.host("yield", [v])
          }
        }
      }
    }

    ast.ImportExpression(..) -> {
      use _ <- anf.then(modify(state.mark_unsupported(_, "import()")))
      use rc <- anf.then(consts())
      anf.pure(rc.undef)
    }
  }
}

pub fn expr(ex: ast.Expression) -> Build(ir.Value) {
  emit(ex, None)
}

fn emit_sequence(exprs: List(ast.Expression)) -> Build(ir.Value) {
  case exprs {
    [] -> {
      use rc <- anf.then(consts())
      anf.pure(rc.undef)
    }
    [only] -> expr(only)
    [head, ..tail] -> {
      use _ <- anf.then(expr(head))
      emit_sequence(tail)
    }
  }
}

pub fn emit_expr(
  e: Emitter2,
  ex: ast.Expression,
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  Ok(anf.run(expr(ex), e))
}

pub fn emit_expr_named(
  e: Emitter2,
  ex: ast.Expression,
  named: Option(String),
) -> Result(#(ir.Expr, Emitter2), EmitError) {
  Ok(anf.run(emit(ex, named), e))
}

pub fn emit_identifier(name: String) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case state.resolve(e, name) {
    scope.Plain(d) -> emit_direct_get(d, name)
    scope.WithChain(..) ->
      throw_at_rt("throw_type_error", "unsupported: with (" <> name <> ")")
  }
}

fn binop(op: ast.BinaryOp, l: ir.Value, r: ir.Value) -> Build(ir.Value) {
  case op {
    ast.Add -> anf.guarded_binop("num_add", "add", l, r)
    ast.Subtract -> anf.guarded_binop("num_sub", "sub", l, r)
    ast.Multiply -> anf.guarded_binop("num_mul", "mul", l, r)
    ast.LessThan -> anf.guarded_cmp(ir.NLt, "lt", l, r)
    ast.LessThanEqual -> anf.guarded_cmp(ir.NLe, "le", l, r)
    ast.GreaterThan -> anf.guarded_cmp(ir.NGt, "gt", l, r)
    ast.GreaterThanEqual -> anf.guarded_cmp(ir.NGe, "ge", l, r)
    ast.Divide -> anf.guarded_div(l, r)
    ast.Modulo -> anf.guarded_mod(l, r)
    ast.Exponentiation -> anf.host("pow", [l, r])
    ast.StrictEqual -> {
      use v <- anf.then(strict_eq(l, r))
      anf.i32_to_js_bool(v)
    }
    ast.StrictNotEqual -> {
      use v <- anf.then(strict_eq(l, r))
      use rc <- anf.then(consts())
      anf.bind_if(v, anf.pure(rc.false_), anf.pure(rc.true_))
    }
    ast.Equal -> {
      use v <- anf.then(loose_eq(l, r))
      anf.i32_to_js_bool(v)
    }
    ast.NotEqual -> {
      use v <- anf.then(loose_eq(l, r))
      use rc <- anf.then(consts())
      anf.bind_if(v, anf.pure(rc.false_), anf.pure(rc.true_))
    }
    ast.LeftShift ->
      int_result(l, r, case perf8_int_const_shift {
        True -> int_const_shift("erl_bsl", "shl_fast", "shl", l, r)
        False -> int_fast("shl_fast", "shl", l, r)
      })
    ast.RightShift ->
      int_result(l, r, case perf8_int_const_shift {
        True -> int_const_shift("erl_bsr", "shr_fast", "shr", l, r)
        False -> int_fast("shr_fast", "shr", l, r)
      })
    ast.UnsignedRightShift ->
      int_result(l, r, case perf8_int_const_shift {
        True -> int_fast("ushr_fast", "ushr", l, r)
        False -> anf.host("ushr", [l, r])
      })
    ast.BitwiseAnd ->
      int_result(l, r, int_const_bit("erl_band", "bitand_fast", "bitand", l, r))
    ast.BitwiseOr -> int_result(l, r, int_fast("bitor_fast", "bitor", l, r))
    ast.BitwiseXor -> int_result(l, r, int_fast("bitxor_fast", "bitxor", l, r))
    ast.In -> anf.then(anf.host("op_in", [l, r]), anf.i32_to_js_bool)
    ast.InstanceOf -> anf.then(instance_of_i32(l, r), anf.i32_to_js_bool)
  }
}

fn instance_of_i32(l: ir.Value, r: ir.Value) -> Build(ir.Value) {
  use v <- anf.then(anf.host("instanceof_fast", [l, r]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
  anf.bind_if_i32(is_miss, anf.host("instance_of", [l, r]), anf.pure(v))
}

pub fn cond_i32(cond: ast.Expression) -> Build(ir.Value) {
  case ast_util.unwrap_parens(cond) {
    ast.BinaryExpression(operator: op, left:, right:, ..) ->
      case op {
        ast.Equal -> loose_eq_i32(left, right)
        ast.NotEqual -> anf.then(loose_eq_i32(left, right), not_i32)
        ast.StrictEqual -> strict_eq_i32(left, right)
        ast.StrictNotEqual -> anf.then(strict_eq_i32(left, right), not_i32)
        ast.LessThan -> cond_rel(ir.NLt, "lt", left, right)
        ast.LessThanEqual -> cond_rel(ir.NLe, "le", left, right)
        ast.GreaterThan -> cond_rel(ir.NGt, "gt", left, right)
        ast.GreaterThanEqual -> cond_rel(ir.NGe, "ge", left, right)
        ast.InstanceOf -> {
          use l <- anf.then(expr(left))
          use r <- anf.then(expr(right))
          instance_of_i32(l, r)
        }
        _ -> anf.then(expr(cond), anf.truthy_i32)
      }
    ast.UnaryExpression(operator: ast.LogicalNot, argument:, ..) ->
      anf.then(cond_i32(argument), not_i32)
    ast.LogicalExpression(operator: ast.LogicalAnd, left:, right:, ..) -> {
      use c <- anf.then(cond_i32(left))
      anf.bind_if_i32(c, cond_i32(right), anf.pure(ir.ConstI32(0)))
    }
    ast.LogicalExpression(operator: ast.LogicalOr, left:, right:, ..) -> {
      use c <- anf.then(cond_i32(left))
      anf.bind_if_i32(c, anf.pure(ir.ConstI32(1)), cond_i32(right))
    }
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) -> {
      use c <- anf.then(cond_i32(condition))
      anf.bind_if_i32(c, cond_i32(consequent), cond_i32(alternate))
    }
    ast.BooleanLiteral(value:, ..) ->
      anf.pure(case value {
        True -> ir.ConstI32(1)
        False -> ir.ConstI32(0)
      })
    _ -> anf.then(expr(cond), anf.truthy_i32)
  }
}

fn not_i32(v: ir.Value) -> Build(ir.Value) {
  anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstI32(0)))
}

fn cond_rel(
  fast: ir.NumTermOp,
  slow_op: String,
  left: ast.Expression,
  right: ast.Expression,
) -> Build(ir.Value) {
  use l <- anf.then(expr_operand(left))
  use r <- anf.then(expr_operand(right))
  anf.cond_cmp(fast, slow_op, l, r)
}

fn is_boolean_expr(ex: ast.Expression) -> Bool {
  case ast_util.unwrap_parens(ex) {
    ast.BooleanLiteral(..) -> True
    ast.UnaryExpression(operator: ast.LogicalNot, ..) -> True
    ast.BinaryExpression(operator: op, ..) ->
      case op {
        ast.Equal
        | ast.NotEqual
        | ast.StrictEqual
        | ast.StrictNotEqual
        | ast.LessThan
        | ast.LessThanEqual
        | ast.GreaterThan
        | ast.GreaterThanEqual
        | ast.In
        | ast.InstanceOf -> True
        _ -> False
      }
    ast.LogicalExpression(operator: ast.LogicalAnd, left:, right:, ..)
    | ast.LogicalExpression(operator: ast.LogicalOr, left:, right:, ..) ->
      is_boolean_expr(left) && is_boolean_expr(right)
    ast.ConditionalExpression(consequent:, alternate:, ..) ->
      is_boolean_expr(consequent) && is_boolean_expr(alternate)
    _ -> False
  }
}

pub fn loose_eq_i32(
  left: ast.Expression,
  right: ast.Expression,
) -> Build(ir.Value) {
  use l <- anf.then(expr_operand(left))
  use r <- anf.then(expr_operand(right))
  loose_eq(l, r)
}

fn loose_eq(l: ir.Value, r: ir.Value) -> Build(ir.Value) {
  case is_nullish_const(l), is_nullish_const(r) {
    True, _ -> nul_eq_inline(r)
    _, True -> nul_eq_inline(l)
    False, False ->
      // consti32 with bit 31 set is a wrapped negative, stays on eq_fast
      case l, r {
        ir.ConstI32(c), _ if c >= 0 && c < 0x80000000 -> int_const_eq(r, l)
        _, ir.ConstI32(c) if c >= 0 && c < 0x80000000 -> int_const_eq(l, r)
        ir.ConstI64(_), _ -> int_const_eq(r, l)
        _, ir.ConstI64(_) -> int_const_eq(l, r)
        _, _ -> loose_eq_slow(l, r)
      }
  }
}

fn int_const_eq(v: ir.Value, c: ir.Value) -> Build(ir.Value) {
  use is_i <- anf.then(anf.bind(ir.TermTest(ir.IsInt, v)))
  anf.bind_if(is_i, anf.bind(ir.NumTerm(ir.NEq, v, c)), loose_eq_slow(v, c))
}

fn loose_eq_slow(l: ir.Value, r: ir.Value) -> Build(ir.Value) {
  use v <- anf.then(anf.host("eq_fast", [l, r]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
  anf.bind_if(is_miss, anf.host("eq", [l, r]), anf.pure(v))
}

pub fn strict_eq_i32(
  left: ast.Expression,
  right: ast.Expression,
) -> Build(ir.Value) {
  use l <- anf.then(expr_operand(left))
  use r <- anf.then(expr_operand(right))
  strict_eq(l, r)
}

pub fn case_test_i32(d: ir.Value, selector: ast.Expression) -> Build(ir.Value) {
  use t <- anf.then(expr_operand(selector))
  strict_eq(d, t)
}

fn strict_eq(l: ir.Value, r: ir.Value) -> Build(ir.Value) {
  case l, r {
    ir.ConstAtom(_), _ | ir.ConstBinary(_), _ ->
      anf.bind(ir.NumTerm(ir.NEq, r, l))
    _, ir.ConstAtom(_) | _, ir.ConstBinary(_) ->
      anf.bind(ir.NumTerm(ir.NEq, l, r))
    ir.ConstI32(c), _ if c >= 0 && c < 0x80000000 -> int_const_seq(r, l)
    _, ir.ConstI32(c) if c >= 0 && c < 0x80000000 -> int_const_seq(l, r)
    ir.ConstI64(_), _ -> int_const_seq(r, l)
    _, ir.ConstI64(_) -> int_const_seq(l, r)
    _, _ -> anf.host("strict_eq_i32", [l, r])
  }
}

fn int_const_seq(v: ir.Value, c: ir.Value) -> Build(ir.Value) {
  use is_i <- anf.then(anf.bind(ir.TermTest(ir.IsInt, v)))
  anf.bind_if_i32(
    is_i,
    anf.bind(ir.NumTerm(ir.NEq, v, c)),
    anf.host("strict_eq_i32", [v, c]),
  )
}

fn expr_operand(ex: ast.Expression) -> Build(ir.Value) {
  case ast_util.unwrap_parens(ex) {
    ast.NumberLiteral(_, ast.FiniteNumber(f)) ->
      case small_int_value(f) {
        Some(v) -> anf.mark_number(v)
        None -> expr(ex)
      }
    ast.UnaryExpression(
      operator: ast.Negate,
      argument: ast.NumberLiteral(_, ast.FiniteNumber(f)),
      ..,
    ) ->
      case small_int_value(f) {
        Some(ir.ConstI32(i)) if i > 0 -> anf.pure(ir.ConstI64(-i))
        _ -> expr(ex)
      }
    _ -> expr(ex)
  }
}

fn nul_eq_inline(v: ir.Value) -> Build(ir.Value) {
  use u <- anf.then(anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("undefined"))))
  anf.bind(ir.If(
    u,
    [ir.TI32],
    ir.Values([ir.ConstI32(1)]),
    ir.NumTerm(ir.NEq, v, ir.ConstAtom("null")),
  ))
}

// fold never-reassigned top-level int vars to literals
pub fn analyze_const_globals(
  body: List(ast.StmtWithLine),
) -> dict.Dict(String, ir.Value) {
  let cands =
    list.fold(body, dict.new(), fn(acc, s) {
      case s.statement {
        ast.VariableDeclaration(kind: ast.Var, declarations:) ->
          list.fold(declarations, acc, fn(acc, d) {
            case d {
              ast.VariableDeclarator(
                id: ast.IdentifierPattern(name:, ..),
                init: Some(init),
              ) ->
                case fold_const_init(acc, init) {
                  Some(v) -> dict.insert(acc, name, v)
                  None -> acc
                }
              _ -> acc
            }
          })
        _ -> acc
      }
    })
  case dict.is_empty(cands) {
    True -> cands
    False -> {
      let #(uses, _) =
        list.fold(body, #(Uses(set.new(), False), set.new()), fn(st, s) {
          let #(acc, seen) = st
          case s.statement {
            ast.VariableDeclaration(declarations:, ..) ->
              list.fold(declarations, st, fn(st, d) {
                let #(acc, seen) = st
                let acc = opt_ex_assigned(acc, d.init)
                case d.id, d.init {
                  ast.IdentifierPattern(name:, ..), Some(_) ->
                    case set.contains(seen, name) {
                      True -> #(uses_assign(acc, name), seen)
                      False -> #(acc, set.insert(seen, name))
                    }
                  ast.IdentifierPattern(..), None -> #(acc, seen)
                  p, _ -> #(pat_bound_assigned(acc, p), seen)
                }
              })
            _ -> #(stmt_assigned_globals(acc, s), seen)
          }
        })
      case uses.names_eval {
        True -> dict.new()
        False ->
          dict.filter(cands, fn(name, _) { !set.contains(uses.assigned, name) })
      }
    }
  }
}

type Uses {
  Uses(assigned: set.Set(String), names_eval: Bool)
}

fn uses_assign(acc: Uses, name: String) -> Uses {
  Uses(..acc, assigned: set.insert(acc.assigned, name))
}

fn uses_name(acc: Uses, name: String) -> Uses {
  case name {
    "eval" | "Function" -> Uses(..acc, names_eval: True)
    _ -> acc
  }
}

fn fold_const_init(
  known: dict.Dict(String, ir.Value),
  ex: ast.Expression,
) -> Option(ir.Value) {
  case fold_const_int(known, ex) {
    Some(i) -> small_int_value(int.to_float(i))
    None -> None
  }
}

fn fold_const_int(
  known: dict.Dict(String, ir.Value),
  ex: ast.Expression,
) -> Option(Int) {
  case ex {
    ast.NumberLiteral(_, ast.FiniteNumber(f)) -> small_int_of(f)
    ast.Identifier(name:, ..) ->
      case dict.get(known, name) {
        Ok(ir.ConstI32(v)) -> Some(v)
        _ -> None
      }
    ast.ParenthesizedExpression(expression:, ..) ->
      fold_const_int(known, expression)
    ast.UnaryExpression(operator: ast.BitwiseNot, argument:, ..) ->
      option.map(fold_const_int(known, argument), fn(a) {
        int.bitwise_exclusive_or(a, -1)
      })
    // -0 and 0 * -n are not integers
    ast.UnaryExpression(operator: ast.Negate, argument:, ..) ->
      case fold_const_int(known, argument) {
        Some(0) -> None
        a -> option.map(a, int.negate)
      }
    ast.BinaryExpression(operator:, left:, right:, ..) ->
      case fold_const_int(known, left), fold_const_int(known, right) {
        Some(a), Some(b) ->
          case operator {
            ast.BitwiseOr -> Some(int.bitwise_or(a, b))
            ast.BitwiseAnd -> Some(int.bitwise_and(a, b))
            ast.BitwiseXor -> Some(int.bitwise_exclusive_or(a, b))
            ast.Add -> small_int(a + b)
            ast.Subtract -> small_int(a - b)
            ast.Multiply if a * b == 0 && { a < 0 || b < 0 } -> None
            ast.Multiply -> small_int(a * b)
            _ -> None
          }
        _, _ -> None
      }
    _ -> None
  }
}

// keep every intermediate inside int32 range
fn small_int(i: Int) -> Option(Int) {
  case i >= -2_147_483_648 && i < 2_147_483_648 {
    True -> Some(i)
    False -> None
  }
}

fn small_int_of(f: Float) -> Option(Int) {
  case f >=. -2_147_483_648.0 && f <. 2_147_483_648.0 {
    False -> None
    True -> {
      let i = float.truncate(f)
      case int.to_float(i) == f {
        True -> Some(i)
        False -> None
      }
    }
  }
}

fn small_int_value(f: Float) -> Option(ir.Value) {
  let i = float.truncate(f)
  case
    int.to_float(i) == f
    && i >= 0
    && i < 2_147_483_648
    && !rt_val.is_neg_zero(f)
  {
    True -> Some(ir.ConstI32(i))
    False -> None
  }
}

fn stmt_assigned_globals(acc: Uses, s: ast.StmtWithLine) -> Uses {
  case s.statement {
    ast.EmptyStatement | ast.DebuggerStatement -> acc
    ast.BreakStatement(..) | ast.ContinueStatement(..) -> acc
    ast.ExpressionStatement(expression:, ..) -> ex_assigned(acc, expression)
    ast.BlockStatement(body:) -> list.fold(body, acc, stmt_assigned_globals)
    ast.VariableDeclaration(declarations:, ..) ->
      list.fold(declarations, acc, decl_assigned)
    ast.ReturnStatement(argument:) -> opt_ex_assigned(acc, argument)
    ast.ThrowStatement(argument:) -> ex_assigned(acc, argument)
    ast.IfStatement(condition:, consequent:, alternate:) -> {
      let acc = ex_assigned(acc, condition)
      let acc = st_assigned(acc, consequent)
      case alternate {
        Some(a) -> st_assigned(acc, a)
        None -> acc
      }
    }
    ast.WhileStatement(condition:, body:)
    | ast.DoWhileStatement(condition:, body:) ->
      st_assigned(ex_assigned(acc, condition), body)
    ast.ForStatement(init:, condition:, update:, body:) -> {
      let acc = case init {
        Some(fi) -> for_init_assigned(acc, fi)
        None -> acc
      }
      let acc = opt_ex_assigned(acc, condition)
      let acc = opt_ex_assigned(acc, update)
      st_assigned(acc, body)
    }
    ast.ForInStatement(left:, right:, body:)
    | ast.ForOfStatement(left:, right:, body:, ..) -> {
      let acc = case left {
        ast.ForInitExpression(target) ->
          ex_assigned(assign_target(acc, target), target)
        ast.ForInitDeclaration(declarations:, ..) ->
          list.fold(declarations, acc, fn(acc, d) {
            pat_bound_assigned(opt_ex_assigned(acc, d.init), d.id)
          })
        ast.ForInitPattern(p) -> pat_bound_assigned(acc, p)
      }
      st_assigned(ex_assigned(acc, right), body)
    }
    ast.SwitchStatement(discriminant:, cases:) -> {
      let acc = ex_assigned(acc, discriminant)
      list.fold(cases, acc, fn(acc, c) {
        let acc = opt_ex_assigned(acc, c.condition)
        list.fold(c.consequent, acc, stmt_assigned_globals)
      })
    }
    ast.TryStatement(block:, tail:) -> {
      let acc = list.fold(block, acc, stmt_assigned_globals)
      case tail {
        ast.TryCatch(handler:) -> catch_assigned(acc, handler)
        ast.TryFinally(finalizer:) ->
          list.fold(finalizer, acc, stmt_assigned_globals)
        ast.TryCatchFinally(handler:, finalizer:) ->
          list.fold(
            finalizer,
            catch_assigned(acc, handler),
            stmt_assigned_globals,
          )
      }
    }
    ast.LabeledStatement(body:, ..) -> st_assigned(acc, body)
    ast.WithStatement(object:, body:) ->
      st_assigned(ex_assigned(acc, object), body)
    ast.FunctionDeclaration(body:, params:, ..) ->
      list.fold(
        body,
        list.fold(params, acc, pat_default_assigned),
        stmt_assigned_globals,
      )
    ast.ClassDeclaration(super_class:, body:, ..) ->
      class_body_assigned(opt_ex_assigned(acc, super_class), body)
  }
}

fn st_assigned(acc: Uses, s: ast.Statement) -> Uses {
  stmt_assigned_globals(acc, ast.StmtWithLine(0, s))
}

fn decl_assigned(acc: Uses, d: ast.VariableDeclarator) -> Uses {
  pat_bound_assigned(opt_ex_assigned(acc, d.init), d.id)
}

fn for_init_assigned(acc: Uses, fi: ast.ForInit) -> Uses {
  case fi {
    ast.ForInitExpression(e) -> ex_assigned(acc, e)
    ast.ForInitDeclaration(declarations:, ..) ->
      list.fold(declarations, acc, decl_assigned)
    ast.ForInitPattern(p) -> pat_bound_assigned(acc, p)
  }
}

fn catch_assigned(acc: Uses, handler: ast.CatchClause) -> Uses {
  let acc = case handler.param {
    Some(p) -> pat_default_assigned(acc, p)
    None -> acc
  }
  list.fold(handler.body, acc, stmt_assigned_globals)
}

fn opt_ex_assigned(acc: Uses, e: Option(ast.Expression)) -> Uses {
  case e {
    Some(ex) -> ex_assigned(acc, ex)
    None -> acc
  }
}

fn pat_default_assigned(acc: Uses, p: ast.Pattern) -> Uses {
  case p {
    ast.IdentifierPattern(..) -> acc
    ast.AssignmentPattern(left:, right:) ->
      pat_default_assigned(ex_assigned(acc, right), left)
    ast.RestElement(argument:) -> pat_default_assigned(acc, argument)
    ast.ArrayPattern(elements:) ->
      list.fold(elements, acc, fn(acc, el) {
        case el {
          Some(ep) -> pat_default_assigned(acc, ep)
          None -> acc
        }
      })
    ast.ObjectPattern(properties:) ->
      list.fold(properties, acc, fn(acc, pp) {
        case pp {
          ast.PatternProperty(key:, value:, ..) ->
            pat_default_assigned(key_assigned(acc, key), value)
          ast.RestProperty(..) -> acc
        }
      })
  }
}

fn pat_bound_assigned(acc: Uses, p: ast.Pattern) -> Uses {
  let acc = pat_default_assigned(acc, p)
  list.fold(ast.pattern_bound_names(p), acc, uses_assign)
}

fn class_body_assigned(acc: Uses, body: List(ast.ClassElement)) -> Uses {
  list.fold(body, acc, fn(acc, el) {
    case el {
      ast.ClassMethod(key:, value: ast.FunctionLiteral(body:, params:, ..), ..) ->
        list.fold(
          body,
          list.fold(params, key_assigned(acc, key), pat_default_assigned),
          stmt_assigned_globals,
        )
      ast.ClassField(key:, value:, ..) ->
        opt_ex_assigned(key_assigned(acc, key), value)
      ast.StaticBlock(body:) -> list.fold(body, acc, stmt_assigned_globals)
    }
  })
}

fn key_assigned(acc: Uses, key: ast.PropertyKey) -> Uses {
  case key {
    ast.KeyComputed(expression:) -> ex_assigned(acc, expression)
    _ -> acc
  }
}

fn ex_assigned(acc: Uses, ex: ast.Expression) -> Uses {
  case ex {
    ast.AssignmentExpression(left:, right:, ..) ->
      ex_assigned(ex_assigned(assign_target(acc, left), left), right)
    ast.UpdateExpression(argument:, ..) ->
      ex_assigned(assign_target(acc, argument), argument)
    ast.UnaryExpression(operator: ast.Delete, argument:, ..) ->
      ex_assigned(assign_target(acc, argument), argument)
    // descends into function bodies
    ast.FunctionExpression(body:, params:, ..) ->
      list.fold(
        body,
        list.fold(params, acc, pat_default_assigned),
        stmt_assigned_globals,
      )
    ast.ArrowFunctionExpression(body:, params:, ..) -> {
      let acc = list.fold(params, acc, pat_default_assigned)
      case body {
        ast.ArrowBodyExpression(e) -> ex_assigned(acc, e)
        ast.ArrowBodyBlock(b) -> list.fold(b, acc, stmt_assigned_globals)
      }
    }
    ast.ClassExpression(super_class:, body:, ..) ->
      class_body_assigned(opt_ex_assigned(acc, super_class), body)
    ast.Identifier(name:, ..) -> uses_name(acc, name)
    ast.NumberLiteral(..)
    | ast.BigIntLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..)
    | ast.ThisExpression(..)
    | ast.SuperExpression(..)
    | ast.MetaProperty(..)
    | ast.RegExpLiteral(..)
    | ast.IntrinsicTemplateObject(..)
    | ast.ImportExpression(..) -> acc
    ast.BinaryExpression(left:, right:, ..)
    | ast.LogicalExpression(left:, right:, ..) ->
      ex_assigned(ex_assigned(acc, left), right)
    ast.UnaryExpression(argument:, ..)
    | ast.AwaitExpression(argument:, ..)
    | ast.SpreadElement(argument:, ..) -> ex_assigned(acc, argument)
    ast.YieldExpression(argument:, ..) -> opt_ex_assigned(acc, argument)
    ast.ParenthesizedExpression(expression:, ..) -> ex_assigned(acc, expression)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      ex_assigned(
        ex_assigned(ex_assigned(acc, condition), consequent),
        alternate,
      )
    ast.CallExpression(callee:, arguments:, ..)
    | ast.OptionalCallExpression(callee:, arguments:, ..)
    | ast.NewExpression(callee:, arguments:, ..) ->
      list.fold(arguments, ex_assigned(acc, callee), ex_assigned)
    ast.MemberExpression(object:, property:, ..)
    | ast.OptionalMemberExpression(object:, property:, ..) -> {
      let acc = ex_assigned(acc, object)
      case property {
        ast.Bracket(expression: ast.StringExpression(value:, ..)) ->
          uses_name(acc, value)
        ast.Bracket(expression:) -> ex_assigned(acc, expression)
        ast.Dot(name:, ..) -> uses_name(acc, name)
      }
    }
    ast.SequenceExpression(expressions:, ..) ->
      list.fold(expressions, acc, ex_assigned)
    ast.ArrayExpression(elements:, ..) ->
      list.fold(elements, acc, fn(acc, el) { opt_ex_assigned(acc, el) })
    ast.ObjectExpression(properties:, ..) ->
      list.fold(properties, acc, fn(acc, p) {
        case p {
          ast.InitProperty(key:, value:, ..) ->
            ex_assigned(key_assigned(acc, key), value)
          ast.SpreadProperty(argument:) -> ex_assigned(acc, argument)
          ast.MethodProperty(
            key:,
            value: ast.FunctionLiteral(body:, params:, ..),
          )
          | ast.AccessorProperty(
              key:,
              value: ast.FunctionLiteral(body:, params:, ..),
              ..,
            ) ->
            list.fold(
              body,
              list.fold(params, key_assigned(acc, key), pat_default_assigned),
              stmt_assigned_globals,
            )
        }
      })
    ast.TemplateLiteral(parts:, ..) ->
      list.fold(parts.tail, acc, fn(acc, part) { ex_assigned(acc, part.0) })
    ast.TaggedTemplateExpression(tag:, parts:, ..) ->
      list.fold(parts.tail, ex_assigned(acc, tag), fn(acc, part) {
        ex_assigned(acc, part.0)
      })
  }
}

fn assign_target(acc: Uses, ex: ast.Expression) -> Uses {
  case ast_util.unwrap_parens(ex) {
    ast.Identifier(name:, ..) -> uses_assign(acc, name)
    ast.ArrayExpression(_, elements) ->
      list.fold(elements, acc, fn(acc, el) {
        case el {
          Some(e) -> assign_target(acc, e)
          None -> acc
        }
      })
    ast.SpreadElement(_, argument) -> assign_target(acc, argument)
    ast.AssignmentExpression(_, ast.Assign, left, _) -> assign_target(acc, left)
    ast.ObjectExpression(_, properties) ->
      list.fold(properties, acc, fn(acc, prop) {
        case prop {
          ast.InitProperty(value:, ..) -> assign_target(acc, value)
          ast.SpreadProperty(argument) -> assign_target(acc, argument)
          ast.MethodProperty(..) | ast.AccessorProperty(..) -> acc
        }
      })
    _ -> acc
  }
}

fn int_result(
  l: ir.Value,
  r: ir.Value,
  op: Build(ir.Value),
) -> Build(ir.Value) {
  case l, r {
    ir.ConstI32(_), _ | _, ir.ConstI32(_) -> anf.then(op, anf.mark_number)
    _, _ -> op
  }
}

fn is_nullish_const(v: ir.Value) -> Bool {
  case v {
    ir.ConstAtom("null") | ir.ConstAtom("undefined") -> True
    _ -> False
  }
}

fn int_fast(
  fast: String,
  slow: String,
  l: ir.Value,
  r: ir.Value,
) -> Build(ir.Value) {
  use v <- anf.then(anf.host(fast, [l, r]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
  anf.bind_if(is_miss, anf.host(slow, [l, r]), anf.pure(v))
}

// skipping toint32 is only safe for band with 0 <= c < 2^31
fn int_const_bit(
  bif: String,
  fast: String,
  slow: String,
  l: ir.Value,
  r: ir.Value,
) -> Build(ir.Value) {
  case l, r {
    ir.ConstI32(c), _ if c >= 0 && c < 0x80000000 ->
      int_const_bit_go(bif, fast, slow, r, l)
    _, ir.ConstI32(c) if c >= 0 && c < 0x80000000 ->
      int_const_bit_go(bif, fast, slow, l, r)
    _, _ -> int_fast(fast, slow, l, r)
  }
}

fn int_const_bit_go(
  bif: String,
  fast: String,
  slow: String,
  v: ir.Value,
  c: ir.Value,
) -> Build(ir.Value) {
  use is_i <- anf.then(anf.bind(ir.TermTest(ir.IsInt, v)))
  anf.bind_if(is_i, anf.host(bif, [v, c]), int_fast(fast, slow, v, c))
}

// bare bsr/bsl only valid for l in [0, mask]
fn int_const_shift(
  bif: String,
  fast: String,
  slow: String,
  l: ir.Value,
  r: ir.Value,
) -> Build(ir.Value) {
  case r {
    ir.ConstI32(c) if c >= 0 && c < 32 -> {
      let mask = case bif {
        "erl_bsl" -> int.bitwise_shift_left(1, 31 - c) - 1
        _ -> 0x7FFFFFFF
      }
      use is_i <- anf.then(anf.bind(ir.TermTest(ir.IsInt, l)))
      anf.bind_if(
        is_i,
        {
          use m <- anf.then(anf.host("erl_band", [l, ir.ConstI32(mask)]))
          use ok <- anf.then(anf.bind(ir.NumTerm(ir.NEq, m, l)))
          anf.bind_if(ok, anf.host(bif, [l, r]), anf.host(fast, [l, r]))
        },
        int_fast(fast, slow, l, r),
      )
    }
    _ -> int_fast(fast, slow, l, r)
  }
}

// consti32 carries unsigned bits, negatives box as w64
fn number_literal(n: ast.LiteralNumber) -> Build(ir.Value) {
  case n {
    ast.InfiniteNumber -> anf.then(consts(), fn(rc) { anf.pure(rc.pos_inf) })
    ast.FiniteNumber(f) -> {
      let i = float.truncate(f)
      let integral = int.to_float(i) == f && !rt_val.is_neg_zero(f)
      case integral && i >= 0 && i < 2_147_483_648 {
        True -> anf.bind_number(ir.Convert(ir.BoxInt(ir.W32), ir.ConstI32(i)))
        False ->
          case integral && i < 0 && i > -2_147_483_648 {
            True ->
              anf.bind_number(ir.Convert(ir.BoxInt(ir.W64), ir.ConstI64(i)))
            False ->
              anf.then(
                anf.host("float_lit", [
                  ir.ConstBinary(bit_array.from_string(float.to_string(f))),
                ]),
                anf.mark_number,
              )
          }
      }
    }
  }
}

fn next_site() -> Build(Int) {
  fn(e: Emitter2, k) {
    k(state.Emitter2(..e, next_site: e.next_site + 1), e.next_site)
  }
}

// §13.2.8.5 holes concat via tostring, not toprimitive
fn emit_template_literal(parts: ast.TemplateParts(String)) -> Build(ir.Value) {
  let head = ir.ConstBinary(bit_array.from_string(parts.head))
  list.fold(parts.tail, anf.pure(head), fn(acc_b, part) {
    let #(sub, quasi) = part
    use acc <- anf.then(acc_b)
    use v <- anf.then(expr(sub))
    use a1 <- anf.then(
      anf.miss_or(anf.host("add_prim", [acc, v]), {
        use s <- anf.then(anf.host("to_string", [v]))
        anf.host("string_concat", [acc, s])
      }),
    )
    case quasi {
      "" -> anf.pure(a1)
      _ ->
        anf.host("string_concat", [
          a1,
          ir.ConstBinary(bit_array.from_string(quasi)),
        ])
    }
  })
}

// §13.2.8.4 gettemplateobject, cached per site key
fn emit_template_object(
  site: Int,
  quasis: List(ast.TemplateQuasi),
) -> Build(ir.Value) {
  use e <- anf.then(ask)
  let rc = e.consts
  let cooked =
    list.map(quasis, fn(q) {
      case q.cooked {
        Some(s) -> ir.ConstBinary(bit_array.from_string(s))
        None -> rc.undef
      }
    })
  let raw =
    list.map(quasis, fn(q) { ir.ConstBinary(bit_array.from_string(q.raw)) })
  let site_v =
    ir.ConstBinary(bit_array.from_string(
      e.module_name <> "#" <> int.to_string(site),
    ))
  use cooked_l <- anf.then(anf.cons_list(cooked))
  use raw_l <- anf.then(anf.cons_list(raw))
  anf.host("get_template_object", [site_v, cooked_l, raw_l])
}

pub fn read_slot(slot: Int, boxed: Bool) -> Build(ir.Value) {
  use e <- anf.then(ask)
  let v = ir.Var(state.get_slot_var(e, slot))
  case boxed {
    True -> anf.host("cell_get", [v])
    False -> anf.pure(v)
  }
}

// top-level reads never fold, may run before the var line
fn const_global(e: Emitter2, name: String) -> Option(ir.Value) {
  case e.fn_scope == scope.root_scope_id {
    True -> None
    False -> option.from_result(dict.get(e.const_globals, name))
  }
}

pub fn emit_direct_get(d: scope.Direct, name: String) -> Build(ir.Value) {
  case d {
    scope.Local(slot:, boxed:, origin_kind: scope.VarBinding, ..) -> {
      use e <- anf.then(ask)
      case const_global(e, name) {
        Some(lit) -> anf.pure(lit)
        None -> read_slot(slot, boxed)
      }
    }
    // §9.1.1.1.6 tdz check unless init already emitted here
    scope.Local(slot:, boxed:, origin_kind:, ..) -> {
      use v <- anf.then(read_slot(slot, boxed))
      use e <- anf.then(ask)
      let checked = case origin_kind {
        scope.LetBinding | scope.ConstBinding | scope.FnNameBinding ->
          !set.contains(e.initialized, slot)
        _ -> False
      }
      case checked {
        False -> anf.pure(v)
        True -> {
          use _ <- anf.then(
            anf.host("tdz_check", [
              v,
              ir.ConstBinary(bit_array.from_string(name)),
            ]),
          )
          anf.pure(v)
        }
      }
    }
    scope.Global(name: g) -> {
      use e <- anf.then(ask)
      case dict.get(e.slotted_globals, g) {
        Ok(slot) -> read_slot(slot, True)
        Error(Nil) ->
          case const_global(e, g) {
            Some(lit) -> anf.pure(lit)
            None -> global_read(e, g)
          }
      }
    }
    scope.EvalEnv(..) ->
      throw_at_rt(
        "throw_type_error",
        "unsupported: direct eval (" <> name <> ")",
      )
  }
}

fn global_read(e: Emitter2, g: String) -> Build(ir.Value) {
  let key = ir.ConstBinary(bit_array.from_string(g))
  case e.fn_scope == scope.root_scope_id {
    True -> anf.host("global_get", [key])
    False -> {
      use site <- anf.then(next_site())
      let site = ir.ConstI32(site)
      use v <- anf.then(anf.host("global_get_fast", [key, site]))
      use miss <- anf.then(
        anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("miss"))),
      )
      anf.bind_if(miss, anf.host("global_get_miss", [key, site]), anf.pure(v))
    }
  }
}

fn resolve_lexical(
  e: Emitter2,
  ref: lexical.LexicalRef,
) -> Option(#(Int, Bool)) {
  let info = state.fn_info(e)
  case lexical.lexical_slot(info.lexical, ref) {
    Some(slot) -> Some(#(slot, state.lexical_is_boxed(e, info, ref)))
    None ->
      case dict.get(info.lexical_captures, ref) {
        Ok(slot) -> Some(#(slot, True))
        Error(Nil) -> None
      }
  }
}

pub fn emit_lexical(ref: lexical.LexicalRef) -> Build(ir.Value) {
  use e <- anf.then(ask)
  use v <- anf.then(lexical_value(ref))
  case ref, e.this_tdz {
    lexical.RefThis, True -> {
      use _ <- anf.then(anf.host("check_this", [v]))
      anf.pure(v)
    }
    _, _ -> anf.pure(v)
  }
}

fn lexical_value(ref: lexical.LexicalRef) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case resolve_lexical(e, ref) {
    Some(#(slot, boxed)) -> read_slot(slot, boxed)
    None -> anf.pure(e.consts.undef)
  }
}

pub fn derived_return_value(v: ir.Value) -> Build(ir.Value) {
  use rc <- anf.then(consts())
  use is_undef <- anf.then(anf.bind(ir.NumTerm(ir.NEq, v, rc.undef)))
  anf.bind_if(is_undef, lexical_value(lexical.RefThis), anf.pure(v))
}

// §10.2.4 bindthisvalue, throws if already initialized
fn set_lexical_this(v: ir.Value) -> Build(Nil) {
  use e <- anf.then(ask)
  case resolve_lexical(e, lexical.RefThis) {
    None -> anf.pure(Nil)
    Some(#(slot, boxed)) -> {
      use _ <- anf.then(this_check_init(slot, boxed))
      anf.host_unit("cell_set", [ir.Var(state.get_slot_var(e, slot)), v])
    }
  }
}

fn this_check_init(slot: Int, boxed: Bool) -> Build(Nil) {
  use rc <- anf.then(consts())
  use cur <- anf.then(read_slot(slot, boxed))
  // term identity, the sentinel is not a js value
  use is_tdz <- anf.then(anf.bind(ir.NumTerm(ir.NEq, cur, rc.tdz)))
  use _ <- anf.then(anf.bind_if(
    is_tdz,
    anf.pure(rc.undef),
    throw_at_rt(
      "throw_reference_error",
      "Super constructor may only be called once",
    ),
  ))
  anf.pure(Nil)
}

pub fn to_property_key(v: ir.Value) -> Build(ir.Value) {
  case perf5_to_property_key_split {
    False -> anf.host("to_property_key", [v])
    True -> {
      use k <- anf.then(anf.host("to_property_key_fast", [v]))
      use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, k)))
      anf.bind_if(is_miss, anf.host("to_property_key", [v]), anf.pure(k))
    }
  }
}

// §6.2.5.5 toobject(base) happens before key coercion
pub fn to_property_key_of(base: ir.Value, v: ir.Value) -> Build(ir.Value) {
  use k <- anf.then(anf.host("to_property_key_fast", [v]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, k)))
  anf.bind_if(is_miss, anf.host("to_property_key_of", [base, v]), anf.pure(k))
}

pub fn emit_key(pk: ast.PropertyKey) -> Build(ir.Value) {
  case pk {
    ast.KeyIdentifier(..)
    | ast.KeyString(..)
    | ast.KeyNumber(..)
    | ast.KeyBigInt(..) -> anf.object_key_lit(pk)
    ast.KeyPrivate(name:, ..) -> emit_identifier(name)
    ast.KeyComputed(expression:) -> {
      use v <- anf.then(expr(expression))
      to_property_key(v)
    }
  }
}

pub fn emit_key_from_prop(prop: ast.MemberProperty) -> Build(ir.Value) {
  case prop {
    ast.Dot(name: "#" <> _ as name, ..) -> emit_identifier(name)
    ast.Dot(name:, span:) -> anf.object_key_lit(ast.KeyIdentifier(name:, span:))
    ast.Bracket(expression:) -> {
      use v <- anf.then(expr(expression))
      to_property_key(v)
    }
  }
}

fn is_private_prop(prop: ast.MemberProperty) -> Bool {
  case prop {
    ast.Dot(name: "#" <> _, ..) -> True
    _ -> False
  }
}

fn math_direct_op(
  obj: ast.Expression,
  prop: ast.MemberProperty,
  args: List(ast.Expression),
) -> Option(String) {
  case obj, prop, ast_util.has_spread_arg(args) {
    ast.Identifier(name: "Math", ..), ast.Dot(name: m, ..), False ->
      case m, list.length(args) {
        "sqrt", 1 -> Some("math_sqrt")
        "floor", 1 -> Some("math_floor")
        "abs", 1 -> Some("math_abs")
        "pow", 2 -> Some("math_pow")
        "min", 2 -> Some("math_min")
        "max", 2 -> Some("math_max")
        _, _ -> None
      }
    _, _, _ -> None
  }
}

fn static_dot_key(prop: ast.MemberProperty) -> Option(BitArray) {
  case prop {
    ast.Dot(name: "#" <> _, ..) -> None
    ast.Dot(name:, ..) -> Some(bit_array.from_string(name))
    ast.Bracket(..) -> None
  }
}

fn get_prop_fast(obj: ir.Value, kb: BitArray) -> Build(ir.Value) {
  use site <- anf.then(next_site())
  let key = ir.ConstBinary(kb)
  let site = ir.ConstI32(site)
  use v <- anf.then(anf.host("get_prop_fast", [obj, key, site]))
  use ic_miss <- anf.then(anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("miss"))))
  anf.bind_if(ic_miss, anf.host("get_prop_slow", [obj, key, site]), anf.pure(v))
}

// compare against miss atom, undefined/null are valid hits
fn get_prop_this(obj: ir.Value, kb: BitArray) -> Build(ir.Value) {
  use site <- anf.then(next_site())
  let key = ir.ConstBinary(kb)
  let site = ir.ConstI32(site)
  use v <- anf.then(anf.host("get_prop_ic", [obj, key, site]))
  use ic_miss <- anf.then(anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("miss"))))
  anf.bind_if(ic_miss, anf.host("get_prop_slow", [obj, key, site]), anf.pure(v))
}

fn set_prop_fast(obj: ir.Value, kb: BitArray, v: ir.Value) -> Build(ir.Value) {
  use e <- anf.then(ask)
  let strict = case e.strict {
    True -> ir.ConstAtom("true")
    False -> ir.ConstAtom("false")
  }
  use site <- anf.then(next_site())
  use _ <- anf.then(
    anf.host("set_prop_site", [
      obj,
      ir.ConstBinary(kb),
      v,
      strict,
      ir.ConstI32(site),
    ]),
  )
  anf.pure(v)
}

pub type PropWriteRun {
  PropWriteRun(
    object: ast.Expression,
    first: #(BitArray, ast.Expression),
    rest: List(#(BitArray, ast.Expression)),
  )
}

// later values must be simple so reordering is unobservable
pub fn prop_write_run(
  e: Emitter2,
  ss: List(ast.StmtWithLine),
) -> Option(#(PropWriteRun, List(ast.StmtWithLine))) {
  case ss {
    [ast.StmtWithLine(statement: s, ..), ..tail] ->
      case prop_write(s) {
        Some(#(object, key, value)) ->
          case stable_receiver(e, object) {
            True -> {
              let #(rest, tail) = prop_write_tail(e, object, tail, [])
              case rest {
                [] -> None
                _ -> Some(#(PropWriteRun(object, #(key, value), rest), tail))
              }
            }
            False -> None
          }
        None -> None
      }
    [] -> None
  }
}

fn prop_write_tail(
  e: Emitter2,
  object: ast.Expression,
  ss: List(ast.StmtWithLine),
  acc: List(#(BitArray, ast.Expression)),
) -> #(List(#(BitArray, ast.Expression)), List(ast.StmtWithLine)) {
  let done = fn() { #(list.reverse(acc), ss) }
  case ss {
    [ast.StmtWithLine(statement: s, ..), ..tail] ->
      case prop_write(s) {
        Some(#(o, key, value)) ->
          case same_receiver(object, o) && simple(e, value) {
            True -> prop_write_tail(e, object, tail, [#(key, value), ..acc])
            False -> done()
          }
        None -> done()
      }
    [] -> done()
  }
}

fn prop_write(
  s: ast.Statement,
) -> Option(#(ast.Expression, BitArray, ast.Expression)) {
  case s {
    ast.ExpressionStatement(
      expression: ast.AssignmentExpression(
        operator: ast.Assign,
        left: ast.MemberExpression(object:, property:, ..),
        right:,
        ..,
      ),
      ..,
    ) ->
      case static_dot_key(property) {
        Some(kb) -> Some(#(object, kb, right))
        None -> None
      }
    _ -> None
  }
}

fn same_receiver(a: ast.Expression, b: ast.Expression) -> Bool {
  case a, b {
    ast.ThisExpression(_), ast.ThisExpression(_) -> True
    ast.Identifier(name: x, ..), ast.Identifier(name: y, ..) -> x == y
    _, _ -> False
  }
}

fn stable_receiver(e: Emitter2, ex: ast.Expression) -> Bool {
  case ex {
    ast.ThisExpression(_) -> !e.this_tdz
    ast.Identifier(name:, ..) -> register_local(e, name)
    _ -> False
  }
}

fn register_local(e: Emitter2, name: String) -> Bool {
  case state.resolve(e, name) {
    scope.Plain(scope.Local(slot:, boxed: False, origin_kind:, ..)) ->
      case origin_kind {
        scope.VarBinding | scope.ParamBinding -> True
        scope.LetBinding | scope.ConstBinding | scope.FnNameBinding ->
          set.contains(e.initialized, slot)
        _ -> False
      }
    _ -> False
  }
}

fn simple(e: Emitter2, ex: ast.Expression) -> Bool {
  case ex {
    ast.NumberLiteral(..)
    | ast.StringExpression(..)
    | ast.BooleanLiteral(..)
    | ast.NullLiteral(..)
    | ast.UndefinedExpression(..) -> True
    ast.ThisExpression(_) -> !e.this_tdz
    ast.Identifier(name:, ..) -> register_local(e, name)
    ast.ParenthesizedExpression(_, inner) -> simple(e, inner)
    ast.UnaryExpression(
      operator: ast.Negate,
      argument: ast.NumberLiteral(..),
      ..,
    ) -> True
    ast.UnaryExpression(operator: ast.LogicalNot, argument:, ..) ->
      simple(e, argument)
    ast.LogicalExpression(left:, right:, ..) ->
      simple(e, left) && simple(e, right)
    ast.ConditionalExpression(condition:, consequent:, alternate:, ..) ->
      simple(e, condition) && simple(e, consequent) && simple(e, alternate)
    _ -> False
  }
}

pub fn emit_prop_write_run(run: PropWriteRun) -> Build(ir.Value) {
  let PropWriteRun(object:, first: #(k0, v0), rest:) = run
  use obj <- anf.then(expr(object))
  use v0 <- anf.then(expr(v0))
  use vs <- anf.then(anf.seq(list.map(rest, fn(p) { expr(p.1) })))
  use keys <- anf.then(
    anf.cons_list([
      ir.ConstBinary(k0),
      ..list.map(rest, fn(p) { ir.ConstBinary(p.0) })
    ]),
  )
  use vals <- anf.then(anf.cons_list([v0, ..vs]))
  use e <- anf.then(ask)
  let strict = case e.strict {
    True -> ir.ConstAtom("true")
    False -> ir.ConstAtom("false")
  }
  use site <- anf.then(next_site())
  anf.host("set_props_init", [obj, keys, vals, strict, ir.ConstI32(site)])
}

// §13.15.2 step 6.b.iv strict failed set throws
pub fn set_prop_op_name(strict: Bool) -> String {
  case strict {
    True -> "set_prop_strict"
    False -> "set_prop"
  }
}

fn set_prop_op() -> Build(String) {
  use e <- anf.then(ask)
  anf.pure(set_prop_op_name(e.strict))
}

// §13.5.1.2 step 5.b.i strict failed delete throws
fn delete_prop_op() -> Build(String) {
  use e <- anf.then(ask)
  anf.pure(case e.strict {
    True -> "delete_prop_strict"
    False -> "delete_prop"
  })
}

// callers own topropertykey so read-modify-write coerces once
fn get_elem_fast(
  obj: ir.Value,
  idx: ir.Value,
  slow: Build(ir.Value),
) -> Build(ir.Value) {
  use v <- anf.then(anf.host("get_elem_fast", [obj, idx]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, v)))
  anf.bind_if(is_miss, slow, anf.pure(v))
}

fn set_elem_fast(
  obj: ir.Value,
  idx: ir.Value,
  v: ir.Value,
  slow: Build(ir.Value),
) -> Build(ir.Value) {
  use r <- anf.then(anf.host("set_elem_fast", [obj, idx, v]))
  use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, r)))
  use _ <- anf.then(anf.bind_if(is_miss, slow, anf.pure(v)))
  anf.pure(v)
}

pub fn emit_member_get(
  obj: ir.Value,
  prop: ast.MemberProperty,
) -> Build(ir.Value) {
  case static_dot_key(prop) {
    Some(kb) -> get_prop_fast(obj, kb)
    None ->
      case prop {
        ast.Bracket(expression:) -> {
          use idx <- anf.then(expr(expression))
          get_elem_fast(obj, idx, {
            use k <- anf.then(to_property_key_of(obj, idx))
            anf.host("get_prop", [obj, k])
          })
        }
        _ -> {
          use k <- anf.then(emit_key_from_prop(prop))
          case is_private_prop(prop) {
            True -> anf.host("private_get", [obj, k])
            False -> anf.host("get_prop", [obj, k])
          }
        }
      }
  }
}

pub fn emit_super_get(prop: ast.MemberProperty) -> Build(ir.Value) {
  use this <- anf.then(emit_lexical(lexical.RefThis))
  use ho <- anf.then(emit_lexical(lexical.RefHomeObject))
  use k <- anf.then(emit_key_from_prop(prop))
  anf.host("super_get", [ho, this, k])
}

pub fn emit_args_list(args: List(ast.Expression)) -> Build(ir.Value) {
  case ast_util.has_spread_arg(args) {
    False -> anf.then(anf.seq(list.map(args, expr)), anf.cons_list)
    True -> {
      use acc0 <- anf.then(anf.host("empty_list", []))
      fold_args_spread(args, acc0)
    }
  }
}

fn fold_args_spread(
  args: List(ast.Expression),
  acc: ir.Value,
) -> Build(ir.Value) {
  case args {
    [] -> anf.pure(acc)
    [ast.SpreadElement(_, arg), ..rest] -> {
      use v <- anf.then(expr(arg))
      use acc <- anf.then(anf.host("spread_into_list", [acc, v]))
      fold_args_spread(rest, acc)
    }
    [arg, ..rest] -> {
      use v <- anf.then(expr(arg))
      use acc <- anf.then(anf.host("list_append_one", [acc, v]))
      fold_args_spread(rest, acc)
    }
  }
}

pub fn emit_call(
  f: ir.Value,
  this: ir.Value,
  args_l: ir.Value,
) -> Build(ir.Value) {
  anf.host("call_fast", [f, this, args_l])
}

pub fn emit_call_pos(
  f: ir.Value,
  this: ir.Value,
  pos: List(ir.Value),
) -> Build(ir.Value) {
  case pos {
    [] | [_] | [_, _] | [_, _, _] ->
      anf.host("call_fast" <> int.to_string(list.length(pos)), [f, this, ..pos])
    _ -> {
      use args_l <- anf.then(anf.cons_list(pos))
      emit_call(f, this, args_l)
    }
  }
}

pub type CallArgs {
  Consed(ir.Value)
  Positional(List(ir.Value))
}

pub fn emit_call_with_pair(
  pair: ir.Value,
  f: ir.Value,
  this: ir.Value,
  args: CallArgs,
) -> Build(ir.Value) {
  // pos values already let-bound, safe to cons per arm
  let cons_args = case args {
    Consed(v) -> anf.pure(v)
    Positional(pos) -> anf.cons_list(pos)
  }
  use is_kfn <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, pair)))
  let fast = {
    use rc <- anf.then(consts())
    use code <- anf.then(anf.bind(anf.tuple_get(pair, 0)))
    use this_r <- anf.then(anf.bind(anf.tuple_get(pair, 1)))
    let frame_path = {
      use args_l <- anf.then(cons_args)
      use frame <- anf.then(anf.make_tuple([this_r, f, rc.undef, rc.undef]))
      anf.bind(ir.CallClosure(code, [frame, args_l]))
    }
    case args {
      Consed(_) -> frame_path
      Positional(pos) -> {
        use simple <- anf.then(anf.bind(anf.tuple_get(pair, 2)))
        use is_some <- anf.then(anf.bind(ir.TermTest(ir.IsTuple, simple)))
        let simple_fast = {
          use inner <- anf.then(anf.bind(anf.tuple_get(simple, 1)))
          use code_s <- anf.then(anf.bind(anf.tuple_get(inner, 0)))
          use arity <- anf.then(anf.bind(anf.tuple_get(inner, 1)))
          use needs_this <- anf.then(anf.bind(anf.tuple_get(inner, 2)))
          use n <- anf.then(
            anf.bind(ir.Convert(
              ir.BoxInt(ir.W32),
              ir.ConstI32(list.length(pos)),
            )),
          )
          use ok <- anf.then(anf.bind(ir.NumTerm(ir.NEq, arity, n)))
          anf.bind_if(
            ok,
            {
              use nt <- anf.then(
                anf.bind(ir.NumTerm(ir.NEq, needs_this, rc.true_)),
              )
              anf.bind_if(
                nt,
                anf.bind(ir.CallClosure(code_s, [this_r, ..pos])),
                anf.bind(ir.CallClosure(code_s, pos)),
              )
            },
            frame_path,
          )
        }
        anf.bind_if(is_some, simple_fast, frame_path)
      }
    }
  }
  anf.bind_if(is_kfn, fast, {
    use args_l <- anf.then(cons_args)
    anf.host("call", [f, this, args_l])
  })
}

// args evaluate once, before the probe
fn emit_member_call(
  o: ir.Value,
  prop: ast.MemberProperty,
  args: List(ast.Expression),
) -> Build(ir.Value) {
  case ast_util.has_spread_arg(args) {
    True -> {
      use f <- anf.then(emit_member_get(o, prop))
      use args_l <- anf.then(emit_args_list(args))
      emit_call(f, o, args_l)
    }
    False ->
      case static_dot_key(prop) {
        None -> {
          // computed key evals before args, §13.3.6
          use f <- anf.then(emit_member_get(o, prop))
          use pos <- anf.then(anf.seq(list.map(args, expr)))
          emit_call_pos(f, o, pos)
        }
        Some(kb) -> {
          use pos <- anf.then(anf.seq(list.map(args, expr)))
          call_method_ic_pos(o, kb, pos)
        }
      }
  }
}

fn call_method_ic_pos(
  recv: ir.Value,
  kb: BitArray,
  pos: List(ir.Value),
) -> Build(ir.Value) {
  case pos {
    [] | [_] | [_, _] | [_, _, _] -> {
      use #(site, rsite) <- anf.then(method_sites())
      anf.host("call_method_ic" <> int.to_string(list.length(pos)), [
        recv,
        ir.ConstBinary(kb),
        site,
        rsite,
        ..pos
      ])
    }
    _ -> {
      use args_l <- anf.then(anf.cons_list(pos))
      call_method_ic(recv, kb, args_l)
    }
  }
}

fn call_method_ic(
  recv: ir.Value,
  kb: BitArray,
  args_l: ir.Value,
) -> Build(ir.Value) {
  use #(site, rsite) <- anf.then(method_sites())
  anf.host("call_method_ic", [recv, ir.ConstBinary(kb), args_l, site, rsite])
}

fn method_sites() -> Build(#(ir.Value, ir.Value)) {
  use site <- anf.then(next_site())
  use rsite <- anf.then(next_site())
  anf.pure(#(ir.ConstI32(site), ir.ConstI32(rsite)))
}

// §13.3.7.1 step 12 initialize instance elements
fn emit_field_init_call() -> Build(Nil) {
  use init_fn <- anf.then(emit_identifier(ast_util.class_fields_init))
  use rc <- anf.then(consts())
  use this <- anf.then(emit_lexical(lexical.RefThis))
  use _ <- anf.then(anf.nullish_if(
    init_fn,
    anf.pure(rc.undef),
    anf.then(anf.cons_list([]), fn(nil_args) {
      emit_call(init_fn, this, nil_args)
    }),
  ))
  anf.pure(Nil)
}

// §13.3.7.1 supercall
fn emit_super_call(args: List(ast.Expression)) -> Build(ir.Value) {
  use af <- anf.then(emit_lexical(lexical.RefActiveFunc))
  use nt <- anf.then(emit_lexical(lexical.RefNewTarget))
  use e <- anf.then(ask)
  use args_l <- anf.then(case e.default_ctor, e.raw_args_var {
    True, Some(raw) -> anf.pure(ir.Var(raw))
    _, _ -> emit_args_list(args)
  })
  use inst <- anf.then(anf.host("super_call", [af, args_l, nt]))
  use _ <- anf.then(set_lexical_this(inst))
  use e <- anf.then(ask)
  use _ <- anf.then(case e.field_init {
    state.FieldInitAfterSuper -> emit_field_init_call()
    state.NoFieldInit | state.FieldInitAtStart -> anf.pure(Nil)
  })
  anf.pure(inst)
}

// §13.3.9.1 optional chain, a nullish link breaks with undefined

pub fn emit_chain_root(ex: ast.Expression) -> Build(ir.Value) {
  use rc <- anf.then(consts())
  anf.bind_block(fn(exit) { emit_chain(ex, exit, rc.undef) })
}

fn emit_chain(
  ex: ast.Expression,
  exit: String,
  undef: ir.Value,
) -> Build(ir.Value) {
  case ast_util.chain_has_optional(ex) {
    False -> expr(ex)
    True ->
      case ex {
        ast.MemberExpression(_, obj, prop)
        | ast.OptionalMemberExpression(_, obj, prop) -> {
          use o <- anf.then(chain_obj(ex, obj, exit, undef))
          emit_member_get(o, prop)
        }
        ast.CallExpression(_, callee, args) ->
          case callee {
            ast.MemberExpression(_, ast.SuperExpression(_), _) -> {
              use #(f, this) <- anf.then(emit_chain_callee(callee, exit, undef))
              use args_l <- anf.then(emit_args_list(args))
              emit_call(f, this, args_l)
            }
            ast.MemberExpression(_, obj, prop)
            | ast.OptionalMemberExpression(_, obj, prop) -> {
              use o <- anf.then(chain_obj(callee, obj, exit, undef))
              emit_member_call(o, prop, args)
            }
            _ -> {
              use #(f, this) <- anf.then(emit_chain_callee(callee, exit, undef))
              use args_l <- anf.then(emit_args_list(args))
              emit_call(f, this, args_l)
            }
          }
        ast.OptionalCallExpression(_, callee, args) -> {
          use #(f, this) <- anf.then(emit_chain_callee(callee, exit, undef))
          use f <- anf.then(chain_guard(f, exit, undef))
          use args_l <- anf.then(emit_args_list(args))
          emit_call(f, this, args_l)
        }
        _ -> expr(ex)
      }
  }
}

fn chain_guard(v: ir.Value, exit: String, undef: ir.Value) -> Build(ir.Value) {
  use is_nul <- anf.then(anf.host_bool("is_nullish", [v]))
  anf.bind_if(is_nul, fn(e, _k) { #(ir.Break(exit, [undef]), e) }, anf.pure(v))
}

fn chain_obj(
  link: ast.Expression,
  obj: ast.Expression,
  exit: String,
  undef: ir.Value,
) -> Build(ir.Value) {
  use o <- anf.then(emit_chain(obj, exit, undef))
  case link {
    ast.OptionalMemberExpression(..) -> chain_guard(o, exit, undef)
    _ -> anf.pure(o)
  }
}

fn emit_chain_callee(
  callee: ast.Expression,
  exit: String,
  undef: ir.Value,
) -> Build(#(ir.Value, ir.Value)) {
  case callee {
    ast.MemberExpression(_, ast.SuperExpression(_), prop) -> {
      use f <- anf.then(emit_super_get(prop))
      use this <- anf.then(emit_lexical(lexical.RefThis))
      anf.pure(#(f, this))
    }
    ast.MemberExpression(_, obj, prop)
    | ast.OptionalMemberExpression(_, obj, prop) -> {
      use o <- anf.then(chain_obj(callee, obj, exit, undef))
      use f <- anf.then(emit_member_get(o, prop))
      anf.pure(#(f, o))
    }
    _ -> {
      use f <- anf.then(emit_chain(callee, exit, undef))
      anf.pure(#(f, undef))
    }
  }
}

// §13.5.3 typeof unresolvable is "undefined", never throws
fn emit_typeof_ident(name: String) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case state.resolve(e, name) {
    scope.Plain(scope.Local(..) as d) -> {
      use v <- anf.then(emit_direct_get(d, name))
      anf.host("type_of", [v])
    }
    scope.Plain(scope.Global(name: g)) ->
      anf.host("global_typeof", [ir.ConstBinary(bit_array.from_string(g))])
    scope.Plain(scope.EvalEnv(..)) ->
      throw_at_rt("throw_type_error", "unsupported: direct eval")
    scope.WithChain(..) -> throw_at_rt("throw_type_error", "unsupported: with")
  }
}

// §13.5.1.2 delete name, sloppy
fn emit_delete_ident(name: String) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case state.resolve(e, name) {
    scope.Plain(scope.Local(..)) -> anf.pure(e.consts.false_)
    scope.Plain(scope.Global(name: g)) ->
      anf.host("global_delete", [ir.ConstBinary(bit_array.from_string(g))])
    scope.Plain(scope.EvalEnv(..)) ->
      throw_at_rt("throw_type_error", "unsupported: direct eval")
    scope.WithChain(..) -> throw_at_rt("throw_type_error", "unsupported: with")
  }
}

fn emit_delete(arg: ast.Expression) -> Build(ir.Value) {
  use rc <- anf.then(consts())
  case ast_util.unwrap_parens(arg) {
    // delete super.x throws after evaluating this and key
    ast.MemberExpression(_, ast.SuperExpression(_), property) -> {
      use _ <- anf.then(emit_lexical(lexical.RefThis))
      use _ <- anf.then(case property {
        ast.Bracket(expression:) -> expr(expression)
        ast.Dot(..) -> anf.pure(rc.undef)
      })
      throw_at_rt("throw_reference_error", "Unsupported reference to 'super'")
    }
    ast.MemberExpression(_, obj, prop) -> {
      use ov <- anf.then(expr(obj))
      use k <- anf.then(case prop {
        ast.Bracket(expression:) -> {
          use v <- anf.then(expr(expression))
          to_property_key_of(ov, v)
        }
        _ -> emit_key_from_prop(prop)
      })
      use op <- anf.then(delete_prop_op())
      anf.host(op, [ov, k])
    }
    ast.Identifier(name:, ..) -> emit_delete_ident(name)
    other -> {
      use _ <- anf.then(expr(other))
      anf.pure(rc.true_)
    }
  }
}

fn emit_unary(op: ast.UnaryOp, arg: ast.Expression) -> Build(ir.Value) {
  case op {
    ast.TypeOf ->
      case ast_util.unwrap_parens(arg) {
        ast.Identifier(name:, ..) -> emit_typeof_ident(name)
        inner -> {
          use v <- anf.then(expr(inner))
          anf.host("type_of", [v])
        }
      }
    ast.Delete -> emit_delete(arg)
    ast.Void -> {
      use _ <- anf.then(expr(arg))
      use rc <- anf.then(consts())
      anf.pure(rc.undef)
    }
    ast.LogicalNot -> {
      use c <- anf.then(cond_i32(arg))
      use rc <- anf.then(consts())
      anf.bind_if(c, anf.pure(rc.false_), anf.pure(rc.true_))
    }
    ast.Negate ->
      case ast_util.unwrap_parens(arg) {
        ast.NumberLiteral(_, ast.FiniteNumber(f)) ->
          number_literal(ast.FiniteNumber(float.negate(f)))
        _ -> anf.then(expr(arg), anf.guarded_neg)
      }
    ast.UnaryPlus -> anf.then(expr(arg), fn(v) { anf.host("plus", [v]) })
    ast.BitwiseNot ->
      anf.then(expr(arg), fn(v) {
        use r <- anf.then(anf.host("bitnot_fast", [v]))
        use is_miss <- anf.then(anf.bind(ir.TermTest(ir.IsAtom, r)))
        anf.bind_if(is_miss, anf.host("bitnot", [v]), anf.pure(r))
      })
  }
}

fn emit_apply_raw_general(
  inner: ast.Expression,
  recv_arg: ast.Expression,
  raw_args: ir.Value,
) -> Build(ir.Value) {
  use f <- anf.then(expr(inner))
  use recv <- anf.then(expr(recv_arg))
  emit_call(f, recv, raw_args)
}

fn emit_apply_arguments(
  inner: ast.Expression,
  recv_arg: ast.Expression,
  raw_args: ir.Value,
) -> Build(ir.Value) {
  case ast_util.unwrap_parens(inner), ast_util.unwrap_parens(recv_arg) {
    ast.MemberExpression(_, ast.ThisExpression(_), mprop), ast.ThisExpression(_)
    ->
      case static_dot_key(mprop) {
        Some(kb) -> {
          use this <- anf.then(emit_lexical(lexical.RefThis))
          call_method_ic(this, kb, raw_args)
        }
        None -> emit_apply_raw_general(inner, recv_arg, raw_args)
      }
    _, _ -> emit_apply_raw_general(inner, recv_arg, raw_args)
  }
}

fn emit_plain_call(ex: ast.Expression) -> Build(ir.Value) {
  let assert ast.CallExpression(_, callee, args) = ex
  case callee {
    ast.SuperExpression(_) -> emit_super_call(args)
    ast.MemberExpression(_, ast.SuperExpression(_), prop) -> {
      use f <- anf.then(emit_super_get(prop))
      use this <- anf.then(emit_lexical(lexical.RefThis))
      use args_l <- anf.then(emit_args_list(args))
      emit_call(f, this, args_l)
    }
    // only when arguments is the implicit binding, not a shadow
    ast.MemberExpression(_, inner, ast.Dot(name: "apply", ..) as prop) -> {
      use e <- anf.then(ask)
      case args, e.raw_args_var, state.arguments_is_implicit(e) {
        [recv_arg, ast.Identifier(name: "arguments", ..)], Some(raw), True ->
          emit_apply_arguments(inner, recv_arg, ir.Var(raw))
        _, _, _ -> {
          use o <- anf.then(expr(inner))
          emit_member_call(o, prop, args)
        }
      }
    }
    ast.MemberExpression(_, obj, prop) ->
      case math_direct_op(obj, prop, args) {
        Some(op) -> {
          use e <- anf.then(ask)
          // only when Math is the untouched global
          case
            state.resolve(e, "Math"),
            state.lookup_slotted_global(e, "Math")
          {
            scope.Plain(scope.Global(_)), None -> {
              use pos <- anf.then(anf.seq(list.map(args, expr)))
              use v <- anf.then(anf.host(op, pos))
              // compare against miss atom, js_nan/js_inf are atoms too
              use is_miss <- anf.then(
                anf.bind(ir.NumTerm(ir.NEq, v, ir.ConstAtom("miss"))),
              )
              anf.bind_if(
                is_miss,
                {
                  // t_plus returns wire jsval, t_to_number does not
                  use coerced <- anf.then(
                    anf.seq(list.map(pos, fn(a) { anf.host("plus", [a]) })),
                  )
                  anf.host(op, coerced)
                },
                anf.pure(v),
              )
            }
            _, _ -> {
              use o <- anf.then(expr(obj))
              emit_member_call(o, prop, args)
            }
          }
        }
        None -> {
          use o <- anf.then(expr(obj))
          emit_member_call(o, prop, args)
        }
      }
    // §13.3.6.1 callee and args evaluate before the eval throw
    ast.Identifier(name: "eval", ..) -> {
      use _ <- anf.then(expr(callee))
      use _ <- anf.then(emit_args_list(args))
      throw_at_rt("throw_type_error", "unsupported: direct eval")
    }
    _ ->
      case ast_util.unwrap_parens(callee) {
        ast.FunctionExpression(
          name: None,
          params:,
          body:,
          is_generator: False,
          is_async: False,
          ..,
        ) ->
          emit_iife(
            callee,
            state.FnExpr(self_name: None, is_gen: False, is_async: False),
            params,
            state.StmtBody(body),
            args,
          )
        ast.ArrowFunctionExpression(params:, body:, is_async: False, ..) ->
          emit_iife(
            callee,
            state.Arrow(is_async: False),
            params,
            case body {
              ast.ArrowBodyBlock(stmts) -> state.StmtBody(stmts)
              ast.ArrowBodyExpression(x) -> state.ExprBody(x)
            },
            args,
          )
        _ -> emit_generic_call(callee, args)
      }
  }
}

// slotted globals keyed -1 - slot
fn emit_generic_call(
  callee: ast.Expression,
  args: List(ast.Expression),
) -> Build(ir.Value) {
  {
    use rc <- anf.then(consts())
    use e <- anf.then(ask)
    let hoisted = case ast_util.unwrap_parens(callee) {
      ast.Identifier(name:, ..) ->
        case state.resolve(e, name) {
          scope.Plain(scope.Local(slot:, boxed: False, ..)) ->
            state.lookup_hoisted_kfn(e, slot)
          scope.Plain(scope.Local(slot:, boxed: True, ..)) ->
            state.lookup_hoisted_kfn(e, -1 - slot)
          _ -> None
        }
      _ -> None
    }
    use f <- anf.then(expr(callee))
    case ast_util.has_spread_arg(args) {
      False -> {
        use pos <- anf.then(anf.seq(list.map(args, expr)))
        case hoisted {
          Some(pair) -> emit_call_with_pair(pair, f, rc.undef, Positional(pos))
          None -> emit_call_pos(f, rc.undef, pos)
        }
      }
      True -> {
        use args_l <- anf.then(emit_args_list(args))
        case hoisted {
          Some(pair) -> emit_call_with_pair(pair, f, rc.undef, Consed(args_l))
          None -> emit_call(f, rc.undef, args_l)
        }
      }
    }
  }
}

fn emit_iife(
  callee: ast.Expression,
  shape: state.FnShape,
  params: List(ast.Pattern),
  body: state.FnBody,
  args: List(ast.Expression),
) -> Build(ir.Value) {
  use <- bool.lazy_guard(ast_util.has_spread_arg(args), fn() {
    emit_generic_call(callee, args)
  })
  use rc <- anf.then(consts())
  use site <- anf.then(fn(e: Emitter2, k) {
    let #(fn_id, e) = state.pop_child_fn(e)
    case e.dispatch.emit_function_site(e, shape, None, params, body, fn_id) {
      Ok(#(site, e)) -> k(e, site)
      Error(err) ->
        {
          use v <- anf.then(throw_at_rt("throw_type_error", describe_error(err)))
          anf.pure(state.ClosureSite(ir.Values([v])))
        }(e, k)
    }
  })
  case site {
    state.DirectFn(name:, captures:, arity:, needs_this:, strict:) -> {
      use pos <- anf.then(anf.seq(list.map(args, expr)))
      use this <- anf.then(case needs_this, strict {
        False, _ -> anf.pure([])
        True, True -> anf.pure([rc.undef])
        True, False -> anf.map(anf.host("global_this", []), fn(g) { [g] })
      })
      let passed = list.take(pos, arity)
      let pad = list.repeat(rc.undef, arity - list.length(passed))
      anf.bind(ir.CallDirect(name, list.flatten([captures, this, passed, pad])))
    }
    state.ClosureSite(tree) -> {
      use f <- anf.then(anf.bind(tree))
      use pos <- anf.then(anf.seq(list.map(args, expr)))
      emit_call_pos(f, rc.undef, pos)
    }
  }
}

// §13.15.2 base and key evaluate once, in order, before rhs

fn write_slot(slot: Int, boxed: Bool, v: ir.Value) -> Build(ir.Value) {
  fn(e: Emitter2, k) {
    case boxed {
      True ->
        anf.then(
          anf.host("cell_set", [ir.Var(state.get_slot_var(e, slot)), v]),
          fn(_) { anf.pure(v) },
        )(e, k)
      False -> {
        let #(name, e) = state.fresh_slot_var(e, slot)
        let e = case anf.is_known_number(e, v) {
          True -> state.mark_known_number(e, name)
          False -> e
        }
        anf.wrap(k(state.set_slot_var(e, slot, name), v), ir.Let(
          [name],
          ir.Values([v]),
          _,
        ))
      }
    }
  }
}

// §9.1.1.1.5 reads do not throw on tdz, check before store
fn write_slot_checked(
  slot: Int,
  boxed: Bool,
  name: String,
  v: ir.Value,
) -> Build(ir.Value) {
  use cur <- anf.then(read_slot(slot, boxed))
  use _ <- anf.then(
    anf.host("tdz_check", [
      cur,
      ir.ConstBinary(bit_array.from_string(name)),
    ]),
  )
  write_slot(slot, boxed, v)
}

pub fn emit_direct_put(
  d: scope.Direct,
  name: String,
  v: ir.Value,
) -> Build(ir.Value) {
  case d {
    scope.Local(origin_kind: scope.ConstBinding, ..) ->
      throw_at_rt("throw_type_error", "Assignment to constant '" <> name <> "'")
    scope.Local(origin_kind: scope.FnNameBinding, ..) -> {
      use e <- anf.then(ask)
      case e.strict {
        True ->
          throw_at_rt(
            "throw_type_error",
            "Assignment to constant '" <> name <> "'",
          )
        False -> anf.pure(v)
      }
    }
    scope.Local(kind: scope.CaptureBinding, slot:, boxed:, ..) ->
      write_slot_checked(slot, boxed, name, v)
    scope.Local(kind: scope.LetBinding, slot:, boxed:, ..) -> {
      use e <- anf.then(ask)
      case set.contains(e.initialized, slot) {
        True -> write_slot(slot, boxed, v)
        False -> write_slot_checked(slot, boxed, name, v)
      }
    }
    scope.Local(slot:, boxed:, ..) -> write_slot(slot, boxed, v)
    scope.Global(_) -> {
      use e <- anf.then(ask)
      case dict.get(e.slotted_globals, name) {
        Ok(slot) -> write_slot(slot, True, v)
        Error(Nil) -> {
          use _ <- anf.then(
            anf.host(global_set_op(e.strict), [
              ir.ConstBinary(bit_array.from_string(name)),
              v,
            ]),
          )
          anf.pure(v)
        }
      }
    }
    scope.EvalEnv(_) ->
      throw_at_rt("throw_type_error", "unsupported: direct eval")
  }
}

// §6.2.5.6 unresolvable: strict throws, sloppy creates global
pub fn global_set_op(strict: Bool) -> String {
  case strict {
    True -> "global_set_strict"
    False -> "global_set"
  }
}

pub fn emit_identifier_put(name: String, v: ir.Value) -> Build(ir.Value) {
  use e <- anf.then(ask)
  case state.resolve(e, name) {
    scope.Plain(d) -> emit_direct_put(d, name, v)
    scope.WithChain(..) ->
      throw_at_rt("throw_type_error", "unsupported: with (" <> name <> ")")
  }
}

pub type LValue {
  LvIdent(name: String, direct: scope.Direct)
  LvMember(
    obj: ir.Value,
    key: ir.Value,
    is_private: Bool,
    own_key: Option(BitArray),
    elem_idx: Option(ir.Value),
  )
  LvSuper(home: ir.Value, this: ir.Value, key: ir.Value)
}

pub fn emit_lvalue(target: ast.Expression) -> Build(LValue) {
  case ast_util.unwrap_parens(target) {
    ast.Identifier(name:, ..) -> {
      use e <- anf.then(ask)
      case state.resolve(e, name) {
        scope.Plain(d) -> anf.pure(LvIdent(name, d))
        scope.WithChain(..) -> {
          use _ <- anf.then(throw_at_rt(
            "throw_type_error",
            "unsupported: with (" <> name <> ")",
          ))
          anf.pure(LvIdent(name, scope.Global(name)))
        }
      }
    }
    ast.MemberExpression(object: ast.SuperExpression(..), property:, ..) -> {
      use home <- anf.then(emit_lexical(lexical.RefHomeObject))
      use this <- anf.then(emit_lexical(lexical.RefThis))
      use key <- anf.then(emit_key_from_prop(property))
      anf.pure(LvSuper(home:, this:, key:))
    }
    ast.MemberExpression(object:, property:, ..) -> {
      use obj <- anf.then(expr(object))
      let own_key = static_dot_key(property)
      use #(key, elem_idx) <- anf.then(case own_key, property {
        Some(_), _ -> anf.pure(#(no_key, None))
        None, ast.Bracket(expression:) -> {
          use idx <- anf.then(expr(expression))
          anf.pure(#(no_key, Some(idx)))
        }
        None, _ -> {
          use k <- anf.then(emit_key_from_prop(property))
          anf.pure(#(k, None))
        }
      })
      anf.pure(LvMember(
        obj:,
        key:,
        is_private: is_private_prop(property),
        own_key:,
        elem_idx:,
      ))
    }
    _ -> {
      use _ <- anf.then(throw_at_rt(
        "throw_reference_error",
        "Invalid assignment target",
      ))
      anf.pure(LvIdent("", scope.Global("")))
    }
  }
}

const no_key: ir.Value = ir.ConstAtom("undefined")

// coerce bracket key once up front for read-modify-write
pub fn settle_lvalue(lv: LValue) -> Build(LValue) {
  case lv {
    LvMember(obj:, is_private: False, elem_idx: Some(idx), ..) -> {
      use key <- anf.then(to_property_key_of(obj, idx))
      anf.pure(LvMember(..lv, key:))
    }
    _ -> anf.pure(lv)
  }
}

fn elem_key(obj: ir.Value, idx: ir.Value, key: ir.Value) -> Build(ir.Value) {
  case key == no_key {
    True -> to_property_key_of(obj, idx)
    False -> anf.pure(key)
  }
}

pub fn lvalue_get(lv: LValue) -> Build(ir.Value) {
  case lv {
    LvIdent(name:, direct:) -> emit_direct_get(direct, name)
    LvMember(obj:, key:, is_private: True, ..) ->
      anf.host("private_get", [obj, key])
    LvMember(obj:, is_private: False, own_key: Some(kb), ..) ->
      get_prop_fast(obj, kb)
    LvMember(obj:, key:, is_private: False, elem_idx: Some(idx), ..) ->
      get_elem_fast(obj, idx, {
        use k <- anf.then(elem_key(obj, idx, key))
        anf.host("get_prop", [obj, k])
      })
    LvMember(obj:, key:, is_private: False, own_key: None, elem_idx: None) ->
      anf.host("get_prop", [obj, key])
    LvSuper(home:, this:, key:) -> anf.host("super_get", [home, this, key])
  }
}

pub fn lvalue_put(lv: LValue, v: ir.Value) -> Build(ir.Value) {
  case lv {
    LvIdent(name:, direct:) -> emit_direct_put(direct, name, v)
    LvMember(obj:, key:, is_private: True, ..) -> {
      use _ <- anf.then(anf.host("private_set", [obj, key, v]))
      anf.pure(v)
    }
    LvMember(obj:, is_private: False, own_key: Some(kb), ..) ->
      set_prop_fast(obj, kb, v)
    LvMember(obj:, key:, is_private: False, elem_idx: Some(idx), ..) ->
      set_elem_fast(obj, idx, v, {
        use k <- anf.then(elem_key(obj, idx, key))
        use op <- anf.then(set_prop_op())
        anf.host(op, [obj, k, v])
      })
    LvMember(obj:, key:, is_private: False, own_key: None, elem_idx: None) -> {
      use op <- anf.then(set_prop_op())
      use _ <- anf.then(anf.host(op, [obj, key, v]))
      anf.pure(v)
    }
    LvSuper(home:, this:, key:) -> {
      use e <- anf.then(ask)
      let strict = case e.strict {
        True -> e.consts.true_
        False -> e.consts.false_
      }
      use _ <- anf.then(anf.host("super_set", [home, this, key, v, strict]))
      anf.pure(v)
    }
  }
}

const js_hole: ir.Value = ir.ConstAtom("js_hole")

fn emit_array_no_spread(
  elements: List(Option(ast.Expression)),
) -> Build(ir.Value) {
  use vs <- anf.then(
    anf.seq(
      list.map(elements, fn(el) {
        case el {
          Some(e) -> expr(e)
          None -> anf.pure(js_hole)
        }
      }),
    ),
  )
  use l <- anf.then(anf.cons_list(vs))
  anf.host("new_array", [l])
}

fn emit_array_slow(elements: List(Option(ast.Expression))) -> Build(ir.Value) {
  use acc0 <- anf.then(anf.host("empty_list", []))
  use l <- anf.then(
    fold_build(elements, acc0, fn(acc, el) {
      case el {
        Some(ast.SpreadElement(_, arg)) -> {
          use it <- anf.then(expr(arg))
          anf.host("spread_into_list", [acc, it])
        }
        Some(e) -> {
          use v <- anf.then(expr(e))
          anf.host("list_append_one", [acc, v])
        }
        None -> anf.host("list_append_one", [acc, js_hole])
      }
    }),
  )
  anf.host("new_array", [l])
}

fn emit_object_property(obj: ir.Value, p: ast.Property) -> Build(ir.Value) {
  case p {
    // annex b __proto__: sets prototype, must precede init arm
    ast.InitProperty(
      key: ast.KeyIdentifier(name: "__proto__", ..),
      value:,
      shorthand: False,
    )
    | ast.InitProperty(
        key: ast.KeyString(value: "__proto__", ..),
        value:,
        shorthand: False,
      ) -> {
      use v <- anf.then(expr(value))
      use _ <- anf.then(anf.host("set_proto", [obj, v]))
      anf.pure(obj)
    }

    ast.InitProperty(key:, value:, shorthand: _) -> {
      use k <- anf.then(emit_key(key))
      use v <- anf.then(emit(value, ast.property_key_static_name(key)))
      use _ <- anf.then(anf.host("define_prop", [obj, k, v]))
      anf.pure(obj)
    }

    ast.MethodProperty(key:, value:) -> {
      use k <- anf.then(emit_key(key))
      use f <- anf.then(emit_method_closure(
        value,
        ast.property_key_static_name(key),
      ))
      use rc <- anf.then(consts())
      use _ <- anf.then(
        anf.host("define_method", [
          obj,
          k,
          f,
          ir.ConstAtom("m_i_method"),
          rc.true_,
        ]),
      )
      anf.pure(obj)
    }

    ast.AccessorProperty(key:, value:, kind:) -> {
      let #(prefix, tag) = accessor_kind(kind)
      let name =
        option.map(ast.property_key_static_name(key), fn(n) { prefix <> n })
      use k <- anf.then(emit_key(key))
      use f <- anf.then(emit_method_closure(value, name))
      use rc <- anf.then(consts())
      use _ <- anf.then(anf.host("define_method", [obj, k, f, tag, rc.true_]))
      anf.pure(obj)
    }

    ast.SpreadProperty(argument:) -> {
      use src <- anf.then(expr(argument))
      use _ <- anf.then(anf.host("copy_data_props", [obj, src]))
      anf.pure(obj)
    }
  }
}

fn accessor_kind(kind: ast.AccessorKind) -> #(String, ir.Value) {
  case kind {
    ast.GetAccessor -> #("get ", ir.ConstAtom("m_i_getter"))
    ast.SetAccessor -> #("set ", ir.ConstAtom("m_i_setter"))
  }
}

fn emit_method_closure(
  lit: ast.FunctionLiteral,
  name: Option(String),
) -> Build(ir.Value) {
  let ast.FunctionLiteral(_, params, body, is_gen, is_async) = lit
  fn(e: Emitter2, k) {
    let #(fn_scope, e) = state.pop_child_fn(e)
    bridge_expr(fn(e) {
      e.dispatch.emit_function(
        e,
        state.Method(is_gen:, is_async:),
        name,
        params,
        state.StmtBody(body),
        fn_scope,
      )
    })(e, k)
  }
}

fn fold_build(xs: List(a), acc: b, step: fn(b, a) -> Build(b)) -> Build(b) {
  case xs {
    [] -> anf.pure(acc)
    [x, ..rest] -> anf.then(step(acc, x), fold_build(rest, _, step))
  }
}

pub fn emit_function_expr(
  shape: state.FnShape,
  named: Option(String),
  params: List(ast.Pattern),
  body: state.FnBody,
) -> Build(ir.Value) {
  fn(e, k) {
    let #(fn_id, e) = state.pop_child_fn(e)
    bridge_expr(fn(e) {
      e.dispatch.emit_function(e, shape, named, params, body, fn_id)
    })(e, k)
  }
}

fn emit_object(
  properties: List(ast.Property),
  _named: Option(String),
) -> Build(ir.Value) {
  let #(lead, rest) = plain_members(properties, [], set.new())
  use obj <- anf.then(case lead {
    [] -> anf.host("new_object", [])
    _ -> {
      use vs <- anf.then(
        anf.seq(
          list.map(lead, fn(m) { emit(m.1, ast.property_key_static_name(m.0)) }),
        ),
      )
      use keys <- anf.then(
        anf.cons_list(
          list.map(lead, fn(m) { ir.ConstBinary(bit_array.from_string(m.2)) }),
        ),
      )
      use vals <- anf.then(anf.cons_list(vs))
      anf.host("new_object_props", [keys, vals])
    }
  })
  fold_build(rest, obj, emit_object_property)
}

// object unobservable until the literal completes
fn plain_members(
  ps: List(ast.Property),
  acc: List(#(ast.PropertyKey, ast.Expression, String)),
  seen: set.Set(String),
) -> #(List(#(ast.PropertyKey, ast.Expression, String)), List(ast.Property)) {
  let done = fn() { #(list.reverse(acc), ps) }
  case ps {
    [ast.InitProperty(key:, value:, ..), ..rest] ->
      case plain_member_name(key) {
        Some(name) ->
          case set.contains(seen, name) {
            True -> done()
            False ->
              plain_members(
                rest,
                [#(key, value, name), ..acc],
                set.insert(seen, name),
              )
          }
        None -> done()
      }
    _ -> done()
  }
}

fn plain_member_name(key: ast.PropertyKey) -> Option(String) {
  case key {
    ast.KeyIdentifier(name: "__proto__", ..)
    | ast.KeyString(value: "__proto__", ..) -> None
    // identifier names never start with a digit
    ast.KeyIdentifier(name:, ..) -> Some(name)
    ast.KeyString(value:, ..) ->
      case key.source_key(value) {
        key.SourceName(_) -> Some(value)
        key.SourceIndex(_) -> None
      }
    _ -> None
  }
}

fn emit_array(elements: List(Option(ast.Expression))) -> Build(ir.Value) {
  case ast_util.has_spread_element(elements) {
    False -> emit_array_no_spread(elements)
    True -> emit_array_slow(elements)
  }
}

fn compound_binop(op: ast.AssignmentOp) -> Option(ast.BinaryOp) {
  case op {
    ast.AddAssign -> Some(ast.Add)
    ast.SubtractAssign -> Some(ast.Subtract)
    ast.MultiplyAssign -> Some(ast.Multiply)
    ast.DivideAssign -> Some(ast.Divide)
    ast.ModuloAssign -> Some(ast.Modulo)
    ast.ExponentiationAssign -> Some(ast.Exponentiation)
    ast.LeftShiftAssign -> Some(ast.LeftShift)
    ast.RightShiftAssign -> Some(ast.RightShift)
    ast.UnsignedRightShiftAssign -> Some(ast.UnsignedRightShift)
    ast.BitwiseAndAssign -> Some(ast.BitwiseAnd)
    ast.BitwiseOrAssign -> Some(ast.BitwiseOr)
    ast.BitwiseXorAssign -> Some(ast.BitwiseXor)
    ast.Assign
    | ast.LogicalAndAssign
    | ast.LogicalOrAssign
    | ast.NullishCoalesceAssign -> None
  }
}

// §13.4 tonumeric on the old value
fn emit_update(
  op: ast.UpdateOp,
  prefix: Bool,
  target: ast.Expression,
) -> Build(ir.Value) {
  case ast_util.unwrap_parens(target) {
    // annex b: f()++ evaluates the call then throws
    ast.CallExpression(..) as call -> {
      use _ <- anf.then(expr(call))
      throw_at_rt(
        "throw_reference_error",
        "Invalid left-hand side expression in postfix operation",
      )
    }
    ast.Identifier(..) | ast.MemberExpression(..) -> {
      use lv <- anf.then(emit_lvalue(target))
      use lv <- anf.then(settle_lvalue(lv))
      use old <- anf.then(lvalue_get(lv))
      use one <- anf.then(number_literal(ast.FiniteNumber(1.0)))
      let #(fast_op, bop) = case op {
        ast.Increment -> #("num_add", ast.Add)
        ast.Decrement -> #("num_sub", ast.Subtract)
      }
      use e <- anf.then(ask)
      case anf.is_known_number(e, old) {
        True -> {
          use new <- anf.then(anf.num_binop(fast_op, old, one))
          use _ <- anf.then(lvalue_put(lv, new))
          case prefix {
            True -> anf.pure(new)
            False -> anf.pure(old)
          }
        }
        False -> {
          use is_num <- anf.then(anf.bind(ir.TermTest(ir.IsNumber, old)))
          use #(old_n, new) <- anf.then(anf.bind_if2(
            is_num,
            anf.map(anf.num_binop(fast_op, old, one), fn(new) { #(old, new) }),
            anf.then(anf.host("to_numeric", [old]), fn(old_n) {
              anf.map(binop(bop, old_n, one), fn(new) { #(old_n, new) })
            }),
          ))
          use _ <- anf.then(lvalue_put(lv, new))
          case prefix {
            True -> anf.pure(new)
            False -> anf.pure(old_n)
          }
        }
      }
    }
    _ -> unreachable("UpdateExpression on non-simple target")
  }
}

fn emit_logical_assign(
  logical: ast.LogicalOp,
  lv: LValue,
  right: ast.Expression,
  inferred: Option(String),
) -> Build(ir.Value) {
  use old <- anf.then(lvalue_get(lv))
  let choose = fn(taken: Build(ir.Value)) {
    case logical {
      ast.LogicalAnd -> anf.truthy_if(old, taken, anf.pure(old))
      ast.LogicalOr -> anf.truthy_if(old, anf.pure(old), taken)
      ast.NullishCoalescing -> anf.nullish_if(old, taken, anf.pure(old))
    }
  }
  case lv {
    // no in-arm write for unboxed locals, rebind once at the join
    // const/fnname puts throw so they must stay guarded
    LvIdent(_, scope.Local(boxed: False, origin_kind:, ..))
      if origin_kind != scope.ConstBinding && origin_kind != scope.FnNameBinding
    -> {
      use r <- anf.then(choose(emit(right, inferred)))
      lvalue_put(lv, r)
    }
    _ -> choose(anf.then(emit(right, inferred), lvalue_put(lv, _)))
  }
}

fn emit_assignment(
  op: ast.AssignmentOp,
  left: ast.Expression,
  right: ast.Expression,
) -> Build(ir.Value) {
  // §13.15.2 step 1.c named evaluation only for a bare identifier
  let inferred = case left {
    ast.Identifier(name: "*default*", ..) -> Some("default")
    ast.Identifier(name:, ..) -> Some(name)
    _ -> None
  }
  case ast_util.unwrap_parens(left) {
    // annex b: f() = v evaluates call, throws before rhs
    ast.CallExpression(..) as call -> {
      use _ <- anf.then(expr(call))
      throw_at_rt(
        "throw_reference_error",
        "Invalid left-hand side in assignment",
      )
    }
    ast.ArrayExpression(..) as pat | ast.ObjectExpression(..) as pat ->
      case op {
        ast.Assign -> {
          use rv <- anf.then(expr(right))
          use _ <- anf.then(emit_destructuring_assign(pat, rv))
          anf.pure(rv)
        }
        _ -> unreachable("compound-assign to destructuring pattern")
      }
    _ -> {
      use lv <- anf.then(emit_lvalue(left))
      use lv <- anf.then(case op {
        ast.Assign -> anf.pure(lv)
        _ -> settle_lvalue(lv)
      })
      case op {
        ast.Assign -> anf.then(emit(right, inferred), lvalue_put(lv, _))
        ast.LogicalAndAssign ->
          emit_logical_assign(ast.LogicalAnd, lv, right, inferred)
        ast.LogicalOrAssign ->
          emit_logical_assign(ast.LogicalOr, lv, right, inferred)
        ast.NullishCoalesceAssign ->
          emit_logical_assign(ast.NullishCoalescing, lv, right, inferred)
        _ ->
          case compound_binop(op) {
            Some(bop) -> {
              use old <- anf.then(lvalue_get(lv))
              use rv <- anf.then(expr(right))
              use nv <- anf.then(binop(bop, old, rv))
              lvalue_put(lv, nv)
            }
            None -> unreachable("compound-assign operator fallthrough")
          }
      }
    }
  }
}

pub fn compound_to_binop(op: ast.AssignmentOp) -> Option(ast.BinaryOp) {
  compound_binop(op)
}

pub fn emit_destructuring_assign(
  target: ast.Expression,
  src: ir.Value,
) -> Build(Nil) {
  case ast_util.unwrap_parens(target) {
    ast.Identifier(name:, ..) ->
      anf.then(emit_identifier_put(name, src), fn(_) { anf.pure(Nil) })
    ast.AssignmentExpression(_, ast.Assign, inner_left, default_expr) -> {
      use rc <- anf.then(consts())
      // gate on raw left, parens defeat isidentifierref
      let named = case inner_left {
        ast.Identifier(name:, ..) -> Some(name)
        _ -> None
      }
      use is_undef <- anf.then(anf.bind(ir.NumTerm(ir.NEq, src, rc.undef)))
      use v <- anf.then(anf.bind_if(
        is_undef,
        emit(default_expr, named),
        anf.pure(src),
      ))
      emit_destructuring_assign(inner_left, v)
    }
    ast.ArrayExpression(_, elements) -> {
      use rc <- anf.then(consts())
      use iter <- anf.then(
        anf.host("get_iterator", [src, ir.ConstAtom("sync")]),
      )
      // §13.15.5.3 step 6 close iterator unless drained
      let drained = array_assign_drains(elements)
      use _ <- anf.then(
        anf.close_iter_on_throw(iter, {
          use _ <- anf.then(emit_array_assign_elements(elements, iter))
          anf.pure(Nil)
        }),
      )
      case drained {
        True -> anf.pure(Nil)
        False -> anf.host_unit("iter_close", [iter, rc.false_])
      }
    }
    ast.ObjectExpression(_, properties) -> {
      // §13.15.5.2 step 1 requireobjectcoercible first
      use _ <- anf.then(anf.host("require_object_coercible", [src]))
      emit_object_assign_props(properties, src, [])
    }
    ast.MemberExpression(..) as m -> {
      use lv <- anf.then(emit_lvalue(m))
      anf.then(lvalue_put(lv, src), fn(_) { anf.pure(Nil) })
    }
    // annex b: for (f() of it) calls then throws
    ast.CallExpression(..) as call -> {
      use _ <- anf.then(expr(call))
      use _ <- anf.then(throw_at_rt(
        "throw_reference_error",
        "Invalid left-hand side in assignment",
      ))
      anf.pure(Nil)
    }
    _ -> {
      use _ <- anf.then(throw_at_rt(
        "throw_syntax_error",
        "Invalid destructuring assignment target",
      ))
      anf.pure(Nil)
    }
  }
}

fn is_member_target(target: ast.Expression) -> Bool {
  case ast_util.unwrap_parens(target) {
    ast.MemberExpression(object: ast.SuperExpression(..), ..) -> False
    ast.MemberExpression(..) -> True
    _ -> False
  }
}

fn iter_next_value(iter: ir.Value) -> Build(ir.Value) {
  use pair <- anf.then(anf.host("iter_next", [iter]))
  anf.bind(anf.tuple_get(pair, 1))
}

fn array_assign_drains(elements: List(Option(ast.Expression))) -> Bool {
  list.any(elements, fn(el) {
    case el {
      Some(ast.SpreadElement(..)) -> True
      _ -> False
    }
  })
}

fn emit_array_assign_elements(
  elements: List(Option(ast.Expression)),
  iter: ir.Value,
) -> Build(Nil) {
  case elements {
    [] -> anf.pure(Nil)
    [Some(ast.SpreadElement(_, argument)), ..] ->
      case is_member_target(argument) {
        // rest member lref evaluated before draining
        True -> {
          use lv <- anf.then(emit_lvalue(argument))
          use rest <- anf.then(anf.host("iter_rest", [iter]))
          anf.then(lvalue_put(lv, rest), fn(_) { anf.pure(Nil) })
        }
        False -> {
          use rest <- anf.then(anf.host("iter_rest", [iter]))
          emit_destructuring_assign(argument, rest)
        }
      }
    [None, ..tail] -> {
      use _ <- anf.then(anf.host("iter_next", [iter]))
      emit_array_assign_elements(tail, iter)
    }
    [Some(el), ..tail] ->
      case is_member_target(el) {
        // member lref evaluated before iteratorstep
        True -> {
          use lv <- anf.then(emit_lvalue(el))
          use v <- anf.then(iter_next_value(iter))
          use _ <- anf.then(lvalue_put(lv, v))
          emit_array_assign_elements(tail, iter)
        }
        False -> {
          use v <- anf.then(iter_next_value(iter))
          use _ <- anf.then(emit_destructuring_assign(el, v))
          emit_array_assign_elements(tail, iter)
        }
      }
  }
}

fn emit_object_assign_props(
  props: List(ast.Property),
  src: ir.Value,
  seen: List(ir.Value),
) -> Build(Nil) {
  case props {
    [] -> anf.pure(Nil)
    [ast.SpreadProperty(argument), ..] -> {
      use excl <- anf.then(anf.cons_list(list.reverse(seen)))
      use rest <- anf.then(anf.host("object_rest", [src, excl]))
      emit_destructuring_assign(argument, rest)
    }
    [ast.InitProperty(key:, value:, ..), ..tail] -> {
      use k <- anf.then(emit_key(key))
      case is_member_target(value) {
        // §13.15.5.6 step 1a lref before getv
        True -> {
          use lv <- anf.then(emit_lvalue(value))
          use v <- anf.then(anf.host("get_prop", [src, k]))
          use _ <- anf.then(lvalue_put(lv, v))
          emit_object_assign_props(tail, src, [k, ..seen])
        }
        False -> {
          use v <- anf.then(anf.host("get_prop", [src, k]))
          use _ <- anf.then(emit_destructuring_assign(value, v))
          emit_object_assign_props(tail, src, [k, ..seen])
        }
      }
    }
    [ast.MethodProperty(..), ..] | [ast.AccessorProperty(..), ..] -> {
      use _ <- anf.then(throw_at_rt(
        "throw_syntax_error",
        "Invalid destructuring assignment target",
      ))
      anf.pure(Nil)
    }
  }
}
