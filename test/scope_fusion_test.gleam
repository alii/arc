//// Regression tests pinning the parser↔scope fusion edge cases.
////
//// These guard the invariants the fused parse-time ScopeBuilder must
//// preserve vs the legacy two-pass `scope.analyze`: every test passes
//// against BOTH implementations, so a fusion regression shows as a
//// red diff here rather than a deep emit/cursor desync.
////
//// Covered (research C7 + open-questions 4-6 + design assumptions):
////   - C7   var-hoist vs enclosing-block lexical (`{ {var x} let x }`)
////   - OQ6  arrow-param speculation discards phantom builder state
////   - C6   block-scope elision reparents children to grandparent
////   -      switch hoist-order: fn-decl child precedes source-earlier
////          fn-expr in `children_at` so emit's positional cursor pops
////          the right FunctionInfo
////   -      class 7-step `fold_class_body` children_at order
////   -      template-literal substitution refs reach the capture pass
////   -      NFE self-name binding (FnNameBinding) declared + immutable
////   -      sloppy-mode Annex-B fn-in-block var promotion

import arc/compiler
import arc/parser
import arc/vm/builtins
import arc/vm/exec/entry
import arc/vm/heap
import arc/vm/value.{Finite, JsNumber, JsString}
import gleam/string

// ── helpers ────────────────────────────────────────────────────────────

fn run(source: String) -> Result(value.JsValue, String) {
  case parser.parse_script(source) {
    Error(err) -> Error("parse: " <> parser.parse_error_to_string(err))
    Ok(#(body, sb)) ->
      case compiler.compile(body, sb) {
        Error(ce) -> Error("compile: " <> string.inspect(ce))
        Ok(template) -> {
          let h = heap.new()
          let #(h, b) = builtins.init(h)
          let #(h, global_object) = builtins.globals(b, h)
          case entry.run(template, h, b, global_object) {
            Ok(#(Ok(v), _)) -> Ok(v)
            Ok(#(Error(v), _)) -> Error("threw: " <> string.inspect(v))
            Error(vm_err) -> Error("vm: " <> string.inspect(vm_err))
          }
        }
      }
  }
}

fn expect(source: String, want: value.JsValue) -> Nil {
  case run(source) {
    Ok(got) ->
      case got == want {
        True -> Nil
        False ->
          panic as {
            "scope_fusion: "
            <> source
            <> "\n  want "
            <> string.inspect(want)
            <> "\n  got  "
            <> string.inspect(got)
          }
      }
    Error(why) ->
      panic as { "scope_fusion: " <> source <> "\n  failed: " <> why }
  }
}

fn n(f: Float) -> value.JsValue {
  JsNumber(Finite(f))
}

fn parses(source: String) -> Bool {
  case parser.parse(source, parser.Script) {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn rejects(source: String) -> Bool {
  !parses(source)
}

// ── C7: `{ {var x} let x }` ───────────────────────────────────────────
//
// Inner `var x` hoists THROUGH both blocks to the enclosing var scope;
// the outer block's LexicallyDeclaredNames {x} therefore intersects its
// VarDeclaredNames {x} → early SyntaxError (§14.2.1). The legacy parser
// Sets dropped the inner var on `restore_block_scope` and relied on the
// post-parse DECLARE walk to catch this; the fused builder records the
// var into `sb.scopes[current_fn].bindings` where it survives the inner
// block pop, so the lexical-conflict check at `let x` sees it directly.

pub fn fusion_c7_var_hoist_then_outer_let_test() {
  // inner-var-first ordering — the case the old Set model lost
  assert rejects("{ { var x; } let x; }")
  assert rejects("{ { { var x; } } let x; }")
  assert rejects("function f(){ { { var x; } let x; } }")
  // and the let-first mirror (already covered in scope_test, kept here
  // so both orderings live next to each other for the fusion oracle)
  assert rejects("{ let x; { var x; } }")
  // var hoists past intervening lexical-free blocks but is STOPPED by a
  // function boundary — the conflict scope is the var scope, not global
  assert parses("(function(){ { var x; } })(); let x;")
  // catch-param is a lexical in its own scope; var hoisting THROUGH a
  // catch with the same name is the Annex-B carve-out, but a sibling
  // `let` in the catch's parent block still conflicts with the hoisted var
  assert rejects("{ try {} catch (e) { var x; } let x; }")
}

// ── OQ6: arrow speculation discards phantom builder state ─────────────
//
// `try_paren_arrow` parses `(…)` as parameters on a CLONED `P`; on
// failure (no `=>` / body invalid) the clone — including its `sb` — is
// discarded and the original `P` resumes. A regression here would leak
// a ParamBinding for `x` into the outer scope and reject `let x` below.

pub fn fusion_arrow_backtrack_discards_state_test() {
  // bare paren-ident — speculated as single arrow param, then backtracked
  assert parses("(x); let x = 1;")
  // comma sequence — speculated as multi-param list, then backtracked
  assert parses("(x, y); let x = 1; let y = 2;")
  // pattern-shaped LHS that turns out to be a destructuring ASSIGNMENT,
  // not arrow params — phantom Param/Let bindings must not survive
  assert parses("let a = [0]; ([a[0]] = [1]); let z = 2;")
  // nested: inner arrow SUCCEEDS, outer backtracks — inner's param `p`
  // belongs to the inner fn scope only and must not leak to outer
  assert parses("((p => p), 1); let p = 2;")
  // runtime: backtracked `(x)` is a plain reference, not a binding;
  // later `let x` is the only declaration and resolves normally
  expect("let r = (function(){ (x); var x = 5; return x; })(); r", n(5.0))
  // runtime: a SUCCESSFUL arrow's params live in the arrow scope and do
  // not collide with a same-named outer `let`
  expect("let f = (x) => x * 2; let x = 10; f(3) + x", n(16.0))
}

// ── C6: empty-block elision reparents children ────────────────────────
//
// `sb_prune_empty_block` deletes a Block scope with no bindings and
// reparents its `children_at` entries to the grandparent so emit's
// `ast_util.block_has_declarations` elision and the scope tree's
// `children_at` stay in lockstep. If reparenting is dropped the inner
// closure's scope id is orphaned and emit's `child_fn_cursor` desyncs.

pub fn fusion_block_elision_reparents_children_test() {
  // closure inside a declaration-free block captures across the elided
  // scope to the grandparent's `let`
  expect(
    "function outer(){ let v = 41; { return (function(){ return v + 1; })(); } } outer()",
    n(42.0),
  )
  // two levels of elided blocks — children must reparent transitively
  expect("function o(){ let v = 7; { { return (() => v)(); } } } o()", n(7.0))
  // elided block BETWEEN two non-elided siblings — middle child's fn
  // scope must land in the parent's children_at at the correct position
  expect(
    "function o(){ let r = 0;"
      <> " { let a = 1; r += (() => a)(); }"
      <> " { r += (() => 10)(); }"
      <> " { let b = 100; r += (() => b)(); }"
      <> " return r; } o()",
    n(111.0),
  )
  // a block that LOOKS empty at `{` but gains a binding mid-body must
  // NOT be pruned — the closure captures the block-local, not outer
  expect("let v = 1; let r; { r = () => v; let v = 2; } r()", n(2.0))
}

// ── switch: fn-decl hoist precedes source-earlier expr children ───────
//
// All cases share ONE Block scope. `declare_stmts_hoist_order` visits
// FunctionDeclaration children BEFORE other statements so emit's
// `child_fn_cursor` pops the decl's FunctionInfo first regardless of
// which case it textually sits in. A source-order children_at would
// hand the case-0 fn-expr the decl's scope (and vice-versa) and the
// `super`/closure reads below would resolve against the wrong frame.

pub fn fusion_switch_hoist_order_across_cases_test() {
  // fn decl in a LATER case is callable from an EARLIER case body
  expect(
    "function t(){ switch (1) {"
      <> " case 1: return g();"
      <> " case 2: function g(){ return 99; }"
      <> " } } t()",
    n(99.0),
  )
  // source-earlier fn EXPRESSION + source-later fn DECLARATION in the
  // same switch scope — decl must be the FIRST children_at entry. Each
  // closure captures a distinct outer `let`; a swap returns 3 not 30.
  expect(
    "function t(){ let a = 10, b = 3, e, r;"
      <> " switch (0) {"
      <> "  case 0: e = function(){ return b; };"
      <> "  default: function d(){ return a; } r = d() * e();"
      <> " } return r; } t()",
    n(30.0),
  )
  // lexical scope is shared across cases — decl in case 0 visible in
  // fall-through case 1
  expect(
    "function t(){ switch (0) {"
      <> " case 0: let k = 5;"
      <> " case 1: return k * 2;"
      <> " } } t()",
    n(10.0),
  )
}

// ── class: 7-step fold_class_body children_at order ──────────────────
//
// emit pops child fn scopes in the order: (1) instance-init fn,
// (2) constructor, (3) heritage expr, (4) computed keys, (5) instance
// methods, (6) static methods, (7) static-init fn. The fused parser
// encounters these in SOURCE order and must reorder at `}`; a desync
// makes `super`/`this` in any element read the wrong FunctionInfo's
// `lexical` table — the canonical symptom (see fold_class_body doc) is
// `super.x` returning `undefined`.

pub fn fusion_class_seven_step_children_order_test() {
  // every step present: heritage, computed key, instance field,
  // explicit ctor, instance method using `super`, static method,
  // static field. If children_at desyncs, `super.tag()` reads the
  // wrong home-object slot and this returns the wrong string.
  expect(
    "let key = 'name';"
      <> " class Base { tag(){ return 'B'; } }"
      <> " class C extends Base {"
      <> "   f = 1;"
      <> "   constructor(){ super(); this.g = 2; }"
      <> "   [key](){ return super.tag() + ':' + (this.f + this.g); }"
      <> "   static sm(){ return 10; }"
      <> "   static sf = C.sm() + 4;"
      <> " }"
      <> " new C().name() + '/' + C.sf",
    JsString("B:3/14"),
  )
  // static block (step-7 path) capturing the class-body inner const
  // binding — verifies the static-init fn child is last
  expect("class K { static v; static { K.v = 6 * 7; } } K.v", n(42.0))
  // heritage expression introduces a child fn scope (arrow) that must
  // sit at step-3 position, AFTER ctor, BEFORE computed keys
  expect(
    "let pick = 0;"
      <> " class A { id(){ return 'A'; } }"
      <> " class B { id(){ return 'B'; } }"
      <> " class C extends ((() => pick ? B : A)()) {"
      <> "   who(){ return super.id(); }"
      <> " }"
      <> " new C().who()",
    JsString("A"),
  )
}

// ── template-literal substitutions are recorded as refs ───────────────
//
// The fused REFERENCE recording fires from `parse_primary_expression`'s
// Identifier arm; template substitution exprs route through there too.
// Missing the ref means the capture pass never marks `x` captured and
// the inner closure reads an unboxed/dead slot.

pub fn fusion_template_substitution_refs_captured_test() {
  expect(
    "function o(){ let x = 'q'; return (function(){ return `[${x}]`; })(); } o()",
    JsString("[q]"),
  )
  // multiple substitutions, one of which is the ONLY reference to `y`
  expect(
    "function o(){ let x = 1, y = 2;"
      <> " let f = () => `${x}-${y}`;"
      <> " x = 9; return f(); } o()",
    JsString("9-2"),
  )
  // tagged template — the tag identifier AND the substitution ids must
  // both be recorded
  expect(
    "function o(){ let n = 3;"
      <> " function tag(s, a){ return s[0] + (a * 2); }"
      <> " return (() => tag`v=${n}`)(); } o()",
    JsString("v=6"),
  )
}

// ── NFE self-name binding ─────────────────────────────────────────────
//
// `(function f(){ … })` declares `f` as a FnNameBinding in the function
// body scope — readable inside, invisible outside, and write-immutable
// (sloppy: silent no-op; strict: TypeError). The fused parser must
// sb_declare this in `parse_function_params_and_body`, not rely on the
// post-parse walk.

pub fn fusion_nfe_self_name_binding_test() {
  // self-reference resolves to the function object
  expect("let g = function f(){ return typeof f; }; g()", JsString("function"))
  // does NOT leak to the enclosing scope
  expect("let g = function f(){ return 1; }; typeof f", JsString("undefined"))
  // recursion through the self-name (closure-captured across the inner
  // call frames — if FnNameBinding isn't declared the recursive `f`
  // call hits the global and throws ReferenceError)
  expect("(function f(n){ return n <= 1 ? 1 : n * f(n - 1); })(5)", n(120.0))
  // sloppy assignment to self-name is a silent no-op
  expect("(function f(){ f = 0; return typeof f; })()", JsString("function"))
  // self-name binding co-exists with a same-named outer let — outer is
  // shadowed inside, untouched outside
  expect(
    "let f = 7; let r = (function f(){ return typeof f; })(); r + ':' + f",
    JsString("function:7"),
  )
}

// ── sloppy Annex-B function-in-block var promotion ───────────────────
//
// §B.3.2: in sloppy mode a FunctionDeclaration directly inside a Block
// also gets a var-twin in the enclosing var scope, assigned when the
// block's binding is initialised. `annexb_candidates`/`annexb_blocked`
// gate this; the fused builder must populate them via
// `register_function_name`'s sloppy-block path.

pub fn fusion_annexb_sloppy_fn_in_block_test() {
  // var-twin reachable AFTER the block in sloppy mode
  expect(
    "function t(){ { function f(){ return 3; } } return f(); } t()",
    n(3.0),
  )
  // BEFORE the block executes the var-twin exists but is `undefined`
  // (it's a var, not the hoisted function object)
  expect(
    "function t(){ let r = typeof f; { function f(){} } return r; } t()",
    JsString("undefined"),
  )
  // STRICT mode: no Annex-B promotion — `f` is block-local only
  expect(
    "function t(){ 'use strict'; { function f(){ return 1; } } return typeof f; } t()",
    JsString("undefined"),
  )
  // promotion is BLOCKED when an outer lexical of the same name exists
  // (§B.3.2 step 1.a.ii.2) — the outer `let f` wins outside the block
  expect(
    "function t(){ let f = 1; { function f(){ return 2; } } return f; } t()",
    n(1.0),
  )
  // `if (cond) function f(){}` — the legacy one-armed-if Annex-B form;
  // parser must still register the candidate and not early-error
  assert parses("function t(){ if (true) function f(){} return f; }")
  expect(
    "function t(){ if (true) function f(){ return 8; } return f(); } t()",
    n(8.0),
  )
}

// ── OQ5: for(var) head does not desync block-scope cursor ─────────────
//
// `for(var …)` does NOT introduce a per-iteration Block scope (only
// `for(let/const …)` does). Open-question 5 flagged a possible
// emit/scope desync where emit unconditionally entered a scope but the
// declare pass conditionally created one. Whatever the resolution, the
// observable behaviour below must hold under fusion.

pub fn fusion_for_var_no_block_scope_test() {
  // a sibling block AFTER the for(var) must pop the correct scope id
  expect(
    "function f(){ for (var i = 0; i < 1; i++){} { let x = 11; return x; } } f()",
    n(11.0),
  )
  // closures over a for-var capture the SHARED var (last value), vs
  // for-let which captures per-iteration — fusion must preserve both
  expect(
    "function f(){ let a = []; for (var i = 0; i < 3; i++) a.push(() => i);"
      <> " return a[0]() + a[1]() + a[2](); } f()",
    n(9.0),
  )
  expect(
    "function f(){ let a = []; for (let i = 0; i < 3; i++) a.push(() => i);"
      <> " return a[0]() + a[1]() + a[2](); } f()",
    n(3.0),
  )
}

// ── §10.2.11 step 28: separate var environment for non-simple params ──
//
// A non-simple parameter list gives the BODY its own variable
// environment (a var-boundary Block under the Function scope), so a
// closure created inside a parameter initializer must never see body
// `var`s, and a body `var` that shadows a parameter name starts with the
// parameter's value (step 28.f.i.2), not undefined.

pub fn fusion_paramsbody_separate_var_env_test() {
  // THE test262 scope-paramsbody-var-open.js shape: the param-default
  // closure resolves `x` to the OUTER binding, the body closure to the
  // body's `var x`.
  expect(
    "var x = 'outside'; var pp, pb;"
      <> " (function(_ = pp = function(){ return x; }){"
      <> "   var x = 'inside'; pb = function(){ return x; };"
      <> " }());"
      <> " pp() + ' ' + pb()",
    JsString("outside inside"),
  )
  // step 28.f.i.2: body `var x` (x also a param) is initialized to the
  // param's value...
  expect("function f(x = 1){ var x; return x; } f()", n(1.0))
  expect("function f(x = 1){ var x = 2; return x; } f()", n(2.0))
  // ...but NOT when x is also a body-level FunctionDeclaration (the
  // hoisted closure wins), and not for free names captured by a
  // param-default closure (a CaptureBinding is not a parameter).
  expect(
    "function f(g = 1){ var r = typeof g; function g(){}; return r; } f()",
    JsString("function"),
  )
  expect(
    "var q = 'out'; function f(a = function(){ return q; }){ var q;"
      <> " return a() + ':' + q; } f()",
    JsString("out:undefined"),
  )
  // rest names and `arguments` are parameter bindings for the copy
  expect("function f(a = 1, ...r){ var r; return r.length; } f(1,2,3)", n(2.0))
  expect(
    "function f(a = 1){ var arguments; return arguments.length; } f(7,8)",
    n(2.0),
  )
  // arrows: same split, no `arguments` binding
  expect(
    "var y = 'out'; var p;"
      <> " ((q = (p = () => y)) => { var y = 'in'; return p(); })()",
    JsString("out"),
  )
  // hoisted body fn-decls live in the body env and still see params
  expect(
    "function f(a = 1){ return g(); function g(){ return a; } } f()",
    n(1.0),
  )
  // §15.2.1: a body-top-level lexical may not redeclare a formal —
  // but vars / nested blocks / `arguments` / the NFE self-name may.
  assert rejects("function f(a=1){ let a; }")
  assert rejects("function f(a=1){ const a = 0; }")
  assert rejects("function f(a=1){ class a {} }")
  assert rejects("function f([a]){ let a; }")
  assert rejects("(a=1) => { let a; };")
  assert parses("function f(a=1){ var a; }")
  assert parses("function f(a=1){ { let a; } }")
  assert parses("function f(a=1){ function a(){} }")
  assert parses("function f(a=1){ let arguments; }")
  assert parses("(function f(a=1){ let f; })")
}

// ── §14.15.2-3: the catch parameter env vs the catch body Block env ───
//
// `catch (param) Block` gives the param its own scope and the Block an
// ordinary child scope (previously merged into one Catch scope — which
// also desynced emit's scope cursor whenever the body contained nested
// declaration blocks, crashing the interpreter).

pub fn fusion_catch_param_body_env_split_test() {
  // closure in the catch-param destructuring default must not see the
  // body's lexicals
  expect(
    "var pp, pb; let x = 'outside';"
      <> " try { throw []; } catch ([_ = pp = function(){ return x; }]) {"
      <> "   pb = function(){ return x; }; let x = 'inside';"
      <> " }"
      <> " pp() + ' ' + pb()",
    JsString("outside inside"),
  )
  // nested declaration blocks inside a catch body (used to crash emit's
  // scope cursor: GetBoxed on a non-box local)
  expect(
    "var o = [];"
      <> " try { throw 1; } catch (e) { let x = e;"
      <> "   { let v = 'a'; o.push(function(){ return v; }); }"
      <> "   { let v = 'b'; o.push(function(){ return v; }); } }"
      <> " o[0]() + o[1]()",
    JsString("ab"),
  )
  // bindingless catch: no catch env at all; a sibling lexical block after
  // it must not be stolen by the catch (used to mis-seed its `let` to TDZ)
  expect(
    "var o = []; try { throw 1; } catch { o.push(1); }"
      <> " { let z = 'sib'; o.push(function(){ return z; }); }"
      <> " '' + o[0] + o[1]()",
    JsString("1sib"),
  )
  // §14.15.1 clause 2 survives the split (clause 3 / Annex B B.3.4: a
  // bare catch param IS var-redeclarable)
  assert rejects("try {} catch (e) { let e; }")
  assert rejects("try {} catch (e) { class e {} }")
  assert rejects("try {} catch (e) { function e(){} }")
  assert parses("try {} catch (e) { var e; }")
  assert parses("try {} catch (e) { { let e; } }")
}
