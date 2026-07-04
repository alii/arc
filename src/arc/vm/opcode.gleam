import arc/vm/binop.{type PureBinOp}
import arc/vm/key.{type PropertyKey}
import gleam/option.{type Option, None, Some}

// ============================================================================
// Lexical bindings — `this` / active-function (super) / new.target
// ============================================================================
//
// ES2024 §9.1.1.3: a non-arrow FunctionEnvironmentRecord holds four value
// slots that GetThisEnvironment() / GetSuperBase() expose: [[ThisValue]],
// [[FunctionObject]], [[HomeObject]], [[NewTarget]]. Arrows inherit all four
// from the nearest enclosing non-arrow. Arc models them as a 4-keyed family
// of pseudo-locals captured through the ordinary closure path — exactly
// QuickJS's resolve_pseudo_var quartet. [[HomeObject]] is sourced from the
// function object at call-setup (FunctionObject.home_object) and cached here
// per-frame so super-reference reads are pure local/capture loads.

/// One quasi (literal chunk) of a tagged template, as carried by
/// GetTemplateObject. `cooked` is the decoded template value (`None` when
/// the quasi contains an invalid escape sequence — its entry becomes
/// undefined, §12.9.6); `raw` is the verbatim source text. Mirrors the
/// parser's `ast.TemplateQuasi` so the VM layer stays independent of the
/// parser AST while a cooked/raw length mismatch stays unrepresentable.
pub type TemplateQuasi {
  TemplateQuasi(cooked: Option(String), raw: String)
}

/// Which lexical pseudo-binding a get_lexical / DeclareLexical refers to.
pub type LexicalRef {
  RefThis
  RefActiveFunc
  RefHomeObject
  RefNewTarget
}

/// Canonical iteration order. Compiler, resolver and runtime MUST agree on
/// this so slot indices line up — see frame.setup_frame.
pub const all_lexical_refs = [
  RefThis,
  RefActiveFunc,
  RefHomeObject,
  RefNewTarget,
]

/// Where a body's four lexical pseudo-bindings live in its locals frame.
///
/// A body either OWNS all four (non-arrow function / class static block /
/// non-eval script root: the call prologue seeds them at frame entry), or
/// owns none and reads whichever ones it references through captures
/// (arrows, direct-eval bodies), or has none at all (module root).
///
/// `OwnedLexicalSlots(base)` carries only the base index: the four owned
/// slots are ALWAYS contiguous, in `all_lexical_refs` order, starting at
/// `base`. That contiguity used to be an unwritten convention the emitter
/// upheld and `arc_vm_ffi:setup_locals_seeded/10` silently assumed; here it
/// is a fact of the type, so a mis-ordered or gappy owned layout is
/// unrepresentable.
pub type LexicalSlots {
  /// All four owned, contiguous, at `base`..`base + 3` in canonical order.
  OwnedLexicalSlots(base: Int)
  /// Some subset inherited from the enclosing non-arrow via captures; each
  /// `Some` is a slot in THIS frame holding the parent's box ref.
  CapturedLexicalSlots(
    this: Option(Int),
    active_func: Option(Int),
    home_object: Option(Int),
    new_target: Option(Int),
  )
  /// Neither owned nor captured (module root; a body that references none).
  NoLexicalSlots
}

/// Number of local slots an `OwnedLexicalSlots` body reserves.
pub const owned_lexical_slot_count = 4

/// Smart constructor for the captured case: all-None collapses to
/// `NoLexicalSlots` so "nothing here" has exactly one representation.
pub fn captured_lexical_slots(
  this this: Option(Int),
  active_func active_func: Option(Int),
  home_object home_object: Option(Int),
  new_target new_target: Option(Int),
) -> LexicalSlots {
  case this, active_func, home_object, new_target {
    None, None, None, None -> NoLexicalSlots
    _, _, _, _ ->
      CapturedLexicalSlots(this:, active_func:, home_object:, new_target:)
  }
}

pub fn lexical_slot(slots: LexicalSlots, ref: LexicalRef) -> Option(Int) {
  case slots {
    NoLexicalSlots -> None
    OwnedLexicalSlots(base) -> Some(base + lexical_ref_offset(ref))
    CapturedLexicalSlots(this:, active_func:, home_object:, new_target:) ->
      case ref {
        RefThis -> this
        RefActiveFunc -> active_func
        RefHomeObject -> home_object
        RefNewTarget -> new_target
      }
  }
}

/// Position of `ref` in the canonical `all_lexical_refs` order — the offset
/// from an `OwnedLexicalSlots` base.
pub fn lexical_ref_offset(ref: LexicalRef) -> Int {
  case ref {
    RefThis -> 0
    RefActiveFunc -> 1
    RefHomeObject -> 2
    RefNewTarget -> 3
  }
}

/// Per-binding "is referenced in this body or a nested arrow" flags.
/// Mirrors JSC's InnerArrowFunctionCodeFeatures bitmask.
pub type LexicalRefs {
  LexicalRefs(
    this: Bool,
    active_func: Bool,
    home_object: Bool,
    new_target: Bool,
  )
}

pub const no_lexical_refs = LexicalRefs(False, False, False, False)

pub fn lexical_refs_or(a: LexicalRefs, b: LexicalRefs) -> LexicalRefs {
  LexicalRefs(
    this: a.this || b.this,
    active_func: a.active_func || b.active_func,
    home_object: a.home_object || b.home_object,
    new_target: a.new_target || b.new_target,
  )
}

pub fn lexical_refs_get(refs: LexicalRefs, ref: LexicalRef) -> Bool {
  case ref {
    RefThis -> refs.this
    RefActiveFunc -> refs.active_func
    RefHomeObject -> refs.home_object
    RefNewTarget -> refs.new_target
  }
}

/// What KIND of code a body is. Stored on FuncTemplate; the syntax-legality
/// bits PerformEval (§19.2.1.1 steps 6-12) needs — may this body's direct
/// eval / nested arrow use `new.target`, `super.x`, `super()`, `arguments`?
/// — are DERIVED from it below, so the five legal combinations are the only
/// representable ones. (The old 4-bool `SyntaxPerms` record could spell 16,
/// including nonsense like "super() allowed but no [[HomeObject]]".)
/// Mirrors QuickJS's new_target_allowed/super_allowed/super_call_allowed/
/// arguments_allowed bits on JSFunctionBytecode.
pub type CodeKind {
  /// Top-level scripts/modules/indirect-eval: none of the function-only
  /// syntax forms are legal. `arguments` resolves as an ordinary (likely
  /// undefined) global, so it's "allowed" in the sense of not being a
  /// SyntaxError.
  ScriptCode
  /// Ordinary non-arrow function body: new.target and arguments allowed,
  /// super forms not (no [[HomeObject]]).
  FunctionCode
  /// Class/object-literal method body: super.prop allowed (has [[HomeObject]]).
  MethodCode
  /// Derived-class constructor body: super.prop and super() allowed.
  DerivedCtorCode
  /// Class instance-field initializer body (§15.7.14): runs as a synthetic
  /// method-like function. new.target is allowed (evaluates to undefined
  /// since the init fn is [[Call]]ed, not constructed); super.prop is allowed
  /// (init fn has [[HomeObject]] = class prototype); super() and arguments
  /// are not.
  FieldInitCode
}

pub fn new_target_allowed(kind: CodeKind) -> Bool {
  case kind {
    ScriptCode -> False
    FunctionCode | MethodCode | DerivedCtorCode | FieldInitCode -> True
  }
}

/// `super.x` requires a [[HomeObject]] — methods, derived constructors and
/// field initializers have one; plain functions and scripts do not.
pub fn super_prop_allowed(kind: CodeKind) -> Bool {
  case kind {
    ScriptCode | FunctionCode -> False
    MethodCode | DerivedCtorCode | FieldInitCode -> True
  }
}

/// `super()` is legal only in a derived-class constructor.
pub fn super_call_allowed(kind: CodeKind) -> Bool {
  case kind {
    DerivedCtorCode -> True
    ScriptCode | FunctionCode | MethodCode | FieldInitCode -> False
  }
}

pub fn arguments_allowed(kind: CodeKind) -> Bool {
  case kind {
    FieldInitCode -> False
    ScriptCode | FunctionCode | MethodCode | DerivedCtorCode -> True
  }
}

// ============================================================================
// Final Bytecode — resolved, ready for VM execution
// ============================================================================
//
// Static property-access ops carry a `key.PropertyKey`, precomputed by
// resolve.gleam via `key.canonical_key` so the interpreter never re-parses a
// compile-time-constant string per dispatch. There is exactly ONE
// canonicalizer (see arc/vm/key), so compile-time and runtime keys for the
// same string can never land in different dict slots.

/// Native error constructor selector for `ThrowError` — mirrors the kind
/// argument to QuickJS OP_throw_error. Extend as needed.
pub type ErrorKind {
  ReferenceErrorKind
  TypeErrorKind
}

/// What a `PushTry` handler frame is FOR. Carried by both `IrPushTry` and
/// `PushTry` (and mirrored onto `state.TryFrame` when the frame is pushed) so
/// that unwinding a *return* completion out of a suspended generator
/// (§27.5.3.4 GeneratorResumeAbrupt) can dispatch on a value the emitter
/// supplied instead of disassembling the instruction at `catch_target`.
///
/// Like every other label-carrying operand, `Finally`'s payload is a label id
/// in `IrPushTry` and an absolute PC after `resolve` rewrites it.
pub type TryKind {
  /// A plain `catch` handler, or an internal handler that only ever catches
  /// throws. A return completion unwinding past it just skips it.
  CatchOnly
  /// A `try`/`finally` — the frame's `catch_target` is the throw entry
  /// (`Gosub(fin_label); Throw`). A return completion must run the finally
  /// subroutine at `fin_label` before it keeps unwinding.
  Finally(fin_label: Int)
  /// A for-of loop / array-destructuring scaffold guarding a live iterator:
  /// the value at the frame's recorded `stack_depth` is the iterator, and a
  /// return completion must close it (§7.4.9 IteratorClose). Only the *sync*
  /// close paths use this — a `for await` guard is `CatchOnly` because
  /// AsyncIteratorClose needs an Await the unwinder cannot perform.
  IterCloseGuard
}

/// Resolved bytecode instruction. All variable references are numeric indices,
/// all jump targets are absolute PC addresses. The VM only sees these.
pub type Op {
  // -- Source mapping --
  /// Record the source line of the following instructions into
  /// `state.current_line`. Emitted by the compiler at the start of each
  /// statement whose line differs from the previous one. Used to build the
  /// line numbers in `Error.prototype.stack`. No stack effect; pc+1.
  SetLine(line: Int)

  // -- Literals + Stack --
  PushConst(index: Int)
  Pop
  Dup
  Swap
  /// [a, b, c, ..] → [c, a, b, ..] — bring the 3rd element to the top.
  /// QuickJS OP_rot3l. Used for destructuring-assignment stack choreography
  /// where a member target's base+key are evaluated before the iterator step.
  Rot3
  /// [a, b, c, d, ..] → [b, c, d, a, ..] — bury the top element under the
  /// next three. QuickJS OP_insert4 inverse-ish; pairs with Rot3 to restore
  /// the [iter, ..] loop invariant after PutElem operands are arranged.
  Unrot4

  // -- Variable Access (resolved) --
  GetLocal(index: Int)
  PutLocal(index: Int)
  /// §9.1.1.3.1 BindThisValue — write locals[index] only if currently
  /// JsUninitialized, else throw ReferenceError. Used for `super()` result.
  PutLocalCheckInit(index: Int)
  GetGlobal(name: String)
  PutGlobal(name: String)
  /// §9.1.1.4.7 DeleteBinding on the global Environment Record — the static
  /// fallback of a sloppy `delete identifier` that resolves to the global
  /// scope. Lexical (let/const) globals live in the declarative record and
  /// are never deletable, so they push false without touching the object
  /// record. Everything else is a REAL [[Delete]] on the global object:
  /// configurable properties (implicit `x = 1` globals) are removed and push
  /// true, non-configurable ones push false, and undeclared names fall
  /// through [[Delete]]'s missing-property case to true (spec step 4).
  /// Never emitted in strict code — `delete identifier` is an early
  /// SyntaxError there (§13.5.1.1).
  DeleteGlobalVar(name: String)
  /// Check state.eval_env dict for `name`; if present push its value, else
  /// fall through to GetGlobal semantics. Emitted in sloppy functions that
  /// contain a direct eval call — lets eval-created vars be read by name.
  GetEvalVar(name: String)
  /// Write to state.eval_env dict if `name` already exists there, else fall
  /// through to PutGlobal semantics. Emitted in sloppy direct-eval contexts.
  PutEvalVar(name: String)
  /// Seed `name` = undefined into state.eval_env dict (create key if absent).
  /// Emitted for `var` declarations at the top level of sloppy direct-eval
  /// code, in place of DeclareGlobalVar.
  DeclareEvalVar(name: String)
  /// `typeof name` — check eval_env first, fall through to TypeofGlobal.
  TypeofEvalVar(name: String)

  // -- `with` statement (§14.11) object-environment access --
  /// §7.1.18 ToObject on the stack top. TypeError on null/undefined.
  /// Emitted for the `with (expr)` head.
  ToObject
  /// §7.1.17 ToString on the stack top (ToPrimitive with string hint, so
  /// toString is tried before valueOf). Emitted for template literal
  /// substitutions (§13.2.8.5), which must not use the Add operator's
  /// default-hint coercion.
  ToStringVal
  /// §13.2.8.4 GetTemplateObject — push the cached template object for this
  /// tagged-template call site, creating + caching it on first evaluation.
  /// `site` is a globally unique id baked in at compile time, so each parse
  /// of the same source (e.g. repeated eval) gets distinct template objects
  /// while re-execution of the same compiled site reuses one. Each quasi
  /// pairs the template value (None → undefined for an invalid escape
  /// sequence) with its verbatim raw text.
  GetTemplateObject(site: Int, quasis: List(TemplateQuasi))
  /// §9.1.1.2.1 HasBinding + §9.1.1.2.6 GetBindingValue against the with
  /// object. Stack: [obj, ..] — if obj has `name` (and it is not blocked by
  /// @@unscopables), replace obj with Get(obj, name) and jump to `target`;
  /// otherwise pop obj and fall through. Mirrors QuickJS OP_with_get_var.
  WithGetVar(name: String, target: Int)
  /// Like WithGetVar, but KEEPS the with object beneath the value:
  /// [obj, ..] → [value, obj, ..] on hit. Used for `f()` where the callee
  /// identifier resolves through a with object — §13.3.6.2 EvaluateCall
  /// step 1.b.ii: thisValue is the env record's WithBaseObject(), i.e. the
  /// with object itself. The pair feeds CallMethod (this = receiver).
  /// Mirrors QuickJS OP_with_get_ref.
  WithGetVarThis(name: String, target: Int)
  /// §9.1.1.2.5 SetMutableBinding against the with object. Stack:
  /// [obj, value, ..] — if obj has `name` (unscopables-checked), perform
  /// Set(obj, name, value, false), pop both, jump to `target`; otherwise pop
  /// obj only and fall through. Mirrors QuickJS OP_with_put_var.
  WithPutVar(name: String, target: Int)
  /// §9.1.1.2.7 DeleteBinding against the with object. Stack: [obj, ..] — if
  /// obj has `name` (unscopables-checked), replace obj with the boolean
  /// result of [[Delete]] and jump to `target`; otherwise pop obj and fall
  /// through. Mirrors QuickJS OP_with_delete_var.
  WithDeleteVar(name: String, target: Int)
  /// §9.1.2.1 GetIdentifierReference at a with object: HasBinding
  /// (unscopables-checked). Stack: [obj, ..] — if bound, KEEP obj (it
  /// becomes the reference base) and jump to `target`; otherwise pop obj
  /// and fall through. Mirrors QuickJS OP_with_make_ref.
  WithMakeRef(name: String, target: Int)
  /// §9.1.1.2.6 GetBindingValue on a previously made reference base.
  /// Stack: [base, ..] — if base is an object, replace it with Get(base,
  /// name) (HasProperty re-check; sloppy reads undefined, strict throws
  /// ReferenceError when gone) and jump to `target`. If base is the
  /// undefined sentinel ("static binding"), pop it and fall through.
  WithGetRefValue(name: String, target: Int)
  /// §9.1.1.2.5 SetMutableBinding on a previously made reference base.
  /// Stack: [base, value, ..] — if base is an object, Set(base, name,
  /// value) (stillExists re-check, strict ReferenceError when gone), pop
  /// both, jump to `target`. If base is the undefined sentinel, pop it and
  /// fall through to the static store.
  WithPutRefValue(name: String, target: Int)

  // -- Property Access --
  GetField(key: PropertyKey)
  GetField2(key: PropertyKey)
  PutField(key: PropertyKey)
  GetElem
  GetElem2
  PutElem
  DeleteField(key: PropertyKey)
  DeleteElem
  // The four static private-element ops carry the RAW private name ("#x").
  // Private names live in their own PropertyKey variant (key.private_key)
  // and are never canonicalized to array indices — carrying a String makes a
  // private→public Index leak unrepresentable.
  /// §7.3.31 PrivateGet. Stack: [obj, ..] → [val, ..]. Throws TypeError if
  /// obj is not a JsObject or if obj lacks the private brand (has_property
  /// returns False). Brand check uses proto-chain walk so private methods
  /// stored on the prototype pass — pragmatic, not spec-pure PrivateName.
  GetPrivateField(name: String)
  /// Like GetPrivateField but keeps obj on stack: [obj, ..] → [val, obj, ..].
  /// Used for `obj.#m(args)` method calls (mirrors GetField2).
  GetPrivateField2(name: String)
  /// §7.3.32 PrivateSet. Stack: [val, obj, ..] → [val, ..]. Throws TypeError
  /// if obj is not a JsObject or lacks the brand.
  PutPrivateField(name: String)
  /// §13.10.1 `#x in obj`. Stack: [obj, ..] → [JsBool, ..]. Throws TypeError
  /// if obj is not a JsObject (step 2). Name is encoded in the opcode, not
  /// on the stack — unlike BinOp(In) which pops two operands.
  PrivateIn(name: String)

  // -- Spec-shaped PrivateName ops (per-class-evaluation unique names) --
  /// §15.7.14 ClassDefinitionEvaluation step 5/6: mint a fresh PrivateName for
  /// `name` ("#m"). Pushes a JsString carrying the unique storage-key text
  /// ("#m\u{0}<uid>" — see value.mint_private_key). The emitter binds it
  /// to a class-scope const named after the source text ("#m"), so nested
  /// classes shadow and closures capture exactly like the spec's
  /// PrivateEnvironment chain.
  NewPrivateName(name: String)
  /// §7.3.30 PrivateGet with the PrivateName on the stack.
  /// Stack: [key, obj, ..] → [val, ..]. TypeError if obj is not a JsObject,
  /// lacks the OWN private element (no prototype walk — spec
  /// [[PrivateElements]] are own-only), or the element is an accessor
  /// without a getter.
  GetPrivateFieldDyn
  /// Like GetPrivateFieldDyn but keeps obj: [key, obj, ..] → [val, obj, ..].
  /// Used for `obj.#m(args)` method calls (mirrors GetField2).
  GetPrivateFieldDyn2
  /// §7.3.31 PrivateSet with the PrivateName on the stack.
  /// Stack: [key, val, obj, ..] → [val, ..]. TypeError if obj is not a
  /// JsObject, lacks the own element, the element is a method
  /// (non-writable), or an accessor without a setter.
  PutPrivateFieldDyn
  /// §13.10.1 `#x in obj` with the PrivateName on the stack.
  /// Stack: [key, obj, ..] → [JsBool, ..]. Own-only check.
  PrivateInDyn
  /// §7.3.28 PrivateFieldAdd. Stack: [val, key, obj, ..] → [obj, ..].
  /// TypeError if obj is non-extensible (proposal
  /// nonextensible-applies-to-private) or already has the element
  /// (return-override double initialization).
  DefinePrivateField
  /// §7.3.29 PrivateMethodOrAccessorAdd for a method.
  /// Stack: [fn, key, obj, ..] → [obj, ..]. Same TypeErrors as
  /// DefinePrivateField. Does NOT touch fn.[[HomeObject]] — private method
  /// closures are created once at class-definition time with
  /// [[HomeObject]] = ctor.prototype (or ctor for statics).
  DefinePrivateMethod
  /// §7.3.29 for one accessor half. Stack: [fn, key, obj, ..] → [obj, ..].
  /// Merges into an existing own accessor entry only if that half is absent
  /// (get+set pair from one class evaluation); TypeError if the half is
  /// already present (double initialization) or obj is non-extensible.
  DefinePrivateAccessor(kind: AccessorKind)

  // -- Object/Array Construction --
  NewObject
  DefineField(key: PropertyKey)
  DefineFieldComputed
  /// §7.1.19 ToPropertyKey. Stack: [key, ..] → [key', ..]. Runs
  /// ToPrimitive(key, string); Symbols pass through, everything else is
  /// ToString'd. Emitted at class-definition time for computed field names
  /// (§15.7.14 ClassFieldDefinitionEvaluation step 1) so name side effects
  /// and abrupt completions happen ONCE, not per instantiation.
  ToPropertyKey
  DefineMethod(key: PropertyKey)
  DefineMethodComputed
  /// `enumerable`: True for object-literal accessors (§13.2.5.5
  /// PropertyDefinitionEvaluation passes enumerable=true), False for class
  /// methods (§15.4.5 MethodDefinitionEvaluation passes enumerable=false).
  DefineAccessor(key: PropertyKey, kind: AccessorKind, enumerable: Bool)
  DefineAccessorComputed(kind: AccessorKind, enumerable: Bool)
  /// §15.4.4 MakeMethod — peek [fn, obj, ...], set fn.[[HomeObject]] = obj.
  /// Stack-neutral. Emitted for object-literal shorthand methods so super.x
  /// works; not for `{m: fn}` (no HomeObject per spec).
  MakeMethod
  /// Annex B §B.3.1 — `{__proto__: v}`. Stack: [val, obj, ...] → [obj, ...].
  /// If val is an Object or null, sets obj.[[Prototype]] = val; else no-op.
  SetProto
  ObjectSpread
  /// Stack: [src, key_n, ..., key_1, ...] → [rest_obj, ...]
  /// Allocates a fresh object and CopyDataProperties(rest_obj, src, excluded)
  /// where excluded is the `excluded_count` keys popped beneath src. Throws
  /// TypeError if src is null/undefined (RequireObjectCoercible). Used for
  /// destructuring rest pattern `{a, b, ...rest}`.
  ObjectRestCopy(excluded_count: Int)
  ArrayFrom(count: Int)
  /// Pop `count - length(holes)` values, build a sparse array of length `count`
  /// with the hole indices left empty. Used for array literals with elisions
  /// (e.g. `[1,,3]` → ArrayFromWithHoles(3, [1])). Hole indices must be sorted
  /// ascending. Falls back to ArrayFrom when holes is empty — emitter decides.
  ArrayFromWithHoles(count: Int, holes: List(Int))
  /// [val, arr] → [arr]; sets arr[arr.length] = val, increments length.
  /// Used for non-spread elements after the first spread in an array literal.
  ArrayPush
  /// [arr] → [arr]; increments length WITHOUT setting any element (creates a
  /// hole at the previous length). Used for elisions after the first spread
  /// in an array literal (e.g. the hole in `[1, ...x, , 3]`).
  ArrayPushHole
  /// [iterable, arr] → [arr]; iterates iterable, appends each to arr.
  /// Used for spread elements in array literals and argument lists.
  /// Throws TypeError if iterable is not iterable (unlike ObjectSpread).
  ArraySpread

  // -- Calls --
  Call(arity: Int)
  /// Like Call but emitted for a syntactic `eval(...)` (identifier callee
  /// named "eval"). At runtime, if the callee resolves to the intrinsic eval
  /// function, performs a DIRECT eval (sees caller's local scope via boxed
  /// locals + FuncTemplate.local_names). Otherwise behaves identically to Call.
  ///
  /// `param_scope_names` is non-empty only when this eval call site sits
  /// inside a formal-parameter initializer: it lists the parameter-scope
  /// binding names (parameters, plus the implicit `arguments` for
  /// non-arrows). §19.2.1.1/§9.3.4 EvalDeclarationInstantiation step 3.d:
  /// sloppy direct eval must throw a SyntaxError when a var-declared name
  /// in the eval code collides with a binding in any environment between
  /// the eval's LexicalEnvironment and its VariableEnvironment — for a
  /// parameter initializer that is exactly the parameter scope.
  ///
  /// `with_names` lists the synthetic with-object locals (innermost first)
  /// for the `with` statements lexically enclosing this eval call site —
  /// including withs inherited from enclosing functions. Direct eval
  /// compiles its code with these as object-environment markers so free
  /// names in the eval'd source check the with objects first (§9.1.2.1
  /// GetIdentifierReference walks the caller's LexicalEnvironment).
  ///
  /// `private_names` lists the private names ("#x") visible at this eval
  /// call site — the caller's [[PrivateEnvironment]] chain. §19.2.1.1
  /// PerformEval step 5: direct eval code is parsed with the caller's
  /// private environment, so `this.#x` in the eval'd source is a
  /// SyntaxError unless "#x" is in this list (or declared by a class in
  /// the eval source itself).
  CallEval(
    arity: Int,
    param_scope_names: List(String),
    with_names: List(String),
    private_names: List(String),
  )
  /// `obj.m(args)`. Stack: [arg_n, ..., arg_1, receiver, fn, ..] → [result, ..]
  /// (the GetField2 pair). Identical to `Call` except `this` = receiver.
  CallMethod(arity: Int)
  /// `new ctor(args)` and `super(args)`. Stack:
  /// [arg_n, ..., arg_1, new_target, ctor, ..] → [instance, ..].
  /// §10.1.13 [[Construct]] takes (args, newTarget); for plain `new X()` the
  /// emitter Dups X so newTarget == ctor, for `super()` it pushes the lexical
  /// new.target slot. Mirrors QuickJS OP_call_constructor.
  CallConstructor(arity: Int)
  /// [args_array, callee] → [result]; this=undefined. Spread-call path.
  CallApply
  /// [args_array, callee, receiver] → [result]; this=receiver. Spread-method-call.
  CallMethodApply
  /// [args_array, new_target, ctor] → [new instance]. Spread-new path.
  CallConstructorApply
  Return

  // -- Control Flow (absolute PC targets) --
  Jump(target: Int)
  JumpIfFalse(target: Int)
  JumpIfTrue(target: Int)
  JumpIfNullish(target: Int)
  /// Push (pc+1) onto operand stack as return address, jump to target.
  /// QuickJS OP_gosub — used to enter the finally block as a subroutine.
  Gosub(target: Int)
  /// Pop return address from operand stack, jump to it. QuickJS OP_ret —
  /// used to return from the finally block to the caller's continuation.
  Ret

  // -- Exception Handling --
  Throw
  /// §9.1.1.1.5 step 6 — assignment to an immutable (const) local binding.
  /// Always throws TypeError (const bindings are strict per §14.3.1.3
  /// CreateImmutableBinding(dn, true)). Emitted in place of PutLocal/PutBoxed
  /// when a name-store resolves to a ConstBinding. Mirrors QuickJS
  /// OP_throw_error/JS_THROW_VAR_RO.
  ThrowConstAssign(name: String)
  /// Compile-time-determined runtime error — throw the given native error
  /// kind with a static message. Used for forms the spec defines as
  /// unconditional runtime throws (e.g. §13.5.1.2 `delete super.x`). Mirrors
  /// QuickJS OP_throw_error.
  ThrowError(kind: ErrorKind, msg: String)
  /// Push an exception-handler frame. `kind` says what the frame is FOR, so
  /// the return-completion unwinder (generators.find_next_return_handler)
  /// never has to guess by disassembling whatever sits at `catch_target`.
  PushTry(catch_target: Int, kind: TryKind)
  PopTry

  // -- Closures --
  MakeClosure(func_index: Int)
  /// Wrap locals[index] value into a BoxSlot on the heap, replace local with ref.
  BoxLocal(index: Int)
  /// Read locals[index] (a box ref), dereference BoxSlot, push value on stack.
  GetBoxed(index: Int)
  /// Pop value from stack, read locals[index] (a box ref), write value into BoxSlot.
  PutBoxed(index: Int)
  /// BindThisValue for boxed `this` (captured by arrow inside ctor).
  PutBoxedCheckInit(index: Int)

  // -- Operators --
  /// Carries the operator ALREADY narrowed to its handler (see `Classified`).
  /// The resolver classifies once, when it lowers `IrBinOp`; the interpreter
  /// then dispatches straight on the stored variant, so a `BinOp` step costs
  /// no call and cannot hand `exec_binop` an `Add`/`In`/`InstanceOf`. Build it
  /// with `bin_op`, never `BinOp(classify(..))` at execution time.
  BinOp(kind: Classified)
  UnaryOp(kind: UnaryOpKind)
  TypeOf
  TypeofGlobal(name: String)

  // -- Fused superinstructions (resolver peephole, see resolve.peephole) --
  /// Statement-position postfix `i++;` on a plain local. Folds
  /// GetLocal(i); UnaryOp(Pos); Dup; PushConst(1); BinOp(Add); PutLocal(i);
  /// Pop — the Dup'd old value was immediately discarded, so the whole
  /// 7-op sequence is one dispatch. Semantics are identical to the sequence:
  /// TDZ check, ToNumber (ToPrimitive for objects), numeric add, store.
  IncLocal(index: Int)
  /// Statement-position postfix `i--;` — same as IncLocal with Sub.
  DecLocal(index: Int)
  /// Fused loop-condition compare-and-branch:
  /// GetLocal(left); GetLocal(right); BinOp(kind); JumpIfFalse(target).
  /// Only emitted for the pure relational kinds (Lt/LtEq/Gt/GtEq) — hence the
  /// `PureBinOp` field: an `Add`/`In`/`InstanceOf` here would be a compile
  /// error, not a runtime surprise for `binop_direct`.
  CmpLocalLocalJump(left: Int, right: Int, kind: PureBinOp, target: Int)
  /// Same with a constant right operand:
  /// GetLocal(left); PushConst(const_index); BinOp(kind); JumpIfFalse(target).
  CmpLocalConstJump(left: Int, const_index: Int, kind: PureBinOp, target: Int)

  // -- Iteration --
  ForInStart
  ForInNext
  GetIterator
  GetAsyncIterator
  /// GetIteratorFromMethod step 4 (§7.4.4): pop an iterator object, Get its
  /// `next` (observable, abrupt propagates), push an internal Iterator Record
  /// caching it. Emitted after GetAsyncIterator for async yield* so the loop
  /// reuses [[NextMethod]] instead of re-Getting `next` each step.
  IteratorRecord
  IteratorNext
  /// §7.4.11 normal-completion close. Stack: [iter, ..] → [..]. Get .return;
  /// undef/null → no-op; call it; throw → propagate; non-object → TypeError.
  IteratorClose
  /// §7.4.11 throw-completion close. Stack: [thrown, iter, ..] → rethrows
  /// thrown. Get .return; call it swallowing any error; rethrow original.
  /// Reached as a PushTry catch_target when a for-of/destructuring body throws.
  IteratorCloseThrow
  /// Stack: [v, ..] → [v, ..]. Throws TypeError if v is not a JsObject.
  /// Used after Await in AsyncIteratorClose normal path (§7.4.12 step 6) and
  /// after Await(next()) in for-await-of (§14.7.5.6 step 6.c).
  IteratorCheckObject
  /// §13.15.5.3 / §14.3.3 rest element. Stack: [iter, ..] → [arr, ..]. Drains
  /// iter via .next() into a fresh Array — does NOT re-GetIterator. Emitter
  /// pops the close-guard try frame first so .next() throw skips close.
  IteratorRest

  // -- Class Inheritance / Super --
  /// Wire prototype chain for derived class: [parent, ctor] → [ctor]
  SetupDerivedClass
  /// Generic [[GetPrototypeOf]]. Stack: [obj, ..] → [proto|null, ..].
  /// Reads the object's [[Prototype]] slot — JsNull if none/null. Emitted as
  /// the second hop for `super.x` (home_object → proto) and for `super()`
  /// (active_func → parent ctor). Mirrors QuickJS OP_get_super.
  GetPrototypeOf
  /// `super.x` / `super[k]`. Stack: [key, base, this, ..] → [val, ..].
  /// OrdinaryGet on base with receiver = this. Mirrors QuickJS
  /// OP_get_super_value.
  GetSuperValue
  /// Read-under variant for `super.x op= v` / `++super[k]`. Stack:
  /// [key, base, this, ..] → [val, pk, base, this, ..]. ToPropertyKey is
  /// applied ONCE; the coerced key is left on stack as a primitive so the
  /// trailing PutSuperValue re-coerces side-effect-free. Mirrors QuickJS
  /// `to_propkey; dup3; get_super_value` but as one op (arc has no Dup3).
  GetSuperValue2
  /// `super.x = v` / `super[k] = v`. Stack: [val, key, base, this, ..] →
  /// [val, ..]. OrdinarySet on base with receiver = this. Mirrors QuickJS
  /// OP_put_super_value.
  PutSuperValue

  // -- Generator --
  /// Emitted at start of generator body. Suspends immediately (SuspendedStart).
  InitialYield
  /// Pop value from stack, suspend generator. On resume, .next(arg) pushed.
  Yield
  /// Self-looping delegate yield. Stack: [arg, iter, ..]. Calls iter.next(arg).
  /// If done → pops both, pushes result.value, pc+1. If !done → yields
  /// result.value, leaves [iter] on stack, pc stays (re-executes on resume
  /// with [resume_val, iter]).
  YieldStar
  /// Async-generator yield* — phase 1/2. Stack: [arg, iter, ..]. Calls
  /// iter.next(arg), pops arg, pushes the (possibly thenable) result →
  /// [result, iter, ..], pc+1. Always followed by Await then
  /// AsyncYieldStarResume; Await suspends and resume pushes the settled
  /// {value,done} object back → [result_obj, iter, ..].
  /// Ok-returning (not Awaited) so job_queue mutations from the inner
  /// .next() call (e.g. AsyncFromSyncIterator's unwrap microtask) thread
  /// through to the Await step. ES §15.5.5 step 8.a.i-ii.
  ///
  /// `after_pc` is the resolved PC of the instruction *following* the whole
  /// yield* sequence. The async-generator driver resumes the body there when
  /// a forwarded `.throw()` makes the inner iterator report done, so nothing
  /// depends on how many opcodes the sequence lowers to.
  AsyncYieldStarNext(after_pc: Int)
  /// Async-generator yield* — phase 2/2. Stack: [result_obj, iter, ..].
  /// IteratorComplete(result_obj): if done → pop both, push value, pc+1.
  /// If !done → Yielded(value); execute_inner's Yielded arm pops result_obj,
  /// keeps iter, sets pc to `next_pc` (the AsyncYieldStarNext op) so
  /// .next(v) resumes with [v, iter, ..]. ES §15.5.5 step 8.a.iv-vii.
  AsyncYieldStarResume(next_pc: Int)

  // -- Async --
  /// Pop value from stack, wrap in Promise.resolve, suspend async function.
  /// On resume, resolved value is pushed onto stack.
  Await

  // -- Arguments object --
  /// Create an arguments object from the current call's original args.
  /// Reads state.call_args, allocates ArgumentsObject, pushes ref onto stack.
  /// `simple_params` is §10.2.11 step 20's "simpleParameterList": False when
  /// the function has a default, destructuring pattern, or rest parameter —
  /// such functions get an UNMAPPED arguments object whose "callee" is the
  /// %ThrowTypeError% accessor even in sloppy mode (§10.4.4.6).
  CreateArguments(simple_params: Bool)

  /// Create a rest-parameter array from the current call's args, taking those
  /// at index `from_index` and beyond. Reads state.call_args, allocates a plain
  /// Array, pushes ref onto stack.
  CreateRestArray(from_index: Int)

  // -- RegExp --
  /// Pop flags string, pop pattern string -> push new RegExp object.
  NewRegExp

  // -- Dynamic import --
  /// §13.3.10 ImportCall: pop options, pop specifier -> push a new promise.
  /// ToString/options failures reject the promise (IfAbruptRejectPromise);
  /// loading is delegated to the host import hook on the global object.
  DynamicImport
  /// `import.source(specifier)`: pop specifier -> push a new promise.
  /// Source Text Module Records have no source phase representation
  /// (GetModuleSource throws), so a coercible specifier rejects with
  /// SyntaxError.
  DynamicImportSource
  /// `import.defer(specifier)`: pop specifier -> push a new promise.
  /// Evaluation deferral is not implemented — behaves as `import(specifier)`.
  DynamicImportDefer

  // -- Global Environment Record --
  /// §9.1.1.4.17 CreateGlobalVarBinding: create a writable/enumerable
  /// data property on globalThis (if not already an own property).
  /// `deletable` is the spec's D argument and becomes [[Configurable]]:
  /// script/function GlobalDeclarationInstantiation passes D = false
  /// (§9.1.1.4.18), so a top-level `var`/function binding survives
  /// `delete`; sloppy eval code passes D = true (§19.2.1.3).
  DeclareGlobalVar(name: String, deletable: Bool)
  /// Create entry in lexical_globals (with JsUninitialized for TDZ).
  DeclareGlobalLex(name: String, is_const: Bool)
  /// Pop value from stack, initialize lexical binding (TDZ → value).
  InitGlobalLex(name: String)

  // -- Explicit Resource Management (using / await using desugar) --
  /// CreateDisposableResource(V, hint): pop the resource value; push a
  /// 0-arity disposer callable (calls the captured @@dispose/@@asyncDispose
  /// method with the resource as `this`), or undefined when the value is
  /// null/undefined. Throws TypeError for non-disposable values.
  GetDisposer(is_async: Bool)
  /// DisposeResources error folding: pop suppressed, pop error; push a new
  /// SuppressedError with .error = error and .suppressed = suppressed.
  MakeSuppressed
}

pub type AccessorKind {
  Getter
  Setter
}

// ============================================================================
// Operator Kinds
// ============================================================================

pub type BinOpKind {
  // Arithmetic
  Add
  Sub
  Mul
  Div
  Mod
  Exp
  // Bitwise
  BitAnd
  BitOr
  BitXor
  ShiftLeft
  ShiftRight
  UShiftRight
  // Comparison (== with coercion)
  Eq
  NotEq
  // Comparison (=== strict)
  StrictEq
  StrictNotEq
  // Relational
  Lt
  LtEq
  Gt
  GtEq
  // Relational keywords
  In
  InstanceOf
}

/// A `BinOpKind` split by WHO evaluates it. Produced by `classify`, which is
/// the single, total place a source-level operator narrows to the pure subset
/// `ops/operators.exec_binop` can actually run.
///
/// This is what `Op.BinOp` stores: classification is a pure function of the
/// operator, so it is done ONCE at bytecode-resolution time rather than on
/// every execution of the instruction — same trick the fused
/// `CmpLocalLocalJump` / `CmpLocalConstJump` superinstructions already use.
pub type Classified {
  /// Pure: two primitives in, one JsValue out. `exec_binop` handles it.
  PureOp(op: PureBinOp)
  /// §13.15.3 `+`: ToPrimitive(default) then string-concat or numeric add.
  /// The interpreter's `add_primitives` owns it.
  AddOp
  /// `in`: needs the heap (and can run a Proxy trap).
  InOp
  /// `instanceof`: needs the heap and can run `Symbol.hasInstance`.
  InstanceOfOp
}

/// Total classification of a binary operator. Adding a `BinOpKind` variant is
/// a compile error here until it is assigned to a handler.
pub fn classify(kind: BinOpKind) -> Classified {
  case kind {
    Add -> AddOp
    In -> InOp
    InstanceOf -> InstanceOfOp
    Sub -> PureOp(binop.Sub)
    Mul -> PureOp(binop.Mul)
    Div -> PureOp(binop.Div)
    Mod -> PureOp(binop.Mod)
    Exp -> PureOp(binop.Exp)
    BitAnd -> PureOp(binop.BitAnd)
    BitOr -> PureOp(binop.BitOr)
    BitXor -> PureOp(binop.BitXor)
    ShiftLeft -> PureOp(binop.Shl)
    ShiftRight -> PureOp(binop.Shr)
    UShiftRight -> PureOp(binop.UShr)
    Eq -> PureOp(binop.Eq)
    NotEq -> PureOp(binop.NotEq)
    StrictEq -> PureOp(binop.StrictEq)
    StrictNotEq -> PureOp(binop.StrictNotEq)
    Lt -> PureOp(binop.Lt)
    LtEq -> PureOp(binop.LtEq)
    Gt -> PureOp(binop.Gt)
    GtEq -> PureOp(binop.GtEq)
  }
}

/// The `BinOp` opcode for a source-level operator, classified once here so the
/// interpreter never has to. The only way `Op.BinOp` should ever be built.
pub fn bin_op(kind: BinOpKind) -> Op {
  BinOp(classify(kind))
}

pub type UnaryOpKind {
  Neg
  Pos
  BitNot
  LogicalNot
  Void
}

// ============================================================================
// IR Opcodes — emitted by the AST emitter, consumed by label resolution
// ============================================================================

/// Symbolic IR instruction. Variable references are emitted as concrete
/// slot ops (GetLocal/GetBoxed/GetGlobal/IrWith*) by the emitter consulting
/// the AST-level scope tree; jump targets use label IDs (resolved in Phase 3).
pub type IrOp {
  // -- Labels and jumps (resolved in Phase 3) --
  IrLabel(id: Int)
  IrJump(label: Int)
  IrJumpIfFalse(label: Int)
  IrJumpIfTrue(label: Int)
  IrJumpIfNullish(label: Int)
  IrPushTry(catch_label: Int, kind: TryKind)
  IrGosub(label: Int)
  IrRet

  // -- Resolved variable access (emitted directly from the scope tree) --
  IrGetLocal(index: Int)
  IrPutLocal(index: Int)
  IrPutLocalCheckInit(index: Int)
  IrGetGlobal(name: String)
  IrPutGlobal(name: String)
  /// Lowers 1:1 to DeleteGlobalVar. See Op.DeleteGlobalVar.
  IrDeleteGlobalVar(name: String)
  IrTypeofGlobal(name: String)
  IrGetEvalVar(name: String)
  IrPutEvalVar(name: String)
  IrDeclareEvalVar(name: String)
  IrTypeofEvalVar(name: String)

  // -- `with` statement ops (object check emitted from the scope tree's
  // with-chain; labels resolved in Phase 3) --
  IrToObject
  /// Lowers 1:1 to ToStringVal (template literal substitutions).
  IrToStringVal
  /// Lowers 1:1 to GetTemplateObject (tagged templates, §13.2.8.4).
  IrGetTemplateObject(site: Int, quasis: List(TemplateQuasi))
  IrWithGetVar(name: String, label: Int)
  IrWithGetVarThis(name: String, label: Int)
  IrWithPutVar(name: String, label: Int)
  IrWithDeleteVar(name: String, label: Int)
  IrWithMakeRef(name: String, label: Int)
  IrWithGetRefValue(name: String, label: Int)
  IrWithPutRefValue(name: String, label: Int)

  // -- Everything else is the same as final Op --
  /// Lowers 1:1 to SetLine. See Op.SetLine.
  IrSetLine(line: Int)
  IrPushConst(index: Int)
  IrPop
  IrDup
  IrSwap
  /// Lowers 1:1 to Rot3. See Op.Rot3.
  IrRot3
  /// Lowers 1:1 to Unrot4. See Op.Unrot4.
  IrUnrot4
  IrGetField(name: String)
  IrGetField2(name: String)
  IrPutField(name: String)
  IrGetElem
  IrGetElem2
  IrPutElem
  IrDeleteField(name: String)
  IrDeleteElem
  IrGetPrivateField(name: String)
  IrGetPrivateField2(name: String)
  IrPutPrivateField(name: String)
  IrPrivateIn(name: String)
  IrNewPrivateName(name: String)
  IrGetPrivateFieldDyn
  IrGetPrivateFieldDyn2
  IrPutPrivateFieldDyn
  IrPrivateInDyn
  IrDefinePrivateField
  IrDefinePrivateMethod
  IrDefinePrivateAccessor(kind: AccessorKind)
  IrNewObject
  IrDefineField(name: String)
  IrDefineFieldComputed
  IrToPropertyKey
  IrDefineMethod(name: String)
  IrDefineMethodComputed
  IrDefineAccessor(name: String, kind: AccessorKind, enumerable: Bool)
  IrDefineAccessorComputed(kind: AccessorKind, enumerable: Bool)
  IrMakeMethod
  IrSetProto
  IrObjectSpread
  IrObjectRestCopy(excluded_count: Int)
  IrArrayFrom(count: Int)
  IrArrayFromWithHoles(count: Int, holes: List(Int))
  IrArrayPush
  IrArrayPushHole
  IrArraySpread
  IrCall(arity: Int)
  IrCallEval(
    arity: Int,
    param_scope_names: List(String),
    with_names: List(String),
    private_names: List(String),
  )
  IrCallMethod(arity: Int)
  IrCallConstructor(arity: Int)
  IrCallApply
  IrCallMethodApply
  IrCallConstructorApply
  IrReturn
  IrThrow
  IrThrowConstAssign(name: String)
  IrThrowError(kind: ErrorKind, msg: String)
  IrPopTry
  IrMakeClosure(func_index: Int)
  IrBoxLocal(index: Int)
  IrGetBoxed(index: Int)
  IrPutBoxed(index: Int)
  IrPutBoxedCheckInit(index: Int)
  IrBinOp(kind: BinOpKind)
  IrUnaryOp(kind: UnaryOpKind)
  IrTypeOf

  // -- Fused superinstructions (produced by the resolver peephole; never
  // emitted directly by the AST emitter). See the matching final Ops.
  IrIncLocal(index: Int)
  IrDecLocal(index: Int)
  IrCmpLocalLocalJump(left: Int, right: Int, kind: PureBinOp, label: Int)
  IrCmpLocalConstJump(left: Int, const_index: Int, kind: PureBinOp, label: Int)
  IrForInStart
  IrForInNext
  IrGetIterator
  IrGetAsyncIterator
  IrIteratorRecord
  IrIteratorNext
  IrIteratorClose
  IrIteratorCloseThrow
  IrIteratorCheckObject
  IrIteratorRest
  IrSetupDerivedClass
  IrGetPrototypeOf
  IrGetSuperValue
  IrGetSuperValue2
  IrPutSuperValue
  IrInitialYield
  IrYield
  IrYieldStar
  IrAsyncYieldStarNext(after_label: Int)
  IrAsyncYieldStarResume(next_label: Int)
  IrAwait
  IrCreateArguments(simple_params: Bool)
  IrCreateRestArray(from_index: Int)
  IrNewRegExp
  IrDynamicImport
  IrDynamicImportSource
  IrDynamicImportDefer

  // -- Global Environment Record --
  /// Lowers 1:1 to DeclareGlobalVar. See Op.DeclareGlobalVar for `deletable`.
  IrDeclareGlobalVar(name: String, deletable: Bool)
  IrDeclareGlobalLex(name: String, is_const: Bool)
  IrInitGlobalLex(name: String)

  // -- Explicit Resource Management (using / await using desugar) --
  IrGetDisposer(is_async: Bool)
  IrMakeSuppressed
}
