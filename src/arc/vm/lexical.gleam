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
//
// Lives in its own module (not opcode.gleam) so the parser and scope
// analyser can name a lexical pseudo-binding without importing the VM's
// bytecode encoding.

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

pub const every_lexical_ref = LexicalRefs(True, True, True, True)

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
