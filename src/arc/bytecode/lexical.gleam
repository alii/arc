import gleam/option.{type Option, None, Some}

// §9.1.1.3 function env slots, like quickjs pseudo vars
pub type LexicalRef {
  RefThis
  RefActiveFunc
  RefHomeObject
  RefNewTarget
}

// order must match frame.setup_frame
pub const all_lexical_refs = [
  RefThis,
  RefActiveFunc,
  RefHomeObject,
  RefNewTarget,
]

pub type LexicalSlots {
  OwnedLexicalSlots(base: Int)
  CapturedLexicalSlots(
    this: Option(Int),
    active_func: Option(Int),
    home_object: Option(Int),
    new_target: Option(Int),
  )
  NoLexicalSlots
}

pub const owned_lexical_slot_count = 4

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

pub fn lexical_ref_offset(ref: LexicalRef) -> Int {
  case ref {
    RefThis -> 0
    RefActiveFunc -> 1
    RefHomeObject -> 2
    RefNewTarget -> 3
  }
}

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

// §19.2.1.1 eval syntax legality derives from this
pub type CodeKind {
  ScriptCode
  FunctionCode
  MethodCode
  DerivedCtorCode
  FieldInitCode
}

pub fn new_target_allowed(kind: CodeKind) -> Bool {
  case kind {
    ScriptCode -> False
    FunctionCode | MethodCode | DerivedCtorCode | FieldInitCode -> True
  }
}

pub fn super_prop_allowed(kind: CodeKind) -> Bool {
  case kind {
    ScriptCode | FunctionCode -> False
    MethodCode | DerivedCtorCode | FieldInitCode -> True
  }
}

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
