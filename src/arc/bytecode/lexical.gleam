import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}

// §9.1.1.3 function env slots, like quickjs pseudo vars
pub type LexicalRef {
  RefThis
  RefActiveFunc
  RefHomeObject
  RefNewTarget
}

// order must match interp/call.setup_frame
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

pub fn captured_slots(
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

pub fn slot_of(slots: LexicalSlots, ref: LexicalRef) -> Option(Int) {
  case slots {
    NoLexicalSlots -> None
    OwnedLexicalSlots(base) -> Some(base + ref_offset(ref))
    CapturedLexicalSlots(this:, active_func:, home_object:, new_target:) ->
      case ref {
        RefThis -> this
        RefActiveFunc -> active_func
        RefHomeObject -> home_object
        RefNewTarget -> new_target
      }
  }
}

pub fn ref_offset(ref: LexicalRef) -> Int {
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

pub const no_lexical_refs = LexicalRefs(
  this: False,
  active_func: False,
  home_object: False,
  new_target: False,
)

pub const every_lexical_ref = LexicalRefs(
  this: True,
  active_func: True,
  home_object: True,
  new_target: True,
)

pub fn refs_or(a: LexicalRefs, b: LexicalRefs) -> LexicalRefs {
  LexicalRefs(
    this: a.this || b.this,
    active_func: a.active_func || b.active_func,
    home_object: a.home_object || b.home_object,
    new_target: a.new_target || b.new_target,
  )
}

pub fn refs_and(a: LexicalRefs, b: LexicalRefs) -> LexicalRefs {
  LexicalRefs(
    this: a.this && b.this,
    active_func: a.active_func && b.active_func,
    home_object: a.home_object && b.home_object,
    new_target: a.new_target && b.new_target,
  )
}

pub fn refs_present(d: Dict(LexicalRef, a)) -> LexicalRefs {
  LexicalRefs(
    this: dict.has_key(d, RefThis),
    active_func: dict.has_key(d, RefActiveFunc),
    home_object: dict.has_key(d, RefHomeObject),
    new_target: dict.has_key(d, RefNewTarget),
  )
}

pub fn refs_get(refs: LexicalRefs, ref: LexicalRef) -> Bool {
  case ref {
    RefThis -> refs.this
    RefActiveFunc -> refs.active_func
    RefHomeObject -> refs.home_object
    RefNewTarget -> refs.new_target
  }
}

// consecutive slots from `from` for each kept ref, in setup_frame order
pub fn number_refs(
  from from: Int,
  keep keep: fn(LexicalRef) -> Bool,
) -> Dict(LexicalRef, Int) {
  let #(numbered, _next) = {
    use #(numbered, next) as acc, ref <- list.fold(all_lexical_refs, #(
      dict.new(),
      from,
    ))
    case keep(ref) {
      True -> #(dict.insert(numbered, ref, next), next + 1)
      False -> acc
    }
  }
  numbered
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
