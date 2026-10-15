import arc/bytecode/lexical.{
  type CodeKind, type LexicalRef, DerivedCtorCode, FieldInitCode, FunctionCode,
  MethodCode, ScriptCode,
}
import arc/compiler/scope.{
  type BindingKind, type CaptureSeed, type FnSource, AnyParams, ArrowInherits,
  CaptureBinding, CaptureSeed, ConstBinding, FnArrow, FnDeclaration,
  FnExpression, FnMethod, FnNameBinding, FnSource, GetterParams, LetBinding,
  ParamBinding, SetterParams, VarBinding,
}
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

/// what a function that has not been compiled yet needs to compile itself,
/// apart from the unit's text: where it starts in it and what the code around
/// it hands it. Kept packed (see pack) and carried by each of its closures
pub type LazyFunction {
  LazyFunction(
    source: FnSource,
    // for each captured value, in env order, the local it has in the parent
    parent_slots: List(Int),
    // the name the code around it gave it
    name: Option(String),
    arity: Int,
    // §15.1.5 the length property value
    length: Int,
    // for each outer name it uses, in order, how the parent hands it over:
    // see pack_free
    free: BitArray,
    // an arrow's this, new.target and the like, see pack_lexical
    lexical: Int,
    // an arrow is the kind of code it is written in
    code_kind: CodeKind,
  )
}

/// the record a closure of a lazy function carries. Where its text starts and
/// the parent's slots come first, since making a closure reads only those
pub fn pack(function: LazyFunction) -> BitArray {
  let LazyFunction(
    source: FnSource(start:, end:, line:, form:, strict:, in_class:),
    parent_slots:,
    name:,
    arity:,
    length:,
    free:,
    lexical:,
    code_kind:,
  ) = function
  let flag = fn(on: Bool, bit: Int) {
    case on {
      True -> bit
      False -> 0
    }
  }
  let form_bits = case form {
    FnDeclaration -> 0
    FnExpression -> 2
    FnArrow(ArrowInherits(
      allow_new_target:,
      allow_super_property:,
      allow_super_call:,
      allow_in:,
      in_generator:,
      in_async:,
    )) ->
      4
      + flag(allow_new_target, 8)
      + flag(allow_super_property, 16)
      + flag(allow_super_call, 32)
      + flag(allow_in, 64)
      + flag(in_generator, 128)
      + flag(in_async, 256)
    FnMethod(params:, is_generator:, is_async:) ->
      1024
      + flag(is_generator, 2048)
      + flag(is_async, 4096)
      + case params {
        AnyParams -> 0
        GetterParams -> 8192
        SetterParams -> 16_384
      }
  }
  let name_bytes = case name {
    None -> put(<<>>, 0)
    Some(name) -> {
      let bytes = bit_array.from_string(name)
      <<put(<<>>, bit_array.byte_size(bytes) + 1):bits, bytes:bits>>
    }
  }
  [start, list.length(parent_slots), ..parent_slots]
  |> list.append([
    end - start,
    line,
    flag(strict, 1) + flag(in_class, 512) + form_bits,
    code_kind_number(code_kind),
    lexical,
    arity,
    length,
  ])
  |> list.fold(<<>>, put)
  |> bit_array.append(name_bytes)
  |> bit_array.append(free)
}

/// where a packed function's text starts, and the parent's slots for what it
/// captures: all that making a closure of it needs
pub fn creation(record: BitArray) -> Result(#(Int, List(Int)), Nil) {
  use #(start, rest) <- result.try(take(record))
  use #(count, rest) <- result.try(take(rest))
  use #(slots, _rest) <- result.map(take_many(rest, count, []))
  #(start, slots)
}

pub fn unpack(record: BitArray) -> Result(LazyFunction, Nil) {
  use #(start, rest) <- result.try(take(record))
  use #(count, rest) <- result.try(take(rest))
  use #(parent_slots, rest) <- result.try(take_many(rest, count, []))
  use #(numbers, rest) <- result.try(take_many(rest, 7, []))
  let assert [size, line, flags, code_kind, lexical, arity, length] = numbers
  use #(name_size, rest) <- result.try(take(rest))
  use #(name, free) <- result.map(case name_size {
    0 -> Ok(#(None, rest))
    _ -> {
      let size = name_size - 1
      case rest {
        <<bytes:bytes-size(size), free:bits>> ->
          bit_array.to_string(bytes)
          |> result.map(fn(name) { #(Some(name), free) })
        _ -> Error(Nil)
      }
    }
  })
  let on = fn(bit: Int) { int.bitwise_and(flags, bit) != 0 }
  let form = case on(1024), on(4), on(2) {
    True, _, _ ->
      FnMethod(
        params: case on(8192), on(16_384) {
          True, _ -> GetterParams
          False, True -> SetterParams
          False, False -> AnyParams
        },
        is_generator: on(2048),
        is_async: on(4096),
      )
    False, True, _ ->
      FnArrow(ArrowInherits(
        allow_new_target: on(8),
        allow_super_property: on(16),
        allow_super_call: on(32),
        allow_in: on(64),
        in_generator: on(128),
        in_async: on(256),
      ))
    False, False, True -> FnExpression
    False, False, False -> FnDeclaration
  }
  LazyFunction(
    source: FnSource(
      start:,
      end: start + size,
      line:,
      form:,
      strict: on(1),
      in_class: on(512),
    ),
    parent_slots:,
    name:,
    arity:,
    length:,
    free:,
    lexical:,
    code_kind: numbered_code_kind(code_kind),
  )
}

// a whole number, seven bits to a byte, low bits first, top bit set on all
// but the last byte
fn put(packed: BitArray, number: Int) -> BitArray {
  case number < 128 {
    True -> <<packed:bits, number>>
    False -> put(<<packed:bits, { number % 128 + 128 }>>, number / 128)
  }
}

fn take(packed: BitArray) -> Result(#(Int, BitArray), Nil) {
  case packed {
    <<byte, rest:bits>> if byte < 128 -> Ok(#(byte, rest))
    <<byte, rest:bits>> -> {
      use #(high, rest) <- result.map(take(rest))
      #(byte - 128 + high * 128, rest)
    }
    _ -> Error(Nil)
  }
}

fn take_many(
  packed: BitArray,
  count: Int,
  taken: List(Int),
) -> Result(#(List(Int), BitArray), Nil) {
  case count {
    0 -> Ok(#(list.reverse(taken), packed))
    _ -> {
      use #(number, rest) <- result.try(take(packed))
      take_many(rest, count - 1, [number, ..taken])
    }
  }
}

fn code_kind_number(kind: CodeKind) -> Int {
  case kind {
    ScriptCode -> 0
    FunctionCode -> 1
    MethodCode -> 2
    DerivedCtorCode -> 3
    FieldInitCode -> 4
  }
}

fn numbered_code_kind(number: Int) -> CodeKind {
  case number {
    0 -> ScriptCode
    1 -> FunctionCode
    2 -> MethodCode
    3 -> DerivedCtorCode
    _ -> FieldInitCode
  }
}

/// a byte for each outer name: 255 when the parent does not supply it (it is
/// a global), else the declared kind, doubled, plus one when it is boxed
pub fn pack_free(names: List(Option(CaptureSeed))) -> BitArray {
  use packed, name <- list.fold(names, <<>>)
  case name {
    None -> <<packed:bits, 255>>
    Some(CaptureSeed(boxed:, declared_kind:)) -> {
      let boxed = case boxed {
        True -> 1
        False -> 0
      }
      <<packed:bits, { kind_number(declared_kind) * 2 + boxed }>>
    }
  }
}

pub fn unpack_free(packed: BitArray) -> List(Option(CaptureSeed)) {
  case packed {
    <<255, rest:bits>> -> [None, ..unpack_free(rest)]
    <<byte, rest:bits>> -> [
      Some(CaptureSeed(
        boxed: byte % 2 == 1,
        declared_kind: numbered_kind(byte / 2),
      )),
      ..unpack_free(rest)
    ]
    _ -> []
  }
}

fn kind_number(kind: BindingKind) -> Int {
  case kind {
    VarBinding -> 0
    LetBinding -> 1
    ConstBinding -> 2
    ParamBinding -> 3
    CaptureBinding -> 4
    FnNameBinding -> 5
  }
}

fn numbered_kind(number: Int) -> BindingKind {
  case number {
    0 -> VarBinding
    1 -> LetBinding
    2 -> ConstBinding
    3 -> ParamBinding
    4 -> CaptureBinding
    _ -> FnNameBinding
  }
}

/// which lexical refs are captured, and which of those the parent holds in a
/// box: bit n for the nth of lexical.all_lexical_refs, bit n + 4 when boxed
pub fn pack_lexical(captured: fn(LexicalRef) -> Option(Bool)) -> Int {
  use packed, ref, n <- list.index_fold(lexical.all_lexical_refs, 0)
  case captured(ref) {
    None -> packed
    Some(False) -> int.bitwise_or(packed, int.bitwise_shift_left(1, n))
    Some(True) -> int.bitwise_or(packed, int.bitwise_shift_left(17, n))
  }
}

/// the captured refs in env order, each with whether it is boxed
pub fn unpack_lexical(packed: Int) -> List(#(LexicalRef, Bool)) {
  list.index_map(lexical.all_lexical_refs, fn(ref, n) {
    let bit = fn(at) {
      int.bitwise_and(int.bitwise_shift_right(packed, at), 1) == 1
    }
    #(ref, bit(n), bit(n + 4))
  })
  |> list.filter_map(fn(entry) {
    case entry.1 {
      True -> Ok(#(entry.0, entry.2))
      False -> Error(Nil)
    }
  })
}
