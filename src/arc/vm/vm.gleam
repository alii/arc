import arc/vm/builtins.{type Builtins}
import arc/vm/builtins/array as builtins_array
import arc/vm/builtins/error as builtins_error
import arc/vm/builtins/math as builtins_math
import arc/vm/builtins/object as builtins_object
import arc/vm/builtins/string as builtins_string
import arc/vm/frame.{type FinallyCompletion, type TryFrame, TryFrame}
import arc/vm/heap.{type Heap}
import arc/vm/object
import arc/vm/opcode.{
  type BinOpKind, type FuncTemplate, type Op, type UnaryOpKind, Add, ArrayFrom,
  BinOp, BitAnd, BitNot, BitOr, BitXor, BoxLocal, Call, CallConstructor,
  CallMethod, DefineField, DefineMethod, DeleteElem, DeleteField, Div, Dup, Eq,
  Exp, ForInNext, ForInStart, GetBoxed, GetElem, GetElem2, GetField, GetField2,
  GetGlobal, GetIterator, GetLocal, GetThis, Gt, GtEq, IteratorClose,
  IteratorNext, Jump, JumpIfFalse, JumpIfNullish, JumpIfTrue, LogicalNot, Lt,
  LtEq, MakeClosure, Mod, Mul, Neg, NewObject, NotEq, Pop, Pos, PushConst,
  PushTry, PutBoxed, PutElem, PutField, PutGlobal, PutLocal, Return, ShiftLeft,
  ShiftRight, StrictEq, StrictNotEq, Sub, Swap, TypeOf, TypeofGlobal,
  UShiftRight, UnaryOp, Void,
}
import arc/vm/value.{
  type JsNum, type JsValue, type Ref, ArrayIteratorSlot, ArrayObject,
  DataProperty, Finite, ForInIteratorSlot, FunctionObject, Infinity, JsBigInt,
  JsBool, JsNull, JsNumber, JsObject, JsString, JsSymbol, JsUndefined,
  JsUninitialized, NaN, NativeFunction, NegInfinity, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/string

// ============================================================================
// Public types
// ============================================================================

/// JS-level completion — either normal return or uncaught exception.
pub type Completion {
  NormalCompletion(value: JsValue, heap: Heap)
  ThrowCompletion(value: JsValue, heap: Heap)
}

/// Internal VM error — these are bugs in the VM, not JS-level errors.
pub type VmError {
  /// Tried to read past end of bytecode
  PcOutOfBounds(pc: Int)
  /// Stack underflow
  StackUnderflow(op: String)
  /// Local variable index out of bounds
  LocalIndexOutOfBounds(index: Int)
  /// Unimplemented opcode
  Unimplemented(op: String)
}

// ============================================================================
// Internal state
// ============================================================================

/// A saved caller frame, pushed onto call_stack when Call enters a function.
type SavedFrame {
  SavedFrame(
    func: FuncTemplate,
    locals: List(JsValue),
    stack: List(JsValue),
    pc: Int,
    try_stack: List(TryFrame),
    this_binding: JsValue,
    /// For constructor calls: the newly created object to return if the
    /// constructor doesn't explicitly return an object.
    constructor_this: option.Option(JsValue),
  )
}

/// Internal execution state for the VM loop.
type State {
  State(
    stack: List(JsValue),
    locals: List(JsValue),
    constants: List(JsValue),
    globals: dict.Dict(String, JsValue),
    func: FuncTemplate,
    code: List(Op),
    heap: Heap,
    pc: Int,
    call_stack: List(SavedFrame),
    try_stack: List(TryFrame),
    finally_stack: List(FinallyCompletion),
    builtins: Builtins,
    /// Maps closure heap ref → FuncTemplate, populated at MakeClosure time.
    /// This is needed because a closure's func_index is relative to its
    /// defining parent, which may no longer be on the call stack when called.
    closure_templates: dict.Dict(Int, FuncTemplate),
    /// The current `this` binding. Set by CallMethod/CallConstructor,
    /// defaults to JsUndefined for regular calls.
    this_binding: JsValue,
  )
}

/// Signals from step() — either continue with new state, or stop.
type StepResult {
  Done
  VmError(VmError)
  Thrown
}

// ============================================================================
// Public API
// ============================================================================

/// Run a function template and return its completion + updated heap.
pub fn run(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
) -> Result(Completion, VmError) {
  run_with_globals(func, heap, builtins, dict.new())
}

/// Run a function template with pre-populated global variables.
pub fn run_with_globals(
  func: FuncTemplate,
  heap: Heap,
  builtins: Builtins,
  globals: dict.Dict(String, JsValue),
) -> Result(Completion, VmError) {
  let locals = list.repeat(JsUndefined, func.local_count)
  let state =
    State(
      stack: [],
      locals:,
      constants: func.constants,
      globals:,
      func:,
      code: func.bytecode,
      heap:,
      pc: 0,
      call_stack: [],
      try_stack: [],
      finally_stack: [],
      builtins:,
      closure_templates: dict.new(),
      this_binding: JsUndefined,
    )
  execute(state)
}

// ============================================================================
// Execution loop
// ============================================================================

/// Main execution loop. Tail-recursive.
fn execute(state: State) -> Result(Completion, VmError) {
  case list_get(state.code, state.pc) {
    Error(_) -> {
      // Reached end of bytecode — return top of stack or undefined
      case state.stack {
        [top, ..] -> Ok(NormalCompletion(top, state.heap))
        [] -> Ok(NormalCompletion(JsUndefined, state.heap))
      }
    }
    Ok(op) -> {
      case step(state, op) {
        Ok(new_state) -> execute(new_state)
        Error(#(Done, result, heap)) -> Ok(NormalCompletion(result, heap))
        Error(#(VmError(err), _, _)) -> Error(err)
        Error(#(Thrown, thrown_value, heap)) -> {
          // Try to unwind to a catch handler
          let updated_state = State(..state, heap:)
          case unwind_to_catch(updated_state, thrown_value) {
            Ok(caught_state) -> execute(caught_state)
            Error(_) -> Ok(ThrowCompletion(thrown_value, heap))
          }
        }
      }
    }
  }
}

/// Try to find a catch handler on the try_stack.
/// If found: restore stack to saved depth, push thrown value, jump to catch_target.
/// If not found: return Error(Nil) → uncaught exception.
fn unwind_to_catch(state: State, thrown_value: JsValue) -> Result(State, Nil) {
  case state.try_stack {
    [] -> Error(Nil)
    [TryFrame(catch_target:, stack_depth:), ..rest_try] -> {
      let restored_stack = truncate_stack(state.stack, stack_depth)
      Ok(
        State(
          ..state,
          stack: [thrown_value, ..restored_stack],
          try_stack: rest_try,
          pc: catch_target,
        ),
      )
    }
  }
}

/// Truncate stack to a given depth.
fn truncate_stack(stack: List(JsValue), depth: Int) -> List(JsValue) {
  case list.length(stack) > depth {
    True -> truncate_stack(list.drop(stack, 1), depth)
    False -> stack
  }
}

// ============================================================================
// Step — single instruction dispatch
// ============================================================================

/// Execute a single instruction. Returns Ok(new_state) to continue,
/// or Error(#(signal, value, heap)) to stop.
fn step(state: State, op: Op) -> Result(State, #(StepResult, JsValue, Heap)) {
  case op {
    PushConst(index) -> {
      case list_get(state.constants, index) {
        Ok(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        Error(_) -> {
          let #(heap, err) =
            object.make_range_error(
              state.heap,
              state.builtins,
              "constant index out of bounds: " <> int.to_string(index),
            )
          Error(#(Thrown, err, heap))
        }
      }
    }

    Pop -> {
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        [] -> Error(#(VmError(StackUnderflow("Pop")), JsUndefined, state.heap))
      }
    }

    Dup -> {
      case state.stack {
        [top, ..] ->
          Ok(State(..state, stack: [top, ..state.stack], pc: state.pc + 1))
        [] -> Error(#(VmError(StackUnderflow("Dup")), JsUndefined, state.heap))
      }
    }

    Swap -> {
      case state.stack {
        [a, b, ..rest] ->
          Ok(State(..state, stack: [b, a, ..rest], pc: state.pc + 1))
        _ -> Error(#(VmError(StackUnderflow("Swap")), JsUndefined, state.heap))
      }
    }

    GetLocal(index) -> {
      case list_get(state.locals, index) {
        Ok(JsUninitialized) -> {
          let #(heap, err) =
            object.make_reference_error(
              state.heap,
              state.builtins,
              "Cannot access variable before initialization (TDZ)",
            )
          Error(#(Thrown, err, heap))
        }
        Ok(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        Error(_) ->
          Error(#(
            VmError(LocalIndexOutOfBounds(index)),
            JsUndefined,
            state.heap,
          ))
      }
    }

    PutLocal(index) -> {
      case state.stack {
        [value, ..rest] -> {
          case list_set(state.locals, index, value) {
            Ok(new_locals) ->
              Ok(
                State(
                  ..state,
                  stack: rest,
                  locals: new_locals,
                  pc: state.pc + 1,
                ),
              )
            Error(_) ->
              Error(#(
                VmError(LocalIndexOutOfBounds(index)),
                JsUndefined,
                state.heap,
              ))
          }
        }
        [] ->
          Error(#(VmError(StackUnderflow("PutLocal")), JsUndefined, state.heap))
      }
    }

    GetGlobal(name) -> {
      case dict.get(state.globals, name) {
        Ok(value) ->
          Ok(State(..state, stack: [value, ..state.stack], pc: state.pc + 1))
        Error(_) ->
          Ok(
            State(
              ..state,
              stack: [JsUndefined, ..state.stack],
              pc: state.pc + 1,
            ),
          )
      }
    }

    PutGlobal(name) -> {
      case state.stack {
        [value, ..rest] ->
          Ok(
            State(
              ..state,
              stack: rest,
              globals: dict.insert(state.globals, name, value),
              pc: state.pc + 1,
            ),
          )
        [] ->
          Error(#(VmError(StackUnderflow("PutGlobal")), JsUndefined, state.heap))
      }
    }

    TypeOf -> {
      case state.stack {
        [val, ..rest] -> {
          let type_str = typeof_value(val, state.heap)
          Ok(
            State(
              ..state,
              stack: [JsString(type_str), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        [] ->
          Error(#(VmError(StackUnderflow("TypeOf")), JsUndefined, state.heap))
      }
    }

    TypeofGlobal(name) -> {
      let val = case dict.get(state.globals, name) {
        Ok(v) -> v
        Error(_) -> JsUndefined
      }
      let type_str = typeof_value(val, state.heap)
      Ok(
        State(
          ..state,
          stack: [JsString(type_str), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    BinOp(kind) -> {
      case state.stack {
        [right, left, ..rest] -> {
          // instanceof and in need heap access
          case kind {
            opcode.InstanceOf -> {
              case js_instanceof(state.heap, left, right) {
                Ok(result) ->
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(result), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                Error(msg) -> {
                  let #(heap, err) =
                    object.make_type_error(state.heap, state.builtins, msg)
                  Error(#(Thrown, err, heap))
                }
              }
            }
            opcode.In -> {
              // left = key, right = object
              case right {
                JsObject(ref) -> {
                  let key_str = to_js_string(left)
                  let result = object.has_property(state.heap, ref, key_str)
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(result), ..rest],
                      pc: state.pc + 1,
                    ),
                  )
                }
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      "Cannot use 'in' operator to search for '"
                        <> to_js_string(left)
                        <> "' in "
                        <> to_js_string(right),
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            _ ->
              case exec_binop(kind, left, right) {
                Ok(result) ->
                  Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
                Error(msg) -> {
                  let #(heap, err) =
                    object.make_type_error(state.heap, state.builtins, msg)
                  Error(#(Thrown, err, heap))
                }
              }
          }
        }
        _ -> Error(#(VmError(StackUnderflow("BinOp")), JsUndefined, state.heap))
      }
    }

    UnaryOp(kind) -> {
      case state.stack {
        [operand, ..rest] -> {
          case exec_unaryop(kind, operand) {
            Ok(result) ->
              Ok(State(..state, stack: [result, ..rest], pc: state.pc + 1))
            Error(msg) -> {
              let #(heap, err) =
                object.make_type_error(state.heap, state.builtins, msg)
              Error(#(Thrown, err, heap))
            }
          }
        }
        [] ->
          Error(#(VmError(StackUnderflow("UnaryOp")), JsUndefined, state.heap))
      }
    }

    Return -> {
      let return_value = case state.stack {
        [value, ..] -> value
        [] -> JsUndefined
      }
      case state.call_stack {
        // No caller — top-level return, we're done
        [] -> Error(#(Done, return_value, state.heap))
        // Pop call frame, restore caller, push return value onto caller's stack
        [
          SavedFrame(
            func:,
            locals:,
            stack:,
            pc:,
            try_stack:,
            this_binding: saved_this,
            constructor_this:,
          ),
          ..rest_frames
        ] -> {
          // Constructor return semantics: if constructor_this is Some(obj),
          // use the constructed object unless the function explicitly returned
          // an object/function.
          let effective_return = case constructor_this {
            Some(constructed_obj) ->
              case return_value {
                // If constructor returns an object, use that instead
                JsObject(_) -> return_value
                // Otherwise use the constructed object
                _ -> constructed_obj
              }
            None -> return_value
          }
          Ok(
            State(
              ..state,
              stack: [effective_return, ..stack],
              locals:,
              func:,
              code: func.bytecode,
              constants: func.constants,
              pc:,
              call_stack: rest_frames,
              try_stack:,
              this_binding: saved_this,
            ),
          )
        }
      }
    }

    MakeClosure(func_index) -> {
      case list_get(state.func.functions, func_index) {
        Ok(child_template) -> {
          // Capture values from current frame according to env_descriptors.
          // For boxed captured vars, the local holds a JsObject(box_ref) —
          // copying that ref means the closure shares the same BoxSlot.
          let captured_values =
            list.map(child_template.env_descriptors, fn(desc) {
              case desc {
                opcode.CaptureLocal(parent_index) ->
                  case list_get(state.locals, parent_index) {
                    Ok(val) -> val
                    Error(_) -> JsUndefined
                  }
                opcode.CaptureEnv(_parent_env_index) ->
                  // Transitive capture not yet implemented
                  JsUndefined
              }
            })
          let #(heap, env_ref) =
            heap.alloc(state.heap, value.EnvSlot(captured_values))
          // Set .name from the template
          let fn_name = case child_template.name {
            Some(n) -> JsString(n)
            None -> JsString("")
          }
          // For non-arrow functions, pre-populate .prototype with a fresh object
          // so that `Foo.prototype.bar = ...` and `new Foo()` work.
          // .constructor on prototype is set after we have the closure ref.
          let #(heap, fn_properties, proto_ref) = case child_template.is_arrow {
            True -> #(
              heap,
              dict.from_list([
                // "name" on functions: not writable, not enumerable, configurable
                #(
                  "name",
                  DataProperty(
                    value: fn_name,
                    writable: False,
                    enumerable: False,
                    configurable: True,
                  ),
                ),
              ]),
              None,
            )
            False -> {
              let #(h, proto_obj_ref) =
                heap.alloc(
                  heap,
                  ObjectSlot(
                    kind: OrdinaryObject,
                    properties: dict.new(),
                    elements: dict.new(),
                    prototype: Some(state.builtins.object.prototype),
                  ),
                )
              #(
                h,
                dict.from_list([
                  // "prototype" on functions: writable, not enumerable, not configurable
                  #(
                    "prototype",
                    DataProperty(
                      value: JsObject(proto_obj_ref),
                      writable: True,
                      enumerable: False,
                      configurable: False,
                    ),
                  ),
                  // "name" on functions: not writable, not enumerable, configurable
                  #(
                    "name",
                    DataProperty(
                      value: fn_name,
                      writable: False,
                      enumerable: False,
                      configurable: True,
                    ),
                  ),
                ]),
                Some(proto_obj_ref),
              )
            }
          }
          let #(heap, closure_ref) =
            heap.alloc(
              heap,
              ObjectSlot(
                kind: FunctionObject(func_index:, env: env_ref),
                properties: fn_properties,
                elements: dict.new(),
                prototype: Some(state.builtins.function.prototype),
              ),
            )
          // Set .constructor on the prototype pointing back to this function
          let heap = case proto_ref {
            Some(pr) ->
              case heap.read(heap, pr) {
                Ok(ObjectSlot(kind:, properties: props, elements:, prototype:)) ->
                  heap.write(
                    heap,
                    pr,
                    ObjectSlot(
                      kind:,
                      properties: dict.insert(
                        props,
                        "constructor",
                        value.builtin_property(JsObject(closure_ref)),
                      ),
                      elements:,
                      prototype:,
                    ),
                  )
                _ -> heap
              }
            None -> heap
          }
          // Cache the template so it can be found when the closure is called
          // from a different scope (after the defining function has returned)
          let closure_templates =
            dict.insert(state.closure_templates, closure_ref.id, child_template)
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(closure_ref), ..state.stack],
              pc: state.pc + 1,
              closure_templates:,
            ),
          )
        }
        Error(_) -> {
          let #(heap, err) =
            object.make_range_error(
              state.heap,
              state.builtins,
              "invalid function index: " <> int.to_string(func_index),
            )
          Error(#(Thrown, err, heap))
        }
      }
    }

    BoxLocal(index) -> {
      // Wrap the current value in locals[index] into a BoxSlot on the heap.
      // Replace the local with a JsObject(box_ref).
      case list_get(state.locals, index) {
        Ok(current_value) -> {
          let #(heap, box_ref) =
            heap.alloc(state.heap, value.BoxSlot(current_value))
          case list_set(state.locals, index, JsObject(box_ref)) {
            Ok(locals) -> Ok(State(..state, heap:, locals:, pc: state.pc + 1))
            Error(_) ->
              Error(#(VmError(LocalIndexOutOfBounds(index)), JsUndefined, heap))
          }
        }
        Error(_) ->
          Error(#(
            VmError(LocalIndexOutOfBounds(index)),
            JsUndefined,
            state.heap,
          ))
      }
    }

    GetBoxed(index) -> {
      // Read locals[index] (a JsObject(box_ref)), dereference BoxSlot, push value.
      case list_get(state.locals, index) {
        Ok(JsObject(box_ref)) -> {
          case heap.read(state.heap, box_ref) {
            Ok(value.BoxSlot(val)) ->
              Ok(State(..state, stack: [val, ..state.stack], pc: state.pc + 1))
            _ ->
              Error(#(
                VmError(Unimplemented("GetBoxed: not a BoxSlot")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        _ ->
          Error(#(
            VmError(Unimplemented("GetBoxed: local is not a box ref")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    PutBoxed(index) -> {
      // Pop value from stack, write into the BoxSlot pointed to by locals[index].
      case state.stack {
        [new_value, ..rest_stack] -> {
          case list_get(state.locals, index) {
            Ok(JsObject(box_ref)) -> {
              let heap =
                heap.write(state.heap, box_ref, value.BoxSlot(new_value))
              Ok(State(..state, heap:, stack: rest_stack, pc: state.pc + 1))
            }
            _ ->
              Error(#(
                VmError(Unimplemented("PutBoxed: local is not a box ref")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        [] ->
          Error(#(VmError(StackUnderflow("PutBoxed")), JsUndefined, state.heap))
      }
    }

    Call(arity) -> {
      // Stack layout: [arg_n, ..., arg_1, callee, ...rest]
      // Pop arity args, then callee
      case pop_n(state.stack, arity) {
        Ok(#(args, after_args)) -> {
          case after_args {
            [JsObject(obj_ref), ..rest_stack] -> {
              case heap.read(state.heap, obj_ref) {
                Ok(ObjectSlot(
                  kind: FunctionObject(func_index: _, env: env_ref),
                  ..,
                )) ->
                  call_function(
                    state,
                    obj_ref,
                    env_ref,
                    args,
                    rest_stack,
                    JsUndefined,
                    None,
                  )
                Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
                  call_native(state, native, args, rest_stack, JsUndefined)
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      "object is not a function",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            [non_func, ..] -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  typeof_value(non_func, state.heap) <> " is not a function",
                )
              Error(#(Thrown, err, heap))
            }
            [] ->
              Error(#(
                VmError(StackUnderflow("Call: no callee")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        Error(_) ->
          Error(#(
            VmError(StackUnderflow("Call: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    Jump(target) -> Ok(State(..state, pc: target))

    JumpIfFalse(target) -> {
      case state.stack {
        [value, ..rest] -> {
          case is_truthy(value) {
            True -> Ok(State(..state, stack: rest, pc: state.pc + 1))
            False -> Ok(State(..state, stack: rest, pc: target))
          }
        }
        [] ->
          Error(#(
            VmError(StackUnderflow("JumpIfFalse")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    JumpIfTrue(target) -> {
      case state.stack {
        [value, ..rest] -> {
          case is_truthy(value) {
            True -> Ok(State(..state, stack: rest, pc: target))
            False -> Ok(State(..state, stack: rest, pc: state.pc + 1))
          }
        }
        [] ->
          Error(#(
            VmError(StackUnderflow("JumpIfTrue")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    JumpIfNullish(target) -> {
      case state.stack {
        [value, ..rest] -> {
          case value {
            JsNull | JsUndefined -> Ok(State(..state, stack: rest, pc: target))
            _ -> Ok(State(..state, stack: rest, pc: state.pc + 1))
          }
        }
        [] ->
          Error(#(
            VmError(StackUnderflow("JumpIfNullish")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Exception handling --
    PushTry(catch_target) -> {
      let frame = TryFrame(catch_target:, stack_depth: list.length(state.stack))
      Ok(
        State(..state, try_stack: [frame, ..state.try_stack], pc: state.pc + 1),
      )
    }

    opcode.PopTry -> {
      case state.try_stack {
        [_, ..rest] -> Ok(State(..state, try_stack: rest, pc: state.pc + 1))
        [] ->
          Error(#(
            VmError(StackUnderflow("PopTry: empty try_stack")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    opcode.Throw -> {
      case state.stack {
        [value, ..] -> Error(#(Thrown, value, state.heap))
        [] ->
          Error(#(VmError(StackUnderflow("Throw")), JsUndefined, state.heap))
      }
    }

    opcode.EnterFinally -> {
      Ok(
        State(
          ..state,
          finally_stack: [frame.NormalCompletion, ..state.finally_stack],
          pc: state.pc + 1,
        ),
      )
    }

    opcode.LeaveFinally -> {
      case state.finally_stack {
        [frame.NormalCompletion, ..rest] ->
          Ok(State(..state, finally_stack: rest, pc: state.pc + 1))
        [frame.ThrowCompletion(value:), ..] ->
          Error(#(Thrown, value, state.heap))
        [frame.ReturnCompletion(value:), ..] ->
          Error(#(Done, value, state.heap))
        [] ->
          Error(#(
            VmError(StackUnderflow("LeaveFinally: empty finally_stack")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Property access --
    NewObject -> {
      let #(heap, ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: dict.new(),
            prototype: Some(state.builtins.object.prototype),
          ),
        )
      Ok(
        State(
          ..state,
          heap:,
          stack: [JsObject(ref), ..state.stack],
          pc: state.pc + 1,
        ),
      )
    }

    GetField(name) -> {
      case state.stack {
        [JsNull, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of null (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsUndefined, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of undefined (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsObject(ref), ..rest] -> {
          let val = case object.get_property(state.heap, ref, name) {
            Ok(v) -> v
            Error(_) -> JsUndefined
          }
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        }
        [JsString(s), ..rest] -> {
          let val =
            get_string_field(s, name, state.heap, state.builtins.string_proto)
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        }
        [_, ..rest] -> {
          // Number/Bool/other primitives: no prototype yet
          Ok(State(..state, stack: [JsUndefined, ..rest], pc: state.pc + 1))
        }
        [] ->
          Error(#(VmError(StackUnderflow("GetField")), JsUndefined, state.heap))
      }
    }

    PutField(name) -> {
      // Consumes [value, obj] and pushes value back (assignment is an expression).
      // Consistent with PutElem which also leaves the value on the stack.
      case state.stack {
        [value, JsObject(ref), ..rest] -> {
          let heap = object.set_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [value, ..rest], pc: state.pc + 1))
        }
        [value, _, ..rest] -> {
          // PutField on non-object: silently ignore, still return value
          Ok(State(..state, stack: [value, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(VmError(StackUnderflow("PutField")), JsUndefined, state.heap))
      }
    }

    DefineField(name) -> {
      // Like PutField but keeps the object on the stack (for object literal construction)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let heap = object.set_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> {
          // DefineField on non-object: no-op, keep object on stack
          Ok(State(..state, pc: state.pc + 1))
        }
        _ ->
          Error(#(
            VmError(StackUnderflow("DefineField")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DefineMethod(name) -> {
      // Like DefineField but creates a non-enumerable property (for class methods)
      case state.stack {
        [value, JsObject(ref) as obj, ..rest] -> {
          let heap = object.define_method_property(state.heap, ref, name, value)
          Ok(State(..state, heap:, stack: [obj, ..rest], pc: state.pc + 1))
        }
        [_, _, ..] -> Ok(State(..state, pc: state.pc + 1))
        _ ->
          Error(#(
            VmError(StackUnderflow("DefineMethod")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Array construction --
    ArrayFrom(count) -> {
      case pop_n(state.stack, count) {
        Ok(#(elements, rest)) -> {
          // elements are in order [first, ..., last]
          let elements_dict =
            list.index_map(elements, fn(val, idx) { #(idx, val) })
            |> dict.from_list()
          let #(heap, ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: ArrayObject(count),
                properties: dict.new(),
                elements: elements_dict,
                prototype: Some(state.builtins.array.prototype),
              ),
            )
          Ok(
            State(
              ..state,
              heap:,
              stack: [JsObject(ref), ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        Error(_) ->
          Error(#(VmError(StackUnderflow("ArrayFrom")), JsUndefined, state.heap))
      }
    }

    // -- Computed property access --
    GetElem -> {
      case state.stack {
        [key, JsObject(ref), ..rest] -> {
          let val = get_elem_value(state.heap, ref, key)
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        }
        [_, JsNull, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of null",
            )
          Error(#(Thrown, err, heap))
        }
        [_, JsUndefined, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of undefined",
            )
          Error(#(Thrown, err, heap))
        }
        [key, JsString(s), ..rest] -> {
          let val =
            get_string_elem(s, key, state.heap, state.builtins.string_proto)
          Ok(State(..state, stack: [val, ..rest], pc: state.pc + 1))
        }
        [_, _, ..rest] -> {
          // Non-object: return undefined
          Ok(State(..state, stack: [JsUndefined, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(VmError(StackUnderflow("GetElem")), JsUndefined, state.heap))
      }
    }

    GetElem2 -> {
      // Like GetElem but keeps obj+key on stack: [key, obj, ...] -> [value, key, obj, ...]
      case state.stack {
        [key, JsObject(ref) as obj, ..rest] -> {
          let val = get_elem_value(state.heap, ref, key)
          Ok(State(..state, stack: [val, key, obj, ..rest], pc: state.pc + 1))
        }
        [key, JsString(s) as str, ..rest] -> {
          let val =
            get_string_elem(s, key, state.heap, state.builtins.string_proto)
          Ok(State(..state, stack: [val, key, str, ..rest], pc: state.pc + 1))
        }
        _ ->
          Error(#(VmError(StackUnderflow("GetElem2")), JsUndefined, state.heap))
      }
    }

    PutElem -> {
      // Stack: [value, key, obj, ...rest]
      case state.stack {
        [val, key, JsObject(ref), ..rest] -> {
          let heap = put_elem_value(state.heap, ref, key, val)
          Ok(State(..state, heap:, stack: [val, ..rest], pc: state.pc + 1))
        }
        [_, _, _, ..rest] -> {
          // PutElem on non-object: silently ignore (JS sloppy mode)
          Ok(State(..state, stack: rest, pc: state.pc + 1))
        }
        _ ->
          Error(#(VmError(StackUnderflow("PutElem")), JsUndefined, state.heap))
      }
    }

    // -- this / method calls / constructors --
    GetThis ->
      Ok(
        State(
          ..state,
          stack: [state.this_binding, ..state.stack],
          pc: state.pc + 1,
        ),
      )

    GetField2(name) -> {
      // Like GetField but keeps the object on the stack for CallMethod.
      // Stack: [obj, ..rest] → [prop_value, obj, ..rest]
      case state.stack {
        [JsNull, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of null (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsUndefined, ..] -> {
          let #(heap, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Cannot read properties of undefined (reading '" <> name <> "')",
            )
          Error(#(Thrown, err, heap))
        }
        [JsObject(ref) as obj, ..rest] -> {
          let val = case object.get_property(state.heap, ref, name) {
            Ok(v) -> v
            Error(_) -> JsUndefined
          }
          Ok(State(..state, stack: [val, obj, ..rest], pc: state.pc + 1))
        }
        [JsString(s) as str, ..rest] -> {
          let val =
            get_string_field(s, name, state.heap, state.builtins.string_proto)
          Ok(State(..state, stack: [val, str, ..rest], pc: state.pc + 1))
        }
        [other, ..rest] -> {
          // Non-object: property is undefined, keep object on stack
          Ok(
            State(
              ..state,
              stack: [JsUndefined, other, ..rest],
              pc: state.pc + 1,
            ),
          )
        }
        [] ->
          Error(#(VmError(StackUnderflow("GetField2")), JsUndefined, state.heap))
      }
    }

    CallMethod(_name, arity) -> {
      // Stack: [arg_n, ..., arg_1, method, receiver, ...rest]
      // Pop arity args, then method, then receiver
      case pop_n(state.stack, arity) {
        Ok(#(args, after_args)) -> {
          case after_args {
            [JsObject(method_ref), receiver, ..rest_stack] -> {
              case heap.read(state.heap, method_ref) {
                Ok(ObjectSlot(
                  kind: FunctionObject(func_index: _, env: env_ref),
                  ..,
                )) ->
                  call_function(
                    state,
                    method_ref,
                    env_ref,
                    args,
                    rest_stack,
                    // Method call: this = receiver
                    receiver,
                    None,
                  )
                Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
                  call_native(state, native, args, rest_stack, receiver)
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      "is not a function",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            [non_func, _, ..] -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  typeof_value(non_func, state.heap) <> " is not a function",
                )
              Error(#(Thrown, err, heap))
            }
            _ ->
              Error(#(
                VmError(StackUnderflow("CallMethod")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        Error(_) ->
          Error(#(
            VmError(StackUnderflow("CallMethod: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    CallConstructor(arity) -> {
      // Stack: [arg_n, ..., arg_1, constructor, ...rest]
      case pop_n(state.stack, arity) {
        Ok(#(args, after_args)) -> {
          case after_args {
            [JsObject(ctor_ref), ..rest_stack] -> {
              case heap.read(state.heap, ctor_ref) {
                Ok(ObjectSlot(
                  kind: FunctionObject(func_index: _, env: env_ref),
                  properties:,
                  ..,
                )) -> {
                  // Read the constructor's .prototype property for the new object's __proto__
                  let proto = case dict.get(properties, "prototype") {
                    Ok(DataProperty(value: JsObject(proto_ref), ..)) ->
                      Some(proto_ref)
                    _ -> Some(state.builtins.object.prototype)
                  }
                  // Create the new object (the `this` for the constructor)
                  let #(heap, new_obj_ref) =
                    heap.alloc(
                      state.heap,
                      ObjectSlot(
                        kind: OrdinaryObject,
                        properties: dict.new(),
                        elements: dict.new(),
                        prototype: proto,
                      ),
                    )
                  let new_obj = JsObject(new_obj_ref)
                  call_function(
                    State(..state, heap:),
                    ctor_ref,
                    env_ref,
                    args,
                    rest_stack,
                    // Constructor: this = new object
                    new_obj,
                    Some(new_obj),
                  )
                }
                // Bound function used as constructor: resolve target, prepend args
                Ok(ObjectSlot(
                  kind: NativeFunction(value.NativeBoundFunction(
                    target:,
                    bound_args:,
                    ..,
                  )),
                  ..,
                )) -> {
                  let final_args = list.append(bound_args, args)
                  construct_value(state, target, final_args, rest_stack)
                }
                Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
                  call_native(state, native, args, rest_stack, JsUndefined)
                _ -> {
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      "is not a constructor",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            }
            [non_func, ..] -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  typeof_value(non_func, state.heap) <> " is not a constructor",
                )
              Error(#(Thrown, err, heap))
            }
            _ ->
              Error(#(
                VmError(StackUnderflow("CallConstructor")),
                JsUndefined,
                state.heap,
              ))
          }
        }
        Error(_) ->
          Error(#(
            VmError(StackUnderflow("CallConstructor: not enough args")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Iteration --
    ForInStart -> {
      case state.stack {
        [obj, ..rest] -> {
          // Collect enumerable keys from the object (or empty for non-objects)
          let keys = case obj {
            JsObject(ref) -> object.enumerate_keys(state.heap, ref)
            // for-in on null/undefined produces no iterations
            JsNull | JsUndefined -> []
            // Primitives: no enumerable properties
            _ -> []
          }
          // Wrap string keys as JsString values for ForInIteratorSlot
          let key_values = list.map(keys, JsString)
          let #(heap, iter_ref) =
            heap.alloc(state.heap, ForInIteratorSlot(keys: key_values))
          Ok(
            State(
              ..state,
              stack: [JsObject(iter_ref), ..rest],
              heap:,
              pc: state.pc + 1,
            ),
          )
        }
        _ ->
          Error(#(
            VmError(StackUnderflow("ForInStart")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    ForInNext -> {
      case state.stack {
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            Ok(ForInIteratorSlot(keys:)) ->
              case keys {
                [val, ..remaining] -> {
                  // Advance the iterator
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ForInIteratorSlot(keys: remaining),
                    )
                  // Push: iterator stays, key, done=false
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
                [] -> {
                  // No more keys — push undefined + done=true
                  Ok(
                    State(
                      ..state,
                      stack: [
                        JsBool(True),
                        JsUndefined,
                        JsObject(iter_ref),
                        ..rest
                      ],
                      pc: state.pc + 1,
                    ),
                  )
                }
              }
            _ ->
              Error(#(
                VmError(Unimplemented("ForInNext: not a ForInIteratorSlot")),
                JsUndefined,
                state.heap,
              ))
          }
        _ ->
          Error(#(VmError(StackUnderflow("ForInNext")), JsUndefined, state.heap))
      }
    }

    GetIterator -> {
      case state.stack {
        [iterable, ..rest] ->
          case iterable {
            JsObject(ref) ->
              case heap.read(state.heap, ref) {
                Ok(ObjectSlot(kind: ArrayObject(_), ..)) -> {
                  // Lazy array iterator — stores source ref + index, reads elements one at a time
                  let #(heap, iter_ref) =
                    heap.alloc(
                      state.heap,
                      ArrayIteratorSlot(source: ref, index: 0),
                    )
                  Ok(
                    State(
                      ..state,
                      stack: [JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
                _ -> {
                  // Non-array object — throw TypeError
                  let #(heap, err) =
                    object.make_type_error(
                      state.heap,
                      state.builtins,
                      "object is not iterable",
                    )
                  Error(#(Thrown, err, heap))
                }
              }
            _ -> {
              let #(heap, err) =
                object.make_type_error(
                  state.heap,
                  state.builtins,
                  typeof_value(iterable, state.heap) <> " is not iterable",
                )
              Error(#(Thrown, err, heap))
            }
          }
        _ ->
          Error(#(
            VmError(StackUnderflow("GetIterator")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    IteratorNext -> {
      case state.stack {
        [JsObject(iter_ref), ..rest] ->
          case heap.read(state.heap, iter_ref) {
            Ok(ArrayIteratorSlot(source:, index:)) -> {
              // Re-read the array length each time (handles mutations during iteration)
              let #(length, elements) = case heap.read(state.heap, source) {
                Ok(ObjectSlot(kind: ArrayObject(len), elements: elems, ..)) -> #(
                  len,
                  elems,
                )
                _ -> #(0, dict.new())
              }
              case index >= length {
                True ->
                  // Done — push undefined + done=true
                  Ok(
                    State(
                      ..state,
                      stack: [
                        JsBool(True),
                        JsUndefined,
                        JsObject(iter_ref),
                        ..rest
                      ],
                      pc: state.pc + 1,
                    ),
                  )
                False -> {
                  // Read element at current index
                  let val = case dict.get(elements, index) {
                    Ok(v) -> v
                    Error(_) -> JsUndefined
                  }
                  // Advance iterator index
                  let heap =
                    heap.write(
                      state.heap,
                      iter_ref,
                      ArrayIteratorSlot(source:, index: index + 1),
                    )
                  Ok(
                    State(
                      ..state,
                      stack: [JsBool(False), val, JsObject(iter_ref), ..rest],
                      heap:,
                      pc: state.pc + 1,
                    ),
                  )
                }
              }
            }
            _ ->
              Error(#(
                VmError(Unimplemented("IteratorNext: not an ArrayIteratorSlot")),
                JsUndefined,
                state.heap,
              ))
          }
        _ ->
          Error(#(
            VmError(StackUnderflow("IteratorNext")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    IteratorClose -> {
      // MVP: just pop the iterator from the stack
      case state.stack {
        [_, ..rest] -> Ok(State(..state, stack: rest, pc: state.pc + 1))
        _ ->
          Error(#(
            VmError(StackUnderflow("IteratorClose")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    // -- Delete operator --
    DeleteField(name) -> {
      case state.stack {
        [obj, ..rest] ->
          case obj {
            JsObject(ref) -> {
              let #(heap, success) =
                object.delete_property(state.heap, ref, name)
              Ok(
                State(
                  ..state,
                  stack: [JsBool(success), ..rest],
                  heap:,
                  pc: state.pc + 1,
                ),
              )
            }
            // delete on non-object returns true
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ ->
          Error(#(
            VmError(StackUnderflow("DeleteField")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    DeleteElem -> {
      case state.stack {
        [key, obj, ..rest] ->
          case obj {
            JsObject(ref) -> {
              let key_str = to_js_string(key)
              let #(heap, success) =
                object.delete_property(state.heap, ref, key_str)
              Ok(
                State(
                  ..state,
                  stack: [JsBool(success), ..rest],
                  heap:,
                  pc: state.pc + 1,
                ),
              )
            }
            _ ->
              Ok(
                State(..state, stack: [JsBool(True), ..rest], pc: state.pc + 1),
              )
          }
        _ ->
          Error(#(
            VmError(StackUnderflow("DeleteElem")),
            JsUndefined,
            state.heap,
          ))
      }
    }

    _ ->
      Error(#(
        VmError(Unimplemented("opcode: " <> string.inspect(op))),
        JsUndefined,
        state.heap,
      ))
  }
}

// ============================================================================
// Call helpers
// ============================================================================

/// Shared logic for Call, CallMethod, and CallConstructor.
/// Looks up the callee template, saves the caller frame, sets up locals,
/// and transitions to the callee's code.
fn call_function(
  state: State,
  callee_ref: value.Ref,
  env_ref: value.Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this_val: JsValue,
  constructor_this: option.Option(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case dict.get(state.closure_templates, callee_ref.id) {
    Ok(callee_template) -> {
      // Save caller frame
      let saved =
        SavedFrame(
          func: state.func,
          locals: state.locals,
          stack: rest_stack,
          pc: state.pc + 1,
          try_stack: state.try_stack,
          this_binding: state.this_binding,
          constructor_this:,
        )
      // Read captured values from env
      let env_values = case heap.read(state.heap, env_ref) {
        Ok(value.EnvSlot(slots)) -> slots
        _ -> []
      }
      let env_count = list.length(env_values)
      let padded_args = pad_args(args, callee_template.arity)
      let remaining =
        callee_template.local_count - env_count - callee_template.arity
      let locals =
        list.flatten([
          env_values,
          padded_args,
          list.repeat(JsUndefined, remaining),
        ])
      // Arrow functions inherit this from their enclosing scope
      let new_this = case callee_template.is_arrow {
        True -> state.this_binding
        False -> this_val
      }
      Ok(
        State(
          ..state,
          stack: [],
          locals:,
          func: callee_template,
          code: callee_template.bytecode,
          constants: callee_template.constants,
          pc: 0,
          call_stack: [saved, ..state.call_stack],
          try_stack: [],
          this_binding: new_this,
        ),
      )
    }
    Error(_) -> {
      let #(heap, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "closure template not found",
        )
      Error(#(Thrown, err, heap))
    }
  }
}

/// Call a native (Gleam-implemented) function. Most natives execute synchronously
/// and push their result onto the stack. However, call/apply/bind need special
/// handling because they invoke other functions (potentially pushing call frames).
fn call_native(
  state: State,
  native: value.NativeFn,
  args: List(JsValue),
  rest_stack: List(JsValue),
  this: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case native {
    // Function.prototype.call(thisArg, ...args)
    // `this` is the target function, args[0] is the thisArg
    value.NativeFunctionCall -> {
      let #(this_arg, call_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(JsUndefined, [])
      }
      call_value(State(..state, stack: rest_stack), this, call_args, this_arg)
    }
    // Function.prototype.apply(thisArg, argsArray)
    // `this` is the target function, args[0] is thisArg, args[1] is array
    value.NativeFunctionApply -> {
      let this_arg = case args {
        [t, ..] -> t
        _ -> JsUndefined
      }
      let call_args = case args {
        [_, JsObject(arr_ref), ..] -> extract_array_args(state.heap, arr_ref)
        // null/undefined argsArray → no args
        _ -> []
      }
      call_value(State(..state, stack: rest_stack), this, call_args, this_arg)
    }
    // Function.prototype.bind(thisArg, ...args)
    // Creates a bound function object
    value.NativeFunctionBind -> {
      let #(this_arg, bound_args) = case args {
        [t, ..rest] -> #(t, rest)
        [] -> #(JsUndefined, [])
      }
      case this {
        JsObject(target_ref) -> {
          // Get the target's name for "bound <name>"
          let name = case heap.read(state.heap, target_ref) {
            Ok(ObjectSlot(properties:, ..)) ->
              case dict.get(properties, "name") {
                Ok(DataProperty(value: JsString(n), ..)) -> "bound " <> n
                _ -> "bound "
              }
            _ -> "bound "
          }
          let #(h, bound_ref) =
            heap.alloc(
              state.heap,
              ObjectSlot(
                kind: NativeFunction(value.NativeBoundFunction(
                  target: target_ref,
                  bound_this: this_arg,
                  bound_args:,
                )),
                properties: dict.from_list([
                  #("name", value.builtin_property(JsString(name))),
                ]),
                elements: dict.new(),
                prototype: Some(state.builtins.function.prototype),
              ),
            )
          Ok(
            State(
              ..state,
              heap: h,
              stack: [JsObject(bound_ref), ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        }
        _ -> {
          let #(h, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "Bind must be called on a function",
            )
          Error(#(Thrown, err, h))
        }
      }
    }
    // Bound function: prepend bound_args, use bound_this
    value.NativeBoundFunction(target:, bound_this:, bound_args:) -> {
      let final_args = list.append(bound_args, args)
      call_value(
        State(..state, stack: rest_stack),
        JsObject(target),
        final_args,
        bound_this,
      )
    }
    // All other native functions: synchronous dispatch
    _ -> {
      let #(h, result) = dispatch_native(native, args, this, state)
      case result {
        Ok(return_value) ->
          Ok(
            State(
              ..state,
              heap: h,
              stack: [return_value, ..rest_stack],
              pc: state.pc + 1,
            ),
          )
        Error(thrown) -> Error(#(Thrown, thrown, h))
      }
    }
  }
}

/// Construct a new object using the target function ref.
/// Used by CallConstructor when the constructor is a bound function.
fn construct_value(
  state: State,
  target_ref: Ref,
  args: List(JsValue),
  rest_stack: List(JsValue),
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case heap.read(state.heap, target_ref) {
    Ok(ObjectSlot(
      kind: FunctionObject(func_index: _, env: env_ref),
      properties:,
      ..,
    )) -> {
      let proto = case dict.get(properties, "prototype") {
        Ok(DataProperty(value: JsObject(proto_ref), ..)) -> Some(proto_ref)
        _ -> Some(state.builtins.object.prototype)
      }
      let #(h, new_obj_ref) =
        heap.alloc(
          state.heap,
          ObjectSlot(
            kind: OrdinaryObject,
            properties: dict.new(),
            elements: dict.new(),
            prototype: proto,
          ),
        )
      let new_obj = JsObject(new_obj_ref)
      call_function(
        State(..state, heap: h),
        target_ref,
        env_ref,
        args,
        rest_stack,
        new_obj,
        Some(new_obj),
      )
    }
    // Chained bound function: resolve further
    Ok(ObjectSlot(
      kind: NativeFunction(value.NativeBoundFunction(target:, bound_args:, ..)),
      ..,
    )) -> {
      let final_args = list.append(bound_args, args)
      construct_value(state, target, final_args, rest_stack)
    }
    Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
      call_native(state, native, args, rest_stack, JsUndefined)
    _ -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "is not a constructor",
        )
      Error(#(Thrown, err, h))
    }
  }
}

/// Call an arbitrary JsValue as a function with the given this and args.
/// Used by Function.prototype.call/apply and bound function invocation.
fn call_value(
  state: State,
  callee: JsValue,
  args: List(JsValue),
  this_val: JsValue,
) -> Result(State, #(StepResult, JsValue, Heap)) {
  case callee {
    JsObject(ref) ->
      case heap.read(state.heap, ref) {
        Ok(ObjectSlot(kind: FunctionObject(func_index: _, env: env_ref), ..)) ->
          call_function(state, ref, env_ref, args, state.stack, this_val, None)
        Ok(ObjectSlot(kind: NativeFunction(native), ..)) ->
          call_native(state, native, args, state.stack, this_val)
        _ -> {
          let #(h, err) =
            object.make_type_error(
              state.heap,
              state.builtins,
              "object is not a function",
            )
          Error(#(Thrown, err, h))
        }
      }
    _ -> {
      let #(h, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          typeof_value(callee, state.heap) <> " is not a function",
        )
      Error(#(Thrown, err, h))
    }
  }
}

/// Extract elements from an array object as a list of JsValues.
/// Used by Function.prototype.apply to unpack the args array.
fn extract_array_args(h: Heap, ref: Ref) -> List(JsValue) {
  case heap.read(h, ref) {
    Ok(ObjectSlot(kind: ArrayObject(length:), elements:, ..)) ->
      extract_elements_loop(elements, 0, length, [])
    _ -> []
  }
}

fn extract_elements_loop(
  elements: dict.Dict(Int, JsValue),
  idx: Int,
  length: Int,
  acc: List(JsValue),
) -> List(JsValue) {
  case idx >= length {
    True -> list.reverse(acc)
    False -> {
      let val = case dict.get(elements, idx) {
        Ok(v) -> v
        Error(_) -> JsUndefined
      }
      extract_elements_loop(elements, idx + 1, length, [val, ..acc])
    }
  }
}

/// Route a NativeFn call to the correct builtin module.
fn dispatch_native(
  native: value.NativeFn,
  args: List(JsValue),
  this: JsValue,
  state: State,
) -> #(Heap, Result(JsValue, JsValue)) {
  case native {
    value.NativeObjectConstructor ->
      builtins_object.call_native(
        args,
        this,
        state.heap,
        state.builtins.object.prototype,
      )
    value.NativeFunctionConstructor -> {
      let #(heap, err) =
        object.make_type_error(
          state.heap,
          state.builtins,
          "Function constructor is not supported",
        )
      #(heap, Error(err))
    }
    value.NativeArrayConstructor ->
      builtins_array.construct(args, state.heap, state.builtins.array.prototype)
    value.NativeArrayIsArray -> builtins_array.is_array(args, state.heap)
    value.NativeErrorConstructor(proto:) ->
      builtins_error.call_native(proto, args, this, state.heap)
    value.NativeObjectGetOwnPropertyDescriptor ->
      builtins_object.get_own_property_descriptor(
        args,
        state.heap,
        state.builtins.object.prototype,
      )
    value.NativeObjectDefineProperty ->
      builtins_object.define_property(args, state.heap)
    value.NativeObjectGetOwnPropertyNames ->
      builtins_object.get_own_property_names(
        args,
        state.heap,
        state.builtins.array.prototype,
      )
    value.NativeObjectKeys ->
      builtins_object.keys(args, state.heap, state.builtins.array.prototype)
    value.NativeObjectPrototypeHasOwnProperty ->
      builtins_object.has_own_property(this, args, state.heap)
    value.NativeObjectPrototypePropertyIsEnumerable ->
      builtins_object.property_is_enumerable(this, args, state.heap)
    value.NativeArrayPrototypeJoin ->
      builtins_array.array_join(this, args, state.heap)
    value.NativeArrayPrototypePush ->
      builtins_array.array_push(this, args, state.heap)
    value.NativeMathPow -> builtins_math.math_pow(args, state.heap)
    // Math methods
    value.NativeMathAbs -> builtins_math.math_abs(args, state.heap)
    value.NativeMathFloor -> builtins_math.math_floor(args, state.heap)
    value.NativeMathCeil -> builtins_math.math_ceil(args, state.heap)
    value.NativeMathRound -> builtins_math.math_round(args, state.heap)
    value.NativeMathTrunc -> builtins_math.math_trunc(args, state.heap)
    value.NativeMathSqrt -> builtins_math.math_sqrt(args, state.heap)
    value.NativeMathMax -> builtins_math.math_max(args, state.heap)
    value.NativeMathMin -> builtins_math.math_min(args, state.heap)
    value.NativeMathLog -> builtins_math.math_log(args, state.heap)
    value.NativeMathSin -> builtins_math.math_sin(args, state.heap)
    value.NativeMathCos -> builtins_math.math_cos(args, state.heap)
    // String.prototype methods
    value.NativeStringPrototypeCharAt ->
      builtins_string.string_char_at(this, args, state.heap)
    value.NativeStringPrototypeCharCodeAt ->
      builtins_string.string_char_code_at(this, args, state.heap)
    value.NativeStringPrototypeIndexOf ->
      builtins_string.string_index_of(this, args, state.heap)
    value.NativeStringPrototypeLastIndexOf ->
      builtins_string.string_last_index_of(this, args, state.heap)
    value.NativeStringPrototypeIncludes ->
      builtins_string.string_includes(this, args, state.heap)
    value.NativeStringPrototypeStartsWith ->
      builtins_string.string_starts_with(this, args, state.heap)
    value.NativeStringPrototypeEndsWith ->
      builtins_string.string_ends_with(this, args, state.heap)
    value.NativeStringPrototypeSlice ->
      builtins_string.string_slice(this, args, state.heap)
    value.NativeStringPrototypeSubstring ->
      builtins_string.string_substring(this, args, state.heap)
    value.NativeStringPrototypeToLowerCase ->
      builtins_string.string_to_lower_case(this, args, state.heap)
    value.NativeStringPrototypeToUpperCase ->
      builtins_string.string_to_upper_case(this, args, state.heap)
    value.NativeStringPrototypeTrim ->
      builtins_string.string_trim(this, args, state.heap)
    value.NativeStringPrototypeTrimStart ->
      builtins_string.string_trim_start(this, args, state.heap)
    value.NativeStringPrototypeTrimEnd ->
      builtins_string.string_trim_end(this, args, state.heap)
    value.NativeStringPrototypeSplit ->
      builtins_string.string_split(
        this,
        args,
        state.heap,
        state.builtins.array.prototype,
      )
    value.NativeStringPrototypeConcat ->
      builtins_string.string_concat(this, args, state.heap)
    value.NativeStringPrototypeToString ->
      builtins_string.string_to_string(this, args, state.heap)
    value.NativeStringPrototypeValueOf ->
      builtins_string.string_value_of(this, args, state.heap)
    value.NativeStringPrototypeRepeat ->
      builtins_string.string_repeat(this, args, state.heap)
    value.NativeStringPrototypePadStart ->
      builtins_string.string_pad_start(this, args, state.heap)
    value.NativeStringPrototypePadEnd ->
      builtins_string.string_pad_end(this, args, state.heap)
    value.NativeStringPrototypeAt ->
      builtins_string.string_at(this, args, state.heap)
    // These are handled in call_native before reaching dispatch_native.
    // If we ever get here, it's a bug.
    value.NativeFunctionCall
    | value.NativeFunctionApply
    | value.NativeFunctionBind
    | value.NativeBoundFunction(..) ->
      panic as "Function call/apply/bind/bound should be handled in call_native"
  }
}

/// Pop n items from stack. Returns #(popped_items_in_order, remaining_stack).
fn pop_n(
  stack: List(JsValue),
  n: Int,
) -> Result(#(List(JsValue), List(JsValue)), Nil) {
  pop_n_loop(stack, n, [])
}

fn pop_n_loop(
  stack: List(JsValue),
  remaining: Int,
  acc: List(JsValue),
) -> Result(#(List(JsValue), List(JsValue)), Nil) {
  case remaining {
    0 -> Ok(#(acc, stack))
    _ ->
      case stack {
        [top, ..rest] -> pop_n_loop(rest, remaining - 1, [top, ..acc])
        [] -> Error(Nil)
      }
  }
}

/// Pad args to exactly `arity` length — truncate extras, fill missing with undefined.
fn pad_args(args: List(JsValue), arity: Int) -> List(JsValue) {
  let len = list.length(args)
  case len >= arity {
    True -> list.take(args, arity)
    False -> list.append(args, list.repeat(JsUndefined, arity - len))
  }
}

// ============================================================================
// Primitive property access helpers
// ============================================================================

/// Look up a named property on a string primitive.
/// Handles "length" directly, delegates the rest to String.prototype.
fn get_string_field(
  s: String,
  name: String,
  heap: Heap,
  string_proto: Ref,
) -> JsValue {
  case name {
    "length" -> JsNumber(Finite(int.to_float(string.length(s))))
    _ ->
      case object.get_property(heap, string_proto, name) {
        Ok(v) -> v
        Error(_) -> JsUndefined
      }
  }
}

/// Look up a computed property on a string primitive.
/// Numeric index → character access, "length" → length, else String.prototype.
fn get_string_elem(
  s: String,
  key: JsValue,
  heap: Heap,
  string_proto: Ref,
) -> JsValue {
  case to_array_index(key) {
    Ok(idx) -> {
      let len = string.length(s)
      case idx >= 0 && idx < len {
        True -> JsString(string.slice(s, idx, 1))
        False -> JsUndefined
      }
    }
    Error(_) ->
      case key {
        JsString(name) -> get_string_field(s, name, heap, string_proto)
        _ -> JsUndefined
      }
  }
}

// ============================================================================
// Computed property access helpers
// ============================================================================

/// Read a property from a unified object using a JsValue key.
/// Dispatches on ExoticKind: arrays use elements dict, others use properties.
fn get_elem_value(heap: Heap, ref: value.Ref, key: JsValue) -> JsValue {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) ->
      case kind {
        ArrayObject(length:) ->
          case to_array_index(key) {
            Ok(idx) ->
              case dict.get(elements, idx) {
                Ok(val) -> val
                Error(_) -> JsUndefined
              }
            Error(_) ->
              // Non-numeric key on array — check "length", then properties, then prototype
              case key {
                JsString("length") -> JsNumber(Finite(int.to_float(length)))
                _ -> {
                  let key_str = to_js_string(key)
                  case dict.get(properties, key_str) {
                    Ok(DataProperty(value: val, ..)) -> val
                    Error(_) ->
                      case prototype {
                        Some(proto_ref) -> get_elem_value(heap, proto_ref, key)
                        None -> JsUndefined
                      }
                  }
                }
              }
          }
        OrdinaryObject | FunctionObject(..) | NativeFunction(_) -> {
          let key_str = to_js_string(key)
          case object.get_property(heap, ref, key_str) {
            Ok(val) -> val
            Error(_) -> JsUndefined
          }
        }
      }
    _ -> JsUndefined
  }
}

/// Write a property to a unified object using a JsValue key.
fn put_elem_value(
  heap: Heap,
  ref: value.Ref,
  key: JsValue,
  val: JsValue,
) -> Heap {
  case heap.read(heap, ref) {
    Ok(ObjectSlot(kind:, properties:, elements:, prototype:)) ->
      case kind {
        ArrayObject(length:) ->
          case to_array_index(key) {
            Ok(idx) -> {
              let new_elements = dict.insert(elements, idx, val)
              let new_length = case idx >= length {
                True -> idx + 1
                False -> length
              }
              heap.write(
                heap,
                ref,
                ObjectSlot(
                  kind: ArrayObject(new_length),
                  properties:,
                  elements: new_elements,
                  prototype:,
                ),
              )
            }
            Error(_) -> {
              let key_str = to_js_string(key)
              object.set_property(heap, ref, key_str, val)
            }
          }
        OrdinaryObject | FunctionObject(..) | NativeFunction(_) -> {
          let key_str = to_js_string(key)
          object.set_property(heap, ref, key_str, val)
        }
      }
    _ -> heap
  }
}

/// Try to convert a JsValue to an array index (non-negative integer).
fn to_array_index(key: JsValue) -> Result(Int, Nil) {
  case key {
    JsNumber(Finite(n)) -> {
      let i = float.truncate(n)
      case int.to_float(i) == n && i >= 0 {
        True -> Ok(i)
        False -> Error(Nil)
      }
    }
    JsString(s) ->
      case int.parse(s) {
        Ok(i) if i >= 0 -> Ok(i)
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

// ============================================================================
// JS type coercion and operators
// ============================================================================

/// JS typeof — needs heap access to distinguish "function" from "object".
fn typeof_value(val: JsValue, heap: Heap) -> String {
  case val {
    JsUndefined | JsUninitialized -> "undefined"
    JsNull -> "object"
    JsBool(_) -> "boolean"
    JsNumber(_) -> "number"
    JsString(_) -> "string"
    JsBigInt(_) -> "bigint"
    JsSymbol(_) -> "symbol"
    JsObject(ref) ->
      case heap.read(heap, ref) {
        Ok(ObjectSlot(kind: FunctionObject(..), ..)) -> "function"
        Ok(ObjectSlot(kind: NativeFunction(_), ..)) -> "function"
        _ -> "object"
      }
  }
}

/// JS ToBoolean: https://tc39.es/ecma262/#sec-toboolean
fn is_truthy(val: JsValue) -> Bool {
  case val {
    JsUndefined | JsNull | JsUninitialized -> False
    JsBool(b) -> b
    JsNumber(NaN) -> False
    JsNumber(Finite(n)) -> n != 0.0
    JsNumber(Infinity) | JsNumber(NegInfinity) -> True
    JsString(s) -> s != ""
    JsBigInt(value.BigInt(n)) -> n != 0
    // Objects (including functions and arrays) and symbols are always truthy
    JsObject(_) | JsSymbol(_) -> True
  }
}

/// Execute a binary operation on two JsValues.
fn exec_binop(
  kind: BinOpKind,
  left: JsValue,
  right: JsValue,
) -> Result(JsValue, String) {
  case kind {
    // Arithmetic — JS + is overloaded for string concat
    Add -> js_add(left, right)
    Sub -> num_binop(left, right, num_sub)
    Mul -> num_binop(left, right, num_mul)
    Div -> num_binop(left, right, num_div)
    Mod -> num_binop(left, right, num_mod)
    Exp -> num_binop(left, right, num_exp)

    // Bitwise — convert to i32, operate, convert back
    BitAnd -> bitwise_binop(left, right, int.bitwise_and)
    BitOr -> bitwise_binop(left, right, int.bitwise_or)
    BitXor -> bitwise_binop(left, right, int.bitwise_exclusive_or)
    ShiftLeft ->
      bitwise_binop(left, right, fn(a, b) {
        int.bitwise_shift_left(a, int.bitwise_and(b, 31))
      })
    ShiftRight ->
      bitwise_binop(left, right, fn(a, b) {
        int.bitwise_shift_right(a, int.bitwise_and(b, 31))
      })
    UShiftRight ->
      bitwise_binop(left, right, fn(a, b) {
        int.bitwise_shift_right(
          int.bitwise_and(a, 0xFFFFFFFF),
          int.bitwise_and(b, 31),
        )
      })

    // Comparison
    StrictEq -> Ok(JsBool(strict_equal(left, right)))
    StrictNotEq -> Ok(JsBool(!strict_equal(left, right)))
    Eq -> Ok(JsBool(abstract_equal(left, right)))
    NotEq -> Ok(JsBool(!abstract_equal(left, right)))

    Lt -> compare_values(left, right, fn(ord) { ord == LtOrd })
    LtEq ->
      compare_values(left, right, fn(ord) { ord == LtOrd || ord == EqOrd })
    Gt -> compare_values(left, right, fn(ord) { ord == GtOrd })
    GtEq ->
      compare_values(left, right, fn(ord) { ord == GtOrd || ord == EqOrd })

    // In and InstanceOf handled in BinOp dispatcher (needs heap access)
    opcode.In -> Error("in: unreachable — handled in dispatcher")
    opcode.InstanceOf ->
      Error("instanceof: unreachable — handled in dispatcher")
  }
}

/// JS + operator: string concat if either side is string, else numeric add.
fn js_add(left: JsValue, right: JsValue) -> Result(JsValue, String) {
  case left, right {
    JsString(a), JsString(b) -> Ok(JsString(a <> b))
    JsString(a), _ -> Ok(JsString(a <> to_js_string(right)))
    _, JsString(b) -> Ok(JsString(to_js_string(left) <> b))
    _, _ -> num_binop(left, right, num_add)
  }
}

/// Execute a unary operation.
fn exec_unaryop(kind: UnaryOpKind, operand: JsValue) -> Result(JsValue, String) {
  case kind {
    Neg ->
      case to_number(operand) {
        Ok(n) -> Ok(JsNumber(num_negate(n)))
        Error(e) -> Error(e)
      }
    Pos ->
      case to_number(operand) {
        Ok(n) -> Ok(JsNumber(n))
        Error(e) -> Error(e)
      }
    BitNot ->
      case to_number(operand) {
        Ok(n) ->
          Ok(JsNumber(Finite(int.to_float(int.bitwise_not(num_to_int32(n))))))
        Error(e) -> Error(e)
      }
    LogicalNot -> Ok(JsBool(!is_truthy(operand)))
    Void -> Ok(JsUndefined)
  }
}

// ============================================================================
// JsNum arithmetic — IEEE 754 semantics without BEAM floats for special values
// ============================================================================

fn num_add(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, NegInfinity | NegInfinity, Infinity -> NaN
    Infinity, _ | _, Infinity -> Infinity
    NegInfinity, _ | _, NegInfinity -> NegInfinity
    Finite(x), Finite(y) -> Finite(x +. y)
  }
}

fn num_sub(a: JsNum, b: JsNum) -> JsNum {
  num_add(a, num_negate(b))
}

fn num_mul(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, Finite(0.0) | Finite(0.0), Infinity -> NaN
    NegInfinity, Finite(0.0) | Finite(0.0), NegInfinity -> NaN
    Infinity, Finite(x) | Finite(x), Infinity ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    NegInfinity, Finite(x) | Finite(x), NegInfinity ->
      case x >. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Infinity, Infinity | NegInfinity, NegInfinity -> Infinity
    Infinity, NegInfinity | NegInfinity, Infinity -> NegInfinity
    Finite(x), Finite(y) -> Finite(x *. y)
  }
}

fn num_div(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, Infinity
    | Infinity, NegInfinity
    | NegInfinity, Infinity
    | NegInfinity, NegInfinity
    -> NaN
    Infinity, Finite(x) ->
      case x >=. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    NegInfinity, Finite(x) ->
      case x >=. 0.0 {
        True -> NegInfinity
        False -> Infinity
      }
    Finite(_), Infinity | Finite(_), NegInfinity -> Finite(0.0)
    Finite(0.0), Finite(0.0) -> NaN
    Finite(x), Finite(0.0) ->
      case x >. 0.0 {
        True -> Infinity
        False -> NegInfinity
      }
    Finite(x), Finite(y) -> Finite(x /. y)
  }
}

fn num_mod(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    NaN, _ | _, NaN -> NaN
    Infinity, _ | NegInfinity, _ -> NaN
    _, Infinity | _, NegInfinity -> a
    Finite(_), Finite(0.0) -> NaN
    Finite(0.0), Finite(_) -> Finite(0.0)
    Finite(x), Finite(y) ->
      Finite(x -. int.to_float(float.truncate(x /. y)) *. y)
  }
}

fn num_exp(a: JsNum, b: JsNum) -> JsNum {
  case a, b {
    _, Finite(0.0) -> Finite(1.0)
    _, NaN -> NaN
    NaN, _ -> NaN
    Finite(x), Finite(y) -> Finite(float_power(x, y))
    Infinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    NegInfinity, Finite(y) ->
      case y >. 0.0 {
        True -> Infinity
        False -> Finite(0.0)
      }
    _, Infinity -> NaN
    _, NegInfinity -> NaN
  }
}

fn num_negate(n: JsNum) -> JsNum {
  case n {
    Finite(x) -> Finite(float.negate(x))
    NaN -> NaN
    Infinity -> NegInfinity
    NegInfinity -> Infinity
  }
}

/// Apply a JsNum binary operation after coercing both operands to numbers.
fn num_binop(
  left: JsValue,
  right: JsValue,
  op: fn(JsNum, JsNum) -> JsNum,
) -> Result(JsValue, String) {
  case to_number(left), to_number(right) {
    Ok(a), Ok(b) -> Ok(JsNumber(op(a, b)))
    Error(e), _ | _, Error(e) -> Error(e)
  }
}

/// Apply a bitwise binary operation (convert to i32, operate, convert back).
fn bitwise_binop(
  left: JsValue,
  right: JsValue,
  op: fn(Int, Int) -> Int,
) -> Result(JsValue, String) {
  case to_number(left), to_number(right) {
    Ok(a), Ok(b) ->
      Ok(JsNumber(Finite(int.to_float(op(num_to_int32(a), num_to_int32(b))))))
    Error(e), _ | _, Error(e) -> Error(e)
  }
}

/// JS ToNumber: https://tc39.es/ecma262/#sec-tonumber
fn to_number(val: JsValue) -> Result(JsNum, String) {
  case val {
    JsNumber(n) -> Ok(n)
    JsUndefined -> Ok(NaN)
    JsNull -> Ok(Finite(0.0))
    JsBool(True) -> Ok(Finite(1.0))
    JsBool(False) -> Ok(Finite(0.0))
    JsString("") -> Ok(Finite(0.0))
    JsString(s) ->
      case float.parse(s) {
        Ok(n) -> Ok(Finite(n))
        Error(_) ->
          case int.parse(s) {
            Ok(n) -> Ok(Finite(int.to_float(n)))
            Error(_) -> Ok(NaN)
          }
      }
    JsBigInt(_) -> Error("Cannot convert BigInt to number")
    JsSymbol(_) -> Error("Cannot convert Symbol to number")
    JsObject(_) -> Ok(NaN)
    JsUninitialized -> Error("Cannot access before initialization")
  }
}

/// JS ToString — delegates to value.to_js_string.
fn to_js_string(val: JsValue) -> String {
  value.to_js_string(val)
}

/// JS instanceof operator.
/// Walks the left operand's prototype chain looking for constructor.prototype.
fn js_instanceof(
  heap: Heap,
  left: JsValue,
  constructor: JsValue,
) -> Result(Bool, String) {
  // Right side must be a callable object (function)
  case constructor {
    JsObject(ctor_ref) ->
      case heap.read(heap, ctor_ref) {
        Ok(ObjectSlot(kind: FunctionObject(..), ..)) -> {
          // Get the constructor's .prototype property
          case object.get_property(heap, ctor_ref, "prototype") {
            Ok(JsObject(proto_ref)) ->
              // Walk left's prototype chain
              case left {
                JsObject(obj_ref) -> instanceof_walk(heap, obj_ref, proto_ref)
                // Primitives are never instances
                _ -> Ok(False)
              }
            Ok(_) ->
              Error("Function has non-object prototype in instanceof check")
            Error(_) -> Ok(False)
          }
        }
        _ -> Error("Right-hand side of instanceof is not callable")
      }
    _ -> Error("Right-hand side of instanceof is not callable")
  }
}

/// Walk an object's [[Prototype]] chain looking for a target ref.
fn instanceof_walk(
  heap: Heap,
  obj_ref: Ref,
  target_proto: Ref,
) -> Result(Bool, String) {
  case heap.read(heap, obj_ref) {
    Ok(ObjectSlot(prototype: Some(proto_ref), ..)) ->
      case proto_ref.id == target_proto.id {
        True -> Ok(True)
        False -> instanceof_walk(heap, proto_ref, target_proto)
      }
    Ok(ObjectSlot(prototype: None, ..)) -> Ok(False)
    _ -> Ok(False)
  }
}

/// JS === (strict equality)
fn strict_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    JsUndefined, JsUndefined -> True
    JsNull, JsNull -> True
    JsBool(a), JsBool(b) -> a == b
    // NaN !== NaN
    JsNumber(NaN), _ | _, JsNumber(NaN) -> False
    JsNumber(a), JsNumber(b) -> a == b
    JsString(a), JsString(b) -> a == b
    JsBigInt(a), JsBigInt(b) -> a == b
    // Object identity (same Ref) — covers functions and arrays too
    JsObject(a), JsObject(b) -> a == b
    JsSymbol(a), JsSymbol(b) -> a == b
    _, _ -> False
  }
}

/// JS == (abstract equality, simplified)
fn abstract_equal(left: JsValue, right: JsValue) -> Bool {
  case left, right {
    // Same type — use strict equality
    JsNull, JsNull
    | JsUndefined, JsUndefined
    | JsNull, JsUndefined
    | JsUndefined, JsNull
    -> True
    JsNumber(_), JsNumber(_)
    | JsBool(_), JsBool(_)
    | JsString(_), JsString(_)
    | JsObject(_), JsObject(_)
    | JsSymbol(_), JsSymbol(_)
    | JsBigInt(_), JsBigInt(_)
    -> strict_equal(left, right)
    // Number vs String — coerce string to number
    JsNumber(_), JsString(s) ->
      case to_number(JsString(s)) {
        Ok(n) -> strict_equal(left, JsNumber(n))
        Error(_) -> False
      }
    JsString(_), JsNumber(_) -> abstract_equal(right, left)
    // Bool vs anything — coerce bool to number
    JsBool(_), _ ->
      case to_number(left) {
        Ok(n) -> abstract_equal(JsNumber(n), right)
        Error(_) -> False
      }
    _, JsBool(_) -> abstract_equal(right, left)
    _, _ -> False
  }
}

/// Comparison order for relational ops.
type CompareOrd {
  LtOrd
  EqOrd
  GtOrd
}

/// Compare two values for relational operators (<, <=, >, >=).
fn compare_values(
  left: JsValue,
  right: JsValue,
  pred: fn(CompareOrd) -> Bool,
) -> Result(JsValue, String) {
  case left, right {
    JsString(a), JsString(b) -> {
      let ord = case string.compare(a, b) {
        order.Lt -> LtOrd
        order.Eq -> EqOrd
        order.Gt -> GtOrd
      }
      Ok(JsBool(pred(ord)))
    }
    _, _ -> {
      case to_number(left), to_number(right) {
        Ok(NaN), _ | _, Ok(NaN) -> Ok(JsBool(False))
        Ok(a), Ok(b) -> Ok(JsBool(pred(compare_nums(a, b))))
        Error(e), _ | _, Error(e) -> Error(e)
      }
    }
  }
}

/// Compare two JsNums (neither is NaN).
fn compare_nums(a: JsNum, b: JsNum) -> CompareOrd {
  case a, b {
    Infinity, Infinity | NegInfinity, NegInfinity -> EqOrd
    Infinity, _ -> GtOrd
    _, Infinity -> LtOrd
    NegInfinity, _ -> LtOrd
    _, NegInfinity -> GtOrd
    Finite(x), Finite(y) ->
      case x == y {
        True -> EqOrd
        False ->
          case x <. y {
            True -> LtOrd
            False -> GtOrd
          }
      }
    // NaN cases handled by caller
    NaN, _ | _, NaN -> EqOrd
  }
}

// ============================================================================
// Helpers
// ============================================================================

/// Get element at index from a list. O(n) — will replace with Array later.
fn list_get(items: List(a), index: Int) -> Result(a, Nil) {
  case index < 0 {
    True -> Error(Nil)
    False ->
      case items {
        [] -> Error(Nil)
        [first, ..] if index == 0 -> Ok(first)
        [_, ..rest] -> list_get(rest, index - 1)
      }
  }
}

/// Set element at index in a list. O(n).
fn list_set(items: List(a), index: Int, value: a) -> Result(List(a), Nil) {
  case items, index {
    [], _ -> Error(Nil)
    [_, ..rest], 0 -> Ok([value, ..rest])
    [first, ..rest], n ->
      case list_set(rest, n - 1, value) {
        Ok(new_rest) -> Ok([first, ..new_rest])
        Error(e) -> Error(e)
      }
  }
}

/// Convert JsNum to int32 (JS ToInt32).
fn num_to_int32(n: JsNum) -> Int {
  case n {
    NaN | Infinity | NegInfinity -> 0
    Finite(f) -> {
      let i = float.truncate(f)
      // Wrap to 32 bits
      let wrapped = int.bitwise_and(i, 0xFFFFFFFF)
      // Sign extend if needed
      case wrapped > 0x7FFFFFFF {
        True -> wrapped - 0x100000000
        False -> wrapped
      }
    }
  }
}

// ============================================================================
// Float helpers — only power needs FFI now
// ============================================================================

@external(erlang, "arc_vm_ffi", "float_power")
fn float_power(base: Float, exp: Float) -> Float
