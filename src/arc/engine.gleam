//// The embed-Arc library facade.
////
//// Hides the heap/builtins/globals bootstrapping dance behind a small API.
//// Callers who just want to run some JS should go through `new()` + `eval()`
//// instead of wiring up heap + builtins + entry directly.

import arc/compiler
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{type Completion}
import arc/vm/exec/entry
import arc/vm/heap.{type Heap}
import arc/vm/state
import arc/vm/value.{type Ref}
import gleam/result
import gleam/string

// ----------------------------------------------------------------------------
// Engine type
// ----------------------------------------------------------------------------

/// An initialized JS engine — heap, builtins, global object.
///
/// Opaque so callers can't reach inside and mutate pieces independently;
/// the only way to advance an engine is via `eval`.
pub opaque type Engine {
  Engine(heap: Heap, builtins: Builtins, global: Ref)
}

/// Errors from `eval` — covers the whole parse → compile → run pipeline.
pub type EvalError {
  ParseError(parser.ParseError)
  CompileError(compiler.CompileError)
  VmError(state.VmError)
}

// ----------------------------------------------------------------------------
// Constructors
// ----------------------------------------------------------------------------

/// Create a fresh engine with a new heap and all builtins installed.
pub fn new() -> Engine {
  let h = heap.new()
  let #(h, b) = builtins.init(h)
  let #(h, global) = builtins.globals(b, h)
  Engine(heap: h, builtins: b, global:)
}

// ----------------------------------------------------------------------------
// Evaluation
// ----------------------------------------------------------------------------

/// Parse, compile, and run a JS source string. Returns the completion
/// (normal return value or uncaught exception) plus a new engine carrying
/// the updated heap.
///
/// Drains the microtask queue but does NOT run the mailbox-backed event
/// loop — use `eval_with_event_loop` if you need `Arc.receiveAsync` or
/// `Arc.setTimeout` to actually fire.
pub fn eval(
  engine: Engine,
  source: String,
) -> Result(#(Completion, Engine), EvalError) {
  do_eval(engine, source, False)
}

/// Like `eval` but runs the BEAM-mailbox-backed event loop, so
/// `Arc.receiveAsync`, `Arc.setTimeout`, and friends work. Blocks until
/// `outstanding` reaches zero.
pub fn eval_with_event_loop(
  engine: Engine,
  source: String,
) -> Result(#(Completion, Engine), EvalError) {
  do_eval(engine, source, True)
}

fn do_eval(
  engine: Engine,
  source: String,
  event_loop: Bool,
) -> Result(#(Completion, Engine), EvalError) {
  use program <- result.try(
    parser.parse(source, parser.Script)
    |> result.map_error(ParseError),
  )
  use template <- result.try(
    compiler.compile(program)
    |> result.map_error(CompileError),
  )
  use completion <- result.map(
    entry.run(template, engine.heap, engine.builtins, engine.global, event_loop)
    |> result.map_error(VmError),
  )
  #(completion, Engine(..engine, heap: completion_heap(completion)))
}

// ----------------------------------------------------------------------------
// Accessors
// ----------------------------------------------------------------------------

/// Peek at the engine's heap. Useful for inspecting returned JsValues
/// (since most are heap refs).
pub fn heap(engine: Engine) -> Heap {
  engine.heap
}

/// The engine's global object ref (`globalThis`).
pub fn global(engine: Engine) -> Ref {
  engine.global
}

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

fn completion_heap(c: Completion) -> Heap {
  case c {
    completion.NormalCompletion(_, h) -> h
    completion.ThrowCompletion(_, h) -> h
    completion.YieldCompletion(_, h) -> h
    completion.AwaitCompletion(_, h) -> h
  }
}

fn compile_error_message(err: compiler.CompileError) -> String {
  case err {
    compiler.BreakOutsideLoop -> "break outside loop"
    compiler.ContinueOutsideLoop -> "continue outside loop"
    compiler.Unsupported(desc) -> "unsupported: " <> desc
  }
}

fn vm_error_message(err: state.VmError) -> String {
  case err {
    state.PcOutOfBounds(pc) -> "pc out of bounds: " <> string.inspect(pc)
    state.StackUnderflow(op) -> "stack underflow in " <> op
    state.LocalIndexOutOfBounds(i) ->
      "local index out of bounds: " <> string.inspect(i)
    state.Unimplemented(op) -> "unimplemented opcode: " <> op
  }
}

pub fn eval_error_message(err: EvalError) -> String {
  case err {
    ParseError(e) -> parser.parse_error_to_string(e)
    CompileError(e) -> compile_error_message(e)
    VmError(e) -> vm_error_message(e)
  }
}
