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
import arc/vm/heap
import arc/vm/internal/elements
import arc/vm/state.{type Heap, type HostFn}
import arc/vm/value.{
  type JsValue, type Ref, JsObject, Named, ObjectSlot, OrdinaryObject,
}
import gleam/dict
import gleam/list
import gleam/option.{Some}
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
// Host FFI — extend the engine with embedder-provided globals
// ----------------------------------------------------------------------------

/// Add a top-level global native function.
///
/// The function becomes callable from JS as `name(...)`. `arity` is the
/// reported `.length` property; the impl still receives all passed args.
pub fn define_fn(
  engine: Engine,
  name: String,
  arity: Int,
  impl: HostFn,
) -> Engine {
  let #(h, fn_ref) =
    common.alloc_host_fn(
      engine.heap,
      engine.builtins.function.prototype,
      impl,
      name,
      arity,
    )
  let h = set_global_property(h, engine.global, name, JsObject(fn_ref))
  Engine(..engine, heap: h)
}

/// Add a top-level namespace object (like `Math` or `JSON`) with methods.
///
/// Creates a plain object at the given global name whose own properties are
/// the supplied methods. Each method spec is `#(name, arity, impl)`.
pub fn define_namespace(
  engine: Engine,
  name: String,
  methods: List(#(String, Int, HostFn)),
) -> Engine {
  let fn_proto = engine.builtins.function.prototype
  let #(h, props) =
    list.fold(methods, #(engine.heap, []), fn(acc, spec) {
      let #(h, props) = acc
      let #(method_name, arity, impl) = spec
      let #(h, fn_ref) =
        common.alloc_host_fn(h, fn_proto, impl, method_name, arity)
      #(h, [#(method_name, value.builtin_property(JsObject(fn_ref))), ..props])
    })
  let #(h, ns_ref) =
    heap.alloc(
      h,
      ObjectSlot(
        kind: OrdinaryObject,
        properties: common.named_props(props),
        symbol_properties: [],
        elements: elements.new(),
        prototype: Some(engine.builtins.object.prototype),
        extensible: True,
      ),
    )
  let h = heap.root(h, ns_ref)
  let h = set_global_property(h, engine.global, name, JsObject(ns_ref))
  Engine(..engine, heap: h)
}

/// Add a raw JsValue as a top-level global binding.
///
/// For constants or pre-built objects that don't fit `define_fn` or
/// `define_namespace`. The value is installed as a writable, configurable,
/// non-enumerable data property on `globalThis`.
pub fn define_global(engine: Engine, name: String, val: JsValue) -> Engine {
  let h = set_global_property(engine.heap, engine.global, name, val)
  Engine(..engine, heap: h)
}

fn set_global_property(h: Heap, global: Ref, name: String, val: JsValue) -> Heap {
  heap.update(h, global, fn(slot) {
    case slot {
      ObjectSlot(properties:, ..) ->
        ObjectSlot(
          ..slot,
          properties: dict.insert(
            properties,
            Named(name),
            value.builtin_property(val),
          ),
        )
      other -> other
    }
  })
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
