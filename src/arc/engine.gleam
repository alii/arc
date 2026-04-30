//// The embed-Arc library facade.
////
//// Hides the heap/builtins/globals bootstrapping dance behind a small API.
//// Callers who just want to run some JS should go through `new()` + `eval()`
//// instead of wiring up heap + builtins + entry directly.

import arc/compiler
import arc/internal/erlang
import arc/parser
import arc/vm/builtins
import arc/vm/builtins/common.{type Builtins}
import arc/vm/completion.{type Completion}
import arc/vm/exec/entry
import arc/vm/exec/event_loop
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

fn set_global_property(
  h: Heap,
  global: Ref,
  name: String,
  val: JsValue,
) -> Heap {
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
/// Drains the microtask queue only — there is no macrotask loop in core.
/// If your host functions use `host.suspend`, drive your own loop via
/// `eval_with` (e.g. `eval_with(eng, src, beam.run)` for the Erlang
/// mailbox loop).
pub fn eval(
  engine: Engine,
  source: String,
) -> Result(#(Completion, Engine), EvalError) {
  eval_with(engine, source, event_loop.finish)
}

/// Like `eval` but the caller supplies the post-script driver. `finish`
/// is handed the State after the top-level script returns and must drain
/// microtasks plus whatever macrotask loop the embedder owns.
pub fn eval_with(
  engine: Engine,
  source: String,
  finish: fn(state.State) -> state.State,
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
    entry.run_with(
      template,
      engine.heap,
      engine.builtins,
      engine.global,
      finish,
    )
    |> result.map_error(VmError),
  )
  #(completion, Engine(..engine, heap: completion_heap(completion)))
}

// ----------------------------------------------------------------------------
// Serialization
// ----------------------------------------------------------------------------

/// Serialize the entire engine state to a binary.
///
/// Host function closures stored in the heap will NOT survive — their Ref
/// slots persist but the Erlang closure data is lost. Embedders must
/// re-register host functions after `deserialize`.
pub fn serialize(engine: Engine) -> BitArray {
  erlang.term_to_binary(#(engine.heap, engine.builtins, engine.global))
}

/// Restore an engine from a binary produced by `serialize`.
pub fn deserialize(data: BitArray) -> Engine {
  let #(heap, builtins, global) = erlang.binary_to_term(data)
  Engine(heap:, builtins:, global:)
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
