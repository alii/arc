/// Bytecode Disassembler
///
/// Renders a compiled `FuncTemplate` as plain text — one instruction per
/// line — so a human can see exactly what the compiler emitted for a piece
/// of JS. This is a learning/debugging aid, not a serialization format: the
/// output is not meant to be parsed back.
///
/// Each op is printed with `string.inspect`, so the mnemonic is the `Op`
/// constructor name (`PushConst(0)`, `JumpIfFalse(12)`, ...). That keeps the
/// disassembler zero-maintenance: a new opcode shows up here the moment it is
/// added to `opcode.Op`, with no second table to keep in sync.
///
/// Jump targets are absolute PCs (phase 3 already resolved them), and every
/// line is prefixed with its own PC, so control flow can be followed by eye.
/// Ops that carry an index into a side table get a trailing `; ...` comment
/// resolving it: `PushConst`/`CmpLocalConstJump` show the constant, and
/// `MakeClosure` shows the nested function's name.
///
/// Nested functions are printed after their parent, depth-first, labelled
/// with their path in the function tree (`[0]`, `[0.1]`, ...) — the same
/// index `MakeClosure` refers to.
import arc/compiler
import arc/esm
import arc/parser
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/types.{type JsVal}
import arc/rt/val
import arc/vm/internal/tuple_array.{type TupleArray}
import arc/vm/opcode.{type Op}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// Which goal symbol a source is parsed and compiled under. Each has its own
/// parse and compile entry point in `parser`/`compiler`; this names the
/// choice so a caller states it once.
pub type Goal {
  /// A classic script.
  Script
  /// An ES module.
  Module
  /// One REPL input: a script whose top-level `let`/`const`/`class` target
  /// the persistent global lexical record, as `engine.repl_eval` runs it.
  ReplInput
}

/// A source that never made it to bytecode. Nothing ran, so it renders with
/// no engine in hand.
pub type SourceError {
  Syntax(parser.ParseError)
  Compile(compiler.CompileError)
}

pub fn format_source_error(err: SourceError) -> String {
  case err {
    Syntax(parse_err) ->
      "SyntaxError: " <> parser.parse_error_to_string(parse_err)
    Compile(compile_err) ->
      "compile error: " <> compiler.error_message(compile_err)
  }
}

/// Parse and compile `source` under `goal`, without running it.
pub fn compile(
  goal: Goal,
  source: String,
) -> Result(FuncTemplate, SourceError) {
  let compiled = case goal {
    Script -> {
      use #(body, sb) <- result.try(
        parser.parse_script(source) |> result.map_error(Syntax),
      )
      compiler.compile(body, sb) |> result.map_error(Compile)
    }
    ReplInput -> {
      use #(body, sb) <- result.try(
        parser.parse_script(source) |> result.map_error(Syntax),
      )
      compiler.compile_repl(body, sb) |> result.map_error(Compile)
    }
    Module -> {
      use #(items, sb) <- result.try(
        parser.parse_module(source) |> result.map_error(Syntax),
      )
      use compiled <- result.map(
        compiler.compile_module(items, sb, esm.analyze(items))
        |> result.map_error(Compile),
      )
      compiled.template
    }
  }
  result.map(compiled, compiler.shared_template)
}

/// Compile `source` under `goal` and disassemble the result.
pub fn source(goal: Goal, source: String) -> Result(String, SourceError) {
  compile(goal, source) |> result.map(disassemble)
}

/// Disassemble a compiled template (and, recursively, every function nested
/// inside it) into one printable string.
pub fn disassemble(template: FuncTemplate) -> String {
  render(template, "<main>", "")
  |> string.join("\n")
  <> "\n"
}

/// One function's section: header line, one line per op, then a blank line
/// and each child function's section. `label` is the display name, `path`
/// the dotted index path from the root ("" for the root itself).
fn render(template: FuncTemplate, label: String, path: String) -> List(String) {
  let ops = tuple_array.to_list(template.bytecode)
  let width = pc_width(list.length(ops))

  let code =
    list.index_map(ops, fn(op, pc) { format_op(pc, width, op, template) })

  let children =
    tuple_array.to_list(template.functions)
    |> list.index_map(fn(child, index) {
      let child_path = join_path(path, index)
      ["", ..render(child, child_label(child), child_path)]
    })
    |> list.flatten

  [header(template, label, path), ..code]
  |> list.append(children)
}

/// `function <main> (arity 0, locals 3) [strict]` — one line describing the
/// function whose code follows. Nested functions also carry their `[path]`
/// index so the reader can match them to the `MakeClosure(n)` that creates
/// them.
fn header(template: FuncTemplate, label: String, path: String) -> String {
  let where = case path {
    "" -> ""
    _ -> "[" <> path <> "] "
  }
  let shape =
    " (arity "
    <> int.to_string(template.arity)
    <> ", locals "
    <> int.to_string(template.local_count)
    <> ")"
  "function " <> where <> label <> shape <> flags(template)
}

/// The subset of the template's boolean flags that are set, as
/// ` [strict generator]` — or `""` when none are.
fn flags(template: FuncTemplate) -> String {
  let set =
    [
      #("strict", template.is_strict),
      #("arrow", template.is_arrow),
      #("generator", template.is_generator),
      #("async", template.is_async),
      #("derived-ctor", template.is_derived_constructor),
      #("class-ctor", template.is_class_constructor),
    ]
    |> list.filter_map(fn(flag) {
      case flag {
        #(name, True) -> Ok(name)
        #(_, False) -> Error(Nil)
      }
    })
  case set {
    [] -> ""
    _ -> " [" <> string.join(set, " ") <> "]"
  }
}

/// `  12  PushConst(0)          ; "hello"`
fn format_op(pc: Int, width: Int, op: Op, template: FuncTemplate) -> String {
  let addr = string.pad_start(int.to_string(pc), width, " ")
  let text = string.inspect(op)
  case annotate(op, template) {
    None -> "  " <> addr <> "  " <> text
    Some(note) ->
      "  " <> addr <> "  " <> string.pad_end(text, 28, " ") <> "  ; " <> note
  }
}

/// The trailing `; ...` comment for ops whose operand is an index into one
/// of the template's side tables — resolved here so the reader never has to.
fn annotate(op: Op, template: FuncTemplate) -> Option(String) {
  case op {
    opcode.PushConst(index) ->
      Some(resolve(index, template.constants, constant_to_string))
    opcode.CmpLocalConstJump(_, index, _, _) ->
      Some(resolve(index, template.constants, constant_to_string))
    opcode.MakeClosure(index) ->
      Some(resolve(index, template.functions, child_label))
    _ -> None
  }
}

/// Render `table[index]` for an inline comment. A bad index is a compiler bug,
/// not the disassembler's problem — every side-table lookup surfaces it the
/// same way instead of crashing (or, worse, silently dropping the annotation)
/// in a debugging tool.
fn resolve(
  index: Int,
  table: TupleArray(a),
  render: fn(a) -> String,
) -> String {
  case tuple_array.get(index, table) {
    Some(entry) -> render(entry)
    None -> "<out of range: " <> int.to_string(index) <> ">"
  }
}

/// Compile-time constants are always primitives (the constant pool never
/// holds heap handles), so this stays pure — no store needed. Every kind is
/// matched explicitly: a catch-all `string.inspect(other)` would render a
/// bigint (which the emitter really does intern) as the raw wire term.
/// `string.inspect` on the string arm gives JS-ish quoting/escaping for free.
fn constant_to_string(constant: JsVal) -> String {
  case types.classify(constant) {
    types.KStr(text) -> string.inspect(text)
    types.KNum(number) -> val.jsnum_to_string(number)
    types.KBig(n) -> int.to_string(n) <> "n"
    types.KBool(True) -> "true"
    types.KBool(False) -> "false"
    types.KNull -> "null"
    types.KUndef -> "undefined"
    // The TDZ sentinel the emitter seeds `let`/`const` slots with.
    types.KTdz -> "<uninitialized>"
    types.KSym(id) -> types.symbol_descriptive_string(id)
    // Unreachable: nothing interns a heap handle in the constant pool. Marked
    // rather than crashed, since this is a debugging aid.
    types.KHandle(_) -> "<object ref>"
  }
}

fn child_label(child: FuncTemplate) -> String {
  option.unwrap(child.name, "<anonymous>")
}

fn join_path(parent: String, index: Int) -> String {
  case parent {
    "" -> int.to_string(index)
    _ -> parent <> "." <> int.to_string(index)
  }
}

/// Wide enough for the largest PC in this function, never less than 3 so
/// small functions still line up with their neighbours.
fn pc_width(op_count: Int) -> Int {
  int.max(3, string.length(int.to_string(op_count)))
}
