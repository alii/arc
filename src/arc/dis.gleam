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
import arc/vm/internal/tuple_array
import arc/vm/opcode.{type Op}
import arc/vm/value.{type FuncTemplate, type JsValue}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

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
    opcode.PushConst(index) -> Some(constant(template, index))
    opcode.CmpLocalConstJump(_, index, _, _) -> Some(constant(template, index))
    opcode.MakeClosure(index) ->
      tuple_array.get(index, template.functions)
      |> option.map(child_label)
    _ -> None
  }
}

/// Render `template.constants[index]` for an inline comment. A bad index is
/// a compiler bug, not the disassembler's problem — surface it instead of
/// crashing a debugging tool.
fn constant(template: FuncTemplate, index: Int) -> String {
  case tuple_array.get(index, template.constants) {
    Some(constant) -> constant_to_string(constant)
    None -> "<out of range: " <> int.to_string(index) <> ">"
  }
}

/// Compile-time constants are always primitives (the constant pool never
/// holds object refs), so this stays pure — no Heap needed. `string.inspect`
/// on the String arm gives JS-ish quoting/escaping for free; anything
/// unexpected falls through to `string.inspect` of the whole value rather
/// than being hidden.
fn constant_to_string(constant: JsValue) -> String {
  case constant {
    value.JsString(text) -> string.inspect(text)
    value.JsNumber(number) -> value.format_number_radix(number, 10)
    value.JsBool(True) -> "true"
    value.JsBool(False) -> "false"
    value.JsNull -> "null"
    value.JsUndefined -> "undefined"
    // The TDZ sentinel the emitter seeds `let`/`const` slots with.
    value.JsUninitialized -> "<uninitialized>"
    other -> string.inspect(other)
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
