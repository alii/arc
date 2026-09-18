import arc/bytecode/opcode.{type Op}
import arc/internal/tuple_array.{type TupleArray}
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/types.{type JsVal}
import arc/rt/val as rt_val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn disassemble(template: FuncTemplate) -> String {
  render(template, "<main>", "")
  |> string.join("\n")
  <> "\n"
}

fn render(template: FuncTemplate, label: String, path: String) -> List(String) {
  let ops = tuple_array.to_list(template.bytecode)
  let width = pc_width(list.length(ops))

  let #(_, code) =
    list.index_fold(ops, #(0, []), fn(acc, op, pc) {
      let #(prev_line, rows) = acc
      let row = format_op(pc, width, op, template)
      case bytecode.line_at(template, pc) {
        line if line == prev_line -> #(line, [row, ..rows])
        line -> #(line, [row, line_marker(width, line), ..rows])
      }
    })
  let code = list.reverse(code)

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
  "function " <> where <> label <> shape <> flags(template) <> regs(template)
}

// negative slot operands index this list
fn regs(template: FuncTemplate) -> String {
  case template.regs {
    bytecode.NoRegs -> ""
    bytecode.Regs(a, b) ->
      " [regs "
      <> string.join(
        list.filter([a, b], fn(r) { r != bytecode.no_register })
          |> list.map(int.to_string),
        " ",
      )
      <> "]"
  }
}

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

fn line_marker(width: Int, line: Int) -> String {
  "  " <> string.repeat(" ", width) <> "  .line " <> int.to_string(line)
}

fn format_op(pc: Int, width: Int, op: Op, template: FuncTemplate) -> String {
  let addr = string.pad_start(int.to_string(pc), width, " ")
  let text = string.inspect(op)
  case annotate(op, template) {
    None -> "  " <> addr <> "  " <> text
    Some(note) ->
      "  " <> addr <> "  " <> string.pad_end(text, 28, " ") <> "  ; " <> note
  }
}

fn annotate(op: Op, template: FuncTemplate) -> Option(String) {
  case op {
    opcode.PushConst(index) ->
      Some(render_entry(index, template.constants, constant_to_string))
    opcode.CmpLocalConstJump(_, index, _, _, _)
    | opcode.IncLocalCmpConstJump(_, _, index, _, _, _)
    | opcode.CmpConstJump(index, _, _, _)
    | opcode.BinOpConst(_, index)
    | opcode.BinOpConstPut(_, index, _)
    | opcode.BinOpLocalConst(_, _, index) ->
      Some(render_entry(index, template.constants, constant_to_string))
    opcode.MakeClosure(index) ->
      Some(render_entry(index, template.functions, child_label))
    _ -> None
  }
}

fn render_entry(
  index: Int,
  table: TupleArray(a),
  render: fn(a) -> String,
) -> String {
  case tuple_array.get(index, table) {
    Some(entry) -> render(entry)
    None -> "<out of range: " <> int.to_string(index) <> ">"
  }
}

fn constant_to_string(constant: JsVal) -> String {
  case types.classify(constant) {
    types.KStr(text) -> string.inspect(text)
    types.KNum(number) -> rt_val.jsnum_to_string(number)
    types.KBig(n) -> int.to_string(n) <> "n"
    types.KBool(True) -> "true"
    types.KBool(False) -> "false"
    types.KNull -> "null"
    types.KUndef -> "undefined"
    types.KTdz -> "<uninitialized>"
    types.KSym(id) -> types.symbol_descriptive_string(id)
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

fn pc_width(op_count: Int) -> Int {
  int.max(3, string.length(int.to_string(op_count)))
}
