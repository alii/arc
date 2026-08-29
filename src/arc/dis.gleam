import arc/bytecode/key
import arc/bytecode/opcode.{type Op}
import arc/compiler
import arc/esm
import arc/internal/tuple_array.{type TupleArray}
import arc/parser
import arc/rt/bytecode.{type FuncTemplate}
import arc/rt/types.{type JsVal}
import arc/rt/val
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Goal {
  Script
  Module
  ReplInput
}

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

pub fn compile(
  goal: Goal,
  source: String,
) -> Result(FuncTemplate, SourceError) {
  case goal {
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
}

pub fn source(goal: Goal, source: String) -> Result(String, SourceError) {
  compile(goal, source) |> result.map(disassemble)
}

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
        list.filter(list.map([a, b], int.to_string), fn(s) { s != "-1" }),
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
      Some(resolve(index, template.constants, constant_to_string))
    opcode.CmpLocalConstJump(_, index, _, _, _)
    | opcode.IncLocalCmpConstJump(_, _, index, _, _, _)
    | opcode.CmpConstJump(index, _, _, _)
    | opcode.BinOpConst(_, index)
    | opcode.BinOpConstPut(_, index, _)
    | opcode.BinOpLocalConst(_, _, index) ->
      Some(resolve(index, template.constants, constant_to_string))
    opcode.MakeClosure(index) ->
      Some(resolve(index, template.functions, child_label))
    opcode.GetField(k)
    | opcode.GetField2(k)
    | opcode.PutField(k)
    | opcode.PutFieldPop(k)
    | opcode.DeleteField(k)
    | opcode.DefineField(k)
    | opcode.DefineMethod(k)
    | opcode.DefineAccessor(k, _, _)
    | opcode.GetLocalField(_, k)
    | opcode.GetLocalField2(_, k)
    | opcode.GetFieldCall(k)
    | opcode.GetFieldCall1(k, _)
    | opcode.GetLocalFieldCall(_, k)
    | opcode.PutLocalLocalField(_, _, k)
    | opcode.BinOpLocalField(_, _, k) ->
      Some(resolve(k, template.keys, key_to_string))
    opcode.PutLocalConstField(_, index, k) ->
      Some(
        resolve(k, template.keys, key_to_string)
        <> " = "
        <> resolve(index, template.constants, constant_to_string),
      )
    opcode.NewObjectWith(slots, _) ->
      Some(
        list.reverse(slots)
        |> list.map(resolve(_, template.keys, key_to_string))
        |> string.join(" "),
      )
    _ -> None
  }
}

fn key_to_string(k: key.PropertyKey) -> String {
  "." <> key.key_display_string(k)
}

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

fn constant_to_string(constant: JsVal) -> String {
  case types.classify(constant) {
    types.KStr(text) -> string.inspect(text)
    types.KNum(number) -> val.jsnum_to_string(number)
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
