import arc/dis
import gleam/string

fn dis_js(source: String) -> String {
  let assert Ok(text) = dis.source(dis.Script, source)
  text
}

pub fn disassemble_shape_test() {
  let text = dis_js("function add(a, b) { return a + b } add(1, 2)")
  assert string.contains(text, "function <main> ")
  assert string.contains(text, "MakeClosure(0)")
  assert string.contains(text, "; add")
  assert string.contains(text, "function [0] add (arity 2,")
  assert string.contains(text, "BinOpLocalLocal(AddOp, 4, 5)")
  assert string.contains(text, "; 2")
}

pub fn disassemble_string_constant_test() {
  let text = dis_js("let greeting = \"hi\"")
  assert string.contains(text, "; \"hi\"")
}

pub fn disassemble_fused_ops_test() {
  let text =
    dis_js(
      "function f(o) { if (o === 1) return; while (o != null) o = o.next; this.count = o.size; o.run(1) }",
    )
  assert string.contains(text, "[regs 4]")
  assert string.contains(text, "CmpLocalConstJump(-1, 0, Equality(StrictEqOp)")
  assert string.contains(text, "GetLocalField(-1, 0)          ; .next")
  assert string.contains(text, "GetLocalField(-1, 1)          ; .size")
  assert string.contains(text, "PutFieldPop(2)                ; .count")
  assert string.contains(text, "GetLocalField2(-1, 3)         ; .run")
}
