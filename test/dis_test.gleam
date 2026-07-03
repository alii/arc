import arc/compiler
import arc/dis
import arc/parser
import gleam/string

// ============================================================================
// Bytecode disassembler (arc/dis)
// ============================================================================

/// Parse + compile a script and disassemble it — the front half of what
/// `arc --dis <file>` does, without touching the filesystem.
fn dis_js(source: String) -> String {
  let assert Ok(#(program, sb)) = parser.parse(source, parser.Script)
  let assert Ok(template) = compiler.compile(program, sb)
  dis.disassemble(template)
}

/// The root section is labelled `<main>`, nested functions are labelled with
/// the index their `MakeClosure` refers to, ops are inspected verbatim, and
/// constant operands are resolved into a trailing `;` comment.
pub fn disassemble_shape_test() {
  let text = dis_js("function add(a, b) { return a + b } add(1, 2)")
  assert string.contains(text, "function <main> ")
  assert string.contains(text, "MakeClosure(0)")
  assert string.contains(text, "; add")
  assert string.contains(text, "function [0] add (arity 2,")
  assert string.contains(text, "BinOp(AddOp)")
  assert string.contains(text, "; 2")
}

pub fn disassemble_string_constant_test() {
  let text = dis_js("let greeting = \"hi\"")
  assert string.contains(text, "; \"hi\"")
}
