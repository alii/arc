import arc_aot/compile
import arc_aot/emit
import arc_aot/playground
import carder/backend/core_printer
import carder/pipeline
import gleam/string

const src = "class Counter { #n = 0; inc() { return ++this.#n; } }
function* fib() { let [a, b] = [0, 1]; for (;;) { yield a; [a, b] = [b, a + b]; } }
async function main() {
  const c = new Counter();
  for (const x of fib()) { if (x > 20) break; c.inc(); }
  await Promise.resolve();
  console.log(\"count\", c.inc(), [1, 2, 3].map(x => x * 2).join());
}
main();"

pub fn core_text_matches_single_process_printer_test() {
  let assert Ok(ir) = compile.to_ir(src, "playground")
  let assert Ok(cmod) = pipeline.ir_to_cmod(ir, emit.binding())
  let assert Ok(emitted) = playground.emit(src, "playground")
  assert emitted.core == core_printer.print_module(cmod)
}

pub fn erlang_text_is_a_module_test() {
  let assert Ok(emitted) = playground.emit(src, "playground")
  assert string.starts_with(emitted.erlang, "-module(playground).\n")
  assert string.contains(emitted.erlang, "js_main(")
  assert string.contains(emitted.ir, "js_main")
}

pub fn syntax_error_is_reported_test() {
  let assert Error(message) = playground.emit("let = ;", "playground")
  assert string.starts_with(message, "SyntaxError")
}

pub fn esm_is_rejected_test() {
  let assert Error(message) =
    playground.emit("export const x = 1", "playground")
  assert string.contains(message, "ES modules")
}
