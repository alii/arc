//// Heap-sized scratch process for parse/compile work.
////
//// Parsing and compiling a LARGE source allocates big transient
//// structures (AST, scope tree, IR) whose live set grows steadily — in a
//// default-sized BEAM process heap the generational GC re-copies that
//// growing live set many times, dwarfing the useful work. Running the
//// task in a short-lived process whose initial heap is sized to the
//// source avoids all of that, and its death frees everything at once.
////
//// Small sources (the overwhelmingly common case) run inline: a spawn
//// plus the result copy would cost more than they save. See
//// `arc_vm_ffi:run_compile_task/2` for the threshold and sizing.

/// Run a parse/compile `task`, in a heap-pre-sized scratch process when
/// `source_bytes` is large, inline otherwise. The task's result is copied
/// back to the caller, so keep it compact (a template or an error), not
/// the intermediate AST.
@external(erlang, "arc_vm_ffi", "run_compile_task")
pub fn run(source_bytes: Int, task: fn() -> a) -> a
