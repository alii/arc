// big sources compile in a heap-presized scratch process
@external(erlang, "arc_compile_task_ffi", "run")
pub fn run(source_bytes: Int, task: fn() -> a) -> a
