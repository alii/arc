//// Thin `gleam run -m emit_2core_bench_verify` wrapper — runs ONLY the
//// bench_verify() gate (correctness + best-of-5 richards/obj_prop timing)
//// without the full trace-profile suite in emit_2core_profile.main().

import emit_2core_profile

pub fn main() {
  emit_2core_profile.bench_verify()
}
