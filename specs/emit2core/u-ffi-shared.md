# u-ffi-shared — Inventory + Placement Plan for arc *_ffi.erl → 2core

## Ground truth

**2core does NOT depend on arc** — `../2core/gleam.toml` deps: `gleam_stdlib`, `gleam_erlang`, `argv`, `simplifile` only. → **All runtime FFI must be COPIED into 2core**. arc keeps its own copies unchanged (its parser + bytecode interpreter still use them).

**2core FFI file placement convention** — flat in `2core/src/twocore_*_ffi.erl` (12 existing: `twocore_rt_state_ffi.erl`, `twocore_rt_mem_atomics_ffi.erl`, `twocore_rt_ref_ffi.erl`, `twocore_rt_gc_ffi.erl`, `twocore_rt_exn_ffi.erl`, `twocore_rt_table_ets_ffi.erl`, `twocore_rt_js_ffi.erl`, …). Gleam bindings live in `2core/src/twocore/runtime/rt_js_*.gleam` and `@external(erlang, "twocore_rt_js_*_ffi", "fn")`.

**Decision: PER-MODULE files, not one `twocore_rt_js_shared_ffi.erl`.** Rationale: (1) matches 2core's existing one-file-per-concern convention; (2) the regex block alone is ~6,400 lines across 7 interdependent .erl modules; (3) tz block is 3 interdependent .erl modules; (4) internal Erlang→Erlang calls (`arc_regexp_ffi:728 → arc_regex_props_ffi`, `arc_tz_ffi:60 → arc_tzif`) mean separate module names are required regardless.

## The one systematic signature change (§JsNum→sentinel)

arc FFI that returns non-finite floats uses arc's Gleam `JsNum` ADT runtime shape (`src/arc/vm/builtins/arc_math_ffi.erl:14-16`):
```erlang
{finite, Float} | na_n | infinity | neg_infinity
```
2core's value model (HANDOFF §2 line 43 + confirmed `../2core/src/twocore_rt_js_ffi.erl:82-84`) uses **unboxed** native `float()`/`integer()` for finite + bare-atom sentinels:
```erlang
Float | js_nan | js_inf | js_neg_inf
```
**Mechanical rewrite** on copy: `{finite, F}` → `F`; `na_n` → `js_nan`; `infinity` → `js_inf`; `neg_infinity` → `js_neg_inf`. Affects: `arc_math_ffi` (exp/pow/cosh/sinh/hypot/fround return + input pattern of pow), `arc_typed_array_ffi` (ta_get_float/3, ta_set_float/4 input, decode_f32_bits/1, decode_f64_bits/1), `arc_float_ffi` (parse_float/1 return). `arc_number_ffi` is unaffected (its Gleam callers pass finite floats only — non-finite handled before FFI).

## Inventory table

| arc file | 2core file | LoC | exports KEPT | exports DROPPED / CHANGED | owning rt_js module | shared by |
|---|---|---|---|---|---|---|
| `arc/vm/internal/arc_tree_array_ffi.erl` | `src/twocore_rt_js_tree_array_ffi.erl` | 66 | tree_array_new/1, tree_array_from_list/2, tree_array_get_option/2, tree_array_set/3, tree_array_size/1, tree_array_resize/2, tree_array_reset/2, tree_array_sparse_fold/3 | — (verbatim; wraps OTP `array`) | **M1** rt_js_store (owns `Elements` type in `JsSlot`) | M4 (SObject.elements read/write), M6 (Array.prototype iteration) |
| `arc/vm/internal/arc_job_queue_ffi.erl` | `src/twocore_rt_js_queue_ffi.erl` | 16 | job_queue_new/0, job_queue_push/2, job_queue_pop/1, job_queue_is_empty/1, job_queue_to_list/1 | — (verbatim; wraps OTP `queue`) | **M8** rt_js_async (microtask FIFO + `SAsyncGenData.queue`) | M1 (SAsyncGenData constructor) |
| `arc/vm/internal/arc_tuple_array_ffi.erl` | **NOT PORTED** | 35 | — | ALL DROPPED — bytecode interpreter only (constant pools, frame locals `arc/vm/internal/tuple_array.gleam:16-41`); compiled code has none | — | — |
| `arc/vm/arc_string_ffi.erl` | `src/twocore_rt_js_string_ffi.erl` | 245 | string_char_at/2, string_codepoint_at/2, string_codepoint_length/1, replacement_codepoint/0, string_index_of/3, string_last_index_of/3, string_cp_slice/3, string_cp_drop/2, string_cp_explode/1 | — (verbatim; pure UTF-8 codepoint indexing) | **M3** rt_js_val (ToString length, ToPropertyKey) | M6 (String.prototype.charAt/indexOf/slice/…) |
| `arc/vm/builtins/arc_number_ffi.erl` | `src/twocore_rt_js_number_ffi.erl` | 225 | js_number_to_string/1, format_to_fixed/2, format_to_exponential/2, format_to_exponential_auto/1, format_to_precision/2 | — (verbatim; callers guarantee finite input) | **M3** rt_js_val (`js_number_to_string` = ToString §7.1.12.1) | M6 (Number.prototype.toFixed/toExponential/toPrecision) |
| `arc/vm/builtins/arc_math_ffi.erl` | `src/twocore_rt_js_math_ffi.erl` | 144 | exp/1, pow/2, cosh/1, sinh/1, hypot/1, fround/1, is_neg_zero/1 | **§JsNum→sentinel** on all except is_neg_zero | **M6** rt_js_builtins (Math.*) | **M5** rt_js_ops (`pow/2` for `**`, `is_neg_zero/1` for SameValue — arc/vm/ops/numeric.gleam:257,277) |
| `arc/parser/arc_float_ffi.erl` | `src/twocore_rt_js_float_ffi.erl` | 108 | parse_float/1 | **§JsNum→sentinel** on return | **M3** rt_js_val (ToNumber(string) §7.1.4 — arc/vm/value.gleam:4839 imports `arc/parser/number` which calls this) | M6 (parseFloat/Number()) |
| `arc/vm/arc_bytes_ffi.erl` | `src/twocore_rt_js_bytes_ffi.erl` | 49 | unsafe_slice/3, drop_start/2, next_char_boundary/2 | — (verbatim; pure O(1) byte-offset UTF-8) | **M6** rt_js_builtins (regexp — re:run byte-index → string slice, arc/vm/builtins/regexp.gleam:94-105) | — (arc's lexer usage stays in arc) |
| `arc/vm/builtins/arc_uri_ffi.erl` | `src/twocore_rt_js_uri_ffi.erl` | 177 | encode/2, decode/2 | — (verbatim) | **M6** rt_js_builtins (encodeURI/decodeURI/encodeURIComponent/decodeURIComponent) | — |
| `arc/vm/builtins/arc_typed_array_ffi.erl` | `src/twocore_rt_js_typed_array_ffi.erl` | 145 | ta_zeroed/1, ta_get_int/3, ta_set_int/4, ta_get_float/3, ta_set_float/4, ta_clamp_uint8/1, ta_splice/3, f32_bits/1, f64_bits/1, decode_f32_bits/1, decode_f64_bits/1 | **§JsNum→sentinel** on ta_get_float/ta_set_float/decode_f32_bits/decode_f64_bits; also `IntElem`/`FloatElem` atoms (`i8`..`u64`,`f32`,`f64`) — verify M6's Gleam element enum compiles to same atoms | **M6** rt_js_builtins (TypedArray/DataView/ArrayBuffer) | — |
| `arc/vm/arc_clock_ffi.erl` | `src/twocore_rt_js_clock_ffi.erl` | 24 | monotonic_now/0, sleep/1 | — (verbatim) | **M8** rt_js_async (setTimeout drain, Atomics.wait timeout) | M6 (performance.now via monotonic) |
| `arc/internal/arc_host_time_ffi.erl` | `src/twocore_rt_js_clock_ffi.erl` (MERGE — add now_ms/0) | 10 | now_ms/0 | — (verbatim; erlang:system_time(millisecond)) | **M6** rt_js_builtins (Date.now) | — |
| `arc/vm/builtins/arc_tz_ffi.erl` | `src/twocore_rt_js_tz_ffi.erl` | 551 | lookup/1, offset_at/2, next_transition/2, previous_transition/2, canonical_id/1, host_zone/0, offset_at_utc_ms/1, offset_at_local_ms/1 | internal calls `arc_tzif:*`→`twocore_rt_js_tzif:*`, `arc_posix_tz:*`→`twocore_rt_js_posix_tz:*`; persistent_term cache OK (immutable, node-wide, threaded-store-safe) | **M6** rt_js_builtins (Date + Temporal) | — |
| `arc/vm/builtins/arc_tzif.erl` (companion, non-FFI) | `src/twocore_rt_js_tzif.erl` | 201 | (internal — called by tz_ffi only) | rename cross-refs | M6 (via tz_ffi) | — |
| `arc/vm/builtins/arc_posix_tz.erl` (companion, non-FFI) | `src/twocore_rt_js_posix_tz.erl` | 273 | (internal — called by tz_ffi only) | rename cross-refs | M6 (via tz_ffi) | — |
| `arc/vm/builtins/arc_regexp_ffi.erl` | `src/twocore_rt_js_regexp_ffi.erl` | 895 | regexp_exec_info/5, pair_trail/1 | internal calls: `arc_regex_props_ffi`→`twocore_rt_js_regex_props`, `arc_regex_charset`→`twocore_rt_js_regex_charset` (`-define(CS,…)` line 25), `arc_regex_vclass`→`twocore_rt_js_regex_vclass` | **M6** rt_js_builtins (RegExp.prototype.exec/test/match/…) | — |
| `arc/vm/builtins/arc_regex_vclass.erl` (companion) | `src/twocore_rt_js_regex_vclass.erl` | 289 | (internal) | rename cross-refs to props/charset | M6 (via regexp_ffi) | — |
| `arc/vm/builtins/arc_regex_charset.erl` (companion) | `src/twocore_rt_js_regex_charset.erl` | 265 | (internal) | rename cross-ref to regex_props (:184) | M6 (via regexp_ffi) | — |
| `arc/parser/arc_regex_props_ffi.erl` | `src/twocore_rt_js_regex_props.erl` (drop `_ffi` suffix — no Gleam @external, only Erlang callers) | 272 | classify_lone/1, classify_pair/2, translate_lone/4, translate_pair/4, char_set/1, string_list/1 | rename cross-refs to prop_tables + unicode_tables | M6 (via regexp_ffi:728,730 + vclass:239,256 + charset:184) | (arc's parser/regex.gleam:1659,1664 also calls it — arc keeps its copy) |
| `arc/parser/arc_regex_prop_tables_ffi.erl` (generated) | `src/twocore_rt_js_regex_prop_tables.erl` | 545 | gc_value/1, binary_prop/1, script_value/1 | rename only | M6 (via regex_props) | — |
| `arc/parser/arc_regex_uni17_ffi.erl` (generated, 4383 LoC) | `src/twocore_rt_js_regex_uni17.erl` | 4383 | ranges/1, strings/1, string_members/1 | rename only | M6 (via unicode_tables) | — |
| `arc/parser/arc_unicode_tables.erl` (companion) | `src/twocore_rt_js_unicode_tables.erl` | 75 | decoded_ranges/1, range_tuple/1, string_members/1 | rename cross-ref to regex_uni17 (:16,35); persistent_term cache OK | M6 (via regex_props:216,230,248) | — |
| `arc/vm/builtins/arc_sab_ffi.erl` | `src/twocore_rt_js_sab_ffi.erl` | 229 | new/1 (or /2), byte_length/1, grow/2, read_bytes/3, write_bytes/3, rmw_element/…, cas_element/… | — (verbatim; OTP `atomics` refs are process-shareable — orthogonal to threaded JsStore) | **M6** rt_js_builtins (SharedArrayBuffer/Atomics) | — |
| `arc/vm/builtins/arc_waiter_ffi.erl` | `src/twocore_rt_js_waiter_ffi.erl` | 299 | local_buffer_key/…, shared_buffer_key/…, insert_waiter/…, cancel_waiter/…, take_waiters/…, take_self_async_tokens/…, start_registry/0 | — (verbatim; ETS registry is node-global by design — cross-agent per spec §25.4; NOT a threaded-store violation) | **M6** rt_js_builtins (Atomics.wait/notify/waitAsync) | M8 (waitAsync integrates with microtask drain) |
| `arc/vm/arc_vm_ffi.erl` | **SPLIT** — see below | 179 | see below | see below | — | — |
| `arc/parser/arc_escape_ffi.erl` | **NOT PORTED** | 226 | — | parse-time only (arc/parser.gleam:106,113); arc keeps it | — | — |
| `arc/parser/arc_unicode_ffi.erl` | **NOT PORTED** | 66 | — | parse-time only (arc/parser/lexer.gleam:1829,1832); arc keeps it | — | — |
| `arc/arc_cli_ffi.erl`, `arc_snapshot_ffi.erl`, `arc/vm/arc_compile_task_ffi.erl`, `arc/wasm/arc_wasm_ffi.erl` | **NOT PORTED** | — | — | arc CLI / bytecode-interpreter infrastructure | — | — |

### `arc_vm_ffi.erl` split (`src/arc/vm/arc_vm_ffi.erl:10-16`)

| export | disposition | destination | reason |
|---|---|---|---|
| `heap_read/2` | KEEP verbatim | `src/twocore_rt_js_store_ffi.erl` (M1) | hot-path `Dict(Int,_)` get→Option for `t_cell_get` |
| `float_same_term/2` | KEEP verbatim | `src/twocore_rt_js_val_ffi.erl` (M3; used by M5 SameValue) | `=:=` on floats distinguishes ±0.0 (OTP 27+) |
| `put_existing_writable_data/3` | KEEP verbatim | `src/twocore_rt_js_obj_ffi.erl` (M4) | property-map fast-path Set on existing writable data desc |
| `define_own_data_property/3` | **REWRITE→/4** | `src/twocore_rt_js_obj_ffi.erl` (M4) | line 86 calls ambient `next_prop_seq()`; becomes `define_own_data_property(Props, Key, Val, Seq)` — caller passes threaded `JsStore.prop_seq` |
| `next_prop_seq/0` | **DROP** | — | ambient pdict counter → threaded `JsStore.prop_seq` (M1 §1 field) |
| `unique_positive_integer/0` | **DROP** | — | ambient `erlang:unique_integer` → threaded `JsStore.next_symbol` (M1) |
| `setup_locals_tuple/6`, `setup_locals_seeded/10` | **DROP** | — | bytecode-interpreter frame construction; M14 emits native arg-unpacking prologue instead |
| `-define(PROP_SEQ_RESERVED, 16)` | KEEP as constant | M4's Gleam module (`pub const prop_seq_reserved = 16`) | initial `JsStore.prop_seq` value |

## New 2core FFI files to CREATE (not ported — no arc equivalent)

| file | owning module | contents |
|---|---|---|
| `src/twocore_rt_js_store_ffi.erl` | M1 | `heap_read/2` (from arc_vm_ffi) + `identity/1` coercion (or reuse `gleam_stdlib:identity` per rt_ref pattern). All other M1 store ops are pure Gleam. |
| `src/twocore_rt_js_val_ffi.erl` | M3 | `float_same_term/2` (from arc_vm_ffi) + the term-classification guards existing `twocore_rt_js_ffi.erl:80-92` already has (`js_type/1`, `num_of/1`, `out/1`, `zero_aware_sign/1`) — MOVE those from `twocore_rt_js_ffi.erl` here |
| `src/twocore_rt_js_obj_ffi.erl` | M4 | `put_existing_writable_data/3` + `define_own_data_property/4` (from arc_vm_ffi, seq-threaded) |

## Existing `twocore_rt_js_ffi.erl` (555 LoC) — REPURPOSE

Currently exports (`:59-66`): `add,sub,mul,neg,divide,modulo,lt,le,gt,ge,strict_eq,eq,truthy,to_string,type_of,cell_new,cell_get,cell_set,new_object,get_prop,set_prop,has_prop,empty_list,console_log,not_callable`.

- `cell_new/1,cell_get/1,cell_set/2` (:475-477 pdict+make_ref) — **DELETED**, replaced by M1's threaded Gleam functions.
- `new_object/0,get_prop/2,set_prop/3,has_prop/2` — **DELETED**, replaced by M4's threaded Gleam functions.
- `add..ge,strict_eq,eq,truthy,to_string,type_of` — **KEPT** as `src/twocore_rt_js_ops_ffi.erl` (M5 backing). These already speak the `js_nan|js_inf|js_neg_inf` sentinel convention.
- `console_log,not_callable,empty_list` — **KEPT** in a small `src/twocore_rt_js_misc_ffi.erl` or fold into ops_ffi.

## Cross-module rename map (internal Erlang→Erlang calls to fix on copy)

```
arc_tzif                  → twocore_rt_js_tzif
arc_posix_tz              → twocore_rt_js_posix_tz
arc_regex_props_ffi       → twocore_rt_js_regex_props
arc_regex_prop_tables_ffi → twocore_rt_js_regex_prop_tables
arc_regex_uni17_ffi       → twocore_rt_js_regex_uni17
arc_unicode_tables        → twocore_rt_js_unicode_tables
arc_regex_charset         → twocore_rt_js_regex_charset   (also -define(CS,…) at regexp_ffi:25)
arc_regex_vclass          → twocore_rt_js_regex_vclass
```
Occurrence sites: `arc_tz_ffi.erl:60,74,…`; `arc_regexp_ffi.erl:25,728,730`; `arc_regex_vclass.erl:239,256`; `arc_regex_charset.erl:184`; `arc_regex_props_ffi.erl:45,47,71,77,83,123,216,230,248`; `arc_unicode_tables.erl:16,35`.

## Threaded-store compatibility audit

- **PURE / stateless** (verbatim OK): tree_array, job_queue, string, number, float, bytes, uri, math, typed_array, tzif, posix_tz, regex_charset, regex_vclass, regex_prop_tables, regex_uni17, unicode_tables, clock, host_time.
- **persistent_term caches** (tz_ffi, unicode_tables, regex_charset:171): node-global immutable read-mostly caches. NOT process-local state, NOT threaded-state-violating — a workers-share-it-by-design read cache. **Verbatim OK.**
- **ETS/atomics** (sab_ffi, waiter_ffi): cross-process shared memory — this is the SPEC-MANDATED semantics of SharedArrayBuffer/Atomics (cross-agent). Orthogonal to the threaded JsStore (which is per-realm). **Verbatim OK**, `start_registry/0` moves from `arc/vm/exec/interpreter.gleam:454` to M6 realm-init.
- **Ambient pdict/counters** (arc_vm_ffi `next_prop_seq`, `unique_positive_integer`): **DROPPED** — replaced by JsStore fields per M1 spec.
- **JsNum ADT boxing** (math, typed_array, float): **rewritten** per §JsNum→sentinel above.

## Dedup answer (the task's core concern)

| concern in task | resolution |
|---|---|
| "M3 and M6 both cite arc_number_ffi" | ONE file `twocore_rt_js_number_ffi.erl`; M3 `@external`s `js_number_to_string/1`, M6 `@external`s `format_to_*`. Owning module doc-header = M3. |
| "M4 and M6 both need tree_array" | ONE file `twocore_rt_js_tree_array_ffi.erl`; M1 owns the Gleam wrapper type (`Elements` inside `JsSlot`), M4+M6 import that Gleam module. |
| M5 and M6 both need `pow`/`is_neg_zero` | ONE file `twocore_rt_js_math_ffi.erl`; M6 owns; M5 `@external`s just those two. |
| M3 and M6 both need string codepoint ops | ONE file `twocore_rt_js_string_ffi.erl`; M3 owns; M6 `@external`s all. |

## Build order

All FFI files are leaves (no Gleam deps) → can be copied+renamed in ONE preliminary commit before M1-M8 start. The 8 regex files + 3 tz files must land together (mutual internal calls). Suggested single work unit **M0-ffi**: copy all 21 files, apply the rename map, apply §JsNum→sentinel to math/typed_array/float, split arc_vm_ffi. `erlc` compiles them standalone (no Gleam needed) → CI check = `rebar3 compile` or Gleam build succeeds.

## File count summary

21 .erl files land in `2core/src/`:
- 5 shared-infra: tree_array, queue, string, number, bytes
- 3 val/ops backing: math, float, val (new)
- 2 obj/store backing: store (new), obj (new)
- 1 clock (merged host_time)
- 3 tz block: tz, tzif, posix_tz
- 8 regex block: regexp, vclass, charset, regex_props, regex_prop_tables, regex_uni17, unicode_tables, (bytes already counted)
- 3 typed-array/SAB: typed_array, sab, waiter
- 1 uri
- Plus repurposed `twocore_rt_js_ops_ffi.erl` (renamed from existing `twocore_rt_js_ffi.erl` minus cell/obj ops)