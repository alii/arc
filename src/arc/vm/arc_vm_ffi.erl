%% VM-core hot-path FFI: heap/property-table map operations, locals-tuple
%% construction and identity/sequence primitives. Bound by
%% src/arc/vm/{heap,value,exec/*,ops/object,realm}.gleam and
%% src/arc/compiler/emit.gleam. Everything here is a pure, non-blocking term
%% operation: no `receive`, no spawn, no I/O. String helpers live in
%% arc_string_ffi, array/queue backing stores in arc_array_ffi, the
%% parse/compile fork/join in arc_compile_task_ffi, and CLI stdin/argv in
%% arc_cli_ffi.
-module(arc_vm_ffi).
-export([setup_locals_tuple/6, setup_locals_seeded/10]).
-export([float_same_term/2]).
-export([unique_positive_integer/0]).
-export([next_prop_seq/0]).
-export([put_existing_writable_data/3]).
-export([define_own_data_property/3]).
-export([heap_read/2]).

%% Seq values 0..PROP_SEQ_RESERVED-1 are reserved for the birth-time constant
%% property seqs in arc/vm/builtins/common.gleam; the runtime counter starts
%% above them. See next_prop_seq/0.
-define(PROP_SEQ_RESERVED, 16).

%% Direct map lookup returning a Gleam Option. Hot path: the VM heap is a
%% Gleam Dict (a bare Erlang map on this target) and read/2 is called for
%% every property access.
heap_read(Map, Key) ->
    case Map of
        #{Key := V} -> {some, V};
        _ -> none
    end.

%% NOTE: an exec-wide min_heap_size floor (raising this process's minimum
%% heap for the duration of bytecode execution) was tried twice and reverted
%% twice: a large floor (8M words) makes the mutator walk a 64MB young heap
%% and costs 40-60% on tight interpreter loops (arith/call microbenches)
%% through cache locality, and it interacts badly with embedders that cap
%% the process with max_heap_size. Keep exec heap flags default; oversized
%% transient parse/compile heaps are handled by
%% arc_compile_task_ffi:run_compile_task/2.

%% Exact term equality for floats (=:=). Distinguishes -0.0 from +0.0
%% (OTP 27+) — used by SameValue (§7.2.11).
float_same_term(A, B) -> A =:= B.

%% Build the locals tuple for a JS function call in one forward pass:
%%   [Env..., Seeds..., Args(padded/truncated to Arity)..., Undef × rest]
%% Replaces the Gleam-side list.append + accumulator + list.reverse +
%% list_to_tuple chain (≈4 traversals, 3 intermediate lists) with a single
%% body-recursive forward build + list_to_tuple. local_count is bounded by
%% the compiler so non-tail recursion is fine here.
setup_locals_tuple(Env, Seeds, Args, Arity, LocalCount, Undef) ->
    list_to_tuple(locals_env(Env, Seeds, Args, Arity, LocalCount, Undef)).

%% Non-arrow locals build. LexicalSlots arrives as one of the Gleam variants
%% of arc/vm/opcode.LexicalSlots:
%%   {owned_lexical_slots, Base}                       — all four owned,
%%       contiguous, in canonical order [this, active_func, home_object,
%%       new_target] starting at Base (== length(Env)). This is the ordinary
%%       function case; the hot clause writes the seeds inline right after
%%       the env values, no intermediate seeds list. The type — not a
%%       convention — guarantees the layout this clause assumes.
%%   no_lexical_slots                                  — none at all.
%% The Gleam type also has a `captured_lexical_slots` variant, but that is
%% assigned only to arrows (scope.gleam owns_lexical), and setup_frame calls
%% setup_locals_tuple/6 for those — so it never reaches here. Left unmatched
%% on purpose: seeding call-time This/FnObj/Home/NT into captured slots (which
%% already hold parent box refs from Env, at non-contiguous indices) would be
%% silently wrong; a case_clause crash is the right answer if the compiler-side
%% invariant is ever broken.
setup_locals_seeded(Env, {owned_lexical_slots, _Base},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef)
        when LocalCount >= 4 ->
    list_to_tuple(locals_env4(Env, This, FnObj, Home, NT, Args, Arity, LocalCount, Undef));
setup_locals_seeded(Env, Lexical,
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef) ->
    {LT, LA, LH, LN} = case Lexical of
        {owned_lexical_slots, B} -> {{some, B}, {some, B + 1}, {some, B + 2}, {some, B + 3}};
        no_lexical_slots -> {none, none, none, none}
    end,
    S0 = seed(LN, NT, []),
    S1 = seed(LH, Home, S0),
    S2 = seed(LA, FnObj, S1),
    Seeds = seed(LT, This, S2),
    list_to_tuple(locals_env(Env, Seeds, Args, Arity, LocalCount, Undef)).

seed(none, _Value, Acc) -> Acc;
seed({some, _Idx}, Value, Acc) -> [Value | Acc].

locals_env4([E | Env], This, FnObj, Home, NT, Args, Arity, N, Undef) when N > 4 ->
    [E | locals_env4(Env, This, FnObj, Home, NT, Args, Arity, N - 1, Undef)];
locals_env4([], This, FnObj, Home, NT, Args, Arity, N, Undef) when N >= 4 ->
    [This, FnObj, Home, NT | locals_args(Args, Arity, N - 4, Undef)];
locals_env4(Env, This, FnObj, Home, NT, Args, Arity, N, Undef) ->
    %% Defensive: local_count exhausted mid-env (compiler bounds local_count,
    %% so unreachable in practice) — match the generic truncation semantics.
    locals_env(Env, [This, FnObj, Home, NT], Args, Arity, N, Undef).

locals_env(_, _, _, _, 0, _) -> [];
locals_env([E | Env], Seeds, Args, Arity, N, Undef) ->
    [E | locals_env(Env, Seeds, Args, Arity, N - 1, Undef)];
locals_env([], Seeds, Args, Arity, N, Undef) ->
    locals_seeds(Seeds, Args, Arity, N, Undef).

locals_seeds(_, _, _, 0, _) -> [];
locals_seeds([S | Seeds], Args, Arity, N, Undef) ->
    [S | locals_seeds(Seeds, Args, Arity, N - 1, Undef)];
locals_seeds([], Args, Arity, N, Undef) ->
    locals_args(Args, Arity, N, Undef).

locals_args(_, _, 0, _) -> [];
locals_args(_, 0, N, Undef) -> locals_pad(N, Undef);
locals_args([A | Args], Arity, N, Undef) ->
    [A | locals_args(Args, Arity - 1, N - 1, Undef)];
locals_args([], Arity, N, Undef) ->
    [Undef | locals_args([], Arity - 1, N - 1, Undef)].

locals_pad(0, _) -> [];
locals_pad(N, Undef) -> [Undef | locals_pad(N - 1, Undef)].

unique_positive_integer() ->
    erlang:unique_integer([positive]).

%% Property-creation sequence number (ES §10.1.11 OrdinaryOwnPropertyKeys:
%% non-index string keys enumerate in ascending chronological order of
%% property creation). Strictly increasing across the runtime, so relative
%% order within any one object's property table follows creation order.
%%
%% The `+ ?PROP_SEQ_RESERVED` offset carves out 0..15 as a RESERVED range for
%% the birth-time constant seqs stamped on function objects without paying a
%% counter read (see arc/vm/builtins/common.gleam: fn_length_property = 0,
%% fn_name_property = 1, fn_prototype_property = 2). `unique_integer` starts
%% at 1 in a fresh runtime, so without the offset the very first counter value
%% would collide with the "name" constant. With it, EVERY counter value is
%% strictly greater than EVERY reserved constant, regardless of runtime state.
next_prop_seq() ->
    erlang:unique_integer([monotonic, positive]) + ?PROP_SEQ_RESERVED.

%% Hot-path `obj.x = v` overwrite: update an EXISTING writable data
%% property's value in one map traversal, preserving its flags and creation
%% seq (ES §10.1.11 — an updated key keeps its enumeration position).
%% Mirrors the Gleam representation: a properties Dict is a bare Erlang map
%% and value.DataProperty is {data_property, Value, Writable, Enumerable,
%% Configurable, Seq}. Returns {some, NewProps}, or `none` when the key is
%% absent, non-writable, or an accessor — callers fall back to the full [[Set]]
%% path. That is a MISSING VALUE, not a failed operation, so it speaks Gleam's
%% `Option` (like heap_read/2), never a `Result(_, Nil)`.
put_existing_writable_data(Props, Key, Val) ->
    case Props of
        #{Key := {data_property, _, true, E, C, S}} ->
            {some, Props#{Key := {data_property, Val, true, E, C, S}}};
        _ ->
            none
    end.

%% §7.3.5 CreateDataProperty on an ordinary property table: insert a
%% {W:T, E:T, C:T} data property in one map traversal. A brand-new key is
%% stamped with a fresh creation seq; an existing key keeps its old seq
%% (§10.1.11 — redefinition keeps the enumeration position). Hot path:
%% object literals.
%%
%% maps:find/2 (not two `#{Key := ...}` clauses) so BOTH paths do exactly one
%% lookup — the new-key path is the dominant one here. Both value.Property
%% variants are matched by NAME and full arity, and the only fallthrough is
%% `error` (key absent), so if either record ever grows or reorders a field an
%% existing key raises case_clause instead of reading some other field as the
%% seq and silently scrambling every object's enumeration order.
define_own_data_property(Props, Key, Val) ->
    case maps:find(Key, Props) of
        {ok, {data_property, _V, _W, _E, _C, Seq}} ->
            Props#{Key := new_data_property(Val, Seq)};
        {ok, {accessor_property, _G, _S, _E, _C, Seq}} ->
            Props#{Key := new_data_property(Val, Seq)};
        error ->
            Props#{Key => new_data_property(Val, next_prop_seq())}
    end.

new_data_property(Val, Seq) ->
    {data_property, Val, true, true, true, Seq}.
