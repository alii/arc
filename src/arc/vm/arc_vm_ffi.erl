%% VM-core hot-path FFI: heap/property-table map operations, locals-tuple
%% construction, identity/sequence primitives and the compile-task fork/join.
%% Bound by src/arc/vm/{heap,value,exec/*,ops/object,realm}.gleam and
%% src/arc/compiler/emit.gleam. String helpers live in arc_string_ffi,
%% array/queue backing stores in arc_array_ffi, and CLI stdin/argv in
%% arc_cli_ffi — nothing here does I/O.
-module(arc_vm_ffi).
-export([setup_locals_tuple/6, setup_locals_seeded/10]).
-export([return_code_sentinel/0]).
-export([float_same_term/2]).
-export([unique_positive_integer/0]).
-export([next_prop_seq/0]).
-export([put_existing_writable_data/3]).
-export([define_own_data_property/3]).
-export([run_compile_task/2]).
-export([heap_read/2]).

%% Direct map lookup returning a Gleam Option. Hot path: the VM heap is a
%% Gleam Dict (a bare Erlang map on this target) and read/2 is called for
%% every property access.
heap_read(Map, Key) ->
    case Map of
        #{Key := V} -> {some, V};
        _ -> none
    end.

%% BOUNDARY NOTE: run_compile_task below contains the ONLY `receive` in this
%% module, and it is a sanctioned synchronous spawn-compute-join: the caller
%% spawns a worker, blocks until that same worker replies (or dies), and
%% nothing else can match the monitored ref. It is a structured fork/join
%% barrier, not event-driven mailbox coupling — no external process, timer,
%% or event can ever be observed through it. All event-driven mailbox
%% functions (selective receives, subject messaging, timers) live in the
%% dedicated mailbox FFI modules (src/arc/vm/arc_realm_ffi.erl,
%% src/arc/vm/builtins/arc_waiter_ffi.erl), not here.
%%
%% Run a 0-arity parse/compile task. For big sources the task runs in a
%% short-lived process whose initial heap is sized to the source, for two
%% reasons: parsing allocates large transient structures (token list, AST,
%% IR) that the generational copying GC would otherwise re-copy many times
%% as the live set grows (a 4MB-source eval spent ~3x longer in GC than in
%% actual work), and process death frees everything at once with no sweep.
%% Small sources (the overwhelmingly common case) stay in-process — a spawn
%% plus result copy would cost more than it saves.
-define(COMPILE_TASK_THRESHOLD, 262144).         %% bytes
-define(COMPILE_HEAP_WORDS_PER_BYTE, 16).
-define(COMPILE_HEAP_MAX_WORDS, 134217728).      %% 128M words (~1GB)

run_compile_task(SourceBytes, Task) when SourceBytes < ?COMPILE_TASK_THRESHOLD ->
    Task();
run_compile_task(SourceBytes, Task) ->
    Heap = min(SourceBytes * ?COMPILE_HEAP_WORDS_PER_BYTE,
               ?COMPILE_HEAP_MAX_WORDS),
    Self = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_opt(
        fun() -> Self ! {Ref, Task()} end,
        [monitor, {min_heap_size, Heap}]),
    receive
        {Ref, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, Pid, Reason} ->
            %% The task crashed (it shouldn't — parse/compile return errors
            %% as values). Propagate the same crash to the caller.
            erlang:exit(Reason)
    end.

%% NOTE: an exec-wide min_heap_size floor (raising this process's minimum
%% heap for the duration of bytecode execution) was tried twice and reverted
%% twice: a large floor (8M words) makes the mutator walk a 64MB young heap
%% and costs 40-60% on tight interpreter loops (arith/call microbenches)
%% through cache locality, and it interacts badly with embedders that cap
%% the process with max_heap_size. Keep exec heap flags default; oversized
%% transient parse/compile heaps are handled by run_compile_task above.

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

%% Non-arrow locals build: the emitter allocates owned lexical slots
%% contiguously after the captures in canonical order
%% [this, active_func, home_object, new_target]. Ordinary functions own all
%% four, so the hot clause writes the seed values inline — no intermediate
%% seeds list. Script/eval/module bodies own a subset (e.g. RefThis only);
%% they fall through to the generic per-Some path. LexicalSlots arrives as
%% the Gleam record {lexical_slots, This, ActiveFunc, HomeObject, NewTarget}
%% with {some, SlotIdx} | none fields.
setup_locals_seeded(Env, {lexical_slots, {some, _}, {some, _}, {some, _}, {some, _}},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef)
        when LocalCount >= 4 ->
    list_to_tuple(locals_env4(Env, This, FnObj, Home, NT, Args, Arity, LocalCount, Undef));
setup_locals_seeded(Env, {lexical_slots, LT, LA, LH, LN},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef) ->
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

%% Constant single-opcode `[Return]` bytecode (Gleam's no-field `Return`
%% constructor is the atom `return`). A literal, so it lives in the module
%% literal pool and every call returns the same shared term — used as the
%% sentinel code for native-handler frames in event_loop.gleam.
return_code_sentinel() ->
    {return}.

unique_positive_integer() ->
    erlang:unique_integer([positive]).

%% Property-creation sequence number (ES §10.1.11 OrdinaryOwnPropertyKeys:
%% non-index string keys enumerate in ascending chronological order of
%% property creation). Strictly increasing across the runtime, so relative
%% order within any one object's property table follows creation order.
next_prop_seq() ->
    erlang:unique_integer([monotonic, positive]).

%% Hot-path `obj.x = v` overwrite: update an EXISTING writable data
%% property's value in one map traversal, preserving its flags and creation
%% seq (ES §10.1.11 — an updated key keeps its enumeration position).
%% Mirrors the Gleam representation: a properties Dict is a bare Erlang map
%% and value.DataProperty is {data_property, Value, Writable, Enumerable,
%% Configurable, Seq}. Returns {ok, NewProps}, or {error, nil} when the key
%% is absent, non-writable, or an accessor — callers fall back to the full
%% [[Set]] path.
put_existing_writable_data(Props, Key, Val) ->
    case Props of
        #{Key := {data_property, _, true, E, C, S}} ->
            {ok, Props#{Key := {data_property, Val, true, E, C, S}}};
        _ ->
            {error, nil}
    end.

%% §7.3.5 CreateDataProperty on an ordinary property table: insert a
%% {W:T, E:T, C:T} data property in one map traversal. A brand-new key is
%% stamped with a fresh creation seq; an existing key keeps its old seq
%% (§10.1.11 — redefinition keeps the enumeration position; both Property
%% variants carry seq as their last element). Hot path: object literals.
define_own_data_property(Props, Key, Val) ->
    case Props of
        #{Key := Old} ->
            Props#{Key := {data_property, Val, true, true, true,
                           element(tuple_size(Old), Old)}};
        _ ->
            Props#{Key => {data_property, Val, true, true, true,
                           erlang:unique_integer([monotonic, positive])}}
    end.
