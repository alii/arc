-module(arc_vm_ffi).
-export([read_line/1]).
-export([array_get/2, array_set/3, array_repeat/2]).
-export([array_unsafe_get/2, array_set_unchecked/3]).
-export([tree_array_new/1, tree_array_from_list/2, tree_array_to_list/1,
         tree_array_get/2, tree_array_get_option/2, tree_array_set/3,
         tree_array_size/1, tree_array_resize/2,
         tree_array_reset/2, tree_array_sparse_fold/3]).
-export([pid_to_string/1]).
-export([receive_settle_only/0, receive_settle_or_subject/1, send_after/3, cancel_timer/1]).
-export([receive_settle_only_timeout/1, receive_settle_or_subject_timeout/2]).
-export([get_script_args/0, sleep/1]).
-export([send_subject_message/3, receive_subject_message/1, receive_subject_message_timeout/2]).
-export([select_message/1, select_message_timeout/2]).
-export([string_char_at/2, string_codepoint_at/2, string_codepoint_length/1,
         replacement_codepoint/0]).
-export([string_index_of/3, string_last_index_of/3]).
-export([string_cp_slice/3, string_cp_drop/2, string_cp_explode/1]).
-export([job_queue_new/0, job_queue_push/2, job_queue_pop/1]).
-export([setup_locals_tuple/6, setup_locals_seeded/10]).
-export([return_code_sentinel/0]).
-export([float_same_term/2]).
-export([unique_positive_integer/0]).
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
read_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> {error, nil};
        {error, _} -> {error, nil};
        Line -> {ok, Line}
    end.

%% Array (tuple-backed) operations
array_get(Index, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {some, element(Index + 1, Tuple)};
        false -> none
    end.
array_set(Index, Value, Tuple) ->
    case Index >= 0 andalso Index < tuple_size(Tuple) of
        true -> {ok, setelement(Index + 1, Tuple, Value)};
        false -> {error, nil}
    end.

%% Unchecked variants for hot-path reads where the compiler guarantees
%% the index is in bounds (bytecode fetch, constant pool, locals). No
%% bounds check, no Option box — badarg on violation.
array_unsafe_get(Index, Tuple) ->
    element(Index + 1, Tuple).
array_set_unchecked(Index, Value, Tuple) ->
    setelement(Index + 1, Tuple, Value).
%% Cap tuple-backed arrays at 10M elements (~80MB on 64-bit).
%% JS specs allow arrays up to 2^32-1 but we use a sparse dict for those.
%% Keep in sync with limits.max_iteration in src/arc/vm/limits.gleam.
-define(MAX_DENSE_ELEMENTS, 10000000).

array_repeat(Value, Count) when Count =< ?MAX_DENSE_ELEMENTS ->
    erlang:make_tuple(Count, Value);
array_repeat(_Value, _Count) ->
    erlang:error(array_too_large).

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

%% Erlang's array module — O(log n) functional array for JS elements.
%% Default is the caller-provided JsUndefined so unset slots and to_list
%% both return valid JsValues (no atom sentinel leaks into Gleam).
tree_array_new(Default) ->
    array:new({default, Default}).
tree_array_from_list(List, Default) ->
    array:from_list(List, Default).
tree_array_to_list(A) ->
    array:to_list(A).
tree_array_get(Index, A) when Index >= 0 ->
    array:get(Index, A);
tree_array_get(_Index, A) ->
    array:default(A).
%% DenseElements uses JsUninitialized as default so holes (reset slots) are
%% distinguishable from explicit `arr[i] = undefined`. A slot that equals
%% default means "hole" → none. Out-of-bounds/negative → none.
tree_array_get_option(Index, A) when Index >= 0 ->
    case Index < array:size(A) of
        true ->
            V = array:get(Index, A),
            case V =:= array:default(A) of
                true -> none;
                false -> {some, V}
            end;
        false -> none
    end;
tree_array_get_option(_Index, _A) ->
    none.
tree_array_set(Index, Value, A) when Index >= 0, Index < ?MAX_DENSE_ELEMENTS ->
    array:set(Index, Value, A);
tree_array_set(_Index, _Value, A) ->
    A.
tree_array_size(A) ->
    array:size(A).
tree_array_resize(A, NewSize) when NewSize >= 0 ->
    array:resize(NewSize, A);
tree_array_resize(A, _NewSize) ->
    A.
%% Reset slot to default (creates a hole). O(log n). Out-of-bounds is a no-op.
tree_array_reset(Index, A) when Index >= 0 ->
    case Index < array:size(A) of
        true -> array:reset(Index, A);
        false -> A
    end;
tree_array_reset(_Index, A) ->
    A.
%% Fold over non-default entries only. Skips holes. O(k) where k = set count.
tree_array_sparse_fold(F, Acc, A) ->
    array:sparse_foldl(F, Acc, A).

%% Job queue — Erlang's queue module (two-list Okasaki FIFO). O(1) amortized
%% in/out vs the previous List+append O(n) per enqueue.
job_queue_new() -> queue:new().
job_queue_push(Q, Item) -> queue:in(Item, Q).
job_queue_pop(Q) ->
    case queue:out(Q) of
        {{value, Item}, Q2} -> {some, {Item, Q2}};
        {empty, _} -> none
    end.

%% Fast string indexing by codepoint (not grapheme cluster). Gleam's
%% string.slice/string.length do grapheme segmentation via unicode_util:gc
%% which is ~20x slower and spec-incorrect for JS (which uses UTF-16 code
%% units). Codepoints are closer to correct and far cheaper.
%%
%% TODO(Deviation): still not fully spec-correct — JS indexes by UTF-16
%% code unit, so astral-plane chars (U+10000+) should count as 2 indices.
%% A full fix needs UTF-16 string storage. Codepoint indexing matches
%% grapheme indexing for all BMP chars so this is strictly more correct
%% than the previous string.slice approach.
%%
%% Both entry points keep a one-entry per-process cache so the canonical
%% JS idiom `for (i = 0; i < s.length; i++) use(s[i])` is O(n) instead of
%% O(n^2):
%%   - string_codepoint_length caches {Bin, Len}: the first call scans,
%%     repeat calls on the same string are O(1).
%%   - string_char_at keeps a cursor {Bin, CharIdx, ByteOffset} and resumes
%%     the UTF-8 walk from the last position for forward access, so a
%%     sequential scan advances one codepoint per call instead of re-walking
%%     from byte 0 (O(i) per access).
%% Cache hits use `=:=`, which is O(1) when the argument is the identical
%% heap term (the common case: the same JsString value threaded through the
%% interpreter loop) and fails fast on different strings (size compared
%% first). Binaries are immutable, so a hit can never be stale; misses just
%% replace the entry. Caches are per-process, so concurrent VM processes
%% (generators, actors) are isolated and merely start cold.
-define(STR_LEN_CACHE, '$arc_str_len_cache').
-define(STR_POS_CACHE, '$arc_str_pos_cache').

string_char_at(Bin, Idx) ->
    case string_codepoint_at(Bin, Idx) of
        {some, C} -> {some, <<C/utf8>>};
        none -> none
    end.

%% Same cursor-cached walk as string_char_at, but returns the codepoint as an
%% integer — for String.prototype.codePointAt, where building even a one-char
%% binary per call would be wasted allocation.
string_codepoint_at(Bin, Idx) when Idx >= 0 ->
    {Base, Skip} =
        case get(?STR_POS_CACHE) of
            {B, CIdx, COff} when B =:= Bin, CIdx =< Idx -> {COff, Idx - CIdx};
            _ -> {0, Idx}
        end,
    <<_:Base/binary, Rest/binary>> = Bin,
    case char_at_skip(Rest, Skip, Base) of
        {Char, Off} ->
            put(?STR_POS_CACHE, {Bin, Idx, Off}),
            {some, Char};
        none -> none
    end;
string_codepoint_at(_, _) -> none.

%% Walk N codepoints forward, returning the integer codepoint there plus its
%% byte offset (for the cursor cache). Off accumulates from the caller's base.
char_at_skip(<<C/utf8, _/binary>>, 0, Off) -> {C, Off};
char_at_skip(<<C/utf8, Rest/binary>>, N, Off) ->
    char_at_skip(Rest, N - 1, Off + cp_byte_size(C));
char_at_skip(_, _, _) -> none.

%% UTF-8 encoded byte length of a codepoint.
cp_byte_size(C) when C < 16#80 -> 1;
cp_byte_size(C) when C < 16#800 -> 2;
cp_byte_size(C) when C < 16#10000 -> 3;
cp_byte_size(_) -> 4.

%% U+FFFD REPLACEMENT CHARACTER. UtfCodepoint is an integer on the Erlang
%% target, so this is a constant-pool literal — no Result/assert overhead.
replacement_codepoint() -> 16#FFFD.

string_codepoint_length(Bin) ->
    case get(?STR_LEN_CACHE) of
        {B, Len} when B =:= Bin -> Len;
        _ ->
            Len = cp_length(Bin, 0),
            put(?STR_LEN_CACHE, {Bin, Len}),
            Len
    end.
%% W:56 clause: 7 ASCII bytes per step, small-int safe (see cp_drop).
cp_length(<<W:56, Rest/binary>>, N)
    when W band 16#80808080808080 =:= 0 ->
    cp_length(Rest, N + 7);
cp_length(<<>>, N) -> N;
cp_length(<<_/utf8, Rest/binary>>, N) -> cp_length(Rest, N + 1);
cp_length(<<_, Rest/binary>>, N) -> cp_length(Rest, N + 1).

%% O(n) StringIndexOf: skip From codepoints to a byte offset, run
%% binary:match (Boyer-Moore BIF) over the remaining scope, convert the
%% match's byte position back to a codepoint index. Caller handles the
%% empty-needle case (binary:match badargs on <<>>).
string_index_of(Hay, Needle, From) ->
    Start = cp_byte_offset(Hay, max(From, 0)),
    case binary:match(Hay, Needle, [{scope, {Start, byte_size(Hay) - Start}}]) of
        nomatch -> -1;
        {BytePos, _} -> cp_length(binary:part(Hay, 0, BytePos), 0)
    end.

%% O(n) reverse StringIndexOf: restrict to the first (From + |Needle|_cp)
%% codepoints, ask string:find/3 for the trailing (last) occurrence —
%% handles overlapping needles — then count codepoints before the match.
string_last_index_of(Hay, Needle, From) ->
    Limit = cp_byte_offset(Hay, max(From, 0) + cp_length(Needle, 0)),
    Prefix = binary:part(Hay, 0, Limit),
    case string:find(Prefix, Needle, trailing) of
        nomatch -> -1;
        Suffix -> cp_length(binary:part(Hay, 0, byte_size(Prefix) - byte_size(Suffix)), 0)
    end.

%% Codepoint-based substring: Len codepoints starting at codepoint Start.
%% Plain UTF-8 byte walk + binary:part — returns a sub-binary referencing
%% the original, so no per-character allocation (vs gleam/string.slice's
%% grapheme clustering which allocates a list cell per character).
string_cp_slice(Bin, Start, Len) when Start >= 0, Len > 0 ->
    Off = cp_off(Bin, Start, 0),
    <<_:Off/binary, Rest/binary>> = Bin,
    binary:part(Bin, Off, cp_off(Rest, Len, 0));
string_cp_slice(_, _, _) -> <<>>.

%% Drop the first N codepoints; sub-binary, alloc-free walk.
string_cp_drop(Bin, N) when N > 0 ->
    Off = cp_off(Bin, N, 0),
    binary:part(Bin, Off, byte_size(Bin) - Off);
string_cp_drop(Bin, _) -> Bin.

%% Split into single-codepoint binaries (String.prototype.split("")).
string_cp_explode(Bin) -> cp_explode(Bin, []).
cp_explode(<<>>, Acc) -> lists:reverse(Acc);
cp_explode(<<C/utf8, Rest/binary>>, Acc) -> cp_explode(Rest, [<<C/utf8>> | Acc]);
cp_explode(<<B, Rest/binary>>, Acc) -> cp_explode(Rest, [<<B>> | Acc]).

%% Byte offset after skipping N codepoints (clamps at end). Alloc-free.
cp_byte_offset(Bin, N) -> cp_off(Bin, N, 0).

%% Codepoint-skip walker. Returns the byte offset (an integer, never the
%% binary) and every clause begins with a binary match — both are required
%% for BEAM's match-context reuse (a leading non-binary clause forces a
%% sub-binary allocation on every step; verify with erlc +bin_opt_info).
%% The W:56 clause batches 7 ASCII bytes per step (high bit of every byte
%% clear means 7 one-byte codepoints; 56 bits stays an immediate small
%% int — 64 would allocate a bignum per step). Non-ASCII steps skip by
%% UTF-8 lead byte class without decoding the codepoint. Invalid lead
%% bytes advance one byte, matching cp_length's per-byte fallback.
cp_off(<<W:56, R/binary>>, N, Off)
    when N >= 7, W band 16#80808080808080 =:= 0 ->
    cp_off(R, N - 7, Off + 7);
cp_off(<<C, R/binary>>, N, Off) when N >= 1, C < 16#80 ->
    cp_off(R, N - 1, Off + 1);
cp_off(<<C, _, R/binary>>, N, Off) when N >= 1, C >= 16#C0, C < 16#E0 ->
    cp_off(R, N - 1, Off + 2);
cp_off(<<C, _, _, R/binary>>, N, Off) when N >= 1, C >= 16#E0, C < 16#F0 ->
    cp_off(R, N - 1, Off + 3);
cp_off(<<C, _, _, _, R/binary>>, N, Off) when N >= 1, C >= 16#F0 ->
    cp_off(R, N - 1, Off + 4);
cp_off(<<_, R/binary>>, N, Off) when N >= 1 -> cp_off(R, N - 1, Off + 1);
cp_off(_, _, Off) -> Off.

pid_to_string(Pid) -> list_to_binary(pid_to_list(Pid)).
get_script_args() -> [list_to_binary(A) || A <- init:get_plain_arguments()].
sleep(Ms) -> timer:sleep(Ms), nil.

%% Event loop selective receive. Accepts SettlePromise, ReceiverTimeout,
%% and optionally subject messages whose ref is in RefMap.
receive_settle_only() ->
    receive
        {settle_promise, _, _} = E -> E;
        {receiver_timeout, _} = E -> E
    end.
receive_settle_or_subject(RefMap) ->
    receive
        {settle_promise, _, _} = E -> E;
        {receiver_timeout, _} = E -> E;
        {Ref, Pm} when is_map_key(Ref, RefMap) -> {subject_message, Ref, Pm}
    end.

%% Timeout variants for embedder loops with pending host-timer / atomics
%% deadlines: {error, nil} on timeout means a deadline is due — re-drain.
receive_settle_only_timeout(Timeout) ->
    receive
        {settle_promise, _, _} = E -> {ok, E};
        {receiver_timeout, _} = E -> {ok, E}
    after Timeout -> {error, nil}
    end.
receive_settle_or_subject_timeout(RefMap, Timeout) ->
    receive
        {settle_promise, _, _} = E -> {ok, E};
        {receiver_timeout, _} = E -> {ok, E};
        {Ref, Pm} when is_map_key(Ref, RefMap) -> {ok, {subject_message, Ref, Pm}}
    after Timeout -> {error, nil}
    end.

%% Subject-based selective receive. Messages are {Ref, PortableMessage} tuples
%% where Ref is the subject's unique erlang:make_ref() tag.
%% The BEAM optimizes receive on a bound ref by skipping older messages.
send_subject_message(Pid, Ref, Msg) ->
    Pid ! {Ref, Msg}, nil.
receive_subject_message(Ref) ->
    receive {Ref, Pm} -> Pm end.
receive_subject_message_timeout(Ref, Timeout) ->
    receive {Ref, Pm} -> {ok, Pm}
    after Timeout -> {error, nil}
    end.

%% Multi-subject select: RefMap is #{Ref1 => true, Ref2 => true, ...}.
%% Uses is_map_key/2 guard BIF for dynamic selective receive.
select_message(RefMap) ->
    receive {Ref, _} = Msg when is_map_key(Ref, RefMap) -> Msg end.
select_message_timeout(RefMap, Timeout) ->
    receive {Ref, _} = Msg when is_map_key(Ref, RefMap) -> {ok, Msg}
    after Timeout -> {error, nil}
    end.

send_after(Ms, Pid, Msg) ->
    erlang:send_after(Ms, Pid, Msg).

%% Returns true if the timer was still active (cancelled successfully),
%% false if it had already fired.
cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
        false -> false;
        _TimeLeft -> true
    end.

unique_positive_integer() ->
    erlang:unique_integer([positive]).

