-module(arc_vm_ffi).
-export([read_line/1]).
-export([array_get/2, array_set/3, array_repeat/2]).
-export([array_unsafe_get/2, array_set_unchecked/3]).
-export([tree_array_new/1, tree_array_from_list/2, tree_array_to_list/1,
         tree_array_get/2, tree_array_get_option/2, tree_array_set/3,
         tree_array_size/1, tree_array_resize/2,
         tree_array_reset/2, tree_array_sparse_fold/3]).
-export([send_message/2, receive_message_infinite/0, receive_message_timeout/1, pid_to_string/1]).
-export([receive_any_event/0, receive_settle_only/0, send_after/3, cancel_timer/1]).
-export([receive_user_message/0, receive_user_message_timeout/1]).
-export([get_script_args/0, sleep/1]).
-export([string_char_at/2, string_codepoint_length/1]).
-export([job_queue_new/0, job_queue_push/2, job_queue_pop/1]).

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
string_char_at(Bin, Idx) when Idx >= 0 ->
    char_at_skip(Bin, Idx);
string_char_at(_, _) -> none.

char_at_skip(<<C/utf8, _/binary>>, 0) -> {some, <<C/utf8>>};
char_at_skip(<<_/utf8, Rest/binary>>, N) -> char_at_skip(Rest, N - 1);
char_at_skip(_, _) -> none.

string_codepoint_length(Bin) -> cp_length(Bin, 0).
cp_length(<<>>, N) -> N;
cp_length(<<_/utf8, Rest/binary>>, N) -> cp_length(Rest, N + 1);
cp_length(<<_, Rest/binary>>, N) -> cp_length(Rest, N + 1).

%% Process primitives for Arc.send/receive
send_message(Pid, Msg) -> Pid ! Msg, nil.
receive_message_infinite() ->
    receive Msg -> Msg end.
receive_message_timeout(Timeout) ->
    receive Msg -> {ok, Msg}
    after Timeout -> {error, nil}
    end.
pid_to_string(Pid) -> list_to_binary(pid_to_list(Pid)).
get_script_args() -> [list_to_binary(A) || A <- init:get_plain_arguments()].
sleep(Ms) -> timer:sleep(Ms), nil.

%% Selective receive for the event loop. Gleam MailboxEvent variants compile to
%% tagged tuples: UserMessage(pm) = {user_message, Pm},
%% SettlePromise(ref, outcome) = {settle_promise, Ref, Outcome}.
%%
%% When the event loop has pending receivers it accepts any event. Otherwise it
%% only accepts settle_promise, leaving user_message in the mailbox for blocking
%% Arc.receive() or a future receiveAsync() call to pick up.
receive_any_event() ->
    receive
        {user_message, _} = E -> E;
        {settle_promise, _, _} = E -> E;
        {receiver_timeout, _} = E -> E
    end.
receive_settle_only() ->
    receive
        {settle_promise, _, _} = E -> E;
        {receiver_timeout, _} = E -> E
    end.

%% Selective receive for Arc.receive() — only matches user_message, leaves
%% settle_promise in the mailbox for the event loop.
receive_user_message() ->
    receive
        {user_message, Pm} -> Pm
    end.
receive_user_message_timeout(Timeout) ->
    receive
        {user_message, Pm} -> {ok, Pm}
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
