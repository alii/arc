-module(arc_regexp_ffi).
-export([regexp_exec/4, regexp_test/3]).

%% Convert JS flags to re:compile options
flags_to_opts(Flags) ->
    flags_to_opts(Flags, [unicode]).  %% always enable unicode for UTF-8 strings

flags_to_opts(<<>>, Acc) -> Acc;
flags_to_opts(<<"i", Rest/binary>>, Acc) -> flags_to_opts(Rest, [caseless | Acc]);
flags_to_opts(<<"m", Rest/binary>>, Acc) -> flags_to_opts(Rest, [multiline | Acc]);
flags_to_opts(<<"s", Rest/binary>>, Acc) -> flags_to_opts(Rest, [dotall | Acc]);
flags_to_opts(<<_, Rest/binary>>, Acc) -> flags_to_opts(Rest, Acc).
%% g, y, u, d, v are handled at the Gleam level, not PCRE options

%% regexp_test(Pattern, Flags, String) -> true | false
regexp_test(Pattern, Flags, String) ->
    Opts = flags_to_opts(Flags),
    case re:run(String, Pattern, Opts) of
        {match, _} -> true;
        nomatch -> false
    end.

%% regexp_exec(Pattern, Flags, String, Offset) -> {ok, Matches} | {error, nil}
%% Matches = [{Start, Length}, ...] for full match + captures
regexp_exec(Pattern, Flags, String, Offset) ->
    Opts = [{offset, Offset}, {capture, all, index} | flags_to_opts(Flags)],
    case re:run(String, Pattern, Opts) of
        {match, Captured} -> {ok, Captured};
        nomatch -> {error, nil}
    end.
