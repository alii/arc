%% RegExp for the browser playground, on the page's own JavaScript engine.
%%
%% AtomVM has no `re` (PCRE), so the real arc_regexp_ffi cannot run there.
%% The browser, though, has the one regular-expression engine that matches
%% JS semantics exactly. This shim keeps the real FFI's API and result shapes
%% (regexp.gleam's @externals) and does the matching by round-tripping to the
%% page through arc_js_bridge; the JS half is `__arcJsBridge.regexp*` in
%% website/src/playground/use-atomvm.ts.
%%
%% Offsets in and out are UTF-8 BYTE offsets, as the Gleam side expects; the
%% JS half converts to and from UTF-16 indices. Same offset policy as the real
%% FFI: negatives clamp to 0, past-the-end is offset_out_of_range, a
%% continuation byte is a plain no_match.
-module(arc_regexp_ffi).
-export([regexp_compile/2, is_compiled/1, regexp_exec_compiled/4]).

-define(RS, 30).   %% U+001E record separator inside a payload

%% regexp_compile(Pattern, Flags) ->
%%     {ok, {{js, Pattern, Flags}, GroupCount, [{Name, Index}]}}
%%   | {error, {pattern_compile_failed, Reason}}
regexp_compile(Pattern, Flags) when is_binary(Pattern), is_binary(Flags) ->
    Script = <<"globalThis.__arcJsBridge.regexpCompile(",
               (arc_js_bridge:js_string(Pattern))/binary, ", ",
               (arc_js_bridge:js_string(Flags))/binary, ")">>,
    case arc_js_bridge:call(Script) of
        {ok, Reply} ->
            [CountBin, NamesBin] = binary:split(Reply, <<?RS>>),
            {ok, {{js, Pattern, Flags}, binary_to_integer(CountBin), parse_names(NamesBin)}};
        {error, Reason} ->
            {error, {pattern_compile_failed, Reason}}
    end.

is_compiled({ok, {{js, _, _}, _GroupCount, _Names}}) -> true;
is_compiled({error, {pattern_compile_failed, _Reason}}) -> true;
is_compiled(_) -> false.

%% regexp_exec_compiled(Compiled, String, Offset, Sticky) ->
%%     {ok, {{Start, Len}, [{S, L} | {-1, 0}], GroupCount, Names}}
%%   | {error, no_match | offset_out_of_range | {pattern_compile_failed, _}}
regexp_exec_compiled({error, {pattern_compile_failed, _}} = Err, _S, _O, _Sticky) ->
    Err;
regexp_exec_compiled({ok, {{js, Pattern, Flags}, GroupCount, Names}}, String, Offset, Sticky) ->
    case check_offset(String, Offset) of
        {error, _} = Err ->
            Err;
        {ok, Offset1} ->
            Script = <<"globalThis.__arcJsBridge.regexpExec(",
                       (arc_js_bridge:js_string(Pattern))/binary, ", ",
                       (arc_js_bridge:js_string(Flags))/binary, ", ",
                       (arc_js_bridge:js_string(String))/binary, ", ",
                       (integer_to_binary(Offset1))/binary, ", ",
                       (atom_to_binary(Sticky))/binary, ")">>,
            case arc_js_bridge:call(Script) of
                {ok, <<"nomatch">>} ->
                    {error, no_match};
                {ok, Reply} ->
                    [WholeBin, GroupsBin] = binary:split(Reply, <<?RS>>),
                    Groups = [parse_span(G) || G <- binary:split(GroupsBin, <<";">>, [global]), G =/= <<>>],
                    Padded = Groups ++ lists:duplicate(max(0, GroupCount - length(Groups)), {-1, 0}),
                    {ok, {parse_span(WholeBin), Padded, GroupCount, Names}};
                {error, Reason} ->
                    erlang:error({regexp_bridge_failed, Reason})
            end
    end.

parse_span(Bin) ->
    [S, L] = binary:split(Bin, <<",">>),
    {binary_to_integer(S), binary_to_integer(L)}.

parse_names(<<>>) -> [];
parse_names(Bin) ->
    [begin
         [Name, Idx] = binary:split(Entry, <<"=">>),
         {Name, binary_to_integer(Idx)}
     end || Entry <- binary:split(Bin, <<",">>, [global]), Entry =/= <<>>].

check_offset(String, Offset) when Offset < 0 ->
    check_offset(String, 0);
check_offset(String, Offset) when Offset > byte_size(String) ->
    {error, offset_out_of_range};
check_offset(String, Offset) when Offset < byte_size(String) ->
    case (binary:at(String, Offset) band 16#C0) =:= 16#80 of
        true -> {error, no_match};
        false -> {ok, Offset}
    end;
check_offset(_String, Offset) ->
    {ok, Offset}.
