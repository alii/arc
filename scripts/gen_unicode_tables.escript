#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Writes src/arc/parser/arc_regex_uni17_ffi.erl: the Unicode 17 property
%% tables as plain Erlang literals (lists/tuples of {Lo, Hi} codepoint ranges,
%% codepoint-sequence lists, and the ready-to-splice PCRE alternations).
%%
%% Input is scripts/unicode/arc_regex_uni17_packed.erl, the hex-packed module
%% generated from the test262 property-escapes corpus. That file stays the
%% source of truth; this script only unpacks it once at build time so nothing
%% has to decode or memoize at run time. The scf domain (codepoints whose
%% simple case fold differs from themselves) is derived here as well, from
%% Changes_When_Casefolded and arc_regex_charset:scf/1.
%%
%% Run from the repository root:  escript scripts/gen_unicode_tables.escript

-mode(compile).

main(_) ->
    Packed = "scripts/unicode/arc_regex_uni17_packed.erl",
    Charset = "src/arc/vm/builtins/arc_regex_charset.erl",
    Out = "src/arc/parser/arc_regex_uni17_ffi.erl",
    ok = load(Packed),
    ok = load(Charset),
    {ok, Src} = file:read_file(Packed),
    RangeKeys = clause_keys(Src, <<"ranges">>),
    StringKeys = clause_keys(Src, <<"strings">>),
    MemberKeys = clause_keys(Src, <<"string_members">>),
    Ranges = [{K, decode_ranges(arc_regex_uni17_packed:ranges(K))} || K <- RangeKeys],
    Members = [{K, decode_members(arc_regex_uni17_packed:string_members(K))}
               || K <- MemberKeys],
    Strings = [{K, arc_regex_uni17_packed:strings(K)} || K <- StringKeys],
    {_, Cwcf} = lists:keyfind(<<"bin:Changes_When_Casefolded">>, 1, Ranges),
    ScfDomain = merge([{C, C} || {Lo, Hi} <- Cwcf, C <- lists:seq(Lo, Hi),
                                 arc_regex_charset:scf(C) =/= C]),
    TupleKeys = [<<"bin:ID_Start">>, <<"bin:ID_Continue">>],
    Body =
        [header(),
         [[fn_clause("ranges", K, ranges_lit(R)), ";\n"] || {K, R} <- Ranges],
         "ranges(_) -> none.\n\n",
         [[fn_clause("range_tuple", K, ["{", ranges_items(element(2, lists:keyfind(K, 1, Ranges))), "}"]),
           ";\n"] || K <- TupleKeys],
         "range_tuple(_) -> none.\n\n",
         [[fn_clause("strings", K, binary_lit(S)), ";\n"] || {K, S} <- Strings],
         "strings(_) -> none.\n\n",
         [[fn_clause("string_members", K, members_lit(M)), ";\n"] || {K, M} <- Members],
         "string_members(_) -> none.\n\n",
         "scf_domain() ->\n    ", ranges_lit(ScfDomain), ".\n"],
    ok = file:write_file(Out, Body),
    io:format("wrote ~ts (~b range keys, ~b string properties, ~b scf ranges)~n",
              [Out, length(Ranges), length(Members), length(ScfDomain)]).

load(Path) ->
    {ok, Mod, Bin} = compile:file(Path, [binary, report_errors]),
    {module, Mod} = code:load_binary(Mod, Path, Bin),
    ok.

%% Every literal binary key a function is defined on, in source order.
clause_keys(Src, Name) ->
    {ok, Re} = re:compile(<<"^", Name/binary, "\\(<<\"([^\"]+)\">>\\) ->">>,
                          [multiline]),
    {match, Ms} = re:run(Src, Re, [global, {capture, all_but_first, binary}]),
    [K || [K] <- Ms].

decode_ranges(<<>>) -> [];
decode_ranges(<<Lo:6/binary, Hi:6/binary, Rest/binary>>) ->
    [{binary_to_integer(Lo, 16), binary_to_integer(Hi, 16)} | decode_ranges(Rest)].

decode_members(<<>>) -> [];
decode_members(<<Count:2/binary, Rest/binary>>) ->
    Width = binary_to_integer(Count, 16) * 6,
    <<Seq:Width/binary, Rest2/binary>> = Rest,
    [decode_codepoints(Seq) | decode_members(Rest2)].

decode_codepoints(<<>>) -> [];
decode_codepoints(<<CP:6/binary, Rest/binary>>) ->
    [binary_to_integer(CP, 16) | decode_codepoints(Rest)].

merge(Ranges) -> merge_sorted(lists:sort(Ranges)).

merge_sorted([{Lo, Hi}, {Lo2, Hi2} | Rest]) when Lo2 =< Hi + 1 ->
    merge_sorted([{Lo, max(Hi, Hi2)} | Rest]);
merge_sorted([R | Rest]) -> [R | merge_sorted(Rest)];
merge_sorted([]) -> [].

fn_clause(Name, Key, Lit) ->
    [Name, "(<<\"", Key, "\">>) ->\n    ", Lit].

ranges_lit(Ranges) -> ["[", ranges_items(Ranges), "]"].

ranges_items(Ranges) ->
    wrap([io_lib:format("{~b,~b}", [Lo, Hi]) || {Lo, Hi} <- Ranges]).

members_lit(Members) ->
    ["[", wrap([["[", lists:join(",", [integer_to_list(C) || C <- Seq]), "]"]
                || Seq <- Members]), "]"].

%% An ASCII binary as adjacent string segments inside one <<...>>, ~90
%% columns each. Segments are cut from the raw bytes and escaped one by one,
%% so a cut never lands inside an escape sequence.
binary_lit(Bin) ->
    Segs = [escape(Seg) || Seg <- segments(Bin, 88)],
    ["<<\"", lists:join("\"\n      \"", Segs), "\">>"].

segments(Bin, N) when byte_size(Bin) =< N -> [Bin];
segments(Bin, N) ->
    <<Seg:N/binary, Rest/binary>> = Bin,
    [Seg | segments(Rest, N)].

escape(Seg) ->
    [case C of
         $\\ -> "\\\\";
         $" -> "\\\"";
         _ when C >= 32, C < 127 -> C
     end || <<C>> <= Seg].

%% Comma-join items, breaking the line every ~90 columns.
wrap(Items) -> wrap(Items, 0, []).

wrap([], _Col, Acc) -> lists:reverse(Acc);
wrap([Item | Rest], Col, Acc) ->
    Flat = iolist_to_binary(Item),
    W = byte_size(Flat),
    case Acc of
        [] -> wrap(Rest, W, [Flat]);
        _ when Col + W + 1 > 90 -> wrap(Rest, W + 5, [Flat, ",\n     " | Acc]);
        _ -> wrap(Rest, Col + W + 1, [Flat, "," | Acc])
    end.

header() ->
    ["%% Exact Unicode 17.0.0 extents for the JS RegExp \\p{...} properties where\n"
     "%% OTP's PCRE2 tables (Unicode 16) disagree with ECMA-262 / test262, plus the\n"
     "%% binary properties of strings (emoji sequences) PCRE2 cannot express.\n"
     "%%\n"
     "%% GENERATED by scripts/gen_unicode_tables.escript from\n"
     "%% scripts/unicode/arc_regex_uni17_packed.erl -- do not edit by hand.\n"
     "%%\n"
     "%% Every function returns a module literal, so the tables live once in the\n"
     "%% code's constant pool and are never rebuilt or copied per process.\n"
     "%%\n"
     "%% ranges(Key) -> [{Lo, Hi}] inclusive codepoint ranges, sorted and disjoint,\n"
     "%% with the surrogate block D800-DFFF removed. `none` for a key with no exact\n"
     "%% data. Keys: <<\"gc:Lu\">> | <<\"sc:Latin\">> | <<\"scx:Latin\">> | <<\"bin:Alphabetic\">>.\n"
     "%%\n"
     "%% range_tuple(Key) -> the same ranges as a tuple, for binary-search membership\n"
     "%% (the lexer's ID_Start / ID_Continue only).\n"
     "%%\n"
     "%% strings(Name) -> a ready-to-splice PCRE2 group matching the\n"
     "%% property-of-strings' sequences, longest sequence first at every branch.\n"
     "%%\n"
     "%% string_members(Name) -> the same sequences as codepoint lists. `none` for a\n"
     "%% name that is not a property of strings.\n"
     "%%\n"
     "%% scf_domain() -> the codepoints c with scf(c) =/= c, as ranges.\n"
     "-module(arc_regex_uni17_ffi).\n"
     "-export([ranges/1, range_tuple/1, strings/1, string_members/1, scf_domain/0]).\n\n"].
