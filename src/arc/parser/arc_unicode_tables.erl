%% The decode + cache layer in front of the GENERATED Unicode 17 tables in
%% arc_regex_uni17_ffi. Everything here is HAND-WRITTEN, and lives outside the
%% generated file so regenerating the tables can never delete it.
%%
%% arc_regex_uni17_ffi is data only: it hands out hex-packed binaries. This
%% module owns the two packings' decoders and memoizes each decoded table in
%% persistent_term, so a table is walked ONCE per key per node instead of once
%% per \p{...} escape of every regex compile.
-module(arc_unicode_tables).
-export([decoded_ranges/1, range_tuple/1, string_members/1]).

%% decoded_ranges(Key) -> [{Lo, Hi}] | none
%% The property's inclusive codepoint ranges (sorted, disjoint), or `none` when
%% the generated tables carry no exact data for the key.
decoded_ranges(Key) ->
    cached({ranges, Key}, fun() -> decode(ranges, arc_regex_uni17_ffi:ranges(Key)) end).

%% range_tuple(Key) -> tuple of {Lo, Hi} | none
%% Same table as decoded_ranges/1, as a tuple, for O(log n) binary-search
%% membership tests (arc_unicode_ffi's ID_Start / ID_Continue).
range_tuple(Key) ->
    cached({range_tuple, Key},
           fun() ->
                   case decoded_ranges(Key) of
                       none -> none;
                       Ranges -> list_to_tuple(Ranges)
                   end
           end).

%% string_members(Name) -> [[Codepoint]] | none
%% The members of a binary property of strings (\p{RGI_Emoji} & friends), as
%% codepoint sequences. `none` when Name is not a property of strings.
string_members(Name) ->
    cached({string_members, Name},
           fun() -> decode(members, arc_regex_uni17_ffi:string_members(Name)) end).

%% ---- Cache ---------------------------------------------------------------
%%
%% persistent_term, not the process dictionary: the tables are derived from
%% immutable generated data, so they are the same in every process. Keys come
%% from the fixed, generated property tables, so the cache is bounded.
cached(Key, Compute) ->
    Term = {?MODULE, Key},
    case persistent_term:get(Term, undefined) of
        undefined ->
            Value = Compute(),
            persistent_term:put(Term, Value),
            Value;
        Value ->
            Value
    end.

%% ---- Decoders (mirror the packings documented in arc_regex_uni17_ffi) -----

decode(_What, none) -> none;
decode(ranges, Hex) -> decode_ranges(Hex);
decode(members, Hex) -> decode_members(Hex).

%% 12 hex chars per range: 6 for the low bound, 6 for the high.
decode_ranges(<<>>) -> [];
decode_ranges(<<Lo:6/binary, Hi:6/binary, Rest/binary>>) ->
    [{binary_to_integer(Lo, 16), binary_to_integer(Hi, 16)}
     | decode_ranges(Rest)].

%% Per sequence: a 2-hex-digit codepoint count, then that many 6-hex-digit
%% codepoints.
decode_members(<<>>) -> [];
decode_members(<<Count:2/binary, Rest/binary>>) ->
    Width = binary_to_integer(Count, 16) * 6,
    <<Seq:Width/binary, Rest2/binary>> = Rest,
    [decode_codepoints(Seq) | decode_members(Rest2)].

decode_codepoints(<<>>) -> [];
decode_codepoints(<<CP:6/binary, Rest/binary>>) ->
    [binary_to_integer(CP, 16) | decode_codepoints(Rest)].
