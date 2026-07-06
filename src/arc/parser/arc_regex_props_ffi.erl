%% Unicode property escape POLICY for JS RegExp \p{...} / \P{...}: which key a
%% property name resolves to, and how it is rendered. Hand-written; the tables
%% it reads are generated (arc_regex_prop_tables_ffi for the name aliases,
%% arc_regex_uni17_ffi via arc_unicode_tables for the codepoint data).
%%
%% Used by:
%%  - the parser (via arc/parser/regex.gleam) to strictly validate property
%%    names/values at parse time (exact-case match, no loose matching), and
%%  - arc_regexp_ffi to translate JS property escapes for the `re` module.
%%
%% Translation prefers the exact Unicode 17 range tables in
%% arc_regex_uni17_ffi (generated from the test262 corpus) and expands them
%% into explicit \x{..}-\x{..} classes, because OTP's PCRE2 ships older
%% Unicode tables (16.0 as of OTP 29) whose property data disagrees with
%% ECMA-262. Properties absent from those tables (i.e. where PCRE2's data
%% already matches Unicode 17) fall back to the PCRE2 property syntax
%% (long GC names -> short codes, Script=X -> sc:X, Script_Extensions=X ->
%% scx:X, Assigned -> negated Cn).
-module(arc_regex_props_ffi).
-export([classify_lone/1, classify_pair/2, translate_lone/4, translate_pair/4,
         char_set/1, string_list/1]).

%% ---- Property-name resolution ----
%%
%% Every consumer below (parse-time classification, PCRE translation, exact
%% range lookup) asks the same two questions of a \p{...} payload. They ask
%% them HERE, once, instead of each re-deriving the gc -> binary_prop ->
%% string_prop chain and the gc/sc/scx guard triple.

%% resolve_lone(Name) -> {gc, ShortCode}
%%                     | {binary, Key, PcreName, Complement, PcreSupported}
%%                     | strings
%%                     | invalid
%% A lone name inside \p{...}: a General_Category value, a binary property,
%% or (v-flag only) a binary property of strings.
%%
%% For a binary property the resolution ALREADY carries the exact-data lookup
%% key (<<"bin:Emoji">>, <<"gc:Cn">>, ...) so no consumer has to re-derive the
%% prefix. `Complement` says the property is the COMPLEMENT of the data at Key
%% -- true only for the complement aliases (Assigned = \P{Cn}), whose data
%% lives under the underlying General_Category key rather than a "bin:" one.
%% `PcreSupported` says whether PCRE2's own tables know the property, i.e.
%% whether the \p{...} fallback is usable when we have no exact ranges.
resolve_lone(Name) ->
    case arc_regex_prop_tables_ffi:gc_value(Name) of
        invalid ->
            case arc_regex_prop_tables_ffi:binary_prop(Name) of
                invalid ->
                    case string_prop(Name) of
                        true -> strings;
                        false -> invalid
                    end;
                {Pcre, true, Supported} ->
                    {binary, <<"gc:", Pcre/binary>>, Pcre, true, Supported};
                {Pcre, false, Supported} ->
                    {binary, <<"bin:", Pcre/binary>>, Pcre, false, Supported}
            end;
        Short -> {gc, Short}
    end.

%% resolve_pair(Name, Value) -> {gc, ShortCode}
%%                            | {sc, Canonical, PcreSupported}
%%                            | {scx, Canonical, PcreSupported}
%%                            | invalid
%% Name=Value inside \p{...}: only gc/sc/scx (and their long forms) may take a
%% value, per ECMA-262 table-nonbinary-unicode-properties. Script and
%% Script_Extensions are probed separately at generation time, hence two flags
%% on script_value/1; every General_Category value is known to PCRE2.
resolve_pair(Name, Value)
  when Name =:= <<"General_Category">>; Name =:= <<"gc">> ->
    case arc_regex_prop_tables_ffi:gc_value(Value) of
        invalid -> invalid;
        Short -> {gc, Short}
    end;
resolve_pair(Name, Value)
  when Name =:= <<"Script">>; Name =:= <<"sc">> ->
    case arc_regex_prop_tables_ffi:script_value(Value) of
        invalid -> invalid;
        {Canon, ScOk, _ScxOk} -> {sc, Canon, ScOk}
    end;
resolve_pair(Name, Value)
  when Name =:= <<"Script_Extensions">>; Name =:= <<"scx">> ->
    case arc_regex_prop_tables_ffi:script_value(Value) of
        invalid -> invalid;
        {Canon, _ScOk, ScxOk} -> {scx, Canon, ScxOk}
    end;
resolve_pair(_, _) ->
    invalid.

%% ---- Public API ----

%% classify_lone(Name) -> prop_valid | prop_string | prop_invalid
classify_lone(Name) ->
    case resolve_lone(Name) of
        invalid -> prop_invalid;
        strings -> prop_string;
        _ -> prop_valid
    end.

%% classify_pair(Name, Value) -> prop_valid | prop_invalid
classify_pair(Name, Value) ->
    case resolve_pair(Name, Value) of
        invalid -> prop_invalid;
        _ -> prop_valid
    end.

%% translate_lone(Name, Negated, InClass, VFlag) -> {ok, iodata()} | error
%% Replacement text for \p{Name} (or \P{Name} when Negated). InClass is
%% whether the escape sits inside a [...] character class (changes how an
%% exact-range expansion is emitted). VFlag enables the properties of
%% strings, which only exist in v mode and only as a non-negated \p outside
%% a class (the parser enforces that for literals; constructor patterns that
%% violate it simply fail to translate and degrade to no-match).
translate_lone(Name, Negated, InClass, VFlag) ->
    case resolve_lone(Name) of
        {gc, Short} ->
            expand(<<"gc:", Short/binary>>, Negated, InClass, true,
                   [esc(Negated), ${, Short, $}]);
        {binary, Key, Pcre, Complement, Supported} ->
            Neg = Negated xor Complement,
            expand(Key, Neg, InClass, Supported, [esc(Neg), ${, Pcre, $}]);
        strings when VFlag, not Negated, not InClass ->
            case arc_regex_uni17_ffi:strings(Name) of
                none -> error;
                Alt -> {ok, Alt}
            end;
        strings -> error;
        invalid -> error
    end.

%% translate_pair(Name, Value, Negated, InClass) -> {ok, iodata()} | error
translate_pair(Name, Value, Negated, InClass) ->
    case resolve_pair(Name, Value) of
        {gc, Short} ->
            expand(<<"gc:", Short/binary>>, Negated, InClass, true,
                   [esc(Negated), ${, Short, $}]);
        {sc, Canon, ScOk} ->
            expand(<<"sc:", Canon/binary>>, Negated, InClass, ScOk,
                   [esc(Negated), "{sc:", Canon, $}]);
        {scx, Canon, ScxOk} ->
            expand(<<"scx:", Canon/binary>>, Negated, InClass, ScxOk,
                   [esc(Negated), "{scx:", Canon, $}]);
        invalid ->
            error
    end.

esc(true) -> "\\P";
esc(false) -> "\\p".

%% ---- Exact data lookups for the v-flag class set desugarer ----
%% (arc_regexp_ffi evaluates v-mode set algebra over explicit codepoint
%% ranges and string lists, so it needs the raw data, not rendered PCRE.)

%% char_set(Payload) -> {ok, [{Lo, Hi}]}
%%                    | {error, unknown_property}    %% not a property at all
%%                    | {error, property_of_strings} %% \p{RGI_Emoji} & friends
%%                    | {error, no_exact_data}       %% real property, no
%%                                                   %% Unicode 17 range table
%% Exact (non-negated) codepoint ranges for a \p{...} payload (lone name or
%% Name=Value). The three causes are distinct because callers act on them
%% differently: only property_of_strings should be retried via string_list/1.
char_set(Payload) ->
    case binary:split(Payload, <<"=">>) of
        [Name, Value] -> pair_char_set(Name, Value);
        [Name] -> lone_char_set(Name)
    end.

lone_char_set(Name) ->
    case resolve_lone(Name) of
        {gc, Short} ->
            ranges_for(<<"gc:", Short/binary>>);
        {binary, Key, _Pcre, true, _Supported} ->
            %% Complement alias (Assigned = \P{Cn}); Key already points at the
            %% underlying GC data. Surrogates are NOT stripped here:
            %% char_set/1 returns the property's true codepoint set (as
            %% builtin_char_set(<<"Any">>) already does), and the PCRE "no lone
            %% surrogates in a UTF pattern" constraint is applied once, at emit
            %% time, by arc_regex_charset:emit_vclass/2.
            case ranges_for(Key) of
                {ok, Ranges} ->
                    {ok, arc_regex_charset:character_complement(Ranges, false)};
                {error, no_exact_data} -> {error, no_exact_data}
            end;
        {binary, Key, Pcre, false, _Supported} ->
            case ranges_for(Key) of
                {ok, Ranges} -> {ok, Ranges};
                {error, no_exact_data} -> builtin_char_set(Pcre)
            end;
        strings ->
            {error, property_of_strings};
        invalid ->
            {error, unknown_property}
    end.

%% Definition-frozen properties (their membership is fixed by the Unicode
%% stability policy) that the generated tables omit because PCRE2's own
%% data already matches — the v-flag desugarer still needs raw ranges.
builtin_char_set(<<"ASCII_Hex_Digit">>) ->
    {ok, [{16#30, 16#39}, {16#41, 16#46}, {16#61, 16#66}]};
builtin_char_set(<<"ASCII">>) ->
    {ok, [{0, 16#7F}]};
builtin_char_set(<<"Any">>) ->
    {ok, [{0, 16#10FFFF}]};
builtin_char_set(_) ->
    {error, no_exact_data}.

pair_char_set(Name, Value) ->
    case resolve_pair(Name, Value) of
        {gc, Short} -> ranges_for(<<"gc:", Short/binary>>);
        {sc, Canon, _ScOk} -> ranges_for(<<"sc:", Canon/binary>>);
        {scx, Canon, _ScxOk} -> ranges_for(<<"scx:", Canon/binary>>);
        invalid -> {error, unknown_property}
    end.

ranges_for(Key) ->
    case arc_unicode_tables:decoded_ranges(Key) of
        none -> {error, no_exact_data};
        Ranges -> {ok, Ranges}
    end.

%% string_list(Name) -> {ok, [[Codepoint]]}
%%                    | {error, unknown_property}
%%                    | {error, no_exact_data}
%% The members of a binary property of strings, straight out of the generated
%% sequence table. Single-codepoint members are 1-element lists.
string_list(Name) ->
    case string_prop(Name) of
        false -> {error, unknown_property};
        true ->
            case arc_unicode_tables:string_members(Name) of
                none -> {error, no_exact_data};
                Members -> {ok, Members}
            end
    end.

%% Expand a property to explicit codepoint ranges when exact Unicode 17 data
%% exists for it, otherwise emit the PCRE2 fallback — but only when PCRE2's own
%% tables actually know the property (`PcreSupported`, probed at generation
%% time). With neither exact ranges nor PCRE2 support there is nothing we can
%% emit, so say `error` instead of handing PCRE2 a \p{...} it will reject.
%%
%% Outside a class the expansion is a (possibly negated) bracket class of its
%% own; inside a class the range items are spliced directly — for a negated
%% escape that means splicing the complement set, since classes cannot nest.
%% The complement excludes the surrogate block: PCRE2 rejects surrogate
%% codepoints in UTF patterns, and valid-UTF-8 subjects cannot contain them.
expand(Key, Negated, InClass, PcreSupported, Fallback) ->
    case arc_unicode_tables:decoded_ranges(Key) of
        none when PcreSupported -> {ok, Fallback};
        none -> error;
        Ranges ->
            Body = fun arc_regex_charset:vrender_ranges/1,
            case {Negated, InClass} of
                {false, false} -> {ok, [$[, Body(Ranges), $]]};
                {true, false} -> {ok, ["[^", Body(Ranges), $]]};
                {false, true} -> {ok, Body(Ranges)};
                {true, true} ->
                    Comp = arc_regex_charset:character_complement(Ranges, false),
                    {ok, Body(arc_regex_charset:vstrip_surrogates(Comp))}
            end
    end.

%% ---- Binary properties of strings (ECMA-262 table-binary-unicode-properties-of-strings).
%% Valid only with the v flag and only in \p (not \P) outside negated classes.
string_prop(<<"Basic_Emoji">>) -> true;
string_prop(<<"Emoji_Keycap_Sequence">>) -> true;
string_prop(<<"RGI_Emoji">>) -> true;
string_prop(<<"RGI_Emoji_Flag_Sequence">>) -> true;
string_prop(<<"RGI_Emoji_Modifier_Sequence">>) -> true;
string_prop(<<"RGI_Emoji_Tag_Sequence">>) -> true;
string_prop(<<"RGI_Emoji_ZWJ_Sequence">>) -> true;
string_prop(_) -> false.
