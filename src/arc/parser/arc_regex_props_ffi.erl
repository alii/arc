%% Unicode property escape tables for JS RegExp \p{...} / \P{...}.
%%
%% GENERATED tables (gc_value/1, script_value/1, binary_prop/1) are derived
%% from the ECMA-262 property tables as encoded in the test262
%% property-escapes corpus (Unicode 17.0.0), with PCRE2 support probed at
%% generation time on OTP 29 (PCRE2 10.47, Unicode 16 tables).
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
%%                     | {binary, PcreName, Flip, PcreSupported}
%%                     | strings
%%                     | invalid
%% A lone name inside \p{...}: a General_Category value, a binary property,
%% or (v-flag only) a binary property of strings. `Flip` marks the complement
%% aliases (Assigned = \P{Cn}), whose exact data lives under the underlying GC
%% key. `PcreSupported` says whether PCRE2's own tables know the property,
%% i.e. whether the \p{...} fallback is usable when we have no exact ranges.
resolve_lone(Name) ->
    case gc_value(Name) of
        invalid ->
            case binary_prop(Name) of
                invalid ->
                    case string_prop(Name) of
                        true -> strings;
                        false -> invalid
                    end;
                {Pcre, Flip, Supported} -> {binary, Pcre, Flip, Supported}
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
    case gc_value(Value) of
        invalid -> invalid;
        Short -> {gc, Short}
    end;
resolve_pair(Name, Value)
  when Name =:= <<"Script">>; Name =:= <<"sc">> ->
    case script_value(Value) of
        invalid -> invalid;
        {Canon, ScOk, _ScxOk} -> {sc, Canon, ScOk}
    end;
resolve_pair(Name, Value)
  when Name =:= <<"Script_Extensions">>; Name =:= <<"scx">> ->
    case script_value(Value) of
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
        {binary, Pcre, Flip, Supported} ->
            Neg = Negated xor Flip,
            Key = case Flip of
                      true -> <<"gc:", Pcre/binary>>;
                      false -> <<"bin:", Pcre/binary>>
                  end,
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
        {binary, Pcre, true, _Supported} ->
            %% Complement alias (Assigned = \P{Cn}); exact data lives under
            %% the underlying GC key.
            case ranges_for(<<"gc:", Pcre/binary>>) of
                {ok, Ranges} -> {ok, strip_surrogates(complement(Ranges))};
                {error, no_exact_data} -> {error, no_exact_data}
            end;
        {binary, Pcre, false, _Supported} ->
            case ranges_for(<<"bin:", Pcre/binary>>) of
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
    case arc_regex_uni17_ffi:decoded_ranges(Key) of
        none -> {error, no_exact_data};
        Ranges -> {ok, Ranges}
    end.

%% string_list(Name) -> {ok, [[Codepoint]]}
%%                    | {error, unknown_property}
%%                    | {error, no_exact_data}
%% The members of a binary property of strings, decoded from the generated
%% alternation data. Single-codepoint members are included as 1-element lists.
string_list(Name) ->
    case string_prop(Name) of
        false -> {error, unknown_property};
        true ->
            case arc_regex_uni17_ffi:strings(Name) of
                none -> {error, no_exact_data};
                Alt -> alt_members(Alt)
            end
    end.

%% Decode "(?:\\x{23}\\x{FE0F}|...)" into {ok, [[16#23, 16#FE0F], ...]}.
%% Some properties (RGI_Emoji) are stored as a compressed regex with nested
%% groups and classes rather than a flat alternation — those are `no_exact_data`
%% and the caller falls back to the non-desugared translation.
alt_members(Bin) ->
    L = unicode:characters_to_list(Bin),
    Inner = lists:sublist(L, 4, length(L) - 4),
    alt_members(Inner, [], []).

alt_members([], Cur, Acc) ->
    {ok, lists:reverse([lists:reverse(Cur) | Acc])};
alt_members([$| | Rest], Cur, Acc) ->
    alt_members(Rest, [], [lists:reverse(Cur) | Acc]);
alt_members([$\\, $x, ${ | Rest], Cur, Acc) ->
    case lists:splitwith(fun is_hex_digit/1, Rest) of
        {HexDigits, [$} | Rest2]} when HexDigits =/= [] ->
            alt_members(Rest2, [list_to_integer(HexDigits, 16) | Cur], Acc);
        {_HexDigits, _NoClosingBrace} -> {error, no_exact_data}
    end;
alt_members(_Unexpected, _Cur, _Acc) ->
    {error, no_exact_data}.

is_hex_digit(C) ->
    (C >= $0 andalso C =< $9)
        orelse (C >= $a andalso C =< $f)
        orelse (C >= $A andalso C =< $F).

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
    case arc_regex_uni17_ffi:decoded_ranges(Key) of
        none when PcreSupported -> {ok, Fallback};
        none -> error;
        Ranges ->
            case {Negated, InClass} of
                {false, false} -> {ok, [$[, render_ranges(Ranges), $]]};
                {true, false} -> {ok, ["[^", render_ranges(Ranges), $]]};
                {false, true} -> {ok, render_ranges(Ranges)};
                {true, true} ->
                    {ok, render_ranges(strip_surrogates(complement(Ranges)))}
            end
    end.

render_ranges([]) -> [];
render_ranges([{Lo, Lo} | Rest]) ->
    ["\\x{", integer_to_list(Lo, 16), $} | render_ranges(Rest)];
render_ranges([{Lo, Hi} | Rest]) ->
    ["\\x{", integer_to_list(Lo, 16), "}-\\x{", integer_to_list(Hi, 16), $}
     | render_ranges(Rest)].

%% Complement of a sorted, disjoint range list over 0..10FFFF.
complement(Ranges) -> complement(Ranges, 0).

complement([], Next) when Next =< 16#10FFFF -> [{Next, 16#10FFFF}];
complement([], _Next) -> [];
complement([{Lo, Hi} | Rest], Next) when Lo > Next ->
    [{Next, Lo - 1} | complement(Rest, Hi + 1)];
complement([{_Lo, Hi} | Rest], Next) ->
    complement(Rest, max(Next, Hi + 1)).

strip_surrogates([]) -> [];
strip_surrogates([{Lo, Hi} | Rest]) when Hi < 16#D800; Lo > 16#DFFF ->
    [{Lo, Hi} | strip_surrogates(Rest)];
strip_surrogates([{Lo, Hi} | Rest]) ->
    Left = case Lo < 16#D800 of
               true -> [{Lo, 16#D7FF}];
               false -> []
           end,
    Right = case Hi > 16#DFFF of
                true -> [{16#E000, Hi}];
                false -> []
            end,
    Left ++ Right ++ strip_surrogates(Rest).

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

%% ---- Generated tables ----

%% gc_value(Alias) -> ShortCode | invalid
gc_value(<<"Final_Punctuation">>) -> <<"Pf">>;
gc_value(<<"Pf">>) -> <<"Pf">>;
gc_value(<<"Connector_Punctuation">>) -> <<"Pc">>;
gc_value(<<"Pc">>) -> <<"Pc">>;
gc_value(<<"Cased_Letter">>) -> <<"LC">>;
gc_value(<<"LC">>) -> <<"LC">>;
gc_value(<<"Close_Punctuation">>) -> <<"Pe">>;
gc_value(<<"Pe">>) -> <<"Pe">>;
gc_value(<<"Cn">>) -> <<"Cn">>;
gc_value(<<"Unassigned">>) -> <<"Cn">>;
gc_value(<<"Open_Punctuation">>) -> <<"Ps">>;
gc_value(<<"Ps">>) -> <<"Ps">>;
gc_value(<<"Lu">>) -> <<"Lu">>;
gc_value(<<"Uppercase_Letter">>) -> <<"Lu">>;
gc_value(<<"S">>) -> <<"S">>;
gc_value(<<"Symbol">>) -> <<"S">>;
gc_value(<<"Ll">>) -> <<"Ll">>;
gc_value(<<"Lowercase_Letter">>) -> <<"Ll">>;
gc_value(<<"Cf">>) -> <<"Cf">>;
gc_value(<<"Format">>) -> <<"Cf">>;
gc_value(<<"Other_Symbol">>) -> <<"So">>;
gc_value(<<"So">>) -> <<"So">>;
gc_value(<<"Combining_Mark">>) -> <<"M">>;
gc_value(<<"M">>) -> <<"M">>;
gc_value(<<"Mark">>) -> <<"M">>;
gc_value(<<"Initial_Punctuation">>) -> <<"Pi">>;
gc_value(<<"Pi">>) -> <<"Pi">>;
gc_value(<<"Cc">>) -> <<"Cc">>;
gc_value(<<"Control">>) -> <<"Cc">>;
gc_value(<<"cntrl">>) -> <<"Cc">>;
gc_value(<<"Cs">>) -> <<"Cs">>;
gc_value(<<"Surrogate">>) -> <<"Cs">>;
gc_value(<<"Lm">>) -> <<"Lm">>;
gc_value(<<"Modifier_Letter">>) -> <<"Lm">>;
gc_value(<<"No">>) -> <<"No">>;
gc_value(<<"Other_Number">>) -> <<"No">>;
gc_value(<<"Math_Symbol">>) -> <<"Sm">>;
gc_value(<<"Sm">>) -> <<"Sm">>;
gc_value(<<"N">>) -> <<"N">>;
gc_value(<<"Number">>) -> <<"N">>;
gc_value(<<"Dash_Punctuation">>) -> <<"Pd">>;
gc_value(<<"Pd">>) -> <<"Pd">>;
gc_value(<<"Other_Punctuation">>) -> <<"Po">>;
gc_value(<<"Po">>) -> <<"Po">>;
gc_value(<<"Paragraph_Separator">>) -> <<"Zp">>;
gc_value(<<"Zp">>) -> <<"Zp">>;
gc_value(<<"Lo">>) -> <<"Lo">>;
gc_value(<<"Other_Letter">>) -> <<"Lo">>;
gc_value(<<"L">>) -> <<"L">>;
gc_value(<<"Letter">>) -> <<"L">>;
gc_value(<<"Mc">>) -> <<"Mc">>;
gc_value(<<"Spacing_Mark">>) -> <<"Mc">>;
gc_value(<<"C">>) -> <<"C">>;
gc_value(<<"Other">>) -> <<"C">>;
gc_value(<<"Line_Separator">>) -> <<"Zl">>;
gc_value(<<"Zl">>) -> <<"Zl">>;
gc_value(<<"P">>) -> <<"P">>;
gc_value(<<"Punctuation">>) -> <<"P">>;
gc_value(<<"punct">>) -> <<"P">>;
gc_value(<<"Modifier_Symbol">>) -> <<"Sk">>;
gc_value(<<"Sk">>) -> <<"Sk">>;
gc_value(<<"Co">>) -> <<"Co">>;
gc_value(<<"Private_Use">>) -> <<"Co">>;
gc_value(<<"Decimal_Number">>) -> <<"Nd">>;
gc_value(<<"Nd">>) -> <<"Nd">>;
gc_value(<<"digit">>) -> <<"Nd">>;
gc_value(<<"Lt">>) -> <<"Lt">>;
gc_value(<<"Titlecase_Letter">>) -> <<"Lt">>;
gc_value(<<"Space_Separator">>) -> <<"Zs">>;
gc_value(<<"Zs">>) -> <<"Zs">>;
gc_value(<<"Enclosing_Mark">>) -> <<"Me">>;
gc_value(<<"Me">>) -> <<"Me">>;
gc_value(<<"Mn">>) -> <<"Mn">>;
gc_value(<<"Nonspacing_Mark">>) -> <<"Mn">>;
gc_value(<<"Separator">>) -> <<"Z">>;
gc_value(<<"Z">>) -> <<"Z">>;
gc_value(<<"Letter_Number">>) -> <<"Nl">>;
gc_value(<<"Nl">>) -> <<"Nl">>;
gc_value(<<"Currency_Symbol">>) -> <<"Sc">>;
gc_value(<<"Sc">>) -> <<"Sc">>;
gc_value(_) -> invalid.

script_value(<<"Tibetan">>) -> {<<"Tibetan">>, true, true};
script_value(<<"Tibt">>) -> {<<"Tibetan">>, true, true};
script_value(<<"Cakm">>) -> {<<"Chakma">>, true, true};
script_value(<<"Chakma">>) -> {<<"Chakma">>, true, true};
script_value(<<"Avestan">>) -> {<<"Avestan">>, true, true};
script_value(<<"Avst">>) -> {<<"Avestan">>, true, true};
script_value(<<"Arab">>) -> {<<"Arabic">>, true, true};
script_value(<<"Arabic">>) -> {<<"Arabic">>, true, true};
script_value(<<"Lyci">>) -> {<<"Lycian">>, true, true};
script_value(<<"Lycian">>) -> {<<"Lycian">>, true, true};
script_value(<<"Khitan_Small_Script">>) -> {<<"Khitan_Small_Script">>, true, true};
script_value(<<"Kits">>) -> {<<"Khitan_Small_Script">>, true, true};
script_value(<<"Wara">>) -> {<<"Warang_Citi">>, true, true};
script_value(<<"Warang_Citi">>) -> {<<"Warang_Citi">>, true, true};
script_value(<<"Old_Sogdian">>) -> {<<"Old_Sogdian">>, true, true};
script_value(<<"Sogo">>) -> {<<"Old_Sogdian">>, true, true};
script_value(<<"Bass">>) -> {<<"Bassa_Vah">>, true, true};
script_value(<<"Bassa_Vah">>) -> {<<"Bassa_Vah">>, true, true};
script_value(<<"Gonm">>) -> {<<"Masaram_Gondi">>, true, true};
script_value(<<"Masaram_Gondi">>) -> {<<"Masaram_Gondi">>, true, true};
script_value(<<"Lana">>) -> {<<"Tai_Tham">>, true, true};
script_value(<<"Tai_Tham">>) -> {<<"Tai_Tham">>, true, true};
script_value(<<"Mend">>) -> {<<"Mende_Kikakui">>, true, true};
script_value(<<"Mende_Kikakui">>) -> {<<"Mende_Kikakui">>, true, true};
script_value(<<"Common">>) -> {<<"Common">>, true, true};
script_value(<<"Zyyy">>) -> {<<"Common">>, true, true};
script_value(<<"Sunu">>) -> {<<"Sunuwar">>, true, true};
script_value(<<"Sunuwar">>) -> {<<"Sunuwar">>, true, true};
script_value(<<"Hano">>) -> {<<"Hanunoo">>, true, true};
script_value(<<"Hanunoo">>) -> {<<"Hanunoo">>, true, true};
script_value(<<"Elym">>) -> {<<"Elymaic">>, true, true};
script_value(<<"Elymaic">>) -> {<<"Elymaic">>, true, true};
script_value(<<"Yi">>) -> {<<"Yi">>, true, true};
script_value(<<"Yiii">>) -> {<<"Yi">>, true, true};
script_value(<<"Kali">>) -> {<<"Kayah_Li">>, true, true};
script_value(<<"Kayah_Li">>) -> {<<"Kayah_Li">>, true, true};
script_value(<<"Kawi">>) -> {<<"Kawi">>, true, true};
script_value(<<"Diak">>) -> {<<"Dives_Akuru">>, true, true};
script_value(<<"Dives_Akuru">>) -> {<<"Dives_Akuru">>, true, true};
script_value(<<"Egyp">>) -> {<<"Egyptian_Hieroglyphs">>, true, true};
script_value(<<"Egyptian_Hieroglyphs">>) -> {<<"Egyptian_Hieroglyphs">>, true, true};
script_value(<<"Runic">>) -> {<<"Runic">>, true, true};
script_value(<<"Runr">>) -> {<<"Runic">>, true, true};
script_value(<<"Old_Permic">>) -> {<<"Old_Permic">>, true, true};
script_value(<<"Perm">>) -> {<<"Old_Permic">>, true, true};
script_value(<<"Khudawadi">>) -> {<<"Khudawadi">>, true, true};
script_value(<<"Sind">>) -> {<<"Khudawadi">>, true, true};
script_value(<<"Toto">>) -> {<<"Toto">>, true, true};
script_value(<<"New_Tai_Lue">>) -> {<<"New_Tai_Lue">>, true, true};
script_value(<<"Talu">>) -> {<<"New_Tai_Lue">>, true, true};
script_value(<<"Zanabazar_Square">>) -> {<<"Zanabazar_Square">>, true, true};
script_value(<<"Zanb">>) -> {<<"Zanabazar_Square">>, true, true};
script_value(<<"Syrc">>) -> {<<"Syriac">>, true, true};
script_value(<<"Syriac">>) -> {<<"Syriac">>, true, true};
script_value(<<"Tai_Viet">>) -> {<<"Tai_Viet">>, true, true};
script_value(<<"Tavt">>) -> {<<"Tai_Viet">>, true, true};
script_value(<<"Hebr">>) -> {<<"Hebrew">>, true, true};
script_value(<<"Hebrew">>) -> {<<"Hebrew">>, true, true};
script_value(<<"Thaa">>) -> {<<"Thaana">>, true, true};
script_value(<<"Thaana">>) -> {<<"Thaana">>, true, true};
script_value(<<"Osage">>) -> {<<"Osage">>, true, true};
script_value(<<"Osge">>) -> {<<"Osage">>, true, true};
script_value(<<"Berf">>) -> {<<"Beria_Erfe">>, false, false};
script_value(<<"Beria_Erfe">>) -> {<<"Beria_Erfe">>, false, false};
script_value(<<"Sidd">>) -> {<<"Siddham">>, true, true};
script_value(<<"Siddham">>) -> {<<"Siddham">>, true, true};
script_value(<<"Hung">>) -> {<<"Old_Hungarian">>, true, true};
script_value(<<"Old_Hungarian">>) -> {<<"Old_Hungarian">>, true, true};
script_value(<<"Narb">>) -> {<<"Old_North_Arabian">>, true, true};
script_value(<<"Old_North_Arabian">>) -> {<<"Old_North_Arabian">>, true, true};
script_value(<<"Tagb">>) -> {<<"Tagbanwa">>, true, true};
script_value(<<"Tagbanwa">>) -> {<<"Tagbanwa">>, true, true};
script_value(<<"Deva">>) -> {<<"Devanagari">>, true, true};
script_value(<<"Devanagari">>) -> {<<"Devanagari">>, true, true};
script_value(<<"Khmer">>) -> {<<"Khmer">>, true, true};
script_value(<<"Khmr">>) -> {<<"Khmer">>, true, true};
script_value(<<"Oriya">>) -> {<<"Oriya">>, true, true};
script_value(<<"Orya">>) -> {<<"Oriya">>, true, true};
script_value(<<"Mero">>) -> {<<"Meroitic_Hieroglyphs">>, true, true};
script_value(<<"Meroitic_Hieroglyphs">>) -> {<<"Meroitic_Hieroglyphs">>, true, true};
script_value(<<"Unknown">>) -> {<<"Unknown">>, true, true};
script_value(<<"Zzzz">>) -> {<<"Unknown">>, true, true};
script_value(<<"Medefaidrin">>) -> {<<"Medefaidrin">>, true, true};
script_value(<<"Medf">>) -> {<<"Medefaidrin">>, true, true};
script_value(<<"Lepc">>) -> {<<"Lepcha">>, true, true};
script_value(<<"Lepcha">>) -> {<<"Lepcha">>, true, true};
script_value(<<"Ethi">>) -> {<<"Ethiopic">>, true, true};
script_value(<<"Ethiopic">>) -> {<<"Ethiopic">>, true, true};
script_value(<<"Sidetic">>) -> {<<"Sidetic">>, false, false};
script_value(<<"Sidt">>) -> {<<"Sidetic">>, false, false};
script_value(<<"Malayalam">>) -> {<<"Malayalam">>, true, true};
script_value(<<"Mlym">>) -> {<<"Malayalam">>, true, true};
script_value(<<"Batak">>) -> {<<"Batak">>, true, true};
script_value(<<"Batk">>) -> {<<"Batak">>, true, true};
script_value(<<"Gran">>) -> {<<"Grantha">>, true, true};
script_value(<<"Grantha">>) -> {<<"Grantha">>, true, true};
script_value(<<"Palm">>) -> {<<"Palmyrene">>, true, true};
script_value(<<"Palmyrene">>) -> {<<"Palmyrene">>, true, true};
script_value(<<"Anatolian_Hieroglyphs">>) -> {<<"Anatolian_Hieroglyphs">>, true, true};
script_value(<<"Hluw">>) -> {<<"Anatolian_Hieroglyphs">>, true, true};
script_value(<<"Gukh">>) -> {<<"Gurung_Khema">>, true, true};
script_value(<<"Gurung_Khema">>) -> {<<"Gurung_Khema">>, true, true};
script_value(<<"Armenian">>) -> {<<"Armenian">>, true, true};
script_value(<<"Armn">>) -> {<<"Armenian">>, true, true};
script_value(<<"Khoj">>) -> {<<"Khojki">>, true, true};
script_value(<<"Khojki">>) -> {<<"Khojki">>, true, true};
script_value(<<"Rejang">>) -> {<<"Rejang">>, true, true};
script_value(<<"Rjng">>) -> {<<"Rejang">>, true, true};
script_value(<<"Hmnp">>) -> {<<"Nyiakeng_Puachue_Hmong">>, true, true};
script_value(<<"Nyiakeng_Puachue_Hmong">>) -> {<<"Nyiakeng_Puachue_Hmong">>, true, true};
script_value(<<"Saur">>) -> {<<"Saurashtra">>, true, true};
script_value(<<"Saurashtra">>) -> {<<"Saurashtra">>, true, true};
script_value(<<"Lao">>) -> {<<"Lao">>, true, true};
script_value(<<"Laoo">>) -> {<<"Lao">>, true, true};
script_value(<<"Mro">>) -> {<<"Mro">>, true, true};
script_value(<<"Mroo">>) -> {<<"Mro">>, true, true};
script_value(<<"Nand">>) -> {<<"Nandinagari">>, true, true};
script_value(<<"Nandinagari">>) -> {<<"Nandinagari">>, true, true};
script_value(<<"Elba">>) -> {<<"Elbasan">>, true, true};
script_value(<<"Elbasan">>) -> {<<"Elbasan">>, true, true};
script_value(<<"Nko">>) -> {<<"Nko">>, true, true};
script_value(<<"Nkoo">>) -> {<<"Nko">>, true, true};
script_value(<<"Mong">>) -> {<<"Mongolian">>, true, true};
script_value(<<"Mongolian">>) -> {<<"Mongolian">>, true, true};
script_value(<<"Cuneiform">>) -> {<<"Cuneiform">>, true, true};
script_value(<<"Xsux">>) -> {<<"Cuneiform">>, true, true};
script_value(<<"Cyrillic">>) -> {<<"Cyrillic">>, true, true};
script_value(<<"Cyrl">>) -> {<<"Cyrillic">>, true, true};
script_value(<<"Mand">>) -> {<<"Mandaic">>, true, true};
script_value(<<"Mandaic">>) -> {<<"Mandaic">>, true, true};
script_value(<<"Tamil">>) -> {<<"Tamil">>, true, true};
script_value(<<"Taml">>) -> {<<"Tamil">>, true, true};
script_value(<<"Sharada">>) -> {<<"Sharada">>, true, true};
script_value(<<"Shrd">>) -> {<<"Sharada">>, true, true};
script_value(<<"Hmng">>) -> {<<"Pahawh_Hmong">>, true, true};
script_value(<<"Pahawh_Hmong">>) -> {<<"Pahawh_Hmong">>, true, true};
script_value(<<"Sinh">>) -> {<<"Sinhala">>, true, true};
script_value(<<"Sinhala">>) -> {<<"Sinhala">>, true, true};
script_value(<<"Cpmn">>) -> {<<"Cypro_Minoan">>, true, true};
script_value(<<"Cypro_Minoan">>) -> {<<"Cypro_Minoan">>, true, true};
script_value(<<"Vith">>) -> {<<"Vithkuqi">>, true, true};
script_value(<<"Vithkuqi">>) -> {<<"Vithkuqi">>, true, true};
script_value(<<"Mani">>) -> {<<"Manichaean">>, true, true};
script_value(<<"Manichaean">>) -> {<<"Manichaean">>, true, true};
script_value(<<"Deseret">>) -> {<<"Deseret">>, true, true};
script_value(<<"Dsrt">>) -> {<<"Deseret">>, true, true};
script_value(<<"Vai">>) -> {<<"Vai">>, true, true};
script_value(<<"Vaii">>) -> {<<"Vai">>, true, true};
script_value(<<"Nag_Mundari">>) -> {<<"Nag_Mundari">>, true, true};
script_value(<<"Nagm">>) -> {<<"Nag_Mundari">>, true, true};
script_value(<<"Adlam">>) -> {<<"Adlam">>, true, true};
script_value(<<"Adlm">>) -> {<<"Adlam">>, true, true};
script_value(<<"Sylo">>) -> {<<"Syloti_Nagri">>, true, true};
script_value(<<"Syloti_Nagri">>) -> {<<"Syloti_Nagri">>, true, true};
script_value(<<"Todhri">>) -> {<<"Todhri">>, true, true};
script_value(<<"Todr">>) -> {<<"Todhri">>, true, true};
script_value(<<"Mahajani">>) -> {<<"Mahajani">>, true, true};
script_value(<<"Mahj">>) -> {<<"Mahajani">>, true, true};
script_value(<<"Beng">>) -> {<<"Bengali">>, true, true};
script_value(<<"Bengali">>) -> {<<"Bengali">>, true, true};
script_value(<<"Tang">>) -> {<<"Tangut">>, true, true};
script_value(<<"Tangut">>) -> {<<"Tangut">>, true, true};
script_value(<<"Cari">>) -> {<<"Carian">>, true, true};
script_value(<<"Carian">>) -> {<<"Carian">>, true, true};
script_value(<<"Brah">>) -> {<<"Brahmi">>, true, true};
script_value(<<"Brahmi">>) -> {<<"Brahmi">>, true, true};
script_value(<<"Phnx">>) -> {<<"Phoenician">>, true, true};
script_value(<<"Phoenician">>) -> {<<"Phoenician">>, true, true};
script_value(<<"Sgnw">>) -> {<<"SignWriting">>, true, true};
script_value(<<"SignWriting">>) -> {<<"SignWriting">>, true, true};
script_value(<<"Limb">>) -> {<<"Limbu">>, true, true};
script_value(<<"Limbu">>) -> {<<"Limbu">>, true, true};
script_value(<<"Hang">>) -> {<<"Hangul">>, true, true};
script_value(<<"Hangul">>) -> {<<"Hangul">>, true, true};
script_value(<<"Inscriptional_Parthian">>) -> {<<"Inscriptional_Parthian">>, true, true};
script_value(<<"Prti">>) -> {<<"Inscriptional_Parthian">>, true, true};
script_value(<<"Lydi">>) -> {<<"Lydian">>, true, true};
script_value(<<"Lydian">>) -> {<<"Lydian">>, true, true};
script_value(<<"Ol_Onal">>) -> {<<"Ol_Onal">>, true, true};
script_value(<<"Onao">>) -> {<<"Ol_Onal">>, true, true};
script_value(<<"Bopo">>) -> {<<"Bopomofo">>, true, true};
script_value(<<"Bopomofo">>) -> {<<"Bopomofo">>, true, true};
script_value(<<"Cham">>) -> {<<"Cham">>, true, true};
script_value(<<"Miao">>) -> {<<"Miao">>, true, true};
script_value(<<"Plrd">>) -> {<<"Miao">>, true, true};
script_value(<<"Newa">>) -> {<<"Newa">>, true, true};
script_value(<<"Cher">>) -> {<<"Cherokee">>, true, true};
script_value(<<"Cherokee">>) -> {<<"Cherokee">>, true, true};
script_value(<<"Cprt">>) -> {<<"Cypriot">>, true, true};
script_value(<<"Cypriot">>) -> {<<"Cypriot">>, true, true};
script_value(<<"Lina">>) -> {<<"Linear_A">>, true, true};
script_value(<<"Linear_A">>) -> {<<"Linear_A">>, true, true};
script_value(<<"Telu">>) -> {<<"Telugu">>, true, true};
script_value(<<"Telugu">>) -> {<<"Telugu">>, true, true};
script_value(<<"Ogam">>) -> {<<"Ogham">>, true, true};
script_value(<<"Ogham">>) -> {<<"Ogham">>, true, true};
script_value(<<"Tagalog">>) -> {<<"Tagalog">>, true, true};
script_value(<<"Tglg">>) -> {<<"Tagalog">>, true, true};
script_value(<<"Merc">>) -> {<<"Meroitic_Cursive">>, true, true};
script_value(<<"Meroitic_Cursive">>) -> {<<"Meroitic_Cursive">>, true, true};
script_value(<<"Phlp">>) -> {<<"Psalter_Pahlavi">>, true, true};
script_value(<<"Psalter_Pahlavi">>) -> {<<"Psalter_Pahlavi">>, true, true};
script_value(<<"Myanmar">>) -> {<<"Myanmar">>, true, true};
script_value(<<"Mymr">>) -> {<<"Myanmar">>, true, true};
script_value(<<"Ahom">>) -> {<<"Ahom">>, true, true};
script_value(<<"Samaritan">>) -> {<<"Samaritan">>, true, true};
script_value(<<"Samr">>) -> {<<"Samaritan">>, true, true};
script_value(<<"Tirh">>) -> {<<"Tirhuta">>, true, true};
script_value(<<"Tirhuta">>) -> {<<"Tirhuta">>, true, true};
script_value(<<"Bamu">>) -> {<<"Bamum">>, true, true};
script_value(<<"Bamum">>) -> {<<"Bamum">>, true, true};
script_value(<<"Soyo">>) -> {<<"Soyombo">>, true, true};
script_value(<<"Soyombo">>) -> {<<"Soyombo">>, true, true};
script_value(<<"Old_Uyghur">>) -> {<<"Old_Uyghur">>, true, true};
script_value(<<"Ougr">>) -> {<<"Old_Uyghur">>, true, true};
script_value(<<"Java">>) -> {<<"Javanese">>, true, true};
script_value(<<"Javanese">>) -> {<<"Javanese">>, true, true};
script_value(<<"Tai_Le">>) -> {<<"Tai_Le">>, true, true};
script_value(<<"Tale">>) -> {<<"Tai_Le">>, true, true};
script_value(<<"Inscriptional_Pahlavi">>) -> {<<"Inscriptional_Pahlavi">>, true, true};
script_value(<<"Phli">>) -> {<<"Inscriptional_Pahlavi">>, true, true};
script_value(<<"Old_Persian">>) -> {<<"Old_Persian">>, true, true};
script_value(<<"Xpeo">>) -> {<<"Old_Persian">>, true, true};
script_value(<<"Ol_Chiki">>) -> {<<"Ol_Chiki">>, true, true};
script_value(<<"Olck">>) -> {<<"Ol_Chiki">>, true, true};
script_value(<<"Nshu">>) -> {<<"Nushu">>, true, true};
script_value(<<"Nushu">>) -> {<<"Nushu">>, true, true};
script_value(<<"Latin">>) -> {<<"Latin">>, true, true};
script_value(<<"Latn">>) -> {<<"Latin">>, true, true};
script_value(<<"Wancho">>) -> {<<"Wancho">>, true, true};
script_value(<<"Wcho">>) -> {<<"Wancho">>, true, true};
script_value(<<"Thai">>) -> {<<"Thai">>, true, true};
script_value(<<"Linb">>) -> {<<"Linear_B">>, true, true};
script_value(<<"Linear_B">>) -> {<<"Linear_B">>, true, true};
script_value(<<"Khar">>) -> {<<"Kharoshthi">>, true, true};
script_value(<<"Kharoshthi">>) -> {<<"Kharoshthi">>, true, true};
script_value(<<"Kana">>) -> {<<"Katakana">>, true, true};
script_value(<<"Katakana">>) -> {<<"Katakana">>, true, true};
script_value(<<"Nabataean">>) -> {<<"Nabataean">>, true, true};
script_value(<<"Nbat">>) -> {<<"Nabataean">>, true, true};
script_value(<<"Armi">>) -> {<<"Imperial_Aramaic">>, true, true};
script_value(<<"Imperial_Aramaic">>) -> {<<"Imperial_Aramaic">>, true, true};
script_value(<<"Phag">>) -> {<<"Phags_Pa">>, true, true};
script_value(<<"Phags_Pa">>) -> {<<"Phags_Pa">>, true, true};
script_value(<<"Hira">>) -> {<<"Hiragana">>, true, true};
script_value(<<"Hiragana">>) -> {<<"Hiragana">>, true, true};
script_value(<<"Gara">>) -> {<<"Garay">>, true, true};
script_value(<<"Garay">>) -> {<<"Garay">>, true, true};
script_value(<<"Geor">>) -> {<<"Georgian">>, true, true};
script_value(<<"Georgian">>) -> {<<"Georgian">>, true, true};
script_value(<<"Bali">>) -> {<<"Balinese">>, true, true};
script_value(<<"Balinese">>) -> {<<"Balinese">>, true, true};
script_value(<<"Osma">>) -> {<<"Osmanya">>, true, true};
script_value(<<"Osmanya">>) -> {<<"Osmanya">>, true, true};
script_value(<<"Sora">>) -> {<<"Sora_Sompeng">>, true, true};
script_value(<<"Sora_Sompeng">>) -> {<<"Sora_Sompeng">>, true, true};
script_value(<<"Tfng">>) -> {<<"Tifinagh">>, true, true};
script_value(<<"Tifinagh">>) -> {<<"Tifinagh">>, true, true};
script_value(<<"Copt">>) -> {<<"Coptic">>, true, true};
script_value(<<"Coptic">>) -> {<<"Coptic">>, true, true};
script_value(<<"Qaac">>) -> {<<"Coptic">>, true, true};
script_value(<<"Yezi">>) -> {<<"Yezidi">>, true, true};
script_value(<<"Yezidi">>) -> {<<"Yezidi">>, true, true};
script_value(<<"Tolong_Siki">>) -> {<<"Tolong_Siki">>, false, false};
script_value(<<"Tols">>) -> {<<"Tolong_Siki">>, false, false};
script_value(<<"Shavian">>) -> {<<"Shavian">>, true, true};
script_value(<<"Shaw">>) -> {<<"Shavian">>, true, true};
script_value(<<"Canadian_Aboriginal">>) -> {<<"Canadian_Aboriginal">>, true, true};
script_value(<<"Cans">>) -> {<<"Canadian_Aboriginal">>, true, true};
script_value(<<"Mult">>) -> {<<"Multani">>, true, true};
script_value(<<"Multani">>) -> {<<"Multani">>, true, true};
script_value(<<"Chorasmian">>) -> {<<"Chorasmian">>, true, true};
script_value(<<"Chrs">>) -> {<<"Chorasmian">>, true, true};
script_value(<<"Pau_Cin_Hau">>) -> {<<"Pau_Cin_Hau">>, true, true};
script_value(<<"Pauc">>) -> {<<"Pau_Cin_Hau">>, true, true};
script_value(<<"Meetei_Mayek">>) -> {<<"Meetei_Mayek">>, true, true};
script_value(<<"Mtei">>) -> {<<"Meetei_Mayek">>, true, true};
script_value(<<"Marc">>) -> {<<"Marchen">>, true, true};
script_value(<<"Marchen">>) -> {<<"Marchen">>, true, true};
script_value(<<"Aghb">>) -> {<<"Caucasian_Albanian">>, true, true};
script_value(<<"Caucasian_Albanian">>) -> {<<"Caucasian_Albanian">>, true, true};
script_value(<<"Lisu">>) -> {<<"Lisu">>, true, true};
script_value(<<"Maka">>) -> {<<"Makasar">>, true, true};
script_value(<<"Makasar">>) -> {<<"Makasar">>, true, true};
script_value(<<"Tai_Yo">>) -> {<<"Tai_Yo">>, false, false};
script_value(<<"Tayo">>) -> {<<"Tai_Yo">>, false, false};
script_value(<<"Kannada">>) -> {<<"Kannada">>, true, true};
script_value(<<"Knda">>) -> {<<"Kannada">>, true, true};
script_value(<<"Ital">>) -> {<<"Old_Italic">>, true, true};
script_value(<<"Old_Italic">>) -> {<<"Old_Italic">>, true, true};
script_value(<<"Glag">>) -> {<<"Glagolitic">>, true, true};
script_value(<<"Glagolitic">>) -> {<<"Glagolitic">>, true, true};
script_value(<<"Gurmukhi">>) -> {<<"Gurmukhi">>, true, true};
script_value(<<"Guru">>) -> {<<"Gurmukhi">>, true, true};
script_value(<<"Gong">>) -> {<<"Gunjala_Gondi">>, true, true};
script_value(<<"Gunjala_Gondi">>) -> {<<"Gunjala_Gondi">>, true, true};
script_value(<<"Dogr">>) -> {<<"Dogra">>, true, true};
script_value(<<"Dogra">>) -> {<<"Dogra">>, true, true};
script_value(<<"Takr">>) -> {<<"Takri">>, true, true};
script_value(<<"Takri">>) -> {<<"Takri">>, true, true};
script_value(<<"Greek">>) -> {<<"Greek">>, true, true};
script_value(<<"Grek">>) -> {<<"Greek">>, true, true};
script_value(<<"Modi">>) -> {<<"Modi">>, true, true};
script_value(<<"Old_Turkic">>) -> {<<"Old_Turkic">>, true, true};
script_value(<<"Orkh">>) -> {<<"Old_Turkic">>, true, true};
script_value(<<"Kirat_Rai">>) -> {<<"Kirat_Rai">>, true, true};
script_value(<<"Krai">>) -> {<<"Kirat_Rai">>, true, true};
script_value(<<"Dupl">>) -> {<<"Duployan">>, true, true};
script_value(<<"Duployan">>) -> {<<"Duployan">>, true, true};
script_value(<<"Han">>) -> {<<"Han">>, true, true};
script_value(<<"Hani">>) -> {<<"Han">>, true, true};
script_value(<<"Sund">>) -> {<<"Sundanese">>, true, true};
script_value(<<"Sundanese">>) -> {<<"Sundanese">>, true, true};
script_value(<<"Buhd">>) -> {<<"Buhid">>, true, true};
script_value(<<"Buhid">>) -> {<<"Buhid">>, true, true};
script_value(<<"Gujarati">>) -> {<<"Gujarati">>, true, true};
script_value(<<"Gujr">>) -> {<<"Gujarati">>, true, true};
script_value(<<"Tulu_Tigalari">>) -> {<<"Tulu_Tigalari">>, true, true};
script_value(<<"Tutg">>) -> {<<"Tulu_Tigalari">>, true, true};
script_value(<<"Bhaiksuki">>) -> {<<"Bhaiksuki">>, true, true};
script_value(<<"Bhks">>) -> {<<"Bhaiksuki">>, true, true};
script_value(<<"Hatr">>) -> {<<"Hatran">>, true, true};
script_value(<<"Hatran">>) -> {<<"Hatran">>, true, true};
script_value(<<"Brai">>) -> {<<"Braille">>, true, true};
script_value(<<"Braille">>) -> {<<"Braille">>, true, true};
script_value(<<"Ugar">>) -> {<<"Ugaritic">>, true, true};
script_value(<<"Ugaritic">>) -> {<<"Ugaritic">>, true, true};
script_value(<<"Inherited">>) -> {<<"Inherited">>, true, true};
script_value(<<"Qaai">>) -> {<<"Inherited">>, true, true};
script_value(<<"Zinh">>) -> {<<"Inherited">>, true, true};
script_value(<<"Sogd">>) -> {<<"Sogdian">>, true, true};
script_value(<<"Sogdian">>) -> {<<"Sogdian">>, true, true};
script_value(<<"Old_South_Arabian">>) -> {<<"Old_South_Arabian">>, true, true};
script_value(<<"Sarb">>) -> {<<"Old_South_Arabian">>, true, true};
script_value(<<"Hanifi_Rohingya">>) -> {<<"Hanifi_Rohingya">>, true, true};
script_value(<<"Rohg">>) -> {<<"Hanifi_Rohingya">>, true, true};
script_value(<<"Bugi">>) -> {<<"Buginese">>, true, true};
script_value(<<"Buginese">>) -> {<<"Buginese">>, true, true};
script_value(<<"Tangsa">>) -> {<<"Tangsa">>, true, true};
script_value(<<"Tnsa">>) -> {<<"Tangsa">>, true, true};
script_value(<<"Goth">>) -> {<<"Gothic">>, true, true};
script_value(<<"Gothic">>) -> {<<"Gothic">>, true, true};
script_value(<<"Kaithi">>) -> {<<"Kaithi">>, true, true};
script_value(<<"Kthi">>) -> {<<"Kaithi">>, true, true};
script_value(_) -> invalid.

binary_prop(<<"EMod">>) -> {<<"Emoji_Modifier">>, false, true};
binary_prop(<<"Emoji_Modifier">>) -> {<<"Emoji_Modifier">>, false, true};
binary_prop(<<"CI">>) -> {<<"Case_Ignorable">>, false, true};
binary_prop(<<"Case_Ignorable">>) -> {<<"Case_Ignorable">>, false, true};
binary_prop(<<"VS">>) -> {<<"Variation_Selector">>, false, true};
binary_prop(<<"Variation_Selector">>) -> {<<"Variation_Selector">>, false, true};
binary_prop(<<"CWT">>) -> {<<"Changes_When_Titlecased">>, false, true};
binary_prop(<<"Changes_When_Titlecased">>) -> {<<"Changes_When_Titlecased">>, false, true};
binary_prop(<<"IDC">>) -> {<<"ID_Continue">>, false, true};
binary_prop(<<"ID_Continue">>) -> {<<"ID_Continue">>, false, true};
binary_prop(<<"Assigned">>) -> {<<"Cn">>, true, true};
binary_prop(<<"Upper">>) -> {<<"Uppercase">>, false, true};
binary_prop(<<"Uppercase">>) -> {<<"Uppercase">>, false, true};
binary_prop(<<"EPres">>) -> {<<"Emoji_Presentation">>, false, true};
binary_prop(<<"Emoji_Presentation">>) -> {<<"Emoji_Presentation">>, false, true};
binary_prop(<<"SD">>) -> {<<"Soft_Dotted">>, false, true};
binary_prop(<<"Soft_Dotted">>) -> {<<"Soft_Dotted">>, false, true};
binary_prop(<<"Pat_Syn">>) -> {<<"Pattern_Syntax">>, false, true};
binary_prop(<<"Pattern_Syntax">>) -> {<<"Pattern_Syntax">>, false, true};
binary_prop(<<"EBase">>) -> {<<"Emoji_Modifier_Base">>, false, true};
binary_prop(<<"Emoji_Modifier_Base">>) -> {<<"Emoji_Modifier_Base">>, false, true};
binary_prop(<<"ExtPict">>) -> {<<"Extended_Pictographic">>, false, true};
binary_prop(<<"Extended_Pictographic">>) -> {<<"Extended_Pictographic">>, false, true};
binary_prop(<<"CWL">>) -> {<<"Changes_When_Lowercased">>, false, true};
binary_prop(<<"Changes_When_Lowercased">>) -> {<<"Changes_When_Lowercased">>, false, true};
binary_prop(<<"Pat_WS">>) -> {<<"Pattern_White_Space">>, false, true};
binary_prop(<<"Pattern_White_Space">>) -> {<<"Pattern_White_Space">>, false, true};
binary_prop(<<"Gr_Ext">>) -> {<<"Grapheme_Extend">>, false, true};
binary_prop(<<"Grapheme_Extend">>) -> {<<"Grapheme_Extend">>, false, true};
binary_prop(<<"QMark">>) -> {<<"Quotation_Mark">>, false, true};
binary_prop(<<"Quotation_Mark">>) -> {<<"Quotation_Mark">>, false, true};
binary_prop(<<"Math">>) -> {<<"Math">>, false, true};
binary_prop(<<"CWCM">>) -> {<<"Changes_When_Casemapped">>, false, true};
binary_prop(<<"Changes_When_Casemapped">>) -> {<<"Changes_When_Casemapped">>, false, true};
binary_prop(<<"CWCF">>) -> {<<"Changes_When_Casefolded">>, false, true};
binary_prop(<<"Changes_When_Casefolded">>) -> {<<"Changes_When_Casefolded">>, false, true};
binary_prop(<<"NChar">>) -> {<<"Noncharacter_Code_Point">>, false, true};
binary_prop(<<"Noncharacter_Code_Point">>) -> {<<"Noncharacter_Code_Point">>, false, true};
binary_prop(<<"Bidi_M">>) -> {<<"Bidi_Mirrored">>, false, true};
binary_prop(<<"Bidi_Mirrored">>) -> {<<"Bidi_Mirrored">>, false, true};
binary_prop(<<"Any">>) -> {<<"Any">>, false, true};
binary_prop(<<"Lower">>) -> {<<"Lowercase">>, false, true};
binary_prop(<<"Lowercase">>) -> {<<"Lowercase">>, false, true};
binary_prop(<<"Gr_Base">>) -> {<<"Grapheme_Base">>, false, true};
binary_prop(<<"Grapheme_Base">>) -> {<<"Grapheme_Base">>, false, true};
binary_prop(<<"Join_C">>) -> {<<"Join_Control">>, false, true};
binary_prop(<<"Join_Control">>) -> {<<"Join_Control">>, false, true};
binary_prop(<<"Radical">>) -> {<<"Radical">>, false, true};
binary_prop(<<"AHex">>) -> {<<"ASCII_Hex_Digit">>, false, true};
binary_prop(<<"ASCII_Hex_Digit">>) -> {<<"ASCII_Hex_Digit">>, false, true};
binary_prop(<<"IDST">>) -> {<<"IDS_Trinary_Operator">>, false, true};
binary_prop(<<"IDS_Trinary_Operator">>) -> {<<"IDS_Trinary_Operator">>, false, true};
binary_prop(<<"RI">>) -> {<<"Regional_Indicator">>, false, true};
binary_prop(<<"Regional_Indicator">>) -> {<<"Regional_Indicator">>, false, true};
binary_prop(<<"Cased">>) -> {<<"Cased">>, false, true};
binary_prop(<<"CWKCF">>) -> {<<"Changes_When_NFKC_Casefolded">>, false, false};
binary_prop(<<"Changes_When_NFKC_Casefolded">>) -> {<<"Changes_When_NFKC_Casefolded">>, false, false};
binary_prop(<<"Dia">>) -> {<<"Diacritic">>, false, true};
binary_prop(<<"Diacritic">>) -> {<<"Diacritic">>, false, true};
binary_prop(<<"Ext">>) -> {<<"Extender">>, false, true};
binary_prop(<<"Extender">>) -> {<<"Extender">>, false, true};
binary_prop(<<"CWU">>) -> {<<"Changes_When_Uppercased">>, false, true};
binary_prop(<<"Changes_When_Uppercased">>) -> {<<"Changes_When_Uppercased">>, false, true};
binary_prop(<<"UIdeo">>) -> {<<"Unified_Ideograph">>, false, true};
binary_prop(<<"Unified_Ideograph">>) -> {<<"Unified_Ideograph">>, false, true};
binary_prop(<<"LOE">>) -> {<<"Logical_Order_Exception">>, false, true};
binary_prop(<<"Logical_Order_Exception">>) -> {<<"Logical_Order_Exception">>, false, true};
binary_prop(<<"XIDC">>) -> {<<"XID_Continue">>, false, true};
binary_prop(<<"XID_Continue">>) -> {<<"XID_Continue">>, false, true};
binary_prop(<<"Term">>) -> {<<"Terminal_Punctuation">>, false, true};
binary_prop(<<"Terminal_Punctuation">>) -> {<<"Terminal_Punctuation">>, false, true};
binary_prop(<<"IDSB">>) -> {<<"IDS_Binary_Operator">>, false, true};
binary_prop(<<"IDS_Binary_Operator">>) -> {<<"IDS_Binary_Operator">>, false, true};
binary_prop(<<"Alpha">>) -> {<<"Alphabetic">>, false, true};
binary_prop(<<"Alphabetic">>) -> {<<"Alphabetic">>, false, true};
binary_prop(<<"STerm">>) -> {<<"Sentence_Terminal">>, false, true};
binary_prop(<<"Sentence_Terminal">>) -> {<<"Sentence_Terminal">>, false, true};
binary_prop(<<"EComp">>) -> {<<"Emoji_Component">>, false, true};
binary_prop(<<"Emoji_Component">>) -> {<<"Emoji_Component">>, false, true};
binary_prop(<<"Dash">>) -> {<<"Dash">>, false, true};
binary_prop(<<"IDS">>) -> {<<"ID_Start">>, false, true};
binary_prop(<<"ID_Start">>) -> {<<"ID_Start">>, false, true};
binary_prop(<<"Bidi_C">>) -> {<<"Bidi_Control">>, false, true};
binary_prop(<<"Bidi_Control">>) -> {<<"Bidi_Control">>, false, true};
binary_prop(<<"White_Space">>) -> {<<"White_Space">>, false, true};
binary_prop(<<"space">>) -> {<<"White_Space">>, false, true};
binary_prop(<<"Emoji">>) -> {<<"Emoji">>, false, true};
binary_prop(<<"ASCII">>) -> {<<"ASCII">>, false, true};
binary_prop(<<"DI">>) -> {<<"Default_Ignorable_Code_Point">>, false, true};
binary_prop(<<"Default_Ignorable_Code_Point">>) -> {<<"Default_Ignorable_Code_Point">>, false, true};
binary_prop(<<"Ideo">>) -> {<<"Ideographic">>, false, true};
binary_prop(<<"Ideographic">>) -> {<<"Ideographic">>, false, true};
binary_prop(<<"Hex">>) -> {<<"Hex_Digit">>, false, true};
binary_prop(<<"Hex_Digit">>) -> {<<"Hex_Digit">>, false, true};
binary_prop(<<"XIDS">>) -> {<<"XID_Start">>, false, true};
binary_prop(<<"XID_Start">>) -> {<<"XID_Start">>, false, true};
binary_prop(<<"Dep">>) -> {<<"Deprecated">>, false, true};
binary_prop(<<"Deprecated">>) -> {<<"Deprecated">>, false, true};
binary_prop(_) -> invalid.

