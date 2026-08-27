%% \p{...} property policy; prefers exact unicode 17 tables over pcre2's older data
-module(arc_regex_props_ffi).
-export([classify_lone/1, classify_pair/2, translate_lone/4, translate_pair/4,
         char_set/1, string_list/1]).

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

classify_lone(Name) ->
    case resolve_lone(Name) of
        invalid -> prop_invalid;
        strings -> prop_string;
        _ -> prop_valid
    end.

classify_pair(Name, Value) ->
    case resolve_pair(Name, Value) of
        invalid -> prop_invalid;
        _ -> prop_valid
    end.

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
            %% complement alias (Assigned = \P{Cn}); surrogates stripped at emit time
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

%% frozen properties the generated tables omit
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
    case arc_regex_uni17_ffi:ranges(Key) of
        none -> {error, no_exact_data};
        Ranges -> {ok, Ranges}
    end.

string_list(Name) ->
    case string_prop(Name) of
        false -> {error, unknown_property};
        true ->
            case arc_regex_uni17_ffi:string_members(Name) of
                none -> {error, no_exact_data};
                Members -> {ok, Members}
            end
    end.

%% complement excludes surrogates, pcre2 rejects them in utf mode
expand(Key, Negated, InClass, PcreSupported, Fallback) ->
    case arc_regex_uni17_ffi:ranges(Key) of
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

%% properties of strings: v flag only, \p only
string_prop(<<"Basic_Emoji">>) -> true;
string_prop(<<"Emoji_Keycap_Sequence">>) -> true;
string_prop(<<"RGI_Emoji">>) -> true;
string_prop(<<"RGI_Emoji_Flag_Sequence">>) -> true;
string_prop(<<"RGI_Emoji_Modifier_Sequence">>) -> true;
string_prop(<<"RGI_Emoji_Tag_Sequence">>) -> true;
string_prop(<<"RGI_Emoji_ZWJ_Sequence">>) -> true;
string_prop(_) -> false.
