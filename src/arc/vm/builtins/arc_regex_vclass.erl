%% v-flag ClassSetExpression parser (§22.2.1 ClassSetExpression).
%%
%% Owns the recursive-descent walk of a v-mode `[...]` body — nested classes,
%% && intersection, -- subtraction, \q{...} string alternatives, and \p{...}
%% properties of strings — none of which PCRE understands. arc_regexp_ffi
%% calls parse/2 at the ONE point it meets a `[` under the v flag; everything
%% here is that grammar and nothing else.
%%
%% A v-mode class evaluates to a pair {Ranges, Strings}: a set of codepoints
%% (sorted disjoint ranges) and a set of multi-codepoint strings (lists of
%% codepoints; single-codepoint members are folded into Ranges, the empty
%% string is the [] member). Union, && intersection and -- subtraction apply
%% pointwise to both components. The parser (regex.gleam) has already
%% validated literals, so this evaluator is lenient: anything it can't
%% express returns `error` and the class falls back to the generic path.
%%
%% What lives here is only the PARSER: it walks the class body and hands the
%% operand sets it recognises to ?CS (arc_regex_charset), which owns the set
%% algebra, the case folding (CI, the i flag, changes what every primitive
%% operand set means — see that module) and the emit back to PCRE text.
-module(arc_regex_vclass).
-export([parse/2]).

%% The codepoint-set algebra module, abbreviated: it appears often enough in
%% the class evaluator that spelling it out would hide the code.
-define(CS, arc_regex_charset).

%% parse(L, CI): the body of a class after `[`, through the closing `]`.
%%   -> {ok, Ranges, Strings, Rest} | error
parse(L, CI) -> vclass(L, CI).

vclass([$^ | Rest], CI) ->
    case vexpr(Rest, CI) of
        {ok, Ranges, [], Rest2} ->
            {ok, ?CS:character_complement(Ranges, CI), [], Rest2};
        %% A negated class may not contain strings (MayContainStrings).
        {ok, _Ranges, [_ | _], _Rest2} -> error;
        error -> error
    end;
vclass(Rest, CI) ->
    vexpr(Rest, CI).

%% First operand decides the expression form: union, && chain, or -- chain.
vexpr([$] | Rest], _CI) ->
    %% `[]` — the empty set, matches nothing.
    {ok, [], [], Rest};
vexpr(L, CI) ->
    case vrange_or_item(L, CI) of
        {ok, R, S, [$&, $& | T]} -> vchain(T, inter, R, S, CI);
        {ok, R, S, [$-, $- | T]} -> vchain(T, subtract, R, S, CI);
        {ok, R, S, Rest} -> vunion(Rest, R, S, CI);
        error -> error
    end.

%% ClassUnion: operands and ranges until the closing ].
vunion([$] | Rest], R, S, _CI) -> {ok, R, S, Rest};
vunion([], _R, _S, _CI) -> error;
vunion(L, R, S, CI) ->
    case vrange_or_item(L, CI) of
        %% Operators may not be mixed into a union (e.g. `[ab--c]`).
        {ok, _R2, _S2, [$&, $& | _]} -> error;
        {ok, _R2, _S2, [$-, $- | _]} -> error;
        {ok, R2, S2, Rest} -> vunion(Rest, R2 ++ R, S2 ++ S, CI);
        error -> error
    end.

%% ClassIntersection / ClassSubtraction: operand (op operand)* ].
vchain(L, Op, R, S, CI) ->
    case vrange_or_item(L, CI) of
        {ok, R2, S2, Rest} ->
            {R3, S3} = vapply(Op, R, S, R2, S2),
            case Rest of
                [$] | Rest2] -> {ok, R3, S3, Rest2};
                [$&, $& | T] when Op =:= inter -> vchain(T, Op, R3, S3, CI);
                [$-, $- | T] when Op =:= subtract -> vchain(T, Op, R3, S3, CI);
                _ -> error
            end;
        error -> error
    end.

vapply(inter, R, S, R2, S2) ->
    {?CS:vinter(R, R2),
     ordsets:intersection(ordsets:from_list(S), ordsets:from_list(S2))};
vapply(subtract, R, S, R2, S2) ->
    {?CS:vsubtract(R, R2),
     ordsets:subtract(ordsets:from_list(S), ordsets:from_list(S2))}.

%% One ClassSetOperand, possibly extended to a ClassSetRange (`a-z`).
%% A trailing `--` is left unconsumed for the caller's operator dispatch.
vrange_or_item(L, CI) ->
    case vitem(L, CI) of
        {char, _Lo, [$-, $- | _]} = Item -> vsingle(Item, CI);
        {char, _Lo, [$-, $] | _]} ->
            %% Dangling `-` is not a ClassSetCharacter in v mode.
            error;
        {char, Lo, [$- | R2]} ->
            case vitem(R2, CI) of
                %% Range validity uses the RAW endpoints; the resulting set
                %% is then folded (MaybeSimpleCaseFolding of CharacterRange).
                {char, Hi, R3} when Lo =< Hi -> {ok, ?CS:vfold([{Lo, Hi}], CI), [], R3};
                {char, _Hi, _R3} -> error;
                {set, _R, _S, _Rest} -> error;
                error -> error
            end;
        {char, _CP, _Rest} = Item -> vsingle(Item, CI);
        {set, R, S, Rest} -> {ok, R, S, Rest};
        error -> error
    end.

vsingle({char, CP, Rest}, CI) -> {ok, ?CS:vfold([{CP, CP}], CI), [], Rest}.

%% One operand: nested class, escape, or literal ClassSetCharacter.
vitem([$[ | Rest], CI) ->
    case vclass(Rest, CI) of
        {ok, R, S, Rest2} -> {set, R, S, Rest2};
        error -> error
    end;
vitem([$\\ | Rest], CI) ->
    vescape(Rest, CI);
vitem([C | _], _CI)
  when C =:= $]; C =:= $(; C =:= $); C =:= ${; C =:= $}; C =:= $/;
       C =:= $-; C =:= $| ->
    error;
vitem([C | Rest], _CI) ->
    {char, C, Rest};
vitem([], _CI) ->
    error.

%% Class escapes (\d, \w, ...), character escapes, \q{...} and \p{...}.
%% Set-producing escapes fold their set here; {char, ...} results are raw
%% (range endpoints must stay raw) and are folded by vsingle/vrange_or_item.
vescape([$d | R], CI) -> {set, ?CS:vfold(?CS:vdigit(), CI), [], R};
vescape([$D | R], CI) -> {set, ?CS:character_complement(?CS:vdigit(), CI), [], R};
vescape([$w | R], CI) -> {set, ?CS:vfold(?CS:vword(), CI), [], R};
vescape([$W | R], CI) -> {set, ?CS:character_complement(?CS:vword(), CI), [], R};
vescape([$s | R], CI) -> {set, ?CS:vfold(?CS:vspace(), CI), [], R};
vescape([$S | R], CI) -> {set, ?CS:character_complement(?CS:vspace(), CI), [], R};
vescape([$b | R], _CI) -> {char, 16#08, R};
vescape([$t | R], _CI) -> {char, $\t, R};
vescape([$n | R], _CI) -> {char, $\n, R};
vescape([$v | R], _CI) -> {char, 16#0B, R};
vescape([$f | R], _CI) -> {char, 16#0C, R};
vescape([$r | R], _CI) -> {char, $\r, R};
vescape([$0, D | _], _CI) when D >= $0, D =< $9 -> error;
vescape([$0 | R], _CI) -> {char, 0, R};
vescape([$c, C | R], _CI)
  when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z) ->
    {char, C band 31, R};
vescape([$x, A, B | R], _CI) ->
    case is_hex(A) andalso is_hex(B) of
        true -> {char, list_to_integer([A, B], 16), R};
        false -> error
    end;
vescape([$u, ${ | R], _CI) ->
    case take_hex(R, []) of
        {Hex, [$} | R2]} when Hex =/= [] ->
            CP = list_to_integer(Hex, 16),
            case CP =< 16#10FFFF of
                true -> {char, CP, R2};
                false -> error
            end;
        _ -> error
    end;
vescape([$u, A, B, C, D | R], _CI) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true ->
            CP = list_to_integer([A, B, C, D], 16),
            case CP >= 16#D800 andalso CP =< 16#DBFF of
                true -> vlead_surrogate(CP, R);
                false -> {char, CP, R}
            end;
        false -> error
    end;
vescape([$q, ${ | R], CI) ->
    vstrings(R, [], [], [], CI);
vescape([P, ${ | R], CI) when P =:= $p; P =:= $P ->
    vprop(P =:= $P, R, CI);
%% Identity escapes: any non-alphanumeric (covers ClassSetSyntaxCharacter
%% and the ClassSetReservedPunctuators).
vescape([C | R], _CI)
  when not ((C >= $0 andalso C =< $9)
            orelse (C >= $a andalso C =< $z)
            orelse (C >= $A andalso C =< $Z)) ->
    {char, C, R};
vescape(_, _CI) ->
    error.

%% A \uHHHH lead surrogate followed by a \uHHHH trail surrogate is one
%% combined codepoint (the pattern is parsed as UTF-16 per the spec).
%% pair_trail/1 is the same trail-surrogate reader the general translation
%% uses, so both paths agree on what a trail escape looks like.
vlead_surrogate(Lead, R) ->
    case arc_regexp_ffi:pair_trail(R) of
        {ok, Trail, R2} -> {char, combine_surrogates(Lead, Trail), R2};
        none -> {char, Lead, R}
    end.

%% \q{a|bc|}: alternatives separated by |. Single-codepoint alternatives are
%% characters; everything else (length 0 or 2+) joins the string set.
vstrings(L, CurRev, Rs, Ss, CI) ->
    case L of
        [$} | Rest] ->
            {R2, S2} = vstring_close(lists:reverse(CurRev), Rs, Ss, CI),
            {set, R2, S2, Rest};
        [$| | Rest] ->
            {R2, S2} = vstring_close(lists:reverse(CurRev), Rs, Ss, CI),
            vstrings(Rest, [], R2, S2, CI);
        _ ->
            case vstring_char(L, CI) of
                {char, CP, Rest} -> vstrings(Rest, [CP | CurRev], Rs, Ss, CI);
                error -> error
            end
    end.

vstring_close([CP], Rs, Ss, CI) -> {?CS:vfold([{CP, CP}], CI) ++ Rs, Ss};
vstring_close(Str, Rs, Ss, CI) -> {Rs, [?CS:vfold_str(Str, CI) | Ss]}.

%% One ClassString character — a ClassSetCharacter; no class escapes here.
vstring_char([$\\ | R], CI) ->
    case vescape(R, CI) of
        {char, CP, Rest} -> {char, CP, Rest};
        {set, _R, _S, _Rest} -> error;
        error -> error
    end;
vstring_char([C | R], _CI)
  when C =/= $(, C =/= $), C =/= $[, C =/= $], C =/= ${, C =/= $},
       C =/= $/, C =/= $-, C =/= $\\, C =/= $| ->
    {char, C, R};
vstring_char(_, _CI) ->
    error.

%% \p{...} / \P{...} inside a v-mode class: exact ranges, or (non-negated)
%% a property of strings. \P complements the FOLDED set (so /[\P{Lu}]/iv
%% behaves like /[^\p{Lu}]/iv, per UnicodeSets semantics).
vprop(Negated, L, CI) ->
    case take_prop(L, []) of
        {Payload, Rest} ->
            PayloadBin = list_to_binary(Payload),
            case arc_regex_props_ffi:char_set(PayloadBin) of
                {ok, Ranges} when Negated ->
                    {set, ?CS:character_complement(Ranges, CI), [], Rest};
                {ok, Ranges} ->
                    {set, ?CS:vfold(Ranges, CI), [], Rest};
                %% Only a property OF STRINGS has a string list, and only \p
                %% (never \P) may name one — no blind retry on other causes.
                {error, property_of_strings} when not Negated ->
                    vstring_prop(PayloadBin, Rest, CI);
                {error, property_of_strings} -> error;
                {error, unknown_property} -> error;
                {error, no_exact_data} -> error
            end;
        none -> error
    end.

vstring_prop(PayloadBin, Rest, CI) ->
    case arc_regex_props_ffi:string_list(PayloadBin) of
        {ok, Strs} ->
            {R, S} = ?CS:vsplit_singles(Strs, CI),
            {set, R, S, Rest};
        %% RGI_Emoji is stored as a compressed regex the desugarer cannot
        %% decode; the class then falls back to the non-desugared translation.
        {error, no_exact_data} -> error
    end.

%% ---- Tiny lexical helpers (duplicated from arc_regexp_ffi) --------------
%% Hex digits and property-name characters are what they are; keeping these
%% three-liners local means this module reads standalone.

combine_surrogates(Lead, Trail) ->
    16#10000 + (Lead - 16#D800) * 16#400 + (Trail - 16#DC00).

take_hex([C | Rest], Acc) ->
    case is_hex(C) of
        true -> take_hex(Rest, [C | Acc]);
        false -> {lists:reverse(Acc), [C | Rest]}
    end;
take_hex([], Acc) -> {lists:reverse(Acc), []}.

is_hex(C) ->
    (C >= $0 andalso C =< $9)
        orelse (C >= $a andalso C =< $f)
        orelse (C >= $A andalso C =< $F).

take_prop([$} | Rest], Acc) -> {lists:reverse(Acc), Rest};
take_prop([C | Rest], Acc)
  when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z);
       (C >= $0 andalso C =< $9); C =:= $_; C =:= $= ->
    take_prop(Rest, [C | Acc]);
take_prop(_, _Acc) -> none.
