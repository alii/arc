%% §22.2.1 v-flag ClassSetExpression parser
-module(arc_regex_vclass).
-export([parse/2]).

-define(CS, arc_regex_charset).

parse(L, CI) -> vclass(L, CI).

vclass([$^ | Rest], CI) ->
    case vexpr(Rest, CI) of
        {ok, Ranges, [], Rest2} ->
            {ok, ?CS:character_complement(Ranges, CI), [], Rest2};
        {ok, _Ranges, [_ | _], _Rest2} -> error;
        error -> error
    end;
vclass(Rest, CI) ->
    vexpr(Rest, CI).

vexpr([$] | Rest], _CI) ->
    {ok, [], [], Rest};
vexpr(L, CI) ->
    case vrange_or_item(L, CI) of
        {ok, R, S, [$&, $& | T]} -> vchain(T, inter, R, S, CI);
        {ok, R, S, [$-, $- | T]} -> vchain(T, subtract, R, S, CI);
        {ok, R, S, Rest} -> vunion(Rest, R, S, CI);
        error -> error
    end.

vunion([$] | Rest], R, S, _CI) -> {ok, R, S, Rest};
vunion([], _R, _S, _CI) -> error;
vunion(L, R, S, CI) ->
    case vrange_or_item(L, CI) of
        {ok, _R2, _S2, [$&, $& | _]} -> error;
        {ok, _R2, _S2, [$-, $- | _]} -> error;
        {ok, R2, S2, Rest} -> vunion(Rest, R2 ++ R, S2 ++ S, CI);
        error -> error
    end.

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
    S2u = lists:usort(S2),
    {?CS:vinter(R, R2), [X || X <- lists:usort(S), lists:member(X, S2u)]};
vapply(subtract, R, S, R2, S2) ->
    S2u = lists:usort(S2),
    {?CS:vsubtract(R, R2), [X || X <- lists:usort(S), not lists:member(X, S2u)]}.

vrange_or_item(L, CI) ->
    case vitem(L, CI) of
        {char, _Lo, [$-, $- | _]} = Item -> vsingle(Item, CI);
        {char, _Lo, [$-, $] | _]} ->
            error;
        {char, Lo, [$- | R2]} ->
            case vitem(R2, CI) of
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
vescape([C | R], _CI)
  when not ((C >= $0 andalso C =< $9)
            orelse (C >= $a andalso C =< $z)
            orelse (C >= $A andalso C =< $Z)) ->
    {char, C, R};
vescape(_, _CI) ->
    error.

vlead_surrogate(Lead, R) ->
    case arc_regexp_ffi:pair_trail(R) of
        {ok, Trail, R2} -> {char, combine_surrogates(Lead, Trail), R2};
        none -> {char, Lead, R}
    end.

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

vprop(Negated, L, CI) ->
    case take_prop(L, []) of
        {Payload, Rest} ->
            PayloadBin = list_to_binary(Payload),
            case arc_regex_props_ffi:char_set(PayloadBin) of
                {ok, Ranges} when Negated ->
                    {set, ?CS:character_complement(Ranges, CI), [], Rest};
                {ok, Ranges} ->
                    {set, ?CS:vfold(Ranges, CI), [], Rest};
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
        {error, no_exact_data} -> error
    end.

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
