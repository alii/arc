%% §22.2.1 v-flag ClassSetExpression parser
-module(arc_regex_vclass).
-export([parse/2]).

-define(CS, arc_regex_charset).

parse(Bin, CI) -> vclass(Bin, CI).

vclass(<<$^, Rest/binary>>, CI) ->
    case vexpr(Rest, CI) of
        {ok, Ranges, [], Rest2} ->
            {ok, ?CS:character_complement(Ranges, CI), [], Rest2};
        {ok, _Ranges, [_ | _], _Rest2} -> error;
        error -> error
    end;
vclass(Rest, CI) ->
    vexpr(Rest, CI).

vexpr(<<$], Rest/binary>>, _CI) ->
    {ok, [], [], Rest};
vexpr(L, CI) ->
    case vrange_or_item(L, CI) of
        {ok, R, S, <<$&, $&, T/binary>>} -> vchain(T, inter, R, S, CI);
        {ok, R, S, <<$-, $-, T/binary>>} -> vchain(T, subtract, R, S, CI);
        {ok, R, S, Rest} -> vunion(Rest, R, S, CI);
        error -> error
    end.

vunion(<<$], Rest/binary>>, R, S, _CI) -> {ok, R, S, Rest};
vunion(<<>>, _R, _S, _CI) -> error;
vunion(L, R, S, CI) ->
    case vrange_or_item(L, CI) of
        {ok, _R2, _S2, <<$&, $&, _/binary>>} -> error;
        {ok, _R2, _S2, <<$-, $-, _/binary>>} -> error;
        {ok, R2, S2, Rest} -> vunion(Rest, R2 ++ R, S2 ++ S, CI);
        error -> error
    end.

vchain(L, Op, R, S, CI) ->
    case vrange_or_item(L, CI) of
        {ok, R2, S2, Rest} ->
            {R3, S3} = vapply(Op, R, S, R2, S2),
            case Rest of
                <<$], Rest2/binary>> -> {ok, R3, S3, Rest2};
                <<$&, $&, T/binary>> when Op =:= inter -> vchain(T, Op, R3, S3, CI);
                <<$-, $-, T/binary>> when Op =:= subtract -> vchain(T, Op, R3, S3, CI);
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
        {char, _Lo, <<$-, $-, _/binary>>} = Item -> vsingle(Item, CI);
        {char, _Lo, <<$-, $], _/binary>>} ->
            error;
        {char, Lo, <<$-, R2/binary>>} ->
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

vitem(<<$[, Rest/binary>>, CI) ->
    case vclass(Rest, CI) of
        {ok, R, S, Rest2} -> {set, R, S, Rest2};
        error -> error
    end;
vitem(<<$\\, Rest/binary>>, CI) ->
    vescape(Rest, CI);
vitem(<<C, _/binary>>, _CI)
  when C =:= $]; C =:= $(; C =:= $); C =:= ${; C =:= $}; C =:= $/;
       C =:= $-; C =:= $| ->
    error;
vitem(<<C/utf8, Rest/binary>>, _CI) ->
    {char, C, Rest};
vitem(<<C, Rest/binary>>, _CI) ->
    {char, C, Rest};
vitem(<<>>, _CI) ->
    error.

vescape(<<$d, R/binary>>, CI) -> {set, ?CS:vfold(?CS:vdigit(), CI), [], R};
vescape(<<$D, R/binary>>, CI) -> {set, ?CS:character_complement(?CS:vdigit(), CI), [], R};
vescape(<<$w, R/binary>>, CI) -> {set, ?CS:vfold(?CS:vword(), CI), [], R};
vescape(<<$W, R/binary>>, CI) -> {set, ?CS:character_complement(?CS:vword(), CI), [], R};
vescape(<<$s, R/binary>>, CI) -> {set, ?CS:vfold(?CS:vspace(), CI), [], R};
vescape(<<$S, R/binary>>, CI) -> {set, ?CS:character_complement(?CS:vspace(), CI), [], R};
vescape(<<$b, R/binary>>, _CI) -> {char, 16#08, R};
vescape(<<$t, R/binary>>, _CI) -> {char, $\t, R};
vescape(<<$n, R/binary>>, _CI) -> {char, $\n, R};
vescape(<<$v, R/binary>>, _CI) -> {char, 16#0B, R};
vescape(<<$f, R/binary>>, _CI) -> {char, 16#0C, R};
vescape(<<$r, R/binary>>, _CI) -> {char, $\r, R};
vescape(<<$0, D, _/binary>>, _CI) when D >= $0, D =< $9 -> error;
vescape(<<$0, R/binary>>, _CI) -> {char, 0, R};
vescape(<<$c, C, R/binary>>, _CI)
  when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z) ->
    {char, C band 31, R};
vescape(<<$x, A, B, R/binary>>, _CI) ->
    case is_hex(A) andalso is_hex(B) of
        true -> {char, list_to_integer([A, B], 16), R};
        false -> error
    end;
vescape(<<$u, ${, R/binary>>, _CI) ->
    case arc_regexp_ffi:take_hex(R) of
        {CP, N, <<$}, R2/binary>>} when N > 0, CP =< 16#10FFFF -> {char, CP, R2};
        _ -> error
    end;
vescape(<<$u, A, B, C, D, R/binary>>, _CI) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true ->
            CP = list_to_integer([A, B, C, D], 16),
            case CP >= 16#D800 andalso CP =< 16#DBFF of
                true -> vlead_surrogate(CP, R);
                false -> {char, CP, R}
            end;
        false -> error
    end;
vescape(<<$q, ${, R/binary>>, CI) ->
    vstrings(R, [], [], [], CI);
vescape(<<P, ${, R/binary>>, CI) when P =:= $p; P =:= $P ->
    vprop(P =:= $P, R, CI);
vescape(<<C, _/binary>>, _CI)
  when (C >= $0 andalso C =< $9); (C >= $a andalso C =< $z);
       (C >= $A andalso C =< $Z) ->
    error;
vescape(<<C/utf8, R/binary>>, _CI) ->
    {char, C, R};
vescape(<<C, R/binary>>, _CI) ->
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
        <<$}, Rest/binary>> ->
            {R2, S2} = vstring_close(lists:reverse(CurRev), Rs, Ss, CI),
            {set, R2, S2, Rest};
        <<$|, Rest/binary>> ->
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

vstring_char(<<$\\, R/binary>>, CI) ->
    case vescape(R, CI) of
        {char, CP, Rest} -> {char, CP, Rest};
        {set, _R, _S, _Rest} -> error;
        error -> error
    end;
vstring_char(<<C, _/binary>>, _CI)
  when C =:= $(; C =:= $); C =:= $[; C =:= $]; C =:= ${; C =:= $};
       C =:= $/; C =:= $-; C =:= $| ->
    error;
vstring_char(<<C/utf8, R/binary>>, _CI) ->
    {char, C, R};
vstring_char(<<C, R/binary>>, _CI) ->
    {char, C, R};
vstring_char(_, _CI) ->
    error.

vprop(Negated, L, CI) ->
    case take_prop(L, 0, L) of
        {PayloadBin, Rest} ->
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

is_hex(C) ->
    (C >= $0 andalso C =< $9)
        orelse (C >= $a andalso C =< $f)
        orelse (C >= $A andalso C =< $F).

take_prop(<<$}, Rest/binary>>, N, Orig) -> {binary:part(Orig, 0, N), Rest};
take_prop(<<C, Rest/binary>>, N, Orig)
  when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z);
       (C >= $0 andalso C =< $9); C =:= $_; C =:= $= ->
    take_prop(Rest, N + 1, Orig);
take_prop(_, _N, _Orig) -> none.
