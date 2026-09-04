-module(arc_regexp_ffi).
-export([regexp_exec_info/5]).
-export([regexp_compile/2, is_compiled/1, regexp_exec_compiled/4]).
-export([pair_trail/1, has_flag/2, take_hex/1]).

has_flag(<<C, _/binary>>, <<C>>) -> true;
has_flag(<<_, R/binary>>, F) -> has_flag(R, F);
has_flag(<<>>, _) -> false.

-define(CS, arc_regex_charset).

%% inclass: false | true | atom (prev item can start a range)
-define(IN_CLASS(X), (X =:= true orelse X =:= atom)).

%% m and s are desugared in tr, never pcre options
flags_to_opts(Flags) ->
    flags_to_opts(Flags, [unicode]).

flags_to_opts(<<>>, Acc) -> Acc;
flags_to_opts(<<"i", Rest/binary>>, Acc) -> flags_to_opts(Rest, [caseless | Acc]);
flags_to_opts(<<_, Rest/binary>>, Acc) -> flags_to_opts(Rest, Acc).

newline_mode(Flags) -> newline_mode(Flags, false, false).

newline_mode(<<>>, M, S) -> {M, S};
newline_mode(<<"m", Rest/binary>>, _M, S) -> newline_mode(Rest, true, S);
newline_mode(<<"s", Rest/binary>>, M, _S) -> newline_mode(Rest, M, true);
newline_mode(<<_, Rest/binary>>, M, S) -> newline_mode(Rest, M, S).

get_compiled(Pattern, Flags) ->
    Opts = flags_to_opts(Flags),
    Mode = unicode_mode(Flags),
    NL = newline_mode(Flags),
    CI = lists:member(caseless, Opts),
    {GroupCount, Names} = count_groups(Pattern, false, 0, []),
    Env = {Mode, CI, index_by_name(Names)},
    Body = tr(Pattern, false, [NL], Env, []),
    Translated = iolist_to_binary([leading_star_prefix(Pattern, NL) | Body]),
    case re:compile(Translated, Opts) of
        {ok, MP} ->
            {ok, {MP, GroupCount, Names}};
        {error, Reason} ->
            {error, {pattern_compile_failed, compile_reason(Reason)}}
    end.

compile_reason({Msg, Pos}) when is_list(Msg), is_integer(Pos) ->
    unicode:characters_to_binary(io_lib:format("~ts at ~b", [Msg, Pos]));
compile_reason(Other) ->
    unicode:characters_to_binary(io_lib:format("~tp", [Other])).

count_groups(<<>>, _InClass, N, Names) ->
    {N, lists:reverse(Names)};
count_groups(<<$\\, _, R/binary>>, InClass, N, Names) ->
    count_groups(R, InClass, N, Names);
count_groups(<<$[, R/binary>>, false, N, Names) ->
    count_groups(R, true, N, Names);
count_groups(<<$], R/binary>>, true, N, Names) ->
    count_groups(R, false, N, Names);
count_groups(<<$(, $?, $<, C, _/binary>> = B, false, N, Names)
  when C =/= $=, C =/= $! ->
    <<_:3/binary, R/binary>> = B,
    {Name, R2, _Terminated} = take_group_name(R),
    count_groups(R2, false, N + 1, [{Name, N + 1} | Names]);
count_groups(<<$(, $?, R/binary>>, false, N, Names) ->
    count_groups(R, false, N, Names);
count_groups(<<$(, R/binary>>, false, N, Names) ->
    count_groups(R, false, N + 1, Names);
count_groups(<<_, R/binary>>, InClass, N, Names) ->
    count_groups(R, InClass, N, Names).

index_by_name(Names) ->
    lists:foldl(
      fun({Name, Idx}, Acc) ->
              case lists:keyfind(Name, 1, Acc) of
                  {Name, Idxs} ->
                      lists:keyreplace(Name, 1, Acc, {Name, Idxs ++ [Idx]});
                  false ->
                      Acc ++ [{Name, [Idx]}]
              end
      end, [], Names).

unicode_mode(<<>>) -> none;
unicode_mode(<<"v", _/binary>>) -> v;
unicode_mode(<<"u", Rest/binary>>) ->
    case unicode_mode(Rest) of
        v -> v;
        _ -> u
    end;
unicode_mode(<<_, Rest/binary>>) -> unicode_mode(Rest).

-define(WORD_BODY, "0-9A-Za-z_").
-define(WORD, "[" ?WORD_BODY "]").
-define(NWORD, "[^" ?WORD_BODY "]").

-define(JSS_CHARS,
        "\\t\\n\\x0B\\f\\r \\x{A0}\\x{1680}\\x{2000}-\\x{200A}"
        "\\x{2028}\\x{2029}\\x{202F}\\x{205F}\\x{3000}\\x{FEFF}").

-define(JS_LT, "\\n\\r\\x{2028}\\x{2029}").

%% restores pcre startline optimisation lost by desugaring dot
leading_star_prefix(<<$., Star, _/binary>>, {_Multiline, DotAll})
  when Star =:= $*; Star =:= $+ ->
    case DotAll of
        true -> "\\G";
        false -> "(?:\\G|(?<=[" ?JS_LT "]))"
    end;
leading_star_prefix(_Pattern, _NewlineMode) ->
    "".

hex_escape(V) -> ["\\x{", integer_to_list(V, 16), "}"].

%% acc is the output chunks, reversed
tr(<<>>, _InClass, _MS, _Env, Acc) ->
    lists:reverse(Acc);
tr(<<$\\, $u, ${, R/binary>>, InClass, MS, Env, Acc) ->
    case take_hex(R) of
        {V, N, <<$}, R2/binary>>} when N > 0 ->
            case V >= 16#D800 andalso V =< 16#DFFF of
                true ->
                    emit_surrogate(?IN_CLASS(InClass), R2, MS, Env, Acc);
                false ->
                    tr(R2, after_atom(InClass), MS, Env, [hex_escape(V) | Acc])
            end;
        _ ->
            tr(R, InClass, MS, Env, [<<"\\u{">> | Acc])
    end;
tr(<<$\\, $u, A, B, C, D, R/binary>> = In, InClass, MS, {Mode, _, _} = Env, Acc) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true ->
            V = list_to_integer([A, B, C, D], 16),
            if
                V >= 16#D800, V =< 16#DBFF,
                (InClass =:= false orelse Mode =/= none) ->
                    case pair_trail(R) of
                        {ok, W, R2} ->
                            Hex = hex_escape(combine_surrogates(V, W)),
                            tr(R2, after_atom(InClass), MS, Env, [Hex | Acc]);
                        none ->
                            emit_surrogate(?IN_CLASS(InClass), R, MS, Env, Acc)
                    end;
                V >= 16#D800, V =< 16#DFFF ->
                    emit_surrogate(?IN_CLASS(InClass), R, MS, Env, Acc);
                true ->
                    tr(R, after_atom(InClass), MS, Env,
                       [<<$\\, $x, ${, A, B, C, D, $}>> | Acc])
            end;
        false ->
            <<_:2/binary, R1/binary>> = In,
            tr(R1, InClass, MS, Env, [<<"\\u">> | Acc])
    end;
tr(<<$\\, P, ${, R/binary>>, InClass, MS, {Mode, _, _} = Env, Acc)
  when (P =:= $p orelse P =:= $P), Mode =/= none ->
    case take_prop(R) of
        {Payload, R2} ->
            case prop_translation(Payload, P =:= $P, ?IN_CLASS(InClass), Mode) of
                {ok, Io} ->
                    tr(R2, after_class_item(InClass), MS, Env, [Io | Acc]);
                error ->
                    tr(R, InClass, MS, Env, [<<$\\, P, ${>> | Acc])
            end;
        none ->
            tr(R, InClass, MS, Env, [<<$\\, P, ${>> | Acc])
    end;
tr(<<$\\, $s, R/binary>>, false, MS, Env, Acc) ->
    tr(R, false, MS, Env, ["[" ?JSS_CHARS "]" | Acc]);
tr(<<$\\, $S, R/binary>>, false, MS, Env, Acc) ->
    tr(R, false, MS, Env, ["[^" ?JSS_CHARS "]" | Acc]);
tr(<<$\\, $w, R/binary>>, false, MS, {Mode, _, _} = Env, Acc) ->
    tr(R, false, MS, Env, [word_atom(Mode) | Acc]);
tr(<<$\\, $W, R/binary>>, false, MS, {Mode, _, _} = Env, Acc) ->
    tr(R, false, MS, Env, [nword_atom(Mode) | Acc]);
tr(<<$\\, $s, R/binary>>, IC, MS, Env, Acc) when ?IN_CLASS(IC) ->
    splice_in_class(?JSS_CHARS, R, MS, Env, Acc);
tr(<<$\\, $S, R/binary>>, IC, MS, {_, CI, _} = Env, Acc) when ?IN_CLASS(IC) ->
    splice_in_class(?CS:emit_complement(?CS:vspace(), CI), R, MS, Env, Acc);
tr(<<$\\, $w, R/binary>>, IC, MS, {Mode, _, _} = Env, Acc) when ?IN_CLASS(IC) ->
    splice_in_class(word_items(Mode), R, MS, Env, Acc);
tr(<<$\\, $W, R/binary>>, IC, MS, {Mode, CI, _} = Env, Acc) when ?IN_CLASS(IC) ->
    splice_in_class(nword_items(Mode, CI), R, MS, Env, Acc);
tr(<<$\\, D, R/binary>>, IC, MS, Env, Acc)
  when ?IN_CLASS(IC), D =:= $d orelse D =:= $D ->
    splice_in_class([$\\, D], R, MS, Env, Acc);
tr(<<$-, $\\, E, _/binary>> = In, IC, MS, Env, Acc)
  when ?IN_CLASS(IC),
       E =:= $d orelse E =:= $D orelse E =:= $s orelse E =:= $S
       orelse E =:= $w orelse E =:= $W ->
    <<_, R/binary>> = In,
    tr(R, true, MS, Env, [<<"\\-">> | Acc]);
tr(<<$-, C, _/binary>> = In, atom, MS, Env, Acc) when C =/= $] ->
    <<_, R/binary>> = In,
    translate_range_hi(R, MS, Env, Acc);
tr(<<$-, R/binary>>, IC, MS, Env, Acc) when ?IN_CLASS(IC) ->
    tr(R, atom, MS, Env, [<<"\\-">> | Acc]);
tr(<<$\\, $b, R/binary>>, false, MS, {Mode, _, _} = Env, Acc) ->
    W = word_atom(Mode),
    Src = ["(?:(?<=", W, ")(?!", W, ")|(?<!", W, ")(?=", W, "))"],
    tr(R, false, MS, Env, [Src | Acc]);
tr(<<$\\, $B, R/binary>>, false, MS, {Mode, _, _} = Env, Acc) ->
    W = word_atom(Mode),
    Src = ["(?:(?<=", W, ")(?=", W, ")|(?<!", W, ")(?!", W, "))"],
    tr(R, false, MS, Env, [Src | Acc]);
tr(<<$\\, $k, $<, R/binary>> = In, false, MS, {_, _, ByName} = Env, Acc) ->
    {Name, R2, _Terminated} = take_group_name(R),
    case lists:keyfind(Name, 1, ByName) of
        {_, [Idx]} ->
            tr(R2, false, MS, Env, [["\\g{", integer_to_list(Idx), "}"] | Acc]);
        {_, Idxs} ->
            Refs = [["\\g{", integer_to_list(I), "}"] || I <- Idxs],
            tr(R2, false, MS, Env, [["(?:", lists:join("|", Refs), ")"] | Acc]);
        false ->
            <<_:2/binary, R1/binary>> = In,
            tr(R1, false, MS, Env, [<<"\\k">> | Acc])
    end;
tr(<<$\\, C, R/binary>>, InClass, MS, Env, Acc)
  when C =:= $v; C =:= $a; C =:= $e; C =:= $g;
       C =:= $h; C =:= $H; C =:= $V; C =:= $R; C =:= $X; C =:= $N;
       C =:= $z; C =:= $Z; C =:= $A; C =:= $G; C =:= $C; C =:= $K ->
    tr(R, after_atom(InClass), MS, Env, [hex_escape(js_escape_cp(C)) | Acc]);
tr(<<$\\, C, R/binary>>, InClass, MS, Env, Acc) ->
    tr(R, after_atom(InClass), MS, Env, [<<$\\, C>> | Acc]);
tr(<<$[, R/binary>>, false, MS, {v, CI, _} = Env, Acc) ->
    case arc_regex_vclass:parse(R, CI) of
        {ok, Ranges0, Strings, R2} ->
            Ranges = case CI of
                         true -> ?CS:vclose(Ranges0);
                         false -> Ranges0
                     end,
            tr(R2, false, MS, Env, [?CS:emit_vclass(Ranges, Strings) | Acc]);
        error ->
            open_class(R, MS, Env, Acc)
    end;
tr(<<$[, R/binary>>, false, MS, Env, Acc) ->
    open_class(R, MS, Env, Acc);
tr(<<$], R/binary>>, IC, MS, Env, Acc) when ?IN_CLASS(IC) ->
    tr(R, false, MS, Env, [$] | Acc]);
%% group names live in Names, pcre only sees the index
tr(<<$(, $?, $<, C, _/binary>> = In, false, [Cur | _] = MS, Env, Acc)
  when C =/= $=, C =/= $! ->
    <<_:3/binary, R/binary>> = In,
    {_Name, R2, _Terminated} = take_group_name(R),
    tr(R2, false, [Cur | MS], Env, [$( | Acc]);
tr(<<$(, R/binary>>, false, [Cur | _] = MS, Env, Acc) ->
    case take_modifiers(R, Cur) of
        {ok, Src, Cur2, R2} ->
            tr(R2, false, [Cur2 | MS], Env, [[$( | Src] | Acc]);
        none ->
            tr(R, false, [Cur | MS], Env, [$( | Acc])
    end;
tr(<<$), R/binary>>, false, MS, Env, Acc) ->
    tr(R, false, pop_ms(MS), Env, [$) | Acc]);
tr(<<$., R/binary>>, false, [{_M, false} | _] = MS, Env, Acc) ->
    tr(R, false, MS, Env, ["[^" ?JS_LT "]" | Acc]);
tr(<<$., R/binary>>, false, [{_M, true} | _] = MS, Env, Acc) ->
    tr(R, false, MS, Env, ["(?s:.)" | Acc]);
tr(<<$^, R/binary>>, false, [{false, _S} | _] = MS, Env, Acc) ->
    tr(R, false, MS, Env, ["\\A" | Acc]);
tr(<<$^, R/binary>>, false, [{true, _S} | _] = MS, Env, Acc) ->
    tr(R, false, MS, Env, ["(?:\\A|(?<=[" ?JS_LT "]))" | Acc]);
tr(<<$$, R/binary>>, false, [{false, _S} | _] = MS, Env, Acc) ->
    tr(R, false, MS, Env, ["\\z" | Acc]);
tr(<<$$, R/binary>>, false, [{true, _S} | _] = MS, Env, Acc) ->
    tr(R, false, MS, Env, ["(?=[" ?JS_LT "]|\\z)" | Acc]);
tr(In, InClass, MS, Env, Acc) ->
    case plain_len(In, 0) of
        0 ->
            <<C, R/binary>> = In,
            tr(R, after_atom(InClass), MS, Env, [C | Acc]);
        N ->
            <<Run:N/binary, R/binary>> = In,
            tr(R, after_atom(InClass), MS, Env, [Run | Acc])
    end.

%% bytes with no clause of their own in tr
plain_len(<<C, R/binary>>, N)
  when C =/= $\\, C =/= $[, C =/= $], C =/= $(, C =/= $), C =/= $.,
       C =/= $^, C =/= $$, C =/= $- ->
    plain_len(R, N + 1);
plain_len(_, N) ->
    N.

open_class(<<$^, Body/binary>>, MS, Env, Acc) ->
    tr(Body, true, MS, Env, [<<"[^">> | Acc]);
open_class(Body, MS, Env, Acc) ->
    tr(Body, true, MS, Env, [$[ | Acc]).

after_atom(false) -> false;
after_atom(_InClass) -> atom.

after_class_item(false) -> false;
after_class_item(_InClass) -> true.

js_escape_cp($v) -> 16#0B;
js_escape_cp(C) -> C.

translate_range_hi(<<$\\, $u, R0/binary>> = L, MS, {Mode, _, _} = Env, Acc) ->
    Braced = case R0 of <<${, _/binary>> -> true; _ -> false end,
    case parse_uescape(R0) of
        {ok, V, R1} when V < 16#D800; V > 16#DFFF ->
            tr(R1, true, MS, Env, [[$- | hex_escape(V)] | Acc]);
        {ok, V, R1} ->
            case (not Braced) andalso Mode =/= none andalso V =< 16#DBFF
                andalso pair_trail(R1) of
                {ok, W, R2} ->
                    Hex = hex_escape(combine_surrogates(V, W)),
                    tr(R2, true, MS, Env, [[$- | Hex] | Acc]);
                _ ->
                    tr(R1, true, MS, Env, [<<"-\\x{D7FF}">> | Acc])
            end;
        none ->
            range_hi_verbatim(L, MS, Env, Acc)
    end;
translate_range_hi(L, MS, Env, Acc) ->
    range_hi_verbatim(L, MS, Env, Acc).

range_hi_verbatim(<<$\\, $x, A, B, R/binary>> = L, MS, Env, Acc) ->
    case is_hex(A) andalso is_hex(B) of
        true -> tr(R, true, MS, Env, [<<$-, $\\, $x, A, B>> | Acc]);
        false -> range_hi_escape(L, MS, Env, Acc)
    end;
range_hi_verbatim(<<$\\, $c, C, R/binary>> = L, MS, Env, Acc) ->
    case (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) of
        true -> tr(R, true, MS, Env, [<<$-, $\\, $c, C>> | Acc]);
        false -> range_hi_escape(L, MS, Env, Acc)
    end;
range_hi_verbatim(L, MS, Env, Acc) ->
    range_hi_escape(L, MS, Env, Acc).

range_hi_escape(<<$\\, C, R/binary>>, MS, Env, Acc)
  when C =:= $v; C =:= $a; C =:= $e; C =:= $g;
       C =:= $h; C =:= $H; C =:= $V; C =:= $R; C =:= $X; C =:= $N;
       C =:= $z; C =:= $Z; C =:= $A; C =:= $G; C =:= $C; C =:= $K ->
    tr(R, true, MS, Env, [[$- | hex_escape(js_escape_cp(C))] | Acc]);
range_hi_escape(<<$\\, C/utf8, R/binary>>, MS, Env, Acc) ->
    tr(R, true, MS, Env, [<<$-, $\\, C/utf8>> | Acc]);
range_hi_escape(<<$\\, C, R/binary>>, MS, Env, Acc) ->
    tr(R, true, MS, Env, [<<$-, $\\, C>> | Acc]);
range_hi_escape(<<C/utf8, R/binary>>, MS, Env, Acc) ->
    tr(R, true, MS, Env, [<<$-, C/utf8>> | Acc]);
range_hi_escape(<<C, R/binary>>, MS, Env, Acc) ->
    tr(R, true, MS, Env, [<<$-, C>> | Acc]).

take_modifiers(<<$?, Rest/binary>>, Cur) ->
    {Add, R1} = take_ims(Rest, []),
    case R1 of
        <<$:, R2/binary>> ->
            {ok, [$? | Add] ++ ":", apply_ims(Cur, Add, []), R2};
        <<$-, R1b/binary>> ->
            case take_ims(R1b, []) of
                {Rem, <<$:, R2/binary>>} ->
                    {ok, [$? | Add] ++ [$- | Rem] ++ ":",
                     apply_ims(Cur, Add, Rem), R2};
                _ -> none
            end;
        _ -> none
    end;
take_modifiers(_Rest, _Cur) -> none.

take_ims(<<C, Rest/binary>>, Acc) when C =:= $i; C =:= $m; C =:= $s ->
    take_ims(Rest, [C | Acc]);
take_ims(Rest, Acc) -> {lists:reverse(Acc), Rest}.

apply_ims({M, S}, Add, Rem) ->
    {ims_bit($m, Add, Rem, M), ims_bit($s, Add, Rem, S)}.

ims_bit(C, Add, Rem, Cur) ->
    case {lists:member(C, Add), lists:member(C, Rem)} of
        {true, _} -> true;
        {_, true} -> false;
        _ -> Cur
    end.

pop_ms([_Inner, Outer | Rest]) -> [Outer | Rest];
pop_ms([Bottom]) -> [Bottom].

%% a dash right after a class escape is literal, escape it
splice_in_class(Items, <<$-, C, _/binary>> = In, MS, Env, Acc) when C =/= $] ->
    <<_, R/binary>> = In,
    tr(R, true, MS, Env, [[Items, "\\-"] | Acc]);
splice_in_class(Items, R, MS, Env, Acc) ->
    tr(R, true, MS, Env, [Items | Acc]).

word_atom(none) -> "\\w";
word_atom(_UOrV) -> ?WORD.

nword_atom(none) -> "\\W";
nword_atom(_UOrV) -> ?NWORD.

word_items(none) -> "\\w";
word_items(_UOrV) -> ?WORD_BODY.

nword_items(none, _CI) -> "\\W";
nword_items(_UOrV, CI) -> ?CS:emit_complement(?CS:vword(), CI).

take_prop(Bin) -> take_prop(Bin, 0, Bin).

take_prop(<<$}, Rest/binary>>, N, Orig) -> {binary:part(Orig, 0, N), Rest};
take_prop(<<C, Rest/binary>>, N, Orig)
  when (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z);
       (C >= $0 andalso C =< $9); C =:= $_; C =:= $= ->
    take_prop(Rest, N + 1, Orig);
take_prop(_, _N, _Orig) -> none.

prop_translation(Payload, Negated, InClass, Mode) ->
    case binary:split(Payload, <<"=">>) of
        [Name, Value] ->
            arc_regex_props_ffi:translate_pair(Name, Value, Negated, InClass);
        [Name] ->
            arc_regex_props_ffi:translate_lone(Name, Negated, InClass, Mode =:= v)
    end.

%% {value, digit count, rest}; value saturates past 0x10ffff
take_hex(Bin) -> take_hex(Bin, 0, 0).

take_hex(<<C, Rest/binary>>, V, N) when C >= $0, C =< $9 ->
    take_hex(Rest, hex_acc(V, C - $0), N + 1);
take_hex(<<C, Rest/binary>>, V, N) when C >= $a, C =< $f ->
    take_hex(Rest, hex_acc(V, C - $a + 10), N + 1);
take_hex(<<C, Rest/binary>>, V, N) when C >= $A, C =< $F ->
    take_hex(Rest, hex_acc(V, C - $A + 10), N + 1);
take_hex(Rest, V, N) -> {V, N, Rest}.

hex_acc(V, _D) when V > 16#10FFFF -> V;
hex_acc(V, D) -> V * 16 + D.

is_hex(C) ->
    (C >= $0 andalso C =< $9)
        orelse (C >= $a andalso C =< $f)
        orelse (C >= $A andalso C =< $F).

%% lone surrogates never match, emit an unmatchable stand-in
emit_surrogate(false, Rest, MS, Env, Acc) ->
    tr(Rest, false, MS, Env, [<<"(?!)">> | Acc]);
emit_surrogate(true, Rest, MS, Env, Acc) ->
    {Item, Rest2} = class_surrogate_item(Rest),
    tr(Rest2, true, MS, Env, [Item | Acc]).

class_surrogate_item(<<$-, C, _/binary>> = Rest) when C =/= $] ->
    <<_, T/binary>> = Rest,
    case class_range_hi(T) of
        {ok, Hi, Rest2} when Hi > 16#DFFF ->
            {["\\x{E000}-\\x{", integer_to_list(Hi, 16), "}"], Rest2};
        {ok, _Hi, Rest2} ->
            {"\\p{Cs}", Rest2};
        none ->
            {"\\p{Cs}\\-", T}
    end;
class_surrogate_item(Rest) ->
    {"\\p{Cs}", Rest}.

class_range_hi(<<$\\, $u, R0/binary>>) -> parse_uescape(R0);
class_range_hi(<<C/utf8, R/binary>>) when C > 16#DFFF -> {ok, C, R};
class_range_hi(_) -> none.

parse_uescape(<<${, R/binary>>) ->
    case take_hex(R) of
        {V, N, <<$}, R2/binary>>} when N > 0 -> {ok, V, R2};
        _ -> none
    end;
parse_uescape(<<A, B, C, D, R/binary>>) ->
    case is_hex(A) andalso is_hex(B) andalso is_hex(C) andalso is_hex(D) of
        true -> {ok, list_to_integer([A, B, C, D], 16), R};
        false -> none
    end;
parse_uescape(_) -> none.

combine_surrogates(Lead, Trail) ->
    16#10000 + (Lead - 16#D800) * 16#400 + (Trail - 16#DC00).

pair_trail(<<$\\, $u, E, F, G, H, Rest/binary>>) ->
    case is_hex(E) andalso is_hex(F) andalso is_hex(G) andalso is_hex(H) of
        true ->
            case list_to_integer([E, F, G, H], 16) of
                W when W >= 16#DC00, W =< 16#DFFF -> {ok, W, Rest};
                _ -> none
            end;
        false -> none
    end;
pair_trail(_) -> none.

regexp_exec_info(Pattern, Flags, String, Offset, Sticky) ->
    case check_offset(String, Offset) of
        {ok, Offset1} ->
            run_compiled(get_compiled(Pattern, Flags), String, Offset1, Sticky);
        {error, _} = Err ->
            Err
    end.

regexp_compile(Pattern, Flags) ->
    get_compiled(Pattern, Flags).

is_compiled({ok, {_MP, _GroupCount, _Names}}) -> true;
is_compiled({error, {pattern_compile_failed, _Reason}}) -> true;
is_compiled(_) -> false.

regexp_exec_compiled(Compiled, String, Offset, Sticky) ->
    case check_offset(String, Offset) of
        {ok, Offset1} -> run_compiled(Compiled, String, Offset1, Sticky);
        {error, _} = Err -> Err
    end.

%% byte offsets; mid-char offset is no_match, never badarg
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

run_compiled({error, {pattern_compile_failed, _Reason}} = Err, _S, _O, _St) ->
    Err;
run_compiled({ok, {MP, GroupCount, Names}}, String, Offset, Sticky) ->
    Opts0 = [{offset, Offset}, {capture, all, index}],
    Opts = case Sticky of
               true -> [anchored | Opts0];
               false -> Opts0
           end,
    case re:run(String, MP, Opts) of
        {match, [Whole | Groups]} ->
            Padded = pad_captures(Groups, GroupCount),
            {ok, {Whole, Padded, GroupCount, Names}};
        nomatch -> {error, no_match}
    end.

pad_captures(Caps, N) when length(Caps) >= N -> Caps;
pad_captures(Caps, N) -> Caps ++ lists:duplicate(N - length(Caps), {-1, 0}).

take_group_name(Bin) ->
    case binary:match(Bin, <<">">>) of
        {P, 1} ->
            {binary:part(Bin, 0, P),
             binary:part(Bin, P + 1, byte_size(Bin) - P - 1), true};
        nomatch ->
            {Bin, <<>>, false}
    end.
