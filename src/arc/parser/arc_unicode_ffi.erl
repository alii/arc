-module(unicode_ffi).
-export([is_id_start/1, is_id_continue/1]).

%% Compiled regexes, lazily initialized via persistent_term.
get_id_start_re() ->
    case persistent_term:get(unicode_id_start_re, undefined) of
        undefined ->
            {ok, Re} = re:compile(<<"^[\\p{L}\\p{Nl}]$">>, [unicode, ucp]),
            persistent_term:put(unicode_id_start_re, Re),
            Re;
        Re -> Re
    end.

get_id_continue_re() ->
    case persistent_term:get(unicode_id_continue_re, undefined) of
        undefined ->
            {ok, Re} = re:compile(<<"^[\\p{L}\\p{Nl}\\p{Mn}\\p{Mc}\\p{Nd}\\p{Pc}]$">>, [unicode, ucp]),
            persistent_term:put(unicode_id_continue_re, Re),
            Re;
        Re -> Re
    end.

%% Check if a codepoint is a valid Unicode ID_Start character.
%% Includes Lu, Ll, Lt, Lm, Lo, Nl + Other_ID_Start.
is_id_start(CP) when is_integer(CP), CP >= 0 ->
    Str = unicode:characters_to_binary([CP], utf8),
    case re:run(Str, get_id_start_re(), [{capture, none}]) of
        match -> true;
        nomatch -> is_other_id_start(CP)
    end.

%% Check if a codepoint is a valid Unicode ID_Continue character.
%% Includes ID_Start + Mn, Mc, Nd, Pc + Other_ID_Continue.
is_id_continue(CP) when is_integer(CP), CP >= 0 ->
    Str = unicode:characters_to_binary([CP], utf8),
    case re:run(Str, get_id_continue_re(), [{capture, none}]) of
        match -> true;
        nomatch ->
            is_other_id_start(CP) orelse is_other_id_continue(CP)
    end.

%% Other_ID_Start characters (not in L or Nl categories).
is_other_id_start(16#2118) -> true;  % SCRIPT CAPITAL P (Weierstrass p)
is_other_id_start(16#212E) -> true;  % ESTIMATED SYMBOL
is_other_id_start(16#309B) -> true;  % KATAKANA-HIRAGANA VOICED SOUND MARK
is_other_id_start(16#309C) -> true;  % KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
is_other_id_start(16#1885) -> true;  % MONGOLIAN LETTER ALI GALI BALUDA
is_other_id_start(16#1886) -> true;  % MONGOLIAN LETTER ALI GALI THREE BALUDA
is_other_id_start(_) -> false.

%% Other_ID_Continue characters (not in Mn, Mc, Nd, Pc or ID_Start).
is_other_id_continue(16#00B7) -> true;  % MIDDLE DOT
is_other_id_continue(16#0387) -> true;  % GREEK ANO TELEIA
is_other_id_continue(16#1369) -> true;  % ETHIOPIC DIGIT ONE
is_other_id_continue(CP) when CP >= 16#136A, CP =< 16#1371 -> true;  % ETHIOPIC DIGITS
is_other_id_continue(16#19DA) -> true;  % NEW TAI LUE THAM DIGIT ONE
is_other_id_continue(_) -> false.
