-module(arc_rt_string_ffi).
-export([trim_js_ws/1, trim_leading_js_ws/1, trim_trailing_js_ws/1]).

%% JS-string byte walkers that need no engine state. Codepoint indexing,
%% search and slicing live in arc_string_ffi (src/arc/rt); this module holds
%% only the StrWhiteSpace trims shared by String.prototype.trim*, parseInt
%% and parseFloat. Same invalid-UTF-8 policy: a JS string is always
%% well-formed UTF-8, so no walker has a per-byte fallback clause.

%% UTF-8 encoded byte length of a codepoint.
cp_byte_size(C) when C < 16#80 -> 1;
cp_byte_size(C) when C < 16#800 -> 2;
cp_byte_size(C) when C < 16#10000 -> 3;
cp_byte_size(_) -> 4.

%% ---------------------------------------------------------------------------
%% TrimString §22.1.3.33.1 — StrWhiteSpace (WhiteSpace ∪ LineTerminator)
%% ---------------------------------------------------------------------------
%% NOT Unicode White_Space: U+0085 NEL is excluded and U+FEFF ZWNBSP included,
%% matching the JS spec. rt_val.gleam has these as private BitArray fns;
%% expose thin binary→binary wrappers here so string.gleam can reuse them.

trim_js_ws(Bin) -> trim_trailing_js_ws(trim_leading_js_ws(Bin)).

trim_leading_js_ws(<<C, R/binary>>)
    when C =:= 16#09; C =:= 16#0A; C =:= 16#0B; C =:= 16#0C; C =:= 16#0D;
         C =:= 16#20 ->
    trim_leading_js_ws(R);
trim_leading_js_ws(<<16#C2, 16#A0, R/binary>>) -> trim_leading_js_ws(R);
%% U+1680, U+2000..U+200A, U+2028, U+2029, U+202F, U+205F, U+3000
trim_leading_js_ws(<<16#E1, 16#9A, 16#80, R/binary>>) -> trim_leading_js_ws(R);
trim_leading_js_ws(<<16#E2, 16#80, C, R/binary>>)
    when C >= 16#80, C =< 16#8A; C =:= 16#A8; C =:= 16#A9; C =:= 16#AF ->
    trim_leading_js_ws(R);
trim_leading_js_ws(<<16#E2, 16#81, 16#9F, R/binary>>) -> trim_leading_js_ws(R);
trim_leading_js_ws(<<16#E3, 16#80, 16#80, R/binary>>) -> trim_leading_js_ws(R);
%% U+FEFF
trim_leading_js_ws(<<16#EF, 16#BB, 16#BF, R/binary>>) -> trim_leading_js_ws(R);
trim_leading_js_ws(Bin) -> Bin.

trim_trailing_js_ws(Bin) ->
    Keep = last_non_ws(Bin, 0, 0),
    case Keep =:= byte_size(Bin) of
        true -> Bin;
        false -> binary:part(Bin, 0, Keep)
    end.

%% Walk forward tracking the byte offset one past the last non-WS codepoint.
last_non_ws(<<>>, _Off, Last) -> Last;
last_non_ws(<<C, R/binary>>, Off, Last)
    when C =:= 16#09; C =:= 16#0A; C =:= 16#0B; C =:= 16#0C; C =:= 16#0D;
         C =:= 16#20 ->
    last_non_ws(R, Off + 1, Last);
last_non_ws(<<16#C2, 16#A0, R/binary>>, Off, Last) ->
    last_non_ws(R, Off + 2, Last);
last_non_ws(<<16#E1, 16#9A, 16#80, R/binary>>, Off, Last) ->
    last_non_ws(R, Off + 3, Last);
last_non_ws(<<16#E2, 16#80, C, R/binary>>, Off, Last)
    when C >= 16#80, C =< 16#8A; C =:= 16#A8; C =:= 16#A9; C =:= 16#AF ->
    last_non_ws(R, Off + 3, Last);
last_non_ws(<<16#E2, 16#81, 16#9F, R/binary>>, Off, Last) ->
    last_non_ws(R, Off + 3, Last);
last_non_ws(<<16#E3, 16#80, 16#80, R/binary>>, Off, Last) ->
    last_non_ws(R, Off + 3, Last);
last_non_ws(<<16#EF, 16#BB, 16#BF, R/binary>>, Off, Last) ->
    last_non_ws(R, Off + 3, Last);
last_non_ws(<<C/utf8, R/binary>>, Off, _Last) ->
    W = cp_byte_size(C),
    last_non_ws(R, Off + W, Off + W).
