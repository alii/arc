-module(arc_parser_ffi).
-export([
    parse_float/1, decode_string_escapes/1, cook_template_string/1,
    unsafe_byte_slice/3, drop_bytes/2
]).

-define(IS_HEX1(C),
    ((C >= $0 andalso C =< $9) orelse
     (C >= $a andalso C =< $f) orelse
     (C >= $A andalso C =< $F))).
-define(IS_HEX4(A, B, C, D),
    (?IS_HEX1(A) andalso ?IS_HEX1(B) andalso ?IS_HEX1(C) andalso ?IS_HEX1(D))).

%% Byte-offset slicing of the lexer's source binary. The implementation (and
%% the single documented out-of-range policy: clamp, never raise, never
%% validate UTF-8) lives in arc_bytes_ffi, shared with arc_regexp_ffi — the
%% two used to keep divergent copies of the same three functions. The lexer's
%% source binary comes from an already-valid Gleam String and every offset the
%% lexer produces is a char boundary, so no offset here can reach the clamp.
unsafe_byte_slice(Bin, Start, Len) ->
    arc_bytes_ffi:unsafe_slice(Bin, Start, Len).

%% Tail of the source binary from byte offset Pos.
drop_bytes(Bin, Pos) ->
    arc_bytes_ffi:drop_start(Bin, Pos).

%% Convert a decimal float/exponent literal to a double.
%% Returns {ok, Float};
%%         {error, out_of_range} when the text is valid float syntax but its
%%             magnitude overflows an IEEE double (binary_to_float raises
%%             badarg for overflow; underflow rounds to 0.0 and succeeds);
%%         {error, invalid} for text binary_to_float cannot parse at all.
%% The tags mirror number.gleam's `Result(Float, FloatParseError)`.
parse_float(S) ->
    case try_binary_to_float(S) of
        {ok, F} -> {ok, F};
        error ->
            %% Erlang's binary_to_float requires a decimal point. For inputs
            %% like "1e10" or "1E20" we insert ".0" before the exponent so the
            %% string becomes "1.0e10" / "1.0E20" which Erlang accepts.
            case insert_dot_before_exp(S) of
                {ok, S2} ->
                    case try_binary_to_float(S2) of
                        {ok, F} -> {ok, F};
                        error -> classify_float_failure(S2)
                    end;
                error -> classify_float_failure(S)
            end
    end.

try_binary_to_float(S) ->
    try
        {ok, erlang:binary_to_float(S)}
    catch
        error:badarg -> error
    end.

%% binary_to_float raised badarg. If the text is nonetheless well-formed
%% float syntax, the only remaining cause is a magnitude outside the double
%% range — a valid JS literal (e.g. "1.0e400") the caller must not zero out.
classify_float_failure(S) ->
    case is_float_syntax(S) of
        true -> {error, out_of_range};
        false -> {error, invalid}
    end.

%% [+-]?Digits "." Digits ([eE][+-]?Digits)? — the shape binary_to_float
%% accepts, so a badarg on a matching input can only be a range error.
is_float_syntax(S0) ->
    S1 = skip_sign(S0),
    case take_digits(S1) of
        {true, <<".", S2/binary>>} ->
            case take_digits(S2) of
                {true, <<>>} -> true;
                {true, <<E, S3/binary>>} when E =:= $e; E =:= $E ->
                    case take_digits(skip_sign(S3)) of
                        {true, <<>>} -> true;
                        _ -> false
                    end;
                _ -> false
            end;
        _ -> false
    end.

skip_sign(<<C, Rest/binary>>) when C =:= $+; C =:= $- -> Rest;
skip_sign(S) -> S.

%% Consume leading decimal digits: {SawAtLeastOneDigit, Rest}.
take_digits(S) -> take_digits(S, false).
take_digits(<<D, Rest/binary>>, _) when D >= $0, D =< $9 ->
    take_digits(Rest, true);
take_digits(S, Seen) ->
    {Seen, S}.

%% Insert ".0" before the first 'e' or 'E' in a binary, if no '.' precedes it.
%% Returns {ok, NewBinary} or `error` if no exponent or already has decimal.
insert_dot_before_exp(S) ->
    case binary:match(S, [<<"e">>, <<"E">>]) of
        {Pos, _Len} ->
            <<Mantissa:Pos/binary, Rest/binary>> = S,
            case binary:match(Mantissa, <<".">>) of
                nomatch -> {ok, <<Mantissa/binary, ".0", Rest/binary>>};
                _ -> error
            end;
        nomatch -> error
    end.

%% ---------------------------------------------------------------------------
%% Escape decoding: ONE loop, two modes.
%%
%% String literals (ES2024 §12.9.4) and template quasis (§12.9.6) share the
%% whole escape grammar. They differ in exactly one place: legacy octal
%% (\1..\7, \0 followed by a digit) and NonOctalDecimalEscape (\8, \9) are
%% legal in string literals (sloppy mode; strict-mode rejection happens at
%% parser level) and are always a NotEscapeSequence in a template.
%%
%% The loop ALWAYS validates hex digits and \u{...} braces, so a malformed
%% escape can never be silently accepted by one caller and rejected by the
%% other. `Mode` is the only knob: `string | template`.
%% ---------------------------------------------------------------------------

%% Cooked value of a JS string literal's raw content (the text between the
%% quotes). The lexer validated every escape before the parser gets here (see
%% lexer.validate_escape), so an error is a lexer/decoder disagreement — a bug
%% in us, not bad input. Raise instead of inventing a value.
decode_string_escapes(S) when is_binary(S) ->
    case decode_escapes(S, string) of
        {ok, Cooked} -> Cooked;
        {error, invalid_escape} -> erlang:error({invalid_string_escape, S})
    end.

%% Template quasi's Template Value (TV). Input is the RAW quasi text (line
%% endings already normalized to LF). {error, nil} when the quasi contains an
%% escape that is invalid in templates: tagged templates map it to an undefined
%% cooked entry, untagged templates turn it into a SyntaxError.
cook_template_string(S) when is_binary(S) ->
    case decode_escapes(S, template) of
        {ok, Cooked} -> {ok, Cooked};
        {error, invalid_escape} -> {error, nil}
    end.

decode_escapes(S, Mode) ->
    %% Fast path: no backslashes means no escapes (and nothing to validate).
    case binary:match(S, <<"\\">>) of
        nomatch -> {ok, S};
        _ ->
            try
                {ok, iolist_to_binary(escape_loop(S, Mode, []))}
            catch
                throw:invalid_escape -> {error, invalid_escape}
            end
    end.

escape_loop(<<>>, _Mode, Acc) ->
    lists:reverse(Acc);
escape_loop(<<"\\", Rest/binary>>, Mode, Acc) ->
    case Rest of
        %% Trailing backslash: unreachable in a string literal (it would have
        %% escaped the closing quote); a NotEscapeSequence in a template.
        <<>> when Mode =:= string -> lists:reverse(Acc);
        <<>> -> throw(invalid_escape);
        <<"b", T/binary>> -> escape_loop(T, Mode, [<<8>> | Acc]);
        <<"t", T/binary>> -> escape_loop(T, Mode, [<<9>> | Acc]);
        <<"n", T/binary>> -> escape_loop(T, Mode, [<<10>> | Acc]);
        <<"v", T/binary>> -> escape_loop(T, Mode, [<<11>> | Acc]);
        <<"f", T/binary>> -> escape_loop(T, Mode, [<<12>> | Acc]);
        <<"r", T/binary>> -> escape_loop(T, Mode, [<<13>> | Acc]);
        <<"\"", T/binary>> -> escape_loop(T, Mode, [<<34>> | Acc]);
        <<"'", T/binary>> -> escape_loop(T, Mode, [<<39>> | Acc]);
        <<"`", T/binary>> -> escape_loop(T, Mode, [<<"`">> | Acc]);
        <<"$", T/binary>> -> escape_loop(T, Mode, [<<"$">> | Acc]);
        <<"\\", T/binary>> -> escape_loop(T, Mode, [<<"\\">> | Acc]);
        %% Line continuations: \<CR><LF>, \<CR>, \<LF> → empty
        <<"\r\n", T/binary>> -> escape_loop(T, Mode, Acc);
        <<"\r", T/binary>> -> escape_loop(T, Mode, Acc);
        <<"\n", T/binary>> -> escape_loop(T, Mode, Acc);
        %% Unicode line terminators U+2028 (LS) and U+2029 (PS)
        <<16#E2, 16#80, 16#A8, T/binary>> -> escape_loop(T, Mode, Acc);
        <<16#E2, 16#80, 16#A9, T/binary>> -> escape_loop(T, Mode, Acc);
        %% \xHH — exactly 2 hex digits required
        <<"x", H1, H2, T/binary>> when ?IS_HEX1(H1), ?IS_HEX1(H2) ->
            CP = list_to_integer([H1, H2], 16),
            escape_loop(T, Mode, [encode_codepoint(CP) | Acc]);
        <<"x", _/binary>> -> throw(invalid_escape);
        %% \u{...} — 1+ hex digits, value =< 0x10FFFF, closing brace required
        <<"u{", T/binary>> ->
            case read_braced_hex(T) of
                {ok, CU, Rest1} -> unicode_escape(CU, Rest1, Mode, Acc);
                error -> throw(invalid_escape)
            end;
        %% \uHHHH — exactly 4 hex digits required
        <<"u", H1, H2, H3, H4, T/binary>> when ?IS_HEX4(H1, H2, H3, H4) ->
            CU = list_to_integer([H1, H2, H3, H4], 16),
            unicode_escape(CU, T, Mode, Acc);
        <<"u", _/binary>> -> throw(invalid_escape);
        %% \0 not followed by a decimal digit → NUL, in both modes.
        %% \0<digit> is a legacy octal escape: strings only.
        <<"0", T/binary>> ->
            case T of
                <<D, _/binary>> when D >= $0, D =< $9 ->
                    ok = require_legacy_octal(Mode),
                    decode_octal(<<"0", T/binary>>, Acc);
                _ ->
                    escape_loop(T, Mode, [<<0>> | Acc])
            end;
        %% LegacyOctalEscapeSequence \1..\7 — strings only
        <<D, _/binary>> when D >= $1, D =< $7 ->
            ok = require_legacy_octal(Mode),
            decode_octal(Rest, Acc);
        %% NonOctalDecimalEscapeSequence \8, \9 — strings only ('\8' === '8')
        <<D, T/binary>> when D =:= $8; D =:= $9 ->
            ok = require_legacy_octal(Mode),
            escape_loop(T, Mode, [<<D>> | Acc]);
        %% NonEscapeCharacter: \g → "g" etc.
        <<C/utf8, T/binary>> ->
            escape_loop(T, Mode, [<<C/utf8>> | Acc]);
        <<B, T/binary>> ->
            escape_loop(T, Mode, [<<B>> | Acc])
    end;
escape_loop(<<C/utf8, Rest/binary>>, Mode, Acc) ->
    escape_loop(Rest, Mode, [<<C/utf8>> | Acc]);
escape_loop(<<B, Rest/binary>>, Mode, Acc) ->
    %% Fallback for any non-UTF8 byte
    escape_loop(Rest, Mode, [<<B>> | Acc]).

%% The one mode-dependent rule: octal / non-octal decimal escapes are legal in
%% string literals (Annex B) and are always a NotEscapeSequence in templates.
require_legacy_octal(string) -> ok;
require_legacy_octal(template) -> throw(invalid_escape).

%% Read up to 3 octal digits for a legacy octal escape (string mode only). The
%% resulting code point can be >= 0x80 (e.g. \251 -> U+00A9), so it must go
%% through encode_codepoint — appending it as a raw byte would produce an
%% invalid UTF-8 binary inside a value the rest of the engine treats as String.
decode_octal(<<D1, Rest/binary>>, Acc) when D1 >= $0, D1 =< $7 ->
    case Rest of
        <<D2, T/binary>> when D2 >= $0, D2 =< $7 ->
            case T of
                <<D3, T2/binary>> when D3 >= $0, D3 =< $7, D1 =< $3 ->
                    %% 3 octal digits, valid only if first is 0-3
                    CP = list_to_integer([D1, D2, D3], 8),
                    escape_loop(T2, string, [encode_codepoint(CP) | Acc]);
                _ ->
                    CP = list_to_integer([D1, D2], 8),
                    escape_loop(T, string, [encode_codepoint(CP) | Acc])
            end;
        _ ->
            CP = list_to_integer([D1], 8),
            escape_loop(Rest, string, [encode_codepoint(CP) | Acc])
    end.

%% Read hex digits up to a closing brace for \u{...}. Requires at least one
%% hex digit, only hex digits before the brace, and value =< 0x10FFFF.
read_braced_hex(Bin) -> read_braced_hex(Bin, []).

read_braced_hex(<<"}", _/binary>>, []) ->
    error;
read_braced_hex(<<"}", Rest/binary>>, Acc) ->
    CU = list_to_integer(lists:reverse(Acc), 16),
    case CU =< 16#10FFFF of
        true -> {ok, CU, Rest};
        false -> error
    end;
read_braced_hex(<<C, Rest/binary>>, Acc) when ?IS_HEX1(C) ->
    read_braced_hex(Rest, [C | Acc]);
read_braced_hex(_, _Acc) ->
    error.

%% Handle the code unit from a \u escape. JS source is UTF-16, so a high
%% surrogate (D800..DBFF) immediately followed by a low-surrogate escape
%% (DC00..DFFF) forms a single astral codepoint. Lone surrogates can't be
%% encoded as UTF-8, so encode_codepoint maps them to U+FFFD (matching the rest
%% of the runtime — JSON.parse, String.fromCharCode).
unicode_escape(CU, Rest, Mode, Acc) when CU >= 16#D800, CU =< 16#DBFF ->
    case read_low_surrogate_escape(Rest) of
        {ok, Low, Rest1} ->
            CP = 16#10000 + (CU - 16#D800) * 16#400 + (Low - 16#DC00),
            escape_loop(Rest1, Mode, [encode_codepoint(CP) | Acc]);
        error ->
            escape_loop(Rest, Mode, [encode_codepoint(CU) | Acc])
    end;
unicode_escape(CU, Rest, Mode, Acc) ->
    escape_loop(Rest, Mode, [encode_codepoint(CU) | Acc]).

%% Peek for a \uHHHH or \u{...} escape that is a low surrogate (DC00..DFFF).
%% Returns {ok, Low, Rest} consuming it, or `error` without consuming — the
%% high surrogate is then emitted on its own and the escape re-scanned by the
%% loop (which is what validates it).
read_low_surrogate_escape(<<"\\u{", T/binary>>) ->
    case read_braced_hex(T) of
        {ok, CU, Rest} -> classify_low_surrogate(CU, Rest);
        error -> error
    end;
read_low_surrogate_escape(<<"\\u", H1, H2, H3, H4, T/binary>>)
    when ?IS_HEX4(H1, H2, H3, H4) ->
    classify_low_surrogate(list_to_integer([H1, H2, H3, H4], 16), T);
read_low_surrogate_escape(_) ->
    error.

classify_low_surrogate(Low, Rest) when Low >= 16#DC00, Low =< 16#DFFF ->
    {ok, Low, Rest};
classify_low_surrogate(_, _) ->
    error.

encode_codepoint(CP) when CP =< 16#7F ->
    <<CP>>;
%% Lone surrogates have no UTF-8 encoding (<<CP/utf8>> raises badarg) → U+FFFD.
encode_codepoint(CP) when CP >= 16#D800, CP =< 16#DFFF ->
    <<16#EF, 16#BF, 16#BD>>;
encode_codepoint(CP) when CP =< 16#10FFFF ->
    <<CP/utf8>>;
encode_codepoint(_) ->
    <<16#EF, 16#BF, 16#BD>>.   %% U+FFFD replacement
