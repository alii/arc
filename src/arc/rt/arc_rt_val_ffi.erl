%%% arc_rt_val_ffi — the ONE wire↔Gleam decode point for `JsVal`
%%% (SPEC.md §2.3 / D16, «VALUE-ABI-FROZEN»).
%%%
%%% Hand-written Erlang, so it carries the `arc_rt_` namespace prefix
%%% (overview §5) and can NEVER collide with an OTP module — exactly like
%%% `arc_rt_store_ffi`. Tier-P: pure BEAM-term
%%% pattern-matching, no NIF, no pdict, cannot crash the node.
%%%
%%% Why an FFI shim at all: `rt_types.JsVal` is OPAQUE at the Gleam type
%%% level (D16). Its wire encoding is the §2.3 tagged-term shape (bare atoms
%%% for undefined/null/booleans/NaN/±Inf/TDZ, bare integers/floats/binaries
%%% for finite numbers/strings, and `{js_bigint,_}` / `{js_sym,_}` /
%%% `{js_cell,_}` tuples for the boxed kinds). Gleam code NEVER matches on
%%% that term directly — it calls `classify/1` to get a `JsValKind` sum, and
%%% builds values via the `mk_*` encoders below. Changing a wire row means
%%% editing exactly this file; nothing in rt_js Gleam recompiles.
%%%
%%% Gleam constructor lowering (verified against gleam_stdlib's compiled
%%% Option: `{some, X} | none`): a nullary variant `KUndef` lowers to the atom
%%% `k_undef`; a payload variant `KNum(JsNum)` lowers to `{k_num, JsNum}`.
%%% `Handle`'s single constructor `JsCell(id: Int)` lowers to `{js_cell, N}`
%%% (R4) — which is ALREADY the object wire form, so `mk_object/1` is identity.
-module(arc_rt_val_ffi).

-export([
    classify/1,
    mk_undefined/0, mk_hole/0, mk_null/0, mk_bool/1, mk_number/1, mk_int/1,
    mk_string/1, mk_bigint/1, mk_symbol/1, mk_object/1, mk_tdz/0,
    to_boolean_i32/1,
    strict_eq/2, same_value_zero/2,
    t_to_property_key_fast/1,
    js_number_to_string/1,
    parse_float/1,
    is_neg_zero/1, float_same_term/2
]).

%% classify(JsVal) -> JsValKind
%%
%% Decode a §2.3 wire term into the `rt_types.JsValKind` sum. Head clauses
%% are ordered by the §2.3 discriminator table (undefined → null → boolean →
%% finite number → NaN/±Inf → string → bigint → symbol → object → TDZ). No
%% catch-all: a term outside the wire encoding is a `function_clause` crash,
%% which is the correct fail-closed behavior for a violated ABI invariant.
classify(undefined) -> k_undef;
classify(null) -> k_null;
classify(true) -> {k_bool, true};
classify(false) -> {k_bool, false};
classify(N) when is_integer(N) -> {k_num, {j_int, N}};
classify(N) when is_float(N) -> {k_num, {j_float, N}};
classify(js_nan) -> {k_num, j_nan};
classify(js_inf) -> {k_num, j_pos_inf};
classify(js_neg_inf) -> {k_num, j_neg_inf};
classify(B) when is_binary(B) -> {k_str, B};
classify({js_bigint, N}) -> {k_big, N};
classify({js_sym, S}) -> {k_sym, S};
classify({js_cell, N}) -> {k_handle, {js_cell, N}};
classify(js_tdz) -> k_tdz.

%% to_boolean_i32(JsVal) -> 0 | 1
%% ES2024 §7.1.2 ToBoolean as an i32 for direct use as an ir.If cond.
%% Direct guard-dispatch on the wire form — no {k_*, …} boxing (drops
%% ~99k classify/1 calls per richards run). Total; must stay row-for-row
%% equivalent with rt_val.gleam:to_boolean.
to_boolean_i32(undefined) -> 0;
to_boolean_i32(null) -> 0;
to_boolean_i32(false) -> 0;
to_boolean_i32(true) -> 1;
to_boolean_i32(0) -> 0;
to_boolean_i32(N) when is_integer(N) -> 1;
to_boolean_i32(F) when is_float(F) ->
    case F == 0.0 of true -> 0; false -> 1 end;
to_boolean_i32(js_nan) -> 0;
to_boolean_i32(js_inf) -> 1;
to_boolean_i32(js_neg_inf) -> 1;
to_boolean_i32(<<>>) -> 0;
to_boolean_i32(B) when is_binary(B) -> 1;
to_boolean_i32({js_bigint, 0}) -> 0;
to_boolean_i32({js_bigint, _}) -> 1;
to_boolean_i32({js_sym, _}) -> 1;
to_boolean_i32({js_cell, _}) -> 1;
to_boolean_i32(js_tdz) -> 0.

%% strict_eq(A, B) -> boolean()
%% §7.2.15 IsStrictlyEqual, total on wire terms. NaN is unequal to itself;
%% Numbers compare numerically (1 === 1.0, +0 === -0); every other row is
%% exact term identity (same atom, same binary, same {js_cell,N} /
%% {js_bigint,N} / {js_sym,S}).
strict_eq(js_nan, _) -> false;
strict_eq(_, js_nan) -> false;
strict_eq(A, B) when is_number(A), is_number(B) -> A == B;
strict_eq(A, B) -> A =:= B.

%% same_value_zero(A, B) -> boolean()
%% §7.2.12 SameValueZero: IsStrictlyEqual except NaN equals NaN.
same_value_zero(js_nan, js_nan) -> true;
same_value_zero(A, B) -> strict_eq(A, B).

%% t_to_property_key_fast(V) -> ObjectKey | miss
%% JPure §7.1.19 ToPropertyKey for the primitive shapes whose result does
%% NOT depend on St (l-jread-reclass): int / string / symbol build the wire
%% key directly; every other shape — {js_cell,_} (needs ToPrimitive → user
%% code → St), float / negative / out-of-range int / bigint / bool / null /
%% undefined / NaN / ±Inf / TDZ — returns `miss` and the emitter falls to
%% JMut `to_property_key`. IsAtom on the 2-tuple result is false, on `miss`
%% true. `?MAX_ARRAY_INDEX` = 2^32-2 is pinned with rt_types.gleam:131
%% (`max_array_index`) by the classify round-trip test.
-define(MAX_ARRAY_INDEX, 4294967294).
t_to_property_key_fast(N)
  when is_integer(N), N >= 0, N =< ?MAX_ARRAY_INDEX ->
    {string_key, {index, N}};
t_to_property_key_fast(B) when is_binary(B) ->
    {string_key, canonical_key_bin(B)};
t_to_property_key_fast({js_sym, S}) ->
    {symbol_key, S};
t_to_property_key_fast(_) -> miss.

%% Mirror of rt_types.canonical_key/1 (gleam:166-185): "5" → {index,5};
%% "05"/non-numeric → {named,B}. Leading-digit guard avoids the try/catch
%% on every named key.
canonical_key_bin(<<C, _/binary>> = B) when C >= $0, C =< $9 ->
    try binary_to_integer(B) of
        N when N >= 0, N =< ?MAX_ARRAY_INDEX ->
            case integer_to_binary(N) =:= B of
                true -> {index, N};
                false -> {named, B}
            end;
        _ -> {named, B}
    catch _:_ -> {named, B}
    end;
canonical_key_bin(B) -> {named, B}.

%% mk_hole() -> the dense element store's default: an ABSENT index. Not a
%% JsVal — classify/1 has no clause for it — so a hole can never escape as a
%% value; readers turn it into `none`. The AOT emitter passes the same atom
%% for an array-literal elision.
mk_hole() -> js_hole.

%% mk_undefined() -> JsVal
%% The `undefined` wire term.
mk_undefined() -> undefined.

%% mk_null() -> JsVal
%% The `null` wire term.
mk_null() -> null.

%% mk_bool(Bool) -> JsVal
%% Gleam `Bool` is already the atoms `true`/`false` — the boolean wire form.
mk_bool(B) -> B.

%% mk_number(JsNum) -> JsVal
%% Invert the number rows of `classify/1`: unwrap `JInt`/`JFloat` to a bare
%% BEAM number, and map the three non-finite `JsNum` variants to their §2.3
%% sentinel atoms (BEAM floats cannot represent NaN/±Inf). `JInt` is the
%% exact-integer shape of a double, so it never leaves here wider than
%% 2^53 - 1: `mk_int/1` rounds anything wider to the nearest double.
mk_number({j_int, N}) -> mk_int(N);
mk_number({j_float, F}) -> F;
mk_number(j_nan) -> js_nan;
mk_number(j_pos_inf) -> js_inf;
mk_number(j_neg_inf) -> js_neg_inf.

%% mk_int(Int) -> JsVal
%% An exact BEAM integer as a JS Number: |N| =< 2^53 - 1 stays a bare int;
%% wider rounds to nearest-even double (or ±Infinity) via the Gleam
%% `num_from_int`, since float/1 double-rounds very wide integers.
-define(MAX_SAFE_INT, 9007199254740991).
mk_int(N) when N > ?MAX_SAFE_INT; N < -?MAX_SAFE_INT ->
    mk_number('arc@rt@val':num_from_int(N));
mk_int(N) -> N.

%% mk_string(String) -> JsVal
%% Gleam `String` is already a UTF-8 binary — the string wire form (D10).
mk_string(S) -> S.

%% mk_bigint(Int) -> JsVal
%% Tag a BEAM integer as the bigint wire form `{js_bigint, N}`.
mk_bigint(N) -> {js_bigint, N}.

%% mk_symbol(SymbolId) -> JsVal
%% Tag a `rt_types.SymbolId` wire term as `{js_sym, S}`. Position 2 is
%% always the SymbolId sum's own wire form — the encoder does NOT flatten
%% well-known symbols to a bare atom (SPEC §2.3 symbol note).
mk_symbol(S) -> {js_sym, S}.

%% mk_object(Handle) -> JsVal
%% `Handle`'s wire form `{js_cell, N}` (R4) IS the object wire form — identity.
mk_object(H) -> H.

%% mk_tdz() -> JsVal
%% The TDZ sentinel atom. Never a JS value; every coercion on it is an engine
%% panic (SPEC §2.3 last row).
mk_tdz() -> js_tdz.

%% ── §6.1.6.1.20 Number::toString ──────────────────────────────────────────
%%
%% JS Number::toString(x, 10) from the double's shortest round-trip digits
%% (shortest_digits/1: Erlang's [short] / Ryu output), which is what the spec
%% asks for with "k is as small as possible". The Number.prototype
%% {toFixed,toExponential,toPrecision} formatters live in arc_rt_number_ffi.

%% JS Number::toString(x, 10) per ES2024 §6.1.6.1.20 for a finite x.
%% Operates on ℝ(x), so -0 stringifies unsigned as "0"; the zero case is
%% short-circuited here because js_positive_to_string/1 has no zero guard.
js_number_to_string(N) when is_float(N) ->
    case N == 0.0 of
        true -> <<"0">>;
        false when N < 0.0 -> <<"-", (js_positive_to_string(-N))/binary>>;
        false -> js_positive_to_string(N)
    end.

%% ---------------------------------------------------------------------------
%% Number::toString (radix 10)
%% ---------------------------------------------------------------------------

%% §6.1.6.1.20 steps 5-10 for a positive finite X, using its shortest
%% round-trip digits d1…dk and the leading digit's decimal exponent E
%% (the spec's e is E + 1).
js_positive_to_string(X) ->
    {Digits, E} = shortest_digits(X),
    K = length(Digits),
    if
        %% Step 6: k =< e =< 21 — integer notation, zero-padded to e digits.
        E >= K - 1, E =< 20 ->
            list_to_binary(Digits ++ lists:duplicate(E + 1 - K, $0));
        %% Step 7: 0 < e =< 21 — decimal point inside the digit string.
        E >= 0, E =< 20 ->
            {I, F} = lists:split(E + 1, Digits),
            list_to_binary(I ++ "." ++ F);
        %% Step 8: -6 < e =< 0 — leading "0." and -e zeros.
        E >= -6, E < 0 ->
            list_to_binary("0." ++ lists:duplicate(-E - 1, $0) ++ Digits);
        %% Steps 9-10: exponential notation.
        true ->
            format_exponential(Digits, E)
    end.

%% ---------------------------------------------------------------------------
%% Digit extraction
%% ---------------------------------------------------------------------------

%% "d.ddd…e±n" from a significant-digit list and the leading digit's decimal
%% exponent: JS style, so no trailing "." for a single digit, no exponent
%% zero-padding, and an explicit "+" for non-negative exponents.
format_exponential([D | Rest], E) ->
    Frac = case Rest of
        [] -> "";
        _ -> [$. | Rest]
    end,
    Sign = case E < 0 of
        true -> $-;
        false -> $+
    end,
    list_to_binary([D, Frac, $e, Sign, integer_to_list(abs(E))]).

%% Decompose positive X into the shortest digit string that round-trips it
%% (leading and trailing zeros removed) and the decimal exponent E of its
%% leading digit: X = d1.d2…dk × 10^E.
shortest_digits(X) ->
    {Mantissa, E0} = split_exponent(arc_rt_float_ffi:shortest(X)),
    [IntPart, FracPart] = string:split(Mantissa, "."),
    Combined = IntPart ++ FracPart,
    Lead = length(lists:takewhile(fun(C) -> C =:= $0 end, Combined)),
    Digits = string:trim(lists:nthtail(Lead, Combined), trailing, "0"),
    {Digits, length(IntPart) - 1 - Lead + E0}.

%% Split Erlang float text into its mantissa and integer exponent
%% ("1.5e-7" -> {"1.5", -7}); no exponent part means 0.
split_exponent(S) ->
    case string:split(S, "e") of
        [Mantissa, Exp] -> {Mantissa, list_to_integer(Exp)};
        [Mantissa] -> {Mantissa, 0}
    end.

%% ---------------------------------------------------------------------------
%% IEEE-754 identity primitives (SameValue support)
%% ---------------------------------------------------------------------------

%% is_neg_zero(Float) -> Bool
%% True iff X is IEEE-754 negative zero. BEAM has no math:copysign/2, so read
%% the sign bit directly: -0.0 is exactly <<1:1, 0:63>>.
is_neg_zero(X) when is_float(X) ->
    case <<X/float>> of
        <<1:1, 0:63>> -> true;
        _ -> false
    end.

%% float_same_term(Float, Float) -> Bool
%% ES2024 §7.2.11 SameValue's number arm distinguishes +0 from -0. Erlang's
%% `=:=` on floats compares the underlying term (bit pattern), so +0.0 =:= -0.0
%% is false — exactly the semantics needed. (BEAM floats never carry NaN.)
float_same_term(A, B) -> A =:= B.

%% ---------------------------------------------------------------------------
%% §7.1.4.1.1 StringToNumber float parsing (parse_float)
%% ---------------------------------------------------------------------------

%% Convert a decimal float/exponent literal to a double.
%% Returns {ok, Float};
%%         {error, out_of_range} when the text is valid float syntax but its
%%             magnitude overflows an IEEE double (binary_to_float raises
%%             badarg for overflow; underflow rounds to 0.0 and succeeds);
%%         {error, invalid} for text binary_to_float cannot parse at all.
%% The tags mirror rt_val.gleam's `Result(Float, FloatParseError)`.
%%
%% A JS decimal literal is not quite what erlang:binary_to_float/1 accepts, so
%% the text is normalized ONCE up front — here, and nowhere else: the caller
%% hands over the literal verbatim. Both binary_to_float and the out-of-range
%% classifier see the same normalized text — the classifier's "does this look
%% like a float?" question is only meaningful about the string binary_to_float
%% actually rejected. (When they disagreed, ".5" and "1e400" classified as
%% `invalid` rather than parsing / overflowing.)
parse_float(S) ->
    Norm = normalize(S),
    case try_binary_to_float(Norm) of
        {ok, F} -> {ok, F};
        %% binary_to_float raised badarg. If the text is nonetheless
        %% well-formed float syntax, the only remaining cause is a magnitude
        %% outside the double range — a valid JS literal (e.g. "1e400") the
        %% caller must not zero out.
        error ->
            case is_float_syntax(Norm) of
                true -> {error, out_of_range};
                false -> {error, invalid}
            end
    end.

try_binary_to_float(S) ->
    try
        {ok, erlang:binary_to_float(S)}
    catch
        error:badarg -> error
    end.

%% Pad a JS decimal literal into the shape binary_to_float accepts:
%% [+-]?Digits "." Digits ([eE][+-]?Digits)?. JS lets the mantissa omit the
%% integer part (".5"), the fraction ("1.", "1.e3") or the dot itself
%% ("1e10"); Erlang requires a dot with a digit on each side. Anything else is
%% left alone for is_float_syntax/1 to reject.
normalize(S) ->
    {Mantissa, Exponent} = split_exponent_bin(S),
    {Sign, Digits} = take_sign(Mantissa),
    <<Sign/binary, (pad_mantissa(Digits))/binary, Exponent/binary>>.

%% Split off the exponent at the first e/E, keeping the marker with it
%% ("1.e3" -> {<<"1.">>, <<"e3">>}); the exponent is <<>> when absent.
%% Binary sibling of split_exponent/1 (which operates on charlists above).
split_exponent_bin(S) ->
    case binary:match(S, [<<"e">>, <<"E">>]) of
        {Pos, _Len} ->
            <<Mantissa:Pos/binary, Exponent/binary>> = S,
            {Mantissa, Exponent};
        nomatch ->
            {S, <<>>}
    end.

take_sign(<<C, Rest/binary>>) when C =:= $+; C =:= $- -> {<<C>>, Rest};
take_sign(S) -> {<<>>, S}.

pad_mantissa(<<>>) ->
    <<>>;
pad_mantissa(<<".", _/binary>> = M) ->
    pad_mantissa(<<"0", M/binary>>);
pad_mantissa(M) ->
    case binary:match(M, <<".">>) of
        nomatch -> <<M/binary, ".0">>;
        _ ->
            case binary:last(M) of
                $. -> <<M/binary, "0">>;
                _ -> M
            end
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
