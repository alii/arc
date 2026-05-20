-module(arc_math_ffi).
-export([fround/1, is_neg_zero/1, js_number_to_string/1]).

%% Math.fround: round a 64-bit float to the nearest 32-bit (IEEE 754
%% single-precision) float, then widen back to 64-bit. This matches the
%% ES2024 §20.2.2.17 spec behavior.
%%
%% We encode the float as a 32-bit big-endian binary and decode it back.
%% This is the standard Erlang trick for single-precision rounding.
fround(X) when is_float(X) ->
    <<F32:32/float>> = <<X:32/float>>,
    F32;
fround(X) when is_integer(X) ->
    fround(float(X)).

%% Detect IEEE 754 negative zero. BEAM floats preserve the sign bit but
%% Erlang's == and =:= don't reliably distinguish ±0 in all contexts.
%% We check the sign bit of the IEEE 754 binary representation directly.
is_neg_zero(X) when is_float(X) ->
    <<Sign:1, _:63>> = <<X:64/float>>,
    X == 0.0 andalso Sign =:= 1;
is_neg_zero(_) ->
    false.

%% JS Number.prototype.toString(10) per ES2024 §6.1.6.1.20.
%% Algorithm (informally):
%%   - 0 → "0" (also +0, -0)
%%   - integers in (-1e21, 1e21) → integer notation
%%   - floats with |x| in [1e-6, 1e21) → decimal notation
%%   - otherwise → exponential notation "Xe+N" / "Xe-N"
%% The mantissa has no trailing ".0", and positive exponents include "+".
js_number_to_string(N) when is_float(N) ->
    %% Normalize -0 to 0 (per spec, -0 stringifies as "0").
    Norm = N + 0.0,
    case Norm == 0.0 of
        true -> <<"0">>;
        false ->
            AbsN = abs(Norm),
            Trunc = trunc(Norm),
            IsInt = float(Trunc) == Norm,
            case IsInt andalso AbsN < 1.0e21 of
                true ->
                    list_to_binary(integer_to_list(Trunc));
                false ->
                    case AbsN >= 1.0e21 orelse AbsN < 1.0e-6 of
                        true -> format_js_exponential(Norm);
                        false -> format_js_decimal(Norm)
                    end
            end
    end;
js_number_to_string(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N)).

%% Format using Erlang short form, then convert to JS exponential format.
%% Erlang: "1.0e21" or "1.5e-7"
%% JS:     "1e+21" or "1.5e-7"
format_js_exponential(N) ->
    Short = erlang:float_to_list(N, [short]),
    case string:split(Short, "e") of
        [Mantissa, Exp] ->
            CleanMantissa = strip_trailing_zero_dot(Mantissa),
            CleanExp = case Exp of
                [$- | _] -> Exp;
                [$+ | _] -> Exp;
                _ -> [$+ | Exp]
            end,
            list_to_binary([CleanMantissa, $e, CleanExp]);
        _ ->
            list_to_binary(Short)
    end.

%% Format with decimals notation, trimming trailing zeros.
%% Use a wide format then strip.
format_js_decimal(N) ->
    %% Use erlang's short form, which for in-range numbers gives decimal output.
    Short = erlang:float_to_list(N, [short]),
    case string:split(Short, "e") of
        [Mantissa, Exp] ->
            %% Erlang gave us exponential but we want decimal — convert.
            ExpInt = list_to_integer(Exp),
            decimal_from_exp(Mantissa, ExpInt);
        _ ->
            list_to_binary(strip_trailing_zero_dot(Short))
    end.

%% Convert "1.0" + exponent into decimal string.
decimal_from_exp(Mantissa, Exp) ->
    %% Split mantissa around the decimal point
    {IntPart, FracPart} = case string:split(Mantissa, ".") of
        [I, F] -> {I, F};
        [I] -> {I, ""}
    end,
    %% Handle negative
    {Sign, IntPartClean} = case IntPart of
        [$- | Rest] -> {"-", Rest};
        _ -> {"", IntPart}
    end,
    Combined = IntPartClean ++ FracPart,
    DotPos = length(IntPartClean) + Exp,
    Result =
        if
            DotPos =< 0 ->
                "0." ++ lists:duplicate(-DotPos, $0) ++ Combined;
            DotPos >= length(Combined) ->
                Combined ++ lists:duplicate(DotPos - length(Combined), $0);
            true ->
                {Before, After} = lists:split(DotPos, Combined),
                Before ++ "." ++ After
        end,
    Trimmed = strip_trailing_zero_dot(Result),
    list_to_binary(Sign ++ Trimmed).

%% Strip trailing ".0" or trailing zeros after decimal.
%% "1.0" → "1"; "1.500" → "1.5"; "100" → "100"
strip_trailing_zero_dot(S) ->
    case string:find(S, ".") of
        nomatch -> S;
        _ ->
            Stripped = string:trim(S, trailing, "0"),
            case string:trim(Stripped, trailing, ".") of
                "" -> "0";
                Result -> Result
            end
    end.
