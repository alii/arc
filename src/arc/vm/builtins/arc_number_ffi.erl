-module(arc_number_ffi).
-export([format_to_fixed/2, format_to_exponential/2, format_to_precision/2]).

%% Number.prototype.toFixed(fractionDigits)
%% Uses Erlang's float_to_list with decimals option for formatting.
format_to_fixed(X, Digits) ->
    %% Handle negative zero
    case X == 0.0 andalso is_neg_zero(X) of
        true ->
            S = format_to_fixed_pos(0.0, Digits),
            <<"-", (list_to_binary(S))/binary>>;
        false when X < 0.0 ->
            S = format_to_fixed_pos(-X, Digits),
            <<"-", (list_to_binary(S))/binary>>;
        false ->
            list_to_binary(format_to_fixed_pos(X, Digits))
    end.

format_to_fixed_pos(X, Digits) ->
    %% For very large numbers (>= 1e21), JS returns exponential notation
    case X >= 1.0e21 of
        true ->
            %% Use JS-style exponential format
            float_to_list(X, [{scientific, 20}]);
        false ->
            float_to_list(X, [{decimals, Digits}])
    end.

%% Number.prototype.toExponential(fractionDigits)
%% FractionDigits = -1 means "auto" (no fractionDigits argument given).
format_to_exponential(X, FractionDigits) ->
    case X == 0.0 andalso is_neg_zero(X) of
        true ->
            S = format_exp_pos(0.0, FractionDigits),
            <<"-", S/binary>>;
        false when X < 0.0 ->
            S = format_exp_pos(-X, FractionDigits),
            <<"-", S/binary>>;
        false ->
            format_exp_pos(X, FractionDigits)
    end.

format_exp_pos(X, FractionDigits) ->
    %% Erlang's scientific notation: "1.23000e+02"
    %% JS wants: "1.23e+2"
    Digits = case FractionDigits of
        -1 ->
            %% Auto: use enough digits to represent uniquely
            %% We'll use 20 and then strip trailing zeros
            20;
        D -> D
    end,
    S = float_to_list(X, [{scientific, Digits}]),
    Bin = list_to_binary(S),
    %% Parse and reformat to JS style
    format_js_exponential(Bin, FractionDigits =:= -1).

format_js_exponential(Bin, StripTrailing) ->
    %% Input like "1.23000e+02" or "0.00000e+00"
    case binary:split(Bin, <<"e">>) of
        [Mantissa, ExpPart] ->
            M2 = case StripTrailing of
                true -> strip_trailing_zeros(Mantissa);
                false -> Mantissa
            end,
            %% Fix exponent: "+02" -> "+2", "+00" -> "+0"
            Exp = fix_exponent(ExpPart),
            <<M2/binary, "e", Exp/binary>>;
        _ ->
            Bin
    end.

strip_trailing_zeros(Bin) ->
    case binary:match(Bin, <<".">>) of
        nomatch -> Bin;
        _ ->
            S = binary_to_list(Bin),
            R = lists:reverse(S),
            R2 = strip_zeros(R),
            list_to_binary(lists:reverse(R2))
    end.

strip_zeros([$0 | Rest]) -> strip_zeros(Rest);
strip_zeros([$. | Rest]) -> [$. | Rest]; %% keep at least "X." -> will become "X"
strip_zeros(Other) -> Other.

fix_exponent(ExpPart) ->
    %% "+02" -> "+2", "-02" -> "-2", "+00" -> "+0"
    case ExpPart of
        <<Sign, Rest/binary>> when Sign =:= $+; Sign =:= $- ->
            N = binary_to_integer(Rest),
            <<Sign, (integer_to_binary(N))/binary>>;
        _ ->
            ExpPart
    end.

%% Number.prototype.toPrecision(precision)
format_to_precision(X, Precision) ->
    case X == 0.0 andalso is_neg_zero(X) of
        true ->
            S = format_prec_pos(0.0, Precision),
            <<"-", S/binary>>;
        false when X < 0.0 ->
            S = format_prec_pos(-X, Precision),
            <<"-", S/binary>>;
        false ->
            format_prec_pos(X, Precision)
    end.

format_prec_pos(X, Precision) ->
    %% Use Erlang's float_to_list with significant digits
    %% Then decide between fixed and exponential notation
    case X == 0.0 of
        true ->
            case Precision of
                1 -> <<"0">>;
                _ ->
                    Zeros = list_to_binary(lists:duplicate(Precision - 1, $0)),
                    <<"0.", Zeros/binary>>
            end;
        false ->
            %% Get the exponent (base 10)
            E = floor(math:log10(X)),
            case E >= 0 andalso E < Precision of
                true ->
                    %% Use fixed notation
                    DecimalDigits = Precision - E - 1,
                    S = decimals_exact(X, DecimalDigits),
                    list_to_binary(S);
                false when E < 0 andalso E >= -(4) ->
                    %% Small numbers: use fixed notation
                    DecimalDigits = Precision - E - 1,
                    S = decimals_exact(X, DecimalDigits),
                    list_to_binary(S);
                false ->
                    %% Use exponential notation
                    FracDigits = Precision - 1,
                    format_to_exponential(X, FracDigits)
            end
    end.

is_neg_zero(X) ->
    <<Sign:1, _:63>> = <<X/float>>,
    Sign =:= 1.

%% float_to_list with {decimals, D} rounds from the float's shortest decimal
%% representation, double-rounding values like 1.3548387096774195 at 15
%% decimals ("…420" instead of the correct "…419"). Format with 30 guard
%% digits of the exact expansion and round once, half away from zero
%% (matching the spec's "pick the larger n").
decimals_exact(X, D) ->
    Wide = float_to_list(X, [{decimals, min(253, D + 30)}]),
    [IntPart, Frac] = string:split(Wide, "."),
    Keep = lists:sublist(Frac, D),
    Rest = lists:nthtail(D, Frac),
    RoundUp = case Rest of [C | _] when C >= $5 -> true; _ -> false end,
    Num0 = list_to_integer(IntPart ++ Keep),
    Num = case RoundUp of true -> Num0 + 1; false -> Num0 end,
    S = integer_to_list(Num),
    case D of
        0 -> S;
        _ ->
            Padded = lists:duplicate(max(0, D + 1 - length(S)), $0) ++ S,
            {I2, F2} = lists:split(length(Padded) - D, Padded),
            I2 ++ "." ++ F2
    end.
