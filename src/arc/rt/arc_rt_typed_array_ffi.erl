-module(arc_rt_typed_array_ffi).
-export([ta_zeroed/1, ta_get_int/3, ta_set_int/4, ta_get_float/3,
         ta_set_float/4, ta_clamp_uint8/1, ta_splice/3,
         f32_bits/1, f64_bits/1, decode_f32_bits/1, decode_f64_bits/1]).

ta_zeroed(N) when N =< 0 -> <<>>;
ta_zeroed(N) -> binary:copy(<<0>>, N).

%% caller guarantees off + size(region) =< size(bin)
ta_splice(Bin, Off, Region) ->
    Len = byte_size(Region),
    <<Before:Off/binary, _:Len/binary, After/bits>> = Bin,
    <<Before/binary, Region/binary, After/bits>>.

ta_get_int(Bin, Off, i8)  -> get_int(Bin, Off, 8, signed);
ta_get_int(Bin, Off, u8)  -> get_int(Bin, Off, 8, unsigned);
ta_get_int(Bin, Off, i16) -> get_int(Bin, Off, 16, signed);
ta_get_int(Bin, Off, u16) -> get_int(Bin, Off, 16, unsigned);
ta_get_int(Bin, Off, i32) -> get_int(Bin, Off, 32, signed);
ta_get_int(Bin, Off, u32) -> get_int(Bin, Off, 32, unsigned);
ta_get_int(Bin, Off, i64) -> get_int(Bin, Off, 64, signed);
ta_get_int(Bin, Off, u64) -> get_int(Bin, Off, 64, unsigned).

get_int(Bin, Off, SizeBits, signed) ->
    <<_:Off/binary, V:SizeBits/little-signed, _/bits>> = Bin,
    V;
get_int(Bin, Off, SizeBits, unsigned) ->
    <<_:Off/binary, V:SizeBits/little-unsigned, _/bits>> = Bin,
    V.

%% encoding truncates mod 2^bits, matching toint wrap
ta_set_int(Bin, Off, i8, V)  -> set_int(Bin, Off, 8, V);
ta_set_int(Bin, Off, u8, V)  -> set_int(Bin, Off, 8, V);
ta_set_int(Bin, Off, i16, V) -> set_int(Bin, Off, 16, V);
ta_set_int(Bin, Off, u16, V) -> set_int(Bin, Off, 16, V);
ta_set_int(Bin, Off, i32, V) -> set_int(Bin, Off, 32, V);
ta_set_int(Bin, Off, u32, V) -> set_int(Bin, Off, 32, V);
ta_set_int(Bin, Off, i64, V) -> set_int(Bin, Off, 64, V);
ta_set_int(Bin, Off, u64, V) -> set_int(Bin, Off, 64, V).

set_int(Bin, Off, SizeBits, V) ->
    SizeBytes = SizeBits div 8,
    <<Before:Off/binary, _:SizeBytes/binary, After/bits>> = Bin,
    <<Before/binary, V:SizeBits/little, After/bits>>.

ta_get_float(Bin, Off, f32) ->
    <<_:Off/binary, B:32/little, _/bits>> = Bin,
    decode_f32_bits(B);
ta_get_float(Bin, Off, f64) ->
    <<_:Off/binary, B:64/little, _/bits>> = Bin,
    decode_f64_bits(B).

%% nan/inf bits cannot decode as an erlang float, match them first
decode_f32_bits(B) ->
    case <<B:32>> of
        <<0:1, 16#FF:8, 0:23>> -> j_pos_inf;
        <<1:1, 16#FF:8, 0:23>> -> j_neg_inf;
        <<_:1, 16#FF:8, _:23>> -> j_nan;
        <<F:32/float>> -> {j_float, F}
    end.

decode_f64_bits(B) ->
    case <<B:64>> of
        <<0:1, 16#7FF:11, 0:52>> -> j_pos_inf;
        <<1:1, 16#7FF:11, 0:52>> -> j_neg_inf;
        <<_:1, 16#7FF:11, _:52>> -> j_nan;
        <<F:64/float>> -> {j_float, F}
    end.

ta_set_float(Bin, Off, f32, N) ->
    set_int(Bin, Off, 32, f32_bits(N));
ta_set_float(Bin, Off, f64, N) ->
    set_int(Bin, Off, 64, f64_bits(N)).

as_float({j_int, I}) ->
    try {j_float, float(I)}
    catch error:badarith ->
        case I > 0 of true -> j_pos_inf; false -> j_neg_inf end
    end;
as_float(N) -> N.

f32_bits(N) ->
    case as_float(N) of
        j_nan -> 16#7FC00000;
        j_pos_inf -> 16#7F800000;
        j_neg_inf -> 16#FF800000;
        {j_float, V} -> <<B:32>> = <<V:32/float>>, B
    end.

f64_bits(N) ->
    case as_float(N) of
        j_nan -> 16#7FF8000000000000;
        j_pos_inf -> 16#7FF0000000000000;
        j_neg_inf -> 16#FFF0000000000000;
        {j_float, V} -> <<B:64>> = <<V:64/float>>, B
    end.

%% §7.1.12 touint8clamp, round half to even
ta_clamp_uint8(j_nan) -> 0;
ta_clamp_uint8(j_pos_inf) -> 255;
ta_clamp_uint8(j_neg_inf) -> 0;
ta_clamp_uint8({j_int, I}) when I =< 0 -> 0;
ta_clamp_uint8({j_int, I}) when I >= 255 -> 255;
ta_clamp_uint8({j_int, I}) -> I;
ta_clamp_uint8({j_float, V}) when V =< 0.0 -> 0;
ta_clamp_uint8({j_float, V}) when V >= 255.0 -> 255;
ta_clamp_uint8({j_float, V}) ->
    F = trunc(V),
    Frac = V - F,
    if
        Frac < 0.5 -> F;
        Frac > 0.5 -> F + 1;
        true ->
            case F rem 2 of
                0 -> F;
                _ -> F + 1
            end
    end.
