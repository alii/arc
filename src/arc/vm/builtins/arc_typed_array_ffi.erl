-module(arc_typed_array_ffi).
-export([ta_zeroed/1, ta_get_int/4, ta_set_int/4, ta_get_float/3,
         ta_set_float/5, ta_clamp_uint8/2, ta_splice/3, ta_fill_region/4]).

%% Allocate an all-zero binary of N bytes (ArrayBuffer backing store).
ta_zeroed(N) when N =< 0 -> <<>>;
ta_zeroed(N) -> binary:copy(<<0>>, N).

%% Replace byte_size(Region) bytes of Bin at Off with Region in ONE rebuild —
%% the single-pass primitive for bulk typed-array writes (fill/set/slice).
%% Caller guarantees Off + byte_size(Region) =< byte_size(Bin).
ta_splice(Bin, Off, Region) ->
    Len = byte_size(Region),
    <<Before:Off/binary, _:Len/binary, After/bits>> = Bin,
    <<Before/binary, Region/binary, After/bits>>.

%% Write Count copies of the encoded element ElemBin at byte offset Off.
%% O(byte_size(Bin) + Count*elem) — one rebuild for the whole fill.
ta_fill_region(Bin, _Off, Count, _ElemBin) when Count =< 0 -> Bin;
ta_fill_region(Bin, Off, Count, ElemBin) ->
    ta_splice(Bin, Off, binary:copy(ElemBin, Count)).

%% Read a little-endian integer element. SizeBits in {8,16,32,64}.
ta_get_int(Bin, Off, SizeBits, true) ->
    <<_:Off/binary, V:SizeBits/little-signed, _/bits>> = Bin,
    V;
ta_get_int(Bin, Off, SizeBits, false) ->
    <<_:Off/binary, V:SizeBits/little-unsigned, _/bits>> = Bin,
    V.

%% Write a little-endian integer element. Erlang truncates V mod 2^SizeBits
%% when encoding, which is exactly the ToInt8/ToUint32/... wrap semantics.
ta_set_int(Bin, Off, SizeBits, V) ->
    SizeBytes = SizeBits div 8,
    <<Before:Off/binary, _:SizeBytes/binary, After/bits>> = Bin,
    <<Before/binary, V:SizeBits/little, After/bits>>.

%% Read a float element. Returns {Tag, Float} with
%% Tag: 0 = finite, 1 = NaN, 2 = +Infinity, 3 = -Infinity.
%% NaN/Inf bit patterns cannot be decoded by an Erlang float segment, so the
%% exponent is inspected on the raw bits first.
ta_get_float(Bin, Off, 32) ->
    <<_:Off/binary, B:32/little, _/bits>> = Bin,
    case <<B:32>> of
        <<0:1, 16#FF:8, 0:23>> -> {2, +0.0};
        <<1:1, 16#FF:8, 0:23>> -> {3, +0.0};
        <<_:1, 16#FF:8, _:23>> -> {1, +0.0};
        <<F:32/float>> -> {0, F}
    end;
ta_get_float(Bin, Off, 64) ->
    <<_:Off/binary, B:64/little, _/bits>> = Bin,
    case <<B:64>> of
        <<0:1, 16#7FF:11, 0:52>> -> {2, +0.0};
        <<1:1, 16#7FF:11, 0:52>> -> {3, +0.0};
        <<_:1, 16#7FF:11, _:52>> -> {1, +0.0};
        <<F:64/float>> -> {0, F}
    end.

%% Write a float element. Tag as in ta_get_float. Finite values that overflow
%% the 32-bit range round to the correctly-signed infinity (IEEE 754
%% round-to-nearest), matching Float32Array store semantics.
ta_set_float(Bin, Off, 32, Tag, V) ->
    ta_set_int(Bin, Off, 32, f32_bits(Tag, V));
ta_set_float(Bin, Off, 64, Tag, V) ->
    ta_set_int(Bin, Off, 64, f64_bits(Tag, V)).

f32_bits(1, _) -> 16#7FC00000;
f32_bits(2, _) -> 16#7F800000;
f32_bits(3, _) -> 16#FF800000;
f32_bits(0, V) ->
    try
        <<B:32>> = <<V:32/float>>,
        B
    catch
        error:badarg ->
            case V < 0.0 of
                true -> 16#FF800000;
                false -> 16#7F800000
            end
    end.

f64_bits(1, _) -> 16#7FF8000000000000;
f64_bits(2, _) -> 16#7FF0000000000000;
f64_bits(3, _) -> 16#FFF0000000000000;
f64_bits(0, V) ->
    <<B:64>> = <<V:64/float>>,
    B.

%% ES2024 §7.1.12 ToUint8Clamp: clamp to [0,255] with round-half-to-EVEN.
%% Tag as in ta_get_float (NaN -> 0, +inf -> 255, -inf -> 0).
ta_clamp_uint8(1, _) -> 0;
ta_clamp_uint8(2, _) -> 255;
ta_clamp_uint8(3, _) -> 0;
ta_clamp_uint8(0, V) when V =< 0.0 -> 0;
ta_clamp_uint8(0, V) when V >= 255.0 -> 255;
ta_clamp_uint8(0, V) ->
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
