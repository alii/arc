%% Typed-array element codecs on the ArrayBuffer backing binary.
%%
%% The ONLY Gleam bindings to this module live in
%% arc/vm/internal/typed_array_ffi.gleam — declare new exports there, not in
%% the modules that call them.
%%
%% Float elements speak the Gleam `arc/vm/value.JsNum` runtime term directly:
%%
%%     {finite, Float} | na_n | infinity | neg_infinity
%%
%% (same technique as arc_math_ffi). BEAM floats cannot represent NaN/±Inf,
%% so the non-finite cases MUST cross the FFI boundary as their own
%% constructors — and doing it with the real JsNum shape means the Gleam
%% side is forced by the type checker to handle them, with no hand-kept
%% integer-tag table to drift out of sync on either side.
%% Integer/float element widths arrive as the Gleam `IntElem` / `FloatElem`
%% atoms (i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64, f32 | f64) — one clause
%% per element the codecs actually implement. A shape with no clause is a
%% `function_clause` crash, never a wrong-width read of an adjacent element.
-module(arc_typed_array_ffi).
-export([ta_zeroed/1, ta_get_int/3, ta_set_int/4, ta_get_float/3,
         ta_set_float/4, ta_clamp_uint8/1, ta_splice/3, ta_fill_region/4]).

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

%% Read a little-endian integer element, one clause per IntElem.
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

%% Write a little-endian integer element, one clause per IntElem. Erlang
%% truncates V mod 2^SizeBits when encoding, which is exactly the
%% ToInt8/ToUint32/... wrap semantics — signedness is irrelevant on write.
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

%% Read a float element as a `value.JsNum`. NaN/Inf bit patterns cannot be
%% decoded by an Erlang float segment, so the exponent is inspected on the
%% raw bits first.
ta_get_float(Bin, Off, f32) ->
    <<_:Off/binary, B:32/little, _/bits>> = Bin,
    case <<B:32>> of
        <<0:1, 16#FF:8, 0:23>> -> infinity;
        <<1:1, 16#FF:8, 0:23>> -> neg_infinity;
        <<_:1, 16#FF:8, _:23>> -> na_n;
        <<F:32/float>> -> {finite, F}
    end;
ta_get_float(Bin, Off, f64) ->
    <<_:Off/binary, B:64/little, _/bits>> = Bin,
    case <<B:64>> of
        <<0:1, 16#7FF:11, 0:52>> -> infinity;
        <<1:1, 16#7FF:11, 0:52>> -> neg_infinity;
        <<_:1, 16#7FF:11, _:52>> -> na_n;
        <<F:64/float>> -> {finite, F}
    end.

%% Write a float element given as a `value.JsNum`. Finite values that
%% overflow the 32-bit range round to the correctly-signed infinity
%% (IEEE 754 round-to-nearest), matching Float32Array store semantics.
ta_set_float(Bin, Off, f32, N) ->
    set_int(Bin, Off, 32, f32_bits(N));
ta_set_float(Bin, Off, f64, N) ->
    set_int(Bin, Off, 64, f64_bits(N)).

%% The ENCODE `<<V:32/float>>` never fails: a finite float64 whose magnitude
%% exceeds the float32 range is rounded to ±infinity's bit pattern by the
%% BEAM's native IEEE 754 round-to-nearest (it is only the DECODE of such
%% bits that raises). arc_math_ffi:fround/1 and data_view's float32 stores
%% already rely on exactly this, so there is no badarg to catch here.
f32_bits(na_n) -> 16#7FC00000;
f32_bits(infinity) -> 16#7F800000;
f32_bits(neg_infinity) -> 16#FF800000;
f32_bits({finite, V}) ->
    <<B:32>> = <<V:32/float>>,
    B.

f64_bits(na_n) -> 16#7FF8000000000000;
f64_bits(infinity) -> 16#7FF0000000000000;
f64_bits(neg_infinity) -> 16#FFF0000000000000;
f64_bits({finite, V}) ->
    <<B:64>> = <<V:64/float>>,
    B.

%% ES2024 §7.1.12 ToUint8Clamp on a `value.JsNum`: clamp to [0,255] with
%% round-half-to-EVEN. NaN -> 0, +Infinity -> 255, -Infinity -> 0.
ta_clamp_uint8(na_n) -> 0;
ta_clamp_uint8(infinity) -> 255;
ta_clamp_uint8(neg_infinity) -> 0;
ta_clamp_uint8({finite, V}) when V =< 0.0 -> 0;
ta_clamp_uint8({finite, V}) when V >= 255.0 -> 255;
ta_clamp_uint8({finite, V}) ->
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
