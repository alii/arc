%% the number a property name is filed under: 58 bits worked out from its bytes
-module(arc_bytecode_key_ffi).
-export([hash/1, named/1, private/1]).

%% as key.named and key.private
named(Bin) -> {named, hash(Bin) * 2, Bin}.

private(Bin) -> {private, hash(Bin) * 2 + 1, Bin}.

-define(MASK, 16#1FFFFFFF).

%% two 29-bit lanes, three bytes a step. Each step multiplies by an odd
%% constant and folds the top bits back down, since a multiply only carries
%% upwards. Every product stays under 2^59, so nothing here is ever a bignum
hash(Bin) -> hash(Bin, 16#11C9DC5 + byte_size(Bin), 16#1747B28C).

hash(<<W:24, Rest/binary>>, A, B) -> hash(Rest, mix_a(A bxor W), mix_b(B bxor W));
hash(<<W:16>>, A, B) -> done(mix_a(A bxor W), mix_b(B bxor W));
hash(<<W:8>>, A, B) -> done(mix_a(A bxor W), mix_b(B bxor W));
hash(<<>>, A, B) -> done(A, B).

-compile({inline, [mix_a/1, mix_b/1, done/2]}).
mix_a(X) ->
    H = (X * 16#1B873593) band ?MASK,
    H bxor (H bsr 15).

mix_b(X) ->
    H = (X * 16#0C2B2AE5) band ?MASK,
    H bxor (H bsr 13).

done(A, B) -> (mix_a(A) bsl 29) bor mix_b(B).
