%% Byte-offset UTF-8 utilities, shared by everything that indexes into a
%% source or subject binary by BYTE offset rather than by grapheme:
%% arc_parser_ffi (the lexer's source binary) and arc_regexp_ffi (re:run's
%% byte-index captures). Both used to carry their own copy of these three
%% functions, with different out-of-range behaviour.
%%
%% OUT-OF-RANGE POLICY (the one policy, for all callers): every offset and
%% length is CLAMPED into [0, byte_size(Bin)] and never raises. Callers pass
%% offsets they have already established are char boundaries inside the
%% binary, so clamping is unreachable in practice; it exists so that a caller
%% bug degrades to a short/empty slice instead of a badarg from binary:part/3
%% deep inside a hot loop. Nothing here validates UTF-8 — hence the `unsafe_`
%% prefix on the slice — because both callers slice out of binaries that came
%% from an already-valid Gleam String at boundaries they computed themselves.
-module(arc_bytes_ffi).
-export([unsafe_slice/3, drop_start/2, next_char_boundary/2]).

%% O(1) sub-binary [Start, Start+Len), clamped. Returned as a Gleam String
%% WITHOUT re-validating UTF-8 (bit_array:to_string's is_utf8 walk is pure
%% overhead when the offsets are known char boundaries).
unsafe_slice(Bin, Start, Len) ->
    Size = byte_size(Bin),
    S = min(max(Start, 0), Size),
    L = min(max(Len, 0), Size - S),
    binary:part(Bin, S, L).

%% O(1) suffix from byte offset Start, clamped.
drop_start(Bin, Start) ->
    Size = byte_size(Bin),
    S = min(max(Start, 0), Size),
    binary:part(Bin, S, Size - S).

%% Smallest UTF-8 character boundary strictly greater than Pos. Skips
%% continuation bytes (2#10xxxxxx). re:run raises badarg for an offset in
%% the middle of a multibyte character, so byte-offset loops that step
%% forward (AdvanceStringIndex, ES2026 22.2.7.3) must use this instead of
%% Pos + 1. May return a value past byte_size(Bin) (e.g. Pos at the last
%% character), which callers use as their loop-termination signal.
next_char_boundary(Bin, Pos) ->
    next_boundary(Bin, Pos + 1, byte_size(Bin)).

next_boundary(_Bin, P, Size) when P >= Size -> P;
next_boundary(Bin, P, Size) ->
    case binary:at(Bin, P) band 16#C0 of
        16#80 -> next_boundary(Bin, P + 1, Size);
        _ -> P
    end.
