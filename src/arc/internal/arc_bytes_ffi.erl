%% offsets are clamped, never raise; utf-8 not validated
-module(arc_bytes_ffi).
-export([unsafe_slice/3, drop_start/2, next_char_boundary/2]).

unsafe_slice(Bin, Start, Len) ->
    Size = byte_size(Bin),
    S = min(max(Start, 0), Size),
    L = min(max(Len, 0), Size - S),
    binary:part(Bin, S, L).

drop_start(Bin, Start) ->
    Size = byte_size(Bin),
    S = min(max(Start, 0), Size),
    binary:part(Bin, S, Size - S).

%% may return past byte_size, callers stop on that
next_char_boundary(Bin, Pos) ->
    next_boundary(Bin, max(Pos + 1, 0), byte_size(Bin)).

next_boundary(_Bin, P, Size) when P >= Size -> P;
next_boundary(Bin, P, Size) ->
    case binary:at(Bin, P) band 16#C0 of
        16#80 -> next_boundary(Bin, P + 1, Size);
        _ -> P
    end.
