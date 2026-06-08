%% SharedArrayBuffer storage backed by the OTP `atomics` module.
%%
%% Cell mapping
%% ------------
%% One UNSIGNED 64-bit atomics cell per 8 bytes of buffer. Buffer byte K
%% lives in cell (K div 8) + 1 (atomics arrays are 1-indexed), at
%% little-endian byte position (K rem 8) within the cell — i.e. a cell
%% holding bytes B0..B7 has the integer value
%%   B0 + (B1 bsl 8) + ... + (B7 bsl 56)
%% so <<CellValue:64/little>> reproduces the byte run exactly.
%%
%% Concurrency
%% -----------
%% Atomics refs pass between BEAM processes by reference: every process
%% sharing the ref operates on the SAME mutable cells, which is what gives
%% SharedArrayBuffer its cross-agent visibility. Whole-cell writes use
%% atomics:put/3 (atomic per cell). Sub-word writes (a run that covers only
%% part of a cell) use a compare_exchange RETRY LOOP: read the cell, splice
%% the new bytes into the 64-bit image, CAS it back, retry on conflict —
%% concurrent writers of OTHER bytes within the same cell are never
%% clobbered. Reads of a whole cell are atomic; multi-cell reads are not a
%% single atomic snapshot (neither is the spec's shared memory model
%% required to provide one for non-atomic accesses).
%%
%% Growable SABs
%% -------------
%% Growable SharedArrayBuffers PRE-ALLOCATE max_byte_length worth of cells
%% up front (atomics cells are zero-initialized by the VM, matching
%% CreateSharedByteDataBlock). The buffer's CURRENT byte length lives in the
%% Gleam heap slot (value.BufShared byte_length); grow() only bumps that
%% number. Documented limitation: a grow performed in one agent is not
%% observed by agents that already hold the buffer — the 25 target test262
%% tests do not require cross-agent grow visibility.
%% Atomic element RMW
%% -------------------
%% Atomics read-modify-write ops (add/sub/and/or/xor/exchange) and
%% compareExchange must be a SINGLE atomic step across agent processes
%% (ES2024 §25.4.3.12) — a read-compute-write done in Gleam over a byte
%% snapshot loses updates under contention. rmw_element/cas_element run the
%% whole cycle here against the containing 64-bit cell: read the cell,
%% extract the element (TypedArray alignment guarantees a 1/2/4/8-byte
%% element never straddles a cell), compute, splice, and CAS the cell back
%% against the exact value the element was read from — retrying the WHOLE
%% read-compute-write on conflict. The witnessed old element value is
%% returned.
-module(arc_sab_ffi).

-export([new/1, read_bytes/3, write_bytes/3, rmw_element/5, cas_element/6]).

%% Allocate zero-filled shared storage able to hold MaxByteLength bytes.
new(MaxByteLength) when is_integer(MaxByteLength), MaxByteLength >= 0 ->
    Cells = max(1, (MaxByteLength + 7) div 8),
    atomics:new(Cells, [{signed, false}]).

%% Read Count bytes starting at byte Offset.
read_bytes(_Ref, _Offset, 0) ->
    <<>>;
read_bytes(Ref, Offset, Count)
  when is_integer(Offset), Offset >= 0, is_integer(Count), Count > 0 ->
    FirstCell = Offset div 8,
    %% Exclusive cell bound covering the last requested byte.
    EndCell = (Offset + Count + 7) div 8,
    Bin = cells_to_bin(Ref, FirstCell + 1, EndCell, []),
    Skip = Offset rem 8,
    binary:part(Bin, Skip, Count).

%% Collect cells [From..To] (1-indexed, inclusive) as a little-endian binary.
%% Built back-to-front so the accumulator prepends.
cells_to_bin(Ref, From, To, Acc) when To >= From ->
    V = atomics:get(Ref, To),
    cells_to_bin(Ref, From, To - 1, [<<V:64/little>> | Acc]);
cells_to_bin(_Ref, _From, _To, Acc) ->
    iolist_to_binary(Acc).

%% Write Bin into the storage at byte Offset.
write_bytes(Ref, Offset, Bin)
  when is_integer(Offset), Offset >= 0, is_binary(Bin) ->
    do_write(Ref, Offset, Bin),
    nil.

do_write(_Ref, _Offset, <<>>) ->
    ok;
do_write(Ref, Offset, Bin) ->
    Cell = Offset div 8 + 1,
    Skip = Offset rem 8,
    InCell = min(8 - Skip, byte_size(Bin)),
    <<Chunk:InCell/binary, Rest/binary>> = Bin,
    case {Skip, InCell} of
        {0, 8} ->
            %% Whole-cell span: plain atomic put.
            <<V:64/little>> = Chunk,
            atomics:put(Ref, Cell, V);
        _ ->
            %% Sub-word span: merge via compare_exchange retry loop.
            cas_merge(Ref, Cell, Skip, Chunk)
    end,
    do_write(Ref, Offset + InCell, Rest).

%% Splice Chunk into the cell's byte image at byte position Skip, retrying
%% until the CAS succeeds (i.e. no concurrent writer raced this cell between
%% our read and our write).
cas_merge(Ref, Cell, Skip, Chunk) ->
    Old = atomics:get(Ref, Cell),
    Len = byte_size(Chunk),
    <<Pre:Skip/binary, _:Len/binary, Post/binary>> = <<Old:64/little>>,
    <<New:64/little>> = <<Pre/binary, Chunk/binary, Post/binary>>,
    case atomics:compare_exchange(Ref, Cell, Old, New) of
        ok -> ok;
        _Raced -> cas_merge(Ref, Cell, Skip, Chunk)
    end.

%% Atomic read-modify-write of one element (§25.4.3.12 AtomicReadModifyWrite).
%% Fun(OldVal) -> NewVal computes the replacement from the element value the
%% cell was witnessed to hold; NewVal is truncated mod 2^SizeBits by the bit
%% syntax. Returns OldVal (sign-extended when Signed). Retries the WHOLE
%% read-compute-write when another writer raced the cell.
rmw_element(Ref, ByteOffset, SizeBits, Signed, Fun)
  when is_integer(ByteOffset), ByteOffset >= 0 ->
    Cell = ByteOffset div 8 + 1,
    Skip = ByteOffset rem 8,
    Size = SizeBits div 8,
    OldCell = atomics:get(Ref, Cell),
    <<Pre:Skip/binary, ElemBin:Size/binary, Post/binary>> = <<OldCell:64/little>>,
    OldVal = decode_elem(ElemBin, SizeBits, Signed),
    NewVal = Fun(OldVal),
    <<NewCell:64/little>> = <<Pre/binary, NewVal:SizeBits/little, Post/binary>>,
    case atomics:compare_exchange(Ref, Cell, OldCell, NewCell) of
        ok -> OldVal;
        _Raced -> rmw_element(Ref, ByteOffset, SizeBits, Signed, Fun)
    end.

%% Atomic compare-and-swap of one element (§25.4.7 Atomics.compareExchange):
%% store Replacement only when the element's current value equals Expected
%% (already wrapped to the element domain by the caller). Returns the
%% witnessed old value either way. Retries only when the CAS itself raced.
cas_element(Ref, ByteOffset, SizeBits, Signed, Expected, Replacement)
  when is_integer(ByteOffset), ByteOffset >= 0 ->
    Cell = ByteOffset div 8 + 1,
    Skip = ByteOffset rem 8,
    Size = SizeBits div 8,
    OldCell = atomics:get(Ref, Cell),
    <<Pre:Skip/binary, ElemBin:Size/binary, Post/binary>> = <<OldCell:64/little>>,
    OldVal = decode_elem(ElemBin, SizeBits, Signed),
    case OldVal =:= Expected of
        false ->
            OldVal;
        true ->
            <<NewCell:64/little>> =
                <<Pre/binary, Replacement:SizeBits/little, Post/binary>>,
            case atomics:compare_exchange(Ref, Cell, OldCell, NewCell) of
                ok -> OldVal;
                _Raced ->
                    cas_element(Ref, ByteOffset, SizeBits, Signed,
                                Expected, Replacement)
            end
    end.

decode_elem(Bin, SizeBits, true) ->
    <<V:SizeBits/little-signed>> = Bin,
    V;
decode_elem(Bin, SizeBits, false) ->
    <<V:SizeBits/little>> = Bin,
    V.
