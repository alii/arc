%% a buffer written a little at a time is a tree of fixed-size blocks, so a
%% write copies only one. A block is its bytes or, once written, a tuple of
%% pieces of them
-module(arc_rt_buffer_ffi).
-export([split/1, join/1, locate/3, read_region/3, write_region/4, update/5,
         element_at/5, with_element/6]).

-include("arc_rt_layout.hrl").

-define(BLOCK_BITS, 10).
-define(IN_BLOCK(Off), ((Off) band 1023)).
%% a piece of 64 bytes lives on the process heap, so replacing it is cheap. past
%% some size the collector is better off not copying the bytes around
-define(PIECE_BITS, 6).
-define(IN_PIECE(Off), ((Off) band 63)).
-define(PIECE(Off), ((?IN_BLOCK(Off) bsr ?PIECE_BITS) + 1)).
-define(TAKES_PIECES(Total), ((Total) =< 262144)).
-define(BLOCK(Off, Tree), arc_tree_array_ffi:get_or_hole((Off) bsr ?BLOCK_BITS, Tree)).
-define(WITH_BLOCK(Off, Block, Tree), arc_tree_array_ffi:set((Off) bsr ?BLOCK_BITS, Block, Tree)).

split(Bin) -> arc_tree_array_ffi:trie_from_list(cut(Bin, 1 bsl ?BLOCK_BITS)).

%% the last part may be short, and an empty buffer is one empty part
cut(Bin, Size) ->
    case Bin of
        <<Part:Size/binary, Rest/binary>> when Rest =/= <<>> -> [Part | cut(Rest, Size)];
        _ -> [Bin]
    end.

pieces(Block) when is_binary(Block) -> list_to_tuple(cut(Block, 1 bsl ?PIECE_BITS));
pieces(Block) -> Block.

bytes(Block) when is_binary(Block) -> Block;
bytes(Block) -> iolist_to_binary(tuple_to_list(Block)).

join(Tree) -> iolist_to_binary(iodata(arc_tree_array_ffi:to_list(Tree))).

iodata([Block | Blocks]) when is_binary(Block) -> [Block | iodata(Blocks)];
iodata([Block | Blocks]) -> [tuple_to_list(Block) | iodata(Blocks)];
iodata([]) -> [].

%% returns the bytes at [Off, Off+Len) as {Binary, OffsetInBinary}. A range
%% inside one piece or whole block returns that, so nothing is copied
locate(Tree, Off, Len) when ?IN_BLOCK(Off) + Len =< 1 bsl ?BLOCK_BITS ->
    case ?BLOCK(Off, Tree) of
        Block when is_binary(Block) -> {Block, ?IN_BLOCK(Off)};
        Block when ?IN_PIECE(Off) + Len =< 1 bsl ?PIECE_BITS ->
            {element(?PIECE(Off), Block), ?IN_PIECE(Off)};
        Block -> {bytes(Block), ?IN_BLOCK(Off)}
    end;
locate(Tree, Off, Len) -> {read_region(Tree, Off, Len), 0}.

read_region(_, _, 0) -> <<>>;
read_region(Tree, Off, Len) -> iolist_to_binary(parts(Tree, Off, Off + Len)).

parts(_, End, End) -> [];
parts(Tree, Off, End) ->
    Len = min(End - Off, (1 bsl ?BLOCK_BITS) - ?IN_BLOCK(Off)),
    {Bin, At} = locate(Tree, Off, Len),
    [binary:part(Bin, At, Len) | parts(Tree, Off + Len, End)].

write_region(Tree, _, _, <<>>) -> Tree;
write_region(Tree, Total, Off, Bin) ->
    Len = min(byte_size(Bin), (1 bsl ?BLOCK_BITS) - ?IN_BLOCK(Off)),
    <<Part:Len/binary, Rest/binary>> = Bin,
    Tree1 = case Len of
        1 bsl ?BLOCK_BITS -> ?WITH_BLOCK(Off, Part, Tree);
        _ -> update(Tree, Total, Off, Len, fun(Old, At) -> spliced(Old, At, Part) end)
    end,
    write_region(Tree1, Total, Off + Len, Rest).

spliced(Bin, At, Part) ->
    After = At + byte_size(Part),
    <<_:After/binary, Rest/binary>> = Bin,
    <<Bin:At/binary, Part/binary, Rest/binary>>.

%% Write(Binary, OffsetInBinary) returns the whole binary with the element set
update(Tree, Total, Off, Len, Write)
  when ?TAKES_PIECES(Total), ?IN_PIECE(Off) + Len =< 1 bsl ?PIECE_BITS ->
    Block = pieces(?BLOCK(Off, Tree)),
    Piece = Write(element(?PIECE(Off), Block), ?IN_PIECE(Off)),
    ?WITH_BLOCK(Off, setelement(?PIECE(Off), Block, Piece), Tree);
update(Tree, _, Off, Len, Write) when ?IN_BLOCK(Off) + Len =< 1 bsl ?BLOCK_BITS ->
    ?WITH_BLOCK(Off, Write(bytes(?BLOCK(Off, Tree)), ?IN_BLOCK(Off)), Tree);
update(Tree, Total, Off, Len, Write) ->
    write_region(Tree, Total, Off, Write(read_region(Tree, Off, Len), 0)).

-compile({inline, [size_bits/1]}).
size_bits(int8_kind) -> 0;
size_bits(uint8_kind) -> 0;
size_bits(uint8_clamped_kind) -> 0;
size_bits(int16_kind) -> 1;
size_bits(uint16_kind) -> 1;
size_bits(int32_kind) -> 2;
size_bits(uint32_kind) -> 2;
size_bits(float32_kind) -> 2;
size_bits(float64_kind) -> 3.

%% §10.4.5.17 for a number element inside a view of fixed length, else miss. a
%% view's offset is a multiple of its element size, so no element spans pieces
element_at(Storage, {num_kind, Kind}, Offset, {?SOME, Len}, Idx)
  when is_integer(Offset), is_integer(Len), is_integer(Idx), 0 =< Idx, Idx < Len ->
    Size = size_bits(Kind),
    Off = Offset + (Idx bsl Size),
    case Storage of
        {bytes, Bin, _} when Offset + (Len bsl Size) =< byte_size(Bin) -> decode(Kind, Bin, Off);
        {chunked, Tree, Total, _} when Offset + (Len bsl Size) =< Total ->
            case ?BLOCK(Off, Tree) of
                Block when is_binary(Block) -> decode(Kind, Block, ?IN_BLOCK(Off));
                Block -> decode(Kind, element(?PIECE(Off), Block), ?IN_PIECE(Off))
            end;
        _ -> miss
    end;
element_at(_, _, _, _, _) -> miss.

-define(AT(Segment), begin <<_:Off/binary, Segment, _/binary>> = Bin, V end).
decode(uint8_kind, Bin, Off) -> ?AT(V:8);
decode(int32_kind, Bin, Off) -> ?AT(V:32/little-signed);
decode(uint32_kind, Bin, Off) -> ?AT(V:32/little);
decode(uint16_kind, Bin, Off) -> ?AT(V:16/little);
decode(int16_kind, Bin, Off) -> ?AT(V:16/little-signed);
decode(int8_kind, Bin, Off) -> ?AT(V:8/signed);
decode(uint8_clamped_kind, Bin, Off) -> ?AT(V:8);
%% nan and the infinities do not match as floats
decode(float64_kind, Bin, Off) ->
    case Bin of
        <<_:Off/binary, V:64/float-little, _/binary>> -> V;
        _ -> miss
    end;
decode(float32_kind, Bin, Off) ->
    case Bin of
        <<_:Off/binary, V:32/float-little, _/binary>> -> V;
        _ -> miss
    end.

%% §10.4.5.18 likewise, for a finite number; the storage after, else miss
with_element(Storage, {num_kind, Kind}, Offset, {?SOME, Len}, Idx, V)
  when is_integer(Offset), is_integer(Len), is_integer(Idx), 0 =< Idx, Idx < Len,
       is_number(V) ->
    Size = size_bits(Kind),
    Off = Offset + (Idx bsl Size),
    case Storage of
        {chunked, Tree, Total, Max} when Offset + (Len bsl Size) =< Total ->
            Block = case ?BLOCK(Off, Tree) of
                Whole when not ?TAKES_PIECES(Total) ->
                    encode(Kind, Whole, ?IN_BLOCK(Off), 1 bsl Size, V);
                Held ->
                    Pieces = pieces(Held),
                    At = ?PIECE(Off),
                    Piece = encode(Kind, element(At, Pieces), ?IN_PIECE(Off), 1 bsl Size, V),
                    setelement(At, Pieces, Piece)
            end,
            {chunked, ?WITH_BLOCK(Off, Block, Tree), Total, Max};
        {bytes, Bin, Max} when byte_size(Bin) =< 64, Offset + (Len bsl Size) =< byte_size(Bin) ->
            {bytes, encode(Kind, Bin, Off, 1 bsl Size, V), Max};
        _ -> miss
    end;
with_element(_, _, _, _, _, _) -> miss.

%% sizing the first segment keeps this from being an append, which would
%% allocate room to grow
encode(Kind, Bin, Off, Size, V) ->
    After = Off + Size,
    <<_:After/binary, Rest/binary>> = Bin,
    case Kind of
        float64_kind -> <<Bin:Off/binary, V:64/float-little, Rest/binary>>;
        float32_kind -> <<Bin:Off/binary, (float(V)):32/float-little, Rest/binary>>;
        uint8_clamped_kind ->
            Num = if is_integer(V) -> {j_int, V}; true -> {j_float, V} end,
            <<Bin:Off/binary, (arc_rt_typed_array_bytes_ffi:clamp_uint8(Num)), Rest/binary>>;
        _ ->
            %% the segment keeps the low bits, as the conversions wrap
            I = if is_integer(V) -> V; true -> trunc(V) end,
            case Size of
                1 -> <<Bin:Off/binary, I:8, Rest/binary>>;
                2 -> <<Bin:Off/binary, I:16/little, Rest/binary>>;
                4 -> <<Bin:Off/binary, I:32/little, Rest/binary>>
            end
    end.
