%% a big buffer as a tree of fixed-size pieces, so one write copies one piece
-module(arc_rt_buffer_ffi).
-export([split/1, join/1, locate/3, read_region/3, write_region/3, update/4]).

-define(PIECE, 1024).

split(Bin) -> arc_tree_array_ffi:from_list(pieces(Bin)).

%% the last piece may be short, an empty buffer is one empty piece
pieces(<<Piece:?PIECE/binary, Rest/binary>>) when Rest =/= <<>> ->
    [Piece | pieces(Rest)];
pieces(Bin) -> [Bin].

join(Tree) -> iolist_to_binary(arc_tree_array_ffi:to_list(Tree)).

%% the bytes at [Off, Off+Len) as {Binary, OffsetInBinary}: the owning piece
%% itself when the range sits inside one, so nothing is copied
locate(Tree, Off, Len) ->
    Rel = Off rem ?PIECE,
    case Rel + Len =< ?PIECE of
        true -> {arc_tree_array_ffi:get_or_hole(Off div ?PIECE, Tree), Rel};
        false -> {read_region(Tree, Off, Len), 0}
    end.

read_region(_Tree, _Off, 0) -> <<>>;
read_region(Tree, Off, Len) ->
    iolist_to_binary(
        [begin
             Piece = arc_tree_array_ffi:get_or_hole(Ix, Tree),
             From = max(Off - Ix * ?PIECE, 0),
             To = min(Off + Len - Ix * ?PIECE, byte_size(Piece)),
             binary:part(Piece, From, To - From)
         end || Ix <- lists:seq(Off div ?PIECE, (Off + Len - 1) div ?PIECE)]).

write_region(Tree, _Off, <<>>) -> Tree;
write_region(Tree, Off, Bin) ->
    Len = byte_size(Bin),
    lists:foldl(
        fun(Ix, Acc) ->
            Piece = arc_tree_array_ffi:get_or_hole(Ix, Acc),
            From = max(Off - Ix * ?PIECE, 0),
            To = min(Off + Len - Ix * ?PIECE, byte_size(Piece)),
            Take = To - From,
            <<Pre:From/binary, _:Take/binary, Post/binary>> = Piece,
            New = binary:part(Bin, Ix * ?PIECE + From - Off, Take),
            arc_tree_array_ffi:set(Ix, <<Pre/binary, New/binary, Post/binary>>, Acc)
        end,
        Tree,
        lists:seq(Off div ?PIECE, (Off + Len - 1) div ?PIECE)).

%% Write(Binary, OffsetInBinary) returns the whole binary with the element set
update(Tree, Off, Len, Write) ->
    Rel = Off rem ?PIECE,
    case Rel + Len =< ?PIECE of
        true ->
            Ix = Off div ?PIECE,
            Piece = arc_tree_array_ffi:get_or_hole(Ix, Tree),
            arc_tree_array_ffi:set(Ix, Write(Piece, Rel), Tree);
        false ->
            write_region(Tree, Off, Write(read_region(Tree, Off, Len), 0))
    end.
