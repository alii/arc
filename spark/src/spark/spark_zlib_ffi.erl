-module(spark_zlib_ffi).
-export([deflate/7, inflate/4, zstd_compress/2, zstd_decompress/1, crc32/2]).

%% every call opens its own stream and closes it: nothing outlives the call,
%% so nothing here ties the engine to one process

%% Ops is every write made so far, each with its flush; the answer is all the
%% output those writes give
deflate(Ops, Format, Level, WindowBits, MemLevel, Strategy, Dictionary) ->
    Z = zlib:open(),
    try
        ok = zlib:deflateInit(Z, level(Level), deflated,
                              deflate_bits(Format, WindowBits), MemLevel,
                              strategy(Strategy)),
        case Dictionary of
            {some, Bytes} -> zlib:deflateSetDictionary(Z, Bytes);
            none -> ok
        end,
        Out = [zlib:deflate(Z, Bytes, flush(Flush)) || {Bytes, Flush} <- Ops],
        {ok, iolist_to_binary(Out)}
    catch
        error:Reason -> {error, code(Reason)}
    after
        zlib:close(Z)
    end.

level(-1) -> default;
level(Level) -> Level.

strategy(1) -> filtered;
strategy(2) -> huffman_only;
strategy(3) -> rle;
strategy(_) -> default.

%% zlib's flush values: 0 none, 1 partial, 2 sync, 3 full, 4 finish, 5 block
flush(1) -> sync;
flush(2) -> sync;
flush(3) -> full;
flush(4) -> finish;
flush(_) -> none.

deflate_bits(gzip, Bits) -> Bits + 16;
deflate_bits(raw, Bits) -> -Bits;
deflate_bits(_, Bits) -> Bits.

%% the answer is the output so far and whether the stream reached its end.
%% Gzip data may be several members one after another, read as one; when
%% that fails but the first member alone is whole, what follows it is not
%% gzip, which zlib reports as a bad header
inflate(Data, Format, WindowBits, Dictionary) when Format =:= gzip; Format =:= either ->
    case inflate_as(Data, Format, WindowBits, Dictionary, reset) of
        {error, <<"Z_DATA_ERROR">>} = Failed ->
            case inflate_as(Data, Format, WindowBits, Dictionary, cut) of
                {ok, {_, true}} -> {error, <<"trailing">>};
                _ -> Failed
            end;
        Other ->
            Other
    end;
inflate(Data, Format, WindowBits, Dictionary) ->
    inflate_as(Data, Format, WindowBits, Dictionary, cut).

inflate_as(Data, Format, WindowBits, Dictionary, AtEnd) ->
    Z = zlib:open(),
    try
        ok = zlib:inflateInit(Z, inflate_bits(Format, WindowBits), AtEnd),
        Out = inflating(Z, Data, Dictionary, []),
        %% ending a stream that has not reached its end is an error, which
        %% is the only way to ask whether it has
        Finished = try zlib:inflateEnd(Z) of
            ok -> true
        catch
            error:data_error -> false
        end,
        {ok, {iolist_to_binary(Out), Finished}}
    catch
        error:Reason -> {error, code(Reason)}
    after
        zlib:close(Z)
    end.

%% `finished` here means the input given is used up, not that the stream ended
inflating(Z, Data, Dictionary, Acc) ->
    case zlib:safeInflate(Z, Data) of
        {finished, Out} ->
            lists:reverse([Out | Acc]);
        {continue, Out} ->
            inflating(Z, [], Dictionary, [Out | Acc]);
        {need_dictionary, _Adler, Out} ->
            case Dictionary of
                {some, Bytes} ->
                    ok = zlib:inflateSetDictionary(Z, Bytes),
                    inflating(Z, [], Dictionary, [Out | Acc]);
                none ->
                    erlang:error(need_dictionary)
            end
    end.

inflate_bits(Format, 0) -> inflate_bits(Format, 15);
inflate_bits(gzip, Bits) -> Bits + 16;
inflate_bits(either, Bits) -> Bits + 32;
inflate_bits(raw, Bits) -> -Bits;
inflate_bits(_, Bits) -> Bits.

code(data_error) -> <<"Z_DATA_ERROR">>;
code(buf_error) -> <<"Z_BUF_ERROR">>;
code(stream_error) -> <<"Z_STREAM_ERROR">>;
code(mem_error) -> <<"Z_MEM_ERROR">>;
code(version_error) -> <<"Z_VERSION_ERROR">>;
code(need_dictionary) -> <<"Z_NEED_DICT">>;
code({need_dictionary, _}) -> <<"Z_NEED_DICT">>;
code(_) -> <<"Z_DATA_ERROR">>.

zstd_compress(Data, Level) ->
    Options = case Level of
        {some, N} -> #{compressionLevel => N};
        none -> #{}
    end,
    try
        {ok, iolist_to_binary(zstd:compress(Data, Options))}
    catch
        error:{zstd_error, Message} -> {error, Message};
        error:_ -> {error, <<"Error (generic)">>}
    end.

%% frames are read one after another for as long as the bytes start a frame;
%% anything else after a whole frame is left alone, as zstd's own reader does
zstd_decompress(Data) ->
    try
        {ok, frames(Data, [], true)}
    catch
        error:{zstd_error, Message} -> {error, Message};
        error:_ -> {error, <<"Error (generic)">>}
    end.

frames(<<16#28, 16#B5, 16#2F, 16#FD, _/binary>> = Data, Acc, _First) ->
    case frame_length(Data) of
        {ok, Length} ->
            <<Frame:Length/binary, Rest/binary>> = Data,
            frames(Rest, [zstd:decompress(Frame) | Acc], false);
        short ->
            {iolist_to_binary(lists:reverse(Acc)), false}
    end;
frames(<<>>, Acc, First) ->
    {iolist_to_binary(lists:reverse(Acc)), not First};
frames(Data, _Acc, true) ->
    %% not a frame: zstd names what is wrong with it, or gives nothing when
    %% the bytes are only the start of one
    Out = iolist_to_binary(zstd:decompress(Data)),
    {Out, byte_size(Out) > 0};
frames(_Junk, Acc, false) ->
    {iolist_to_binary(lists:reverse(Acc)), true}.

%% how many bytes one frame takes: its header, each block (three bytes that
%% give the block's kind and size, then its data), and a checksum if it has one
frame_length(Data) ->
    case zstd:get_frame_header(Data) of
        {ok, #{headerSize := Header} = Info} when byte_size(Data) >= Header ->
            Checksum = case maps:get(checksumFlag, Info, false) of
                true -> 4;
                false -> 0
            end,
            blocks(Data, Header, Checksum);
        _ ->
            short
    end.

blocks(Data, At, Checksum) ->
    case Data of
        <<_:At/binary, Head:24/little, _/binary>> ->
            Size = case (Head bsr 1) band 3 of
                1 -> 1;
                _ -> Head bsr 3
            end,
            Next = At + 3 + Size,
            case Head band 1 of
                1 when byte_size(Data) >= Next + Checksum -> {ok, Next + Checksum};
                1 -> short;
                0 -> blocks(Data, Next, Checksum)
            end;
        _ ->
            short
    end.

crc32(Data, Initial) ->
    erlang:crc32(Initial, Data).
