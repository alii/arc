#!/usr/bin/env escript
%%! -noshell
%% Minimal AVM packer — just enough of packbeam to bundle Arc for AtomVM.
%%
%% usage: pack_avm.escript <output.avm> <start_module> <input.beam|input.avm>...
%%
%% Format: 24-byte shebang header, then per-entry:
%%   <<Size:32, Flags:32, 0:32, Name/binary, 0, Pad, Data, Pad>>
%% where Size is the whole entry, Flags bit0=start bit1=beam, all fields
%% 4-byte aligned. Trailing all-zero entry terminates.
%%
%% Beam chunks are filtered to what AtomVM reads, and compressed LitT is
%% inflated to LitU since AtomVM doesn't decompress at load time.

-define(HEADER, <<"#!/usr/bin/env AtomVM\n", 0, 0>>).
-define(CHUNKS, ["AtU8","Code","ExpT","LocT","ImpT","LitU","FunT","StrT","LitT","Type"]).
-define(F_START, 1).
-define(F_BEAM, 2).

main([Out, Start | Inputs]) ->
    StartMod = list_to_atom(Start),
    Entries = dedup(lists:flatmap(fun load/1, Inputs)),
    {S, Rest} = lists:partition(fun({M,_,_}) -> M =:= StartMod end, Entries),
    S =/= [] orelse halt_err("start module ~s not found", [Start]),
    Ordered = S ++ Rest,
    Body = [pack(E, StartMod) || E <- Ordered],
    ok = file:write_file(Out, [?HEADER, Body, <<0:32,0:32,0:32>>]),
    io:format("~s: ~b modules~n", [Out, length(Ordered)]);
main(_) ->
    halt_err("usage: pack_avm.escript OUT START IN...", []).

load(Path) ->
    case filename:extension(Path) of
        ".beam" -> [load_beam(Path)];
        ".avm"  -> load_avm(Path);
        Ext     -> halt_err("unsupported input ~s (~s)", [Path, Ext])
    end.

load_beam(Path) ->
    {ok, Bin} = file:read_file(Path),
    {ok, Mod, Chunks0} = beam_lib:all_chunks(Bin),
    Chunks1 = [{K,V} || {K,V} <- Chunks0, lists:member(K, ?CHUNKS)],
    Chunks2 = uncompress_lit(Chunks1),
    {ok, Stripped} = beam_lib:build_module(Chunks2),
    {Mod, atom_to_list(Mod) ++ ".beam", Stripped}.

uncompress_lit(Chunks) ->
    case lists:keytake("LitT", 1, Chunks) of
        false -> Chunks;
        {value, {_, <<0:32, Data/binary>>}, Rest} ->
            [{"LitU", Data} | Rest];
        {value, {_, <<_Sz:32, Comp/binary>>}, Rest} ->
            [{"LitU", zlib:uncompress(Comp)} | Rest]
    end.

%% Pre-packed library avm — slurp its entries so shadowing works (our shims
%% for unicode_ffi/arc_regexp_ffi must win over any duplicates downstream).
load_avm(Path) ->
    {ok, <<"#!/usr/bin/env AtomVM\n", 0, 0, Body/binary>>} = file:read_file(Path),
    avm_entries(Body, []).

avm_entries(<<0:32, _/binary>>, Acc) -> lists:reverse(Acc);
avm_entries(<<Size:32, Flags:32, _Res:32, Rest/binary>>, Acc) ->
    HeaderLen = 12,
    BodyLen = Size - HeaderLen,
    <<Body:BodyLen/binary, Tail/binary>> = Rest,
    {Name, Data} = split_name(Body),
    Entry = case Flags band ?F_BEAM of
        0 -> {undefined, Name, Data};
        _ -> {list_to_atom(filename:rootname(Name)), Name, Data}
    end,
    avm_entries(Tail, [Entry | Acc]).

split_name(Bin) ->
    [Name, Rest] = binary:split(Bin, <<0>>),
    NameLen = byte_size(Name) + 1,
    Skip = pad_to_4(12 + NameLen) - (12 + NameLen),
    <<_:Skip/binary, Data0/binary>> = Rest,
    {binary_to_list(Name), strip_trailing_zeros(Data0)}.

strip_trailing_zeros(B) ->
    S = byte_size(B),
    strip_tz(B, S).
strip_tz(B, 0) -> B;
strip_tz(B, N) ->
    case binary:at(B, N-1) of
        0 -> strip_tz(B, N-1);
        _ -> binary:part(B, 0, N)
    end.

pack({Mod, Name, Data}, Start) ->
    NameB = list_to_binary(Name),
    HdrLen = 12 + byte_size(NameB) + 1,
    HdrPad = pad(HdrLen),
    DataPad = pad(byte_size(Data)),
    Size = HdrLen + byte_size(HdrPad) + byte_size(Data) + byte_size(DataPad),
    Flags = case Mod of
        undefined -> 4;
        Start     -> ?F_START bor ?F_BEAM;
        _         -> ?F_BEAM
    end,
    <<Size:32, Flags:32, 0:32, NameB/binary, 0, HdrPad/binary,
      Data/binary, DataPad/binary>>.

pad(N) -> binary:copy(<<0>>, (4 - N rem 4) rem 4).
pad_to_4(N) -> N + (4 - N rem 4) rem 4.

%% First occurrence wins — shims packed ahead of stdlib shadow it.
dedup(Entries) -> dedup(Entries, #{}, []).
dedup([], _, Acc) -> lists:reverse(Acc);
dedup([{M, _, _} = E | T], Seen, Acc) ->
    case maps:is_key(M, Seen) of
        true -> dedup(T, Seen, Acc);
        false -> dedup(T, Seen#{M => 1}, [E | Acc])
    end.

halt_err(Fmt, Args) ->
    io:format(standard_error, Fmt ++ "~n", Args),
    halt(1).
