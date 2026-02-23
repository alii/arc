-module(arc_ffi).
-export([read_stdin/0]).

%% Read all available data from stdin as a binary string.
%% Returns an empty binary if stdin is empty/EOF immediately.
read_stdin() ->
    read_stdin_loop(<<>>).

read_stdin_loop(Acc) ->
    case io:get_chars(standard_io, <<>>, 4096) of
        eof -> Acc;
        {error, _} -> Acc;
        Data when is_list(Data) ->
            Bin = unicode:characters_to_binary(Data),
            read_stdin_loop(<<Acc/binary, Bin/binary>>);
        Data when is_binary(Data) ->
            read_stdin_loop(<<Acc/binary, Data/binary>>)
    end.
