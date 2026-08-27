-module(arc_cli_ffi).
-export([read_line/1]).
-export([get_script_args/0]).

read_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> eof;
        {error, Reason} -> {read_error, Reason};
        Line when is_binary(Line) -> {line, Line};
        Line when is_list(Line) -> encode_line(Line)
    end.

encode_line(Line) ->
    case unicode:characters_to_binary(Line) of
        Bin when is_binary(Bin) -> {line, Bin};
        {error, _Encoded, Rest} -> {read_error, {invalid_unicode, Rest}};
        {incomplete, _Encoded, Rest} -> {read_error, {incomplete_unicode, Rest}}
    end.

%% list_to_binary would badarg on codepoints > 255
get_script_args() ->
    [encode_arg(A) || A <- init:get_plain_arguments()].

encode_arg(A) ->
    case unicode:characters_to_binary(A) of
        Bin when is_binary(Bin) -> Bin;
        _Error -> erlang:error({bad_argv_encoding, A})
    end.
