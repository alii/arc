%% CLI-only FFI: argv. Bound exclusively by src/arc_aot.gleam.
-module(arc_aot_cli_ffi).
-export([get_script_args/0]).

get_script_args() ->
    [encode_arg(A) || A <- init:get_plain_arguments()].

encode_arg(A) ->
    case unicode:characters_to_binary(A) of
        Bin when is_binary(Bin) -> Bin;
        _Error -> erlang:error({bad_argv_encoding, A})
    end.
