-module(emit_2core_probe_ffi).
-export([env_int/2, load_average/0]).

env_int(Name, Default) ->
    case os:getenv(binary_to_list(Name)) of
        false -> Default;
        S ->
            case string:to_integer(string:trim(S)) of
                {N, ""} when N > 0 -> N;
                _ -> Default
            end
    end.

load_average() ->
    Out = os:cmd("uptime"),
    Tail = case string:find(Out, "load average") of
        nomatch -> Out;
        T -> T
    end,
    Nums = [N || N <- string:lexemes(Tail, ": ,\n"), is_number_token(N)],
    unicode:characters_to_binary(lists:join(" ", Nums)).

is_number_token(S) ->
    case string:to_float(S) of
        {F, ""} when is_float(F) -> true;
        _ -> false
    end.
