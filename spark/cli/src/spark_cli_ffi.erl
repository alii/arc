-module(spark_cli_ffi).
-export([access/2, realpath/1, write/2, env/0, cwd/0, platform/0, arch/0, pid/0, is_tty/1]).
-export([os_release/0, os_version/0, machine/0, hostname/0,
         homedir/0, tmpdir/0, little_endian/0, parallelism/0]).

-include_lib("kernel/include/file.hrl").

%% node's access modes: 4 read, 2 write, 1 execute, 0 exists
access(Path, Mode) ->
    case file:read_file_info(Path) of
        {ok, Info} ->
            case allowed(Info, Mode) of
                true -> {ok, done};
                false -> {error, {failure, <<"EACCES">>}}
            end;
        {error, Reason} when is_atom(Reason) ->
            Code = string:uppercase(atom_to_binary(Reason, utf8)),
            {error, {failure, Code}}
    end.

allowed(#file_info{access = Access, mode = Bits}, Mode) ->
    (Mode band 4 =:= 0 orelse Access =:= read orelse Access =:= read_write)
        andalso
        (Mode band 2 =:= 0 orelse Access =:= write orelse Access =:= read_write)
        andalso
        (Mode band 1 =:= 0 orelse Bits band 8#111 =/= 0).

write(Stream, Data) ->
    try io:put_chars(device(Stream), Data) of
        ok -> {ok, done}
    catch
        error:_ -> {error, {failure, <<"EIO">>}}
    end.

device(stdout) -> standard_io;
device(stderr) -> standard_error.

env() ->
    lists:map(fun(Entry) ->
        case binary:split(unicode:characters_to_binary(Entry), <<"=">>) of
            [Name, Value] -> {Name, Value};
            [Name] -> {Name, <<>>}
        end
    end, os:getenv()).

cwd() ->
    {ok, Dir} = file:get_cwd(),
    unicode:characters_to_binary(Dir).

platform() ->
    case os:type() of
        {unix, darwin} -> {ok, darwin};
        {unix, linux} -> {ok, linux};
        {_, Name} -> {error, atom_to_binary(Name, utf8)}
    end.

arch() ->
    Triple = erlang:system_info(system_architecture),
    case hd(string:split(Triple, "-")) of
        "aarch64" -> <<"arm64">>;
        "arm64" -> <<"arm64">>;
        "x86_64" -> <<"x64">>;
        "amd64" -> <<"x64">>;
        Other -> unicode:characters_to_binary(Other)
    end.

pid() -> list_to_integer(os:getpid()).

is_tty(Stream) ->
    case io:columns(device(Stream)) of
        {ok, _} -> true;
        {error, _} -> false
    end.

os_release() ->
    case os:version() of
        {Major, Minor, Patch} ->
            iolist_to_binary(io_lib:format("~b.~b.~b", [Major, Minor, Patch]));
        Text ->
            unicode:characters_to_binary(Text)
    end.

os_version() -> uname("-v").

machine() -> uname("-m").

uname(Flag) ->
    case os:type() of
        {unix, _} ->
            unicode:characters_to_binary(string:trim(os:cmd("uname " ++ Flag)));
        _ ->
            <<>>
    end.

hostname() ->
    {ok, Name} = inet:gethostname(),
    unicode:characters_to_binary(Name).

homedir() ->
    case os:getenv("HOME") of
        false -> <<"/">>;
        Home -> unicode:characters_to_binary(Home)
    end.

%% the first of node's temp variables that is set, else /tmp
tmpdir() ->
    Found = [V || Name <- ["TMPDIR", "TMP", "TEMP", "TEMPDIR"],
                  V <- [os:getenv(Name)], V =/= false, V =/= ""],
    Dir = case Found of
        [First | _] -> First;
        [] -> "/tmp"
    end,
    unicode:characters_to_binary(drop_trailing_slash(Dir)).

drop_trailing_slash(Dir) ->
    case lists:reverse(Dir) of
        [$/, Next | Rest] -> lists:reverse([Next | Rest]);
        _ -> Dir
    end.

little_endian() -> <<1:16/native>> =:= <<1:16/little>>.

parallelism() ->
    case erlang:system_info(logical_processors_available) of
        N when is_integer(N) -> N;
        _ -> erlang:system_info(schedulers_online)
    end.

%% follows every symlink, one path part at a time
realpath(Path) ->
    Absolute = filename:absname(unicode:characters_to_list(Path)),
    follow(filename:split(Absolute), [], 0).

follow(_Parts, _Done, Hops) when Hops > 40 ->
    {error, {failure, <<"ELOOP">>}};
follow([], Done, _Hops) ->
    {ok, {text, unicode:characters_to_binary(filename:join(Done))}};
follow(["." | Rest], Done, Hops) ->
    follow(Rest, Done, Hops);
follow([".." | Rest], Done, Hops) ->
    follow(Rest, parent(Done), Hops);
follow([Part | Rest], Done, Hops) ->
    Here = filename:join(Done ++ [Part]),
    case file:read_link_info(Here) of
        {ok, #file_info{type = symlink}} ->
            case file:read_link_all(Here) of
                {ok, Target} ->
                    Text = unicode:characters_to_list(Target),
                    case filename:pathtype(Text) of
                        absolute -> follow(filename:split(Text) ++ Rest, [], Hops + 1);
                        _ -> follow(filename:split(Text) ++ Rest, Done, Hops + 1)
                    end;
                {error, Reason} ->
                    {error, {failure, posix(Reason)}}
            end;
        {ok, _Info} ->
            follow(Rest, Done ++ [Part], Hops);
        {error, Reason} ->
            {error, {failure, posix(Reason)}}
    end.

parent([Root]) -> [Root];
parent(Done) -> lists:droplast(Done).

posix(Reason) when is_atom(Reason) ->
    string:uppercase(atom_to_binary(Reason, utf8)).
