-module(spark_cli_ffi).
-export([access/2, realpath/1, write/2, env/0, cwd/0, platform/0, arch/0, pid/0, is_tty/1]).
-export([stat/2, readlink/1, readdir/1, read_file/1]).
-export([user/0, certificates/0, terminals/0]).
-export([fetch/4, read_body/1, drop_body/1]).
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

failure(Reason) when is_atom(Reason) ->
    {error, {failure, string:uppercase(atom_to_binary(Reason, utf8))}}.

%% erlang reports times in whole seconds and has no block counts or birth
%% time, so those are filled in: blocks from the size, birth as the change time
stat(Path, Follow) ->
    Read = case Follow of
        true -> fun file:read_file_info/2;
        false -> fun file:read_link_info/2
    end,
    case Read(Path, [{time, posix}]) of
        {ok, #file_info{size = Size, mode = Mode, links = Links,
                        major_device = Dev, minor_device = Rdev, inode = Ino,
                        uid = Uid, gid = Gid,
                        atime = Atime, mtime = Mtime, ctime = Ctime}} ->
            Second = 1000000000,
            {ok, {info, {file_info, kind(Mode), Mode band 8#7777, Size, Dev,
                         Ino, Links, Uid, Gid, Rdev, 4096, (Size + 511) div 512,
                         Atime * Second, Mtime * Second, Ctime * Second,
                         Ctime * Second}}};
        {error, Reason} -> failure(Reason)
    end.

kind(Mode) ->
    case Mode band 8#170000 of
        8#040000 -> directory;
        8#120000 -> symlink;
        8#010000 -> fifo;
        8#140000 -> socket;
        8#020000 -> char_device;
        8#060000 -> block_device;
        _ -> file
    end.

readlink(Path) ->
    case file:read_link_all(Path) of
        {ok, Target} -> {ok, {text, unicode:characters_to_binary(Target)}};
        {error, Reason} -> failure(Reason)
    end.

readdir(Path) ->
    case file:list_dir_all(Path) of
        {ok, Names} ->
            {ok, {entries, [entry(Path, Name) || Name <- Names]}};
        {error, Reason} -> failure(Reason)
    end.

entry(Dir, Name) ->
    Text = unicode:characters_to_binary(Name),
    Kind = case file:read_link_info(filename:join(Dir, Name)) of
        {ok, #file_info{mode = Mode}} -> kind(Mode);
        {error, _} -> file
    end,
    {entry, Text, Kind}.

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Bytes} -> {ok, {bytes, Bytes}};
        {error, Reason} -> failure(Reason)
    end.

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

%% erlang cannot ask who it runs as, so: the owner of the home directory, and
%% the names the shell left in the environment
user() ->
    Home = case os:getenv("HOME") of false -> "/"; H -> H end,
    {Uid, Gid} = case file:read_file_info(Home) of
        {ok, #file_info{uid = U, gid = G}} -> {U, G};
        {error, _} -> {0, 0}
    end,
    Name = case os:getenv("USER") of false -> ""; N -> N end,
    Shell = case os:getenv("SHELL") of
        false -> none;
        S -> {some, unicode:characters_to_binary(S)}
    end,
    {user, Uid, Gid, unicode:characters_to_binary(Name), Shell}.

%% the machine's trusted roots as PEM texts; none if they cannot be read
certificates() ->
    try public_key:cacerts_get() of
        Certs -> [pem(Der) || {cert, Der, _} <- Certs]
    catch
        _:_ -> []
    end.

pem(Der) ->
    Lines = lines(base64:encode(Der)),
    iolist_to_binary(["-----BEGIN CERTIFICATE-----\n", Lines,
                      "-----END CERTIFICATE-----"]).

lines(<<Line:64/binary, Rest/binary>>) -> [Line, $\n | lines(Rest)];
lines(<<>>) -> [];
lines(Last) -> [Last, $\n].

terminals() ->
    [Fd || {Fd, Stream} <- [{1, stdout}, {2, stderr}], is_tty(Stream)].

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

%% ---- http, for fetch
%%
%% each request has a process of its own, which owns the connection and hands
%% the body out a piece at a time as it is asked for. A small registry gives
%% each of those processes the number the script knows the body by

fetch(Method, Url, Headers, Body) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(ssl),
    Caller = self(),
    Owner = spawn(fun() -> request(Caller, Method, Url, Headers, Body) end),
    receive {Owner, Answer} -> Answer end.

%% the headers go out exactly as given, so the three the connection itself
%% needs are added here
request(Caller, Method, Url, Headers, Body) ->
    Given = [{binary_to_list(Name), binary_to_list(Value)} || {Name, Value} <- Headers],
    Verb = verb(Method),
    HasBody = Body =/= <<>> orelse lists:member(Verb, [post, put, patch]),
    Length = case HasBody of
        true -> [{"content-length", integer_to_list(byte_size(Body))}];
        false -> []
    end,
    Sent = [{"host", host(Url)}, {"connection", "keep-alive"}] ++ Length ++ Given,
    Request = case HasBody of
        true -> {binary_to_list(Url), Sent, "", Body};
        false -> {binary_to_list(Url), Sent}
    end,
    HttpOptions = [{autoredirect, false}, {timeout, infinity},
                   {connect_timeout, 30000}, {ssl, tls_options()}],
    Options = [{sync, false}, {stream, {self, once}}, {body_format, binary},
               {headers_as_is, true}],
    case Verb of
        unknown ->
            Caller ! {self(), {error, {failure, <<"EINVAL">>}}};
        _ ->
            case httpc:request(Verb, Request, HttpOptions, Options) of
                {ok, Id} -> head(Caller, Id);
                {error, Reason} -> Caller ! {self(), {error, {failure, network_failure(Reason)}}}
            end
    end.

host(Url) ->
    case uri_string:parse(binary_to_list(Url)) of
        #{host := Host, port := Port} -> Host ++ ":" ++ integer_to_list(Port);
        #{host := Host} -> Host;
        _ -> ""
    end.

verb(Method) ->
    case string:lowercase(binary_to_list(Method)) of
        "get" -> get;
        "head" -> head;
        "post" -> post;
        "put" -> put;
        "patch" -> patch;
        "delete" -> delete;
        "options" -> options;
        _ -> unknown
    end.

tls_options() ->
    try public_key:cacerts_get() of
        Certs ->
            [{verify, verify_peer}, {cacerts, Certs},
             {customize_hostname_check,
              [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]
    catch
        _:_ -> [{verify, verify_none}]
    end.

%% httpc hands over a 200 or 206 piece by piece, and anything else whole
head(Caller, Id) ->
    receive
        {http, {Id, stream_start, Headers, Stream}} ->
            {Status, Reason} = case proplists:is_defined("content-range", Headers) of
                true -> {206, <<"Partial Content">>};
                false -> {200, <<"OK">>}
            end,
            Caller ! {self(), {ok, {head, Status, Reason, headers(Headers), register_body()}}},
            streaming(Id, Stream);
        {http, {Id, {{_Version, Status, Reason}, Headers, Body}}} ->
            Caller ! {self(), {ok, {head, Status, unicode:characters_to_binary(Reason),
                                    headers(Headers), register_body()}}},
            whole(Body);
        {http, {Id, {error, Reason}}} ->
            Caller ! {self(), {error, {failure, network_failure(Reason)}}}
    end.

headers(Headers) ->
    [{unicode:characters_to_binary(Name, latin1), unicode:characters_to_binary(Value, latin1)}
     || {Name, Value} <- Headers].

streaming(Id, Stream) ->
    receive
        {read, From} ->
            httpc:stream_next(Stream),
            receive
                {http, {Id, stream, Piece}} ->
                    From ! {self(), {ok, {bytes, Piece}}},
                    streaming(Id, Stream);
                {http, {Id, stream_end, _Trailers}} ->
                    From ! {self(), {ok, done}};
                {http, {Id, {error, Reason}}} ->
                    From ! {self(), {error, {failure, network_failure(Reason)}}};
                drop ->
                    httpc:cancel_request(Id),
                    From ! {self(), {ok, done}}
            end;
        drop ->
            httpc:cancel_request(Id)
    end.

whole(Body) ->
    receive
        {read, From} when Body =:= <<>> ->
            From ! {self(), {ok, done}};
        {read, From} ->
            From ! {self(), {ok, {bytes, Body}}},
            whole(<<>>);
        drop ->
            ok
    end.

read_body(Body) ->
    case find_body(Body) of
        {ok, Owner} ->
            Watch = erlang:monitor(process, Owner),
            Owner ! {read, self()},
            receive
                {Owner, Answer} ->
                    erlang:demonitor(Watch, [flush]),
                    Answer;
                {'DOWN', Watch, process, Owner, _} ->
                    {ok, done}
            end;
        error ->
            {ok, done}
    end.

drop_body(Body) ->
    case find_body(Body) of
        {ok, Owner} -> Owner ! drop;
        error -> ok
    end,
    bodies() ! {forget, Body},
    {ok, done}.

register_body() ->
    bodies() ! {add, self()},
    receive {body, Number} -> Number end.

find_body(Body) ->
    bodies() ! {find, self(), Body},
    receive {found, Found} -> Found end.

bodies() ->
    case whereis(spark_cli_bodies) of
        undefined ->
            Pid = spawn(fun() -> bodies(#{}, 1) end),
            try register(spark_cli_bodies, Pid) of
                true -> Pid
            catch
                error:badarg ->
                    exit(Pid, kill),
                    whereis(spark_cli_bodies)
            end;
        Pid ->
            Pid
    end.

bodies(Owners, Next) ->
    receive
        {add, Owner} ->
            Owner ! {body, Next},
            bodies(Owners#{Next => Owner}, Next + 1);
        {find, From, Body} ->
            From ! {found, maps:find(Body, Owners)},
            bodies(Owners, Next);
        {forget, Body} ->
            bodies(maps:remove(Body, Owners), Next)
    end.

%% the posix name inside whatever httpc gave as its reason
network_failure(Reason) ->
    Known = [econnrefused, econnreset, etimedout, ehostunreach, enetunreach,
             eaddrnotavail, epipe, nxdomain, timeout, closed, socket_closed_remotely],
    case [Name || Name <- Known, mentions(Name, Reason)] of
        [nxdomain | _] -> <<"ENOTFOUND">>;
        [timeout | _] -> <<"ETIMEDOUT">>;
        [closed | _] -> <<"ECONNRESET">>;
        [socket_closed_remotely | _] -> <<"ECONNRESET">>;
        [Name | _] -> list_to_binary(string:uppercase(atom_to_list(Name)));
        [] -> <<"EIO">>
    end.

mentions(Name, Name) -> true;
mentions(Name, Term) when is_tuple(Term) -> mentions(Name, tuple_to_list(Term));
mentions(Name, [Head | Tail]) -> mentions(Name, Head) orelse mentions(Name, Tail);
mentions(_, _) -> false.
