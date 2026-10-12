-module(spark_cli_http_ffi).
-export([fetch/4, read_body/1, drop_body/1]).

%% http/1.1 over a socket of its own for each request: enough for fetch.
%%
%% Each request has a process, which owns the socket and hands the body out a
%% piece at a time as it is asked for, so nothing is read ahead of the
%% script. A small registry gives each of those processes the number the
%% script knows the body by. There is no connection reuse and no proxy.

fetch(Method, Url, Headers, Body) ->
    Caller = self(),
    Owner = spawn(fun() -> request(Caller, Method, Url, Headers, Body) end),
    receive {Owner, Answer} -> Answer end.

request(Caller, Method, Url, Headers, Body) ->
    try
        Parts = uri_string:parse(Url),
        Scheme = maps:get(scheme, Parts),
        Host = maps:get(host, Parts),
        Port = maps:get(port, Parts, default_port(Scheme)),
        Socket = connect(Scheme, Host, Port),
        ok = send(Socket, [Method, " ", target(Parts), " HTTP/1.1\r\n",
                           lines(Scheme, Host, Port, Method, Headers, Body),
                           "\r\n", Body]),
        {Status, Reason, Received, Rest} = head(Socket, <<>>),
        Framing = framing(Method, Status, Received),
        Caller ! {self(), {ok, {head, Status, Reason, Received, register_body()}}},
        body(Socket, Framing, Rest)
    catch
        throw:{failed, Code} -> Caller ! {self(), {error, {failure, Code}}};
        error:_ -> Caller ! {self(), {error, {failure, <<"EINVAL">>}}}
    end.

default_port(<<"https">>) -> 443;
default_port(_) -> 80.

target(Parts) ->
    Path = case maps:get(path, Parts, <<>>) of
        <<>> -> <<"/">>;
        Given -> Given
    end,
    case maps:get(query, Parts, undefined) of
        undefined -> Path;
        Query -> [Path, "?", Query]
    end.

%% the headers as given, after the three the connection itself needs
lines(Scheme, Host, Port, Method, Headers, Body) ->
    Named = case Port =:= default_port(Scheme) of
        true -> Host;
        false -> [Host, ":", integer_to_binary(Port)]
    end,
    Sends = Body =/= <<>> orelse lists:member(Method, [<<"POST">>, <<"PUT">>, <<"PATCH">>]),
    Length = case Sends of
        true -> ["content-length: ", integer_to_binary(byte_size(Body)), "\r\n"];
        false -> []
    end,
    ["host: ", Named, "\r\n", "connection: close\r\n", Length,
     [[Name, ": ", Value, "\r\n"] || {Name, Value} <- Headers]].

%% ---- the socket, plain or tls

connect(Scheme, Host, Port) ->
    Name = binary_to_list(string:trim(Host, both, "[]")),
    Address = case inet:parse_address(Name) of
        {ok, Parsed} -> Parsed;
        {error, _} -> Name
    end,
    Options = [binary, {active, false}, {packet, raw}],
    case Scheme of
        <<"https">> ->
            {ok, _} = application:ensure_all_started(ssl),
            Tls = [{verify, verify_peer}, {cacerts, public_key:cacerts_get()},
                   {alpn_advertised_protocols, [<<"http/1.1">>]},
                   {customize_hostname_check,
                    [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]
                ++ [{server_name_indication, Name} || not is_tuple(Address)],
            case ssl:connect(Address, Port, Options ++ Tls, 30000) of
                {ok, Socket} -> {ssl, Socket};
                {error, Reason} -> throw({failed, failure(Reason)})
            end;
        _ ->
            case gen_tcp:connect(Address, Port, [{nodelay, true} | Options], 30000) of
                {ok, Socket} -> {tcp, Socket};
                {error, Reason} -> throw({failed, failure(Reason)})
            end
    end.

send({tcp, Socket}, Data) -> sent(gen_tcp:send(Socket, Data));
send({ssl, Socket}, Data) -> sent(ssl:send(Socket, Data)).

sent(ok) -> ok;
sent({error, Reason}) -> throw({failed, failure(Reason)}).

%% blocks until some bytes are in, or the other side has closed
recv({tcp, Socket}) -> gen_tcp:recv(Socket, 0);
recv({ssl, Socket}) -> ssl:recv(Socket, 0).

%% asks for the next bytes as one message, so a `drop` can come first
arm({tcp, Socket}) -> inet:setopts(Socket, [{active, once}]);
arm({ssl, Socket}) -> ssl:setopts(Socket, [{active, once}]).

close({tcp, Socket}) -> gen_tcp:close(Socket);
close({ssl, Socket}) -> ssl:close(Socket).

failure(econnrefused) -> <<"ECONNREFUSED">>;
failure(econnreset) -> <<"ECONNRESET">>;
failure(closed) -> <<"ECONNRESET">>;
failure(timeout) -> <<"ETIMEDOUT">>;
failure(etimedout) -> <<"ETIMEDOUT">>;
failure(nxdomain) -> <<"ENOTFOUND">>;
failure(ehostunreach) -> <<"EHOSTUNREACH">>;
failure(enetunreach) -> <<"ENETUNREACH">>;
failure(epipe) -> <<"EPIPE">>;
failure({tls_alert, _}) -> <<"EPROTO">>;
failure(Reason) when is_atom(Reason) ->
    list_to_binary(string:uppercase(atom_to_list(Reason)));
failure(_) -> <<"EIO">>.

%% ---- the status line and the headers

head(Socket, Buffer) ->
    case erlang:decode_packet(http_bin, Buffer, []) of
        {ok, {http_response, _Version, Status, Reason}, Rest} ->
            {Headers, After} = headers(Socket, Rest, []),
            %% a 1xx answer other than a protocol switch comes before the real one
            case Status >= 100 andalso Status < 200 andalso Status =/= 101 of
                true -> head(Socket, After);
                false -> {Status, Reason, Headers, After}
            end;
        {more, _} ->
            head(Socket, more(Socket, Buffer));
        _ ->
            throw({failed, <<"HPE_INVALID_CONSTANT">>})
    end.

headers(Socket, Buffer, Acc) ->
    case erlang:decode_packet(httph_bin, Buffer, []) of
        {ok, {http_header, _, _Field, Name, Value}, Rest} ->
            headers(Socket, Rest, [{Name, Value} | Acc]);
        {ok, http_eoh, Rest} ->
            {lists:reverse(Acc), Rest};
        {more, _} ->
            headers(Socket, more(Socket, Buffer), Acc);
        _ ->
            throw({failed, <<"HPE_INVALID_HEADER_TOKEN">>})
    end.

more(Socket, Buffer) ->
    case recv(Socket) of
        {ok, Data} -> <<Buffer/binary, Data/binary>>;
        {error, Reason} -> throw({failed, failure(Reason)})
    end.

%% how the body's end is told
framing(Method, Status, Headers) ->
    Lower = [{string:lowercase(Name), Value} || {Name, Value} <- Headers],
    Chunked = case proplists:get_value(<<"transfer-encoding">>, Lower) of
        undefined -> false;
        Encoding -> string:find(string:lowercase(Encoding), <<"chunked">>) =/= nomatch
    end,
    if
        Method =:= <<"HEAD">>; Status =:= 204; Status =:= 304; Status < 200 -> none;
        Chunked -> {chunked, size};
        true ->
            case proplists:get_value(<<"content-length">>, Lower) of
                undefined -> until_close;
                Text ->
                    try binary_to_integer(string:trim(Text)) of
                        Length -> {length, Length}
                    catch
                        error:badarg -> until_close
                    end
            end
    end.

%% ---- the body, a piece for each time it is asked for

body(Socket, none, _Buffer) ->
    close(Socket),
    ended();
body(Socket, Framing, Buffer) ->
    receive
        {read, From} -> piece(Socket, Framing, Buffer, From);
        drop -> close(Socket)
    end.

%% the body is over; say so to whoever still asks
ended() ->
    receive
        {read, From} -> From ! {self(), {ok, done}}, ended();
        drop -> ok
    after 60000 -> ok
    end.

piece(Socket, Framing, Buffer, From) ->
    case take(Framing, Buffer) of
        {bytes, Bytes, Next, Rest} ->
            From ! {self(), {ok, {bytes, Bytes}}},
            body(Socket, Next, Rest);
        done ->
            close(Socket),
            From ! {self(), {ok, done}},
            ended();
        more ->
            arm(Socket),
            receive
                {Kind, _, Data} when Kind =:= tcp; Kind =:= ssl ->
                    piece(Socket, Framing, <<Buffer/binary, Data/binary>>, From);
                {Closed, _} when Closed =:= tcp_closed; Closed =:= ssl_closed ->
                    close(Socket),
                    case Framing of
                        until_close -> From ! {self(), {ok, done}};
                        _ -> From ! {self(), {error, {failure, <<"ECONNRESET">>}}}
                    end,
                    ended();
                {Failed, _, Reason} when Failed =:= tcp_error; Failed =:= ssl_error ->
                    close(Socket),
                    From ! {self(), {error, {failure, failure(Reason)}}};
                drop ->
                    close(Socket),
                    From ! {self(), {ok, done}}
            end
    end.

%% what of the body the bytes in hand hold: a piece, its end, or not enough
take({length, 0}, _Buffer) ->
    done;
take({length, _}, <<>>) ->
    more;
take({length, Left}, Buffer) ->
    Size = min(Left, byte_size(Buffer)),
    <<Bytes:Size/binary, Rest/binary>> = Buffer,
    {bytes, Bytes, {length, Left - Size}, Rest};
take(until_close, <<>>) ->
    more;
take(until_close, Buffer) ->
    {bytes, Buffer, until_close, <<>>};
take({chunked, size}, Buffer) ->
    case binary:split(Buffer, <<"\r\n">>) of
        [Line, Rest] ->
            [Hex | _] = binary:split(Line, <<";">>),
            case binary_to_integer(string:trim(Hex), 16) of
                0 -> take({chunked, trailers}, Rest);
                Size -> take({chunked, {data, Size}}, Rest)
            end;
        [_] ->
            more
    end;
take({chunked, {data, _}}, <<>>) ->
    more;
take({chunked, {data, Left}}, Buffer) ->
    Size = min(Left, byte_size(Buffer)),
    <<Bytes:Size/binary, Rest/binary>> = Buffer,
    Next = case Left - Size of
        0 -> {chunked, ending};
        Still -> {chunked, {data, Still}}
    end,
    {bytes, Bytes, Next, Rest};
take({chunked, ending}, <<"\r\n", Rest/binary>>) ->
    take({chunked, size}, Rest);
take({chunked, ending}, _Buffer) ->
    more;
take({chunked, trailers}, Buffer) ->
    case binary:split(Buffer, <<"\r\n">>) of
        [<<>>, _Rest] -> done;
        [_Trailer, Rest] -> take({chunked, trailers}, Rest);
        [_] -> more
    end.

%% ---- for the script's side

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
