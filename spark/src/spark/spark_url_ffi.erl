%% the whatwg url standard's parser, serialiser and setters
%% https://url.spec.whatwg.org/
-module(spark_url_ffi).
-export([parse/2, can_parse/2, update/3, domain_to_ascii/1, domain_to_unicode/1,
         idna_to_ascii/1]).

%% host: null | empty | {domain, Bin} | {ipv4, Int} | {ipv6, [Piece]} | {opaque, Bin}
%% path: {opaque, Bin} | segments, last one first
-record(url, {scheme = <<>>, username = <<>>, password = <<>>, host = null,
              port = null, path = [], query = null, fragment = null}).

%% override: none, or the state a setter starts the parser in
-record(st, {base = null, override = none, all = [], at_seen = false,
             pass_seen = false, in_brackets = false}).

-define(IS_ALPHA(C), ((C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z))).
-define(IS_DIGIT(C), (C >= $0 andalso C =< $9)).
-define(IS_HEX(C), (?IS_DIGIT(C) orelse (C >= $a andalso C =< $f) orelse (C >= $A andalso C =< $F))).

%% {ok, {Href, Positions}} with the positions node's url class reads, or
%% {error, nil}. Base is `none` or `{some, String}`
parse(Input, Base) ->
    try
        BaseUrl = case Base of
            none -> null;
            {some, Text} -> basic_parse(Text, null)
        end,
        {ok, serialize(basic_parse(Input, BaseUrl))}
    catch
        throw:failure -> {error, nil}
    end.

can_parse(Input, Base) ->
    case parse(Input, Base) of
        {ok, _} -> true;
        {error, nil} -> false
    end.

%% a setter: Action is the part being set, Href a serialised url
update(Href, Action, Value) ->
    try
        Url = basic_parse(Href, null),
        {ok, serialize(set(Action, unicode:characters_to_list(Value), Url))}
    catch
        throw:failure -> {error, nil}
    end.

%% ---- setters

set(href, Value, _Url) ->
    basic_parse(unicode:characters_to_binary(Value), null);
set(protocol, Value, Url) ->
    override(scheme_start, Value ++ ":", Url);
set(username, Value, Url) ->
    case cannot_have_credentials(Url) of
        true -> Url;
        false -> Url#url{username = encode_all(Value, userinfo)}
    end;
set(password, Value, Url) ->
    case cannot_have_credentials(Url) of
        true -> Url;
        false -> Url#url{password = encode_all(Value, userinfo)}
    end;
set(host, _Value, #url{path = {opaque, _}} = Url) -> Url;
set(host, Value, Url) -> override(host, Value, Url);
set(hostname, _Value, #url{path = {opaque, _}} = Url) -> Url;
set(hostname, Value, Url) -> override(hostname, Value, Url);
set(port, Value, Url) ->
    case cannot_have_credentials(Url) of
        true -> Url;
        false when Value =:= [] -> Url#url{port = null};
        false -> override(port, Value, Url)
    end;
set(pathname, _Value, #url{path = {opaque, _}} = Url) -> Url;
set(pathname, Value, Url) -> override(path_start, Value, Url#url{path = []});
set(search, [], Url) -> Url#url{query = null};
set(search, [$? | Value], Url) -> override(query, Value, Url#url{query = <<>>});
set(search, Value, Url) -> override(query, Value, Url#url{query = <<>>});
set(hash, [], Url) -> Url#url{fragment = null};
set(hash, [$# | Value], Url) -> override(fragment, Value, Url);
set(hash, Value, Url) -> override(fragment, Value, Url).

cannot_have_credentials(#url{host = null}) -> true;
cannot_have_credentials(#url{host = empty}) -> true;
cannot_have_credentials(#url{scheme = <<"file">>}) -> true;
cannot_have_credentials(_) -> false.

override(State, Value, Url) ->
    Chars = strip_tab_newline(Value),
    St = #st{override = State, all = Chars},
    case State of
        scheme_start -> scheme_start(Chars, Url, St);
        host -> host(Chars, [], Url, St);
        hostname -> host(Chars, [], Url, St);
        port -> port(Chars, [], Url, St);
        path_start -> path_start(Chars, Url, St);
        query -> query(Chars, [], Url, St);
        fragment -> fragment(Chars, <<>>, Url, St)
    end.

%% ---- the basic url parser, one function per state

basic_parse(Input, Base) ->
    Chars = strip_tab_newline(trim(unicode:characters_to_list(Input))),
    scheme_start(Chars, #url{}, #st{base = Base, all = Chars}).

trim(Chars) ->
    lists:reverse(drop_c0_space(lists:reverse(drop_c0_space(Chars)))).

drop_c0_space([C | Rest]) when C =< 16#20 -> drop_c0_space(Rest);
drop_c0_space(Chars) -> Chars.

strip_tab_newline(Chars) ->
    [C || C <- Chars, C =/= $\t, C =/= $\n, C =/= $\r].

scheme_start([C | Rest], Url, St) when ?IS_ALPHA(C) ->
    scheme(Rest, [lower(C)], Url, St);
scheme_start(Rest, Url, #st{override = none} = St) ->
    no_scheme(Rest, Url, St);
scheme_start(_, _, _) ->
    throw(failure).

scheme([C | Rest], Buf, Url, St)
  when ?IS_ALPHA(C); ?IS_DIGIT(C); C =:= $+; C =:= $-; C =:= $. ->
    scheme(Rest, [lower(C) | Buf], Url, St);
scheme([$: | Rest], Buf, Url, #st{override = none} = St) ->
    after_scheme(Rest, Url#url{scheme = list_to_binary(lists:reverse(Buf))}, St);
scheme([$: | _], Buf, Url, _St) ->
    Scheme = list_to_binary(lists:reverse(Buf)),
    Blocked =
        special(Url#url.scheme) =/= special(Scheme)
        orelse (Scheme =:= <<"file">>
                andalso (has_credentials(Url) orelse Url#url.port =/= null))
        orelse (Url#url.scheme =:= <<"file">> andalso Url#url.host =:= empty),
    case Blocked of
        true -> Url;
        false ->
            case default_port(Scheme) of
                Port when Port =:= Url#url.port -> Url#url{scheme = Scheme, port = null};
                _ -> Url#url{scheme = Scheme}
            end
    end;
scheme(_, _, Url, #st{override = none, all = All} = St) ->
    no_scheme(All, Url, St);
scheme(_, _, _, _) ->
    throw(failure).

after_scheme(Rest, #url{scheme = <<"file">>} = Url, St) ->
    file(Rest, Url, St);
after_scheme(Rest, #url{scheme = Scheme} = Url, #st{base = Base} = St) ->
    case special(Scheme) of
        true when Base =/= null, Base#url.scheme =:= Scheme ->
            special_relative_or_authority(Rest, Url, St);
        true ->
            special_authority_slashes(Rest, Url, St);
        false ->
            case Rest of
                [$/ | After] -> path_or_authority(After, Url, St);
                _ -> opaque_path(Rest, <<>>, Url, St)
            end
    end.

no_scheme(_, _, #st{base = null}) ->
    throw(failure);
no_scheme([$# | Rest], Url, #st{base = #url{path = {opaque, _}} = Base} = St) ->
    fragment(Rest, <<>>,
             Url#url{scheme = Base#url.scheme, path = Base#url.path,
                     query = Base#url.query},
             St);
no_scheme(_, _, #st{base = #url{path = {opaque, _}}}) ->
    throw(failure);
no_scheme(Rest, Url, #st{base = #url{scheme = <<"file">>}} = St) ->
    file(Rest, Url, St);
no_scheme(Rest, Url, St) ->
    relative(Rest, Url, St).

special_relative_or_authority([$/, $/ | Rest], Url, St) ->
    special_authority_ignore_slashes(Rest, Url, St);
special_relative_or_authority(Rest, Url, St) ->
    relative(Rest, Url, St).

path_or_authority([$/ | Rest], Url, St) -> authority(Rest, [], Url, St);
path_or_authority(Rest, Url, St) -> path(Rest, [], Url, St).

relative(Rest, Url0, #st{base = Base} = St) ->
    Url = Url0#url{scheme = Base#url.scheme},
    Special = special(Url#url.scheme),
    case Rest of
        [$/ | After] -> relative_slash(After, Url, St);
        [$\\ | After] when Special -> relative_slash(After, Url, St);
        _ ->
            Same = Url#url{username = Base#url.username, password = Base#url.password,
                           host = Base#url.host, port = Base#url.port,
                           path = Base#url.path, query = Base#url.query},
            case Rest of
                [$? | After] -> query(After, [], Same#url{query = <<>>}, St);
                [$# | After] -> fragment(After, <<>>, Same, St);
                [] -> Same;
                _ -> path(Rest, [], shorten(Same#url{query = null}), St)
            end
    end.

relative_slash(Rest, Url, #st{base = Base} = St) ->
    Special = special(Url#url.scheme),
    case Rest of
        [C | After] when Special, (C =:= $/ orelse C =:= $\\) ->
            special_authority_ignore_slashes(After, Url, St);
        [$/ | After] ->
            authority(After, [], Url, St);
        _ ->
            path(Rest, [],
                 Url#url{username = Base#url.username, password = Base#url.password,
                         host = Base#url.host, port = Base#url.port},
                 St)
    end.

special_authority_slashes([$/, $/ | Rest], Url, St) ->
    special_authority_ignore_slashes(Rest, Url, St);
special_authority_slashes(Rest, Url, St) ->
    special_authority_ignore_slashes(Rest, Url, St).

special_authority_ignore_slashes([C | Rest], Url, St) when C =:= $/; C =:= $\\ ->
    special_authority_ignore_slashes(Rest, Url, St);
special_authority_ignore_slashes(Rest, Url, St) ->
    authority(Rest, [], Url, St).

%% Buf is what has been read since the last '@', newest first
authority([$@ | Rest], Buf, Url, St) ->
    Seen = case St#st.at_seen of
        true -> "%40" ++ lists:reverse(Buf);
        false -> lists:reverse(Buf)
    end,
    {Url1, PassSeen} = credentials(Seen, Url, St#st.pass_seen),
    authority(Rest, [], Url1, St#st{at_seen = true, pass_seen = PassSeen});
authority(Rest, Buf, Url, St) ->
    case ends_authority(Rest, special(Url#url.scheme)) of
        true when St#st.at_seen, Buf =:= [] -> throw(failure);
        true -> host(lists:reverse(Buf, Rest), [], Url, St);
        false ->
            [C | After] = Rest,
            authority(After, [C | Buf], Url, St)
    end.

credentials([], Url, PassSeen) -> {Url, PassSeen};
credentials([$: | Rest], Url, false) -> credentials(Rest, Url, true);
credentials([C | Rest], Url, false) ->
    Enc = list_to_binary(encode(C, userinfo)),
    credentials(Rest, Url#url{username = <<(Url#url.username)/binary, Enc/binary>>}, false);
credentials([C | Rest], Url, true) ->
    Enc = list_to_binary(encode(C, userinfo)),
    credentials(Rest, Url#url{password = <<(Url#url.password)/binary, Enc/binary>>}, true).

%% end of input, '/', '?', '#', and '\' in a special url
ends_authority([], _) -> true;
ends_authority([C | _], _) when C =:= $/; C =:= $?; C =:= $# -> true;
ends_authority([$\\ | _], true) -> true;
ends_authority(_, _) -> false.

%% the host and hostname states
host(Rest, Buf, #url{scheme = <<"file">>} = Url, #st{override = Override} = St)
  when Override =/= none ->
    file_host(Rest, Buf, Url, St);
host([$: | Rest], Buf, Url, #st{in_brackets = false} = St) ->
    case Buf =:= [] orelse St#st.override =:= hostname of
        true -> throw(failure);
        false ->
            Host = parse_host(lists:reverse(Buf), not special(Url#url.scheme)),
            port(Rest, [], Url#url{host = Host}, St)
    end;
host(Rest, Buf, Url, #st{override = Override} = St) ->
    Special = special(Url#url.scheme),
    case ends_authority(Rest, Special) of
        true when Special, Buf =:= [] ->
            throw(failure);
        true when Override =/= none, Buf =:= [] ->
            case has_credentials(Url) orelse Url#url.port =/= null of
                true -> Url;
                false -> host_done(Rest, Buf, Url, St)
            end;
        true ->
            host_done(Rest, Buf, Url, St);
        false ->
            [C | After] = Rest,
            InBrackets = case C of
                $[ -> true;
                $] -> false;
                _ -> St#st.in_brackets
            end,
            host(After, [C | Buf], Url, St#st{in_brackets = InBrackets})
    end.

host_done(Rest, Buf, Url, #st{override = Override} = St) ->
    Host = parse_host(lists:reverse(Buf), not special(Url#url.scheme)),
    case Override of
        none -> path_start(Rest, Url#url{host = Host}, St);
        _ -> Url#url{host = Host}
    end.

port([C | Rest], Buf, Url, St) when ?IS_DIGIT(C) ->
    port(Rest, [C | Buf], Url, St);
%% a setter that fails here keeps what it had already changed, the host
port(Rest, Buf, Url, #st{override = none} = St) ->
    ends_authority(Rest, special(Url#url.scheme)) orelse throw(failure),
    case Buf of
        [] -> path_start(Rest, Url, St);
        _ ->
            case with_port(Buf, Url) of
                failure -> throw(failure);
                Url1 -> path_start(Rest, Url1, St)
            end
    end;
port(_Rest, [], Url, _St) ->
    Url;
port(_Rest, Buf, Url, _St) ->
    case with_port(Buf, Url) of
        failure -> Url;
        Url1 -> Url1
    end.

with_port(Buf, Url) ->
    case list_to_integer(lists:reverse(Buf)) of
        Port when Port > 65535 -> failure;
        Port ->
            case default_port(Url#url.scheme) of
                Port -> Url#url{port = null};
                _ -> Url#url{port = Port}
            end
    end.

file(Rest, Url0, #st{base = Base} = St) ->
    Url = Url0#url{scheme = <<"file">>, host = empty},
    case Rest of
        [C | After] when C =:= $/; C =:= $\\ ->
            file_slash(After, Url, St);
        _ when Base =/= null, Base#url.scheme =:= <<"file">> ->
            Same = Url#url{host = Base#url.host, path = Base#url.path,
                           query = Base#url.query},
            case Rest of
                [$? | After] -> query(After, [], Same#url{query = <<>>}, St);
                [$# | After] -> fragment(After, <<>>, Same, St);
                [] -> Same;
                _ ->
                    NoQuery = Same#url{query = null},
                    Next = case starts_with_drive_letter(Rest) of
                        true -> NoQuery#url{path = []};
                        false -> shorten(NoQuery)
                    end,
                    path(Rest, [], Next, St)
            end;
        _ ->
            path(Rest, [], Url, St)
    end.

file_slash([C | Rest], Url, St) when C =:= $/; C =:= $\\ ->
    file_host(Rest, [], Url, St);
file_slash(Rest, Url, #st{base = #url{scheme = <<"file">>} = Base} = St) ->
    Url1 = Url#url{host = Base#url.host},
    Url2 = case starts_with_drive_letter(Rest) of
        true -> Url1;
        false ->
            case first_segment(Base#url.path) of
                {ok, First} ->
                    case is_normalized_drive_letter(First) of
                        true -> Url1#url{path = [First]};
                        false -> Url1
                    end;
                none -> Url1
            end
    end,
    path(Rest, [], Url2, St);
file_slash(Rest, Url, St) ->
    path(Rest, [], Url, St).

first_segment({opaque, _}) -> none;
first_segment([]) -> none;
first_segment(Segments) -> {ok, lists:last(Segments)}.

file_host(Rest, Buf, Url, #st{override = Override} = St) ->
    case Rest of
        [C | After] when C =/= $/, C =/= $\\, C =/= $?, C =/= $# ->
            file_host(After, [C | Buf], Url, St);
        _ ->
            Seen = lists:reverse(Buf),
            case Override =:= none andalso is_drive_letter(Seen) of
                true ->
                    %% not a host after all: the path state takes the buffer
                    path(Rest, Buf, Url, St);
                false when Buf =:= [] ->
                    case Override of
                        none -> path_start(Rest, Url#url{host = empty}, St);
                        _ -> Url#url{host = empty}
                    end;
                false ->
                    Host = case parse_host(Seen, false) of
                        {domain, <<"localhost">>} -> empty;
                        Parsed -> Parsed
                    end,
                    case Override of
                        none -> path_start(Rest, Url#url{host = Host}, St);
                        _ -> Url#url{host = Host}
                    end
            end
    end.

path_start(Rest, Url, #st{override = Override} = St) ->
    Special = special(Url#url.scheme),
    case Rest of
        [C | After] when Special, (C =:= $/ orelse C =:= $\\) -> path(After, [], Url, St);
        _ when Special -> path(Rest, [], Url, St);
        [$? | After] when Override =:= none -> query(After, [], Url#url{query = <<>>}, St);
        [$# | After] when Override =:= none -> fragment(After, <<>>, Url, St);
        [$/ | After] -> path(After, [], Url, St);
        [_ | _] -> path(Rest, [], Url, St);
        [] when Override =/= none, Url#url.host =:= null -> push(Url, <<>>);
        [] -> Url
    end.

%% Buf is the segment being read, already percent-encoded, newest first
path(Rest, Buf, Url, #st{override = Override} = St) ->
    Special = special(Url#url.scheme),
    Stop = case Rest of
        [] -> true;
        [$/ | _] -> true;
        [$\\ | _] -> Special;
        [C | _] when C =:= $?; C =:= $# -> Override =:= none;
        _ -> false
    end,
    case Stop of
        false ->
            [Char | After] = Rest,
            path(After, lists:reverse(encode(Char, path), Buf), Url, St);
        true ->
            Slash = case Rest of
                [$/ | _] -> true;
                [$\\ | _] -> Special;
                _ -> false
            end,
            Segment = lists:reverse(Buf),
            Url1 = case segment_kind(Segment) of
                double when Slash -> shorten(Url);
                double -> push(shorten(Url), <<>>);
                single when Slash -> Url;
                single -> push(Url, <<>>);
                plain ->
                    Fixed = case Url#url.scheme =:= <<"file">>
                                 andalso Url#url.path =:= []
                                 andalso is_drive_letter(Segment) of
                        true -> [hd(Segment), $:];
                        false -> Segment
                    end,
                    push(Url, list_to_binary(Fixed))
            end,
            case Rest of
                [] -> Url1;
                [$? | After] when Override =:= none ->
                    query(After, [], Url1#url{query = <<>>}, St);
                [$# | After] when Override =:= none ->
                    fragment(After, <<>>, Url1, St);
                [_ | After] -> path(After, [], Url1, St)
            end
    end.

segment_kind(Segment) ->
    case [lower(C) || C <- Segment] of
        "." -> single;
        "%2e" -> single;
        ".." -> double;
        ".%2e" -> double;
        "%2e." -> double;
        "%2e%2e" -> double;
        _ -> plain
    end.

push(#url{path = Segments} = Url, Segment) -> Url#url{path = [Segment | Segments]}.

shorten(#url{scheme = <<"file">>, path = [Only]} = Url) ->
    case is_normalized_drive_letter(Only) of
        true -> Url;
        false -> Url#url{path = []}
    end;
shorten(#url{path = [_ | Rest]} = Url) -> Url#url{path = Rest};
shorten(Url) -> Url.

opaque_path([$? | Rest], Acc, Url, St) ->
    query(Rest, [], Url#url{path = {opaque, Acc}, query = <<>>}, St);
opaque_path([$# | Rest], Acc, Url, St) ->
    fragment(Rest, <<>>, Url#url{path = {opaque, Acc}}, St);
opaque_path([$\s, Next | Rest], Acc, Url, St) when Next =:= $?; Next =:= $# ->
    opaque_path([Next | Rest], <<Acc/binary, "%20">>, Url, St);
opaque_path([$\s | Rest], Acc, Url, St) ->
    opaque_path(Rest, <<Acc/binary, $\s>>, Url, St);
opaque_path([C | Rest], Acc, Url, St) ->
    Enc = list_to_binary(encode(C, c0)),
    opaque_path(Rest, <<Acc/binary, Enc/binary>>, Url, St);
opaque_path([], Acc, Url, _St) ->
    Url#url{path = {opaque, Acc}}.

query([$# | Rest], Buf, Url, #st{override = none} = St) ->
    fragment(Rest, <<>>, add_query(Buf, Url), St);
query([], Buf, Url, _St) ->
    add_query(Buf, Url);
query([C | Rest], Buf, Url, St) ->
    query(Rest, [C | Buf], Url, St).

add_query(Buf, #url{query = Query} = Url) ->
    Set = case special(Url#url.scheme) of
        true -> special_query;
        false -> query
    end,
    Enc = encode_all(lists:reverse(Buf), Set),
    Url#url{query = <<Query/binary, Enc/binary>>}.

fragment([C | Rest], Acc, Url, St) ->
    Enc = list_to_binary(encode(C, fragment)),
    fragment(Rest, <<Acc/binary, Enc/binary>>, Url, St);
fragment([], Acc, Url, _St) ->
    Url#url{fragment = Acc}.

%% ---- schemes, drive letters

special(<<"http">>) -> true;
special(<<"https">>) -> true;
special(<<"ws">>) -> true;
special(<<"wss">>) -> true;
special(<<"ftp">>) -> true;
special(<<"file">>) -> true;
special(_) -> false.

default_port(<<"http">>) -> 80;
default_port(<<"https">>) -> 443;
default_port(<<"ws">>) -> 80;
default_port(<<"wss">>) -> 443;
default_port(<<"ftp">>) -> 21;
default_port(_) -> none.

has_credentials(#url{username = <<>>, password = <<>>}) -> false;
has_credentials(_) -> true.

lower(C) when C >= $A, C =< $Z -> C + 32;
lower(C) -> C.

is_drive_letter([A, B]) when ?IS_ALPHA(A), (B =:= $: orelse B =:= $|) -> true;
is_drive_letter(_) -> false.

is_normalized_drive_letter(<<A, $:>>) when ?IS_ALPHA(A) -> true;
is_normalized_drive_letter(_) -> false.

starts_with_drive_letter([A, B]) when ?IS_ALPHA(A), (B =:= $: orelse B =:= $|) -> true;
starts_with_drive_letter([A, B, C | _])
  when ?IS_ALPHA(A), (B =:= $: orelse B =:= $|),
       (C =:= $/ orelse C =:= $\\ orelse C =:= $? orelse C =:= $#) -> true;
starts_with_drive_letter(_) -> false.

%% ---- percent-encoding

encode_all(Chars, Set) ->
    list_to_binary([encode(C, Set) || C <- Chars]).

encode(C, Set) ->
    case in_set(C, Set) of
        false -> [C];
        true -> lists:append([percent(Byte) || <<Byte>> <= <<C/utf8>>])
    end.

percent(Byte) -> [$%, hex_digit(Byte bsr 4), hex_digit(Byte band 15)].

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $A + N - 10.

in_set(C, _) when C < 16#20; C > 16#7E -> true;
in_set(_, c0) -> false;
in_set(C, fragment) -> lists:member(C, " \"<>`");
in_set(C, query) -> lists:member(C, " \"#<>");
in_set($', special_query) -> true;
in_set(C, special_query) -> in_set(C, query);
in_set(C, path) -> in_set(C, query) orelse lists:member(C, "?^`{}");
in_set(C, userinfo) -> in_set(C, path) orelse lists:member(C, "/:;=@[\\]|").

percent_decode([$%, A, B | Rest]) when ?IS_HEX(A), ?IS_HEX(B) ->
    [list_to_integer([A, B], 16) | percent_decode(Rest)];
percent_decode([C | Rest]) -> binary_to_list(<<C/utf8>>) ++ percent_decode(Rest);
percent_decode([]) -> [].

%% ---- hosts

parse_host([$[ | Rest], _Opaque) ->
    case Rest =/= [] andalso lists:last(Rest) =:= $] of
        true -> {ipv6, parse_ipv6(lists:droplast(Rest))};
        false -> throw(failure)
    end;
parse_host([], true) ->
    empty;
parse_host(Input, true) ->
    lists:any(fun forbidden_host/1, Input) andalso throw(failure),
    {opaque, encode_all(Input, c0)};
parse_host(Input, false) ->
    Domain = case unicode:characters_to_list(list_to_binary(percent_decode(Input))) of
        Chars when is_list(Chars) -> Chars;
        _ -> throw(failure)
    end,
    Ascii = to_ascii(Domain),
    case ends_in_number(Ascii) of
        true -> {ipv4, parse_ipv4(Ascii)};
        false -> {domain, list_to_binary(Ascii)}
    end.

forbidden_host(C) ->
    lists:member(C, [0, $\t, $\n, $\r, $\s, $#, $/, $:, $<, $>, $?, $@, $[, $\\, $], $^, $|]).

forbidden_domain(C) ->
    C =< 16#20 orelse C =:= $% orelse C =:= 16#7F orelse forbidden_host(C).

%% domain to ascii. A name that is all ascii is only lower-cased, encoded
%% labels and all, which is what node does. For names beyond ascii this
%% follows uts 46 only roughly: compatibility mapping, lower case and
%% punycode, without the full mapping table or the bidi and joiner checks
to_ascii(Domain) ->
    Ascii = case lists:all(fun(C) -> C < 128 end, Domain) of
        true -> [lower(C) || C <- Domain];
        false ->
            Labels = [ascii_label(Label) || Label <- split_dots(map_unicode(Domain))],
            lists:append(lists:join(".", Labels))
    end,
    (Ascii =:= [] orelse lists:any(fun forbidden_domain/1, Ascii)) andalso throw(failure),
    Ascii.

map_unicode(Domain) ->
    lists:member(16#FFFD, Domain) andalso throw(failure),
    Kept = [dot(C) || C <- Domain, not ignored(C)],
    Folded = string:lowercase(unicode:characters_to_nfkc_list(Kept)),
    [dot(C) || C <- unicode:characters_to_nfkc_list(Folded)].

dot(16#3002) -> $.;
dot(16#FF0E) -> $.;
dot(16#FF61) -> $.;
dot(C) -> C.

ignored(16#00AD) -> true;
ignored(16#034F) -> true;
ignored(16#200B) -> true;
ignored(16#2060) -> true;
ignored(16#FEFF) -> true;
ignored(C) when C >= 16#FE00, C =< 16#FE0F -> true;
ignored(_) -> false.

split_dots(Chars) -> split_dots(Chars, [], []).

split_dots([$. | Rest], Label, Labels) ->
    split_dots(Rest, [], [lists:reverse(Label) | Labels]);
split_dots([C | Rest], Label, Labels) ->
    split_dots(Rest, [C | Label], Labels);
split_dots([], Label, Labels) ->
    lists:reverse([lists:reverse(Label) | Labels]).

ascii_label("xn--" ++ Encoded = Label) ->
    Decoded = punycode_decode(Encoded),
    %% it has to hold something beyond ascii, in the form mapping leaves alone
    Valid = Decoded =/= []
        andalso lists:any(fun(C) -> C >= 128 end, Decoded)
        andalso map_unicode(Decoded) =:= Decoded,
    Valid orelse throw(failure),
    Label;
ascii_label(Label) ->
    case lists:all(fun(C) -> C < 128 end, Label) of
        true -> Label;
        false -> "xn--" ++ punycode_encode(Label)
    end.

%% uts 46 to-ascii alone, the empty string when the name is not valid
idna_to_ascii(Domain) ->
    try unicode:characters_to_binary(to_ascii(unicode:characters_to_list(Domain)))
    catch throw:failure -> <<>>
    end.

domain_to_ascii(Domain) ->
    case parse(<<"ws://", Domain/binary>>, none) of
        {ok, {Href, [_, _, HostStart, HostEnd | _]}} ->
            binary:part(Href, HostStart, HostEnd - HostStart);
        {error, nil} -> <<>>
    end.

domain_to_unicode(Domain) ->
    Ascii = unicode:characters_to_list(domain_to_ascii(Domain)),
    Labels = [unicode_label(Label) || Label <- split_dots(Ascii)],
    unicode:characters_to_binary(lists:join(".", Labels)).

unicode_label("xn--" ++ Encoded = Label) ->
    try punycode_decode(Encoded)
    catch throw:failure -> Label
    end;
unicode_label(Label) -> Label.

%% the last label is a number in some base
ends_in_number(Ascii) ->
    Parts = case lists:reverse(split_dots(Ascii)) of
        [[], Last | _] -> {ok, Last};
        [[]] -> none;
        [Last | _] -> {ok, Last}
    end,
    case Parts of
        none -> false;
        {ok, []} -> false;
        {ok, Part} ->
            lists:all(fun(C) -> ?IS_DIGIT(C) end, Part)
                orelse ipv4_number(Part) =/= failure
    end.

parse_ipv4(Ascii) ->
    Parts0 = split_dots(Ascii),
    Parts = case lists:reverse(Parts0) of
        [[] | Rest] when Rest =/= [] -> lists:reverse(Rest);
        _ -> Parts0
    end,
    length(Parts) > 4 andalso throw(failure),
    Numbers = [case ipv4_number(Part) of
                   failure -> throw(failure);
                   N -> N
               end || Part <- Parts],
    {Init, [Last]} = lists:split(length(Numbers) - 1, Numbers),
    lists:any(fun(N) -> N > 255 end, Init) andalso throw(failure),
    Last >= (1 bsl (8 * (5 - length(Numbers)))) andalso throw(failure),
    {Sum, _} = lists:foldl(fun(N, {Acc, I}) -> {Acc + (N bsl (8 * (3 - I))), I + 1} end,
                           {Last, 0}, Init),
    Sum.

ipv4_number([]) -> failure;
ipv4_number([$0, X | Rest]) when X =:= $x; X =:= $X -> digits(Rest, 16);
ipv4_number([$0, _ | _] = Part) -> digits(tl(Part), 8);
ipv4_number(Part) -> digits(Part, 10).

digits([], _) -> 0;
digits(Part, Radix) ->
    try list_to_integer(Part, Radix) of
        N when N >= 0 ->
            %% list_to_integer takes a sign, a number here cannot have one
            case hd(Part) of
                $+ -> failure;
                $- -> failure;
                _ -> N
            end;
        _ -> failure
    catch
        error:badarg -> failure
    end.

%% eight pieces, each 0..65535
parse_ipv6([$:, $: | Rest]) -> ipv6(Rest, [], 1, 0);
parse_ipv6([$: | _]) -> throw(failure);
parse_ipv6(Input) -> ipv6(Input, [], 0, none).

%% Pieces newest first. Count is the standard's piece index, which also
%% counts a "::" as one. Compress is how many pieces came before the "::"
ipv6([], Pieces, _Count, Compress) ->
    ipv6_done(lists:reverse(Pieces), Compress);
ipv6(_, _, 8, _) ->
    throw(failure);
ipv6([$: | Rest], Pieces, Count, none) ->
    ipv6(Rest, Pieces, Count + 1, length(Pieces));
ipv6([$: | _], _, _, _) ->
    throw(failure);
ipv6(Input, Pieces, Count, Compress) ->
    {Value, Length, Rest} = hex_piece(Input, 0, 0),
    case Rest of
        [$. | _] when Length =:= 0 -> throw(failure);
        [$. | _] when Count > 6 -> throw(failure);
        [$. | _] ->
            {High, Low} = ipv4_in_ipv6(Input),
            ipv6_done(lists:reverse([Low, High | Pieces]), Compress);
        [$:] -> throw(failure);
        [$: | After] -> ipv6(After, [Value | Pieces], Count + 1, Compress);
        [] -> ipv6([], [Value | Pieces], Count + 1, Compress);
        _ -> throw(failure)
    end.

hex_piece([C | Rest], Value, Length) when Length < 4, ?IS_HEX(C) ->
    hex_piece(Rest, Value * 16 + list_to_integer([C], 16), Length + 1);
hex_piece(Rest, Value, Length) ->
    {Value, Length, Rest}.

%% four decimal numbers with dots, no leading zeros, as two pieces
ipv4_in_ipv6(Input) ->
    case [ipv4_piece(Part) || Part <- split_dots(Input)] of
        [A, B, C, D] -> {A * 256 + B, C * 256 + D};
        _ -> throw(failure)
    end.

ipv4_piece([]) -> throw(failure);
ipv4_piece([$0, _ | _]) -> throw(failure);
ipv4_piece(Part) ->
    lists:all(fun(C) -> ?IS_DIGIT(C) end, Part) orelse throw(failure),
    case list_to_integer(Part) of
        N when N =< 255 -> N;
        _ -> throw(failure)
    end.

ipv6_done(Pieces, none) when length(Pieces) =:= 8 -> Pieces;
ipv6_done(_, none) -> throw(failure);
ipv6_done(Pieces, _) when length(Pieces) >= 8 -> throw(failure);
ipv6_done(Pieces, Compress) ->
    {Before, After} = lists:split(Compress, Pieces),
    Before ++ lists:duplicate(8 - length(Pieces), 0) ++ After.

%% ---- punycode, rfc 3492

-define(BASE, 36).
-define(TMIN, 1).
-define(TMAX, 26).
-define(SKEW, 38).
-define(DAMP, 700).

punycode_encode(Input) ->
    Basic = [C || C <- Input, C < 128],
    Handled = length(Basic),
    Out = case Basic of
        [] -> [];
        _ -> [$- | lists:reverse(Basic)]
    end,
    puny_enc(Input, 128, 0, 72, Handled, Handled, Out).

%% Out is the result so far, newest first
puny_enc(Input, _N, _Delta, _Bias, _Basic, Handled, Out) when Handled >= length(Input) ->
    lists:reverse(Out);
puny_enc(Input, N, Delta0, Bias0, Basic, Handled0, Out0) ->
    M = lists:min([C || C <- Input, C >= N]),
    Delta1 = Delta0 + (M - N) * (Handled0 + 1),
    {Delta, Bias, Handled, Out} =
        lists:foldl(
            fun(C, {D, B, H, O}) when C < M -> {D + 1, B, H, O};
               (C, {D, B, H, O}) when C =:= M ->
                    O1 = puny_digits(D, ?BASE, B, O),
                    {0, adapt(D, H + 1, H =:= Basic), H + 1, O1};
               (_, Acc) -> Acc
            end,
            {Delta1, Bias0, Handled0, Out0}, Input),
    puny_enc(Input, M + 1, Delta + 1, Bias, Basic, Handled, Out).

puny_digits(Q, K, Bias, Out) ->
    T = threshold(K, Bias),
    case Q < T of
        true -> [puny_digit(Q) | Out];
        false ->
            puny_digits((Q - T) div (?BASE - T), K + ?BASE, Bias,
                        [puny_digit(T + (Q - T) rem (?BASE - T)) | Out])
    end.

threshold(K, Bias) when K =< Bias + ?TMIN -> ?TMIN;
threshold(K, Bias) when K >= Bias + ?TMAX -> ?TMAX;
threshold(K, Bias) -> K - Bias.

puny_digit(D) when D < 26 -> $a + D;
puny_digit(D) -> $0 + D - 26.

adapt(Delta, Points, First) ->
    D0 = case First of
        true -> Delta div ?DAMP;
        false -> Delta div 2
    end,
    adapt_loop(D0 + D0 div Points, 0).

adapt_loop(Delta, K) when Delta > ((?BASE - ?TMIN) * ?TMAX) div 2 ->
    adapt_loop(Delta div (?BASE - ?TMIN), K + ?BASE);
adapt_loop(Delta, K) ->
    K + ((?BASE - ?TMIN + 1) * Delta) div (Delta + ?SKEW).

punycode_decode(Input) ->
    {Basic, Rest} = case string:rchr(Input, $-) of
        0 -> {[], Input};
        At -> {lists:sublist(Input, At - 1), lists:nthtail(At, Input)}
    end,
    lists:all(fun(C) -> C < 128 end, Basic) orelse throw(failure),
    puny_dec(Rest, Basic, 128, 0, 72).

puny_dec([], Out, _N, _I, _Bias) -> Out;
puny_dec(Input, Out, N0, I0, Bias0) ->
    {I1, Rest} = puny_number(Input, I0, 1, ?BASE, Bias0),
    Length = length(Out) + 1,
    Bias = adapt(I1 - I0, Length, I0 =:= 0),
    N = N0 + I1 div Length,
    (N > 16#10FFFF orelse (N >= 16#D800 andalso N =< 16#DFFF)) andalso throw(failure),
    I = I1 rem Length,
    {Before, After} = lists:split(I, Out),
    puny_dec(Rest, Before ++ [N | After], N, I + 1, Bias).

puny_number([], _I, _W, _K, _Bias) -> throw(failure);
puny_number([C | Rest], I, W, K, Bias) ->
    Digit = if
        C >= $a, C =< $z -> C - $a;
        C >= $A, C =< $Z -> C - $A;
        C >= $0, C =< $9 -> C - $0 + 26;
        true -> throw(failure)
    end,
    I1 = I + Digit * W,
    T = threshold(K, Bias),
    case Digit < T of
        true -> {I1, Rest};
        false -> puny_number(Rest, I1, W * (?BASE - T), K + ?BASE, Bias)
    end.

%% ---- serialising, with the positions node's url class reads

-define(OMITTED, 16#FFFFFFFF).

serialize(#url{scheme = Scheme, host = Host} = Url) ->
    ProtocolEnd = byte_size(Scheme) + 1,
    Head = <<Scheme/binary, $:>>,
    {Authority, UsernameEnd, HostStart, HostEnd} = case Host of
        null -> {<<>>, ProtocolEnd, ProtocolEnd, ProtocolEnd};
        _ ->
            #url{username = User, password = Pass} = Url,
            HostText = host_text(Host),
            UserEnd = ProtocolEnd + 2 + byte_size(User),
            {Credentials, At} = case {User, Pass} of
                {<<>>, <<>>} -> {<<>>, UserEnd};
                {_, <<>>} -> {<<User/binary, $@>>, UserEnd};
                _ -> {<<User/binary, $:, Pass/binary, $@>>, UserEnd + 1 + byte_size(Pass)}
            end,
            NameStart = ProtocolEnd + 2 + byte_size(Credentials),
            PortText = case Url#url.port of
                null -> <<>>;
                Port -> <<$:, (integer_to_binary(Port))/binary>>
            end,
            {<<"//", Credentials/binary, HostText/binary, PortText/binary>>,
             UserEnd, At, NameStart + byte_size(HostText)}
    end,
    %% a path that would read as a host gets "/." in front
    Guard = case Url of
        #url{host = null, path = [_, _ | _] = Segments} ->
            case lists:last(Segments) of
                <<>> -> <<"/.">>;
                _ -> <<>>
            end;
        _ -> <<>>
    end,
    PathText = case Url#url.path of
        {opaque, Opaque} -> Opaque;
        Segments1 -> << <<$/, Segment/binary>> || Segment <- lists:reverse(Segments1) >>
    end,
    PathStart = ProtocolEnd + byte_size(Authority) + byte_size(Guard),
    {QueryText, SearchStart} = case Url#url.query of
        null -> {<<>>, ?OMITTED};
        Query -> {<<$?, Query/binary>>, PathStart + byte_size(PathText)}
    end,
    {FragmentText, HashStart} = case Url#url.fragment of
        null -> {<<>>, ?OMITTED};
        Fragment ->
            {<<$#, Fragment/binary>>,
             PathStart + byte_size(PathText) + byte_size(QueryText)}
    end,
    Href = <<Head/binary, Authority/binary, Guard/binary, PathText/binary,
             QueryText/binary, FragmentText/binary>>,
    PortNumber = case Url#url.port of
        null -> ?OMITTED;
        P -> P
    end,
    {Href, [ProtocolEnd, UsernameEnd, HostStart, HostEnd, PortNumber, PathStart,
            SearchStart, HashStart, scheme_type(Scheme)]}.

scheme_type(<<"http">>) -> 0;
scheme_type(<<"https">>) -> 2;
scheme_type(<<"ws">>) -> 3;
scheme_type(<<"ftp">>) -> 4;
scheme_type(<<"wss">>) -> 5;
scheme_type(<<"file">>) -> 6;
scheme_type(_) -> 1.

host_text(empty) -> <<>>;
host_text({domain, Domain}) -> Domain;
host_text({opaque, Opaque}) -> Opaque;
host_text({ipv4, N}) ->
    list_to_binary(io_lib:format("~b.~b.~b.~b",
                                 [N bsr 24, (N bsr 16) band 255, (N bsr 8) band 255, N band 255]));
host_text({ipv6, Pieces}) ->
    {Start, Length} = longest_zero_run(Pieces, 0, {none, 0}, {none, 0}),
    Text = ipv6_text(Pieces, 0, Start, Length),
    <<$[, (list_to_binary(Text))/binary, $]>>.

%% the first of the longest runs of zero pieces, when longer than one
longest_zero_run([0 | Rest], Index, {none, 0}, Best) ->
    longest_zero_run(Rest, Index + 1, {Index, 1}, Best);
longest_zero_run([0 | Rest], Index, {Start, Length}, Best) ->
    longest_zero_run(Rest, Index + 1, {Start, Length + 1}, Best);
longest_zero_run([_ | Rest], Index, Current, Best) ->
    longest_zero_run(Rest, Index + 1, {none, 0}, longer(Current, Best));
longest_zero_run([], _Index, Current, Best) ->
    case longer(Current, Best) of
        {Start, Length} when Length > 1 -> {Start, Length};
        _ -> {none, 0}
    end.

longer({_, A} = Current, {_, B}) when A > B -> Current;
longer(_, Best) -> Best.

ipv6_text([], _Index, _Start, _Length) -> [];
ipv6_text(Pieces, Index, Index, Length) ->
    Sep = case Index of
        0 -> "::";
        _ -> ":"
    end,
    Sep ++ ipv6_text(lists:nthtail(Length, Pieces), Index + Length, none, 0);
ipv6_text([Piece | Rest], Index, Start, Length) ->
    Hex = string:lowercase(integer_to_list(Piece, 16)),
    case Index of
        7 -> Hex;
        _ -> Hex ++ ":" ++ ipv6_text(Rest, Index + 1, Start, Length)
    end.
