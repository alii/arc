%% rt/prop_map: an object's own string-keyed properties.
%%
%% #{Slot => {Text, Value}}. A name's slot is its key's id, a small integer,
%% which the vm finds in a map far faster than it finds a text. Two names can
%% share an id. The second to arrive goes to the next free slot four further on
%% (the two low bits of an id say what kind it is), as {Text, Value, Id}, three
%% parts so that nobody takes the slot for its id. So:
%%   nothing at a name's id    -> the object does not have it
%%   its own text there        -> found
%%   anything else there       -> keep looking four further on
%% The kernels elsewhere only handle the first two and answer miss on the third.
%% An index n has slot -1 - n, which no id is, and an empty text.
-module(arc_rt_prop_map_ffi).
-export([get/2, has_key/2, insert/3, delete/2, to_list/1, keys/1, values/1, fold/3,
         map_values/2, filter/2, from_list/1, key_at/2]).

-include("arc_rt_layout.hrl").

get(Props, {?KEY_INDEX, N}) ->
    case Props of
        #{?INDEX_SLOT(N) := {_, V}} -> {ok, V};
        _ -> {error, nil}
    end;
get(Props, {_, Id, Text}) ->
    case Props of
        #{Id := {Text, V}} -> {ok, V};
        #{Id := _} ->
            case moved(Props, Id + 4, Id, Text) of
                {at, _, V} -> {ok, V};
                {free, _} -> {error, nil}
            end;
        _ -> {error, nil}
    end.

has_key(Props, K) -> get(Props, K) =/= {error, nil}.

%% where Text sits past its id, or the first free slot there
moved(Props, Slot, Id, Text) ->
    case Props of
        #{Slot := {Text, V, Id}} -> {at, Slot, V};
        #{Slot := _} -> moved(Props, Slot + 4, Id, Text);
        _ -> {free, Slot}
    end.

insert(Props, {?KEY_INDEX, N}, V) -> Props#{?INDEX_SLOT(N) => ?INDEX_ENTRY(V)};
insert(Props, {_, Id, Text}, V) ->
    case Props of
        #{Id := {Text, _}} -> Props#{Id := {Text, V}};
        #{Id := _} ->
            {_, Slot, _} = case moved(Props, Id + 4, Id, Text) of
                {free, Free} -> {free, Free, V};
                At -> At
            end,
            Props#{Slot => {Text, V, Id}};
        _ -> Props#{Id => {Text, V}}
    end.

delete(Props, {?KEY_INDEX, N}) -> maps:remove(?INDEX_SLOT(N), Props);
delete(Props, {_, Id, Text}) ->
    case Props of
        #{Id := {Text, _}} -> close_up(maps:remove(Id, Props), Id + 4);
        #{Id := _} ->
            case moved(Props, Id + 4, Id, Text) of
                {at, Slot, _} -> close_up(maps:remove(Slot, Props), Slot + 4);
                {free, _} -> Props
            end;
        _ -> Props
    end.

%% a slot came free: whatever follows without a gap may have been waiting for it
close_up(Props, Slot) ->
    case Props of
        #{Slot := Entry} ->
            close_up(insert(maps:remove(Slot, Props), key_at(Slot, Entry),
                            element(2, Entry)),
                     Slot + 4);
        _ -> Props
    end.

key_at(Slot, _) when Slot < 0 -> {?KEY_INDEX, -1 - Slot};
key_at(Slot, {Text, _}) -> text_key(Slot, Text);
key_at(_, {Text, _, Id}) -> text_key(Id, Text).

text_key(Id, Text) when Id band 1 =:= 0 -> {?KEY_NAMED, Id, Text};
text_key(Id, Text) -> {?KEY_PRIVATE, Id, Text}.

from_list(Pairs) -> lists:foldl(fun({K, V}, Props) -> insert(Props, K, V) end, #{}, Pairs).

to_list(Props) ->
    maps:fold(fun(Slot, E, Acc) -> [{key_at(Slot, E), element(2, E)} | Acc] end, [], Props).

keys(Props) -> maps:fold(fun(Slot, E, Acc) -> [key_at(Slot, E) | Acc] end, [], Props).

values(Props) -> [element(2, E) || E <- maps:values(Props)].

fold(Props, Acc0, F) ->
    maps:fold(fun(Slot, E, Acc) -> F(Acc, key_at(Slot, E), element(2, E)) end, Acc0, Props).

map_values(Props, F) ->
    maps:map(fun(Slot, E) -> setelement(2, E, F(key_at(Slot, E), element(2, E))) end, Props).

%% through delete, so that what stays can still be found
filter(Props, Keep) ->
    maps:fold(fun(Slot, E, Acc) ->
                  K = key_at(Slot, E),
                  case Keep(K, element(2, E)) of
                      true -> Acc;
                      false -> delete(Acc, K)
                  end
              end, Props, Props).
