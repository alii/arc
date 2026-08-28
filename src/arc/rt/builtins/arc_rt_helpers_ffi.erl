%% read-only fast paths for builtins, miss sends the caller to rt/obj
-module(arc_rt_helpers_ffi).
-export([get_symbol_data/3, is_miss/1]).

-include("../arc_rt_layout.hrl").

is_miss(V) -> V =:= miss.

%% §10.1.8.1 over data props; a getter that returns this is the receiver
get_symbol_data(St, {?HANDLE_TAG, Id} = Recv, Sym) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, St)),
    walk(Data, Id, Sym, Recv, 64);
get_symbol_data(_, _, _) -> miss.

walk(_, _, _, _, 0) -> miss;
walk(Data, Id, Sym, Recv, Fuel) ->
    case arc_rt_arena_ffi:get(Id, Data) of
        {?SSHAPED_TAG, _, Proto, _} -> next(Data, Proto, Sym, Recv, Fuel);
        {?SOBJECT_TAG, Kind, Proto, _, SymProps, _, _} ->
            case is_tuple(Kind) andalso element(1, Kind) =:= ?PROXYOBJ_TAG of
                true -> miss;
                false ->
                    case lists:keyfind(Sym, 1, SymProps) of
                        {_, Prop} when element(1, Prop) =:= ?DATAPROP_TAG ->
                            element(?DATAPROP_VALUE, Prop);
                        {_, {?ACCESSORPROP_TAG, {?SOME, Getter}, _, _, _, _}} ->
                            returns_this(Data, Getter, Recv);
                        {_, _} -> miss;
                        false -> next(Data, Proto, Sym, Recv, Fuel)
                    end
            end;
        _ -> miss
    end.

next(_, ?NONE, _, _, _) -> undefined;
next(Data, {?SOME, {?HANDLE_TAG, Id}}, Sym, Recv, Fuel) ->
    walk(Data, Id, Sym, Recv, Fuel - 1).

returns_this(Data, {?HANDLE_TAG, G}, Recv) ->
    case arc_rt_arena_ffi:get(G, Data) of
        {?SOBJECT_TAG, {?KNATIVE_TAG, return_this, _, _, _}, _, _, _, _, _} -> Recv;
        _ -> miss
    end;
returns_this(_, _, _) -> miss.
