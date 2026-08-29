%% aot call-site fast paths, {miss, St} falls back to the full path
-module(arc_rt_call_fast_ffi).
-export([t_call_fast/4, t_call_fast0/3, t_call_fast1/4, t_call_fast2/5,
         t_call_fast3/6,
         t_call_method_ic/6, t_call_method_ic0/5,
         t_call_method_ic1/6, t_call_method_ic2/7, t_call_method_ic3/8,
         t_new_simple/3, t_bind_compiled/4]).

-include("arc_rt_layout.hrl").
-include("arc_rt_names.hrl").

%% deltablue proto chains reach 3 hops
-define(MONO_PROTO_MAX, 4).

-define(KFN_PLAIN(Flags),
        (element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false andalso
         element(?FNFLAGS_IS_GEN, Flags) =:= false andalso
         element(?FNFLAGS_IS_ASYNC, Flags) =:= false)).

t_call_fast(St, F, This, Args) ->
    call_fast(St, F, This, Args, undefined, undefined, undefined).

t_call_fast0(St, F, This) ->
    call_fast(St, F, This, 0, undefined, undefined, undefined).
t_call_fast1(St, F, This, A) ->
    call_fast(St, F, This, 1, A, undefined, undefined).
t_call_fast2(St, F, This, A, B) ->
    call_fast(St, F, This, 2, A, B, undefined).
t_call_fast3(St, F, This, A, B, C) ->
    call_fast(St, F, This, 3, A, B, C).

call_fast(St, F = {?HANDLE_TAG, Id}, This, N, A, B, C) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, Home, Flags, _, Simple, _, _, _}
                  when ?KFN_PLAIN(Flags) ->
                    case element(?FNFLAGS_IS_ARROW, Flags)
                         orelse element(?FNFLAGS_IS_STRICT, Flags) of
                        true ->
                            apply_fast(St, F, Code, Home, Simple, This, N, A,
                                       B, C);
                        false when This =:= undefined; This =:= null ->
                            G = element(?REALM_GLOBAL, element(?AGENT_REALM, St)),
                            apply_fast(St, F, Code, Home, Simple, G, N, A, B,
                                       C);
                        false when element(1, This) =:= ?HANDLE_TAG ->
                            apply_fast(St, F, Code, Home, Simple, This, N, A,
                                       B, C);
                        false -> call_slow(St, F, This, N, A, B, C)
                    end;
                _ -> call_slow(St, F, This, N, A, B, C)
            end;
        _ -> call_slow(St, F, This, N, A, B, C)
    end;
call_fast(St, F, This, N, A, B, C) -> call_slow(St, F, This, N, A, B, C).

call_slow(St, F, This, N, A, B, C) ->
    arc@rt@call:t_call_checked(St, F, This, args(N, A, B, C)).

apply_fast(St, _, _, _, {?SOME, {CodeS, Arity, NeedsThis}}, ThisR, Args, _, _,
           _)
  when is_list(Args), length(Args) =:= Arity ->
    case NeedsThis of
        true -> apply_this(CodeS, St, ThisR, Args);
        false -> erlang:apply(CodeS, [St | Args])
    end;
apply_fast(St, _, _, _, {?SOME, {CodeS, N, true}}, ThisR, N, A, B, C) ->
    case N of
        0 -> CodeS(St, ThisR);
        1 -> CodeS(St, ThisR, A);
        2 -> CodeS(St, ThisR, A, B);
        3 -> CodeS(St, ThisR, A, B, C)
    end;
apply_fast(St, _, _, _, {?SOME, {CodeS, N, false}}, _, N, A, B, C) ->
    case N of
        0 -> CodeS(St);
        1 -> CodeS(St, A);
        2 -> CodeS(St, A, B);
        3 -> CodeS(St, A, B, C)
    end;
apply_fast(St, F, Code, Home, _, ThisR, N, A, B, C) ->
    Code(St, {ThisR, F, home(Home), undefined}, args(N, A, B, C)).

home({?SOME, H}) -> H;
home(?NONE) -> undefined.

-define(IC_CALL, ic_call).
-define(IC_CALL_WAYS, 16).

t_call_method_ic(St, Recv, K, Args, Site, RSite) ->
    ic(St, Recv, K, Site, RSite, Args, undefined, undefined, undefined).

t_call_method_ic0(St, Recv, K, Site, RSite) ->
    ic(St, Recv, K, Site, RSite, 0, undefined, undefined, undefined).
t_call_method_ic1(St, Recv, K, Site, RSite, A) ->
    ic(St, Recv, K, Site, RSite, 1, A, undefined, undefined).
t_call_method_ic2(St, Recv, K, Site, RSite, A, B) ->
    ic(St, Recv, K, Site, RSite, 2, A, B, undefined).
t_call_method_ic3(St, Recv, K, Site, RSite, A, B, C) ->
    ic(St, Recv, K, Site, RSite, 3, A, B, C).

ic(St, Recv = {?HANDLE_TAG, RId}, K, Site, RSite, N, A, B, C) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    RSlot = arc_rt_arena_ffi:get(RId, Data),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_CALL, K, _, Shaped}}
          when element(1, RSlot) =:= ?SSHAPED_TAG ->
            %% shaped ways nest sid then proto id, no tuple key to build
            case Shaped of
                #{element(?SSHAPED_SID, RSlot) := Protos} ->
                    case element(?SSHAPED_PROTO, RSlot) of
                        {?SOME, {?HANDLE_TAG, PId}} = Proto ->
                            case Protos of
                                #{PId := {Chain, Fn, Kind}} ->
                                    case ic_chain_ok(Data, Proto, Chain) of
                                        true ->
                                            apply_kind(St, Kind, Fn, Recv, N,
                                                       A, B, C);
                                        false ->
                                            ic_miss(St, Recv, RSlot, K,
                                                    Site, RSite, N, A, B, C)
                                    end;
                                _ ->
                                    ic_miss(St, Recv, RSlot, K,
                                            ways_room(Protos, Site), RSite,
                                            N, A, B, C)
                            end;
                        _ ->
                            ic_miss(St, Recv, RSlot, K, none, RSite, N,
                                    A, B, C)
                    end;
                _ ->
                    ic_miss(St, Recv, RSlot, K, ways_room(Shaped, Site),
                            RSite, N, A, B, C)
            end;
        #{Site := {?IC_CALL, K, Ways, _}} ->
            Fill = case ic_probe(Data, RId, RSlot, K, Ways) of
                {hit, _, _} = Hit -> Hit;
                stale -> Site;
                spent -> none;
                miss -> ways_room(Ways, Site)
            end,
            case Fill of
                {hit, Fn1, Kind1} ->
                    apply_kind(St, Kind1, Fn1, Recv, N, A, B, C);
                _ ->
                    ic_miss(St, Recv, RSlot, K, Fill, RSite, N, A, B, C)
            end;
        #{Site := _} ->
            ic_miss(St, Recv, RSlot, K, none, RSite, N, A, B, C);
        _ -> ic_miss(St, Recv, RSlot, K, Site, RSite, N, A, B, C)
    end;
ic(St, Recv, K, Site, RSite, N, A, B, C) ->
    case prim_wrapper(Recv, K) of
        none -> slow({miss, St}, Recv, K, RSite, N, A, B, C);
        W -> prim(St, Recv, W, K, Site, RSite, N, A, B, C)
    end.

prim_wrapper(Recv, K) when is_binary(Recv), K =/= ?K_length ->
    ?REALM_STRING;
prim_wrapper(Recv, _) when is_number(Recv) -> ?REALM_NUMBER;
prim_wrapper(_, _) -> none.

prim(St, Recv, W, K, Site, RSite, N, A, B, C) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    Proto = {?SOME, {?HANDLE_TAG, PId}} =
        {?SOME, element(?PAIR_PROTO, element(W, element(?AGENT_REALM, St)))},
    Fill = case element(?STORE_ICS, Store) of
        #{Site := {?IC_CALL, K, Ways, _}} ->
            case Ways of
                #{{ic_prim, W, PId} := {Chain, Fn0, Kind0}} ->
                    case ic_chain_ok(Data, Proto, Chain) of
                        true -> {hit, Fn0, Kind0};
                        false -> Site
                    end;
                _ when map_size(Ways) < ?IC_CALL_WAYS -> Site;
                _ -> none
            end;
        #{Site := _} -> none;
        _ -> Site
    end,
    case Fill of
        {hit, Fn, Kind} -> apply_kind(St, Kind, Fn, Recv, N, A, B, C);
        none -> slow({miss, St}, Recv, K, RSite, N, A, B, C);
        _ ->
            slow(mono_proto_walk(St, Data, PId, K, Recv,
                                 args(N, A, B, C), ?MONO_PROTO_MAX,
                                 {Fill, {ic_prim, W}, []}),
                 Recv, K, RSite, N, A, B, C)
    end.

ways_room(Ways, Site) when map_size(Ways) < ?IC_CALL_WAYS -> Site;
ways_room(_, _) -> none.

ic_miss(St, Recv, RSlot, K, Fill, RSite, N, A, B, C) ->
    slow(mono(St, Recv, RSlot, K, args(N, A, B, C), Fill), Recv, K,
         RSite, N, A, B, C).

slow({miss, St}, Recv, K, RSite, N, A, B, C) ->
    {F, St1} = arc_rt_obj_ffi:t_get_prop_site(St, Recv, K, RSite),
    call_fast(St1, F, Recv, N, A, B, C);
slow(Hit, _, _, _, _, _, _, _) -> Hit.

args(L, _, _, _) when is_list(L) -> L;
args(0, _, _, _) -> [];
args(1, A, _, _) -> [A];
args(2, A, B, _) -> [A, B];
args(3, A, B, C) -> [A, B, C].

apply_kind(St, Kind, Fn, Recv, Args, _, _, _) when is_list(Args) ->
    kind_apply(St, Kind, Fn, Recv, Args);
apply_kind(St, {?KFN_TAG, Code, Home, _, _, Simple, _, _, _}, Fn, Recv, N, A,
           B, C) ->
    case Simple of
        {?SOME, {CodeT, N, true}} ->
            case N of
                0 -> CodeT(St, Recv);
                1 -> CodeT(St, Recv, A);
                2 -> CodeT(St, Recv, A, B);
                3 -> CodeT(St, Recv, A, B, C)
            end;
        {?SOME, {CodeS, N, false}} ->
            case N of
                0 -> CodeS(St);
                1 -> CodeS(St, A);
                2 -> CodeS(St, A, B);
                3 -> CodeS(St, A, B, C)
            end;
        _ -> Code(St, {Recv, Fn, home(Home), undefined}, args(N, A, B, C))
    end;
apply_kind(St, {?KNATIVE_TAG, Tag, _, _, _}, _, Recv, N, A, B, C) ->
    arc@rt@builtins:dispatch_native(St, Tag, Recv, args(N, A, B, C)).

ic_probe(Data, RId, RSlot, K, Ways) when element(1, RSlot) =:= ?SOBJECT_TAG ->
    case Ways of
        #{{ic_own, RId} := {[{_, Slot}], Fn, Kind}} ->
            case Slot =:= RSlot of
                true -> {hit, Fn, Kind};
                false -> spent
            end;
        _ ->
            case element(?SOBJECT_PROTO, RSlot) of
                {?SOME, {?HANDLE_TAG, PId}} = Proto ->
                    case Ways of
                        #{{ic_plain, PId} := {Chain, Fn, Kind}} ->
                            Own = is_map_key(K,
                                             element(?SOBJECT_PROPS, RSlot))
                                orelse not arc_rt_obj_ffi:named_plain(
                                             element(?SOBJECT_KIND, RSlot),
                                             K),
                            case Own of
                                true -> miss;
                                false ->
                                    case ic_chain_ok(Data, Proto, Chain) of
                                        true -> {hit, Fn, Kind};
                                        false -> stale
                                    end
                            end;
                        _ -> miss
                    end;
                _ -> miss
            end
    end;
ic_probe(_, _, _, _, _) -> miss.

ic_chain_ok(_, _, []) -> true;
ic_chain_ok(Data, {?SOME, {?HANDLE_TAG, PId}}, [{PId, PSlot} | Rest]) ->
    case arc_rt_arena_ffi:get(PId, Data) of
        PSlot -> ic_chain_ok(Data, element(?SOBJECT_PROTO, PSlot), Rest);
        _ -> false
    end;
ic_chain_ok(_, _, _) -> false.

mono(St, Recv = {?HANDLE_TAG, RId}, RSlot, K, Args, Site)
  when is_tuple(RSlot) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    {Own, Ic} = case element(1, RSlot) of
        ?SOBJECT_TAG when Site =:= none ->
            {mono_own_value(RSlot, K), none};
        ?SOBJECT_TAG ->
            case arc_rt_obj_ffi:named_plain(element(?SOBJECT_KIND, RSlot),
                                            K) of
                true -> {mono_own_value(RSlot, K), {Site, ic_plain, []}};
                false -> {mono_own_value(RSlot, K), none}
            end;
        ?SSHAPED_TAG when Site =:= none ->
            {mono_shaped_own(Store, RSlot, K), none};
        ?SSHAPED_TAG ->
            {mono_shaped_own(Store, RSlot, K),
             {Site, {ic_shaped, element(?SSHAPED_SID, RSlot)}, []}};
        _ -> {miss, none}
    end,
    case Own of
        absent ->
            %% proto is element 3 for both s_object and s_shaped_object
            mono_proto(St, Data, element(?SOBJECT_PROTO, RSlot), K,
                       Recv, Args, Ic);
        miss -> {miss, St};
        V when Ic =/= none, element(1, RSlot) =:= ?SOBJECT_TAG ->
            mono_found(St, Data, V, K, Recv, Args,
                       {Site, {ic_own, RId, RSlot}, []});
        V -> mono_apply(St, Data, V, Recv, Args)
    end;
mono(St, _, _, _, _, _) -> {miss, St}.

mono_proto(St, Data, {?SOME, {?HANDLE_TAG, PId}}, K, Recv, Args, Ic) ->
    mono_proto_walk(St, Data, PId, K, Recv, Args, ?MONO_PROTO_MAX, Ic);
mono_proto(St, _, _, _, _, _, _) -> {miss, St}.

mono_proto_walk(St, _, _, _, _, _, 0, _) -> {miss, St};
mono_proto_walk(St, Data, Id, K, Recv, Args, Fuel, Ic) ->
    case arc_rt_arena_ffi:get(Id, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            mono_hop(St, Data, Id, Slot, mono_own_value(Slot, K),
                     K, Recv, Args, Fuel, Ic);
        Slot when element(1, Slot) =:= ?SSHAPED_TAG ->
            Own = mono_shaped_own(element(?AGENT_STORE, St), Slot, K),
            mono_hop(St, Data, Id, Slot, Own, K, Recv, Args, Fuel, Ic);
        _ -> {miss, St}
    end.

mono_hop(St, Data, Id, Slot, absent, K, Recv, Args, Fuel, Ic) ->
    case element(?SOBJECT_PROTO, Slot) of
        {?SOME, {?HANDLE_TAG, NId}} ->
            mono_proto_walk(St, Data, NId, K, Recv, Args, Fuel - 1,
                            ic_hop(Ic, Id, Slot));
        _ -> {miss, St}
    end;
mono_hop(St, Data, Id, Slot, V, K, Recv, Args, _, Ic) when Ic =/= none ->
    mono_found(St, Data, V, K, Recv, Args, ic_hop(Ic, Id, Slot));
mono_hop(St, Data, _, _, V, _, Recv, Args, _, _) ->
    mono_apply(St, Data, V, Recv, Args).

mono_found(St, Data, Fn = {?HANDLE_TAG, _}, K, Recv, Args, Ic) ->
    case mono_kind(Data, Fn) of
        miss -> {miss, St};
        {?KFN_TAG, _, _, Flags, _, _, _, _, _}
          when not is_tuple(Recv),
               element(?FNFLAGS_IS_STRICT, Flags) =/= true ->
            {miss, St};
        Kind ->
            kind_apply(ic_fill(St, Ic, Fn, Kind, K), Kind, Fn, Recv, Args)
    end;
mono_found(St, _, _, _, _, _, _) -> {miss, St}.

ic_hop(none, _, _) -> none;
ic_hop({Site, Match, Chain}, Id, Slot) -> {Site, Match, [{Id, Slot} | Chain]}.

ic_fill(St, {Site, Match0, RevChain}, Fn, Kind, K)
  when tuple_size(St) =:= ?AGENT_ARITY,
       tuple_size(element(?AGENT_STORE, St)) =:= ?STORE_ARITY ->
    Store = element(?AGENT_STORE, St),
    Ics = element(?STORE_ICS, Store),
    Chain = lists:reverse(RevChain),
    {Ways, Shaped} = case Ics of
        #{Site := {?IC_CALL, K, Ways0, Shaped0}} -> {Ways0, Shaped0};
        _ -> {#{}, #{}}
    end,
    Way = {Chain, Fn, Kind},
    IcE = case {Match0, Chain} of
        {{ic_shaped, Sid}, [{PId, _} | _]} ->
            case Shaped of
                #{Sid := Protos} when map_size(Protos) < ?IC_CALL_WAYS ->
                    {?IC_CALL, K, Ways, Shaped#{Sid := Protos#{PId => Way}}};
                #{Sid := _} ->
                    {?IC_CALL, K, Ways, Shaped#{Sid := #{PId => Way}}};
                _ when map_size(Shaped) < ?IC_CALL_WAYS ->
                    {?IC_CALL, K, Ways, Shaped#{Sid => #{PId => Way}}};
                _ -> full
            end;
        {ic_plain, [{PId, _} | _]} ->
            ways_put(K, Ways, Shaped, {ic_plain, PId}, Way);
        {{ic_own, RId, RSlot}, []} ->
            ways_put(K, Ways, Shaped, {ic_own, RId},
                     {[{RId, RSlot}], Fn, Kind});
        {{ic_prim, W}, [{PId, _} | _]} ->
            ways_put(K, Ways, Shaped, {ic_prim, W, PId}, Way)
    end,
    case IcE of
        full -> St;
        _ ->
            setelement(?AGENT_STORE, St,
                       setelement(?STORE_ICS, Store, Ics#{Site => IcE}))
    end.

ways_put(K, Ways, Shaped, Match, Way) ->
    case is_map_key(Match, Ways) orelse map_size(Ways) < ?IC_CALL_WAYS of
        true -> {?IC_CALL, K, Ways#{Match => Way}, Shaped};
        false -> full
    end.

%% an own accessor shadows proto, so miss rather than absent
mono_own_value(Slot, K) ->
    case element(?SOBJECT_PROPS, Slot) of
        #{K := Prop}
          when element(1, Prop) =:= ?DATAPROP_TAG ->
            element(?DATAPROP_VALUE, Prop);
        #{K := _} -> miss;
        _ -> absent
    end.

mono_shaped_own(_, RSlot, K) ->
    case element(?SSHAPED_OFFSETS, RSlot) of
        #{K := Off} -> element(Off + 1, element(?SSHAPED_SLOTS, RSlot));
        _ -> absent
    end.

mono_apply(St, Data, Fn = {?HANDLE_TAG, _}, Recv, Args) ->
    case mono_kind(Data, Fn) of
        miss -> {miss, St};
        Kind -> kind_apply(St, Kind, Fn, Recv, Args)
    end;
mono_apply(St, _, _, _, _) -> {miss, St}.

mono_kind(Data, {?HANDLE_TAG, FnId}) ->
    case arc_rt_arena_ffi:get(FnId, Data) of
        FSlot when element(1, FSlot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, FSlot) of
                Kind = {?KFN_TAG, _, _, Flags, _, _, _, _, _}
                  when ?KFN_PLAIN(Flags) ->
                    Kind;
                Kind when element(1, Kind) =:= ?KNATIVE_TAG -> Kind;
                _ -> miss
            end;
        _ -> miss
    end.

kind_apply(St, {?KFN_TAG, Code, Home, _, _, Simple, _, _, _}, Fn, Recv, Args) ->
    case Simple of
        {?SOME, {CodeT, Arity, true}} when length(Args) =:= Arity ->
            apply_this(CodeT, St, Recv, Args);
        {?SOME, {CodeS, Arity, false}} when length(Args) =:= Arity ->
            erlang:apply(CodeS, [St | Args]);
        _ -> Code(St, {Recv, Fn, home(Home), undefined}, Args)
    end;
kind_apply(St, {?KNATIVE_TAG, Tag, _, _, _}, _, Recv, Args) ->
    arc@rt@builtins:dispatch_native(St, Tag, Recv, Args).

apply_this(CodeT, St, Recv, []) -> CodeT(St, Recv);
apply_this(CodeT, St, Recv, [A]) -> CodeT(St, Recv, A);
apply_this(CodeT, St, Recv, [A, B]) -> CodeT(St, Recv, A, B);
apply_this(CodeT, St, Recv, [A, B, C]) -> CodeT(St, Recv, A, B, C);
apply_this(CodeT, St, Recv, Args) -> erlang:apply(CodeT, [St, Recv | Args]).

t_new_simple(St, Ctor = {?HANDLE_TAG, CId}, Args) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(CId, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                Kind = {?KFN_TAG, _, _, Flags, ?NONE, _, _, _, _}
                  when element(?FNFLAGS_IS_CTOR, Flags) =:= true,
                       element(?FNFLAGS_IS_DERIVED, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{?K_prototype := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                Proto = {?HANDLE_TAG, _} ->
                                    new_simple_apply(St, Store, Data, Ctor,
                                                     Kind, Proto, Args);
                                _ -> {miss, St}
                            end;
                        _ -> {miss, St}
                    end;
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end;
t_new_simple(St, _, _) -> {miss, St}.

new_simple_apply(St, Store, Data, Ctor, {_, Code, Home, _, _, Simple, _, _, _},
                 Proto, Args)
  when tuple_size(St) =:= ?AGENT_ARITY, tuple_size(Store) =:= ?STORE_ARITY ->
    NewSlot = {?SSHAPED_TAG, 0, {?SOME, Proto}, {}, #{}},
    NewId = element(?STORE_NEXT, Store),
    Store2 = setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(NewId, NewSlot, Data)),
    Store3 = setelement(?STORE_NEXT, Store2, NewId + 1),
    Store4 = setelement(?STORE_ALLOC, Store3, element(?STORE_ALLOC, Store) + 1),
    St2 = setelement(?AGENT_STORE, St, Store4),
    NewThis = {?HANDLE_TAG, NewId},
    {V, St3} = case Simple of
        {?SOME, {CodeT, Arity, true}} when length(Args) =:= Arity ->
            apply_this(CodeT, St2, NewThis, Args);
        {?SOME, {CodeS, Arity, false}} when length(Args) =:= Arity ->
            erlang:apply(CodeS, [St2 | Args]);
        _ -> Code(St2, {NewThis, Ctor, home(Home), Ctor}, Args)
    end,
    case V of
        {?HANDLE_TAG, _} -> {V, St3};
        _ -> {NewThis, St3}
    end.

%% bind once for natives that call back per element, none takes the frame path
t_bind_compiled(St, F, {?KFN_TAG, Code, Home, Flags, _, Simple, _, _, _}, This)
  when ?KFN_PLAIN(Flags) ->
    ThisR = case element(?FNFLAGS_IS_ARROW, Flags)
                 orelse element(?FNFLAGS_IS_STRICT, Flags) of
        true -> This;
        false when This =:= undefined; This =:= null ->
            element(?REALM_GLOBAL, element(?AGENT_REALM, St));
        false when element(1, This) =:= ?HANDLE_TAG -> This;
        false -> prim
    end,
    case ThisR of
        prim -> ?NONE;
        _ ->
            Frame = {ThisR, F, home(Home), undefined},
            Slow = fun(S, Args) -> Code(S, Frame, Args) end,
            {?SOME, bound(Simple, ThisR, Slow)}
    end;
t_bind_compiled(_, _, _, _) -> ?NONE.

bound({?SOME, {CodeS, 0, true}}, T, _) ->
    fun(S, _) -> CodeS(S, T) end;
bound({?SOME, {CodeS, 1, true}}, T, _) ->
    fun(S, [A | _]) -> CodeS(S, T, A);
       (S, []) -> CodeS(S, T, undefined)
    end;
bound({?SOME, {CodeS, 2, true}}, T, Slow) ->
    fun(S, [A, B | _]) -> CodeS(S, T, A, B);
       (S, Args) -> Slow(S, Args)
    end;
bound({?SOME, {CodeS, 3, true}}, T, Slow) ->
    fun(S, [A, B, C | _]) -> CodeS(S, T, A, B, C);
       (S, Args) -> Slow(S, Args)
    end;
bound({?SOME, {CodeS, 0, false}}, _, _) ->
    fun(S, _) -> CodeS(S) end;
bound({?SOME, {CodeS, 1, false}}, _, _) ->
    fun(S, [A | _]) -> CodeS(S, A);
       (S, []) -> CodeS(S, undefined)
    end;
bound({?SOME, {CodeS, 2, false}}, _, Slow) ->
    fun(S, [A, B | _]) -> CodeS(S, A, B);
       (S, Args) -> Slow(S, Args)
    end;
bound({?SOME, {CodeS, 3, false}}, _, Slow) ->
    fun(S, [A, B, C | _]) -> CodeS(S, A, B, C);
       (S, Args) -> Slow(S, Args)
    end;
bound(_, _, Slow) -> Slow.

