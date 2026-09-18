%% aot call site ics and direct entry; exports may answer {miss, St}
-module(arc_rt_call_ic_ffi).
-export([t_call_by_kind/4, t_call_by_kind0/3, t_call_by_kind1/4, t_call_by_kind2/5,
         t_call_by_kind3/6,
         t_call_method_mono/4, t_call_method_ic/6, t_call_method_ic0/5,
         t_call_method_ic1/6, t_call_method_ic2/7, t_call_method_ic3/8,
         t_new_direct/3, t_bind_compiled/4]).

-include("arc_rt_layout.hrl").

%% deltablue proto chains reach 3 hops
-define(WALK_MAX_HOPS, 4).
-define(IC_CALL_WAYS, 16).

%% args travel as a count n with a, b, c, or as a list in place of n
t_call_by_kind(St, F, This, Args) ->
    dispatch_kind(St, F, This, Args, undefined, undefined, undefined).

t_call_by_kind0(St, F, This) ->
    dispatch_kind(St, F, This, 0, undefined, undefined, undefined).
t_call_by_kind1(St, F, This, A) ->
    dispatch_kind(St, F, This, 1, A, undefined, undefined).
t_call_by_kind2(St, F, This, A, B) ->
    dispatch_kind(St, F, This, 2, A, B, undefined).
t_call_by_kind3(St, F, This, A, B, C) ->
    dispatch_kind(St, F, This, 3, A, B, C).

dispatch_kind(St, F = {?HANDLE_TAG, Id}, This, N, A, B, C) ->
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, element(?AGENT_STORE, St))) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Cell) of
                ?COMPILEDFN(Code, Home, Flags, _, DirectEntry) when ?IS_PLAIN_FN(Flags) ->
                    case element(?FNFLAGS_IS_ARROW, Flags)
                         orelse element(?FNFLAGS_IS_STRICT, Flags) of
                        true ->
                            enter_compiled(St, F, Code, Home, DirectEntry, This, N,
                                           A, B, C);
                        false when This =:= undefined; This =:= null ->
                            G = element(?REALM_GLOBAL, element(?AGENT_REALM, St)),
                            enter_compiled(St, F, Code, Home, DirectEntry, G, N, A,
                                           B, C);
                        false when element(1, This) =:= ?HANDLE_TAG ->
                            enter_compiled(St, F, Code, Home, DirectEntry, This, N,
                                           A, B, C);
                        false -> call_general(St, F, This, N, A, B, C)
                    end;
                _ -> call_general(St, F, This, N, A, B, C)
            end;
        _ -> call_general(St, F, This, N, A, B, C)
    end;
dispatch_kind(St, F, This, N, A, B, C) -> call_general(St, F, This, N, A, B, C).

call_general(St, F, This, N, A, B, C) ->
    'arc@rt@call':t_call_checked(St, F, This, arg_list(N, A, B, C)).

enter_compiled(St, _, _, _, {?SOME, ?DIRECT_ENTRY(CodeS, Arity, TakesThis)},
               ThisR, Args, _, _, _)
  when is_list(Args), length(Args) =:= Arity ->
    case TakesThis of
        true -> apply_this(CodeS, St, ThisR, Args);
        false -> erlang:apply(CodeS, [St | Args])
    end;
enter_compiled(St, _, _, _, {?SOME, ?DIRECT_ENTRY(CodeS, N, true)}, ThisR, N,
               A, B, C) ->
    case N of
        0 -> CodeS(St, ThisR);
        1 -> CodeS(St, ThisR, A);
        2 -> CodeS(St, ThisR, A, B);
        3 -> CodeS(St, ThisR, A, B, C)
    end;
enter_compiled(St, _, _, _, {?SOME, ?DIRECT_ENTRY(CodeS, N, false)}, _, N, A,
               B, C) ->
    case N of
        0 -> CodeS(St);
        1 -> CodeS(St, A);
        2 -> CodeS(St, A, B);
        3 -> CodeS(St, A, B, C)
    end;
enter_compiled(St, F, Code, Home, _, ThisR, N, A, B, C) ->
    Code(St, ?FRAME(ThisR, F, home_or_undefined(Home), undefined),
         arg_list(N, A, B, C)).

home_or_undefined({?SOME, H}) -> H;
home_or_undefined(?NONE) -> undefined.

t_call_method_ic(St, Recv, KeyBin, Args, Site, RSite) ->
    call_via_ic(St, Recv, KeyBin, Site, RSite, Args, undefined, undefined,
                undefined).

t_call_method_ic0(St, Recv, KeyBin, Site, RSite) ->
    call_via_ic(St, Recv, KeyBin, Site, RSite, 0, undefined, undefined,
                undefined).
t_call_method_ic1(St, Recv, KeyBin, Site, RSite, A) ->
    call_via_ic(St, Recv, KeyBin, Site, RSite, 1, A, undefined, undefined).
t_call_method_ic2(St, Recv, KeyBin, Site, RSite, A, B) ->
    call_via_ic(St, Recv, KeyBin, Site, RSite, 2, A, B, undefined).
t_call_method_ic3(St, Recv, KeyBin, Site, RSite, A, B, C) ->
    call_via_ic(St, Recv, KeyBin, Site, RSite, 3, A, B, C).

call_via_ic(St, Recv = {?HANDLE_TAG, RId}, KeyBin, Site, RSite, N, A, B, C) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    RCell = arc_rt_arena_ffi:get(RId, Data),
    case element(?STORE_ICS, Store) of
        #{Site := {?IC_CALL, KeyBin, _, Shaped}}
          when element(1, RCell) =:= ?SSHAPED_TAG ->
            %% shaped ways nest sid then proto id, no tuple key to build
            case Shaped of
                #{element(?SSHAPED_SID, RCell) := Protos} ->
                    case element(?SSHAPED_PROTO, RCell) of
                        {?SOME, {?HANDLE_TAG, PId}} = Proto ->
                            case Protos of
                                #{PId := {Chain, Fn, Kind}} ->
                                    case ic_chain_ok(Data, Proto, Chain) of
                                        true ->
                                            call_kind(St, Kind, Fn, Recv, N,
                                                      A, B, C);
                                        false ->
                                            ic_miss(St, Recv, RCell, KeyBin,
                                                    Site, RSite, N, A, B, C)
                                    end;
                                _ ->
                                    ic_miss(St, Recv, RCell, KeyBin,
                                            ways_room(Protos, Site), RSite,
                                            N, A, B, C)
                            end;
                        _ ->
                            ic_miss(St, Recv, RCell, KeyBin, none, RSite, N,
                                    A, B, C)
                    end;
                _ ->
                    ic_miss(St, Recv, RCell, KeyBin, ways_room(Shaped, Site),
                            RSite, N, A, B, C)
            end;
        #{Site := {?IC_CALL, KeyBin, Ways, _}} ->
            Probe = case ic_probe(Data, RId, RCell, KeyBin, Ways) of
                {hit, _, _} = Hit -> Hit;
                stale -> Site;
                spent -> none;
                miss -> ways_room(Ways, Site)
            end,
            case Probe of
                {hit, Fn1, Kind1} ->
                    call_kind(St, Kind1, Fn1, Recv, N, A, B, C);
                _ ->
                    ic_miss(St, Recv, RCell, KeyBin, Probe, RSite, N, A, B, C)
            end;
        #{Site := _} ->
            ic_miss(St, Recv, RCell, KeyBin, none, RSite, N, A, B, C);
        _ -> ic_miss(St, Recv, RCell, KeyBin, Site, RSite, N, A, B, C)
    end;
call_via_ic(St, Recv, KeyBin, Site, RSite, N, A, B, C) ->
    case prim_wrapper(Recv, KeyBin) of
        none -> after_lookup({miss, St}, Recv, KeyBin, RSite, N, A, B, C);
        W -> call_on_primitive(St, Recv, W, KeyBin, Site, RSite, N, A, B, C)
    end.

prim_wrapper(Recv, KeyBin) when ?IS_STR(Recv), KeyBin =/= <<"length">> ->
    ?REALM_STRING;
prim_wrapper(Recv, _) when is_number(Recv) -> ?REALM_NUMBER;
prim_wrapper(_, _) -> none.

call_on_primitive(St, Recv, W, KeyBin, Site, RSite, N, A, B, C) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    Proto = {?SOME, {?HANDLE_TAG, PId}} =
        {?SOME, element(?PAIR_PROTO, element(W, element(?AGENT_REALM, St)))},
    Probe = case element(?STORE_ICS, Store) of
        #{Site := {?IC_CALL, KeyBin, Ways, _}} ->
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
    case Probe of
        {hit, Fn, Kind} -> call_kind(St, Kind, Fn, Recv, N, A, B, C);
        none -> after_lookup({miss, St}, Recv, KeyBin, RSite, N, A, B, C);
        _ ->
            after_lookup(walk_chain(St, Data, PId, KeyBin, Recv,
                                    arg_list(N, A, B, C), ?WALK_MAX_HOPS,
                                    {Probe, {ic_prim, W}, []}),
                         Recv, KeyBin, RSite, N, A, B, C)
    end.

ways_room(Ways, Site) when map_size(Ways) < ?IC_CALL_WAYS -> Site;
ways_room(_, _) -> none.

%% fill site: the site to refill after the walk, or none when the ways are full
ic_miss(St, Recv, RCell, KeyBin, FillSite, RSite, N, A, B, C) ->
    after_lookup(call_via_walk(St, Recv, RCell, KeyBin, arg_list(N, A, B, C),
                               FillSite),
                 Recv, KeyBin, RSite, N, A, B, C).

after_lookup({miss, St}, Recv, KeyBin, RSite, N, A, B, C) ->
    {F, St1} = arc_rt_obj_ffi:t_get_named_site(St, Recv, KeyBin, RSite),
    dispatch_kind(St1, F, Recv, N, A, B, C);
after_lookup(Hit, _, _, _, _, _, _, _) -> Hit.

arg_list(L, _, _, _) when is_list(L) -> L;
arg_list(0, _, _, _) -> [];
arg_list(1, A, _, _) -> [A];
arg_list(2, A, B, _) -> [A, B];
arg_list(3, A, B, C) -> [A, B, C].

call_kind(St, Kind, Fn, Recv, Args, _, _, _) when is_list(Args) ->
    call_kind_list(St, Kind, Fn, Recv, Args);
call_kind(St, ?COMPILEDFN(Code, Home, _, _, DirectEntry), Fn, Recv, N, A, B, C) ->
    case DirectEntry of
        {?SOME, ?DIRECT_ENTRY(CodeT, N, true)} ->
            case N of
                0 -> CodeT(St, Recv);
                1 -> CodeT(St, Recv, A);
                2 -> CodeT(St, Recv, A, B);
                3 -> CodeT(St, Recv, A, B, C)
            end;
        {?SOME, ?DIRECT_ENTRY(CodeS, N, false)} ->
            case N of
                0 -> CodeS(St);
                1 -> CodeS(St, A);
                2 -> CodeS(St, A, B);
                3 -> CodeS(St, A, B, C)
            end;
        _ ->
            Code(St, ?FRAME(Recv, Fn, home_or_undefined(Home), undefined),
                 arg_list(N, A, B, C))
    end;
call_kind(St, {?NATIVEFN_TAG, Tag, _, _, _}, _, Recv, N, A, B, C) ->
    'arc@rt@builtins':dispatch_native(St, Tag, Recv, arg_list(N, A, B, C)).

%% {hit, Fn, Kind} | miss | stale when a cached chain changed | spent when
%% the own way no longer matches the receiver
ic_probe(Data, RId, RCell, KeyBin, Ways) when element(1, RCell) =:= ?SOBJECT_TAG ->
    case Ways of
        #{{ic_own, RId} := {[{_, Cell}], Fn, Kind}} ->
            case Cell =:= RCell of
                true -> {hit, Fn, Kind};
                false -> spent
            end;
        _ ->
            case element(?SOBJECT_PROTO, RCell) of
                {?SOME, {?HANDLE_TAG, PId}} = Proto ->
                    case Ways of
                        #{{ic_plain, PId} := {Chain, Fn, Kind}} ->
                            Own = is_map_key({?KEY_NAMED, KeyBin},
                                             element(?SOBJECT_PROPS, RCell))
                                orelse not arc_rt_obj_ffi:named_plain(
                                             element(?SOBJECT_KIND, RCell),
                                             KeyBin),
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
ic_chain_ok(Data, {?SOME, {?HANDLE_TAG, PId}}, [{PId, PCell} | Rest]) ->
    case arc_rt_arena_ffi:get(PId, Data) of
        PCell -> ic_chain_ok(Data, element(?CELL_PROTO, PCell), Rest);
        _ -> false
    end;
ic_chain_ok(_, _, _) -> false.

%% st unchanged on miss; emitter guards V =:= miss, not is_atom
t_call_method_mono(St, Recv = {?HANDLE_TAG, RId}, KeyBin, Args) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, St)),
    call_via_walk(St, Recv, arc_rt_arena_ffi:get(RId, Data), KeyBin, Args, none);
t_call_method_mono(St, _, _, _) -> {miss, St}.

%% resolves keybin along a plain chain, filling site when one is given
call_via_walk(St, Recv = {?HANDLE_TAG, RId}, RCell, KeyBin, Args, Site)
  when is_tuple(RCell) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    {Own, Ic} = case element(1, RCell) of
        ?SOBJECT_TAG when Site =:= none ->
            {own_named(RCell, KeyBin), none};
        ?SOBJECT_TAG ->
            case arc_rt_obj_ffi:named_plain(element(?SOBJECT_KIND, RCell),
                                            KeyBin) of
                true -> {own_named(RCell, KeyBin), {Site, ic_plain, []}};
                false -> {own_named(RCell, KeyBin), none}
            end;
        ?SSHAPED_TAG when Site =:= none ->
            {own_shaped(RCell, KeyBin), none};
        ?SSHAPED_TAG ->
            {own_shaped(RCell, KeyBin),
             {Site, {ic_shaped, element(?SSHAPED_SID, RCell)}, []}};
        _ -> {miss, none}
    end,
    case Own of
        absent ->
            walk_proto(St, Data, element(?CELL_PROTO, RCell), KeyBin, Recv,
                       Args, Ic);
        miss -> {miss, St};
        V when Ic =/= none, element(1, RCell) =:= ?SOBJECT_TAG ->
            call_found_fill(St, Data, V, KeyBin, Recv, Args,
                            {Site, {ic_own, RId, RCell}, []});
        V -> call_found(St, Data, V, Recv, Args)
    end;
call_via_walk(St, _, _, _, _, _) -> {miss, St}.

walk_proto(St, Data, {?SOME, {?HANDLE_TAG, PId}}, KeyBin, Recv, Args, Ic) ->
    walk_chain(St, Data, PId, KeyBin, Recv, Args, ?WALK_MAX_HOPS, Ic);
walk_proto(St, _, _, _, _, _, _) -> {miss, St}.

walk_chain(St, _, _, _, _, _, 0, _) -> {miss, St};
walk_chain(St, Data, Id, KeyBin, Recv, Args, Fuel, Ic) ->
    case arc_rt_arena_ffi:get(Id, Data) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            walk_hop(St, Data, Id, Cell, own_named(Cell, KeyBin), KeyBin,
                     Recv, Args, Fuel, Ic);
        Cell when element(1, Cell) =:= ?SSHAPED_TAG ->
            walk_hop(St, Data, Id, Cell, own_shaped(Cell, KeyBin), KeyBin,
                     Recv, Args, Fuel, Ic);
        _ -> {miss, St}
    end.

walk_hop(St, Data, Id, Cell, absent, KeyBin, Recv, Args, Fuel, Ic) ->
    case element(?CELL_PROTO, Cell) of
        {?SOME, {?HANDLE_TAG, NId}} ->
            walk_chain(St, Data, NId, KeyBin, Recv, Args, Fuel - 1,
                       ic_hop(Ic, Id, Cell));
        _ -> {miss, St}
    end;
walk_hop(St, Data, Id, Cell, V, KeyBin, Recv, Args, _, Ic) when Ic =/= none ->
    call_found_fill(St, Data, V, KeyBin, Recv, Args, ic_hop(Ic, Id, Cell));
walk_hop(St, Data, _, _, V, _, Recv, Args, _, _) ->
    call_found(St, Data, V, Recv, Args).

call_found_fill(St, Data, Fn = {?HANDLE_TAG, _}, KeyBin, Recv, Args, Ic) ->
    case plain_callee_kind(Data, Fn) of
        miss -> {miss, St};
        ?COMPILEDFN(_, _, Flags, _, _)
          when not is_tuple(Recv),
               element(?FNFLAGS_IS_STRICT, Flags) =/= true ->
            {miss, St};
        Kind ->
            call_kind_list(ic_fill(St, Ic, Fn, Kind, KeyBin), Kind, Fn, Recv,
                           Args)
    end;
call_found_fill(St, _, _, _, _, _, _) -> {miss, St}.

ic_hop(none, _, _) -> none;
ic_hop({Site, Match, Chain}, Id, Cell) -> {Site, Match, [{Id, Cell} | Chain]}.

ic_fill(St, {Site, Match0, RevChain}, Fn, Kind, KeyBin)
  when tuple_size(St) =:= ?AGENT_SIZE,
       tuple_size(element(?AGENT_STORE, St)) =:= ?STORE_SIZE ->
    Store = element(?AGENT_STORE, St),
    Ics = element(?STORE_ICS, Store),
    Chain = lists:reverse(RevChain),
    {Ways, Shaped} = case Ics of
        #{Site := {?IC_CALL, KeyBin, Ways0, Shaped0}} -> {Ways0, Shaped0};
        _ -> {#{}, #{}}
    end,
    Way = {Chain, Fn, Kind},
    IcE = case {Match0, Chain} of
        {{ic_shaped, Sid}, [{PId, _} | _]} ->
            case Shaped of
                #{Sid := Protos} when map_size(Protos) < ?IC_CALL_WAYS ->
                    {?IC_CALL, KeyBin, Ways, Shaped#{Sid := Protos#{PId => Way}}};
                #{Sid := _} ->
                    {?IC_CALL, KeyBin, Ways, Shaped#{Sid := #{PId => Way}}};
                _ when map_size(Shaped) < ?IC_CALL_WAYS ->
                    {?IC_CALL, KeyBin, Ways, Shaped#{Sid => #{PId => Way}}};
                _ -> full
            end;
        {ic_plain, [{PId, _} | _]} ->
            ways_put(KeyBin, Ways, Shaped, {ic_plain, PId}, Way);
        {{ic_own, RId, RCell}, []} ->
            ways_put(KeyBin, Ways, Shaped, {ic_own, RId},
                     {[{RId, RCell}], Fn, Kind});
        {{ic_prim, W}, [{PId, _} | _]} ->
            ways_put(KeyBin, Ways, Shaped, {ic_prim, W, PId}, Way)
    end,
    case IcE of
        full -> St;
        _ ->
            setelement(?AGENT_STORE, St,
                       setelement(?STORE_ICS, Store, Ics#{Site => IcE}))
    end.

ways_put(KeyBin, Ways, Shaped, Match, Way) ->
    case is_map_key(Match, Ways) orelse map_size(Ways) < ?IC_CALL_WAYS of
        true -> {?IC_CALL, KeyBin, Ways#{Match => Way}, Shaped};
        false -> full
    end.

%% an own accessor shadows proto, so miss rather than absent
own_named(Cell, KeyBin) ->
    case element(?SOBJECT_PROPS, Cell) of
        #{{?KEY_NAMED, KeyBin} := Prop}
          when element(1, Prop) =:= ?DATAPROP_TAG ->
            element(?DATAPROP_VALUE, Prop);
        #{{?KEY_NAMED, KeyBin} := _} -> miss;
        _ -> absent
    end.

own_shaped(RCell, KeyBin) ->
    case element(?SSHAPED_OFFSETS, RCell) of
        #{KeyBin := Off} -> ?SLOT_AT(element(?SSHAPED_SLOTS, RCell), Off);
        _ -> absent
    end.

call_found(St, Data, Fn = {?HANDLE_TAG, _}, Recv, Args) ->
    case plain_callee_kind(Data, Fn) of
        miss -> {miss, St};
        Kind -> call_kind_list(St, Kind, Fn, Recv, Args)
    end;
call_found(St, _, _, _, _) -> {miss, St}.

plain_callee_kind(Data, {?HANDLE_TAG, FnId}) ->
    case arc_rt_arena_ffi:get(FnId, Data) of
        FCell when element(1, FCell) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, FCell) of
                Kind = ?COMPILEDFN(_, _, Flags, _, _) when ?IS_PLAIN_FN(Flags) -> Kind;
                Kind when element(1, Kind) =:= ?NATIVEFN_TAG -> Kind;
                _ -> miss
            end;
        _ -> miss
    end.

call_kind_list(St, ?COMPILEDFN(Code, Home, _, _, DirectEntry), Fn, Recv, Args) ->
    case DirectEntry of
        {?SOME, ?DIRECT_ENTRY(CodeT, Arity, true)} when length(Args) =:= Arity ->
            apply_this(CodeT, St, Recv, Args);
        {?SOME, ?DIRECT_ENTRY(CodeS, Arity, false)} when length(Args) =:= Arity ->
            erlang:apply(CodeS, [St | Args]);
        _ -> Code(St, ?FRAME(Recv, Fn, home_or_undefined(Home), undefined), Args)
    end;
call_kind_list(St, {?NATIVEFN_TAG, Tag, _, _, _}, _, Recv, Args) ->
    'arc@rt@builtins':dispatch_native(St, Tag, Recv, Args).

apply_this(CodeT, St, Recv, []) -> CodeT(St, Recv);
apply_this(CodeT, St, Recv, [A]) -> CodeT(St, Recv, A);
apply_this(CodeT, St, Recv, [A, B]) -> CodeT(St, Recv, A, B);
apply_this(CodeT, St, Recv, [A, B, C]) -> CodeT(St, Recv, A, B, C);
apply_this(CodeT, St, Recv, Args) -> erlang:apply(CodeT, [St, Recv | Args]).

t_new_direct(St, Ctor = {?HANDLE_TAG, CId}, Args) ->
    Store = element(?AGENT_STORE, St),
    Data = element(?STORE_DATA, Store),
    case arc_rt_arena_ffi:get(CId, Data) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Cell) of
                Kind = ?COMPILEDFN(_, _, Flags, ?NONE, _)
                  when element(?FNFLAGS_IS_CTOR, Flags) =:= true,
                       element(?FNFLAGS_IS_DERIVED, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    case element(?SOBJECT_PROPS, Cell) of
                        #{{?KEY_NAMED, <<"prototype">>} := Prop}
                          when element(1, Prop) =:= ?DATAPROP_TAG ->
                            case element(?DATAPROP_VALUE, Prop) of
                                Proto = {?HANDLE_TAG, _} ->
                                    new_direct_apply(St, Store, Data, Ctor,
                                                     Kind, Proto, Args);
                                _ -> {miss, St}
                            end;
                        _ -> {miss, St}
                    end;
                _ -> {miss, St}
            end;
        _ -> {miss, St}
    end;
t_new_direct(St, _, _) -> {miss, St}.

new_direct_apply(St, Store, Data, Ctor, ?COMPILEDFN(Code, Home, _, _, DirectEntry), Proto,
                 Args)
  when tuple_size(St) =:= ?AGENT_SIZE, tuple_size(Store) =:= ?STORE_SIZE ->
    NewCell = {?SSHAPED_TAG, 0, {?SOME, Proto}, {}, #{}},
    NewId = element(?STORE_NEXT, Store),
    Store2 = setelement(?STORE_DATA, Store, arc_rt_arena_ffi:set(NewId, NewCell, Data)),
    Store3 = setelement(?STORE_NEXT, Store2, NewId + 1),
    Store4 = setelement(?STORE_ALLOC_SINCE_GC, Store3,
                        element(?STORE_ALLOC_SINCE_GC, Store) + 1),
    St2 = setelement(?AGENT_STORE, St, Store4),
    NewThis = {?HANDLE_TAG, NewId},
    {V, St3} = case DirectEntry of
        {?SOME, ?DIRECT_ENTRY(CodeT, Arity, true)} when length(Args) =:= Arity ->
            apply_this(CodeT, St2, NewThis, Args);
        {?SOME, ?DIRECT_ENTRY(CodeS, Arity, false)} when length(Args) =:= Arity ->
            erlang:apply(CodeS, [St2 | Args]);
        _ -> Code(St2, ?FRAME(NewThis, Ctor, home_or_undefined(Home), Ctor), Args)
    end,
    case V of
        {?HANDLE_TAG, _} -> {V, St3};
        _ -> {NewThis, St3}
    end.

%% bind once for natives that call back per element, none takes the frame path
t_bind_compiled(St, F, ?COMPILEDFN(Code, Home, Flags, _, DirectEntry), This)
  when ?IS_PLAIN_FN(Flags) ->
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
            Frame = ?FRAME(ThisR, F, home_or_undefined(Home), undefined),
            General = fun(S, Args) -> Code(S, Frame, Args) end,
            {?SOME, prepared_direct_entry(DirectEntry, ThisR, General)}
    end;
t_bind_compiled(_, _, _, _) -> ?NONE.

prepared_direct_entry({?SOME, ?DIRECT_ENTRY(CodeS, 0, true)}, T, _) ->
    fun(S, _) -> CodeS(S, T) end;
prepared_direct_entry({?SOME, ?DIRECT_ENTRY(CodeS, 1, true)}, T, _) ->
    fun(S, [A | _]) -> CodeS(S, T, A);
       (S, []) -> CodeS(S, T, undefined)
    end;
prepared_direct_entry({?SOME, ?DIRECT_ENTRY(CodeS, 2, true)}, T, General) ->
    fun(S, [A, B | _]) -> CodeS(S, T, A, B);
       (S, Args) -> General(S, Args)
    end;
prepared_direct_entry({?SOME, ?DIRECT_ENTRY(CodeS, 3, true)}, T, General) ->
    fun(S, [A, B, C | _]) -> CodeS(S, T, A, B, C);
       (S, Args) -> General(S, Args)
    end;
prepared_direct_entry({?SOME, ?DIRECT_ENTRY(CodeS, 0, false)}, _, _) ->
    fun(S, _) -> CodeS(S) end;
prepared_direct_entry({?SOME, ?DIRECT_ENTRY(CodeS, 1, false)}, _, _) ->
    fun(S, [A | _]) -> CodeS(S, A);
       (S, []) -> CodeS(S, undefined)
    end;
prepared_direct_entry({?SOME, ?DIRECT_ENTRY(CodeS, 2, false)}, _, General) ->
    fun(S, [A, B | _]) -> CodeS(S, A, B);
       (S, Args) -> General(S, Args)
    end;
prepared_direct_entry({?SOME, ?DIRECT_ENTRY(CodeS, 3, false)}, _, General) ->
    fun(S, [A, B, C | _]) -> CodeS(S, A, B, C);
       (S, Args) -> General(S, Args)
    end;
prepared_direct_entry(_, _, General) -> General.
