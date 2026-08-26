%%% arc_interp_ffi — the bytecode interpreter's value kernels: miss/TDZ/
%%% undefined probes, ToBoolean/typeof/nullish, box and cell reads, and the
%%% `instanceof` fast path. Like every kernel family (arc_interp_prop_ffi,
%%% arc_interp_locals_ffi) they match the raw JsVal wire term and the store
%%% records directly and answer the result, or the atom `miss` when the
%%% operands need anything observable (a getter, a proxy trap, a throw).
%%% They are TOTAL: no clause raises for any wire term. The Gleam side types
%%% each kernel with its hit type and checks `is_miss/1` before touching the
%%% result. The operator kernels (add/2, lt/2, ...) live in arc_rt_ops_ffi.
-module(arc_interp_ffi).
-export([is_miss/1, is_tdz/1, is_undefined/1,
         truthy/1, lnot/1, nullish/1, typeof/1, typeof/2,
         box_get/2, cell_of/2, instance_of/4]).

-include("../rt/arc_rt_layout.hrl").

%% is_miss(X) -> boolean()
%% The one probe that knows a kernel result may be the `miss` atom instead
%% of its declared type. `miss` is not a JsVal wire term, so it can never
%% collide with a real value.
is_miss(miss) -> true;
is_miss(_) -> false.

%% is_tdz(V) -> boolean()
%% V is the TDZ sentinel `js_tdz` (an uninitialised let/const/class slot).
is_tdz(js_tdz) -> true;
is_tdz(_) -> false.

%% is_undefined(V) -> boolean()
is_undefined(undefined) -> true;
is_undefined(_) -> false.

%% truthy(V) -> boolean()
%% §7.1.2 ToBoolean, total; row-for-row with arc_rt_val_ffi:to_boolean_i32.
truthy(undefined) -> false;
truthy(null) -> false;
truthy(false) -> false;
truthy(true) -> true;
truthy(0) -> false;
truthy(N) when is_integer(N) -> true;
truthy(F) when is_float(F) -> F /= 0.0;
truthy(js_nan) -> false;
truthy(js_inf) -> true;
truthy(js_neg_inf) -> true;
truthy(<<>>) -> false;
truthy(B) when is_binary(B) -> true;
truthy({js_bigint, 0}) -> false;
truthy({js_bigint, _}) -> true;
truthy({js_sym, _}) -> true;
truthy({?HANDLE_TAG, _}) -> true;
truthy(js_tdz) -> false.

%% lnot(V) -> boolean()
%% `!V`. Total.
lnot(V) -> not truthy(V).

%% nullish(V) -> boolean()
%% `V` is null or undefined (the `??` / `?.` / JumpIfNullish test). Total.
nullish(undefined) -> true;
nullish(null) -> true;
nullish(_) -> false.

%% typeof(V) -> binary() | miss
%% §13.5.3 for primitives. Objects need the store to tell "function" from
%% "object": use typeof/2, or take the miss.
typeof(undefined) -> <<"undefined">>;
typeof(null) -> <<"object">>;
typeof(B) when is_boolean(B) -> <<"boolean">>;
typeof(N) when is_number(N) -> <<"number">>;
typeof(A) when A =:= js_nan; A =:= js_inf; A =:= js_neg_inf -> <<"number">>;
typeof(B) when is_binary(B) -> <<"string">>;
typeof({js_bigint, _}) -> <<"bigint">>;
typeof({js_sym, _}) -> <<"symbol">>;
typeof(js_tdz) -> <<"undefined">>;
typeof(_) -> miss.

%% typeof(Store, V) -> binary() | miss
%% typeof/1 plus the object rows: a cell whose kind has [[Call]] is
%% "function", any other object cell "object". A Proxy answers from its
%% target (§10.5.14), so it misses rather than chase the chain here.
typeof(Store, {?HANDLE_TAG, Id}) ->
    case array:get(Id, element(?STORE_DATA, Store)) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case kind_tag(element(?SOBJECT_KIND, Slot)) of
                ?KFN_TAG -> <<"function">>;
                ?KBYTECODE_TAG -> <<"function">>;
                ?KNATIVE_TAG -> <<"function">>;
                k_bound -> <<"function">>;
                ?PROXYOBJ_TAG -> miss;
                _ -> <<"object">>
            end;
        Slot when element(1, Slot) =:= ?SSHAPED_TAG -> <<"object">>;
        _ -> miss
    end;
typeof(_Store, V) -> typeof(V).

%% The constructor atom of an ObjKind term (nullary variants are bare
%% atoms, payload variants are tagged tuples).
kind_tag(Kind) when is_atom(Kind) -> Kind;
kind_tag(Kind) -> element(1, Kind).

%% cell_of(Agent, V) -> JsSlot | miss
%% The store cell behind an object value (the fast call arms' callee read);
%% any other value, or a freed id, misses.
cell_of(Agent, {?HANDLE_TAG, Id}) ->
    case array:get(Id, element(?STORE_DATA, element(?AGENT_STORE, Agent))) of
        ?STORE_FREE_SLOT -> miss;
        Slot -> Slot
    end;
cell_of(_, _) -> miss.

%% box_get(Agent, Slot) -> JsVal | miss
%% The value in the SBox cell a captured local holds (GetBoxed). The TDZ
%% sentinel, a local that is not a box handle, or a dangling handle miss.
box_get(Agent, {?HANDLE_TAG, Id}) ->
    case array:get(Id, element(?STORE_DATA, element(?AGENT_STORE, Agent))) of
        {?SBOX_TAG, js_tdz} -> miss;
        {?SBOX_TAG, V} -> V;
        _ -> miss
    end;
box_get(_, _) -> miss.

%% instance_of(Agent, V, Ctor, HasInstanceSym) -> boolean() | miss
%% §13.10.2 InstanceofOperator when GetMethod(Ctor, @@hasInstance) provably
%% resolves to the intrinsic %Function.prototype%[@@hasInstance] or to
%% undefined: Ctor is a plain (bytecode / compiled / native, so callable and
%% not bound) function cell and no hop of its prototype chain below the
%% realm's Function.prototype holds an own HasInstanceSym (that intrinsic is
%% {W:false, C:false}, so reaching Function.prototype settles it). Both
%% cases run §7.3.22 OrdinaryHasInstance, inlined: a non-object V is false
%% before "prototype" is read; Ctor's own data "prototype" must hold an
%% object; then V's chain is compared to it by identity. A proxy hop, an
%% accessor or absent "prototype", an own @@hasInstance, any other Ctor, or
%% more than 64 hops miss.
instance_of(Agent, V, {?HANDLE_TAG, CId}, Sym) ->
    Data = element(?STORE_DATA, element(?AGENT_STORE, Agent)),
    {?HANDLE_TAG, FP} =
        element(?PAIR_PROTO,
                element(?REALM_FUNCTION, element(?AGENT_REALM, Agent))),
    case array:get(CId, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            Kind = kind_tag(element(?SOBJECT_KIND, Slot)),
            case
                (Kind =:= ?KBYTECODE_TAG orelse Kind =:= ?KFN_TAG
                 orelse Kind =:= ?KNATIVE_TAG)
                andalso ordinary_has_instance(Data, Slot, FP, Sym, 64)
            of
                false -> miss;
                true ->
                    case V of
                        {?HANDLE_TAG, VId} ->
                            case element(?SOBJECT_PROPS, Slot) of
                                #{{?KEY_NAMED, <<"prototype">>} := Prop}
                                  when element(1, Prop) =:= ?DATAPROP_TAG ->
                                    case element(?DATAPROP_VALUE, Prop) of
                                        {?HANDLE_TAG, PId} ->
                                            chain_reaches(Data, VId, PId, 64);
                                        _ -> miss
                                    end;
                                _ -> miss
                            end;
                        _ -> false
                    end
            end;
        _ -> miss
    end;
instance_of(_, _, _, _) -> miss.

%% No own Sym on this cell or any plain hop above it short of FP (or the
%% end of the chain).
ordinary_has_instance(_, _, _, _, 0) -> false;
ordinary_has_instance(Data, Slot, FP, Sym, Fuel) ->
    (not lists:keymember(Sym, 1, element(?SOBJECT_SYMBOL_PROPS, Slot)))
        andalso case element(?SOBJECT_PROTO, Slot) of
                    ?NONE -> true;
                    {?SOME, {?HANDLE_TAG, FP}} -> true;
                    {?SOME, {?HANDLE_TAG, P}} -> plain_above(Data, P, FP, Sym, Fuel - 1);
                    _ -> false
                end.

plain_above(Data, P, FP, Sym, Fuel) ->
    case array:get(P, Data) of
        %% A shape holds string keys only: no own symbols on a shaped hop.
        {?SSHAPED_TAG, _, ?NONE, _} -> true;
        {?SSHAPED_TAG, _, {?SOME, {?HANDLE_TAG, FP}}, _} -> true;
        {?SSHAPED_TAG, _, {?SOME, {?HANDLE_TAG, Q}}, _} ->
            plain_above(Data, Q, FP, Sym, Fuel - 1);
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case kind_tag(element(?SOBJECT_KIND, Slot)) of
                ?PROXYOBJ_TAG -> false;
                _ -> ordinary_has_instance(Data, Slot, FP, Sym, Fuel)
            end;
        _ -> false
    end.

%% §7.3.22 step 7: whether the cell PId is on VId's prototype chain. A Proxy
%% hop ([[GetPrototypeOf]] is a trap) or fuel exhaustion miss.
chain_reaches(_, _, _, 0) -> miss;
chain_reaches(Data, VId, PId, Fuel) ->
    case array:get(VId, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG;
                  element(1, Slot) =:= ?SSHAPED_TAG ->
            case element(1, Slot) =:= ?SOBJECT_TAG
                 andalso kind_tag(element(?SOBJECT_KIND, Slot)) =:= ?PROXYOBJ_TAG of
                true -> miss;
                false ->
                    %% proto is element 3 of both cell shapes.
                    case element(?SOBJECT_PROTO, Slot) of
                        ?NONE -> false;
                        {?SOME, {?HANDLE_TAG, PId}} -> true;
                        {?SOME, {?HANDLE_TAG, Next}} ->
                            chain_reaches(Data, Next, PId, Fuel - 1);
                        _ -> miss
                    end
            end;
        _ -> miss
    end.
