%%% arc_interp_ffi — the bytecode interpreter's Erlang shims.
%%%
%%% Three families, all pure term work over threaded values (no process
%%% state, no NIF):
%%%
%%%  1. guardN / guard_unitN — the raise adapter between the raise-based
%%%     runtime (`arc_rt_store_ffi:t_throw/2` raises
%%%     `error:{wasm_exn, 0, [Agent, Thrown]}`) and the Result-based
%%%     interpreter. `guardN(F, Agent, A2..AN)` applies the arity-N runtime
%%%     function and answers `{ok, V, Agent2}` or `{threw, Agent2, E}`, which
%%%     is the wire form of `arc/interp/ffi.Guarded`. Callers pass literal
%%%     remote fun refs (`fun 'arc@rt@obj':t_get_prop/3`), so no closure is
%%%     allocated per call.
%%%
%%%  2. Fused hot-path kernels (truthy/1, get_field/3, put_elem/4, ...) that
%%%     match the raw JsVal wire term and the store records directly and
%%%     answer the result, or the atom `miss` when the operands need anything
%%%     observable (a getter, a proxy trap, a throw). They are TOTAL: no
%%%     clause raises for any wire term. The Gleam side types each kernel
%%%     with its hit type and checks `is_miss/1` before touching the result.
%%%     The operator kernels (add/2, lt/2, ...) live in arc_rt_ops_ffi.
%%%
%%%  3. setup_locals_tuple/6, setup_locals_seeded/10 — the one-pass locals
%%%     tuple build for a call prologue.
-module(arc_interp_ffi).

-export([guard1/2, guard2/3, guard3/4, guard4/5, guard5/6, guard6/7,
         guard7/8,
         guard_unit1/2, guard_unit2/3, guard_unit3/4, guard_unit4/5,
         guard_unit5/6, guard_unit6/7]).
-export([is_miss/1, is_tdz/1, is_undefined/1,
         truthy/1, lnot/1, nullish/1, typeof/1, typeof/2,
         box_get/2, cell_of/2, get_global/3, put_global/6, instance_of/4,
         get_field/3, get_elem/3, get_elem2/3, put_field/4, put_elem/4,
         define_field/4]).
-export([setup_locals_tuple/6, setup_locals_seeded/10]).

-include("arc_rt_layout.hrl").
%% The Named "length" PropertyKey term.
-define(LENGTH_KEY, {?KEY_NAMED, <<"length">>}).

%% ── 1. raise adapter ─────────────────────────────────────────────────────

%% guardN(F, St, A2..AN) -> {ok, V, St2} | {threw, St2, E}
%% F is a value-first runtime function `F(St, ..) -> {V, St2}`. Only the JS
%% exception term is caught; engine panics and other errors propagate. The
%% `of` arm runs outside the protected region.
guard1(F, St) ->
    try F(St) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard2(F, St, A) ->
    try F(St, A) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard3(F, St, A, B) ->
    try F(St, A, B) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard4(F, St, A, B, C) ->
    try F(St, A, B, C) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard5(F, St, A, B, C, D) ->
    try F(St, A, B, C, D) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard6(F, St, A, B, C, D, X) ->
    try F(St, A, B, C, D, X) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard7(F, St, A, B, C, D, X, Y) ->
    try F(St, A, B, C, D, X, Y) of {V, St2} -> {ok, V, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

%% guard_unitN(F, St, A2..AN) -> {ok, nil, St2} | {threw, St2, E}
%% Same, for runtime functions that return the bare Agent.
guard_unit1(F, St) ->
    try F(St) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit2(F, St, A) ->
    try F(St, A) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit3(F, St, A, B) ->
    try F(St, A, B) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit4(F, St, A, B, C) ->
    try F(St, A, B, C) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit5(F, St, A, B, C, D) ->
    try F(St, A, B, C, D) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

guard_unit6(F, St, A, B, C, D, X) ->
    try F(St, A, B, C, D, X) of St2 -> {ok, nil, St2}
    catch error:{wasm_exn, 0, [St2, E]} -> {threw, St2, E} end.

%% ── 2. fused kernels ─────────────────────────────────────────────────────

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

%% get_field(Agent, V, K) -> JsVal | miss
%% §10.1.8.1 OrdinaryGet for a Named string key on an object cell, walking
%% the prototype chain while every hop is an ordinary read: an own slot on
%% an SShapedObject, or an own DataProperty in an SObject's props map for a
%% kind whose named keys are not virtual. A string or number primitive
%% starts the walk at its realm wrapper prototype (String "length" is
%% answered directly, as is an Array cell's). Accessors, Proxy / module
%% namespace / TypedArray cells, a String object's "length", a dangling
%% handle, any other primitive, or more than 64 hops all miss. Absent on
%% the whole chain is `undefined`, exactly as OrdinaryGet answers.
%% K is the canonical `{named, KeyBin}` PropertyKey term the opcode carries
%% (the compiler emits Index keys for array-index strings), used as the
%% props-map key as is so no hop rebuilds it.
get_field(Agent, {?HANDLE_TAG, Id}, K) ->
    cell_field(element(?AGENT_STORE, Agent), Id, K);
get_field(_, Bin, ?LENGTH_KEY) when is_binary(Bin) ->
    arc_string_ffi:string_codepoint_length(Bin);
get_field(Agent, Bin, K) when is_binary(Bin) ->
    proto_field(Agent, ?REALM_STRING, K);
get_field(Agent, N, K) when is_number(N) ->
    proto_field(Agent, ?REALM_NUMBER, K);
get_field(_, _, _) -> miss.

%% get_global(Agent, Lex, NameBin) -> JsVal | miss
%% §9.1.1.4.6 GetBindingValue on the global Environment Record for the
%% plain case: an initialised lexical (let/const/class) binding from Lex
%% (the realm's `lexical_globals` map of {let|const, V}), else an own or
%% inherited plain data property of the global object, walked as get_field
%% walks. A binding in its TDZ, an accessor, an exotic hop, or a name absent
%% everywhere (ReferenceError, or "undefined" for typeof) miss.
get_global(Agent, Lex, Name) ->
    case Lex of
        #{Name := Binding} ->
            case element(2, Binding) of
                js_tdz -> miss;
                V -> V
            end;
        _ ->
            {?HANDLE_TAG, G} = element(?REALM_GLOBAL, element(?AGENT_REALM, Agent)),
            Store = element(?AGENT_STORE, Agent),
            field_walk(element(?STORE_DATA, Store), element(?STORE_SHAPES, Store),
                       G, {?KEY_NAMED, Name}, 64, miss)
    end.

%% put_global(Store, Lex, Global, NameBin, V, Strict) -> Store2 | miss
%% §9.1.1.4.5 SetMutableBinding, object-record half, as put_field on the
%% global object: an existing own writable data property is replaced; a
%% sloppy frame may also create it (strict must see ReferenceError). A
%% lexical binding of the name, or anything put_field misses on, miss.
put_global(Store, Lex, Global, Name, V, Strict) ->
    case is_map_key(Name, Lex) of
        true -> miss;
        false -> put_field(Store, Global, {?KEY_NAMED, Name}, V, not Strict)
    end.

%% A string / number primitive has no own named props besides String
%% "length", so a read walks the realm's wrapper prototype. Only a data
%% property answers here; a getter misses so the slow path can pass the
%% primitive as `this`.
proto_field(Agent, Which, K) ->
    Pair = element(Which, element(?AGENT_REALM, Agent)),
    {?HANDLE_TAG, Id} = element(?PAIR_PROTO, Pair),
    cell_field(element(?AGENT_STORE, Agent), Id, K).

cell_field(Store, Id, K) ->
    field_walk(element(?STORE_DATA, Store), element(?STORE_SHAPES, Store),
               Id, K, 64, undefined).

%% Absent is the answer when the whole chain lacks the key: `undefined` for
%% OrdinaryGet, `miss` for a global binding lookup.
field_walk(_, _, _, _, 0, _) -> miss;
field_walk(Data, Shapes, Id, K, Fuel, Absent) ->
    case array:get(Id, Data) of
        {?SSHAPED_TAG, Sid, Proto, Slots} ->
            case Shapes of
                #{Sid := Desc} ->
                    KeyBin = element(2, K),
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{KeyBin := Off} -> element(Off + 1, Slots);
                        _ -> field_next(Data, Shapes, Proto, K, Fuel, Absent)
                    end;
                _ -> miss
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            Kind = element(?SOBJECT_KIND, Slot),
            case named_plain(Kind, K) of
                false -> named_virtual(Kind, K);
                true ->
                    case element(?SOBJECT_PROPS, Slot) of
                        #{K := Prop} ->
                            case element(1, Prop) of
                                ?DATAPROP_TAG -> element(?DATAPROP_VALUE, Prop);
                                _ -> miss
                            end;
                        _ ->
                            field_next(Data, Shapes,
                                       element(?SOBJECT_PROTO, Slot),
                                       K, Fuel, Absent)
                    end
            end;
        _ -> miss
    end.

field_next(_, _, ?NONE, _, _, Absent) -> Absent;
field_next(Data, Shapes, {?SOME, {?HANDLE_TAG, P}}, K, Fuel, Absent) ->
    field_walk(Data, Shapes, P, K, Fuel - 1, Absent);
field_next(_, _, _, _, _, _) -> miss.

%% The one virtual named data property a read kernel synthesizes: an Array
%% cell's "length" IS its kind payload (§10.4.2, always an own data
%% property, so no chain walk). Every other non-plain named read misses.
named_virtual({?ARRAYOBJ_TAG, Length}, ?LENGTH_KEY) -> Length;
named_virtual(_, _) -> miss.

%% Whether a Named key on this ObjKind is a plain props-map entry for both
%% [[Get]] and [[Set]] (rt/obj own_property_of, get_from, set arms): Proxy,
%% module namespace and TypedArray cells are exotic for string keys, and
%% Array / String objects synthesize "length".
-compile({inline, [named_plain/2, named_virtual/2]}).
named_plain(?ORDINARY, _) -> true;
named_plain(Kind, _) when is_atom(Kind) -> true;
named_plain(Kind, K) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        ?ARRAYOBJ_TAG -> K =/= ?LENGTH_KEY;
        string_obj -> K =/= ?LENGTH_KEY;
        _ -> true
    end.

%% get_elem(Store, V, Key) -> JsVal | miss
%% `V[Key]` for the shapes a loop body produces: a non-negative integer
%% index into an Array or Arguments cell (own element present, no
%% {index,_} props override; holes miss so the full path does the proto
%% walk), an array index on an ordinary props-only cell (an own data
%% property, or `undefined` when the whole plain chain lacks it), or a
%% string key, which canonicalizes and reads as get_field / an index.
%% Anything else (float or negative index, symbol, object key, an exotic
%% cell) misses.
-define(MAX_ARRAY_INDEX, 4294967294).
get_elem(Store, {?HANDLE_TAG, Id}, Idx) when is_integer(Idx), Idx >= 0 ->
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            Props = element(?SOBJECT_PROPS, Slot),
            case element(?SOBJECT_KIND, Slot) of
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    case index_overridden(Props, Idx) of
                        true -> miss;
                        false -> elem_read(element(?SOBJECT_ELEMENTS, Slot), Idx)
                    end;
                {?ARRAYOBJ_TAG, _} -> miss;
                {?ARGUMENTSOBJ_TAG, _, _} ->
                    case index_overridden(Props, Idx) of
                        true -> miss;
                        false -> elem_read(element(?SOBJECT_ELEMENTS, Slot), Idx)
                    end;
                Kind when Idx =< ?MAX_ARRAY_INDEX ->
                    case index_is_plain(Kind) of
                        false -> miss;
                        true ->
                            case Props of
                                #{{?KEY_INDEX, Idx} := Prop} ->
                                    case element(1, Prop) of
                                        ?DATAPROP_TAG -> element(?DATAPROP_VALUE, Prop);
                                        _ -> miss
                                    end;
                                _ ->
                                    case index_free(Data, element(?STORE_SHAPES, Store),
                                                    element(?SOBJECT_PROTO, Slot),
                                                    Idx, 64) of
                                        true -> undefined;
                                        false -> miss
                                    end
                            end
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
get_elem(Store, {?HANDLE_TAG, _} = Obj, Key) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
        {?OKEY_STRING, {?KEY_NAMED, _} = K} ->
            cell_field(Store, element(?HANDLE_ID, Obj), K);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> get_elem(Store, Obj, Idx);
        _ -> miss
    end;
get_elem(_, _, _) -> miss.

%% get_elem2(Store, V, Key) -> JsVal | miss
%% get_elem for GetElem2, which also re-pushes the canonical key: only an
%% integer key is its own canonical value, so any other key misses.
get_elem2(Store, Obj, Idx) when is_integer(Idx) -> get_elem(Store, Obj, Idx);
get_elem2(_, _, _) -> miss.

%% An {index,Idx} props entry shadowing the elements store (a defineProperty'd
%% element). An array's props map is nearly always empty, which map_size
%% sees without building the key.
-compile({inline, [index_overridden/2]}).
index_overridden(Props, Idx) ->
    map_size(Props) =/= 0 andalso is_map_key({?KEY_INDEX, Idx}, Props).

elem_read({?ELEMS_DENSE, A}, Idx) ->
    case Idx < array:size(A) of
        true ->
            case array:get(Idx, A) of
                ?ELEMS_HOLE -> miss;
                V -> V
            end;
        false -> miss
    end;
elem_read({?ELEMS_SPARSE, M}, Idx) ->
    case M of
        #{Idx := V} -> V;
        _ -> miss
    end;
elem_read(_, _) -> miss.

%% put_field(Store, V, K, Val) -> Store2 | miss
%% §10.1.9.2 OrdinarySetWithOwnDescriptor for a kind whose named keys are
%% ordinary. Step 2, an EXISTING own writable data property: overwrite the
%% SShapedObject slot, or replace the value inside the DataProperty
%% (attributes and creation seq kept, §10.1.11). Step 1 → 2.c-h, CREATION
%% on an extensible SObject: only when the prototype chain holds nothing
%% at the key but plain writable data (chain_free), so a setter or a
%% read-only property up the chain still takes the slow path; the new
%% {W,E,C} property is stamped with the store's prop_seq (t_next_prop_seq).
%% Non-writable, accessors, non-extensible / shaped receivers for a new key
%% and exotic receivers miss. Returns the rebuilt store.
put_field(Store, Obj, K, V) -> put_field(Store, Obj, K, V, true).

%% Create: whether an absent key may be created (false: replace only).
put_field(Store, {?HANDLE_TAG, Id}, K, V, Create) ->
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
        {?SSHAPED_TAG, Sid, P, Slots} ->
            case element(?STORE_SHAPES, Store) of
                #{Sid := Desc} ->
                    KeyBin = element(2, K),
                    case element(?SHAPE_OFFSETS, Desc) of
                        #{KeyBin := Off} ->
                            NewSlot = {?SSHAPED_TAG, Sid, P,
                                       setelement(Off + 1, Slots, V)},
                            setelement(?STORE_DATA, Store, array:set(Id, NewSlot, Data));
                        _ -> miss
                    end;
                _ -> miss
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case named_plain(element(?SOBJECT_KIND, Slot), K) of
                false -> miss;
                true -> put_prop(Store, Data, Id, Slot, K, V, Create)
            end;
        _ -> miss
    end;
put_field(_, _, _, _, _) -> miss.

%% put_prop(Store, Data, Id, Slot, K, V, Create) -> Store2 | miss
%% The props-map half of put_field / put_elem for an SObject whose lookup
%% of K ({named,Bin} or {index,Idx}) is a plain props probe: replace the
%% value of an existing own writable data property, or (Create) add a
%% {W,E,C} one stamped with the store's prop_seq when the receiver is
%% extensible and the chain above holds nothing but writable data at K.
put_prop(Store, Data, Id, Slot, K, V, Create) ->
    Props = element(?SOBJECT_PROPS, Slot),
    case Props of
        #{K := Prop}
          when element(1, Prop) =:= ?DATAPROP_TAG,
               element(?DATAPROP_WRITABLE, Prop) =:= true ->
            NewProps = Props#{K := setelement(?DATAPROP_VALUE, Prop, V)},
            NewSlot = setelement(?SOBJECT_PROPS, Slot, NewProps),
            setelement(?STORE_DATA, Store, array:set(Id, NewSlot, Data));
        #{K := _} -> miss;
        _ when Create, element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            case chain_free(Data, element(?STORE_SHAPES, Store),
                            element(?SOBJECT_PROTO, Slot), K) of
                false -> miss;
                true ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    Prop = {?DATAPROP_TAG, V, true, true, true, Seq},
                    NewSlot = setelement(?SOBJECT_PROPS, Slot, Props#{K => Prop}),
                    arc_rt_obj_ffi:store_put_seq(Store, array:set(Id, NewSlot, Data),
                                                 Seq + 1)
            end;
        _ -> miss
    end.

%% define_field(Store, V, K, Val) -> Store2 | miss
%% §7.3.5 CreateDataProperty of a Named key on an ordinary, extensible
%% SObject (the `{key: v}` literal field): a fresh {W,E,C} data property
%% stamped with the store's prop_seq, or an in-place replacement of a
%% configurable data property (creation order kept, §10.1.11). A
%% non-configurable or accessor current property, and any other receiver,
%% miss to the full [[DefineOwnProperty]].
define_field(Store, {?HANDLE_TAG, Id}, K, V) ->
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_KIND, Slot) =:= ?ORDINARY,
                  element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            Props = element(?SOBJECT_PROPS, Slot),
            case Props of
                #{K := Old} when element(1, Old) =:= ?DATAPROP_TAG,
                                 element(?DATAPROP_CONFIGURABLE, Old) =:= true ->
                    Prop = {?DATAPROP_TAG, V, true, true, true,
                            element(?DATAPROP_SEQ, Old)},
                    NewSlot = setelement(?SOBJECT_PROPS, Slot, Props#{K := Prop}),
                    setelement(?STORE_DATA, Store, array:set(Id, NewSlot, Data));
                #{K := _} -> miss;
                _ ->
                    Seq = element(?STORE_PROP_SEQ, Store),
                    Prop = {?DATAPROP_TAG, V, true, true, true, Seq},
                    NewSlot = setelement(?SOBJECT_PROPS, Slot, Props#{K => Prop}),
                    arc_rt_obj_ffi:store_put_seq(Store, array:set(Id, NewSlot, Data),
                                                 Seq + 1)
            end;
        _ -> miss
    end;
define_field(_, _, _, _) -> miss.

chain_free(Data, Shapes, Proto, {?KEY_NAMED, _} = K) ->
    arc_rt_obj_ffi:named_free(Data, Shapes, Proto, K, 64);
chain_free(Data, Shapes, Proto, {?KEY_INDEX, Idx}) ->
    index_free(Data, Shapes, Proto, Idx, 64).

%% put_elem(Store, V, Idx, Val) -> Store2 | miss
%% `V[Idx] = Val` on an extensible Array cell for an array index Idx
%% (0 =< Idx =< 2^32-2, rt_types.max_array_index) in [0, Length].
%% Overwriting a present element is a write to an own writable data
%% property. Filling a hole or appending at Idx == Length creates a
%% property, so it first needs the prototype chain to hold nothing at Idx
%% (a setter or read-only index up the chain takes the store, §10.1.9.2
%% step 2) and, for the append, a writable "length" (§10.4.2.1 step 2.h).
%% An ordinary props-only receiver takes the put_field write under the
%% {index,Idx} key; a string key canonicalizes to one of the two. An
%% {index,Idx} props override on an array, a non-extensible or exotic
%% receiver, a key past the array-index range (2^32-1 is a Named key and
%% never moves "length"), or a dense fill past the allocated size misses.
put_elem(Store, {?HANDLE_TAG, Id}, Idx, V)
  when is_integer(Idx), Idx >= 0, Idx =< ?MAX_ARRAY_INDEX ->
    Data = element(?STORE_DATA, Store),
    case array:get(Id, Data) of
        Slot when tuple_size(Slot) =:= ?SOBJECT_ARITY,
                  element(1, Slot) =:= ?SOBJECT_TAG,
                  element(?SOBJECT_EXTENSIBLE, Slot) =:= true ->
            Props = element(?SOBJECT_PROPS, Slot),
            case element(?SOBJECT_KIND, Slot) of
                Kind when is_atom(Kind) ->
                    put_prop(Store, Data, Id, Slot, {?KEY_INDEX, Idx}, V, true);
                _ when map_size(Props) =/= 0
                       andalso is_map_key({?KEY_INDEX, Idx}, Props) -> miss;
                {?ARRAYOBJ_TAG, Length} when Idx < Length ->
                    Elems = element(?SOBJECT_ELEMENTS, Slot),
                    case elem_has(Elems, Idx)
                         orelse index_free(Data, element(?STORE_SHAPES, Store),
                                           element(?SOBJECT_PROTO, Slot), Idx, 64) of
                        false -> miss;
                        true ->
                            case elem_write(Elems, Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewSlot = setelement(?SOBJECT_ELEMENTS, Slot, NewE),
                                    setelement(?STORE_DATA, Store,
                                               array:set(Id, NewSlot, Data))
                            end
                    end;
                {?ARRAYOBJ_TAG, Idx} ->
                    case length_writable(Props)
                         andalso index_free(Data, element(?STORE_SHAPES, Store),
                                            element(?SOBJECT_PROTO, Slot), Idx, 64) of
                        false -> miss;
                        true ->
                            case elem_write_grow(element(?SOBJECT_ELEMENTS, Slot), Idx, V) of
                                miss -> miss;
                                NewE ->
                                    NewSlot = setelement(?SOBJECT_ELEMENTS,
                                        setelement(?SOBJECT_KIND, Slot,
                                                   {?ARRAYOBJ_TAG, Idx + 1}),
                                        NewE),
                                    setelement(?STORE_DATA, Store,
                                               array:set(Id, NewSlot, Data))
                            end
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
put_elem(Store, {?HANDLE_TAG, _} = Obj, Key, V) when is_binary(Key) ->
    case arc_rt_val_ffi:t_to_property_key_fast(Key) of
        {?OKEY_STRING, {?KEY_NAMED, _} = K} -> put_field(Store, Obj, K, V);
        {?OKEY_STRING, {?KEY_INDEX, Idx}} -> put_elem(Store, Obj, Idx, V);
        _ -> miss
    end;
put_elem(_, _, _, _) -> miss.

%% The Array "length" attribute override, when defineProperty made one;
%% absent means the default writable length.
length_writable(#{?LENGTH_KEY := Prop})
  when element(1, Prop) =:= ?DATAPROP_TAG ->
    element(?DATAPROP_WRITABLE, Prop) =:= true;
length_writable(_) -> true.

%% index_free(Data, Shapes, Proto, Idx, Fuel) -> boolean()
%% No object on the prototype chain starting at Proto has an own property
%% at Idx, along hops whose index lookup is a pure props/elements probe.
%% A Proxy, String, TypedArray or namespace hop, a dangling handle, or more
%% than Fuel hops answer false.
index_free(_, _, ?NONE, _, _) -> true;
index_free(_, _, _, _, 0) -> false;
index_free(Data, Shapes, {?SOME, {?HANDLE_TAG, P}}, Idx, Fuel) ->
    case array:get(P, Data) of
        {?SSHAPED_TAG, Sid, Proto, _Slots} ->
            case Shapes of
                #{Sid := Desc} ->
                    (not is_map_key(integer_to_binary(Idx),
                                    element(?SHAPE_OFFSETS, Desc)))
                        andalso index_free(Data, Shapes, Proto, Idx, Fuel - 1);
                _ -> false
            end;
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            index_is_plain(element(?SOBJECT_KIND, Slot))
                andalso (not is_map_key({?KEY_INDEX, Idx},
                                        element(?SOBJECT_PROPS, Slot)))
                andalso (not elem_has(element(?SOBJECT_ELEMENTS, Slot), Idx))
                andalso index_free(Data, Shapes, element(?SOBJECT_PROTO, Slot),
                                   Idx, Fuel - 1);
        _ -> false
    end;
index_free(_, _, _, _, _) -> false.

%% Whether an Index key on this ObjKind is answered by the props map plus
%% the elements store alone (rt/obj own_property_of): Proxy and namespace
%% cells trap, String objects expose their code units, TypedArrays their
%% buffer.
index_is_plain(Kind) when is_atom(Kind) -> true;
index_is_plain(Kind) ->
    case element(1, Kind) of
        ?PROXYOBJ_TAG -> false;
        module_namespace -> false;
        typed_array_obj -> false;
        string_obj -> false;
        _ -> true
    end.

%% A present (non-hole) element at Idx.
elem_has({?ELEMS_DENSE, A}, Idx) ->
    Idx < array:size(A) andalso array:get(Idx, A) =/= ?ELEMS_HOLE;
elem_has({?ELEMS_SPARSE, M}, Idx) -> is_map_key(Idx, M);
elem_has(_, _) -> false.

elem_write({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx < array:size(A) of
        true -> {?ELEMS_DENSE, array:set(Idx, V, A)};
        false -> miss
    end;
elem_write({?ELEMS_SPARSE, M}, Idx, V) -> {?ELEMS_SPARSE, M#{Idx => V}};
elem_write(_, _, _) -> miss.

%% Append at Idx == Length. A dense array:set/3 extends past size(A) itself;
%% the gap and size bounds are rt/elements' dense-promotion policy, past
%% which the write belongs to the sparse representation (miss). An empty
%% store starts dense the way rt/elements `set` does.
-define(MAX_GAP, 1024).
-define(MAX_DENSE_INDEX, 10000000).
elem_write_grow({?ELEMS_DENSE, A}, Idx, V) ->
    case Idx - array:size(A) =< ?MAX_GAP andalso Idx < ?MAX_DENSE_INDEX of
        true -> {?ELEMS_DENSE, array:set(Idx, V, A)};
        false -> miss
    end;
elem_write_grow({?ELEMS_SPARSE, M}, Idx, V) -> {?ELEMS_SPARSE, M#{Idx => V}};
elem_write_grow(?ELEMS_NONE, Idx, V) when Idx =< ?MAX_GAP ->
    {?ELEMS_DENSE, array:set(Idx, V, array:new({default, ?ELEMS_HOLE}))};
elem_write_grow(_, _, _) -> miss.

%% ── 3. locals tuple build ────────────────────────────────────────────────

%% Build the locals tuple for a JS function call in one forward pass:
%%   [Env..., Seeds..., Args(padded/truncated to Arity)..., Undef × rest]
%% One body-recursive build + list_to_tuple instead of append / reverse
%% chains. Env is the closure's captured environment, a list or a tuple of
%% values. local_count is compiler-bounded, so non-tail recursion is fine.
setup_locals_tuple({}, [], Args, Arity, Arity, _Undef) when length(Args) =:= Arity ->
    list_to_tuple(Args);
setup_locals_tuple(Env, Seeds, Args, Arity, LocalCount, Undef) when is_tuple(Env) ->
    setup_locals_tuple(tuple_to_list(Env), Seeds, Args, Arity, LocalCount, Undef);
setup_locals_tuple(Env, Seeds, Args, Arity, LocalCount, Undef) ->
    list_to_tuple(locals_env(Env, Seeds, Args, Arity, LocalCount, Undef)).

%% Non-arrow locals build. Lexical is an arc/bytecode/lexical.LexicalSlots term
%% (that module owns the ordering, see lexical.all_lexical_refs):
%%   {owned_lexical_slots, Base} — all four owned, contiguous, in canonical
%%       order [this, active_func, home_object, new_target] starting at
%%       Base (== length(Env)); the hot clause writes the seeds inline right
%%       after the env values.
%%   no_lexical_slots — none at all.
%% `captured_lexical_slots` belongs to arrows, which go through
%% setup_locals_tuple/6; it is left unmatched on purpose: seeding call-time
%% values into captured slots (which hold parent box refs at
%% non-contiguous indices) would be silently wrong.
%% No env, every arg supplied, no extra locals: the tuple is the seeds
%% followed by the args as given.
setup_locals_seeded({}, {owned_lexical_slots, _Base},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, _Undef)
        when LocalCount =:= Arity + 4, length(Args) =:= Arity ->
    list_to_tuple([This, FnObj, Home, NT | Args]);
%% No env: the seeds lead, then the args padded/truncated to the local count.
setup_locals_seeded({}, {owned_lexical_slots, _Base},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef)
        when LocalCount >= 4 ->
    list_to_tuple([This, FnObj, Home, NT
                   | locals_args(Args, Arity, LocalCount - 4, Undef)]);
setup_locals_seeded(Env, Lexical, This, FnObj, Home, NT, Args, Arity,
                    LocalCount, Undef) when is_tuple(Env) ->
    setup_locals_seeded(tuple_to_list(Env), Lexical, This, FnObj, Home, NT,
                        Args, Arity, LocalCount, Undef);
setup_locals_seeded(Env, {owned_lexical_slots, _Base},
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef)
        when LocalCount >= 4 ->
    list_to_tuple(locals_env4(Env, This, FnObj, Home, NT, Args, Arity,
                              LocalCount, Undef));
setup_locals_seeded(Env, Lexical,
                    This, FnObj, Home, NT, Args, Arity, LocalCount, Undef) ->
    {LT, LA, LH, LN} = case Lexical of
        {owned_lexical_slots, B} ->
            {{some, B}, {some, B + 1}, {some, B + 2}, {some, B + 3}};
        no_lexical_slots -> {none, none, none, none}
    end,
    S0 = seed(LN, NT, []),
    S1 = seed(LH, Home, S0),
    S2 = seed(LA, FnObj, S1),
    Seeds = seed(LT, This, S2),
    list_to_tuple(locals_env(Env, Seeds, Args, Arity, LocalCount, Undef)).

seed(none, _Value, Acc) -> Acc;
seed({some, _Idx}, Value, Acc) -> [Value | Acc].

locals_env4([E | Env], This, FnObj, Home, NT, Args, Arity, N, Undef) when N > 4 ->
    [E | locals_env4(Env, This, FnObj, Home, NT, Args, Arity, N - 1, Undef)];
locals_env4([], This, FnObj, Home, NT, Args, Arity, N, Undef) when N >= 4 ->
    [This, FnObj, Home, NT | locals_args(Args, Arity, N - 4, Undef)];
locals_env4(Env, This, FnObj, Home, NT, Args, Arity, N, Undef) ->
    %% local_count exhausted mid-env (compiler bounds local_count, so
    %% unreachable in practice): fall back to the generic truncation.
    locals_env(Env, [This, FnObj, Home, NT], Args, Arity, N, Undef).

locals_env(_, _, _, _, 0, _) -> [];
locals_env([E | Env], Seeds, Args, Arity, N, Undef) ->
    [E | locals_env(Env, Seeds, Args, Arity, N - 1, Undef)];
locals_env([], Seeds, Args, Arity, N, Undef) ->
    locals_seeds(Seeds, Args, Arity, N, Undef).

locals_seeds(_, _, _, 0, _) -> [];
locals_seeds([S | Seeds], Args, Arity, N, Undef) ->
    [S | locals_seeds(Seeds, Args, Arity, N - 1, Undef)];
locals_seeds([], Args, Arity, N, Undef) ->
    locals_args(Args, Arity, N, Undef).

locals_args(_, _, 0, _) -> [];
locals_args(_, 0, N, Undef) -> locals_pad(N, Undef);
%% Every parameter supplied, no extras: the args as given, then the pad.
locals_args(Args, Arity, N, Undef) when N >= Arity, length(Args) =:= Arity ->
    Args ++ locals_pad(N - Arity, Undef);
locals_args([A | Args], Arity, N, Undef) ->
    [A | locals_args(Args, Arity - 1, N - 1, Undef)];
locals_args([], Arity, N, Undef) ->
    [Undef | locals_args([], Arity - 1, N - 1, Undef)].

locals_pad(0, _) -> [];
locals_pad(N, Undef) -> [Undef | locals_pad(N - 1, Undef)].
