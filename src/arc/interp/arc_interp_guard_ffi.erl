%%% arc_interp_guard_ffi — the raise adapter between the raise-based runtime
%%% (`arc_rt_store_ffi:t_throw/2` raises `error:{wasm_exn, 0, [Agent, Thrown]}`)
%%% and the Result-based interpreter. `guardN(F, Agent, A2..AN)` applies the
%%% arity-N runtime function and answers `{ok, V, Agent2}` or
%%% `{threw, Agent2, E}`, the wire form of `arc/interp/ffi.Guarded`. Callers
%%% pass literal remote fun refs (`fun 'arc@rt@obj':t_get_prop/3`), so no
%%% closure is allocated per call. Pure term work: no process state, no NIF.
-module(arc_interp_guard_ffi).
-export([guard1/2, guard2/3, guard3/4, guard4/5, guard5/6, guard6/7,
         guard7/8,
         guard_unit1/2, guard_unit2/3, guard_unit3/4, guard_unit4/5,
         guard_unit5/6, guard_unit6/7]).

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
