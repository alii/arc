-module(arc_interp_guard_ffi).
-export([guard1/2, guard2/3, guard3/4, guard4/5, guard5/6, guard6/7,
         guard7/8,
         guard_unit1/2, guard_unit2/3, guard_unit3/4, guard_unit4/5,
         guard_unit5/6, guard_unit6/7]).

%% catches only the js exception, engine errors propagate
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

%% same for functions returning the bare agent
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
