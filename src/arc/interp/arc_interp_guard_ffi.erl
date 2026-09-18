-module(arc_interp_guard_ffi).
-export([guard1/2, guard2/3, guard3/4, guard4/5, guard5/6, guard7/8,
         guard_unit1/2, guard_unit3/4, guard_unit4/5, guard_unit5/6,
         guard_unit6/7]).

-include("../rt/arc_rt_layout.hrl").

%% catches only the js exception, engine errors propagate
guard1(F, St) ->
    try F(St) of {V, St2} -> {value, V, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard2(F, St, A) ->
    try F(St, A) of {V, St2} -> {value, V, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard3(F, St, A, B) ->
    try F(St, A, B) of {V, St2} -> {value, V, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard4(F, St, A, B, C) ->
    try F(St, A, B, C) of {V, St2} -> {value, V, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard5(F, St, A, B, C, D) ->
    try F(St, A, B, C, D) of {V, St2} -> {value, V, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard7(F, St, A, B, C, D, X, Y) ->
    try F(St, A, B, C, D, X, Y) of {V, St2} -> {value, V, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

%% same for functions returning the bare agent
guard_unit1(F, St) ->
    try F(St) of St2 -> {value, nil, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard_unit3(F, St, A, B) ->
    try F(St, A, B) of St2 -> {value, nil, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard_unit4(F, St, A, B, C) ->
    try F(St, A, B, C) of St2 -> {value, nil, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard_unit5(F, St, A, B, C, D) ->
    try F(St, A, B, C, D) of St2 -> {value, nil, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.

guard_unit6(F, St, A, B, C, D, X) ->
    try F(St, A, B, C, D, X) of St2 -> {value, nil, St2}
    catch error:?JS_THROW(St2, E) -> {thrown, E, St2} end.
