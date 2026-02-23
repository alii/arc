-module(arc_vm_ffi).
-export([float_power/2, math_sqrt/1, math_log/1, math_sin/1, math_cos/1, math_floor/1, math_ceil/1]).

float_power(Base, Exp) ->
    math:pow(Base, Exp).

math_sqrt(X) -> math:sqrt(X).
math_log(X) -> math:log(X).
math_sin(X) -> math:sin(X).
math_cos(X) -> math:cos(X).
math_floor(X) -> math:floor(X).
math_ceil(X) -> math:ceil(X).
