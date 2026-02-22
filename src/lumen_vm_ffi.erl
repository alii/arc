-module(lumen_vm_ffi).
-export([float_power/2]).

float_power(Base, Exp) ->
    math:pow(Base, Exp).
