%% AtomVM shim, because AtomVM doesn't ship OTP's rand module. Arc only uses
%% uniform/0 for Math.random, which scales atomvm:random/0's 32-bit unsigned
%% result to [0.0, 1.0).
-module(rand).
-export([uniform/0]).

uniform() ->
    atomvm:random() / 4294967296.
