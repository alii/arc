%% AtomVM shim — OTP's rand module isn't shipped.
%% Arc only uses uniform/0 (for Math.random). atomvm:random/0 returns
%% a 32-bit unsigned; scale to [0.0, 1.0).
-module(rand).
-export([uniform/0]).

uniform() ->
    atomvm:random() / 4294967296.
