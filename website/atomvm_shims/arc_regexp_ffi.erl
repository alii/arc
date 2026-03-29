%% AtomVM shim — no `re` module. RegExp throws a clear error in the
%% playground rather than crashing the VM on undef.
-module(arc_regexp_ffi).
-export([regexp_test/3, regexp_exec/4]).

regexp_test(_Pattern, _Flags, _String) ->
    false.

regexp_exec(_Pattern, _Flags, _String, _Offset) ->
    {error, nil}.
