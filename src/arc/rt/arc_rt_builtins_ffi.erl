%% shim so rt_call compiles before builtins exists
-module(arc_rt_builtins_ffi).
-export([dispatch_native/4, dispatch_native_construct/4]).

dispatch_native(St, Tag, This, Args) ->
    arc@rt@builtins:dispatch_native(St, Tag, This, Args).

dispatch_native_construct(St, Tag, Args, NewTarget) ->
    arc@rt@builtins:dispatch_native_construct(St, Tag, Args, NewTarget).
