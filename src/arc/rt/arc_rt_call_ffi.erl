-module(arc_rt_call_ffi).
-export([t_call_protected/4, t_apply_protected/2, t_native_protected/4,
         mk_frame/4, t_kfn_code/3, birth_props/2]).

-include("arc_rt_layout.hrl").

t_kfn_code(St, {js_cell, Id}, This) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        Slot when element(1, Slot) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Slot) of
                {?KFN_TAG, Code, none, Flags, _, Simple, _, _, _}
                  when element(?FNFLAGS_IS_CLASS_CTOR, Flags) =:= false,
                       element(?FNFLAGS_IS_GEN, Flags) =:= false,
                       element(?FNFLAGS_IS_ASYNC, Flags) =:= false ->
                    %% §10.2.1.2 bind this, sloppy primitive this misses
                    case element(?FNFLAGS_IS_ARROW, Flags)
                         orelse element(?FNFLAGS_IS_STRICT, Flags) of
                        true -> {Code, This, Simple};
                        false when This =:= undefined; This =:= null ->
                            {Code,
                             element(?REALM_GLOBAL,
                                     element(?AGENT_REALM, St)),
                             Simple};
                        false when element(1, This) =:= ?HANDLE_TAG ->
                            {Code, This, Simple};
                        false -> undefined
                    end;
                _ -> undefined
            end;
        _ -> undefined
    end;
t_kfn_code(_, _, _) -> undefined.

t_call_protected(St, Code, Frame, Args) ->
    try Code(St, Frame, Args) of
        {V, St2} -> {{?COMPLETION_NORMAL, V}, St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{?COMPLETION_THROW, E}, St2}
    end.

t_native_protected(St, Tag, This, Args) ->
    try arc_rt_builtins_ffi:dispatch_native(St, Tag, This, Args) of
        {V, St2} -> {{?COMPLETION_NORMAL, V}, St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{?COMPLETION_THROW, E}, St2}
    end.

t_apply_protected(St, Body) ->
    try Body(St) of
        {V, St2} -> {{?COMPLETION_NORMAL, V}, St2}
    catch
        error:{wasm_exn, 0, [St2, E]} -> {{?COMPLETION_THROW, E}, St2}
    end.

mk_frame(This, ActiveFunc, HomeObj, NewTarget) ->
    {This, ActiveFunc, HomeObj, NewTarget}.

birth_props(LengthV, Name) ->
    #{{?KEY_NAMED, <<"length">>} =>
          {?DATAPROP_TAG, LengthV, false, false, true, 0},
      {?KEY_NAMED, <<"name">>} =>
          {?DATAPROP_TAG, Name, false, false, true, 1}}.
