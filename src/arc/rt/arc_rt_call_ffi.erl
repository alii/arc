%% protected calls and frames; t_direct_callee may answer miss
-module(arc_rt_call_ffi).
-export([t_call_protected/4, t_apply_protected/2, t_native_protected/4,
         mk_frame/4, t_direct_callee/3, birth_props/2]).

-include("arc_rt_layout.hrl").

t_direct_callee(St, {?HANDLE_TAG, Id}, This) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_DATA, Store)) of
        Cell when element(1, Cell) =:= ?SOBJECT_TAG ->
            case element(?SOBJECT_KIND, Cell) of
                ?COMPILEDFN(Code, ?NONE, Flags, _, DirectEntry) when ?IS_PLAIN_FN(Flags) ->
                    %% §10.2.1.2 bind this, sloppy primitive this misses
                    case element(?FNFLAGS_IS_ARROW, Flags)
                         orelse element(?FNFLAGS_IS_STRICT, Flags) of
                        true -> {Code, This, DirectEntry};
                        false when This =:= undefined; This =:= null ->
                            {Code,
                             element(?REALM_GLOBAL,
                                     element(?AGENT_REALM, St)),
                             DirectEntry};
                        false when element(1, This) =:= ?HANDLE_TAG ->
                            {Code, This, DirectEntry};
                        false -> miss
                    end;
                _ -> miss
            end;
        _ -> miss
    end;
t_direct_callee(_, _, _) -> miss.

%% runs body under the js guard, answering a Completion
-define(PROTECT(Body),
        try Body of
            {V, St2} -> {{?COMPLETION_NORMAL, V}, St2}
        catch
            error:?JS_THROW(St2, E) -> {{?COMPLETION_THROW, E}, St2}
        end).

t_call_protected(St, Code, Frame, Args) -> ?PROTECT(Code(St, Frame, Args)).

t_native_protected(St, Token, This, Args) ->
    ?PROTECT('arc@rt@builtins':dispatch_native(St, Token, This, Args)).

t_apply_protected(St, Body) -> ?PROTECT(Body(St)).

mk_frame(This, ActiveFunc, HomeObj, NewTarget) ->
    ?FRAME(This, ActiveFunc, HomeObj, NewTarget).

birth_props(LengthV, Name) ->
    #{{?KEY_NAMED, <<"length">>} =>
          {?DATAPROP_TAG, LengthV, false, false, true, 0},
      {?KEY_NAMED, <<"name">>} =>
          {?DATAPROP_TAG, Name, false, false, true, 1}}.
