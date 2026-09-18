%% protected calls and frames; t_direct_callee may answer miss
-module(arc_rt_call_ffi).
-export([try_call_code/4, try_run/2, try_call_native/4,
         mk_frame/4, t_direct_callee/3, birth_props/2]).

-include("arc_rt_layout.hrl").

t_direct_callee(St, {?HANDLE_TAG, Id}, This) ->
    Store = element(?AGENT_STORE, St),
    case arc_rt_arena_ffi:get(Id, element(?STORE_CELLS, Store)) of
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

try_call_code(St, Code, Frame, Args) -> ?PROTECT(Code(St, Frame, Args)).

try_call_native(St, Token, This, Args) ->
    ?PROTECT('arc@rt@builtins':dispatch_native(St, Token, This, Args)).

try_run(St, Body) -> ?PROTECT(Body(St)).

mk_frame(This, ActiveFunc, HomeObj, NewTarget) ->
    ?FRAME(This, ActiveFunc, HomeObj, NewTarget).

birth_props(LengthV, Name) ->
    #{{?KEY_NAMED, <<"length">>} =>
          {?DATAPROPERTY_TAG, LengthV, false, false, true, 0},
      {?KEY_NAMED, <<"name">>} =>
          {?DATAPROPERTY_TAG, Name, false, false, true, 1}}.
