-module(arc_aot_pp_ffi).
-export([forms_to_erl/1, pmap/2]).

forms_to_erl(Forms) when is_list(Forms) ->
    Parts = pmap(Forms, fun(F) ->
                     unicode:characters_to_binary(
                       [erl_pp:form(F, [{encoding, unicode}]), $\n])
                 end),
    erlang:iolist_to_binary(Parts).

%% fresh process per item keeps atomvm gc cheap
pmap(Items, F) when is_function(F, 1), is_list(Items) ->
    Self = self(),
    Refs = [begin
                Ref = make_ref(),
                spawn_opt(fun() ->
                              Self ! {Ref, try {ok, F(Item)}
                                           catch C:R:St -> {error, {C, R, St}}
                                           end}
                          end, []),
                Ref
            end || Item <- Items],
    [receive
         {Ref, {ok, V}} -> V;
         {Ref, {error, {C, R, St}}} -> erlang:raise(C, R, St)
     end || Ref <- Refs].

