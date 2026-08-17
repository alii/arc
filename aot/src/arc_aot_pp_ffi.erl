-module(arc_aot_pp_ffi).
-export([forms_to_erl/1, pmap/2]).

%% Pretty-print Erlang abstract forms as UTF-8 source text, one form per
%% short-lived process (see `pmap`). Unicode mode (not erl_pp:form/1's latin1
%% default) both because the output is UTF-8 and because it keeps erl_pp on the
%% io_lib functions AtomVM's stdlib implements.
forms_to_erl(Forms) when is_list(Forms) ->
    Parts = pmap(Forms, fun(F) ->
                     unicode:characters_to_binary(
                       [erl_pp:form(F, [{encoding, unicode}]), $\n])
                 end),
    erlang:iolist_to_binary(Parts).

%% Map F over Items, each element in its own fresh process, results in order.
%%
%% Why: AtomVM's copying GC makes every allocation cost O(live heap), so a
%% single process building a large output crawls (measured 20x slower than
%% the split form for erl_pp on the playground). Small processes keep each
%% live heap small; results that are binaries are refc (off-heap) so sending
%% them back is cheap. A worker crash is re-raised in the caller.
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

