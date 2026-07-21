%%% Spike FFI for the perf5 CFunRef re-enable investigation
%%% (test/emit_2core_cfunref_spike.gleam). Compiles .core text with an
%%% arbitrary extra-options list and CATCHES OTP compiler crashes so the
%%% spike can tabulate which pass falls over instead of taking the whole
%%% node down. See emit_2core_profile.gleam:713-715 for the crash this
%%% narrows: `'f'/N` values inside a large js_main → beam_ssa_opt
%%% ssa_opt_type_start badmatch.
-module(emit_2core_cfunref_spike_ffi).
-export([compile_core_opts/2, funref_count/1, otp_release/0]).

%% compile_core_opts(CoreBin, ExtraOpts) ->
%%     {compiled, ByteSize} | {rejected, [MsgBin]} | {crashed, Class, TopBin}
%%
%% CoreBin is `.core` source text. ExtraOpts is a list of atoms/tuples
%% appended to [from_core, binary, return_errors] (e.g. no_type_opt,
%% no_ssa_opt, no_copt, {inline_size,24}). The compile runs in-process
%% (`no_spawn_compiler_process`) so an OTP internal crash is a catchable
%% error in THIS process rather than a linked-process EXIT that unwinds
%% the spike. `crashed` returns the Class atom + a one-line rendering of
%% the head of the reason term (enough to identify the failing pass —
%% e.g. beam_ssa_opt / ssa_opt_type_start / {badmatch,…}).
compile_core_opts(CoreBin, ExtraOpts) when is_binary(CoreBin) ->
    Str = unicode:characters_to_list(CoreBin),
    case core_scan:string(Str) of
        {error, EI, _} -> {rejected, [fmt_one(EI)]};
        {ok, Toks, _} ->
            case core_parse:parse(Toks) of
                {error, EI} -> {rejected, [fmt_one(EI)]};
                {ok, CMod} ->
                    Base = [from_core, binary, return_errors,
                            return_warnings, no_spawn_compiler_process],
                    try compile:forms(CMod, Base ++ ExtraOpts) of
                        {ok, _M, Beam, _W} -> {compiled, byte_size(Beam)};
                        {ok, _M, Beam}     -> {compiled, byte_size(Beam)};
                        {error, Es, _W}    -> {rejected, fmt_errs(Es)}
                    catch
                        Class:Reason:Stk ->
                            {crashed, atom_to_binary(Class, utf8),
                             crash_head(Reason, Stk)}
                    end
            end
    end.

%% Render the crash reason down to a compact single-line binary that
%% names the failing pass. The compiler wraps pass crashes as
%% `{Pass, Reason}` (older) or surfaces them raw with the pass module
%% at the top of the stacktrace; try both.
crash_head(Reason, Stk) ->
    Pass = case Stk of
               [{M,_F,_A,_}|_] -> atom_to_binary(M, utf8);
               _ -> <<"?">>
           end,
    R = iolist_to_binary(io_lib:format("~0p", [trim(Reason, 4)])),
    <<Pass/binary, " :: ", R/binary>>.

%% Depth-limit a term so a giant SSA dump doesn't flood the spike output.
trim(T, 0) -> if is_tuple(T); is_list(T) -> '...'; true -> T end;
trim(T, D) when is_tuple(T) ->
    list_to_tuple([trim(E, D-1) || E <- tuple_to_list(T)]);
trim([H|T], D) -> [trim(H, D-1) | trim(T, D-1)];
trim(T, _) -> T.

fmt_errs(Errs) ->
    lists:flatten([[fmt_one(EI) || EI <- EIs] || {_F, EIs} <- Errs]).
fmt_one({Loc, Mod, Desc}) ->
    Msg = unicode:characters_to_binary(Mod:format_error(Desc)),
    <<(loc_bin(Loc))/binary, ": ", Msg/binary>>.
loc_bin({L,_})              -> integer_to_binary(L);
loc_bin(L) when is_integer(L) -> integer_to_binary(L);
loc_bin(none)               -> <<"module">>.

%% Rough count of bare `'name'/N` fun-ref VALUES in .core text — a ref
%% preceded by `= ` (RHS of a let/def binding), which is where
%% emit_make_closure places CFunRef. NOT a precise parse; just a cheap
%% sanity check that the CFunRef path actually fired before we blame
%% beam_ssa_opt (0 for the eta-wrapper baseline, ~40 for richards with
%% the gate flipped).
funref_count(CoreBin) ->
    {ok, RE} = re:compile(<<"= '[A-Za-z0-9_@]+'/\\d+ in">>),
    case re:run(CoreBin, RE, [global]) of
        nomatch -> 0;
        {match, Ms} -> length(Ms)
    end.

otp_release() ->
    list_to_binary(erlang:system_info(otp_release)).
