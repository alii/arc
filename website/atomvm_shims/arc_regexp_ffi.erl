%% AtomVM shim — no `re` module (PCRE) in AtomVM, so no RegExp in the browser
%% playground. Same API as the real arc_regexp_ffi (regexp.gleam's @externals):
%% compiling "succeeds" with a marker, and the first attempt to match raises a
%% clear error the playground surfaces as-is — better than the alternative,
%% which is regexes that silently never match.
-module(arc_regexp_ffi).
-export([regexp_compile/2, is_compiled/1, regexp_exec_compiled/4, regexp_exec_info/5, pair_trail/1]).

regexp_compile(_Pattern, _Flags) ->
    {error, {pattern_compile_failed, unsupported_message()}}.

is_compiled({error, {pattern_compile_failed, _}}) -> true;
is_compiled(_) -> false.

regexp_exec_compiled(_Compiled, _String, _Offset, _Sticky) ->
    erlang:error({regexp_unsupported, unsupported_message()}).

regexp_exec_info(_Pattern, _Flags, _String, _Offset, _Sticky) ->
    erlang:error({regexp_unsupported, unsupported_message()}).

pair_trail(_) ->
    erlang:error({regexp_unsupported, unsupported_message()}).

unsupported_message() ->
    <<"RegExp is not available in the browser playground yet: AtomVM has no "
      "PCRE (`re`) module. Everything else works.">>.
