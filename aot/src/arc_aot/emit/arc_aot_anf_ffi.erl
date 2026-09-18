-module(arc_aot_anf_ffi).
-export([str_parts/1]).

%% the runtime's canonical form for a string literal, spelled as constants
str_parts(S) ->
    case arc_rt_js_string_ffi:from_text(S) of
        B when is_binary(B) -> ascii;
        {js_str, B, Len, Crumbs} -> {tagged, B, Len, tuple_to_list(Crumbs)}
    end.
