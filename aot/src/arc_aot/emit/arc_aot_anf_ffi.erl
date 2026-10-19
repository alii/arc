-module(arc_aot_anf_ffi).
-export([str_parts/1, has_function/1]).

%% the runtime's canonical form for a string literal, spelled as constants
str_parts(S) ->
    case arc_rt_js_string_ffi:from_text(S) of
        B when is_binary(B) -> ascii;
        {js_str, B, Len, Crumbs} -> {tagged, B, Len, tuple_to_list(Crumbs)}
    end.

%% a plain walk over the syntax tree's terms
has_function([H | T]) -> has_function(H) orelse has_function(T);
has_function(T) when is_tuple(T), tuple_size(T) > 0 ->
    case element(1, T) of
        function_expression -> true;
        arrow_function_expression -> true;
        function_declaration -> true;
        function_literal -> true;
        class_expression -> true;
        class_declaration -> true;
        _ -> has_function(tuple_to_list(T))
    end;
has_function(_) -> false.
