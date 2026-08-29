-module(arc_names_test_ffi).
-export([macro_fixed_count/0, macro_length/0, macro_proto/0, name_key/1, index_key/1,
         index_of_text/1]).

-include("arc/rt/arc_rt_names.hrl").

macro_fixed_count() -> ?N_FIXED_COUNT.
macro_length() -> ?N_length.
macro_proto() -> ?N___proto__.
name_key(N) -> ?NAME_KEY(N) + ?KEY_KIND_NAME.
index_key(I) -> ?INDEX_KEY(I).

index_of_text(B) ->
    case arc_rt_val_ffi:index_of_text(B) of
        none -> none;
        I -> {some, I}
    end.
