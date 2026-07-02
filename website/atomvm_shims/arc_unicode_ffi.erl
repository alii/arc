%% AtomVM shim — no `re` or `persistent_term` here. The lexer's ASCII
%% fast path handles a-z/A-Z/0-9/_/$ before this is called, so returning
%% false just means non-ASCII identifiers (let café = 1) are rejected
%% in the browser playground. Acceptable for a demo.
-module(unicode_ffi).
-export([is_id_start/1, is_id_continue/1]).

is_id_start(_CP) -> false.
is_id_continue(_CP) -> false.
