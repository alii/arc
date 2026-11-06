%% AtomVM shim, because AtomVM has no `re` or `persistent_term`. The lexer's
%% ASCII fast path handles a-z/A-Z/0-9/_/$ before this is called, so returning
%% false only means the browser playground rejects non-ASCII identifiers
%% (let café = 1). That's fine for a demo.
-module(arc_unicode_ffi).
-export([is_id_start/1, is_id_continue/1]).

is_id_start(_CP) -> false.
is_id_continue(_CP) -> false.
