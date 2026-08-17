%% AtomVM shim: erl_pp asks epp for the default source encoding.
-module(epp).
-export([default_encoding/0, source_encoding/0]).
default_encoding() -> utf8.
source_encoding() -> utf8.
