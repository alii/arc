%% CLI-only FFI: stdin and argv. Bound exclusively by src/arc.gleam (the
%% REPL / `arc <file>` entry point). Nothing under src/arc/vm/ may touch
%% stdin or the command line — the VM is an embeddable library.
-module(arc_cli_ffi).
-export([read_line/1]).
-export([get_script_args/0]).

%% Read one line from stdin, distinguishing end-of-input from a real I/O
%% failure. Mirrors the Gleam `ReadLine` type in src/arc.gleam:
%%   {line, Binary} | eof | {read_error, Reason}
%% The REPL treats `eof` (Ctrl-D) as a clean exit and prints `read_error`.
read_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> eof;
        {error, Reason} -> {read_error, Reason};
        %% io:get_line returns a binary only when stdio is in binary mode
        %% (an option set elsewhere, by Gleam's generated entrypoint).
        %% Normalize here so `{line, binary()}` holds unconditionally
        %% instead of depending on that cross-module precondition.
        Line when is_binary(Line) -> {line, Line};
        Line when is_list(Line) -> encode_line(Line)
    end.

%% unicode:characters_to_binary/1 is NOT total: on ill-formed input it returns
%% `{error, Encoded, Rest}` or `{incomplete, Encoded, Rest}`. Handing either of
%% those to Gleam would put a 3-tuple where the `ReadLine` type promises a
%% binary. A terminal that hands us bytes we cannot decode is an input failure,
%% so it becomes the `read_error` variant the REPL already prints.
encode_line(Line) ->
    case unicode:characters_to_binary(Line) of
        Bin when is_binary(Bin) -> {line, Bin};
        {error, _Encoded, Rest} -> {read_error, {invalid_unicode, Rest}};
        {incomplete, _Encoded, Rest} -> {read_error, {incomplete_unicode, Rest}}
    end.

%% argv arrives as lists of Unicode CODEPOINTS. `list_to_binary/1` would
%% badarg on any codepoint > 255 (e.g. `arc café.js`) and silently emit
%% latin-1 (not UTF-8) bytes for codepoints 128-255, so encode properly.
%%
%% As in encode_line/1, characters_to_binary/1 can answer with an error tuple.
%% There is no `String` value that could honestly represent an argument we
%% cannot decode, and the return type here IS `List(String)` — so crash loudly
%% at the boundary rather than smuggle a 3-tuple into Gleam.
get_script_args() ->
    [encode_arg(A) || A <- init:get_plain_arguments()].

encode_arg(A) ->
    case unicode:characters_to_binary(A) of
        Bin when is_binary(Bin) -> Bin;
        _Error -> erlang:error({bad_argv_encoding, A})
    end.
