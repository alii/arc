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
        Line when is_list(Line) -> {line, unicode:characters_to_binary(Line)}
    end.

%% argv arrives as lists of Unicode CODEPOINTS. `list_to_binary/1` would
%% badarg on any codepoint > 255 (e.g. `arc café.js`) and silently emit
%% latin-1 (not UTF-8) bytes for codepoints 128-255, so encode properly.
get_script_args() ->
    [unicode:characters_to_binary(A) || A <- init:get_plain_arguments()].
