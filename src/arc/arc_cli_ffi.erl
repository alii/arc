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
        Line -> {line, Line}
    end.

get_script_args() -> [list_to_binary(A) || A <- init:get_plain_arguments()].
