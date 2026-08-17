%% AtomVM shim: there is no filesystem in the browser.
%%
%% arc_tz_ffi probes /etc/localtime, /etc/timezone and the zoneinfo tree
%% through prim_file to find the host time zone, and already treats "no
%% zoneinfo" as UTC. Without this module AtomVM's loader prints three lines
%% ("prim_file.beam is not a valid BEAM file" …) to stdout — straight into the
%% playground's output — before the undef is caught. Answering enoent keeps
%% the same UTC fallback and none of the noise.
-module(prim_file).
-export([read_file/1, read_file_info/1, read_link_all/1, list_dir/1]).

read_file(_Path) -> {error, enoent}.
read_file_info(_Path) -> {error, enoent}.
read_link_all(_Path) -> {error, enoent}.
list_dir(_Path) -> {error, enoent}.
