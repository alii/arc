%% AtomVM switches to OTP-style boot when it finds an init module, calling
%% boot/1 with ["-s", StartModule] instead of StartModule:start/0 directly.
%% We need this module anyway to satisfy arc_vm_ffi's init:get_plain_arguments
%% import, so boot/1 just forwards.
-module(init).
-export([boot/1, get_plain_arguments/0, stop/0, stop/1]).

boot([<<"-s">>, Mod]) when is_atom(Mod) -> Mod:start();
boot([<<"-s">>, Mod]) -> (binary_to_atom(Mod, utf8)):start();
boot(_) -> arc_wasm_ffi:start().

get_plain_arguments() -> [].
stop() -> ok.
stop(_Status) -> ok.
