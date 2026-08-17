#!/usr/bin/env bash
set -euo pipefail

# Build Arc for AtomVM-WebAssembly.
#
# Produces in website/public/atomvm/:
#   AtomVM.js, AtomVM.wasm  — our patched AtomVM web runtime (see
#                             scripts/build-atomvm-web.sh; committed, not built here)
#   arc.avm                 — Arc interpreter + AOT emitter (aot/) + carder +
#                             gleam_stdlib + shims + a few OTP stdlib modules
#                             (erl_pp & friends) + AtomVM stdlib
#
# Requires: gleam, erlc, escript, curl, python3, and an OTP install that ships
# its stdlib sources (lib/stdlib-*/src — standard in OTP releases).

ATOMVM_VERSION="0.7.0-alpha.0"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WEBSITE="$REPO_ROOT/website"
OUT="$WEBSITE/public/atomvm"
WORK="$WEBSITE/.atomvm-build"

mkdir -p "$OUT" "$WORK"

# AtomVM's estdlib is a few functions short of what gleam_stdlib.erl calls.
# Patch them into the downloaded sources rather than shadowing whole modules.
patch_stdlib() {
  local src="$1"
  patch_mod "$src/lists.erl" "partition/2, suffix/2" <<'EOF'
partition(Pred, L) -> partition(Pred, L, [], []).
partition(_, [], Y, N) -> {lists:reverse(Y), lists:reverse(N)};
partition(Pred, [H | T], Y, N) ->
    case Pred(H) of true -> partition(Pred, T, [H | Y], N);
                    false -> partition(Pred, T, Y, [H | N]) end.
suffix(Suf, L) ->
    D = length(L) - length(Suf),
    D >= 0 andalso lists:nthtail(D, L) =:= Suf.
EOF
  # What erl_pp / io_lib_pretty (the playground's "Erlang" tab) call.
  # NB: inside this module lists:reverse/1 is a NIF stub, so call it remotely.
  patch_mod "$src/lists.erl" "unzip/1, concat/1, sum/1, splitwith/2, takewhile/2, keysearch/3, flatlength/1" <<'EOF'
unzip(L) -> unzip(L, [], []).
unzip([{X, Y} | T], Xs, Ys) -> unzip(T, [X | Xs], [Y | Ys]);
unzip([], Xs, Ys) -> {lists:reverse(Xs), lists:reverse(Ys)}.
concat(L) -> lists:flatmap(fun thing_to_list/1, L).
thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X) -> float_to_list(X);
thing_to_list(X) when is_atom(X) -> atom_to_list(X);
thing_to_list(X) when is_list(X) -> X.
sum(L) -> sum(L, 0).
sum([H | T], S) -> sum(T, S + H);
sum([], S) -> S.
splitwith(Pred, L) -> splitwith(Pred, L, []).
splitwith(Pred, [H | T], Taken) ->
    case Pred(H) of
        true -> splitwith(Pred, T, [H | Taken]);
        false -> {lists:reverse(Taken), [H | T]}
    end;
splitwith(_, [], Taken) -> {lists:reverse(Taken), []}.
takewhile(Pred, [H | T]) ->
    case Pred(H) of
        true -> [H | takewhile(Pred, T)];
        false -> []
    end;
takewhile(_, []) -> [].
keysearch(K, N, L) ->
    case lists:keyfind(K, N, L) of
        false -> false;
        T -> {value, T}
    end.
flatlength(L) -> length(lists:flatten(L)).
EOF
  # AtomVM's maps:find/2 and maps:get/3 are `try maps:get/2 catch badkey`:
  # every dictionary miss is an exception, and AtomVM builds a raw stacktrace
  # per raise. Arc's interpreter misses constantly (property lookups). Replace
  # the bodies with is_map_key/map_get (both AtomVM BIFs).
  python3 - "$src/maps.erl" <<'PYEOF'
import re, sys
p = sys.argv[1]; s = open(p).read()
s2 = re.sub(r"find\(Key, Map\) ->\n    try\n        \{ok, \?MODULE:get\(Key, Map\)\}\n    catch\n        _:\{badkey, _\} ->\n            error\n    end\.",
            "find(Key, Map) ->\n    case erlang:is_map_key(Key, Map) of\n        true -> {ok, erlang:map_get(Key, Map)};\n        false -> error\n    end.", s)
s2 = re.sub(r"get\(Key, Map, Default\) ->\n    try\n        \?MODULE:get\(Key, Map\)\n    catch\n        error:\{badkey, _\} ->\n            Default\n    end\.",
            "get(Key, Map, Default) ->\n    case erlang:is_map_key(Key, Map) of\n        true -> erlang:map_get(Key, Map);\n        false -> Default\n    end.", s2)
if s2 == s:
    sys.exit("maps.erl: find/2 or get/3 did not match the expected try/catch shape")
open(p, "w").write(s2)
print("maps.erl: find/2 and get/3 now exception-free")
PYEOF
  patch_mod "$src/maps.erl" "with/2, without/2, update_with/4" <<'EOF'
with(Ks, M) -> lists:foldl(fun(K, A) ->
    case maps:find(K, M) of {ok, V} -> A#{K => V}; error -> A end
  end, #{}, Ks).
without(Ks, M) -> lists:foldl(fun maps:remove/2, M, Ks).
update_with(K, F, Init, M) ->
    case maps:find(K, M) of {ok, V} -> M#{K => F(V)}; error -> M#{K => Init} end.
EOF
}

patch_mod() {
  local file="$1" exports="$2"
  grep -q "$exports" "$file" && return
  local body; body="$(cat)"
  local line; line=$(grep -n '^-module(' "$file" | cut -d: -f1)
  { head -n "$line" "$file"
    printf -- '-export([%s]).\n' "$exports"
    tail -n +$((line + 1)) "$file"
    printf '\n%s\n' "$body"
  } > "$file.tmp"
  mv "$file.tmp" "$file"
}

echo "==> gleam build (arc)"
(cd "$REPO_ROOT" && gleam build --target erlang)

echo "==> gleam build (aot)"
(cd "$REPO_ROOT/aot" && gleam deps download >/dev/null && gleam build --target erlang)
AOT_BUILD="$REPO_ROOT/aot/build/dev/erlang"

echo "==> compile shims"
SHIM_OUT="$WORK/shims"
rm -rf "$SHIM_OUT" && mkdir -p "$SHIM_OUT"
erlc -o "$SHIM_OUT" "$WEBSITE"/atomvm_shims/*.erl

# AtomVM 0.7.0-alpha doesn't publish a standalone atomvmlib.avm, so we
# compile its stdlib from source. Skip hardware/network modules we don't hit.
echo "==> compile AtomVM stdlib"
STDLIB_SRC="$WORK/stdlib-src"
STDLIB_OUT="$WORK/stdlib"
STDLIB_MODS=(
  estdlib/src/erlang estdlib/src/lists estdlib/src/maps estdlib/src/binary
  estdlib/src/string estdlib/src/io estdlib/src/io_lib estdlib/src/timer
  estdlib/src/unicode estdlib/src/queue estdlib/src/math estdlib/src/proplists
  estdlib/src/gen estdlib/src/gen_server estdlib/src/proc_lib estdlib/src/sys
  eavmlib/src/atomvm eavmlib/src/timer_manager eavmlib/src/console
  avm_emscripten/src/emscripten
)
if [[ ! -d "$STDLIB_OUT" ]]; then
  mkdir -p "$STDLIB_SRC" "$STDLIB_OUT"
  RAW="https://raw.githubusercontent.com/atomvm/AtomVM/v${ATOMVM_VERSION}/libs"
  # logger.hrl is needed by a couple of modules
  curl -fsSL -o "$STDLIB_SRC/logger.hrl" "$RAW/include/logger.hrl" 2>/dev/null ||
    curl -fsSL -o "$STDLIB_SRC/logger.hrl" "$RAW/estdlib/include/logger.hrl" 2>/dev/null || true
  for m in "${STDLIB_MODS[@]}"; do
    f="$STDLIB_SRC/$(basename "$m").erl"
    [[ -f "$f" ]] || curl -fsSL -o "$f" "$RAW/$m.erl"
  done
  patch_stdlib "$STDLIB_SRC"
  if ! erlc -I "$STDLIB_SRC" -o "$STDLIB_OUT" "$STDLIB_SRC"/*.erl > "$WORK/stdlib-erlc.log" 2>&1; then
    cat "$WORK/stdlib-erlc.log" >&2
    echo "AtomVM stdlib failed to compile" >&2
    exit 1
  fi
fi

# erl_pp (Erlang source pretty-printer, behind the playground's "Erlang" tab)
# and what it pulls in. Compiled from OTP's own sources rather than taken as
# prebuilt beams: OTP 29 ships those with opcodes AtomVM 0.7 can't decode.
# io_lib_pretty uses is_record/1 (OTP 29 native records — opcode 186, unknown
# to AtomVM); no native record can exist in this bundle, so the guard is
# dropped rather than shipped.
echo "==> compile OTP stdlib modules for erl_pp"
OTP_OUT="$WORK/otp"
OTP_STDLIB_SRC="$(erl -noshell -eval 'io:format("~s/src", [code:lib_dir(stdlib)]), halt().')"
[[ -f "$OTP_STDLIB_SRC/erl_pp.erl" ]] || {
  echo "OTP stdlib sources not found at $OTP_STDLIB_SRC" >&2; exit 1; }
rm -rf "$OTP_OUT" && mkdir -p "$OTP_OUT/src"
# `array` too: arc's JS heap store is an OTP `array` (10-ary tuple tree,
# O(log n) writes). AtomVM has no array module, and a map-backed stand-in is
# catastrophic there — AtomVM maps are flat, so every heap write copied every
# cell (measured: ~1ms per JS loop iteration in the playground).
for m in erl_pp erl_anno erl_parse erl_internal array; do
  cp "$OTP_STDLIB_SRC/$m.erl" "$OTP_OUT/src/"
done
python3 "$WEBSITE/scripts/patch_io_lib_pretty.py" \
  "$OTP_STDLIB_SRC/io_lib_pretty.erl" "$OTP_OUT/src/io_lib_pretty.erl"
if ! erlc -I "$OTP_STDLIB_SRC" -I "$OTP_STDLIB_SRC/../include" -o "$OTP_OUT" "$OTP_OUT"/src/*.erl > "$WORK/otp-erlc.log" 2>&1; then
  cat "$WORK/otp-erlc.log" >&2
  echo "OTP stdlib modules failed to compile" >&2
  exit 1
fi

# The AtomVM web runtime (AtomVM.js/.wasm) is OUR build of upstream + patches,
# produced by scripts/build-atomvm-web.sh and committed; this script only
# packs arc.avm against it.
[[ -f "$OUT/AtomVM.wasm" && -f "$OUT/AtomVM.js" ]] || {
  echo "AtomVM web runtime missing in $OUT — run website/scripts/build-atomvm-web.sh" >&2
  exit 1
}

echo "==> pack arc.avm"
# First occurrence of a module wins: shims first so they shadow the real
# arc_unicode_ffi/arc_regexp_ffi beams (and AtomVM's own string module).
# The start module (arc_aot_wasm_ffi) registers both endpoints: `main` (the
# interpreter, arc_wasm_ffi's loop) and `aot` (JS → IR / Core / Erlang text).
escript "$WEBSITE/scripts/pack_avm.escript" "$OUT/arc.avm" arc_aot_wasm_ffi \
  "$SHIM_OUT"/*.beam \
  "$AOT_BUILD"/arc_aot/ebin/*.beam \
  "$REPO_ROOT"/build/dev/erlang/arc/ebin/*.beam \
  "$REPO_ROOT"/build/dev/erlang/gleam_stdlib/ebin/*.beam \
  "$AOT_BUILD"/gleam_erlang/ebin/*.beam \
  "$AOT_BUILD"/carder/ebin/*.beam \
  "$OTP_OUT"/*.beam \
  "$STDLIB_OUT"/*.beam

ls -lh "$OUT"
