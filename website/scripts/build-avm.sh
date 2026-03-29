#!/usr/bin/env bash
set -euo pipefail

# Build Arc for AtomVM-WebAssembly.
#
# Produces in website/public/atomvm/:
#   AtomVM.js, AtomVM.wasm  — prebuilt AtomVM web runtime
#   arc.avm                 — Arc + gleam_stdlib + shims + AtomVM stdlib
#
# Requires: gleam, erlc, escript, curl.

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

echo "==> gleam build"
(cd "$REPO_ROOT" && gleam build --target erlang)

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
  erlc -I "$STDLIB_SRC" -o "$STDLIB_OUT" "$STDLIB_SRC"/*.erl 2>&1 |
    grep -v "Warning:" || true
fi

echo "==> fetch AtomVM web runtime"
fetch() {
  [[ -f "$2" ]] || curl -fsSL -o "$2" \
    "https://github.com/atomvm/AtomVM/releases/download/v${ATOMVM_VERSION}/$1"
}
fetch "AtomVM-web-v${ATOMVM_VERSION}.js" "$OUT/AtomVM.js"
fetch "AtomVM-web-v${ATOMVM_VERSION}.wasm" "$OUT/AtomVM.wasm"

echo "==> pack arc.avm"
# Shims first so they shadow the real unicode_ffi/arc_regexp_ffi beams.
escript "$WEBSITE/scripts/pack_avm.escript" "$OUT/arc.avm" arc_wasm_ffi \
  "$SHIM_OUT"/*.beam \
  "$REPO_ROOT"/build/dev/erlang/arc/ebin/*.beam \
  "$REPO_ROOT"/build/dev/erlang/gleam_stdlib/ebin/*.beam \
  "$STDLIB_OUT"/*.beam

ls -lh "$OUT"
