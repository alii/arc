#!/usr/bin/env bash
set -euo pipefail

# Build the AtomVM WebAssembly runtime the playground runs on, from upstream
# source plus our patches, and install it into website/public/atomvm/.
#
# Why not the prebuilt release? Upstream AtomVM 0.7.0-alpha.0 has three
# behaviours that make an allocation-heavy Gleam program (Arc's interpreter)
# crawl — see atomvm_patches/0001-arc-playground-perf.patch:
#   * `case` on atoms (OP_SELECT_VAL) compared atoms by fetching and memcmp'ing
#     their names from the atom table; now an immediate bit-compare;
#   * any heap fragment (every compound literal use creates one) forced a full
#     copying GC at the next function return / NIF call / allocation; now
#     fragments are folded in only when large;
#   * the default heap policy kept heaps nearly full (a GC every few
#     allocations); default is now BEAM-like fibonacci growth.
# Measured on the playground: fib(15) 3.2s -> 14ms native, 1.6s -> 40ms in the
# browser; a 5000-iteration loop 2.2s -> 38ms in the browser.
#
# It is also linked with memory growth allowed (256MB initial, 2GB max): the
# copying collector's alloc/free of ever larger heaps fragments a fixed arena
# and OOM'd at 256MB on programs that need ~70MB.
#
# The outputs are committed (CI's build-avm.sh only packs arc.avm), so run
# this whenever the patch set or the AtomVM version changes.
#
# Requires: emscripten (brew install emscripten), cmake, gperf, git.

ATOMVM_VERSION="v0.7.0-alpha.0"
WEBSITE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK="$WEBSITE/.atomvm-build/AtomVM"
OUT="$WEBSITE/public/atomvm"

command -v emcmake >/dev/null || { echo "emcmake not found: brew install emscripten" >&2; exit 1; }

if [[ ! -d "$WORK/.git" ]]; then
  echo "==> clone AtomVM $ATOMVM_VERSION"
  git clone --depth 1 --branch "$ATOMVM_VERSION" https://github.com/atomvm/AtomVM.git "$WORK"
fi

echo "==> reset to $ATOMVM_VERSION and apply patches"
(cd "$WORK" && git checkout -q -- . && git clean -qfd -- src)
for p in "$WEBSITE"/atomvm_patches/*.patch; do
  echo "    $(basename "$p")"
  (cd "$WORK" && git apply "$p")
done

echo "==> emscripten web build"
BUILD="$WORK/src/platforms/emscripten/build"
rm -rf "$BUILD" && mkdir -p "$BUILD"
(cd "$BUILD" &&
  emcmake cmake .. -DAVM_EMSCRIPTEN_ENV=web -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_EXE_LINKER_FLAGS="-sALLOW_MEMORY_GROWTH=1 -sMAXIMUM_MEMORY=2147483648 -sINITIAL_MEMORY=268435456" >/dev/null &&
  emmake make -j8 AtomVM 2>&1 | grep -E "error|Built target AtomVM")

mkdir -p "$OUT"
cp "$BUILD/src/AtomVM.js" "$BUILD/src/AtomVM.wasm" "$OUT/"
ls -lh "$OUT"/AtomVM.js "$OUT"/AtomVM.wasm
echo "done — now run scripts/build-avm.sh to pack arc.avm against it"
