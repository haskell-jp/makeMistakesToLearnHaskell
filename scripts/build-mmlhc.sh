#!/usr/bin/env bash

set -euxo pipefail

# Original: https://gitlab.haskell.org/ghc/ghc/-/blob/af40606aaaf65c112e67cb4e4c37aec4ea74fb36/testsuite/tests/ghc-api-browser/playground001.sh
# Copyright 2002, The University Court of the University of Glasgow. All rights reserved.

cd "$(dirname "$0")/.."

outdir=./web-src/assets/ghc
tmpdir=./web-src/assets/ghc/tmp

mkdir -p "$tmpdir"

wasm32-wasi-ghc -O2 -package ghc -shared -dynamic -no-keep-hi-files -no-keep-o-file -o "$tmpdir/libmmlhc.so" c-src/mmlhc.hs

# $tmpdir/clib contains libc/libc++ .so files
WASI_SDK="$(nix eval --raw 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#wasi-sdk')"
cp -r "$WASI_SDK"/share/wasi-sysroot/lib/wasm32-wasi "$tmpdir"/clib
chmod -R 0777 "$tmpdir"/clib
# trim unneeded stuff in c libdir
find "$tmpdir"/clib -type f ! -name "*.so" -delete
rm -f "$tmpdir"/clib/libsetjmp.so "$tmpdir"/clib/libwasi-emulated-*.so

mkdir -p "$tmpdir"/hslib
cp -r "$(wasm32-wasi-ghc --print-libdir)" "$tmpdir"/hslib/lib
chmod -R 0777 "$tmpdir"/hslib
# unregister Cabal/Cabal-syntax, too big
rm "$tmpdir"/hslib/lib/package.conf.d/package.cache.lock || true
wasm32-wasi-ghc-pkg --no-user-package-db --global-package-db="$tmpdir"/hslib/lib/package.conf.d unregister Cabal Cabal-syntax || true
wasm32-wasi-ghc-pkg --no-user-package-db --global-package-db="$tmpdir"/hslib/lib/package.conf.d recache || true

# we only need non-profiling .dyn_hi/.so, trim as much as we can
find "$tmpdir"/hslib/lib "(" \
  -name "*.hi" \
  -o -name "*.a" \
  -o -name "*.p_hi" \
  -o -name "libHS*_p.a" \
  -o -name "*.p_dyn_hi" \
  -o -name "libHS*_p*.so" \
  -o -name "libHSrts*_debug*.so" \
  ")" -delete
rm -rf \
  "$tmpdir"/hslib/lib/doc \
  "$tmpdir"/hslib/lib/html \
  "$tmpdir"/hslib/lib/latex \
  "$tmpdir"/hslib/lib/*.mjs \
  "$tmpdir"/hslib/lib/*.js \
  "$tmpdir"/hslib/lib/*.txt
# HS_SEARCHDIR is something like
# /tmp/hslib/lib/wasm32-wasi-ghc-9.15.20251024 which is the
# dynamic-library-dirs that contains all libHS*.so in one place, and
# also static libraries in per-unit directories
HS_SEARCHDIR=$(find "$tmpdir"/hslib/lib -type f -name "*.so" -print0 | xargs -0 -n1 dirname | sort -u | sed "s|^\./|/|")
# hunt down the remaining bits of Cabal/Cabal-syntax. too bad there's
# no ghc-pkg uninstall.
rm -rf ."$HS_SEARCHDIR"/*Cabal*

tar -cf "$outdir"/rootfs.tar.zst --zstd "$tmpdir"
