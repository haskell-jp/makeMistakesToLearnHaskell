#!/usr/bin/env bash

set -euxo pipefail

# Original: https://gitlab.haskell.org/ghc/ghc/-/blob/af40606aaaf65c112e67cb4e4c37aec4ea74fb36/testsuite/tests/ghc-api-browser/playground001.sh
# Copyright 2002, The University Court of the University of Glasgow. All rights reserved.

project_root="$(realpath "$(dirname "$0")"/..)"

cd "$project_root/web-src/assets/ghc"

mkdir -p tmp

wasm32-wasi-ghc -O2 -package ghc -shared -dynamic -no-keep-hi-files -no-keep-o-file -o tmp/libmmlhc.so "$project_root/c-src/mmlhc.hs"

# tmp/clib contains libc/libc++ .so files
WASI_SDK="$(nix build --no-link --print-out-paths 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#wasi-sdk')"
cp -r "$WASI_SDK"/share/wasi-sysroot/lib/wasm32-wasi tmp/clib
chmod -R 0777 tmp/clib
# trim unneeded stuff in c libdir
find tmp/clib -type f ! -name "*.so" -delete
rm -f tmp/clib/libsetjmp.so tmp/clib/libwasi-emulated-*.so

mkdir -p tmp/hslib
cp -r "$(wasm32-wasi-ghc --print-libdir)" tmp/hslib/lib
chmod -R 0777 tmp/hslib
# unregister Cabal/Cabal-syntax, too big
rm tmp/hslib/lib/package.conf.d/package.cache.lock || true
wasm32-wasi-ghc-pkg --no-user-package-db --global-package-db=tmp/hslib/lib/package.conf.d unregister Cabal Cabal-syntax || true
wasm32-wasi-ghc-pkg --no-user-package-db --global-package-db=tmp/hslib/lib/package.conf.d recache || true

# we only need non-profiling .dyn_hi/.so, trim as much as we can
find tmp/hslib/lib "(" \
  -name "*.hi" \
  -o -name "*.a" \
  -o -name "*.p_hi" \
  -o -name "libHS*_p.a" \
  -o -name "*.p_dyn_hi" \
  -o -name "libHS*_p*.so" \
  -o -name "libHSrts*_debug*.so" \
  ")" -delete
rm -rf \
  tmp/hslib/lib/doc \
  tmp/hslib/lib/html \
  tmp/hslib/lib/latex \
  tmp/hslib/lib/*.mjs \
  tmp/hslib/lib/*.js \
  tmp/hslib/lib/*.txt
# HS_SEARCHDIR is something like
# /tmp/hslib/lib/wasm32-wasi-ghc-9.15.20251024 which is the
# dynamic-library-dirs that contains all libHS*.so in one place, and
# also static libraries in per-unit directories
HS_SEARCHDIR=$(find tmp/hslib/lib -type f -name "*.so" -print0 | xargs -0 -n1 dirname | sort -u | sed "s|^\./|/|")
# hunt down the remaining bits of Cabal/Cabal-syntax. too bad there's
# no ghc-pkg uninstall.
rm -rf ."$HS_SEARCHDIR"/*Cabal*

tar -cf rootfs.tar.zst --zstd tmp
