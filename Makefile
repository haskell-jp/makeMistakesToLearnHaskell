ASSETSDIR = ./web-src/assets
GHCDIR = $(ASSETSDIR)/ghc
TMPDIR = $(GHCDIR)/tmp
WASI_SDK = $(shell nix eval --raw 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#wasi-sdk')

.PHONY: external
external: $(GHCDIR)/dyld.mjs $(GHCDIR)/bsdtar.wasm

# TODO:
# Build targets: $(TMPDIR)/clib, $(TMPDIR)/hslib, $(TMPDIR)/libmmlhc.so
# Configure: ZSTD_CLEVEL = 19

$(GHCDIR)/dyld.mjs: $(shell wasm32-wasi-ghc --print-libdir)/dyld.mjs
	mkdir -p $(GHCDIR)
	cp $< $@

$(GHCDIR)/bsdtar.wasm:
	wget -O $@ https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm
