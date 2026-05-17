GHC_SRC_DIR = $(shell wasm32-wasi-ghc --print-libdir)
GHC_SRC_FILES = $(wildcard $(GHC_SRC_DIR)/*.mjs)

ASSETS_DIR = ./web-src/assets
GHC_DEST_DIR = $(ASSETS_DIR)/ghc
GHC_DEST_FILES = $(patsubst $(GHC_SRC_DIR)/%.mjs, $(GHC_DEST_DIR)/%.mjs, $(GHC_SRC_FILES))
TMP_DIR = $(GHC_DEST_DIR)/tmp
WASI_SDK = $(shell nix eval --raw 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#wasi-sdk')

.PHONY: external

external: $(GHC_DEST_FILES) $(GHC_DEST_DIR)/bsdtar.wasm

# TODO:
# Build targets: $(TMP_DIR)/clib, $(TMP_DIR)/hslib, $(TMP_DIR)/libmmlhc.so
# Configure: ZSTD_CLEVEL = 19

$(GHC_DEST_DIR)/%.mjs: $(GHC_SRC_DIR)/%.mjs
	mkdir -p $(GHC_DEST_DIR)
	install -m 644 $< $@

$(GHC_DEST_DIR)/bsdtar.wasm:
	wget -O $@ https://haskell-wasm.github.io/bsdtar-wasm/bsdtar.wasm
