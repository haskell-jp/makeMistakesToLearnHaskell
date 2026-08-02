// Original: https://gitlab.haskell.org/ghc/ghc/-/blob/af40606aaaf65c112e67cb4e4c37aec4ea74fb36/testsuite/tests/ghc-api-browser/index.html#L107-195
// Copyright 2002, The University Court of the University of Glasgow. All rights reserved.

import {
  ConsoleStdout,
  File,
  OpenFile,
  PreopenDirectory,
  WASI,
} from "./browser_wasi_shim/src/";
import { DyLDBrowserHost, main } from "./assets/ghc/dyld.mjs";
import { buildOnConnectHandler } from "./from-worker/facade";

console.log("Worker started");

const definitions = {
  waitForWasmFiles: async (): Promise<void> => {
    console.log("Worker: Received loadedWasms message");
    await loadWasms;
  },
  waitForGhcReady: async (): Promise<void> => {
    console.log("Worker: Received initializedGhc message");
    await loadGhc;
  },
};
addEventListener("connect", buildOnConnectHandler(definitions));
console.log("Worker: Connect handler registered");

const rootfs = new PreopenDirectory("/", new Map());
const bsdtar_wasi = new WASI(
  ["bsdtar.wasm", "-x"],
  [],
  [
    new OpenFile(new File(new Uint8Array(), { readonly: true })),
    ConsoleStdout.lineBuffered((msg) => console.info(msg)),
    ConsoleStdout.lineBuffered((msg) => console.warn(msg)),
    rootfs,
  ],
  { debug: false },
);

const loadWasms = Promise.all([
  WebAssembly.instantiateStreaming(fetch("./assets/ghc/bsdtar.wasm"), {
    wasi_snapshot_preview1: bsdtar_wasi.wasiImport,
  }),
  fetch("./assets/ghc/rootfs.tar.zst").then((r) => r.bytes()),
]).then(([{ instance }, rootfs_bytes]) => {
  bsdtar_wasi.fds[0] = new OpenFile(new File(rootfs_bytes, { readonly: true }));
  bsdtar_wasi.start(instance);
  console.log("Worker: Finished loading wasm files and extracting rootfs");
});

const loadGhc = new Promise((resolve) => {
  loadWasms.then(() => {
    main({
      rpc: new DyLDBrowserHost({
        rootfs,
        stdout: (msg) => {
          console.log("Worker: STDOUT:", msg);
          //document.getElementById("stdout").value += `${msg}\n`;
        },
        stderr: (msg) => {
          console.log("Worker: STDERR:", msg);
          //document.getElementById("stderr").value += `${msg}\n`;
        },
      }),
      searchDirs: [
        "/tmp/clib",
        "/tmp/hslib/lib/wasm32-wasi-ghc-9.15.20260331-4030",
      ],
      mainSoPath: "/tmp/libmmlhc.so",
      args: ["libmmlhc.so", "+RTS", "-c", "-RTS"],
      isIserv: false,
    })
      .then((dyld) => {
        console.log("Worker: Finished DyldJs.main");
        return dyld.exportFuncs.mmlhcMain("/tmp/hslib/lib");
      })
      .then(resolve)
      .catch((err) => {
        console.error("Worker: Error in DyldJs.main:", err);
      });
  });
});
