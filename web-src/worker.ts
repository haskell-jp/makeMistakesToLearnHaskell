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

console.log("Worker started");

// TODO: Extract as FromWorkerFacade
onconnect = async (event): Promise<void> => {
  console.log("Worker connected");
  const port = event.ports[0];
  port.start();
  console.log("Worker: Loading WASMs");
  await loadWasms;
  console.log("Worker: Loaded WASMs");
  port.postMessage({ event: "loadedWasms" });
  console.log("Worker: WAITING Initializing GHC");
  const ghcMain = await loadGhc;
  console.log("Worker: Initialized GHC");
  port.postMessage({ event: "initializedGhc" });
};

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
});

const loadGhc = new Promise((resolve) => {
  main({
    rpc: new DyLDBrowserHost({
      rootfs,
      stdout: (msg) => {
        //document.getElementById("stdout").value += `${msg}\n`;
      },
      stderr: (msg) => {
        //document.getElementById("stderr").value += `${msg}\n`;
      },
    }),
    searchDirs: ["/tmp/clib", "/tmp/hslib/lib/wasm32-wasi-ghc-9.15.20251024"],
    mainSoPath: "/tmp/libmmlhc.so",
    args: ["libmmlhc.so", "+RTS", "-c", "-RTS"],
    isIserv: false,
  })
    .then((dyld) => {
      console.log("Worker: Finished DyldJs.main");
      return dyld.exportFuncs.myMain("/tmp/hslib/lib");
    })
    .then(resolve);
});
