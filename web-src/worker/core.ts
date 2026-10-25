// Original: https://gitlab.haskell.org/ghc/ghc/-/blob/af40606aaaf65c112e67cb4e4c37aec4ea74fb36/testsuite/tests/ghc-api-browser/index.html#L107-195
// Copyright 2002, The University Court of the University of Glasgow. All rights reserved.

import {
  ConsoleStdout,
  File,
  OpenFile,
  PreopenDirectory,
  WASI,
} from "../browser_wasi_shim/src/";
import { DyLDBrowserHost, main } from "../assets/ghc/dyld.mjs";

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

export const loadWasms = Promise.all([
  WebAssembly.instantiateStreaming(fetch("./assets/ghc/bsdtar.wasm"), {
    wasi_snapshot_preview1: bsdtar_wasi.wasiImport,
  }),
  fetch("../assets/ghc/rootfs.tar.zst").then((r) => r.bytes()),
]).then(([{ instance }, rootfs_bytes]) => {
  bsdtar_wasi.fds[0] = new OpenFile(new File(rootfs_bytes, { readonly: true }));
  bsdtar_wasi.start(instance);
  console.log("Worker: Finished loading wasm files and extracting rootfs");
});

let runMmlhcCore: (
  ghcArgs: string,
  hsSrc: string,
) => Promise<string> | undefined;

export const runMmlhc = async (
  ghcArgs: string,
  hsSrc: string,
): Promise<void> => {
  if (!runMmlhcCore) {
    console.error(
      "runMmlhcCore is not initialized. Ensure loadMmlhc has completed.",
    );
    return;
  }
  const result = await runMmlhcCore(ghcArgs, hsSrc);
  console.log("Worker: Finished runMmlhc with result:", result);
};

export const loadMmlhc = new Promise((resolve) => {
  loadWasms
    .then(async () => {
      try {
        const dyld = await main({
          rpc: new DyLDBrowserHost({
            rootfs,
            stdout: (msg: string) => {
              console.log("Worker: STDOUT:", msg);
            },
            stderr: (msg: string) => {
              console.warn("Worker: STDERR:", msg);
            },
          }),
          searchDirs: [
            "/tmp/clib",
            "/tmp/hslib/lib/wasm32-wasi-ghc-9.15.20260331-4030",
          ],
          mainSoPath: "/tmp/libmmlhc.so",
          args: ["libmmlhc.so", "+RTS", "-c", "-RTS"],
          isIserv: false,
        });
        console.log("Worker: Finished DyldJs.main");
        runMmlhcCore = await dyld.exportFuncs.mmlhcMain("/tmp/hslib/lib");
        console.log("Worker: Finished mmlhcMain");
        resolve(undefined);
      } catch (err) {
        console.error("Worker: Error in DyldJs.main:", err);
      }
    })
    .catch((err) => {
      console.error("Worker: Error loading wasm files:", err);
    });
});
