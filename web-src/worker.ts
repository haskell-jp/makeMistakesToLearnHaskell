import { buildOnConnectHandler } from "./from-worker/facade";

let coreModule: typeof import("./worker/core.js") | null = null;

const definitions = {
  waitForWasmFiles: async (): Promise<void> => {
    coreModule ??= await import("./worker/core.js");
    console.log("Worker: Received loadedWasms message");
    await coreModule.loadWasms;
  },
  waitForGhcReady: async (): Promise<void> => {
    coreModule ??= await import("./worker/core.js");
    console.log("Worker: Received initializedGhc message");
    await coreModule.loadGhc;
  },
};

addEventListener("connect", buildOnConnectHandler(definitions));
