import { buildOnConnectHandler } from "./from-worker/facade";

const coreModulePromise = import("./worker/core.js");

const beforeOnMessage = (port: MessagePort): void => {
  coreModulePromise.then(async (coreModule) => {
    await coreModule.loadWasms;
    console.log("Worker: Wasm files loaded");
    port.postMessage({ func: "loadWasms" });
    await coreModule.loadGhc;
    console.log("Worker: GHC is ready");
    port.postMessage({ func: "loadGhc" });
  });
};

const definitions = {
  runMmlhcMain: async (_hsSrc: string): Promise<void> => {
    console.error("runMmlhcMain: Not implemented yet.");
  },
};

// TODO: Fix tsconfig to allow this cast without error.
(self as unknown as SharedWorkerGlobalScope).onconnect = buildOnConnectHandler(
  beforeOnMessage,
  definitions,
);

console.log("Worker: Connect handler registered");
