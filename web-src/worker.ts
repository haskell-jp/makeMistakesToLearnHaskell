import { buildOnConnectHandler } from "./from-worker/facade";
import { ToWorkerFunctions } from "./worker-operations";

const coreModulePromise = import("./worker/core.js");

const beforeOnMessage = (port: MessagePort): void => {
  coreModulePromise
    .then(async (coreModule) => {
      coreModule.ports.push(port);

      await coreModule.loadWasms;
      console.log("Worker: Wasm files loaded");
      port.postMessage({ func: "loadWasms" });
      await coreModule.loadMmlhc;
      console.log("Worker: GHC is ready");
      port.postMessage({ func: "loadGhc" });
    })
    .catch((err) => {
      console.error("Worker: Error loading core module:", err);
    });
};

const definitions: ToWorkerFunctions = {
  runMmlhc: async (ghcArgs: string, hsSrc: string): Promise<void> => {
    const coreModule = await coreModulePromise;
    console.log(
      "Worker: BEFORE Running runMmlhc with args:",
      ghcArgs,
      "and source:",
      hsSrc,
    );
    await coreModule.runMmlhc(ghcArgs, hsSrc);
    console.log("Worker: AFTER Running runMmlhc");
  },
};

// TODO: Fix tsconfig to allow this cast without error.
(self as unknown as SharedWorkerGlobalScope).onconnect = buildOnConnectHandler(
  beforeOnMessage,
  definitions,
);

console.log("Worker: Connect handler registered");
