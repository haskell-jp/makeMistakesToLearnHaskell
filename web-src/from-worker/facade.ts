import type {
  ToWorkerArgumentsOf,
  ToWorkerFunctionName,
  ToWorkerFunctions,
} from "../worker-operations";

export function buildOnConnectHandler(
  beforeOnMessage: (port: MessagePort) => void,
  definitions: ToWorkerFunctions,
): (event: MessageEvent) => void {
  console.log("Worker: Building onConnect handler with definitions");
  return (event: MessageEvent) => {
    const port = event.ports[0];
    console.log("Worker: Connected to main thread", port);
    beforeOnMessage(port);
    port.addEventListener(
      "message",
      (e: MessageEvent) => {
        console.log("Worker: Received message", e.data);
        const { func, args } = e.data;
        if (func in definitions) {
          console.log("Worker: returnPort", port);
          const handler = definitions[func as ToWorkerFunctionName] as (
            ...args: ToWorkerArgumentsOf<ToWorkerFunctionName>
          ) => void;
          handler(...(args as ToWorkerArgumentsOf<ToWorkerFunctionName>));
        } else {
          console.warn(`Unknown operation: ${func}`);
        }
      },
      { once: true },
    );
    console.log("Worker: Message handler registered");
    port.start();
  };
}
