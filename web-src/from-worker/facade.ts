import type {
  FromWorkerArgumentsOf,
  FromWorkerFunctionName,
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
    ports.push(port);
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

export function broadcast<T extends FromWorkerFunctionName>(
  func: T,
  ...args: FromWorkerArgumentsOf<T>
): void {
  console.log(`Worker: Broadcasting "${func}" message to all ports`, args);
  for (const port of ports) {
    port.postMessage({ func, result: args[0] });
  }
}

// TODO: Figure out better way to collect (and delete) ports.
export const ports: MessagePort[] = [];
