import type {
  WorkerArgumentsOf,
  WorkerMessage,
  WorkerMessageDefinitions,
  WorkerReturnOf,
} from "../worker-operations";

export function buildOnConnectHandler(
  definitions: WorkerMessageDefinitions,
): (event: Event) => void {
  console.log("Worker: Building onConnect handler with definitions");
  return (event: Event) => {
    const port = event.ports[0];
    console.log("Worker: Connected to main thread");
    port.addEventListener(
      "message",
      (e: MessageEvent) => {
        console.log("Worker: Received message", e.data);
        const { op, args } = e.data;
        if (op in definitions) {
          // TODO: Create returnPort correctly using MessageChannel, and use it to send the result back to the main thread.
          const returnPort = e.ports[0];
          console.log("Worker: returnPort", returnPort);
          const handler = definitions[op as WorkerMessage] as (
            ...args: WorkerArgumentsOf<WorkerMessage>
          ) => WorkerReturnOf<WorkerMessage>;
          handler(...(args as WorkerArgumentsOf<WorkerMessage>)).then(
            (result) => {
              console.log("Worker: Sending result back to main thread", result);
              port.postMessage(result);
            },
          );
        } else {
          console.warn(`Unknown operation: ${op}`);
        }
      },
      { once: true },
    );
    console.log("Worker: Message handler registered");
    port.start();
  };
}
