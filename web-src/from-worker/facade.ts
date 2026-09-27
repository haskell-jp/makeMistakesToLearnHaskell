import type {
  WorkerArgumentsOf,
  WorkerMessage,
  WorkerMessageDefinitions,
  WorkerReturnOf,
} from "../worker-operations";

export function buildOnMessageHandler(
  definitions: WorkerMessageDefinitions,
): (event: MessageEvent) => void {
  console.log("Worker: Building onmessage handler with definitions");
  return (e: MessageEvent) => {
    console.log("Worker: Received message", e.data);
    const { op, args } = e.data;
    if (op in definitions) {
      const handler = definitions[op as WorkerMessage] as (
        ...args: WorkerArgumentsOf<WorkerMessage>
      ) => WorkerReturnOf<WorkerMessage>;
      handler(...(args as WorkerArgumentsOf<WorkerMessage>)).then((result) => {
        console.log("Worker: Sending result back to main thread", result);
        postMessage(result);
      });
    } else {
      console.warn(`Unknown operation: ${op}`);
    }
  };
}
