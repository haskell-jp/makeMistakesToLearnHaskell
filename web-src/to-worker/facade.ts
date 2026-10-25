import {
  WorkerHandlerDefinitions,
  WorkerMessage,
  type WorkerArgumentsOf,
} from "../worker-operations";

export class ToWorkerFacade {
  #worker: SharedWorker;

  constructor(worker: SharedWorker) {
    this.#worker = worker;
    this.#worker.port.start();
  }

  // TODO: correct type
  runMmlhcMain(...args: WorkerArgumentsOf<"runMmlhc">): void {
    console.log('Main thread: Sending "runMmlhc" message to worker');
    this.#worker.port.postMessage({ func: "runMmlhc", args });
  }

  forEachEvent(handlers: WorkerHandlerDefinitions): void {
    this.#worker.port.addEventListener("message", (event: MessageEvent) => {
      const { func, result } = event.data;
      if (func in handlers) {
        console.log("Main thread: Handling message from worker", {
          func,
          result,
        });
        handlers[func as WorkerMessage](result);
        return;
      }
      console.warn(`Main thread: Unknown operation from worker: "${func}"`);
    });
  }
}
