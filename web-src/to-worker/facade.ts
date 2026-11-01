import {
  FromWorkerFunctions,
  FromWorkerFunctionName,
  type FromWorkerArgumentsOf,
  ToWorkerFunctionName,
  ToWorkerArgumentsOf,
} from "../worker-operations";

export class ToWorkerFacade {
  #worker: SharedWorker;

  constructor(worker: SharedWorker) {
    this.#worker = worker;
    this.#worker.port.start();
  }

  call<T extends ToWorkerFunctionName>(
    func: T,
    ...args: ToWorkerArgumentsOf<T>
  ): void {
    console.log(`Main thread: Sending "${func}" message to worker`, args);
    this.#worker.port.postMessage({ func, args });
  }

  forEachEvent(handlers: FromWorkerFunctions): void {
    this.#worker.port.addEventListener("message", (event: MessageEvent) => {
      console.log("Main thread: Received message from worker", event);
      const { func, result } = event.data;
      if (func in handlers) {
        console.log("Main thread: Handling message from worker", {
          func,
          result,
        });
        handlers[func as FromWorkerFunctionName](result);
        return;
      }
      console.warn(`Main thread: Unknown operation from worker: "${func}"`);
    });
  }
}
