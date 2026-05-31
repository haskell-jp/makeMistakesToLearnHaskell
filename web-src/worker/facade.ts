import {
  type FromWorkerMessage,
  type FromWorkerResultOf,
  type FromWorkerHandlers,
  ToWorkerMessage,
  type ToWorkerArgumentsOf,
} from "./operations";

export class ToWorkerFacade {
  #worker: SharedWorker;

  constructor(worker: SharedWorker) {
    this.#worker = worker;
  }

  #ipc<M extends ToWorkerMessage>(
    op: ToWorkerMessage,
    ...args: ToWorkerArgumentsOf<M>
  ): Promise<void> {
    return new Promise((resolve) => {
      const returnPort = new MessagePort();
      this.#worker.port.addEventListener("message", (event): void => {
        resolve(event.data);
      });
      this.#worker.port.start();
      this.#worker.port.postMessage({ op, args, returnPort });
    });
  }

  on(handlers: FromWorkerHandlers) {
    console.log("Setting up message handlers for worker");
    this.#worker.port.addEventListener("message", (e): void => {
      const { event, result } = e.data;
      const handler = handlers[event as unknown as FromWorkerMessage];
      handler(result as FromWorkerResultOf<FromWorkerMessage>);
    });
    console.log("Message handlers set up");
    this.#worker.port.start();
    this.#worker.port.postMessage("");
  }
}
