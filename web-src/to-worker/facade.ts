import { WorkerMessage, type WorkerArgumentsOf } from "../worker-operations";

export class ToWorkerFacade {
  #worker: SharedWorker;

  constructor(worker: SharedWorker) {
    this.#worker = worker;
    this.#worker.port.start();
  }

  call<M extends WorkerMessage>(
    op: WorkerMessage,
    ...args: WorkerArgumentsOf<M>
  ): Promise<void> {
    return new Promise((resolve) => {
      this.#worker.port.addEventListener("message", (event): void => {
        console.log("Main thread: Received message from worker", event.data);
        resolve(event.data);
      });
      console.log("Main thread: Sending message to worker", { op, args });
      this.#worker.port.postMessage({ op, args });
    });
  }
}
