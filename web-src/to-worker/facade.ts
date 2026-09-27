import { WorkerMessage, type WorkerArgumentsOf } from "../worker-operations";

export class ToWorkerFacade {
  #worker: Worker;

  constructor(worker: Worker) {
    this.#worker = worker;
  }

  call<M extends WorkerMessage>(
    op: WorkerMessage,
    ...args: WorkerArgumentsOf<M>
  ): Promise<void> {
    return new Promise((resolve) => {
      this.#worker.addEventListener(
        "message",
        (event) => {
          console.log("Main thread: Received message from worker", event.data);
          resolve();
        },
        { once: true },
      );
      this.#worker.postMessage({ op, args });
    });
  }
}
