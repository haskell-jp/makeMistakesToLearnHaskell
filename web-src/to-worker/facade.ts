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
    const channel = new MessageChannel();
    return new Promise((resolve) => {
      channel.port1.addEventListener(
        "message",
        (event): void => {
          console.log("Main thread: Received message from worker", event.data);
          resolve(event.data);
        },
        { once: true },
      );
      console.log("Main thread: Sending message to worker", {
        op,
        args,
        port: channel.port2,
      });
      this.#worker.port.postMessage({ op, args }, [channel.port2]);
    });
  }
}
