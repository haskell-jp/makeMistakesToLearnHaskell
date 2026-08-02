// Perhaps overengineering. I'll simplify later

const WorkerMessageDefinitions = {
  //runGhc: (): void => {},
  waitForWasmFiles: (): Promise<void> => {
    throw new Error("only for typing");
  },
  waitForGhcReady: (): Promise<void> => {
    throw new Error("only for typing");
  },
} as const;
export type WorkerMessageDefinitions = typeof WorkerMessageDefinitions;
export type WorkerMessage = keyof typeof WorkerMessageDefinitions;
export type WorkerArgumentsOf<M extends WorkerMessage> = Parameters<
  (typeof WorkerMessageDefinitions)[M]
>;
export type WorkerReturnOf<M extends WorkerMessage> = ReturnType<
  (typeof WorkerMessageDefinitions)[M]
>;
