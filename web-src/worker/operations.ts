// Perhaps overengineering. I'll simplify later

export const ToWorkerMessageDefinitions = {
  runGhc: [] as const,
} as const;
export type ToWorkerMessage = keyof typeof ToWorkerMessageDefinitions;
export type ToWorkerArgumentsOf<M extends ToWorkerMessage> =
  (typeof ToWorkerMessageDefinitions)[M];

export const FromWorkerMessageDefinitions = {
  loadedWasms: undefined,
  initlizedGhc: undefined,
} as const;
export type FromWorkerMessage = keyof typeof FromWorkerMessageDefinitions;
export type FromWorkerResultOf<M extends FromWorkerMessage> =
  (typeof FromWorkerMessageDefinitions)[M];
export type FromWorkerHandlers = {
  [M in FromWorkerMessage]: (args: FromWorkerResultOf<M>) => void;
};
