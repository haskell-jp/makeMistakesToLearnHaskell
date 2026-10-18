// TODO: Perhaps this is overkill.
// TODO: Migrate to the definition in `interface`.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const WorkerFunctionDefinitions = {
  runMmlhcMain: (_hsSrc: string): Promise<void> => {
    throw new Error("Only for type definition. Should not be called directly.");
  },
} as const;
export type WorkerFunctionDefinitions = typeof WorkerFunctionDefinitions;
export type WorkerMessage = keyof typeof WorkerFunctionDefinitions;
export type WorkerArgumentsOf<M extends WorkerMessage> = Parameters<
  (typeof WorkerFunctionDefinitions)[M]
>;
export type WorkerReturnOf<M extends WorkerMessage> = ReturnType<
  (typeof WorkerFunctionDefinitions)[M]
>;
type OtherHandlerDefinitions = {
  loadWasms: () => void;
  loadGhc: () => void;
};
export type WorkerHandlerDefinitions = {
  [func in WorkerMessage]: (result: Awaited<WorkerReturnOf<func>>) => void;
} & OtherHandlerDefinitions;
