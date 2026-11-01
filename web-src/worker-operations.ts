export interface FromWorkerFunctions {
  loadWasms: () => void;
  loadGhc: () => void;
  printedStdout: (result: string) => void;
  printedStderr: (result: string) => void;
}

export interface ToWorkerFunctions {
  runMmlhc: (ghcArgs: string, hsSrc: string) => void;
}

export type ToWorkerFunctionName = keyof ToWorkerFunctions;

export type ToWorkerArgumentsOf<T extends ToWorkerFunctionName> = Parameters<
  ToWorkerFunctions[T]
>;

export type FromWorkerFunctionName = keyof FromWorkerFunctions;

export type FromWorkerArgumentsOf<T extends FromWorkerFunctionName> =
  Parameters<FromWorkerFunctions[T]>;
