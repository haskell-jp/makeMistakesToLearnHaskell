import {
  component$,
  Slot,
  useStyles$,
  useVisibleTask$,
} from "@builder.io/qwik";
import { routeLoader$ } from "@builder.io/qwik-city";

import Header from "../components/starter/header/header";
import Footer from "../components/starter/footer/footer";

import styles from "./styles.css?inline";
import { ToWorkerFacade } from "../to-worker/facade";

export const useServerTimeLoader = routeLoader$(() => {
  return {
    date: new Date().toISOString(),
  };
});

export default component$(() => {
  useStyles$(styles);
  // This is essential for loading and connecting with the GHC worker.
  // eslint-disable-next-line qwik/no-use-visible-task
  useVisibleTask$(
    () => {
      console.log("Main thread: Connecting to the GHC worker...");
      const worker = new ToWorkerFacade(
        new SharedWorker(new URL("/worker.js", import.meta.url), {
          type: "module",
        }),
      );
      console.log("Main thread: Connected to the GHC worker:", worker);
      worker.forEachEvent({
        loadWasms: () => {
          console.log("Main thread: Worker has loaded WASMs");
        },
        loadGhc: () => {
          console.log("Main thread: Worker has loaded GHC");
          worker.runMmlhcMain(
            "",
            'main :: IO ()\nmain = putStrLn "Hello from Mmlhc!"',
          );
        },
        printedStdout: (result: string) => {
          console.log("Main thread: Worker printed to stdout:", result);
        },
        printedStderr: (result: string) => {
          console.error("Main thread: Worker printed to stderr:", result);
        },
        runMmlhc: () => {
          console.log("Main thread: Worker has run Mmlhc");
        },
      });
    },
    { strategy: "document-ready" },
  );
  return (
    <>
      <Header />
      <main>
        <Slot />
      </main>
      <Footer />
    </>
  );
});
