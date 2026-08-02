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
      console.log("Connecting to the GHC worker...");
      const worker = new ToWorkerFacade(
        new SharedWorker(new URL("/worker.js", import.meta.url), {
          type: "module",
        }),
      );
      worker.call("waitForWasmFiles").then(() => {
        console.log("Wasm files are ready");
        worker.call("waitForGhcReady").then(() => {
          console.log("GHC is ready");
        });
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
