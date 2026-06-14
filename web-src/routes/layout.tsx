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
import { ToWorkerFacade } from "../worker/facade";

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
      const worker = new ToWorkerFacade(
        new SharedWorker(new URL("/worker.js", import.meta.url), {
          type: "module",
        }),
      );
      worker.on({
        loadedWasms: () => {
          console.log("Page: WASM files loaded");
        },
        initializedGhc: () => {
          console.log("Page: GHC initialized");
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
