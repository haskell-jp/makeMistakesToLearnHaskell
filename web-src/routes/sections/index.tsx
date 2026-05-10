import { component$ } from "@builder.io/qwik";
import { routeLoader$ } from "@builder.io/qwik-city";

type SectionId = string;

type SectionLink = {
  id: SectionId;
  path: string;
};

type SectionLinks = SectionLink[];

// Probably false positive:
// eslint-disable-next-line qwik/loader-location
const useSectionLinks = routeLoader$((): SectionLinks => {
  const mods = import.meta.glob("./*/index.md", { eager: true });
  const sectionLinks: SectionLinks = [];
  for (const [modPath, mod_] of Object.entries(mods)) {
    // FIXME: add a concrete type
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const mod = mod_ as any;
    console.log(mod.headings);
    const id = modPath.match(/([^/]*)\/index\.md$/)![1];
    sectionLinks.push({
      id,
      path: modPath.replace(/\/index.md$/, ""),
    });
  }
  return sectionLinks;
});

export default component$(() => {
  const sectionLinks = useSectionLinks();

  return (
    <div>
      <h1>Sections</h1>
      <ul>
        {sectionLinks.value.map(({ id, path }) => (
          <li key={id}>
            <a href={path}>{id}</a>
          </li>
        ))}
      </ul>
    </div>
  );
});
