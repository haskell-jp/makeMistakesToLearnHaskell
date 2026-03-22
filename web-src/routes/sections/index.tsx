import { component$ } from "@builder.io/qwik";
import { routeLoader$ } from "@builder.io/qwik-city";

type SectionId = string;

type SectionLink = { id: SectionId; path: string };

// Probably false positive:
// eslint-disable-next-line qwik/loader-location
const useSectionLinks = routeLoader$((): SectionLink[] => {
  const mods = import.meta.glob("../../../assets/*.md", { eager: true });
  const sectionIds: SectionLink[] = [];

  for (const path in mods) {
    // FIXME: add a concrete type
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const mod = mods[path] as any;
    const id = path.match(/([^/]*)\.md$/)![1];
    sectionIds.push({ id, path: `/sections/${id}` });
  }
  return sectionIds;
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
