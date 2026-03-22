import { component$ } from '@builder.io/qwik';
import { routeLoader$ } from '@builder.io/qwik-city';

type SectionId = string;

type SectionLinks = Map<SectionId, string>;

// Probably false positive:
// eslint-disable-next-line qwik/loader-location
const useSectionIds = routeLoader$((): SectionLinks => {
  const mods = import.meta.glob('./*.md', { eager: true });
  const sectionIds: SectionLinks = new Map();
  
  for (const path in mods) {
    // FIXME: add a concrete type
    // eslint-disable-next-line @typescript/no-explicit-any
    const mod = mods[path] as any;
    console.log(mod.headings);
    const id = path.replace('./', '').replace('.md', '');
    sectionIds.set(id, path);
  }
  return sectionIds;
});

export default component$(() => {
  const sectionIds = useSectionIds();
  
  return (
    <div>
      <h1>Sections</h1>
      <ul>
        {[...sectionIds.value.entries()].map(([id, path]) => (
          <li key={id}>
            <a href={path}>{id}</a>
          </li>
        ))}
      </ul>
    </div>
  );
});
