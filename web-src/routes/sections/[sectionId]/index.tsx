import { glob } from 'node:fs/promises';

import { component$ } from '@builder.io/qwik';
import { useLocation } from '@builder.io/qwik-city';

export default component$(() => {
  const { sectionId } = useLocation().params;
  // FIXME: add a concrete type
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const mod = import(`../../../../assets/${sectionId}.md`) as any;
  console.log(mod.headings);
  return (
    <div>
      <h1>{sectionId}</h1>
      <p>Content from {mod}</p>
    </div>
  );
});

export const onStaticGenerate = async () => {
  const paths = await Array.fromAsync(glob('../../../../assets/*.md'));
  return {
    params: paths.map((path) => {
      const sectionId = path.match(/([^/]*)\.md$/)![1];
      return { sectionId };
    }),
  };
};
