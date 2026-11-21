// 🪪 rax's build: one hash over every file its server process is made of.
// Two processes with the same hash are the same rax.
import { createHash } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

import { dirname, resolve } from 'node:path';

// 🧵 What the process itself is made of: its entry and every file that
// reaches it by an import, followed from file to file. rax's pages (the
// client, the view, the pieces on its panels) are read from disk each time
// they are asked for, so a change to them alone is live at once and is no
// reason to stop the world: before 2026-11-20 the hash took in every file
// under rax/, and a deploy that only touched a page put everyone out of the
// room for a moment and set them down again.
const ENTRY = fileURLToPath(new URL('./rax-process.mjs', import.meta.url));
const IMPORT = /(?:\bimport|\bexport)\s[^'"`;]*?\bfrom\s*['"]([^'"]+)['"]|\bimport\s*\(\s*['"]([^'"]+)['"]\s*\)|\bimport\s*['"]([^'"]+)['"]/g;
export function serverFiles(entry = ENTRY) {
  const seen = new Set(), queue = [entry];
  while (queue.length) {
    const path = queue.pop();
    if (seen.has(path)) continue;
    let source;
    try { source = readFileSync(path, 'utf8'); } catch { continue; } // (a file named but not there is no part of it)
    seen.add(path);
    for (const m of source.matchAll(IMPORT)) {
      const to = m[1] || m[2] || m[3];
      if (!to.startsWith('.')) continue; // node's own, and packages: a change there comes with a restart of its own
      const next = resolve(dirname(path), to.split('?')[0]);
      if (!next.includes('/node_modules/') && /\.(mjs|js|json)$/.test(next)) queue.push(next);
    }
  }
  return [...seen].sort();
}
export function raxBuild() {
  const hash = createHash('sha1');
  for (const path of serverFiles()) {
    hash.update(path.slice(-60));
    hash.update(readFileSync(path));
  }
  return hash.digest('hex');
}
