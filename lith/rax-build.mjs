// 🪪 rax's build: one hash over every file its server and its pages are made
// of. Two processes with the same hash are the same rax.
import { createHash } from 'node:crypto';
import { readdirSync, readFileSync, statSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';

const ROOTS = ['../rax', '../xbox/live'].map((p) => fileURLToPath(new URL(p, import.meta.url)));
const OWN = ['rax-process.mjs', 'rax-build.mjs', 'socket-routing.mjs'].map((p) => fileURLToPath(new URL(p, import.meta.url)));
const SKIP = new Set(['node_modules', 'test', 'film', '.git']);

function files(dir, out = []) {
  for (const name of readdirSync(dir).sort()) {
    if (SKIP.has(name) || name.startsWith('.')) continue;
    const path = join(dir, name), is = statSync(path);
    if (is.isDirectory()) files(path, out);
    else if (/\.(mjs|js|html|json)$/.test(name)) out.push(path);
  }
  return out;
}
export function raxBuild() {
  const hash = createHash('sha1');
  for (const path of [...ROOTS.flatMap((root) => files(root)), ...OWN]) {
    hash.update(path.slice(-60));
    hash.update(readFileSync(path));
  }
  return hash.digest('hex');
}
