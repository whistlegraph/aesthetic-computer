#!/usr/bin/env node
// fetch-phones.mjs — rotary telephone bell rings + dial tones from Freesound
// (vault creds/cache, attribution recorded) into assets/phones/.
import { fetchSamples } from "../../lib/freesound.mjs";
import { mkdirSync, copyFileSync } from "node:fs";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../assets/phones");
mkdirSync(OUT, { recursive: true });

for (const q of [
  { query: "rotary telephone bell ring", filter: 'license:("Creative Commons 0" OR "Attribution") duration:[1 TO 10]', count: 4 },
  { query: "telephone dial tone", filter: 'license:("Creative Commons 0" OR "Attribution") duration:[2 TO 15]', count: 4 },
]) {
  const paths = await fetchSamples(q);
  for (const p of paths) {
    copyFileSync(p, resolve(OUT, basename(p)));
    console.log(`✓ ${q.query}: ${basename(p)}`);
  }
}
