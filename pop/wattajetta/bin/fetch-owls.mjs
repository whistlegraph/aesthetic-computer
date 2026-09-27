#!/usr/bin/env node
// fetch-owls.mjs — pull a few CC0/CC-BY owl hoot candidates from Freesound
// (via pop/lib/freesound.mjs, creds + cache in the vault) into assets/owls/
// so render-wattajetta.mjs can mix them like the vocal assets. Attribution
// is recorded by the lib in the vault cache's _attributions.json.
import { fetchSamples } from "../../lib/freesound.mjs";
import { mkdirSync, copyFileSync } from "node:fs";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../assets/owls");
mkdirSync(OUT, { recursive: true });

const paths = await fetchSamples({
  query: "owl hoot",
  filter: 'license:("Creative Commons 0" OR "Attribution") duration:[1 TO 12]',
  count: 6,
});
for (const p of paths) {
  const dest = resolve(OUT, basename(p));
  copyFileSync(p, dest);
  console.log(`✓ ${basename(p)}`);
}
console.log(`${paths.length} owl candidates in ${OUT} (attribution in vault cache)`);
