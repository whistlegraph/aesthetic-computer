#!/usr/bin/env node
// fetch-punches.mjs — fight-foley punch impacts (whoosh+hit) from Freesound
// into assets/punches/. Stand-ins for the SF6 stash; same folder contract.
import { fetchSamples } from "../../lib/freesound.mjs";
import { mkdirSync, copyFileSync } from "node:fs";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";
const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../assets/punches");
mkdirSync(OUT, { recursive: true });
const paths = await fetchSamples({
  query: "punch impact whoosh fight",
  filter: 'license:("Creative Commons 0" OR "Attribution") duration:[0.2 TO 2]',
  count: 6,
});
for (const p of paths) { copyFileSync(p, resolve(OUT, basename(p))); console.log(`✓ ${basename(p)}`); }
