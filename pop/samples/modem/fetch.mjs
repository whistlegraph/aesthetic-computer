// fetch.mjs — pull dial-up modem samples from Freesound into pop/samples/modem/.
//
// Re-runnable: already-copied samples are skipped (dedupe by Freesound id
// prefix in the filename, <id>-<slug>.mp3). Attribution is recorded by the
// freesound helper in the vault cache _attributions.json.
//
//   node pop/samples/modem/fetch.mjs

import { fetchSamples } from "../../lib/freesound.mjs";
import { copyFileSync, existsSync, readdirSync } from "node:fs";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));

const LICENSE = 'license:("Creative Commons 0" OR "Attribution")';

// Queries chosen after probing: "modem carrier tone", "300 baud modem",
// "modem data noise clicks", and "fax handshake" return zero results under
// this license filter — the ones below actually hit.
const SEARCHES = [
  { query: "56k modem handshake", filter: `${LICENSE} duration:[2 TO 30]`, count: 6 },
  { query: "dial up modem connect", filter: `${LICENSE} duration:[2 TO 30]`, count: 6 },
  { query: "56k modem", filter: `${LICENSE} duration:[2 TO 30]`, count: 6 },
  { query: "modem dialing", filter: `${LICENSE} duration:[2 TO 30]`, count: 5 },
  { query: "dialup", filter: `${LICENSE} duration:[2 TO 30]`, count: 6 },
  { query: "baud", filter: `${LICENSE} duration:[2 TO 30]`, count: 6 }, // RTTY/BPSK carriers
  { query: "modem noise", filter: `${LICENSE} duration:[2 TO 30]`, count: 5 },
  { query: "modem disconnect", filter: `${LICENSE} duration:[2 TO 30]`, count: 2 },
];

// Curation: results that matched but were pruned from the collection —
// near-duplicate short dial clicks (8034/8038 vs kept 8037/8055),
// redundant RTTY/BPSK carrier variants (kept 109143/109145/109147),
// redundant digital-radio noise takes (kept 397079), and an off-theme
// 92 bpm glitch loop (818453).
const EXCLUDE = new Set([
  "8034", "8038",
  "109144", "109146", "109148",
  "397080", "397082", "397084", "397085",
  "818453",
]);

function idOf(file) {
  const m = basename(file).match(/^(\d+)-/);
  return m ? m[1] : null;
}

// Ids already present in this directory (from previous runs).
const have = new Set(
  readdirSync(HERE).filter((f) => f.endsWith(".mp3")).map(idOf).filter(Boolean),
);

let copied = 0;
const failures = [];

for (const search of SEARCHES) {
  console.log(`search: "${search.query}"`);
  let paths = [];
  try {
    paths = await fetchSamples(search);
  } catch (e) {
    console.error(`  FAILED: ${e.message}`);
    failures.push({ query: search.query, error: e.message });
    continue;
  }
  for (const p of paths) {
    const id = idOf(p);
    if (!id || have.has(id) || EXCLUDE.has(id)) {
      console.log(`  skip (${EXCLUDE.has(id) ? "excluded" : "dupe"}): ${basename(p)}`);
      continue;
    }
    const dest = resolve(HERE, basename(p));
    if (!existsSync(dest)) copyFileSync(p, dest);
    have.add(id);
    copied += 1;
    console.log(`  + ${basename(p)}`);
  }
}

console.log(`\n${copied} new samples copied, ${have.size} total in ${HERE}`);
if (failures.length) {
  console.log("failures:");
  for (const f of failures) console.log(`  ${f.query}: ${f.error}`);
}
