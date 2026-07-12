// whistlegraph merge-site-data — additively fold new songs into graphs.json
//
//   node merge-site-data.mjs           # write merged graphs.json
//   node merge-site-data.mjs --dry     # report only, don't write
//
// The live graphs.json carries hand-curation the generator can't reproduce
// (Alex's per-song attributions, renamed-code `asset` keys, tuned view
// counts, the curated ten). Regenerating from CODES.json wholesale would
// regress that and churn codes (breaking deep links). So instead we MERGE:
//
//   • every existing live entry is preserved verbatim — codes, titles,
//     authors, assets, order of the curated ten;
//   • we append only GENUINELY-NEW graph clusters: kind==="graph", a code
//     not already live, and zero clip-overlap with any prior cluster (so a
//     re-coded existing song — e.g. appl→boo — is skipped, not duplicated);
//   • the non-curated archive is re-sorted most-sung-first so new high-reach
//     songs interleave under the "more from the archive" divider.
//
// Talks / other clusters stay in CODES.json (the database) but are not
// published here — they're one flag from visible when the arrangement calls.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const DOWNLOADS = join(HERE, "downloads");
const SITE = join(HERE, "..", "..", "system", "public", "whistlegraph.org", "graphs.json");
const DRY = process.argv.includes("--dry");

const live = JSON.parse(readFileSync(SITE, "utf8"));
const codes = JSON.parse(readFileSync(join(DOWNLOADS, "CODES.json"), "utf8")).songs;

// Prior clip ids come from the pre-ingest CODES snapshot (backed up to /tmp).
const prior = JSON.parse(readFileSync("/tmp/CODES-before.json", "utf8")).songs;
const priorClipIds = new Set(prior.flatMap((p) => p.clips));
const liveCodes = new Set(live.graphs.map((e) => e.code));

const year = (span) => Number(String(span?.[0] ?? "").slice(0, 4)) || null;

const fresh = codes
  .filter((e) => e.kind === "graph" && !liveCodes.has(e.code) && e.clips.every((c) => !priorClipIds.has(c)))
  .map((e) => ({
    code: e.code,
    title: e.title,
    by: "Whistlegraph", // default collective credit; Alex re-attributes broad strokes
    year: year(e.span),
    views: e.views,
    perf: e.performances,
    c: "#b44887", // archive rose
  }));

const curated = live.graphs.slice(0, live.curated);
const archive = [...live.graphs.slice(live.curated), ...fresh].sort((a, b) => b.views - a.views);
const graphs = [...curated, ...archive];

const out = {
  generated: new Date().toISOString().slice(0, 10),
  count: graphs.length,
  curated: live.curated,
  graphs,
};

console.log(`live: ${live.graphs.length} graphs (${live.curated} curated)`);
console.log(`fresh added: ${fresh.length} (perf>1: ${fresh.filter((f) => f.perf > 1).length})`);
console.log(`merged total: ${graphs.length}`);
if (DRY) { console.log("(dry — not written)"); process.exit(0); }
writeFileSync(SITE, JSON.stringify(out));
console.log(`wrote ${SITE}`);
