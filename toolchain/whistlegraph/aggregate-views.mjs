// whistlegraph aggregate views — total reach across platforms
//
//   node aggregate-views.mjs
//
// Sums the view counts we've gathered, per platform/source, into one
// defensible number. Only counts what's verified: our own TikTok account
// (CATALOG.json), our own YouTube channel + human-confirmed YouTube
// reposts (YOUTUBE.json). Unverified YouTube search hits
// (YOUTUBE-CANDIDATES.json) are reported separately as "unreviewed" and
// NOT added — our keywords collide with the Smile.dk "Butterfly" song and
// generic butterfly memes, so search alone over-counts wildly.

import { existsSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const D = join(HERE, "downloads");
const read = (f) => (existsSync(join(D, f)) ? JSON.parse(readFileSync(join(D, f), "utf8")) : null);
const sum = (a) => a.reduce((s, v) => s + (v || 0), 0);
const fmt = (n) => (n >= 1e9 ? `${(n / 1e9).toFixed(2)}B` : n >= 1e6 ? `${(n / 1e6).toFixed(1)}M` : `${Math.round(n / 1e3)}K`);

const lines = [];
let total = 0;

const cat = read("CATALOG.json");
if (cat) {
  const v = sum(cat.videos.map((x) => x.views || 0));
  total += v;
  lines.push(["TikTok — @whistlegraph", cat.videos.length, v]);
}

const yt = read("YOUTUBE.json");
if (yt) {
  const own = yt.videos.filter((x) => x.source === "own");
  const rep = yt.videos.filter((x) => x.source === "repost");
  const ov = sum(own.map((x) => x.views));
  const rv = sum(rep.map((x) => x.views));
  total += ov + rv;
  lines.push(["YouTube — own channel", own.length, ov]);
  lines.push(["YouTube — confirmed reposts", rep.length, rv]);
}

const w = Math.max(...lines.map((l) => l[0].length));
console.log("\n  WHISTLEGRAPH — AGGREGATE REACH\n");
for (const [label, n, v] of lines) {
  console.log(`  ${label.padEnd(w)}  ${String(n).padStart(4)} videos  ${fmt(v).padStart(8)}  (${v.toLocaleString()})`);
}
console.log(`  ${"".padEnd(w)}  ${"".padStart(4)}          ${"—".padStart(8)}`);
console.log(`  ${"CONFIRMED TOTAL".padEnd(w)}  ${"".padStart(4)}          ${fmt(total).padStart(8)}  (${total.toLocaleString()})`);

const cand = read("YOUTUBE-CANDIDATES.json");
if (cand) {
  console.log(`\n  + ${cand.count} unreviewed YouTube search hits (${fmt(sum(cand.videos.map((v) => v.views)))} views) — mostly NOT ours`);
  console.log(`    (keyword collision with Smile.dk "Butterfly"); confirm real reposts with youtube-views.mjs --ids`);
}
console.log("");
