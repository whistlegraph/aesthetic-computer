// whistlegraph codes — assign a unique shortcode to every clustered song
//
//   node codes.mjs            # merge the naming batches → CODES.json
//
// Consumes the per-batch naming files written by the naming subagents
// (scratchpad/names-batch-*.json: [{i, kind, title, code}]) plus
// downloads/SONGS.json, and produces downloads/CODES.json — the master
// index: one row per cluster with a globally-unique 4-ish char code (like
// imab), title, kind (graph/talk/other), performance count, combined
// views, date span, and the id of its most-viewed clip (whose final frame
// is the glyph). The ten site codes are reserved and always win a
// collision. Everything else keeps its proposed code unless taken, in
// which case it's minimally mutated (trailing digit) to stay unique.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const DOWNLOADS = join(HERE, "downloads");
// Naming batches (names-batch-<n>.json: [{i, kind, title, code}]) written by
// the naming agents. Overridable via WG_SCRATCH; defaults to a stable dir
// under downloads so a re-name run isn't tied to any one session's tmp path.
const SCRATCH = process.env.WG_SCRATCH || join(DOWNLOADS, "naming-batches");

// The ten already live on whistlegraph.org — their codes are canonical.
const RESERVED = {
  "Butterfly Cosplayer": "imab",
  "Lately When I Fly": "l8ly",
  "Time To Grow": "grow",
  "I Don't Need an iPhone": "idni",
  "People Pleaser": "ppl",
  "What's Inside Your Heart?": "wiyh",
  Loner: "lonr",
  "Slinky Dog": "sdog",
  "Mommy Wow": "w0w",
  Puzzle: "puzz",
};

const songs = JSON.parse(readFileSync(join(DOWNLOADS, "SONGS.json"), "utf8")).songs;

// Code stickiness: a code, once assigned, is a permanent identifier — its
// bucket assets (/whistlegraph/index/<code>.{jpg,mp4}) live under that name.
// Re-clustering must not churn codes. We remember each prior code by the
// top-clip id it was minted from; a new cluster containing that clip
// inherits the code. When two old clusters merge, the survivor keeps the
// code of whichever old cluster had more views (or the reserved one).
const prior = existsSync(join(DOWNLOADS, "CODES.json"))
  ? JSON.parse(readFileSync(join(DOWNLOADS, "CODES.json"), "utf8")).songs
  : [];
const priorByGlyph = new Map(
  prior.map((p) => [p.glyph, { code: p.code, title: p.title, kind: p.kind, views: p.views }]),
);

// Gather the batch outputs, keyed by cluster index.
const named = new Map();
for (let b = 0; b < 32; b += 1) {
  const f = join(SCRATCH, `names-batch-${b}.json`);
  if (!existsSync(f)) continue;
  for (const row of JSON.parse(readFileSync(f, "utf8"))) {
    if (typeof row.i === "number") named.set(row.i, row);
  }
}
if (!named.size) {
  console.error("no names-batch-*.json found — run the naming agents first");
  process.exit(1);
}

const cleanCode = (s) =>
  (s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]/g, "")
    .slice(0, 5) || "wg";

// Order: reserved-known first (stable), then by performances, then views —
// so the most-established songs claim the tidiest codes.
const rows = songs.map((c, i) => ({ i, cluster: c, name: named.get(i) }));
rows.sort((a, b) => {
  const ar = RESERVED[a.cluster.title] ? 0 : 1;
  const br = RESERVED[b.cluster.title] ? 0 : 1;
  return (
    ar - br ||
    b.cluster.performances - a.cluster.performances ||
    b.cluster.views - a.cluster.views
  );
});

const taken = new Set();
const unique = (want) => {
  let code = want;
  if (!taken.has(code)) {
    taken.add(code);
    return code;
  }
  // Try dropping to 3 chars + a counter, then 4 + counter.
  const base = want.slice(0, 3);
  for (let n = 2; n < 100; n += 1) {
    code = `${base}${n}`;
    if (!taken.has(code)) {
      taken.add(code);
      return code;
    }
  }
  code = `${want}${taken.size}`;
  taken.add(code);
  return code;
};

const out = rows.map(({ cluster, name }) => {
  const top = [...cluster.clips].sort((a, b) => (b.views || 0) - (a.views || 0))[0];
  // Inherit code+title+kind from a prior run, keyed by clip id (stable
  // across re-clustering; the naming batch's index join is not). When a
  // cluster merges several prior ones, the reserved / highest-views prior
  // wins — so e.g. Triangles absorbs its chalk-take rather than the reverse.
  const inherited = cluster.clips
    .map((c) => priorByGlyph.get(c.id))
    .filter(Boolean)
    .sort((a, b) => (RESERVED[cluster.title] === b.code) - (RESERVED[cluster.title] === a.code) || b.views - a.views)[0];
  // Known (seed-matched) titles are ground truth and override a stale prior.
  const title = cluster.known ? cluster.title : inherited?.title || name?.title || cluster.title;
  const kind = inherited?.kind || name?.kind || "graph";
  const want =
    RESERVED[cluster.title] ||
    inherited?.code ||
    cleanCode(name?.code || title.replace(/\s+/g, ""));
  const code = unique(want);
  return {
    code,
    title,
    kind,
    known: cluster.known,
    performances: cluster.performances,
    views: cluster.views,
    span: cluster.span,
    glyph: top.id, // final frame of this clip is the graph's glyph
    clips: cluster.clips.map((c) => c.id),
  };
});

// Sort the file the way a human reads it: graphs first, by reach.
out.sort((a, b) => {
  const rank = (k) => (k === "graph" ? 0 : k === "talk" ? 1 : 2);
  return rank(a.kind) - rank(b.kind) || b.views - a.views;
});

writeFileSync(
  join(DOWNLOADS, "CODES.json"),
  JSON.stringify(
    { generated: new Date().toISOString(), count: out.length, songs: out },
    null,
    1,
  ),
);

const graphs = out.filter((o) => o.kind === "graph");
const talk = out.filter((o) => o.kind === "talk");
const other = out.filter((o) => o.kind === "other");
console.log(
  `${out.length} coded → ${graphs.length} graphs, ${talk.length} talk, ${other.length} other`,
);
console.log(`\ntop graphs:`);
for (const g of graphs.slice(0, 25)) {
  const v = g.views >= 1e6 ? `${(g.views / 1e6).toFixed(1)}M` : `${Math.round(g.views / 1e3)}K`;
  console.log(`  ${g.code.padEnd(6)} ${String(g.performances).padStart(2)}×  ${v.padStart(7)}  ${g.title}`);
}
console.log(`\n→ ${join(DOWNLOADS, "CODES.json")}`);
