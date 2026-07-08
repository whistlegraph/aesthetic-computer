// whistlegraph.org site data — emit graphs.json from CODES.json
//
//   node gen-site-data.mjs
//
// The site loads system/public/whistlegraph.org/graphs.json: every graph
// (kind==="graph") with code, title, composer, year, views, performances,
// and whether it has a glyph. The curated ten carry extra fields (slug,
// color, canonical) so their detail pages keep the score sheet + drawn
// animation served from /whistlegraph/<slug>/; everyone else uses the
// bucket assets at /whistlegraph/index/<code>.{jpg,mp4}.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const CODES = join(HERE, "downloads", "CODES.json");
const OUT = join(HERE, "..", "..", "system", "public", "whistlegraph.org", "graphs.json");

// The curated ten — canonical order, composers, hues, slugs (mirror the
// old hardcoded table). imab also has a drawn canonical animation.
const CURATED = {
  imab: { by: "Jeffrey Alan Scudder", slug: "butterfly-cosplayer", c: "#ff9600", canonical: true },
  l8ly: { by: "Jeffrey Alan Scudder", slug: "lately-when-i-fly", c: "#7a1fe6" },
  grow: { by: "Alex Freundlich", slug: "time-to-grow", c: "#ff8fbf" },
  idni: { by: "Whistlegraph", slug: "i-dont-need-an-iphone", c: "#e60e0e" },
  ppl: { by: "Jeffrey Alan Scudder", slug: "people-pleaser", c: "#be50dc" },
  wiyh: { by: "Whistlegraph", slug: "whats-inside-your-heart", c: "#0000f5" },
  lonr: { by: "Camille Klein", slug: "loner", c: "#ff8282" },
  sdog: { by: "Alex Freundlich", slug: "slinky-dog", c: "#101014" },
  w0w: { by: "Whistlegraph", slug: "mommy-wow", c: "#ffc800" },
  puzz: { by: "Camille Klein", slug: "puzzle", c: "#30c8fc" },
};
const ORDER = Object.keys(CURATED);

const { songs } = JSON.parse(readFileSync(CODES, "utf8"));
const graphs = songs.filter((s) => s.kind === "graph");
const byCode = Object.fromEntries(graphs.map((g) => [g.code, g]));

// Graphs missing a glyph frame (audio-only TikTok posts, no video stream).
const NO_GLYPH = new Set(["crep", "meet", "kvds"]);

const row = (g) => {
  const cur = CURATED[g.code];
  const out = {
    code: g.code,
    title: g.title,
    by: cur?.by ?? "Whistlegraph",
    year: Number(g.span[0].slice(0, 4)),
    views: g.views,
    perf: g.performances,
    c: cur?.c ?? "#b44887",
  };
  if (cur) {
    out.slug = cur.slug;
    if (cur.canonical) out.canonical = true;
  }
  if (NO_GLYPH.has(g.code)) out.noGlyph = true;
  return out;
};

// Curated ten first in canonical order; then the rest by reach.
const curatedRows = ORDER.filter((c) => byCode[c]).map((c) => row(byCode[c]));
const rest = graphs
  .filter((g) => !CURATED[g.code])
  .sort((a, b) => b.views - a.views)
  .map(row);

const data = {
  generated: new Date().toISOString().slice(0, 10),
  count: curatedRows.length + rest.length,
  curated: curatedRows.length,
  graphs: [...curatedRows, ...rest],
};
writeFileSync(OUT, JSON.stringify(data));
console.log(`wrote ${data.count} graphs (${data.curated} curated) → ${OUT}`);
