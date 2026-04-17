#!/usr/bin/env node
// Cross-references the thomaslawson.com site catalog (data/catalog.json) against
// the Valise API dump (valise/data/artworks.json) and writes gap.json.
//
// Produces:
//   - Per-period / per-page coverage counts (strong + weak matches)
//   - Lists of site artworks not in Valise (need uploading)
//   - Lists of Valise artworks not on the site (could be added to pages)

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const siteCat = JSON.parse(readFileSync(join(ROOT, "data/catalog.json"), "utf-8"));
const valise = JSON.parse(readFileSync(join(__dirname, "data/artworks.json"), "utf-8"));

// Pretty-URL map: period slug → public URL.
// Inferred from thomaslawson.com/wp-sitemap-posts-page-1.xml.
const PAGE_URL = {
  "1977-1979": "https://www.thomaslawson.com/inthestudio_1977-1979/",
  "1980-1982": "https://www.thomaslawson.com/inthestudio_1980-1982/",
  "1983-1987": "https://www.thomaslawson.com/?p=428",
  "1987-1990": "https://www.thomaslawson.com/inthestudio_1987-1990/",
  "1991-1993": "https://www.thomaslawson.com/inthestudio_1991-1993/",
  "1994-1998": "https://www.thomaslawson.com/inthestudio_1994-1998/",
  "1999-2006": "https://www.thomaslawson.com/inthestudio_1999-2006/",
  "2006-2010": "https://www.thomaslawson.com/inthestudio_2006-2010/",
  "2010-2015": "https://www.thomaslawson.com/inthestudio_2010-2015/",
  "2015-2016": "https://www.thomaslawson.com/inthestudio_2015-2016/",
  "2017-2020": "https://www.thomaslawson.com/inthestudio_2017-2020/",
  "beyond-the-studio": "https://www.thomaslawson.com/beyond-the-studio/",
};

function norm(s) {
  return (s || "")
    .toLowerCase()
    .replace(/\([^)]*\)/g, " ")
    .replace(/[/]+/g, " ")
    .replace(/\b(the|a|an|no|installation|detail|from)\b/g, " ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

const vByNorm = new Map();
for (const a of valise) {
  const n = norm(a.title);
  if (!n) continue;
  (vByNorm.get(n) ?? vByNorm.set(n, []).get(n)).push(a);
}

function findMatch(site) {
  const nt = norm(site.title);
  if (!nt) return null;
  const sy = site.year ? String(site.year) : "";

  if (vByNorm.has(nt)) {
    const hits = vByNorm.get(nt);
    const sameYear = sy && hits.find((h) => h.year === sy);
    if (sameYear) return { a: sameYear, score: 100, how: "exact+year" };
    if (!sy) return { a: hits[0], score: 85, how: "exact (site has no year)" };
    if (hits.length === 1) return { a: hits[0], score: 75, how: "exact, year differs" };
    return { a: hits[0], score: 70, how: "exact, multiple candidates" };
  }
  const subs = [];
  for (const a of valise) {
    const n = norm(a.title);
    if (n.length < 5 || nt.length < 5) continue;
    if (n.includes(nt) || nt.includes(n)) {
      const score = sy && a.year === sy ? 60 : 45;
      subs.push({ a, score, how: score === 60 ? "substring+year" : "substring" });
    }
  }
  subs.sort((a, b) => b.score - a.score);
  return subs[0] || null;
}

// Cross-reference
const matchedValiseIds = new Set();
const perPage = {};

function pageKey(site) {
  return site.period || site.section || "misc";
}

const siteArtworks = siteCat.artworks.map((s) => {
  const m = findMatch(s);
  const tier = !m
    ? "missing"
    : m.score >= 65
    ? "strong"
    : m.score >= 45
    ? "weak"
    : "missing";
  if (tier === "strong" || tier === "weak") matchedValiseIds.add(m.a.id);
  return {
    site: {
      id: s.id,
      serial: s.serial,
      title: s.title,
      year: s.year,
      medium: s.medium,
      dimensions: s.dimensions?.text || null,
      section: s.section,
      period: s.period,
      image: s.images?.[0]?.url || null,
      sourceUrl: s.sourceUrl,
    },
    match: m
      ? {
          tier,
          score: m.score,
          how: m.how,
          valise: {
            id: m.a.id,
            uid: m.a.uid,
            title: m.a.title,
            year: m.a.year,
            medium: m.a.medium,
            dimensions: m.a.dimensions,
            hasImage: !!m.a.images?.length,
            imageUrl: m.a.images?.[0]?.url || null,
          },
        }
      : { tier: "missing" },
  };
});

// Per-page rollup
for (const row of siteArtworks) {
  const k = pageKey(row.site);
  perPage[k] ??= {
    key: k,
    pageUrl: PAGE_URL[k] || null,
    total: 0,
    strong: 0,
    weak: 0,
    missing: 0,
    strongWithImage: 0,
  };
  perPage[k].total += 1;
  perPage[k][row.match.tier] += 1;
  if (row.match.tier === "strong" && row.match.valise?.hasImage) perPage[k].strongWithImage += 1;
}

for (const p of Object.values(perPage)) {
  p.replaceableTier = p.total === 0
    ? "empty"
    : p.strongWithImage / p.total >= 0.8
    ? "ready"
    : p.strongWithImage / p.total >= 0.4
    ? "partial"
    : "blocked";
}

// Valise-only: works in Valise that aren't matched to any site page.
// Keep them compact — just summary fields.
const valiseOnly = valise
  .filter((a) => !matchedValiseIds.has(a.id))
  .map((a) => ({
    id: a.id,
    uid: a.uid,
    title: a.title,
    year: a.year,
    medium: a.medium,
    dimensions: a.dimensions,
    hasImage: !!a.images?.length,
    imageUrl: a.images?.[0]?.url || null,
    tags: (a.tags || []).map((t) => t.title),
  }));

// Tiers of site works
const bucket = { strong: [], weak: [], missing: [] };
for (const row of siteArtworks) bucket[row.match.tier].push(row);

const report = {
  generatedAt: new Date().toISOString(),
  siteTotal: siteCat.artworks.length,
  valiseTotal: valise.length,
  strong: bucket.strong.length,
  weak: bucket.weak.length,
  missing: bucket.missing.length,
  valiseOnly: valiseOnly.length,
  pages: Object.values(perPage).sort((a, b) => a.key.localeCompare(b.key)),
  siteArtworks, // full list with match info
  valiseOnlyArtworks: valiseOnly,
};

import { mkdirSync } from "node:fs";
const gapDir = join(ROOT, "dist/valise/gap");
mkdirSync(gapDir, { recursive: true });
writeFileSync(join(gapDir, "gap.json"), JSON.stringify(report));

console.log(`Gap report → dist/valise/gap.json`);
console.log(`  Site artworks:     ${report.siteTotal}`);
console.log(`  Strong matches:    ${report.strong}`);
console.log(`  Weak matches:      ${report.weak}  (review needed)`);
console.log(`  Missing (upload):  ${report.missing}`);
console.log(`  Valise-only:       ${report.valiseOnly}  (could be added to pages)`);
console.log(`\nPage readiness:`);
for (const p of report.pages) {
  console.log(
    `  ${p.key.padEnd(20)} ${String(p.strong + "/" + p.total).padEnd(8)} strong, ${p.strongWithImage} w/image → ${p.replaceableTier}`,
  );
}
