#!/usr/bin/env node
// bbc-fetch.mjs — pull samples from the BBC Sound Effects archive
// (sound-effects.bbcrewind.co.uk) into pop/samples/<slug>/.
//
// ⚠ RemArc licence: NON-COMMERCIAL only (personal/educational/research +
// non-commercial live performance). Fetched samples are tagged
// commercialUse:false — never bake them into a DistroKid release master.
// See pop/lib/bbc-rewind.mjs for the full note.
//
// Each sample downloads as full-quality WAV when the CDN allows it, else
// falls back to the MP3 preview decoded to WAV (the manifest records which
// tier — "wav" or "preview" — landed per id).
//
// Usage:
//   # search only — print results so you can pick ids:
//   node pop/bin/bbc-fetch.mjs --query "pachinko" --list
//
//   # search + download the top N into pop/samples/<slug>/:
//   node pop/bin/bbc-fetch.mjs --query "pachinko" --count 6
//   node pop/bin/bbc-fetch.mjs --query "pachinko" --count 6 --slug pachinko-bbc
//
//   # download specific ids directly (skips search):
//   node pop/bin/bbc-fetch.mjs --slug pachinko-bbc --id 07022449,07032210
//
// Flags:
//   --query <text>   search text (required unless --id given)
//   --count N        number of top results to download (default 8)
//   --from N         search offset for paging (default 0)
//   --slug NAME      output dir under pop/samples/ (default: <query>-bbc)
//   --id A,B,C       download these exact BBC ids (slug required)
//   --list           search and print results only, download nothing

import { search, downloadSample, writeManifest, slugify, SAMPLES_DIR } from "../lib/bbc-rewind.mjs";
import { resolve } from "node:path";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
  else flags[a.slice(2)] = true;
}

const fmtDur = (s) => (typeof s === "number" ? `${s.toFixed(1)}s`.padStart(7) : "    ?  ");

function die(msg) { console.error(msg); process.exit(1); }

const ids = flags.id ? String(flags.id).split(",").map((s) => s.trim()).filter(Boolean) : null;

if (!flags.query && !ids) {
  die(
    "usage: bbc-fetch.mjs --query <text> [--count N] [--slug NAME] [--list]\n" +
    "       bbc-fetch.mjs --slug NAME --id <id,id,...>\n" +
    "⚠ RemArc licence — non-commercial use only (see pop/lib/bbc-rewind.mjs)"
  );
}

// ── direct id download (no search) ───────────────────────────────────
if (ids) {
  const slug = flags.slug ? slugify(flags.slug) : null;
  if (!slug) die("--id requires --slug NAME");
  const dir = resolve(SAMPLES_DIR, slug);
  const got = [];
  for (const id of ids) {
    process.stderr.write(`↓ ${id} … `);
    try {
      const r = await downloadSample(id, dir);
      console.error(r.skipped ? "cached" : `${r.quality} ${(r.bytes / 1e6).toFixed(1)} MB`);
      got.push({ id, description: "", category: "", durationSec: null, tags: [], quality: r.quality });
    } catch (e) { console.error(`FAILED — ${e.message}`); }
  }
  if (got.length) {
    const { manifest } = writeManifest({ slug, query: flags.query || slug, samples: got });
    console.error(`✓ ${got.length} sample(s) → ${dir}`);
    console.error(`  manifest: ${manifest}  (license: remarc-noncommercial)`);
  }
  process.exit(0);
}

// ── search ───────────────────────────────────────────────────────────
const count = Number(flags.count || 8);
const from = Number(flags.from || 0);
const { total, results } = await search({ query: flags.query, from, size: Math.max(count, flags.list ? 20 : count) });

console.error(`“${flags.query}” — ${total} result(s) in the archive:`);
for (const r of results) {
  console.error(`  ${String(r.id).padEnd(12)} ${fmtDur(r.durationSec)}  ${r.category ? `[${r.category}] ` : ""}${r.description}`);
}

if (flags.list || total === 0) {
  if (!total) console.error("(no results)");
  else console.error(`\n↳ download with: --count N  or  --slug NAME --id ${results.slice(0, 3).map((r) => r.id).join(",")}`);
  process.exit(0);
}

// ── download top `count` ─────────────────────────────────────────────
const slug = flags.slug ? slugify(flags.slug) : `${slugify(flags.query)}-bbc`;
const dir = resolve(SAMPLES_DIR, slug);
const picks = results.slice(0, count);
const got = [];
console.error(`\n→ ${slug}/`);
for (const r of picks) {
  process.stderr.write(`↓ ${r.id} … `);
  try {
    const d = await downloadSample(r.id, dir);
    console.error(d.skipped ? "cached" : `${d.quality} ${(d.bytes / 1e6).toFixed(1)} MB`);
    got.push({ ...r, quality: d.quality });
  } catch (e) { console.error(`FAILED — ${e.message}`); }
}

if (got.length) {
  const { manifest } = writeManifest({ slug, query: flags.query, samples: got });
  const previews = got.filter((g) => g.quality === "preview").length;
  console.error(`\n✓ ${got.length}/${picks.length} sample(s) → ${dir}`);
  if (previews) console.error(`  (${previews} fell back to MP3 preview — rerun from a residential IP for full WAV)`);
  console.error(`  manifest: ${manifest}  (license: remarc-noncommercial — non-commercial use only)`);
} else {
  die("\n✗ nothing downloaded");
}
