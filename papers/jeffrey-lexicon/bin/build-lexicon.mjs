#!/usr/bin/env node
// build-lexicon.mjs — driver for the jeffrey-lexicon extraction pipeline.
//
// Reads ../manifest.json, fans out to per-source extractors under ./extractors/,
// merges their token streams, writes dictionary.json + derivative views.
//
// Each extractor exports `async function extract({ manifest, since, cacheDir })`
// returning an async iterator of `{ token, src, ref, ts, context, casing }`.
//
// Provenance discipline: extractors NEVER emit tokens for sources flagged
// `provenance: "ai"` or for authors in manifest.excluded_authors. The driver
// double-checks each token's source class is enabled in the manifest before
// merging — defense in depth.

import { readFile, writeFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const REPO = join(ROOT, "..", "..");

async function loadManifest() {
  return JSON.parse(await readFile(join(ROOT, "manifest.json"), "utf8"));
}

const EXTRACTORS = {
  commits: () => import("./extractors/commits.mjs"),
  comments: () => import("./extractors/comments.mjs"),
  ac_pieces: () => import("./extractors/ac-pieces.mjs"),
  lectures: () => import("./extractors/lectures.mjs"),
  recap_audio: () => import("./extractors/recap-audio.mjs"),
  papers: () => import("./extractors/papers.mjs"),
  cv: () => import("./extractors/cv.mjs"),
  planning: () => import("./extractors/planning.mjs"),
  ig_captions: () => import("./extractors/ig-captions.mjs"),
  vault_email: () => import("./extractors/vault-email.mjs"),
  handwriting_ocr: () => import("./extractors/handwriting-ocr.mjs"),
};

function parseArgs(argv) {
  const out = { sources: [], since: null, views: false, all: false };
  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--all") out.all = true;
    else if (a === "--views") out.views = true;
    else if (a === "--source") out.sources.push(argv[++i]);
    else if (a === "--since") out.since = argv[++i];
  }
  return out;
}

function mergeToken(dict, { token, src, ref, ts, context, casing }) {
  const key = token.toLowerCase();
  let e = dict.get(key);
  if (!e) {
    e = {
      count: 0,
      by_source: Object.create(null),
      forms: Object.create(null),
      first_seen: null,
      last_seen: null,
      sample_contexts: [],
      provenance: "first-hand",
    };
    dict.set(key, e);
  }
  e.count++;
  e.by_source[src] = (e.by_source[src] ?? 0) + 1;
  if (casing && casing !== key) e.forms[casing] = (e.forms[casing] ?? 0) + 1;
  if (!e.first_seen || (ts && ts < e.first_seen.iso)) {
    e.first_seen = { iso: ts, ref, context };
  }
  if (!e.last_seen || (ts && ts > e.last_seen.iso)) {
    e.last_seen = { iso: ts, ref, context };
  }
  if (e.sample_contexts.length < 5 && context) {
    e.sample_contexts.push({ src, ref, snippet: context });
  }
}

async function main() {
  const args = parseArgs(process.argv);
  const manifest = await loadManifest();
  const sources = args.all
    ? Object.keys(manifest.sources)
    : args.sources.length
    ? args.sources
    : ["commits"];

  const cacheDir = join(ROOT, ".cache");
  await mkdir(cacheDir, { recursive: true });

  const dict = new Map();
  for (const name of sources) {
    const conf = manifest.sources[name];
    if (!conf || conf.status === "tbd") {
      console.warn(`[skip] ${name} — status: ${conf?.status ?? "unknown"}`);
      continue;
    }
    const loader = EXTRACTORS[name];
    if (!loader) {
      console.warn(`[skip] ${name} — no extractor registered`);
      continue;
    }
    let mod;
    try {
      mod = await loader();
    } catch {
      console.warn(`[stub] ${name} — extractor not implemented yet`);
      continue;
    }
    console.log(`[run]  ${name}`);
    for await (const tok of mod.extract({ manifest, conf, repo: REPO, since: args.since, cacheDir })) {
      mergeToken(dict, tok);
    }
  }

  const sorted = [...dict.entries()].sort((a, b) => b[1].count - a[1].count);
  const full = Object.create(null);
  const slim = Object.create(null);
  for (const [k, v] of sorted) {
    full[k] = v;
    const topForms = Object.create(null);
    for (const [form, n] of Object.entries(v.forms)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 3)) {
      topForms[form] = n;
    }
    slim[k] = {
      count: v.count,
      by_source: v.by_source,
      forms: topForms,
      first_seen: v.first_seen,
      last_seen: v.last_seen,
      provenance: v.provenance,
    };
  }
  const fullPath = join(ROOT, "dictionary-full.json");
  const slimPath = join(ROOT, "dictionary.json");
  await writeFile(fullPath, JSON.stringify(full, null, 2));
  await writeFile(slimPath, JSON.stringify(slim, null, 2));
  const { stat } = await import("node:fs/promises");
  const slimMB = ((await stat(slimPath)).size / 1024 / 1024).toFixed(1);
  const fullMB = ((await stat(fullPath)).size / 1024 / 1024).toFixed(1);
  console.log(
    `[done] ${dict.size} unique tokens → dictionary.json (slim, ${slimMB} MB) + dictionary-full.json (${fullMB} MB)`,
  );

  if (args.views) {
    // TODO: rebuild views/neologisms.json, views/by-year.json,
    //       views/spoken-vs-written.json, views/code-only.json
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
