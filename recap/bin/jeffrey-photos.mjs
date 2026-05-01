#!/usr/bin/env node
// jeffrey-photos.mjs — pre-pipeline step. For each segment in the audience
// config that has a `metaphor` field, call OpenAI gpt-image-2 (images.edit)
// with the platter SHOOT_REFS + SELFIE_REFS for identity grounding and save
// the result to recap/out/jeffrey-photos/<segment>.png.
//
// Each successful generation is also archived into the jeffrey platter as a
// new entry in the `gens` bucket: a dated copy of the PNG goes to
// system/public/assets/jeffreys/gens/<context>-<segment>-<ts>.png (synced to
// the CDN by `npm run assets:sync:up`), and an item with full provenance
// (model, refs, prompt, context, segment, timestamp) is appended to
// papers/jeffrey-platter/manifest.json under buckets.gens.items.
//
// Caching: skip if the working pipeline cache already exists; pass --force to
// regen. Run a single segment by name: --only 02_menuband.
//
// Usage:
//   node bin/jeffrey-photos.mjs jeffrey-24h
//   node bin/jeffrey-photos.mjs jeffrey-24h --force
//   node bin/jeffrey-photos.mjs jeffrey-24h --only 04_platter

import { readFileSync, writeFileSync, mkdirSync, existsSync, statSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");

const audienceName = process.argv[2] || "jeffrey-24h";
const force = process.argv.includes("--force");
const onlyIdx = process.argv.indexOf("--only");
const only = onlyIdx >= 0 ? process.argv[onlyIdx + 1] : null;

const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);

const PHOTOS_DIR = `${ROOT}/out/jeffrey-photos`;
mkdirSync(PHOTOS_DIR, { recursive: true });

// Platter archive — a copy of every successful gen goes here, plus a manifest
// entry. The dir is synced to assets.aesthetic.computer/jeffreys/gens/ via
// `npm run assets:sync:up`.
const PLATTER_GENS_DIR = `${REPO}/system/public/assets/jeffreys/gens`;
const PLATTER_MANIFEST = `${REPO}/papers/jeffrey-platter/manifest.json`;
mkdirSync(PLATTER_GENS_DIR, { recursive: true });

// Mirrors generate-neo.py refs.
const SHOOT_DIR = `${REPO}/portraits/jeffrey/corpus/shoot`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const SHOOT_REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
];
const SELFIE_REFS = [
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
  `${ARCHIVE_DIR}/2017-04-10_BStid5yjTHq.jpg`,
];
const REFS = [...SHOOT_REFS, ...SELFIE_REFS].filter((p) => {
  if (existsSync(p)) return true;
  console.warn(`  ⚠ ref missing, dropping: ${p}`);
  return false;
});

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) {
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("OPENAI_API_KEY not set and not found in vault devcontainer.env");
}

const apiKey = loadOpenAIKey();

const MODEL = "gpt-image-2";
const SIZE = "1024x1536";
const QUALITY = "high";

async function generate(metaphor, outPath) {
  const fd = new FormData();
  fd.append("model", MODEL);
  fd.append("prompt", metaphor);
  fd.append("size", SIZE);
  fd.append("quality", QUALITY);
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}` },
    body: fd,
  });
  if (!res.ok) {
    const err = await res.text();
    throw new Error(`OpenAI ${res.status}: ${err.slice(0, 500)}`);
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) throw new Error(`no image returned: ${JSON.stringify(json).slice(0, 200)}`);
  writeFileSync(outPath, Buffer.from(b64, "base64"));
  const usage = json.usage || {};
  return { tokens_in: usage.input_tokens, tokens_out: usage.output_tokens };
}

function isoStamp() {
  const d = new Date();
  const pad = (n) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}T${pad(d.getHours())}${pad(d.getMinutes())}`;
}

// Archive a successful gen into the platter: copy PNG to the assets/gens dir
// + append a metadata entry to papers/jeffrey-platter/manifest.json. Lazy-
// initializes the `gens` bucket if it doesn't exist yet.
function archiveToPlatter({ segName, metaphor, sourcePath, context }) {
  const stamp = isoStamp();
  const archiveName = `${context}-${segName}-${stamp}.png`;
  const archivePath = `${PLATTER_GENS_DIR}/${archiveName}`;
  writeFileSync(archivePath, readFileSync(sourcePath));

  const manifest = JSON.parse(readFileSync(PLATTER_MANIFEST, "utf8"));
  manifest.buckets.gens ??= {
    label: "Generated images — gpt-image-2 with platter-grounded identity (real+goofy default tone). One PNG per successful gen, dated. Synced to assets CDN via `npm run assets:sync:up`.",
    url_pattern: "https://assets.aesthetic.computer/jeffreys/gens/{name}",
    key_includes_extension: true,
    items: {},
  };
  manifest.buckets.gens.items[archiveName] = {
    model: MODEL,
    size: SIZE,
    quality: QUALITY,
    refs: REFS.map((r) => r.replace(REPO + "/", "")),
    context,
    segment: segName,
    generated: new Date().toISOString(),
    bytes: statSync(archivePath).size,
    prompt: metaphor,
  };
  writeFileSync(PLATTER_MANIFEST, JSON.stringify(manifest, null, 2) + "\n");
  return { archiveName, archivePath };
}

const context = `recap-${audienceName}`;

console.log(`refs: ${REFS.length} (${REFS.length} found)`);
console.log(`out:  ${PHOTOS_DIR}`);
console.log(`platter archive: ${PLATTER_GENS_DIR.replace(REPO + "/", "")}`);

let generated = 0, cached = 0, failed = 0;
for (const seg of audience.segments) {
  if (only && seg.name !== only) continue;
  const slide = audience.slides[seg.name];
  const metaphor = slide && typeof slide === "object" ? slide.metaphor : null;
  if (!metaphor) {
    console.log(`  · ${seg.name}: no metaphor, skipping`);
    continue;
  }
  const outPath = `${PHOTOS_DIR}/${seg.name}.png`;
  if (existsSync(outPath) && !force) {
    console.log(`  ✓ ${seg.name}.png (cached)`);
    cached++;
    continue;
  }
  process.stdout.write(`▸ ${seg.name}… `);
  const t0 = Date.now();
  try {
    const usage = await generate(metaphor, outPath);
    const archive = archiveToPlatter({ segName: seg.name, metaphor, sourcePath: outPath, context });
    const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
    const tok = usage.tokens_in
      ? ` · tokens in=${usage.tokens_in} out=${usage.tokens_out}`
      : "";
    console.log(`✓ ${elapsed}s${tok} → ${archive.archiveName}`);
    generated++;
  } catch (e) {
    console.log(`✗`);
    console.error(`  ${e.message}`);
    failed++;
  }
}

console.log(`✓ photos: ${generated} new, ${cached} cached, ${failed} failed`);
if (generated > 0) {
  console.log(`  · platter manifest updated: papers/jeffrey-platter/manifest.json`);
  console.log(`  · run \`node papers/jeffrey-platter/sync.mjs\` to refresh consumer copy`);
  console.log(`  · run \`npm run assets:sync:up\` to push gens/ to the CDN`);
}
// Don't fail the pipeline on per-segment gen failures — slides fall back to a
// dark-bg placeholder when the glob matches nothing. Re-run later to retry
// just the missing photos.
