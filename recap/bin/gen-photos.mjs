#!/usr/bin/env node
// gen-photos.mjs — photo-ONLY batch gen for an audience. No puppeteer,
// no slide compositing — just OpenAI gpt-image-2 calls + save PNGs to
// recap/out/jeffrey-photos/<segment>.png. This is the RAM-cheap path.
//
// Iterates segments, gens missing (or all with --force), one at a time
// by default (1 concurrent). Up to `--concurrency N` (capped at 3) for
// users with headroom. The 8 GB MacBook should stay at 1.
//
// Each API call holds ~30 MB working set (8 reference images + base64
// response). Memory stays low. Wallclock ≈ N_photos × 200s when
// concurrency=1; ≈ N_photos × 200s / N when concurrent.
//
// Usage:
//   node bin/gen-photos.mjs <audience>
//   node bin/gen-photos.mjs <audience> --only 02_menuband_arc
//   node bin/gen-photos.mjs <audience> --force
//   node bin/gen-photos.mjs <audience> --concurrency 2

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { jeffreyRefs } from "../../marketing/lib/jeffrey-refs.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  } else positional.push(a);
}
const audienceName = positional[0];
if (!audienceName) {
  console.error("usage: gen-photos.mjs <audience-name> [--only NAME] [--force] [--concurrency N]");
  process.exit(1);
}
const force = !!flags.force;
const only = flags.only || null;
const CONCURRENCY = Math.max(1, Math.min(3, Number(flags.concurrency || 1)));

const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);
const PHOTOS_DIR = `${ROOT}/out/jeffrey-photos`;
mkdirSync(PHOTOS_DIR, { recursive: true });

// Shared SHOOT + SELFIE refs (2k-downscaled) live in
// marketing/lib/jeffrey-refs.mjs so recap + marketing don't drift.
const REFS = jeffreyRefs();

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
  throw new Error("OPENAI_API_KEY not found");
}
const apiKey = loadOpenAIKey();

async function genOne(segName, metaphor) {
  const outPath = `${PHOTOS_DIR}/${segName}.png`;
  if (existsSync(outPath) && !force) return { segName, status: "cached", outPath };
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", metaphor);
  fd.append("size", "1024x1536");
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  const t0 = Date.now();
  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}` },
    body: fd,
  });
  if (!res.ok) throw new Error(`OpenAI ${res.status}: ${(await res.text()).slice(0, 300)}`);
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) throw new Error(`no image returned`);
  writeFileSync(outPath, Buffer.from(b64, "base64"));
  return { segName, status: "fresh", outPath, durSec: (Date.now() - t0) / 1000 };
}

// ── build target list ─────────────────────────────────────────────
const targets = [];
for (const seg of audience.segments) {
  if (only && seg.name !== only) continue;
  const slide = audience.slides[seg.name];
  if (!slide || typeof slide !== "object" || !slide.metaphor) continue;
  targets.push({ segName: seg.name, metaphor: slide.metaphor });
}

console.log(`▸ ${targets.length} target(s) · concurrency=${CONCURRENCY} · force=${force}`);

// ── simple promise-pool ───────────────────────────────────────────
let cursor = 0;
const results = [];
async function worker() {
  while (cursor < targets.length) {
    const i = cursor++;
    const t = targets[i];
    try {
      console.log(`  → ${t.segName}`);
      const r = await genOne(t.segName, t.metaphor);
      if (r.status === "cached") console.log(`    · cached: ${t.segName}`);
      else console.log(`    ✓ ${t.segName} (${r.durSec.toFixed(1)}s)`);
      results.push(r);
    } catch (e) {
      console.error(`    ✗ ${t.segName}: ${e.message}`);
      results.push({ segName: t.segName, status: "fail", error: e.message });
    }
  }
}
await Promise.all(Array.from({ length: CONCURRENCY }, () => worker()));

const fresh = results.filter(r => r.status === "fresh").length;
const cached = results.filter(r => r.status === "cached").length;
const failed = results.filter(r => r.status === "fail").length;
console.log(`✓ done · fresh ${fresh} · cached ${cached} · failed ${failed}`);
if (failed) process.exit(2);
