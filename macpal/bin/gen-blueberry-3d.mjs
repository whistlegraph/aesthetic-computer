#!/usr/bin/env node
// macpal/bin/gen-blueberry-3d.mjs — render a 3D blueberry pal as an OBJ.
//
// Meshy-6 text-to-3d via the fal.ai queue API (fal-ai/meshy/v6/text-to-3d).
// Text → fully-textured mesh; we pull the OBJ + GLB + PBR texture PNGs down
// into macpal/Resources/blueberry-3d/ so MacPal's SceneKit avatar can load it.
//
//   node macpal/bin/gen-blueberry-3d.mjs                 # default cute pal
//   node macpal/bin/gen-blueberry-3d.mjs --lowpoly       # game-asset look
//   node macpal/bin/gen-blueberry-3d.mjs --prompt "…"    # custom prompt
//   node macpal/bin/gen-blueberry-3d.mjs --force         # re-render (ignore cache)
//
// A submitted job keeps running (and billing) on fal even if this process
// dies, so the queue handle is persisted beside the output — a rerun RESUMES
// the in-flight job instead of paying twice. (Same discipline as fal-seedance.)

import { mkdirSync, existsSync, readFileSync, writeFileSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { falKey } from "../../pop/lib/fal-seedance.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT_DIR = resolve(HERE, "../Resources/blueberry-3d");
const ENDPOINT = "fal-ai/meshy/v6/text-to-3d";

const argv = process.argv.slice(2);
const has = (f) => argv.includes(f);
const opt = (f, d) => {
  const i = argv.indexOf(f);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : d;
};

const FORCE = has("--force");
const LOWPOLY = has("--lowpoly");

// The blueberry pal: one plump, friendly berry with a tiny leaf-crown and a
// soft dusty bloom — a desktop companion, not a photoreal fruit. Texture
// prompt carries the surface look so geometry stays clean.
const PROMPT = opt("--prompt",
  "A single cute cartoon blueberry character, one plump round berry with a soft " +
  "matte deep-blue skin and a gentle frosty bloom, a small five-point star calyx " +
  "crown and one tiny green leaf on top, simple friendly mascot, smooth clean " +
  "geometry, centered, full body, neutral pose.");
const TEXTURE_PROMPT =
  "soft matte blueberry skin, deep indigo-blue with a dusty powder-blue bloom, " +
  "a subtle pale star calyx, fresh green leaf, gentle hand-illustrated look.";

const input = {
  prompt: PROMPT,
  mode: "full",                                  // textured, one shot
  model_type: LOWPOLY ? "lowpoly" : "standard",
  topology: "triangle",                          // ModelIO/SceneKit-friendly
  target_polycount: LOWPOLY ? 8000 : 30000,
  should_remesh: true,
  symmetry_mode: "auto",
  texture_prompt: TEXTURE_PROMPT,
  enable_pbr: true,
};

const OBJ_OUT = `${OUT_DIR}/blueberry.obj`;
const QUEUE_PATH = `${OUT_DIR}/.queue.json`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const log = (...a) => console.log(...a);

if (existsSync(OBJ_OUT) && !FORCE) {
  log(`✓ cached → ${OBJ_OUT}  (use --force to re-render)`);
  process.exit(0);
}

mkdirSync(OUT_DIR, { recursive: true });

let key;
try {
  key = falKey();
} catch (e) {
  console.error(`✗ ${e.message}`);
  console.error("  Add FAL_KEY to aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  console.error("  or run with FAL_KEY=… node macpal/bin/gen-blueberry-3d.mjs");
  process.exit(1);
}
const auth = { Authorization: `Key ${key}`, "Content-Type": "application/json" };

log(`▸ blueberry 3D · Meshy-6 text-to-3d · ${input.model_type} · ${input.target_polycount} tris`);

// ── submit (or resume) ───────────────────────────────────────────────────
let queued = null;
if (existsSync(QUEUE_PATH)) {
  queued = JSON.parse(readFileSync(QUEUE_PATH, "utf8"));
  log(`  ↻ resuming in-flight job from ${QUEUE_PATH.split("/").pop()}`);
}

const MAX_SUBMIT = 4;
for (let attempt = 1; !queued && attempt <= MAX_SUBMIT; attempt++) {
  const res = await fetch(`https://queue.fal.run/${ENDPOINT}`, {
    method: "POST", headers: auth, body: JSON.stringify(input),
  });
  if (res.ok) {
    queued = await res.json();
    writeFileSync(QUEUE_PATH, JSON.stringify(queued, null, 2));
    break;
  }
  const err = await res.text();
  // fal billing-state lag shows up as a spurious 403; retry transients.
  const transient = res.status === 429 || res.status >= 500 || res.status === 403;
  if (transient && attempt < MAX_SUBMIT) {
    log(`  ⚠ submit ${res.status} — retry ${attempt}/${MAX_SUBMIT - 1} in ${5 * attempt}s`);
    await sleep(5000 * attempt);
    continue;
  }
  console.error(`✗ submit ${res.status}: ${err.slice(0, 300)}`);
  process.exit(1);
}

// ── poll (rides out transient network failures) ───────────────────────────
const MAX_NET_FAILS = 8;
let netFails = 0;
const tryFetch = async (...args) => {
  try {
    const res = await fetch(...args);
    netFails = 0;
    return res;
  } catch (err) {
    netFails++;
    log(`  ⚠ ${err.cause?.code ?? err.message} — retry ${netFails}/${MAX_NET_FAILS}`);
    if (netFails >= MAX_NET_FAILS) throw err;
    await sleep(5000 * netFails);
    return null;
  }
};

const t0 = Date.now();
let status = "";
let lastTick = 0;
while (status !== "COMPLETED") {
  await sleep(4000);
  const res = await tryFetch(queued.status_url, { headers: auth });
  if (!res) continue;
  const body = await res.json();
  const elapsed = ((Date.now() - t0) / 1000).toFixed(0);
  if (body.status !== status) {
    status = body.status;
    lastTick = Date.now();
    log(`  ${status.toLowerCase()} · ${elapsed}s`);
  } else if (Date.now() - lastTick > 12000) {
    lastTick = Date.now();
    log(`  ${status.toLowerCase()} · ${elapsed}s`);
  }
  if (status === "FAILED" || body.error) {
    unlinkSync(QUEUE_PATH);
    console.error(`✗ generation failed: ${JSON.stringify(body).slice(0, 300)}`);
    process.exit(1);
  }
}

let result = null;
while (!result) {
  const res = await tryFetch(queued.response_url, { headers: auth });
  if (res) result = await res.json();
}

// ── download artifacts ─────────────────────────────────────────────────────
const urls = result.model_urls || {};
if (!urls.obj?.url) {
  console.error(`✗ no OBJ in response: ${JSON.stringify(result).slice(0, 400)}`);
  process.exit(1);
}

const download = async (url, dest) => {
  for (let i = 0; i < MAX_NET_FAILS; i++) {
    const res = await tryFetch(url);
    if (!res) continue;
    if (!res.ok) { console.error(`  ⚠ ${res.status} for ${dest.split("/").pop()}`); return false; }
    const buf = Buffer.from(await res.arrayBuffer());
    writeFileSync(dest, buf);
    log(`  ↓ ${dest.split("/").pop()} · ${(buf.length / 1e6).toFixed(2)}MB`);
    return true;
  }
  return false;
};

await download(urls.obj.url, OBJ_OUT);
if (urls.glb?.url) await download(urls.glb.url, `${OUT_DIR}/blueberry.glb`);   // self-contained backup
if (result.thumbnail?.url) await download(result.thumbnail.url, `${OUT_DIR}/blueberry-thumb.png`);

// PBR texture maps — MacPal applies base_color to the SceneKit material; the
// rest are kept for future PBR shading.
const tex = (result.texture_urls && result.texture_urls[0]) || {};
for (const [kind, file] of Object.entries(tex)) {
  if (file?.url) await download(file.url, `${OUT_DIR}/blueberry-${kind}.png`);
}

// Record provenance so a rebuild is reproducible.
writeFileSync(`${OUT_DIR}/meshy.json`, JSON.stringify({
  endpoint: ENDPOINT, input, seed: result.seed ?? null,
  actual_prompt: result.actual_prompt ?? null,
  model_urls: urls,
}, null, 2));

try { unlinkSync(QUEUE_PATH); } catch {}

log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s → ${OUT_DIR}`);
log(`  blueberry.obj${urls.glb ? " + .glb" : ""}${tex.base_color ? " + base_color texture" : ""}`);
