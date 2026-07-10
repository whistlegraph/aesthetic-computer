#!/usr/bin/env node
// macpal/bin/anim-blueberry-3d.mjs — rig + animate the blueberry pal.
//
// Meshy auto-rigging + multi-animation via fal (fal-ai/meshy/rigging/multi-animation).
// Feeds the static text-to-3d GLB (from gen-blueberry-3d.mjs) back in, rigs it as
// a humanoid, and bakes preset motion clips. Each clip comes back as its own
// rigged GLB; we keep idle as the resting loop plus a couple of gestures.
//
//   node macpal/bin/anim-blueberry-3d.mjs                  # idle + wave + cheer
//   node macpal/bin/anim-blueberry-3d.mjs --model-url URL  # rig a different mesh
//   node macpal/bin/anim-blueberry-3d.mjs --ids 0,28,298   # custom action ids
//
// Action ids come from Meshy's library (https://docs.meshy.ai/en/api/animation-library):
//   0 Idle · 28 Big_Wave_Hello · 298 Cheer_with_Both_Hands_Up
//
// Output → system/public/assets/macpal/blueberry-3d/ (DO Spaces bin, gitignored).
// A submitted job keeps running/billing on fal even if this dies, so the queue
// handle is persisted and a rerun RESUMES instead of paying twice.

import { mkdirSync, existsSync, readFileSync, writeFileSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { falKey } from "../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENDPOINT = "fal-ai/meshy/rigging/multi-animation";

const argv = process.argv.slice(2);
const opt = (f, d) => { const i = argv.indexOf(f); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const FORCE = argv.includes("--force");

const NAME = opt("--name", "blueberry");   // per-machine model (blueberry, neo, …)
const OUT_DIR = resolve(HERE, `../../system/public/assets/macpal/${NAME}-3d`);

// The static mesh to rig. Defaults to the fal-hosted GLB from the last
// gen-3d run for this NAME (recorded in meshy.json); pass --model-url to override.
let MODEL_URL = opt("--model-url", null);
if (!MODEL_URL) {
  if (existsSync(`${OUT_DIR}/meshy.json`)) {
    MODEL_URL = JSON.parse(readFileSync(`${OUT_DIR}/meshy.json`, "utf8"))?.model_urls?.glb?.url || null;
  }
}
if (!MODEL_URL) {
  console.error("✗ no model_url — pass --model-url, or run gen-blueberry-3d.mjs first (its meshy.json glb url is reused).");
  process.exit(1);
}

const IDS = opt("--ids", "0,28,298").split(",").map((n) => parseInt(n.trim(), 10)).filter(Number.isFinite);
// safety checker off — the blueberry's stubby limbs can trip the humanoid gate.
const input = { model_url: MODEL_URL, animation_action_ids: IDS, height_meters: 1.0, enable_safety_checker: false };

const QUEUE_PATH = `${OUT_DIR}/.anim-queue.json`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const log = (...a) => console.log(...a);

mkdirSync(OUT_DIR, { recursive: true });

if (existsSync(`${OUT_DIR}/${NAME}-idle.glb`) && !FORCE) {
  log(`✓ cached → ${OUT_DIR}/${NAME}-idle.glb  (use --force to re-rig)`);
  process.exit(0);
}

let key;
try { key = falKey(); }
catch (e) { console.error(`✗ ${e.message}`); process.exit(1); }
const auth = { Authorization: `Key ${key}`, "Content-Type": "application/json" };

log(`▸ rig + animate · ${ENDPOINT} · ids [${IDS.join(", ")}]`);
log(`  source: ${MODEL_URL.slice(0, 72)}…`);

// ── submit (or resume) ───────────────────────────────────────────────────
let queued = existsSync(QUEUE_PATH) ? JSON.parse(readFileSync(QUEUE_PATH, "utf8")) : null;
if (queued) log(`  ↻ resuming in-flight job`);
const MAX_SUBMIT = 4;
for (let attempt = 1; !queued && attempt <= MAX_SUBMIT; attempt++) {
  const res = await fetch(`https://queue.fal.run/${ENDPOINT}`, {
    method: "POST", headers: auth, body: JSON.stringify(input),
  });
  if (res.ok) { queued = await res.json(); writeFileSync(QUEUE_PATH, JSON.stringify(queued, null, 2)); break; }
  const err = await res.text();
  const transient = res.status === 429 || res.status >= 500 || res.status === 403;
  if (transient && attempt < MAX_SUBMIT) {
    log(`  ⚠ submit ${res.status} — retry ${attempt}/${MAX_SUBMIT - 1} in ${5 * attempt}s`);
    await sleep(5000 * attempt); continue;
  }
  console.error(`✗ submit ${res.status}: ${err.slice(0, 300)}`); process.exit(1);
}

// ── poll ───────────────────────────────────────────────────────────────────
const MAX_NET_FAILS = 8;
let netFails = 0;
const tryFetch = async (...args) => {
  try { const res = await fetch(...args); netFails = 0; return res; }
  catch (err) {
    netFails++; log(`  ⚠ ${err.cause?.code ?? err.message} — retry ${netFails}/${MAX_NET_FAILS}`);
    if (netFails >= MAX_NET_FAILS) throw err;
    await sleep(5000 * netFails); return null;
  }
};

const t0 = Date.now();
let status = "", lastTick = 0;
while (status !== "COMPLETED") {
  await sleep(4000);
  const res = await tryFetch(queued.status_url, { headers: auth });
  if (!res) continue;
  const body = await res.json();
  const elapsed = ((Date.now() - t0) / 1000).toFixed(0);
  if (body.status !== status) { status = body.status; lastTick = Date.now(); log(`  ${status.toLowerCase()} · ${elapsed}s`); }
  else if (Date.now() - lastTick > 12000) { lastTick = Date.now(); log(`  ${status.toLowerCase()} · ${elapsed}s`); }
  if (status === "FAILED" || body.error) {
    unlinkSync(QUEUE_PATH);
    console.error(`✗ rigging failed: ${JSON.stringify(body).slice(0, 400)}`); process.exit(1);
  }
}

let result = null;
while (!result) { const res = await tryFetch(queued.response_url, { headers: auth }); if (res) result = await res.json(); }
writeFileSync(`${OUT_DIR}/rig-result.json`, JSON.stringify(result, null, 2));   // raw, for debugging
if (!(result.animations?.length) && !result.rigged_character_glb?.url) {
  console.error(`✗ empty rig result — raw saved to rig-result.json:\n${JSON.stringify(result).slice(0, 500)}`);
  try { unlinkSync(QUEUE_PATH); } catch {}
  process.exit(1);
}

// ── download artifacts ─────────────────────────────────────────────────────
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

const CLIP = { 0: "idle", 11: "idle", 12: "idle", 28: "wave", 298: "cheer", 49: "cheer", 44: "jump", 61: "jump" };
if (result.rigged_character_glb?.url) await download(result.rigged_character_glb.url, `${OUT_DIR}/${NAME}-rigged.glb`);
for (const a of result.animations || []) {
  const nm = CLIP[a.action_id] || `anim-${a.action_id}`;
  if (a.animation_glb?.url) await download(a.animation_glb.url, `${OUT_DIR}/${NAME}-${nm}.glb`);
}

writeFileSync(`${OUT_DIR}/anim.json`, JSON.stringify({
  endpoint: ENDPOINT, input, rig_task_id: result.rig_task_id ?? null,
  animations: (result.animations || []).map((a) => ({ action_id: a.action_id, glb: a.animation_glb?.url })),
}, null, 2));

try { unlinkSync(QUEUE_PATH); } catch {}
log(`✓ ${((Date.now() - t0) / 1000).toFixed(0)}s → ${OUT_DIR}  (idle/wave/cheer GLBs)`);
