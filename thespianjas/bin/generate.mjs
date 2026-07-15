#!/usr/bin/env node
// Canonical Jeffrey platter refs → versioned relightable digital twin.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { falKey, dataUri } from "../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");
const argv = process.argv.slice(2);
const flag = (name, fallback) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[i + 1] : fallback;
};
const provider = flag("provider", "meshy");
const version = flag("version", "v001");
if (provider !== "meshy") throw new Error(`provider ${provider} not implemented yet`);

const identity = JSON.parse(readFileSync(resolve(ROOT, "identity.json"), "utf8"));
const refs = identity.references.map((p) => resolve(REPO, p));
if (!refs.every(existsSync)) throw new Error("one or more canonical platter references are missing");
const outDir = resolve(ROOT, "assets", "versions", version);
mkdirSync(outDir, { recursive: true });

const endpoint = "fal-ai/meshy/v6/multi-image-to-3d";
const input = {
  image_urls: refs.map(dataUri),
  topology: "quad",
  target_polycount: 30000,
  symmetry_mode: "auto",
  should_remesh: true,
  should_texture: true,
  enable_pbr: true,
  pose_mode: "a-pose",
  texture_prompt: "faithful natural studio portrait texture, white layered shirt with small yellow tiger patch, coral undershirt, blue wide-leg shorts, dark shoes, natural skin and medium brown hair",
  enable_rigging: true,
  rigging_height_meters: 1.82,
  enable_animation: true,
  animation_action_id: 0,
  enable_safety_checker: true
};

const auth = { Authorization: `Key ${falKey()}`, "Content-Type": "application/json" };
const sleep = (ms) => new Promise((ok) => setTimeout(ok, ms));
console.log(`thespianjas ${version} · ${endpoint} · ${refs.length} canonical refs`);
const sub = await fetch(`https://queue.fal.run/${endpoint}`, { method: "POST", headers: auth, body: JSON.stringify(input) });
if (!sub.ok) throw new Error(`submit ${sub.status}: ${(await sub.text()).slice(0, 500)}`);
const queued = await sub.json();
let state = "";
while (state !== "COMPLETED") {
  await sleep(5000);
  const status = await (await fetch(queued.status_url, { headers: auth })).json();
  if (status.status !== state) { state = status.status; console.log(`  ${state.toLowerCase()}`); }
  if (state === "FAILED" || status.error) throw new Error(JSON.stringify(status).slice(0, 800));
}
const result = await (await fetch(queued.response_url, { headers: auth })).json();
const files = {
  "model.glb": result.model_glb?.url || result.model_urls?.glb?.url,
  "model.usdz": result.model_urls?.usdz?.url,
  "rigged.glb": result.rigged_character_glb?.url,
  "idle.glb": result.animation_glb?.url,
  "preview.png": result.thumbnail?.url,
};
for (const [name, url] of Object.entries(files)) {
  if (!url) continue;
  const bytes = Buffer.from(await (await fetch(url)).arrayBuffer());
  writeFileSync(resolve(outDir, name), bytes);
  console.log(`  ${name} ${(bytes.length / 1024 / 1024).toFixed(1)} MB`);
}
const manifest = {
  slug: identity.slug,
  version,
  created: new Date().toISOString(),
  provider,
  endpoint,
  references: identity.references,
  requestId: queued.request_id,
  input: { ...input, image_urls: identity.references },
  outputs: Object.fromEntries(Object.entries(files).filter(([, url]) => url)),
};
writeFileSync(resolve(outDir, "manifest.json"), JSON.stringify(manifest, null, 2) + "\n");
console.log(`✓ ${outDir}`);
