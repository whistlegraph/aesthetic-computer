#!/usr/bin/env node
// Recover additional formats from an already-completed fal/Meshy request.
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { falKey } from "../../pop/lib/fal.mjs";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const version = process.argv[2] || "v001";
const dir = resolve(ROOT, "assets", "versions", version);
const manifestPath = resolve(dir, "manifest.json");
if (!existsSync(manifestPath)) throw new Error(`missing ${manifestPath}`);
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const auth = { Authorization: `Key ${falKey()}` };
const base = `https://queue.fal.run/${manifest.endpoint}/requests/${manifest.requestId}`;
const status = await (await fetch(`${base}/status`, { headers: auth })).json();
const url = status.response_url || status.responseUrl || `${base}/response`;
const response = await fetch(url, { headers: auth });
if (!response.ok) throw new Error(`recover ${response.status}: ${(await response.text()).slice(0, 400)}`);
const result = await response.json();
const outputs = {
  "model.usdz": result.model_urls?.usdz?.url,
  "model.fbx": result.model_urls?.fbx?.url,
  "rigged.fbx": result.rigged_character_fbx?.url,
  "idle.fbx": result.animation_fbx?.url,
};
for (const [name, remote] of Object.entries(outputs)) {
  if (!remote) continue;
  const bytes = Buffer.from(await (await fetch(remote)).arrayBuffer());
  writeFileSync(resolve(dir, name), bytes);
  console.log(`✓ ${name} ${(bytes.length / 1024 / 1024).toFixed(1)} MB`);
}
