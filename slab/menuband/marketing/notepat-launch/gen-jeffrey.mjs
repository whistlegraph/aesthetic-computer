#!/usr/bin/env node
// slab/menuband/marketing/notepat-launch/gen-jeffrey.mjs
// Hero illustration: Jeffrey playing BOTH neo colorways at once (two hands,
// citrus + indigo), via gpt-image-2 (images/edits). Feeds a portrait ref +
// both two-tone colorway machines so likeness and piano-key treatment carry.
//
//   node gen-jeffrey.mjs           # cached
//   node gen-jeffrey.mjs --force   # regenerate

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..", "..");
const PROMPT_PATH = resolve(HERE, "jeffrey-duo.illy.txt");
const OUT = resolve(HERE, "illy-jeffrey-duo.png");

const FORCE = process.argv.slice(2).includes("--force");

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
  throw new Error("OPENAI_API_KEY not set and not found in vault");
}

if (existsSync(OUT) && !FORCE) {
  console.log(`✓ cached: ${OUT.replace(REPO + "/", "")} (--force to regen)`);
  process.exit(0);
}

const apiKey = loadOpenAIKey();
const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
const size = "1024x1536"; // portrait hero — headline room above

// Refs: Jeffrey (likeness) + a full open-laptop shot (correct lid/body/
// perspective) + both flat colorway keyboards (bodies + piano-key layout).
// More real input imagery → less invented, warped geometry.
const REFS = [
  `${HERE}/refs/jeffrey-face.jpg`,
  `${HERE}/refs/neo-open-display.jpg`,
  `${HERE}/refs/neo-twotone-citrus-blue.jpg`,
  `${HERE}/refs/neo-twotone-indigo-green.jpg`,
].filter((p) => {
  if (existsSync(p)) return true;
  console.warn(`  ⚠ ref missing, dropping: ${p}`);
  return false;
});

console.log(`▸ jeffrey duo hero · ${size} · ${REFS.length} ref(s)`);
const t0 = Date.now();

const fd = new FormData();
fd.append("model", "gpt-image-2");
fd.append("prompt", prompt);
fd.append("size", size);
fd.append("quality", "high");
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
  console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 600)}`);
  process.exit(1);
}
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) {
  console.error(`✗ no image: ${JSON.stringify(json).slice(0, 280)}`);
  process.exit(1);
}
writeFileSync(OUT, Buffer.from(b64, "base64"));
console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT.replace(REPO + "/", "")}`);
