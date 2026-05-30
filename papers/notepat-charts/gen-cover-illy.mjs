#!/usr/bin/env node
// papers/notepat-charts/gen-cover-illy.mjs — alternate cover image for
// the learn-notepat deck, generated via gpt-image-2. Companion to the
// code-rendered cover (see ChartRenderer.renderCover). Saves to
// papers/notepat-charts/illy-cover.png.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");
const OUT = resolve(HERE, "illy-cover.png");
const PROMPT_PATH = resolve(HERE, "cover.illy.txt");

const flags = Object.fromEntries(
  process.argv.slice(2).filter(a => a.startsWith("--")).map(a => [a.slice(2), true])
);
const FORCE = flags.force === true;

if (existsSync(OUT) && !FORCE) {
  console.log(`✓ cached: ${OUT.replace(REPO + "/", "")} (--force to regen)`);
  process.exit(0);
}

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

const apiKey = loadOpenAIKey();
const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
const size = "1024x1536";  // portrait, ~2:3 (matches the 4×6 deck page)

// Reference image — the existing flat notepat keymap diagram. The
// model uses it to read the exact color-to-letter assignments and
// renders a hand-drawn 3D keycap version on warm cream paper.
const REFS = [
  `${REPO}/slides/notepat-keymap/notepat-keymap.png`,
].filter((p) => {
  if (existsSync(p)) return true;
  console.warn(`  ⚠ ref missing, dropping: ${p}`);
  return false;
});

console.log(`▸ learn-notepat cover · ${size} · ${REFS.length} ref(s)`);
const t0 = Date.now();

const fd = new FormData();
fd.append("model", "gpt-image-2");
fd.append("prompt", prompt);
fd.append("size", size);
fd.append("quality", "high");
fd.append("n", "1");
for (const ref of REFS) {
  const buf = readFileSync(ref);
  const ext = ref.toLowerCase().endsWith(".png") ? "png"
            : ref.toLowerCase().endsWith(".webp") ? "webp" : "jpeg";
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
