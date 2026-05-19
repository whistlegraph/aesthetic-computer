#!/usr/bin/env node
// gen-header.mjs — generate a paper-header illustration via gpt-image-2 with
// jeffrey identity refs (AV-shoot headshots) AND first-person POV refs (the
// 90 platter screenshots cached at portraits/jeffrey/corpus/screenshots/).
//
// Reads:    papers/figures/<slug>.txt   (full prompt body)
// Writes:   papers/figures/<slug>_<timestamp>.png
//
// Mirrors the call surface of gen-cover.mjs but adds reference images so the
// generation locks jeffrey's identity AND the candid-laptop-POV atmosphere.
//
// Usage:
//   node papers/bin/gen-header.mjs header-prompt-wowed-1 --size 1024x1536
//   node papers/bin/gen-header.mjs header-prompt-jam-field --size 1536x1024 --quality high
//   node papers/bin/gen-header.mjs header-prompt-wowed-2 --pov 6     # use 6 POV refs (default 4)

import { readFileSync, writeFileSync, existsSync, readdirSync, mkdirSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS_DIR = resolve(HERE, "..");
const REPO = resolve(PAPERS_DIR, "..");
const FIG_DIR = join(PAPERS_DIR, "figures");
const SHOOT_DIR = join(REPO, "portraits", "jeffrey", "corpus", "shoot");
const POV_DIR = join(REPO, "portraits", "jeffrey", "corpus", "screenshots");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next && !next.startsWith("--")) {
      flags[a.slice(2)] = next;
      i++;
    } else {
      flags[a.slice(2)] = true;
    }
  } else {
    positional.push(a);
  }
}

const slug = positional[0];
if (!slug) {
  console.error("usage: gen-header.mjs <slug> [--size 1024x1536] [--quality high] [--pov N]");
  console.error("  slug = filename stem under papers/figures/, e.g. header-prompt-wowed-1");
  process.exit(2);
}

const SIZE = flags.size || "1024x1536";
const QUALITY = flags.quality || "high";
const POV_COUNT = parseInt(flags.pov || "4", 10);
const MODEL = flags.model || "gpt-image-2";

const promptPath = join(FIG_DIR, `${slug}.txt`);
if (!existsSync(promptPath)) {
  console.error(`prompt file not found: ${promptPath}`);
  process.exit(1);
}
const prompt = readFileSync(promptPath, "utf8").trim();

const SHOOT_REFS = [
  join(SHOOT_DIR, "jeffery-av--07.jpg"),
  join(SHOOT_DIR, "jeffery-av--01.jpg"),
  join(SHOOT_DIR, "jeffery-av--04.jpg"),
];
for (const r of SHOOT_REFS) {
  if (!existsSync(r)) {
    console.error(`shoot ref not found: ${r}`);
    process.exit(1);
  }
}

let povRefs = [];
if (existsSync(POV_DIR)) {
  const all = readdirSync(POV_DIR)
    .filter(n => n.endsWith(".webp"))
    .map(n => join(POV_DIR, n));
  // Pick evenly spaced subset for variety across the 90 captures
  const step = Math.max(1, Math.floor(all.length / POV_COUNT));
  for (let i = 0; i < POV_COUNT && i * step < all.length; i++) {
    povRefs.push(all[i * step]);
  }
}

const refs = [...SHOOT_REFS, ...povRefs];
console.error(`refs (${refs.length}): ${refs.length} total — ${SHOOT_REFS.length} identity + ${povRefs.length} POV`);
console.error(`model=${MODEL} size=${SIZE} quality=${QUALITY}`);
console.error(`prompt: ${promptPath} (${prompt.length} chars)`);

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

async function main() {
  const apiKey = loadOpenAIKey();
  const t0 = Date.now();

  // Multipart form for /v1/images/edits
  const form = new FormData();
  form.append("model", MODEL);
  form.append("prompt", prompt);
  form.append("size", SIZE);
  form.append("quality", QUALITY);
  form.append("n", "1");
  for (const r of refs) {
    const buf = readFileSync(r);
    const ext = r.split(".").pop().toLowerCase();
    const mime = ext === "webp" ? "image/webp" : ext === "png" ? "image/png" : "image/jpeg";
    form.append("image[]", new Blob([buf], { type: mime }), r.split("/").pop());
  }

  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}` },
    body: form,
  });

  if (!res.ok) {
    const err = await res.text();
    console.error(`HTTP ${res.status}: ${err.slice(0, 800)}`);
    process.exit(1);
  }

  const data = await res.json();
  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  if (!data.data || !data.data[0] || !data.data[0].b64_json) {
    console.error("no image data in response:", JSON.stringify(data).slice(0, 400));
    process.exit(1);
  }

  const ts = new Date().toISOString().replace(/[:.]/g, "-").slice(0, 19);
  const outPath = join(FIG_DIR, `${slug}_${ts}.png`);
  mkdirSync(FIG_DIR, { recursive: true });
  writeFileSync(outPath, Buffer.from(data.data[0].b64_json, "base64"));
  console.error(`done in ${elapsed}s → ${outPath}`);
  if (data.usage) {
    console.error(`tokens: input=${data.usage.input_tokens || 0} output=${data.usage.output_tokens || 0}`);
  }
}

main().catch(e => { console.error(`error: ${e.message}`); process.exit(1); });
