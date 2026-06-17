#!/usr/bin/env node
// americomputadora/bin/gen-illy.mjs — generate the album cover via
// gpt-image-2 (text-to-image, no identity refs — it's an object still
// life, not a portrait), then embed it into the mastered mp3 as ID3 art.
//
// Prompt: pop/americomputadora/americomputadora.illy.txt (papers voice)
// Output: pop/americomputadora/out/americomputadora-cover.png
// Embed:  pop/americomputadora/out/americomputadora.mp3 (ID3v2 picture)
//
// Usage:
//   node bin/gen-illy.mjs            # cached if cover exists
//   node bin/gen-illy.mjs --force    # regenerate
//   node bin/gen-illy.mjs --embed-only

import { readFileSync, writeFileSync, existsSync, mkdirSync, renameSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  if (process.argv[i].startsWith("--")) flags[process.argv[i].slice(2)] = true;
}
const FORCE = flags.force === true;
const SIZE = "1024x1024";

const PROMPT_PATH = `${LANE}/americomputadora.illy.txt`;
const OUT_PATH = `${LANE}/out/americomputadora-cover.png`;
const MP3_PATH = `${LANE}/out/americomputadora.mp3`;
mkdirSync(`${LANE}/out`, { recursive: true });

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

function embedCover() {
  if (!existsSync(OUT_PATH) || !existsSync(MP3_PATH)) {
    console.warn(`  ⚠ skip embed — need both ${OUT_PATH} and ${MP3_PATH}`);
    return;
  }
  const tmp = `${MP3_PATH}.embed.mp3`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", MP3_PATH, "-i", OUT_PATH,
    "-map", "0:a", "-map", "1:v", "-c", "copy", "-id3v2_version", "3",
    "-metadata:s:v", "title=cover", "-metadata:s:v", "comment=Cover (front)",
    "-disposition:v", "attached_pic", tmp], { stdio: "inherit" });
  if (r.status !== 0 || !existsSync(tmp)) { console.error("✗ embed failed"); return; }
  renameSync(tmp, MP3_PATH);
  console.log(`✓ cover embedded → ${MP3_PATH.replace(REPO + "/", "")}`);
}

if (flags["embed-only"]) { embedCover(); process.exit(0); }

if (existsSync(OUT_PATH) && !FORCE) {
  console.log(`✓ cached cover → ${OUT_PATH.replace(REPO + "/", "")} (use --force to regen)`);
  embedCover();
  process.exit(0);
}

const apiKey = loadOpenAIKey();
const prompt = readFileSync(PROMPT_PATH, "utf8").trim();
console.log(`▸ americomputadora cover · ${SIZE} · gpt-image-2`);
const t0 = Date.now();

const res = await fetch("https://api.openai.com/v1/images/generations", {
  method: "POST",
  headers: { Authorization: `Bearer ${apiKey}`, "content-type": "application/json" },
  body: JSON.stringify({ model: "gpt-image-2", prompt, size: SIZE, quality: "high", n: 1 }),
});
if (!res.ok) {
  console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 600)}`);
  process.exit(1);
}
const json = await res.json();
const b64 = json.data?.[0]?.b64_json;
if (!b64) { console.error(`✗ no image: ${JSON.stringify(json).slice(0, 280)}`); process.exit(1); }
writeFileSync(OUT_PATH, Buffer.from(b64, "base64"));
console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT_PATH.replace(REPO + "/", "")}`);

embedCover();
