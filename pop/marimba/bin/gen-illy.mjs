#!/usr/bin/env node
// marimba/bin/gen-illy.mjs — generate the marimbaba album cover via
// gpt-image-2, then embed it into the rendered mp3 as ID3 art.
//
// Same identity-ref path as pop/chillwave/bin/gen-illy.mjs (jeffrey
// SHOOT + SELFIE refs + the whistlegraph-butterfly scrap), so the
// cover sits in the same hand-drawn singles series.
//
// Prompt: pop/marimba/marimbaba.illy.txt (lowercase fragments — papers voice)
// Output: pop/marimba/out/marimbaba-cover.png
// Embed:  pop/marimba/out/marimbaba.mp3  (ID3v2 attached picture)
//
// Usage:
//   node pop/marimba/bin/gen-illy.mjs            # cached if cover exists
//   node pop/marimba/bin/gen-illy.mjs --force    # regenerate
//   node pop/marimba/bin/gen-illy.mjs --embed-only   # skip gen, just embed

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
// --norefs: skip the jeffrey identity refs and generate from the prompt alone
// (images/generations instead of images/edits). For object-only covers — e.g.
// a cover that is ONLY plastic butterflies, no figure — where the portrait refs
// would wrongly bias toward a person.
const NOREFS = flags.norefs === true;
const SIZE = "1024x1024";

// --prompt / --cover / --mp3 let variation tracks reuse this script;
// defaults target the marimbaba master.
const _af = (k, d) => { const i = process.argv.indexOf(k); return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : d; };
const PROMPT_PATH = resolve(process.cwd(), _af("--prompt", `${LANE}/marimbaba.illy.txt`));
const OUT_PATH    = resolve(process.cwd(), _af("--cover",  `${LANE}/out/marimbaba-cover.png`));
const MP3_PATH    = resolve(process.cwd(), _af("--mp3",    `${LANE}/out/marimbaba.mp3`));
mkdirSync(`${LANE}/out`, { recursive: true });

// ── identity refs (mirrors chillwave/bin/gen-illy.mjs) ───────────────
const SHOOT_DIR   = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
  // the whistlegraph butterfly scrap — model DRAWS it, never composites
  `${REPO}/pop/chillwave/assets/wg-scrap.png`,
].filter((p) => {
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
  throw new Error("OPENAI_API_KEY not set and not found in vault");
}

// ── embed the cover into the mp3 as ID3v2 attached picture ───────────
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
console.log(`▸ cover · ${SIZE} · ${NOREFS ? "no refs (generations)" : `${REFS.length} refs (edits)`}`);
const t0 = Date.now();

let res;
if (NOREFS) {
  // prompt-only generation — no identity refs to leak a figure in
  res = await fetch("https://api.openai.com/v1/images/generations", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
    body: JSON.stringify({ model: "gpt-image-2", prompt, size: SIZE, quality: "high", n: 1 }),
  });
} else {
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", prompt);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png"
              : ref.toLowerCase().endsWith(".webp") ? "webp" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}` },
    body: fd,
  });
}
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
writeFileSync(OUT_PATH, Buffer.from(b64, "base64"));
console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT_PATH.replace(REPO + "/", "")}`);

embedCover();
