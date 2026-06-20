#!/usr/bin/env node
// gen-illy.mjs — generate the boombaboom album cover as a needle-felted wool
// diorama (momabobasheep / restless-egg felt aesthetic), grounded in jeffrey's
// real reference photos AND the source whistlegraph frames via gpt-image-2
// /v1/images/edits.
//
// The prompt lives in pop/boombaboom/boombaboom.illy.txt (square album cover).
//
// Usage:
//   node gen-illy.mjs                 # gen 2 variants if not present
//   node gen-illy.mjs --n 3           # gen N variants
//   node gen-illy.mjs --force         # regen
//   node gen-illy.mjs --no-jeffrey    # moderation fallback: drop jeffrey refs

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/illy`;
mkdirSync(OUT, { recursive: true });

const argv = process.argv.slice(2);
const FORCE = argv.includes("--force");
const NO_JEFFREY = argv.includes("--no-jeffrey");
const nIdx = argv.indexOf("--n");
const N = nIdx >= 0 ? parseInt(argv[nIdx + 1], 10) : 2;
const SIZE = "1024x1024";

const PROMPT = readFileSync(`${LANE}/boombaboom.illy.txt`, "utf8").trim();

const SHOOT_DIR = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;

// jeffrey identity refs (felt doll grounding) + the source whistlegraph frames
// so the model copies the real circle-dot grid / V / zigzag geometry.
const JEFFREY_REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
];
const SCENE_REFS = [
  `${OUT}/frame-12s.jpg`,
  `${OUT}/frame-28s.jpg`,
];

const REFS = [...(NO_JEFFREY ? [] : JEFFREY_REFS), ...SCENE_REFS].filter(
  (p) => existsSync(p) || (console.warn(`  ⚠ ref missing: ${p}`), false),
);

function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(vault, "utf8").split("\n")) {
    if (line.startsWith("OPENAI_API_KEY=")) {
      return line
        .slice("OPENAI_API_KEY=".length)
        .trim()
        .replace(/^['"]|['"]$/g, "");
    }
  }
  throw new Error("OPENAI_API_KEY not found");
}

const apiKey = loadKey();

async function gen(idx) {
  const tag = NO_JEFFREY ? `nojeff-${idx}` : `v${idx}`;
  const out = `${OUT}/boombaboom-cover-${tag}.png`;
  if (existsSync(out) && !FORCE) {
    console.log(`✓ cached ${out} (use --force)`);
    return out;
  }
  console.log(`▸ boombaboom-${tag} · ${SIZE} · ${REFS.length} refs`);
  const t0 = Date.now();
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", PROMPT);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  let res, lastErr;
  for (let attempt = 1; attempt <= 4; attempt++) {
    try {
      res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST",
        headers: { Authorization: `Bearer ${apiKey}` },
        body: fd,
      });
      break;
    } catch (e) {
      lastErr = e;
      console.warn(`  ⚠ attempt ${attempt} failed (${e?.cause?.code || e.message}); retrying…`);
    }
  }
  if (!res) {
    console.error(`✗ network failed after retries: ${lastErr?.message}`);
    return null;
  }
  if (!res.ok) {
    const body = await res.text();
    console.error(`✗ OpenAI ${res.status}: ${body.slice(0, 600)}`);
    if (body.includes("moderation") || res.status === 400) console.error("  → MODERATION/REQUEST issue");
    return null;
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) {
    console.error(`✗ no image: ${JSON.stringify(json).slice(0, 280)}`);
    return null;
  }
  writeFileSync(out, Buffer.from(b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${out}`);
  return out;
}

const results = [];
for (let i = 1; i <= N; i++) {
  const r = await gen(i);
  if (r) results.push(r);
}
console.log(`\nDone. ${results.length} image(s):`);
results.forEach((r) => console.log(`  ${r}`));
