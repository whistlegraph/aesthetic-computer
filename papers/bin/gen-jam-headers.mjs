#!/usr/bin/env node
// gen-jam-headers.mjs — generate N variations of the "field jam" keymaps header
// using gpt-image-1 IMAGE EDITS with REAL reference images, so Jeffrey, the
// whistlegraph butterfly, and the notepat.com interface are accurate (not
// invented). Base prompt: papers/figures/header-prompt-jam-field.txt, plus a
// hard correction block fixing the laptop-screen issues (interface legible on
// FRONT-facing lids; the BACK of a lid is a logo-only shell).
//
//   node papers/bin/gen-jam-headers.mjs           # 6 variations
//   node papers/bin/gen-jam-headers.mjs 4
//
// Writes: papers/figures/jam-header-v1.png … vN.png  (1536x1024)

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, join, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS = resolve(HERE, "..");
const REPO = resolve(PAPERS, "..");
const FIG = join(PAPERS, "figures");
const N = Math.max(1, Math.min(8, parseInt(process.argv[2] || "6", 10)));

// Real reference images (paths relative to repo root).
const REFS = [
  // Jeffrey's actual face — real studio shoot photos (identity grounding, the
  // way marketing/lib/jeffrey-refs.mjs locks likeness for gpt-image-2 edits).
  "papers/figures/refs/jeffrey-01-2k.jpg",
  "papers/figures/refs/jeffery-av--04-2k.jpg",
  "papers/figures/refs/jeffery-av--07-2k.jpg",
  // Canonical research-platter colored-pencil portrait of Jeffrey seated, WITH
  // the faceless single-stroke whistlegraph butterfly emblem (style + emblem).
  "papers/arxiv-whistlegraph/figures/cover.png",
  // The actual notepat.com interface, upright: colored key grid + waveform
  // tabs + FX sliders.
  "marketing/campaigns/serpentine-video/01_title/refs/notepat-screen.png",
].map((p) => join(REPO, p));

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

const BASE = readFileSync(join(FIG, "header-prompt-jam-field.txt"), "utf8").trim();

const REF_BLOCK = `

USING THE REFERENCE IMAGES (critical — these are real, not decoration):
- The PHOTOGRAPH is the real JEFFREY: match THIS man's actual face, hair, brow, nose, and build precisely for the single Jeffrey figure in the scene (a contemporary man in his late thirties, medium-length light-brown hair, warm eyes), rendered in the warm colored-pencil idiom. He appears ONCE, NOT centered, at peer eye-level.
- The colored-pencil PORTRAIT reference shows Jeffrey's seated pose and his WHISTLEGRAPH BUTTERFLY EMBLEM (the small blue single-stroke butterfly floating beside him) — draw that emblem EXACTLY on the back of Jeffrey's chartreuse laptop lid: four soft petal-lobes around a pinched waist, a slim body, two curving antennae, pure single-color outline, NO face, NO fill, NO veins.
- The SCREENSHOT reference is the ACTUAL notepat.com interface. EVERY front-facing laptop screen must show THIS interface, hand-rendered in colored pencil: the grid of soft-colored note-pad keys (each a flat pastel square with a tiny letter + note label), the slim toolbar of waveform tabs (sine / tri / saw / square / pno / harp / whist / sample), and the thin FX sliders along the top. Each laptop lights up DIFFERENT keys. Do NOT invent a different music app or a generic piano.`;

const FIX = `

CRITICAL LAPTOP-SCREEN RULES — these OVERRIDE anything that conflicts, and are the single most important thing to get right:
1. A glowing screen showing the notepat interface appears ONLY on the INNER face of an open lid angled TOWARD the viewer. Render every front-facing screen CLEARLY and LEGIBLY as the notepat interface from the screenshot reference.
2. The OUTER BACK of a laptop lid — the shell facing the viewer when a laptop is seen from behind — shows ONLY the solid matte shell color and a SINGLE small brand logo (or, on Jeffrey's chartreuse Neo, the whistlegraph butterfly emblem). NEVER render a screen, app interface, keys, visualizer, pixels, glyphs, text, glow, or reflection on the BACK of a lid. The back of a laptop is an opaque solid shell. This is the most common mistake — do not make it.
3. No laptop screen shows the meadow, sky, faces, or any reflection of the scene. No floating UI detached from a screen.
4. Make at least FOUR or FIVE laptops face the viewer clearly enough to read the notepat interface; the rest are seen from behind as clean LOGO-ONLY shells.
5. Realistic, consistent laptop bezels, hinges, and proportions; no impossible double-screens, no screen on both sides of a lid.`;

const PROMPT = BASE + REF_BLOCK + FIX;

const mimeOf = (p) => (p.toLowerCase().endsWith(".jpg") || p.toLowerCase().endsWith(".jpeg") ? "image/jpeg" : "image/png");

async function gen(i, key) {
  const t0 = Date.now();
  const form = new FormData();
  form.append("model", "gpt-image-2");
  form.append("prompt", PROMPT);
  form.append("size", "1536x1024");
  form.append("quality", "high");
  form.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    form.append("image[]", new Blob([buf], { type: mimeOf(ref) }), basename(ref));
  }
  // gpt-image-2 can drop the socket mid-upload of a multi-ref bundle and the
  // server-side render is slow — raise the per-request timeout to 10 minutes.
  const ac = new AbortController();
  const timer = setTimeout(() => ac.abort(), 10 * 60 * 1000);
  let res;
  try {
    res = await fetch("https://api.openai.com/v1/images/edits", {
      method: "POST",
      headers: { Authorization: `Bearer ${key}` },
      body: form,
      signal: ac.signal,
    });
  } finally {
    clearTimeout(timer);
  }
  if (!res.ok) throw new Error(`v${i}: OpenAI ${res.status}: ${(await res.text()).slice(0, 300)}`);
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) throw new Error(`v${i}: no image returned`);
  const out = join(FIG, `jam-header-v${i}.png`);
  writeFileSync(out, Buffer.from(b64, "base64"));
  console.log(`  ✓ v${i} (${((Date.now() - t0) / 1000).toFixed(1)}s) -> ${out}`);
  return out;
}

async function genWithRetry(i, key, tries = 5) {
  let last;
  for (let t = 1; t <= tries; t++) {
    try { return await gen(i, key); }
    catch (e) {
      last = e;
      const cause = e?.cause?.code || e?.code || e?.name || "";
      console.error(`  · v${i} try ${t} failed: ${e.message}${cause ? ` (${cause})` : ""}`);
      if (t < tries) {
        const backoff = Math.min(60, 3 * 2 ** (t - 1)); // 3,6,12,24,48s
        await new Promise((r) => setTimeout(r, backoff * 1000));
      }
    }
  }
  throw last;
}

for (const ref of REFS) if (!existsSync(ref)) throw new Error(`missing reference: ${ref}`);
const key = loadOpenAIKey();
console.log(`▸ ${N} jam-header variations via gpt-image-2 EDITS, refs:`);
REFS.forEach((r) => console.log(`    - ${r}`));
const START = Math.max(1, parseInt(process.argv[3] || "1", 10));
const ok = [];
for (let k = START; k < START + N; k++) {
  try { ok.push(await genWithRetry(k, key)); }
  catch (e) { console.error(`  ✗ ${e.message}`); }
}
console.log(`done: ${ok.length}/${N} succeeded`);
if (ok.length) console.log(ok.join("\n"));
