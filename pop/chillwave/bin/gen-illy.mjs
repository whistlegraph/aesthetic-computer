#!/usr/bin/env node
// chillwave/bin/gen-illy.mjs — generate a single album-cover illustration
// for a chillwave track. Calls gpt-image-2 with the jeffrey-platter
// identity refs (SHOOT + SELFIE), same path as recap/bin/jeffrey-photos.mjs.
//
// Output: pop/chillwave/out/<slug>-cover.png (1024x1024 by default)
// Prompt: pop/chillwave/<slug>.illy.txt (lowercase, fragments — papers voice)
//
// Cached: if the cover already exists, --force to regen.
//
// Usage:
//   node bin/gen-illy.mjs --slug wundabeach
//   node bin/gen-illy.mjs --slug wundabeach --force
//   node bin/gen-illy.mjs --slug wundabeach --size 1024x1536

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const POP  = resolve(LANE, "..");
const REPO = resolve(POP, "..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

const SLUG  = flags.slug || "undabeach";
// --portrait → vertical 9:16 set: gpt-image 1024x1536, "-p" filename
// infix, and a tall-composition override appended to every prompt.
const PORTRAIT = flags.portrait === true;
const SIZE  = flags.size || (PORTRAIT ? "1024x1536" : "1024x1024");
const TAG   = PORTRAIT ? "-p" : "";
const FORCE = flags.force === true;
const PROMPT_PATH = `${LANE}/${SLUG}.illy.txt`;
const OUT_PATH    = `${LANE}/out/${SLUG}${TAG}-cover.png`;
const PORTRAIT_NOTE =
  "PORTRAIT OVERRIDE — recompose for a TALL vertical 9:16 frame (not square): " +
  "jeffrey seated low in the lower third, the closed laptop near the bottom, " +
  "and a large expanse of open beach sky filling the upper two-thirds. keep the " +
  "rough-tooth colored-pencil medium, the no-outline volume modeling, the white " +
  "paper scrap + penned whistlegraph butterfly on the lid, and the same light/age " +
  "for this section — only the framing changes to tall vertical.";

mkdirSync(`${LANE}/out`, { recursive: true });

// ── identity refs (mirrors recap/bin/jeffrey-photos.mjs) ─────────────
const SHOOT_DIR  = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const SHOOT_REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
];
const SELFIE_REFS = [
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
  `${ARCHIVE_DIR}/2017-04-10_BStid5yjTHq.jpg`,
];
// Beach + shirtless refs — used for chillwave beach gens specifically.
// Pulled from curated/jeffrey-described.jsonl by filtering on
// shirtless/beach/pool tags + is_jeffrey_confirmed=true + confidence>=0.85.
const BEACH_REFS = [
  `${ARCHIVE_DIR}/2020-07-01_CCFxuLdFeTR.jpg`,   // shirtless outdoor sunny
  `${ARCHIVE_DIR}/2019-07-13_Bz3cjM2lCsb.jpg`,   // shirtless wet-hair beach/pool
  `${ARCHIVE_DIR}/2021-08-18_CSuBwrPFtfN_2.jpg`, // shirtless retro (torso ref)
  `${ARCHIVE_DIR}/2016-11-25_BNP3aLbj917.jpg`,   // beach environment context
];
// The whistlegraph butterfly, given to the model as a REFERENCE so it
// DRAWS it (hand-penned, in the colored-pencil medium) on a white
// paper scrap on the lid — never composited/pasted afterwards.
const BUTTERFLY_REF = `${LANE}/assets/wg-scrap.png`;
const REFS = [...SHOOT_REFS, ...SELFIE_REFS, ...BEACH_REFS, BUTTERFLY_REF].filter((p) => {
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
  throw new Error("OPENAI_API_KEY not set and not found in vault devcontainer.env");
}

if (!existsSync(PROMPT_PATH)) {
  console.error(`✗ no prompt file at ${PROMPT_PATH.replace(REPO + "/", "")}`);
  process.exit(1);
}

const SECTIONS_MODE = flags.sections === true;

// Per-section time-arc — a SUBTLE vacation day easing from bright
// midday into a soft evening. Gentle, natural, restful; no tech, no
// drama — only the light and warmth slowly shifting. Keys MUST match
// struct.sections names + ordering so preview-score.mjs's safeName()
// resolves the files.
const SECTION_ORDER = ["tide-in", "drift 1", "swell", "drift 2", "tide-out"];
const SECTION_VARIANTS = {
  "tide-in":
    "SECTION OVERRIDE — tide-in (bright midday): clear, easy overhead daylight. the most lit panel, natural and true-colored — soft blue sky, bright but believable, no fantasy glow. empty calm sea, gentle surf. jeffrey upright and relaxed, hand pressed into the wet sand. AGE: jeffrey here is his youngest, about 30 — matches the reference photos, smooth-faced, brown hair. LAPTOP: the chartreuse MacBook Neo is nearly new — clean and smooth, the white paper scrap crisp and bright, the penned whistlegraph butterfly fresh and dark. the most resolved, fully-modeled panel.",
  "drift 1":
    "SECTION OVERRIDE — drift 1 (early afternoon): the sun off overhead, light flattening and warming a touch, a faint summer haze softening the horizon. colour eases back just slightly from midday. jeffrey gazing out at the calm water, content, a small smile. AGE: jeffrey is now about 35 — a touch more weathered, the first faint lines, brown hair. LAPTOP: a little wear now — light surface scuffs, a bit of beach grit, the paper scrap slightly curling at one corner, the butterfly ink still clear. still bright and peaceful.",
  "swell":
    "SECTION OVERRIDE — swell (warm mid-afternoon): the warmest, most golden panel — soft amber light on jeffrey and the chartreuse lid, gentle long shadows on the sand. colour stays natural and a little dusty, never neon. the sea calm and glinting. AGE: jeffrey is now about 40 — leaner and more rugged, sun-worn, a few grey hairs starting, handsome. LAPTOP: visibly used — fine scratches across the lid, dirt and sand worked into the seams, the white scrap yellowing and dog-eared, the butterfly ink fading a touch. easy, drowsy, content.",
  "drift 2":
    "SECTION OVERRIDE — drift 2 (late afternoon, cooling): the sun lower, light going soft rose and pale gold, the air a little hazy and cooler, colour gently muting. long quiet shadows. jeffrey leaning slightly back, calmer, dreamy. AGE: jeffrey is now about 45 — noticeably greying hair, weathered and lean, a tougher more hardcore look, still handsome. LAPTOP: well-battered — deep scratches and dings, scuffed worn corners, grime and salt haze on the chartreuse, the paper scrap grubby and torn and re-taped, the butterfly faded and partly rubbed away. restful and warm, the day winding down.",
  "tide-out":
    "SECTION OVERRIDE — tide-out (early evening, after the sun is low): soft dusk — gentle lavender, peach and grey-blue, the light tender and dim but NOT dark, NOT night. the calm sea going pewter, a single faint early star. jeffrey serene, still, hand lifted from the sand, content. AGE: jeffrey is now about 50 — distinctly grey hair, deeply weathered and rugged, hardcore and intense but still handsome, a lifetime on the beach in his face. LAPTOP: an old battlescarred machine — deep gouges, cracked worn edges, sun-bleached faded chartreuse, caked grime, the paper scrap brown and frayed and barely hanging on with old tape, the whistlegraph butterfly almost worn away to a ghost. the quietest, softest, most dissolved panel — the fewest marks, the figure easing into the dusk paper.",
};
function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}

const apiKey = loadOpenAIKey();
const basePrompt = readFileSync(PROMPT_PATH, "utf8").trim();

// One gpt-image-2 edit call → write outPath. Per-file cached; --force regens.
async function generate(promptText, outPath, label) {
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) {
    console.log(`✓ cached → ${rel}`);
    return;
  }
  console.log(`▸ ${label} · ${SIZE} · ${REFS.length} refs`);
  const t0 = Date.now();
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", promptText);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const lower = ref.toLowerCase();
    const ext = lower.endsWith(".png") ? "png"
              : lower.endsWith(".webp") ? "webp"
              : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}` },
    body: fd,
  });
  if (!res.ok) {
    const err = await res.text();
    console.error(`✗ OpenAI ${res.status} (${label}): ${err.slice(0, 600)}`);
    return;
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) {
    console.error(`✗ no image (${label}): ${JSON.stringify(json).slice(0, 280)}`);
    return;
  }
  writeFileSync(outPath, Buffer.from(b64, "base64"));
  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  const u = json.usage || {};
  const tok = u.input_tokens ? ` · tok in=${u.input_tokens} out=${u.output_tokens}` : "";
  console.log(`✓ ${elapsed}s${tok} → ${rel}`);
}

const portraitTail = PORTRAIT ? `\n\n${PORTRAIT_NOTE}` : "";

// Main hero cover (its own composition via the base prompt alone).
await generate(basePrompt + portraitTail, OUT_PATH, `${SLUG}${TAG} cover`);

// Per-section illys (sequential — keeps API pressure low, lets each be
// individually cached/--force'd). The butterfly is drawn natively by
// the model from BUTTERFLY_REF — no post-pass composite.
if (SECTIONS_MODE) {
  for (let i = 0; i < SECTION_ORDER.length; i++) {
    const name = SECTION_ORDER[i];
    const variant = SECTION_VARIANTS[name];
    const out = `${LANE}/out/${SLUG}${TAG}-sec-${i}-${safeName(name)}.png`;
    await generate(
      `${basePrompt}\n\n${variant}${portraitTail}`,
      out,
      `${SLUG}${TAG} §${i} ${name}`,
    );
  }
}
