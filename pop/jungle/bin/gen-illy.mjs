#!/usr/bin/env node
// jungle/bin/gen-illy.mjs — generate one colored-pencil illustration per
// jungle track, in the rough-tooth / no-outline / 15°-hatch medium we
// developed for the chillwave covers. TEXT-TO-IMAGE (gpt-image-2,
// generations endpoint) — these are fía's tracks, NOT jeffrey portraits,
// so no identity refs: the cover is a single sunlit sound-system emblem.
//
// Prompt:  pop/jungle/<slug>.illy.txt   (lowercase, fragments — papers voice)
// Output:  pop/jungle/out/<slug>.illy.png  (raw art; cover.mjs composites)
//
// Cached: skips if the illy png exists; --force to regen.
//
// Usage:
//   node pop/jungle/bin/gen-illy.mjs --slug raggasol
//   node pop/jungle/bin/gen-illy.mjs --slug raggasol --force --size 1024x1024

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

const SLUG  = flags.slug || "raggasol";
const SIZE  = flags.size || "1024x1024";
const FORCE = flags.force === true;
const PROMPT_PATH = `${LANE}/${SLUG}.illy.txt`;
const OUT_PATH    = `${LANE}/out/${SLUG}.illy.png`;
// --ref <path>[,<path>...] — identity reference photo(s). When present
// we switch from text→image (generations) to image-conditioned edits,
// the same path the chillwave covers use for jeffrey's likeness.
const REFS = (flags.ref ? String(flags.ref).split(",") : [])
  .map((s) => s.trim()).filter(Boolean);

mkdirSync(`${LANE}/out`, { recursive: true });

// ── Fía source photos (macOS screenshots use U+202F before AM/PM) ─────
const NB = " ";
const GENS = "/Users/jas/Documents/Working Desktop/gens";
const FIA = {
  pose:    `${GENS}/Screenshot 2026-05-17 at 10.39.55${NB}AM.png`, // leaning/wonder
  frontal: `${GENS}/IMG_0933.jpg`,                                  // clean face
  cat:     `${GENS}/Screenshot 2026-05-17 at 9.11.09${NB}AM.png`,   // candid w/ cat
};
// six DISTINCT real Fía photos mined from the @whistlegraph IG platter
// (insightface face-match, face-cropped) — one per section so each illy
// references a different one of her. private (kept out of the repo).
// sec0 is the cleanest frontal → reused as the identity anchor.
const FIA_SEC = [0, 1, 2, 3, 4, 5].map((i) => `${GENS}/.fia-platter/sec${i}.png`);
// gpt-image edits 400s on odd colorspaces — normalize each ref to a
// clean stripped sRGB PNG ≤1500 (same fix as the chillwave covers).
const refClean = new Map();
function normRef(src) {
  if (!existsSync(src)) return null;
  if (refClean.has(src)) return refClean.get(src);
  const tag = src.replace(/[^a-z0-9]/gi, "_").slice(-48);
  const dst = `${LANE}/out/.refs/${tag}.png`;
  mkdirSync(`${LANE}/out/.refs`, { recursive: true });
  if (!existsSync(dst) || FORCE) {
    const r = spawnSync("magick", [src, "-auto-orient", "-colorspace", "sRGB",
      "-depth", "8", "-strip", "-resize", "1500x1500>", dst]);
    if (r.status !== 0) { console.warn(`  ⚠ ref normalize failed: ${src}`); return null; }
  }
  refClean.set(src, dst);
  return dst;
}

// solafiya is Fía's own track — she EVOLVES section by section: a
// different source photo each section, growing more youthful, then
// sweetly metamorphosing into a kitten herself by the finale. Each
// entry: { ref:[primary,…anchor], v:"override appended to base prompt" }.
// THROUGH-LINE for every section:
//  • EXACTLY ONE Fía in the frame — a single continuous person who is
//    herself slowly turning into a cat. NEVER a second / smaller / child
//    / duplicate Fía, and NEVER a separate human-Fía standing beside a
//    cat-Fía. just her, mid-transformation.
//  • the change is SLOW and GRADUAL across the sections — it creeps over
//    her BODY: hands first (furry → paw-like), then forearms, then up
//    over her whole body, posture going feline by degrees.
//  • KEEP HER REAL HUMAN EYES the whole way — exact large warm human
//    eyes, human irises/brows/gaze from the photo, never cat eyes, never
//    slit pupils. the human soul always looks out. wholesome storybook.
const EYES = "ABSOLUTELY KEEP HER REAL HUMAN EYES — the exact large warm human eyes, human irises, human eyebrows and gaze from the reference photo; never cat eyes, never slit pupils, never replaced — the human soul looks out.";
const ONE = "THERE IS EXACTLY ONE FÍA IN THE WHOLE IMAGE — a single continuous figure who is herself transforming. absolutely NO second, smaller, younger, child, or duplicate Fía anywhere; do NOT also draw a separate human Fía next to her. the kittens are ordinary kittens; she is the only person/transforming one.";
const SOLAFIYA_ARC = {
  "intro": { ref: [FIA_SEC[0], FIA.frontal], v:
    `SECTION — intro / ARRIVAL (still basically human): grown Fía stepping out of her white Mercedes-Benz E430, walking toward camera onto the bright beach, iced matcha in hand — NO kittens yet. she is still essentially HUMAN; the only sign of change is the very first faint downy fuzz on the BACKS OF HER HANDS and a barely-there soft fur at her hairline. fully upright, human-bodied. ${ONE} ${EYES}` },
  "roll-in": { ref: [FIA_SEC[1], FIA_SEC[0]], v:
    `SECTION — roll-in / FIRST KITTY (hands going furry): she has crouched and found ONE kitten. the change has crept further: her HANDS are now clearly furry and a little paw-like, soft fur up her forearms, small rounded ears just beginning in her hair, the faintest whiskers — but her body is still mostly her, mostly human, upright. ${ONE} ${EYES}` },
  "drop 1": { ref: [FIA_SEC[2], FIA_SEC[0]], v:
    `SECTION — drop 1 / TWO–THREE (half-furred): 2–3 kittens, energy building, sparks. about HALFWAY now — her hands are soft paws, fur covers her arms and creeps over her shoulders, neck and cheeks, ears grown, a tail started, her crouch a little more feline — but she is still clearly a person, just furring over. ${ONE} ${EYES}` },
  "dub-break": { ref: [FIA_SEC[3], FIA_SEC[0]], v:
    `SECTION — dub-break / A QUIET FEW (mostly cat now): ~4 kittens, a hushed intimate golden moment. she is MOSTLY a cat now — fur over almost all of her, paws, full ears, tail, a low feline curl — only her human face and a little of her shoulders still read as the woman she was. one single transforming figure. ${ONE} ${EYES}` },
  "drop 2": { ref: [FIA_SEC[4], FIA_SEC[0]], v:
    `SECTION — drop 2 / THE GANG GROWS (almost fully a cat): 5–6 kittens swarming, blazing sun, big sparkle. she is ALMOST entirely a cat — full fur, paws, tail, feline body and posture among the swarm — only her unmistakable human eyes and a trace of her features remain. the big cat IS her; there is no separate human her. ${ONE} ${EYES}` },
  "out": { ref: [FIA_SEC[5], FIA_SEC[0]], v:
    `SECTION — out / SHE HAS BECOME A CAT (one cat, with the gang): she has fully become a single dark wavy-furred CAT — her coloring, a tiny soft-pink ruffle collar — resting content among the seven kittens, family complete, serene fading evening gold. she is the ONLY transformed one and the only Fía; you know it's her because her human eyes look back, gentle and knowing. ${ONE} ${EYES}` },
};

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

const apiKey = loadOpenAIKey();
const basePrompt = readFileSync(PROMPT_PATH, "utf8").trim();

const useRefs = REFS.filter((p) => {
  if (existsSync(p)) return true;
  console.warn(`  ⚠ ref missing, dropping: ${p}`);
  return false;
});

// one gpt-image-2 call → outPath. per-file cached; --force regens.
async function generate(promptText, outPath, label, refsOverride) {
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) { console.log(`✓ cached → ${rel}`); return; }
  const refs = (refsOverride && refsOverride.length) ? refsOverride : useRefs;
  const t0 = Date.now();
  let res;
  if (refs.length) {
    console.log(`▸ ${label} · ${SIZE} · gpt-image-2 (edits · ${refs.length} ref)`);
    const fd = new FormData();
    fd.append("model", "gpt-image-2");
    fd.append("prompt", promptText);
    fd.append("size", SIZE);
    fd.append("quality", "high");
    fd.append("n", "1");
    for (const ref of refs) {
      const buf = readFileSync(ref);
      const lo = ref.toLowerCase();
      const ext = lo.endsWith(".png") ? "png" : lo.endsWith(".webp") ? "webp" : "jpeg";
      fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
    }
    res = await fetch("https://api.openai.com/v1/images/edits", {
      method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
    });
  } else {
    console.log(`▸ ${label} · ${SIZE} · gpt-image-2 (text→image)`);
    res = await fetch("https://api.openai.com/v1/images/generations", {
      method: "POST",
      headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
      body: JSON.stringify({ model: "gpt-image-2", prompt: promptText, size: SIZE, quality: "high", n: 1 }),
    });
  }
  if (!res.ok) { console.error(`✗ OpenAI ${res.status} (${label}): ${(await res.text()).slice(0, 400)}`); return; }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) { console.error(`✗ no image (${label}): ${JSON.stringify(json).slice(0, 240)}`); return; }
  writeFileSync(outPath, Buffer.from(b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${rel}`);
}

// Per-section STORY arc — same her, same safe-zone + style. she arrives
// and finds the kittens one by one, the gang growing each section, then
// she's vibing with them all. vary her pose across sections (the multi
// reference photos give the model pose range).
const SECTION_VARIANTS = {
  "intro":     "SECTION — intro / ARRIVAL: she is stepping OUT of the open door of her white Mercedes-Benz E430, mid-stride WALKING TOWARD the camera onto the sunny beach, sunglasses up, matcha in hand — NO kittens yet, just her arriving, bright and excited, anticipation. wide, lots of croppable margin.",
  "roll-in":   "SECTION — roll-in / FIRST KITTY: she has crouched down and found ONE single kitten — she's delighted, reaching toward just the one, the rest of the beach empty. tender discovery, soft building light.",
  "drop 1":    "SECTION — drop 1 / TWO–THREE: another kitty (and another) has come — now 2–3 kittens around her, energy building, brighter sun, sparks beginning, her grinning.",
  "dub-break": "SECTION — dub-break / A QUIET FEW: about FOUR kittens now, a hushed intimate close moment — she's bonding with the little group she's found, dim golden hush, sparks faint, dreamy.",
  "drop 2":    "SECTION — drop 2 / THE GANG GROWS: 5–6 kittens now swarming in, blazing sun, big sparkle and motion, peak joyful chaos as the whole gang assembles.",
  "out":       "SECTION — out / VIBING WITH THEM ALL: the full gang of SEVEN kittens, she is sitting back content among all of them, the family complete, serene fading evening gold, calm and happy. widest calmest frame.",
};
const safeName = (n) => n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");

if (flags.sections === true) {
  const structPath = `${LANE}/out/${SLUG}.struct.json`;
  if (!existsSync(structPath)) {
    console.error(`✗ no ${structPath.replace(REPO + "/", "")} — run render.mjs --slug ${SLUG} first`);
    process.exit(1);
  }
  const sections = JSON.parse(readFileSync(structPath, "utf8")).sections || [];
  const arc = SLUG === "solafiya" ? SOLAFIYA_ARC : null;
  for (let i = 0; i < sections.length; i++) {
    const nm = sections[i].name;
    const out = `${LANE}/out/${SLUG}-sec-${i}-${safeName(nm)}.png`;
    if (arc && arc[nm]) {
      // solafiya: evolving Fía — different source photo + age/species
      // morph per section. normalize each ref to clean sRGB PNG first.
      const secRefs = (arc[nm].ref || []).map(normRef).filter(Boolean);
      await generate(`${basePrompt}\n\n${arc[nm].v}`, out, `${SLUG} §${i} ${nm} (evolving)`, secRefs);
    } else {
      const variant = SECTION_VARIANTS[nm] || `SECTION — ${nm}.`;
      await generate(`${basePrompt}\n\n${variant}`, out, `${SLUG} §${i} ${nm}`);
    }
  }
} else {
  await generate(basePrompt, OUT_PATH, `${SLUG} cover`);
}
