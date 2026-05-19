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
    "SECTION OVERRIDE — tide-in (bright midday): clear easy overhead daylight, natural true colour, soft blue sky, calm empty sea. jeffrey upright and relaxed, hand in the wet sand. AGE: about 30 — smooth-faced, brown hair, lean and fit, a quiet natural soldier's strength. NO tattoos, NO jewelry. TRUNKS: plain SOLID navy swim trunks, no pattern at all. OBJECT (OVERRIDES THE BASE PROMPT AND THE BUTTERFLY REFERENCE FOR THIS PANEL ONLY): a BRAND-NEW, factory-fresh, perfectly clean chartreuse MacBook Neo — on the smooth lid ONLY the plain standard small Apple logo, nothing else; ABSOLUTELY NO paper, NO scrap, NO tape, NO butterfly, NO ink — ignore the butterfly reference here. FIGURE: VERY far down the empty beach, a lone WOMAN — a tiny faraway ambiguous silhouette, barely there, never identifiable. the most resolved, fully-modeled panel.",
  "drift 1":
    "SECTION OVERRIDE — drift 1 (early afternoon): sun off overhead, light flattening and warming, faint summer haze. jeffrey gazing at the calm water, content. AGE: about 35 — a touch weathered, browned and stronger, an aging-into-strength soldier's build with a faint MILITARY bearing now. NO tattoos, NO jewelry. TRUNKS: plain SOLID forest-green trunks (a quiet colour shift from navy) — still NO pattern. OBJECT: still a chartreuse MacBook Neo but slightly OLDER-looking and lightly scuffed — a small torn WHITE PAPER SCRAP now stuck over the Apple logo with the hand-penned whistlegraph BUTTERFLY on it. it stays a laptop; NO book, NO pen. FIGURE: the woman is still FAR off, a soft distant shadow / silhouette, a little older in her walk — never clearly identifiable. bright and peaceful.",
  "swell":
    "SECTION OVERRIDE — swell (warm mid-afternoon): warmest, most golden panel — soft amber light, long gentle shadows, calm glinting sea, natural and dusty, never neon. AGE: about 40 — leaner, sun-worn, a few grey hairs, powerfully built like a veteran soldier; a clearly MILITARY hardness in posture, but he KEEPS his usual MEDIUM-LENGTH tousled brown hair — NOT short, NOT buzzed, NOT a crew cut, NOT a military haircut; same windblown medium hair as the other panels, just lightly greying. NO tattoos, NO jewelry. TRUNKS: plain SOLID faded brick-red trunks — solid colour only, NO pattern. OBJECT: the same laptop, now visibly DATED and worn — a clunkier older model, yellowed and scratched, the white scrap + butterfly grubbier. still a laptop; NO book, NO pen, NO gilding. FIGURE: the woman is still FAR off, a distant shadow — and now a small CHILD walks beside her holding her hand, also a tiny far shadow. both ambiguous, never identifiable. easy, drowsy.",
  "drift 2":
    "SECTION OVERRIDE — drift 2 (late afternoon, cooling): sun lower, light soft rose and pale gold, hazy and cooler, colour gently muting. jeffrey leaning slightly back, calmer. AGE: about 45 — greying, deeply weathered, lean and immensely strong; distinctly MILITARY now — a hardened veteran's bearing and discipline. NO tattoos, NO jewelry. TRUNKS: plain SOLID khaki / sand trunks (military-toned) — solid colour, NO pattern. OBJECT: an old DATED, battered laptop now — thick, yellowed, grimed, an outdated clunky model; the white scrap + butterfly faded and half rubbed away. still a laptop; NO book, NO pen. FIGURE: the woman + child are still FAR off, both distant soft silhouettes, visibly older, still impossible to identify. restful, the day winding down.",
  "tide-out":
    "SECTION OVERRIDE — tide-out (NIGHT): full night now — a deep STARRY SKY, dark sea, the most dissolved panel. jeffrey sits by a small cool blue-white LED CAMPFIRE that lights him from below; far up in the night sky a single MISSILE streaks with a thin contrail. AGE: about 50 — distinctly grey, deeply weathered, an unbreakable old MILITARY veteran who aged entirely into strength, hard and calm, lit by the cold LED fire. NO tattoos, NO jewelry. TRUNKS: plain SOLID deep-teal trunks — solid colour, NO pattern. OBJECT: an ancient, very dated battlescarred laptop — old, yellowed, cracked, clunky; the whistlegraph-butterfly scrap brown and frayed. still a laptop; NO book, NO pen. FIGURE: the woman + child are STILL present but at their MOST DISTANT and faint — tiny soft silhouettes far off in the starlit dark, very old now, almost dissolving but unmistakably still there. fewest marks, jeffrey and the LED fire easing into the night.",
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
  // --only <name|idx>[,…] regenerates just those panels (others untouched).
  const onlySet = typeof flags.only === "string"
    ? new Set(flags.only.split(",").map((x) => x.trim()))
    : null;
  for (let i = 0; i < SECTION_ORDER.length; i++) {
    const name = SECTION_ORDER[i];
    if (onlySet && !onlySet.has(name) && !onlySet.has(String(i))) continue;
    const variant = SECTION_VARIANTS[name];
    const out = `${LANE}/out/${SLUG}${TAG}-sec-${i}-${safeName(name)}.png`;
    await generate(
      `${basePrompt}\n\n${variant}${portraitTail}`,
      out,
      `${SLUG}${TAG} §${i} ${name}`,
    );
  }
}
