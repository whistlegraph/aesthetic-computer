#!/usr/bin/env node
// marimba/bin/gen-sections.mjs — generate the marimbaba STORYLINE panel
// set: a portrait (9:16) cover + 10 per-beat illustrations tracing
// jeffrey's late-night "computer help call" with Bill Gates.
//
// Concept (jas, 2026-05-22):
//   the marimbaba lullaby visualized as one quiet story — jeffrey
//   ARRIVES at Bill Gates's hushed late-night study, says hi, and walks
//   him through a basic computer thing on the IBM Model M. TEN beats,
//   two per .np section (hush / twinkle / wow / baba / sleep), so the
//   visualizer changes faster. the final 'sleep2' panel lands exactly
//   on the locked marimbaba album cover.
//
// Medium + identity are inherited verbatim from marimbaba.illy.txt
// (colored-pencil + gouache on warm cream paper, the two men's exact
// faces / outfits / Sailor pens, the Model M, NO screen of any kind).
// Each SECTION_VARIANT only re-stages the composition + the story beat;
// PORTRAIT_NOTE widens the album-cover crop into the tall shot the
// 1080x1920 insta-story needs.
//
// Output: pop/marimba/out/marimbaba-p-cover.png            (portrait hero)
//         pop/marimba/out/marimbaba-p-sec-<i>-<name>.png    (10 panels)
//
// Usage:
//   node pop/marimba/bin/gen-sections.mjs                # cached
//   node pop/marimba/bin/gen-sections.mjs --force        # regen all
//   node pop/marimba/bin/gen-sections.mjs --only wow2    # one panel

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import * as progress from "../../lib/render-progress.mjs";

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
const FORCE = flags.force === true;
const SIZE  = "1024x1536";                  // portrait 9:16-ish for the story cut
mkdirSync(`${LANE}/out`, { recursive: true });

// ── identity refs (mirrors marimba/bin/gen-illy.mjs) ─────────────────
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
  throw new Error("OPENAI_API_KEY not set and not found in vault devcontainer.env");
}

// ── PORTRAIT recomposition — widen the album-cover crop ──────────────
const PORTRAIT_NOTE =
`PORTRAIT OVERRIDE — recompose for a TALL vertical 9:16 frame (NOT the square album-cover crop). PULL THE CAMERA BACK and slightly UP — a wide, roomy shot: the WHOLE setting of this beat is visible around the figures, the figures sitting/standing comfortably WITHIN it, never cropped tight. keep the colored-pencil + gouache medium on warm cream paper, the confident hatching + striping, diegetic light only (warm desk-lamp / window light), and the calm somber late-night lullaby mood — only the framing widens.`;

// ── shared scene law (distilled from marimbaba.illy.txt) ─────────────
const MEDIUM =
`A colored-pencil + gouache drawing on warm cream paper. confident hatching and striping build tone — tapered pencil edges, visible paper grain, optical mixing in the knit textures. diegetic light only — a warm desk-lamp glow, no overlay shine. NOT a photograph, NOT cinematic, NOT neon. a hushed late-night world — but the two men's EXPRESSIONS and MOOD genuinely SHIFT from beat to beat as the little story moves: read the EMOTION called out in this beat's description and let their faces and posture show it clearly and distinctly. peers at one shared eye-line, neither centered as a hero; neither looks at the camera.`;

const JEFFREY =
`JEFFREY — about 30, recognizable from the jeffrey reference photographs: tousled medium-length brown hair, light beard, a thin coral-red glasses-cord at his neck. he wears a light-blue long-sleeve BUTTON-DOWN SHIRT with a faint vertical pinstripe and a real patch chest pocket; his yellow Sailor Pro Gear fountain pen — short cigar-shaped barrel, flat-topped cap, polished metal clip, gold trim ring — is CLIPPED INTO that chest pocket, clip hooked over the pocket hem, barrel tucked inside so only the capped top shows. NOT a sweater. he is the patient helper — calm, quietly glad to assist, mellow and a little drowsy.`;

const GATES =
`BILL GATES — recognizably him: older, grey hair, soft rectangular glasses. he wears a muted sage-green crewneck sweater (a clearly different colour from jeffrey's blue shirt). his deep-red fountain pen — same cigar-shaped Sailor barrel and flat-topped cap as jeffrey's — hangs from the sweater's ribbed neckline, its polished metal clip HOOKED OVER AND GRIPPING the collar edge, visibly doing its job. he is the one being helped — pensive, still, faintly melancholy, attentive.`;

const MODEL_M =
`THE COMPUTER — the only device in the story is an IBM Model M keyboard, the classic buckling-spring board — a pebble-grey / greige gently-curved case, slightly sculpted off-white keycaps, a small oval corner badge rendered as an indistinct emblem (NOT a readable wordmark). it sits on the study desk in the lamp light. ABSOLUTELY NO screen, monitor, laptop, phone or any display anywhere — the help happens entirely at this keyboard, with hands and gestures.`;

const PALETTE =
`PALETTE — warm cream paper ground; light-blue cotton shirt and sage-green knit; soft brown hair and warm grey; buttery yellow on jeffrey's pen, deep red on Bill's; one coral pop on the glasses-cord; pebble-grey on the Model M; the gentle amber pool of the desk lamp against cool blue night dark. hand-drawn, hatched, print-tech-aware.`;

const AVOID =
`AVOID — any photographic / cinematic / neon look (this is a colored-pencil + gouache drawing); ANY screen, monitor, laptop, phone or display; any readable brand wordmark or logo; modern flat tech-illustration style; hero-pose centering; either figure looking at or acknowledging the camera. no text anywhere in the image.`;

// ── per-beat story — the late-night help call, 10 beats ──────────────
// Keys + ordering MUST match marimbaba.struct.json sections[] so
// preview-score.mjs's safeName() resolves out/marimbaba-p-sec-<i>-<name>.png.
const SECTION_ORDER = [
  "hush1", "hush2", "twinkle1", "twinkle2", "wow1",
  "wow2", "baba1", "baba2", "sleep1", "sleep2",
];
const SECTION_VARIANTS = {
  hush1:
`BEAT — HUSH 1 (jeffrey approaches) · EMOTION: quiet ANTICIPATION, a little wistful — jeffrey calm and resolved, faintly nervous-hopeful about the visit ahead. the opening establishing panel, OUTSIDE at night. jeffrey walks up a quiet garden path toward Bill Gates's house, hands in pockets, his face thoughtful and expectant, a small private half-smile. one ground-floor window glows warm amber — the study, where a small lamplit silhouette of Gates can just be made out at a desk inside. dark trees, a cool blue night, a sliver of moon. jeffrey small in the frame, arriving.`,
  hush2:
`BEAT — HUSH 2 (the doorway hello) · EMOTION: a warm, shy GREETING — jeffrey friendly and a touch bashful, Bill Gates SURPRISED and quietly PLEASED, lonely-glad that someone came. jeffrey has stepped into the doorway of the hushed late-night STUDY, raising one hand in a small hello, an open friendly look. across the lamplit room Bill Gates twists round in his chair, eyebrows lifted in mild surprise, the start of a relieved smile breaking on his face. the desk lamp the one warm pool of light; bookshelves, a worn armchair, a dark night window.`,
  twinkle1:
`BEAT — TWINKLE 1 (crossing the room) · EMOTION: RELIEF and brightening — Bill Gates visibly glad, his worry easing, brightening up; jeffrey easy, open and friendly. jeffrey is mid-step across the study, moving toward the desk; Bill Gates turns fully in his chair to welcome him, both hands lifting a little, a clear warm smile now, glad not to be stuck alone with the thing. the room warm and dim, the Model M waiting on the lamplit desk.`,
  twinkle2:
`BEAT — TWINKLE 2 (settles in) · EMOTION: COMPANIONABLE and cozy — both men settling, soft contentment, the easy comfort of company. jeffrey has pulled a worn chair up beside Bill Gates at the desk and settles into it; the two share a relaxed, contented closed-mouth look, shoulders loose, at ease together. the help call begins, both lit by the desk lamp, the Model M between their hands. peers, side by side.`,
  wow1:
`BEAT — WOW 1 (the lesson begins) · EMOTION: jeffrey EARNEST and focused; Bill Gates UNCERTAIN — a little lost, brow furrowed, faintly anxious, out of his depth. jeffrey leans in and POINTS at the Model M, explaining a first step, concentrated and kind. Bill Gates squints at jeffrey's finger, mouth tight, clearly not following yet, a worried crease between his brows. the lamp light close on their hands and the keyboard.`,
  wow2:
`BEAT — WOW 2 (it clicks) · EMOTION: soft DELIGHT and WONDER — Bill Gates lit up with a small amazed joy, eyes wide and bright; jeffrey quietly PROUD and glad. the breakthrough: Bill Gates leans in over the Model M, his face opening into delighted surprise, eyebrows up, a real smile — amazed it was that simple. beside him jeffrey beams a quiet glad half-smile, pleased to have helped. the tender peak, both bright in the warm lamp light.`,
  baba1:
`BEAT — BABA 1 (Gates tries it) · EMOTION: anxious CONCENTRATION — Bill Gates tense and intent, tongue-of-effort focus, a little fretful; jeffrey patient, faintly amused, holding back a fond smile. Bill Gates hunches over the Model M and tries it himself, one finger out and hovering, brow hard-furrowed, jaw set in concentration. jeffrey sits close beside him, hand resting near, an indulgent patient warmth on his face, almost laughing kindly.`,
  baba2:
`BEAT — BABA 2 (hands on, together) · EMOTION: absorbed, determined TEAMWORK — both fully engaged, warm and busy, a shared purposeful energy. the busiest, most active beat — both pairs of hands on the Model M at once, fingers mid-keystroke. jeffrey's hand guides near Bill Gates's; both faces are lively and intent, leaning in together, a gentle determined back-and-forth, working as a team in the lamp light.`,
  sleep1:
`BEAT — SLEEP 1 (it worked) · EMOTION: shared RELIEF and quiet TRIUMPH — both men beaming, a real warm exchanged smile, a small glow of accomplishment. the help has worked: both sit back a touch from the Model M, hands easing off the keys, turning to each other with open relieved smiles, a soft triumphant look passing between them. the lamp warm, the night still.`,
  sleep2:
`BEAT — SLEEP 2 (the drowsy close) · EMOTION: deep CALM and drowsy contentment — both somber-content, half-asleep, wistful, the night winding down. the resolution, landing EXACTLY on the marimbaba album cover: a tight, warm, album-cover-close head-and-shoulders framing of the two men side by side, heads almost touching the top edge, both quiet and drowsy, eyes half-lowered and heavy, lips closed, a touch wistful — two people who got it working, now just typing softly on into the night. their hands rest on the Model M along the bottom of the frame. (the intimate cover crop, not the wide room shot.)`,
};

function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}
function build(sectionBeat, tightCrop) {
  const portrait = tightCrop ? "" : `\n\n${PORTRAIT_NOTE}`;
  return [MEDIUM, JEFFREY, GATES, MODEL_M, sectionBeat, PALETTE, AVOID].join("\n\n")
    + portrait + "\n";
}

const apiKey = loadOpenAIKey();
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function buildForm(promptText) {
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", promptText);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of REFS) {
    const buf = readFileSync(ref);
    const lower = ref.toLowerCase();
    const ext = lower.endsWith(".png") ? "png" : lower.endsWith(".webp") ? "webp" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  return fd;
}

// One gpt-image-2 edit call → write outPath. Per-file cached; --force
// regens. Transient 429/5xx + network blips retried; hard 4xx fails
// fast. Never throws — a failed panel logs and the batch continues.
async function generate(promptText, outPath, label) {
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) {
    console.log(`✓ cached → ${rel}`);
    return;
  }
  console.log(`▸ ${label} · ${SIZE} · ${REFS.length} refs`);
  const MAX_TRIES = 4;
  for (let attempt = 1; attempt <= MAX_TRIES; attempt++) {
    const t0 = Date.now();
    try {
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST",
        headers: { Authorization: `Bearer ${apiKey}` },
        body: buildForm(promptText),
      });
      if (!res.ok) {
        const err = await res.text();
        const transient = res.status === 429 || res.status >= 500;
        if (transient && attempt < MAX_TRIES) {
          const wait = 4000 * attempt;
          console.warn(`  ⚠ OpenAI ${res.status} (${label}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
          await sleep(wait);
          continue;
        }
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
      return;
    } catch (e) {
      const cause = e?.cause?.code || e?.cause?.message || e?.message || "unknown";
      if (attempt < MAX_TRIES) {
        const wait = 4000 * attempt;
        console.warn(`  ⚠ network fail (${label}: ${cause}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
        await sleep(wait);
        continue;
      }
      console.error(`✗ network fail (${label}): ${cause} — gave up after ${MAX_TRIES} tries`);
      return;
    }
  }
}

// --only hush1,wow2,… regenerates just those panels.
const onlySet = typeof flags.only === "string"
  ? new Set(flags.only.split(",").map((x) => x.trim().toLowerCase()))
  : null;
const wants = (name) => !onlySet || onlySet.has(name);

const jobs = [];
if (wants("cover"))
  jobs.push({ prompt: build(SECTION_VARIANTS.sleep2, true), out: `${LANE}/out/marimbaba-p-cover.png`, label: "marimbaba-p cover" });
for (let i = 0; i < SECTION_ORDER.length; i++) {
  const name = SECTION_ORDER[i];
  if (!wants(name)) continue;
  jobs.push({
    prompt: build(SECTION_VARIANTS[name], name === "sleep2"),
    out: `${LANE}/out/marimbaba-p-sec-${i}-${safeName(name)}.png`,
    label: `marimbaba-p §${i} ${name}`,
  });
}

progress.begin({ type: "illy", label: `marimbaba-p · ${jobs.length} panels` });
let done = 0;
for (const job of jobs) {
  await generate(job.prompt, job.out, job.label);
  progress.update((++done / jobs.length) * 100);
}
progress.end();
console.log(`\n✓ marimbaba storyline panel set — ${jobs.length} job(s) · the late-night help call`);
