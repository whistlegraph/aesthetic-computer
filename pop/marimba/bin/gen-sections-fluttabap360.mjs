#!/usr/bin/env node
// marimba/bin/gen-sections-fluttabap360.mjs — generate the fluttabap360
// STORYLINE panel set: a portrait (9:16) cover + 11 per-beat
// colored-pencil + gouache illustrations tracing "the monarch keeper" —
// the butterfly cosplayer (jeffrey identity-ref character) learning to
// DANCE with living-data monarchs in a near-future AC nature-park.
//
// Concept + story: pop/marimba/fluttabap360.story.txt (read it first).
// Medium + character inherited verbatim from the locked album cover
// (pop/marimba/fluttabap360.illy.txt): colored-pencil + gouache on warm
// cream paper, the butterfly cosplayer — pale-yellow button-down,
// hand-made monarch paper-mache wings on wooden dowels, antenna headband.
//
// THE 11 UNIQUE BEATS (one illustration per unique section NAME):
//   intro · butterfly · palofmine · mommywow · slinky · fly · ride ·
//   cave · progression · land · button
// The struct (out/fluttabap360.struct.json) repeats these names across
// four key passes (C→D→E→home C→F). preview-score.mjs resolves a panel
// per section INDEX as <slug>-p-sec-<i>-<name>.png, so after generating
// the 11 unique-by-NAME panels we COPY each into every struct index that
// shares its name — 34 index files, but only 11 + cover gpt-image-2 calls
// (the visualizer's per-section tint + key-brightening makes each pass
// read brighter/higher). Cheap to re-run; per-file cached.
//
// Output: pop/marimba/out/fluttabap360-p-cover.png            (portrait hero)
//         pop/marimba/out/fluttabap360-p-sec-<i>-<name>.png    (34 index files)
//         pop/marimba/out/_fluttabap360-beat-<name>.png        (11 unique masters, cached)
//
// Usage:
//   node pop/marimba/bin/gen-sections-fluttabap360.mjs              # cached
//   node pop/marimba/bin/gen-sections-fluttabap360.mjs --force      # regen all
//   node pop/marimba/bin/gen-sections-fluttabap360.mjs --only fly   # one beat
//   node pop/marimba/bin/gen-sections-fluttabap360.mjs --proof      # proof set only
//       (cover + intro + fly + button — validate look/continuity cheap)
//   node pop/marimba/bin/gen-sections-fluttabap360.mjs --no-fan-out # skip index copies

import { readFileSync, writeFileSync, existsSync, mkdirSync, copyFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import * as progress from "../../lib/render-progress.mjs";
import { COLORED_PENCIL_TOOTH } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const SLUG = "fluttabap360";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}
const FORCE   = flags.force === true;
const PROOF   = flags.proof === true;       // cover + intro + fly + button only
const NO_FAN  = flags["no-fan-out"] === true;
const SIZE    = "1024x1536";                // portrait 9:16-ish
const TAG     = "-p";
mkdirSync(`${LANE}/out`, { recursive: true });

// ── identity refs — the butterfly-cosplayer character ────────────────
// jeffrey portrait refs (same set as gen-illy.mjs) carry the FACE; the
// locked cover carries the COSTUME + medium (wings / antenna headband /
// pale-yellow button-down / the monarchs / the colored-pencil look) so
// every panel holds character + medium continuity.
const SHOOT_DIR   = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const COVER_REF   = `${LANE}/out/${SLUG}-cover.png`;
const REFS = [
  COVER_REF,                                 // costume + medium anchor (first)
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

// ── PORTRAIT recomposition — tall vertical 9:16 staging ──────────────
const PORTRAIT_NOTE =
`PORTRAIT OVERRIDE — compose for a TALL vertical 9:16 frame. a roomy full-figure shot: the keeper stands comfortably WITHIN the park, head near the upper third, the lawn falling away below, sky above — never cropped tight. keep the colored-pencil + gouache medium on warm cream paper, the confident hatching + striping, diegetic park sunlight only.`;

// ── shared scene law (distilled from fluttabap360.illy.txt cover) ────
const MEDIUM =
`${COLORED_PENCIL_TOOTH}

confident hatching and striping build tone — tapered pencil edges, visible paper grain, optical mixing in the knit textures. diegetic light only — clean park sunlight, NO overlay shine, NO glow filter, NO neon. NOT a photograph, NOT cinematic. this is a hand-drawn drawing. the keeper NEVER looks at the camera — his eyes always follow the butterflies.`;

const KEEPER =
`THE KEEPER — the butterfly cosplayer, recognizable from the jeffrey reference photographs AND the album-cover ref: a gently-grinning adult about 30, soft brown curly hair, light beard. he wears his REAL clothes — a pale-yellow short-sleeve button-down with a small enamel butterfly pin on the collar, and blue jeans; NO tutu, NO leotard, NO costume beyond the wings. strapped to his back are HAND-MADE monarch wings — orange + black markings on paper-mache panels, edges visibly hand-cut, two slim wooden dowels along the bone of each wing; the wings tip + open with his motion. a thin black antenna headband arcs above his hair, springs tipped with two tiny felt balls. he is delighted, easy, alive — the dance is happening TO him.`;

const MONARCHS =
`THE MONARCHS — real monarch butterflies (orange + black, painted-paper edged, drawn at different scales for depth) that are ALSO the living data of this near-future AESTHETIC COMPUTER nature-park. they drift on their OWN little arcs, each sharing the same turning motion — a current of grace, NEVER a swarm. the park's quiet computation reads ONLY as: a faint hand-ruled ground-grid in the lawn (almost invisible, pencil) and soft hand-drawn DOTTED signal-arcs trailing the butterflies' paths. NO screens, NO monitors, NO glowing UI, NO neon — the park is hand-drawn and warm; the "data" is just painted-paper butterflies + faint pencil grids/arcs.`;

const PALETTE =
`PALETTE — warm cream paper ground; pale-sky-blue overhead; sap-green park grass; pale-yellow button-down; soft brown hair + warm grey; orange + black on the monarch wings (cosplay + real). hand-drawn, hatched, print-tech-aware. clean park sunlight; no haze, no neon, no glow filters.`;

const AVOID =
`AVOID — any photographic / cinematic / neon look (this is a colored-pencil + gouache drawing); ANY screen, monitor, laptop, phone, tablet or glowing UI; any swarm of butterflies (a few partners on arcs, never a cloud); any motion-blur / speed-line / glow-filter language; any readable text / wordmark / logo; modern flat tech-illustration style; the keeper looking at or acknowledging the camera; a tutu or ballet costume (his real clothes only).`;

// ── COVER prompt — the locked album-cover composition, widened ───────
const COVER_VARIANT =
`COVER COMPOSITION — re-stage the locked album cover for a TALL portrait crop: the keeper caught mid-ballet, rising onto the ball of one foot (relevé) with the other leg extended back into a low arabesque, both arms lifted + curved overhead in a soft port de bras, fingertips reaching. four or five monarchs circle his lifted hands + arched back like dance partners, one resting a beat on his outstretched fingertips. weight floating up, chin lifted, lost in the dance. a soft ground-shadow anchors his standing foot to the green lawn. clean late-morning sun raking from camera-left.`;

// ── the 11 unique story beats — "the monarch keeper" ─────────────────
// One illustration per UNIQUE name (preview-score reuses by index).
const BEAT_NAMES = [
  "intro", "butterfly", "palofmine", "mommywow", "slinky",
  "fly", "ride", "cave", "progression", "land", "button",
];
const PROOF_NAMES = new Set(["intro", "fly", "button"]);  // + cover

const BEATS = {
  intro:
`BEAT — INTRO (dawn, the water-drop) · MOOD: stillness, pre-sun hush. a WIDE low vertical shot of the empty park lawn at first light, dew on the grass. ONE monarch rests folded on a single tall grass blade; a fat dew-drop hangs from the blade tip, caught the instant before it falls. the keeper is FAR OFF and small at the edge of the frame, just arriving, barely awake. cool pale-blue pre-sun sky, long soft shadows, a faint hand-ruled ground-grid in the lawn almost invisible. the world holding its breath before the first note.`,
  butterfly:
`BEAT — BUTTERFLY (the hook wakes) · MOOD: recognition, first lift. the keeper, mid-frame now, lifts one open hand and the resting monarch UNFOLDS + lifts off toward his fingertips — the very first flutter. his face lighting up with recognition, weight just beginning to shift onto the ball of one foot. a faint hand-drawn dotted signal-arc trails the butterfly's rising path. morning sun warming the lawn behind him.`,
  palofmine:
`BEAT — PALOFMINE (partner found) · MOOD: warm, companionable. a tighter three-quarter shot: the monarch settles for a beat on his outstretched fingertip, the two of them regarding each other — "pal of mine." his curly head tilted, a quiet grin, antenna headband springs tipped with felt balls. one or two more monarchs drift in on their own little arcs at the frame edges. the data has chosen to land on him.`,
  mommywow:
`BEAT — MOMMYWOW (the held hush) · MOOD: suspended awe (this lands under a long held chord). the keeper goes very still, both hands cupped softly open at chest height, eyes half-lowered, a monarch hovering a hand-span above his open palms — neither touching. the lawn behind blurs into warm cream paper. time suspended. "mommy — wow."`,
  slinky:
`BEAT — SLINKY (the learning spiral) · MOOD: playful, gathering momentum. the keeper begins to MOVE — a loose spiralling step, body uncoiling, arms drawing a long S-curve through the air; three or four monarchs trace the SAME spiralling path beside him on their arcs, learning his shape as he learns theirs. faint hand-drawn dotted signal-arcs describe the shared spiral. the wings on his back tip open with the motion.`,
  fly:
`BEAT — FLY (full flight — the big lift) · MOOD: open joy, the brightest beat. the keeper at FULL ballet lift — relevé + low arabesque + port de bras, both arms lifted + curved overhead — four or five monarchs circling his lifted hands + arched back like dance partners, all sharing the same upward turning motion. chin lifted, lost in it, a current of grace. clean late-morning sun. a full-body vertical action shot, camera pulled back + up.`,
  ride:
`BEAT — RIDE (the squeak-ride duet) · MOOD: mischievous, fast, a little wild. the keeper mid-turn, ONE monarch swooping in LOW and another answering UP HIGH — a call-and-response chase, his head whipping to follow the high one, a delighted open-mouthed laugh. two crossing hand-drawn dotted signal-arcs (one low, one high) describe the duet. light + quick.`,
  cave:
`BEAT — CAVE (dub breakdown, the data rests) · MOOD: somber, breath-held — the ONE dark panel. the park dims: the keeper has stepped into the cool mouth of a computational cave / under a deep arching shade, the lawn falling away to dusk-blue behind him. he stands very still, head bowed, ONE monarch perched on his lowered hand, its wings half-folded + dim — the data resting, the system breathing. the faint hand-ruled ground-grid is a touch more visible in the low light (still hand-drawn pencil, NEVER neon). holding his breath.`,
  progression:
`BEAT — PROGRESSION (the recommitment, climbing into bright light) · MOOD: rising, brighter than any pass before. the keeper steps OUT of the cave-shade back into bright light, the monarchs streaming back to him from the lawn — a rising procession of three, four, five butterflies converging on his lifting hands as he RE-ENTERS the dance, higher + brighter. the brightest light yet, sun fully up.`,
  land:
`BEAT — LAND (the soft landing) · MOOD: warm, satisfied, spent. the keeper coming gently DOWN out of the lift — descending from relevé back to flat-footed, arms lowering in a soft port de bras — the monarchs settling around him: one returning to rest on his fingertip, others alighting on the grass + on his shoulders. weight returning to the earth. evening-gold light.`,
  button:
`BEAT — BUTTON (the last frame) · MOOD: a quiet held breath out. the final image: the keeper at rest on the lawn, the dance done, the monarchs settled, ONE last monarch lifting off his fingertip into the warm gold sky — a quiet goodbye. balanced, light, gently grinning, eyes following the last butterfly up. a soft ground-shadow anchors his foot to the lawn. the end.`,
};

function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}
function build(beatText, isCover) {
  const orient = isCover ? "" : `\n\n${PORTRAIT_NOTE}`;
  return [MEDIUM, KEEPER, MONARCHS, beatText, PALETTE, AVOID].join("\n\n") + orient + "\n";
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
// regens. Transient 429/5xx + network blips retried; never throws.
async function generate(promptText, outPath, label) {
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) { console.log(`✓ cached → ${rel}`); return true; }
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
          await sleep(wait); continue;
        }
        console.error(`✗ OpenAI ${res.status} (${label}): ${err.slice(0, 600)}`);
        return false;
      }
      const json = await res.json();
      const b64 = json.data?.[0]?.b64_json;
      if (!b64) { console.error(`✗ no image (${label}): ${JSON.stringify(json).slice(0, 280)}`); return false; }
      writeFileSync(outPath, Buffer.from(b64, "base64"));
      const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
      const u = json.usage || {};
      const tok = u.input_tokens ? ` · tok in=${u.input_tokens} out=${u.output_tokens}` : "";
      console.log(`✓ ${elapsed}s${tok} → ${rel}`);
      return true;
    } catch (e) {
      const cause = e?.cause?.code || e?.cause?.message || e?.message || "unknown";
      if (attempt < MAX_TRIES) {
        const wait = 4000 * attempt;
        console.warn(`  ⚠ network fail (${label}: ${cause}) — retry ${attempt}/${MAX_TRIES - 1} in ${wait / 1000}s`);
        await sleep(wait); continue;
      }
      console.error(`✗ network fail (${label}): ${cause} — gave up after ${MAX_TRIES} tries`);
      return false;
    }
  }
}

// --only fly,intro,… regenerates just those beats.
const onlySet = typeof flags.only === "string"
  ? new Set(flags.only.split(",").map((x) => x.trim().toLowerCase()))
  : null;
const wantBeat = (name) => {
  if (PROOF) return PROOF_NAMES.has(name);
  if (onlySet) return onlySet.has(name);
  return true;
};
const wantCover = !onlySet || onlySet.has("cover");

// Master path for a unique beat illustration (one per NAME, reused
// across struct indices). Underscore prefix so it sorts away from the
// -p-sec-<i>- index files the visualizer actually loads.
const masterPath = (name) => `${LANE}/out/_${SLUG}-beat-${safeName(name)}.png`;

// ── 1) generate the cover + the 11 unique beat masters ───────────────
const jobs = [];
if (wantCover)
  jobs.push({ prompt: build(COVER_VARIANT, true), out: `${LANE}/out/${SLUG}${TAG}-cover.png`, label: `${SLUG}${TAG} cover` });
for (const name of BEAT_NAMES) {
  if (!wantBeat(name)) continue;
  jobs.push({ prompt: build(BEATS[name], false), out: masterPath(name), label: `${SLUG} beat · ${name}` });
}

progress.begin({ type: "illy", label: `${SLUG}${TAG} · ${jobs.length} panels${PROOF ? " (PROOF)" : ""}` });
let done = 0;
for (const job of jobs) {
  await generate(job.prompt, job.out, job.label);
  progress.update((++done / jobs.length) * 100, { done, total: jobs.length });
}
progress.end();

// ── 2) fan out the unique masters → one file per struct INDEX ────────
// preview-score.mjs loads <slug>-p-sec-<i>-<name>.png by section index;
// names repeat across passes, so copy each master into every index that
// shares its name. Skipped in --proof / --no-fan-out.
if (!PROOF && !NO_FAN) {
  const structPath = `${LANE}/out/${SLUG}.struct.json`;
  if (existsSync(structPath)) {
    const struct = JSON.parse(readFileSync(structPath, "utf8"));
    const sections = struct.sections.slice().sort((a, b) => a.startSec - b.startSec);
    let fanned = 0, missing = 0;
    for (let i = 0; i < sections.length; i++) {
      const name = sections[i].name;
      const src = masterPath(name);
      const dst = `${LANE}/out/${SLUG}${TAG}-sec-${i}-${safeName(name)}.png`;
      if (!existsSync(src)) { console.warn(`  ⚠ no master for "${name}" (§${i}) — run without --only/--proof`); missing++; continue; }
      copyFileSync(src, dst);
      fanned++;
    }
    console.log(`  fan-out: ${fanned} index panels written${missing ? `, ${missing} missing` : ""}`);
  } else {
    console.warn(`  ⚠ struct missing (${structPath.replace(REPO + "/", "")}) — skip fan-out; run render-fluttabap360.mjs --no-open first`);
  }
}

console.log(`\n✓ ${SLUG} storyline panel set${PROOF ? " (PROOF: cover + intro + fly + button)" : ""} — ${jobs.length} gpt-image-2 job(s) · "the monarch keeper"`);
