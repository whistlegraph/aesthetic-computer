#!/usr/bin/env node
// marimba/bin/gen-sections.mjs — generate the marimbaba STORYLINE panel
// set: a portrait (9:16) cover + 10 per-beat illustrations tracing
// jeffrey's late-night "computer help call" with Bill Gates.
//
// Concept (jas, 2026-05-22):
//   the marimbaba lullaby visualized as one quiet story — jeffrey
//   ARRIVES at Bill Gates's hushed late-night study, says hi, and walks
//   him through a typing session on a white IBM Selectric portable
//   typewriter — the iconic type-ball model. they type, discuss its
//   mechanisms, and react happy / sad / SHOCKED to what's on the page —
//   but the audience NEVER sees what's typed. TEN beats,
//   two per .np section (hush / twinkle / wow / baba / sleep), so the
//   visualizer changes faster. the final 'sleep2' panel lands exactly
//   on the locked marimbaba album cover.
//
// Medium + identity are inherited verbatim from marimbaba.illy.txt
// (colored-pencil + gouache on warm cream paper, the two men's exact
// faces / outfits / Sailor pens, the typewriter, NO screen of any kind).
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

const SELECTRIC =
`THE TYPEWRITER — the only device in the story is a lovely WHITE IBM Selectric portable typewriter — the iconic 1970s/80s Selectric II model with the spherical metal TYPE BALL (no traditional swinging typebars; the chrome/silver type-element ball rotates and tilts to strike the paper). a clean glossy white plastic shell with the small IBM oval emblem on the front-right (rendered as an indistinct emblem, NOT a readable wordmark), cream/tan rubber platen with the round platen knob on the right side, a black ribbon cartridge sitting visibly behind the type ball, a chrome paper-bail bar across the front of the platen. a single sheet of plain white typing paper is rolled into the platen — BUT THE AUDIENCE NEVER SEES WHAT IS TYPED ON THE PAGE: every shot frames the typewriter from front-three-quarter or from the side or from behind the figures so the paper / platen surface is occluded by the type ball mechanism, the typebar housing, jeffrey's or Bill's hands, or the typewriter's own carriage body. NEVER a top-down or over-the-shoulder shot of the paper, NEVER a readable line of typed text, NEVER a thought-bubble or floating words. ABSOLUTELY NO screen, monitor, laptop, phone, tablet or any display anywhere — only this typewriter.`;

const PALETTE =
`PALETTE — warm cream paper ground; light-blue cotton shirt and sage-green knit; soft brown hair and warm grey; buttery yellow on jeffrey's pen, deep red on Bill's; one coral pop on the glasses-cord; glossy white plastic on the Selectric typewriter with chrome accents on the type ball and paper bail; the gentle amber pool of the desk lamp against cool blue night dark. hand-drawn, hatched, print-tech-aware.`;

const AVOID =
`AVOID — any photographic / cinematic / neon look (this is a colored-pencil + gouache drawing); ANY screen, monitor, laptop, phone or display; the IBM Model M / any computer keyboard (the device is the IBM Selectric typewriter, NOT a keyboard); ANY visible typed line on the paper — the page surface must always be occluded by the type ball mechanism, hands, the carriage body, or the camera angle (NO top-down or over-the-shoulder shots of the paper); any thought-bubble / floating words / readable text anywhere; any readable brand wordmark or logo; modern flat tech-illustration style; hero-pose centering; either figure looking at or acknowledging the camera.`;

// ── per-beat story — the late-night help call, 10 beats ──────────────
// Keys + ordering MUST match marimbaba.struct.json sections[] so
// preview-score.mjs's safeName() resolves out/marimbaba-p-sec-<i>-<name>.png.
const SECTION_ORDER = [
  "hush1", "hush2", "twinkle1", "twinkle2", "wow1",
  "wow2", "baba1", "baba2", "sleep1", "sleep2",
];
// Dedicated COVER prompt — the locked album-cover composition (intimate
// drowsy close at the typewriter). Used only for the portrait hero cover;
// the visualizer's sleep2 beat is the DEPARTURE shot, not this one.
const COVER_VARIANT =
`COVER COMPOSITION — the locked album-cover crop: a tight, warm, head-and-shoulders close of the two men side by side at the WHITE SELECTRIC TYPEWRITER, heads almost touching the top edge, both quiet and drowsy, eyes half-lowered, lips closed, a touch wistful — typing softly into the night. the top of the typewriter (type ball + ribbon cartridge + chrome paper bail) along the bottom edge; THE PAPER SURFACE IS NEVER VISIBLE.`;

const SECTION_VARIANTS = {
  hush1:
`BEAT — HUSH 1 (jeffrey approaches) · EMOTION: quiet ANTICIPATION, a little wistful. the opening establishing panel, OUTSIDE at night. jeffrey walks up a quiet garden path toward Bill Gates's house, hands in pockets, thoughtful and expectant. one ground-floor window glows warm amber — the study, where a small lamplit silhouette of Gates can just be made out hunched over the white typewriter inside. dark trees, a cool blue night, a sliver of moon. jeffrey small in the frame, arriving. WIDE establishing shot, low angle from across the path.`,
  hush2:
`BEAT — HUSH 2 (the doorway hello) · EMOTION: warm, shy GREETING — Bill Gates SURPRISED and quietly PLEASED, lonely-glad that someone came. jeffrey has stepped into the doorway of the hushed late-night STUDY, raising one hand in a small hello. across the lamplit room Bill Gates twists round from the WHITE SELECTRIC TYPEWRITER on the desk, eyebrows lifted in mild surprise, a relieved smile breaking. the typewriter shown from the side, paper-bail and rolled paper occluded by the type ball and carriage. desk lamp the one warm pool of light; bookshelves, a worn armchair, a dark night window. MEDIUM-WIDE room shot from near the doorway.`,
  twinkle1:
`BEAT — TWINKLE 1 (companionable close-up) · EMOTION: warm COMPANIONABLE ease — both side by side, soft contented closed-mouth smiles, shoulders loose. a TIGHTER two-shot of jeffrey and Bill seated together at the typewriter, the warm lamp between them, the Selectric in soft focus in the foreground (type ball clearly visible). intimate paired portrait, neither centred. the page surface occluded by the type ball and carriage.`,
  twinkle2:
`BEAT — TWINKLE 2 (the type ball, in close-up) · EMOTION: jeffrey CURIOUS, leaning in to learn; Gates absorbed in his beloved machine, lit up showing it off. EXTREME CLOSE-UP on Bill's hand pointing at the silver chrome TYPE BALL on the Selectric mid-spin, his finger an inch away. the type ball mechanism, ribbon cartridge, the typebar housing fill the frame. behind / above the typewriter we see the upper portions of both faces leaning in, eyes fixed on the gadget — half-faces, half a typewriter close-up. the lamp light catches the chrome. no readable page surface.`,
  wow1:
`BEAT — WOW 1 (jeffrey types — over the shoulder) · EMOTION: FOCUSED concentration — jeffrey absorbed; Gates watching attentively beside him. OVER-THE-SHOULDER shot from BEHIND jeffrey: we see the back of jeffrey's head and his right shoulder in the near foreground, his hand on the Selectric's keys mid-strike. beyond him Bill Gates is leaning in from the right, eyes fixed on the type ball, brow concentrated. the typewriter occupies the centre of the frame, the rolled paper held in by the carriage so we see only its TOP EDGE from this rear angle, never the typed surface. lamp warm. completely different framing from any other beat.`,
  wow2:
`BEAT — WOW 2 (HAPPY reaction — wide) · EMOTION: open JOY — both faces break into bright delighted smiles, eyes crinkled, eyebrows up high; Gates clapping or fist-pumping in the air, jeffrey laughing, both clearly THRILLED by what just appeared. WIDE shot pulled back to show both men leaning back from the typewriter, gesturing with delight in the lamplit study, the Selectric on the desk between them. the page surface is never shown (paper hidden by the type ball mechanism and the carriage from this angle). the brightest, warmest beat of the set.`,
  baba1:
`BEAT — BABA 1 (the disagreement begins) · EMOTION: TENSION — jeffrey re-reading with growing concern, face clouding, brow knitting; Bill Gates defensive, leaning back, arms starting to cross or hands raised in a "what?" gesture. a clear COOLING between them, the smiles gone. they sit at the typewriter but turned slightly AWAY from each other now. lamp dimming subtly, the warmth of the previous beat draining. the page surface still occluded by the typewriter body.`,
  baba2:
`BEAT — BABA 2 (the ARGUMENT — standing) · EMOTION: open CONFLICT — both men STANDING UP at the desk now, gesturing emphatically, faces flushed and tense; Bill pointing accusingly at the typewriter / page area, jeffrey gesturing back angrily, brow furrowed, mouth open mid-word. genuine ARGUMENT — voices raised. the typewriter sits between them on the desk, an obstacle and the cause; the page still rolled in but never visible to the camera (hidden by their gesturing hands and the carriage body). the study harshly lit, harder edges, the warmth gone cool.`,
  sleep1:
`BEAT — SLEEP 1 (the TUG-OF-WAR over the sheet) · EMOTION: physical STRUGGLE — desperate, urgent, no music left in their faces. jeffrey and Bill BOTH GRIPPING the typed sheet of paper at the SAME TIME, one at the carriage end (still rolled into the platen), the other a hand-span away pulling — the paper VISIBLY TEARING, a jagged rip beginning across it, scraps and shredded fibres at the rip line. their hands in tight fists on the paper, faces taut, teeth set, eyes locked. the WHITE SELECTRIC TYPEWRITER between them on the desk, the platen still gripping the paper's bottom half. CRUCIAL — the camera is angled so the FRONT FACE of the paper is HIDDEN: we see the torn paper EDGE-ON (paper rendered as a near-vertical strip from the side, the tear ragged but ONLY the white paper substrate + the edge of the rip visible, NEVER readable typed text). intense, ugly, raw.`,
  sleep2:
`BEAT — SLEEP 2 (DEPARTURE — bittersweet) · EMOTION: heavy SADNESS, regret, finality — neither angry anymore, just hollow. jeffrey is at the open DOORWAY of the study, half-turned back for a last look, his hand clutching ONE TORN HALF of the paper at his side (the rip edge ragged, the back of the paper facing camera, never the typed side). across the lamplit room Bill Gates remains at the WHITE SELECTRIC TYPEWRITER, slumped in his chair, the OTHER TORN HALF of the paper limp in his hand, head bowed. the night through the open door behind jeffrey, the study warmly lit but emptied of any warmth between them. a quiet bittersweet GOODBYE — the visit ended badly. wide framing, room-scale. NEVER show readable typed text on either torn half.`,
};

function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}
function build(sectionBeat, tightCrop) {
  const portrait = tightCrop ? "" : `\n\n${PORTRAIT_NOTE}`;
  return [MEDIUM, JEFFREY, GATES, SELECTRIC, sectionBeat, PALETTE, AVOID].join("\n\n")
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
  jobs.push({ prompt: build(COVER_VARIANT, true), out: `${LANE}/out/marimbaba-p-cover.png`, label: "marimbaba-p cover" });
for (let i = 0; i < SECTION_ORDER.length; i++) {
  const name = SECTION_ORDER[i];
  if (!wants(name)) continue;
  jobs.push({
    prompt: build(SECTION_VARIANTS[name], false),
    out: `${LANE}/out/marimbaba-p-sec-${i}-${safeName(name)}.png`,
    label: `marimbaba-p §${i} ${name}`,
  });
}

progress.begin({ type: "illy", label: `marimbaba-p · ${jobs.length} panels` });
let done = 0;
for (const job of jobs) {
  await generate(job.prompt, job.out, job.label);
  progress.update((++done / jobs.length) * 100, { done, total: jobs.length });
}
progress.end();
console.log(`\n✓ marimbaba storyline panel set — ${jobs.length} job(s) · the late-night help call`);
