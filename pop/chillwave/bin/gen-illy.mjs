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
//   node bin/gen-illy.mjs --slug helpabeach
//   node bin/gen-illy.mjs --slug helpabeach --force
//   node bin/gen-illy.mjs --slug helpabeach --size 1024x1536

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

const SLUG  = flags.slug || "helpabeach";
// --portrait → vertical 9:16 set: gpt-image 1024x1536, "-p" filename
// infix, and a tall-composition override appended to every prompt.
const PORTRAIT = flags.portrait === true;
const SIZE  = flags.size || (PORTRAIT ? "1024x1536" : "1024x1024");
const TAG   = PORTRAIT ? "-p" : "";
const FORCE = flags.force === true;
const PROMPT_PATH = `${LANE}/${SLUG}.illy.txt`;
const OUT_PATH    = `${LANE}/out/${SLUG}${TAG}-cover.png`;
const PORTRAIT_NOTE =
  "PORTRAIT OVERRIDE — recompose for a TALL vertical 9:16 frame (not square), " +
  "and PULL THE CAMERA BACK and slightly UP — a WIDE, roomy shot with a generous " +
  "band of loft CEILING showing above the windows: the WHOLE Rhizome Health clinic " +
  "is visible around the figures — exam bench, counter, rolling cart, tall arched " +
  "windows, ceiling, plants, pale wood floor — a complete room, the figures " +
  "comfortably within it, NEVER cropped tight. CRUCIAL — the PROJECTED Rhizome " +
  "Health logo sits HIGH and HORIZONTALLY CENTRED in the upper frame, inside the " +
  "central safe zone with a wide cream margin on BOTH sides, never near an edge: " +
  "the entire 'Rhizome Health' wordmark legible, both words complete, prominent, " +
  "'Health' never cut off — clear billboard signage centred above the scene. keep " +
  "the colored-pencil + gouache medium on cream paper, the no-outline volume " +
  "modeling, diegetic light only, and the same story beat for this section — only " +
  "the framing widens.";

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
// Rhizome Health logo lockup — handed to the model so the PROJECTED
// clinic logo (green radiating-roots mark + wordmark + cyan
// Aesthetic.Computer line + grey tagline) renders faithfully on the
// back wall. Same ref the /marketing rhizome-health campaign uses.
const RHIZOME_LOGO_REF =
  `${REPO}/marketing/campaigns/rhizome-health/refs/rhizome-health-logo.png`;
// The whistlegraph butterfly, given to the model as a REFERENCE so it
// DRAWS it (hand-penned, in the colored-pencil medium) on a white
// paper scrap on jeffrey's closed Neo lid — never composited/pasted.
const BUTTERFLY_REF = `${LANE}/assets/wg-scrap.png`;
const REFS = [...SHOOT_REFS, ...SELFIE_REFS, RHIZOME_LOGO_REF, BUTTERFLY_REF].filter((p) => {
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

// Per-section arc — THE EXTRACTION STORY. The 9 song sections tell one
// satirical visit: jeffrey arrives, signs in, has his digital "cultural
// juices" extracted (a 3-panel arc — begins → scary flickering peak →
// calms), gets a gentle checkup, is paid $800 cash, a handshake, and
// leaves. Keys MUST match struct.sections names + ordering so
// preview-score.mjs's safeName() resolves the files.
const SECTION_ORDER = [
  "tide-in", "drift 1", "swell 1", "deep-current", "drift 2",
  "undertow", "swell 2", "tide-out", "ebb",
];
const SECTION_VARIANTS = {
  "tide-in":
    "SECTION OVERRIDE — tide-in (jeffrey arrives from the street): the widest, most establishing panel — the Rhizome Health clinic seen almost from OUTSIDE, looking in through the open cast-iron loft entrance from the sunlit SoHo sidewalk; a sliver of street, pavement and a curbside planter frame the doorway. through it the whole clinic opens up — tall arched windows, a band of ceiling, white brick, exam bench, counter, rolling cart, plants, and the projected 'Rhizome Health' logo glowing HIGH and CENTRED on the back wall, the complete wordmark fully visible. jeffrey has just stepped through the doorway, mid-stride, fairly small in the frame, his chartreuse Neo CLOSED under one arm, looking into the room. the clinician across the room by the cart glances up to greet him. calm bright mid-morning, lights steady and warm. the OFFICE itself is the subject — jeffrey is simply arriving. this panel may break peer-framing: jeffrey small in the doorway, the room is the hero.",
  "drift 1":
    "SECTION OVERRIDE — drift 1 (he signs in): jeffrey has settled onto the edge of the exam bench, his chartreuse Neo now OPEN on his lap — one hand still typing on it as ever — while with his other hand he signs an intake / consent form on a clipboard the clinician holds out (a tiny green root-mark at the form's corner). the laminated '@jeffrey' handle wristband is on his wrist. calm, bright, even mid-morning daylight, lights steady. both relaxed, peers. the projected logo steady, its handle-count at its LOWEST. jeffrey faintly amused, half his attention still on the laptop. the signing hands and the clipboard carry the resolved modeling.",
  "swell 1":
    "SECTION OVERRIDE — swell 1 (the extraction begins): jeffrey reclines back on the exam bench, his open Neo still on his lap, still typing one-handed. the clinician gently clips a soft, pale, organic-tech extractor to his forearm — a faintly Cronenberg sci-fi soft-bodied apparatus, a flexible translucent conduit running from it to a clear empty glass vessel on the cart. the FIRST faint glowing matrixy bits — little green code-glyphs and colored pixels — begin to drift up out of jeffrey along the conduit. lights still normal and warm; the mood curious, not yet scary. jeffrey mildly intrigued, unbothered. the extraction is starting — building. NO blood, NO needle-in-skin, NO wound — only light and code leaving him.",
  "deep-current":
    "SECTION OVERRIDE — deep-current (the scary peak — lights flickering): the unsettling moment. the loft lights FLICKER and strobe, the room dims and jumps, long shadows leaping — an eerie, uncanny Cronenberg sci-fi beat. the extraction is in full flow: a thick streaming ribbon of matrixy bits — glowing green code, colored pixel-light, jeffrey's digital 'cultural juices' and art-soul — pulled out of him along the conduit into the glass vessel, which now glows bright with captured light. the soft apparatus pulses. jeffrey's expression is UNEASY — eyes wide, gripping the edge of the bench, the glow of his own laptop stuttering with the flicker. the deepest, darkest, scariest panel — built purely from flickering light and uncanny mood. still NO blood, NO gore, NO wound — jeffrey is whole, only light and code leaving him.",
  "drift 2":
    "SECTION OVERRIDE — drift 2 (it calms — the chill-out): the scare passes. the loft lights are steady, even and warm again, the room settled and ordinary. the extraction is done — the conduit unhooked and coiled, the soft extractor set aside, and the clear glass vessel now FULL, glowing gently with jeffrey's captured cultural juices, standing on the rolling cart. jeffrey exhales, relaxes back, and returns to calmly typing on his open Neo — fine again, unshaken. relief, calm restored. soft daylight, gentle mood.",
  "undertow":
    "SECTION OVERRIDE — undertow (a gentle checkup): the quietest, stillest panel — an ordinary soothing wellness checkup now, the clinician making sure jeffrey is alright after the extraction: she listens with a stethoscope at his back, or takes his pulse at his wrist — calm, gentle, unhurried. jeffrey sits serene, half-absorbed again in his open Neo on his lap. the hushed restful beat — soft low light, the room calm, the glowing vessel of cultural juices quiet on the cart, the projected logo a soft glow. jeffrey's expression: calm, eyes half-lowered.",
  "swell 2":
    "SECTION OVERRIDE — swell 2 (paid — $800 cash): the payoff, the brightest fullest panel. the clinician hands jeffrey a plain unsealed envelope with a clear fan of US one-hundred-dollar bills at its mouth — eight of them, $800 cash, payment for his extracted cultural juices. jeffrey looks UP from his laptop at last to take the envelope, a real warm shared closed-mouth smile between the two peers. the projected Rhizome Health logo flares BRIGHTEST here, its handle-count ticked to its HIGHEST, a clear lime-and-cyan glow. bright, warm, the deal done well.",
  "tide-out":
    "SECTION OVERRIDE — tide-out (the handshake): the visit resolves on a firm, friendly two-person HANDSHAKE — jeffrey and the clinician shaking hands, the deal sealed, both pleased. the $800 cash envelope now tucked into jeffrey's shirt pocket; his chartreuse Neo CLOSED and held under his other arm. softer, settling late-morning light, lights warm and steady. both still peers at one eye-line — a contented, ordinary, slightly absurd goodbye.",
  "ebb":
    "SECTION OVERRIDE — ebb (out to the street): the long dissolving outro. jeffrey steps back out through the open loft door onto the sunlit SoHo sidewalk, his CLOSED Neo under one arm and the cash envelope in hand, half-turned for a last easy look back. behind him the clinician tidies the cart, and the glass vessel of his glowing extracted cultural juices stays there on it. the room eases to quiet — the FEWEST marks of the set, much bare cream paper, the projected Rhizome Health logo glowing gently on the near-empty wall. calm, restful, almost dissolving.",
};
function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}

const apiKey = loadOpenAIKey();
const basePrompt = readFileSync(PROMPT_PATH, "utf8").trim();

// One gpt-image-2 edit call → write outPath. Per-file cached; --force regens.
// Build the multipart body fresh per attempt — a FormData/Blob body is
// consumed on send and can't be re-streamed on a retry.
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
    const ext = lower.endsWith(".png") ? "png"
              : lower.endsWith(".webp") ? "webp"
              : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  return fd;
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// One gpt-image-2 edit call → write outPath. Per-file cached; --force
// regens. Network failures (ETIMEDOUT / EPIPE mid-upload) and transient
// 429/5xx are retried with backoff so one blip can't kill the batch; a
// hard 4xx (billing / moderation) fails fast. Never throws — a failed
// panel logs and returns so the rest of the run continues.
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
        // 429 / 5xx are transient → retry; other 4xx (billing, moderation) → stop.
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
      // network-level failure (fetch threw) — ETIMEDOUT / EPIPE / DNS …
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

const portraitTail = PORTRAIT ? `\n\n${PORTRAIT_NOTE}` : "";

// --only <name|idx>[,…] regenerates just those panels (others untouched).
const onlySet = SECTIONS_MODE && typeof flags.only === "string"
  ? new Set(flags.only.split(",").map((x) => x.trim()))
  : null;
const panelIdx = SECTIONS_MODE
  ? SECTION_ORDER.map((_, i) => i).filter((i) =>
      !onlySet || onlySet.has(SECTION_ORDER[i]) || onlySet.has(String(i)))
  : [];
// progress heartbeat → ~/.ac-pop-renders/ (Slab menubar reads it)
const totalPanels = 1 + panelIdx.length;
let donePanels = 0;
progress.begin({ type: "illy", label: `${SLUG}${TAG} · ${totalPanels} panels` });

// Main hero cover (its own composition via the base prompt alone).
await generate(basePrompt + portraitTail, OUT_PATH, `${SLUG}${TAG} cover`);
progress.update((++donePanels / totalPanels) * 100);

// Per-section illys (sequential — keeps API pressure low, lets each be
// individually cached/--force'd). The butterfly is drawn natively by
// the model from BUTTERFLY_REF — no post-pass composite.
for (const i of panelIdx) {
  const name = SECTION_ORDER[i];
  const variant = SECTION_VARIANTS[name];
  const out = `${LANE}/out/${SLUG}${TAG}-sec-${i}-${safeName(name)}.png`;
  await generate(
    `${basePrompt}\n\n${variant}${portraitTail}`,
    out,
    `${SLUG}${TAG} §${i} ${name}`,
  );
  progress.update((++donePanels / totalPanels) * 100);
}
progress.end();
