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
//   node bin/gen-illy.mjs --slug helpabeach --portrait                   # 1024x1536 "-p" set (IG story)
//   node bin/gen-illy.mjs --slug helpabeach --landscape                  # 1536x1024 "-yt" set (YouTube widescreen)
//   node bin/gen-illy.mjs --slug helpabeach --landscape --sections       # cover + 9 panels for YT
//   node bin/gen-illy.mjs ... --validate-butterfly                       # gpt-4o-mini checks each gen
//                                                                          for a coherent whistlegraph
//                                                                          butterfly on jeffrey's chartreuse
//                                                                          Neo lid; FAILs are regenned up
//                                                                          to --validate-retries N (default 2).
//                                                                          SKIP (no laptop in frame) and PASS
//                                                                          both accept.

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
// --portrait → vertical 9:16 set: gpt-image 1024x1536, "-p" filename.
// --landscape → wide 3:2 / 16:9 set: gpt-image 1536x1024, "-yt" filename.
//   Mutually exclusive with --portrait. Used for the YouTube widescreen
//   visualizer fork (preview-score-helpabeach-yt.mjs).
const PORTRAIT  = flags.portrait === true;
const LANDSCAPE = flags.landscape === true;
if (PORTRAIT && LANDSCAPE) {
  console.error("✗ --portrait and --landscape are mutually exclusive");
  process.exit(1);
}
const SIZE  = flags.size
  || (PORTRAIT ? "1024x1536" : LANDSCAPE ? "1536x1024" : "1024x1024");
const TAG   = PORTRAIT ? "-p" : LANDSCAPE ? "-yt" : "";
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
const LANDSCAPE_NOTE =
  "LANDSCAPE OVERRIDE — recompose for a WIDE 3:2 / 16:9 frame (NOT square, NOT " +
  "tall), and PULL THE CAMERA BACK so the WHOLE Rhizome Health clinic stretches " +
  "horizontally across the frame: tall arched windows running the full width, the " +
  "white-brick back wall extending across, exam bench, counter, rolling cart, " +
  "plants, pale wood floor — a complete, wide room with generous LEFT and RIGHT " +
  "breathing space around the figures, NEVER cropped tight, NEVER tall. CRUCIAL " +
  "— the PROJECTED Rhizome Health logo sits HIGH and HORIZONTALLY CENTRED in the " +
  "upper-third band, complete wordmark legible ('Rhizome' + 'Health' both words " +
  "fully visible, neither cut off), with a wide cream margin on BOTH sides. " +
  "FIGURES are positioned in the LOWER-CENTER of the frame at chest-and-above " +
  "framing, not crammed in a corner, allowing room for a subtle bottom-edge band " +
  "of empty cream paper / floor where the YouTube visualizer chrome (title, " +
  "score-train, progress bar) will sit. keep the colored-pencil + gouache medium " +
  "on cream paper, the no-outline volume modeling, diegetic light only, and the " +
  "same story beat for this section — only the framing widens.";

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
    "SECTION OVERRIDE — tide-in (jeffrey arrives from the street): the widest, most establishing panel — the Rhizome Health clinic seen almost from OUTSIDE, looking in through the open cast-iron loft entrance from the sunlit SoHo sidewalk; a sliver of street, pavement and a curbside planter frame the doorway. through it the whole clinic opens up — tall arched windows, a band of ceiling, white brick, exam bench, counter, rolling cart, plants, and on the white-brick back wall the projected 'Rhizome Health' logo glowing HIGH and CENTRED, the complete wordmark visible, cast by the ceiling-mounted projector. jeffrey has just stepped through the doorway, mid-stride, fairly small in the frame; slung across his body he wears his everyday canvas MESSENGER / SHOULDER BAG — the same bag seen in the jeffrey reference photos — with his laptop tucked away INSIDE it: NO laptop in his hands, nothing carried, just the messenger bag on his shoulder. he looks into the room. the clinician across the room by the cart glances up to greet him. calm bright mid-morning, lights steady and warm. the OFFICE itself is the subject — jeffrey is simply arriving. this panel may break peer-framing: jeffrey small in the doorway, the room is the hero.",
  "drift 1":
    "SECTION OVERRIDE — drift 1 (he signs in): jeffrey has settled onto the edge of the exam bench, his chartreuse Neo now OPEN on his lap — one hand still typing on it as ever — while with his other hand he signs an intake / consent form on a clipboard the clinician holds out (a tiny green root-mark at the form's corner). the laminated '@jeffrey' handle wristband is on his wrist. calm, bright, even mid-morning daylight, lights steady. both relaxed, peers. the projected logo steady, its handle-count at its LOWEST. jeffrey faintly amused, half his attention still on the laptop. the signing hands and the clipboard carry the resolved modeling.",
  "swell 1":
    "SECTION OVERRIDE — swell 1 (the extraction begins): jeffrey reclines back on the exam bench, his hands resting easy at his sides. CRUCIAL — NO laptop in THIS panel: no Neo on his lap or anywhere, his hands are empty. the clinician gently clips a soft, pale, organic-tech extractor to his forearm — a faintly Cronenberg sci-fi soft-bodied apparatus, a flexible translucent conduit running from it to a clear empty glass vessel on the cart. the FIRST faint glowing matrixy bits — little green code-glyphs and colored pixels — begin to drift up out of jeffrey along the conduit. lights still normal and warm; the mood curious, not yet scary. jeffrey mildly intrigued, unbothered. the extraction is starting — building. NO blood, NO needle-in-skin, NO wound — only light and code leaving him.",
  "deep-current":
    "SECTION OVERRIDE — deep-current (the scary peak — lights flickering): the unsettling moment. the loft lights FLICKER and strobe, the room dims and jumps, long shadows leaping — an eerie, uncanny Cronenberg sci-fi beat. the extraction is in full flow: a thick streaming ribbon of matrixy bits — glowing green code, colored pixel-light, jeffrey's digital 'cultural juices' and art-soul — pulled out of him along the conduit into the glass vessel, which now glows bright with captured light. the soft apparatus pulses. jeffrey's expression is UNEASY — eyes wide, both hands gripping the edge of the bench. CRUCIAL — NO laptop in THIS panel: no Neo anywhere, his hands are empty and gripping the bench. the deepest, darkest, scariest panel — built purely from flickering light and uncanny mood. still NO blood, NO gore, NO wound — jeffrey is whole, only light and code leaving him.",
  "drift 2":
    "SECTION OVERRIDE — drift 2 (it calms — the chill-out): the scare passes. the loft lights are steady, even and warm again, the room settled and ordinary. the extraction is done — the conduit unhooked and coiled, the soft extractor set aside, and the clear glass vessel now FULL, glowing gently with jeffrey's captured cultural juices, standing on the rolling cart. jeffrey exhales, relaxes back, and returns to calmly typing on his open Neo — fine again, unshaken. relief, calm restored. soft daylight, gentle mood.",
  "undertow":
    "SECTION OVERRIDE — undertow (a gentle checkup): the quietest, stillest panel — an ordinary soothing wellness checkup now, the clinician making sure jeffrey is alright after the extraction: she listens with a stethoscope at his back, or takes his pulse at his wrist — calm, gentle, unhurried. jeffrey sits serene, half-absorbed again in his open Neo on his lap. the hushed restful beat — soft low light, the room calm, the glowing vessel of cultural juices quiet on the cart, the projected logo a soft glow. jeffrey's expression: calm, eyes half-lowered.",
  "swell 2":
    "SECTION OVERRIDE — swell 2 (paid — $800 cash): the payoff, the brightest fullest panel. the clinician hands jeffrey a plain unsealed envelope with a clear fan of US one-hundred-dollar bills at its mouth — eight of them, $800 cash, payment for his extracted cultural juices. jeffrey looks UP from his laptop at last to take the envelope, a real warm shared closed-mouth smile between the two peers. the projected Rhizome Health logo flares BRIGHTEST here, its handle-count ticked to its HIGHEST, a clear lime-and-cyan glow. bright, warm, the deal done well.",
  "tide-out":
    "SECTION OVERRIDE — tide-out (the handshake): the visit resolves on a firm, friendly two-person HANDSHAKE — jeffrey and the clinician shaking hands, the deal sealed, both pleased. the $800 cash envelope now tucked into jeffrey's shirt pocket; his other hand relaxed easy at his side. CRUCIAL — NO laptop in THIS panel: no Neo anywhere, both his arms are free for the handshake. softer, settling late-morning light, lights warm and steady. both still peers at one eye-line — a contented, ordinary, slightly absurd goodbye.",
  "ebb":
    "SECTION OVERRIDE — ebb (out to the street): the long dissolving outro. jeffrey steps back out through the open loft door onto the sunlit SoHo sidewalk, his CLOSED Neo under one arm and the cash envelope in hand, half-turned for a last easy look back. behind him the clinician tidies the cart, and the glass vessel of his glowing extracted cultural juices stays there on it. the room eases to quiet — the FEWEST marks of the set, much bare cream paper, the projected Rhizome Health logo glowing gently on the near-empty wall. calm, restful, almost dissolving.",
};
function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}

const apiKey = loadOpenAIKey();
const basePrompt = readFileSync(PROMPT_PATH, "utf8").trim();

// ── butterfly validator ──────────────────────────────────────────────
// gpt-4o-mini vision check: does jeffrey's chartreuse-green Neo show
// the whistlegraph-butterfly white scrap on its lid? Three possible
// verdicts:
//   PASS — butterfly is recognizable + coherent on the lid
//   SKIP — no chartreuse laptop visible in this panel (accepted)
//   FAIL — laptop is present but the lid shows the WRONG mark
//          (tree-person / apple / pals / random doodle / nothing)
// Failures trigger a regeneration of the panel up to N attempts.
// See [[feedback_imagegen_jeffrey_butterfly_neo.md]].
const VALIDATE_BUTTERFLY = flags["validate-butterfly"] === true;
const VALIDATE_RETRIES   = Number(flags["validate-retries"] ?? 2);

function imgDataUrl(path) {
  const buf = readFileSync(path);
  const lower = path.toLowerCase();
  const mime = lower.endsWith(".png") ? "image/png"
             : lower.endsWith(".webp") ? "image/webp"
             : "image/jpeg";
  return `data:${mime};base64,${buf.toString("base64")}`;
}

async function validateButterfly(imagePath) {
  if (!existsSync(BUTTERFLY_REF)) {
    console.warn(`  ⚠ butterfly ref missing at ${BUTTERFLY_REF.replace(REPO + "/", "")} — skipping validation`);
    return { verdict: "SKIP", reason: "no reference image on disk" };
  }
  const body = {
    model: "gpt-4o-mini",
    max_tokens: 80,
    messages: [{
      role: "user",
      content: [
        { type: "text", text:
`You are validating an AI-generated illustration for whistlegraph-butterfly coherence on jeffrey's laptop lid.

IMAGE 1 is the reference whistlegraph butterfly: a hand-drawn figure with a smiling rectangular head/body down the centre, four round wings (two upper, two lower), and a small rectangular tail. Simple grey marker lines on white.

IMAGE 2 is the illustration to check. jeffrey's laptop is a chartreuse / lime / yellow-green "MacBook Neo". Look for it. The lid (closed or visible back side) should carry a small WHITE TORN PAPER SCRAP with a hand-drawn butterfly that resembles IMAGE 1.

Decide ONE of three verdicts:
  PASS — chartreuse laptop visible AND its lid clearly shows a white scrap with a butterfly that resembles the reference (smiling head + wings + tail; small variations OK).
  SKIP — NO chartreuse laptop visible anywhere in IMAGE 2 (jeffrey isn't with his Neo in this beat). Accept.
  FAIL — chartreuse laptop IS visible BUT the lid shows the wrong mark (tree-person / stick-figure / apple logo / pals figure / random scribble / blank / wordmark / something else). Reject.

Reply on ONE LINE in exactly this format:
  PASS
  SKIP: <one short clause>
  FAIL: <one short clause naming what's on the lid>` },
        { type: "image_url", image_url: { url: imgDataUrl(BUTTERFLY_REF), detail: "low" } },
        { type: "image_url", image_url: { url: imgDataUrl(imagePath),   detail: "high" } },
      ],
    }],
  };
  const res = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  if (!res.ok) {
    const err = await res.text();
    console.warn(`  ⚠ vision validator ${res.status}: ${err.slice(0, 200)} — accepting as SKIP`);
    return { verdict: "SKIP", reason: `validator error ${res.status}` };
  }
  const json = await res.json();
  const raw  = (json.choices?.[0]?.message?.content || "").trim();
  const verdict = raw.startsWith("PASS") ? "PASS"
                : raw.startsWith("SKIP") ? "SKIP"
                : raw.startsWith("FAIL") ? "FAIL"
                : "SKIP";   // unparseable → accept
  const reason = raw.replace(/^(PASS|SKIP|FAIL)\s*:?\s*/, "").trim();
  return { verdict, reason, raw };
}

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
    // Cached files still get butterfly-validated when --validate-butterfly
    // is on, so a re-run picks up the previous batch's FAILs and regens
    // them in place.
    if (VALIDATE_BUTTERFLY) {
      const v = await validateButterfly(outPath);
      if (v.verdict === "FAIL") {
        console.log(`✗ cached panel failed butterfly check (${v.reason}) — regenerating: ${rel}`);
      } else {
        console.log(`✓ cached · ${v.verdict}${v.reason ? ` (${v.reason})` : ""} → ${rel}`);
        return;
      }
    } else {
      console.log(`✓ cached → ${rel}`);
      return;
    }
  }
  console.log(`▸ ${label} · ${SIZE} · ${REFS.length} refs${VALIDATE_BUTTERFLY ? " · ↻validate" : ""}`);
  const MAX_TRIES = 4;
  // Whole-attempt loop: each VALIDATE_RETRY round burns one image-gen
  // (and its inner network/transient retries). VALIDATE_RETRIES caps
  // how many distinct butterfly-failed images we'll throw away before
  // giving up and keeping the latest as-is.
  for (let vRound = 0; vRound <= VALIDATE_RETRIES; vRound++) {
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
        console.log(`✓ ${elapsed}s${tok} → ${rel}${vRound > 0 ? ` (regen ${vRound}/${VALIDATE_RETRIES})` : ""}`);
        break;   // out of attempt-loop → fall through to validation
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
    // Validation: PASS / SKIP → keep file; FAIL → loop (next vRound burns a fresh gen).
    if (!VALIDATE_BUTTERFLY) return;
    const v = await validateButterfly(outPath);
    if (v.verdict !== "FAIL") {
      console.log(`  ↻ butterfly · ${v.verdict}${v.reason ? ` (${v.reason})` : ""}`);
      return;
    }
    if (vRound < VALIDATE_RETRIES) {
      console.warn(`  ✗ butterfly FAIL (${v.reason}) — regenerating panel (${vRound + 1}/${VALIDATE_RETRIES})`);
    } else {
      console.warn(`  ✗ butterfly FAIL (${v.reason}) — exhausted ${VALIDATE_RETRIES} regens, keeping latest`);
    }
  }
}

const portraitTail = PORTRAIT ? `\n\n${PORTRAIT_NOTE}`
                   : LANDSCAPE ? `\n\n${LANDSCAPE_NOTE}`
                   : "";

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
progress.update((++donePanels / totalPanels) * 100, { done: donePanels, total: totalPanels });

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
  progress.update((++donePanels / totalPanels) * 100, { done: donePanels, total: totalPanels });
}
progress.end();
