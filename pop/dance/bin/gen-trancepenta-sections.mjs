#!/usr/bin/env node
// dance/bin/gen-trancepenta-sections.mjs — generate the TRANCEPENTA
// storyline panel set for the YOUTUBE landscape visualizer: a 16:9-ish
// (1536×1024) cover + 16 panels (2 per struct section).
//
// Concept (jas, 2026-05-23):
//
//   jeffrey + his small MAGICAL PIXSIES walk into Trader Joe's late
//   afternoon. They shop the aisles together, push a red cart, end
//   at the checkout where young MARK ZUCKERBERG is the cashier. Mark
//   scans their groceries, then spots the iconic PALS laptop tucked
//   under jeffrey's arm. jeffrey casually opens it on the counter and
//   shows it to him. They SHAKE HANDS — jeffrey LEAVES the laptop
//   with Mark as a gift. jeffrey + pixsies walk out and PACK THE CAR
//   in the parking lot. Across the 16 panels the store PROGRESSIVELY
//   DARKENS — late afternoon → dusk → twilight → night → fully dim
//   + dismal by the end. FELT-COZY texture throughout (chunky knits,
//   wool jumpers, soft store textiles, felt-painted aesthetic). The
//   pixsies are SMALLER-THAN-ADULT magical humans with subtle synthetic
//   tells — not super-soldiers, not children, somewhere whimsical.
//
// Output: pop/dance/out/trancepenta-yt-cover.png      (landscape hero)
//         pop/dance/out/trancepenta-yt-sec-<i>-<name>.png  (16 panels)

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
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
const SIZE  = "1536x1024";                  // landscape 3:2 (for 1920x1080 YT)
mkdirSync(`${LANE}/out`, { recursive: true });

// ── identity refs ────────────────────────────────────────────────────
const SHOOT_DIR   = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const ZUCK_DIR    = `${homedir()}/Documents/Shelf/gens/_zuck-refs`;
const TP_REFS_DIR = `${homedir()}/Documents/Shelf/gens/trancepenta-sections/intro/refs`;
const COVER_REF   = `${homedir()}/Documents/Shelf/trancepenta-cover-3000.jpg`;
const REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ZUCK_DIR}/zuck-2005-a.jpg`,
  `${ZUCK_DIR}/zuck-2005-crop.jpg`,
  `${TP_REFS_DIR}/zuck-2007-frick.jpg`,
  `${TP_REFS_DIR}/zuck-2007-goldberg.jpg`,
  `${TP_REFS_DIR}/pals-logo.png`,
  // (released cover ref REMOVED — its multi-element composition was
  // nudging gpt-image-2 toward collage outputs.)
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

// ── LANDSCAPE framing override ───────────────────────────────────────
const LANDSCAPE_NOTE =
`LANDSCAPE OVERRIDE — WIDE horizontal 3:2 / 16:9 frame (NOT a tall portrait or square). pull the camera back so the WHOLE Trader Joe's space breathes around jeffrey + Mark + the pixsies — wide cinematic establishing room shots, the WHOLE aisle / checkout / parking lot visible. figures live in a horizontal mid-band, never cropped tight. keep the felt-cozy texture, the photographic-with-felt-painted feel, and the section's progressive darkening.`;

// ── core scene law ────────────────────────────────────────────────────
// FELT-TABLEAU style (matches the album cover at
// `gens/trancepenta-cover-final/cover-prompt.txt`): the figures are
// crafted-felt puppets placed inside a real photographic environment.
// Aardman / Isle-of-Dogs craft quality, NOT cute, NOT plush. Clothes
// are visibly tattered. Same arc as before (daytime → night), but the
// medium is felt throughout.
const MEDIUM =
`A HAND-CRAFTED FELT TABLEAU — figures alone are CRAFTED-FELT puppets placed inside a real photographic environment. The PHOTO of the store stays photoreal (wood shelves, painted maritime mural, store lights, kraft bags). The FIGURES are MADE OF FELT: visible wool fibre texture on every surface of skin / hair / clothing, slightly fuzzy outlines where each body meets the photographic background, soft 3-D forms sculpted from felt. high-end stop-motion film still craft quality — Aardman-studio / Isle-of-Dogs felt-puppet realism. NOT cartoon, NOT plush-toy, NOT cute, NOT kid's-craft. Editorial peer-horizontality: jeffrey + Mark + the pixsies share an everyday eye-line. NEITHER figure looks at the camera or acknowledges the viewer; everyone absorbed in their task. EVERYTHING IN SHARP FOCUS — no motion blur, no out-of-focus blur, no depth-of-field haze. NO app / phone / UI chrome and NO LEGIBLE laptop or phone screen content (a soft slate-blue colour glow IS allowed; never legible UI / text / icons / wordmarks).`;

const SETTING =
`SETTING — a Trader Joe's with cedar-plank ceiling, wood-panelled shelving stocked with generic goods, hand-lettered chalkboard signs, the painted maritime/island mural on the back wall, buckets of cut flowers, a red shopping cart, brown kraft bags. Daytime business hours but the light progressively DARKENS across the visit — late afternoon at the start, into dusk through the aisles, into twilight + closing-time at the register, into NIGHT + a faintly dismal stillness by the time they're packing the car outside. KEEP ALL PACKAGING + SIGNAGE GENERIC AND ILLEGIBLE — no readable real brand wordmarks anywhere. Mark works the daytime cashier shift; jeffrey + the pixsies are his everyday customers, shopping the aisles and ending at his line.`;

const JEFFREY =
`JEFFREY — a felt-crafted puppet of the real jeffrey from the attached reference photographs: medium-length brown felt-yarn hair, short BEARD sculpted from short cut felt fibres, hazel felt eyes with glossy bead pupils, smooth pale-flesh felt face with subtle cheek-pink. Felt mouth, expressions sculpted with sewn-seam micro-stitches (a slight smile-seam, a faint worry-line seam at the brow). His easygoing everyday-shopper energy. OUTFIT (felt throughout): pale-blue felt long-sleeve button-down with hand-applied darker-blue felt pinstripes, small embroidered YELLOW felt bear emblem low on the chest, a single bright YELLOW Sailor-Pro-Gear FOUNTAIN PEN clipped at the chest pocket — a real fountain pen (NOT felt, NOT a ballpoint): glossy enamel cylindrical barrel in saturated yellow, smooth CAP with gold CLIP, flat-top with knurled grip-ring, cap stays ON, NO brand logo. Soft RED plastic glasses hooked over the placket so they dangle freely against his chest (NO cord, NO chain, NO lanyard). Wide-leg medium-cobalt felt trousers, worn black canvas-felt low-top sneakers. Soft canvas-felt MESSENGER / SHOULDER BAG slung across his body; in earlier beats his small PALS LAPTOP tucked under his arm or peeking from the bag.`;

const ZUCK =
`THE CASHIER = a felt-crafted puppet of a VERY YOUNG mark zuckerberg, age ~20-21, matching the attached real reference photos (zuck-2005-*.jpg + zuck-2007-*.jpg): pale flesh felt face with darker felt FRECKLES across the nose + cheekbones, slight felt overbite at his closed-ish mouth, slightly curly light-brown felt-yarn hair with a clear cowlick + forehead curl, wide-set hazel felt eyes with bead pupils. NOT the modern older Zuckerberg. OUTFIT (felt throughout): navy-blue felt HOODED SWEATSHIRT with a faded HARVARD-style collegiate arch in dark-crimson felt letters (legible but soft-edged), plain blue felt jeans. Cashier touch: a simple Trader Joe's pin-on felt NAME BADGE + a thin LANYARD worn over the hoodie. Working a normal cashier shift — scanning + bagging groceries earnestly. Shy + deadpan, never looks at the camera.`;

const TATTERED =
`★ TATTERED FELT CLOTHES — both jeffrey's + Mark's felt outfits show visible WEAR + DAMAGE rendered in the felt itself:
  · jeffrey's pale-blue felt button-down: small FRAYS at cuffs + collar, a few exposed threads hanging loose, a small TEAR on the right shoulder seam with felt fibres pulled out, the placket slightly ragged. A couple of pinstripe felt-strips have come unstuck and lift slightly off the fabric.
  · Mark's navy felt hooded sweatshirt: a few small HOLES (real felt cutouts) at the hem + on one sleeve, the HARVARD print FADED with worn patches + a couple of letter edges lost, hanging threads at seams, hood drawstring frayed at the ends, a worn patch on one elbow.
  · the pixsies' chunky-knit jumpers + scarves: each pixsie has its own small wear — a loose thread, a frayed cuff, a tiny moth-hole.
The damage is REAL FELT damage (frayed fibres, loose threads, cut-out holes), NOT painted-on dirt or photo-grime. Clean craft execution of distressed clothing — these felt puppets have lived in this story for a while.`;

const LAPTOP =
`THE LAPTOP — EXACTLY ONE laptop in the whole picture: jeffrey's PALS LAPTOP. It is photographic / real (NOT felt — a normal small ac laptop, matte aluminium-feel finish). Match the TRANCEPENTA COVER ART exactly: the lid is a clean coloured panel with the PALS LOGO rendered LARGE + CENTRED + BRIGHT NEON-BLUE BACK-LIT — TWO Keith-Haring-style outline figures (a smaller one + a slightly taller one, side by side, hand-drawn outline, each with a round head, simple body, arms held out) glowing in a saturated cyan/electric-blue against the muted lid colour. The PALS logo is PROMINENT (about 55-65% of the lid width). Use the attached pals-logo.png AS THE REFERENCE for the exact figure shapes — never "JOY" text or any other symbol, NEVER an Apple logo. EARLY beats: laptop closed under arm or in bag. CHECKOUT-onward: open on the counter with a soft cool slate-blue screen glow spilling on jeffrey + Mark's felt faces from below; screen content UNREADABLE. LATE beats: jeffrey leaves the open laptop on the counter for Mark. The laptop is the only thing in the frame that is UNDAMAGED (only the clothes are tattered — the PALS machine is intact).`;

const PIXSIES =
`THE PIXSIES — small felt-crafted characters around jeffrey, ~3/4 to 4/5 his height, felt puppets like him + Mark, with MORE PROMINENT uncanny tells than regular humans: the LED beads under their felt skin glow noticeably — a clear pinprick of cyan-green light at a temple, behind an ear, in an eye — one or two hairline seams visible on faces, slight off-human proportion in the heads. NATURAL HUMAN ears — never pointed, never elf, never animal. Wardrobe (felt throughout): cozy felt + wool — chunky-knit jumpers in muted earth tones, soft felted beanies, felt scarves, small canvas-felt bags. Each pixsie wears something slightly tattered (see TATTERED FELT CLOTHES). They are jeffrey's everyday companions out shopping with him — pushing the red cart, carrying brown kraft bags, browsing flowers, helping at the counter, packing the car. Quietly curious, gently uncanny, content. NOT super-soldiers, NOT children, NOT cute mascots — small uncanny felt-puppet adults.`;

const AVOID =
`AVOID — photoreal skin or hair on the figures (the figures are FELT — wool fibre texture, fuzzy meet-edges with the photo backdrop); cartoonish / kid's-craft / plush-toy look; cinematic-glossy or neon-poster mood; 3D-render look; ANY motion blur or out-of-focus / depth-of-field blur (everything sharp); pointed elf / animal ears on pixsies; CHILD-SIZED or "cute mascot" pixsies; ADULT super-soldier-bodybuilder pixsies; any "JOY" text on the laptop lid; any readable wordmark / typography on the laptop lid (only the PALS two-figure logo); an Apple / Dell / brand logo on any device; readable real-world brand wordmarks anywhere in the store; ANY app/phone/UI chrome; legible laptop screen content (only diffuse slate-blue glow); a second laptop anywhere; the modern older Zuckerberg (he is YOUNG, early-Facebook era); hero-pose centering; jeffrey or Mark looking at the camera; CLEAN-SHAVEN jeffrey (he must have the short felt beard); ANY damage on the laptop itself (only the clothes are tattered).`;

// ── 16 VIZ BEATS — 2 per struct section ─────────────────────────────
// Names map: <struct_section>-<a|b>. Renderer's SLIDES array maps each
// beat to a half-section time window. Progressive darkening across the
// arc: late-afternoon (intro) → dusk (build1) → twilight (drop1) →
// night (drop2) → fully dim & dismal (outro).
const SECTION_ORDER = [
  "intro-a",    "intro-b",
  "break1-a",   "break1-b",
  "build1-a",   "build1-b",
  "drop1-a",    "drop1-b",
  "break2-a",   "break2-b",
  "build2-a",   "build2-b",
  "drop2-a",    "drop2-b",
  "outro-a",    "outro-b",
];

const SECTION_VARIANTS = {
  // INTRO — late afternoon arrival
  "intro-a":
`BEAT 1 (intro-a) · "approach" · LATE AFTERNOON warm light. OUTSIDE the front of the open Trader Joe's, golden hour. jeffrey + four-or-five small magical pixsies walking up the terracotta-tiled entryway. jeffrey has the PALS laptop tucked closed under one arm. soft cozy felt vibes on their clothing — chunky-knit jumpers + felted hats. the painted "TRADER" mural visible above the sliding doors, just opening for them.`,
  "intro-b":
`BEAT 2 (intro-b) · "into the store" · just-past-golden-hour warm interior. they have STEPPED THROUGH the doors. jeffrey + pixsies inside the entryway, taking in the store. one pixsie reaches for a red shopping cart. warm overhead lights, cedar-plank ceiling visible above. felted store textiles, an air of cozy bustle. light just starting to soften.`,
  // BREAK1 — produce / flowers, late afternoon turning dim
  "break1-a":
`BEAT 3 (break1-a) · "the flowers" · soft late-afternoon light cooling. jeffrey + a few pixsies at the cut-flower buckets. one pixsie selecting a small bunch of dahlias with cozy concentration, another tucking flowers into the red cart. soft felted scarves wrapped around small necks. the store light a touch dimmer than the entry beat — clouds outside, day going.`,
  "break1-b":
`BEAT 4 (break1-b) · "produce" · light dimming further toward early dusk. jeffrey + pixsies in the produce section, picking apples + bagging citrus + selecting bread. cart is filling. the chunky-knit jumpers + wool textures vivid in the warm-cool mix of fluorescent + window light. laptop still tucked under jeffrey's arm.`,
  // BUILD1 — snack aisle, dusk
  "build1-a":
`BEAT 5 (build1-a) · "the snack aisle" · DUSK now, the warm window light tipping cool. deeper into the store — a snack / dry-goods aisle. jeffrey + pixsies pulling boxes off shelves into the cart. one pixsie compares two boxes thoughtfully, another piles snacks into the cart. interior lights stronger as outside dims.`,
  "build1-b":
`BEAT 6 (build1-b) · "wine + drinks" · twilight outside, store amber inside. moving through a beverage aisle — pixsies handling boxed drinks, a small bottle of olive oil, a tin of nuts. cart full. jeffrey reaches for a tall bottle on a high shelf, laptop secured under the other arm.`,
  // DROP1 — at checkout, twilight
  "drop1-a":
`BEAT 7 (drop1-a) · "the line" · early TWILIGHT outside, store overhead lights take over. jeffrey + pixsies arrive at the checkout. Mark (young Zuckerberg, name badge + lanyard) stands at his register actively SCANNING the first items pulled from the cart. jeffrey waits at the counter end with the laptop still tucked under his arm. pixsies hover at the cart helping unload onto the conveyor belt.`,
  "drop1-b":
`BEAT 8 (drop1-b) · "scanning groceries" · twilight outside fully cool blue. Mark continues scanning — a box of crackers, a bunch of bananas — bagging into brown kraft sacks. jeffrey patient, half-watching. pixsies organizing the next pile of groceries onto the belt, their felted gear softly visible.`,
  // BREAK2 — Mark notices the laptop, dim store evening
  "break2-a":
`BEAT 9 (break2-a) · "the spot" · early NIGHT outside (deep blue), store overheads only. mid-scan Mark has PAUSED — a box of crackers half-raised in one hand — because he just SPOTTED the small PALS LAPTOP under jeffrey's arm. eyebrows up, curious half-question forming. jeffrey's noticed Mark noticing — soft half-smile, lifts his arm so Mark can see the PALS logo clearly.`,
  "break2-b":
`BEAT 10 (break2-b) · "the question" · the store dimmer still, work-lights pooling. Mark sets the crackers down, leans forward over the register asking softly about the laptop. jeffrey nods, starts to slide it from under his arm. the pixsies pause their bagging, glancing over with quiet interest. small felted hands stilled.`,
  // BUILD2 — the reveal, dim
  "build2-a":
`BEAT 11 (build2-a) · "the reveal" · NIGHT outside, the store growing distinctly dim except for a single overhead spot over the counter. jeffrey has SLID THE PALS LAPTOP OUT and is OPENING it on the counter beside the bagged groceries. cool slate-blue screen-spill catches under his jaw + on Mark's pale face. Mark leans across the register, transfixed. the cyan glow is THE PRIMARY light now.`,
  "build2-b":
`BEAT 12 (build2-b) · "explaining" · the only light now is the cool laptop slate-blue + a faint overhead. dimmer + slightly dismal around them — the store closing in around the counter. jeffrey mid-explanation, pointing at the keyboard, gesture mid-air. Mark concentrated, brow up. pixsies cluster closer to watch, their felted shapes silhouetted in the cyan light.`,
  // DROP2 — the handshake + gift, nearly dark
  "drop2-a":
`BEAT 13 (drop2-a) · "the handshake" · the store NEARLY DARK around them — only the laptop's slate-blue spill + a low overhead pool. jeffrey + Mark SHAKING HANDS across the counter, a firm friendly handshake. the laptop sits open between them, glowing cyan. groceries bagged in kraft sacks around them. a moment of real exchange.`,
  "drop2-b":
`BEAT 14 (drop2-b) · "leaving the laptop" · same near-dark, the cyan glow dominant. jeffrey is SLIDING THE OPEN PALS LAPTOP across the counter TOWARD Mark, leaving it as a gift. Mark's hand reaching to accept it. the PALS logo softly visible back-glowing on the lid. pixsies stand by approvingly, faintly lit by the cyan. quiet warmth.`,
  // OUTRO — packing the car at night, dismal
  "outro-a":
`BEAT 15 (outro-a) · "to the car" · OUTSIDE now in the parking lot, FULLY NIGHT, a dim parking-lot lamppost the only light. jeffrey + pixsies walking from the store to the parked car (small hatchback / station wagon), kraft bags in arms + in the red cart. a thin dismal stillness in the air — the store warm behind them but small in the distance.`,
  "outro-b":
`BEAT 16 (outro-b) · "packing" · same NIGHT, same dim lamppost. they are PACKING the car — tailgate open, pixsies handing brown kraft bags into the trunk + arranging flowers carefully + folding the last sack inside. jeffrey lifts the last bag in. through the store window in the far distance you can JUST see Mark at his register hunched over the open PALS laptop, a small cyan glow. a contented but slightly dismal night close.`,
};

function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}
// SINGLE-SCENE PREAMBLE — explicit anti-collage instruction first,
// because gpt-image-2 was reading the long multi-paragraph prompt as a
// request to compose a 9-panel storyboard of the whole arc into ONE
// image. This forces it to render a SINGLE cinematic frame.
const SINGLE_FRAME =
`A SINGLE LANDSCAPE PHOTOGRAPH — ONE coherent cinematic frame, ONE moment in time, ONE composition. NOT a collage. NOT a comic strip. NOT a multi-panel grid. NOT a storyboard. NOT split-screen. The entire image is ONE continuous scene captured from ONE camera position at ONE moment.`;
function build(sectionBeat) {
  return [SINGLE_FRAME, LANDSCAPE_NOTE, sectionBeat, MEDIUM, SETTING, JEFFREY, ZUCK, TATTERED, LAPTOP, PIXSIES, AVOID].join("\n\n") + "\n";
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

const onlySet = typeof flags.only === "string"
  ? new Set(flags.only.split(",").map((x) => x.trim().toLowerCase()))
  : null;
const wants = (name) => !onlySet || onlySet.has(name);

const jobs = [];
// Cover = the build2-a "reveal" moment (matches the released cover composition).
if (wants("cover"))
  jobs.push({ prompt: build(SECTION_VARIANTS["build2-a"]), out: `${LANE}/out/trancepenta-yt-cover.png`, label: "trancepenta-yt cover" });
for (let i = 0; i < SECTION_ORDER.length; i++) {
  const name = SECTION_ORDER[i];
  if (!wants(name)) continue;
  jobs.push({
    prompt: build(SECTION_VARIANTS[name]),
    out: `${LANE}/out/trancepenta-yt-sec-${i}-${safeName(name)}.png`,
    label: `trancepenta-yt §${i} ${name}`,
  });
}

progress.begin({ type: "illy", label: `trancepenta-yt · ${jobs.length} panels` });
let done = 0;
// Concurrency 3 — gpt-image-2 server-side is per-request anyway, and 8 GB
// machine can hold three in-flight HTTPS uploads + base64 decodes.
const POOL = 3;
let cursor = 0;
async function worker() {
  while (true) {
    const i = cursor++;
    if (i >= jobs.length) return;
    const job = jobs[i];
    await generate(job.prompt, job.out, job.label);
    progress.update((++done / jobs.length) * 100);
  }
}
await Promise.all(Array.from({ length: POOL }, () => worker()));
progress.end();
console.log(`\n✓ trancepenta YT panel set — ${jobs.length} job(s) · 16 beats · the long blue hum`);
