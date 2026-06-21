#!/usr/bin/env node
// gen-felt.mjs — regenerate the Restless Egg video stills as needle-felted
// wool dioramas (momabobasheep aesthetic: pop/momboba/momabobasheep.illy.txt),
// grounded in jeffrey's real reference photos via gpt-image-2 /v1/images/edits.
//
// Brief (from @jeffrey): brighter, open-plan FELT office (not the dark studio),
// felt-jeffrey actively PLAYING notepat, real clothing/face from the refs.
//
// Each beat = SHARED style preamble + DOLL block + a per-beat scene line.
// Landscape 1536x1024 to match the 16:9 video cards.
//
// Usage:
//   node gen-felt.mjs                 # gen all missing beats
//   node gen-felt.mjs --only instrument   # one beat
//   node gen-felt.mjs --force         # regen all
//   node gen-felt.mjs --only commons --force

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");
const OUT = `${HERE}/felt`;
mkdirSync(OUT, { recursive: true });

const argv = process.argv.slice(2);
const FORCE = argv.includes("--force");
const onlyIdx = argv.indexOf("--only");
const ONLY = onlyIdx >= 0 ? argv[onlyIdx + 1] : null;
const SIZE = "1536x1024";

const SHOOT_DIR = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
const REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
].filter((p) => existsSync(p) || (console.warn(`  ⚠ ref missing: ${p}`), false));

const STYLE =
  "a real photograph of a hand-crafted needle-felt diorama, landscape — " +
  "shot straight on like a bright, clean documentation photo of a miniature " +
  "scene. visible wool fibers everywhere, fuzzy soft edges, tiny felting " +
  "imperfections that prove human hands. NOT a 3d render, NOT clay, NOT " +
  "plastic — soft matted wool throughout. crisp focus, BRIGHT airy natural " +
  "daylight, light and cheerful, never dark or moody.";

const DOLL =
  "the central figure is a needle-felted wool doll of the man in the " +
  "reference photos — recognizably HIM in doll form, about 30, medium-length " +
  "tousled brown wool hair, short brown wool beard, soft red felt glasses, " +
  "warm friendly felted face clearly visible and never cropped. his doll " +
  "wears a tiny felt version of the ACTUAL clothing he is wearing in the " +
  "reference photos — match the real clothes faithfully in felt, do not " +
  "invent new garments.";

const OFFICE =
  "the setting is a bright, open-plan modern office built entirely from felt: " +
  "pale wood-toned felt floor, tall felt windows flooding warm daylight, a " +
  "few simple felt desks, a leafy green felt plant, soft white felt walls. " +
  "airy, open, and light.";

const NOTEPAT =
  "a tiny felt laptop, OPEN as a properly hinged wedge — the lit screen on " +
  "the inner face, the keyboard base receding below it; the outer lid back is " +
  "plain blank felt with NO screen and no glow on it. the screen is a felted " +
  "grid of bright candy-colored notepat note-tiles with a little wool " +
  "sound-waveform — the real software instrument, lovingly rendered in wool.";

const TAIL =
  "no text, no wordmarks, no readable logos, no signage anywhere. no other " +
  "real people. soft felt shadows. tender and a little playful. " +
  "LAPTOP GEOMETRY (CRITICAL): a laptop has exactly ONE screen, on the INNER " +
  "face. COMPOSE every laptop from the FRONT or SIDE so its glowing screen is " +
  "angled toward the camera. NEVER show the outside/back of a laptop lid bearing " +
  "a screen, UI, tiles, or glow — the lid back is plain blank felt. If a laptop " +
  "would face away from the camera, draw it CLOSED and FLAT (no screen) or omit " +
  "it. No device has a second screen on the wrong surface.";

// Appended ONLY when a beat carries real-UI screenshot refs. Pushes gpt-image
// to reproduce the actual on-screen interface (not invent a generic one).
const SCREEN_NOTE =
  "CRITICAL — THE LAPTOP/DEVICE SCREEN: the lit screen must faithfully " +
  "reproduce the EXACT interface shown in the attached app-screenshot " +
  "reference image(s) — the same arrangement of colored tiles/blocks, the " +
  "same colors, the same on-screen layout and proportions — only re-rendered " +
  "softly in wool felt. Replicate THAT specific real interface closely; do " +
  "NOT invent a different, generic, or abstract screen. It should be " +
  "unmistakably the same software, just felted.";

// Per-beat scene line (the action). Keep one clear felt-jeffrey doll per scene.
const BEATS = {
  painter:
    "SCENE: felt-jeffrey sits on a simple wool chair in the bright open felt " +
    "office with a small CHARTREUSE citrus-green felt MacBook (his 'MacBook " +
    "Neo') open on his lap, screen facing him. BOTH his little wool hands are " +
    "spread across the keyboard like a pianist — left hand on the left half, " +
    "right hand on the right — genuinely playing. he is SINGING along, mouth " +
    "open mid-note, head tipped toward the screen, absorbed and joyful with a " +
    "small private half-smile (echoing the keymaps-paper drawing of him). the " +
    "laptop screen is a bright felted grid of candy-colored notepat note-tiles " +
    "with a thin wool waveform strip. NO paintbrush, no painting tools anywhere.",
  instrument:
    "SCENE: close, warm three-quarter view of felt-jeffrey actively PLAYING " +
    NOTEPAT + " his little felt hands pressed on the laptop keys mid-chord, " +
    "delighted, leaning into the music. bright daylight from a big felt window " +
    "behind him. this is the hero shot: a wool man joyfully playing his instrument.",
  boot:
    "SCENE: felt-jeffrey leans over a salvaged felt ThinkPad-style laptop on a " +
    "felt desk and slots a tiny pastel felt USB stick into its side; the screen " +
    "blooms to life with " + NOTEPAT + " warm light spilling onto his happy face. " +
    "the bright open felt office around him.",
  commons:
    "SCENE: pull back to show the bright open felt office full of MANY needle-felt " +
    "MAKERS — the pixsies: a wonderfully diverse felt crew of all ages, races and " +
    "genders in eclectic clashed felt wardrobe (felt glasses, beanies, cardigans, " +
    "little tactical-vests), bead eyes, with tiny cyan-green LED beads glowing " +
    "softly UNDER their felt skin at a temple or ear, rounded human ears. each " +
    "sits at a felt desk with a small open felt laptop, its lit screen on the " +
    "inner face showing a different bright little program. among them, as " +
    "same-eye-line peers (not villains), sit two felt FOILS: an older grey-haired " +
    "felt Bill Gates in a muted sage-green felt crewneck with a deep-red felt pen " +
    "at his collar, and a young felt Mark Zuckerberg with curly light-brown " +
    "felt-yarn hair and a navy felt hoodie with a faded crimson arch. felt-jeffrey " +
    "stands in the lower foreground, three-quarter turned so his face shows, warm " +
    "and proud. soft wool threads of light connect the screens.",
  invitation:
    "SCENE: felt-jeffrey stands in the bright open felt office having just set " +
    NOTEPAT + " down on a clean felt desk, and turns toward us with a hopeful, " +
    "open half-smile — offering the little glowing instrument to the viewer. an " +
    "invitation, generous and warm.",
  synth:
    "SCENE: an extreme close-up of just the felt laptop screen filling the whole " +
    "frame — a big beautiful felted control panel of MANY bright candy-colored " +
    "notepat voice-tiles in neat rows (dozens of little wool squares, like a " +
    "felted synthesizer with 128 voices) and a soft wool sound-waveform across " +
    "the bottom. felt-jeffrey's wool fingertips rest at the lower edge of the " +
    "keys. no face needed. bright clean daylight, crisp wool detail.",
  close:
    "SCENE: a single hero felted laptop, open and glowing softly with a bright " +
    "felted grid of notepat note-tiles, sitting small and centered-LOW on a " +
    "clean pale wool surface in the bright open felt office. generous empty " +
    "bright felt space fills the upper two-thirds of the frame — a calm, minimal " +
    "product-hero shot of the little instrument on its own. no figure. soft " +
    "daylight, gentle felt shadow beneath it.",
  anywhere:
    "SCENE: felt-jeffrey holds a tiny felt smartphone in one raised hand while " +
    "his other hand rests on " + NOTEPAT + " on the desk — the SAME bright " +
    "felted notepat tile-grid screen glows on BOTH the little phone and the " +
    "laptop, showing the instrument runs everywhere. he smiles, delighted. " +
    "bright open felt office.",
  prompt:
    "SCENE: felt-jeffrey sits at a felt laptop whose screen shows the Aesthetic " +
    "Computer command PROMPT — a calm felted text screen with a couple of small " +
    "felt buttons and a blinking cursor. he's leaning in about to type, curious " +
    "and inviting. bright open felt office.",
  "kidlisp-roz":
    "SCENE: felt-jeffrey leans in delighted at a felt desk where the felt laptop " +
    "screen blooms with a swirling, colorful generative artwork — soft wool " +
    "spirals of pink, blue and lilac color. a six-line little program made into " +
    "a whole world. bright open felt office.",
  "kidlisp-ger":
    "SCENE: felt-jeffrey at a felt laptop whose screen is alive with vivid " +
    "streaks of colored light over a warm field — a different little generative " +
    "program. he gestures at it, as if to say anyone can make this. bright open " +
    "felt office.",
  chat:
    "SCENE: the NELA Computer Club — a cozy felt cinema with deeply saturated " +
    "SAGE-GREEN velvet seats, walls and carpet, washed in a riot of multi-colored " +
    "screen-light. rows of needle-felt makers (the pixsies — diverse, eclectic " +
    "felt wardrobe, bead eyes, a faint cyan-green LED glow under the felt) fill " +
    "the seats; the front rows cradle small open felt laptops showing different " +
    "bright little programs, while fashionable felt LA folks behind them hold " +
    "tiny felt snacks and tallboys. felt-jeffrey sits among them, off-center, " +
    "just another member, smiling up toward a big glowing felt screen. warm " +
    "brass-sconce gold. a place, not an app.",
};

// Per-beat REAL aesthetic.computer UI screenshots, appended as extra image[]
// refs AFTER the jeffrey identity refs, so gpt-image-2 keys the felt laptop
// SCREENS to the actual software (notepat tile grid + waveform, KidLisp pieces).
const CAPTURES = `${REPO}/marketing/captures/platter`;
const SCREENS = {
  painter: [`${CAPTURES}/notepat.png`],
  instrument: [`${CAPTURES}/notepat.png`],
  boot: [`${CAPTURES}/notepat.png`],
  synth: [`${CAPTURES}/notepat.png`],
  anywhere: [`${CAPTURES}/notepat.png`],
  invitation: [`${CAPTURES}/notepat.png`],
  close: [`${CAPTURES}/notepat.png`],
  prompt: [`${CAPTURES}/prompt.png`],
  "kidlisp-roz": [`${CAPTURES}/kidlisp-roz.png`],
  "kidlisp-ger": [`${CAPTURES}/kidlisp-ger.png`],
  // group/ensemble scenes get MULTIPLE screen-refs so the many laptops show variety
  chat: [
    `${CAPTURES}/notepat.png`,
    `${CAPTURES}/kidlisp-roz.png`,
    `${CAPTURES}/kidlisp-ger.png`,
    `${CAPTURES}/laklok.png`,
  ],
  commons: [
    `${CAPTURES}/notepat.png`,
    `${CAPTURES}/kidlisp-roz.png`,
    `${CAPTURES}/kidlisp-ger.png`,
    `${CAPTURES}/laklok.png`,
    `${CAPTURES}/prompt.png`,
  ],
};

function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  for (const line of readFileSync(vault, "utf8").split("\n")) {
    if (line.startsWith("OPENAI_API_KEY=")) {
      return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
    }
  }
  throw new Error("OPENAI_API_KEY not found");
}

const apiKey = loadKey();

// ── post-image QA (vision) ───────────────────────────────────────────────
// gpt-image-2 stubbornly paints screen content on laptop lid-BACKS. Defensive
// prompting (TAIL) reduces it; this catches what slips through. After each gen
// a vision model inspects the image; on a fail we regenerate with the reason
// fed back. Disable with --no-qa; tune rounds with --qa-retries N.
const QA = !argv.includes("--no-qa");
const qaIdx = argv.indexOf("--qa-retries");
const QA_RETRIES = qaIdx >= 0 ? parseInt(argv[qaIdx + 1], 10) : 2;
const QA_MODEL = "gpt-4o";
const GEOMETRY_FIX =
  "Render EVERY laptop from the front or side with its screen angled toward the " +
  "camera. The OUTSIDE/BACK of every laptop lid is plain blank felt with " +
  "absolutely no screen, tiles, glow, or interface on it. Any laptop facing away " +
  "is closed flat or omitted.";

async function qaImage(b64) {
  const body = {
    model: QA_MODEL,
    messages: [{
      role: "user",
      content: [
        { type: "text", text:
          "This is a needle-felt diorama. Inspect EVERY laptop/computer. Does ANY " +
          "laptop show actual SCREEN CONTENT — a recognizable UI, colored " +
          "note-tiles, a waveform, or a lit display image — on the OUTSIDE/BACK " +
          "of its lid (the side facing away from its user)? IMPORTANT: ambient " +
          "colored room-light, glow, or reflections on felt surfaces are FINE " +
          "and are NOT bleed — only flag a genuine screen IMAGE rendered on a " +
          "lid back. Reply with ONLY compact JSON: {\"bleed\": true|false, " +
          "\"reason\": \"short\"}." },
        { type: "image_url", image_url: { url: `data:image/png;base64,${b64}` } },
      ],
    }],
    max_tokens: 150,
  };
  try {
    const r = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
    if (!r.ok) { console.warn(`  ⚠ QA skipped (HTTP ${r.status})`); return { pass: true }; }
    const j = await r.json();
    const txt = j.choices?.[0]?.message?.content || "{}";
    const m = txt.match(/\{[\s\S]*\}/);
    const v = m ? JSON.parse(m[0]) : {};
    return { pass: !v.bleed, reason: v.reason || "" };
  } catch (e) {
    console.warn(`  ⚠ QA error (${e.message}) — passing`);
    return { pass: true };
  }
}

// One image request (FormData + transient-network retry). Returns b64 PNG.
async function requestImage(promptText, refs) {
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", promptText);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of refs) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  let res, lastErr;
  for (let attempt = 1; attempt <= 4; attempt++) {
    try {
      res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
      });
      break;
    } catch (e) {
      lastErr = e;
      console.warn(`  ⚠ net attempt ${attempt} failed (${e?.cause?.code || e.message}); retrying…`);
    }
  }
  if (!res) { console.error(`✗ network failed after retries: ${lastErr?.message}`); process.exit(1); }
  if (!res.ok) { console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 500)}`); process.exit(1); }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) { console.error(`✗ no image: ${JSON.stringify(json).slice(0, 280)}`); process.exit(1); }
  return b64;
}

function buildPrompt(beat) {
  const screenNote = (SCREENS[beat] || []).length ? `\n\n${SCREEN_NOTE}` : "";
  return `${STYLE}\n\n${OFFICE}\n\n${DOLL}\n\n${BEATS[beat]}\n\n${TAIL}${screenNote}`;
}

async function gen(beat) {
  const out = `${OUT}/felt-${beat}.png`;
  if (existsSync(out) && !FORCE) {
    console.log(`✓ cached felt-${beat}.png (use --force)`);
    return;
  }
  const basePrompt = buildPrompt(beat);
  writeFileSync(`${OUT}/felt-${beat}.prompt.txt`, basePrompt);
  // Real AC UI screenshots for this beat's laptop screen (appended after REFS).
  const screens = (SCREENS[beat] || []).filter(
    (p) => existsSync(p) || (console.warn(`  ⚠ screen missing: ${p}`), false),
  );
  // gpt-image-2 edits accepts ~10 image refs; keep screen variety, trim jeffrey
  // identity refs to fit (always keep at least 3 for identity).
  const jeffN = Math.max(3, 10 - screens.length);
  const refs = [...REFS.slice(0, jeffN), ...screens];
  // sidecar so ShotWizard can show the exact inputs (prompt + refs) per shot
  writeFileSync(`${OUT}/felt-${beat}.meta.json`, JSON.stringify({
    beat, prompt: basePrompt, jeffreyRefs: REFS.slice(0, jeffN), screens,
    model: "gpt-image-2", size: SIZE,
  }, null, 2));
  const rounds = QA ? QA_RETRIES + 1 : 1;
  console.log(`▸ felt-${beat} · ${SIZE} · ${jeffN} jeffrey + ${screens.length} screens · QA ${QA ? "on" : "off"}`);
  const t0 = Date.now();
  let best = null;
  for (let round = 1; round <= rounds; round++) {
    // After a QA rejection, feed the reason back + restate the geometry fix.
    const prompt = round === 1 ? basePrompt
      : `${basePrompt}\n\nPREVIOUS ATTEMPT REJECTED — ${best.reason || "screen on a lid back"}. ${GEOMETRY_FIX}`;
    const b64 = await requestImage(prompt, refs);
    if (!QA) { best = { b64, reason: "" }; break; }
    const v = await qaImage(b64);
    best = { b64, reason: v.reason };
    if (v.pass) { console.log(`  ✓ QA pass (round ${round})`); break; }
    console.warn(`  ✗ QA fail (round ${round}): ${v.reason}${round < rounds ? " — regenerating" : " — keeping best effort"}`);
  }
  writeFileSync(out, Buffer.from(best.b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → felt/felt-${beat}.png`);
}

const beats = ONLY ? [ONLY] : Object.keys(BEATS);
for (const b of beats) {
  if (!BEATS[b]) { console.error(`unknown beat: ${b}`); process.exit(1); }
  await gen(b);
}
