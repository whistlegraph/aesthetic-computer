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
  "real people. soft felt shadows. tender and a little playful.";

// Per-beat scene line (the action). Keep one clear felt-jeffrey doll per scene.
const BEATS = {
  painter:
    "SCENE: felt-jeffrey sits at a felt desk in the bright open office, a " +
    "painter's posture, a tiny felt paintbrush resting in one hand — but his " +
    "'canvas' is " + NOTEPAT + " glowing softly in front of him. a small tidy " +
    "stack of two or three closed felt laptops rests beside the desk. he looks " +
    "down at the glowing screen, curious and warm.",
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
    "SCENE: pull back to show the open felt office filled with many tiny felt " +
    "laptops on felt desks, each an open hinged wedge with its lit screen on the " +
    "inner face turned toward the viewer, showing a different bright little " +
    "felted program (grids, dots, shapes, waveforms) — a cheerful felt community " +
    "of screens, soft wool threads of light loosely connecting them. any laptop " +
    "seen from behind shows only a plain blank felt lid back with NO screen and " +
    "no glow. felt-jeffrey stands among them in the lower foreground, " +
    "three-quarter turned so his face shows, looking out over the room, proud " +
    "and warm.",
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
  invitation: [`${CAPTURES}/notepat.png`],
  close: [`${CAPTURES}/notepat.png`],
  commons: [
    `${CAPTURES}/notepat.png`,
    `${CAPTURES}/kidlisp-roz.png`,
    `${CAPTURES}/kidlisp-ger.png`,
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

function buildPrompt(beat) {
  return `${STYLE}\n\n${OFFICE}\n\n${DOLL}\n\n${BEATS[beat]}\n\n${TAIL}`;
}

async function gen(beat) {
  const out = `${OUT}/felt-${beat}.png`;
  if (existsSync(out) && !FORCE) {
    console.log(`✓ cached felt-${beat}.png (use --force)`);
    return;
  }
  const prompt = buildPrompt(beat);
  writeFileSync(`${OUT}/felt-${beat}.prompt.txt`, prompt);
  // Real AC UI screenshots for this beat's laptop screen (appended after REFS).
  const screens = (SCREENS[beat] || []).filter(
    (p) => existsSync(p) || (console.warn(`  ⚠ screen missing: ${p}`), false),
  );
  console.log(`▸ felt-${beat} · ${SIZE} · ${REFS.length} refs · ${screens.length} screens`);
  const t0 = Date.now();
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", prompt);
  fd.append("size", SIZE);
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of [...REFS, ...screens]) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  // Retry on transient network failures (this machine intermittently ETIMEDOUTs
  // on the large multipart upload to OpenAI).
  let res, lastErr;
  for (let attempt = 1; attempt <= 4; attempt++) {
    try {
      res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST",
        headers: { Authorization: `Bearer ${apiKey}` },
        body: fd,
      });
      break;
    } catch (e) {
      lastErr = e;
      console.warn(`  ⚠ attempt ${attempt} failed (${e?.cause?.code || e.message}); retrying…`);
    }
  }
  if (!res) {
    console.error(`✗ network failed after retries: ${lastErr?.message}`);
    process.exit(1);
  }
  if (!res.ok) {
    console.error(`✗ OpenAI ${res.status}: ${(await res.text()).slice(0, 500)}`);
    process.exit(1);
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) {
    console.error(`✗ no image: ${JSON.stringify(json).slice(0, 280)}`);
    process.exit(1);
  }
  writeFileSync(out, Buffer.from(b64, "base64"));
  console.log(`✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → felt/felt-${beat}.png`);
}

const beats = ONLY ? [ONLY] : Object.keys(BEATS);
for (const b of beats) {
  if (!BEATS[b]) { console.error(`unknown beat: ${b}`); process.exit(1); }
  await gen(b);
}
