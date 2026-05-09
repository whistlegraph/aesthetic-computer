#!/usr/bin/env node
// jeffrey-photos.mjs — pre-pipeline step. For each segment in the audience
// config that has a `metaphor` field, call OpenAI gpt-image-2 (images.edit)
// with the platter SHOOT_REFS + SELFIE_REFS for identity grounding and save
// the result to recap/out/jeffrey-photos/<segment>.png.
//
// Each successful generation is also archived into the jeffrey platter as a
// new entry in the `gens` bucket: a dated copy of the PNG goes to
// system/public/assets/jeffreys/gens/<context>-<segment>-<ts>.png (synced to
// the CDN by `npm run assets:sync:up`), and an item with full provenance
// (model, refs, prompt, context, segment, timestamp) is appended to
// papers/jeffrey-platter/manifest.json under buckets.gens.items.
//
// Caching: skip if the working pipeline cache already exists; pass --force to
// regen. Run a single segment by name: --only 02_menuband.
//
// Usage:
//   node bin/jeffrey-photos.mjs jeffrey-24h
//   node bin/jeffrey-photos.mjs jeffrey-24h --force
//   node bin/jeffrey-photos.mjs jeffrey-24h --only 04_platter

import { readFileSync, writeFileSync, mkdirSync, existsSync, statSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");

const audienceName = process.argv[2] || "jeffrey-24h";
const force = process.argv.includes("--force");
const onlyIdx = process.argv.indexOf("--only");
const only = onlyIdx >= 0 ? process.argv[onlyIdx + 1] : null;

const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);

const PHOTOS_DIR = `${ROOT}/out/jeffrey-photos`;
mkdirSync(PHOTOS_DIR, { recursive: true });

// Platter archive — a copy of every successful gen goes here, plus a manifest
// entry. The dir is synced to assets.aesthetic.computer/jeffreys/gens/ via
// `npm run assets:sync:up`.
const PLATTER_GENS_DIR = `${REPO}/system/public/assets/jeffreys/gens`;
const PLATTER_MANIFEST = `${REPO}/papers/jeffrey-platter/manifest.json`;
mkdirSync(PLATTER_GENS_DIR, { recursive: true });

// Mirrors generate-neo.py refs.
// 2k-downscaled shoot refs — full-size JPGs (~24MB each) reliably tripped
// gpt-image-2's multipart upload with "fetch failed" at the connection
// layer; these are 2048px-wide, ~0.5MB each, identity-grounding still
// works fine. Generate with:
//   ffmpeg -i shoot/<name>.jpg -vf "scale='min(2048,iw)':-1" -q:v 3 shoot-2k/<name>.jpg
const SHOOT_DIR = `${REPO}/portraits/jeffrey/corpus/shoot-2k`;
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
const REFS = [...SHOOT_REFS, ...SELFIE_REFS].filter((p) => {
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

const apiKey = loadOpenAIKey();

const MODEL = "gpt-image-2";
const SIZE = "1024x1536";
const QUALITY = "high";

async function generate(metaphor, outPath, extraRefs = []) {
  const fd = new FormData();
  fd.append("model", MODEL);
  fd.append("prompt", metaphor);
  fd.append("size", SIZE);
  fd.append("quality", QUALITY);
  fd.append("n", "1");
  // Identity refs first (jeffrey's face/wardrobe), then per-segment extras.
  // Extras are scene-specific assets the prompt explicitly references —
  // e.g. the actual VFC painting for slide 10, real AC screenshots for
  // chapters that need a UI in shot. Each extra is identified by its
  // FILE NAME in the multipart upload so the prompt can reference it.
  const allRefs = [...REFS, ...extraRefs];
  for (const ref of allRefs) {
    if (!existsSync(ref)) {
      console.warn(`  ⚠ extra ref missing, dropping: ${ref}`);
      continue;
    }
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
    throw new Error(`OpenAI ${res.status}: ${err.slice(0, 500)}`);
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) throw new Error(`no image returned: ${JSON.stringify(json).slice(0, 200)}`);
  writeFileSync(outPath, Buffer.from(b64, "base64"));
  const usage = json.usage || {};
  return { tokens_in: usage.input_tokens, tokens_out: usage.output_tokens };
}

function isoStamp() {
  const d = new Date();
  const pad = (n) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}T${pad(d.getHours())}${pad(d.getMinutes())}`;
}

// Archive a successful gen into the platter: copy PNG to the assets/gens dir
// + append a metadata entry to papers/jeffrey-platter/manifest.json. Lazy-
// initializes the `gens` bucket if it doesn't exist yet.
function archiveToPlatter({ segName, metaphor, sourcePath, context }) {
  const stamp = isoStamp();
  const archiveName = `${context}-${segName}-${stamp}.png`;
  const archivePath = `${PLATTER_GENS_DIR}/${archiveName}`;
  writeFileSync(archivePath, readFileSync(sourcePath));

  const manifest = JSON.parse(readFileSync(PLATTER_MANIFEST, "utf8"));
  manifest.buckets.gens ??= {
    label: "Generated images — gpt-image-2 with platter-grounded identity (real+goofy default tone). One PNG per successful gen, dated. Synced to assets CDN via `npm run assets:sync:up`.",
    url_pattern: "https://assets.aesthetic.computer/jeffreys/gens/{name}",
    key_includes_extension: true,
    items: {},
  };
  manifest.buckets.gens.items[archiveName] = {
    model: MODEL,
    size: SIZE,
    quality: QUALITY,
    refs: REFS.map((r) => r.replace(REPO + "/", "")),
    context,
    segment: segName,
    generated: new Date().toISOString(),
    bytes: statSync(archivePath).size,
    prompt: metaphor,
  };
  writeFileSync(PLATTER_MANIFEST, JSON.stringify(manifest, null, 2) + "\n");
  return { archiveName, archivePath };
}

const context = `recap-${audienceName}`;

// Per-episode arc: every recap moves jeffrey through a day —
//   title  → cozy wake-up morning (always, regardless of chapter index)
//   first third → fresh morning energy, focused but unhurried
//   middle third → midday work state, engaged + varied micro-emotion
//   last third → subtle evening fatigue + occasional vape, warm/dim light
//   final → deep-night exhaustion or wired second wind
//   chat-instance → laptop-POV addendum (the photo's POV is the screen)
//
// The arc beat is APPENDED to the per-scene metaphor — per-scene specifics
// (diegetic chapter color, props, expressions) still win where they conflict;
// the arc beat just tints energy / fatigue / time-of-day.
function photoSegmentsInOrder() {
  return audience.segments
    .map((s) => ({ seg: s, slide: audience.slides[s.name] }))
    .filter(({ slide }) => slide && typeof slide === "object" && slide.metaphor);
}

function arcBeatFor(slide, idx, total) {
  if (slide.fixedArc) return ARC_BEATS[slide.fixedArc];
  // Slides explicitly tagged as a chat instance get the laptop-POV addendum
  // regardless of their position; the per-scene metaphor still drives the
  // composition, the arc beat just reinforces tone.
  if (slide.chatInstance) {
    return ARC_BEATS.chat;
  }
  // First photo-bearing segment is always title-morning; last is outro.
  if (idx === 0) return ARC_BEATS.title;
  if (idx === total - 1) return ARC_BEATS.outro;
  // Middle slides arc through the day in equal thirds. Use the index AMONG
  // photo-bearing segments (excluding title + outro) so the daily arc is
  // independent of how many chapters the episode happens to have.
  const middleIdx = idx - 1;
  const middleTotal = Math.max(1, total - 2);
  const t = middleIdx / middleTotal; // 0 (just after morning) → 1 (pre-outro)
  if (t < 1 / 3) return ARC_BEATS.fresh;
  if (t < 2 / 3) return ARC_BEATS.midday;
  return ARC_BEATS.late;
}

// ARC BEATS apply ENERGY / FATIGUE / VAPE-VISIBILITY only — they do NOT
// override per-scene time-of-day or lighting. If the per-scene metaphor
// is explicitly a night scene, the arc beat respects that and contributes
// only the body-language / mood / vape-prop cues. The "PER-SCENE WINS"
// preamble in each beat tells the model so.
const ARC_BEATS = {
  title:  `\n\nARC BEAT — DAY ARC, OPENING (per-scene wins for time-of-day / lighting): "cozy wake-up" energy. Just-woken: hopeful, soft, unhurried, slightly bedheaded but pleased. Eyes bright, posture relaxed, ZERO fatigue, NO vape pen visible. Color palette of the morning beat (pale-peach + warm-cream + soft pink dawn) layers over the per-scene lighting where they don't conflict.`,
  fresh:  `\n\nARC BEAT — DAY ARC, EARLY (per-scene wins for time-of-day / lighting): awake-and-focused energy. Posture engaged, expression alert without strain. NO fatigue. NO vape pen visible (he hasn't gotten there yet today). Body language reads "fresh".`,
  midday: `\n\nARC BEAT — DAY ARC, MIDDLE (per-scene wins for time-of-day / lighting): mid-work engaged state. Hands at the keyboard or mid-conversation. Expression stays hyperbolic-but-varied per scene (shock / mock-detective / smug / laugh). NO fatigue yet, vape pen is fine to appear if scene dictates but it's not the focus.`,
  late:   `\n\nARC BEAT — DAY ARC, LATER (per-scene wins for time-of-day / lighting): subtle fatigue setting in the BODY LANGUAGE. Slightly heavier eyelids, slight slump in shoulders, longer hair-pull, leftover food/drink visible somewhere in frame, the laptop's screen glow more dominant on his face than earlier in the day. The Stiiizy-style USB-stick vape pen with its tiny LED is MORE LIKELY visible — sometimes mid-puff with a faint vapor wisp, sometimes resting on the desk. Mood: frayed-but-funny, second-thought, weary affection. The per-scene metaphor still controls the actual room light + props; this beat just tilts the energy.`,
  outro:  `\n\nARC BEAT — DAY ARC, END (per-scene wins for time-of-day / lighting): end-of-day. Either deep exhaustion or wired second-wind — whichever fits the per-scene mood. Body posture either slumped + mug-in-hand satisfied, or alert eyebrows + closing-strong. Color palette of cobalt indigo + screen-glow cyan + a pool of warm lamp where it doesn't clash with the per-scene light.`,
  chat:   `\n\nARC BEAT — POV ADDENDUM (overrides camera position, NOT scene content): laptop-series FIRST-PERSON shot. The camera is where the laptop's webcam would be — looking UP at jeffrey from chest height, SCREEN-SIDE perspective, as if the laptop itself caught the still mid-conversation. He looks forward AT the camera (= at the screen), focused or mid-laugh per scene. The bottom edge of the Citrus-green MacBook Neo is just visible at the FOOT of the frame. His hands are out of frame on the keyboard, or at chest level. Soft natural room light + screen glow on his face. Phone-snapshot tone, not cinematic.`,
};

const photoSegs = photoSegmentsInOrder();
const photoTotal = photoSegs.length;

console.log(`refs: ${REFS.length} (${REFS.length} found)`);
console.log(`out:  ${PHOTOS_DIR}`);
console.log(`platter archive: ${PLATTER_GENS_DIR.replace(REPO + "/", "")}`);
console.log(`arc:  title → fresh → midday → late → outro · ${photoTotal} portraits`);

let generated = 0, cached = 0, failed = 0;
for (let i = 0; i < photoSegs.length; i++) {
  const { seg, slide } = photoSegs[i];
  if (only && seg.name !== only) continue;
  const arc = arcBeatFor(slide, i, photoTotal);
  const metaphor = slide.metaphor + (arc || "");
  const outPath = `${PHOTOS_DIR}/${seg.name}.png`;
  // Per-segment extra refs (relative to repo root) — passed to gpt-image-2
  // alongside the standard SHOOT + SELFIE identity refs. Used for
  // baking real artifacts into the scene (e.g. the VFC painting,
  // calarts header illustration, AC screenshots for chapters that
  // need real UI in shot). Source: audience.slides[seg.name].extraRefs.
  const extraRefRels = (slide && Array.isArray(slide.extraRefs)) ? slide.extraRefs : [];
  const extraRefs = extraRefRels.map((rel) => rel.startsWith("/") ? rel : `${REPO}/${rel}`);
  if (existsSync(outPath) && !force) {
    console.log(`  ✓ ${seg.name}.png (cached)`);
    cached++;
    continue;
  }
  process.stdout.write(`▸ ${seg.name}… `);
  const t0 = Date.now();
  try {
    const usage = await generate(metaphor, outPath, extraRefs);
    const archive = archiveToPlatter({ segName: seg.name, metaphor, sourcePath: outPath, context });
    const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
    const tok = usage.tokens_in
      ? ` · tokens in=${usage.tokens_in} out=${usage.tokens_out}`
      : "";
    console.log(`✓ ${elapsed}s${tok} → ${archive.archiveName}`);
    generated++;
  } catch (e) {
    console.log(`✗`);
    console.error(`  ${e.message}`);
    failed++;
  }
}

console.log(`✓ photos: ${generated} new, ${cached} cached, ${failed} failed`);
if (generated > 0) {
  console.log(`  · platter manifest updated: papers/jeffrey-platter/manifest.json`);
  console.log(`  · run \`node papers/jeffrey-platter/sync.mjs\` to refresh consumer copy`);
  console.log(`  · run \`npm run assets:sync:up\` to push gens/ to the CDN`);
}
// Don't fail the pipeline on per-segment gen failures — slides fall back to a
// dark-bg placeholder when the glob matches nothing. Re-run later to retry
// just the missing photos.
