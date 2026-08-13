#!/usr/bin/env node
// aesthetic-captutor — narrated, captioned tutorials for aesthetic.computer.
//
// The AC-facing sibling of `captutor/` (fuser's tutorial renderer). Same one
// idea — narrate FIRST, then perform to the click track — and the same measured
// -offset sync, but the stage is different: instead of filming a real desktop
// window through SlabMenubar's ScreenCaptureKit bridge, this drives a dedicated
// headless Chromium (the chromium-pool approach the office minis use for
// captures) and records the composited page over CDP Page.screencast, exactly
// like captutor's own bin/cdp-reel.mjs fallback. No Screen Recording TCC grant,
// no display rotation, no window sizing — a 9:16 tutorial is just a 9:16
// viewport, and AC's mobile-first layout is the star.
//
//   node aesthetic-captutor.mjs narrate <screenplay>   # voice only — check pacing
//   node aesthetic-captutor.mjs render  <screenplay>   # narrate → boot → drive → mp4 + vtt
//     --format vertical|landscape   (default: vertical, 1080x1920)
//     --url https://aesthetic.computer  (or a local dev server)
//     --keep-frames                  (keep the raw screencast jpegs for QA)
//
// How a take stays in sync (captutor's contract, kept verbatim):
//   1. Every spoken line is synthesized first via /api/say (field `from`, NOT
//      `text` — see captutor/lib/narrate.mjs for the silent-default trap), so
//      each beat's exact duration is known before recording starts.
//   2. The recorder reports `since` — the wall clock of the first captured
//      frame. That is the timeline's true origin.
//   3. Each beat is stamped when it ACTUALLY begins relative to `since`; fast
//      beats are padded to their narration length, slow beats simply take as
//      long as they take.
//   4. Narration is laid down at those MEASURED offsets. No re-sync, no drift.

import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { dirname, join, resolve, basename } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import { narrate } from "../captutor/lib/narrate.mjs";
import { launchChromium, attachPage, ScreencastRecorder } from "./lib/stage.mjs";
import { writeVTT, mux, probe } from "./lib/compose.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT_ROOT = process.env.AESTHETIC_CAPTUTOR_OUT || join(HERE, "out");

/// Delivery geometries. AC is mobile-first, so `vertical` is the default: a
/// phone-shaped viewport rendered at 2x, delivered as a native 1080x1920 take —
/// never a landscape recording cropped into a phone frame (see captutor's
/// FORMATS note on why cropped verticals always look wrong).
export const FORMATS = {
  vertical: { view: { w: 540, h: 960, dpr: 2 }, out: { w: 1080, h: 1920 }, fps: 30 },
  landscape: { view: { w: 960, h: 540, dpr: 2 }, out: { w: 1920, h: 1080 }, fps: 30 },
};

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Breathing room after each line lands, so beats don't feel butt-spliced.
const BEAT_GAP_SEC = 0.55;
// Let the last picture settle before the cut.
const TAIL_HOLD_SEC = 1.4;

function flag(name, fallback = null) {
  const i = process.argv.indexOf(name);
  return i >= 0 ? process.argv[i + 1] : fallback;
}
const has = (name) => process.argv.includes(name);

async function loadScreenplay(ref) {
  const candidates = [
    resolve(ref),
    join(HERE, "screenplays", ref),
    join(HERE, "screenplays", `${ref}.mjs`),
  ];
  const path = candidates.find((p) => existsSync(p) && p.endsWith(".mjs"));
  if (!path) throw new Error(`screenplay not found: ${ref}`);
  const mod = await import(pathToFileURL(path).href);
  const sp = mod.default;
  if (!sp?.beats?.length) throw new Error(`${basename(path)} exports no beats`);
  sp.slug ||= basename(path, ".mjs");
  return sp;
}

/// The verbs a beat's `do` can use. Deliberately AC-shaped: the prompt is a
/// keyboard instrument, so `type`/`press` are primary; `tap`/`drag` cover the
/// pieces that want a finger. Coordinates are fractions of the viewport so a
/// screenplay renders identically in every format.
function beatAPI(page, view) {
  const px = (fx, fy) => ({ x: Math.round(fx * view.w), y: Math.round(fy * view.h) });
  return {
    cdp: page,
    sleep,
    // Type like a person: one visible keystroke at a time. AC's prompt echoes
    // every character, and the typing itself is the picture.
    async type(text, { cps = 9 } = {}) {
      for (const ch of text) {
        await page.key(ch);
        await sleep(1000 / cps + Math.random() * 40);
      }
    },
    async press(key) { await page.key(key); },
    async tap(fx, fy) { const { x, y } = px(fx, fy); await page.tap(x, y); },
    async drag(fromX, fromY, toX, toY, { steps = 24, ms = 600 } = {}) {
      await page.drag(px(fromX, fromY), px(toX, toY), steps, ms);
    },
  };
}

async function render(sp) {
  const formatName = flag("--format", sp.format || "vertical");
  const F = FORMATS[formatName];
  if (!F) throw new Error(`unknown format: ${formatName} (have: ${Object.keys(FORMATS).join(", ")})`);
  const url = flag("--url", sp.url || "https://aesthetic.computer");
  const take = join(OUT_ROOT, sp.slug, formatName);
  mkdirSync(take, { recursive: true });

  console.log(`● narrating (${sp.beats.length} beats, voice: ${sp.voice || "jeffrey"})`);
  const beats = await narrate(sp.beats, { voice: sp.voice || "jeffrey", dir: join(OUT_ROOT, sp.slug, "voice") });
  const plannedSec = beats.reduce((s, b) => s + b.durationSec + BEAT_GAP_SEC, 0);
  console.log(`  planned narration: ${plannedSec.toFixed(1)}s`);

  console.log(`● booting chromium (${F.view.w}x${F.view.h}@${F.view.dpr}x → ${F.out.w}x${F.out.h})`);
  const chromium = await launchChromium({ view: F.view });
  let clip;
  const cues = [];
  try {
    const page = await attachPage(chromium);
    await page.setViewport(F.view);
    await page.nav(url);
    if (sp.boot) await sleep(sp.boot); // let the AC boot canvas finish

    const recorder = new ScreencastRecorder(page, { dir: join(take, "frames"), fps: F.fps });
    const since = await recorder.start();
    console.log(`● recording cdp-screencast since ${since.toFixed(3)}`);

    const api = beatAPI(page, F.view);
    for (const beat of beats) {
      const at = Date.now() / 1000 - since;
      console.log(`  ${String(beat.index + 1).padStart(2)}. @${at.toFixed(2)}s  ${beat.say.slice(0, 58)}`);
      const t0 = Date.now();
      if (beat.do) await beat.do(api);
      const took = (Date.now() - t0) / 1000;
      const owed = beat.durationSec + BEAT_GAP_SEC - took;
      if (owed > 0) await sleep(owed * 1000); // pad: voice must never outrun picture
      cues.push({ ...beat, offsetSec: at, tookSec: Math.max(took, beat.durationSec) });
    }
    await sleep(TAIL_HOLD_SEC * 1000);

    clip = await recorder.stop({ out: F.out });
  } finally {
    chromium.kill();
  }

  const cuesPath = join(take, "cues.json");
  writeFileSync(cuesPath, JSON.stringify(
    cues.map(({ mp3, say, offsetSec, durationSec, words }) => ({ say, mp3, offsetSec, durationSec, words })),
    null, 2));

  const vtt = join(take, `${sp.slug}.vtt`);
  writeVTT(cues, vtt);

  const out = join(take, `${sp.slug}-${formatName}.mp4`);
  mux({ clip, beats: cues, out, vtt });
  const info = probe(out);
  console.log(`\n✓ ${out}`);
  console.log(`  ${info.width}x${info.height} ${Number(info.duration).toFixed(1)}s`);
  if (!has("--keep-frames")) {
    const { rmSync } = await import("node:fs");
    rmSync(join(take, "frames"), { recursive: true, force: true });
  }
  return out;
}

const [command, ref] = process.argv.slice(2);
if (command === "narrate" && ref) {
  const sp = await loadScreenplay(ref);
  await narrate(sp.beats, { voice: sp.voice || "jeffrey", dir: join(OUT_ROOT, sp.slug, "voice") });
} else if (command === "render" && ref) {
  await render(await loadScreenplay(ref));
} else {
  console.log("usage: aesthetic-captutor.mjs narrate|render <screenplay> [--format vertical] [--url …] [--keep-frames]");
  process.exit(command ? 1 : 0);
}
