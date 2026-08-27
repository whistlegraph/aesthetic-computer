#!/usr/bin/env node
// menuband-reel.mjs — the @menuband.app reel lane, as clockwork calls it.
//
// The content is the waltz series: vertical scrolling play-alongs rendered
// from pop/menuband/waltzes/reel-lane.json. This driver publishes that
// manifest's first variation the account ledger has not seen, oldest first;
// what the NEXT variation should be is the reelboy rock's question, answered
// from reel feedback and shipped through @jeffrey's gate (REELBOY.md).
//
//   node toolchain/instagram/menuband-reel.mjs queue          lane state
//   node toolchain/instagram/menuband-reel.mjs next           dry-run the pick
//   node toolchain/instagram/menuband-reel.mjs next --auto    render + publish
//   node toolchain/instagram/menuband-reel.mjs generate       mint the next
//                                                             variation, no post
//   node toolchain/instagram/menuband-reel.mjs insights       refresh ledger
//
// Publishing rides ig.mjs (credentials, Spaces upload, ledger append) and
// closes the loop with reelboy autobind --account menuband, so each posted
// waltz becomes the lane's newest intake. A missing reelboy route only says
// so — publishing never fails on the feedback loop's absence.

import { execFileSync, spawnSync } from "node:child_process";
import { existsSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { basename, dirname, join, resolve } from "node:path";

const ROOT = resolve(import.meta.dirname, "../..");
const MANIFEST = join(ROOT, "pop/menuband/waltzes/reel-lane.json");
const RENDERER = join(ROOT, "pop/menuband/bin/render-menu-band-waltzes.mjs");
const LEDGER = join(ROOT, "social/instagram/menuband-ledger.json");
const IG = join(ROOT, "toolchain/instagram/ig.mjs");
const REELBOY = join(ROOT, "toolchain/instagram/reelboy.mjs");

const argv = process.argv.slice(2);
const cmd = argv.find((a) => !a.startsWith("--")) || "queue";
const auto = argv.includes("--auto");

function die(msg) { console.error(`✗ ${msg}`); process.exit(1); }
const readJson = (path, fallback) => {
  try { return JSON.parse(readFileSync(path, "utf8")); } catch { return fallback; }
};

const manifest = readJson(MANIFEST, null);
if (!manifest?.variations?.length) die(`no variations in ${MANIFEST}`);

// A variation is published when the ledger holds a post whose source video
// has its basename. Paths differ between checkouts; basenames don't.
const posted = new Set(
  (readJson(LEDGER, { posts: [] }).posts || [])
    .map((post) => basename(String(post.source || ""))));
const published = (entry) => posted.has(basename(entry.outPath));

// The series numbering lives in the id — "04-stairwell" is waltz no. 4.
const waltzNumber = (entry) => Number.parseInt(entry.id, 10);

// Let the score and its file name carry the reel; the caption is only routing.
function caption() { return "#menuband #mac #apple"; }

// Every reel's Instagram audio is named as a playable score file — .mbscore
// is Menu Band's real format, and the name is the marketing.
const audioName = (entry) => `${String(entry.name).toLowerCase()}.mbscore`;

function mp4Path(entry) {
  return resolve(dirname(MANIFEST), manifest.baseDir || ".", entry.outPath);
}

// ── the generator ────────────────────────────────────────────────────
// An hourly cadence outruns any hand-written queue, so when every
// variation is posted the lane writes the next one itself. Waltz no. N is
// a pure function of N — every checkout that generates it agrees, so the
// manifest append is a record, not a coordination point. The rock's charter
// is exactly this function's taste: it may reshape any of these choices.

const WORDS = [
  "attic", "orchard", "harbor", "meadow", "copper", "satchel", "sparrow",
  "teacup", "ribbon", "chimney", "garden", "pillow", "mirror", "bicycle",
  "umbrella", "kettle", "lighthouse", "postcard", "marble", "clover",
  "fountain", "drawer", "blanket", "compass", "sailboat", "thimble",
  "walnut", "ivy", "parlor", "gramophone", "saucer", "tangerine", "awning",
  "brook", "candle", "dormer", "easel", "foyer", "gazebo", "hallway",
  "inkwell", "jasmine", "knoll", "lattice", "mantel", "napkin", "oriel",
  "pantry", "quill", "rafter", "shutter", "trellis", "veranda", "wharf",
  "yarrow", "zinnia", "bellows", "cistern", "dovecote", "embers",
]; // names repeat after 60 waltzes; the number keeps them distinct

// The audio renderer draws only the white Menu Band keys and fatals on any
// accidental, so mode DECIDES tonic — these are the three all-natural
// pairings it can play (its tonic table maps exactly A, D, and default-C).
const MODES = [
  ["major", "C"], ["major", "C"], ["minor", "A"], ["dorian", "D"],
];
// The lane's voice is GM patch 79, Whistle (0-indexed program 78) — every
// Menu Band reel whistles (@jeffrey, 2026-08-26). The one-element list
// keeps pick()'s rand() consumption, so already-minted waltz numbers keep
// their melodies.
const INSTRUMENTS = [[78, "Whistle"]];
const PROGRESSIONS = [
  [0, 3, 4, 0, 5, 3, 4, 0], [0, 5, 3, 4, 0, 3, 4, 0],
  [0, 3, 0, 4, 5, 3, 4, 0], [0, 4, 5, 3, 0, 5, 4, 0],
];
// bars × 3 beats at this bpm = exactly 60 s, the lane's fixed form.
const BPMS = [66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 99, 102];

function mulberry32(seed) {
  let a = seed >>> 0;
  return () => {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function hex(h, s, l) {
  const f = (n) => {
    const k = (n + h / 30) % 12;
    const c = l / 100 - (s / 100) * Math.min(l / 100, 1 - l / 100) *
      Math.max(-1, Math.min(k - 3, 9 - k, 1));
    return Math.round(255 * c).toString(16).padStart(2, "0");
  };
  return `#${f(0)}${f(8)}${f(4)}`;
}

function generateVariation(n) {
  const rand = mulberry32(1009 * n + 7);
  const pick = (list) => list[Math.floor(rand() * list.length)];
  const name = WORDS[(n - 5 + WORDS.length * 100) % WORDS.length];
  const bpm = pick(BPMS);
  // A melody of eight bar-groups: a small random walk over the scale
  // ladder, 2–4 notes a group, always landing home. It starts mid-ladder
  // and reflects off the ends — clamping there parks whole phrases on the
  // floor and everything comes out bass and drone.
  let degree = 3 + Math.floor(rand() * 5);
  const melodyBars = Array.from({ length: 8 }, (_, group) => {
    const notes = Array.from({ length: 2 + Math.floor(rand() * 3) }, () => {
      const step = [-2, -1, 1, 1, 2][Math.floor(rand() * 5)];
      degree += degree + step < 0 || degree + step > 11 ? -step : step;
      return degree;
    });
    if (group === 7) notes[notes.length - 1] = 0;
    return notes;
  });
  const hue = (n * 137.508) % 360;
  const id = `${String(n).padStart(2, "0")}-${name}`;
  const dir = `pop/menuband/out/menu-band-waltzes/${id}`;
  const [instrumentProgram, instrumentName] = pick(INSTRUMENTS);
  const [mode, tonic] = pick(MODES);
  return {
    id, name: name[0].toUpperCase() + name.slice(1),
    seed: 1009 * n + 7, bpm, bars: bpm / 3,
    tonic, mode,
    instrumentProgram, instrumentName,
    development: n % 2 ? "lift" : "mirror",
    harmonyDegrees: pick(PROGRESSIONS),
    melodyBars,
    notesPath: `${dir}/${id}.notes.json`,
    audioPath: `${dir}/${id}.wav`,
    outPath: `${dir}/${id}.mp4`,
    visual: { palette: [hex(hue, 30, 93), hex(hue, 35, 65), hex(hue, 40, 30)] },
  };
}

// When the queue is dry, append waltz no. (highest + 1) to the manifest and
// hand it back as the next thing to post.
function ensureQueue() {
  const open = manifest.variations.find((entry) => !published(entry));
  if (open) return open;
  const next = 1 + Math.max(...manifest.variations.map(waltzNumber));
  const entry = generateVariation(next);
  manifest.variations.push(entry);
  writeFileSync(MANIFEST, JSON.stringify(manifest, null, 2) + "\n");
  console.log(`✎ generated waltz no. ${next} — ${entry.id} · ` +
    `${entry.bpm} BPM ${entry.tonic} ${entry.mode} · ${entry.instrumentName}`);
  return entry;
}

function complete(path, durationSec) {
  // A killed render can leave a VALID but truncated mp4 (its ffmpeg child
  // finalizes on pipe EOF) — size alone once let a 12-second fragment reach
  // Instagram. Only a probe that matches the score's duration counts.
  if (!existsSync(path) || statSync(path).size < 100_000) return false;
  const ffprobe = existsSync(join(process.env.HOME ?? "", ".local/bin/ffprobe"))
    ? join(process.env.HOME, ".local/bin/ffprobe") : "ffprobe";
  const probe = spawnSync(ffprobe, ["-v", "error", "-show_entries",
    "format=duration", "-of", "default=nw=1:nk=1", path], { encoding: "utf8" });
  const actual = Number(probe.stdout?.trim());
  return probe.status === 0 && Number.isFinite(actual) &&
    Math.abs(actual - durationSec) < 0.1;
}

function renderIfNeeded(entry) {
  const out = mp4Path(entry);
  if (complete(out, entry.durationSec ?? manifest.defaults?.durationSec ?? 60)) return out;
  console.log(`▸ rendering ${entry.id} (${entry.bpm} BPM, ${entry.bars} bars)`);
  const render = spawnSync("nice", ["-n", "19", process.execPath, RENDERER,
    "--manifest", MANIFEST, "--ids", entry.id], { cwd: ROOT, stdio: "inherit" });
  if (render.status !== 0) die(`render failed for ${entry.id}`);
  if (!existsSync(out)) die(`render finished but ${out} is missing`);
  return out;
}

function publish(entry) {
  const video = renderIfNeeded(entry);
  console.log(`▸ posting ${entry.id} as @menuband`);
  const post = spawnSync(process.execPath, [IG, "--as", "menuband", "post",
    video, "--caption", caption(entry), "--audio-name", audioName(entry)],
    { cwd: ROOT, stdio: "inherit" });
  if (post.status !== 0) die(`ig.mjs post failed for ${entry.id}`);
  // ig.mjs leaves the receipt beside the video (extension swapped for
  // .instagram.json); the media id in it is what reelboy watches.
  const receipt = readJson(video.replace(/\.[^.]+$/, "") + ".instagram.json", null);
  const mediaId = receipt?.mediaId;
  if (!mediaId) { console.log(`⚠ no receipt media id — skipping autobind`); return; }
  try {
    const bound = execFileSync(process.execPath,
      [REELBOY, "autobind", String(mediaId), "--account", "menuband"],
      { encoding: "utf8", timeout: 20000 }).trim();
    console.log(`   ${bound}`);
  } catch (error) {
    console.log(`   reelboy autobind skipped: ` +
      `${String(error.stderr || error.message || error).split("\n")[0]}`);
  }
  // Secondary platforms (YouTube now, TikTok post-audit); a skip or failure
  // there is a log line, never this publish's problem.
  try {
    const lines = execFileSync(process.execPath,
      [join(ROOT, "toolchain/social/syndicate.mjs"), video,
        "--account", "menuband", "--media-id", String(mediaId),
        "--caption", `${entry.name.toLowerCase()} — menu band waltz no. ${waltzNumber(entry)} · menuband.app`,
        "--seconds", String(entry.durationSec ?? 60)],
      { encoding: "utf8", timeout: 15 * 60_000 }).trim();
    console.log(lines.replace(/^/gm, "   "));
  } catch (error) {
    console.log(`   syndication skipped: ` +
      `${String(error.stderr || error.message || error).split("\n")[0]}`);
  }
}

if (cmd === "queue") {
  for (const entry of manifest.variations)
    console.log(`${published(entry) ? "✓ posted " : "· pending"} ${entry.id} · ` +
      `${entry.name} · ${entry.bpm} BPM · ${entry.instrumentName}`);
} else if (cmd === "generate") {
  // Force-mint the next variation without posting — for inspection, and for
  // the rock to see what the house generator would have done.
  const before = manifest.variations.length;
  const entry = ensureQueue();
  if (manifest.variations.length === before)
    console.log(`queue is not dry — next up is ${entry.id}, nothing generated`);
} else if (cmd === "next") {
  const entry = ensureQueue();
  if (!auto) {
    console.log(`would post ${entry.id} · audio "${audioName(entry)}" · caption:`);
    console.log(caption(entry).replace(/^/gm, "  "));
    console.log(`(pass --auto to render and publish)`);
    process.exit(0);
  }
  publish(entry);
} else if (cmd === "insights") {
  const refresh = spawnSync(process.execPath,
    [IG, "--as", "menuband", "insights", "--refresh"],
    { cwd: ROOT, stdio: "inherit" });
  process.exit(refresh.status ?? 1);
} else die(`unknown command "${cmd}" — queue | next [--auto] | generate | insights`);
