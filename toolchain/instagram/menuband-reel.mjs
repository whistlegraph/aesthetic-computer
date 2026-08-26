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
//   node toolchain/instagram/menuband-reel.mjs insights       refresh ledger
//
// Publishing rides ig.mjs (credentials, Spaces upload, ledger append) and
// closes the loop with reelboy autobind --account menuband, so each posted
// waltz becomes the lane's newest intake. A missing reelboy route only says
// so — publishing never fails on the feedback loop's absence.

import { execFileSync, spawnSync } from "node:child_process";
import { existsSync, readFileSync, statSync } from "node:fs";
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

// The account's register: lowercase, simple cryptic hashes plus basic info,
// and a comment bait line because reelboy is listening.
function caption(entry) {
  const name = String(entry.name).toLowerCase();
  return [
    `${name} — menu band waltz no. ${waltzNumber(entry)}`,
    `play along in the mac menu bar · menuband.app`,
    `tell the band what to change`,
    `#mbscore #menubar #waltz`,
  ].join("\n");
}

// Every reel's Instagram audio is named as a playable score file — .mbscore
// is Menu Band's real format, and the name is the marketing.
const audioName = (entry) => `${String(entry.name).toLowerCase()}-waltz.mbscore`;

function mp4Path(entry) {
  return resolve(dirname(MANIFEST), manifest.baseDir || ".", entry.outPath);
}

function renderIfNeeded(entry) {
  const out = mp4Path(entry);
  if (existsSync(out) && statSync(out).size > 100_000) return out;
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
  // ig.mjs leaves the receipt beside the video; the media id in it is what
  // reelboy watches.
  const receipt = readJson(`${video}.instagram.json`, null);
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
}

if (cmd === "queue") {
  for (const entry of manifest.variations)
    console.log(`${published(entry) ? "✓ posted " : "· pending"} ${entry.id} · ` +
      `${entry.name} · ${entry.bpm} BPM · ${entry.instrumentName}`);
} else if (cmd === "next") {
  const entry = manifest.variations.find((candidate) => !published(candidate));
  if (!entry) {
    console.log(`lane is empty — every variation in ${basename(MANIFEST)} is ` +
      `posted; the rock owes the next one`);
    process.exit(0);
  }
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
} else die(`unknown command "${cmd}" — queue | next [--auto] | insights`);
