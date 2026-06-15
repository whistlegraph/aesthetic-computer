#!/usr/bin/env node
// grab.mjs — download a whistlegraph TikTok and read its musicality, as
// the first step in porting a whistlegraph into a /pop track.
//
// Given a video URL (or the @whistlegraph account), it:
//   1. yt-dlp downloads the mp4 (best quality, no watermark when offered)
//   2. ffmpeg extracts a mono 44.1k WAV for analysis
//   3. analyze.py prints tempo / key / whistled-melody note sequence and
//      writes <id>.analysis.json
//   4. updates downloads/INDEX.json so the corpus is browsable
//
// The mp4 / wav are gitignored (downloads/.gitignore); the per-clip
// .analysis.json and INDEX.json are small and tracked so the melody read
// is reproducible without re-pulling from TikTok.
//
// Usage:
//   node grab.mjs <tiktok-url>            # one clip
//   node grab.mjs --latest [N]           # newest N from @whistlegraph (default 1)
//   node grab.mjs --account @handle ...  # a different account
//   node grab.mjs <url> --redo           # re-download + re-analyze
//   node grab.mjs --list                 # print INDEX.json summary

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE      = dirname(fileURLToPath(import.meta.url));
const DOWNLOADS = resolve(HERE, "downloads");
const INDEX     = resolve(DOWNLOADS, "INDEX.json");
const ANALYZE   = resolve(HERE, "analyze.py");
const PY        = resolve(HERE, "../../pop/.venv/bin/python");
const HOME      = process.env.HOME || "";

// ── flags ──────────────────────────────────────────────────────────────
const argv    = process.argv.slice(2);
const url     = argv.find((a) => /^https?:/.test(a));
const flags   = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (!a.startsWith("--")) continue;
  const k = a.slice(2), n = argv[i + 1];
  if (n !== undefined && !n.startsWith("--") && !/^https?:/.test(n)) { flags[k] = n; i++; }
  else flags[k] = true;
}
const ACCOUNT = typeof flags.account === "string" ? flags.account.replace(/^@/, "") : "whistlegraph";
const REDO    = !!flags.redo;

function run(cmd, args, opts = {}) {
  const r = spawnSync(cmd, args, { stdio: "inherit", ...opts });
  if (r.status !== 0) throw new Error(`${cmd} ${args.join(" ")} → exit ${r.status}`);
}
function capture(cmd, args) {
  const r = spawnSync(cmd, args, { encoding: "utf8" });
  if (r.status !== 0) throw new Error(`${cmd} ${args.join(" ")} → exit ${r.status}\n${r.stderr}`);
  return r.stdout;
}
const tilde = (p) => p.replace(HOME, "~");

mkdirSync(DOWNLOADS, { recursive: true });

// ── --list: just dump the index ────────────────────────────────────────
if (flags.list && !url && !flags.latest) {
  if (!existsSync(INDEX)) { console.log("no clips yet"); process.exit(0); }
  const idx = JSON.parse(readFileSync(INDEX, "utf8"));
  for (const c of idx.clips ?? []) {
    console.log(`  ${c.id}  ${c.title}`);
    console.log(`      ${c.tempoBPM} BPM · ${c.key} · ${c.noteCount} notes · ${c.durationSec}s`);
  }
  process.exit(0);
}

// ── resolve target URLs ────────────────────────────────────────────────
let urls = [];
if (url) {
  urls = [url];
} else if (flags.latest) {
  const n = Number.isFinite(+flags.latest) && +flags.latest > 0 ? +flags.latest : 1;
  console.log(`→ fetching newest ${n} from @${ACCOUNT}…`);
  const ids = capture("yt-dlp", [
    "--flat-playlist", "--no-warnings",
    "--playlist-end", String(n),
    "--print", "url",
    `https://www.tiktok.com/@${ACCOUNT}`,
  ]).trim().split("\n").filter(Boolean);
  urls = ids;
} else {
  console.error("usage: grab.mjs <tiktok-url> | --latest [N] [--account @handle] | --list");
  process.exit(2);
}

// ── per-clip pipeline ──────────────────────────────────────────────────
function grabOne(clipUrl) {
  console.log(`\n→ ${clipUrl}`);
  const meta = JSON.parse(capture("yt-dlp", ["-J", "--no-warnings", "--no-playlist", clipUrl]));
  const id    = meta.id;
  const title = (meta.title || meta.description || "untitled").replace(/\s+/g, " ").trim();
  const base  = resolve(DOWNLOADS, `${ACCOUNT}-${id}`);
  const mp4   = `${base}.mp4`;
  const wav   = `${base}.wav`;
  const json  = `${base}.analysis.json`;

  console.log(`  id    : ${id}`);
  console.log(`  title : ${title}`);

  if (!existsSync(mp4) || REDO) {
    run("yt-dlp", ["--no-warnings", "--no-playlist", "-o", mp4, clipUrl]);
  } else {
    console.log(`  mp4   : present, reusing (--redo to refetch)`);
  }

  if (!existsSync(wav) || REDO) {
    run("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
      "-i", mp4, "-vn", "-ac", "1", "-ar", "44100", "-c:a", "pcm_s16le", wav]);
  }

  run(PY, [ANALYZE, wav, "--json", json]);
  const analysis = JSON.parse(readFileSync(json, "utf8"));

  return {
    id, title,
    account: ACCOUNT,
    url: clipUrl,
    durationSec: analysis.durationSec,
    tempoBPM: analysis.tempoBPM,
    key: analysis.key,
    keyConfidence: analysis.keyConfidence,
    noteCount: analysis.noteCount,
    pitchRange: analysis.pitchRange,
    mp4: tilde(mp4),
    analysis: tilde(json),
  };
}

const results = [];
for (const u of urls) {
  try { results.push(grabOne(u)); }
  catch (e) { console.error(`  ✗ ${e.message}`); }
}

// ── update INDEX.json (tracked) ────────────────────────────────────────
let index = { version: 1, clips: [] };
if (existsSync(INDEX)) {
  try { index = JSON.parse(readFileSync(INDEX, "utf8")); } catch { /* recreate */ }
}
if (!Array.isArray(index.clips)) index.clips = [];
for (const r of results) {
  index.clips = index.clips.filter((c) => c.id !== r.id);
  index.clips.push(r);
}
index.clips.sort((a, b) => String(b.id).localeCompare(String(a.id)));
writeFileSync(INDEX, JSON.stringify(index, null, 2) + "\n");
console.log(`\n✓ ${results.length} clip(s) → ${tilde(INDEX)} (${index.clips.length} total)`);
