#!/usr/bin/env node
// pop/bin/pop-library.mjs — build pop/out/pop-library.json: an index of the
// canonical /pop tracks with status + metadata for JukeWizard's library view.
//
// Per track: path, title, lane, backend (engine), status (from RELEASES.md),
// last-updated (mtime), revisions (git commits touching the lane), duration,
// size, bpm/key + release links (spotify/apple/youtube/distrokid).
//
//   node pop/bin/pop-library.mjs            # writes pop/out/pop-library.json
//   node pop/bin/pop-library.mjs --no-probe # skip ffprobe (faster, no durations)

import { readFileSync, writeFileSync, mkdirSync, statSync, existsSync, readdirSync } from "node:fs";
import { resolve, dirname, basename, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP = resolve(HERE, "..");
const REPO = resolve(POP, "..");
const NOPROBE = process.argv.includes("--no-probe");

// ── parse RELEASES.md into per-track status/links blocks ───────────────────
const STATUSES = ["RELEASED", "SUBMITTED", "MASTERING", "RENDER", "WIP", "IDEA"];
function parseReleases() {
  const md = existsSync(`${POP}/RELEASES.md`) ? readFileSync(`${POP}/RELEASES.md`, "utf8") : "";
  const blocks = md.split(/^## /m).slice(1);
  const out = {};
  for (const b of blocks) {
    const header = b.split("\n", 1)[0];
    if (/youtube state/i.test(header)) continue;
    const name = header.split(/ — | \(|—/)[0].trim();
    const status = STATUSES.find((s) => header.includes(s)) || "WIP";
    const laneM = b.match(/\*\*Lane:\*\*\s*`?pop\/([\w/-]+?)\/?`?[\s·]/i) || b.match(/`pop\/([\w/-]+?)\/`/);
    const lane = laneM ? laneM[1] : null;
    const grab = (re) => { const m = b.match(re); return m ? m[1] : null; };
    out[name.toLowerCase()] = {
      name, status, lane,
      bpm: grab(/(\d{2,3})\s*BPM/i),
      key: grab(/·\s*([A-G][#b]?\s*(?:major|minor))/i),
      releaseDate: grab(/RELEASED[^\n]*?(\d{4}-\d{2}-\d{2})/) || grab(/DistroKid\s*(\d{4}-\d{2}-\d{2})/),
      spotify: grab(/(https:\/\/open\.spotify\.com\/track\/\S+)/),
      apple: grab(/(https:\/\/music\.apple\.com\/\S+)/),
      youtube: grab(/(https:\/\/youtu\.be\/\S+)/),
      distrokid: grab(/(https:\/\/distrokid\.com\/\S+)/),
    };
  }
  return out;
}

// ── walk every pop/**/out for mp3s ─────────────────────────────────────────
// Lanes whose every track is a kept deliverable (this session's batches).
const KEEP_ALL = new Set(["minitek", "maytrax", "cornerfive"]);
function walkMp3(dir, acc) {
  for (const e of readdirSync(dir, { withFileTypes: true })) {
    const p = resolve(dir, e.name);
    if (e.isDirectory()) {
      if (["node_modules", ".git", "archive", "samples", "shouts", "references", ".tmp", "sfx", "narration", "stems", "motion"].includes(e.name)) continue;
      walkMp3(p, acc);
    } else if (e.name.endsWith(".mp3") && basename(dir) === "out") {
      acc.push(p);   // only files directly in an out/ dir, not nested scratch
    }
  }
  return acc;
}
// Canonical = a real track, not a scratch stem/section. The main track of a
// lane is almost always a single-token name (marimbaba, hellsine, solafiya);
// scratch is hyphenated (amazing-wavewizard-dance-…). Keep: any single-token
// name, anything named in RELEASES.md, and the keep-all session lanes.
function isCanonical(title, laneTop, laneDirRel, releases) {
  if (KEEP_ALL.has(laneTop) || laneDirRel.includes("lullabies")) return true;
  if (releases[title.toLowerCase()]) return true;
  return !title.includes("-");
}

function gitCount(laneDir) {
  const r = spawnSync("git", ["-C", REPO, "rev-list", "--count", "HEAD", "--", laneDir], { encoding: "utf8" });
  return r.status === 0 ? parseInt(r.stdout.trim(), 10) || 0 : 0;
}
function probeDur(p) {
  if (NOPROBE) return null;
  const r = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=noprint_wrappers=1:nokey=1", p], { encoding: "utf8" });
  return r.status === 0 ? Math.round(parseFloat(r.stdout.trim()) * 10) / 10 : null;
}
function backendFor(laneTop, laneDir) {
  if (existsSync(`${POP}/${laneTop}/c`)) return "C engine";
  if (laneTop === "marimba") return "marimba synth (JS)";
  if (laneDir.includes("lullabies")) return "marimba synth (JS)";
  return "JS DSP";
}

// ── art + associated media discovery ───────────────────────────────────────
// Covers live in a lane's out/ (usually), sometimes illy/ or the lane root,
// named "<title>-cover.png" (prefer the plain square over sized/-p-/-yt-/thumb
// variants). Media = the videos (reels/stories/canvas) whose name starts with
// the track title — the interesting clips, not scratch stills.
const IMG_EXT = new Set([".png", ".jpg", ".jpeg", ".webp"]);
const VID_EXT = new Set([".mp4", ".mov", ".webm", ".m4v"]);
function lsFiles(dir) {
  try { return readdirSync(dir, { withFileTypes: true }).filter((e) => e.isFile()).map((e) => e.name); }
  catch { return []; }
}
function findMedia(absMp3, title) {
  const outDir = dirname(absMp3);                 // pop/<lane>/out
  const laneDir = dirname(outDir);                // pop/<lane>
  const searchDirs = [outDir, `${laneDir}/illy`, laneDir];
  const t = title.toLowerCase();
  const ext = (n) => n.slice(n.lastIndexOf(".")).toLowerCase();
  const baseOf = (n) => n.slice(0, n.lastIndexOf(".")).toLowerCase();

  // cover ranking (lower = better); 99 = not this track's cover, skip it.
  const coverScore = (n) => {
    const b = baseOf(n);
    if (b === `${t}-cover`) return 0;
    if (b === `${t}-cover-3000` || b === `${t}-cover-1024`) return 1;
    if (b === `${t}-p-cover`) return 2;                      // portrait/square
    if (b.startsWith(`${t}`) && b.includes("cover")) return 3; // -yt-cover, -cover-v2…
    if (b === "cover" || b === t) return 4;
    return 99;
  };
  let cover = null, coverRank = 100;
  const media = [];
  const seen = new Set();
  for (const d of searchDirs) {
    for (const n of lsFiles(d)) {
      const e = ext(n), full = `${d}/${n}`, b = baseOf(n);
      if (IMG_EXT.has(e)) {
        const r = coverScore(n);
        if (r < 99 && r < coverRank) { coverRank = r; cover = full; }
      } else if (VID_EXT.has(e) && b.startsWith(t) && !seen.has(full)) {
        seen.add(full); media.push({ kind: "video", path: full });
      }
    }
  }
  media.sort((a, b) => a.path.localeCompare(b.path));
  return { art: cover, media };
}

const releases = parseReleases();
const files = walkMp3(POP, []).sort();
const gitCache = {};
const tracks = [];
for (const p of files) {
  const rel = relative(POP, p);                       // e.g. marimba/out/marimbaba.mp3
  const laneDirRel = dirname(rel).replace(/\/out$/, "");
  const laneTop = laneDirRel.split("/")[0];
  const title = basename(p, ".mp3");
  if (!isCanonical(title, laneTop, laneDirRel, releases)) continue;
  const st = statSync(p);
  const relInfo = releases[title.toLowerCase()] || releases[laneTop.toLowerCase()] || null;
  const laneAbs = `pop/${laneDirRel}`;
  if (gitCache[laneAbs] === undefined) gitCache[laneAbs] = gitCount(laneAbs);
  const { art, media } = findMedia(p, title);
  tracks.push({
    path: p,
    title,
    lane: laneDirRel,
    backend: backendFor(laneTop, laneDirRel),
    status: relInfo?.status || "UNRELEASED",
    updated: st.mtime.toISOString(),
    revisions: gitCache[laneAbs],
    bytes: st.size,
    durationSec: probeDur(p),
    bpm: relInfo?.bpm ? Number(relInfo.bpm) : null,
    key: relInfo?.key || null,
    releaseDate: relInfo?.releaseDate || null,
    art,
    media,
    links: relInfo ? {
      spotify: relInfo.spotify, apple: relInfo.apple,
      youtube: relInfo.youtube, distrokid: relInfo.distrokid,
    } : null,
  });
}

// released first, then by most-recently-updated
const rank = (s) => STATUSES.indexOf(s) === -1 ? 99 : STATUSES.indexOf(s);
tracks.sort((a, b) => rank(a.status) - rank(b.status) || b.updated.localeCompare(a.updated));

mkdirSync(`${POP}/out`, { recursive: true });
const outPath = `${POP}/out/pop-library.json`;
writeFileSync(outPath, JSON.stringify({ generated: new Date().toISOString(), count: tracks.length, tracks }, null, 2));
const byStatus = tracks.reduce((m, t) => ((m[t.status] = (m[t.status] || 0) + 1), m), {});
console.log(`✓ ${tracks.length} tracks → ${relative(REPO, outPath)}`);
console.log("  " + Object.entries(byStatus).map(([k, v]) => `${k}:${v}`).join("  "));
