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
const KEEP_ALL = new Set(["minitek", "maytrax"]);
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
