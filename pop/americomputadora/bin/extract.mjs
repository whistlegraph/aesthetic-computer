#!/usr/bin/env node
// extract utterances ("america", "dora") from downloaded sources.
//
// usage:
//   node bin/extract.mjs                  # extract everything in clips.json
//   node bin/extract.mjs --group america  # only one group
//   node bin/extract.mjs --scan <file>    # silencedetect-driven candidate map
//
// clips.json schema:
// [
//   { "source": "sources/america/ray-charles-1972.mp3",
//     "group": "america", "tag": "ray-1", "start": 12.34, "end": 13.10,
//     "fade": 0.025, "gain_db": 0 }
// ]
//
// every clip → utterances/<group>/<source-slug>-<tag>.wav  (mono 48k normalized)

import { spawnSync, execSync } from "node:child_process";
import {
  existsSync, mkdirSync, readFileSync, writeFileSync,
} from "node:fs";
import { basename, dirname, join, extname } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = dirname(here);
const clipsPath = join(root, "clips.json");
const uttDir = join(root, "utterances");

function ensureClipsFile() {
  if (existsSync(clipsPath)) return;
  // seed with placeholder entries that match the planned source slugs from
  // bin/fetch.mjs. real timestamps need a human ear (or whisperX). leave
  // start/end as null so we don't accidentally cut silence.
  const seed = [
    // america the beautiful — every "america! america!" appears twice; we
    // want at least 2 clips per source. fill in timestamps after a listen.
    { source: "sources/america/ray-charles-1972.mp3",   group: "america", tag: "a1", start: null, end: null },
    { source: "sources/america/ray-charles-1972.mp3",   group: "america", tag: "a2", start: null, end: null },
    { source: "sources/america/whitney-houston.mp3",    group: "america", tag: "a1", start: null, end: null },
    { source: "sources/america/whitney-houston.mp3",    group: "america", tag: "a2", start: null, end: null },
    { source: "sources/america/beyonce-inauguration.mp3", group: "america", tag: "a1", start: null, end: null },
    { source: "sources/america/choral-public-domain.mp3", group: "america", tag: "a1", start: null, end: null },
    // dora — the theme has the chorus "dora, dora, dora the explorer";
    // expect 3+ clean cuts in a row. the intro variants give a "hi i'm dora!"
    { source: "sources/dora/theme-song.mp3",   group: "dora", tag: "d1", start: null, end: null },
    { source: "sources/dora/theme-song.mp3",   group: "dora", tag: "d2", start: null, end: null },
    { source: "sources/dora/theme-song.mp3",   group: "dora", tag: "d3", start: null, end: null },
    { source: "sources/dora/im-dora-intro.mp3", group: "dora", tag: "intro", start: null, end: null },
    { source: "sources/dora/soy-dora.mp3",      group: "dora", tag: "soy",   start: null, end: null },
  ];
  writeFileSync(clipsPath, JSON.stringify(seed, null, 2));
  console.log(`seeded ${clipsPath} — fill in start/end timestamps then re-run`);
}

function slug(s) {
  return s.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}

function srcSlug(srcRel) {
  return basename(srcRel, extname(srcRel));
}

function ffmpegCut(srcAbs, outAbs, start, end, fade = 0.02, gainDb = 0) {
  const dur = +(end - start).toFixed(3);
  const filters = [
    `afade=t=in:st=0:d=${fade}`,
    `afade=t=out:st=${(dur - fade).toFixed(3)}:d=${fade}`,
    "loudnorm=I=-14:TP=-1:LRA=11",
    `volume=${gainDb}dB`,
  ].join(",");
  const args = [
    "-y",
    "-ss", String(start),
    "-i", srcAbs,
    "-t", String(dur),
    "-af", filters,
    "-ac", "1",
    "-ar", "48000",
    outAbs,
  ];
  const res = spawnSync("ffmpeg", args, { stdio: ["ignore", "ignore", "ignore"] });
  return res.status === 0;
}

function scan(srcAbs) {
  // silencedetect finds gaps ≥ 200ms below -35 dB → candidate cut boundaries.
  const out = execSync(
    `ffmpeg -hide_banner -i "${srcAbs}" -af silencedetect=noise=-35dB:d=0.2 -f null - 2>&1 || true`,
    { encoding: "utf8" }
  );
  const events = [];
  for (const line of out.split("\n")) {
    const mStart = line.match(/silence_start:\s*([0-9.]+)/);
    const mEnd = line.match(/silence_end:\s*([0-9.]+)/);
    if (mStart) events.push({ kind: "silence_start", t: +mStart[1] });
    if (mEnd) events.push({ kind: "silence_end", t: +mEnd[1] });
  }
  // segments are silence_end → next silence_start
  const segments = [];
  let openStart = 0;
  for (const e of events) {
    if (e.kind === "silence_end") openStart = e.t;
    else if (e.kind === "silence_start") segments.push({ start: +openStart.toFixed(3), end: +e.t.toFixed(3), dur: +(e.t - openStart).toFixed(3) });
  }
  return segments;
}

function main() {
  const argv = process.argv.slice(2);
  if (argv[0] === "--scan") {
    const file = argv[1];
    if (!file) { console.error("--scan <file>"); process.exit(1); }
    const segs = scan(join(root, file));
    console.log(JSON.stringify(segs, null, 2));
    console.log(`# ${segs.length} candidate segments (silencedetect, -35dB / 200ms)`);
    return;
  }
  ensureClipsFile();
  const filter = argv.includes("--group") ? argv[argv.indexOf("--group") + 1] : null;
  const clips = JSON.parse(readFileSync(clipsPath, "utf8"));
  let made = 0, skipped = 0, todo = 0, missing = 0;
  for (const c of clips) {
    if (filter && c.group !== filter) continue;
    if (c.start == null || c.end == null) { todo++; continue; }
    const srcAbs = join(root, c.source);
    if (!existsSync(srcAbs)) {
      console.log(`  ? source missing: ${c.source}`);
      missing++;
      continue;
    }
    const outDir = join(uttDir, c.group);
    mkdirSync(outDir, { recursive: true });
    const outAbs = join(outDir, `${srcSlug(c.source)}-${slug(c.tag)}.wav`);
    if (existsSync(outAbs)) { skipped++; continue; }
    const ok = ffmpegCut(srcAbs, outAbs, c.start, c.end, c.fade ?? 0.02, c.gain_db ?? 0);
    if (ok) { console.log(`  ✓ ${c.group}/${basename(outAbs)}  (${(c.end - c.start).toFixed(2)}s)`); made++; }
    else { console.log(`  ✗ ${c.group}/${basename(outAbs)}`); }
  }
  console.log(`\n# ${made} made, ${skipped} skipped, ${todo} todo, ${missing} missing-source`);
  if (todo) console.log(`# edit ${clipsPath} and fill in start/end timestamps for the ${todo} todo clips.`);
}

main();
