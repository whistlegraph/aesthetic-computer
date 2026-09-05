#!/usr/bin/env node
// fetch-real-kit.mjs — pull a REAL drum kit for imab from Freesound,
// CC0 only (SCORE.md commercial-safe law — imab ships to DistroKid).
//
// For each role: CC0 text search → download previews → score every
// candidate by duration + spectral centroid (role-appropriate windows)
// + Freesound rating → trim leading silence, normalize, write the
// winner to samples/real/<role>.wav (48k mono) + provenance in
// samples/real/kit-real.json.
//
//   node pop/imab/bin/fetch-real-kit.mjs           # fetch + pick all
//   node pop/imab/bin/fetch-real-kit.mjs kick      # redo one role

import { execSync, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { searchSounds, downloadPreview } from "../../lib/freesound.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REAL = resolve(HERE, "../samples/real");
mkdirSync(REAL, { recursive: true });

const CC0 = 'license:"Creative Commons 0"';
const ROLES = {
  kick: {
    query: "house kick drum sample",
    filter: `${CC0} duration:[0.1 TO 1.0]`,
    centroid: [40, 2600], target: 800,
  },
  snare: {
    query: "snare drum one shot acoustic",
    filter: `${CC0} duration:[0.08 TO 0.9]`,
    centroid: [1200, 8000], target: 3500,
  },
  clap: {
    query: "hand clap one shot",
    filter: `${CC0} duration:[0.08 TO 0.9]`,
    centroid: [1200, 9000], target: 3800,
  },
  "hat-closed": {
    query: "closed hi hat one shot",
    filter: `${CC0} duration:[0.02 TO 0.4]`,
    centroid: [4500, 16000], target: 9000,
  },
  "hat-open": {
    query: "open hi hat one shot",
    filter: `${CC0} duration:[0.15 TO 1.6]`,
    centroid: [3500, 16000], target: 8000,
  },
  shaker: {
    query: "shaker single hit",
    filter: `${CC0} duration:[0.04 TO 0.7]`,
    centroid: [2500, 16000], target: 7000,
  },
};

const centroidOf = (wav) => {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-i", wav, "-af",
    "aspectralstats=measure=centroid:win_size=2048," +
    "ametadata=print:key=lavfi.aspectralstats.1.centroid:file=-",
    "-f", "null", "-"], { encoding: "utf8", maxBuffer: 16 << 20 });
  const vals = [...(r.stdout || "").matchAll(/centroid=([\d.]+)/g)]
    .map((m) => Number(m[1])).filter((v) => v > 0);
  if (!vals.length) return null;
  vals.sort((a, b) => a - b);
  return vals[Math.floor(vals.length / 2)];
};

const only = process.argv[2];
const picks = existsSync(`${REAL}/kit-real.json`)
  ? JSON.parse(readFileSync(`${REAL}/kit-real.json`, "utf8")) : {};

for (const [role, spec] of Object.entries(ROLES)) {
  if (only && role !== only) continue;
  console.log(`\n── ${role}: "${spec.query}"`);
  const data = await searchSounds({
    query: spec.query, filter: spec.filter, pageSize: 12,
    sort: "rating_desc",
  });
  const scored = [];
  for (const sound of data.results || []) {
    if (!/creativecommons.org\/publicdomain|Creative Commons 0/i
      .test(sound.license)) continue;                    // belt & braces
    let path;
    try { path = await downloadPreview(sound); }
    catch (e) { console.log(`  ✗ ${sound.id}: ${e.message}`); continue; }
    const wav = path.replace(/\.mp3$/, ".wav");
    if (!existsSync(wav)) continue;
    const c = centroidOf(wav);
    if (c === null || c < spec.centroid[0] || c > spec.centroid[1]) {
      console.log(`  · ${sound.id} ${sound.name.slice(0, 34)} — centroid ${c?.toFixed(0) ?? "?"} out of window`);
      continue;
    }
    const closeness = 1 - Math.min(1, Math.abs(Math.log2(c / spec.target)));
    const rating = (sound.avg_rating || 3) / 5;
    scored.push({ sound, wav, c, score: closeness * 0.6 + rating * 0.4 });
    console.log(`  ✓ ${sound.id} ${sound.name.slice(0, 34)} — centroid ${c.toFixed(0)}, rating ${sound.avg_rating ?? "—"}`);
  }
  if (!scored.length) { console.log(`  !! no ${role} candidate survived`); continue; }
  scored.sort((a, b) => b.score - a.score);
  const win = scored[0];

  // trim leading silence, normalize to −1 dB peak, 48k mono
  const probe = spawnSync("ffmpeg", ["-hide_banner", "-i", win.wav, "-af",
    "silenceremove=start_periods=1:start_threshold=-45dB,volumedetect",
    "-f", "null", "-"], { encoding: "utf8" });
  const peak = Number(/max_volume:\s*(-?[\d.]+) dB/.exec(probe.stderr)?.[1] ?? 0);
  execSync(`ffmpeg -hide_banner -loglevel error -y -i ${JSON.stringify(win.wav)} ` +
    `-af "silenceremove=start_periods=1:start_threshold=-45dB,volume=${(-1 - peak).toFixed(1)}dB" ` +
    `-ar 48000 -ac 1 ${JSON.stringify(`${REAL}/${role}.wav`)}`);

  picks[role] = {
    id: win.sound.id, name: win.sound.name, by: win.sound.username,
    license: win.sound.license,
    url: `https://freesound.org/s/${win.sound.id}/`,
    centroid_hz: Math.round(win.c),
    duration_s: win.sound.duration,
  };
  console.log(`  → ${role}.wav = #${win.sound.id} “${win.sound.name}” by ${win.sound.username}`);
}

writeFileSync(`${REAL}/kit-real.json`, JSON.stringify(picks, null, 2));
console.log(`\n✓ ${REAL}/kit-real.json`);
