#!/usr/bin/env node
// fetch-zoo.mjs — source the FLATTERBOP180 zoo: fresh animal one-shots from
// freesound (CC0 / CC-BY only — DistroKid-safe), converted to 48k mono WAV
// with leading-silence trim + peak normalize, landed in ../samples/zoo/.
// Attributions (id / author / license / url) go to ../samples/zoo/zoo.json —
// CC-BY credits belong in the release notes.
//
//   node pop/marimba/bin/fetch-zoo.mjs           # fetch all roles
//   node pop/marimba/bin/fetch-zoo.mjs monkey    # one role
//
// Each role = a musical chair in the menagerie: a query + a duration window
// sized for a one-shot accent (this is a fast 3/4 bop — short calls only).

import { searchSounds, downloadPreview } from "../../lib/freesound.mjs";
import { execSync } from "node:child_process";
import { mkdirSync, writeFileSync, existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ZOO = resolve(HERE, "..", "samples", "zoo");
const MANIFEST = resolve(ZOO, "zoo.json");

// DistroKid-safe licenses only. freesound filter syntax wants full names.
const LICENSE = `license:("Creative Commons 0" OR "Attribution")`;

const ROLES = [
  { role: "monkey",   query: "monkey",                 dur: [0.3, 4],   tag: "monkey" },
  { role: "parrot",   query: "parrot squawk",          dur: [0.3, 3.5], tag: "parrot" },
  { role: "elephant", query: "elephant trumpet",       dur: [0.5, 5],   tag: "elephant" },
  { role: "sealion",  query: "sea lion bark",          dur: [0.3, 4] },
  { role: "lion",     query: "lion roar",              dur: [0.6, 5],   tag: "lion" },
  { role: "hyena",    query: "hyena laugh",            dur: [0.4, 5],   tag: "hyena" },
];

const only = process.argv[2] || null;
mkdirSync(ZOO, { recursive: true });
const manifest = existsSync(MANIFEST) ? JSON.parse(readFileSync(MANIFEST, "utf8")) : {};

for (const { role, query, dur, tag } of ROLES) {
  if (only && role !== only) continue;
  const filter = `${LICENSE} duration:[${dur[0]} TO ${dur[1]}]${tag ? ` tag:${tag}` : ""}`;
  const data = await searchSounds({ query, filter, sort: "rating_desc", pageSize: 10 });
  // real animals only — skip human imitations / instruments monkeying around
  const FAKE = /imitat|human|voice|mouth|clarinet|synth/i;
  const picks = (data.results || []).filter((s) => s.previews && !FAKE.test(s.name));
  if (!picks.length) { console.warn(`! ${role}: no results for "${query}"`); continue; }
  const s = picks[0];
  const mp3 = await downloadPreview(s);
  const wav = resolve(ZOO, `${role}.wav`);
  // 48k mono (engine SR), strip leading silence, tame to -1 dB peak
  execSync(
    `ffmpeg -hide_banner -loglevel error -y -i ${JSON.stringify(mp3)} ` +
    `-af "silenceremove=start_periods=1:start_threshold=-40dB,alimiter=limit=0.89" ` +
    `-ar 48000 -ac 1 -c:a pcm_f32le ${JSON.stringify(wav)}`
  );
  manifest[role] = {
    id: s.id, name: s.name, username: s.username, license: s.license,
    duration: s.duration, rating: s.avg_rating,
    source_url: `https://freesound.org/people/${s.username}/sounds/${s.id}/`,
    wav,
  };
  console.log(`✓ ${role} ← "${s.name}" by ${s.username} (${s.license.match(/zero|publicdomain/) ? "CC0" : "CC-BY"} · ${s.duration.toFixed(1)}s · ★${s.avg_rating?.toFixed(1) ?? "?"})`);
}

writeFileSync(MANIFEST, JSON.stringify(manifest, null, 2) + "\n");
console.log(`→ ${MANIFEST}`);
