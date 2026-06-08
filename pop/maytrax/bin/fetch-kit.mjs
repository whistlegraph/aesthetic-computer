#!/usr/bin/env node
// fetch-kit.mjs — pull a matrix / big-beat sampled-INSTRUMENT kit from
// Freesound for maytrax. Two kinds of role:
//   • percussive one-shots (taiko, gong) — played at native pitch as accents
//   • pitchable instruments (choir "aah", brass hit, strings, bass) — these
//     are resampled per-note in maytrax.mjs to play the F-minor lines over
//     the .np / chord data ("sampled insts over MIDI").
//
//   node pop/maytrax/bin/fetch-kit.mjs            # fetch all roles
//   node pop/maytrax/bin/fetch-kit.mjs --list     # show 5 candidates per role
//   node pop/maytrax/bin/fetch-kit.mjs --pick "choir=1,brass=0"   # pin picks
//
// Writes pop/maytrax/kit.json. CC0 / CC-BY only (every track ships to
// DistroKid — no copyrighted film/break audio).

import { searchSounds, downloadPreview } from "../../lib/freesound.mjs";
import { writeFileSync, existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const KIT_PATH = resolve(HERE, "..", "kit.json");

// role → query + duration filter + (for pitchable insts) an assumed root
// note so the resampler knows the transpose origin. f0 is re-detected at
// load time in maytrax.mjs, so `root` is only a fallback.
// JUNGLE kit — a chopped breakbeat assembled from CC0 one-shots (no real
// amen break — it's a copyrighted recording), deep sub + reese bass, a jazzy
// sampled flute lead (the "main voice"), pads, orchestra hits.
const ROLES = {
  // break / drums (played at native pitch)
  kick:    { query: "breakbeat kick drum",       filter: "duration:[0.1 TO 1.0]" },
  snare:   { query: "breakbeat snare",           filter: "duration:[0.1 TO 1.2]" },
  ghost:   { query: "ghost snare drum",          filter: "duration:[0.05 TO 0.6]" },
  hat:     { query: "closed hihat",              filter: "duration:[0.02 TO 0.5]" },
  ohat:    { query: "open hihat",                filter: "duration:[0.05 TO 1.0]" },
  ride:    { query: "jazz ride cymbal",          filter: "duration:[0.3 TO 4]" },
  shaker:  { query: "shaker percussion",         filter: "duration:[0.03 TO 1]" },
  cowbell: { query: "cowbell",                   filter: "duration:[0.05 TO 1]" },
  tom:     { query: "tom drum",                  filter: "duration:[0.1 TO 1.5]" },
  click:   { query: "click percussion",          filter: "duration:[0.01 TO 0.4]" },
  gong:    { query: "gong hit",                  filter: "duration:[1 TO 6]" },
  // ambience beds (played raw, long) — rainstorm + jungle for the opener
  rain:    { query: "rain storm ambience",       filter: "duration:[5 TO 40]" },
  jungle:  { query: "jungle birds ambience",     filter: "duration:[5 TO 40]" },
  cricket: { query: "crickets meadow night",     filter: "duration:[5 TO 40]" },
  // pitchable instruments (resampled per-note over the F-minor MIDI)
  sub:     { query: "sub bass note sine",        filter: "duration:[0.3 TO 3]", root: "C1", pitched: true },
  reese:   { query: "reese bass dnb",            filter: "duration:[0.3 TO 4]", root: "C2", pitched: true },
  lead:    { query: "flute single note",         filter: "duration:[0.5 TO 4]", root: "C4", pitched: true },
  pad:     { query: "warm synth pad",            filter: "duration:[2 TO 8]",   root: "C3", pitched: true },
  choir:   { query: "choir aah voice",           filter: "duration:[1 TO 6]",  root: "C4", pitched: true },
  // John Williams-style cinematic legato string swell (Fm chord pads)
  strings: { query: "cinematic orchestral strings", filter: "duration:[2 TO 9]", root: "C4", pitched: true },
  // animal "horn" — replaces the cheesy orchestra-hit stab. A tonal big-cat
  // roar pitched to the Fm walk = a jungle creature singing the stabs.
  animal:  { query: "jaguar growl roar",         filter: "duration:[1 TO 5]",  root: "C3", pitched: true },
  wolf:    { query: "wolf howling",              filter: "duration:[1 TO 8] license:\"Creative Commons 0\"", root: "A3", pitched: true },
};

const argv = process.argv.slice(2);
const LIST = argv.includes("--list");
const pins = {};
for (let i = 0; i < argv.length; i++) {
  if (!argv[i].startsWith("--pick")) continue;
  const specs = [];
  if (argv[i].includes("=")) specs.push(...argv[i].replace(/^--pick=?/, "").split(","));
  while (argv[i + 1] && !argv[i + 1].startsWith("--")) specs.push(...argv[++i].split(","));
  for (const s of specs) { const [r, n] = s.split("="); if (r && n !== undefined) pins[r.trim()] = parseInt(n, 10) || 0; }
}

const kit = existsSync(KIT_PATH) ? JSON.parse(readFileSync(KIT_PATH, "utf8")) : {};

for (const [role, spec] of Object.entries(ROLES)) {
  try {
    const data = await searchSounds({ query: spec.query, filter: spec.filter, pageSize: 5 });
    const results = data.results || [];
    if (LIST) {
      console.log(`\n${role}  (${spec.query})`);
      results.forEach((s, i) => console.log(`  [${i}] ${s.name} · ${s.duration.toFixed(2)}s · ${s.username} · ${s.license.split("/").slice(-3, -1).join("/")}`));
      continue;
    }
    const idx = pins[role] ?? 0;
    const chosen = results[idx] || results[0];
    if (!chosen) { console.warn(`! no result for ${role}`); continue; }
    const mp3 = await downloadPreview(chosen);
    const wav = mp3.replace(/\.mp3$/, ".wav");
    kit[role] = {
      path: existsSync(wav) ? wav : mp3,
      id: chosen.id, name: chosen.name, username: chosen.username,
      license: chosen.license, duration: chosen.duration,
      pitched: !!spec.pitched, root: spec.root || null,
    };
    console.log(`✓ ${role.padEnd(8)} ${chosen.name}  (#${chosen.id} · ${chosen.username})`);
  } catch (e) {
    console.error(`✗ ${role}: ${e.message}`);
  }
}

if (!LIST) {
  writeFileSync(KIT_PATH, JSON.stringify(kit, null, 2) + "\n");
  console.log(`\n✓ wrote ${KIT_PATH} (${Object.keys(kit).length} roles)`);
}
