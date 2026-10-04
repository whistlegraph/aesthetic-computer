#!/usr/bin/env node
// collection.mjs — write scores/macneopolitan.mbscore, the Trio's album.
//
//   node bin/collection.mjs            # rewrite scores/macneopolitan.mbscore
//   node bin/collection.mjs --print    # show the track list, write nothing
//
// A collection is a `.mbscore` whose `tracks` name sibling `.mbscore` files
// instead of carrying `voices` (slab/menuband/scores/README.md → Collections).
// Opening it in Menu Band shows the list; `node bin/trio.mjs
// scores/macneopolitan.mbscore` prints it. Each entry carries the sibling's
// title, bpm, member count and length as hints, so a reader can draw the
// list without opening thirty files — the sibling stays the truth.
//
// Sections follow the lane's own lists: the four movements (setlist.json),
// the songbook (songbook.json), the dialogs (dialogs.json), then the fleet
// arrangements and the studies that fit nowhere else, in name order.
import { readFileSync, writeFileSync, readdirSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SCORES = resolve(HERE, "..", "scores");
const OUT = resolve(SCORES, "macneopolitan.mbscore");
const printOnly = process.argv.includes("--print");

const readJson = (p) => JSON.parse(readFileSync(p, "utf8"));
const beatsOf = (s) => String(s || "").split(",").reduce((a, t) => a + (parseFloat(t.split(":")[1]) || 0), 0);
const voiceBeats = (v) => Math.max(0, ...["notes", "notes2", "notes3", "notes4"].map((k) => beatsOf(v[k])));

// The lane's titles all wear the band's name; the album already says it.
function shortTitle(title, file) {
  let t = String(title || basename(file, ".mbscore"));
  t = t.replace(/^The MacNeoPolitan Trio\s+—\s+/, "");
  t = t.replace(/\s+—\s+MacNeoPolitan Trio$/, "");
  t = t.replace(/\s+—\s+MacNeoPolitan (.+)$/, " ($1)");
  return t;
}

function entry(file, section) {
  const score = readJson(resolve(SCORES, file));
  if (score.tracks) return null;                       // a collection is not a track
  const voices = score.voices || [];
  const bpm = score.bpm || 120;
  const beats = Math.max(0, ...voices.map(voiceBeats));
  const e = {
    file,
    title: shortTitle(score.title, file),
    section,
    machines: score.machines ?? voices.length,
    bpm,
    seconds: Math.round(beats * 60 / bpm * 10) / 10,
  };
  if (score.requiresFleet) e.requiresFleet = true;
  if (score.phonemeOnly) e.phonemeOnly = true;
  return e;
}

const tracks = [];
const seen = new Set();
const add = (file, section) => {
  if (seen.has(file)) return;
  const e = entry(file, section);
  if (!e) return;
  seen.add(file);
  tracks.push(e);
};

for (const f of readJson(resolve(SCORES, "setlist.json")).movements) add(f, "Movements");
for (const f of readJson(resolve(SCORES, "songbook.json")).movements) add(f, "Songbook");
for (const f of readJson(resolve(SCORES, "dialogs.json")).movements) add(f, "Dialogs");
const rest = readdirSync(SCORES).filter((f) => f.endsWith(".mbscore") && !seen.has(f) && f !== basename(OUT)).sort();
for (const f of rest) if (readJson(resolve(SCORES, f)).requiresFleet) add(f, "Fleet arrangements");
for (const f of rest) add(f, "Studies");

const total = tracks.reduce((a, t) => a + t.seconds, 0);
const collection = {
  title: "The MacNeoPolitan Trio",
  composer: "The machines — neo, blueberry and frisbee — with jeffrey",
  machines: 3,
  description: `Every score the Trio knows, in one album: the four movements, the songbook, the ten dialogs, the fleet arrangements and the studies. ${tracks.length} tracks, about ${Math.round(total / 60)} minutes. Open it in Menu Band to pick a track; node bin/trio.mjs scores/macneopolitan.mbscore lists them.`,
  gap: 4,
  tracks,
};

if (printOnly) {
  let section = null;
  for (const [i, t] of tracks.entries()) {
    if (t.section !== section) { section = t.section; console.log(`\n${section}`); }
    const m = Math.floor(t.seconds / 60), s = String(Math.round(t.seconds % 60)).padStart(2, "0");
    console.log(`  ${String(i + 1).padStart(2)}. ${t.title.padEnd(34)} ${"●".repeat(t.machines)}  ${String(t.bpm).padStart(3)} bpm  ${m}:${s}${t.requiresFleet ? "  fleet" : ""}`);
  }
  console.log(`\n${tracks.length} tracks · ${Math.round(total / 60)} min`);
} else {
  writeFileSync(OUT, JSON.stringify(collection, null, 2) + "\n");
  console.log(`wrote ${OUT} — ${tracks.length} tracks, ${Math.round(total / 60)} min`);
}
