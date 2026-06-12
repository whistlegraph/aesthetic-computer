#!/usr/bin/env node
// scorodeon-data.mjs — adapt momabobasheep's composed events
// (out/momabobasheep.events.json, from score-extract.mjs) into the generic
// scorodeon score format (see pop/bin/scorodeon.mjs). Each lane keeps the
// graphic score's palette (bin/build-score.mjs) so the flythrough and the
// still score read as the same artwork.
//
// Run:  node pop/momboba/bin/scorodeon-data.mjs
// Out:  pop/momboba/out/momabobasheep.scorodeon.json

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const D = JSON.parse(readFileSync(resolve(HERE, "../out/momabobasheep.events.json"), "utf8"));

const ev = (e) => ({ t: e.t, dur: e.dur, pitch: e.midi, g: e.g });
const score = {
  title: "momabobasheep",
  artist: "aesthetic dot computer",
  dur: D.masterSec,
  movements: D.movements.map((m) => ({
    name: m.name, sub: m.sub, t0: m.startSec, t1: m.endSec, level: m.level,
  })),
  chords: D.chords.map((c) => ({ t: c.t, dur: c.dur, name: c.name })),
  arc: D.arc,                       // per-second 0..1 dynamic curve
  goldenSec: D.goldenSec,
  lanes: [
    { name: "whistle",  color: "#2A6F77", events: D.whistle.map(ev) },
    { name: "jeffrey",  color: "#8E5A77", events: (D.jeffrey ?? []).map(ev) },
    { name: "bells",    color: "#6E5A8E", events: D.bells.map((b) => ({
        t: b.t, dur: b.dur, pitch: Math.max(...b.midis), g: b.gain })) },
    { name: "sparkle",  color: "#C08A2D", events: D.sparkle.map(ev) },
    { name: "arpeggio", color: "#A65A3A", events: D.arp.map(ev) },
    { name: "alto",     color: "#7A7B45", events: D.alto.map(ev) },
    { name: "pad",      color: "#5B7FA6", events: D.pad.map(ev) },
    { name: "bass",     color: "#6B4A36", events: D.bass.map(ev) },
  ],
  ribbons: [
    // droneEnv alone is ~1.0 from sneak-in to tail (a flat slab on screen) —
    // weight it by the dynamic arc so the ribbon breathes with the night.
    { name: "drone bed + sub", color: "#8A8378",
      env: D.droneEnv.map((v, i) => v * (0.30 + 0.70 * (D.arc[i] ?? 0.5))) },
  ],
};

const out = resolve(HERE, "../out/momabobasheep.scorodeon.json");
writeFileSync(out, JSON.stringify(score) + "\n");
console.log(`✓ ${out} · ${score.lanes.length} lanes · ${score.chords.length} chords · ${score.dur}s`);
