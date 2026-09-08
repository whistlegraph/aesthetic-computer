#!/usr/bin/env node
// scorodeon → nsscore. Special Sign's release score (13 named+colored
// lanes of {t, dur, pitch, g, kind} events, six movements, ribbons — the
// spatial-rotation envelope included) becomes the Notepat Spatial
// performance score:
//
//   .nsscore
//   {
//     "name", "dur",
//     "movements": [{ "name", "sub", "t0", "t1", "level" }],
//     "rotation": [..],            // spatial-rotation ribbon env, verbatim
//     "lanes": [{ "name", "color": [r,g,b],
//                 "events": [{ "t", "dur", "hz", "wave", "g" }] }]
//   }
//
// Pitch may be fractional midi (microtonal) — converted to hz here so the
// player stays dumb. Per-event synthesis kinds resolve to native waves.
//
//   node scorodeon-to-nsscore.mjs <scorodeon.json> [out.nsscore]

import { readFileSync, writeFileSync } from "node:fs";

const WAVE = {
  tone: "sine", triangle: "triangle", whistle: "whistle",
  resonator: "triangle", noise: "noise", rotor: "noise",
};

const [, , inPath, outPath] = process.argv;
if (!inPath) {
  console.error("usage: scorodeon-to-nsscore.mjs <scorodeon.json> [out.nsscore]");
  process.exit(1);
}

const s = JSON.parse(readFileSync(inPath, "utf8"));
const hz = (m) => +(440 * Math.pow(2, (m - 69) / 12)).toFixed(3);
const hexRgb = (h) => [1, 3, 5].map(i => parseInt(h.slice(i, i + 2), 16));

const lanes = (s.lanes || []).map(l => ({
  name: l.name,
  color: hexRgb(l.color || "#808080"),
  events: (l.events || []).map(e => ({
    t: +e.t.toFixed(4), dur: +(e.dur ?? 0.2).toFixed(4),
    ...(e.pitch != null ? { hz: hz(e.pitch) } : {}),
    wave: WAVE[e.kind] || "sine",
    g: +(e.g ?? 0.5).toFixed(3),
  })),
}));

const rotation = (s.ribbons || []).find(r => /rotation/i.test(r.name || ""));
const out = {
  name: (s.title || "untitled").toLowerCase().replace(/[^a-z0-9]+/g, "-"),
  dur: s.dur,
  movements: s.movements || [],
  ...(rotation ? { rotation: rotation.env } : {}),
  lanes,
};

const dest = outPath || out.name + ".nsscore";
writeFileSync(dest, JSON.stringify(out) + "\n");
const n = lanes.reduce((a, l) => a + l.events.length, 0);
console.log(`${dest} — ${lanes.length} lanes, ${n} events, ${Math.round(s.dur)}s, ${out.movements.length} movements${rotation ? ", rotation ribbon" : ""}`);
