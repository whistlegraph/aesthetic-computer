#!/usr/bin/env node
// gen-struct.mjs — author the visualizer telemetry (struct.json) for the
// amaythingra release video. amaythingra is rendered by a bespoke C engine
// (c/amaythingra.c), which (unlike the JS pop tracks) does NOT emit a
// struct.json. This script reconstructs an equivalent from the engine's
// known bar map so the marimbaba/trancepenta-style score-panel + section
// narrative can animate against it.
//
// Track facts (from c/amaythingra.c): 120 BPM, 4/4, G major, 273 s (4:33).
//   bars  0-15   (0-32s)    INTRO   — sine pads + church bells, kick from ~bar 3
//   bars 16-31   (32-64s)   BUILD   — WOOP at 32s; kick+shaker, powersaws+bells
//   bars 32-47   (64-96s)   DROP 1  — full kit + chord stab
//   bars 48-55   (96-112s)  BREAK   — pads + resolved chord-tone bell melody
//   bars 56-63   (112-128s) VORTEX  — scratch-glissando bell storm into drop 2
//   bars 64-95   (128-192s) DROP 2  — full kit + glock; kick arpeggio @164-178s
//   bars 96-123  (192-248s) OUTRO   — 3:15 mix-switch, perc steps back; reverse
//                                     kick swooshes ~3:00; vowel vocal in @213s
//   bars 124-136 (248-273s) RESOLVE — wind-down, final tolls, descending outro
//                                     melody, vowel finale, 7 s tail fade
//
// Writes: pop/big-pictures/out/amaythingra.struct.json  (and a copy next to
//         the mp3 under <track>.assets/struct.json, where the builders look).
//
// Events are REPRESENTATIVE (the engine's per-render chord progression is
// random, so pitches use a fixed G-pentatonic stand-in) — enough to drive a
// rhythmically/melodically honest score panel.

import { writeFileSync, mkdirSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");
const OUT  = resolve(POP, "big-pictures/out/amaythingra.struct.json");
const TRACK_ASSETS = `${homedir()}/Documents/Shelf/amaythingra/amaythingra.mp3.assets`;

const BPM = 120, SPB = 60 / BPM, BAR = 4 * SPB;   // 0.5 s beat, 2.0 s bar
const TOTAL = 273;

// ── sections (8) — musical arc + narrative beats for the illustrations ──
const sections = [
  { name: "intro",   startSec: 0,   endSec: 32 },
  { name: "build",   startSec: 32,  endSec: 64 },
  { name: "drop1",   startSec: 64,  endSec: 96 },
  { name: "break",   startSec: 96,  endSec: 112 },
  { name: "vortex",  startSec: 112, endSec: 128 },
  { name: "drop2",   startSec: 128, endSec: 192 },
  { name: "outro",   startSec: 192, endSec: 248 },
  { name: "resolve", startSec: 248, endSec: 273 },
];

// ── helpers ─────────────────────────────────────────────────────────
const barT = (bar) => bar * BAR;
const inBreak = (bar) => bar >= 48 && bar < 64;           // kit drops here
// G major pentatonic stand-in pitches (the engine's real prog is random)
const PENT = [55, 57, 59, 62, 64, 67, 69, 71, 74, 76]; // G3..D5
const ROOTS = [43, 48, 43, 50];                          // G C G D (per 4-bar block)

const events = { kick: [], sub: [], glock: [], pad: [] };

// percMult mirror (for gain shaping in the back third, matches the engine)
function ramp01(t, a, b) { return t <= a ? 0 : t >= b ? 1 : (t - a) / (b - a); }
function percGain(t) {
  let g = 1 - 0.55 * ramp01(t, 195, 225);
  g *= 1 - 0.80 * ramp01(t, 214, 230);
  g *= 1 - ramp01(t, 255, 268);
  return g;
}

for (let bar = 0; bar < Math.floor(TOTAL / BAR); bar++) {
  const t0 = barT(bar);
  const kickOn = bar >= 3 && !inBreak(bar) && bar < 134;   // groove window
  if (kickOn) {
    const pg = percGain(t0);
    for (let beat = 0; beat < 4; beat++) {
      // 4-on-floor + offbeat 8th "heartbeat" kick
      events.kick.push({ t: +(t0 + beat * SPB).toFixed(3), dur: 0.18, midi: 36, gain: +(0.9 * pg).toFixed(3) });
      events.kick.push({ t: +(t0 + (beat + 0.5) * SPB).toFixed(3), dur: 0.14, midi: 36, gain: +(0.45 * pg).toFixed(3) });
    }
    // sub-bass follows the downbeats
    events.sub.push({ t: +t0.toFixed(3), dur: BAR, midi: 31, gain: +(0.5 * pg).toFixed(3) });
  }
  // pad chord every 4 bars (bars 4..end), sustained
  if (bar >= 4 && bar % 4 === 0) {
    const root = ROOTS[(bar / 4) % 4];
    events.pad.push({ t: +t0.toFixed(3), dur: 4 * BAR, midi: root, gain: 0.5 });
    events.pad.push({ t: +t0.toFixed(3), dur: 4 * BAR, midi: root + 7, gain: 0.4 });
  }
}

// glock: break resolved melody (bars 48-55, 2/bar), vortex storm (56-63,
// dense), drop2 arpeggio (64-95, on beats), + kick-arp marker (82-89).
for (let bar = 48; bar < 56; bar++) {
  events.glock.push({ t: +(barT(bar)).toFixed(3), dur: 1.6, midi: PENT[5], gain: 0.5 });
  events.glock.push({ t: +(barT(bar) + 2 * SPB).toFixed(3), dur: 1.6, midi: PENT[3], gain: 0.45 });
}
for (let bar = 56; bar < 64; bar++) {                       // vortex: fast flicks
  const subdiv = bar < 60 ? 8 : 16;
  for (let s = 0; s < subdiv; s++) {
    events.glock.push({ t: +(barT(bar) + s * (BAR / subdiv)).toFixed(3), dur: 0.14,
      midi: PENT[(bar * 3 + s) % PENT.length], gain: 0.45 });
  }
}
for (let bar = 64; bar < 96; bar++) {                       // drop2 arpeggio
  for (let beat = 0; beat < 4; beat++) {
    events.glock.push({ t: +(barT(bar) + beat * SPB).toFixed(3), dur: 0.45,
      midi: PENT[(bar + beat) % PENT.length], gain: 0.45 });
  }
}
// outro descending resolve (256-262s)
const line = [76, 74, 71, 67, 64, 59];
line.forEach((m, i) => events.glock.push({ t: +(256 + i).toFixed(3), dur: 6 + i, midi: m, gain: 0.55 }));

// sort every lane by time
for (const k of Object.keys(events)) events.events?.length, events[k].sort((a, b) => a.t - b.t);

const struct = {
  _comment: "amaythingra visualizer telemetry — reconstructed from c/amaythingra.c (representative events; engine prog is random)",
  meter: 4, bpm: BPM, scale: "major", rootMidi: 67, totalSec: TOTAL, prerollSec: 0,
  sections, events,
};

mkdirSync(dirname(OUT), { recursive: true });
writeFileSync(OUT, JSON.stringify(struct, null, 2));
let copied = "";
if (existsSync(dirname(TRACK_ASSETS)) || true) {
  mkdirSync(TRACK_ASSETS, { recursive: true });
  writeFileSync(`${TRACK_ASSETS}/struct.json`, JSON.stringify(struct, null, 2));
  copied = ` + ${TRACK_ASSETS}/struct.json`;
}
const counts = Object.entries(events).map(([k, v]) => `${k}:${v.length}`).join(" ");
console.log(`✓ ${OUT.replace(POP + "/", "pop/")}${copied}`);
console.log(`  ${sections.length} sections · events { ${counts} } · ${TOTAL}s @ ${BPM} BPM`);
