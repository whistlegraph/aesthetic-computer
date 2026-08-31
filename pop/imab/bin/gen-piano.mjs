#!/usr/bin/env node
// gen-piano.mjs — the grand piano accompaniment for imabclub: a real
// GM grand (FluidSynth + GeneralUser GS) reading a MIDI this script
// writes. Arpeggios in the intro and the break, sustained chords with
// a beat-3 restrike through the passes, fuller after the drop, sparse
// whole notes peeling out. Chord map = the track's C/F/G per bar.
//
//   node pop/imab/bin/gen-piano.mjs
//   → ~/.cache/ac/imab/piano.wav (imabclub.mjs mixes it)

import { writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";

const WORK = `${process.env.HOME}/.cache/ac/imab`;
const SF2 = `${process.env.HOME}/.cache/ac/soundfonts/GeneralUserGS.sf2`;
const BARS = 96, TPQN = 480, BPM = 124;
const CH = { C: [48, 60, 64, 67], F: [41, 57, 60, 65], G: [43, 55, 59, 62] };
const MAP16 = ["C", "C", "C", "C", "C", "F", "F", "G", "C", "C", "C", "C", "C", "C", "C", "C"];
const act = (b) => (b < 16 ? "arp" : b < 56 ? "chords" : b < 64 ? "arp-high" : b < 80 ? "full" : "sparse");

const ev = [];                                     // {tick, bytes[]}
const on = (t, n, v) => ev.push({ t, b: [0x90, n, v] });
const off = (t, n) => ev.push({ t, b: [0x80, n, 0] });
const note = (tick, n, durTicks, vel) => { on(tick, n, vel); off(tick + durTicks, n); };
const B = 4 * TPQN;                                 // one bar in ticks
for (let bar = 0; bar < BARS; bar++) {
  const chord = CH[MAP16[bar % 16]];
  const t0 = bar * B, a = act(bar);
  if (a === "arp" || a === "arp-high") {
    const lift = a === "arp-high" ? 12 : 0;
    const seq = [...chord, chord[2], chord[1]];     // up and part-way back
    for (let k = 0; k < 8; k++)
      note(t0 + (k * TPQN) / 2, seq[k % seq.length] + lift + (k >= 4 ? 12 : 0) * (a === "arp-high" ? 1 : 0),
           TPQN * 0.9, 48 + (k % 4 === 0 ? 12 : 0));
    if (bar % 4 === 0) note(t0, chord[0] - 12, B * 0.95, 52);
  } else if (a === "chords") {
    note(t0, chord[0] - 12, B * 0.95, 58);
    for (const n of chord.slice(1)) note(t0, n, 2 * TPQN * 0.95, 62);
    for (const n of chord.slice(1)) note(t0 + 2 * TPQN, n, 2 * TPQN * 0.9, 50);
  } else if (a === "full") {
    note(t0, chord[0] - 12, B * 0.95, 68);
    for (const n of chord.slice(1)) { note(t0, n, TPQN * 1.8, 72); note(t0 + 2 * TPQN, n + 12, TPQN * 1.8, 60); }
    note(t0 + 3 * TPQN, chord[3] + 12, TPQN * 0.9, 55);
  } else if (bar % 2 === 0) {
    note(t0, chord[0] - 12, 2 * B * 0.95, 50);
    for (const n of chord.slice(1)) note(t0, n, 2 * B * 0.9, 46);
  }
}
ev.sort((x, y) => x.t - y.t);
const vlq = (n) => { const out = []; let v = n & 0x7f; while ((n >>= 7)) out.unshift((n & 0x7f) | 0x80); out.push(v); return out.length > 1 ? [...out.slice(0, -1), v] : [v]; };
const trk = [0, 0xff, 0x51, 3, ...[(60000000 / BPM) >> 16 & 0xff, (60000000 / BPM) >> 8 & 0xff, (60000000 / BPM) & 0xff], 0, 0xc0, 0];
let last = 0;
for (const e of ev) { trk.push(...vlq(e.t - last), ...e.b); last = e.t; }
trk.push(0, 0xff, 0x2f, 0);
const u32 = (n) => [n >> 24 & 255, n >> 16 & 255, n >> 8 & 255, n & 255];
const mid = Buffer.from([0x4d, 0x54, 0x68, 0x64, ...u32(6), 0, 0, 0, 1, TPQN >> 8, TPQN & 255,
  0x4d, 0x54, 0x72, 0x6b, ...u32(trk.length), ...trk]);
writeFileSync(`${WORK}/piano.mid`, mid);
const r = spawnSync("fluidsynth", ["-ni", "-g", "0.7", "-F", `${WORK}/piano-raw.wav`, "-r", "48000", SF2, `${WORK}/piano.mid`],
  { stdio: ["ignore", "ignore", "inherit"] });
if (r.status !== 0) { console.error("✗ fluidsynth failed"); process.exit(1); }
spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", `${WORK}/piano-raw.wav`,
  "-ac", "1", "-ar", "48000", "-af", "highpass=f=60", `${WORK}/piano.wav`], { stdio: ["ignore", "ignore", "inherit"] });
console.log(`✓ ${WORK}/piano.wav`);
