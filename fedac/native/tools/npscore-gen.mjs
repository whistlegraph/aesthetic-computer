#!/usr/bin/env node
// npscore-gen — composes a long-form .npscore (see mbscore-to-npscore.mjs
// for the format): pad, bass, arpeggio, lead, and a percussion voice over
// an i-VI-III-VII cycle, arranged intro → (groove → break → build → drop)
// × N → outro. Velocity carries the arrangement's breath — soft breaks dim
// the room, builds ramp it, kicks flash white — since the player maps
// velocity to both volume and light gain.
//
// --style lullaby swaps the club for the nursery: a 3/4 music-box
// pentatonic melody over I-vi-IV-V, a warm dyad every other bar, block
// and tambourine ticks — low velocities so the room glows dim and the
// sparse events sit clear of frame-boundary jitter.
//
//   node npscore-gen.mjs [--minutes 4] [--seed 7] [--bpm 122]
//                        [--style lullaby] [out.npscore]

import { writeFileSync } from "node:fs";

const arg = (k, d) => {
  const i = process.argv.indexOf("--" + k);
  return i > 0 ? parseFloat(process.argv[i + 1]) : d;
};
const styleIdx = process.argv.indexOf("--style");
const style = styleIdx > 0 ? process.argv[styleIdx + 1] : "club";
const minutes = arg("minutes", 4);
const seed = arg("seed", 7);
const bpm = arg("bpm", style === "lullaby" ? 63 : 122);
const outArg = process.argv.slice(2).filter(a => !a.startsWith("--") &&
  a !== String(minutes) && a !== String(seed) && a !== String(bpm) && a !== style)[0];

let s = seed >>> 0 || 1;
const rnd = () => (s = (s * 1664525 + 1013904223) >>> 0) / 2 ** 32;
const pick = (a) => a[Math.floor(rnd() * a.length)];

const NAMES1 = style === "lullaby"
  ? ["hush", "moth", "tide", "fern", "dune", "lull", "wisp", "eider"]
  : ["umber", "vesper", "cobalt", "ember", "sable", "aurora", "quartz", "willow"];
const NAMES2 = style === "lullaby"
  ? ["moon", "cradle", "lantern", "sleep", "shoal", "feather", "ember", "hollow"]
  : ["cascade", "orbit", "meadow", "signal", "harbor", "lattice", "drift", "engine"];
const name = pick(NAMES1) + "-" + pick(NAMES2);

const spb = 60 / bpm;
const bar = 4 * spb;
// A minor: i VI III VII — triads + bass roots.
const CHORDS = [
  { root: 33, triad: [57, 60, 64] }, // Am
  { root: 29, triad: [53, 57, 60] }, // F
  { root: 36, triad: [60, 64, 67] }, // C
  { root: 31, triad: [55, 59, 62] }, // G
];

// One cycle = groove 16 + break 8 + build 8 + drop 16 = 48 bars.
// intro 8 + cycles + outro 8 fill the asked minutes.
const cycleBars = 48;
const askedBars = Math.max(24, Math.round((minutes * 60) / bar));
const cycles = Math.max(1, Math.round((askedBars - 16) / cycleBars));

const pad = [], bass = [], arp = [], lead = [], drums = [];
const tone = (v, midi, start, dur, vel) =>
  v.push({ midi, start: +start.toFixed(4), dur: +dur.toFixed(4), velocity: Math.round(vel) });
const hit = (drum, start, vel) =>
  drums.push({ drum, start: +start.toFixed(4), velocity: Math.round(vel) });

let t = 0;
const chordAt = (barIdx) => CHORDS[barIdx % 4];

function padBar(b, vel) {
  const c = chordAt(b);
  for (const m of c.triad) tone(pad, m, t, bar * 0.98, vel);
}
function bassBar(b, vel) {
  const c = chordAt(b);
  for (let e = 0; e < 8; e++) // offbeat eighths, techno lope
    if (e % 2 === 1) tone(bass, c.root + 12, t + e * spb / 2, spb * 0.45, vel + rnd() * 8);
}
function arpBar(b, vel) {
  const c = chordAt(b);
  const tones = [...c.triad, c.triad[0] + 12, c.triad[1] + 12, c.triad[2] + 12];
  for (let x = 0; x < 16; x++) {
    const accent = x % 4 === 0 ? 14 : 0; // downbeat accents breathe the light
    tone(arp, tones[(x * 3 + b) % tones.length], t + x * spb / 4, spb * 0.22, vel + accent);
  }
}
function leadBar(b, vel) {
  const c = chordAt(b);
  const steps = [0, 2, 1, 2]; // slow motif over the triad
  for (let q = 0; q < 4; q++)
    if (rnd() < 0.7) tone(lead, c.triad[steps[(q + b) % 4]] + 12, t + q * spb, spb * 0.9, vel);
}
function drumBar(kind, ramp = 0) {
  for (let e = 0; e < 8; e++) {
    const et = t + e * spb / 2;
    if (kind !== "break") {
      if (e % 2 === 0 && kind !== "buildup") hit("kick", et, 115);
      if (e % 2 === 1) hit("hat-c", et, 55 + (e === 7 ? 25 : rnd() * 12));
    }
    if ((e === 2 || e === 6) && (kind === "full" || kind === "drop")) hit("clap", et, 100);
  }
  if (kind === "drop") hit("hat-o", t + 3.5 * spb, 70);
  if (kind === "buildup") // snare ladder — 8ths then 16ths, velocity climbing
    for (let x = 0; x < (ramp < 0.5 ? 8 : 16); x++)
      hit("snare", t + x * bar / (ramp < 0.5 ? 8 : 16), 60 + ramp * 60);
}

function section(bars, fn) { for (let b = 0; b < bars; b++) { fn(b); t += bar; } }

if (style === "lullaby") {
  // 3/4 nursery waltz: a music-box line wandering the C pentatonic over
  // I-vi-IV-V, a warm dyad every other bar, block tick on the downbeat,
  // tambourine breath every fourth bar. The last nine bars hush to nothing.
  const bar3 = 3 * spb;
  const PENTA = [60, 62, 64, 67, 69, 72];
  const ROOTS = [48, 45, 41, 43];
  const bars = Math.max(16, Math.round((minutes * 60) / bar3));
  let m = 2;
  for (let b = 0; b < bars; b++) {
    const fade = b > bars - 10 ? (bars - b) / 10 : 1;
    const root = ROOTS[b % 4];
    if (b % 2 === 0) {
      tone(pad, root + 12, t, bar3 * 1.9, 30 * fade);
      tone(pad, root + 19, t, bar3 * 1.9, 26 * fade);
    }
    let q = 0;
    while (q < 3) {
      if (q > 0 && rnd() < 0.25) { q++; continue; } // breaths between phrases
      m = Math.max(0, Math.min(PENTA.length - 1, m + Math.floor(rnd() * 3) - 1));
      const held = q === 0 && rnd() < 0.4;
      tone(arp, PENTA[m], t + q * spb, spb * (held ? 1.8 : 0.85), (36 + rnd() * 16) * fade);
      q += held ? 2 : 1;
    }
    hit("block", t, 28 * fade);
    if (b % 4 === 3) hit("tambo", t + 2 * spb, 22 * fade);
    t += bar3;
  }
} else {

section(8, b => { padBar(b, 45 + b * 2); if (b >= 4) bassBar(b, 80); }); // intro
for (let cy = 0; cy < cycles; cy++) {
  hit("crash", t, 120);
  section(16, b => { // groove
    padBar(b, 58); bassBar(b, 96); drumBar(b < 8 ? "full" : "drop");
    if (b >= 8) arpBar(b, 62);
  });
  section(8, b => { padBar(b, 40 - b * 2); arpBar(b, 48); drumBar("break"); }); // break
  section(8, b => { // build
    padBar(b, 45 + b * 4); bassBar(b, 90 + b * 3); arpBar(b, 55 + b * 5);
    drumBar("buildup", b / 8);
  });
  hit("crash", t, 127);
  section(16, b => { // drop
    padBar(b, 66); bassBar(b, 108); arpBar(b, 84); leadBar(b, 100);
    drumBar("drop");
  });
}
section(8, b => { padBar(b, 55 - b * 6); arpBar(b, Math.max(20, 50 - b * 6)); }); // outro
}

const score = {
  name, bpm, leadSeconds: 1, tailSeconds: 2,
  voices: style === "lullaby" ? [
    { name: "musicbox", program: 10, velocity: 45, notes: arp },
    { name: "warm", program: 89, velocity: 30, notes: pad },
    { name: "ticks", kind: "percussion", velocity: 30, notes: drums },
  ] : [
    { name: "pad", program: 89, velocity: 60, notes: pad },
    { name: "bass", program: 38, velocity: 100, notes: bass },
    { name: "arp", program: 4, velocity: 80, notes: arp },
    { name: "lead", program: 80, velocity: 100, notes: lead },
    { name: "drums", kind: "percussion", velocity: 110, notes: drums },
  ],
  // no lights: the player derives velocity-gained colors + white drum flashes
};

const dest = outArg || name + ".npscore";
writeFileSync(dest, JSON.stringify(score) + "\n");
const n = pad.length + bass.length + arp.length + lead.length + drums.length;
console.log(`${dest} — "${name}" ${Math.round(t / 60)}m${Math.round(t % 60)}s, ${n} events (${drums.length} drum hits), ${cycles} cycle(s)`);
