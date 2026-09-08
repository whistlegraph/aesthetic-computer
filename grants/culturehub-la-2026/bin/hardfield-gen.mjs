#!/usr/bin/env node
// hardfield-gen — the hardcore club version of the spatial test. 175 BPM,
// distorted kick, and everything that isn't the kick moves: the room
// becomes the effect rather than a place the effect happens in.
//
// The spatial arrangement is the arrangement:
//   kick    — pinned CENTRE and LOW, close. It never moves; it is the floor.
//   bass     — pinned centre, just under the kick.
//   hats     — pinned HIGH, front-left. Above your head, always.
//   ride     — pinned HIGH, front-right. The other half of the ceiling.
//   stab     — ORBITS. Short, hard, many — the thing that flies past you.
//   lead     — ORBITS. Short decays, lots of notes: an acid line in motion.
//   siren    — ORBITS overhead on the risers.
//
//   node hardfield-gen.mjs [out.nsscore] [--bars 64] [--bpm 175]

import { writeFileSync } from "node:fs";

const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf("--" + k); return i >= 0 ? parseFloat(args[i + 1]) : d; };
const BPM = opt("bpm", 175);
const BARS = opt("bars", 64);
const B = 60 / BPM;          // one beat
const BAR = 4 * B;
const S16 = B / 4;           // sixteenth

const kick = [], bass = [], hats = [], ride = [], stab = [], lead = [], siren = [];
const at = (bar, beat = 0) => bar * BAR + beat * B;

// A minor pentatonic, two octaves — the acid ladder.
const ROOT = 55; // A1
const SCALE = [0, 3, 5, 7, 10, 12, 15, 17, 19, 22, 24];
const hz = (semi) => +(ROOT * Math.pow(2, semi / 12)).toFixed(2);

// ── structure: eight-bar blocks, hardcore shape ──────────────────────
const SECTIONS = [
  { name: "I · Load in", sub: "kick alone, the floor arrives", bars: 8, kick: 1, hats: 0, stab: 0, lead: 0, drive: 2.2 },
  { name: "II · Hats up", sub: "the ceiling starts ticking", bars: 8, kick: 1, hats: 1, stab: 0, lead: 0.6, drive: 2.6 },
  { name: "III · Drop", sub: "everything, spinning", bars: 16, kick: 1, hats: 1, stab: 1, lead: 1, drive: 3.2 },
  { name: "IV · Break", sub: "no floor — only the room", bars: 8, kick: 0, hats: 0.5, stab: 1, lead: 1, drive: 2.4 },
  { name: "V · Riser", sub: "the siren comes overhead", bars: 8, kick: 1, hats: 1, stab: 0.6, lead: 1, drive: 3.4 },
  { name: "VI · Slam", sub: "the hardest pass, then nothing", bars: 16, kick: 1, hats: 1, stab: 1, lead: 1, drive: 4.0 },
];

const movements = [];
let bar = 0;
for (const sec of SECTIONS) {
  const t0 = at(bar);
  for (let b = 0; b < sec.bars; b++, bar++) {
    const barT = at(bar);
    const last = b === sec.bars - 1;

    // KICK — four on the floor, hard. Every downbeat, plus a rolling
    // sixteenth pair at the end of every fourth bar.
    if (sec.kick) {
      for (let beat = 0; beat < 4; beat++)
        kick.push({ t: +(barT + beat * B).toFixed(4), dur: 0.26, hz: 46, wave: "kick",
                    sweep: 4.6, sweepMs: 18, decay: 0.085, drive: sec.drive, g: 0.95 });
      if (b % 4 === 3)
        for (const off of [3.5, 3.75])
          kick.push({ t: +(barT + off * B).toFixed(4), dur: 0.18, hz: 46, wave: "kick",
                      sweep: 4.2, sweepMs: 14, decay: 0.06, drive: sec.drive, g: 0.8 });
      // BASS — offbeat eighths under it
      for (let e = 0; e < 8; e++)
        if (e % 2 === 1)
          bass.push({ t: +(barT + e * B / 2).toFixed(4), dur: 0.14, hz: hz(SCALE[b % 3]),
                      wave: "square", decay: 0.05, drive: 1.8, g: 0.42 });
    }

    // HATS — offbeat sixteenths above you, with a closing roll
    if (sec.hats) {
      for (let s = 0; s < 16; s++) {
        if (s % 2 === 0) continue;
        hats.push({ t: +(barT + s * S16).toFixed(4), dur: 0.035, hz: 11000, wave: "click",
                    decay: 0.012, g: 0.4 * sec.hats });
      }
      if (last) for (let s = 12; s < 16; s++)
        ride.push({ t: +(barT + s * S16).toFixed(4), dur: 0.05, hz: 7000, wave: "click",
                    decay: 0.02, g: 0.5 });
      if (b % 2 === 1)
        ride.push({ t: +(barT + 2 * B).toFixed(4), dur: 0.09, hz: 5200, wave: "click", decay: 0.04, g: 0.45 });
    }

    // STAB — hard offbeat chord hits that fly around the room
    if (sec.stab) for (const beat of [1.5, 3.5]) {
      for (const iv of [0, 7, 12])
        stab.push({ t: +(barT + beat * B).toFixed(4), dur: 0.13, hz: hz(24 + iv + (b % 4 === 3 ? 2 : 0)),
                    wave: "saw", decay: 0.045, drive: 2.4, g: 0.3 * sec.stab });
    }

    // LEAD — the acid ladder: sixteenths, short decays, many notes
    if (sec.lead) {
      for (let s = 0; s < 16; s++) {
        if (Math.random() > 0.62 * sec.lead) continue;
        const step = SCALE[(s * 3 + b * 2) % SCALE.length] + 24;
        lead.push({ t: +(barT + s * S16).toFixed(4), dur: 0.075, hz: hz(step),
                    wave: "saw", decay: 0.028, drive: 2.0, g: 0.24 * sec.lead });
      }
    }
  }

  // SIREN — a rising sweep across the last two bars of the riser
  if (sec.name.startsWith("V ·")) {
    for (let i = 0; i < 24; i++)
      siren.push({ t: +(at(bar - 2) + i * (BAR * 2 / 24)).toFixed(4), dur: 0.16,
                   hz: 300 + i * 95, wave: "saw", decay: 0.09, drive: 1.6, g: 0.3 });
  }
  movements.push({ ...sec, t0: +t0.toFixed(3), t1: +at(bar).toFixed(3), level: sec.drive / 4 });
}

const dur = +at(bar).toFixed(3);

// ── motion: the ribbons ──────────────────────────────────────────────
const N = 512;
const inM = (i, τ) => τ >= movements[i].t0 && τ < movements[i].t1;
const spinAt = (τ) => {
  if (inM(0, τ)) return 0.2;
  if (inM(1, τ)) return 0.6;
  if (inM(2, τ)) return 1.6;          // drop: hard rotation
  if (inM(3, τ)) return 2.4;          // break: fastest — the room is the drop
  if (inM(4, τ)) return 1.2;
  return 2.0;                          // slam
};
const elevAt = (τ) => {
  if (inM(3, τ)) { const u = (τ - movements[3].t0) / (movements[3].t1 - movements[3].t0); return Math.sin(Math.PI * u) * 0.8; }
  if (inM(4, τ)) { const u = (τ - movements[4].t0) / (movements[4].t1 - movements[4].t0); return u * 0.9; }
  if (inM(5, τ)) { const u = (τ - movements[5].t0) / (movements[5].t1 - movements[5].t0); return Math.cos(u * Math.PI * 3) * 0.45; }
  return 0;
};
const distAt = (τ) => (inM(2, τ) || inM(5, τ) ? 0.08 : 0.28);
const ramp = (fn) => Array.from({ length: N }, (_, i) => +fn((i / (N - 1)) * dur).toFixed(4));

const score = {
  name: "hardfield",
  bpm: BPM,
  dur,
  masterDrive: 2.1,                    // club glue on the summed ears
  masterTarget: 0.25,                  // hardcore-loud, short of crushed
  movements,
  rotation: ramp(spinAt),
  elevation: ramp(elevAt),
  distance: ramp(distAt),
  lanes: [
    // pinned: the floor and the ceiling never move
    { name: "kick", color: [179, 64, 46], events: kick, az: 0, el: -0.25, dist: 0.5 },
    { name: "bass", color: [150, 90, 70], events: bass, az: 0, el: -0.1, dist: 0.7 },
    { name: "hats", color: [62, 124, 138], events: hats, az: -0.75, el: 0.85, dist: 0.9 },
    { name: "ride", color: [90, 150, 160], events: ride, az: 0.75, el: 0.8, dist: 0.9 },
    // orbiting: the things that fly past you
    { name: "stab", color: [200, 120, 40], events: stab },
    { name: "lead", color: [230, 190, 60], events: lead, azOffset: Math.PI },
    { name: "siren", color: [120, 60, 150], events: siren, azOffset: Math.PI / 2 },
  ],
};

const dest = args.find(a => !a.startsWith("--") && a.endsWith(".nsscore")) || "hardfield.nsscore";
writeFileSync(dest, JSON.stringify(score) + "\n");
const total = [kick, bass, hats, ride, stab, lead, siren].reduce((a, l) => a + l.length, 0);
console.log(`${dest} — ${BPM} BPM, ${bar} bars, ${Math.floor(dur / 60)}m${Math.round(dur % 60)}s, ${total} events (kick ${kick.length}, hats ${hats.length}, lead ${lead.length}, stab ${stab.length})`);
