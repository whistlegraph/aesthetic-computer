#!/usr/bin/env node
// render-teknull.mjs — fast techno carved from cancelled noise, on a
// strict 32-EQ-point budget. Composition lives here; ALL DSP runs in the
// shared C engine (pop/nullabye/c/nullnoise.c) — this script just bakes
// the score and calls run-c.mjs with the techno master preset.
//
// The trick (after Andy Brewer, https://youtu.be/_Rk-hmIMv6I): two copies
// of pink noise, one phase-inverted — silence — and every sound is a
// peaking EQ bell breaking the cancellation. Techno turns out to be the
// technique's home turf:
//   · the CLAP is literally a burst of the noise itself (wide 2.3k bell)
//   · the ACID LINE is one monophonic EQ point — 303 slides are freqEnd
//     glides, accents are gain, note-off is the mono-cut
//   · the KICK is the classic 115→44 Hz pitch-drop, carved not synthesized
//
// 140 BPM 4/4 → bar = 240/140 s, so 70 bars = 120.0 s exactly: 2:00.
//
//   bars   section     what runs
//   0–7    intro       kick, hats join, clap at 6, veil throughout
//   8–15   build       + offbeat sub, drone, perc, riser
//   16–23  acid        + the 303 line, working the room
//   24–39  drop A      + stabs, rim ghosts, air; lead hook from 32
//   40–47  breakdown   kick out — chord swells, noise-wash, subdrop boom
//   48–63  drop B      everything + ride 16ths + acid echo an octave up
//   64–69  outro       strip to kick + hats, final boom, veil closes
//
// Run:
//   node pop/teknull/bin/render-teknull.mjs            # → out/teknull.mp3
//   node pop/teknull/bin/render-teknull.mjs --bake-only

import { writeFileSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const BPM = 140;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_BARS = 70; // 70 × 240/140 = 120.0 s — exactly 2:00
const totalSec = TOTAL_BARS * BAR;

const t = (bar, beat = 0) => bar * BAR + beat * BEAT;
const st = (semis) => 110 * Math.pow(2, semis / 12); // A2 = 110 Hz base

// sections
const DROP_A = (b) => b >= 24 && b <= 39;
const BREAK = (b) => b >= 40 && b <= 47;
const DROP_B = (b) => b >= 48 && b <= 63;
const DROP = (b) => DROP_A(b) || DROP_B(b);
const OUTRO = (b) => b >= 64;

// ── 32-point budget: every lane declared up front, audited below ──────
const MAX_POINTS = 32;
const lanes = new Map(); // name → { events: [] }

function note(lane, t0, { freq, freqEnd = 0, peakDb, q, atk, hold, rel, group = 0 }) {
  let L = lanes.get(lane);
  if (!L) { L = { events: [], group }; lanes.set(lane, L); }
  const prev = L.events[L.events.length - 1];
  if (prev && prev.end > t0) { prev.cut = t0; prev.end = t0; } // mono-clip
  L.events.push({ t0, freq, freqEnd, peakDb, q, atk, hold, rel, end: t0 + atk + hold + rel, cut: -1 });
}

// ── groove machinery ──────────────────────────────────────────────────
// SWING: odd 16ths land late. EAGER hats push early, claps drag late
// (the hellsine "eager percussion" trick). JITTER: deterministic ±4 ms
// human wobble on everything that isn't the kick — the kick is the law.
const SW = 0.08; // beats of swing on odd 16ths (~34 ms at 140)
const sw = (beat) => (Math.round(beat * 4) % 2 ? beat + SW : beat);
const jit = (seedA, seedB, amt = 0.004) => {
  let s = (seedA * 374761393 + seedB * 668265263 + 1442695041) >>> 0;
  s = (s ^ (s >> 13)) * 1274126177 >>> 0;
  return ((s / 4294967296) * 2 - 1) * amt;
};
const EAGER = -0.008, DRAG = 0.007; // seconds: hats lean in, claps lean back

// ── the score ─────────────────────────────────────────────────────────
// A natural minor. Stabs on Am (A3 C4 E4 A4); acid on A2.
const STAB = [220, 261.63, 329.63];
const STAB_RHYTHM = [[0.75, 1.75, 3.25], [0.5, 2.25, 3.75]]; // alternates by bar

// acid pattern: 16 steps × [semitones from A2 | null=rest, accent, slideToNext]
// the RESTS are the groove — a 303 line has to breathe
const ACID = [
  [0, 1, 0], [null, 0, 0], [0, 0, 0], [12, 0, 1],
  [null, 0, 0], [0, 1, 0], [null, 0, 0], [7, 0, 0],
  [null, 0, 0], [0, 0, 0], [3, 0, 1], [null, 0, 0],
  [0, 1, 0], [null, 0, 0], [10, 0, 1], [12, 0, 0],
];
const ACID_FILL = [ // every 4th bar climbs out the top
  [0, 1, 0], [0, 0, 0], [12, 0, 0], [0, 0, 0],
  [3, 1, 0], [7, 0, 1], [10, 0, 0], [12, 0, 1],
  [15, 1, 0], [12, 0, 0], [10, 0, 0], [7, 0, 0],
  [3, 0, 1], [0, 1, 0], [-2, 0, 1], [0, 0, 0],
];

for (let bar = 0; bar < TOTAL_BARS; bar++) {
  const inBreak = BREAK(bar);
  const drop = DROP(bar);

  // kick — four on the floor, out for the breakdown. two stacked bells
  // per hit: the click-thump and a SUB BODY underneath (52→40) with a
  // long tail — at 40 Hz a cycle is 25 ms, the boom needs time to bloom.
  // SOFT from the very start: the intro opens at +22 dB with a slow
  // attack and a duller sweep, growing to the full machine thump by bar 8.
  if (!inBreak && bar <= 67)
    for (const beat of [0, 1, 2, 3]) {
      const soft = bar < 8 ? bar / 8 : 1;
      note("kick", t(bar, beat), {
        freq: 88 + soft * 42, freqEnd: 42, peakDb: 23 + soft * 10, q: 2.8,
        atk: 0.0015 + (1 - soft) * 0.01, hold: 0.016, rel: 0.24,
      });
      if (bar >= 4)
        note("kickSub", t(bar, beat + 0.018), { freq: 58, freqEnd: 43, peakDb: 27 * Math.min(1, soft + 0.3), q: 5.5, atk: 0.005, hold: 0.055, rel: 0.2 });
    }

  // hats — eager (lean in early), accent cycle, open hat wanders
  if (bar <= 67) // ticking from bar 0 — soft at first, full by bar 8
    [0.5, 1.5, 2.5, 3.5].forEach((beat, i) =>
      note("hatC", t(bar, beat) + EAGER + jit(bar, i), {
        freq: 8200, peakDb: (bar < 8 ? -3 : 0) + [18, 16, 19, 17][i], q: 4.2,
        atk: 0.002, hold: 0.006, rel: 0.04,
      }));
  if (bar >= 4 && bar <= 67 && !inBreak)
    note("hatO", t(bar, bar % 4 === 2 ? 1.5 : 3.5) + EAGER, { freq: 6800, peakDb: 17, q: 3.2, atk: 0.002, hold: 0.012, rel: 0.16 });

  // clap — 2 and 4, dragged late against the eager hats; ghost "clap-a"
  // closing every 4th bar. the most honest instrument here, it IS the noise
  if (bar >= 6 && bar <= 67 && !inBreak) {
    for (const beat of [1, 3])
      note("clap", t(bar, beat) + DRAG, { freq: 2050, peakDb: 19, q: 2.2, atk: 0.0015, hold: 0.022, rel: 0.11 });
    if (bar % 4 === 3)
      note("clap", t(bar, 3.375) + DRAG, { freq: 2050, peakDb: 13, q: 2.2, atk: 0.0015, hold: 0.015, rel: 0.08 });
  }

  // sub — offbeat "oontz" with a small downward settle per note; odd bars
  // anticipate beat 3 by a 16th — that little push IS the groove
  if (bar >= 8 && bar <= 67 && !inBreak)
    for (const beat of bar % 2 ? [0.5, 1.5, 2.75, 3.5] : [0.5, 1.5, 2.5, 3.5])
      note("sub", t(bar, beat), { freq: 56, freqEnd: 49, peakDb: 26, q: 6, atk: 0.01, hold: 0.16, rel: 0.16 });

  // acid — ONE monophonic EQ point doing 303 work, now swung + jittered
  if ((bar >= 16 && bar <= 39) || DROP_B(bar)) {
    const pat = bar % 4 === 3 ? ACID_FILL : ACID;
    for (let k = 0; k < 16; k++) {
      const [semi, accent, slide] = pat[k];
      if (semi === null) continue;
      const nxt = pat[(k + 1) % 16];
      note("acid", t(bar, sw(k * 0.25)) + jit(bar * 16 + k, 7, 0.003), {
        freq: st(semi),
        freqEnd: slide && nxt[0] !== null ? st(nxt[0]) : 0,
        peakDb: accent ? 23 : 17, q: 22, atk: 0.008, hold: 0.05, rel: 0.06,
      });
    }
  }

  // acid2 — drop B echo, an octave up, 3 sixteenths behind, quieter
  if (DROP_B(bar)) {
    const pat = bar % 4 === 3 ? ACID_FILL : ACID;
    for (let k = 0; k < 16; k++) {
      const [semi, , ] = pat[k];
      if (semi === null) continue;
      note("acid2", t(bar, sw(k * 0.25 + 0.75)) + jit(bar * 16 + k, 13, 0.003), { freq: st(semi + 12), peakDb: 13, q: 30, atk: 0.008, hold: 0.04, rel: 0.05 });
    }
  }

  // stabs — Am hits, rhythm alternates by bar, riding the swing
  if (drop) {
    for (const beat of STAB_RHYTHM[bar % 2]) {
      STAB.forEach((f, i) =>
        note(`stab${i}`, t(bar, sw(beat)), { freq: f, peakDb: 17, q: 40, atk: 0.006, hold: 0.07, rel: 0.12 }));
      note("stabOct", t(bar, sw(beat)), { freq: 440, peakDb: 14, q: 40, atk: 0.006, hold: 0.07, rel: 0.12 });
    }
  }

  // percussion spice — swung
  if (bar >= 8 && bar <= 63)
    note("perc1", t(bar, sw(bar % 2 ? 3.75 : 1.25)) + jit(bar, 23), { freq: 380, freqEnd: 200, peakDb: 16, q: 5, atk: 0.003, hold: 0.02, rel: 0.09 });
  if (drop)
    for (const beat of [2.75, 3.25])
      note("perc2", t(bar, sw(beat)) + jit(bar * 4 + beat, 31), { freq: 4600, peakDb: 14, q: 9, atk: 0.002, hold: 0.008, rel: 0.05 });

  // ride 16ths — drop B sizzle, swung, with a hole at the bar's last 16th
  if (DROP_B(bar))
    for (let k = 0; k < 16; k++) {
      if (k % 8 === 7) continue;
      note("ride16", t(bar, sw(k * 0.25)) + jit(bar * 16 + k, 41, 0.002), { freq: 10500, peakDb: k % 4 === 2 ? 10 : 7, q: 7, atk: 0.002, hold: 0.004, rel: 0.032 });
    }

  // LEADS — the hook rides the WHOLE drop now: a swung 2-bar pentatonic
  // riff, lifting a fifth in each drop's back half, octave echo trailing
  if (drop) {
    const riff = bar % 2 === 0
      ? [[440, 0.75, 0.5], [523.25, 1.5, 0.25], [659.26, 2.25, 0.75], [587.33, 3.25, 0.5]]
      : [[659.26, 0.5, 0.5], [587.33, 1.25, 0.25], [523.25, 1.75, 0.5], [440, 2.5, 1.25]];
    const lift = (DROP_A(bar) && bar >= 32) || (DROP_B(bar) && bar >= 56) ? 1.5 : 1;
    for (const [f, beat, beats] of riff) {
      const t0 = t(bar, sw(beat)) + jit(bar * 7 + beat * 4, 99, 0.003);
      note("lead", t0, { freq: f * lift, peakDb: 22, q: 35, atk: 0.015, hold: beats * BEAT - 0.02, rel: 0.18 });
      note("lead2", t0 + 0.75 * BEAT, { freq: f * lift * 2, peakDb: 13, q: 45, atk: 0.02, hold: beats * BEAT * 0.5, rel: 0.12 });
    }
  }

  // breakdown — chord swells + rising noise-wash while the kick rests
  if (inBreak) {
    note("noisewash", t(bar), { freq: 1000 + (bar - 40) * 250, peakDb: 7 + (bar - 40) * 0.55, q: 1.1, atk: 0.35, hold: BAR - 0.35, rel: 0.9 });
  }
}

// one-shots: crashes, risers, drop zaps, sub booms
for (const bar of [8, 16, 24, 32, 40, 48, 56, 64])
  note("crash", t(bar), { freq: 4800, peakDb: 13, q: 1.5, atk: 0.003, hold: 0.035, rel: 0.75 });
for (const bar of [14, 22, 46, 62])
  note("riser", t(bar), { freq: 600, freqEnd: 4800, peakDb: 7, q: 2, atk: 2 * BAR - 0.6, hold: 0.3, rel: 0.3 });
for (const bar of [24, 48, 64])
  note("fall", t(bar), { freq: 3500, freqEnd: 90, peakDb: 18, q: 2, atk: 0.004, hold: 0.05, rel: 0.45 });
for (const bar of [40, 64, 68])
  note("subdrop", t(bar), { freq: 55, freqEnd: 30, peakDb: 24, q: 3, atk: 0.01, hold: 0.4, rel: 2.0 });

// breakdown chord swells — ONE long event per lane, not per-bar
STAB.forEach((f, i) =>
  note(`swell${i}`, t(40), { freq: f, peakDb: 20, q: 28, atk: 2.0, hold: 8 * BAR - 3.5, rel: 1.5, group: 1 }));

// ── HARMONY CHANNEL (group 1) — a SECOND cancellation pair ────────────
// independent pink noise + its own serial chain, residues summed in
// parallel with the main channel. that means these chords can sit on the
// SAME frequencies the stabs/leads already boost — no serial dB blowup.
// voice-led Am / F / C / G, two bars per chord: real harmonic motion.
{
  const VOICED = {
    Am: [220, 261.63, 329.63],   // A3 C4 E4
    F:  [220, 261.63, 349.23],   // A3 C4 F4
    C:  [196, 261.63, 329.63],   // G3 C4 E4
    G:  [196, 246.94, 293.66],   // G3 B3 D4
  };
  const PROG_BUILD = ["Am", "F"];           // bars 16–23: gentle sway
  const PROG_DROP = ["Am", "F", "C", "G"];  // drops: the full turn
  for (let bar = 16; bar < 64; bar += 2) {
    if (BREAK(bar)) continue; // the swells own the breakdown
    const drop = DROP(bar);
    if (!drop && bar >= 24) continue;
    const prog = drop ? PROG_DROP : PROG_BUILD;
    const chord = VOICED[prog[Math.floor((bar - (drop ? 24 : 16)) / 2) % prog.length]];
    chord.forEach((f, i) =>
      note(`harmPad${i}`, t(bar), { freq: f, peakDb: drop ? 16 : 13, q: 26, atk: 0.5, hold: 2 * BAR - 1.3, rel: 0.8, group: 1 }));
    note("harmHi", t(bar), { freq: chord[2] * 2, peakDb: drop ? 12 : 9, q: 30, atk: 0.6, hold: 2 * BAR - 1.4, rel: 0.8, group: 1 });
  }
}

// the NEVER-SILENT floor — veil (mids) + drone (low) + air (high), as
// 8-BAR events: per-bar bed events left a mono-cut hole at every bar
// line (the loudness curve notched −35 dB at each seam, 70 times). With
// 8-bar spans the only seams sit on crash-marked section starts.
for (const [a, z] of [[0, 8], [8, 16], [16, 24], [24, 32], [32, 40], [40, 48], [48, 56], [56, 64], [64, 70]]) {
  const dropSect = DROP(a);
  const span = (z - a) * BAR;
  note("veil", t(a), { freq: 900, peakDb: dropSect ? 7 : 6.5, q: 0.5, atk: 0.3, hold: span - 0.3, rel: 0.6 });
  note("drone", t(a), { freq: 110, peakDb: a >= 8 ? 13 : 11, q: 12, atk: 0.4, hold: span - 0.4, rel: 0.6 });
  note("air", t(a), { freq: 13000, peakDb: dropSect ? 6 : 4.5, q: 1.0, atk: 0.5, hold: span - 0.5, rel: 0.5 });
}

// ── audit the budget ──────────────────────────────────────────────────
if (lanes.size > MAX_POINTS)
  throw new Error(`${lanes.size} EQ points exceeds the ${MAX_POINTS}-point budget`);
console.log(`→ teknull · ${TOTAL_BARS} bars · A minor @ ${BPM} BPM · ${lanes.size}/${MAX_POINTS} EQ points`);

// ── bake the score for the C engine ───────────────────────────────────
// Give each family its own independently generated and filtered source.
// This keeps the 32 serial EQ voices from smearing into one shared wash:
// drums are dry velvet impulses, the low end uses rounder brown noise,
// and the musical/ambient layers retain separate pink-noise identities.
const family = (name) => {
  if (/^kick|^sub$|^subdrop$/.test(name)) return 2;
  if (/^(hat|clap|perc|ride|crash|riser|fall)/.test(name)) return 3;
  if (/^acid/.test(name)) return 4;
  if (/^(veil|drone|air|noisewash)$/.test(name)) return 5;
  return lanes.get(name).group ?? 1;
};
for (const [name, lane] of lanes) lane.group = family(name);
const bands = [...lanes.values()];
for (const b of bands) b.events.sort((x, y) => x.t0 - y.t0);

// loudness ride: techno wants a firm floor — flat-ish targets, dip for
// the breakdown, lift for the drops
const rideTarget = (bar) => {
  if (BREAK(bar)) return -17.5;
  if (DROP(bar)) return -16.6;
  if (OUTRO(bar)) return -19;
  if (bar >= 16) return -18;
  if (bar >= 8) return -18.5;
  return -19.5;
};
const RIDE_BARS = 68; // leave the final boom + veil tail alone

const L = [];
L.push(`sr ${SR}`, `dur ${totalSec}`, `detune 1.0009 0.9991`, `seed ${0xAC1D} ${0x303}`);
L.push(`normpeak 0.9`, `fadein 0.004`, `fadeout 1.2`);
L.push(`ridewin ${BAR} ${RIDE_BARS}`);
for (let b = 0; b < RIDE_BARS; b++) L.push(String(rideTarget(b)));
const noiseType = ["pink", "pink", "brown", "velvet", "pink", "pink"];
for (const g of [...new Set(bands.map((b) => b.group ?? 0))].sort()) {
  const groupBands = bands.filter((b) => (b.group ?? 0) === g);
  if (!groupBands.length) continue;
  if (g > 0) L.push(`group ${0x7E77 + g} ${0x9A11 + g}`); // fresh noise pair per family
  L.push(`noisetype ${noiseType[g] ?? "pink"}`);
  for (const band of groupBands) {
    L.push(`band ${band.events.length}`);
    for (const e of band.events)
      L.push([e.t0, e.freq, e.freqEnd, e.peakDb, e.q, e.atk, e.hold, e.rel, e.cut].join(" "));
  }
}
const OUT_DIR = resolve(HERE, "..", "out");
mkdirSync(OUT_DIR, { recursive: true });
const scorePath = resolve(OUT_DIR, "teknull.score.txt");
writeFileSync(scorePath, L.join("\n") + "\n");
console.log(`✓ baked ${bands.length} bands → ${scorePath}`);
if (process.argv.includes("--bake-only")) process.exit(0);

// ── render through the C engine + techno master ───────────────────────
const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const outPath = _argi("--out") || resolve(OUT_DIR, "teknull.mp3");
const r = spawnSync("node", [resolve(HERE, "../../nullabye/c/run-c.mjs"),
  scorePath, "--out", outPath, "--master", "teknull"], { stdio: "inherit" });
process.exit(r.status ?? 1);
