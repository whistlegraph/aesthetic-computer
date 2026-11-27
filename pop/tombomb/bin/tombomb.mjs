#!/usr/bin/env node
// tombomb.mjs — a dance track in which every sound is a synthesized tom, struck or rubbed, and the tempo is
// a dirt bike on a motocross track.
//
// Drum and bass pushed toward hard techno, written the way a fugue is: one subject, imitated, mirrored,
// played faster and slower, over a ground of four chords in F minor and a sequence round the fifths, with a
// slow top line over it (a chorale with suspensions, a pop hook, and at the end both). There are no samples.
//   · A tom is a drumhead's modes that start sharp and settle, a stick, and for the rimshot a shell. Seven
//     kinds, from an 18" floor tom to a 6" one, tuned to pitches, are the kick, bass, snare, hats, voices.
//   · A rubbed head is a finger on that same drum: a hand that moves like one, stick-slip friction and the
//     bumps it passes, and the head's own modes answering. Scratches, scrapes, brakes. (The top line is
//     Menu Band's friction model held steady so it sustains.)
//   · The beat is the engine's stroke. A rider with a throttle, a gearbox and a temper goes round a track
//     of climbs, mud, rocks and jumps; the tempo is whatever speed and gear make it.
//
//   node pop/tombomb/bin/tombomb.mjs            → out/tombomb.{wav,mp3,json}
//   TOMBOMB_RIDE=1 node pop/tombomb/bin/tombomb.mjs   prints the ride bar by bar and stops

import { writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync, execFileSync } from "node:child_process";
import { bjorklund } from "../../lib/necklace.mjs";
import { isConsonant } from "../../lib/counterpoint.mjs";

const OUT = resolve(dirname(fileURLToPath(import.meta.url)), "../out");
const SR = 48_000, BAR = 16, BARS = 120, STEPS = BARS * BAR, TAIL = 9;
const mod = (a, n) => ((a % n) + n) % n;
const rng = (seed) => () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 2 ** 32);
const NAMES = ["C", "C#", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B"];
const name = (m) => NAMES[mod(Math.round(m), 12)] + (Math.floor(Math.round(m) / 12) - 1);

// ── the ride ──────────────────────────────────────────────────────────
// The track, composed bar by bar (bars from 0): [from, to, grade %, ground, throttle].
const COURSE = [
  [0, 16, 0, "dirt", "steady"],                                          // i: the start straight
  [16, 26, 3.5, "dirt", "steady"], [26, 32, -3, "dirt", "steady"],       // ii: a climb, a crest, down the back
  [32, 36, 0, "dirt", "boost"], [36, 40, 0, "rocks", "boost"], [40, 48, -1, "dirt", "boost"],   // iii: boost held, a rock garden, two jumps
  [48, 60, 0, "mud", "boost"], [60, 64, 0, "dirt", "boost"],             // iv: lands in the mud, still on the boost
  [64, 74, 3, "dirt", "steady"], [74, 80, -10, "dirt", "steady"],        // v: the long climb with a hot engine, then the drop off the top
  [80, 96, 0, "dirt", "boost"], [96, 100, 0, "rocks", "boost"], [100, 104, 0, "dirt", "boost"], // vi: flat out
  [104, 116, -2, "dirt", "steady"], [116, 120, 0, "dirt", "off"],        // vii: downhill to the line, throttle shut
];
const JUMPS = [[40, 4], [48, 8], [88, 4], [96, 6], [104, 8], [112, 8]];   // [the bar it lands on, sixteenths in the air]
// 180 kg of bike and rider. BPM is engine speed: 14.5 per m/s in fourth, six gears 9 % apart. The rider
// shifts down when the engine labours under 160 and up over 183, on a beat, never twice in half a bar. The boost is a third
// more power; held, it heats the engine, which then runs at 0.7 until it has cooled.
const BIKE = { mass: 180, cda: .6, crr: { dirt: .06, rocks: .095, mud: .18 }, power: 1900, boost: 1.35, cooling: .7,
  gears: [-4, -3, -2, -1, 0, 1].map((k) => 1.09 ** k), perMps: 14.5, low: 160, high: 183, limiter: 194, heatBars: 31, coolBars: 18 };
function ride() {
  const r = { bpm: [], v: [], gear: [], air: [], coast: [], rocks: [], load: [], heat: [], hot: [], T: [0], shifts: [], jumps: [] };
  let v = 11.2, g = 4, heat = 0, hot = false, fresh = 1, lastShift = -99, airBpm = 0;
  for (let s = 0; s < STEPS + 160; s++) {
    const bar = s / BAR, over = s >= STEPS, [, , grade, ground, throttle] = over ? [0, 0, 0, "dirt", "brake"] : COURSE.find(([a, z]) => bar >= a && bar < z);
    const jump = JUMPS.find(([land, n]) => s >= land * BAR - n && s < land * BAR), slope = Math.atan(grade / 100), weight = BIKE.mass * 9.81;
    let bpm = BIKE.perMps * v / BIKE.gears[g], coast = false, power = 0;
    if (jump) {
      // the wheel is off the ground: nothing to push against, so the engine races and the speed only bleeds to the air
      if (s === jump[0] * BAR - jump[1]) airBpm = bpm * 1.08;
      bpm = airBpm;
    } else if (!over) {
      if (throttle !== "off" && s % 4 === 0 && s - lastShift >= BAR / 2) {
        const to = bpm < BIKE.low && g > 0 ? g - 1 : bpm > BIKE.high && g < 5 ? g + 1 : g;
        if (to !== g) { r.shifts.push({ step: s, bar: +(bar + 1).toFixed(2), from: g + 1, to: to + 1, bpm: [Math.round(bpm), Math.round(BIKE.perMps * v / BIKE.gears[to])] }); g = to; lastShift = s; bpm = BIKE.perMps * v / BIKE.gears[g]; }
      }
      const boosting = throttle === "boost" && !hot;
      heat = Math.max(0, heat + (boosting ? 1 / BIKE.heatBars : -1 / BIKE.coolBars) / BAR);
      if (heat >= 1) hot = true; else if (heat <= .25) hot = false;
      // freewheeling: spun out in top gear down a steep hill, the rider shuts off and lets it roll
      coast = grade <= -8 && g === 5 && bpm > 181;
      power = throttle === "off" || coast || bpm > BIKE.limiter ? 0 : BIKE.power * (hot ? BIKE.cooling : boosting ? BIKE.boost : 1) * (.93 + .07 * fresh);
      fresh = Math.max(0, Math.min(1, fresh + (power > BIKE.power ? -.0006 : .0004)));
    }
    const dt = 60 / bpm / 4, roll = jump ? 0 : BIKE.crr[ground] * weight * Math.cos(slope) + (over ? 260 : 0), hill = jump ? 0 : weight * Math.sin(slope);
    const whoop = ground === "rocks" && !jump ? 60 * Math.sin(s * 1.9) : 0;       // the bike pitching over the stones
    v = Math.max(over ? 0 : 3, v + (power / Math.max(v, 3) - roll - hill - whoop - .5 * 1.2 * BIKE.cda * v * v) / BIKE.mass * dt);
    if (jump && s === jump[0] * BAR - 1) {
      // the landing: longer flights cost more, and a flat landing more than a downslope
      const t = jump[1] * dt, [, , land] = COURSE.find(([a, z]) => jump[0] >= a && jump[0] < z) ?? [0, 0, 0], loss = .03 + .006 * jump[1] + (land >= 0 ? .03 : 0);
      r.jumps.push({ lands: jump[0] + 1, takeoff: jump[0] * BAR - jump[1], steps: jump[1], airtime: +t.toFixed(2), height: +(9.81 * t * t / 8).toFixed(2),
        ramp: +(Math.asin(Math.min(1, 9.81 * t / (2 * v))) * 180 / Math.PI).toFixed(1), kmh: +(v * 3.6).toFixed(1), bpm: [Math.round(airBpm / 1.08), Math.round(airBpm), Math.round(BIKE.perMps * v * (1 - loss) / BIKE.gears[g])], lost: `${Math.round(loss * 100)} %` });
      v *= 1 - loss;
    }
    r.bpm.push(over ? r.bpm[STEPS - 1] : bpm); r.v.push(v); r.gear.push(g); r.air.push(!!jump); r.coast.push(coast); r.rocks.push(ground === "rocks" && !jump);
    r.load.push(jump ? 0 : Math.max(0, Math.min(1, (hill + roll - BIKE.crr.dirt * weight) / 260))); r.heat.push(heat); r.hot.push(hot);
    r.T.push(r.T.at(-1) + 60 / r.bpm.at(-1) / 4);
  }
  return r;
}
const RIDE = ride();
// over the rock garden the wheel is knocked about: every stroke off the beat lands up to 5 ms early or late
const stone = rng(0x570e), KNOCKS = RIDE.rocks.map((on, j) => (on && j % 4 ? (stone() - .5) * .011 : 0));
// Seconds at a step: the engine's tempo integrated, then leaned by the crank. A stroke is not even: the
// middle of each beat is the power phase and the beat itself the dead spot, so the second sixteenth comes a
// little early and the fourth a little late, 2 ms on the flat and up to 7 under load.
function timeAt(step) {
  const s = Math.max(0, Math.min(step, RIDE.bpm.length - 1.001)), j = Math.floor(s), t = RIDE.T[j] + (RIDE.T[j + 1] - RIDE.T[j]) * (s - j);
  return t + KNOCKS[j] * (1 - (s - j)) + (KNOCKS[j + 1] ?? 0) * (s - j) - (RIDE.air[j] ? 0 : .03 + .09 * RIDE.load[j]) * 60 / RIDE.bpm[j] * Math.sin(2 * Math.PI * (s % 4) / 4) / (2 * Math.PI);
}
const perBar = Array.from({ length: BARS }, (_, b) => {
  const s = b * BAR, [, , grade, ground, throttle] = COURSE.find(([a, z]) => b >= a && b < z), mid = RIDE.bpm.slice(s, s + BAR);
  return { bar: b + 1, at: +RIDE.T[s].toFixed(1), grade, ground, throttle, kmh: +(RIDE.v[s] * 3.6).toFixed(1), gear: RIDE.gear[s] + 1, bpm: Math.round(mid.reduce((a, x) => a + x, 0) / BAR),
    heat: +RIDE.heat[s].toFixed(2), hot: RIDE.hot[s], air: RIDE.air.slice(s, s + BAR).filter(Boolean).length, coast: RIDE.coast.slice(s, s + BAR).filter(Boolean).length };
});
if (process.env.TOMBOMB_RIDE) { console.table(perBar); console.table(RIDE.shifts); console.table(RIDE.jumps); process.exit(0); }

// ── the toms ──────────────────────────────────────────────────────────
// ring: seconds a 110 Hz drum of this kind takes to fall to 1/e (smaller drums ring shorter).
// bend: how far above its note a hard stroke starts, as a fraction; bendT: how fast it settles.
// stick: the click of the stick; bright: where that click is rolled off.
const KIND = {
  kick: { ring: .17, bend: 2.6, bendT: .022, stick: .4, bright: 3500, modes: 2, say: '18" floor tom, slack head, struck dead centre: it falls two octaves in 20 ms' },
  floor: { ring: .5, bend: .18, bendT: .05, stick: .2, bright: 2600, modes: 5, say: '16" floor tom: the bass, the landings' },
  rack: { ring: .3, bend: .12, bendT: .04, stick: .24, bright: 4500, modes: 5, say: '12" rack tom: the tenor, the fills' },
  high: { ring: .2, bend: .08, bendT: .03, stick: .34, bright: 7500, modes: 4, say: '8" concert tom and octobans: alto and lead, the flips' },
  tick: { ring: .03, bend: .3, bendT: .008, stick: .75, bright: 11000, modes: 3, say: '6" tom damped with a hand: the hats, the freewheel' },
  ring: { ring: 1.3, bend: .04, bendT: .06, stick: .1, bright: 3000, modes: 5, say: "undamped concert toms, soft mallets: the chorale, the start of each sung note" },
  power: { ring: .42, bend: .06, bendT: .03, stick: .22, bright: 4000, modes: 1, say: '13" tom tuned to one clear note (its overtones damped): the power chords' },
  rim: { ring: .1, bend: .22, bendT: .02, stick: 1.4, bright: 10000, modes: 4, shell: true, say: 'rimshot on a 10" tom, the shell ringing: the snare, the gear changes' },
};
const MEMBRANE = [[1, 1], [1.59, .5], [2.14, .3], [2.3, .2], [2.65, .12]];
const SHELL = [[3.17, .5], [4.73, .4], [6.41, .3], [8.2, .2]];
const toms = new Map(), noise = rng(7);
function tom(kind, midi, vel, held) {
  const key = `${kind}:${midi.toFixed(2)}:${Math.round(vel * 8)}:${Math.round(held * 50)}`;
  if (toms.has(key)) return toms.get(key);
  const k = KIND[kind], hz = 440 * 2 ** ((midi - 69) / 12), ring = k.ring * (110 / hz) ** .3;
  const n = Math.round(Math.min(held + .03, 7 * ring) * SR), y = new Float32Array(n), hard = .5 + .5 * Math.min(1, vel);
  MEMBRANE.slice(0, k.modes).forEach(([ratio, amp], j) => {
    const tau = ring / (1 + 1.3 * j) * SR;
    for (let i = 0, ph = 0; i < n; i++) {
      ph += 2 * Math.PI * hz * ratio * (1 + k.bend * hard * Math.exp(-i / (k.bendT * SR))) / SR;
      y[i] += amp * Math.exp(-i / tau) * Math.sin(ph);
    }
  });
  // the rimshot: the stick lands on hoop and head at once, so the shell rings and the head buzzes for 30 ms,
  // a buzz rolled off above 6 kHz and kept under the shell
  if (k.shell) {
    SHELL.forEach(([ratio, amp]) => { for (let i = 0; i < n; i++) y[i] += amp * Math.exp(-i / (.035 * SR)) * Math.sin(2 * Math.PI * hz * ratio * i / SR); });
    for (let i = 0, lp = 0, top = 0; i < n; i++) { const x = noise() * 2 - 1; lp += .35 * (x - lp); top += .55 * (x - lp - top); y[i] += .65 * top * Math.exp(-i / (.026 * SR)); }
  }
  const a = 1 - Math.exp(-2 * Math.PI * k.bright / SR);
  for (let i = 0, lp = 0; i < Math.min(n, .02 * SR); i++) { lp += a * (noise() * 2 - 1 - lp); y[i] += k.stick * hard * lp * Math.exp(-i / (.0035 * SR)); }
  const fade = Math.min(n, Math.round(.02 * SR));                 // the hand that stops it
  for (let i = 0; i < fade; i++) y[n - 1 - i] *= i / fade;
  for (let i = 0; i < n; i++) y[i] = Math.tanh(1.4 * y[i] * Math.min(1, i / 24));
  toms.set(key, y);
  return y;
}

// ── the harmony ───────────────────────────────────────────────────────
// Pitches are semitones above F. A chord is its root and its three notes inside one octave from F, so the
// same rung of two chords' ladders is never far apart and the voices move by step.
const CH = {
  Fm: { say: "Fm", root: 0, pcs: [0, 3, 7] }, Db: { say: "Db", root: 8, pcs: [0, 3, 8] }, Bbm: { say: "Bbm", root: 5, pcs: [0, 5, 8] },
  C: { say: "C", root: 7, pcs: [2, 7, 11], leading: true }, Eb: { say: "Eb", root: 10, pcs: [2, 5, 10] }, Ab: { say: "Ab", root: 3, pcs: [3, 7, 10] },
  F: { say: "F", root: 0, pcs: [0, 4, 7] },
};
const GROUND = [CH.Fm, CH.Db, CH.Bbm, CH.C];                                        // i VI iv V
const FIFTHS = [CH.Fm, CH.Bbm, CH.Eb, CH.Ab, CH.Db, CH.Bbm, CH.C, CH.C];            // round the fifths and home
const ladder = (c, k, base) => base + c.pcs[mod(k, 3)] + 12 * Math.floor(k / 3);
// F minor, its seventh raised over the dominant
const scaleNote = (c, d, base) => base + 12 * Math.floor(d / 7) + (c.leading ? [0, 2, 3, 5, 7, 8, 11] : [0, 2, 3, 5, 7, 8, 10])[mod(d, 7)];
const F = 53, BASS = 29, TENOR = 53, ALTO = 65, LEAD = 77;                          // F3; F1, F3, F4, F5
// the bass sits between F1 and Eb2; the kick takes the root when it is low enough to thump (F1–C2) and
// otherwise the chord's F or Bb
const bassNote = (c) => BASS + c.root;
const kickNote = (c) => BASS + (c.root <= 7 ? c.root : c.root === 8 ? 0 : 5);

// ── the score ─────────────────────────────────────────────────────────
const events = [], sections = [], barChord = [], spins = [];
let POS = 0;
function section(title, prog, bars, what) {
  sections.push({ title, start: POS, bars, chords: prog.map((c) => c.say).join(" "), what });
  for (let b = 0; b < bars; b++) barChord.push(prog[b % prog.length]);
  const s = POS;
  POS += bars * BAR;
  return (bar, step = 0) => s + (bar - 1) * BAR + step;
}
const chordAt = (step) => barChord[Math.max(0, Math.min(barChord.length - 1, Math.floor(step / BAR)))] ?? CH.F;
// One stroke. In the air there is no pulse (kick, bass and snare fall silent unless `always`); freewheeling,
// the kick goes half-time. more: ang, an angle round the head in radians (0 ahead, π behind) instead of a
// pan; nave, how much of the stroke is sent to the cathedral; dist, metres away (quieter, darker, later and
// mostly room); rev, played backwards so that it ends on its step; gap, it may sound inside a gap.
// The gaps: stretches where one hit is left to ring and everything else is silent (absolute steps). Bar 24's
// last beat, half of bar 56, the last beats of bars 100 and 116.
const GAPS = [[23 * BAR + 12, 24 * BAR], [55 * BAR + 8, 56 * BAR], [99 * BAR + 12, 100 * BAR], [115 * BAR + 12, 116 * BAR]];
const inGap = (step) => GAPS.some(([a, z]) => step >= a && step < z);
const PULSE = new Set(["kick", "bass", "snare"]);
function put(bus, kind, step, midi, len = 1, vel = 1, pan = 0, more = {}) {
  const s = Math.floor(step);
  if (!more.gap && inGap(step)) return;
  if (!more.always && PULSE.has(bus) && RIDE.air[s]) return;
  if (!more.always && bus === "kick" && RIDE.coast[s] && step % 8) return;
  events.push({ bus, kind, step, midi, len, vel, pan, ...more });
}

// The subject, as rungs of whatever chord is sounding: [step, rung, length]. One bar.
const SUBJECT = [[0, 0, 2], [2, 2, 2], [4, 1, 1], [5, 2, 1], [6, 3, 2], [8, 2, 2], [10, 1, 2], [12, 0, 1], [13, 1, 1], [14, 2, 2]];
// Its countersubject: long notes above it, moving where the subject holds.
const COUNTER = [[0, 3, 4], [4, 2, 2], [6, 4, 2], [8, 3, 4], [12, 2, 2], [14, 1, 2]];
// flip mirrors a figure about its middle rungs; scale stretches it (2 = augmentation, .5 = diminution)
function fig(bus, kind, t0, figure, { base, shift = 0, scale = 1, flip = false, vel = 1, pan = 0, ...more } = {}) {
  for (const [t, k, len] of figure) {
    const at = t0 + t * scale;
    put(bus, kind, at, ladder(chordAt(at), (flip ? 3 - k : k) + shift, base), len * scale, vel * (t % 4 === 0 ? 1 : .8), pan, more);
  }
}
// a scale run, louder toward where it is going
function run(bus, kind, t0, count, from, dir, { base, gap = 1, vel = 1, pan = 0 } = {}) {
  for (let i = 0; i < count; i++) put(bus, kind, t0 + i * gap, scaleNote(chordAt(t0 + i * gap), from + dir * i, base), gap, vel * (.55 + .45 * i / (count - 1)), pan * (1 - 2 * i / (count - 1)));
}
// Drum bars as boxes: X full, x light, o ghost. The kick is tuned to the chord.
const BOX = {
  two: { k: "X.........X.....", s: "....X.......X..." },
  chopA: { k: "X..x......X..x..", s: "....X..o.o..X..o" },
  chopB: { k: "X.....x...X.x...", s: "....X.o...o.X.o." },
  four: { k: "X...X...X...X...", s: "....X.......X..." },
  hard: { k: "X..xX..xX..xX.xx", s: "....X..o....X.oo" },
  floor: { k: "X...X...X...X...", s: "................" },
  half: { k: "X.....x.......x.", s: "........X......." },
  halfB: { k: "X.....x...x...xx", s: "........X.....oo" },
};
// flam: a grace stroke 25 ms before each backbeat. fat: a rack tom struck with the rimshot, for weight.
function drums(t0, box, vel = 1, { flam = false, fat = false } = {}) {
  const c = chordAt(t0), { k, s } = BOX[box];
  [...k].forEach((ch, i) => { if (ch !== ".") put("kick", "kick", t0 + i, kickNote(c), 2, vel * (ch === "X" ? 1 : .7)); });
  [...s].forEach((ch, i) => {
    if (ch === "X") {
      put("snare", "rim", t0 + i, 58 + c.pcs[1] % 5, 2, vel, .05);
      if (flam) put("snare", "rim", t0 + i - .3, 58 + c.pcs[1] % 5, .3, vel * .3, -.15);
      if (fat) put("snare", "rack", t0 + i, 55 + c.pcs[0], 3, vel * .9, .05);
    }
    if (ch === "o") put("snare", "rim", t0 + i, 63 + i % 4, 1, vel * .32, -.1);
  });
}
// hats: tiny damped toms on a Euclidean necklace, five pitches in turn; open ones ring on the off-beats.
// They are the wheel: they keep turning when the pulse stops.
function hats(t0, k, { rot = 0, vel = 1, open = false, turn = null } = {}) {
  bjorklund(k, 16, rot).onsets.forEach((p, j) => put("hats", "tick", t0 + p, 96 + [0, 3, 7, 10, 5][(p + j) % 5], 1, vel * (p % 4 === 0 ? 1 : p % 2 === 0 ? .75 : .55), j % 2 ? .6 : -.6,
    turn == null ? {} : { ang: turn + (j % 2 ? 1 : -1) }));
  if (open) for (const p of [2, 6, 10, 14]) put("hats", "high", t0 + p, 91 + chordAt(t0).pcs[2] % 5, 2, vel * .8, p % 8 === 2 ? -.4 : .4);
}
// the bass: r root, o its octave, f its fifth; a note lasts until the next
function bass(t0, pattern, vel = 1) {
  const root = bassNote(chordAt(t0)), on = [...pattern].map((ch, i) => [ch, i]).filter(([ch]) => ch !== ".");
  on.forEach(([ch, i], k) => put("bass", "floor", t0 + i, root + { r: 0, o: 12, f: 7 }[ch], (on[k + 1]?.[1] ?? BAR) - i, vel * (i % 4 === 0 ? 1 : .82)));
}
// the classic: a roll down the toms, here down the chord, small drums to large and left to right; its last
// strokes are left hanging in the cathedral
function fill(t0, count, gap = 1, vel = 1) {
  for (let i = 0; i < count; i++) {
    const rung = 9 - Math.floor(i * 10 / count), midi = ladder(chordAt(t0), rung, TENOR - 12);
    put("tenor", rung > 6 ? "high" : rung > 2 ? "rack" : "floor", t0 + i * gap, midi, gap, vel * (.7 + .3 * i / count) * (i % 2 ? .85 : 1), -.8 + 1.6 * i / (count - 1), { nave: i > count - 4 ? .7 : 0 });
  }
}
// a roll: one drum struck `per` times a step, swelling or dying
function roll(bus, kind, t0, steps, midi, [v0, v1], { per = 2, pan = 0, ...more } = {}) {
  for (let i = 0, n = steps * per; i < n; i++) put(bus, kind, t0 + i / per, midi, 4, (v0 + (v1 - v0) * i / n) * (i % 2 ? .8 : 1), pan + (i % 2 ? .08 : -.08), more);
}

// ── rubbing the head ──────────────────────────────────────────────────
// A gesture is a finger put down on a head, moved in one or more strokes, and lifted. A stroke goes from d0
// to d1 (0 the centre of the head, 1 its metal edge) in `len` steps and reaches `speed` at its fastest:
// bow starts and stops like a hand does, out ends at full speed, stop slows to nothing, flat holds.
// tune: the semitones above the chord's root the rubbed drum is tuned to (null: whatever drum fits).
// orbit: turns round the listener's head while it lasts. nave: how much of it goes to the cathedral.
const scrapes = [];
const stroke = (step, len, d0, d1, speed, { shape = "bow", pan = 0, tune = 0, orbit = 0, nave = 0 } = {}) => scrapes.push({ step, strokes: [{ len, d0, d1, speed, shape }], pan, tune, orbit, nave });
// a scratch: the finger stays down and goes back and forth between two places on the head, one stroke per
// length, stopping dead at each turn; scratches take the root and the fifth by turns
let nScratch = 0;
const scratch = (t0, lens, { a = .34, b = .58, speed = 2.1, pan = 0 } = {}) => scrapes.push({ step: t0, pan, tune: nScratch++ % 2 ? 7 : 0, orbit: 0, nave: 0,
  strokes: lens.map((len, i) => ({ len, d0: i % 2 ? b : a, d1: i % 2 ? a : b, speed: speed * (i % 2 ? .88 : 1), shape: "bow" })) });
const TRIPLET = [2 / 3, 2 / 3, 2 / 3];
// a riser: one scrape from the middle out to the edge, accelerating, ending on `end`
const scrapeUp = (end, steps, pan = 0) => stroke(end - steps, steps, .08, .97, 2.75, { shape: "out", pan });
// the scratch figures of a drop, a different one each bar of four, moving between the wires and the rim
function scratches(at, b, speed = 2.1) {
  const o = { a: b % 2 ? .34 : .5, b: b % 2 ? .58 : .8, speed: speed + .1 * (b % 3), pan: b % 2 ? .4 : -.4 };
  if (b % 4 === 1) { scratch(at(b, 2), [1, 1], o); scratch(at(b, 10), [...TRIPLET, 1, 1], o); }
  if (b % 4 === 2) scratch(at(b, 6), [.5, .5, .5, .5, 1, 1], o);
  if (b % 4 === 3) { scratch(at(b, 2), TRIPLET, o); scratch(at(b, 13), [1, 1, 1], o); }
  if (b % 4 === 0) { scratch(at(b), [1, 1], o); scratch(at(b, 6), [1, 1], o); }
}
// a fingernail at the edge, answering each snare
const nails = (at, b) => [5.5, 13.5].forEach((p, j) => stroke(at(b, p), 1.4, .9, .97, 2.3, { pan: j ? -.45 : .45, tune: j ? 7 : 0 }));

// ── the top line ──────────────────────────────────────────────────────
// A slow voice over the fast drums: a head bowed with two fingers so it holds a note (see `the song` below),
// each note started by one soft stroke on a ringing tom. Notes are [step, midi, length] from the start of
// the tune; everything sung goes to the cathedral.
const songs = [], tunes = [];
function sing(label, t0, notes, { shift = 0, vel = 1 } = {}) {
  for (const [t, m, len] of notes) {
    songs.push({ step: t0 + t, midi: m + shift, len, vel });
    put("song", "ring", t0 + t, m + shift, len, .5 * vel, 0, { nave: .6 });
  }
  reverso(t0 + notes[0][0], "ring", notes[0][1] + shift, 8, .7, 0, { dist: 18 });
  tunes.push({ tune: label, bar: Math.floor(t0 / BAR) + 1, bars: Math.ceil(Math.max(...notes.map(([t, , len]) => t + len)) / BAR), notes: notes.map(([, m]) => name(m + shift)).join(" ") });
}
// a figure of rungs, sung: the subject as a cantus firmus
const rungs = (t0, figure, { base, scale, flip = false }) => figure.map(([t, k, len]) => [t * scale, ladder(chordAt(t0 + t * scale), flip ? 3 - k : k, base), len * scale]);
// Bachian, over the ground twice (8 bars). It dallies: C held; Db leaning on C; F tied over the bar so that it
// hangs a fourth over the dominant's bass before falling to E (4–3); G carried into F minor as a ninth
// before it falls to F (9–8); the same F–E again to close.
const CHORALE = [[0, 72, 16], [16, 73, 12], [28, 72, 4], [32, 77, 16], [48, 77, 8], [56, 76, 4], [60, 79, 4], [64, 79, 8], [72, 77, 8],
  [80, 77, 8], [88, 73, 8], [96, 73, 8], [104, 77, 8], [112, 77, 8], [120, 76, 8]];
// its second voice, in thirds and sixths under it, moving only where the chord does
const CHORALE_UNDER = [[0, 68, 32], [32, 73, 16], [48, 72, 16], [64, 72, 16], [80, 68, 16], [96, 70, 16], [112, 72, 8], [120, 67, 8]];
// Bachian, over the fifths (8 bars): a line that falls by step, each note a third that is held to become the
// next chord's seventh (Ab over Bbm, G over Ab) before it steps down; F–E at the cadence.
const FALLING = [[0, 68, 32], [32, 67, 32], [64, 65, 40], [104, 64, 8], [112, 64, 8], [120, 67, 8]];
const FALLING_OVER = [[0, 72, 16], [16, 73, 32], [48, 72, 32], [80, 70, 24], [104, 72, 16], [120, 72, 8]];   // its partner, a third and more above
// Pop, over the ground twice (8 bars): one rhythm (long, short, long) every bar, steps down, one leap (Ab up
// to F), a call that ends open on C and a reply that ends on G, waiting for F.
const HOOK = [[0, 72, 6], [6, 70, 2], [8, 68, 8], [16, 77, 6], [22, 75, 2], [24, 73, 8], [32, 73, 6], [38, 72, 2], [40, 70, 8], [48, 72, 6], [54, 73, 2], [56, 72, 8],
  [64, 72, 6], [70, 70, 2], [72, 68, 8], [80, 77, 6], [86, 75, 2], [88, 73, 8], [96, 73, 6], [102, 72, 2], [104, 70, 8], [112, 67, 6], [118, 68, 2], [120, 67, 8]];

// ── rock ──────────────────────────────────────────────────────────────
// The riff: power chords, root fifth and octave struck together on single-mode toms (one clean pitch each, so
// the drive on their bus adds harmonics and not hash). Four bars the ear can hold, over the ground, leaning
// on Ab and Eb. Per chord: [step, semitones from F2, length].
const RIFF = {
  Fm: [[0, 0, 3], [3, 0, 3], [6, 3, 2], [8, 0, 2], [10, -2, 2], [12, 0, 2], [14, 3, 2]],
  Db: [[0, -4, 3], [3, -4, 3], [6, 0, 2], [8, -4, 2], [10, -2, 2], [12, -4, 2], [14, -2, 2]],
  Bbm: [[0, 5, 3], [3, 5, 3], [6, 8, 2], [8, 5, 2], [10, 3, 2], [12, 5, 2], [14, 8, 2]],
  C: [[0, 7, 3], [3, 7, 3], [6, 2, 2], [8, 7, 2], [10, 5, 2], [12, 7, 4]],
};
const low = (c) => (c.root > 7 ? c.root - 12 : c.root);
const power = (step, off, len, vel = 1, more = {}) => [[0, 0], [7, -.5], [12, .5]].forEach(([up, pan]) => put("riff", "power", step, 41 + off + up, len, vel * (up ? .85 : 1), pan, more));
// half: for the mud and the brakes, the root held, pushed, its fifth below, the root again.
// eighths: the bass drives in straight eighths under the riff, on whatever root the riff is on.
function riff(t0, { half = false, vel = 1, eighths = false } = {}) {
  const c = chordAt(t0), r = low(c), notes = half ? [[0, r, 6], [6, r, 2], [8, r - 5, 4], [12, r, 4]] : RIFF[c.say] ?? [[0, r, 3], [3, r, 3], [6, r + 7, 2], [8, r, 2], [10, r + 7, 2], [12, r, 4]];
  for (const [t, off, len] of notes) power(t0 + t, off, len, vel * (t % 8 === 0 ? 1 : .85));
  if (eighths) for (let e = 0; e < BAR; e += 2) put("bass", "floor", t0 + e, BASS + mod(notes.findLast(([t]) => t <= e)[1], 12), 2, vel * (e % 4 ? .8 : 1));
}
// a crash, without noise: three high toms left to ring, in the cathedral
const crash = (step, vel = .85) => [0, 2, 3].forEach((k, j) => put("fx", "ring", step, ladder(chordAt(step), k, LEAD), 24, vel, [-.6, .6, 0][j], { nave: .6 }));

// ── donks and wobbles ─────────────────────────────────────────────────
// A donk is the bass as a small hard drum: a short hollow stab on the "and" of each beat, answering the kick,
// in a four-note figure per chord from F minor's root, b3, 4, 5 and b7 (semitones from F2).
const donks = [], wobs = [];
const DONK = { Fm: [0, 3, 0, 7], Db: [0, 3, 0, 10], Bbm: [5, 0, 5, 3], C: [7, 5, 7, 7] };
function donk(t0, vel = 1) {
  DONK[chordAt(t0).say].forEach((off, i) => { const step = t0 + 2 + 4 * i; if (!inGap(step) && !RIDE.air[step]) donks.push({ step, midi: 41 + off, vel: vel * (i % 2 ? .9 : 1) }); });
}
// a wobble is one long bass note whose brightness opens and closes in time (see `the wobble` below)
const wobble = (step, len, midi, vel = 1) => wobs.push({ step, len, midi, vel });

// ── reversos ──────────────────────────────────────────────────────────
// A reversed stroke swells for `len` steps and stops dead on `step`. A reversed fill is the roll down the
// kit played backwards, so it climbs and swells. `swells` are reversed cathedral tails (see `the reversed tails`).
const swells = [];
const reverso = (step, kind, midi, len, vel = 1, pan = 0, more = {}) => put("rev", kind, step, midi, len, vel, pan, { rev: true, ...more });
function rfill(t0, count, gap = 1, vel = 1, more = {}) {
  for (let i = 0; i < count; i++) {
    const rung = Math.floor(i * 10 / count);
    reverso(t0 + (i + 1) * gap, rung > 6 ? "high" : rung > 2 ? "rack" : "floor", ladder(chordAt(t0), rung, TENOR - 12), 2 * gap, vel * (.5 + .5 * i / count), -.8 + 1.6 * i / (count - 1), more);
  }
}

// ════ i · intro: something to mix on ══════════════════════════════════
{
  const at = section("i · intro", GROUND, 16, "kick on every beat and hats; from bar 5 open hats; from bar 9 the bass and the subject, quietly and 20 m off, in the tenor; from bar 13 the snare; slow drags across the head; a far floor tom left to ring every fourth bar");
  for (let b = 1; b <= 16; b++) {
    drums(at(b), b > 12 ? "four" : "floor", .95);
    hats(at(b), b < 5 ? 5 : b < 9 ? 7 : 9, { rot: b, vel: .9, open: b > 4 });
    if (b > 8) { bass(at(b), "..r...r...r..fr.", .85); fig("tenor", "rack", at(b), SUBJECT, { base: TENOR, vel: .5 + .04 * (b - 8), pan: -.35, dist: 20 }); }
    if (b % 4 === 0 && b < 16) put("fx", "floor", at(b, 12), bassNote(chordAt(at(b))) + 12, 8, 1, b % 8 ? -.4 : .4, { dist: 30 });
  }
  rfill(at(16), 8, 1, .8, { dist: 12 });
  fill(at(16, 8), 8, 1, .9);
  for (let b = 1; b <= 15; b += 2) {
    stroke(at(b, 4), 10, .05, .95, .9 + .04 * b, { pan: b % 4 === 1 ? -.5 : .5, nave: .5 });
    stroke(at(b + 1, 2), 8, .95, .1, 1, { pan: b % 4 === 1 ? .5 : -.5, tune: 7, nave: .5 });
  }
  scrapeUp(at(17), 8);
  swells.push(at(17));
}

// ════ ii · exposition: the voices enter, and the chorale ══════════════
{
  const at = section("ii · exposition", GROUND, 16, "two-step with flams on the backbeat; the subject in the tenor every bar, close; from bar 5 the alto answers half a bar later, a rung higher, from 14 m away, and comes close at bar 9; the countersubject in the lead; the chorale, twice; bar 8 ends on a hit left to ring");
  for (let b = 1; b <= 16; b++) {
    drums(at(b), b % 4 === 0 ? "chopA" : "two", 1, { flam: b % 2 === 0 });
    hats(at(b), 9, { rot: b, open: b > 8 });
    bass(at(b), "r..r..r.f.r.o.r.");
    fig("tenor", "rack", at(b), SUBJECT, { base: TENOR, pan: -.35 });
    if (b > 4) fig("alto", "high", at(b, 8), SUBJECT, { base: ALTO, shift: 1, vel: .9, pan: .35, ...(b < 9 ? { dist: 14 } : {}) });
    if (b > 8) fig("lead", "high", at(b), COUNTER, { base: LEAD, vel: .9, pan: b % 2 ? .15 : -.15 });
    if (b % 4 === 0) fill(at(b, 12), 4, 1, .9);
    if (b > 4 && b % 2 === 0) nails(at, b);
    if (b > 8 && b % 4 === 0) scratch(at(b, 8), [1, 1, ...TRIPLET], { pan: b % 8 ? .3 : -.3 });
    if (b === 12 || b === 16) reverso(at(b, 12), "ring", 60, 4, .8, .3);
  }
  sing("chorale", at(1), CHORALE);
  sing("chorale", at(9), CHORALE);
  sing("chorale, second voice", at(9), CHORALE_UNDER, { vel: .7 });
  scrapeUp(at(17), 4);
  swells.push(at(17));
}

// ════ iii · drop: the riff ════════════════════════════════════════════
{
  const at = section("iii · drop", GROUND, 16, "the riff in power chords over a chopped two-step with a fat, flammed backbeat; bars 1–8 the bass in straight eighths, bars 9–16 the bass on the beats and the donks on the off-beats; subject in the tenor and, twice as fast, in the lead; the top line sings the subject four times as slow, then its mirror");
  crash(at(1));
  for (let b = 1; b <= 16; b++) {
    drums(at(b), b % 2 ? "chopA" : "chopB", 1, { flam: true, fat: true });
    hats(at(b), 8, { rot: b, open: true });
    riff(at(b), { eighths: b <= 8 });
    if (b > 8) { bass(at(b), "r...r...r...r..."); donk(at(b)); }
    fig("tenor", "rack", at(b), SUBJECT, { base: TENOR, pan: -.35 });
    fig("lead", "high", at(b, b % 2 ? 8 : 0), SUBJECT, { base: LEAD, scale: .5, shift: b % 2, vel: .85, pan: b % 2 ? .2 : -.2 });
    if (b % 4 === 0 && b !== 8) run("tenor", "rack", at(b, 8), 16, 7, -1, { base: TENOR, gap: .5, pan: .7 });
    if (b % 2 === 0) scratches(at, b);
    if (b % 4 === 0 && b !== 8) reverso(at(b, 12), "ring", 60, 4, .8, -.3);
  }
  crash(at(9), .7);
  for (const [bar, flip] of [[1, false], [5, true], [9, false], [13, true]])
    sing(flip ? "subject, mirrored, four times as slow" : "subject, four times as slow", at(bar), rungs(at(bar), SUBJECT, { base: ALTO, scale: 4, flip }));
}

// ════ iv · sequence: round the fifths, half-time in the mud ═══════════
{
  const at = section("iv · sequence", FIFTHS, 16, "half-time rock in the mud: a kick on one, a fat flammed backbeat on three, the riff slow in power chords, a wobble bass under it whose rate follows the gear; the lead climbs and falls each chord in sixteenths, the countersubject in the tenor; the top line falls by step, a chain of sevenths; bar 8 stops for half a bar on a hit left to ring");
  const ARP = [0, 1, 2, 3, 2, 1, 2, 3, 4, 3, 2, 3, 2, 1, 0, 1];
  crash(at(1));
  for (let b = 1; b <= 16; b++) {
    drums(at(b), b % 4 === 0 ? "halfB" : "half", 1, { flam: true, fat: true });
    hats(at(b), 9, { rot: b, open: true });
    riff(at(b), { half: true });
    wobble(at(b), b === 8 ? 8 : 16, bassNote(chordAt(at(b))));
    ARP.forEach((k, i) => put("lead", "high", at(b, i), ladder(chordAt(at(b)), k + (b > 8 ? 1 : 0), LEAD), 1, i % 4 === 0 ? 1 : .7, i % 2 ? .25 : -.25));
    fig("tenor", "rack", at(b), COUNTER, { base: TENOR, shift: -1, pan: -.35 });
    if (b % 4 === 0) fill(at(b, 8), 16, .5, 1);
    // a quick rub at the rim before beats three and one, every other bar, and a triplet scratch every fourth
    if (b % 2) for (const p of [7, 15]) stroke(at(b, p), .9, .52, .68, 2.2, { pan: p === 7 ? -.35 : .35, tune: p === 7 ? 0 : 7 });
    if (b % 4 === 0) scratch(at(b, 12), TRIPLET, { a: .66, b: .9, speed: 2.4 });
    if (b % 4 === 2) reverso(at(b, 8), "ring", 58, 6, .8, b % 8 === 2 ? .3 : -.3);
  }
  rfill(at(16), 8, 1, .9);
  sing("falling sevenths", at(1), FALLING);
  sing("falling sevenths", at(9), FALLING);
  sing("falling sevenths, voice above", at(9), FALLING_OVER, { vel: .75 });
  scrapeUp(at(17), 8);
  swells.push(at(17));
}

// ════ v · breakdown: a chorale of rolled toms, and the hook ═══════════
// The engine has overheated; the climb is slow. Everything turns slowly round the listener, twice in sixteen bars.
{
  const at = section("v · breakdown", GROUND, 16, "no kick: four undamped toms rolled on each chord in the deep cathedral, the whole circle of them turning slowly round the head; a floor tom 30 m off on the third beat of every other bar, some of them swelled into from further off; the pop hook, alone, twice; then a roll that climbs two octaves and a kick that doubles, down the hill into the drop");
  const turn = (b) => 2 * Math.PI * 2 * (b - 1) / 16;
  for (let b = 1; b <= 16; b++) {
    const c = chordAt(at(b)), swell = .25 + .12 * ((b - 1) % 4) + .03 * b;
    [[BASS + 12, 0], [TENOR - 12, 1], [TENOR, 2], [ALTO, 0]].forEach(([base, k], j) =>
      roll("choir", "ring", at(b), BAR, k ? ladder(c, k, base) : bassNote(c) + 12, [swell, swell + .12], { ang: turn(b) + j * Math.PI / 2, nave: .7 }));
    hats(at(b), 3 + Math.floor(b / 4), { rot: b, vel: .6, turn: turn(b) + Math.PI });
    if (b % 2 === 0 && b <= 12) {
      put("fx", "floor", at(b, 8), bassNote(c) + 12, 8, 1.1, b % 4 ? -.3 : .3, { dist: 30 });
      if (b % 4 === 0) reverso(at(b, 8), "ring", bassNote(c) + 24, 8, .8, b % 8 ? .4 : -.4, { dist: 25 });
    }
  }
  rfill(at(12, 8), 8, 1, .8, { dist: 16 });
  spins.push({ what: "the chorale's four toms and the hats, a quarter-circle apart", turns: 2, where: "v · breakdown, all 16 bars" });
  sing("hook", at(1), HOOK);
  sing("hook", at(9), HOOK);
  // the riser: a rack tom rolled faster and higher, and the kick from quarters to sixteenths
  for (let i = 0, t = 0; t < 64; i++) { const u = t / 64; put("fx", "rack", at(13, t), 48 + 24 * u ** 1.5, 1, .35 + .65 * u, Math.sin(i) * .6); t += 2 * .25 ** u; }
  for (const [bar, gap] of [[13, 4], [14, 4], [15, 2], [16, 1]]) for (let t = 0; t < (bar === 16 ? 12 : 16); t += gap) put("kick", "kick", at(bar, t), BASS, 2, .6 + .1 * (bar - 13), 0, { always: true });
  roll("snare", "rim", at(16, 8), 4, 60, [.4, 1], { per: 4, nave: .5 });
  // slow drags over the whole head while the tempo sinks, a slower one under them every four bars
  for (let b = 1; b <= 12; b++) {
    stroke(at(b), 14, b % 2 ? .05 : .95, b % 2 ? .95 : .05, .7 + .06 * b, { pan: b % 2 ? -.6 : .6, tune: b % 2 ? 0 : 7, nave: .7 });
    if (b % 4 === 1) stroke(at(b, 8), 40, .2, .9, .6, { tune: 7, orbit: 1, nave: .7 });
  }
  // and with the riser, scrapes outward that halve in length each bar: 2, 4, 8, 16 to the bar
  for (let bar = 13; bar <= 16; bar++) { const len = 8 / 2 ** (bar - 13); for (let t = 0; t < 16; t += len) stroke(at(bar, t), len, .1, .95, 1.2 + .4 * (bar - 13) + .02 * t, { shape: "out", pan: (t / len) % 2 ? .5 : -.5 }); }
  swells.push(at(1), at(17));
}

// ════ vi · second drop: stretto and donks, then the riff ══════════════
{
  const at = section("vi · second drop", GROUND, 24, "bars 1–8: stretto, three voices a quarter bar apart, handed round the head a third of a circle each bar, over the donks; 9–16: the riff over a kick on every beat, the subject twice as slow in the bass; 17–24: the riff with the bass in eighths and the donks by turns, runs, and bar 20 ending on a hit left to ring; the hook three times over the top");
  crash(at(1)); crash(at(9), .7); crash(at(17));
  for (let b = 1; b <= 24; b++) {
    const part = b < 9 ? 0 : b < 17 ? 1 : 2, round = part === 0 ? (k) => ({ ang: 2 * Math.PI * (b + k) / 3 }) : () => ({});
    drums(at(b), part === 0 ? (b % 2 ? "chopB" : "chopA") : b % 4 === 0 ? "hard" : "four", 1, { flam: true, fat: part > 0 });
    if (part) for (const p of [7, 9, 15]) put("snare", "rim", at(b, p), 63 + p % 4, 1, .35, -.1);
    hats(at(b), part === 2 ? 9 : 8, { rot: b, open: true });
    if (part === 0) { bass(at(b), "r.........r....."); donk(at(b)); }
    if (part === 1) { riff(at(b)); if (b % 2) fig("bass", "floor", at(b), SUBJECT, { base: BASS + 12, scale: 2 }); }
    if (part === 2) { riff(at(b), { eighths: b % 2 === 0 }); if (b % 2) { bass(at(b), "r...r...r...r..."); donk(at(b)); } }
    fig("tenor", "rack", at(b), SUBJECT, { base: TENOR, pan: -.35, ...round(0) });
    if (part === 0) fig("alto", "high", at(b, 4), SUBJECT, { base: ALTO, shift: 1, vel: .9, pan: .35, ...round(1) });
    fig("lead", "high", at(b, 8), SUBJECT, { base: LEAD, shift: 2, vel: .9, pan: b % 2 ? .15 : -.15, ...round(2) });
    if (part === 2 && b % 2 === 0 && b !== 24 && b !== 20) run("tenor", "rack", at(b, 8), 16, b % 4 ? 0 : 9, b % 4 ? 1 : -1, { base: TENOR, gap: .5, pan: .7 });
    if (b % 8 === 0 && b !== 8 && b !== 16) fill(at(b, 8), 16, .5, 1);
    if (part !== 1 && b % 2 === 0) scratches(at, b, 2.3);
    if (part === 2 && b % 2) for (const p of [3, 11]) stroke(at(b, p), .9, .52, .68, 2.3, { pan: p === 3 ? -.35 : .35, tune: p === 3 ? 0 : 7 });
    if (part === 1 && b % 2 === 0) nails(at, b);
    if (b % 4 === 0 && b % 8) reverso(at(b, 12), "ring", 60, 4, .8, b % 8 === 4 ? .3 : -.3);
  }
  rfill(at(16), 8, 1, .9);
  spins.push({ what: "the stretto: tenor, alto and lead a third of a circle apart, stepping round", turns: "a third of a turn a bar, 8 bars", where: "vi · second drop, bars 1–8" });
  for (const bar of [1, 9, 17]) sing("hook", at(bar), HOOK);
  for (const bar of [17, 21]) sing("subject, four times as slow, under the hook", at(bar), rungs(at(bar), SUBJECT, { base: TENOR, scale: 4 }), { vel: .7 });
  swells.push(at(25));
}

// ════ vii · finale: the hook over the chorale ═════════════════════════
{
  const at = section("vii · finale", [...FIFTHS, ...GROUND, ...GROUND], 16, "round the fifths once more over the hardest kick, the line of sevenths above; then the riff with the subject in tenor and lead, the pop hook over the chorale, donks and eighths by turns, bar 12 ending on a hit left to ring; the throttle shuts and the last bars go to half time over a wobble as the bike slows; a roll down all the toms; F major, and the brakes");
  const ARP = [0, 1, 2, 3, 2, 1, 2, 3, 4, 3, 2, 3, 2, 1, 0, 1];
  crash(at(1)); crash(at(9));
  for (let b = 1; b <= 16; b++) {
    const braking = b > 12;
    drums(at(b), braking ? (b === 16 ? "halfB" : "half") : b % 4 === 0 ? "hard" : "four", 1, { flam: b > 8, fat: b > 8 });
    hats(at(b), 9, { rot: b, open: true });
    if (b <= 8) {
      bass(at(b), "..r...o...r...of");
      ARP.forEach((k, i) => put("lead", "high", at(b, i), ladder(chordAt(at(b)), k + 1, LEAD), 1, i % 4 === 0 ? 1 : .7, i % 2 ? .25 : -.25));
      fig("tenor", "rack", at(b), SUBJECT, { base: TENOR, pan: -.35 });
      fig("alto", "high", at(b, 8), SUBJECT, { base: ALTO, flip: true, vel: .9, pan: .35 });
      if (b % 2) for (const p of [7, 15]) stroke(at(b, p), .9, .52, .68, 2.3, { pan: p === 7 ? -.35 : .35, tune: p === 7 ? 0 : 7 });
    } else {
      if (braking) { riff(at(b), { half: true }); if (b < 16) wobble(at(b), 16, bassNote(chordAt(at(b)))); }
      else { riff(at(b), { eighths: b % 2 === 0 }); if (b % 2) { bass(at(b), "r...r...r...r..."); donk(at(b)); } }
      for (const [bus, kind, base, pan] of [["tenor", "rack", TENOR, -.35], ["lead", "high", LEAD, 0]]) fig(bus, kind, at(b), SUBJECT, { base, pan });
      if (b < 16 && b % 2 === 0) scratches(at, b, 2.4);
    }
    if (b === 4 || b === 12) fill(at(b, 8), 16, .5, 1);
    if (b === 14) reverso(at(b, 8), "ring", 58, 6, .8, .3);
  }
  sing("falling sevenths", at(1), FALLING, { shift: 12 });
  sing("hook", at(9), HOOK);
  sing("chorale, under the hook", at(9), CHORALE, { shift: -12, vel: .75 });
  fill(at(16), 32, .5, 1.05);
  // the last chord: F major, every drum struck once and the chorale rolled away under it, all of it in the cathedral
  const end = at(17);
  put("kick", "kick", end, BASS, 4, 1.1);
  [[BASS, 0], [BASS + 12, 0], [TENOR, 0], [TENOR, 1], [TENOR, 2], [ALTO, 1], [ALTO, 2], [LEAD, 0], [LEAD, 1]].forEach(([base, k], j) => {
    const midi = ladder(CH.F, k, base);
    put(j < 2 ? "bass" : "choir", "ring", end, midi, 40, 1, [-.6, .6, -.3, .3, 0][j % 5], { nave: j < 2 ? 0 : 1 });
    if (j > 1) roll("choir", "ring", end + 1, 28, midi, [.5, .05], { pan: [-.6, .6, -.3, .3, 0][j % 5], nave: 1 });
  });
  put("snare", "rim", end, 60, 3, 1.1, .05, { nave: .8 });
  sing("the last note", end, [[0, 77, 36]]);
  // the brakes: one long drag in to the centre, slowing to nothing
  stroke(end + 2, 30, .92, .1, 2.4, { shape: "stop", tune: 0, nave: .8 });
  swells.push(end);
}
const END = POS;
// The gaps: at each, one hit (the chord in power chords, and a floor tom 25 m off) is left to ring in the
// cathedral while everything else is silent.
for (const [a, z] of GAPS) {
  power(a, low(chordAt(a)), z - a, 1.1, { gap: true, nave: 1 });
  put("fx", "floor", a, bassNote(chordAt(a)) + 12, z - a, 1.1, 0, { gap: true, dist: 25 });
  put("kick", "kick", a, kickNote(chordAt(a)), 2, 1, 0, { gap: true, always: true });
}

// ── the machine, heard ────────────────────────────────────────────────
// Every gear change is a clunk (a rimshot and a floor tom, lower for a downshift). Every jump is a flip: a
// roll of high toms and a scrape circling the head for as long as the wheel is off the ground, more turns
// the longer the flight, cut dead by the landing, which is the biggest floor tom there is, in the cathedral.
// Freewheeling or flying, the ratchet clicks at wheel speed and circles too. The tyre scrapes on the way down.
for (const s of RIDE.shifts) {
  const down = s.to < s.from;
  put("fx", "rim", s.step, down ? 46 : 53, 1, .8, down ? -.3 : .3, { nave: .3 });
  put("fx", "floor", s.step, down ? 38 : 43, 2, .7, down ? -.3 : .3);
  // and two strokes down the kit into it
  [[1, 5], [.5, 3]].forEach(([back, rung]) => put("tenor", rung > 3 ? "rack" : "floor", s.step - back, ladder(chordAt(s.step), rung, TENOR - 12), .5, .7, down ? -.4 : .4));
}
for (const j of RIDE.jumps) {
  const turns = j.steps <= 4 ? 1 : j.steps <= 6 ? 2 : 3, n = 2 * j.steps, land = j.takeoff + j.steps;
  for (let i = 0; i < n; i++) put("fx", "high", j.takeoff + i / 2, ladder(chordAt(j.takeoff), 3 + i % 4, TENOR + 12), .5, .55 + .45 * i / n, 0, { ang: 2 * Math.PI * turns * i / n, nave: .35 });
  stroke(j.takeoff, j.steps, .3, .95, 2.6, { shape: "flat", orbit: turns });
  put("kick", "kick", land, BASS, 4, 1.25, 0, { always: true });
  put("fx", "floor", land, BASS, 14, 1.25, 0, { nave: .9 });
  put("fx", "rim", land, 48, 2, .9, 0, { nave: .6 });
  reverso(land, "floor", BASS + 12, j.steps, 1, 0, { nave: .5 });      // the ground coming up
  spins.push({ what: "a roll of high toms and a scrape", turns, where: `the jump landing on bar ${j.lands}, ${j.steps} sixteenths in the air` });
}
{
  // the freewheel: runs of steps spent coasting or in the air, clicked at 2.6 clicks a metre
  const tick = rng(11);
  for (let s = 0, from = -1; s <= STEPS; s++) {
    const free = s < STEPS && (RIDE.coast[s] || RIDE.air[s]);
    if (free && from < 0) from = s;
    if (!free && from >= 0) {
      const t0 = RIDE.T[from], t1 = RIDE.T[s], air = RIDE.air[from], turns = Math.max(1, Math.round((t1 - t0) / 1.5));
      for (let t = t0, i = 0; t < t1; i++) {
        const u = (t - t0) / (t1 - t0);
        events.push({ bus: "hats", kind: "tick", t, step: from + u * (s - from), midi: 103 + i % 3, len: .1, vel: .5 + .15 * tick(), pan: 0, ang: 2 * Math.PI * turns * u, ratchet: true });
        t += 1 / (2.6 * RIDE.v[from] * (air ? 1.3 + .4 * u : 1));
      }
      if (!air) { spins.push({ what: "the freewheel's ratchet", turns, where: `coasting, bars ${Math.floor(from / BAR) + 1}–${Math.floor((s - 1) / BAR) + 1}` }); stroke(from, s - from, .5, .72, 1.5, { shape: "flat", tune: null, orbit: turns }); }
      from = -1;
    }
  }
}
// the rock garden's rattle: loose strokes on rack toms and rims, about five a bar, wherever a stone is
{ const loose = rng(0x0c7); for (let st = 0; st < STEPS; st++) if (RIDE.rocks[st] && loose() < .3) put("fx", loose() < .6 ? "rack" : "rim", st + .5 * Math.floor(loose() * 2), 43 + Math.floor(loose() * 14), .5, .25 + .3 * loose(), loose() * 1.6 - .8); }
const N = Math.ceil((timeAt(END) + TAIL) * SR);

// ── the ear (from gundrum-and-bass-2) ─────────────────────────────────
// Every placed source is heard by two ears 17.5 cm apart instead of being panned by level: from the side
// the far ear hears it up to 0.66 ms later, about 3 dB quieter and darker (the head's shadow). earAt is a
// fixed source; earMoving takes a pan per sample. A source behind the head (an angle past a quarter turn)
// is also a little quieter and duller in both ears, which is what tells a circle from a swing.
const EAR_GAP = .0875 / 343;
function earAt(out, pan) {
  const th = Math.max(-1, Math.min(1, pan)) * Math.PI / 2, s = Math.abs(Math.sin(th));
  const d = Math.round(EAR_GAP * (Math.abs(th) + s) * SR), near = .707 * (1 + .12 * s), far = .707 * (1 - .3 * s);
  const a = 1 - Math.exp(-2 * Math.PI * (20000 / (1 + 4 * s)) / SR);
  const nearCh = pan >= 0 ? out.R : out.L, farCh = pan >= 0 ? out.L : out.R, n = nearCh.length;
  let lp = 0;
  return (k, v) => { if (k >= 0 && k < n) nearCh[k] += near * v; lp += a * (v - lp); const j = k + d; if (j >= 0 && j < n) farCh[j] += far * lp; };
}
function earMoving(out) {
  let lpL = 0, lpR = 0;
  const n = out.L.length, add = (ch, x, v) => { const j = Math.floor(x), f = x - j; if (j >= 0 && j + 1 < n) { ch[j] += v * (1 - f); ch[j + 1] += v * f; } };
  return (k, v, pan) => {
    const th = Math.max(-1, Math.min(1, pan)) * Math.PI / 2, s = Math.sin(th), as = Math.abs(s), itd = EAR_GAP * (Math.abs(th) + as) * SR;
    const shade = 1 - Math.exp(-2 * Math.PI * (20000 / (1 + 4 * as)) / SR), open = 1 - Math.exp(-2 * Math.PI * 20000 / SR);
    lpL += (s > 0 ? shade : open) * (v - lpL); lpR += (s < 0 ? shade : open) * (v - lpR);
    add(out.L, k + (s > 0 ? itd : 0), .707 * (1 + (s < 0 ? .12 : -.3) * as) * lpL);
    add(out.R, k + (s < 0 ? itd : 0), .707 * (1 + (s > 0 ? .12 : -.3) * as) * lpR);
  };
}
const round = (ang) => [Math.sin(ang), Math.max(0, -Math.cos(ang))];             // an angle as [pan, how far behind]
const stereo = () => ({ L: new Float32Array(N), R: new Float32Array(N) });
// lay a rendered stroke into a bus at a place
// dist: at that many metres the stroke is quieter, loses its top to the air, and arrives late (2.9 ms a metre)
function lay(out, y, i0, gain, pan, behind = 0, dist = 0) {
  const ear = earAt(out, pan), g = gain * (1 - .3 * behind) * (dist ? Math.min(1, (2 / dist) ** .6) : 1), a = (1 - .75 * behind) * (dist ? 1 - Math.exp(-2 * Math.PI * 14000 / (1 + dist / 3) / SR) : 1), late = Math.round(dist / 343 * SR);
  for (let i = 0, lp = 0; i < y.length && i0 + late + i < N; i++) { lp += a * (y[i] - lp); ear(i0 + late + i, g * lp); }
}

// ── render: every event is one stroke on one tom, placed in its bus ───
// level: where the bus sits (dB, RMS of its loud part). send: how much of it reaches the small hall.
// nave: how much of the whole bus reaches the cathedral (single strokes and scrapes also send by moment).
// drive: the bus is pushed into a tanh, its loud-part level arriving at that value, before it is set to its
// level: on single-pitched toms that adds harmonics (the power chords' crunch, the bass's grit), not hash.
const BUS = {
  kick: { level: -12.5, send: 0 }, bass: { level: -13.5, send: 0, drive: .8 }, snare: { level: -17, send: .16 }, hats: { level: -23.5, send: .05 },
  tenor: { level: -19.5, send: .2 }, alto: { level: -21, send: .25 }, lead: { level: -22, send: .3 }, choir: { level: -19.5, send: .3 }, fx: { level: -19.5, send: .3 },
  scrape: { level: -24.5, send: .08 }, song: { level: -15, send: .1, nave: .85 },
  riff: { level: -18.5, send: .12, drive: 1.6 }, donk: { level: -17.5, send: .04 }, wob: { level: -15.5, send: 0 }, rev: { level: -20, send: .15, nave: .35 },
};
const bus = Object.fromEntries(Object.keys(BUS).map((k) => [k, stereo()])), nave = Object.fromEntries(Object.keys(BUS).map((k) => [k, stereo()]));
events.sort((a, b) => a.step - b.step);
for (const e of events) {
  e.time = +(e.t ?? timeAt(e.step)).toFixed(5);
  let y = tom(e.kind, e.midi, e.vel, e.t != null ? .03 : e.rev ? e.time - timeAt(e.step - e.len) : timeAt(e.step + e.len) - e.time), i0 = Math.round(e.time * SR);
  if (e.rev) { y = Float32Array.from(y).reverse(); for (let i = 0; i < 96; i++) y[y.length - 1 - i] *= i / 96; i0 -= y.length; }   // it ends on its step
  const [pan, behind] = e.ang == null ? [e.pan, 0] : round(e.ang), wet = e.nave ?? (e.dist ? .9 : 0);
  lay(bus[e.bus], y, i0, e.vel, pan, behind, e.dist);
  if (wet) lay(nave[e.bus], y, i0, e.vel * wet * (e.dist ? Math.min(1, (2 / e.dist) ** .3) : 1), pan, behind, e.dist);
}

// ── the friction: a finger on a drumhead ─────────────────────────────
// Menu Band's drum-skin scratch gave the layout (skin, wires, rim, hat and metal edge by distance from the
// centre) and the way it is driven: control arriving 120 times a second and held, then smoothed with a
// 2.5 ms attack and a 6 ms release. The sound itself is now three things a real scratch is made of.
//   The hand. Each stroke has a bell-shaped speed (minimum jerk); between the strokes of a scratch the finger
//   stands still for 10–40 ms; length, top speed, pressure and path differ from stroke to stroke (most
//   strokes are light, one in five or so is leaned on); speed
//   and pressure tremble at 8–12 Hz; a gesture starts a few ms early or late.
//   The contact. The fingertip is a 1.5 g mass on a spring dragged by the hand. Friction falls as it slips
//   faster (Stribeck), so it sticks, stretches the spring, lets go and catches again: slow irregular creaks
//   when the hand is slow and heavy, a steady slide when it is fast. The head's surface is uneven (0.4 mm
//   cells), which is what makes the catches irregular. On top, every bump it passes is a tiny impact: a
//   Poisson train whose rate is the tip's speed times bumps per metre, softened less the faster it goes, so
//   the sound brightens with speed and is nothing when the finger stops. A nail is harder: fewer, sharper
//   impacts, less grip, no rumble.
//   The drum. That force drives the same head the toms have (modes at 1, 1.59, 2.14, 2.30, 2.65 times its
//   note), each mode as strongly as its shape says for where the finger is (the centre cannot move the
//   modes with a node there). The pressing finger damps the head and tightens it, so it barely rings and
//   sits a little sharp while rubbed, and rings on at its own pitch when the finger lifts. On the rim and
//   the metal edge the stiffer, brighter modes of hoop and shell answer instead.
const smooth = (a, b, x) => { const t = Math.max(0, Math.min(1, (x - a) / (b - a))); return t * t * (3 - 2 * t); };
const EDGES = [[.23, .31], [.4, .48], [.62, .7], [.88, .965]], HEAD = [48, 90, 185, 360, 560];
const material = (vals, d) => EDGES.reduce((v, [a, b], k) => v + (vals[k + 1] - v) * smooth(a, b, d), vals[0]);
const lift = (speed) => 2 ** Math.min(2.25, .82 * speed);
const bessel = (n, x) => { let sum = 0; for (let k = 0; k < 32; k++) { const t = Math.PI * (k + .5) / 32; sum += Math.cos(n * t - x * Math.sin(t)); } return sum / 32; };
const SHAPES = [[0, 2.405, 1], [1, 3.832, .58], [2, 5.136, .49], [0, 5.52, 1], [3, 6.38, .43]];     // [order, root, peak] of each head mode's shape
const RIM = [2.9, 4.6, 6.3, 8.1, 10.4], METAL = [5.4, 8.9, 12.6, 17.2];
const CONTROL = 120, HOLD = Math.round(SR / CONTROL);
// grip standing and sliding, bumps per metre, how soft an impact is at rest and how bright at full speed, and what is cut below
const SKIN = { muS: .95, muK: .45, perMetre: 9000, soft: 450, bright: 4200, cut: 0 }, NAIL = { muS: .5, muK: .3, perMetre: 2500, soft: 2500, bright: 9000, cut: 500 };
const TIP = { mass: .0015, spring: 900, damp: .35, slipSpeed: .02 }, METRES = .12, NEWTONS = 2.2;   // one unit of speed is 12 cm/s; full pressure 2.2 N
const hand = rng(0xf1d6), surface = rng(0x51a7);
const cells = Float32Array.from({ length: 4096 }, () => surface());
const grain = (x) => { const q = Math.abs(x) / .0004, j = Math.floor(q), f = q - j; return .7 + .6 * (cells[j & 4095] * (1 - f) + cells[(j + 1) & 4095] * f); };
const SPEED = { bow: (u) => 30 * u * u * (1 - u) * (1 - u) / 1.875, out: (u) => u * u * (3 - 2 * u), stop: (u) => (1 - u) ** 1.5, flat: (u) => Math.min(1, u * 12, (1 - u) * 12) };
// one gesture → its sound, mono, with what the hand did (for the measurements)
function rub(g) {
  const start = timeAt(g.step) + (hand() - .5) * .008, many = g.strokes.length > 1;
  let at = g.step;
  const strokes = g.strokes.map((k) => {
    const a = timeAt(at) - timeAt(g.step), dur = timeAt(at + k.len) - timeAt(at), rest = many ? Math.min(dur * .4, .01 + .03 * hand()) : 0;
    at += k.len;
    return { ...k, a, dur, move: dur - rest, peak: k.speed * (.85 + .3 * hand()), press: (.22 + .78 * hand() ** 3) * (.7 + .12 * k.speed), d0: k.d0 + .03 * (hand() - .5), d1: k.d1 + .03 * (hand() - .5) };
  });
  const total = strokes.at(-1).a + strokes.at(-1).dur, n = Math.round(total * SR), y = new Float32Array(n + Math.round(.4 * SR));
  const first = strokes[0], nail = (first.d0 + first.d1) / 2 > .86, tip = nail ? NAIL : SKIN;
  // which drum: the one the old model's pitch pointed at, moved to the chord's root or fifth, between F2 and F4
  // (rack and floor toms: a rubbed head that small would only whistle)
  const m = 69 + 12 * Math.log2(material(HEAD, (first.d0 + first.d1) / 2) * lift(first.speed) / 440), want = g.tune == null ? Math.round(m) : F + chordAt(g.step).root + g.tune;
  let midi = want + 12 * Math.round((m - want) / 12);
  while (midi > 65) midi -= 12; while (midi < 41) midi += 12;
  g.drum = name(midi);
  const hz = 440 * 2 ** ((midi - 69) / 12), ring = .3 * (110 / hz) ** .3;
  // the hand, 120 times a second: speed, pressure and place
  const C = Math.ceil(n / HOLD) + 1, V = new Float32Array(C), P = new Float32Array(C), D = new Float32Array(C), shake = 8 + 4 * hand(), ph = 6.28 * hand();
  for (const k of strokes) {
    const c0 = Math.round(k.a * CONTROL), c1 = Math.min(C, Math.round((k.a + k.dur) * CONTROL));
    let gone = 0, sum = 1e-9;
    for (let c = c0; c < c1; c++) sum += SPEED[k.shape](Math.min(1, (c / CONTROL - k.a) / k.move)) * ((c / CONTROL - k.a) < k.move);
    for (let c = c0; c < c1; c++) {
      const t = c / CONTROL, u = (t - k.a) / k.move, moving = u < 1, sp = moving ? SPEED[k.shape](u) : 0;
      V[c] = k.peak * sp * (1 + .06 * Math.sin(2 * Math.PI * shake * t + ph));
      P[c] = k.press * (1 + .08 * Math.sin(2 * Math.PI * shake * 1.3 * t + 2 * ph)) * Math.min(1, t / .015);
      D[c] = k.d0 + (k.d1 - k.d0) * gone / sum; gone += sp;
    }
  }
  const res = [...SHAPES.map((_, j) => ({ ratio: MEMBRANE[j][0], amp: MEMBRANE[j][1], tau: ring / (1 + 1.3 * j), kind: "head", j })),
    ...RIM.map((ratio, j) => ({ ratio, amp: .5 / (1 + .3 * j), tau: .03, kind: "rim" })), ...METAL.map((ratio, j) => ({ ratio, amp: .4 / (1 + .2 * j), tau: .1, kind: "metal" }))]
    .filter((r) => hz * r.ratio < .45 * SR).map((r) => ({ ...r, y1: 0, y2: 0, c1: 0, c2: 0, g: 0 }));
  const up = 1 - Math.exp(-1 / (.0025 * SR)), down = 1 - Math.exp(-1 / (.006 * SR)), dt = 1 / SR, slips = [];
  let Vs = 0, Ns = 0, Vt = 0, Nt = 0, vt = 0, x = 0, stretch = 0, stuck = true, dc = 0, a1 = 0, a2 = 0, low = 0, creak = 0;
  for (let i = 0; i < y.length; i++) {
    if (i % HOLD === 0) {
      const c = Math.floor(i / HOLD), on = c < C && i < n, d = D[Math.min(c, C - 1)], press = Ns / NEWTONS;
      Vt = on ? V[c] * METRES : 0; Nt = on ? P[c] * NEWTONS : 0;
      const skin = 1 - smooth(.62, .7, d), metal = smooth(.88, .965, d), r = Math.min(1, d / .66);
      for (const q of res) {
        const tau = q.kind === "head" ? q.tau / (1 + 80 * press) : q.tau / (1 + 10 * press), R = Math.exp(-1 / (tau * SR));
        q.c1 = 2 * R * Math.cos(2 * Math.PI * hz * q.ratio * (1 + (q.kind === "head" ? .035 : 0) * press) / SR); q.c2 = R * R;
        q.g = (1 - R) * q.amp * (q.kind === "head" ? (.15 + .85 * skin) * Math.abs(bessel(SHAPES[q.j][0], SHAPES[q.j][1] * r)) / SHAPES[q.j][2] : q.kind === "rim" ? 1 - skin - metal : metal);
      }
    }
    Vs += (Vt > Vs ? up : down) * (Vt - Vs); Ns += (Nt > Ns ? up : down) * (Nt - Ns);
    // the tip: stuck until the spring outpulls the standing grip, then sliding until it stops
    const rough = grain(x), pull = TIP.spring * stretch + TIP.damp * (Vs - vt);
    let rubbed = pull;
    if (stuck && Math.abs(pull) > tip.muS * Ns * rough) { stuck = false; if (Vs > .002 && i < n) slips.push(i); }
    if (!stuck) {
      const way = vt !== 0 ? Math.sign(vt) : Math.sign(pull);
      rubbed = Ns * rough * (tip.muK + (tip.muS - tip.muK) * Math.exp(-Math.abs(vt) / TIP.slipSpeed)) * way;
      const next = vt + (pull - rubbed) / TIP.mass * dt;
      if ((vt !== 0 && Math.sign(next) !== Math.sign(vt)) || Ns < 1e-4) { vt = 0; stuck = Ns >= 1e-4; } else vt = next;
    }
    x += vt * dt; stretch += (Vs - vt) * dt;
    if (Ns < 1e-4) stretch *= .99;                                 // lifted: the tip springs back
    dc += .002 * (rubbed - dc);
    creak += .12 * (rubbed - dc - creak);                          // the pad of the finger is soft: its catches are thumps, not clicks (about 1 kHz)
    // the bumps it passes
    const tipSpeed = Math.abs(vt), soften = 1 - Math.exp(-2 * Math.PI * (tip.soft + tip.bright * Math.min(1, tipSpeed / .3)) / SR);
    const bump = surface() < tip.perMetre * tipSpeed * dt ? -Math.log(surface() + 1e-6) * 3 * Math.sqrt(Ns) * (surface() < .5 ? 1 : -1) : 0;
    a1 += soften * (bump - a1); a2 += soften * (a1 - a2);
    low += (1 - Math.exp(-2 * Math.PI * tip.cut / SR)) * (a2 - low);
    const bumps = tip.cut ? a2 - low : a2, force = (nail ? .25 : .5) * creak + 5 * bumps;
    let out = 2.4 * bumps;                                          // the finger itself, through the air
    for (const q of res) { const v = q.c1 * q.y1 - q.c2 * q.y2 + q.g * force; q.y2 = q.y1; q.y1 = v; out += v; }
    y[i] = out;
  }
  return { y, i0: Math.round(start * SR), n, strokes, V, slips, nail };
}
// a small FFT, for measuring: the centre of a 512-sample frame's spectrum, in Hz
function centroid(y, a) {
  const N2 = 512, re = new Float64Array(N2), im = new Float64Array(N2);
  for (let i = 0; i < N2; i++) { let j = 0; for (let b = 0; b < 9; b++) j |= ((i >> b) & 1) << (8 - b); re[j] = (y[a + i] ?? 0) * (.5 - .5 * Math.cos(2 * Math.PI * i / N2)); }
  for (let len = 2; len <= N2; len <<= 1) for (let i = 0; i < N2; i += len) for (let k = 0; k < len / 2; k++) {
    const w = -2 * Math.PI * k / len, cr = Math.cos(w), ci = Math.sin(w), p = i + k, q = p + len / 2, tr = re[q] * cr - im[q] * ci, ti = re[q] * ci + im[q] * cr;
    re[q] = re[p] - tr; im[q] = im[p] - ti; re[p] += tr; im[p] += ti;
  }
  let top = 0, all = 0;
  for (let k = 1; k < N2 / 2; k++) { const mag = Math.hypot(re[k], im[k]); top += k * mag; all += mag; }
  return all > 1e-9 ? top / all * SR / N2 : 0;
}
const rms = (y, a, z) => { let e = 0; for (let i = a; i < z; i++) e += y[i] * y[i]; return Math.sqrt(e / Math.max(1, z - a)); };
const pearson = (xs, ys) => { const mx = xs.reduce((a, b) => a + b, 0) / xs.length, my = ys.reduce((a, b) => a + b, 0) / ys.length; let sxy = 0, sx = 0, sy = 0; xs.forEach((v, i) => { sxy += (v - mx) * (ys[i] - my); sx += (v - mx) ** 2; sy += (ys[i] - my) ** 2; }); return sxy / Math.sqrt(sx * sy + 1e-18); };
const median = (v) => v.length ? [...v].sort((a, b) => a - b)[v.length >> 1] : null;
// The slow drags of the quiet sections (the ones sent to the cathedral) are exposed, and sit 4 dB forward of
// the scratches in the drops. `flat` is the same bus without that, to set the level by and to measure against.
const rubbed = { tracks: [], turns: [], gaps: [], frames: [] }, flat = stereo(), EXPOSED = 10 ** (4 / 20);
for (const g of scrapes.filter((g) => !inGap(g.step))) {
  const { y, i0, n, strokes, V, slips } = rub(g), dry = g.orbit ? earMoving(bus.scrape) : earAt(bus.scrape, g.pan), plain = g.orbit ? earMoving(flat) : earAt(flat, g.pan), wet = g.nave ? (g.orbit ? earMoving(nave.scrape) : earAt(nave.scrape, g.pan)) : null;
  let dull = 0;
  for (let i = 0; i < y.length && i0 + i < N; i++) {
    let out = y[i];
    const [pan, behind] = g.orbit ? round(2 * Math.PI * g.orbit * Math.min(1, i / n)) : [g.pan, 0];
    if (g.orbit) { dull += (1 - .75 * behind) * (out - dull); out = dull * (1 - .3 * behind); }
    dry(i0 + i, g.nave ? out * EXPOSED : out, pan); plain(i0 + i, out, pan);
    if (wet) wet(i0 + i, out * g.nave, pan);
  }
  // does brightness follow the finger? frame by frame within each stroke long enough to tell
  for (const k of strokes) {
    const a = Math.round(k.a * SR), z = Math.round((k.a + k.move) * SR), bright = [], fast = [], loud = rms(y, a, z);
    for (let f = a; f + 512 <= z; f += 256) if (rms(y, f, f + 512) > .05 * loud) { bright.push(centroid(y, f)); fast.push(V[Math.min(V.length - 1, Math.round((f + 256) / HOLD))]); }
    if (bright.length >= 6) rubbed.tracks.push(pearson(fast, bright));
    bright.forEach((hz, j) => rubbed.frames.push([fast[j], hz]));
    // and is it quiet where the finger stands still between strokes?
    if (k.dur - k.move > .008 && k !== strokes.at(-1)) rubbed.turns.push(20 * Math.log10((rms(y, z + Math.round(.6 * (k.dur - k.move) * SR), Math.round((k.a + k.dur) * SR)) + 1e-9) / (loud + 1e-9)));
  }
  for (let j = 1; j < slips.length; j++) rubbed.gaps.push((slips[j] - slips[j - 1]) / SR * 1000);
  g.slips = slips.length;
}
// a short room round the rubbed drums: three reflections off near walls, 6 to 14 ms, nothing longer
for (const [ms, gain, cross] of [[6.1, .25, false], [9.7, .18, true], [14.3, .12, false]]) {
  const d = Math.round(ms * SR / 1000), { L: sl, R: sr } = bus.scrape;
  for (let i = N - 1; i >= d; i--) { const l = sl[i - d], r = sr[i - d]; sl[i] += gain * (cross ? r : l); sr[i] += gain * (cross ? l : r); }
}
rubbed.frames.sort((a, b) => a[0] - b[0]);
const third = (k) => { const part = rubbed.frames.slice(Math.floor(k * rubbed.frames.length / 3), Math.floor((k + 1) * rubbed.frames.length / 3)); return { speed: +(part.reduce((a, f) => a + f[0], 0) / part.length).toFixed(2), centroidHz: Math.round(part.reduce((a, f) => a + f[1], 0) / part.length) }; };
const gapMean = rubbed.gaps.reduce((a, b) => a + b, 0) / Math.max(1, rubbed.gaps.length);
const scrapeCheck = {
  gestures: scrapes.length, strokes: scrapes.reduce((a, g) => a + g.strokes.length, 0),
  brightnessFollowsSpeed: { frames: rubbed.frames.length, slowThird: third(0), middleThird: third(1), fastThird: third(2), correlationOverAllFrames: +pearson(rubbed.frames.map((f) => f[0]), rubbed.frames.map((f) => f[1])).toFixed(2), strokesMeasured: rubbed.tracks.length, medianCorrelation: +median(rubbed.tracks)?.toFixed(2), shareOver0_5: +(rubbed.tracks.filter((r) => r > .5).length / Math.max(1, rubbed.tracks.length)).toFixed(2) },
  atTheTurns: { turnsMeasured: rubbed.turns.length, medianDbUnderTheStroke: +median(rubbed.turns)?.toFixed(1), shareAtLeast12DbUnder: +(rubbed.turns.filter((v) => v <= -12).length / Math.max(1, rubbed.turns.length)).toFixed(2), worstDb: +Math.max(...rubbed.turns).toFixed(1) },
  stickSlip: { catches: rubbed.gaps.length + scrapes.filter((g) => g.slips).length, medianGapMs: +median(rubbed.gaps)?.toFixed(2), meanGapMs: +gapMean.toFixed(2),
    spread: +(Math.sqrt(rubbed.gaps.reduce((a, b) => a + (b - gapMean) ** 2, 0) / Math.max(1, rubbed.gaps.length)) / gapMean).toFixed(2), p10Ms: +[...rubbed.gaps].sort((a, b) => a - b)[Math.floor(rubbed.gaps.length * .1)]?.toFixed(2), p90Ms: +[...rubbed.gaps].sort((a, b) => a - b)[Math.floor(rubbed.gaps.length * .9)]?.toFixed(2) },
};

// ── the song: a head bowed with two fingers ───────────────────────────
// The same friction, held still and steady so it sustains, and here kept clean: the grip's noise is heard
// only slowly. Smoothed to about 30 Hz it leans on the tone's loudness (a bow's unevenness, a few per cent)
// and lets the pitch wander by a few cents; at audio rate it would be hiss on every note, and was. What
// is left of the noise itself is a breath 34 dB under the tone. The head's second and third modes sound
// with the first. Two fingers, 8 cents apart, sit left and right. A note swells in over 35 ms, wavers a
// little after a third of a second, and lets go in 0.2 s.
{
  const white = rng(0x50f6);
  for (const e of songs) {
    const t0 = timeAt(e.step), n = Math.round((timeAt(e.step + e.len) - t0) * SR), i0 = Math.round(t0 * SR), hz = 440 * 2 ** ((e.midi - 69) / 12);
    e.time = +t0.toFixed(5);
    for (const [cents, pan] of [[-4, -.5], [4, .5]]) {
      const f = hz * 2 ** (cents / 1200), ear = earAt(bus.song, pan), fa = 1 - Math.exp(-2 * Math.PI * Math.min(2600, 2.4 * f) / SR), fs = 1 - Math.exp(-2 * Math.PI * .4 * f / SR);
      let level = 0, n1 = 0, n2 = 0, ph = 0, grip = .4, drift = 0;
      for (let i = 0; i < n + .5 * SR && i0 + i < N; i++) {
        const t = i / SR, want = i < n ? e.vel : 0;
        level += (want > level ? 1 - Math.exp(-1 / (.035 * SR)) : 1 - Math.exp(-1 / (.2 * SR))) * (want - level);
        const w = white() * 2 - 1;
        n1 += fa * (w - n1); n2 += fs * (w - n2);
        const band = n1 - n2, gnarl = Math.tanh(band * 9);
        grip += .004 * (Math.abs(gnarl) - grip); drift += .002 * (Math.tanh(8 * band) - drift);
        ph += f * (1 + .02 * drift + (t > .33 ? .004 * Math.min(1, (t - .33) / .4) * Math.sin(2 * Math.PI * 5 * t) : 0)) / SR;
        const head = Math.sin(2 * Math.PI * ph) + .34 * Math.sin(2 * Math.PI * 1.59 * ph) + .16 * Math.sin(2 * Math.PI * 2.14 * ph);
        ear(i0 + i, level * (.02 * gnarl + head * (.7 + .75 * grip)));
      }
    }
  }
}

// ── the donk ──────────────────────────────────────────────────────────
// The bass as a small hard drum. Its pitch starts 2.4 times high and is home in 11 ms (the blip); its body is
// a sine whose phase is pushed by a sine an octave up, hard at first and easing in 30 ms, which hollows it
// (the octave push feeds the odd harmonics, like a square wave); it is gone in a tenth of a second. Mono.
for (const e of donks) {
  const t0 = timeAt(e.step), i0 = Math.round(t0 * SR), hz = 440 * 2 ** ((e.midi - 69) / 12);
  e.time = +t0.toFixed(5);
  for (let i = 0, ph = 0; i < .16 * SR && i0 + i < N; i++) {
    const t = i / SR; ph += 2 * Math.PI * hz * (1 + 1.4 * Math.exp(-t / .011)) / SR;
    const v = .707 * e.vel * Math.min(1, i / 60) * Math.exp(-t / .055) * Math.sin(ph + (2.4 * Math.exp(-t / .03) + .5) * Math.sin(2 * ph));
    bus.donk.L[i0 + i] += v; bus.donk.R[i0 + i] += v;
  }
}
// ── the wobble ────────────────────────────────────────────────────────
// One long bass note: a floor tom rolled so fast it is a tone (a saw and a square at its pitch) through a
// low-pass that opens from 110 Hz to 2 kHz and closes again in time with the bar, over a sine that does not
// move. How fast it opens is the gearbox: one wobble every half bar in first gear, then half-bar triplets,
// quarters, quarter triplets, eighths and, in sixth, eighth triplets. It is locked to the ride's own steps,
// so it slows as the bike slows. Mono.
const WOBBLE = [8, 16 / 3, 4, 8 / 3, 2, 4 / 3];
for (const e of wobs) {
  const t0 = timeAt(e.step), i0 = Math.round(t0 * SR), n = Math.round((timeAt(e.step + e.len) - t0) * SR), hz = 440 * 2 ** ((e.midi - 69) / 12);
  e.time = +t0.toFixed(5); e.every = WOBBLE[RIDE.gear[Math.min(STEPS - 1, Math.floor(e.step))]];
  for (let i = 0, j = Math.floor(e.step), ph = 0, lo = 0, bp = 0; i < n + .05 * SR && i0 + i < N; i++) {
    const t = t0 + i / SR;
    while (j < RIDE.T.length - 2 && RIDE.T[j + 1] <= t) j++;
    const pos = j + (t - RIDE.T[j]) / (RIDE.T[j + 1] - RIDE.T[j]), per = WOBBLE[RIDE.gear[Math.min(STEPS - 1, j)]], open = .5 - .5 * Math.cos(2 * Math.PI * (pos % per) / per);
    ph += hz / SR; if (ph >= 1) ph -= 1;
    const f = 2 * Math.sin(Math.PI * 110 * 2 ** (4.2 * open) / SR), hp = .6 * (2 * ph - 1) + (ph < .5 ? .4 : -.4) - lo - .45 * bp;
    bp += f * hp; lo += f * bp;
    const v = .707 * e.vel * Math.min(1, i / 240) * (i < n ? 1 : Math.exp(-(i - n) / (.012 * SR))) * (.8 * lo * (.55 + .45 * open) + .75 * Math.sin(2 * Math.PI * ph));
    bus.wob.L[i0 + i] += v; bus.wob.R[i0 + i] += v;
  }
}
// ── the reversed tails ────────────────────────────────────────────────
// Into each section and into the last chord: the chord struck on four ringing toms, put through a room of
// its own (4 s), and only the room kept, backwards, so 2.6 s of reverberation gathers and stops on the downbeat.
for (const step of swells) {
  const c = step >= END ? CH.F : chordAt(step), n = Math.round(2.6 * SR), dry = { L: new Float32Array(n), R: new Float32Array(n) };
  [0, 7, 12, 19].forEach((up, j) => { const y = tom("ring", 53 + low(c) + up, 1, 1.2); for (let i = 0; i < y.length && i < n; i++) { dry.L[i] += y[i] * (j % 2 ? .4 : 1); dry.R[i] += y[i] * (j % 2 ? 1 : .4); } });
  const wet = hall(dry.L, dry.R, { rt60: 4, damp: 2600, size: 2.2 }), end = Math.round(timeAt(step) * SR);
  let top = 1e-9; for (let i = 0; i < n; i++) top = Math.max(top, Math.abs(wet.L[i]), Math.abs(wet.R[i]));
  for (let i = 0; i < n; i++) { const k = end - 1 - i, g = .9 / top * Math.min(1, (n - i) / (.5 * SR), i / 200); if (k >= 0 && k < N) { bus.rev.L[k] += g * wet.L[i]; bus.rev.R[k] += g * wet.R[i]; } }
}
// the bass gets out of the kick's way; the same curve, deeper and slower, will hold the cathedral down
const ducked = (depth, tau, backbeat = false) => {
  const d = new Float32Array(N).fill(1);
  for (const e of events.filter((e) => e.bus === "kick" || (backbeat && e.bus === "snare" && e.kind === "rim" && e.vel >= .9))) { const i0 = Math.round(e.time * SR); for (let i = 0; i < 5 * tau * SR && i0 + i < N; i++) d[i0 + i] = Math.min(d[i0 + i], 1 - depth * Math.exp(-i / (tau * SR))); }
  return d;
};
{ const duck = ducked(.55, .045); for (const b of [bus.bass, bus.wob]) for (let i = 0; i < N; i++) { b.L[i] *= duck[i]; b.R[i] *= duck[i]; } }
const dB = (v) => +(20 * Math.log10(v + 1e-12)).toFixed(1);
function loudPart(L, R, a = 0, z = N) {
  const B = 2400, blocks = [];
  for (let s = a; s + B <= z; s += B) { let e = 0; for (let i = s; i < s + B; i++) e += L[i] * L[i] + R[i] * R[i]; blocks.push(e / B); }
  const top = Math.max(...blocks), on = blocks.filter((b) => b > top * .01);
  return Math.sqrt(on.reduce((x, y) => x + y, 0) / on.length);
}
const L = new Float32Array(N), R = new Float32Array(N), send = stereo(), far = stereo(), levels = {}, gains = {};
for (const [k, b] of Object.entries(bus)) {
  if (BUS[k].drive) { const a = BUS[k].drive / loudPart(b.L, b.R); for (let i = 0; i < N; i++) { b.L[i] = Math.tanh(b.L[i] * a) / a; b.R[i] = Math.tanh(b.R[i] * a) / a; } }
  const g = 10 ** (BUS[k].level / 20) / (k === "scrape" ? loudPart(flat.L, flat.R) : loudPart(b.L, b.R)), all = BUS[k].nave ?? 0;
  gains[k] = g;
  levels[k] = { strokes: k === "scrape" ? scrapes.length : k === "song" ? songs.length : k === "donk" ? donks.length : k === "wob" ? wobs.length : events.filter((e) => e.bus === k).length, gainDb: dB(g), loudRms: BUS[k].level };
  for (let i = 0; i < N; i++) {
    const l = b.L[i] * g, r = b.R[i] * g;
    L[i] += l; R[i] += r; send.L[i] += l * BUS[k].send; send.R[i] += r * BUS[k].send;
    far.L[i] += l * all + nave[k].L[i] * g; far.R[i] += r * all + nave[k].R[i] * g;
  }
}
// eight delay lines through a Hadamard matrix, darker as they ring (from gundrum-and-bass-2); size stretches the room
function hall(inL, inR, { rt60, damp, size = 1 }) {
  const lens = [1931, 2213, 2617, 2903, 3329, 3761, 4127, 4549].map((n) => Math.round(n * size)), line = lens.map((n) => new Float32Array(n)), at = new Int32Array(8), lp = new Float64Array(8), h = new Float64Array(8);
  const fb = lens.map((n) => 10 ** (-3 * n / SR / rt60)), a = Math.exp(-2 * Math.PI * damp / SR), o = { L: new Float32Array(inL.length), R: new Float32Array(inL.length) };
  for (let i = 0; i < inL.length; i++) {
    for (let k = 0; k < 8; k++) { lp[k] = (1 - a) * line[k][at[k]] + a * lp[k]; h[k] = lp[k]; }
    o.L[i] = (h[0] - h[2] + h[4] - h[6]) * .5; o.R[i] = (h[1] - h[3] + h[5] - h[7]) * .5;
    for (let s = 1; s < 8; s <<= 1) for (let k = 0; k < 8; k++) if (!(k & s)) { const x = h[k], y = h[k + s]; h[k] = x + y; h[k + s] = x - y; }
    for (let k = 0; k < 8; k++) { line[k][at[k]] = h[k] * fb[k] / Math.sqrt(8) + (k & 1 ? inR[i] : inL[i]) * .3; if (++at[k] === lens[k]) at[k] = 0; }
  }
  return o;
}
const space = hall(send.L, send.R, { rt60: 2.2, damp: 5200 });
// the rubbed drums against the snare and the lead, section by section: as they were (the bus at −17.5 dB,
// nothing forward) and as they are
const scrapeVsKit = sections.map((sec) => {
  const a = Math.round(timeAt(sec.start) * SR), z = Math.round(timeAt(sec.start + sec.bars * BAR) * SR), lvl = (b, g) => 20 * Math.log10(loudPart(b.L, b.R, a, z) * g + 1e-12);
  const was = lvl(flat, 10 ** (-17.5 / 20) / loudPart(flat.L, flat.R)), is = lvl(bus.scrape, gains.scrape), snare = lvl(bus.snare, gains.snare), lead = lvl(bus.lead, gains.lead);
  return { section: sec.title, beforeVsSnare: +(was - snare).toFixed(1), afterVsSnare: +(is - snare).toFixed(1), beforeVsLead: +(was - lead).toFixed(1), afterVsLead: +(is - lead).toFixed(1), changeDb: +(is - was).toFixed(1) };
});
// The cathedral: a long stone room, used by moment, and in two depths. In the drops what is sent goes to
// the nave: 60 ms late, five sparse early reflections, 5 s to die away, damped from 2 kHz, held down hard
// under every kick and backbeat so the drops stay tight and dry. In the intro, the breakdown and after the
// last chord it goes instead to the deep end: 110 ms late (the dry hit is clear in front of it), the
// reflections spread 1.6 times wider, the network stretched 3.4 times, 12 s to die away and damped from
// 1.1 kHz, so the tail is dark and its lows bloom and outlast everything. The two cross over in 0.4 s.
const DEEP = [...sections.filter((sec) => /intro|breakdown/.test(sec.title)).map((sec) => [timeAt(sec.start), timeAt(sec.start + sec.bars * BAR)]), [timeAt(END), Infinity]];
const depth = Float32Array.from({ length: N }, (_, i) => Math.max(...DEEP.map(([a, z]) => Math.max(0, Math.min(1, (i / SR - a + .2) / .4, (z + .2 - i / SR) / .4)))));
const TAPS = [[0, 1, 1], [.023, .5, 0], [.041, 0, .5], [.067, .4, .2], [.103, .2, .4], [.13, .3, .3]];
function room({ deep, pre, spread, rt60, damp, size, hold }) {
  const inp = stereo();
  for (const [sec, gl, gr] of TAPS) { const d = Math.round((pre + sec * spread) * SR); for (let i = d; i < N; i++) { const w = deep ? depth[i - d] : 1 - depth[i - d]; inp.L[i] += gl * w * far.L[i - d]; inp.R[i] += gr * w * far.R[i - d]; } }
  const o = hall(inp.L, inp.R, { rt60, damp, size });
  for (let i = 0; i < N; i++) { o.L[i] = (o.L[i] + .25 * inp.L[i]) * hold[i]; o.R[i] = (o.R[i] + .25 * inp.R[i]) * hold[i]; }
  return o;
}
const SPACE = { nave: { deep: false, pre: .06, spread: 1, rt60: 5, damp: 2000, size: 2.2, gain: 1 }, deep: { deep: true, pre: .11, spread: 1.6, rt60: 12, damp: 1100, size: 3.4, gain: 1.5 } };
const naveRoom = room({ ...SPACE.nave, hold: ducked(.75, .13, true) }), deepRoom = room({ ...SPACE.deep, hold: ducked(.5, .2, true) });
const cathedral = { L: Float32Array.from(naveRoom.L, (v, i) => v + SPACE.deep.gain * deepRoom.L[i]), R: Float32Array.from(naveRoom.R, (v, i) => v + SPACE.deep.gain * deepRoom.R[i]) };
// how long the deep end really takes: the slope of its level over the six seconds after the last chord has rung in
const deepDecay = (() => {
  const a = Math.round((timeAt(END) + 2) * SR), pts = [];
  for (let k = 0; k < 12; k++) { let e = 0; for (let i = a + k * SR / 2; i < a + (k + 1) * SR / 2 && i < N; i++) e += deepRoom.L[i] ** 2 + deepRoom.R[i] ** 2; pts.push([k / 2, 10 * Math.log10(e + 1e-20)]); }
  const mx = pts.reduce((s2, q) => s2 + q[0], 0) / pts.length, my = pts.reduce((s2, q) => s2 + q[1], 0) / pts.length;
  return +(-60 / (pts.reduce((s2, q) => s2 + (q[0] - mx) * (q[1] - my), 0) / pts.reduce((s2, q) => s2 + (q[0] - mx) ** 2, 0))).toFixed(1);
})();

// ── master: drive into a soft knee, then a look-ahead limiter trimmed toward the target ──
const soft = (v, knee = .5) => { const a = Math.abs(v); return a < knee ? v : Math.sign(v) * (knee + (1 - knee) * Math.tanh((a - knee) / (1 - knee))); };
const DRIVE = Number(process.env.TOMBOMB_DRIVE ?? 1.45), TARGET = Number(process.env.TOMBOMB_LUFS ?? -9.5), NAVE = Number(process.env.TOMBOMB_NAVE ?? 1.4), CEIL = 10 ** (-1.5 / 20), LOOK = 240;
const preL = new Float32Array(N), preR = new Float32Array(N);
for (let i = 0; i < N; i++) { preL[i] = soft(L[i] * DRIVE) + space.L[i] + NAVE * cathedral.L[i]; preR[i] = soft(R[i] * DRIVE) + space.R[i] + NAVE * cathedral.R[i]; }
// ── where the noise is (measured, since nothing is heard) ────────────
// Each source alone, as it sits in the mix, over the drops and over the quiet sections: its level, how much
// of its energy is above 4 kHz and in 2–6 kHz, how much it adds to the mix up there in absolute terms, and
// its spectral flatness between 150 Hz and 16 kHz (0 a pure tone, 1 white noise). Then the mix before and
// after the drive, to see what the soft knee adds.
function spectrum(get, a, n = 2048) {
  const re = new Float64Array(n), im = new Float64Array(n), bits = Math.log2(n);
  for (let i = 0; i < n; i++) { let j = 0; for (let b = 0; b < bits; b++) j |= ((i >> b) & 1) << (bits - 1 - b); re[j] = get(a + i) * (.5 - .5 * Math.cos(2 * Math.PI * i / n)); }
  for (let len = 2; len <= n; len <<= 1) for (let i = 0; i < n; i += len) for (let k = 0; k < len / 2; k++) {
    const w = -2 * Math.PI * k / len, cr = Math.cos(w), ci = Math.sin(w), u = i + k, v = u + len / 2, tr = re[v] * cr - im[v] * ci, ti = re[v] * ci + im[v] * cr;
    re[v] = re[u] - tr; im[v] = im[u] - ti; re[u] += tr; im[u] += ti;
  }
  return Float64Array.from({ length: n / 2 }, (_, k) => (re[k] * re[k] + im[k] * im[k]) / n);
}
function probe(get, spans) {
  const hzPer = SR / 2048, bin = (hz) => Math.round(hz / hzPer);
  let all = 0, top = 0, mid = 0, air = 0, flatSum = 0, weight = 0, frames = 0;
  for (const [a, z] of spans) for (let f = a; f + 2048 <= z; f += 6144) {
    const p = spectrum(get, f);
    let e = 0, logs = 0, lin = 0;
    for (let k = 1; k < 1024; k++) { e += p[k]; if (k >= bin(4000)) top += p[k]; if (k >= bin(2000) && k < bin(6000)) mid += p[k]; if (k >= bin(8000)) air += p[k]; }
    for (let k = bin(150); k < bin(16000); k++) { logs += Math.log(p[k] + 1e-20); lin += p[k]; }
    const count = bin(16000) - bin(150);
    all += e; frames++;
    if (lin > 1e-12) { flatSum += Math.exp(logs / count) / (lin / count) * e; weight += e; }
  }
  const lvl = (v) => +(10 * Math.log10(v / Math.max(1, frames) + 1e-20)).toFixed(1);
  return { level: lvl(all), over4k: lvl(top), band2to6k: lvl(mid), over8k: lvl(air), shareOver4k: +(10 * Math.log10(top / (all + 1e-20) + 1e-12)).toFixed(1), flatness: +(flatSum / (weight + 1e-20)).toFixed(3) };
}
const spansOf = (titles) => sections.filter((sec) => titles.some((t) => sec.title.includes(t))).map((sec) => [Math.round(timeAt(sec.start) * SR), Math.round(timeAt(sec.start + sec.bars * BAR) * SR)]);
const noiseReport = Object.fromEntries([["drops", ["iii", "vi ·", "vii"]], ["quiet", ["i · intro", "v · breakdown"]]].map(([label, titles]) => {
  const spans = spansOf(titles), rows = {};
  for (const [k, b] of Object.entries(bus)) rows[k] = probe((i) => .5 * (b.L[i] + b.R[i]) * gains[k], spans);
  rows.hall = probe((i) => .5 * (space.L[i] + space.R[i]), spans);
  rows.cathedral = probe((i) => .5 * NAVE * (cathedral.L[i] + cathedral.R[i]), spans);
  rows.nave = probe((i) => .5 * NAVE * (naveRoom.L[i] + naveRoom.R[i]), spans);
  rows.deep = probe((i) => .5 * NAVE * SPACE.deep.gain * (deepRoom.L[i] + deepRoom.R[i]), spans);
  rows.mixBeforeDrive = probe((i) => .5 * DRIVE * (L[i] + R[i]), spans);
  rows.mixAfterDrive = probe((i) => .5 * (soft(L[i] * DRIVE) + soft(R[i] * DRIVE)), spans);
  rows.master = probe((i) => .5 * (preL[i] + preR[i]), spans);
  let over = 0, on = 0;
  for (const [a, z] of spans) for (let i = a; i < z; i++) { const v = Math.max(Math.abs(L[i]), Math.abs(R[i])) * DRIVE; if (v > .01) on++; if (v > .5) over++; }
  rows.shareOfSamplesInTheKnee = +(over / on).toFixed(3);
  return [label, rows];
}));
// the new voices, section by section: how many, and how loud against the kick and the snare
const added = sections.map((sec) => {
  const z0 = sec.start + sec.bars * BAR, a = Math.round(timeAt(sec.start) * SR), z = Math.round(timeAt(z0) * SR), within = (st) => st >= sec.start && st < z0;
  const lvl = (k) => { const v = loudPart(bus[k].L, bus[k].R, a, z) * gains[k]; return v > 1e-6 ? 20 * Math.log10(v) : null; }, kick = lvl("kick"), snare = lvl("snare");
  const versus = (k) => { const v = lvl(k); return v == null ? null : { vsKick: kick == null ? null : +(v - kick).toFixed(1), vsSnare: snare == null ? null : +(v - snare).toFixed(1) }; };
  return { section: sec.title, reversedStrokes: events.filter((e) => e.rev && within(e.step - .001)).length, reversedTails: swells.filter((st) => within(st - .001)).length, donks: donks.filter((e) => within(e.step)).length,
    wobbles: wobs.filter((e) => within(e.step)).length, wobbleEverySteps: [...new Set(wobs.filter((e) => within(e.step)).map((e) => +e.every.toFixed(2)))], powerChords: events.filter((e) => e.bus === "riff" && within(e.step)).length / 3,
    far: events.filter((e) => e.dist && within(e.step)).length, riff: versus("riff"), donk: versus("donk"), wobble: versus("wob"), reversos: versus("rev") };
});
// peaks between samples count too
const SINC = [.25, .5, .75].map((f) => Array.from({ length: 16 }, (_, k) => { const x = k - 7 - f; return Math.sin(Math.PI * x) / (Math.PI * x) * (.5 + .5 * Math.cos(Math.PI * x / 8)); }));
const between = (x, i) => {
  let p = Math.abs(x[i]);
  if (p < .3 || i < 8 || i > N - 9) return p;
  for (const taps of SINC) { let v = 0; for (let k = 0; k < 16; k++) v += x[i - 7 + k] * taps[k]; p = Math.max(p, Math.abs(v)); }
  return p;
};
const need = new Float32Array(N), ahead = new Float32Array(N), gr = new Float32Array(N), fadeN = Math.round(2.5 * SR);
function limit(makeup) {
  for (let i = 0; i < N; i++) { L[i] = preL[i] * makeup; R[i] = preR[i] * makeup; }
  for (let i = 0, g = 1; i < N; i++) {
    const p = Math.max(between(L, i), between(R, i), i ? between(L, i - 1) : 0, i ? between(R, i - 1) : 0);
    g = Math.min(p > CEIL ? CEIL / p : 1, g + (1 - g) * .0004);
    need[i] = g;
  }
  for (let i = 0; i < N; i++) { let m = 1; for (let j = i, e = Math.min(N, i + LOOK); j < e; j++) if (need[j] < m) m = need[j]; ahead[i] = m; }
  for (let i = 0, sum = 0; i < N; i++) {
    sum += ahead[i] - (i >= LOOK ? ahead[i - LOOK] : 0);
    gr[i] = sum / Math.min(LOOK, i + 1);
    const out = gr[i] * (i > N - fadeN ? ((N - i) / fadeN) ** 2 : 1);
    L[i] *= out; R[i] *= out;
  }
}
mkdirSync(OUT, { recursive: true });
const WAV = resolve(OUT, "tombomb.wav"), MP3 = resolve(OUT, "tombomb.mp3");
function writeWav() {
  const wav = Buffer.alloc(44 + N * 6);
  wav.write("RIFF", 0); wav.writeUInt32LE(36 + N * 6, 4); wav.write("WAVEfmt ", 8); wav.writeUInt32LE(16, 16);
  wav.writeUInt16LE(1, 20); wav.writeUInt16LE(2, 22); wav.writeUInt32LE(SR, 24); wav.writeUInt32LE(SR * 6, 28);
  wav.writeUInt16LE(6, 32); wav.writeUInt16LE(24, 34); wav.write("data", 36); wav.writeUInt32LE(N * 6, 40);
  for (let i = 0; i < N; i++) {
    wav.writeIntLE(Math.round(Math.max(-1, Math.min(1, L[i])) * 8388607), 44 + i * 6, 3);
    wav.writeIntLE(Math.round(Math.max(-1, Math.min(1, R[i])) * 8388607), 47 + i * 6, 3);
  }
  writeFileSync(WAV, wav);
}
const loudness = (file) => {
  const log = spawnSync("ffmpeg", ["-nostats", "-i", file, "-af", "ebur128=peak=true", "-f", "null", "-"]).stderr.toString();
  const tail = log.slice(log.lastIndexOf("Summary:")), num = (re) => Number(tail.match(re)?.[1]);
  return { lufs: num(/I:\s+(-?[\d.]+) LUFS/), lra: num(/LRA:\s+(-?[\d.]+) LU/), truePeak: num(/Peak:\s+(-?[\d.]+) dBFS/) };
};
const passes = [];
let makeup = 1;
for (let pass = 0; pass < 6; pass++) {
  limit(makeup); writeWav();
  const got = loudness(WAV);
  passes.push({ makeupDb: dB(makeup), ...got });
  if (Math.abs(got.lufs - TARGET) < .15) break;
  makeup *= 10 ** (1.5 * (TARGET - got.lufs) / 20);   // the limiter gives back about a third of each step
}
execFileSync("ffmpeg", ["-v", "error", "-y", "-i", WAV, "-af", "volume=-1dB", "-c:a", "libmp3lame", "-b:a", "320k",
  "-metadata", "title=tombomb", "-metadata", "artist=Aesthetic Dot Computer", MP3]);

// ── measure (nothing is played) ───────────────────────────────────────
let bad = 0, side = 0, mid = 0;
for (let i = 0; i < N; i++) { if (!Number.isFinite(L[i]) || !Number.isFinite(R[i])) bad++; mid += (L[i] + R[i]) ** 2; side += (L[i] - R[i]) ** 2; }
const reduced = Array.from(gr.subarray(0, Math.round(timeAt(END) * SR))).map((g) => -20 * Math.log10(g)).sort((a, b) => a - b);
// the top line against the bass: how many of its notes begin on a consonance with the chord's root
const heard = songs.filter((e) => e.step < END).map((e) => ({ ...e, over: e.midi - (F + chordAt(e.step).root) }));
const strong = heard.filter((e) => e.step % 8 === 0), clash = strong.filter((e) => !isConsonant(e.over));
const report = {
  title: "tombomb", key: "F minor", seconds: +(N / SR).toFixed(2), strokes: events.length, distinctToms: toms.size, scrapeCount: scrapes.length, sungNotes: songs.length, notFinite: bad, sideToMidDb: dB(Math.sqrt(side / mid)),
  sections: sections.map((s) => {
    const end = s.start + s.bars * BAR, a = Math.round(timeAt(s.start) * SR), z = Math.round(timeAt(end) * SR), mine = perBar.slice(s.start / BAR, end / BAR);
    return { title: s.title, from: +timeAt(s.start).toFixed(1), bars: s.bars, chords: s.chords, bpm: `${Math.min(...mine.map((b) => b.bpm))}–${Math.max(...mine.map((b) => b.bpm))}`, gears: [...new Set(mine.map((b) => b.gear))].join(" "),
      grade: [...new Set(mine.map((b) => `${b.grade} %`))].join(", "), ground: [...new Set(mine.map((b) => b.ground))].join(", "), throttle: [...new Set(mine.map((b) => (b.hot ? "hot: " : "") + b.throttle))].join(", "),
      rmsDb: dB(loudPart(L, R, a, z) / Math.SQRT2), what: s.what };
  }),
  ride: { bike: BIKE, course: COURSE.map(([from, to, grade, ground, throttle]) => ({ bars: `${from + 1}–${to}`, grade, ground, throttle })), bars: perBar, shifts: RIDE.shifts, jumps: RIDE.jumps,
    overheated: perBar.filter((b, i) => b.hot && !perBar[i - 1]?.hot).map((b) => b.bar), cooled: perBar.filter((b, i) => !b.hot && perBar[i - 1]?.hot).map((b) => b.bar) },
  spins, tunes, scrapeCheck, scrapeVsKit, noise: noiseReport, added,
  space: { hall: { rt60: 2.2 }, nave: { ...SPACE.nave, where: "the drops and everything not listed under deep" }, deep: { ...SPACE.deep, measuredRt60: deepDecay, where: "i · intro, v · breakdown (with the coasting bars 79–80), the last chord and the brakes" },
    gaps: GAPS.map(([a, z]) => ({ bar: Math.floor(a / BAR) + 1, beats: (z - a) / 4, seconds: +(timeAt(z) - timeAt(a)).toFixed(2) })) },
  topLine: { strongBeats: strong.length, consonantWithTheBass: strong.length - clash.length, leaning: clash.map((e) => `${name(e.midi)} over ${chordAt(e.step).say}, bar ${Math.floor(e.step / BAR) + 1}`) },
  cathedral: { sentWholeBus: Object.entries(BUS).filter(([, b]) => b.nave).map(([k]) => k), strokesSent: events.filter((e) => e.nave).length, scrapesSent: scrapes.filter((g) => g.nave).length },
  kinds: Object.fromEntries(Object.entries(KIND).map(([k, v]) => {
    const ms = events.filter((e) => e.kind === k).map((e) => e.midi);
    return [k, { drum: v.say, strokes: ms.length, range: `${name(Math.min(...ms))}–${name(Math.max(...ms))}` }];
  })),
  levels, limiter: { drive: DRIVE, makeupDb: dB(makeup), passes, reductionDb: { mean: +(reduced.reduce((a, b) => a + b, 0) / reduced.length).toFixed(2), p95: +reduced[Math.floor(reduced.length * .95)].toFixed(2), max: +reduced.at(-1).toFixed(2) } },
  loudness: { wav: loudness(WAV), mp3: loudness(MP3) },
};
writeFileSync(resolve(OUT, "tombomb.json"), JSON.stringify({ ...report, events, scrapes, songs, donks, wobs }, null, 1));
const { ride: _, kinds, levels: __, ...short } = report;
console.log(JSON.stringify(short, null, 1));
