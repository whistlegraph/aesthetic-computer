#!/usr/bin/env node
// Note(s)pat(ial) Native — the 13-minute ring form for six laptops.
//
// Grows the September 18 rehearsal material (the walking sine, the soft
// 3-against-2 swing, the octave climb, the D-major theme) into ONE score
// the spatial-rehearsal piece plays start to finish: no conductor moves
// mid-piece, one cue. Seats 1–6 stand in a ring around the audience,
// seat 1 at the front, numbers increasing clockwise.
//
// Spatial writing uses two kinds of lane. Pinned seat lanes (az) put a
// note entirely inside ONE laptop, so a "walk" is a discrete hop from
// machine to machine with no two-seat crossfade — robust to clock skew.
// Orbiting lanes (orbitSeconds) glide continuously and hand off with
// equal power. Every lane sets one or the other, so the runtime's
// default drift (0.15 rad/s when lanes > 1) never applies.
//
//   node fedac/native/tools/compose-notespatial-native.mjs
//     → scores/notespatial-native.nsscore            (the piece)
//     → scores/notespatial-native-<n>-<name>.nsscore (each section, rebased to 0)

import { mkdir, writeFile } from 'node:fs/promises';

const SEATS = 6, TAU = Math.PI * 2;
const bpm = 112, beat = 60 / bpm, cycle = 2 * beat; // cycle = one 3-against-2 group
const hz = n => 440 * 2 ** ((n - 69) / 12);
const r4 = x => +x.toFixed(4);
const seatAz = k => (k % SEATS) / SEATS * TAU;

// ── lanes ─────────────────────────────────────────────────────────────
const lanes = [];
const lane = (name, color, spatial) => (lanes.push({ name, color, ...spatial, events: [] }), lanes.length - 1);
const VOICE = Array.from({ length: SEATS }, (_, k) => lane(`voice ${k + 1}`, [255, 205, 110], { az: seatAz(k) }));
const ECHO = Array.from({ length: SEATS }, (_, k) => lane(`echo ${k + 1}`, [180, 160, 225], { az: seatAz(k) }));
const TAP_FRONT = lane('tap front', [110, 205, 230], { az: seatAz(0) });
const TAP_BACK = lane('tap back', [110, 205, 230], { az: seatAz(3) });
const BASS = lane('bass', [234, 128, 88], { az: seatAz(3) }); // the floor sits at the back; the kick at the front
const PAD = [0, 1, 2].map(m => {
  const i = lanes.length;
  return lane(`pad ${'abc'[m]}`, [156, 179, 237], { orbitSeconds: 60, azOffset: m * TAU / 3 - i / 24 * TAU });
});
const TOP = lane('top line', [225, 235, 170], { orbitSeconds: 24, orbitDirection: -1 });
const THEME = lane('theme', [255, 194, 72], { orbitSeconds: 12.5 });
const ANSWER = lane('answer', [116, 210, 235], { orbitSeconds: 15, orbitDirection: -1 });
const KICK = lane('kick', [255, 107, 125], { az: seatAz(0) });
const SNARE = lane('snare', [122, 223, 153], { az: seatAz(3) });
const HATS = lane('hats', [230, 233, 241], { orbitSeconds: 5 });
if (lanes.length !== 24) throw Error('lane count changed; pad azOffset assumes 24');
for (const l of lanes) if (!(Number.isFinite(l.az) || l.orbitSeconds > 0)) throw Error(`${l.name}: neither pinned nor orbiting`);

function note(i, t, dur, freq, g, wave = 'sine', attack = .01, decay = .06) {
  if (!(dur > 0) || !(g > 0)) return;
  lanes[i].events.push({ t: r4(t), dur: r4(dur), hz: +freq.toFixed(2), g: +g.toFixed(3), wave, attack, decay });
}
// Angle of an orbiting lane at time t (mirrors lib/spatial-rehearsal.mjs voicePosition).
function orbitAngle(i, t) {
  const l = lanes[i];
  return i / lanes.length * TAU + (l.azOffset || 0) + t / l.orbitSeconds * TAU * (l.orbitDirection === -1 ? -1 : 1);
}
const seatOf = angle => ((Math.round(angle / TAU * SEATS) % SEATS) + SEATS) % SEATS;

// ── material ──────────────────────────────────────────────────────────
const CHORDS = [[72, 76, 79], [69, 72, 76], [65, 69, 72], [67, 71, 74]]; // C · Am · F · G, four cycles each
const ARP = [60, 64, 67, 72, 76, 79, 84, 88];
const swing = u => u <= .5 ? u * 1.2 : .6 + (u - .5) * .8; // 60:40
// The D-major theme (compose-spatial-groove.mjs), phrases of 8 beats.
const THEME_HARMONY = [[62, 66, 69], [59, 62, 66], [55, 59, 62], [57, 61, 64]];
const PHRASES = [
  [[0, 1, 74], [1, .5, 78], [1.5, .5, 81], [2, 1.5, 78], [3.5, .5, 76], [4, 1, 78], [5, 1, 76], [6, 2, 74]],
  [[0, 1, 78], [1, .5, 81], [1.5, .5, 83], [2, 1, 81], [3, 1, 78], [4, 1.5, 76], [5.5, .5, 74], [6, 2, 71]],
  [[0, 1, 74], [1, 1, 79], [2, .5, 81], [2.5, .5, 83], [3, 1, 81], [4, 1, 79], [5, .5, 78], [5.5, .5, 76], [6, 2, 74]],
  [[0, 1, 76], [1, .5, 78], [1.5, .5, 81], [2, 1.5, 85], [3.5, .5, 81], [4, 1, 78], [5, 1, 76], [6, 2, 74]],
];

const movements = [];
const movement = (name, sub, t0, t1, level) => movements.push({ name, sub, t0: r4(t0), t1: r4(t1), level });

// ── I · Appear (rubato) ───────────────────────────────────────────────
// One sine at the front laptop breathes, then walks the ring clockwise,
// one note per machine, each lap a little faster. Ends back at seat 1.
let t = 0;
for (const [pitch, dur, gap] of [[60, 3.2, 2.6], [60, 3.2, 2.2], [67, 3.4, 1.8], [64, 3.0, 1.6], [60, 3.6, 2.0]]) {
  note(VOICE[0], t, dur, hz(pitch), .9, 'sine', .35, .9);
  t += dur + gap;
}
let walk = 0;
const LAPS = 10;
for (let lap = 0; lap < LAPS; lap++) {
  const step = 1.55 - 1.05 * (lap / (LAPS - 1)) ** 1.4; // 1.55 s → 0.50 s per seat
  for (let k = 0; k < SEATS; k++) {
    note(VOICE[k], t, step * .92, hz(ARP[walk % ARP.length]), .8, 'sine', .012, Math.min(.08, step * .13));
    walk++; t += step;
  }
  t += step * .8; // breath before the next lap starts at the front
}
note(VOICE[0], t, 2.4, hz(60), .85, 'sine', .02, .4);
const T0 = t + 2.6; // the grid begins here
movement('I · Appear', 'one sine breathes at the front, then walks the ring', 0, T0, .3);

// ── the grid: II–VI ───────────────────────────────────────────────────
const S = { II: [0, 80], III: [80, 192], IV: [192, 320], V: [320, 448], VI: [448, 576] };
const at = c => T0 + c * cycle;
const inS = (c, k) => c >= S[k][0] && c < S[k][1];
const frac = (c, k) => (c - S[k][0]) / (S[k][1] - S[k][0]);
const lerp = (a, b, u) => a + (b - a) * Math.max(0, Math.min(1, u));

let walkIndex = 0;
for (let c = S.II[0]; c < S.VI[1]; c++) {
  const inD = inS(c, 'V'); // the lift: everything up a whole step
  const tr = inD ? 2 : 0;
  const chord = CHORDS[Math.floor(c / 4) % 4].map(n => n + tr);
  const t0 = at(c);

  // section-shaped controls
  let lvl = 1, register = 0, echoes = 0, melody = true, taps = true, bass = false, pads = false, top = false;
  let seatsAllowed = SEATS; // converge: how many front seats the walk may use
  if (inS(c, 'II')) {
    const u = frac(c, 'II');
    lvl = lerp(.55, .8, u);
    melody = c >= S.II[0] + 16; // taps alone for the first 16 cycles
    bass = c >= S.II[0] + 48;
  } else if (inS(c, 'III')) {
    const u = frac(c, 'III');
    lvl = lerp(.8, .9, u);
    bass = true; pads = c >= S.III[0] + 32;
    echoes = c < S.III[0] + 32 ? 1 : c < S.III[0] + 72 ? 2 : 3;
  } else if (inS(c, 'IV')) {
    const u = frac(c, 'IV');
    register = Math.floor((c - S.IV[0]) / 32) - 1; // -1, 0, +1, +2
    lvl = lerp(.9, 1, u);
    bass = true; pads = true; echoes = 3; top = register >= 1;
  } else if (inD) {
    lvl = 1; register = -1; bass = true; pads = true; echoes = 3; taps = false;
  } else if (inS(c, 'VI')) {
    const u = frac(c, 'VI');
    const q = c - S.VI[0];
    lvl = lerp(.85, .45, u);
    echoes = q < 32 ? 3 : q < 64 ? 2 : q < 96 ? 1 : 0;
    bass = q < 64; pads = q < 96; taps = q < 80;
    register = q >= 64 ? -1 : 0;
    seatsAllowed = q < 96 ? 6 : q < 112 ? 3 : 1;
  }
  const soft = .86 ** Math.max(0, register);

  // the 3 — chord tones with swing, each note hopping to the next laptop
  if (melody) {
    const n = inS(c, 'VI') && c >= S.VI[1] - 16 ? 1 : 3; // the last 16 cycles: one note per cycle
    for (let j = 0; j < n; j++) {
      const tn = t0 + swing(j / 3) * cycle;
      const seat = seatsAllowed === 6 ? walkIndex % 6 : seatsAllowed === 3 ? [5, 0, 1][walkIndex % 3] : 0;
      const f = hz(chord[j] + register * 12);
      const g = .27 * soft * lvl * (n === 1 ? 1.4 : 1);
      note(VOICE[seat], tn, .64, f, g, 'sine', .055, .43);
      // echoes answer from other laptops: +2 seats, opposite, +1 seat
      const plan = [[cycle * .375, 2, .42], [cycle * .75, 3, .22], [cycle * 1.5, 1, .11]];
      for (let e = 0; e < echoes; e++) {
        const [d, hop, lv] = plan[e];
        note(ECHO[(seat + hop) % 6], tn + d, .72, f, g * lv, 'sine', .07, .56);
      }
      walkIndex++;
    }
  }

  // the 2 — soft taps, front then back
  if (taps) {
    const tapLvl = inS(c, 'II') ? lerp(.3, 1, frac(c, 'II') * 2.5) : inS(c, 'VI') ? lerp(1, .4, frac(c, 'VI')) : 1;
    for (let j = 0; j < 2; j++) {
      const tn = t0 + swing(j / 2) * cycle, L = j === 0 ? TAP_FRONT : TAP_BACK;
      note(L, tn, .27, j === 0 ? 130 : 330, (j === 0 ? .12 : .085) * tapLvl * lvl, 'sine', .018, .21);
      note(L, tn + .022, .31, j === 0 ? 90 : 220, .055 * tapLvl * lvl, 'sine', .025, .25);
    }
  }

  // the floor — root in triangle, back laptop
  if (bass) {
    const root = chord[0] - 24; // two octaves under the chord
    if (inD) for (const b of [0, 1.5]) note(BASS, t0 + b * beat, .42 * beat, hz(root), .55 * lvl, 'triangle', .008, .06);
    else note(BASS, t0, cycle * .8, hz(root), .34 * lvl, 'triangle', .02, .3);
  }

  // pads — the triad revolving slowly around the room, one note per 4 cycles
  if (pads && c % 4 === 0) {
    const padLvl = inS(c, 'VI') ? lerp(1, .2, (c - S.VI[0]) / 96) : inS(c, 'III') ? lerp(.4, 1, (c - S.III[0] - 32) / 32) : 1;
    chord.forEach((p, m) => note(PAD[m], t0, cycle * 3.8, hz(p - 12), .11 * padLvl * lvl, 'sine', .5, .9));
  }

  // top line — little answering phrases, gliding counter-clockwise
  if (top && c % 2 === 1) {
    const tn = t0 + .18 * cycle, high = Math.min(106, chord[1] + register * 12 + 12);
    note(TOP, tn, .40, hz(high), .055 * soft * lvl, 'sine', .065, .28);
    note(TOP, tn + cycle * .23, .52, hz(Math.min(108, high + 2)), .04 * soft * lvl, 'sine', .08, .39);
    note(TOP, tn + cycle * .76, .62, hz(high), .016 * soft * lvl, 'sine', .09, .48);
  }

  // hats — from the top register of the climb onward, orbiting fast
  if ((inS(c, 'IV') && register >= 2) || (inD && c - S.V[0] >= 64 && c < S.V[1] - 8)) {
    for (let b = 0; b < 2; b += .5) {
      const off = b % 1 ? .045 : .027;
      note(HATS, t0 + b * beat, off, 7000, (b % 1 ? .30 : .19) * (inD ? 1 : .6) * lvl, 'noise', .001, off * .7);
    }
  }
}

// ── V · Lift — the D-major theme over the floor ──────────────────────
// Two statements of 32 bars (a bar = two cycles). The second adds the
// answer and the hats; bars 16–24 of it lift the phrase tails an octave.
for (let bar = 0; bar < 64; bar++) {
  const c = S.V[0] + bar * 2, b0 = at(c), h = THEME_HARMONY[Math.floor(bar / 2) % 4];
  const statement = Math.floor(bar / 32), sb = bar % 32;
  if (bar % 2 === 0) {
    const phrase = PHRASES[Math.floor(sb / 2) % 4];
    for (const [a, d, p] of phrase) {
      const lift = statement === 1 && sb >= 16 && sb < 24 && a >= 4 ? 12 : 0;
      const tn = b0 + a * beat;
      note(THEME, tn, d * beat * .86, hz(p + lift), .62, 'sine', .008, .06);
      if (statement === 1 && sb >= 8 && a >= 4) note(ANSWER, tn + .5 * beat, d * beat * .6, hz(p - 12), .26, 'triangle', .008, .06);
      // the theme's echoes answer from two laptops on from wherever it is
      if (a === 0 || a === 4) {
        const s = seatOf(orbitAngle(THEME, tn));
        note(ECHO[(s + 2) % 6], tn + cycle * .375, .6, hz(p + lift), .16, 'sine', .07, .5);
        note(ECHO[(s + 3) % 6], tn + cycle * .75, .6, hz(p + lift), .08, 'sine', .07, .5);
      }
    }
  }
  // kick on the floor (front), snare across the room (back)
  for (const a of [0, 2, ...(bar % 4 === 3 ? [3.5] : [])]) {
    note(KICK, b0 + a * beat, .055, 150, .8, 'sine', .001, .04);
    note(KICK, b0 + a * beat + .06, .16, 78, .75, 'sine', .001, .11);
  }
  if (bar >= 8 && bar < 62) for (const a of [1, 3]) {
    note(SNARE, b0 + a * beat, .11, 2400, .4, 'noise', .001, .08);
    note(SNARE, b0 + a * beat, .095, 185, .45, 'triangle', .001, .07);
  }
}

// ── VII · Vanish (rubato) ─────────────────────────────────────────────
// One slow lap the other way, descending, then the front laptop alone.
t = at(S.VI[1]) + .6;
const DESC = [88, 84, 79, 76, 72, 67];
for (let k = 0; k < SEATS; k++) {
  const seat = (SEATS - k) % SEATS; // 1, 6, 5, 4, 3, 2
  note(VOICE[seat], t, 2.6, hz(DESC[k]), .7, 'sine', .3, .8);
  t += 3.4 + k * .45;
}
for (const [pitch, dur, gap] of [[64, 3.4, 3.0], [60, 3.8, 4.2], [60, 4.4, 5.6]]) {
  note(VOICE[0], t, dur, hz(pitch), .8, 'sine', .4, 1.0);
  t += dur + gap;
}
note(VOICE[0], t, 11, hz(48), .75, 'sine', 3.5, 3.0); // C3, rising out of nothing, then gone
const END = t + 11 + 4;

movement('II · Ring', 'the pulse arrives front and back; the walk becomes a swing', at(S.II[0]), at(S.II[1]), .55);
movement('III · Echoes', 'every note is answered from across the room', at(S.III[0]), at(S.III[1]), .75);
movement('IV · Climb', 'four registers, an octave apart; the top line glides against the walk', at(S.IV[0]), at(S.IV[1]), .9);
movement('V · Lift', 'up a whole step: the theme orbits over the floor', at(S.V[0]), at(S.V[1]), 1);
movement('VI · Return', 'back home; the echoes thin, the ring narrows to the front', at(S.VI[0]), at(S.VI[1]), .6);
movement('VII · Vanish', 'one slow lap the other way, then one laptop', at(S.VI[1]), END, .3);

for (const l of lanes) l.events.sort((a, b) => a.t - b.t);
const score = {
  name: 'Note(s)pat(ial) Native', bpm, geometry: 'ring', seats: SEATS, dur: r4(END), gain: .36, swing: .6,
  movements, lanes,
};

// ── write ─────────────────────────────────────────────────────────────
const dir = new URL('../scores/', import.meta.url);
await mkdir(dir, { recursive: true });
await writeFile(new URL('notespatial-native.nsscore', dir), JSON.stringify(score) + '\n');
const slug = s => s.toLowerCase().replace(/^[ivx]+ · /, '').replace(/[^a-z]+/g, '-').replace(/^-|-$/g, '');
movements.forEach((m, n) => {
  const part = {
    ...score, name: `${score.name} — ${m.name}`, dur: r4(m.t1 - m.t0 + 2), movements: [{ ...m, t0: 0, t1: r4(m.t1 - m.t0) }],
    lanes: lanes.map(l => ({ ...l, events: l.events.filter(e => e.t >= m.t0 && e.t < m.t1).map(e => ({ ...e, t: r4(e.t - m.t0) })) })),
  };
  return writeFile(new URL(`notespatial-native-${n + 1}-${slug(m.name)}.nsscore`, dir), JSON.stringify(part) + '\n');
});

// ── report ────────────────────────────────────────────────────────────
const mmss = s => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}`;
const all = lanes.flatMap((l, i) => l.events.map(e => ({ ...e, lane: i }))).sort((a, b) => a.t - b.t);
function maxVoices(a, b) {
  let mx = 0;
  const ev = all.filter(e => e.t >= a && e.t < b);
  for (const e of ev) { const n = ev.filter(o => o.t <= e.t && o.t + o.dur > e.t).length; if (n > mx) mx = n; }
  return mx;
}
console.log(`${score.name}: ${mmss(END)} (${END.toFixed(1)} s), ${bpm} BPM, ${lanes.length} lanes, ${all.length} events, ring of ${SEATS}`);
console.log('section        start   length  events  maxvoices');
for (const m of movements) {
  const ev = all.filter(e => e.t >= m.t0 && e.t < m.t1);
  console.log(`${m.name.padEnd(14)} ${mmss(m.t0).padStart(6)}  ${mmss(m.t1 - m.t0).padStart(6)}  ${String(ev.length).padStart(6)}  ${String(maxVoices(m.t0, m.t1)).padStart(9)}`);
}
