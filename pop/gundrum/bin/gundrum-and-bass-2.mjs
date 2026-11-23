#!/usr/bin/env node
// gundrum-and-bass-2.mjs — version 2 of the drum-and-bass suite.
//
// The same five movements and score as gundrum-and-bass.mjs (left untouched
// so version 1 renders exactly as it did), plus four layers:
//   1. high sines: a pure-sine descant three to four octaves over the sub,
//      and soft sine chord tones in the passacaglia and stretto;
//   2. a microtiming and dynamics layer kept apart from the score (rhythm-
//      platter digest 07): the grid says which pulse, an offset table says
//      where in the bin. Kick, snare and sub stay on the grid;
//   3. FEM bells from pop/bell: a bronze church bell as a gong on movement
//      downbeats and drops, bronze handbells doubling the subject's head at
//      cadences, glass on the last chord;
//   4. a spatial mix: every gun voice at its own azimuth and distance, and
//      explosions far off or crossing the field into a long hall.
// Bells, sines, booms and the hall come in after the drive, so they never crunch.
//
//   node pop/gundrum/bin/gundrum-and-bass-2.mjs   → out/gundrum-and-bass-2.{wav,mp3,json} + -timeline.png

import { writeFileSync, mkdirSync, existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync, execFileSync } from "node:child_process";
import { isConsonant, generateFirstSpecies, checkFirstSpecies } from "../../lib/counterpoint.mjs";
import { rhythm, complement, interlocks, bjorklund, rotate, metricWeights, syncopationLhl, toBox } from "../../lib/necklace.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
const GUNS = resolve(LANE, "../wattajetta/assets/guns");
const PY = resolve(LANE, "../.venv/bin/python");
const BELL = resolve(LANE, "../bell/c/bell"), BELLS = resolve(OUT, ".bell-cache");

const SR = 48_000, BPM = 172, STEP = 60 / BPM / 4, TAIL = 6.5;

// ── the key and the subject (as in gundrum.mjs) ───────────────────────
const TONIC = 62, MINOR = [0, 2, 3, 5, 7, 8, 10];
const mod = (a, n) => ((a % n) + n) % n;
const midiOf = (d) => TONIC + 12 * Math.floor(d / 7) + MINOR[mod(d, 7)];
const degOf = (m) => 7 * Math.floor((m - TONIC) / 12) + MINOR.indexOf(mod(m - TONIC, 12));
const NAMES = ["C", "C#", "D", "Eb", "E", "F", "F#", "G", "G#", "A", "Bb", "B"];
const name = (m) => NAMES[mod(m, 12)] + (Math.floor(m / 12) - 1);
const line = (rows) => rows.map(([t, d, len]) => ({ t, d, len }));
const span = (l) => Math.max(...l.map((n) => n.t + n.len));

const SUBJECT = line([
  [0, 0, 3], [3, 4, 3], [6, 2, 2], [8, 1, 2], [10, 3, 2], [12, 2, 2], [14, 1, 1], [15, 0, 1],
  [16, 1, 1], [17, 2, 1], [18, 3, 1], [19, 4, 1], [20, 5, 2], [22, 4, 2], [24, 3, 2], [26, 1, 2], [28, 2, 4],
]);
const LEN = span(SUBJECT);

function transform(l, { delay = 0, interval = 0, invert = false, retrograde = false, scale = 1 } = {}) {
  const end = span(l);
  return l
    .map((n) => ({ t: (retrograde ? end - n.t - n.len : n.t) * scale + delay, d: (invert ? 4 - n.d : n.d) + interval, len: n.len * scale }))
    .sort((a, b) => a.t - b.t);
}
function tonalAnswer(subject) {
  let head = true;
  return subject.map((n) => {
    const dominant = mod(n.d, 7) === 4, d = n.d + (head && dominant ? 3 : 4);
    if (dominant) head = false;
    return { ...n, d };
  });
}
const skeleton = (l, beats = Math.ceil(span(l) / 4)) =>
  Array.from({ length: beats }, (_, b) => { const n = l.findLast((n) => n.t <= b * 4 && b * 4 < n.t + n.len); return n ? midiOf(n.d) : null; });
const fit = (a, b) => {
  const A = skeleton(a, 40), B = skeleton(b, 40);
  let good = 0, of = 0;
  A.forEach((m, i) => { if (m != null && B[i] != null) { of++; good += isConsonant(m - B[i]); } });
  return `${good}/${of}`;
};
const up = (l, interval, more = {}) => transform(l, { interval, ...more });

const ANSWER = tonalAnswer(SUBJECT);
// first species over a cantus, trying bands until counterpoint.mjs finds a line
function species(cantus, bands) {
  for (const [lo, hi] of bands) {
    const sk = generateFirstSpecies(cantus, { tonic: TONIC % 12, scale: MINOR, minAbove: lo, maxAbove: hi });
    if (sk) return { sk, lo, hi, ok: checkFirstSpecies(cantus, sk).ok };
  }
  return null;
}
const CS = species(skeleton(ANSWER), [[8, 12], [7, 12], [5, 12], [3, 12]]);
const HOLES = (() => { const kept = []; for (const p of complement(rhythm(SUBJECT.map((n) => n.t), LEN)).onsets) if (kept.at(-1) !== p - 1) kept.push(p); return kept; })();
const COUNTER = HOLES.map((t, i) => ({ t, d: degOf(CS.sk[Math.floor(t / 4)]), len: (HOLES[i + 1] ?? LEN) - t }));
const HEAD = SUBJECT.filter((n) => n.t < 8);
const RUN = SUBJECT.filter((n) => n.t >= 16 && n.t < 20).map((n) => ({ ...n, t: n.t - 16 }));
const TUNE = SUBJECT.map((n) => n.d);            // the subject as a bare row of pitches

// ── the kit ───────────────────────────────────────────────────────────
// home: the pitch a sample plays at untransposed. level: where its bus is
// set after rendering (dB, loud-part RMS), so no voice sits under the rest.
// pan is azimuth (−1 left … 1 right), dist is metres from the listener.
const KIT = {
  S: { guns: ["sniper-shot-01"], takes: true, home: 60, hold: .2, fat: 2.5, level: -17, pan: -.68, dist: 4.5, wet: .1, label: "soprano · .30-06" },
  A: { guns: ["pistol-9mm-shot-01"], takes: true, home: 64, hold: .14, fat: 3, level: -19, pan: .5, dist: 1.4, wet: .1, label: "alto · 9mm pistol" },
  T: { guns: ["ak47-shot-01"], takes: true, home: 57, hold: .17, fat: 3, level: -19, pan: -.3, dist: 2.6, wet: .1, label: "tenor · AK-47" },
  snare: { guns: ["shotgun-pump-shot-01"], takes: true, home: 55, hold: .19, fat: 3.5, level: -16.5, pan: 0, dist: 1.6, wet: .14, highpass: 170, label: "snare · 12 gauge" },
  ghost: { guns: ["smg-9mm-shot-01"], takes: true, home: 60, hold: .09, fat: 2.5, level: -23, pan: .25, dist: 2.2, wet: .12, label: "ghosts and rolls · 9mm SMG" },
  kick: { guns: ["pistol-heavy-shot-03"], takes: true, home: 60, hold: .17, fat: 2.5, level: -15.5, pan: 0, dist: 0, wet: 0, lowpass: 750, label: "kick · 1911 .45" },
  hat: { guns: ["pistol-9mm-dry-fire-01", "rifle-dry-fire-01", "pistol-9mm-dry-fire-02"], home: 60, hold: .05, fat: 1.5, level: -27, pan: .72, dist: 1.2, wet: .04, label: "hats · dry fire" },
  shot: { guns: ["shotgun-pump-shot-01"], takes: true, home: 55, hold: .7, fat: 3, level: -19, pan: 0, dist: 2.5, wet: .2, label: "shotgun · 12 gauge" },
  orn: { guns: [], home: 60, hold: 1, fat: 1.5, level: -27, pan: 0, dist: 3, wet: .2, label: "racks, magazines, brass, bursts" },
};
// the three dry fires stand in three places: right, left, half right
const HAT_PAN = { "pistol-9mm-dry-fire-01": .72, "rifle-dry-fire-01": -.6, "pistol-9mm-dry-fire-02": .3 };
// with the upright carrying the line, the sine sub stays only as weight under it
const SUB_LEVEL = process.env.GUNDRUM_SYNTH_BASS ? -15.5 : -22;

const peak = (x, a = 0, b = x.length) => { let p = 0; for (let i = a; i < b; i++) p = Math.max(p, Math.abs(x[i])); return p; };
const firstOver = (x, level, a = 0, b = x.length) => { for (let i = a; i < b; i++) if (Math.abs(x[i]) > level) return i; return -1; };
const decode = (gun) => {
  const file = execFileSync("sh", ["-c", `ls "${GUNS}/${gun}".*`]).toString().trim().split("\n")[0];
  const raw = execFileSync("ffmpeg", ["-v", "error", "-i", file, "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
  return new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2).slice();
};
const sliced = new Map();
function slices(gun, takes) {
  if (sliced.has(gun)) return sliced.get(gun);
  const x = decode(gun), pk = peak(x), starts = [];
  if (takes) { for (let i = 0, last = -SR; i < x.length; i++) if (Math.abs(x[i]) > .3 * pk) { if (i - last > .25 * SR) starts.push(i); last = i; } }
  else starts.push(firstOver(x, .12 * pk));
  const out = starts.map((s) => {
    const a = Math.max(0, s - 960), local = peak(x, a, Math.min(x.length, s + 2400));
    const on = takes ? firstOver(x, .12 * local, a) : s;
    return x.subarray(Math.max(0, on - 96), Math.min(x.length, on + 3 * SR));
  });
  sliced.set(gun, out);
  return out;
}
function lowpass(x, hz) {
  const w = 2 * Math.PI * hz / SR, al = Math.sin(w) / Math.SQRT2, c = Math.cos(w), a0 = 1 + al;
  const b0 = (1 - c) / 2 / a0, b1 = (1 - c) / a0, a1 = -2 * c / a0, a2 = (1 - al) / a0;
  const y = Float32Array.from(x);
  for (let pass = 0; pass < 2; pass++) {
    let x1 = 0, x2 = 0, y1 = 0, y2 = 0;
    for (let i = 0; i < y.length; i++) { const v = b0 * y[i] + b1 * x1 + b0 * x2 - a1 * y1 - a2 * y2; x2 = x1; x1 = y[i]; y2 = y1; y1 = v; y[i] = v; }
  }
  return y;
}
const hits = new Map();
function hit(gun, { semis, hold, take, takes, lp, hp, fat }) {
  const key = [gun, semis, Math.round(hold * 1000), take].join(":");
  if (hits.has(key)) return hits.get(key);
  const all = slices(gun, takes);
  let x = all[take % all.length];
  const rate = 2 ** (semis / 12);
  if (rate > 1) x = lowpass(x.subarray(0, Math.min(x.length, Math.ceil((hold + .01) * SR * rate) + 64)), .42 * SR / rate);
  const n = Math.min(Math.floor((x.length - 2) / rate), Math.round(hold * SR));
  const y = new Float32Array(n);
  for (let i = 0; i < n; i++) { const p = i * rate, j = p | 0, f = p - j; y[i] = x[j] * (1 - f) + x[j + 1] * f; }
  if (lp) { const low = lowpass(y, lp); for (let i = 0; i < n; i++) y[i] = low[i] + .1 * y[i]; }
  if (hp) { const low = lowpass(y, hp); for (let i = 0; i < n; i++) y[i] -= low[i]; }
  const fade = Math.min(n, Math.round(Math.max(.012, hold * .45) * SR));
  for (let i = 0; i < fade; i++) y[n - 1 - i] *= (i / fade) ** 2;
  const pk = peak(y) + 1e-9, sat = Math.tanh(fat);
  for (let i = 0; i < n; i++) y[i] = Math.tanh(y[i] / pk * fat) / sat;
  const made = { y, lead: Math.max(0, firstOver(y, .12)) };
  hits.set(key, made);
  return made;
}

// ── the score ─────────────────────────────────────────────────────────
const events = [], movements = [], bars = [];
let NOW, POS = 0;
function movement(title, count, { per = 16, device, cross }) {
  NOW = { title, start: POS, bars: count, per, device, cross, entries: [] };
  for (let b = 0; b < count; b++) bars.push(POS + b * per);
  movements.push(NOW);
  const here = NOW;
  POS += count * per;
  return (bar, step = 0) => here.start + (bar - 1) * per + step;
}
const put = (voice, step, midi, len, vel, form, more = {}) => events.push({ voice, step, midi, len, vel, form, mvt: NOW.title, ...more });
const accent = (t) => (t % 4 === 0 ? 1 : t % 2 === 0 ? .82 : t % 1 === 0 ? .66 : .55);

// A line enters. Louder as it climbs: the dynamics follow the contour.
function enter(voice, l, t0, form, { vel = 1, oct = 0, say } = {}) {
  const ds = l.map((n) => n.d), lo = Math.min(...ds), wide = Math.max(1, Math.max(...ds) - lo);
  l.forEach((n, i) => put(voice, t0 + n.t, midiOf(n.d) + oct, n.len, vel * accent(n.t) * (.6 + .65 * (n.d - lo) / wide) * (i ? 1 : 1.15), form));
  NOW.entries.push({ bar: +(1 + (t0 + l[0].t - NOW.start) / NOW.per).toFixed(2), voice, form, on: say ?? name(midiOf(l[0].d) + oct) });
}
const sub = (l, t0, form, more = {}) => enter("sub", l, t0, form, { oct: -24, ...more });
const orn = (gun, step, hold, vel = 1, pan = 0, semis = 0) => put("orn", step, 60 + semis, hold / STEP, vel, "drums", { gun, hold, pan });
// a roll: evenly spaced hits sweeping in pitch and loudness
function roll(voice, t0, count, gap, [m0, m1], [v0, v1]) {
  for (let i = 0; i < count; i++) { const f = count > 1 ? i / (count - 1) : 0; put(voice, t0 + i * gap, Math.round(m0 + (m1 - m0) * f), gap, v0 + (v1 - v0) * f, "drums"); }
}

// ── version 2's layers, written into the same score ───────────────────
// A high sine line: moved by octaves until its middle sits near A5, so it
// rides three to four octaves over the sub whatever line it doubles.
let nLine = 0;
function hi(l, t0, form, { vel = 1, centre = 81, say } = {}) {
  const ms = l.map((n) => midiOf(n.d)), mid = (Math.min(...ms) + Math.max(...ms)) / 2;
  const oct = 12 * Math.round((centre - mid) / 12), id = `hi${nLine++}`;
  l.forEach((n, i) => put("hi", t0 + n.t, ms[i] + oct, n.len, vel * (.8 + .2 * accent(n.t)), form, { line: id }));
  NOW.entries.push({ bar: +(1 + (t0 + l[0].t - NOW.start) / NOW.per).toFixed(2), voice: "hi", form, on: say ?? name(ms[0] + oct) });
}
// soft sustained sine chord tones, one line per chord member
const pad = (step, chord, len, vel = 1) => chord.forEach((m, k) => put("pad", step, m, len, vel, "ground", { line: `pad${k}` }));
// bells: a bronze handbell doubling a head, folded into D4–D5 (higher reads tangy on small speakers)
const fold = (m) => { while (m > 74) m -= 12; while (m < 62) m += 12; return m; };
const bells = (l, t0, vel = 1) => l.forEach((n, i) => put("bell", t0 + n.t, fold(midiOf(n.d)), n.len, vel * (i ? .8 : 1), "bell", { kind: "handbell" }));
// deeper bells (@jeffrey: "make some of the fem bells deeper"): the head on bronze church bells folded
// into D3–D4, an octave and more under the handbells, with a bronze bowl a further octave down on its first note
const deepFold = (m) => { while (m > 62) m -= 12; while (m < 50) m += 12; return m; };
const deep = (l, t0, vel = 1) => l.forEach((n, i) => {
  put("bell", t0 + n.t, deepFold(midiOf(n.d)), n.len, vel * (i ? .85 : 1), "bell", { kind: "deep" });
  if (!i) put("bell", t0 + n.t, deepFold(midiOf(n.d)) - 12, n.len, vel * .7, "bell", { kind: "bowl" });
});
const gong = (step, midi = 38, vel = 1) => put("bell", step, midi, 32, vel, "bell", { kind: "church" });
// an explosion: layered guns and a sub drop, far away, maybe crossing the field
const boom = (step, spec) => put("boom", step, 36, 16, spec.vel ?? 1, "boom", { boom: spec });

// Drum bars as boxes: X accent, x hit, o ghost. Ghosts are pitched off the
// subject, so a bar of them outlines it.
const PATTERN = {
  two: { kick: "X.........X.....", snare: "....X.......X..." },
  chopA: { kick: "X.....o...X.x...", snare: "....X..o.o..X..o" },
  chopB: { kick: "X..x......X..x..", snare: "....X.o...o.X.o." },
  half: { kick: "X.........x.....", snare: "........X......." },
  six: { kick: "X...........X.....o.....", snare: "......X...........X....." },
  three: { kick: "X.......X.......X.......", snare: "....X.......X.......X..." },
};
const tuneAt = (step) => SUBJECT.findLast((n) => n.t <= step % LEN).d;
// kick and snare are retuned hit by hit, on two short cycles of different lengths
let nK = 0, nS = 0;
function drums(t0, pattern, { vel = 1, lift = 0 } = {}) {
  const { kick, snare } = PATTERN[pattern];
  lift += [0, 3, -2, 5, 0, 2, -4][nS++ % 7];
  [...kick].forEach((c, i) => {
    if (c === "X") put("kick", t0 + i, 48 + [0, 0, -3, 0, 2][nK++ % 5], 2, vel, "drums");
    if (c === "x") put("kick", t0 + i, 50, 2, vel * .8, "drums");
    if (c === "o") put("kick", t0 + i, 55, 1, vel * .5, "drums");
  });
  [...snare].forEach((c, i) => {
    if (c === "X") { put("snare", t0 + i, 60 + lift + (i % 8 === 4 ? 0 : 2), 3, vel, "drums"); put("ghost", t0 + i, 62 + lift, 2, vel * .9, "drums", { lock: true }); }
    if (c === "o") put("ghost", t0 + i, midiOf(tuneAt(i) + 4) - 5 + lift, 1, vel * (.28 + .06 * (i % 3)), "drums");
  });
}
// hats on a Euclidean necklace, loud where the metre is strong, three guns in turn
function hats(t0, k, { n = 16, rot = 0, vel = 1, lift = 0 } = {}) {
  const w = metricWeights(n === 24 ? 8 : n);
  bjorklund(k, n, rot).onsets.forEach((p, j) =>
    put("hat", t0 + p, 60 + lift + [0, 7, 3, 10, 5][(p + j) % 5], 1, vel * (.4 + .14 * w[p % w.length]), "drums", { gun: KIT.hat.guns[(p + j) % 3] }));
}

// ════ I · Invention ═══════════════════════════════════════════════════
// Two voices, pistol and sub, trading subject and countersubject.
{
  const at = movement("I · Invention", 18, { device: "two-part invention: imitation at the octave, countersubject in invertible counterpoint, inversion, sequence, stretto", cross: "the subject's 3+3+2 head against the two-step; hats on E(5,16)" });
  enter("A", SUBJECT, at(1), "subject");
  sub(SUBJECT, at(3), "subject");
  enter("A", up(COUNTER, -4), at(3), "countersubject", { say: "above the sub" });
  enter("A", ANSWER, at(5), "subject", { say: "A4, tonal answer" });
  sub(up(COUNTER, -14), at(5), "countersubject", { say: "below" });
  for (const k of [0, 1]) { enter("A", up(HEAD, -k), at(7 + k), "fragment", { say: "head" }); sub(up(HEAD, 3 - k - 7), at(7 + k, 8), "fragment", { say: "head" }); }
  sub(up(SUBJECT, 0, { invert: true }), at(9), "inversion");
  enter("A", up(COUNTER, 11, { invert: true }), at(9), "inversion", { say: "countersubject, mirrored" });
  enter("A", up(SUBJECT, 2), at(11), "subject", { say: "F4, relative major" });
  sub(up(COUNTER, -16), at(11), "countersubject", { say: "below" });
  for (const k of [0, 1]) { enter("A", up(HEAD, k, { invert: true }), at(13 + k), "fragment", { say: "head, mirrored" }); sub(up(HEAD, k), at(13 + k, 8), "fragment", { say: "head" }); }
  sub(SUBJECT, at(15), "subject", { say: "D2, stretto" });
  enter("A", up(SUBJECT, 4), at(15, 12), "subject", { say: "A4, stretto +3 beats" });
  NOW.checks = { stretto: fit(SUBJECT, up(SUBJECT, 4, { delay: 12 })) };

  for (let b = 1; b <= 18; b++) {
    hats(at(b), b < 3 ? 5 : 7, { rot: b % 2, vel: b < 3 ? .8 : 1 });
    if (b >= 3) drums(at(b), b < 9 ? "two" : b % 2 ? "chopA" : "chopB", { vel: b === 18 ? .9 : 1 });
  }
  orn("pistol-9mm-mag-insert-01", at(1, 6), .3, .8, -.4);
  orn("pistol-heavy-slide-rack-01", at(2, 12), .4, 1, .3);
  roll("ghost", at(8, 8), 8, 1, [58, 72], [.25, 1]);
  roll("ghost", at(14, 12), 4, 1, [72, 60], [.9, .4]);
  roll("ghost", at(18, 8), 8, 1, [55, 79], [.3, 1.1]);
  orn("shotgun-pump-rack-01", at(18, 4), .5, 1, -.3);

  // the sine follows the countersubject wherever it goes, then joins the stretto
  hi(COUNTER, at(3), "countersubject", { vel: .85 });
  hi(COUNTER, at(5), "countersubject", { say: "over the sub's" });
  hi(up(COUNTER, 0, { invert: true }), at(9), "inversion");
  hi(COUNTER, at(11), "countersubject");
  hi(up(SUBJECT, 4), at(15, 12), "subject", { say: "doubling the alto's stretto entry" });
  boom(at(3), { pan: .75, dist: 16, layers: ["grenade-explosion-02"], vel: .8 });
  // no bells before 0:30 (@jeffrey 2026-11-22: "the fem bells at the start are too annoying"): the invention's
  // downbeat gong and the handbells on its sequences are out; the first bell is the courante's cadence (about 0:40)
}

// ════ II · Courante ═══════════════════════════════════════════════════
// 6/4. The sub walks two to the bar, the rifle three; the pistol runs
// eighths between them. After four bars the two slow voices change places.
{
  const at = movement("II · Courante", 8, { per: 24, device: "first-species pair (Fux) in voice exchange, a running double above; hemiola at the two cadence bars", cross: "3 against 2 in every bar; drums regroup 6/4 as 3/2 in bars 4 and 8" });
  const cantus = [...skeleton(SUBJECT), ...skeleton(ANSWER)];
  const first = species(cantus.slice(0, 8), [[3, 12], [3, 16], [2, 16]]) ?? { sk: CS.sk.map((m) => midiOf(degOf(m) - 4)), ok: false };
  const counter = [...first.sk, ...CS.sk];
  NOW.checks = { firstSpecies: [first.ok, CS.ok], cantus: cantus.map(name).join(" "), counter: counter.map(name).join(" ") };
  const low = (m) => { while (m > 46) m -= 12; while (m < 31) m += 12; return m; };
  for (let b = 1; b <= 8; b++) {
    const swap = b > 4, hemiola = b % 4 === 0;
    for (const h of [0, 1]) {
      const n = cantus[2 * (b - 1) + h], c = counter[2 * (b - 1) + h];
      // two to the bar
      if (swap) put("T", at(b, 12 * h), n - 12, 10, 1, "ground");
      else put("sub", at(b, 12 * h), low(n), 11, 1, "ground");
      // the running double: counter and cantus tones in turn
      [c, n, c - 12, n, c, n + 12].forEach((m, e) =>
        put("A", at(b, 12 * h + 2 * e), m - (m > 84 ? 12 : 0), 2, (hemiola ? [1, .5, .7, .5][(6 * h + e) % 4] : [1, .45, .7, .45, .7, .45][e]) * (.75 + (m - 60) / 60), "division"));
    }
    // three to the bar
    [0, 1, 2].forEach((g) => {
      const c = counter[2 * (b - 1) + (g === 2 ? 1 : 0)] + (g === 1 ? 12 : 0);
      if (swap) put("sub", at(b, 8 * g), low(c), 7, g ? .85 : 1, "countersubject");
      else put("T", at(b, 8 * g), c - 12, 6, g ? .8 : 1, "countersubject");
    });
    drums(at(b), hemiola ? "three" : "six", { vel: .9 });
    hats(at(b), hemiola ? 12 : 9, { n: 24, vel: .9, lift: 2 });
  }
  NOW.entries.push({ bar: 1, voice: "sub", form: "ground", on: "two to the bar" }, { bar: 1, voice: "T", form: "countersubject", on: "three to the bar" },
    { bar: 1, voice: "A", form: "division", on: "running eighths" }, { bar: 5, voice: "T", form: "ground", on: "voices exchanged" }, { bar: 5, voice: "sub", form: "countersubject", on: "three to the bar" });
  orn("casing-drop-02", at(1, 3), .7, .7, .5);
  orn("smg-9mm-bolt-lock-01", at(4, 20), .35, .9, -.4);
  roll("ghost", at(8, 16), 8, 1, [79, 55], [1, .3]);

  // a slow descant: the counter's tones as half bars, gliding
  hi(counter.map((m, i) => ({ t: 12 * i, d: degOf(m), len: 11.5 })), at(1), "countersubject", { vel: .8, centre: 79, say: "the counter in half bars" });
  // (its downbeat gong is out too: no bells before 0:30, @jeffrey 2026-11-22)
  boom(at(1), { pan: [.85, -.85], travel: 2.6, dist: 12, layers: ["grenade-explosion-01", "distant-shot-01"] });
  bells([{ t: 0, d: degOf(cantus[14]), len: 8 }, { t: 8, d: degOf(counter[14]), len: 8 }, { t: 16, d: degOf(cantus[15]), len: 8 }], at(8), .7);
}

// ════ III · Prolation canon ═══════════════════════════════════════════
// The subject's seventeen pitches as an even row, at four speeds at once.
{
  const at = movement("III · Canon per prolationem", 16, { device: "mensuration canon: one row at 1 : 3 : 4 : 5 notes to the bar, at the octave and the fourth below", cross: "3 against 4 against 5, over a sub one note to the bar" });
  const row = (voice, from, perBar, shift, vel, oct = 0) => {
    const gap = 16 / perBar, count = Math.round((at(17) - at(from)) / gap);
    for (let i = 0; i < count; i++) {
      const d = TUNE[i % TUNE.length] + shift;
      put(voice, at(from) + i * gap, midiOf(d) + oct, gap, vel * (i % perBar ? .62 + .06 * (TUNE[i % TUNE.length]) : 1.1), "canon");
    }
    NOW.entries.push({ bar: from, voice, form: "canon", on: `${perBar} to the bar, from ${name(midiOf(shift) + oct)}` });
  };
  row("A", 1, 4, 0, 1);
  row("T", 3, 3, -3 - 7, 1);
  row("S", 5, 5, 7, .9);
  for (let b = 1; b <= 16; b++) {
    const m = midiOf(TUNE[b - 1]) - 24;
    put("sub", at(b), m, 6, 1, "augmentation"); put("sub", at(b, 6), m > 38 ? m - 12 : m + 12, 3.5, .6, "augmentation"); put("sub", at(b, 10), m, 5.5, .85, "augmentation");
    hats(at(b), b < 9 ? 5 : 9, { rot: b, vel: b < 5 ? .7 : 1, lift: b < 9 ? 0 : 5 });
    if (b < 5) put("kick", at(b), 48, 2, .8, "drums");
    else if (b < 9) drums(at(b), "half", { vel: .7 + .05 * (b - 5) });
    else drums(at(b), b % 2 ? "two" : "chopA");
  }
  NOW.entries.push({ bar: 1, voice: "sub", form: "augmentation", on: "1 to the bar, from D2" });
  roll("ghost", at(8), 8, 2, [55, 67], [.2, .6]); roll("ghost", at(8, 8) + 1, 4, 2, [69, 79], [.6, 1.1]);
  orn("shotgun-pump-rack-01", at(8, 10), .45, 1, .3);
  orn("casing-shotgun-shell-drop-01", at(9, 6), .8, .7, -.5);
  roll("ghost", at(16, 12), 4, 1, [60, 74], [.5, 1.1]);

  // the missing prolation: the row at two to the bar, in sine, from the drop
  hi(Array.from({ length: 16 }, (_, i) => ({ t: 8 * i, d: TUNE[i % TUNE.length] + 7, len: 8 })), at(9), "canon", { say: "2 to the bar, from D5" });
  gong(at(1), 38, .6);
  gong(at(9), 38, 1.1);
  boom(at(9), { pan: -.3, dist: 10, layers: ["grenade-explosion-01", "grenade-explosion-02"], drop: true });
  deep(HEAD, at(9));
}

// ════ IV · Passacaglia ════════════════════════════════════════════════
// A five-bar ground in the sub, six times, a new variation each time.
{
  const at = movement("IV · Passacaglia", 30, { device: "ground bass D C Bb G A; six variations: theme, first species, tresillo and its holes, divisions, the ground carried up to the tenor, all together", cross: "a click every third sixteenth (3 against 4) over a five-bar ground under a two-bar break: it comes round every 240 steps" });
  const GROUND = [0, -1, -2, -4, -3];
  const cantus = GROUND.flatMap((d) => [midiOf(d - 7), midiOf(d - 7)]);
  const near = species(cantus, [[3, 10], [3, 12]]), far = species(cantus, [[12, 19], [10, 19], [8, 17]]);
  const CA = near.sk.map(degOf), CB = far.sk.map(degOf);
  NOW.checks = { ground: cantus.map(name).join(" "), near: near.sk.map(name).join(" "), far: far.sk.map(name).join(" "), firstSpecies: [near.ok, far.ok] };
  const cyc = (v, bar, step = 0) => at((v - 1) * 5 + bar, step);
  const ground = (voice, v, bar, shape, vel = 1) => {
    const m = midiOf(GROUND[bar - 1]) - (voice === "sub" ? 24 : 12);
    for (const [t, len, oct, g] of shape) put(voice, cyc(v, bar, t), m + oct, len, vel * g, "ground");
  };
  const LONG = [[0, 15.5, 0, 1]], WALK = [[0, 6, 0, 1], [6, 3.5, 12, .6], [10, 5.5, 0, .85]];
  const TRES = [[0, 3, 0, 1], [3, 3, 0, .7], [6, 2, 12, .6], [8, 3, 0, .95], [11, 3, 7, .65], [14, 2, 0, .8]];
  const BOUNCE = [[0, 2.5, 0, 1], [3, 2.5, 12, .65], [6, 2, 0, .85], [8, 1.5, 12, .6], [10, 1, 0, .8], [11, 2.5, 7, .7], [14, 2, 12, .75]];
  const halves = (voice, v, C, shift, vel, form) => C.forEach((d, i) => put(voice, cyc(v, 1, 8 * i), midiOf(d + shift), 7, vel * (i % 2 ? .8 : 1), form));
  const tresillo = (v, vel) => CA.forEach((d, i) => [0, 3, 6].forEach((t, j) => put("T", cyc(v, 1, 8 * i + t), midiOf(d + (j === 2 ? 7 : 0)) - 12, j === 2 ? 2 : 3, vel * [1, .7, .85][j], "countersubject")));
  const holes = (v, vel) => CB.forEach((d, i) => [2, 5].forEach((t, j) => put("A", cyc(v, 1, 8 * i + t), midiOf(d - 7 + (j ? 2 : 0)), 2, vel * [.75, .55][j], "countersubject")));
  // divisions: sixteenths walking by step from each far-counter note to the next
  const divisions = (v, vel) => CB.forEach((d, i) => {
    const to = CB[i + 1] ?? CB[0];
    for (let j = 0; j < 8; j++) {
      const walk = Math.round(d + (to - d) * j / 8) + (to === d && j % 2 ? 1 : 0) + (j === 4 && Math.abs(to - d) < 3 ? 2 : 0);
      put("S", cyc(v, 1, 8 * i + j), midiOf(walk), 1, vel * (j ? .5 + .07 * j : 1.1), "division");
    }
  });

  for (let b = 1; b <= 5; b++) {
    ground("sub", 1, b, LONG); put("kick", cyc(1, b), 48, 2, .85, "drums");
    ground("sub", 2, b, WALK); drums(cyc(2, b), "two");
    ground("sub", 3, b, TRES); drums(cyc(3, b), b % 2 ? "chopA" : "two");
    ground("sub", 4, b, WALK); drums(cyc(4, b), b % 2 ? "chopB" : "chopA");
    ground("T", 5, b, WALK, 1.05); if (b > 3) ground("sub", 5, b, LONG, .9);
    ground("sub", 6, b, BOUNCE); drums(cyc(6, b), b % 2 ? "chopB" : "chopA", { lift: 2 });
  }
  halves("A", 2, CB, -7, 1, "countersubject");
  tresillo(3, 1); holes(3, 1);
  divisions(4, .95); halves("T", 4, CA, -7, .9, "countersubject");
  halves("A", 5, CB, -7, .5, "countersubject");
  divisions(6, 1); tresillo(6, 1); holes(6, 1);
  ["theme in the sub alone", "far counter in half notes (alto)", "near counter as tresillo (tenor), far counter in its holes (alto)", "divisions (soprano) over the near counter",
    "ground up in the tenor, sub out three bars; roll", "everything at once, sub bouncing octaves"].forEach((on, i) => NOW.entries.push({ bar: 1 + 5 * i, voice: "—", form: `variation ${i + 1}`, on }));
  // the roll through the break climbs the subject, eighths then sixteenths
  for (let i = 0; i < 8; i++) put("ghost", cyc(5, 3, 2 * i), midiOf(TUNE[i]) - 7, 2, .18 + .04 * i, "fragment");
  for (let i = 0; i < 32; i++) put("ghost", cyc(5, 4, i), midiOf(TUNE[i % 17]) - 7 + Math.floor(i / 2), 1, .3 + .027 * i, "fragment");
  // sine chord tones over the ground from variation 3, the dominant made major
  const chord = (d) => [0, 2, 4].map((k) => midiOf(d + k) + (k === 1 && mod(d, 7) === 4 ? 1 : 0)).map((m) => { while (m < 69) m += 12; while (m > 81) m -= 12; return m; }).sort((a, b) => a - b);
  for (let v = 3; v <= 6; v++) for (let b = 1; b <= 5; b++) pad(cyc(v, b), chord(GROUND[b - 1]), 16, v === 5 ? .7 : .6 + .1 * (v - 3));
  NOW.entries.push({ bar: 11, voice: "pad", form: "ground", on: "sine chord tones over the ground" });
  hi(CB.map((d, i) => ({ t: 8 * i, d, len: 7.5 })), cyc(5, 1), "countersubject", { vel: .9, say: "far counter, in sine" });
  hi(CB.map((d, i) => ({ t: 8 * i, d, len: 7.5 })), cyc(6, 1), "countersubject", { say: "far counter again, over everything" });
  gong(at(1), 38, .7);
  gong(cyc(6, 1), 38, 1.1);
  boom(cyc(6, 1), { pan: [-.9, .9], travel: 3, dist: 9, layers: ["grenade-explosion-02", "distant-shot-01"], drop: true });
  deep(HEAD, cyc(6, 1));
  orn("casing-drop-01", cyc(1, 1, 3), .9, .6, .5);
  orn("pistol-heavy-slide-rack-01", cyc(2, 5, 12), .4, 1, -.3);
  orn("m4-bolt-rack-01", cyc(3, 5, 12), .4, .9, .4);
  orn("shotgun-pump-rack-01", cyc(4, 5, 10), .45, 1, -.3);
  roll("ghost", cyc(4, 5, 12), 4, 1, [76, 62], [1, .5]);
  roll("ghost", cyc(6, 5, 8), 8, 1, [58, 82], [.3, 1.1]);
  for (let s = 0, j = 0; s < 480; s += 3, j++)
    if (!(s >= 320 && s < 400 && j % 2)) put("hat", at(1, s), 62 + [0, 5, 10, 3, 12][j % 5], 1, .5 + (j % 4 ? 0 : .5), "drums", { gun: KIT.hat.guns[j % 3] });
}

// ════ V · Stretto ═════════════════════════════════════════════════════
{
  const at = movement("V · Stretto", 20, { device: "stretto at three beats at the fifth over the subject augmented in the sub; again with inversion; head in close imitation; subject, answer and crab over the last drop; major close", cross: "augmentation 2 : 1 against the entries; hats on E(11,16), ghosts on E(7,16)" });
  sub(up(SUBJECT, 0, { scale: 2 }), at(1), "augmentation");
  enter("T", up(SUBJECT, -7), at(1), "subject", { say: "D3" });
  enter("A", up(SUBJECT, -3), at(1, 12), "subject", { say: "A3, +3 beats" });
  enter("S", up(SUBJECT, 7), at(3), "subject", { say: "D5" });
  enter("A", up(SUBJECT, 4), at(3, 12), "subject", { say: "A4, +3 beats" });
  sub(up(SUBJECT, 0, { invert: true, scale: 2 }), at(5), "augmentation", { say: "A2, augmented and inverted" });
  enter("T", up(SUBJECT, -7), at(5), "subject", { say: "D3" });
  enter("A", up(SUBJECT, -3), at(5, 12), "subject", { say: "A3, +3 beats" });
  enter("S", up(SUBJECT, 7, { invert: true }), at(6), "inversion", { say: "A5, +4 beats, mirrored" });
  NOW.checks = {
    "T/A": fit(up(SUBJECT, -7), up(SUBJECT, -3, { delay: 12 })), "T/S mirrored": fit(up(SUBJECT, -7), up(SUBJECT, 7, { invert: true, delay: 16 })),
    "A/S mirrored": fit(up(SUBJECT, -3, { delay: 12 }), up(SUBJECT, 7, { invert: true, delay: 16 })), "sub augmented / T": fit(up(SUBJECT, -14, { scale: 2 }), up(SUBJECT, -7)),
  };
  // the head handed up through three voices, a step higher every half bar, over a dominant pedal
  for (let k = 0; k < 8; k++) {
    const v = "TAS"[k % 3];
    enter(v, up(HEAD, k - 4 + { T: -7, A: 0, S: 7 }[v]), at(9, 8 * k), "fragment", { say: "head", vel: .7 + .05 * k });
  }
  for (let b = 9; b <= 12; b++) [0, 3, 6, 8, 11, 14].forEach((t, j) => { if (!(b === 12 && t > 8)) put("sub", at(b, t), 33 + (j % 3 === 2 ? 12 : 0), 2.5, .6 + .08 * (b - 9) + (j ? 0 : .15), "ground"); });
  roll("ghost", at(11), 8, 2, [55, 65], [.2, .55]);
  for (let i = 0; i < 12; i++) put("ghost", at(12, i), midiOf(TUNE[i]) - 2 + i, 1, .5 + .05 * i, "fragment");
  orn("shotgun-pump-rack-01", at(12, 12), .32, 1.1, 0);

  sub(SUBJECT, at(13), "subject");
  for (const [v, o] of [["T", -7], ["A", 0], ["S", 7]]) enter(v, up(SUBJECT, o), at(13), "subject", { say: "tutti in octaves" });
  sub(up(ANSWER, -7), at(15), "subject", { say: "A1, answer" });
  enter("A", up(ANSWER, -7), at(15), "subject", { say: "A3, answer" });
  enter("S", up(COUNTER, -7), at(15), "countersubject", { say: "above" });
  enter("T", up(COUNTER, -14), at(15), "countersubject", { say: "below" });
  sub(SUBJECT, at(17), "subject");
  enter("S", up(SUBJECT, 7), at(17), "subject");
  enter("A", up(SUBJECT, 0, { retrograde: true }), at(17), "retrograde", { say: "crab, against the soprano" });
  enter("T", up(COUNTER, -11), at(17), "countersubject");
  // cadence: dominant pedal, the head twice, the run, three dominants, a major chord
  for (const h of [0, 1]) for (const [v, o] of [["T", -7], ["A", 0], ["S", 7]]) enter(v, up(HEAD, o + 4 * h - (v === "S" ? 7 * h : 0), { invert: !!h }), at(19, 8 * h), "fragment", { say: "head" });
  [0, 3, 6, 8, 11, 14].forEach((t) => put("sub", at(19, t), 33, 2.5, .9, "ground"));
  for (const [v, o] of [["T", -7], ["A", 0], ["S", 7]]) {
    enter(v, up(RUN, o + 1), at(20), "fragment", { say: "run" });
    for (const s of [4, 8, 10]) put(v, at(20, s), midiOf(o + (v === "A" ? 1 : 4)), 2, 1, "fragment");
  }
  [0, 1, 2, 3].forEach((t) => put("sub", at(20, t), [40, 41, 43, 45][t], 1, .9, "fragment"));
  for (const s of [4, 8, 10]) put("sub", at(20, s), 33, 2, 1, "ground");
  orn("smg-9mm-burst-01", at(20, 12), 4 * STEP * .96, 1, .3);
  for (const [v, m, hold] of [["S", 74, .5], ["A", 66, .4], ["T", 57, .45], ["snare", 62, .5], ["kick", 48, .4]]) put(v, at(21), m, 16, 1.2, "cadence", { hold });
  put("sub", at(21), 38, 20, 1.1, "cadence"); put("sub", at(21), 26, 20, .5, "cadence");
  orn("sniper-shot-tail-01", at(21), 2.8, 1.1, 0, -3);
  orn("casing-shotgun-shell-drop-01", at(21, 7), 1, .7, -.45);
  orn("casing-drop-01", at(21, 15), 1.1, .6, .45);

  // sines: the dominant chord climbing through the build, then the line on top of the last drop
  [[69, 73, 76], [73, 76, 81], [76, 81, 85], [76, 81, 85]].forEach((c, i) => pad(at(9 + i), c, 16, .5 + .1 * i));
  NOW.entries.push({ bar: 9, voice: "pad", form: "ground", on: "A major, climbing" });
  hi(SUBJECT, at(13), "subject", { centre: 84 });
  hi(COUNTER, at(15), "countersubject");
  hi(SUBJECT, at(17), "subject", { centre: 84 });
  pad(at(21), [74, 78, 81, 86], 20, 1);
  gong(at(1), 38, .8);
  gong(at(9), 33, .8);
  gong(at(13), 38, 1.2);
  gong(at(21), 38, 1.2);
  boom(at(13), { pan: 0, dist: 8, layers: ["grenade-explosion-01", "grenade-explosion-02", "distant-shot-01"], drop: true, vel: 1.15 });
  boom(at(21), { pan: -.8, dist: 20, layers: ["distant-shot-01"], vel: .8 });
  deep(SUBJECT.filter((n) => n.t < 8), at(13));
  bells(HEAD, at(19)); bells(up(HEAD, 4, { invert: true }), at(19, 8), .9);
  for (const [m, k] of [[66, 0], [69, 1], [74, 2]]) put("bell", at(21) + k * .5, m, 32, .9, "bell", { kind: "glass" });

  for (let b = 1; b <= 20; b++) {
    const build = b >= 9 && b <= 12;
    if (!build) hats(at(b), b >= 13 ? 11 : 9, { rot: b, lift: b >= 13 ? 7 : 0 });
    else hats(at(b), 5, { rot: b, vel: .8 });
    if (b === 20) { drums(at(b), "half", { vel: 1 }); continue; }
    if (build) { if (b < 11) drums(at(b), "half", { vel: .8 }); else if (b === 11) put("kick", at(b), 48, 2, .9, "drums"); continue; }
    drums(at(b), b % 2 ? "chopB" : "chopA", { lift: b >= 13 ? 2 : 0 });
    if (b >= 13 && b < 19) bjorklund(7, 16, b).onsets.forEach((p) => { if (p % 4) put("ghost", at(b, p), midiOf(tuneAt(p + 16 * (b % 2)) + 7) - 5, 1, .22 + .05 * (p % 3), "drums"); });
    if (b % 4 === 0 && b < 19) roll("ghost", at(b, 12), 4, 1, [60 + b, 74 + b / 2], [.5, 1.1]);
  }
}
// ── shotguns (@jeffrey 2026-11-22: "i want to also explore shotguns") ──────────────────────────────
// The 12 gauge as its own voice, not just the snare: on every drop a pump rack on the and of four and the
// blast on the next downbeat (rack, then shot: the pump is the upbeat), pitched down a fourth so it sits
// under the snare; in the passacaglia's last variation the blasts walk the ground's five notes; the stretto's
// tutti ends on a semi-auto run of three; and the last chord is a single shot far away.
const shotAt = (k, bar, step = 0) => movements[k].start + (bar - 1) * movements[k].per + step;
const blast = (k, bar, step, midi, vel = 1, pan = 0) => {
  events.push({ voice: "shot", step: shotAt(k, bar, step), midi, len: 16, vel, form: "drums", mvt: movements[k].title, pan, line: "shot" });
  events.push({ voice: "orn", step: shotAt(k, bar, step) - 4, midi: 60, len: 4, vel: .8 * vel, form: "drums", mvt: movements[k].title,
                gun: "shotgun-pump-rack-01", hold: .45, pan: -pan * .6 });
};
for (let b = 9; b <= 16; b += 2) blast(2, b, 0, 50, b === 9 ? 1.2 : .9, [0, -.4, .4, 0][(b - 9) / 2]);
[50, 48, 46, 43, 45].forEach((m, i) => blast(3, 26 + i, 0, m, 1 - .06 * i, [-.5, -.25, 0, .25, .5][i]));
for (let b = 13; b <= 18; b += 2) blast(4, b, 0, 50, b === 13 ? 1.25 : .95, [0, .45, -.45][(b - 13) / 2]);
[0, 2, 4].forEach((s, i) => events.push({ voice: "shot", step: shotAt(4, 18, 8 + s), midi: 53 + 2 * i, len: 2, vel: .8 + .1 * i,
  form: "drums", mvt: movements[4].title, gun: "shotgun-auto-shot-01", pan: .3 - .3 * i, line: "shot" }));
events.push({ voice: "shot", step: shotAt(4, 21), midi: 45, len: 16, vel: .7, form: "drums", mvt: movements[4].title, pan: -.8, line: "shot" });

// ── bubble pops (@jeffrey: "can we add some sharp bubble pops") ───────────────────────────────────
// Short sines that leap up a fifth in 18 ms after a click: water drops, scattered in the quiet passages
// (the courante, the canon before its drop, the passacaglia's first and fifth variations, the stretto's
// hush) on the scale's notes between C6 and D7, near and wide.
{
  let s = 99;
  const r = () => ((s = (s * 1664525 + 1013904223) >>> 0) / 2 ** 32);
  const scale = [0, 2, 3, 5, 7, 8, 10].map((d) => 62 + d);
  const quiet = [[1, 1, 9], [2, 1, 9], [3, 1, 11], [3, 21, 26], [4, 9, 13]];
  for (const [k, b0, b1] of quiet) {
    const m = movements[k];
    for (let st = (b0 - 1) * m.per; st < (b1 - 1) * m.per; st++) {
      if (r() > .16) continue;
      const midi = scale[Math.floor(r() * 7)] + 12 * (1 + Math.floor(r() * 2));
      events.push({ voice: "pop", step: m.start + st + (r() < .3 ? .5 : 0), midi, len: 1, vel: .55 + .45 * r(), form: "pop",
                    mvt: m.title, pan: (r() * 2 - 1) * .9, line: "pop" });
    }
  }
}
// ── the donkey (@jeffrey 2026-11-22: "can we source donkey sounds … and try and aesthetivox some donkey") ──
// A real bray (felix.blume, Freesound, CC0: assets in pop/wattajetta/assets/donkeys) sung onto the subject's
// whole subject by bin/donkeyvox.py (@jeffrey: "can we extend the vocals"): seventeen "haw"s from two
// takes of the same donkey on D A F E G F E D · E F G A Bb A G E F, three times as slow as the subject,
// each haw stretched to hold its note (six bars a statement). It answers three times: in the invention's second sequence, in the
// passacaglia's hushed fifth variation, and in the stretto's hush before the last drop; and a herd brays
// far off at the passacaglia's start.
const DONKEY = resolve(OUT, ".donkey/subject"), AUG = 3;
const HAW_AT = SUBJECT.map((n) => AUG * n.t);
if (!existsSync(resolve(DONKEY, "donkeyvox.json"))) {
  const ends = [...HAW_AT.slice(1), AUG * LEN], takes = ["donkey-bray-04.mp3", "donkey-bray-05.mp3"];
  const r = spawnSync(PY, [resolve(HERE, "donkeyvox.py"), takes.map((t) => resolve(LANE, "../wattajetta/assets/donkeys", t)).join(","), DONKEY,
    "--notes", SUBJECT.map((n) => name(midiOf(n.d))).join(","), "--skip", "7,12",          // two runs with no clear vowel
    "--hold", HAW_AT.map((t, i) => ((ends[i] - t) * STEP * .97).toFixed(3)).join(",")], { stdio: "inherit" });
  if (r.status) throw new Error("donkeyvox failed");
}
const haws = JSON.parse(readFileSync(resolve(DONKEY, "donkeyvox.json"), "utf8")).syllables;
for (const [k, bar, pan] of [[0, 13, .45], [3, 21, -.4], [4, 9, .3]]) {
  haws.forEach((h, i) => {
    const gap = ((HAW_AT[i + 1] ?? AUG * LEN) - HAW_AT[i]) * STEP + .12;
    events.push({ voice: "donkey", step: shotAt(k, bar, HAW_AT[i]) - h.voicedFrom / STEP, midi: h.midi, len: 2, vel: i ? .9 : 1,
                  form: "donkey", mvt: movements[k].title, file: resolve(DONKEY, h.file), hold: Math.min(h.seconds, gap), pan, line: "donkey" });
  });
}
events.push({ voice: "donkey", step: shotAt(3, 1, 4), midi: 62, len: 16, vel: .5, form: "donkey", mvt: movements[3].title,
              raw: resolve(LANE, "../wattajetta/assets/donkeys/donkey-herd-01.flac"), hold: 6, pan: .8, line: "herd" });
// ── springs and mice (@jeffrey 2026-11-22: "can we also add spring sounds and mice?") ──────────────
// One-shots cut by bin/slice-hits.py from the CC0 libraries in pop/wattajetta/assets/springs and /mice.
// Springs mark the seams: a door-stop boing on the and of four into every movement and every drop, a
// jaw-harp bounce on the courante's hemiola bars and the passacaglia's hushed fifth variation, short
// battery-spring boings answering in the invention's sequences, and a coil pulled and let go at the end.
// Mice (real ones: a pet mouse, wild mice, baby rats) chatter high and wide in the quiet passages, and a
// little crowd of them answers every time the donkey finishes.
const CRIT = resolve(OUT, ".critters");
const cut = (kind) => JSON.parse(readFileSync(resolve(CRIT, kind, "hits.json"), "utf8")).map((h) => ({ ...h, path: resolve(CRIT, kind, h.file) }));
const springs = cut("springs"), mice = cut("mice");
const from = (list, src) => list.filter((h) => h.source.startsWith(src));
let crit = 7;
const cr = () => ((crit = (crit * 1664525 + 1013904223) >>> 0) / 2 ** 32);
const clip = (voice, k, step, h, vel, pan) => events.push({ voice, step, midi: 72, len: 2, vel, form: voice, mvt: movements[k].title,
  file: h.path, hold: h.seconds, pan, line: voice });
{
  const door = from(springs, "spring-doorstop"), harp = from(springs, "spring-jawharp"), small = from(springs, "spring-boing-0");
  const coil = from(springs, "spring-coil");
  const seams = [[1, 1], [2, 1], [3, 1], [4, 1], [2, 9], [3, 26], [4, 13]];
  seams.forEach(([k, bar], i) => { const st = shotAt(k, bar) - 4, m = movements.findLastIndex((x) => x.start <= st);
    clip("spring", m, st, door[i % door.length], .9, i % 2 ? .55 : -.55); });
  [[1, 4], [1, 8], [3, 21]].forEach(([k, bar], i) => clip("spring", k, shotAt(k, bar), harp[i % harp.length], .8, [-.3, .3, 0][i]));
  for (let bar = 9; bar <= 16; bar += 2) clip("spring", 0, shotAt(0, bar, 14), small[(bar - 9) / 2 % small.length], .6, bar % 4 === 1 ? .7 : -.7);
  if (coil.length) clip("spring", 4, shotAt(4, 20, 8), coil[0], .8, 0);
  const quiet = [[1, 1, 9], [2, 1, 9], [3, 1, 11], [3, 21, 26], [4, 9, 13]];
  for (const [k, b0, b1] of quiet) {
    const m = movements[k];
    for (let st = (b0 - 1) * m.per; st < (b1 - 1) * m.per; st++) if (cr() < .05) clip("mouse", k, m.start + st, mice[Math.floor(cr() * mice.length)], .5 + .4 * cr(), (cr() * 2 - 1) * .9);
  }
  for (const [k, bar] of [[0, 13], [3, 21], [4, 9]]) {
    const end = shotAt(k, bar) + 3 * LEN;
    for (let j = 0; j < 6; j++) clip("mouse", k, end + 1 + j * 1.5 + cr(), mice[Math.floor(cr() * mice.length)], .7 - .06 * j, (cr() * 2 - 1) * .8);
  }
}
// ── the rock kit (@jeffrey 2026-11-22: "bringing in classic rock drum samples around 1:20 would be dope") ──
// A real miked kit (DRSKit and CrocellKit by the DrumGizmo team, CC BY 4.0: assets/rockkit, credit in its
// SOURCES.json) doubles the gun drums from the passacaglia's third variation (about 1:20) to the end, so the
// groove stays the guns' and gains a room. It comes in by halves: hats and kick in variation 3, the snare
// and a crash in variation 4, out for the hushed fifth but for a tom fill down into the last, then ride and
// rimshots with a crash every second bar. In the stretto it plays on, keeps only a soft ride bell through
// the hush, fills down the toms into the last drop, and the last chord gets the big crash.
const ROCK = resolve(LANE, "assets/rockkit"), rockFrom = shotAt(3, 11), rr = {};
const rockFile = (kind, layers, takes) => { const k = `${kind}`; rr[k] = ((rr[k] ?? -1) + 1) % takes; return `rock-${kind}${layers}-0${rr[k] + 1}`; };
const rock = (step, name, vel, room = 0) => events.push({ voice: "rock", step, midi: 60, len: 2, vel, form: "drums",
  mvt: movements.findLast((m) => m.start <= step).title, rock: name, room, line: "rock" });
const inRock = (st) => st >= rockFrom && !(st >= shotAt(3, 21) && st < shotAt(3, 26)) && !(st >= shotAt(4, 9) && st < shotAt(4, 13));
for (const e of events.filter((e) => ["kick", "snare", "ghost", "hat"].includes(e.voice) && inRock(e.step))) {
  const full = e.step >= shotAt(3, 16);
  if (e.voice === "kick") rock(e.step, rockFile("kick", e.vel >= .85 ? "-v2" : "-v1", 3), e.vel, .7);
  else if (e.voice === "hat") rock(e.step, e.vel >= .75 ? rockFile("hat-closed", "-v2", 2) : rockFile("hat-closed", "-v1", 2), .8 * e.vel);
  else if (!full) continue;
  else if (e.voice === "snare") rock(e.step, e.step >= shotAt(3, 26) && e.vel >= .9 ? rockFile("snare-rim", "-v2", 3) : rockFile("snare", e.vel >= .8 ? "-v2" : "-v1", 3), e.vel, .8);
  else if (e.vel > .5) rock(e.step, rockFile("snare-ghost", "", 3), e.vel);
}
const crashAt = [[3, 16], [3, 26], [3, 28], [3, 30], [4, 1], [4, 13], [4, 15], [4, 17]];
crashAt.forEach(([k, bar], i) => rock(shotAt(k, bar), rockFile(i % 2 ? "crash-b" : "crash", "", 2), i < 1 ? .8 : 1, .9));
for (let st = shotAt(3, 26); st < shotAt(3, 31); st += 4) rock(st, rockFile("ride-bow", "-v2", 2), .7);
for (let st = shotAt(4, 13); st < shotAt(4, 19); st += 4) rock(st, rockFile("ride-bow", "-v2", 2), .7);
for (let st = shotAt(4, 9); st < shotAt(4, 13) - 8; st += 4) rock(st, rockFile("ride-bell", "", 2), .45);
for (const [k, bar] of [[3, 25], [4, 12]]) ["tom-high", "tom-high", "tom-mid", "tom-mid", "tom-floor", "tom-floor", "tom-floor", "tom-floor"]
  .forEach((t, j) => rock(shotAt(k, bar, 8 + j), rockFile(t, j % 2 ? "-v1" : "-v2", 2), .7 + .04 * j, .8));
rock(shotAt(4, 21), rockFile("crash", "", 2), 1, 1);
// ── sheep (@jeffrey 2026-11-22: "can we find sheep sounds to aesthetivox in?") ───────────────────────
// Real bleats (pop/wattajetta/assets/sheep, CC0) sung onto the passacaglia's ground, D C Bb G A, one held
// bleat a bar, by bin/donkeyvox.py (each lands within 15 cents). A sheep doubles the ground an octave over
// the sub in the theme (variation 1); in the hushed fifth variation, where the ground climbs into the
// tenor, three sheep sing it in octaves: a low one, a middle one and a high one (Breviceps, Garuda1982).
const SHEEP = resolve(OUT, ".sheep");
const sheepSing = (who, bar0, vel, pan) => {
  const list = JSON.parse(readFileSync(resolve(SHEEP, who, "donkeyvox.json"), "utf8")).syllables;
  list.forEach((h, i) => events.push({ voice: "sheep", step: shotAt(3, bar0 + i) - h.voicedFrom / STEP, midi: h.midi, len: 16, vel,
    form: "ground", mvt: movements[3].title, file: resolve(SHEEP, who, h.file), hold: h.seconds, pan, line: who }));
};
// arpeggiated (@jeffrey: "can the sheeps be arpeggiated"): bin/sheep-arps.py breaks each bar's chord
// (D minor, C, Bb, G minor, A major) into separately sung bleats. In the theme one sheep goes up and back
// down in quarters; in the hushed fifth variation a low sheep climbs in quarters, a middle one follows an
// eighth late, and a high one falls in dotted eighths, so it drifts three against four across the bar.
const ARPS = JSON.parse(readFileSync(resolve(SHEEP, "arps/arps.json"), "utf8"));
// (@jeffrey: "the vocals i want more of the sheep"): they now sing through every variation, gathering a voice
// at a time: one in the theme and the second, two in the third and fourth, all four in the fifth and the last.
for (const [who, bar0, vel, pan] of [["solo", 1, .9, -.35], ["solo", 6, .85, -.35], ["solo", 11, .85, -.35], ["mid", 11, .7, .3],
  ["low", 16, .75, -.6], ["mid", 16, .75, .3], ["solo", 21, .8, -.2], ["low", 21, .8, -.6], ["mid", 21, .75, .2], ["high", 21, .7, .6],
  ["solo", 26, .9, -.2], ["low", 26, .85, -.6], ["mid", 26, .8, .2], ["high", 26, .8, .6]]) {
  const a = ARPS[who];
  a.syllables.forEach((h, i) => events.push({ voice: "sheep", step: shotAt(3, bar0) + a.onsets[i] - h.voicedFrom / STEP, midi: h.midi,
    len: 4, vel: vel * (i % 4 ? .88 : 1), form: "ground", mvt: movements[3].title, file: resolve(SHEEP, "arps", who, h.file),
    hold: h.seconds, pan: pan + (i % 2 ? .08 : -.08), line: `sheep-${who}` }));
}
// ── a ukulele, played by a person (@jeffrey 2026-11-22: "a simulated human ukulele, with tweaked notes, and a
// more humanized flow, steady rock back and forth") ─────────────────────────────────────────────────
// Re-entrant G C E A, plucked-string synthesis. Over the passacaglia's ground it strums steady eighths, down
// and up, with the chords coloured: D minor 9, C add 9, Bb 6/9, G minor 7, A sus 4 resolving to A7. A person's
// hand: downstrokes rake all four strings over 9–16 ms, upstrokes catch the top three faster and lighter, the
// downbeats lean, two and four get a palm-muted chunk, every stroke drifts a few ms, the ups lag a hair
// (the rock), and the chord change gets a little fret squeak. The theme is left to the sub; the hushed fifth
// variation is fingerpicked, one string an eighth.
const UKE_CHORDS = [[69, 62, 64, 72], [67, 62, 64, 72], [65, 62, 67, 70], [67, 62, 65, 70], [69, 62, 64, 69], [67, 61, 64, 69]];
{
  let u = 31;
  const ur = () => ((u = (u * 1664525 + 1013904223) >>> 0) / 2 ** 32);
  const g = () => ur() + ur() - 1;
  for (let bar = 6; bar <= 30; bar++) {
    const v = Math.floor((bar - 1) / 5), b = (bar - 1) % 5, picked = v === 4;
    for (let e = 0; e < 8; e++) {
      const chord = UKE_CHORDS[b === 4 && e >= 4 ? 5 : b], step = shotAt(3, bar, 2 * e), down = e % 2 === 0;
      const late = 4 * g() + (down ? 0 : 7);                                              // ms: the rock
      if (picked) {
        events.push({ voice: "uke", step, late, midi: chord[[0, 2, 1, 3, 2, 1, 3, 2][e]], vel: .45 + .1 * ur(), mvt: movements[3].title,
                      form: "uke", len: 2, line: "uke", string: [0, 2, 1, 3, 2, 1, 3, 2][e], mute: false });
        continue;
      }
      const vel = (e === 0 ? 1 : e === 4 ? .9 : down ? .78 : .58) * (.92 + .16 * ur()) * (.85 + .15 * (v - 1) / 4);
      const mute = (e === 2 || e === 6) && ur() < .7;
      const order = down ? [0, 1, 2, 3] : [3, 2, 1];
      const rake = down ? 9 + 7 * ur() : 5 + 4 * ur();
      order.forEach((s, j) => events.push({ voice: "uke", step, late: late + rake * j / (order.length - 1), midi: chord[s], string: s,
        vel: vel * (down ? 1 - .06 * j : .9 - .05 * j), mvt: movements[3].title, form: "uke", len: 2, line: "uke", mute }));
      if (e === 0 && b !== 0) events.push({ voice: "uke", step, late: late - 60, midi: 0, squeak: true, vel: .5 * ur(), mvt: movements[3].title, form: "uke", len: 1, line: "uke" });
    }
  }
}
// ── brushes, brooms and whooshes (@jeffrey 2026-11-22: "brushy sounds like sweep … broom sweep and woosh and
// hair flip") ─────────────────────────────────────────────────────────────────────────────────────
// Real wire brushes on a snare (DRSKit, DrumGizmo, CC BY 4.0), broom strokes (CC0 and F.M.Audio, CC BY 4.0) and
// whooshes and cloth swishes (CC0; no clean hair flip exists under a free licence, so a swung shirt stands in):
// assets/brushes, credits in its SOURCES.json. In the quiet passages a brush groove plays under the guns:
// a circular swish on two and four, brush taps on the ands, a broom stroke on three every other bar (in the
// courante's 6/4, one swish a half bar). At the seams a whoosh crosses the head, its loudest moment on the
// downbeat; between the passacaglia's variations and after each donkey a cloth flip goes the other way.
{
  const BR = resolve(OUT, ".brushes"), lib = (kind) => JSON.parse(readFileSync(resolve(BR, kind, "hits.json"), "utf8")).map((h) => ({ ...h, path: resolve(BR, kind, h.file) }));
  const taps = lib("taps"), swish = lib("swish"), broom = lib("broom"), whoosh = lib("whoosh"), cloth = lib("cloth");
  let w = 11;
  const wr = () => ((w = (w * 1664525 + 1013904223) >>> 0) / 2 ** 32);
  const hit = (voice, k, step, h, vel, pan, more = {}) => events.push({ voice, step, midi: 60, len: 2, vel, form: voice, mvt: movements[k].title,
    file: h.path, hold: more.hold ?? h.seconds, pan, line: voice, ...more });
  for (const [k, b0, b1] of [[1, 1, 9], [2, 1, 9], [3, 1, 6], [3, 21, 26], [4, 9, 13]]) {
    const m = movements[k], six = m.per === 24;
    for (let bar = b0; bar < b1; bar++) {
      const at = (st) => shotAt(k, bar, st);
      for (const st of six ? [0, 12] : [4, 12]) hit("brush", k, at(st), swish[Math.floor(wr() * swish.length)], .75, -.25, { hold: (six ? 12 : 8) * STEP + .1 });
      (six ? [2, 6, 10, 14, 18, 22] : [2, 6, 10, 14]).forEach((st, i) => hit("brush", k, at(st) + (wr() - .5) * .12, taps[i % 2 ? 2 + Math.floor(wr() * 2) : Math.floor(wr() * 2)],
        (i % 2 ? .55 : .8) * (.9 + .2 * wr()), -.1));
      if (bar % 2 === 0) hit("brush", k, at(six ? 6 : 8), broom[Math.floor(wr() * broom.length)], .6, .4);
    }
  }
  const across = (k, bar, list, vel, from, to) => { const h = list[Math.floor(wr() * list.length)];
    hit("whoosh", k, shotAt(k, bar) - 1 - h.peakSeconds / STEP, h, vel, from, { to }); };
  [[1, 1], [2, 1], [3, 1], [4, 1]].forEach(([k, bar], i) => across(k, bar, whoosh, 1, i % 2 ? .9 : -.9, i % 2 ? -.9 : .9));
  [[3, 6], [3, 11], [3, 16], [3, 21], [4, 5], [4, 9]].forEach(([k, bar], i) => across(k, bar, cloth, .8, i % 2 ? -.8 : .8, i % 2 ? .8 : -.8));
  for (const [k, bar] of [[0, 13], [3, 21], [4, 9]]) across(k, bar + 6, cloth, .7, .6, -.6);
}
const END = POS, N = Math.ceil((END * STEP + TAIL) * SR);

// ── the hand: where in the bin, and how hard ──────────────────────────
// After rhythm-platter digest 07: the score is the grid and stays the grid;
// this table only moves a hit inside its beat bin and shades its loudness.
// Kick, snare (and the ghost doubled on it), sub, bells and booms are locked.
let seed = 172;
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 2 ** 32);
const wobble = () => rnd() + rnd() - 1;                     // −1…1, most of it near 0
// late: ms behind the grid ± spread. arc: swell across the phrase. drift: slow wander. jitter: per hit.
const FEEL = {
  kick: { late: 0, spread: 0, arc: .06 }, snare: { late: 0, spread: 0, arc: .06 },
  ghost: { late: 7, spread: 2.5, arc: .14, drift: .15 },
  hat: { late: 3, spread: 1.5, arc: .16, drift: .15, jitter: .08 },
  S: { late: 0, spread: 2.5, arc: .1, drift: .08 }, A: { late: 0, spread: 2.5, arc: .1, drift: .08 },
  T: { late: 1, spread: 2.5, arc: .1, drift: .08 }, orn: { late: 0, spread: 4, arc: 0 }, shot: { late: 0, spread: 0, arc: 0 },
  hi: { late: 0, spread: 0, arc: .1 }, pad: { late: 0, spread: 0, arc: .08 },
};
// swing: the share of each eighth given to its first sixteenth (hats, ghosts; half as much for the guns' lines)
const SWING = { "I · Invention": .56, "V · Stretto": .55 };
const wander = {};
for (const e of events) {
  const m = movements.find((m) => m.title === e.mvt), f = FEEL[e.voice];
  e.grid = +(e.step * STEP).toFixed(5); e.late = e.voice === "uke" ? e.late : 0; e.score = e.vel;   // the uke brings its own hand
  if (!f) continue;
  const phrase = m.per === 24 ? 2 : 4, p = (((e.step - m.start) / m.per) % phrase) / phrase;
  let v = e.vel * (1 + f.arc * (Math.sin(Math.PI * p) - .5));
  if (f.drift) { wander[e.voice] = .9 * (wander[e.voice] ?? 0) + .3 * wobble(); v *= 1 + f.drift * wander[e.voice]; }
  if (f.jitter) v *= 1 + f.jitter * wobble();
  e.vel = v;
  if (e.lock || e.voice === "kick" || e.voice === "snare") continue;
  let late = f.late + f.spread * wobble();
  const sw = SWING[e.mvt];
  if (sw && Number.isInteger(e.step) && e.step % 2 === 1) late += (sw - .5) * 2 * STEP * 1000 * (e.voice === "hat" || e.voice === "ghost" ? 1 : e.voice in KIT ? .5 : 0);
  e.late = +late.toFixed(2);
}

// ── render the guns ───────────────────────────────────────────────────
const bus = Object.fromEntries(Object.keys(KIT).map((k) => [k, { L: new Float32Array(N), R: new Float32Array(N) }]));
const lastTake = {};
// a take at random, never the same one twice running
const pick = (voice, n) => { let t = Math.floor(rnd() * n); if (n > 1 && t === lastTake[voice]) t = (t + 1 + Math.floor(rnd() * (n - 1))) % n; return (lastTake[voice] = t); };
// ── the mid synth: the tenor's line given a voice in the middle of the spectrum ─
// (@jeffrey 2026-11-22: "a synth voice thats in the mids would be sick too"). The tenor is a pitched
// AK-47, which reads as tom tuning, not notes; this doubles every tenor note around G4 (300–900 Hz),
// so the inner line of the counterpoint is heard as a tune.
{
  const tenor = events.filter((e) => e.voice === "T" && e.midi != null).sort((a, b) => a.step - b.step);
  const ms = tenor.map((e) => e.midi), middle = (Math.min(...ms) + Math.max(...ms)) / 2;
  const oct = 12 * Math.round((67 - middle) / 12);
  for (const e of tenor) {
    let m = e.midi + oct;
    while (m > 81) m -= 12;
    while (m < 55) m += 12;
    events.push({ ...e, voice: "mid", midi: m });   // with the tenor's own timing and velocity
  }
}
const LAYER = new Set(["hi", "pad", "bell", "boom", "sub", "mid", "pop", "donkey", "spring", "mouse", "rock", "sheep", "uke", "implode", "bullet", "brush", "whoosh"]);

// ── the stereo story (@jeffrey 2026-11-22: "can we do a better stereo story") ─────────────────────
// Each movement stages its counterpoint across the field instead of leaving every voice where it
// stands. I: the invention's two parts argue across the room, alto right, tenor and its mid synth
// left. II: the courante's pair trade sides at bar 5, when they exchange parts. III: the canon opens
// out, one position per voice as each speed enters. IV: the passacaglia keeps its ground in the
// centre and walks the counters round the field, a new position each variation, everything wide on
// the last. V: the stretto starts at the edges and closes in entry by entry, the tutti in octaves
// stands in the middle, and the last chord blooms wide again. Each change glides over a bar.
const STAGE_HOME = { S: -.68, A: .5, T: -.3, mid: -.2, hi: .3, ghost: .25 };
const lerp = (a, b, x) => a + (b - a) * Math.max(0, Math.min(1, x));
function staged(k, voice, bar) {
  const home = STAGE_HOME[voice];
  if (home === undefined) return undefined;
  switch (k) {
    case 0: return { S: -.8, A: .6, T: -.6, mid: -.55, hi: .45, ghost: .15 }[voice];
    case 1: {
      const side = lerp(-.55, .55, bar - 4.5);                       // the exchange at bar 5
      return { S: -.75, A: .05, T: side, mid: side * .95, hi: .5, ghost: -.2 }[voice];
    }
    case 2: {
      const open = .2 + .8 * Math.min(1, (bar - 1) / 6);             // wider as each prolation enters
      return { S: .8, A: 0, T: -.8, mid: -.75, hi: .45, ghost: .3 }[voice] * open;
    }
    case 3: {
      const ring = { A: [0, -.5, -.5, .3, .6, -.85], T: [0, 0, .5, .5, 0, .85], S: [-.7, -.7, -.7, -.8, -.5, .95],
                     mid: [0, 0, .45, .45, 0, .8], hi: [.4, .4, .4, .4, -.5, .6], ghost: [.25, .25, -.25, -.25, .25, .4] }[voice];
      const c = Math.min(5, Math.floor((bar - 1) / 5)), into = (bar - 1) - 5 * c;
      return lerp(ring[Math.max(0, c - 1)], ring[c], c ? into : 1);
    }
    default: {
      const wide = { S: -.9, A: .85, T: -.85, mid: -.8, hi: .6, ghost: .3 }[voice];
      const squeeze = bar < 13 ? lerp(1, .3, (bar - 1) / 11) : bar < 19 ? lerp(.3, .08, (bar - 13) / 4) : lerp(.08, .75, bar - 19);
      return wide * squeeze;
    }
  }
}
function stage(voice, step, fallback) {
  let k = 0;
  movements.forEach((m, i) => { if (step >= m.start) k = i; });
  const m = movements[k], bar = (step - m.start) / m.per + 1;
  let p = staged(k, voice, bar);
  if (p === undefined) return fallback;
  if (k > 0 && bar < 2) p = lerp(staged(k - 1, voice, movements[k - 1].bars + 1), p, bar - 1);
  return Math.max(-.95, Math.min(.95, p));
}

// ── the ear (@jeffrey 2026-11-22: "have the stereo mix be based on an ear") ──────────────────────────
// Every placed source is heard by two ears 17.5 cm apart instead of being panned by level. From the side
// (pan ±1 = 90°) the far ear hears it up to 0.66 ms later (Woodworth: a/c · (θ + sin θ)), about 3 dB
// quieter and darker (the head's shadow: a low-pass down to about 4 kHz); the near ear a little louder.
// In the middle both ears hear the same. earAt is a fixed source; earMoving takes a pan per sample.
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

// ── implosions (@jeffrey 2026-11-22: "can we spatialize this so we can have implosions") ─────────────
// The explosions run backwards: noise and a reversed grenade drawn in from far off at both sides (or spiralling
// round the head) to a point between the ears, opening from dark to bright and rising from a sub rumble, cut
// dead on the downbeat of a drop with a thump, so each one is sucked in just before its explosion goes off.
for (const [k, bar, kind] of [[1, 1, "converge"], [2, 9, "converge"], [3, 26, "spiral"], [4, 13, "spiral"]])
  events.push({ voice: "implode", step: shotAt(k, bar), midi: 36, len: 1, vel: 1, form: "boom", mvt: movements[k].title, kind, dur: 1.9, line: "implode" });

// ── ballistics (@jeffrey: "do we have ballistics sim so sometimes gunshots can ricochet or be zipping past
// the ear") ─────────────────────────────────────────────────────────────────────────────────────────
// A few of the rifle and pistol shots in the loud passages send a round past you or off a wall. A pass-by
// is a bullet on a straight line that misses your head by 0.4–1.5 m: its whiz rises in pitch as it comes and
// drops as it goes (Doppler), louder as it nears, and it crosses from one ear to the other at its closest point,
// where its shock wave cracks. A ricochet is a hit on something hard (a click and a metal ping) and then the
// tumbling round whining away, falling in pitch and fluttering as it spins, carried off to the far side.
{
  let b = 2026;
  const br = () => ((b = (b * 1664525 + 1013904223) >>> 0) / 2 ** 32);
  const loud = [[0, 9, 19], [2, 9, 17], [3, 16, 21], [3, 26, 31], [4, 1, 9], [4, 13, 21]];
  const hot = (st) => loud.some(([k, a, z]) => st >= shotAt(k, a) && st < shotAt(k, z));
  let made = 0;
  for (const e of events.filter((e) => ["S", "A", "T"].includes(e.voice) && hot(e.step))) {
    if (made >= 34 || br() > .07) continue;
    const from = stage(e.voice, e.step, KIT[e.voice].pan), side = from >= 0 ? 1 : -1;
    const ricochet = e.voice !== "A" && br() < .5;
    events.push({ voice: "bullet", step: e.step + (ricochet ? (.12 + .2 * br()) : (.04 + .05 * br())) / STEP, midi: 72, len: 2, vel: .8 + .3 * br(),
      form: "ballistics", mvt: e.mvt, kind: ricochet ? "ricochet" : "zip", from: side * (.6 + .35 * br()), to: -side * (.6 + .35 * br()),
      miss: .4 + 1.1 * br(), pitch: 3000 + 1400 * br(), line: "bullet" });
    made++;
  }
}
events.sort((a, b) => a.step - b.step);
for (const e of events) {
  e.time = +(e.step * STEP + (e.late ?? 0) / 1000).toFixed(5);
  if (LAYER.has(e.voice)) continue;
  const k = KIT[e.voice], gun = e.gun ?? k.guns[0];
  e.semis = Math.max(-19, Math.min(31, e.midi - k.home));
  const hold = e.hold ?? Math.min(k.hold, Math.max(k.hold * .45, e.len * STEP * .95));
  const takes = !!k.takes && !e.gun, take = takes ? pick(e.voice, slices(gun, true).length) : 0;
  e.take = take;
  const { y, lead } = hit(gun, { semis: e.semis, hold, take, takes, lp: k.lowpass, hp: k.highpass, fat: k.fat });
  const pan = e.voice === "hat" ? HAT_PAN[gun] : e.voice === "orn" ? Math.max(-.9, Math.min(.9, 1.5 * (e.pan ?? 0))) : e.voice === "shot" ? (e.pan ?? 0) : stage(e.voice, e.step, k.pan);
  e.pan = +pan.toFixed(3);
  const b = bus[e.voice], i0 = Math.round(e.time * SR) - lead, ear = earAt(b, pan);
  for (let i = Math.max(0, -i0); i < y.length && i0 + i < N; i++) ear(i0 + i, y[i] * e.vel);
}

// ── the sub: a sine with a little second and third, and a quiet reese an
// octave up so it reads on small speakers. Mono, dry. ─────────────────
const subBus = new Float32Array(N), reese = new Float32Array(N);
for (const e of events.filter((e) => e.voice === "sub")) {
  const hz = 440 * 2 ** ((e.midi - 69) / 12), i0 = Math.round(e.time * SR), n = Math.round(e.len * STEP * SR);
  // attack: 8 ms on a raised-sine curve (no click); decay: down to 72 % over about 0.35 s; release: an
  // exponential tail of 70 ms from wherever the level is when the note ends (no straight-line cut)
  const rise = .008 * SR, fall = .07 * SR, lean = Math.min(1.25, (65 / hz) ** .35);   // the lowest notes a little hotter
  const shape = (i) => { const t = i / SR, on = i < rise ? .5 - .5 * Math.cos(Math.PI * i / rise) : 1; return on * (.72 + .28 * Math.exp(-t / .35)); };
  const atEnd = shape(n);
  for (let i = 0; i < n + 5 * fall && i0 + i < N; i++) {
    const t = i / SR, env = i < n ? shape(i) : atEnd * Math.exp(-(i - n) / fall);
    const p = 2 * Math.PI * hz * t;
    subBus[i0 + i] += e.vel * lean * env * Math.tanh(1.4 * (Math.sin(p) + .22 * Math.sin(2 * p) + .1 * Math.sin(3 * p)));
    const saw = (f) => 2 * ((f * t) % 1) - 1;
    reese[i0 + i] += e.vel * env * .5 * (saw(2 * hz * 1.007) + saw(2 * hz / 1.007));
  }
}
{ const growl = lowpass(reese, 420); for (let i = 0; i < N; i++) subBus[i] += .16 * growl[i]; }
// the kick pushes the sub (and, harder, the booms) out of its way
const kickDuck = (depth, tau) => {
  const d = new Float32Array(N).fill(1);
  for (const e of events.filter((e) => e.voice === "kick" && e.vel > .6)) {
    const i0 = Math.round(e.time * SR);
    for (let i = 0; i < 6 * tau * SR && i0 + i < N; i++) d[i0 + i] = Math.min(d[i0 + i], 1 - depth * Math.exp(-i / (tau * SR)) * Math.min(1, i / 96));
  }
  return d;
};
{ const duck = kickDuck(.6, .035); for (let i = 0; i < N; i++) subBus[i] *= duck[i]; }

// ── the upright (@jeffrey 2026-11-22: "for the bass can we try sourcing real upright bass samples") ──
// A real double bass plays every sub note: pjcohen's plucked, let-ring set (Freesound, CC0), one take a
// semitone from E1 to E3, each retuned by its measured cents (assets/upright/SOURCES.json). A note is the
// pluck from its attack, held for its written length and let go over 90 ms; below E1 or above E3 the
// nearest take is resampled. It sits just left of centre and close, ducked a little under the kick.
const UPRIGHT = resolve(LANE, "assets/upright");
const uprightTakes = JSON.parse(readFileSync(resolve(UPRIGHT, "SOURCES.json"), "utf8")).sounds
  .filter((x) => x.file.startsWith("pjcohen/upright-pizz-"));
const pluckCache = new Map();
function pluck(midi) {
  if (pluckCache.has(midi)) return pluckCache.get(midi);
  const take = uprightTakes.reduce((a, b) => (Math.abs(b.midi - midi) < Math.abs(a.midi - midi) ? b : a));
  const raw = execFileSync("ffmpeg", ["-v", "error", "-i", resolve(UPRIGHT, take.file), "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
  let x = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2).slice();
  const top = peak(x) + 1e-9, on = Math.max(0, firstOver(x, .05 * top) - Math.round(.002 * SR));
  x = x.subarray(on);
  const rate = 2 ** ((midi - take.midi - (take.measuredCents ?? 0) / 100) / 12), n = Math.floor((x.length - 2) / rate), y = new Float32Array(n);
  for (let i = 0; i < n; i++) { const p = i * rate, j = p | 0, f = p - j; y[i] = x[j] * (1 - f) + x[j + 1] * f; }
  const pk = peak(y) + 1e-9;
  for (let i = 0; i < n; i++) y[i] /= pk;
  pluckCache.set(midi, y);
  return y;
}
const uprightBus = { L: new Float32Array(N), R: new Float32Array(N) };
for (const e of events.filter((e) => e.voice === "sub")) {
  const y = pluck(e.midi), i0 = Math.round(e.time * SR);
  const hold = Math.round(Math.min(1.8, Math.max(.09, e.len * STEP)) * SR), rel = .09 * SR;
  const pan = -.12 + .08 * Math.sin(e.step * .37), ear = earAt(uprightBus, pan);
  for (let i = 0; i < y.length && i < hold + 6 * rel && i0 + i < N; i++) {
    const v = e.vel * y[i] * (i < hold ? 1 : Math.exp(-(i - hold) / rel));
    ear(i0 + i, v);
  }
}
{ const duck = kickDuck(.35, .04); for (let i = 0; i < N; i++) { uprightBus.L[i] *= duck[i]; uprightBus.R[i] *= duck[i]; } }

// ── the high sines: pure, gliding, never clipped ──────────────────────
// Each line is one oscillator: notes that touch glide into each other.
// Each note has an attack, a decay to its sustain level and a release: the gate below is the shape the
// one-pole follower chases, so touching (legato) notes still re-articulate with a small bloom.
function sines(voice, { attack, release, glide, decay = .25, sustain = .6, bloom = 1 }) {
  const out = { L: new Float32Array(N), R: new Float32Array(N) }, lines = {};
  for (const e of events.filter((e) => e.voice === voice)) (lines[e.line] ??= []).push(e);
  Object.values(lines).forEach((notes, li) => {
    notes.sort((a, b) => a.time - b.time);
    const ons = notes.map((e) => Math.round(e.time * SR)), a0 = ons[0];
    const a1 = Math.min(N, ons.at(-1) + Math.round((notes.at(-1).len * STEP + 8 * release) * SR)), len = a1 - a0;
    const gA = new Float32Array(len), gM = new Float32Array(len);
    notes.forEach((e, k) => {
      const on = ons[k] - a0, gate = on + Math.round(e.len * STEP * .94 * SR), next = k + 1 < ons.length ? ons[k + 1] - a0 : len;
      const held = next - gate < .03 * SR ? next : Math.min(gate, next);   // touching notes are legato
      for (let i = on; i < next && i < len; i++) {
        gM[i] = e.midi;
        if (i < held) { const t = (i - on) / SR; gA[i] = e.vel * (sustain + (bloom - sustain) * Math.exp(-t / decay)); }
      }
    });
    const up = Math.exp(-1 / (attack * SR)), down = Math.exp(-1 / (release * SR)), slide = Math.exp(-1 / (glide * SR));
    const pan = voice === "pad" ? [-.55, .55, -.2, .25][li % 4] : stage("hi", notes[0].step, .3), ear = earAt(out, pan);
    let a = 0, m = gM[0], ph = 0, age = 0;
    for (let i = 0; i < len; i++) {
      a = gA[i] > a ? gA[i] + (a - gA[i]) * up : gA[i] + (a - gA[i]) * down;
      if (a < 1e-4) { m = gM[i]; age = 0; } else m = gM[i] + (m - gM[i]) * slide;
      age++;
      const vib = .05 * Math.min(1, Math.max(0, age / SR - .25) / .4) * Math.sin(2 * Math.PI * 5.1 * age / SR);
      ph += 2 * Math.PI * 440 * 2 ** ((m + vib - 69) / 12) / SR;
      if (ph > 2 * Math.PI) ph -= 2 * Math.PI;
      const v = a * Math.sin(ph);
      ear(a0 + i, v);
    }
  });
  return out;
}
// the line: a quick soft attack, a bloom that settles to 55 % over a quarter second, a 0.3 s tail
// two saws a few cents apart and a square an octave down, through a resonant low-pass whose cutoff
// opens on the attack and closes over 0.15 s: a plucky, warm mid voice. Attack 5 ms, decay to 50 %
// over 0.2 s, release 0.12 s; each note on its own, so runs stay articulate.
function midSynth() {
  const out = { L: new Float32Array(N), R: new Float32Array(N) };
  for (const e of events.filter((e) => e.voice === "mid")) {
    const hz = 440 * 2 ** ((e.midi - 69) / 12), i0 = Math.round(e.time * SR);
    const len = Math.max(.07, Math.min(e.len * STEP, .6)), n = Math.round(len * SR), tail = Math.round(.12 * 5 * SR);
    let low = 0, band = 0, p1 = (i0 * .618) % 1, p2 = (i0 * .382) % 1, p3 = 0;
    const pan = Math.max(-.95, Math.min(.95, stage("mid", e.step, -.2) + (e.midi - 67) / 60)), ear = earAt(out, pan);
    let level = 0;
    for (let i = 0; i < n + tail && i0 + i < N; i++) {
      const t = i / SR;
      const amp = i < n ? Math.min(1, t / .005) * (.5 + .5 * Math.exp(-t / .2)) : level * Math.exp(-(t - len) / .12);
      if (i < n) level = amp;
      p1 = (p1 + hz * 1.004 / SR) % 1; p2 = (p2 + hz / 1.004 / SR) % 1; p3 = (p3 + hz / 2 / SR) % 1;
      const x = (2 * p1 - 1) + (2 * p2 - 1) + .6 * (p3 < .5 ? 1 : -1);
      const fc = Math.min(.45 * SR, 380 + hz * 1.2 + (900 + 2600 * e.vel) * Math.exp(-t / .15)), f = 2 * Math.sin(Math.PI * fc / SR);
      const hp = x - low - .55 * band; band += f * hp; low += f * band;
      const v = amp * e.vel * Math.tanh(.6 * low);
      ear(i0 + i, v);
    }
  }
  return out;
}
const midBus = midSynth();
function clips(voice) {
const donkeyBus = { L: new Float32Array(N), R: new Float32Array(N) };
for (const e of events.filter((e) => e.voice === voice)) {
  let y;
  if (e.raw) {
    const raw = execFileSync("ffmpeg", ["-v", "error", "-i", e.raw, "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
    y = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2).slice();
    const pk = peak(y) + 1e-9; for (let i = 0; i < y.length; i++) y[i] /= pk;
  } else { const b = readFileSync(e.file); y = new Float32Array(b.buffer, b.byteOffset, b.byteLength >> 2).slice(); }
  const i0 = Math.round(e.time * SR), n = Math.min(y.length, Math.round(e.hold * SR)), fade = Math.round((voice === "donkey" || voice === "sheep" ? .05 : .008) * SR);
  if (e.to !== undefined) {                                   // a moving source: from e.pan to e.to across its length
    const ear = earMoving(donkeyBus);
    for (let i = 0; i < n && i0 + i < N; i++) ear(i0 + i, e.vel * y[i] * Math.min(1, (n - i) / fade), e.pan + (e.to - e.pan) * i / n);
  } else {
    const ear = earAt(donkeyBus, e.pan);
    for (let i = 0; i < n && i0 + i < N; i++) ear(i0 + i, e.vel * y[i] * Math.min(1, (n - i) / fade));
  }
}
return donkeyBus;
}
const rockBus = { L: new Float32Array(N), R: new Float32Array(N) }, rockWav = new Map();
const stereoWav = (name) => {
  const f = resolve(ROCK, `${name}.wav`);
  if (!existsSync(f)) return null;
  if (!rockWav.has(f)) {
    const raw = execFileSync("ffmpeg", ["-v", "error", "-i", f, "-ac", "2", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
    rockWav.set(f, new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2).slice());
  }
  return rockWav.get(f);
};
for (const e of events.filter((e) => e.voice === "rock")) {
  const i0 = Math.round(e.time * SR);
  for (const [y, g] of [[stereoWav(e.rock), 1], [e.room ? stereoWav(`${e.rock}-room`) : null, e.room]]) {
    if (!y) continue;
    for (let i = 0; 2 * i + 1 < y.length && i0 + i < N; i++) { rockBus.L[i0 + i] += e.vel * g * y[2 * i]; rockBus.R[i0 + i] += e.vel * g * y[2 * i + 1]; }
  }
}
// Karplus–Strong per string: a pick-shaped noise burst into a delay line, averaged as it goes round, with a
// small body (two resonances, nylon-string box). Each string is choked when it is struck again.
const ukeBus = { L: new Float32Array(N), R: new Float32Array(N) };
{
  let z = 5;
  const zr = () => ((z = (z * 1664525 + 1013904223) >>> 0) / 2 ** 32) * 2 - 1;
  const strings = [[], [], [], []];
  for (const e of events.filter((e) => e.voice === "uke").sort((a, b) => a.time - b.time)) if (!e.squeak) strings[e.string].push(e);
  strings.forEach((list, s) => list.forEach((e, k) => {
    const next = list[k + 1], hz = 440 * 2 ** ((e.midi - 69) / 12), P = Math.max(2, Math.round(SR / hz));
    const i0 = Math.round(e.time * SR), ring = e.mute ? .05 : Math.min(1.6, next ? next.time - e.time + .01 : 1.6), n = Math.round(ring * SR);
    const line = new Float32Array(P), bright = .35 + .5 * e.vel;
    let lp = 0;
    for (let j = 0; j < P; j++) { lp += bright * (zr() - lp); line[j] = lp; }
    const pan = [.42, .28, .34, .5][s], ear = earAt(ukeBus, pan);
    const loss = e.mute ? .93 : .9965;
    let prev = 0;
    for (let i = 0; i < n && i0 + i < N; i++) {
      const j = i % P, y = line[j];
      line[j] = loss * .5 * (y + prev); prev = y;
      const v = e.vel * y * Math.min(1, (n - i) / (.006 * SR));
      ear(i0 + i, v);
    }
    if (e.mute) for (let i = 0; i < .03 * SR && i0 + i < N; i++) { ear(i0 + i, .25 * e.vel * zr() * Math.exp(-i / (.006 * SR))); }
  }));
  for (const e of events.filter((e) => e.voice === "uke" && e.squeak)) {            // a finger sliding on a wound string
    const i0 = Math.round(e.time * SR), n = Math.round(.07 * SR);
    let bp = 0, lo = 0;
    for (let i = 0; i < n && i0 + i < N; i++) {
      const fc = 1800 + 2400 * i / n, f = 2 * Math.sin(Math.PI * fc / SR), x = zr();
      const hp = x - lo - .2 * bp; bp += f * hp; lo += f * bp;
      const v = .12 * e.vel * bp * Math.sin(Math.PI * i / n);
      ukeBus.L[i0 + i] += v * .8; ukeBus.R[i0 + i] += v;
    }
  }
  const body = (b, hz, q, g) => { for (const ch of [b.L, b.R]) { const w = 2 * Math.PI * hz / SR, a = Math.sin(w) / (2 * q), c = Math.cos(w);
    let x1 = 0, x2 = 0, y1 = 0, y2 = 0; const b0 = a, a0 = 1 + a, a1 = -2 * c, a2 = 1 - a;
    for (let i = 0; i < N; i++) { const x = ch[i], y = (b0 * x - b0 * x2 - a1 * y1 - a2 * y2) / a0; x2 = x1; x1 = x; y2 = y1; y1 = y; ch[i] = x + g * y; } } };
  body(ukeBus, 240, 3, .6); body(ukeBus, 520, 4, .35);
}
const brushBus = clips("brush"), whooshBus = clips("whoosh"), sheepBus = clips("sheep"), donkeyBus = clips("donkey"), springBus = clips("spring"), mouseBus = clips("mouse");
const implodeBus = { L: new Float32Array(N), R: new Float32Array(N) }, bulletBus = { L: new Float32Array(N), R: new Float32Array(N) };
{
  let z = 77;
  const zr = () => ((z = (z * 1664525 + 1013904223) >>> 0) / 2 ** 32) * 2 - 1;
  const grenade = (() => { const raw = execFileSync("ffmpeg", ["-v", "error", "-i", resolve(GUNS, "grenade-explosion-01.mp3"), "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
    const x = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2).slice(), pk = peak(x) + 1e-9; for (let i = 0; i < x.length; i++) x[i] /= pk; return x; })();
  for (const e of events.filter((e) => e.voice === "implode")) {
    const T = Math.round(e.time * SR), D = Math.round(e.dur * SR), i0 = T - D;
    const left = earMoving(implodeBus), right = earMoving(implodeBus), mid = earAt(implodeBus, 0);
    let lp1 = 0, lp2 = 0, ph = 0;
    for (let i = 0; i < D; i++) {
      const u = i / D, fc = 250 * 32 ** u, a = 1 - Math.exp(-2 * Math.PI * fc / SR), amp = u ** 3;
      lp1 += a * (zr() - lp1); lp2 += a * (zr() - lp2);
      const back = D - i, g = back < grenade.length ? .9 * grenade[back] * u ** 1.5 : 0;      // the grenade, backwards, landing on T
      const wide = (1 - u) ** .8 * .95;
      if (e.kind === "spiral") {
        const turn = 2 * Math.PI * 2.5 * (1 - u) ** 1.4, p = Math.sin(turn) * wide;
        left(i0 + i, amp * lp1 + g, p); right(i0 + i, amp * lp2, -p);
      } else { left(i0 + i, amp * lp1 + .5 * g, -wide); right(i0 + i, amp * lp2 + .5 * g, wide); }
      ph += 2 * Math.PI * (28 * (80 / 28) ** u) / SR;
      mid(i0 + i, .5 * u * u * Math.sin(ph));
    }
    for (let i = 0; i < .12 * SR; i++) mid(T + i, (i < 96 ? .8 * zr() * (1 - i / 96) : 0) + Math.sin(2 * Math.PI * 55 * i / SR) * Math.exp(-i / (.05 * SR)));
  }
  for (const e of events.filter((e) => e.voice === "bullet")) {
    const t0 = Math.round(e.time * SR), ear = earMoving(bulletBus);
    if (e.kind === "zip") {
      // a straight line past the head: x from −14 m to +14 m at 260 m/s, closest at miss metres (at t0)
      const v = 260, dur = 28 / v, n = Math.round(dur * SR), c = 343;
      let lp = 0, bp = 0, ph = 0;
      for (let i = 0; i < n; i++) {
        const tt = i / SR - dur / 2, x = v * tt, r = Math.hypot(x, e.miss), az = Math.atan2(x, e.miss) / (Math.PI / 2);
        const pan = e.from + (e.to - e.from) * (az + 1) / 2, doppler = c / (c + v * x / r), amp = Math.min(3, 1 / r) * e.vel;
        const f = e.pitch * doppler, w = 2 * Math.sin(Math.PI * Math.min(f, 9000) / SR), nz = zr();
        const hp = nz - lp - .35 * bp; bp += w * hp; lp += w * bp;                  // a whistle in the air around the round
        ph += 2 * Math.PI * f * .55 / SR;
        ear(t0 - Math.round(n / 2) + i, amp * (.55 * bp + .12 * Math.sin(ph)), pan);
      }
      const crack = Math.round(.0003 * SR);                                             // the shock wave: an N-wave at closest approach
      for (let i = 0; i < 2 * crack; i++) ear(t0 + i, e.vel * (1.4 / e.miss) * (1 - i / crack), (e.from + e.to) / 2);
    } else {
      const ping = [3100 + 400 * zr(), 4700 + 500 * zr()];
      for (let i = 0; i < .04 * SR; i++) {
        const t = i / SR, v = e.vel * ((i < .004 * SR ? .9 * zr() : 0) + .35 * Math.exp(-t / .012) * (Math.sin(2 * Math.PI * ping[0] * t) + .7 * Math.sin(2 * Math.PI * ping[1] * t)));
        ear(t0 + i, v, e.from);
      }
      const dur = .45 + .25 * Math.abs(zr()), n = Math.round(dur * SR), spin = 26 + 14 * Math.abs(zr());
      let ph = 0, lp = 0;
      for (let i = 0; i < n; i++) {
        const u = i / n, f = e.pitch * (1 - .5 * u), amp = e.vel * .7 * Math.exp(-u * 3) * (.6 + .4 * Math.sin(2 * Math.PI * spin * i / SR));
        ph += 2 * Math.PI * f / SR;
        const a = 1 - Math.exp(-2 * Math.PI * (9000 - 6500 * u) / SR); lp += a * (Math.sin(ph) + .25 * zr() - lp);
        ear(t0 + Math.round(.012 * SR) + i, amp * lp, e.from + (e.to - e.from) * u ** .7);
      }
    }
  }
}
const popBus = { L: new Float32Array(N), R: new Float32Array(N) };
for (const e of events.filter((e) => e.voice === "pop")) {
  const f0 = 440 * 2 ** ((e.midi - 69) / 12), i0 = Math.round(e.time * SR), n = Math.round(.06 * SR);
  const ear = earAt(popBus, e.pan);
  let ph = 0;
  for (let i = 0; i < n && i0 + i < N; i++) {
    const t = i / SR, hz = f0 * 1.5 ** Math.min(1, t / .018);
    ph += 2 * Math.PI * hz / SR;
    const v = e.vel * (Math.min(1, t / .0008) * Math.exp(-t / .016) * Math.sin(ph) + (i < 24 ? .5 * (1 - i / 24) * (i % 2 ? 1 : -1) : 0));
    ear(i0 + i, v);
  }
}
const hiBus = sines("hi", { attack: .018, release: .3, glide: .025, decay: .22, sustain: .55 });
// the chords: swell in over a third of a second, settle slowly, ring out for over a second
const padBus = sines("pad", { attack: .32, release: 1.2, glide: .12, decay: 1.4, sustain: .72 });

// ── FEM bells from pop/bell, rendered once and kept in out/.bell-cache ─
const BELL_KIND = { handbell: ["bronze", "handbell", 5], glass: ["glass", "glass", 7], church: ["bronze", "church", 12],
  deep: ["bronze", "church", 9], bowl: ["bronze", "bowl", 9] };
const bellSample = new Map();
function bellOf(midi, kind) {
  const [material, geometry, dur] = BELL_KIND[kind], note = NAMES[mod(midi, 12)].replace("Bb", "A#") + (Math.floor(midi / 12) - 1);
  const file = resolve(BELLS, `${note.replace("#", "s")}-${material}-${geometry}-${dur}.wav`);
  if (bellSample.has(file)) return bellSample.get(file);
  if (!existsSync(file)) {
    mkdirSync(BELLS, { recursive: true });
    const r = spawnSync(BELL, ["--note", note, "--material", material, "--geometry", geometry, "--dur", String(dur), "--out", file]);
    if (r.status) throw new Error(`bell: ${note} ${kind} failed`);
  }
  const raw = execFileSync("ffmpeg", ["-v", "error", "-i", file, "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
  const y = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2).slice(), pk = peak(y) + 1e-9;
  for (let i = 0; i < y.length; i++) y[i] /= pk;
  bellSample.set(file, y);
  return y;
}
const bellBus = { L: new Float32Array(N), R: new Float32Array(N) }, gongBus = { L: new Float32Array(N), R: new Float32Array(N) };
events.filter((e) => e.voice === "bell").forEach((e, j) => {
  const y = bellOf(e.midi, e.kind), b = e.kind === "church" || e.kind === "bowl" ? gongBus : bellBus;
  const pan = e.kind === "church" ? -.15 : e.kind === "glass" ? [-.6, 0, .6][j % 3] : e.kind === "deep" ? [-.2, .2][j % 2] : e.kind === "bowl" ? 0 : [-.35, .35][j % 2];
  e.pan = pan;
  const ear = earAt(b, pan), i0 = Math.round(e.time * SR);
  for (let i = 0; i < y.length && i0 + i < N; i++) ear(i0 + i, y[i] * e.vel);
});

// ── explosions: guns layered, a sub drop, far off or crossing ─────────
const boomBus = { L: new Float32Array(N), R: new Float32Array(N) }, boomSendL = new Float32Array(N), boomSendR = new Float32Array(N), drop = new Float32Array(N);
const stereoFile = new Map();
const whole = (gun) => { if (!stereoFile.has(gun)) { const x = decode(gun), pk = peak(x) + 1e-9; for (let i = 0; i < x.length; i++) x[i] /= pk; stereoFile.set(gun, x); } return stereoFile.get(gun); };
for (const e of events.filter((e) => e.voice === "boom")) {
  const { pan, travel = 0, dist, layers, drop: sub } = e.boom, [p0, p1] = Array.isArray(pan) ? pan : [pan, pan];
  const len = Math.min(6.5 * SR, Math.max(...layers.map((g) => whole(g).length)));
  let src = new Float32Array(len);
  for (const g of layers) { const x = whole(g), w = g.startsWith("distant") ? .7 : 1; for (let i = 0; i < Math.min(len, x.length); i++) src[i] += w * x[i]; }
  src = lowpass(src, 14000 / (1 + dist / 3));                  // air takes the top off with distance
  const g = e.vel * (dist / 4) ** -.35, i0 = Math.round(e.time * SR);
  for (let i = 0; i < len && i0 + i < N; i++) {
    const f = travel ? Math.min(1, i / (travel * SR)) : 0, p = p0 + (p1 - p0) * f * f * (3 - 2 * f), th = (p + 1) * Math.PI / 4;
    const v = src[i] * g;
    boomBus.L[i0 + i] += v * Math.cos(th); boomBus.R[i0 + i] += v * Math.sin(th);
    boomSendL[i0 + i] += v * (.6 + .4 * Math.cos(th)); boomSendR[i0 + i] += v * (.6 + .4 * Math.sin(th));
  }
  if (sub) for (let i = 0, ph = 0; i < 2 * SR && i0 + i < N; i++) {
    const t = i / SR, hz = 30 + 60 * Math.exp(-t / .35);
    ph += 2 * Math.PI * hz / SR;
    drop[i0 + i] += e.vel * Math.min(1, i / 200) * Math.exp(-t / .55) * Math.sin(ph);
  }
}

// ── balance: every bus to its level, measured where it is playing ─────
const dB = (v) => +(20 * Math.log10(v + 1e-12)).toFixed(1);
function loudPart(L, R) {
  const B = 2400, blocks = [];
  for (let a = 0; a + B <= N; a += B) { let s = 0; for (let i = a; i < a + B; i++) s += L[i] * L[i] + (R ? R[i] * R[i] : 0); blocks.push(s / B); }
  const top = Math.max(...blocks), on = blocks.filter((b) => b > top * .01);
  return Math.sqrt(on.reduce((a, b) => a + b, 0) / on.length);
}
const soft = (v, knee = .5) => { const a = Math.abs(v); return a < knee ? v : Math.sign(v) * (knee + (1 - knee) * Math.tanh((a - knee) / (1 - knee))); };
const levels = {};
for (const [v, b] of Object.entries(bus)) {
  const g = 10 ** (KIT[v].level / 20) / loudPart(b.L, b.R);
  for (let i = 0; i < N; i++) { b.L[i] = soft(b.L[i] * g, .75); b.R[i] = soft(b.R[i] * g, .75); }
  levels[v] = { gainDb: dB(g) };
}
{ const g = 10 ** (SUB_LEVEL / 20) / loudPart(subBus); for (let i = 0; i < N; i++) subBus[i] = soft(subBus[i] * g, .7); levels.sub = { gainDb: dB(g) }; }
// the clean layers are set against the driven mix, and are never clipped
const CLEAN = { brush: [brushBus, -23], whoosh: [whooshBus, -21], implode: [implodeBus, -16], bullet: [bulletBus, -21], uke: [ukeBus, -21], sheep: [sheepBus, -17], rock: [rockBus, -16], spring: [springBus, -22], mouse: [mouseBus, -27], donkey: [donkeyBus, -19], upright: [uprightBus, -15.5], pop: [popBus, -24], mid: [midBus, -21], hi: [hiBus, -20], pad: [padBus, -27], bell: [bellBus, -21], gong: [gongBus, -20], boom: [boomBus, -17] };
const scale = (b, to) => { const g = 10 ** (to / 20) / loudPart(b.L, b.R); for (let i = 0; i < N; i++) { b.L[i] *= g; b.R[i] *= g; } return dB(g); };
for (const [v, [b, to]] of Object.entries(CLEAN)) levels[v] = { gainDb: scale(b, to), loudRms: to };
{ const g = 10 ** (-17 / 20) / loudPart(boomSendL, boomSendR); for (let i = 0; i < N; i++) { boomSendL[i] *= g; boomSendR[i] *= g; } }
{ const g = 10 ** (-19 / 20) / loudPart(drop); for (let i = 0; i < N; i++) drop[i] *= g; levels.drop = { gainDb: dB(g), loudRms: -19 }; }

// ── the space ─────────────────────────────────────────────────────────
// A room for the guns (as version 1) and a long hall for everything far off.
function room(send, seed) {
  let s = seed;
  const rnd = () => ((s = (s * 1664525 + 1013904223) >>> 0) / 2 ** 32), out = new Float32Array(N);
  for (let k = 0; k < 36; k++) {
    const ms = 8 + 240 * rnd() ** 1.6, d = Math.round(ms * SR / 1000), g = (rnd() < .5 ? -1 : 1) * .5 * 10 ** (-3 * ms / 260);
    for (let i = d; i < N; i++) out[i] += send[i - d] * g;
  }
  return lowpass(out, 6000);
}
// eight delay lines through a Hadamard matrix, darker as they ring
function hall(inL, inR, { rt60, damp, size = 1 }) {
  const lens = [1931, 2213, 2617, 2903, 3329, 3761, 4127, 4549].map((n) => Math.round(n * size));
  const line = lens.map((n) => new Float32Array(n)), at = new Int32Array(8), lp = new Float64Array(8), h = new Float64Array(8);
  const fb = lens.map((n) => 10 ** (-3 * n / SR / rt60)), a = Math.exp(-2 * Math.PI * damp / SR);
  const L = new Float32Array(N), R = new Float32Array(N);
  for (let i = 0; i < N; i++) {
    for (let k = 0; k < 8; k++) { const y = line[k][at[k]]; lp[k] = (1 - a) * y + a * lp[k]; h[k] = lp[k]; }
    L[i] = (h[0] - h[2] + h[4] - h[6]) * .5; R[i] = (h[1] - h[3] + h[5] - h[7]) * .5;
    for (let s = 1; s < 8; s <<= 1) for (let k = 0; k < 8; k++) if (!(k & s)) { const x = h[k], y = h[k + s]; h[k] = x + y; h[k + s] = x - y; }
    for (let k = 0; k < 8; k++) { line[k][at[k]] = h[k] * fb[k] / Math.sqrt(8) + (k & 1 ? inR[i] : inL[i]) * .3; if (++at[k] === lens[k]) at[k] = 0; }
  }
  return { L, R };
}
const onePole = (x, hz) => { const a = Math.exp(-2 * Math.PI * hz / SR); for (let i = 0, s = 0; i < N; i++) x[i] = s = (1 - a) * x[i] + a * s; };
// Place a stereo bus at an azimuth and a distance: the direct sound loses
// level and top with distance, six early reflections (near wall, then far
// wall) arrive sooner after it the farther it is, and the sends get a pre-
// delay that shrinks with distance.
const ER = [4.5, 7.5, 11, 15, 21, 29];
const roomSend = new Float32Array(N), hallL = new Float32Array(N), hallR = new Float32Array(N), placed = {};
function place(name, b, { pan, dist, wet = 0, far = 0 }) {
  if (dist > 0) {
    const g = Math.min(1.1, (dist / 1.5) ** -.35), fc = 18000 / (1 + dist / 2.5);
    onePole(b.L, fc); onePole(b.R, fc);
    const mono = new Float32Array(N);
    for (let i = 0; i < N; i++) { b.L[i] *= g; b.R[i] *= g; mono[i] = .5 * (b.L[i] + b.R[i]); }
    onePole(mono, 5000);
    const [near, away] = pan >= 0 ? [b.R, b.L] : [b.L, b.R];
    ER.forEach((ms, k) => {
      const d = Math.round(ms / (1 + dist / 5) * SR / 1000), gk = .3 * .8 ** k * dist / (dist + 2), side = k % 2 ? away : near;
      for (let i = N - 1; i >= d; i--) side[i] += mono[i - d] * gk;
    });
  }
  const pd = Math.round(Math.max(2, 22 - 3 * dist) * SR / 1000), sendW = wet * (.6 + .25 * dist);
  for (let i = 0; i + pd < N; i++) {
    roomSend[i + pd] += (b.L[i] + b.R[i]) * .7 * sendW;
    hallL[i + pd] += b.L[i] * far; hallR[i + pd] += b.R[i] * far;
  }
  placed[name] = { pan, dist, wet: +sendW.toFixed(3), hall: far, preDelayMs: +(pd / SR * 1000).toFixed(1) };
}
const L = new Float32Array(N), R = new Float32Array(N);
for (const [v, b] of Object.entries(bus)) {
  if (v !== "kick") place(v, b, { pan: KIT[v].pan, dist: KIT[v].dist, wet: KIT[v].wet, far: .015 * Math.max(0, KIT[v].dist - 2) });
  for (let i = 0; i < N; i++) { L[i] += b.L[i]; R[i] += b.R[i]; }
}
placed.kick = { pan: 0, dist: 0, dry: true }; placed.sub = { pan: 0, dist: 0, dry: true };
const wetL = room(roomSend, 7), wetR = room(roomSend, 1311);
for (let i = 0; i < N; i++) { const s = subBus[i] * Math.SQRT1_2; L[i] += wetL[i] + s; R[i] += wetR[i] + s; }

// ── the dynamics (@jeffrey 2026-11-22: "better amplitude narratives with pianissimo sections") ──
// One level curve over the whole suite, in dB, by movement and bar (pp −14 · p −9 · mp −5 · mf −2 ·
// f 0 · ff +1). It is applied before the drive, so the quiet passages are quiet and clean and the
// loud ones lean into the knee; the explosions are left out of it. Breaks a tenth of a bar long
// are steps; the rest are hairpins.
const DYNAMICS = [
  [[1, -13], [3, -9], [7, -5], [9, -1], [15, 0], [18.9, -2]],                                   // I: alto alone, then the break
  [[1, -7], [4.9, -5], [5, -6], [8.9, -4]],                                                     // II: the courante, lighter
  [[1, -14], [5, -9], [8.9, -3], [9, 1], [16.9, 0]],                                            // III: pp until the drop
  [[1, -14], [5.9, -12], [6, -9], [11, -5], [16, -2], [20.9, -1], [21, -14], [25.9, -2], [26, 1], [30.9, 0]],   // IV
  [[1, -4], [8.9, -1], [9, -12], [12.9, -1], [13, 1], [18.9, 0], [19, -2], [21, -5]],          // V: hush before the drop
];
const dynSteps = [], dynDb = [];
// (@jeffrey 2026-11-22: "keep the overall volume … relatively equalized": the curve keeps its shape at
// GUNDRUM_DYN of its depth, so pp is about −6 dB, not −14)
const DEPTH = Number(process.env.GUNDRUM_DYN ?? .45);
movements.forEach((m, k) => (DYNAMICS[k] ?? [[1, 0]]).forEach(([bar, db]) => { dynSteps.push(m.start + (bar - 1) * m.per); dynDb.push(db * DEPTH); }));
const DYN = new Float32Array(N);
for (let i = 0, j = 0; i < N; i++) {
  const step = i / SR / STEP;
  while (j < dynSteps.length - 1 && dynSteps[j + 1] <= step) j++;
  const db = step <= dynSteps[0] ? dynDb[0] : j >= dynSteps.length - 1 ? dynDb.at(-1)
    : lerp(dynDb[j], dynDb[j + 1], (step - dynSteps[j]) / (dynSteps[j + 1] - dynSteps[j]));
  DYN[i] = 10 ** (db / 20);
}
for (let i = 0; i < N; i++) { L[i] *= DYN[i]; R[i] *= DYN[i]; }

// ── master: the guns drive into a soft knee; the clean layers join after ──
const DRIVE = Number(process.env.GUNDRUM_DRIVE ?? 1.8), CEIL = 10 ** (-1.5 / 20), LOOK = 240;
let kneed = 0, sounding = 0;
for (let i = 0; i < N; i++) {
  const l = L[i] * DRIVE, r = R[i] * DRIVE, top = Math.max(Math.abs(l), Math.abs(r));
  if (top > .01) sounding++; if (top > .5) kneed++;
  L[i] = soft(l); R[i] = soft(r);
}
place("brush", brushBus, { pan: 0, dist: 1.6, wet: .15, far: .05 });
place("whoosh", whooshBus, { pan: 0, dist: 0, wet: .2, far: .1 });
place("implode", implodeBus, { pan: 0, dist: 0, wet: .2, far: .25 });
place("bullet", bulletBus, { pan: 0, dist: 0, wet: .25, far: .12 });
place("uke", ukeBus, { pan: .38, dist: 1.8, wet: .18, far: .06 });
place("sheep", sheepBus, { pan: 0, dist: 4, wet: .25, far: .3 });
place("rock", rockBus, { pan: 0, dist: 0, wet: .1, far: .04 });
place("spring", springBus, { pan: 0, dist: 2.5, wet: .3, far: .15 });
place("mouse", mouseBus, { pan: 0, dist: 1.5, wet: .15, far: .05 });
place("donkey", donkeyBus, { pan: .3, dist: 3.5, wet: .2, far: .25 });
place("upright", uprightBus, { pan: -.12, dist: 1.2, wet: .12, far: .02 });
place("pop", popBus, { pan: 0, dist: 1, wet: .3, far: .05 });
place("mid", midBus, { pan: -.2, dist: 2.2, wet: .25, far: .12 });
place("hi", hiBus, { pan: .3, dist: 3, far: .2 });
place("pad", padBus, { pan: 0, dist: 6, far: .35 });
place("bell", bellBus, { pan: 0, dist: 3, far: .3 });
place("gong", gongBus, { pan: -.15, dist: 7, far: .5 });
const space = hall(hallL, hallR, { rt60: 3.2, damp: 5000 });
const boomHall = hall(boomSendL, boomSendR, { rt60: 5.5, damp: 3200, size: 1.35 });
placed.boom = events.filter((e) => e.voice === "boom").map((e) => ({ at: +e.time.toFixed(2), ...e.boom }));
const boomDuck = kickDuck(.72, .08);
for (let i = 0; i < N; i++) {
  const d = boomDuck[i], dm = drop[i] * d * Math.SQRT1_2;
  const g = DYN[i];
  L[i] += (brushBus.L[i] + whooshBus.L[i] + implodeBus.L[i] + bulletBus.L[i] + ukeBus.L[i] + sheepBus.L[i] + rockBus.L[i] + springBus.L[i] + mouseBus.L[i] + donkeyBus.L[i] + uprightBus.L[i] + popBus.L[i] + midBus.L[i] + hiBus.L[i] + padBus.L[i] + bellBus.L[i] + gongBus.L[i] + space.L[i]) * g + (boomBus.L[i] + .5 * boomHall.L[i]) * d + dm;
  R[i] += (brushBus.R[i] + whooshBus.R[i] + implodeBus.R[i] + bulletBus.R[i] + ukeBus.R[i] + sheepBus.R[i] + rockBus.R[i] + springBus.R[i] + mouseBus.R[i] + donkeyBus.R[i] + uprightBus.R[i] + popBus.R[i] + midBus.R[i] + hiBus.R[i] + padBus.R[i] + bellBus.R[i] + gongBus.R[i] + space.R[i]) * g + (boomBus.R[i] + .5 * boomHall.R[i]) * d + dm;
}
// ── a slow leveller before the limiter: the level over 0.4 s is drawn halfway toward the track's own
// loud-part level (±6 dB at most), easing over about a second, so a passage doesn't swell or sag and the
// limiter only catches peaks. (The limiter was taking up to 17 dB off loud hits and the level fell after
// every kick and bass note: @jeffrey heard "a weird attenuation" at 0:38.)
{
  const win = Math.round(.4 * SR), hop = Math.round(.05 * SR), lev = [];
  for (let i = 0; i < N; i += hop) {
    let e = 0; const a = Math.max(0, i - win / 2), b = Math.min(N, i + win / 2);
    for (let j = a; j < b; j += 4) e += L[j] * L[j] + R[j] * R[j];
    lev.push(Math.sqrt(e / Math.max(1, (b - a) / 4) / 2) + 1e-9);
  }
  const loud = [...lev].sort((x, y) => x - y)[Math.floor(lev.length * .7)];
  let g = 1;
  const gains = lev.map((v) => { const want = Math.min(2, Math.max(.5, (loud / v) ** .5)); g += (want - g) * (want < g ? .25 : .05); return g; });
  for (let i = 0; i < N; i++) { const k = i / hop, j = Math.min(gains.length - 1, Math.floor(k)), f = Math.min(1, k - j);
    const gg = gains[j] * (1 - f) + (gains[j + 1] ?? gains[j]) * f; L[i] *= gg; R[i] *= gg; }
}
const preL = L.slice(), preR = R.slice();

// a look-ahead limiter that also watches the peaks between samples
const SINC = [.25, .5, .75].map((f) => Array.from({ length: 16 }, (_, k) => { const x = k - 7 - f, w = .5 + .5 * Math.cos(Math.PI * x / 8); return Math.sin(Math.PI * x) / (Math.PI * x) * w; }));
const between = (x, i) => {
  let p = Math.abs(x[i]);
  if (p < .3 || i < 8 || i > N - 9) return p;
  for (const taps of SINC) { let v = 0; for (let k = 0; k < 16; k++) v += x[i - 7 + k] * taps[k]; p = Math.max(p, Math.abs(v)); }
  return p;
};
const need = new Float32Array(N), ahead = new Float32Array(N), gr = new Float32Array(N), fadeN = Math.round(1.5 * SR);
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

// ── write, measure, and trim the make-up gain toward −10 LUFS ─────────
mkdirSync(OUT, { recursive: true });
const base = resolve(OUT, "gundrum-and-bass-2"), WAV = base + ".wav", MP3 = base + ".mp3";
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
const TARGET = Number(process.env.GUNDRUM_LUFS ?? -11), passes = [];
let makeup = 1;
for (let pass = 0; pass < 5; pass++) {
  limit(makeup); writeWav();
  const got = loudness(WAV);
  passes.push({ makeupDb: dB(makeup), ...got });
  if (Math.abs(got.lufs - TARGET) < .15) break;
  makeup *= 10 ** (1.5 * (TARGET - got.lufs) / 20);   // the limiter gives back about a third of each step
}
execFileSync("ffmpeg", ["-v", "error", "-y", "-i", WAV, "-af", "volume=-1dB", "-c:a", "libmp3lame", "-b:a", "320k", MP3]);
const reduced = Array.from(gr.subarray(0, Math.round(END * STEP * SR))).map((g) => -20 * Math.log10(g)).sort((a, b) => a - b);
const limiter = {
  drive: DRIVE, softKneeShare: +(kneed / sounding).toFixed(3), makeupDb: dB(makeup), passes,
  reductionDb: { mean: +(reduced.reduce((a, b) => a + b, 0) / reduced.length).toFixed(2), median: +reduced[reduced.length >> 1].toFixed(2), p95: +reduced[Math.floor(reduced.length * .95)].toFixed(2), max: +reduced.at(-1).toFixed(2) },
  shareOver1dB: +(reduced.filter((g) => g > 1).length / reduced.length).toFixed(3), shareOver3dB: +(reduced.filter((g) => g > 3).length / reduced.length).toFixed(3),
};

// the hand, as played: how far each voice sits from the grid and how its loudness moved
const feel = Object.fromEntries(Object.keys(FEEL).map((v) => {
  const mine = events.filter((e) => e.voice === v), late = mine.map((e) => e.late).sort((a, b) => a - b), ch = mine.map((e) => e.vel / e.score);
  return [v, { hits: mine.length, lateMs: { min: late[0], median: late[late.length >> 1], max: late.at(-1) }, velocityChange: `${dB(Math.min(...ch))}…+${dB(Math.max(...ch))} dB` }];
}));

// ── measure (nothing is played) ───────────────────────────────────────
for (const v of [...Object.keys(KIT), "sub"]) {
  const mine = events.filter((e) => e.voice === v), b = v === "sub" ? { L: subBus, R: null } : bus[v];
  let pk = 0; for (let i = 0; i < N; i++) pk = Math.max(pk, Math.abs(b.L[i]), b.R ? Math.abs(b.R[i]) : 0);
  const vels = mine.map((e) => e.vel), ms = mine.map((e) => e.midi), off = [];
  if (b.R && v !== "orn") mine.forEach((e, i) => {                 // attacks out of near-silence, against their scheduled time
    if (i && e.time - mine[i - 1].time < .25) return;
    const c = Math.round(e.time * SR), a = Math.max(0, c - 480), z = Math.min(N, c + 960);
    let top = 0; for (let j = a; j < z; j++) top = Math.max(top, Math.abs(b.L[j]) + Math.abs(b.R[j]));
    for (let j = a; j < z; j++) if (Math.abs(b.L[j]) + Math.abs(b.R[j]) > .12 * top) { off.push(Math.abs(j - c) / SR * 1000); break; }
  });
  Object.assign(levels[v], {
    hits: mine.length, loudRms: dB(loudPart(b.L, b.R) / (b.R ? Math.SQRT2 : 1)), peak: dB(pk),
    range: `${name(Math.min(...ms))}–${name(Math.max(...ms))}`, semitones: Math.max(...ms) - Math.min(...ms),
    transposed: v === "sub" ? "synth" : `${Math.min(...mine.map((e) => e.semis))}…+${Math.max(...mine.map((e) => e.semis))}`,
    timed: off.length, worstMs: +Math.max(0, ...off).toFixed(2),
    velocity: `${Math.min(...vels).toFixed(2)}–${Math.max(...vels).toFixed(2)}`, dynamicsDb: dB(Math.max(...vels) / Math.min(...vels)),
  });
}
// every attack found in the drum mix against the nearest scheduled hit (the tuplets are off the grid by design)
function timing() {
  const dl = new Float32Array(N);
  for (const [v, b] of Object.entries(bus)) if (v !== "orn") for (let i = 0; i < N; i++) dl[i] += Math.abs(b.L[i]) + Math.abs(b.R[i]);
  const times = events.filter((e) => e.voice in KIT && e.voice !== "orn").map((e) => e.time).sort((a, b) => a - b);
  const hop = 48, n = Math.floor(N / hop), env = new Float32Array(n), errs = [];
  for (let i = 0; i < n; i++) { let p = 0; for (let j = 0; j < hop; j++) p = Math.max(p, dl[i * hop + j]); env[i] = p; }
  for (let i = 12, k = 0; i < n; i++) {
    let before = 0; for (let j = i - 12; j < i - 1; j++) before = Math.max(before, env[j]);
    if (env[i] > .08 && env[i] > 2.2 * before) {
      const t = i * hop / SR;
      while (k < times.length - 1 && Math.abs(times[k + 1] - t) <= Math.abs(times[k] - t)) k++;
      errs.push(Math.abs(times[k] - t) * 1000); i += 16;
    }
  }
  errs.sort((a, b) => a - b);
  return { attacks: errs.length, medianMs: +errs[errs.length >> 1].toFixed(2), within3ms: +(errs.filter((e) => e <= 3).length / errs.length).toFixed(3), worstMs: +errs.at(-1).toFixed(2) };
}
const boxes = Object.fromEntries(Object.entries(PATTERN).map(([k, p]) => [k, { ...p, kickSyncopationLhl: syncopationLhl(rhythm(p.kick.replace(/[Xxo]/g, "x"))) }]));
const report = {
  title: "gundrum and bass 2", bpm: BPM, key: "D minor", seconds: +(N / SR).toFixed(2),
  subject: { rhythm: toBox(rhythm(SUBJECT.map((n) => n.t), LEN)), notes: SUBJECT.map((n) => name(midiOf(n.d))).join(" ") },
  countersubject: { rhythm: toBox(rhythm(HOLES, LEN)), firstSpecies: CS.ok, interlocks: interlocks(rhythm(SUBJECT.map((n) => n.t), LEN), rhythm(HOLES, LEN)) },
  movements: movements.map((m) => ({
    title: m.title, bars: m.bars, metre: m.per === 24 ? "6/4" : "4/4", from: +(m.start * STEP).toFixed(1), seconds: +(m.bars * m.per * STEP).toFixed(1),
    device: m.device, crossRhythm: m.cross, checks: m.checks, entries: m.entries.sort((a, b) => a.bar - b.bar),
  })),
  patterns: boxes, levels, feel, swing: SWING, space: placed, timing: timing(), limiter, loudness: { wav: loudness(WAV), mp3: loudness(MP3) },
};
writeFileSync(base + ".json", JSON.stringify({ ...report, kit: KIT, barStarts: bars.map((s) => +(s * STEP).toFixed(4)), events }, null, 1));

// ── the timeline ──────────────────────────────────────────────────────
const plot = String.raw`
import json, sys
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
d = json.load(open(sys.argv[1])); ev = d["events"]
INK, MUTE, GRID, PAPER, GREY = "#0b0b0b", "#52514e", "#e4e3df", "#fcfcfb", "#8f8e88"
HUE = {"subject": "#2a78d6", "countersubject": "#1baf7a", "inversion": "#eda100", "augmentation": "#e87ba4",
       "canon": "#008300", "retrograde": "#6250d6", "fragment": "#e34948", "ground": "#eb6834", "bell": "#b07d10", "boom": "#8b1e3f"}
ALIAS = {"division": "countersubject", "cadence": "subject"}
LANES = ["whoosh", "brush", "bullet", "implode", "uke", "sheep", "rock", "mouse", "spring", "donkey", "pop", "hi", "mid", "pad", "bell", "S", "A", "T", "sub", "snare", "ghost", "kick", "hat", "shot", "orn", "boom"]
SAY = {"brush": "brushes,\nbroom", "whoosh": "whooshes,\ncloth flips", "implode": "implosions", "bullet": "ricochets,\nbullets past", "uke": "ukulele\n(simulated)", "sheep": "sheep\n(aesthetivox)", "rock": "rock kit\n(DrumGizmo)", "spring": "springs", "mouse": "mice", "donkey": "donkey\n(aesthetivox)", "upright": "upright bass\n(pjcohen, CC0)", "pop": "bubble pops", "shot": "shotgun\n12 gauge", "mid": "mid synth\n(tenor line)", "hi": "high sine", "pad": "sine chords", "bell": "bells, gong\n(FEM)", "boom": "explosions", "S": "soprano\n.30-06", "A": "alto\n9mm pistol", "T": "tenor\nAK-47", "sub": "sub bass\n(synth)", "snare": "snare\n12 gauge",
       "ghost": "ghosts, rolls\n9mm SMG", "kick": "kick .45", "hat": "hats\ndry fire", "orn": "racks, brass"}
TALL = {"whoosh": .8, "brush": .8, "bullet": .8, "implode": .8, "uke": 1.4, "sheep": 1.4, "rock": .8, "mouse": .8, "spring": .8, "donkey": 1.4, "pop": 1.4, "shot": .8, "mid": 2.4, "hi": 2.4, "pad": 1.4, "bell": 1.6, "boom": .8, "S": 3, "A": 3, "T": 3, "sub": 3, "snare": .8, "ghost": 2, "kick": .8, "hat": 1.4, "orn": .7}
base, y = {}, 0
for v in reversed(LANES): base[v] = y; y += TALL[v] + .4
span = {v: (min(e["midi"] for e in ev if e["voice"] == v), max(e["midi"] for e in ev if e["voice"] == v)) for v in LANES}
total = d["seconds"]; cuts = [0, 47.5, 95, total]
fig, axes = plt.subplots(3, 1, figsize=(20, 21), dpi=105, facecolor=PAPER)
for ax, t0, t1 in zip(axes, cuts, cuts[1:]):
    ax.set_facecolor(PAPER)
    for b in d["barStarts"]:
        if t0 <= b < t1: ax.axvline(b, color=GRID, lw=.6, zorder=0)
    for m in d["movements"]:
        if t0 <= m["from"] < t1:
            ax.axvline(m["from"], color=MUTE, lw=1.6, zorder=1)
            ax.text(m["from"] + .25, y + .05, m["title"] + "  ·  " + m["metre"], fontsize=13, color=INK, va="bottom", weight="bold")
    for e in ev:
        if not (t0 <= e["time"] < t1): continue
        v = e["voice"]; lo, hi = span[v]; h = .3 if TALL[v] > 1 else .46
        yy = base[v] + (TALL[v] - h) * ((e["midi"] - lo) / max(1, hi - lo) if TALL[v] > 1 else .5)
        w = max(e["len"] * 60 / d["bpm"] / 4 * .9, .05)
        form = ALIAS.get(e["form"], e["form"])
        ax.barh(yy, w, left=e["time"], height=h, color=HUE.get(form, GREY), alpha=min(1, .22 + .78 * min(1, e["vel"]) ** 1.5), align="edge", lw=0, zorder=2)
    ax.set_xlim(t0, t0 + 47.6); ax.set_ylim(-.3, y + .9)
    ax.set_yticks([base[v] + TALL[v] / 2 for v in LANES]); ax.set_yticklabels([SAY[v] for v in LANES], fontsize=11, color=INK)
    ax.tick_params(axis="x", labelsize=11, colors=MUTE, length=0); ax.tick_params(axis="y", length=0)
    for s in ax.spines.values(): s.set_visible(False)
axes[-1].set_xlabel("seconds (thin lines are bar lines)", fontsize=11, color=MUTE)
fig.suptitle(f"gundrum and bass 2: five movements at {d['bpm']} BPM (height within a lane is pitch, ink is loudness)", x=.01, ha="left", fontsize=16, color=INK)
fig.legend(handles=[Patch(color=c, label={"subject": "subject, answer", "countersubject": "countersubject, counter, divisions", "canon": "prolation canon", "ground": "ground, pedal"}.get(k, k)) for k, c in HUE.items()] + [Patch(color=GREY, label="drums")],
           ncol=9, loc="upper left", bbox_to_anchor=(.005, .972), frameon=False, fontsize=11, labelcolor=MUTE, handlelength=1.2, columnspacing=1.3)
fig.tight_layout(rect=(0, 0, 1, .95)); fig.savefig(sys.argv[2], facecolor=PAPER)
`;
const drew = spawnSync(PY, ["-c", plot, base + ".json", base + "-timeline.png"]);
if (drew.status) console.error(drew.stderr.toString());

const { movements: mv, levels: lv, patterns, ...short } = report;
console.log(JSON.stringify(short, null, 1));
console.table(lv);
for (const m of mv) { console.log(`\n${m.title} · ${m.bars} bars of ${m.metre} · from ${m.from}s · ${m.seconds}s\n  ${m.device}\n  ${m.crossRhythm}\n  checks ${JSON.stringify(m.checks)}`); console.table(m.entries); }
