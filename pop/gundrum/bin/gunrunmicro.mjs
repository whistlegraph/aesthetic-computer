#!/usr/bin/env node
// gunrunmicro.mjs — a club track: a gun kit and a bag of chips on a 12/8 bell-pattern groove, four to the floor.
//
// Nothing here is gundrumbasic's but the render engine (copied from gundrum-and-bass-2.mjs, which is left
// untouched). No fugue, no drum and bass. The rhythm is from the Afro-Cuban 12/8 family, set on a kick on
// every beat: a seven-stroke bell timeline that never changes, a shaker on the step before each beat, a
// middle drum with two open tones, a slap on two and four, a lead drum that talks and runs scales, a bass
// that rolls between the kicks and reaches each chord a step early, and a call answered by the lead drum.
// The chords are sweet (Cadd9, G, Am7, Fmaj7 …) and there are four progressions: home (C G Am F), lift
// (F G C C · F G Am G), turn (Am Dm G C · F Dm G G) and fall (F Em Dm C) for the breaks.
//
// Every sample is one gundrumbasic never used: a Barrett .50 as the low drum (over a synthesized thump), a
// Desert Eagle as the middle drum, an M4 as the slap, a Glock for the touches, a PPSh burst as the lead,
// pistol slides and a magazine as the shaker. The snare's other half is someone eating potato chips, and
// the cymbals are drinks: cans cracked on the drops, a pour under each build, a cork, ice, gulps, a straw.
// The chords are an electric piano, synthesized here, as are the bass, the sines and every third bite;
// the bell (steel) and the chime (aluminium) are from pop/bell.
//
//   node pop/wattajetta/bin/fetch-guns.mjs && node pop/gundrum/bin/fetch-snacks.mjs   (once per machine)
//   node pop/gundrum/bin/robot-cries.mjs          (once per machine: the ElevenLabs cries)
//   node pop/gundrum/bin/gunrunmicro.mjs          → out/gunrunmicro.{wav,mp3,json}

import { writeFileSync, mkdirSync, existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync, execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
const GUNS = resolve(LANE, "../wattajetta/assets/guns");
const PY = resolve(LANE, "../.venv/bin/python");
const BELL = resolve(LANE, "../bell/c/bell"), BELLS = resolve(OUT, ".bell-cache");
const SNACKS = resolve(LANE, "assets/snacks");   // chips and drinks: bin/fetch-snacks.mjs

// a step is a triplet eighth: three to the beat, four beats and twelve steps to the bar
const SR = 48_000, BPM = 124, BEAT = 3, BAR = 12, STEP = 60 / BPM / BEAT, TAIL = 13;
const mod = (a, n) => ((a % n) + n) % n;
const NAMES = ["C", "C#", "D", "Eb", "E", "F", "F#", "G", "G#", "A", "Bb", "B"];
const name = (m) => NAMES[mod(m, 12)] + (Math.floor(m / 12) - 1);

// ── the kit ───────────────────────────────────────────────────────────
// home: the pitch a sample plays at untransposed. level: where its bus is set after rendering (dB, loud-part
// RMS). pan is azimuth (−1 left … 1 right), dist is metres from the listener.
// A club kit: the drums stand close, the low drum dry and in the middle.
const KIT = {
  S: { guns: ["smg-micro-burst-01"], takes: true, home: 60, hold: .11, fat: 2.5, level: -19.5, pan: .55, dist: 2.5, wet: .15, label: "lead drum · PPSh burst" },
  A: { guns: ["pistol-heavy-shot-02"], takes: true, home: 57, hold: .16, fat: 3, level: -19, pan: -.45, dist: 2, wet: .14, label: "middle drum · Desert Eagle" },
  snare: { guns: ["m4-shot-01"], takes: true, home: 60, hold: .12, fat: 3.5, level: -18.5, pan: .1, dist: 1.6, wet: .2, highpass: 220, label: "slap · M4" },
  // the snare's other half: things bitten with the mouth open, a different one each time (chips, apple,
  // carrot, celery, popcorn, a bone)
  crunch: { guns: ["crunch-bite-02", "crunch-apple-01", "crunch-chew-01", "crunch-celery-01", "crunch-crispy-01", "crunch-carrot-01", "crunch-bite-03",
    "crunch-bone-01", "crunch-apple-02", "crunch-popcorn-01", "crunch-tortilla-01", "crunch-snap-01", "crunch-chew-02"], home: 60, hold: .3, fat: 2.2, level: -17.5, pan: .12, dist: 1.2, wet: .1, label: "crunch · chips, apple, carrot, celery, bone" },
  // robot cries from ElevenLabs (bin/robot-cries.mjs): twelve generated ones, and four crunches said back by a voice
  cry: { guns: [...Array.from({ length: 12 }, (_, i) => `cry-${String(i + 1).padStart(2, "0")}`), "mouth-01", "mouth-02", "mouth-03", "mouth-04"], home: 60, hold: .45, fat: 1.6, level: -17, pan: -.1, dist: 1.5, wet: .18, label: "robot cries · ElevenLabs" },
  ghost: { guns: ["pistol-9mm-shot-02"], takes: true, home: 60, hold: .09, fat: 2.5, level: -24, pan: -.2, dist: 2.5, wet: .15, label: "touches and rolls · Glock" },
  kick: { guns: ["sniper-50cal-shot-01"], takes: true, home: 60, hold: .16, fat: 3, level: -16, pan: 0, dist: 0, wet: 0, lowpass: 700, label: "low drum · Barrett .50" },
  hat: { guns: ["pistol-9mm-slide-drop-01", "pistol-heavy-slide-release-01", "pistol-9mm-mag-detach-01"], home: 60, hold: .06, fat: 1.5, level: -24.5, pan: .7, dist: 1.2, wet: .06, label: "shaker · slides, magazine" },
  orn: { guns: [], home: 60, hold: 1, fat: 1.5, level: -28, pan: 0, dist: 6, wet: .35, label: "bolts, magazines, reloads" },
};
// the three shaker clicks stand in three places: right, left, half right
const HAT_PAN = { "pistol-9mm-slide-drop-01": .72, "pistol-heavy-slide-release-01": -.6, "pistol-9mm-mag-detach-01": .3 };
const SUB_LEVEL = -13.5;

const peak = (x, a = 0, b = x.length) => { let p = 0; for (let i = a; i < b; i++) p = Math.max(p, Math.abs(x[i])); return p; };
const firstOver = (x, level, a = 0, b = x.length) => { for (let i = a; i < b; i++) if (Math.abs(x[i]) > level) return i; return -1; };
const decode = (gun) => {
  const file = execFileSync("sh", ["-c", `ls "${GUNS}/${gun}".* 2>/dev/null || ls "${SNACKS}/${gun}".*`]).toString().trim().split("\n")[0];
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
function movement(title, count, { device, cross }) {
  NOW = { title, start: POS, bars: count, per: BAR, device, cross, entries: [] };
  for (let b = 0; b < count; b++) bars.push(POS + b * BAR);
  movements.push(NOW);
  const here = NOW;
  POS += count * BAR;
  return (bar, step = 0) => here.start + (bar - 1) * BAR + step;
}
const put = (voice, step, midi, len, vel, form, more = {}) => events.push({ voice, step, midi, len, vel, form, mvt: NOW.title, ...more });
const say = (bar, voice, form, on) => NOW.entries.push({ bar, voice, form, on });
const orn = (gun, step, hold, vel = 1, pan = 0, semis = 0) => put("orn", step, 60 + semis, hold / STEP, vel, "drums", { gun, hold, pan });
// a roll: evenly spaced hits sweeping in pitch and loudness
function roll(voice, t0, count, gap, [m0, m1], [v0, v1]) {
  for (let i = 0; i < count; i++) { const f = count > 1 ? i / (count - 1) : 0; put(voice, t0 + i * gap, Math.round(m0 + (m1 - m0) * f), gap, v0 + (v1 - v0) * f, "drums"); }
}
const rng = (seed) => () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 2 ** 32);

// ── the chords ────────────────────────────────────────────────────────
// Each chord has a sweet note added (`color`).
const CH = {
  C: { say: "Cadd9", root: 48, tones: [60, 64, 67], color: 74 },
  CM: { say: "Cmaj7", root: 48, tones: [60, 64, 67], color: 71 },
  G: { say: "G", root: 43, tones: [55, 59, 62], color: 64 },
  Am: { say: "Am7", root: 45, tones: [57, 60, 64], color: 67 },
  F: { say: "Fmaj7", root: 41, tones: [53, 57, 60], color: 64 },
  Dm: { say: "Dm7", root: 38, tones: [50, 53, 57], color: 60 },
  Em: { say: "Em7", root: 40, tones: [52, 55, 59], color: 62 },
};
// four progressions, a bar a chord
const PROG = {
  home: [CH.C, CH.G, CH.Am, CH.F],                                   // the groove: C · G · Am · F
  lift: [CH.F, CH.G, CH.C, CH.C, CH.F, CH.G, CH.Am, CH.G],           // climbs home: F G C C · F G Am G
  turn: [CH.Am, CH.Dm, CH.G, CH.CM, CH.F, CH.Dm, CH.G, CH.G],        // round the fifths: Am Dm G C · F Dm G G
  fall: [CH.F, CH.Em, CH.Dm, CH.CM],                                 // the breaks, stepping down: F · Em · Dm · C
};
const CHORDS = PROG.home;
const of = (prog) => (bar) => prog[mod(bar - 1, prog.length)];
// the white notes from A3
const scaleNote = (k) => 57 + 12 * Math.floor(k / 7) + [0, 2, 3, 5, 7, 8, 10][mod(k, 7)];

// ── the parts ─────────────────────────────────────────────────────────
// The bell: seven strokes in twelve steps (2 2 1 2 2 2 1) on two brass singing bowls, E3 and A3, the low
// one on the one and on the stroke that starts the second half.
const BELL_STROKES = [0, 2, 4, 5, 7, 9, 11];
const bell = (t0, vel = 1) => BELL_STROKES.forEach((p) =>
  put("bell", t0 + p, p === 0 || p === 7 ? 52 : 57, 2, vel * (p === 0 ? 1 : p === 7 ? .9 : .75), "bell", { kind: "agogo", pan: p === 0 || p === 7 ? -.5 : .5 }));
// The shaker: the step before each beat, loud (the off-beat a club leans on), and the beat itself, light.
const shaker = (t0, vel = 1) => [0, 2, 3, 5, 6, 8, 9, 11].forEach((p, j) =>
  put("hat", t0 + p, 60 + [0, 5, 0, 7][j % 4], 1, vel * (p % 3 === 2 ? 1 : .5), "drums", { gun: KIT.hat.guns[p % 3 === 0 ? 0 : 1 + (j >> 1) % 2] }));
// The drums are written by hand, a bar at a time, as verse. The bell is the metre and never changes; the
// four drums are the words. A bar is a line of twelve syllables in four feet of three; four lines make a
// stanza, a stanza is one time round the chords, and each stanza tells one small tale, named beside it.
// A line is [low, slap, middle, touch]:
//   low     X a full stroke (the kick), x a light pickup
//   slap    X a full stroke with a bite, x a light one
//   middle  r t f the chord's root, third and the fifth below, open tones; capitals are leaned on
//   touch   x a touch, X a firmer one
// The characters: the low drum is the giant, who walks; the slap is the robot, who eats; the middle drum is
// the child, who sings; the touches are the mice.
const __ = "............";
const VERSE = {
  intro: [
    { tale: "the giant walks alone, and the mice come out", lines: [
      ["X..X..X..X..", __, __, __],                                          // four steps, nothing else in the world
      ["X..X..X..X.x", __, __, __],                                          // he shifts his weight at the end of the line
      ["X..X..X..X..", __, __, ".....x.....x"],                              // one mouse, twice, behind his heel
      ["X..X..X.xX.x", __, __, "..x..x..x..x"],                              // a mouse before every step
    ] },
    { tale: "the robot and the child join the walk", lines: [
      ["X..X..X..X..", "...X.....X..", "..rt........", ".x..x..x..x."],     // the child says two notes and stops
      ["X..X..X..X..", "...X.....X.x", "..rt....ft..", ".x..x..x...."],     // and says them again from lower down
      ["X..X..X..X..", "...X.....X..", "..rt..f.tr..", ".x..x.....x."],     // a whole sentence
      ["X..X..X.....", "...X..x.....", "..rt..ft....", __],                 // everyone stops to listen; the mice run
    ] },
  ],
  groove: [
    { tale: "dactyls: everyone lands on the giant's foot", lines: [
      ["X..X..X..X..", "...X.....X..", "..rt....ft..", ".x..x..x..x."],
      ["X..X..X..X.x", "...X.....X.x", "..rt..f.tr..", ".x..x..x...."],
      ["X..X..X..X..", "...X..x..X..", "R.t.....ft..", ".x..x.....x."],     // the child steps on his foot once
      ["X..X..X..X.x", "...X.....X.x", "..tr..f.tr..", "....x..x...."],
    ] },
    { tale: "anapests: everyone runs up behind him", lines: [
      ["X..X..X.xX..", "...X.....X..", ".rt..rt.....", "..x..x..x..x"],
      ["X.xX..X..X.x", "...X....xX.x", ".....ft..ft.", "..x.....x..."],
      ["X..X.xX..X..", "..xX.....X..", ".rt.....ft..", ".....x..x..x"],     // the robot arrives a bite early
      ["X..X..X.xX.x", "...X....xX.x", ".rt..ft..rt.", "..x..x......"],
    ] },
    { tale: "the child counts in twos while the giant counts in threes", lines: [
      ["X..X..X..X..", "...X.....X..", "r.t.f.t.r...", ".x.x.x......"],
      ["X..X..X..X.x", "...X.x...X.x", "......r.t.f.", ".x.x........"],
      ["X.X.X.X..X..", "...X.....X..", "......f.t.r.", ".x.x.x......"],     // the giant tries twos himself, half a line
      ["X..X..X.X.X.", "...X.x...X.x", "r.t.f.......", ".......x.x.."],     // and ends the line in them
    ] },
    { tale: "three wishes in a bar of four", lines: [
      ["X..X..X..X..", "...X.....X..", "R...t...f...", "..x...x...x."],
      ["X..X..X..X.x", "...X...x.X.x", "....r...t...", "....x...x..."],
      ["X...X...X..X", "...X.....X..", "..f...t...r.", ".x...x...x.."],     // the giant grants them: three strides, and a stumble home
      ["X..X..X.....", "...X.....X..", "r...t...f...", __],                 // the mice run off with the last one
    ] },
  ],
  lift: [
    { tale: "the child learns the bell's song", lines: [
      ["X..X..X..X..", "...X.....X..", "..r.tf......", "....x...x..."],     // its first half
      ["X..X..X..X.x", "...X.....X.x", ".......f.t.r", "....x...x..."],     // its second half
      ["X..X..X..X..", "...X.....X..", "R.t.fr.t.f.r", __],                 // all seven strokes, with the bell
      ["X..X..X..X.x", "...X.x.x.X.x", "..r.t.......", "........x..."],     // the robot claps with its mouth full
    ] },
    { tale: "three notes late for school, a step later every day", lines: [
      ["X..X..X..X..", "...X.....X..", "rtf.........", "....x...x..."],
      ["X..X..X..X.x", "...X.....X..", ".rtf........", "......x..x.."],
      ["X..X..X..X..", "...X.....X.x", "..rtf...rtf.", ".x.........."],
      ["X..X..X.....", "...X..x.x...", "...rtf......", __],                 // on the fourth day they are on time, and it is over
    ] },
    { tale: "the child asks, the mice answer", lines: [
      ["X..X..X..X..", "...X.....X..", ".rt.f.......", ".......xx.x."],
      ["X..X..X..X.x", "...X.....X.x", ".......ft.r.", ".xx.x......."],     // the mice answer before they are asked
      ["X..X..X..X..", "...X.....X..", "..t..r..f...", ".x.....x...."],
      ["X..X..X.xX.x", "...X.....X.x", "..f..t..r..t", __],
    ] },
    { tale: "a house built a stroke at a time", lines: [
      ["X..X..X..X..", "...X.....X..", "..r.........", "....x......."],
      ["X..X..X..X.x", "...X.....X.x", "..rt....f...", "....x...x..."],
      ["X..X..X.xX..", "...X..x..X..", "..rtf...ft..", ".x..x...x..."],
      ["X.xX.xX.....", "...X..x.....", "..rtf.tr....", __],                 // the roof goes on and everyone stands back
    ] },
  ],
  turn: [
    { tale: "the giant falls asleep, a different step each line", lines: [
      ["X..X..X..X..", "...X.....X..", "..rt........", ".......x..x."],
      ["X..X.....X.x", "...X.....X.x", "........ft..", ".x..x......."],     // he misses the third step
      ["X.....X..X..", "...X.....X..", "...r.t......", "..........x."],     // and the second; the robot bites alone
      ["X..X..X.....", "...X........", "..rt..f.t...", __],                 // and the whole last beat
    ] },
    { tale: "sentences that will not stop at the end of the line", lines: [
      ["X..X..X..X..", "...X.....X..", ".........rtf", ".x..x......."],
      ["X..X..X..X.x", "...X.....X.x", "rt.......ftr", "....x..x...."],
      ["X..X..X..X..", "...X.....X.x", "ft........rt", "....x..x...."],
      ["X..X..X.....", "...X..x.....", "fr.t........", __],                 // the last one ends where it should have begun
    ] },
    { tale: "the giant dances", lines: [
      ["X..X..X..X..", "...X.....X..", "..rt....ft..", ".x..x..x..x."],     // he remembers how he used to walk
      ["X....xX..X.x", "...X.....X.x", "..r.....f...", ".x.....x...."],     // a skip into the third step
      ["X..X...x.X..", "...X.....X..", ".rt.....t.r.", "....x......."],     // he lands late
      ["X.x..xX..X.x", "...X.....X.x", "........ft..", ".x.........."],     // twice he hops
    ] },
    { tale: "mirrors: the child's lines read the same backwards", lines: [
      ["X..X..X..X..", "...X.....X..", "..rt....tr..", ".x..x..x..x."],
      ["X..X..X..X.x", "...X.....X.x", "r.t..ff..t.r", "...x....x..."],
      ["X..X..X..X..", "...X.....X..", ".rt.f..f.tr.", __],
      ["X..X..X.....", "...X........", "..rt.ff.tr..", __],
    ] },
  ],
  gunrun: [
    { tale: "each of them takes a line", lines: [
      ["X..X..X..X.x", "...X.....X..", "..rt....ft..", "....x...x..."],
      ["X.xX.xX.xX.x", "...X.....X.x", "..r.....f...", "....x......."],     // the giant's
      ["X..X..X..X..", "...X.x.x.X.x", "..rt........", "....x...x..."],     // the robot's
      ["X..X..X..X.x", "...X.....X.x", "rt.ft.rt.ft.", __],                 // the child's
    ] },
    { tale: "three against four, a step round the circle each line", lines: [
      ["X..X..X..X..", "...X.....X..", "R...t...f...", "..x...x...x."],
      ["X..X..X..X.x", "...X.....X.x", "..r...t...f.", ".x...x...x.."],
      ["X...X...X..x", "...X.....X..", ".r...t...f..", "...x...x...x"],     // the giant joins the threes
      ["X..X..X..X.x", "...Xx..x.X.x", "..rt....ft..", "....x...x..."],
    ] },
    { tale: "the bell shared out", lines: [
      ["X..X..X..X..", "...X.....X..", "..r.tf......", ".......x.x.x"],     // the child takes its first half, the mice its second
      ["X..X..X..X.x", "...X.....X.x", ".......f.t.r", "..x.xx......"],     // they swap
      ["X.X.XX.X.X.X", "...X.....X..", "..rt....ft..", __],                 // the giant walks the whole bell
      ["X..X..X..X.x", "...X.....X.x", "R.t.fr.t.f.r", "x.x.xx.x.x.x"],     // child and mice together
    ] },
    { tale: "the walk home", lines: [
      ["X..X..X..X..", "...X.....X..", "..rt..f.tr..", ".x..x..x..x."],
      ["X..X..X..X.x", "...X..x..X.x", ".rt..ft..rt.", "..x..x......"],
      ["X..X..X.xX.x", "...X....xX.x", "rtf.rtf.rtf.", __],                 // the child skips the last of the way
    ] },
  ],
};
// The tones are written the same way, a bar at a time, and follow the same tales as the drums.
// A line is [keys, bass, chime]:
//   keys   D d a full chord, U u a stab, x a touch (see `keys` below); a stroke on the last step is the next chord
//   bass   r the root, f its fifth, o its octave, t the chord's third, n the NEXT chord's root
//   chime  r t f c the chord's root, third, fifth and sweet note, an octave over the chord; capitals an octave higher
const TONES = {
  intro: [
    [__, __, __],
    [__, __, __],
    [__, __, ".....c.....c"],
    [__, "...........n", "..r..t..f..c"],
    ["d.....u.....", "..r..r..f..n", "r.t.f......."],
    ["d.u...u..u..", "..r..r..f..n", "r.t.f.c....."],
    ["d.ud.ud.u..u", "..r..r.rf..n", "r.t.f.c.f.t."],
    ["d.u...d.....", "..r..r.....n", "r.t.c......."],
  ],
  groove: [
    ["D..u..u..u..", "..r..r..f..n", "r.t.f.c.f.t."],
    ["D..u..d..u.u", "..r..r..f..n", "r..t..f..c.."],
    ["D..u..u..d..", "..r..r..o..n", "c..f..t..r.."],
    ["D..d..u..u.u", "..r..f..r..n", "r.t.f.c.F.C."],
    ["..u..u..u..u", "..r..r..f..n", "..r..t..f..c"],
    ["..u..ud.u..u", ".rr..r..f..n", ".rt..fc....."],
    [".xu..u.xu..d", "..r.rr..f..n", "..c..f..t..r"],
    ["..u.xu..u.xu", "..r..r.ff..n", ".rt..tf..fc."],
    ["u.u.u.d....u", ".r.r.r..f..n", "r.t.f.c.f.t."],
    ["d.....u.u.u.", "..r..r.f.f.n", "......c.f.t."],
    ["u.u.u.u.d..u", ".r.r.r.r.f.n", "R.r.T.t.F.f."],
    ["d.u.u.....xu", "..r..r..f..n", "c.f.t.r....."],
    ["D...u...u...", "..r...r...f.", "r...t...f..."],
    ["....d...u..u", "..r...f...n.", "..c...f...t."],
    ["D...D...D..u", "r...f...o..n", "r...t...c..."],
    ["d.....u.....", "..r..r......", "c...f...t..."],
  ],
  lift: [
    ["D.u.u.......", "..r..r..f..n", "r.t.fc......"],
    ["d......u.u.u", "..r..r..f..n", ".......c.f.t"],
    ["D.u.uu.u.u.u", "r.r.rr.r.r.n", "r.t.fc.f.t.r"],
    ["D.u.xu.xu.du", "..r..r..f..n", "C.....c....."],
    ["D.u.xu.xu.du", "..r..r..f..n", "rtfc........"],
    ["d..u.xu.xu.u", "..r..r..f..n", ".rtfc......."],
    ["D...u.xu.x.u", "..r..r..f..n", "..rtfc..rtfc"],
    ["d....u......", "..r..r.....n", "...rtfc....."],
    ["Du.u........", "..r..r..f..n", ".......rt.f."],
    [".......uu.d.", "..r..r..f..n", ".tf.c......."],
    ["D.u..u..u...", "..r..o..f..n", "...c..t....c"],
    ["D.u..u..u..u", "..r..f..o..n", ".c..f..t..r."],
    ["D...........", "..r........n", "r..........."],
    ["D.....u....u", "..r..r.....n", "r.t...f....."],
    ["D.u...u.u..u", "..r..r..f..n", "r.t.f.c.f..."],
    ["D.u.xu.xu.du", "..r.rr..f..n", "r.t.f.c.F.C."],
  ],
  turn: [
    ["..U..u.xU..u", "..r..r..f..n", "r.t.f.c.f.t."],
    ["..U..u.....u", "..r..r.....n", "r.t.f......."],
    ["..U.....U..u", "..r.....f..n", "r.......f.t."],
    ["..U..u......", "..r..r......", "r.t.f.c....."],
    ["..U..u...u.D", "..r..r..f.tn", ".........rtf"],
    ["...u..u..u.D", "..r..r..f.tn", "ct.......rtf"],
    ["...u..u....D", "..r..r..f.tn", "c.........rt"],
    ["......u.....", "..r..r.....n", "fc.t........"],
    ["..U..u.xU..u", "..r..r..f..n", "r.t.f.c.f.t."],
    ["..U.u..xU..u", "..r.r..rf..n", "r..t.f..c..."],
    [".xU..u..U.u.", ".r...r.r...n", ".t.f...c.r.."],
    ["..U.xU..u.xu", ".r.rr...f..n", "r.tf...c.f.t"],
    ["..U..uu..U..", "..r..ff..r..", "r.t.f..f.t.r"],
    [".u..U..U..u.", ".r..f..f..r.", "c..f.tt.f..c"],
    ["U....uu....U", "..r.f..f.r..", ".rt.f..f.tr."],
    ["..U..uu..U..", "..r..rr..r..", "rt.f.cc.f.tr"],
  ],
  gunrun: [
    ["D.u.xu.xu.du", "..r..r..f..n", "r.t.f.c.f.t."],
    ["D....u.....u", ".rr.rr.ff.fn", "r.....f....."],
    ["D.uxuu.uxu.u", "..r..r..f..n", "r.....c....."],
    ["D....u.....u", "..r..r..f..n", "rtfcfcFCFCfc"],
    ["D...u...u...", "..r...r...f.", "r...t...f..."],
    ["..d...u...u.", "...r...r...n", ".c...f...t.."],
    [".u...u...u.d", "..r...f...o.", "...r...t...f"],
    ["D.u.xu.xu.du", "..r..r..f..n", "r.t.f.c.f.t."],
    ["D.u.uu......", "..r..r..f..n", ".......c.f.t"],
    [".......u.u.d", "..r..r..f..n", "r.t.fc......"],
    ["D.u.uu.u.u.u", "r.r.rr.r.r.n", "r....c......"],
    ["D.u.uu.u.u.d", "..r..r..f..n", "r.t.fc.f.t.c"],
    ["D.u.xu.xu.du", "..r..r..f..n", "r.t.f.c.f.t."],
    ["D.u..u..u..u", "..r..o..f..n", "c.f.t.r.t.f."],
    ["D.u.xu.xu.du", ".rr.rr.ff.fn", "rtf.tfc.fcF."],
  ],
};
const tones = (section, b) => TONES[section][b - 1];
const tonesOf = (section, part, from = 1, count = TONES[section].length) => TONES[section].slice(from - 1, from - 1 + count).map((l) => l[part]);
// the line for a bar of a section, and the tale it belongs to told once at the head of its stanza
const verse = (section, b) => VERSE[section][(b - 1) >> 2].lines[(b - 1) & 3];
const tell = (section) => VERSE[section].forEach((st, k) => say(1 + 4 * k, "—", "tale", st.tale));
// A bite. Two in three are recordings, the next of thirteen things crunched, and each is turned into a
// robot's cry: the crunch is the robot's mouth opening, and its loudness drives a little voice (the `cry`
// treatment in `bent` below), alone or chained with one of the other treatments. One recorded bite in four also
// echoes twice, two and four steps on (in twos across the beat's threes), smaller and on the other side,
// each echo through a chain of its own. Every third bite is not a recording at all but the synthesized one,
// singing the next note of a pentatonic.
let nBite = 0, nChain = 0, nCryShot = 0;
const CHAINS = [["cry"], ["cry", "gate"], ["stutter", "cry"], ["cry", "crush"], ["cry", "comb"], ["low", "cry"], ["cry", "smear"], ["grain", "cry"],
  ["cry"], ["swell", "cry"], ["cry", "ring"], ["dive", "cry"]];
const BITE_TUNE = [72, 76, 79, 74, 81, 67];
const crunch = (step, vel = 1, lift = 0) => {
  const n = nBite++;
  if (n % 3 === 2) return events.push({ voice: "bite", step, midi: BITE_TUNE[(n / 3 | 0) % 6] + lift, len: 3, vel, form: "bite", line: "bite", vowel: (n / 3 | 0) % 4, pan: n % 2 ? .25 : -.1, mvt: NOW.title });
  const gun = KIT.crunch.guns[n % KIT.crunch.guns.length], pitch = 60 + lift + [0, -2, 3, 1, -1][n % 5];
  // even bites: the crunch itself for 60 ms, and one of ElevenLabs' sixteen robot cries behind it, retuned.
  // odd bites: the crunch through the next local chain.
  const bite = (at, v, pan, up) => {
    if (n % 2 === 0) {
      put("crunch", at, pitch + up, 1, v, "drums", { gun, hold: .06, pan });
      put("cry", at, 60 + up + [0, 3, -2, 5, -4, 2, 7, -1][nCryShot % 8], 3, v, "cry", { gun: KIT.cry.guns[nCryShot++ % KIT.cry.guns.length], pan: -pan });
    } else put("crunch", at, pitch + up, 3, v, "drums", { gun, weird: CHAINS[nChain++ % CHAINS.length], tune: BITE_TUNE[nChain % 6] - 12, pan });
  };
  bite(step, vel * (.85 + .15 * ((n * 7) % 5) / 4), .12, 0);
  if (n % 4 === 1 || n % 4 === 0) { bite(step + 2, vel * .5, -.6, 5); if (n % 4 === 1) bite(step + 4, vel * .3, .7, 10); }
};
// a drink sound, laid in whole so that its loudest moment (or, with align, its first sound) lands on the step
const drink = (step, file, { vel = 1, pan = 0, hold = 2, align = .5 } = {}) => events.push({ voice: "drink", step, midi: 60, len: hold / STEP, vel, pan, hold, align,
  raw: resolve(SNACKS, `${file}.mp3`), form: "drink", line: file, mvt: NOW.title });
function groove(t0, c, which, { vel = 1, low = true, slap = true, mid = true } = {}) {
  const [lowLine, slapLine, midLine, touchLine] = which, [r, third, fifth] = c.tones;
  if (low) [...lowLine].forEach((ch, i) => {
    if (ch === "X") put("kick", t0 + i, 48, 2, vel, "drums");
    if (ch === "x") put("kick", t0 + i, 52, 1, vel * .55, "pickup");
  });
  if (slap) [...slapLine].forEach((ch, i) => {
    if (ch === "X") { put("snare", t0 + i, 62, 2, vel, "drums"); crunch(t0 + i, vel); }
    if (ch === "x") { put("snare", t0 + i, 67, 1, vel * .5, "drums"); crunch(t0 + i, vel * .6, 5); }
  });
  const tone = { r, t: third, f: fifth - 12 };
  if (mid) [...midLine].forEach((ch, i) => { const m = tone[ch.toLowerCase()]; if (m != null) put("A", t0 + i, m, 1.5, vel * (ch < "a" ? 1.1 : i % 3 === 0 ? .95 : .8), "tumbao"); });
  [...touchLine].forEach((ch, i) => { if (ch === "x" || ch === "X") put("ghost", t0 + i, 60 + (i % 5), 1, vel * (ch === "X" ? .5 : .32), "drums"); });
}
// The bass rolls on the step before each beat, between the kicks: root, root, fifth, then the NEXT chord's
// root, so every chord arrives early.
function bass(t0, c, nx, vel = 1, line = "..r..r..f..n") {
  const note = { r: c.root, f: c.root + 7, o: c.root + 12, t: c.tones[1] - 12, n: nx.root };
  [...line].forEach((ch, i) => { if (note[ch] != null) put("sub", t0 + i, note[ch], 1.7, vel * (ch === "n" ? 1 : ch === "f" || ch === "o" ? .8 : i === 2 ? .95 : .85), "bass"); });
}
// The keys: an electric piano carries the chords. One bar of pattern in twelve steps, a chord a bar:
// D d a full chord with its root an octave under, held to the next stroke (d softer); U u a short stab on
// the top three notes; x a touch, barely sounded; . a rest. A stroke on the last step plays the NEXT bar's
// chord, so the harmony arrives early with the bass. The hand is a few ms loose and no two strokes weigh
// the same.
const kr = rng(53);
function keys(t0, chords, pattern, { vel = 1 } = {}) {
  chords.forEach((c, b) => {
    const strokes = [...(Array.isArray(pattern) ? pattern[b] : pattern)].map((ch, i) => [ch, i]).filter(([ch]) => ch !== ".");
    strokes.forEach(([ch, i], k) => {
      const on = i === BAR - 1 ? chords[b + 1] ?? c : c, voicing = [...on.tones, on.color].sort((x, y) => x - y);
      const until = (strokes[k + 1]?.[1] ?? BAR) - i, full = ch === "D" || ch === "d", touch = ch === "x";
      const notes = full ? [on.root + 12, ...voicing] : voicing.slice(1), late = 5 * (kr() + kr() - 1) + (i % 3 ? 4 : 0);
      const v = vel * (ch === "D" ? 1 : ch === "U" ? .9 : ch === "d" ? .75 : ch === "u" ? .7 : .35) * (.88 + .24 * kr());
      notes.forEach((m, j) => events.push({ voice: "keys", step: t0 + b * BAR + i, late: late + 2.5 * j, midi: m, len: touch ? .35 : full ? until * .92 : Math.min(1.3, until * .9),
        vel: v * (j ? 1 : .85), form: "keys", line: "keys", mvt: NOW.title }));
    });
  });
  say(+(1 + (t0 - NOW.start) / BAR).toFixed(2), "keys", "keys", Array.isArray(pattern) ? "a line a bar, by hand" : pattern);
}
const span = (prog, from, count) => Array.from({ length: count }, (_, i) => prog[mod(from - 1 + i, prog.length)]);
// sustained sine chord tones, one line per chord member
const ninth = (c) => [...c.tones.map((m) => m + 12), c.color + 12];
// a deep bowl on the chord's root at the top of every bar, left to ring
const bowl = (t0, c, vel = .6) => put("bell", t0, c.root, 24, vel, "bell", { kind: "toll", pan: -.15 });
// the chime: a music box an octave over the chord, the chord and its sweet note in six even notes a bar (twos over the threes)
const chime = (t0, c, vel = 1, line = "r.t.f.c.f.t.") => { let first = true; [...line].forEach((ch, i) => {
  const m = { r: c.tones[0], t: c.tones[1], f: c.tones[2], c: c.color }[ch.toLowerCase()];
  if (m == null) return;
  put("bell", t0 + i, m + (ch < "a" ? 24 : 12), 2, vel * (first ? 1 : .7), "chime", { kind: "chime", pan: (i >> 1) % 2 ? .6 : -.6 }); first = false;
}); };
const pad = (step, tones, len, vel = 1) => tones.forEach((m, k) => put("pad", step, m, len, vel, "harmony", { line: `pad${k}` }));
// the call: a two-bar phrase in a high sine over C and G, a little bounce up and back
let nLine = 0;
const sing = (t0, notes, vel = 1, shift = 0) => { const id = `hi${nLine++}`; notes.forEach(([t, m, len]) => put("hi", t0 + t, m + shift, len, vel, "call", { line: id })); };
const CALL = [[0, 76, 3], [3, 79, 2], [5, 81, 1], [6, 79, 5], [12, 74, 3], [15, 79, 2], [17, 83, 1], [18, 79, 5]];
// the answer: the same bounce carried on over Am and F, played by the lead drum
const ANSWER = [[0, 72, 3], [3, 76, 2], [5, 81, 1], [6, 76, 3], [9, 79, 2], [11, 76, 1], [12, 72, 3], [15, 77, 2], [17, 81, 1], [18, 79, 6]];
const answer = (t0, { bars = 2, vel = 1, sine = 0 } = {}) => {
  const notes = ANSWER.filter(([t]) => t < bars * BAR);
  notes.forEach(([t, m, len], i) => put("S", t0 + t, m, len, vel * (i ? .85 : 1), "answer"));
  if (sine) sing(t0, notes, sine, 12);
};
// the lead drum talking between the calls: short figures that cross the beat, pitched off the chord
const qr = rng(2026);
const RIFFS = [[1, 2, 4, 5], [0, 4, 8], [0, 1.5, 3, 4.5, 6, 7.5], [7, 8, 10, 11], [2, 5, 8, 11], [6, 7.5, 9, 10, 11]];
function lead(t0, c, vel = 1) {
  const riff = RIFFS[Math.floor(qr() * RIFFS.length)], top = c.tones.map((m) => m + 12);
  riff.forEach((p, i) => put("S", t0 + p, top[Math.floor(qr() * 3)] + (qr() < .25 ? 12 : 0), 1, vel * (.6 + .4 * qr()) * (i ? 1 : 1.1), "lead"));
}

// ── the run ───────────────────────────────────────────────────────────
// A rush: a shaker click that runs away, clicks accelerating to six times their first speed, their pitch
// diving from two octaves up while it circles ±9 semitones. It always lands on `end`.
const rush = (end, { n = 26, beats = 1.5, top = 24, low = -6, turns = 2.5, vel = .75, flip = 1 } = {}) => {
  const width = beats * BEAT, gaps = Array.from({ length: n }, (_, i) => 1 / (1 + 5 * i / (n - 1)));
  const sum = gaps.reduce((a, b) => a + b, 0);
  let st = end - width;
  gaps.forEach((g, i) => {
    const f = i / (n - 1), semis = top + (low - top) * f ** .8 + flip * 9 * Math.sin(2 * Math.PI * turns * f);
    events.push({ voice: "hat", step: st, midi: Math.round(60 + semis), len: 1, vel: vel * (.75 + .25 * Math.cos(2 * Math.PI * f)), form: "run",
                  mvt: NOW.title, gun: KIT.hat.guns[i % 3], spin: true });
    st += width * g / sum;
  });
};
// A gun run: the lead drum up or down the scale, two notes to the step, louder toward where it is going.
function gunrun(voice, t0, from, count, { dir = 1, gap = .5, vel = .9 } = {}) {
  for (let i = 0; i < count; i++) put(voice, t0 + i * gap, scaleNote(from + dir * i) + 12, gap, vel * (.5 + .5 * i / (count - 1)) * (i % 2 ? .85 : 1), "run");
  say(+(1 + (t0 - NOW.start) / BAR).toFixed(2), voice, "run", `${count} notes ${dir > 0 ? "up" : "down"} from ${name(scaleNote(from) + 12)}`);
}
const boom = (step, spec) => put("boom", step, 36, 16, spec.vel ?? 1, "boom", { boom: spec });
const ROCK = resolve(LANE, "assets/rockkit");
const shotAt = (k, bar, step = 0) => movements[k].start + (bar - 1) * BAR + step;
// One bar of the full band: bell, shaker, chords, chime, drums, bass. A section switches parts off.
function band(t0, c, nx, which, { vel = 1, low = true, slap = true, mid = true, bassOn = true, padV = .7, chimeV = .7, bellV = 1, shake = 1, tone = [] } = {}) {
  bell(t0, bellV); shaker(t0, shake);
  pad(t0, ninth(c), BAR, padV); chime(t0, c, chimeV, tone[2]); bowl(t0, c);
  groove(t0, c, which, { vel, low, slap, mid });
  if (bassOn) bass(t0, c, nx, vel, tone[1]);
}
// A break: no drums and no bass line. Bell, shaker, chords and chime over held roots, quiet keys, a line
// in the sine a bar a note; in the last bar the slap rolls, sixteen strokes that get faster and louder.
function breakdown(at, count, c, line) {
  for (let b = 1; b <= count; b++) {
    bell(at(b), .55); shaker(at(b), .45);
    put("sub", at(b), c(b).root, 11, .7, "bass");
    pad(at(b), ninth(c(b)), BAR, .85 + .02 * b); chime(at(b), c(b), .8); bowl(at(b), c(b), .8);
  }
  keys(at(1), span(PROG.fall, 1, count), "d.....d..u..", { vel: .6 });
  sing(at(1), line, .85);
  put("kick", at(1), 48, 2, .9, "drums");
  for (let i = 0; i < 6; i++) put("A", at(count - 1, 2 * i), PROG.fall[2].tones[i % 3] - (i < 3 ? 12 : 0), 2, .3 + .08 * i, "drums");
  for (let i = 0; i < 6; i++) put("snare", at(count, i), 60 + i, 1, .3 + .06 * i, "roll");
  for (let i = 0; i < 12; i++) put("snare", at(count, 6 + .5 * i), 66 + i, .5, .6 + .04 * i, "roll");
  orn("shotgun-pump-rack-02", at(count, 3), .4, 1, 0);
  rush(at(count + 1), { beats: 2, n: 32, turns: 3.5, vel: .7 });
  // a drink is poured under the build, there is ice and a gulp in every bar, and the cork comes out on the drop
  drink(at(count + 1) - 3.3 / STEP, "pour-drink-01", { hold: 3.3, align: .08, vel: .9, pan: -.2 });
  for (let b = 1; b <= count; b++) {
    drink(at(b, 3), b % 2 ? "ice-drop-01" : "ice-drop-02", { vel: .7, pan: b % 2 ? .6 : -.6, hold: .9 });
    if (b < count) drink(at(b, 9), `gulp-0${1 + (b - 1) % 4}`, { vel: .8, pan: b % 2 ? -.35 : .35, hold: .7 });
    if (b % 2 === 0) for (const p of [6, 8]) crunch(at(b, p), .7);
  }
  drink(at(count + 1), "cork-pop-01", { vel: 1, hold: .34, align: .08 });
}

// ════ i · intro ═══════════════════════════════════════════════════════
// Kick, bell and shaker to mix in on; then the middle drum, the slap, the bass and soft keys.
{
  const at = movement("i · intro", 8, { device: "C G Am F twice: kick, bell and shaker alone for four bars, then the middle drum, the slap, the bass and soft keys", cross: "seven bell strokes in twelve steps (2 2 1 2 2 2 1) over a kick on each of four beats of three" });
  const c = of(PROG.home);
  for (let b = 1; b <= 8; b++) band(at(b), c(b), c(b + 1), verse("intro", b), { tone: tones("intro", b), padV: .35 + .04 * b, chimeV: .6, vel: .95 });
  keys(at(1), span(PROG.home, 1, 8), tonesOf("intro", 0), { vel: .7 });
  tell("intro");
  for (let b = 1; b <= 4; b++) { crunch(at(b, 3), .8); crunch(at(b, 9), .9); if (b % 2 === 0) crunch(at(b, 11), .6, 4); }
  drink(at(1), "crunch-bag-01", { hold: 1.7, align: .08, vel: .7, pan: .4 });
  drink(at(4, 9), "ice-clink-01", { hold: 1.6, vel: .7, pan: -.5 });
  drink(at(9), "can-open-01", { vel: 1, hold: .84, pan: .2 });
  say(1, "kick", "drums", "four to the bar"); say(1, "crunch", "drums", "chip bites on two and four"); say(5, "A", "tumbao", "middle drum, slap, bass");
  orn("m4-mag-insert-01", at(1, 6), .5, .8, -.4);
  orn("ak47-bolt-rack-01", at(4, 6), .6, .9, .4);
  roll("ghost", at(8, 6), 6, 1, [58, 72], [.25, 1]);
  rush(at(9), { vel: .6 });
}

// ════ ii · groove ═════════════════════════════════════════════════════
// Four times round C G Am F: the band, the keys in stabs, the call and its answer, the lead drum talking.
{
  const at = movement("ii · groove", 16, { device: "C G Am F four times: the full groove and the keys in stabs; from the second time a call in the high sine (C, G) answered by the lead drum (Am, F); from the third time the lead drum talks under the call", cross: "the bass on the step before each beat, between the kicks, and the next chord a step early; the lead drum's figures in twos and fours against the beat's threes" });
  const c = of(PROG.home);
  for (let b = 1; b <= 16; b++) {
    const time = Math.floor((b - 1) / 4), inTime = (b - 1) % 4;
    band(at(b), c(b), b === 16 ? CH.F : c(b + 1), verse("groove", b), { tone: tones("groove", b), padV: .6 + .04 * time });
    if (time >= 1 && inTime === 0) { sing(at(b), CALL); say(b, "hi", "call", "E5 G5 A5 G5 · D5 G5 B5 G5"); }
    if (time >= 1 && inTime === 2) { answer(at(b)); say(b, "S", "answer", "C5 E5 A5 E5 G5 E5 · C5 F5 A5 G5"); }
    if (time >= 2 && inTime < 2) lead(at(b), c(b), .8);
  }
  keys(at(1), span(PROG.home, 1, 16), tonesOf("groove", 0), { vel: .85 });
  tell("groove");
  for (let b = 4; b <= 16; b += 4) { drink(at(b, 9), `gulp-0${b / 4}`, { vel: .8, pan: b % 8 ? .4 : -.4, hold: .7 }); crunch(at(b, 10), .7, 3); crunch(at(b, 11), .8, 5); }
  drink(at(9), "can-open-02", { vel: .9, hold: 1, pan: -.3 });
  gunrun("S", at(4, 6), 0, 12);
  gunrun("S", at(12, 6), 7, 12, { dir: -1 });
  rush(at(9), { vel: .6 });
  orn("sniper-bolt-rack-01", at(12, 9), .5, .9, -.4);
  roll("ghost", at(16, 6), 6, 1, [79, 55], [1, .3]);
}

// ════ iii · break ═════════════════════════════════════════════════════
// The floor drops out over a progression that steps down: F · Em · Dm · C, twice.
{
  const at = movement("iii · break", 8, { device: "drums and bass line out: F Em Dm C twice in sines, chime and quiet keys over held roots; a line in the sine, a bar a note, the second time a third higher; the slap rolls back in", cross: "the roll doubling from one stroke a step to two" });
  breakdown(at, 8, of(PROG.fall), [[0, 81, 12], [12, 79, 12], [24, 77, 12], [36, 76, 12], [48, 84, 12], [60, 83, 12], [72, 81, 12], [84, 79, 6], [90, 83, 6]]);
  say(1, "hi", "call", "A5 G5 F5 E5 · C6 B5 A5 G5 B5");
  orn("smg-micro-reload-01", at(1, 3), 1.2, .7, .5);
  orn("sniper-bolt-rack-01", at(4, 6), .5, .9, -.4);
}

// ════ iv · lift ═══════════════════════════════════════════════════════
// The drop, on a progression that climbs home: F · G · C · C, then F · G · Am · G. Twice.
{
  const at = movement("iv · lift", 16, { device: "F G C C · F G Am G, twice: the full groove with the keys on every beat and off-beat, under a long line in the sine that rises with the chords; the second time the lead drum talks on the held bars and runs", cross: "the same bell and kick; the bass climbing a step early" });
  const c = of(PROG.lift);
  // a bar a note, mostly: A B C–B–G G · A B C–E D
  const LINE = [[0, 81, 12], [12, 83, 12], [24, 84, 6], [30, 83, 3], [33, 79, 3], [36, 79, 12], [48, 81, 12], [60, 83, 12], [72, 84, 6], [78, 88, 6], [84, 86, 12]];
  for (let b = 1; b <= 16; b++) {
    band(at(b), c(b), b === 16 ? CH.Am : c(b + 1), verse("lift", b), { tone: tones("lift", b), padV: .75 });
    if (b > 8 && (b % 8 === 3 || b % 8 === 4)) lead(at(b), c(b), .85);
  }
  keys(at(1), span(PROG.lift, 1, 16), tonesOf("lift", 0), { vel: .9 });
  tell("lift");
  sing(at(1), LINE, .9); sing(at(9), LINE, 1); sing(at(9), LINE.filter((_, i) => i % 2 === 0), .45, 12);
  say(1, "hi", "call", "A5 B5 C6 B5 G5 G5 · A5 B5 C6 E6 D6"); say(9, "hi", "call", "again, an octave over it on the long notes");
  gunrun("S", at(8, 6), 0, 12);
  gunrun("S", at(16, 6), 7, 12, { dir: -1 });
  boom(at(1), { pan: [-.85, .85], travel: 2.6, dist: 9, layers: ["rpg-launch-01"], drop: true, vel: 1.1 });
  rush(at(9), { vel: .6, flip: -1 });
  orn("ak47-bolt-rack-01", at(8, 3), .6, .8, .4);
  drink(at(1), "can-crack-01", { vel: 1, hold: 1.9, pan: .3 });
  drink(at(9), "can-open-03", { vel: .9, hold: .6, pan: -.3 });
  for (let b = 4; b <= 16; b += 4) { crunch(at(b, 10), .7, 3); crunch(at(b, 11), .8, 5); }
}

// ════ v · turn ════════════════════════════════════════════════════════
// A third progression, round the fifths: Am · Dm · G · C, then F · Dm · G · G. Twice, the keys in stabs.
{
  const at = movement("v · turn", 16, { device: "Am Dm G C · F Dm G G, twice: the groove under off-beat stabs on the keys and a line in the sine; the lead drum talks every other bar and runs into each half", cross: "the keys on the step before the beat, with the bass and the shaker, against the kick" });
  const c = of(PROG.turn);
  // E F G–D E · A F–A G B
  const LINE = [[0, 76, 12], [12, 77, 12], [24, 79, 6], [30, 74, 6], [36, 76, 12], [48, 81, 12], [60, 77, 6], [66, 81, 6], [72, 79, 12], [84, 83, 12]];
  for (let b = 1; b <= 16; b++) {
    band(at(b), c(b), b === 16 ? CH.F : c(b + 1), verse("turn", b), { tone: tones("turn", b), padV: .75 });
    if (b % 2 === 0) lead(at(b), c(b), .85);
  }
  keys(at(1), span(PROG.turn, 1, 16), tonesOf("turn", 0), { vel: .9 });
  tell("turn");
  sing(at(1), LINE, .9); sing(at(9), LINE, 1); sing(at(9), LINE, .4, 12);
  say(1, "hi", "call", "E5 F5 G5 D5 E5 · A5 F5 A5 G5 B5");
  gunrun("S", at(8, 6), 2, 12);
  gunrun("S", at(16, 6), 9, 12, { dir: -1 });
  rush(at(9), { vel: .6 });
  roll("ghost", at(16, 6), 6, 1, [79, 55], [1, .3]);
  drink(at(1), "can-open-02", { vel: .9, hold: 1, pan: .3 });
  for (let b = 2; b <= 16; b += 2) drink(at(b, 9), `gulp-0${1 + (b / 2) % 4}`, { vel: .75, pan: b % 4 ? .4 : -.4, hold: .7 });
  for (let b = 4; b <= 16; b += 4) { crunch(at(b, 10), .7, 3); crunch(at(b, 11), .8, 5); }
  drink(at(8, 6), "slurp-can-01", { vel: .7, hold: 1.4, pan: -.4 });
}

// ════ vi · hush ═══════════════════════════════════════════════════════
// Once more down F · Em · Dm · C with the floor out, and the roll back in.
{
  const at = movement("vi · hush", 4, { device: "drums and bass line out once down F Em Dm C; the slap rolls into the last section", cross: "the roll doubling from one stroke a step to two" });
  breakdown(at, 4, of(PROG.fall), [[0, 84, 12], [12, 83, 12], [24, 81, 12], [36, 79, 6], [42, 83, 6]]);
  orn("smg-micro-reload-01", at(1, 3), 1.2, .7, -.5);
}

// ════ vii · gunrun ════════════════════════════════════════════════════
// Home: C G Am F four times with everything, the lead drum running; then all the drums on the bell's rhythm, and one chord.
{
  const at = movement("vii · gunrun", 16, { device: "C G Am F four times with everything: call and answer, the lead drum talking and running scales; the last bar turns to G with every drum on the bell's seven strokes; a last C chord", cross: "three against four in the second stanza; the bell shared between the drums in the third; the runs two notes to the step" });
  const c = of(PROG.home);
  for (let b = 1; b <= 16; b++) {
    const inTime = (b - 1) % 4;
    if (b === 16) { bell(at(b)); shaker(at(b)); pad(at(b), ninth(CH.G), BAR, .9); chime(at(b), CH.G, .8); continue; }
    band(at(b), c(b), b === 15 ? CH.G : c(b + 1), verse("gunrun", b), { tone: tones("gunrun", b), padV: .8, vel: 1.03 });
    if (inTime === 0) sing(at(b), CALL, 1);
    if (inTime < 2) lead(at(b), c(b), .9);
    if (inTime === 2) answer(at(b), { bars: b === 15 ? 1 : 2, vel: 1.05, sine: .6 });
  }
  keys(at(1), span(PROG.home, 1, 15), tonesOf("gunrun", 0), { vel: 1 });
  tell("gunrun");
  keys(at(16), [CH.G], "D.D.DD.D.D.D", { vel: 1.05 });
  keys(at(17), [CH.C], "D...........", { vel: 1 });
  events.filter((e) => e.voice === "keys" && e.step >= at(17)).forEach((e) => { e.len = 30; });   // the last chord rings
  say(1, "hi", "call", "over everything"); say(3, "S", "answer", "with a sine an octave over it");
  gunrun("S", at(2, 6), 4, 12, { dir: -1 });
  gunrun("S", at(6, 6), 0, 12);
  gunrun("S", at(10, 6), 2, 12);
  gunrun("S", at(14, 6), 4, 12);
  for (const [bar, step, o] of [[5, 0, {}], [9, 0, { flip: -1, top: 18 }], [13, 0, {}], [15, 6, { flip: -1, beats: 1, n: 20 }]]) rush(at(bar, step), { vel: .6, ...o });
  // the close: every drum on the bell's seven strokes, climbing the G chord, and home to C
  const climb = [55, 59, 62, 67, 71, 74, 79];
  BELL_STROKES.forEach((p, i) => {
    const v = .8 + .05 * i;
    put("kick", at(16, p), 48, 2, v, "drums"); put("snare", at(16, p), 62 + i, 2, v, "close");
    put("A", at(16, p), climb[i], 1.5, v, "close"); put("S", at(16, p), climb[i] + 12, 1, v, "close");
    put("sub", at(16, p), CH.G.root + (i === 6 ? 12 : 0), 1.2, .9, "close");
  });
  say(16, "—", "close", "all the drums on the bell's rhythm, up the G chord");
  rush(at(17) - 1, { beats: 2, n: 32, turns: 3.5, vel: .7 });
  for (const [v, m, hold] of [["S", 72, .5], ["A", 60, .45], ["snare", 64, .5], ["kick", 48, .45]]) put(v, at(17), m, BAR, 1.2, v === "kick" ? "drums" : "close", { hold });
  put("sub", at(17), 48, 16, 1.1, "close"); put("sub", at(17), 36, 16, .6, "close");
  pad(at(17), [72, 76, 79, 86], 24, 1);
  [72, 76, 79, 86, 84].forEach((m, i) => put("bell", at(17) + 1.5 * i, m, 6, .9 - .08 * i, "chime", { kind: "chime", pan: i % 2 ? .6 : -.6 }));
  put("bell", at(17), 48, 24, 1, "bell", { kind: "toll", pan: -.15 });
  put("bell", at(17), 36, 24, .8, "bell", { kind: "toll", pan: .15 });
  put("bell", at(17), 52, 4, 1, "bell", { kind: "agogo", pan: .3 });
  orn("sniper-shot-tail-02", at(17), 2.8, 1.1, 0, -3);
  orn("bullet-impact-metal-01", at(17, 5), .4, .7, -.45);
  orn("ricochet-01", at(17, 9), .8, .6, .45);
  boom(at(1), { pan: [.85, -.85], travel: 2.6, dist: 9, layers: ["rpg-launch-01"], drop: true, vel: 1.1 });
  boom(at(17), { pan: -.8, dist: 20, layers: ["sniper-shot-tail-03"], vel: .8 });
  drink(at(1), "can-crack-01", { vel: 1, hold: 1.9, pan: -.3 });
  drink(at(9), "can-open-01", { vel: .9, hold: .84, pan: .3 });
  for (let b = 4; b <= 12; b += 4) { drink(at(b, 9), `gulp-0${b / 4}`, { vel: .8, pan: b % 8 ? .4 : -.4, hold: .7 }); crunch(at(b, 10), .7, 3); crunch(at(b, 11), .8, 5); }
  BELL_STROKES.forEach((p, i) => crunch(at(16, p), .8 + .04 * i, i));
  crunch(at(17), 1.1);
  drink(at(17), "can-fizz-01", { vel: .9, hold: 2.9 });
  drink(at(17, 8), "slurp-straw-01", { vel: .8, hold: 2.2, align: .08, pan: .3 });
  drink(at(18, 9), "gulp-03", { vel: .9, hold: .7 });
}

// ── the drone: C4 and G4 in two soft sines, from the first bar through the last chord ──
for (let b = 0; b <= POS / BAR; b++) for (const [m, v, id] of [[60, .35, "droneC"], [67, .28, "droneG"]])
  events.push({ voice: "pad", step: b * BAR, midi: m, len: BAR, vel: v, form: "harmony", mvt: movements.findLast((x) => x.start <= Math.min(b * BAR, POS - 1)).title, line: id });

// ── bubble pops: short sines that leap a fifth after a click, on C's pentatonic: a few everywhere, many in the breaks ──
{
  const r = rng(99), scale = [0, 2, 4, 7, 9].map((d) => 72 + d);
  movements.forEach((m, k) => {
    const often = k === 2 || k === 5 ? .22 : k === 0 ? .04 : .08;
    for (let st = 0; st < m.bars * BAR; st++) {
      if (r() > often) continue;
      const midi = scale[Math.floor(r() * 5)] + 12 * Math.floor(r() * 2);
      events.push({ voice: "pop", step: m.start + st + (r() < .3 ? .5 : 0), midi, len: 1, vel: .55 + .45 * r(), form: "pop", mvt: m.title, pan: (r() * 2 - 1) * .9, line: "pop" });
    }
  });
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
const SWING = {};   // the triplet grid is the swing
const wander = {};
for (const e of events) {
  const m = movements.find((m) => m.title === e.mvt), f = FEEL[e.voice];
  e.grid = +(e.step * STEP).toFixed(5); e.late = e.voice === "keys" ? e.late : 0; e.score = e.vel;   // the uke brings its own hand
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

// ── bent bites ────────────────────────────────────────────────────────
// A chain is a list of these, applied in order to one bite:
//   cry      a robot's cry. The bite stays as the first 45 ms. Behind it a thin pulse wave sings: it leaps up
//            to its note in 30 ms and then sags a sixth or climbs a fourth, moving only in whole semitones (a
//            robot has no glide) with a square wobble of a semitone eleven or so times a second. It is as
//            loud as the bite is, slowed down two and a half times, and breaks off twice for a sob. Three
//            resonances make it a vowel that changes as it goes (a to e, o to i, a to u or e to a).
//   mouth    through two resonances that fall from an open "a" to a closed "u" as the jaw shuts
//   stutter  its first 28 ms five times, each three semitones higher and a little quieter
//   crush    held at 7 kHz and rounded to 5 bits
//   comb     a feedback comb at the bite's note, so it rings like struck metal at that pitch
//   swell    backwards, then forwards: it is sucked in and bitten on the beat
//   back     backwards only, ending on the beat
//   grain    cut into 11 ms grains and dealt out in a shuffled order
//   dive     read at a speed that falls from double to half; rise, the other way
//   ring     multiplied by a sine falling from 190 to 70 Hz
//   low      read at half speed, an octave down
//   smear    every 20 ms grain played twice, overlapped: the same bite, twice as long
//   gate     switched on and off four times a step
// The result is brought back to full scale, so every chain hits as hard as a plain bite.
const shuffle = rng(808), CRY_VOWELS = [[[850, 1600, 2800], [500, 1900, 2600]], [[450, 800, 2500], [300, 2300, 3000]], [[850, 1600, 2800], [320, 900, 2400]], [[500, 1900, 2600], [900, 1400, 2800]]];
let nCry = 0;
function bend(y, lead, how, tune) {
  const n = y.length, at = (p) => { const j = Math.floor(p), f = p - j; return j < 0 || j + 1 >= n ? 0 : y[j] * (1 - f) + y[j + 1] * f; };
  switch (how) {
    case "cry": {
      const k = nCry++, T = Math.round((.26 + .05 * (k % 4)) * SR), o = new Float32Array(T + 2400), [from, to] = CRY_VOWELS[k % 4];
      const env = new Float32Array(n);
      for (let i = 0, f = 0; i < n; i++) { const a = Math.abs(y[i]); f += (a > f ? .02 : .0004) * (a - f); env[i] = f; }
      const top = peak(env) + 1e-9, hz0 = 440 * 2 ** ((tune + 12 - 69) / 12), falls = k % 3 !== 2, wob = 9 + 2 * (k % 3);
      const lo = [0, 0, 0], bp = [0, 0, 0];
      for (let i = 0, ph = 0, ph2 = 0; i < o.length; i++) {
        const t = i / SR, u = Math.min(1, i / T), j = Math.min(n - 1, lead + Math.floor(i / 2.5));
        const semis = Math.round(7 * (1 - Math.exp(-t / .03)) - 7 + (falls ? -9 : 5) * u ** 1.5 + (Math.sin(2 * Math.PI * wob * t) > 0 ? .6 : -.6) * Math.min(1, t / .06));
        ph = (ph + hz0 * 2 ** (semis / 12) / SR) % 1; ph2 = (ph2 + hz0 * 2 ** (semis / 12) / SR / 2) % 1;
        const sob = (u > .42 && u < .5) || (u > .72 && u < .78) ? .15 : 1;
        const loud = Math.max(env[j] / top, .45 * (1 - u)) * sob * Math.min(1, (o.length - i) / 2400);
        const voice = loud * ((ph < .25 ? 1 : -1 / 3) + .3 * (ph2 < .5 ? 1 : -1));   // a quarter-wide pulse, and a square an octave under it
        let out = 0;
        for (let m = 0; m < 3; m++) {
          const fc = from[m] * (to[m] / from[m]) ** u, g = 2 * Math.sin(Math.PI * fc / SR), h = voice - lo[m] - .08 * bp[m];
          bp[m] += g * h; lo[m] += g * bp[m]; out += [1, .75, .5][m] * bp[m];
        }
        o[i] = .5 * out + .12 * voice + (i < .045 * SR && lead + i < n ? .8 * y[lead + i] * (1 - i / (.045 * SR)) : 0);
      }
      return { y: o, lead: 0 };
    }
    case "back": { const o = new Float32Array(n); for (let i = 0; i < n; i++) o[i] = y[n - 1 - i] * Math.min(1, i / 400); return { y: o, lead: Math.max(0, n - 1 - lead) }; }
    case "swell": { const o = new Float32Array(2 * n); for (let i = 0; i < n; i++) { o[i] = .8 * y[n - 1 - i] * (i / n) ** 2; o[n + i] = y[i]; } return { y: o, lead: n + lead }; }
    case "low": { const o = new Float32Array(2 * n - 2); for (let i = 0; i < o.length; i++) o[i] = at(i / 2); return { y: o, lead: 2 * lead }; }
    case "ring": { const o = new Float32Array(n); for (let i = 0, ph = 0; i < n; i++) { ph += 2 * Math.PI * (70 + 120 * Math.exp(-i / (.05 * SR))) / SR; o[i] = y[i] * Math.sin(ph); } return { y: o, lead }; }
    case "crush": { const o = new Float32Array(n), hop = Math.round(SR / 7000); for (let i = 0, v = 0; i < n; i++) { if (i % hop === 0) v = Math.round(y[i] * 16) / 16; o[i] = v; } return { y: o, lead }; }
    case "gate": { const o = new Float32Array(n), per = STEP * SR / 4; for (let i = 0; i < n; i++) { const ph = ((i - lead + 8 * per) % per) / per; o[i] = y[i] * (ph < .55 ? Math.min(1, ph * 40, (.55 - ph) * 40) : 0); } return { y: o, lead }; }
    case "dive": case "rise": {
      const o = new Float32Array(n); let p = lead;
      for (let i = lead; i < n; i++) { const u = (i - lead) / (n - lead); o[i] = at(p); p += how === "dive" ? 2 * .25 ** u : .5 * 4 ** u; }
      for (let i = 0; i < lead; i++) o[i] = y[i];
      return { y: o, lead };
    }
    case "comb": {
      const P = Math.round(SR / (440 * 2 ** ((tune - 69) / 12))), o = new Float32Array(n + Math.round(.25 * SR));
      for (let i = 0; i < o.length; i++) o[i] = (i < n ? y[i] : 0) + (i >= P ? .86 * o[i - P] : 0);
      for (let i = 0, f = 1200; i < f; i++) o[o.length - 1 - i] *= i / f;
      return { y: o, lead };
    }
    case "stutter": {
      const g = Math.round(.028 * SR), o = new Float32Array(5 * g + 64);
      for (let r = 0, w = 0; r < 5; r++, w += Math.round(g * .86)) { const rate = 2 ** (3 * r / 12), len = Math.floor(g / rate);
        for (let i = 0; i < len; i++) o[w + i] = at(lead + i * rate) * (1 - .12 * r) * Math.min(1, (len - i) / 96); }
      return { y: o, lead: 0 };
    }
    case "grain": case "smear": {
      const g = Math.round((how === "grain" ? .011 : .02) * SR), count = Math.max(1, Math.floor((n - lead) / g)), order = Array.from({ length: count }, (_, k) => k);
      if (how === "grain") for (let k = count - 1; k > 0; k--) { const j = Math.floor(shuffle() * (k + 1)); [order[k], order[j]] = [order[j], order[k]]; }
      const each = how === "grain" ? 1 : 2, o = new Float32Array(count * g * each + g);
      order.forEach((src, k) => { for (let r = 0; r < each; r++) for (let i = 0; i < g; i++) {
        const win = how === "grain" ? Math.min(1, i / 48, (g - i) / 48) : .5 - .5 * Math.cos(2 * Math.PI * i / g);
        o[(k * each + r) * g + i] += y[lead + src * g + i] * win * (how === "grain" ? 1 - .5 * k / count : 1); } });
      if (how === "smear") for (let i = o.length - 1; i >= g / 2; i--) o[i] += o[i - (g >> 1)];
      return { y: o, lead: 0 };
    }
    default: {                                                      // mouth
      const o = new Float32Array(n);
      for (let i = 0, l1 = 0, b1 = 0, l2 = 0, b2 = 0; i < n; i++) {
        const u = Math.min(1, i / (.16 * SR)), f1 = 2 * Math.sin(Math.PI * (850 - 520 * u) / SR), f2 = 2 * Math.sin(Math.PI * (1500 - 650 * u) / SR);
        const h1 = y[i] - l1 - .12 * b1; b1 += f1 * h1; l1 += f1 * b1;
        const h2 = y[i] - l2 - .1 * b2; b2 += f2 * h2; l2 += f2 * b2;
        o[i] = .25 * y[i] + .55 * b1 + .45 * b2;
      }
      return { y: o, lead };
    }
  }
}
function bent(y, lead, chain, tune) {
  for (const how of chain) ({ y, lead } = bend(y, lead, how, tune));
  const top = peak(y) + 1e-9;
  for (let i = 0; i < y.length; i++) y[i] /= top;
  return { y, lead };
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
  // turns (@jeffrey 2026-11-23: "boring … excite that more … more musically integrated"): a held note
  // with room after it flicks up to the next D-minor scale step and back, a sixteenth each, half-way
  // through; the slide turns it into a wiggle. Seeded, so the take is the same every render.
  const SCALE = [0, 2, 4, 5, 7, 9, 10].map((d) => (d + 2) % 12);
  const above = (m) => { for (let k = 1; k < 3; k++) if (SCALE.includes((m + k) % 12)) return m + k; return m + 2; };
  let seed = 8833; const sr = () => ((seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff);
  const mids = events.filter((e) => e.voice === "mid").sort((a, b) => a.step - b.step);
  mids.forEach((e, k) => {
    const next = mids[k + 1], room = next ? next.step - e.step : e.len;
    if (e.len < 4 || room < 4 || sr() > .6) return;
    const at = Math.floor(e.len / 2), m = above(e.midi);
    events.push({ ...e, step: e.step + at, len: 1, midi: m, vel: e.vel * .9, form: "turn" });
    events.push({ ...e, step: e.step + at + 1, len: Math.max(1, e.len - at - 1), midi: e.midi, vel: e.vel * .85, form: "turn" });
  });
}
const LAYER = new Set(["drink", "bite", "stamp", "cuckoo", "hi", "pad", "bell", "boom", "sub", "mid", "pop", "donkey", "spring", "mouse", "rock", "sheep", "keys", "implode", "bullet", "brush", "whoosh"]);

// ── the stereo story ─────────────────────────────────────────────────
// The kick, the bass and the keys stay put. Each groove opens the drums out over its first four
// bars, the breaks stand close together, and the last section closes in for its last bar before the chord.
const STAGE_HOME = { S: -.68, A: .5, T: -.3, mid: -.2, hi: .3, ghost: .25 };
const lerp = (a, b, x) => a + (b - a) * Math.max(0, Math.min(1, x));
function staged(k, voice, bar) {
  const home = STAGE_HOME[voice];
  if (home === undefined) return undefined;
  const places = { S: .8, A: -.7, T: -.8, mid: -.75, hi: .4, ghost: -.3 }[voice];
  switch (k) {
    case 0: return places * .6;
    case 2: case 5: return places * .3;                                       // the breaks stand close together
    case 6: return places * (bar < 16 ? lerp(1, .5, (bar - 1) / 15) : bar < 17 ? lerp(.5, .1, bar - 16) : .8);
    default: return places * (.4 + .6 * Math.min(1, (bar - 1) / 4));          // opens as the parts settle
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
// The explosions run backwards: noise and a reversed bazooka shot drawn in from far off at both sides (or spiralling
// round the head) to a point between the ears, opening from dark to bright and rising from a sub rumble, cut
// dead on the downbeat of a drop with a thump, so each one is sucked in just before its explosion goes off.
for (const [k, bar, kind] of [[3, 1, "converge"], [6, 1, "spiral"]])
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
  const loud = [];
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
// ── the drummer (@jeffrey 2026-11-23: "humanize the percussion … varying amplitude / velocity … more human
// breath, a bit of mistakes, specially when we hit the rock drums") ──────────────────────────────────────
// The rock kit was on the grid and at the gun kit's velocities. Now a player sits at it:
//  · velocity: the hand alternates (hats strong on the beat, ~25 % softer off it), kick leans on 1 and 3,
//    snare on 2 and 4, every hit scatters ±12 %, and the whole kit breathes, a swell over each four bars
//    (±10 %) that also pushes into fills.
//  · time: each limb has its own place, kick 2 ms ahead, snare 5 ms behind (laid back), hats ±6 ms, cymbals
//    8 ms late; over that a slow drift of ±6 ms that wanders bar to bar, and the tom fills rush, ending 12 ms
//    early.
//  · mistakes, seeded so a take is repeatable: about 1 snare in 18 flammed (a stick lands 20–30 ms early at a
//    third), 1 hat in 30 caught twice, 1 ghost in 12 left out, 1 crash in 4 late by 15 ms.
// The gun kit, which had the microtiming table already, gets the velocity scatter (±8 %) and the four-bar breath.
{
  let z = 2025;
  const r = () => ((z = (z * 1664525 + 1013904223) >>> 0) / 2 ** 32);
  const g = () => (r() + r() + r() - 1.5) * 1.15;                                  // about normal, sd ~.58
  const drift = []; for (let b = 0, d = 0; b < 400; b++) drift.push(d = .7 * d + .3 * 6 * (2 * r() - 1) * 1.6);
  const breath = (st) => 1 + .1 * Math.sin(2 * Math.PI * (st / (4 * BAR)) - Math.PI / 2);
  const add = [];
  for (const e of events.filter((e) => e.voice === "rock")) {
    const n = e.rock, inBeat = e.step % 4, beat = Math.floor(e.step / 4) % 4, bar = Math.floor(e.step / 16);
    let v = e.vel * breath(e.step) * (1 + .12 * g()), late = (drift[bar] ?? 0) + 0;
    if (n.startsWith("rock-hat")) { v *= inBeat === 0 ? 1.05 : inBeat === 2 ? .78 : .7; late += 6 * g(); }
    else if (n.startsWith("rock-kick")) { v *= beat === 0 || beat === 2 ? 1.06 : .9; late += -2 + 3 * g(); }
    else if (n.startsWith("rock-snare-ghost")) { if (r() < 1 / 12) { e.drop = true; continue; } v *= .9; late += 7 + 4 * g(); }
    else if (n.startsWith("rock-snare")) {
      v *= beat === 1 || beat === 3 ? 1.06 : .92; late += 5 + 4 * g();
      if (r() < 1 / 18) add.push({ ...e, vel: v * .33, late: late - 20 - 10 * r(), rock: n, room: 0, flam: true });
    } else if (n.startsWith("rock-tom")) { const j = (e.step % 16) - 8; late += -1.5 * j + 3 * g(); v *= 1 + .02 * j; }
    else if (n.startsWith("rock-crash") || n.startsWith("rock-ride")) { late += 8 + 4 * g() + (n.startsWith("rock-crash") && r() < .25 ? 15 : 0); }
    if (n.startsWith("rock-hat") && r() < 1 / 30) add.push({ ...e, vel: v * .5, late: late + 28 + 10 * r() });
    e.vel = Math.max(.05, Math.min(1.25, v));
    e.late = e.step < 1 ? Math.max(0, late) : late;
  }
  for (let i = events.length - 1; i >= 0; i--) if (events[i].drop) events.splice(i, 1);
  events.push(...add);
  for (const e of events.filter((e) => ["kick", "snare", "ghost", "hat"].includes(e.voice)))
    e.vel = Math.max(.05, Math.min(1.2, e.vel * breath(e.step) * (1 + .08 * g())));
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
  let { y, lead } = hit(gun, { semis: e.semis, hold, take, takes, lp: k.lowpass, hp: k.highpass, fat: k.fat });
  if (e.weird) ({ y, lead } = bent(y, lead, e.weird, e.tune));
  const pan = e.voice === "hat" ? HAT_PAN[gun] : e.voice === "orn" ? Math.max(-.9, Math.min(.9, 1.5 * (e.pan ?? 0))) : e.voice === "shot" || e.voice === "crunch" || e.voice === "cry" ? (e.pan ?? k.pan) : stage(e.voice, e.step, k.pan);
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
  for (const e of events.filter((e) => e.voice === "kick" && e.form === "drums")) {
    const i0 = Math.round(e.time * SR);
    for (let i = 0; i < 6 * tau * SR && i0 + i < N; i++) d[i0 + i] = Math.min(d[i0 + i], 1 - depth * Math.exp(-i / (tau * SR)) * Math.min(1, i / 96));
  }
  return d;
};
{ const duck = kickDuck(.75, .06); for (let i = 0; i < N; i++) subBus[i] *= duck[i]; }
// the thump: the Barrett has the crack but not the chest, so every full stroke of the low drum gets a sine
// that falls from 135 to 46 Hz in a few ms and rings for a fifth of a second
for (const e of events.filter((e) => e.voice === "kick" && e.form === "drums")) {
  const i0 = Math.round(e.time * SR);
  for (let i = 0, ph = 0; i < .26 * SR && i0 + i < N; i++) {
    const t = i / SR; ph += 2 * Math.PI * (46 + 89 * Math.exp(-t / .028)) / SR;
    subBus[i0 + i] += 1.5 * e.vel * Math.min(1, i / 40) * Math.exp(-t / .1) * Math.sin(ph);
  }
}

// no sampled bass here: the synthesized sub carries the line alone
const uprightBus = { L: new Float32Array(N), R: new Float32Array(N) };

// ── the high sines: pure, gliding, never clipped ──────────────────────
// Each line is one oscillator: notes that touch glide into each other.
// Each note has an attack, a decay to its sustain level and a release: the gate below is the shape the
// one-pole follower chases, so touching (legato) notes still re-articulate with a small bloom.
function sines(voice, { attack, release, glide, decay = .25, sustain = .6, bloom = 1, vibDepth = .05, vibOnset = .25 }) {
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
      const vib = vibDepth * Math.min(1, Math.max(0, age / SR - vibOnset) / .4) * Math.sin(2 * Math.PI * 5.1 * age / SR);
      ph += 2 * Math.PI * 440 * 2 ** ((m + vib - 69) / 12) / SR;
      if (ph > 2 * Math.PI) ph -= 2 * Math.PI;
      const v = a * Math.sin(ph);
      ear(a0 + i, v);
    }
  });
  return out;
}
// the line, slippery (@jeffrey 2026-11-23: "more slippery / slidey … more like wiggly than blasted"):
// notes closer than 0.18 s are one phrase on one oscillator, so the pitch slides from note to note
// (portamento 45 ms, leaving 35 ms before the note so it lands on the beat) instead of each note being
// struck. Each phrase scoops up a semitone in 30 ms and falls five semitones as it lets go; a vibrato of ±0.3 st at eighth notes fades in 0.1 s
// into every held note. Attack 6 ms, a small
// bloom on each new note, release 0.15 s. Two saws a few cents apart and a square an octave down, through
// a resonant low-pass that pumps on every beat (widest on the one, where the line is also 18 % louder)
// instead of opening on each attack, and ducks 25 % under every kick.
function midSynth(list = events.filter((e) => e.voice === "mid"), { gate = 0, pan: panAt = null, gain = 1, out = { L: new Float32Array(N), R: new Float32Array(N) } } = {}) {
  const duck = new Float32Array(N).fill(1);                              // tucks under each kick, back in 0.12 s
  for (const k of events.filter((e) => e.voice === "kick")) {
    const i0 = Math.round(k.time * SR);
    for (let i = 0; i < .25 * SR && i0 + i < N; i++) if (i0 + i >= 0) duck[i0 + i] = Math.min(duck[i0 + i], 1 - .25 * Math.exp(-i / SR / .12));
  }
  const notes = list.slice().sort((a, b) => a.time - b.time), phrases = [];
  for (const e of notes) {
    const last = phrases.at(-1)?.at(-1);
    if (last && e.time - (last.time + Math.min(last.len * STEP, .6)) < .18) phrases.at(-1).push(e); else phrases.push([e]);
  }
  for (const ph of phrases) {
    const ons = ph.map((e) => Math.round(e.time * SR)), a0 = ons[0];
    const stop = ons.at(-1) - a0 + Math.round(Math.min(ph.at(-1).len * STEP, .6) * SR), len = stop + Math.round(.9 * SR);
    const gA = new Float32Array(len), gM = new Float32Array(len), gV = new Float32Array(len), gO = new Int32Array(len);
    ph.forEach((e, k) => {
      const on = ons[k] - a0, next = k + 1 < ph.length ? ons[k + 1] - a0 : stop;
      for (let i = on; i < next && i < len; i++) {
        const held = !gate || i - on < e.len * STEP * gate * SR;   // gate: stabs stop short, the pitch keeps sliding
        gV[i] = e.vel; gO[i] = on; gA[i] = held ? gain * e.vel * (.7 + .3 * Math.exp(-(i - on) / SR / .16)) : 0;
      }
      // the pitch leaves for each note 35 ms early, so the slide lands on the beat instead of behind it
      const lead = k ? Math.round(.035 * SR) : 0;
      for (let i = Math.max(0, on - lead); i < next - (k + 1 < ph.length ? Math.round(.035 * SR) : 0) && i < len; i++) gM[i] = e.midi;
    });
    for (let i = stop; i < len; i++) { gM[i] = gM[stop - 1]; gV[i] = gV[stop - 1]; gO[i] = gO[stop - 1]; }
    const avg = ph.reduce((t, e) => t + e.midi, 0) / ph.length;
    const pan = panAt ?? Math.max(-.95, Math.min(.95, stage("mid", ph[0].step, -.2) + (avg - 67) / 60)), ear = earAt(out, pan);
    const up = Math.exp(-1 / (.006 * SR)), down = Math.exp(-1 / (.15 * SR)), slide = Math.exp(-1 / (.045 * SR)), scoop = Math.exp(-1 / (.03 * SR));
    for (let i = stop; i < len; i++) gM[i] = gM[stop - 1] - 5 * Math.min(1, (i - stop) / SR / .35);   // the phrase falls away
    let a = 0, m = gM[0] - 1, low = 0, band = 0, p1 = (a0 * .618) % 1, p2 = (a0 * .382) % 1, p3 = 0, vp = (a0 * .137) % 1;
    for (let i = 0; i < len && a0 + i < N; i++) {
      a = gA[i] > a ? gA[i] + (a - gA[i]) * up : gA[i] + (a - gA[i]) * down;
      m = gM[i] + (m - gM[i]) * (i < .06 * SR ? scoop : slide);
      const age = (i - gO[i]) / SR, t = i / SR;
      const beat = ((a0 + i) / SR) / (60 / BPM), inBeat = beat % 1, inBar = beat % 4;
      vp = (vp + BEAT * BPM / 60 / SR) % 1;                               // vibrato at the step
      const vib = .3 * Math.min(1, Math.max(0, age - .1) / .25) * Math.sin(2 * Math.PI * vp);
      const hz = 440 * 2 ** ((m + vib - 69) / 12);
      p1 = (p1 + hz * 1.004 / SR) % 1; p2 = (p2 + hz / 1.004 / SR) % 1; p3 = (p3 + hz / 2 / SR) % 1;
      const x = (2 * p1 - 1) + (2 * p2 - 1) + .6 * (p3 < .5 ? 1 : -1);
      const pump = Math.exp(-inBeat * 3.2), down1 = inBar < 1 ? 1 : 0;   // filter opens on every beat, most on the one
      const fc = Math.min(.45 * SR, 420 + hz * 1.4 + 600 * gV[i] + (500 + 500 * down1) * pump), f = 2 * Math.sin(Math.PI * fc / SR);
      const hp = x - low - .5 * band; band += f * hp; low += f * band;
      ear(a0 + i, a * (1 + .18 * down1 * pump) * duck[a0 + i] * Math.tanh(.6 * low));
    }
  }
  return out;
}
const midBus = midSynth();
// ── the chords (@jeffrey 2026-11-23: "more forward … like chord progressions") ─────────────────
// Under the mid line, three more voices of the same synth play the harmony the sub implies: every half bar
// the root is the sub note sounding then, the chord its D-minor triad (A major when the root is A). Each
// voice takes the nearest note of the next chord, so the changes slide. The rhythm pushes: the subject's
// 3+3+2 head, a stab on 10, and on 14 the NEXT chord a sixteenth early, so every change arrives ahead.
{
  const subs = events.filter((e) => e.voice === "sub" && e.midi != null).sort((a, b) => a.step - b.step);
  const mids = events.filter((e) => e.voice === "mid").sort((a, b) => a.step - b.step);
  const SC = [2, 4, 5, 7, 9, 10, 0];
  const triad = (r) => { const pc = ((r % 12) + 12) % 12, k = SC.indexOf(pc);
    if (pc === 9) return [9, 1, 4];                                   // the dominant made major
    if (k < 0) return [pc, (pc + 4) % 12, (pc + 7) % 12];
    return [pc, SC[(k + 2) % 7], SC[(k + 4) % 7]]; };
  const rootAt = (st) => { let r = null; for (const e of subs) { if (e.step > st) break; if (e.step + e.len > st - 8) r = e.midi; } return r; };
  const live = (st) => mids.some((e) => e.step < st + 8 && e.step + e.len > st);
  const voice = (pcs, prev) => {                                     // nearest voicing between G3 and D5
    const cand = (pc, near) => { let best = null; for (let m = 55; m <= 74; m++) if (m % 12 === pc && (best === null || Math.abs(m - near) < Math.abs(best - near))) best = m; return best; };
    const v = prev ? prev.map((p) => p) : [58, 62, 66];
    const out = [], used = new Set();
    for (const p of v) { let best = null, bd = 99; for (const pc of pcs) { if (used.has(pc) && used.size < 3) continue; const m = cand(pc, p), d = Math.abs(m - p); if (d < bd) { bd = d; best = m; } } used.add(best % 12); out.push(best); }
    return out;
  };
  const lines = [[], [], []], last = events.reduce((t, e) => Math.max(t, e.step), 0);
  let prev = null;
  for (let bar = 0; bar < last; bar += BAR) {
    for (const [off, len, ahead] of [[0, 2, 0], [3, 1.5, 0], [5, 1, 0], [8, 2, 0], [11, 1.5, 1]]) {
      const st = bar + off, look = ahead ? bar + BAR : st >= bar + 6 ? bar + 6 : bar;
      if (!live(look) || !live(st)) { if (!live(st)) prev = null; continue; }
      const r = rootAt(look); if (r == null) continue;
      prev = voice(triad(r), prev);
      prev.forEach((m, k) => lines[k].push({ step: st, time: st * STEP, len, midi: m, vel: (off === 0 ? .8 : off === 11 ? .75 : .6) }));
    }
  }
  lines.forEach((notes, k) => midSynth(notes, { gate: .8, pan: [-.45, .05, .5][k], gain: .55, out: midBus }));
}
function clips(voice) {
const donkeyBus = { L: new Float32Array(N), R: new Float32Array(N) };
for (const e of events.filter((e) => e.voice === voice)) {
  let y;
  if (e.raw) {
    const raw = execFileSync("ffmpeg", ["-v", "error", "-i", e.raw, "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
    y = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2).slice();
    const pk = peak(y) + 1e-9; for (let i = 0; i < y.length; i++) y[i] /= pk;
    if (e.align) e.time -= Math.max(0, firstOver(y, e.align)) / SR;      // the clip's first sound that loud lands on the step
  } else { const b = readFileSync(e.file); y = new Float32Array(b.buffer, b.byteOffset, b.byteLength >> 2).slice(); }
  const i0 = Math.round(e.time * SR), n = Math.min(y.length, Math.round(e.hold * SR) + (e.align ? Math.max(0, firstOver(y, e.align)) : 0)), fade = Math.round((voice === "donkey" || voice === "sheep" ? .05 : .008) * SR);
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
// A glitch crash keeps its first 0.12 s, then the decay is read at a sliding rate: down a fifth over 1.6 s,
// with a wobble of ±70 cents that quickens from 5 to 11 Hz; every 70–130 ms a 25–60 ms slice may stick and
// repeat (once to three times), and now and then one jumps up a fourth. Seeded; both channels read alike.
const glitched = (y) => {
  let z = 172; const r = () => ((z = (z * 1664525 + 1013904223) >>> 0) / 2 ** 32);
  const n = y.length >> 1, out = [], keep = Math.round(.12 * SR);
  let pos = 0, i = 0, next = keep, repeat = 0, from = 0, len = 0, jump = 0;
  while (pos < n - 2 && out.length < 2 * 4 * SR) {
    const t = i / SR, after = Math.max(0, t - .12);
    if (i === next) {
      next = i + Math.round((.07 + .06 * r()) * SR);
      if (r() < .45) { repeat = 1 + Math.floor(3 * r()); len = Math.round((.025 + .035 * r()) * SR); from = pos; }
      jump = r() < .15 ? 5 : 0;
    }
    const semis = i < keep ? 0 : -7 * Math.min(1, after / 1.6) + .7 * Math.sin(2 * Math.PI * (5 * after + 1.5 * after * after)) + jump;
    const k = Math.floor(pos), f = pos - k;
    out.push(y[2 * k] * (1 - f) + y[2 * k + 2] * f, y[2 * k + 1] * (1 - f) + y[2 * k + 3] * f);
    pos += 2 ** (semis / 12);
    if (repeat && pos - from >= len) { pos = from; repeat--; }
    i++;
  }
  return Float32Array.from(out);
};
for (const e of events.filter((e) => e.voice === "rock")) {
  const i0 = Math.round(e.time * SR);
  for (let [y, g] of [[stereoWav(e.rock), 1], [e.room ? stereoWav(`${e.rock}-room`) : null, e.room]]) {
    if (!y) continue;
    if (e.glitch) y = glitched(y);
    for (let i = 0; 2 * i + 1 < y.length && i0 + i < N; i++) { rockBus.L[i0 + i] += e.vel * g * y[2 * i]; rockBus.R[i0 + i] += e.vel * g * y[2 * i + 1]; }
  }
}
// ── the keys: an electric piano, synthesized ──────────────────────────
// Each note is a sine whose phase is pushed by another sine at the same pitch (the tine and the bar it
// drives): pushed hard at the attack, more the harder the key is struck, and relaxing in a quarter of a
// second, so a hard note barks and then goes round. A quiet partial seven times up rings for 40 ms as the
// hammer leaves. Two copies a few cents apart sit left and right, and the whole instrument sways between
// the speakers 4.6 times a second. Then a 4.5 kHz low-pass and a slow chorus (13 ms ±3 ms at 0.6 Hz).
const keysBus = { L: new Float32Array(N), R: new Float32Array(N) };
for (const e of events.filter((e) => e.voice === "keys")) {
  const hz = 440 * 2 ** ((e.midi - 69) / 12), i0 = Math.round(e.time * SR), held = Math.round(e.len * STEP * SR), tau = 1.1 * (262 / hz) ** .35;
  const n = held + Math.round(.35 * SR), bark = .35 + 2.4 * Math.min(1.2, e.vel) ** 1.5;
  for (let i = 0; i < n && i0 + i < N; i++) {
    const t = i / SR, env = Math.min(1, i / 144) * Math.exp(-t / tau) * (i < held ? 1 : Math.exp(-(i - held) / (.06 * SR)));
    const idx = .22 + bark * Math.exp(-t / .25), tine = .22 * e.vel * Math.exp(-t / .04) * Math.sin(2 * Math.PI * hz * 7.01 * t);
    const a = Math.sin(2 * Math.PI * hz * 1.0015 * t + idx * Math.sin(2 * Math.PI * hz * 1.0015 * t)), b = Math.sin(2 * Math.PI * hz / 1.0015 * t + idx * Math.sin(2 * Math.PI * hz / 1.0015 * t));
    const sway = .28 * Math.sin(2 * Math.PI * 4.6 * (i0 + i) / SR), v = e.vel * env;
    keysBus.L[i0 + i] += v * ((.8 * a + .35 * b + tine) * (1 + sway));
    keysBus.R[i0 + i] += v * ((.35 * a + .8 * b + tine) * (1 - sway));
  }
}
for (const [ch, sign] of [["L", 1], ["R", -1]]) {
  const dark = lowpass(keysBus[ch], 4500), out = keysBus[ch];
  for (let i = 0; i < N; i++) {
    const d = (.013 + sign * .003 * Math.sin(2 * Math.PI * .6 * i / SR)) * SR, p = i - d, j = Math.floor(p), f = p - j;
    out[i] = dark[i] + (j >= 0 ? .55 * (dark[j] * (1 - f) + dark[j + 1] * f) : 0);
  }
}

// ── the synthesized bite ──────────────────────────────────────────────
// No recording. A crunch is something brittle breaking in many places at once, so: forty to eighty cracks,
// most of them in the first few ms and thinning out over a fifth of a second, each a click and a damped ring
// between 1.8 and 8 kHz. They are heard through a mouth: three resonances that slide from one vowel to
// another as the jaw closes (or, for two of the four mouths, opens). And the mouth is tuned: a short comb at
// the bite's pitch rings behind the cracks, so each bite sings its note.
const biteBus = { L: new Float32Array(N), R: new Float32Array(N) };
{
  let z = 404;
  const zr = () => ((z = (z * 1664525 + 1013904223) >>> 0) / 2 ** 32) * 2 - 1, u = () => (zr() + 1) / 2;
  const MOUTHS = [[[850, 1600, 2800], [320, 900, 2400]], [[550, 2000, 2700], [400, 800, 2500]], [[330, 850, 2400], [300, 2300, 3000]], [[320, 900, 2400], [900, 1500, 2900]]];
  for (const e of events.filter((e) => e.voice === "bite")) {
    const dur = .17 + .1 * u(), n = Math.round(dur * SR), x = new Float32Array(n + 2400), cracks = 40 + Math.floor(40 * u());
    for (let c = 0; c < cracks; c++) {
      const at = Math.floor(dur * u() ** 2.4 * SR), len = (.0004 + .003 * u()) * SR, f = 1800 + 6200 * u(), amp = (1 - at / n) ** 1.5 * (.4 + .6 * u()), ph = 6.28 * u();
      for (let j = 0; j < 6 * len && at + j < x.length; j++) x[at + j] += amp * (Math.exp(-j / len) * Math.sin(2 * Math.PI * f * j / SR + ph) + (j < len ? .6 * zr() * (1 - j / len) : 0));
    }
    const [from, to] = MOUTHS[e.vowel % 4], P = Math.round(SR / (440 * 2 ** ((e.midi - 69) / 12))), comb = new Float32Array(P);
    const i0 = Math.round(e.time * SR), ear = earAt(biteBus, e.pan), lo = [0, 0, 0], bp = [0, 0, 0];
    for (let i = 0; i < x.length && i0 + i < N; i++) {
      const w = Math.min(1, i / n), sung = x[i] + .82 * comb[i % P];
      comb[i % P] = sung;
      let out = .3 * x[i] + .22 * sung;
      for (let k = 0; k < 3; k++) {
        const fc = from[k] * (to[k] / from[k]) ** w, g = 2 * Math.sin(Math.PI * fc / SR), h = sung - lo[k] - .09 * bp[k];
        bp[k] += g * h; lo[k] += g * bp[k]; out += [.9, .7, .45][k] * bp[k];
      }
      ear(i0 + i, e.vel * out * Math.min(1, (x.length - i) / 1200));
    }
  }
}
const drinkBus = clips("drink"), brushBus = clips("brush"), whooshBus = clips("whoosh"), sheepBus = clips("sheep"), stampBus = clips("stamp"), cuckooBus = clips("cuckoo"), donkeyBus = clips("donkey"), springBus = clips("spring"), mouseBus = clips("mouse");
const implodeBus = { L: new Float32Array(N), R: new Float32Array(N) }, bulletBus = { L: new Float32Array(N), R: new Float32Array(N) };
{
  let z = 77;
  const zr = () => ((z = (z * 1664525 + 1013904223) >>> 0) / 2 ** 32) * 2 - 1;
  const grenade = (() => { const raw = execFileSync("ffmpeg", ["-v", "error", "-i", resolve(GUNS, "rpg-launch-02.mp3"), "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
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
const hiBus = sines("hi", { attack: .018, release: .3, glide: .07, decay: .22, sustain: .55, vibDepth: .16, vibOnset: .1 });
// the chords: swell in over a third of a second, settle slowly, ring out for over a second
const padBus = sines("pad", { attack: .6, release: 2.8, glide: .25, decay: 1.4, sustain: .8 });

// ── FEM bells from pop/bell, rendered once and kept in out/.bell-cache ─
const BELL_KIND = { chime: ["aluminum", "handbell", 4], agogo: ["brass", "bowl", 6], toll: ["brass", "bowl", 9] };
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
  const pan = e.pan !== undefined ? e.pan : e.kind === "church" ? -.15 : e.kind === "glass" ? [-.6, 0, .6][j % 3] : e.kind === "deep" ? [-.2, .2][j % 2] : e.kind === "bowl" ? 0 : [-.35, .35][j % 2];
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
  const g = 10 ** (KIT[v].level / 20) / (loudPart(b.L, b.R) || 1);
  for (let i = 0; i < N; i++) { b.L[i] = soft(b.L[i] * g, .75); b.R[i] = soft(b.R[i] * g, .75); }
  levels[v] = { gainDb: dB(g) };
}
{ const g = 10 ** (SUB_LEVEL / 20) / loudPart(subBus); for (let i = 0; i < N; i++) subBus[i] = soft(subBus[i] * g, .7); levels.sub = { gainDb: dB(g) }; }
// the clean layers are set against the driven mix, and are never clipped
const CLEAN = { brush: [brushBus, -23], whoosh: [whooshBus, -21], implode: [implodeBus, -18], bullet: [bulletBus, -27], keys: [keysBus, -18.5], bite: [biteBus, -18], drink: [drinkBus, -19], sheep: [sheepBus, -17], stamp: [stampBus, -15], cuckoo: [cuckooBus, -20], rock: [rockBus, -16], spring: [springBus, -22], mouse: [mouseBus, -27], donkey: [donkeyBus, -19], upright: [uprightBus, -15.5], pop: [popBus, -25], mid: [midBus, -19], hi: [hiBus, -19.5], pad: [padBus, -19.5], bell: [bellBus, -20], gong: [gongBus, -20], boom: [boomBus, -19] };
const scale = (b, to) => { const lp = loudPart(b.L, b.R); if (!(lp > 0)) return null; const g = 10 ** (to / 20) / lp; for (let i = 0; i < N; i++) { b.L[i] *= g; b.R[i] *= g; } return dB(g); };
for (const [v, [b, to]] of Object.entries(CLEAN)) levels[v] = { gainDb: scale(b, to), loudRms: to };
// the pump: the chords, the keys and the chime dip under every kick and swell back between them
{ const pump = kickDuck(.45, .11); for (const b of [padBus, keysBus, bellBus, hiBus]) for (let i = 0; i < N; i++) { b.L[i] *= pump[i]; b.R[i] *= pump[i]; } }
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
  if (v !== "kick") place(v, b, { pan: KIT[v].pan, dist: KIT[v].dist, wet: KIT[v].wet, far: .02 * Math.max(0, KIT[v].dist - 1) });
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
  [[1, -4], [4.9, -3], [5, -2], [8.9, -1]],        // i: the intro
  [[1, -1], [16.9, 0]],                            // ii: the groove
  [[1, -8], [7, -7], [8.9, -2]],                   // iii: the break, swelling back in
  [[1, 1], [16.9, 0]],                             // iv: the lift
  [[1, 0], [16.9, 0]],                             // v: the turn
  [[1, -8], [3, -7], [4.9, -2]],                   // vi: the hush
  [[1, 1], [16.9, 1], [17, -2]],                   // vii: everything, to the chord
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
const DRIVE = Number(process.env.GUNDRUM_DRIVE ?? 1.6), CEIL = 10 ** (-1.5 / 20), LOOK = 240;
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
place("keys", keysBus, { pan: 0, dist: 0, wet: .2, far: .22 });
place("bite", biteBus, { pan: .1, dist: 1.2, wet: .14, far: .08 });
place("drink", drinkBus, { pan: 0, dist: 1.2, wet: .15, far: .08 });
place("sheep", sheepBus, { pan: 0, dist: 4, wet: .25, far: .3 });
place("stamp", stampBus, { pan: 0, dist: 2.2, wet: .45, far: .1 });
place("cuckoo", cuckooBus, { pan: 0, dist: 5, wet: .3, far: .25 });
place("rock", rockBus, { pan: 0, dist: 0, wet: .1, far: .04 });
place("spring", springBus, { pan: 0, dist: 2.5, wet: .3, far: .15 });
place("mouse", mouseBus, { pan: 0, dist: 1.5, wet: .15, far: .05 });
place("donkey", donkeyBus, { pan: .3, dist: 3.5, wet: .2, far: .25 });
place("upright", uprightBus, { pan: -.12, dist: 1.2, wet: .12, far: .02 });
place("pop", popBus, { pan: 0, dist: 3, wet: .3, far: .3 });
place("mid", midBus, { pan: -.2, dist: 2.2, wet: .25, far: .12 });
place("hi", hiBus, { pan: .3, dist: 3, far: .3 });
place("pad", padBus, { pan: 0, dist: 4, far: .4 });
place("bell", bellBus, { pan: 0, dist: 5, far: .5 });
place("gong", gongBus, { pan: -.15, dist: 7, far: .5 });
// ── the cathedral ─────────────────────────────────────────────────────
// A stone church behind the room: 65 ms before anything comes back, four sparse early reflections off far
// walls, then an 8.5 s tail that is dark from 2.2 kHz. It is sent to by voice (the robot cries, the bites,
// the bowls and chime, the call, the keys, the drinks, a little of the lead and the slap; never the low drum
// or the bass) and by moment: everything sent in the breaks and on the last chord, most of it in the intro,
// a third of it in the grooves, where the tail also ducks under each kick.
const cathSendL = new Float32Array(N), cathSendR = new Float32Array(N), cathAmount = new Float32Array(N);
{
  const where = [.75, .34, 1, .34, .34, 1, .34], last = movements.at(-1), close = (last.start + last.bars * BAR - BAR) * STEP;
  for (let i = 0, k = 0; i < N; i++) {
    const t = i / SR, step = t / STEP;
    while (k < movements.length - 1 && movements[k + 1].start <= step) k++;
    const m = movements[k], into = (step - m.start) / BAR, left = m.bars - into;
    let a = where[k] ?? .34;
    if (into < .5 && k > 0) a = lerp(where[k - 1] ?? a, a, into * 2);          // half a bar to cross between sections
    if (left < 1 && (where[k + 1] ?? 0) > a) a = lerp(a, .8, 1 - left);        // the bar before a break leans in
    cathAmount[i] = t >= close ? 1 : a;
  }
  const feed = [[bus.cry, 1], [bus.crunch, .45], [biteBus, .8], [bellBus, 1], [hiBus, 1], [padBus, .5], [keysBus, .7], [drinkBus, .8], [bus.S, .4], [bus.snare, .25], [bus.A, .2], [popBus, .8]];
  const pre = Math.round(.065 * SR), taps = [[.09, .5], [.142, -.4], [.213, .35], [.331, -.28]].map(([sec, g]) => [Math.round(sec * SR), g]);
  for (const [b, w] of feed) for (let i = 0; i + pre < N; i++) { const a = w * cathAmount[i]; cathSendL[i + pre] += a * b.L[i]; cathSendR[i + pre] += a * b.R[i]; }
  for (const ch of [cathSendL, cathSendR]) { const dry = ch.slice(); for (const [d, g] of taps) for (let i = d; i < N; i++) ch[i] += g * dry[i - d]; }
}
const cathedral = hall(cathSendL, cathSendR, { rt60: 8.5, damp: 2200, size: 1.75 });
{ const duck = kickDuck(.5, .14); for (let i = 0; i < N; i++) { cathedral.L[i] *= .85 * duck[i]; cathedral.R[i] *= .85 * duck[i]; } }
const space = hall(hallL, hallR, { rt60: 3.6, damp: 4800, size: 1.15 });
const boomHall = hall(boomSendL, boomSendR, { rt60: 5.5, damp: 3200, size: 1.35 });
placed.boom = events.filter((e) => e.voice === "boom").map((e) => ({ at: +e.time.toFixed(2), ...e.boom }));
const boomDuck = kickDuck(.72, .08);
for (let i = 0; i < N; i++) {
  const d = boomDuck[i], dm = drop[i] * d * Math.SQRT1_2;
  const g = DYN[i];
  L[i] += (stampBus.L[i] + cuckooBus.L[i] + brushBus.L[i] + whooshBus.L[i] + implodeBus.L[i] + bulletBus.L[i] + keysBus.L[i] + biteBus.L[i] + drinkBus.L[i] + sheepBus.L[i] + rockBus.L[i] + springBus.L[i] + mouseBus.L[i] + donkeyBus.L[i] + uprightBus.L[i] + popBus.L[i] + midBus.L[i] + hiBus.L[i] + padBus.L[i] + bellBus.L[i] + gongBus.L[i] + space.L[i] + cathedral.L[i]) * g + (boomBus.L[i] + .5 * boomHall.L[i]) * d + dm;
  R[i] += (stampBus.R[i] + cuckooBus.R[i] + brushBus.R[i] + whooshBus.R[i] + implodeBus.R[i] + bulletBus.R[i] + keysBus.R[i] + biteBus.R[i] + drinkBus.R[i] + sheepBus.R[i] + rockBus.R[i] + springBus.R[i] + mouseBus.R[i] + donkeyBus.R[i] + uprightBus.R[i] + popBus.R[i] + midBus.R[i] + hiBus.R[i] + padBus.R[i] + bellBus.R[i] + gongBus.R[i] + space.R[i] + cathedral.R[i]) * g + (boomBus.R[i] + .5 * boomHall.R[i]) * d + dm;
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
const base = resolve(OUT, "gunrunmicro"), WAV = base + ".wav", MP3 = base + ".mp3";
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
const TARGET = Number(process.env.GUNDRUM_LUFS ?? -9.5), passes = [];
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
const boxes = VERSE;
const report = {
  title: "gunrunmicro", bpm: BPM, key: "C major", seconds: +(N / SR).toFixed(2),
  progressions: Object.fromEntries(Object.entries(PROG).map(([k, p]) => [k, p.map((c) => c.say).join(" · ")])), bell: BELL_STROKES,
  movements: movements.map((m) => ({
    title: m.title, bars: m.bars, metre: "12/8", from: +(m.start * STEP).toFixed(1), seconds: +(m.bars * m.per * STEP).toFixed(1),
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
       "canon": "#008300", "retrograde": "#6250d6", "fragment": "#e34948", "ground": "#eb6834", "run": "#00a3a3", "bell": "#b07d10", "boom": "#8b1e3f"}
ALIAS = {"division": "countersubject", "cadence": "subject"}
LANES = ["stamp", "cuckoo", "whoosh", "brush", "bullet", "implode", "keys", "sheep", "rock", "mouse", "spring", "donkey", "pop", "hi", "mid", "pad", "bell", "S", "A", "T", "sub", "snare", "ghost", "kick", "hat", "shot", "orn", "boom"]
SAY = {"stamp": "aesthetic dot\ncomputer (sung)", "cuckoo": "cuckoo clock\n(DiArchangeli, CC0)", "brush": "brushes,\nbroom", "whoosh": "whooshes,\ncloth flips", "implode": "implosions", "bullet": "ricochets,\nbullets past", "keys": "montuno\n(plucked synth)", "sheep": "sheep\n(aesthetivox)", "rock": "rock kit\n(DrumGizmo)", "spring": "springs", "mouse": "mice", "donkey": "donkey\n(aesthetivox)", "upright": "upright bass\n(pjcohen, CC0)", "pop": "bubble pops", "shot": "shotgun\nMossberg", "mid": "mid synth\n(tenor line)", "hi": "high sine", "pad": "sine chords", "bell": "bell\n(steel, FEM)", "boom": "explosions", "S": "lead drum\nPPSh burst", "A": "middle drum\nDesert Eagle", "T": "tenor\nAK-47", "sub": "bass\n(synth)", "snare": "slap\nM4",
       "ghost": "touches, rolls\nGlock", "kick": "low drum\nBarrett .50", "hat": "shaker\nslides", "orn": "bolts, reloads"}
TALL = {"stamp": 1.4, "cuckoo": 1.4, "whoosh": .8, "brush": .8, "bullet": .8, "implode": .8, "keys": 1.4, "sheep": 1.4, "rock": .8, "mouse": .8, "spring": .8, "donkey": 1.4, "pop": 1.4, "shot": .8, "mid": 2.4, "hi": 2.4, "pad": 1.4, "bell": 1.6, "boom": .8, "S": 3, "A": 3, "T": 3, "sub": 3, "snare": .8, "ghost": 2, "kick": .8, "hat": 1.4, "orn": .7}
LANES = [v for v in LANES if any(e["voice"] == v for e in ev)]
base, y = {}, 0
for v in reversed(LANES): base[v] = y; y += TALL[v] + .4
span = {v: (min(e["midi"] for e in ev if e["voice"] == v), max(e["midi"] for e in ev if e["voice"] == v)) for v in LANES}
total = d["seconds"]; half = total / 2; cuts = [0, half, total]
fig, axes = plt.subplots(2, 1, figsize=(20, 14), dpi=105, facecolor=PAPER)
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
        w = max(e["len"] * 60 / d["bpm"] / 3 * .9, .05)
        form = ALIAS.get(e["form"], e["form"])
        ax.barh(yy, w, left=e["time"], height=h, color=HUE.get(form, GREY), alpha=min(1, .22 + .78 * min(1, e["vel"]) ** 1.5), align="edge", lw=0, zorder=2)
    ax.set_xlim(t0, t0 + half + .1); ax.set_ylim(-.3, y + .9)
    ax.set_yticks([base[v] + TALL[v] / 2 for v in LANES]); ax.set_yticklabels([SAY[v] for v in LANES], fontsize=11, color=INK)
    ax.tick_params(axis="x", labelsize=11, colors=MUTE, length=0); ax.tick_params(axis="y", length=0)
    for s in ax.spines.values(): s.set_visible(False)
axes[-1].set_xlabel("seconds (thin lines are bar lines)", fontsize=11, color=MUTE)
fig.suptitle(f"gunrunmicro: seven sections and a chord at {d['bpm']} BPM (height within a lane is pitch, ink is loudness)", x=.01, ha="left", fontsize=16, color=INK)
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

