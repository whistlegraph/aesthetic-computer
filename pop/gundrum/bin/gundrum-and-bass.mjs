#!/usr/bin/env node
// gundrum-and-bass.mjs — the gundrum subject as a drum-and-bass suite.
//
// Five movements at 172 BPM, each on a different Bach device and a different
// cross-rhythm, over a synthesized sub that carries a line of its own. Every
// drum is still a CC0 gun from pop/wattajetta/assets/guns. The subject, its
// tonal answer and its countersubject are the ones gundrum.mjs uses; the
// theory functions are repeated here so that script stays exactly as it was.
//
//   node pop/gundrum/bin/gundrum-and-bass.mjs   → out/gundrum-and-bass.{wav,mp3,json} + -timeline.png

import { writeFileSync, mkdirSync } from "node:fs";
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

const SR = 48_000, BPM = 172, STEP = 60 / BPM / 4, TAIL = 3.6;

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
const KIT = {
  S: { guns: ["sniper-shot-01"], takes: true, home: 60, hold: .2, fat: 2.5, level: -18.5, pan: -.5, wet: .1, label: "soprano · .30-06" },
  A: { guns: ["pistol-9mm-shot-01"], takes: true, home: 64, hold: .14, fat: 3, level: -19, pan: .42, wet: .1, label: "alto · 9mm pistol" },
  T: { guns: ["ak47-shot-01"], takes: true, home: 57, hold: .17, fat: 3, level: -19, pan: -.22, wet: .1, label: "tenor · AK-47" },
  snare: { guns: ["shotgun-pump-shot-01"], takes: true, home: 55, hold: .19, fat: 3.5, level: -16.5, pan: 0, wet: .14, highpass: 170, label: "snare · 12 gauge" },
  ghost: { guns: ["smg-9mm-shot-01"], takes: true, home: 60, hold: .09, fat: 2.5, level: -23, pan: .16, wet: .12, label: "ghosts and rolls · 9mm SMG" },
  kick: { guns: ["pistol-heavy-shot-03"], takes: true, home: 60, hold: .17, fat: 2.5, level: -15.5, pan: 0, wet: 0, lowpass: 750, label: "kick · 1911 .45" },
  hat: { guns: ["pistol-9mm-dry-fire-01", "rifle-dry-fire-01", "pistol-9mm-dry-fire-02"], home: 60, hold: .05, fat: 1.5, level: -27, pan: .5, wet: .04, label: "hats · dry fire" },
  orn: { guns: [], home: 60, hold: 1, fat: 1.5, level: -27, pan: 0, wet: .2, label: "racks, magazines, brass, bursts, one grenade" },
};
const SUB_LEVEL = -15.5;

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
    if (c === "X") { put("snare", t0 + i, 60 + lift + (i % 8 === 4 ? 0 : 2), 3, vel, "drums"); put("ghost", t0 + i, 62 + lift, 2, vel * .9, "drums"); }
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
  orn("grenade-explosion-01", at(9), .5, 1, 0, -5);
  orn("casing-shotgun-shell-drop-01", at(9, 6), .8, .7, -.5);
  roll("ghost", at(16, 12), 4, 1, [60, 74], [.5, 1.1]);
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
  orn("grenade-explosion-01", cyc(6, 1), .5, 1, 0, -5);
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

  orn("grenade-explosion-01", at(13), .5, 1.1, 0, -7);
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
const END = POS, N = Math.ceil((END * STEP + TAIL) * SR);

// ── render the guns ───────────────────────────────────────────────────
const bus = Object.fromEntries(Object.keys(KIT).map((k) => [k, { L: new Float32Array(N), R: new Float32Array(N) }]));
const turn = {};
events.sort((a, b) => a.step - b.step);
for (const e of events) {
  e.time = e.step * STEP;
  if (e.voice === "sub") continue;
  const k = KIT[e.voice], gun = e.gun ?? k.guns[0];
  e.semis = Math.max(-19, Math.min(31, e.midi - k.home));
  const hold = e.hold ?? Math.min(k.hold, Math.max(k.hold * .45, e.len * STEP * .95));
  const take = k.takes ? (turn[e.voice] = (turn[e.voice] ?? -1) + 1) : 0;
  const { y, lead } = hit(gun, { semis: e.semis, hold, take, takes: !!k.takes && !e.gun, lp: k.lowpass, hp: k.highpass, fat: k.fat });
  const th = ((e.pan ?? k.pan) + 1) * Math.PI / 4, b = bus[e.voice], i0 = Math.round(e.time * SR) - lead;
  for (let i = Math.max(0, -i0); i < y.length && i0 + i < N; i++) { const v = y[i] * e.vel; b.L[i0 + i] += v * Math.cos(th); b.R[i0 + i] += v * Math.sin(th); }
}

// ── the sub: a sine with a little second and third, and a quiet reese an
// octave up so it reads on small speakers. Mono. ──────────────────────
const subBus = new Float32Array(N), reese = new Float32Array(N);
for (const e of events.filter((e) => e.voice === "sub")) {
  const hz = 440 * 2 ** ((e.midi - 69) / 12), i0 = Math.round(e.time * SR), n = Math.round(e.len * STEP * SR);
  const rise = .005 * SR, fall = .03 * SR, lean = Math.min(1.25, (65 / hz) ** .35);   // the lowest notes a little hotter
  for (let i = 0; i < n + fall && i0 + i < N; i++) {
    const t = i / SR, env = Math.min(1, i / rise) * (i < n ? 1 : 1 - (i - n) / fall) * (1 - .25 * Math.min(1, t / .5));
    const p = 2 * Math.PI * hz * t;
    subBus[i0 + i] += e.vel * lean * env * Math.tanh(1.4 * (Math.sin(p) + .22 * Math.sin(2 * p) + .1 * Math.sin(3 * p)));
    const saw = (f) => 2 * ((f * t) % 1) - 1;
    reese[i0 + i] += e.vel * env * .5 * (saw(2 * hz * 1.007) + saw(2 * hz / 1.007));
  }
}
const growl = lowpass(reese, 420);
for (let i = 0; i < N; i++) subBus[i] += .16 * growl[i];
// the kick pushes the sub out of its way for 70 ms
const duck = new Float32Array(N).fill(1);
for (const e of events.filter((e) => e.voice === "kick" && e.vel > .6)) {
  const i0 = Math.round(e.time * SR);
  for (let i = 0; i < .12 * SR && i0 + i < N; i++) duck[i0 + i] = Math.min(duck[i0 + i], 1 - .6 * Math.exp(-i / (.035 * SR)) * Math.min(1, i / 96));
}
for (let i = 0; i < N; i++) subBus[i] *= duck[i];

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
  for (let i = 0; i < N; i++) { b.L[i] = soft(b.L[i] * g, .6); b.R[i] = soft(b.R[i] * g, .6); }
  levels[v] = { gainDb: dB(g) };
}
{ const g = 10 ** (SUB_LEVEL / 20) / loudPart(subBus); for (let i = 0; i < N; i++) subBus[i] = soft(subBus[i] * g, .7); levels.sub = { gainDb: dB(g) }; }

function room(send, seed) {
  let s = seed;
  const rnd = () => ((s = (s * 1664525 + 1013904223) >>> 0) / 2 ** 32), out = new Float32Array(N);
  for (let k = 0; k < 36; k++) {
    const ms = 8 + 240 * rnd() ** 1.6, d = Math.round(ms * SR / 1000), g = (rnd() < .5 ? -1 : 1) * .5 * 10 ** (-3 * ms / 260);
    for (let i = d; i < N; i++) out[i] += send[i - d] * g;
  }
  return lowpass(out, 6000);
}
const L = new Float32Array(N), R = new Float32Array(N), send = new Float32Array(N);
for (const [v, b] of Object.entries(bus)) for (let i = 0; i < N; i++) { L[i] += b.L[i]; R[i] += b.R[i]; send[i] += (b.L[i] + b.R[i]) * .7 * KIT[v].wet; }
const wetL = room(send, 7), wetR = room(send, 1311);
for (let i = 0; i < N; i++) { const s = subBus[i] * Math.SQRT1_2; L[i] += wetL[i] + s; R[i] += wetR[i] + s; }

// ── master: drive into a soft knee, then a look-ahead limiter that also
// watches the peaks between samples ───────────────────────────────────
const DRIVE = Number(process.env.GUNDRUM_DRIVE ?? 2.5), CEIL = 10 ** (-1.5 / 20), LOOK = 240;
let kneed = 0, sounding = 0;
for (let i = 0; i < N; i++) {
  const l = L[i] * DRIVE, r = R[i] * DRIVE, top = Math.max(Math.abs(l), Math.abs(r));
  if (top > .01) sounding++; if (top > .5) kneed++;
  L[i] = soft(l); R[i] = soft(r);
}
const SINC = [.25, .5, .75].map((f) => Array.from({ length: 16 }, (_, k) => { const x = k - 7 - f, w = .5 + .5 * Math.cos(Math.PI * x / 8); return Math.sin(Math.PI * x) / (Math.PI * x) * w; }));
const between = (x, i) => {
  let p = Math.abs(x[i]);
  if (p < .3 || i < 8 || i > N - 9) return p;
  for (const taps of SINC) { let v = 0; for (let k = 0; k < 16; k++) v += x[i - 7 + k] * taps[k]; p = Math.max(p, Math.abs(v)); }
  return p;
};
const need = new Float32Array(N), ahead = new Float32Array(N), gr = new Float32Array(N);
for (let i = 0, g = 1; i < N; i++) {
  const p = Math.max(between(L, i), between(R, i), i ? between(L, i - 1) : 0, i ? between(R, i - 1) : 0);
  g = Math.min(p > CEIL ? CEIL / p : 1, g + (1 - g) * .0004);
  need[i] = g;
}
for (let i = 0; i < N; i++) { let m = 1; for (let j = i, e = Math.min(N, i + LOOK); j < e; j++) if (need[j] < m) m = need[j]; ahead[i] = m; }
const fadeN = Math.round(.6 * SR);
for (let i = 0, sum = 0; i < N; i++) {
  sum += ahead[i] - (i >= LOOK ? ahead[i - LOOK] : 0);
  gr[i] = sum / Math.min(LOOK, i + 1);
  const out = gr[i] * (i > N - fadeN ? ((N - i) / fadeN) ** 2 : 1);
  L[i] *= out; R[i] *= out;
}
const reduced = Array.from(gr.subarray(0, Math.round(END * STEP * SR))).map((g) => -20 * Math.log10(g)).sort((a, b) => a - b);
const limiter = {
  drive: DRIVE, softKneeShare: +(kneed / sounding).toFixed(3),
  reductionDb: { mean: +(reduced.reduce((a, b) => a + b, 0) / reduced.length).toFixed(2), median: +reduced[reduced.length >> 1].toFixed(2), p95: +reduced[Math.floor(reduced.length * .95)].toFixed(2), max: +reduced.at(-1).toFixed(2) },
  shareOver1dB: +(reduced.filter((g) => g > 1).length / reduced.length).toFixed(3), shareOver3dB: +(reduced.filter((g) => g > 3).length / reduced.length).toFixed(3),
};

// ── write ─────────────────────────────────────────────────────────────
mkdirSync(OUT, { recursive: true });
const wav = Buffer.alloc(44 + N * 6);
wav.write("RIFF", 0); wav.writeUInt32LE(36 + N * 6, 4); wav.write("WAVEfmt ", 8); wav.writeUInt32LE(16, 16);
wav.writeUInt16LE(1, 20); wav.writeUInt16LE(2, 22); wav.writeUInt32LE(SR, 24); wav.writeUInt32LE(SR * 6, 28);
wav.writeUInt16LE(6, 32); wav.writeUInt16LE(24, 34); wav.write("data", 36); wav.writeUInt32LE(N * 6, 40);
for (let i = 0; i < N; i++) {
  wav.writeIntLE(Math.round(Math.max(-1, Math.min(1, L[i])) * 8388607), 44 + i * 6, 3);
  wav.writeIntLE(Math.round(Math.max(-1, Math.min(1, R[i])) * 8388607), 47 + i * 6, 3);
}
const base = resolve(OUT, "gundrum-and-bass"), WAV = base + ".wav", MP3 = base + ".mp3";
writeFileSync(WAV, wav);
execFileSync("ffmpeg", ["-v", "error", "-y", "-i", WAV, "-af", "volume=-1dB", "-c:a", "libmp3lame", "-b:a", "320k", MP3]);

// ── measure (nothing is played) ───────────────────────────────────────
const loudness = (file) => {
  const log = spawnSync("ffmpeg", ["-nostats", "-i", file, "-af", "ebur128=peak=true", "-f", "null", "-"]).stderr.toString();
  const tail = log.slice(log.lastIndexOf("Summary:")), num = (re) => Number(tail.match(re)?.[1]);
  return { lufs: num(/I:\s+(-?[\d.]+) LUFS/), lra: num(/LRA:\s+(-?[\d.]+) LU/), truePeak: num(/Peak:\s+(-?[\d.]+) dBFS/) };
};
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
  const times = events.filter((e) => e.voice !== "sub" && e.voice !== "orn").map((e) => e.time).sort((a, b) => a - b);
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
  title: "gundrum and bass", bpm: BPM, key: "D minor", seconds: +(N / SR).toFixed(2),
  subject: { rhythm: toBox(rhythm(SUBJECT.map((n) => n.t), LEN)), notes: SUBJECT.map((n) => name(midiOf(n.d))).join(" ") },
  countersubject: { rhythm: toBox(rhythm(HOLES, LEN)), firstSpecies: CS.ok, interlocks: interlocks(rhythm(SUBJECT.map((n) => n.t), LEN), rhythm(HOLES, LEN)) },
  movements: movements.map((m) => ({
    title: m.title, bars: m.bars, metre: m.per === 24 ? "6/4" : "4/4", from: +(m.start * STEP).toFixed(1), seconds: +(m.bars * m.per * STEP).toFixed(1),
    device: m.device, crossRhythm: m.cross, checks: m.checks, entries: m.entries.sort((a, b) => a.bar - b.bar),
  })),
  patterns: boxes, levels, timing: timing(), limiter, loudness: { wav: loudness(WAV), mp3: loudness(MP3) },
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
       "canon": "#008300", "retrograde": "#6250d6", "fragment": "#e34948", "ground": "#eb6834"}
ALIAS = {"division": "countersubject", "cadence": "subject"}
LANES = ["S", "A", "T", "sub", "snare", "ghost", "kick", "hat", "orn"]
SAY = {"S": "soprano\n.30-06", "A": "alto\n9mm pistol", "T": "tenor\nAK-47", "sub": "sub bass\n(synth)", "snare": "snare\n12 gauge",
       "ghost": "ghosts, rolls\n9mm SMG", "kick": "kick .45", "hat": "hats\ndry fire", "orn": "racks, brass"}
TALL = {"S": 3, "A": 3, "T": 3, "sub": 3, "snare": .8, "ghost": 2, "kick": .8, "hat": 1.4, "orn": .7}
base, y = {}, 0
for v in reversed(LANES): base[v] = y; y += TALL[v] + .4
span = {v: (min(e["midi"] for e in ev if e["voice"] == v), max(e["midi"] for e in ev if e["voice"] == v)) for v in LANES}
total = d["seconds"]; cuts = [0, 47.5, 95, total]
fig, axes = plt.subplots(3, 1, figsize=(20, 17), dpi=105, facecolor=PAPER)
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
fig.suptitle(f"gundrum and bass: five movements at {d['bpm']} BPM (height within a lane is pitch, ink is loudness)", x=.01, ha="left", fontsize=16, color=INK)
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
