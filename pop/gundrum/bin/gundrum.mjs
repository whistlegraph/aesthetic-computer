#!/usr/bin/env node
// gundrum.mjs — a four-voice fugue played on guns cut short as drums.
//
// Every sound is a CC0 gun sample from pop/wattajetta/assets/guns, sliced at
// its attack and resampled to a pitch, the way last-minute.py's `gun` does it.
// The form follows papers/composition-platter/bach/digest 05 (fugue) and 06
// (contrivance). Those entries end in `tools` blocks nobody had built yet;
// tonalAnswer, transform, strettoDistances and invertibleAt below are those
// tools, on top of pop/lib/counterpoint.mjs (consonance, first species) and
// pop/lib/necklace.mjs (the countersubject plays the holes in the subject).
//
//   node pop/gundrum/bin/gundrum.mjs          → out/gundrum.{wav,mp3,png,json}

import { writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync, execFileSync } from "node:child_process";
import { isConsonant, isPerfect, generateFirstSpecies, checkFirstSpecies } from "../../lib/counterpoint.mjs";
import { rhythm, complement, interlocks, toBox } from "../../lib/necklace.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
const GUNS = resolve(LANE, "../wattajetta/assets/guns");
const PY = resolve(LANE, "../.venv/bin/python");

const SR = 48_000;
const BPM = 120;
const STEP = 60 / BPM / 4;            // a sixteenth, the grid everything sits on
const BAR = 16;                       // steps
const BARS = 36;                      // the last hit lands on the downbeat of bar 37
const TAIL = 3.2;
const N = Math.ceil((BARS * BAR * STEP + TAIL) * SR);

// ── the key: D minor, as scale degrees (0 = D4) ───────────────────────
const TONIC = 62, MINOR = [0, 2, 3, 5, 7, 8, 10];
const mod = (a, n) => ((a % n) + n) % n;
const midiOf = (d) => TONIC + 12 * Math.floor(d / 7) + MINOR[mod(d, 7)];
const degOf = (m) => 7 * Math.floor((m - TONIC) / 12) + MINOR.indexOf(mod(m - TONIC, 12));
const NAMES = ["C", "C#", "D", "Eb", "E", "F", "F#", "G", "G#", "A", "Bb", "B"];
const name = (m) => NAMES[mod(m, 12)] + (Math.floor(m / 12) - 1);

// A line is [{ t, d, len }] — onset and length in steps, pitch as a degree.
const line = (rows) => rows.map(([t, d, len]) => ({ t, d, len }));
const span = (l) => Math.max(...l.map((n) => n.t + n.len));

// The subject: a tresillo head leaping tonic → dominant, a run of sixteenths
// up to the sixth, eighths back down. Two bars. The rhythm is the identity.
const SUBJECT = line([
  [0, 0, 3], [3, 4, 3], [6, 2, 2], [8, 1, 2], [10, 3, 2], [12, 2, 2], [14, 1, 1], [15, 0, 1],
  [16, 1, 1], [17, 2, 1], [18, 3, 1], [19, 4, 1], [20, 5, 2], [22, 4, 2], [24, 3, 2], [26, 1, 2], [28, 2, 4],
]);
const LEN = span(SUBJECT);

// ── digest 06: one line related to itself by a transformation ─────────
// invert mirrors about the third, so tonic and dominant swap.
function transform(l, { delay = 0, interval = 0, invert = false, retrograde = false, scale = 1 } = {}) {
  const end = span(l);
  return l
    .map((n) => ({
      t: (retrograde ? end - n.t - n.len : n.t) * scale + delay,
      d: (invert ? 4 - n.d : n.d) + interval,
      len: n.len * scale,
    }))
    .sort((a, b) => a.t - b.t);
}

// ── digest 05: the answer a fifth up, tonal ───────────────────────────
// Up to the first dominant, tonic is answered by dominant and dominant by
// tonic; after it the line is simply a fifth higher. (standard practice; the
// digest states only that tonic and dominant swap.)
function tonalAnswer(subject) {
  let head = true;
  return subject.map((n) => {
    const dominant = mod(n.d, 7) === 4;
    const d = n.d + (head && dominant ? 3 : 4);
    if (dominant) head = false;
    return { ...n, d };
  });
}

// What a line is sounding on each quarter-note beat. Every vertical check
// here is on this skeleton: on drums the beats are what the ear compares.
function skeleton(l, beats = Math.ceil(span(l) / 4)) {
  return Array.from({ length: beats }, (_, b) => {
    const n = l.findLast((n) => n.t <= b * 4 && b * 4 < n.t + n.len);
    return n ? midiOf(n.d) : null;
  });
}
const meetings = (a, b) => {
  const A = skeleton(a, 24), B = skeleton(b, 24), out = [];
  A.forEach((m, i) => { if (m != null && B[i] != null) out.push({ beat: i, semis: m - B[i] }); });
  return out;
};

// Every delay and interval at which the subject is consonant against itself.
function strettoDistances(subject) {
  const out = [];
  for (const invert of [false, true])
    for (const delay of [4, 8, 12, 16])
      for (const interval of [7, 4, 3, -3, -4, -7]) {
        const met = meetings(subject, transform(subject, { delay, interval, invert }));
        const good = met.filter((m) => isConsonant(m.semis)).length;
        out.push({ delay, interval, invert, good, of: met.length, ok: good === met.length });
      }
  return out;
}

// Inverting at the octave turns a fifth into a fourth, so the fifth is a fault.
function invertibleAt(upper, lower, at = 8) {
  if (at !== 8) throw new Error("only the octave is worked out here");
  const faults = meetings(upper, lower).filter((m) => !isConsonant(m.semis) || (isPerfect(m.semis) && mod(m.semis, 12) === 7));
  return { ok: faults.length === 0, faults };
}

// ── the countersubject ────────────────────────────────────────────────
// Pitches: first species (Fux, via counterpoint.mjs) over the answer's beats,
// kept within an octave so it can go underneath. Rhythm: the answer's silent
// sixteenths (rhythm-platter 06: give the second player the holes), never two
// in a row.
const ANSWER = tonalAnswer(SUBJECT);
function countersubject() {
  const cantus = skeleton(ANSWER);
  let best;
  for (let lo = 3; lo <= 8; lo++)
    for (let hi = 9; hi <= 12; hi++) {
      const sk = generateFirstSpecies(cantus, { tonic: TONIC % 12, scale: MINOR, minAbove: lo, maxAbove: hi });
      if (!sk) continue;
      const faults = sk.filter((m, i) => mod(m - cantus[i], 12) === 7).length;
      if (!best || faults < best.faults) best = { sk, faults, lo, hi };
    }
  const holes = complement(rhythm(SUBJECT.map((n) => n.t), LEN)).onsets;
  const kept = [];
  for (const p of holes) if (kept.at(-1) !== p - 1) kept.push(p);
  const l = kept.map((t, i) => ({ t, d: degOf(best.sk[Math.floor(t / 4)]), len: (kept[i + 1] ?? LEN) - t }));
  return { l, cantus, ...best, check: checkFirstSpecies(cantus, best.sk) };
}
const CS = countersubject();
const COUNTER = CS.l;                               // as written: above the answer on A4
const HEAD = SUBJECT.filter((n) => n.t < 8);        // the tresillo
const RUN = SUBJECT.filter((n) => n.t >= 16 && n.t < 20).map((n) => ({ ...n, t: n.t - 16 }));

// ── the kit ───────────────────────────────────────────────────────────
// home: the pitch the sample plays at untransposed. hold: longest ring (s).
const KIT = {
  S: { gun: "pistol-9mm-dry-fire-01", home: 69, hold: .07, fat: 1.5, gain: .62, pan: -.55, wet: .10, label: "soprano · 9mm dry fire" },
  A: { gun: "pistol-9mm-shot-01", takes: true, home: 67, hold: .13, fat: 3, gain: .60, pan: .38, wet: .12, label: "alto · 9mm pistol" },
  T: { gun: "ak47-shot-01", takes: true, home: 57, hold: .17, fat: 3, gain: .62, pan: -.24, wet: .12, label: "tenor · AK-47" },
  B: { gun: "shotgun-pump-shot-01", takes: true, home: 52, hold: .30, fat: 4, gain: .80, pan: 0, wet: .08, label: "bass · 12 gauge" },
  snare: { gun: "smg-9mm-shot-01", takes: true, home: 60, hold: .11, fat: 3, gain: .42, pan: .1, wet: .16, label: "backbeat · 9mm SMG" },
  hat: { gun: "rifle-dry-fire-01", home: 60, hold: .045, gain: .16, pan: .5, wet: .05, label: "offbeat · .22 dry fire" },
  kick: { gun: "pistol-heavy-shot-03", takes: true, home: 60, hold: .21, fat: 2, gain: .95, pan: 0, wet: .02, lowpass: 900, label: "pulse · 1911 .45" },
  orn: { home: 60, gain: .34, wet: .22, label: "racks, magazines, brass, bursts" },
};

const decode = (gun) => {
  const file = execFileSync("sh", ["-c", `ls "${GUNS}/${gun}".*`]).toString().trim().split("\n")[0];
  const raw = execFileSync("ffmpeg", ["-v", "error", "-i", file, "-ac", "1", "-ar", String(SR), "-f", "f32le", "-"], { maxBuffer: 1 << 28 });
  return new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2).slice();
};
const peak = (x, a = 0, b = x.length) => { let p = 0; for (let i = a; i < b; i++) p = Math.max(p, Math.abs(x[i])); return p; };
const firstOver = (x, level, a = 0, b = x.length) => { for (let i = a; i < b; i++) if (Math.abs(x[i]) > level) return i; return -1; };

// The WAVs hold several shots each. One slice per shot, from 2 ms before the
// first sample over 12 % of that shot's peak.
const sliced = new Map();
function slices(gun, takes) {
  if (sliced.has(gun)) return sliced.get(gun);
  const x = decode(gun), pk = peak(x), starts = [];
  if (takes) {
    for (let i = 0, last = -SR; i < x.length; i++)
      if (Math.abs(x[i]) > .3 * pk) { if (i - last > .25 * SR) starts.push(i); last = i; }
  } else starts.push(firstOver(x, .12 * pk));
  const out = starts.map((s) => {
    const a = Math.max(0, s - 960), local = peak(x, a, Math.min(x.length, s + 2400));
    const on = takes ? firstOver(x, .12 * local, a) : s;
    return x.subarray(Math.max(0, on - 96), Math.min(x.length, on + 3 * SR));
  });
  sliced.set(gun, out);
  return out;
}

// RBJ lowpass, run twice: keeps a sample pitched up from folding over, and
// turns the .45 into a kick.
function lowpass(x, hz) {
  const w = 2 * Math.PI * hz / SR, al = Math.sin(w) / Math.SQRT2, c = Math.cos(w), a0 = 1 + al;
  const b0 = (1 - c) / 2 / a0, b1 = (1 - c) / a0, a1 = -2 * c / a0, a2 = (1 - al) / a0;
  const y = Float32Array.from(x);
  for (let pass = 0; pass < 2; pass++) {
    let x1 = 0, x2 = 0, y1 = 0, y2 = 0;
    for (let i = 0; i < y.length; i++) {
      const v = b0 * y[i] + b1 * x1 + b0 * x2 - a1 * y1 - a2 * y2;
      x2 = x1; x1 = y[i]; y2 = y1; y1 = v; y[i] = v;
    }
  }
  return y;
}

// A hit: one take, played `semis` higher or lower as a sampler would (pitch
// and length together), cut to `hold`, faded. `lead` is where its attack is.
const hits = new Map();
function hit(gun, { semis = 0, hold, take = 0, takes = false, lp = 0, body = 0, fat = 0 }) {
  const key = [gun, semis, Math.round(hold * 1000), take, lp, fat].join(":");
  if (hits.has(key)) return hits.get(key);
  const all = slices(gun, takes);
  let x = all[take % all.length];
  const rate = 2 ** (semis / 12);
  if (rate > 1) x = lowpass(x.subarray(0, Math.min(x.length, Math.ceil((hold + .01) * SR * rate) + 64)), .42 * SR / rate);
  const n = Math.min(Math.floor((x.length - 2) / rate), Math.round(hold * SR));
  let y = new Float32Array(n);
  for (let i = 0; i < n; i++) { const p = i * rate, j = p | 0, f = p - j; y[i] = x[j] * (1 - f) + x[j + 1] * f; }
  if (lp) { const low = lowpass(y, lp); for (let i = 0; i < n; i++) y[i] = low[i] + body * y[i]; }
  const fade = Math.min(n, Math.round(Math.max(.012, hold * .45) * SR));
  for (let i = 0; i < fade; i++) y[n - 1 - i] *= (i / fade) ** 2;
  // a shot is a spike a few ms wide: lean on it, as on a drum bus, so the body counts
  const pk = peak(y) + 1e-9, sat = fat ? Math.tanh(fat) : 1;
  for (let i = 0; i < n; i++) y[i] = fat ? Math.tanh(y[i] / pk * fat) / sat : y[i] / pk;
  const made = { y, lead: Math.max(0, firstOver(y, .12)) };
  hits.set(key, made);
  return made;
}

// ── the score ─────────────────────────────────────────────────────────
const events = [];                    // { voice, step, midi, len, vel, form, gun?, hold? }
const forms = [];                     // { bar, voice, form, on } for the report
const at = (bar, step = 0) => (bar - 1) * BAR + step;
const accent = (t) => (t % 4 === 0 ? 1 : t % 2 === 0 ? .84 : t % 1 === 0 ? .7 : .58);

function enter(voice, l, bar, form, { step = 0, vel = 1, on } = {}) {
  const t0 = at(bar, step);
  l.forEach((n, i) => events.push({ voice, step: t0 + n.t, midi: midiOf(n.d), len: n.len, vel: vel * accent(n.t) * (i === 0 ? 1.12 : 1), form }));
  forms.push({ bar: bar + step / BAR, voice, form, on: on ?? name(midiOf(l[0].d)) });
}
const strike = (voice, bar, step, { midi = KIT[voice].home, vel = 1, len = 2, form = "pulse", ...rest } = {}) =>
  events.push({ voice, step: at(bar, step), midi, len, vel, form, ...rest });
const orn = (gun, bar, step, hold, vel = 1, pan = 0, semis = 0) =>
  events.push({ voice: "orn", step: at(bar, step), midi: 60 + semis, len: hold / STEP, vel, form: "ornament", gun, hold, pan });

const up = (l, interval, more = {}) => transform(l, { interval, ...more });
const stretto = strettoDistances(SUBJECT);
const closest = (invert, intervals) => stretto.filter((s) => s.ok && s.invert === invert && intervals.includes(s.interval)).sort((a, b) => a.delay - b.delay)[0];
const STRETTO = closest(false, [4]), MIRROR = closest(true, [7]);
const placed = {};                    // the lines of each stretto, to check every pair afterwards
const fits = (ls) => Object.entries(ls).flatMap(([a, la], i) => Object.entries(ls).slice(i + 1).map(([b, lb]) => {
  const met = meetings(la, lb);
  return `${a}/${b} ${met.filter((m) => isConsonant(m.semis)).length}/${met.length}`;
})).join(" · ");

// 1–2 · loading
orn("pistol-9mm-mag-insert-01", 1, 6, .3, .8, -.4);
orn("ak47-mag-insert-01", 1, 14, .3, .7, .4);
orn("pistol-heavy-slide-rack-01", 2, 12, .42, 1, .2);

// 3–10 · exposition: alto, soprano, tenor, bass
enter("A", SUBJECT, 3, "subject");
enter("S", ANSWER, 5, "answer");
enter("A", up(COUNTER, -14), 5, "countersubject", { on: "below" });
enter("T", up(SUBJECT, -7), 7, "subject");
enter("S", up(COUNTER, -4), 7, "countersubject", { on: "above" });
enter("B", up(ANSWER, -14), 9, "answer");
enter("T", up(COUNTER, -14), 9, "countersubject", { on: "above" });
enter("S", up(RUN, 7, { delay: 28 }), 9, "fragment", { on: "run" });
orn("pistol-9mm-mag-insert-01", 6, 14, .25, .8, .5);
orn("shotgun-pump-rack-01", 10, 10, .6, 1, -.3);

// 11–14 · episode: the head in sequence down a step a bar, soprano answering
// the alto at the half bar, over a bass falling in fifths (digest 02: the
// bass's leaps go down a fifth or up a fourth almost three times in four)
const FIFTHS = [-7, -11, -8, -12, -9, -13, -10, -14];
for (let k = 0; k < 4; k++) {
  enter("A", up(HEAD, -k), 11 + k, "fragment", { on: "head" });
  enter("S", up(HEAD, 3 - k), 11 + k, "fragment", { step: 8, on: "head" });
  enter("T", up(RUN, -k - 7), 11 + k, "fragment", { step: 12, vel: .9, on: "run" });
  for (const half of [0, 1]) {
    const midi = midiOf(FIFTHS[2 * k + half]);
    strike("B", 11 + k, 8 * half, { midi, len: 6, form: "sequence" });
    strike("B", 11 + k, 8 * half + 6, { midi, len: 2, vel: .7, form: "sequence" });
  }
}
forms.push({ bar: 11, voice: "B", form: "sequence", on: "D G C F Bb E A D" });
orn("casing-drop-02", 11, 2, .7, .7, .5);
orn("smg-9mm-bolt-lock-01", 14, 12, .4, .9, .35);

// 15–18 · middle entries: the subject in F with the countersubject now
// underneath (the invertible pair turned over), then the subject upside down
enter("A", up(SUBJECT, 2), 15, "subject", { on: "F4 (relative major)" });
enter("T", up(COUNTER, -2 - 14), 15, "countersubject", { on: "below" });
for (const b of [15, 16]) for (const s of [0, 10]) strike("B", b, s, { midi: midiOf(2 - 14), len: 4, form: "sequence", vel: s ? .75 : 1 });
enter("B", up(SUBJECT, -14, { invert: true }), 17, "inversion");
enter("S", up(COUNTER, 14, { invert: true }), 17, "inversion", { on: "countersubject, mirrored" });
orn("pistol-heavy-slide-rack-01", 18, 12, .42, 1, -.3);

// 19–22 · mensuration canon, 4 : 2 : 1 — bass at half speed, alto as
// written, soprano at double speed, all from the same downbeat
enter("B", up(SUBJECT, -7, { scale: 2 }), 19, "augmentation");
enter("A", SUBJECT, 19, "subject");
enter("A", ANSWER.map((n) => ({ ...n, d: n.d - 7 })), 21, "answer");
enter("T", up(SUBJECT, -7, { invert: true }), 21, "inversion");
for (let k = 0; k < 4; k++) enter("S", up(k % 2 ? ANSWER : SUBJECT, k % 2 ? 0 : 7, { scale: .5 }), 19 + k, "diminution", { vel: .85 });
orn("casing-shotgun-shell-drop-01", 19, 3, .8, .7, -.5);
orn("shotgun-pump-rack-01", 22, 10, .6, 1, .3);

// 23–26 · break: the subject backwards alone, then forwards and backwards at once
enter("A", up(SUBJECT, 0, { retrograde: true }), 23, "retrograde");
enter("T", up(SUBJECT, -7), 25, "subject");
enter("A", up(SUBJECT, 0, { retrograde: true }), 25, "retrograde", { on: "crab, against the tenor" });
orn("casing-drop-01", 23, 2, .9, .6, .5);
orn("m4-bolt-rack-01", 24, 12, .45, .8, -.4);
for (let s = 8; s < 16; s++) strike("snare", 26, s, { vel: .5 + .09 * (s - 8), len: 1, form: "pulse" });
orn("smg-9mm-burst-01", 26, 12, 4 * STEP * .96, .9, .3);

// 27–30 · stretto at the one delay the subject bears cleanly against itself
// (three beats, at the fifth): bass and tenor, then alto and soprano the same
// way two bars on. The soprano's tail hangs over into bar 31.
const order = ["B", "T", "A", "S"], octave = { B: -14, T: -7, A: 0, S: 7 };
placed.rectus = {};
order.forEach((v, i) => {
  const l = up(SUBJECT, octave[v] + (i % 2 ? STRETTO.interval - 7 : 0), { delay: (i >> 1) * LEN + (i % 2) * STRETTO.delay });
  placed.rectus[v] = l;
  enter(v, l, 27, "subject", { on: `${name(midiOf(l[0].d))}, stretto +${l[0].t / 4} beats` });
});
orn("casing-drop-02", 27, 2, .7, .6, -.5);
orn("pistol-heavy-slide-rack-01", 30, 12, .42, 1, .3);

// 31–34 · stretto again in two pairs, each answered upside down at the
// octave; the tenor's pair sits a fifth above the bass's, three beats behind
placed.mirror = {
  B: up(SUBJECT, -14),
  A: up(SUBJECT, 0, { invert: true, delay: MIRROR.delay }),
  T: up(SUBJECT, -7 + STRETTO.interval - 7, { delay: STRETTO.delay }),
  S: up(SUBJECT, STRETTO.interval, { invert: true, delay: STRETTO.delay + MIRROR.delay }),
};
for (const [v, l] of Object.entries(placed.mirror))
  enter(v, l, 31, "AS".includes(v) ? "inversion" : "subject", { on: `${name(midiOf(l[0].d))}, stretto +${l[0].t / 4} beats` });
for (const v of order) strike(v, 34, 12, { midi: midiOf(octave[v] + { B: 4, T: 4, A: 1, S: 4 }[v]), len: 2, form: "fragment" });
orn("shotgun-pump-rack-01", 34, 10, .6, 1, -.3);

// 35–36 · coda: all four on the subject's first bar in octaves, the run, a
// dominant, and a major third in the last chord (digest 03: 139 of 177
// minor-third chorales end major)
for (const v of order) {
  enter(v, up(SUBJECT.filter((n) => n.t < 16), octave[v]), 35, "subject", { on: "tutti, first bar" });
  enter(v, up(RUN, octave[v] + 1), 36, "fragment", { on: "run" });
  for (const s of [4, 8, 10]) strike(v, 36, s, { midi: midiOf(octave[v] + { B: 4, T: 4, A: 1, S: 4 }[v]), len: 2, form: "fragment", vel: .95 });
}
const FINAL = { B: 38, T: 57, A: 66, S: 74 };            // D2 · A3 · F#4 · D5
for (const v of order) strike(v, 37, 0, { midi: FINAL[v], len: 16, vel: 1.15, form: "cadence", hold: v === "B" ? 1.4 : .5 });
orn("smg-9mm-burst-01", 36, 12, 4 * STEP * .96, 1, .3);
orn("sniper-shot-tail-01", 37, 0, 2.8, 1.1, 0, -3);
orn("casing-shotgun-shell-drop-01", 37, 5, 1, .7, -.45);
orn("casing-drop-01", 37, 11, 1.1, .6, .45);

// the pulse under all of it: the .45 on every beat (dominant under the two
// pedals), a dry click on the off eighths, an SMG on two and four
for (let b = 1; b <= BARS; b++) {
  const out = b === 23 || b === 24, half = b === 25 || b === 26;
  const pedal = (b === 34 || b === 36) ? -5 : 0;
  for (let q = 0; q < 4; q++) {
    if (!out && !(half && q % 2) && !(b === 36 && q === 3)) strike("kick", b, 4 * q, { midi: 50 + pedal, vel: q ? .92 : 1 });
    if (b >= 2 && !(b === 36 && q > 1)) strike("hat", b, 4 * q + 2, { vel: q % 2 ? 1 : .8, midi: 65 });
    if (q % 2 && b >= 7 && !(b >= 23 && b <= 26) && b < 36) strike("snare", b, 4 * q, { midi: 62 });
  }
}
strike("kick", 37, 0, { midi: 50, vel: 1.1, hold: .5 });

// ── render ────────────────────────────────────────────────────────────
const bus = Object.fromEntries(Object.keys(KIT).map((k) => [k, { L: new Float32Array(N), R: new Float32Array(N), send: new Float32Array(N) }]));
const turn = {};
for (const e of events.sort((a, b) => a.step - b.step)) {
  const k = KIT[e.voice], gun = e.gun ?? k.gun;
  const semis = Math.max(-17, Math.min(15, e.midi - k.home));
  const hold = e.hold ?? Math.min(k.hold, Math.max(k.hold * .5, e.len * STEP * .95));
  const take = k.takes ? (turn[e.voice] = (turn[e.voice] ?? -1) + 1) : 0;
  const { y, lead } = hit(gun, { semis, hold, take, takes: !!k.takes, lp: k.lowpass, body: .12, fat: e.gun ? 1.5 : k.fat });
  const pan = e.pan ?? k.pan, th = (pan + 1) * Math.PI / 4, g = k.gain * e.vel;
  const b = bus[e.voice], i0 = Math.round(e.step * STEP * SR) - lead;
  e.semis = semis; e.time = e.step * STEP;
  for (let i = Math.max(0, -i0); i < y.length && i0 + i < N; i++) {
    const v = y[i] * g;
    b.L[i0 + i] += v * Math.cos(th); b.R[i0 + i] += v * Math.sin(th); b.send[i0 + i] += v * k.wet;
  }
}

// a small room: sparse early reflections, different in each ear, gone in a third of a second
function room(send, seed) {
  let s = seed;
  const rnd = () => ((s = (s * 1664525 + 1013904223) >>> 0) / 2 ** 32);
  const out = new Float32Array(N);
  for (let k = 0; k < 40; k++) {
    const ms = 9 + 300 * rnd() ** 1.6, d = Math.round(ms * SR / 1000), g = (rnd() < .5 ? -1 : 1) * .5 * 10 ** (-3 * ms / 330);
    for (let i = d; i < N; i++) out[i] += send[i - d] * g;
  }
  return lowpass(out, 6500);
}
const L = new Float32Array(N), R = new Float32Array(N), send = new Float32Array(N);
for (const b of Object.values(bus)) for (let i = 0; i < N; i++) { L[i] += b.L[i]; R[i] += b.R[i]; send[i] += b.send[i]; }
const wetL = room(send, 7), wetR = room(send, 1311);
for (let i = 0; i < N; i++) { L[i] += wetL[i]; R[i] += wetR[i]; }

// master: drive into a soft knee, then a 5 ms look-ahead limiter under -1 dBTP
const DRIVE = Number(process.env.GUNDRUM_DRIVE ?? 1.9), CEIL = 10 ** (-1.4 / 20), LOOK = 240;
const soft = (v) => { const a = Math.abs(v); return a < .5 ? v : Math.sign(v) * (.5 + .5 * Math.tanh((a - .5) / .5)); };
const need = new Float32Array(N);
for (let i = 0; i < N; i++) { L[i] = soft(L[i] * DRIVE); R[i] = soft(R[i] * DRIVE); }
// the limiter watches the peaks between samples too (a 16-tap sinc at the three quarter points)
const SINC = [.25, .5, .75].map((f) => Array.from({ length: 16 }, (_, k) => {
  const x = k - 7 - f, w = .5 + .5 * Math.cos(Math.PI * x / 8);
  return x === 0 ? 1 : Math.sin(Math.PI * x) / (Math.PI * x) * w;
}));
const between = (x, i) => {
  let p = Math.abs(x[i]);
  if (p < .3 || i < 8 || i > N - 9) return p;
  for (const taps of SINC) { let v = 0; for (let k = 0; k < 16; k++) v += x[i - 7 + k] * taps[k]; p = Math.max(p, Math.abs(v)); }
  return p;
};
for (let i = 0, g = 1; i < N; i++) {
  const p = Math.max(between(L, i), between(R, i), i ? between(L, i - 1) : 0, i ? between(R, i - 1) : 0);
  g = Math.min(p > CEIL ? CEIL / p : 1, g + (1 - g) * .0004);
  need[i] = g;
}
const ahead = new Float32Array(N), ML = new Float32Array(N), MR = new Float32Array(N);
for (let i = 0; i < N; i++) { let m = 1; for (let j = i, e = Math.min(N, i + LOOK); j < e; j++) if (need[j] < m) m = need[j]; ahead[i] = m; }
for (let i = 0, sum = 0; i < N; i++) {                  // every window averaged here already saw sample i coming
  sum += ahead[i] - (i >= LOOK ? ahead[i - LOOK] : 0);
  const g = sum / Math.min(LOOK, i + 1);
  ML[i] = L[i] * g; MR[i] = R[i] * g;
}
const fadeN = Math.round(.6 * SR);
for (let i = 0; i < fadeN; i++) { const g = (i / fadeN) ** 2; ML[N - 1 - i] *= g; MR[N - 1 - i] *= g; }

// ── write ─────────────────────────────────────────────────────────────
mkdirSync(OUT, { recursive: true });
const wav = Buffer.alloc(44 + N * 6);
wav.write("RIFF", 0); wav.writeUInt32LE(36 + N * 6, 4); wav.write("WAVEfmt ", 8); wav.writeUInt32LE(16, 16);
wav.writeUInt16LE(1, 20); wav.writeUInt16LE(2, 22); wav.writeUInt32LE(SR, 24); wav.writeUInt32LE(SR * 6, 28);
wav.writeUInt16LE(6, 32); wav.writeUInt16LE(24, 34); wav.write("data", 36); wav.writeUInt32LE(N * 6, 40);
for (let i = 0; i < N; i++) {
  wav.writeIntLE(Math.round(Math.max(-1, Math.min(1, ML[i])) * 8388607), 44 + i * 6, 3);
  wav.writeIntLE(Math.round(Math.max(-1, Math.min(1, MR[i])) * 8388607), 47 + i * 6, 3);
}
const WAV = resolve(OUT, "gundrum.wav"), MP3 = resolve(OUT, "gundrum.mp3");
writeFileSync(WAV, wav);
// the encoder overshoots the limiter by a few tenths of a dB, so the mp3 is cut half a dB lower
execFileSync("ffmpeg", ["-v", "error", "-y", "-i", WAV, "-af", "volume=-0.5dB", "-c:a", "libmp3lame", "-b:a", "320k", MP3]);

// ── measure (nothing is played) ───────────────────────────────────────
const dB = (v) => +(20 * Math.log10(v + 1e-12)).toFixed(1);
const loudness = (file) => {
  const log = spawnSync("ffmpeg", ["-nostats", "-i", file, "-af", "ebur128=peak=true", "-f", "null", "-"]).stderr.toString();
  const tail = log.slice(log.lastIndexOf("Summary:"));
  const num = (re) => Number(tail.match(re)?.[1]);
  return { lufs: num(/I:\s+(-?[\d.]+) LUFS/), lra: num(/LRA:\s+(-?[\d.]+) LU/), truePeak: num(/Peak:\s+(-?[\d.]+) dBFS/) };
};

// where each scheduled hit's attack really is, in its own voice's bus
const levels = {};
for (const [v, b] of Object.entries(bus)) {
  const mine = events.filter((e) => e.voice === v);
  let sq = 0, pk = 0;
  for (let i = 0; i < N; i++) { const m = (b.L[i] ** 2 + b.R[i] ** 2); sq += m; pk = Math.max(pk, Math.abs(b.L[i]), Math.abs(b.R[i])); }
  const off = [];
  mine.forEach((e, i) => {
    if (v === "orn" || (i && e.time - mine[i - 1].time < .1)) return;   // only attacks out of near-silence can be timed
    const c = Math.round(e.time * SR), a = Math.max(0, c - 480), z = Math.min(N, c + 960);
    let top = 0; for (let j = a; j < z; j++) top = Math.max(top, Math.abs(b.L[j]) + Math.abs(b.R[j]));
    for (let j = a; j < z; j++) if (Math.abs(b.L[j]) + Math.abs(b.R[j]) > .12 * top) { off.push((j - c) / SR * 1000); break; }
  });
  levels[v] = {
    hits: mine.length, rms: dB(Math.sqrt(sq / N)), peak: dB(pk),
    timed: off.length, worstMs: +Math.max(0, ...off.map(Math.abs)).toFixed(2),
    semis: [Math.min(...mine.map((e) => e.semis)), Math.max(...mine.map((e) => e.semis))],
    range: [name(Math.min(...mine.map((e) => e.midi))), name(Math.max(...mine.map((e) => e.midi)))],
  };
}

// and independently in the finished mix: do its attacks sit on the grid?
function gridFit() {
  const dryL = new Float32Array(N), dryR = new Float32Array(N);
  for (const [v, b] of Object.entries(bus)) if (v !== "orn") for (let i = 0; i < N; i++) { dryL[i] += b.L[i]; dryR[i] += b.R[i]; }
  const hop = 48, n = Math.floor(N / hop), env = new Float32Array(n);
  for (let i = 0; i < n; i++) { let p = 0; for (let j = 0; j < hop; j++) p = Math.max(p, Math.abs(dryL[i * hop + j]) + Math.abs(dryR[i * hop + j])); env[i] = p; }
  const errs = [];
  for (let i = 12; i < n; i++) {
    let before = 0; for (let j = i - 12; j < i - 1; j++) before = Math.max(before, env[j]);
    if (env[i] > .15 && env[i] > 2.2 * before) { const t = i * hop / SR, g = STEP / 2; errs.push(Math.abs(t - Math.round(t / g) * g) * 1000); i += 20; }
  }
  errs.sort((a, b) => a - b);
  return { attacks: errs.length, medianMs: +errs[errs.length >> 1].toFixed(2), within3ms: +(errs.filter((e) => e <= 3).length / errs.length).toFixed(3), worstMs: +errs.at(-1).toFixed(2) };
}

const report = {
  bpm: BPM, key: "D minor", bars: BARS, seconds: +(N / SR).toFixed(2),
  subject: { rhythm: toBox(rhythm(SUBJECT.map((n) => n.t), LEN)), notes: SUBJECT.map((n) => name(midiOf(n.d))).join(" ") },
  answer: ANSWER.map((n) => name(midiOf(n.d))).join(" "),
  countersubject: {
    rhythm: toBox(rhythm(COUNTER.map((n) => n.t), LEN)), notes: COUNTER.map((n) => name(midiOf(n.d))).join(" "),
    interlocksAnswer: interlocks(rhythm(SUBJECT.map((n) => n.t), LEN), rhythm(COUNTER.map((n) => n.t), LEN)),
    cantus: CS.cantus.map(name).join(" "), skeleton: CS.sk.map(name).join(" "),
    firstSpecies: { ok: CS.check.ok, violations: CS.check.violations, warnings: CS.check.warnings.map((w) => `${w.index}:${w.rule}`) },
    invertibleAtOctave: invertibleAt(skeletonLine(CS.sk), skeletonLine(CS.cantus)),
    againstSubject: meetings(up(skeletonLine(CS.sk), -4), skeletonLine(skeleton(SUBJECT))).map((m) => `${m.beat}:${m.semis}${isConsonant(m.semis) ? "" : "!"}`).join(" "),
  },
  stretto: { used: STRETTO, mirrored: MIRROR, pairsBars27: fits(placed.rectus), pairsBars31: fits(placed.mirror), table: stretto.map((s) => `${s.invert ? "inv " : ""}+${s.delay / 4}beat @${s.interval}: ${s.good}/${s.of}`) },
  form: forms.sort((a, b) => a.bar - b.bar),
  levels,
  grid: gridFit(),
  loudness: { wav: loudness(WAV), mp3: loudness(MP3) },
  drive: DRIVE,
};
function skeletonLine(sk) { return sk.map((m, i) => ({ t: i * 4, d: degOf(m), len: 4 })); }
writeFileSync(resolve(OUT, "gundrum.json"), JSON.stringify({ ...report, kit: KIT, events }, null, 1));

// ── the timeline ──────────────────────────────────────────────────────
const plot = String.raw`
import json, sys
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
d = json.load(open(sys.argv[1])); ev = d["events"]
INK, MUTE, GRID, PAPER = "#0b0b0b", "#52514e", "#e4e3df", "#fcfcfb"
HUE = {"subject": "#2a78d6", "answer": "#eb6834", "countersubject": "#1baf7a", "inversion": "#eda100",
       "augmentation": "#e87ba4", "diminution": "#008300", "retrograde": "#6250d6", "fragment": "#e34948"}
GREY = "#a9a8a2"
LANES = ["S", "A", "T", "B", "snare", "hat", "kick", "orn"]
SAY = {"S": "soprano\n9mm dry fire", "A": "alto\n9mm pistol", "T": "tenor\nAK-47", "B": "bass\n12 gauge",
       "snare": "SMG", "hat": "dry click", "kick": ".45 pulse", "orn": "racks, brass"}
TALL = {"S": 3, "A": 3, "T": 3, "B": 3, "snare": .7, "hat": .7, "kick": .7, "orn": .7}
base, y = {}, 0
for v in reversed(LANES): base[v] = y; y += TALL[v] + .35
span = {v: (min(e["midi"] for e in ev if e["voice"] == v), max(e["midi"] for e in ev if e["voice"] == v)) for v in LANES}
rows = [(1, 19), (19, 38.6)]
fig, axes = plt.subplots(2, 1, figsize=(19, 12), dpi=110, facecolor=PAPER)
for ax, (b0, b1) in zip(axes, rows):
    ax.set_facecolor(PAPER)
    for b in range(int(b0), int(b1) + 1):
        ax.axvline(b, color=GRID, lw=1.4 if (b - 1) % 4 == 0 else .6, zorder=0)
    for e in ev:
        bar = 1 + e["step"] / 16
        if not (b0 <= bar < b1): continue
        v = e["voice"]; lo, hi = span[v]
        yy = base[v] + (TALL[v] - .3) * ((e["midi"] - lo) / max(1, hi - lo) if TALL[v] > 1 else .5)
        w = max(e["len"] / 16 * .9, .028)
        ax.barh(yy, w, left=bar, height=.3 if TALL[v] > 1 else .42, color=HUE.get(e["form"], GREY), align="edge", lw=0, zorder=2)
    ax.set_xlim(b0, b1); ax.set_ylim(-.3, y)
    ax.set_yticks([base[v] + TALL[v] / 2 for v in LANES]); ax.set_yticklabels([SAY[v] for v in LANES], fontsize=11, color=INK)
    ax.set_xticks(range(int(b0), int(b1) + 1)); ax.tick_params(axis="x", labelsize=11, colors=MUTE, length=0); ax.tick_params(axis="y", length=0)
    for s in ax.spines.values(): s.set_visible(False)
    ax.set_xlabel("bar", fontsize=11, color=MUTE)
axes[0].set_title(f"gundrum: which voice has what, bar by bar ({d['bpm']} BPM, {d['key']}; height within a voice is pitch)", loc="left", fontsize=15, color=INK, pad=34)
axes[0].legend(handles=[Patch(color=c, label=k) for k, c in HUE.items()] + [Patch(color=GREY, label="pulse, sequence bass, ornaments")],
               ncol=9, loc="lower left", bbox_to_anchor=(0, 1.0), frameon=False, fontsize=11, labelcolor=MUTE, handlelength=1.2, columnspacing=1.4)
fig.tight_layout(); fig.savefig(sys.argv[2], facecolor=PAPER)
`;
const PNG = resolve(OUT, "gundrum-timeline.png");
const drew = spawnSync(PY, ["-c", plot, resolve(OUT, "gundrum.json"), PNG]);
if (drew.status) console.error(drew.stderr.toString());

const { form, levels: lv, ...short } = report;
console.log(JSON.stringify(short, null, 1));
console.table(lv);
console.table(form);
