#!/usr/bin/env node
// render-frothyscreama.mjs — frothyscreama. A screamo track: quiet twinkle,
// then everything at once, then quiet again, then the long scream out.
// The voice is jeffrey-pvc screaming whole lines (bin/scream.mjs renders and
// caches the takes); the guitars are the physical strum engine
// (pop/guitar/c/strum) clean and then through a wall of tanh; the bass and
// the kit are sampled (bin/fetch-samples.mjs).
//
// This file is the composition. The sound is made by the C engine
// (c/frothyscreama.c): the arrangement below is written out as a score, every
// random choice already made, and the engine renders it. The voices here are
// the engine's reference — the same DSP in JS, kept so c/compare.mjs can hold
// the two to each other sample for sample.
//
//   node pop/frothyscreama/bin/scream.mjs                    the takes (billable, cached)
//   node pop/frothyscreama/bin/render-frothyscreama.mjs      score → C engine → out/frothyscreama.mp3
//   node pop/frothyscreama/bin/render-frothyscreama.mjs --form    print the form
//   node pop/frothyscreama/bin/render-frothyscreama.mjs --material none
//   node pop/frothyscreama/bin/render-frothyscreama.mjs --bake <score.txt>   write the score and stop
//   node pop/frothyscreama/bin/render-frothyscreama.mjs --js           render with the JS reference instead
//   node pop/frothyscreama/bin/render-frothyscreama.mjs --js --stems   …and write per-bus wavs
//   node pop/frothyscreama/bin/render-frothyscreama.mjs --js --bake <score.txt>
//        the score plus the JS premaster to hold the engine against; stops before the master

import { mkdirSync, readFileSync, writeFileSync, existsSync, statSync, openSync, writeSync, closeSync, mkdtempSync, rmSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { tmpdir } from "node:os";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";
import * as progress from "../../lib/render-progress.mjs";
import { applyMaterial } from "../../lib/material.mjs";
import { lyric, takePath } from "./scream.mjs";
import { KIT, kitPath, bassPath } from "./fetch-samples.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
const VOX = resolve(OUT, "vox");
mkdirSync(VOX, { recursive: true });
const STRUM = resolve(HERE, "../../guitar/c/strum");
const FFMPEG = existsSync("/opt/homebrew/bin/ffmpeg") ? "/opt/homebrew/bin/ffmpeg" : "ffmpeg";
const flag = (n, d) => { const i = process.argv.indexOf(n); return i > 0 ? process.argv[i + 1] : d; };
const MATERIAL = flag("--material", "tape");
const TARGET_LUFS = -10.5;

// ── bake ────────────────────────────────────────────────────────────────
// The score the C engine reads: one line per event, in the order played here,
// every random choice already made. Decoded samples go beside it as raw f32.
// Without --js nothing is synthesized here: the voices only write their events.
const JS = process.argv.includes("--js");
const CANON = !JS && !flag("--bake", null);                  // the plain run: score, engine, master
const BAKE = flag("--bake", JS ? null : resolve(OUT, "frothyscreama.score.txt")), baked = [], bakeIds = new Map();
const BAKE_DIR = BAKE && resolve(dirname(resolve(BAKE)), ".c-bake");
if (BAKE) mkdirSync(BAKE_DIR, { recursive: true });
const ev = BAKE ? (...a) => baked.push(a.join(" ")) : () => {};
// A sample's id in the score. With `data`, the decoded floats are written out
// once (named by the source's hash); without, `key` is a file the engine reads as is.
function bakeSmp(key, data) {
  if (!bakeIds.has(key)) {
    let path = key;
    if (data) {
      path = join(BAKE_DIR, `${createHash("sha1").update(key).digest("hex").slice(0, 12)}.f32`);
      if (!existsSync(path) || statSync(path).size !== data.byteLength) writeFileSync(path, Buffer.from(data.buffer, data.byteOffset, data.byteLength));
    }
    bakeIds.set(key, bakeIds.size);
    baked.push(`smp ${bakeIds.get(key)} ${path}`);
  }
  return bakeIds.get(key);
}

const SR = 48_000, TAU = Math.PI * 2;
const BPM = 180, BEAT = 60 / BPM, BAR = 4 * BEAT;

// ── harmony: B minor, with the emo colours ──────────────────────────────
// quiet: the ninth and the major seventh ring open. loud: the same roots as fifths.
const QUIET = ["Bm9", "Gmaj7", "Dadd9", "F#m7"];
const LOUD = ["B5", "G5", "D5", "F#5"];
const COLOUR = ["Bm9", "Gmaj7", "Dadd9", "F#m7"];
const SLOW = ["Em9", "Gmaj7", "Bm", "F#7sus4"];
const ROOTS = { Bm9: 35, Gmaj7: 31, Dadd9: 38, "F#m7": 30, B5: 35, G5: 31, D5: 38, "F#5": 30, Em9: 28, Bm: 35, "F#7sus4": 30 };
const TWINKLE = {       // eight notes a bar, picked one at a time
  Bm9: [59, 66, 73, 74, 69, 66, 73, 78], Gmaj7: [55, 62, 66, 71, 74, 71, 66, 78],
  Dadd9: [62, 69, 76, 78, 74, 69, 76, 81], "F#m7": [54, 61, 64, 69, 73, 69, 64, 76],
};

// ── form ────────────────────────────────────────────────────────────────
// drums: quiet | dbeat | blast | slow (half-time triplets) | build | 0
// vox: [bar, line, { bars, arp, back }] — the bar is a whole number (it picks
// the chord the scream is tuned to). `back: pan` is a second screamer
// answering from one side, quieter, while the lead line is still going.
const FORM = [
  { name: "kettle",    bars: 16, chords: QUIET, drums: "quiet", drumsFrom: 4, twinkle: 1, clean: 0.7, bass: "long", bassFrom: 8,
    vox: [[4, "w1"], [10, "w2"], [14, "s17", { bars: 2 }]] },
  { name: "boil over", bars: 16, chords: LOUD, colour: COLOUR, drums: "dbeat", loud: 1, bass: "eighths", crash: true,
    vox: [[0, "s1", { bars: 4 }], [3, "s16", { bars: 2, back: -0.7 }], [5, "s2", { bars: 4 }], [8, "s15", { bars: 2, back: 0.7 }], [10, "s3", { bars: 5.5 }], [13, "s14", { bars: 2.5, back: -0.7 }]] },
  { name: "blast",     bars: 12, chords: LOUD, colour: COLOUR, drums: "blast", loud: 1, bass: "eighths", crash: true, fill: true,
    vox: [[0, "s13", { bars: 3.8 }], [4, "s4", { bars: 6.5 }], [9, "s16", { bars: 3, back: 0.7 }]] },
  { name: "collapse",  bars: 12, chords: SLOW, drums: "slow", loud: 1, slow: true, bass: "slow", crash: true,
    vox: [[1, "s6", { bars: 4.5 }], [4, "s14", { bars: 2.5, back: 0.7 }], [6, "s9", { bars: 5.5 }], [9, "s15", { bars: 3, back: -0.7 }]] },
  { name: "the house", bars: 16, chords: QUIET, drums: "quiet", twinkle: 1, clean: 0.6, bass: "long", build: 4,
    vox: [[1, "w3"], [9, "w4"], [12, "s10", { bars: 4 }]] },
  { name: "foam",      bars: 12, chords: LOUD, colour: COLOUR, drums: "blast", loud: 1, bass: "eighths", crash: true,
    vox: [[0, "s5", { bars: 3.8 }], [3, "s17", { bars: 2, back: -0.7 }], [4, "s2", { bars: 3.8 }], [8, "s11", { bars: 3.8 }], [10, "s14", { bars: 2, back: 0.7 }]] },
  { name: "froth",     bars: 8,  chords: LOUD, colour: COLOUR, drums: "dbeat", loud: 1, bass: "eighths", crash: true, fill: true,
    vox: [[0, "s7", { bars: 4.8 }], [3, "s16", { bars: 2, back: 0.7 }], [5, "s6", { bars: 3 }], [6, "s15", { bars: 2, back: -0.7 }]] },
  { name: "more",      bars: 8,  chords: LOUD, colour: COLOUR, drums: "blast", loud: 1, bass: "eighths", crash: true, fill: true,
    vox: [[0, "s12", { bars: 3.8 }], [2, "s13", { bars: 2.5, back: -0.7 }], [4, "s10", { bars: 3.8, arp: true }], [6, "s17", { bars: 2, back: 0.7 }]] },
  { name: "scream out", bars: 10, chords: SLOW, drums: "slow", loud: 1, slow: true, bass: "slow", crash: true,
    vox: [[0, "s8", { bars: 4.8 }], [3, "s11", { bars: 3, back: -0.7 }], [5, "s9", { bars: 6, arp: true }], [7, "s10", { bars: 3.5, back: 0.7 }]] },
  { name: "steam",     bars: 6,  chords: ["Bm9"], twinkle: 0.7, clean: 0.6, ring: true, vox: [[2, "w5"]] },
];
let _b = 0;
for (const s of FORM) { s.at = _b; _b += s.bars; }
const BARS = _b, DUR = BARS * BAR + 6, N = Math.ceil(DUR * SR);
const clock = (s) => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;
if (process.argv.includes("--form")) {
  for (const s of FORM) console.log(`${clock(s.at * BAR).padEnd(5)} ${String(s.bars).padStart(2)} bars  ${s.name.padEnd(11)} ${s.drums || "—"}  ${(s.vox || []).map((v) => v[1]).join(" ")}`);
  console.log(`${BARS} bars · ${BPM} BPM · ${clock(DUR)}`);
  process.exit(0);
}

// ── buses ───────────────────────────────────────────────────────────────
const st = () => new Float32Array(N * 2);
const gtr = st(), clean = st(), drums = st(), vox = st(), rev = st();
const bass = new Float32Array(N), dly = new Float32Array(N);
const mtof = (m) => 440 * 2 ** ((m - 69) / 12);
let _n = 0xf207f207;
const noise = () => { _n ^= _n << 13; _n ^= _n >>> 17; _n ^= _n << 5; return (_n >>> 0) / 2147483648 - 1; };
let _r = 0x5c2ea4a;
const rnd = () => ((_r = (_r * 1664525 + 1013904223) >>> 0) / 4294967296);
const panL = (p) => Math.cos((p + 1) * Math.PI / 4), panR = (p) => Math.sin((p + 1) * Math.PI / 4);

function readWav(path) {
  const buf = readFileSync(path);
  let i = 12, ch = 2, bits = 32, fmt = 3;
  while (i < buf.length - 8) {
    const id = buf.toString("ascii", i, i + 4), size = buf.readUInt32LE(i + 4), body = i + 8;
    if (id === "fmt ") { fmt = buf.readUInt16LE(body); ch = buf.readUInt16LE(body + 2); bits = buf.readUInt16LE(body + 14); }
    else if (id === "data") {
      const frames = Math.floor(Math.min(size, buf.length - body) / (ch * bits / 8)), L = new Float32Array(frames), R = new Float32Array(frames);
      for (let f = 0; f < frames; f++) {
        const o = body + f * ch * bits / 8, rd = (k) => (fmt === 3 ? buf.readFloatLE(o + k * 4) : buf.readInt16LE(o + k * 2) / 32768);
        L[f] = rd(0); R[f] = ch > 1 ? rd(1) : L[f];
      }
      return { L, R };
    }
    i = body + size + (size & 1);
  }
  throw new Error(`no data chunk in ${path}`);
}
const tmp = mkdtempSync(join(tmpdir(), "froth-"));

// ── guitars ─────────────────────────────────────────────────────────────
function strum(args) {
  const out = join(BAKE ? BAKE_DIR : tmp, `g${Math.floor(rnd() * 1e9)}.wav`);
  const r = spawnSync(STRUM, [...args.map(String), "--bpm", BPM, "--out", out], { stdio: ["ignore", "ignore", "ignore"] });
  if (r.status !== 0) throw new Error(`strum failed: ${args.join(" ")}`);
  return Object.assign(readWav(out), { path: out });
}
// The wall: the engine's amp, then two more tanh stages and a cabinet. One
// performance per ear, different seeds, so the two never line up.
function wall(g, drive) {
  for (const ch of JS ? [g.L, g.R] : []) {
    let hp = 0, lp1 = 0, lp2 = 0;
    for (let i = 0; i < ch.length; i++) {
      hp += 0.012 * (ch[i] - hp);
      let x = Math.tanh((ch[i] - hp) * drive);
      x = Math.tanh(x * 2.2 + 0.08) - 0.08;
      lp1 += 0.48 * (x - lp1); lp2 += 0.48 * (lp1 - lp2);
      ch[i] = lp2;
    }
  }
  g.drive = drive;
  return g;
}
function place(bus, g, t, gain, pan) {
  ev("place", bus === gtr ? "gtr" : "clean", t, gain, pan, g.drive ?? 0, g.path);
  if (!JS) return;
  const i0 = Math.round(t * SR), gl = gain * panL(pan), gr = gain * panR(pan);
  for (let k = 0; k < g.L.length && i0 + k < N; k++) {
    const m = 0.5 * (g.L[k] + g.R[k]), j = (i0 + k) * 2;
    bus[j] += m * gl; bus[j + 1] += m * gr;
    rev[j] += m * gl * 0.12; rev[j + 1] += m * gr * 0.12;
  }
}
// Twinkle: one Karplus-Strong string per note, picked clean.
function pick(t, midi, gain, pan) {
  ev("pick", t, midi, gain, pan);
  if (!JS) return;
  const f = mtof(midi), len = Math.round(SR / f), line = new Float32Array(len);
  let lp = 0;
  for (let i = 0; i < len; i++) { lp += 0.45 * (noise() - lp); line[i] = lp; }
  const i0 = Math.round(t * SR), n = Math.floor(1.6 * SR), gl = gain * panL(pan), gr = gain * panR(pan);
  let idx = 0, prev = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const y = line[idx], v = 0.5 * (y + prev) * 0.9975;
    prev = y; line[idx] = v; if (++idx === len) idx = 0;
    const j = (i0 + k) * 2;
    clean[j] += y * gl; clean[j + 1] += y * gr;
    rev[j] += y * gl * 0.5; rev[j + 1] += y * gr * 0.5;
    dly[i0 + k] += y * gain * 0.35;
  }
}

// ── samples ─────────────────────────────────────────────────────────────
// Sourced instruments (bin/fetch-samples.mjs): a real kit and a real bass,
// both CC0. Decoded once to 48 k float.
const smpCache = new Map();
function smp(path, ch = 2) {
  const key = `${path}|${ch}`;
  if (!smpCache.has(key)) {
    if (!existsSync(path)) throw new Error(`sample missing: ${path} — run bin/fetch-samples.mjs`);
    const r = spawnSync(FFMPEG, ["-v", "error", "-i", path, "-f", "f32le", "-ac", String(ch), "-ar", String(SR), "pipe:1"], { maxBuffer: 1 << 28 });
    if (r.status !== 0) throw new Error(`sample decode failed: ${path}`);
    smpCache.set(key, new Float32Array(r.stdout.buffer.slice(r.stdout.byteOffset, r.stdout.byteOffset + r.stdout.byteLength)));
  }
  return smpCache.get(key);
}
// Humanized time: a player is never on the grid. Roughly gaussian, in milliseconds.
let _h = 0x0badbee5;
const hrnd = () => ((_h = (_h * 1664525 + 1013904223) >>> 0) / 4294967296);
const human = (ms) => (hrnd() + hrnd() + hrnd() - 1.5) * 0.82 * ms / 1000;

// ── bass ────────────────────────────────────────────────────────────────
// Growlybass: a Squier Jazz Bass, direct. The library names its octaves one
// high — "e2" is the open low E at 41 Hz (measured) — so the true notes are below.
const BASS_TRUE = { db2: 25, e2: 28, gb2: 30, a2: 33, c3: 36 };
let bassRR = 0;
function bassNote(t, dur, midi, gain, grit) {
  const [name, nominal] = Object.entries(BASS_TRUE).reduce((a, b) => (Math.abs(b[1] - midi) < Math.abs(a[1] - midi) ? b : a));
  const file = bassPath(`sustain/${name}_${grit > 0.4 ? "ff" : "f"}_rr${grit > 0.4 ? 1 + (bassRR++ % 2) : 1}.wav`), src = smp(file, 1);
  const rate = 2 ** ((midi - nominal) / 12), i0 = Math.round((t + human(5)) * SR), n = Math.floor((dur + 0.06) * SR), drive = 1 + 3 * grit;
  ev("bass", BAKE && bakeSmp(`${file}|1`, src), i0, n, rate, drive, gain);
  if (!JS) return;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const p = k * rate, q = Math.floor(p), fr = p - q;
    if (q + 1 >= src.length) break;
    const env = Math.min(1, k / 96) * Math.min(1, (n - k) / (0.06 * SR));
    bass[i0 + k] += Math.tanh((src[q] * (1 - fr) + src[q + 1] * fr) * 4 * drive) / Math.tanh(drive) * env * gain * 1.15;
  }
}

// ── drums ───────────────────────────────────────────────────────────────
// Virtuosity Drums: every hit is the close mic, the overheads and the room
// of the same strike. Louder hits pick a harder velocity layer; repeated hits
// walk the round robins; every hit lands a few milliseconds off the grid.
const MIC = { kickmic: 1, snaremic: 1, oh: 0.75, room: 0.4 };
const PIECE = { kick: 0.62, snare: 0.5, ghost: 0.5, rim: 0.9, hat: 1.3, ride: 1.5, crash: 2.4, htom: 0.8, ltom: 0.8 };
const robin = {};
function drum(piece, t, g, { rate = 1, lay = 0, jit = 4, wet = 0.04, max = 6 } = {}) {
  const K = KIT[piece], n = K.files.length, rr = n % 4 === 0 && /_rr/.test(K.files[0]);
  robin[piece] = (robin[piece] ?? -1) + 1;
  const idx = rr ? (n === 8 && g > 0.72 ? 4 : 0) + robin[piece] % 4
    : Math.max(0, Math.min(n - 1, Math.round(Math.min(1, g) * (n - 1)) - robin[piece] % 2));
  const at = t + lay / 1000 + human(jit), i0 = Math.round(at * SR), vel = g * (0.92 + 0.16 * hrnd()) * PIECE[piece] * 1.45;
  for (const mic of K.mics) {
    const file = kitPath(mic, K.files[idx]), src = smp(file), frames = Math.min(src.length >> 1, Math.floor(max * SR * rate)), a = vel * MIC[mic], fade = 0.3 * SR;
    const len = Math.floor(frames / rate), w = mic === "room" ? 0.1 : wet;
    ev("drum", BAKE && bakeSmp(`${file}|2`, src), i0, a, rate, max, w);
    if (!JS) continue;
    for (let k = 0; k < len && i0 + k < N; k++) {
      const q = rate === 1 ? k : Math.floor(k * rate), e = Math.min(1, (len - k) / fade), j = (i0 + k) * 2;
      const l = src[q * 2] * a * e, r = src[q * 2 + 1] * a * e;
      drums[j] += l; drums[j + 1] += r; rev[j] += l * w; rev[j + 1] += r * w;
    }
  }
}
const kick = (t, g = 1) => drum("kick", t, g, { jit: 3, max: 1.2 });
const snare = (t, g = 1) => (g < 0.5 ? drum("ghost", t, g * 1.8, { jit: 5, max: 1 }) : drum("snare", t, g / 1.15, { lay: 3, jit: 4, max: 1.6, wet: 0.08 }));
const rim = (t, g = 1) => drum("rim", t, g, { jit: 6, max: 1, wet: 0.1 });
const tom = (t, hz, g = 1) => (hz > 150 ? drum("htom", t, g, { rate: hz / 210, jit: 5, max: 2 }) : drum("ltom", t, g, { rate: hz / 130, jit: 5, max: 2.4 }));
const ride = (t, g = 1) => drum("ride", t, g, { jit: 6, max: 2.5 });
const hat = (t, g = 1) => drum("hat", t, g, { jit: 5, max: 0.6 });
const crash = (t, g = 1) => drum("crash", t, Math.min(1.4, g), { jit: 4, max: 5 });

function drumBar(kind, t, bar, s) {
  const e = BEAT / 2, x = BEAT / 4, last = bar === s.bars - 1;
  if (kind === "quiet") {
    kick(t, 0.5); if (bar % 2) kick(t + 2.5 * BEAT, 0.4);
    rim(t + 2 * BEAT);
    for (let i = 0; i < 4; i++) ride(t + i * BEAT, i === 0 ? 0.9 : 0.6);
    if (bar % 4 === 3) ride(t + 3.5 * BEAT, 0.5);
  } else if (kind === "dbeat") {
    for (const p of [0, 3, 5]) kick(t + p * e);
    if (bar % 2) kick(t + 7 * e, 0.8);
    for (const p of [2, 6]) snare(t + p * e);
    for (let i = 0; i < 8; i++) ride(t + i * e, i % 2 ? 0.7 : 1);
    if (bar % 4 === 0) crash(t, 1, bar % 8 ? 0.4 : -0.4);
  } else if (kind === "blast") {
    for (let i = 0; i < 16; i++) (i % 2 ? snare(t + i * x, 0.72 + 0.1 * rnd()) : kick(t + i * x, 0.85));
    for (let i = 0; i < 8; i++) hat(t + i * e, 1);
    if (bar % 2 === 0) crash(t, 1, bar % 4 ? 0.4 : -0.4);
  } else if (kind === "slow") {       // half-time in triplets: the 12/8 collapse
    const tr = BAR / 12;
    kick(t); kick(t + 5 * tr, 0.8); if (bar % 2) { kick(t + 9 * tr, 0.8); kick(t + 11 * tr, 0.9); }
    snare(t + 6 * tr, 1.15);
    for (const p of [0, 3, 6, 9]) ride(t + p * tr, p === 0 ? 1.1 : 0.8);
    if (bar % 2 === 0) crash(t, 1.1, bar % 4 ? 0.4 : -0.4);
  }
  if (last && s.fill) {               // the fill replaces nothing — it lands on top of the last half bar
    [[8, 210], [9, 210], [10, 165], [11, 165], [12, 130], [13, 130], [14, 105], [15, 105]].forEach(([p, hz], i) => tom(t + p * x, hz, 0.8 + 0.03 * i, -0.5 + i * 0.14));
    snare(t + 14 * x, 1); snare(t + 15 * x, 1.1);
  }
}

// ── arrangement ─────────────────────────────────────────────────────────
progress.begin({ type: "audio", label: "frothyscreama" });
FORM.forEach((s, si) => {
  const t0 = s.at * BAR, cyc = (list) => Array.from({ length: s.bars }, (_, i) => list[i % list.length]);
  const chords = cyc(s.chords);

  if (s.loud) {
    const pat = s.slow ? "D..D..x..D.u" : s.drums === "blast" ? "DUDUDUDUDUDUDUDU" : "D.DUD.DUD.DUDUDU";
    const a = wall(strum(["--chord", chords.join("|"), "--bars", s.bars, "--pattern", pat, "--electric", "--drive", 0.95, "--force", 0.95, "--rake", 9, "--human", 0.6, "--seed", 11 + si, "--tail", s.slow ? 2.5 : 0.4]), 3.2);
    const b = wall(strum(["--chord", (s.colour ? cyc(s.colour) : chords).join("|"), "--bars", s.bars, "--pattern", pat, "--electric", "--drive", 0.9, "--force", 0.9, "--rake", 12, "--seed", 71 + si, "--human", 0.8, "--tail", s.slow ? 2.5 : 0.4]), 2.8);
    place(gtr, a, t0, 0.42, -0.85); place(gtr, b, t0 + 0.011, 0.42, 0.85);
  }
  if (s.clean) {
    const c = strum(["--chord", chords.join("|"), "--bars", s.bars, "--pattern", s.ring ? "D..............." : "D.......d...u...", "--electric", "--drive", 0.06, "--force", 0.5, "--rake", 46, "--seed", 5 + si, "--tail", s.ring ? 6 : 1.5]);
    place(clean, c, t0, s.clean * 0.9, -0.25);
  }
  for (let bar = 0; bar < s.bars; bar++) {
    const t = t0 + bar * BAR, ch = chords[bar], root = ROOTS[ch];
    if (s.twinkle && !(s.ring && bar > 1)) (TWINKLE[ch] || TWINKLE.Bm9).forEach((m, i) => pick(t + i * BEAT / 2 + (i % 2 ? 0.006 : 0) + human(8), m, s.twinkle * 0.32 * (i % 4 === 0 ? 1 : 0.72), i % 2 ? 0.55 : -0.1));
    if (s.drums && bar >= (s.drumsFrom ?? 0)) {
      if (s.build && bar >= s.bars - s.build) {       // the build: a roll that doubles its rate and climbs
        const u0 = (bar - (s.bars - s.build)) / s.build, n = u0 < 0.5 ? 8 : 16;
        for (let i = 0; i < n; i++) { const u = u0 + i / n / s.build; snare(t + i * BAR / n, 0.25 + 0.9 * u * u); }
        kick(t, 0.8); kick(t + 2 * BEAT, 0.8);
      } else drumBar(s.drums, t, bar, s);
    }
    if (s.bass === "eighths") for (let i = 0; i < 8; i++) bassNote(t + i * BEAT / 2, BEAT / 2 * 0.9, root, 0.34, 0.7);
    else if (s.bass === "slow") { bassNote(t, BAR * 0.48, root, 0.4, 0.8); bassNote(t + BAR / 2, BAR * 0.3, root, 0.36, 0.8); bassNote(t + BAR * 10 / 12, BAR / 12 * 1.8, root + 12, 0.3, 0.8); }
    else if (s.bass === "long" && bar >= (s.bassFrom ?? 0)) bassNote(t, BAR * 0.96, root, 0.2, 0.12);
  }
  if (s.crash) crash(t0, 1.4, 0);
  progress.update(Math.round(40 * (si + 1) / FORM.length));
});
// feedback into the second explosion: one sine climbing out of the bridge
{
  const s = FORM[4], t0 = (s.at + s.bars - 4) * BAR, n = Math.floor(4 * BAR * SR), i0 = Math.round(t0 * SR);
  let ph = 0;
  ev("feedback", i0, n);
  for (let k = 0; JS && k < n; k++) {
    const u = k / n; ph += mtof(78 + 0.3 * Math.sin(TAU * 5 * k / SR)) / SR; if (ph >= 1) ph -= 1;
    const v = Math.tanh(3 * Math.sin(TAU * ph)) * u * u * 0.09, j = (i0 + k) * 2;
    gtr[j] += v; gtr[j + 1] += v * 0.7; rev[j] += v * 0.5; rev[j + 1] += v * 0.5;
  }
}

// ── jeffrey ─────────────────────────────────────────────────────────────
// Each take: decoded, trimmed to the breath, optionally held longer (a
// rubberband stretch of the same scream — the line stays one take), and for
// screams doubled wide a few cents apart with a quiet octave underneath.
const LINES = Object.fromEntries(lyric().map((l) => [l.id, l]));
function sh(cmd, args) { const r = spawnSync(cmd, args.map(String), { stdio: ["ignore", "ignore", "pipe"] }); if (r.status !== 0) throw new Error(`${cmd} failed: ${r.stderr?.toString().slice(0, 200)}`); }
function take(id, { hold = 1, cents = 0, semis = 0 } = {}) {
  const line = LINES[id], { path } = takePath(line);
  if (!existsSync(path)) throw new Error(`take ${id} is not rendered — run bin/scream.mjs first`);
  const base = resolve(VOX, `${id}.wav`);
  if (!existsSync(base)) {
    const gate = "silenceremove=start_periods=1:start_threshold=-45dB:start_silence=0.02";
    sh(FFMPEG, ["-y", "-v", "error", "-i", path, "-af", `${gate},areverse,${gate},areverse`, "-ar", SR, "-ac", 1, "-c:a", "pcm_f32le", base]);
  }
  if (hold === 1 && !cents && !semis) return readWav(base).L;
  const out = resolve(VOX, `${id}-x${hold}-p${semis}-c${cents}.wav`);
  if (!existsSync(out)) sh("/opt/homebrew/bin/rubberband", ["-q", "-3", "-F", "-t", hold, "-p", semis + cents / 100, base, out]);
  return readWav(out).L;
}
// Screams go through the aesthetivox instead: extended in the WORLD frame
// domain (consonants at their own length, voiced runs one common ratio) and
// the pitch replaced by a hard-stepped arpeggio over the chord underneath —
// sixteenths, then thirty-seconds — so they warble like a modem handshake.
// A quiet copy an octave up and one an octave down sit either side.
const PYTHON = resolve(HERE, "../../.venv/bin/python");
const MINOR = new Set(["Bm9", "B5", "F#m7", "F#5", "Em9", "Bm", "F#7sus4"]);
const avoxJobs = [];
function arp(chord) {
  let r = ROOTS[chord] + 36; if (r > 71) r -= 12;       // the root nearest his scream register (about A4)
  const third = MINOR.has(chord) ? 3 : 4;
  // from an octave under his scream up through it: the low notes are the floor of the warble
  return [r - 12, r - 5, r - 12 + third, r, r + 7, r + third, r + 12, r - 5];
}
// Not every scream is arpeggiated: without `arp`, the scream holds one note a
// bar — the chord's root or fifth, whichever sits nearer his scream register.
function held(chord) {
  let r = ROOTS[chord] + 36; if (r > 71) r -= 12;
  return [r - 12];                                      // an octave under the take: chest, not head
}
function avox(id, s, bar, bars, transpose, arpeggiate) {
  take(id);                                             // makes the trimmed base wav
  const segs = [];
  for (let b = 0; b < Math.ceil(bars); b++) segs.push([b * BAR, (arpeggiate ? arp : held)(s.chords[(bar + b) % s.chords.length])]);
  const key = createHash("sha1").update(JSON.stringify([id, bars, transpose, segs, BPM, "tail-hold"])).digest("hex").slice(0, 10);
  const out = resolve(VOX, `${id}-avox-${key}.f32`);
  avoxJobs.push({ in: resolve(VOX, `${id}.wav`), out, dur: bars * BAR, step: BEAT / 4, transpose, segs });
  return out;
}
const loadF32 = (path) => { const raw = readFileSync(path); return new Float32Array(raw.buffer.slice(raw.byteOffset, raw.byteOffset + raw.byteLength)); };
const vocalLog = [];
// A pop vocal chain per take: rumble out, a little low-mid out, presence in,
// then a 4:1 compressor so every word sits at the front. Peak-normalized.
function biquadPeak(x, hz, db, q) {
  const A = 10 ** (db / 40), w = TAU * hz / SR, al = Math.sin(w) / (2 * q), a0 = 1 + al / A;
  const b0 = (1 + al * A) / a0, b1 = -2 * Math.cos(w) / a0, b2 = (1 - al * A) / a0, a2 = (1 - al / A) / a0;
  let x1 = 0, x2 = 0, y1 = 0, y2 = 0;
  for (let i = 0; i < x.length; i++) { const y = b0 * x[i] + b1 * x1 + b2 * x2 - b1 * y1 - a2 * y2; x2 = x1; x1 = x[i]; y2 = y1; y1 = y; x[i] = y; }
}
function shapeVocal(src) {
  const x = Float32Array.from(src);
  let h1 = 0, h2 = 0; const kh = 1 - Math.exp(-TAU * 110 / SR);
  for (let i = 0; i < x.length; i++) { h1 += kh * (x[i] - h1); const a = x[i] - h1; h2 += kh * (a - h2); x[i] = a - h2; }
  biquadPeak(x, 300, -2.5, 1.0); biquadPeak(x, 3200, 3.5, 0.9); biquadPeak(x, 7500, -2, 1.5);
  let pk = 1e-9; for (let i = 0; i < x.length; i++) if (Math.abs(x[i]) > pk) pk = Math.abs(x[i]);
  const thr = 0.14, atk = Math.exp(-1 / (0.004 * SR)), rel = Math.exp(-1 / (0.09 * SR));
  let env = 0, out = 1e-9;
  for (let i = 0; i < x.length; i++) {
    const v = x[i] / pk, a = Math.abs(v);
    env = a > env ? a + (env - a) * atk : a + (env - a) * rel;
    x[i] = v * (env > thr ? (thr + (env - thr) / 4) / env : 1);
    if (Math.abs(x[i]) > out) out = Math.abs(x[i]);
  }
  for (let i = 0; i < x.length; i++) x[i] /= out;
  return x;
}
// A real dial-up handshake (Freesound 454650, CC0) under the arpeggiated
// screams, replayed 2.9% fast so its 480 Hz carrier sits on B4.
const MODEM = resolve(process.env.AC_VAULT ?? resolve(HERE, "../../../../aesthetic-computer-vault"), "personal/pop/freesound-cache/454650-modem_2_aif.wav");
let modemAt = 2;
function modem(t, dur) {
  if (!existsSync(MODEM)) { console.log("modem sample not in the vault cache — skipped"); return; }
  const src = smp(MODEM, 1), rate = 1.0289, i0 = Math.round(t * SR), n = Math.floor(dur * SR), off = Math.floor(modemAt * SR);
  modemAt = (modemAt + dur * rate) % 12;
  ev("modem", BAKE && bakeSmp(`${MODEM}|1`, src), i0, n, off, rate);
  if (!JS) return;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const p = off + k * rate, q = Math.floor(p), v = (src[q] ?? 0) * Math.min(1, k / 2400, (n - k) / 9600) * 0.07, j = (i0 + k) * 2;
    vox[j] += v * (k % 9600 < 4800 ? 1 : 0.4); vox[j + 1] += v * (k % 9600 < 4800 ? 0.4 : 1);
  }
}
function sing(t, id, o = {}, files) {
  t += human(14) - 0.008;                                // he comes in a hair early, never on the grid
  const line = LINES[id], scream = line.kind === "scream";
  const layers = scream
    ? [[loadF32(files[0]), 0.7, 0, 0, files[0], 1], [loadF32(files[1]), 0.12, -0.75, 0.015, files[1], 0], [loadF32(files[1]), 0.12, 0.75, 0.024, files[1], 0], [loadF32(files[2]), 0.34, 0, 0.008, files[2], 0]]
    : [[shapeVocal(take(id)), 2.9, 0, 0, resolve(VOX, `${id}.wav`), 1]];
  if (scream) layers[0][0] = shapeVocal(layers[0][0]);
  if (o.back != null) for (const l of layers) { l[1] *= 0.5; l[2] = Math.max(-1, Math.min(1, l[2] * 0.3 + o.back)); }
  if (o.arp) modem(t, layers[0][0].length / SR);
  for (const [src, gain, pan, late, file, shaped] of layers) {
    const i0 = Math.round((t + late) * SR), gl = gain * panL(pan), gr = gain * panR(pan);
    ev("vox", i0, gain, pan, scream ? 1 : 0, shaped, file);
    if (!JS) continue;
    let pk = 1e-9, hp = 0;
    for (let k = 0; k < src.length; k++) if (Math.abs(src[k]) > pk) pk = Math.abs(src[k]);
    for (let k = 0; k < src.length && i0 + k < N; k++) {
      hp += 0.02 * (src[k] - hp);
      let v = (src[k] - hp) / pk;
      if (scream) v = Math.tanh(v * 2.6) * 0.8;        // the scream into the mic pre, hard
      const j = (i0 + k) * 2;
      vox[j] += v * gl; vox[j + 1] += v * gr;
      const wet = scream ? 0.07 : 0.06;                // dry and up front; the room is for the band
      rev[j] += v * gl * wet; rev[j + 1] += v * gr * wet;
      dly[i0 + k] += v * gain * (scream ? 0.03 : 0.04);
    }
  }
  vocalLog.push({ id, at: t, len: layers[0][0].length / SR, text: line.text, kind: line.kind });
}
const placed = [];
for (const s of FORM) for (const [bar, id, o = {}] of s.vox || []) {
  const files = LINES[id].kind === "scream" ? [0, 12, -12].map((tr) => avox(id, s, bar, o.bars ?? 3, tr, o.arp)) : null;
  placed.push([(s.at + bar) * BAR, id, o, files]);
}
if (avoxJobs.length) {
  const jobsFile = resolve(VOX, "avox-jobs.json");
  writeFileSync(jobsFile, JSON.stringify({ sr: SR, jobs: avoxJobs }));
  const r = spawnSync(PYTHON, [resolve(HERE, "aesthetivox-scream.py"), jobsFile], { stdio: "inherit" });
  if (r.status !== 0) throw new Error("aesthetivox-scream failed");
  for (const j of avoxJobs) if (!existsSync(j.out)) throw new Error(`aesthetivox take missing: ${j.out}`);
}
for (const [t, id, o, files] of placed) sing(t, id, o, files);
progress.update(55);

const G = { gtr: 0.85, clean: 1.0, drums: 1.1, bass: 0.85, vox: 1.8, rev: 1.0 };
const report = () => { for (const v of vocalLog) console.log(`${clock(v.at).padEnd(5)} ${v.id} ${v.len.toFixed(1)}s  ${v.text}`); };
if (BAKE) {
  const head = [`sr ${SR}`, `frames ${N}`, `bpm ${BPM}`, `material ${MATERIAL}`, ...Object.entries(G).map(([k, v]) => `gain ${k} ${v}`)];
  writeFileSync(resolve(BAKE), [...head, ...baked].join("\n") + "\n");
  console.log(`baked ${baked.length} events → ${resolve(BAKE)}`);
}
if (!JS) {        // the engine makes the sound from here
  rmSync(tmp, { recursive: true, force: true });
  if (CANON) {
    progress.update(60);
    const r = spawnSync("node", [resolve(HERE, "../c/run-c.mjs"), resolve(BAKE)], { stdio: "inherit" });
    if (r.status !== 0) throw new Error("the C engine failed");
    report();
    console.log(`material ${MATERIAL}\n${BARS} bars · ${BPM} BPM · ${clock(DUR)}`);
  }
  progress.update(100); progress.end();
  process.exit(0);
}

// ── post ────────────────────────────────────────────────────────────────
{ // dotted-eighth ping-pong for the quiet things
  const D = Math.round(0.75 * BEAT * SR), L = new Float32Array(D), R = new Float32Array(D);
  let a = 0, b = 0;
  for (let i = 0; i < N; i++) {
    const w = i % D, oL = L[w], oR = R[w];
    a += 0.5 * (oR - a); b += 0.5 * (oL - b);
    L[w] = dly[i] + 0.38 * a; R[w] = b;
    clean[i * 2] += oL * 0.7; clean[i * 2 + 1] += oR * 0.7;
  }
}
{ // a short room
  const scale = SR / 44100, COMB = [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617], AP = [556, 441, 341, 225];
  for (let ch = 0; ch < 2; ch++) {
    const combs = COMB.map((n) => ({ buf: new Float32Array(Math.round((n + ch * 23) * scale)), i: 0, lp: 0 }));
    const aps = AP.map((n) => ({ buf: new Float32Array(Math.round((n + ch * 23) * scale)), i: 0 }));
    for (let i = 0; i < N; i++) {
      const x = rev[i * 2 + ch];
      let y = 0;
      for (const c of combs) { const o = c.buf[c.i]; y += o; c.lp = o * 0.6 + c.lp * 0.4; c.buf[c.i] = x + c.lp * 0.84; if (++c.i === c.buf.length) c.i = 0; }
      for (const a of aps) { const o = a.buf[a.i]; a.buf[a.i] = y + o * 0.5; y = o - y; if (++a.i === a.buf.length) a.i = 0; }
      rev[i * 2 + ch] = y * 0.1;
    }
  }
}
progress.update(70);

for (const [name, b] of Object.entries({ gtr, clean, drums, vox, rev, bass, dly })) {
  let first = -1; for (let i = 0; i < b.length; i++) if (!Number.isFinite(b[i])) { first = i; break; }
  if (first >= 0) console.error(`non-finite in ${name} from ${(first / (b.length / N) / SR).toFixed(2)} s`);
}
const mix = new Float32Array(N * 2);
let peak = 0, bad = 0, env = 0;
for (let i = 0; i < N; i++) for (let ch = 0; ch < 2; ch++) {
  const j = i * 2 + ch;
  // the wall leans back a little whenever he is screaming
  if (ch === 0) { const a = Math.abs(vox[j]) + Math.abs(vox[j + 1]); env += (a > env ? 0.002 : 0.00012) * (a - env); }
  const lean = 1 - 0.32 * Math.min(1, env / 0.25);
  const v = gtr[j] * G.gtr * lean + clean[j] * G.clean + Math.tanh(drums[j] * 1.5) * 0.75 * G.drums + bass[i] * G.bass + vox[j] * G.vox + rev[j] * G.rev;
  if (!Number.isFinite(v)) bad++;
  mix[j] = v; if (Math.abs(v) > peak) peak = Math.abs(v);
}
if (bad) throw new Error(`${bad} non-finite samples in the mix`);
if (MATERIAL !== "none") {
  const pre = 0.6 / peak;
  for (let j = 0; j < N * 2; j++) mix[j] *= pre;
  applyMaterial(mix, SR, MATERIAL);
  peak = 0; for (let j = 0; j < N * 2; j++) if (Math.abs(mix[j]) > peak) peak = Math.abs(mix[j]);
}
for (let j = 0; j < N * 2; j++) mix[j] *= 0.9 / peak;
progress.update(85);

function writeWav(path, data, channels) {
  const bytes = data.length * 4, h = Buffer.alloc(44);
  h.write("RIFF", 0); h.writeUInt32LE(36 + bytes, 4); h.write("WAVEfmt ", 8); h.writeUInt32LE(16, 16);
  h.writeUInt16LE(3, 20); h.writeUInt16LE(channels, 22); h.writeUInt32LE(SR, 24);
  h.writeUInt32LE(SR * channels * 4, 28); h.writeUInt16LE(channels * 4, 32); h.writeUInt16LE(32, 34);
  h.write("data", 36); h.writeUInt32LE(bytes, 40);
  const fd = openSync(path, "w"); writeSync(fd, h); writeSync(fd, Buffer.from(data.buffer, data.byteOffset, bytes)); closeSync(fd);
}
const pre = resolve(OUT, "frothyscreama-premaster.wav"), wav = resolve(OUT, "frothyscreama-master.wav"), mp3 = resolve(OUT, "frothyscreama.mp3");
writeWav(pre, mix, 2);
writeWav(resolve(OUT, "frothyscreama-vox.wav"), vox, 2);
if (process.argv.includes("--stems")) {
  writeWav(resolve(OUT, "stem-guitars.wav"), gtr, 2); writeWav(resolve(OUT, "stem-clean.wav"), clean, 2);
  writeWav(resolve(OUT, "stem-drums.wav"), drums, 2); writeWav(resolve(OUT, "stem-bass.wav"), bass, 1);
}
if (BAKE) {       // the score and the premaster are the whole job; the master on disk stays as it was
  rmSync(tmp, { recursive: true, force: true }); progress.end();
  console.log(`premaster ${pre}`); process.exit(0);
}
// Master: glue, then measured static gain into a limiter (pop/MASTERING.md) — no loudness rider.
const glue = "highpass=f=32,acompressor=threshold=-16dB:ratio=2.2:attack=12:release=140:makeup=1.5:knee=6";
const probe = spawnSync(FFMPEG, ["-hide_banner", "-nostats", "-i", pre, "-af", `${glue},ebur128`, "-f", "null", "-"], { encoding: "utf8" });
const lufs = Number(probe.stderr.match(/I:\s+(-?[\d.]+) LUFS(?![\s\S]*I:\s)/)?.[1]);
const gain = Number.isFinite(lufs) ? Math.min(14, TARGET_LUFS - lufs + 0.8) : 0;
const chain = `${glue},volume=${gain.toFixed(2)}dB,alimiter=limit=0.84:attack=3:release=60:level=disabled,afade=t=out:st=${(DUR - 3).toFixed(2)}:d=3`;
const run = (args) => { const r = spawnSync(FFMPEG, ["-y", "-hide_banner", "-loglevel", "error", ...args], { stdio: "inherit" }); if (r.status !== 0) throw new Error("ffmpeg failed"); };
run(["-i", pre, "-af", chain, "-ar", SR, "-c:a", "pcm_s24le", wav].map(String));
run(["-i", wav, "-c:a", "libmp3lame", "-b:a", "320k", "-metadata", "title=frothyscreama", "-metadata", "artist=Whistlegraph Dot Org", "-metadata", "album=pixsies", mp3]);
rmSync(tmp, { recursive: true, force: true });
progress.update(100); progress.end();
report();
console.log(`glue ${lufs} LUFS → ${gain.toFixed(1)} dB into the limiter · material ${MATERIAL}\n${mp3}\n${BARS} bars · ${BPM} BPM · ${clock(DUR)}`);
