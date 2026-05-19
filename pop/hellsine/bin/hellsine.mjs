#!/usr/bin/env node
// hellsine.mjs — the all-sine hardcore engine.
//
// THE LAW: every sample written here is a sum of Math.sin() terms, or a
// memoryless waveshaping (tanh / hard clip) of such a sum. No noise, no
// saw, no square, no samples. The gabber kick is a pitch-enveloped sine
// clipped past its skin — that distorted sine IS hardcore. The "hell"
// is the drive.
//
// It carries a hand-composed John Williams structure (a heroic D-minor
// leitmotif: stated → developed → transposed → restated) and is built
// to run as epic study music: continuous, no dead air, loop-friendly
// tail under a film arc.
//
// Output: a 32-bit float WAV (pre-master, headroom intact) + a
// scratch-mix-compatible struct.json (bpm + sections + kick/snare
// onset grid). bake.mjs runs the post-FX + Spotify finalize.
//
// Usage:
//   node pop/hellsine/bin/hellsine.mjs --out OUT.wav [--struct S.json]
//        [--bpm 182] [--hell 11] [--seed hellsine]
import { writeFileSync, mkdirSync } from "node:fs";
import { dirname } from "node:path";

// ── flags ─────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  if (argv[i].startsWith("--")) {
    const k = argv[i].slice(2);
    const v = argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[++i] : "true";
    flags[k] = v;
  }
}
const SR = 48_000;
const BPM = Number(flags.bpm ?? 182);
const HELL = Number(flags.hell ?? 11);          // base gabber drive ("hell knob")
const SEED_STR = flags.seed || "hellsine";
const OUT = flags.out || `${process.env.HOME}/Documents/Working Desktop/hellsine/.hellsine-pre.wav`;
const STRUCT = flags.struct || `${OUT.replace(/\.wav$/, "")}.assets/struct.json`;

const SPB = 60 / BPM;          // seconds per beat
const SPBAR = 4 * SPB;         // 4/4

// ── deterministic rng (humanization only — the music is composed) ─────
function hashString(s) { let h = 2166136261 >>> 0; for (let i = 0; i < s.length; i++) { h ^= s.charCodeAt(i); h = Math.imul(h, 16777619); } return h >>> 0; }
function makeRng(seedStr) { let s = hashString(seedStr) || 1; return () => { s ^= s << 13; s ^= s >>> 17; s ^= s << 5; s >>>= 0; return s / 4294967296; }; }
const rng = makeRng(SEED_STR);
const hum = (amt) => (rng() * 2 - 1) * amt;     // ± humanization

const m2f = (m) => 440 * Math.pow(2, (m - 69) / 12);
const TAU = Math.PI * 2;

// D natural minor reference: D E F G A Bb C → 0 2 3 5 7 8 10. Theme +
// chord pitches below are written as explicit semitone offsets.

// ── the principal theme (leitmotif) ───────────────────────────────────
// Hand-written, singable, heroic. Semitone offsets from melodic root
// D4. Antecedent = a rising Dm-arpeggio "call" + an upper-neighbour
// swell + a lyric descent that ends OPEN on the 5th (a question).
// Consequent = climbs the full arpeggio to a soaring high D (the apex)
// and resolves home (a strong cadence). Durations in beats.
const ROOT_MEL = 62;   // D4
const THEME = [
  // antecedent (bars 1–4)
  [-5, 1], [0, 1.5], [3, 0.5], [7, 1],            // A3 → D4 F4 A4   (the call)
  [7, 1], [8, 1], [7, 1], [5, 1],                 // A4 Bb4 A4 G4    (swell)
  [3, 2], [2, 1], [0, 1],                         // F4 . E4 D4      (descent)
  [0, 2], [-2, 1], [-5, 1],                       // D4 . C4 A3      (open — question)
  // consequent (bars 5–8)
  [0, 1], [3, 1], [7, 1], [10, 1],                // D4 F4 A4 C5     (climb)
  [12, 2], [10, 1], [8, 1],                       // D5 . C5 Bb4     (the apex)
  [7, 2], [5, 1], [3, 1],                         // A4 . G4 F4      (descent)
  [0, 4],                                          // D4              (resolve home)
];
// B theme — lyrical, narrow, stepwise: the study calm. 8 bars.
const BTHEME = [
  [-5, 2], [-2, 1], [0, 1],
  [3, 2], [2, 1], [0, 1],
  [-2, 2], [0, 1], [2, 1],
  [0, 4],
  [0, 1], [2, 1], [3, 1], [5, 1],
  [7, 2], [5, 1], [3, 1],
  [2, 2], [0, 1], [-2, 1],
  [0, 4],
];

// ── chords (low register, root/third/fifth as sines) ──────────────────
const CHORD = {
  Dm: { root: 38, q: [0, 3, 7] }, Bb: { root: 46, q: [0, 4, 7] },
  F:  { root: 41, q: [0, 4, 7] }, C:  { root: 48, q: [0, 4, 7] },
  Gm: { root: 43, q: [0, 3, 7] }, A:  { root: 45, q: [0, 4, 7] },
};
const cyc = (arr, n) => arr[n % arr.length];

// ── arrangement: ordered sections, each N bars ────────────────────────
// "study music under a film arc" — continuous, no dead air. The kick is
// the hardcore spine; it thins to a pulse in the bridge (the study
// zone) and is fully absent only in the overture / coda tail.
const PLAN = [
  { name: "overture",  bars: 8,  kick: "none",  drive: 0,    chords: ["Dm","Dm","Bb","Bb","F","F","C","A"], theme: "soft" },
  { name: "statement", bars: 16, kick: "four",  drive: HELL, chords: ["Dm","Dm","Bb","Bb","F","F","C","C"], theme: "brass" },
  { name: "bridge",    bars: 16, kick: "pulse", drive: HELL*0.7, chords: ["Gm","Gm","Dm","Dm","C","C","Bb","A"], theme: "bsoft" },
  { name: "develop",   bars: 16, kick: "four",  drive: HELL*1.15, chords: ["Dm","Dm","F","F","Gm","Gm","A","A","Bb","Bb","C","C","Dm","A","Dm","A"], theme: "frag" },
  { name: "climax",    bars: 16, kick: "hard",  drive: HELL*1.3, transpose: 2, chords: ["Dm","Dm","Bb","Bb","F","F","C","C"], theme: "brass" },
  { name: "coda",      bars: 8,  kick: "fade",  drive: HELL*0.6, chords: ["Dm","Dm","Gm","Gm","C","C","Dm","Dm"], theme: "dissolve" },
];
const TOTAL_BARS = PLAN.reduce((a, s) => a + s.bars, 0);
const TAIL = 3.2;
const totalSec = TOTAL_BARS * SPBAR + TAIL;
const N = Math.ceil(totalSec * SR);
const L = new Float32Array(N), Rb = new Float32Array(N);
// sidechain duck (1 = open) — kicks pull it down; sub/pad/strings ride it.
const DUCK = new Float32Array(N).fill(1);

// ── core sine-only voices ─────────────────────────────────────────────
function write(t, fn, dur) {
  let i = Math.max(0, Math.floor(t * SR));
  const e = Math.min(N, Math.ceil((t + dur) * SR));
  for (; i < e; i++) {
    const lt = i / SR - t;
    const s = fn(lt);
    if (s) { L[i] += s[0]; Rb[i] += s[1]; }
  }
}

// The gabber/Rotterdam kick: ONE sine, fast exponential pitch drop, then
// tanh-saturated past its skin. The distorted sine = the kick.
function kick(t, drive = HELL, gain = 1) {
  const dur = 0.26, pStart = 240, pEnd = 47, pT = 0.034;
  let ph = 0;
  // mark sidechain duck
  const di = Math.floor(t * SR), dN = Math.floor(0.20 * SR);
  for (let k = 0; k < dN && di + k < N; k++) {
    const env = 0.32 + 0.68 * (1 - Math.exp(-k / (0.16 * SR)));
    if (env < DUCK[di + k]) DUCK[di + k] = env;
  }
  write(t, (lt) => {
    const f = pEnd + (pStart - pEnd) * Math.exp(-lt / pT);
    ph += (TAU * f) / SR;
    const amp = Math.exp(-lt / 0.10) * (1 - Math.exp(-lt / 0.0008)); // click→thump
    let x = Math.sin(ph);
    x = Math.tanh(x * (drive * (0.7 + 0.3 * Math.exp(-lt / 0.05)))); // the hell
    x = Math.max(-0.97, Math.min(0.97, x * 1.06));                   // skin
    // gabber transient — a bright, ~4 ms high sine SNAP at the attack.
    // This is the genre's HF "tick"; pure sine, just fast. Without it a
    // sine-only mix is far too dark for hardcore.
    const click = Math.sin(TAU * 3400 * lt) * Math.exp(-lt / 0.0016) * 0.5
                + Math.sin(TAU * 1700 * lt) * Math.exp(-lt / 0.0042) * 0.35;
    const v = (x * amp + click) * 0.9 * gain;
    return [v, v];
  }, dur);
}

// Sub — low sine at the chord root, rides the duck (the pump).
function sub(t, dur, midi, gain = 0.5) {
  const f = m2f(midi - 12);
  write(t, (lt) => {
    const a = Math.min(1, lt / 0.008) * Math.exp(-Math.max(0, lt - (dur - 0.06)) / 0.04);
    const d = DUCK[Math.min(N - 1, Math.floor((t + lt) * SR))];
    let x = Math.sin(TAU * f * lt);
    x = Math.tanh(x * 1.6) * 0.7;                 // gentle sine drive
    const v = x * a * d * gain;
    return [v, v];
  }, dur);
}

// Additive-sine brass / strings — carries the leitmotif. Odd-harmonic
// emphasis = brass bite; slow attack + many partials = strings.
function voice(t, dur, midi, gain, opt = {}) {
  const f = m2f(midi);
  const parts = opt.parts || [
    [1, 1.0], [2, 0.5], [3, 0.34], [4, 0.16], [5, 0.12], [6, 0.06],
  ];
  let amps = 0; for (const p of parts) amps += p[1];
  const norm = (opt.gain ?? 1) / amps;
  const atk = opt.atk ?? 0.05, rel = opt.rel ?? 0.18;
  const vibR = opt.vibR ?? 5.2, vibD = opt.vibD ?? 0.006;
  const pan = opt.pan ?? 0, drive = opt.drive ?? 1.0;
  write(t, (lt) => {
    let env = Math.min(1, lt / atk);
    if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
    const vib = 1 + Math.sin(TAU * vibR * lt) * vibD * Math.min(1, lt / 0.12);
    let x = 0;
    for (const [r, a] of parts) x += a * Math.sin(TAU * f * r * vib * lt);
    x = Math.tanh(x * norm * drive);              // soft sine glue
    const v = x * env * gain;
    return [v * (1 - Math.max(0, pan)), v * (1 + Math.min(0, pan))];
  }, dur + rel);
}

// Hoover — detuned sine stack + rising sine-on-sine FM index. All sine,
// reads aggressive after the saturation.
function hoover(t, dur, midi, gain = 0.3) {
  const f = m2f(midi), det = [0.994, 1.0, 1.007, 1.013];
  write(t, (lt) => {
    const env = Math.min(1, lt / 0.02) * Math.max(0, 1 - lt / dur);
    const idx = 1.4 + 3.0 * Math.min(1, lt / (dur * 0.6));
    let x = 0;
    for (const dd of det) {
      const mod = Math.sin(TAU * f * dd * 0.5 * lt) * idx;
      x += Math.sin(TAU * f * dd * lt + mod);
    }
    x = Math.tanh(x * 0.5 * (1 + idx * 0.2)) * env * gain;
    return [x * 0.85, x];
  }, dur);
}

// Hardcore stab — 2-op sine FM (sine modulates sine), high index, clipped.
function stab(t, midi, gain = 0.34) {
  const f = m2f(midi), dur = 0.16;
  write(t, (lt) => {
    const env = Math.exp(-lt / 0.06) * (1 - Math.exp(-lt / 0.001));
    const mod = Math.sin(TAU * f * 1.997 * lt) * (5.5 * Math.exp(-lt / 0.05));
    let x = Math.sin(TAU * f * lt + mod);
    x = Math.max(-0.9, Math.min(0.9, x * 1.5));
    const v = x * env * gain;
    return [v, v * 0.92];
  }, dur);
}

// Perc — the "hat" re-derived as a very high, sub-ms sine blip. Metallic
// open-hat = a ring-modded sine pair (product of two sines → sines).
function tick(t, gain = 0.22, open = false) {
  const f = open ? 7400 : 9200, dur = open ? 0.06 : 0.022;
  write(t, (lt) => {
    const env = Math.exp(-lt / (open ? 0.026 : 0.004));
    let x = Math.sin(TAU * f * lt) + 0.45 * Math.sin(TAU * f * 1.51 * lt);
    if (open) x *= Math.sin(TAU * 5300 * lt);     // ring-mod metal
    const v = x * env * gain;
    return [v * 0.9, v];
  }, dur);
}

// Scream / riser — a sine swept up over the span with a rising FM index
// and the same hard saturation. The build.
function riser(t, dur, m0, m1, gain = 0.26) {
  const f0 = m2f(m0), f1 = m2f(m1);
  let ph = 0;
  write(t, (lt) => {
    const p = lt / dur, f = f0 * Math.pow(f1 / f0, p);
    ph += (TAU * f) / SR;
    const idx = 1 + 5 * p;
    let x = Math.sin(ph + Math.sin(ph * 0.5) * idx);
    x = Math.tanh(x * 2.2);
    const env = Math.min(1, lt / 0.05) * (0.3 + 0.7 * p);
    const v = x * env * gain;
    return [v, v * 0.95];
  }, dur);
}

// ── render the arrangement ────────────────────────────────────────────
const kickEvents = [], snareEvents = [], sectionRanges = [];
let bar = 0;
for (const sec of PLAN) {
  const tr = sec.transpose || 0;
  const startSec = bar * SPBAR;
  sectionRanges.push({ name: sec.name, startBar: bar, endBar: bar + sec.bars,
    startSec, endSec: (bar + sec.bars) * SPBAR });

  // chords / sub / pad
  for (let b = 0; b < sec.bars; b++) {
    const tBar = (bar + b) * SPBAR;
    const ch = CHORD[cyc(sec.chords, b)];
    const root = ch.root + tr;
    // pad = sine triad (strings), rides the duck
    for (const semi of ch.q) {
      voice(tBar + hum(0.004), SPBAR * 0.98, root + semi + 12,
        sec.name === "overture" || sec.name === "coda" ? 0.10 : 0.085,
        { atk: 0.35, rel: 0.4, vibD: 0.003,
          parts: [[1, 1], [2, 0.4], [3, 0.22], [4, 0.12], [5, 0.07]] });
    }
    if (sec.kick !== "none" || sec.name === "coda")
      sub(tBar, SPBAR * 0.99, root, sec.name === "bridge" ? 0.42 : 0.52);
  }

  // kick + perc grid
  for (let b = 0; b < sec.bars; b++) {
    const tBar = (bar + b) * SPBAR;
    const dr = sec.drive;
    const lastBar = b === sec.bars - 1;
    if (sec.kick === "four" || sec.kick === "hard") {
      for (let beat = 0; beat < 4; beat++) {
        const tk = tBar + beat * SPB + hum(0.003);
        kick(tk, dr, 1); kickEvents.push({ t: +tk.toFixed(4) });
        if (sec.kick === "hard" && beat === 3) {       // climax: offbeat double
          kick(tk + SPB * 0.5, dr * 0.9, 0.9); kickEvents.push({ t: +(tk + SPB * 0.5).toFixed(4) });
        }
      }
      if (lastBar && (sec.name === "develop")) {        // 16th roll into climax
        for (let r = 0; r < 8; r++) {
          const tk = tBar + 3 * SPB + r * (SPB / 2);
          kick(tk, dr * (0.7 + r * 0.05), 0.85);
          kickEvents.push({ t: +tk.toFixed(4) });
        }
      }
    } else if (sec.kick === "pulse") {                  // study zone: half-time
      for (const beat of [0, 2]) {
        const tk = tBar + beat * SPB; kick(tk, dr, 0.85);
        kickEvents.push({ t: +tk.toFixed(4) });
      }
    } else if (sec.kick === "fade") {
      if (b < 4) { const tk = tBar; kick(tk, dr * (1 - b / 5), 0.8);
        kickEvents.push({ t: +tk.toFixed(4) }); }
    }
    // perc tick — offbeat 8ths; sparser in the study bridge
    if (sec.kick !== "none" && sec.name !== "coda") {
      const every = sec.name === "bridge" ? 2 : 1;
      for (let s8 = 1; s8 < 8; s8 += 2 * every) {
        tick(tBar + s8 * (SPB / 2), sec.name === "bridge" ? 0.16 : 0.24);
      }
      tick(tBar + 3.5 * SPB, 0.26, true);               // metallic open hat
      snareEvents.push({ t: +(tBar + SPB).toFixed(4) });
      snareEvents.push({ t: +(tBar + 3 * SPB).toFixed(4) });
    }
  }

  // melodic layer
  const layTheme = (notes, baseGain, brass, regOff = 0) => {
    let beatPos = 0;
    for (const [off, beats] of notes) {
      const tN = sectionStartT + beatPos * SPB;
      const midi = ROOT_MEL + off + tr + regOff;
      const dur = beats * SPB * 0.96;
      if (brass) {
        voice(tN + hum(0.004), dur, midi, baseGain, {
          atk: 0.018, rel: 0.10, vibR: 5.6, vibD: 0.007, drive: 1.25,
          parts: [[1, 1], [2, 0.6], [3, 0.45], [4, 0.28], [5, 0.2], [6, 0.12], [7, 0.07]] });
        voice(tN + hum(0.004), dur, midi - 12, baseGain * 0.5, {
          atk: 0.03, rel: 0.12, parts: [[1, 1], [2, 0.4], [3, 0.2]] }); // oct double
      } else {
        voice(tN + hum(0.004), dur, midi, baseGain, {
          atk: 0.12, rel: 0.22, vibR: 4.6, vibD: 0.005,
          parts: [[1, 1], [2, 0.4], [3, 0.22], [4, 0.1]] });
      }
      beatPos += beats;
    }
    return beatPos;
  };
  const sectionStartT = startSec;

  if (sec.theme === "soft") {
    layTheme(THEME.slice(0, 11), 0.085, false);          // antecedent, strings, hushed
  } else if (sec.theme === "brass") {
    layTheme(THEME, 0.16, true);                          // full, brass
    layTheme(THEME, 0.13, true);                          // restate over bars 8–15 (loops 8-bar)
  } else if (sec.theme === "bsoft") {
    layTheme(BTHEME, 0.11, false);                        // B theme, lyrical
    layTheme(BTHEME, 0.10, false, -0)
  } else if (sec.theme === "frag") {
    // development: fragment the head (first 4 notes), sequence it up the
    // scale every 2 bars, hoover doubling, riser into the climax.
    const head = THEME.slice(0, 4);
    for (let seg = 0; seg < 8; seg++) {
      const t0 = sectionStartT + seg * 2 * SPBAR;
      let bp = 0;
      for (const [off, beats] of head) {
        const midi = ROOT_MEL + off + tr + (seg % 4) * 2;
        voice(t0 + bp * SPB + hum(0.004), beats * SPB * 0.9, midi, 0.135, {
          atk: 0.016, rel: 0.09, drive: 1.3,
          parts: [[1, 1], [2, 0.6], [3, 0.4], [4, 0.24], [5, 0.16]] });
        if (seg >= 4) hoover(t0 + bp * SPB, beats * SPB * 0.8, midi - 12, 0.13);
        bp += beats;
      }
    }
    riser(sectionStartT + (sec.bars - 2) * SPBAR, 2 * SPBAR,
      ROOT_MEL - 12, ROOT_MEL + 14, 0.28);                // scream into climax
  } else if (sec.theme === "dissolve") {
    // coda: theme fragment, each restatement softer + longer, strings
    // bloom and hold → continuous loop-friendly tail (no dead air).
    layTheme(THEME.slice(0, 8), 0.10, true);
    voice(sectionStartT + 6 * SPBAR, 6 * SPBAR + TAIL, ROOT_MEL + tr, 0.07,
      { atk: 1.2, rel: 2.4, parts: [[1, 1], [2, 0.35], [3, 0.18], [5, 0.06]] });
    voice(sectionStartT + 6 * SPBAR, 6 * SPBAR + TAIL, ROOT_MEL + tr - 12, 0.06,
      { atk: 1.2, rel: 2.4, parts: [[1, 1], [2, 0.3]] });
  }

  // stabs — climax only, on the offbeats (the hardcore "ear")
  if (sec.name === "climax") {
    for (let b = 0; b < sec.bars; b++) {
      const tBar = (bar + b) * SPBAR;
      const ch = CHORD[cyc(sec.chords, b)];
      stab(tBar + 1.5 * SPB, ch.root + tr + 24, 0.30);
      stab(tBar + 3.5 * SPB, ch.root + tr + 24 + 7, 0.26);
    }
  }
  bar += sec.bars;
}

// ── master sum: gentle bus glue, no clipping (pre-master headroom) ─────
let peak = 0;
for (let i = 0; i < N; i++) { const a = Math.abs(L[i]); const b = Math.abs(Rb[i]); if (a > peak) peak = a; if (b > peak) peak = b; }
const g = peak > 0 ? Math.min(1, 0.89 / peak) : 1;
for (let i = 0; i < N; i++) {
  L[i] = Math.tanh(L[i] * g * 1.04);
  Rb[i] = Math.tanh(Rb[i] * g * 1.04);
}

// ── write 32-bit float WAV ────────────────────────────────────────────
mkdirSync(dirname(OUT), { recursive: true });
const ch = 2, bps = 4, dataLen = N * ch * bps;
const buf = Buffer.alloc(44 + dataLen);
buf.write("RIFF", 0); buf.writeUInt32LE(36 + dataLen, 4); buf.write("WAVE", 8);
buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20); // IEEE float
buf.writeUInt16LE(ch, 22); buf.writeUInt32LE(SR, 24);
buf.writeUInt32LE(SR * ch * bps, 28); buf.writeUInt16LE(ch * bps, 32);
buf.writeUInt16LE(32, 34);
buf.write("data", 36); buf.writeUInt32LE(dataLen, 40);
let o = 44;
for (let i = 0; i < N; i++) { buf.writeFloatLE(L[i], o); o += 4; buf.writeFloatLE(Rb[i], o); o += 4; }
writeFileSync(OUT, buf);

// ── struct.json (scratch-mix grid + tooling) ──────────────────────────
mkdirSync(dirname(STRUCT), { recursive: true });
writeFileSync(STRUCT, JSON.stringify({
  engine: "hellsine", allSine: true, meter: 4, bpm: BPM,
  scale: "minor", rootMidi: 50, totalBars: TOTAL_BARS, totalSec,
  sections: sectionRanges,
  counts: { kick: kickEvents.length, snare: snareEvents.length },
  events: { kick: kickEvents, snare: snareEvents },
}, null, 2));

console.log(`hellsine · ${BPM} BPM · hell=${HELL} · ${TOTAL_BARS} bars · ${totalSec.toFixed(1)}s`);
console.log(`→ wav    · ${OUT}`);
console.log(`→ struct · ${STRUCT}`);
