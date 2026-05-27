#!/usr/bin/env node
// render.mjs — americomputadora final mix.
//
// reads:
//   melody.json        — BPM, key, hook melody (3 target MIDI notes × 4 variants), chord prog, structure
//   arrangement.json   — ordered list of clips from audition.html (the picked hook strings)
//     OR --auto         — auto-pick from utterances/* (one per group, deterministic)
//
// for every hook beat, takes a clip, detects its dominant pitch via YIN-lite
// autocorrelation, pitch-shifts to the melody's target MIDI (no rubberband
// dep — uses ffmpeg's asetrate+atempo chain), and places it on the grid.
//
// the bed is synthesized in-process: dry kick + clap on 2&4, 16th tambourine,
// sub-sine bass on chord roots, FM bell stab on hook downbeats, square lead
// shadowing the melody an octave up, toy-piano on verses.
//
// usage:
//   node bin/render.mjs                       # uses arrangement.json + melody.json
//   node bin/render.mjs --auto                # picks one clip per group automatically
//   node bin/render.mjs --bpm 116             # override bpm
//   node bin/render.mjs --bed-only            # instrumental, no hook clips
//   node bin/render.mjs --wav                 # output wav instead of mp3
//   node bin/render.mjs --out ~/Desktop/x.mp3 # explicit out path

import { writeFileSync, mkdirSync, unlinkSync, readFileSync, existsSync, readdirSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir, tmpdir } from "node:os";

import { readWavMono } from "../../lib/wav.mjs";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);

// ── args ────────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

// ── load melody + arrangement ──────────────────────────────────────────
const melody = JSON.parse(readFileSync(join(ROOT, "melody.json"), "utf8"));
const BPM = Number(flags.bpm ?? melody.bpm);
const beat = 60 / BPM;
const bar = beat * 4;

function autoArrangement() {
  // deterministic pick: first utterance found per group. computer prefers fred-mid
  // (the canon synth-voice), america/dora pick alphabetically.
  const pick = {};
  for (const g of ["america", "computer", "dora"]) {
    const d = join(ROOT, "utterances", g);
    if (!existsSync(d)) continue;
    const files = readdirSync(d).filter((f) => f.endsWith(".wav"));
    if (!files.length) continue;
    const preferred = g === "computer" ? files.find((f) => f === "fred-mid.wav") : null;
    pick[g] = preferred || files.sort()[0];
  }
  return pick;
}

let arrangement;
const arrPath = expandHome(flags.arr) || join(ROOT, "arrangement.json");
if (flags.auto || !existsSync(arrPath)) {
  const a = autoArrangement();
  arrangement = {
    america: a.america ? join("utterances", "america", a.america) : null,
    computer: a.computer ? join("utterances", "computer", a.computer) : null,
    dora: a.dora ? join("utterances", "dora", a.dora) : null,
  };
  if (!flags["bed-only"]) {
    console.log(`# auto-arrangement (no arrangement.json or --auto flag):`);
    for (const [g, p] of Object.entries(arrangement)) console.log(`  ${g}: ${p || "(missing)"}`);
  }
} else {
  // arrangement.json from audition.html is a flat ordered list of clips;
  // we just want one per group. take the first occurrence of each group.
  const raw = JSON.parse(readFileSync(arrPath, "utf8"));
  arrangement = { america: null, computer: null, dora: null };
  for (const e of raw) {
    if (arrangement[e.group] === null) arrangement[e.group] = e.path;
  }
  console.log(`# arrangement.json picks:`);
  for (const [g, p] of Object.entries(arrangement)) console.log(`  ${g}: ${p || "(missing)"}`);
}

// ── helpers ─────────────────────────────────────────────────────────────
const TAU = Math.PI * 2;
const midiToHz = (m) => 440 * Math.pow(2, (m - 69) / 12);
function add(buf, idx, v) { if (idx >= 0 && idx < buf.length) buf[idx] += v; }

// ── pitch detection (YIN-lite autocorrelation) ─────────────────────────
// good enough for whole-word vocal clips where one fundamental dominates.
function detectPitchHz(samples, sr) {
  // use central 60% of the clip (skip onset transient + fade tail)
  const n = samples.length;
  const start = Math.floor(n * 0.2);
  const end = Math.floor(n * 0.8);
  const seg = samples.subarray(start, end);
  const minLag = Math.floor(sr / 600); // 600 Hz upper bound
  const maxLag = Math.floor(sr / 80);  // 80 Hz lower bound
  let bestLag = 0, bestScore = -Infinity;
  for (let lag = minLag; lag <= maxLag; lag++) {
    let r = 0, e0 = 0, e1 = 0;
    for (let i = 0; i + lag < seg.length; i++) {
      r += seg[i] * seg[i + lag];
      e0 += seg[i] * seg[i];
      e1 += seg[i + lag] * seg[i + lag];
    }
    const norm = Math.sqrt(e0 * e1) || 1;
    const score = r / norm;
    if (score > bestScore) { bestScore = score; bestLag = lag; }
  }
  if (bestLag === 0) return null;
  return sr / bestLag;
}

const hzToMidi = (hz) => 69 + 12 * Math.log2(hz / 440);

// ── pitch-shift via ffmpeg (asetrate + atempo) ─────────────────────────
function chainAtempo(target) {
  const stages = [];
  let r = target;
  while (r > 2.0) { stages.push(2.0); r /= 2.0; }
  while (r < 0.5) { stages.push(0.5); r /= 0.5; }
  stages.push(+r.toFixed(6));
  return stages.map((s) => `atempo=${s}`).join(",");
}

function pitchShiftWav(srcAbs, semitones) {
  // write a temp wav with pitch shifted by `semitones`, return Float32Array.
  const ratio = Math.pow(2, semitones / 12);
  const tmp = join(tmpdir(), `acd-${process.pid}-${Date.now()}-${Math.random().toString(36).slice(2, 8)}.wav`);
  const filter = `asetrate=${SR}*${ratio},aresample=${SR},${chainAtempo(1 / ratio)}`;
  const res = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", srcAbs,
    "-af", filter,
    "-ac", "1", "-ar", String(SR),
    tmp,
  ], { stdio: ["ignore", "ignore", "inherit"] });
  if (res.status !== 0) throw new Error(`pitch-shift failed for ${srcAbs}`);
  const { samples } = readWavMono(tmp);
  try { unlinkSync(tmp); } catch {}
  return samples;
}

// ── load + pitch-correct each hook clip ────────────────────────────────
// each group gets autotuned to a single dominant MIDI source pitch, then we
// store per-target-midi pre-rendered Float32Arrays so the mix loop is fast.
const hookClips = {}; // { america: { [targetMidi]: Float32Array }, ... }

function prepareClip(group, relPath) {
  if (!relPath) return null;
  const abs = join(ROOT, relPath);
  if (!existsSync(abs)) {
    console.warn(`  ! ${group}: missing ${relPath}, skipping`);
    return null;
  }
  const { samples } = readWavMono(abs);
  const hz = detectPitchHz(samples, SR);
  const srcMidi = hz ? hzToMidi(hz) : null;
  console.log(`  · ${group}: ${relPath.split("/").pop()}  source ≈ ${hz ? hz.toFixed(1) + " Hz / MIDI " + srcMidi.toFixed(1) : "unknown"}`);
  // gather all target midi notes needed across hook variants
  const wordIdx = melody.hook.words.indexOf(group);
  if (wordIdx < 0) return { source: samples, srcMidi };
  const targets = new Set(melody.hook.variants.map((v) => v.notes[wordIdx]));
  const out = { source: samples, srcMidi, byMidi: {} };
  for (const t of targets) {
    if (srcMidi == null) {
      // pitch undetected — just place at unity. better than crashing.
      out.byMidi[t] = samples;
      continue;
    }
    // shift to land on target. clamp to ±18 semitones so we don't time-stretch wildly.
    let semis = t - srcMidi;
    while (semis > 12) semis -= 12;
    while (semis < -12) semis += 12;
    out.byMidi[t] = pitchShiftWav(abs, semis);
  }
  return out;
}

console.log("\n# preparing hook clips (pitch detect + autotune):");
for (const g of ["america", "computer", "dora"]) {
  hookClips[g] = prepareClip(g, arrangement[g]);
}

// ── total length from structure ────────────────────────────────────────
let totalBars = 0;
for (const sec of melody.structure) totalBars += sec.bars;
const totalSec = totalBars * bar + 0.5; // tail
const N = Math.ceil(totalSec * SR);

// ── synth voices (bubblegum kit) ───────────────────────────────────────
// kick — punchy, dry, short. major-key pop kick.
function kick(buf, t, g = 1.0) {
  const dur = 0.18, n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const tt = i / SR;
    const f = 55 + (140 - 55) * Math.exp(-tt * 38);
    ph += (TAU * f) / SR;
    const body = Math.sin(ph) * Math.exp(-tt * 11);
    const click = i < SR * 0.002 ? (Math.random() * 2 - 1) * 0.4 * (1 - i / (SR * 0.002)) : 0;
    add(buf, s0 + i, (body + click) * g);
  }
}

// clap — short stack of 4 noise bursts spaced ~7 ms (classic clap envelope).
function clap(buf, t, g = 0.9) {
  const burstGap = 0.007, bursts = 4, burstDur = 0.012;
  for (let b = 0; b < bursts; b++) {
    const bt = t + b * burstGap;
    const s0 = Math.floor(bt * SR), n = Math.floor(burstDur * SR);
    let prev = 0;
    for (let i = 0; i < n; i++) {
      const nz = Math.random() * 2 - 1;
      const hp = nz - prev; prev = nz;
      add(buf, s0 + i, hp * Math.exp(-(i / SR) * 120) * g * (b === bursts - 1 ? 1 : 0.55));
    }
  }
  // bright tail
  const s0 = Math.floor(t * SR), tail = Math.floor(0.13 * SR);
  let prev = 0;
  for (let i = 0; i < tail; i++) {
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-(i / SR) * 22) * g * 0.18);
  }
}

// closed hat / tambourine — bright 16th shimmer.
function tamb(buf, t, g = 0.10) {
  const dur = 0.05, n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-(i / SR) * 80) * g);
  }
}

// FM bell stab — 2-op (sine carrier + sine modulator). DX-7ish.
function bell(buf, t, midi, dur = 0.6, g = 0.22) {
  const fc = midiToHz(midi);
  const fm = fc * 3.5; // inharmonic ratio for bell-like spectrum
  const n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  let phc = 0, phm = 0;
  for (let i = 0; i < n; i++) {
    const tt = i / SR;
    const env = Math.exp(-tt * 5.5);
    const modIndex = 4.5 * Math.exp(-tt * 9);
    phm += (TAU * fm) / SR;
    const mod = Math.sin(phm) * modIndex;
    phc += (TAU * fc) / SR + mod / SR;
    add(buf, s0 + i, Math.sin(phc) * env * g);
  }
}

// square lead — bright pulse wave with gentle envelope.
function square(buf, t, midi, dur, g = 0.16) {
  const f = midiToHz(midi);
  const n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  const att = Math.floor(0.008 * SR), rel = Math.floor(0.04 * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    ph += (TAU * f) / SR;
    const sq = Math.sin(ph) > 0 ? 1 : -1;
    let env = 1;
    if (i < att) env = i / att;
    else if (i > n - rel) env = Math.max(0, (n - i) / rel);
    add(buf, s0 + i, sq * env * g);
  }
}

// sub-bass — clean sine on the chord root. minimal saturation.
function sub(buf, t, midi, dur, g = 0.35) {
  const f = midiToHz(midi - 12); // octave below for low-end
  const n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  const att = Math.floor(0.012 * SR), rel = Math.floor(0.04 * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    ph += (TAU * f) / SR;
    let env = 1;
    if (i < att) env = i / att;
    else if (i > n - rel) env = Math.max(0, (n - i) / rel);
    add(buf, s0 + i, Math.tanh(Math.sin(ph) * 1.4) * env * g);
  }
}

// toy piano — sine + harmonic 2/3 + short decay. mallet-y.
function toyPiano(buf, t, midi, dur, g = 0.14) {
  const f = midiToHz(midi);
  const n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  let p1 = 0, p2 = 0, p3 = 0;
  for (let i = 0; i < n; i++) {
    const tt = i / SR;
    p1 += (TAU * f) / SR;
    p2 += (TAU * f * 2.01) / SR;
    p3 += (TAU * f * 3.04) / SR;
    const env = Math.exp(-tt * 4.5);
    const s = (Math.sin(p1) * 0.55 + Math.sin(p2) * 0.30 + Math.sin(p3) * 0.18) * env;
    add(buf, s0 + i, s * g);
  }
}

// paint a sample buffer into the mix at startSec with gain.
function paint(buf, sampBuf, startSec, g = 1.0) {
  const s0 = Math.floor(startSec * SR);
  for (let i = 0; i < sampBuf.length; i++) {
    add(buf, s0 + i, sampBuf[i] * g);
  }
}

// ── render ──────────────────────────────────────────────────────────────
const drm = new Float32Array(N);
const bel = new Float32Array(N);
const sqr = new Float32Array(N);
const sb  = new Float32Array(N);
const toy = new Float32Array(N);
const voc = new Float32Array(N);

const prog = melody.bed.chord_progression; // 8 bars of I-V-vi-IV
const progBars = prog.reduce((a, c) => a + c.bars, 0);

function chordAtBar(barIdx) {
  let b = barIdx % progBars;
  for (const c of prog) {
    if (b < c.bars) return c;
    b -= c.bars;
  }
  return prog[0];
}

// schedule everything
let barCursor = 0;
let hookCount = 0;
const kickTimes = [];
const beat16 = beat / 4;

for (const sec of melody.structure) {
  const reps = sec.reps || 1;
  for (let r = 0; r < reps; r++) {
    for (let b = 0; b < sec.bars; b++) {
      const bt = (barCursor + b) * bar;
      const ch = chordAtBar(barCursor + b);
      const root = ch.root_midi;
      const third = root + (ch.quality === "min" ? 3 : 4);
      const fifth = root + 7;

      // sub on the root, every bar
      if (sec.feel !== "claps-only" && sec.feel !== "fade") {
        sub(sb, bt, root, bar * 0.95, sec.feel === "thin" || sec.feel === "sparse" ? 0.22 : 0.35);
      }

      // drums per feel
      if (sec.section === "stop" || sec.feel === "claps-only") {
        // stop-time bar: only clap on 2 & 4
        clap(drm, bt + beat * 1, 1.05);
        clap(drm, bt + beat * 3, 1.05);
      } else if (sec.section === "intro" || sec.feel === "fade") {
        // soft kick on 1 only, light tambourine
        kick(drm, bt, 0.55);
        for (let i = 0; i < 8; i++) tamb(drm, bt + i * (beat / 2), 0.05);
      } else if (sec.section === "verse") {
        // verse: thinned — kick on 1 & 3, clap on 2 & 4 (quieter), no tamb
        kick(drm, bt, 0.75); kickTimes.push(bt);
        kick(drm, bt + beat * 2, 0.75); kickTimes.push(bt + beat * 2);
        clap(drm, bt + beat, 0.55);
        clap(drm, bt + beat * 3, 0.55);
        // verse melody on toy piano — outline I-V-vi-IV with arpeggios
        if (sec.feel === "thin") {
          toyPiano(toy, bt,            third,  beat * 0.9, 0.16);
          toyPiano(toy, bt + beat,     fifth,  beat * 0.9, 0.13);
          toyPiano(toy, bt + beat * 2, root + 12, beat * 0.9, 0.13);
          toyPiano(toy, bt + beat * 3, fifth,  beat * 0.9, 0.13);
        } else {
          // sparse — fewer notes
          toyPiano(toy, bt,            third,     beat * 1.5, 0.14);
          toyPiano(toy, bt + beat * 2, root + 12, beat * 1.5, 0.14);
        }
      } else if (sec.section === "bridge") {
        // bridge: just sub + soft kick + atmospheric bell on each downbeat
        kick(drm, bt, 0.65); kickTimes.push(bt);
        bell(bel, bt, root + 12, bar * 0.9, 0.18);
      } else {
        // hook — full bubblegum kit
        kick(drm, bt, 1.0); kickTimes.push(bt);
        kick(drm, bt + beat * 2, 1.0); kickTimes.push(bt + beat * 2);
        clap(drm, bt + beat, 0.95);
        clap(drm, bt + beat * 3, 0.95);
        // 16th tambourine
        for (let i = 0; i < 16; i++) tamb(drm, bt + i * beat16, i % 4 === 0 ? 0.14 : 0.09);
        // DX bell on downbeat — chord root up an octave
        bell(bel, bt, root + 12, bar * 0.95, 0.22);
      }
    }
    // hook reps: pick a melody variant and paint the syllables + square lead
    if (sec.section === "hook") {
      const variant = melody.hook.variants[hookCount % melody.hook.variants.length];
      const words = melody.hook.words;
      const lens = melody.hook.beats_per_word;
      // hook is 4 bars × 4 beats = 16 beats, repeats per bar. each bar plays the
      // 3-word hook once: america(1 beat) + computer(1 beat) + dora(2 beats).
      for (let b = 0; b < sec.bars; b++) {
        const barStart = (barCursor + b) * bar;
        let beatOff = 0;
        for (let w = 0; w < words.length; w++) {
          const word = words[w];
          const noteLen = lens[w] * beat;
          const targetMidi = variant.notes[w];
          // paint the autotuned vocal sample
          const clip = hookClips[word];
          if (clip && clip.byMidi && clip.byMidi[targetMidi]) {
            paint(voc, clip.byMidi[targetMidi], barStart + beatOff, 0.95);
          }
          // square lead shadows the melody an octave up — adds the candy
          square(sqr, barStart + beatOff, targetMidi + 12, noteLen * 0.9, 0.12);
          beatOff += noteLen;
        }
      }
      hookCount++;
    }
    barCursor += sec.bars;
  }
}

// ── light sidechain duck on bell + square under each kick ─────────────
const duck = new Float32Array(N).fill(1);
for (const kt of kickTimes) {
  const s0 = Math.floor(kt * SR), len = Math.floor(0.10 * SR);
  for (let i = 0; i < len && s0 + i < N; i++) {
    const w = i / len;
    const d = 0.78 + 0.22 * w;
    if (d < duck[s0 + i]) duck[s0 + i] = d;
  }
}

// ── fade-out on the outro section (last 4 bars) ────────────────────────
const fadeStart = (totalBars - 4) * bar;
const fadeStartSamp = Math.floor(fadeStart * SR);
const fadeMaster = new Float32Array(N).fill(1);
for (let i = fadeStartSamp; i < N; i++) {
  fadeMaster[i] = Math.max(0, 1 - (i - fadeStartSamp) / (N - fadeStartSamp));
}

// ── master ─────────────────────────────────────────────────────────────
const mix = new Float32Array(N);
const bedOnly = !!flags["bed-only"];
for (let i = 0; i < N; i++) {
  const drumsBus = drm[i];
  const melodicBus = (bel[i] + sqr[i] + toy[i]) * duck[i];
  const vocalBus = bedOnly ? 0 : voc[i];
  const s = drumsBus + melodicBus + sb[i] + vocalBus * 0.85;
  mix[i] = Math.tanh(s * 0.95) * fadeMaster[i];
}
let peak = 0;
for (let i = 0; i < N; i++) { const a = Math.abs(mix[i]); if (a > peak) peak = a; }
if (peak > 0) { const n = 0.85 / peak; for (let i = 0; i < N; i++) mix[i] *= n; }

// ── write ──────────────────────────────────────────────────────────────
const wantWav = !!flags.wav;
const ext = wantWav ? "wav" : "mp3";
const outPath = expandHome(flags.out) || resolve(ROOT, `out/americomputadora${bedOnly ? "-bed" : ""}.${ext}`);
mkdirSync(dirname(outPath), { recursive: true });

const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(N * 4);
for (let i = 0; i < N; i++) b.writeFloatLE(mix[i], i * 4);
writeFileSync(rawPath, b);

console.log(
  `\n# americomputadora · ${BPM} BPM · ${melody.key} ${melody.mode} · ` +
  `${totalBars} bars · ${totalSec.toFixed(1)}s` +
  (bedOnly ? " · (bed only)" : "")
);

const ffArgs = wantWav
  ? [
      "-hide_banner", "-y", "-loglevel", "error",
      "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
      "-c:a", "pcm_s16le", outPath,
    ]
  : [
      "-hide_banner", "-y", "-loglevel", "error",
      "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
      "-c:a", "libmp3lame", "-q:a", "2",
      "-metadata", "title=americomputadora",
      "-metadata", "artist=jeffrey",
      "-metadata", "album=pixsies",
      outPath,
    ];

const ff = spawnSync("ffmpeg", ffArgs, { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath}`);
