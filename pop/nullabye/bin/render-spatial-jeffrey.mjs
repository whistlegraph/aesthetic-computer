#!/usr/bin/env node
// render-spatial-jeffrey.mjs — Jeffrey's stretched phoneme choir for the
// two-minute spatial sineabye room mix.
//
// The first four bars remain Jeffrey-choir-free while the C bed's melody and
// beat move through the decelerating whole-room spin.  The choir enters as
// mm/oo, opens through oh,
// blooms to ah/eh through the 62–70 s eight-turn centrifuge, then closes back
// toward oo/mm.  All sources are the project-owned, pitch-locked Jeffrey takes
// already used by momabobasheep; this renderer makes no network/API calls.
//
// Usage:
//   node pop/nullabye/bin/render-spatial-jeffrey.mjs
//   node pop/nullabye/bin/render-spatial-jeffrey.mjs --bed path/to/bed.wav \
//     --out path/to/MASTER.wav --mp3 path/to/preview.mp3

import { existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, extname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { readWavMono } from "../../lib/wav.mjs";
import { pitchTrack } from "../../lib/analysis.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const SR = 48_000;
const BPM = 76;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const TOTAL_SEC = 38 * BAR;
const NS = Math.round(TOTAL_SEC * SR);
const arg = (key, fallback = null) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
};

const bedPath = resolve(arg("--bed", resolve(HERE, "../out/review/spatial-room-spin-clean.wav")));
const outPath = resolve(arg("--out", resolve(HERE, "../out/review/spatial-room-spin-jeffrey-MASTER.wav")));
const mp3Path = resolve(arg("--mp3", outPath.slice(0, -extname(outPath).length) + ".mp3"));
const stemPath = resolve(arg("--stem", outPath.slice(0, -extname(outPath).length) + "-VOCALS.wav"));
const manifestPath = resolve(REPO, "pop/momboba/voice/manifest.json");

for (const p of [bedPath, manifestPath]) {
  if (!existsSync(p)) throw new Error(`missing required input: ${p}`);
}
for (const p of [outPath, mp3Path, stemPath]) mkdirSync(dirname(p), { recursive: true });

const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));

// Turn a clean vowel take into a long, seam-crossfaded sustain.  Locating its
// strongest contiguous voiced run also excludes keyboard clicks and breaths.
function loadSustain(path) {
  const { samples, sampleRate } = readWavMono(resolve(REPO, path));
  const frames = pitchTrack(samples, { sampleRate, fmin: 60, fmax: 500 });
  let best = [0, 0], cur = null;
  frames.forEach((r, i) => {
    if (r.hz != null && r.clarity > 0.55) {
      if (!cur) cur = [i, i]; else cur[1] = i;
      if (cur[1] - cur[0] > best[1] - best[0]) best = [...cur];
    } else cur = null;
  });
  const hop = Math.floor(0.010 * sampleRate);
  const s0 = Math.max(0, best[0] * hop - Math.floor(0.06 * sampleRate));
  const s1 = Math.min(samples.length, best[1] * hop + Math.floor(0.10 * sampleRate));
  const seg = Float32Array.from(samples.subarray(s0, s1));
  const xf = Math.min(Math.floor(0.40 * sampleRate), Math.floor(seg.length * 0.14));
  const ls = Math.floor(seg.length * 0.25), le = Math.floor(seg.length * 0.82);
  const total = Math.floor(18 * sampleRate);
  const sus = new Float32Array(total);
  sus.set(seg.subarray(0, Math.min(le, total)));
  let w = Math.min(le, total);
  while (w < total && le - ls > xf * 2) {
    for (let i = 0; i < xf; i++) {
      const dst = w - xf + i;
      if (dst < 0 || dst >= total) continue;
      const a = (i / xf) * Math.PI / 2;
      sus[dst] = sus[dst] * Math.cos(a) + seg[ls + i] * Math.sin(a);
    }
    const chunk = Math.min(le - ls - xf, total - w);
    sus.set(seg.subarray(ls + xf, ls + xf + chunk), w);
    w += chunk;
  }
  return { samples: sus, sampleRate };
}

console.log("[voice] loading Jeffrey's pitch-locked phonemes…");
const takes = new Map(manifest.map((m) => {
  const natural = loadSustain(m.file);
  const flat = loadSustain(m.flatFile || m.file);
  return [m.vowel, { natural, flat, midi: m.midi, source: m.file, flatSource: m.flatFile || m.file }];
}));

const busL = new Float32Array(NS), busR = new Float32Array(NS);
const smooth = (x) => { x = Math.max(0, Math.min(1, x)); return x * x * (3 - 2 * x); };

// Match the C renderer's exact eight-turn 62–70 s centrifuge.  The vocal
// images orbit with the room instead of sitting like a centered overdub.
function orbitPan(t, basePan, phase) {
  const slow = 0.12 * Math.sin(t * 0.19 + phase);
  if (t < 62 || t > 70) return Math.max(-0.84, Math.min(0.84, basePan + slow));
  const u = smooth((t - 62) / 8);
  const a = Math.PI * 2 * 8 * u + phase;
  return Math.max(-0.9, Math.min(0.9, basePan * 0.28 + 0.78 * Math.sin(a)));
}

function placeVoice({ vowel, midi, start, dur, gain, pan, phase = 0, flat = false, detune = 0 }) {
  const take = takes.get(vowel);
  if (!take) throw new Error(`missing vowel take: ${vowel}`);
  const source = flat ? take.flat : take.natural;
  const rate = Math.pow(2, (midi + detune - take.midi) / 12) * (source.sampleRate / SR);
  const nOut = Math.min(Math.floor(dur * SR), Math.floor((source.samples.length - 2) / rate));
  const i0 = Math.floor(start * SR);
  const attack = Math.min(1.35 * SR, nOut * 0.34);
  const release = Math.min(2.1 * SR, nOut * 0.42);
  const cutoff = flat ? 1150 : vowel === "eh" ? 2500 : vowel === "ah" ? 2200 : 1750;
  const k = 1 - Math.exp(-2 * Math.PI * cutoff / SR);
  let pos = 0, lp = 0;
  for (let i = 0; i < nOut; i++) {
    const dst = i0 + i;
    if (dst < 0) { pos += rate; continue; }
    if (dst >= NS) break;
    const pi = Math.floor(pos), f = pos - pi;
    if (pi + 1 >= source.samples.length) break;
    const raw = source.samples[pi] * (1 - f) + source.samples[pi + 1] * f;
    lp += k * (raw - lp);
    let env = i < attack ? 0.5 - 0.5 * Math.cos(Math.PI * i / attack) : 1;
    const remain = nOut - i;
    if (remain < release) env *= 0.5 - 0.5 * Math.cos(Math.PI * remain / release);
    const p = orbitPan(dst / SR, pan, phase);
    const a = (p + 1) * Math.PI / 4;
    const s = lp * env * gain;
    busL[dst] += s * Math.cos(a);
    busR[dst] += s * Math.sin(a);
    pos += rate;
  }
}

// Compact voicings stay close to the B2 source.  During bars 20–27 the C
// score rises a perfect fourth; wrapping upper notes down keeps Jeffrey human.
const CHORDS = [
  [43, 48, 52], // C/G
  [45, 48, 52], // Am
  [41, 45, 48], // F
  [43, 47, 50], // G
];
function chordForBar(bar) {
  const tones = CHORDS[bar % 4].map((m) => m + (bar >= 20 && bar < 28 ? 5 : 0));
  return tones.map((m) => m > 53 ? m - 12 : m).sort((a, b) => a - b);
}

const phrases = [
  { bar: 4,  beats: 7.5, vowel: "mm", voices: 1, gain: 0.050 },
  { bar: 7,  beats: 7.2, vowel: "oo", voices: 2, gain: 0.044 },
  { bar: 10, beats: 7.0, vowel: "oo", voices: 2, gain: 0.045 },
  { bar: 12, beats: 8.3, vowel: "oh", voices: 3, gain: 0.038 },
  { bar: 15, beats: 7.3, vowel: "oh", voices: 2, gain: 0.043 },
  { bar: 18, beats: 6.0, vowel: "mm", voices: 2, gain: 0.044 },
  // A consonant-like closed hum opens into vowels right as the room accelerates.
  { bar: 19, beat: 1.0, beats: 4.0, vowel: "mm", voices: 3, gain: 0.047 },
  { bar: 20, beat: 0.5, beats: 7.0, vowel: "oh", voices: 3, gain: 0.052 },
  { bar: 21, beat: 2.0, beats: 6.0, vowel: "ah", voices: 3, gain: 0.056 },
  { bar: 22, beat: 1.0, beats: 5.0, vowel: "eh", voices: 3, gain: 0.047 },
  { bar: 23, beat: 2.0, beats: 7.0, vowel: "ah", voices: 3, gain: 0.052 },
  { bar: 25, beats: 7.2, vowel: "oh", voices: 3, gain: 0.047 },
  { bar: 28, beats: 8.0, vowel: "oh", voices: 3, gain: 0.043 },
  { bar: 31, beats: 7.4, vowel: "oo", voices: 2, gain: 0.044 },
  { bar: 34, beats: 7.0, vowel: "mm", voices: 2, gain: 0.040 },
  { bar: 36, beats: 7.5, vowel: "oo", voices: 1, gain: 0.046 },
];

const rendered = [];
phrases.forEach((p, pi) => {
  const chord = chordForBar(p.bar);
  const start = p.bar * BAR + (p.beat || 0) * BEAT;
  const dur = p.beats * BEAT;
  const order = p.voices === 1 ? [1] : p.voices === 2 ? [0, 2] : [0, 1, 2];
  order.forEach((ci, vi) => {
    const midi = chord[ci];
    const pan = p.voices === 1 ? 0 : (vi / (p.voices - 1)) * 1.12 - 0.56;
    const stagger = vi * 0.13;
    placeVoice({ vowel: p.vowel, midi, start: start + stagger, dur: dur - stagger,
      gain: p.gain * (vi === 1 ? 0.92 : 1), pan, phase: pi * 1.37 + vi * 2.1,
      flat: midi <= 43 });
    // A nearly-unison shadow makes one take sound like a small human section.
    if (p.voices >= 2 && vi !== 1) {
      placeVoice({ vowel: p.vowel, midi, start: start + stagger + 0.18, dur: dur - stagger - 0.15,
        gain: p.gain * 0.36, pan: -pan * 0.72, phase: pi * 0.83 + vi,
        flat: midi <= 43, detune: vi ? 0.09 : -0.11 });
    }
    rendered.push({ vowel: p.vowel, midi, start: +(start + stagger).toFixed(3),
      dur: +(dur - stagger).toFixed(3), pan: +pan.toFixed(2) });
  });
});

// A long, dark vocal room.  It gives the phonemes depth without washing the
// direct bed or creating a new full-band noise layer.
{
  const mk = (n) => ({ b: new Float32Array(n), i: 0, lp: 0 });
  const combL = [1789, 1999, 2131, 2381].map(mk);
  const combR = [1831, 2029, 2179, 2417].map(mk);
  const allL = [877, 683].map(mk), allR = [919, 727].map(mk);
  const comb = (c, x) => {
    const y = c.b[c.i];
    c.lp = y * 0.56 + c.lp * 0.44;
    c.b[c.i] = x + c.lp * 0.83;
    c.i = (c.i + 1) % c.b.length;
    return y;
  };
  const allpass = (c, x) => {
    const y = c.b[c.i];
    const out = -x + y;
    c.b[c.i] = x + y * 0.5;
    c.i = (c.i + 1) % c.b.length;
    return out;
  };
  for (let i = 0; i < NS; i++) {
    const l = busL[i], r = busR[i];
    let wl = 0, wr = 0;
    for (const c of combL) wl += comb(c, l);
    for (const c of combR) wr += comb(c, r);
    for (const c of allL) wl = allpass(c, wl);
    for (const c of allR) wr = allpass(c, wr);
    busL[i] = l * 0.72 + wl * 0.18;
    busR[i] = r * 0.72 + wr * 0.18;
  }
}

let peak = 0;
for (let i = 0; i < NS; i++) peak = Math.max(peak, Math.abs(busL[i]), Math.abs(busR[i]));
if (peak > 0) {
  const gain = 0.54 / peak;
  for (let i = 0; i < NS; i++) { busL[i] *= gain; busR[i] *= gain; }
}

const rawPath = `${stemPath}.f32.raw`;
const raw = Buffer.alloc(NS * 8);
for (let i = 0; i < NS; i++) {
  raw.writeFloatLE(busL[i], i * 8);
  raw.writeFloatLE(busR[i], i * 8 + 4);
}
writeFileSync(rawPath, raw);

const run = (args, capture = false) => {
  const r = spawnSync("ffmpeg", args, capture ? { encoding: "utf8" } : { stdio: "inherit" });
  if (r.status !== 0) throw new Error(`ffmpeg failed (${r.status})`);
  return r;
};

run(["-hide_banner", "-y", "-loglevel", "error", "-f", "f32le", "-ar", String(SR),
  "-ac", "2", "-i", rawPath, "-c:a", "pcm_s24le", stemPath]);

// Build a float premaster, then use loudnorm only as a meter.  The final gain
// is linear, preserving the authored spin/choir dynamics and staying below
// both the -15 LUFS musical target and -1.2 dBTP ceiling.
const premasterPath = `${outPath}.premaster.wav`;
const mixFilter = [
  "[0:a]volume=0.94[bed]",
  "[1:a]volume=0.82[voc]",
  "[bed][voc]amix=inputs=2:normalize=0:duration=first," +
    "highpass=f=28,equalizer=f=72:t=q:w=.8:g=1.2," +
    "equalizer=f=7200:t=q:w=.9:g=-1,lowpass=f=15800," +
    "alimiter=limit=.87:attack=6:release=120[mix]",
].join(";");
run(["-hide_banner", "-y", "-loglevel", "error", "-i", bedPath, "-i", stemPath,
  "-filter_complex", mixFilter, "-map", "[mix]", "-ar", String(SR), "-c:a", "pcm_f32le", premasterPath]);

console.log("[master] measuring the bed + choir…");
const measured = run(["-hide_banner", "-nostats", "-loglevel", "info", "-i", premasterPath,
  "-af", "loudnorm=print_format=json", "-f", "null", "-"], true);
const field = (name) => {
  const m = (measured.stderr || "").match(new RegExp(`"${name}"\\s*:\\s*"([^"]+)"`));
  return m ? parseFloat(m[1]) : NaN;
};
const inputI = field("input_i"), inputTp = field("input_tp");
const gainDb = Number.isFinite(inputI) && Number.isFinite(inputTp)
  ? Math.min(-15 - inputI, -1.2 - inputTp) : -1;
const finalFilter = `volume=${gainDb.toFixed(2)}dB`;
console.log(`   · measured ${inputI.toFixed(2)} LUFS / ${inputTp.toFixed(2)} dBTP · linear gain ${gainDb >= 0 ? "+" : ""}${gainDb.toFixed(2)} dB`);
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premasterPath, "-af", finalFilter,
  "-ar", String(SR), "-c:a", "pcm_s24le", outPath]);
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premasterPath, "-af", finalFilter,
  "-ar", String(SR), "-c:a", "libmp3lame", "-b:a", "320k", mp3Path]);

const provenancePath = outPath.replace(/\.[^.]+$/, ".provenance.json");
writeFileSync(provenancePath, JSON.stringify({
  title: "spatial sineabye — Jeffrey choir candidate",
  bed: bedPath.replace(REPO + "/", ""), bpm: BPM, duration: TOTAL_SEC,
  opening: "bars 0–3 Jeffrey-choir-free; melody, beat, and 96-turn decelerating room spin are rendered in C bed",
  superSpin: { start: 62, end: 70, turns: 8, vocalOrbitMatched: true },
  voiceManifest: manifestPath.replace(REPO + "/", ""),
  sources: manifest.map(({ file, flatFile, vowel, midi }) => ({ file, flatFile, vowel, midi })),
  events: rendered,
  mastering: { targetLufs: -15, maxTruePeakDb: -1.2, measuredPremasterLufs: inputI,
    measuredPremasterTruePeakDb: inputTp, linearGainDb: gainDb, sampleRate: SR, bitDepth: 24 },
}, null, 2) + "\n");

for (const p of [rawPath, premasterPath]) { try { unlinkSync(p); } catch {} }
console.log(`✓ vocal stem → ${stemPath}`);
console.log(`✓ 24-bit release candidate → ${outPath}`);
console.log(`✓ listening copy → ${mp3Path}`);
console.log(`✓ provenance → ${provenancePath}`);
