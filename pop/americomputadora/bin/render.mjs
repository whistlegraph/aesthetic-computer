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
// the hook clips are pulled from the snapped/ library (bin/hooks.mjs) when it
// exists — already autotuned + loudnormed, with whispered twins — and only
// fall back to live pitch-shifting when a snap is missing.
//
// usage:
//   node bin/render.mjs                       # uses arrangement.json + melody.json
//   node bin/render.mjs --auto                # picks one clip per group automatically
//   node bin/render.mjs --pick america=whitney-houston-w1 --pick computer=fred-mid
//   node bin/render.mjs --whisper             # whispered hook vocals (all slots)
//   node bin/render.mjs --whisper america,dora  # whisper just those slots
//   node bin/render.mjs --fav hooks-favorites.json --fav-index 0  # from hooks.html export
//   node bin/render.mjs --bpm 116             # override bpm
//   node bin/render.mjs --bed-only            # instrumental, no hook clips
//   node bin/render.mjs --wav                 # output wav instead of mp3
//   node bin/render.mjs --out ~/Desktop/x.mp3 # explicit out path

import { writeFileSync, mkdirSync, unlinkSync, readFileSync, existsSync, readdirSync, statSync } from "node:fs";
import { resolve, dirname, join, basename } from "node:path";
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
const pickPins = {};    // --pick group=name[,name…] — rotates per phrase
const stretchPins = {}; // --stretch group=factor — time-stretch, pitch kept
const decapPins = {};   // --decap group — chop the leading consonant ("ahmputer")
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a === "--pick") {
    const [g, name] = (argv[++i] || "").split("=");
    if (g && name) pickPins[g] = name.split(",").map((n) => n.trim().replace(/\.wav$/, "")).filter(Boolean);
  } else if (a === "--stretch") {
    const [g, f] = (argv[++i] || "").split("=");
    if (g && +f > 0) stretchPins[g] = +f;
  } else if (a === "--decap") {
    for (const g of (argv[++i] || "").split(",")) if (g) decapPins[g.trim()] = true;
  } else if (a.startsWith("--")) {
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
  // duration-aware pick: the clip whose length best fits its hook slot, so a
  // 7-second phrase never lands in a 1-second hole. 16-bit mono 48k wavs →
  // duration ≈ (bytes - header) / 2 / 48000, close enough to rank by.
  const pick = {};
  melody.hook.words.forEach((g, w) => {
    const d = join(ROOT, "utterances", g);
    if (!existsSync(d)) return;
    const files = readdirSync(d).filter((f) => f.endsWith(".wav"));
    if (!files.length) return;
    const slot = melody.hook.beats_per_word[w] * beat + 0.15;
    let best = files[0], bestDiff = Infinity;
    for (const f of files) {
      const dur = (statSync(join(d, f)).size - 44) / 2 / 48000;
      const diff = Math.abs(dur - slot);
      if (diff < bestDiff) { bestDiff = diff; best = f; }
    }
    pick[g] = best;
  });
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

// per-slot whisper: --whisper (all) or --whisper america,dora
const whisperSlots = { america: false, computer: false, dora: false };
if (flags.whisper === true) for (const g of Object.keys(whisperSlots)) whisperSlots[g] = true;
else if (typeof flags.whisper === "string")
  for (const g of flags.whisper.split(",")) if (g in whisperSlots) whisperSlots[g] = true;

// --fav: a kept combo exported from hooks.html (clips + per-slot whisper)
if (flags.fav) {
  const favs = JSON.parse(readFileSync(expandHome(flags.fav), "utf8"));
  const f = favs[Number(flags["fav-index"] ?? 0)];
  if (!f) { console.error(`✗ no favorite at index ${flags["fav-index"] ?? 0}`); process.exit(1); }
  console.log(`# favorite combo [${f.label}]:`);
  for (const g of Object.keys(whisperSlots)) {
    arrangement[g] = join("utterances", g, f.picks[g].name + ".wav");
    whisperSlots[g] = !!f.picks[g].whisper;
    console.log(`  ${g}: ${f.picks[g].name}${whisperSlots[g] ? " ·whisper" : ""}`);
  }
}

// --pick overrides, last word. comma lists become a roster that rotates
// per hook phrase ("switch voices every utterance").
for (const [g, names] of Object.entries(pickPins)) {
  arrangement[g] = names.map((n) => join("utterances", g, n + ".wav"));
  console.log(`# pinned ${g}: ${names.join(" → ")}`);
}

// normalize every slot to a roster (array of clip paths)
for (const g of Object.keys(whisperSlots)) {
  if (arrangement[g] && !Array.isArray(arrangement[g])) arrangement[g] = [arrangement[g]];
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

// find where the opening consonant ends and the vowel begins — the /k/ of
// "computer" is an unvoiced burst (high zero-crossing rate, modest energy);
// the vowel is loud and periodic. returns the sample index of vowel onset.
function vowelOnset(samples) {
  const frame = Math.floor(0.01 * SR); // 10 ms frames
  const nF = Math.floor(samples.length / frame);
  let peakE = 0;
  const E = new Array(nF), Z = new Array(nF);
  for (let f = 0; f < nF; f++) {
    let e = 0, z = 0;
    for (let i = f * frame + 1; i < (f + 1) * frame; i++) {
      e += samples[i] * samples[i];
      if ((samples[i] >= 0) !== (samples[i - 1] >= 0)) z++;
    }
    E[f] = e / frame; Z[f] = z / frame;
    if (E[f] > peakE) peakE = E[f];
  }
  const maxTrim = Math.floor(nF * 0.4); // never eat more than 40% of the word
  for (let f = 0; f < maxTrim; f++) {
    if (E[f] > peakE * 0.22 && Z[f] < 0.14) return f * frame;
  }
  return 0;
}

// match every vocal clip's loudness: normalize the active region (gate at
// 0.02) to a common RMS so america / computer / dora sit in the same space.
function normalizeRms(s, target = 0.12) {
  let sum = 0, n = 0, peak = 0;
  for (let i = 0; i < s.length; i++) {
    const a = Math.abs(s[i]);
    if (a > peak) peak = a;
    if (a > 0.02) { sum += s[i] * s[i]; n++; }
  }
  if (!n) return s;
  let g = target / Math.sqrt(sum / n);
  if (peak * g > 0.95) g = 0.95 / peak;
  for (let i = 0; i < s.length; i++) s[i] *= g;
  return s;
}

// time-stretch a wav (slower, same pitch) via ffmpeg atempo. factor 1.5 =
// "cawwwmputerrrr" — the word takes 1.5× as long to say.
function stretchWav(absIn, factor) {
  const tmp = join(tmpdir(), `acd-stretch-${process.pid}-${Math.random().toString(36).slice(2, 8)}.wav`);
  const res = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", absIn, "-af", chainAtempo(1 / factor),
    "-ac", "1", "-ar", String(SR), tmp,
  ], { stdio: ["ignore", "ignore", "inherit"] });
  if (res.status !== 0) throw new Error(`stretch failed for ${absIn}`);
  const { samples } = readWavMono(tmp);
  try { unlinkSync(tmp); } catch {}
  return samples;
}

function prepareClip(group, relPath, whisper) {
  const stretch = stretchPins[group];
  if (!relPath) return null;
  const abs = join(ROOT, relPath);
  if (!existsSync(abs)) {
    console.warn(`  ! ${group}: missing ${relPath}, skipping`);
    return null;
  }
  const name = basename(relPath, ".wav");
  const wordIdx = melody.hook.words.indexOf(group);
  if (wordIdx < 0) return null;
  const targets = new Set(melody.hook.variants.map((v) => v.notes[wordIdx]));
  const out = { byMidi: {} };
  let srcMidi = null, hz = null, lazySamples = null;
  for (const t of targets) {
    // snapped library first — pre-autotuned + loudnormed by bin/hooks.mjs
    const snap = join(ROOT, "snapped", group, `${name}__t${t}${whisper ? "__w" : ""}.wav`);
    if (existsSync(snap)) {
      let s = stretch ? stretchWav(snap, stretch) : readWavMono(snap).samples;
      if (decapPins[group]) {
        // detect the consonant on the VOICED twin (whispered audio has no
        // periodicity to read), then scale the cut into stretched time.
        const voiced = join(ROOT, "snapped", group, `${name}__t${t}.wav`);
        const cutSrc = whisper && existsSync(voiced) ? readWavMono(voiced).samples : (stretch ? readWavMono(snap).samples : s);
        let cut = vowelOnset(cutSrc);
        if (stretch) cut = Math.floor(cut * stretch);
        if (cut > 0 && cut < s.length) {
          s = s.slice(cut);
          const fadeN = Math.floor(0.008 * SR);
          for (let i = 0; i < fadeN && i < s.length; i++) s[i] *= i / fadeN;
        }
      }
      out.byMidi[t] = normalizeRms(s);
      continue;
    }
    // fallback: live detect + shift (no whisper available on this path)
    if (lazySamples == null) {
      lazySamples = readWavMono(abs).samples;
      hz = detectPitchHz(lazySamples, SR);
      srcMidi = hz ? hzToMidi(hz) : null;
    }
    if (srcMidi == null) {
      out.byMidi[t] = normalizeRms(lazySamples); // pitch undetected — unity
      continue;
    }
    let semis = t - srcMidi;
    while (semis > 12) semis -= 12;
    while (semis < -12) semis += 12;
    out.byMidi[t] = normalizeRms(pitchShiftWav(abs, semis));
  }
  console.log(`  · ${group}: ${name}${whisper ? " ·whisper" : ""}${stretch ? ` ·stretch×${stretch}` : ""}  (${Object.keys(out.byMidi).length} target notes${lazySamples ? ", live-shifted" : ", snapped lib"})`);
  return out;
}

console.log("\n# preparing hook clips (snapped library / autotune):");
for (const g of ["america", "computer", "dora"]) {
  hookClips[g] = (arrangement[g] || [])
    .map((p) => prepareClip(g, p, whisperSlots[g]))
    .filter(Boolean);
}

// ── total length from structure ────────────────────────────────────────
// reps COUNT: a 4-bar hook section with reps:4 occupies 16 bars. (an old
// version summed bare sec.bars and silently truncated the back half of the
// song — verse 2, bridge, stop-time and final chorus never made the buffer.)
let totalBars = 0;
for (const sec of melody.structure) totalBars += sec.bars * (sec.reps || 1);
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

// serious kick — four-on-the-floor techno weight. long 42 Hz body, hard
// click, tanh drive. the thing the sidechain pumps against.
function kickSerious(buf, t, g = 1.15) {
  const dur = 0.32, n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const tt = i / SR;
    const f = 42 + (190 - 42) * Math.exp(-tt * 30);
    ph += (TAU * f) / SR;
    const body = Math.tanh(Math.sin(ph) * 2.4) * Math.exp(-tt * 7.5);
    const click = i < SR * 0.003 ? (Math.random() * 2 - 1) * 0.6 * (1 - i / (SR * 0.003)) : 0;
    add(buf, s0 + i, (body + click) * g);
  }
}

// pluck — saw+sine through a fast-closing one-pole lowpass. harpsichord pixel.
function pluck(buf, t, midi, dur, g = 0.12) {
  const f = midiToHz(midi);
  const n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  let ph = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const tt = i / SR;
    ph += (TAU * f) / SR;
    const saw = 2 * ((ph / TAU) % 1) - 1;
    const raw = saw * 0.55 + Math.sin(ph) * 0.45;
    const a = 0.12 + 0.55 * Math.exp(-tt * 14); // brightness dies first
    lp += a * (raw - lp);
    add(buf, s0 + i, lp * Math.exp(-tt * 7) * g);
  }
}

// bachiamatrixian arp — 16th-note baroque figuration, the matrix rain.
// chord tones + diatonic ninth sequenced like a two-part invention; the
// sidechain duck carves the pump into it.
function bachArp(buf, barStart, root, quality, { density = 16, octave = 12, g = 0.11 } = {}) {
  const T = quality === "min" ? 3 : 4;
  const steps = density === 16
    ? [0, T, 7, 12, 7, T, 0, T, 7, 12, 14, 12, 7, T, 7, 12]
    : [0, 7, T, 12, 14, 12, 7, T];
  const stepLen = bar / steps.length;
  for (let i = 0; i < steps.length; i++) {
    pluck(buf, barStart + i * stepLen, root + octave + steps[i], stepLen * 1.6, g * (i % 4 === 0 ? 1.15 : 0.85));
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

// sine lead — soft, breathing, slow vibrato. shadows the vocal melody
// without the chiptune edge a square brings.
function sineLead(buf, t, midi, dur, g = 0.11) {
  const f = midiToHz(midi);
  const n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  const att = Math.floor(0.05 * SR), rel = Math.floor(0.12 * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const tt = i / SR;
    const vib = tt > 0.25 ? Math.sin(TAU * 5.2 * tt) * 0.004 * Math.min(1, (tt - 0.25) * 3) : 0;
    ph += (TAU * f * (1 + vib)) / SR;
    let env = 1;
    if (i < att) env = i / att;
    else if (i > n - rel) env = Math.max(0, (n - i) / rel);
    add(buf, s0 + i, Math.sin(ph) * env * g);
  }
}

// snare — noise crack + 190 Hz body. the rock-ballad backbeat.
function snare(buf, t, g = 0.85) {
  const n = Math.floor(0.22 * SR), s0 = Math.floor(t * SR);
  let prev = 0, ph = 0;
  for (let i = 0; i < n; i++) {
    const tt = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    ph += (TAU * 190) / SR;
    const body = Math.sin(ph) * Math.exp(-tt * 30) * 0.7;
    add(buf, s0 + i, (hp * Math.exp(-tt * 24) + body) * g);
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

// supersaw pad — 3 detuned saws per note through a one-pole lowpass. the
// pop glue: holds the chord under the hook, pumped by the sidechain.
function pad(buf, t, midis, dur, g = 0.06) {
  const n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  const att = Math.floor(0.06 * SR), rel = Math.floor(0.12 * SR);
  for (const midi of midis) {
    const f0 = midiToHz(midi);
    for (const det of [-0.006, 0, 0.007]) {
      const f = f0 * (1 + det);
      let ph = Math.random() * TAU, lp = 0;
      for (let i = 0; i < n; i++) {
        ph += (TAU * f) / SR;
        const saw = 2 * ((ph / TAU) % 1) - 1;
        lp += 0.10 * (saw - lp); // dark enough to sit behind the vocal
        let env = 1;
        if (i < att) env = i / att;
        else if (i > n - rel) env = Math.max(0, (n - i) / rel);
        add(buf, s0 + i, lp * env * g);
      }
    }
  }
}

// crash — bright noise wash with a long tail, marks phrase arrivals.
function crash(buf, t, g = 0.22) {
  const dur = 1.4, n = Math.floor(dur * SR), s0 = Math.floor(t * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-(i / SR) * 3.2) * g);
  }
}

// paint with a piecewise gain envelope (clip-relative breakpoints).
// breaks: [{at: sec, to: mult, over: sec}] — cosine ramp from the current
// level to `to` starting at `at`; attack is a fade-in from silence. this is
// how the phrase elides: each word crossfades into the next instead of
// piling up — "america aaaaawwwmputerrrrr dora!".
function paintShaped(buf, sampBuf, startSec, g, breaks, attack = 0) {
  const s0 = Math.floor(startSec * SR);
  let level = attack > 0 ? 0 : 1, bi = 0;
  let from = level, rampStart = 0, rampLen = attack, target = 1;
  for (let i = 0; i < sampBuf.length; i++) {
    const tt = i / SR;
    if (bi < breaks.length && tt >= breaks[bi].at) {
      from = level; target = breaks[bi].to;
      rampStart = tt; rampLen = Math.max(breaks[bi].over, 1e-4);
      bi++;
    }
    if (level !== target) {
      const w = Math.min(1, (tt - rampStart) / rampLen);
      level = from + (target - from) * (0.5 - 0.5 * Math.cos(Math.PI * w));
      if (w >= 1) level = target;
    }
    if (level === 0 && bi >= breaks.length) break; // fully choked, rest is silence
    add(buf, s0 + i, sampBuf[i] * g * level);
  }
}

// ── render ──────────────────────────────────────────────────────────────
const drm = new Float32Array(N);
const snr = new Float32Array(N); // snare on its own bus → reverb send
const bel = new Float32Array(N);
const sqr = new Float32Array(N);
const sb  = new Float32Array(N);
const toy = new Float32Array(N);
const arp = new Float32Array(N);
const pds = new Float32Array(N);
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
let phraseCounter = 0; // rotates the per-slot clip rosters across the song
// total hook reps in the structure — the percussion ramp's denominator
const totalHookReps = melody.structure
  .filter((s) => s.section === "hook")
  .reduce((a, s) => a + (s.reps || 1), 0);
const kickTimes = [];

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
        // verse: thinned — kick on 1 & 3, soft snare backbeat, no tamb
        kick(drm, bt, 0.75); kickTimes.push({ t: bt });
        kick(drm, bt + beat * 2, 0.75); kickTimes.push({ t: bt + beat * 2 });
        snare(snr, bt + beat, 0.5);
        snare(snr, bt + beat * 3, 0.5);
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
        // bridge: the matrix breakdown — serious kick on the downbeat, sub,
        // and the bach arp alone in 8ths an octave down. green rain.
        kickSerious(drm, bt, 0.9); kickTimes.push({ t: bt, serious: true });
        bachArp(arp, bt, root, ch.quality, { density: 8, octave: 0, g: 0.15 });
      } else {
        // hook — percussion RAMPS across the song. the first chorus is the
        // minimal verse kit; each chorus gains weight until the finale hits
        // the full rock backbeat (and the very last rep goes four-on-the-
        // floor). tIns: 0 → 1 across all hook reps in the structure.
        const tIns = totalHookReps > 1 ? hookCount / (totalHookReps - 1) : 1;
        if (tIns < 0.34) {
          // chorus 1 — minimal: soft kick 1 & 3, quiet snare, no hats
          kick(drm, bt, 0.8); kickTimes.push({ t: bt });
          kick(drm, bt + beat * 2, 0.8); kickTimes.push({ t: bt + beat * 2 });
          snare(snr, bt + beat, 0.55);
          snare(snr, bt + beat * 3, 0.55);
        } else if (tIns < 0.67) {
          // chorus 2 — the pump arrives: serious kicks, fuller snare
          kickSerious(drm, bt, 0.95); kickTimes.push({ t: bt, serious: true });
          kickSerious(drm, bt + beat * 2, 0.95); kickTimes.push({ t: bt + beat * 2, serious: true });
          snare(snr, bt + beat, 0.8);
          snare(snr, bt + beat * 3, 0.8);
          for (let i = 0; i < 8; i++) tamb(drm, bt + i * (beat / 2), 0.04);
        } else {
          // finale — full rock backbeat; last rep goes four-on-the-floor
          const floor = tIns > 0.95;
          for (let i = 0; i < 4; i++) {
            if (i % 2 === 0 || floor) {
              kickSerious(drm, bt + beat * i, floor ? 1.1 : 1.05);
              kickTimes.push({ t: bt + beat * i, serious: true });
            }
          }
          snare(snr, bt + beat, 0.95);
          snare(snr, bt + beat * 3, 0.95);
          for (let i = 0; i < 8; i++) tamb(drm, bt + i * (beat / 2), 0.055);
        }
        // bell / arp / pad swell with the ramp
        bell(bel, bt, root + 12, bar * 0.95, 0.10 + 0.05 * tIns);
        bachArp(arp, bt, root, ch.quality, { density: 16, octave: 12, g: 0.07 + 0.04 * tIns });
        pad(pds, bt, [root - 12, root, third, fifth], bar * 1.02, 0.05 + 0.02 * tIns);
      }
    }
    // hook reps: pick a melody variant and paint the syllables + square lead
    if (sec.section === "hook") {
      const variant = melody.hook.variants[hookCount % melody.hook.variants.length];
      const words = melody.hook.words;
      const lens = melody.hook.beats_per_word;
      // the phrase spans phraseBars bars (8 beats = 2 bars) so the words
      // actually get to speak — a 4-bar hook section plays it twice.
      const phraseBeats = lens.reduce((a, c) => a + c, 0);
      const phraseBars = Math.max(1, Math.round(phraseBeats / 4));
      const phraseLen = phraseBeats * beat;
      for (let p = 0; p + phraseBars <= sec.bars; p += phraseBars) {
        const phraseStart = (barCursor + p) * bar;
        const phraseIdx = phraseCounter++;
        crash(drm, phraseStart, p === 0 ? 0.26 : 0.16);
        // word onsets within the phrase
        const onsets = [0];
        for (let w = 0; w < words.length - 1; w++) onsets.push(onsets[w] + lens[w] * beat);
        for (let w = 0; w < words.length; w++) {
          const word = words[w];
          const noteLen = lens[w] * beat;
          const targetMidi = variant.notes[w];
          // layered merge — every word FINISHES its word: when the next one
          // enters it ducks underneath (still audibly singing), and is only
          // choked one word later. "america" completes under "computer",
          // "computer" rolls under "dora!", dora rings past the phrase end.
          // a roster of clips per slot rotates voice per phrase.
          const roster = hookClips[word];
          const clip = roster && roster.length ? roster[phraseIdx % roster.length] : null;
          if (clip && clip.byMidi && clip.byMidi[targetMidi]) {
            const last = w === words.length - 1;
            // BLEND: the middle word blooms in 0.3 s EARLY, swelling from
            // inside america's open vowel while america eases down slowly —
            // "americaaaaa" and "ahmputer" morph instead of trading places.
            const BLEND = 0.30;
            const lead = !last && w > 0 ? BLEND : 0;
            let breaks, attack, gain;
            if (last) {
              // dora — the payoff: sharp attack, hot, rings over the line
              breaks = [{ at: phraseLen - onsets[w] + 0.06, to: 0, over: 0.22 }];
              attack = 0.008; gain = 1.15;
            } else {
              const duckAt = onsets[w + 1] - onsets[w] + lead;
              const chokeAt = (w + 2 < words.length ? onsets[w + 2] : phraseLen) - onsets[w] + lead;
              breaks = w === 0
                ? [
                    { at: duckAt - BLEND, to: 0.45, over: 0.40 }, // ease down through the morph
                    { at: chokeAt - 0.06, to: 0, over: 0.30 },
                  ]
                : [
                    { at: duckAt - 0.06, to: 0.30, over: 0.20 },
                    { at: chokeAt - 0.06, to: 0, over: 0.22 },
                  ];
              attack = w === 0 ? 0 : BLEND; gain = 0.95;
            }
            paintShaped(voc, clip.byMidi[targetMidi], phraseStart + onsets[w] - lead, gain, breaks, attack);
          }
          // sine lead shadows the melody an octave up — soft, not chippy
          sineLead(sqr, phraseStart + onsets[w], targetMidi + 12, noteLen * 0.95, 0.10);
        }
      }
      hookCount++;
    }
    barCursor += sec.bars;
  }
}

// ── sidechain duck under each kick ──────────────────────────────────────
// verse kicks duck lightly (the old polite 0.78); serious four-on-the-floor
// kicks slam to 0.22 and recover over most of the beat — the pump.
const duck = new Float32Array(N).fill(1);
for (const k of kickTimes) {
  const depth = k.serious ? 0.32 : 0.78; // ballad breath, not techno slam
  const len = Math.floor((k.serious ? beat * 0.85 : 0.10) * SR);
  const s0 = Math.floor(k.t * SR);
  for (let i = 0; i < len && s0 + i < N; i++) {
    const w = i / len;
    const d = depth + (1 - depth) * (0.5 - 0.5 * Math.cos(Math.PI * w)); // cosine recovery
    if (d < duck[s0 + i]) duck[s0 + i] = d;
  }
}

// ── fade-out across the outro (last 8 bars — long ballad fade) ─────────
const fadeStart = (totalBars - 8) * bar;
const fadeStartSamp = Math.floor(fadeStart * SR);
const fadeMaster = new Float32Array(N).fill(1);
for (let i = fadeStartSamp; i < N; i++) {
  fadeMaster[i] = Math.max(0, 1 - (i - fadeStartSamp) / (N - fadeStartSamp));
}

// ── reverb: schroeder combs + allpass, the room everything sits in ─────
function reverberate(send, decay = 0.62) {
  const wet = new Float32Array(N);
  const combs = [0.0297, 0.0371, 0.0411, 0.0437].map((d) => ({
    buf: new Float32Array(Math.floor(d * SR)), i: 0,
  }));
  for (let i = 0; i < N; i++) {
    let s = 0;
    for (const c of combs) {
      const y = c.buf[c.i];
      c.buf[c.i] = send[i] + y * decay;
      c.i = (c.i + 1) % c.buf.length;
      s += y;
    }
    wet[i] = s * 0.25;
  }
  // two allpass stages diffuse the comb ringing into a wash
  for (const d of [0.005, 0.0017]) {
    const ap = new Float32Array(Math.floor(d * SR));
    let ai = 0;
    for (let i = 0; i < N; i++) {
      const y = ap[ai];
      const x = wet[i];
      ap[ai] = x + y * 0.5;
      wet[i] = y - x * 0.5;
      ai = (ai + 1) % ap.length;
    }
  }
  return wet;
}

// ── vocal glue: tempo-synced dotted-8th feedback delay ─────────────────
// the echo tail tucks the words into the groove instead of sitting dry on
// top — the cheapest "vocals and instruments blend" move in pop.
const dSamp = Math.floor(beat * 0.75 * SR);
const echo = new Float32Array(N);
for (let i = 0; i < N; i++) {
  echo[i] = voc[i] + (i >= dSamp ? echo[i - dSamp] * 0.40 : 0);
}

// ── reverb send: vocals, snare, bells, lead, arps go to the room ───────
const bedOnly = !!flags["bed-only"];
const send = new Float32Array(N);
for (let i = 0; i < N; i++) {
  send[i] = (bedOnly ? 0 : voc[i] * 0.85) + snr[i] * 0.65 + bel[i] * 0.55 + sqr[i] * 0.5 + arp[i] * 0.3;
}
const room = reverberate(send);

// ── master ─────────────────────────────────────────────────────────────
const mix = new Float32Array(N);
for (let i = 0; i < N; i++) {
  const drumsBus = (drm[i] + snr[i]) * 0.92;
  // arps + bells + leads + pad take the full pump; sub and vocals duck
  // partially so the kick owns the low end but the words stay legible.
  const melodicBus = (bel[i] + sqr[i] + toy[i] + arp[i] + pds[i]) * duck[i];
  const subBus = sb[i] * (0.55 + 0.45 * duck[i]);
  const vocalDry = voc[i];
  const vocalTail = (echo[i] - voc[i]) * 0.28; // pure delay tail, ducked with the band
  const vocalBus = bedOnly ? 0 : (vocalDry * (0.75 + 0.25 * duck[i]) + vocalTail * duck[i]);
  const roomBus = room[i] * 0.55 * (0.7 + 0.3 * duck[i]); // the space breathes a little
  const s = drumsBus + melodicBus + subBus + vocalBus * 0.85 + roomBus;
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
