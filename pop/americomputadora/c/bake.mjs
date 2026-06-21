#!/usr/bin/env node
// bake.mjs — prepare everything americomputadora.c needs (the hellsine
// pattern: JS bakes the score + samples, C renders the song).
//
//   1. vocal prep: for every (word, roster clip, melody variant) pull the
//      snapped wav (whisper twin where the recipe says so), time-stretch,
//      decap, RMS-match — the same chain render.mjs runs — and write a
//      float32 mono 48k wav into c/vocals/.
//   2. score: melody.json structure / chords / variants + the vocal file
//      table → score.h.
//
// usage: node c/bake.mjs        (canonical recipe is the default)

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync, unlinkSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

import { readWavMono } from "../../lib/wav.mjs";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);
const melody = JSON.parse(readFileSync(join(ROOT, "melody.json"), "utf8"));

// ── the canonical recipe (mirror of the render.mjs invocation) ─────────
// one sample per word — the three choices. (rosters still rotate per
// phrase if you list more than one, but jeffrey prefers the fixed trio.)
// hold: loop the post-decap "um" region so the word says "ummputer".
const RECIPE = {
  america: {
    roster: ["whitney-houston-w1"],
    stretch: 1.35, whisper: false, decap: false, hold: false,
    // the FULL sample, no trims, no loops — the engine's envelope just
    // fades the aaaa out into dora. (firstPhrase/sustainVowel retired for
    // america by jeffrey's call: "i want the full americaaaaa".)
  },
  computer: {
    roster: ["samantha-slow"],
    stretch: 1.4, whisper: true, decap: false, hold: true, sustainEnd: true,
    // decap off: the /k/ stays — "KAHM-puter". the hold loops the vowel
    // after the /k/ ("kaaahm"). sustainEnd loops the FINAL vowel ("errrr")
    // so the word rings continuously up to dora — no vocal gap.
  },
  dora: {
    roster: ["theme-song-w1"],
    stretch: 1, whisper: false, decap: false, hold: false, graftHead: true,
  },
};

// granular hold: repeat the [aSec, bSec] region with short crossfades —
// duration grows, pitch and formants don't. "omputer" → "ummmputer".
function holdUm(s, aSec = 0.03, bSec = 0.13, reps = 2, fadeSec = 0.012) {
  const a = Math.floor(aSec * SR), b = Math.floor(bSec * SR), F = Math.floor(fadeSec * SR);
  if (s.length < b + F || a < F) return s;
  const seg = s.subarray(a, b);
  const chunks = [s.subarray(0, b)];
  for (let r = 0; r < reps; r++) chunks.push(seg);
  chunks.push(s.subarray(b));
  const total = chunks.reduce((n, c) => n + c.length, 0) - F * (chunks.length - 1);
  const out = new Float32Array(total);
  let pos = 0;
  chunks.forEach((c, idx) => {
    if (idx === 0) { out.set(c, 0); pos = c.length; return; }
    for (let i = 0; i < F; i++) {
      const w = i / F;
      out[pos - F + i] = out[pos - F + i] * (1 - w) + c[i] * w;
    }
    out.set(c.subarray(F), pos);
    pos += c.length - F;
  });
  return out;
}

// graft the raw clip's head onto a tuned clip — WORLD smears plosive
// transients (dora's /d/ took 100ms to rise instead of 20ms), so the
// first ~70ms come straight from the untouched sample.
function graftHead(tuned, raw, headSec = 0.07, fadeSec = 0.015) {
  const H = Math.floor(headSec * SR), F = Math.floor(fadeSec * SR);
  if (raw.length < H + F || tuned.length < H + F) return tuned;
  const out = Float32Array.from(tuned);
  for (let i = 0; i < H; i++) out[i] = raw[i];
  for (let i = 0; i < F; i++) {
    const w = i / F;
    out[H + i] = raw[H + i] * (1 - w) + tuned[H + i] * w;
  }
  return out;
}

// sustain the final vowel: loop the region just before the sung word's
// energy dies until the clip reaches targetSec, then let the natural tail
// decay. fixes the hole the waveform analysis found — whitney's "aaa"
// ended ~1.9s but computer doesn't bloom until ~2.1s.
function sustainVowel(s, targetSec, loopSec = 0.14, fadeSec = 0.015) {
  const target = Math.floor(targetSec * SR);
  if (s.length >= target) {
    // long enough on paper — but check the ENERGY reaches the junction
  }
  const W = Math.floor(0.01 * SR);
  const nF = Math.floor(s.length / W);
  const rms = [];
  let peak = 0;
  for (let f = 0; f < nF; f++) {
    let e = 0;
    for (let i = f * W; i < (f + 1) * W; i++) e += s[i] * s[i];
    const v = Math.sqrt(e / W);
    rms.push(v); peak = Math.max(peak, v);
  }
  let endF = nF - 1;
  while (endF > 0 && rms[endF] < peak * 0.3) endF--;
  const vowelEnd = endF * W;
  if (vowelEnd >= target) return s; // energy already covers the slot
  const L = Math.floor(loopSec * SR), F = Math.floor(fadeSec * SR);
  if (vowelEnd < L + F) return s;
  const segA = vowelEnd - L, segB = vowelEnd;
  const seg = s.subarray(segA, segB);
  const chunks = [s.subarray(0, segB)];
  let have = segB;
  const tail = s.length - segB; // the word's natural ending ("...ca") rides AFTER the loops
  while (have < target - tail) { chunks.push(seg); have += seg.length - F; }
  chunks.push(s.subarray(segB)); // natural decay tail — the ca lands before dora
  const total = chunks.reduce((n, c) => n + c.length, 0) - F * (chunks.length - 1);
  const out = new Float32Array(total);
  let pos = 0;
  chunks.forEach((c, idx) => {
    if (idx === 0) { out.set(c, 0); pos = c.length; return; }
    for (let i = 0; i < F; i++) {
      const w = i / F;
      out[pos - F + i] = out[pos - F + i] * (1 - w) + c[i] * w;
    }
    out.set(c.subarray(F), pos);
    pos += c.length - F;
  });
  return out;
}

// keep only the first sung phrase: find the first sustained internal
// silence (>=120ms under 12% of peak, after minSec) and cut there with a
// 30ms fade. whitney-w1 sings the word twice; we want one.
function firstPhrase(s, minSec = 0.8, gapSec = 0.10, thresh = 0.22) {
  const W = Math.floor(0.01 * SR);
  const nF = Math.floor(s.length / W);
  const rms = []; let peak = 0;
  for (let f = 0; f < nF; f++) {
    let e = 0;
    for (let i = f * W; i < (f + 1) * W; i++) e += s[i] * s[i];
    const v = Math.sqrt(e / W);
    rms.push(v); peak = Math.max(peak, v);
  }
  const need = Math.round(gapSec / 0.01);
  let run = 0;
  for (let f = Math.floor(minSec / 0.01); f < nF; f++) {
    if (rms[f] < peak * thresh) {
      if (++run >= need) {
        const cut = (f - run + 1) * W;
        const out = Float32Array.from(s.subarray(0, cut));
        const F = Math.floor(0.03 * SR);
        for (let i = 0; i < F && cut - 1 - i >= 0; i++) out[cut - 1 - i] *= i / F;
        return out;
      }
    } else run = 0;
  }
  return s;
}

// "putaa": locate the final /t/ closure (last >=30ms energy gap in the
// back 45% of the word), keep the fresh vowel right after it, loop it with
// decaying gain, fade — the rhotic "er" tail never happens.
function kahmTail(s) {
  const W = Math.floor(0.01 * SR);
  const nF = Math.floor(s.length / W);
  const rms = []; let peak = 0;
  for (let f = 0; f < nF; f++) {
    let e = 0;
    for (let i = f * W; i < (f + 1) * W; i++) e += s[i] * s[i];
    const v = Math.sqrt(e / W);
    rms.push(v); peak = Math.max(peak, v);
  }
  let closureEnd = -1, run = 0;
  for (let f = Math.floor(nF * 0.55); f < nF; f++) {
    if (rms[f] < peak * 0.22) run++;
    else { if (run >= 3) closureEnd = f; run = 0; }
  }
  if (closureEnd < 0) return s;
  const vStart = closureEnd * W;
  const keep = Math.min(s.length, vStart + Math.floor(0.10 * SR));
  const F = Math.floor(0.012 * SR);
  const segA = Math.min(vStart + Math.floor(0.015 * SR), keep - F - 1);
  if (segA <= 0 || keep - segA < F * 2) return s;
  const seg = s.subarray(segA, keep);
  const reps = 1, gains = [0.85]; // single short "a" — "kahmputa", crisp
  const total = keep + (seg.length - F) * reps;
  const out = new Float32Array(total);
  out.set(s.subarray(0, keep), 0);
  let pos = keep;
  for (let r = 0; r < reps; r++) {
    const g = gains[r];
    for (let i = 0; i < F; i++) {
      const w = i / F;
      out[pos - F + i] = out[pos - F + i] * (1 - w) + seg[i] * g * w;
    }
    for (let i = F; i < seg.length; i++) out[pos + i - F] = seg[i] * g;
    pos += seg.length - F;
  }
  const FF = Math.floor(0.10 * SR);
  for (let i = 0; i < FF && i < pos; i++) out[pos - 1 - i] *= i / FF;
  return out.subarray(0, pos);
}

// ── processing helpers (same math as render.mjs) ────────────────────────
function chainAtempo(target) {
  const stages = [];
  let r = target;
  while (r > 2.0) { stages.push(2.0); r /= 2.0; }
  while (r < 0.5) { stages.push(0.5); r /= 0.5; }
  stages.push(+r.toFixed(6));
  return stages.map((s) => `atempo=${s}`).join(",");
}
function stretchWav(absIn, factor) {
  const tmp = join(tmpdir(), `acd-bake-${process.pid}-${Math.random().toString(36).slice(2, 8)}.wav`);
  const res = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", absIn, "-af", chainAtempo(1 / factor), "-ac", "1", "-ar", String(SR), tmp],
    { stdio: ["ignore", "ignore", "inherit"] });
  if (res.status !== 0) throw new Error(`stretch failed: ${absIn}`);
  const { samples } = readWavMono(tmp);
  try { unlinkSync(tmp); } catch {}
  return samples;
}
function vowelOnset(samples) {
  const frame = Math.floor(0.01 * SR);
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
  for (let f = 0; f < Math.floor(nF * 0.4); f++) {
    if (E[f] > peakE * 0.22 && Z[f] < 0.14) return f * frame;
  }
  return 0;
}
function detectPitchHz(samples) {
  const n = samples.length;
  const seg = samples.subarray(Math.floor(n * 0.2), Math.floor(n * 0.8));
  const minLag = Math.floor(SR / 600), maxLag = Math.floor(SR / 80);
  let bestLag = 0, bestScore = -Infinity;
  for (let lag = minLag; lag <= maxLag; lag++) {
    let r = 0, e0 = 0, e1 = 0;
    for (let i = 0; i + lag < seg.length; i++) {
      r += seg[i] * seg[i + lag];
      e0 += seg[i] * seg[i];
      e1 += seg[i + lag] * seg[i + lag];
    }
    const score = r / (Math.sqrt(e0 * e1) || 1);
    if (score > bestScore) { bestScore = score; bestLag = lag; }
  }
  return bestLag ? SR / bestLag : null;
}

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
function writeWavF32(path, samples) {
  const n = samples.length;
  const b = Buffer.alloc(44 + n * 4);
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 4, 4); b.write("WAVE", 8);
  b.write("fmt ", 12); b.writeUInt32LE(16, 16);
  b.writeUInt16LE(3, 20);            // IEEE float
  b.writeUInt16LE(1, 22);            // mono
  b.writeUInt32LE(SR, 24);
  b.writeUInt32LE(SR * 4, 28);
  b.writeUInt16LE(4, 32); b.writeUInt16LE(32, 34);
  b.write("data", 36); b.writeUInt32LE(n * 4, 40);
  for (let i = 0; i < n; i++) b.writeFloatLE(samples[i], 44 + i * 4);
  writeFileSync(path, b);
}

// ── bake vocals — three treatment tiers, ramping with the song ─────────
//   raw   (chorus 1): the utterance itself, RMS-matched only. "by the
//                     book" — no pitch-snap, no stretch, no whisper.
//   plain (chorus 2): autotuned to the variant note (snapped lib, voiced),
//                     still unstretched and un-whispered.
//   full  (finale):   the whole canonical treatment.
const vocDir = join(HERE, "vocals");
mkdirSync(vocDir, { recursive: true });
const words = melody.hook.words;
const variants = melody.hook.variants;
const rawTable = [];   // [word][roster] → path
const plainTable = []; // [word][roster][variant] → path
const fullTable = [];  // [word][roster][variant] → path

console.log("# baking vocals → c/vocals/ (raw / plain / full tiers)");
words.forEach((word, w) => {
  const r = RECIPE[word];
  rawTable[w] = []; plainTable[w] = []; fullTable[w] = [];
  r.roster.forEach((name, ri) => {
    // decap applies on EVERY tier — never "computer". cut 25 ms INTO the
    // voiced onset so no burst residue survives, then hold the "um".
    const decapped = (s, voicedSamples, scale = 1) => {
      if (!r.decap) return s;
      let cut = vowelOnset(voicedSamples);
      if (cut > 0) cut += Math.floor(0.025 * SR);
      cut = Math.floor(cut * scale);
      if (cut > 0 && cut < s.length) {
        s = s.slice(cut);
        const fadeN = Math.floor(0.008 * SR);
        for (let i = 0; i < fadeN && i < s.length; i++) s[i] *= i / fadeN;
      }
      return s;
    };
    // hold the vowel after whatever consonant survives: locate the voiced
    // onset (scaled into stretched time) and loop just past it.
    const held = (s, voicedSamples, scale = 1) => {
      if (!r.hold) return s;
      const on = r.decap ? 0 : (vowelOnset(voicedSamples) * scale) / SR;
      return holdUm(s, on + 0.02, on + 0.12);
    };

    // sustain targets (seconds):
    //  - sustainTo "junction": america rings until computer (slot+next slot)
    //  - sustainEnd: computer's final vowel rings to dora's onset — its own
    //    slot plus a small overlap so there's NO gap before dora.
    const beat = 60 / melody.bpm;
    const sustainTarget = r.sustainTo === "junction"
      ? (melody.hook.beats_per_word[w] + melody.hook.beats_per_word[w + 1]) * beat - 0.05
      : r.sustainEnd
      ? melody.hook.beats_per_word[w] * beat + 0.22
      : 0;
    const sustained = (s) => {
      if (r.firstWordOnly) s = firstPhrase(s);
      return sustainTarget ? sustainVowel(s, sustainTarget) : s;
    };

    // raw tier — straight from utterances/ (plus decap)
    const utt = join(ROOT, "utterances", word, `${name}.wav`);
    if (!existsSync(utt)) throw new Error(`missing utterance: ${utt}`);
    const uttS = Float32Array.from(readWavMono(utt).samples);
    let rawS = held(decapped(uttS, uttS), uttS);
    if (r.tailAa) rawS = kahmTail(rawS);
    const raw = normalizeRms(sustained(rawS));
    rawTable[w][ri] = `vocals/${word}-${ri}-raw.wav`;
    writeWavF32(join(HERE, rawTable[w][ri]), raw);

    plainTable[w][ri] = []; fullTable[w][ri] = [];
    variants.forEach((v, vi) => {
      const t = v.notes[w];
      const voiced = join(ROOT, "snapped", word, `${name}__t${t}.wav`);
      const snap = join(ROOT, "snapped", word, `${name}__t${t}${r.whisper ? "__w" : ""}.wav`);
      if (!existsSync(snap) || !existsSync(voiced)) throw new Error(`missing snap: ${snap}`);

      // plain tier — tuned + decap (+ raw transient graft), nothing else
      const voicedS = Float32Array.from(readWavMono(voiced).samples);
      let plainS = held(decapped(Float32Array.from(voicedS), voicedS), voicedS);
      if (r.tailAa) plainS = kahmTail(plainS);
      if (r.graftHead) plainS = graftHead(plainS, uttS);
      const plain = normalizeRms(sustained(plainS));
      plainTable[w][ri][vi] = `vocals/${word}-${ri}-v${vi}-plain.wav`;
      writeWavF32(join(HERE, plainTable[w][ri][vi]), plain);

      // full tier — stretch + decap + whisper, the canonical character
      let s = r.stretch !== 1 ? stretchWav(snap, r.stretch) : Float32Array.from(readWavMono(snap).samples);
      const sc = r.stretch !== 1 ? r.stretch : 1;
      s = held(decapped(s, voicedS, sc), voicedS, sc);
      if (r.tailAa) s = kahmTail(s);
      if (r.graftHead && r.stretch === 1) s = graftHead(s, uttS);
      s = sustained(s);
      normalizeRms(s);
      fullTable[w][ri][vi] = `vocals/${word}-${ri}-v${vi}.wav`;
      writeWavF32(join(HERE, fullTable[w][ri][vi]), s);
    });
    console.log(`  ✓ ${word}/${name}  raw + ${variants.length}×2 variants`);
  });
});

// ── syllable chant: 'a me ri com pu ta do ra' (the literal phonetics of
// americomputadora — eight syllables, grouped [4,2,2]), two voices (jeffrey
// + a computer voice), pitched to every verse-melody degree. KEY: we analyze
// each syllable's ACTUAL base pitch and choose the octave of the target note
// NEAREST that base — so jeffrey is nudged only a few semitones, never hauled
// across an octave. retain<1 keeps his natural inflection. each syllable is
// stretched a touch for legato.
const SYL_NAMES = ["a", "me", "ri", "com", "pu", "ta", "do", "ra"];
const sylDir = join(HERE, "syl-cache");
let sylNotes = [], sylPaths = [], sylPathsC = [];
const NOTE_NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const midiNameOf = (m) => NOTE_NAMES[m % 12] + (Math.floor(m / 12) - 1);
const SYL_STRETCH = 1.18; // a bit longer — legato chant

// LIVE jeffrey takes from the wave-wizard win over the ElevenLabs clips:
// wave-wizard/samples/americomputadora/takes/americomputadora-chant-note-N.wav
// (N = 0..6, same order as SYL_NAMES). copy any present take into syl-cache
// as the jeffrey source so the rest of the pipeline is unchanged.
// wave-wizard live takes are DISABLED (jeffrey's recorded vocals removed).
// flip USE_WIZARD back to true to bring them in again.
const USE_WIZARD = false;
const wizTakes = join(ROOT, "..", "..", "wave-wizard", "samples", "americomputadora", "takes");
let nWiz = 0;
if (USE_WIZARD) {
  SYL_NAMES.forEach((n, si) => {
    const take = join(wizTakes, `americomputadora-chant-note-${si}.wav`);
    if (existsSync(take)) {
      spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", take,
        "-ac", "1", "-ar", String(SR), join(sylDir, n + ".wav")],
        { stdio: ["ignore", "ignore", "inherit"] });
      nWiz++;
    }
  });
  if (nWiz) console.log(`# wave-wizard: ${nWiz}/${SYL_NAMES.length} live jeffrey takes → syl-cache`);
}

if (SYL_NAMES.every((n) => existsSync(join(sylDir, n + ".wav")))) {
  const degrees = new Set();
  for (const c of melody.bed.chord_progression) {
    const root = c.root_midi, min = c.quality === "min";
    degrees.add(root);                 // the harmonic opener chants the root too
    degrees.add(root + (min ? 3 : 4)); // third
    degrees.add(root + 7);             // fifth
    degrees.add(root + 12);            // octave
  }
  sylNotes = [...degrees].sort((a, b) => a - b);
  const PY = join(ROOT, "..", ".venv", "bin", "python3");
  const PITCHSNAP = join(ROOT, "..", "bin", "pitchsnap_world.py");

  // tune one voice's syllable to the octave of `t` nearest its base pitch.
  const tuneOne = (src, rel, t, baseMidi) => {
    const outAbs = join(HERE, rel);
    // stretch first (atempo, pitch unchanged), then loudnorm
    spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-i", src, "-af", `${chainAtempo(1 / SYL_STRETCH)},loudnorm=I=-15:TP=-1.2`,
      "-ac", "1", "-ar", String(SR), outAbs], { stdio: ["ignore", "ignore", "inherit"] });
    // nearest octave of the target pitch-class to his actual base
    let tEff = t;
    if (baseMidi != null) {
      while (tEff - baseMidi > 6) tEff -= 12;
      while (tEff - baseMidi < -6) tEff += 12;
    } else { while (tEff > 60) tEff -= 12; while (tEff < 48) tEff += 12; }
    // retain 0.7 — lands on the tone but keeps jeffrey's own movement
    const tune = spawnSync(PY, [PITCHSNAP, outAbs, outAbs, "--notes", midiNameOf(tEff),
      "--retain", "0.7", "--xfade-ms", "20"], { stdio: ["ignore", "ignore", "inherit"] });
    if (tune.status !== 0) console.warn(`  ! WORLD tune failed: ${rel} → ${midiNameOf(tEff)}`);
  };

  SYL_NAMES.forEach((n, si) => {
    const jSrc = join(sylDir, n + ".wav");
    const cSrc = join(sylDir, `c-${n}.wav`);
    const jBase = (() => { const hz = detectPitchHz(readWavMono(jSrc).samples); return hz ? 69 + 12 * Math.log2(hz / 440) : null; })();
    const cBase = existsSync(cSrc) ? (() => { const hz = detectPitchHz(readWavMono(cSrc).samples); return hz ? 69 + 12 * Math.log2(hz / 440) : null; })() : null;
    sylPaths[si] = []; sylPathsC[si] = [];
    sylNotes.forEach((t, ti) => {
      const jRel = `vocals/syl-${n}-t${t}.wav`;
      tuneOne(jSrc, jRel, t, jBase);
      sylPaths[si][ti] = jRel;
      if (existsSync(cSrc)) {
        const cRel = `vocals/sylc-${n}-t${t}.wav`;
        tuneOne(cSrc, cRel, t, cBase);
        sylPathsC[si][ti] = cRel;
      } else sylPathsC[si][ti] = jRel;
    });
    console.log(`  · syl ${n}: jeffrey base ${jBase ? midiNameOf(Math.round(jBase)) : "?"}${cBase ? ` · computer base ${midiNameOf(Math.round(cBase))}` : ""}`);
  });
  console.log(`✓ syllable chant: ${SYL_NAMES.length} syls × ${sylNotes.length} notes × 2 voices`);
} else {
  console.log("· no syl-cache — run node c/syllables.mjs for the chant layer");
}

// ── score.h ──────────────────────────────────────────────────────────────
const prog = melody.bed.chord_progression;
const h = `// score.h — generated by c/bake.mjs from melody.json. do not hand-edit.
#define SCORE_BPM ${melody.bpm}
#define N_WORDS ${words.length}
#define N_VARIANTS ${variants.length}
#define MAX_ROSTER ${Math.max(...words.map((w) => RECIPE[w].roster.length))}

static const int BPW[N_WORDS] = {${melody.hook.beats_per_word.join(", ")}};
static const int VARIANT_NOTES[N_VARIANTS][N_WORDS] = {
${variants.map((v) => `  {${v.notes.join(", ")}}, // ${v.label}`).join("\n")}
};

typedef struct { const char *section; int bars; int reps; const char *feel; } Sec;
static const Sec STRUCTURE[] = {
${melody.structure.map((s) => `  {"${s.section}", ${s.bars}, ${s.reps || 1}, "${s.feel}"},`).join("\n")}
};
#define N_SECS ${melody.structure.length}

typedef struct { int bars; int root; int minor; } ChordDef;
static const ChordDef PROG[] = {
${prog.map((c) => `  {${c.bars}, ${c.root_midi}, ${c.quality === "min" ? 1 : 0}},`).join("\n")}
};
#define N_PROG ${prog.length}
#define PROG_BARS ${prog.reduce((a, c) => a + c.bars, 0)}

static const int ROSTER_N[N_WORDS] = {${words.map((w) => RECIPE[w].roster.length).join(", ")}};
// raw tier — the utterance itself, untouched ("by the book" stitch)
static const char *VOC_RAW[N_WORDS][MAX_ROSTER] = {
${rawTable.map((byRoster) => "  {" + byRoster.map((p) => `"${p}"`).join(", ") + "},").join("\n")}
};
// plain tier — autotuned to the variant note, nothing else
static const char *VOC_PLAIN[N_WORDS][MAX_ROSTER][N_VARIANTS] = {
${plainTable.map((byRoster) =>
  "  {\n" + byRoster.map((byVar) =>
    "    {" + byVar.map((p) => `"${p}"`).join(", ") + "},"
  ).join("\n") + "\n  },"
).join("\n")}
};
// full tier — stretch + decap + whisper, the canonical character
static const char *VOC_FULL[N_WORDS][MAX_ROSTER][N_VARIANTS] = {
${fullTable.map((byRoster) =>
  "  {\n" + byRoster.map((byVar) =>
    "    {" + byVar.map((p) => `"${p}"`).join(", ") + "},"
  ).join("\n") + "\n  },"
).join("\n")}
};

// jeffrey syllable chant (verse layer)
#define N_SYL ${sylPaths.length}
#define N_SYLT ${Math.max(1, sylNotes.length)}
static const int SYL_NOTES[N_SYLT] = {${sylNotes.length ? sylNotes.join(", ") : 0}};
static const char *SYL_PATHS[N_SYL ? N_SYL : 1][N_SYLT] = {
${sylPaths.length
  ? sylPaths.map((row) => "  {" + row.map((p) => `"${p}"`).join(", ") + "},").join("\n")
  : '  {""},'}
};
// second voice (computer) chanting the same syllables, same notes
static const char *SYL_PATHS_C[N_SYL ? N_SYL : 1][N_SYLT] = {
${sylPathsC.length
  ? sylPathsC.map((row) => "  {" + row.map((p) => `"${p}"`).join(", ") + "},").join("\n")
  : '  {""},'}
};
`;
writeFileSync(join(HERE, "score.h"), h);
console.log(`✓ c/score.h (${variants.length} variants · structure ${melody.structure.length} sections)`);
