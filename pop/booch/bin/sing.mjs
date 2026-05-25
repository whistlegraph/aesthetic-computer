#!/usr/bin/env node
// sing.mjs — booch · "visualize my booch" rolled across the bed using
// macOS `say` TTS voices, treated like an instrument: elongated, cut,
// reversed, pitched, panned, harmonized into stacks.
//
// NO ElevenLabs. NO jeffrey-pvc. Pure Apple system voices as a phonetic
// kernel that gets sampled / chopped / repitched all over the 103.6s
// bed. The phrase is the song. (Old ElevenLabs pipeline parked at
// sing.elevenlabs.mjs.bak for reference.)
//
// Pipeline:
//   1. base — for each voice × phrase variant, `say -v V -r R <phrase>`
//             → AIFF → 48k mono WAV → cached Float32Array
//   2. derive — for each (voice, phrase, pitchSemi, stretch, reverse)
//             event variant, run rubberband + reverse via ffmpeg, cache
//   3. place — assemble a stereo vocal bus by blitting each scheduled
//             event into voxL/voxR with pan, gain, fades
//   4. mix  — sidechain-duck the bed under the vocals, mix, master
//             via loudnorm → mp3
//
//   node pop/booch/bin/sing.mjs              # → out/visualize-my-booch-song.mp3
//   node pop/booch/bin/sing.mjs --bed <path>
//   node pop/booch/bin/sing.mjs --voxonly    # render vocal bus only (debug)

import { existsSync, readFileSync, writeFileSync, mkdirSync, unlinkSync, readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));
const BB = resolve(HERE, "..");
const OUT_DIR = `${BB}/out`;
const CACHE = `${OUT_DIR}/.vox-cache`;
mkdirSync(CACHE, { recursive: true });

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}

const SLUG = "visualize-my-booch";
const BED_DEFAULT = `${OUT_DIR}/${SLUG}.wav`;
let BED = flags.bed ? resolve(process.cwd(), flags.bed) : BED_DEFAULT;
if (!existsSync(BED)) {
  // try mp3
  const alt = BED.replace(/\.wav$/, ".mp3");
  if (existsSync(alt)) BED = alt;
  else { console.error(`✗ bed missing: ${BED}\n  run: node pop/booch/bin/render.mjs --wav`); process.exit(1); }
}
const FINAL = resolve(process.cwd(), flags.out || `${OUT_DIR}/${SLUG}-song.mp3`);

// ── runner ──────────────────────────────────────────────────────────────
function run(cmd, args, opts = {}) {
  const r = spawnSync(cmd, args, { stdio: opts.quiet ? "ignore" : "inherit", ...opts });
  if (r.status !== 0) {
    if (!opts.soft) {
      console.error(`✗ ${cmd} ${args.slice(0, 4).join(" ")}… exit ${r.status}`);
      process.exit(1);
    }
    return false;
  }
  return true;
}

// ── 1 + 2: render any unique variant of the phrase, cached ─────────────
// Phrase kernels — what `say` is asked to speak.
const PHRASES = {
  full:  "visualize my booch",
  first: "visualize",
  last:  "my booch",
  short: "booch",
  vi:    "vi",          // for chops
  su:    "su",
  a:     "ah",          // ‘a’ alone confuses some voices
  lize:  "lize",
};

// Voices used in the schedule. Verified present via `say -v "?"`.
// (Albert/Bahh/Trinoids/Zarvox are character voices — alien texture.)
const VOICES = ["Alex", "Samantha", "Daniel", "Victoria", "Whisper", "Zarvox"];

// Return Float32Array for (voice, phrase, rate, pitchSemi, stretch, reverse).
// All cached on disk as raw f32le so reruns cost almost nothing.
const sampleCache = new Map();
function getVariant({ voice, phrase, rate = 175, pitchSemi = 0, stretch = 1.0, reverse = false }) {
  const key = `${voice}_${phrase}_r${rate}_p${pitchSemi.toFixed(2)}_s${stretch.toFixed(3)}_${reverse ? "rev" : "fwd"}`;
  if (sampleCache.has(key)) return sampleCache.get(key);
  const rawPath = `${CACHE}/${key}.f32`;

  if (!existsSync(rawPath)) {
    // 1a. say → AIFF
    const aiff = `${CACHE}/${key}.aiff`;
    const text = PHRASES[phrase] ?? phrase;
    run("say", ["-v", voice, "-r", String(rate), "-o", aiff, text], { quiet: true });
    if (!existsSync(aiff)) {
      console.error(`✗ say failed for voice="${voice}" phrase="${phrase}"`);
      process.exit(1);
    }
    // 1b. aiff → 48k mono wav (intermediate)
    const wav = `${CACHE}/${key}.wav`;
    run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-i", aiff, "-ar", String(SR), "-ac", "1", wav], { quiet: true });
    try { unlinkSync(aiff); } catch {}

    // 2. derive: rubberband (independent pitch + time), then reverse via ffmpeg
    let cur = wav;
    if (pitchSemi !== 0 || stretch !== 1.0) {
      const rb = `${CACHE}/${key}.rb.wav`;
      // rubberband: --time scales DURATION (>1 = longer), --pitch in cents
      const rbArgs = ["--time", String(stretch), "--pitch", String(pitchSemi), cur, rb];
      run("rubberband", rbArgs, { quiet: true });
      try { unlinkSync(cur); } catch {}
      cur = rb;
    }
    if (reverse) {
      const rv = `${CACHE}/${key}.rv.wav`;
      run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
        "-i", cur, "-af", "areverse", rv], { quiet: true });
      try { unlinkSync(cur); } catch {}
      cur = rv;
    }
    // → raw f32le mono
    run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-i", cur, "-ar", String(SR), "-ac", "1", "-f", "f32le", rawPath], { quiet: true });
    try { unlinkSync(cur); } catch {}
  }

  const buf = readFileSync(rawPath);
  const arr = new Float32Array(buf.buffer, buf.byteOffset, buf.byteLength / 4);
  // Copy to a fresh Float32 (the underlying Buffer may be a view)
  const out = new Float32Array(arr.length);
  out.set(arr);
  sampleCache.set(key, out);
  return out;
}

// ── 3: blit a variant into a stereo vocal bus with pan + gain + fades ──
function placeStereo(voxL, voxR, sample, opts = {}) {
  const startIdx = Math.floor((opts.t ?? 0) * SR);
  const gain = opts.gain ?? 1.0;
  const pan = Math.max(-1, Math.min(1, opts.pan ?? 0));
  // equal-power pan
  const aL = Math.sqrt(0.5 * (1 - pan));
  const aR = Math.sqrt(0.5 * (1 + pan));
  const fadeIn = Math.floor((opts.fadeIn ?? 0) * SR);
  const fadeOut = Math.floor((opts.fadeOut ?? 0) * SR);
  const n = sample.length;
  for (let i = 0; i < n; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= voxL.length) continue;
    let env = 1;
    if (fadeIn > 0 && i < fadeIn) env *= i / fadeIn;
    if (fadeOut > 0 && i > n - fadeOut) env *= Math.max(0, (n - i) / fadeOut);
    const s = sample[i] * gain * env;
    voxL[dst] += s * aL;
    voxR[dst] += s * aR;
  }
}

function place(voxL, voxR, ev) {
  // Harmony stack — repeats the same placement at multiple pitch offsets.
  // ev.harm = [0, +3, +7] for a D-minor triad (root + minor-3rd + 5th).
  // ev.harmVoices = optional [v1, v2, v3] to assign per-harmony voice (so
  // a chord sounds like a small ensemble, not a chorus effect on one voice).
  const harm = ev.harm ?? [0];
  const harmVoices = ev.harmVoices ?? null;
  const harmGain = ev.harmGain ?? 1.0;
  const baseGain = ev.gain ?? 1.0;
  for (let h = 0; h < harm.length; h++) {
    const pitchOffset = harm[h];
    const voiceForH = harmVoices ? (harmVoices[h] ?? ev.voice) : ev.voice;
    const sample = getVariant({
      voice: voiceForH,
      phrase: ev.phrase ?? "full",
      rate: ev.rate ?? 175,
      pitchSemi: (ev.pitch ?? 0) + pitchOffset,
      stretch: ev.stretch ?? 1.0,
      reverse: !!ev.reverse,
    });
    placeStereo(voxL, voxR, sample, {
      t: ev.t,
      gain: baseGain * (h === 0 ? 1.0 : harmGain),
      pan: (ev.pan ?? 0) + (harm.length > 1 ? (h - (harm.length - 1) / 2) * 0.15 : 0),
      fadeIn: ev.fadeIn ?? 0,
      fadeOut: ev.fadeOut ?? 0,
    });
  }
}

// ── the schedule: phrase rolled across the entire bed ──────────────────
// Section boundaries (94 BPM, 4-bar units = ~10.21s):
//   intro   0.00 – 10.21
//   hook1  10.21 – 20.43
//   verse1 20.43 – 40.85
//   hook2  40.85 – 51.06
//   verse2 51.06 – 71.49   ← Timbaland sparse — cut-up territory
//   bridge 71.49 – 81.70   ← drums out — most experimental
//   hook3  81.70 – 91.91
//   outro  91.91 – 103.60
const HOOK1 = 10.21, V1 = 20.43, HOOK2 = 40.85, V2 = 51.06, BR = 71.49, HOOK3 = 81.70, OUTRO = 91.91;
const BAR = (60 / 94) * 4;                  // ≈ 2.553 s
const HALF = BAR / 2;
const Q = BAR / 4;                          // quarter / beat ≈ 0.638
const SX = BAR / 16;                         // 16th ≈ 0.1596

const EVENTS = [
  // ── INTRO — one long stretched whisper-pitch-down phrase, fades in
  { t: 1.6, voice: "Whisper", phrase: "full", rate: 130, pitch: -5, stretch: 3.6,
    gain: 0.55, pan: 0.0, fadeIn: 1.5, fadeOut: 1.0 },
  // sub-bass spoken word: deep Alex underneath
  { t: 3.5, voice: "Alex", phrase: "full", rate: 110, pitch: -9, stretch: 2.8,
    gain: 0.42, pan: -0.4, fadeIn: 0.8, fadeOut: 0.6 },

  // ── HOOK1 — call the phrase straight, then harmonized, then chopped
  // bar 1 — Alex straight, dead center
  { t: HOOK1 + 0.10, voice: "Alex", phrase: "full", rate: 150, gain: 0.95 },
  // bar 2 — harmonized stack (D minor triad: 0, +3, +7) with 3 voices
  { t: HOOK1 + BAR + 0.05, voice: "Alex", phrase: "full", rate: 150,
    gain: 0.78, harm: [0, +3, +7],
    harmVoices: ["Alex", "Samantha", "Daniel"], harmGain: 0.55 },
  // bar 3 — just "visualize" panned, low
  { t: HOOK1 + 2*BAR + 0.10, voice: "Victoria", phrase: "first", rate: 140,
    pitch: -2, gain: 0.85, pan: -0.5 },
  // bar 3 — "my booch" answer panned other way, slight up
  { t: HOOK1 + 2*BAR + HALF + 0.05, voice: "Daniel", phrase: "last", rate: 150,
    pitch: +2, gain: 0.85, pan: +0.5 },
  // bar 4 — full hook with a reversed shadow underneath
  { t: HOOK1 + 3*BAR + 0.10, voice: "Alex", phrase: "full", rate: 150, gain: 0.92 },
  { t: HOOK1 + 3*BAR + 0.10, voice: "Samantha", phrase: "full", rate: 150,
    reverse: true, gain: 0.35, pan: 0.0 },

  // ── V1 — sparse, conversational fragments scattered
  // bar 1 — "visualize" lone, panned L
  { t: V1 + 0.20, voice: "Alex", phrase: "first", rate: 160, pitch: -3,
    gain: 0.62, pan: -0.6 },
  // bar 2.5 — "booch" alone way up, ping R
  { t: V1 + 1.5*BAR + 0.30, voice: "Samantha", phrase: "short", rate: 170,
    pitch: +7, stretch: 0.7, gain: 0.55, pan: +0.7 },
  // bar 4 — slow full Victoria, dead center
  { t: V1 + 3*BAR + 0.00, voice: "Victoria", phrase: "full", rate: 130,
    stretch: 1.4, gain: 0.72 },
  // bar 5 — "my booch" reversed, low
  { t: V1 + 4.5*BAR + 0.10, voice: "Alex", phrase: "last", rate: 150,
    reverse: true, pitch: -2, gain: 0.55, pan: -0.3 },
  // bar 6 — "visualize" stretched 2x, center
  { t: V1 + 5*BAR + 0.00, voice: "Daniel", phrase: "first", rate: 140,
    stretch: 2.0, pitch: 0, gain: 0.55, pan: +0.2 },
  // bar 7 — short rapid stutter (3 quick "vi"s)
  { t: V1 + 6*BAR + 0.00, voice: "Alex", phrase: "vi", rate: 200, gain: 0.7, pan: -0.4 },
  { t: V1 + 6*BAR + SX*2, voice: "Alex", phrase: "vi", rate: 200, pitch: +2, gain: 0.7 },
  { t: V1 + 6*BAR + SX*4, voice: "Alex", phrase: "vi", rate: 200, pitch: +4, gain: 0.7, pan: +0.4 },
  // bar 8 — full phrase lead-in to hook2 — Samantha, normal
  { t: V1 + 7*BAR + HALF, voice: "Samantha", phrase: "full", rate: 155, gain: 0.85 },

  // ── HOOK2 — bigger version: harmony stack each bar + chop decorations
  // bar 1 — main hit
  { t: HOOK2 + 0.10, voice: "Alex", phrase: "full", rate: 150, gain: 0.95 },
  // bar 2 — 4-voice harmonized stack (root, +3, +7, +12 — adds octave)
  { t: HOOK2 + BAR + 0.05, voice: "Alex", phrase: "full", rate: 150,
    gain: 0.78, harm: [0, +3, +7, +12],
    harmVoices: ["Alex", "Samantha", "Daniel", "Victoria"], harmGain: 0.50 },
  // bar 3 — call-and-answer
  { t: HOOK2 + 2*BAR + 0.05, voice: "Samantha", phrase: "short", rate: 160, gain: 0.85 },
  { t: HOOK2 + 2*BAR + HALF + 0.05, voice: "Daniel", phrase: "short", rate: 160,
    pitch: -5, gain: 0.85, pan: -0.4 },
  { t: HOOK2 + 2*BAR + 3*Q + 0.05, voice: "Alex", phrase: "short", rate: 160,
    pitch: +5, gain: 0.85, pan: +0.4 },
  // bar 4 — full hook stack, whisper layer on top, slight echo
  { t: HOOK2 + 3*BAR + 0.10, voice: "Alex", phrase: "full", rate: 150, gain: 0.92 },
  { t: HOOK2 + 3*BAR + 0.10, voice: "Whisper", phrase: "full", rate: 150,
    pitch: +7, gain: 0.45, pan: +0.6 },
  { t: HOOK2 + 3*BAR + 0.18, voice: "Whisper", phrase: "full", rate: 150,
    pitch: -5, gain: 0.42, pan: -0.6 },

  // ── V2 TIMBALAND-SPARSE — cut-up syllabic territory, big rests
  // bar 1 — "vi-su-a-lize" chopped across the bar, alternating pan
  { t: V2 + SX*0,  voice: "Alex", phrase: "vi",   rate: 200, gain: 0.78, pan: -0.5 },
  { t: V2 + SX*2,  voice: "Alex", phrase: "su",   rate: 200, gain: 0.78, pan: +0.5 },
  { t: V2 + SX*4,  voice: "Alex", phrase: "a",    rate: 200, gain: 0.78, pan: -0.5 },
  { t: V2 + SX*6,  voice: "Alex", phrase: "lize", rate: 200, gain: 0.78, pan: +0.5 },
  // bar 1 end — answered by a pitched-up "booch"
  { t: V2 + SX*10, voice: "Samantha", phrase: "short", rate: 180, pitch: +7, gain: 0.8 },
  // bar 2 — sparse: just one full-phrase Victoria, low, dead center
  { t: V2 + BAR + 0.1, voice: "Victoria", phrase: "full", rate: 140, pitch: -3,
    stretch: 1.1, gain: 0.7 },
  // bar 3 — reverse hits punctuated with ZARVOX alien character
  { t: V2 + 2*BAR + 0.0, voice: "Zarvox", phrase: "full", rate: 130, pitch: -2,
    gain: 0.55, pan: -0.3 },
  { t: V2 + 2*BAR + HALF + 0.0, voice: "Alex", phrase: "first", rate: 160,
    reverse: true, gain: 0.6, pan: +0.4 },
  // bar 4 — total silence except a single "booch" deep down at the end
  { t: V2 + 3*BAR + 3*Q, voice: "Alex", phrase: "short", rate: 140, pitch: -10,
    stretch: 1.6, gain: 0.7 },
  // bar 5 — chop again but smaller subset + reversed "lize"
  { t: V2 + 4*BAR + 0.0,  voice: "Daniel", phrase: "vi",   rate: 200, gain: 0.7, pan: -0.6 },
  { t: V2 + 4*BAR + SX*3, voice: "Daniel", phrase: "su",   rate: 200, gain: 0.7, pan: +0.6 },
  { t: V2 + 4*BAR + SX*6, voice: "Daniel", phrase: "lize", rate: 200, reverse: true, gain: 0.7 },
  // bar 6 — "my booch" stretched, harmonized 2-voice (root + 5th)
  { t: V2 + 5*BAR + HALF, voice: "Samantha", phrase: "last", rate: 145,
    stretch: 1.3, gain: 0.75, harm: [0, +7], harmVoices: ["Samantha", "Daniel"],
    harmGain: 0.6 },
  // bar 7 — alien character "visualize" Zarvox, low
  { t: V2 + 6*BAR + 0.2, voice: "Zarvox", phrase: "first", rate: 130, pitch: -5,
    gain: 0.5, pan: -0.5 },
  // bar 7.5 — short fast call-and-answer
  { t: V2 + 6*BAR + HALF + SX*2, voice: "Whisper", phrase: "short", rate: 180,
    pitch: +5, gain: 0.55, pan: +0.5 },
  // bar 8 — lead-in to bridge: long stretched "viiisualize"
  { t: V2 + 7*BAR + HALF, voice: "Alex", phrase: "first", rate: 130, stretch: 2.5,
    pitch: -3, gain: 0.6, fadeOut: 0.4 },

  // ── BRIDGE — drum-out — most experimental, big space
  // Long stretched main phrase, whisper, panned wide
  { t: BR + 0.3, voice: "Whisper", phrase: "full", rate: 130, stretch: 4.5,
    pitch: -1, gain: 0.6, pan: -0.7, fadeIn: 1.0, fadeOut: 1.5 },
  { t: BR + 0.6, voice: "Whisper", phrase: "full", rate: 130, stretch: 4.5,
    pitch: +6, gain: 0.42, pan: +0.7, fadeIn: 1.5, fadeOut: 2.0 },
  // Mid-bridge: a Zarvox alien text statement
  { t: BR + 1.5*BAR + 0.0, voice: "Zarvox", phrase: "full", rate: 140, pitch: -4,
    stretch: 1.3, gain: 0.65 },
  // Late-bridge: Victoria intimate, dead center
  { t: BR + 2.5*BAR + 0.2, voice: "Victoria", phrase: "full", rate: 130,
    stretch: 1.4, gain: 0.78, fadeOut: 0.5 },
  // Pickup back into hook3: a tiny rising chip-pitch "booch"
  { t: BR + 3.5*BAR + 0.1, voice: "Samantha", phrase: "short", rate: 200,
    pitch: +4, stretch: 0.8, gain: 0.7 },
  { t: BR + 3.5*BAR + SX*2, voice: "Samantha", phrase: "short", rate: 200,
    pitch: +7, stretch: 0.7, gain: 0.7 },
  { t: BR + 3.5*BAR + SX*4, voice: "Samantha", phrase: "short", rate: 200,
    pitch: +12, stretch: 0.6, gain: 0.7 },

  // ── HOOK3 — the BIG version. Full ensemble, harmonized, chops behind.
  // bar 1 — full triad stack
  { t: HOOK3 + 0.10, voice: "Alex", phrase: "full", rate: 150,
    gain: 0.95, harm: [0, +3, +7],
    harmVoices: ["Alex", "Samantha", "Daniel"], harmGain: 0.6 },
  // bar 1 decoration — Victoria 1 octave under, low
  { t: HOOK3 + 0.10, voice: "Victoria", phrase: "full", rate: 150,
    pitch: -12, gain: 0.40, pan: 0.0 },
  // bar 2 — answer-call with 4-voice ensemble (root + 3 + 5 + octave)
  { t: HOOK3 + BAR + 0.05, voice: "Alex", phrase: "full", rate: 150,
    gain: 0.82, harm: [0, +3, +7, +12],
    harmVoices: ["Alex", "Samantha", "Daniel", "Victoria"], harmGain: 0.50 },
  // bar 3 — pure chops — "vi" "su" "a" "lize" answered with stretched "booch"
  { t: HOOK3 + 2*BAR + 0.00, voice: "Alex",   phrase: "vi", rate: 200, gain: 0.7, pan: -0.4 },
  { t: HOOK3 + 2*BAR + SX*2, voice: "Daniel", phrase: "su", rate: 200, gain: 0.7, pan: +0.4 },
  { t: HOOK3 + 2*BAR + SX*4, voice: "Samantha", phrase: "a", rate: 200, gain: 0.7, pan: -0.4 },
  { t: HOOK3 + 2*BAR + SX*6, voice: "Victoria", phrase: "lize", rate: 200, gain: 0.7, pan: +0.4 },
  { t: HOOK3 + 2*BAR + HALF + 0.05, voice: "Alex", phrase: "short", rate: 130,
    stretch: 1.6, pitch: -4, gain: 0.8 },
  // bar 4 — final hook hit, dense stack incl whisper top
  { t: HOOK3 + 3*BAR + 0.10, voice: "Alex", phrase: "full", rate: 150,
    gain: 0.95, harm: [0, +3, +7],
    harmVoices: ["Alex", "Samantha", "Daniel"], harmGain: 0.55 },
  { t: HOOK3 + 3*BAR + 0.10, voice: "Whisper", phrase: "full", rate: 150,
    pitch: +12, gain: 0.40, pan: +0.7 },
  { t: HOOK3 + 3*BAR + 0.10, voice: "Whisper", phrase: "full", rate: 150,
    pitch: -7, gain: 0.36, pan: -0.7 },

  // ── OUTRO — one final stretched "visualize my booch" Alex, fade out
  { t: OUTRO + 0.5, voice: "Alex", phrase: "full", rate: 140, stretch: 2.6,
    pitch: -2, gain: 0.75, fadeIn: 0.6, fadeOut: 3.5 },
  // ghost reversed under it
  { t: OUTRO + 1.2, voice: "Samantha", phrase: "full", rate: 140, stretch: 2.4,
    reverse: true, pitch: -4, gain: 0.32, pan: -0.5, fadeOut: 4.5 },
  // very last whisper word, panned center, super short fade
  { t: OUTRO + 7.0, voice: "Whisper", phrase: "short", rate: 130, stretch: 1.5,
    pitch: -2, gain: 0.55, fadeIn: 0.4, fadeOut: 2.0 },
];

// ── render the vocal bus ───────────────────────────────────────────────
// Probe bed duration
const bp = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", BED], { encoding: "utf8" });
const bedDur = parseFloat(bp.stdout.trim()) || 103.6;
const NS = Math.ceil((bedDur + 2.0) * SR);
const voxL = new Float32Array(NS);
const voxR = new Float32Array(NS);

console.log(`━━━ booch · sing (apple TTS · phrase = "visualize my booch") ━━━`);
console.log(`  bed ${bedDur.toFixed(1)}s · ${EVENTS.length} events · cache → ${CACHE.replace(BB + "/", "")}\n`);

const t0 = Date.now();
let cacheHits = 0, cacheNew = 0;
const beforeFiles = new Set(readdirSync(CACHE));
for (const ev of EVENTS) place(voxL, voxR, ev);
const afterFiles = readdirSync(CACHE);
for (const f of afterFiles) {
  if (beforeFiles.has(f)) cacheHits++; else cacheNew++;
}
console.log(`  ✓ vocal bus built in ${((Date.now() - t0) / 1000).toFixed(1)}s ` +
            `(cache ${cacheHits} hits / ${cacheNew} new)`);

// Normalize the vocal bus so we have predictable headroom going into the mix.
let voxPeak = 0;
for (let i = 0; i < NS; i++) {
  const a = Math.max(Math.abs(voxL[i]), Math.abs(voxR[i]));
  if (a > voxPeak) voxPeak = a;
}
if (voxPeak > 0) {
  const norm = 0.85 / voxPeak;
  for (let i = 0; i < NS; i++) { voxL[i] *= norm; voxR[i] *= norm; }
}

// Write vocal bus as stereo raw f32le for ffmpeg
const voxRaw = `${OUT_DIR}/.tmp-vox.f32`;
mkdirSync(dirname(voxRaw), { recursive: true });
const interleave = Buffer.alloc(NS * 2 * 4);
for (let i = 0; i < NS; i++) {
  interleave.writeFloatLE(voxL[i], i * 8);
  interleave.writeFloatLE(voxR[i], i * 8 + 4);
}
writeFileSync(voxRaw, interleave);

// ── debug: voxonly skips the bed mix and just dumps the vocal bus ──────
if (flags.voxonly) {
  const voxOut = resolve(process.cwd(), flags.out || `${OUT_DIR}/${SLUG}-vox-only.wav`);
  run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", voxRaw,
    "-c:a", "pcm_s16le", voxOut]);
  try { unlinkSync(voxRaw); } catch {}
  console.log(`✓ ${voxOut} (vocal bus only)`);
  process.exit(0);
}

// ── 4: mix with bed via ffmpeg ─────────────────────────────────────────
// Bed is mono — duplicate to stereo. Sidechain-duck under the vocal bus
// so phrases punch through; light loudnorm at the end.
const fadeStart = Math.max(0, bedDur - 2.0);
const fc =
  `[0:a]channelmap=channel_layout=mono,pan=stereo|c0=c0|c1=c0[bedS];` +
  `[1:a]volume=1.0[voxS];` +
  `[voxS]asplit=2[voxM][voxK];` +
  `[bedS]volume=0.62[bedlo];` +
  `[bedlo][voxK]sidechaincompress=threshold=0.04:ratio=5:attack=8:release=240:makeup=1[bedd];` +
  `[bedd][voxM]amix=inputs=2:duration=longest:normalize=0,` +
    `atrim=duration=${bedDur.toFixed(3)},` +
    `afade=t=out:st=${fadeStart.toFixed(3)}:d=2.0,` +
    `loudnorm=I=-14:TP=-1.0:LRA=11,aresample=48000[out]`;

run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-i", BED,
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", voxRaw,
  "-filter_complex", fc, "-map", "[out]",
  "-c:a", "libmp3lame", "-q:a", "2", FINAL]);
try { unlinkSync(voxRaw); } catch {}

const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", FINAL], { encoding: "utf8" });
const sz = (readFileSync(FINAL).length / 1024).toFixed(0);
console.log(`\n✓ ${FINAL} (${sz} KB · ${parseFloat(probe.stdout || 0).toFixed(1)}s)`);
console.log(`  ${EVENTS.length} placements across ${VOICES.join(", ")} · phrase rolled, harmonized, chopped, reversed, panned`);
