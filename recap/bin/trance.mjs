#!/usr/bin/env node
// trance.mjs — render a trance / trancewaltz bed for pop/dance/.
//
// Bottom-up + native-portable: every voice resolves to AC's
// sound.synth(...) contract (the same one percussion.mjs uses, the same
// one fedac/native audio.c implements). Drums come from percussion.mjs.
// Supersaw lead/pad come from pop/dance/synths/supersaw.mjs (now N
// parallel sound.synth({type:"sawtooth"}) calls — runs everywhere).
// Sub-bass is one triangle voice — natively dark, no filter needed.
//
// Two meters supported:
//   --meter 4   (default trance, 4/4, kick-on-every-beat ×4)
//   --meter 3   (trancewaltz, 3/4, kick-on-every-beat ×3) — waltz lilt
//               retained, trance engine + arc on top.
//
// Per-section effects: each SECTION_TEMPLATES entry can carry `wobble`
// and `bitcrush` configs (with envelopes) applied to that section only.
//
// Vocal: pass `--vocal-stem path.mp3` to mix a pre-rendered vocal stem
// during the drops. (Generation lives in pop/dance/bin/vocal.mjs.)
//
// Usage:
//   node recap/bin/trance.mjs                                # 4/4 radio edit
//   node recap/bin/trance.mjs --meter 3 --bpm 108            # trancewaltz
//   node recap/bin/trance.mjs --section drop --bars 16       # 16-bar drop
//   node recap/bin/trance.mjs --vocal-stem ~/Desktop/vox.mp3 --out ~/Desktop/trance.mp3

import {
  readFileSync,
  writeFileSync,
  mkdirSync,
  unlinkSync,
  existsSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { playPercussion } from "../../system/public/aesthetic.computer/lib/percussion.mjs";
import { mixEventSinePower as mixEventLeadPad } from "../../pop/dance/synths/sinepower.mjs";
import { mixEventSupersaw } from "../../pop/dance/synths/supersaw.mjs";
import { makeBufferSynth as _makeBufferSynth } from "../../pop/dance/synths/bus.mjs";

// Chill ONLY: every oscillator becomes a pure SINE — no saw / square /
// triangle timbre anywhere (jas: "replace every wave type with sine",
// "no more saw"). `noise` stays noise: it is air/percussion texture, not
// an oscillator timbre, and sining it would erase the hats/kicks/risers.
// ALSO snaps attacks short so the whole thing is PUNCHY (jas: "some
// stuff just has too long attacks ... really shorten the attacks ...
// make it all PUNCHY"): attack → min(attack*0.3, 50 ms) — ~3× snappier
// across the board, capped so pads/swells still don't click.
// Non-chill returns the untouched instance → trancenwaltz byte-identical.
function makeBufferSynth(target, startSec, sr, rng) {
  const inst = _makeBufferSynth(target, startSec, sr, rng);
  if (typeof isChill === "undefined" || !isChill) return inst;
  const _synth = inst.synth.bind(inst);
  inst.synth = (opts) => {
    if (opts) {
      const needSine = opts.type && opts.type !== "noise" && opts.type !== "sine";
      const needSnap = typeof opts.attack === "number" && opts.attack > 0.0005;
      const needTail = typeof opts.decay === "number";
      if (needSine || needSnap || needTail) {
        opts = { ...opts };
        if (needSine) opts.type = "sine";
        // Softer, couch-like (jas: "lengthen the decay and attack on
        // stuff a lil") — relax the hard snap (was *0.3 cap 50 ms) and
        // give tails a little more time.
        if (needSnap) opts.attack = Math.min(opts.attack * 0.65, 0.12);
        if (needTail) opts.decay = opts.decay * 1.2;
      }
    }
    return _synth(opts);
  };
  return inst;
}
import { applyWobble, applyBitcrush, applyFlange, softClip } from "../../pop/dance/synths/fx.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");

// ── piano sample bank (native ships fedac/native/samples/piano) ───────
// Each anchor is a 48kHz mono Float32 raw at the given MIDI pitch.
const PIANO_DIR = `${REPO}/fedac/native/samples/piano`;
const PIANO_ANCHORS = [21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96];
let PIANO_BANK = null;
function loadPianoBank() {
  if (PIANO_BANK) return PIANO_BANK;
  PIANO_BANK = new Map();
  for (const m of PIANO_ANCHORS) {
    const path = `${PIANO_DIR}/${m}.raw`;
    if (!existsSync(path)) throw new Error(`missing piano anchor ${path}`);
    const buf = readFileSync(path);
    const f32 = new Float32Array(buf.buffer, buf.byteOffset, buf.byteLength / 4);
    PIANO_BANK.set(m, Float32Array.from(f32));
  }
  return PIANO_BANK;
}
function pianoAnchorFor(midi) {
  let best = PIANO_ANCHORS[0];
  for (const a of PIANO_ANCHORS) if (Math.abs(a - midi) < Math.abs(best - midi)) best = a;
  const ratio = Math.pow(2, (midi - best) / 12);
  return { sample: PIANO_BANK.get(best), ratio };
}
// ── zoo sample bank (lion roar etc — for the drop BOOM) ─────────────
// Native ships fedac/native/samples/zoo with f32 mono 48k samples.
const ZOO_DIR = `${REPO}/fedac/native/samples/zoo`;
const ZOO_BANK = new Map();
function loadZooSample(name) {
  if (ZOO_BANK.has(name)) return ZOO_BANK.get(name);
  const path = `${ZOO_DIR}/${name}.raw`;
  if (!existsSync(path)) return null;
  const buf = readFileSync(path);
  const f32 = new Float32Array(buf.buffer, buf.byteOffset, buf.byteLength / 4);
  const owned = Float32Array.from(f32);
  ZOO_BANK.set(name, owned);
  return owned;
}
// Decode a sourced SFX wav (CC0 archive.org assets in pop/dance/out)
// to mono f32 at SAMPLE_RATE, cached. Used by the --gallop layer.
const _sfxCache = new Map();
function loadSfxF32(file) {
  if (_sfxCache.has(file)) return _sfxCache.get(file);
  const p = `${REPO}/pop/dance/out/${file}`;
  if (!existsSync(p)) { _sfxCache.set(file, null); return null; }
  const tmp = `${dirname(OUT_PATH)}/.sfxdec-${file.replace(/[^a-z0-9]/gi, "")}.f32`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", p, "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1", tmp]);
  let s = null;
  if (r.status === 0 && existsSync(tmp)) {
    const b = readFileSync(tmp);
    s = Float32Array.from(new Float32Array(b.buffer, b.byteOffset, b.byteLength / 4));
    try { unlinkSync(tmp); } catch {}
  }
  _sfxCache.set(file, s);
  return s;
}
function mixZooSample(ev, out, sampleRate) {
  const sample = loadZooSample(ev.name);
  if (!sample) return;
  const pitchRatio = ev.pitchRatio ?? 1.0;
  const startIdx = Math.floor(ev.startSec * sampleRate);
  const lenSamples = Math.floor(sample.length / pitchRatio);
  const attackS = Math.min(0.020 * sampleRate, lenSamples * 0.05);
  const releaseS = Math.min(0.180 * sampleRate, lenSamples * 0.30);
  for (let i = 0; i < lenSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    const srcF = i * pitchRatio;
    const s0 = Math.floor(srcF);
    const s1 = s0 + 1;
    if (s1 >= sample.length) break;
    const frac = srcF - s0;
    const v = sample[s0] * (1 - frac) + sample[s1] * frac;
    let env = 1;
    if (i < attackS) env = i / attackS;
    else if (i > lenSamples - releaseS) env = Math.max(0, (lenSamples - i) / releaseS);
    out[dst] += v * env * (ev.gain ?? 1);
  }
}

// Cat DRONE — granular time-stretch of the meow so it sustains over
// many bars while its pitch is set independently (overlap-add Hann
// grains; source crawls across the whole sample over durSec, each
// grain repitched by pitchRatio). Layer several at consonant ratios
// for an autotuned, harmonized "cat choir" pad.
function mixCatDrone(out, startSec, durSec, pitchRatio, gain) {
  const sample = loadZooSample("cat");
  if (!sample) return;
  const SR = SAMPLE_RATE;
  const grain = Math.floor(0.09 * SR);
  const hop = Math.floor(grain * 0.5);
  const outStart = Math.floor(startSec * SR);
  const outLen = Math.floor(durSec * SR);
  if (outLen < grain) return;
  // BUGFIX: crawling the whole sample over durSec was a ~48× stretch
  // that froze on one instant (no audible elongation/pitch). Instead
  // loop the meow at a moderate slow pass (~4.5 s) so it's a
  // recognizably *slowed, evolving* meow, with a pitch GLIDE down
  // (deepen + darken across the drone) and a slow volume ROLL.
  const passSec = 4.5;
  const srcPerOut = sample.length / (passSec * SR);
  for (let gpos = 0; gpos + grain < outLen; gpos += hop) {
    const prog = gpos / outLen;                         // 0..1 over the drone
    const pr = pitchRatio * (1 - 0.45 * prog);          // glide down — darkens
    const roll = 0.55 + 0.45 * Math.sin(prog * Math.PI * 5.0 + 0.7); // vol roll
    const srcCenter = (gpos * srcPerOut) % sample.length;
    for (let k = 0; k < grain; k++) {
      const w = 0.5 - 0.5 * Math.cos((2 * Math.PI * k) / grain); // Hann
      let si = srcCenter + (k - grain / 2) * pr;
      si = ((si % sample.length) + sample.length) % sample.length;
      const i0 = Math.floor(si);
      const i1 = (i0 + 1) % sample.length;
      const fr = si - i0;
      const v = sample[i0] * (1 - fr) + sample[i1] * fr;
      const d = outStart + gpos + k;
      if (d >= 0 && d < out.length) out[d] += v * w * gain * roll;
    }
  }
}

function mixEventPiano(ev, out, sampleRate) {
  const { sample, ratio } = pianoAnchorFor(ev.midi);
  if (!sample) return;
  const startIdx = Math.floor(ev.startSec * sampleRate);
  const durSamples = Math.floor(ev.durSec * sampleRate);
  const attack = Math.min(0.005 * sampleRate, durSamples * 0.05);
  const release = Math.min(0.10 * sampleRate, durSamples * 0.5);
  const lenOut = durSamples + Math.floor(release);
  // Optional per-note expression (jas, chill high piano keys): play
  // backwards (ev.reverse), a pitch-BEND glide over the note (ev.bend
  // in semitones, ± ), and a longer sustain comes from a bigger
  // ev.durSec from the caller.
  const rev = !!ev.reverse;
  const bend = ev.bend || 0;
  const span = Math.min(lenOut, Math.floor((sample.length - 2) / Math.max(1e-6, ratio)));
  for (let i = 0; i < lenOut; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    const u = lenOut > 1 ? i / lenOut : 0;
    const r = ratio * Math.pow(2, (bend * u) / 12);   // glide
    const srcF = rev ? (span - i) * r : i * r;
    if (srcF < 0) break;
    const s0 = Math.floor(srcF);
    const s1 = s0 + 1;
    if (s1 >= sample.length || s0 < 0) { if (rev) continue; else break; }
    const frac = srcF - s0;
    const v = sample[s0] * (1 - frac) + sample[s1] * frac;
    let env = 1;
    if (i < attack) env = i / attack;
    else if (i > durSamples) env = Math.max(0, 1 - (i - durSamples) / release);
    // reversed notes swell IN (soft attack → transient at the end)
    if (rev) env = Math.min(1, (i + 1) / Math.max(1, durSamples)) * (i > durSamples ? Math.max(0, 1 - (i - durSamples) / release) : 1);
    out[dst] += v * env * ev.gain;
  }
}

// ── sinebells (pure additive synth — native-portable via sound.synth) ─
const BELL_PARTIALS = [
  { ratio: 0.5,  amp: 0.28, decayT60: 5.5 },
  { ratio: 1.0,  amp: 1.00, decayT60: 4.5 },
  { ratio: 2.0,  amp: 0.32, decayT60: 2.6 },
  { ratio: 2.4,  amp: 0.10, decayT60: 1.2 },
  { ratio: 3.0,  amp: 0.09, decayT60: 1.0 },
  { ratio: 4.5,  amp: 0.04, decayT60: 0.6 },
];
function mixEventSinebell(ev, out, sampleRate) {
  const startIdx = Math.floor(ev.startSec * sampleRate);
  const tailSec = ev.tailSec ?? 4.0;
  const ringSamples = Math.floor((ev.durSec + tailSec) * sampleRate);
  // Attack — short by default (struck-bell), but the tail-pad mode
  // takes a much longer attack (slow swell, opening-pad character).
  const attackSec = ev.attackSec ?? 0.012;
  const attackS = attackSec * sampleRate;
  // Decay scale — multiply all partials' T60s by this. 1.0 = standard
  // struck bell. Higher = longer, more pad-like sustain. The tail-pad
  // mode passes ~3.0 so the bells turn into a slow opening drone.
  const decayScale = ev.decayScale ?? 1.0;
  const fundFreq = 440 * Math.pow(2, (ev.midi - 69) / 12);
  const twoPiOverSr = (2 * Math.PI) / sampleRate;
  const partials = BELL_PARTIALS.map((p) => ({
    omega: twoPiOverSr * fundFreq * p.ratio,
    amp: p.amp,
    decay: Math.exp(-Math.log(1000) / (p.decayT60 * decayScale * sampleRate)),
  }));
  for (let i = 0; i < ringSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    let s = 0;
    for (const p of partials) {
      const env = p.amp * Math.pow(p.decay, i);
      if (env < 1e-5) continue;
      s += Math.sin(p.omega * i) * env;
    }
    let att = 1;
    if (i < attackS) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackS);
    out[dst] += s * att * ev.gain * 0.42;
  }
}

// ── harp (low-octave plucked voice — pure harmonic partials) ─────────
// A close cousin of the sinebell — same additive-sine engine — but with
// strictly harmonic partial ratios (no inharmonicity) and a softer
// pluck envelope. Sounds warm and resonant at low pitches, perfect
// for the opening before any drums enter. Native-portable via the
// same sound.synth contract.
const HARP_PARTIALS = [
  { ratio: 1.0,  amp: 1.00, decayT60: 6.0 },
  { ratio: 2.0,  amp: 0.55, decayT60: 4.5 },
  { ratio: 3.0,  amp: 0.30, decayT60: 3.0 },
  { ratio: 4.0,  amp: 0.18, decayT60: 2.0 },
  { ratio: 5.0,  amp: 0.10, decayT60: 1.3 },
  { ratio: 6.0,  amp: 0.06, decayT60: 0.9 },
];
function mixEventHarp(ev, out, sampleRate) {
  const startIdx = Math.floor(ev.startSec * sampleRate);
  const tailSec = 5.0;
  const ringSamples = Math.floor((ev.durSec + tailSec) * sampleRate);
  // Slow soft pluck (~30 ms) — not a sharp bell attack.
  const attackS = 0.030 * sampleRate;
  const fundFreq = 440 * Math.pow(2, (ev.midi - 69) / 12);
  const twoPiOverSr = (2 * Math.PI) / sampleRate;
  const partials = HARP_PARTIALS.map((p) => ({
    omega: twoPiOverSr * fundFreq * p.ratio,
    amp: p.amp,
    decay: Math.exp(-Math.log(1000) / (p.decayT60 * sampleRate)),
  }));
  for (let i = 0; i < ringSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    let s = 0;
    for (const p of partials) {
      const env = p.amp * Math.pow(p.decay, i);
      if (env < 1e-5) continue;
      s += Math.sin(p.omega * i) * env;
    }
    let att = 1;
    if (i < attackS) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackS);
    out[dst] += s * att * ev.gain * 0.55;
  }
}

// ── parse args ─────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const METER       = Number(flags.meter ?? 4);       // 4=trance, 3=trancewaltz
const isWaltz     = METER === 3;
// MODE — "normal" (default — full narrative track with drops, screams,
// impacts, lion roar, machine guns, vocal stem, tape-stop) or
// "chill" (study-vibes minimal dance mix — continuous bed, no drops,
// no roar, no machine guns, no screams, no vocal stem, no tape-stop,
// flatter dynamics, longer runtime). Triggered by --mode chill.
const MODE        = flags.mode || "normal";
const isChill     = MODE === "chill";
// Default tempo tuned so 64 bars in waltz mode = exactly 84.0 s (1:24
// — jeffrey's birthday). 137.143 bpm × 3/4 × 64 = 84.000 s. Effectively
// "138 bpm trance" within 0.6%, but lands on a clean loop boundary.
// Chill is a DANCE track — base tempo well above the 137 trancenwaltz
// cut, and it accelerates from here (see BPM_END). Explicit --bpm wins.
// (Non-chill keeps the birthday tempo.)
const BPM         = Number(flags.bpm ?? (isChill ? 150 : 137.143));
// Chill: BRIGHT major key — flips the whole mood up / happy upswing
// (jas: "g major :D"). Non-chill keeps minor → trancenwaltz unchanged.
const SCALE_NAME  = flags.scale || (isChill ? "major" : "minor");
// Chill sits a key lower (57→54, down a minor 3rd) — brings the whole
// harmonic texture + the high tones down, in unison. --root overrides.
const ROOT_MIDI   = Number(flags.root ?? (isChill ? 55 : 57)); // chill: G (G3) — G major, bright/happy (jas)
const SECTION     = flags.section || "full";
const BARS_FLAG   = flags.bars !== undefined ? Number(flags.bars) : null;
const SEED_STR    = flags.seed || (isWaltz ? "trancewaltz" : "trance");
// Chill: PERCUSSION-FORWARD, tones quieter — a harder NIN/industrial
// balance (jas: "more percussion forward mix ... tones all quieter ...
// more NIN friendly industrial"). Drums up, lead/pad well down.
const DRUM_GAIN   = Number(flags["drum-gain"] ?? (isChill ? 0.92 : 0.95)); // perc lower in the mix (was 1.28, jeffrey: ~3 dB down)
// Hellsine "hardcorism" — opt-in per track (trancepenta sets --hell;
// trancenwaltzi leaves it 0 so its kick is unchanged). >0 swaps the
// chill kick for the ported gabber/Rotterdam kick.
const HELL        = Number(flags.hell ?? 0);
// chill hi-hats: "on" (default) | "off" | "low" (jas wants them OUT of
// trancepenta or far less in that pitch range).
const CHILL_HATS  = String(flags["chill-hats"] ?? "on");
// Synthesised galloping-hoof layer — opt-in (trancepenta sets
// --gallop; trancenwaltzi never does). Opens the track ("preview of
// what's coming") + drives the back half as a lighter bed.
const GALLOP      = Boolean(flags.gallop);
// --beat-in <sec>: hold the beat (kick/snare/tick/floor) off until N
// music-seconds, then ease in over ~3 s (jas: "start the beat more
// around 30 seconds in"). 0 = no delay (trancenwaltzi unchanged).
const BEAT_IN     = Number(flags["beat-in"] ?? 0);
// The "drop": ~28 s after BEAT_IN (≈ 1 min). Before it, the intro
// sits in a SHIFTED key; at the drop it resolves home for a big
// musical shift (jas). 0 = disabled (trancenwaltzi never sets BEAT_IN).
const DROP_SEC    = BEAT_IN > 0 ? BEAT_IN + 28 : 0;
// --kick-only: an experimental "drums = kicks ONLY" pass (jas: "remove
// all hats and all snares for now ... ONLY do kicks for this next
// pass"). Mutes the backbeat snare, the chill tick, the hat lane and
// any snare-roll — keeps the kick. Flag-gated + chill-scoped so
// trancewaltz stays byte-identical when the flag is absent.
const KICK_ONLY   = Boolean(flags["kick-only"]);
const LEAD_GAIN   = Number(flags["lead-gain"] ?? (isChill ? 0.24 : 0.55)); // melodic lower, esp highs (jas)
const PAD_GAIN    = Number(flags["pad-gain"]  ?? (isChill ? 0.12 : 0.30)); // chill: lower the "organ" pad (jas) — poppier
const BASS_GAIN   = Number(flags["bass-gain"] ?? (isChill ? 0.42 : 0.45)); // thwomp stays forward with the drums
const SIDECHAIN   = flags.sidechain !== "off";
const DUCK_DEPTH  = Number(flags["duck-depth"] ?? 0.40); // was 0.65 — too heavy, masking the supersaw under every kick
const DUCK_MS     = Number(flags["duck-ms"] ?? 120);
const VOCAL_STEM  = expandHome(flags["vocal-stem"]) || null;
const VOCAL_GAIN  = Number(flags["vocal-gain"] ?? 2.9);
const VOCAL_MODE  = flags["vocal-mode"] || "loop"; // "single" | "drops" | "all" | "loop"
const VOCAL_DUCK  = Number(flags["vocal-duck"] ?? 0.6); // bed gain under vocal

// Per-section vocal level — applied as a per-sample envelope when in
// loop mode so the vocal isn't blasting at full gain during the intro
// or breakdowns. Drops keep full power; builds and outro mute the
// vocal so the drop impact and musical ending breathe.
const VOCAL_SECTION_GAIN = {
  intro:  0.75,  // strong from t=0 — words intelligible right away
  break1: 0.85,
  build1: 0.0,   // bed-only build = real drop punch
  drop1:  1.00,
  break2: 0.85,
  build2: 0.0,
  drop2:  1.00,
  outro:  0.0,   // musical ending — no vocal
};
const PIANO_GAIN  = Number(flags["piano-gain"] ?? (isChill ? 0.66 : 0.45)); // lift the high piano out (jas)
const BELLS_GAIN  = Number(flags["bells-gain"] ?? (isChill ? 0.12 : 0.32)); // chill: tones quieter, bells are shrill (jas)
const STRUCT_PATH = expandHome(flags["struct-out"]) || null;
const OUT_PATH    = expandHome(flags.out) || `${ROOT}/out/${isWaltz ? "trancewaltz" : "trance"}.mp3`;
// --master → a streaming RELEASE cut (DistroKid/Spotify): restore the
// proper musical fade-in/out (a real single ending, NOT the 6ms loop
// declick) and, when the output is .wav, encode lossless 16-bit/44.1k.
const RELEASE_MASTER = Boolean(flags.master);
const OUT_IS_WAV = /\.wav$/i.test(OUT_PATH);

const SAMPLE_RATE = 48_000;

// ── PRNG ──────────────────────────────────────────────────────────────
function hashString(s) {
  let h = 2166136261 >>> 0;
  for (let i = 0; i < s.length; i++) { h ^= s.charCodeAt(i); h = Math.imul(h, 16777619); }
  return h >>> 0;
}
function makeRng(seedStr) {
  let s = hashString(seedStr) || 1;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}
const rng = makeRng(SEED_STR);
const noiseRng = makeRng(SEED_STR + ":noise");

// ── humanize layer ───────────────────────────────────────────────────
// Tiny random timing offsets applied to every note/hit so the track
// feels organically played rather than grid-locked. Anchor moments
// (drop impacts, sniper, stamps, vocal-aligned events, the boot &
// shutdown melodies) skip humanize so the song's tentpoles stay
// musically locked.
const humanRng = makeRng(SEED_STR + ":humanize");
const HUMANIZE_DEFAULT_MS = 7; // rehumanized — gentle bedroom looseness (kawaii), not robotic
function humanize(t, scaleMs = HUMANIZE_DEFAULT_MS) {
  // Symmetric ±scaleMs uniform jitter, with a tiny biased tail so a
  // few hits land further off the grid than the rest.
  const r = humanRng() * 2 - 1; // -1..+1
  const bias = humanRng() < 0.08 ? 1.6 : 1.0;
  return t + (r * scaleMs * bias) / 1000;
}

// ── musical theory ────────────────────────────────────────────────────
const SCALES = {
  major:    [0, 2, 4, 5, 7, 9, 11],
  minor:    [0, 2, 3, 5, 7, 8, 10],
  harmonic: [0, 2, 3, 5, 7, 8, 11],
  dorian:   [0, 2, 3, 5, 7, 9, 10],
};
const SCALE = SCALES[SCALE_NAME] || SCALES.minor;

function scaleNoteMidi(degree, octaveOffset = 0) {
  const len = SCALE.length;
  const idx = ((degree % len) + len) % len;
  const octShift = Math.floor(degree / len);
  return ROOT_MIDI + 12 * (octaveOffset + octShift) + SCALE[idx];
}
function chordMidis(rootDegree, octaveOffset = 0) {
  return [
    scaleNoteMidi(rootDegree,     octaveOffset),
    scaleNoteMidi(rootDegree + 2, octaveOffset),
    scaleNoteMidi(rootDegree + 4, octaveOffset),
  ];
}

// Multiple chord progressions that rotate every 8 bars. Gives real
// harmonic motion across the track instead of one looping cadence.
//   cycle 0: i  VI III  VII   — Am F C G   (canonical trance)
//   cycle 1: i  iv VI   VII   — Am Dm F G  (more melancholic)
//   cycle 2: i  III VI  iv    — Am C F Dm  (lifted)
//   cycle 3: i  VI iv   VII   — Am F Dm G  (resolution)
const PROGRESSIONS = [
  [0, 5, 2, 6],
  [0, 3, 5, 6],
  [0, 2, 5, 3],
  [0, 5, 3, 6],
];
const CHORD_BARS  = 2;
// Chill: a LONGER pool walked slowly, with chords held 3 bars — so it
// loops far less and travels through much more harmonic territory
// across the track ("more travel in the larger tones throughout").
const PROGRESSIONS_CHILL = [
  [0, 5, 2, 6], [0, 3, 5, 6], [0, 2, 5, 3], [0, 5, 3, 6],
  [0, 4, 5, 3], [0, 6, 3, 5], [0, 2, 3, 6], [0, 3, 6, 2],
];
// Chords move EVERY bar in chill (was 3) — the held 3-bar chords on a
// pure-sine pad just droned; per-bar changes give real melodic
// MOVEMENT instead of a static ring (jas: "not enough melody changes
// ... it seems to mostly drone ... more melodic MOVEMENTS").
const CHORD_BARS_CHILL = 1;
// ── SONATA-FORM ARCHITECTURE (chill only) ──────────────────────────
// jas: "please architect that based on a sonata / structure classic."
// Classical sonata-allegro mapped onto the track by bar fraction. The
// perceptually defining features — a PRIMARY theme, a contrasting
// SECONDARY theme a step away in key, a turbulent DEVELOPMENT that
// fragments + modulates, then a RECAPITULATION that brings BOTH themes
// home to the tonic, then a CODA — are expressed purely through the
// harmonic controller (progression row + a diatonic key-degree offset).
// Every pitched voice flows through scaleNoteMidi(chordDeg + …), so
// returning `prog + keyDeg` from progressionAt() restructures pad,
// bass, lead-harmony, gong and sub into the sonata in one chokepoint.
// keyDeg is a SCALE-degree offset (scaleNoteMidi wraps it diatonically
// & shifts octave) — no semitone-threading needed; +4 ≈ the dominant.
function sonataAt(barIdx) {
  const fr = TOTAL_BARS > 1 ? barIdx / (TOTAL_BARS - 1) : 0;
  // section, progression-row index, diatonic key offset
  if (fr < 0.14) return { sec: "expo-P",   prog: 0, keyDeg: 0 };  // primary, tonic
  if (fr < 0.20) return { sec: "expo-Tr",  prog: 1, keyDeg: 2 };  // transition, rising
  if (fr < 0.34) return { sec: "expo-S",   prog: 2, keyDeg: 4 };  // secondary, "dominant"
  if (fr < 0.40) return { sec: "expo-K",   prog: 3, keyDeg: 4 };  // codetta, dominant
  if (fr < 0.62) {                                                // DEVELOPMENT
    const ph = Math.floor((fr - 0.40) / 0.22 * 6);                // 6 unstable phrases
    const wander = [0, 2, 4, 1, 5, 3][Math.max(0, Math.min(5, ph))];
    return { sec: "devel", prog: (ph + 1) % PROGRESSIONS_CHILL.length, keyDeg: wander };
  }
  if (fr < 0.76) return { sec: "recap-P",  prog: 0, keyDeg: 0 };  // primary home
  if (fr < 0.80) return { sec: "recap-Tr", prog: 1, keyDeg: 0 };  // transition, no modulation
  if (fr < 0.90) return { sec: "recap-S",  prog: 2, keyDeg: 0 };  // secondary RESOLVED in tonic
  return { sec: "coda", prog: 3, keyDeg: 0 };                      // coda — settle
}
function progressionAt(barIdx) {
  if (isChill) {
    const s = sonataAt(barIdx);
    const step = Math.floor(barIdx / CHORD_BARS_CHILL);
    let deg = PROGRESSIONS_CHILL[s.prog][step % 4] + s.keyDeg;
    // INTRO KEY SHIFT → DROP (jas: "different key then drop it at the
    // 1 minute mark for a larger musical shift"). Whole intro sits a
    // diatonic 3rd up; at DROP_SEC it resolves home — every pitched
    // voice shifts together via this one chokepoint.
    if (DROP_SEC > 0) {
      const bt = (barStartRel[barIdx] ?? 1e9) + OPENING_PREFIX_SEC;
      if (bt < DROP_SEC) deg += 2;
    }
    return deg;
  }
  const progCycle = Math.floor(barIdx / (CHORD_BARS * 4)) % PROGRESSIONS.length;
  const progStep  = Math.floor((barIdx % (CHORD_BARS * 4)) / CHORD_BARS);
  return PROGRESSIONS[progCycle][progStep];
}

// ── theme ─────────────────────────────────────────────────────────────
// 4/4 theme: 8 eighth-notes per bar × 4 bars.
const THEME_4 = [
  0, 2, 4, 3,  4, 7, 4, 3,
  4, 5, 7, 4,  7, 9, 7, 4,
  7, 9, 7, 5,  4, 3, 4, 2,
  3, 2, 0, 2,  3, 4, 2, 0,
];
// 3/4 theme: 6 eighth-notes per bar × 4 bars. Three-feel phrasing —
// ascending arc resolving down. Each bar has its own internal 1-2-3.
const THEME_3 = [
  0, 2, 4,  3, 2, 0,
  6, 0, 2,  4, 3, 2,
  3, 4, 7,  4, 3, 2,
  0, 6, 4,  0, 2, 0,
];
const THEME = isWaltz ? THEME_3 : THEME_4;
const NOTES_PER_BAR_LEAD = isWaltz ? 6 : 8;

// ── arrangement ───────────────────────────────────────────────────────
// Per-section: which layers are active + which fx envelopes to apply.
// fx.wobble / fx.bitcrush envelopes are interpreted with t=0 at the
// start of the section, ending at section length.
const SECTION_TEMPLATES_4 = {
  intro:  { bars: 8,  kick: false, hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: false, riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: true,  drumDensity: 0.5 },
  break1: { bars: 16, kick: false, hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: false, drumDensity: 0.5, fx: { wobble: { target: "filter", baseCutoffHz: 2400, rate: [{ time: 0, rate: 0.5 }, { time: 1, rate: 1.5 }], depth: [{ time: 0, depth: 0.15 }, { time: 1, depth: 0.55 }] } } },
  build1: { bars: 8,  kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 16 }, { time: 1, bits: 6 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 4 }], mix: 0.7 } } },
  drop1:  { bars: 16, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: false, riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 0.5, bits: 10 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.5, downsample: 2 }, { time: 1, downsample: 1 }], mix: 0.3 } } },
  break2: { bars: 8,  kick: false, hat: false, sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: false, drumDensity: 0, fx: { wobble: { target: "filter", baseCutoffHz: 1800, rate: 2.0, depth: 0.45 } } },
  build2: { bars: 4,  kick: true,  hat: true,  sub: true,  lead: false, pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 1, bits: 5 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 6 }], mix: 0.8 } } },
  drop2:  { bars: 16, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: false, riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 12 }, { time: 0.4, bits: 8 }, { time: 0.7, bits: 12 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.4, downsample: 3 }, { time: 0.7, downsample: 1 }, { time: 1, downsample: 1 }], mix: 0.35 } } },
  outro:  { bars: 4,  kick: true,  hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: true,  drumDensity: 0.7 },
};
const SECTION_TEMPLATES_3 = {
  intro:  { bars: 6,  kick: false, hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: false, riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: true,  drumDensity: 0.5 },
  break1: { bars: 12, kick: false, hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: false, drumDensity: 0.5, fx: { wobble: { target: "filter", baseCutoffHz: 2200, rate: [{ time: 0, rate: 0.4 }, { time: 1, rate: 1.2 }], depth: [{ time: 0, depth: 0.10 }, { time: 1, depth: 0.50 }] } } },
  build1: { bars: 6,  kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 16 }, { time: 1, bits: 6 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 4 }], mix: 0.65 } } },
  drop1:  { bars: 12, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: false, riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 0.5, bits: 10 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.5, downsample: 2 }, { time: 1, downsample: 1 }], mix: 0.3 } } },
  break2: { bars: 6,  kick: false, hat: false, sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: true,  ducked: false, drumDensity: 0, fx: { wobble: { target: "filter", baseCutoffHz: 1600, rate: 1.5, depth: 0.4 } } },
  build2: { bars: 3,  kick: true,  hat: true,  sub: true,  lead: false, pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 1, bits: 5 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 5 }], mix: 0.7 } } },
  drop2:  { bars: 12, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: false, riser: false, snareRoll: false, supersaw: true,  vocal: true,  ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 12 }, { time: 0.4, bits: 8 }, { time: 0.7, bits: 12 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.4, downsample: 3 }, { time: 0.7, downsample: 1 }, { time: 1, downsample: 1 }], mix: 0.35 } } },
  outro:  { bars: 7,  kick: true,  hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: true,  drumDensity: 0.7 },
};
// CHILL mode — study-vibes minimal dance mix. Continuous bed, no
// drops, no machine guns, no riser sweeps. Bells + lead + pad + sub
// bass + hat + supersaw all running gently throughout. Long sections
// give the loops time to breathe. ~3 minutes total (135 bars × 1.3125 s).
const SECTION_TEMPLATES_3_CHILL = {
  // Intro is gentle — pad + sparse hat + slow bells only. NO kick
  // (which lands as a doubled drum in chill density and reads as
  // a glitch) and NO greeting vocal (the chill is wordless). The
  // kick enters at break1 so there's actual instrumental arrival.
  intro:   { bars: 11, kick: false, hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: false, drumDensity: 0.30 },
  break1:  { bars: 24, kick: true,  hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.55 },
  build1:  { bars: 24, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.75 },
  drop1:   { bars: 29, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.90 },
  break2:  { bars: 12, kick: true,  hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.55 },
  build2:  { bars: 0,  kick: false, hat: false, sub: false, lead: false, pad: false, piano: false, bells: false, riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: false, drumDensity: 0 },
  drop2:   { bars: 24, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.90 },
  outro:   { bars: 9,  kick: true,  hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: true,  drumDensity: 0.40 },
};
// trancepenta (chill + meter 5 dorian) — early-drop variant.
// Shorter intro/break/build so the first drop lands within the first
// 30 s of the track (@jeffrey, 2026-05-20). Total bars preserved (80)
// so the song still runs ~190 s — drop1 + drop2 each absorb the time
// shaved off the run-up.
const SECTION_TEMPLATES_5_CHILL = {
  intro:   { bars: 4,  kick: false, hat: true,  sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: false, drumDensity: 0.30 },
  break1:  { bars: 4,  kick: true,  hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.50 },
  build1:  { bars: 4,  kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.75, fx: { bitcrush: { bits: [{ time: 0, bits: 16 }, { time: 1, bits: 7 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 3 }], mix: 0.5 } } },
  drop1:   { bars: 24, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 0.92, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 0.5, bits: 10 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.5, downsample: 2 }, { time: 1, downsample: 1 }], mix: 0.25 } } },
  break2:  { bars: 4,  kick: true,  hat: true,  sub: false, lead: true,  pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: true,  drumDensity: 0.55, fx: { wobble: { target: "filter", baseCutoffHz: 1800, rate: 1.8, depth: 0.4 } } },
  build2:  { bars: 4,  kick: true,  hat: true,  sub: true,  lead: false, pad: true,  piano: false, bells: false, riser: true,  snareRoll: true,  supersaw: true,  vocal: false, ducked: true,  drumDensity: 1, fx: { bitcrush: { bits: [{ time: 0, bits: 14 }, { time: 1, bits: 5 }], downsample: [{ time: 0, downsample: 1 }, { time: 1, downsample: 5 }], mix: 0.7 } } },
  drop2:   { bars: 28, kick: true,  hat: true,  sub: true,  lead: true,  pad: true,  piano: true,  bells: true,  riser: false, snareRoll: false, supersaw: true,  vocal: false, ducked: true,  drumDensity: 1.0, fx: { bitcrush: { bits: [{ time: 0, bits: 12 }, { time: 0.4, bits: 8 }, { time: 0.7, bits: 12 }, { time: 1, bits: 14 }], downsample: [{ time: 0, downsample: 1 }, { time: 0.4, downsample: 3 }, { time: 0.7, downsample: 1 }, { time: 1, downsample: 1 }], mix: 0.30 } } },
  outro:   { bars: 8,  kick: false, hat: false, sub: false, lead: false, pad: true,  piano: false, bells: true,  riser: false, snareRoll: false, supersaw: false, vocal: false, ducked: false, drumDensity: 0 },
};
const SECTION_TEMPLATES = isChill && isWaltz
  ? SECTION_TEMPLATES_3_CHILL
  : (isChill && !isWaltz)
  ? SECTION_TEMPLATES_5_CHILL
  : isWaltz ? SECTION_TEMPLATES_3 : SECTION_TEMPLATES_4;

let SECTIONS;
if (SECTION === "full") SECTIONS = ["intro", "break1", "build1", "drop1", "break2", "build2", "drop2", "outro"];
else if (SECTION === "drop")  SECTIONS = ["drop1"];
else if (SECTION === "break") SECTIONS = ["break1"];
else if (SECTION === "build") SECTIONS = ["build1"];
else { console.error(`unknown section '${SECTION}'`); process.exit(1); }
// Drop zero-bar sections from the layout (chill mode has no build2).
SECTIONS = SECTIONS.filter((s) => SECTION_TEMPLATES[s].bars > 0);

const sectionBars = SECTIONS.map((s) => SECTION_TEMPLATES[s].bars);
const naturalBars = sectionBars.reduce((a, b) => a + b, 0);
const TOTAL_BARS = BARS_FLAG !== null ? BARS_FLAG : naturalBars;

const beatSec  = 60 / BPM;
const barSec   = beatSec * METER;
// Opening prefix — boot melody chime + greeting voice play
// SIMULTANEOUSLY at t=0.05. Trimmed to 2.7 s so music + vocal enter
// while the greeting's "Angeles" tail is still resolving — the
// sniper bang lands on a 16th-note hihat tick just inside the
// overlap, making the entry punchier.
// In chill mode the opening boot-sonification (startup melody + the
// beep→hat blend) is removed, so there's no narrative prefix to host.
// Start the music almost immediately — just a hair of lead-in for the
// --master fade — instead of leaving ~2.7 s of dead air at the top of
// the single.
const OPENING_PREFIX_SEC = isChill ? 0.25 : 2.7;

// Tempo accelerando — chill ramps LINEARLY from the base BPM up to a
// faster end BPM across the whole track (the track's "linear swing →
// variable bpm": it's a dance track that should drive harder as it
// goes). Deterministic — a pure function of bar index, so struct.json /
// section boundaries / video alignment all regenerate consistently.
// Non-chill: BPM_END === BPM → constant tempo → trancenwaltz stays
// byte-identical.
const BPM_END = isChill ? 180 : BPM; // chill = exponential pacing top (for the log)
const barDurSec   = []; // per-bar duration (accelerating in chill)
const barStartRel = []; // music-relative start of each bar
                        // (length TOTAL_BARS+1 — last entry = musicSec)
{
  let acc = 0;
  for (let i = 0; i < TOTAL_BARS; i++) {
    const fr = TOTAL_BARS > 1 ? i / (TOTAL_BARS - 1) : 0;
    let bpmI;
    if (isChill) {
      // The WHOLE track gradually SPEEDS UP over time — a monotonic
      // dance-club accelerando (jas: "can the whole track speed up a
      // bit more / over time"). The single ~3 s mid-track moment is a
      // turntable SCRATCH applied in the audio domain (see the scratch
      // pass before the raw write), NOT a bpm move — a bar is too
      // coarse (~1.4 s) to scratch with per-bar tempo.
      // Explicit --bpm → STEADY at that tempo; else the default
      // 129→155 accelerando (keeps trancenwaltzi unchanged).
      const START_BPM = flags.bpm ? Number(flags.bpm) : 129;
      const END_BPM = flags.bpm ? Number(flags.bpm) : 155;
      bpmI = START_BPM + (END_BPM - START_BPM) * fr;

    } else {
      bpmI = BPM; // constant — trancenwaltz byte-identical
    }
    barStartRel.push(acc);
    const d = (60 / bpmI) * METER;
    barDurSec.push(d);
    acc += d;
  }
  barStartRel.push(acc); // final boundary = musicSec
}
const musicSec = barStartRel[TOTAL_BARS] ?? (barSec * TOTAL_BARS);
const totalSec = musicSec + OPENING_PREFIX_SEC;
const tailSec  = 0;
const totalSamples = Math.ceil((totalSec + tailSec) * SAMPLE_RATE);

console.log(`→ ${isWaltz ? "trancewaltz" : "trance"} · ${BPM}${BPM_END !== BPM ? ` exp-pacing⇅${BPM_END}×2` : ""} bpm · ${METER}/4 · ${SCALE_NAME} (root ${ROOT_MIDI}) · ${TOTAL_BARS} bars · ${totalSec.toFixed(1)}s · sections [${SECTIONS.join(", ")}]`);

// Bar → section info, with cumulative start time so fx envelopes
// know the local-zero of their section.
const sectionRanges = [];
{
  let accBars = 0;
  for (const name of SECTIONS) {
    const len = SECTION_TEMPLATES[name].bars;
    sectionRanges.push({
      name,
      startBar: accBars,
      endBar: accBars + len,
      // startSec/endSec include the opening prefix so any downstream
      // dispatch (machine guns, drop impact, screams, fx envelopes,
      // dynamic envelope) lands at wall-clock times correctly.
      startSec: barStartRel[accBars] + OPENING_PREFIX_SEC,
      endSec: barStartRel[accBars + len] + OPENING_PREFIX_SEC,
      template: SECTION_TEMPLATES[name],
    });
    accBars += len;
  }
}
function sectionAtBar(barIdx) {
  for (const r of sectionRanges) {
    if (barIdx < r.endBar) return { name: r.name, localBar: barIdx - r.startBar, sectionLen: r.endBar - r.startBar };
  }
  const last = sectionRanges[sectionRanges.length - 1];
  return { name: last.name, localBar: 0, sectionLen: last.endBar - last.startBar };
}

// ── buffers ───────────────────────────────────────────────────────────
const dryBuf  = new Float32Array(totalSamples); // kick, hat, snare roll, lead, riser
const duckBuf = new Float32Array(totalSamples); // pad (sidechained)
const bassBuf = new Float32Array(totalSamples); // sub bass — its own bus so we can wobble it deep without affecting the pad
const mgBuf   = new Float32Array(totalSamples); // machine-gun bus — bypasses bitcrush so depth survives
const sfxDryBuf = new Float32Array(totalSamples); // dry SFX bus — opening sniper, skips the mgBuf distance reverb
const sawBuf  = new Float32Array(totalSamples); // supersaw bus — gets fast flange before it joins the sidechained duckBuf
const shutdownBuf = new Float32Array(totalSamples); // ending bus — bye jeffrey + shutdown chime + ending hihats. NOT tape-stopped so the ending mirrors the opening: voice + chime + hats stay at full pitch while the bed slows.
const chillBellBuf = new Float32Array(totalSamples); // chill mode: super-low drawn-out bells routed here so we can flanger them
const kickTimes = [];

// ── lead variation state — subtle, evolving, microtonal ──────────────
// Random-walk drift across the track so no two loops are identical.
// Per-step: cent offset wanders (Brownian ±3¢, clamped ±25¢), small
// chance of ±1 scale-step shift, tiny chance of a rest.
const leadRng = makeRng(SEED_STR + ":lead-evolve");
let leadCentsDrift = 0;
// Bunny-bow comp state: deterministic rng + the last note it played
// (so it can voice-lead by nearest tone — the "bounce").
const bunnyRng = makeRng(SEED_STR + ":bunny-bow");
let bunnyPrevMidi = null;
function nextLeadVariation() {
  leadCentsDrift += (leadRng() - 0.5) * 6;
  if (leadCentsDrift > 25) leadCentsDrift = 25;
  if (leadCentsDrift < -25) leadCentsDrift = -25;
  let stepShift = 0;
  const r = leadRng();
  if (r < 0.06) stepShift = leadRng() < 0.5 ? 1 : -1;
  else if (r < 0.10) stepShift = leadRng() < 0.5 ? 2 : -2;
  const skip = leadRng() < 0.03;
  return { cents: leadCentsDrift, stepShift, skip };
}

// ── synths over the sound.synth bus ───────────────────────────────────
function fireDrum(target, startSec, letter, opts = {}) {
  const sound = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
  playPercussion(sound, letter, { phase: "both", ...opts });
}

// ac-native boot melody — triangle C5→E5→G5 ascending (matches
// audio.c:audio_ready_melody) + a descending GLIDE tail that pitches
// down from G5 to A4 so the chime resolves into the square arp's
// starting note instead of stopping abruptly. The glide is a stepped
// exponential sweep emitting many tiny triangle voices.
function fireBootMelody(target, startSec, gain = 1.0) {
  const notes = [
    { tone: 523.25, dur: 0.15, vol: 0.70 }, // C5
    { tone: 659.25, dur: 0.15, vol: 0.70 }, // E5
    { tone: 783.99, dur: 0.20, vol: 0.80 }, // G5
  ];
  // Emit one SFX event per note so the visualizer can render a
  // fullscreen dink per beep (not just a single span for the whole
  // melody).
  {
    let bt = startSec;
    let idx = 0;
    for (const n of notes) {
      events.sfx.push({ t: bt, name: `boot-beep-${++idx}`, dur: n.dur, point: true });
      bt += n.dur + 0.060;
    }
  }
  let t = startSec;
  for (const n of notes) {
    const v = makeBufferSynth(target, t, SAMPLE_RATE, noiseRng);
    v.synth({
      type: "triangle",
      tone: n.tone,
      duration: n.dur,
      volume: gain * n.vol,
      attack: 0.003,
      decay: n.dur * 0.6,
    });
    t += n.dur + 0.060; // 60 ms gap between notes
  }
  // (No descending glide — the abrupt dip + climb made it sound like
  // two separate events. The pre-roll arp continues the boot melody
  // timbre (triangle, same C5-E5-G5 pitch range) so they read as
  // ONE voice that gradually morphs into the square wave by music
  // entry.)
}

// ac-native shutdown sound — triangle G5→E5→C5 descending. Matches
// audio.c:audio_shutdown_sound(). Same envelope/gap as boot melody.
function fireShutdownMelody(target, startSec, gain = 1.0) {
  const notes = [
    { tone: 783.99, dur: 0.15, vol: 0.70 }, // G5
    { tone: 659.25, dur: 0.15, vol: 0.70 }, // E5
    { tone: 523.25, dur: 0.20, vol: 0.80 }, // C5
  ];
  // One SFX event per descending note so the visualizer can render
  // a fullscreen "dink" per beep on the way out.
  {
    let bt = startSec;
    let idx = 0;
    for (const n of notes) {
      events.sfx.push({ t: bt, name: `shutdown-beep-${++idx}`, dur: n.dur, point: true });
      bt += n.dur + 0.060;
    }
  }
  let t = startSec;
  for (const n of notes) {
    const v = makeBufferSynth(target, t, SAMPLE_RATE, noiseRng);
    v.synth({
      type: "triangle",
      tone: n.tone,
      duration: n.dur,
      volume: gain * n.vol,
      attack: 0.003,
      decay: n.dur * 0.6,
    });
    t += n.dur + 0.060;
  }
}

// Whistle — approximation of the ac-native Cook/STK physically-
// modeled flute (audio.c:generate_whistle_sample). The full waveguide
// model would be a major port; this version captures the same
// perceptual character: pure tonal core + 5 Hz vibrato (±3 %) +
// breath noise rider. Emits many sustained sine segments stepping
// through the vibrato curve so the pitch wobbles like the real
// thing.
function fireWhistle(target, startSec, midi, durSec, gain = 1.0) {
  const baseFreq = 440 * Math.pow(2, (midi - 69) / 12);
  // For short notes (≤ 0.30 s) fire a single voice — vibrato barely
  // develops over such a short duration and the segmented-stepping
  // approach smeared the attack. For longer notes, use vibrato
  // segments stepping at a real 5 Hz rate.
  if (durSec <= 0.30) {
    const v = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sine",     tone: baseFreq,        duration: durSec, volume: gain * 0.55, attack: 0.005, decay: durSec * 0.85 });
    v.synth({ type: "sine",     tone: baseFreq * 2,    duration: durSec, volume: gain * 0.18, attack: 0.008, decay: durSec * 0.85 });
    v.synth({ type: "sine",     tone: baseFreq * 3,    duration: durSec, volume: gain * 0.08, attack: 0.010, decay: durSec * 0.85 });
    v.synth({ type: "noise",    tone: baseFreq * 1.5,  duration: durSec, volume: gain * 0.28, attack: 0.005, decay: durSec * 0.85 });
    v.synth({ type: "noise",    tone: 4500,            duration: durSec, volume: gain * 0.10, attack: 0.005, decay: durSec * 0.85 });
    return;
  }
  // Long-note path — segmented vibrato (5 Hz, ±3 % pitch).
  const segDur = 0.045; // fixed-size segments → real-time vibrato rate
  const segments = Math.max(3, Math.floor(durSec / segDur));
  const VIBRATO_HZ = 5;
  for (let i = 0; i < segments; i++) {
    const sf = i / segments;
    const elapsedSec = sf * durSec;
    const vibrato = Math.sin(elapsedSec * 2 * Math.PI * VIBRATO_HZ) * 0.03;
    const tone = baseFreq * (1 + vibrato);
    const stepStart = startSec + elapsedSec;
    const env = Math.sin(sf * Math.PI);
    const v = makeBufferSynth(target, stepStart, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sine",     tone,               duration: segDur + 0.04, volume: gain * 0.40 * env, attack: 0.010, decay: segDur * 0.9 });
    v.synth({ type: "sine",     tone: tone * 2,     duration: segDur + 0.04, volume: gain * 0.10 * env, attack: 0.012, decay: segDur * 0.9 });
    v.synth({ type: "sine",     tone: tone * 3,     duration: segDur + 0.04, volume: gain * 0.06 * env, attack: 0.014, decay: segDur * 0.9 });
    v.synth({ type: "noise",    tone: tone * 1.5,   duration: segDur,        volume: gain * 0.25 * env, attack: 0.008, decay: segDur * 0.85 });
    v.synth({ type: "noise",    tone: 4500,         duration: segDur,        volume: gain * 0.10 * env, attack: 0.008, decay: segDur * 0.85 });
  }
}

// Mosquito / drone-bird — a thin detuned sawtooth "insect" that
// wanders in pitch (slow random walk), buzzes (fast ~8-13 Hz wing
// vibrato), flutters in amplitude (comes near / drifts off) and
// circles the stereo field. Used in the chill mix instead of the
// sustained supersaw wall — a small triad of these zip around like
// annoying-but-musical flies.
// fireTheremin — long sustained theremin voice for the Odyssey melody.
// Stable envelope (fast attack + held sustain + slow release), slow
// vibrato, optional pitch glide to the NEXT note in the last 22% of the
// duration, modest stereo width via two detuned saws + a centred sine
// layer. Designed for held melody notes (1-8 bars) — unlike fireMosquito
// which uses a half-sine envelope tuned for short chirps.
function fireTheremin(target, startSec, midi, durSec, gain = 1.0, nextMidi = null) {
  const segDur = 0.04;
  const segments = Math.max(8, Math.floor(durSec / segDur));
  const vibHz = 3.3; // slower vibrato per @jeffrey (was 5.5 — too fast)
  const glideStart = nextMidi !== null ? Math.floor(segments * 0.78) : segments;
  const attackSec = 0.10;
  const releaseSec = 0.25;
  for (let i = 0; i < segments; i++) {
    const sf = i / segments;
    const elapsed = sf * durSec;
    let env;
    if (elapsed < attackSec) env = elapsed / attackSec;
    else if (elapsed > durSec - releaseSec) env = Math.max(0, (durSec - elapsed) / releaseSec);
    else env = 1;
    const vib = Math.sin(elapsed * 2 * Math.PI * vibHz) * 0.018; // ~1.8% pitch wobble
    let curMidi = midi;
    if (i >= glideStart && nextMidi !== null) {
      const u = (i - glideStart) / Math.max(1, segments - glideStart);
      const smooth = u * u * (3 - 2 * u);
      curMidi = midi + (nextMidi - midi) * smooth;
    }
    const tone = 440 * Math.pow(2, (curMidi - 69) / 12) * (1 + vib);
    // Harmonic-bloom envelope — 2nd + 3rd harmonics swell in during the
    // middle of the note ("more harmonics at times"). Hann-like window
    // centred at note middle so the timbre opens up + closes back to
    // pure tone at the edges.
    const hMid = 4 * sf * (1 - sf); // 0 at sf=0/1, peak 1.0 at sf=0.5
    const harmGain = Math.max(0, hMid - 0.15) / 0.85; // gates softer edges
    const v = makeBufferSynth(target, startSec + elapsed, SAMPLE_RATE, noiseRng);
    // Fundamental — two detuned saws + a sine for theremin warmth.
    v.synth({ type: "sawtooth", tone,            duration: segDur + 0.02, volume: gain * 0.70 * env, attack: 0.003, decay: segDur * 0.95, pan: -0.18 });
    v.synth({ type: "sawtooth", tone: tone * 1.006, duration: segDur + 0.02, volume: gain * 0.45 * env, attack: 0.003, decay: segDur * 0.95, pan:  0.18 });
    v.synth({ type: "sine",     tone,            duration: segDur + 0.02, volume: gain * 0.50 * env, attack: 0.003, decay: segDur * 0.95, pan:  0 });
    // 2nd harmonic (octave up) blooming in the middle of the note.
    v.synth({ type: "sine",     tone: tone * 2,  duration: segDur + 0.02, volume: gain * 0.28 * env * harmGain, attack: 0.003, decay: segDur * 0.95, pan: -0.10 });
    // 3rd harmonic (octave + 5th, perfect twelfth) for ring/sparkle.
    v.synth({ type: "sine",     tone: tone * 3,  duration: segDur + 0.02, volume: gain * 0.18 * env * harmGain, attack: 0.003, decay: segDur * 0.95, pan:  0.10 });
  }
}

function fireMosquito(target, startSec, midi, durSec, gain = 1.0, panBase = 0) {
  const baseFreq = 440 * Math.pow(2, (midi - 69) / 12);
  const segDur = 0.05;
  const segments = Math.max(4, Math.floor(durSec / segDur));
  const vibHz = 8 + noiseRng() * 5;            // 8–13 Hz buzzy wingbeat
  const panRate = 0.5 + noiseRng() * 1.6;      // how fast it circles
  const panPhase = noiseRng() * Math.PI * 2;
  let drift = 0;                                // slow pitch walk (cents)
  for (let i = 0; i < segments; i++) {
    const sf = i / segments;
    const elapsed = sf * durSec;
    drift += (noiseRng() - 0.5) * 22;
    if (drift > 120) drift = 120;
    if (drift < -120) drift = -120;
    const vib = Math.sin(elapsed * 2 * Math.PI * vibHz) * 0.04;
    const tone = baseFreq * Math.pow(2, drift / 1200) * (1 + vib);
    const flutter = 0.5 + 0.5 * Math.abs(Math.sin(elapsed * 2 * Math.PI * (vibHz * 0.5)));
    const env = Math.sin(sf * Math.PI);         // swell in, fade out
    const pan = Math.max(-1, Math.min(1,
      panBase + 0.9 * Math.sin(elapsed * panRate * 2 * Math.PI + panPhase)));
    const v = makeBufferSynth(target, startSec + elapsed, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sawtooth", tone, duration: segDur + 0.03, volume: gain * 0.5 * env * flutter, attack: 0.004, decay: segDur * 0.9, pan });
    v.synth({ type: "sawtooth", tone: tone * 1.004, duration: segDur + 0.03, volume: gain * 0.30 * env * flutter, attack: 0.004, decay: segDur * 0.9, pan: -pan });
  }
}

// Meditation gong — a high struck sine bell with a very long, slowly
// dissipating ring, trailing into a white-noise "wave" that swells,
// crests and zippers back out (a tone sweep up-then-down across a
// noise band). Fired every ~8 bars in the chill mix.
// Simulated CHINESE GONG / tam-tam (no real gong sample in the repo).
// A LOW fundamental + a stack of INHARMONIC partials (non-integer
// ratios → metallic clang), each slightly detuned for shimmer/beating,
// with a BLOOM envelope: the higher partials swell IN over ~0.6 s as a
// real gong "opens up", then a very long shimmering decay, with a dark
// metallic noise wash under the bloom. `deep` = bigger / lower / longer.
function fireGong(startSec, midi, gain = 1.0, deep = false, reverse = false) {
  const f = 440 * Math.pow(2, (midi - 69) / 12) * (deep ? 0.5 : 1.0);
  const dur = deep ? 10.0 : 7.5;
  const seg = 0.06;
  const segN = Math.floor(dur / seg);
  const partials = [
    { r: 1.00, g: 1.00, blo: 0.05 },
    { r: 1.48, g: 0.62, blo: 0.18 },
    { r: 2.03, g: 0.70, blo: 0.30 },
    { r: 2.74, g: 0.48, blo: 0.45 },
    { r: 3.46, g: 0.40, blo: 0.55 },
    { r: 4.21, g: 0.30, blo: 0.65 },
    { r: 5.40, g: 0.24, blo: 0.75 },
    { r: 6.79, g: 0.18, blo: 0.85 },
  ];
  for (let s = 0; s < segN; s++) {
    const u = s / segN;
    // reverse = the swell runs backward (quiet → bloom → strike): the
    // envelope timeline is time-flipped while segments stay in place.
    const ur = reverse ? 1 - u : u;
    const tt = ur * dur;
    const t0 = startSec + s * seg;
    const decay = Math.pow(reverse ? u : 1 - u, 1.5); // long ring / reverse swell
    const v = makeBufferSynth(dryBuf, t0, SAMPLE_RATE, noiseRng);
    for (let p = 0; p < partials.length; p++) {
      const P = partials[p];
      const peak = P.blo * (deep ? 1.4 : 1.0);     // higher partials bloom later
      const bloom = Math.exp(-Math.pow((tt - peak) / (peak + 0.5), 2) * 1.3);
      const det = 1 + (p % 2 === 0 ? 1 : -1) * 0.0016 * (p + 1); // beating
      const amp = gain * P.g * decay * bloom * (p === 0 ? 1.0 : 0.8);
      if (amp < 0.0008) continue;
      v.synth({ type: "sine", tone: f * P.r * det, duration: 0.12, volume: amp, attack: 0.012, decay: 0.11 });
    }
    const wash = Math.exp(-Math.pow((tt - 0.4) / 0.9, 2)) * decay;
    if (wash > 0.01) {
      const w = makeBufferSynth(dryBuf, t0, SAMPLE_RATE, noiseRng);
      w.synth({ type: "noise", tone: 320 + 380 * (1 - u), duration: seg + 0.03, volume: gain * 0.05 * wash, attack: 0.01, decay: seg });
    }
  }
}

// iPhone Taptic Engine emulation. Apple's haptic actuator is a linear
// resonant actuator tuned to ~150 Hz; iOS impact/selection haptics are
// very short transients around that pitch. We synth a ~150 Hz damped
// sine + a faint crisp tick. kinds: "tap" (single), "double" (peek/pop),
// "notify" (triple). Audio-only — a felt-in-time tactile pulse layer.
function fireHaptic(target, startSec, gain = 1.0, kind = "tap") {
  const F = 150; // Apple Taptic resonant frequency
  const one = (t0, amp) => {
    const v = makeBufferSynth(target, t0, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sine",  tone: F,        duration: 0.016, volume: gain * 0.9 * amp,  attack: 0.0004, decay: 0.014 });
    v.synth({ type: "sine",  tone: F * 2,    duration: 0.010, volume: gain * 0.25 * amp, attack: 0.0004, decay: 0.009 });
    v.synth({ type: "noise", tone: 2200,     duration: 0.004, volume: gain * 0.18 * amp, attack: 0.0002, decay: 0.0035 }); // crisp tick
  };
  if (kind === "double") { one(startSec, 0.85); one(startSec + 0.072, 1.0); }
  else if (kind === "notify") { one(startSec, 0.8); one(startSec + 0.09, 0.8); one(startSec + 0.18, 0.95); }
  else one(startSec, 1.0);
}

// Sonic-boom VORTEX. mode "boom": a deep impact + a descending
// high→low whoosh tail (the crack). mode "intake": an accelerating,
// rising "sucky" vortex that climaxes exactly at startSec+durSec — so
// in a perfect loop the end-intake wraps straight into the start-boom.
// BYTEBEAT VORTEX — rawstyle. A swept bytebeat counter (8-bit integer
// xor/shift screech) hard-clipped through tanh: aliased + distorted by
// construction = wicked/gnarly. "boom" = a loud descending churn-impact;
// "intake" = an accelerating swelling suck that climaxes at the seam.
function fireVortex(target, startSec, durSec, mode) {
  const SR = SAMPLE_RATE;
  const oS = Math.floor(startSec * SR);
  const N = Math.floor(durSec * SR);
  for (let i = 0; i < N; i++) {
    const u = i / N;
    // the vortex sweep: boom churns fast→slow, intake accelerates up
    const k = mode === "boom"
      ? 1 + 26 * Math.pow(1 - u, 1.6)   // slower one-way zipper roll
      : 1 + 96 * Math.pow(u, 2.6);
    const t = (i * k * 0.5) | 0;                       // swept bytebeat time
    // gnarly layered xor/shift bytebeat (0..255)
    const b = ((t * (3 + ((t >> 9) & 7))) ^ (t >> 4) ^ (t >> 7) | (t >> 3)) & 255;
    const sub = ((t >> 2) & 64) ? 1 : -1;              // crude square sub for weight
    let s = ((b / 128) - 1) * 0.8 + sub * 0.25;
    const env = mode === "boom"
      ? Math.min(1, u / 0.06) * Math.pow(1 - u, 1.0)    // soft rise → long unidirectional roll-out
      : (0.05 + 0.95 * Math.pow(u, 2.0));               // swell → climax at seam
    s = Math.tanh(s * env * 2.0) * 0.7;                 // rawstyle clip (a touch tamer)
    const d = oS + i;
    if (d >= 0 && d < target.length) target[d] += s * 0.22; // soft (kawaii — not gnarly/gothic)
  }
}

// (Removed fireImessageDing — the iOS-style two-tone bell with
// wiggle tail. Drops now use an inlined CHORD version that fires
// 4 chord-tones with the same wiggle envelope. The single-tone
// helper was redundant after that refactor.)

// Sniper SFX — opening shot reads as an actual GUNSHOT, not a bell.
// Sharp transient + supersonic crack + descending noise tail. A
// short tonal body underneath ties it harmonically to the track
// (root A), but the bell-sustain has been removed so it doesn't
// bleed over the intro vocal.
//
// Routed to sfxDryBuf so the mgBuf distance-reverb chain doesn't
// drown the shot.
function fireSniper(target, startSec, gain = 1.0) {
  // Snare body — chest impact, pitched down.
  fireDrum(target, startSec, "d", { volume: gain * 1.5, pitchFactor: 0.45 });

  const body = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);

  // Deep boom — short low-frequency body.
  body.synth({ type: "sine", tone: 42, duration: 0.22, volume: gain * 1.10, attack: 0.002, decay: 0.18 });
  // Supersonic crack — bangin' top transient, snappier.
  body.synth({ type: "triangle", tone: 2400, duration: 0.040, volume: gain * 1.15, attack: 0.0005, decay: 0.035 });
  // Extra-high snap layer — short 4 kHz triangle whip that gives the
  // shot its "snap" — the cracking transient on top of the boom.
  body.synth({ type: "triangle", tone: 4200, duration: 0.020, volume: gain * 0.95, attack: 0.0003, decay: 0.018 });
  // Tiny noise pop at the very start for the gunpowder "snap".
  body.synth({ type: "noise",    tone: 6000, duration: 0.012, volume: gain * 0.85, attack: 0.0002, decay: 0.011 });
  // A-minor chord-tone sine + triangle ring (short — 0.5–0.8 s) so
  // the shot is harmonically grounded but clears out before the
  // intro vocal proper kicks in.
  body.synth({ type: "sine",     tone: 110, duration: 0.80, volume: gain * 0.30, attack: 0.005, decay: 0.78 });
  body.synth({ type: "triangle", tone: 220, duration: 0.70, volume: gain * 0.20, attack: 0.008, decay: 0.68 });
  body.synth({ type: "sine",     tone: 330, duration: 0.60, volume: gain * 0.15, attack: 0.012, decay: 0.58 });

  // Descending pitched-noise tail — 5 bursts over ~0.18 s, gives the
  // sniper its whip-crack tail without an over-long ring.
  for (let i = 0; i < 5; i++) {
    const t = startSec + i * 0.035;
    const tail = makeBufferSynth(target, t, SAMPLE_RATE, noiseRng);
    tail.synth({
      type: "noise",
      tone: 3500 - i * 480,
      duration: 0.06,
      volume: gain * 0.55 * Math.exp(-i * 0.40),
      attack: 0.003,
      decay: 0.055,
    });
  }
}

// Drop impact — overlapping sine bass cluster + 30 Hz boom + pitched-
// down kick transient. Fires once at the start of each drop section for
// a massive, deep "punch" the regular per-beat sub bass can't deliver
// alone. Lives on the SFX bus so the master section dyn envelope
// doesn't tame it.
//
// Voices stack across three octaves at the chord root, plus a 5th and
// a sub-detuned root for thickening. The sustain lets the impact ring
// for ~1 second behind the new section before settling into the regular
// sub-bass off-beat pattern.
function fireDropImpact(target, startSec, midi, gain = 1.0) {
  const baseFreq = 440 * Math.pow(2, (midi - 69) / 12);
  const sound = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
  // 7 overlapping sine voices spanning 4 octaves around the root.
  const voices = [
    { freq: baseFreq * 0.25,                                  g: 1.60, dec: 1.80 }, // -2 oct
    { freq: baseFreq * 0.50,                                  g: 1.35, dec: 1.50 }, // -1 oct
    { freq: baseFreq * 0.50 * Math.pow(2, 5 / 1200),          g: 0.90, dec: 1.25 }, // -1 oct detuned
    { freq: baseFreq * 1.00,                                  g: 1.10, dec: 1.10 }, // root
    { freq: baseFreq * 1.00 * Math.pow(2, -5 / 1200),         g: 0.70, dec: 1.05 }, // root detuned
    { freq: baseFreq * 1.50,                                  g: 0.55, dec: 0.85 }, // 5th
    { freq: baseFreq * 2.00,                                  g: 0.38, dec: 0.65 }, // +1 oct
  ];
  for (const v of voices) {
    sound.synth({
      type: "sine",
      tone: v.freq,
      duration: 2.0,
      volume: gain * v.g * 0.65,
      attack: 0.004,
      decay: v.dec,
    });
  }
  // BOOM — sustained 25 Hz body, longer + louder than before.
  sound.synth({
    type: "sine",
    tone: 25,
    duration: 1.6,
    volume: gain * 1.55,
    attack: 0.001,
    decay: 1.30,
  });
  // Second sub-boom at 45 Hz overlapping for double-thump weight.
  sound.synth({
    type: "sine",
    tone: 45,
    duration: 1.0,
    volume: gain * 1.20,
    attack: 0.002,
    decay: 0.85,
  });
  // SUPERSONIC pitch-sweep — a downward whoosh from 8 kHz to 80 Hz
  // over 350 ms. Many overlapping sine voices step through the
  // exponential sweep so the drop entry has the classic bomb-falling
  // tail BEFORE the boom hits.
  const sweepSteps = 22;
  for (let i = 0; i < sweepSteps; i++) {
    const f = i / sweepSteps;
    const tone = 8000 * Math.pow(80 / 8000, f); // 8 kHz → 80 Hz expo
    const stepStart = startSec + f * 0.35;
    const stepDur = 0.08;
    const sweepVoice = makeBufferSynth(target, stepStart, SAMPLE_RATE, noiseRng);
    sweepVoice.synth({
      type: "sine",
      tone,
      duration: stepDur,
      volume: gain * 0.65 * (0.4 + f * 0.6),
      attack: 0.001,
      decay: stepDur * 0.8,
    });
    // Triangle layer adds harmonic content so the sweep cuts through.
    sweepVoice.synth({
      type: "triangle",
      tone,
      duration: stepDur,
      volume: gain * 0.32 * (0.4 + f * 0.6),
      attack: 0.001,
      decay: stepDur * 0.8,
    });
  }
  // Triangle transient for the snap on top.
  sound.synth({
    type: "triangle",
    tone: 600,
    duration: 0.05,
    volume: gain * 0.50,
    attack: 0.001,
    decay: 0.045,
  });
  // Pitched-down kick at t=0 for the chest impact (louder).
  fireDrum(target, startSec, "c", { volume: gain * 1.9, pitchFactor: 0.50 });
}

// Machine-gun SFX — pitched melodic chromatic-descending pattern on
// the beat. Each hit is a staccato snare+sub-boom with a melodic
// triangle on top that walks down a chord-tone arpeggio, giving the
// gunfire a little "lil melody" instead of a random noise wash.
// Lives on its own bus so master fx don't shred the depth.
//
// Hits are snapped to the nearest 16th-note subdivision of the kick
// grid, so they feel on-the-beat rather than randomly scattered.
function fireMachineGun(target, startSec, durSec, gain = 1.0) {
  // 16th-note grid relative to the global tempo — guns sit on the
  // beat instead of free-running.
  const sixteenthSec = (60 / BPM) / 4;
  const total = Math.max(2, Math.floor(durSec / sixteenthSec));
  const beatIdx0 = Math.floor(startSec / sixteenthSec);
  const barIdx = Math.floor(startSec / barSec);
  const chordDeg = progressionAt(barIdx);
  // Pattern uses SCALE DEGREES (not semitones), so the melodic "bleep"
  // line stays diatonic to the minor scale — no more chromatic
  // neighbour notes that registered as "half-step off" against the
  // rest of the track. Values are degree offsets from the chord root,
  // resolved via scaleNoteMidi.
  const PATTERN_DEG = [0, 4, 2, 0, 4, 2, 7, 4, 0, 4, 2, 7, 4, 2, 0, -3];
  const fadeInBullets = Math.max(2, Math.floor(total * 0.35));
  for (let i = 0; i < total; i++) {
    const t = startSec + i * sixteenthSec;
    const deg = PATTERN_DEG[(beatIdx0 + i) % PATTERN_DEG.length];
    // Tight per-bullet pitch variation — ±15¢ so bullets feel pitched-
    // around their target note WITHOUT drifting into another scale
    // degree (which produced the previous half-step-off feeling).
    const centsJitter = (noiseRng() - 0.5) * 0.30;
    const noteMidi = scaleNoteMidi(chordDeg + deg, 0) + centsJitter;
    const noteFreq = 440 * Math.pow(2, (noteMidi - 69) / 12);
    // Harmony — a chord-tone 5th above (degree +4 in scale terms).
    const harmMidi = scaleNoteMidi(chordDeg + deg + 4, 0);
    const harmFreq = 440 * Math.pow(2, (harmMidi - 69) / 12);
    const onBeat = ((beatIdx0 + i) % 4) === 0;
    const fadeMul = i < fadeInBullets ? 0.3 + 0.7 * (i / fadeInBullets) : 1.0;
    const vol = (onBeat ? 0.95 : 0.65) * gain * fadeMul;
    // Quiet chest-impact snare — distant feel.
    fireDrum(target, t, "d", { volume: vol * 0.20, pitchFactor: 0.28 + (noiseRng() - 0.5) * 0.06 });
    const sound = makeBufferSynth(target, t, SAMPLE_RATE, noiseRng);
    // Sub-boom 2 octaves below the melodic note.
    sound.synth({
      type: "sine",
      tone: noteFreq * 0.25,
      duration: sixteenthSec * 0.85,
      volume: vol * 1.10,
      attack: 0.003,
      decay: sixteenthSec * 0.80,
    });
    // MAIN melodic triangle ping — boosted to cut through.
    sound.synth({
      type: "triangle",
      tone: noteFreq,
      duration: sixteenthSec * 0.55,
      volume: vol * 0.85,
      attack: 0.002,
      decay: sixteenthSec * 0.50,
    });
    // Harmony — perfect 5th up, sustained slightly less than main.
    sound.synth({
      type: "triangle",
      tone: harmFreq,
      duration: sixteenthSec * 0.50,
      volume: vol * 0.50,
      attack: 0.003,
      decay: sixteenthSec * 0.45,
    });
    // Octave-up shimmer on downbeats only.
    if (onBeat) {
      sound.synth({
        type: "triangle",
        tone: noteFreq * 2,
        duration: sixteenthSec * 0.30,
        volume: vol * 0.42,
        attack: 0.001,
        decay: sixteenthSec * 0.25,
      });
    }
    // ECHO — a delayed copy of the melodic ping a 16th later, at
    // lower gain, gives the bleeps a spatial bounce / call-and-
    // response feel.
    const echoT = t + sixteenthSec * 1.5;
    const echo = makeBufferSynth(target, echoT, SAMPLE_RATE, noiseRng);
    echo.synth({
      type: "triangle",
      tone: noteFreq,
      duration: sixteenthSec * 0.45,
      volume: vol * 0.35,
      attack: 0.002,
      decay: sixteenthSec * 0.42,
    });
  }
}

// Sub bass — wavetype morph + harmonies. Each hit fires:
//   - root @ blended triangle/square crossfade (morphT 0..1)
//   - 5th up @ sine (harmony layer, lower gain)
//   - octave up @ sine (sky harmony, even lower)
// morphT progresses across the track so the bass timbre evolves from
// warm (triangle) → harsh (square) → back. All voices are native via
// sound.synth({type:"..."}) — port to fedac/native unchanged.
function fireSubBass(target, startSec, midi, durSec, gain, morphT = 0, harmonies = true) {
  const sound = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
  const freq = 440 * Math.pow(2, (midi - 69) / 12);
  // Crossfade triangle ↔ square. morphT clamped 0..1.
  const t = Math.max(0, Math.min(1, morphT));
  const triMix = Math.cos(t * Math.PI * 0.5);     // 1 → 0
  const sqMix  = Math.sin(t * Math.PI * 0.5);     // 0 → 1
  if (triMix > 0.01) {
    sound.synth({ type: "triangle", tone: freq, duration: durSec, volume: gain * triMix,        attack: 0.005, decay: durSec * 0.50 });
  }
  if (sqMix > 0.01) {
    // Square is loud — knock it down so it doesn't dominate the morph.
    sound.synth({ type: "square",   tone: freq, duration: durSec, volume: gain * sqMix * 0.45,  attack: 0.005, decay: durSec * 0.50 });
  }
  if (harmonies) {
    const fifth = freq * Math.pow(2, 7 / 12);   // perfect 5th up
    const oct   = freq * 2;                      // octave up
    sound.synth({ type: "sine", tone: fifth, duration: durSec, volume: gain * 0.28, attack: 0.008, decay: durSec * 0.40 });
    sound.synth({ type: "sine", tone: oct,   duration: durSec, volume: gain * 0.22, attack: 0.010, decay: durSec * 0.45 });
  }
}

// Chill intro→break1 bass ARC (chill only — the template has no sub
// here). `musicT` = seconds since music start. Three blended stages:
//   < 8 s    CRUNCH — punchy bit-crushed square stack + a noise spit
//   8–18 s   OCEAN  — crossfade into a deep, smooth lo-fi sub swell
//   ≥ 18 s   (caller adds the SWING — off-beats pushed late)
// `grind` 1→0: BVROOOM grindy wobble sub at the start that mellows into
// a smooth deep ocean sub as the track chills out.
function fireChillArcBass(startSec, midi, durSec, grind) {
  const freq = 440 * Math.pow(2, (midi - 69) / 12);
  const g = BASS_GAIN;
  const gr = Math.max(0, Math.min(1, grind));
  const smooth = 1 - gr;
  if (gr > 0.02) {
    // GRINDY WOB — sawtooth+square with an internal wobble LFO on pitch
    // + amplitude (the "woob"/Bvrooom), loudest at the start, easing
    // out as it chills. Segmented so the wobble actually moves.
    const segN = 8;
    const segLen = durSec / segN;
    const wobHz = 5.5;
    for (let s = 0; s < segN; s++) {
      const lfo = 0.5 + 0.5 * Math.sin((s / segN) * durSec * 2 * Math.PI * wobHz);
      const wob = 0.85 + 0.30 * lfo;            // pitch wobble
      const amp = (0.55 + 0.45 * lfo) * gr;
      const v = makeBufferSynth(bassBuf, startSec + s * segLen, SAMPLE_RATE, noiseRng);
      v.synth({ type: "sawtooth", tone: freq * 2 * wob,        duration: segLen * 1.3, volume: g * 0.80 * amp, attack: 0.004, decay: segLen });
      v.synth({ type: "square",   tone: freq * wob,            duration: segLen * 1.3, volume: g * 0.52 * amp, attack: 0.004, decay: segLen });
      v.synth({ type: "sawtooth", tone: freq * 4 * wob * 1.01, duration: segLen,       volume: g * 0.16 * amp, attack: 0.003, decay: segLen * 0.7 });
    }
  }
  if (smooth > 0.02) {
    // GUITAR THUMP / THWOMP — a plucked, palm-muted low string, not a
    // sustained ocean sub (jas: "more of a guitar thump thwomp bass").
    // Fast pluck, SHORT percussive decay, a quick downward pitch
    // "thwomp", and a short noisy finger-thud transient for the attack.
    const k = makeBufferSynth(bassBuf, startSec, SAMPLE_RATE, noiseRng);
    // PER-HIT VARIATION — the bass "changes sizes" so it's not always
    // the same sound (jas: "bass should be more variant in the tones
    // ... as if the drum is changing sizes a bit"). Deterministic via
    // noiseRng → varied but reproducible.
    const sizeMul = 0.7 + noiseRng() * 0.85;          // 0.7–1.55: small ↔ big drum
    const body = Math.min(durSec * 0.55 * sizeMul, 0.55);
    const dropDur = 0.045 + noiseRng() * 0.06;        // 45–105 ms thwomp
    const dropAmt = 0.035 + noiseRng() * 0.075;       // pitch-drop depth varies
    const h2 = noiseRng() * 0.30;                     // sometimes an upper partial (tighter/bigger)
    const segs = 6;
    for (let s = 0; s < segs; s++) {
      const u = s / segs;
      const bend = u < 1 ? Math.pow(1 - Math.min(1, (u * body) / dropDur), 2) : 0;
      const pf = 1 + dropAmt * bend;
      const seg = makeBufferSynth(bassBuf, startSec + u * body, SAMPLE_RATE, noiseRng);
      seg.synth({ type: "sine", tone: freq * pf,        duration: body * 0.22, volume: g * 1.05 * smooth, attack: 0.001, decay: body * 0.20 });
      seg.synth({ type: "sine", tone: freq * 0.5 * pf,  duration: body * 0.22, volume: g * 0.70 * smooth, attack: 0.001, decay: body * 0.20 });
      if (h2 > 0.12) seg.synth({ type: "sine", tone: freq * 2 * pf, duration: body * 0.16, volume: g * h2 * smooth, attack: 0.001, decay: body * 0.14 });
    }
    // finger/pick thud — short low noise transient (varies per hit)
    k.synth({ type: "noise", tone: 100 + noiseRng() * 100, duration: 0.022 + noiseRng() * 0.018, volume: g * 0.45 * smooth, attack: 0.0006, decay: 0.024 });
    k.synth({ type: "sine",  tone: freq * 2 * (0.9 + noiseRng() * 0.5), duration: 0.018, volume: g * 0.30 * smooth, attack: 0.0005, decay: 0.016 }); // pluck click
  }
}

// Chill KICK — a deep, HARMONIC, tuned kick (not a sample). Pitched to
// a chord tone (harmonizes with the progression), 3 octaves down for
// weight, with per-hit variety: mixed body wavetypes, some long-decay
// booms / some super-short tight ones, and a phase-shifted sub layer
// (varying offset → comb/phase movement). No double-kicks. Deterministic
// via noiseRng so it's varied but not random-sounding.
function fireChillKick(target, startSec, chordDeg, idx, gain, wall = 1) {
  const r2 = noiseRng();
  // Punchy classic kick: TR-808 + sharp click PUNCH + long bassy body
  // + low echo taps. `wall` 0→1: "behind the wall" (muffled, quiet,
  // distant — no bright click, duller 808, echo emphasised) ramping
  // OUT to full punchy presence by ~70 s.
  const w = Math.max(0, Math.min(1, wall));
  const gK = gain * (0.30 + 0.32 * w);                 // quieter behind the wall
  const deep = (0.64 + r2 * 0.12) * (0.78 + 0.22 * w); // duller/lower when veiled
  fireDrum(target, startSec, "c", { volume: gK, pitchFactor: deep });
  if (w > 0.4) {
    const tk = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
    tk.synth({ type: "noise", tone: 2600, duration: 0.006, volume: gK * 0.30 * w, attack: 0.0002, decay: 0.005 }); // punch click — only once it's OUT
  }
  const midi = scaleNoteMidi(chordDeg, -3);
  const freq = 440 * Math.pow(2, (midi - 69) / 12);
  const bodyDur = 0.7 + r2 * 0.5;                       // long ring — not short
  const kb = makeBufferSynth(target, startSec, SAMPLE_RATE, noiseRng);
  kb.synth({ type: "sine", tone: freq, duration: bodyDur, volume: gK * 0.50, attack: 0.003 + 0.02 * (1 - w), decay: bodyDur * 0.95 });
  const echoBoost = 1 + 0.8 * (1 - w);                  // more distant/echoey behind the wall
  for (let e = 1; e <= 2; e++) {
    const et = startSec + e * 0.135;
    const ee = makeBufferSynth(target, et, SAMPLE_RATE, noiseRng);
    ee.synth({ type: "sine", tone: freq * 0.5, duration: bodyDur * 0.9, volume: gK * 0.22 * echoBoost / e, attack: 0.004, decay: bodyDur * 0.85 });
    fireDrum(target, et, "c", { volume: gK * 0.30 * echoBoost / e, pitchFactor: deep * 0.88 });
  }
}

// Hellsine GABBER/Rotterdam kick — ONE sine, fast exponential pitch
// drop, tanh-saturated past its skin (the distorted sine IS the kick),
// + a bright ~4 ms HF sine SNAP at the attack. Ported from
// pop/hellsine/bin/hellsine.mjs (jas: "i loooove the kick in
// hellsine"). Opt-in via --hell; writes mono into the bus.
function fireGabberKick(target, startSec, drive, gain = 1, tight = 0, pitchMul = 1) {
  const SR = SAMPLE_RATE;
  // `tight` 0→1: a smaller, shorter, less-saturated "lil kick" (jas:
  // "our kick should be tighter right at the start first 24 seconds").
  // `pitchMul` < 1 gives a deeper "tok" alternation (tik/tok pattern).
  const dur = 0.26 * (1 - 0.58 * tight) * (pitchMul < 1 ? 1.20 : 1); // tok rings longer
  drive *= 1 - 0.45 * tight;
  gain *= (1 - 0.30 * tight) * (pitchMul < 1 ? 1.15 : 1);              // tok pronounced
  const pStart = 240 * pitchMul, pEnd = 47 * pitchMul, pT = 0.034 * (1 - 0.4 * tight) * (pitchMul < 1 ? 1.25 : 1);
  const i0 = Math.floor(startSec * SR);
  const nN = Math.floor(dur * SR);
  let ph = 0;
  for (let k = 0; k < nN; k++) {
    const idx = i0 + k;
    if (idx < 0) continue;
    if (idx >= target.length) break;
    const lt = k / SR;
    const f = pEnd + (pStart - pEnd) * Math.exp(-lt / pT);
    ph += (2 * Math.PI * f) / SR;
    const amp = Math.exp(-lt / 0.10) * (1 - Math.exp(-lt / 0.0008));
    let x = Math.sin(ph);
    x = Math.tanh(x * (drive * (0.7 + 0.3 * Math.exp(-lt / 0.05))));   // the hell
    x = Math.max(-0.97, Math.min(0.97, x * 1.06));                     // skin
    const click = Math.sin(2 * Math.PI * 3400 * lt) * Math.exp(-lt / 0.0016) * 0.5
                + Math.sin(2 * Math.PI * 1700 * lt) * Math.exp(-lt / 0.0042) * 0.35;
    target[idx] += (x * amp + click) * 0.9 * gain;
  }
}

// Stick CLICK — single wooden tap, ~12 ms. `toneHz` lets callers pitch
// it anywhere across a wide range for tumbling/scattered dice character.
function fireClick(target, startSec, gain = 1.0, toneHz = 4400) {
  const SR = SAMPLE_RATE;
  const dur = 0.012;
  const nN = Math.floor(dur * SR);
  const i0 = Math.floor(startSec * SR);
  // Lower clicks ring slightly longer (woodier).
  const decayT = 0.0030 + Math.max(0, (4400 - toneHz) / 4400) * 0.0035;
  for (let k = 0; k < nN; k++) {
    const idx = i0 + k;
    if (idx < 0) continue;
    if (idx >= target.length) break;
    const t = k / SR;
    const sine = Math.sin(2 * Math.PI * toneHz * t) * Math.exp(-t / decayT);
    const n = (noiseRng() * 2 - 1) * Math.exp(-t / 0.005);
    target[idx] += (sine * 0.55 + n * 0.28) * gain;
  }
}

// Dice-roll CLICK-CLACK — a cluster of 5-9 wooden taps with WIDELY
// pitched-wandering tones (1400-5400 Hz random per click), spread out
// over a longer window with bigger irregular gaps so the cluster reads
// as dice tumbling across wood — pitching DOWN and UP, scattered, NOT
// a regular drum-rest tick (per @jeffrey).
function fireDiceRoll(target, startSec, gain = 1.0) {
  const nClicks = 5 + Math.floor(noiseRng() * 5); // 5-9 clicks (was 3-6)
  let t = startSec;
  for (let i = 0; i < nClicks; i++) {
    // Random pitch each click — wide range, no fixed alternation.
    const toneHz = 1400 + noiseRng() * 4000;        // 1.4 kHz – 5.4 kHz
    const v = gain * (0.55 + noiseRng() * 0.65);    // tumbling unevenness
    fireClick(target, t, v, toneHz);
    // Bigger gaps — sometimes tight chatter, sometimes a pause that
    // makes the next click feel like a die landing flat.
    t += 0.030 + noiseRng() * 0.110;                // 30-140 ms gaps (was 22-62)
  }
}

// Traditional hand-CLAP — four staggered noise bursts (the multi-hand
// "shh-clap" effect), high-passed band, very short decay. Subtle
// trance-backbeat punctuation per @jeffrey.
function fireClap(target, startSec, gain = 1.0) {
  const SR = SAMPLE_RATE;
  const offsets = [0, 0.004, 0.009, 0.015];
  const dur = 0.085;
  const nN = Math.floor(dur * SR);
  for (const off of offsets) {
    const i0 = Math.floor((startSec + off) * SR);
    let lp1 = 0, lp2 = 0;
    for (let k = 0; k < nN; k++) {
      const idx = i0 + k;
      if (idx < 0) continue;
      if (idx >= target.length) break;
      const t = k / SR;
      const n = (noiseRng() * 2 - 1);
      lp1 += 0.45 * (n - lp1);          // 1-pole LP ≈ 3 kHz roll-off
      lp2 += 0.10 * (lp1 - lp2);        // very slow LP for HP-via-subtract
      const hp = lp1 - lp2;             // HP'd noise ≈ 800 Hz upward
      const env = Math.exp(-t / 0.018) * (1 - Math.exp(-t / 0.0008));
      target[idx] += hp * env * gain * 0.55;
    }
  }
}

// "Bunny bow" — the comp glue. One BOWED, BRUSHED, sustained note: a
// slow swell (soft attack → crescendo → release) so consecutive notes
// OVERLAP/legato, warm timbre (triangle + soft saw bite + body), a
// brush/shaker noise grain riding the same swell, and gentle bow
// vibrato. Pitch is chosen by the caller via nearest-tone voice-leading
// through the chord (with passing/diminished steps) — it "bounces"
// around the other voices like a soft bow.
function fireBunnyBow(target, startSec, midi, durSec, gain) {
  const f0 = 440 * Math.pow(2, (midi - 69) / 12);
  const seg = 0.045;
  const n = Math.max(4, Math.floor(durSec / seg));
  const atk = 0.40, rel = 0.32;
  for (let i = 0; i < n; i++) {
    const sf = i / n;
    const tt = sf * durSec;
    let env;
    if (sf < atk) env = 0.5 - 0.5 * Math.cos(Math.PI * (sf / atk));
    else if (sf > 1 - rel) env = 0.5 - 0.5 * Math.cos(Math.PI * ((1 - sf) / rel));
    else env = 1;
    const vib = 1 + 0.013 * Math.sin(2 * Math.PI * 5.0 * tt); // gentle bow vibrato
    const t0 = startSec + i * seg;
    const v = makeBufferSynth(target, t0, SAMPLE_RATE, noiseRng);
    v.synth({ type: "triangle", tone: f0 * vib,     duration: seg + 0.03, volume: gain * 0.55 * env, attack: 0.012, decay: seg * 0.9 });
    v.synth({ type: "sawtooth", tone: f0 * vib,     duration: seg + 0.03, volume: gain * 0.15 * env, attack: 0.014, decay: seg * 0.9 }); // soft bow bite
    v.synth({ type: "sine",     tone: f0 * 2 * vib, duration: seg + 0.03, volume: gain * 0.10 * env, attack: 0.014, decay: seg * 0.8 }); // body
    v.synth({ type: "noise",    tone: 3200,         duration: seg,        volume: gain * 0.06 * env, attack: 0.010, decay: seg * 0.8 }); // brush/shaker grain
  }
}

// Riser — pitched sine sweep + chord-tone arpeggios with a thin noise
// top. The user wanted "more pitched noise" instead of plain white-
// noise sloshes, so the body is pitched tones; noise is a glaze on top.
function fireRiser(target, startSec, durSec, gain) {
  // Continuous pitched sweep — 18 sine steps from ~200 Hz to ~5 kHz.
  // Overlapping windows so the sweep is heard as one rising tone.
  const sweepSteps = 18;
  for (let i = 0; i < sweepSteps; i++) {
    const t = i / sweepSteps;
    const tone = 200 * Math.pow(25, t); // 200 Hz → ~5 kHz exponential
    const stepStart = startSec + t * durSec;
    const stepDur = durSec / sweepSteps + 0.12; // overlap with next step
    const sweep = makeBufferSynth(target, stepStart, SAMPLE_RATE, noiseRng);
    sweep.synth({
      type: "sine",
      tone,
      duration: stepDur,
      volume: gain * 0.7 * (0.3 + t * 0.7),
      attack: stepDur * 0.25,
      decay: stepDur * 0.55,
    });
    // Layer a 5th-above sine for harmonic density
    sweep.synth({
      type: "sine",
      tone: tone * 1.5,
      duration: stepDur,
      volume: gain * 0.35 * (0.3 + t * 0.7),
      attack: stepDur * 0.30,
      decay: stepDur * 0.50,
    });
  }
  // Thin noise glaze (was the whole riser before — now just adds shimmer)
  const noiseSteps = 4;
  for (let i = 0; i < noiseSteps; i++) {
    const t = i / noiseSteps;
    const tone = 1500 * Math.pow(6, t); // 1.5 kHz → 9 kHz bandpass
    const stepStart = startSec + t * durSec;
    const stepDur = durSec / noiseSteps + 0.1;
    const noise = makeBufferSynth(target, stepStart, SAMPLE_RATE, noiseRng);
    noise.synth({
      type: "noise",
      tone,
      duration: stepDur,
      volume: gain * 0.25 * (0.4 + t * 0.6),
      attack: stepDur * 0.3,
      decay: stepDur * 0.45,
    });
  }
}

// Lazy-load piano bank only if some section actually wants piano.
const needPiano = Object.values(SECTION_TEMPLATES).some((t) => t.piano);
if (needPiano) loadPianoBank();

// Per-instrument entry bars — instruments come in progressively across
// the track. Even if the SECTION template enables them, they're gated
// until their entry bar so the opening starts lean.
// Defaults work for the typical 60-bar trancewaltz arrangement.
const INSTRUMENT_ENTRY_BAR_NORMAL = {
  pad:       0,
  kick:      2,
  hat:       2,
  sub:       8,
  bells:     6,
  lead:      10,
  piano:     22,
  riser:     12,
  snareRoll: 14,
  supersaw:  6,
};
// CHILL mode entry bars — every voice starts MUCH sooner so the track
// doesn't wait until 30s for piano + lead. Beat is on from bar 0, the
// melodic voices settle in over the first 4 bars (~7 s).
const INSTRUMENT_ENTRY_BAR_CHILL = {
  pad:       0,
  kick:      0,
  hat:       0,
  sub:       4,
  bells:     0,
  lead:      2,
  piano:     4,
  riser:     999, // disabled
  snareRoll: 999, // disabled
  supersaw:  2,
};
const INSTRUMENT_ENTRY_BAR = isChill ? INSTRUMENT_ENTRY_BAR_CHILL : INSTRUMENT_ENTRY_BAR_NORMAL;
function instrumentEnabled(name, bar) {
  const entry = INSTRUMENT_ENTRY_BAR[name];
  if (entry === undefined) return true;
  return bar >= entry;
}

// ── build event list ──────────────────────────────────────────────────
let kickCount = 0, hatCount = 0, subCount = 0, leadCount = 0, padCount = 0, riserCount = 0, snareRollCount = 0, pianoCount = 0, bellsCount = 0, machineGunCount = 0, supersawCount = 0, dropImpactCount = 0;
// Event timeline — every fired note recorded for the scrolling-score
// visualization. Each entry: { t: startSec, dur: durSec, midi?: noteMidi, gain?: vol }
const events = {
  kick: [],
  hat: [],
  snare: [], // snare-roll subdivisions (build crescendo + main snare roll)
  sub: [],
  lead: [],
  bells: [],
  piano: [],
  supersaw: [],
  arp: [], // intro/break1 square-wave arpeggio (its own visible lane)
  dropImpact: [],
  sfx: [], // boot chime, sniper, shutdown chime, machine guns, drone flybys, cats, birds, lion roar
  vox: [], // greeting, booty, screams, bye, vocal stem chunks
};

// Machine-gun moments — guns now bypass per-section bitcrush (added
// to `out` after fx), so we can run them louder AND more frequent
// without losing depth. Bursts on every bar of every build section,
// plus scattered hits across each drop section.
const machineGunHits = [];
for (const r of sectionRanges) {
  if (r.name.startsWith("build")) {
    // Gentle, distant-sounding bursts in the second half of the build.
    const buildLen = r.endBar - r.startBar;
    const startBar = Math.min(2, Math.max(0, buildLen - 3));
    for (let b = startBar; b < buildLen; b++) {
      const barStart = r.startSec + b * barSec;
      const frac = (b - startBar) / Math.max(1, buildLen - 1 - startBar);
      const gain = 0.55 + frac * 0.55; // 0.55 → 1.10 (was 1.0 → 2.1)
      machineGunHits.push({ startSec: barStart + barSec * 0.10, durSec: barSec * 0.75, gain });
    }
  }
  if (r.name.startsWith("drop")) {
    // Both drops now use the same simpler 2-burst layout — drop2's
    // pattern (held + late accent) felt much cooler than drop1's 4
    // scattered bursts, so drop1 matches it for narrative consistency.
    const dropLen = r.endSec - r.startSec;
    machineGunHits.push({ startSec: r.startSec,                  durSec: barSec * 1.5, gain: 0.40 });
    machineGunHits.push({ startSec: r.startSec + dropLen * 0.65, durSec: 0.55,         gain: 0.45 });
  }
}

// Ambient field recording → "castle". Decoded up-front (before the
// per-bar loop) so its grains can be beat-aligned + pitched on the
// grid. Chill only; gitignored stem, address-named source stays off
// the repo. ambientBuf also feeds the continuous breathing bed in the
// stereo mixdown.
let ambientBuf = null;
if (isChill) {
  const AMB_PATH = `${REPO}/pop/dance/out/.ambient-room.wav`;
  if (existsSync(AMB_PATH)) {
    const tmpAmb = `${dirname(OUT_PATH)}/.ambient-room.f32.raw`;
    const decA = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", AMB_PATH,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpAmb,
    ]);
    if (decA.status === 0 && existsSync(tmpAmb)) {
      const raw = readFileSync(tmpAmb);
      ambientBuf = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
      try { unlinkSync(tmpAmb); } catch {}
      console.log(`  ambient · field recording (${(ambientBuf.length / SAMPLE_RATE).toFixed(1)} s) → castle bed + grains`);
    }
  }
}
// One pitched, windowed GRAIN of the field recording — a soft Hann
// slice resampled to a musical ratio (magical, tuned), with attack +
// long tail so it's textural, not clicky. Routed by the caller.
function fireAmbGrain(target, startSec, srcSec, durSec, pitch, gain) {
  if (!ambientBuf || !ambientBuf.length) return;
  const SR = SAMPLE_RATE;
  const outN = Math.floor(durSec * SR);
  const srcStart = (Math.floor(srcSec * SR) % ambientBuf.length + ambientBuf.length) % ambientBuf.length;
  const oS = Math.floor(startSec * SR);
  for (let k = 0; k < outN; k++) {
    const w = 0.5 - 0.5 * Math.cos((2 * Math.PI * k) / outN); // Hann
    const sf = srcStart + k * pitch;
    const si = ((Math.floor(sf) % ambientBuf.length) + ambientBuf.length) % ambientBuf.length;
    const d = oS + k;
    if (d >= 0 && d < target.length) target[d] += ambientBuf[si] * w * gain;
  }
}

for (let bar = 0; bar < TOTAL_BARS; bar++) {
  // Per-bar tempo (accelerating in chill). These shadow the module-level
  // base beatSec/barSec for everything computed inside the loop, so all
  // note durations / sub-steps / arp grid follow the accelerando for free.
  const barSec  = barDurSec[bar];
  const beatSec = barSec / METER;
  const barStart = OPENING_PREFIX_SEC + barStartRel[bar];
  const { name: sec, localBar, sectionLen } = sectionAtBar(bar);
  const t = SECTION_TEMPLATES[sec];

  const chordDeg = progressionAt(bar);
  const triad    = chordMidis(chordDeg, 0);
  const bassMidi = scaleNoteMidi(chordDeg, -2);

  const isLastBar  = localBar === sectionLen - 1;
  const isFinalBar = sec === "outro" && isLastBar;
  const kickEnabled = t.kick && !(t.riser && isLastBar) && !isFinalBar && instrumentEnabled("kick", bar);
  const densityRoll = rng();

  // The track's linear "swing" is now a TEMPO accelerando (see BPM_END /
  // barDurSec) rather than an off-beat delay — pushing off-beats late
  // fought the "faster dance track" goal. swingSec stays 0 so the
  // existing `+ swingSec` call sites are harmless no-ops.
  // EXTREME SWING ENVELOPE — crescendos to the stamp bar (39, ~1:33)
  // then settles back to straight rhythm for the last half (after bar
  // 51). Bell-shaped via raised-cosine. Peak swing pushes off-beat
  // events ~85 ms late — distinctly perceived as a drunken/loose feel
  // around the vortex, then tightens up so drop2 rides on a regular
  // grid. Per @jeffrey.
  const STAMP_BAR = 39, SWING_WIDTH_BARS = 12, SWING_MAX_SEC = 0.085;
  const _sd = 1 - Math.abs(bar - STAMP_BAR) / SWING_WIDTH_BARS;
  const _swingFr = Math.max(0, _sd);
  const _swingHump = 0.5 - 0.5 * Math.cos(_swingFr * Math.PI); // smooth bell 0→1→0
  const swingSec = SWING_MAX_SEC * _swingHump;

  // Percussion-silent zones (chill): the first 8 s, the last 8 s, and a
  // mid bridge (~±7 s around the midpoint) have NO kick/hat — then they
  // come back. Lets the track open, breathe mid-way, and resolve.
  const _mt = barStart - OPENING_PREFIX_SEC;
  const T = totalSec;
  const _mid = T * 0.40; // drop out of all the tracks SOONER (~40% in)
  // Gradual perc envelope (0..1) — an adventure, not hard cuts: a SLOW
  // hello at the open, a slow GOODBYE into the early mid-bridge, an
  // even SLOWER hello back out, and a slow goodbye into the ending.
  const _ru = (x, a, b) => Math.max(0, Math.min(1, (x - a) / (b - a)));
  let _pe = 1;
  // Perc-envelope ramp: chill+meter-5 trancepenta has the early-drop
  // structure (drop1 at ~29 s), so hats/perc ramp in during break1+
  // build1 (10→26 s). Chill+waltz (trancenwaltz) keeps the original
  // 38→56 slow-open. Other modes don't reach this branch.
  const PE_RAMP_A = (isChill && !isWaltz) ? 10 : 38;
  const PE_RAMP_B = (isChill && !isWaltz) ? 26 : 56;
  _pe *= _ru(_mt, PE_RAMP_A, PE_RAMP_B);     // SPARSE early, then perc ramps in
  _pe *= 1 - _ru(barStart, T - 14, T - 2);   // slow goodbye into the end (~12 s)
  // Global arrangement build: ~0 for the sparse first 40 s, then a
  // fast ramp to full by ~58 s. Scales the DENSE layers (arp, mosquito,
  // bass-arc) so the open is sparse and everything "really ramps in".
  const chillBuild = isChill ? _ru(_mt, 38, 58) : 1;
  let _bridge;
  if (barStart < _mid - 1) _bridge = 1 - _ru(barStart, _mid - 7, _mid - 1); // slow goodbye
  else if (barStart < _mid + 4) _bridge = 0;                                 // dropped out
  else _bridge = _ru(barStart, _mid + 4, _mid + 26);                         // even slower hello
  _pe *= _bridge;
  const chillPercGain = isChill ? _pe : 1;
  const chillHatGain = isChill ? Math.pow(_pe, 1.7) : 1; // hats lag — slower hello
  const chillPercMute = isChill && chillPercGain < 0.03;
  const chillHatMute = isChill && chillHatGain < 0.03;
  // Kick runs the WHOLE track (no sparse-open silence): only the mid
  // bridge dip + the end goodbye gate it. For the first ~minute it's
  // "behind the wall" (muffled + quiet + distant), then it comes OUT
  // and is present with us by ~70 s.
  const _endgb = 1 - _ru(barStart, T - 24, T - 8); // perc fades out over last 24s → all gone by T-8
  const kickGate = isChill ? _endgb * _bridge : 1;
  const kickWall = isChill ? _ru(_mt, 26, 50) : 1; // present by ~50s — consistent perc @53s (jas)

  // Meditation gong — every 8 bars in chill. Every OTHER one (and
  // sometimes extra) is a big deep hollow warbling "BONGGG"; the rest
  // stay the high struck bell. Skips bar 0 so the intro breathes in.
  if (isChill && bar > 0 && bar % 8 === 0) {
    const gongIdx = Math.floor(bar / 8);
    const deep = gongIdx % 2 === 1 || noiseRng() < 0.2;
    fireGong(barStart, scaleNoteMidi(chordDeg, deep ? -1 : 0), deep ? 0.42 : 0.36, deep, gongIdx % 2 === 0);
    bellsCount++;
  }

  // Taptic haptic pulse (chill) — a felt-in-time ~150 Hz tap on the
  // downbeat; a peek/pop "double" on phrase boundaries. Muted in the
  // percussion-silent zones so the breaths stay clean.
  if (isChill && !chillPercMute) {
    const ht = humanize(barStart, 4);
    fireHaptic(dryBuf, ht, 0.12 * chillPercGain, bar % 8 === 0 ? "double" : "tap");
  }

  // Final musical-ending cadence — fires on the last bar of the outro.
  // Held tonic chord (root + 3rd + 5th + octave) on the pad, a bell
  // ring on the root-octave, and a deep root thud. All overshoot
  // totalSec into the tail; the natural decay ends the track without
  // any forced ffmpeg fade.
  if (isFinalBar) {
    // Final cadence — durations tuned so the natural pad+bell decay
    // tapers to silence within the last bar (totalSec). No more
    // 4-second overshoot into a tail that gets ffmpeg-faded; the
    // music decays itself, supporting the clean loop boundary.
    // PERFECT LOOP — normal short final cadence that resolves on the
    // last (bar-aligned) bar and wraps via the ~6 ms declick. (No more
    // 16 s sustain-to-fill; that was for the removed long fade.)
    const endStart = barStart;
    const endDur   = totalSec - barStart;
    const finalTriad = chordMidis(0, 0);
    const finalNotes = [...finalTriad, scaleNoteMidi(0, 1)];
    for (const m of finalNotes) {
      mixEventLeadPad(
        { startSec: endStart, midi: m, gain: PAD_GAIN * 1.8, durSec: endDur },
        duckBuf,
        { preset: "pad", sampleRate: SAMPLE_RATE, seed: `final:${m}` }
      );
      padCount++;
    }
    mixEventSinebell(
      { startSec: endStart, midi: scaleNoteMidi(0, 1), gain: BELLS_GAIN * 1.45, durSec: endDur },
      dryBuf,
      SAMPLE_RATE
    );
    bellsCount++;
    fireSubBass(bassBuf, endStart, scaleNoteMidi(0, -2), Math.min(endDur, 12), BASS_GAIN * 1.3, 0, true);
    subCount++;
    if (isChill) {
      // Upper-harmony GOODBYE — soft high sustained triad, a farewell
      // shimmer that rings through the long fade.
      for (const d of [0, 2, 4]) {
        fireBunnyBow(duckBuf, endStart + 0.04 * d, scaleNoteMidi(d, 2), endDur * 0.95, 0.085);
      }
      // Close-up percussive kick — dry + present, marks the very end.
      const endK = totalSec - 2.3;
      fireDrum(dryBuf, endK, "c", { volume: 0.9 * DRUM_GAIN, pitchFactor: 0.8 });
      events.kick.push({ t: endK });
    }
  }

  // Kick — every beat (METER hits per bar) with rhythmic variation.
  // Approaching the tape-stop window (last 4 s of music) the kicks
  // organically settle: density falls to a regular metronome and
  // volume tapers so the slowdown feels like the song breathing
  // out rather than being spliced into the shutdown.
  if (kickEnabled && (!isChill || kickGate > 0.02)) {
    const TAPE_STOP_START = totalSec - 5.0;
    const SETTLE_WINDOW   = 4.0; // 4 s before tape-stop start
    const sixteenthSec_k = beatSec / 4;
    for (let beat = 0; beat < METER; beat++) {
      if (densityRoll > t.drumDensity && sec === "intro" && localBar < Math.floor(sectionLen / 2)) continue;
      const startSec = barStart + beat * beatSec;
      // Hard stop at tape-stop start — slowed kicks won't muddy the
      // shutdown chime.
      if (startSec >= TAPE_STOP_START) continue;
      // Settle factor — 1.0 during normal play, drops to 0.0 by the
      // tape-stop start. Used to gate variation + taper volume.
      let settleAmp = 1.0;
      let settleVar = 1.0;
      if (startSec > TAPE_STOP_START - SETTLE_WINDOW) {
        const u = (startSec - (TAPE_STOP_START - SETTLE_WINDOW)) / SETTLE_WINDOW; // 0 → 1
        settleAmp = 1.0 - u * 0.5;  // taper to 50 % volume
        settleVar = 1.0 - u;        // skips + ghosts fade out
      }
      // Variation: beat 1 always; other beats fire 85 % normally;
      // probability climbs to 1.0 in the settle window so the
      // rhythm becomes regular again.
      const baseProb = beat === 0 ? 1.0
                     : (sec === "break1" ? 0.70 : 0.92);
      const beatFireProb = baseProb + (1.0 - baseProb) * (1.0 - settleVar);
      // Chill: NEVER drop / skip kicks (the random drops read as
      // mistakes + too regular). Every beat fires; the life comes from
      // a slow DISTANCE arc — the kick recedes a bit and returns over
      // ~13 bars (it never goes near-silent, so it never feels dropped).
      if (!isChill && rng() > beatFireProb) continue;
      // WALTZ lilt — strong ONE, light two-three (oom-pa-pa).
      const accent = isChill ? (beat === 0 ? 1.40 : 0.55) : (beat === 0 ? 1.05 : 0.95);
      const kickDist = isChill ? 0.88 + 0.12 * Math.cos(2 * Math.PI * bar / 13) : 1; // shallower recede — steadier perc (jas)
      // Humanize — tight ±5 ms jitter, scaled down in the settle window.
      const kickT = humanize(startSec, 5 * settleVar + 2);
      if (isChill) {
        // Lose the kick as the track fades out (jas: "lose the last
        // kick ... as its fading") — no kicks in the final ~20 s
        // (the master fade is the last 18 s); the snare/other beat
        // elements still taper naturally.
        const kickInFade = kickT > totalSec - 20;
        // Hold the whole beat off until BEAT_IN s, then ease in ~3 s.
        const _mrel = kickT - OPENING_PREFIX_SEC;
        const beatInGate = BEAT_IN <= 0 ? 1
          : (_mrel < BEAT_IN ? 0 : Math.min(1, (_mrel - BEAT_IN) / 3));
        // The real grinding beat "drops" ~28 s after BEAT_IN (≈ 1 min);
        // before that, a subtle dampened 4-on-floor + a shifted key.
        const DROP_SEC = BEAT_IN > 0 ? BEAT_IN + 28 : 0;
        // ── WALTZ 3/4 perc restructure (jas: "we should not have gone
        // into a bts bts rhythm ... keep the waltz 3/4 time ...
        // aggressively restructure the whole perc"). One clean
        // oom-pa-pa per bar: KICK only on the downbeat, deep SNARE on
        // beat 2, a soft light TICK on beat 3. No kick on 2/3 → never
        // four-on-the-floor. ───────────────────────────────────────
        if (!kickInFade && beat === 0) {                 // the ONE + a syncopated push
          const kg = accent * settleAmp * DRUM_GAIN * kickDist * kickGate * beatInGate;
          const pushT = kickT + beatSec * 1.5;
          const pushIn = pushT < totalSec - 20;
          if (HELL > 0) {
            // Hellsine gabber kick (jas: "i loooove the kick in
            // hellsine"). First 24 s (music-relative): a TIGHT "lil
            // kick", then it opens to the full hellsine kick.
            // TIK/TOK — alternate every other downbeat kick to a deeper
            // pitch (pitchMul 0.55) so the kick reads as a tik-tok
            // rocking pulse instead of an unvarying thud (@jeffrey).
            const tight = (kickT - OPENING_PREFIX_SEC) < 24 ? 1 : 0;
            const isTok = (kickCount % 2) === 1;
            const mainPitch = isTok ? 0.55 : 1.0;
            fireGabberKick(dryBuf, kickT, HELL, kg * 0.85, tight, mainPitch);
            // Push kick gets the OPPOSITE pitch — keeps the tik/tok
            // call-and-response alive across the syncopated off-beat.
            if (pushIn) fireGabberKick(dryBuf, pushT, HELL, kg * 0.50, tight, isTok ? 1.0 : 0.55);
          } else {
            fireChillKick(dryBuf, kickT, chordDeg, kickCount, kg, kickWall);
            // SYNCOPATED off-beat kick on the "& of 2" — forward DANCE
            // momentum so 3/4 grooves instead of reading as a stiff
            // ballroom waltz (jas). Not on a beat → not four-on-floor.
            if (pushIn) fireChillKick(dryBuf, pushT, chordDeg, kickCount, accent * 0.55 * settleAmp * DRUM_GAIN * kickDist * kickGate, kickWall);
            if (kickWall > 0.5 && noiseRng() < 0.06) {   // rare chop&screw on the one
              for (let c = 1; c <= 2; c++) {
                fireChillKick(dryBuf, kickT + c * 0.085, chordDeg, kickCount, accent * settleAmp * DRUM_GAIN * kickDist * kickGate * (0.7 / c), kickWall);
              }
            }
          }
        }
        // FLOOR — a quiet GUARANTEED downbeat pulse so the beat NEVER
        // fully drops mid-track (jas: "a 'beat' should never FULLLLY
        // drop in this track"). Independent of section/kickGate so the
        // intro & breaks still have a heartbeat; off only in the final
        // ~20 s so the ending can still resolve.
        if (beat === 0 && kickT < totalSec - 20) {
          const fl = makeBufferSynth(dryBuf, kickT, SAMPLE_RATE, noiseRng);
          const fg = 0.19 * settleAmp * DRUM_GAIN * beatInGate;
          fl.synth({ type: "sine", tone: 55, duration: 0.17, volume: fg,        attack: 0.002, decay: 0.15 });
          fl.synth({ type: "sine", tone: 92, duration: 0.06, volume: fg * 0.38, attack: 0.001, decay: 0.05 });
        }
        // INTRO 4-ON-THE-FLOOR — a VERY SUBTLE dampened kick on EVERY
        // beat through the intro (jas: "first 45s ... subtle 4 on the
        // floor ... dampened kick ... before the real grinding beat
        // ~1 min"). Eases out before the drop + crossfades under the
        // real beat (1-beatInGate).
        if (DROP_SEC > 0 && _mrel < DROP_SEC && kickT < totalSec - 20) {
          const introFade = Math.min(1, (DROP_SEC - _mrel) / 6);
          const ig = 0.085 * settleAmp * DRUM_GAIN * introFade * (1 - 0.7 * beatInGate);
          if (ig > 0.001) {
            const ik = makeBufferSynth(dryBuf, kickT, SAMPLE_RATE, noiseRng);
            ik.synth({ type: "sine", tone: 48, duration: 0.20, volume: ig,        attack: 0.006, decay: 0.18 });
            ik.synth({ type: "sine", tone: 70, duration: 0.05, volume: ig * 0.22, attack: 0.004, decay: 0.045 });
          }
        }
        if (beat === 1 && kickGate > 0.02 && !KICK_ONLY) { // PA — deep snare
          const snr = makeBufferSynth(dryBuf, kickT, SAMPLE_RATE, noiseRng);
          const sg = 0.62 * settleAmp * DRUM_GAIN * kickGate * kickDist * beatInGate;
          snr.synth({ type: "noise",    tone: 1500, duration: 0.12, volume: sg * 0.52, attack: 0.0006, decay: 0.10 });
          snr.synth({ type: "noise",    tone: 240,  duration: 0.14, volume: sg * 0.46, attack: 0.0006, decay: 0.12 });
          snr.synth({ type: "triangle", tone: 124,  duration: 0.14, volume: sg * 0.72, attack: 0.0009, decay: 0.12 });
          snr.synth({ type: "sine",     tone: 76,   duration: 0.11, volume: sg * 0.46, attack: 0.0010, decay: 0.095 });
          events.kick.push({ t: kickT, kind: "snare" });
        }
        if (isChill && beat === 2 && kickGate > 0.02 && !KICK_ONLY) { // PA — soft light tick
          const tk = makeBufferSynth(dryBuf, kickT, SAMPLE_RATE, noiseRng);
          const tv = 0.34 * settleAmp * DRUM_GAIN * kickGate * kickDist * beatInGate;
          tk.synth({ type: "noise",    tone: 2400, duration: 0.030, volume: tv * 0.40, attack: 0.0004, decay: 0.026 });
          tk.synth({ type: "triangle", tone: 360,  duration: 0.045, volume: tv * 0.40, attack: 0.0006, decay: 0.038 });
          events.kick.push({ t: kickT, kind: "tick" });
        }
      } else {
        fireDrum(dryBuf, kickT, "c", { volume: accent * settleAmp * DRUM_GAIN });
      }
      kickTimes.push(kickT);
      events.kick.push({ t: kickT });
      kickCount++;
      // Ghost 16th — disabled in chill mode (the doubled hit reads as
      // a kick repeat at the top of the track), disabled in the
      // settle window for organic feel.
      if (!isChill && rng() < 0.12 * settleVar) {
        const ghostT = humanize(startSec + sixteenthSec_k, 8);
        if (ghostT < TAPE_STOP_START) {
          fireDrum(dryBuf, ghostT, "c", { volume: 0.45 * settleAmp * DRUM_GAIN });
          kickTimes.push(ghostT);
          events.kick.push({ t: ghostT, kind: "ghost" });
          kickCount++;
        }
      }
    }
  }

  // Closed hat — off-beats (the "and" of every kick). METER hits per bar.
  if (t.hat && instrumentEnabled("hat", bar) && !chillHatMute && !(isChill && CHILL_HATS === "off") && !KICK_ONLY) {
    // Hat pattern with skips and 16th-note flams. Variation tapers
    // out in the last 4 s before tape-stop so the rhythm settles
    // into a regular pattern as the slowdown approaches — organic
    // transition, not spliced.
    const HAT_SUPPRESS_AFTER = totalSec - 5.0;
    const SETTLE_WINDOW_H = 4.0;
    const sixteenthSec_h = beatSec / 4;
    // Chill: hats arrive LATE and FROM DISTANT — nothing before
    // HAT_ENTER, then volume swells up from far away over ~18 s.
    const _hmt = barStart - OPENING_PREFIX_SEC;
    const HAT_ENTER = 10; // Match the early-drop structure — hats arrive at the start of break1 (was 30, pre-restructure)
    const hatsArrived = !isChill || _hmt >= HAT_ENTER;
    // Ease the entrance: a SHORT raised-cosine fade-in over ~7 s after
    // HAT_ENTER so they don't slam in (jas: "hats come in too
    // abruptly") — but NOT the long distant swell (that "came in like
    // shit"). Quick and smooth, then full.
    const HAT_RAMP = 7;
    const hatDistVol = !isChill
      ? 1
      : 0.5 - 0.5 * Math.cos(Math.PI * Math.max(0, Math.min(1, (_hmt - HAT_ENTER) / HAT_RAMP)));
    for (let beat = 0; beat < METER; beat++) {
      if (!hatsArrived) continue;
      const startSec = barStart + (beat + 0.5) * beatSec + swingSec;
      if (startSec >= HAT_SUPPRESS_AFTER) continue;
      // Settle factor — 1.0 normally, drops to 0.0 by tape-stop.
      let settleVar = 1.0;
      if (startSec > HAT_SUPPRESS_AFTER - SETTLE_WINDOW_H) {
        settleVar = 1.0 - (startSec - (HAT_SUPPRESS_AFTER - SETTLE_WINDOW_H)) / SETTLE_WINDOW_H;
      }
      // Chill: NO random skips — a CONSISTENT steady hat groove (jas:
      // "i want it consistent ... simple dance track"). Non-chill keeps
      // the organic skip.
      if (!isChill && rng() < 0.20 * settleVar) continue;
      const v = isChill ? 0.20 : 0.32 + rng() * 0.10; // chill: hat sits ~6 dB under kick (radio-balance, was 0.34 → too forward)
      // Humanize hats — ±8 ms feels like a real drummer.
      const hatT = humanize(startSec, 8 * settleVar + 1);
      // Chill: per-hit random pitchFactor scatters the noise band so no
      // two hats sound identical (more organic, stochastic shimmer).
      let hatPF = isChill ? 0.92 + noiseRng() * 0.16 : 1; // chill: SUBTLE scatter — consistent (jas)
      // Hats START like CLICKS: for ~15 s after they enter they're
      // high-pitched + quiet ticks, then open into full hats.
      const clickAmt = isChill ? Math.max(0, 1 - (_hmt - HAT_ENTER) / 15) : 0;
      const hatVol = v * DRUM_GAIN * hatDistVol * chillHatGain * (1 - 0.55 * clickAmt) * (isChill ? 0.78 : 1); // chill: a touch quieter — less annoying (jas)
      hatPF *= 1 + 1.6 * clickAmt;
      if (isChill && clickAmt < 0.5 && noiseRng() < 0.12) {
        // WEIRD hat — an occasional "shove" (jas: "a little shove here
        // and there but consistent") — short INHARMONIC shimmer.
        const hv = makeBufferSynth(dryBuf, hatT, SAMPLE_RATE, noiseRng);
        const bf = 5200 * (0.55 + noiseRng() * 0.95);
        const wv = hatVol * 0.9;
        hv.synth({ type: "sine",  tone: bf,        duration: 0.045, volume: wv * 0.50, attack: 0.0004, decay: 0.04 });
        hv.synth({ type: "sine",  tone: bf * 2.41, duration: 0.034, volume: wv * 0.32, attack: 0.0004, decay: 0.03 });
        hv.synth({ type: "sine",  tone: bf * 3.93, duration: 0.026, volume: wv * 0.22, attack: 0.0004, decay: 0.022 });
        hv.synth({ type: "noise", tone: 7000,      duration: 0.011, volume: wv * 0.18, attack: 0.0003, decay: 0.009 });
      } else {
        fireDrum(dryBuf, hatT, "g", { volume: hatVol, pitchFactor: hatPF });
      }
      if (false && isChill) { // echo taps OFF — simpler, consistent (jas)
        // echoey + bassier hat — 2 low, quiet, decaying delayed taps
        for (let e = 1; e <= 2; e++) {
          const het = hatT + e * 0.115;
          if (het < HAT_SUPPRESS_AFTER) {
            fireDrum(dryBuf, het, "g", { volume: v * DRUM_GAIN * hatDistVol * chillHatGain * (0.30 / e), pitchFactor: hatPF * 0.6 });
          }
        }
      }
      events.hat.push({ t: hatT, dur: 0.025 });
      hatCount++;
      // Flam — disabled in chill mode (reads as a doubled hihat in
      // the intro at the slow chill density). Probability ramps out
      // in the settle window otherwise.
      if (!isChill && rng() < 0.22 * settleVar) {
        const flamT = humanize(startSec + sixteenthSec_h, 10);
        if (flamT < HAT_SUPPRESS_AFTER) {
          const vf = 0.20 + rng() * 0.08;
          fireDrum(dryBuf, flamT, "g", { volume: vf * DRUM_GAIN });
          events.hat.push({ t: flamT, dur: 0.025, kind: "flam" });
          hatCount++;
        }
      }
    }
    // Chill: occasional fast hi-hat ROLL into a phrase boundary — a
    // quick crescendo of hats across the last beat (fun little fill).
    if (isChill && false && (bar % 16 === 15 || noiseRng() < 0.02)) { // rolls OFF — consistent simple groove (jas)
      const rollN = 9 + Math.floor(noiseRng() * 5);      // 9–13 hits
      const rollStart = barStart + (METER - 1) * beatSec;
      const deepEcho = noiseRng() < 0.4;                  // some run deep + echoey
      const pf = deepEcho ? 0.34 + noiseRng() * 0.12 : 1 + (noiseRng() - 0.5) * 0.4;
      for (let h = 0; h < rollN; h++) {
        const rt = humanize(rollStart + (h / rollN) * beatSec, 3);
        if (rt >= HAT_SUPPRESS_AFTER) break;
        const rv = (0.10 + 0.22 * (h / rollN)) * DRUM_GAIN;
        fireDrum(dryBuf, rt, "g", { volume: rv, pitchFactor: pf });
        if (deepEcho) {
          // echoey: a couple of decaying delayed taps trailing each hit
          for (let e = 1; e <= 2; e++) {
            const et = rt + e * 0.085;
            if (et < HAT_SUPPRESS_AFTER) fireDrum(dryBuf, et, "g", { volume: rv * (0.45 / e), pitchFactor: pf * (1 - 0.04 * e) });
          }
        }
        events.hat.push({ t: rt, dur: 0.02, kind: "roll" });
        hatCount++;
      }
    }
    // Open hat on phrase boundary every 4 bars. Skipped in chill —
    // it lands on the same 16th as beat-2's main hat (METER-0.5) and
    // reads as a doubled hit.
    if (!isChill && (bar + 1) % 4 === 0) {
      const openHatSec = humanize(barStart + (METER - 0.5) * beatSec, 6);
      if (openHatSec < HAT_SUPPRESS_AFTER) {
        fireDrum(dryBuf, openHatSec, "a", { volume: 0.45 * DRUM_GAIN });
        events.hat.push({ t: openHatSec, dur: 0.10, kind: "open" });
      }
    }
    // Chill: an OPEN hi-hat on the last off-beat every 4 bars (a fresh
    // slot, doesn't double the closed hats), once hats have arrived.
    if (isChill && hatsArrived && (bar + 1) % 4 === 0) {
      const oh = humanize(barStart + (METER - 0.5) * beatSec + swingSec, 7);
      if (oh < HAT_SUPPRESS_AFTER) {
        fireDrum(dryBuf, oh, "a", { volume: 0.34 * DRUM_GAIN * chillHatGain });
        events.hat.push({ t: oh, dur: 0.12, kind: "open" });
      }
    }
  }

  // Chill bass ARC — intro + break1 have no template sub, so fill them
  // with the crunch→ocean→swung arc bass (on the beat). Crunchy bitted
  // punch for the first ~8 s, morphing into a deep ocean lo-fi sub,
  // then (≥18 s) levelling out on a swing (off-beats pushed late).
  if (isChill && (sec === "intro" || sec === "break1")) {
    const barMusic0 = barStart - OPENING_PREFIX_SEC;
    // ZERO bass for the first 24 s — then DROP IT IN like thunder.
    if (barMusic0 < 24 && barMusic0 + barSec >= 24) {
      const thT = OPENING_PREFIX_SEC + 24;
      fireSubBass(bassBuf, thT, scaleNoteMidi(0, -1), 1.6, BASS_GAIN * 1.0, 0.1, true); // soft gentle drop (kawaii)
      const rb = makeBufferSynth(bassBuf, thT - 0.6, SAMPLE_RATE, noiseRng);
      rb.synth({ type: "noise", tone: 90, duration: 1.4, volume: BASS_GAIN * 0.5, attack: 0.5, decay: 0.9 }); // rumble swell
      events.sub.push({ t: thT, midi: scaleNoteMidi(0, -2), dur: 3.2 });
      subCount++;
    }
    for (let beat = 0; beat < METER; beat++) {
      const baseT  = barStart + beat * beatSec;
      const musicT = baseT - OPENING_PREFIX_SEC;
      if (musicT < 24) continue;                  // ZERO bass first 24 s
      const swung  = (musicT >= 18 && beat !== 0) ? 0.16 * beatSec : 0;
      const bt     = humanize(baseT + swung, 6);
      const m = (beat === 1 && METER >= 3)
        ? scaleNoteMidi(chordDeg + 4, -2)   // 5th up for color
        : scaleNoteMidi(chordDeg, -2);      // root, low
      const grind = Math.max(0, 1 - musicT / 60); // BVROOOM → chilled over ~60s
      fireChillArcBass(bt, m, beatSec * 0.62, grind);
      events.sub.push({ t: bt, midi: m, dur: beatSec * 0.62 });
      subCount++;
    }
  }

  // Sub bass — off-beat of every beat. Its own bus for deep wobble.
  // Wave morphs from triangle (early track) through square (mid) back
  // to triangle (late) following a track-position arc. Chord-tone
  // alternation: beat 1 = root, beat 2 = 5th up, beat 3 = root.
  if (t.sub && instrumentEnabled("sub", bar)) {
    const trackProgress = bar / Math.max(1, TOTAL_BARS - 1);
    // Sinusoidal morph: 0 → 1 → 0 across the track.
    const morphT = 0.5 - 0.5 * Math.cos(trackProgress * Math.PI * 2);
    const fifthBassMidi = scaleNoteMidi(chordDeg + 4, -2); // chord 5th, low octave
    // Sub bass entry-ramp — when sub turns on at a section boundary
    // (eg. break1 → build1) it used to cold-start at full volume,
    // causing an abrupt textural shift. Ramp the gain in over the
    // first 2 bars of the section so the bass texture emerges
    // gradually under the existing bed.
    let subEntryGain = 1.0;
    if (sec === "build1" || sec === "build2" || sec === "drop1" || sec === "drop2") {
      // Only ramp if the PREVIOUS section had sub: false (real entry).
      const prevSecIdx = sectionRanges.findIndex((r) => r.name === sec) - 1;
      const prevSec = prevSecIdx >= 0 ? sectionRanges[prevSecIdx] : null;
      if (prevSec && !prevSec.template.sub) {
        const beatsIntoSection = localBar * METER;
        const rampBeats = 2 * METER; // 2 bars of ramp
        if (beatsIntoSection < rampBeats) {
          subEntryGain = beatsIntoSection / rampBeats;
        }
      }
    }
    for (let beat = 0; beat < METER; beat++) {
      const startSec = humanize(barStart + (beat + 0.5) * beatSec + swingSec, 6);
      const useFifth = (beat === 1 && METER >= 3); // 2nd beat → 5th up for color
      const midiNote = useFifth ? fifthBassMidi : bassMidi;
      // Per-beat ramp so the FIRST beat of the entry bar is quietest
      // and each subsequent beat is a hair louder.
      const beatRamp = sec === "build1" && localBar < 2
        ? Math.min(1, (localBar * METER + beat) / (2 * METER))
        : subEntryGain;
      fireSubBass(bassBuf, startSec, midiNote, beatSec * 0.55, BASS_GAIN * beatRamp, morphT, true);
      events.sub.push({ t: startSec, midi: midiNote, dur: beatSec * 0.55 });
      subCount++;
    }
  }

  // Pad — long sustained chord per bar. Voicing varies: even cycles
  // play the triad, odd cycles add a 7th for color and sometimes a 9th.
  if (t.pad && instrumentEnabled("pad", bar)) {
    const cycle = Math.floor(bar / (CHORD_BARS * 4));
    const padNotes = [...triad];
    if (cycle % 2 === 1) padNotes.push(scaleNoteMidi(chordDeg + 6, 0)); // 7th
    if (cycle % 4 === 3) padNotes.push(scaleNoteMidi(chordDeg + 1, 1)); // 9th, higher octave
    for (const m of padNotes) {
      mixEventLeadPad(
        { startSec: barStart,
          midi: m - (isChill ? 12 : 0),          // chill: an octave lower — was ringing/too high
          gain: PAD_GAIN,
          durSec: barSec * (isChill ? 0.55 : 0.98) }, // chill: re-articulates, doesn't drone
        duckBuf,
        { preset: "pad", sampleRate: SAMPLE_RATE, seed: `pad:${bar}:${m}` }
      );
      padCount++;
    }
  }

  // Power supersaw — sustained background chord, octave above the pad.
  // Lives on duckBuf so it sidechain-pumps with the kick. Detuned
  // sawtooth stack (JP-8000 style) gives the "trance wall" texture
  // the sine-only voices can't carry alone.
  if (t.supersaw && instrumentEnabled("supersaw", bar)) {
   if (isChill) {
    // Chill mosquito DISABLED — replaced by the Odyssey-arc theremin
    // that runs unconditionally across the whole track (see the
    // ODYSSEY_THEREMIN block AFTER this conditional, outside the
    // t.supersaw gate, so the melody carries through intro / break2 /
    // outro too).
   } else {
    // High supersaw — pulled back to chill levels. Was pushed to 0.50
    // (+12dB) for "trance wall" but felt too loud against the chill
    // bed per @jeffrey. Settled at 0.32 (+8 dB from original 0.13).
    for (const m of triad) {
      mixEventSupersaw(
        { startSec: barStart, midi: m + 12, gain: 0.32, durSec: barSec },
        sawBuf,
        { preset: "pad", sampleRate: SAMPLE_RATE, seed: `saw:${bar}:${m}` }
      );
      events.supersaw.push({ t: barStart, midi: m + 12, dur: barSec });
      supersawCount++;
    }
    // Super-low harsh supersaw — root + 5th. Pulled back from 0.42/0.30
    // to 0.26/0.18 — still present, less heavy.
    if (sec.startsWith("drop") || sec.startsWith("build")) {
      const rootDeg = chordDeg;
      const fifthDeg = chordDeg + 4;
      mixEventSupersaw(
        { startSec: barStart, midi: scaleNoteMidi(rootDeg, -1), gain: 0.26, durSec: barSec },
        sawBuf,
        { preset: "lead", detuneCents: 32, voices: 7, sampleRate: SAMPLE_RATE, seed: `saw-low:${bar}:r` }
      );
      mixEventSupersaw(
        { startSec: barStart, midi: scaleNoteMidi(fifthDeg, -1), gain: 0.18, durSec: barSec },
        sawBuf,
        { preset: "lead", detuneCents: 32, voices: 7, sampleRate: SAMPLE_RATE, seed: `saw-low:${bar}:5` }
      );
      supersawCount += 2;
    }
   }
  }

  // ── ODYSSEY THEREMIN ─────────────────────────────────────────────
  // Single voice melodic line carrying a Homer-style arc across the
  // whole track. Three acts:
  //   JOURNEY OUT  bars 0-35  : gentle dorian ascent, hopeful sail
  //   BATTLE       bars 36-43 : fast wide intervals around the 1:35
  //                              audio stamp — the encounter
  //   RETURN HOME  bars 44-71 : descending peace, settled
  //   OUTRO        bars 72-79 : sustained tonic, resolution
  // Runs UNCONDITIONALLY (outside the t.supersaw gate) so the melody
  // carries through intro / break2 / outro sections where supersaw is
  // off in the section template. Uses fireMosquito as the voice (its
  // slow pan + drift gives a theremin-ish wander) but with a single
  // call per held note instead of a 3-voice ensemble.
  {
    const ODYSSEY = [
      // [midi, holdBars, vel]  JOURNEY OUT (sum = 36 bars)
      [62, 4, 0.55],  [65, 4, 0.60],  [67, 4, 0.65],  [69, 4, 0.70],
      [72, 2, 0.65],  [74, 2, 0.70],  [72, 4, 0.65],  [74, 4, 0.75],
      [77, 4, 0.80],  [76, 2, 0.75],  [74, 2, 0.70],
      // BATTLE (sum = 8 bars, 1 bar each — agitated)
      [81, 1, 0.92],  [79, 1, 0.88],  [82, 1, 0.95],  [86, 1, 1.00],
      [83, 1, 0.95],  [80, 1, 0.88],  [77, 1, 0.82],  [74, 1, 0.78],
      // RETURN HOME (sum = 28 bars — descending)
      [74, 4, 0.65],  [72, 4, 0.60],  [69, 4, 0.58],  [67, 4, 0.55],
      [65, 2, 0.52],  [69, 2, 0.55],  [67, 4, 0.50],  [65, 4, 0.45],
      // OUTRO (sum = 8 bars — sustained tonic resolution)
      [62, 8, 0.40],
    ];
    let acc = 0;
    for (let mi = 0; mi < ODYSSEY.length; mi++) {
      const [midi, hold, vel] = ODYSSEY[mi];
      if (bar === acc) {
        const noteDurSec = hold * barSec * 1.02; // tiny overlap → glide-blend
        // DISTANCE breathing — long LFO across the song so the theremin
        // wanders from close (loud + present) to very distant (faint
        // call across the horizon). BATTLE bars stay confident + loud.
        const inBattle = bar >= 36 && bar < 44;
        const distLFO = inBattle
          ? 1.0
          : 0.35 + 0.65 * Math.max(0, 0.5 + 0.5 * Math.sin((bar / 19) * 2 * Math.PI));
        const gain = vel * 0.45 * distLFO;
        const next = ODYSSEY[mi + 1];
        const nextMidi = next ? next[0] : null;
        // Theremin at the original ODYSSEY register (D4-D6 / midi 62-86).
        fireTheremin(sawBuf, barStart, midi, noteDurSec, gain, nextMidi);
        events.supersaw.push({ t: barStart, midi, dur: noteDurSec, kind: "theremin" });
        supersawCount++;
        break;
      }
      if (bar < acc + hold) break;
      acc += hold;
    }
  }

  // CLAP — classic trance backbeat punctuation, sparingly placed
  // ("here or there" per @jeffrey). Fires on beat 2 (mid-bar) of every
  // other drop1/drop2 bar — present enough to feel, sparse enough to
  // stay "or there" instead of every beat.
  if ((sec === "drop1" || sec === "drop2") && (bar % 2 === 0)) {
    const clapT = barStart + barSec * 2 / 5;  // beat 2 of 5
    fireClap(dryBuf, clapT, 0.32 * DRUM_GAIN);  // chill backbeat — pulled from 0.55
    events.kick.push({ t: clapT, kind: "clap" });
  }

  // DICE-ROLL CLICK-CLACK — kicks in STRAIGHT AT BAR 41 (right out the
  // gate after the vortex stamp at bar 39-40) and carries through to
  // the end. Cluster of 3-6 tumbling wooden clicks per fire, alternating
  // hi/lo pitch ("click-clack"). Placed on beats 1 + 3 of every bar
  // post-vortex for a steady rolling clatter under the drop2 + outro.
  if (bar >= 41) {
    const c1 = barStart + barSec * 1 / 5;
    const c2 = barStart + barSec * 3 / 5;
    fireDiceRoll(dryBuf, c1, 0.28 * DRUM_GAIN);  // chill click-clack — pulled from 0.50
    fireDiceRoll(dryBuf, c2, 0.28 * DRUM_GAIN);
    events.kick.push({ t: c1, kind: "dice" });
    events.kick.push({ t: c2, kind: "dice" });
  }

  // Lead — base theme + subtle microtonal drift + per-bar octave AND
  // rhythmic-subdivision shuffles. Each bar rolls:
  //   octave    : -7 / 0 / +7 / +14 scale-degrees (down 1 → up 2 octaves)
  //   subdivision: 0.5× (half-time) / 1× / 2× (double-time)
  // gives a vastly wider melodic and rhythmic range than the fixed theme.
  if (t.lead && instrumentEnabled("lead", bar)) {
    const themeOffsetBar = bar % 4;
    const rhRoll = leadRng();
    const ocRoll = leadRng();
    const subdivMul = rhRoll < 0.18 ? 0.5 : rhRoll < 0.82 ? 1.0 : 2.0;
    const octaveShift =
      ocRoll < 0.14 ? -7 :
      ocRoll < 0.28 ?  7 :
      ocRoll < 0.36 ? 14 : 0;
    // Skip-bar — about 1/3 of bars get a syncopated rhythm: every 3rd
    // note becomes a rest, and the surviving notes get pushed back by a
    // 16th-note for that "delay skip" / behind-the-beat feel. Creates
    // the bouncing skipping melody instead of a flat march.
    // Chill: skip more bars AND thin each bar to ~half the notes so the
    // melody holds + breathes instead of running.
    const skipBar  = leadRng() < (isChill ? 0.55 : 0.34);
    const notesThisBar = Math.max(1, Math.floor(NOTES_PER_BAR_LEAD * subdivMul * (isChill ? 0.5 : 1)));
    const noteSec = barSec / notesThisBar;
    const skipDelay = noteSec * 0.18; // 16th-note nudge late
    for (let s = 0; s < notesThisBar; s++) {
      // Skip pattern — drop every 3rd slot (positions 1, 4, 7…) so the
      // remaining notes feel bunched + bouncy.
      if (skipBar && s % 3 === 1) continue;
      // For double-time wrap THEME; for half-time take every other note.
      const sourceIdx = subdivMul >= 1
        ? s % NOTES_PER_BAR_LEAD
        : Math.floor(s * (1 / subdivMul));
      const themeIdx = themeOffsetBar * NOTES_PER_BAR_LEAD + sourceIdx;
      const baseDeg = THEME[themeIdx % THEME.length];
      if (baseDeg == null) continue;
      const v = nextLeadVariation();
      if (v.skip) continue;
      // Chill: extra rests — leave space between phrases.
      if (isChill && leadRng() < 0.28) continue;
      const deg = baseDeg + v.stepShift + octaveShift;
      // On-beat notes in skip bars get nudged late so the bar feels
      // like it's lagging then catching up.
      const skipOffset = skipBar && s % 3 === 2 ? skipDelay : 0;
      // Humanize lead — ±11 ms feels expressive without losing the line.
      // Chill rides the swing pocket too, so the line lays back with the groove.
      const startSec = humanize(barStart + s * noteSec + skipOffset + swingSec + (isChill ? 0.022 : 0), 11); // laid-back pocket
      // Chill: drop the lead an OCTAVE (it sat too high) and use the
      // warm PAD voice instead of the bright plucky "lead" timbre the
      // user disliked.
      const leadMidi = scaleNoteMidi(deg, isChill ? -2 : 0) + v.cents / 100; // chill arps an octave deeper (jas)
      const inBreak = !t.kick;
      // Chill: very long sustained notes (not punctual) — ring well
      // across the rests.
      const durMul = isChill
        ? (skipBar ? 2.4 : 3.4)
        : (skipBar ? 0.65 : (inBreak ? 1.4 : 0.85));
      const durSec = noteSec * durMul;
      mixEventLeadPad(
        { startSec, midi: leadMidi, gain: LEAD_GAIN * (isChill ? 0.85 : 1), durSec },
        dryBuf,
        { preset: isChill ? "pad" : "lead", sampleRate: SAMPLE_RATE, seed: `lead:${bar}:${s}` }
      );
      events.lead.push({ t: startSec, midi: leadMidi, dur: durSec });
      leadCount++;
      if (isChill) {
        // Bring UP the harmonies on the main voice (jas) — an audible
        // diatonic 3rd + 5th above, not a faint doubling.
        for (const [hd, hg] of [[2, 0.32], [4, 0.22]]) { // lower — less "organ" (jas)
          mixEventLeadPad(
            { startSec, midi: scaleNoteMidi(deg + hd, -1) + v.cents / 100, gain: LEAD_GAIN * hg, durSec },
            dryBuf,
            { preset: "pad", sampleRate: SAMPLE_RATE, seed: `leadharm:${bar}:${s}:${hd}` }
          );
        }
      }
    }
  }

  // Bunny-bow comp (chill) — overlapping bowed/brushed notes that
  // voice-lead by NEAREST chord tone to the last one ("bounce"), with
  // occasional scale/diminished passing approaches. Long swells that
  // bleed into each other = the legato 4th-octave glue the sine-chime
  // comp was missing. Routed to duckBuf so it sits in the harmonic bed.
  // NOT the whole track: comes in waves — rests every 3rd 8-bar phrase
  // and stays out of the first phrase. Lots of bounce: nearest-tone
  // voice-leading by default, but sometimes drops WAY low, sometimes
  // WRAPS through a little scale run, sometimes springs WAY up an
  // arpeggio then falls back down (BOING). Variable strokes/durations.
  const bunnyActive = isChill && bar >= 6 && (Math.floor(bar / 8) % 3 !== 2);
  if (bunnyActive) {
    const strokes = 1 + (bunnyRng() < 0.55 ? 1 : 0) + (bunnyRng() < 0.18 ? 1 : 0); // 1–3
    const step = barSec / strokes;
    const g = 0.16;
    for (let nb = 0; nb < strokes; nb++) {
      const t0 = barStart + nb * step + (isChill ? 0.022 : 0); // laid-back human pocket (behind the grid)
      // nearest chord tone to the last note (the "bounce")
      let target = scaleNoteMidi(chordDeg, 1);
      if (bunnyPrevMidi != null) {
        let best = 1e9;
        for (const tone of [0, 2, 4, 6]) {
          for (const oc of [-1, 0, 1, 2]) {
            const m = scaleNoteMidi(chordDeg + tone, oc);
            if (m < 56 || m > 84) continue;
            const d = Math.abs(m - bunnyPrevMidi);
            if (d < best) { best = d; target = m; }
          }
        }
      }
      const roll = bunnyRng();
      if (roll < 0.12) {
        // BOING — fast arpeggio WAY up the chord, then fall back down
        const aDeg = [0, 2, 4, 7, 9, 11];
        const upN = 5;
        const aStep = step / (upN + 3);
        for (let a = 0; a < upN; a++) {
          const am = scaleNoteMidi(chordDeg + aDeg[a % aDeg.length], 2 + Math.floor(a / 3));
          fireBunnyBow(duckBuf, humanize(t0 + a * aStep, 6), am, aStep * 1.8, g * 0.85);
        }
        for (let a = 0; a < 3; a++) {
          const dm = scaleNoteMidi(chordDeg + aDeg[(upN - 1 - a) % aDeg.length], 1);
          fireBunnyBow(duckBuf, humanize(t0 + (upN + a) * aStep, 6), dm, aStep * 2.4, g * 0.7);
        }
        target = scaleNoteMidi(chordDeg, 1);
      } else if (roll < 0.30) {
        // WRAP — a little weaving scale run from prev toward target
        const base = bunnyPrevMidi ?? target;
        const dir = target >= base ? 1 : -1;
        const wn = 4;
        for (let w = 0; w < wn; w++) {
          const wm = base + dir * (w + 1) * 2 + (w % 2 ? -1 : 0);
          fireBunnyBow(duckBuf, humanize(t0 + w * (step / wn), 7), wm, (step / wn) * 1.7, g * 0.8);
        }
      } else {
        let m = target;
        if (bunnyRng() < 0.16) m -= 12 + (bunnyRng() < 0.5 ? 12 : 0); // sometimes WAY lower
        if (bunnyRng() < 0.14) m += 12;                                // little hop
        const dur = step * (1.5 + bunnyRng() * 0.6);                   // varied overlap
        if (bunnyPrevMidi != null && bunnyRng() < 0.24) {
          const dir = m >= bunnyPrevMidi ? 1 : -1;
          const pass = m - dir * (bunnyRng() < 0.5 ? 1 : 2);           // scale/dim approach
          fireBunnyBow(duckBuf, humanize(t0, 8), pass, step * 0.5, g * 0.8);
          fireBunnyBow(duckBuf, humanize(t0 + step * 0.42, 8), m, dur * 0.75, g);
        } else {
          fireBunnyBow(duckBuf, humanize(t0, 8), m, dur, g);
        }
        target = m;
      }
      events.lead.push({ t: t0, midi: target, dur: step });
      bunnyPrevMidi = target;
    }
  }

  // Grand-piano frills (chill) — sparse "here and there" held, EXTENDED
  // notes way up in the UPPER register of the sampled piano: a high
  // chord tone + a sparkle a 3rd/5th above, rung long. A little glint.
  if (isChill && bar >= 8 && bunnyRng() < 0.13) {
    const hi = scaleNoteMidi(chordDeg + (bunnyRng() < 0.5 ? 4 : 2), 3); // upper register
    const t0 = barStart + (bunnyRng() < 0.5 ? 0 : beatSec);
    const dur = barSec * (1.6 + bunnyRng() * 1.4);                      // held / extended
    mixEventPiano({ startSec: t0, midi: hi, gain: 0.12, durSec: dur }, dryBuf, SAMPLE_RATE);
    if (bunnyRng() < 0.6) {
      mixEventPiano({ startSec: t0 + 0.05, midi: hi + (bunnyRng() < 0.5 ? 3 : 7), gain: 0.08, durSec: dur * 0.9 }, dryBuf, SAMPLE_RATE);
    }
    events.lead.push({ t: t0, midi: hi, dur });
  }

  // Field-recording GRAINS — beat-aligned, pitched to the chord, a
  // magical textural sprinkle from the 18 N Main St room. A reading
  // head wanders the recording across the track; each grain is tuned
  // to a chord tone (octave-down = lush, up = sparkle). Routed to the
  // sfx bus (wide pan = enveloping). Sparse-open aware via chillBuild.
  if (ambientBuf && isChill && chillBuild > 0.05 && bar >= 4) {
    const nG = bunnyRng() < 0.55 ? 1 : (bunnyRng() < 0.4 ? 2 : 0);
    for (let gi = 0; gi < nG; gi++) {
      const beat = Math.floor(bunnyRng() * METER);
      const t0 = humanize(barStart + beat * beatSec, 10);
      // reading head crawls through the whole recording over the track
      const srcSec = ((bar / TOTAL_BARS) * (ambientBuf.length / SAMPLE_RATE)
        + bunnyRng() * 3) % (ambientBuf.length / SAMPLE_RATE);
      // tune to a chord tone; mostly down (lush), sometimes up (sparkle)
      const tone = [0, 2, 4, 7][Math.floor(bunnyRng() * 4)];
      const semis = (scaleNoteMidi(chordDeg + tone, bunnyRng() < 0.7 ? -1 : 1) - 57);
      const pitch = Math.pow(2, semis / 12);
      const long = bunnyRng() < 0.4;
      const dur = long ? 0.8 + bunnyRng() * 1.2 : 0.18 + bunnyRng() * 0.22;
      const g = (long ? 0.13 : 0.18) * chillBuild; // pulled back for clarity
      fireAmbGrain(sfxDryBuf, t0, srcSec, dur, pitch, g);
      events.sfx.push({ t: t0, name: "amb-grain", dur });
    }
  }

  // Riser — fires once at the start of a build section.
  if (t.riser && localBar === 0 && instrumentEnabled("riser", bar)) {
    fireRiser(dryBuf, barStart, barSec * sectionLen, 0.40);
    riserCount++;
  }

  // Piano stabs — backbeat (beats 2 and METER) during drops. Adds a
  // bright harmonic accent on top of the supersaw lead.
  if (t.piano && instrumentEnabled("piano", bar)) {
    if (isChill) {
      // ENRICHED chill piano (jas: "more melody and hold and chord
      // progressions there"): a SUSTAINED chord hold + a little
      // melodic top line that walks chord/scale tones through the
      // progression. progressionAt() already drives the harmony.
      const cd = progressionAt(bar);
      const holdT = humanize(barStart, 6);
      for (const m of triad) {
        mixEventPiano({ startSec: holdT, midi: m + 12, gain: PIANO_GAIN * 0.78, durSec: barSec * 1.7 }, duckBuf, SAMPLE_RATE);
        events.piano.push({ t: holdT, midi: m + 12, dur: barSec * 1.7 });
        pianoCount++;
      }
      const mel = [0, 4, 2, 5, 4, 2];               // scale-degree contour over the chord
      const nMel = 3 + (bar % 2);                    // 3–4 little keys per bar
      for (let i = 0; i < nMel; i++) {
        const md = scaleNoteMidi(cd + mel[(bar + i) % mel.length], 2); // high "little keys"
        const mt = humanize(barStart + (i / nMel) * barSec, 8);
        // jas: some REVERSED, some pitch-BENT, some held longer w/ sustain.
        const r = noiseRng();
        const reverse = r < 0.30;
        const sustainLong = !reverse && noiseRng() < 0.35;
        const bend = noiseRng() < 0.35 ? (noiseRng() < 0.5 ? -2 : 2) * (0.5 + noiseRng()) : 0;
        const mdur = (barSec / nMel) * (1.3 + 0.4 * (i % 2)) * (sustainLong ? 2.4 : 1) * (reverse ? 1.6 : 1);
        mixEventPiano({ startSec: mt, midi: md, gain: PIANO_GAIN * (reverse ? 0.5 : 0.6), durSec: mdur, reverse, bend }, duckBuf, SAMPLE_RATE);
        events.piano.push({ t: mt, midi: md, dur: mdur });
        pianoCount++;
      }
    } else {
      const stabBeats = isWaltz ? [1] : [1, 3]; // 3/4: beat 2 only; 4/4: 2 and 4
      for (const sb of stabBeats) {
        // One humanized stab time for the whole chord so the triad
        // stays struck together (a "hand" of stabbed notes).
        const stabSec = humanize(barStart + sb * beatSec, 9);
        for (const m of triad) {
          mixEventPiano(
            { startSec: stabSec, midi: m + 12, gain: PIANO_GAIN, durSec: beatSec * 0.6 },
            duckBuf,
            SAMPLE_RATE
          );
          events.piano.push({ t: stabSec, midi: m + 12, dur: beatSec * 0.6 });
          pianoCount++;
        }
      }
    }
  }

  // Sinebells — counter-melody arpeggio with a "delay skip" feel.
  // Each bar fires 4 chord-tone notes on dotted-skip positions, so the
  // bells dance rather than hold. Offsets in 8ths of a bar:
  //   0,    3,    5,    8   (out of 8) — skips the strict 1-2-3-4 grid
  // The 3rd and 5th positions are intentionally "delayed" so the
  // pattern feels off-the-beat / skipping along the rhythm.
  if (t.bells && instrumentEnabled("bells", bar)) {
    // Bar-position determined chord tones — root, 3rd, 5th, octave —
    // give the arpeggio harmonic shape rather than a single drone.
    // Patterns rotate by bar-group so the bells don't repeat every
    // 2 bars (the old ascending/descending toggle). Chill mode needs
    // longer-cycle variation since it has 12-24 bar sections.
    const patternIdx = Math.floor(bar / 2) % 6;
    const PATTERNS = [
      // ascending close
      { degs: [4, 6, 2, 7], octs: [1, 1, 2, 2] },
      // descending close
      { degs: [7, 4, 2, 6], octs: [2, 2, 1, 1] },
      // open-voiced wide jump
      { degs: [0, 4, 2, 7], octs: [2, 1, 2, 1] },
      // high register
      { degs: [4, 2, 7, 6], octs: [2, 2, 2, 1] },
      // low register
      { degs: [0, 4, 6, 2], octs: [1, 1, 0, 1] },
      // zig-zag
      { degs: [4, 7, 2, 6], octs: [1, 2, 2, 1] },
    ];
    const pat = PATTERNS[patternIdx];
    const arpDegrees = pat.degs.map((d) => chordDeg + d);
    let arpOctaves = pat.octs.slice();
    // CHILL mode bell variation — every 8 bars, transpose the whole
    // arp by one octave for a longer-cycle melodic shape so the bells
    // don't feel locked. Cycle: +0, +0, -1, +0, +1, +0, -1, +0, ...
    if (isChill) {
      const chillCycle = Math.floor(bar / 8) % 4;
      const chillShift = [0, -1, 1, 0][chillCycle];
      arpOctaves = arpOctaves.map((o) => o + chillShift);
    }
    // Eighth-note positions inside the bar, with delayed-skip offsets.
    // 3/4 bar = 6 eighths;  4/4 bar = 8 eighths. Scale by METER.
    const eighthSec = barSec / (METER * 2);
    const positions = [0, 3, 5, 8].map((p) => p * eighthSec * 0.85);
    // Tail-pad transition — in the LAST ~10 s of music, the bells
    // morph from struck-bell arps into slow opening-pad swells. They
    // drop a full octave, take a long slow attack, and ring with a
    // much longer T60 so they bleed into the tape-stop as a warm
    // sub-octave drone. Goal: by 1:17-ish the bells are no longer
    // arpeggiating, they're a held sub pad behind the shutdown.
    const TAIL_PAD_START = totalSec - 10;
    const inTailPad = (barStart + barSec * 0.5) > TAIL_PAD_START;
    // Smooth ramp 0 → 1 across the 10-second window so the morph isn't
    // abrupt — the bell character bends down rather than cutting.
    const tailPadT = inTailPad
      ? Math.min(1, Math.max(0, (barStart + barSec * 0.5 - TAIL_PAD_START) / 10))
      : 0;
    for (let i = 0; i < arpDegrees.length; i++) {
      // Humanize bell arp — ±10 ms gives the dotted-skip pattern an
      // organic rubato feel instead of a quantized music-box.
      const startSec = humanize(barStart + Math.min(positions[i], barSec * 0.92), 10);
      // Lower the octave by 1 in tail-pad mode. arpOctaves are already
      // {0,1,2} so subtract 1 → {-1,0,1}, dropping the whole arp down.
      const padOctaveOffset = tailPadT > 0 ? -1 : 0;
      const midi = scaleNoteMidi(arpDegrees[i], arpOctaves[i] + padOctaveOffset);
      // Hold time stretches up in tail-pad mode: from 0.85 bar to
      // ~2.5 bars on the held first note, so the pad sustain bleeds
      // organically into the tape-stop.
      const padDurMul = 1 + tailPadT * 2.0;
      const durSec = barSec * (i === 0 ? 0.85 : 0.45 - i * 0.05) * padDurMul;
      const gainMul = i === 0 ? 1.0 : 0.65 - i * 0.06;
      // Slow opening swell attack in tail-pad mode: 12 ms → 380 ms.
      const attackSec = 0.012 + tailPadT * 0.37;
      // Decay scale: 1× struck → 3× slow pad ring.
      const decayScale = 1 + tailPadT * 2.0;
      // CHILL bells — very LOW tone (drop 2 octaves), super DRAWN
      // OUT (long attack + 4× decay scale), quieter, and routed to
      // a dedicated bus that gets a slow flanger before mixing in.
      // They sit underneath the bed as a low harmonic wash instead
      // of competing for attention.
      let chillMidi = midi;
      let chillGain = BELLS_GAIN * gainMul;
      let chillAttack = attackSec;
      let chillDecay = decayScale;
      let chillTail = 4 + tailPadT * 5;
      let bellTarget = dryBuf;
      if (isChill) {
        chillMidi = midi - 24;                  // drop 2 octaves — sub-low harmonic wash
        chillGain = BELLS_GAIN * gainMul * 0.55; // quieter, less attention
        chillAttack = 0.45;                      // slow swell instead of struck attack
        chillDecay = 6.5;                        // much longer T60 — super drawn out
        chillTail = 13;                          // very long ring tail
        bellTarget = chillBellBuf;               // dedicated bus for the flanger
      }
      mixEventSinebell(
        { startSec, midi: chillMidi, gain: chillGain, durSec, attackSec: chillAttack, decayScale: chillDecay, tailSec: chillTail },
        bellTarget,
        SAMPLE_RATE
      );
      events.bells.push({ t: startSec, midi: chillMidi, dur: durSec });
      bellsCount++;
    }
  }

  // Square arpeggio — a voice that transforms over the track:
  //   Phase 1 (~0-4 s of arp life): bright square-wave 16ths, the
  //     classic intro expression.
  //   Phase 2 (4 s → ~8 s): crossfade square → sine, OCTAVE DOWN,
  //     volume DIPS WAY back, notes ELONGATE to ~half rate.
  //   Phase 3 (8 s → end of break1, ~30 s wall-clock): low elongated
  //     sine voice tapers to silence — a different instrument
  //     entirely, lingering as the bed takes over.
  //
  // Arp enters at music entry (intro bar 0) — bright square arpeggio
  // crescendos in over 4 s, hits full volume by mid-intro, then runs
  // through break1 with the timbre/octave/density transformation.
  const ARP_START_SEC = OPENING_PREFIX_SEC;
  if (sec === "intro" || sec === "break1") {
    const baseSteps = METER === 3 ? 12 : 16;
    const arpPattern = METER === 3
      ? [0, 2, 4, 6, 4, 2, 0, 4, 2, 6, 4, 2]
      : [0, 2, 4, 6, 4, 2, 0, 4, 2, 6, 4, 2, 0, 4, 6, 4];
    // Sample the arp time at the bar's midpoint to pick the phase.
    const tArp = (barStart + barSec * 0.5) - ARP_START_SEC;
    // Phase decision:
    //   tArp <= 4   → Phase 1 (pure square, full density, normal vol)
    //   4 < tArp < 8 → Phase 2 (crossfade, octave down ramps in)
    //   tArp >= 8   → Phase 3 (sine only, elongated, fading out)
    let waveMix;  // 0 = pure square, 1 = pure sine
    let octShift; // semitones to subtract
    let steps;    // notes per bar
    let durMul;   // duration multiplier per note (relative to stepSec)
    let fadeMul;
    if (tArp <= 4.0) {
      // Phase 1 — bright square, linear crescendo over the first 4 s.
      waveMix = 0;
      octShift = 0;
      steps = baseSteps;
      durMul = 0.85;
      fadeMul = Math.max(0.10, Math.min(1, tArp / 4.0));
    } else if (tArp < 8.0) {
      // Phase 2 — crossfade. 4 → 8 seconds of arp life.
      const u = (tArp - 4.0) / 4.0; // 0 → 1
      waveMix = u;
      octShift = 12 * u; // ramps from 0 to one octave down
      steps = Math.round(baseSteps - u * (baseSteps - baseSteps / 2));
      durMul = 0.85 + u * 0.65; // 0.85 → 1.5 (longer notes)
      fadeMul = 1.0 - u * 0.75; // dips from 1.0 → 0.25
    } else {
      // Phase 3 — transformed voice. Sine, octave down, elongated.
      waveMix = 1;
      octShift = 12;
      steps = Math.max(3, Math.floor(baseSteps / 2));
      durMul = 1.5;
      // Across break1, fade from 0.25 → 0 by end of section. With
      // break1 ~12 bars, this happens between localBar ~3 and end.
      const breakProgress = sec === "break1" ? localBar / Math.max(1, sectionLen - 1) : 0;
      fadeMul = 0.25 * Math.max(0, 1 - breakProgress);
    }
    // Chill mix: the arp is a pure SINE one octave down for its entire
    // life — no bright square intro expression. The phase logic still
    // governs the crescendo / elongation / fade dynamics; only timbre
    // and octave are pinned. trancenwaltz keeps the square→sine morph.
    if (isChill) {
      waveMix = 1;     // sine only — never the square
      octShift = 12;   // one octave down throughout
      if (sec === "intro") {
        // START: fast + CACOPHONOUS — dense, short, dissonant shimmer
        // (a chaotic open that then settles into the meditative arp).
        steps = Math.max(baseSteps, Math.round(steps * 2));
        durMul = durMul * 0.5;
      } else {
        // Settle: half the notes, long holds — breathes.
        steps = Math.max(3, Math.round(steps / 2));
        durMul = durMul * 2.0;
      }
    }
    if (fadeMul > 0.01) {
      const stepSec = barSec / steps;
      for (let sIdx = 0; sIdx < steps; sIdx++) {
        // Chill: extra rests so the arp skips + leaves space — but NOT
        // during the intro (the start stays dense + cacophonous).
        if (isChill && sec !== "intro" && noiseRng() < 0.22) continue;
        // Chill swings its off-step (odd) notes into the same late pocket
        // as the hats/sub. Humanize the arp — ±6 ms keeps it alive.
        const swung = (isChill && sIdx % 2 === 1) ? swingSec : 0;
        const startSec = humanize(barStart + sIdx * stepSec + swung, 6);
        const patIdx = sIdx % arpPattern.length;
        const deg = chordDeg + arpPattern[patIdx];
        const octBump = sIdx % 4 < 2 ? 1 : 2;
        const midi = scaleNoteMidi(deg, octBump) - octShift;
        const freq = 440 * Math.pow(2, (midi - 69) / 12);
        const synth = makeBufferSynth(dryBuf, startSec, SAMPLE_RATE, noiseRng);
        // Emit BOTH square and sine voices with complementary
        // weights — crossfade the timbre cleanly without needing a
        // morph waveform.
        const _cb = isChill ? chillBuild : 1;          // sparse open → ramp in
        const sqVol = (1 - waveMix) * 0.20 * fadeMul * _cb;
        const siVol = waveMix       * 0.32 * fadeMul * _cb; // sine gets a small boost to match perceived loudness
        if (sqVol > 0.001) {
          synth.synth({ type: "square", tone: freq, duration: stepSec * durMul, volume: sqVol, attack: 0.003, decay: stepSec * 0.55 });
        }
        if (siVol > 0.001) {
          // Chill: the sine "chime" rings MUCH longer (≈3× duration,
          // long decay) so it sustains + overlaps instead of pinging.
          const aDur = isChill ? stepSec * durMul * 3.0 : stepSec * durMul;
          const aDec = isChill ? aDur * 1.4 : stepSec * 0.85;
          synth.synth({ type: "sine", tone: freq, duration: aDur, volume: siVol, attack: 0.012, decay: aDec });
        }
        // Chill intro CACOPHONY — a slightly-detuned tritone an
        // octave+ up, clashing against the root for a dissonant,
        // chaotic shimmer at the very start only.
        if (isChill && sec === "intro" && siVol > 0.001) {
          const clashTone = freq * Math.pow(2, (12 + 6) / 12) * 1.013;
          synth.synth({ type: "sine", tone: clashTone, duration: stepSec * durMul, volume: siVol * 0.55, attack: 0.004, decay: stepSec * 0.55 });
        }
        // Emit the arp note so the visualizer draws it as its own lane
        // (the intro square arp was previously invisible).
        if (sqVol > 0.001 || siVol > 0.001) {
          events.arp.push({ t: startSec, midi, dur: stepSec * durMul });
        }
      }
    }
  }

  // Snare roll — subdivisions double across the section. Volume range
  // pulled back so it doesn't clip when stacked under the sub bass +
  // lead. Earlier code peaked at 0.76 which slammed the mix.
  if (t.snareRoll && !isLastBar && instrumentEnabled("snareRoll", bar) && !KICK_ONLY) {
    const progress = localBar / Math.max(1, sectionLen - 1);
    const subdiv = progress < 0.33 ? METER * 2 : progress < 0.66 ? METER * 4 : METER * 8;
    for (let h = 0; h < subdiv; h++) {
      // Humanize snare-roll subdivisions — small ±4 ms keeps the roll
      // tight while the hand-played feel comes through.
      const startSec = humanize(barStart + (h / subdiv) * barSec, 4);
      const v = 0.12 + (h / subdiv) * 0.32; // peaks ~0.44 instead of 0.80
      fireDrum(dryBuf, startSec, "d", { volume: v * DRUM_GAIN * 0.85 });
      events.snare.push({ t: startSec, dur: 0.025, kind: "roll" });
      snareRollCount++;
    }
  }

  // Snare-roll pre-roll — the 4 bars BEFORE every build section get a
  // very quiet whispering snare roll that crescendos into the build.
  // Gives the listener a sense the climb is coming, without the
  // dramatic clipping the build-section snare alone causes.
  //
  // Kick pre-roll — same 4-bar window, but for the KICK. Single hit
  // on beat 1 at -4 bars (whisper), then progressively more hits and
  // louder, so the kick has a narrative entry rather than slamming
  // in at the build downbeat. Skipped in chill mode — the chill
  // kicks come in naturally without a crescendo build.
  if (!isChill) {
    const nextBuildStart = (() => {
      for (const r of sectionRanges) {
        if (r.name.startsWith("build") && r.startBar > bar) return r.startBar;
      }
      return -1;
    })();
    const barsAhead = nextBuildStart - bar;
    // Pre-roll window EXTENDED from 4 → 6 bars so the percussion
    // textural shift starts emerging deeper into break1. The
    // crescendo is gentler and longer — the listener feels the build
    // "wake up" gradually instead of snapping on 4 bars before.
    const PREROLL_BARS = 6;
    if (barsAhead > 0 && barsAhead <= PREROLL_BARS) {
      const prerollT = (PREROLL_BARS - barsAhead) / PREROLL_BARS; // 0 → 0.83
      const subdiv = METER * 2;
      for (let h = 0; h < subdiv; h++) {
        // Humanize pre-roll subdivisions — same ±4 ms as the main roll.
        const startSec = humanize(barStart + (h / subdiv) * barSec, 4);
        const v = prerollT * (0.05 + (h / subdiv) * 0.18);
        if (v < 0.01) continue;
        fireDrum(dryBuf, startSec, "d", { volume: v * DRUM_GAIN * 0.7 });
        events.snare.push({ t: startSec, dur: 0.025, kind: "preroll" });
        snareRollCount++;
      }
      // ── kick pre-roll — SMOOTH crescendo across the 6 bars ──
      // Continuous probability + velocity curves; per-beat in each bar:
      //   probability of firing rises 0.20 → 1.00 across the 6 bars
      //   velocity rises 0.10 → 0.95 across the same window
      //   beat 1 always fires; other beats roll dice
      const arrivalT = 1 - (barsAhead / PREROLL_BARS); // 0 → ~0.83
      for (let beat = 0; beat < METER; beat++) {
        const beatT = arrivalT + (beat / METER) * (1 / PREROLL_BARS);
        const fireProb = 0.20 + beatT * 0.80; // 0.20 → 1.0
        const beatVel  = 0.10 + beatT * 0.85; // 0.10 → 0.95
        // Always fire beat 1; roll dice for others.
        const fires = beat === 0 || noiseRng() < fireProb;
        if (!fires) continue;
        // Humanize the pre-roll kicks too — looser ±9 ms because these
        // are sparse and crescendoing, so the sloppy timing helps the
        // narrative of "drummer waking up to the build".
        const kickT = humanize(barStart + beat * beatSec, 9);
        fireDrum(dryBuf, kickT, "c", { volume: beatVel * DRUM_GAIN });
        kickTimes.push(kickT);
        events.kick.push({ t: kickT, kind: "preroll" });
        kickCount++;
      }
    }
  }
}

// Fire all machine-gun bursts into their OWN bus so master bitcrush
// (which lives on `out`) doesn't shred the depth. Skipped in chill
// mode — no machine guns in the study-vibes mix.
if (!isChill) {
  for (const mg of machineGunHits) {
    fireMachineGun(mgBuf, mg.startSec, mg.durSec, mg.gain);
    events.sfx.push({ t: mg.startSec, name: "machine-gun", dur: mg.durSec });
    machineGunCount++;
  }
}

// Drop impact — stacked sine bass cluster at the start of every drop
// section. Lives on the SFX bus (mgBuf) so it survives both the
// section dynamic envelope and the master bitcrush.
//
// CHILL mode gets a soft drop: sub bass donk + chord ding, NO roar,
// NO machine guns, NO massive impact. Just a gentle "arrival" cue
// when each drop section enters — fun without being a hard drop.
for (const r of sectionRanges) {
  if (isChill && r.name.startsWith("drop")) {
    events.dropImpact.push({ t: r.startSec, name: r.name });
    events.sfx.push({ t: r.startSec, name: `chill-drop-${r.name}`, dur: 0.45 });
    const chordDeg = progressionAt(r.startBar);
    // Gentle chord-tone ding — root + 5th, sine + 2nd harmonic, no
    // pitch wiggle, longer decay. Sits soft in the mix.
    const dingMidis = [chordDeg, chordDeg + 4];
    for (const dm of dingMidis) {
      const dingFreq = 440 * Math.pow(2, (scaleNoteMidi(dm, 1) - 69) / 12);
      const synth = makeBufferSynth(sfxDryBuf, r.startSec, SAMPLE_RATE, noiseRng);
      synth.synth({ type: "sine", tone: dingFreq,     duration: 0.45, volume: 0.22, attack: 0.005, decay: 0.42 });
      synth.synth({ type: "sine", tone: dingFreq * 2, duration: 0.30, volume: 0.07, attack: 0.005, decay: 0.28 });
    }
    // Soft sub bass donk — single octave-down note, gentle decay.
    const donkMidi = scaleNoteMidi(chordDeg, -1);
    fireSubBass(bassBuf, r.startSec, donkMidi, 2.0, 0.65 * BASS_GAIN, 0.2, false);
    dropImpactCount++;
    continue;
  }
  if (!isChill && r.name.startsWith("drop")) {
    const chordDeg = progressionAt(r.startBar);
    const impactMidi = scaleNoteMidi(chordDeg, -2); // root, low octave
    fireDropImpact(mgBuf, r.startSec, impactMidi, 1.25);
    events.sfx.push({ t: r.startSec, name: "drop-impact+lion", dur: 0.5 });
    // BIG VIBEY DONK — sustained low bass that rings under the drop
    // entry so it doesn't feel like blip emptiness after the hush.
    // Drop1 gets the biggest donk (3.5 s decay), drop2 a slightly
    // shorter one. Multiple sine voices stacked at sub octaves with
    // a fifth-up harmony for movement.
    const donkGain = r.name === "drop1" ? 1.6 : 1.2;
    const donkDur  = r.name === "drop1" ? 3.5 : 2.5;
    fireSubBass(bassBuf, r.startSec, impactMidi, donkDur, donkGain * BASS_GAIN, 0.3, true);
    // Second deeper donk — another full octave down for SEISMIC weight.
    fireSubBass(bassBuf, r.startSec, impactMidi - 12, donkDur * 0.85, donkGain * BASS_GAIN * 0.75, 0, false);
    // Lion roar from ac-native zoo bank — pitched down for BIG drop
    // weight. BOOSTED gain (1.4 → 2.6) and routed to sfxDryBuf so the
    // roar isn't smeared into the distance-reverb tail.
    mixZooSample(
      { startSec: r.startSec, name: "lion", gain: 2.6, pitchRatio: 0.55 },
      sfxDryBuf,
      SAMPLE_RATE
    );
    // CHORDIFIED ding — fire the pitch-wiggle ding voice across four
    // chord-tones (root + 3rd + 5th + octave) so the drop bleep
    // sounds spacious rather than two-tone. Routed dry so it punches
    // through the post-hush silence.
    const chordTones = [
      scaleNoteMidi(chordDeg,     1), // root, oct +1
      scaleNoteMidi(chordDeg + 2, 1), // 3rd
      scaleNoteMidi(chordDeg + 4, 1), // 5th
      scaleNoteMidi(chordDeg,     2), // octave
    ];
    for (const m of chordTones) {
      // Treat each chord-tone as a (sine + 2nd harmonic) bell with
      // the iMessage-style 6-cycle pitch wiggle on the tail.
      const f = 440 * Math.pow(2, (m - 69) / 12);
      const steady = makeBufferSynth(sfxDryBuf, r.startSec, SAMPLE_RATE, noiseRng);
      steady.synth({ type: "sine", tone: f,     duration: 0.20, volume: 1.10 * 0.55, attack: 0.002, decay: 0.18 });
      steady.synth({ type: "sine", tone: f * 2, duration: 0.15, volume: 1.10 * 0.16, attack: 0.003, decay: 0.14 });
      const wiggleSteps = 22;
      const wiggleDur   = 0.35;
      for (let i = 0; i < wiggleSteps; i++) {
        const sf = i / wiggleSteps;
        const wobble = Math.sin(sf * Math.PI * 2 * 6);
        const wTone = f * Math.pow(2, (wobble * 1.2) / 12);
        const stepStart = r.startSec + 0.18 + sf * wiggleDur;
        const stepDur = (wiggleDur / wiggleSteps) + 0.04;
        const wiggle = makeBufferSynth(sfxDryBuf, stepStart, SAMPLE_RATE, noiseRng);
        wiggle.synth({
          type: "sine",
          tone: wTone,
          duration: stepDur,
          volume: 1.10 * 0.36 * (1 - sf * 0.85),
          attack: 0.002,
          decay: stepDur * 0.85,
        });
      }
    }
    dropImpactCount++;
  }
}

// ── drone flybys (drop1 → break2 bridge, ~45-50 s) ──────────────────
// Slower, harmonic flybys. Skipped in chill mode — no whooshes.
if (!isChill) {
  const flybyCount = 4;
  // Drones run inside break2 (post-prefix break2 ≈ 50.25-58.13 s).
  // Shifted with the music by OPENING_PREFIX_SEC so they stay
  // musically aligned to the section structure.
  const startSec = 50.5 + OPENING_PREFIX_SEC;
  const endSec   = 55.0 + OPENING_PREFIX_SEC;
  const spanSec = endSec - startSec;
  // Each flyby anchored on a chord tone — root (deg 0), 5th (deg 4),
  // octave (deg 7), back to root — climbs harmonically.
  const flybyDegrees = [0, 4, 7, 0];
  // Reference chord at 45 s = bar 34 in the 137.143 bpm waltz
  const refBar = Math.floor(startSec / barSec);
  const refChord = progressionAt(refBar);
  for (let f = 0; f < flybyCount; f++) {
    const tFraction = flybyCount > 1 ? f / (flybyCount - 1) : 0;
    const baseT = startSec + tFraction * (spanSec - 1.8);
    events.sfx.push({ t: baseT, name: "drone-flyby", dur: 1.8 });
    // Pitch the flyby to a chord tone, octave +2 (whistle-band).
    const flybyMidi = scaleNoteMidi(refChord + flybyDegrees[f], 2);
    const basePitch = 440 * Math.pow(2, (flybyMidi - 69) / 12);
    const flybyDur = 1.8;   // slow drone — 3× the previous speed
    const flybyGain = 0.28;
    const sweepSteps = 36;
    for (let i = 0; i < sweepSteps; i++) {
      const sf = i / sweepSteps;
      // Gentler doppler curve: 1.0× → 1.08× → 0.94× (less extreme).
      const dopplerCurve = sf < 0.5
        ? 1.0 + 0.08 * (sf * 2)
        : 1.08 - 0.14 * ((sf - 0.5) * 2);
      const stepStart = baseT + sf * flybyDur;
      const stepDur = flybyDur / sweepSteps + 0.08;
      // Longer, smoother amplitude envelope — quieter ride.
      let envAmp;
      if (sf < 0.35)        envAmp = sf / 0.35;
      else if (sf > 0.65)   envAmp = (1 - sf) / 0.35;
      else                  envAmp = 1.0;
      const voice = makeBufferSynth(duckBuf, stepStart, SAMPLE_RATE, noiseRng);
      voice.synth({
        type: "sine",
        tone: basePitch * dopplerCurve,
        duration: stepDur,
        volume: flybyGain * envAmp,
        attack: 0.004,
        decay: stepDur * 0.90,
      });
      // 5th-up harmony layer — chord tone above the drone for density.
      const harmMidi = scaleNoteMidi(refChord + flybyDegrees[f] + 4, 2);
      const harmFreq = 440 * Math.pow(2, (harmMidi - 69) / 12);
      voice.synth({
        type: "sine",
        tone: harmFreq * dopplerCurve,
        duration: stepDur,
        volume: flybyGain * envAmp * 0.45,
        attack: 0.005,
        decay: stepDur * 0.90,
      });
    }
  }
}

// ── flute whistle sings along with the SECOND "computer" ────────────
// Lyric: i / A / love / computer / computer / C / surviving / death.
// At 92 bpm × 0.652 s/beat, the SECOND "computer" lands ~9.13 s into
// the vocal stem; vocal starts at OPENING_PREFIX_SEC (2.7 s) →
// wall-clock 11.83 s.
if (isChill) {
  // Chill whistle: a slow, LOW tone that wanders the WHOLE track on its
  // OWN rhythm — irregular, non-bar-aligned spacing, with varied pitch /
  // length each time so it never loops. Long sustained tones (engages
  // fireWhistle's vibrato path), pitched two octaves below the old chill
  // whistle. A ghostly recurring sigh, not a one-off blip.
  const wEnd = totalSec - 8;
  let wt = 9 + noiseRng() * 4;             // first entry ~9–13 s
  let walk = 0;                             // slow melodic drift
  while (wt < wEnd) {
    const wbar = Math.max(0, Math.floor((wt - OPENING_PREFIX_SEC) / barSec));
    const wchord = progressionAt(wbar);
    walk += Math.round((noiseRng() - 0.5) * 3);
    if (walk > 4) walk = 4;
    if (walk < -4) walk = -4;
    const deg = wchord + [0, 2, 4, 6][Math.abs(walk) % 4];
    const oct = noiseRng() < 0.5 ? -1 : 0;
    const midi = scaleNoteMidi(deg, oct) - 12;   // much lower
    const dur = 2.6 + noiseRng() * 2.8;          // 2.6–5.4 s — very slow
    fireWhistle(sfxDryBuf, wt, midi, dur, 0.16 + noiseRng() * 0.06);
    events.sfx.push({ t: wt, name: "whistle", dur });
    wt += dur + 4 + noiseRng() * 9;              // irregular 4–13 s gap
  }
} else {
  const compStart = 11.83;
  const compBar = Math.floor(compStart / barSec);
  const compChord = progressionAt(compBar);
  // SKIPPY 3-note phrase — short staccato whistle notes tracking the
  // punchy "com-PU-ter" syllables of the vocal.
  const compPhrase = [
    { deg: 0, oct: 1, dur: 0.18, gap: 0.08 },
    { deg: 2, oct: 1, dur: 0.18, gap: 0.08 },
    { deg: 4, oct: 1, dur: 0.32, gap: 0.0  },
  ];
  let t = compStart;
  for (const n of compPhrase) {
    const midi = scaleNoteMidi(compChord + n.deg, n.oct);
    fireWhistle(sfxDryBuf, t, midi, n.dur, 0.32);
    events.sfx.push({ t, name: "whistle", dur: n.dur });
    t += n.dur + n.gap;
  }
}

// ── flute whistle phrase at build2 (~58 s) ───────────────────────────
// Cook/STK physically-modeled whistle approximation playing a short
// 4-note melodic phrase across the build2 entry. Adds a cool tonal
// embellishment that lifts the bed entering drop2.
{
  const whistleStartSec = 58.0;
  const chordAtWhistle = progressionAt(Math.floor(whistleStartSec / barSec));
  // Phrase: chord root (oct +1) → 3rd → 5th → root (oct +2). Climbs
  // a chord-tone arpeggio over ~4 s, ending right at drop2 entry.
  const phrase = [
    { deg: 0, oct: 1, dur: 0.95 },
    { deg: 2, oct: 1, dur: 0.95 },
    { deg: 4, oct: 1, dur: 0.95 },
    { deg: 0, oct: 2, dur: 1.30 }, // soar into the drop
  ];
  let t = whistleStartSec;
  for (const n of phrase) {
    const midi = scaleNoteMidi(chordAtWhistle + n.deg, n.oct);
    // Quiet — sits underneath the build bed rather than soaring on top.
    fireWhistle(sfxDryBuf, t, midi, n.dur, 0.18);
    events.sfx.push({ t, name: "whistle", dur: n.dur });
    t += n.dur;
  }
}

// (Helicopter removed — too loud, distracting from the break2 mix.
// Tried noise-pulse and chord-tone versions; both crowded the
// breakdown. Birds + cats handle the texture in this window.)

// ── jeffrey "booty" SUNG melodic vocal — skipped in chill mode ──────
if (!isChill) {
  const BOOTY_PATH = `${REPO}/pop/dance/out/.booty-sung.mp3`;
  if (existsSync(BOOTY_PATH)) {
    const tmpRaw = `${dirname(OUT_PATH)}/.booty.f32.raw`;
    const dec = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", BOOTY_PATH,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpRaw,
    ]);
    if (dec.status === 0 && existsSync(tmpRaw)) {
      const raw = readFileSync(tmpRaw);
      const booty = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
      let bpeak = 0;
      for (let i = 0; i < booty.length; i++) { const a = Math.abs(booty[i]); if (a > bpeak) bpeak = a; }
      const norm = bpeak > 0 ? 0.95 / bpeak : 1.0;
      // Place at drop1 bar 26 (= 8 bars into drop1) so it lands ~37 s
      // and the ascending pitch peaks during the drop's busiest beat.
      const bootyStartSec = OPENING_PREFIX_SEC + barSec * 26;
      const bootyDurSec = booty.length / SAMPLE_RATE;
      events.vox.push({ t: bootyStartSec, name: "booty", dur: bootyDurSec });
      const bootyGain = 0.85;
      const startIdx = Math.floor(bootyStartSec * SAMPLE_RATE);
      const fadeS = Math.floor(0.04 * SAMPLE_RATE);
      for (let i = 0; i < booty.length; i++) {
        const j = startIdx + i;
        if (j < 0 || j >= sfxDryBuf.length) break;
        let fade = 1;
        if (i < fadeS) fade = i / fadeS;
        else if (i > booty.length - fadeS) fade = Math.max(0, (booty.length - i) / fadeS);
        sfxDryBuf[j] += booty[i] * norm * bootyGain * fade;
      }
      try { unlinkSync(tmpRaw); } catch {}
      console.log(`  booty · sung melodic stem at ${bootyStartSec.toFixed(2)}s (drop1 bar 26)`);
    }
  }
}

// ── kitty meows (48-53 s) ───────────────────────────────────────────
// Cat samples from the ac-native zoo bank, scattered through break2.
// Skipped in chill mode — chill outdoor vibe stays clean without
// cat/bird zoo samples (which are positioned at narrative-specific
// timestamps that don't line up with the chill arrangement anyway).
if (!isChill) {
  // Cats pulled 5 s earlier than before and pitched HIGH — kittens
  // chirping rather than full-grown growls. Birds tweet through the
  // same window so they read as a chorus together.
  // Widely-varied pitches — from chunky low growl to tiny kitten
  // squeak (0.65× to 2.4×) so each meow reads as a distinct cat.
  const meows = [
    { offsetSec: 43.10 + OPENING_PREFIX_SEC, pitch: 1.55, gain: 0.30 },
    { offsetSec: 44.20 + OPENING_PREFIX_SEC, pitch: 2.40, gain: 0.22 },
    { offsetSec: 45.60 + OPENING_PREFIX_SEC, pitch: 0.78, gain: 0.34 },
    { offsetSec: 46.70 + OPENING_PREFIX_SEC, pitch: 1.95, gain: 0.24 },
    { offsetSec: 47.80 + OPENING_PREFIX_SEC, pitch: 0.65, gain: 0.32 },
  ];
  for (const m of meows) {
    mixZooSample(
      { startSec: m.offsetSec, name: "cat", gain: m.gain, pitchRatio: m.pitch },
      sfxDryBuf,
      SAMPLE_RATE
    );
    events.sfx.push({ t: m.offsetSec, name: "cat", dur: 0.35 });
  }
  // Bird tweets interspersed with the kittens — same window, varied
  // pitches so it reads as a chorus of small creatures, not echoes.
  const tweets = [
    { offsetSec: 43.50 + OPENING_PREFIX_SEC, pitch: 1.10, gain: 0.35 },
    { offsetSec: 44.80 + OPENING_PREFIX_SEC, pitch: 1.35, gain: 0.30 },
    { offsetSec: 46.10 + OPENING_PREFIX_SEC, pitch: 0.92, gain: 0.32 },
    { offsetSec: 47.30 + OPENING_PREFIX_SEC, pitch: 1.50, gain: 0.28 },
  ];
  for (const tw of tweets) {
    mixZooSample(
      { startSec: tw.offsetSec, name: "bird", gain: tw.gain, pitchRatio: tw.pitch },
      sfxDryBuf,
      SAMPLE_RATE
    );
    events.sfx.push({ t: tw.offsetSec, name: "bird", dur: 0.30 });
  }
}

// Chill cat CHOIR — a HARMONIZED stack of long, time-stretched,
// autotuned meow drones (minor-triad consonant ratios, in tune with
// itself) + a few short high kitten accents. Enters LATE (back half,
// bars 70–110) — it was coming in way too early before.
if (isChill && barStartRel.length > 110) {
  const tA = OPENING_PREFIX_SEC + barStartRel[70];
  const tB = OPENING_PREFIX_SEC + barStartRel[110];
  const span = tB - tA;
  const r = 0.42 * 0.25; // TWO OCTAVES down — was harsh/sharp/shrill (jas)
  const chord = [
    { ratio: r,                          gain: 0.055 }, // root
    { ratio: r * Math.pow(2, 3 / 12),    gain: 0.044 }, // minor 3rd
    { ratio: r * Math.pow(2, 7 / 12),    gain: 0.040 }, // 5th
    { ratio: r * 2,                      gain: 0.014 }, // octave shimmer (way down)
  ];
  chord.forEach((c, vi) => {
    const off = vi * (span * 0.04);                 // staggered bloom-in
    mixCatDrone(sfxDryBuf, tA + off, span - off, c.ratio, c.gain);
    events.sfx.push({ t: tA + off, name: "cat", dur: span - off });
  });
  // a few short high kitten accents so the choir still reads "cat"
  for (let k = 0; k < 4; k++) {
    const t = tA + span * ((k + 0.5) / 4) + (noiseRng() - 0.5) * (span * 0.15);
    mixZooSample({ startSec: t, name: "cat", gain: 0.07, pitchRatio: (1.9 + noiseRng() * 0.8) * 0.25 }, sfxDryBuf, SAMPLE_RATE); // 2 octaves down, quieter — was harsh sharp cuts (jas)
    events.sfx.push({ t, name: "cat", dur: 0.35 });
  }
}

// Boot sequence — system chime → voice → bang → music:
//   0.05 s   ac-native boot melody (triangle C5→E5→G5)
//   0.75 s   greeting "good evening jeffrey. enjoy los angeles."
//   3.72 s   greeting ends
//   2.625 s  sniper BANG — snapped to 16th-note hihat tick 24 (= 2
//             bars at 137.143 bpm), lands during "Angeles" tail just
//             before music entry. Bumps earlier than the 8th-tick
//             position so the bang punches more inside the voice.
//   2.700 s  music + vocal enter
// Boot chime always plays — sets the AC voice. Sniper bang skipped
// in chill mode (no dramatic openings for study vibes).
// ── TYPING INTRO — someone types "trancenwaltz" into the AC prompt
//    with quick keystroke clicks, THEN the boot melody fires. The
//    visualizer (cover-video.mjs) draws the pink AC prompt block +
//    types the title char-by-char synced to these keyclick events.
//    12 clicks = the 12 letters of "trancenwaltz". Fast (~70 ms
//    apart). All inside the 2.7 s opening prefix — music entry and
//    the sniper (2.625 s) are unaffected.
const TYPE_N = 12;            // "trancenwaltz"
const TYPE_START = 0.18;
const TYPE_GAP = 0.072;       // ~14 keys/sec — quick typing
// --master (release WAV): DROP the typing keyclicks + enter thunk
// entirely — a clean streaming single. The boot / startup melody
// (fired below, ungated) is KEPT.
if (!isChill && !RELEASE_MASTER) {
  const kRng = makeRng(SEED_STR + ":keyclick");
  for (let i = 0; i < TYPE_N; i++) {
    const kt = TYPE_START + i * TYPE_GAP;
    // short bright tick, slight per-key pitch jitter so it reads as
    // mechanical-keyboard typing rather than one repeated blip.
    // LOWER-PITCHED tick — chunkier mechanical-keyboard thock.
    const tone = 620 + (kRng() - 0.5) * 240;
    const ks = makeBufferSynth(sfxDryBuf, kt, SAMPLE_RATE, noiseRng);
    ks.synth({ type: "square", tone, duration: 0.016, volume: 0.17, attack: 0.0009, decay: 0.014 });
    const ks2 = makeBufferSynth(sfxDryBuf, kt, SAMPLE_RATE, noiseRng);
    ks2.synth({ type: "triangle", tone: tone * 1.9, duration: 0.011, volume: 0.09, attack: 0.0006, decay: 0.010 });
    events.sfx.push({ t: kt, name: `keyclick-${i}`, dur: 0.05, point: true });
  }
  // ENTER — a chunkier, lower return-key thunk after the last letter,
  // a beat before the boot melody. Marks "command submitted".
  const enterT = TYPE_START + TYPE_N * TYPE_GAP + 0.07;
  const en1 = makeBufferSynth(sfxDryBuf, enterT, SAMPLE_RATE, noiseRng);
  en1.synth({ type: "square", tone: 300, duration: 0.045, volume: 0.22, attack: 0.001, decay: 0.04 });
  const en2 = makeBufferSynth(sfxDryBuf, enterT, SAMPLE_RATE, noiseRng);
  en2.synth({ type: "triangle", tone: 150, duration: 0.06, volume: 0.16, attack: 0.001, decay: 0.055 });
  events.sfx.push({ t: enterT, name: "prompt-enter", dur: 0.08, point: true });
}
// Boot melody fires AFTER the typing finishes (was 0.05 s — now
// gated behind the ~0.95 s typing burst). Still well before the
// sniper (2.625 s) and music entry (2.7 s).
// BOOT_SEC stays defined in chill — the non-chill pre-roll hats
// anchor on it — but the audible startup melody is skipped below.
const BOOT_SEC = isChill ? 0.05 : (TYPE_START + TYPE_N * TYPE_GAP + 0.13);
// Startup beeps — skipped in chill mode. The study mix has no
// system-boot narrative; it just begins on the music.
if (!isChill) {
  fireBootMelody(sfxDryBuf, BOOT_SEC, 1.0);
  // Boot chime — tagged as a point marker so the visualizer renders
  // it decorrelated from the boot-beeps waveform behind it. Three
  // chime tones C5→E5→G5 at ~0.4s each.
  events.sfx.push({ t: BOOT_SEC, name: "boot-chime", dur: 0.4, point: true });
}
if (isChill) {
  // INTRO = "belwooom": ONE unidirectional, slower ~6 s boom (a slow
  // descending zipper-roll) that BLOOMS — a harmonized melodic bloom
  // rises from its belly. No intake suck (clean loop end wraps to this).
  const boomDur = 6.0;
  fireVortex(sfxDryBuf, 0.05, boomDur, "boom");
  events.sfx.push({ t: 0.05, name: "vortex-boom", dur: boomDur });
  // BLOOM — fires ONCE ~8 s in, ON a sampled bell from the field
  // recording, then just decays away (fire-and-die, never returns up
  // in pitch). Soft + blended — sits under, doesn't poke out.
  const bloomT = 8.0;
  if (ambientBuf) fireAmbGrain(sfxDryBuf, bloomT, 6.0, 1.8, 1.0, 0.16); // the sampled bell it lands on
  const bloomDeg = [0, 2, 4, 0]; // tonic triad + octave
  for (let bi = 0; bi < bloomDeg.length; bi++) {
    const m0 = scaleNoteMidi(bloomDeg[bi], bi === 3 ? -2 : -3); // deep
    const f0 = 440 * Math.pow(2, (m0 - 69) / 12);
    const dur = 5.0;
    const v = makeBufferSynth(sfxDryBuf, bloomT + bi * 0.03, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sine",     tone: f0,        duration: dur,       volume: 0.24, attack: 0.05, decay: dur * 0.92 });
    v.synth({ type: "triangle", tone: f0 * 2.01, duration: dur * 0.7, volume: 0.06, attack: 0.05, decay: dur * 0.6 });
  }
}
const sixteenthSec_BOOT = (60 / BPM) / 4;
const sniperSec = 24 * sixteenthSec_BOOT;
if (!isChill) {
  fireSniper(sfxDryBuf, sniperSec, 2.4);
  events.sfx.push({ t: sniperSec, name: "sniper", dur: 0.12 });
}

// ── shutdown sequence — mirrors the boot, NOT tape-stopped ──────────
// Routes to shutdownBuf so the bye voice + chime + ending hats stay
// at full pitch while the music underneath spins down. Mirrors the
// opening: voice + chime + hats at the same wall-clock anchor on
// both ends of the track. Skipped in chill mode — no shutdown story,
// the chill outro is a natural fade.
if (!isChill) {
  // bye lands AFTER the tape-stop slowdown has already started (at
  // totalSec - 5.0), so the voice rides on top of the music's
  // already-slowing collapse — feels more like a system saying goodbye
  // as it dies rather than announcing the shutdown.
  const BYE_SEC             = totalSec - 3.4;
  const SHUTDOWN_MELODY_SEC = totalSec - 1.5;
  fireShutdownMelody(shutdownBuf, SHUTDOWN_MELODY_SEC, 0.65);
  events.sfx.push({ t: SHUTDOWN_MELODY_SEC, name: "shutdown-chime", dur: 0.4, point: true });
  events.vox.push({ t: BYE_SEC, name: "bye-jeffrey", dur: 1.6 });
  const BYE_PATH = `${REPO}/pop/dance/out/.ac-bye.mp3`;
  if (existsSync(BYE_PATH)) {
    const tmpBye = `${dirname(OUT_PATH)}/.ac-bye.f32.raw`;
    const dec = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", BYE_PATH,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpBye,
    ]);
    if (dec.status === 0 && existsSync(tmpBye)) {
      const raw = readFileSync(tmpBye);
      const bye = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
      let bpeak = 0;
      for (let i = 0; i < bye.length; i++) { const a = Math.abs(bye[i]); if (a > bpeak) bpeak = a; }
      const norm = bpeak > 0 ? 0.85 / bpeak : 1.0;
      const startIdx = Math.floor(BYE_SEC * SAMPLE_RATE);
      const fadeS = Math.floor(0.05 * SAMPLE_RATE);
      for (let i = 0; i < bye.length; i++) {
        const j = startIdx + i;
        if (j < 0 || j >= shutdownBuf.length) break;
        let fade = 1;
        if (i < fadeS) fade = i / fadeS;
        else if (i > bye.length - fadeS) fade = Math.max(0, (bye.length - i) / fadeS);
        shutdownBuf[j] += bye[i] * norm * fade;
      }
      try { unlinkSync(tmpBye); } catch {}
      console.log(`  shutdown · "bye jeffrey" at ${BYE_SEC.toFixed(2)}s + chime at ${SHUTDOWN_MELODY_SEC.toFixed(2)}s`);
    }
  }
}

// ── post-roll hihats — "system shutting down" ───────────────────────
// DECELERATE from 8th-note rate to a half-note crawl across the tail,
// with progressive timing jitter and amplitude quantization (mini
// bitcrush per hit) so the hats turn into "smushy bitcrunches" as
// the track dies. Skipped in chill mode — chill outro is just a
// natural decay, no system-shutdown narrative.
if (!isChill) {
  const eighthSec     = beatSec / 2;
  const halfNoteSec   = beatSec * 2;       // very slow tail rate
  const tailHatStart  = totalSec - 5.0;
  const tailHatEnd    = totalSec - 2.0; // earlier cutoff — no hat near the final beep
  let t = tailHatStart;
  while (t < tailHatEnd) {
    const f = (t - tailHatStart) / Math.max(0.001, tailHatEnd - tailHatStart);
    // Timing jitter grows quadratically across the tail.
    const jitter = (noiseRng() - 0.5) * 0.12 * f * f;
    // Amplitude bitcrush — quantize the hat's volume to fewer levels
    // as the track dies. f=0: 16 levels (clean). f=1: 2 levels (very
    // crushed).
    const levels = Math.max(2, Math.round(16 * (1 - f * 0.85)));
    const rawV = 0.32 + noiseRng() * 0.10;
    const crushedV = Math.round(rawV * levels) / levels;
    // Volume also tapers toward silence so the hats fade out.
    const ampEnv = 1 - f * 0.55;
    // randomly pitched around a bit per tick — the dying tail hats
    // wobble in pitch as they smush out.
    const hatPF = 1 + (noiseRng() - 0.5) * 0.36; // ~±2.8 semitones
    fireDrum(shutdownBuf, t + jitter, "g", { volume: crushedV * ampEnv * DRUM_GAIN, pitchFactor: hatPF });
    hatCount++;
    // Step DECELERATES from 8th → half-note across the tail (the
    // opposite of "taking off" — the system is winding down).
    const step = eighthSec + (halfNoteSec - eighthSec) * f * f;
    t += step;
  }
}

// ── pre-roll hihats — beeps that morph into noisy hats ──────────────
// Each tick fires TWO layers crossfading from beep → hat across an
// EXTENDED window that runs PAST the prefix into the first ~2 bars
// of music — beeps stay tonal longer before yielding to the hat
// spine. Quadratic beep weight (1-f)^2 keeps tonality dominant for
// roughly the first 70 % of the crossfade.
//
// Rate decelerates from 16th → 8th (the "coming in for a landing"
// curve).
//
// SKIPPED IN CHILL MODE — this is the startup "beep bop"
// sonification, and its hat layer overlaps the intro's own in-bar
// hats (which begin at music entry), producing an audible DOUBLED
// hi-hat at the top of the track. The chill study mix opens clean
// on the bed instead.
const BEEP_BLEND_END_SEC = OPENING_PREFIX_SEC + 2.3; // ≈ 5.0 s
if (!isChill) {
  const sixteenthSec = beatSec / 4;
  const eighthSec    = beatSec / 2;
  // Starts with the (moved) boot melody so the typing window stays
  // clean — just keyclicks — before the "beep bop" sonification.
  const firstHatSec = BOOT_SEC;
  const endHatSec = BEEP_BLEND_END_SEC;
  const winSec = endHatSec - firstHatSec;
  // Span entry for the whole boot-beep window — gives the SFX lane
  // a single waveform region covering the opening "beep bop" so the
  // timeline shows the startup sonification.
  events.sfx.push({ t: firstHatSec, name: "boot-beeps", dur: winSec });
  let t = firstHatSec;
  while (t < endHatSec) {
    const f = (t - firstHatSec) / Math.max(0.001, winSec); // 0 → 1
    // Quadratic beep curve — tonality persists longer before fading.
    const beepWeight = (1.0 - f) * (1.0 - f);
    const hatWeight  = f;
    // LOW tone climbs from 110 Hz → 220 Hz across the prefix. Kept
    // well below the boot melody's C5 (523 Hz) start so the system
    // beeps and the pre-roll beeps occupy different frequency bands
    // and don't compete. Dropped the harmonic partial that was
    // muddying the boot melody's low-mids.
    const beepTone = 110 + 110 * f;
    if (beepWeight > 0.01) {
      const beep = makeBufferSynth(dryBuf, t, SAMPLE_RATE, noiseRng);
      beep.synth({ type: "sine", tone: beepTone, duration: 0.040, volume: 0.20 * beepWeight, attack: 0.001, decay: 0.038 });
    }
    if (hatWeight > 0.01) {
      const v = 0.16 + noiseRng() * 0.06;
      // randomly pitched around a bit per tick so the fast intro hats
      // shimmer instead of being one repeated sample.
      const hatPF = 1 + (noiseRng() - 0.5) * 0.36; // ~±2.8 semitones
      fireDrum(dryBuf, t, "g", { volume: v * hatWeight * DRUM_GAIN, pitchFactor: hatPF });
      // Capture EACH pre-roll hat tick so the timeline shows the
      // beep→hat blend layer in the HAT lane during the intro,
      // not just the in-bar hats that start at bar 0.
      events.hat.push({ t });
      hatCount++;
    }
    // Step decelerates from 16th → 8th.
    const step = sixteenthSec + (eighthSec - sixteenthSec) * f;
    t += step;
  }
}

// ── ac-native boot greeting ("hi @jeffrey. welcome to los angeles.")
// Fires right after the opening sniper. Cached at:
//   pop/dance/out/.ac-greeting.mp3
// Generated by bin/say.mjs (jeffrey-pvc neutral voice, calm settings).
const GREETING_PATH = `${REPO}/pop/dance/out/.ac-greeting.mp3`;
if (!isChill && existsSync(GREETING_PATH)) {
  events.vox.push({ t: 0.05, name: "greeting", dur: 3.7 });
  const tmpGreetRaw = `${dirname(OUT_PATH)}/.ac-greeting.f32.raw`;
  const dec = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", GREETING_PATH,
    "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
    tmpGreetRaw,
  ]);
  if (dec.status === 0 && existsSync(tmpGreetRaw)) {
    const raw = readFileSync(tmpGreetRaw);
    const greet = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
    let gpeak = 0;
    for (let i = 0; i < greet.length; i++) { const a = Math.abs(greet[i]); if (a > gpeak) gpeak = a; }
    const norm = gpeak > 0 ? 0.85 / gpeak : 1.0;
    // Greeting + boot melody both start at the very top so the chime
    // and voice ring together — same wall-clock entry as an actual
    // ac-native boot where the C5→E5→G5 ascending chime plays
    // simultaneously with the time-of-day greeting.
    const greetStartSec = 0.05;
    const greetStartIdx = Math.floor(greetStartSec * SAMPLE_RATE);
    const fadeS = Math.floor(0.06 * SAMPLE_RATE);
    for (let i = 0; i < greet.length; i++) {
      const j = greetStartIdx + i;
      if (j < 0 || j >= sfxDryBuf.length) break;
      let fade = 1;
      if (i < fadeS) fade = i / fadeS;
      else if (i > greet.length - fadeS) fade = Math.max(0, (greet.length - i) / fadeS);
      sfxDryBuf[j] += greet[i] * norm * fade;
    }
    try { unlinkSync(tmpGreetRaw); } catch {}
    console.log(`  greeting · "good evening jeffrey. enjoy los angeles." (flite slt) at ${greetStartSec}s`);
  }
}

// (Bird tweets removed — the ending should kill cleanly with only the
// boot-melody shutdown chime as the final sound; no jungle texture
// bleeding into the cut.)

// (Pre-roll square/triangle arpeggio removed — was distracting in
// the boot intro. The per-bar arp now handles the arpeggio entirely
// from music bar 0 onward.)

// Low harp — sustained warm harmonic voice across the first 4 measures
// of the intro. Fills the rhythmic gap before kick/bells/lead enter
// without competing with the vocal arc. Each bar gets a new low note
// from the bar's chord; the previous note's tail bleeds into the
// next so the harp feels continuous.
let harpCount = 0;
{
  const harpBars = Math.min(4, TOTAL_BARS);
  for (let bar = 0; bar < harpBars; bar++) {
    const chordDeg = progressionAt(bar);
    // Low octave (-1) root, plus the chord 5th one octave below the
    // root for warmth. Both are below middle C so they sit under the
    // vocal without masking.
    const rootMidi  = scaleNoteMidi(chordDeg,     -1);
    const fifthMidi = scaleNoteMidi(chordDeg + 4, -2);
    mixEventHarp(
      { startSec: bar * barSec + OPENING_PREFIX_SEC, midi: rootMidi,  gain: 0.55, durSec: barSec * 1.4 },
      dryBuf,
      SAMPLE_RATE
    );
    mixEventHarp(
      { startSec: bar * barSec + OPENING_PREFIX_SEC, midi: fifthMidi, gain: 0.40, durSec: barSec * 1.4 },
      dryBuf,
      SAMPLE_RATE
    );
    harpCount += 2;
  }
}

console.log(`→ events · kick=${kickCount} hat=${hatCount} sub=${subCount} lead=${leadCount} pad=${padCount} saw=${supersawCount} piano=${pianoCount} bells=${bellsCount} harp=${harpCount} riser=${riserCount} snareRoll=${snareRollCount} mg=${machineGunCount} dropImpact=${dropImpactCount}`);

// ── supersaw flange ───────────────────────────────────────────────────
// Slower flange — the previous 6 Hz rate warbled too fast for the
// trance tempo. 2.5 Hz now gives a more languid jet-sweep that
// breathes with the music instead of fluttering through it.
applyFlange(sawBuf, {
  rate: 2.5,
  depthMs: 4.5,
  baseDelayMs: 5,
  feedback: 0.45,
  mix: 0.35, // pulled from 0.55 — wet was eating the dry saw fundamentals, making the wall hard to hear
  sampleRate: SAMPLE_RATE,
});
for (let i = 0; i < totalSamples; i++) duckBuf[i] += sawBuf[i];

// ── chill bell flanger ────────────────────────────────────────────────
// Chill mode bells live on chillBellBuf — slow LFO flanger here gives
// the low-tone drawn-out wash a hypnotic moving timbre.
if (isChill) {
  applyFlange(chillBellBuf, {
    rate: 0.18,        // very slow — a 5.5 s LFO period
    depthMs: 8.0,      // wide depth for noticeable flange motion
    baseDelayMs: 6,
    feedback: 0.55,
    mix: 0.65,
    sampleRate: SAMPLE_RATE,
  });
  // Mix into duckBuf so the chill bells share the sidechain pumping
  // with the pad — keeps them tucked underneath the kick.
  for (let i = 0; i < totalSamples; i++) duckBuf[i] += chillBellBuf[i];
}

// ── sidechain ─────────────────────────────────────────────────────────
if (SIDECHAIN && kickTimes.length > 0) {
  const recoverS = (DUCK_MS / 1000) * SAMPLE_RATE;
  const gain = new Float32Array(totalSamples);
  for (let i = 0; i < totalSamples; i++) gain[i] = 1;
  const attackS = Math.max(1, Math.floor(0.003 * SAMPLE_RATE));
  for (const kt of kickTimes) {
    const idx = Math.floor(kt * SAMPLE_RATE);
    for (let i = 0; i < attackS; i++) {
      const j = idx + i;
      if (j < 0 || j >= totalSamples) continue;
      const tt = i / attackS;
      const target = 1 - DUCK_DEPTH;
      gain[j] = gain[j] + (target - gain[j]) * tt;
    }
    for (let i = 0; i < recoverS; i++) {
      const j = idx + attackS + i;
      if (j < 0 || j >= totalSamples) continue;
      const tt = i / recoverS;
      const floor = 1 - DUCK_DEPTH;
      const ducked = floor + (1 - floor) * (1 - Math.exp(-4 * tt));
      if (ducked < gain[j]) continue;
      gain[j] = ducked;
    }
  }
  for (let i = 0; i < totalSamples; i++) duckBuf[i] *= gain[i];
}

// ── deep bass wobble (bass bus only) ─────────────────────────────────
// Slow LFO modulating a low-pass cutoff on the sub bass. Gives a
// dubstep-style "wub wub" without affecting the pad above it.
applyWobble(bassBuf, {
  target: "filter",
  baseCutoffHz: 480,
  rate:  [{ time: 0, rate: 1.2 }, { time: totalSec / 2, rate: 2.6 }, { time: totalSec, rate: 1.8 }],
  depth: [{ time: 0, depth: 0.65 }, { time: totalSec / 2, depth: 0.88 }, { time: totalSec, depth: 0.75 }],
  sampleRate: SAMPLE_RATE,
  startSec: 0,
  endSec: totalSec,
});

// ── per-section dynamic envelope ──────────────────────────────────────
// Apply a track-shape multiplier so the bed isn't slammed-to-1 all the
// way through. Intro fades up, breaks dip slightly, builds ramp into
// drops, drops at full, outro fades out. Gives a real build-up arc.
// More dramatic dynamic arc — quieter intro, big climb, drops can
// overshoot. Wider range than before so the track breathes.
const SECTION_LEVELS_NORMAL = {
  intro:  [0.12, 0.38],
  break1: [0.38, 0.58],
  build1: [0.58, 1.20],
  drop1:  [1.20, 1.15],
  break2: [0.30, 0.42],
  build2: [0.42, 1.35],
  drop2:  [1.35, 1.20],
  outro:  [1.20, 0.75],
};
// CHILL — FLAT dynamic arc. Range 0.55-0.95 instead of 0.12-1.35 so
// the track stays in study-vibes territory throughout. No big drops,
// no deep breaks. Sections still have gentle contour for interest.
const SECTION_LEVELS_CHILL = {
  intro:  [0.55, 0.70],
  break1: [0.70, 0.78],
  build1: [0.78, 0.88],
  drop1:  [0.88, 0.92],
  break2: [0.70, 0.78],
  build2: [0.78, 0.88],
  drop2:  [0.88, 0.95],
  outro:  [0.85, 0.55],
};
const SECTION_LEVELS = isChill ? SECTION_LEVELS_CHILL : SECTION_LEVELS_NORMAL;
const dynEnv = new Float32Array(totalSamples);
for (let i = 0; i < totalSamples; i++) dynEnv[i] = 1;
for (const r of sectionRanges) {
  const [g0, g1] = SECTION_LEVELS[r.name] || [1, 1];
  const startIdx = Math.floor(r.startSec * SAMPLE_RATE);
  const endIdx = Math.floor(r.endSec * SAMPLE_RATE);
  // Build sections use an exponential climb (slow start, fast end)
  // so the crescendo psychoacoustically maps to the perception of a
  // build — drama LATE, not a flat ramp the whole way. Other
  // sections use linear interpolation as before.
  const isBuild = r.name.startsWith("build");
  for (let i = startIdx; i < endIdx && i < totalSamples; i++) {
    const t = (i - startIdx) / Math.max(1, endIdx - startIdx);
    const shapedT = isBuild ? Math.pow(t, 2.4) : t; // ease-in for builds
    dynEnv[i] = g0 + (g1 - g0) * shapedT;
  }
}

// ── unified tape-stop ending — ONE clock dialing down ───────────────
// The whole mix decelerates together as if a single master clock is
// being turned down. Same pitch-curve applied to every instrument
// bus so the slowdown reads as cohesive (a tape spinning down).
// Skipped in chill mode — no system-shutdown narrative, the outro
// just decays naturally.
//
// EVENT REMAPPING — captured events in the tail window are pushed
// to a NEW displayed time so the visualizer scatters/decelerates
// them in sync with the audio. The pitch curve is END_PITCH^f =
// 0.20^f over the 5 s tail. An event at source-tail-offset s
// (0..2.487 s of source) is heard at output-tail-offset:
//   f = ln(1 + s · ln(END_PITCH) / TAIL) / ln(END_PITCH)
// (clamped to 1.0 — anything past max coverage is never heard).
if (!isChill) {
  const TAIL_BUFSEC_REMAP = 5.0;
  const END_PITCH_REMAP = 0.20;
  const tailStart = totalSec - TAIL_BUFSEC_REMAP;
  const lnEnd = Math.log(END_PITCH_REMAP);
  const maxSrcCoverage = TAIL_BUFSEC_REMAP * (END_PITCH_REMAP - 1) / lnEnd;
  function remapInTail(evt) {
    if (evt.t < tailStart) return;
    const s = evt.t - tailStart;
    if (s > maxSrcCoverage) {
      evt.skipped = true;
      return;
    }
    const f = Math.log(1 + (s * lnEnd) / TAIL_BUFSEC_REMAP) / lnEnd;
    evt.displayedT = tailStart + TAIL_BUFSEC_REMAP * Math.min(1, Math.max(0, f));
  }
  for (const k of Object.keys(events)) {
    for (const evt of events[k]) remapInTail(evt);
  }
  // Add a tape-stop waveform event to the SFX lane so the visualizer
  // shows the slowdown audio as a 5 s waveform region.
  events.sfx.push({ t: tailStart, name: "tape-stop", dur: TAIL_BUFSEC_REMAP });
}
if (!isChill) {
  const TAIL_BUFSEC = 5.0;
  const tailStartIdx = Math.floor((totalSec - TAIL_BUFSEC) * SAMPLE_RATE);
  const tailEndIdx = Math.floor(totalSec * SAMPLE_RATE);
  // ONE master pitch curve — exponential from 1.0× → 0.20× over the
  // 5-second tail. Every bus gets the same curve so they all dial
  // down together. The amplitude fade is also shared so the whole
  // mix collapses to silence as a single entity.
  const END_PITCH = 0.20;
  function tapeStopBus(buf) {
    if (!buf || tailEndIdx <= tailStartIdx) return;
    const len = tailEndIdx - tailStartIdx;
    const src = buf.slice(tailStartIdx, tailEndIdx);
    let srcPos = 0;
    for (let i = 0; i < len; i++) {
      const f = i / len;
      const pitch = Math.pow(END_PITCH, f); // shared 1.0 → 0.20 curve
      srcPos += pitch;
      const s0 = Math.floor(srcPos);
      const s1 = s0 + 1;
      let sample = 0;
      if (s1 < src.length) {
        const frac = srcPos - s0;
        sample = src[s0] * (1 - frac) + src[s1] * frac;
      }
      // Shared amplitude fade — kicks in after the 60% mark so the
      // music has weight up until the very last second.
      const amp = f < 0.6 ? 1 : (1 - f) / 0.4;
      buf[tailStartIdx + i] = sample * amp;
    }
  }
  tapeStopBus(bassBuf);
  tapeStopBus(duckBuf);
  tapeStopBus(dryBuf);
  tapeStopBus(mgBuf);
  tapeStopBus(sfxDryBuf);
  console.log(`  tape-stop · unified one-clock slowdown last ${TAIL_BUFSEC}s (1.0× → ${END_PITCH}×)`);
}

if (GALLOP && isChill) {
  // REAL horse GALLOP — CC0 archive.org recording (Red Library), not
  // synthesised (jas: "i wanted the horse gallop to be real?"). Spread
  // around (random window each stride — NOT loop-loop-loop), pitched
  // DEEPER + mildly MELODIC to chord-scale tones, with echo taps. Plus
  // a HARMONIZED real neigh (3 stacked pitches) and a synth thunder.
  // Opens loud (a "preview of whats comin"), then a lighter bed.
  const gRng = makeRng(SEED_STR + ":gallop");
  const gal = loadSfxF32(".gallop.wav");
  const nei = loadSfxF32(".neigh.wav");
  const SRg = SAMPLE_RATE, galEnd = musicSec - 5;
  const place = (src, t0, ratio, gain, echo) => {
    if (!src) return;
    const taps = echo ? [[0, gain], [0.14, gain * 0.42], [0.31, gain * 0.19], [0.55, gain * 0.09]]
                      : [[0, gain]];
    const off = Math.floor(gRng() * Math.max(1, src.length - SRg * 1.5)); // spread, not a loop
    const len = Math.floor((0.55 + gRng() * 0.8) * SRg);
    const fadeN = Math.floor(0.014 * SRg);
    // VARIABLE playback rate (jas: "variable playback rate on the
    // horze ... bend a bit with the rhythm ... morph under into it"):
    // the read rate warbles with the beat + slowly drifts DOWN across
    // the slice, so the real samples bend musically instead of playing
    // flat. (beat from --bpm; steady in trancepenta.)
    const galBeatHz = (Number(flags.bpm) || 120) / 60;
    for (const [dly, tg] of taps) {
      const baseI = Math.floor((t0 + dly) * SRg);
      let sp = off;
      for (let k = 0; k < len; k++) {
        const di = baseI + k;
        if (di < 0) { sp += ratio; continue; }
        if (di >= dryBuf.length) break;
        const si = Math.floor(sp);
        if (si + 1 >= src.length) break;
        const fr = sp - si;
        let e = 1;
        if (k < fadeN) e = k / fadeN;
        else if (k > len - fadeN) e = Math.max(0, (len - k) / fadeN);
        dryBuf[di] += (src[si] * (1 - fr) + src[si + 1] * fr) * e * tg;
        const tt = (t0 + dly) + k / SRg;
        sp += ratio * (1 + 0.05 * Math.sin(2 * Math.PI * galBeatHz * tt)) // bend w/ rhythm
                    * (1 - 0.10 * (k / len));                              // morph under
      }
    }
  };
  let ts = OPENING_PREFIX_SEC + 0.05, strideCount = 0;
  const barApprox = musicSec / Math.max(1, TOTAL_BARS);
  const melo = [0, 3, 5, 7, 3];                       // small deep melodic set (semitones)
  while (ts < galEnd) {
    const mt = ts - OPENING_PREFIX_SEC;
    const lvl = (mt < 24 ? 1.0 : 0.42) * Math.min(1, (galEnd - ts) / 4);
    // deeper-set base ratio 0.5 (down an octave) + a mild melodic step
    const ratio = 0.5 * Math.pow(2, melo[strideCount % melo.length] / 12);
    place(gal, ts, ratio, 0.55 * DRUM_GAIN * lvl * (0.85 + gRng() * 0.3), true);
    events.sfx.push({ t: ts, name: "gallop", dur: 0.55 });
    ts += (0.55 + gRng() * 0.7) / (mt < 24 ? 1.3 : 1.0); // irregular = spread
    strideCount++;
  }
  // HARMONIZED real neigh — 3 stacked chord pitches, echoed, at the
  // open + mid (jas: "a horse neighhh in hermony!").
  if (nei) {
    for (const nt of [OPENING_PREFIX_SEC + 1.8, OPENING_PREFIX_SEC + musicSec * 0.5]) {
      const cd = progressionAt(Math.floor(((nt - OPENING_PREFIX_SEC) / barApprox)) % TOTAL_BARS);
      for (const [hd, hg] of [[0, 0.5], [2, 0.34], [4, 0.26]]) {
        const r = Math.pow(2, (scaleNoteMidi(cd + hd, -1) - 57) / 12);
        place(nei, nt, Math.max(0.4, r), hg * DRUM_GAIN * 0.6, true);
      }
      events.sfx.push({ t: nt, name: "neigh", dur: 1.2 });
    }
  }
  // THUNDERCLAP / lightning — synth crack + deep rumble + tail (no
  // clean CC0 clap was found). Open + one mid hit.
  const thunder = (t0, g) => {
    const bi = Math.floor(t0 * SRg), nN = Math.floor(2.6 * SRg);
    let lp = 0;
    for (let k = 0; k < nN; k++) {
      const di = bi + k;
      if (di < 0) continue;
      if (di >= dryBuf.length) break;
      const lt = k / SRg;
      const crack = lt < 0.10 ? (gRng() * 2 - 1) * Math.exp(-lt / 0.022) : 0;
      const n = gRng() * 2 - 1;
      lp += 0.0010 * (n - lp);
      const rum = lp * Math.exp(-lt / 0.95) * (0.7 + 0.3 * Math.sin(2 * Math.PI * 6 * lt));
      dryBuf[di] += (crack * 0.55 + rum * 3.4) * g;
    }
  };
  thunder(OPENING_PREFIX_SEC + 0.12, 0.5 * DRUM_GAIN);
  thunder(OPENING_PREFIX_SEC + musicSec * 0.5, 0.4 * DRUM_GAIN);
  events.sfx.push({ t: OPENING_PREFIX_SEC + 0.12, name: "thunder", dur: 2.6 });
  events.sfx.push({ t: OPENING_PREFIX_SEC + musicSec * 0.5, name: "thunder", dur: 2.6 });
  // STEAM / TRAIN WHISTLE (jas) — a detuned 3-note chord cluster with
  // breathy steam, slow swell in, mournful pitch fall + vibrato, long
  // tail. Opens like a locomotive; one more mid.
  const whistle = (t0, dur, g) => {
    const bi = Math.floor(t0 * SRg), nN = Math.floor(dur * SRg);
    const f = [392, 466.2, 587.3];
    const ph = [0, 0, 0];
    for (let k = 0; k < nN; k++) {
      const di = bi + k;
      if (di < 0) continue;
      if (di >= dryBuf.length) break;
      const lt = k / SRg, u = lt / dur;
      const swell = Math.min(1, lt / 0.20) * Math.min(1, Math.max(0, (dur - lt) / 0.6));
      const drift = 1 - 0.03 * u;
      const vib = 1 + 0.004 * Math.sin(2 * Math.PI * 5.5 * lt);
      let s = 0;
      for (let v = 0; v < 3; v++) {
        ph[v] += (2 * Math.PI * f[v] * drift * vib) / SRg;
        s += Math.sin(ph[v]) * (v === 0 ? 0.5 : 0.32);
      }
      const air = (gRng() * 2 - 1) * 0.10;
      dryBuf[di] += (s + air) * swell * g;
    }
  };
  whistle(OPENING_PREFIX_SEC + 0.05, 1.7, 0.32 * DRUM_GAIN);
  whistle(OPENING_PREFIX_SEC + musicSec * 0.5 - 0.45, 1.4, 0.26 * DRUM_GAIN);
  events.sfx.push({ t: OPENING_PREFIX_SEC + 0.05, name: "train-whistle", dur: 1.7 });
  events.sfx.push({ t: OPENING_PREFIX_SEC + musicSec * 0.5 - 0.45, name: "train-whistle", dur: 1.4 });
  // OCEAN / HARBOUR scene in the last third (jas: "wave crashing
  // sounds ... a boat horn ... fog horn in distance"). Real CC0
  // Freesound samples, placed via the same deep/echo/variable-rate
  // helper so they bend with the rhythm too.
  const waves = loadSfxF32(".waves.wav");
  const fogh = loadSfxF32(".foghorn-low.wav"); // pre-stretched ~5s, -12st, slow attack + smooth decay (jas)
  const boath = loadSfxF32(".boathorn.wav");
  // CLEAN placement (no beat-warble) for the sustained low fog horn —
  // it must read as one long smooth ~5s tone, not a bent stutter.
  const placeClean = (src, t0, gain) => {
    if (!src) return;
    const taps = [[0, gain], [0.22, gain * 0.34], [0.5, gain * 0.16]];
    for (const [dly, tg] of taps) {
      const bi = Math.floor((t0 + dly) * SRg);
      for (let k = 0; k < src.length; k++) {
        const di = bi + k;
        if (di < 0) continue;
        if (di >= dryBuf.length) break;
        dryBuf[di] += src[k] * tg;
      }
    }
  };
  const oc0 = OPENING_PREFIX_SEC + musicSec * 0.66;
  const ocEnd = OPENING_PREFIX_SEC + musicSec * 0.86;
  if (waves) {                                   // spread crashing waves bed
    let wt = oc0;
    while (wt < ocEnd) {
      place(waves, wt, 0.85 + gRng() * 0.2, 0.20 * DRUM_GAIN * (0.8 + gRng() * 0.3), true);
      events.sfx.push({ t: wt, name: "wave", dur: 1.2 });
      wt += 1.4 + gRng() * 1.6;
    }
  }
  // Foghorn removed per @jeffrey — the boathorns + train-whistle carry
  // the harbour mood without the long sustained low-frequency bed tone.
  // (fogh loaded but intentionally unused; left in place so the asset
  // path stays warm if we want to re-enable later.)
  void fogh;
  // Boathorns fully removed per @jeffrey — both the ~2:17 and the
  // ~2:40 blasts read foghorn-ish against the ocean bed. The harbour
  // mood is now carried by waves + train-whistle + neigh alone.
  void boath;
  console.log(`  gallop · REAL CC0 horse — ${strideCount} strides + neigh + thunder + steam whistle + ocean/boat/fog (CC0)`);
}

if (false && isChill) { // single-mix refactor: scratch-mix.mjs now owns ALL post-FX
  // Bit-CRUNCH some tracks at times (jas: "bit crunch ... of some
  // tracks at times") — NOT a global constant crush. The pad bus gets
  // a gritty lo-fi crush that deepens through the unstable sonata
  // DEVELOPMENT, and the drum/lead bus gets short crunch STABS on a
  // ~12 s pulse. Deterministic, chill only.
  const bsec = (fr) =>
    (barStartRel[Math.max(0, Math.min(TOTAL_BARS, Math.round(fr * TOTAL_BARS)))] ?? 0) +
    OPENING_PREFIX_SEC;
  applyBitcrush(duckBuf, {
    sampleRate: SAMPLE_RATE, startSec: bsec(0.40), endSec: bsec(0.62),
    bits: [{ time: 0, bits: 9 }, { time: 0.5, bits: 5 }, { time: 1, bits: 9 }],
    downsample: [{ time: 0, downsample: 1 }, { time: 0.5, downsample: 3 }, { time: 1, downsample: 1 }],
    mix: 0.5,
  });
  // PERC GLITCH ENGINE — break/glitch the drum bus SOMETIMES (jas:
  // "sometimes take bit crunching off the perc ... pitch it down ...
  // stretch it at times ... BReeeeeaaaakkk it glitch the perc
  // sometimes"). Deterministic: a ~5 s grid where most slots stay
  // CLEAN and the rest fire one gesture — crush / stutter-roll /
  // pitch-down+stretch / reverse-tear. dryBuf only, length-preserving.
  const gRng = makeRng(SEED_STR + ":perc-glitch");
  const SRg = SAMPLE_RATE;
  const clampG = (a) => Math.max(0, Math.min(dryBuf.length, Math.floor(a)));
  const grainGlitch = (s0, s1, kind, pdRate) => {
    s0 = clampG(s0); s1 = clampG(s1);
    const N = s1 - s0;
    if (N < 128) return;
    const src = dryBuf.slice(s0, s1);
    if (kind === "stutter") {
      const reps = 4 + Math.floor(gRng() * 5);
      const g = Math.max(256, Math.floor(N / reps));
      for (let i = 0; i < N; i++) {
        const blk = Math.floor(i / g);
        const decay = Math.max(0, 1 - 0.22 * blk);
        dryBuf[s0 + i] = src[i % g] * decay;
      }
    } else if (kind === "pitchdown") {
      let p = 0;
      for (let i = 0; i < N; i++) {
        const i0 = Math.floor(p), fr = p - i0;
        const a = src[Math.min(N - 1, i0)], b = src[Math.min(N - 1, i0 + 1)];
        dryBuf[s0 + i] = a + (b - a) * fr;
        p += pdRate;
        if (p >= N - 1) p = (N - 1) * 0.5; // wrap = time-stretch tail
      }
    } else { // reverse-tear: alternate blocks play backwards
      const g = Math.max(256, Math.floor(N / 3));
      for (let i = 0; i < N; i++) {
        const blk = Math.floor(i / g), gi = i % g;
        dryBuf[s0 + i] = blk % 2 === 0 ? src[Math.min(N - 1, blk * g + (g - 1 - gi))] : src[i];
      }
    }
    const fz = Math.min(256, Math.floor(N / 8));
    for (let i = 0; i < fz; i++) { const f = i / fz; dryBuf[s0 + i] *= f; dryBuf[s1 - 1 - i] *= f; }
  };
  for (let ts = 12; ts < musicSec - 4; ts += 5) {
    if (gRng() < 0.45) continue;                       // SOMETIMES clean — perc untouched
    const winSec = 0.5 + gRng() * 1.2;                  // 0.5–1.7 s gesture
    const s0 = ts * SRg, s1 = (ts + winSec) * SRg;
    const pick = gRng();
    if (pick < 0.30) {
      applyBitcrush(dryBuf, { sampleRate: SRg, startSec: ts, endSec: ts + winSec, bits: 5, downsample: 3, mix: 0.6 });
    } else if (pick < 0.58) {
      grainGlitch(s0, s1, "stutter");
    } else if (pick < 0.82) {
      grainGlitch(s0, s1, "pitchdown", 0.5 + gRng() * 0.25); // 0.5–0.75× = down + stretch
    } else {
      grainGlitch(s0, s1, "reverse");
    }
  }
}

// ── sum buses (with dynamic envelope applied to the BED only — the
// machine-gun bus stays loud regardless of section level, and is
// added in AFTER the per-section fx loop so wobble/bitcrush don't
// crush the gun depth) ────────────────────────────────────────────
const out = new Float32Array(totalSamples);
for (let i = 0; i < totalSamples; i++) {
  out[i] = (dryBuf[i] + duckBuf[i] + bassBuf[i]) * dynEnv[i];
}

// ── pre-drop hush ─────────────────────────────────────────────────────
// Gentle dip across 500 ms before each drop using a single smooth
// cosine ease. mgBuf (drop impact + lion + ding) bypasses this duck.
// Skipped in chill mode — chill has no drops, no hush.
if (!isChill) {
  const HUSH_MS = 500;
  const HUSH_SAMPLES = Math.floor((HUSH_MS / 1000) * SAMPLE_RATE);
  for (const r of sectionRanges) {
    if (!r.name.startsWith("drop")) continue;
    const dropIdx = Math.floor(r.startSec * SAMPLE_RATE);
    for (let i = 0; i < HUSH_SAMPLES; i++) {
      const j = dropIdx - HUSH_SAMPLES + i;
      if (j < 0 || j >= out.length) continue;
      const t = i / HUSH_SAMPLES;
      // Smooth cosine ease — continuous gentle dip from 1.0 → 0.30
      // across the whole 500 ms window. No flat segment, no plunge.
      const duck = 0.30 + 0.35 * (1 + Math.cos(Math.PI * t));
      out[j] *= duck;
    }
  }
}

// ── per-section fx envelopes ──────────────────────────────────────────
for (const r of sectionRanges) {
  const fx = r.template.fx;
  if (!fx) continue;
  if (fx.wobble) {
    applyWobble(out, {
      ...fx.wobble,
      sampleRate: SAMPLE_RATE,
      startSec: r.startSec,
      endSec: r.endSec,
    });
  }
  if (fx.bitcrush) {
    applyBitcrush(out, {
      ...fx.bitcrush,
      sampleRate: SAMPLE_RATE,
      startSec: r.startSec,
      endSec: r.endSec,
    });
  }
}

// ── add machine-gun / SFX bus AFTER per-section fx ────────────────────
// Bitcrush would otherwise crunch the gun depth into a thin click. By
// adding mgBuf here, the guns stay full-bandwidth and louder than the
// bed even during bitcrushed drops.
//
// Distance treatment — gives the guns the "far-away" feel they have on
// AC native: gentle one-pole lowpass to roll off the harsh top end +
// short pre-delay reverb tap that trails each bullet. Without these
// the guns sit too close in your face.
{
  // 6.5 kHz one-pole lowpass — kill the snare-snap brightness.
  const cutoffHz = 6500;
  const rc = 1 / (2 * Math.PI * cutoffHz);
  const alpha = (1 / SAMPLE_RATE) / (rc + 1 / SAMPLE_RATE);
  let lp = 0;
  for (let i = 0; i < totalSamples; i++) {
    lp += alpha * (mgBuf[i] - lp);
    mgBuf[i] = lp;
  }
  // Short echo tail — 110ms + 240ms feedback taps, lower-bandwidth.
  // Adds spatial depth so the guns feel like they're echoing across a
  // room rather than right at your ear.
  const tapMs = [110, 240, 420];
  const tapGain = [0.42, 0.26, 0.14];
  for (let k = 0; k < tapMs.length; k++) {
    const tapSamples = Math.floor((tapMs[k] / 1000) * SAMPLE_RATE);
    for (let i = totalSamples - 1; i >= tapSamples; i--) {
      mgBuf[i] += mgBuf[i - tapSamples] * tapGain[k];
    }
  }
}
for (let i = 0; i < totalSamples; i++) {
  out[i] += mgBuf[i] * 0.65; // distant — pulled back further
}

// ── add dry SFX bus (opening sniper) — no distance reverb ─────────────
for (let i = 0; i < totalSamples; i++) {
  out[i] += sfxDryBuf[i];
}

// ── add shutdown bus (bye + chime + tail hats) — NOT tape-stopped ─────
// shutdownBuf was filled after the per-bus tape-stop pass so its
// contents stay at full pitch while the music collapses underneath.
// This is what gives the ending its symmetry with the opening.
for (let i = 0; i < totalSamples; i++) {
  out[i] += shutdownBuf[i];
}

// ── mix vocal stem if provided ────────────────────────────────────────
// VOCAL_MODE controls placement:
//   "single" — once at start of drop1
//   "drops"  — at start of drop1 + drop2 + start of break1 (anthem path)
//   "all"    — at start of every section with vocal:true in template
// Vocal duck: lift surrounding mix slightly to make the vocal poke
// through (small +amp duck of the bed where the vocal lives is faked
// with a higher vocal gain — keeps the bed engine intact).
if (!isChill && VOCAL_STEM && existsSync(VOCAL_STEM)) {
  console.log(`→ mixing vocal stem · ${VOCAL_STEM} · mode=${VOCAL_MODE}`);
  const tmpRaw = `${dirname(OUT_PATH)}/.vocal-stem.f32.raw`;
  const dec = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", VOCAL_STEM,
    "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
    tmpRaw,
  ]);
  if (dec.status === 0 && existsSync(tmpRaw)) {
    const raw = readFileSync(tmpRaw);
    const vocalSrc = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);

    // Peak-normalize the vocal stem to 0.95. /api/say output is famously
    // quiet (~-19 dBFS peak) which loses against the supersaw bed even
    // at high VOCAL_GAIN. Normalize first → gain becomes meaningful.
    let vocalPeak = 0;
    for (let i = 0; i < vocalSrc.length; i++) { const a = Math.abs(vocalSrc[i]); if (a > vocalPeak) vocalPeak = a; }
    const vocal = new Float32Array(vocalSrc.length);
    if (vocalPeak > 0) {
      const norm = 0.95 / vocalPeak;
      for (let i = 0; i < vocalSrc.length; i++) vocal[i] = vocalSrc[i] * norm;
      console.log(`  vocal · stem peak ${(20 * Math.log10(vocalPeak)).toFixed(1)} dBFS → normalized to -0.4 dBFS`);
    } else {
      vocal.set(vocalSrc);
    }

    // Pick anchor times based on vocal mode.
    let anchorSecs = [];
    const vocalDurSec = vocal.length / SAMPLE_RATE;
    if (VOCAL_MODE === "loop") {
      // Tile the stem across the music section of the track, starting
      // AT the opening prefix so the greeting plays in silence and
      // the sung vocal only kicks in when the music does.
      const stride = Math.max(0.1, vocalDurSec - 0.3);
      for (let t = OPENING_PREFIX_SEC; t < totalSec; t += stride) anchorSecs.push(t);
    } else if (VOCAL_MODE === "single") {
      const drop = sectionRanges.find((r) => r.name === "drop1");
      if (drop) anchorSecs = [drop.startSec];
    } else if (VOCAL_MODE === "drops") {
      for (const r of sectionRanges) {
        if (r.name === "drop1" || r.name === "drop2") anchorSecs.push(r.startSec);
        if (r.name === "break1") anchorSecs.push(r.startSec + barSec * 4);
      }
    } else { // "all"
      for (const r of sectionRanges) {
        if (r.template.vocal) anchorSecs.push(r.startSec);
      }
    }

    // Step 1: duck the bed under each vocal placement.
    // In "loop" mode the vocal is continuous → duck applies for the
    // whole track. A deep duck (0.32-0.6) would crush the bed
    // permanently. Use a light, constant duck (0.85) for loop mode;
    // per-placement deep duck for the other modes.
    const fadeS    = Math.floor(0.20 * SAMPLE_RATE); // 200ms fade so loop crossovers don't snap
    if (VOCAL_MODE === "loop") {
      const LOOP_DUCK = Math.max(VOCAL_DUCK, 0.85);
      for (let j = 0; j < out.length; j++) out[j] *= LOOP_DUCK;
    } else {
      const duckSlope = Math.floor(0.06 * SAMPLE_RATE);
      const VOCAL_DUCK_FLOOR = VOCAL_DUCK;
      for (const ts of anchorSecs) {
        const baseIdx = Math.floor(ts * SAMPLE_RATE);
        const endIdx  = baseIdx + vocal.length;
        for (let j = Math.max(0, baseIdx - duckSlope); j < Math.min(out.length, endIdx + duckSlope); j++) {
          let g = VOCAL_DUCK_FLOOR;
          if (j < baseIdx) {
            const t = (baseIdx - j) / duckSlope;
            g = VOCAL_DUCK_FLOOR + (1 - VOCAL_DUCK_FLOOR) * t;
          } else if (j > endIdx) {
            const t = (j - endIdx) / duckSlope;
            g = VOCAL_DUCK_FLOOR + (1 - VOCAL_DUCK_FLOOR) * t;
          }
          if (g < 1) out[j] *= g;
        }
      }
    }

    // Build a per-sample vocal section arc — quiet in intro/breaks,
    // full in drops, silent in builds/outro. Crossfade across section
    // boundaries so the level changes are inaudible.
    const vocalArc = new Float32Array(out.length);
    // Default to 0 in the opening prefix so any vocal mix-in there
    // gets silenced — the greeting owns the first OPENING_PREFIX_SEC.
    const prefixEndIdx = Math.floor(OPENING_PREFIX_SEC * SAMPLE_RATE);
    for (let i = 0; i < out.length; i++) vocalArc[i] = i < prefixEndIdx ? 0 : 1;
    for (const r of sectionRanges) {
      const g = VOCAL_SECTION_GAIN[r.name] ?? 1;
      const startIdx = Math.floor(r.startSec * SAMPLE_RATE);
      const endIdx   = Math.floor(r.endSec   * SAMPLE_RATE);
      for (let i = startIdx; i < endIdx && i < out.length; i++) vocalArc[i] = g;
    }
    // 250 ms boxcar smoothing across section boundaries.
    const arcSmooth = Math.max(1, Math.floor(0.125 * SAMPLE_RATE));
    {
      const src = Float32Array.from(vocalArc);
      let acc = 0;
      const W = arcSmooth * 2 + 1;
      for (let i = 0; i < arcSmooth && i < src.length; i++) acc += src[i];
      for (let i = 0; i < src.length; i++) {
        const add = i + arcSmooth < src.length ? src[i + arcSmooth] : src[src.length - 1];
        const sub = i - arcSmooth - 1 >= 0 ? src[i - arcSmooth - 1] : src[0];
        acc += add - sub;
        vocalArc[i] = acc / W;
      }
    }

    // Step 2: mix the vocal in with short fades so repeats don't pop.
    let vocalMixCount = 0;
    for (const ts of anchorSecs) {
      const baseIdx = Math.floor(ts * SAMPLE_RATE);
      for (let i = 0; i < vocal.length; i++) {
        const j = baseIdx + i;
        if (j < 0 || j >= out.length) continue;
        let fade = 1;
        if (i < fadeS) fade = i / fadeS;
        else if (i > vocal.length - fadeS) fade = Math.max(0, (vocal.length - i) / fadeS);
        out[j] += vocal[i] * VOCAL_GAIN * fade * vocalArc[j];
      }
      events.vox.push({ t: ts, name: "vocal", dur: vocalDurSec });
      vocalMixCount++;
    }

    // ── chopped & screwed at drops ────────────────────────────────────
    // At DROP1 only: layer a DJ-Screw style slowed + pitched-down
    // version of a vocal slice + a couple of quiet stutter-chops snapped
    // to the 16th-note grid. Drop2 is intentionally cleaner — its
    // entry reads as one event, with the held gunfire as the only
    // sustained texture (per user feedback that drop2 felt like "two
    // drops" at the 1:00 mark).
    for (const r of sectionRanges) {
      if (r.name !== "drop1") continue;
      const dropIdx = Math.floor(r.startSec * SAMPLE_RATE);

      // Source slice — read from a "memorable" position in the vocal
      // (3.5s in — usually after the first phrase has settled).
      const srcStartSec = Math.min(3.5, (vocal.length / SAMPLE_RATE) * 0.3);
      const srcStartIdx = Math.floor(srcStartSec * SAMPLE_RATE);
      const screwDurSec = 1.5; // 1.5s slowed → reads 0.75s of vocal
      const screwOutSamples = Math.floor(screwDurSec * SAMPLE_RATE);
      const screwFadeS = Math.floor(0.06 * SAMPLE_RATE);

      // Screwed (pitched-down by 1 octave via 2× duration via linear
      // resample) — kicks in at the drop entry.
      for (let i = 0; i < screwOutSamples; i++) {
        const j = dropIdx + i;
        if (j < 0 || j >= out.length) continue;
        const srcF = srcStartIdx + i * 0.5;
        const s0 = Math.floor(srcF);
        const s1 = s0 + 1;
        if (s1 >= vocal.length) break;
        const frac = srcF - s0;
        const sample = vocal[s0] * (1 - frac) + vocal[s1] * frac;
        let fade = 1;
        if (i < screwFadeS) fade = i / screwFadeS;
        else if (i > screwOutSamples - screwFadeS) fade = Math.max(0, (screwOutSamples - i) / screwFadeS);
        out[j] += sample * VOCAL_GAIN * 0.65 * fade;
      }

      // Chop stutter — TWO quiet triplet chops snapped to the 16th-note
      // grid relative to the drop entry. Previous 4-chop pattern at
      // arbitrary intervals registered as a second drop, especially at
      // the 1-minute drop2 entry. Now they're closer to a tasteful fill
      // than a separate drop event.
      const sixteenthSec = (60 / BPM) / 4;
      const chopSliceSamples = Math.floor(0.18 * SAMPLE_RATE);
      const chopStartIdx = dropIdx + screwOutSamples;
      const chopCount = 2;
      for (let c = 0; c < chopCount; c++) {
        // Snap each chop to the nearest 16th-note position.
        const chopOutIdx = chopStartIdx + Math.floor(c * sixteenthSec * SAMPLE_RATE);
        for (let i = 0; i < chopSliceSamples; i++) {
          const j = chopOutIdx + i;
          if (j < 0 || j >= out.length) continue;
          const src = srcStartIdx + Math.floor(chopSliceSamples * 0.5) + i;
          if (src >= vocal.length) break;
          let fade = 1;
          const eIn = Math.floor(0.005 * SAMPLE_RATE);
          const eOut = Math.floor(0.020 * SAMPLE_RATE);
          if (i < eIn) fade = i / eIn;
          else if (i > chopSliceSamples - eOut) fade = Math.max(0, (chopSliceSamples - i) / eOut);
          // Quieter chops — they're an ornament, not a re-entry.
          const chopGain = 0.30 - c * 0.10;
          out[j] += vocal[src] * VOCAL_GAIN * chopGain * fade;
        }
      }
    }
    try { unlinkSync(tmpRaw); } catch {}
    console.log(`  vocal · ${vocalMixCount} placements · gain=${VOCAL_GAIN}`);
  } else {
    console.warn(`✗ vocal decode failed — skipping`);
  }
}

// ── screamed build vocals ────────────────────────────────────────────
// Adds the screamed command phrases ("turn around", "look up", "look
// down", "look in circles", "stare in spirals") on top of each build
// section. Raw stem cached at pop/dance/out/.trance-screams.mp3.
//
// MELODIC PREPROCESSING — each build gets its own melodic stem,
// rendered to EXACTLY that build's duration so the 12 ascending notes
// stretch evenly across the full build (not just the first few
// seconds). Cache key includes build duration so build1 (7.83s) and
// build2 (3.91s) each get their own pre-rendered file.
//
// Pitch+time decoupling uses ffmpeg's asetrate + aresample + atempo
// chain — pitch shift via asetrate, time-stretch via atempo, the
// aresample brings the rate back to SR after the asetrate.
const SCREAMS_RAW_PATH = `${REPO}/pop/dance/out/.trance-screams.mp3`;
// 3 notes climbing — each held very long so consonants smear into
// sustained vowel tones (the "SUNG" quality the user wants). With
// build1 = 7.87 s split 3 ways, each note holds ~2.6 s — well into
// sustained-pitch territory. Pitch ratios stay ≤ 2× so atempo math
// is clean (no squeegee).
const SCREAMS_CLIMB_SEMIS = [0, 7, 12];
const SCREAMS_RAW_DUR_SEC = 2.229116; // measured length of the raw stem
                                       // ("i fucked up, i made a sound. still around.")
function buildMelodicScreamsStem(targetDurSec) {
  const cachePath = `${REPO}/pop/dance/out/.trance-screams-melodic-${targetDurSec.toFixed(2)}s.mp3`;
  if (existsSync(cachePath)) return cachePath;
  if (!existsSync(SCREAMS_RAW_PATH)) return null;
  const N = SCREAMS_CLIMB_SEMIS.length;
  const inputSliceSec  = SCREAMS_RAW_DUR_SEC / N;
  const outputSliceSec = targetDurSec / N;
  const filterParts = [];
  for (let i = 0; i < N; i++) {
    const semis = SCREAMS_CLIMB_SEMIS[i];
    const pitchRatio = Math.pow(2, semis / 12);
    // After asetrate=SR*pitchRatio: slice duration = inputSliceSec / pitchRatio
    // After atempo=A: slice duration = inputSliceSec / (pitchRatio * A)
    // We want output = outputSliceSec → A = inputSliceSec / (pitchRatio * outputSliceSec)
    let atempo = inputSliceSec / (pitchRatio * outputSliceSec);
    // atempo only supports 0.5..100 per filter; chain multiple if needed.
    const atempoChain = [];
    while (atempo < 0.5) { atempoChain.push("0.5"); atempo /= 0.5; }
    while (atempo > 100) { atempoChain.push("2");   atempo /= 2;   }
    atempoChain.push(atempo.toFixed(4));
    const start = (i * inputSliceSec).toFixed(4);
    const end   = ((i + 1) * inputSliceSec).toFixed(4);
    // Per-slice chain:
    //   atrim   →  slice
    //   asetrate + aresample  →  pitch shift
    //   atempo  →  time stretch
    //   agate   →  dry out the natural ElevenLabs verb tail between
    //              words (fast attack/release, -28 dB threshold)
    //   equalizer 1.8kHz +5dB  →  emphasize the whistle band
    //   equalizer 280Hz -3dB   →  thin out chest resonance for a
    //                             more "whistle-like" tonal character
    filterParts.push(
      `[0:a]atrim=${start}:${end},asetrate=${SAMPLE_RATE}*${pitchRatio.toFixed(4)},aresample=${SAMPLE_RATE},${atempoChain.map((a) => `atempo=${a}`).join(",")},agate=threshold=-28dB:ratio=6:attack=8:release=120,equalizer=f=1800:t=q:w=1.2:g=5,equalizer=f=280:t=q:w=1.0:g=-3[s${i}]`
    );
  }
  const concatInputs = Array.from({ length: N }, (_, i) => `[s${i}]`).join("");
  const fullFilter = [...filterParts, `${concatInputs}concat=n=${N}:v=0:a=1[out]`].join(";");
  const r = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", SCREAMS_RAW_PATH,
    "-filter_complex", fullFilter,
    "-map", "[out]",
    "-c:a", "libmp3lame", "-q:a", "3",
    cachePath,
  ]);
  if (r.status === 0) {
    console.log(`  screams · melodic stem rendered (${N} notes → ${targetDurSec.toFixed(2)}s)`);
    return cachePath;
  }
  console.warn(`✗ melodic screams render failed at ${targetDurSec.toFixed(2)}s`);
  return null;
}

// Render a melodic stem for each unique build duration up front so the
// mix step can decode them as needed.
const SCREAMS_PER_BUILD = new Map(); // sectionName → path
// Gated by !isChill: the chill mix never mixes screams (see below), so
// don't waste an ffmpeg pass pre-rendering a stem it won't use.
if (!isChill && existsSync(SCREAMS_RAW_PATH)) {
  for (const r of sectionRanges) {
    if (!r.name.startsWith("build")) continue;
    const dur = r.endSec - r.startSec;
    const path = buildMelodicScreamsStem(dur);
    if (path) SCREAMS_PER_BUILD.set(r.name, path);
  }
}
// Decode each build's pre-rendered melodic stem and mix at its
// position. Each build has its own stem (already stretched/pitched to
// fit its exact duration), so the 12 ascending notes always span the
// FULL build — no compressed-to-first-few-seconds bug.
if (!isChill && SCREAMS_PER_BUILD.size > 0) {
  let screamCount = 0;
  const screamGain = 0.55;
  const screamFadeS = Math.floor(0.05 * SAMPLE_RATE);
  for (const r of sectionRanges) {
    if (!r.name.startsWith("build")) continue;
    const stemPath = SCREAMS_PER_BUILD.get(r.name);
    if (!stemPath || !existsSync(stemPath)) continue;
    const tmpScreams = `${dirname(OUT_PATH)}/.screams-${r.name}.f32.raw`;
    const dec = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", stemPath,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpScreams,
    ]);
    if (dec.status !== 0 || !existsSync(tmpScreams)) continue;
    const raw = readFileSync(tmpScreams);
    const screamsSrc = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
    let peak = 0;
    for (let i = 0; i < screamsSrc.length; i++) { const a = Math.abs(screamsSrc[i]); if (a > peak) peak = a; }
    const screams = new Float32Array(screamsSrc.length);
    if (peak > 0) {
      const norm = 0.95 / peak;
      for (let i = 0; i < screamsSrc.length; i++) screams[i] = screamsSrc[i] * norm;
    } else screams.set(screamsSrc);
    const outStartIdx = Math.floor(r.startSec * SAMPLE_RATE);
    const buildSamples = Math.floor((r.endSec - r.startSec) * SAMPLE_RATE);
    const outSamples = Math.min(screams.length, buildSamples);
    events.vox.push({ t: r.startSec, name: `screams-${r.name}`, dur: outSamples / SAMPLE_RATE });
    for (let i = 0; i < outSamples; i++) {
      const j = outStartIdx + i;
      if (j < 0 || j >= out.length) continue;
      let fade = 1;
      if (i < screamFadeS) fade = i / screamFadeS;
      else if (i > outSamples - screamFadeS) fade = Math.max(0, (outSamples - i) / screamFadeS);
      out[j] += screams[i] * screamGain * fade;
    }
    try { unlinkSync(tmpScreams); } catch {}
    screamCount++;
  }
  console.log(`  screams · ${screamCount} build placements · gain=${screamGain}`);
}

// ── master mix-down — proper headroom for the ffmpeg chain ───────────
// Many voices stack here (lead + pad + supersaw + bass + bells + harp +
// drop-impact + machine guns + vocal). Pre-mix peaks routinely hit 3-5×
// full-scale; the previous softClip(1.15) tanh-saturated everything
// above 1.15 hard, which sounded like clipping rather than glue.
//
// New strategy: measure peak, scale to a sensible pre-master headroom
// (0.85), then soft-clip with a low threshold so only the very rare
// over-target peaks get gentle saturation. The ffmpeg chain that
// follows runs a proper compressor + alimiter brick wall so this stage
// only needs to deliver clean audio in the right ballpark.
{
  let peakIn = 0;
  for (let i = 0; i < out.length; i++) { const a = Math.abs(out[i]); if (a > peakIn) peakIn = a; }
  if (peakIn > 0) {
    // Lower pre-master headroom — house-style mixes prioritize air
    // and dynamics over loudness; the alimiter + loudnorm chain
    // brings the perceived level back without crushing transients.
    const target = 0.72;
    const gain = target / peakIn;
    for (let i = 0; i < out.length; i++) out[i] *= gain;
    console.log(`  master · peak ${(20 * Math.log10(peakIn)).toFixed(1)} dBFS → -${(-20 * Math.log10(target)).toFixed(1)} dBFS`);
  }
  // Catch the very rare overshoot from the wobble/bitcrush stages.
  softClip(out, 0.95);
}


// ── write structure JSON (for diagram + downstream tooling) ───────────
const structJson = {
  meter: METER,
  bpm: BPM,
  scale: SCALE_NAME,
  rootMidi: ROOT_MIDI,
  totalBars: TOTAL_BARS,
  totalSec,
  sections: sectionRanges.map((r) => ({
    name: r.name,
    startBar: r.startBar,
    endBar: r.endBar,
    startSec: r.startSec,
    endSec: r.endSec,
    layers: {
      kick: !!r.template.kick,
      hat: !!r.template.hat,
      sub: !!r.template.sub,
      pad: !!r.template.pad,
      lead: !!r.template.lead,
      piano: !!r.template.piano,
      bells: !!r.template.bells,
      riser: !!r.template.riser,
      snareRoll: !!r.template.snareRoll,
      supersaw: !!r.template.supersaw,
      vocal: !!r.template.vocal,
    },
    fx: r.template.fx ? Object.keys(r.template.fx) : [],
  })),
  counts: { kickCount, hatCount, subCount, leadCount, padCount, pianoCount, bellsCount, riserCount, snareRollCount, supersawCount, dropImpactCount, harpCount },
  // Per-note events for the scrolling-score video visualizer.
  events,
};
// Tuck intermediate files (struct.json, etc.) under a `<basename>.assets/`
// subfolder next to the final mp3 so the desktop stays uncluttered.
const baseStem = OUT_PATH.replace(/\.mp3$/, "");
const assetsDir = `${baseStem}.assets`;
const defaultStruct = `${assetsDir}/struct.json`;
const finalStructPath = STRUCT_PATH || defaultStruct;
mkdirSync(dirname(finalStructPath), { recursive: true });
writeFileSync(finalStructPath, JSON.stringify(structJson, null, 2));
console.log(`→ struct · ${finalStructPath}`);

// ── write mp3 ─────────────────────────────────────────────────────────
const outDir = dirname(OUT_PATH);
mkdirSync(outDir, { recursive: true });
const rawPath = `${outDir}/.trance-${SEED_STR}.f32.raw`;

// ── AC voice stamp (release master only) ───────────────────────────
// High-pitched "aesthetic dot computer" branding ID dropped mid-track
// (~13 s, just after the music enters). Deterministic seed → re-cutting
// --master reproduces the exact same take with just this added.
let centerStampData = null; // chill: stashed here, overlaid CLEAN post-scratch
if (RELEASE_MASTER) {
  const STAMP_PATH = `${REPO}/pop/dance/out/.ac-dot-stamp-vocal.mp3`;
  if (existsSync(STAMP_PATH)) {
    const tmpStamp = `${outDir}/.ac-dot-stamp.f32.raw`;
    const dec = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", STAMP_PATH,
      "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
      tmpStamp,
    ]);
    if (dec.status === 0 && existsSync(tmpStamp)) {
      const raw = readFileSync(tmpStamp);
      const st = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength / 4);
      let speak = 0;
      for (let i = 0; i < st.length; i++) { const a = Math.abs(st[i]); if (a > speak) speak = a; }
      const norm = speak > 0 ? 0.85 / speak : 1.0;
      // Chill: stretch it WAY out — slow + low (PITCH < 1 lengthens the
      // playback and drops the pitch). Non-chill keeps the quick high ID.
      const PITCH = isChill ? 0.55 : 1.6;
      const STAMP_SEC = 58.0;            // early in the build2 climb into drop2
      const startIdx = Math.floor(STAMP_SEC * SAMPLE_RATE);
      const outLen = Math.floor(st.length / PITCH);
      const fadeS = Math.floor(0.03 * SAMPLE_RATE);
      const GAIN = 0.18;                 // quiet — a subtle whisper ID
      for (let i = 0; i < outLen; i++) {
        const j = startIdx + i;
        if (j < 0 || j >= out.length) break;
        const s = st[Math.floor(i * PITCH)] || 0;
        let fade = 1;
        if (i < fadeS) fade = i / fadeS;
        else if (i > outLen - fadeS) fade = Math.max(0, (outLen - i) / fadeS);
        out[j] += s * norm * GAIN * fade;
      }
      // Chill: a SECOND "aesthetic dot computer" stamp goes dead centre
      // — over the scratch but NOT scratched (jas: "should not be
      // scratched ... but overlay over the scratch"). So instead of
      // mixing it into out[] (which the scratch pass would mangle), the
      // decoded vocal is stashed here and overlaid CLEAN onto the
      // stereo buffer AFTER the scratch pass.
      if (isChill) {
        centerStampData = { st: Float32Array.from(st), norm };
        console.log(`  stamp · centre "aesthetic dot computer" (clean overlay, post-scratch)`);
      }
      try { unlinkSync(tmpStamp); } catch {}
      console.log(`  stamp · "aesthetic dot computer" (${isChill ? "stretched slow+low" : "high-pitched"}) at ${STAMP_SEC}s`);
    }
  }
}

// ── STEREO REFACTOR (chill only) ─────────────────────────────────────
// Non-chill keeps the exact mono raw write → byte-identical trancenwaltz.
// Chill rebuilds a true STEREO pair from the individual buses, each with
// its own slow independent pan LFO — genuine per-track "panning stories"
// (bass sways wide + slow, pad/saw drifts opposite, drums/lead stay
// near-centre, the sfx voices — whistle/cat/gong — wander widest).
// Equal-power panning. dynEnv matches the original (dry+duck+bass)*env;
// sfx/mg/shutdown added flat as in the mono path.
let STEREO = false;
let buf;
if (isChill) {
  STEREO = true;
  buf = Buffer.alloc(out.length * 2 * 4);
  const SR = SAMPLE_RATE;
  for (let i = 0; i < out.length; i++) {
    const t = i / SR;
    const pBass = 0.55 * Math.sin(t * 2 * Math.PI * 0.028);
    const pPad  = 0.40 * Math.sin(t * 2 * Math.PI * 0.017 + 2.1);
    const pDry  = 0.14 * Math.sin(t * 2 * Math.PI * 0.045 + 4.0);
    const pSfx  = 0.85 * Math.sin(t * 2 * Math.PI * 0.022 + 1.0);
    const m = dynEnv[i];
    const dB = bassBuf[i] * m, dD = duckBuf[i] * m, dY = dryBuf[i] * m;
    const sX = sfxDryBuf[i];
    const ctr = (mgBuf[i] * 0.65 + shutdownBuf[i]) * 0.7071;
    const aB = (pBass + 1) * Math.PI / 4, aD = (pPad + 1) * Math.PI / 4;
    const aY = (pDry + 1) * Math.PI / 4,  aS = (pSfx + 1) * Math.PI / 4;
    let L = dB * Math.cos(aB) + dD * Math.cos(aD) + dY * Math.cos(aY) + sX * Math.cos(aS) + ctr;
    let R = dB * Math.sin(aB) + dD * Math.sin(aD) + dY * Math.sin(aY) + sX * Math.sin(aS) + ctr;
    if (ambientBuf && ambientBuf.length) {
      // The field-recording "castle" — a looped, slowly breathing low
      // ambient bed UNDER everything, slightly decorrelated L/R so the
      // whole track sits inside a real room/space.
      const ai = i % ambientBuf.length;
      const und = 0.78 + 0.22 * Math.sin((i / SR) * 2 * Math.PI * 0.05);
      const ag = 0.09 * und; // tones quieter — field bed well back behind the percussion
      L += ambientBuf[ai] * ag;
      R += ambientBuf[(ai + 240) % ambientBuf.length] * ag; // ~5 ms offset = width
    }
    buf.writeFloatLE(L, i * 8);
    buf.writeFloatLE(R, i * 8 + 4);
  }
} else {
  buf = Buffer.alloc(out.length * 4);
  for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
}
if (STEREO && !isChill) { // perfect-loop chill: NO end power-down (it broke the seam)
  // Last ~16 s: a vari-speed POWER-DOWN — re-read the tail at a falling
  // rate so the pitch + tempo of EVERYTHING drop together in unison (a
  // dying-machine wind-down). Length unchanged; trails into the fade.
  const SRr = SAMPLE_RATE;
  const frames = out.length;
  const winN = Math.min(frames, Math.floor(16 * SRr));
  const startF = frames - winN;
  const sl = new Float32Array(winN), sr = new Float32Array(winN);
  for (let k = 0; k < winN; k++) {
    sl[k] = buf.readFloatLE((startF + k) * 8);
    sr[k] = buf.readFloatLE((startF + k) * 8 + 4);
  }
  let srcPos = 0;
  for (let k = 0; k < winN; k++) {
    const rate = 1 - 0.28 * (k / winN);   // 1.0 → ~0.72 : gentle, not shutdowny
    const i0 = Math.floor(srcPos);
    const fr = srcPos - i0;
    let L = 0, R = 0;
    if (i0 + 1 < winN) {
      L = sl[i0] * (1 - fr) + sl[i0 + 1] * fr;
      R = sr[i0] * (1 - fr) + sr[i0 + 1] * fr;
    } else if (i0 < winN) { L = sl[winN - 1]; R = sr[winN - 1]; }
    buf.writeFloatLE(L, (startF + k) * 8);
    buf.writeFloatLE(R, (startF + k) * 8 + 4);
    srcPos += rate;
  }
}
if (STEREO) {
  // GLOBAL STORY (chill) — slow, independent LFOs across the WHOLE
  // track on low-pass + high-pass + stereo width + amplitude, so the
  // mix breathes/opens/narrows and elements rotate into the spotlight
  // (also de-muds: the moving filters carve space). One-pole filters.
  const SRr = SAMPLE_RATE;
  const frames = out.length;
  const lfo = (i, P, ph, lo, hi) => lo + (hi - lo) * (0.5 + 0.5 * Math.sin((i / SRr / P + ph) * 2 * Math.PI));
  let lpL = 0, lpR = 0, hpL = 0, hpR = 0;
  for (let i = 0; i < frames; i++) {
    let L = buf.readFloatLE(i * 8), R = buf.readFloatLE(i * 8 + 4);
    const aLP = Math.min(1, (2 * Math.PI * lfo(i, 41, 0.0, 4500, 15000)) / SRr); // stays bright/airy (kawaii)
    lpL += aLP * (L - lpL); lpR += aLP * (R - lpR);
    L = lpL; R = lpR;
    const aHP = Math.min(1, (2 * Math.PI * lfo(i, 57, 0.4, 26, 230)) / SRr);
    hpL += aHP * (L - hpL); hpR += aHP * (R - hpR);
    L -= hpL; R -= hpR;
    const w = lfo(i, 49, 0.7, 0.60, 1.45);
    const mid = (L + R) * 0.5, side = (L - R) * 0.5 * w;
    L = mid + side; R = mid - side;
    const amp = lfo(i, 33, 0.2, 0.74, 1.0);
    buf.writeFloatLE(L * amp, i * 8);
    buf.writeFloatLE(R * amp, i * 8 + 4);
  }
}
if (false && STEREO && isChill) { // single-mix refactor: scratch-mix.mjs owns the scratch
  // ── 3 s HIP-HOP TURNTABLE SCRATCH (dead centre) ──────────────────
  // jas: the mid-track moment should be "like a scratch hip hop
  // scratch". A bar (~1.4 s) is too coarse for the bpm curve to
  // scratch, so it's done in the audio domain: re-read the centre
  // window with a zig-zag (forward/back) playback pointer + a
  // crossfader chop that ducks the backward strokes. Endpoints stay
  // sample-aligned (env=0 at both edges → pos==k) so the rest of the
  // track is untouched and the length is unchanged.
  // GNARLY: deep accelerating RIPs that spiral in, each snapped back
  // with a fast TEAR, on a beat-snapped grid, with a transformer chop
  // on the rips (jas: "more gnarly more spiraly more DEEP RIP and back
  // ... rip tear repeat beat snapped scratchin"). Endpoints stay
  // sample-aligned (env→0 at edges) so the track length/continuity
  // around the window is unchanged.
  const SRr = SAMPLE_RATE, frames = out.length;
  const winN = Math.min(Math.floor(3.0 * SRr), frames - 2);
  const w0 = Math.max(0, Math.floor(frames / 2) - Math.floor(winN / 2));
  const guard = Math.floor(1.0 * SRr);
  const g0 = Math.max(0, w0 - guard);
  const g1 = Math.min(frames, w0 + winN + guard);
  const N = g1 - g0;
  const sl = new Float32Array(N), sr = new Float32Array(N);
  for (let k = 0; k < N; k++) {
    sl[k] = buf.readFloatLE((g0 + k) * 8);
    sr[k] = buf.readFloatLE((g0 + k) * 8 + 4);
  }
  const STROKES = 9;                 // rip-tear strokes on a beat-snapped grid
  const strokeLen = winN / STROKES;
  const A = 0.62 * SRr;              // DEEP platter throw (samples)
  const baseOff = w0 - g0;
  for (let k = 0; k < winN; k++) {
    const tau = k / winN;                                 // 0..1
    const env = 0.5 - 0.5 * Math.cos(2 * Math.PI * tau);   // 0→1→0 ease
    const s = Math.floor(k / strokeLen);                   // stroke index
    const u = (k - s * strokeLen) / strokeLen;             // 0..1 in stroke
    // RIP (first 70 %): accelerating dive forward — spiral.
    // TEAR (last 30 %): fast snap back.
    let exc;
    if (u < 0.7) exc = Math.pow(u / 0.7, 1.8);
    else { const tu = (u - 0.7) / 0.3; exc = (1 - tu) * (1 - tu); }
    const deepen = 0.45 + 0.55 * env;                      // deepest mid-window
    const dir = (s % 2 === 0) ? 1 : 0.62;                  // alternate depth
    const drift = 0.18 * SRr * Math.sin(Math.PI * tau);    // travel out & back
    let srcF = baseOff + k + drift + A * exc * deepen * dir * env;
    if (srcF < 0) srcF = 0; else if (srcF > N - 2) srcF = N - 2; // stay in snapshot
    const i0 = Math.floor(srcF), frc = srcF - i0;
    const L = sl[i0] * (1 - frc) + sl[i0 + 1] * frc;
    const R = sr[i0] * (1 - frc) + sr[i0 + 1] * frc;
    // Crossfader: open on the RIP with a fast transformer chop
    // (snapped), hard-ducked on the TEAR back — gnarly articulation.
    let chop;
    if (u < 0.7) {
      const trans = Math.sin(2 * Math.PI * (k / SRr) * 41) > -0.25 ? 1 : 0.35;
      chop = 0.58 + 0.42 * trans;
    } else chop = 0.16;                                    // tear = cut
    const gmix = (1 - env) + env * chop;                   // only inside window
    buf.writeFloatLE(L * gmix, (w0 + k) * 8);
    buf.writeFloatLE(R * gmix, (w0 + k) * 8 + 4);
  }
}
if (false && STEREO && isChill && centerStampData) { // single-mix: scratch-mix.mjs owns the centre ID
  // CLEAN "aesthetic dot computer" overlay — sits dead-centre, ON TOP
  // of the (already-scratched) music, NOT scratched itself. Equal
  // L/R (centred), near-natural pitch, hearable above the chop.
  const { st, norm } = centerStampData;
  const PITCH2 = 0.92, GAIN2 = 0.62;
  const SRr = SAMPLE_RATE;
  const outLen2 = Math.floor(st.length / PITCH2);
  const fadeS = Math.floor(0.03 * SRr);
  const startIdx2 = Math.floor(out.length / 2) - Math.floor(outLen2 / 2);
  for (let i = 0; i < outLen2; i++) {
    const j = startIdx2 + i;
    if (j < 0 || j >= out.length) continue;
    const s2 = (st[Math.floor(i * PITCH2)] || 0) * norm * GAIN2;
    let f2 = 1;
    if (i < fadeS) f2 = i / fadeS;
    else if (i > outLen2 - fadeS) f2 = Math.max(0, (outLen2 - i) / fadeS);
    const add = s2 * f2;
    buf.writeFloatLE(buf.readFloatLE(j * 8) + add, j * 8);
    buf.writeFloatLE(buf.readFloatLE(j * 8 + 4) + add, j * 8 + 4);
  }
  console.log(`  stamp · centre overlay clean over the scratch at ~${(out.length / 2 / SAMPLE_RATE).toFixed(1)}s`);
}
writeFileSync(rawPath, buf);

// Mono f32 → stereo mp3 with:
//   reverb taps panned L + R         spatial depth
//   3-band mid-range EQ boost        fills the sine-only mix where it lacks bite
//   master compressor                glues dynamics, fills out the loud sections
//   loudnorm                         EBU R128 perceived-loudness normalize so
//                                    the whole track sits at a consistent level
//                                    (replaces brick-wall alimiter)
// Master chain — proper headroom and limiting so the final mix isn't
// hard-clipping like the previous gentle-compressor + loudnorm-only
// path. Input arrives at ~-1.5 dBFS peak from the pre-master soft
// clip. Chain:
//   stereo reverb taps  →  short fades  →  gentle EQ to fill the mids
//   →  stereo width  →  proper musical compressor  →  brick-wall
//   alimiter (-1 dBTP)  →  perceptual loudnorm (quieter target so the
//   compressor stays in control of dynamics).
// Time-varying reverb envelope — wet signals fade in over the first
// 1.5 s ("reverb coming online" at the boot) and fade out smoothly
// over the last 3 s ("returning to reality" as the system shuts
// down). Fade-out uses a quadratic ease so the transition to dry
// audio doesn't have a perceptible volume "skip" near the very end.
const REVERB_FADE_IN_SEC = 1.5;
const REVERB_FADE_OUT_START = totalSec - 3.0;
const REVERB_FADE_OUT_END = totalSec;
// pow(max(0, (END-t)/3), 2) — quadratic ease-out from 1 → 0 over 3 s.
// Squaring keeps the last second very soft so there's no abrupt drop.
const wetEnv =
  `if(lt(t\\,${REVERB_FADE_IN_SEC})\\,t/${REVERB_FADE_IN_SEC}\\,` +
  `if(gt(t\\,${REVERB_FADE_OUT_START})\\,` +
  `pow(max(0\\,(${REVERB_FADE_OUT_END}-t)/3)\\,2)\\,1))`;
// Chill wants a TINY, soft, dead room — a small carpeted basement, NOT
// the long house reverb: short, absorbed early reflections, very low
// wet, narrow image. The master is also eased for the quiet bed
// (gentler glue compression, softer loudness) so nothing is squashed.
const reverbDefs = isChill
  // DRY + SPREAD: the old short 7–37 ms taps comb-filtered the
  // sustained pure sines into a fixed metallic RING and smushed the
  // space (jas: "definitely the reverb on the pads ... too heavy ...
  // more spread out ... less SMUSHSHED ... more walloped"). Now:
  // long, sparse, distinct echoes (>50 ms — no comb ring) thrown HARD
  // L / hard R for width, at a fraction of the wet level so it stays
  // dry and punchy.
  ? `[0:a]aecho=0.5:0.35:67|151|241:0.10|0.055|0.03,pan=stereo|c0=0.92*c0|c1=0.08*c0,volume='${wetEnv}':eval=frame[wet1];` +
    `[0:a]aecho=0.5:0.35:97|193:0.075|0.04,pan=stereo|c0=0.08*c0|c1=0.92*c0,volume='${wetEnv}':eval=frame[wet2];`
  : `[0:a]aecho=0.85:0.85:60|140|260|480|820:0.40|0.27|0.18|0.11|0.06,pan=stereo|c0=0.35*c0|c1=0.65*c0,volume='${wetEnv}':eval=frame[wet1];` +
    `[0:a]aecho=0.85:0.85:90|200|360|620:0.30|0.20|0.13|0.08,pan=stereo|c0=0.65*c0|c1=0.35*c0,volume='${wetEnv}':eval=frame[wet2];`;
const wetWeights = isChill ? "1 0.025 0.018" : "1 0.28 0.28"; // chill: even less reverb (jas)
const stereoStage = isChill ? "extrastereo=m=1.05:c=true," : "extrastereo=m=1.5:c=true,";
const compStage = isChill
  // very light glue only — let the natural dynamics breathe, then a
  // gentle recompress (high threshold, ~1.25:1, soft knee).
  ? "acompressor=threshold=-24dB:ratio=1.25:attack=40:release=400:makeup=1:knee=8,"
  : "acompressor=threshold=-14dB:ratio=1.8:attack=18:release=200:makeup=1:knee=4,";
const loudStage = isChill
  ? "loudnorm=I=-22:TP=-2:LRA=26[out]" // quiet + wide — natural loudness shines
  : "loudnorm=I=-15:TP=-1.5:LRA=20[out]";

const filter =
  (STEREO ? "[0:a]acopy[dry];" : "[0:a]pan=stereo|c0=c0|c1=c0[dry];") +
  // Long multi-tap reverb (60-820 ms) — restored. The short cluster
  // tried earlier (12-95 ms) made the mix feel boxy + the aggressive
  // compressor that paired with it pumped audibly. Back to the prior
  // tap layout that gave the house-y depth.
  reverbDefs +
  `[dry][wet1][wet2]amix=inputs=3:weights='${wetWeights}':duration=longest:normalize=0,` +
  // PERFECT LOOP — the bed is bar-aligned so totalSec wraps to t=0 on
  // the next downbeat, and the kick-settle ending already winds down to
  // the same gentle level the intro starts at. The old 0.15s fade-in /
  // 1.6s fade-out drove both ends to digital silence → an audible dip
  // at the loop seam. Replaced with ~6ms declicks (sub-perceptual, just
  // kills end-sample pops) so the ending flows straight into the intro.
  // Kawaii cut: chill gets a GENTLE ~6 s fade-out (bedroom-soft, never
  // an abrupt cutoff — the bar-aligned bed sustains under it). Non-chill
  // --master keeps its 1.6 s musical fade; others = ~6 ms declick.
  (isChill
    ? "afade=t=in:st=0.4:d=2.0," +
      `afade=t=out:st=${(totalSec - 10).toFixed(3)}:d=10,`
    : (RELEASE_MASTER
      ? "afade=t=in:st=0:d=0.15," +
        `afade=t=out:st=${(totalSec - 1.6).toFixed(3)}:d=1.6,`
      : "afade=t=in:st=0:d=0.006," +
        `afade=t=out:st=${(totalSec - 0.006).toFixed(3)}:d=0.006,`)) +
  // House-style EQ — more AIR up top, less mid crowding, subtle low.
  "equalizer=f=180:t=q:w=1.2:g=1.0," +
  "equalizer=f=900:t=q:w=1.4:g=0.8," +
  "equalizer=f=2400:t=q:w=1.3:g=1.0," +
  "equalizer=f=10000:t=q:w=0.9:g=2.5," +
  "equalizer=f=60:t=q:w=1.0:g=1.0," +
  stereoStage +
  // Gentle compressor — was 1.6:1 / 20 ms / 240 ms. The 2.4:1 / 4 ms
  // version was creating an audible oscillating envelope pump on the
  // master. Back to slow musical compression that glues without
  // breathing.
  compStage +
  // Brick-wall safety limiter — mid-ceiling so the limiter isn't
  // working hard but still catches stray peaks. 0.94 was clipping
  // the loudnorm chain into pumping; 0.88 is the sweet spot.
  // Chill: near-transparent safety ceiling (barely catches peaks) so
  // natural dynamics shine; non-chill keeps the working 0.88 limiter.
  (isChill
    ? "alimiter=limit=0.97:attack=8:release=80:level=disabled,"
    : "alimiter=limit=0.88:attack=5:release=50:level=disabled,") +
  // Loudness target — back to -15 LUFS (was -13 which was forcing
  // the limiter and acompressor to overdrive into pump). LRA=20 keeps
  // the new wider dynamic arc.
  loudStage;
const ff = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", STEREO ? "2" : "1",
  "-i", rawPath,
  "-filter_complex", filter,
  "-map", "[out]",
  // Hard-trim at exactly totalSec so the file boundary lines up with
  // bar 60 — the file loops cleanly to t=0 on the next downbeat.
  "-t", String(totalSec),
  ...(OUT_IS_WAV
    ? ["-c:a", "pcm_s16le", "-ar", "44100"]   // DistroKid release WAV
    : ["-c:a", "libmp3lame", "-q:a", "3"]),
  OUT_PATH,
], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }

// (AC stamp is now mixed into the buffer at ~13 s — earlier in the
// track as a branding ID. The shutdown chime is the absolute final
// sound. No post-encode stamp overlay needed.)
console.log(`✓ ${OUT_PATH}`);

// (AC stamp is mixed INTO the out buffer in the outro section so the
// final mp3 is exactly bar-60-aligned and loops cleanly. See the
// stamp mix-in step before the ffmpeg encode.)
