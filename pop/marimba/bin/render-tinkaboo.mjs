#!/usr/bin/env node
// render-marimbaba.mjs — render the marimbaba lullaby to mp3.
//
// Score notation lives in pop/marimba/marimbaba.np (the human-readable
// form). This script holds the same music as scheduled per-voice
// events. Inline because (a) there's no .np parser in the marimba
// lane yet, and (b) lullabies want per-voice velocity / timing nuance
// the syllabic .np grid doesn't carry.
//
// Voices:
//   rosewood       — the singer; syllabic melody
//   bass           — the rocking chair; low half-bar pulse
//   kalimba        — twinkles on phrase tails (off-beat sparkle)
//   vibraphone_off — held thirds for the "wow" / dream-haze pad
//
// Run:
//   node pop/marimba/bin/render-marimbaba.mjs
//   node pop/marimba/bin/render-marimbaba.mjs --out ~/marimbaba.mp3
//
// All timing in seconds. 56 BPM 3/4 → beat = 60/56 = 1.0714, bar = 3.2143.

import { mixEventMarimba } from "../synths/marimba.mjs";
import { readFileSync, existsSync, statSync } from "node:fs";

// ── SDT physical bubble (port of system/.../lib/sound/bubble.mjs) ─────
// Ported from the Sound Design Toolkit liquid model, the same one
// shipped as `sound.bubble()` in AC and used in the `bubble` disk. We
// re-implement here so (a) it runs offline at arbitrary sample rates,
// (b) we can render thousands of overlapping bubbles into a buffer
// instead of one-per-Worklet, (c) the math stays JS↔C portable in case
// we later want to bake it into fedac/native.
//
// Parameters (matching the AC class):
//   radiusMM — bubble radius in mm. 3 = tiny droplet (~1000 Hz),
//              30 = deep gloop (~100 Hz). Lower = higher pitched.
//   rise     — surface-tension upward pitch glide. 0 = static, 1 = strong,
//              4+ = boiling water. 0.1–0.8 is the gentle bath range.
//   volume   — 0..1 source level
//   pan      — -1..+1 stereo position
//
// The SDT trick: amplitude is divided by a running max so each bubble
// self-normalises to [-1, 1]. We keep that behavior for parity with AC.
function mixSDTBubble(L, R, startSec, radiusMM, rise, volume, pan, sr, depth = 1.0) {
  const startIdx = Math.floor(startSec * sr);
  const radius = radiusMM * 0.001;
  const timestep = 1 / sr;
  const pRadius = radius * Math.sqrt(radius);
  let amp = 17.2133 * pRadius * depth;
  const decay = 0.13 / radius + 0.0072 * pRadius;
  const gain = Math.exp(-decay * timestep);
  let phaseStep = (3.0 / radius) * timestep;
  const phaseRise = phaseStep * decay * rise * timestep;
  let phase = 0;
  let lastOut = 0;
  let maxOut = 1;
  const QUIET = 0.000001;
  // equal-power pan
  const angle = (pan * 0.5 + 0.5) * (Math.PI / 2);
  const gL = Math.cos(angle), gR = Math.sin(angle);
  // Generous max render time so bigger (slower-decaying) bubbles ring out.
  const maxSamples = Math.floor(4.0 * sr);
  for (let i = 0; i < maxSamples; i++) {
    if (amp < QUIET && phase > 1.0) break;
    // Clamp alpha to [0, 1] — the original AC model assumes phase only
    // grows from 0 upward, so a negative-rise bubble (where phaseStep
    // eventually goes negative and phase reverses) breaks the (1-α)·last
    // smoothing into an explosive feedback. Clamping fixes that without
    // changing positive-rise behavior at all.
    const alpha = phase < 0 ? 0 : (phase < 1.0 ? phase : 1.0);
    // Bubble tone = fundamental + 2nd + 3rd harmonic. The SDT model is
    // a pure sine; adding overtones gives the bubbles a rounder, more
    // "vocal/woody" timbre that blends with the marimba bars instead of
    // sounding like bare test tones. Normalised so peak stays ≈ 1.
    const ph = Math.PI * 2 * phase;
    const rich = (Math.sin(ph)
                + 0.34 * Math.sin(2 * ph)
                + 0.17 * Math.sin(3 * ph)) / 1.51;
    const out = (1.0 - alpha) * lastOut + alpha * amp * rich;
    lastOut = out;
    phase += phaseStep;
    phaseStep += phaseRise;
    amp *= gain;
    let v = out * volume * 1000;
    if (Math.abs(v) > maxOut) maxOut = Math.abs(v);
    v = v / maxOut;
    const dst = startIdx + i;
    if (dst < 0 || dst >= L.length) continue;
    L[dst] += v * gL;
    R[dst] += v * gR;
  }
}

// ── sample loader ─────────────────────────────────────────────────────
// Decode an mp3/wav/whatever via ffmpeg to f32le stereo at SR, return
// {L,R} arrays. Returns null if the file is missing — caller prints a
// helpful hint and skips the layer rather than failing the render.
function loadSampleStereo(path, sr) {
  if (!existsSync(path)) return null;
  const tmpPath = path + ".decode.raw";
  const r = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", path, "-f", "f32le", "-ar", String(sr), "-ac", "2",
    tmpPath,
  ]);
  if (r.status !== 0 || !existsSync(tmpPath)) {
    console.warn(`     ! decode failed for ${path}`);
    return null;
  }
  const buf = readFileSync(tmpPath);
  const interleaved = new Float32Array(buf.buffer, buf.byteOffset, buf.byteLength / 4);
  const n = interleaved.length / 2;
  const sL = new Float32Array(n);
  const sR = new Float32Array(n);
  for (let i = 0; i < n; i++) { sL[i] = interleaved[i * 2]; sR[i] = interleaved[i * 2 + 1]; }
  try { unlinkSync(tmpPath); } catch {}
  return { L: sL, R: sR };
}

function mixSampleStereoPanned(L, R, sample, startSec, gain, pan, sr) {
  if (!sample) return;
  // Equal-power pan; if pan is exactly 0 we want unity passthrough so
  // pre-stereo samples keep their L/R differentiation.
  const angle = (pan * 0.5 + 0.5) * (Math.PI / 2);
  const gL = pan === 0 ? 1 : Math.cos(angle) * Math.SQRT2;
  const gR = pan === 0 ? 1 : Math.sin(angle) * Math.SQRT2;
  const startIdx = Math.floor(startSec * sr);
  for (let i = 0; i < sample.L.length; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= L.length) continue;
    L[dst] += sample.L[i] * gain * gL;
    R[dst] += sample.R[i] * gain * gR;
  }
}

// Auto-pan a stereo bus — a slow LFO sweeps the image L↔R while a
// second, slower LFO does a gentle volume swell. Decorrelated phase
// offsets per bus keep the stereo field constantly, musically moving.
function autoPanVol(L, R, panRateHz, panDepth, volRateHz, volDepth, ph0, sr) {
  const dt = 1 / sr;
  let pPh = ph0, vPh = ph0 * 0.37;
  for (let i = 0; i < L.length; i++) {
    pPh += panRateHz * dt; if (pPh >= 1) pPh -= 1;
    vPh += volRateHz * dt; if (vPh >= 1) vPh -= 1;
    const pan = Math.sin(2 * Math.PI * pPh) * panDepth;     // −depth..depth
    const vol = 1 + Math.sin(2 * Math.PI * vPh) * volDepth;
    let gL = vol, gR = vol;
    if (pan > 0) gL *= (1 - pan);          // balance law
    else         gR *= (1 + pan);
    L[i] *= gL; R[i] *= gR;
  }
}

// ── synthesised 2026 NYC police siren ─────────────────────────────────
// NYPD siren: a "wail" (high-low alternation, ~800 Hz ↔ 1500 Hz, ~0.7 Hz
// rate, with a non-sinusoidal slewed contour — closer to triangle than
// sine), layered with the "Rumbler" — a brief sub-bass burst (30–80 Hz)
// that's added periodically for low-frequency cut-through. Placeholder
// until a freesound sample lands in assets/manhattan-siren.mp3.
function mixSiren(L, R, startSec, durSec, gain, pan, sr) {
  const startIdx = Math.floor(startSec * sr);
  const n = Math.floor(durSec * sr);
  const dt = 1 / sr;
  const angle = (pan * 0.5 + 0.5) * (Math.PI / 2);
  const gL = Math.cos(angle), gR = Math.sin(angle);
  let phWail = 0, phRumble = 0, phSweep = 0;
  for (let i = 0; i < n; i++) {
    const t = i * dt;
    // gentle attack/release envelope so it doesn't pop
    const attRel = Math.min(t / 0.5, (durSec - t) / 0.8);
    const env = Math.max(0, Math.min(1, attRel));
    // 0.7 Hz triangle wave for the high-low rate (sharper than sin)
    phSweep += 0.7 * dt; if (phSweep >= 1) phSweep -= 1;
    const tri = phSweep < 0.5 ? (phSweep * 2) : (2 - phSweep * 2); // 0..1 triangle
    const wailFreq = 800 + (1500 - 800) * tri;
    phWail += wailFreq * dt; if (phWail >= 1) phWail -= 1;
    // sawtooth-ish wail (real sirens are sawtooth horn drivers)
    const wail = (2 * phWail - 1) * 0.55;
    // rumbler: low burst every ~2.5s for 0.6s
    const rumbleCycle = (t % 2.5) / 2.5;
    const rumbleEnv = rumbleCycle < 0.24 ? Math.sin(rumbleCycle * 4.17 * Math.PI) : 0;
    phRumble += 55 * dt; if (phRumble >= 1) phRumble -= 1;
    const rumble = Math.sin(2 * Math.PI * phRumble) * rumbleEnv * 0.85;
    const sample = (wail + rumble) * env * gain;
    const dst = startIdx + i;
    if (dst < 0 || dst >= L.length) continue;
    L[dst] += sample * gL;
    R[dst] += sample * gR;
  }
}

// ── connecting sine ───────────────────────────────────────────────────
// Pure sine voice that bridges the silent tails of marimba notes. The
// marimba bar decays exponentially; by the second half of a held *3
// the bar is essentially gone and the texture goes silent. A faint sine
// gliding between target pitches keeps the air alive without competing
// with the mallet attacks.
//
// Each connector is a pitch trajectory — an array of [tSec, midi]
// breakpoints relative to its start — rendered as one continuous sine
// with linear-interpolated frequency and a raised-cosine envelope.
function midiToFreqF(midi) { return 440 * Math.pow(2, (midi - 69) / 12); }
// ── Schroeder reverb ──────────────────────────────────────────────────
// Classic 4-comb-then-2-allpass topology (Schroeder 1962). Cheap, sounds
// natural enough for ambient beds, applied as a wet send on the master
// bus AFTER all voices and connectors are mixed. Delays are prime-ish in
// samples to avoid resonant peaks; gains derived from target decay so
// "decay = 1.5" means roughly a 1.5s reverb tail at 48k.
function applyReverb(buffer, sampleRate, wet, decay) {
  // Schroeder's original sample-delay set, scaled from 44.1k → 48k.
  const combD = [1996, 1894, 2429, 2664];
  const apD = [411, 134];
  const combG = combD.map(d => Math.pow(10, -3 * d / (decay * sampleRate)));
  const apG = 0.7;
  const combBuf = combD.map(d => new Float32Array(d));
  const combIdx = combD.map(() => 0);
  const apBuf = apD.map(d => new Float32Array(d));
  const apIdx = apD.map(() => 0);
  // Compute the wet path into a side buffer, then mix it back. Keeping
  // wet separate lets us scale the dry path independently for the
  // "more wet but not louder" feel that lullabies want.
  const wetBuf = new Float32Array(buffer.length);
  for (let i = 0; i < buffer.length; i++) {
    const inp = buffer[i];
    let combOut = 0;
    for (let c = 0; c < combD.length; c++) {
      const d = combBuf[c][combIdx[c]];
      combOut += d;
      combBuf[c][combIdx[c]] = inp + d * combG[c];
      combIdx[c] = (combIdx[c] + 1) % combD[c];
    }
    combOut /= combD.length;
    let ap = combOut;
    for (let a = 0; a < apD.length; a++) {
      const d = apBuf[a][apIdx[a]];
      const v = ap - apG * d;
      ap = apG * v + d;
      apBuf[a][apIdx[a]] = v;
      apIdx[a] = (apIdx[a] + 1) % apD[a];
    }
    wetBuf[i] = ap;
  }
  for (let i = 0; i < buffer.length; i++) {
    buffer[i] = buffer[i] * (1 - wet * 0.4) + wetBuf[i] * wet;
  }
}

// ── stereo cascaded-allpass phaser ────────────────────────────────────
// Eno-bed sound. N one-pole all-pass filters in series, each with its
// center frequency modulated by a slow LFO. Allpass filters do not
// change amplitude — only phase — so the mallet attacks pass through
// undisturbed; the "phasiness" comes from the moving spectral notches
// where the all-passed signal is summed back with the dry. L and R
// share the same LFO with a small phase offset so the bed swirls
// across the stereo field instead of pulsing mono.
function applyPhaserStereo(L, R, sampleRate, rateHz, minFreq, maxFreq, stages, wet) {
  // per-channel allpass state (one z^-1 per stage per channel)
  const zL = new Float64Array(stages);
  const zR = new Float64Array(stages);
  // small per-stage center offset so the notches stagger across freq
  const stageMul = [];
  for (let s = 0; s < stages; s++) stageMul.push(1 + s * 0.35);
  const dt = 1 / sampleRate;
  let lfoPhaseL = 0, lfoPhaseR = 0.27;   // 0.27 cycle L↔R offset
  const logMin = Math.log(minFreq), logMax = Math.log(maxFreq);
  for (let i = 0; i < L.length; i++) {
    lfoPhaseL += rateHz * dt; if (lfoPhaseL >= 1) lfoPhaseL -= 1;
    lfoPhaseR += rateHz * dt; if (lfoPhaseR >= 1) lfoPhaseR -= 1;
    const lfoL = 0.5 * (1 + Math.sin(2 * Math.PI * lfoPhaseL));
    const lfoR = 0.5 * (1 + Math.sin(2 * Math.PI * lfoPhaseR));
    const fL = Math.exp(logMin + (logMax - logMin) * lfoL);
    const fR = Math.exp(logMin + (logMax - logMin) * lfoR);
    let xL = L[i], xR = R[i];
    for (let s = 0; s < stages; s++) {
      const fc = stageMul[s];
      // 1-pole allpass coefficient: a = (1 - tan(πf/SR)) / (1 + tan(πf/SR))
      const tL = Math.tan(Math.PI * Math.min(fL * fc, sampleRate * 0.45) / sampleRate);
      const tR = Math.tan(Math.PI * Math.min(fR * fc, sampleRate * 0.45) / sampleRate);
      const aL = (1 - tL) / (1 + tL);
      const aR = (1 - tR) / (1 + tR);
      const yL = aL * xL + zL[s];
      const yR = aR * xR + zR[s];
      zL[s] = xL - aL * yL;
      zR[s] = xR - aR * yR;
      xL = yL; xR = yR;
    }
    L[i] = L[i] * (1 - wet * 0.5) + xL * wet;
    R[i] = R[i] * (1 - wet * 0.5) + xR * wet;
  }
}

// seeded RNG so bubble layout is deterministic across re-renders
function makeRng(seed) {
  let s = seed >>> 0 || 1;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}

function mixSineConnector(out, startSec, durSec, points, gain, sampleRate) {
  const startIdx = Math.floor(startSec * sampleRate);
  const n = Math.floor(durSec * sampleRate);
  const dt = 1 / sampleRate;
  let phase = 0;
  // raised-cosine envelope over the full duration so it fades in AND out
  for (let i = 0; i < n; i++) {
    const t = i * dt;
    // find the segment
    let f;
    if (points.length === 1) {
      f = midiToFreqF(points[0][1]);
    } else {
      let s = 0;
      while (s < points.length - 2 && points[s + 1][0] <= t) s++;
      const [t0, m0] = points[s];
      const [t1, m1] = points[s + 1];
      const u = Math.max(0, Math.min(1, (t - t0) / Math.max(1e-6, t1 - t0)));
      const m = m0 + (m1 - m0) * u;
      f = midiToFreqF(m);
    }
    phase += f * dt;
    if (phase >= 1) phase -= Math.floor(phase);
    // raised-cosine envelope: zero at edges, 1 at the middle
    const u = i / Math.max(1, n - 1);
    const env = 0.5 * (1 - Math.cos(2 * Math.PI * u));
    const sample = Math.sin(2 * Math.PI * phase) * env * gain;
    const dst = startIdx + i;
    if (dst >= 0 && dst < out.length) out[dst] += sample;
  }
}
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
// --bpm <n> overrides the tempo (variations render at different rates);
// --lead <preset> remaps the marimba lead voice to another tuned-perc
// preset (orchestration variations). Defaults = the marimbaba master.
const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 56;
const LEAD_PRESET = _argi("--lead") || "rosewood";
const BEAT = 60 / BPM;            // beat length (s)
const BAR  = 3 * BEAT;            // 3/4 time

// ── note helpers ──────────────────────────────────────────────────────
// Standard MIDI: A4 = 69 = 440 Hz, C4 = 60.
// tinkaboo — G major. G A B C D E F#. Bright music-box register.
const N = {
  G2: 43, A2: 45, B2: 47, D3: 50, G3: 55, B3: 59,
  C4: 60, D4: 62, E4: 64, Fs4: 66, G4: 67, A4: 69, B4: 71,
  C5: 72, D5: 74, E5: 76, Fs5: 78, G5: 79, A5: 81, B5: 83,
  C6: 84, D6: 86, E6: 88, Fs6: 90, G6: 91,
  // legacy aliases kept so any stray reference still resolves
  F2: 43, F3: 55, F4: 67, F5: 79, F6: 91, Bb4: 71, Bb5: 83,
};

// Per-voice decay stretch — pulls T60 longer than the physically-
// accurate preset values for a dreamier, more-connected lullaby ring.
// rosewood mode 1 is 1.6s by default → 2.9s here; kalimba 1.8s → 3.2s;
// bass 2.4s → 4.3s. The connecting sines + vibraphone pad still fill
// any remaining seam.
const DECAY = {
  rosewood: 1.8,
  bass: 1.8,
  kalimba: 1.75,
  vibraphone: 1.4,
  vibraphone_off: 1.4,
};

function v(voice, bar, beat, midi, beats, gain = 0.85) {
  return {
    preset: voice,
    startSec: bar * BAR + beat * BEAT,
    midi,
    durSec: beats * BEAT,
    gain,
    decayMul: DECAY[voice] ?? 1.0,
  };
}

// ── the lullaby — same shape as marimbaba.np ───────────────────────────
// Bars 0-indexed. (bar, beat-within-bar, midi, beat-duration, gain)
const events = [];

// ── [hush hush] — bars 0..3 — wind-up climb ──────────────────────────
// rosewood: G4 → B4 → D5 → (held) → C5 → D5 → B4 (held) — perky climb
events.push(v("rosewood", 0, 0, N.G4, 1.0, 0.55));
events.push(v("rosewood", 0, 1, N.B4, 1.0, 0.55));
events.push(v("rosewood", 0, 2, N.D5, 1.0, 0.55));
events.push(v("rosewood", 1, 0, N.G4, 3.0, 0.45));
events.push(v("rosewood", 2, 0, N.C5, 1.5, 0.55));
events.push(v("rosewood", 2, 1.5, N.D5, 1.5, 0.55));
events.push(v("rosewood", 3, 0, N.B4, 3.0, 0.55));

// bass: gentle low rocker on beat 1 every bar — G root
for (let b = 0; b < 4; b++) {
  events.push(v("bass", b, 0, N.G2, 3.0, 0.55));
}

// ── [twinkle] — bars 4..9 — bouncing music-box runs ──────────────────
// bars 4-6 over C major, bars 7-9 over D major
events.push(v("rosewood", 4, 0, N.E5, 1.0, 0.60));
events.push(v("rosewood", 4, 1, N.G5, 1.0, 0.60));
events.push(v("rosewood", 4, 2.0, N.C6, 0.5, 0.65));
events.push(v("rosewood", 4, 2.5, N.G5, 0.5, 0.55));
events.push(v("rosewood", 5, 0, N.E5, 3.0, 0.55));
events.push(v("rosewood", 6, 0, N.C5, 1.0, 0.55));
events.push(v("rosewood", 6, 1, N.E5, 1.0, 0.55));
events.push(v("rosewood", 6, 2.0, N.G5, 0.5, 0.60));
events.push(v("rosewood", 6, 2.5, N.E5, 0.5, 0.50));
events.push(v("rosewood", 7, 0, N.D5, 3.0, 0.55));
events.push(v("rosewood", 8, 0, N.Fs5, 1.0, 0.60));
events.push(v("rosewood", 8, 1, N.A5, 1.0, 0.65));
events.push(v("rosewood", 8, 2, N.D6, 1.0, 0.55));
events.push(v("rosewood", 9, 0, N.B5, 1.5, 0.60));
events.push(v("rosewood", 9, 1.5, N.A5, 1.5, 0.55));

// bass pulse — C-then-D alternating roots, gives the bounce
const twinkleBass = [N.C4 - 12, N.C4, N.C4 - 12, N.D3, N.D3 - 12, N.D3];
for (let i = 0; i < 6; i++) {
  events.push(v("bass", 4 + i, 0, twinkleBass[i], 3.0, 0.45));
}

// vibraphone_off pad — C major sustained over bars 4-6, D major over 7-9
events.push({ preset: "vibraphone_off", startSec: 4 * BAR, midi: N.C4, durSec: 3 * BAR, gain: 0.18 });
events.push({ preset: "vibraphone_off", startSec: 4 * BAR, midi: N.E4, durSec: 3 * BAR, gain: 0.18 });
events.push({ preset: "vibraphone_off", startSec: 4 * BAR, midi: N.G4, durSec: 3 * BAR, gain: 0.18 });
events.push({ preset: "vibraphone_off", startSec: 7 * BAR, midi: N.D4, durSec: 3 * BAR, gain: 0.18 });
events.push({ preset: "vibraphone_off", startSec: 7 * BAR, midi: N.Fs4, durSec: 3 * BAR, gain: 0.18 });
events.push({ preset: "vibraphone_off", startSec: 7 * BAR, midi: N.A4, durSec: 3 * BAR, gain: 0.18 });

// ── [wow wow wow] — bars 10..13 — held wobble (vibraphone takes over) ─
// vibraphone WITH motor for the "wow" wobble feel — G major
events.push({ preset: "vibraphone", startSec: 10 * BAR, midi: N.B5, durSec: 2 * BAR, gain: 0.45 });
events.push({ preset: "vibraphone", startSec: 10 * BAR, midi: N.D6, durSec: 2 * BAR, gain: 0.40 });
events.push(v("rosewood", 12, 0, N.B5, 1.0, 0.55));
events.push(v("rosewood", 12, 1, N.D6, 1.0, 0.55));
events.push(v("rosewood", 12, 2, N.B5, 1.0, 0.55));
events.push(v("rosewood", 13, 0, N.G5, 3.0, 0.55));

// kalimba sparkles overlaid on the wow — soft, off-beat ornaments
events.push(v("kalimba", 10, 2.5, N.D6, 0.5, 0.30));
events.push(v("kalimba", 11, 1.0, N.G6, 0.5, 0.25));
events.push(v("kalimba", 12, 2.5, N.D6, 0.5, 0.30));
events.push(v("kalimba", 13, 1.5, N.B5, 0.5, 0.25));

// quiet bass under the wow
events.push(v("bass", 10, 0, N.G2, 3.0, 0.35));
events.push(v("bass", 12, 0, N.G2, 3.0, 0.35));

// ratatatata — a fast staccato 16th-note tumble across bar 11, a
// playful machine-gun burst against the slow held vibraphone.
const wowRatata = [N.B5, N.A5, N.B5, N.A5, N.G5, N.A5, N.B5, N.D6];
for (let i = 0; i < wowRatata.length; i++) {
  events.push(v("staccato", 11, i * 0.1875, wowRatata[i], 0.22, 0.34));
}

// ── [ba-ba-ba bap] — bars 14..17 — bouncy skip-hop ───────────────────
events.push(v("rosewood", 14, 0, N.B5, 0.5, 0.60));
events.push(v("rosewood", 14, 0.5, N.A5, 0.5, 0.55));
events.push(v("rosewood", 14, 1.0, N.B5, 1.0, 0.60));
events.push(v("rosewood", 14, 2.0, N.D6, 1.0, 0.65));
events.push(v("rosewood", 15, 0, N.C6, 0.5, 0.60));
events.push(v("rosewood", 15, 0.5, N.B5, 0.5, 0.55));
events.push(v("rosewood", 15, 1.0, N.C6, 1.0, 0.60));
events.push(v("rosewood", 15, 2.0, N.E6, 1.0, 0.65));
events.push(v("rosewood", 16, 0, N.D6, 1.0, 0.55));
events.push(v("rosewood", 16, 1, N.C6, 1.0, 0.55));
events.push(v("rosewood", 16, 2, N.A5, 1.0, 0.50));
events.push(v("rosewood", 17, 0, N.D6, 3.0, 0.55));

// kalimba doubles the "bap" on bars 14, 15 (Octave up sparkle)
events.push(v("kalimba", 14, 2, N.G6, 1.0, 0.28));
events.push(v("kalimba", 15, 2, N.B5, 1.0, 0.28));

// bass: walk — G, C, D, D
const babaBass = [N.G2, N.C4 - 12, N.D3, N.D3];
for (let i = 0; i < 4; i++) {
  events.push(v("bass", 14 + i, 0, babaBass[i], 3.0, 0.45));
}

// ── fast marimba 16ths — section-closing flourishes ──────────────────
// 16th-note runs (kelon preset — bright, crisp) tumbling through the
// last bar of three sections. At 56 BPM 3/4 a 16th = BEAT/4 ≈ 0.27 s.
function run16(bar, startBeat, notes, gain) {
  for (let i = 0; i < notes.length; i++) {
    events.push(v("kelon", bar, startBeat + i * 0.25, notes[i], 0.25, gain));
  }
}
// bar 9 — bright G-major scale 16ths closing the twinkle section
run16(9, 0, [N.D6, N.B5, N.A5, N.G5, N.Fs5, N.E5,
             N.D5, N.E5, N.Fs5, N.G5, N.A5, N.B5], 0.30);
// bar 13 — ascending 16ths spilling out of the wow
run16(13, 0, [N.G5, N.A5, N.B5, N.D6, N.E6, N.D6,
              N.B5, N.A5, N.G5, N.A5, N.B5, N.D6], 0.28);
// bar 21 — fast 16ths winding up into the ending cadence
run16(21, 0, [N.B5, N.A5, N.G5, N.Fs5, N.E5, N.D5,
              N.B4, N.D5, N.G5, N.B5, N.D6, N.G6], 0.26);

// ── [sleep now] — bars 18..23 — bouncy settle ────────────────────────
events.push(v("rosewood", 18, 0, N.B4, 1.5, 0.45));
events.push(v("rosewood", 18, 1.5, N.D5, 1.5, 0.40));
events.push(v("rosewood", 19, 0, N.G5, 1.0, 0.40));
events.push(v("rosewood", 19, 1, N.Fs5, 1.0, 0.40));
events.push(v("rosewood", 19, 2, N.G5, 1.0, 0.35));
events.push(v("rosewood", 20, 0, N.D5, 1.0, 0.40));
events.push(v("rosewood", 20, 1, N.E5, 1.0, 0.35));
events.push(v("rosewood", 20, 2, N.G5, 1.0, 0.35));
events.push(v("rosewood", 21, 0, N.D5, 3.0, 0.45));

// kalimba — one last twinkle, then silence
events.push(v("kalimba", 19, 2.5, N.D6, 0.5, 0.18));
events.push(v("kalimba", 21, 1.0, N.B5, 0.5, 0.15));

// soft bass through the settle — G root
events.push(v("bass", 18, 0, N.G2, 3.0, 0.35));
events.push(v("bass", 20, 0, N.G2, 3.0, 0.35));

// ── [ending] — bars 22..25 — composed cadence ────────────────────────
// bar 22-23: a soft echo of the opening "hush" descending motif, an
// octave grouping that lands the piece back on its first idea.
// bar 23 beat 1.5: a fast "ratatatata" 16th-note ascending flourish
// that scoops up into the final chord.
// bar 24-25: the final rolled F major chord, full register, ringing
// out under the master fade.
events.push(v("rosewood", 22, 0,   N.G4, 1.0, 0.45));
events.push(v("rosewood", 22, 1,   N.B4, 1.0, 0.45));
events.push(v("rosewood", 22, 2,   N.D5, 1.0, 0.45));
events.push(v("rosewood", 23, 0,   N.G5, 1.5, 0.42));
// ratatatata — fast staccato 16th run scooping up to the cadence
const flourish = [N.G4, N.B4, N.D5, N.G5, N.B5, N.D6, N.E6, N.G6];
for (let i = 0; i < flourish.length; i++) {
  events.push(v("staccato", 23, 1.5 + i * 0.1875, flourish[i], 0.22, 0.40));
}
// bar 24-25: final rolled G major chord — staggered entries 30ms apart
const finalChord = [N.G2, N.G3, N.B3, N.D4, N.G4, N.B4, N.D5, N.G5];
for (let i = 0; i < finalChord.length; i++) {
  events.push({
    preset: i === 0 ? "bass" : "rosewood",
    startSec: 24 * BAR + i * 0.030,
    midi: finalChord[i],
    durSec: 2 * BAR,
    gain: 0.46 - i * 0.03,
    decayMul: 2.2,
  });
}
// vibraphone_off halo under the final chord — G major
for (const m of [N.G3, N.B3, N.D4, N.G4]) {
  events.push({ preset: "vibraphone_off", startSec: 24 * BAR, midi: m,
                durSec: 2 * BAR, gain: 0.16, decayMul: 1.8 });
}
events.push(v("bass", 22, 0, N.G2, 3.0, 0.34));

// ── continuous pad ────────────────────────────────────────────────────
// vibraphone_off chord under the whole piece, following local harmony.
// Voicings are stacked in the mid register (F4–F5) so they sit under
// the rosewood melody without crowding the bass. Some bars already
// have a pad voicing from before — we don't double those.
//
// chord plan, by bar range:
//   0..3   F major   (F-A-C)  [new]
//   4..6   F major          (already present)
//   7..9   Bb major         (already present)
//   10..13 G minor          [new, sits under the motor-on vibraphone]
//   14     F major   [new]
//   15     C major   (C-E-G) [new]
//   16..17 F major   [new]
//   18..21 F major   [new]
//   22..24 F major          (already present)
const PAD_GAIN = 0.13;
function pushPad(bar, beats, midis) {
  for (const m of midis) {
    events.push({
      preset: "vibraphone_off",
      startSec: bar * BAR,
      midi: m,
      durSec: beats * BEAT,
      gain: PAD_GAIN,
      decayMul: 1.6,
    });
  }
}
// fill the gaps that the per-section pad calls left empty
pushPad(0, 4,    [N.G4, N.B4, N.D5]);              // bars 0..3   G
pushPad(10, 4,   [N.G4, N.B4, N.D5]);              // bars 10..13 G
pushPad(14, 1,   [N.G4, N.B4, N.D5]);              // bar 14      G
pushPad(15, 1,   [N.E4, N.G4, N.C5]);              // bar 15      C
pushPad(16, 2,   [N.Fs4, N.A4, N.D5]);             // bars 16..17 D
pushPad(18, 4,   [N.G4, N.B4, N.D5]);              // bars 18..21 G

// ── music-box ostinato ────────────────────────────────────────────────
// Sparse kalimba pattern outlining the local chord — one note per beat
// in 3/4 (root–fifth–third), low-gain enough to register as twinkles
// rather than a fourth voice. Inspired by Brahms-lullaby left hand +
// the music-box-rocking we want around the rosewood phrases.
// Skipped on bars 0–1 (intro should be sparse) and bars 22–24
// (goodnight should be settling).
const OSTI_GAIN = 0.10;
function pushOsti(bar, chord) {
  // chord = [root, third, fifth] in MIDI; play root(beat0) fifth(beat1) third(beat2)
  events.push(v("kalimba", bar, 0,   chord[0], 0.9, OSTI_GAIN));
  events.push(v("kalimba", bar, 1,   chord[2], 0.9, OSTI_GAIN * 0.85));
  events.push(v("kalimba", bar, 2,   chord[1], 0.9, OSTI_GAIN * 0.9));
}
pushOsti(2,  [N.G4, N.B4, N.D5]);    // hush — G
pushOsti(3,  [N.G4, N.B4, N.D5]);
pushOsti(4,  [N.C4, N.E4, N.G4]);    // twinkle A — C
pushOsti(5,  [N.C4, N.E4, N.G4]);
pushOsti(6,  [N.C4, N.E4, N.G4]);
pushOsti(7,  [N.D4, N.Fs4, N.A4]);   // twinkle B — D
pushOsti(8,  [N.D4, N.Fs4, N.A4]);
pushOsti(9,  [N.D4, N.Fs4, N.A4]);
// bars 10–13 — leave the wow section spacious, no ostinato
pushOsti(14, [N.G4, N.B4, N.D5]);    // baba — G
pushOsti(15, [N.E4, N.G4, N.C5]);    // C
pushOsti(16, [N.Fs4, N.A4, N.D5]);   // D
pushOsti(17, [N.Fs4, N.A4, N.D5]);
pushOsti(18, [N.G4, N.B4, N.D5]);    // sleep — G
pushOsti(19, [N.G4, N.B4, N.D5]);
// bars 20–21 fade out the ostinato gradually
events.push(v("kalimba", 20, 0, N.G4, 0.9, OSTI_GAIN * 0.6));
events.push(v("kalimba", 20, 2, N.B4, 0.9, OSTI_GAIN * 0.45));
events.push(v("kalimba", 21, 1, N.G4, 0.9, OSTI_GAIN * 0.3));

// Soft pure-sine glides that bridge the held-note tails and section
// seams. Trajectories are written in (tSec-from-start, midi) pairs.
// Gains stay around 0.10–0.16 so they sit under the rosewood without
// becoming an additional voice.
const SINE_GAIN = 0.13;
const connectors = [
  // bar 1 hush tail: hum G4 → B4 → G4 (gentle breath shape)
  { startBar: 1, startBeat: 0.5, durBeats: 2.5, gain: SINE_GAIN,
    points: [[0.0, N.G4], [1.2, N.B4], [2.5, N.G4]] },
  // bar 3 hush tail: B4 sustained, drifting up a tiny bit then back
  { startBar: 3, startBeat: 0.5, durBeats: 2.5, gain: SINE_GAIN,
    points: [[0.0, N.B4], [1.5, N.D5], [2.5, N.B4]] },
  // bar 5 twinkle tail: E5 → G5 → E5 (matches the wave shape above)
  { startBar: 5, startBeat: 0.5, durBeats: 2.5, gain: SINE_GAIN * 0.85,
    points: [[0.0, N.E5], [1.2, N.G5], [2.5, N.E5]] },
  // bar 7 twinkle tail: D5 lifting briefly to Fs5
  { startBar: 7, startBeat: 0.5, durBeats: 2.5, gain: SINE_GAIN * 0.85,
    points: [[0.0, N.D5], [1.3, N.Fs5], [2.5, N.D5]] },
  // section seam — between [twinkle] and [wow], glide A5 → B5 (where
  // the vibraphone takes over) across the bar 9→10 transition
  { startBar: 9, startBeat: 1.5, durBeats: 2.5, gain: SINE_GAIN * 0.9,
    points: [[0.0, N.A5], [2.5, N.B5]] },
  // bar 13 wow tail: G5 hum settling
  { startBar: 13, startBeat: 0.5, durBeats: 2.5, gain: SINE_GAIN * 0.9,
    points: [[0.0, N.G5], [1.5, N.A5], [2.5, N.G5]] },
  // bar 17 baba tail: D6 → C6 → D6 (matches the bap resolution)
  { startBar: 17, startBeat: 0.5, durBeats: 2.5, gain: SINE_GAIN,
    points: [[0.0, N.D6], [1.5, N.C6], [2.5, N.D6]] },
  // bar 21 sleep tail: D5 humming
  { startBar: 21, startBeat: 0.5, durBeats: 2.5, gain: SINE_GAIN,
    points: [[0.0, N.D5], [1.0, N.B4], [2.5, N.D5]] },
  // bars 22–25 — the goodnight, a drifting low G drone under the
  // composed cadence, pulling the room tone to the final chord.
  { startBar: 22, startBeat: 0, durBeats: 12.0, gain: SINE_GAIN * 1.1,
    points: [[0.0, N.G3], [4.0, N.G4], [8.0, N.G3], [12.0, N.G3]] },
];

// ── mix ────────────────────────────────────────────────────────────────
// Piece is 24 bars of marimba + an extended ~10 bar bubble-bass tail.
// The marimba goes to sleep at bar 24; the bubble bass beat keeps the
// bed going for another half-minute before fading out entirely.
const TOTAL_BARS = 26;   // ≈ 83.6 s (1:24) — ends on a composed cadence
const totalSec = TOTAL_BARS * BAR;
const ns = Math.ceil(totalSec * SR);

// First pass — mono: marimba + sine connectors. The marimba+connectors
// stay mono and centered; only the new sineflower bed actually moves.
const monoMix = new Float32Array(ns);
for (const ev of events) {
  // orchestration variation — remap the rosewood lead to --lead preset
  const preset = ev.preset === "rosewood" ? LEAD_PRESET : ev.preset;
  const decayMul = ev.decayMul ?? DECAY[preset] ?? DECAY[ev.preset] ?? 1.0;
  mixEventMarimba(ev, monoMix, { sampleRate: SR, preset, decayMul });
}
for (const c of connectors) {
  const startSec = c.startBar * BAR + c.startBeat * BEAT;
  const durSec = c.durBeats * BEAT;
  mixSineConnector(monoMix, startSec, durSec, c.points, c.gain, SR);
}
// reverb send on the mono mix (mallet+connector wash). Wet=0.35, tail=1.8s.
applyReverb(monoMix, SR, 0.35, 1.8);

// ── sub-buses ─────────────────────────────────────────────────────────
// Render each element to its own bus, then normalise each to a known
// peak, then sum at explicit relative levels. Without this every layer
// got crushed by the loudest peak in the master normalise; the bubble
// chord stacks dominated and the marimba ended up 10-15 dB too quiet.
// Now every element is heard at a chosen relative level.
const marimbaL = new Float32Array(ns);
const marimbaR = new Float32Array(ns);
for (let i = 0; i < ns; i++) { marimbaL[i] = monoMix[i]; marimbaR[i] = monoMix[i]; }

const bubbleL = new Float32Array(ns);
const bubbleR = new Float32Array(ns);

const voiceL = new Float32Array(ns);
const voiceR = new Float32Array(ns);

const sirenL = new Float32Array(ns);
const sirenR = new Float32Array(ns);

function normalizeBus(L, R) {
  let p = 0;
  for (let i = 0; i < L.length; i++) {
    const a = Math.abs(L[i]); if (a > p) p = a;
    const b = Math.abs(R[i]); if (b > p) p = b;
  }
  if (p === 0) return;
  const g = 1 / p;
  for (let i = 0; i < L.length; i++) { L[i] *= g; R[i] *= g; }
}

// ── sineflower bubble BASS BEAT (SDT physical model) ──────────────────
// Beat-quantized, chord-tuned bubble bass. Bubble radius is computed
// from a target MIDI pitch via the SDT relation f ≈ 3000 / r_mm (Hz);
// each beat fires a chord stack (root + fifth + octave) so the bubbles
// land like a synth bass on the downbeat. Weirdness sprinkles —
// hyper-rises, negative rises (pitch DROPS), tiny droplets, deep
// thunders — get layered in at low probability per beat.
//
// Chord progression follows the marimba harmony bar-by-bar. After the
// marimba sleeps at bar 24 the bubble bass keeps going for a tail,
// gradually fading out into silence.
const rng = makeRng(0xb00b1e5);   // seeded — reruns are bit-identical

function midiToFreqB(m) { return 440 * Math.pow(2, (m - 69) / 12); }
function midiToRadiusMM(m) { return 3000 / midiToFreqB(m); }

// chord per bar — three voicings: deep root (low), bass-fifth (mid),
// upper-octave (tinkle), and the third (chord colour). MIDI numbers
// chosen low so the bubble radii land in the bass-beat range.
const CHORDS = new Array(TOTAL_BARS);
function setChord(barFrom, barToInclusive, root, fifth, octave, third) {
  for (let b = barFrom; b <= barToInclusive; b++) {
    CHORDS[b] = { root, fifth, octave, third };
  }
}
setChord(0, 6,    31, 38, 43, 47);   // G1 D2 G2 B2 — G major
setChord(7, 9,    38, 45, 50, 54);   // D2 A2 D3 Fs3 — D major
setChord(10, 13,  31, 38, 43, 47);   // G1 D2 G2 B2 — G major
setChord(14, 14,  31, 38, 43, 47);   // G
setChord(15, 15,  36, 43, 48, 52);   // C2 G2 C3 E3 — C major
setChord(16, 17,  38, 45, 50, 54);   // D2 A2 D3 Fs3 — D major
setChord(18, 21,  31, 38, 43, 47);   // G (sleep)
setChord(22, TOTAL_BARS - 1, 31, 38, 43, 47);   // G (goodnight + tail)

const bubbles = [];
function pushB(bar, beat, midi, opts = {}) {
  const startSec = bar * BAR + beat * BEAT;
  if (startSec >= totalSec) return;
  // bed-positional gain ramp:
  //   - ramp in over the first 4 bars
  //   - hold full through marimba section
  //   - taper across the bubble-tail bars (24..34)
  const ramp = Math.min(1, bar / 4);
  // fade the bubble bed across the closing cadence (bars 22-25)
  const tail = bar < 22 ? 1.0 : Math.max(0, 1 - (bar - 22) / 4);
  const positional = ramp * tail;
  if (positional <= 0) return;
  // small radius detune for life (±1.5 %)
  const radiusBase = midiToRadiusMM(midi);
  const radius = radiusBase * (1 + (rng() - 0.5) * 0.03);
  const rise = opts.rise ?? (0.02 + rng() * 0.20);
  const pan = opts.pan ?? (rng() - 0.5) * 1.8;
  const baseVol = opts.vol ?? 0.45;
  const volume = baseVol * positional;
  const depth = opts.depth ?? 1.0;
  bubbles.push({ startSec, radiusMM: radius, rise, volume, pan, depth });
}

// Bubbles are now bright, tight, on-the-beat ACCENTS — pitched two
// octaves above the old bass-beat range so they read as punctuation,
// not bed. Smaller radius → faster decay → naturally punctual. The
// continuous "underneath" role is handed to the sine bed below.
const BUB_OCT = 24;   // +2 octaves
// bars 12-13 — the midpoint PERC DROP: bubbles + clicks cut out for a
// breakdown breath; bluejay + the aesthetic.computer stamp fill the gap.
const PERC_DROP = new Set([12, 13]);
for (let bar = 0; bar < TOTAL_BARS; bar++) {
  if (PERC_DROP.has(bar)) continue;
  const ch = CHORDS[bar];
  // beat 1 + beat 3: bright chord-tone stabs
  pushB(bar, 0, ch.root + BUB_OCT,  { vol: 0.40, depth: 0.7, pan: -0.15 });
  pushB(bar, 1, ch.octave + BUB_OCT,{ vol: 0.34, depth: 0.6, pan: (rng()-0.5)*0.6 });
  pushB(bar, 2, ch.third + BUB_OCT, { vol: 0.36, depth: 0.7, pan: +0.12 });
  // crisp offbeat tinkles — punctual 16th-feel sparkle
  if (rng() < 0.7) pushB(bar, 0.5, ch.octave + BUB_OCT + 12, { vol: 0.16, depth: 0.4 });
  if (rng() < 0.6) pushB(bar, 1.5, ch.third  + BUB_OCT + 12, { vol: 0.15, depth: 0.4 });
  if (rng() < 0.6) pushB(bar, 2.5, ch.fifth  + BUB_OCT + 12, { vol: 0.15, depth: 0.4 });
  // sometimes — tiny bubbles: very small radius (3-7 mm → 400-1000 Hz),
  // a quick scatter of 16th-note micro-droplets across one beat
  if (rng() < 0.35) {
    const tBeat = Math.floor(rng() * 3);
    for (let s = 0; s < 4; s++) {
      const tinyMidi = 84 + Math.floor(rng() * 16);   // C6..E7 — tiny + high
      pushB(bar, tBeat + s * 0.25, tinyMidi, { vol: 0.07, depth: 0.3,
        rise: 0.1 + rng() * 0.4, pan: (rng() - 0.5) * 1.6 });
    }
  }
}

for (const b of bubbles) {
  mixSDTBubble(bubbleL, bubbleR, b.startSec, b.radiusMM, b.rise, b.volume, b.pan, SR, b.depth);
}

// ── sine bed underneath ───────────────────────────────────────────────
// Continuous low sine pad following the chord progression — the new
// "underneath". Each chord region holds root + fifth + octave low sines
// (+ a little 2nd/3rd harmonic for warmth), with slow raised-cosine
// fades so sections crossfade. Mixed into the bubble bus.
function mixBedNote(startSec, durSec, midi, gain, pan) {
  const startIdx = Math.floor(startSec * SR);
  const n = Math.floor(durSec * SR);
  const fadeS = Math.floor(1.3 * SR);
  const f = 440 * Math.pow(2, (midi - 69) / 12);
  const angle = (pan * 0.5 + 0.5) * (Math.PI / 2);
  const gL = Math.cos(angle) * Math.SQRT2, gR = Math.sin(angle) * Math.SQRT2;
  let phase = 0;
  for (let i = 0; i < n; i++) {
    phase += f / SR; if (phase >= 1) phase -= 1;
    const ph = 2 * Math.PI * phase;
    const s = (Math.sin(ph) + 0.22 * Math.sin(2 * ph) + 0.10 * Math.sin(3 * ph)) / 1.32;
    let env = 1;
    if (i < fadeS) env = 0.5 - 0.5 * Math.cos(Math.PI * i / fadeS);
    else if (i > n - fadeS) env = 0.5 - 0.5 * Math.cos(Math.PI * (n - i) / fadeS);
    const dst = startIdx + i;
    if (dst < 0 || dst >= bubbleL.length) continue;
    bubbleL[dst] += s * gain * gL * env;
    bubbleR[dst] += s * gain * gR * env;
  }
}
{
  // merge consecutive identical chords into bed segments
  let segStart = 0;
  let bedSegs = 0;
  for (let bar = 1; bar <= TOTAL_BARS; bar++) {
    const prev = CHORDS[bar - 1];
    const cur  = bar < TOTAL_BARS ? CHORDS[bar] : null;
    const changed = !cur || cur.root !== prev.root || cur.fifth !== prev.fifth;
    if (changed) {
      const ch = CHORDS[segStart];
      const startSec = segStart * BAR;
      const durSec = (bar - segStart) * BAR + 1.2;   // slight overlap
      mixBedNote(startSec, durSec, ch.root,   0.34, -0.2);
      mixBedNote(startSec, durSec, ch.fifth,  0.24, +0.2);
      mixBedNote(startSec, durSec, ch.octave, 0.16,  0.0);
      bedSegs++;
      segStart = bar;
    }
  }
  console.log(`  sine bed → ${bedSegs} chord segments`);
}

// ── bike-spoke / train-track click percussion ─────────────────────────
// CC0 freesound click (#384187 "Click Tick"), triggered on the 16th-
// note grid for fast, subtle, beat-locked percussion. A sparse pattern
// (downbeats always, ~half the offbeat 16ths) keeps it a texture, not
// a drum machine. Mixed onto the bubble bus, so the opening gate keeps
// it silent for the first 2 bars.
{
  const clickSample = loadSampleStereo(resolve(HERE, "..", "assets", "spoke-click.mp3"), SR);
  if (clickSample) {
    const THIRTYSECOND = BEAT / 8;               // faster grid — tiktiktik
    let clicks = 0;
    for (let bar = 2; bar < TOTAL_BARS - 1; bar++) {
      if (PERC_DROP.has(bar)) continue;          // midpoint breakdown
      for (let s = 0; s < 24; s++) {             // 24 thirty-seconds / 3/4 bar
        const onBeat = (s % 8 === 0);
        const onEighth = (s % 4 === 0);
        // downbeats + eighths always; ~55% of the rest
        if (!onEighth && rng() > 0.55) continue;
        const t = bar * BAR + s * THIRTYSECOND;
        // naptime — clicks soft, a faint tick texture, not a drum machine
        const gain = (onBeat ? 0.26 : onEighth ? 0.18 : 0.12) * (0.8 + rng() * 0.4);
        mixSampleStereoPanned(bubbleL, bubbleR, clickSample, t, gain,
          (rng() - 0.5) * 1.2, SR);
        clicks++;
      }
    }
    console.log(`  spoke-clicks → ${clicks} on the 32nd grid (tiktiktik)`);
  } else {
    console.log(`     · no spoke-click.mp3 in assets/`);
  }
}

// ── midpoint stamp — bluejay + aesthetic.computer ─────────────────────
// At the perc-drop breakdown (bars 12-13, the track midpoint) a blue
// jay calls and the "aesthetic dot computer" voice stamp lands — the
// audio signature, revealed in the cleared space.
{
  const A = (stem) => loadSampleStereo(resolve(HERE, "..", "assets", stem), SR);
  const jayCall   = A("bluejay-call.mp3");
  const jayWarble = A("bluejay-warble.mp3");
  if (jayCall)   mixSampleStereoPanned(bubbleL, bubbleR, jayCall,   12 * BAR + 0.5 * BEAT, 0.55, +0.35, SR);
  if (jayWarble) mixSampleStereoPanned(bubbleL, bubbleR, jayWarble, 13 * BAR + 2.0 * BEAT, 0.45, -0.4,  SR);
  // the ac-stamp itself is placed at the MASTER stage (ducked + loud),
  // not here — see the stamp block after the master bus sum.
  console.log(`  midpoint bluejay → ${jayCall ? "ok" : "(missing)"}`);
}

// ── pure formant-synthesis vocoder voice ──────────────────────────────
// No ElevenLabs / no jeffrey source. Each note is a sawtooth carrier at
// the target f0 driven through 3 resonant band-pass filters tuned to
// vowel formants (a/u/o for baa/woo/whoa). Pure DSP, lives entirely in
// node, JS↔C portable, no network calls or cache files.
//
// Vowel formants from Peterson & Barney (1952) / Hillenbrand et al.
// (1995) measurements of adult male vowels:
//   "a" (baa)  — F1 700, F2 1220, F3 2600
//   "u" (woo)  — F1 320, F2  800, F3 2400
//   "o" (whoa) — F1 500, F2  880, F3 2540
const VOWEL = {
  a: { F: [700, 1220, 2600], Q: [6, 8, 10], A: [1.00, 0.55, 0.30] },
  u: { F: [320,  800, 2400], Q: [7, 8, 10], A: [1.00, 0.40, 0.22] },
  o: { F: [500,  880, 2540], Q: [7, 8, 10], A: [1.00, 0.50, 0.25] },
};

// `slideFromMidi` (optional) makes the note begin at that pitch and
// glide to `midi` over the first 80 ms of its envelope — portamento.
function synthVoice(L, R, startSec, midi, durSec, vowel, gain, pan, sr, slideFromMidi = null, slideSec = 0.080) {
  const startIdx = Math.floor(startSec * sr);
  const n = Math.floor(durSec * sr);
  const f0Target = 440 * Math.pow(2, (midi - 69) / 12);
  const f0Start = slideFromMidi != null
    ? 440 * Math.pow(2, (slideFromMidi - 69) / 12)
    : f0Target;
  // slideSec = 0.080 → quick portamento; pass a long value (≈ durSec)
  // for a slow "falling whoa" glide across the whole note.
  const slideS = Math.floor(slideSec * sr);
  const v = VOWEL[vowel] || VOWEL.a;
  // Sawtooth carrier (rich in harmonics) — what the formant bank filters
  let phase = 0;
  // 3 Chamberlin SVFs tuned to the formants
  const N = v.F.length;
  const lp = new Float64Array(N);
  const bp = new Float64Array(N);
  const Kf = new Float64Array(N);
  const Qd = new Float64Array(N);
  for (let i = 0; i < N; i++) {
    Kf[i] = 2 * Math.sin(Math.PI * Math.min(v.F[i], sr * 0.45) / sr);
    Qd[i] = 1 / v.Q[i];
  }
  // ADSR-ish envelope: 25ms attack, hold, 80ms release
  const attS = Math.floor(0.025 * sr);
  const relS = Math.floor(0.080 * sr);
  const sustS = Math.max(0, n - attS - relS);
  // equal-power pan
  const angle = (pan * 0.5 + 0.5) * (Math.PI / 2);
  const gL = Math.cos(angle), gR = Math.sin(angle);
  for (let i = 0; i < n; i++) {
    // log-glide from f0Start to f0Target over the first 80 ms
    let f0;
    if (slideFromMidi != null && i < slideS) {
      const u = i / slideS;
      f0 = f0Start * Math.pow(f0Target / f0Start, u);
    } else {
      f0 = f0Target;
    }
    phase += f0 / sr;
    if (phase >= 1) phase -= 1;
    const saw = 2 * phase - 1;
    let voiced = 0;
    for (let f = 0; f < N; f++) {
      const hi = saw - lp[f] - Qd[f] * bp[f];
      bp[f] += Kf[f] * hi;
      lp[f] += Kf[f] * bp[f];
      voiced += bp[f] * v.A[f];
    }
    let env;
    if (i < attS) env = i / attS;
    else if (i < attS + sustS) env = 1;
    else env = 1 - (i - attS - sustS) / relS;
    if (env <= 0) continue;
    const s = voiced * env * gain;
    const dst = startIdx + i;
    if (dst < 0 || dst >= L.length) continue;
    L[dst] += s * gL;
    R[dst] += s * gR;
  }
}

const TARGET_OFFSET = -12;   // jeffrey-substitute sits an octave below marimba

// Whole-track melody — the rosewood lead in bars 0-21, vowel="a" for
// baa unless overridden. Bars 12-13 sing woo/whoa instead.
const voiceMelody = [
  // bars 0-3 are instrumental — the voice's old hush-phrase notes (at
  // ~6-10 s) were weak, so they're cut; the voice now enters at bar 4.
  // twinkle bars 4-9 — tracks the new G-major lead
  { bar: 4,  beat: 0,   midi: N.E5,  durBeats: 1.0, v: "a" },
  { bar: 4,  beat: 1,   midi: N.G5,  durBeats: 1.0, v: "a" },
  { bar: 4,  beat: 2.0, midi: N.C6,  durBeats: 0.5, v: "a" },
  { bar: 4,  beat: 2.5, midi: N.G5,  durBeats: 0.5, v: "a" },
  { bar: 5,  beat: 0,   midi: N.E5,  durBeats: 3.0, v: "a" },
  { bar: 6,  beat: 0,   midi: N.C5,  durBeats: 1.0, v: "a" },
  { bar: 6,  beat: 1,   midi: N.E5,  durBeats: 1.0, v: "a" },
  { bar: 6,  beat: 2.0, midi: N.G5,  durBeats: 0.5, v: "a" },
  { bar: 6,  beat: 2.5, midi: N.E5,  durBeats: 0.5, v: "a" },
  { bar: 7,  beat: 0,   midi: N.D5,  durBeats: 3.0, v: "a" },
  { bar: 8,  beat: 0,   midi: N.Fs5, durBeats: 1.0, v: "a" },
  { bar: 8,  beat: 1,   midi: N.A5,  durBeats: 1.0, v: "a" },
  { bar: 8,  beat: 2,   midi: N.D6,  durBeats: 1.0, v: "a" },
  { bar: 9,  beat: 0,   midi: N.B5,  durBeats: 1.5, v: "a" },
  { bar: 9,  beat: 1.5, midi: N.A5,  durBeats: 1.5, v: "a" },
  // wow bars 12-13 — "woo woo woo whoa"
  { bar: 12, beat: 0,   midi: N.B5,  durBeats: 1.0, v: "u" },
  { bar: 12, beat: 1,   midi: N.D6,  durBeats: 1.0, v: "u" },
  { bar: 12, beat: 2,   midi: N.B5,  durBeats: 1.0, v: "u" },
  { bar: 13, beat: 0,   midi: N.G5,  durBeats: 3.0, v: "o" },
  // baba bars 14-17
  { bar: 14, beat: 0,   midi: N.B5,  durBeats: 0.5, v: "a" },
  { bar: 14, beat: 0.5, midi: N.A5,  durBeats: 0.5, v: "a" },
  { bar: 14, beat: 1.0, midi: N.B5,  durBeats: 1.0, v: "a" },
  { bar: 14, beat: 2.0, midi: N.D6,  durBeats: 1.0, v: "a" },
  { bar: 15, beat: 0,   midi: N.C6,  durBeats: 0.5, v: "a" },
  { bar: 15, beat: 0.5, midi: N.B5,  durBeats: 0.5, v: "a" },
  { bar: 15, beat: 1.0, midi: N.C6,  durBeats: 1.0, v: "a" },
  { bar: 15, beat: 2.0, midi: N.E6,  durBeats: 1.0, v: "a" },
  { bar: 16, beat: 0,   midi: N.D6,  durBeats: 1.0, v: "a" },
  { bar: 16, beat: 1,   midi: N.C6,  durBeats: 1.0, v: "a" },
  { bar: 16, beat: 2,   midi: N.A5,  durBeats: 1.0, v: "a" },
  { bar: 17, beat: 0,   midi: N.D6,  durBeats: 3.0, v: "a" },
  // sleep bars 18-21
  { bar: 18, beat: 0,   midi: N.B4,  durBeats: 1.5, v: "a" },
  { bar: 18, beat: 1.5, midi: N.D5,  durBeats: 1.5, v: "a" },
  { bar: 19, beat: 0,   midi: N.G5,  durBeats: 1.0, v: "a" },
  { bar: 19, beat: 1,   midi: N.Fs5, durBeats: 1.0, v: "a" },
  { bar: 19, beat: 2,   midi: N.G5,  durBeats: 1.0, v: "a" },
  { bar: 20, beat: 0,   midi: N.D5,  durBeats: 1.0, v: "a" },
  { bar: 20, beat: 1,   midi: N.E5,  durBeats: 1.0, v: "a" },
  { bar: 20, beat: 2,   midi: N.G5,  durBeats: 1.0, v: "a" },
  { bar: 21, beat: 0,   midi: N.D5,  durBeats: 3.0, v: "a" },
  // ending bars 22-23 — closing "ooh" climb of the wind-up motif
  { bar: 22, beat: 0,   midi: N.G4,  durBeats: 1.0, v: "o" },
  { bar: 22, beat: 1,   midi: N.B4,  durBeats: 1.0, v: "o" },
  { bar: 22, beat: 2,   midi: N.D5,  durBeats: 1.0, v: "o" },
  { bar: 23, beat: 0,   midi: N.G5,  durBeats: 3.0, v: "o" },
];

// Pre-pass: every note that follows the previous one within ≤ 0.25 s
// gets a slide-from-previous-pitch attribute, so the voice glides
// between consecutive notes (theremin-style portamento) instead of
// stepping discretely.
for (let i = 1; i < voiceMelody.length; i++) {
  const prev = voiceMelody[i - 1];
  const cur  = voiceMelody[i];
  const prevEnd = prev.bar * BAR + prev.beat * BEAT + prev.durBeats * BEAT;
  const curStart = cur.bar * BAR + cur.beat * BEAT;
  if (curStart - prevEnd <= 0.25 && prev.midi !== cur.midi) {
    cur.slideFrom = prev.midi;
  }
}

// ── request-for-audio · voice-take manifest + alignment ───────────────
// The renderer emits a punch-list of every voice note to voice-takes/
// manifest.json — pop/bin/rfa.mjs reads it to record jeffrey's voice.
// If a recorded take exists for a note, it REPLACES the synth voice:
// WORLD-pitched to the score note + rubberband-fit to its duration.
// See pop/REQUEST-FOR-AUDIO.md.
const VTAKES_DIR = resolve(HERE, "..", "voice-takes");
mkdirSync(VTAKES_DIR, { recursive: true });
// --voice shaped (default) routes takes through the vocoder chain with
// the synth voice; --voice raw keeps them dry (jeffrey's real voice).
const VOICE_MODE = (() => {
  const i = process.argv.indexOf("--voice");
  return i >= 0 && process.argv[i + 1] === "raw" ? "raw" : "shaped";
})();
const NN = ["C","C#","D","Eb","E","F","F#","G","Ab","A","Bb","B"];
const noteName = (m) => NN[((Math.round(m) % 12) + 12) % 12] + (Math.floor(Math.round(m) / 12) - 1);
const takeId = (n) => `${n.bar}-${String(n.beat).replace(".", "_")}`;
// Only the canonical master render (default bpm + lead) owns the
// manifest — variation renders must not clobber it with off-tempo data.
const IS_MASTER = BPM === 56 && LEAD_PRESET === "rosewood";
if (IS_MASTER) {
  const notes = voiceMelody.map((n) => {
    const id = takeId(n);
    return { id, bar: n.bar, beat: n.beat, midi: n.midi, note: noteName(n.midi),
             durBeats: n.durBeats, durSec: n.durBeats * BEAT,
             startSec: n.bar * BAR + n.beat * BEAT, vowel: n.v,
             hasTake: existsSync(resolve(VTAKES_DIR, `${id}.wav`)) };
  });
  writeFileSync(resolve(VTAKES_DIR, "manifest.json"), JSON.stringify({
    track: "tinkaboo", lane: "marimba", bpm: BPM,
    beatSec: BEAT, barSec: BAR, notes }, null, 2));
}
// Align a recorded take to its score note — WORLD f0-replace to the
// pitch, rubberband -D to the duration. Cached; re-runs if the take
// .wav is newer than the cached aligned file.
const VENV_PY = "/Users/jas/aesthetic-computer/pop/.venv/bin/python";
const WORLDSING = resolve(HERE, "worldsing.py");
const ALIGNED_DIR = resolve(VTAKES_DIR, ".aligned");
function alignTake(n) {
  const id = takeId(n);
  const src = resolve(VTAKES_DIR, `${id}.wav`);
  if (!existsSync(src)) return null;
  mkdirSync(ALIGNED_DIR, { recursive: true });
  const pitched = resolve(ALIGNED_DIR, `${id}.pitched.wav`);
  const fit = resolve(ALIGNED_DIR, `${id}.fit.wav`);
  const stale = !existsSync(fit) || statSync(src).mtimeMs > statSync(fit).mtimeMs;
  if (stale) {
    const r1 = spawnSync(VENV_PY, [WORLDSING, src, pitched,
      "--note", noteName(n.midi), "--ap-scale", "0.7", "--unvoiced-gain", "0.5"],
      { stdio: ["ignore", "ignore", "ignore"] });
    if (r1.status !== 0 || !existsSync(pitched)) return null;
    const r2 = spawnSync("rubberband",
      ["-D", (n.durBeats * BEAT).toFixed(3), pitched, fit],
      { stdio: ["ignore", "ignore", "ignore"] });
    if (r2.status !== 0 || !existsSync(fit)) return null;
  }
  return loadSampleStereo(fit, SR);
}

// ── voice render — recorded take if present, else synth ───────────────
const takeClips = [];           // recorded takes, mixed after the shape decision
let takesUsed = 0;
for (const n of voiceMelody) {
  const startSec = n.bar * BAR + n.beat * BEAT;
  const durSec = n.durBeats * BEAT;
  const pan = ((n.bar * 7 + n.beat * 13) % 9 - 4) / 12;
  const take = alignTake(n);
  if (take) {
    takeClips.push({ sample: take, startSec, pan });
    takesUsed++;
    continue;                   // jeffrey's recorded voice replaces the synth note
  }
  const slideMain = n.slideFrom != null ? n.slideFrom + TARGET_OFFSET : null;
  const slideHarm = n.slideFrom;
  // main voice — octave below marimba (baritone)
  synthVoice(voiceL, voiceR, startSec, n.midi + TARGET_OFFSET, durSec,
             n.v, 0.28, pan, SR, slideMain);
  // octave-up harmony — unison with marimba pitch, panned opposite
  synthVoice(voiceL, voiceR, startSec, n.midi, durSec,
             n.v, 0.18, -pan, SR, slideHarm);
}
console.log(`  voice → ${voiceMelody.length} notes · ${takesUsed} recorded take(s) · rest synth`);
function mixVoiceTakes() {
  for (const c of takeClips)
    mixSampleStereoPanned(voiceL, voiceR, c.sample, c.startSec, 0.85, c.pan, SR);
}

// ── falling "whoaaa" gestures ──────────────────────────────────────────
// Long downward portamento glides — the voice tumbling through its
// whole register. One falls through the chaotic opening (resolving the
// search), one drops out of the wow section. slideSec ≈ durSec so the
// glide spans the entire note.
{
  const falls = [
    { bar: 13, beat: 0,   fromMidi: N.D6, toMidi: N.F4, durBeats: 2.4, gain: 0.26, pan: +0.2 },
  ];
  for (const f of falls) {
    const st = f.bar * BAR + f.beat * BEAT;
    const dur = f.durBeats * BEAT;
    synthVoice(voiceL, voiceR, st, f.toMidi, dur, "o", f.gain, f.pan, SR,
               f.fromMidi, dur);   // slideSec = dur → full-note fall
  }
}

// ── voice-bus shaping ─────────────────────────────────────────────────
// Light highpass + lowpass tightens the synth voice into a robotic-tube
// band. Ring mod at 80 Hz keeps the Daft-Punk / vocoder character.
function onePoleLowpass(buf, fc, sr) {
  const dt = 1 / sr;
  const rc = 1 / (2 * Math.PI * fc);
  const alpha = dt / (rc + dt);
  let y = 0;
  for (let i = 0; i < buf.length; i++) {
    y += alpha * (buf[i] - y);
    buf[i] = y;
  }
}
function onePoleHighpass(buf, fc, sr) {
  const dt = 1 / sr;
  const rc = 1 / (2 * Math.PI * fc);
  const alpha = rc / (rc + dt);
  let yPrev = 0, xPrev = 0;
  for (let i = 0; i < buf.length; i++) {
    const x = buf[i];
    const y = alpha * (yPrev + x - xPrev);
    buf[i] = y;
    xPrev = x; yPrev = y;
  }
}
function ringMod(buf, carrierHz, mix, sr) {
  const dt = 1 / sr;
  let phase = 0;
  for (let i = 0; i < buf.length; i++) {
    phase += carrierHz * dt;
    if (phase >= 1) phase -= 1;
    const carrier = Math.sin(2 * Math.PI * phase);
    buf[i] = buf[i] * (1 - mix) + (buf[i] * carrier) * mix;
  }
}
// Flanger — short modulated delay summed with dry signal creates a
// swept comb-filter that adds the classic "jet-engine / underwater"
// shimmer. Each channel gets a slightly different LFO phase so the
// flange-notch drifts across the stereo image.
function flanger(buf, rateHz, depthMs, mix, lfoPhaseOffset, sr) {
  const maxSamples = Math.ceil(depthMs * 0.001 * sr) + 4;
  const dbuf = new Float32Array(maxSamples * 2);
  let widx = 0;
  let lfoPhase = lfoPhaseOffset;
  const dt = 1 / sr;
  for (let i = 0; i < buf.length; i++) {
    lfoPhase += rateHz * dt;
    if (lfoPhase >= 1) lfoPhase -= 1;
    const lfo = 0.5 * (1 + Math.sin(2 * Math.PI * lfoPhase));
    const delaySamples = lfo * depthMs * 0.001 * sr + 1;
    let rIdx = widx - delaySamples + dbuf.length;
    const i0 = Math.floor(rIdx) % dbuf.length;
    const i1 = (i0 + 1) % dbuf.length;
    const f = rIdx - Math.floor(rIdx);
    const delayed = dbuf[i0] * (1 - f) + dbuf[i1] * f;
    dbuf[widx % dbuf.length] = buf[i];
    buf[i] = buf[i] * (1 - mix * 0.5) + delayed * mix;
    widx++;
  }
}
function shapeVoice(buf, sr, lfoOffset) {
  onePoleHighpass(buf, 220, sr);
  for (let p = 0; p < 3; p++) onePoleLowpass(buf, 3200, sr);
  ringMod(buf, 80, 0.20, sr);
  // subtle slow flange — 0.3 Hz sweep, ~6 ms depth, 40 % mix
  flanger(buf, 0.3, 6.0, 0.40, lfoOffset, sr);
}
// shaped: recorded takes go in BEFORE shapeVoice → vocoder chain hits
// them too (jeffrey becomes the synth). raw: takes mixed in AFTER →
// dry, jeffrey's actual voice replacing the computer voice.
if (VOICE_MODE === "shaped") mixVoiceTakes();
shapeVoice(voiceL, SR, 0.00);
shapeVoice(voiceR, SR, 0.33);   // L/R offset gives stereo flange width
if (VOICE_MODE === "raw") mixVoiceTakes();

// Voice reverb — but DRY for the first 2 bars (the searching opening),
// crossfading into reverb at bar 2. The voice-alone entrance stays
// intimate + close; once the tune is "found" the reverb blooms.
{
  const dryL = voiceL.slice();
  const dryR = voiceR.slice();
  applyReverb(voiceL, SR, 0.26, 1.6);
  applyReverb(voiceR, SR, 0.26, 1.6);
  const gateN = Math.floor(2 * BAR * SR);
  const fade = Math.floor(1.0 * SR);
  for (let i = 0; i < gateN + fade && i < voiceL.length; i++) {
    let dryAmt = 1;
    if (i > gateN) dryAmt = Math.max(0, 1 - (i - gateN) / fade);
    voiceL[i] = voiceL[i] * (1 - dryAmt) + dryL[i] * dryAmt;
    voiceR[i] = voiceR[i] * (1 - dryAmt) + dryR[i] * dryAmt;
  }
}

const ASSETS_DIR = resolve(HERE, "..", "assets");
mkdirSync(ASSETS_DIR, { recursive: true });

// All jeffrey/ElevenLabs voice plumbing removed — the synth voice block
// above is the only thing that fills voiceL / voiceR now.

// ── optional samples from freesound (whip crack + lamb + siren) ───────
// Drop a CC0 sample into pop/marimba/assets/<name>.{mp3,wav,m4a,flac}
// and it gets mixed in at the placement below. Missing files print a
// hint and skip cleanly so the renderer always produces output.
function checkAndLoad(stem) {
  const exts = ["mp3", "wav", "m4a", "flac", "ogg"];
  for (const e of exts) {
    const p = resolve(ASSETS_DIR, `${stem}.${e}`);
    if (existsSync(p)) return { path: p, sample: loadSampleStereo(p, SR) };
  }
  return null;
}

const whip = checkAndLoad("whip-crack");
if (whip) {
  console.log(`     ✓ loaded ${whip.path.replace(ASSETS_DIR + "/", "assets/")}`);
  mixSampleStereoPanned(voiceL, voiceR, whip.sample, 0.06, 0.9, 0.0, SR);
} else {
  console.log(`     · no whip-crack.* in assets/ — drop a freesound sample to enable`);
}

const lamb = checkAndLoad("lamb-bleat");
if (lamb) {
  console.log(`     ✓ loaded ${lamb.path.replace(ASSETS_DIR + "/", "assets/")}`);
  // Two lamb bleats drifting in different places, panned away from the
  // jeffrey-baa placements so the lamb and the jeffrey-lamb interleave.
  mixSampleStereoPanned(voiceL, voiceR, lamb.sample, 10 * BAR + 1 * BEAT, 0.35, +0.5, SR);
  mixSampleStereoPanned(voiceL, voiceR, lamb.sample, 17 * BAR + 0 * BEAT, 0.30, -0.5, SR);
} else {
  console.log(`     · no lamb-bleat.* in assets/ — drop a freesound sample to enable`);
}

const siren = checkAndLoad("manhattan-siren");
if (siren) {
  console.log(`     ✓ loaded ${siren.path.replace(ASSETS_DIR + "/", "assets/")}`);
  // The siren becomes a chromatic chordal PAD — rubberband-pitched
  // copies of the wail stacked into minor triads (low / root / 3rd /
  // 5th), each chord fading in + out like a synth pad. Chord roots
  // wander chromatically through the track. The low voice (root −12)
  // is a deep groaning drone under the chord.
  const sirenWav = siren.path.replace(/\.[^.]+$/, ".decoded.wav");
  if (!existsSync(sirenWav)) {
    spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-i", siren.path, "-ar", String(SR), "-ac", "2", sirenWav],
      { stdio: "ignore" });
  }
  const sirenPitchCache = new Map();
  function pitchSiren(semis) {
    const k = Math.round(semis);
    if (sirenPitchCache.has(k)) return sirenPitchCache.get(k);
    let s;
    if (k === 0) {
      s = siren.sample;
    } else {
      const w = resolve(ASSETS_DIR, `manhattan-siren.p${k >= 0 ? "+" : ""}${k}.wav`);
      if (!existsSync(w)) {
        const r = spawnSync("rubberband", ["-p", String(k), sirenWav, w],
          { stdio: ["ignore", "ignore", "ignore"] });
        if (r.status !== 0 || !existsSync(w)) { sirenPitchCache.set(k, null); return null; }
      }
      s = loadSampleStereo(w, SR);
    }
    sirenPitchCache.set(k, s);
    return s;
  }
  // pad-fade mixer — raised-cosine fade + internal pitch-wobble + a
  // bitcrush so the siren has a crunchy, modulating-pitch character.
  function mixSirenPad(sample, startSec, durSec, gain, pan, driftSemis = 0) {
    if (!sample) return;
    const startIdx = Math.floor(startSec * SR);
    const n = Math.floor(durSec * SR);
    const fIn = Math.floor(1.6 * SR), fOut = Math.floor(5.0 * SR);
    const angle = (pan * 0.5 + 0.5) * (Math.PI / 2);
    const gL = Math.cos(angle) * Math.SQRT2, gR = Math.sin(angle) * Math.SQRT2;
    // bitcrush — ~5.5-bit quantise
    const crush = (x) => Math.round(x * 24) / 24;
    // internal pitch-wobble (±5 % at 0.4 Hz) + a slow linear pitch
    // DRIFT of driftSemis over the whole note — the siren audibly
    // changes pitch as it swells, sliding up or down.
    let readPos = 0;
    let holdL = 0, holdR = 0;
    for (let i = 0; i < n; i++) {
      const prog = i / n;
      const driftRate = Math.pow(2, (driftSemis * prog) / 12);
      const lfo = Math.sin(2 * Math.PI * 0.4 * i / SR);
      readPos += driftRate * (1.0 + lfo * 0.05);
      const ri = Math.floor(readPos);
      if (ri >= sample.L.length - 1) break;
      const f = readPos - ri;
      const sL = sample.L[ri] * (1 - f) + sample.L[ri + 1] * f;
      const sR = sample.R[ri] * (1 - f) + sample.R[ri + 1] * f;
      // 2× sample-and-hold downsample → extra digital crunch
      if ((i & 1) === 0) { holdL = crush(sL); holdR = crush(sR); }
      let env = 1;
      if (i < fIn) env = 0.5 - 0.5 * Math.cos(Math.PI * i / fIn);
      else if (i > n - fOut) env = 0.5 - 0.5 * Math.cos(Math.PI * (n - i) / fOut);
      const dst = startIdx + i;
      if (dst < 0 || dst >= sirenL.length) continue;
      sirenL[dst] += holdL * gain * gL * env;
      sirenR[dst] += holdR * gain * gR * env;
    }
  }
  // SIREN_BASE drops the whole thing ~a fourth so it reads as a pad.
  // Each chord now varies — `oct` shifts a whole chord lower or higher,
  // `dur` makes some chords long swells and others short stabs.
  const SIREN_BASE = -5;
  // `drift` = semitone glide each chord makes over its duration — the
  // siren audibly slides pitch as it swells (up on some, down on others).
  const sirenChords = [
    { bar: 3,  root:  0, oct: -12, dur:  6.5, pan: +0.45, drift: +3 },  // low · short · up
    { bar: 7,  root: -2, oct:  +0, dur: 12.0, pan: -0.45, drift: -5 },  // high · long · down
    { bar: 12, root: +1, oct: -12, dur:  5.5, pan: +0.35, drift: +4 },  // low · short · up
    { bar: 16, root: -3, oct:  +7, dur: 11.0, pan: -0.35, drift: -7 },  // high · long · down
    { bar: 21, root:  0, oct:  -5, dur:  8.0, pan: +0.25, drift: +2 },  // mid · up
  ];
  let sirenVoices = 0;
  for (const ch of sirenChords) {
    const r = SIREN_BASE + ch.root + ch.oct;
    // Voices spread WIDE across octaves (−12 … +19, over 2½ octaves)
    // and enter STAGGERED in time (tOff) — so the siren builds an
    // overlapping, evolving harmony instead of one stacked chord that
    // loops in lockstep. Each drifts a touch differently too.
    const chord = [
      { semi: r - 12, gain: 0.16, panJ:  0.00, dMul: 1.0, tOff: 0.0 }, // low drone first
      { semi: r,      gain: 0.12, panJ:  0.22, dMul: 1.3, tOff: 1.5 }, // root enters
      { semi: r + 7,  gain: 0.10, panJ: -0.22, dMul: 0.8, tOff: 3.0 }, // fifth
      { semi: r + 15, gain: 0.07, panJ:  0.30, dMul: 1.5, tOff: 4.3 }, // high colour
    ];
    for (const c of chord) {
      const s = pitchSiren(c.semi);
      if (s) {
        mixSirenPad(s, ch.bar * BAR + c.tOff, ch.dur, c.gain,
                    ch.pan + c.panJ, ch.drift * c.dMul);
        sirenVoices++;
      }
    }
  }
  console.log(`     · siren chords: ${sirenChords.length} × 4-voice (${sirenVoices} pitched pads · varied oct/dur · wobble + drift + crush)`);
} else {
  console.log(`     · no manhattan-siren.* in assets/ — synth sirens only`);
}
// Synthesised sirens ALWAYS layer in too — the wail-stand-ins sit
// alongside the sample chords on the same bus, very low gain, as extra
// distant city texture. (Kept because the synth ones sounded good.)
mixSiren(sirenL, sirenR, 6 * BAR, 6.5, 0.030, +0.7, SR);
mixSiren(sirenL, sirenR, 15 * BAR + 1 * BEAT, 5.0, 0.024, -0.75, SR);
mixSiren(sirenL, sirenR, 20 * BAR, 4.0, 0.022, +0.5, SR);

// ── master bus sum ────────────────────────────────────────────────────
// Per-bus relative levels. Numbers chosen by ear: marimba lead is the
// focus, bubbles support, jeffrey is a clear character, siren is
// distant. Each bus gets normalised to peak 1.0 first so the constants
// below are TRUE relative dB ratios, independent of how loud each bus
// happens to render.
// "Naptime mix" — gentle lullaby balance: the marimba + voice + sine
// bed carry it; the awake/bright layers (siren, percussion) sit well
// back so the whole thing stays soft and restful.
const BUS_GAIN = {
  marimba: 0.82,   // lead — gentle focus
  bubble:  0.50,   // sine bed + soft bubble accents
  voice:   0.30,   // synth singer
  siren:   0.15,   // chordal pads + synth sirens — far back, distant city
};
normalizeBus(marimbaL, marimbaR);
normalizeBus(bubbleL,  bubbleR);
normalizeBus(voiceL,   voiceR);
normalizeBus(sirenL,   sirenR);

// ── cool panning ──────────────────────────────────────────────────────
// Slow auto-pan + volume-swell LFOs per bus, each at its own rate +
// phase so the stereo image is always drifting. Marimba + voice stay
// near-centred (small depth — they're the focus); bubbles + siren
// sweep wide.
autoPanVol(marimbaL, marimbaR, 0.060, 0.18, 0.040, 0.06, 0.00, SR);
autoPanVol(bubbleL,  bubbleR,  0.085, 0.55, 0.052, 0.13, 0.31, SR);
autoPanVol(voiceL,   voiceR,   0.071, 0.22, 0.045, 0.08, 0.63, SR);
autoPanVol(sirenL,   sirenR,   0.047, 0.68, 0.034, 0.16, 0.88, SR);

// ── tom beat — sampled low tom drum on the 3/4 pattern ────────────────
// CC0 freesound acoustic low-tom (#581467). Replaces the synthesised
// kick (which clipped on its pitch-drop transient). Downbeat strong +
// a softer hit on the "and" of beat 2, the full track. Dropped during
// the midpoint perc-break (bars 12-13).
{
  const tom = loadSampleStereo(resolve(HERE, "..", "assets", "kick-tom.mp3"), SR);
  if (tom) {
    let hits = 0;
    // start at bar 1 — the track opens on the gong, not a kick
    for (let bar = 1; bar < TOTAL_BARS - 1; bar++) {
      if (PERC_DROP.has(bar)) continue;          // breakdown
      mixSampleStereoPanned(marimbaL, marimbaR, tom, bar * BAR + 0.0 * BEAT, 0.34, -0.06, SR);
      mixSampleStereoPanned(marimbaL, marimbaR, tom, bar * BAR + 1.5 * BEAT, 0.22, +0.07, SR);
      hits += 2;
    }
    console.log(`  tom beat → ${hits} hits (sampled low tom)`);

    // ── baby breakcore break — ~46 s (bar 14) ──────────────────────────
    // A little chopped-up percussion burst — fast 32nd/64th tom + click
    // hits with stutters — a playful jolt right after the midpoint
    // breakdown. Soft-gained (it's still a lullaby) but frantic.
    const clk = loadSampleStereo(resolve(HERE, "..", "assets", "spoke-click.mp3"), SR);
    const S64 = BEAT / 16;                       // 64th-note grid
    let chops = 0;
    for (let step = 0; step < 48; step++) {      // 3 beats of bar 14
      // dense but not every slot — irregular for the chopped feel
      if (rng() > 0.62) continue;
      const t = 14 * BAR + step * S64;
      const useTom = rng() < 0.35;
      const samp = useTom ? tom : (clk || tom);
      const gain = (useTom ? 0.30 : 0.20) * (0.7 + rng() * 0.5);
      mixSampleStereoPanned(marimbaL, marimbaR, samp, t, gain,
        (rng() - 0.5) * 1.5, SR);
      chops++;
      // occasional stutter — 2-3 rapid repeats of the same hit
      if (rng() < 0.22) {
        const reps = 2 + Math.floor(rng() * 2);
        for (let r = 1; r <= reps; r++) {
          mixSampleStereoPanned(marimbaL, marimbaR, samp,
            t + r * (S64 * 0.5), gain * (1 - r * 0.18),
            (rng() - 0.5) * 1.5, SR);
          chops++;
        }
      }
    }
    console.log(`  baby breakcore → ${chops} chops at bar 14`);
  } else {
    console.log(`     · no kick-tom.mp3 in assets/`);
  }
}

const outL = new Float32Array(ns);
const outR = new Float32Array(ns);
for (let i = 0; i < ns; i++) {
  outL[i] = marimbaL[i] * BUS_GAIN.marimba
          + bubbleL[i]  * BUS_GAIN.bubble
          + voiceL[i]   * BUS_GAIN.voice
          + sirenL[i]   * BUS_GAIN.siren;
  outR[i] = marimbaR[i] * BUS_GAIN.marimba
          + bubbleR[i]  * BUS_GAIN.bubble
          + voiceR[i]   * BUS_GAIN.voice
          + sirenR[i]   * BUS_GAIN.siren;
}

// ── opening gong — one huge hit ───────────────────────────────────────
// A single deep CC0 gong (#415200), pitched down 3 semitones for size,
// struck ONCE at t=0. Rings out at its natural speed — no stretching.
{
  const gongSrc  = resolve(HERE, "..", "assets", "gong.mp3");
  const gongDeep = resolve(HERE, "..", "assets", "gong-deep.wav");
  if (existsSync(gongSrc) && !existsSync(gongDeep)) {
    const gw = gongSrc.replace(/\.mp3$/, ".decoded.wav");
    spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-i", gongSrc, "-ar", String(SR), "-ac", "2", gw], { stdio: "ignore" });
    spawnSync("rubberband", ["-p", "-3", gw, gongDeep],
      { stdio: ["ignore", "ignore", "ignore"] });
  }
  const gong = existsSync(gongDeep) ? loadSampleStereo(gongDeep, SR) : null;
  if (gong) {
    for (let i = 0; i < gong.L.length && i < ns; i++) {
      outL[i] += gong.L[i] * 0.60;
      outR[i] += gong.R[i] * 0.60;
    }
    console.log(`  opening gong → one hit (${(gong.L.length / SR).toFixed(1)} s natural ring)`);
  }
}

// (the spoken "aesthetic dot computer" audio stamp was removed — the
// midpoint breakdown stays instrumental: bluejay + the cleared space.)

// ── phaser on the master bed ──────────────────────────────────────────
// Very slow LFO (0.10 Hz → one full sweep every 10 s) so the swirl is
// "background weather" rather than a foreground effect. L/R LFOs are
// offset by ~0.27 cycles so the notches drift across the stereo image.
applyPhaserStereo(outL, outR, SR,
  /*rateHz*/ 0.10,
  /*minFreq*/ 280,
  /*maxFreq*/ 1400,
  /*stages*/ 4,
  /*wet*/ 0.55);

console.log(`→ tinkaboo · ${events.length} marimba + ${connectors.length} connectors + ${bubbles.length} bubbles + phaser · ${totalSec.toFixed(1)} s · G major 3/4 @ ${BPM} BPM`);

// scrub NaN / Inf before normalize — defensive, since some weird SDT
// parameter combinations (extreme negative rise + huge radius) could
// otherwise blow up psymodel inside libmp3lame.
let nanCount = 0;
for (let i = 0; i < ns; i++) {
  if (!Number.isFinite(outL[i])) { outL[i] = 0; nanCount++; }
  if (!Number.isFinite(outR[i])) { outR[i] = 0; nanCount++; }
}
if (nanCount) console.warn(`     ! scrubbed ${nanCount} non-finite samples`);

// normalise to -1 dBFS peak across both channels
let peak = 0;
for (let i = 0; i < ns; i++) {
  const aL = Math.abs(outL[i]); if (aL > peak) peak = aL;
  const aR = Math.abs(outR[i]); if (aR > peak) peak = aR;
}
if (peak > 0) {
  const nrm = 0.86 / peak;
  for (let i = 0; i < ns; i++) { outL[i] *= nrm; outR[i] *= nrm; }
}

// ── auto-trim trailing dead space ─────────────────────────────────────
// Scan back from the end for the last audibly-loud sample, then keep
// only up to that point + a 0.5 s tail. Guarantees the file never
// carries dead silence regardless of how the decay tails land.
let lastLoud = ns - 1;
const trimThresh = 0.004;   // ≈ −48 dBFS
while (lastLoud > 0 &&
       Math.abs(outL[lastLoud]) < trimThresh &&
       Math.abs(outR[lastLoud]) < trimThresh) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.5 * SR));
if (trimN < ns) {
  console.log(`  trimmed ${((ns - trimN) / SR).toFixed(1)} s trailing silence → ${(trimN / SR).toFixed(1)} s`);
}

// fade in / fade out — fade-out anchored to the TRIMMED end
// tiny fade-in — just click-suppression (5 ms) so the downbeat kick
// lands hard on beat 1 instead of being swallowed by a long fade.
const fadeIn = Math.floor(0.005 * SR);
const fadeOut = Math.floor(1.8 * SR);
for (let i = 0; i < fadeIn && i < trimN; i++) {
  const g = i / fadeIn;
  outL[i] *= g; outR[i] *= g;
}
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i;
  const g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const argv = process.argv.slice(2);
let outFlag = null;
for (let i = 0; i < argv.length; i++) {
  if (argv[i] === "--out" && argv[i + 1]) outFlag = argv[i + 1];
}
const outPath = expandHome(outFlag) || resolve(HERE, "..", "out", "marimbaba.mp3");

mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
// interleave L,R for stereo float-LE — only the trimmed length
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// ── pop master chain ──────────────────────────────────────────────────
// subsonic high-pass → gentle glue compression → brickwall true-peak
// limiter. Loudness lifts out of the comp makeup + limiter so the track
// sits at a commercial pop level without crushing the lullaby dynamics.
const MASTER = [
  "highpass=f=30",                                       // trim subsonic
  "acompressor=threshold=-20dB:ratio=2.2:attack=22:release=240:makeup=2.0:knee=6", // soft glue
  "treble=g=1.0:f=7500",                                 // a touch of air (gentle — naptime)
  "alimiter=limit=0.95:attack=4:release=60",             // brickwall
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER,
  "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (pop-mastered · ${(trimN / SR).toFixed(1)} s)`);

// auto-open + play — close any stale document first so QuickTime
// reloads the fresh render. Suppressed with --no-open (variation
// renders shouldn't each grab QuickTime).
if (process.argv.includes("--no-open")) process.exit(0);
spawnSync("osascript", ["-e", `
tell application "QuickTime Player"
  if running then close every document
  open POSIX file ${JSON.stringify(outPath)}
  play (front document)
  activate
end tell`], { stdio: "ignore" });
