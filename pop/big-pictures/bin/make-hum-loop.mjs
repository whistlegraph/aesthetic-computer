#!/usr/bin/env node
// make-hum-loop.mjs — turn the IMG_8802 field recording (a CAR HORN,
// dominant pitch ~C#) into a GRANULAR HARMONIC LOOP that opens the
// amaythingra track under the boot chime.
//
// Concept (@jeffrey 2026-06-02): "a car horn i want in the intro… a more
// granular smooth loop cuz it sort of harmonizes." A car horn is already
// a sustained harmonic tone; granulating it (many small Hann-windowed
// grains at jittered read positions) extends + smooths it into an
// evolving loop that never audibly repeats, while keeping its bright
// horn character.
//
// The horn sits on C# — a tritone against the track's G major pentatonic.
// Pitched UP +1 semitone, C# → D (the drone's fifth) and the secondary
// D# → E, so both partials land inside G pentatonic and the horn locks to
// the bed's pads instead of fighting them. The lowpass just rounds off
// the very top so it sits as a warm horn-drone, not a blaring honk.
//
// Granular synthesis runs in-process (JS), NOT via an ffmpeg filtergraph:
// a dense grain cloud is thousands of grains, far past what amix can take.
//
// Reads:   pop/big-pictures/voice-takes/IMG_8802.m4a   (override --src)
// Writes:  pop/big-pictures/out/intro-hum.wav          (stereo 48k)
//          pop/big-pictures/out/intro-hum-preview.mp3
//
// Usage:
//   node pop/big-pictures/bin/make-hum-loop.mjs \
//     [--semitones 1] [--length 22] [--grain 0.09] [--hop 0.028] \
//     [--jitter 0.9] [--lowpass 1500] [--no-cache]

import { execSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");

const argv = process.argv.slice(2);
function flag(name, def) {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : def;
}
const SRC = resolve(flag("src", resolve(POP, "big-pictures/voice-takes/IMG_8802.m4a")));
const OUT = resolve(flag("out", resolve(POP, "big-pictures/out/intro-hum.wav")));
const OUT_MP3 = OUT.replace(/\.wav$/, "-preview.mp3");
const SEMITONES = parseFloat(flag("semitones", "1"));   // C# → D
const LENGTH    = parseFloat(flag("length", "58"));     // total hum seconds (rides ~26s→1:24, into the saws)
const GRAIN     = parseFloat(flag("grain", "0.13"));    // grain length (s)
const HOP       = parseFloat(flag("hop", "0.04"));      // time between grain onsets (s)
const JITTER    = parseFloat(flag("jitter", "0.9"));    // read-position jitter (fraction of usable core)
const LOWPASS   = parseFloat(flag("lowpass", "3800"));  // a touch brighter so it meshes with the saws
const BUMP      = parseFloat(flag("bump", "0.06"));     // kick-synced UPWARD pitch-bump depth (0 = off)
const BUMP_BPM  = parseFloat(flag("bump-bpm", "120"));  // beat rate the bump syncs to (matches the bed)
const BUMP_TAU  = parseFloat(flag("bump-tau", "0.07")); // bump decay (s) — sharp spike on the beat
const CORE_A    = parseFloat(flag("core-start", "1.0"));// steady region of the take
const CORE_B    = parseFloat(flag("core-end", "9.5"));
const NO_CACHE  = argv.includes("--no-cache");
const SR = 48000;
const TMP = "/tmp/intro-hum";
mkdirSync(TMP, { recursive: true });
mkdirSync(dirname(OUT), { recursive: true });

function sh(cmd) { execSync(cmd, { stdio: ["ignore", "ignore", "pipe"] }); }
function dur(p) {
  return parseFloat(execSync(
    `ffprobe -v error -show_entries format=duration -of csv=p=0 "${p}"`
  ).toString().trim());
}
// deterministic PRNG (no Math.random — reruns stay identical)
function makeRng(seed) {
  let s = seed >>> 0;
  return () => { s ^= s << 13; s ^= s >>> 17; s ^= s << 5; s >>>= 0; return s / 4294967296; };
}

if (!existsSync(SRC)) { console.error(`✗ source not found: ${SRC}`); process.exit(1); }

// 1 — mono 48k + pitch up +SEMITONES (C# → D) → isolate a steady core.
const CORE_F32 = `${TMP}/core.f32`;
if (NO_CACHE || !existsSync(CORE_F32)) {
  const mono = `${TMP}/mono.wav`;
  sh(`ffmpeg -y -loglevel error -i "${SRC}" -ar ${SR} -ac 1 "${mono}"`);
  const pitched = `${TMP}/pitched.wav`;
  if (Math.abs(SEMITONES) > 0.001) {
    sh(`rubberband --pitch ${SEMITONES} --formant --crisp 5 "${mono}" "${pitched}" 2>/dev/null`);
  } else { execSync(`cp "${mono}" "${pitched}"`); }
  const srcDur = dur(pitched);
  const a = Math.max(0, Math.min(CORE_A, srcDur - 1));
  const b = Math.min(CORE_B, srcDur);
  const coreLen = Math.max(1.5, b - a);
  // emit the core as raw float32 so JS can scatter grains sample-accurately
  sh(`ffmpeg -y -loglevel error -ss ${a.toFixed(3)} -t ${coreLen.toFixed(3)} -i "${pitched}" ` +
     `-af "loudnorm=I=-20:TP=-3:LRA=5" -ar ${SR} -ac 1 -f f32le "${CORE_F32}"`);
}
const coreBytes = readFileSync(CORE_F32);
const core = new Float32Array(coreBytes.buffer, coreBytes.byteOffset, Math.floor(coreBytes.length / 4));
const coreLen = core.length;
const grainN = Math.max(8, Math.round(GRAIN * SR));
const usable = Math.max(1, coreLen - grainN);

// 2 — GRANULAR CLOUD. Two independent grain streams (L / R) with separate
//     RNG seeds → naturally decorrelated stereo width, and the density
//     dissolves any voice. Each grain: Hann window, jittered read pos.
const N = Math.round(LENGTH * SR);
const L = new Float32Array(N);
const R = new Float32Array(N);
const hopN = Math.max(1, Math.round(HOP * SR));

function scatter(out, seed) {
  const rng = makeRng(seed);
  let grains = 0;
  for (let onset = 0; onset < N; onset += hopN) {
    // read position wanders across the whole usable core (heavy jitter)
    const read = Math.floor(rng() * usable * JITTER + (1 - JITTER) * 0.5 * usable);
    const r0 = Math.max(0, Math.min(usable - 1, read));
    for (let i = 0; i < grainN; i++) {
      const o = onset + i;
      if (o >= N) break;
      // Hann window
      const w = 0.5 - 0.5 * Math.cos((2 * Math.PI * i) / (grainN - 1));
      out[o] += core[r0 + i] * w;
    }
    grains++;
  }
  return grains;
}
const gL = scatter(L, 0x1234567);
const gR = scatter(R, 0x89abcdef);
console.log(`granular cloud: ${gL}+${gR} grains · grain ${(GRAIN * 1000) | 0}ms · hop ${(HOP * 1000) | 0}ms · ${LENGTH}s`);

// kick-synced UPWARD pitch bump (@jeffrey "its sidechain a little pitch
// bump, pitch up"): resample with rate r(t) = 1 + depth·exp(-(t mod beat)/τ)
// so the pitch SPIKES UP on every beat (= the kick grid, since the hum is
// delayed onto a beat at bake time) then settles. The hum is a texture, so
// the tiny cumulative read-drift is inaudible.
function pitchBump(buf) {
  if (BUMP <= 0) return buf;
  const beat = 60 / BUMP_BPM;
  const out = new Float32Array(buf.length);
  let p = 0;
  for (let j = 0; j < out.length; j++) {
    const phase = (j / SR) % beat;
    const rate = 1 + BUMP * Math.exp(-phase / BUMP_TAU);
    const i0 = Math.floor(p);
    if (i0 >= buf.length - 1) { out[j] = buf[buf.length - 1] || 0; continue; }
    out[j] = buf[i0] + (buf[i0 + 1] - buf[i0]) * (p - i0);
    p += rate;
  }
  return out;
}
const Lb = pitchBump(L), Rb = pitchBump(R);
if (BUMP > 0) console.log(`  pitch-bump: +${(BUMP * 100) | 0}% on each beat @ ${BUMP_BPM} BPM (kick-synced)`);

// normalize the raw cloud to ~ -3 dBFS before ffmpeg post
let peak = 0;
for (let i = 0; i < N; i++) { const a = Math.abs(Lb[i]); if (a > peak) peak = a; const b = Math.abs(Rb[i]); if (b > peak) peak = b; }
const norm = peak > 0 ? 0.7 / peak : 1;
const inter = Buffer.alloc(N * 2 * 4);
for (let i = 0; i < N; i++) {
  inter.writeFloatLE(Math.max(-1, Math.min(1, Lb[i] * norm)), i * 8);
  inter.writeFloatLE(Math.max(-1, Math.min(1, Rb[i] * norm)), i * 8 + 4);
}
const CLOUD = `${TMP}/cloud.f32`;
writeFileSync(CLOUD, inter);

// 3 — gentle warm lowpass (round the top), long fades (swell in /
//     dissolve out), final loudnorm.
// Long CRESCENDO — starts very quiet and grows over almost the whole loop
// (@jeffrey "move it to the start, rly quiet, get louder as we progress"),
// with just a short tail at the very end.
const fadeIn = (LENGTH * 0.82).toFixed(2);
const fadeOutStart = (LENGTH - 4).toFixed(2);
execSync(
  `ffmpeg -y -loglevel error -f f32le -ar ${SR} -ac 2 -i "${CLOUD}" ` +
  `-af "lowpass=f=${LOWPASS},` +
  `afade=t=in:st=0:d=${fadeIn}:curve=exp,` +
  `afade=t=out:st=${fadeOutStart}:d=4,` +
  `loudnorm=I=-18:TP=-2:LRA=6" ` +
  `-ar ${SR} -ac 2 -c:a pcm_s16le "${OUT}"`
);
sh(`ffmpeg -y -loglevel error -i "${OUT}" -b:a 256k "${OUT_MP3}"`);
console.log(`✓ ${OUT.replace(POP + "/", "pop/")} (${dur(OUT).toFixed(1)}s) — car horn pitched +${SEMITONES}st, granular`);
console.log(`✓ ${OUT_MP3.replace(POP + "/", "pop/")}  ← quick listen`);
