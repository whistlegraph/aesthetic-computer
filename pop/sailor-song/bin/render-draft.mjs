#!/usr/bin/env node
// render-draft.mjs — put drums + a bed under the niece's Sailor Song take,
// two ways, so they can be A/B'd:
//
//   follow — her audio untouched; every hit lands on *her* detected beat
//            (take.analysis.json pulse.beats), so the kit breathes with
//            her rubato.
//   warp   — rubberband time-maps the whole take onto a fixed grid in one
//            pass (-M timemap: each detected beat → k·60/BPM), then the
//            kit plays straight on that grid.
//
// Same kit, bed, and excerpt either way — only the clock differs. Kit is
// the booch inline boom-bap synthesis softened for a ballad; bed is the
// booch sub + mellow rhodes on the 2-bar chord cycle, detuned +13¢ to sit
// on her guitar. Bottom-up posture: nothing sampled but her.
//
//   node pop/sailor-song/bin/render-draft.mjs --mode follow
//   node pop/sailor-song/bin/render-draft.mjs --mode warp --bpm 120
//   node pop/sailor-song/bin/render-draft.mjs --mode follow --from 0 --to 179

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

import { mixEventSub } from "../../booch/synths/sub.mjs";
import { mixEventRhodes } from "../../booch/synths/rhodes.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const SR = 48_000;

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const n = process.argv[i + 1];
  if (n !== undefined && !n.startsWith("--")) { flags[a.slice(2)] = n; i++; } else flags[a.slice(2)] = true;
}
const MODE = flags.mode || "follow";
const FROM = Number(flags.from ?? 20);   // excerpt, in *source* seconds
const TO = Number(flags.to ?? 80);
const BPM = Number(flags.bpm ?? 120);   // warp grid
const BED = Number(flags.bed ?? 0.5);   // bed level under her
const TUNE = 0.13;                      // her guitar sits +13¢ sharp

const A = JSON.parse(readFileSync(resolve(LANE, "take.analysis.json"), "utf8"));
const SRC_BEATS = A.pulse.beats;
const TAKE = resolve(LANE, "src/take.wav");
const OUT = resolve(LANE, "out");
mkdirSync(OUT, { recursive: true });

// ── wav io (16/32-bit PCM or float in, float32 stereo out) ───────────────
function readWav(path) {
  const b = readFileSync(path);
  let p = 12, fmt, data;
  while (p < b.length) {
    const id = b.toString("ascii", p, p + 4), sz = b.readUInt32LE(p + 4);
    if (id === "fmt ") fmt = { tag: b.readUInt16LE(p + 8), ch: b.readUInt16LE(p + 10), sr: b.readUInt32LE(p + 12), bits: b.readUInt16LE(p + 22) };
    if (id === "data") data = b.subarray(p + 8, p + 8 + sz);
    p += 8 + sz + (sz & 1);
  }
  const n = data.length / (fmt.bits / 8) / fmt.ch;
  const L = new Float32Array(n), R = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    for (let c = 0; c < 2; c++) {
      const k = i * fmt.ch + Math.min(c, fmt.ch - 1);
      const v = fmt.bits === 16 ? data.readInt16LE(k * 2) / 32768
        : fmt.tag === 3 ? data.readFloatLE(k * 4) : data.readInt32LE(k * 4) / 2147483648;
      (c ? R : L)[i] = v;
    }
  }
  return { sr: fmt.sr, L, R };
}

function writeWav(path, L, R) {
  const n = L.length, b = Buffer.alloc(44 + n * 4);
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 4, 4); b.write("WAVEfmt ", 8);
  b.writeUInt32LE(16, 16); b.writeUInt16LE(1, 20); b.writeUInt16LE(2, 22);
  b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 4, 28); b.writeUInt16LE(4, 32);
  b.writeUInt16LE(16, 34); b.write("data", 36); b.writeUInt32LE(n * 4, 40);
  for (let i = 0; i < n; i++) {
    b.writeInt16LE(Math.round(Math.max(-1, Math.min(1, L[i])) * 32767), 44 + i * 4);
    b.writeInt16LE(Math.round(Math.max(-1, Math.min(1, R[i])) * 32767), 46 + i * 4);
  }
  writeFileSync(path, b);
}

// ── the clock ────────────────────────────────────────────────────────────
// beats[] = the times hits land on in the *output* file's timeline, plus
// the audio that goes under them.
let beats, voice, mapTime = (s) => s;
if (MODE === "warp") {
  const src = readWav(TAKE);
  const spb = 60 / BPM;
  const t0 = SRC_BEATS[0];
  const map = SRC_BEATS.map((s, k) => [s, t0 + k * spb]);
  const endSrc = src.L.length / src.sr;
  const endDst = map.at(-1)[1] + (endSrc - map.at(-1)[0]);
  const mapPath = resolve(OUT, `timemap-${BPM}.txt`);
  writeFileSync(mapPath, map.map(([s, d]) => `${Math.round(s * src.sr)} ${Math.round(d * src.sr)}`).join("\n") + "\n");
  const warped = resolve(OUT, `take-warp-${BPM}.wav`);
  if (!existsSync(warped) || flags.rewarp) {
    console.log(`warping take → ${BPM} BPM grid (rubberband R3, timemap of ${map.length} beats)`);
    const r = spawnSync("rubberband", ["-3", "-F", "-D", endDst.toFixed(4), "-M", mapPath, TAKE, warped], { stdio: "inherit" });
    if (r.status !== 0) process.exit(1);
  }
  voice = readWav(warped);
  beats = SRC_BEATS.map((_, k) => t0 + k * spb);
  // Excerpt bounds follow the same map, so both drafts cover the same song.
  const toDst = (s) => { let k = SRC_BEATS.findIndex((b) => b > s); if (k <= 0) k = 1;
    const [s0, s1] = [SRC_BEATS[k - 1], SRC_BEATS[k]], [d0, d1] = [beats[k - 1], beats[k]];
    return d0 + ((s - s0) / (s1 - s0)) * (d1 - d0); };
  mapTime = toDst;
} else {
  voice = readWav(TAKE);
  beats = SRC_BEATS.slice();
}
const from = mapTime(FROM), to = mapTime(TO);

// ── harmony per bar (bar = 4 beats; downbeats are beats[4k]) ─────────────
const perBeat = [];
for (const r of A.chords.runs) for (let i = 0; i < r.beats; i++) perBeat.push(r.chord);
const ROOT = { E: 40, "G#": 44, "G#m": 44, B: 47, A: 45 };
function barChord(k) {
  // perBeat[i] covers bounds[i] = [0, ...beats][i]; beat j starts at perBeat[j+1].
  const tally = {};
  for (let j = 4 * k; j < 4 * k + 4; j++) {
    const c = (perBeat[j + 1] || "").replace(/(maj7|m7|7|sus4|sus2)$/, "");
    if (c in ROOT) tally[c] = (tally[c] || 0) + 1;
  }
  return Object.entries(tally).sort((a, b) => b[1] - a[1])[0]?.[0] || null;
}
// Pad voicings avoid the G#-vs-G#m third (unresolved by ear yet): G#
// chords get root–fifth–octave–ninth; E and B carry their thirds.
const PAD = { E: [52, 56, 59, 63], B: [59, 63, 66, 70], "G#": [56, 63, 68, 70], "G#m": [56, 63, 68, 70], A: [57, 61, 64, 68] };

// ── kit (booch inline synthesis, softened) ───────────────────────────────
const n = Math.ceil((to - from + 3) * SR);
const dL = new Float32Array(n), dR = new Float32Array(n);
const bus = new Float32Array(n);
const at = (t) => Math.floor((t - from) * SR);
function add(buf, i, v) { if (i >= 0 && i < buf.length) buf[i] += v; }
function kick(t, g) {
  const s0 = at(t); let ph = 0;
  for (let i = 0; i < 0.34 * SR; i++) { const u = i / SR;
    ph += (2 * Math.PI * (48 + 52 * Math.exp(-u * 30))) / SR;
    const v = Math.sin(ph) * Math.exp(-u * 8) * g; add(dL, s0 + i, v); add(dR, s0 + i, v); }
}
function snare(t, g) {
  const s0 = at(t); let prev = 0, lp = 0, ph = 0;
  for (let i = 0; i < 0.22 * SR; i++) { const u = i / SR, nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz; lp += (hp - lp) * 0.35; ph += (2 * Math.PI * 190) / SR;
    const v = (lp * Math.exp(-u * 18) * 1.3 + Math.sin(ph) * 0.4 * Math.exp(-u * 30)) * g;
    add(dL, s0 + i, v * 0.95); add(dR, s0 + i, v); }
}
function hat(t, g, pan) {
  const s0 = at(t); let prev = 0;
  for (let i = 0; i < 0.03 * SR; i++) { const nz = Math.random() * 2 - 1, hp = nz - prev; prev = nz;
    const v = hp * Math.exp(-(i / SR) * 240) * g; add(dL, s0 + i, v * (1 - pan)); add(dR, s0 + i, v * (1 + pan)); }
}

// ── arrangement: half-time ballad pocket ─────────────────────────────────
// At 123 the take *feels* ~61, so the backbeat snare goes on beat 3 of
// each 4-beat bar; kick on 1 and the "and" of 2; 8th hats, off-8ths
// quieter. Voice enters ~27s — before that, kick + bed only.
const VOICE_IN = mapTime(27);
const nBars = Math.floor(beats.length / 4);
for (let k = 0; k < nBars; k++) {
  const b = beats.slice(4 * k, 4 * k + 5);
  if (b.length < 5 || b[4] < from || b[0] > to) continue;
  const full = b[0] >= VOICE_IN - 0.5;
  const eighth = (j) => b[j] + (b[j + 1] - b[j]) / 2;
  kick(b[0], 0.9);
  if (full) {
    kick(eighth(1), 0.55);
    snare(b[2], 0.55);
    for (let j = 0; j < 4; j++) { hat(b[j], 0.16, -0.3); hat(eighth(j), 0.09, 0.3); }
  }
  // bed: chords change every 2 bars — voice the pad on bar starts.
  const c = barChord(k);
  if (!c) continue;
  const barDur = b[4] - b[0];
  mixEventSub({ startSec: b[0] - from, midi: ROOT[c] - 12 + TUNE, durSec: barDur * 0.95, gain: 0.5, preset: "funk" }, bus, { sampleRate: SR });
  for (const m of PAD[c]) {
    mixEventRhodes({ startSec: b[0] - from, midi: m + TUNE, durSec: barDur * 0.98, gain: 0.16, preset: "mellow" }, bus, { sampleRate: SR, preset: "mellow" });
  }
}

// ── mix: her take on top, bed under ──────────────────────────────────────
const L = new Float32Array(n), R = new Float32Array(n);
const vs = Math.floor(from * voice.sr);
for (let i = 0; i < n; i++) {
  const vi = vs + i, vL = voice.L[vi] || 0, vR = voice.R[vi] || 0;
  // 1.5s fades at both ends of the excerpt
  const fi = Math.min(1, i / (1.5 * SR), (n - 3 * SR - i) / (1.5 * SR) + 1);
  const f = Math.max(0, fi);
  L[i] = (vL * 1.6 + (dL[i] + bus[i] * 0.8) * BED) * f;
  R[i] = (vR * 1.6 + (dR[i] + bus[i] * 0.8) * BED) * f;
}
let peak = 0;
for (let i = 0; i < n; i++) peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
const g = 0.89 / (peak || 1);
for (let i = 0; i < n; i++) { L[i] *= g; R[i] *= g; }

const tag = MODE === "warp" ? `warp-${BPM}` : "follow";
const wav = resolve(OUT, `sailor-song-${tag}.wav`);
writeWav(wav, L, R);
spawnSync("ffmpeg", ["-v", "error", "-y", "-i", wav, "-b:a", "256k", wav.replace(/\.wav$/, ".mp3")]);
console.log(`✓ ${tag}: ${(n / SR).toFixed(1)}s → ${wav.replace(/\.wav$/, ".mp3")}`);
