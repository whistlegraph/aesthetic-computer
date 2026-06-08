#!/usr/bin/env node
// render-hopehop.mjs — render hopehop: C synth BED + freesound TRAP KIT.
//
// Pipeline (mirrors amaythingra's "C render + JS bake" split):
//   1. build + run c/hopehop.c          → melodic bed WAV (no drums)
//   2. load kit.json freesound samples   → 48k mono one-shots
//   3. sequence a trap drum/perc layer    → hard 808 kick, half-time
//      snare+clap on the 3, ratchet hi-hat rolls, layered perc
//   4. sidechain-duck the bed under the kick (trap pump)
//   5. bright summer master → mp3, play
//
// Run:
//   node pop/hopehop/bin/render-hopehop.mjs
//   node pop/hopehop/bin/render-hopehop.mjs --seed 7 --swing 0.66 --no-open
//   node pop/hopehop/bin/render-hopehop.mjs --out ~/hopehop.mp3
//   (refresh samples first with: node pop/hopehop/bin/fetch-kit.mjs)

import { existsSync, statSync, mkdirSync, readFileSync, writeFileSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir, tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const C_DIR = resolve(HERE, "..", "c");
const SRC = resolve(C_DIR, "hopehop.c");
const BIN = resolve(C_DIR, "hopehop");
const OUT_DIR = resolve(HERE, "..", "out");
const KIT_PATH = resolve(HERE, "..", "kit.json");
const expand = (p) => (p.startsWith("~") ? p.replace(/^~/, homedir()) : p);

const argv = process.argv.slice(2);
function takeFlag(n) { const i = argv.indexOf(n); if (i < 0) return null; const v = argv[i + 1]; argv.splice(i, 2); return v; }
const hasFlag = (n) => { const i = argv.indexOf(n); if (i < 0) return false; argv.splice(i, 1); return true; };

const noOpen = hasFlag("--no-open");
const outPath = (() => { const o = takeFlag("--out"); return o ? expand(o) : resolve(OUT_DIR, "hopehop.mp3"); })();
const SWING = parseFloat(takeFlag("--swing") ?? "0.60");
const SEED = parseInt(takeFlag("--seed") ?? "7", 10);
// jeffrey-pvc lead vocal stem (from bin/say.mjs). When present it becomes
// the lead: the Apple `say` adlibs + the panther HOOK melody drop out so
// jeffrey's voice carries the words (panther stays on the bass + drop roar).
const VOCAL_STEM = (() => { const v = takeFlag("--vocal-stem"); return v ? expand(v) : null; })();
const VOCAL_GAIN = parseFloat(takeFlag("--vocal-gain") ?? "0.95");
const VOCAL_OFFSET = takeFlag("--vocal-offset"); // seconds; default = end of 2-bar intro
// tempo — forwarded to the C bed so render + bed stay in lock-step. Lower
// BPM = slower, longer track (more room to stretch the sung vocal).
const BPM = parseFloat(takeFlag("--bpm") ?? "82");
mkdirSync(OUT_DIR, { recursive: true });

// ── transport (must match c/hopehop.c) ───────────────────────────────
const SR = 48000, BEAT = 60 / BPM, BAR = BEAT * 4, S16 = BEAT / 4;
// arrangement arc (bar ranges) — mirrors the C bed's SECTIONS for FLOW.
const ARC = [["intro", 2], ["groove", 8], ["hook", 8], ["break", 2], ["drop", 8], ["outro", 2]];
const SEC_OF = []; for (const [nm, bars] of ARC) for (let i = 0; i < bars; i++) SEC_OF.push(nm);
const t16 = (bar, s) => { const pb = Math.floor(s / 2) * 2; const off = (s & 1) ? 2 * SWING : 0; return bar * BAR + (pb + off) * S16; };
// deterministic RNG (mulberry32)
let _s = (SEED ^ 0x9e3779b9) >>> 0;
const rng = () => { _s |= 0; _s = (_s + 0x6d2b79f5) | 0; let t = Math.imul(_s ^ (_s >>> 15), 1 | _s); t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t; return ((t ^ (t >>> 14)) >>> 0) / 4294967296; };
const hr = (a) => (rng() * 2 - 1) * a;

// ── 1 · build + render the bed ───────────────────────────────────────
if (!existsSync(BIN) || statSync(BIN).mtimeMs < statSync(SRC).mtimeMs) {
  console.log("• building c/hopehop.c …");
  if (spawnSync("bash", [resolve(C_DIR, "build-hopehop.sh")], { stdio: "inherit" }).status !== 0) process.exit(1);
}
console.log("• rendering bed …");
const bedWav = resolve(OUT_DIR, "hopehop.bed.wav");
// the C bed is buffer-limited to --seconds; size it to the full 30-bar
// arrangement at THIS bpm so lower tempos don't get truncated (kept the
// render + bed bar grids in lock-step).
const ARR_BARS = ARC.reduce((a, [, b]) => a + b, 0);
const bedSeconds = (ARR_BARS * BAR + 0.5).toFixed(2);
if (spawnSync(BIN, ["--out", bedWav, "--swing", String(SWING), "--seed", String(SEED), "--bpm", String(BPM), "--seconds", bedSeconds, ...argv], { stdio: "inherit" }).status !== 0) process.exit(1);

// ── audio helpers ────────────────────────────────────────────────────
function decodeF32(path, channels) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-i", path,
    "-f", "f32le", "-ar", String(SR), "-ac", String(channels), "-"], { maxBuffer: 1 << 30 });
  if (r.status !== 0) throw new Error(`decode failed: ${path}\n${r.stderr}`);
  return new Float32Array(r.stdout.buffer, r.stdout.byteOffset, Math.floor(r.stdout.length / 4));
}
function loadOneShot(path) {
  let s = decodeF32(path, 1);
  // trim leading silence (tight transient) + normalize peak
  let start = 0; const TH = 0.004;
  while (start < s.length && Math.abs(s[start]) < TH) start++;
  s = s.subarray(Math.max(0, start - 4));
  let peak = 0; for (let i = 0; i < s.length; i++) peak = Math.max(peak, Math.abs(s[i]));
  if (peak > 0) { const g = 0.95 / peak; for (let i = 0; i < s.length; i++) s[i] *= g; }
  return s;
}

// decode bed → L/R
const bedI = decodeF32(bedWav, 2);
const N = Math.floor(bedI.length / 2);
const L = new Float32Array(N), R = new Float32Array(N);
for (let i = 0; i < N; i++) { L[i] = bedI[i * 2]; R[i] = bedI[i * 2 + 1]; }
const TOTAL_BARS = Math.floor(N / (BAR * SR));

// load kit
const kit = JSON.parse(readFileSync(KIT_PATH, "utf8"));
const S = {};
for (const role of Object.keys(kit)) { try { S[role] = loadOneShot(kit[role].path); } catch (e) { console.warn(`! ${role}: ${e.message}`); } }
console.log(`• kit: ${Object.keys(S).join(", ")}`);

// ── PANTHER as a pitched instrument ──────────────────────────────────
// The panther roar becomes the voice: pitch-shifted (resampled) copies
// sing the bassline and the hook. We detect the roar's natural pitch so
// the shifts land in-key, then resample per note. "pitch around the
// panther."  Tune with --panther-base <midi> / --panther-oct <n>.
const PANTHER_BASE = (() => { const v = takeFlag("--panther-base"); return v ? parseFloat(v) : null; })();
const PANTHER_OCT = parseInt(takeFlag("--panther-oct") ?? "0", 10);
function detectF0(s) {
  let pk = 0, pidx = 0; for (let i = 0; i < s.length; i++) { const a = Math.abs(s[i]); if (a > pk) { pk = a; pidx = i; } }
  const w = Math.min(8192, s.length), start = Math.max(0, Math.min(s.length - w, pidx - (w >> 1)));
  const minLag = Math.floor(SR / 320), maxLag = Math.floor(SR / 55);
  let best = -1, bestLag = 0;
  for (let lag = minLag; lag <= maxLag; lag++) {
    let sum = 0; for (let i = 0; i + lag < w; i++) sum += s[start + i] * s[start + i + lag];
    if (sum > best) { best = sum; bestLag = lag; }
  }
  return bestLag > 0 ? SR / bestLag : 0;
}
const panF0 = S.panther ? detectF0(S.panther) : 0;
const panBaseMidi = (PANTHER_BASE ?? (panF0 > 50 && panF0 < 320 ? 69 + 12 * Math.log2(panF0 / 440) : 48));
console.log(`• panther: f0≈${panF0.toFixed(1)}Hz → base midi ${panBaseMidi.toFixed(1)}`);
// generic linear-resample pitch shift (semitones), optional max length
function resample(s, semis, maxDurSec = Infinity) {
  const ratio = Math.pow(2, semis / 12);
  const outLen = Math.min(Math.floor(s.length / ratio), maxDurSec === Infinity ? Infinity : Math.floor(maxDurSec * SR));
  const out = new Float32Array(outLen);
  for (let i = 0; i < outLen; i++) { const sp = i * ratio, i0 = Math.floor(sp), fr = sp - i0, i1 = Math.min(s.length - 1, i0 + 1); out[i] = s[i0] * (1 - fr) + s[i1] * fr; }
  const fn = Math.floor(0.012 * SR); for (let i = 0; i < fn && i < outLen; i++) { out[i] *= i / fn; out[outLen - 1 - i] *= i / fn; }
  return out;
}
function trimFade(s, maxDurSec) {
  const len = Math.min(s.length, Math.floor(maxDurSec * SR));
  const out = s.slice(0, len);
  const an = Math.floor(0.006 * SR); for (let i = 0; i < an && i < len; i++) out[i] *= i / an;
  const fn = Math.floor(0.02 * SR); for (let i = 0; i < fn && i < len; i++) out[len - 1 - i] *= i / fn;
  return out;
}

// ── autotuned panther via WORLD (worldsing.py) ───────────────────────
// Locks the roar's pitch to an exact note (replaces f0 with a constant,
// scales down breath) so it SINGS in tune instead of wobbling. Disk-
// cached per note so reruns are instant. --no-autotune = raw resample.
const AUTOTUNE = !hasFlag("--no-autotune");
const PY = resolve(HERE, "..", "..", ".venv", "bin", "python");
const WORLDSING = resolve(HERE, "..", "..", "marimba", "bin", "worldsing.py");
const TUNE_DIR = resolve(OUT_DIR, "panther-tuned");
mkdirSync(TUNE_DIR, { recursive: true });
const panSrc = kit.panther?.path, panId = kit.panther?.id;
function midiToNote(m) { const NM = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]; m = Math.round(m); return NM[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1); }
const _tuned = new Map();
function tunedPanther(midi) {
  if (_tuned.has(midi)) return _tuned.get(midi);
  const note = midiToNote(midi), out = resolve(TUNE_DIR, `${panId}-${note}.wav`);
  if (!existsSync(out)) {
    const r = spawnSync(PY, [WORLDSING, panSrc, out, "--note", note, "--ap-scale", "0.12", "--unvoiced-gain", "0.05"], { stdio: ["ignore", "ignore", "pipe"] });
    if (r.status !== 0) { console.warn(`! worldsing ${note} failed`); _tuned.set(midi, null); return null; }
  }
  let s = null; try { s = loadOneShot(out); } catch {}
  _tuned.set(midi, s); return s;
}
const _panRaw = new Map();
function placePanther(midi, startSec, maxDurSec, gain, pan) {
  if (!S.panther) return;
  let src = null;
  if (AUTOTUNE) { const t = tunedPanther(midi); if (t) src = trimFade(t, maxDurSec); }
  if (!src) { const k = `${midi}|${maxDurSec}`; src = _panRaw.get(k) || resample(S.panther, midi - panBaseMidi, maxDurSec); _panRaw.set(k, src); }
  place(src, startSec, gain, pan);
}

// ── Apple `say` saucy vocal adlibs ───────────────────────────────────
// macOS TTS → per-word clips, scattered + pitched through the hook/drop
// as a saucy adlib track. Tune the line with --say "..." / voice with
// --say-voice <name>.
const SAY_VOICE = takeFlag("--say-voice") ?? "Samantha";
const SAY_LINE = takeFlag("--say") ?? "want my,latina,gimme,thicc,milkshake,milkshake,mmm yeah";
// when a jeffrey-pvc stem leads, skip the Apple `say` adlib synth entirely
const ADLIBS = VOCAL_STEM ? [] : SAY_LINE.split(",").map((w) => w.trim()).filter(Boolean);
const SAY_DIR = resolve(OUT_DIR, "say-tuned"); mkdirSync(SAY_DIR, { recursive: true });
const slugw = (t) => t.replace(/\W+/g, "_");
function loadSafe(p) { try { return loadOneShot(p); } catch { return null; } }
function sayWord(text) {
  const aiff = resolve(tmpdir(), `hh-say-${slugw(text)}.aiff`);
  if (spawnSync("say", ["-v", SAY_VOICE, "-o", aiff, text], { stdio: "ignore" }).status !== 0) return null;
  const wav = aiff.replace(/\.aiff$/, ".wav");
  if (spawnSync("ffmpeg", ["-y", "-loglevel", "error", "-i", aiff, "-ar", "44100", "-ac", "1", wav], { stdio: "ignore" }).status !== 0) return null;
  return wav;
}
const sayPaths = ADLIBS.map(sayWord);
const sayRaw = sayPaths.map((p) => (p ? loadSafe(p) : null));
console.log(`• say-vocal: ${sayPaths.filter(Boolean).length}/${ADLIBS.length} adlibs (voice ${SAY_VOICE}, autotuned)`);
// autotune (WORLD pitch-lock) a say word to a NOTE — higher ap/unvoiced
// than the panther so the words stay intelligible while singing in tune.
const _sayTuned = new Map();
function tunedSay(idx, midi) {
  const key = `${idx}|${midi}`; if (_sayTuned.has(key)) return _sayTuned.get(key);
  const src = sayPaths[idx]; if (!src) { _sayTuned.set(key, null); return null; }
  const note = midiToNote(midi), out = resolve(SAY_DIR, `${slugw(ADLIBS[idx])}-${note}.wav`);
  if (!existsSync(out)) {
    const r = spawnSync(PY, [WORLDSING, src, out, "--note", note, "--ap-scale", "0.45", "--unvoiced-gain", "0.7"], { stdio: ["ignore", "ignore", "pipe"] });
    if (r.status !== 0) { _sayTuned.set(key, null); return null; }
  }
  const s = loadSafe(out); _sayTuned.set(key, s); return s;
}
// vocal HOOK melody — the words SING this line (NOT random pitches),
// autotuned per note. Loop-bar indexed [step, midi].
const VOCAL_MEL = {
  0: [[4, 67], [10, 69]], 1: [[4, 65]],
  2: [[4, 64], [10, 67]], 3: [[2, 69]],
  4: [[4, 67], [10, 72]], 5: [[4, 69]],
  6: [[2, 71], [8, 67]], 7: [[4, 65]],
};
// place a vocal with a hip-hop slap "throw" so it sits into the mix
function placeVox(s, startSec, gain, pan) {
  place(s, startSec, gain, pan);
  place(s, startSec + BEAT * 0.5, gain * 0.38, -pan * 0.8);   // 1/8 throw
  place(s, startSec + BEAT * 1.0, gain * 0.18, pan * 0.5);    // 1/4 tail
}
let adlibIdx = 0;

// panther voicing data (loop-bar indexed). bass = chord roots; the hook
// is the melodic line the lead will eventually follow.
const PAN_ROOT = [41, 45, 38, 43, 41, 40, 45, 43];          // F A D G F E A G
const PAN_HOOK = {                                           // [step, midi, maxBeats]
  0: [[4, 53, 1.4], [10, 57, 1.0]], 1: [[4, 55, 1.6]],
  2: [[4, 53, 1.0], [10, 60, 1.2]], 3: [[2, 62, 1.4]],
  4: [[4, 53, 1.4], [10, 57, 1.0]], 5: [[4, 55, 1.6]],
  6: [[2, 60, 1.0], [8, 64, 1.2]], 7: [[4, 55, 1.8]],
};

// ── trap drum/perc sequencer ─────────────────────────────────────────
const duck = new Float32Array(N).fill(1); // sidechain envelope on the bed
function place(sample, startSec, gain, pan, isKick = false) {
  if (!sample) return;
  const i0 = Math.round(startSec * SR);
  const a = (pan * 0.5 + 0.5) * (Math.PI / 2), gL = Math.cos(a) * gain, gR = Math.sin(a) * gain;
  for (let j = 0; j < sample.length; j++) { const i = i0 + j; if (i < 0 || i >= N) break; L[i] += sample[j] * gL; R[i] += sample[j] * gR; }
  if (isKick) { // carve a DEEP pump under the kick (sidechain "wiggle")
    const dur = 0.22 * SR;
    for (let j = 0; j < dur; j++) { const i = i0 + j; if (i < 0 || i >= N) break; const p = j / dur; const d = 0.42 + 0.58 * p * p; if (d < duck[i]) duck[i] = d; }
  }
}

// hi-hat with ratchet rolls: subdiv quick hits across one swung 16th cell
function hatCell(bar, s, baseVel, subdiv) {
  const t0 = t16(bar, s), t1 = t16(bar, (s + 1) % 16) + ((s + 1) >= 16 ? BAR : 0);
  const span = (t1 > t0 ? t1 - t0 : S16);
  for (let k = 0; k < subdiv; k++) {
    const v = baseVel * (subdiv > 1 ? 0.7 + 0.3 * (k / Math.max(1, subdiv - 1)) : 1) * (0.85 + 0.3 * rng());
    place(S.hat, t0 + (span * k) / subdiv + hr(0.003), 0.30 * v, hr(0.3));
  }
}

// synth swell riser (cute noise crescendo that LANDS on endSec)
function riser(endSec, durSec, gain) {
  const i1 = Math.round(endSec * SR), i0 = Math.round((endSec - durSec) * SR);
  let lp = 0;
  for (let i = Math.max(0, i0); i < Math.min(N, i1); i++) {
    const p = (i - i0) / (i1 - i0);
    const nz = rng() * 2 - 1; lp += (nz - lp) * (0.03 + 0.4 * p);
    const env = p * p * gain;
    const a = (hr(0.3) * 0.5 + 0.5) * (Math.PI / 2);
    L[i] += lp * env * Math.cos(a); R[i] += lp * env * Math.sin(a);
  }
}

// ── soft LEAD SYNTH — doubles/answers the sung pentatonic melody ──────
// Mellow sine+harmonics with a slow attack + exponential body and a touch
// of vibrato; sits an octave above the vocal (C4–C5) so the two lines
// braid. A short stereo slap gives it air. Kept gentle so the voice leads.
const mtof = (m) => 440 * Math.pow(2, (m - 69) / 12);
function leadNote(midi, startSec, durSec, gain, pan) {
  const f = mtof(midi), i0 = Math.round(startSec * SR), nLen = Math.floor(durSec * SR);
  const atkN = Math.floor(0.018 * SR), relN = Math.floor(0.12 * SR);
  const a = (pan * 0.5 + 0.5) * (Math.PI / 2), gL = Math.cos(a) * gain, gR = Math.sin(a) * gain;
  for (let j = 0; j < nLen; j++) {
    const i = i0 + j; if (i < 0 || i >= N) break;
    const t = j / SR;
    const vib = 1 + 0.006 * Math.sin(2 * Math.PI * 5.2 * t) * Math.min(1, t / 0.18); // gentle delayed vibrato
    const ph = 2 * Math.PI * f * vib * t;
    let s = Math.sin(ph) + 0.3 * Math.sin(2 * ph) + 0.12 * Math.sin(3 * ph); // soft, few harmonics
    s *= 0.7;
    let env = Math.exp(-2.0 * t);                       // mellow body decay
    if (j < atkN) env *= j / atkN;                      // soft attack
    if (j > nLen - relN) env *= (nLen - j) / relN;      // release tail
    L[i] += s * env * gL; R[i] += s * env * gR;
  }
}
// lead phrase per loop-bar: [step, midi, beats] — C-major pentatonic in
// C4–C5, a call line that complements the vocal contour, resolving to C4.
const LEAD_MEL = {
  0: [[0, 67, 1.0], [8, 64, 1.0]], 1: [[4, 69, 1.2]],
  2: [[0, 72, 1.0], [8, 69, 1.0]], 3: [[4, 67, 1.4]],
  4: [[0, 64, 1.0], [8, 67, 1.0]], 5: [[4, 62, 1.2]],
  6: [[0, 64, 1.0], [8, 67, 1.0]], 7: [[4, 60, 1.6]],
};

console.log("• sequencing trap layer (chill / flow-aware) …");
for (let bar = 0; bar < TOTAL_BARS; bar++) {
  const lb = bar % 8, phraseEnd = lb === 7;
  const sec = SEC_OF[bar] || "drop";
  const isDrop = sec === "drop", isBreak = sec === "break", isIntro = sec === "intro", isOutro = sec === "outro";

  // BREAK = breather: drums drop out, bed washes; riser swells into the drop
  if (isBreak) {
    for (const s of [2, 6, 10, 14]) place(S.shaker, t16(bar, s) + hr(0.004), 0.2 * (0.8 + 0.4 * rng()), hr(0.5));
    if (bar === SEC_OF.indexOf("break") + 1) riser(t16(bar + 1, 0), BAR * 1.6, 0.5); // land on the drop
    continue;
  }

  // intro builds; outro thins out; drop is fullest. Density scales it all.
  const build = isIntro ? (lb >= 1 ? 0.7 : 0.4) : 1.0;
  const energy = isDrop ? 1.0 : 0.85;

  // KICK — softer 808 (less "war war"), syncopated, lighter in intro/outro
  const kicks = new Set([0, 6, 10]);
  if (!isOutro && rng() < 0.4) kicks.add(13);
  if (isDrop && rng() < 0.3) kicks.add(11);
  if (isOutro) { kicks.clear(); kicks.add(0); kicks.add(10); }
  for (const s of kicks) place(S.kick, t16(bar, s) + hr(0.003), 0.82 * build, 0, true);

  // BACKBEAT — snare + clap on beat 3 (half-time), gentler
  if (!(isIntro && lb === 0)) {
    place(S.snare, t16(bar, 8) + hr(0.003), 0.62 * build, 0.0);
    place(S.clap, t16(bar, 8) + hr(0.004), 0.42 * build, 0.0);
  }
  if (phraseEnd && !isOutro) place(S.snare, t16(bar, 14) + hr(0.003), 0.34, 0.05);

  // HI-HATS — laid-back 16ths, FEWER rolls (less aggressive)
  const rolls = new Map();
  if (!isOutro && rng() < 0.3) rolls.set(7, 2);
  if (isDrop && rng() < 0.25) rolls.set(11, rng() < 0.5 ? 3 : 2);
  if (phraseEnd && isDrop) { rolls.set(14, 2); rolls.set(15, 3); }
  const hatGain = (isOutro ? 0.6 : 1.0) * energy;
  for (let s = 0; s < 16; s++) {
    if (isOutro && s % 2) continue;             // thin to 8ths in the outro
    const accent = (s % 4 === 2) ? 0.9 : (s % 2 === 0 ? 0.7 : 0.5);
    hatCell(bar, s, accent * hatGain * build, rolls.get(s) || 1);
  }
  if ((lb === 1 || lb === 5) && !rolls.has(14)) place(S.ohat, t16(bar, 14) + hr(0.003), 0.24, 0.25);

  // PERC — native-tongues hand percussion: shaker + rim + conga/quinto,
  // tambourine jangle, bongo syncopation. panned, kept gentle.
  for (const s of [2, 6, 10, 14]) place(S.shaker, t16(bar, s) + hr(0.004), 0.26 * (0.8 + 0.4 * rng()) * build, hr(0.5));
  for (const s of (rng() < 0.5 ? [3, 11] : [3, 11, 6])) place(S.rim, t16(bar, s) + hr(0.004), 0.3 * (0.8 + 0.4 * rng()) * build, hr(0.55));
  if (!isOutro && rng() < 0.6) place(S.perc, t16(bar, rng() < 0.5 ? 7 : 13) + hr(0.005), 0.3 * build, hr(0.45));
  // tambourine — sparse, soft jangle on the backbeat (chill), right side
  if (S.tamb && !isIntro && !isOutro) {
    const tsteps = (sec === "hook" || isDrop) ? [8, 12] : [8];
    for (const s of tsteps) if (rng() < 0.6) place(S.tamb, t16(bar, s) + hr(0.005), 0.10 * (0.8 + 0.4 * rng()) * build, 0.4 + hr(0.15));
  }
  // bongo — syncopated answer hits, panned opposite the conga
  if (S.bongo && (sec === "groove" || sec === "hook" || isDrop) && rng() < 0.55)
    place(S.bongo, t16(bar, [5, 7, 13, 15][Math.floor(rng() * 4)]) + hr(0.005), 0.26 * build, -0.4 + hr(0.15));
  // soft cymbal to mark the drop's downbeat (gentle, not a crash-out)
  if (isDrop && lb === 0) place(S.crash, t16(bar, 0), 0.28, 0.0);

  // ── LEAD SYNTH — braids with the vocal melody through hook + drop ───
  if ((sec === "hook" || isDrop) && !isBreak) {
    for (const [s, midi, beats] of (LEAD_MEL[lb] || [])) {
      if (rng() < 0.15) continue;                    // occasional breath
      const g = (isDrop ? 0.2 : 0.16) * build;
      leadNote(midi, t16(bar, s) + hr(0.004), beats * BEAT, g, hr(0.25));
      leadNote(midi, t16(bar, s) + 0.11 + hr(0.004), beats * BEAT * 0.7, g * 0.3, -0.3); // stereo slap
    }
  }

  // ── PANTHER VOICE — sings the bass + the hook ──────────────────────
  const po = PANTHER_OCT * 12;
  if (sec === "groove" || sec === "hook" || isDrop) {
    // panther bass — low growl on the roots
    placePanther(PAN_ROOT[lb] + po, t16(bar, 0) + hr(0.004), 1.7 * BEAT, 0.55, 0);
    if (rng() < 0.6) placePanther(PAN_ROOT[lb] + po, t16(bar, 8) + hr(0.004), 1.0 * BEAT, 0.4, 0);
  }
  if (!VOCAL_STEM && (sec === "hook" || isDrop)) {
    // panther hook — the melodic lead (pitched up into a growl-sing).
    // Silenced when jeffrey's vocal leads, so the voices don't fight.
    for (const [s, midi, mb] of (PAN_HOOK[lb] || []))
      placePanther(midi + po, t16(bar, s) + hr(0.004), mb * BEAT, 0.5, hr(0.2));
  }
  // dramatic full roar announcing the drop
  if (isDrop && lb === 0) placePanther(PAN_ROOT[0] + po, t16(bar, 0) - 0.04, 3.0, 0.62, 0);

  // ── saucy say-vocal — autotuned, SINGS the vocal hook melody ───────
  if (sayPaths.some(Boolean) && (sec === "hook" || isDrop)) {
    for (const [step, midi] of (VOCAL_MEL[lb] || [])) {
      if (sec === "hook" && rng() < 0.45) continue;            // sparser in the first hook
      const idx = adlibIdx++ % ADLIBS.length;
      let s = tunedSay(idx, midi);
      if (!s && sayRaw[idx]) s = resample(sayRaw[idx], midi - 60);   // fallback
      if (s) placeVox(trimFade(s, 1.3), t16(bar, step) + hr(0.008), 0.52, hr(0.35));
    }
  }

  // ── record scratches — phrase transitions + drop accents ───────────
  if (S.scratch && phraseEnd && !isOutro) place(S.scratch, t16(bar, 12) + hr(0.005), 0.45, hr(0.4));
  if (S.scratch2 && isDrop && rng() < 0.3) place(S.scratch2, t16(bar, [3, 7, 11][Math.floor(rng() * 3)]) + hr(0.005), 0.38, hr(0.5));
}

// ── sidechain pump (gentle — chill, not a hard wiggle) ───────────────
for (let i = 0; i < N; i++) { const d = 0.66 + 0.34 * duck[i]; L[i] *= d; R[i] *= d; }

// ── filter + volume automation (FLOW) ────────────────────────────────
// One-pole LP whose cutoff opens over the intro and dips in the break,
// plus a volume swell in/out — gives the track an arc instead of a flat loop.
const introEnd = 2 * BAR, brkStart = 18 * BAR, brkEnd = 20 * BAR, outroStart = 28 * BAR, end = 30 * BAR;
const lerp = (a, b, t) => a + (b - a) * Math.max(0, Math.min(1, t));
function cutoff(t) {
  if (t < introEnd) return 700 * Math.pow(20000 / 700, t / introEnd);     // open up
  if (t >= brkStart && t < brkStart + BAR) return lerp(20000, 1400, (t - brkStart) / BAR);
  if (t >= brkStart + BAR && t < brkEnd) return lerp(1400, 20000, (t - (brkStart + BAR)) / BAR);
  return 20000;
}
function autoGain(t) {
  if (t < introEnd) return lerp(0.55, 1.0, (t / introEnd) ** 0.5);
  if (t >= brkStart && t < brkEnd) return lerp(1.0, 0.82, Math.sin(Math.PI * (t - brkStart) / (brkEnd - brkStart)));
  if (t >= outroStart) return lerp(1.0, 0.0, (t - outroStart) / (end - outroStart)); // fade out
  return 1.0;
}
let lpL = 0, lpR = 0;
for (let i = 0; i < N; i++) {
  const t = i / SR;
  const a = 1 - Math.exp(-2 * Math.PI * cutoff(t) / SR);
  lpL += a * (L[i] - lpL); lpR += a * (R[i] - lpR);
  const g = autoGain(t);
  L[i] = lpL * g; R[i] = lpR * g;
}

// ── jeffrey-pvc lead vocal — mixed clean over the finished bed ───────
// Mixed AFTER the sidechain pump + filter automation so the voice stays
// present and un-ducked (the ffmpeg master still EQs it). Starts at the
// end of the 2-bar intro by default; lands the first "hope" on the groove.
if (VOCAL_STEM) {
  if (!existsSync(VOCAL_STEM)) { console.error(`✗ vocal stem not found: ${VOCAL_STEM}`); process.exit(1); }
  const vI = decodeF32(VOCAL_STEM, 2);
  const vN = Math.floor(vI.length / 2);
  // ElevenLabs stems run quiet (~ -33 dB mean). Peak-normalize so VOCAL_GAIN
  // is meaningful, then mix the voice forward and DUCK the bed under it.
  let vpk = 0; for (let j = 0; j < vN * 2; j++) { const a = Math.abs(vI[j]); if (a > vpk) vpk = a; }
  const norm = vpk > 0 ? 0.97 / vpk : 1;
  const off = Math.round((VOCAL_OFFSET != null ? parseFloat(VOCAL_OFFSET) : 2 * BAR) * SR);
  const fadeN = Math.floor(0.01 * SR);
  // envelope follower on the (normalized) vocal → ducks the instrumental
  const atk = Math.exp(-1 / (0.005 * SR)), rel = Math.exp(-1 / (0.18 * SR));
  let venv = 0;
  console.log(`• mixing jeffrey-pvc vocal: ${VOCAL_STEM.split("/").pop()} (peak-norm ×${norm.toFixed(2)} · gain ${VOCAL_GAIN} · duck bed)`);
  for (let j = 0; j < vN; j++) {
    const i = off + j; if (i < 0 || i >= N) break;
    const vl = vI[j * 2] * norm, vr = vI[j * 2 + 1] * norm;
    const mag = Math.max(Math.abs(vl), Math.abs(vr));
    venv = mag > venv ? atk * venv + (1 - atk) * mag : rel * venv + (1 - rel) * mag;
    const dip = 1 - 0.45 * Math.min(1, venv / 0.5);     // duck bed up to 45% under voice
    let env = VOCAL_GAIN;
    if (j < fadeN) env *= j / fadeN;                     // tiny in/out fades to avoid clicks
    if (j > vN - fadeN) env *= (vN - j) / fadeN;
    L[i] = L[i] * dip + vl * env; R[i] = R[i] * dip + vr * env;
  }
}

// ── write mixed f32 → warm (less-bright, less-loud) master → mp3 ──────
const mix = new Float32Array(N * 2);
for (let i = 0; i < N; i++) { mix[i * 2] = L[i]; mix[i * 2 + 1] = R[i]; }
const tmpRaw = resolve(tmpdir(), `hopehop-${SEED}.f32`);
writeFileSync(tmpRaw, Buffer.from(mix.buffer));

// CHILL master: warm low-mids, mud trimmed gently, harsh presence + air
// rolled OFF (soft / lo-fi), easy glue, quiet + dynamic loudness so the
// track breathes instead of squashing flat.
const MASTER = [
  "highpass=f=32",
  "equalizer=f=60:t=q:w=0.8:g=1.6",      // gentle sub warmth
  "equalizer=f=200:t=q:w=1.0:g=1.1",     // low-mid body / coziness
  "equalizer=f=320:t=q:w=1.1:g=-1.0",    // light mud trim
  "equalizer=f=3400:t=h:w=0.8:g=-0.8",   // gently tame harsh presence (still smooth)
  "equalizer=f=8000:t=h:w=0.7:g=1.2",    // restore air → harmonies shimmer (emo sheen)
  "equalizer=f=12000:t=h:w=0.7:g=1.0",   // top sparkle
  "acompressor=threshold=-21dB:ratio=2.4:attack=15:release=200:makeup=2.2",  // tighter glue
  "alimiter=limit=0.97:level=false",
  "loudnorm=I=-10:TP=-1.0:LRA=9",         // loud, lush emo-rap level
].join(",");

const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", tmpRaw,
  "-af", MASTER, "-c:a", "libmp3lame", "-b:a", "256k", outPath], { stdio: "inherit" });
try { unlinkSync(tmpRaw); } catch {}
try { unlinkSync(bedWav); } catch {}
if (ff.status !== 0) { console.error("✗ master failed"); process.exit(1); }
console.log(`✓ ${outPath}  (${TOTAL_BARS} bars · trap kit + C bed · seed ${SEED})`);

if (noOpen) process.exit(0);
spawnSync("osascript", ["-e", `
tell application "QuickTime Player"
  if running then close every document
  open POSIX file ${JSON.stringify(outPath)}
  play (front document)
  activate
end tell`], { stdio: "ignore" });
