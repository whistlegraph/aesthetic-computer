#!/usr/bin/env node
// render3.mjs — "whistlegraph cult --- remix (v3, chorus first)"
//
// v2 was the cut @jeffrey called sick. v3 is a targeted revision of it, not
// a rebuild: same sound, same harmony pool, same mixing law, same WORLD
// speech-to-singing. What changed is the FORM and the VOCAL TREATMENT, on
// nine notes he gave in two batches.
//
//   1. "can we start the track off from the chorus"  → COLD OPEN. Bar 0,
//      beat 1, the first thing you hear is "run real fast". v2 spent 48
//      seconds getting to its hook; v3 spends none.
//   2. "and then lets play up that"                  → bars 8–24 escalate
//      the same eight bars — octave-mixed at 8, then torn down to nothing
//      but held dashes at 16. v2's drone / pulse / morse are still here,
//      but they now arrive at 0:48 as the RELEASE from the chorus rather
//      than the runway into it.
//   3. the chorus, dictated line by line:
//         run real fast / i wanna hide away / i wanna dash / dot dot dash
//      "run it fast" became "run real fast"; "i wanna hide away" is new.
//      Both are new ElevenLabs jeffrey-pvc lines, sung through sing.py.
//   6. "would be cool if we could mix the octaves"   → every lead line is
//      rendered at two octaves and the arrangement moves BETWEEN them.
//      The held dashes put Camille/Alex on top and Jeffrey — whose chant
//      hit was already B2 — an octave under: three real people, two
//      octaves, one syllable.
//   7. "spatialize like special sines spatialize the vocals" → the Special
//      Sign topology, now with its own copy on the VOICES. Direct sung
//      voice stays dry on a stable equal-power pan; a band-limited
//      80 Hz–11.5 kHz ear model returns ANTISYMMETRICALLY (L=+, R=−) on a
//      breathing send. Mono-safe by construction — the antisymmetric part
//      cancels on fold-down and the intelligible dry pan is what survives.
//   8. "side kick / side chain the vocals like daaaaaaaaaash with the kick
//      and snare" → the held words pump. Against kick AND snare, ~9 ms
//      ramp, never a step. The trick that keeps the words readable while
//      they pump: the duck is applied per VOICE with an onset ramp, so a
//      word's attack lands unducked and only its sustain breathes. You
//      hear "DASH" and then you hear it pumping.
//   9. "i'd love some switch ups / change ups on the chorus" → six chorus
//      statements, no two the same. See CHORUS_TAKES.
//  10. a "dasssh only" break (bars 16–24, reprised 108–116) and a "dot
//      only" section (bars 56–64). One is all sustain and pump; the other
//      is all staccato and dry. They are the two halves of morse played as
//      two halves of a track.
//
// Everything from v2's quality bar is carried: 10 ms raised-cosine tails,
// ramped ducks, no master tanh, sine-bump bass, no presence/air boost,
// mono-safe pans, dotted-eighth dub delay, measure → static gain → limiter.
//
//   node pop/cult/bin/render3.mjs           # → out/cult-remix-v3.wav
//   node pop/cult/bin/render3.mjs --stems   # + bus stems
//   ./pop/cult/bin/render-v3.sh             # + master to mp3 / 24-bit

import { existsSync, mkdirSync, readdirSync, writeFileSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const OUT = resolve(LANE, "out");
mkdirSync(OUT, { recursive: true });

const SR = 48_000;
const BPM = 120;
const BEAT = 60 / BPM;               // 0.5 s
const BAR = 4 * BEAT;                // 2.0 s
const BARS = 76;                     // 152 s = 2:32 — every part kept, all compressed
const TAU = Math.PI * 2;
const N = Math.round((BARS * BAR + 3.2) * SR);

// Buses. Float32 throughout — at these magnitudes the accumulated error
// sits near -140 dBFS and the arrays are half the size, which matters at
// four minutes of five stereo buses.
const musicL = new Float32Array(N), musicR = new Float32Array(N);
const drumsL = new Float32Array(N), drumsR = new Float32Array(N);
const voxL = new Float32Array(N), voxR = new Float32Array(N);
const pumpL = new Float32Array(N), pumpR = new Float32Array(N);
const sideB = new Float32Array(N);   // bed's Special Sign side field
const sideV = new Float32Array(N);   // voices' — note 7
const sideP = new Float32Array(N);   // the pumped share of the voices'
const dlySend = new Float32Array(N);

const VOXG = 1.42;                   // +3 dB on the sung bus, measured in v2
const PUMPG = 1.62;                  // +4.2 dB makeup on the pumped share
const clamp = (v, a, b) => (v < a ? a : v > b ? b : v);
const hz = (midi) => 440 * Math.pow(2, (midi - 69) / 12);
const at = (bar, beat = 0) => bar * BAR + beat * BEAT;

// The click amnesty — every voice exits through 10 ms of raised cosine.
const tailFade = (i, n) => {
  const u = (n - 1 - i) / (0.010 * SR);
  return u >= 1 ? 1 : u <= 0 ? 0 : u * u * (3 - 2 * u);
};

// ── sample bank ───────────────────────────────────────────────────────
const BANK = {};
function load(name, path) {
  if (!existsSync(path)) { console.warn(`  ! missing sample ${path}`); return; }
  let p = path;
  if (!path.endsWith(".wav")) {
    p = join(OUT, `.cache-${name}.wav`);
    if (!existsSync(p))
      execFileSync("ffmpeg", ["-y", "-v", "error", "-i", path, "-ac", "1", "-ar", String(SR), p]);
  }
  const { samples, sampleRate } = readWavMono(p);
  if (sampleRate !== SR) {
    const q = join(OUT, `.cache-${name}-rs.wav`);
    execFileSync("ffmpeg", ["-y", "-v", "error", "-i", p, "-ac", "1", "-ar", String(SR), q]);
    BANK[name] = readWavMono(q).samples;
  } else BANK[name] = samples;
  const s = BANK[name];
  let a = 0; while (a < s.length - 1 && Math.abs(s[a]) < 0.008) a++;
  const t = s.subarray(Math.max(0, a - Math.round(0.002 * SR)));
  let peak = 0; for (const v of t) peak = Math.max(peak, Math.abs(v));
  const g = peak > 1e-6 ? 0.95 / peak : 1;
  const out = new Float32Array(t.length);
  for (let i = 0; i < t.length; i++) out[i] = t[i] * g;
  BANK[name] = out;
}

for (const dir of ["samples", "sung"]) {
  const d = resolve(LANE, dir);
  if (!existsSync(d)) continue;
  for (const f of readdirSync(d)) if (f.endsWith(".wav")) load(f.replace(/\.wav$/, ""), join(d, f));
}
const DEMOS = resolve(REPO, "pop/demos/samples");
for (const [n, f] of Object.entries({
  hatC: "perc-hat-c.mp3", hatO: "perc-hat-o.mp3", clap: "perc-clap.mp3",
  ride: "perc-ride.mp3", snap: "perc-snap.mp3", snare: "perc-snare.mp3",
  tambo: "perc-tambo.mp3", block: "perc-block.mp3", sweep: "bed-noise-sweep.mp3",
})) load(n, join(DEMOS, f));

// ── form ──────────────────────────────────────────────────────────────
// The chorus is first, and it is the spine: six statements with drone,
// morse, dash-only and dot-only material threaded between them.
const S = {
  chorus1: [0, 8],      // COLD OPEN — the hook, bar 0 beat 1
  chorus2: [8, 12],     // the play-up: both octaves, full kit, double-time tag
  dashes: [12, 16],     // "dasssh only" — all sustain, all pump
  air: [16, 20],        // release: the sung "cult" drone, no drums
  pulse: [20, 24],      // kick and sub return under the drone
  morse: [24, 28],      // the SOS figure
  chorus3: [28, 32],    // chorus inverted — low lead, high dashes answering
  dots: [32, 36],       // "dot only" — staccato, dry, three octaves
  chorus4: [36, 40],    // chorus stripped: lines 1 and 4 only
  hollow: [40, 44],     // "the three of us are in a CULT", kick out
  drift: [44, 48],      // harmonic travel, held dashes as pads
  chorus5: [48, 56],    // fullest
  chorus6: [56, 64],    // climax — the double-time "run real fast"
  dashout: [64, 68],    // dash-only reprise, elements peeling away
  ebb: [68, 72],        // drums out, the original spoken chant returns
  out: [72, 76],        // the drone walks out the way it walked in
};
const ORDER = Object.keys(S);
const inS = (bar, k) => bar >= S[k][0] && bar < S[k][1];
const sectionAt = (bar) => ORDER.find((k) => inS(bar, k)) ?? "out";
const CHORUSES = ["chorus1", "chorus2", "chorus3", "chorus4", "chorus5", "chorus6"];
const isChorus = (bar) => CHORUSES.includes(sectionAt(bar));

const NO_KICK = new Set(["air", "hollow", "ebb", "out"]);
const kickOn = (b) => !NO_KICK.has(sectionAt(b));
// Snare on 2 and 4 — quiet, and mostly there to give the vocal pump a
// second trigger. It appears only where the pump is the point.
const SNARE = new Set(["chorus2", "dashes", "chorus5", "chorus6", "dashout"]);
const snareOn = (b) => SNARE.has(sectionAt(b)) && !(sectionAt(b) === "dashout" && b >= S.dashout[0] + 4);
const hatOn = (b) => kickOn(b) || inS(b, "hollow");
const dense = (b) => ["chorus2", "chorus5", "chorus6", "dots"].includes(sectionAt(b));

// ── space ─────────────────────────────────────────────────────────────
function spatial(az) {
  const itd = Math.round(0.00027 * SR * Math.sin(az));
  const shadow = 0.35 * Math.sin(az);
  return { itd, gl: 1 - shadow, gr: 1 + shadow };
}

// `w` splits a voice between its unpumped and pumped shares. The pumped
// share is multiplied by the kick+snare envelope at sum time, which is
// exactly per-sample ducking without needing a second scoring pass.
function emit(bus, i, mono, pan, sp, sideAmt, dly = 0, w = 0) {
  if (i < 0 || i >= N) return;
  const a = (Math.PI / 4) * (1 + pan);
  const cl = Math.cos(a), cr = Math.sin(a);
  if (bus === "drums") { drumsL[i] += mono * cl; drumsR[i] += mono * cr; }
  else if (bus === "vox") {
    const dry = mono * (1 - w), wet = mono * w;
    voxL[i] += dry * cl; voxR[i] += dry * cr;
    pumpL[i] += wet * cl; pumpR[i] += wet * cr;
  } else { musicL[i] += mono * cl; musicR[i] += mono * cr; }
  if (dly) dlySend[i] += mono * dly;
  if (sp && sideAmt) {
    const li = i + sp.itd, ri = i - sp.itd;
    const l = li >= 0 && li < N ? mono * sp.gl : 0;
    const r = ri >= 0 && ri < N ? mono * sp.gr : 0;
    const s = 0.5 * (l - r) * sideAmt;
    if (bus === "vox") { sideV[i] += s * (1 - w); sideP[i] += s * w; }
    else sideB[i] += s;
  }
}

// ── sidechain ─────────────────────────────────────────────────────────
// Two envelopes, both precomputed rather than searched per sample: the
// bed's breath (kick only, v2's depth) and the vocal pump (kick AND snare,
// deeper). Both ramp to the floor over ~9 ms — a stepped duck is a click
// with extra steps — and recover on a smoothstep.
const kicks = [], snares = [];
function buildEnv(triggers) {
  const e = new Float32Array(N).fill(1);
  const pre = Math.round(0.010 * SR);
  for (const { t, depth, atk, rel } of triggers) {
    const i0 = Math.round(t * SR) - pre;
    const span = pre + Math.round((atk + rel) * SR);
    for (let i = 0; i < span; i++) {
      const j = i0 + i;
      if (j < 0) continue;
      if (j >= N) break;
      const dt = i / SR - 0.010;
      let g;
      if (dt < atk) g = 1 - depth * clamp((dt + 0.010) / (0.010 + atk), 0, 1);
      else { const u = clamp((dt - atk) / rel, 0, 1); g = (1 - depth) + depth * (u * u * (3 - 2 * u)); }
      if (g < e[j]) e[j] = g;
    }
  }
  return e;
}

// ── voices ────────────────────────────────────────────────────────────
function kick(t, gain = 1) {
  kicks.push(t);
  const n = Math.round(0.46 * SR), i0 = Math.round(t * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const f = 46 + 96 * Math.exp(-u * 38);
    ph += (TAU * f) / SR;
    const env = Math.exp(-u * 8.2) * Math.min(1, u / 0.0016);
    const click = Math.exp(-u * 380) * 0.075 * Math.sin(TAU * 1150 * u);
    emit("drums", i0 + i, (Math.sin(ph) * env + click) * 0.80 * gain * tailFade(i, n), 0, null, 0);
  }
}

function bass(t, midi, dur, gain = 1, slideFrom = null) {
  const n = Math.round((dur + 0.12) * SR), i0 = Math.round(t * SR);
  const f1 = hz(midi), f0 = slideFrom !== null ? hz(slideFrom) : f1;
  let p1 = 0, p2 = 0, p3 = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const glide = clamp(u / 0.075, 0, 1);
    const f = f0 + (f1 - f0) * (glide * glide * (3 - 2 * glide));
    p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
    let env = Math.min(1, u / 0.012);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.12);
    const s = Math.sin(p1) + 0.52 * Math.sin(p2) + 0.10 * Math.sin(p3);
    lp += 0.50 * (s - lp);
    emit("music", i0 + i, lp * 0.40 * env * gain * tailFade(i, n), 0, null, 0);
  }
}

function sines(t, midis, dur, gain, pan, sideAmt = 0.5, bright = 1, dly = 0, attack = 0.020) {
  const n = Math.round((dur + 0.40) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  const ph = midis.map(() => [0, 0, 0]);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    let s = 0;
    for (let v = 0; v < midis.length; v++) {
      const f = hz(midis[v]);
      ph[v][0] += (TAU * f) / SR;
      ph[v][1] += (TAU * f * 1.0035) / SR;
      ph[v][2] += (TAU * f * 2) / SR;
      s += Math.sin(ph[v][0]) + 0.7 * Math.sin(ph[v][1]) + 0.08 * bright * Math.sin(ph[v][2]);
    }
    s /= midis.length * 1.8;
    lp += 0.28 * (s - lp);
    let env = Math.min(1, u / attack);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.40);
    emit("music", i0 + i, lp * env * gain * tailFade(i, n), pan, sp, sideAmt, dly);
  }
}

const missing = new Set();
// One-shot player. `pump` is note 8: how much of this voice is handed to
// the kick+snare-ducked share, and `pumpIn` is how long the attack gets
// before the duck reaches full depth. A word therefore SPEAKS first and
// PUMPS after, which is the only way a 4-second "daaaaash" can breathe in
// time and still be a word.
function shot(name, t, {
  gain = 1, pan = 0, semis = 0, bus = "drums", side = 0.35, dark = 0,
  dur = null, dly = 0, pump = 0, pumpIn = 0.30,
} = {}) {
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  const step = Math.pow(2, semis / 12);
  const avail = Math.floor((s.length - 2) / step);
  const n = dur ? Math.min(avail, Math.round(dur * SR)) : avail;
  const i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  const rampN = Math.max(1, pumpIn * SR);
  let lp = 0, pos = 0;
  for (let i = 0; i < n; i++, pos += step) {
    const q = pos | 0;
    if (q + 1 >= s.length) break;
    const f = pos - q;
    let v = s[q] + (s[q + 1] - s[q]) * f;
    if (dark > 0) { lp += (1 - dark) * (v - lp); v = lp; }
    const env = Math.min(1, i / (0.0015 * SR));
    const w = pump > 0 ? pump * Math.min(1, i / rampN) : 0;
    emit(bus, i0 + i, v * env * gain * tailFade(i, n), pan, sp, side, dly, w);
  }
}
// The moving words: intelligibility first, so a light pump and a long
// onset guard. The held ones override both.
const sung = (name, t, o = {}) =>
  shot(name, t, { bus: "vox", side: 0.6, pump: 0.34, pumpIn: 0.34, ...o });
// A held note is mostly vowel, so it can take the deep pump — this is the
// "daaaaaaaaaash" @jeffrey asked to hear breathing.
const held = (name, t, o = {}) =>
  shot(name, t, { bus: "vox", side: 0.7, pump: 0.88, pumpIn: 0.22, ...o });

function stretched(name, t, { gain = 1, pan = 0, semis = 0, stretch = 1, dur = 1, side = 0.7, dark = 0.45, bus = "vox", dly = 0 } = {}) {
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  const step = Math.pow(2, semis / 12);
  const n = Math.round(dur * SR);
  const grain = Math.round(0.055 * SR), hopOut = grain >> 1, hopIn = (hopOut * step) / stretch;
  const acc = new Float64Array(n + grain);
  let read = 0;
  for (let g = 0; g * hopOut < n; g++) {
    if (read + grain * step >= s.length - 2) break;
    for (let k = 0; k < grain; k++) {
      const o = g * hopOut + k;
      if (o >= acc.length - 1) break;
      const pos = read + k * step, q = pos | 0, f = pos - q;
      acc[o] += (s[q] + (s[q + 1] - s[q]) * f) * (0.5 - 0.5 * Math.cos((TAU * k) / (grain - 1)));
    }
    read += hopIn;
  }
  const i0 = Math.round(t * SR), sp = spatial(pan * 1.3);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    lp += (1 - dark) * (acc[i] - lp);
    let env = Math.min(1, u / 0.030);
    const left = (n - i) / SR;
    if (left < 0.12) env *= left / 0.12;
    emit(bus, i0 + i, lp * env * gain * tailFade(i, n), pan, sp, side, dly);
  }
}

// ── harmony ───────────────────────────────────────────────────────────
// B natural minor. Away from the chorus the chords still walk v2's four
// rows out of PROGRESSIONS_CHILL (recap/bin/trance.mjs). The CHORUS gets
// its own fixed progression — Bm D G Em, row [0,2,5,3] — because a chorus
// that arrives on the same harmony every time is what makes six different
// arrangements of it read as one chorus.
const SCALE = [0, 2, 3, 5, 7, 8, 10];
const sd = (i) => SCALE[((i % 7) + 7) % 7] + 12 * Math.floor(i / 7);
const ROWS = [[0, 5, 2, 6], [0, 6, 3, 5], [0, 4, 5, 3], [0, 2, 5, 3]];
const CHORUS_DEGS = [0, 2, 5, 3];                   // Bm · D · G · Em
function degAt(bar) {
  if (isChorus(bar)) {
    const a = S[sectionAt(bar)][0];
    return CHORUS_DEGS[Math.floor(((bar - a) % 8) / 2)];
  }
  return ROWS[Math.floor(bar / 8) % ROWS.length][Math.floor((bar % 8) / 2)];
}
const bassRoot = (deg) => 35 + sd(deg);                        // B1 = 35
const triad = (deg, base = 59) => [sd(deg), sd(deg + 2), sd(deg + 4)].map((s) => base + s);

const CULT_PITCH = { b2: 47, d3: 50, fs3: 54, g3: 55, a3: 57, b3: 59, cs4: 61, d4: 62, e4: 64, fs4: 66, g4: 67 };
function choirFor(deg) {
  const pcs = new Set(triad(deg).map((m) => ((m % 12) + 12) % 12));
  const ok = Object.keys(CULT_PITCH)
    .filter((n) => pcs.has(CULT_PITCH[n] % 12))
    .sort((a, b) => CULT_PITCH[a] - CULT_PITCH[b]);
  if (!ok.length) return ["b3"];
  const low = ok.filter((n) => CULT_PITCH[n] <= 55);
  const high = ok.filter((n) => CULT_PITCH[n] >= 57);
  const pick = [low[0] ?? high[1] ?? high[0], high[0], high[high.length - 1]];
  return [...new Set(pick.filter(Boolean))];
}

// ── octave mixing (note 6) ────────────────────────────────────────────
// HI and LO are index-aligned, so `dashFor` always hands back the same
// pitch class an octave apart: Camille and Alex sing the upper one, and
// Jeffrey — already a B2 in the source — sings the one beneath it. The
// octave mix is three humans, not a shifted copy.
const HI = ["b3", "d4", "e4", "fs4", "g4"];
const LO = ["b2", "d3", "e3", "fs3", "g3"];
const PC = { b2: 47, d3: 50, e3: 52, fs3: 54, g3: 55, a3: 57, b3: 59, cs4: 61, d4: 62, e4: 64, fs4: 66, g4: 67, a4: 69 };
function dashFor(deg, i = 0) {
  const pcs = triad(deg).map((m) => ((m % 12) + 12) % 12);
  const ok = HI.filter((n) => pcs.includes(PC[n] % 12));
  const hi = ok.length ? ok[i % ok.length] : "b3";
  return { hi, lo: LO[HI.indexOf(hi)] };
}

// Three registers of dot, per performer, filtered to the chord.
const DOTS = {
  j: ["b2", "d3", "e3", "fs3", "g3", "a3"],
  a: ["fs3", "g3", "a3", "b3", "d4", "e4"],
  c: ["b3", "cs4", "d4", "e4", "fs4", "g4", "a4"],
};
function dotsFor(deg) {
  const pcs = triad(deg).map((m) => ((m % 12) + 12) % 12);
  const out = {};
  for (const k of Object.keys(DOTS)) {
    const ok = DOTS[k].filter((n) => pcs.includes(PC[n] % 12));
    out[k] = ok.length ? ok : [DOTS[k][0]];
  }
  return out;
}

let seed = 20220120;                                   // the cult post date
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);
const jit = (ms = 6) => ((rnd() - 0.5) * 2 * ms) / 1000;
const vel = (spread = 0.20) => 1 - rnd() * spread;

// ── THE CHORUS ────────────────────────────────────────────────────────
// Eight bars over Bm · D · G · Em, and every word of it is sung:
//
//   0.00  run real fast      G4 → F#4 → D4 held 1.20      (Bm)
//   2.00  i wanna hide       D4 → E4 → F#4                 (D)
//   3.00  a — waaaaay        G4 → A4 held 1.60             (D)
//   4.00  i wanna            B3 → C#4                      (G)
//   4.50  dash ───────────── D4 held 1.50, three performers (G)
//   6.00  dot · dot          two chord tones, staccato      (Em)
//   7.00  dash ───────────── B3 held 1.50, into the turn    (Em)
//
// Lines 1 and 2 are the ElevenLabs words. Lines 3 and 4 hand the syllables
// back to the whistlegraph itself: the dashes and dots are Camille, Alex
// and Jeffrey, stacked 28 ms apart on one pitch, which is v2's best move
// and stays.
function dashStack(t, deg, G, { oct = "both", idx = 0, long = false, hold = 1.5 } = {}) {
  const { hi, lo } = dashFor(deg, idx);
  const sfx = long ? "" : "-hold";
  const hiC = long ? `dashlong-camille-${hi}` : `dash-camille-${hi}${sfx}`;
  const hiA = long ? null : `dash-alex-${hi}-hold`;
  const loJ = long ? `dashlong-jeffrey-${lo}` : `dash-jeffrey-${lo}-hold`;
  const dur = long ? null : hold;
  if (oct !== "lo") {
    held(hiC, t + 0.000 + jit(3), { gain: 0.52 * G, pan: -0.42, side: 0.70, dly: 0.10, dur });
    if (hiA) held(hiA, t + 0.028 + jit(3), { gain: 0.48 * G, pan: 0.42, side: 0.70, dly: 0.10, dur });
  }
  if (oct !== "hi")
    held(loJ, t + 0.056 + jit(3), { gain: (oct === "lo" ? 0.72 : 0.50) * G, pan: 0.0,
      side: 0.34, dark: 0.20, dly: 0.08, dur });
}

// Six statements, no two alike (note 9). `lead` picks the octave of the
// ElevenLabs lines; `oct` picks which octaves the dashes use; `drop`
// removes lines; `fast` swaps line 1 for its double-time self.
const CHORUS_TAKES = {
  chorus1: { lead: "hi", oct: "hi", g: 1.00, answer: "none", tag: null },
  chorus2: { lead: "both", oct: "both", g: 1.00, answer: "dots", tag: "fast" },
  chorus3: { lead: "lo", oct: "hi", g: 0.96, answer: "sos", tag: null },
  chorus4: { lead: "hi", oct: "lo", g: 0.92, drop: [2, 3], answer: "none", tag: null, longDash: true },
  chorus5: { lead: "both", oct: "both", g: 1.00, answer: "dots", tag: null, choir: true },
  chorus6: { lead: "both", oct: "both", g: 1.00, fast: true, answer: "dots", tag: "fast", choir: true },
};

function leadPair(base, t, G, mode, o = {}) {
  // The octave mix as an arrangement device: "hi" and "lo" are different
  // takes of the same line an octave apart, "both" plays them together.
  const put = (nm, g, pan, extra) =>
    sung(nm, t + (extra ?? 0), { gain: g * G, pan, side: o.side ?? 0.5, dly: o.dly ?? 0.20,
      pump: o.pump ?? 0.34, pumpIn: o.pumpIn ?? 0.34 });
  if (mode === "hi") put(`${base}-hi`, o.g ?? 0.92, o.pan ?? 0);
  else if (mode === "lo") put(`${base}-lo`, (o.g ?? 0.92) * 1.02, o.pan ?? 0);
  else {
    put(`${base}-hi`, (o.g ?? 0.92) * 0.88, (o.pan ?? 0) - 0.16);
    put(`${base}-lo`, (o.g ?? 0.92) * 0.62, (o.pan ?? 0) + 0.16, 0.022);
  }
}

function chorus(bar, take) {
  const t = at(bar);
  const G = take.g ?? 1.0;
  const drop = new Set(take.drop ?? []);
  const degs = CHORUS_DEGS;

  // ── line 1 · "run real fast" ─────────────────────────────────────────
  if (!drop.has(1)) {
    if (take.fast) {
      // The lyric performing itself: the same words at exactly half the
      // length, so it fits TWICE in the two bars the slow one takes — and
      // it stops at 2.00 s, because that is where "i wanna hide" starts
      // and two lines on the same downbeat is mud, not a climax.
      for (const [k, oct] of [[0.00, "hi"], [1.00, "lo"]])
        sung(`runrealfast-fast-${oct}`, t + k + jit(3),
          { gain: (oct === "hi" ? 0.92 : 0.70) * G, pan: oct === "hi" ? -0.14 : 0.14,
            side: 0.5, dly: 0.22, pump: 0.30, pumpIn: 0.20 });
    } else {
      leadPair("runrealfast", t + jit(4), G, take.lead, { g: 0.96, dly: 0.24 });
    }
  }

  // ── line 2 · "i wanna hide away" ─────────────────────────────────────
  if (!drop.has(2)) {
    leadPair("iwannahide", t + 2.00 + jit(4), G, take.lead, { g: 0.86, pan: -0.14, dly: 0.20 });
    // "a — waaaaay": 1.60 s on A4 over the D chord. This is the held vowel
    // the new line was written for, so it goes on the deep pump.
    const aw = take.lead === "lo" ? ["away-lo"] : take.lead === "hi" ? ["away-hi"] : ["away-hi", "away-lo"];
    aw.forEach((nm, i) => held(nm, t + 3.00 + i * 0.026 + jit(3), {
      gain: (i ? 0.56 : 0.90) * G, pan: i ? 0.22 : -0.06, side: 0.62, dly: 0.26,
      pump: 0.80, pumpIn: 0.40,
    }));
  }

  // ── line 3 · "i wanna dash" ──────────────────────────────────────────
  if (!drop.has(3)) {
    sung("iwanna-c-sung", t + 4.00 + jit(4), { gain: 0.84 * G, pan: 0.18, side: 0.5, dly: 0.20 });
    dashStack(t + 4.50, degs[2], G * 0.98, { oct: take.oct, idx: 1 });
  } else {
    dashStack(t + 4.00, degs[2], G * 0.98,
      { oct: take.oct, idx: 1, long: take.longDash, hold: 1.5 });
  }

  // ── line 4 · "dot dot dash" ──────────────────────────────────────────
  if (!drop.has(4)) {
    const d = dotsFor(degs[3]);
    const [p0, p1] = take.lead === "lo" ? ["j", "a"] : ["c", "a"];
    sung(`dot-${p0}-${d[p0][0]}`, t + 6.00 + jit(3),
      { gain: 0.92 * G, pan: -0.45, side: 0.75, dly: 0.36, pump: 0.15 });
    sung(`dot-${p1}-${d[p1][0]}`, t + 6.50 + jit(3),
      { gain: 0.90 * G, pan: 0.45, side: 0.75, dly: 0.36, pump: 0.15 });
    dashStack(t + 7.00, degs[3], G * 0.92, { oct: take.oct, idx: 0 });
  }

  // ── the answer figure — a different one each time (note 9) ───────────
  if (take.answer === "dots") {
    const d = dotsFor(degs[1]);
    for (const [k, reg, pan] of [[1.30, "c", 0.38], [1.65, "j", -0.38]])
      sung(`dot-${reg}-${d[reg][0]}`, t + k + jit(4),
        { gain: 0.30 * G, pan, side: 0.8, dly: 0.5, pump: 0.2 });
  } else if (take.answer === "sos") {
    const d = dotsFor(degs[1]);
    for (const [k, reg] of [[0, "c"], [1, "a"], [2, "j"]])
      sung(`dot-${reg}-${d[reg][0]}`, t + 1.20 + k * 0.25 + jit(3),
        { gain: 0.26 * G, pan: k === 1 ? 0 : k ? 0.4 : -0.4, side: 0.85, dly: 0.55, pump: 0.2 });
  }

  // A tag in the last bar: the double-time line, used as punctuation.
  if (take.tag === "fast")
    for (const [k, oct] of [[0.00, "hi"], [0.50, "lo"]])
      sung(`runrealfast-fast-${oct}`, t + 7.0 + k + jit(3), {
        gain: (oct === "hi" ? 0.44 : 0.34) * G, pan: oct === "hi" ? 0.3 : -0.3,
        side: 0.7, dly: 0.45, pump: 0.25, pumpIn: 0.15,
      });
}

// ── the SOS figure (v2's, kept) ───────────────────────────────────────
function sos(bar, g = 1) {
  const t = at(bar);
  for (const [k, n, p] of [[0, "dot-b3", -0.32], [1, "dot-fs3", 0.0], [2, "dot-d4", 0.32]])
    sung(n, t + k * BEAT + jit(3), { gain: 0.74 * g, pan: p, side: 0.72, dly: 0.30, pump: 0.15 });
  held("sos-dash-d4", t + 2.0 + jit(4), { gain: 0.70 * g, pan: -0.26, side: 0.62, dly: 0.20, pump: 0.75 });
  held("sos-dash-fs4", t + 3.5 + jit(4), { gain: 0.70 * g, pan: 0.26, side: 0.62, dly: 0.20, pump: 0.75 });
  held("sos-dash-e4", t + 5.0 + jit(4), { gain: 0.68 * g, pan: 0.00, side: 0.62, dly: 0.20, pump: 0.75 });
  for (const [k, n, p] of [[0, "dot-d4", 0.32], [1, "dot-a3", 0.0], [2, "dot-b3", -0.32]])
    sung(n, t + 6.5 + k * BEAT + jit(3), { gain: 0.64 * g, pan: p, side: 0.78, dly: 0.42, pump: 0.15 });
}

function choir(bar, g) {
  const t = at(bar);
  const picks = choirFor(degAt(bar));
  const gains = [0.46, 0.38, 0.30], pans = [0.0, -0.50, 0.50], sides = [0.40, 0.85, 0.85];
  picks.forEach((nm, i) => held(`cult-${nm}`, t + i * 0.045, {
    gain: g * (gains[i] ?? 0.28), pan: pans[i] ?? 0, side: sides[i] ?? 0.7, dark: 0.32,
    pump: 0.55, pumpIn: 0.60,
  }));
}

// ── the "dasssh only" break (note 10) ─────────────────────────────────
// Eight bars of nothing but held sung dashes. No dots, no hook words. Two
// octaves at once, four seconds a note, over a kick and a snare that are
// only there to make the vowels breathe. This is the pump on display.
function dashBreak(bar, { g = 1, longs = true } = {}) {
  const t = at(bar), deg = degAt(bar), { hi, lo } = dashFor(deg, (bar >> 1) & 1);
  if (longs) {
    held(`dashlong-camille-${hi}`, t + jit(4), {
      gain: 0.56 * g, pan: -0.34, side: 0.80, dly: 0.16, pump: 0.94, pumpIn: 0.20,
    });
    held(`dashlong-jeffrey-${lo}`, t + 0.05 + jit(4), {
      gain: 0.62 * g, pan: 0.24, side: 0.42, dark: 0.18, dly: 0.10, pump: 0.94, pumpIn: 0.20,
    });
  }
  // a 1.5 s answer from Alex, on the other chord tone, half a bar late
  const alt = dashFor(deg, ((bar >> 1) & 1) + 1);
  held(`dash-alex-${alt.hi}-hold`, t + 2 * BEAT + jit(5), {
    gain: 0.40 * g, pan: 0.46, side: 0.85, dly: 0.30, pump: 0.90, pumpIn: 0.18,
  });
}

// ── the "dot only" section (note 10) ──────────────────────────────────
// The complement: staccato, dry, no sustain anywhere. Three registers of
// dot — Jeffrey's, Alex's, Camille's — placed on a morse-flavoured grid
// that never quite repeats. Side and delay are deliberately small; this
// section is the one place in the track that is close-mic'd.
const DOT_FIG = [
  [[0, "c"], [0.5, "a"], [1, "c"], [2, "j"], [2.5, "a"], [3, "c"], [3.5, "a"]],
  [[0, "j"], [1, "c"], [1.5, "a"], [2, "c"], [3, "j"], [3.25, "a"], [3.5, "c"]],
  [[0, "c"], [0.5, "c"], [1, "a"], [1.5, "a"], [2, "j"], [3, "c"], [3.5, "j"]],
  [[0, "j"], [0.75, "a"], [1.5, "c"], [2, "c"], [2.5, "a"], [3, "j"], [3.5, "c"], [3.75, "c"]],
];
function dotBar(bar, g = 1) {
  const t = at(bar), d = dotsFor(degAt(bar));
  const fig = DOT_FIG[bar % DOT_FIG.length];
  fig.forEach(([b, reg], i) => {
    const list = d[reg];
    const nm = `dot-${reg}-${list[(i + bar) % list.length]}`;
    const pan = reg === "j" ? 0.0 : reg === "a" ? (i & 1 ? 0.40 : -0.40) : (i & 1 ? -0.28 : 0.28);
    sung(nm, t + b * BEAT + jit(4), {
      gain: (reg === "j" ? 0.95 : reg === "a" ? 0.84 : 0.92) * g * vel(0.22),
      pan, side: 0.20, dly: 0.06, pump: 0.0,
    });
  });
}

// ── score ─────────────────────────────────────────────────────────────
console.log(`→ scoring ${BARS} bars @ ${BPM} BPM · B minor · chill techno · chorus-first · ${(BARS * BAR).toFixed(1)}s`);

for (let bar = 0; bar < BARS; bar++) {
  const t = at(bar);
  const sec = sectionAt(bar);
  const deg = degAt(bar);
  const root = bassRoot(deg);
  const chord = triad(deg);
  const four = bar % 4, eight = bar % 8;
  const D = dense(bar);

  // ---- kick ------------------------------------------------------------
  if (kickOn(bar)) {
    const g =
      sec === "chorus1" ? 0.60 + 0.042 * (bar - S.chorus1[0])   // the cold open
        : sec === "pulse" ? 0.62 + 0.04 * (bar - S.pulse[0])
          : sec === "chorus4" ? 0.78
            : sec === "dashout" ? 0.88 - 0.045 * (bar - S.dashout[0]) : 0.92;
    for (let b = 0; b < 4; b++) kick(t + b * BEAT + jit(2.5), g * (b === 0 ? 1 : 0.96));
  }

  // ---- snare on 2 and 4 — quiet, and the second pump trigger -----------
  if (snareOn(bar))
    for (const b of [1, 3]) {
      const st = t + b * BEAT + jit(3);
      snares.push(st);
      shot("snare", st, { gain: 0.115 * vel(0.15), pan: -0.06, side: 0.5, dur: 0.20, dark: 0.30 });
      shot("snap", st + 0.010, { gain: 0.055, pan: 0.28, side: 0.5 });
    }

  // ---- hats -------------------------------------------------------------
  if (hatOn(bar)) {
    for (let s = 0; s < 8; s++) {
      const swing = s & 1 ? 0.035 : 0;
      const u = t + (s * 0.5 + swing) * BEAT + jit(5);
      if (s & 1) shot("hatC", u, { gain: 0.20 * vel(), pan: 0.20, side: 0.5, dur: 0.085 });
      else if (s % 4 === 2) shot("hatC", u, { gain: 0.11 * vel(), pan: -0.18, side: 0.5, dur: 0.065 });
    }
    if (D && (eight >= 4 || sec === "dots"))
      for (let s = 0; s < 16; s++)
        if (s % 4 === 1 || s % 4 === 3)
          shot("hatC", t + (s / 16) * BAR + jit(4), { gain: 0.055 * vel(0.5), pan: (s & 2 ? 0.35 : -0.35), side: 0.6, dur: 0.045 });
    if ((D || sec === "drift" || sec === "dashes") && four === 3)
      shot("hatO", t + 3.5 * BEAT + 0.02, { gain: 0.20, pan: -0.28, side: 0.65, dur: 0.34 });
  }

  // ---- rim, clap --------------------------------------------------------
  if (kickOn(bar) && sec !== "chorus1") {
    shot("block", t + 2 * BEAT + jit(5), { gain: 0.17 * vel(), pan: 0.26, side: 0.6, dur: 0.10 });
    if (D) shot("clap", t + 2 * BEAT + jit(5), { gain: 0.20, pan: -0.12, side: 0.72 });
  } else if (sec === "chorus1" && bar >= 4) {
    shot("block", t + 2 * BEAT + jit(5), { gain: 0.13 * vel(), pan: 0.26, side: 0.6, dur: 0.10 });
  }
  if (["drift", "chorus5", "chorus6", "dashes"].includes(sec))
    for (let s = 0; s < 4; s++)
      shot("ride", t + (s + 0.5) * BEAT + jit(8), { gain: 0.055 * vel(0.4), pan: s & 1 ? 0.38 : -0.38, side: 0.7, dur: 0.22 });
  if (hatOn(bar) && !D && sec !== "dashes")
    for (let s = 0; s < 4; s++)
      shot("tambo", t + (s + 0.5) * BEAT + jit(9), { gain: 0.055 * vel(0.4), pan: s & 1 ? 0.42 : -0.42, side: 0.7, dur: 0.09 });

  // ---- bass -------------------------------------------------------------
  if (kickOn(bar) || sec === "hollow") {
    const g =
      sec === "chorus1" ? 0.58 + 0.035 * (bar - S.chorus1[0])
        : sec === "pulse" ? 0.52 : sec === "hollow" ? 0.44
          : sec === "dashout" ? 0.78 - 0.03 * (bar - S.dashout[0])
            : sec === "chorus4" ? 0.70 : 0.86;
    // The dash-only break leaves the offbeats empty so the held vowels own
    // the space — only the sub floor and a root on the downbeat.
    if (sec === "dashes") {
      bass(t + jit(3), root, 0.30, g * 0.9);
      bass(t + 2 * BEAT + jit(3), root, 0.30, g * 0.8);
    } else {
      for (let b = 0; b < 4; b++) {
        const fifth = four === 3 && b === 3;
        bass(t + (b + 0.5) * BEAT + jit(3), root + (fifth ? 7 : 0), 0.26, g, fifth ? root : null);
      }
    }
    if (bar % 2 === 0) bass(t, root - 12, BAR * 0.90, g * 0.42);
  }

  // ---- pad + stabs ------------------------------------------------------
  if (bar % 2 === 0 && sec !== "hollow")
    sines(t, chord.map((m) => m - 12), BAR * 1.85,
      sec === "air" ? 0.055 + 0.012 * (bar - S.air[0]) : 0.085,
      bar % 4 ? 0.22 : -0.22, 0.75, 0.5, 0, 0.30);
  if (isChorus(bar) || sec === "drift")
    for (const b of [0.5, 2.5])
      sines(t + b * BEAT + jit(4), chord, 0.20, 0.075, b > 1 ? 0.36 : -0.36, 0.7, 0.9, 0.34);
  if (sec === "drift" && four === 1)
    sines(t + 3.5 * BEAT, chord.map((m) => m + 12), 0.13, 0.070, rnd() > 0.5 ? 0.5 : -0.5, 0.85, 0.7, 0.70);

  // ---- the sung material ------------------------------------------------
  if (isChorus(bar) && bar === S[sec][0]) chorus(bar, CHORUS_TAKES[sec]);
  if (isChorus(bar) && CHORUS_TAKES[sec].choir && bar % 4 === 0) choir(bar, 0.24);

  if (sec === "dashes" && bar % 2 === 0) dashBreak(bar, { g: 0.92 + 0.03 * ((bar - S.dashes[0]) / 2) });
  if (sec === "dashout" && bar % 2 === 0)
    dashBreak(bar, { g: 0.92 - 0.10 * ((bar - S.dashout[0]) / 2) });
  if (sec === "dots") dotBar(bar, bar < S.dots[0] + 2 ? 0.80 : 1.0);

  if (sec === "air" && bar % 2 === 0) choir(bar, 0.55 + 0.10 * ((bar - S.air[0]) / 8));
  if (sec === "pulse" && bar % 4 === 0) choir(bar, 0.50);
  if (sec === "morse" && bar % 4 === 0) sos(bar, bar === S.morse[0] ? 0.80 : 1.0);

  // hollow: "the three of us are in a", rising then answered falling
  if (bar === S.hollow[0]) held("threeofus-rise", at(bar), { gain: 0.62, pan: -0.10, side: 0.7, dly: 0.24, pump: 0.30 });
  if (bar === S.hollow[0] + 4) held("threeofus-fall", at(bar), { gain: 0.62, pan: 0.10, side: 0.7, dly: 0.24, pump: 0.30 });
  if (sec === "hollow" && bar % 2 === 0) choir(bar, 0.34);
  if (bar === S.hollow[0] + 2 || bar === S.hollow[0] + 6)
    held("bassdash-b2", at(bar, 2), { gain: 0.26, pan: 0, side: 0.3, dark: 0.55, pump: 0.4 });

  // drift: no chorus at all — held dashes as pads, both octaves
  if (sec === "drift" && bar % 2 === 0) {
    const { hi, lo } = dashFor(deg, (bar >> 1) & 1);
    held(`dash-camille-${hi}-hold`, at(bar, 1), { gain: 0.40, pan: -0.45, side: 0.85, dly: 0.30, pump: 0.70 });
    held(`dash-jeffrey-${lo}-hold`, at(bar, 1.1), { gain: 0.34, pan: 0.45, side: 0.50, dly: 0.24, dark: 0.2, pump: 0.70 });
    const low = { 0: "b2", 2: "a2", 3: "e2", 4: "b2", 5: "g2", 6: "a2" }[deg] ?? "b2";
    held(`bassdash-${low}`, at(bar, 0), { gain: 0.26, pan: 0, side: 0.25, dark: 0.6, pump: 0.5 });
  }
  if (sec === "drift" && bar === S.drift[0] + 8) sos(bar, 0.70);

  // ---- the original whistlegraph, unprocessed, as bookends -------------
  if (bar === S.air[0] + 2) shot("chant-full", at(bar, 1), { gain: 0.42, pan: 0, bus: "vox", side: 0.9, dark: 0.55, dly: 0.30 });
  if (bar === S.air[0] + 6) shot("tagline", at(bar, 1), { gain: 0.44, pan: 0, bus: "vox", side: 0.7, dark: 0.40, dly: 0.28 });
  if (bar === S.ebb[0]) shot("hook-spoken", at(bar, 1), { gain: 0.56, pan: -0.10, bus: "vox", side: 0.6, dly: 0.22 });
  if (bar === S.ebb[0] + 4) shot("chant-full", at(bar, 1), { gain: 0.48, pan: 0, bus: "vox", side: 0.9, dark: 0.3, dly: 0.34 });
  if (bar === S.out[0] + 1) shot("tagline", at(bar, 1), { gain: 0.50, pan: 0, bus: "vox", side: 0.8, dark: 0.35, dly: 0.40 });

  if ((sec === "ebb" || sec === "out") && bar % 2 === 0)
    choir(bar, sec === "out" ? 0.48 * (1 - (bar - S.out[0]) / 7) : 0.46);
  if (bar === S.ebb[0] + 2)
    stretched("three-of-us", at(bar, 1), { gain: 0.22, pan: 0.35, semis: 0, stretch: 1.8, dur: BAR * 1.6, side: 0.9, dly: 0.3 });

  // one very soft noise wash at section joins — never a riser
  if ([S.dashes[0], S.air[0], S.morse[0], S.chorus3[0], S.dots[0], S.chorus5[0], S.ebb[0]].includes(bar))
    shot("sweep", t - 0.9, { gain: 0.055, pan: 0, bus: "music", side: 0.9, dur: 2.2, dark: 0.35 });
}
if (missing.size) console.warn(`  ! missing samples: ${[...missing].join(", ")}`);

// ── the two ducks ─────────────────────────────────────────────────────
// The bed keeps v2's breath. The voices get a deeper one fed by BOTH the
// kick and the snare (note 8) — which is why the snare exists at all in a
// track that otherwise doesn't want a backbeat.
console.log(`  ${kicks.length} kicks · ${snares.length} snares → building ducks`);
const bedEnv = buildEnv(kicks.map((t) => ({ t, depth: 0.50, atk: 0.009, rel: 0.31 })));
const pumpEnv = buildEnv([
  ...kicks.map((t) => ({ t, depth: 0.66, atk: 0.009, rel: 0.30 })),
  ...snares.map((t) => ({ t, depth: 0.44, atk: 0.009, rel: 0.20 })),
]);

// ── dub delay ─────────────────────────────────────────────────────────
{
  const Dl = Math.round(0.75 * BEAT * SR);       // dotted eighth = 0.375 s
  const FB = 0.42;
  const damp = 1 - Math.exp((-TAU * 2600) / SR);
  const hpRc = 1 / (TAU * 180), hpA = hpRc / (hpRc + 1 / SR);
  const bL = new Float32Array(N + Dl + 1), bR = new Float32Array(N + Dl + 1);
  let dL = 0, dR = 0, hpL = 0, hpR = 0, pL = 0, pR = 0;
  for (let i = 0; i < N; i++) {
    const tapL = i >= Dl ? bL[i - Dl] : 0;
    const tapR = i >= Dl ? bR[i - Dl] : 0;
    dL += damp * (tapR - dL);
    dR += damp * (tapL - dR);
    bL[i] = dlySend[i] + dR * FB;
    bR[i] = dL * FB;
    hpL = hpA * (hpL + bL[i] - pL); pL = bL[i];
    hpR = hpA * (hpR + bR[i] - pR); pR = bR[i];
    musicL[i] += hpL * 0.50;
    musicR[i] += hpR * 0.50;
  }
}

// ── Special Sign side returns ─────────────────────────────────────────
// One for the bed, and — note 7 — one for the VOICES, run twice so the
// pumped share of a voice keeps its own ambience pumping with it.
// Band-limited 80 Hz – 11.5 kHz, handed back antisymmetrically (L=+, R=−).
// Mono-safe by construction: the direct sung voice never left its
// equal-power pan, so a phone speaker still gets the words.
const SEND = {           // per-section side depth
  chorus1: [0.62, 0.34], chorus2: [0.70, 0.52], dashes: [0.86, 0.78],
  air: [0.92, 0.72], pulse: [0.72, 0.60], morse: [0.66, 0.64],
  chorus3: [0.58, 0.58], dots: [0.40, 0.22], chorus4: [0.52, 0.44],
  hollow: [0.80, 0.70], drift: [0.74, 0.72], chorus5: [0.56, 0.46],
  chorus6: [0.50, 0.40], dashout: [0.72, 0.74], ebb: [0.84, 0.66],
  out: [0.90, 0.72],
};
function sideReturn(src, L, R, pick) {
  const hpRc = 1 / (TAU * 80), hpA = hpRc / (hpRc + 1 / SR);
  const lpK = 1 - Math.exp((-TAU * 11500) / SR);
  let hp = 0, lp = 0, prev = 0, send = SEND[ORDER[0]][pick];
  for (let i = 0; i < N; i++) {
    const s = src[i];
    hp = hpA * (hp + s - prev); prev = s;
    lp += lpK * (hp - lp);
    const target = SEND[sectionAt(Math.min(BARS - 1, Math.floor((i / SR) / BAR)))][pick];
    send += 0.00004 * (target - send);
    L[i] += lp * send;
    R[i] -= lp * send;
  }
}
sideReturn(sideB, musicL, musicR, 0);
sideReturn(sideV, voxL, voxR, 1);
sideReturn(sideP, pumpL, pumpR, 1);

// ── stems ─────────────────────────────────────────────────────────────
const writeStereo = (path, L, R, scale = 1, env = null) => {
  const bytes = N * 2 * 4;
  const buf = Buffer.alloc(44 + bytes);
  buf.write("RIFF", 0, "ascii"); buf.writeUInt32LE(36 + bytes, 4); buf.write("WAVE", 8, "ascii");
  buf.write("fmt ", 12, "ascii"); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(2, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 8, 28);
  buf.writeUInt16LE(8, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36, "ascii"); buf.writeUInt32LE(bytes, 40);
  for (let i = 0; i < N; i++) {
    const g = scale * (env ? env[i] : 1);
    buf.writeFloatLE(L[i] * g, 44 + i * 8);
    buf.writeFloatLE(R[i] * g, 44 + i * 8 + 4);
  }
  writeFileSync(path, buf);
};
if (process.argv.includes("--stems")) {
  const dir = resolve(OUT, "stems");
  mkdirSync(dir, { recursive: true });
  // The vox stem is written POST-pump and post-makeup, i.e. exactly what
  // lands in the sum — otherwise the balance check lies about note 8.
  const vl = new Float32Array(N), vr = new Float32Array(N);
  for (let i = 0; i < N; i++) {
    vl[i] = voxL[i] * VOXG + pumpL[i] * pumpEnv[i] * PUMPG;
    vr[i] = voxR[i] * VOXG + pumpR[i] * pumpEnv[i] * PUMPG;
  }
  writeStereo(resolve(dir, "v3-vox.wav"), vl, vr);
  writeStereo(resolve(dir, "v3-music.wav"), musicL, musicR, 1, bedEnv);
  writeStereo(resolve(dir, "v3-drums.wav"), drumsL, drumsR);
  console.log(`  stems → ${dir}`);
}

// ── sum ───────────────────────────────────────────────────────────────
// Clean: duck, fade, measure, trim linearly. No master tanh anywhere.
let peak = 0;
for (let i = 0; i < N; i++) {
  const d = bedEnv[i];
  const p = pumpEnv[i];
  const fadeIn = Math.min(1, i / (0.014 * SR));
  const fadeOut = Math.min(1, (N - 1 - i) / (2.6 * SR));
  const fade = Math.max(0, Math.min(fadeIn, fadeOut));
  const l = (musicL[i] * d + voxL[i] * VOXG + pumpL[i] * p * PUMPG + drumsL[i]) * fade;
  const r = (musicR[i] * d + voxR[i] * VOXG + pumpR[i] * p * PUMPG + drumsR[i]) * fade;
  musicL[i] = l; musicR[i] = r;
  if (Math.abs(l) > peak) peak = Math.abs(l);
  if (Math.abs(r) > peak) peak = Math.abs(r);
}
const norm = peak > 1e-9 ? 0.92 / peak : 1;
for (let i = 0; i < N; i++) { musicL[i] *= norm; musicR[i] *= norm; }
console.error(`# pre-master peak ${peak.toFixed(6)} · linear trim ${norm.toFixed(3)} · ${kicks.length} kicks / ${snares.length} snares`);

const outWav = resolve(OUT, "cult-remix-v3.wav");
writeStereo(outWav, musicL, musicR);

writeFileSync(resolve(OUT, "cult-remix-v3.events.json"), JSON.stringify({
  schema: "aesthetic.computer/pop-events/v1",
  track: "whistlegraph cult --- remix (v3, chorus first)",
  renderer: "pop/cult/bin/render3.mjs",
  source: {
    work: "cult — The Three of Us Are in a Cult (Whistlegraph, 2022)",
    tiktok: "https://www.tiktok.com/@whistlegraph/video/7055106286232325423",
    mirror: "https://assets.aesthetic.computer/whistlegraph/index/posts/7055106286232325423.mp4",
    performers: ["Jeffrey Alan Scudder", "Camille Klein", "Alex Freundlich"],
    transcript: "Dash, dash, dash, dot, dot, dot. The three of us are in a cult.",
    caption: "dot dot dot dash dash dash dot dot dot (jk)",
  },
  chorus: ["run real fast", "i wanna hide away", "i wanna dash", "dot dot dash"],
  chorusHarmony: "Bm · D · G · Em, two bars a chord (PROGRESSIONS_CHILL row [0,2,5,3])",
  chorusTakes: CHORUS_TAKES,
  tempoBPM: BPM, bars: BARS, seconds: +(BARS * BAR).toFixed(2),
  sections: Object.fromEntries(Object.entries(S).map(([k, [a, b]]) => [k, { bars: [a, b], seconds: [+at(a).toFixed(2), +at(b).toFixed(2)] }])),
  vocalTreatment: {
    octaveMix: "every lead line rendered at two octaves; dashes put Camille/Alex above Jeffrey's B2-register hold",
    spatial: "Special Sign topology applied to the voices: dry equal-power pan + band-limited 80 Hz–11.5 kHz antisymmetric side return, run separately for the pumped share",
    sidechain: "held vocals ducked by kick (depth 0.66) AND snare (0.44) with a ~9 ms ramp; per-voice onset guard (0.18–0.40 s) so attacks land unducked",
  },
  prePeak: +peak.toFixed(6), linearTrim: +norm.toFixed(4),
  kicks: kicks.length, snares: snares.length,
}, null, 2));

console.log(`✓ ${outWav}  (${(BARS * BAR).toFixed(1)}s)`);
