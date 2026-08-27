#!/usr/bin/env node
// render.mjs — "whistlegraph season --- remix (v1)"
//
// The source is the `h0t` work: six spring-2022 takes of the same nine
// seconds — an argument and a reconciliation:
//
//     it's too hot / no it's not / now I'm back in season
//
// The material is LITERALLY an argument, so the record stages it as one.
// "It's too hot" states its case from the left; "no it's not" answers from
// the right — and in the second half the answer comes back HIGH (the A4
// take from "spring flower"), so the argument crosses registers as well as
// the stereo field. The sung tagline (E4 D4 C#4 B3 A3, a descending
// pentachord that lands on A) is the making-up, and it gets the breakdown.
//
// Measured, not guessed (analysis/harvest.json, analysis/melody.json):
//   · the full "it's too hot" phrase spans 1.96 s — one bar at 122.4 BPM,
//     so the track runs 122 and no vocal is ever time-stretched;
//   · the take's own walk (beat_track + the doo IOIs) sits near 103, and
//     122 keeps the chant hits inside the bar with air around them;
//   · the tagline lands on A, so the remix lives on an A pedal. The bass
//     and pads play root+fifth with the third left OPEN — the doo walk
//     carries C-natural and the tagline carries C#, and an open fifth is
//     the only floor both can stand on without anybody flinching.
//
// Form, 88 bars at 122 (≈2:53):
//
//   intro   0– 8  the kick fades in from under a pad; "hot" / "not"
//                 teasers flicker at the edges
//   hookA   8–24  the argument, low vs low: too-hot left, no-it's-not
//                 right, two bars each — the call and the answer
//   doo    24–32  the sung doo walk (F F G G E D C) takes the lead;
//                 drums thin to kick and ride
//   hookB  32–48  the argument again and the answer moves UP — the A4
//                 "no it's not", claps on 2 and 4, open hats
//   break  48–56  kick out. "now I'm back in season", twice, over the
//                 pad — the argument stops and the season comes back
//   hookC  56–80  fullest: both answers trade bars, 16th hats, and from
//                 bar 72 the tagline sails over the argument — both
//                 things true at once
//   outro  80–88  drums peel; the original take speaks once, unprocessed;
//                 the last word on the record is "season", ringing in
//                 the delay
//
// The DSP is pop/cult's scaffolding (render10.mjs), stripped to what a
// dance record needs: four-on-floor thick electro kick, sine-bump bass,
// harmonized-sine pads, the dotted-eighth dub delay, mono-safe Special
// Sign side return, kick-keyed sidechain with a proud vox bus. No DTMF,
// no tube, no skids — that record was a séance; this one is a dance.
// Every rule survives: 10 ms raised-cosine tails on every voice, ducks
// that ramp rather than step, no master tanh, ONE linear trim.
//
//   node pop/season/bin/render.mjs            # → out/season-remix-v1.wav
//   node pop/season/bin/render.mjs --stems    # + per-bus stems
//   bash pop/season/bin/cut-v1.sh             # master + mp3

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
const BPM = 122;
const BEAT = 60 / BPM;               // 0.4918 s
const BAR = 4 * BEAT;                // 1.9672 s
const BARS = 88;                     // ≈173 s
const TAU = Math.PI * 2;
const N = Math.round((BARS * BAR + 3.0) * SR);

// Three buses. Drums never duck, the bed takes the breath, the voices take
// a quarter-depth duck and ride proud (+3 dB — cult v2's measurement, kept).
const musicL = new Float32Array(N), musicR = new Float32Array(N);
const drumsL = new Float32Array(N), drumsR = new Float32Array(N);
const voxL = new Float32Array(N), voxR = new Float32Array(N);
const sideB = new Float32Array(N);   // bed's side field
const sideV = new Float32Array(N);   // voices'
const dlySend = new Float32Array(N);

const VOXG = 1.42;
const clamp = (v, a, b) => (v < a ? a : v > b ? b : v);
const hz = (midi) => 440 * Math.pow(2, (midi - 69) / 12);
const at = (bar, beat = 0) => bar * BAR + beat * BEAT;
const smooth = (u) => (u <= 0 ? 0 : u >= 1 ? 1 : u * u * (3 - 2 * u));

// ── the receipt's event array ─────────────────────────────────────────
// Every scored hit is pushed from inside the voice that makes it, so a
// video renderer can draw the same performance the ear hears. Debug:
// MUTE=bass node bin/render.mjs · ONLY=vox node bin/render.mjs
const EVENTS = [];
const MUTED = new Set((process.env.MUTE || "").split(",").map((v) => v.trim()).filter(Boolean));
const ONLY = new Set((process.env.ONLY || "").split(",").map((v) => v.trim()).filter(Boolean));
const allow = (v) => !MUTED.has(v) && (!ONLY.size || ONLY.has(v));
if (MUTED.size) console.log("  MUTED:", [...MUTED].join(", "));
if (ONLY.size) console.log("  ONLY:", [...ONLY].join(", "));

function classify(name) {
  if (/^(its-too-hot|its|too|hot)(-yell)?$/.test(name)) return { voice: "call" };
  if (/^(no-its-not|no|not)(-high|-mid)?$/.test(name)) return { voice: "answer" };
  if (/^doo/.test(name)) return { voice: "doo" };
  if (/^(season|season-b|season-line|season-line-b|now-im|back-in)$/.test(name))
    return { voice: "tagline" };
  if (name === "chant-full") return { voice: "source" };
  if (/^hat[CO]$/.test(name)) return { voice: "hat" };
  if (/^(clap|snap|ride|snare)$/.test(name)) return { voice: "perc" };
  if (name === "sweep") return { voice: "sweep" };
  return { voice: "sample" };
}

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
  let s = samples;
  if (sampleRate !== SR) {
    const q = join(OUT, `.cache-${name}-rs.wav`);
    execFileSync("ffmpeg", ["-y", "-v", "error", "-i", p, "-ac", "1", "-ar", String(SR), q]);
    s = readWavMono(q).samples;
  }
  let a = 0; while (a < s.length - 1 && Math.abs(s[a]) < 0.008) a++;
  const t = s.subarray(Math.max(0, a - Math.round(0.002 * SR)));
  let peak = 0; for (const v of t) peak = Math.max(peak, Math.abs(v));
  const g = peak > 1e-6 ? 0.95 / peak : 1;
  const out = new Float32Array(t.length);
  for (let i = 0; i < t.length; i++) out[i] = t[i] * g;
  BANK[name] = out;
}

const SAMPLES = resolve(LANE, "samples");
for (const f of readdirSync(SAMPLES))
  if (f.endsWith(".wav")) load(f.replace(/\.wav$/, ""), join(SAMPLES, f));

const DEMOS = resolve(REPO, "pop/demos/samples");
for (const [n, f] of Object.entries({
  hatC: "perc-hat-c.mp3", hatO: "perc-hat-o.mp3", clap: "perc-clap.mp3",
  ride: "perc-ride.mp3", snap: "perc-snap.mp3", sweep: "bed-noise-sweep.mp3",
})) load(n, join(DEMOS, f));

// ── space ─────────────────────────────────────────────────────────────
function spatial(az) {
  const itd = Math.round(0.00027 * SR * Math.sin(az));
  const shadow = 0.35 * Math.sin(az);
  return { itd, gl: 1 - shadow, gr: 1 + shadow };
}

function emit(bus, i, mono, pan, sp, sideAmt, dly = 0) {
  if (i < 0 || i >= N) return;
  const a = (Math.PI / 4) * (1 + pan);
  const cl = Math.cos(a), cr = Math.sin(a);
  if (bus === "drums") { drumsL[i] += mono * cl; drumsR[i] += mono * cr; }
  else if (bus === "vox") { voxL[i] += mono * cl; voxR[i] += mono * cr; }
  else { musicL[i] += mono * cl; musicR[i] += mono * cr; }
  if (dly) dlySend[i] += mono * dly;
  if (sp && sideAmt) {
    const li = i + sp.itd, ri = i - sp.itd;
    const l = li >= 0 && li < N ? mono * sp.gl : 0;
    const r = ri >= 0 && ri < N ? mono * sp.gr : 0;
    const s = 0.5 * (l - r) * sideAmt;
    if (bus === "vox") sideV[i] += s;
    else sideB[i] += s;
  }
}

// ── sidechain ─────────────────────────────────────────────────────────
const kicks = [];
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
      else { const u = clamp((dt - atk) / rel, 0, 1); g = (1 - depth) + depth * smooth(u); }
      if (g < e[j]) e[j] = g;
    }
  }
  return e;
}

// ── the thick electro kick — cult v5's, unchanged ─────────────────────
// Sweep 200→47 Hz at 62/s, two-stage envelope, tanh on the kick's OWN
// waveform (the only tanh in the render, nowhere near the master), a real
// transient, a 44 Hz sub layer. POW per hit; the master still sums clean.
const SATN = Math.tanh(2.4);
function kick(t, gain = 1, { weight = 1 } = {}) {
  if (!allow("kick")) return;
  kicks.push(t);
  EVENTS.push({ t: +t.toFixed(4), voice: "kick", bus: "drums", dur: 0.52, gain: +gain.toFixed(3) });
  const n = Math.round(0.52 * SR), i0 = Math.round(t * SR);
  let ph = 0, sub = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const f = 47 + 153 * Math.exp(-u * 62);
    ph += (TAU * f) / SR;
    sub += (TAU * 44) / SR;
    const slam = Math.exp(-u * 34), tail = Math.exp(-u * 7.0);
    const env = (0.62 * slam + 0.52 * tail) * Math.min(1, u / 0.0009);
    const body = Math.tanh(Math.sin(ph) * env * 2.4) / SATN;
    const low = Math.sin(sub) * Math.exp(-u * 5.6) * 0.40 * weight;
    const click = Math.exp(-u * 300) * 0.13 * Math.sin(TAU * 1600 * u)
      + Math.exp(-u * 760) * 0.075 * Math.sin(TAU * 3900 * u);
    emit("drums", i0 + i, (body + low + click) * 0.74 * gain * tailFade(i, n), 0, null, 0);
  }
}

// Sine bumps: fundamental + sub octave + a whisper of the 2nd.
function bass(t, midi, dur, gain = 1, slideFrom = null) {
  if (!allow("bass")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "bass", bus: "music", midi, dur: +dur.toFixed(3),
    gain: +gain.toFixed(3) });
  const n = Math.round((dur + 0.12) * SR), i0 = Math.round(t * SR);
  const f1 = hz(midi), f0 = slideFrom !== null ? hz(slideFrom) : f1;
  let p1 = 0, p2 = 0, p3 = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const glide = smooth(clamp(u / 0.075, 0, 1));
    const f = f0 + (f1 - f0) * glide;
    p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
    let env = Math.min(1, u / 0.012);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.12);
    const s = Math.sin(p1) + 0.52 * Math.sin(p2) + 0.10 * Math.sin(p3);
    lp += 0.50 * (s - lp);
    emit("music", i0 + i, lp * 0.40 * env * gain * tailFade(i, n), 0, null, 0);
  }
}

// Harmonized sines — the pad and the stab.
function sines(t, midis, dur, gain, pan, sideAmt = 0.5, bright = 1, dly = 0, attack = 0.020) {
  if (!allow(dur > 1 ? "pad" : "stab")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: dur > 1 ? "pad" : "stab", bus: "music", midis,
    dur: +dur.toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2) });
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

// ── the one-shot player ───────────────────────────────────────────────
const missing = new Set();
function shot(name, t, {
  gain = 1, pan = 0, semis = 0, bus = "drums", side = 0.35, dark = 0,
  dur = null, dly = 0, off = 0, evVoice = null,
} = {}) {
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  const c = classify(name);
  if (!allow(evVoice ?? c.voice)) return;
  const step = Math.pow(2, semis / 12);
  const start = Math.max(0, Math.min(s.length - 2, Math.round(off * SR)));
  const avail = Math.floor((s.length - 2 - start) / step);
  const n = dur ? Math.min(avail, Math.round(dur * SR)) : avail;
  if (n <= 4) return;
  const i0 = Math.round(t * SR);
  EVENTS.push({
    t: +t.toFixed(4), voice: evVoice ?? c.voice, bus, sample: name,
    dur: +(n / SR).toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2),
    ...(semis ? { semis } : {}),
  });
  const sp = spatial(pan * 1.2);
  let lp = 0, pos = start;
  for (let i = 0; i < n; i++) {
    const q = pos | 0;
    if (q + 1 >= s.length) break;
    const f = pos - q;
    let v = s[q] + (s[q + 1] - s[q]) * f;
    if (dark > 0) { lp += (1 - dark) * (v - lp); v = lp; }
    const env = Math.min(1, i / (0.0015 * SR));
    emit(bus, i0 + i, v * env * gain * tailFade(i, n), pan, sp, side, dly);
    pos += step;
  }
}
const sung = (name, t, o = {}) => shot(name, t, { bus: "vox", side: 0.6, ...o });

// ── harmony ───────────────────────────────────────────────────────────
// An A pedal with F and G colour — thirds left open everywhere, because
// the doo walk speaks C-natural and the tagline speaks C#, and this record
// refuses to referee that argument too. Four-bar cycle: A · A · F · G.
const ROOTS = [33, 33, 29, 31];                 // A1 A1 F1 G1
const rootAt = (bar) => ROOTS[bar % 4];
const fifth = (m) => [m + 12, m + 19];          // root+fifth, one octave up

// ── form ──────────────────────────────────────────────────────────────
const S = {
  intro: [0, 8], hookA: [8, 24], doo: [24, 32], hookB: [32, 48],
  breakd: [48, 56], hookC: [56, 80], outro: [80, 88],
};
const inS = (bar, k) => bar >= S[k][0] && bar < S[k][1];
const hookBar = (b) => inS(b, "hookA") || inS(b, "hookB") || inS(b, "hookC");
const kickOn = (b) => !inS(b, "breakd") && !(b >= 84);
const clapOn = (b) => inS(b, "hookB") || inS(b, "hookC");

// deterministic jitter — the humanizer, seeded so every render is the take
let seed = 20220327;                            // the primary take's post date
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);
const jit = (ms = 5) => ((rnd() - 0.5) * 2 * ms) / 1000;

// ── drums ─────────────────────────────────────────────────────────────
for (let b = 0; b < BARS; b++) {
  const t0 = at(b);
  if (kickOn(b)) {
    // the intro kick swells in over its eight bars — arrival, not a drop
    const g = inS(b, "intro") ? 0.30 + 0.70 * smooth(b / 8) : 1.0;
    for (let q = 0; q < 4; q++) kick(t0 + q * BEAT, g);
  }
  if (kickOn(b) && !inS(b, "intro")) {
    // closed hats on the offbeats; 16ths once the record is at its fullest
    const six = inS(b, "hookC");
    for (let q = 0; q < 4; q++) {
      shot("hatC", t0 + (q + 0.5) * BEAT + jit(3), { gain: 0.34, pan: 0.22, side: 0.4 });
      if (six) {
        shot("hatC", t0 + (q + 0.25) * BEAT + jit(3), { gain: 0.15, pan: -0.18, side: 0.4 });
        shot("hatC", t0 + (q + 0.75) * BEAT + jit(3), { gain: 0.15, pan: -0.18, side: 0.4 });
      }
    }
    if (clapOn(b)) {
      shot("clap", t0 + 1 * BEAT, { gain: 0.5, pan: -0.08, side: 0.5 });
      shot("clap", t0 + 3 * BEAT, { gain: 0.5, pan: 0.08, side: 0.5 });
      shot("hatO", t0 + 2.5 * BEAT, { gain: 0.26, pan: 0.3, side: 0.55 });
    }
    if (inS(b, "doo")) shot("ride", t0 + 2 * BEAT, { gain: 0.22, pan: 0.35, side: 0.5 });
    if (b % 4 === 3) shot("snap", t0 + 3.5 * BEAT, { gain: 0.2, pan: -0.3, side: 0.5 });
  }
  // one riser in the whole record: the noise sweep out of the breakdown
  if (b === 54) shot("sweep", t0, { gain: 0.30, pan: 0, side: 0.6, evVoice: "sweep", bus: "music", dur: 2 * BAR });
}

// ── bass ──────────────────────────────────────────────────────────────
// Offbeat sine bumps under the hooks — the classic house push — walking
// the A·A·F·G cycle. The breakdown gets one long held root instead.
for (let b = 0; b < BARS; b++) {
  const t0 = at(b);
  if (inS(b, "breakd")) {
    if (b % 4 === 0) bass(t0, 33, 4 * BAR * 0.98, 0.5);
    continue;
  }
  if (inS(b, "intro") && b < 4) continue;
  const root = rootAt(b);
  for (let q = 0; q < 4; q++)
    bass(t0 + (q + 0.5) * BEAT, root + 12, 0.30, inS(b, "intro") ? 0.5 : 0.72);
  // a low anchor on the downbeat of each chord change
  if (b % 4 === 2 || b % 4 === 0) bass(t0, root, 0.42, 0.55);
}

// ── pads ──────────────────────────────────────────────────────────────
// Open fifths, two bars at a time, breathing with the duck. The breakdown
// swells brighter and adds the ninth (B) — colour, still no third.
for (let b = 0; b < BARS; b += 2) {
  const t0 = at(b);
  const root = rootAt(b) + 24;                   // A3-region
  const brk = inS(b, "breakd");
  const g = inS(b, "intro") ? 0.30 : brk ? 0.42 : inS(b, "outro") ? 0.26 : 0.20;
  const notes = brk ? [root, root + 7, root + 14] : fifth(root);
  sines(t0 + 0.01, notes, 2 * BAR * 0.96, g, b % 4 ? 0.3 : -0.3, 0.6, brk ? 1.4 : 1.0, brk ? 0.10 : 0.05, 0.6);
}

// ── the argument ──────────────────────────────────────────────────────
// Two bars of call, two bars of answer, all through the hooks. The call
// fills its bar exactly (1.96 s at 122); the answer is short and gets the
// second half of its bar to ring in the delay — an argument needs air to
// hang in or it's just noise.
const CALL_PAN = -0.55, ANS_PAN = 0.55;
function argument(b, { high = false, mid = false, dense = false } = {}) {
  const t0 = at(b);
  sung("its-too-hot", t0 + jit(4), { pan: CALL_PAN, gain: 0.95, dly: 0.10 });
  const ans = high ? "no-its-not-high" : mid ? "no-its-not-mid" : "no-its-not";
  sung(ans, at(b + 1) + jit(4), { pan: ANS_PAN, gain: high ? 0.85 : 0.95, dly: 0.22 });
  // the answer doubles down on beat 3 — "not." — thrown further out
  sung("not", at(b + 1, 2) + jit(4), { pan: ANS_PAN * 1.3, gain: 0.55, dly: 0.30, dark: 0.2 });
  if (dense) {
    // at full tilt the call won't let it go either
    sung("hot", at(b + 1, 3) + jit(4), { pan: CALL_PAN * 1.2, gain: 0.5, dly: 0.24, dark: 0.15 });
  }
}

for (let b = S.hookA[0]; b < S.hookA[1]; b += 2)
  argument(b, { mid: ((b - S.hookA[0]) / 2) % 4 === 3 });
for (let b = S.hookB[0]; b < S.hookB[1]; b += 2)
  argument(b, { high: ((b - S.hookB[0]) / 2) % 2 === 1 });
for (let b = S.hookC[0]; b < S.hookC[1]; b += 2)
  argument(b, { high: ((b - S.hookC[0]) / 2) % 2 === 1, dense: b >= 64 });

// intro teasers: single words flickering at the edges before the argument
// has even started — you hear the weather before you hear the fight
for (const [b, w, p] of [[2, "hot", -0.7], [3, "not", 0.7], [5, "hot", -0.7],
  [6, "not", 0.7], [7, "no", 0.7]])
  sung(w, at(b, 3) + jit(6), { pan: p, gain: 0.42, dark: 0.35, dly: 0.30 });

// ── the doo walk ──────────────────────────────────────────────────────
// The whistled-adjacent material: F F G G E D C, sung. It leads its own
// eight bars and comes back under the breakdown. Individual doo notes
// echo across the field on the offbeats — the walk answering itself.
for (let b = S.doo[0]; b < S.doo[1]; b += 4) {
  sung("doo-run", at(b) + jit(4), { pan: -0.2, gain: 0.9, dly: 0.18 });
  sung("doo-f", at(b + 1, 2.5), { pan: 0.5, gain: 0.4, dly: 0.28, dark: 0.2 });
  sung("doo-g", at(b + 2, 1.5), { pan: -0.5, gain: 0.4, dly: 0.28, dark: 0.2 });
  sung("doo-ed", at(b + 2, 3.5), { pan: 0.55, gain: 0.4, dly: 0.28, dark: 0.2 });
  sung("doo-c", at(b + 3, 1.5), { pan: -0.4, gain: 0.42, dly: 0.28, dark: 0.2 });
  sung("doo-run-b", at(b + 3, 2) + jit(4), { pan: 0.35, gain: 0.5, dark: 0.3, dly: 0.2 });
}

// ── the tagline — the making-up ───────────────────────────────────────
// "Now I'm back in season" (E4 D4 C#4 B3 A3) owns the breakdown, twice,
// answered by the doo walk. From bar 72 it returns OVER the argument —
// the record's only stacked truth — and the outro gives the last word to
// "season" alone.
sung("season-line", at(48, 0.5), { pan: 0, gain: 1.0, dly: 0.16 });
sung("doo-run", at(50, 1) + jit(4), { pan: -0.45, gain: 0.55, dark: 0.25, dly: 0.22 });
sung("season-line", at(52, 0.5), { pan: 0.15, gain: 1.0, dly: 0.16 });
sung("season-line-b", at(54, 1), { pan: -0.5, gain: 0.5, dark: 0.3, dly: 0.26 });

for (let b = 72; b < 80; b += 4) {
  sung("season-line", at(b, 0.5) + jit(4), { pan: 0, gain: 0.9, dly: 0.14 });
  sung("season-b", at(b + 2, 2) + jit(4), { pan: -0.35, gain: 0.5, dark: 0.3, dly: 0.2 });
}

// ── outro: the humans ─────────────────────────────────────────────────
// The original take speaks once, unprocessed — an argument was three
// friends in a yard all along — and "season" rings out through the delay.
sung("chant-full", at(80, 0.5), { pan: 0, gain: 0.95, side: 0.4 });
sung("no-its-not-high", at(82, 2), { pan: 0.5, gain: 0.5, dly: 0.34 });
sung("season-line", at(83, 0.5), { pan: -0.1, gain: 0.85, dly: 0.2 });
sung("season", at(85, 2), { pan: 0.1, gain: 0.8, dly: 0.45 });
sung("season-b", at(86, 3), { pan: -0.3, gain: 0.45, dark: 0.35, dly: 0.5 });

if (missing.size) console.warn("  ! missing samples:", [...missing].join(", "));

// ── dub delay ─────────────────────────────────────────────────────────
// Dotted-eighth ping-pong, 2.6 kHz damp in the loop, 180 Hz highpass on
// the return. This is the record's reverb. The return is narrowed to 0.7
// width before it lands: a HARD ping-pong parks each repeat wholly in one
// channel, which folds to mono at exactly −3 dB — and in the outro the
// tail is most of what's left, so qc.mjs's worst fold-down window was the
// last two seconds of the record until this blend.
{
  const D = Math.round(0.75 * BEAT * SR);
  const FB = 0.42;
  const damp = 1 - Math.exp((-TAU * 2600) / SR);
  const hpRc = 1 / (TAU * 180), hpA = hpRc / (hpRc + 1 / SR);
  const bL = new Float32Array(N + D + 1), bR = new Float32Array(N + D + 1);
  let dL = 0, dR = 0, hpL = 0, hpR = 0, pL = 0, pR = 0;
  for (let i = 0; i < N; i++) {
    const tapL = i >= D ? bL[i - D] : 0;
    const tapR = i >= D ? bR[i - D] : 0;
    dL += damp * (tapR - dL);
    dR += damp * (tapL - dR);
    bL[i] = dlySend[i] + dR * FB;
    bR[i] = dL * FB;
    hpL = hpA * (hpL + bL[i] - pL); pL = bL[i];
    hpR = hpA * (hpR + bR[i] - pR); pR = bR[i];
    const mid = 0.5 * (hpL + hpR), side = 0.5 * (hpL - hpR) * 0.7;
    musicL[i] += (mid + side) * 0.50;
    musicR[i] += (mid - side) * 0.50;
  }
}

// ── the duck ──────────────────────────────────────────────────────────
// Kick only, depth 0.55 — a dance record breathes harder than a chill one.
const bedEnv = buildEnv(kicks.map((t) => ({ t, depth: 0.55, atk: 0.009, rel: 0.30 })));

// ── stems ─────────────────────────────────────────────────────────────
const writeStereo = (path, L, R) => {
  const bytes = N * 2 * 4;
  const buf = Buffer.alloc(44 + bytes);
  buf.write("RIFF", 0, "ascii"); buf.writeUInt32LE(36 + bytes, 4); buf.write("WAVE", 8, "ascii");
  buf.write("fmt ", 12, "ascii"); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(2, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 8, 28);
  buf.writeUInt16LE(8, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36, "ascii"); buf.writeUInt32LE(bytes, 40);
  for (let i = 0; i < N; i++) {
    buf.writeFloatLE(L[i], 44 + i * 8);
    buf.writeFloatLE(R[i], 44 + i * 8 + 4);
  }
  writeFileSync(path, buf);
};
if (process.argv.includes("--stems")) {
  const dir = resolve(OUT, "stems");
  mkdirSync(dir, { recursive: true });
  const mk = (L, R, env, g) => {
    const a = new Float32Array(N), b = new Float32Array(N);
    for (let i = 0; i < N; i++) { a[i] = L[i] * (env ? env[i] : 1) * g; b[i] = R[i] * (env ? env[i] : 1) * g; }
    return [a, b];
  };
  const voxDuck = new Float32Array(N);
  for (let i = 0; i < N; i++) voxDuck[i] = Math.pow(bedEnv[i], 0.25);
  writeStereo(resolve(dir, "v1-vox.wav"), ...mk(voxL, voxR, voxDuck, VOXG));
  writeStereo(resolve(dir, "v1-music.wav"), ...mk(musicL, musicR, bedEnv, 1));
  writeStereo(resolve(dir, "v1-drums.wav"), ...mk(drumsL, drumsR, null, 1));
  console.log(`  stems → ${dir}`);
}

// ── Special Sign side return ──────────────────────────────────────────
// Band-limited 80 Hz – 11.5 kHz, handed back antisymmetrically on a slewed
// send. Wide through the hooks, narrower in the breakdown — and NOT widest
// at the end: in the outro the delay tail is most of what's left, and a
// wide send over a vanishing dry mix is exactly the thing a phone speaker
// folds away (qc.mjs measured −3 dB in that window at 0.92).
const sideOut = new Float32Array(N);
{
  const hpRc = 1 / (TAU * 80), hpA = hpRc / (hpRc + 1 / SR);
  const lpK = 1 - Math.exp((-TAU * 11500) / SR);
  let hp = 0, lp = 0, prev = 0, send = 0.8;
  for (let i = 0; i < N; i++) {
    const be = bedEnv[i];
    const s = sideB[i] * be + sideV[i] * Math.pow(be, 0.25);
    hp = hpA * (hp + s - prev); prev = s;
    lp += lpK * (hp - lp);
    const bar = (i / SR) / BAR;
    const target =
      bar < S.hookA[0] ? 0.85 :
      bar < S.doo[0] ? 0.72 :
      bar < S.hookB[0] ? 0.85 :
      bar < S.breakd[0] ? 0.68 :
      bar < S.hookC[0] ? 0.55 :
      bar < S.outro[0] ? 0.72 : 0.55;
    send += 0.00004 * (target - send);
    sideOut[i] = lp * send;
  }
}

// ── sum ───────────────────────────────────────────────────────────────
// Clean: duck, fade, measure, ONE linear trim. No master tanh anywhere.
let peak = 0;
const L = new Float32Array(N), R = new Float32Array(N);
for (let i = 0; i < N; i++) {
  const be = bedEnv[i];
  const dv = Math.pow(be, 0.25);
  const fadeIn = Math.min(1, i / (0.014 * SR));
  const fadeOut = Math.min(1, (N - 1 - i) / (2.4 * SR));
  const fade = Math.max(0, Math.min(fadeIn, fadeOut));
  const l = (musicL[i] * be + voxL[i] * dv * VOXG + drumsL[i] + sideOut[i]) * fade;
  const r = (musicR[i] * be + voxR[i] * dv * VOXG + drumsR[i] - sideOut[i]) * fade;
  L[i] = l; R[i] = r;
  if (Math.abs(l) > peak) peak = Math.abs(l);
  if (Math.abs(r) > peak) peak = Math.abs(r);
}
const norm = peak > 1e-9 ? 0.92 / peak : 1;
for (let i = 0; i < N; i++) { L[i] *= norm; R[i] *= norm; }
console.error(`# pre-master peak ${peak.toFixed(6)} · linear trim ${norm.toFixed(3)}`);

const outWav = resolve(OUT, "season-remix-v1.wav");
writeStereo(outWav, L, R);

EVENTS.sort((a, b) => a.t - b.t || (a.bus < b.bus ? -1 : 1));
const voiceCounts = {};
for (const e of EVENTS) voiceCounts[e.voice] = (voiceCounts[e.voice] ?? 0) + 1;
const clock = (sec) => `${Math.floor(sec / 60)}:${String(Math.floor(sec % 60)).padStart(2, "0")}`;
console.log(`  ${EVENTS.length} events · ${Object.entries(voiceCounts).map(([k, v]) => `${k} ${v}`).join(" · ")}`);
writeFileSync(resolve(OUT, "season-remix-v1.events.json"), JSON.stringify({
  schema: "aesthetic.computer/pop-events/v1",
  track: "whistlegraph season --- remix (v1)",
  renderer: "pop/season/bin/render.mjs",
  source: {
    work: "h0t — it's too hot / no it's not / now I'm back in season (Whistlegraph, spring 2022)",
    takes: {
      primary: "7079639110025088298 — all four seasons in 9 seconds",
      high: "7080453509149134126 — spring flower (the A4 answer)",
      mid: "7087134943930846506 — springy vibesies",
    },
    mirror: "https://assets.aesthetic.computer/whistlegraph/index/posts/",
    measured: {
      taglineNotes: "E4 D4 C#4 B3 A3 — lands on A",
      dooWalk: "F4 F4 G4 G4 E4 D4 C4",
      chantPhrase: "1.96 s = one bar at 122.4 BPM",
      takeWalk: "~103 BPM (beat_track + doo IOIs)",
    },
  },
  harmony: {
    key: "A, third left open (root+fifth everywhere)",
    why: "the doo walk carries C-natural, the tagline carries C# — an open fifth is the only floor both can stand on",
    cycle: "A · A · F · G, one bar each",
  },
  tempoBPM: BPM, bars: BARS, seconds: +(BARS * BAR).toFixed(2),
  sections: Object.entries(S).map(([k, [a, b]]) => ({
    key: k, bars: [a, b],
    start: +at(a).toFixed(3), end: +at(b).toFixed(3),
    clock: [clock(at(a)), clock(at(b))],
  })),
  buses: {
    music: "bass, pads, dub-delay return — ducks with the kick (depth 0.55)",
    drums: "kick, hats, claps, ride, snaps — never ducks",
    vox: "the argument, the doo walk, the tagline — light duck (bedEnv^0.25), +3 dB makeup",
  },
  eventCounts: voiceCounts,
  events: EVENTS,
  prePeak: +peak.toFixed(6), linearTrim: +norm.toFixed(4),
  kicks: kicks.length,
}, null, 2));
console.log(`✓ ${outWav}`);
