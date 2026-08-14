#!/usr/bin/env node
// render4.mjs — "whistlegraph cult --- remix (v2, chill)"
//
// v1 was a club banger. @jeffrey heard it and asked for four things:
//
//   1. "we should be extending their words — use our whole
//      speech-to-singing pipeline"
//   2. "the orchestration is weird"
//   3. "can we like rethink the whole track melody"
//   4. "lets use some of our chord progression experiments"
//      … and, over the top of all of it: "make it more cool chill techno".
//
// So: same source, same sample bank, new everything else.
//
// (1) Every lead line in this render is a SUNG note, not a chopped speech
//     hit. bin/sing.mjs runs each word through bin/sing.py — the Saitou
//     recipe on the WORLD vocoder — which lengthens the phoneme to fit the
//     note and REPLACES the f0 contour rather than shifting it. A 0.31 s
//     chanted "dash" comes back as a 1.50 s sung F#4 that actually holds.
//     The three performers' dashes are rendered separately at the same
//     pitch and stacked with 28 ms offsets, so one syllable becomes a
//     chord sung by the three people who recorded it.
//
// (2)+(3) New form, new melody. 120 BPM — which is where the source chant
//     was already sitting, so nothing is dragged to fit — 120 bars, 4:00.
//     No drops, no snare-roll builds, no risers, no crashes. Chill techno
//     is subtraction and patience: the arrangement moves by adding and
//     removing one element at a time over a kick that mostly just stays.
//
// (4) The harmony comes from the repo's own experiment rather than a
//     generic loop: PROGRESSIONS_CHILL in recap/bin/trance.mjs, the
//     8-progression pool documented in pop/dance/cutezenwaltzi.md as
//     "chill walks an 8-progression pool ... so it loops far less and
//     travels more harmonically". Four of its rows, two bars a chord,
//     make a 32-bar harmonic cycle that never quite repeats where you
//     expect.
//
// And the caption — "dot dot dot dash dash dash dot dot dot (jk)" — is
// SOS, so the SOS FIGURE is a real four-bar riff: three short sung notes,
// three long ones, three short. Long/short is what the words already are.
//
// Mixing keeps every rule from pop/teknull/c/taksmukkeklokken-smooch.c:
// 10 ms raised-cosine tails on every voice, a duck that ramps rather than
// steps, sine-bump bass (fundamental + sub + a whisper of 2nd), no master
// tanh, mono-safe equal-power pans with a band-limited antisymmetric side
// return. New for chill: a dotted-eighth ping-pong dub delay, damped in
// the loop, because that is the genre's actual reverb.
//
//   node pop/cult/bin/render2.mjs      # → out/cult-remix-v2.wav (f32)
//   ./pop/cult/bin/render-v2.sh        # + master to mp3 / 24-bit

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
const BPM = 120;                     // the tempo the source chant already had
const BEAT = 60 / BPM;               // 0.5 s
const BAR = 4 * BEAT;                // 2.0 s
const BARS = 120;                    // 240 s = 4:00
const TAU = Math.PI * 2;
const N = Math.round((BARS * BAR + 3.2) * SR);

// Four buses. Drums never duck. The harmonic bed takes the full sidechain.
// The sung voices take a quarter-depth duck (they are the song, and the
// hook lands on downbeats). The delay return is its own bus so the dub
// tails keep breathing after the dry signal has stopped.
const musicL = new Float64Array(N), musicR = new Float64Array(N);
const drumsL = new Float64Array(N), drumsR = new Float64Array(N);
const voxL = new Float64Array(N), voxR = new Float64Array(N);
const sideB = new Float64Array(N);
const dlySend = new Float64Array(N);

const VOXG = 1.42;                   // +3 dB on the sung bus — see the sum
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

// samples/ = v1's slices (kept, untouched); sung/ = the speech-to-singing
// renders, already at pitch and already the right length.
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

// ── space ─────────────────────────────────────────────────────────────
function spatial(az) {
  const itd = Math.round(0.00027 * SR * Math.sin(az));
  const shadow = 0.35 * Math.sin(az);
  return { itd, gl: 1 - shadow, gr: 1 + shadow };
}

function emit(bus, i, mono, pan, sp, sideAmt, dly = 0) {
  if (i < 0 || i >= N) return;
  const a = (Math.PI / 4) * (1 + pan);
  const L = bus === "drums" ? drumsL : bus === "vox" ? voxL : musicL;
  const R = bus === "drums" ? drumsR : bus === "vox" ? voxR : musicR;
  L[i] += mono * Math.cos(a);
  R[i] += mono * Math.sin(a);
  if (dly) dlySend[i] += mono * dly;
  if (sp && sideAmt) {
    const li = i + sp.itd, ri = i - sp.itd;
    const l = li >= 0 && li < N ? mono * sp.gl : 0;
    const r = ri >= 0 && ri < N ? mono * sp.gr : 0;
    sideB[i] += 0.5 * (l - r) * sideAmt;
  }
}

// ── sidechain ─────────────────────────────────────────────────────────
// Shallower and slower than v1's club duck. Chill techno wants a breath,
// not a pump. It still RAMPS to the floor over ~9 ms — a stepped duck is
// a click with extra steps.
const kicks = [];
function duckAt(t) {
  let d = 1;
  for (const k of kicks) {
    const dt = t - k;
    if (dt < -0.010 || dt > 0.40) continue;
    let g;
    if (dt < 0.009) g = 1 - 0.50 * clamp((dt + 0.010) / 0.019, 0, 1);
    else {
      const u = clamp((dt - 0.009) / 0.31, 0, 1);
      g = 0.50 + 0.50 * (u * u * (3 - 2 * u));
    }
    d = Math.min(d, g);
  }
  return d;
}

// ── voices ────────────────────────────────────────────────────────────
// Softer kick than v1: less transient click, a touch longer in the body,
// so it reads as a pulse rather than a punch.
function kick(t, gain = 1) {
  kicks.push(t);
  logEvent({ t: +t.toFixed(4), voice: "kick", gain: +gain.toFixed(3), bus: "drums" });
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

// Sine bumps: fundamental + sub octave + a whisper of the 2nd, through a
// one-pole. Nothing plucked, nothing twangy, nothing to clip.
function bass(t, midi, dur, gain = 1, slideFrom = null) {
  logEvent({ t: +t.toFixed(4), voice: "bass", midi, dur: +dur.toFixed(3), gain: +gain.toFixed(3), bus: "music" });
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

// Harmonized sines — the pad and the stab. Detuned pairs, gentle one-pole,
// no presence or air boost anywhere.
function sines(t, midis, dur, gain, pan, sideAmt = 0.5, bright = 1, dly = 0, attack = 0.020) {
  logEvent({ t: +t.toFixed(4), voice: "sines", midi: midis[0], dur: +dur.toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2), bus: "music" });
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

// One-shot player. `semis` is here for percussion only — the sung bank is
// already at pitch and always plays at semis 0.
function shot(name, t, { gain = 1, pan = 0, semis = 0, bus = "drums", side = 0.35, dark = 0, dur = null, dly = 0, off = 0 } = {}) {
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  const step = Math.pow(2, semis / 12);
  logEvent({ t: +t.toFixed(4), voice: voiceOf(name), name, gain: +(gain).toFixed(3),
    bus, pan: +pan.toFixed(2), ...(dur ? { dur: +dur.toFixed(3) } : {}),
    ...(semis ? { semis } : {}), ...(whoOf(name) ? { who: whoOf(name) } : {}) });
  const start = Math.max(0, Math.min(s.length - 2, Math.round(off * SR)));
  const avail = Math.floor((s.length - 2 - start) / step);
  const n = dur ? Math.min(avail, Math.round(dur * SR)) : avail;
  const i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let lp = 0, pos = start;
  for (let i = 0; i < n; i++, pos += step) {
    const q = pos | 0;
    if (q + 1 >= s.length) break;
    const f = pos - q;
    let v = s[q] + (s[q + 1] - s[q]) * f;
    if (dark > 0) { lp += (1 - dark) * (v - lp); v = lp; }
    const env = Math.min(1, i / (0.0015 * SR));
    emit(bus, i0 + i, v * env * gain * tailFade(i, n), pan, sp, side, dly);
  }
}
const missing = new Set();

// ── the score ledger ──────────────────────────────────────────────────
// Every hit lands here as well as in the buffers, so bin/score-video.mjs can
// draw the exact performance the ear hears (the pattern femrag++ uses). The
// vocal takes are named by performer, so `who` makes the three-people-on-one-
// pitch stacking visible, not just audible.
const EVENTS = [];
const whoOf = (name) => /camille/i.test(name) ? "camille"
  : /alex/i.test(name) ? "alex"
  : /jeffrey|bassdash/i.test(name) ? "jeffrey" : undefined;
const voiceOf = (name) => {
  const n = name.toLowerCase();
  if (/^hat/.test(n)) return "hat";
  if (/snare|snap/.test(n)) return "snare";
  if (/clap/.test(n)) return "clap";
  if (/ride|tambo/.test(n)) return "hat";
  if (/block/.test(n)) return "tap";
  if (/sweep/.test(n)) return "skid";
  if (/dash/.test(n)) return "dash";
  if (/cult/.test(n)) return "cult";
  if (/dot/.test(n)) return "dot";
  if (/runreal|runit|iwanna|away|hook/.test(n)) return "hook";
  return "vox";
};
const logEvent = (e) => { if (e.t >= 0) EVENTS.push(e); };
const sung = (name, t, o = {}) => shot(name, t, { bus: "vox", side: 0.6, ...o });

// ── voice as material ─────────────────────────────────────────────────
// @jeffrey: "cut the voice more into a material". Grains are lifted from
// anywhere inside a take — mid-vowel, mid-consonant — and retriggered on the
// 16th grid, pitched to the running chord and thrown wide. The word stops
// being a word; what is left is the grain of the three of them as rhythm.
// Deterministic: the gaps and offsets come from the bar number, not random.
function material(bar, name, g = 1, {
  steps = 16, grain = 0.070, dark = 0.18, side = 0.85, dly = 0.22, span = 0.9,
} = {}) {
  const src = BANK[name];
  if (!src) { missing.add(name); return; }
  const len = src.length / SR;
  const tri = triad(degAt(bar));
  for (let k = 0; k < steps; k++) {
    if ((k * 7 + bar * 3) % 5 === 0) continue;          // holes make the groove
    const t = at(bar) + k * (BEAT / 4);
    const off = ((k * 5 + bar * 3) % 11) / 11 * Math.max(0, len - grain) * span;
    const semis = tri[(k + bar) % tri.length] - tri[0]  // ride the chord
      + (k % 8 === 4 ? 12 : 0);                          // an octave flick
    const accent = k % 4 === 0 ? 1.0 : k % 2 === 0 ? 0.72 : 0.52;
    shot(name, t, {
      bus: "vox", gain: 0.30 * g * accent, semis,
      pan: (k % 2 ? 0.55 : -0.55) * (0.6 + 0.4 * ((k % 3) / 2)),
      side, dark, dly, off, dur: grain * (k % 4 === 0 ? 1.6 : 1),
    });
  }
}


// Granular stretch — v1's, kept for texture only. The lead lines are sung
// by WORLD now, not stretched.
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
// B natural minor. Chords come from PROGRESSIONS_CHILL in
// recap/bin/trance.mjs — the 8-row pool cutezenwaltzi.md describes as
// walking "far less" of a loop. Four of its rows, two bars a chord:
//
//   bars  0– 7   row 0  [0,5,2,6]   i  VI III VII   Bm G  D  A
//   bars  8–15   row 5  [0,6,3,5]   i  VII iv  VI   Bm A  Em G
//   bars 16–23   row 4  [0,4,5,3]   i  v  VI  iv    Bm F#m G  Em
//   bars 24–31   row 2  [0,2,5,3]   i  III VI  iv   Bm D  G  Em
//
// 32 bars before it comes round, which at 120 BPM is a full minute — long
// enough that the ear reads travel, not loop.
const SCALE = [0, 2, 3, 5, 7, 8, 10];
const sd = (i) => SCALE[((i % 7) + 7) % 7] + 12 * Math.floor(i / 7);
const ROWS = [[0, 5, 2, 6], [0, 6, 3, 5], [0, 4, 5, 3], [0, 2, 5, 3]];
const degAt = (bar) => ROWS[Math.floor(bar / 8) % ROWS.length][Math.floor((bar % 8) / 2)];
const bassRoot = (deg) => 35 + sd(deg);                        // B1 = 35
const triad = (deg, base = 59) => [sd(deg), sd(deg + 2), sd(deg + 4)].map((s) => base + s);

// Which sung "cult" holds are in this chord. The choir is literally one
// word — Camille's sung "cult" — rendered at eleven pitches and picked
// from here, so a whole pad section is one syllable.
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

// ── form ──────────────────────────────────────────────────────────────
// 120 bars, no drop anywhere. The shape is one element at a time.
const S = {
  air: [0, 8],          // sung "cult" drone, no drums
  pulse: [8, 16],       // kick + offbeat sub arrive under the drone
  morse: [16, 24],      // the SOS figure, twice
  hookA: [24, 40],      // the hook, four times
  hollow: [40, 48],     // kick out — hats, and "the three of us" sung
  hookB: [48, 64],      // hook again, SOS dots answering
  drift: [64, 76],      // no hook: pure harmonic travel, dub stabs, ride
  hookC: [76, 96],      // fullest — hook five times, open hats
  descent: [96, 104],   // peel off
  ebb: [104, 112],      // drums out, the original chant returns
  out: [112, 120],      // drone walks out
};
const inS = (bar, k) => bar >= S[k][0] && bar < S[k][1];
const kickOn = (b) => !(inS(b, "air") || inS(b, "hollow") || inS(b, "ebb") || inS(b, "out"));
const hatOn = (b) => kickOn(b) || inS(b, "hollow");
const hookSection = (b) => inS(b, "hookA") || inS(b, "hookB") || inS(b, "hookC") || inS(b, "descent");

let seed = 20220120;                                   // the cult post date
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);
const jit = (ms = 6) => ((rnd() - 0.5) * 2 * ms) / 1000;
const vel = (spread = 0.20) => 1 - rnd() * spread;

// ── THE HOOK, SUNG ────────────────────────────────────────────────────
// Four bars now instead of two, because the words are being held and a
// held word needs the room. Every syllable below came out of sing.py.
//
//   0.00  dash ──────── F#4, 1.50 s   three performers stacked
//   1.50  i wanna       D4 → E4
//   2.00  dash ──────── D4,  1.50 s
//   3.50  i wanna       B3 → C#4
//   4.00  run it fast   G4 → F#4 → D4 (D4 held 1.20 s)
//   6.00  dot           B3
//   6.50  dot           F#3
//   7.00  ── rest ──
function dashStack(t, which, G) {
  // One syllable, three people, one pitch, 28 ms apart. This is the
  // "stack one voice at root/3rd/5th with small offsets" trick except the
  // voices are real and the unison is the point.
  sung(`dash-camille-${which}-hold`, t + 0.000 + jit(3), { gain: 0.50 * G, pan: -0.42, side: 0.70, dly: 0.10 });
  sung(`dash-alex-${which}-hold`, t + 0.028 + jit(3), { gain: 0.47 * G, pan: 0.42, side: 0.70, dly: 0.10 });
  sung(`dash-jeffrey-${which}-hold`, t + 0.056 + jit(3), { gain: 0.58 * G, pan: 0.00, side: 0.38, dark: 0.22, dly: 0.08 });
}

function hook(bar, { full = true } = {}) {
  const t = at(bar);
  const G = full ? 1.0 : 0.70;
  dashStack(t + 0.00, "fs4", G);
  sung("iwanna-a-sung", t + 1.50 + jit(4), { gain: 0.82 * G, pan: -0.18, side: 0.50, dly: 0.20 });
  dashStack(t + 2.00, "d4", G * 0.95);
  sung("iwanna-b-sung", t + 3.50 + jit(4), { gain: 0.82 * G, pan: 0.18, side: 0.50, dly: 0.20 });
  // v4: @jeffrey — "replace 'it' with 'real'". The v3 sung bank already
  // has it, and the take is the same 2.000 s, so it drops straight in.
  // v4: the long-"real" take — run · reeeeeaaaal (2.0 s) · fast. At 2.80 s it
  // rings on under the dot answers at +6.00, which is the point: the held
  // vowel is still going when the dots reply to it.
  sung("runrealfast-long-hi", t + 4.00 + jit(4), { gain: 0.96 * G, pan: 0.00, side: 0.45, dly: 0.26 });
  sung("dot-b3", t + 6.00 + jit(3), { gain: 0.90 * G, pan: -0.45, side: 0.75, dly: 0.38 });
  sung("dot-fs3", t + 6.50 + jit(3), { gain: 0.90 * G, pan: 0.45, side: 0.75, dly: 0.38 });
  if (full) sung("dot-d4", t + 7.00, { gain: 0.34 * G, pan: 0.0, side: 0.60, dly: 0.55 });
}

// ── THE SOS FIGURE ────────────────────────────────────────────────────
// ··· ——— ··· — the caption, played. Four bars: three short sung notes,
// three long ones, three short. It works as a riff because morse's
// long/short is exactly the contrast a chill techno lead lives on.
function sos(bar, g = 1) {
  const t = at(bar);
  for (const [k, n, p] of [[0, "dot-b3", -0.32], [1, "dot-fs3", 0.0], [2, "dot-d4", 0.32]])
    sung(n, t + k * BEAT + jit(3), { gain: 0.74 * g, pan: p, side: 0.72, dly: 0.30 });
  sung("sos-dash-d4", t + 2.0 + jit(4), { gain: 0.70 * g, pan: -0.26, side: 0.62, dly: 0.20 });
  sung("sos-dash-fs4", t + 3.5 + jit(4), { gain: 0.70 * g, pan: 0.26, side: 0.62, dly: 0.20 });
  sung("sos-dash-e4", t + 5.0 + jit(4), { gain: 0.68 * g, pan: 0.00, side: 0.62, dly: 0.20 });
  for (const [k, n, p] of [[0, "dot-d4", 0.32], [1, "dot-a3", 0.0], [2, "dot-b3", -0.32]])
    sung(n, t + 6.5 + k * BEAT + jit(3), { gain: 0.64 * g, pan: p, side: 0.78, dly: 0.42 });
}

// The choir: the sung "cult" held four seconds at three chord tones, with
// 45 ms offsets. Two bars per call.
function choir(bar, g) {
  const t = at(bar);
  const picks = choirFor(degAt(bar));
  const gains = [0.46, 0.38, 0.30], pans = [0.0, -0.50, 0.50], sides = [0.40, 0.85, 0.85];
  picks.forEach((nm, i) => sung(`cult-${nm}`, t + i * 0.045, {
    gain: g * (gains[i] ?? 0.28), pan: pans[i] ?? 0, side: sides[i] ?? 0.7, dark: 0.32,
  }));
}

// ── score ─────────────────────────────────────────────────────────────
console.log(`→ scoring ${BARS} bars @ ${BPM} BPM · B minor · chill techno · ${(BARS * BAR).toFixed(1)}s`);

for (let bar = 0; bar < BARS; bar++) {
  const t = at(bar);
  const deg = degAt(bar);
  const root = bassRoot(deg);
  const chord = triad(deg);
  const four = bar % 4, eight = bar % 8;
  const dense = inS(bar, "hookC") || inS(bar, "hookB");

  // ---- kick: 4/4, and it just stays -----------------------------------
  if (kickOn(bar)) {
    const g = inS(bar, "pulse") ? 0.62 + 0.04 * (bar - S.pulse[0])
      : inS(bar, "descent") ? 0.86 - 0.06 * (bar - S.descent[0]) : 0.92;
    for (let b = 0; b < 4; b++) kick(t + b * BEAT + jit(2.5), g * (b === 0 ? 1 : 0.96));
  }

  // ---- hats: offbeat eighths, quiet -----------------------------------
  if (hatOn(bar)) {
    for (let s = 0; s < 8; s++) {
      const swing = s & 1 ? 0.035 : 0;                 // a whisper of shuffle
      const u = t + (s * 0.5 + swing) * BEAT + jit(5);
      if (s & 1) shot("hatC", u, { gain: 0.20 * vel(), pan: 0.20, side: 0.5, dur: 0.085 });
      else if (s % 4 === 2) shot("hatC", u, { gain: 0.11 * vel(), pan: -0.18, side: 0.5, dur: 0.065 });
    }
    // 16th ghosts only where the track is at its fullest — this is the
    // only "build" the arrangement gets, and it never resolves in a drop.
    if (dense && eight >= 4)
      for (let s = 0; s < 16; s++)
        if (s % 4 === 1 || s % 4 === 3)
          shot("hatC", t + (s / 16) * BAR + jit(4), { gain: 0.055 * vel(0.5), pan: (s & 2 ? 0.35 : -0.35), side: 0.6, dur: 0.045 });
    if ((dense || inS(bar, "drift")) && four === 3)
      shot("hatO", t + 3.5 * BEAT + 0.02, { gain: 0.20, pan: -0.28, side: 0.65, dur: 0.34 });
  }

  // ---- rim on 3, clap only when the track is full ---------------------
  if (kickOn(bar)) {
    shot("block", t + 2 * BEAT + jit(5), { gain: 0.17 * vel(), pan: 0.26, side: 0.6, dur: 0.10 });
    if (dense) {
      shot("clap", t + 2 * BEAT + jit(5), { gain: 0.22, pan: -0.12, side: 0.72 });
      shot("snap", t + 2 * BEAT + 0.012, { gain: 0.09, pan: 0.30, side: 0.55 });
    }
  }
  if (inS(bar, "drift") || inS(bar, "hookC"))
    for (let s = 0; s < 4; s++)
      shot("ride", t + (s + 0.5) * BEAT + jit(8), { gain: 0.055 * vel(0.4), pan: s & 1 ? 0.38 : -0.38, side: 0.7, dur: 0.22 });
  if (hatOn(bar) && !dense)
    for (let s = 0; s < 4; s++)
      shot("tambo", t + (s + 0.5) * BEAT + jit(9), { gain: 0.055 * vel(0.4), pan: s & 1 ? 0.42 : -0.42, side: 0.7, dur: 0.09 });

  // ---- bass: sine bumps, offbeat, with a long sub under the chord -----
  if (kickOn(bar) || inS(bar, "hollow")) {
    const g = inS(bar, "pulse") ? 0.52 : inS(bar, "hollow") ? 0.44 : inS(bar, "descent") ? 0.72 : 0.86;
    for (let b = 0; b < 4; b++) {
      const fifth = four === 3 && b === 3;             // one lift per phrase
      bass(t + (b + 0.5) * BEAT + jit(3), root + (fifth ? 7 : 0), 0.26, g,
        fifth ? root : null);
    }
    if (bar % 2 === 0) bass(t, root - 12, BAR * 0.90, g * 0.42);   // the sub floor
  }

  // ---- pad + stabs -----------------------------------------------------
  if (bar % 2 === 0 && !inS(bar, "hollow"))
    sines(t, chord.map((m) => m - 12), BAR * 1.85, inS(bar, "air") ? 0.055 + 0.012 * bar : 0.085,
      bar % 4 ? 0.22 : -0.22, 0.75, 0.5, 0, 0.30);
  if (inS(bar, "hookA") || inS(bar, "hookB") || inS(bar, "hookC") || inS(bar, "drift"))
    for (const b of [0.5, 2.5])
      sines(t + b * BEAT + jit(4), chord, 0.20, 0.075, b > 1 ? 0.36 : -0.36, 0.7, 0.9, 0.34);
  if (inS(bar, "drift") && four === 1)               // dub stab into the delay
    sines(t + 3.5 * BEAT, chord.map((m) => m + 12), 0.13, 0.070, rnd() > 0.5 ? 0.5 : -0.5, 0.85, 0.7, 0.70);

  // ---- the sung material ----------------------------------------------
  if (inS(bar, "air") && bar % 2 === 0) choir(bar, 0.55 + 0.10 * (bar / 8));
  if (inS(bar, "pulse") && bar % 4 === 0) choir(bar, 0.50);
  if (inS(bar, "morse") && bar % 4 === 0) sos(bar, bar === S.morse[0] ? 0.80 : 1.0);
  if (hookSection(bar) && (bar - (inS(bar, "hookA") ? S.hookA[0] : inS(bar, "hookB") ? S.hookB[0]
    : inS(bar, "hookC") ? S.hookC[0] : S.descent[0])) % 4 === 0)
    hook(bar, { full: (bar / 4) % 2 === 0 });

  // the SOS dots answer the hook in hookB, in the hole the hook leaves
  if (inS(bar, "hookB") && four === 3)
    for (const [k, n, p] of [[0, "dot-d4", 0.34], [1, "dot-a3", -0.34]])
      sung(n, t + (2 + k) * BEAT + jit(4), { gain: 0.34, pan: p, side: 0.8, dly: 0.5 });

  // hollow: no kick, and "the three of us are in a" finally sung as a
  // melody — rising, then answered falling.
  // v4: @jeffrey — "i don't think we need the 'the three of us are in a'
  // part, but 'cult' is fun". The sung sentence is gone; hollow now states
  // the one word it was walking toward, and chops the rest into material.
  if (bar === S.hollow[0] || bar === S.hollow[0] + 4) {
    const picks = choirFor(degAt(bar));
    picks.forEach((nm, i) => sung(`cult-${nm}`, at(bar) + i * 0.045, {
      gain: [0.56, 0.46, 0.36][i] ?? 0.30, pan: [0, -0.46, 0.46][i] ?? 0,
      side: [0.40, 0.85, 0.85][i] ?? 0.7, dark: 0.30, dly: 0.26,
    }));
  }
  if (inS(bar, "hollow") && bar % 2 === 1) material(bar, "cult-b3", 0.80);
  if (inS(bar, "hollow") && bar % 2 === 0) choir(bar, 0.34);
  // v4: @jeffrey — "at 0:39 it should say cult, not dash, and we should all
  // harmonize". This lands mid-hollow, just before "the three of us are in
  // a" answers falling, so the word the phrase is about arrives first — sung
  // as a chord-picked triad of cults instead of one bass dash.
  if (bar === S.hollow[0] + 2 || bar === S.hollow[0] + 6) {
    const picks = choirFor(degAt(bar));
    const gains = [0.50, 0.42, 0.34], pans = [0.0, -0.46, 0.46], sides = [0.40, 0.85, 0.85];
    picks.forEach((nm, i) => sung(`cult-${nm}`, at(bar, 2) + i * 0.045, {
      gain: gains[i] ?? 0.30, pan: pans[i] ?? 0, side: sides[i] ?? 0.7, dark: 0.30, dly: 0.26,
    }));
  }

  // drift: no hook at all. Long sung dashes as pads over the travelling
  // chords, plus the sung bass voice doubling the root.
  if (inS(bar, "drift") && bar % 2 === 0) {
    const nm = ["fs4", "d4"][(bar / 2) % 2];
    sung(`dash-camille-${nm}-hold`, at(bar, 1), { gain: 0.40, pan: -0.45, side: 0.85, dly: 0.30 });
    sung(`dash-alex-${nm}-hold`, at(bar, 1.1), { gain: 0.37, pan: 0.45, side: 0.85, dly: 0.30 });
    const low = { 0: "b2", 2: "a2", 3: "e2", 4: "b2", 5: "g2", 6: "a2" }[deg] ?? "b2";
    sung(`bassdash-${low}`, at(bar, 0), { gain: 0.26, pan: 0, side: 0.25, dark: 0.6 });
  }
  if (inS(bar, "drift") && bar === S.drift[0] + 8) sos(bar, 0.70);

  // Material threaded through the record: the chopped voice answers the hook
  // in hookB, gets denser under drift, and carries hookC's back half. Always
  // a different source word, so the texture keeps changing colour.
  if (inS(bar, "hookB") && bar % 4 === 2) material(bar, "dash-camille-fs4-hold", 0.62, { grain: 0.055 });
  if (inS(bar, "drift") && bar % 2 === 1) material(bar, "cult-d4", 0.72, { grain: 0.085, dly: 0.30 });
  if (inS(bar, "hookC") && bar % 4 === 3) material(bar, "cult-fs4", 0.66, { grain: 0.060, side: 0.9 });
  if (inS(bar, "descent") && bar % 2 === 0) material(bar, "cult-b3", 0.50, { grain: 0.10, dly: 0.34 });

  // ---- the original whistlegraph, unprocessed, as bookends ------------
  if (bar === 2) shot("chant-full", at(2, 1), { gain: 0.44, pan: 0, bus: "vox", side: 0.9, dark: 0.55, dly: 0.30 });
  if (bar === 6) shot("tagline", at(6, 1), { gain: 0.46, pan: 0, bus: "vox", side: 0.7, dark: 0.40, dly: 0.28 });
  if (bar === S.ebb[0]) shot("hook-spoken", at(bar, 1), { gain: 0.56, pan: -0.10, bus: "vox", side: 0.6, dly: 0.22 });
  if (bar === S.ebb[0] + 4) shot("chant-full", at(bar, 1), { gain: 0.48, pan: 0, bus: "vox", side: 0.9, dark: 0.3, dly: 0.34 });
  if (bar === S.out[0] + 2) shot("tagline", at(bar, 1), { gain: 0.50, pan: 0, bus: "vox", side: 0.8, dark: 0.35, dly: 0.40 });

  // ebb / out: the drone comes back and the track leaves the way it came
  if ((inS(bar, "ebb") || inS(bar, "out")) && bar % 2 === 0)
    choir(bar, inS(bar, "out") ? 0.48 * (1 - (bar - S.out[0]) / 11) : 0.46);
  if (inS(bar, "ebb") && bar === S.ebb[0] + 2)
    stretched("three-of-us", at(bar, 1), { gain: 0.22, pan: 0.35, semis: 0, stretch: 1.8, dur: BAR * 1.6, side: 0.9, dly: 0.3 });

  // one very soft noise wash where sections change — the only "transition
  // effect" in the track, and it never becomes a riser.
  if ([S.morse[0], S.hookA[0], S.hookB[0], S.hookC[0], S.ebb[0]].includes(bar))
    shot("sweep", t - 0.9, { gain: 0.055, pan: 0, bus: "music", side: 0.9, dur: 2.2, dark: 0.35 });
}
if (missing.size) console.warn(`  ! missing samples: ${[...missing].join(", ")}`);

// ── dub delay ─────────────────────────────────────────────────────────
// Dotted-eighth ping-pong with a one-pole damp in the loop and an 180 Hz
// highpass on the return, so the tails never muddy the sub. This is the
// chill techno "reverb" — it is what makes the space feel like a room
// with a dub engineer in it rather than a plate.
{
  const D = Math.round(0.75 * BEAT * SR);       // dotted eighth = 0.375 s
  const FB = 0.42;
  const damp = 1 - Math.exp((-TAU * 2600) / SR);
  const hpRc = 1 / (TAU * 180), hpA = hpRc / (hpRc + 1 / SR);
  const bL = new Float64Array(N + D + 1), bR = new Float64Array(N + D + 1);
  let dL = 0, dR = 0, hpL = 0, hpR = 0, pL = 0, pR = 0;
  for (let i = 0; i < N; i++) {
    const tapL = i >= D ? bL[i - D] : 0;
    const tapR = i >= D ? bR[i - D] : 0;
    dL += damp * (tapR - dL);                  // ping: right tail feeds left
    dR += damp * (tapL - dR);
    bL[i] = dlySend[i] + dR * FB;
    bR[i] = dL * FB;
    hpL = hpA * (hpL + bL[i] - pL); pL = bL[i];
    hpR = hpA * (hpR + bR[i] - pR); pR = bR[i];
    musicL[i] += hpL * 0.50;
    musicR[i] += hpR * 0.50;
  }
}

// ── Special Sign side return ──────────────────────────────────────────
// Band-limited 80 Hz – 11.5 kHz, handed back antisymmetrically (L=+, R=−)
// on a slewed send that breathes with the arrangement. Mono-safe by
// construction: the direct signal never left the equal-power pans.
{
  const hpRc = 1 / (TAU * 80), hpA = hpRc / (hpRc + 1 / SR);
  const lpK = 1 - Math.exp((-TAU * 11500) / SR);
  let hp = 0, lp = 0, prev = 0, send = 0.9;
  for (let i = 0; i < N; i++) {
    const s = sideB[i];
    hp = hpA * (hp + s - prev); prev = s;
    lp += lpK * (hp - lp);
    const bar = (i / SR) / BAR;
    const target =
      bar < S.pulse[0] ? 0.92 :
      bar < S.morse[0] ? 0.72 :
      bar < S.hookA[0] ? 0.60 :
      bar < S.hollow[0] ? 0.46 :
      bar < S.hookB[0] ? 0.88 :
      bar < S.drift[0] ? 0.48 :
      bar < S.hookC[0] ? 0.78 :
      bar < S.descent[0] ? 0.42 :
      bar < S.ebb[0] ? 0.60 : 0.90;
    send += 0.00004 * (target - send);
    musicL[i] += lp * send;
    musicR[i] -= lp * send;
  }
}

// ── stems ─────────────────────────────────────────────────────────────
// `--stems` writes the three buses out separately, pre-duck. v1's hard
// lesson was that the hook can be perfectly scored and still be inaudible
// under the bed (whisper could read it in the bridge and nowhere else),
// and you cannot hear that in the sum. Stems make it measurable.
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
  mkdirSync(resolve(OUT, "stems"), { recursive: true });
  writeStereo(resolve(OUT, "stems", "v2-vox.wav"), voxL, voxR);
  writeStereo(resolve(OUT, "stems", "v2-music.wav"), musicL, musicR);
  writeStereo(resolve(OUT, "stems", "v2-drums.wav"), drumsL, drumsR);
  console.log(`  stems → ${resolve(OUT, "stems")}`);
}

// ── sum ───────────────────────────────────────────────────────────────
// Clean: duck, fade, measure, trim linearly. No master tanh anywhere —
// loudness and the ceiling belong to render-v2.sh.
let peak = 0;
for (let i = 0; i < N; i++) {
  const t = i / SR;
  const d = duckAt(t);
  const fadeIn = Math.min(1, i / (0.014 * SR));
  const fadeOut = Math.min(1, (N - 1 - i) / (2.6 * SR));
  const fade = Math.max(0, Math.min(fadeIn, fadeOut));
  const dv = Math.pow(d, 0.25);                       // voices only breathe
  // The sung words are the song, so the vox bus rides +3 dB over the bed.
  // Measured with `--stems`: without it the lead sat ~6 dB UNDER music +
  // drums through the hooks, which is where v1's "the hook is inaudible"
  // failure came from. +3 puts it ~3 dB proud and it reads immediately.
  const l = (musicL[i] * d + voxL[i] * dv * VOXG + drumsL[i]) * fade;
  const r = (musicR[i] * d + voxR[i] * dv * VOXG + drumsR[i]) * fade;
  musicL[i] = l; musicR[i] = r;
  if (Math.abs(l) > peak) peak = Math.abs(l);
  if (Math.abs(r) > peak) peak = Math.abs(r);
}
const norm = peak > 1e-9 ? 0.92 / peak : 1;
for (let i = 0; i < N; i++) { musicL[i] *= norm; musicR[i] *= norm; }
console.error(`# pre-master peak ${peak.toFixed(6)} · linear trim ${norm.toFixed(3)} · ${kicks.length} kicks`);

const outWav = resolve(OUT, "cult-remix-v4-full.wav");
{
  const frames = N, bytes = frames * 2 * 4;
  const buf = Buffer.alloc(44 + bytes);
  buf.write("RIFF", 0, "ascii"); buf.writeUInt32LE(36 + bytes, 4); buf.write("WAVE", 8, "ascii");
  buf.write("fmt ", 12, "ascii"); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(2, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 8, 28);
  buf.writeUInt16LE(8, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36, "ascii"); buf.writeUInt32LE(bytes, 40);
  for (let i = 0; i < frames; i++) {
    buf.writeFloatLE(musicL[i], 44 + i * 8);
    buf.writeFloatLE(musicR[i], 44 + i * 8 + 4);
  }
  writeFileSync(outWav, buf);
}

writeFileSync(resolve(OUT, "cult-remix-v4.events.json"), JSON.stringify({
  events: EVENTS.slice().sort((a, b) => a.t - b.t),
  schema: "aesthetic.computer/pop-events/v1",
  track: "whistlegraph cult --- remix (v2, chill)",
  renderer: "pop/cult/bin/render2.mjs",
  source: {
    work: "cult — The Three of Us Are in a Cult (Whistlegraph, 2022)",
    tiktok: "https://www.tiktok.com/@whistlegraph/video/7055106286232325423",
    mirror: "https://assets.aesthetic.computer/whistlegraph/index/posts/7055106286232325423.mp4",
    performers: ["Jeffrey Alan Scudder", "Camille Klein", "Alex Freundlich"],
    transcript: "Dash, dash, dash, dot, dot, dot. The three of us are in a cult.",
    caption: "dot dot dot dash dash dash dot dot dot (jk)",
    measuredRegisters: { jeffrey: "B2 ≈127 Hz", alex: "F#3 ≈193 Hz", camille: "C#4 ≈245 Hz", cult: "B3 ≈247 Hz" },
  },
  speechToSinging: {
    engine: "WORLD (pyworld 0.3.5) via pop/cult/bin/sing.py — Saitou 2007 recipe",
    steps: ["harvest/stonemask f0", "cheaptrick envelope", "d4c aperiodicity",
      "duration control (vowel-weighted frame warp)", "f0 substitution from score",
      "overshoot / preparation / vibrato / fine fluctuation", "singer's formant +3.2 dB @ 2.8 kHz",
      "voiced/unvoiced compositing with the warped source", "de-ess"],
    verification: "librosa.pyin per note (SPEECH-TO-SINGING-V2 §1)",
    driver: "pop/cult/bin/sing.mjs → pop/cult/sung/*.wav (33 renders)",
  },
  harmony: {
    key: "B natural minor",
    source: "PROGRESSIONS_CHILL, recap/bin/trance.mjs — the 8-row pool described in pop/dance/cutezenwaltzi.md",
    rows: [[0, 5, 2, 6], [0, 6, 3, 5], [0, 4, 5, 3], [0, 2, 5, 3]],
    reading: ["i VI III VII (Bm G D A)", "i VII iv VI (Bm A Em G)", "i v VI iv (Bm F#m G Em)", "i III VI iv (Bm D G Em)"],
    chordBars: 2, cycleBars: 32,
  },
  tempoBPM: BPM, bars: BARS, seconds: +(BARS * BAR).toFixed(2),
  sections: Object.fromEntries(Object.entries(S).map(([k, [a, b]]) => [k, { bars: [a, b], seconds: [+at(a).toFixed(2), +at(b).toFixed(2)] }])),
  hook: "dash — i wanna — dash — i wanna — run it fast — dot dot (four bars, sung)",
  sosFigure: "··· ——— ··· as a four-bar riff: three short sung notes, three long, three short",
  direction: "cool chill techno at 120 in B minor: no drops, no risers, no crashes; a soft 4/4 that mostly just stays, offbeat sine-bump bass, dotted-eighth ping-pong dub delay, and sung — not chopped — vocals as the whole lead",
  prePeak: +peak.toFixed(6), linearTrim: +norm.toFixed(4), kicks: kicks.length,
}, null, 2));

console.log(`✓ ${outWav}  (${(BARS * BAR).toFixed(1)}s)`);
