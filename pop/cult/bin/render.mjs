#!/usr/bin/env node
// render.mjs — "whistlegraph cult --- remix" (dash dash dash remix)
//
// A club remix of the cult whistlegraph — "The Three of Us Are in a Cult"
// (@whistlegraph, 2022, 39.6M views), whose caption was already the whole
// idea: "dot dot dot dash dash dash dot dot dot (jk)". SOS in morse, joke
// retracted. The clip holds six chant hits, one dash and one dot from each
// of Jeffrey, Camille and Alex, sitting on B2 / F#3 / C#4 — a B minor
// power chord with the ninth on top. So the remix is in B minor at 128 BPM
// and the source needs no tuning; it was always a club record.
//
// The hook @jeffrey asked for:
//
//     dash — i wanna — dash — i wanna — run it fast — dot dot
//
// "dash" and "dot" are the real three voices off the TikTok. "i wanna" and
// "run it fast" are ElevenLabs jeffrey-pvc, because the TikTok never says
// them (see bin/slice.mjs).
//
// Mixing follows the taksmukkeklokken-smooch revision notes:
//   · every voice exits through a 10 ms raised-cosine tail (no clicks)
//   · the sidechain ramps to its floor over ~7 ms (no stepped ducks)
//   · no master tanh — clean sum, linear peak trim, loudnorm sets loudness
//   · bass is sine bumps: fundamental + sub octave + a whisper of 2nd
//   · Special Sign space — dry equal-power pans plus a band-limited
//     antisymmetric side return whose send breathes with the arrangement
//   · the choir is one word ("cult", sung on B3) granular-stretched and
//     stacked at root/3rd/5th/octave with small offsets
//
//   node pop/cult/bin/render.mjs            # → out/cult-remix.wav (f32)
//   ./pop/cult/bin/render.sh                # + master to mp3/24-bit

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
const BPM = 128;
const BEAT = 60 / BPM;              // .46875 s
const BAR = 4 * BEAT;               // 1.875 s
const BARS = 136;                   // 255 s ≈ 4:15
const TAU = Math.PI * 2;
const N = Math.round((BARS * BAR + 3.0) * SR);

// Three buses. Drums are never ducked; the harmonic bed takes the full
// sidechain; the voices take only a quarter of it. The hook lands on the
// beat by design — "dash" is a downbeat word — so a full-depth duck would
// swallow the very syllable the track is built around. (It did: whisper
// could read the hook in the bridge and hear nothing but "upbeat music"
// through the drops until the voices got their own bus.)
const musicL = new Float64Array(N), musicR = new Float64Array(N);
const drumsL = new Float64Array(N), drumsR = new Float64Array(N);
const voxL   = new Float64Array(N), voxR   = new Float64Array(N);
const sideB  = new Float64Array(N);

const clamp = (v, a, b) => (v < a ? a : v > b ? b : v);
const hz = (midi) => 440 * Math.pow(2, (midi - 69) / 12);
const at = (bar, beat = 0) => bar * BAR + beat * BEAT;

// The click amnesty: every voice ends through this 10 ms raised-cosine ramp.
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
    mkdirSync(dirname(p), { recursive: true });
    execFileSync("ffmpeg", ["-y", "-v", "error", "-i", path, "-ac", "1", "-ar", String(SR), p]);
  }
  const { samples, sampleRate } = readWavMono(p);
  if (sampleRate !== SR) {
    const q = join(OUT, `.cache-${name}-rs.wav`);
    execFileSync("ffmpeg", ["-y", "-v", "error", "-i", p, "-ac", "1", "-ar", String(SR), q]);
    BANK[name] = readWavMono(q).samples;
  } else BANK[name] = samples;
  // Trim any leading dead air the source left behind, then normalize.
  const s = BANK[name];
  let a = 0; while (a < s.length - 1 && Math.abs(s[a]) < 0.008) a++;
  const t = s.subarray(Math.max(0, a - Math.round(0.002 * SR)));
  let peak = 0; for (const v of t) peak = Math.max(peak, Math.abs(v));
  const g = peak > 1e-6 ? 0.95 / peak : 1;
  const out = new Float32Array(t.length);
  for (let i = 0; i < t.length; i++) out[i] = t[i] * g;
  BANK[name] = out;
}

const SAMPLES = resolve(LANE, "samples");
for (const f of readdirSync(SAMPLES)) if (f.endsWith(".wav")) load(f.replace(/\.wav$/, ""), join(SAMPLES, f));

const DEMOS = resolve(REPO, "pop/demos/samples");
for (const [n, f] of Object.entries({
  hatC: "perc-hat-c.mp3", hatO: "perc-hat-o.mp3", clap: "perc-clap.mp3",
  ride: "perc-ride.mp3", snap: "perc-snap.mp3", crash: "perc-crash.mp3",
  snare: "perc-snare.mp3", tambo: "perc-tambo.mp3", block: "perc-block.mp3",
  sweep: "bed-noise-sweep.mp3",
})) load(n, join(DEMOS, f));

// The three performers again, from the What's Inside Your Heart session —
// same people, different song. Used strictly as texture and stabs, never
// as a naked lead: @jeffrey's note is that these takes read as funny when
// they front a track they weren't sung for.
const WIYH = resolve(REPO, "pop/samples/whats-inside-your-heart/sfx");
for (const [n, f] of Object.entries({
  wJeff: "Jeffrey whistle.wav", wAlex: "Alex whistlegraph.wav",
  sCam: "Camille hey!.wav", sPop: "Jeffrey pop.wav", sTch: "Jeffrey tttchew.wav",
})) load(n, join(WIYH, f));

// ── space ─────────────────────────────────────────────────────────────
// Special Sign topology. The direct voice is dry on a stable equal-power
// pan so the mix stays mono-safe; a cheap interaural model (ITD + head
// shadow) feeds a side bus that is band-limited and returned
// antisymmetrically in the master. Space as a return, not a replacement.
function spatial(az) {
  const itd = Math.round(0.00027 * SR * Math.sin(az));   // ≤ ~13 samples
  const shadow = 0.35 * Math.sin(az);
  return { itd, gl: 1 - shadow, gr: 1 + shadow };
}

function emit(bus, i, mono, pan, sp, sideAmt) {
  if (i < 0 || i >= N) return;
  const a = (Math.PI / 4) * (1 + pan);
  const L = bus === "drums" ? drumsL : bus === "vox" ? voxL : musicL;
  const R = bus === "drums" ? drumsR : bus === "vox" ? voxR : musicR;
  L[i] += mono * Math.cos(a);
  R[i] += mono * Math.sin(a);
  if (sp && sideAmt) {
    const li = i + sp.itd, ri = i - sp.itd;
    const l = li >= 0 && li < N ? mono * sp.gl : 0;
    const r = ri >= 0 && ri < N ? mono * sp.gr : 0;
    sideB[i] += 0.5 * (l - r) * sideAmt;
  }
}

// ── sidechain ─────────────────────────────────────────────────────────
// Kick times get a duck that *ramps* down over 7 ms and recovers over
// 260 ms. Instant steps are what make a pump click.
const kicks = [];
function duckAt(t) {
  let d = 1;
  for (const k of kicks) {
    const dt = t - k;
    if (dt < -0.008 || dt > 0.34) continue;
    let g;
    if (dt < 0.007) g = 1 - 0.66 * clamp((dt + 0.008) / 0.015, 0, 1);
    else {
      const u = clamp((dt - 0.007) / 0.26, 0, 1);
      g = 0.34 + 0.66 * (u * u * (3 - 2 * u));
    }
    d = Math.min(d, g);
  }
  return d;
}

// ── voices ────────────────────────────────────────────────────────────
function kick(t, gain = 1) {
  kicks.push(t);
  const n = Math.round(0.42 * SR), i0 = Math.round(t * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const f = 48 + 118 * Math.exp(-u * 44);              // pitch drop
    ph += (TAU * f) / SR;
    const env = Math.exp(-u * 9.5) * Math.min(1, u / 0.0012);
    const click = Math.exp(-u * 420) * 0.16 * Math.sin(TAU * 1400 * u);
    emit("drums", i0 + i, (Math.sin(ph) * env + click) * 0.86 * gain * tailFade(i, n), 0, null, 0);
  }
}

// Sine bumps, per @jeffrey: fundamental + sub octave + a whisper of the
// 2nd harmonic. No plucked string, nothing twangy, nothing to clip.
function bass(t, midi, dur, gain = 1, slideFrom = null) {
  const n = Math.round((dur + 0.10) * SR), i0 = Math.round(t * SR);
  const f1 = hz(midi), f0 = slideFrom !== null ? hz(slideFrom) : f1;
  let p1 = 0, p2 = 0, p3 = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const glide = clamp(u / 0.075, 0, 1);
    const f = f0 + (f1 - f0) * (glide * glide * (3 - 2 * glide));
    p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
    let env = Math.min(1, u / 0.010);
    const rel = dur;
    if (u > rel) env *= Math.max(0, 1 - (u - rel) / 0.10);
    const s = Math.sin(p1) + 0.52 * Math.sin(p2) + 0.11 * Math.sin(p3);
    lp += 0.55 * (s - lp);                                // keep it round
    emit("music", i0 + i, lp * 0.40 * env * gain * tailFade(i, n), 0, null, 0);
  }
}

// Harmonized sines, the "smooth not buzzy" pad/stab. Detuned pairs, gentle
// low-pass, no presence boost anywhere.
function sines(t, midis, dur, gain, pan, sideAmt = 0.5, bright = 1) {
  const n = Math.round((dur + 0.35) * SR), i0 = Math.round(t * SR);
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
      s += Math.sin(ph[v][0]) + 0.7 * Math.sin(ph[v][1]) + 0.09 * bright * Math.sin(ph[v][2]);
    }
    s /= midis.length * 1.8;
    lp += 0.30 * (s - lp);
    let env = Math.min(1, u / 0.020);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.35);
    emit("music", i0 + i, lp * env * gain * tailFade(i, n), pan, sp, sideAmt);
  }
}

// One-shot player: resample by semitones, optional band shaping.
function shot(name, t, { gain = 1, pan = 0, semis = 0, bus = "drums", side = 0.35, dark = 0, dur = null } = {}) {
  const s = BANK[name];
  if (!s) return;
  const step = Math.pow(2, semis / 12);
  const avail = Math.floor((s.length - 2) / step);
  const n = dur ? Math.min(avail, Math.round(dur * SR)) : avail;
  const i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let lp = 0, pos = 0;
  for (let i = 0; i < n; i++, pos += step) {
    const q = pos | 0;
    if (q + 1 >= s.length) break;
    const f = pos - q;
    let v = s[q] + (s[q + 1] - s[q]) * f;
    if (dark > 0) { lp += (1 - dark) * (v - lp); v = lp; }
    const env = Math.min(1, i / (0.0015 * SR));
    emit(bus, i0 + i, v * env * gain * tailFade(i, n), pan, sp, side);
  }
}

// Granular time-stretch — 55 ms Hann grains at 50% overlap, read at the
// pitch rate but advanced more slowly, so a word can be held out without
// dropping in pitch. This is what turns one "cult" into a choir.
function stretched(name, t, { gain = 1, pan = 0, semis = 0, stretch = 1, dur = 1, side = 0.7, dark = 0.45, bus = "vox" } = {}) {
  const s = BANK[name];
  if (!s) return;
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
      const v = s[q] + (s[q + 1] - s[q]) * f;
      acc[o] += v * (0.5 - 0.5 * Math.cos((TAU * k) / (grain - 1)));
    }
    read += hopIn;
  }
  const i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.3);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    lp += (1 - dark) * (acc[i] - lp);
    let env = Math.min(1, u / 0.030);
    const left = (n - i) / SR;
    if (left < 0.12) env *= left / 0.12;
    emit(bus, i0 + i, lp * env * gain * tailFade(i, n), pan, sp, side);
  }
}

// ── harmony ───────────────────────────────────────────────────────────
// B minor: i – VI – III – VII, two bars each.
const ROOTS = [35, 31, 38, 33];                       // B1 G1 D2 A1
const CHORDS = [[59, 62, 66], [55, 59, 62], [50, 54, 57], [57, 61, 64]];
const chordAt = (bar) => ((bar >> 1) % 4);

// ── arrangement ───────────────────────────────────────────────────────
// bar → what is running. Everything below reads off this.
const S = {
  intro:   [0, 8],
  beatIn:  [8, 16],
  grooveA: [16, 24],
  buildA:  [24, 32],
  drop1:   [32, 48],
  breakA:  [48, 56],
  grooveB: [56, 64],
  down:    [64, 72],     // no drums — the angelic choir
  buildB:  [72, 80],
  drop2:   [80, 104],
  bridge:  [104, 112],
  drop3:   [112, 128],
  outro:   [128, 136],
};
const inS = (bar, name) => bar >= S[name][0] && bar < S[name][1];
const dropBar = (b) => inS(b, "drop1") || inS(b, "drop2") || inS(b, "drop3");
const beatBar = (b) => dropBar(b) || inS(b, "beatIn") || inS(b, "grooveA") ||
  inS(b, "buildA") || inS(b, "breakA") || inS(b, "grooveB") || inS(b, "buildB") || inS(b, "bridge");

// Human pocket: a few ms of scatter, never quantized dead.
let seed = 20220120;                                   // the cult post date
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);
const jit = (ms = 7) => ((rnd() - 0.5) * 2 * ms) / 1000;
const vel = (spread = 0.22) => 1 - rnd() * spread;

// ── THE HOOK ──────────────────────────────────────────────────────────
// dash — i wanna — dash — i wanna — run it fast — dot dot
// Two bars. The dashes are the three whistlegraph voices stacked; the
// "i wanna"s and "run it fast" are jeffrey-pvc; the dots close it out
// with Camille and Alex answering each other across the stereo field.
function hook(bar, { big = true } = {}) {
  const t = at(bar);
  const G = big ? 1.0 : 0.80;
  const V = { bus: "vox" };

  // dash ×2 — all three at once, panned wide, Jeffrey centred and low.
  for (const [b, sc] of [[0, 1], [2, 1]]) {
    const u = t + b * BEAT + jit(5);
    shot("dash-jeffrey", u, { ...V, gain: 1.30 * G * sc, pan: 0.0, side: 0.5, semis: 0 });
    shot("dash-camille", u + 0.012, { ...V, gain: 0.70 * G * sc, pan: -0.62, side: 0.85, semis: -12 });
    shot("dash-alex",    u + 0.024, { ...V, gain: 0.72 * G * sc, pan: 0.62, side: 0.85, semis: 0 });
    // A sine bump under each dash so the word lands with weight.
    sines(u, [47, 54], 0.30, 0.16 * G, 0, 0.3, 0.4);
  }

  // i wanna ×2 — jeffrey-pvc, pitched up a touch so it sings over the B.
  shot("i-wanna-a", t + 1 * BEAT + jit(4), { ...V, gain: 1.24 * G, pan: -0.20, side: 0.55, semis: 2 });
  shot("i-wanna-b", t + 3 * BEAT + jit(4), { ...V, gain: 1.24 * G, pan: 0.20, side: 0.55, semis: 2 });

  // run it fast — the line that gives the drop its name.
  shot("run-it-fast", t + 4 * BEAT + jit(4), { ...V, gain: 1.45 * G, pan: 0.0, side: 0.45, semis: 1 });
  if (big) shot("run-it-fast", t + 4 * BEAT + 0.030, { ...V, gain: 0.42, pan: 0.55, side: 0.9, semis: 13, dark: 0.25 });

  // dot dot — Camille left, Alex right, the morse tail.
  shot("dot-camille", t + 6 * BEAT + jit(3), { ...V, gain: 1.24 * G, pan: -0.55, side: 0.8 });
  shot("dot-alex",    t + 6.5 * BEAT + jit(3), { ...V, gain: 1.24 * G, pan: 0.55, side: 0.8 });
  if (big) shot("dot-jeffrey", t + 7 * BEAT, { ...V, gain: 0.52, pan: 0, side: 0.4, semis: -5 });
}

// ── score ─────────────────────────────────────────────────────────────
console.log(`→ scoring ${BARS} bars @ ${BPM} BPM · B minor · ${(BARS * BAR).toFixed(1)}s`);

for (let bar = 0; bar < BARS; bar++) {
  const t = at(bar);
  const c = chordAt(bar);
  const root = ROOTS[c], chord = CHORDS[c];
  const four = bar % 4, eight = bar % 8;

  // ---- kick: four-on-the-floor wherever there's a beat -----------------
  if (beatBar(bar)) {
    const drop = dropBar(bar);
    for (let b = 0; b < 4; b++) {
      // the classic one-bar gap before each drop
      if (inS(bar, "buildA") && bar === 31) continue;
      if (inS(bar, "buildB") && bar === 79) continue;
      if (inS(bar, "bridge") && bar === 111) continue;
      kick(t + b * BEAT + jit(3), drop ? 1.0 : inS(bar, "beatIn") ? 0.74 : 0.90);
    }
    if (drop && four === 3) kick(t + 3.5 * BEAT, 0.62);   // drop-bar pickup
  }

  // ---- hats / shaker ---------------------------------------------------
  if (beatBar(bar) && !inS(bar, "beatIn")) {
    for (let s = 0; s < 8; s++) {
      const swing = s & 1 ? 0.055 : 0;                    // house shuffle
      const u = t + (s * 0.5 + swing) * BEAT + jit(5);
      if (s & 1) shot("hatC", u, { gain: 0.30 * vel(), pan: 0.22, side: 0.5, dur: 0.09 });
      else if (s % 4 === 2) shot("hatC", u, { gain: 0.17 * vel(), pan: -0.18, side: 0.5, dur: 0.07 });
    }
    if (dropBar(bar) || inS(bar, "grooveB"))
      shot("hatO", t + 2.5 * BEAT + 0.03, { gain: 0.30, pan: -0.30, side: 0.65, dur: 0.30 });
  }
  if (dropBar(bar) || inS(bar, "grooveA") || inS(bar, "grooveB"))
    for (let s = 0; s < 4; s++)
      shot("tambo", t + (s + 0.5) * BEAT + jit(9), { gain: 0.10 * vel(0.4), pan: (s & 1 ? 0.4 : -0.4), side: 0.7, dur: 0.10 });

  // ---- clap / snap on 2 and 4 -----------------------------------------
  if (beatBar(bar) && !inS(bar, "beatIn")) {
    for (const b of [1, 3]) {
      shot("clap", t + b * BEAT + jit(6), { gain: dropBar(bar) ? 0.46 : 0.34, pan: -0.10, side: 0.75 });
      shot("snap", t + b * BEAT + 0.014, { gain: 0.16, pan: 0.28, side: 0.6 });
    }
  }

  // ---- bass ------------------------------------------------------------
  if (beatBar(bar) && !inS(bar, "bridge")) {
    const g = dropBar(bar) ? 1.0 : inS(bar, "beatIn") ? 0.55 : 0.80;
    // offbeat house bass with a slide into the downbeat of each new chord
    bass(t + 0.5 * BEAT, root, 0.30, g);
    bass(t + 1.5 * BEAT, root, 0.30, g);
    bass(t + 2.5 * BEAT, root + (dropBar(bar) ? 0 : 0), 0.30, g);
    bass(t + 3.5 * BEAT, root + (four === 3 ? 3 : 0), 0.30, g, four === 3 ? root : null);
    if (dropBar(bar)) bass(t, root - 12, 0.42, g * 0.55);   // sub anchor
  }

  // ---- pads / stabs ----------------------------------------------------
  if (inS(bar, "intro"))
    sines(t, chord.map((m) => m - 12), BAR * 0.92, 0.10 + 0.02 * bar, (bar % 2 ? 0.3 : -0.3), 0.85, 0.5);
  if (inS(bar, "grooveA") || inS(bar, "grooveB") || inS(bar, "breakA"))
    for (const b of [0.5, 2.5])
      sines(t + b * BEAT, chord, 0.34, 0.15, b > 1 ? 0.34 : -0.34, 0.7, 0.9);
  if (dropBar(bar))
    for (const b of [0.5, 1.5, 2.5, 3.5])
      sines(t + b * BEAT + jit(4), chord, 0.24, 0.13, (b % 2 < 1 ? -0.38 : 0.38), 0.8, 1.0);
  if (inS(bar, "down"))
    sines(t, chord.map((m) => m - 12), BAR * 1.6, 0.16, (bar % 2 ? 0.25 : -0.25), 1.0, 0.35);

  // ---- the hook, every 4 bars inside a drop ----------------------------
  if (dropBar(bar) && four === 0) hook(bar, { big: !inS(bar, "drop1") || bar >= 36 });

  // ---- the chant, the tagline, the choir --------------------------------
  if (bar === 0) shot("chant-full", at(0, 1), { gain: 0.62, pan: 0, bus: "vox", side: 0.9, dark: 0.55 });
  if (bar === 4) shot("tagline", at(4, 0.5), { gain: 0.66, pan: 0, bus: "vox", side: 0.7, dark: 0.35 });
  if (bar === 12) shot("chant-full", at(12), { gain: 0.50, pan: -0.25, bus: "vox", side: 0.9, dark: 0.2 });
  if (bar === 20) shot("tagline", at(20, 0.5), { gain: 0.68, pan: 0.05, bus: "vox", side: 0.6 });
  if (bar === 22) shot("hook-spoken", at(22, 2), { gain: 0.52, pan: -0.15, bus: "vox", side: 0.7 });

  // the break: chopped dashes/dots as a stutter figure
  if (inS(bar, "breakA")) {
    const who = ["dot-jeffrey", "dot-camille", "dot-alex", "dash-camille"];
    for (let s = 0; s < 4; s++)
      shot(who[(bar + s) % 4], t + s * BEAT + jit(6), {
        gain: 0.46, pan: (s % 2 ? 0.5 : -0.5), bus: "vox", side: 0.8,
        semis: [0, -3, 4, -5][(bar + s) % 4],
      });
    if (four === 3) shot("wAlex", t + 2 * BEAT, { gain: 0.22, pan: 0.4, bus: "vox", side: 0.85, dark: 0.3 });
  }

  // the breakdown: one word, "cult" (sung on B3), stretched and stacked at
  // root / minor 3rd / 5th / octave — a choir out of a single take.
  if (inS(bar, "down") && bar % 2 === 0) {
    const stack = [[0, 0.0, 0.34], [3, 0.045, 0.24], [7, 0.090, 0.24], [12, 0.135, 0.16], [-12, 0.02, 0.20]];
    const key = [59, 55, 50, 57][chordAt(bar)] - 59;     // move with the chord
    for (const [iv, off, g] of stack)
      stretched("cult", t + off, {
        gain: g, pan: clamp(iv / 14, -0.7, 0.7), semis: key + iv,
        stretch: 6.2, dur: BAR * 2 - 0.2, side: 0.9, dark: 0.5,
      });
    if (bar === S.down[0]) shot("tagline", at(bar, 1), { gain: 0.55, pan: 0, bus: "vox", side: 0.6, dark: 0.3 });
    if (bar === S.down[0] + 4) stretched("three-of-us", at(bar, 2), { gain: 0.34, pan: 0.3, semis: 0, stretch: 1.6, dur: BAR * 2, side: 0.9 });
  }

  // bridge: the whole spoken hook, then the real tagline answering it
  if (bar === S.bridge[0]) shot("hook-spoken", at(bar, 0.5), { gain: 0.80, pan: 0, bus: "vox", side: 0.55 });
  if (bar === S.bridge[0] + 4) shot("tagline", at(bar, 0.5), { gain: 0.78, pan: 0, bus: "vox", side: 0.6 });
  if (inS(bar, "bridge")) {
    sines(t, chord.map((m) => m - 12), BAR * 0.9, 0.13, bar % 2 ? 0.3 : -0.3, 0.9, 0.4);
    bass(t, root, BAR * 0.85, 0.62);
  }

  // outro: the chant walks out the way it walked in
  if (bar === S.outro[0]) shot("chant-full", at(bar, 0.5), { gain: 0.60, pan: 0, bus: "vox", side: 0.9, dark: 0.25 });
  if (bar === S.outro[0] + 4) shot("tagline", at(bar), { gain: 0.62, pan: 0, bus: "vox", side: 0.8, dark: 0.4 });
  if (inS(bar, "outro")) sines(t, chord.map((m) => m - 12), BAR * 1.1, 0.13 * (1 - (bar - S.outro[0]) / 9), bar % 2 ? 0.3 : -0.3, 1.0, 0.3);

  // ---- builds ----------------------------------------------------------
  for (const [name, endBar] of [["buildA", 32], ["buildB", 80]]) {
    if (!inS(bar, name)) continue;
    const u = (bar - S[name][0]) / 8;                     // 0→1 across the build
    const div = u < 0.4 ? 2 : u < 0.7 ? 4 : u < 0.9 ? 8 : 16;
    for (let s = 0; s < div; s++)
      shot("snare", t + (s / div) * BAR, { gain: (0.10 + 0.24 * u) * vel(0.2), pan: (s / div - 0.5) * 0.7, side: 0.7, dur: 0.14 });
    if (bar === endBar - 4) shot("sweep", t, { gain: 0.30, pan: 0, bus: "music", side: 0.9, dur: BAR * 4 });
    if (bar === endBar - 1) {
      shot("wJeff", t + 2 * BEAT, { gain: 0.26, pan: 0.2, bus: "vox", side: 0.9, semis: 0, dark: 0.15 });
      shot("sTch", t + 3 * BEAT, { gain: 0.30, pan: -0.3, side: 0.8 });
    }
  }
  // crash on every drop entry and section head
  if ([S.drop1[0], S.drop2[0], S.drop3[0], S.grooveB[0], S.breakA[0], S.bridge[0]].includes(bar))
    shot("crash", t, { gain: 0.34, pan: 0.1, side: 0.8, dur: 2.4 });

  // little human punctuation from the three of them
  if (eight === 6 && (inS(bar, "grooveA") || inS(bar, "grooveB")))
    shot("sCam", t + 3 * BEAT, { gain: 0.20, pan: -0.45, bus: "vox", side: 0.85, dark: 0.2 });
  if (dropBar(bar) && four === 2) shot("sPop", t + 3.5 * BEAT, { gain: 0.22, pan: 0.45, side: 0.7 });
}

// ── master ────────────────────────────────────────────────────────────
// Special Sign return: band-limit the antisymmetric ear field (80 Hz –
// 11.5 kHz) and hand it back L=+ / R=−, with a slewed send that breathes
// with the arrangement. Nothing steps.
{
  const hpRc = 1 / (TAU * 80), hpA = hpRc / (hpRc + 1 / SR);
  const lpK = 1 - Math.exp((-TAU * 11500) / SR);
  let hp = 0, lp = 0, prev = 0, send = 0.5;
  for (let i = 0; i < N; i++) {
    const s = sideB[i];
    hp = hpA * (hp + s - prev); prev = s;
    lp += lpK * (hp - lp);
    const bar = (i / SR) / BAR;
    const target =
      bar < S.beatIn[0] ? 0.85 :
      bar < S.drop1[0] ? 0.62 :
      bar < S.breakA[0] ? 0.44 :
      bar < S.down[0] ? 0.66 :
      bar < S.buildB[0] ? 0.95 :
      bar < S.drop2[0] ? 0.70 :
      bar < S.bridge[0] ? 0.42 :
      bar < S.drop3[0] ? 0.80 :
      bar < S.outro[0] ? 0.44 : 0.90;
    send += 0.00005 * (target - send);
    musicL[i] += lp * send;
    musicR[i] -= lp * send;
  }
}

// Clean sum: duck, fade, measure, normalize linearly. No master tanh —
// loudness and the ceiling belong to the mastering chain.
let peak = 0;
for (let i = 0; i < N; i++) {
  const t = i / SR;
  const d = duckAt(t);
  const fadeIn = Math.min(1, i / (0.012 * SR));
  const fadeOut = Math.min(1, (N - 1 - i) / (2.2 * SR));
  const fade = Math.max(0, Math.min(fadeIn, fadeOut));
  // Voices ride a quarter-depth duck (d^0.25): they breathe with the kick
  // instead of disappearing under it.
  const dv = Math.pow(d, 0.25);
  const l = (musicL[i] * d + voxL[i] * dv + drumsL[i]) * fade;
  const r = (musicR[i] * d + voxR[i] * dv + drumsR[i]) * fade;
  musicL[i] = l; musicR[i] = r;
  if (Math.abs(l) > peak) peak = Math.abs(l);
  if (Math.abs(r) > peak) peak = Math.abs(r);
}
const norm = peak > 1e-9 ? 0.92 / peak : 1;
for (let i = 0; i < N; i++) { musicL[i] *= norm; musicR[i] *= norm; }
console.error(`# pre-master peak ${peak.toFixed(6)} · linear trim ${norm.toFixed(3)} · ${kicks.length} kicks`);

const outWav = resolve(OUT, "cult-remix.wav");
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

writeFileSync(resolve(OUT, "cult-remix.events.json"), JSON.stringify({
  schema: "aesthetic.computer/pop-events/v1",
  track: "whistlegraph cult --- remix",
  renderer: "pop/cult/bin/render.mjs",
  source: {
    work: "cult — The Three of Us Are in a Cult (Whistlegraph, 2022)",
    tiktok: "https://www.tiktok.com/@whistlegraph/video/7055106286232325423",
    mirror: "https://assets.aesthetic.computer/whistlegraph/index/posts/7055106286232325423.mp4",
    performers: ["Jeffrey Alan Scudder", "Camille Klein", "Alex Freundlich"],
    transcript: "Dash, dash, dash, dot, dot, dot. The three of us are in a cult.",
    measuredRegisters: { jeffrey: "B2 ≈127 Hz", alex: "F#3 ≈193 Hz", camille: "C#4 ≈245 Hz", cult: "B3 ≈247 Hz" },
  },
  synthesized: {
    provider: "ElevenLabs jeffrey-pvc via /api/say (provider=jeffrey, voice=neutral:0)",
    lines: ["I wanna. I wanna. Run it fast. Run it fast.", "Dash. I wanna. Dash. I wanna. Run it fast. Dot dot."],
  },
  tempoBPM: BPM, key: "B minor", bars: BARS, seconds: +(BARS * BAR).toFixed(2),
  sections: Object.fromEntries(Object.entries(S).map(([k, [a, b]]) => [k, { bars: [a, b], seconds: [+at(a).toFixed(2), +at(b).toFixed(2)] }])),
  hook: "dash — i wanna — dash — i wanna — run it fast — dot dot",
  direction: "B-minor club remix at 128: four-on-the-floor with a ramped duck, sine-bump bass (fundamental + sub + whisper 2nd), harmonized sine stabs, the three real voices as the morse hook, granular-stretched 'cult' as an angelic choir, Special Sign side return, clean un-clipped master",
  prePeak: +peak.toFixed(6), linearTrim: +norm.toFixed(4), kicks: kicks.length,
}, null, 2));

console.log(`✓ ${outWav}  (${(BARS * BAR).toFixed(1)}s)`);
