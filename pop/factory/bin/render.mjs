#!/usr/bin/env node
// render.mjs — "whistlegraph factory --- remix (v1, the stamp)"
//
// A remix of `fact` — "factory 🏭 cookie-cutter🎄personalities"
// (@whistlegraph, 2021-02-04, 50.8M views). The poem is three lines:
//
//     factory / cookie cutter / personalities
//     we must break free from the states that we're in
//     spinning away, I hear a bird
//
// and the title is an instruction. A cookie cutter's whole job is to make
// IDENTICAL copies — so the arrangement's central move is a sampler doing
// exactly that: the same take of the same line stamped onto the grid again
// and again with zero humanization, until the middle line of the poem
// comes true and the copies drift out of spec. The poem already carries
// the arc; nothing had to be invented, only obeyed:
//
//   I    POWER-ON   0:00  Motor hum, relay clicks finding their rhythm.
//                        The take's own count-in — "a one, two, ready,
//                        and…" — is the operator switching the machine on.
//   II   THE STAMP  0:19  The kick is a press. Line 1 stamped every two
//                        bars, IDENTICAL every time — same sample, same
//                        gain, no jitter. That is not laziness, it is the
//                        subject. Conveyor ticks cross the stereo field
//                        left to right, one belt-length per bar.
//   III  FULL SPEC  0:58  The whole poem cycles over the full groove and
//                        the harmony starts to move (Dm · Dm · Bb · C) —
//                        the factory at capacity, everything to tolerance.
//   IV   OUT OF     1:36  Each successive stamp slips: cents of detune
//        SPEC             accumulate, milliseconds of timing slop, double-
//                        strikes, conveyor steps dropped. "we must break
//                        free" starts punching through between the copies.
//   V    BREAK FREE 2:14  THE TURN. Press stops, kick out. The middle line
//                        alone on the floor of the factory — twice, then
//                        the other pressing of it — while the last ticks
//                        slip their timing. The machine has noticed.
//   VI   SPINNING   2:34  The groove returns rotating instead of stamping:
//        AWAY             "spinning away" circles the stereo field, the
//                        harmony travels (Dm · Bb · F · C), and the bird
//                        arrives — chirped in sine over the top, then the
//                        word itself, an octave up.
//   VII  SHUTDOWN   3:12  The press decelerates. Copies come out slower
//                        and lower (a real varispeed drift, not a filter
//                        gag), the hum winds down through a minor third,
//                        the belt stretches its ticks apart. The last
//                        sound is the long low "bird" from the 02-13 take
//                        — G#2, a note the factory's key never contained —
//                        alone, after the music has stopped.
//
// ══ MEASURED, NOT GUESSED (bin/harvest.py, receipt in harvest.json) ═══
//
//   · 100 BPM is the chant's own tempo: median syllable IOI 0.299 s =
//     eighths at 100.3, beat_track 101.4/100.4 on the two 2021-02 takes,
//     and each poem line spans ~2 bars at 100 (line 2 is 4.80 s against a
//     4.80 s cell — the take is already ON this grid).
//   · D minor is the chant's own key: factory, cookie, spinning, I all sit
//     on D3 (~147 Hz); the pitch set is D·F·G·A·Bb·C; "bird" lands on Bb2.
//   · The stamps are raw slices at natural speed — nothing is dragged to
//     fit, because at the source's own tempo nothing has to be.
//
// Every mixing rule from pop/cult carries over: 10 ms raised-cosine tails
// on every voice, ducks that ramp rather than step, sine-bump bass, no
// master tanh, mono-safe equal-power pans with a band-limited antisymmetric
// side return, one linear trim at the end. Mastering is cut-v1.sh's job.
//
//   node pop/factory/bin/render.mjs           # → out/factory-remix-v1-full.wav
//   node pop/factory/bin/render.mjs --stems   # + per-bus stems
//   bash pop/factory/bin/cut-v1.sh            # master + mp3

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
const BPM = 100;                     // the chant's measured tempo
const BEAT = 60 / BPM;               // 0.6 s
const BAR = 4 * BEAT;                // 2.4 s
const BARS = 88;                     // 211.2 s = 3:31
const TAU = Math.PI * 2;
const N = Math.round((BARS * BAR + 3.2) * SR);

// Four buses. Drums never duck. The bed (music) takes the full breath, the
// voices a quarter-depth duck, the machine layer (presses' hiss, relays,
// conveyor) a half-depth one so it sits inside the groove, not on top.
const musicL = new Float32Array(N), musicR = new Float32Array(N);
const drumsL = new Float32Array(N), drumsR = new Float32Array(N);
const voxL = new Float32Array(N), voxR = new Float32Array(N);
const machL = new Float32Array(N), machR = new Float32Array(N);
const sideB = new Float32Array(N);   // bed's side field
const sideV = new Float32Array(N);   // voices'
const sideM = new Float32Array(N);   // the machine's
const dlySend = new Float32Array(N);

const VOXG = 1.42;                   // the +3 dB the vox bus earned in cult v2
const MACHG = 2.40;                  // the machine layer — first stem pass read
                                     // -41 LUFS, a factory with no belt; this
                                     // plus the tick gains brings it to ~-26
const clamp = (v, a, b) => (v < a ? a : v > b ? b : v);
const hz = (midi) => 440 * Math.pow(2, (midi - 69) / 12);
const at = (bar, beat = 0) => bar * BAR + beat * BEAT;
const smooth = (u) => (u <= 0 ? 0 : u >= 1 ? 1 : u * u * (3 - 2 * u));

// Debug: MUTE=press node bin/render.mjs subtracts a voice; ONLY=bird
// renders one alone — the fastest way to argue about one layer.
const MUTED = new Set((process.env.MUTE || "").split(",").map((v) => v.trim()).filter(Boolean));
const ONLY = new Set((process.env.ONLY || "").split(",").map((v) => v.trim()).filter(Boolean));
const allow = (v) => !MUTED.has(v) && (!ONLY.size || ONLY.has(v));
if (MUTED.size) console.log("  MUTED:", [...MUTED].join(", "));
if (ONLY.size) console.log("  ONLY:", [...ONLY].join(", "));
const EVENTS = [];

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

const SAMPLES = resolve(LANE, "samples");
if (!existsSync(SAMPLES)) {
  console.error("samples/ missing — run ../.venv/bin/python bin/harvest.py first");
  process.exit(1);
}
for (const f of readdirSync(SAMPLES))
  if (f.endsWith(".wav")) load(f.replace(/\.wav$/, ""), join(SAMPLES, f));

const DEMOS = resolve(REPO, "pop/demos/samples");
for (const [n, f] of Object.entries({
  hatC: "perc-hat-c.mp3", hatO: "perc-hat-o.mp3", clap: "perc-clap.mp3",
  ride: "perc-ride.mp3", snap: "perc-snap.mp3",
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
  else if (bus === "mach") { machL[i] += mono * cl; machR[i] += mono * cr; }
  else { musicL[i] += mono * cl; musicR[i] += mono * cr; }
  if (dly) dlySend[i] += mono * dly;
  if (sp && sideAmt) {
    const li = i + sp.itd, ri = i - sp.itd;
    const l = li >= 0 && li < N ? mono * sp.gl : 0;
    const r = ri >= 0 && ri < N ? mono * sp.gr : 0;
    const s = 0.5 * (l - r) * sideAmt;
    if (bus === "vox") sideV[i] += s;
    else if (bus === "mach") sideM[i] += s;
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

// ── the kick — cult v5's thick electro press-foot, unchanged ──────────
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

// ── THE PRESS — the stamp made audible ────────────────────────────────
// A die coming down is a thud plus metal. The thud is a short 120→55 Hz
// sweep; the metal is three INHARMONIC partials in free-bar ratios
// (1 : 2.76 : 5.40 — a struck bar, not a chord) around a base tuned to
// the section, with a 2 ms noise chiff for the contact. Fast decay: a
// press strikes, it does not ring like a bell.
function press(t, gain = 1, { base = 392, pan = 0, ring = 1 } = {}) {
  if (!allow("press")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "press", bus: "drums", dur: 0.30,
    gain: +gain.toFixed(3), hz: base });
  const n = Math.round(0.30 * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  const part = [1, 2.76, 5.40].map((r) => ({ f: base * r, p: 0 }));
  let ph = 0, hp = 0, prev = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const f = 55 + 65 * Math.exp(-u * 90);
    ph += (TAU * f) / SR;
    const thud = Math.sin(ph) * Math.exp(-u * 26) * 0.85;
    let metal = 0;
    for (let k = 0; k < 3; k++) {
      part[k].p += (TAU * part[k].f) / SR;
      metal += Math.sin(part[k].p) * Math.exp(-u * (34 + k * 26)) * [0.42, 0.30, 0.16][k];
    }
    const w = nrnd();
    hp = 0.80 * (hp + w - prev); prev = w;
    const chiff = hp * Math.exp(-u * 480) * 0.55;
    const env = Math.min(1, u / 0.0012);
    emit("drums", i0 + i, (thud + metal * ring + chiff) * env * 0.62 * gain * tailFade(i, n),
      pan, sp, 0.25);
  }
}

// ── pneumatic hiss — the press lifting off ────────────────────────────
function hiss(t, gain = 1, { pan = 0.3, dur = 0.22 } = {}) {
  if (!allow("hiss")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "hiss", bus: "mach", dur, gain: +gain.toFixed(3), pan });
  const n = Math.round((dur + 0.03) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    lp += 0.16 * (nrnd() - lp);
    let env = Math.min(1, u / 0.010) * Math.exp(-u * (3.4 / dur) * 1.6);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.03);
    emit("mach", i0 + i, lp * env * 1.10 * gain * tailFade(i, n), pan, sp, 0.6);
  }
}

// ── sine-bump bass (cult's, verbatim) ─────────────────────────────────
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

// Harmonized sines — pad and stab, for the sections that travel.
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

// ── MOTOR HUM — the factory's floor tone ──────────────────────────────
// D1 + D2 + a whisper of the 3rd harmonic, fluttering at 0.4 Hz with an
// 8.3 Hz rotation roughness (a motor is a pitch with a wobble budget).
// `bend` glides the whole stack — shutdown winds it down a minor third.
function hum(t, dur, gain = 1, { bend = 1, fadeIn = 2.0, fadeOut = 2.0 } = {}) {
  if (!allow("hum")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "hum", bus: "music", dur: +dur.toFixed(2),
    gain: +gain.toFixed(3), ...(bend !== 1 ? { bend } : {}) });
  const n = Math.round(dur * SR), i0 = Math.round(t * SR);
  let p1 = 0, p2 = 0, p3 = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const g = Math.pow(bend, smooth(u / dur));           // slow spectral fall
    const f = 36.71 * g;                                 // D1, bending
    p1 += (TAU * f) / SR; p2 += (TAU * f * 2.003) / SR; p3 += (TAU * f * 3) / SR;
    const flutter = 1 + 0.05 * Math.sin(TAU * 0.4 * u) + 0.018 * Math.sin(TAU * 8.3 * u);
    const s = 0.9 * Math.sin(p1) + 0.55 * Math.sin(p2) + 0.13 * Math.sin(p3);
    lp += 0.09 * (s - lp);
    let env = smooth(clamp(u / fadeIn, 0, 1));
    if (u > dur - fadeOut) env *= smooth(clamp((dur - u) / fadeOut, 0, 1));
    emit("music", i0 + i, lp * flutter * env * 0.30 * gain * tailFade(i, n), 0, null, 0);
  }
}

// ── clicks and taps — relays and belt ─────────────────────────────────
let nseed = 20210204;                // the post date is the seed
const nrnd = () => {
  nseed ^= nseed << 13; nseed >>>= 0;
  nseed ^= nseed >>> 17;
  nseed ^= nseed << 5; nseed >>>= 0;
  return (nseed / 4294967295) * 2 - 1;
};
function noiseHit(t, { gain = 1, pan = 0, side = 0.5, dur = 0.004, tone = 0, q = 0.35, dly = 0, evVoice = "click" } = {}) {
  if (!allow(evVoice)) return;
  EVENTS.push({ t: +t.toFixed(4), voice: evVoice, bus: "mach", dur: +dur.toFixed(4),
    gain: +gain.toFixed(3), pan: +pan.toFixed(2) });
  const n = Math.round((dur + 0.006) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let hp = 0, prev = 0, lp = 0, p = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const w = nrnd();
    hp = 0.86 * (hp + w - prev); prev = w;
    lp += 0.42 * (hp - lp);
    let v = lp;
    if (tone) { p += (TAU * tone) / SR; v = v * (1 - q) + Math.sin(p) * q * Math.exp(-u * 120); }
    const env = Math.min(1, u / 0.0009) * Math.exp(-u * (tone ? 150 : 620));
    emit("mach", i0 + i, v * env * 1.10 * gain * tailFade(i, n), pan, sp, side, dly);
  }
}
const click = (t, o = {}) => noiseHit(t, { dur: 0.004, tone: 0, evVoice: "click", ...o });
const tick = (t, o = {}) => noiseHit(t, { dur: 0.014, tone: 1450, q: 0.38, evVoice: "tick", ...o });

// The conveyor: sixteen ticks a bar that CROSS the stereo field — one
// belt-length per bar, left to right, accent every fourth. `drop` is the
// fraction of steps the belt loses (out-of-spec wear), deterministic.
function conveyor(bar, gain = 1, { drop = 0 } = {}) {
  for (let k = 0; k < 16; k++) {
    if (drop > 0 && rnd() < drop) continue;
    const t = at(bar) + k * (BEAT / 4);
    const pan = -0.65 + 1.3 * (k / 15);
    const acc = k % 4 === 0 ? 1.0 : 0.55;
    tick(t, { gain: 0.34 * gain * acc, pan, side: 0.7 });
  }
}

// ── THE BIRD — a sine whistle, not a sample ───────────────────────────
// Two-note chirps gliding up a fourth, vibrato arriving late, high over
// the machine and echoed through the dub delay. The one voice in the
// track with no grid at all.
function birdcall(t, f0 = 2349, { gain = 1, pan = 0.4, chirps = 3 } = {}) {
  if (!allow("bird")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "bird", bus: "vox", hz: f0,
    gain: +gain.toFixed(3), pan: +pan.toFixed(2), chirps });
  const sp = spatial(pan * 1.2);
  for (let c = 0; c < chirps; c++) {
    const t0 = t + c * 0.21, dur = 0.13, n = Math.round(dur * SR);
    const i0 = Math.round(t0 * SR);
    let p = 0;
    for (let i = 0; i < n; i++) {
      const u = i / SR;
      const gl = smooth(clamp(u / 0.09, 0, 1));
      const vib = 1 + 0.012 * Math.sin(TAU * 31 * u) * smooth(u / 0.05);
      const f = f0 * (0.82 + 0.33 * gl) * vib;
      p += (TAU * f) / SR;
      const env = Math.sin(Math.PI * clamp(u / dur, 0, 1));
      emit("vox", i0 + i, Math.sin(p) * env * 0.16 * gain * tailFade(i, n),
        pan, sp, 0.75, 0.5);
    }
  }
}

// ── the one-shot player (cult's, with the wiggle) ─────────────────────
// `wigDrift` is the shutdown's whole trick: cents of varispeed drift
// applied over the note, so a copy can come out of the press slower and
// lower — a real deceleration of the read head, not a post effect.
const missing = new Set();
function shot(name, t, {
  gain = 1, pan = 0, semis = 0, bus = "vox", side = 0.5, dark = 0,
  dur = null, dly = 0, off = 0, evVoice = null,
  wig = 0, wigHz = 5.0, wigPhase = 0, wigDrift = 0, wigIn = 0.45,
} = {}) {
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  if (!allow(evVoice ?? "word")) return;
  const step = Math.pow(2, semis / 12);
  const start = Math.max(0, Math.min(s.length - 2, Math.round(off * SR)));
  let avail = Math.floor((s.length - 2 - start) / step);
  if (wig) avail = Math.floor(avail * 0.965);
  if (wigDrift) {
    // The drift ramps in on a smoothstep, so the average rate over the note
    // is 2^(drift/2 /1200). A slowing read (negative drift) therefore needs
    // MORE output samples to cover the same input — and either way we stop
    // short of the end, because running out mid-read would exit without the
    // tail fade, which is the one thing we never allow.
    const avg = Math.pow(2, (wigDrift * 0.5) / 1200);
    avail = Math.floor((avail / Math.max(avg, 0.05)) * (wigDrift > 0 ? 0.90 : 0.94));
  }
  const n = dur ? Math.min(avail, Math.round(dur * SR)) : avail;
  if (n <= 4) return;
  const i0 = Math.round(t * SR);
  EVENTS.push({
    t: +t.toFixed(4), voice: evVoice ?? "word", bus, sample: name,
    dur: +(n / SR).toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2),
    ...(semis ? { semis } : {}),
    ...(wig ? { wiggleCents: +wig.toFixed(1) } : {}),
    ...(wigDrift ? { driftCents: +wigDrift.toFixed(1) } : {}),
  });
  const sp = spatial(pan * 1.2);
  const span = n / SR;
  const ramp = Math.max(1e-4, wigIn);
  let lp = 0, pos = start;
  for (let i = 0; i < n; i++) {
    const q = pos | 0;
    if (q + 1 >= s.length) break;
    const f = pos - q;
    let v = s[q] + (s[q + 1] - s[q]) * f;
    if (dark > 0) { lp += (1 - dark) * (v - lp); v = lp; }
    const env = Math.min(1, i / (0.0015 * SR));
    emit(bus, i0 + i, v * env * gain * tailFade(i, n), pan, sp, side, dly);
    if (wig || wigDrift) {
      const u = i / SR;
      const d = smooth(u / ramp);
      const cents = d * wig * Math.sin(TAU * wigHz * u + wigPhase)
        + wigDrift * smooth(u / span);
      pos += step * Math.pow(2, cents / 1200);
    } else pos += step;
  }
}
const word = (name, t, o = {}) => shot(name, t, { bus: "vox", side: 0.5, ...o });

// ── harmony ───────────────────────────────────────────────────────────
// D natural minor — the chant's measured key. Machines don't modulate, so
// the stamp sections sit on i; FULL SPEC walks i·i·VI·VII and SPINNING
// walks i·VI·III·VII (Dm Bb F C), one chord a bar... the classic row the
// poem's escape deserves.
const SCALE = [0, 2, 3, 5, 7, 8, 10];             // natural minor
const sd = (i) => SCALE[((i % 7) + 7) % 7] + 12 * Math.floor(i / 7);
const D2 = 38;                                     // bass root
const bassRoot = (deg) => D2 + sd(deg);
const triad = (deg, base = 62) => [sd(deg), sd(deg + 2), sd(deg + 4)].map((s) => base + s);

// ── form ──────────────────────────────────────────────────────────────
const S = {
  poweron:   [0, 8],
  stamp:     [8, 24],
  spec:      [24, 40],
  drift:     [40, 56],
  breakfree: [56, 64],
  spinning:  [64, 80],
  shutdown:  [80, 88],
};
const ACTS = {
  poweron: "I · POWER-ON", stamp: "II · THE STAMP", spec: "III · FULL SPEC",
  drift: "IV · OUT OF SPEC", breakfree: "V · BREAK FREE",
  spinning: "VI · SPINNING AWAY", shutdown: "VII · SHUTDOWN",
};
const inS = (bar, k) => bar >= S[k][0] && bar < S[k][1];

const SPEC_ROW = [0, 0, 5, 6];                    // i · i · VI · VII
const SPIN_ROW = [0, 5, 2, 6];                    // i · VI · III · VII
function degAt(bar) {
  if (inS(bar, "spec")) return SPEC_ROW[(bar - S.spec[0]) % 4];
  if (inS(bar, "spinning")) return SPIN_ROW[Math.floor((bar - S.spinning[0]) / 2) % 4];
  if (inS(bar, "breakfree")) return bar < 60 ? 5 : 6;   // Bb, then C, hanging
  return 0;
}

let seed = 20210204;
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);

// ── THE STAMP — line 1 pressed onto its own grid ──────────────────────
// The chant's syllables are eighths at 100, so the word starts land at
// beats 0 · 1.5 · 2.5 · 3.5 — measured from the take, not designed. In
// THE STAMP every copy is identical by construction: same slices, same
// gains, no jitter. The drift parameters exist so OUT OF SPEC can spend
// them; here they are all zero, and that zero is the concept.
const LINE1 = [
  ["factory",       0.0, 1.00],
  ["cookie",        1.5, 0.95],
  ["cutter",        2.5, 0.97],
  ["personalities", 3.5, 1.00],
];
function stampLine1(bar, g = 1, {
  slipMs = 0, cents = 0, wig = 0, dblAt = -1, dark = 0, take = "",
} = {}) {
  for (let w = 0; w < LINE1.length; w++) {
    const [name, beat, wg] = LINE1[w];
    const nm = take && BANK[name + take] ? name + take : name;
    const slip = slipMs ? ((w % 2 ? -1 : 1) * slipMs) / 1000 : 0;
    const det = cents ? ((w % 2 ? -1 : 1) * cents) / 100 : 0;
    const t = at(bar, beat) + slip;
    word(nm, t, {
      gain: 0.92 * g * wg, semis: det, dark,
      wig, wigHz: 4.3 + w * 0.7, wigPhase: w * 1.9,
      pan: 0, side: 0.4, evVoice: "stamp",
    });
    if (dblAt === w)                       // a die bounce — the copy struck twice
      word(nm, t + 0.048, { gain: 0.34 * g, semis: det - 0.18, dark: 0.3,
        pan: 0.25, side: 0.5, evVoice: "stamp" });
  }
}

// ══ THE SCORE ═════════════════════════════════════════════════════════

// ── I · POWER-ON (bars 0–8) ───────────────────────────────────────────
// The hum arrives, relays find a rhythm, and the operator counts the
// machine in. The count-in is the original take's own first two seconds.
hum(at(0), at(8) - at(0) + 1.5, 0.9, { fadeIn: 3.0, fadeOut: 0.8 });
// relay clicks, sparse then finding the eighth grid
for (let b = 1; b < 8; b++) {
  const density = b < 3 ? 2 : b < 5 ? 4 : 8;
  for (let k = 0; k < density; k++) {
    const t = at(b) + (k * BAR) / density + (b < 5 ? rnd() * 0.06 - 0.03 : 0);
    click(t, { gain: 0.45 + 0.05 * b, pan: (k % 2 ? 0.4 : -0.4), side: 0.7 });
  }
}
hiss(at(3), 0.6, { pan: -0.4, dur: 0.30 });
hiss(at(5, 2), 0.7, { pan: 0.4, dur: 0.26 });
word("count-in", at(4), { gain: 1.05, pan: 0, side: 0.3, evVoice: "count" });
// the machine memorized the operator: "and" again, an octave down, dark
word("and", at(6, 2), { gain: 0.55, semis: -12, dark: 0.45, pan: -0.3, side: 0.5, evVoice: "count" });
press(at(7, 3.5), 0.7, { base: 294, ring: 0.7 });   // the die seats itself

// ── II · THE STAMP (bars 8–24) ────────────────────────────────────────
for (let b = 8; b < 24; b++) {
  kick(at(b, 0), 1.0); kick(at(b, 1), 0.96); kick(at(b, 2), 1.0); kick(at(b, 3), 0.96);
  conveyor(b, b < 10 ? 0.6 : 1.0);
  // offbeat sine-bump bass on the root — the factory floor moving
  for (let e = 0; e < 4; e++) bass(at(b, e + 0.5), D2, 0.22, 0.80);
  if (b % 2 === 0) {
    press(at(b), 1.0, { base: 392 });
    hiss(at(b, 3.5), 0.55, { pan: 0.35 });
  }
}
for (let c = 0; c < 8; c++) {
  const b = 8 + c * 2;
  if (c === 0) continue;                    // one empty cycle: press first, words second
  stampLine1(b, 1.0);                       // IDENTICAL. every. time.
}

// ── III · FULL SPEC (bars 24–40) ──────────────────────────────────────
// The whole poem, twice around an 8-bar cell, the harmony walking.
for (let b = 24; b < 40; b++) {
  kick(at(b, 0), 1.0); kick(at(b, 1), 0.96); kick(at(b, 2), 1.0); kick(at(b, 3), 0.96);
  conveyor(b, 1.0);
  const root = bassRoot(degAt(b));
  for (let e = 0; e < 4; e++) bass(at(b, e + 0.5), root, 0.22, 0.80);
  if (BANK.clap) {
    shot("clap", at(b, 1), { bus: "drums", gain: 0.34, pan: 0.15, side: 0.3, evVoice: "clap" });
    shot("clap", at(b, 3), { bus: "drums", gain: 0.34, pan: -0.15, side: 0.3, evVoice: "clap" });
  }
  if (b % 4 === 0) press(at(b), 1.0, { base: 392 });
  // a low pad breathing under the poem, one chord a bar (the row moves
  // per bar, so the pad must too)
  sines(at(b), triad(degAt(b), 50), BAR - 0.05, 0.16, 0, 0.6, 0.6, 0, 0.7);
}
for (const cell of [24, 32]) {
  stampLine1(cell, 1.0);
  word("line2", at(cell + 2), { gain: 1.00, pan: -0.12, side: 0.5, evVoice: "line" });
  word("line3", at(cell + 4), { gain: 1.00, pan: 0.12, side: 0.5, evVoice: "line" });
  // bars +6,+7: the machine answers — personalities echoed through the dub
  word("personalities", at(cell + 6), { gain: 0.55, semis: -5, dark: 0.35,
    pan: 0.4, side: 0.7, dly: 0.5, evVoice: "echo" });
  word("cutter", at(cell + 7), { gain: 0.50, semis: -5, dark: 0.35,
    pan: -0.4, side: 0.7, dly: 0.5, evVoice: "echo" });
}

// ── IV · OUT OF SPEC (bars 40–56) ─────────────────────────────────────
// Eight more stamps, and stamp k is k steps out of tolerance: cents and
// milliseconds accumulate, the die bounces, the belt drops steps, and the
// middle line starts forcing its way between the copies.
for (let b = 40; b < 56; b++) {
  const k = (b - 40) / 2;
  kick(at(b, 0), 1.0); kick(at(b, 1), 0.96); kick(at(b, 2), 1.0); kick(at(b, 3), 0.96);
  conveyor(b, 1.0, { drop: (b - 40) / 40 });
  for (let e = 0; e < 4; e++) bass(at(b, e + 0.5), D2, 0.22, 0.80);
  if (b % 2 === 0) {
    press(at(b), 1.0, { base: 392 * Math.pow(2, -k * 0.005) });
    hiss(at(b, 3.5), 0.5, { pan: 0.35 });
  }
}
for (let k = 0; k < 8; k++) {
  const b = 40 + k * 2;
  stampLine1(b, 1.0 - k * 0.025, {
    slipMs: k * 9, cents: k * 9, wig: k * 3.2,
    dblAt: k >= 3 ? (k % LINE1.length) : -1,
    dark: k * 0.03,
    take: k === 5 ? "-b" : k === 7 ? "-c" : "",   // wrong dies in the press
  });
  if (k >= 2)
    word("break-free", at(b + 1, 1.5), { gain: 0.55 + k * 0.05, pan: (k % 2 ? 0.45 : -0.45),
      side: 0.6, dly: 0.35, evVoice: "interject" });
  if (k >= 5)
    word("we", at(b + 1, 3), { gain: 0.45, semis: -2, dark: 0.2,
      pan: (k % 2 ? -0.5 : 0.5), side: 0.7, dly: 0.4, evVoice: "interject" });
}

// ── V · BREAK FREE (bars 56–64) ───────────────────────────────────────
// The turn. Press stops, kick out, belt slipping. The middle line of the
// poem, alone on the factory floor — then the OTHER pressing of it, the
// same words in a different voice, which is what breaking out of a
// cookie-cutter personality sounds like.
hum(at(56), at(64) - at(56) + 1.0, 0.75, { fadeIn: 0.6, fadeOut: 1.2 });
sines(at(56), triad(5, 50), BAR * 4 - 0.2, 0.15, 0, 0.7, 0.5, 0.2, 1.4);   // Bb, hanging
sines(at(60), triad(6, 50), BAR * 3.5, 0.14, 0, 0.7, 0.5, 0.2, 1.4);       // C, asking
word("line2", at(56, 1), { gain: 1.12, pan: 0, side: 0.4, evVoice: "line" });
word("break-free", at(58, 3), { gain: 0.6, dly: 0.55, pan: 0.4, side: 0.7, evVoice: "echo" });
word("line2-b", at(60), { gain: 0.95, pan: -0.3, dark: 0.15, side: 0.6, evVoice: "line" });
// the last ticks slip their timing — the belt creeping out of clock
for (let k = 0; k < 14; k++) {
  const t = at(56) + k * (BEAT * 1.07 + k * 0.013);
  if (t > at(63, 2)) break;
  tick(t, { gain: 0.30, pan: -0.5 + (k % 3) * 0.5, side: 0.7 });
}
// three presses, tightening — the machine trying to restart itself
press(at(62, 2), 0.55, { base: 349, ring: 0.8 });
press(at(63, 0.5), 0.70, { base: 370, ring: 0.9 });
press(at(63, 2.5), 0.85, { base: 392, ring: 1.0 });
hiss(at(63, 3.2), 0.8, { pan: 0, dur: 0.35 });

// ── VI · SPINNING AWAY (bars 64–80) ───────────────────────────────────
// The groove comes back rotating instead of stamping. "spinning away"
// circles the field — each repetition a step further around — while the
// harmony finally travels and the bird arrives over the top.
for (let b = 64; b < 80; b++) {
  kick(at(b, 0), 1.0); kick(at(b, 1), 0.96); kick(at(b, 2), 1.0); kick(at(b, 3), 0.96);
  conveyor(b, 0.85);
  const root = bassRoot(degAt(b));
  for (let e = 0; e < 4; e++) bass(at(b, e + 0.5), root, 0.22, 0.82);
  if (BANK.clap) {
    shot("clap", at(b, 1), { bus: "drums", gain: 0.36, pan: 0.15, side: 0.3, evVoice: "clap" });
    shot("clap", at(b, 3), { bus: "drums", gain: 0.36, pan: -0.15, side: 0.3, evVoice: "clap" });
  }
  if (BANK.hatO && b % 2 === 1)
    shot("hatO", at(b, 2.5), { bus: "drums", gain: 0.22, pan: 0.3, side: 0.5, evVoice: "hat" });
  if (BANK.ride && b >= 72)
    shot("ride", at(b, 0.5), { bus: "drums", gain: 0.15, pan: -0.3, side: 0.5, evVoice: "hat" });
  if (b % 2 === 0) sines(at(b), triad(degAt(b), 62), BAR * 2 - 0.1, 0.13, 0.2, 0.7, 1.0, 0.25, 0.6);
}
for (let r = 0; r < 8; r++) {
  const b = 64 + r * 2;
  const pan = 0.8 * Math.sin((TAU * r) / 4);       // a full circle every 8 bars
  const nm = r % 2 === 0 ? "spinning-away" : "spinning-away-c";
  word(nm, at(b, 0), { gain: 0.95, pan, side: 0.75, dly: 0.25, evVoice: "spin" });
}
word("line3", at(70), { gain: 1.0, pan: 0.1, side: 0.5, evVoice: "line" });
word("line3", at(78), { gain: 1.0, pan: -0.1, side: 0.5, evVoice: "line" });
birdcall(at(66, 2.5), 2349, { gain: 1.0, pan: 0.5 });            // D7 territory
birdcall(at(70, 2.5), 2093, { gain: 0.9, pan: -0.5, chirps: 2 }); // C7
birdcall(at(74, 2.5), 2349, { gain: 1.0, pan: 0.55 });
birdcall(at(77, 1.5), 2794, { gain: 0.8, pan: -0.35, chirps: 2 }); // F7
word("bird", at(68, 2), { gain: 0.8, semis: 12, pan: 0.45, side: 0.7, dly: 0.5, evVoice: "birdword" });
word("bird-c", at(76, 2), { gain: 0.75, pan: -0.45, side: 0.7, dly: 0.5, evVoice: "birdword" });

// ── VII · SHUTDOWN (bars 80–88) ───────────────────────────────────────
// The press decelerates: each copy comes out slower and lower — a real
// varispeed drift on the read head — the hum winds down a minor third,
// the belt stretches, the kick thins to nothing.
hum(at(80), at(88) - at(80) - 1.0, 0.85, { bend: 0.84, fadeIn: 1.0, fadeOut: 3.0 });
for (let b = 80; b < 84; b++) {
  kick(at(b, 0), 1.0 - (b - 80) * 0.08); kick(at(b, 2), 0.9 - (b - 80) * 0.08);
  for (let e = 0; e < 2; e++) bass(at(b, e * 2 + 0.5), D2, 0.22, 0.7 - (b - 80) * 0.06);
}
kick(at(84, 0), 0.62); kick(at(85, 0), 0.5); kick(at(86, 0), 0.4);
// the belt stretching its ticks apart
{
  let t = at(80), gap = BEAT / 2;
  while (t < at(87)) {
    tick(t, { gain: 0.30, pan: -0.4 + rnd() * 0.8, side: 0.7 });
    gap *= 1.13; t += gap;
  }
}
stampLine1(80, 0.95);                                      // one last perfect copy
stampLine1(82, 0.85, { slipMs: 6, cents: 12 });            // already sagging
word("line1", at(84), { gain: 0.75, wigDrift: -420, dark: 0.2, side: 0.5, evVoice: "stamp" });
// the last copy never finishes: the read head grinds down an octave and
// the die stops mid-word (dur truncates it; the tail fade catches it)
word("line1", at(86), { gain: 0.60, wigDrift: -1050, dark: 0.4, side: 0.5,
  dur: 3.3, evVoice: "stamp" });
press(at(80), 0.9, { base: 392 });
press(at(82), 0.75, { base: 370 });
press(at(84), 0.6, { base: 330, ring: 0.8 });
press(at(86), 0.45, { base: 294, ring: 0.6 });
hiss(at(86, 2), 0.9, { pan: 0, dur: 0.5 });                // the final exhale
click(at(87, 1.7), { gain: 0.5, pan: 0.2 });               // the main breaker
// and then, after the machine: the bird. G#2 — a note the factory's key
// never contained. It was outside the whole time.
word("bird-b", at(87, 2), { gain: 1.0, pan: 0, side: 0.2, evVoice: "birdword" });

if (missing.size) console.warn("  ! missing samples:", [...missing].join(", "));

// ── dub delay ─────────────────────────────────────────────────────────
{
  const D = Math.round(0.75 * BEAT * SR);       // dotted eighth = 0.45 s
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
    musicL[i] += hpL * 0.50;
    musicR[i] += hpR * 0.50;
  }
}

// ── the duck ──────────────────────────────────────────────────────────
const bedEnv = buildEnv(kicks.map((t) => ({ t, depth: 0.50, atk: 0.009, rel: 0.31 })));

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
  const voxDuck = new Float32Array(N), machDuck = new Float32Array(N);
  for (let i = 0; i < N; i++) { voxDuck[i] = Math.pow(bedEnv[i], 0.25); machDuck[i] = Math.pow(bedEnv[i], 0.5); }
  writeStereo(resolve(dir, "v1-vox.wav"), ...mk(voxL, voxR, voxDuck, VOXG));
  writeStereo(resolve(dir, "v1-music.wav"), ...mk(musicL, musicR, bedEnv, 1));
  writeStereo(resolve(dir, "v1-drums.wav"), ...mk(drumsL, drumsR, null, 1));
  writeStereo(resolve(dir, "v1-mach.wav"), ...mk(machL, machR, machDuck, MACHG));
  console.log(`  stems → ${dir}`);
}

// ── Special Sign side return ──────────────────────────────────────────
const sideOut = new Float32Array(N);
{
  const hpRc = 1 / (TAU * 80), hpA = hpRc / (hpRc + 1 / SR);
  const lpK = 1 - Math.exp((-TAU * 11500) / SR);
  let hp = 0, lp = 0, prev = 0, send = 0.9;
  for (let i = 0; i < N; i++) {
    const be = bedEnv[i];
    const s = sideB[i] * be + sideV[i] * Math.pow(be, 0.25) + sideM[i] * Math.pow(be, 0.5) * MACHG;
    hp = hpA * (hp + s - prev); prev = s;
    lp += lpK * (hp - lp);
    const bar = (i / SR) / BAR;
    const target =
      bar < S.stamp[0] ? 0.88 :
      bar < S.spec[0] ? 0.60 :
      bar < S.drift[0] ? 0.70 :
      bar < S.breakfree[0] ? 0.78 :
      bar < S.spinning[0] ? 0.55 :
      bar < S.shutdown[0] ? 0.92 : 0.70;
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
  const dm = Math.pow(be, 0.5);
  const fadeIn = Math.min(1, i / (0.014 * SR));
  const fadeOut = Math.min(1, (N - 1 - i) / (1.4 * SR));
  const fade = Math.max(0, Math.min(fadeIn, fadeOut));
  const l = (musicL[i] * be + voxL[i] * dv * VOXG + machL[i] * dm * MACHG
    + drumsL[i] + sideOut[i]) * fade;
  const r = (musicR[i] * be + voxR[i] * dv * VOXG + machR[i] * dm * MACHG
    + drumsR[i] - sideOut[i]) * fade;
  L[i] = l; R[i] = r;
  if (Math.abs(l) > peak) peak = Math.abs(l);
  if (Math.abs(r) > peak) peak = Math.abs(r);
}
const norm = peak > 1e-9 ? 0.92 / peak : 1;
for (let i = 0; i < N; i++) { L[i] *= norm; R[i] *= norm; }
console.error(`# pre-master peak ${peak.toFixed(6)} · linear trim ${norm.toFixed(3)}`);

const outWav = resolve(OUT, "factory-remix-v1-full.wav");
writeStereo(outWav, L, R);

EVENTS.sort((a, b) => a.t - b.t || (a.bus < b.bus ? -1 : 1));
const voiceCounts = {};
for (const e of EVENTS) voiceCounts[e.voice] = (voiceCounts[e.voice] ?? 0) + 1;
console.log(`  ${EVENTS.length} events · ${Object.entries(voiceCounts).map(([k, v]) => `${k} ${v}`).join(" · ")}`);
writeFileSync(resolve(OUT, "factory-remix-v1.events.json"), JSON.stringify({
  schema: "aesthetic.computer/pop-events/v1",
  track: "whistlegraph factory --- remix (v1, the stamp)",
  renderer: "pop/factory/bin/render.mjs",
  bpm: BPM,
  key: "D minor",
  narrative: {
    logline: "A cookie cutter's whole job is identical copies — so the sampler stamps the "
      + "phrase out identically until the poem's middle line comes true and the copies "
      + "drift out of spec.",
    premise: "The source take's own count-in switches the machine on; its own last word — "
      + "the long low bird from the second pressing, on a note outside the factory's key — "
      + "switches it off.",
    turn: "2:14 — BREAK FREE. Press stops, kick out, belt slipping: the middle line of the "
      + "poem alone on the factory floor, then the same words from the other pressing.",
    acts: Object.fromEntries(Object.entries(S).map(([k, [a, b]]) => [k, {
      act: ACTS[k], bars: [a, b],
      seconds: [+at(a).toFixed(2), +at(b).toFixed(2)],
    }])),
  },
  measured: {
    tempo: "median syllable IOI 0.299 s = eighths at 100.3 BPM; beat_track 101.4/100.4 "
      + "on the two 2021-02 takes; each poem line spans ~2 bars at 100",
    key: "chant pitch set D·F·G·A·Bb·C rooted on D3 (~147 Hz); 'bird' lands on Bb2; "
      + "the 02-13 take's long bird is G#2 — outside the key, kept as the last word",
  },
  events: EVENTS,
}, null, 1));
console.log(`✓ ${outWav}`);
