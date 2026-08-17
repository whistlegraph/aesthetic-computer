#!/usr/bin/env node
// render3.mjs — "whistlegraph loner --- remix (v3, angelic drum & bass)"
//
// @jeffrey, in order: "for camilles we need to like make her vocals more
// angelic" · "more arpeggiating within them" · "no alex voice — this
// track is all solo camille" · "i want a kick in it — sitting kick ick
// waiting kick cik" · "lets start splitting it up — making it more
// almost like a drum and bass".
//
// So v3 is v2's ballad wearing a drum & bass frame:
//
//   ANGELIC   Every vocal is still a WORLD render (bin/halo.py), now
//             with AIR (+2.5 dB above 8 kHz in the envelope) and BREATH
//             (aperiodicity lifted on held vowels only). Behind every
//             lead phrase a SELF-CHOIR: the same slice doubled an octave
//             up (f0 × 2, envelope untouched — head-voice, not
//             chipmunk), two copies detuned +6/−7 cents, 28/41 ms late,
//             darker, vowels-only, panned ±0.55 — a halo, not a duet.
//             Diatonic 3rds/5ths swell in on the held phrase-ends
//             ("myself…", "stone…", "pass…") with 0.5 s attacks.
//   HARP      Her held vowels also break into ARPEGGIOS of herself:
//             vowel excerpts re-sung flat at A# minor tones (st +12…+27)
//             and run 4–7 notes at 0.12–0.20 s — dotArp()'s move (cult
//             render10) built from WORLD notes.
//   SOLO      Camille only. The o-/s- takes and every spoken aside are
//             gone; f- and n- singing is all her.
//   KICK      In the verse the kick serves her words, not the grid:
//             placed in the syllable gaps (sitting·KICK·ick), with tiny
//             chopped fragments of her own WORLD renders — the "ick",
//             the reversed "cik" — bouncing off each kick. The voice
//             joins the kit.
//   DNB       The harmonic floor stays at 80 (pads, halos, space — the
//             ballad DNA), the drums think in 160: a synthesized
//             two-step (kick steps 0·10, snare 4·12 on the 16th grid),
//             rolling velocity-shaped 16th ticks, ghost snares, chop
//             syncopations, and a moving sub under the pads. No sample
//             packs — the break is built from this lane's own voices.
//   SPACE     The vox bus gets a real diffuse tail: a decorrelated
//             Schroeder pair — 4 combs per side (44.6–54.2 ms, R offset
//             +23…+37 samples), RT60 ≈ 3.2 s, 3.4 kHz one-pole damping
//             in the loop, two series allpasses (22.2 / 9.0 ms, g 0.7),
//             40 ms pre-delay, high-passed at 180 Hz on the return —
//             mixed under the lead, hotter under the halos.
//
// Form, 52 bars at 80 BPM (2:36 + tail):
//
//   INTRO    0:00  0–4    hiss, drone, a first arp forming, a halo with
//                         no lead inside it
//   VERSE    0:12  4–12   the Feral take over pads — and the kicks
//                         arrive between her syllables
//   BUILD    0:36  12–16  the two-step assembles around her last phrase
//   DROP 1   0:48  16–26  drum & bass under the "not again!" verse,
//                         chops bouncing, harp answering
//   BREATHE  1:18  26–34  everything stops: the whole take, naked but
//                         HALOED, in the long tail
//   REBUILD  1:42  34–36  the kit re-forms while "pass" still rings
//   DROP 2   1:48  36–46  fuller: of-a-stone octaves answering, rolls,
//                         both harps at once
//   OUT      2:18  46–52  the reverb and the hiss finish it
//
// Tuning is v2's decision, untouched: TONIC = 237 Hz, the +30¢ lives in
// the aesthetivox grid. Mixing rules survive: raised-cosine tails, no
// master tanh, one linear trim, mono-safe pans + antisymmetric side.
//
//   node pop/loner/bin/render3.mjs           # → out/loner-remix-v3-full.wav
//   node pop/loner/bin/render3.mjs --stems   # + per-bus stems
//   MUTE=arp node …  /  ONLY=chop node …     # subtract or solo voices

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const OUT = resolve(LANE, "out");
mkdirSync(OUT, { recursive: true });

const SR = 48_000;
const BPM = 80;                       // the harmonic floor; the drums think 160
const BEAT = 60 / BPM;                // 0.75 s
const BAR = 4 * BEAT;                 // 3.0 s
const STEP = BEAT / 4;                // 0.1875 s — the 16th grid the kit lives on
const BARS = 52;                      // 156 s = 2:36
const TAU = Math.PI * 2;
const N = Math.round((BARS * BAR + 3.2) * SR);

const TONIC = 237.0;                  // A#3, Camille's own centre (v2 decision)
const hzOf = (st) => TONIC * Math.pow(2, st / 12);

// ── buses ─────────────────────────────────────────────────────────────
const musicL = new Float32Array(N), musicR = new Float32Array(N);
const drumsL = new Float32Array(N), drumsR = new Float32Array(N);
const voxL = new Float32Array(N), voxR = new Float32Array(N);
const sideB = new Float32Array(N);
const sideV = new Float32Array(N);
const dlySend = new Float32Array(N);
const rvbSend = new Float32Array(N);  // the vox bus's diffuse tail

const VOXG = 1.42;
const clamp = (v, a, b) => (v < a ? a : v > b ? b : v);
const at = (bar, beat = 0) => bar * BAR + beat * BEAT;
const smooth = (u) => (u <= 0 ? 0 : u >= 1 ? 1 : u * u * (3 - 2 * u));
const tailFade = (i, n) => {
  const u = (n - 1 - i) / (0.010 * SR);
  return u >= 1 ? 1 : u <= 0 ? 0 : u * u * (3 - 2 * u);
};

const MUTED = new Set((process.env.MUTE || "").split(",").map((v) => v.trim()).filter(Boolean));
const ONLY = new Set((process.env.ONLY || "").split(",").map((v) => v.trim()).filter(Boolean));
const allow = (v) => !MUTED.has(v) && (!ONLY.size || ONLY.has(v));
if (MUTED.size) console.log("  MUTED:", [...MUTED].join(", "));
if (ONLY.size) console.log("  ONLY:", [...ONLY].join(", "));
const EVENTS = [];

// ── sample bank — vox3/ ONLY: all Camille, all aesthetivoxed ──────────
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
{
  const d = resolve(LANE, "vox3");
  for (const f of readdirSync(d)) if (f.endsWith(".wav")) load(f.replace(/\.wav$/, ""), join(d, f));
}
const DEMOS = resolve(REPO, "pop/demos/samples");
for (const [n, f] of Object.entries({ hatC: "perc-hat-c.mp3", tambo: "perc-tambo.mp3" }))
  load(n, join(DEMOS, f));
const missing = new Set();

// ── space ─────────────────────────────────────────────────────────────
function spatial(az) {
  const itd = Math.round(0.00027 * SR * Math.sin(az));
  const shadow = 0.35 * Math.sin(az);
  return { itd, gl: 1 - shadow, gr: 1 + shadow };
}
function emit(bus, i, mono, pan, sp, sideAmt, dly = 0, rvb = 0) {
  if (i < 0 || i >= N) return;
  const a = (Math.PI / 4) * (1 + pan);
  const cl = Math.cos(a), cr = Math.sin(a);
  if (bus === "drums") { drumsL[i] += mono * cl; drumsR[i] += mono * cr; }
  else if (bus === "vox") { voxL[i] += mono * cl; voxR[i] += mono * cr; }
  else { musicL[i] += mono * cl; musicR[i] += mono * cr; }
  if (dly) dlySend[i] += mono * dly;
  if (rvb) rvbSend[i] += mono * rvb;
  if (sp && sideAmt) {
    const li = i + sp.itd, ri = i - sp.itd;
    const l = li >= 0 && li < N ? mono * sp.gl : 0;
    const r = ri >= 0 && ri < N ? mono * sp.gr : 0;
    const s = 0.5 * (l - r) * sideAmt;
    if (bus === "vox") sideV[i] += s; else sideB[i] += s;
  }
}

// ── sidechain ─────────────────────────────────────────────────────────
const thumps = [], dnbKicks = [];
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

// ── the kit — synthesized, no sample packs ────────────────────────────
// thump: v1's felt heartbeat, kept for the verse.
const SOFTSAT = Math.tanh(1.3);
function thump(t, gain = 1) {
  if (!allow("thump")) return;
  thumps.push(t);
  EVENTS.push({ t: +t.toFixed(4), voice: "thump", bus: "drums", dur: 0.5, gain: +gain.toFixed(3) });
  const n = Math.round(0.5 * SR), i0 = Math.round(t * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const f = 46 + 36 * Math.exp(-u * 30);
    ph += (TAU * f) / SR;
    const env = Math.exp(-u * 9) * Math.min(1, u / 0.004);
    const body = Math.tanh(Math.sin(ph) * env * 1.3) / SOFTSAT;
    const felt = Math.sin(TAU * 190 * u) * Math.exp(-u * 60) * 0.10;
    emit("drums", i0 + i, (body + felt) * 0.52 * gain * tailFade(i, n), 0, null, 0);
  }
}
// kick: the DnB one — between cult's POW and the thump. 155→48 Hz sweep,
// a slam over a tail, drive 1.9 on the voice, one small 2.2 kHz blip so
// the two-step has an edge to read at speed. Still no master clipping.
const KSAT = Math.tanh(1.9);
function kick(t, gain = 1) {
  if (!allow("kick")) return;
  dnbKicks.push(t);
  EVENTS.push({ t: +t.toFixed(4), voice: "kick", bus: "drums", dur: 0.34, gain: +gain.toFixed(3) });
  const n = Math.round(0.34 * SR), i0 = Math.round(t * SR);
  let ph = 0, sub = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const f = 48 + 107 * Math.exp(-u * 45);
    ph += (TAU * f) / SR;
    sub += (TAU * 45) / SR;
    const env = (0.6 * Math.exp(-u * 28) + 0.5 * Math.exp(-u * 9)) * Math.min(1, u / 0.0012);
    const body = Math.tanh(Math.sin(ph) * env * 1.9) / KSAT;
    const low = Math.sin(sub) * Math.exp(-u * 7) * 0.22;
    const blip = Math.exp(-u * 520) * 0.07 * Math.sin(TAU * 2200 * u);
    emit("drums", i0 + i, (body + low + blip) * 0.80 * gain * tailFade(i, n), 0, null, 0);
  }
}
// snare: a 196 Hz knock inside a band-passed noise burst, mild drive.
let nseed = 20210725;
const nrnd = () => {
  nseed ^= nseed << 13; nseed ^= nseed >>> 17; nseed ^= nseed << 5;
  return ((nseed >>> 0) / 4294967296) * 2 - 1;
};
function snare(t, gain = 1, pan = 0.06) {
  if (!allow("snare")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "snare", bus: "drums", dur: 0.18, gain: +gain.toFixed(3) });
  const n = Math.round(0.18 * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let ph = 0, bp = 0, bp2 = 0;
  const k = 1 - Math.exp((-TAU * 3200) / SR);
  const k2 = 1 - Math.exp((-TAU * 1200) / SR);
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    ph += (TAU * 196) / SR;
    const w = nrnd();
    bp += k * (w - bp); bp2 += k2 * (w - bp2);
    const noise = (bp - bp2) * Math.exp(-u * 22);
    const knock = Math.sin(ph) * Math.exp(-u * 30) * 0.5;
    const s = Math.tanh((noise * 1.6 + knock) * 1.4);
    emit("drums", i0 + i, s * 0.52 * gain * Math.min(1, u / 0.001) * tailFade(i, n),
      pan, sp, 0.3, 0, 0.05);
  }
}
function brush(t, { gain = 1, pan = 0.2, dur = 0.85, rise = 0.42 } = {}) {
  if (!allow("brush")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "brush", bus: "drums", dur, gain: +gain.toFixed(3), pan });
  const n = Math.round(dur * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  let bp = 0, bp2 = 0;
  const k = 1 - Math.exp((-TAU * 1500) / SR);
  const k2 = 1 - Math.exp((-TAU * 380) / SR);
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const w = nrnd();
    bp += k * (w - bp); bp2 += k2 * (w - bp2);
    const env = u < rise ? smooth(u / rise) : Math.max(0, 1 - (u - rise) / (dur - rise));
    emit("drums", i0 + i, (bp - bp2) * env * env * 0.16 * gain * tailFade(i, n), pan, sp, 0.5);
  }
}
function tick(t, gain = 0.12, pan = 0.3) {
  shot("hatC", t, { bus: "drums", gain, pan, dark: 0.55, side: 0.4, evVoice: "tick" });
}

// ── tape ──────────────────────────────────────────────────────────────
function hissBed() {
  if (!allow("hiss")) return;
  let lp = 0, hp = 0, prev = 0, lvl = 0.010;
  const kLp = 1 - Math.exp((-TAU * 5200) / SR);
  const hpRc = 1 / (TAU * 320), hpA = hpRc / (hpRc + 1 / SR);
  for (let i = 0; i < N; i++) {
    const bar = i / SR / BAR;
    const target =
      bar < 4 ? 0.012 : bar < 26 ? 0.0045 : bar < 34 ? 0.010 :
      bar < 46 ? 0.0045 : 0.013;
    lvl += 0.000004 * (target - lvl);
    const w = nrnd();
    lp += kLp * (w - lp);
    hp = hpA * (hp + lp - prev); prev = lp;
    musicL[i] += hp * lvl; musicR[i] += hp * lvl * 0.94;
  }
}
function dust(t, gain = 0.018) {
  if (!allow("hiss")) return;
  const n = Math.round(0.0022 * SR), i0 = Math.round(t * SR);
  for (let i = 0; i < n; i++)
    emit("music", i0 + i, nrnd() * Math.exp(-i / (0.0006 * SR)) * gain, nrnd() * 0.5, null, 0);
}

// ── the pad, with wow (v1's, unchanged) ───────────────────────────────
function pad(t, sts, dur, gain, { attack = 1.2, pan = 0, side = 0.6, dly = 0, dark = 0.30 } = {}) {
  if (!allow("pad")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "pad", bus: "music", sts, dur: +dur.toFixed(3),
    gain: +gain.toFixed(3), pan: +pan.toFixed(2) });
  const n = Math.round((dur + 0.9) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.2);
  const ph = sts.map((_, v) => [((t * 7 + v * 2.39) % 1) * TAU, ((t * 3 + v * 1.17) % 1) * TAU]);
  const drift = sts.map((_, v) => 0.055 + 0.02 * v);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    const wall = (i0 + i) / SR;
    const wow = 0.0023 * Math.sin(TAU * 0.38 * wall)
      + 0.0011 * Math.sin(TAU * 0.11 * wall + 1.7);
    let s = 0;
    for (let v = 0; v < sts.length; v++) {
      const f = hzOf(sts[v]) * (1 + wow + 0.0007 * Math.sin(TAU * drift[v] * wall + v * 2.1));
      ph[v][0] += (TAU * f) / SR;
      ph[v][1] += (TAU * f * 1.0028) / SR;
      s += Math.sin(ph[v][0]) + 0.62 * Math.sin(ph[v][1]);
    }
    s /= sts.length * 1.62;
    lp += (1 - dark) * 0.32 * (s - lp);
    let env = smooth(u / attack);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.9);
    emit("music", i0 + i, lp * env * gain * tailFade(i, n), pan, sp, side, dly);
  }
}

// ── bass — soft in the verse, fast for the sub ────────────────────────
function bass(t, st, dur, gain = 1, attack = 0.045) {
  if (!allow("bass")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "bass", bus: "music", st, dur: +dur.toFixed(3), gain: +gain.toFixed(3) });
  const n = Math.round((dur + 0.16) * SR), i0 = Math.round(t * SR);
  const f = hzOf(st);
  let p1 = 0, p2 = 0, p3 = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
    let env = smooth(u / attack);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.16);
    const s = Math.sin(p1) + 0.48 * Math.sin(p2) + 0.07 * Math.sin(p3);
    lp += 0.42 * (s - lp);
    emit("music", i0 + i, lp * 0.38 * env * gain * tailFade(i, n), 0, null, 0);
  }
}

// ── the one-shot player — attack, reverse, reverb send ────────────────
function shot(name, t, {
  gain = 1, pan = 0, semis = 0, bus = "vox", side = 0.35, dark = 0,
  dur = null, dly = 0, rvb = 0, off = 0, rev = false, attack = 0.0015,
  evVoice = null, who = null,
} = {}) {
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  const voice = evVoice ?? (/^(f|n)-/.test(name) ? "take" : name);
  if (!allow(voice)) return;
  const step = Math.pow(2, semis / 12);
  const start = Math.max(0, Math.min(s.length - 2, Math.round(off * SR)));
  const avail = Math.floor((rev ? start || s.length - 2 : s.length - 2 - start) / step);
  const n = dur ? Math.min(avail, Math.round(dur * SR)) : avail;
  if (n <= 4) return;
  const i0 = Math.round(t * SR);
  EVENTS.push({ t: +t.toFixed(4), voice, bus, sample: name,
    dur: +(n / SR).toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2),
    ...(who ? { who } : {}), ...(semis ? { semis } : {}), ...(rev ? { rev } : {}) });
  const sp = spatial(pan * 1.2);
  let lp = 0, pos = rev ? (start || s.length - 2) : start;
  for (let i = 0; i < n; i++) {
    const q = pos | 0;
    if (q + 1 >= s.length || q < 0) break;
    const f = pos - q;
    let v = s[q] + (s[q + 1] - s[q]) * f;
    if (dark > 0) { lp += (1 - dark) * (v - lp); v = lp; }
    const env = smooth((i / SR) / attack);
    emit(bus, i0 + i, v * env * gain * tailFade(i, n), pan, sp, side, dly, rvb);
    pos += rev ? -step : step;
  }
}
const sung = (name, t, o = {}) =>
  shot(name, t, { bus: "vox", side: 0.5, who: "camille", rvb: 0.28, ...o });

// ── the halo ──────────────────────────────────────────────────────────
// The octave self-choir behind a lead: two vowels-only WORLD renders of
// the same slice, +6/−7 cents, 28/41 ms late, wide, wet, slow.
function halo(name, t, g = 0.18, { attack = 0.35, rvb = 0.55 } = {}) {
  shot(`${name}-8ve-a`, t + 0.028, { bus: "vox", gain: g, pan: -0.55, side: 0.9,
    dly: 0.20, rvb, attack, evVoice: "halo", who: "camille" });
  shot(`${name}-8ve-b`, t + 0.041, { bus: "vox", gain: g * 0.92, pan: 0.55, side: 0.9,
    dly: 0.24, rvb, attack, evVoice: "halo", who: "camille" });
}
// A diatonic interval swelling in at a held word: `off` seconds into the
// (vowels-only) interval render, 0.5 s attack after the lead has landed.
function swell(name, tag, t, off, g = 0.2, attack = 0.5) {
  shot(`${name}-${tag}`, t + off, { bus: "vox", gain: g, pan: tag === "3rd" ? 0.4 : -0.4,
    side: 0.85, dly: 0.26, rvb: 0.6, off, attack, evVoice: "swell", who: "camille" });
}
const haloed = (name, t, o = {}, hg = 0.18, ho = {}) => {
  sung(name, t, o);
  halo(name, t, hg, ho);
};

// ── the harp — dotArp() with WORLD notes ──────────────────────────────
// Chord-tone runs from the arp-note bank (st +12…+27), decaying gains,
// alternating pans, lightly swung, very wet.
let seed = 20210725;
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);
const jit = (ms = 5) => ((rnd() - 0.5) * 2 * ms) / 1000;
const ARP_TONES = {
  i: [12, 15, 19, 24, 27], III: [15, 19, 22, 27], iv: [17, 20, 24],
  VI: [15, 20, 24, 27], VII: [17, 22, 26],
};
function arp(t, deg, { vowel = "oh", count = 5, up = true, gap = 0.15,
  gain = 0.15, pan = 0.35 } = {}) {
  const tones = ARP_TONES[deg];
  for (let k = 0; k < count; k++) {
    const st = tones[up ? k % tones.length : tones.length - 1 - (k % tones.length)]
      + 12 * Math.floor(k / tones.length) * (up ? 1 : -1);
    const nm = `arp-${vowel}-${st}`;
    if (!BANK[nm]) continue;
    shot(nm, t + k * gap + jit(4) + (k % 2 ? 0.010 : 0), {
      bus: "vox", gain: gain * (1 - 0.06 * k), pan: pan * (k % 2 ? -1 : 1),
      side: 0.8, dly: 0.30, rvb: 0.5, dur: 0.22, attack: 0.022,
      evVoice: "arp", who: "camille",
    });
  }
}

// ── the chops — her words as percussion ───────────────────────────────
// Tiny fragments of the WORLD leads (never raw takes): the "ick" of
// sitting, the reversed "cik", the "ting" of waiting. They bounce off
// the kicks on the 16th grid.
const CHOPS = [
  { name: "f-sitting-curled", off: 1.06, dur: 0.10 },              // "ick"
  { name: "f-sitting-curled", off: 1.16, dur: 0.09, rev: true },   // "cik"
  { name: "f-waiting-patiently", off: 2.92, dur: 0.10 },           // "ting"
  { name: "f-waiting-patiently", off: 3.02, dur: 0.09, rev: true },
  { name: "n-getting-curled", off: 0.95, dur: 0.11 },              // "ting"
  { name: "f-think-stone", off: 0.32, dur: 0.09 },                 // "ink"
  { name: "f-pass", off: 0.72, dur: 0.10, rev: true },             // "ss"
  { name: "f-for-time-to-pass", off: 4.42, dur: 0.12 },            // "pa"
];
function chop(k, t, gain = 0.30, pan = 0) {
  const c = CHOPS[k % CHOPS.length];
  shot(c.name, t, { bus: "drums", gain, pan, off: c.off, dur: c.dur, rev: !!c.rev,
    dark: 0.15, side: 0.5, rvb: 0.08, attack: 0.002, evVoice: "chop", who: "camille" });
}

// ── harmony ───────────────────────────────────────────────────────────
const CH = {
  i:   { root: 0,  tones: [0, 3, 7] },
  III: { root: 3,  tones: [3, 7, 10] },
  iv:  { root: 5,  tones: [5, 8, 12] },
  VI:  { root: -4, tones: [-4, 0, 3] },
  VII: { root: -2, tones: [-2, 2, 5] },
};
const ROWS = {
  intro:   ["i", "i", "i", "i"],
  verse:   ["i", "i", "VI", "VI", "III", "III", "VII", "VII"],
  build:   ["iv", "iv", "VII", "VII"],
  drop1:   ["i", "i", "VI", "VI", "III", "III", "iv", "iv", "VII", "VII"],
  breathe: ["i", "i", "i", "i", "i", "i", "i", "i"],
  rebuild: ["VI", "VII"],
  drop2:   ["VI", "VI", "VII", "VII", "i", "i", "VI", "VI", "VII", "VII"],
  out:     ["i", "i", "i", "i", "i", "i"],
};
const S = {
  intro:   [0, 4],
  verse:   [4, 12],
  build:   [12, 16],
  drop1:   [16, 26],
  breathe: [26, 34],
  rebuild: [34, 36],
  drop2:   [36, 46],
  out:     [46, 52],
};
const ACTS = {
  intro: "I · HALO", verse: "II · SITTING·KICK·ICK", build: "III · SPLITTING UP",
  drop1: "IV · DROP", breathe: "V · BREATHE (NAKED, HALOED)",
  rebuild: "VI · REFORM", drop2: "VII · OF A STONE, DOUBLE", out: "VIII · TAIL",
};
const inS = (bar, k) => bar >= S[k][0] && bar < S[k][1];
const sectionAt = (bar) => Object.keys(S).find((k) => inS(bar, k)) ?? "out";
const degAt = (bar) => {
  const k = sectionAt(bar);
  return ROWS[k][bar - S[k][0]] ?? "i";
};
const chordAt = (bar) => CH[degAt(bar)];
const dropBar = (bar) => inS(bar, "drop1") || inS(bar, "drop2");

// ── score ─────────────────────────────────────────────────────────────
console.log(`→ scoring ${BARS} bars @ ${BPM} BPM (drums at ${BPM * 2}) · A# minor @ ${TONIC} Hz`);

// THE BED — pads on the 2-bar harmonic rhythm; nothing during BREATHE
// and OUT except one final low chord. The ballad DNA the drums split up.
for (let bar = 0; bar < S.breathe[0]; bar += 2) {
  const ch = chordAt(bar);
  const t = at(bar) + 0.02, dur = 2 * BAR - 0.1;
  const low = [ch.root - 24, ch.root - 12, ch.tones[1] - 12];
  const g = inS(bar, "intro") ? 0.13 : inS(bar, "verse") ? 0.16 :
    inS(bar, "build") ? 0.17 : 0.16;
  pad(t, low, dur, g, { attack: inS(bar, "intro") ? 2.6 : 1.2, side: 0.55 });
  if (bar >= S.build[0])
    pad(t + 0.15, ch.tones.map((s) => s), dur - 0.3, g * 0.72,
      { attack: 1.6, pan: 0.18, side: 0.7, dly: 0.10, dark: 0.36 });
}
for (let bar = S.rebuild[0]; bar < S.out[0]; bar += 2) {
  const ch = chordAt(bar);
  const t = at(bar) + 0.02, dur = 2 * BAR - 0.1;
  pad(t, [ch.root - 24, ch.root - 12, ch.tones[1] - 12], dur, 0.18, { attack: 1.1, side: 0.55 });
  pad(t + 0.15, ch.tones.map((s) => s), dur - 0.3, 0.13,
    { attack: 1.5, pan: 0.18, side: 0.7, dly: 0.10, dark: 0.36 });
  if (inS(bar, "drop2"))
    pad(t + 0.4, ch.tones.map((s) => s + 12), dur - 0.8, 0.10,
      { attack: 2.2, pan: -0.3, side: 0.85, dly: 0.22, dark: 0.20 });
}
pad(at(46), [-24, -12, -5], 3 * BAR, 0.08, { attack: 3.0, side: 0.5 });

// THE BASS — soft whole roots in the verse; a moving sub in the drops.
for (let bar = S.verse[0]; bar < S.breathe[0]; bar++) {
  const ch = chordAt(bar);
  if (dropBar(bar) || (inS(bar, "build") && bar >= 14)) {
    bass(at(bar), ch.root - 24, 0.42, 0.95, 0.012);
    bass(at(bar) + 6 * STEP, ch.root - 24, 0.30, 0.72, 0.012);
    bass(at(bar) + 10 * STEP, ch.root - 24, 0.42, 0.86, 0.012);
    if (bar % 2 === 1) bass(at(bar) + 14 * STEP, ch.root - 24 + 7, 0.20, 0.60, 0.012);
  } else {
    bass(at(bar), ch.root - 24, BAR - 0.12, 0.54);
  }
}
for (let bar = S.rebuild[0]; bar < S.out[0]; bar++) {
  const ch = chordAt(bar);
  if (inS(bar, "drop2") || bar >= 35) {
    bass(at(bar), ch.root - 24, 0.42, 0.98, 0.012);
    bass(at(bar) + 6 * STEP, ch.root - 24, 0.30, 0.74, 0.012);
    bass(at(bar) + 10 * STEP, ch.root - 24, 0.42, 0.88, 0.012);
    if (bar % 2 === 1) bass(at(bar) + 14 * STEP, ch.root - 24 + 7, 0.20, 0.62, 0.012);
  } else {
    bass(at(bar), ch.root - 24, BAR - 0.12, 0.56);
  }
}

// THE KIT — the two-step. Kick steps 0·10, snare 4·12, ghosts, rolling
// velocity-shaped ticks, chops on the syncopations. Drop 2 adds the roll.
const VEL = [1, 0.45, 0.7, 0.5, 0.9, 0.45, 0.72, 0.5, 1, 0.45, 0.7, 0.5, 0.85, 0.5, 0.78, 0.55];
function dnbBar(bar, { hatG = 0.085, kickG = 0.95, snareG = 0.8, chops = true, roll = false } = {}) {
  const t = at(bar);
  kick(t, kickG);
  kick(t + 10 * STEP, kickG * 0.92);
  if (bar % 4 === 2) kick(t + 6 * STEP, kickG * 0.5);
  snare(t + 4 * STEP, snareG);
  snare(t + 12 * STEP, snareG * 0.96);
  if (bar % 2 === 1) snare(t + 7 * STEP, snareG * 0.32, -0.2);
  if (roll && bar % 4 === 3)
    for (let k = 0; k < 3; k++) snare(t + (15 + k / 3) * STEP, 0.34 * (1 - 0.22 * k), 0.25);
  for (let s = 0; s < 16; s++) tick(t + s * STEP + jit(2), hatG * VEL[s], s % 2 ? 0.3 : -0.24);
  if (bar % 2 === 0) shot("tambo", t + 2 * STEP, { bus: "drums", gain: 0.10, pan: 0.35, dark: 0.4, side: 0.5, evVoice: "tick" });
  if (chops)
    for (const s of [3, 6, 11, 14])
      if ((bar * 7 + s) % 3 === 0) chop(bar + s, t + s * STEP, 0.30, s % 2 ? 0.35 : -0.35);
}

// INTRO — a halo with no lead inside it, and the first arp forming.
dust(at(0, 1.2)); dust(at(1, 2.6), 0.014); dust(at(3, 0.7), 0.012);
halo("f-of-a-stone", at(2), 0.14, { attack: 0.6, rvb: 0.7 });
arp(at(1), "i", { vowel: "oh", count: 5, up: true, gap: 0.18, gain: 0.12 });
arp(at(3), "i", { vowel: "ah", count: 4, up: false, gap: 0.20, gain: 0.10, pan: -0.35 });

// VERSE — the Feral take with kicks between her syllables. Offsets are
// the harvest's word boundaries relative to each dressed slice.
haloed("f-sitting-curled", at(4), { gain: 0.95, dly: 0.13 }, 0.18);
swell("f-sitting-curled", "5th", at(4), 3.89, 0.22);          // myself…
kick(at(4) + 1.42, 0.5); chop(0, at(4) + 1.53, 0.34, 0.3);    // sitting·KICK·ick
kick(at(4) + 3.14, 0.45); chop(1, at(4) + 3.25, 0.30, -0.3);  // up·KICK·cik

haloed("f-think-stone", at(6), { gain: 0.95, dly: 0.15 }, 0.18);
swell("f-think-stone", "3rd", at(6), 2.83, 0.22);             // stone…
kick(at(6) + 1.72, 0.5); chop(5, at(6) + 1.83, 0.32, 0.3);    // think·KICK·ink
arp(at(7), "III", { vowel: "oh", count: 5, up: true, gain: 0.13 });

haloed("f-waiting-patiently", at(8), { gain: 0.95, dly: 0.13 }, 0.18);
swell("f-waiting-patiently", "5th", at(8), 4.87, 0.20);       // patiently…
kick(at(8) + 1.60, 0.5); chop(2, at(8) + 1.71, 0.32, 0.3);    // just·KICK
kick(at(8) + 3.29, 0.55); chop(3, at(8) + 3.40, 0.34, -0.3);  // waiting·KICK·cik

haloed("f-for-time-to-pass", at(10.6), { gain: 0.95, dly: 0.17 }, 0.18);
swell("f-for-time-to-pass", "5th", at(10.6), 4.30, 0.22);     // pass…
kick(at(10.6) + 0.98, 0.5); chop(7, at(10.6) + 1.09, 0.30, 0.3);
kick(at(10.6) + 2.60, 0.45);
arp(at(11.3), "VII", { vowel: "ah", count: 5, up: false, gain: 0.13, pan: -0.35 });
for (let bar = 8; bar < 12; bar++) thump(at(bar), 0.6);
for (let bar = 8; bar < 12; bar++) { tick(at(bar, 2), 0.08, 0.3); tick(at(bar, 3.5), 0.06, -0.2); }

// BUILD — the two-step assembles under her last held word.
for (let bar = 12; bar < 16; bar++) {
  const t = at(bar), u = (bar - 12) / 4;
  kick(t, 0.55 + 0.3 * u);
  kick(t + 10 * STEP, 0.5 + 0.3 * u);
  if (bar >= 14) { snare(t + 4 * STEP, 0.5 + 0.3 * u); }
  snare(t + 12 * STEP, 0.45 + 0.35 * u);
  for (let s = 0; s < 16; s += 2) tick(t + s * STEP + jit(2), (0.05 + 0.04 * u) * VEL[s], s % 4 ? 0.28 : -0.22);
  if (bar >= 13) for (const s of [3, 11]) chop(bar + s, t + s * STEP, 0.26, s % 2 ? 0.3 : -0.3);
}
arp(at(13.5), "iv", { vowel: "oh", count: 6, up: true, gap: 0.13, gain: 0.14 });
arp(at(15.5), "VII", { vowel: "ah", count: 6, up: true, gap: 0.13, gain: 0.15 });
brush(at(16) - 0.42, { gain: 1, pan: 0.25 });

// DROP 1 — drum & bass under the "not again!" verse.
for (let bar = 16; bar < 26; bar++) dnbBar(bar, { hatG: 0.085, chops: true });
haloed("n-getting-curled", at(16), { gain: 0.92, dly: 0.14 }, 0.20);
haloed("n-stone-waiting", at(18), { gain: 0.92, dly: 0.14 }, 0.20);
swell("n-stone-waiting", "3rd", at(18), 0.76, 0.20);          // stone…
haloed("n-for-time-to-pass", at(21.4), { gain: 0.92, dly: 0.18 }, 0.20);
sung("f-pass", at(24.2), { gain: 0.34, pan: 0.4, dark: 0.45, dly: 0.5, side: 0.8, rvb: 0.5 });
arp(at(17.5), "i", { vowel: "oh", count: 6, up: true, gap: 0.13 });
arp(at(19.5), "VI", { vowel: "ah", count: 6, up: false, gap: 0.13 });
arp(at(21.5), "III", { vowel: "oh", count: 6, up: true, gap: 0.13 });
arp(at(23.5), "iv", { vowel: "ah", count: 7, up: false, gap: 0.13 });

// BREATHE — everything stops. The whole take, naked but haloed, in the
// long tail. The one interval is the faint 5th under the last "pass".
haloed("f-whole-line", at(26), { gain: 1.0, dly: 0.10, rvb: 0.32 }, 0.13,
  { attack: 0.6, rvb: 0.7 });
swell("f-whole-line", "5th", at(26), 22.79, 0.13, 0.8);
arp(at(30), "i", { vowel: "oh", count: 4, up: true, gap: 0.30, gain: 0.08, pan: 0.3 });
dust(at(28, 1.1), 0.013); dust(at(31, 2.8), 0.015);

// REBUILD — the kit re-forms while "pass" still rings.
brush(at(34) + 0.2, { gain: 0.9, pan: -0.25, dur: 1.2, rise: 0.7 });
for (let bar = 34; bar < 36; bar++) {
  const t = at(bar), u = (bar - 34) / 2;
  kick(t, 0.6 + 0.3 * u); kick(t + 10 * STEP, 0.55 + 0.3 * u);
  snare(t + 12 * STEP, 0.5 + 0.3 * u);
  for (let s = 0; s < 16; s += 2) tick(t + s * STEP, (0.06 + 0.03 * u) * VEL[s], 0.25);
  for (const s of [6, 14]) chop(bar + s, t + s * STEP, 0.28, s % 2 ? 0.3 : -0.3);
}
arp(at(34.5), "VI", { vowel: "ah", count: 7, up: true, gap: 0.12, gain: 0.16 });

// DROP 2 — fuller: the octave leap answering itself over the roll.
for (let bar = 36; bar < 46; bar++) dnbBar(bar, { hatG: 0.10, snareG: 0.85, roll: true });
haloed("f-of-a-stone", at(36), { gain: 1.0, dly: 0.20 }, 0.24);
swell("f-of-a-stone", "3rd", at(36), 1.13, 0.24);             // stone…
haloed("n-of-a-stone", at(38), { gain: 0.85, pan: 0.35, dly: 0.30 }, 0.22);
haloed("f-think-stone", at(40), { gain: 0.95, dly: 0.15 }, 0.24);
swell("f-think-stone", "3rd", at(40), 2.83, 0.24);
haloed("f-of-a-stone", at(42), { gain: 1.0, dly: 0.22 }, 0.24);
sung("f-i-think", at(43.5), { gain: 0.5, pan: -0.3, dark: 0.35, dly: 0.35 });
haloed("f-pass", at(44.5), { gain: 0.6, pan: 0.2, dly: 0.3 }, 0.2);
for (let bar = 36; bar < 46; bar += 2) {
  arp(at(bar + 0.5), degAt(bar), { vowel: bar % 4 ? "ah" : "oh", count: 7,
    up: (bar / 2) % 2 === 0, gap: 0.12, gain: 0.17 });
}

// OUT — the reverb and the hiss finish it; the harp dissolves.
halo("f-pass", at(46.5), 0.12, { attack: 0.8, rvb: 0.75 });
arp(at(47), "i", { vowel: "oh", count: 4, up: false, gap: 0.28, gain: 0.09 });
arp(at(49), "i", { vowel: "ah", count: 3, up: false, gap: 0.30, gain: 0.07, pan: -0.3 });
dust(at(48, 0.4), 0.012); dust(at(50, 2.1), 0.014);

if (missing.size) console.warn("  ! missing samples:", [...missing].join(", "));

// ── dub delay (v1's, unchanged) ───────────────────────────────────────
{
  const D = Math.round(0.75 * BEAT * SR);
  const FB = 0.38;
  const damp = 1 - Math.exp((-TAU * 2200) / SR);
  const hpRc = 1 / (TAU * 160), hpA = hpRc / (hpRc + 1 / SR);
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
    musicL[i] += hpL * 0.56;
    musicR[i] += hpR * 0.56;
  }
}

// ── the diffuse tail — a decorrelated Schroeder pair on the vox bus ───
// 4 combs per side (mutually prime-ish delays, R offset a few ms), each
// with a one-pole 3.4 kHz damp INSIDE the loop so the tail darkens as it
// decays; feedback set for RT60 ≈ 3.2 s; then two series allpasses per
// side for density. 40 ms pre-delay keeps the words intelligible; the
// return is high-passed at 180 Hz and lands on the vox bus, so the tail
// wears the voice's duck and makeup like everything else she does.
{
  const PRE = Math.round(0.040 * SR);
  const RT60 = 3.2;
  const CL = [2142, 2281, 2452, 2603];
  const CR = CL.map((d, k) => d + [29, 37, 23, 31][k]);
  const AL = [1067, 432], AR = [1080, 441], AG = 0.7;
  const dampK = 1 - Math.exp((-TAU * 3400) / SR);
  const mkComb = (d) => ({ buf: new Float32Array(d), i: 0, lp: 0, g: Math.pow(10, (-3 * d) / (RT60 * SR)) });
  const mkAp = (d) => ({ buf: new Float32Array(d), i: 0 });
  const combsL = CL.map(mkComb), combsR = CR.map(mkComb);
  const apsL = AL.map(mkAp), apsR = AR.map(mkAp);
  const hpRc = 1 / (TAU * 180), hpA = hpRc / (hpRc + 1 / SR);
  let hpL = 0, hpR = 0, pL = 0, pR = 0;
  const RET = 0.34;
  for (let i = 0; i < N; i++) {
    const x = i >= PRE ? rvbSend[i - PRE] * 0.25 : 0;
    let l = 0, r = 0;
    for (const c of combsL) {
      const y = c.buf[c.i];
      c.lp += dampK * (y - c.lp);
      c.buf[c.i] = x + c.lp * c.g;
      if (++c.i >= c.buf.length) c.i = 0;
      l += y;
    }
    for (const c of combsR) {
      const y = c.buf[c.i];
      c.lp += dampK * (y - c.lp);
      c.buf[c.i] = x + c.lp * c.g;
      if (++c.i >= c.buf.length) c.i = 0;
      r += y;
    }
    for (const a of apsL) {
      const y = a.buf[a.i];
      const v = l + y * AG;
      a.buf[a.i] = v;
      if (++a.i >= a.buf.length) a.i = 0;
      l = y - v * AG;
    }
    for (const a of apsR) {
      const y = a.buf[a.i];
      const v = r + y * AG;
      a.buf[a.i] = v;
      if (++a.i >= a.buf.length) a.i = 0;
      r = y - v * AG;
    }
    hpL = hpA * (hpL + l - pL); pL = l;
    hpR = hpA * (hpR + r - pR); pR = r;
    voxL[i] += hpL * RET;
    voxR[i] += hpR * RET;
  }
}

hissBed();

// ── the breath ────────────────────────────────────────────────────────
// The thump breathes the bed like v1; the DnB kick ducks it harder and
// faster, which is what keeps the pads out of the kick's way at 160.
const bedEnv = buildEnv([
  ...thumps.map((t) => ({ t, depth: 0.18, atk: 0.009, rel: 0.34 })),
  ...dnbKicks.map((t) => ({ t, depth: 0.30, atk: 0.009, rel: 0.20 })),
]);

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
  writeStereo(resolve(dir, "v3-vox.wav"), ...mk(voxL, voxR, voxDuck, VOXG));
  writeStereo(resolve(dir, "v3-music.wav"), ...mk(musicL, musicR, bedEnv, 1));
  writeStereo(resolve(dir, "v3-drums.wav"), ...mk(drumsL, drumsR, null, 1));
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
    const s = sideB[i] * be + sideV[i] * Math.pow(be, 0.25);
    hp = hpA * (hp + s - prev); prev = s;
    lp += lpK * (hp - lp);
    const bar = i / SR / BAR;
    const target =
      bar < S.verse[0] ? 0.92 :
      bar < S.build[0] ? 0.62 :
      bar < S.drop1[0] ? 0.55 :
      bar < S.breathe[0] ? 0.48 :
      bar < S.rebuild[0] ? 0.85 :
      bar < S.drop2[0] ? 0.55 :
      bar < S.out[0] ? 0.48 : 0.92;
    send += 0.00004 * (target - send);
    sideOut[i] = lp * send;
  }
}

// ── sum ───────────────────────────────────────────────────────────────
let peak = 0;
const L = new Float32Array(N), R = new Float32Array(N);
for (let i = 0; i < N; i++) {
  const be = bedEnv[i];
  const dv = Math.pow(be, 0.25);
  const fadeIn = Math.min(1, i / (0.014 * SR));
  const fadeOut = Math.min(1, (N - 1 - i) / (2.6 * SR));
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

const outWav = resolve(OUT, "loner-remix-v3-full.wav");
writeStereo(outWav, L, R);

// ── the receipt ───────────────────────────────────────────────────────
EVENTS.sort((a, b) => a.t - b.t || (a.bus < b.bus ? -1 : 1));
const voiceCounts = {};
for (const e of EVENTS) voiceCounts[e.voice] = (voiceCounts[e.voice] ?? 0) + 1;
const clock = (sec) => `${Math.floor(sec / 60)}:${String(Math.floor(sec % 60)).padStart(2, "0")}`;
console.log(`  ${EVENTS.length} events · ${Object.entries(voiceCounts).map(([k, v]) => `${k} ${v}`).join(" · ")}`);
writeFileSync(resolve(OUT, "loner-remix-v3.events.json"), JSON.stringify({
  schema: "aesthetic.computer/pop-events/v1",
  track: "whistlegraph loner --- remix (v3, angelic drum & bass)",
  renderer: "pop/loner/bin/render3.mjs",
  source: {
    work: "lonr — Loner (Whistlegraph), composed by Camille Klein (@cksuperstore)",
    lyric: "sitting curled up in myself, i think of a stone, just waiting very patiently for time to pass",
    soloRule: "no alex voice — this track is all solo camille; the ensemble takes and every spoken aside are gone",
    takes: {
      f: "7108062006980201771 — Ten Whistlegraphs / Feral File, the spine",
      n: "7021262898479549702 — the sung answer from the 13.8M 'not again!' post",
    },
    tuning: "A# minor at the take's own pitch — tonic 237 Hz, ~+30 cents over A440; the grid lives in bin/halo.py",
    aesthetivox: "every vocal object is a WORLD render from bin/halo.py — leads (air+breath), octave/3rd/5th halos (vowels-only), the arp-note harp, and the chops are cut from those renders, never raw takes",
  },
  tempoBPM: BPM, drumsFeelBPM: BPM * 2, bars: BARS, seconds: +(BARS * BAR).toFixed(2),
  angelicChain: {
    halo: "same-slice octave pair, f0×2 with envelope untouched, +6/−7 cents, +28/+41 ms, darker (5.5 kHz tilt), breathier ×1.5, vowels-only, pan ±0.55, reverb send 0.55–0.75",
    swells: "diatonic 3rd/5th renders of the held phrase-ends, 0.5–0.8 s attacks, offset into the vowels-only render so only the held word sounds",
    air: "+2.5 dB shelf-equivalent above 8 kHz in cheaptrick envelope (leads only)",
    breath: "aperiodicity +0.14 (halos +0.21) above 8 kHz, ramped in 150 ms into each voiced run — held vowels only",
    harp: "arp-oh/arp-ah WORLD notes (flat grid tones st+12…+27, 12¢/5.2 Hz vibrato), chord-tone runs of 4–7 notes at 0.12–0.30 s",
    reverb: "Schroeder pair on the vox bus: combs L 2142/2281/2452/2603 smp (R +29/37/23/31), RT60 3.2 s, 3.4 kHz in-loop damp, allpasses 1067/432 (R 1080/441) g 0.7, pre-delay 40 ms, HP 180 Hz, return 0.34",
  },
  drumDesign: {
    grid: "16ths of the 80 BPM bar = the 8th-note grid of 160 — the drums think double-time",
    twoStep: "kick steps 0·10 (+6 every 4th bar), snare 4·12, ghost 7 on odd bars, snare roll into every 4th bar of drop 2",
    hats: "velocity-shaped 16th ticks (darkened hatC), tambo accent every other bar",
    chops: "8 fragments of Camille's WORLD leads — 'ick', reversed 'cik', 'ting', 'ss' — on steps 3/6/11/14, gated (bar·7+step)%3",
    syllableKicks: "verse kicks placed at the harvest's word boundaries inside each phrase: sitting·KICK·ick, up·KICK·cik, think·KICK·ink, waiting·KICK·cik",
    sub: "sine-bump bass, 12 ms attack, root steps 0/6/10 + fifth pickup on step 14 of odd bars",
  },
  sections: Object.entries(S).map(([k, [a, b]]) => ({
    key: k, act: ACTS[k], bars: [a, b],
    start: +at(a).toFixed(3), end: +at(b).toFixed(3),
    clock: [clock(at(a)), clock(at(b))],
  })),
  buses: {
    music: "pads (wow), sub bass, dub-delay return, hiss — ducks 0.18 to the thump, 0.30 to the kick",
    drums: "kick, snare, ticks, brushes, vocal chops — never ducks",
    vox: "leads, halos, swells, harp, Schroeder return — light duck (bedEnv^0.25), +3 dB makeup",
  },
  events: EVENTS,
}, null, 1));
console.log(`✓ ${outWav}`);
console.log(`  ${(N / SR).toFixed(1)} s scored · master with: bash pop/loner/bin/cut-v3.sh`);
