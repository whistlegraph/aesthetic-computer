#!/usr/bin/env node
// render2.mjs — "whistlegraph loner --- remix (v2, aesthetivox)"
//
// v1 with one rule applied: VOCALS NEVER SKIP THE AESTHETIVOX. ,
// on the cult lane, hearing exposed unprocessed takes: too raw. So every
// vocal here — sung phrases, the unaccompanied BREATHE take, the three
// spoken asides — is a WORLD render from bin/aesthetivox.py: Camille's
// rubato kept (gentle f0 pull toward the lane's 237 Hz grid, strength
// 0.7, correction smoothed so slides stay slides), the spoken lines
// range-compressed, dropped a semitone and darkened. The bank below
// loads vox/, not samples/, and the score is otherwise v1's.
//
// Camille's `loner` (composed by @cksuperstore, 2021; nineteen posts carry
// the work, 13.8M views on the biggest) is one sung sentence:
//
//     sitting curled up in myself, i think of a stone,
//     just waiting very patiently for time to pass
//
// The cult remix answered eight seconds of chant with a club record. Loner
// is the opposite material — a slow, private melody sung alone at a desk —
// so v1 is a BEDROOM BALLAD: the voice close and forward, tape-warm pads,
// soft-attack bass, brushed-adjacent percussion that mostly isn't there,
// and a dotted-eighth delay doing the work a reverb would. Tenderness over
// punch: no thick kick, no signal layer, no drop of any kind.
//
// ══ WHAT THE SOURCE GAVE US ════════════════════════════════════════════
//
// The Ten Whistlegraphs / Feral File take (7108062006980201771) is the
// spine — the whole lyric, one clean unaccompanied voice, ~80 BPM by
// beat_track, and (per-word librosa.pyin, receipt in harvest.json) an
// A# minor frame sitting about +30 cents sharp of A440: tonic hits at
// ~237 Hz against A#3's 233. So the band tunes to Camille instead of
// dragging her to concert pitch — every synth pitch below derives from
// TONIC = 237 Hz, and A440 is never consulted.
//
// The melody, snapped to that frame (degree above the tonic):
//
//     sitting F4(5)  curled C#4(b3)  up in A#3(1)  myself D#4(4)
//     i think G#3(b7, below)  of a A#4(octave)  stone ~E4 (a blue note
//     that drifts between 4 and #4 — the accompaniment keeps a VI chord
//     under it, where the drift reads as 6th/b7 colour, and stays out of
//     its way over i)
//     just C4(2)  waiting C#4(b3)  very A#3(1)  patiently D#4(4)
//     for D#4(4)  time F4(5)  to pass C#4(b3)
//
// Every accompaniment chord was chosen so the measured word pitches land
// as chord colour: i / VI / III / VII / iv in A# natural minor — Bbm, Gb,
// Db (the maj7 under "just waiting" is the lyric's own C4), Ab, Ebm.
//
// Three other takes join it (see bin/slice.mjs): the 13.8M "not again!"
// take opens with a SPOKEN "Camille, are you doing emo whistlegraphs
// again?" and closes "i knew it" — the record keeps both — and the origin
// take opens "Here's a whistlegraph by Camille called loner. Ready?",
// which is the tape leader this ballad starts from.
//
// ══ FORM — 56 bars at 80 BPM, 2:48 + tail ══════════════════════════════
//
//   TAPE     0:00  bars 0–4    hiss, a low drone breathing in, and the
//                              spoken introduction: "…called loner. ready?"
//   VERSE 1  0:12  bars 4–14   the Feral File take, phrase by phrase at
//                              its own rubato. Bass and pad only; a hat
//                              tick doesn't dare enter until bar 9.
//   DRIFT    0:42  bars 14–18  instrumental breath. "stone" comes back as
//                              a dark echo in the delay; a music-box line
//                              answers.
//   VERSE 2  0:54  bars 18–28  the "not again!" take sings it again —
//                              same sentence, another day — over a soft
//                              thump on the downbeats.
//   LIFT     1:24  bars 28–36  the middle: both takes' "of a stone" —
//                              the octave leap, the one loud feeling in
//                              the lyric — answer each other over the
//                              warmest pads, an octave shimmer on top.
//   VERSE 3  1:48  bars 36–44  the Feral take's back half returns and
//                              the room thins out under it, one element
//                              per bar.
//   BREATHE  2:12  bars 44–53  unaccompanied. The whole original take,
//                              alone with the hiss, exactly as sung. At
//                              the end, from the other take, spoken:
//                              "i knew it."
//   OUT      2:39  bars 53–56  the delay finishes the sentence.
//
// Mixing keeps every cult-lane rule that still applies: 10 ms raised-
// cosine tails on every voice, ducks that ramp rather than step (and here
// the duck is depth 0.18 — a breath, nothing pumps), sine-bump bass, no
// master tanh, clean sum with ONE linear trim, mono-safe equal-power pans
// with a band-limited antisymmetric side return. Mastering (bin/cut-v2.sh)
// targets −16 LUFS, not −14: a ballad keeps its dynamics.
//
//   node pop/loner/bin/render2.mjs           # → out/loner-remix-v2-full.wav
//   node pop/loner/bin/render2.mjs --stems   # + per-bus stems
//   MUTE=pad node …  /  ONLY=vox node …      # subtract or solo voices

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
const BPM = 80;                       // the Feral File take's own tempo
const BEAT = 60 / BPM;                // 0.75 s
const BAR = 4 * BEAT;                 // 3.0 s
const BARS = 56;                      // 168 s = 2:48
const TAU = Math.PI * 2;
const N = Math.round((BARS * BAR + 3.2) * SR);

// The take's own tuning: tonic A#3 measured at ~237 Hz (+30 cents over
// A440's 233.08). st 0 = the tonic; every pitch is semitones from there.
const TONIC = 237.0;
const hzOf = (st) => TONIC * Math.pow(2, st / 12);

// ── buses ─────────────────────────────────────────────────────────────
// Three of cult's five: music (pads, bass, music box, delay return, hiss),
// drums (thump, hats, brushes), vox (the takes). No tube, no signal —
// a ballad has nothing to pump and nothing to dial.
const musicL = new Float32Array(N), musicR = new Float32Array(N);
const drumsL = new Float32Array(N), drumsR = new Float32Array(N);
const voxL = new Float32Array(N), voxR = new Float32Array(N);
const sideB = new Float32Array(N);
const sideV = new Float32Array(N);
const dlySend = new Float32Array(N);

const VOXG = 1.42;                    // the voice rides proud (v2's measurement)
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
{
  const d = resolve(LANE, "vox");
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
    if (bus === "vox") sideV[i] += s; else sideB[i] += s;
  }
}

// ── sidechain ─────────────────────────────────────────────────────────
// One envelope, from the thumps, depth 0.18. In a ballad the duck is a
// breath the bed takes when the pulse lands — never audible as pumping.
const thumps = [];
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

// ── the soft thump ────────────────────────────────────────────────────
// cult's kick with everything punchy removed: a slower 82→46 Hz sweep, a
// single soft envelope, gentle drive (1.3 — warmth, not cut), no click
// transient at all, and a felt-mallet touch of 190 Hz body. A heartbeat
// heard through a wall.
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

// ── brushes and ticks ─────────────────────────────────────────────────
let nseed = 20210725;                              // the "loner by @cksuperstore" post date
const nrnd = () => {
  nseed ^= nseed << 13; nseed ^= nseed >>> 17; nseed ^= nseed << 5;
  return ((nseed >>> 0) / 4294967296) * 2 - 1;
};
// A brush swell: band-passed noise that arrives over ~0.4 s INTO a
// downbeat and leaves just after it — the gesture of a brush circling
// onto a snare head, without a snare hit anywhere.
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
    const band = bp - bp2;                          // ~380–1500 Hz swish
    const env = u < rise ? smooth(u / rise) : Math.max(0, 1 - (u - rise) / (dur - rise));
    emit("drums", i0 + i, band * env * env * 0.16 * gain * tailFade(i, n), pan, sp, 0.5);
  }
}
// The closed hat from the shared demo bank, darkened to a tick.
function tick(t, gain = 0.12, pan = 0.3) {
  shot("hatC", t, { bus: "drums", gain, pan, dark: 0.55, side: 0.4, evVoice: "tick" });
}

// ── tape ──────────────────────────────────────────────────────────────
// The hiss is a real mix element, not set dressing: band-limited noise
// whose level breathes with the sections (loudest when the music is gone),
// plus a few dust ticks in the empty acts. This is what "recorded in a
// bedroom" sounds like on the medium the melody was born on.
function hissBed() {
  if (!allow("hiss")) return;
  let lp = 0, hp = 0, prev = 0, lvl = 0.010;
  const kLp = 1 - Math.exp((-TAU * 5200) / SR);
  const hpRc = 1 / (TAU * 320), hpA = hpRc / (hpRc + 1 / SR);
  for (let i = 0; i < N; i++) {
    const bar = i / SR / BAR;
    const target =
      bar < 4 ? 0.012 : bar < 44 ? 0.0045 : bar < 53 ? 0.010 : 0.013;
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

// ── the pad, with wow ─────────────────────────────────────────────────
// Detuned sine pairs with a one-pole lid and a slow shared WOW — ±4 cents
// at 0.38 Hz, the pitch breathing of a worn cassette transport. The wow is
// one LFO for all pad voices so the whole bed leans together, the way tape
// actually does; each voice adds a still slower personal drift so unisons
// never freeze.
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

// ── soft-attack bass ──────────────────────────────────────────────────
// cult's sine-bump bass with the attack opened to 45 ms, so every note
// arrives the way a finger lands on an upright's string, not a pick.
function bass(t, st, dur, gain = 1) {
  if (!allow("bass")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "bass", bus: "music", st, dur: +dur.toFixed(3), gain: +gain.toFixed(3) });
  const n = Math.round((dur + 0.16) * SR), i0 = Math.round(t * SR);
  const f = hzOf(st);
  let p1 = 0, p2 = 0, p3 = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
    let env = smooth(u / 0.045);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.16);
    const s = Math.sin(p1) + 0.48 * Math.sin(p2) + 0.07 * Math.sin(p3);
    lp += 0.42 * (s - lp);
    emit("music", i0 + i, lp * 0.38 * env * gain * tailFade(i, n), 0, null, 0);
  }
}

// ── the music box ─────────────────────────────────────────────────────
// A small answering voice for the instrumental acts: one soft partial-lite
// sine per note, longish delay send, panned like something across the
// room. It plays chord tones only — it comments, it never leads.
function box(t, st, dur = 0.55, gain = 0.10, pan = 0.4) {
  if (!allow("box")) return;
  EVENTS.push({ t: +t.toFixed(4), voice: "box", bus: "music", st, dur, gain: +gain.toFixed(3), pan });
  const n = Math.round((dur + 0.5) * SR), i0 = Math.round(t * SR);
  const sp = spatial(pan * 1.3);
  const f = hzOf(st);
  let p1 = 0, p2 = 0, lp = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR;
    p1 += (TAU * f) / SR; p2 += (TAU * f * 3.01) / SR;
    let env = smooth(u / 0.030) * Math.exp(-u * 2.4);
    if (u > dur) env *= Math.max(0, 1 - (u - dur) / 0.5);
    const s = Math.sin(p1) + 0.10 * Math.sin(p2) * Math.exp(-u * 9);
    lp += 0.30 * (s - lp);
    emit("music", i0 + i, lp * env * gain * tailFade(i, n), pan, sp, 0.8, 0.55);
  }
}

// ── the one-shot player ───────────────────────────────────────────────
// cult's varispeed player, wiggle kept but off by default — one singer,
// nothing to beat against. `dark` is the one-pole lid; the spoken lines
// wear a little of it so they sit behind the sung ones.
function shot(name, t, {
  gain = 1, pan = 0, semis = 0, bus = "vox", side = 0.35, dark = 0,
  dur = null, dly = 0, off = 0, evVoice = null, who = null,
} = {}) {
  const s = BANK[name];
  if (!s) { missing.add(name); return; }
  const voice = evVoice ?? (/^(f|n|s|o)-/.test(name) ? "take" : name);
  if (!allow(voice)) return;
  const step = Math.pow(2, semis / 12);
  const start = Math.max(0, Math.min(s.length - 2, Math.round(off * SR)));
  const avail = Math.floor((s.length - 2 - start) / step);
  const n = dur ? Math.min(avail, Math.round(dur * SR)) : avail;
  if (n <= 4) return;
  const i0 = Math.round(t * SR);
  EVENTS.push({ t: +t.toFixed(4), voice, bus, sample: name,
    dur: +(n / SR).toFixed(3), gain: +gain.toFixed(3), pan: +pan.toFixed(2),
    ...(who ? { who } : {}), ...(semis ? { semis } : {}) });
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
const sung = (name, t, o = {}) =>
  shot(name, t, { bus: "vox", side: 0.5, who: "camille", ...o });
const spoken = (name, t, o = {}) =>
  shot(name, t, { bus: "vox", side: 0.4, dark: 0.22, evVoice: "spoken", ...o });

// ── harmony ───────────────────────────────────────────────────────────
// A# natural minor in the take's own tuning. Chords named by degree; the
// verse row was derived from the melody (header) — every measured word
// pitch is a chord tone or a tension the chord asked for.
const CH = {
  i:   { root: 0,  tones: [0, 3, 7] },      // Bbm
  III: { root: 3,  tones: [3, 7, 10] },     // Db
  iv:  { root: 5,  tones: [5, 8, 12] },     // Ebm
  VI:  { root: -4, tones: [-4, 0, 3] },     // Gb, voiced under the tonic
  VII: { root: -2, tones: [-2, 2, 5] },     // Ab, likewise
};
const VERSE1 = ["i", "i", "VI", "VI", "III", "III", "VII", "VII", "iv", "VII"];
const VERSE2 = ["i", "i", "VI", "VI", "III", "III", "iv", "iv", "VII", "VII"];
const LIFT   = ["VI", "VI", "VII", "VII", "i", "i", "VI", "VII"];
const VERSE3 = ["III", "III", "VII", "VII", "iv", "iv", "i", "i"];

// ── form ──────────────────────────────────────────────────────────────
const S = {
  tape:    [0, 4],
  verse1:  [4, 14],
  drift:   [14, 18],
  verse2:  [18, 28],
  lift:    [28, 36],
  verse3:  [36, 44],
  breathe: [44, 53],
  out:     [53, 56],
};
const ACTS = {
  tape: "I · TAPE", verse1: "II · VERSE", drift: "III · DRIFT",
  verse2: "IV · VERSE AGAIN", lift: "V · OF A STONE", verse3: "VI · THINNING",
  breathe: "VII · BREATHE", out: "VIII · OUT",
};

// chord at bar, from the section rows (pedal i elsewhere)
function chordAt(bar) {
  if (bar >= S.verse1[0] && bar < S.verse1[1]) return CH[VERSE1[bar - S.verse1[0]]];
  if (bar >= S.drift[0] && bar < S.drift[1]) return CH[["VI", "VI", "III", "III"][bar - S.drift[0]]];
  if (bar >= S.verse2[0] && bar < S.verse2[1]) return CH[VERSE2[bar - S.verse2[0]]];
  if (bar >= S.lift[0] && bar < S.lift[1]) return CH[LIFT[bar - S.lift[0]]];
  if (bar >= S.verse3[0] && bar < S.verse3[1]) return CH[VERSE3[bar - S.verse3[0]]];
  return CH.i;
}
const inS = (bar, k) => bar >= S[k][0] && bar < S[k][1];

// ── score ─────────────────────────────────────────────────────────────

// THE BED. One pad chord per two bars (the harmonic rhythm), voiced low;
// a second, higher voicing joins from verse 2; a shimmer an octave up
// lives only in the lift. Everything stops at bar 44 and the last chord's
// 0.9 s release is the only accompaniment BREATHE gets.
for (let bar = 0; bar < S.breathe[0]; bar += 2) {
  const ch = chordAt(bar);
  const t = at(bar) + 0.02;
  const dur = 2 * BAR - 0.1;
  const low = [ch.root - 24, ch.root - 12, ch.tones[1] - 12];
  const g =
    inS(bar, "tape") ? 0.13 :
    inS(bar, "verse1") ? 0.16 :
    inS(bar, "drift") ? 0.18 :
    inS(bar, "verse2") ? 0.18 :
    inS(bar, "lift") ? 0.20 : 0.16;
  pad(t, low, dur, g, { attack: inS(bar, "tape") ? 2.6 : 1.2, side: 0.55 });
  if (bar >= S.verse2[0])
    pad(t + 0.15, ch.tones.map((s) => s), dur - 0.3, g * 0.72,
      { attack: 1.6, pan: 0.18, side: 0.7, dly: 0.10, dark: 0.36 });
  if (inS(bar, "lift"))
    pad(t + 0.4, ch.tones.map((s) => s + 12), dur - 0.8, 0.10,
      { attack: 2.2, pan: -0.3, side: 0.85, dly: 0.22, dark: 0.20 });
}

// THE BASS. One root per bar, soft, whole. From verse 2 it walks to the
// fifth on the fourth beat of every second bar — the entire bassline
// vocabulary of this record.
for (let bar = S.verse1[0]; bar < S.breathe[0]; bar++) {
  const ch = chordAt(bar);
  const g = inS(bar, "verse1") || inS(bar, "verse3") ? 0.56 : inS(bar, "drift") ? 0.48 : 0.66;
  if (inS(bar, "verse3") && bar >= 42) {                // the thinning
    if (bar === 42) bass(at(bar), ch.root - 24, 2 * BAR - 0.2, 0.45);
    continue;
  }
  bass(at(bar), ch.root - 24, BAR - 0.12, g);
  if (bar >= S.verse2[0] && bar % 2 === 1 && !inS(bar, "verse3"))
    bass(at(bar, 3), ch.root - 24 + 7, BEAT - 0.05, g * 0.55);
}

// THE PULSE. Nothing at all until verse 2. Then a thump on the downbeat,
// a tick keeping eighth-note time nobody asked it to keep, and a brush
// circling into each four-bar corner. The lift gets the only beat-3
// thumps in the record. Verse 3 sheds one element per bar.
for (let bar = 9; bar < S.verse1[1]; bar++) {           // late verse-1 ticks
  tick(at(bar, 2), 0.09, 0.3);
  tick(at(bar, 3.5), 0.065, -0.2);
}
for (let bar = S.verse2[0]; bar < S.verse3[1]; bar++) {
  const inLift = inS(bar, "lift");
  const inV3 = inS(bar, "verse3");
  if (inV3 && bar >= 41) break;                         // pulse is gone by 41
  if (!inV3 || bar % 2 === 0) thump(at(bar), inLift ? 0.9 : 0.78);
  if (inLift && bar % 2 === 1) thump(at(bar, 2), 0.55);
  if (!inV3 || bar < 39) {
    tick(at(bar, 1.5), 0.10, 0.32);
    tick(at(bar, 2), 0.075, -0.22);
    tick(at(bar, 3.5), 0.08, 0.26);
  }
}
for (const bar of [20, 24, 28, 32, 36]) brush(at(bar) - 0.42, { gain: 1, pan: 0.25 });

// TAPE. The drone is already breathing (bar 0 pad above); the spoken
// introduction arrives at bar 2, half behind the lid, and two dust ticks
// place the medium before any music does.
spoken("o-heres-loner", at(2), { gain: 0.60, pan: 0.06, dly: 0.16, who: "jeffrey" });
dust(at(0, 1.2)); dust(at(1, 2.6), 0.014); dust(at(3, 0.7), 0.012);

// VERSE 1 — the Feral File take at its own rubato, each phrase placed on
// the chord the melody was measured against (header table).
sung("f-sitting-curled", at(4), { gain: 0.95, dly: 0.13 });
sung("f-think-stone", at(6), { gain: 0.95, dly: 0.15 });
sung("f-waiting-patiently", at(8), { gain: 0.95, dly: 0.13 });
sung("f-for-time-to-pass", at(10.67), { gain: 0.95, dly: 0.17 });

// DRIFT — "stone" comes back from across the room, and the music box
// answers with the chord tones the voice just left behind.
sung("f-stone", at(15.4), { gain: 0.34, pan: -0.45, dark: 0.5, dly: 0.5, side: 0.8 });
for (const [b, st] of [[14, 15], [14.375, 12], [14.75, 8], [16, 14], [16.375, 10], [16.75, 7], [17.5, 3]])
  box(at(b), st, 0.55, 0.10, b % 1 ? -0.4 : 0.4);

// VERSE 2 — the "not again!" take sings the same sentence over the pulse.
// Its spoken question opens the act, tucked low: the joke the whole
// record is answering.
spoken("n-emo-again", at(17.4), { gain: 0.42, pan: -0.3, dly: 0.3, who: "jeffrey" });
sung("n-getting-curled", at(18), { gain: 0.92, dly: 0.14 });
sung("n-stone-waiting", at(20), { gain: 0.92, dly: 0.14 });
sung("n-for-time-to-pass", at(23.4), { gain: 0.92, dly: 0.18 });
sung("f-pass", at(26.2), { gain: 0.30, pan: 0.4, dark: 0.45, dly: 0.5, side: 0.8 });

// LIFT — both takes' octave leap, answering each other; the one place
// the record raises its voice, and it still never gets loud.
sung("f-of-a-stone", at(28), { gain: 1.0, dly: 0.20 });
sung("n-of-a-stone", at(30), { gain: 0.85, pan: 0.35, dly: 0.30 });
sung("f-i-think", at(31.5), { gain: 0.5, pan: -0.3, dark: 0.35, dly: 0.35 });
sung("f-of-a-stone", at(32), { gain: 1.0, dly: 0.22 });
sung("n-of-a-stone", at(34), { gain: 0.6, pan: 0.4, dark: 0.4, dly: 0.45 });
for (const [b, st] of [[29.5, 19], [33.5, 19], [35, 17], [35.5, 14]])
  box(at(b), st, 0.5, 0.085, -0.45);

// VERSE 3 — the back half of the sentence returns while the room empties.
sung("f-waiting-patiently", at(36), { gain: 0.95, dly: 0.13 });
sung("f-for-time-to-pass", at(39), { gain: 0.95, dly: 0.16 });

// BREATHE — the whole original take, alone. The pads stopped at bar 44;
// the hiss comes back up; nothing else. Then, spoken, the button.
sung("f-whole-line", at(44), { gain: 1.0, dly: 0.10 });
dust(at(46, 1.1), 0.013); dust(at(49, 2.8), 0.015); dust(at(52, 0.4), 0.012);
spoken("n-i-knew-it", at(52.45), { gain: 0.52, pan: 0.1, dly: 0.24, who: "jeffrey" });

// OUT — the delay and the hiss finish it. One last low tonic, far away.
pad(at(53), [-24, -12, -5], 2.4 * BAR, 0.075, { attack: 3.0, side: 0.5 });
box(at(54), 3, 0.8, 0.06, 0.3);
box(at(54.5), 0, 1.2, 0.055, -0.3);

if (missing.size) console.warn("  ! missing samples:", [...missing].join(", "));

// ── generous space: the dub delay ─────────────────────────────────────
// Dotted eighth at 80 BPM = 0.5625 s, damped at 2.2 kHz in the loop,
// high-passed at 160 on the return. The ballad's reverb, as in cult v2 —
// but the feedback is lower and the return louder: fewer, softer repeats,
// closer to the ear.
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

hissBed();

// ── the breath ────────────────────────────────────────────────────────
const bedEnv = buildEnv(thumps.map((t) => ({ t, depth: 0.18, atk: 0.009, rel: 0.34 })));

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
  writeStereo(resolve(dir, "v2-vox.wav"), ...mk(voxL, voxR, voxDuck, VOXG));
  writeStereo(resolve(dir, "v2-music.wav"), ...mk(musicL, musicR, bedEnv, 1));
  writeStereo(resolve(dir, "v2-drums.wav"), ...mk(drumsL, drumsR, null, 1));
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
      bar < S.verse1[0] ? 0.92 :
      bar < S.drift[0] ? 0.62 :
      bar < S.verse2[0] ? 0.85 :
      bar < S.lift[0] ? 0.55 :
      bar < S.verse3[0] ? 0.80 :
      bar < S.breathe[0] ? 0.60 :
      bar < S.out[0] ? 0.75 : 0.92;
    send += 0.00004 * (target - send);
    sideOut[i] = lp * send;
  }
}

// ── sum ───────────────────────────────────────────────────────────────
// Clean: breathe, fade, measure, ONE linear trim. No master tanh anywhere.
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

const outWav = resolve(OUT, "loner-remix-v2-full.wav");
writeStereo(outWav, L, R);

// ── the receipt ───────────────────────────────────────────────────────
EVENTS.sort((a, b) => a.t - b.t || (a.bus < b.bus ? -1 : 1));
const voiceCounts = {};
for (const e of EVENTS) voiceCounts[e.voice] = (voiceCounts[e.voice] ?? 0) + 1;
const clock = (sec) => `${Math.floor(sec / 60)}:${String(Math.floor(sec % 60)).padStart(2, "0")}`;
console.log(`  ${EVENTS.length} events · ${Object.entries(voiceCounts).map(([k, v]) => `${k} ${v}`).join(" · ")}`);
writeFileSync(resolve(OUT, "loner-remix-v2.events.json"), JSON.stringify({
  schema: "aesthetic.computer/pop-events/v1",
  track: "whistlegraph loner --- remix (v2, aesthetivox)",
  renderer: "pop/loner/bin/render2.mjs",
  source: {
    work: "lonr — Loner (Whistlegraph), composed by Camille Klein (@cksuperstore)",
    lyric: "sitting curled up in myself, i think of a stone, just waiting very patiently for time to pass",
    primaryTake: "https://assets.aesthetic.computer/whistlegraph/index/posts/7108062006980201771.mp4",
    takes: {
      f: "7108062006980201771 — Ten Whistlegraphs / Feral File, the spine",
      n: "7021262898479549702 — 13.8M 'not again!', the second verse and the button",
      o: "6988619239657622790 — the origin take, the spoken introduction",
      s: "6988954628167585030 — solo lower-register take (sliced, unused in v1)",
    },
    tuning: "A# minor at the take's own pitch — tonic 237 Hz, ~+30 cents over A440 (the aesthetivox grid carries the +30c; the band's TONIC is untouched)",
    aesthetivox: "every vocal is a WORLD render (bin/aesthetivox.py, receipt vox/.manifest.json): sung takes pulled toward the 237 Hz A#-minor grid at strength 0.7, spoken asides range-compressed ^0.55, -1 semitone, darkened — no raw exposed takes",
  },
  tempoBPM: BPM, bars: BARS, seconds: +(BARS * BAR).toFixed(2),
  harmony: {
    key: "A# natural minor (tonic 237 Hz)",
    chords: "i / III / iv / VI / VII — chosen so every measured word pitch lands as chord colour",
    verseRow: VERSE1, verse2Row: VERSE2, liftRow: LIFT, verse3Row: VERSE3,
  },
  sections: Object.entries(S).map(([k, [a, b]]) => ({
    key: k, act: ACTS[k], bars: [a, b],
    start: +at(a).toFixed(3), end: +at(b).toFixed(3),
    clock: [clock(at(a)), clock(at(b))],
  })),
  buses: {
    music: "pads (with tape wow), bass, music box, dub-delay return, hiss — breathes with the thump (depth 0.18)",
    drums: "thump, ticks, brush swells — never ducks",
    vox: "the four takes — light duck (bedEnv^0.25), +3 dB makeup, close and forward",
  },
  events: EVENTS,
}, null, 1));
console.log(`✓ ${outWav}`);
console.log(`  ${(N / SR).toFixed(1)} s scored · master with: bash pop/loner/bin/cut-v2.sh`);
