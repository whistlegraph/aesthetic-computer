#!/usr/bin/env node
// club.mjs — the hum melody, evolved into a club track.
//
// Takes the same 16-note D-minor melody from voice-takes/manifest.json
// (the RFA sound test) and rebuilds it as a ~2:20 four-on-the-floor
// club mix at 128 BPM: intro → break → build → drop ×2 → outro.
//
// Sound design rules (jas):
//   • percussion is AC's own drumtrack instrument — playPercussion()
//     from lib/percussion.mjs rendered through the bus.mjs synth, the
//     same TR-808/909 voices the notepat drum row plays
//   • the little "trackpad edge clicks" (the g# woodblock) are a
//     featured tick layer running the whole track
//   • the kick is ALWAYS present — it ramps in slowly across the
//     intro, stays through the breakdowns, never hard-drops out
//   • the build/drop noise is a chromatic LADDER — separated
//     bandpassed-noise steps walking up semitone by semitone, and the
//     ticks + accents organize around that grid
//   • no saws — every pitched voice is a harmonized SINE stack,
//     harmony voices fade in gradually across the drops
//   • click-free — raised-cosine attack/release on everything
//   • no clipping on bass or perc — clean gain staging, peak
//     normalize only, no tanh on the master
//   • spatialized like Special Sign — per-voice equal-power pan with
//     slowly drifting azimuth, a slow whole-room rotation on the
//     music bus, and a mono-safe L=−R wet return
//
// Voice takes (voice-takes/<id>.wav) still land in the breakdowns,
// same as render.mjs — the club cut becomes "real" one sung note at
// a time too.
//
// Usage: node pop/hum/bin/club.mjs [--play] [--bpm 128]
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { playPercussion } from "../../../system/public/aesthetic.computer/lib/percussion.mjs";
import { makeBufferSynth } from "../../dance/synths/bus.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const TAKES = resolve(LANE, "voice-takes");
const OUTDIR = resolve(LANE, "out");
const PLAY = process.argv.includes("--play");
const bpmIdx = process.argv.indexOf("--bpm");
const BPM = bpmIdx >= 0 ? Number(process.argv[bpmIdx + 1]) : 128;

const SR = 48_000;
const TAU = Math.PI * 2;
const SPB = 60 / BPM;
const BAR = SPB * 4;
const m2f = (m) => 440 * Math.pow(2, (m - 69) / 12);

// ── melody — pulled from the manifest, retimed to the club grid ───────
// The manifest is phrased in 2-beat notes at 182; halving the grid puts
// each note on one club beat → a 20-beat phrase (5 bars) that breathes.
const manifest = JSON.parse(readFileSync(resolve(TAKES, "manifest.json"), "utf8"));
const PHRASE = manifest.notes.map((n) => ({
  id: n.id,
  beat: (n.bar * 4 + n.beat) / 2,
  durBeats: n.durBeats / 2,
  midi: n.midi,
}));

// D natural minor, for diatonic harmony (a third above the melody).
const SCALE = [50, 52, 53, 55, 57, 58, 60]; // D E F G A Bb C
function thirdAbove(midi) {
  const pc = ((midi - 50) % 12 + 12) % 12;
  const deg = SCALE.findIndex((s) => (s - 50) % 12 === pc);
  if (deg < 0) return midi + 3;
  const up = SCALE[(deg + 2) % 7] + (deg + 2 >= 7 ? 12 : 0);
  return midi + (up - SCALE[deg]);
}

// ── arrangement ───────────────────────────────────────────────────────
// kick: [gain at section start, gain at section end] — never zero until
// the outro tail, and it eases in slowly (jas: "always present and come
// in more slow").
const SECTIONS = [
  { name: "intro",  bars: 8,  kick: [0.14, 0.34], hat: "off8", clap: 0, bass: 0, pad: 1, pluck: 1, lead: 0, ladder: 0 },
  { name: "break1", bars: 8,  kick: [0.34, 0.42], hat: "off8", clap: 0, bass: 0, pad: 1, pluck: 1, lead: 0, ladder: 0, voice: 1 },
  { name: "build1", bars: 8,  kick: [0.46, 0.56], hat: "off8", clap: 0, bass: 1, pad: 1, pluck: 1, lead: 0, ladder: "build" },
  { name: "drop1",  bars: 16, kick: [0.62, 0.62], hat: "full", clap: 1, bass: 1, pad: 1, pluck: 0, lead: 1, ladder: "shimmer", impact: 1 },
  { name: "break2", bars: 8,  kick: [0.44, 0.44], hat: "off8", clap: 0, bass: 0, pad: 1, pluck: 1, lead: 0, ladder: 0, voice: 1 },
  { name: "build2", bars: 4,  kick: [0.50, 0.58], hat: "off8", clap: 0, bass: 1, pad: 1, pluck: 0, lead: 0, ladder: "build" },
  { name: "drop2",  bars: 16, kick: [0.62, 0.62], hat: "full", clap: 1, bass: 1, pad: 1, pluck: 0, lead: 1, ladder: "shimmer", impact: 1, lift: 1 },
  { name: "outro",  bars: 8,  kick: [0.55, 0.0],  hat: "off8", clap: 0, bass: 0, pad: 1, pluck: 0, lead: 0, ladder: 0 },
];
let cursor = 0;
for (const s of SECTIONS) { s.start = cursor * BAR; cursor += s.bars; }
const TOTAL_BARS = cursor;
const TAIL = 3.0;
const N = Math.ceil((TOTAL_BARS * BAR + TAIL) * SR);

// ── buses (stereo pairs) ──────────────────────────────────────────────
const mkBus = () => ({ L: new Float32Array(N), R: new Float32Array(N) });
const drums = mkBus(), bass = mkBus(), music = mkBus(), voice = mkBus(), fx = mkBus();
const kickTimes = [];

let rngState = 0x9e3779b9;
function rnd() {
  rngState ^= rngState << 13; rngState >>>= 0;
  rngState ^= rngState >>> 17; rngState ^= rngState << 5; rngState >>>= 0;
  return (rngState >>> 0) / 0xffffffff * 2 - 1;
}

// ── humanize — a band, not a grid ────────────────────────────────────
// Ensemble model (jas: "played by an actual band ... each section
// humanized slightly different with models of one trying to follow
// another / a lead"). The drummer (kick) walks a slow timing drift;
// every other player FOLLOWS that drift — heard one beat late, at
// their own depth, plus their own hand-noise. The melody is a soloist
// with an independent walk. Each section has its own tightness: the
// breakdowns are loose, the drops lock in.
const TOTAL_BEATS_ALL = TOTAL_BARS * 4 + 2;
const drift = new Float64Array(TOTAL_BEATS_ALL);      // drummer's walk (sec)
for (let b = 1; b < TOTAL_BEATS_ALL; b++)
  drift[b] = drift[b - 1] * 0.88 + rnd() * 0.004;
const soloDrift = new Float64Array(TOTAL_BEATS_ALL);  // soloist's own walk
for (let b = 1; b < TOTAL_BEATS_ALL; b++)
  soloDrift[b] = soloDrift[b - 1] * 0.85 + rnd() * 0.005;
const TIGHT = { intro: 1.3, break1: 1.45, build1: 1.0, drop1: 0.7, break2: 1.5, build2: 0.95, drop2: 0.65, outro: 1.25 };
// follower: chases the drummer's drift `lag` beats late at `follow`
// depth, with `own` seconds of personal slop
function playerT(beatIdx, { follow = 0.8, lag = 1, own = 0.004 }, tight = 1) {
  const d = drift[Math.max(0, Math.min(TOTAL_BEATS_ALL - 1, beatIdx - lag))];
  return (follow * d + rnd() * own) * tight;
}
const hmg = (g, amt = 0.25) => g * (1 + rnd() * amt);          // velocity
// slow ensemble swell — the whole band leans in and out over ~8 bars
const swell = (t) => 1 + 0.10 * Math.sin(TAU * t / (BAR * 8) + 1.3);

// Raised-cosine attack/release — the no-clicks contract for every event.
function rcEnv(lt, dur, atk, rel) {
  if (lt < 0 || lt >= dur) return 0;
  let e = 1;
  if (lt < atk) e = 0.5 - 0.5 * Math.cos(Math.PI * lt / atk);
  const rem = dur - lt;
  if (rem < rel) e *= Math.min(e, 0.5 - 0.5 * Math.cos(Math.PI * rem / rel));
  return e;
}

// Equal-power placement (Special Sign style): pan ∈ [-1,1] → cos/sin.
function place(bus, t, dur, fn, { pan = 0, atk = 0.008, rel = 0.03, gain = 1, drift = 0 } = {}) {
  const i0 = Math.max(0, Math.floor(t * SR));
  const i1 = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = i0; i < i1; i++) {
    const lt = i / SR - t;
    const e = rcEnv(lt, dur, atk, rel);
    if (e === 0) continue;
    const p = Math.max(-1, Math.min(1, pan + drift * Math.sin(TAU * 0.06 * (i / SR))));
    const a = (p + 1) * Math.PI / 4;
    const s = fn(lt) * e * gain;
    bus.L[i] += s * Math.cos(a);
    bus.R[i] += s * Math.sin(a);
  }
}

// Drop a mono scratch buffer into a stereo bus, equal-power panned,
// with raised-cosine edge fades so truncated tails can't click.
function placeScratch(bus, scratch, t, gain, pan) {
  const a = (Math.max(-1, Math.min(1, pan)) + 1) * Math.PI / 4;
  const gL = Math.cos(a) * gain, gR = Math.sin(a) * gain;
  const start = Math.floor(t * SR);
  const fadeN = Math.floor(0.004 * SR);
  for (let k = 0; k < scratch.length; k++) {
    const di = start + k;
    if (di < 0 || di >= N) continue;
    let s = scratch[k];
    if (k < fadeN) s *= 0.5 - 0.5 * Math.cos(Math.PI * k / fadeN);
    if (scratch.length - k < fadeN) s *= 0.5 - 0.5 * Math.cos(Math.PI * (scratch.length - k) / fadeN);
    bus.L[di] += s * gL;
    bus.R[di] += s * gR;
  }
}

// ── AC drumtrack percussion — playPercussion → scratch → stereo bus ──
function perc(letter, t, { volume = 1, pan = 0, pitchFactor = 1, dur = 0.8, bus = drums } = {}) {
  const scratch = new Float32Array(Math.ceil(dur * SR));
  playPercussion(makeBufferSynth(scratch, 0, SR), letter, { volume, pitchFactor });
  placeScratch(bus, scratch, t, 1, pan);
}

function kick(t, gain) {
  if (gain <= 0.01) return;
  kickTimes.push(t);
  perc("c", t, { volume: gain * 1.1, dur: 0.6 });
}
// the "trackpad edge click" — AC's g# woodblock, tiny and dry
function tick(t, gain, pan, pf = 1) {
  perc("g#", t, { volume: gain, pan, pitchFactor: pf, dur: 0.1 });
}

// Harmonized smooth sine stack — 3 detuned sines + soft partials.
function sineStack(f, detCents = 5) {
  const fs = [f * Math.pow(2, -detCents / 1200), f, f * Math.pow(2, detCents / 1200)];
  const ph = [rnd() * 0.5 + 0.5, rnd() * 0.5 + 0.5, rnd() * 0.5 + 0.5];
  return (lt) => {
    let x = 0;
    for (let v = 0; v < 3; v++) x += Math.sin(TAU * (fs[v] * lt + ph[v]));
    x /= 3;
    x += 0.16 * Math.sin(TAU * f * 2 * lt) + 0.05 * Math.sin(TAU * f * 3 * lt);
    return x * 0.8;
  };
}

function subBass(t, dur, midi, gain) {
  const f = m2f(midi);
  place(bass, t, dur, (lt) =>
    Math.sin(TAU * f * lt) + 0.12 * Math.sin(TAU * f * 2 * lt),
  { pan: 0, atk: 0.008, rel: 0.05, gain });
}

function padChord(t, dur, midis, gain) {
  const spread = [-0.55, 0, 0.55];
  midis.forEach((m, i) => {
    place(music, t, dur, sineStack(m2f(m), 7),
      { pan: spread[i % 3], drift: 0.3, atk: dur * 0.25, rel: dur * 0.3, gain: gain / midis.length });
  });
}

function pluck(t, dur, midi, gain, pan = 0) {
  const fn = sineStack(m2f(midi), 4);
  place(music, t, Math.min(dur, 0.9), (lt) => fn(lt) * Math.exp(-lt / 0.28),
    { pan, drift: 0.25, atk: 0.004, rel: 0.06, gain });
}

function lead(t, dur, midi, gain, pan = 0) {
  place(music, t, dur, sineStack(m2f(midi), 9),
    { pan, drift: 0.4, atk: 0.012, rel: Math.min(0.12, dur * 0.3), gain });
}

// One chromatic ladder step: a SEPARATED bandpassed-noise pitch (the
// bus.mjs noise voice is SVF-bandpassed at `tone` — noise with a note).
function ladderStep(t, midi, dur, gain, pan) {
  const scratch = new Float32Array(Math.ceil((dur + 0.06) * SR));
  makeBufferSynth(scratch, 0, SR).synth({
    type: "noise", tone: m2f(midi), duration: dur, volume: 1,
    attack: dur * 0.25, decay: dur * 0.45,
  });
  placeScratch(fx, scratch, t, gain, pan);
}

function impact(t) {
  perc("c#", t, { volume: 0.55, dur: 2.5, bus: fx });          // AC crash
  perc("c", t, { volume: 0.8, pitchFactor: 0.8, dur: 0.8, bus: fx }); // low 808 boom
  place(fx, t, 1.0, (lt) => Math.sin(TAU * (42 * lt - 8 * lt * lt)) * Math.exp(-lt / 0.3),
    { pan: 0, atk: 0.002, rel: 0.3, gain: 0.4 });
}

// ── wav loader (voice takes) — same contract as render.mjs ────────────
function loadWav(path) {
  const b = readFileSync(path);
  let p = 12, fmt = null, dOff = 0, dLen = 0;
  while (p + 8 <= b.length) {
    const id = b.toString("ascii", p, p + 4);
    const sz = b.readUInt32LE(p + 4);
    if (id === "fmt ") fmt = { format: b.readUInt16LE(p + 8), ch: b.readUInt16LE(p + 10),
      sr: b.readUInt32LE(p + 12), bits: b.readUInt16LE(p + 22) };
    else if (id === "data") { dOff = p + 8; dLen = sz; }
    p += 8 + sz + (sz & 1);
  }
  if (!fmt || !dOff) throw new Error(`bad WAV: ${path}`);
  const fb = (fmt.bits / 8) * fmt.ch, frames = Math.floor(dLen / fb);
  let mono = new Float32Array(frames);
  for (let i = 0; i < frames; i++) {
    let acc = 0;
    for (let c = 0; c < fmt.ch; c++) {
      const o = dOff + i * fb + c * (fmt.bits / 8);
      if (fmt.format === 3 && fmt.bits === 32) acc += b.readFloatLE(o);
      else if (fmt.bits === 16) acc += b.readInt16LE(o) / 32768;
      else if (fmt.bits === 24) acc += (b.readUInt8(o) | (b.readUInt8(o + 1) << 8) | (b.readInt8(o + 2) << 16)) / 8388608;
      else if (fmt.bits === 32) acc += b.readInt32LE(o) / 2147483648;
    }
    mono[i] = acc / fmt.ch;
  }
  if (fmt.sr !== SR) {
    const outN = Math.round(frames * SR / fmt.sr), rs = new Float32Array(outN);
    for (let i = 0; i < outN; i++) {
      const x = i * fmt.sr / SR, i0 = Math.floor(x), fr = x - i0;
      rs[i] = (mono[i0] || 0) + ((mono[i0 + 1] || 0) - (mono[i0] || 0)) * fr;
    }
    mono = rs;
  }
  let a = 0, e = mono.length; const TH = 0.02;
  while (a < e && Math.abs(mono[a]) < TH) a++;
  while (e > a && Math.abs(mono[e - 1]) < TH) e--;
  mono = mono.subarray(a, e);
  let pk = 0; for (let i = 0; i < mono.length; i++) pk = Math.max(pk, Math.abs(mono[i]));
  if (pk > 0) for (let i = 0; i < mono.length; i++) mono[i] /= pk;
  return mono;
}

let takeCount = 0;
function placeTake(t, id) {
  const path = resolve(TAKES, `${id}.wav`);
  if (!existsSync(path)) return false;
  const samp = loadWav(path);
  const fadeN = Math.floor(0.02 * SR);
  const start = Math.floor(t * SR);
  for (let k = 0; k < samp.length; k++) {
    const di = start + k;
    if (di < 0 || di >= N) continue;
    let s = samp[k] * 0.9;
    if (k < fadeN) s *= 0.5 - 0.5 * Math.cos(Math.PI * k / fadeN);
    if (samp.length - k < fadeN) s *= 0.5 - 0.5 * Math.cos(Math.PI * (samp.length - k) / fadeN);
    voice.L[di] += s * Math.SQRT1_2;
    voice.R[di] += s * Math.SQRT1_2;
  }
  takeCount++;
  return true;
}

// ── chords: i – VI – III – VII in D minor, one per bar ───────────────
const CHORD_PADS = [[57, 62, 65], [58, 62, 65], [57, 60, 65], [55, 60, 64]];
const CHORD_ROOTS = [38, 34, 41, 36]; // D2 Bb1 F2 C2

// ── render ────────────────────────────────────────────────────────────
function melodyStatement(t0, { octave = 0, gain = 0.30, harmony = 0, asPluck = false, withVoice = false, tight = 1 } = {}) {
  for (const n of PHRASE) {
    // the soloist: their own drift walk, phrasing that leans into the
    // climb (crescendo toward the high C/D) and softens the landings
    const gb = Math.max(0, Math.round((t0 + n.beat * SPB) / SPB));
    const t = t0 + n.beat * SPB + (soloDrift[Math.min(gb, TOTAL_BEATS_ALL - 1)] + rnd() * 0.003) * tight;
    const dur = n.durBeats * SPB * 0.95;
    const m = n.midi + octave;
    const phrasing = (0.8 + 0.35 * (n.midi - 50) / 12) * (n.durBeats >= 2 ? 0.9 : 1);
    const g = hmg(gain * phrasing, 0.18);
    if (asPluck) pluck(t, dur, m + 12, g, (n.beat % 2 === 0 ? -1 : 1) * 0.25);
    else lead(t, dur, m + 12, g, (n.beat % 4 < 2 ? -1 : 1) * 0.2);
    // the harmony singer shadows the soloist a hair late, a touch softer
    if (harmony > 0) lead(t + 0.006 + rnd() * 0.004, dur, thirdAbove(m) + 12, g * harmony, (n.beat % 4 < 2 ? 1 : -1) * 0.35);
    if (withVoice) placeTake(t, n.id);
  }
}

for (const s of SECTIONS) {
  const beats = s.bars * 4;
  const secDur = s.bars * BAR;

  // The chromatic ladder is the organizing grid for this section (jas:
  // "noise more separated and chromaticized when it ramps up ... the
  // perc and other instruments could organize around that").
  //   build:   quarter-note steps, one semitone each, D4 climbing —
  //            ticks ride the same steps, pitched with the ladder
  //   shimmer: 16th-note pulse inside the drops, one semitone per
  //            beat cycling up an octave every 4 bars — snaps accent
  //            each new semitone
  const ladderPlan = [];
  if (s.ladder === "build") {
    const steps = beats; // one per quarter
    for (let k = 0; k < steps; k++) {
      const midi = 62 + Math.floor(k * 24 / steps); // D4 → D6 chromatic
      const o = k / steps;
      ladderPlan.push({ t: s.start + k * SPB, midi, dur: SPB * 0.6, gain: 0.05 + 0.30 * o * o, pan: (k % 2 ? 1 : -1) * (0.3 + 0.4 * o), tick: true, tickPf: Math.pow(2, (midi - 62) / 12) });
    }
  } else if (s.ladder === "shimmer") {
    for (let b = 0; b < beats; b++) {
      const semis = b % 16; // cycle an octave + 4 per 4-bar phrase
      const midi = 74 + semis; // D5 upward
      for (let q = 0; q < 4; q++) {
        ladderPlan.push({ t: s.start + b * SPB + q * SPB / 4, midi, dur: SPB * 0.22, gain: q === 0 ? 0.10 : 0.055, pan: (q % 2 ? 1 : -1) * 0.5, tick: false });
      }
      if (semis === 0 || b % 4 === 2) // accent each phrase reset + mid-bar
        perc("f", s.start + b * SPB, { volume: 0.30, pan: (b % 8 < 4 ? -1 : 1) * 0.3, dur: 0.15 });
    }
  }
  // the ladder is the anchor grid — near-machine, just a whisper of hand
  for (const st of ladderPlan) {
    ladderStep(st.t + rnd() * 0.0015, st.midi, st.dur, hmg(st.gain, 0.2), st.pan);
    if (st.tick) tick(st.t + rnd() * 0.003, hmg(0.10 + st.gain * 0.3, 0.35), st.pan * -1, st.tickPf);
  }

  const tight = TIGHT[s.name] ?? 1;
  for (let b = 0; b < beats; b++) {
    const t = s.start + b * SPB;
    const gb = Math.round(s.start / SPB) + b;
    const bar = Math.floor(b / 4);
    const prog = bar % 4;
    const o = b / beats;

    // kick — the leader: always present, slow-ramped, walks its own drift
    kick(t + drift[gb] * 0.4 * tight, hmg(s.kick[0] + (s.kick[1] - s.kick[0]) * o, 0.08));

    // the featured tick layer — trackpad edge clicks on the offbeat
    // 16ths: the loosest follower, one beat behind the drummer, and
    // the most dynamic player in the band
    const tickPf = [1, 0.891, 1.122, 1][prog]; // follows i–VI–III–VII loosely
    tick(t + SPB * 0.5 + playerT(gb, { follow: 0.9, lag: 1, own: 0.006 }, tight),
      hmg(0.085, 0.45) * swell(t), b % 2 ? 0.4 : -0.4, tickPf);
    if (s.hat === "full")
      tick(t + SPB * 0.75 + playerT(gb, { follow: 0.9, lag: 1, own: 0.007 }, tight),
        hmg(0.05, 0.5) * swell(t), b % 2 ? -0.55 : 0.55, tickPf * 1.335);

    // hats — AC closed/open hats, following the kick a beat behind
    if (s.hat === "off8")
      perc("g", t + SPB / 2 + playerT(gb, { follow: 0.8, lag: 1, own: 0.004 }, tight),
        { volume: hmg(0.22, 0.3) * swell(t), pan: b % 2 ? 0.25 : -0.25, dur: 0.12 });
    else if (s.hat === "full") {
      for (let q = 1; q < 4; q++)
        perc("g", t + q * SPB / 4 + playerT(gb, { follow: 0.8, lag: 1, own: 0.004 }, tight),
          { volume: hmg(q === 2 ? 0.24 : 0.13, 0.3) * swell(t), pan: (q % 2 ? 1 : -1) * 0.3, dur: 0.12 });
      if (b % 2 === 1)
        perc("a", t + SPB / 2 + playerT(gb, { follow: 0.8, lag: 1, own: 0.005 }, tight),
          { volume: hmg(0.16, 0.25), pan: 0.1, dur: 0.9 });
    }

    // clap + snap on 2 & 4 — steadier hands, shallower follow
    if (s.clap && (b % 4 === 1 || b % 4 === 3)) {
      perc("e", t + playerT(gb, { follow: 0.6, lag: 1, own: 0.005 }, tight), { volume: hmg(0.5, 0.2), dur: 0.35 });
      perc("f", t + playerT(gb, { follow: 0.6, lag: 1, own: 0.006 }, tight), { volume: hmg(0.22, 0.3), pan: 0.15, dur: 0.15 });
    }

    // offbeat sub bass — locked tight to the drummer, no lag
    if (s.bass)
      subBass(t + SPB / 2 + playerT(gb, { follow: 0.7, lag: 0, own: 0.002 }, tight),
        SPB * 0.42, CHORD_ROOTS[prog], hmg(0.42, 0.12));
  }

  // pads — one chord per bar
  if (s.pad) {
    for (let bar = 0; bar < s.bars; bar++) {
      let g = 0.24;
      if (s.name === "intro") g = 0.10 + 0.14 * (bar / s.bars);
      if (s.name === "outro") g = 0.24 * Math.max(0.15, 1 - bar / s.bars);
      padChord(s.start + bar * BAR, BAR * 1.02, CHORD_PADS[bar % 4], g);
    }
  }
  // melody
  if (s.pluck) melodyStatement(s.start + BAR, { gain: s.name === "intro" ? 0.16 : 0.22, asPluck: true, withVoice: Boolean(s.voice), tight });
  if (s.lead) {
    melodyStatement(s.start, { gain: 0.30, harmony: s.lift ? 0.55 : 0.35, tight });
    melodyStatement(s.start + 8 * BAR, { gain: 0.30, harmony: s.lift ? 0.7 : 0.45, withVoice: true, tight });
  }
  if (s.impact) impact(s.start);
}

// ── ping-pong delay on the music bus (dotted-eighth) ─────────────────
{
  const d = Math.floor(SPB * 0.75 * SR);
  for (let i = d; i < N; i++) {
    music.L[i] += music.R[i - d] * 0.30;
    music.R[i] += music.L[i - d] * 0.30;
  }
}

// ── sidechain duck — bass hard, music soft, from real kick times ─────
{
  const duck = new Float32Array(N).fill(1);
  const hold = Math.floor(0.02 * SR), rec = Math.floor(0.24 * SR);
  for (const kt of kickTimes) {
    const k0 = Math.floor(kt * SR);
    for (let i = 0; i < hold + rec; i++) {
      const di = k0 + i;
      if (di >= N) break;
      const d = i < hold ? 1 : 0.5 + 0.5 * Math.cos(Math.PI * (1 - (i - hold) / rec));
      duck[di] = Math.min(duck[di], 1 - d);
    }
  }
  for (let i = 0; i < N; i++) {
    const bassG = 1 - (1 - duck[i]) * 0.85;
    const musG = 1 - (1 - duck[i]) * 0.45;
    bass.L[i] *= bassG; bass.R[i] *= bassG;
    music.L[i] *= musG; music.R[i] *= musG;
  }
}

// ── spatialize: slow room rotation + mono-safe wet side return ───────
{
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    const th = 0.22 * Math.sin(TAU * 0.026 * t);
    const c = Math.cos(th), sn = Math.sin(th);
    const l = music.L[i], r = music.R[i];
    music.L[i] = l * c - r * sn;
    music.R[i] = l * sn + r * c;
  }
  const wd = Math.floor(0.012 * SR);
  for (let i = N - 1; i >= wd; i--) {
    const side = (music.L[i - wd] - music.R[i - wd]) * 0.5;
    const w = 0.16 + 0.06 * Math.sin(TAU * 0.02 * (i / SR));
    music.L[i] += side * w;
    music.R[i] -= side * w;
  }
}

// ── mix + master: sum buses, peak-normalize, NO clipping stage ───────
const L = new Float32Array(N), R = new Float32Array(N);
const BUS_GAIN = [[drums, 1.0], [bass, 0.9], [music, 1.0], [voice, 1.0], [fx, 1.0]];
for (const [bus, g] of BUS_GAIN)
  for (let i = 0; i < N; i++) { L[i] += bus.L[i] * g; R[i] += bus.R[i] * g; }
// global fade-out over the tail
const fadeStart = N - Math.floor((TAIL + 1.5) * SR);
for (let i = fadeStart; i < N; i++) {
  const f = 0.5 + 0.5 * Math.cos(Math.PI * (i - fadeStart) / (N - fadeStart));
  L[i] *= f; R[i] *= f;
}
let peak = 0;
for (let i = 0; i < N; i++) peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
const g = peak > 0 ? 0.89 / peak : 1;

mkdirSync(OUTDIR, { recursive: true });
const wavPath = resolve(OUTDIR, "hum-club.wav");
const w = Buffer.alloc(44 + N * 4);
w.write("RIFF", 0); w.writeUInt32LE(36 + N * 4, 4); w.write("WAVE", 8);
w.write("fmt ", 12); w.writeUInt32LE(16, 16); w.writeUInt16LE(1, 20);
w.writeUInt16LE(2, 22); w.writeUInt32LE(SR, 24); w.writeUInt32LE(SR * 4, 28);
w.writeUInt16LE(4, 32); w.writeUInt16LE(16, 34);
w.write("data", 36); w.writeUInt32LE(N * 4, 40);
for (let i = 0; i < N; i++) {
  w.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(L[i] * g * 32767))), 44 + i * 4);
  w.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(R[i] * g * 32767))), 46 + i * 4);
}
writeFileSync(wavPath, w);
const mp3Path = resolve(OUTDIR, "hum-club.mp3");
spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wavPath,
  "-af", "loudnorm=I=-14:TP=-1.5:LRA=9", "-b:a", "320k", mp3Path], { stdio: "ignore" });

console.log(`hum-club · ${BPM} BPM · ${TOTAL_BARS} bars · ${(N / SR).toFixed(1)}s · ` +
  `${kickTimes.length} kicks · ${takeCount} voice-take placements`);
console.log(`→ ${wavPath}`);
console.log(`→ ${mp3Path}`);
if (PLAY) spawnSync("afplay", [wavPath], { stdio: "ignore" });
