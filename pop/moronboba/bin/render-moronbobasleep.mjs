#!/usr/bin/env node
// render-moronbobasleep.mjs — Moronbobasleep, a 10-minute marimba sleep mix.
//
// The calm nocturne twin of pop/marimba/marimbaba. Where sleephellsine
// abandoned hellsine's instruments for pure sine, Moronbobasleep keeps the
// actual marimba synth — that's what makes it a *marimbaba remix* and not a
// sleephellsine clone. The 80-second lullaby is dissolved into a slow,
// generative 10-minute drift: marimbaba's own phrase cells recur on a lazy
// random walk, very sparse, rung out to their full decay, over a held
// vibraphone pad, a slow rocking-chair bass, kalimba twinkles, and a soft
// brownian bed underneath.
//
// "Moron" = the lullaby gone dozy and dumb-happy-slow. Stays home in F major
// (marimbaba's world: F / Dm / Bb / C, all diatonic), wandering between the
// four chords every ~16-22 s, voice-led by the pad's long ring.
//
// Meter is free; a slow ~40 BPM pulse only sets cell note lengths. Events get
// sparser toward the end — the piece itself falls asleep.
//
// Engine renders 10:10; the master truncates to 10:00 with a 60 s fade-in and
// a 14 s fade-out, loudnorm'd to the -28 LUFS sleep target (same chain as
// pop/sleephellsine/bin/bake.mjs).
//
// Run:
//   node pop/moronboba/bin/render-moronbobasleep.mjs
//   node pop/moronboba/bin/render-moronbobasleep.mjs --out ~/m.mp3
//   node pop/moronboba/bin/render-moronbobasleep.mjs --seed nightfall

import { mixEventMarimba } from "../../marimba/synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync, readFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 44_100;

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };

// ── timing ─────────────────────────────────────────────────────────────
const BPM = 96;                 // rolling 8th-note pulse (see ROLL below)
const RENDER_SEC = 610;         // 10:10 — master truncates to 10:00
const ns = Math.ceil(RENDER_SEC * SR);

function midiToFreq(m) { return 440 * Math.pow(2, (m - 69) / 12); }

// minimal PCM WAV → mono Float32 reader (16/24/32-bit + float). The rain
// sample is 44.1k mono 16-bit, so no resample needed.
function readWavMono(path) {
  const buf = readFileSync(path);
  let off = 12, fmt = null, dataOff = 0, dataLen = 0;
  while (off + 8 <= buf.length) {
    const id = buf.toString("ascii", off, off + 4), sz = buf.readUInt32LE(off + 4);
    if (id === "fmt ") fmt = { af: buf.readUInt16LE(off + 8), ch: buf.readUInt16LE(off + 10), sr: buf.readUInt32LE(off + 12), bits: buf.readUInt16LE(off + 22) };
    else if (id === "data") { dataOff = off + 8; dataLen = sz; }
    off += 8 + sz + (sz & 1);
  }
  if (!fmt || !dataOff) throw new Error(`bad wav: ${path}`);
  const { ch, bits, af } = fmt, bps = bits / 8;
  const frames = Math.floor(dataLen / (bps * ch));
  const out = new Float32Array(frames);
  for (let i = 0; i < frames; i++) {
    let acc = 0;
    for (let c = 0; c < ch; c++) {
      const p = dataOff + (i * ch + c) * bps;
      let v = 0;
      if (bits === 16) v = buf.readInt16LE(p) / 32768;
      else if (bits === 24) { let x = (buf[p + 2] << 16) | (buf[p + 1] << 8) | buf[p]; if (x & 0x800000) x |= ~0xffffff; v = x / 8388608; }
      else if (bits === 32 && af === 3) v = buf.readFloatLE(p);
      else if (bits === 32) v = buf.readInt32LE(p) / 2147483648;
      acc += v;
    }
    out[i] = acc / ch;
  }
  return { data: out, sampleRate: fmt.sr };
}

// ── deterministic RNG (re-renders are bit-identical for a given seed) ───
function seedFrom(str) {
  let h = 0x811c9dc5 >>> 0;
  for (let i = 0; i < str.length; i++) { h ^= str.charCodeAt(i); h = Math.imul(h, 0x01000193) >>> 0; }
  return h >>> 0;
}
let _rng = seedFrom(_argi("--seed") || "moronbobasleep");
function rnd() { _rng = (Math.imul(_rng, 1664525) + 1013904223) >>> 0; return _rng / 0xffffffff; }
const noise = () => rnd() * 2 - 1;
const range = (a, b) => a + rnd() * (b - a);

// ── note table (F-major world) ─────────────────────────────────────────
const N = {
  Bb1: 34, C2: 36, D2: 38, F2: 41, A2: 45, Bb2: 46,
  C3: 48, D3: 50, F3: 53, G3: 55, A3: 57, Bb3: 58,
  C4: 60, D4: 62, E4: 64, F4: 65, G4: 67, A4: 69, Bb4: 70,
  C5: 72, D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, Bb5: 82,
  C6: 84, D6: 86,
};

// ═══════════════════════════════════════════════════════════════════════
//  HARMONY — marimbaba's four diatonic chords, walked slowly.
// ═══════════════════════════════════════════════════════════════════════
// 0:F  1:Dm  2:Bb  3:C  +  4:Gm(ii)  5:Eb(bVII modal mixture — the weird one).
// `drone` is a deep-to-lush 5-voice stack (root −2 oct … 3rd) for the bed;
// `pad` stays mid-register for the marimba bloom; `roll` is an ascending
// chord-tone scale (triad + a 6th/9th colour) the rolling line weaves up and
// down — keeps every note locked to the harmony while it tumbles.
const CHORDS = [
  { name: "F",  bass: N.F2,  pad: [N.F4, N.A4, N.C5], spark: [N.F5, N.A5, N.C6], drone: [29, 41, 48, 53, 57], roll: [65, 69, 72, 74, 77, 81, 84, 86] },
  { name: "Dm", bass: N.D2,  pad: [N.D4, N.F4, N.A4], spark: [N.D5, N.F5, N.A5], drone: [26, 38, 45, 50, 53], roll: [62, 65, 69, 72, 74, 77, 81, 84] },
  { name: "Bb", bass: N.Bb1, pad: [N.Bb3, N.D4, N.F4], spark: [N.Bb4, N.D5, N.F5], drone: [22, 34, 41, 46, 50], roll: [58, 62, 65, 67, 70, 74, 77, 82] },
  { name: "C",  bass: N.C2,  pad: [N.C4, N.E4, N.G4], spark: [N.C5, N.E5, N.G5], drone: [24, 36, 43, 48, 52], roll: [60, 64, 67, 69, 72, 76, 79, 84] },
  { name: "Gm", bass: 43,    pad: [N.D4, N.G4, N.Bb4], spark: [N.G5, N.Bb5, N.D6], drone: [31, 43, 50, 55, 58], roll: [55, 58, 62, 67, 70, 74, 79, 82] },
  { name: "Eb", bass: 39,    pad: [63, 67, 70],        spark: [75, 79, 82],        drone: [27, 39, 46, 51, 55], roll: [63, 67, 70, 75, 79, 82, 87] },
];
// (the chord order is now a fixed functional PROGRESSION — see PROG below.)

// ═══════════════════════════════════════════════════════════════════════
//  PER-VOICE EVENT LISTS — gathered first, rendered in groups (low memory).
// ═══════════════════════════════════════════════════════════════════════
const evLead  = [];   // rosewood — the broken-chord arpeggio figuration + swoops
const evBass  = [];   // bass — the walking baroque bass
const evPad   = [];   // vibraphone_off — the dream-haze pad bloom
const evAlto  = [];   // rosewood — the quaternary inner voice (bigger movements)
const evSpark = [];   // kalimba — a high chord sparkle per bar
// (the top melody is a WHISTLE synth → whistleEvents, rendered separately)

// ── timing — slower, measured baroque pulse ──────────────────────────────
const ARP = 0.40;                 // one arpeggio note (~75 BPM 8ths) — flowing, not fast
const ARPN = 8;                   // arpeggio notes per bar
const BAR = ARP * ARPN;           // ~3.2 s per chord

// ── a LIBRARY of functional progressions — each chapter gets its own, so the
//    HARMONY itself goes on a journey (not one loop transposed). 0:F 1:Dm
//    2:Bb 3:C 4:Gm 5:Eb.
const PROGS = [
  [0, 0, 3, 0],                       // calm tonic rock (drift)        F · F · C · F
  [0, 2, 3, 0],                       // plagal sway (settle)           F · Bb · C · F
  [0, 1, 4, 3],                       // ii–V begins (sink)             F · Dm · Gm · C
  [1, 4, 3, 0,  2, 0, 4, 3],          // fuller turn (deepen)           Dm Gm C F · Bb F Gm C
  [4, 3, 0, 1,  4, 3, 2, 0],          // dreaming                       Gm C F Dm · Gm C Bb F
  [0, 4, 3, 1,  4, 5, 2, 3],          // deep dream (Eb colour)         F Gm C Dm · Gm Eb Bb C
  [5, 2, 4, 5,  3, 0, 1, 4],          // strange REM (modal)            Eb Bb Gm Eb · C F Dm Gm
  [2, 3, 0, 0],                       // plagal homecoming (dawn)       Bb · C · F · F
];
//                drift settle sink deepen dream deep THE  REM  dawn
const MOVE_PROG = [0,    1,    2,   3,     4,    5,   5,   6,   7];

// ── FORM: a FIBONACCI ARCH of 9 movements telling a NIGHT NARRATIVE — each is
//    a chapter, and the bar-counts grow to a LATE peak so the climax (the
//    deepest dream) lands near the GOLDEN SECTION (~0.6 of the runtime), then
//    recede to dawn. Calm throughout (sleep genre): the "climax" is the
//    richest dream, never a loud drop. Each movement carries its own dynamic
//    LEVEL, key, whistle REGISTER, density, and gnarl — so the 10 minutes
//    genuinely transforms instead of holding one constant texture.
//          drift settle  sink  deepen dream- DEEP   THE    REM    dawn
//                                      -ing   dream  DREAM  strange
const FIB     = [3,   5,    8,    13,   21,   34,   55,   34,   13];   // Σ=186 ≈ 9:55
const MOVE_TR = [0,   0,    0,    2,    5,    7,    7,    3,    0];     // gentle key journey, home at the ends
const M_LEVEL = [0.26, 0.42, 0.58, 0.72, 0.85, 0.95, 1.0, 0.76, 0.42];// dynamic arc (baked into the mix)
const M_WOCT  = [-12, -12,  -12,  -12,  0,    0,    0,   -12,  -12];   // whistle register per movement
const MOVE = FIB.map((bars, i) => ({
  bars, tr: MOVE_TR[i], level: M_LEVEL[i], woct: M_WOCT[i], prog: PROGS[MOVE_PROG[i]],
  arp:     i >= 1,                              // no arpeggio in the opening "drift" (rain + pad only)
  arpDens: Math.min(1, 0.35 + 0.65 * M_LEVEL[i]),  // sparser when quiet
  span:    M_LEVEL[i] > 0.8 ? 7 : M_LEVEL[i] > 0.55 ? 6 : 5,  // arpeggio range widens as the night deepens
  whistle: i >= 2,                              // the whistle enters on the 3rd chapter
  alto:    i >= 4 && i <= 7,                     // four-part harmony through the dream
  triplet: i === 5 || i === 6 ? 0.30 : 0.0,     // triplet flourishes only deep in
  gnarl:   i === 6 ? 0.20 : i === 7 ? 0.32 : 0.0,  // pterodactyl swoops at the dream + the strange REM
  sparkle: M_LEVEL[i] > 0.7,                     // high sparkle only when the texture is full
}));

const chordTimeline = [];   // { t, dur, ci, tr, mi }
{
  let t = 0;
  for (let mi = 0; mi < MOVE.length; mi++) {
    const mv = MOVE[mi];
    for (let k = 0; k < mv.bars; k++) {
      const ci = mv.prog[k % mv.prog.length];     // each chapter walks its OWN progression
      chordTimeline.push({ t, dur: BAR, ci, tr: mv.tr, mi });
      for (const m of CHORDS[ci].pad) evPad.push({ startSec: t, midi: m + mv.tr, durSec: BAR + 3, gain: range(0.05, 0.075) });
      t += BAR;
    }
  }
}
// F-major scale (midi, F4…F6) for the diatonic upper voices + their thirds.
const SCALE = [65, 67, 69, 70, 72, 74, 76, 77, 79, 81, 82, 84, 86, 88, 89];
const TRIAD = [[5, 9, 0], [2, 5, 9], [10, 2, 5], [0, 4, 7], [7, 10, 2], [3, 7, 10]];
const nearestIdx = (cur, pcs, lo, hi) => {
  let best = lo, bestd = 1e9;
  for (let i = lo; i <= hi; i++) {
    if (!pcs.includes(SCALE[i] % 12)) continue;
    const d = Math.abs(SCALE[i] - cur); if (d < bestd) { bestd = d; best = i; }
  }
  return best;
};

// ── 1) the TOP MELODY — a WHISTLE (MenuBand instrument 79 vibe): a voice-led
//      stepwise line landing on chord tones, harmonised a third below. Two
//      notes/bar. Collected for the whistle synth (rendered later). + tr.
const melOnsets = [];
const whistleEvents = [];
{
  let idx = SCALE.indexOf(81);            // start on A5
  for (const { t, ci, tr, mi } of chordTimeline) {
    const mv = MOVE[mi];
    if (!mv.whistle) continue;
    const pcs = TRIAD[ci];
    for (let h = 0; h < 2; h++) {
      const tgt = nearestIdx(SCALE[idx], pcs, 7, SCALE.length - 1);
      if (idx < tgt) idx++; else if (idx > tgt) idx--;
      idx = Math.max(7, Math.min(SCALE.length - 1, idx));
      // breathe: rest more often when the chapter is quiet (sleep phrasing)
      if (rnd() > 0.4 + 0.55 * mv.level) continue;
      const ts = t + h * (BAR / 2), dur = (BAR / 2) * range(0.96, 1.12);
      const top = SCALE[idx] + tr + mv.woct;   // register follows the movement (low at edges)
      whistleEvents.push({ t: ts, midi: top, dur, gain: 0.30 * range(0.95, 1.05), p: 0.14 });
      if (mv.level > 0.62) whistleEvents.push({ t: ts, midi: SCALE[Math.max(0, idx - 2)] + tr + mv.woct, dur: dur * 1.03, gain: 0.16, p: 0.26 });
      melOnsets.push(ts);
    }
  }
}

// ── 2) the QUATERNARY voice — a 4th inner (alto) line that comes in only in
//      the bigger movements (MOVE[mi].alto), slow counterpoint in F4–F5. + tr.
{
  let idx = 4;
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    if (!MOVE[mi].alto || t < 12) continue;
    const tgt = nearestIdx(SCALE[Math.min(idx, 6)], TRIAD[ci], 0, 6);
    if (idx < tgt) idx++; else if (idx > tgt) idx--;
    idx = Math.max(0, Math.min(6, idx));
    evAlto.push({ startSec: t, midi: SCALE[idx] + tr, durSec: BAR * range(0.9, 1.05), gain: 0.12 });
  }
}

// fractal PTERODACTYL SWOOP — a big up-then-dive arch with a nested faster
// ripple inside it (self-similar), in the screech register, gnarly + chromatic.
const swoop = (t, ci, tr) => {
  const sc = CHORDS[ci].roll, top = sc.length - 1, Nn = 20, step = BAR / Nn;
  for (let n = 0; n < Nn; n++) {
    const x = n / (Nn - 1);
    const arch = Math.sin(Math.PI * x);                       // 0→1→0 swoop
    const ripple = 0.30 * Math.abs(Math.sin(Math.PI * x * 5));// nested fractal wobble
    let idx = Math.round((arch * 0.7 + ripple) * top);
    idx = Math.max(0, Math.min(top, idx));
    let midi = sc[idx] + tr + 12;                             // screech octave up
    if (rnd() < 0.3) midi += (rnd() < 0.5 ? 1 : -1);          // gnarl chromatic
    evLead.push({ startSec: t + n * step, midi, durSec: step * range(1.0, 1.5), gain: 0.24 * range(0.9, 1.1) });
  }
};

// ── 3) the ARPEGGIO figuration — broken chord per bar (+ tr), TRIPLET
//      flourishes per movement, ducked under the whistle; and the fractal
//      pterodactyl SWOOPS replace a bar in the gnarly middle movements.
// a LIBRARY of 8-step figurations (values 0..4, scaled to each chapter's
// `span`) — the renderer walks THROUGH it so the arpeggio keeps reshaping
// bar to bar instead of repeating two patterns forever.
const PATS = [
  [0, 1, 2, 3, 4, 3, 2, 1],   // pendulum up-down
  [0, 2, 4, 2, 1, 3, 4, 2],   // broken thirds
  [0, 1, 2, 4, 3, 2, 1, 0],   // climb then settle
  [4, 3, 2, 1, 0, 1, 2, 3],   // descending start (inversion)
  [0, 2, 1, 3, 2, 4, 3, 1],   // climbing zigzag
  [0, 4, 1, 3, 2, 4, 0, 2],   // wide leaps
  [0, 1, 0, 2, 0, 3, 0, 4],   // pedal-tone (Alberti-ish)
  [4, 2, 0, 2, 4, 3, 1, 3],   // arch from the top
];
const TRIP = [0, 1, 2, 3, 4, 2, 0, 1, 2, 3, 4, 2];
const nearMel = (ts) => { for (const m of melOnsets) if (Math.abs(m - ts) < 0.11) return true; return false; };
{
  let pidx = 0;
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    const mv = MOVE[mi];
    if (!mv.arp) continue;                               // skip the opening drift
    if (rnd() < mv.gnarl) { swoop(t, ci, tr); continue; }   // pterodactyl bar
    const tones = CHORDS[ci].roll.slice(0, Math.min(mv.span, CHORDS[ci].roll.length)).map((m) => m - 12 + tr);
    const sc = (v) => tones[Math.round((v / 4) * (tones.length - 1))];   // 0..4 → the chapter's range
    const triplet = rnd() < mv.triplet;
    const Nn = triplet ? 12 : ARPN, step = BAR / Nn;
    pidx = (pidx + 1 + (rnd() < 0.3 ? 1 : 0)) % PATS.length;   // walk the figuration library
    const pat = triplet ? TRIP : PATS[pidx];
    for (let n = 0; n < Nn; n++) {
      if (n !== 0 && rnd() > mv.arpDens) continue;        // thin the run when the chapter is quiet
      const ts = t + n * step;
      const accent = n === 0 ? 1.0 : (n === (Nn >> 1) ? 0.82 : 0.66);
      let g = 0.30 * accent * range(0.95, 1.05);
      if (nearMel(ts)) g *= 0.7;                          // duck under the whistle
      evLead.push({ startSec: ts, midi: sc(pat[n % pat.length]), durSec: step * range(1.3, 1.8), gain: g });
      if (n === 0) evLead.push({ startSec: t, midi: tones[Math.min(2, tones.length - 1)], durSec: ARP * 2.4, gain: g * 0.55 });
    }
  }
}

// ── 4) the WALKING BASS — root on the downbeat, stepwise approach tone into
//      the next bar's actual (transposed) root. Onsets drive the low-end pump.
const bassOnsets = [];
{
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    if (!MOVE[mi].arp) continue;                         // drift chapter = no bass
    const root = CHORDS[ci].bass + tr;
    const ne = chordTimeline[(bi + 1) % chordTimeline.length];
    const next = CHORDS[ne.ci].bass + ne.tr;
    evBass.push({ startSec: t, midi: root, durSec: (BAR / 2) * 1.02, gain: 0.30 });
    const approach = next === root ? root + 7 : next - Math.sign(next - root) * 2;
    evBass.push({ startSec: t + BAR / 2, midi: approach, durSec: (BAR / 2) * 1.0, gain: 0.23 });
    bassOnsets.push(t, t + BAR / 2);
  }
}

// ── 5) chord SPARKLE on a EUCLIDEAN rhythm E(5,8) — evenly-spread pulses
//      (mathematical phrasing), only in the fuller chapters. + tr.
{
  let si = 0;
  for (const { t, ci, tr, mi } of chordTimeline) {
    if (!MOVE[mi].sparkle) continue;
    const onset = Math.floor(si * 5 / 8) !== Math.floor((si - 1) * 5 / 8);   // E(5,8)
    si++;
    if (!onset) continue;
    const sp = CHORDS[ci].spark;
    evSpark.push({ startSec: t, midi: sp[sp.length - 1] + tr, durSec: BAR * 0.8, gain: range(0.07, 0.11) });
  }
}

console.log(`→ moronbobasleep · seed=${_argi("--seed") || "moronbobasleep"} · ${chordTimeline.length} bars · ${MOVE.length} movements · ` +
  `${evLead.length} arp / ${evBass.length} bass / ${whistleEvents.length} whistle / ${evAlto.length} alto / ${evSpark.length} sparkle`);

// ═══════════════════════════════════════════════════════════════════════
//  RENDER — one mono bus reused per voice group, folded to stereo.
// ═══════════════════════════════════════════════════════════════════════
const outL = new Float32Array(ns);
const outR = new Float32Array(ns);
const bus  = new Float32Array(ns);

function pan(p) { const a = (p + 1) * Math.PI / 4; return [Math.cos(a), Math.sin(a)]; }

// render a voice group into `bus`, then fold into outL/outR with pan + gain,
// optionally widening with a small haas delay on the right channel.
function foldGroup(events, { preset, decayMul, p, gain, haasMs = 0, params = null, label }) {
  bus.fill(0);
  for (const ev of events) mixEventMarimba({ ...ev, preset, decayMul }, bus, { sampleRate: SR, params });
  const [lg, rg] = pan(p);
  const haas = Math.floor((haasMs / 1000) * SR);
  for (let i = 0; i < ns; i++) {
    const s = bus[i] * gain;
    outL[i] += s * lg;
    outR[i] += (haas && i - haas >= 0 ? bus[i - haas] : bus[i]) * gain * rg;
  }
  console.log(`   · ${label}`);
}

// decays tuned for ROLLING — tighter rings so the tumbling notes stay
// distinct and groovy (long rings would smear into mud), pad still blooms.
// SOFT FELT MALLETS — a long mallet contact time low-passes the strike + a
// slow amplitude attack, so every note blooms in soft and thick like it's
// played with a felt-wrapped mallet (wide bell vibe, no hard "ping").
const FELT_LEAD = { mallet: 0.009, attack: 0.007 };
const FELT_SOFT = { mallet: 0.007, attack: 0.006 };
const FELT_BASS = { mallet: 0.006, attack: 0.004 };
// mix placement — spread the voices across the field so each has room: bass +
// pad anchor the centre, arpeggio sits left, the inner alto centre-left, the
// sparkle right; the whistle lead (rendered below) lands centre-right.
foldGroup(evBass,  { preset: "bass",           decayMul: 1.15, p:  0.00, gain: 0.92, params: FELT_BASS, label: "walking bass" });
foldGroup(evPad,   { preset: "vibraphone_off", decayMul: 3.0,  p:  0.00, gain: 0.38, haasMs: 14, params: FELT_SOFT, label: "pad bloom" });
foldGroup(evLead,  { preset: "rosewood",       decayMul: 1.35, p: -0.26, gain: 0.82, params: FELT_LEAD, label: "arpeggio + swoops" });
foldGroup(evAlto,  { preset: "rosewood",       decayMul: 2.4,  p: -0.08, gain: 0.55, params: FELT_SOFT, label: "quaternary (alto)" });
foldGroup(evSpark, { preset: "kalimba",        decayMul: 1.6,  p:  0.46, gain: 0.50, params: FELT_SOFT, label: "chord sparkle" });

// fractional delay read (linear interp) for the chorus lines.
function readDelay(buf, w, d, len) {
  let r = w - d; while (r < 0) r += len;
  const i0 = Math.floor(r), frac = r - i0;
  const a = buf[i0 % len], b2 = buf[(i0 + 1) % len];
  return a + (b2 - a) * frac;
}

// ═══════════════════════════════════════════════════════════════════════
//  CONTINUOUS DRONE BED — the lush "always something playing" layer.
// ═══════════════════════════════════════════════════════════════════════
// Sustained synths that NEVER stop, so the marimba rides a bed instead of
// dropping into silence — now deeper, richer, and moving:
//   • deep sub WUB   — a saw at the chord's bass octave through a resonant
//     LFO-swept lowpass (~0.13 Hz wobble) + a sub-octave sine for body;
//   • lush DRONE     — a deep-to-high 5-voice chord stack (root −2 oct … 3rd),
//     each voice 3 detuned sines (±6¢ chorus), low-weighted for depth,
//     warmed by a one-pole, then run through a PHASER (4-stage allpass with
//     feedback, slow sweep) and a stereo CHORUS (two modulated delays);
//   • AIR + brownian — soft filtered noise + two stereo random walks.
// All follow the chord walk via a one-pole glide — harmony moves, sound is
// unbroken from the 3 s sneak-in to the tail.
{
  let ck = 0;
  const droneFs = CHORDS[chordTimeline[0].ci].drone.map(midiToFreq);
  let subFs = midiToFreq(CHORDS[chordTimeline[0].ci].bass);
  const glide = Math.exp(-1 / (1.8 * SR));
  let sawPh = 0, subPh = 0, lfoPh = 0;
  let svfLow = 0, svfBand = 0;
  const Q = 0.42;
  const V = droneFs.length;                   // 5 drone voices
  const dPh = Array.from({ length: V }, () => [0, 0, 0]);
  const detune = [1.0, 1.0035, 0.9966];       // ±6¢ chorus spread per voice
  const vGain = [0.24, 0.24, 0.20, 0.17, 0.14];   // even spread — rolling bass owns the low end now
  let droneLp = 0, droneEnv = 0, fiberLp = 0;   // felting: env-follower + fuzz
  // phaser (4-stage allpass + feedback)
  const apX = [0, 0, 0, 0], apY = [0, 0, 0, 0];
  let phLfo = 0, phFb = 0;
  // stereo chorus delay lines
  const CL = Math.ceil(0.040 * SR);
  const chBuf = new Float32Array(CL);
  let cw = 0, chLfoA = 0, chLfoB = 0.27;
  let bL = 0, bR = 0, airLp = 0;
  const sneak = Math.floor(3 * SR);
  // sidechain pump — the low bed ducks on every walking-bass onset so the
  // bass + drone temper one another (they breathe together instead of piling).
  let ki = 0, lastBass = -1e9;
  const pumpTau = 0.10, pumpDepth = 0.45;
  for (let i = 0; i < ns; i++) {
    const t = i / SR;
    while (ck + 1 < chordTimeline.length && chordTimeline[ck + 1].t <= t) ck++;
    const tr = chordTimeline[ck].tr;
    const c = CHORDS[chordTimeline[ck].ci];
    const subTarget = midiToFreq(c.bass + tr);
    subFs = subTarget + (subFs - subTarget) * glide;
    for (let n = 0; n < V; n++) { const tg = midiToFreq(c.drone[n] + tr); droneFs[n] = tg + (droneFs[n] - tg) * glide; }
    const env = i < sneak ? i / sneak : 1;
    while (ki < bassOnsets.length && bassOnsets[ki] <= t) { lastBass = bassOnsets[ki]; ki++; }
    const pump = 1 - pumpDepth * Math.exp(-(t - lastBass) / pumpTau);   // dip on bass, recover

    // — deep sub wub —
    lfoPh += 0.13 / SR; if (lfoPh >= 1) lfoPh -= 1;
    const lfo = 0.5 - 0.5 * Math.cos(2 * Math.PI * lfoPh);
    sawPh += subFs / SR; if (sawPh >= 1) sawPh -= 1;
    const saw = 2 * sawPh - 1;
    const fc = 36 + 140 * lfo;                       // cutoff wobble = the wub
    const f = 2 * Math.sin(Math.PI * Math.min(fc, SR / 4) / SR);
    svfLow += f * svfBand;
    svfBand += f * (saw - svfLow - Q * svfBand);
    subPh += (subFs * 0.5) / SR; if (subPh >= 1) subPh -= 1;   // octave-below sub sine
    const sub = Math.sin(2 * Math.PI * subPh);
    const subSig = (svfLow * 0.95 + sub * 1.0) * 0.18 * (0.6 + 0.4 * lfo) * env * pump;

    // — lush drone: 5 voices × 3 detuned sines, WARBLY: a gentle ~5.5 Hz
    //   vibrato + a slow handmade drift, each voice on its own phase so the
    //   "woozies" beat against each other (felt is never glassy-perfect).
    let dryD = 0;
    for (let n = 0; n < V; n++) {
      const vib   = 0.006  * Math.sin(2 * Math.PI * 5.5 * t + n * 1.9);
      const drift = 0.0025 * (Math.sin(2 * Math.PI * 0.07 * t + n * 1.3)
                            + Math.sin(2 * Math.PI * 0.113 * t + n * 2.7));
      const base = droneFs[n] * (1 + vib + drift);
      let v = 0;
      for (let d = 0; d < 3; d++) {
        dPh[n][d] += (base * detune[d]) / SR; if (dPh[n][d] >= 1) dPh[n][d] -= 1;
        v += Math.sin(2 * Math.PI * dPh[n][d]);
      }
      const breath = 0.74 + 0.26 * Math.sin(2 * Math.PI * 0.04 * t + n * 1.7);
      dryD += (v / 3) * vGain[n] * breath;
    }
    // — FELTING the sines — bond a soft FIZZ of bandlimited noise that tracks
    //   the drone's own loudness (wool coming off the tone, not flat hiss),
    //   soft-saturate for matte warmth, then damp the highs the way felt
    //   absorbs them: soft like a sine, but fuzzy + woozy at the edges.
    droneEnv += 0.0015 * (Math.abs(dryD) - droneEnv);
    fiberLp += 0.30 * (noise() - fiberLp);           // ~2 kHz fuzz band
    const fizz = fiberLp * droneEnv * 1.0;
    let felt = Math.tanh((dryD + fizz) * 1.5) * 0.62;
    droneLp += 0.10 * (felt - droneLp);              // felt damping (matte)
    dryD = droneLp * 0.5 * env * (0.6 + 0.4 * pump); // gently pumped by the bass

    // — phaser (the "phasing") —
    phLfo += 0.07 / SR; if (phLfo >= 1) phLfo -= 1;
    const sweep = 0.5 - 0.5 * Math.cos(2 * Math.PI * phLfo);
    const g = 0.30 + 0.62 * sweep;                   // allpass coefficient sweep
    let s = dryD + phFb * 0.55;
    for (let k = 0; k < 4; k++) {
      const y = -g * s + apX[k] + g * apY[k];
      apX[k] = s; apY[k] = y; s = y;
    }
    phFb = s;
    const phased = dryD * 0.5 + s * 0.6;             // mix dry + phased

    // — stereo chorus (lush width) —
    chBuf[cw] = phased;
    const dlA = (0.012 + 0.006 * (0.5 - 0.5 * Math.cos(2 * Math.PI * chLfoA))) * SR;
    const dlB = (0.013 + 0.006 * (0.5 - 0.5 * Math.cos(2 * Math.PI * chLfoB))) * SR;
    const rdL = readDelay(chBuf, cw, dlA, CL);
    const rdR = readDelay(chBuf, cw, dlB, CL);
    cw = (cw + 1) % CL;
    chLfoA += 0.11 / SR; if (chLfoA >= 1) chLfoA -= 1;
    chLfoB += 0.093 / SR; if (chLfoB >= 1) chLfoB -= 1;

    // — air + brownian —
    bL = bL * 0.9994 + noise() * 0.0009;
    bR = bR * 0.9994 + noise() * 0.0009;
    airLp += 0.0016 * (noise() - airLp);
    const air = airLp * 0.014 * env;

    outL[i] += subSig + (phased * 0.5 + rdL * 0.5) + air + Math.max(-1, Math.min(1, bL)) * 0.02 * env;
    outR[i] += subSig + (phased * 0.5 + rdR * 0.5) + air + Math.max(-1, Math.min(1, bR)) * 0.02 * env;
  }
}

// ═══════════════════════════════════════════════════════════════════════
//  WHISTLE — the top-melody voice (MenuBand instrument 79 vibe): a near-pure
//  tone (sine + a faint 2nd/3rd partial) with breath noise, a fade-in vibrato,
//  a soft attack, and a tiny pitch scoop into each note. Washed by the reverb.
// ═══════════════════════════════════════════════════════════════════════
function whistle(t0, midi, dur, gain, p) {
  const f = midiToFreq(midi);
  const i0 = Math.floor(t0 * SR), n = Math.ceil((dur + 0.12) * SR);
  const a = (p + 1) * Math.PI / 4, lg = Math.cos(a), rg = Math.sin(a);
  const att = 0.045 * SR, rel = 0.10 * SR, hold = dur * SR;
  let ph = 0, brLp = 0;
  for (let i = 0; i < n; i++) {
    const dst = i0 + i; if (dst < 0 || dst >= ns) continue;
    const x = i / SR;
    // amplitude envelope: soft attack → hold → gentle release
    let amp = i < att ? i / att : i > hold ? Math.max(0, 1 - (i - hold) / rel) : 1;
    // vibrato fades in after ~0.25 s; tiny pitch scoop up into the note
    const vibDepth = 0.006 * Math.min(1, Math.max(0, (x - 0.25) / 0.4));
    const vib = 1 + vibDepth * Math.sin(2 * Math.PI * 5.4 * x);
    const scoop = 1 - 0.03 * Math.exp(-x / 0.05);
    ph += f * vib * scoop / SR; if (ph >= 1) ph -= 1;
    // near-pure tone + faint upper partials (whistle has a touch of 2nd/3rd)
    let s = Math.sin(2 * Math.PI * ph) + 0.10 * Math.sin(4 * Math.PI * ph) + 0.04 * Math.sin(6 * Math.PI * ph);
    // breath: a whisper of band-passed noise riding the envelope
    brLp += 0.4 * (noise() - brLp);
    s += brLp * 0.06;
    const o = s * amp * gain;
    outL[dst] += o * lg; outR[dst] += o * rg;
  }
}
{
  for (const e of whistleEvents) whistle(e.t, e.midi, e.dur, e.gain, e.p);
  console.log(`   · whistle melody (${whistleEvents.length}, instr-79 vibe)`);
}

// ═══════════════════════════════════════════════════════════════════════
//  REVERSO SINE BELLS — reverse-envelope swells: a soft sine (+ octave)
//  rises up out of the drone with a slow ease-in, then cuts at the peak —
//  the "sucked-in" reverse-bell ghost. Chord-tone, random pan/time, washed
//  by the reverb that follows. Added straight into outL/outR.
// ═══════════════════════════════════════════════════════════════════════
// chorded reverso bell — a swelling stack of soft sines (a whole chord),
// rising in with a slow ease then cutting at the peak.
function reversoBell(t0, midis, dur, gain, p) {
  const i0 = Math.floor(t0 * SR), nB = Math.ceil(dur * SR);
  const a = (p + 1) * Math.PI / 4, lg = Math.cos(a), rg = Math.sin(a);
  const fs = midis.map(midiToFreq);
  const ph = fs.map(() => 0), ph2 = fs.map(() => 0);
  for (let i = 0; i < nB; i++) {
    const dst = i0 + i; if (dst < 0 || dst >= ns) continue;
    const x = i / nB;
    const envB = x < 0.93 ? x * x : Math.max(0, 1 - (x - 0.93) / 0.07);  // swell then cut
    const vib = 1 + 0.004 * Math.sin(2 * Math.PI * 5 * (i / SR));
    let s = 0;
    for (let j = 0; j < fs.length; j++) {
      ph[j]  += fs[j] * vib / SR;     if (ph[j]  >= 1) ph[j]  -= 1;
      ph2[j] += fs[j] * 2 * vib / SR; if (ph2[j] >= 1) ph2[j] -= 1;
      s += Math.sin(2 * Math.PI * ph[j]) + 0.22 * Math.sin(2 * Math.PI * ph2[j]);
    }
    s = (s / fs.length) * envB * gain;
    outL[dst] += s * lg; outR[dst] += s * rg;
  }
}
{
  let count = 0;
  // crown a chord arrival every 4 bars: the bell swells up and PEAKS exactly
  // on that bar's downbeat, voiced with the chord it resolves INTO — so it
  // always lands consonant on the harmony instead of smearing across changes.
  for (let bi = 4; bi < chordTimeline.length; bi += 4) {
    const tgt = chordTimeline[bi];
    const dur = BAR * range(1.6, 2.2);          // swells across ~2 bars
    const start = tgt.t - dur * 0.93;           // envelope peaks (x≈0.93) on the downbeat
    if (start < 6) continue;
    const tri = CHORDS[tgt.ci].spark.map((m) => m - 12 + tgt.tr);   // the arriving chord's own tones
    reversoBell(start, tri, dur, range(0.12, 0.18), range(-0.55, 0.55));
    count++;
  }
  console.log(`   · reverso bells (${count}, peaking on chord arrivals)`);
}

// ═══════════════════════════════════════════════════════════════════════
//  LUSH STEREO REVERB — Schroeder (4 comb + 2 allpass / channel), ~0.16 wet.
//  Glues the marimba + drone into one soft, washed, lush space.
// ═══════════════════════════════════════════════════════════════════════
{
  const FB = 0.84, DAMP = 0.28, WET = 0.17;
  const mk = (len) => ({ buf: new Float32Array(len), i: 0, lp: 0 });
  const combsL = [1116, 1188, 1277, 1356].map(mk);
  const combsR = [1139, 1211, 1300, 1379].map(mk);
  const apsL = [556, 441].map(mk), apsR = [579, 464].map(mk);
  const comb = (c, x) => {
    const y = c.buf[c.i];
    c.lp = y * (1 - DAMP) + c.lp * DAMP;
    c.buf[c.i] = x + c.lp * FB;
    c.i = (c.i + 1) % c.buf.length;
    return y;
  };
  const allp = (c, x) => {
    const bv = c.buf[c.i];
    const y = -x + bv;
    c.buf[c.i] = x + bv * 0.5;
    c.i = (c.i + 1) % c.buf.length;
    return y;
  };
  for (let i = 0; i < ns; i++) {
    const xl = outL[i], xr = outR[i];
    let wl = 0, wr = 0;
    for (const c of combsL) wl += comb(c, xl);
    for (const c of combsR) wr += comb(c, xr);
    for (const c of apsL) wl = allp(c, wl);
    for (const c of apsR) wr = allp(c, wr);
    outL[i] = xl + wl * WET;
    outR[i] = xr + wr * WET;
  }
}

// ═══════════════════════════════════════════════════════════════════════
//  DYNAMIC ARC — bake the night narrative into the MUSIC level (not the rain,
//  added next): a smooth curve interpolated across the movement centres, so
//  the piece swells from the quiet drift up to the deep-dream climax (near the
//  golden section) and recedes to dawn. This is the dynamic range the flat
//  loudness was missing — the master is gentled (below) so it survives.
{
  const span = [];
  let accBar = 0;
  for (let i = 0; i < MOVE.length; i++) { span.push({ c: (accBar + MOVE[i].bars / 2) * BAR, lv: MOVE[i].level }); accBar += MOVE[i].bars; }
  const levelAt = (t) => {
    if (t <= span[0].c) return span[0].lv;
    if (t >= span[span.length - 1].c) return span[span.length - 1].lv;
    for (let i = 0; i < span.length - 1; i++) {
      if (t >= span[i].c && t <= span[i + 1].c) {
        const f = (t - span[i].c) / (span[i + 1].c - span[i].c);
        return span[i].lv + (span[i + 1].lv - span[i].lv) * f;
      }
    }
    return 1;
  };
  for (let i = 0; i < ns; i++) { const g = levelAt(i / SR); outL[i] *= g; outR[i] *= g; }
}

// ═══════════════════════════════════════════════════════════════════════
//  RAINY-JUNGLE OPENING — a real CC0 rain sample (Freesound #583932 "jungle
//  rain 02" by rucisko) loops seamlessly underneath, big and present at the
//  start then settling to a soft bed for the rest. Stereo-decorrelated.
// ═══════════════════════════════════════════════════════════════════════
{
  const rainPath = resolve(HERE, "..", "assets", "jungle-rain.wav");
  if (existsSync(rainPath)) {
    const { data: rain, sampleRate: rsr } = readWavMono(rainPath);
    const L = rain.length;
    const xf = Math.min(Math.floor(2 * rsr), Math.floor(L / 4));   // 2 s loop crossfade
    const rainAt = (j) => {
      const pos = ((j % L) + L) % L;
      let s = rain[pos];
      if (pos >= L - xf) { const f = (pos - (L - xf)) / xf; s = rain[pos] * (1 - f) + rain[pos - (L - xf)] * f; }
      return s;
    };
    const haas = Math.floor(0.017 * SR);                            // stereo width
    const ratio = rsr / SR;                                         // 1.0 here (44.1k)
    for (let i = 0; i < ns; i++) {
      const t = i / SR;
      // opening envelope: loud rain intro → settle to a soft constant bed.
      const g = t < 14 ? 0.50 : t < 45 ? 0.50 - (0.50 - 0.16) * ((t - 14) / 31) : 0.16;
      const j = i * ratio;
      outL[i] += rainAt(j) * g;
      outR[i] += rainAt(j - haas * ratio) * g;
    }
    console.log("   · rainy-jungle bed (assets/jungle-rain.wav · CC0 — see .sfx-credits.txt)");
  } else {
    console.warn("   ! jungle-rain.wav missing — no rain layer");
  }
}

// ── scrub + normalize ────────────────────────────────────────────────────
let nan = 0;
for (let i = 0; i < ns; i++) {
  if (!Number.isFinite(outL[i])) { outL[i] = 0; nan++; }
  if (!Number.isFinite(outR[i])) { outR[i] = 0; nan++; }
}
if (nan) console.warn(`   ! scrubbed ${nan} non-finite samples`);
let peak = 0;
for (let i = 0; i < ns; i++) { const a = Math.abs(outL[i]); if (a > peak) peak = a; const b = Math.abs(outR[i]); if (b > peak) peak = b; }
if (peak > 0) { const nrm = 0.82 / peak; for (let i = 0; i < ns; i++) { outL[i] *= nrm; outR[i] *= nrm; } }

// ── write f32 raw ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "moronbobasleep.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(ns * 2 * 4);
for (let i = 0; i < ns; i++) { b.writeFloatLE(outL[i], i * 8); b.writeFloatLE(outR[i], i * 8 + 4); }
writeFileSync(rawPath, b);

// ═══════════════════════════════════════════════════════════════════════
//  SLEEP MASTER — gentle, so the baked dynamic ARC survives: clean low end,
//  a touch of warmth + air, ONE soft, slow glue compressor (NOT a leveller —
//  a leveller would flatten the night narrative), long fade-in, then a
//  two-pass loudnorm to a calm -16 LUFS with a WIDE LRA (the arc lives in the
//  dynamics). Writes a 24-bit WAV master + a 320k mp3.
// ═══════════════════════════════════════════════════════════════════════
const TOTAL_DUR = 600.0, FADE_OUT = 14.0, FADE_IN = 8.0;
const PRE = [
  "highpass=f=22",
  "bass=g=3:f=85",                 // gentle low-shelf weight
  "highshelf=f=4200:g=-1.0",
  // one soft, slow glue compressor — cohesion without flattening the arc
  "acompressor=threshold=-24dB:ratio=2:attack=80:release=700:makeup=1.5:knee=6",
  "treble=g=0.8:f=9500",           // a whisper of air
  `afade=t=in:st=0:d=${FADE_IN}`,
  `afade=t=out:st=${TOTAL_DUR - FADE_OUT}:d=${FADE_OUT}`,
].join(",");
const LN = "loudnorm=I=-16:TP=-1:LRA=18";

// pass 1 — measure the EQ'd/compressed signal
console.log("[master] pass 1/2 — measuring loudness…");
const meas = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-loglevel", "info",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", `${PRE},${LN}:print_format=json`, "-to", String(TOTAL_DUR),
  "-f", "null", "-"], { encoding: "utf8" });
const jf = (re) => { const m = (meas.stderr || "").match(re); return m ? m[1] : null; };
const mI = jf(/"input_i"\s*:\s*"([^"]+)"/), mTP = jf(/"input_tp"\s*:\s*"([^"]+)"/),
      mLRA = jf(/"input_lra"\s*:\s*"([^"]+)"/), mThr = jf(/"input_thresh"\s*:\s*"([^"]+)"/);
const LN2 = (mI && mTP && mLRA && mThr)
  ? `${LN}:measured_I=${mI}:measured_TP=${mTP}:measured_LRA=${mLRA}:measured_thresh=${mThr}:linear=true`
  : LN;
if (LN2 === LN) console.warn("   ! couldn't parse pass-1 measurement — falling back to single-pass");

// pass 2 — apply → 24-bit WAV (release master) + 320k mp3
const shelf = resolve(homedir(), "Documents", "Shelf", "moronboba");
mkdirSync(shelf, { recursive: true });
const wavPath = resolve(shelf, "moronbobasleep-MASTER.wav");
console.log("[master] pass 2/2 — applying → WAV + mp3…");
const apply = (extra, dst) => spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", `${PRE},${LN2}`, "-to", String(TOTAL_DUR), ...extra, dst], { stdio: "inherit" });
const w = apply(["-ar", "44100", "-c:a", "pcm_s24le"], wavPath);
const ff = apply(["-ar", "44100", "-c:a", "libmp3lame", "-b:a", "320k"], outPath);
try { unlinkSync(rawPath); } catch {}
if (w.status !== 0 || ff.status !== 0) { console.error("✗ ffmpeg master failed"); process.exit(1); }
spawnSync("cp", [outPath, resolve(shelf, "moronbobasleep.mp3")]);
console.log(`✓ release master → ${wavPath.replace(homedir(), "~")}`);
console.log(`✓ ${outPath} (320k · two-pass · target -16 LUFS / -1 dBTP · dynamic arc)`);

// ── CLIP TEST — measure the final build's sample peak + true peak + LUFS ──
{
  const vd = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-loglevel", "info",
    "-i", wavPath, "-af", "volumedetect", "-f", "null", "-"], { encoding: "utf8" });
  const mm = (vd.stderr || "").match(/max_volume:\s*(-?[\d.]+) dB/);
  const maxDb = mm ? parseFloat(mm[1]) : NaN;
  const ld = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-loglevel", "info",
    "-i", wavPath, "-af", "loudnorm=print_format=summary", "-f", "null", "-"], { encoding: "utf8" });
  const tpm = (ld.stderr || "").match(/Input True Peak:\s*(-?[\d.]+|inf)/);
  const im = (ld.stderr || "").match(/Input Integrated:\s*(-?[\d.]+|inf)/);
  const clip = Number.isFinite(maxDb) && maxDb >= -0.05;
  console.log(`[clip-test] sample peak ${Number.isFinite(maxDb) ? maxDb.toFixed(2) : "?"} dBFS · ` +
    `true peak ${tpm ? tpm[1] : "?"} dBTP · integrated ${im ? im[1] : "?"} LUFS`);
  console.log(clip ? "   ✗ CLIPPING — sample peak ≥ -0.05 dBFS" : "   ✓ no clipping (headroom present)");
}

// ── struct.json — the chord walk, for any future visualizer ──────────────
{
  const struct = {
    _comment: "Chord map for moronbobasleep — the rolling marimbaba sleep mix. ~96 BPM 8th-note roll, F major + modal Gm/Eb (F/Dm/Bb/C/Gm/Eb walk). Master truncates the 10:10 render to 10:00. Derived from render-moronbobasleep.mjs.",
    bpm: BPM, scale: "major", rootMidi: 65, totalSec: TOTAL_DUR, renderSec: RENDER_SEC,
    chords: chordTimeline.map((c) => ({ name: CHORDS[c.ci].name, startSec: +c.t.toFixed(3), durSec: +c.dur.toFixed(3) })),
  };
  writeFileSync(resolve(HERE, "..", "out", "moronbobasleep.struct.json"), JSON.stringify(struct, null, 2) + "\n");
}
