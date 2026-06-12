#!/usr/bin/env node
// render-momabobasheep.mjs — Mombobasleep, a 10-minute marimba sleep mix.
//
// The calm nocturne twin of pop/marimba/marimbaba. Where sleephellsine
// abandoned hellsine's instruments for pure sine, Mombobasleep keeps the
// actual marimba synth — that's what makes it a *marimbaba remix* and not a
// sleephellsine clone. The 80-second lullaby is dissolved into a slow,
// generative 10-minute drift: marimbaba's own phrase cells recur on a lazy
// random walk, very sparse, rung out to their full decay, over a held
// vibraphone pad, a slow rocking-chair bass, and kalimba twinkles. Every
// layer is pitched and musical — no field recordings, no noise beds.
//
// "Mom" = the lullaby rocked dozy and dumb-happy-slow. Stays home in F major
// (marimbaba's world: F / Dm / Bb / C, all diatonic), wandering between the
// chords every ~16-22 s, voice-led by the pad's long ring.
//
// FORM — one NIGHT OF SLEEP. The nine chapters model real sleep architecture:
// dusk → drowse → descend → deep sleep → a first dream → a still trough → the
// long vivid dream (the crest, at the golden section) → the drift toward
// morning → resolve. The energy ROTATES in slow waves (down-up-down-up-down)
// instead of one straight ramp — that rocking carries a listener under. Every
// figure stays at the lazy arpeggio pulse or slower: NO fast runs, no swoops,
// no triplet flourishes. The harmony circles away from home through the
// dreams and lands back on F at the end.
//
// LOOP — the final chapter dissolves to the same tonic pad that opens the
// piece (the drone eases out underneath it), so 10:00 meets 0:00 on the same
// soft F-major haze. Gentle master fades guard the seam — play it on repeat
// all night.
//
// Engine renders 10:10; the master truncates to 10:00 with gentle seam fades
// (the composed dissolve is the real out) and a measured LINEAR gain to
// -16 LUFS — no dynamic leveller, so the baked arc + seam survive.
//
// Run:
//   node pop/momboba/bin/render-momabobasheep.mjs
//   node pop/momboba/bin/render-momabobasheep.mjs --out ~/m.mp3
//   node pop/momboba/bin/render-momabobasheep.mjs --seed nightfall

import { mixEventMarimba } from "../../marimba/synths/marimba.mjs";
import { readWavMono } from "../../lib/wav.mjs";
import { pitchTrack } from "../../lib/analysis.mjs";
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
const RENDER_SEC = 610;         // 10:10 — master truncates to MASTER_SEC
const MASTER_SEC = 600;         // 10:00 — the LOOP seam lives here (end ≈ start)
const ns = Math.ceil(RENDER_SEC * SR);

function midiToFreq(m) { return 440 * Math.pow(2, (m - 69) / 12); }

// ── deterministic RNG (re-renders are bit-identical for a given seed) ───
function seedFrom(str) {
  let h = 0x811c9dc5 >>> 0;
  for (let i = 0; i < str.length; i++) { h ^= str.charCodeAt(i); h = Math.imul(h, 0x01000193) >>> 0; }
  return h >>> 0;
}
// the default seed stays "mombobasleep" (the track's former name) — it is
// only the PRNG key for THIS exact composed walk; renaming it would
// reshuffle the whole composition. The track title is momabobasheep.
let _rng = seedFrom(_argi("--seed") || "mombobasleep");
function rnd() { _rng = (Math.imul(_rng, 1664525) + 1013904223) >>> 0; return _rng / 0xffffffff; }
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
const evLead  = [];   // rosewood — the broken-chord arpeggio figuration
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
// NO chord ever repeats bar-to-bar (jas, 2026-06-11: "i dont want any
// REPEATS there") — including each progression's own cycle wrap. Resty
// chapters alternate home with its relative minor instead of doubling.
const PROGS = [
  [0, 1, 3, 2],                       // calm tonic drift (dusk)        F · Dm · C · Bb
  [0, 2, 3, 1],                       // plagal sway (settle/still)     F · Bb · C · Dm
  [0, 1, 4, 3],                       // ii–V begins (sink)             F · Dm · Gm · C
  [1, 4, 3, 0,  2, 0, 4, 3],          // fuller turn (deepen)           Dm Gm C F · Bb F Gm C
  [4, 3, 0, 1,  4, 3, 2, 0],          // dreaming                       Gm C F Dm · Gm C Bb F
  [0, 4, 3, 1,  4, 5, 2, 3],          // deep dream (Eb colour)         F Gm C Dm · Gm Eb Bb C
  [5, 2, 4, 5,  3, 0, 1, 4],          // strange REM (modal)            Eb Bb Gm Eb · C F Dm Gm
  [2, 3, 1, 0],                       // plagal homecoming (morning)    Bb · C · Dm · F
  [1, 0, 2, 0],                       // tonic rest (resolve)           Dm · F · Bb · F — ends Dm→F(tail)
];
//                dusk drowse descend deep dream still vivid morning resolve
const MOVE_PROG = [0,   1,     2,     3,   4,    1,    5,    7,      8];

// ── FORM: one NIGHT OF SLEEP in nine chapters. Sleep is cyclical — you sink
//    through light sleep into deep sleep, rise into a dream, sink again, dream
//    once more (longer, more vivid), then drift up toward morning. The level
//    curve traces those waves: two troughs, two dream crests, never loud. The
//    key ROTATES away from home through the dreams (tr 0 → ±little orbits) and
//    lands back on F for the last two chapters — the harmony genuinely
//    RESOLVES instead of floating off. Chapter lengths follow the Fibonacci
//    run, so the vivid dream crests near the golden section. The first and
//    last chapters are the same material (tonic pad alone) — the loop
//    seam: another night begins.
//          dusk  drowse descend deep  dream  still  vivid  morning resolve
const FIB     = [3,    5,     8,     13,   21,    34,    55,    34,    13];    // Σ=186 bars ≈ 9:55 — the vivid dream sits at the golden section
const MOVE_TR = [0,    0,     1,    -2,    2,    -1,     3,     0,     0];     // key rotation: orbits away in the dreams, lands home on F
const M_LEVEL = [0.38, 0.44,  0.55,  0.36, 0.62,  0.42,  0.74,  0.50,  0.38];  // sleep waves: two troughs, two dream crests — capped well below loud. Seam chapters lifted 0.30→0.38 (jas: the outro read as silence)
const M_WOCT  = [-24, -24,  -24,  -24,  -24,  -24,  -24, -24,  -24];           // whistle locked to Jeffrey's sung octave (B2..C4) — warm + low throughout
const MOVE = FIB.map((bars, i) => ({
  bars, tr: MOVE_TR[i], level: M_LEVEL[i], woct: M_WOCT[i], prog: PROGS[MOVE_PROG[i]],
  arp:     i >= 1 && i <= 7,                    // dusk + resolve are pad-only — the loop seam
  arpDens: Math.min(1, 0.35 + 0.65 * M_LEVEL[i]),  // sparser when quiet
  span:    M_LEVEL[i] > 0.55 ? 5 : 4,          // arpeggio kept tight + low (never climbs into the high marimba)
  whistle: i >= 2 && i <= 7,                    // the whistle sleeps through the seam chapters
  alto:    i === 4 || i === 6,                  // four-part harmony only inside the dreams
  sparkle: M_LEVEL[i] > 0.55,                   // high sparkle only at the dream crests
}));

// smooth chapter-level curve (interpolated across movement centres) — the
// dynamic-arc bake AND any per-event gain that should swell with the night
// use THIS instead of the stepped MOVE level, so chapter seams never lurch.
const _lvSpan = [];
{
  let accBar = 0;
  for (let i = 0; i < MOVE.length; i++) { _lvSpan.push({ c: (accBar + MOVE[i].bars / 2) * BAR, lv: MOVE[i].level }); accBar += MOVE[i].bars; }
}
function levelSmoothAt(t) {
  if (t <= _lvSpan[0].c) return _lvSpan[0].lv;
  if (t >= _lvSpan[_lvSpan.length - 1].c) return _lvSpan[_lvSpan.length - 1].lv;
  for (let i = 0; i < _lvSpan.length - 1; i++) {
    if (t >= _lvSpan[i].c && t <= _lvSpan[i + 1].c) {
      const f = (t - _lvSpan[i].c) / (_lvSpan[i + 1].c - _lvSpan[i].c);
      return _lvSpan[i].lv + (_lvSpan[i + 1].lv - _lvSpan[i].lv) * f;
    }
  }
  return 1;
}

const chordTimeline = [];   // { t, dur, ci, tr, mi, k: bar index within its movement }
{
  let t = 0, prevCi = null, prevTr = 0;
  for (let mi = 0; mi < MOVE.length; mi++) {
    const mv = MOVE[mi];
    // PIVOT SEAMS — enter each chapter on the chord sharing the MOST tones
    // with the chord we arrive from (tie-break: closest on the circle of
    // fifths; never the identical chord). Without this, key rotations made
    // cold cross-seam jumps like F → F# — the "uncanny" changes.
    let off = 0;
    if (prevCi != null) {
      const prevSet = CHORDS[prevCi].pad.map((m) => (m + prevTr + 120) % 12);
      const prevRoot = (CHORDS[prevCi].bass + prevTr + 120) % 12;
      let bestScore = -1e9;
      for (let o = 0; o < mv.prog.length; o++) {
        const set = CHORDS[mv.prog[o]].pad.map((m) => (m + mv.tr + 120) % 12);
        const common = set.filter((pc) => prevSet.includes(pc)).length;
        if (common === 3) continue;               // identical chord = repeat
        const root = (CHORDS[mv.prog[o]].bass + mv.tr + 120) % 12;
        let cf = 99;
        for (let n = -6; n <= 6; n++)
          if (((prevRoot + 7 * n) % 12 + 12) % 12 === root) cf = Math.min(cf, Math.abs(n));
        const score = common * 10 - cf;
        if (score > bestScore) { bestScore = score; off = o; }
      }
    }
    for (let k = 0; k < mv.bars; k++) {
      const ci = mv.prog[(k + off) % mv.prog.length];   // chapter walks its OWN progression, pivot-rotated
      chordTimeline.push({ t, dur: BAR, ci, tr: mv.tr, mi, k });
      for (const m of CHORDS[ci].pad) evPad.push({ startSec: t, midi: m + mv.tr, durSec: BAR + 3, gain: range(0.05, 0.075) });
      t += BAR;
      prevCi = ci; prevTr = mv.tr;
    }
  }
  // carry the pad ACROSS the loop seam — the timeline ends at ~595 s but the
  // master cuts at 600, so keep the resolve chord (F, home) blooming right up
  // to the cut. On loop, the restart's first pad bars pick the same F haze
  // back up: the same tonic pad on both sides of the seam.
  for (; t < MASTER_SEC + 2; t += BAR) {
    for (const m of CHORDS[0].pad) evPad.push({ startSec: t, midi: m, durSec: BAR + 3, gain: range(0.05, 0.075) });
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
      whistleEvents.push({ t: ts, midi: top, dur, gain: 0.22 * range(0.95, 1.05), p: 0.14 });
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
    const { t, ci, tr, mi, k } = chordTimeline[bi];
    if (!MOVE[mi].alto || t < 12) continue;
    const tgt = nearestIdx(SCALE[Math.min(idx, 6)], TRIAD[ci], 0, 6);
    if (idx < tgt) idx++; else if (idx > tgt) idx--;
    idx = Math.max(0, Math.min(6, idx));
    const rampIn = Math.min(1, (k + 1) / 4);     // fade the layer in across the chapter's first bars — no seam lurch
    evAlto.push({ startSec: t, midi: SCALE[idx] - 12 + tr, durSec: BAR * range(1.4, 1.7), gain: 0.12 * rampIn });
  }
}

// ── 3) the ARPEGGIO figuration — broken chord per bar (+ tr), ducked under
//      the whistle. The lazy 8th-note roll is the FASTEST thing in the piece —
//      no triplets, no swoops, nothing quicker than the rocking pulse.
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
const nearMel = (ts) => { for (const m of melOnsets) if (Math.abs(m - ts) < 0.11) return true; return false; };
{
  let pidx = 0;
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    const mv = MOVE[mi];
    if (!mv.arp) continue;                               // skip the seam chapters
    const tones = CHORDS[ci].roll.slice(0, Math.min(mv.span, CHORDS[ci].roll.length)).map((m) => m - 12 + tr);
    const sc = (v) => tones[Math.round((v / 4) * (tones.length - 1))];   // 0..4 → the chapter's range
    const Nn = ARPN, step = BAR / Nn;
    pidx = (pidx + 1 + (rnd() < 0.3 ? 1 : 0)) % PATS.length;   // walk the figuration library
    const pat = PATS[pidx];
    for (let n = 0; n < Nn; n++) {
      if (n !== 0 && rnd() > mv.arpDens) continue;        // thin the run when the chapter is quiet
      const ts = t + n * step;
      const accent = n === 0 ? 1.0 : (n === (Nn >> 1) ? 0.82 : 0.66);
      let g = 0.30 * accent * range(0.95, 1.05);
      if (nearMel(ts)) g *= 0.7;                          // duck under the whistle
      evLead.push({ startSec: ts, midi: sc(pat[n % pat.length]), durSec: step * range(2.6, 3.4), gain: g });   // long overlap = legato, not staccato
      if (n === 0) evLead.push({ startSec: t, midi: tones[Math.min(2, tones.length - 1)], durSec: ARP * 2.4, gain: g * 0.55 });
    }
  }
}

// ── 4) the WALKING BASS — root on the downbeat, stepwise approach tone into
//      the next bar's actual (transposed) root.
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
  }
}

// ── 5) chord SPARKLE on a EUCLIDEAN rhythm E(5,8) — evenly-spread pulses
//      (mathematical phrasing), only in the fuller chapters. + tr.
{
  let si = 0;
  for (const { t, ci, tr, mi, k } of chordTimeline) {
    if (!MOVE[mi].sparkle) continue;
    const onset = Math.floor(si * 5 / 8) !== Math.floor((si - 1) * 5 / 8);   // E(5,8)
    si++;
    if (!onset) continue;
    const sp = CHORDS[ci].spark;
    const rampIn = Math.min(1, (k + 1) / 4);     // fade in with the chapter
    // dropped an octave so the sparkle sits in a warmer register (was up at C6/A5)
    evSpark.push({ startSec: t, midi: sp[sp.length - 1] - 12 + tr, durSec: BAR * 0.8, gain: range(0.06, 0.09) * rampIn });
  }
}

console.log(`→ momabobasheep · seed=${_argi("--seed") || "mombobasleep"} · ${chordTimeline.length} bars · ${MOVE.length} movements · ` +
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

// decays tuned for LEGATO BLOOM — long, soft rings that overlap and smear into
// each other (NOT staccato): the tumbling notes melt together like a felt
// lullaby instead of stabbing. SOFT FELT MALLETS — a long mallet contact time
// low-passes the strike + a slow amplitude attack, so every note blooms in soft
// and thick like it's played with a felt-wrapped mallet (wide bell, no "ping").
const FELT_LEAD = { mallet: 0.016, attack: 0.014 };
const FELT_SOFT = { mallet: 0.012, attack: 0.010 };
// the bass is the LOWEST voice — it gets the slowest, softest mallet so each
// note swells in like a rocking chair, never thumps like a kick (the old
// 5 ms attack + 8 ms contact read as "a bit of a drum"; softened again
// same day — "softer ins and outs": 80 ms swell in, longer ring out)
const FELT_BASS = { mallet: 0.040, attack: 0.080 };
// mix placement — spread the voices across the field so each has room: bass +
// pad anchor the centre, arpeggio sits left, the inner alto centre-left, the
// sparkle right; the whistle lead (rendered below) lands centre-right.
foldGroup(evBass,  { preset: "bass",           decayMul: 1.6,  p:  0.00, gain: 0.92, params: FELT_BASS, label: "walking bass" });
foldGroup(evPad,   { preset: "vibraphone_off", decayMul: 3.0,  p:  0.00, gain: 0.38, haasMs: 14, params: FELT_SOFT, label: "pad bloom" });
foldGroup(evLead,  { preset: "rosewood",       decayMul: 2.8,  p: -0.26, gain: 0.50, params: FELT_LEAD, label: "arpeggio" });
foldGroup(evAlto,  { preset: "rosewood",       decayMul: 3.2,  p: -0.08, gain: 0.40, params: FELT_SOFT, label: "quaternary (alto)" });
foldGroup(evSpark, { preset: "kalimba",        decayMul: 2.0,  p:  0.46, gain: 0.30, params: FELT_SOFT, label: "chord sparkle" });

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
// dropping into silence — deep and still:
//   • deep SUB       — a saw at the chord's bass octave through a FIXED warm
//     lowpass + a sub-octave sine for body (the old LFO-swept "wub" is gone);
//   • lush DRONE     — a deep-to-high 5-voice chord stack (root −2 oct … 3rd),
//     each voice 3 detuned sines (±6¢ chorus), low-weighted for depth,
//     warmed by a one-pole, then a stereo CHORUS (two modulated delays).
// All follow the chord walk via a one-pole glide — harmony moves, sound is
// unbroken from the 3 s sneak-in to the tail. NO filter sweeps and NO noise
// texture anywhere in the bed (old air/brownian hiss, felting fizz, phaser
// sweep, and the wub's cutoff wobble are all removed); movement comes only
// from the chorus and each voice's own woozy vibrato/drift.
{
  let ck = 0;
  // lift any drone voice below ~B♭0+ (midi 26 ≈ 37 Hz) an octave — the
  // sub-35 Hz fundamentals only rattle speakers, they don't read as pitch
  const droneMidi = (m) => (m < 26 ? m + 12 : m);
  const droneFs = CHORDS[chordTimeline[0].ci].drone.map((m) => midiToFreq(droneMidi(m)));
  let subFs = midiToFreq(CHORDS[chordTimeline[0].ci].bass);
  const glide = Math.exp(-1 / (1.8 * SR));
  let sawPh = 0, subPh = 0;
  let svfLow = 0, svfBand = 0;
  const Q = 0.42;
  const V = droneFs.length;                   // 5 drone voices
  const dPh = Array.from({ length: V }, () => [0, 0, 0]);
  const detune = [1.0, 1.0035, 0.9966];       // ±6¢ chorus spread per voice
  const vGain = [0.24, 0.24, 0.20, 0.17, 0.14];   // even spread — rolling bass owns the low end now
  let droneLp = 0;                              // felt damping one-pole
  // stereo chorus delay lines
  const CL = Math.ceil(0.040 * SR);
  const chBuf = new Float32Array(CL);
  let cw = 0, chLfoA = 0, chLfoB = 0.27;
  const sneak = Math.floor(3 * SR);
  // (the old sidechain pump — a 45% duck of the bed on every bass onset —
  // is gone: its rhythmic dip-and-recover read as a kick drum. The bed and
  // bass now just coexist; the soft bass mallet keeps them from piling.)
  for (let i = 0; i < ns; i++) {
    const t = i / SR;
    while (ck + 1 < chordTimeline.length && chordTimeline[ck + 1].t <= t) ck++;
    const tr = chordTimeline[ck].tr;
    const c = CHORDS[chordTimeline[ck].ci];
    const subTarget = midiToFreq(c.bass + tr);
    subFs = subTarget + (subFs - subTarget) * glide;
    for (let n = 0; n < V; n++) { const tg = midiToFreq(droneMidi(c.drone[n] + tr)); droneFs[n] = tg + (droneFs[n] - tg) * glide; }
    // sneak in over 3 s; dissolve over the last 20 s before the loop seam so
    // the restart's own sneak-in doesn't pop (the pad alone carries the seam).
    const env = (i < sneak ? i / sneak : 1) *
      (t > MASTER_SEC - 20 ? Math.max(0, (MASTER_SEC - t) / 20) : 1);

    // — deep sub — fixed warm lowpass, no sweep (the wub's wobble is gone)
    sawPh += subFs / SR; if (sawPh >= 1) sawPh -= 1;
    const saw = 2 * sawPh - 1;
    const f = 2 * Math.sin(Math.PI * 90 / SR);       // fixed gentle cutoff
    svfLow += f * svfBand;
    svfBand += f * (saw - svfLow - Q * svfBand);
    // octave-below sub sine — but never below ~38 Hz (speaker-rattle band)
    const subHz = subFs * 0.5 >= 38 ? subFs * 0.5 : subFs;
    subPh += subHz / SR; if (subPh >= 1) subPh -= 1;
    const sub = Math.sin(2 * Math.PI * subPh);
    const subSig = (svfLow * 0.95 + sub * 1.0) * 0.18 * 0.8 * env;

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
    // — FELTING the sines — soft-saturate for matte warmth, then damp the
    //   highs the way felt absorbs them. (The noise-fizz texture and the
    //   4-stage phaser sweep that used to live here are removed — pure tone.)
    const felt = Math.tanh(dryD * 1.5) * 0.62;
    droneLp += 0.10 * (felt - droneLp);              // felt damping (matte)
    const phased = droneLp * 0.5 * env;

    // — stereo chorus (lush width) —
    chBuf[cw] = phased;
    const dlA = (0.012 + 0.006 * (0.5 - 0.5 * Math.cos(2 * Math.PI * chLfoA))) * SR;
    const dlB = (0.013 + 0.006 * (0.5 - 0.5 * Math.cos(2 * Math.PI * chLfoB))) * SR;
    const rdL = readDelay(chBuf, cw, dlA, CL);
    const rdR = readDelay(chBuf, cw, dlB, CL);
    cw = (cw + 1) % CL;
    chLfoA += 0.11 / SR; if (chLfoA >= 1) chLfoA -= 1;
    chLfoB += 0.093 / SR; if (chLfoB >= 1) chLfoB -= 1;

    outL[i] += subSig + (phased * 0.5 + rdL * 0.5);
    outR[i] += subSig + (phased * 0.5 + rdR * 0.5);
  }
}

// ═══════════════════════════════════════════════════════════════════════
//  WHISTLE — the top-melody voice (MenuBand instrument 79 vibe): a near-pure
//  tone (sine + a faint 2nd/3rd partial) with a fade-in vibrato, a soft
//  attack, and a tiny pitch scoop into each note. Washed by the reverb.
//  (Its breath-noise whisper was removed 2026-06-11 — the recurring "airy"
//  layer; the whistle is pure tone now, like everything else.)
// ═══════════════════════════════════════════════════════════════════════
function whistle(t0, midi, dur, gain, p) {
  const f = midiToFreq(midi);
  const i0 = Math.floor(t0 * SR), n = Math.ceil((dur + 0.45) * SR);
  const a = (p + 1) * Math.PI / 4, lg = Math.cos(a), rg = Math.sin(a);
  const att = 0.045 * SR, rel = 0.40 * SR, hold = dur * SR;   // long sighing release — no clipped note-ends
  let ph = 0;
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
    const s = Math.sin(2 * Math.PI * ph) + 0.10 * Math.sin(4 * Math.PI * ph) + 0.04 * Math.sin(6 * Math.PI * ph);
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
//  rises up out of the drone with a slow ease-in, peaks, then melts away on
//  a smooth cosine release — the reverse-bell ghost, with no hard cut.
//  Chord-tone, random pan/time, washed by the reverb that follows.
// ═══════════════════════════════════════════════════════════════════════
// chorded reverso bell — a swelling stack of soft sines (a whole chord),
// rising in with a slow ease, then released on a cosine (never clipped).
const BELL_PEAK = 0.82;            // envelope peaks here; the last 18% is release
function reversoBell(t0, midis, dur, gain, p) {
  const i0 = Math.floor(t0 * SR), nB = Math.ceil(dur * SR);
  const a = (p + 1) * Math.PI / 4, lg = Math.cos(a), rg = Math.sin(a);
  const fs = midis.map(midiToFreq);
  const ph = fs.map(() => 0), ph2 = fs.map(() => 0);
  for (let i = 0; i < nB; i++) {
    const dst = i0 + i; if (dst < 0 || dst >= ns) continue;
    const x = i / nB;
    const envB = x < BELL_PEAK
      ? (x / BELL_PEAK) * (x / BELL_PEAK)
      : 0.5 + 0.5 * Math.cos(Math.PI * (x - BELL_PEAK) / (1 - BELL_PEAK));  // swell → cosine melt
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
    const start = tgt.t - dur * BELL_PEAK;      // envelope peaks on the downbeat
    if (start < 6) continue;
    const tri = CHORDS[tgt.ci].spark.map((m) => m - 12 + tgt.tr);   // the arriving chord's own tones
    reversoBell(start, tri, dur, range(0.12, 0.18), range(-0.55, 0.55));
    count++;
  }
  console.log(`   · reverso bells (${count}, peaking on chord arrivals)`);
}

// ═══════════════════════════════════════════════════════════════════════
//  JEFFREY HARMONIES — sampled vowel tones (wave-wizard takes, see
//  bin/analyze-voice.mjs) repitched onto chord tones near his natural
//  register (≈B2) and laid under the pad as a slow choir. Uses its OWN
//  seeded rng stream so the rest of the composition stays bit-identical.
//  Take selection is deterministic (nearest pitch, ≤4 st shift) so the
//  event plan can be mirrored rng-exactly in score-extract.mjs.
// ═══════════════════════════════════════════════════════════════════════
const jeffreyEvents = [];
{
  let _rj = seedFrom("jeffrey-harmonies");
  const rndJ = () => { _rj = (Math.imul(_rj, 1664525) + 1013904223) >>> 0; return _rj / 0xffffffff; };
  const CENTER = 47.2;                          // his takes cluster at B2/C3
  // present the WHOLE track (jas), always breathing in and out: a candidate
  // entry EVERY bar, drifted off the grid, with tails long enough that
  // neighbouring entries overlap and crossfade — that grain of voices
  // arriving while others leave is the texture. Density rides the arc.
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    const mv = MOVE[mi];
    if (rndJ() >= 0.35 + mv.level * 0.45) continue;
    const tj = t + rndJ() * 0.8;                // off-grid drift
    const cands = TRIAD[ci].flatMap((pc) => [pc + 36 + tr, pc + 48 + tr])
      .filter((m) => m >= 41 && m <= 54);
    let midi = cands[0];
    for (const m of cands) if (Math.abs(m - CENTER) < Math.abs(midi - CENTER)) midi = m;
    const dur = BAR * (1.6 + rndJ() * 1.1);     // long tails → overlap
    const pan = -0.35 + rndJ() * 0.7;
    const lvl = levelSmoothAt(tj);              // swell with the night, never step at seams
    jeffreyEvents.push({ t: tj, dur, midi, g: 0.08 + lvl * 0.065, p: pan });
    if (mv.alto && rndJ() < 0.5) {              // dreams: a second voice above
      const ups = cands.filter((m) => m > midi + 2);
      if (ups.length) jeffreyEvents.push({ t: tj + 0.4, dur: dur * 0.9, midi: ups[0], g: 0.06 + lvl * 0.05, p: -pan });
    }
  }
}
{
  const manifestPath = resolve(HERE, "..", "voice", "manifest.json");
  if (!existsSync(manifestPath)) {
    console.log("   · jeffrey harmonies: no voice/manifest.json — skipped");
  } else {
    const REPO = resolve(HERE, "../../..");
    const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
    // ── takes → LOOPABLE SUSTAINS ──────────────────────────────────────
    // Trim each take to its longest contiguous VOICED run (key clicks are
    // unvoiced → cut), then loop-crossfade the core into a 16 s sustain so
    // any voice can hold for bars and be pitched far down (slowed =
    // stretched) without running out of tape.
    const SUS_SEC = 16;
    const loadSustain = (path) => {
      const { samples, sampleRate } = readWavMono(resolve(REPO, path));
      const frames = pitchTrack(samples, { sampleRate, fmin: 60, fmax: 500 });
      let best = [0, 0], cur = null;
      frames.forEach((r, i) => {
        if (r.hz != null && r.clarity > 0.55) {
          if (!cur) cur = [i, i]; else cur[1] = i;
          if (cur[1] - cur[0] > best[1] - best[0]) best = [...cur];
        } else cur = null;
      });
      const hop = Math.floor(0.010 * sampleRate);
      const s0 = Math.max(0, best[0] * hop - Math.floor(0.06 * sampleRate));
      const s1 = Math.min(samples.length, best[1] * hop + Math.floor(0.10 * sampleRate));
      const seg = Float32Array.from(samples.subarray(s0, s1));
      const xf = Math.floor(0.40 * sampleRate);                 // loop crossfade
      const ls = Math.floor(seg.length * 0.25), le = Math.floor(seg.length * 0.82);
      const total = Math.floor(SUS_SEC * sampleRate);
      const sus = new Float32Array(total);
      sus.set(seg.subarray(0, Math.min(le, total)), 0);
      let w = Math.min(le, total);
      while (w < total && le - ls > xf * 2) {
        for (let i = 0; i < xf; i++) {                           // equal-power seam
          const idx = w - xf + i; if (idx < 0 || idx >= total) continue;
          const fa = Math.cos((i / xf) * Math.PI / 2), fb = Math.sin((i / xf) * Math.PI / 2);
          sus[idx] = sus[idx] * fa + seg[ls + i] * fb;
        }
        const chunk = Math.min(le - ls - xf, total - w);
        sus.set(seg.subarray(ls + xf, ls + xf + chunk), w);
        w += chunk;
      }
      const fade = Math.floor(0.10 * sampleRate);                // soft head edge
      for (let i = 0; i < fade; i++) sus[i] *= 0.5 - 0.5 * Math.cos(Math.PI * i / fade);
      return { sus, sr: sampleRate };
    };
    const takes = manifest.map((m) => {
      const vib = loadSustain(m.file);
      // flat (vibrato-free) twin for LOW voices — vibrato at the walking
      // bass's own pitch beats against it (the 1:09 phase churn)
      const flat = m.flatFile ? loadSustain(m.flatFile) : vib;
      return { sus: vib.sus, susFlat: flat.sus, sr: vib.sr, midi: m.midi, vowel: m.vowel };
    });

    // ── CHORAL placement, deep in space ────────────────────────────────
    // Each planned entry becomes a small classical VOICING of the chord
    // under it: a dark octave-down BASS on the chord root, the planned
    // TENOR line with a detuned unison twin (choir blur), and on alternate
    // entries a soft UPPER voice on the next chord tone — wide spacing at
    // the bottom, close on top, entries gently staggered. Every voice is
    // looped, slow-enveloped (1.4 s in / 2.2 s out), one-pole darkened
    // (farther = duller) and rendered to a dedicated vocal bus that gets
    // its own LONG hall below — deeper in space than the instruments.
    const busL = new Float32Array(ns), busR = new Float32Array(ns);
    const chordAt = (t) => {
      let c = chordTimeline[0];
      for (const e of chordTimeline) { if (e.t <= t) c = e; else break; }
      return c;
    };
    const nearestTake = (target) => {
      let ti = 0, bdd = 1e9;
      takes.forEach((tk, i) => { const d = Math.abs(tk.midi - target); if (d < bdd) { bdd = d; ti = i; } });
      return takes[ti];
    };
    // opts.flat — use the vibrato-free lock (low voices); opts.glideFrom —
    // dynamic-autotune portamento: pitch eases from glideFrom to target
    // over ~0.35 s, like a singer sliding between melody notes.
    const voiceInto = (tk, target, t0, dur, gain, pan, fc, stagger, opts = {}) => {
      const buf = opts.flat ? tk.susFlat : tk.sus;
      const rate = Math.pow(2, (target - tk.midi) / 12) * (tk.sr / SR);
      const rate0 = opts.glideFrom != null
        ? Math.pow(2, (opts.glideFrom - tk.midi) / 12) * (tk.sr / SR) : rate;
      // never outrun the tape: clamp duration so the cosine release lands
      // INSIDE the sustain buffer — running off the end mid-envelope was
      // an instant cut at full volume (audible pops on fast up-shifts)
      const maxOut = Math.floor((buf.length - 2) / Math.max(rate, rate0));
      const nOut = Math.min(Math.floor(dur * SR), maxOut);
      if (nOut < SR * 0.5) return;
      const glideN = Math.min(Math.floor(0.35 * SR), Math.floor(nOut * 0.25));
      const att = Math.min(1.4 * SR, nOut * 0.4), rel = Math.min(2.2 * SR, nOut * 0.5);
      const i0 = Math.floor((t0 + stagger) * SR);
      const a = (pan + 1) * Math.PI / 4, lg = Math.cos(a), rg = Math.sin(a);
      const k = 1 - Math.exp(-2 * Math.PI * fc / SR);
      let pos = 0, lp = 0;
      for (let i = 0; i < nOut; i++) {
        const dst = i0 + i; if (dst >= ns) break;
        const r = i < glideN && rate0 !== rate
          ? rate0 + (rate - rate0) * (0.5 - 0.5 * Math.cos(Math.PI * i / glideN))
          : rate;
        if (dst < 0) { pos += r; continue; }
        const pi = Math.floor(pos), fr = pos - pi;
        if (pi + 1 >= buf.length) break;
        const s = buf[pi] * (1 - fr) + buf[pi + 1] * fr;
        lp += k * (s - lp);
        let env = i < att ? 0.5 - 0.5 * Math.cos(Math.PI * i / att) : 1;
        const rem = nOut - i;
        if (rem < rel) env *= 0.5 - 0.5 * Math.cos(Math.PI * rem / rel);
        const o = lp * env * gain;
        busL[dst] += o * lg; busR[dst] += o * rg;
        pos += r;
      }
    };
    // ── STAGED INTRODUCTION — each voice type debuts in its own chapter
    // of the night and fades up across that chapter's first bars, so the
    // choir assembles one voice at a time instead of arriving all at once
    // (the all-at-once open read as cacophonous):
    //   dusk: silence (the drift stays a drift) · drowse: TENOR ·
    //   descend: + detuned TWIN · deep: + BASS · dream: + UPPER ·
    //   vivid: + OCTAVE STRETCHES (full choir at the climax) ·
    //   resolve: thins back to tenor/twin/bass — the bookend.
    let placed = 0, voices = 0;
    jeffreyEvents.forEach((e, ei) => {
      const { ci, tr, mi, k } = chordAt(e.t);
      const deb = (m0) => mi < m0 ? 0 : mi === m0 ? Math.min(1, (k + 1) / 5) : 1;
      const wind = mi === 8 ? 0 : 1;              // resolve sheds the extras
      if (deb(1) === 0) return;                   // dusk: no choir at all
      if (ei % 3 === 2) return;                   // breathe — skip a third of
                                                  // the entries (it ran thick)
      const dur = e.dur * 1.1;                    // looped — linger, don't pile
      const pcs = TRIAD[ci];
      // never strain upward: a tenor target far above the takes drops an
      // octave instead (reads as another bass — choral, not chipmunk)
      let tenor = e.midi;
      if (tenor - nearestTake(tenor).midi > 3) tenor -= 12;
      voiceInto(nearestTake(tenor), tenor, e.t, dur, e.g * 0.55 * deb(1), e.p, 2400, 0);
      voices++;
      if (deb(2) > 0) {
        voiceInto(nearestTake(tenor), tenor + 0.13, e.t, dur * 0.96, e.g * 0.32 * deb(2),
          Math.max(-0.9, Math.min(0.9, e.p + 0.5)), 2000, 0.22);
        voices++;
      }
      // bass: chord root an octave down — but never below the walking
      // bass's own register (the choir stays inside the instruments' range).
      // FLAT lock: vibrato at the walking bass's pitch beats against it.
      if (deb(3) > 0) {
        let bRoot = pcs[0] + 36 + tr - 12;
        if (bRoot < 34) bRoot += 12;
        voiceInto(nearestTake(bRoot + 12), bRoot, e.t, dur, e.g * 0.38 * deb(3), -e.p * 0.5, 750, 0.35 + (ei % 3) * 0.1, { flat: true });
        voices++;
      }
      if (ei % 2 === 0 && deb(4) > 0 && wind) {   // upper voice, close position
        const ups = pcs.flatMap((pc) => [pc + 48 + tr, pc + 60 + tr])
          .filter((m) => m > tenor + 2 && m <= Math.min(tenor + 9, 53)).sort((x, y) => x - y);
        if (ups.length) { voiceInto(nearestTake(ups[0]), ups[0], e.t, dur * 0.9, e.g * 0.30 * deb(4), -e.p, 1700, 0.5); voices++; }
      }
      // OCTAVE STRETCHES — climax-only colours: a 2-octave-down ghost
      // (slowed = his voice stretched like taffy, flat lock, very dark)
      // and an up-octave shimmer (airy, barely there)
      if (ei % 5 === 1 && deb(6) > 0 && wind) {
        const deep = tenor - 24;
        if (deep >= 29) { voiceInto(nearestTake(tenor), deep, e.t, dur * 1.3, e.g * 0.40 * deb(6), e.p * 0.3, 500, 0.6, { flat: true }); voices++; }
      }
      if (ei % 7 === 3 && deb(6) > 0 && wind) {
        const high = tenor + 12;
        if (high <= 60) { voiceInto(nearestTake(tenor), high, e.t, dur * 0.8, e.g * 0.20 * deb(6), -e.p * 0.8, 2800, 0.8); voices++; }
      }
      placed++;
    });

    // ── MELODY SHADOW — his voice follows the whistle line ──────────────
    // Every few whistle phrases, a vocal doubles the melody an octave
    // down into his register, GLIDING from the previous shadow note to
    // the new one (dynamic autotune portamento) — the choir stops being
    // only a pad and starts singing the tune.
    {
      let prevMidi = null, shadows = 0;
      whistleEvents.forEach((e, wi) => {
        if (wi % 4 !== 0) return;
        const smi = chordAt(e.t).mi;              // staged intro: the shadow
        if (smi < 4 || smi > 7) return;           // debuts in the dream
        const lvl = levelSmoothAt(e.t);
        if (lvl < 0.40) return;
        let m = e.midi;
        while (m > 53) m -= 12;
        while (m < 41) m += 12;
        voiceInto(nearestTake(m), m, e.t, Math.max(e.dur * 2.2, 3.0), 0.05 + lvl * 0.05,
          0.22, 2200, 0.05, { glideFrom: prevMidi ?? m });
        prevMidi = m;
        shadows++; voices++;
      });
      console.log(`   · melody shadow (${shadows} glided notes)`);
    }

    // ── SIDECHAIN to the walking bass — the whole choir breathes ────────
    // A gentle duck (35%, ~0.3 s recovery) on every bass onset: the
    // marimba speaks, the voices lean back, then swell into the gap —
    // movement and space, slow enough that it never reads as a club pump.
    {
      const onsets = evBass.map((e) => e.startSec).sort((a, b) => a - b);
      let oi = 0, last = -1e9, duckLp = 1;
      const depth = 0.42, tau = 0.30;             // carve a little more air
      // the duck target is slewed through a ~12 ms one-pole — an instant
      // gain step on a loud sustained voice clicks on every onset (it
      // read as "lots of clipping"); the recovery was already smooth
      const aCoef = 1 - Math.exp(-1 / (0.012 * SR));
      for (let i = 0; i < ns; i++) {
        const t = i / SR;
        while (oi < onsets.length && onsets[oi] <= t) { last = onsets[oi]; oi++; }
        const duck = 1 - depth * Math.exp(-(t - last) / tau);
        duckLp += (duck - duckLp) * aCoef;
        busL[i] *= duckLp; busR[i] *= duckLp;
      }
    }

    // ── the vocal hall — a second, longer Schroeder just for the choir ──
    // (the main mix reverb below still washes over the sum)
    {
      const FB = 0.85, DAMP = 0.44, WET = 0.45, DRY = 0.30;   // shorter, drier
                                                              // hall — depth
                                                              // without the murk
      const mk = (len) => ({ buf: new Float32Array(len), i: 0, lp: 0 });
      const combsL = [1687, 1781, 1933, 2053].map(mk);
      const combsR = [1723, 1823, 1979, 2087].map(mk);
      const apsL = [841, 667].map(mk), apsR = [877, 701].map(mk);
      const comb = (c, x) => { const y = c.buf[c.i]; c.lp = y * (1 - DAMP) + c.lp * DAMP; c.buf[c.i] = x + c.lp * FB; c.i = (c.i + 1) % c.buf.length; return y; };
      const allp = (c, x) => { const bv = c.buf[c.i]; const y = -x + bv; c.buf[c.i] = x + bv * 0.5; c.i = (c.i + 1) % c.buf.length; return y; };
      for (let i = 0; i < ns; i++) {
        let wl = 0, wr = 0;
        for (const c of combsL) wl += comb(c, busL[i]);
        for (const c of combsR) wr += comb(c, busR[i]);
        for (const c of apsL) wl = allp(c, wl);
        for (const c of apsR) wr = allp(c, wr);
        outL[i] += busL[i] * DRY + wl * WET;
        outR[i] += busR[i] * DRY + wr * WET;
      }
    }
    console.log(`   · jeffrey harmonies (${placed} entries → ${voices} choir voices · looped sustains · deep-space hall)`);
  }
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
//  DYNAMIC ARC — bake the night narrative into the MUSIC level:
//  a smooth curve interpolated across the movement centres, so
//  the piece swells from the quiet drift up to the deep-dream climax (near the
//  golden section) and recedes to dawn. This is the dynamic range the flat
//  loudness was missing — the master is gentled (below) so it survives.
{
  for (let i = 0; i < ns; i++) { const g = levelSmoothAt(i / SR); outL[i] *= g; outR[i] *= g; }
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
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "momabobasheep.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(ns * 2 * 4);
for (let i = 0; i < ns; i++) { b.writeFloatLE(outL[i], i * 8); b.writeFloatLE(outR[i], i * 8 + 4); }
writeFileSync(rawPath, b);

// ═══════════════════════════════════════════════════════════════════════
//  SLEEP MASTER — gentle, so the baked dynamic ARC survives: clean low end,
//  a touch of warmth + air, ONE soft, slow glue compressor (NOT a leveller —
//  a leveller would flatten the night narrative), then a measured LINEAR gain
//  to a calm -16 LUFS / -1 dBTP (the arc lives in the dynamics). Gentle seam
//  fades (the composed pad/drone dissolve is the real out) guarantee the file
//  never ends on a hard cut. Writes a 24-bit WAV master + a 320k mp3.
// ═══════════════════════════════════════════════════════════════════════
const TOTAL_DUR = MASTER_SEC, FADE_OUT = 5, FADE_IN = 2;   // soft breath at the seam — over pad-only chapters, so nothing is gouged
// 2004-CD master (jas: "dynamic, toasty, like a CD in 2004"): tight MONO
// lows (the haas/chorus spread was leaking width below 150 Hz — phase-
// coherent LR crossover at 120, side collapsed), a warm 240 Hz body bell,
// the same single slow glue comp (dynamics stay linear, the arc lives),
// and a CD-era −13.5 LUFS instead of the streaming-quiet −16.
const TARGET_LUFS = -13.5;
const CHAIN = (vol) => [
  "[0:a]highpass=f=32,acrossover=split=120:order=4th[lo][hi]",
  "[lo]pan=stereo|c0=.5*c0+.5*c1|c1=.5*c0+.5*c1[lom]",            // bass mono
  "[lom][hi]amix=inputs=2:normalize=0," + [
    "bass=g=3:f=85",               // low-shelf weight
    "equalizer=f=240:t=q:w=1.0:g=1.5",   // the TOAST — warm low-mid body
    "highshelf=f=3200:g=-3.5",     // tame the bright marimba attack/ring
    // one soft, slow glue compressor — cohesion without flattening the arc
    "acompressor=threshold=-24dB:ratio=2:attack=80:release=700:makeup=1.5:knee=6",
    "treble=g=-1.0:f=8000",        // matte, but keep a little CD sheen
    `afade=t=in:st=0:d=${FADE_IN}`,
    `afade=t=out:st=${TOTAL_DUR - FADE_OUT}:d=${FADE_OUT}`,
    ...(vol ? [vol] : []),
  ].join(",") + "[m]",
].join(";");
// pass 1 — measure the EQ'd/compressed signal (loudnorm used as a METER only)
console.log("[master] pass 1/2 — measuring loudness…");
const meas = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-loglevel", "info",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-filter_complex", CHAIN("loudnorm=print_format=json"), "-map", "[m]", "-to", String(TOTAL_DUR),
  "-f", "null", "-"], { encoding: "utf8" });
const jf = (re) => { const m = (meas.stderr || "").match(re); return m ? m[1] : null; };
const mI = jf(/"input_i"\s*:\s*"([^"]+)"/), mTP = jf(/"input_tp"\s*:\s*"([^"]+)"/);
// LINEAR GAIN master — never let loudnorm APPLY the normalization: its linear
// mode silently reverts to DYNAMIC when the measured LRA beats the target,
// and dynamic mode rode the loop-seam tail down ~12 dB (erasing the
// composed end-swell). A plain volume gain — loudness up to -16 LUFS, clamped
// so true peak stays under -1 dBTP — preserves the baked dynamic arc and the
// pad seam exactly as composed.
let gainDb = 0;
if (mI && mTP) gainDb = Math.min(TARGET_LUFS - parseFloat(mI), -1 - parseFloat(mTP));
else console.warn("   ! couldn't parse pass-1 measurement — applying unity gain");
const VOL = `volume=${gainDb.toFixed(2)}dB`;
console.log(`   · linear gain ${gainDb >= 0 ? "+" : ""}${gainDb.toFixed(2)} dB (measured ${mI ?? "?"} LUFS / ${mTP ?? "?"} dBTP)`);

// pass 2 — apply → 24-bit WAV (release master) + 320k mp3
const shelf = resolve(homedir(), "Documents", "Shelf", "momboba");
mkdirSync(shelf, { recursive: true });
const wavPath = resolve(shelf, "momabobasheep-MASTER.wav");
console.log("[master] pass 2/2 — applying → WAV + mp3…");
const apply = (extra, dst) => spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-filter_complex", CHAIN(VOL), "-map", "[m]", "-to", String(TOTAL_DUR), ...extra, dst], { stdio: "inherit" });
const w = apply(["-ar", "44100", "-c:a", "pcm_s24le"], wavPath);
const ff = apply(["-ar", "44100", "-c:a", "libmp3lame", "-b:a", "320k"], outPath);
try { unlinkSync(rawPath); } catch {}
if (w.status !== 0 || ff.status !== 0) { console.error("✗ ffmpeg master failed"); process.exit(1); }
spawnSync("cp", [outPath, resolve(shelf, "momabobasheep.mp3")]);
console.log(`✓ release master → ${wavPath.replace(homedir(), "~")}`);
console.log(`✓ ${outPath} (320k · measured linear gain · target ${TARGET_LUFS} LUFS / -1 dBTP · 2004-CD toasty · mono lows · dynamic arc + loop seam intact)`);

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
    _comment: "Chord map for momabobasheep — the rolling marimbaba sleep mix. ~96 BPM 8th-note roll, F major + modal Gm/Eb (F/Dm/Bb/C/Gm/Eb walk). Nine sleep-cycle chapters resolving home to F; the master truncates the 10:10 render to 10:00 and the end dissolves to the opening tonic pad, so the file loops gently. Derived from render-momabobasheep.mjs.",
    bpm: BPM, scale: "major", rootMidi: 65, totalSec: TOTAL_DUR, renderSec: RENDER_SEC,
    chords: chordTimeline.map((c) => ({ name: CHORDS[c.ci].name, startSec: +c.t.toFixed(3), durSec: +c.dur.toFixed(3) })),
  };
  writeFileSync(resolve(HERE, "..", "out", "momabobasheep.struct.json"), JSON.stringify(struct, null, 2) + "\n");
}
