#!/usr/bin/env node
// score-extract.mjs — dump momabobasheep's composed events as JSON for the
// graphic score. A verbatim copy of the COMPOSITION half of
// render-momabobasheep.mjs (same seed, same RNG call order — bit-identical
// event lists) with all audio rendering removed. Never imports or touches
// the renderer itself.
//
// Run:  node pop/momboba/bin/score-extract.mjs
// Out:  pop/momboba/out/momabobasheep.events.json

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));

const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };

const RENDER_SEC = 610;
const MASTER_SEC = 600;

// ── deterministic RNG (identical to the renderer) ────────────────────────
function seedFrom(str) {
  let h = 0x811c9dc5 >>> 0;
  for (let i = 0; i < str.length; i++) { h ^= str.charCodeAt(i); h = Math.imul(h, 0x01000193) >>> 0; }
  return h >>> 0;
}
// seed stays "mombobasleep" — the PRNG key for this exact walk (see renderer)
let _rng = seedFrom(_argi("--seed") || "mombobasleep");
function rnd() { _rng = (Math.imul(_rng, 1664525) + 1013904223) >>> 0; return _rng / 0xffffffff; }
const range = (a, b) => a + rnd() * (b - a);

const N = {
  Bb1: 34, C2: 36, D2: 38, F2: 41, A2: 45, Bb2: 46,
  C3: 48, D3: 50, F3: 53, G3: 55, A3: 57, Bb3: 58,
  C4: 60, D4: 62, E4: 64, F4: 65, G4: 67, A4: 69, Bb4: 70,
  C5: 72, D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, Bb5: 82,
  C6: 84, D6: 86,
};

const CHORDS = [
  { name: "F",  minor: false, bass: N.F2,  pad: [N.F4, N.A4, N.C5], spark: [N.F5, N.A5, N.C6], drone: [29, 41, 48, 53, 57], roll: [65, 69, 72, 74, 77, 81, 84, 86] },
  { name: "Dm", minor: true,  bass: N.D2,  pad: [N.D4, N.F4, N.A4], spark: [N.D5, N.F5, N.A5], drone: [26, 38, 45, 50, 53], roll: [62, 65, 69, 72, 74, 77, 81, 84] },
  { name: "Bb", minor: false, bass: N.Bb1, pad: [N.Bb3, N.D4, N.F4], spark: [N.Bb4, N.D5, N.F5], drone: [22, 34, 41, 46, 50], roll: [58, 62, 65, 67, 70, 74, 77, 82] },
  { name: "C",  minor: false, bass: N.C2,  pad: [N.C4, N.E4, N.G4], spark: [N.C5, N.E5, N.G5], drone: [24, 36, 43, 48, 52], roll: [60, 64, 67, 69, 72, 76, 79, 84] },
  { name: "Gm", minor: true,  bass: 43,    pad: [N.D4, N.G4, N.Bb4], spark: [N.G5, N.Bb5, N.D6], drone: [31, 43, 50, 55, 58], roll: [55, 58, 62, 67, 70, 74, 79, 82] },
  { name: "Eb", minor: false, bass: 39,    pad: [63, 67, 70],        spark: [75, 79, 82],        drone: [27, 39, 46, 51, 55], roll: [63, 67, 70, 75, 79, 82, 87] },
];

const evLead  = [];
const evBass  = [];
const evPad   = [];
const evAlto  = [];
const evSpark = [];

const ARP = 0.40;
const ARPN = 8;
const BAR = ARP * ARPN;

const PROGS = [
  [0, 1, 3, 2],
  [0, 2, 3, 1],
  [0, 1, 4, 3],
  [1, 4, 3, 0,  2, 0, 4, 3],
  [4, 3, 0, 1,  4, 3, 2, 0],
  [0, 4, 3, 1,  4, 5, 2, 3],
  [5, 2, 4, 5,  3, 0, 1, 4],
  [2, 3, 1, 0],
  [1, 0, 2, 0],
];
const MOVE_PROG = [0,   1,     2,     3,   4,    1,    5,    7,      8];

const FIB     = [3,    5,     8,     13,   21,    34,    55,    34,    13];
const MOVE_TR = [0,    0,     1,    -2,    2,    -1,     3,     0,     0];
const M_LEVEL = [0.38, 0.44,  0.55,  0.36, 0.62,  0.42,  0.74,  0.50,  0.38];
const M_WOCT  = [-24, -24,  -24,  -24,  -24,  -24,  -24, -24,  -24];
const MOVE = FIB.map((bars, i) => ({
  bars, tr: MOVE_TR[i], level: M_LEVEL[i], woct: M_WOCT[i], prog: PROGS[MOVE_PROG[i]],
  arp:     i >= 1 && i <= 7,
  arpDens: Math.min(1, 0.35 + 0.65 * M_LEVEL[i]),
  span:    M_LEVEL[i] > 0.55 ? 5 : 4,
  whistle: i >= 2 && i <= 7,
  alto:    i === 4 || i === 6,
  sparkle: M_LEVEL[i] > 0.55,
}));

const chordTimeline = [];
{
  let t = 0, prevCi = null, prevTr = 0;
  for (let mi = 0; mi < MOVE.length; mi++) {
    const mv = MOVE[mi];
    // PIVOT SEAMS — mirrors render-momabobasheep.mjs exactly
    let off = 0;
    if (prevCi != null) {
      const prevSet = CHORDS[prevCi].pad.map((m) => (m + prevTr + 120) % 12);
      const prevRoot = (CHORDS[prevCi].bass + prevTr + 120) % 12;
      let bestScore = -1e9;
      for (let o = 0; o < mv.prog.length; o++) {
        const set = CHORDS[mv.prog[o]].pad.map((m) => (m + mv.tr + 120) % 12);
        const common = set.filter((pc) => prevSet.includes(pc)).length;
        if (common === 3) continue;
        const root = (CHORDS[mv.prog[o]].bass + mv.tr + 120) % 12;
        let cf = 99;
        for (let n = -6; n <= 6; n++)
          if (((prevRoot + 7 * n) % 12 + 12) % 12 === root) cf = Math.min(cf, Math.abs(n));
        const score = common * 10 - cf;
        if (score > bestScore) { bestScore = score; off = o; }
      }
    }
    for (let k = 0; k < mv.bars; k++) {
      const ci = mv.prog[(k + off) % mv.prog.length];
      chordTimeline.push({ t, dur: BAR, ci, tr: mv.tr, mi, k });
      for (const m of CHORDS[ci].pad) evPad.push({ startSec: t, midi: m + mv.tr, durSec: BAR + 3, gain: range(0.05, 0.075) });
      t += BAR;
      prevCi = ci; prevTr = mv.tr;
    }
  }
  for (; t < MASTER_SEC + 2; t += BAR) {
    for (const m of CHORDS[0].pad) evPad.push({ startSec: t, midi: m, durSec: BAR + 3, gain: range(0.05, 0.075) });
  }
}

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

// ── whistle melody ────────────────────────────────────────────────────────
const melOnsets = [];
const whistleEvents = [];
{
  let idx = SCALE.indexOf(81);
  for (const { t, ci, tr, mi } of chordTimeline) {
    const mv = MOVE[mi];
    if (!mv.whistle) continue;
    const pcs = TRIAD[ci];
    for (let h = 0; h < 2; h++) {
      const tgt = nearestIdx(SCALE[idx], pcs, 7, SCALE.length - 1);
      if (idx < tgt) idx++; else if (idx > tgt) idx--;
      idx = Math.max(7, Math.min(SCALE.length - 1, idx));
      if (rnd() > 0.4 + 0.55 * mv.level) continue;
      const ts = t + h * (BAR / 2), dur = (BAR / 2) * range(0.96, 1.12);
      const top = SCALE[idx] + tr + mv.woct;
      whistleEvents.push({ t: ts, midi: top, dur, gain: 0.22 * range(0.95, 1.05), p: 0.14 });
      if (mv.level > 0.62) whistleEvents.push({ t: ts, midi: SCALE[Math.max(0, idx - 2)] + tr + mv.woct, dur: dur * 1.03, gain: 0.16, p: 0.26 });
      melOnsets.push(ts);
    }
  }
}

// ── alto ──────────────────────────────────────────────────────────────────
{
  let idx = 4;
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi, k } = chordTimeline[bi];
    if (!MOVE[mi].alto || t < 12) continue;
    const tgt = nearestIdx(SCALE[Math.min(idx, 6)], TRIAD[ci], 0, 6);
    if (idx < tgt) idx++; else if (idx > tgt) idx--;
    idx = Math.max(0, Math.min(6, idx));
    const rampIn = Math.min(1, (k + 1) / 4);
    evAlto.push({ startSec: t, midi: SCALE[idx] - 12 + tr, durSec: BAR * range(1.4, 1.7), gain: 0.12 * rampIn });
  }
}

// ── arpeggio ──────────────────────────────────────────────────────────────
const PATS = [
  [0, 1, 2, 3, 4, 3, 2, 1],
  [0, 2, 4, 2, 1, 3, 4, 2],
  [0, 1, 2, 4, 3, 2, 1, 0],
  [4, 3, 2, 1, 0, 1, 2, 3],
  [0, 2, 1, 3, 2, 4, 3, 1],
  [0, 4, 1, 3, 2, 4, 0, 2],
  [0, 1, 0, 2, 0, 3, 0, 4],
  [4, 2, 0, 2, 4, 3, 1, 3],
];
const nearMel = (ts) => { for (const m of melOnsets) if (Math.abs(m - ts) < 0.11) return true; return false; };
{
  let pidx = 0;
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    const mv = MOVE[mi];
    if (!mv.arp) continue;
    const tones = CHORDS[ci].roll.slice(0, Math.min(mv.span, CHORDS[ci].roll.length)).map((m) => m - 12 + tr);
    const sc = (v) => tones[Math.round((v / 4) * (tones.length - 1))];
    const Nn = ARPN, step = BAR / Nn;
    pidx = (pidx + 1 + (rnd() < 0.3 ? 1 : 0)) % PATS.length;
    const pat = PATS[pidx];
    for (let n = 0; n < Nn; n++) {
      if (n !== 0 && rnd() > mv.arpDens) continue;
      const ts = t + n * step;
      const accent = n === 0 ? 1.0 : (n === (Nn >> 1) ? 0.82 : 0.66);
      let g = 0.30 * accent * range(0.95, 1.05);
      if (nearMel(ts)) g *= 0.7;
      evLead.push({ startSec: ts, midi: sc(pat[n % pat.length]), durSec: step * range(2.6, 3.4), gain: g });
      if (n === 0) evLead.push({ startSec: t, midi: tones[Math.min(2, tones.length - 1)], durSec: ARP * 2.4, gain: g * 0.55 });
    }
  }
}

// ── walking bass ──────────────────────────────────────────────────────────
const bassOnsets = [];
{
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    if (!MOVE[mi].arp) continue;
    const root = CHORDS[ci].bass + tr;
    const ne = chordTimeline[(bi + 1) % chordTimeline.length];
    const next = CHORDS[ne.ci].bass + ne.tr;
    evBass.push({ startSec: t, midi: root, durSec: (BAR / 2) * 1.02, gain: 0.30 });
    const approach = next === root ? root + 7 : next - Math.sign(next - root) * 2;
    evBass.push({ startSec: t + BAR / 2, midi: approach, durSec: (BAR / 2) * 1.0, gain: 0.23 });
    bassOnsets.push(t, t + BAR / 2);
  }
}

// ── sparkle E(5,8) ────────────────────────────────────────────────────────
{
  let si = 0;
  for (const { t, ci, tr, mi, k } of chordTimeline) {
    if (!MOVE[mi].sparkle) continue;
    const onset = Math.floor(si * 5 / 8) !== Math.floor((si - 1) * 5 / 8);
    si++;
    if (!onset) continue;
    const sp = CHORDS[ci].spark;
    const rampIn = Math.min(1, (k + 1) / 4);
    evSpark.push({ startSec: t, midi: sp[sp.length - 1] - 12 + tr, durSec: BAR * 0.8, gain: range(0.06, 0.09) * rampIn });
  }
}

// ── reverso bells (range calls happen in this order in the renderer too —
//    bells are laid out after the marimba groups render, which uses no rnd) ──
const BELL_PEAK = 0.82;
const bellEvents = [];
{
  for (let bi = 4; bi < chordTimeline.length; bi += 4) {
    const tgt = chordTimeline[bi];
    const dur = BAR * range(1.6, 2.2);
    const start = tgt.t - dur * BELL_PEAK;
    if (start < 6) continue;
    const tri = CHORDS[tgt.ci].spark.map((m) => m - 12 + tgt.tr);
    bellEvents.push({ t: +start.toFixed(3), dur: +dur.toFixed(3), peakT: +tgt.t.toFixed(3), midis: tri, gain: +range(0.12, 0.18).toFixed(4), p: +range(-0.55, 0.55).toFixed(3) });
  }
}

// ── dynamic arc — the same interpolation the renderer bakes in ────────────
const span = [];
{
  let accBar = 0;
  for (let i = 0; i < MOVE.length; i++) { span.push({ c: (accBar + MOVE[i].bars / 2) * BAR, lv: MOVE[i].level }); accBar += MOVE[i].bars; }
}
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
const arc = [];
for (let t = 0; t <= MASTER_SEC; t += 1) arc.push(+levelAt(t).toFixed(4));

// drone/sub envelope: 3 s sneak-in, 20 s dissolve before the seam
const droneEnv = [];
for (let t = 0; t <= MASTER_SEC; t += 1) {
  const env = Math.min(1, t / 3) * (t > MASTER_SEC - 20 ? Math.max(0, (MASTER_SEC - t) / 20) : 1);
  droneEnv.push(+env.toFixed(4));
}

// ── movement metadata ─────────────────────────────────────────────────────
const CHAPTERS = ["dusk", "drowse", "descend", "deep", "dream", "still", "vivid", "morning", "resolve"];
const CHAPTER_SUB = [
  "drift", "settle", "sink", "deepening", "dreaming",
  "still trough", "THE DREAM", "toward morning", "resolve",
];
const PC = ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"];
const trName = (ci, tr) => PC[((CHORDS[ci].bass + tr) % 12 + 12) % 12] + (CHORDS[ci].minor ? "m" : "");

const movements = [];
{
  let t = 0;
  for (let i = 0; i < MOVE.length; i++) {
    const mv = MOVE[i];
    movements.push({
      name: CHAPTERS[i], sub: CHAPTER_SUB[i], bars: mv.bars, tr: mv.tr, level: mv.level,
      startSec: +t.toFixed(3), endSec: +(t + mv.bars * BAR).toFixed(3),
      prog: mv.prog.map((ci) => trName(ci, mv.tr)),
      arp: mv.arp, whistle: mv.whistle, alto: mv.alto, sparkle: mv.sparkle,
    });
    t += mv.bars * BAR;
  }
}

const chords = chordTimeline.map((c) => ({
  t: +c.t.toFixed(3), dur: +c.dur.toFixed(3), name: trName(c.ci, c.tr), base: CHORDS[c.ci].name, tr: c.tr, mi: c.mi,
}));

// ── jeffrey harmonies — MUST mirror render-momabobasheep.mjs rng-exactly ──
const jeffreyEvents = [];
{
  let _rj = seedFrom("jeffrey-harmonies");
  const rndJ = () => { _rj = (Math.imul(_rj, 1664525) + 1013904223) >>> 0; return _rj / 0xffffffff; };
  const CENTER = 47.2;
  for (let bi = 0; bi < chordTimeline.length; bi++) {
    const { t, ci, tr, mi } = chordTimeline[bi];
    const mv = MOVE[mi];
    if (rndJ() >= 0.35 + mv.level * 0.45) continue;
    const tj = t + rndJ() * 0.8;
    const cands = TRIAD[ci].flatMap((pc) => [pc + 36 + tr, pc + 48 + tr])
      .filter((m) => m >= 41 && m <= 54);
    let midi = cands[0];
    for (const m of cands) if (Math.abs(m - CENTER) < Math.abs(midi - CENTER)) midi = m;
    const dur = BAR * (1.6 + rndJ() * 1.1);
    rndJ();                                     // pan draw — kept for rng parity
    const lvl = levelAt(tj);                    // smooth — mirrors levelSmoothAt
    jeffreyEvents.push({ t: +tj.toFixed(3), dur: +dur.toFixed(3), midi, g: +(0.08 + lvl * 0.065).toFixed(4) });
    if (mv.alto && rndJ() < 0.5) {
      const ups = cands.filter((m) => m > midi + 2);
      if (ups.length) jeffreyEvents.push({ t: +(tj + 0.4).toFixed(3), dur: +(dur * 0.9).toFixed(3), midi: ups[0], g: +(0.06 + lvl * 0.05).toFixed(4) });
    }
  }
}

const out = {
  _comment: "Event dump for the momabobasheep graphic score — extracted from a verbatim copy of render-momabobasheep.mjs composition logic (seed: momabobasheep, bit-identical RNG walk). Times in seconds.",
  bar: BAR, masterSec: MASTER_SEC, renderSec: RENDER_SEC, totalBars: chordTimeline.length,
  movements, chords,
  goldenSec: +(MASTER_SEC * 0.618).toFixed(1),
  arp: evLead.map((e) => ({ t: +e.startSec.toFixed(3), midi: e.midi, dur: +e.durSec.toFixed(3), g: +e.gain.toFixed(4) })),
  bass: evBass.map((e) => ({ t: +e.startSec.toFixed(3), midi: e.midi, dur: +e.durSec.toFixed(3), g: +e.gain.toFixed(4) })),
  pad: evPad.map((e) => ({ t: +e.startSec.toFixed(3), midi: e.midi, dur: +e.durSec.toFixed(3), g: +e.gain.toFixed(4) })),
  alto: evAlto.map((e) => ({ t: +e.startSec.toFixed(3), midi: e.midi, dur: +e.durSec.toFixed(3), g: +e.gain.toFixed(4) })),
  sparkle: evSpark.map((e) => ({ t: +e.startSec.toFixed(3), midi: e.midi, dur: +e.durSec.toFixed(3), g: +e.gain.toFixed(4) })),
  whistle: whistleEvents.map((e) => ({ t: +e.t.toFixed(3), midi: e.midi, dur: +e.dur.toFixed(3), g: +e.gain.toFixed(4) })),
  bells: bellEvents,
  jeffrey: jeffreyEvents,
  arc, droneEnv,
};

const outPath = resolve(HERE, "..", "out", "momabobasheep.events.json");
writeFileSync(outPath, JSON.stringify(out) + "\n");
console.log(`✓ ${chordTimeline.length} bars · ${evLead.length} arp / ${evBass.length} bass / ${whistleEvents.length} whistle / ` +
  `${evAlto.length} alto / ${evSpark.length} sparkle / ${bellEvents.length} bells / ${evPad.length} pad → ${outPath}`);
