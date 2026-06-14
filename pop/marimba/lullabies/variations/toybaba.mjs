// toybaba.mjs — a TOY BOX TIPPING OVER: the marimbaba nursery riff chopped
// into tiny plastic cells and scrambled with cheeky non-sequiturs.
//
// STRATEGY — FRAGMENTATION & INTERPOLATION. We keep toybaba's identity (C major
// pentatonic, glockenspiel + xylophone "toy piano" plink, soft woodblock
// tick-tock, bouncy ~76 BPM 3/4, cozy nursery reverb) but the MELODY no longer
// restates the tune faithfully. Instead TWINKLE and BABA are diced into 2–3
// note cells and recombined: a cell answered an octave too high, a sudden
// "wrong-but-cute" interpolated note, a hiccup (repeated stutter), a cheeky
// woodblock pitch answering back, a fragment quoted backwards, a fragment in
// the bass. Playful musical non-sequiturs that always resolve home — a toy box
// tipping over, plinking everything out, then settling.
//
// FORM (development arc):
//   intro    bars 0-1   : hush sigh, glassy + high (the recognizable thread)
//   spill    bars 2-9   : TWINKLE & BABA shattered into cells, scrambled,
//                         octave-displaced, with wrong-note interpolations,
//                         hiccups, and woodblock answers (the box tips over)
//   chase    bars 10-15 : cells sequenced up by step + retrograde quotes +
//                         a stutter accelerando (toys rolling across the floor)
//   reassemble bars 16-21: the fragments snap back toward the real tune —
//                         twinkle head re-forms, baba cadence lands
//   outro    bars 22-25 : sleep settle, low & calm (lullaby rest)
//
// Run:  node variations/toybaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 76;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // marimbaba 3/4 lilt

// ── C major pentatonic: root C, scale {0,2,4,7,9} = C D E G A ────────────────
const ROOT_PC = m("C4") % 12; // 0
const PENTA = [0, 2, 4, 7, 9];

function snap(midi, rootPc = ROOT_PC, scale = PENTA) {
  let best = midi, bestD = Infinity;
  for (let oct = -1; oct <= 1; oct++) {
    for (const deg of scale) {
      const pc = (rootPc + deg) % 12;
      const base = Math.round((midi - pc) / 12) * 12 + pc + oct * 12;
      const d = Math.abs(base - midi);
      if (d < bestD - 1e-6 || (Math.abs(d - bestD) < 1e-6 && base < best)) {
        best = base; bestD = d;
      }
    }
  }
  return best;
}

// ── cell helpers — every cell is an array of [noteName, beats] tuples ─────────
// We work in MIDI-tuples internally so transpose/invert/retrograde compose.
const cellMidi = (cell) => cell.map(([n, b]) => [snap(m(n)), b]);
const tuneMidi = (mc) => mc; // identity, for readability

// pentatonic-degree arithmetic: move a midi note by N scale steps (not semis),
// so sequencing keeps the toy-piano in-key without half-step clashes.
function pentaStep(midi, steps) {
  // find current degree index
  const pc = ((midi % 12) + 12) % 12;
  let idx = PENTA.indexOf(((pc - ROOT_PC) % 12 + 12) % 12);
  if (idx < 0) idx = 0;
  let oct = Math.floor((midi - (ROOT_PC + PENTA[idx])) / 12);
  let n = idx + steps;
  oct += Math.floor(n / PENTA.length);
  n = ((n % PENTA.length) + PENTA.length) % PENTA.length;
  return ROOT_PC + PENTA[n] + 12 * oct + 12; // +12: snap() world centers near C5
}

const stepCell = (mc, steps) => mc.map(([mi, b]) => [pentaStep(mi, steps), b]);
const octCell = (mc, n) => mc.map(([mi, b]) => [mi + 12 * n, b]);
const retro = (mc) => [...mc].reverse();
// mirror intervals around the cell's first note, then re-snap to scale.
function invertCell(mc) {
  if (!mc.length) return mc;
  const pivot = mc[0][0];
  return mc.map(([mi, b]) => [snap(2 * pivot - mi), b]);
}
// stretch / squeeze durations
const stretchCell = (mc, k) => mc.map(([mi, b]) => [mi, b * k]);

// ── fragment library: tiny cells diced out of TWINKLE and BABA ───────────────
const TW = cellMidi(MOTIFS.twinkle); // F5 A5 C6 A5 G5  -> snapped to penta
const BA = cellMidi(MOTIFS.baba);    // A5 G5 A5 F5 C6 Bb5 C6 A5
const HU = cellMidi(MOTIFS.hush);
const SL = cellMidi(MOTIFS.sleep);

// twinkle cells
const tw_head = TW.slice(0, 2);          // climb: deg-pair (the head)
const tw_peak = TW.slice(2, 4);          // peak + fall
const tw_tail = TW.slice(4);             // resting tone
// baba cells
const ba_hop = BA.slice(0, 3);           // ba-ba-ba bounce
const ba_dip = BA.slice(2, 5);           // turn down then up
const ba_top = BA.slice(4, 7);           // upper wobble
const ba_cad = BA.slice(6);              // landing

const events = [];

// ── lay a cell of [midi,beats] tuples into the toy-piano voice ───────────────
function lay(startBar, startBeat, mc, opts = {}) {
  const {
    preset = "glockenspiel",
    altPreset = null,
    octaveShift = 0,
    gain = 0.4,
    pan = 0,
    stretch = 1.0,
    bounce = 0,
    decayBoost = 1.35,
  } = opts;
  let bar = startBar, beat = startBeat, i = 0;
  for (const [mi, beats] of mc) {
    const useAlt = altPreset && (i % 2 === 1);
    const pr = useAlt ? altPreset : preset;
    const g = gain * (1 + (i % 2 === 0 ? bounce : -bounce));
    events.push({
      preset: pr,
      startSec: bar * BAR + beat * BEAT,
      midi: mi + octaveShift,
      durSec: beats * stretch * BEAT,
      gain: g,
      decayMul: (DECAY[pr] ?? 1.1) * decayBoost,
      pan,
    });
    beat += beats * stretch; i += 1;
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
}

// a single cheeky/wrong note — the interpolation surprise (always cute, in key)
function poke(bar, beat, note, opts = {}) {
  const { preset = "glockenspiel", gain = 0.3, pan = 0.3, beats = 0.5, octaveShift = 0 } = opts;
  events.push({
    preset,
    startSec: bar * BAR + beat * BEAT,
    midi: snap(m(note)) + octaveShift,
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY[preset] ?? 1.1) * 1.3,
    pan,
  });
}

// a soft woodblock tick — the wind-up pulse
function tick(bar, beat, gain = 0.16, note = "C5", pan = -0.28, beats = 0.5) {
  events.push({
    preset: "woodblock",
    startSec: bar * BAR + beat * BEAT,
    midi: snap(m(note)),
    durSec: beats * BEAT,
    gain,
    decayMul: 0.7,
    pan,
  });
}

// a plucky toy bass root
function bass(bar, note = "C3", gain = 0.32, beats = 3, beat = 0) {
  events.push({
    preset: "bass",
    startSec: bar * BAR + beat * BEAT,
    midi: snap(m(note)),
    durSec: beats * BEAT,
    gain,
    decayMul: 1.5,
    pan: 0,
  });
}

// ════════════════════════════════════════════════════════════════════════════
// INTRO (bars 0-1) — the recognizable thread: hush sigh, glassy + high.
// ════════════════════════════════════════════════════════════════════════════
lay(0, 0, octCell(HU, 1), { preset: "glockenspiel", gain: 0.3, pan: 0.1, stretch: 0.9 });
bass(0, "C2", 0.26); bass(1, "C2", 0.24);
// one early wrong-but-cute poke that foreshadows the spill
poke(1, 2, "D6", { gain: 0.2, pan: -0.3 });

// ════════════════════════════════════════════════════════════════════════════
// SPILL (bars 2-9) — TWINKLE & BABA shattered: cells scrambled, octave-jumped,
// wrong-note interpolations, hiccups, and woodblock answers. The box tips over.
// ════════════════════════════════════════════════════════════════════════════

// bar 2: twinkle head states normally (so we still hear the tune start)...
lay(2, 0, tw_head, { preset: "xylophone", altPreset: "glockenspiel", gain: 0.46, pan: -0.12, bounce: 0.12 });
// ...then HICCUP: the peak cell stutters (repeat its first note) before resolving
lay(2, 1, [tw_peak[0], tw_peak[0], ...tw_peak.slice(1)].map(([mi]) => [mi, 0.4]),
  { preset: "glockenspiel", altPreset: "xylophone", gain: 0.4, pan: 0.18, bounce: 0.14 });
poke(2, 2.6, "A5", { gain: 0.24, pan: 0.34 }); // wrong-but-cute extra plink

// bar 3: tw_tail quoted an octave TOO HIGH (non-sequitur), woodblock answers low
lay(3, 0, octCell(tw_tail, 1), { preset: "glockenspiel", gain: 0.34, pan: 0.22, stretch: 0.6 });
lay(3, 0.7, stepCell(tw_head, 2), { preset: "xylophone", gain: 0.36, pan: -0.2, bounce: 0.1 }); // head sequenced UP
tick(3, 1.8, 0.2, "C4", 0.32); // cheeky low woodblock "answer" to the high quote

// bar 4-5: baba hop fragment, then the SAME hop inverted (mirror) right after
lay(4, 0, ba_hop, { preset: "xylophone", altPreset: "glockenspiel", gain: 0.46, pan: 0.02, bounce: 0.14 });
lay(4, 1.5, invertCell(ba_hop), { preset: "glockenspiel", altPreset: "xylophone", gain: 0.4, pan: -0.18, bounce: 0.12 });
poke(4, 2.6, "G5", { preset: "xylophone", gain: 0.26, pan: 0.3 });
lay(5, 0, ba_top, { preset: "glockenspiel", altPreset: "xylophone", octaveShift: 0, gain: 0.42, pan: 0.16, bounce: 0.12 });
// hiccup: cad note repeated twice fast then lands
lay(5, 1.5, [ba_cad[0], ba_cad[0]].map(([mi]) => [mi, 0.4]).concat([ba_cad[0]]),
  { preset: "xylophone", gain: 0.36, pan: -0.1 });

// bar 6-7: a fragment quoted in the WRONG octave (bass voice plinks the toy tune!)
lay(6, 0, octCell(tw_head, -1), { preset: "bass", gain: 0.3, pan: 0, decayBoost: 1.0 });
lay(6, 1, octCell(ba_dip, 1), { preset: "glockenspiel", gain: 0.34, pan: 0.24, stretch: 0.7, bounce: 0.1 });
tick(6, 2.5, 0.18, "E5", 0.28);
lay(7, 0, ba_dip, { preset: "xylophone", altPreset: "glockenspiel", gain: 0.44, pan: -0.06, bounce: 0.13 });
poke(7, 2, "A5", { gain: 0.26, pan: 0.32 });
poke(7, 2.5, "C6", { gain: 0.22, pan: -0.3 }); // little double-poke fill

// bar 8-9: scramble — tw_peak then ba_hop retrograde, woodblock cross-talk
lay(8, 0, tw_peak, { preset: "glockenspiel", altPreset: "xylophone", gain: 0.42, pan: 0.14, bounce: 0.12 });
lay(8, 1.5, retro(ba_hop), { preset: "xylophone", gain: 0.4, pan: -0.16, bounce: 0.12 });
tick(8, 2.4, 0.16, "G5", 0.3);
lay(9, 0, octCell(tw_tail, 1), { preset: "glockenspiel", gain: 0.3, pan: 0.2, stretch: 0.8 });
lay(9, 1, stepCell(ba_hop, 1), { preset: "xylophone", gain: 0.36, pan: -0.18, bounce: 0.1 });

// ════════════════════════════════════════════════════════════════════════════
// CHASE (bars 10-15) — cells sequenced UP by step + retrograde quotes + a
// stutter accelerando. Toys rolling across the floor.
// ════════════════════════════════════════════════════════════════════════════
// sequence the twinkle head up by step each bar (a rising staircase)
for (let k = 0; k < 3; k++) {
  const bar = 10 + k;
  lay(bar, 0, stepCell(tw_head, k), { preset: "xylophone", altPreset: "glockenspiel", gain: 0.42 - k * 0.02, pan: -0.1 + k * 0.12, bounce: 0.13 });
  lay(bar, 1, stepCell(retro(tw_head), k + 1), { preset: "glockenspiel", gain: 0.34, pan: 0.2 - k * 0.1, stretch: 0.7 });
  poke(bar, 2.5, k % 2 ? "C6" : "A5", { gain: 0.22, pan: k % 2 ? 0.3 : -0.3 });
}

// bar 13: a STUTTER ACCELERANDO — baba hop diminished into a fast tumble,
// notes packed tighter and tighter (the toy clatters down).
{
  const stut = ba_hop.concat(ba_top, ba_cad).map(([mi]) => mi); // 9 pitches
  let beat = 0, dur = 0.4;
  for (let i = 0; i < stut.length; i++) {
    poke(13, beat, "C5", { preset: i % 2 ? "xylophone" : "glockenspiel", gain: 0.3, pan: (i % 2 ? 0.22 : -0.22), beats: dur });
    // overwrite that poke's pitch with the real fragment pitch:
    events[events.length - 1].midi = stut[i];
    beat += dur;
    dur = Math.max(0.16, dur * 0.86); // accelerate
    if (beat >= 3) break;
  }
}

// bar 14-15: cells answered in call-and-response between L glock and R xylo
lay(14, 0, ba_top, { preset: "glockenspiel", gain: 0.42, pan: -0.22, bounce: 0.12 });
lay(14, 1.5, octCell(ba_top, 1), { preset: "xylophone", gain: 0.34, pan: 0.26, stretch: 0.7 });
lay(15, 0, invertCell(tw_peak), { preset: "xylophone", gain: 0.4, pan: -0.14, bounce: 0.12 });
lay(15, 1.5, tw_peak, { preset: "glockenspiel", gain: 0.4, pan: 0.2, bounce: 0.1 });
poke(15, 2.6, "D6", { gain: 0.2, pan: -0.32 });

// ════════════════════════════════════════════════════════════════════════════
// REASSEMBLE (bars 16-21) — the fragments snap back toward the real tune.
// ════════════════════════════════════════════════════════════════════════════
// twinkle head re-forms, mostly whole now, with just one cheeky poke left
lay(16, 0, [...tw_head, ...tw_peak], { preset: "xylophone", altPreset: "glockenspiel", gain: 0.46, pan: -0.1, bounce: 0.12 });
lay(17, 0, tw_tail, { preset: "glockenspiel", gain: 0.4, pan: 0.12, stretch: 1.0 });
poke(17, 1.5, "A5", { preset: "xylophone", gain: 0.24, pan: 0.3 }); // last little hiccup
// baba played nearly whole — the tune found again
lay(18, 0, [...ba_hop, ...ba_dip.slice(1)], { preset: "xylophone", altPreset: "glockenspiel", gain: 0.46, pan: 0, bounce: 0.12 });
lay(19, 0, [...ba_top, ...ba_cad], { preset: "glockenspiel", altPreset: "xylophone", gain: 0.42, pan: 0.14, bounce: 0.1 });
// twinkle head one more time, higher and tender (the box winding down)
lay(20, 0, octCell([...tw_head, ...tw_peak], 1), { preset: "glockenspiel", gain: 0.32, pan: 0.16, stretch: 1.05 });
lay(21, 0, stretchCell(tw_tail, 1.15), { preset: "glockenspiel", gain: 0.3, pan: -0.08, stretch: 1.1 });

// ════════════════════════════════════════════════════════════════════════════
// OUTRO (bars 22-26) — sleep settle, low & calm, slowing down.
// ════════════════════════════════════════════════════════════════════════════
lay(22, 0, SL, { preset: "glockenspiel", gain: 0.3, pan: -0.06, stretch: 1.2 });
// a last, slow, glassy echo of the twinkle head — winding fully down
lay(25, 0, stretchCell(tw_head, 1.5), { preset: "glockenspiel", gain: 0.26, pan: 0.12, stretch: 1.2 });
lay(26, 0, stretchCell(tw_tail, 1.4), { preset: "glockenspiel", gain: 0.22, pan: -0.04, stretch: 1.3 });
poke(26, 1.6, "C4", { gain: 0.16, pan: 0, beats: 2 }); // one soft low bell to close

// ── the wind-up woodblock pulse under the active middle (soft tick-tock) ─────
for (let bar = 2; bar <= 19; bar++) {
  tick(bar, 0, 0.16, "G5", -0.3);
  tick(bar, 1, 0.1, "C5", 0.26);
  tick(bar, 2, 0.12, "E5", -0.22);
}

// ── plucky toy bass walking gently under the tune (C-pentatonic roots) ───────
const bassPlan = [
  [2, "C2"], [3, "C3"], [4, "A2"], [5, "C2"], [6, "G2"], [7, "C3"],
  [8, "C2"], [9, "A2"], [10, "G2"], [11, "C2"], [12, "A2"], [13, "G2"],
  [14, "C2"], [15, "C3"], [16, "A2"], [17, "C2"], [18, "G2"], [19, "C3"],
  [20, "A2"], [21, "G2"], [22, "C2"], [23, "C2"], [25, "A2"], [26, "C2"],
];
for (const [bar, note] of bassPlan) bass(bar, note, 0.3);

const { mp3, durationSec } = renderLullaby(events, {
  name: "toybaba",
  here: HERE,
  title: "toybaba",
  reverb: { wet: 0.32, decay: 0.8, damp: 0.42 },
  fadeIn: 0.5,
  fadeOut: 3.5,
  tailSec: 4.0,
  peak: 0.84,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
