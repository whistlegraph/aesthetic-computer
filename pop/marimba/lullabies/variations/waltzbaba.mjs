// waltzbaba.mjs — a waltz that trips over itself.
//
// marimbaba is in 3/4, sleepily. waltzbaba keeps the F-major key, the rosewood
// lead, the ~66 BPM spin and the warm marimba mood — but it DRAMATICALLY develops
// the melody through HEMIOLA & METRIC MODULATION. The dance keeps slipping its
// footing: 3-against-2 and 2-against-3 cross-rhythms superimpose, accents migrate
// so the bar-line seems to walk, the tune briefly *feels* in duple (a stomping
// 2/4) then snaps back into 3/4, dizzy and reeling. The recognizable threads —
// the hush descent, the slinky baba wobble, the F-major cadence — survive, but
// they travel through a meter that won't hold still.
//
// ARC:
//   A  (0-5)   plain 3/4 statement — establish the waltz & the hush thread.
//   B  (6-11)  hemiola I: lead phrases in groups of 2 over the 3/4 bass (3:2).
//   C  (12-17) metric modulation: the dotted-quarter pulse becomes the new beat;
//              the dance briefly stomps in 2/4, baba diminished into fast runs.
//   D  (18-23) hemiola II: 2-against-3 the other way — bass implies duple, lead
//              keeps spinning in 3 — accents collide, then resolve.
//   E  (24-29) recombination: fragments of every meter overlap (stretto) and
//              the bar-line dissolves before the F-major cadence pulls it home.
//
// Run:  node variations/waltzbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 66;
const BEAT = 60 / BPM;          // quarter-note in 3/4
const BAR = 3 * BEAT;           // a 3/4 bar
const SWING = 0.10;

// ── F major ──
const ROOT_PC = m("F4") % 12;
const MAJOR = [0, 2, 4, 5, 7, 9, 11];
function snap(midi, rootPc = ROOT_PC, scale = MAJOR) {
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

// ── little melodic-development helpers (arrays of [note, beats]) ──────────────
const toMidi = (cell) => cell.map(([n, b]) => [m(n), b]);
const fromMidi = (cell) => cell; // we work in midi after toMidi

function transpose(cell, semis) { return cell.map(([n, b]) => [n + semis, b]); }
function invert(cell, axisMidi) {
  return cell.map(([n, b]) => [axisMidi + (axisMidi - n), b]);
}
function retrograde(cell) { return [...cell].reverse(); }
function diminish(cell, factor) { return cell.map(([n, b]) => [n, b / factor]); }
function augment(cell, factor) { return cell.map(([n, b]) => [n, b * factor]); }
function sequence(cell, steps) {
  // steps: array of semitone offsets; concatenate transposed copies.
  return steps.flatMap((s) => transpose(cell, s));
}

// emit a melodic cell (midi pitches, beats in BEAT units) into events, starting
// at absolute time t0, using `beatUnit` seconds per beat (lets us metrically
// modulate — pass a different unit to redefine the pulse). Returns end time.
function emit(events, cell, t0, {
  preset = "rosewood",
  beatUnit = BEAT,
  gain = 0.5,
  octave = 0,
  pan = 0,
  decayMul = 1.2,
  swing = 0,
  legato = 1.0,
  accentEvery = 0,     // accent index period (0 = none)
  accentBoost = 1.35,
} = {}) {
  let t = t0, i = 0;
  for (const [midi, beats] of cell) {
    const dur = beats * beatUnit;
    const frac = ((t - t0) / beatUnit) % 1;
    const sw = (frac > 0.4 && frac < 0.6) ? swing * beatUnit : 0;
    const accent = accentEvery && (i % accentEvery === 0) ? accentBoost : 1;
    events.push({
      preset,
      startSec: t + sw,
      midi: snap(midi) + octave * 12,
      durSec: dur * legato,
      gain: gain * accent,
      decayMul: (DECAY[preset] ?? 1.6) * decayMul,
      pan,
    });
    t += dur; i += 1;
  }
  return t;
}

const events = [];

// motif source cells in midi.
const HUSH = toMidi(MOTIFS.hush);       // C5 A4 F4 / F4
const TWINKLE = toMidi(MOTIFS.twinkle);
const BABA = toMidi(MOTIFS.baba);       // slinky wobble
const WOW = toMidi(MOTIFS.wow);
const SLEEP = toMidi(MOTIFS.sleep);

// ════════════════════════════════════════════════════════════════════════════
// THE WALTZ ENGINE — oom-pah-pah, but with shiftable accent so the bar can walk.
// chordMode: "waltz" (root-stab-stab), "hemiola" (accent every 2 beats),
//            "duple" (modulated 2/4 stomp), "implied2" (bass groups of 2 over 3).
// ════════════════════════════════════════════════════════════════════════════
const CH = {
  F:  ["F2", ["A4", "C5"]],
  C:  ["C3", ["E4", "G4"]],
  Bb: ["Bb2", ["D4", "F4"]],
  Dm: ["D3", ["F4", "A4"]],
  Gm: ["G2", ["Bb4", "D5"]],
  Eb: ["Eb2", ["G4", "Bb4"]], // borrowed colour around the baba
};

function oom(t, bassNote, gain = 0.42, dur = 1.0 * BEAT) {
  events.push({
    preset: "bass", startSec: t, midi: m(bassNote), durSec: dur,
    gain, decayMul: (DECAY.bass ?? 1.8), pan: 0,
  });
}
function stab(t, note, gain = 0.16, pan = 0, dur = 0.5 * BEAT) {
  events.push({
    preset: "staccato", startSec: t, midi: snap(m(note)), durSec: dur,
    gain, decayMul: 0.9, pan,
  });
}

// plain waltz bar: oom (b1) pah (b2) pah (b3)
function barWaltz(bar, chord, g = 1) {
  const [bn, [a, b]] = CH[chord];
  const t = bar * BAR;
  oom(t + 0 * BEAT, bn, 0.42 * g);
  stab(t + 1 * BEAT, a, 0.15 * g, -0.16);
  stab(t + 1 * BEAT, b, 0.15 * g, -0.16);
  stab(t + 2 * BEAT, a, 0.15 * g, 0.16);
  stab(t + 2 * BEAT, b, 0.15 * g, 0.16);
}

// hemiola bar (3:2): accent the *halves* of the bar — booms at beats 0 and 1.5,
// stabs filling — so the ear hears two strong pulses across a 3/4 bar.
function barHemiola2over3(bar, chord, g = 1) {
  const [bn, [a, b]] = CH[chord];
  const t = bar * BAR;
  oom(t + 0 * BEAT, bn, 0.42 * g);
  stab(t + 0.75 * BEAT, a, 0.12 * g, -0.2);
  oom(t + 1.5 * BEAT, bn, 0.38 * g, 0.8 * BEAT); // second "downbeat" mid-bar
  stab(t + 2.25 * BEAT, b, 0.12 * g, 0.2);
}

// 3-against-2 bar: bass keeps 3, but a counter-stab line lands every two beats
// across pairs of bars — handled at the span level (see spanHemiola3over2).

// duple stomp bar (post metric-modulation): the new beat = dotted-quarter of the
// old. We just place two heavy booms per old-bar, splitting it into 2.
function barDuple(bar, chord, g = 1) {
  const [bn, [a, b]] = CH[chord];
  const t = bar * BAR;
  const half = 1.5 * BEAT;
  oom(t + 0, bn, 0.44 * g, half * 0.9);
  stab(t + 0.5 * BEAT, a, 0.13 * g, -0.18);
  oom(t + half, bn, 0.40 * g, half * 0.9);
  stab(t + half + 0.5 * BEAT, b, 0.13 * g, 0.18);
}

// implied-2 bass over 3/4: booms every 2 beats, drifting against the bar line
// (resets every 3 bars = 9 beats vs 2-beat groups: classic walking accent).
function spanImplied2(startBar, nBars, chordSeq, g = 1) {
  const t0 = startBar * BAR;
  const totalBeats = nBars * 3;
  let chordIdx = 0;
  for (let bt = 0; bt < totalBeats; bt += 2) {
    const ch = chordSeq[chordIdx % chordSeq.length]; chordIdx++;
    const [bn] = CH[ch];
    oom(t0 + bt * BEAT, bn, 0.4 * g, 1.4 * BEAT);
  }
  // soft 3/4 ghost stabs underneath to keep the friction audible
  for (let bar = 0; bar < nBars; bar++) {
    const ch = chordSeq[bar % chordSeq.length];
    const [, [a, b]] = CH[ch];
    const t = t0 + bar * BAR;
    stab(t + 1 * BEAT, a, 0.09 * g, -0.24);
    stab(t + 2 * BEAT, b, 0.09 * g, 0.24);
  }
}

// ════════════════════════════════════════════════════════════════════════════
// SECTION A (bars 0-5): plain 3/4 — establish the waltz and the hush thread.
// ════════════════════════════════════════════════════════════════════════════
{
  const prog = ["F", "F", "Dm", "Bb", "C", "F"];
  prog.forEach((c, i) => barWaltz(i, c));
  // lead: the hush sigh (recognizable), answered by a spinning curtsy of baba.
  let t = 0 * BAR;
  t = emit(events, HUSH, t, { gain: 0.52, decayMul: 1.25, swing: SWING });
  // a calm baba statement, plain, as the "theme" we'll deform.
  emit(events, BABA, 2 * BAR, { gain: 0.5, pan: -0.05, decayMul: 1.2, swing: SWING });
  // a high kalimba shimmer answering at the cadence
  emit(events, transpose(HUSH, 12), 4 * BAR, {
    preset: "kalimba", gain: 0.2, pan: 0.32, decayMul: 1.3,
  });
}

// ════════════════════════════════════════════════════════════════════════════
// SECTION B (bars 6-11): HEMIOLA I — lead phrases in groups of TWO over the 3/4.
// The bass stays oom-pah-pah; the melody is re-rhythmed so every note is a
// half-note triplet-feel of 2 beats, walking across the bar line. Accent every
// 2 lead-notes to make the duple grouping pop against the triple bass.
// ════════════════════════════════════════════════════════════════════════════
{
  const prog = ["F", "C", "Dm", "Bb", "C", "F"];
  prog.forEach((c, i) => barWaltz(6 + i, c, 0.85));
  // also lay hemiola booms on bars 8-9 so the bass itself starts to slip
  barHemiola2over3(8, "Dm", 1.0);
  barHemiola2over3(9, "Bb", 1.0);

  // Lead: take TWINKLE, re-rhythm so each pitch lasts 2 beats (a 3:2 hemiola:
  // 3 melody notes span 2 bars). Sequence it up a step then resolve.
  const twHemiola = TWINKLE.map(([n]) => [n, 2]); // all duple
  let t = 6 * BAR;
  t = emit(events, twHemiola, t, {
    gain: 0.5, decayMul: 1.2, accentEvery: 1, accentBoost: 1.25,
  });
  // a fragmented baba in duple grouping, sequenced down (it spins off-axis)
  const babaFrag = BABA.slice(0, 4).map(([n]) => [n, 1.5]); // dotted-feel
  emit(events, sequence(babaFrag, [0, -2]), t, {
    gain: 0.46, pan: 0.1, decayMul: 1.2, accentEvery: 2, accentBoost: 1.3,
  });
  // glassy counter-line a 3rd above, in the bass's 3/4 — the two meters rub.
  emit(events, transpose(TWINKLE, -5), 9 * BAR, {
    preset: "kalimba", gain: 0.18, pan: -0.3, octave: 0, decayMul: 1.3,
  });
}

// ════════════════════════════════════════════════════════════════════════════
// SECTION C (bars 12-17): METRIC MODULATION → the dance stomps in DUPLE.
// The dotted-quarter of the 3/4 becomes the new beat; we feel a 2/4 stomp.
// The baba motif is DIMINISHED into a fast run (the dizzy spin tightening),
// then inverted, riding the new pulse.
// ════════════════════════════════════════════════════════════════════════════
{
  const prog = ["F", "Bb", "C", "Dm", "Bb", "C"];
  prog.forEach((c, i) => barDuple(12 + i, c));

  const newBeat = 1.5 * BEAT; // the modulated pulse = dotted quarter

  // baba diminished into a rolling run, on the NEW beat, stated then sequenced.
  const babaRun = diminish(BABA, 2); // twice as fast (sixteenth-ish spin)
  let t = 12 * BAR;
  t = emit(events, babaRun, t, {
    beatUnit: newBeat, gain: 0.46, decayMul: 1.1, accentEvery: 4,
    accentBoost: 1.35, pan: -0.08,
  });
  t = emit(events, sequence(babaRun, [3]), t, {  // sequence up a 4th
    beatUnit: newBeat, gain: 0.46, decayMul: 1.1, accentEvery: 4,
    accentBoost: 1.35, pan: 0.08,
  });
  // inverted baba answer (mirror the contour) high & far — the spin reversing.
  const axis = m("A5");
  emit(events, invert(BABA, axis), 15 * BAR, {
    preset: "kalimba", beatUnit: newBeat, gain: 0.22, octave: 0, pan: 0.34,
    decayMul: 1.25,
  });
  // a duple xylophone tick marking the new strong beats (makes the 2/4 felt)
  for (let bar = 12; bar < 18; bar++) {
    const t2 = bar * BAR;
    events.push({ preset: "woodblock", startSec: t2, midi: m("F5"), durSec: 0.12, gain: 0.12, pan: 0 });
    events.push({ preset: "woodblock", startSec: t2 + newBeat, midi: m("C5"), durSec: 0.12, gain: 0.12, pan: 0 });
  }
}

// ════════════════════════════════════════════════════════════════════════════
// SECTION D (bars 18-23): HEMIOLA II — the OTHER way. Bass implies duple
// (booms every 2 beats), while the lead snaps back into spinning 3/4 phrases.
// Accents collide; we let WOW and HUSH fragments overlap in canon (stretto),
// then a sequence pulls toward the cadence.
// ════════════════════════════════════════════════════════════════════════════
{
  spanImplied2(18, 6, ["F", "C", "Bb", "Gm", "C", "F"], 1.0);

  // lead back in 3 — WOW wobble, augmented slightly so it floats over the duple.
  let t = 18 * BAR;
  t = emit(events, augment(WOW, 1.0), t, {
    gain: 0.5, decayMul: 1.25, swing: SWING, accentEvery: 3, accentBoost: 1.2,
  });
  // canon: a second voice (kalimba) enters one bar later with the SAME WOW,
  // a 5th up — stretto, two spinning lines crossing the duple bass.
  emit(events, transpose(WOW, 7), 19 * BAR, {
    preset: "kalimba", gain: 0.2, pan: 0.3, decayMul: 1.3, swing: SWING,
  });
  // retrograde baba sequenced down toward the cadence — the tune unwinding.
  const babaRetro = retrograde(BABA);
  emit(events, sequence(babaRetro.slice(0, 4), [0, -2, -4]), 21 * BAR, {
    gain: 0.46, pan: -0.1, decayMul: 1.2, accentEvery: 4, accentBoost: 1.25,
  });
}

// ════════════════════════════════════════════════════════════════════════════
// SECTION E (bars 24-29): RECOMBINATION + cadence. Fragments of every meter
// overlap (stretto) — a 3/4 hush, a duple stomp, a hemiola boom — then the
// bar-line re-coheres and the original F-major cadence brings it home to rest.
// ════════════════════════════════════════════════════════════════════════════
{
  // bar 24-25: collide all three feels at once
  barWaltz(24, "F", 0.9);
  barDuple(25, "C", 0.9);
  barHemiola2over3(26, "Bb", 0.95);
  barWaltz(27, "Dm", 0.9);
  barWaltz(28, "C", 0.95);
  barWaltz(29, "F", 1.0);

  // overlapping fragments (stretto): hush (3/4), baba diminished (duple), and
  // an inverted twinkle — all entering close together, the dizziness peaking.
  emit(events, HUSH, 24 * BAR, { gain: 0.5, decayMul: 1.25, swing: SWING });
  emit(events, diminish(BABA, 2), 24 * BAR + 1.5 * BEAT, {
    beatUnit: 1.5 * BEAT, preset: "kalimba", gain: 0.2, pan: 0.32, decayMul: 1.2,
  });
  emit(events, invert(TWINKLE, m("A5")), 25 * BAR, {
    preset: "staccato", gain: 0.16, pan: -0.28, decayMul: 1.0,
  });

  // the bar-line re-coheres: a clean plain baba (the theme returns, recognizable)
  emit(events, BABA, 26 * BAR, { gain: 0.5, decayMul: 1.25, swing: SWING, pan: -0.04 });

  // the cadence: SLEEP descent, augmented, settling to F — home at last.
  let t = emit(events, augment(SLEEP, 1.0), 28 * BAR, {
    gain: 0.5, decayMul: 1.35, swing: SWING,
  });
  // a final low F bloom under the rest
  oom(29 * BAR, "F2", 0.4, 3 * BEAT);
  // a far high F kalimba — the last spin coming to rest
  events.push({
    preset: "kalimba", startSec: 29 * BAR + 0.5 * BEAT, midi: m("F6"),
    durSec: 2.5 * BEAT, gain: 0.16, decayMul: 1.4, pan: 0.36,
  });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "waltzbaba",
  here: HERE,
  title: "waltzbaba",
  reverb: { wet: 0.34, decay: 0.85, damp: 0.36 },
  fadeIn: 0.9,
  fadeOut: 4.5,
  tailSec: 5.0,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
