// lanternbaba.mjs — a flickering-lantern riff on the marimbaba lullaby, lit
// by kalimba and tapped by woodblock.
//
// Direction: F# minor, ~56 BPM, kalimba-led with a soft woodblock pulse. A
// lantern's light is never quite steady — it gutters, flares, dapples the
// walls. So the tune does too.
//
// DEVELOPMENT STRATEGY — STOCHASTIC FLICKER ORNAMENTATION:
//  - A STEADY CORE PHRASE (the whistlegraph contour: hush → twinkle → wow →
//    baba → sleep, re-keyed into F# minor) is the lantern's flame: it is
//    always there, always recognizable.
//  - Around that flame, a DETERMINISTIC-RANDOM flicker engine sprinkles grace
//    notes, mordents, neighbor-tone shivers and tiny broken-chord cells. The
//    randomness is seeded (an LCG) so every render is identical, but the
//    placement, octave and length of the flicker feels gusty and alive.
//  - The FLICKER INTENSITY breathes across the arc: it starts as a faint
//    glow (rare, soft graces), swells to a guttering flare at the apex (dense
//    dapple, brief flame-up runs), then SETTLES — the flicker thins and dims
//    as the lantern is carried to sleep, leaving the bare core phrase.
//  - A recognizable thread survives every flare: the core melody is laid down
//    FIRST and loudest, the flicker only orbits it, and the final cadence
//    settles to F# in the home octave with the flame nearly still.
//
// Run:  node variations/lanternbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 56;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4, like the source

// ── F# natural-minor scale-fold (root pc = 6). Snap voices into the mode. ──
const ROOT = 6; // F#
const MINOR = [0, 2, 3, 5, 7, 8, 10]; // natural minor
function snap(midi) {
  const pc = ((midi % 12) + 12) % 12;
  const rel = ((pc - ROOT) % 12 + 12) % 12;
  let best = MINOR[0], bestD = 99;
  for (const s of MINOR) {
    const d = Math.min(Math.abs(s - rel), 12 - Math.abs(s - rel));
    if (d < bestD) { bestD = d; best = s; }
  }
  const base = midi - rel;
  let cand = base + best;
  if (cand - midi > 6) cand -= 12;
  if (midi - cand > 6) cand += 12;
  return cand;
}

// step up/down N scale degrees from a midi already in the mode.
function step(midi, deg) {
  const pc = ((midi % 12) + 12) % 12;
  const rel = ((pc - ROOT) % 12 + 12) % 12;
  let idx = MINOR.indexOf(rel);
  if (idx < 0) { // not in mode → snap first
    return step(snap(midi), deg);
  }
  let oct = Math.floor((midi - (ROOT)) / 12); // rough octave bucket
  let n = idx + deg;
  while (n < 0) { n += 7; oct -= 1; }
  while (n >= 7) { n -= 7; oct += 1; }
  return snap(midi) + (MINOR[n] - rel) + 12 * (Math.floor((idx + deg) / 7) - 0) * 0
    + ((idx + deg) >= 7 ? 12 : 0) * 0; // keep simple; fix below
}

// The above got fiddly; use a clean degree walker instead.
const DEG_BASE = m("F#3"); // anchor degree-0 here
function degToMidi(d) {
  // d is an integer scale-degree offset from F#3 (degree 0).
  const oct = Math.floor(d / 7);
  const within = ((d % 7) + 7) % 7;
  return DEG_BASE + 12 * oct + MINOR[within];
}
function midiToDeg(midi) {
  // nearest scale degree to a (snapped) midi.
  const snapped = snap(midi);
  let best = 0, bestD = 999;
  for (let d = -21; d <= 28; d++) {
    const mm = degToMidi(d);
    const dd = Math.abs(mm - snapped);
    if (dd < bestD) { bestD = dd; best = d; }
  }
  return best;
}

// ── the source MOTIFS are written in F major. Re-key to F# minor: take each
//    note as a scale degree of the source contour and re-cast it onto the F#
//    minor scale, so the *shape* (the whistlegraph contour) survives but the
//    color turns minor and lantern-warm. We do this by mapping F-major
//    degrees → F#-minor degrees of the same index. ─────────────────────────
const FMAJ_ROOT = m("F4") % 12; // 5
const FMAJ = [0, 2, 4, 5, 7, 9, 11];
function fmajDeg(midi) {
  // degree index (with octave) of a note in F major.
  const pc = ((midi - FMAJ_ROOT) % 12 + 12) % 12;
  let idx = FMAJ.indexOf(pc);
  if (idx < 0) { // chromatic (Bb etc handled), snap to nearest
    let best = 0, bd = 99;
    for (let i = 0; i < FMAJ.length; i++) {
      const dd = Math.min(Math.abs(FMAJ[i] - pc), 12 - Math.abs(FMAJ[i] - pc));
      if (dd < bd) { bd = dd; best = i; }
    }
    idx = best;
  }
  const ref = m("F4");
  const oct = Math.round((midi - ref - (FMAJ[idx])) / 12);
  return oct * 7 + idx;
}

// re-cast an F-major motif into an F#-minor cell of [midi, beats], dropped to
// a cozy lantern register (a tad lower than the source).
function cell(motif, octShift = -1) {
  return MOTIFS[motif].map(([name, beats]) => {
    const d = fmajDeg(m(name));
    return [degToMidi(d + octShift * 7), beats];
  });
}

// transpose a cell by scale-degrees (sequencing).
function seq(c, degs) { return c.map(([midi, beats]) => [degToMidi(midiToDeg(midi) + degs), beats]); }
// invert a cell around its first note, in scale degrees.
function invert(c) {
  const axis = midiToDeg(c[0][0]);
  return c.map(([midi, beats]) => [degToMidi(axis - (midiToDeg(midi) - axis)), beats]);
}
// retrograde.
function retro(c) { return [...c].reverse(); }

// ── deterministic flicker RNG (LCG) ─────────────────────────────────────────
function lcg(seed) {
  let s = (seed * 2654435761) >>> 0;
  return () => ((s = (s * 1103515245 + 12345) >>> 0) / 4294967296);
}

// ── lay the CORE phrase — kalimba flame, the steady recognizable thread. ─────
function lay(out, c, { startBar, beat0 = 0, voice = "kalimba", gain = 0.5, decayMul = 1.7, pan = 0.18 } = {}) {
  let beat = beat0;
  for (let i = 0; i < c.length; i++) {
    const [midi, beats] = c[i];
    out.push({
      preset: voice,
      startSec: startBar * BAR + beat * BEAT,
      midi,
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY[voice] ?? 1) * decayMul,
      pan,
    });
    beat += beats;
  }
  return beat;
}

// ── THE FLICKER ENGINE ───────────────────────────────────────────────────
// For a span of beats anchored on a core cell, sprinkle deterministic-random
// ornaments: grace notes (a step above/below a core note), neighbor-tone
// shivers, and brief broken-chord flame-ups. `intensity` (0..1) scales how
// often and how dense the flicker is; `seed` keeps it repeatable.
function flicker(out, coreCell, { startBar, beat0 = 0, intensity = 0.5, seed = 1, voice = "kalimba", gainBase = 0.22, pan = 0.3 } = {}) {
  const rnd = lcg(seed);
  let beat = beat0;
  for (let i = 0; i < coreCell.length; i++) {
    const [midi, beats] = coreCell[i];
    const noteStart = beat;

    // (a) GRACE before the core note — a quick step from above or below that
    //     "lights" the note. Fires more as intensity rises.
    if (rnd() < intensity * 0.7 && beat > beat0 + 0.05) {
      const dir = rnd() < 0.5 ? 1 : -1;
      const gm = degToMidi(midiToDeg(midi) + dir); // a scale-step away
      const glen = 0.12 + rnd() * 0.1;
      out.push({
        preset: voice,
        startSec: startBar * BAR + (noteStart - glen) * BEAT,
        midi: gm,
        durSec: glen * BEAT * 1.6,
        gain: gainBase * (0.6 + rnd() * 0.4),
        decayMul: (DECAY[voice] ?? 1) * 1.3,
        pan: pan + (rnd() - 0.5) * 0.3,
      });
    }

    // (b) NEIGHBOR-TONE SHIVER — a faint upper-neighbor dapple part-way
    //     through a longer core note (the flame wavering on the wall).
    if (beats >= 1 && rnd() < intensity * 0.6) {
      const nm = degToMidi(midiToDeg(midi) + (rnd() < 0.6 ? 1 : 2));
      const at = noteStart + beats * (0.45 + rnd() * 0.3);
      out.push({
        preset: voice,
        startSec: startBar * BAR + at * BEAT,
        midi: nm + (rnd() < 0.3 ? 12 : 0), // sometimes flares an octave up
        durSec: (0.25 + rnd() * 0.25) * BEAT,
        gain: gainBase * (0.4 + rnd() * 0.35),
        decayMul: (DECAY[voice] ?? 1) * 1.2,
        pan: pan + (rnd() - 0.5) * 0.4,
      });
    }

    beat += beats;
  }

  // (c) FLAME-UP RUN — at higher intensity, a brief broken-chord/ scalewise
  //     sparkle run somewhere in the span (a gust catching the wick).
  if (rnd() < intensity * 0.8) {
    const runLen = 2 + Math.floor(rnd() * 3); // 2..4 notes
    const anchorDeg = midiToDeg(coreCell[Math.floor(rnd() * coreCell.length)][0]);
    const up = rnd() < 0.6;
    const stepBy = rnd() < 0.5 ? 1 : 2; // step or skip (broken chord)
    const totalBeats = beat - beat0;
    const startB = beat0 + rnd() * Math.max(0.1, totalBeats - runLen * 0.25);
    const nlen = 0.18 + rnd() * 0.1;
    for (let k = 0; k < runLen; k++) {
      const dg = anchorDeg + (up ? 1 : -1) * stepBy * k + 7; // lift an octave for sparkle
      out.push({
        preset: voice,
        startSec: startBar * BAR + (startB + k * nlen * 1.1) * BEAT,
        midi: degToMidi(dg),
        durSec: nlen * BEAT * 1.4,
        gain: gainBase * (0.5 + rnd() * 0.3) * (1 - k * 0.12),
        decayMul: (DECAY[voice] ?? 1) * 1.2,
        pan: pan + (rnd() - 0.5) * 0.5,
      });
    }
  }
}

// ── woodblock pulse — the steady carrying footstep under the lantern. A soft
//    tick on beat 1 (and a quieter ghost) keeps the cradle rocking. ─────────
function woodPulse(out, startBar, lenBars, { gain = 0.16, ghost = true } = {}) {
  for (let b = 0; b < lenBars; b++) {
    out.push({
      preset: "woodblock",
      startSec: (startBar + b) * BAR,
      midi: m("F#4"),
      durSec: 0.2 * BEAT,
      gain,
      decayMul: 0.9,
      pan: -0.28,
    });
    if (ghost) {
      out.push({
        preset: "woodblock",
        startSec: (startBar + b) * BAR + 2 * BEAT,
        midi: m("C#4"),
        durSec: 0.18 * BEAT,
        gain: gain * 0.55,
        decayMul: 0.9,
        pan: -0.34,
      });
    }
  }
}

// soft bass root under a span (kept low + clear).
function bass(out, startBar, name, lenBars = 1, gain = 0.4) {
  out.push({
    preset: "bass",
    startSec: startBar * BAR,
    midi: snap(m(name)),
    durSec: lenBars * BAR,
    gain,
    decayMul: DECAY.bass * 1.4,
    pan: 0,
  });
}

// vibraphone_off pad — a warm lantern halo holding the harmony.
const padName = (deg, oct) => snap(m(["F#", "G#", "A", "B", "C#", "D", "E"][deg] + oct));
function pad(out, startBar, lenBars, triad, baseOct, gain = 0.12) {
  for (let i = 0; i < triad.length; i++) {
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: padName(triad[i], baseOct),
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.0,
      pan: i === 0 ? -0.2 : i === 1 ? 0.0 : 0.2,
    });
  }
}

const events = [];

// ════════════════════════════════════════════════════════════════════════
//  ARC: faint glow → kindling → guttering flare (apex) → settling to sleep.
//  Flicker intensity rises then falls; the core phrase is constant.
// ════════════════════════════════════════════════════════════════════════

// ── PASS 0 (bars 0–3): FAINT GLOW. The hush sigh as the bare flame, almost
//    no flicker — just one or two soft graces so the contour reads clean. ───
{
  const hush = cell("hush");
  lay(events, hush, { startBar: 0, gain: 0.5, decayMul: 1.8, pan: 0.16 });
  flicker(events, hush, { startBar: 0, intensity: 0.18, seed: 3, gainBase: 0.16, pan: 0.3 });
  woodPulse(events, 0, 4, { gain: 0.14 });
  bass(events, 0, "F#2", 2, 0.42);
  bass(events, 2, "F#2", 2, 0.42);
  pad(events, 0, 4, [0, 2, 4], 3, 0.11); // F# minor halo (F#-A-C#)
}

// ── PASS 1 (bars 4–9): KINDLING. The twinkle climb, flicker waking up
//    (intensity ~0.4). A sequenced answer a 3rd up, dappled differently. ───
{
  const tw = cell("twinkle");
  lay(events, tw, { startBar: 4, gain: 0.48, decayMul: 1.7, pan: 0.14 });
  flicker(events, tw, { startBar: 4, intensity: 0.4, seed: 11, gainBase: 0.2, pan: 0.32 });

  const tw3 = seq(tw, 2); // up a scale-3rd
  lay(events, tw3, { startBar: 6, gain: 0.44, decayMul: 1.7, pan: 0.2 });
  flicker(events, tw3, { startBar: 6, intensity: 0.45, seed: 17, gainBase: 0.2, pan: 0.34 });

  const fly = cell("flyHigh");
  lay(events, fly, { startBar: 8, gain: 0.45, decayMul: 1.6, pan: 0.22 });
  flicker(events, fly, { startBar: 8, intensity: 0.5, seed: 23, gainBase: 0.21, pan: 0.36 });

  woodPulse(events, 4, 6, { gain: 0.15 });
  bass(events, 4, "F#2", 2, 0.36); bass(events, 6, "A2", 2, 0.36); bass(events, 8, "D2", 2, 0.36);
  pad(events, 4, 2, [0, 2, 4], 3, 0.11);     // F#m
  pad(events, 6, 2, [2, 4, 6], 3, 0.11);     // A major-ish (A-C#-E)
  pad(events, 8, 2, [5, 0, 2], 3, 0.11);     // D (D-F#-A)
}

// ── PASS 2 (bars 10–15): GUTTERING FLARE — the apex. The "wow" wobble and
//    the "baba" slinky-bap are the core flame; flicker is DENSE (intensity
//    ~0.85), full of grace shivers and flame-up runs catching the gusts. ───
{
  const wow = cell("wow");
  lay(events, wow, { startBar: 10, gain: 0.46, decayMul: 1.7, pan: 0.12 });
  flicker(events, wow, { startBar: 10, intensity: 0.85, seed: 31, gainBase: 0.24, pan: 0.34 });

  // an inverted echo — the flame answering itself across the wall.
  const wowI = invert(wow);
  lay(events, wowI, { startBar: 12, gain: 0.36, decayMul: 1.7, pan: -0.16 });
  flicker(events, wowI, { startBar: 12, intensity: 0.8, seed: 37, gainBase: 0.22, pan: -0.3 });

  const baba = cell("baba");
  lay(events, baba, { startBar: 14, gain: 0.44, decayMul: 1.6, pan: 0.16 });
  flicker(events, baba, { startBar: 14, intensity: 0.9, seed: 43, gainBase: 0.24, pan: 0.36 });

  // a couple of glockenspiel embers floating over the flare
  for (const [b, bt, nm, pn] of [[11, 1.4, "C#6", 0.34], [13, 0.6, "F#6", 0.3], [15, 1.8, "A5", -0.26]]) {
    events.push({ preset: "glockenspiel", startSec: b * BAR + bt * BEAT, midi: snap(m(nm)), durSec: 1.5 * BEAT, gain: 0.1, decayMul: (DECAY.glockenspiel ?? 1) * 1.6, pan: pn });
  }

  woodPulse(events, 10, 6, { gain: 0.16 });
  bass(events, 10, "C#2", 2, 0.34); bass(events, 12, "F#2", 2, 0.34); bass(events, 14, "D2", 2, 0.34);
  pad(events, 10, 2, [4, 6, 1], 3, 0.12); // C# (dominant-ish)
  pad(events, 12, 2, [0, 2, 4], 3, 0.12); // F#m
  pad(events, 14, 2, [5, 0, 2], 3, 0.12); // D
}

// ── PASS 3 (bars 16–19): the gust passes — flicker THINNING (intensity
//    falling 0.5 → 0.3). The twinkle returns in retrograde, the flame
//    folding back toward stillness. ───────────────────────────────────────
{
  const twR = retro(cell("twinkle"));
  lay(events, twR, { startBar: 16, gain: 0.42, decayMul: 1.7, pan: 0.14 });
  flicker(events, twR, { startBar: 16, intensity: 0.45, seed: 53, gainBase: 0.19, pan: 0.3 });

  const w = cell("wow");
  lay(events, w, { startBar: 18, gain: 0.38, decayMul: 1.7, pan: 0.12 });
  flicker(events, w, { startBar: 18, intensity: 0.3, seed: 59, gainBase: 0.17, pan: 0.28 });

  woodPulse(events, 16, 4, { gain: 0.13 });
  bass(events, 16, "B2", 2, 0.32); bass(events, 18, "C#2", 2, 0.32);
  pad(events, 16, 2, [3, 5, 0], 3, 0.11); // B (B-D-F#)
  pad(events, 18, 2, [4, 6, 1], 3, 0.11); // C#
}

// ── PASS 4 (bars 20–25): SETTLING TO SLEEP. The sleep cadence comes home,
//    the flicker nearly out (intensity ~0.12) — a last faint ember or two,
//    then the bare flame on F#. The lantern is set down beside the cradle. ─
{
  const sleep = cell("sleep");
  lay(events, sleep, { startBar: 20, gain: 0.46, decayMul: 1.9, pan: 0.1 });
  flicker(events, sleep, { startBar: 20, intensity: 0.14, seed: 67, gainBase: 0.14, pan: 0.26 });

  // a final, faint, close hush echo — the recognizable thread, signed off.
  const hush = cell("hush");
  lay(events, hush, { startBar: 23, gain: 0.4, decayMul: 2.0, pan: 0.08 });
  flicker(events, hush, { startBar: 23, intensity: 0.1, seed: 71, gainBase: 0.12, pan: 0.22 });

  woodPulse(events, 20, 3, { gain: 0.1, ghost: false });
  bass(events, 20, "F#2", 2, 0.36); bass(events, 22, "C#2", 2, 0.32); bass(events, 24, "F#2", 2, 0.32);
  pad(events, 20, 6, [0, 2, 4], 3, 0.12); // F#m — long resolving glow
  // one last ember fading over the home tonic.
  events.push({ preset: "glockenspiel", startSec: 23 * BAR + 1.0 * BEAT, midi: snap(m("F#6")), durSec: 4 * BEAT, gain: 0.08, decayMul: (DECAY.glockenspiel ?? 1) * 1.7, pan: 0.3 });
  events.push({ preset: "kalimba", startSec: 24 * BAR + 1.5 * BEAT, midi: snap(m("C#5")), durSec: 3 * BEAT, gain: 0.09, decayMul: DECAY.kalimba * 1.8, pan: -0.22 });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "lanternbaba",
  here: HERE,
  title: "lanternbaba",
  reverb: { wet: 0.4, decay: 0.86, damp: 0.36 }, // warm, glowing lantern-room
  healingHz: 396, // Solfeggio UT — grounding, sits under F# minor
  fadeIn: 1.4,
  fadeOut: 6.0,
  tailSec: 6.0,
  peak: 0.84,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
