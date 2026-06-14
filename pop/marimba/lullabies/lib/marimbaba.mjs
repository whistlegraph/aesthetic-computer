// marimbaba.mjs — the canonical marimbaba lullaby melody as reusable data,
// transcribed from pop/marimba/marimbaba.np (F major, 3/4, ~56 BPM, 24 bars).
// It is itself a riff on the whistlegraph platter phrases — the "unspoken
// lyric" in the .np: hush-hush / twinkle-little-star / mommy-wow-wow /
// slinky ba-ba-ba-bap / sleep-now-little-one.
//
// buildMarimbaba(opts) returns a flat event array (seconds) for core.mjs.
// Variations import this, then transpose / re-tempo / re-voice / ornament,
// or splice the MOTIFS to riff a new tune over the same DNA.

import { m } from "./core.mjs";

// per-voice ring stretch (dreamier than physically accurate) + default pan.
export const DECAY = { rosewood: 1.8, bass: 1.8, kalimba: 1.75, vibraphone: 1.4, vibraphone_off: 1.4, staccato: 1.0, kelon: 1.3, xylophone: 0.9 };
const PAN = { rosewood: 0, bass: 0, kalimba: 0.32, vibraphone: 0.18, vibraphone_off: -0.18, staccato: -0.22, kelon: 0.1, xylophone: 0.25 };

// The score as tuples: [voice, bar, beatWithinBar, noteName, beats, gain].
// Bars 0-indexed, 3 beats per bar. Faithful to marimbaba.np + the renderer.
export const SCORE = [
  // ── [hush hush] bars 0-3 — descending sigh ──
  ["rosewood",0,0,"C5",1,0.55],["rosewood",0,1,"A4",1,0.55],["rosewood",0,2,"F4",1,0.55],
  ["rosewood",1,0,"F4",3,0.45],
  ["rosewood",2,0,"A4",1.5,0.55],["rosewood",2,1.5,"G4",1.5,0.55],
  ["rosewood",3,0,"F4",3,0.55],
  ["bass",0,0,"F2",3,0.5],["bass",1,0,"F2",3,0.5],["bass",2,0,"F2",3,0.5],["bass",3,0,"F2",3,0.5],
  // ── [twinkle] bars 4-9 — climbing-falling waves ──
  ["rosewood",4,0,"F5",1,0.6],["rosewood",4,1,"A5",1,0.6],["rosewood",4,2,"C6",0.5,0.65],["rosewood",4,2.5,"A5",0.5,0.55],
  ["rosewood",5,0,"G5",3,0.55],
  ["rosewood",6,0,"F5",1,0.55],["rosewood",6,1,"G5",1,0.55],["rosewood",6,2,"A5",0.5,0.6],["rosewood",6,2.5,"G5",0.5,0.5],
  ["rosewood",7,0,"F5",3,0.55],
  ["rosewood",8,0,"Bb5",1,0.6],["rosewood",8,1,"D6",1,0.65],["rosewood",8,2,"Bb5",1,0.55],
  ["rosewood",9,0,"C6",1.5,0.6],["rosewood",9,1.5,"A5",1.5,0.55],
  ["bass",4,0,"F2",3,0.45],["bass",5,0,"F3",3,0.45],["bass",6,0,"F2",3,0.45],["bass",7,0,"F3",3,0.45],["bass",8,0,"F2",3,0.45],["bass",9,0,"F3",3,0.45],
  ["vibraphone_off",4,0,"F4",9,0.18],["vibraphone_off",4,0,"A4",9,0.18],["vibraphone_off",4,0,"C5",9,0.18],
  ["vibraphone_off",7,0,"Bb4",9,0.18],["vibraphone_off",7,0,"D5",9,0.18],["vibraphone_off",7,0,"F5",9,0.18],
  // ── [wow wow wow] bars 10-13 — held wobble ──
  ["vibraphone",10,0,"G5",6,0.45],["vibraphone",10,0,"Bb5",6,0.4],
  ["rosewood",12,0,"A5",1,0.55],["rosewood",12,1,"G5",1,0.55],["rosewood",12,2,"A5",1,0.55],
  ["rosewood",13,0,"F5",3,0.55],
  ["kalimba",10,2.5,"D6",0.5,0.3],["kalimba",11,1,"F6",0.5,0.25],["kalimba",12,2.5,"C6",0.5,0.3],["kalimba",13,1.5,"A5",0.5,0.25],
  ["bass",10,0,"F2",3,0.35],["bass",12,0,"F2",3,0.35],
  // ratatatata — staccato 16th tumble across bar 11
  ["staccato",11,0,"A5",0.22,0.34],["staccato",11,0.1875,"G5",0.22,0.34],["staccato",11,0.375,"A5",0.22,0.34],["staccato",11,0.5625,"G5",0.22,0.34],
  ["staccato",11,0.75,"F5",0.22,0.34],["staccato",11,0.9375,"G5",0.22,0.34],["staccato",11,1.125,"A5",0.22,0.34],["staccato",11,1.3125,"C6",0.22,0.34],
  // ── [ba-ba-ba bap] bars 14-17 — slinky-dog wobble ──
  ["rosewood",14,0,"A5",0.5,0.6],["rosewood",14,0.5,"G5",0.5,0.55],["rosewood",14,1,"A5",1,0.6],["rosewood",14,2,"F5",1,0.65],
  ["rosewood",15,0,"C6",0.5,0.6],["rosewood",15,0.5,"Bb5",0.5,0.55],["rosewood",15,1,"C6",1,0.6],["rosewood",15,2,"A5",1,0.65],
  ["rosewood",16,0,"G5",1,0.55],["rosewood",16,1,"F5",1,0.55],["rosewood",16,2,"E5",1,0.5],
  ["rosewood",17,0,"F5",3,0.55],
  ["kalimba",14,2,"F6",1,0.28],["kalimba",15,2,"A5",1,0.28],
  ["bass",14,0,"F2",3,0.45],["bass",15,0,"C3",3,0.45],["bass",16,0,"Eb2",3,0.45],["bass",17,0,"F2",3,0.45],
  // ── [sleep now] bars 18-23 — final settling descent ──
  ["rosewood",18,0,"C5",1.5,0.5],["rosewood",18,1.5,"A4",1.5,0.5],
  ["rosewood",19,0,"G4",1,0.5],["rosewood",19,1,"F4",1,0.5],["rosewood",19,2,"F4",1,0.45],
  ["rosewood",20,0,"A4",1,0.5],["rosewood",20,1,"G4",1,0.5],["rosewood",20,2,"F4",1,0.5],
  ["rosewood",21,0,"F4",3,0.5],
  ["rosewood",22,0,"F3",3,0.45],
  ["bass",18,0,"F2",3,0.4],["bass",20,0,"F2",3,0.4],["bass",22,0,"F2",3,0.35],
];

// Named scale-degree motif cells (in C-relative semitone offsets from the
// key root, octave-marked) so variations can re-key / re-mode and re-voice
// the whistlegraph phrases. Pair with a root midi + a scale.
export const MOTIFS = {
  hush:    [["C5",1],["A4",1],["F4",1],["F4",3]],          // descending sigh
  twinkle: [["F5",1],["A5",1],["C6",0.5],["A5",0.5],["G5",3]], // climbing wave
  flyHigh: [["Bb5",1],["D6",1],["Bb5",1],["C6",1.5],["A5",1.5]], // butterfly / "way up high"
  wow:     [["G5",1],["Bb5",1],["A5",1],["G5",1],["A5",1],["F5",3]], // mommy-wow wobble
  baba:    [["A5",0.5],["G5",0.5],["A5",1],["F5",1],["C6",0.5],["Bb5",0.5],["C6",1],["A5",1]], // slinky-dog
  sleep:   [["C5",1.5],["A4",1.5],["G4",1],["F4",1],["F4",1],["F4",3]], // settle
};

// the whistlegraph platter phrases the lullaby chants (for naming / docs).
export const WHISTLEGRAPH = ["butterfly-cosplayer", "mommy-wow", "slinky-dog", "lately-when-i-fly"];

// buildMarimbaba — turn SCORE into seconds-based events.
//  opts: { bpm=56, beatsPerBar=3, transpose=0, leadPreset="rosewood",
//          gainMul=1, decayMul=1, swing=0 }
// transpose is in semitones; leadPreset replaces "rosewood"; swing drags
// off-beats (fraction of a beat). Returns a fresh event array.
export function buildMarimbaba(opts = {}) {
  const { bpm = 56, beatsPerBar = 3, transpose = 0, leadPreset = "rosewood", gainMul = 1, decayMul = 1, swing = 0, filter = null } = opts;
  const BEAT = 60 / bpm, BAR = beatsPerBar * BEAT;
  const out = [];
  for (const [voice0, bar, beat, note, beats, gain] of SCORE) {
    if (filter && !filter(voice0, bar, beat, note)) continue;
    const voice = voice0 === "rosewood" ? leadPreset : voice0;
    const frac = beat - Math.floor(beat);
    const sw = (frac > 0.4 && frac < 0.6) ? swing * BEAT : 0; // nudge the &s
    out.push({
      preset: voice,
      startSec: bar * BAR + beat * BEAT + sw,
      midi: m(note) + transpose,
      durSec: beats * BEAT,
      gain: gain * gainMul,
      decayMul: (DECAY[voice] ?? 1) * decayMul,
      pan: PAN[voice] ?? 0,
    });
  }
  return out;
}
