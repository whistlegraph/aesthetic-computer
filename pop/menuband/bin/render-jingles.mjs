#!/usr/bin/env node
// render-jingles.mjs — the four campaign jingles for the Menu Band promo
// reels (announce / features / chords / scales), built on the same /pop
// lullaby engine as the launch waltz (render-waltz.mjs). Each jingle writes
// an mp3 + a notes.json its sim choreographs to; the chords jingle also
// writes a segment score (menuband-chords.score.json) so the audio and the
// on-screen modifier keycaps agree to the frame; the scales jingle writes
// menuband-scales.score.json (spoken cue times + the letter ladder) that
// sing-jingle.mjs and sim-scales.mjs both read, so the voice, the bed and
// the visuals share ONE timeline.
//
// EVERY lead note is a WHITE KEY: the strip rig re-lights the real captured
// menu-bar piano from the single-note captures sim.mjs cached (G4..B5,
// C-major scale), so staying diatonic keeps every lit key pixel-real.
//
// Run:  node pop/menuband/bin/render-jingles.mjs        (from repo root)

import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { writeFileSync } from "node:fs";
import { renderLullaby, m } from "../../marimba/lullabies/lib/core.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT_DIR = resolve(HERE, "..", "out");

function makeTrack() {
  const events = [];
  const push = (lane, preset, startSec, note, durSec, gain, pan = 0, decayMul = 1.2) => {
    events.push({ lane, preset, startSec, midi: typeof note === "number" ? note : m(note), durSec, gain, pan, decayMul });
  };
  return { events, push };
}

function writeScore(name, events, extra, meta) {
  const notes = events
    .map((e) => ({ t: +e.startSec.toFixed(4), dur: +e.durSec.toFixed(4), midi: e.midi, vel: +e.gain.toFixed(3), lane: e.lane }))
    .sort((a, b) => a.t - b.t || a.midi - b.midi);
  writeFileSync(resolve(OUT_DIR, `${name}.notes.json`), JSON.stringify({ ...meta, notes, ...extra }, null, 2));
}

function master(name, events, opts) {
  const { mp3, durationSec } = renderLullaby(events, {
    name, here: HERE, out: resolve(OUT_DIR, `${name}.mp3`),
    healing: false, reverb: { wet: 0.26, decay: 0.82, damp: 0.38 },
    fadeIn: 0.3, fadeOut: 1.8, tailSec: 2.2, ...opts,
  });
  console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
  return durationSec;
}

// ════════════════════════════════════════════════════════════════════════
// 1 · ANNOUNCE — a 12-bar fanfare waltz in C (138 BPM): the launch waltz's
//     voice (glockenspiel over oom-pah-pah) but shorter and more emphatic,
//     opening with a rising call and cadencing clean on tonic for the
//     "on the Mac App Store now" card.
// ════════════════════════════════════════════════════════════════════════
{
  const BPM = 138, BEAT = 60 / BPM, BAR = 3 * BEAT;
  const { events, push } = makeTrack();
  const CH = {
    C: ["C2", ["E4", "G4"]], G: ["G2", ["D4", "G4"]], G7: ["G2", ["F4", "B3"]],
    F: ["F2", ["A3", "C4"]], Am: ["A2", ["C4", "E4"]], Dm: ["D2", ["F4", "A3"]],
  };
  const accompany = (bar, chord, g = 1) => {
    const [bn, [a, b]] = CH[chord];
    const t = bar * BAR;
    push("bass", "bass", t, bn, 1.05 * BEAT, 0.4 * g, 0);
    push("harmony", "staccato", t + BEAT, a, 0.42 * BEAT, 0.15 * g, -0.16, 0.9);
    push("harmony", "staccato", t + BEAT, b, 0.42 * BEAT, 0.15 * g, -0.16, 0.9);
    push("harmony", "staccato", t + 2 * BEAT, a, 0.42 * BEAT, 0.14 * g, 0.16, 0.9);
    push("harmony", "staccato", t + 2 * BEAT, b, 0.42 * BEAT, 0.14 * g, 0.16, 0.9);
  };
  const melody = (startBar, cell, { gain = 0.5, pan = 0.04, ring = 1.25 } = {}) => {
    let t = startBar * BAR;
    for (const [note, beats] of cell) {
      if (note != null) push("lead", "glockenspiel", t, note, beats * BEAT * ring, gain, pan, 1.3);
      t += beats * BEAT;
    }
  };
  ["C", "C", "F", "C", "Am", "F", "G7", "G7", "C", "F", "G7", "C"].forEach((c, i) => accompany(i, c));
  melody(0, [
    // bars 0-1: the rising fanfare call — climb the tonic triad, ring high
    ["C5", 1], ["E5", 1], ["G5", 1],
    ["C6", 2], ["G5", 1],
    // bars 2-3: answer over F — a bright turn and settle
    ["A5", 1], ["G5", 0.5], ["F5", 0.5], ["E5", 1],
    ["F5", 1], ["E5", 1], ["C5", 1],
    // bars 4-5: lean minor, reach back up
    ["E5", 1], ["A5", 1], ["C6", 1],
    ["A5", 1], ["F5", 1], ["A5", 1],
    // bars 6-7: dominant gather — poised, expectant
    ["G5", 1], ["F5", 1], ["D5", 1],
    ["B4", 1], ["D5", 1], ["G5", 1],
    // bars 8-11: the arrival — tune rings home, authentic cadence on C
    ["E5", 1], ["G5", 1], ["C6", 1],
    ["A5", 1], ["C6", 1], ["F5", 1],
    ["F5", 1], ["E5", 1], ["D5", 1],
    ["C5", 1], ["E5", 0.5], ["G5", 0.5], ["C6", 2],
  ]);
  // a far sparkle over the final bar
  push("harmony", "glockenspiel", 11 * BAR + BEAT, m("G6"), 2.2 * BEAT, 0.1, 0.32, 1.5);
  const durationSec = master("menuband-announce", events, { title: "Menu Band Announce Fanfare" });
  writeScore("menuband-announce", events, {}, {
    bpm: BPM, beatSec: +BEAT.toFixed(6), barSec: +BAR.toFixed(6), durationSec: +durationSec.toFixed(4),
  });
}

// ════════════════════════════════════════════════════════════════════════
// 2 · FEATURES — a 10-bar 4/4 tour groove (116 BPM): kalimba arpeggio bed,
//     woodblock ticks, and a singable vibraphone hook on top (the hook is
//     the `lead` lane, so it's what the strip lights).
// ════════════════════════════════════════════════════════════════════════
{
  const BPM = 116, BEAT = 60 / BPM, BAR = 4 * BEAT;
  const { events, push } = makeTrack();
  const ARPS = {                       // white-key arp tones per chord
    C: ["C4", "E4", "G4", "E4"], Am: ["A3", "C4", "E4", "C4"],
    F: ["F3", "A3", "C4", "A3"], G: ["G3", "B3", "D4", "B3"],
  };
  const BASSN = { C: "C2", Am: "A2", F: "F2", G: "G2" };
  const PROG = ["C", "Am", "F", "G", "C", "Am", "F", "G", "C", "C"];
  PROG.forEach((c, i) => {
    const t = i * BAR;
    push("bass", "bass", t, BASSN[c], 1.4 * BEAT, 0.36, 0);
    push("bass", "bass", t + 2 * BEAT, BASSN[c], 1.1 * BEAT, 0.24, 0);
    for (let s = 0; s < 8; s++) {      // 8th-note kalimba arps
      push("harmony", "kalimba", t + s * BEAT * 0.5, ARPS[c][s % 4], 0.5 * BEAT, 0.16, s % 2 ? 0.22 : -0.22, 1.0);
    }
    for (const b of [1, 3]) push("perc", "woodblock", t + b * BEAT, "E5", 0.15, 0.11, 0.1);
  });
  const hook = (bar, cell, gain = 0.42) => {
    let t = bar * BAR;
    for (const [note, beats] of cell) {
      if (note != null) push("lead", "vibraphone", t, note, beats * BEAT * 1.15, gain, 0.05, 1.25);
      t += beats * BEAT;
    }
  };
  hook(0, [[null, 1], ["E5", 1], ["G5", 1], ["C6", 1]]);
  hook(1, [["A5", 1.5], ["E5", 1.5], ["C5", 1]]);
  hook(2, [["F5", 1], ["A5", 1], ["C6", 1.5], [null, 0.5]]);
  hook(3, [["B5", 1], ["G5", 1], ["D5", 2]]);
  hook(4, [["E5", 1], ["G5", 1], ["C6", 1], ["E6", 1]]);
  hook(5, [["C6", 1.5], ["A5", 1.5], ["E5", 1]]);
  hook(6, [["A5", 1], ["C6", 1], ["F5", 2]]);
  hook(7, [["D5", 1], ["G5", 1], ["B5", 1], ["D6", 1]]);
  hook(8, [["C6", 2], ["G5", 1], ["E5", 1]]);
  hook(9, [["C5", 1], ["E5", 0.5], ["G5", 0.5], ["C6", 2]]);
  const durationSec = master("menuband-features", events, { title: "Menu Band Feature Tour" });
  writeScore("menuband-features", events, {}, {
    bpm: BPM, beatSec: +BEAT.toFixed(6), barSec: +BAR.toFixed(6), durationSec: +durationSec.toFixed(4),
  });
}

// ════════════════════════════════════════════════════════════════════════
// 3 · CHORDS — the modifier-grammar demo. One segment per quality, exactly
//     the app's own mapping (MenuBandController.chordQuality/chordIntervals):
//     ⌘ major · ⌥ minor · ⌘⌥ sus2 · ⌥⌃ diminished · ⌘⌥⌃ sus4 — voiced on
//     white keys so every chord's strip lighting is pixel-real. Ends on a
//     quick I–vi–IV–V–I progression with ⌘/⌥ flipping live.
// ════════════════════════════════════════════════════════════════════════
{
  const { events, push } = makeTrack();
  const SEG = 2.5;
  const segs = [];
  const chordHit = (t, notes, { gain = 0.3, ring = 1.9, restrike = true } = {}) => {
    push("bass", "bass", t, notes[0] - 24, 1.3, 0.34, 0);
    notes.forEach((n, i) => push("lead", "vibraphone", t + i * 0.06, n, ring, gain, (i - 1) * 0.14, 1.3));
    if (restrike) notes.forEach((n, i) => push("lead", "vibraphone", t + 1.3 + i * 0.05, n, 1.1, gain * 0.6, (i - 1) * 0.14, 1.2));
  };
  const seg = (i, label, mods, chordName, notes) => {
    const t0 = 0.4 + i * SEG, t1 = t0 + SEG;
    segs.push({ t0: +t0.toFixed(3), t1: +t1.toFixed(3), label, mods, chordName, notes });
    chordHit(t0 + 0.15, notes);
    return t0;
  };
  seg(0, "one key plays one note", [], "C", [m("C5")]);
  seg(1, "hold ⌘ — major", ["cmd"], "C major", [m("C5"), m("E5"), m("G5")]);
  seg(2, "hold ⌥ — minor", ["opt"], "A minor", [m("A4"), m("C5"), m("E5")]);
  seg(3, "⌘ ⌥ — sus2", ["cmd", "opt"], "C sus2", [m("C5"), m("D5"), m("G5")]);
  seg(4, "⌥ ⌃ — diminished", ["opt", "ctl"], "B dim", [m("B4"), m("D5"), m("F5")]);
  seg(5, "⌘ ⌥ ⌃ — sus4", ["cmd", "opt", "ctl"], "C sus4", [m("C5"), m("F5"), m("G5")]);

  // finale — the grammar in motion: C · Am · F · G · C with mods flipping
  const FIN0 = 0.4 + 6 * SEG;
  const FINALE = [
    ["C major", ["cmd"], [m("C5"), m("E5"), m("G5")]],
    ["A minor", ["opt"], [m("A4"), m("C5"), m("E5")]],
    ["F major", ["cmd"], [m("F4"), m("A4"), m("C5")]],
    ["G major", ["cmd"], [m("G4"), m("B4"), m("D5")]],
    ["C major", ["cmd"], [m("C5"), m("E5"), m("G5"), m("C6")]],
  ];
  FINALE.forEach(([name, mods, notes], k) => {
    const t0 = FIN0 + k * 0.85;
    const last = k === FINALE.length - 1;
    const t1 = last ? t0 + 3.0 : t0 + 0.85;
    segs.push({ t0: +t0.toFixed(3), t1: +t1.toFixed(3), label: "every chord under three keys", mods, chordName: name, notes });
    chordHit(t0, notes, { gain: last ? 0.34 : 0.28, ring: last ? 2.6 : 1.0, restrike: false });
  });
  push("harmony", "glockenspiel", FIN0 + 4 * 0.85 + 0.4, m("G6"), 2.0, 0.1, 0.3, 1.5);

  const durationSec = master("menuband-chords", events, { title: "Menu Band Chord Grammar" });
  writeScore("menuband-chords", events, {}, { durationSec: +durationSec.toFixed(4) });
  writeFileSync(resolve(OUT_DIR, "menuband-chords.score.json"),
    JSON.stringify({ durationSec: +durationSec.toFixed(4), segs }, null, 2));
  console.log(`  ${segs.length} chord segments`);
}

// ════════════════════════════════════════════════════════════════════════
// 4 · SCALES — the teaching singalong (round 6). Spoken frame + the notepat
//     two-octave letter ladder (c d e f g a b · h i j k l m n = the second
//     octave, straight from notepat's NOTE_TO_KEYBOARD_KEY) sung up then
//     down at 108 BPM over a soft drone bed. The `lead` lane carries the
//     ladder at the STRIP's midis (C4..B5 — every one a real cached key);
//     jeffrey sings it an octave below (C3..B4, bright top, unstrained).
//     The bed stays light — a pedal drone, gentle kalimba, a quiet
//     vibraphone doubling the ladder — never burying the voice.
// ════════════════════════════════════════════════════════════════════════
{
  const BPM = 108, BEAT = 60 / BPM, BAR = 4 * BEAT;
  const { events, push } = makeTrack();

  // ── the shared timeline (spoken cues measured off the cached TTS takes) ──
  // R6·5: the spoken letter run is GONE (jeffrey's v1 review — intro
  // straight into the singing); the ladder starts a breath after the intro.
  const INTRO_T = 0.8;    // "Here's how to type out the C scale." (~2.1s)
  const SING0 = +(7 * BEAT).toFixed(4);   // ≈3.89 — the ladder's first beat
  const LETTERS_ASC = ["c", "d", "e", "f", "g", "a", "b",
    "h", "i", "j", "k", "l", "m", "n"];
  const STRIP_ASC = [60, 62, 64, 65, 67, 69, 71, 72, 74, 76, 77, 79, 81, 83];
  const HOLD_TOP = 2.0, HOLD_BOTTOM = 2.5;   // beats: top n rings, home c lands
  const ladder = [];
  LETTERS_ASC.forEach((letter, i) => {
    const top = i === LETTERS_ASC.length - 1;
    ladder.push({ letter, t: +(SING0 + i * BEAT).toFixed(4),
      dur: +((top ? HOLD_TOP : 0.92) * BEAT).toFixed(4),
      strip: STRIP_ASC[i], vocal: STRIP_ASC[i] - 12, dir: "up" });
  });
  // descending starts on m after the top-n hold (2 beats of ring)
  const DESC0 = SING0 + (LETTERS_ASC.length + 1) * BEAT;
  for (let j = 0; j < 13; j++) {
    const i = 12 - j;                      // m l k j i h b a g f e d c
    const bottom = j === 12;
    ladder.push({ letter: LETTERS_ASC[i], t: +(DESC0 + j * BEAT).toFixed(4),
      dur: +((bottom ? HOLD_BOTTOM : 0.92) * BEAT).toFixed(4),
      strip: STRIP_ASC[i], vocal: STRIP_ASC[i] - 12, dir: "down" });
  }
  const SING1 = DESC0 + 12 * BEAT + HOLD_BOTTOM * BEAT;   // ladder ends
  const OUTRO_T = +(SING1 + 0.9).toFixed(3);   // "Wanna type it yourself? …"

  // ── the bed ──────────────────────────────────────────────────────────────
  // pedal drone: soft C2/G2 bass, one note per bar, entering under the intro
  const LAST_BAR = Math.ceil((OUTRO_T + 3.2) / BAR);
  for (let bar = 0; bar < LAST_BAR; bar++) {
    const t = bar * BAR;
    push("bass", "bass", t, bar % 4 === 3 ? "G2" : "C2", 3.6 * BEAT, 0.20, 0);
  }
  // gentle kalimba broken chord, quiet, from bar 1 onward
  const ARP = ["C4", "E4", "G4", "A3"];
  for (let bar = 1; bar < LAST_BAR - 1; bar++) {
    for (let s = 0; s < 4; s++) {
      push("harmony", "kalimba", bar * BAR + s * BEAT, ARP[s % 4],
        0.9 * BEAT, 0.085, s % 2 ? 0.2 : -0.2, 1.0);
    }
  }
  // the ladder itself: lead lane at STRIP midis — lights the real keys and
  // doubles the voice an octave up, quietly
  for (const n of ladder) {
    push("lead", "vibraphone", n.t, n.strip, n.dur * 1.15, 0.22, 0.05, 1.25);
  }
  // soft woodblock ticks keep the singalong honest through the ladder bars
  for (let t = SING0; t < SING1 - BEAT; t += 2 * BEAT) {
    push("perc", "woodblock", +t.toFixed(4), "E5", 0.12, 0.06, 0.12);
  }
  // home-chord sparkle under the outro CTA
  push("harmony", "glockenspiel", OUTRO_T + 0.3, m("E6"), 2.0, 0.08, 0.28, 1.5);
  push("harmony", "glockenspiel", OUTRO_T + 0.9, m("C6"), 2.4, 0.09, -0.2, 1.5);

  const durationSec = master("menuband-scales", events, { title: "Menu Band C Scale Singalong" });
  writeScore("menuband-scales", events, {}, {
    bpm: BPM, beatSec: +BEAT.toFixed(6), barSec: +BAR.toFixed(6),
    durationSec: +durationSec.toFixed(4),
  });
  writeFileSync(resolve(OUT_DIR, "menuband-scales.score.json"), JSON.stringify({
    bpm: BPM, beatSec: +BEAT.toFixed(6), durationSec: +durationSec.toFixed(4),
    spoken: { intro: { t: INTRO_T }, outro: { t: OUTRO_T } },
    sing: { t0: +SING0.toFixed(4), t1: +SING1.toFixed(4) },
    ladder,
  }, null, 2));
  console.log(`  ladder ${ladder.length} letters · sing ${SING0.toFixed(2)}–${SING1.toFixed(2)}s · outro @ ${OUTRO_T}s`);
}
