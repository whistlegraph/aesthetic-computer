// starbaba.mjs — THEME & VARIATIONS on the twinkle star (à la Mozart K.265).
//
// C major, ~58 BPM feel, glockenspiel starlight identity preserved. The seed
// (marimbaba: F major, 3/4 — the "twinkle-little-star" lyric hidden inside
// MOTIFS.twinkle) is stated plainly, then put through a real escalating set of
// variations the way Mozart spun "Ah! vous dirai-je, Maman":
//
//   Theme  — the bare twinkle melody, glockenspiel stars over a warm bed.
//   Var I  — the same tune, lightly decorated; the bed answers in turn.
//   Var II — running 16ths swarm above the long melody notes (toccata sparkle).
//   Var III— the melody dissolved into broken arpeggios (Alberti starfields).
//   Var IV — the BASS takes the tune; high glints decorate it from above.
//   Var V  — a glittering DOUBLE-TIME finale, the whole sky cascading.
//   Coda   — a hushed final statement, the sky dimming to one last star.
//
// Everything stays in C major; the melody is remapped (not transposed) and
// then fragmented, diminished, re-rhythmed, octave-displaced and re-voiced
// hard — but the do-do-sol-sol-la-la-sol contour and the falling cadence
// stay recognizable as the thread.
//
// Run:  node variations/starbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

// ── mode remap: fold any midi to the nearest pitch in C major ──────────────
const ROOT = 0; // C
const MAJOR = [0, 2, 4, 5, 7, 9, 11];
function snap(midi) {
  const pc = ((midi - ROOT) % 12 + 12) % 12;
  let best = MAJOR[0], bestD = 99;
  for (const d of MAJOR) {
    const dist = Math.min(Math.abs(d - pc), 12 - Math.abs(d - pc));
    if (dist < bestD) { bestD = dist; best = d; }
  }
  return midi + (best - pc);
}
const snapName = (name, oct = 0) => snap(m(name) + oct * 12);
// the C-major scale as absolute midi, for arpeggio / scalar helpers
const SCALE = MAJOR;
function degToMidi(deg, baseOct = 5) {
  // deg 0 = C(baseOct); negative/large degrees wrap octaves
  const o = Math.floor(deg / 7);
  const within = ((deg % 7) + 7) % 7;
  return 12 * (baseOct + 1) + SCALE[within] + 12 * o;
}

// ── timing — 58 BPM; phrases of 4 beats (the rhyme's natural meter) ─────────
const BPM = 58;
const BEAT = 60 / BPM;
let BAR = 4 * BEAT;        // mutable so the final phrases can ritard

const ev = [];
let t = 0; // running cursor in seconds, advanced phrase by phrase

const ROSE_DEC = (DECAY.rosewood ?? 1.8) * 1.05; // warm, long ring
const GLOCK_DEC = 1.9;     // let the high stars hang in the air

// ── primitive emitters ─────────────────────────────────────────────────────

// glockenspiel star, up high — the melody itself (default two octaves up)
function star(beatIn, name, beats, gain = 0.3, octShift = 2, pan = 0.22) {
  ev.push({
    preset: "glockenspiel",
    startSec: t + beatIn * BEAT,
    midi: snapName(name, octShift),
    durSec: beats * BEAT * 1.1,
    gain,
    decayMul: GLOCK_DEC,
    pan,
  });
}
// star by absolute midi (for arpeggio runs computed in degrees)
function starMidi(beatIn, midi, beats, gain = 0.3, pan = 0.22, dec = GLOCK_DEC) {
  ev.push({ preset: "glockenspiel", startSec: t + beatIn * BEAT, midi: snap(midi), durSec: beats * BEAT * 1.1, gain, decayMul: dec, pan });
}
// faint shimmer — a single fragile re-strike, fills the gaps
function shimmer(beatIn, name, gain = 0.12, octShift = 2, pan = -0.26) {
  ev.push({ preset: "glockenspiel", startSec: t + beatIn * BEAT, midi: snapName(name, octShift), durSec: 0.45 * BEAT, gain, decayMul: GLOCK_DEC * 1.1, pan });
}
// soft rosewood bed (warm wood under the melody)
function bed(beatIn, name, beats, gain = 0.34, octShift = 0, pan = -0.06) {
  ev.push({ preset: "rosewood", startSec: t + beatIn * BEAT, midi: snapName(name, octShift), durSec: beats * BEAT * 1.15, gain, decayMul: ROSE_DEC, pan });
}
function bedMidi(beatIn, midi, beats, gain = 0.34, pan = -0.06) {
  ev.push({ preset: "rosewood", startSec: t + beatIn * BEAT, midi: snap(midi), durSec: beats * BEAT * 1.15, gain, decayMul: ROSE_DEC, pan });
}
// low rosewood root, grounding each phrase (kept low + clear, no remap)
function root(name, gain = 0.32, beats = null) {
  ev.push({ preset: "rosewood", startSec: t, midi: m(name), durSec: (beats ? beats * BEAT : BAR) * 1.1, gain, decayMul: ROSE_DEC * 1.1, pan: 0 });
}
// deeper bass voice for the Var IV bass-melody
function bassNote(beatIn, name, beats, gain = 0.4, pan = 0) {
  ev.push({ preset: "bass", startSec: t + beatIn * BEAT, midi: snapName(name, 0), durSec: beats * BEAT * 1.1, gain, decayMul: (DECAY.bass ?? 1.8) * 1.05, pan });
}

const nextPhrase = (mul = 1) => { t += BAR * mul; };

// ── the THEME as scale-degree cells (C major), the recognizable thread ──────
// "twinkle twinkle little star":  C C G G A A G   (do do sol sol la la sol)
// "how I wonder what you are":    F F E E D D C
// "up above the world so high":   G G F F E E D
// "like a diamond in the sky":    G G F F E E D
const L1 = [["C5", .5], ["C5", .5], ["G5", .5], ["G5", .5], ["A5", .5], ["A5", .5], ["G5", 1]];
const L2 = [["F5", .5], ["F5", .5], ["E5", .5], ["E5", .5], ["D5", .5], ["D5", .5], ["C5", 1]];
const L3 = [["G5", .5], ["G5", .5], ["F5", .5], ["F5", .5], ["E5", .5], ["E5", .5], ["D5", 1]];
// the chord under each line (root names for bass) and a bed-degree helper
const CHORD = { I: [0, 2, 4], IV: [3, 5, 0], V: [4, 6, 1], ii: [1, 3, 5], vi: [5, 0, 2] };

// =====================================================================
// INTRO — a single distant star wakes, a soft rising glint
// =====================================================================
root("C2", 0.24);
star(0.5, "C5", 1, 0.2);
shimmer(1.6, "E5", 0.11);
star(2.2, "G5", 1.6, 0.22);
shimmer(3.2, "C6", 0.11);
nextPhrase();

// =====================================================================
// THEME — the bare twinkle melody, stars over a warm bed
// =====================================================================
function themeLine(cells, rootName, bedShift = -1, gStar = 0.3, gBed = 0.3) {
  root(rootName, 0.3);
  let b = 0;
  for (const [name, beats] of cells) {
    star(b, name, beats, gStar);
    bed(b, name, beats, gBed, bedShift);
    b += beats;
  }
}
themeLine(L1, "C2"); nextPhrase();
themeLine(L2, "F2"); nextPhrase();
themeLine(L3, "G2"); nextPhrase();
// line 4 = repeat of L3 ("like a diamond") — close the theme on a half-lift
themeLine(L3, "G2"); shimmer(3.4, "D6", 0.1, 2); nextPhrase();

// =====================================================================
// VARIATION I — same tune, lightly decorated; bed ANSWERS in call/response
// each melody note gets a tiny grace glint; the rosewood echoes a beat late.
// =====================================================================
function varI(cells, rootName) {
  root(rootName, 0.28);
  let b = 0;
  for (const [name, beats] of cells) {
    star(b, name, beats, 0.28);
    // grace-note flurry just before the beat (upper neighbor)
    shimmer(b - 0.12, name, 0.08, 2);
    // bed answers a half-beat later, an octave under — the "response"
    bed(b + 0.5, name, Math.max(beats, 0.5), 0.22, -1, 0.1);
    b += beats;
  }
}
varI(L1, "C2"); nextPhrase();
varI(L2, "F2"); nextPhrase();

// =====================================================================
// VARIATION II — running 16ths SWARM above the long held melody notes.
// the melody is augmented (held) in the bed; a glockenspiel toccata of
// step-wise 16ths sparkles overhead (diminution of the contour).
// =====================================================================
function varII(cells, rootName, climbBase) {
  root(rootName, 0.26);
  // long, augmented melody underneath (bed holds each note longer)
  let b = 0;
  for (const [name, beats] of cells) {
    bed(b, name, beats, 0.3, -1);
    b += beats;
  }
  // a continuous 16th-note swarm of stars across the whole 4 beats:
  // an up-and-down scalar wave that orbits the harmony's top notes.
  const STEPS = 16; // sixteenth notes
  for (let i = 0; i < STEPS; i++) {
    const phase = i / STEPS;
    // triangle wave 0..6..0 over the C-major scale, riding upward each bar
    const tri = Math.round(6 * (1 - Math.abs(1 - 2 * phase)));
    const deg = climbBase + tri;
    const g = 0.14 + 0.05 * (1 - Math.abs(1 - 2 * phase)); // swell mid-phrase
    starMidi(i * 0.25, degToMidi(deg, 6), 0.3, g, i % 2 ? 0.28 : -0.24);
  }
}
varII(L1, "C2", 0);  nextPhrase();
varII(L2, "F2", 3);  nextPhrase(); // start the swarm from F

// =====================================================================
// VARIATION III — the melody DISSOLVED into broken arpeggios (Alberti star-
// fields). each beat is a rolled chord of the harmony, the melody note on top;
// rosewood walks the bass line. the contour survives as the top of each chord.
// =====================================================================
function arpBeat(beatIn, chordDegs, topName, gain = 0.2) {
  // roll low→high inside a beat: 4 sixteenths, top = the melody note
  const base = 5;
  const ds = chordDegs;
  for (let i = 0; i < 3; i++) {
    starMidi(beatIn + i * 0.18, degToMidi(ds[i % ds.length] + 7, base), 0.6, gain, i % 2 ? 0.3 : -0.3);
  }
  // melody note crowns the beat, brighter, up high
  star(beatIn + 0.5, topName, 0.6, gain + 0.12, 2);
}
function varIII(cells, rootName, chordSeq) {
  root(rootName, 0.26);
  let b = 0, ci = 0;
  for (const [name, beats] of cells) {
    const ch = chordSeq[ci % chordSeq.length];
    arpBeat(b, CHORD[ch], name, 0.18);
    // rosewood walking bass under the broken chords
    bed(b, name, beats, 0.2, -2, -0.04);
    b += beats; ci++;
  }
}
// I–I–V–V–vi–vi–V over line 1
varIII(L1, "C2", ["I", "I", "V", "V", "vi", "vi", "V"]); nextPhrase();
// IV–IV–I–I–ii–ii–V over line 2
varIII(L2, "F2", ["IV", "IV", "I", "I", "ii", "ii", "V"]); nextPhrase();

// =====================================================================
// VARIATION IV — the BASS takes the tune (octave-displaced down, augmented),
// while high glints decorate it from above (inverted shimmer counter-melody).
// =====================================================================
function varIV(cells, rootName) {
  root(rootName, 0.2); // lighter root so the bass-melody reads
  let b = 0;
  for (const [name, beats] of cells) {
    // melody in the bass, two octaves down from where stars sang it
    bassNote(b, name, Math.max(beats * 1.4, 0.7), 0.34, -0.05);
    // a single high decorating star, inverted contour (mirror around G5)
    const pivot = m("G5");
    const inv = 2 * pivot - m(name);
    starMidi(b + 0.45, snap(inv) + 24, 0.5, 0.16, 0.3);
    if (beats >= 1) shimmer(b + 0.7, name, 0.08, 3, -0.3);
    b += beats;
  }
}
varIV(L1, "C2"); nextPhrase();
varIV(L3, "G2"); nextPhrase();

// =====================================================================
// VARIATION V — glittering DOUBLE-TIME finale. the melody compressed into a
// half-bar, then echoed, the whole sky cascading: stars + bed + sparkle storm.
// =====================================================================
function varV(cells, rootName, chordDegs) {
  root(rootName, 0.3);
  // compress the 7-note line into 2 beats (diminution), then play it TWICE
  const compressed = cells.map(([n, beats]) => [n, beats * 0.5]);
  let half = 0;
  for (let rep = 0; rep < 2; rep++) {
    let b = half;
    for (const [name, beats] of compressed) {
      star(b, name, beats, 0.26, 2, rep ? 0.3 : -0.3);
      if (rep === 0) bed(b, name, Math.max(beats, 0.4), 0.22, -1);
      b += beats;
    }
    half = b; // second statement starts where the first ended
  }
  // a cascading 16th arpeggio storm beneath, sweeping the chord top to bottom
  for (let i = 0; i < 16; i++) {
    const deg = chordDegs[i % chordDegs.length] + 7 + (i < 8 ? 7 : 0);
    starMidi(i * 0.25, degToMidi(deg, 5), 0.3, 0.1, i % 2 ? 0.32 : -0.32);
  }
}
varV(L1, "C2", CHORD.I);  nextPhrase();
varV(L3, "G2", CHORD.V);  nextPhrase();
// finale tag — a single rocketing C-major run up the sky, then a bright peak
{
  root("C2", 0.28);
  for (let i = 0; i < 12; i++) {
    starMidi(i * 0.22, degToMidi(i, 5) + 7, 0.5, 0.12 + i * 0.006, i % 2 ? 0.3 : -0.3);
  }
  bed(0, "C4", 4, 0.24, 0);
  bed(0, "G4", 4, 0.2, 0);
  star(3.0, "C5", 1.5, 0.3, 2); // the topmost star, the climax
}
nextPhrase();

// =====================================================================
// CODA — a hushed FINAL STATEMENT, the sky dimming to one last star.
// MOTIFS.hush descending sigh, plain twinkle head, ritarding.
// =====================================================================
BAR *= 1.18; // begin the ritard
{
  root("C2", 0.26);
  // the bare twinkle head one more time, slow + quiet (theme recalled)
  const head = [["C5", 1], ["C5", 1], ["G5", 1], ["G5", 1]];
  let b = 0;
  for (const [name, beats] of head) {
    star(b, name, beats, 0.18, 2);
    bed(b, name, beats, 0.22, -1);
    b += beats;
  }
}
nextPhrase();

BAR *= 1.25;
{
  root("C2", 0.26);
  const hush = MOTIFS.hush; // [["C5",1],["A4",1],["F4",1],["F4",3]]
  let b = 0;
  for (const [name, beats] of hush.slice(0, 3)) {
    bed(b, name, beats, 0.3, 0);
    star(b, name, beats, 0.14, 1);
    b += beats;
  }
}
nextPhrase();

// final resting chord — low C major, a last lone star fading
BAR *= 1.35;
root("C2", 0.28);
bed(0, "E4", BAR / BEAT, 0.24, 0);
bed(0, "G4", BAR / BEAT, 0.2, 0);
bed(0, "C5", BAR / BEAT, 0.22, 0);
star(0.4, "C5", 2, 0.16, 2);
shimmer(2.2, "G5", 0.09, 2);
shimmer(3.6, "E5", 0.07, 2);
nextPhrase();

// the very last star — a single distant point, soft and high
ev.push({
  preset: "glockenspiel",
  startSec: t + 0.4 * BEAT,
  midi: snapName("C5", 2),
  durSec: 1.0 * BEAT,
  gain: 0.1,
  decayMul: GLOCK_DEC * 1.3,
  pan: 0.18,
});

// ── render ─────────────────────────────────────────────────────────────────
const { mp3, durationSec } = renderLullaby(ev, {
  name: "starbaba",
  here: HERE,
  title: "starbaba",
  reverb: { wet: 0.36, decay: 0.86, damp: 0.3 }, // wide, glassy, starlit air
  fadeIn: 0.7,
  fadeOut: 4.8,
  tailSec: 5.5,
  peak: 0.82,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
