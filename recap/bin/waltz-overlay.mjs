#!/usr/bin/env node
// waltz-overlay.mjs — render a live notepat-style piano-roll overlay for
// the per-cut waltz, sourced from the deterministic event list that
// `waltz.mjs` writes to `out/waltz-events.json`.
//
// Output: `out/waltz-keys.ass` — an Advanced SubStation Alpha file that
// libass can render as a single ffmpeg `subtitles=` filter on top of the
// composed video. Each note becomes a short Dialogue line drawing a
// filled rectangle on its key, alive for the note's duration.
//
// Layout (1080×1920 portrait):
//   - Bottom-left corner, TV-station-logo position (opposite the
//     PALS mark in the bottom-right): 540 × 90 keyboard at (40, 1700)
//   - 2-octave notepat window: MIDI C4 (60) through B5 (83)
//   - This window covers ~87% of the waltz's events (the chord triads,
//     the melody, and most extension tones) — bass (C2–A2, ~12% of
//     events) and very-top notes are clipped. The most-played region
//     in the actual waltz lives here, so the keyboard reads as a live
//     readout of "what you're hearing" rather than a piano survey.
//
// Key drawing:
//   - White keys: 14 across the 540-wide strip (~38 px each, 90 tall)
//   - Black keys: laid over the boundaries, ~24 px wide × ~56 tall
//   - Highlights: octave-coded hue + hot border, filled rectangle.
//     Short sixteenth-notes are extended to MIN_HL_SEC so the flash
//     is perceptible at this size (~38 px wide keys).
//
// Usage: node bin/waltz-overlay.mjs

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const eventsPath = `${ROOT}/out/waltz-events.json`;
const assPath = `${ROOT}/out/waltz-keys.ass`;
const layoutsPath = `${ROOT}/out/layouts.json`;
const segmentsPath = `${ROOT}/out/segments.json`;

const { events, totalSec, bpm } = JSON.parse(readFileSync(eventsPath, "utf8"));

// Per-segment layout from bin/layout.mjs — tells us where the piano roll
// should sit for each segment (and whether to hide it). Falls back to a
// single full-duration "default segment" when layouts.json is missing.
const layouts = existsSync(layoutsPath) ? JSON.parse(readFileSync(layoutsPath, "utf8")) : {};
const segmentList = existsSync(segmentsPath)
  ? JSON.parse(readFileSync(segmentsPath, "utf8"))
  : [{ name: "_default", startSec: 0, endSec: totalSec || 60 }];

// ── layout ─────────────────────────────────────────────────────────────
// Bottom-left corner — TV-station-logo position, opposite the PALS mark
// in the bottom-right. Compact 4-octave keyboard (C2–B5 = MIDI 36–83)
// covering ~95% of the waltz events without crowding the slide content.
const KB_W = 1080;         // full frame width — piano is the bottom strip
const KB_H = 90;           // 90 tall, sits flush to the bottom edge
const KB_LEFT_DEFAULT = 0;   // flush left
// Bottom-chrome stack (matches subtitles.mjs + build-filter.mjs):
//   subtitle pill : y=1480..1640
//   waveform      : y=1660..1740
//   piano roll    : y=1760..1850   ← THIS row (left half 40..580)
//   PALS bug      : y=1760..1850   ← right half 920..1010 (baked in slides.mjs)
//   progress bar  : y=1900..1908
// Piano flush to the bottom edge of the frame so the progress bar can
// sit on top of the keys (build-filter.mjs draws the progress segments
// AFTER the subtitles= filters so they layer over the piano roll).
const KB_TOP_DEFAULT = 1830; // KB_H=90 → keys at y=1830..1920
// 4-octave range: C2 to B5. Covers the waltz bass line (transposed C2-ish)
// up through the melody/ornament (transposed C5-ish). Was 2 octaves
// (C4-B5) which silently dropped every bass note from the visualizer.
const MIDI_LOW = 36;       // C2
const MIDI_HIGH = 83;      // B5 inclusive
const N_WHITE = 28;        // 7 white keys × 4 octaves
const MIN_HL_SEC = 0.18;   // floor on highlight duration so flashes register
const WHITE_W = KB_W / N_WHITE;
const BLACK_W = WHITE_W * 0.62;
const BLACK_H = KB_H * 0.62;

// White-key index for a midi pitch within an octave (C=0, D=1, E=2, F=3, G=4, A=5, B=6)
const WHITE_OF = { 0: 0, 2: 1, 4: 2, 5: 3, 7: 4, 9: 5, 11: 6 };
const isBlack = (midi) => [1, 3, 6, 8, 10].includes(midi % 12);

// X position (left edge) of a key on the keyboard, given the segment's
// kbLeft offset. Per-segment kbLeft lets the piano roll relocate to
// avoid jeffrey's face/laptop when the layout solver picks a different
// corner for a given segment.
function keyX(midi, kbLeft) {
  const semis = midi - MIDI_LOW;
  const octave = Math.floor(semis / 12);
  const inOct = semis % 12;
  if (!isBlack(midi)) {
    return kbLeft + (octave * 7 + WHITE_OF[inOct]) * WHITE_W;
  }
  // Black-key positioning: centered on the gap between two whites
  const leftWhite = ({ 1: 0, 3: 1, 6: 3, 8: 4, 10: 5 })[inOct];
  return kbLeft + (octave * 7 + leftWhite + 1) * WHITE_W - BLACK_W / 2;
}

// ── ASS time format ────────────────────────────────────────────────────
function assTime(t) {
  const h = Math.floor(t / 3600);
  const m = Math.floor((t % 3600) / 60);
  const s = (t % 60).toFixed(2);
  return `${h}:${String(m).padStart(2, "0")}:${String(s).padStart(5, "0")}`;
}

// ASS color = &HAABBGGRR& (alpha + BGR, with trailing `&`). The trailing
// `&` is REQUIRED when an override is directly followed by another
// override token like `\1c` / `\p1` — otherwise libass's parser will
// swallow the next backslash as part of the hex value and the override
// silently no-ops. (This bit us once: Layer 20 highlights were emitted
// fine but never rendered.)
//
// Octave-coded hues so the eye groups chord / melody / extension at a
// glance. These are deliberately punchy — at 38-px-wide keys, subtler
// shades blend into the cream/black keyboard.
const HUE_BY_OCTAVE = {
  2: "&H00E0E070&", // teal       → deep bass (rare in this 2-oct window)
  3: "&H0040F0FC&", // hot gold   → low chord (rare)
  4: "&H00FF40D0&", // hot magenta → chord triad + lower melody (most-played)
  5: "&H0070D0FF&", // hot cyan   → melody (top half of window)
  6: "&H006080FF&", // hot orange → upper melody (clipped)
};
function colorFor(midi) {
  const oct = Math.floor(midi / 12) - 1;
  return HUE_BY_OCTAVE[oct] || "&H00FFFFFF&";
}
// Border color for highlights — contrasting hot pink that pops over
// any of the fill hues above, so the active key is unmistakable.
const HL_BORDER = "&H00FF40A0&";
// Static-key colors (also with trailing `&` so they parse robustly).
const WHITE_FILL = "&H00F5F0E8&";
const WHITE_EDGE = "&H00282838&";
const BLACK_FILL = "&H00100810&";

// ASS drawing: a closed rectangle at given (x,y,w,h) using {\p1}m..l..{\p0}.
function drawRect(w, h) {
  return `m 0 0 l ${w} 0 ${w} ${h} 0 ${h}`;
}

// ── header ─────────────────────────────────────────────────────────────
const lines = [
  "[Script Info]",
  "ScriptType: v4.00+",
  "PlayResX: 1080",
  "PlayResY: 1920",
  "WrapStyle: 0",
  "ScaledBorderAndShadow: yes",
  "",
  "[V4+ Styles]",
  "Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding",
  "Style: White,Sans,1,&H00FFFFFF,&H00FFFFFF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,0,0,7,0,0,0,1",
  "Style: Black,Sans,1,&H00000000,&H00000000,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,0,0,7,0,0,0,1",
  "Style: Hl,Sans,1,&H00FFFFFF,&H00FFFFFF,&H00000000,&H00000000,0,0,0,0,100,100,0,0,1,0,0,7,0,0,0,1",
  "",
  "[Events]",
  "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text",
];

const tEnd = (totalSec || (events[events.length - 1]?.startSec + events[events.length - 1]?.durSec) || 60) + 1;

// Per-segment piano: layouts.json says where each segment's keyboard
// should land (or that it should be hidden because no good corner
// exists). We bracket every dialogue line by the segment's [start, end]
// so the keyboard moves with the narration arc — and disappears for
// segments where the laptop / face would crowd it out.
//
// Layers (higher = drawn on top):
//   10 → white keys (background) — bracketed per visible segment
//   20 → white-key highlights — bracketed per event ∩ segment
//   30 → black keys — bracketed per visible segment
//   40 → black-key highlights — bracketed per event ∩ segment
let visibleSegments = 0;
let hiddenSegments = 0;

for (const seg of segmentList) {
  const layout = layouts[seg.name];
  const piano = layout && layout.piano;
  if (piano && piano.hidden) { hiddenSegments++; continue; }

  const kbLeft = piano && Number.isFinite(piano.x) ? piano.x : KB_LEFT_DEFAULT;
  const kbTop  = piano && Number.isFinite(piano.y) ? piano.y : KB_TOP_DEFAULT;
  const segStart = assTime(Math.max(0, seg.startSec));
  const segEnd   = assTime(Math.min(tEnd, seg.endSec));
  visibleSegments++;

  // Layer 10: static white keys for this segment
  for (let m = MIDI_LOW; m <= MIDI_HIGH; m++) {
    if (isBlack(m)) continue;
    const x = keyX(m, kbLeft);
    lines.push(
      `Dialogue: 10,${segStart},${segEnd},White,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${kbTop})\\bord1\\3c${WHITE_EDGE}\\1c${WHITE_FILL}\\p1}${drawRect(WHITE_W - 1, KB_H)}{\\p0}`
    );
  }
  // Layer 20: white-key highlights for events that fall in this segment
  for (const ev of events) {
    if (ev.midi < MIDI_LOW || ev.midi > MIDI_HIGH) continue;
    if (isBlack(ev.midi)) continue;
    if (ev.startSec < seg.startSec || ev.startSec >= seg.endSec) continue;
    const x = keyX(ev.midi, kbLeft);
    const dur = Math.max(ev.durSec, MIN_HL_SEC);
    const start = assTime(ev.startSec);
    const end = assTime(Math.min(ev.startSec + dur, seg.endSec, tEnd));
    lines.push(
      `Dialogue: 20,${start},${end},Hl,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${kbTop})\\bord3\\3c${HL_BORDER}\\1c${colorFor(ev.midi)}\\p1}${drawRect(WHITE_W - 1, KB_H)}{\\p0}`
    );
  }
  // Layer 30: static black keys for this segment
  for (let m = MIDI_LOW; m <= MIDI_HIGH; m++) {
    if (!isBlack(m)) continue;
    const x = keyX(m, kbLeft);
    lines.push(
      `Dialogue: 30,${segStart},${segEnd},Black,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${kbTop})\\bord0\\1c${BLACK_FILL}\\p1}${drawRect(BLACK_W, BLACK_H)}{\\p0}`
    );
  }
  // Layer 40: black-key highlights for events that fall in this segment
  for (const ev of events) {
    if (ev.midi < MIDI_LOW || ev.midi > MIDI_HIGH) continue;
    if (!isBlack(ev.midi)) continue;
    if (ev.startSec < seg.startSec || ev.startSec >= seg.endSec) continue;
    const x = keyX(ev.midi, kbLeft);
    const dur = Math.max(ev.durSec, MIN_HL_SEC);
    const start = assTime(ev.startSec);
    const end = assTime(Math.min(ev.startSec + dur, seg.endSec, tEnd));
    lines.push(
      `Dialogue: 40,${start},${end},Hl,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${kbTop})\\bord2\\3c${HL_BORDER}\\1c${colorFor(ev.midi)}\\p1}${drawRect(BLACK_W, BLACK_H)}{\\p0}`
    );
  }
}

writeFileSync(assPath, lines.join("\n") + "\n");
const eventsInRange = events.filter((e) => e.midi >= MIDI_LOW && e.midi <= MIDI_HIGH).length;
console.log(`✓ ${assPath} · ${eventsInRange}/${events.length} notes in range · ${visibleSegments} visible / ${hiddenSegments} hidden segments · ${totalSec.toFixed(1)}s · ${bpm} bpm`);
