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

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const eventsPath = `${ROOT}/out/waltz-events.json`;
const assPath = `${ROOT}/out/waltz-keys.ass`;

const { events, totalSec, bpm } = JSON.parse(readFileSync(eventsPath, "utf8"));

// ── layout ─────────────────────────────────────────────────────────────
// Bottom-left corner — TV-station-logo position, opposite the PALS mark
// in the bottom-right. Compact 4-octave keyboard (C2–B5 = MIDI 36–83)
// covering ~95% of the waltz events without crowding the slide content.
const FRAME_H = 1920;
const KB_W = 540;          // half-frame width
const KB_H = 90;           // logo-bug height
const KB_LEFT = 40;        // matches the chapter-text left margin
const KB_TOP = FRAME_H - KB_H - 130; // y=1700 — well clear of the progress bar
const MIDI_LOW = 60;       // C4
const MIDI_HIGH = 83;      // B5 inclusive
const N_WHITE = 14;        // 7 white keys × 2 octaves (notepat layout)
const MIN_HL_SEC = 0.18;   // floor on highlight duration so flashes register
const WHITE_W = KB_W / N_WHITE;
const BLACK_W = WHITE_W * 0.62;
const BLACK_H = KB_H * 0.62;

// White-key index for a midi pitch within an octave (C=0, D=1, E=2, F=3, G=4, A=5, B=6)
const WHITE_OF = { 0: 0, 2: 1, 4: 2, 5: 3, 7: 4, 9: 5, 11: 6 };
const isBlack = (midi) => [1, 3, 6, 8, 10].includes(midi % 12);

// X position (left edge) of a key on the keyboard, including the global
// KB_LEFT offset for the bottom-left logo placement.
function keyX(midi) {
  const semis = midi - MIDI_LOW;
  const octave = Math.floor(semis / 12);
  const inOct = semis % 12;
  if (!isBlack(midi)) {
    return KB_LEFT + (octave * 7 + WHITE_OF[inOct]) * WHITE_W;
  }
  // Black-key positioning: centered on the gap between two whites
  const leftWhite = ({ 1: 0, 3: 1, 6: 3, 8: 4, 10: 5 })[inOct];
  return KB_LEFT + (octave * 7 + leftWhite + 1) * WHITE_W - BLACK_W / 2;
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
const allEnd = assTime(tEnd);

// Layers (higher = drawn on top):
//   10 → white keys (background)
//   20 → white-key highlights
//   30 → black keys (must render OVER both white keys + white highlights)
//   40 → black-key highlights (top of stack)

// ── Layer 10: static white keys ────────────────────────────────────────
for (let m = MIDI_LOW; m <= MIDI_HIGH; m++) {
  if (isBlack(m)) continue;
  const x = keyX(m);
  lines.push(
    `Dialogue: 10,0:00:00.00,${allEnd},White,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${KB_TOP})\\bord1\\3c${WHITE_EDGE}\\1c${WHITE_FILL}\\p1}${drawRect(WHITE_W - 1, KB_H)}{\\p0}`
  );
}

// ── Layer 20: per-event WHITE-KEY highlights ──────────────────────────
for (const ev of events) {
  if (ev.midi < MIDI_LOW || ev.midi > MIDI_HIGH) continue;
  if (isBlack(ev.midi)) continue;
  const x = keyX(ev.midi);
  const dur = Math.max(ev.durSec, MIN_HL_SEC);
  const start = assTime(ev.startSec);
  const end = assTime(Math.min(ev.startSec + dur, tEnd));
  lines.push(
    `Dialogue: 20,${start},${end},Hl,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${KB_TOP})\\bord3\\3c${HL_BORDER}\\1c${colorFor(ev.midi)}\\p1}${drawRect(WHITE_W - 1, KB_H)}{\\p0}`
  );
}

// ── Layer 30: static black keys (over white-key highlights) ───────────
for (let m = MIDI_LOW; m <= MIDI_HIGH; m++) {
  if (!isBlack(m)) continue;
  const x = keyX(m);
  lines.push(
    `Dialogue: 30,0:00:00.00,${allEnd},Black,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${KB_TOP})\\bord0\\1c${BLACK_FILL}\\p1}${drawRect(BLACK_W, BLACK_H)}{\\p0}`
  );
}

// ── Layer 40: per-event BLACK-KEY highlights (top of stack) ───────────
for (const ev of events) {
  if (ev.midi < MIDI_LOW || ev.midi > MIDI_HIGH) continue;
  if (!isBlack(ev.midi)) continue;
  const x = keyX(ev.midi);
  const dur = Math.max(ev.durSec, MIN_HL_SEC);
  const start = assTime(ev.startSec);
  const end = assTime(Math.min(ev.startSec + dur, tEnd));
  lines.push(
    `Dialogue: 40,${start},${end},Hl,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${KB_TOP})\\bord2\\3c${HL_BORDER}\\1c${colorFor(ev.midi)}\\p1}${drawRect(BLACK_W, BLACK_H)}{\\p0}`
  );
}

writeFileSync(assPath, lines.join("\n") + "\n");
const eventsInRange = events.filter((e) => e.midi >= MIDI_LOW && e.midi <= MIDI_HIGH).length;
console.log(`✓ ${assPath} · ${eventsInRange}/${events.length} notes in range · ${totalSec.toFixed(1)}s · ${bpm} bpm`);
