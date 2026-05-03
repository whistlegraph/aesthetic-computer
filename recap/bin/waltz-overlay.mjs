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
//   - Top of frame, 1080 × 140 strip, anchored at y=24
//   - Two-octave window: MIDI C3 (48) through B4 (71) by default
//   - Notes outside the window are clipped at the edges (rare in this
//     waltz — bass goes to ~A1, melody to ~C6 — clip to the window for
//     a clean two-octave notepat readout)
//
// Key drawing:
//   - White keys: 14 of them across 1080 → ~77 px wide
//   - Black keys: laid over the boundaries, ~46 px wide × 80 px tall
//   - Highlights: chapter-color (cycling per octave-group) filled
//     rectangle on the key, slightly inset, full key height
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
const W = 1080;
const KB_TOP = 24;
const KB_H = 140;
const MIDI_LOW = 48;   // C3
const MIDI_HIGH = 71;  // B4 inclusive
const N_WHITE = 14;    // 7 white keys × 2 octaves
const WHITE_W = W / N_WHITE;
const BLACK_W = WHITE_W * 0.62;
const BLACK_H = KB_H * 0.62;

// White-key index for a midi pitch within an octave (C=0, D=1, E=2, F=3, G=4, A=5, B=6)
const WHITE_OF = { 0: 0, 2: 1, 4: 2, 5: 3, 7: 4, 9: 5, 11: 6 };
const isBlack = (midi) => [1, 3, 6, 8, 10].includes(midi % 12);

// X position (left edge) of a key on the keyboard
function keyX(midi) {
  const semis = midi - MIDI_LOW;
  const octave = Math.floor(semis / 12);
  const inOct = semis % 12;
  if (!isBlack(midi)) {
    return (octave * 7 + WHITE_OF[inOct]) * WHITE_W;
  }
  // Black-key positioning: centered on the gap between two whites
  const leftWhite = ({ 1: 0, 3: 1, 6: 3, 8: 4, 10: 5 })[inOct];
  return (octave * 7 + leftWhite + 1) * WHITE_W - BLACK_W / 2;
}

// ── ASS time format ────────────────────────────────────────────────────
function assTime(t) {
  const h = Math.floor(t / 3600);
  const m = Math.floor((t % 3600) / 60);
  const s = (t % 60).toFixed(2);
  return `${h}:${String(m).padStart(2, "0")}:${String(s).padStart(5, "0")}`;
}

// ASS color = &HAABBGGRR (alpha + BGR). For opaque colors use AA=00.
// Octave-coded hues so the eye groups bass vs melody.
const HUE_BY_OCTAVE = {
  3: "&H00C5F7FC", // cream → octave 3 (lower)
  4: "&H00B469FF", // magenta → octave 4 (upper)
  5: "&H00FFB4B4", // sky → fallbacks
  2: "&H0070F0E0", // cyan → bass that lands here
};
function colorFor(midi) {
  const oct = Math.floor(midi / 12) - 1; // C3 = MIDI 48 / 12 - 1 = 3
  return HUE_BY_OCTAVE[oct] || "&H00FFFFFF";
}

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

// ── Layer 0: static white-key keyboard background ──────────────────────
for (let m = MIDI_LOW; m <= MIDI_HIGH; m++) {
  if (isBlack(m)) continue;
  const x = keyX(m);
  // White keys: cream-on-faint-glow with a thin separator on the right
  lines.push(
    `Dialogue: 0,0:00:00.00,${allEnd},White,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${KB_TOP})\\bord1\\3c&H00282838&\\1c&H00F5F0E8&\\p1}${drawRect(WHITE_W - 1, KB_H)}{\\p0}`
  );
}

// ── Layer 1: static black-key keyboard background (drawn on top) ──────
for (let m = MIDI_LOW; m <= MIDI_HIGH; m++) {
  if (!isBlack(m)) continue;
  const x = keyX(m);
  lines.push(
    `Dialogue: 1,0:00:00.00,${allEnd},Black,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${KB_TOP})\\bord0\\1c&H00100810&\\p1}${drawRect(BLACK_W, BLACK_H)}{\\p0}`
  );
}

// ── Layer 2: per-event highlight overlays ──────────────────────────────
for (const ev of events) {
  if (ev.midi < MIDI_LOW || ev.midi > MIDI_HIGH) continue;
  const x = keyX(ev.midi);
  const w = isBlack(ev.midi) ? BLACK_W : WHITE_W - 1;
  const h = isBlack(ev.midi) ? BLACK_H : KB_H;
  const start = assTime(ev.startSec);
  const end = assTime(Math.min(ev.startSec + ev.durSec, tEnd));
  lines.push(
    `Dialogue: 2,${start},${end},Hl,,0,0,0,,{\\an7\\pos(${x.toFixed(1)},${KB_TOP})\\bord0\\1c${colorFor(ev.midi)}\\p1}${drawRect(w, h)}{\\p0}`
  );
}

writeFileSync(assPath, lines.join("\n") + "\n");
const eventsInRange = events.filter((e) => e.midi >= MIDI_LOW && e.midi <= MIDI_HIGH).length;
console.log(`✓ ${assPath} · ${eventsInRange}/${events.length} notes in range · ${totalSec.toFixed(1)}s · ${bpm} bpm`);
