// clock.mjs — Native-friendly minimal melody clock for ac-native.
//
// This is a much simpler port of the web clock.mjs (disks/clock.mjs) which
// uses web-only APIs (hud, typeface, net.pieces, store.persist, api.send)
// that native doesn't provide — the web piece just throws inside paint() on
// native and leaves the screen black. This native version covers the core
// use case: parse the melody from params, play it on a repeating schedule,
// and show a compact per-note visualization.
//
// Usage: `clock:c:e:g`, `clock:cdefgab`, `clock:c:e:g:f`, etc.
// Params are joined with space to form the melody string the parser wants.

import { parseMelody } from "/lib/melody-parser.mjs";

// Chromatic note order per octave (matches notepat + melody-parser conventions)
const CHROMATIC = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"];

// Note → RGB for the visualization strip
const NOTE_COLORS = {
  c:   [255, 30, 30],  "c#": [255, 80, 0],
  d:   [255, 150, 0],  "d#": [200, 200, 0],
  e:   [230, 220, 0],
  f:   [30, 200, 30],  "f#": [0, 200, 180],
  g:   [30, 100, 255], "g#": [80, 50, 255],
  a:   [140, 30, 220], "a#": [200, 30, 150],
  b:   [200, 50, 255],
};

function noteToFreq(note, oct) {
  const idx = CHROMATIC.indexOf((note || "").toLowerCase());
  if (idx < 0) return 440;
  return 440 * Math.pow(2, (oct - 4) + (idx - 9) / 12);
}

function noteColor(n) {
  return NOTE_COLORS[(n || "").toLowerCase()] || [120, 120, 120];
}

let notes = [];            // parsed array: [{note, octave, duration, waveType, volume, ...}]
let melodyString = "";     // original string for display
let index = 0;             // current note index (wraps)
let frame = 0;
let nextFireFrame = 0;     // when to trigger the next note (frame count)
let activeSynth = null;    // handle for the currently-playing note (so we can kill it)
let bpm = 120;             // default tempo (can come from colon[0])
let noteDurationFrames = 30; // filled in from parsed duration + bpm
let lastFiredNote = null;  // {note, octave, color, frame} for paint feedback

function computeDurationFrames(noteDuration) {
  // melody-parser durations are in "beats" where 2 = quarter, 1 = eighth, etc.
  // Convert: frames = (60 / bpm) * (duration / 2) * 60 fps
  const beats = noteDuration || 2;
  const secs = (60.0 / bpm) * (beats / 2.0);
  return Math.max(4, Math.round(secs * 60));
}

function playCurrentNote(sound) {
  if (!notes.length) return;
  const n = notes[index];
  if (!n || !n.note) {
    index = (index + 1) % notes.length;
    return;
  }
  const freq = noteToFreq(n.note, n.octave || 4);
  const vol = n.volume != null ? n.volume : 0.6;
  const wave = n.waveType || "sine";
  const durationSecs = (noteDurationFrames / 60) * 0.95;  // slight gap between notes
  // Kill previous voice so notes don't overlap
  if (activeSynth) { sound?.kill?.(activeSynth, 0.02); activeSynth = null; }
  activeSynth = sound?.synth?.({
    type: wave,
    tone: freq,
    duration: durationSecs,
    volume: vol,
    attack: 0.005,
    decay: durationSecs * 0.6,
    pan: 0,
  });
  lastFiredNote = { note: n.note, octave: n.octave || 4, color: noteColor(n.note), frame };
}

function boot({ params, colon, wipe, screen, sound }) {
  wipe(0);
  // Tempo / divisor from colon[0] (matches web clock convention)
  const divisor = parseFloat(colon?.[0]);
  if (isFinite(divisor) && divisor > 0) bpm = Math.round(120 / divisor);
  // Join all params with space so "clock:c:e:g" → "c e g" → parsed as 3 notes
  melodyString = (params || []).join(" ").trim();
  if (!melodyString) {
    console.log("[clock] no melody param — silent mode");
    return;
  }
  try {
    notes = parseMelody(melodyString, 4) || [];
  } catch (e) {
    console.error("[clock] parse error:", e?.message || e);
    notes = [];
  }
  console.log(`[clock] parsed ${notes.length} notes from "${melodyString}" @ ${bpm} bpm`);
  index = 0;
  nextFireFrame = 2;  // fire the first note almost immediately
  if (notes.length > 0) {
    noteDurationFrames = computeDurationFrames(notes[0].duration);
  }
}

function sim({ sound }) {
  frame++;
  if (notes.length === 0) return;
  if (frame >= nextFireFrame) {
    playCurrentNote(sound);
    const n = notes[index];
    noteDurationFrames = computeDurationFrames(n?.duration);
    nextFireFrame = frame + noteDurationFrames;
    index = (index + 1) % notes.length;
  }
}

function paint({ wipe, ink, box, line, write, screen }) {
  const w = screen.width;
  const h = screen.height;
  // Background: deep black with a subtle current-note tint
  if (lastFiredNote) {
    const age = Math.max(0, 1 - (frame - lastFiredNote.frame) / 20);
    const [r, g, b] = lastFiredNote.color;
    ink(Math.floor(r * 0.12 * age), Math.floor(g * 0.12 * age), Math.floor(b * 0.15 * age));
    box(0, 0, w, h, true);
  } else {
    wipe(0);
  }

  if (notes.length === 0) {
    ink(180, 180, 180);
    write("clock (no melody)", { x: 6, y: 6, size: 1, font: "font_1" });
    ink(100, 100, 100);
    write("try: clock:c:e:g", { x: 6, y: 18, size: 1, font: "font_1" });
    return;
  }

  // Top strip: melody string
  ink(220, 220, 220);
  write(melodyString.slice(0, Math.floor(w / 6) - 2), { x: 4, y: 4, size: 1, font: "font_1" });
  ink(140, 140, 160);
  write(`${bpm}bpm  ${index + 1}/${notes.length}`, { x: 4, y: 14, size: 1, font: "font_1" });

  // Middle: stacked colored bars, one per note, current-note highlighted
  const stripTop = 28;
  const stripBottom = h - 28;
  const stripH = stripBottom - stripTop;
  const rowH = Math.max(4, Math.floor(stripH / Math.max(1, notes.length)));
  for (let i = 0; i < notes.length; i++) {
    const n = notes[i];
    const y = stripTop + i * rowH;
    if (y + rowH > stripBottom) break;
    const color = noteColor(n.note);
    const isCurrent = (i === (index - 1 + notes.length) % notes.length);
    const mul = isCurrent ? 1.0 : 0.32;
    ink(Math.floor(color[0] * mul), Math.floor(color[1] * mul), Math.floor(color[2] * mul));
    box(0, y, w, rowH - 1, true);
    if (isCurrent) {
      ink(255, 255, 255, 200);
      const label = (n.note || "-").toUpperCase() + (n.octave || 4);
      write(label, { x: w - label.length * 6 - 4, y: y + Math.floor((rowH - 10) / 2), size: 1, font: "font_1" });
    }
  }

  // Bottom hint
  ink(90, 90, 100);
  write("esc → prompt", { x: 4, y: h - 12, size: 1, font: "font_1" });
}

function leave({ sound }) {
  if (activeSynth) { sound?.kill?.(activeSynth, 0.05); activeSynth = null; }
}

export { boot, sim, paint, leave };
