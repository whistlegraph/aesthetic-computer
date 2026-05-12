// babypat.mjs — a baby's notepat. Every key plays a sound, every press
// changes the whole-screen background color, and the letter you pressed
// splashes onto the screen. No modes, no menus, no effects.
//
// The note layout is THE SAME as notepat.mjs so muscle-memory carries
// over once the baby graduates. Letter keys + ; ' ] are all musical
// notes (piano voice). Numbers play drums, arrows swoosh, space kicks.
// Triple-press escape within 2 s to bail back to the prompt.

import { playPercussion } from "/lib/percussion.mjs";

// ── notepat note layout (mirrors NOTE_TO_KEY in notepat.mjs) ──────
// Each letter / punctuation key → note. Octave shift is parsed from the
// "+", "++", or "-" prefix, then folded into a real octave number so we
// can compute frequency. Base octave is 4 (c=C4, +c=C5, ++c=C6, -a#=A#3).
const KEY_NOTE = {
  // base octave (4)
  a: "a", b: "b", c: "c", d: "d", e: "e", f: "f", g: "g",
  q: "a#", r: "g#", s: "d#", v: "c#", w: "f#",
  // upper octave (+, octave 5)
  h: "+c", i: "+d", j: "+e", k: "+f", l: "+g",
  m: "+a", n: "+b", o: "+g#", p: "+a#",
  t: "+c#", u: "+f#", y: "+d#",
  // highest (++, octave 6)
  ";": "++c", "'": "++c#", "]": "++d",
  // sub-base (-, octave 3)
  z: "-a#", x: "-b",
};

const CHROMATIC = ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"];

// Notepat's note-color palette — keeps the chromatic rainbow consistent
// across the two pieces so the baby sees the same color for the same
// note in notepat later.
const NOTE_COLORS = {
  c:    [255,  30,  30], "c#": [255,  80,   0],
  d:    [255, 150,   0], "d#": [200, 200,   0],
  e:    [230, 220,   0],
  f:    [ 30, 200,  30], "f#": [  0, 200, 180],
  g:    [ 30, 100, 255], "g#": [ 80,  50, 255],
  a:    [140,  30, 220], "a#": [200,  30, 150],
  b:    [255,  60, 120],
};

// Non-note keys — drums, swooshes, kicks. These keep babypat's fun side
// while preserving the notepat-learning layout for the letters.
//   kind="perc"  → playPercussion(sound, arg)
//   kind="tone"  → sound.synth({ type, tone })
const EXTRA = {
  // number row — 10 drum hits for satisfying chaos
  "1": { kind: "perc", arg: "c",  color: [255, 140, 100] }, // kick
  "2": { kind: "perc", arg: "d",  color: [255, 200, 100] }, // snare
  "3": { kind: "perc", arg: "e",  color: [255, 240, 140] }, // clap
  "4": { kind: "perc", arg: "f",  color: [200, 255, 140] }, // snap
  "5": { kind: "perc", arg: "g",  color: [140, 240, 200] }, // hat-c
  "6": { kind: "perc", arg: "a",  color: [140, 220, 255] }, // hat-o
  "7": { kind: "perc", arg: "b",  color: [180, 180, 255] }, // ride
  "8": { kind: "perc", arg: "c#", color: [220, 140, 255] }, // crash
  "9": { kind: "perc", arg: "d#", color: [255, 140, 220] }, // splash
  "0": { kind: "perc", arg: "f#", color: [255, 200,  80] }, // cowbell
  // big satisfying kick on space
  " ":        { kind: "perc", arg: "c",            color: [255, 255, 255] },
  // arrows — directional sawtooth swooshes
  arrowleft:  { kind: "tone", wave: "sawtooth", tone: 220, color: [255, 100, 200] },
  arrowright: { kind: "tone", wave: "sawtooth", tone: 660, color: [100, 255, 200] },
  arrowup:    { kind: "tone", wave: "sawtooth", tone: 880, color: [255, 200, 100] },
  arrowdown:  { kind: "tone", wave: "sawtooth", tone: 165, color: [200, 100, 255] },
};

// Parse a note token like "c", "c#", "+c", "++c", "-a#" → [letter, octave]
function parseNote(token) {
  let oct = 4;
  let t = token;
  if (t.startsWith("++")) { oct = 6; t = t.slice(2); }
  else if (t.startsWith("+")) { oct = 5; t = t.slice(1); }
  else if (t.startsWith("-")) { oct = 3; t = t.slice(1); }
  return [t, oct];
}

// Equal-tempered A=440 Hz frequency for any chromatic-letter + octave.
function noteToFreq(letter, oct) {
  const idx = CHROMATIC.indexOf(letter);
  if (idx < 0) return 440;
  return 440 * Math.pow(2, (oct - 4) + (idx - 9) / 12);
}

// ── state ─────────────────────────────────────────────────────────
const SPLASH_LIFE = 30;
const BG_FADE = 28;          // frames it takes the bg flash to settle
const splashes = [];
let frame = 0;
let bgColor = [12, 12, 22];  // current settled background
let bgFlash = [12, 12, 22];  // most-recent press color (fades into bgColor)
let bgFlashFrame = -1000;

// Triple-escape exit
let escCount = 0;
let lastEscFrame = -1000;

function rand(min, max) {
  return min + Math.floor(Math.random() * (max - min));
}

function spawnSplash(label, color, screen) {
  splashes.push({
    label,
    color,
    x: rand(20, Math.max(40, screen.width - 60)),
    y: rand(20, Math.max(40, screen.height - 40)),
    born: frame,
  });
  if (splashes.length > 64) splashes.splice(0, splashes.length - 64);
}

function setBg(color) {
  bgFlash = color;
  bgFlashFrame = frame;
  // Soft the settled bg toward the press color so successive taps drift
  // through hues instead of always snapping back to deep navy.
  bgColor = [
    Math.floor(bgColor[0] * 0.5 + color[0] * 0.15),
    Math.floor(bgColor[1] * 0.5 + color[1] * 0.15),
    Math.floor(bgColor[2] * 0.5 + color[2] * 0.15),
  ];
}

function playNote(sound, key, pan) {
  const token = KEY_NOTE[key];
  if (!token) return null;
  const [letter, oct] = parseNote(token);
  const freq = noteToFreq(letter, oct);
  // Piano voice — matches the bank baked into the OTA. Short percussive
  // envelope so each press is a clean strike, not a sustained hold.
  sound?.synth?.({
    type: "piano",
    tone: freq,
    duration: 0.6,
    volume: 0.9,
    attack: 0.001,
    decay: 0.55,
    pan,
  });
  return { letter, oct, color: NOTE_COLORS[letter] || [200, 200, 200] };
}

function playExtra(sound, entry, pan) {
  if (entry.kind === "perc") {
    playPercussion(sound, entry.arg, 1.2, pan);
  } else if (entry.kind === "tone") {
    sound?.synth?.({
      type: entry.wave || "sine",
      tone: entry.tone,
      duration: 0.25,
      volume: 0.45,
      attack: 0.005,
      decay: 0.22,
      pan,
    });
  }
}

function boot() {
  frame = 0;
  splashes.length = 0;
  escCount = 0;
  bgColor = [12, 12, 22];
  bgFlash = bgColor;
}

function act({ event: e, sound, screen, system }) {
  // Triple-escape → prompt
  if (e.is("keyboard:down:escape")) {
    if (frame - lastEscFrame > 120) escCount = 0;
    escCount++;
    lastEscFrame = frame;
    if (escCount >= 3) {
      system?.jump?.("prompt");
      return;
    }
    sound?.synth?.({
      type: "triangle",
      tone: 440 + escCount * 120,
      duration: 0.08, volume: 0.25, attack: 0.002, decay: 0.07, pan: 0,
    });
    spawnSplash(`esc ${escCount}/3`, [255, 80, 80], screen);
    setBg([60, 0, 0]);
    return;
  }

  if (!e.is("keyboard:down")) return;
  const key = String(e.key || "").toLowerCase();

  // Stereo pan: notes pan by chromatic position (low→left, high→right);
  // non-note keys pan center.
  let pan = 0;
  let label, color;

  const noteToken = KEY_NOTE[key];
  if (noteToken) {
    const [letter, oct] = parseNote(noteToken);
    // Pan: c3→-0.9, c6→+0.9, scaled by 36 chromatic steps from C3 to C6.
    const idx = CHROMATIC.indexOf(letter);
    const semitone = (oct - 3) * 12 + idx;       // 0..35 ish
    pan = Math.max(-1, Math.min(1, (semitone - 18) / 18 * 0.9));
    const info = playNote(sound, key, pan);
    if (!info) return;
    color = info.color;
    label = key === ";" ? "C" : key === "'" ? "C#" : key === "]" ? "D"
          : key.toUpperCase();
  } else {
    const entry = EXTRA[key];
    if (!entry) return;
    playExtra(sound, entry, 0);
    color = entry.color;
    label = key === " " ? "␣"
          : key.startsWith("arrow")
            ? { arrowleft:"◀", arrowright:"▶", arrowup:"▲", arrowdown:"▼" }[key]
            : key.toUpperCase();
  }

  spawnSplash(label, color, screen);
  setBg(color);
}

function paint({ wipe, ink, write }) {
  frame++;

  // Background flash: fade from the most-recent press color back toward
  // the drifting settled color over BG_FADE frames. Each press paints
  // the whole screen for a moment before settling.
  const age = frame - bgFlashFrame;
  let bg;
  if (age < BG_FADE) {
    const t = age / BG_FADE;
    bg = [
      Math.floor(bgFlash[0] * (1 - t) + bgColor[0] * t),
      Math.floor(bgFlash[1] * (1 - t) + bgColor[1] * t),
      Math.floor(bgFlash[2] * (1 - t) + bgColor[2] * t),
    ];
  } else {
    bg = bgColor;
  }
  wipe(bg[0], bg[1], bg[2]);

  for (let i = 0; i < splashes.length; i++) {
    const s = splashes[i];
    const a = frame - s.born;
    if (a > SPLASH_LIFE) continue;
    const t = a / SPLASH_LIFE;
    const alpha = Math.floor((1 - t) * 255);
    const [r, g, b] = s.color;
    ink(r, g, b, alpha);
    write(s.label, { x: s.x, y: s.y, size: 4, font: "font_1" });
  }
  while (splashes.length > 0 && frame - splashes[0].born > SPLASH_LIFE) {
    splashes.shift();
  }
}

function sim() {}
function leave() { splashes.length = 0; }

export { boot, paint, act, sim, leave };
