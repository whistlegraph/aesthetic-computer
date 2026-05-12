// babypat.mjs — every key makes a sound, every press paints a splash.
//
// Designed for the smallest hands: no modes, no menus, no effects. Mash
// any key and you get a sound effect plus a colored letter on screen.
// Triple-press escape (within 2 s) to bail back to the prompt — keeps
// curious fingers from accidentally exiting after one tap.
//
// Sound bank: 26 letters + 10 number keys + space/arrows. Letters split
// across zoo samples, percussion kit, and a handful of synth chirps so
// every press feels distinct.

import { playPercussion } from "/lib/percussion.mjs";

// ── key → sound bank ──────────────────────────────────────────────
// Each entry is { kind, arg, color }.
//   kind="zoo"   → sound.zoo.play(arg)         (uses the baked samples)
//   kind="perc"  → playPercussion(sound, arg)  (synth drum kit)
//   kind="tone"  → sound.synth({ type, tone })  (synth chirp at tone Hz)
const KEYS = {
  // top row — numbers play a happy C-major run, low to high
  "1": { kind: "tone", wave: "sine",     tone: 262, color: [255, 100, 100] }, // C4
  "2": { kind: "tone", wave: "sine",     tone: 294, color: [255, 160,  80] }, // D4
  "3": { kind: "tone", wave: "sine",     tone: 330, color: [255, 220,  80] }, // E4
  "4": { kind: "tone", wave: "sine",     tone: 349, color: [180, 255,  80] }, // F4
  "5": { kind: "tone", wave: "sine",     tone: 392, color: [100, 255, 140] }, // G4
  "6": { kind: "tone", wave: "sine",     tone: 440, color: [100, 220, 255] }, // A4
  "7": { kind: "tone", wave: "sine",     tone: 494, color: [140, 160, 255] }, // B4
  "8": { kind: "tone", wave: "sine",     tone: 523, color: [200, 120, 255] }, // C5
  "9": { kind: "tone", wave: "sine",     tone: 587, color: [255, 140, 220] }, // D5
  "0": { kind: "tone", wave: "sine",     tone: 659, color: [255, 200, 200] }, // E5

  // qwerty row — zoo animals (10 of 12)
  q: { kind: "zoo", arg: "dog",   color: [200, 130,  80] },
  w: { kind: "zoo", arg: "cat",   color: [230, 180, 220] },
  e: { kind: "zoo", arg: "cow",   color: [220, 200, 170] },
  r: { kind: "zoo", arg: "sheep", color: [240, 240, 220] },
  t: { kind: "zoo", arg: "bird",  color: [140, 220, 220] },
  y: { kind: "zoo", arg: "pig",   color: [240, 180, 180] },
  u: { kind: "zoo", arg: "lion",  color: [220, 170,  90] },
  i: { kind: "zoo", arg: "owl",   color: [120, 120, 180] },
  o: { kind: "zoo", arg: "frog",  color: [140, 220, 140] },
  p: { kind: "zoo", arg: "horse", color: [170, 120,  80] },

  // home row — percussion kit
  a: { kind: "perc", arg: "c",   color: [255, 140, 100] }, // kick
  s: { kind: "perc", arg: "d",   color: [255, 200, 100] }, // snare
  d: { kind: "perc", arg: "e",   color: [255, 240, 140] }, // clap
  f: { kind: "perc", arg: "f",   color: [200, 255, 140] }, // snap
  g: { kind: "perc", arg: "g",   color: [140, 240, 200] }, // hat-c
  h: { kind: "perc", arg: "a",   color: [140, 220, 255] }, // hat-o
  j: { kind: "perc", arg: "b",   color: [180, 180, 255] }, // ride
  k: { kind: "perc", arg: "c#",  color: [220, 140, 255] }, // crash
  l: { kind: "perc", arg: "d#",  color: [255, 140, 220] }, // splash

  // bottom row — remaining zoo + extra perc + synth chirps
  z: { kind: "zoo",  arg: "snake", color: [170, 230, 130] },
  x: { kind: "zoo",  arg: "whale", color: [100, 180, 230] },
  c: { kind: "perc", arg: "f#",    color: [255, 220, 100] }, // cowbell
  v: { kind: "perc", arg: "g#",    color: [200, 200, 200] }, // block
  b: { kind: "perc", arg: "a#",    color: [255, 200, 240] }, // tambo
  n: { kind: "tone", wave: "triangle", tone: 880,  color: [180, 255, 255] },
  m: { kind: "tone", wave: "triangle", tone: 440,  color: [255, 200, 100] },

  // space — big satisfying kick
  " ":        { kind: "perc", arg: "c",  color: [255, 255, 255] },
  // arrows — directional swooshes
  arrowleft:  { kind: "tone", wave: "sawtooth", tone: 220, color: [255, 100, 200] },
  arrowright: { kind: "tone", wave: "sawtooth", tone: 660, color: [100, 255, 200] },
  arrowup:    { kind: "tone", wave: "sawtooth", tone: 880, color: [255, 200, 100] },
  arrowdown:  { kind: "tone", wave: "sawtooth", tone: 165, color: [200, 100, 255] },
};

// ── splash state ──────────────────────────────────────────────────
// Each press spawns a splash that fades over ~30 frames. We keep them
// in a small ring so concurrent presses overlap naturally.
const SPLASH_LIFE = 30;
const splashes = [];
let frame = 0;

// Triple-escape exit. Three presses within 2 s (120 frames) jumps to
// the prompt; otherwise the counter resets so a stray tap won't bail.
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
  // Cap so memory doesn't grow if a key is held / repeated rapidly.
  if (splashes.length > 64) splashes.splice(0, splashes.length - 64);
}

function triggerSound(sound, entry, pan = 0) {
  if (!entry) return;
  if (entry.kind === "zoo") {
    sound?.zoo?.play?.(entry.arg, { volume: 1.0, pan, pitch: 1.0 });
  } else if (entry.kind === "perc") {
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
}

function act({ event: e, sound, screen, system }) {
  // Triple-escape → prompt. Reset if it's been more than 2 s.
  if (e.is("keyboard:down:escape")) {
    if (frame - lastEscFrame > 120) escCount = 0;
    escCount++;
    lastEscFrame = frame;
    if (escCount >= 3) {
      system?.jump?.("prompt");
      return;
    }
    // Audible feedback so the user knows the count is climbing.
    sound?.synth?.({
      type: "triangle",
      tone: 440 + escCount * 120,
      duration: 0.08, volume: 0.25, attack: 0.002, decay: 0.07, pan: 0,
    });
    spawnSplash(`esc ${escCount}/3`, [255, 80, 80], screen);
    return;
  }

  // Any other key → sound + splash. Match the generic key-down event
  // and pull the actual key name from e.key (lowercased — matches the
  // KEYS table).
  if (e.is("keyboard:down")) {
    const key = String(e.key || "").toLowerCase();
    const entry = KEYS[key];
    if (!entry) return;
    // Stereo pan based on letter position (a=left, z=right) for tactile
    // spatial feel.
    let pan = 0;
    if (key.length === 1 && key >= "a" && key <= "z") {
      pan = (key.charCodeAt(0) - 97) / 25 * 1.4 - 0.7;
    }
    triggerSound(sound, entry, pan);
    const label = key === " " ? "␣"
                : key.startsWith("arrow") ? { arrowleft:"◀", arrowright:"▶", arrowup:"▲", arrowdown:"▼" }[key]
                : key.toUpperCase();
    spawnSplash(label, entry.color, screen);
  }
}

function paint({ wipe, ink, write }) {
  frame++;

  // Soft fade so trails dissolve cleanly without staying as a blur.
  wipe(8, 8, 14);

  for (let i = 0; i < splashes.length; i++) {
    const s = splashes[i];
    const age = frame - s.born;
    if (age > SPLASH_LIFE) continue;
    const t = age / SPLASH_LIFE; // 0 → 1
    const alpha = Math.floor((1 - t) * 255);
    const [r, g, b] = s.color;
    ink(r, g, b, alpha);
    // Big block letter. font_1 at size 4 ≈ 24×40 px per char.
    write(s.label, { x: s.x, y: s.y, size: 4, font: "font_1" });
  }
  // Drop expired splashes.
  while (splashes.length > 0 && frame - splashes[0].born > SPLASH_LIFE) {
    splashes.shift();
  }

  // No HUD. The whole screen is sound-pressed letters.
}

function sim() {}
function leave() { splashes.length = 0; }

export { boot, paint, act, sim, leave };
