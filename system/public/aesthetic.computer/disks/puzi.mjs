// puzi, 26.09.11
// Notepat with only three keys. Tap the pads or press C, E, G.

import { getNoteColorWithOctave } from "../lib/note-colors.mjs";

const NOTES = ["c", "e", "g"]; // The whole instrument.
const WAVES = ["sine", "triangle", "square", "sawtooth"];
const KILL_FADE = 0.15;

let octave = 4;
let waveIndex = 0;
let wave = WAVES[waveIndex];

const buttons = {}; // note -> ui.Button
const sounds = {}; // note -> synth handle
const downs = {}; // hardware keys currently held
const glow = {}; // note -> 0..1 afterglow

let soundContext = null;

// The system paints its corner label at (6, 6) in a 6x10 font, so the pads
// start below it and the readout lives along the bottom.
const LABEL_BOTTOM = 20;
const READOUT_HEIGHT = 14;

function setup({ ui, geo, screen }) {
  const margin = 3;
  const top = LABEL_BOTTOM + margin;
  const w = (screen.width - margin * (NOTES.length + 1)) / NOTES.length;
  const h = screen.height - top - margin - READOUT_HEIGHT;

  NOTES.forEach((note, i) => {
    const geometry = [
      Math.round(margin + i * (w + margin)),
      top,
      Math.round(w),
      h,
    ];
    if (!buttons[note]) {
      buttons[note] = new ui.Button(...geometry);
      buttons[note].id = `pad-${note}`;
    } else {
      buttons[note].box = new geo.Box(...geometry);
    }
  });
}

function colorFor(note) {
  return getNoteColorWithOctave(note, octave);
}

function startNote(note) {
  const synth = soundContext?.synth;
  if (!synth) return;
  if (sounds[note]) sounds[note].kill(0.02); // Retrigger cleanly.
  sounds[note] = synth({
    type: wave,
    tone: `${octave}${note.toUpperCase()}`,
    duration: "🔁",
    attack: 0.005,
    volume: 0.8,
  });
  glow[note] = 1;
}

function stopNote(note) {
  sounds[note]?.kill(KILL_FADE);
  delete sounds[note];
}

function boot({ ui, geo, screen, sound }) {
  soundContext = sound;
  setup({ ui, geo, screen });
}

function sim({ sound }) {
  soundContext = sound;
  NOTES.forEach((note) => {
    if (sounds[note]) {
      glow[note] = 1;
    } else if (glow[note] > 0) {
      glow[note] = Math.max(0, glow[note] - 0.06);
    }
  });
}

function paint({ wipe, ink, screen, write }) {
  wipe(12, 10, 20);

  NOTES.forEach((note) => {
    const btn = buttons[note];
    if (!btn) return;
    const { x, y, w, h } = btn.box;
    const [r, g, b] = colorFor(note);
    const lit = glow[note] || 0;
    const held = Boolean(sounds[note]);

    // Dim body, brightening with the afterglow.
    const fade = 0.35 + 0.65 * lit;
    ink(r * fade, g * fade, b * fade).box(x, y, w, h);
    ink(held ? 255 : 90 + 120 * lit).box(x, y, w, h, "outline");

    ink(held ? [12, 10, 20] : [255, 255, 255]).write(
      note.toUpperCase() + octave,
      { center: "xy", x: x + w / 2, y: y + h / 2 },
    );
  });

  // Readout along the bottom, clear of the corner label.
  const readoutY = screen.height - READOUT_HEIGHT / 2;
  ink(200, 200, 255).write(wave, { center: "y", x: 4, y: readoutY });
  ink(160, 160, 200).write(`oct ${octave}`, {
    center: "y",
    x: screen.width - 4 - 7 * 6,
    y: readoutY,
  });
}

function act({ event: e, sound, pens, ui, geo, screen }) {
  soundContext = sound;

  if (e.is("reframed")) setup({ ui, geo, screen });

  NOTES.forEach((note) => {
    buttons[note]?.act(
      e,
      {
        down: () => startNote(note),
        over: (btn) => {
          if (btn.up) {
            btn.up = false;
            startNote(note);
          }
        },
        out: () => stopNote(note),
        push: () => stopNote(note),
        cancel: () => stopNote(note),
      },
      pens?.(),
    );
  });

  NOTES.forEach((note) => {
    if (e.is(`keyboard:down:${note}`) && !downs[note]) {
      downs[note] = true;
      if (buttons[note]) buttons[note].down = true;
      startNote(note);
    }
    if (e.is(`keyboard:up:${note}`)) {
      delete downs[note];
      if (buttons[note]) buttons[note].down = false;
      stopNote(note);
    }
  });

  // Octave and waveform.
  if (e.is("keyboard:down:arrowup")) octave = Math.min(8, octave + 1);
  if (e.is("keyboard:down:arrowdown")) octave = Math.max(1, octave - 1);
  if (e.is("keyboard:down:w")) {
    waveIndex = (waveIndex + 1) % WAVES.length;
    wave = WAVES[waveIndex];
  }
}

function leave() {
  NOTES.forEach(stopNote);
}

export { boot, sim, paint, act, leave };
