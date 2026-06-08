// Virtual Synth Controller — a reusable instrument any AC disk piece can
// instantiate to play pitched notes, chords, sustained voices, and the shared
// notepat percussion kit, without re-deriving note frequencies or drum recipes.
//
// Usage inside a piece (sim/act both receive `sound`):
//
//   import { Synth } from "../lib/synth.mjs";
//   let synth;
//   function sim({ sound }) { synth ??= Synth(sound); }
//   function act({ event: e, sound }) {
//     synth ??= Synth(sound);
//     if (e.is("keyboard:down:space")) synth.note("c4");        // pitched
//     if (e.is("keyboard:down:k"))     synth.kick();            // drum
//     if (e.is("keyboard:down:c"))     synth.chord(["c4","e4","g4"]);
//     const held = synth.hold("a3");   // sustained (duration "🔁")
//     // ...later: held.kill(0.1);
//   }
//
// It wraps the piece's own `sound` API (sound.synth / sound.freq) plus the
// shared 12-drum kit in ./percussion.mjs. No persistent global state — each
// Synth() is an independent controller you can keep on a module variable.

import { playPercussion, PERCUSSION_NAMES } from "./percussion.mjs";

// Drum display-name → note letter (reverse of PERCUSSION_NAMES), so you can
// say synth.perc("kick") instead of synth.perc("c").
const NAME_TO_LETTER = Object.fromEntries(
  Object.entries(PERCUSSION_NAMES).map(([letter, name]) => [name, letter]),
);

// Friendly aliases for the convenience drum methods (hat → closed hat, etc.).
const DRUM_ALIASES = {
  kick: "c", bass: "c", snare: "d", clap: "e", snap: "f",
  hat: "g", hatClosed: "g", closedHat: "g", hatOpen: "a", openHat: "a",
  ride: "b", crash: "c#", splash: "d#", cowbell: "f#", block: "g#", tambo: "a#",
};

// Equal-temperament fallback (A4 = 440) used only if sound.freq is absent.
const SEMITONE = {
  c: 0, "c#": 1, db: 1, d: 2, "d#": 3, eb: 3, e: 4, f: 5,
  "f#": 6, gb: 6, g: 7, "g#": 8, ab: 8, a: 9, "a#": 10, bb: 10, b: 11,
};
function fallbackHz(str) {
  const m = String(str).toLowerCase().match(/^([a-g](?:#|b)?)(-?\d)?$/);
  if (!m) return parseFloat(str) || 440;
  const semi = SEMITONE[m[1]] ?? 0;
  const oct = m[2] != null ? parseInt(m[2], 10) : 4;
  return 440 * Math.pow(2, (semi + (oct - 4) * 12 - 9) / 12);
}

export function Synth(sound, opts = {}) {
  if (!sound || !sound.synth) {
    // Return an inert controller so callers never crash before audio is ready.
    const noop = () => null;
    return new Proxy({}, { get: () => noop });
  }

  const state = {
    wave: opts.wave || opts.voice || "sine",
    volume: opts.volume ?? 0.4,
    pan: opts.pan ?? 0,
  };

  // note name (or raw Hz number) → frequency in Hz.
  const hz = (n) =>
    typeof n === "number" ? n : sound.freq?.(n) ?? fallbackHz(n);

  // Play a single pitched note. Returns the synth handle (has .kill / .update).
  function note(n, o = {}) {
    const duration = o.duration ?? o.dur ?? 0.25;
    return sound.synth({
      type: o.wave || o.type || state.wave,
      tone: hz(n),
      attack: o.attack ?? 0.01,
      decay: o.decay ?? duration * 0.5,
      sustain: o.sustain ?? 0,
      release: o.release ?? 0.06,
      volume: o.volume ?? state.volume,
      pan: o.pan ?? state.pan,
      duration,
    });
  }

  // Start a sustained note (loops until killed). Returns the handle — call
  // handle.kill(fade) to release it.
  function hold(n, o = {}) {
    return sound.synth({
      type: o.wave || o.type || state.wave,
      tone: hz(n),
      attack: o.attack ?? 0.01,
      decay: o.decay ?? 0.9,
      sustain: o.sustain ?? 1,
      volume: o.volume ?? state.volume,
      pan: o.pan ?? state.pan,
      duration: "🔁",
    });
  }

  // Play several notes at once (root slightly louder). Returns an array.
  function chord(notes, o = {}) {
    const base = o.volume ?? state.volume;
    return (notes || []).map((n, i) =>
      note(n, { ...o, volume: base * (i === 0 ? 1 : 0.7) }),
    );
  }

  // Sustained chord — returns handles; kill them all with releaseAll(handles).
  function holdChord(notes, o = {}) {
    const base = o.volume ?? state.volume;
    return (notes || []).map((n, i) =>
      hold(n, { ...o, volume: base * (i === 0 ? 1 : 0.7) }),
    );
  }

  function releaseAll(handles, fade = 0.12) {
    (handles || []).forEach((h) => h?.kill?.(fade));
  }

  // Fire a drum from the shared kit. `name` accepts a display name ("kick",
  // "hat-c"), an alias ("openHat"), or a raw note letter ("c", "c#").
  function perc(name, o = {}) {
    const letter = NAME_TO_LETTER[name] || DRUM_ALIASES[name] || name;
    return playPercussion(sound, letter, o);
  }

  const api = {
    note,
    hold,
    chord,
    holdChord,
    releaseAll,
    perc,
    // Set the default voice/waveform for subsequent note()/chord() calls.
    voice: (w) => ((state.wave = w), w),
    // Tweak a default (volume / pan / wave).
    set: (k, v) => ((state[k] = v), v),
    hz,
    state,
    drumNames: PERCUSSION_NAMES,
  };

  // Convenience drum methods: synth.kick(), .snare(), .hat(), .openHat(), …
  for (const [alias, letter] of Object.entries(DRUM_ALIASES)) {
    api[alias] = (o) => perc(letter, o);
  }

  return api;
}

export default Synth;
