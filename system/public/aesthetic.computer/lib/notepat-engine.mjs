// Standalone Notepat instrument API.
//
// This is deliberately independent of disk.mjs/bios.mjs: any ordinary web
// page can import it, while the note layout and GM sample player stay shared
// with Aesthetic Computer's Notepat piece.

import { loadPatch, setGMAssetBase } from "./gm.mjs";

export const NOTEPAT_KEY_TO_MIDI = Object.freeze({
  z: 49, x: 59,
  c: 60, v: 61, d: 62, s: 63, e: 64, f: 65, w: 66,
  g: 67, r: 68, a: 69, q: 70, b: 71,
  h: 72, t: 73, i: 74, y: 75, j: 76, k: 77, u: 78,
  l: 79, o: 80, m: 81, p: 82, n: 83,
  ";": 84, "'": 85, "]": 86,
});

export const NOTEPAT_WHITE_KEYS = Object.freeze([
  ["c", 60], ["d", 62], ["e", 64], ["f", 65], ["g", 67], ["a", 69], ["b", 71],
  ["h", 72], ["i", 74], ["j", 76], ["k", 77], ["l", 79], ["m", 81], ["n", 83],
]);

export function createNotepat({
  program = 0,
  volume = 0.72,
  audioContext,
  assetBase = "https://assets.aesthetic.computer/gm",
} = {}) {
  setGMAssetBase(assetBase);
  const AudioContextClass = globalThis.AudioContext || globalThis.webkitAudioContext;
  const context = audioContext || (AudioContextClass ? new AudioContextClass({ latencyHint: "interactive" }) : null);
  let currentProgram = Math.max(0, Math.min(127, program | 0));
  let octaveShift = 0;
  let programRevision = 0;
  const held = new Map();
  const pending = new Set();
  const listeners = new Set();
  let patchPromise = null;

  const emit = (detail) => listeners.forEach((listener) => listener(detail));
  const patch = () => patchPromise ??= loadPatch(currentProgram, context);

  async function ready() {
    if (!context) throw new Error("Web Audio is unavailable");
    if (context.state === "suspended") await context.resume();
    return patch();
  }

  async function pressMidi(midi, id = `midi:${midi}`) {
    if (held.has(id) || pending.has(id)) return held.get(id) || null;
    pending.add(id);
    const revision = programRevision;
    const instrument = await ready();
    if (!pending.delete(id)) return null;
    if (revision !== programRevision) return null;
    if (held.has(id)) return held.get(id);
    const soundingMidi = Math.max(0, Math.min(127, midi + octaveShift * 12));
    const voice = instrument.play(soundingMidi, {
      velocity: Math.round(Math.max(0, Math.min(1, volume)) * 127),
      attack: 0.006,
      release: 0.18,
    });
    held.set(id, voice);
    emit({ type: "press", id, midi: soundingMidi, displayMidi: midi });
    return voice;
  }

  function release(id) {
    pending.delete(id);
    const voice = held.get(id);
    if (!voice) return false;
    voice.release?.();
    held.delete(id);
    emit({ type: "release", id });
    return true;
  }

  function pressKey(key) {
    const normalized = String(key).toLowerCase();
    const midi = NOTEPAT_KEY_TO_MIDI[normalized];
    return midi == null ? null : pressMidi(midi, `key:${normalized}`);
  }

  function releaseKey(key) {
    return release(`key:${String(key).toLowerCase()}`);
  }

  function releaseAll() {
    pending.clear();
    [...held.keys()].forEach(release);
  }

  function setProgram(nextProgram) {
    const value = Math.max(0, Math.min(127, Number(nextProgram) | 0));
    if (value === currentProgram) return currentProgram;
    releaseAll();
    currentProgram = value;
    const revision = ++programRevision;
    patchPromise = null;
    emit({ type: "program", program: currentProgram, voice: currentProgram + 1 });
    patch().then((instrument) => {
      if (revision !== programRevision) return;
      emit({
        type: "ready",
        program: currentProgram,
        voice: currentProgram + 1,
        name: instrument?.name || `program-${currentProgram}`,
      });
    });
    return currentProgram;
  }

  function setOctave(nextOctave) {
    const value = Math.max(-4, Math.min(4, Number(nextOctave) | 0));
    if (value === octaveShift) return octaveShift;
    releaseAll();
    octaveShift = value;
    emit({ type: "octave", octave: octaveShift });
    return octaveShift;
  }

  return {
    context,
    ready,
    pressMidi,
    release,
    pressKey,
    releaseKey,
    releaseAll,
    setProgram,
    setOctave,
    stepOctave: (delta) => setOctave(octaveShift + Number(delta)),
    get program() { return currentProgram; },
    get voice() { return currentProgram + 1; },
    get octave() { return octaveShift; },
    midiForKey: (key) => NOTEPAT_KEY_TO_MIDI[String(key).toLowerCase()] ?? null,
    onActivity(listener) { listeners.add(listener); return () => listeners.delete(listener); },
  };
}
