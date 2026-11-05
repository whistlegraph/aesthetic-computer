import { loadPatch } from "./gm.mjs";

// The instrument runs in a worker; samples belong to the browser AudioContext.
export function createGmBridge({ context, destination, status = () => {}, load = loadPatch }) {
  const voices = new Map();
  let selection = 0;
  function stop(id) {
    const entry = voices.get(id);
    if (!entry) return;
    voices.delete(id); entry.voice?.release();
  }
  return {
    async program(program) {
      const revision = ++selection;
      const ctx = context();
      if (!ctx) { status({ ready: false, error: "Tap a pad to enable sound" }); return; }
      status({ ready: false, loading: true });
      try {
        const patch = await load(program, ctx);
        if (!patch) throw new Error("missing patch");
        const samples = await Promise.all([48, 60, 72].map(note => patch.prefetch(note)));
        if (samples.some(sample => !sample)) throw new Error("missing sample");
        if (revision === selection) status({ ready: true, program });
      } catch { if (revision === selection) status({ ready: false, error: "Instrument could not load. Try again." }); }
    },
    async note({ id, program, note, velocity = 100, pan = 0, volume = 0.65 }) {
      if (!Number.isInteger(id) || !Number.isFinite(note) || note < 0 || note > 127 || !Number.isInteger(program) || program < 0 || program > 127) return;
      stop(id);
      const ctx = context();
      if (!ctx) return;
      const entry = { voice: null, pitch: 0 };
      voices.set(id, entry);
      try {
        const patch = await load(program, ctx);
        if (voices.get(id) !== entry) return;
        if (!patch) { voices.delete(id); return; }
        entry.voice = patch.play(note, { velocity, pan, volume, destination: destination(ctx) });
        entry.voice.update?.({ semitones: entry.pitch });
      } catch { if (voices.get(id) === entry) voices.delete(id); }
    },
    update({ id, semitones = 0 }) {
      const entry = voices.get(id);
      if (!entry || !Number.isFinite(semitones)) return;
      entry.pitch = Math.max(-24, Math.min(24, semitones));
      entry.voice?.update?.({ semitones: entry.pitch });
    },
    stop,
    panic() { selection++; for (const id of [...voices.keys()]) stop(id); },
  };
}
