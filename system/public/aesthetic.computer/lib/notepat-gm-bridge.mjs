import { loadPatch, preloadPatchFiles, playLoadingTone } from "./gm.mjs";

// The instrument runs in a worker; samples belong to the browser AudioContext.
export function createGmBridge({ context, destination, status = () => {}, load = loadPatch, preload = preloadPatchFiles, fallback = playLoadingTone }) {
  const voices = new Map();
  const patches = new WeakMap();
  function remember(ctx,program,patch) {
    if(!patches.has(ctx)) patches.set(ctx,new Map());
    patches.get(ctx).set(program,patch);
  }
  let selection = 0;
  function stop(id, fade) {
    const entry = voices.get(id);
    if (!entry) return;
    clearTimeout(entry.timer);
    entry.released=true;
    entry.voice?.release(fade);
    if(fade>1 && entry.voice) entry.timer=setTimeout(()=>{if(voices.get(id)===entry) voices.delete(id);},fade*1000+50);
    else voices.delete(id);
  }
  return {
    async program(program) {
      const revision = ++selection;
      const ctx = context();
      if (!ctx) {
        status({ready:false,loading:true});
        await preload(program);
        if(revision===selection) status({ready:false,loading:false});
        return;
      }
      status({ ready: false, loading: true });
      try {
        const patch = await load(program, ctx);
        if (!patch) throw new Error("missing patch");
        remember(ctx,program,patch);
        const samples = await patch.prefetch();
        if (Array.isArray(samples) && samples.some(sample => !sample)) throw new Error("missing sample");
        if (revision === selection) status({ ready: true, program });
      } catch { if (revision === selection) status({ ready: false, error: "Instrument could not load. Try again." }); }
    },
    async note({ id, program, note, velocity = 100, pan = 0, volume = 0.65 }) {
      if (!Number.isInteger(id) || !Number.isFinite(note) || note < 0 || note > 127 || !Number.isInteger(program) || program < 0 || program > 127) return;
      stop(id);
      const ctx = context();
      if (!ctx) return;
      const options={velocity,pan,volume,destination:destination(ctx),fallback:true};
      const cached=patches.get(ctx)?.get(program);
      const entry = { voice: cached ? cached.play(note,options) : fallback(ctx,note,options), pitch: 0 };
      voices.set(id, entry);
      if(cached) return;
      try {
        const patch = await load(program, ctx);
        if (voices.get(id) !== entry || entry.released) return;
        if (!patch) { if(!entry.voice) voices.delete(id); return; }
        remember(ctx,program,patch);
        if (entry.voice) { void patch.prefetch(note); return; }
        entry.voice = patch.play(note,options);
        entry.voice.update?.({ semitones: entry.pitch });
      } catch { if (voices.get(id) === entry && !entry.voice) voices.delete(id); }
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
