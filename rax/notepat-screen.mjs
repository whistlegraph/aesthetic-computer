// The panel displays the room's pixels. Play its timestamped voices from the
// position of the instrument, using this listener's own audio output.
import { createGmBridge } from './ac/lib/notepat-gm-bridge.mjs';
import { setGMAssetBase } from './ac/lib/gm.mjs';

export function createNotepatScreen({ at, sfx, audible, clock, send }) {
  let panel, pending, decoding = false, generation = 0, revision = 0, applied = 0;
  let desired = [], fades = {}, program, prepared, output, playing = false, dirty = false, latest;
  const live = new Map(), timers = new Set();
  setGMAssetBase('https://assets.aesthetic.computer/gm');
  const gm = createGmBridge({ context: () => sfx.audio, destination: () => output ??= sfx.sampleOutput(at) });

  // Each frame is complete. A slow decoder can skip straight to the newest.
  async function decode() {
    if (decoding || !panel) return;
    decoding = true;
    try {
      while (pending) {
        const data = pending, epoch = generation;
        pending = null;
        const zip = Uint8Array.from(atob(data), c => c.charCodeAt(0));
        const stream = new Blob([zip]).stream().pipeThrough(new DecompressionStream('deflate'));
        const pixels = new Uint8Array(await new Response(stream).arrayBuffer());
        if (epoch === generation && pixels.length === panel.pixels.length) {
          panel.pixels.set(pixels);
          panel.frame++;
        }
      }
    } catch (err) { console.warn('Rax Notepat frame:', err.message); }
    finally { decoding = false; }
  }
  function silence() {
    for (const v of live.values()) v.kill();
    live.clear();
    gm.panic();
  }
  function step() {
    const on = audible() && sfx.ready;
    if (!on) { if (playing) silence(); playing = false; return; }
    if (playing && !dirty) return;
    playing = true; dirty = false;
    if (program != null && prepared !== program) { prepared = program; void gm.program(program); }
    const ids = new Set(desired.map(v => v.id));
    for (const [id, v] of live) if (!ids.has(id)) { v.kill(fades[id]); live.delete(id); }
    for (const v of desired) {
      let voice = live.get(v.id);
      if (!voice) {
        if (v.note !== undefined) {
          void gm.note(v);
          voice = { kill: fade => gm.stop(v.id, fade), update: next => gm.update(next) };
        } else voice = sfx.hold(v.tone, { type: v.type, volume: 0.035 * (v.volume ?? 1), at });
        live.set(v.id, voice);
      }
      voice.update?.(v);
    }
  }
  return {
    mount(board) {
      panel = board;
      panel.step = step;
      panel.key = (way, e) => {
        if (!e.repeat) send({ type: 'notepat-key', way, code: e.code, key: e.key, shiftKey: e.shiftKey, altKey: e.altKey });
      };
      void decode();
    },
    receive(data) {
      if (data.type === 'notepat-frame') { pending = data.pixels; void decode(); return; }
      latest = data;
      const seq = ++revision;
      const apply = () => {
        if (seq < applied) return;
        applied = seq; desired = data.voices; fades = data.releases || {}; program = data.program; dirty = true; step();
      };
      const delay = Math.max(0, Math.min(120, data.at - clock()));
      if (!delay || !audible()) return apply();
      const timer = setTimeout(() => { timers.delete(timer); apply(); }, delay);
      timers.add(timer);
    },
    visibility() {
      if (!audible() && latest) {
        for (const timer of timers) clearTimeout(timer);
        timers.clear();
        applied = revision; desired = latest.voices; program = latest.program;
      }
      step();
    },
    leave() {
      generation++; pending = latest = null;
      for (const timer of timers) clearTimeout(timer);
      timers.clear(); desired = []; dirty = false; playing = false;
      silence();
    },
    get voices() { return live.size; },
  };
}
