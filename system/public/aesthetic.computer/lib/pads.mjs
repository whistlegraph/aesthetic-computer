// pads, 26.07.12
// Shared engine for self-running audiovisual "pad" instruments (prism, lull,
// voop, gloob, …). Each pad is a THIN WRAPPER: it calls `initPad({...})` with
// its identity + hooks, then re-exports boot/sim/paint/act from here. Patterned
// on lib/nom.mjs (a session-singleton engine shared by many wrapper disks).
//
// The engine owns the concerns every pad shares, so the wrappers only describe
// what makes them unique:
//
//  • UTC-CLOCK BEAT GRID — onBeat fires on net-synced beat crossings, so two
//    instances opened anywhere lock to the same grid (clock.time(), NaN-guarded).
//  • RATE OVERRIDE via params[0] — `lull 0.5` → 0.5 seconds per beat (default is
//    each pad's own 60/bpm). Same value on two instances → they still align.
//  • THE WHOLE PAD IS A BUTTON — tap/XY-drag PUMPS a decaying `pump` energy and
//    spawns a visual `burst`; the wrapper's onTap adds the matching sonic boost.
//  • AUDIO-REACTIVE POLLING — speaker.poll() + amplitudes/frequencies/beat, read
//    once and handed to every hook via ctx.
//  • RICH ZERO-LATENCY VOICES — see `voices` below (harp/flute/bubble physical
//    models + ADSR + layered detune), so pads sound full without sample latency.
//
// Wrapper skeleton (initPad MUST run in boot, not at import — this module is a
// session singleton shared by every pad, so each entry must re-assert its config
// before the engine's boot/sim/paint/act run):
//   import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";
//   const CONFIG = { bpm: 132, steps: 16, hooks: { onBoot, onBeat, onSim, onPaint, onTap } };
//   function boot(api) { initPad(CONFIG); padBoot(api); }
//   export { boot, sim, paint, act };

let cfg = { bpm: 120, steps: 16, hooks: {} };

// —— shared runtime state (reset every boot) ——
let beatSeconds = 0.5;
let lastBeat = -1;
let step = 0;
let beatProgress = 0;
let simMs = 0;
let pump = 0; // decaying tap energy, 0..3
let bursts = []; // visual tap bursts: { x, y, r, life, hue, grow, decay }
let bands = [];
let amp = 0;
let beatHit = 0;

export function initPad(config) {
  cfg = { bpm: 120, steps: 16, hooks: {}, ...config };
  cfg.hooks = config.hooks || {};
}

function reset() {
  lastBeat = -1;
  step = 0;
  beatProgress = 0;
  simMs = 0;
  pump = 0;
  bursts = [];
  bands = [];
  amp = 0;
  beatHit = 0;
}

const band = (name) =>
  bands?.find?.((b) => b.name === name)?.amplitude || 0;

// The context object every hook receives — live audio + rhythm + pump state.
function ctx(extra) {
  return {
    step,
    beatProgress,
    beatSeconds,
    pump,
    bursts,
    amp,
    beatHit,
    band, // band("subBass"|"lowMid"|"mid"|"air"|…)
    bands,
    simMs,
    ...extra,
  };
}

export function boot(api) {
  const { params, clock, sound, hud } = api;
  reset();
  const bpm = cfg.bpm || 120;
  // params[0] = seconds-per-beat override (e.g. `lull 0.5`). Default = 60/bpm.
  const p0 = parseFloat(params?.[0]);
  beatSeconds = Number.isFinite(p0) && p0 > 0 ? p0 : 60 / bpm;
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  sound?.bpm?.(60 / beatSeconds); // advisory; UTC scheduler below is authoritative
  hud?.label?.(""); // pads are full-bleed — hide the corner label
  cfg.hooks.onBoot?.({ ...api, ...ctx() });
}

export function sim(api) {
  const { sound, clock, num } = api;
  const speaker = sound?.speaker;
  const synth = sound?.synth;
  speaker?.poll(); // MANDATORY before reading audio

  bands = speaker?.frequencies?.left || [];
  amp = speaker?.amplitudes?.left || 0;
  beatHit = speaker?.beat?.detected ? 1 : 0;

  // UTC beat grid (rate-scaled) with the Invalid-Date NaN guard.
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now();
  simMs = ms;
  const beatMs = beatSeconds * 1000;
  const globalBeat = ms / beatMs;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx;
  const steps = cfg.steps || 16;
  step = ((idx % steps) + steps) % steps;

  if (idx !== lastBeat) {
    lastBeat = idx;
    cfg.hooks.onBeat?.({ ...api, ...ctx({ idx, synth }) });
  }

  // decay pump + advance/expire bursts
  pump *= 0.95;
  if (pump < 0.001) pump = 0;
  for (const b of bursts) {
    b.r += b.grow ?? 6;
    b.life -= b.decay ?? 0.03;
  }
  bursts = bursts.filter((b) => b.life > 0);

  cfg.hooks.onSim?.({ ...api, ...ctx({ num }) });
}

export function paint(api) {
  cfg.hooks.onPaint?.(api, ctx());
  // Shared tap-burst overlay: expanding hollow rings at each tap, unless the
  // wrapper opted to draw bursts itself (drawBursts:false).
  if (cfg.drawBursts !== false && api.ink && api.circle) {
    for (const b of bursts) {
      const a = Math.max(0, Math.min(255, Math.round(b.life * 200)));
      const [r, g, bl] = hueRGB(b.hue);
      api.ink(r, g, bl, a).circle(b.x, b.y, b.r, false);
    }
  }
}

export function act(api) {
  const { event: e, sound, screen } = api;
  const synth = sound?.synth;
  if (e.is("touch") || e.is("draw")) {
    const w = screen?.width || 1;
    const h = screen?.height || 1;
    const x = e.x / w;
    const y = e.y / h;
    pump = Math.min(3, pump + (e.is("draw") ? 0.12 : 0.9));
    const burst = {
      x: e.x,
      y: e.y,
      r: 0,
      life: 1,
      hue: x * 360,
      grow: 6,
      decay: 0.03,
    };
    bursts.push(burst);
    cfg.hooks.onTap?.({
      ...api,
      ...ctx({ x, y, ex: e.x, ey: e.y, isDraw: e.is("draw"), synth, burst }),
    });
  }
  cfg.hooks.onAct?.({ ...api, ...ctx() });
}

// —— helpers ————————————————————————————————————————————————————————————————
// Cheap HSL(hue 0-360, full sat/light)→RGB for burst coloring (no api needed).
function hueRGB(h) {
  h = ((h % 360) + 360) % 360;
  const c = 1,
    x = 1 - Math.abs(((h / 60) % 2) - 1);
  let r = 0,
    g = 0,
    b = 0;
  if (h < 60) [r, g, b] = [c, x, 0];
  else if (h < 120) [r, g, b] = [x, c, 0];
  else if (h < 180) [r, g, b] = [0, c, x];
  else if (h < 240) [r, g, b] = [0, x, c];
  else if (h < 300) [r, g, b] = [x, 0, c];
  else [r, g, b] = [c, 0, x];
  return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}

// —— rich, zero-latency reusable voices ——————————————————————————————————————
// All oscillator / physical-model based (no CDN samples), so they're instant and
// support live pitch-bend. Pads may also call raw `synth(...)` for bespoke tones.
export const voices = {
  // Karplus–Strong plucked string — bright, decaying, great for arps.
  pluck(synth, tone, o = {}) {
    return synth({
      tone,
      type: "harp",
      beats: o.beats ?? 0.6,
      attack: o.attack ?? 0.002,
      decay: o.decay ?? 0.7,
      volume: o.volume ?? 0.5,
      pan: o.pan ?? 0,
    });
  },
  // Layered sine+triangle bell — shimmering, long tail.
  bell(synth, tone, o = {}) {
    const v = o.volume ?? 0.4;
    synth({ tone, type: "sine", beats: o.beats ?? 1.2, attack: 0.004, decay: 0.9, volume: v, pan: o.pan ?? 0 });
    synth({ tone, type: "triangle", beats: (o.beats ?? 1.2) * 0.6, attack: 0.002, decay: 0.6, volume: v * 0.5, pan: o.pan ?? 0 });
  },
  // Warm sub with a touch of body — sine + faint saw an octave down feel.
  sub(synth, tone, o = {}) {
    const v = o.volume ?? 0.5;
    synth({ tone, type: "sine", beats: o.beats ?? 1.4, attack: o.attack ?? 0.02, decay: o.decay ?? 0.7, volume: v, pan: o.pan ?? 0 });
    synth({ tone, type: "sawtooth", beats: (o.beats ?? 1.4) * 0.7, attack: 0.02, decay: 0.5, volume: v * 0.18, pan: o.pan ?? 0 });
  },
  // Breathy waveguide flute/whistle — airy lead / pad top.
  flute(synth, tone, o = {}) {
    return synth({ tone, type: "flute", beats: o.beats ?? 1.0, attack: o.attack ?? 0.06, decay: o.decay ?? 0.5, volume: o.volume ?? 0.3, pan: o.pan ?? 0 });
  },
  // Held, detuned triad — sustained pad. Returns the voice handles (kill/update).
  padChord(synth, tones, o = {}) {
    const v = o.volume ?? 0.12;
    return tones.map((tone, i) =>
      synth({ tone, type: o.type ?? "sine", duration: "🔁", attack: o.attack ?? 0.6, decay: o.decay ?? 0.9, volume: v, pan: (i - (tones.length - 1) / 2) * (o.spread ?? 0.3) }),
    );
  },
  // Soft closed-hat tick from filtered noise.
  hat(synth, o = {}) {
    return synth({ type: "noise-white", tone: o.tone ?? 800, beats: o.beats ?? 0.1, attack: 0.001, decay: 0.18, volume: o.volume ?? 0.14, pan: o.pan ?? 0 });
  },
};
