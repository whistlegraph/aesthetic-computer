// oskiewar procedural SFX mock bank.
//
// The short layered percussion voices follow Aesthetic Computer's shared
// notepat kit (`system/public/aesthetic.computer/lib/percussion.mjs`): tiny
// noise transients, pitched drum bodies, the 800/540/522.7/369.6 Hz
// inharmonic hat cluster, and small per-hit pitch variation. The rising
// droplets adapt the phase-rise character of AC's physical bubble model
// (`system/public/aesthetic.computer/lib/sound/bubble.mjs`). Everything is
// synthesized locally with Web Audio; there are no samples or dependencies.
// Tonal roots come from the released `/pop` manifests: marimbaba's F-major
// lullaby, helpabeach's A-minor pentatonic palette, and hellsine's F#-minor
// combat color (`system/public/aesthetic.computer/disks/pop/*.json`). Keeping
// these as tiny realtime voices avoids decoding full `/pop` tracks on every
// match while retaining their musical family.

const clamp = (value, low, high) => Math.max(low, Math.min(high, value));
const HAT_FREQS = [800, 540, 522.7, 369.6];
const POP_TONES = Object.freeze({
  marimbaba: [174.61, 220, 261.63],
  helpabeach: [220, 261.63, 293.66, 329.63, 392],
  hellsine: [185, 220, 277.18, 329.63],
});

// Complete literal/dynamic gameSignal vocabulary audited from oskiewar.js.
export const OSKIEWAR_SIGNAL_EVENTS = Object.freeze([
  "hello", "select", "fighters", "wind", "ballserve",
  "countdown", "tie", "roundwin", "matchwin",
  "move", "jump", "ultrajump", "fastdrop", "dash", "shield",
  "punch", "kick", "block", "reach", "grab", "release", "pogo",
  "bullet", "grenade", "blast", "cancel", "pickup", "powerup",
  "wack", "crosswack", "boot", "bodybounce", "ballblock",
  "bodyhit", "partdamage", "partremoved",
  "balled", "ko", "killcam",
]);

export const OSKIEWAR_DRUM_NAMES = Object.freeze([
  "kick", "snare", "clap", "hat", "bell", "block", "whoosh",
]);

const ROUTES = Object.freeze({
  hello: "title-bloom",
  select: "menu-select",
  fighters: "fighters-lock",
  wind: "wind-sign",
  ballserve: "ball-serve",
  countdown: "countdown-bell",
  tie: "tie",
  roundwin: "round-win",
  matchwin: "match-win",
  move: "step",
  jump: "jump",
  ultrajump: "ultra-jump",
  fastdrop: "fast-drop",
  dash: "dash",
  shield: "shield-on",
  punch: "punch",
  kick: "kick",
  block: "block",
  reach: "reach",
  grab: "grab",
  release: "release",
  pogo: "pogo",
  bullet: "bullet-hat",
  grenade: "grenade-kicks",
  blast: "blast-kicks",
  cancel: "cancel-hats",
  pickup: "pickup",
  powerup: "powerup",
  wack: "ball-wack",
  crosswack: "cross-wack",
  boot: "ball-boot",
  bodybounce: "body-bounce",
  ballblock: "ball-block",
  bodyhit: "body-hit",
  partdamage: "part-damage",
  partremoved: "part-removed",
  balled: "balled",
  ko: "ko",
  killcam: "killcam",
  laugh: "atonal-laugh",
});

const DRUM_ROUTES = Object.freeze({
  kick: "drum-kick",
  snare: "drum-snare",
  clap: "drum-clap",
  hat: "drum-hat",
  bell: "drum-bell",
  block: "drum-block",
  whoosh: "drum-whoosh",
});

function hashText(text) {
  let hash = 2166136261;
  for (let index = 0; index < text.length; index++) {
    hash ^= text.charCodeAt(index);
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0;
}

function seeded(seed) {
  let state = seed >>> 0 || 0x6d2b79f5;
  return () => {
    state ^= state << 13;
    state ^= state >>> 17;
    state ^= state << 5;
    return (state >>> 0) / 4294967296;
  };
}

function defaultPan(player) {
  return player === 0 ? -0.62 : player === 1 ? 0.62 : 0;
}

/** Pure routing helper useful to telemetry, tests, and non-WebAudio clients. */
export function describeOskiewarSignal(event, player = -1, value = 0,
    value2 = 0, options = {}) {
  const recipe = ROUTES[event];
  if (!recipe) return null;
  const pan = Number.isFinite(options.pan) ? options.pan : defaultPan(player);
  const intensity = Number.isFinite(options.intensity) ? options.intensity : 1;
  return Object.freeze({
    event, recipe, player,
    value: Number(value) || 0,
    value2: Number(value2) || 0,
    pan: clamp(pan, -1, 1),
    intensity: clamp(intensity, 0, 2),
  });
}

/**
 * Build a lazy WebAudio bank. Nothing, including AudioContext construction,
 * happens until unlock() is called from a user gesture. signal()/drum() return
 * false before that point, which makes replay and headless use safe.
 */
export function createOskiewarSfx(options = {}) {
  const suppliedContext = options.context || null;
  const contextFactory = options.contextFactory || (() => {
    const Constructor = globalThis.AudioContext || globalThis.webkitAudioContext;
    return Constructor ? new Constructor({ latencyHint: "interactive" }) : null;
  });
  const ownsContext = !suppliedContext;
  const maxVoices = Math.max(8, Number(options.maxVoices) || 72);
  const playerPans = new Map([[0, -0.62], [1, 0.62]]);
  const active = new Set();
  let context = suppliedContext;
  let output = null;
  let noiseBuffer = null;
  let unlocked = false;
  let muted = false;
  let eventCounter = 0;

  function parameter(param, value, at) {
    if (!param) return;
    if (typeof param.setValueAtTime === "function") param.setValueAtTime(value, at);
    else param.value = value;
  }

  function ramp(param, value, at, exponential = false) {
    if (!param) return;
    const method = exponential ? "exponentialRampToValueAtTime"
      : "linearRampToValueAtTime";
    if (typeof param[method] === "function") param[method](value, at);
    else param.value = value;
  }

  function buildOutput() {
    const master = context.createGain();
    parameter(master.gain, clamp(options.volume ?? 0.72, 0, 1), context.currentTime);
    const destination = options.destination || context.destination;
    if (typeof context.createDynamicsCompressor === "function") {
      const limiter = context.createDynamicsCompressor();
      parameter(limiter.threshold, -8, context.currentTime);
      parameter(limiter.knee, 5, context.currentTime);
      parameter(limiter.ratio, 10, context.currentTime);
      parameter(limiter.attack, 0.002, context.currentTime);
      parameter(limiter.release, 0.08, context.currentTime);
      master.connect(limiter).connect(destination);
    } else master.connect(destination);
    output = master;
  }

  // One seeded buffer is shared by every transient. BufferSourceNodes are
  // cheap and one-shot; allocating a fresh Float32Array per hit is not.
  function buildNoiseBuffer() {
    const length = Math.max(2048, Math.ceil(context.sampleRate * 1.25));
    const buffer = context.createBuffer(1, length, context.sampleRate);
    const channel = buffer.getChannelData(0);
    const random = seeded(0x05c1e5f0);
    let pinkish = 0;
    for (let index = 0; index < length; index++) {
      const white = random() * 2 - 1;
      pinkish = pinkish * 0.64 + white * 0.36;
      channel[index] = white * 0.76 + pinkish * 0.24;
    }
    noiseBuffer = buffer;
  }

  function register(source, nodes) {
    const record = { source, nodes };
    active.add(record);
    while (active.size > maxVoices) {
      const oldest = active.values().next().value;
      try { oldest.source.stop(context.currentTime); } catch {}
      active.delete(oldest);
    }
    const cleanup = () => {
      active.delete(record);
      for (const node of nodes) {
        try { node.disconnect(); } catch {}
      }
    };
    if (typeof source.addEventListener === "function")
      source.addEventListener("ended", cleanup, { once: true });
    else source.onended = cleanup;
  }

  function panNode(pan) {
    if (typeof context.createStereoPanner === "function") {
      const node = context.createStereoPanner();
      parameter(node.pan, clamp(pan, -1, 1), context.currentTime);
      return node;
    }
    return context.createGain();
  }

  function tone(cue, spec) {
    const at = cue.at + (spec.delay || 0);
    const duration = Math.max(0.004, spec.duration || 0.08);
    const source = context.createOscillator();
    const envelope = context.createGain();
    const panner = panNode(cue.pan + (spec.pan || 0));
    source.type = spec.type || "sine";
    const frequency = Math.max(20, (spec.frequency || 220) * cue.pitch);
    parameter(source.frequency, frequency, at);
    if (spec.endFrequency)
      ramp(source.frequency, Math.max(20, spec.endFrequency * cue.pitch),
        at + duration, spec.exponential !== false);
    const peak = clamp((spec.gain || 0.2) * cue.gain, 0.0001, 1.5);
    const attack = Math.min(duration * 0.45, spec.attack ?? 0.001);
    parameter(envelope.gain, 0.0001, at);
    ramp(envelope.gain, peak, at + attack);
    ramp(envelope.gain, 0.0001, at + duration, true);
    source.connect(envelope).connect(panner).connect(output);
    register(source, [source, envelope, panner]);
    source.start(at);
    source.stop(at + duration + 0.008);
  }

  function noise(cue, spec) {
    const at = cue.at + (spec.delay || 0);
    const duration = Math.max(0.004, Math.min(1.2, spec.duration || 0.08));
    const source = context.createBufferSource();
    source.buffer = noiseBuffer;
    const filter = context.createBiquadFilter();
    const envelope = context.createGain();
    const panner = panNode(cue.pan + (spec.pan || 0));
    filter.type = spec.filter || "bandpass";
    parameter(filter.frequency, spec.frequency || 1800, at);
    parameter(filter.Q, spec.q ?? 0.8, at);
    if (spec.endFrequency)
      ramp(filter.frequency, Math.max(20, spec.endFrequency), at + duration,
        spec.exponential !== false);
    const peak = clamp((spec.gain || 0.2) * cue.gain, 0.0001, 1.5);
    const attack = Math.min(duration * 0.45, spec.attack ?? 0.001);
    parameter(envelope.gain, 0.0001, at);
    ramp(envelope.gain, peak, at + attack);
    ramp(envelope.gain, 0.0001, at + duration, true);
    source.connect(filter).connect(envelope).connect(panner).connect(output);
    register(source, [source, filter, envelope, panner]);
    // A deterministic offset prevents identical transients from phase-locking.
    source.start(at, cue.random() * Math.max(0.01, 1.25 - duration), duration);
    source.stop(at + duration + 0.008);
  }

  const kick = (cue, gain = 1, delay = 0) => {
    noise(cue, { duration: 0.005, frequency: 3000, q: 0.6,
      gain: 0.18 * gain, delay });
    tone(cue, { frequency: 180, endFrequency: 45, duration: 0.25,
      gain: 0.72 * gain, delay });
    tone(cue, { frequency: 58, endFrequency: 42, duration: 0.34,
      gain: 0.38 * gain, delay: delay + 0.004 });
  };

  const snare = (cue, gain = 1, delay = 0) => {
    noise(cue, { duration: 0.105, frequency: 2700, q: 0.72,
      gain: 0.62 * gain, delay });
    tone(cue, { type: "triangle", frequency: 238, endFrequency: 165,
      duration: 0.07, gain: 0.3 * gain, delay });
  };

  const hat = (cue, gain = 1, delay = 0) => {
    for (const frequency of HAT_FREQS)
      tone(cue, { type: "square", frequency, duration: 0.011,
        gain: 0.055 * gain, delay });
    noise(cue, { duration: 0.045, frequency: 7600, filter: "highpass",
      q: 0.6, gain: 0.28 * gain, delay });
  };

  const clap = (cue, gain = 1, delay = 0) => {
    for (const offset of [0, 0.012, 0.024])
      noise(cue, { duration: 0.025, frequency: 1250 + offset * 11000,
        q: 0.9, gain: 0.27 * gain, delay: delay + offset });
    noise(cue, { duration: 0.13, frequency: 2100, q: 0.75,
      gain: 0.32 * gain, attack: 0.025, delay });
  };

  const block = (cue, gain = 1, delay = 0) => {
    tone(cue, { type: "triangle", frequency: 940, endFrequency: 610,
      duration: 0.075, gain: 0.44 * gain, delay });
    tone(cue, { type: "square", frequency: 1880, duration: 0.025,
      gain: 0.11 * gain, delay });
  };

  const bell = (cue, gain = 1, delay = 0, root = 880) => {
    for (const [multiple, level, duration] of [[1, .45, .34], [1.5, .25, .25], [2, .12, .17]])
      tone(cue, { type: multiple === 2 ? "triangle" : "sine",
        frequency: root * multiple, duration, gain: level * gain, delay });
  };

  const whoosh = (cue, gain = 1, delay = 0, down = false) =>
    noise(cue, { duration: 0.25,
      frequency: down ? 4400 : 480, endFrequency: down ? 420 : 5200,
      q: 0.65, gain: 0.45 * gain, attack: 0.035, delay });

  // AC bubble-inspired chirp: a small sine body whose frequency rises as its
  // amplitude decays. Layered radii become a playful, liquid UI vocabulary.
  const bubble = (cue, gain = 1, delay = 0, root = 310, rise = 2.6) => {
    tone(cue, { frequency: root, endFrequency: root * rise,
      duration: 0.12, gain: 0.35 * gain, attack: 0.002, delay });
    tone(cue, { type: "triangle", frequency: root * 2.01,
      endFrequency: root * rise * 1.45, duration: 0.07,
      gain: 0.09 * gain, delay: delay + 0.004 });
  };

  function playRecipe(recipe, meta = {}) {
    const random = seeded(hashText(`${recipe}:${eventCounter++}:${meta.player ?? -1}`));
    const variation = 0.97 + random() * 0.06;
    const cue = {
      at: context.currentTime + 0.002,
      pan: clamp(meta.pan ?? 0, -1, 1),
      gain: clamp((meta.intensity ?? 1) * (muted ? 0 : 1), 0, 2),
      pitch: variation,
      random,
    };
    if (!cue.gain) return true;

    switch (recipe) {
      case "title-bloom":
        bubble(cue, .82, 0, POP_TONES.marimbaba[0], 2.25);
        bubble(cue, .7, .075, POP_TONES.marimbaba[1], 2.1);
        bell(cue, .42, .14, POP_TONES.marimbaba[2]);
        break;
      case "menu-select": hat(cue, .62); bubble(cue, .46, .018, 520, 1.7); break;
      case "fighters-lock": block(cue, .72); bell(cue, .46, .04, 330); break;
      case "wind-sign": whoosh(cue, .32, 0, (meta.value || 0) < 0); break;
      case "ball-serve": bubble(cue, .85, 0, 180, 3.1); block(cue, .32, .02); break;
      case "countdown-bell": bell(cue, .82, 0, 880 + clamp(meta.value, 0, 10) * 8); break;
      case "tie":
        bell(cue, .52, 0, POP_TONES.hellsine[1] * 2);
        bell(cue, .44, .17, POP_TONES.hellsine[0] * 2.24);
        break;
      case "round-win":
        bell(cue, .65, 0, POP_TONES.helpabeach[1] * 2);
        bell(cue, .7, .11, POP_TONES.helpabeach[3] * 2);
        break;
      case "match-win":
        bell(cue, .78, 0, POP_TONES.marimbaba[1] * 2);
        bell(cue, .72, .11, POP_TONES.marimbaba[2] * 2);
        bell(cue, .8, .22, POP_TONES.helpabeach[3] * 2);
        break;
      case "atonal-laugh":
        // Three uneven, detuned syllables: recognizably a chuckle but never a
        // sampled human voice or a tonal little melody.
        bubble(cue, .58, 0, 173, 1.42);
        bubble(cue, .68, .085, 241, .71);
        bubble(cue, .62, .19, 137, 1.83);
        hat(cue, .18, .11);
        break;
      case "step": block(cue, .15, 0); break;
      case "jump": bubble(cue, .68, 0, 220, 3.2); break;
      case "ultra-jump":
        bubble(cue, .8, 0, 150, 3.5); bubble(cue, .65, .045, 230, 3.6); whoosh(cue, .4); break;
      case "fast-drop": whoosh(cue, .65, 0, true); kick(cue, .24, .12); break;
      case "dash": whoosh(cue, .72); hat(cue, .38); break;
      case "shield-on": block(cue, .62); bubble(cue, .28, .02, 240, 1.8); break;
      case "punch": snare(cue, .85); break;
      case "kick": kick(cue, .9); break;
      case "block": block(cue, .95); kick(cue, .25, .012); break;
      case "reach": whoosh(cue, .24); break;
      case "grab": bubble(cue, .72, 0, 420, .58); break;
      case "release": bubble(cue, .45, 0, 260, 2.2); break;
      case "pogo": kick(cue, .78); bubble(cue, .4, .035, 140, 3.4); break;
      case "bullet-hat": hat(cue, 1); hat(cue, .35, .026); break;
      case "grenade-kicks": kick(cue, .48); kick(cue, .34, .055); break;
      case "blast-kicks":
        kick(cue, 1.15); kick(cue, .78, .045); kick(cue, .62, .095); break;
      case "cancel-hats": hat(cue, .78); hat(cue, .7, .022); break;
      case "pickup": bubble(cue, .7, 0, meta.value === 2 ? 290 : 430, 2.8); break;
      case "powerup": bell(cue, .5, 0, 660); bubble(cue, .7, .045, 330, 3.2); break;
      case "ball-wack": clap(cue, .82); bubble(cue, .5, .015, 180, 3); break;
      case "cross-wack": clap(cue, 1); kick(cue, .62); bell(cue, .3, .03, 760); break;
      case "ball-boot": kick(cue, .75); block(cue, .25, .015); break;
      case "body-bounce": bubble(cue, .6, 0, 150, 2.4); block(cue, .3); break;
      case "ball-block": block(cue, .9); bubble(cue, .78, .025, 170, 4); break;
      case "body-hit": kick(cue, .55); snare(cue, .42); break;
      case "part-damage": snare(cue, .55); block(cue, .48, .012); break;
      case "part-removed":
        noise(cue, { duration: .36, frequency: 3900, endFrequency: 420,
          q: .55, gain: .6 });
        block(cue, .42, .04);
        break;
      case "balled": bubble(cue, 1, 0, 115, 5.2); kick(cue, .8, .04); break;
      case "ko": kick(cue, .95); snare(cue, .7, .04); break;
      case "killcam": whoosh(cue, .85); bell(cue, .38, .17, 220); break;
      case "drum-kick": kick(cue); break;
      case "drum-snare": snare(cue); break;
      case "drum-clap": clap(cue); break;
      case "drum-hat": hat(cue); break;
      case "drum-bell": bell(cue); break;
      case "drum-block": block(cue); break;
      case "drum-whoosh": whoosh(cue); break;
      default: return false;
    }
    return true;
  }

  async function unlock() {
    if (!context) context = contextFactory();
    if (!context) return false;
    if (typeof context.resume === "function" && context.state === "suspended")
      await context.resume();
    if (!output) buildOutput();
    if (!noiseBuffer) buildNoiseBuffer();
    unlocked = context.state !== "suspended";
    return unlocked;
  }

  function signal(event, player = -1, value = 0, value2 = 0, signalOptions = {}) {
    if (!unlocked || !context || !output) return false;
    const described = describeOskiewarSignal(event, player, value, value2, {
      ...signalOptions,
      pan: Number.isFinite(signalOptions.pan) ? signalOptions.pan
        : playerPans.get(player) ?? 0,
    });
    if (!described) return false;
    return playRecipe(described.recipe, described);
  }

  function drum(name, amount = 1, pan = 0) {
    if (!unlocked || !context || !output || !DRUM_ROUTES[name]) return false;
    return playRecipe(DRUM_ROUTES[name], {
      player: -1, pan: clamp(Number(pan) || 0, -1, 1),
      intensity: clamp(Number(amount) || 0, 0, 2), value: 0, value2: 0,
    });
  }

  function connectSignals(target = globalThis) {
    if (!target?.addEventListener) return () => {};
    const listener = ({ detail = {} }) => signal(detail.event, detail.player,
      detail.value, detail.value2, detail);
    target.addEventListener("oskiewar:signal", listener);
    return () => target.removeEventListener?.("oskiewar:signal", listener);
  }

  function setPlayerPan(player, pan) {
    playerPans.set(player, clamp(Number(pan) || 0, -1, 1));
  }

  function setVolume(value) {
    if (!output) return false;
    parameter(output.gain, clamp(Number(value) || 0, 0, 1), context.currentTime);
    return true;
  }

  function destroy() {
    for (const record of [...active]) {
      try { record.source.stop(context?.currentTime || 0); } catch {}
    }
    active.clear();
    try { output?.disconnect(); } catch {}
    output = null;
    noiseBuffer = null;
    unlocked = false;
    if (ownsContext && options.closeContext && context?.close) context.close();
  }

  return Object.freeze({
    unlock, signal, drum, connectSignals, setPlayerPan, setVolume, destroy,
    mute(value = true) { muted = Boolean(value); },
    get context() { return context; },
    get unlocked() { return unlocked; },
    get activeVoices() { return active.size; },
    get playedEvents() { return eventCounter; },
    events: OSKIEWAR_SIGNAL_EVENTS,
    drums: OSKIEWAR_DRUM_NAMES,
  });
}

export default createOskiewarSfx;
