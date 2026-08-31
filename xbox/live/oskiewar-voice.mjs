// oskiewar fighter voices — a procedural vocal tract, plus a word-sized
// announcer.
//
// Two layers, because the two jobs have opposite constraints. Combat
// reactions have to land on the frame that caused them and can fire a dozen
// times a second, so they are synthesized: a glottal sawtooth plus breath
// noise through three bandpass formants — the source/filter split every
// vocoder is built on. Centre frequencies are Peterson & Barney's mean male
// vowels ("Control Methods Used in a Study of the Vowels", JASA 24(2), 1952,
// Table II), scaled per fighter by a vocal-tract length factor, so @SAT and
// @JEFFREY say the same "owch" through different mouths.
//
// Words are the other job — rare, structural, and worth real diction — so
// "FIGHT", "K.O." and the fighter names go to the platform's speechSynthesis.
// That queue is slow and unschedulable, which is exactly why it never touches
// the 60fps layer: a spoken "owch" arriving two seconds after the hit is
// worse than no voice at all, so spoken onomatopoeia is rate-limited and
// dropped rather than queued. The synthesized cry always fires; the word is
// the garnish that fits.
//
// Nothing here is sampled. Signals arrive on the same `oskiewar:signal`
// CustomEvent the procedural drum bank listens to (`oskiewar-sfx.mjs`), and
// the same lazy-unlock contract applies: no AudioContext exists, and voice()
// returns false, until a user gesture calls unlock().

const clamp = (value, low, high) => Math.max(low, Math.min(high, value));

// Peterson & Barney mean male formants, Hz. F3 is what stops the result
// sounding like a filter sweep and starts it sounding like a head.
export const OSKIEWAR_VOWELS = Object.freeze({
  aa: [730, 1090, 2440], // father — the open cry
  ae: [660, 1720, 2410], // cat
  uh: [640, 1190, 2390], // cut — the default effort grunt
  oh: [570, 840, 2410],  // caught
  eh: [530, 1840, 2480], // bet
  ih: [390, 1990, 2550], // bit
  oo: [300, 870, 2240],  // boot
  ee: [270, 2290, 3010], // beet
});

// A fighter is a base pitch, a tract length, and a temperament. The tract
// factor multiplies every formant: a shorter tract is a smaller mouth, which
// is what actually makes a voice read as young rather than merely high.
export const OSKIEWAR_FIGHTERS = Object.freeze([
  { handle: "JEFFREY", f0: 118, tract: 1, growl: .3, speech: [.72, 1.02] },
  { handle: "FIFI", f0: 205, tract: 1.16, growl: .1, speech: [1.5, 1.14] },
  { handle: "OSKIE", f0: 165, tract: 1.08, growl: .18, speech: [1.16, 1.08] },
  { handle: "SAT", f0: 250, tract: 1.24, growl: .06, speech: [1.8, 1.24] },
]);

// Everything off the roster — BOT, DUMMY, SPIDERDUMMY — shares one machine
// throat: low, long, and rough, so a training partner never sounds like a pal.
export const OSKIEWAR_MACHINE = Object.freeze({
  handle: "CHALLENGER", f0: 96, tract: .93, growl: .62, speech: [.5, .88],
});

// The onomatopoeia. `path` is a vowel walk (a diphthong is what makes "owch"
// read as a word rather than a tone), `f0` a pitch contour in multiples of the
// fighter's base, both sampled across the same normalized duration. `say` is
// what the speech layer speaks when it has the budget; null means the sound is
// not a word — nobody pronounces a gasp.
export const OSKIEWAR_UTTERANCES = Object.freeze({
  hup: { say: "hup", path: ["uh"], f0: [1.06, 1.34], duration: .12,
    onset: "h", gain: .7, tier: "effort" },
  hyah: { say: "hyah", path: ["ih", "aa"], f0: [1.3, .94], duration: .17,
    onset: "h", gain: .76, tier: "effort" },
  hut: { say: "hut", path: ["uh"], f0: [1.22, .9], duration: .12,
    coda: "t", gain: .62, tier: "effort" },
  hah: { say: "hah", path: ["aa"], f0: [1.18, .88], duration: .15,
    onset: "h", breath: .3, gain: .7, tier: "effort" },
  nnh: { say: null, path: ["uh"], f0: [.96, .86], duration: .17,
    mouth: .12, growl: .3, gain: .78, tier: "effort" },
  grr: { say: "grr", path: ["uh"], f0: [.78, .7], duration: .22,
    mouth: .2, growl: .85, gain: .82, tier: "effort" },
  ptooey: { say: "ptoo", path: ["oo"], f0: [1.1, .8], duration: .13,
    onset: "t", coda: "f", gain: .56, tier: "effort" },
  ooh: { say: "ooh", path: ["oo", "ee"], f0: [1, 1.42], duration: .2,
    gain: .56, tier: "idle" },
  whoa: { say: "whoa", path: ["oo", "aa"], f0: [.92, 1.5], duration: .3,
    gain: .42, tier: "effort" },
  yikes: { say: "whoa", path: ["aa", "oo"], f0: [1.45, .68], duration: .34,
    gain: .46, tier: "effort" },
  oof: { say: "oof", path: ["oo"], f0: [.94, .6], duration: .21,
    coda: "f", growl: .35, gain: .95, tier: "pain" },
  owch: { say: "owch", path: ["aa", "oh", "oo"], f0: [1.16, .72],
    duration: .34, coda: "ch", growl: .22, gain: 1.02, tier: "pain" },
  argh: { say: "argh", path: ["aa"], f0: [1.04, .66], duration: .48,
    coda: "h", growl: .72, gain: 1.05, tier: "pain" },
  // An inhale, so there is no glottal source at all — just breath pulled
  // through a mouth that is closing. The slow attack is the whole gesture.
  gasp: { say: null, path: ["aa", "ih"], f0: [1, 1], duration: .3,
    voiced: 0, breath: 1, attack: .09, gain: 1.1, tier: "pain" },
  aaah: { say: "aaah", path: ["aa"], f0: [1.22, 1.78], duration: .74,
    growl: .34, gain: 1.9, tier: "big" },
  wah: { say: "wah", path: ["oo", "aa"], f0: [1.1, .52], duration: .52,
    growl: .5, gain: 1.3, tier: "big" },
  heh: { say: "heh heh", path: ["eh"], f0: [1.14, .98], duration: .1,
    onset: "h", repeat: 3, gain: .56, tier: "idle" },
  yay: { say: "yes", path: ["eh", "ee"], f0: [1.1, 1.6], duration: .28,
    gain: .7, tier: "idle" },
  aww: { say: "aww", path: ["aa", "oh"], f0: [1, .6], duration: .38,
    gain: .7, tier: "idle" },
});

// Who owns the voice is not always who owns the signal. `ko`, `bodyhit` and
// `decapitate` are all reported from the attacker's side with the victim in
// `value` — and it is the victim who screams.
const ROUTES = Object.freeze({
  jump: ["hup", "self"],
  pogo: ["hup", "self"],
  ultrajump: ["whoa", "self"],
  boost: ["whoa", "self"],
  "skate-mount": ["whoa", "self"],
  fastdrop: ["yikes", "self"],
  dash: ["hah", "self"],
  punch: ["hyah", "self"],
  wack: ["hyah", "self"],
  "throw-player": ["hyah", "self"],
  kick: ["hut", "self"],
  boot: ["hut", "self"],
  "shield-bash": ["hut", "self"],
  spit: ["ptooey", "self"],
  grab: ["grr", "self"],
  "grab-player": ["grr", "self"],
  "grab-part": ["grr", "self"],
  steal: ["grr", "self"],
  reach: ["nnh", "self"],
  block: ["nnh", "self"],
  ballblock: ["nnh", "self"],
  pickup: ["ooh", "self"],
  powerup: ["ooh", "self"],
  shieldbreak: ["gasp", "self"],
  bodybounce: ["oof", "self"],
  bodyhit: ["oof", "target"],
  partdamage: ["owch", "self"],
  partremoved: ["argh", "self"],
  decapitate: ["argh", "target"],
  sink: ["wah", "self"],
  balled: ["wah", "target"],
  ko: ["aaah", "target"],
  laugh: ["heh", "self"],
  reaction: ["reaction", "self"],
});

// The announcer only speaks the shape of the match. Anything that can fire
// twice in a second belongs to the tract, not to the queue.
const COUNT_WORDS = ["go", "one", "two", "three", "four", "five",
  "six", "seven", "eight", "nine", "ten"];

export const OSKIEWAR_VOICE_EVENTS = Object.freeze([
  ...Object.keys(ROUTES),
  "fighters", "select", "countdown", "fighters-lock", "tie",
  "roundwin", "matchwin",
]);

// A cry can outlive its cause, but it must not stack. These are the shortest
// gaps that still read as one voice per fighter rather than a crowd.
const COOLDOWNS = Object.freeze({
  effort: .17, pain: .24, idle: .4, big: 0,
});

// How far an onset consonant runs ahead of the vowel it opens.
const ONSET_LEAD = .028;

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

function fighterFor(rosterIndex) {
  return OSKIEWAR_FIGHTERS[rosterIndex] || OSKIEWAR_MACHINE;
}

/** Pure routing, for telemetry, tests, and hosts with no Web Audio. */
export function describeOskiewarVoice(event, player = -1, value = 0,
    value2 = 0, options = {}) {
  const route = ROUTES[event];
  if (!route) return null;
  let [name, owner] = route;
  if (name === "reaction") name = Number(value) >= 0 ? "yay" : "aww";
  const pad = owner === "target" ? Math.round(Number(value)) : player;
  if (pad !== 0 && pad !== 1) return null;
  const utterance = OSKIEWAR_UTTERANCES[name];
  return Object.freeze({
    event, utterance: name, pad, tier: utterance.tier,
    say: utterance.say,
    pan: clamp(Number.isFinite(options.pan) ? options.pan
      : pad === 0 ? -.62 : .62, -1, 1),
    intensity: clamp(Number.isFinite(options.intensity)
      ? options.intensity : 1, 0, 2),
  });
}

/**
 * Build a lazy voice. Like the SFX bank, nothing — including the
 * AudioContext — exists until unlock() runs from a user gesture, so replays
 * and headless renders stay silent instead of throwing.
 */
export function createOskiewarVoice(options = {}) {
  const suppliedContext = options.context || null;
  const contextFactory = options.contextFactory || (() => {
    const Constructor = globalThis.AudioContext || globalThis.webkitAudioContext;
    return Constructor ? new Constructor({ latencyHint: "interactive" }) : null;
  });
  const speechApi = options.speech === false ? null
    : options.speech || globalThis.speechSynthesis || null;
  const Utterance = options.utterance ||
    globalThis.SpeechSynthesisUtterance || null;
  const speakFighters = options.speakFighters !== false;
  const speakAnnouncer = options.speakAnnouncer !== false;
  const maxVoices = Math.max(4, Number(options.maxVoices) || 12);

  const active = new Set();
  const rosters = new Map([[0, -1], [1, -1]]);
  const nextAt = new Map();
  let context = suppliedContext;
  let output = null;
  let breathBuffer = null;
  let unlocked = false;
  let muted = false;
  let counter = 0;
  let nextSpokenAt = 0;
  let announcerVoice = null;
  let spokenCount = 0;

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
    parameter(master.gain, clamp(options.volume ?? .8, 0, 1),
      context.currentTime);
    const destination = options.destination || context.destination;
    // The voice sits above a busy drum bank, so it gets its own limiter with a
    // slower release than the percussion's — a scream that ducks itself in the
    // middle stops being a scream.
    if (typeof context.createDynamicsCompressor === "function") {
      const limiter = context.createDynamicsCompressor();
      parameter(limiter.threshold, -10, context.currentTime);
      parameter(limiter.knee, 6, context.currentTime);
      parameter(limiter.ratio, 8, context.currentTime);
      parameter(limiter.attack, .004, context.currentTime);
      parameter(limiter.release, .18, context.currentTime);
      master.connect(limiter).connect(destination);
    } else master.connect(destination);
    output = master;
  }

  function buildBreathBuffer() {
    const length = Math.max(2048, Math.ceil(context.sampleRate));
    const buffer = context.createBuffer(1, length, context.sampleRate);
    const channel = buffer.getChannelData(0);
    const random = seeded(0x7a19c3d5);
    // Breath is closer to pink than white; a flat spectrum through a formant
    // bank hisses instead of sighing.
    let low = 0;
    for (let index = 0; index < length; index++) {
      const white = random() * 2 - 1;
      low = low * 0.82 + white * 0.18;
      channel[index] = white * .42 + low * 1.35;
    }
    breathBuffer = buffer;
  }

  function register(sources, nodes) {
    const record = { sources, nodes };
    active.add(record);
    while (active.size > maxVoices) {
      const oldest = active.values().next().value;
      for (const source of oldest.sources) {
        try { source.stop(context.currentTime); } catch {}
      }
      active.delete(oldest);
    }
    const cleanup = () => {
      active.delete(record);
      for (const node of nodes) {
        try { node.disconnect(); } catch {}
      }
    };
    const last = sources[sources.length - 1];
    if (typeof last?.addEventListener === "function")
      last.addEventListener("ended", cleanup, { once: true });
    else if (last) last.onended = cleanup;
  }

  function panNode(pan) {
    if (typeof context.createStereoPanner === "function") {
      const node = context.createStereoPanner();
      parameter(node.pan, clamp(pan, -1, 1), context.currentTime);
      return node;
    }
    return context.createGain();
  }

  // One formant: a bandpass whose Q is centre-over-bandwidth. 90 Hz is the
  // middle of the measured range, but a resonance that narrow only passes
  // whatever harmonics happen to fall inside it — so a high or rising voice
  // loses most of its level exactly when it is screaming. Real formant
  // synthesizers widen bandwidth with pitch for the same reason; keeping it
  // above the fundamental guarantees the passband always contains a harmonic.
  function formant(cue, index, level, at, end) {
    const bandwidth = Math.max(90, cue.f0 * 1.15);
    const filter = context.createBiquadFilter();
    const gain = context.createGain();
    filter.type = "bandpass";
    const track = cue.path.map((vowel) =>
      OSKIEWAR_VOWELS[vowel][index] * cue.tract);
    parameter(filter.frequency, track[0], at);
    parameter(filter.Q, Math.max(1.2, track[0] / bandwidth), at);
    for (let step = 1; step < track.length; step++)
      ramp(filter.frequency, track[step],
        at + (end - at) * (step / (track.length - 1)));
    parameter(gain.gain, level, at);
    filter.connect(gain).connect(cue.throat);
    return [filter, gain];
  }

  // A consonant is a shaped noise burst, which is all the ear needs to hear a
  // coda: "owch" without the /tʃ/ is just "ow".
  const CONSONANTS = Object.freeze({
    h: ["highpass", 900, .1, .3],
    t: ["highpass", 4200, .035, .5],
    k: ["highpass", 3200, .04, .55],
    ch: ["bandpass", 2500, .075, .55],
    f: ["highpass", 5200, .095, .4],
  });

  // `scale` is the utterance's own level: a consonant articulates the word it
  // belongs to, so it has to get quieter with it. Left on its own it becomes
  // the loudest thing in the cry, and "owch" arrives as a hiss with a vowel
  // somewhere behind it.
  function consonant(cue, key, at, scale) {
    const spec = CONSONANTS[key];
    if (!spec) return [];
    const [type, frequency, duration, level] = spec;
    const source = context.createBufferSource();
    const filter = context.createBiquadFilter();
    const envelope = context.createGain();
    source.buffer = breathBuffer;
    filter.type = type;
    parameter(filter.frequency, frequency, at);
    parameter(filter.Q, 1.1, at);
    parameter(envelope.gain, .0001, at);
    ramp(envelope.gain, level * scale * cue.gain, at + duration * .3);
    ramp(envelope.gain, .0001, at + duration, true);
    source.connect(filter).connect(envelope).connect(cue.panner);
    source.start(at, cue.random() * .5, duration);
    source.stop(at + duration + .008);
    return [source, [source, filter, envelope]];
  }

  function utter(cue, spec, at) {
    const duration = spec.duration;
    const end = at + duration;
    const sources = [];
    const nodes = [];
    const panner = panNode(cue.pan);
    const throat = context.createGain();
    const envelope = context.createGain();
    cue.panner = panner;
    cue.throat = throat;

    // Roughness is amplitude modulation below the pitch — the buzz of a voice
    // pushed past what it can hold. It rides the throat gain so it colours the
    // formants rather than the raw glottis.
    const growl = clamp((spec.growl ?? 0) * (1 + cue.growlBias), 0, .95);
    if (growl > .01) {
      const lfo = context.createOscillator();
      const depth = context.createGain();
      lfo.type = "sine";
      parameter(lfo.frequency, 34 + cue.random() * 22, at);
      parameter(depth.gain, growl * .5, at);
      parameter(throat.gain, 1 - growl * .5, at);
      lfo.connect(depth).connect(throat.gain);
      lfo.start(at);
      lfo.stop(end + .01);
      sources.push(lfo);
      nodes.push(lfo, depth);
    } else parameter(throat.gain, 1, at);

    const voiced = spec.voiced ?? 1;
    if (voiced > 0) {
      const glottis = context.createOscillator();
      const tilt = context.createBiquadFilter();
      const level = context.createGain();
      glottis.type = "sawtooth";
      const [low, high] = spec.f0;
      parameter(glottis.frequency, cue.f0 * low, at);
      ramp(glottis.frequency, Math.max(30, cue.f0 * high), end);
      // Without the tilt the sawtooth's top octaves fizz straight through the
      // formants and the result reads as a buzzer, not a throat.
      tilt.type = "lowpass";
      parameter(tilt.frequency, 3800, at);
      parameter(tilt.Q, .7, at);
      parameter(level.gain, voiced * .5, at);
      glottis.connect(tilt).connect(level);
      for (const index of [0, 1, 2]) {
        const [filter, gain] = formant(cue, index,
          [1, .52 * cue.mouth, .2 * cue.mouth][index], at, end);
        level.connect(filter);
        nodes.push(filter, gain);
      }
      glottis.start(at);
      glottis.stop(end + .01);
      sources.push(glottis);
      nodes.push(glottis, tilt, level);
    }

    const breath = spec.breath ?? .14;
    if (breath > 0) {
      const source = context.createBufferSource();
      const level = context.createGain();
      source.buffer = breathBuffer;
      parameter(level.gain, breath * .6, at);
      source.connect(level);
      for (const index of [0, 1, 2]) {
        const [filter, gain] = formant(cue, index,
          [1, .6 * cue.mouth, .3 * cue.mouth][index], at, end);
        level.connect(filter);
        nodes.push(filter, gain);
      }
      source.start(at, cue.random() * .4, duration);
      source.stop(end + .01);
      sources.push(source);
      nodes.push(source, level);
    }

    const attack = Math.min(duration * .5, spec.attack ?? .014);
    const peak = clamp((spec.gain ?? .6) * cue.gain, .0001, 2.2);
    parameter(envelope.gain, .0001, at);
    ramp(envelope.gain, peak, at + attack);
    // Holding the body of the vowel before the release is what separates a
    // shout from a click; a pure attack/decay pair reads as percussion.
    ramp(envelope.gain, peak * .78, at + duration * .72);
    ramp(envelope.gain, .0001, end, true);
    throat.connect(envelope).connect(panner).connect(output);
    nodes.push(throat, envelope, panner);

    if (spec.onset) {
      const [source, built] = consonant(cue, spec.onset, at - ONSET_LEAD, peak);
      if (source) { sources.push(source); nodes.push(...built); }
    }
    if (spec.coda) {
      const [source, built] = consonant(cue, spec.coda, end - .03, peak);
      if (source) { sources.push(source); nodes.push(...built); }
    }
    register(sources, nodes);
    return end;
  }

  function play(name, meta) {
    const spec = OSKIEWAR_UTTERANCES[name];
    if (!spec) return false;
    const profile = fighterFor(rosters.get(meta.pad) ?? -1);
    const random = seeded(hashText(`${name}:${counter++}:${meta.pad}`));
    const gain = clamp((meta.intensity ?? 1) * (muted ? 0 : 1), 0, 2);
    if (!gain) return true;
    const cue = {
      pan: clamp(meta.pan ?? 0, -1, 1),
      gain,
      // Nobody says the same word twice. A few percent of pitch and tract
      // wobble is the difference between a fighter and a soundboard.
      f0: profile.f0 * (.94 + random() * .12),
      tract: profile.tract * (.985 + random() * .03),
      mouth: spec.mouth ?? 1,
      growlBias: profile.growl * .35,
      path: spec.path,
      random,
    };
    // The onset consonant is scheduled ahead of the vowel, so the utterance
    // needs that much lead or the first cry after unlock asks for a negative
    // time and throws — the context clock starts at zero.
    let at = context.currentTime + .004 + (spec.onset ? ONSET_LEAD : 0);
    // A laugh is one syllable said unevenly three times; spacing it evenly is
    // what makes synthetic laughter sound like a machine gun.
    for (let take = 0; take < (spec.repeat || 1); take++) {
      const end = utter(cue, spec, at);
      at = end + .03 + random() * .05;
    }
    return true;
  }

  function pickAnnouncerVoice() {
    if (announcerVoice || !speechApi?.getVoices) return announcerVoice;
    const voices = speechApi.getVoices() || [];
    if (!voices.length) return null;
    const wanted = options.announcerVoice ||
      ["Daniel", "Google UK English Male", "Alex", "Fred", "Arthur"];
    announcerVoice = wanted.map((name) =>
      voices.find((voice) => voice.name === name)).find(Boolean) ||
      voices.find((voice) => /^en(-|$)/i.test(voice.lang || "")) || voices[0];
    return announcerVoice;
  }

  function speak(text, { pitch = 1, rate = 1, volume = 1, announcer = false }) {
    if (!speechApi || !Utterance || !text || muted) return false;
    const utterance = new Utterance(text);
    utterance.pitch = clamp(pitch, 0, 2);
    utterance.rate = clamp(rate, .1, 4);
    utterance.volume = clamp(volume, 0, 1);
    const voice = pickAnnouncerVoice();
    if (voice) utterance.voice = voice;
    // The announcer owns the queue: "K.O." must not wait behind a countdown
    // digit that is already stale. Fighters never cancel — they are dropped.
    if (announcer) speechApi.cancel?.();
    speechApi.speak(utterance);
    spokenCount += 1;
    return true;
  }

  function announce(text, opts = {}) {
    if (!speakAnnouncer) return false;
    return speak(text, { pitch: .82, rate: 1.06, ...opts, announcer: true });
  }

  function nameFor(pad) {
    return fighterFor(rosters.get(pad) ?? -1).handle;
  }

  // The winner's pad rides in `player` for roundwin/matchwin, and the victim's
  // in `value` for ko/balled — the announcer names whoever the event is about,
  // not whoever reported it.
  function announceSignal(event, player, value) {
    if (!speakAnnouncer) return false;
    switch (event) {
      case "fighters":
        return announce(`${nameFor(0)} versus ${nameFor(1)}`, { rate: 1 });
      case "countdown": {
        const second = Math.round(value);
        if (second < 1 || second > 3) return false;
        return announce(COUNT_WORDS[second], { rate: 1.15 });
      }
      case "fighters-lock": return announce("fight", { pitch: .7, rate: .95 });
      case "ko": return announce("K O", { pitch: .68, rate: .82 });
      case "balled": return announce("balled", { pitch: .7, rate: .9 });
      case "tie": return announce("draw", { rate: .95 });
      case "roundwin": return announce(`${nameFor(player)} wins the round`);
      case "matchwin":
        return announce(`${nameFor(player)} wins the match`, { rate: .95 });
      default: return false;
    }
  }

  function voice(event, player = -1, value = 0, value2 = 0, meta = {}) {
    if (!unlocked || !context || !output) return false;
    // Identity is learned before anything is voiced, or the first cry of the
    // match comes out of the wrong throat. `select` lands first and carries one
    // pad; `fighters` confirms both at the lock.
    if (event === "fighters") {
      rosters.set(0, Math.round(value));
      rosters.set(1, Math.round(value2));
    } else if (event === "select" && (player === 0 || player === 1)) {
      rosters.set(player, Math.round(value));
    }
    const spoke = announceSignal(event, player, value);
    const described = describeOskiewarVoice(event, player, value, value2, meta);
    if (!described) return spoke;

    const key = `${described.pad}:${described.tier}`;
    const now = context.currentTime;
    if (now < (nextAt.get(key) ?? 0)) return spoke;
    nextAt.set(key, now + COOLDOWNS[described.tier]);
    const played = play(described.utterance, described);

    // The word is opportunistic. It never queues, never interrupts, and only
    // rides the moments big enough to be worth a whole second of speech.
    if (speakFighters && described.say && described.tier !== "effort" &&
        now >= nextSpokenAt && !speechApi?.speaking) {
      const profile = fighterFor(rosters.get(described.pad) ?? -1);
      const [pitch, rate] = profile.speech;
      nextSpokenAt = now + (described.tier === "big" ? 1.1 : 1.6);
      speak(described.say, { pitch, rate, volume: .9 });
    }
    return played || spoke;
  }

  async function unlock() {
    if (!context) context = contextFactory();
    if (!context) return false;
    // A refused resume is not a failed unlock. Autoplay policy can reject the
    // first one and grant the next gesture, and an offline render context has
    // no resume at all — either way the graph is still worth building.
    if (typeof context.resume === "function" && context.state === "suspended")
      await context.resume().catch(() => {});
    if (!output) buildOutput();
    if (!breathBuffer) buildBreathBuffer();
    // iOS only lets speech start from inside a gesture, and only once. An
    // empty utterance here buys every later announcement the permission.
    if (speechApi && Utterance && !spokenCount) {
      try { speechApi.speak(new Utterance(" ")); spokenCount = 1; } catch {}
    }
    pickAnnouncerVoice();
    unlocked = context.state !== "suspended";
    return unlocked;
  }

  function connectSignals(target = globalThis) {
    if (!target?.addEventListener) return () => {};
    const listener = ({ detail = {} }) => voice(detail.event, detail.player,
      detail.value, detail.value2, detail);
    target.addEventListener("oskiewar:signal", listener);
    return () => target.removeEventListener?.("oskiewar:signal", listener);
  }

  function setFighters(padZero, padOne) {
    rosters.set(0, Math.round(Number(padZero)));
    rosters.set(1, Math.round(Number(padOne)));
  }

  function setVolume(value) {
    if (!output) return false;
    parameter(output.gain, clamp(Number(value) || 0, 0, 1), context.currentTime);
    return true;
  }

  function destroy() {
    for (const record of [...active]) {
      for (const source of record.sources) {
        try { source.stop(context?.currentTime || 0); } catch {}
      }
    }
    active.clear();
    speechApi?.cancel?.();
    try { output?.disconnect(); } catch {}
    output = null;
    breathBuffer = null;
    unlocked = false;
    if (!suppliedContext && options.closeContext && context?.close) context.close();
  }

  return Object.freeze({
    unlock, voice, announce, connectSignals, setFighters, setVolume, destroy,
    mute(value = true) { muted = Boolean(value); speechApi?.cancel?.(); },
    get context() { return context; },
    get unlocked() { return unlocked; },
    get activeVoices() { return active.size; },
    get spokenCount() { return spokenCount; },
    fighters: OSKIEWAR_FIGHTERS,
    events: OSKIEWAR_VOICE_EVENTS,
    utterances: Object.freeze(Object.keys(OSKIEWAR_UTTERANCES)),
  });
}

export default createOskiewarVoice;
