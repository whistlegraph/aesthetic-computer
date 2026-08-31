import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";

import createOskiewarVoice, {
  OSKIEWAR_FIGHTERS,
  OSKIEWAR_UTTERANCES,
  OSKIEWAR_VOICE_EVENTS,
  OSKIEWAR_VOWELS,
  describeOskiewarVoice,
} from "../oskiewar-voice.mjs";

class FakeParam {
  value = 0;
  events = [];
  setValueAtTime(value, at) { this.value = value; this.events.push(["set", value, at]); }
  linearRampToValueAtTime(value, at) { this.value = value; this.events.push(["linear", value, at]); }
  exponentialRampToValueAtTime(value, at) { this.value = value; this.events.push(["exponential", value, at]); }
}

class FakeNode {
  connections = [];
  connect(node) { this.connections.push(node); return node; }
  disconnect() {}
}

class FakeSource extends FakeNode {
  frequency = new FakeParam();
  started = [];
  stopped = [];
  start(...args) { this.started.push(args); }
  stop(...args) { this.stopped.push(args); }
  addEventListener() {}
}

class FakeAudioContext {
  currentTime = 3;
  sampleRate = 8000;
  state = "suspended";
  destination = new FakeNode();
  oscillators = [];
  filters = [];
  buffers = [];
  bufferSources = [];
  panners = [];
  async resume() { this.state = "running"; }
  createGain() { const node = new FakeNode(); node.gain = new FakeParam(); return node; }
  createDynamicsCompressor() {
    const node = new FakeNode();
    for (const name of ["threshold", "knee", "ratio", "attack", "release"])
      node[name] = new FakeParam();
    return node;
  }
  createStereoPanner() {
    const node = new FakeNode(); node.pan = new FakeParam();
    this.panners.push(node); return node;
  }
  createOscillator() {
    const node = new FakeSource(); this.oscillators.push(node); return node;
  }
  createBiquadFilter() {
    const node = new FakeNode();
    node.frequency = new FakeParam(); node.Q = new FakeParam();
    this.filters.push(node); return node;
  }
  createBuffer(channels, length) {
    const data = Array.from({ length: channels }, () => new Float32Array(length));
    const buffer = { getChannelData: (channel) => data[channel] };
    this.buffers.push(buffer); return buffer;
  }
  createBufferSource() {
    const node = new FakeSource(); this.bufferSources.push(node); return node;
  }
}

class FakeUtterance {
  constructor(text) { this.text = text; }
}

function fakeSpeech() {
  return {
    spoken: [],
    cancelled: 0,
    speaking: false,
    getVoices: () => [{ name: "Daniel", lang: "en-GB" }, { name: "Fred", lang: "en-US" }],
    speak(utterance) { this.spoken.push(utterance); },
    cancel() { this.cancelled += 1; },
  };
}

function bank(overrides = {}) {
  const context = new FakeAudioContext();
  const speech = fakeSpeech();
  const voice = createOskiewarVoice({
    context, speech, utterance: FakeUtterance, ...overrides,
  });
  return { context, speech, voice };
}

test("every utterance names real vowels and a known tier", () => {
  const tiers = new Set(["effort", "pain", "idle", "big"]);
  for (const [name, spec] of Object.entries(OSKIEWAR_UTTERANCES)) {
    assert.ok(spec.path.length, `${name} has a vowel path`);
    for (const vowel of spec.path)
      assert.ok(OSKIEWAR_VOWELS[vowel], `${name} uses a measured vowel: ${vowel}`);
    assert.ok(tiers.has(spec.tier), `${name} has a rate-limit tier`);
    assert.ok(spec.duration > 0 && spec.duration < 1.2, `${name} is short`);
    assert.equal(spec.f0.length, 2, `${name} has a pitch contour`);
  }
});

test("routing gives the cry to whoever is hurt, not whoever reported it", () => {
  // ko/bodyhit/decapitate arrive from the attacker with the victim in `value`.
  assert.equal(describeOskiewarVoice("ko", 0, 1, 3).pad, 1);
  assert.equal(describeOskiewarVoice("bodyhit", 1, 0, 2).pad, 0);
  assert.equal(describeOskiewarVoice("decapitate", 0, 1, 1).pad, 1);
  assert.equal(describeOskiewarVoice("balled", 1, 0, 0).pad, 0);
  // partdamage/partremoved already report from the victim's side.
  assert.equal(describeOskiewarVoice("partdamage", 1, 2, .5).pad, 1);
  assert.equal(describeOskiewarVoice("partremoved", 0, 1, 1).pad, 0);
  // and an attack is the attacker's own effort.
  assert.equal(describeOskiewarVoice("punch", 1, -1, 0).pad, 1);
});

test("reaction picks its vowel from the sign of the result", () => {
  assert.equal(describeOskiewarVoice("reaction", 0, 1, 0).utterance, "yay");
  assert.equal(describeOskiewarVoice("reaction", 0, -1, 0).utterance, "aww");
});

test("padless and unknown events route nowhere", () => {
  assert.equal(describeOskiewarVoice("wind", -1, 0, 0), null);
  assert.equal(describeOskiewarVoice("punch", -1, 0, 0), null);
  assert.equal(describeOskiewarVoice("not-real", 0), null);
});

test("voice is inert before explicit audio unlock", async () => {
  let constructions = 0;
  const voice = createOskiewarVoice({
    speech: false,
    contextFactory: () => { constructions += 1; return new FakeAudioContext(); },
  });
  assert.equal(voice.voice("ko", 0, 1), false);
  assert.equal(constructions, 0);
  assert.equal(await voice.unlock(), true);
  assert.equal(constructions, 1);
});

test("every routed event schedules a bounded formant voice", async () => {
  const { context, voice } = bank({ maxVoices: 256, speech: false });
  await voice.unlock();
  voice.setFighters(0, 3);
  for (const event of OSKIEWAR_VOICE_EVENTS) {
    // Cooldowns are per pad and tier, so step the clock between utterances.
    context.currentTime += 1;
    voice.voice(event, 0, 1, 1);
  }
  assert.ok(context.oscillators.length > 10, "glottal sources were scheduled");
  // Three formants per source, and both the glottis and the breath get a set.
  assert.ok(context.filters.length > context.oscillators.length * 3);
  assert.equal(context.buffers.length, 1, "one breath buffer is allocated");
  assert.ok(context.panners.some((node) => node.pan.value < 0));
});

test("a fighter's tract sets the formants the same word comes out of", async () => {
  const heard = {};
  for (const pad of [0, 3]) {
    const { context, voice } = bank({ speech: false });
    await voice.unlock();
    voice.setFighters(pad, pad);
    voice.voice("partdamage", 0, 1, 1);
    // The first bandpass is F1 of the first vowel in "owch" — /a/, 730 Hz.
    // (The lowpass ahead of it is the glottal tilt, which no tract moves.)
    const [f1] = context.filters.filter((node) => node.type === "bandpass");
    heard[pad] = f1.frequency.events[0][1];
  }
  const [jeffrey, sat] = [heard[0], heard[3]];
  assert.ok(sat > jeffrey, "the shorter tract lands higher");
  const ratio = OSKIEWAR_FIGHTERS[3].tract / OSKIEWAR_FIGHTERS[0].tract;
  // Per-utterance wobble is a few percent; the tract difference is 24%.
  assert.ok(Math.abs(sat / jeffrey - ratio) < .06, `${sat / jeffrey} ~ ${ratio}`);
});

test("the announcer names the fighter each event is actually about", async () => {
  const { context, speech, voice } = bank();
  await voice.unlock();
  speech.spoken.length = 0;
  voice.voice("fighters", -1, 1, 3);
  assert.match(speech.spoken.at(-1).text, /FIFI versus SAT/);
  // roundwin/matchwin carry the winner in `player`.
  context.currentTime += 1;
  voice.voice("roundwin", 1, 1, 2);
  assert.match(speech.spoken.at(-1).text, /SAT wins the round/);
  context.currentTime += 1;
  voice.voice("matchwin", 0, 2, 3);
  assert.match(speech.spoken.at(-1).text, /FIFI wins the match/);
  context.currentTime += 1;
  voice.voice("fighters-lock", -1, 0, 0);
  assert.equal(speech.spoken.at(-1).text, "fight");
});

test("the announcer counts the intro in and ignores the round clock", async () => {
  const { context, speech, voice } = bank();
  await voice.unlock();
  speech.spoken.length = 0;
  for (const second of [3, 2, 1]) {
    context.currentTime += 1;
    voice.voice("countdown", -1, second, 1);
  }
  assert.deepEqual(speech.spoken.map((line) => line.text),
    ["three", "two", "one"]);
  context.currentTime += 1;
  voice.voice("countdown", -1, 9, 0);
  assert.equal(speech.spoken.length, 3, "nine seconds left is not a word");
});

test("off-roster pads get the machine throat and no handle of their own", async () => {
  const { speech, voice } = bank();
  await voice.unlock();
  speech.spoken.length = 0;
  voice.voice("fighters", -1, -1, 2);
  assert.match(speech.spoken.at(-1).text, /CHALLENGER versus OSKIE/);
});

test("spoken onomatopoeia is dropped, never queued, and never on efforts", async () => {
  const { context, speech, voice } = bank();
  await voice.unlock();
  voice.setFighters(0, 1);
  speech.spoken.length = 0;
  voice.voice("punch", 0, 1, 0);
  assert.equal(speech.spoken.length, 0, "an effort grunt is not a word");

  context.currentTime += 1;
  voice.voice("partdamage", 0, 1, 1);
  assert.equal(speech.spoken.at(-1).text, "owch");
  assert.equal(speech.spoken.at(-1).pitch, OSKIEWAR_FIGHTERS[0].speech[0]);

  // Inside the spoken cooldown the cry still fires, but the word is dropped.
  const spokenBefore = speech.spoken.length;
  const voicesBefore = context.oscillators.length;
  context.currentTime += .3;
  voice.voice("partdamage", 0, 1, 1);
  assert.equal(speech.spoken.length, spokenBefore, "no backlog of stale owches");
  assert.ok(context.oscillators.length > voicesBefore, "the cry still lands");

  // Nor does it queue behind speech already in flight.
  context.currentTime += 5;
  speech.speaking = true;
  voice.voice("partremoved", 0, 1, 1);
  assert.equal(speech.spoken.length, spokenBefore);
});

test("the announcer takes the queue from a fighter mid-word", async () => {
  const { context, speech, voice } = bank();
  await voice.unlock();
  const cancelled = speech.cancelled;
  context.currentTime += 1;
  voice.voice("partdamage", 0, 1, 1);
  assert.equal(speech.cancelled, cancelled, "a fighter waits its turn");
  context.currentTime += 1;
  voice.voice("ko", 0, 1, 1);
  assert.ok(speech.cancelled > cancelled, "K.O. interrupts whatever is talking");
});

test("cooldowns keep one throat per fighter without silencing the other", async () => {
  const { context, voice } = bank({ speech: false });
  await voice.unlock();
  const first = context.oscillators.length;
  voice.voice("punch", 0, 1, 0);
  const second = context.oscillators.length;
  assert.ok(second > first);
  voice.voice("punch", 0, 1, 0);
  assert.equal(context.oscillators.length, second, "pad 0 is still mid-grunt");
  voice.voice("punch", 1, 1, 0);
  assert.ok(context.oscillators.length > second, "pad 1 has its own throat");
  context.currentTime += .5;
  voice.voice("punch", 0, 1, 0);
  assert.ok(context.oscillators.length > second + 1, "the cooldown expires");
});

test("muting stops both layers and cancels anything in the queue", async () => {
  const { context, speech, voice } = bank();
  await voice.unlock();
  voice.mute(true);
  const oscillators = context.oscillators.length;
  const spoken = speech.spoken.length;
  context.currentTime += 1;
  voice.voice("ko", 0, 1, 1);
  assert.equal(context.oscillators.length, oscillators);
  assert.equal(speech.spoken.length, spoken);
});

test("the bank runs with no speech platform at all", async () => {
  const context = new FakeAudioContext();
  const voice = createOskiewarVoice({ context, speech: false });
  await voice.unlock();
  assert.equal(voice.voice("ko", 0, 1, 1), true);
  assert.equal(voice.spokenCount, 0);
});

test("signal bridge forwards details and detaches", async () => {
  const { context, voice } = bank({ speech: false });
  await voice.unlock();
  const listeners = new Map();
  const target = {
    addEventListener(name, listener) { listeners.set(name, listener); },
    removeEventListener(name, listener) {
      if (listeners.get(name) === listener) listeners.delete(name);
    },
  };
  const detach = voice.connectSignals(target);
  const before = context.oscillators.length;
  listeners.get("oskiewar:signal")({ detail: {
    event: "partremoved", player: 1, value: 2, value2: 1, pan: .5,
  } });
  assert.ok(context.oscillators.length > before);
  assert.ok(context.panners.some((node) => node.pan.value === .5));
  detach();
  assert.equal(listeners.has("oskiewar:signal"), false);
});

test("every routed event is one oskiewar.js actually emits", async () => {
  // The router invents nothing. A voice hung on an event the game stopped
  // sending is silence nobody notices until somebody plays the match.
  const source = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");
  // Read whole calls, not just their first argument — `ko`, `balled`, `wack`
  // and `ballblock` are all chosen by a ternary at the call site.
  const emitted = new Set(
    [...source.matchAll(/emitSignal\(([\s\S]*?)\);/g)]
      .flatMap((call) => [...call[1].matchAll(/"([a-z][a-z-]*)"/g)])
      .map((match) => match[1]));
  // `kind.toLowerCase()` at the melee call site sends these two dynamically.
  emitted.add("punch").add("kick");
  for (const event of OSKIEWAR_VOICE_EVENTS)
    assert.ok(emitted.has(event), `${event} is still emitted by the game`);
});
