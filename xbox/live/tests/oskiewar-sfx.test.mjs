import test from "node:test";
import assert from "node:assert/strict";

import createOskiewarSfx, {
  OSKIEWAR_DRUM_NAMES,
  OSKIEWAR_SIGNAL_EVENTS,
  describeOskiewarSignal,
} from "../oskiewar-sfx.mjs";

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
  currentTime = 4;
  sampleRate = 8000;
  state = "suspended";
  destination = new FakeNode();
  oscillators = [];
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
    return node;
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

test("signal vocabulary covers every current oskiewar.js game event", () => {
  const required = [
    "hello", "select", "fighters", "wind", "ballserve", "countdown",
    "tie", "roundwin", "matchwin", "move", "jump", "ultrajump",
    "fastdrop", "dash", "shield", "punch", "kick", "block", "reach",
    "grab", "release", "pogo", "bullet", "grenade", "blast", "cancel",
    "pickup", "powerup", "wack", "crosswack", "boot", "bodybounce",
    "ballblock", "bodyhit", "partdamage", "partremoved", "balled", "ko",
    "killcam",
  ];
  assert.deepEqual([...OSKIEWAR_SIGNAL_EVENTS].sort(), required.sort());
  for (const event of required)
    assert.ok(describeOskiewarSignal(event), `${event} has a sound route`);
});

test("router gives fighters stable stereo positions and clamps overrides", () => {
  assert.equal(describeOskiewarSignal("kick", 0).pan, -0.62);
  assert.equal(describeOskiewarSignal("kick", 1).pan, 0.62);
  assert.equal(describeOskiewarSignal("wind", -1).pan, 0);
  assert.equal(describeOskiewarSignal("kick", 0, 0, 0, { pan: 8 }).pan, 1);
  assert.equal(describeOskiewarSignal("not-real"), null);
});

test("bank is inert before explicit audio unlock", async () => {
  let constructions = 0;
  const bank = createOskiewarSfx({ contextFactory: () => {
    constructions += 1;
    return new FakeAudioContext();
  } });
  assert.equal(bank.signal("kick", 0), false);
  assert.equal(bank.drum("kick"), false);
  assert.equal(constructions, 0);
  assert.equal(await bank.unlock(), true);
  assert.equal(constructions, 1);
});

test("every signal and legacy drum route schedules bounded WebAudio voices", async () => {
  const context = new FakeAudioContext();
  const bank = createOskiewarSfx({ context, maxVoices: 256 });
  await bank.unlock();
  for (const event of OSKIEWAR_SIGNAL_EVENTS)
    assert.equal(bank.signal(event, event === "wind" ? -1 : 0, 1, 2), true, event);
  for (const drum of OSKIEWAR_DRUM_NAMES)
    assert.equal(bank.drum(drum, 0.8, 0.4), true, drum);
  assert.equal(bank.playedEvents,
    OSKIEWAR_SIGNAL_EVENTS.length + OSKIEWAR_DRUM_NAMES.length);
  assert.ok(context.oscillators.length > OSKIEWAR_SIGNAL_EVENTS.length);
  assert.ok(context.bufferSources.length > 10);
  assert.ok(context.buffers.length === 1, "noise buffer is allocated once");
  assert.ok(context.panners.some((node) => node.pan.value < 0));
  assert.equal(bank.signal("unknown"), false);
});

test("offline bank schedules cues on the demo clock without resuming", async () => {
  const context = new FakeAudioContext();
  let resumed = false;
  context.resume = async () => { resumed = true; context.state = "running"; };
  const bank = createOskiewarSfx({ context, offline: true });
  assert.equal(await bank.unlock(), true);
  assert.equal(resumed, false);
  assert.equal(bank.drumAt(1.25, "kick", 1, -.2), true);
  assert.equal(bank.signalAt(2.5, "punch", 0, 0, 0, { pan: -.4 }), true);
  const starts = [...context.oscillators, ...context.bufferSources]
    .flatMap((source) => source.started.map(([at]) => at));
  assert.ok(starts.includes(1.25));
  assert.ok(starts.includes(2.5));
});

test("event listener bridge forwards signal details and detaches", async () => {
  const context = new FakeAudioContext();
  const bank = createOskiewarSfx({ context });
  await bank.unlock();
  const listeners = new Map();
  const target = {
    addEventListener(name, listener) { listeners.set(name, listener); },
    removeEventListener(name, listener) {
      if (listeners.get(name) === listener) listeners.delete(name);
    },
  };
  const detach = bank.connectSignals(target);
  const before = context.oscillators.length + context.bufferSources.length;
  listeners.get("oskiewar:signal")({ detail: {
    event: "ballblock", player: 1, value: -1, value2: 2100, pan: 0.25,
  } });
  assert.ok(context.oscillators.length + context.bufferSources.length > before);
  assert.ok(context.panners.some((node) => node.pan.value === 0.25));
  detach();
  assert.equal(listeners.has("oskiewar:signal"), false);
});
