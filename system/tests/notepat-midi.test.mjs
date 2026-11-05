import test from "node:test";
import assert from "node:assert/strict";
import { createMidiController } from "../public/aesthetic.computer/lib/midi-controller.mjs";
import { createMidiPerformance } from "../public/aesthetic.computer/lib/notepat-midi-performance.mjs";
import { createGmBridge } from "../public/aesthetic.computer/lib/notepat-gm-bridge.mjs";
import { GM_PROGRAM_NAMES, gmDigitSelection } from "../public/aesthetic.computer/lib/notepat-programs.mjs";
import { classicNotepatLayout } from "../public/aesthetic.computer/lib/notepat-layout.mjs";

const tick = () => new Promise(resolve => setImmediate(resolve));
const deferred = () => { let resolve; const promise = new Promise(r => { resolve = r; }); return { promise, resolve }; };
function fixture() {
  const sent = [], messages = [], states = [];
  const input = { id: "keyboard", name: "Keyboard", state: "connected", open: async () => {}, close: async () => {} };
  const output = { id: "synth", name: "Synth", state: "connected", send: data => sent.push([...data]) };
  const access = { inputs: new Map([[input.id, input]]), outputs: new Map([[output.id, output]]) };
  let requests = 0;
  const controller = createMidiController({ navigator: { requestMIDIAccess: async options => { requests++; assert.deepEqual(options, { sysex: false }); return access; } }, message: e => messages.push(e), changed: e => states.push(e) });
  return { controller, access, input, output, sent, messages, states, requests: () => requests };
}

test("MIDI is explicit, connects once, filters malformed input and releases disconnected sources", async () => {
  const f = fixture();
  assert.equal(f.requests(), 0);
  await Promise.all([f.controller.connect(), f.controller.connect()]);
  assert.equal(f.requests(), 1);
  f.input.onmidimessage({ data: [0x91, 24, 110] });
  f.input.onmidimessage({ data: [0xc1, 127] });
  f.input.onmidimessage({ data: [0xb1, 64, 127] });
  for (const data of [[0x90, 60], [0xf0, 1, 2], [0x90, 200, 80], [0x90, -1, 80]]) f.input.onmidimessage({ data });
  assert.equal(f.messages.length, 3);
  assert.deepEqual(f.messages[0], { source: "keyboard", data: [0x91, 24, 110] });
  assert.equal(f.sent.length, 0, "input never echoes to output");
  f.input.state = "disconnected"; f.access.onstatechange({ port: f.input });
  assert.equal(f.input.onmidimessage, null);
  assert.deepEqual(f.messages.at(-1), { source: "keyboard", disconnected: true });
  f.input.state = "connected"; f.access.onstatechange({ port: f.input });
  assert.equal(typeof f.input.onmidimessage, "function");
  f.controller.disconnect();
  assert.equal(f.access.onstatechange, null);
  assert.equal(f.input.onmidimessage, null);
});

test("output is selected explicitly; overlapping owners, channel changes and panic cannot leave held notes", async () => {
  const f = fixture(); await f.controller.connect();
  assert.equal(f.controller.noteOn(1, 60), false);
  f.controller.selectOutput("synth");
  f.controller.noteOn(1, 60, 90); f.controller.noteOn(2, 60, 100);
  assert.deepEqual(f.sent, [[0x90, 60, 90]]);
  f.controller.noteOff(1); assert.equal(f.sent.length, 1);
  f.controller.noteOff(2); assert.deepEqual(f.sent.at(-1), [0x80, 60, 0]);
  f.controller.noteOn(3, 64, 100); f.controller.selectChannel(3);
  assert.deepEqual(f.sent.at(-1), [0x80, 64, 0]);
  f.controller.noteOn(4, 67, 90); f.controller.program(127);
  assert.deepEqual(f.sent.at(-1), [0xc3, 127]);
  f.controller.selectOutput(""); assert.deepEqual(f.sent.at(-1), [0x83, 67, 0]);
  const count = f.sent.length; f.controller.panic(); assert.equal(f.sent.length, count);
});

test("late permission completion cannot reconnect a disposed instrument; permission denial is retryable", async () => {
  const pending = deferred(); let requested = 0;
  const controller = createMidiController({ navigator: { requestMIDIAccess: () => { requested++; return pending.promise; } } });
  const first = controller.connect(); await tick(); controller.disconnect();
  pending.resolve({ inputs: new Map(), outputs: new Map() }); await first;
  assert.equal(controller.state().connected, false);
  assert.equal(requested, 1);
  const denied = createMidiController({ navigator: { requestMIDIAccess: async () => { throw Error("denied"); } } });
  await denied.connect(); assert.match(denied.state().error, /permission/); assert.equal(denied.state().pending, false);
  await denied.connect(); assert.equal(denied.state().pending, false);
  assert.equal(createMidiController({ navigator: {} }).state().supported, false);
});

test("MIDI preserves exact pitch, velocity and independent source/channel sustain and pitch bend", () => {
  const starts = [], stops = [], bends = [], programs = [];
  const player = createMidiPerformance({ start: v => { starts.push({ ...v }); return {}; }, stop: v => stops.push(v.id), bend: (v, n) => bends.push([v.id, n]), program: n => programs.push(n) });
  const send = (source, data) => player.receive({ source, data });
  send("a", [0x90, 24, 31]); send("b", [0x90, 24, 100]); send("a", [0x91, 24, 80]);
  assert.deepEqual(starts.map(v => [v.note, v.velocity]), [[24,31],[24,100],[24,80]]);
  send("a", [0xb0, 64, 127]); send("a", [0x90, 24, 0]);
  assert.equal(player.notes.size, 3, "velocity-zero note-off obeys sustain");
  send("a", [0xe1, 0, 96]); assert.equal(bends.at(-1)[1], 1);
  assert.equal(bends.at(-1)[0], starts[2].id, "bend remains on its source/channel");
  send("a", [0xb0, 64, 0]); assert.equal(player.notes.size, 2);
  player.receive({ source: "a", disconnected: true }); assert.equal(player.notes.size, 1);
  send("b", [0xc0, 127]); assert.deepEqual(programs, [127]);
  send("b", [0xb0, 120, 0]); assert.equal(player.notes.size, 0);
  assert.equal(stops.length, 3);
});

test("GM uses native 1–128 numbering and zero selects MIDI-only mode", () => {
  assert.equal(GM_PROGRAM_NAMES.length, 128);
  assert.equal(gmDigitSelection("", "1", 1000, 0).program, 0);
  assert.equal(gmDigitSelection("12", "8", 1100, 1000).program, 127);
  assert.equal(gmDigitSelection("12", "1", 2000, 1000).program, 0);
  assert.equal(gmDigitSelection("", "0", 1000, 0).passthrough, true);
});

test("GM bridge cancels early releases and stale program loads, and retains bends while a patch loads", async () => {
  const requests = new Map(), played = [], updated = [], statuses = [];
  const bridge = createGmBridge({ context: () => ({}), destination: () => ({}), status: s => statuses.push(s), load: n => { const d = deferred(); requests.set(n, d); return d.promise; } });
  const patch = { prefetch: async () => ({}), play: (n, options) => { played.push([n, options]); return { update: x => updated.push(x), release() {} }; } };
  const early = bridge.note({ id: 1, program: 0, note: 60 });
  bridge.stop(1); requests.get(0).resolve(patch); await early;
  assert.equal(played.length, 0, "releasing during download cannot produce a ghost note");
  const held = bridge.note({ id: 2, program: 2, note: 70, velocity: 50 });
  bridge.update({ id: 2, semitones: 1.5 }); requests.get(2).resolve(patch); await held;
  assert.equal(played[0][0], 70); assert.equal(played[0][1].velocity, 50); assert.equal(updated.at(-1).semitones, 1.5);
  const older = bridge.program(3), newer = bridge.program(4);
  requests.get(4).resolve(patch); await newer;
  requests.get(3).resolve(patch); await older;
  assert.equal(statuses.at(-1).program, 4);
  bridge.panic();
});

test("the two octave grids remain inside the playing area across mobile and desktop layouts", () => {
  for (const [width,height] of [[160,300],[195,400],[300,500],[320,180],[480,270],[800,500]]) {
    const l = classicNotepatLayout({width,height});
    const rects = Array.from({length:24}, (_,i) => {
      const side = i >= 12, row = Math.floor((l.splitLayout ? i % 12 : i) / 4), col = i % 4;
      return { x: (l.splitLayout && side ? l.rightOctaveX : l.leftOctaveX) + col*l.buttonWidth,
        y: l.topButtonY + row*l.buttonHeight + (!l.splitLayout && side ? l.octaveGap : 0), w:l.buttonWidth, h:l.buttonHeight };
    });
    for (const rect of rects) {
      assert.ok(rect.x >= 0 && rect.x+rect.w <= width, `${width}×${height} horizontal fit`);
      assert.ok(rect.y >= 33 && rect.y+rect.h <= height-2, `${width}×${height} header/edge fit`);
    }
    for (let i=0;i<24;i++) for(let j=i+1;j<24;j++) {
      const a=rects[i],b=rects[j]; assert.ok(a.x+a.w<=b.x || b.x+b.w<=a.x || a.y+a.h<=b.y || b.y+b.h<=a.y);
    }
    assert.ok(l.buttonWidth >= 18 && l.buttonHeight >= 18, "pads remain usable");
  }
});

test("GM sample voices stay on their AudioContext and cancel or bend during lazy loading", async () => {
  const { loadPatch, setGMAssetBase } = await import("../public/aesthetic.computer/lib/gm.mjs");
  const originalFetch = globalThis.fetch, sample = deferred();
  globalThis.fetch = async url => String(url).endsWith("manifest.json")
    ? { ok: true, json: async () => ({ patches: { 0: { notes: [60] } } }) }
    : { ok: true, arrayBuffer: () => sample.promise };
  const param = () => ({ value: 0, setValueAtTime(v) { this.value = v; }, linearRampToValueAtTime(v) { this.value = v; }, setTargetAtTime(v) { this.value = v; }, cancelAndHoldAtTime() {} });
  const context = () => ({ currentTime: 1, destination: {}, started: [], stopped: [],
    decodeAudioData: async () => ({}),
    createGain: () => ({ gain: param(), connect() {}, disconnect() {} }),
    createStereoPanner: () => ({ pan: param(), connect() {}, disconnect() {} }),
    createBufferSource() { const ctx = this; return { playbackRate: param(), connect() {}, disconnect() {}, start() { ctx.started.push(this); }, stop() { ctx.stopped.push(this); } }; }
  });
  try {
    setGMAssetBase("https://example.invalid/gm");
    assert.equal(await loadPatch(0), null, "a missing context cannot poison a later load");
    const a = context(), b = context(), patchA = await loadPatch(0, a), patchB = await loadPatch(0, b);
    assert.notEqual(patchA, patchB);
    const early = patchA.play(60); early.release();
    const held = patchB.play(60); held.update({ semitones: 12 });
    sample.resolve(new ArrayBuffer(1)); await tick();
    assert.equal(a.started.length, 0);
    assert.equal(b.started.length, 1);
    assert.equal(b.started[0].playbackRate.value, 2);
    held.release(); held.release(); assert.equal(b.stopped.length, 1);
  } finally { globalThis.fetch = originalFetch; setGMAssetBase("https://assets.aesthetic.computer/gm"); }
});
