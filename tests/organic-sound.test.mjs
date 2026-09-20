import assert from "node:assert/strict";
import test from "node:test";
import { createOrganic, ORGANICS } from "../system/public/aesthetic.computer/lib/sound/organic.mjs";
import Synth from "../system/public/aesthetic.computer/lib/sound/synth.mjs";

globalThis.sampleRate = 48000;

for (const kind of Object.keys(ORGANICS)) {
  test(`${kind} renders finite audible samples and ends`, () => {
    const voice = createOrganic({ kind, id: 1, params: { duration: 0.2 } });
    let peak = 0;
    for (let i = 0; i < sampleRate; i++) {
      const sample = voice.next();
      assert.ok(Number.isFinite(sample));
      assert.ok(Math.abs(sample) <= 1);
      peak = Math.max(peak, Math.abs(sample));
    }
    assert.ok(peak > 0.001);
    assert.equal(voice.playing, false);
    assert.equal(voice.next(), 0);
  });
  test(`${kind} updates pitch, volume and pan, then fades to silence`, () => {
    const voice = createOrganic({ kind, id: 2, params: { duration: "🔁", count: 100 } });
    voice.update({ volume: 0.4, pan: 1, pitch: 300, duration: 0.01 });
    for (let i = 0; i < 600; i++) assert.ok(Number.isFinite(voice.next()));
    assert.ok(Math.abs(voice.volume - 0.4) < 1e-10);
    assert.ok(Math.abs(voice.pan(0, 1)) < 1e-10);
    voice.kill(0.01);
    for (let i = 0; i < 600; i++) assert.ok(Number.isFinite(voice.next()));
    assert.equal(voice.playing, false);
    assert.equal(voice.next(), 0);
  });
}

test("synth expression combines slide, vibrato and formants without invalid samples", () => {
  const synth = new Synth({ type: "sawtooth", id: 3,
    options: { tone: 220, slide: 440, vibrato: 0.2, formant: "a" },
    duration: 4800, attack: 480, decay: 960, volume: 0.5, pan: 0 });
  let peak = 0;
  for (let i = 0; i < sampleRate; i++) {
    const sample = synth.next();
    assert.ok(Number.isFinite(sample));
    peak = Math.max(peak, Math.abs(sample));
  }
  assert.ok(peak > 0.001);
  assert.equal(synth.playing, false);
});
