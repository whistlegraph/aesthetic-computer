import test from 'node:test';
import assert from 'node:assert/strict';
import { filterFeed, renderSubFeed } from './notespatial-room-audio.mjs';

const sr = 8000;
const rms = a => Math.sqrt(a.reduce((s, v) => s + v * v, 0) / a.length);
test('SUB crossover retains 60 Hz and attenuates DC and upper harmonics', () => {
  const level = hz => {
    const a = Float32Array.from({ length: sr }, (_, i) => Math.sin(2 * Math.PI * hz * i / sr));
    filterFeed(a, sr, 'highpass', 25);
    filterFeed(a, sr, 'lowpass', 80); filterFeed(a, sr, 'lowpass', 80);
    return rms(a.slice(sr / 2));
  };
  assert.ok(level(60) > .4);
  assert.ok(level(5) < level(60) / 10);
  assert.ok(level(400) < level(60) / 100);
});

test('SUB render transposes the body, honors level and preserves a cropped note', () => {
  const score = { dur: 2, gain: .36, lanes: [{ name: 'kick', events: [
    { t: .2, dur: 1.5, hz: 120, g: .4, wave: 'sine', attack: .01, decay: .1 },
  ] }] };
  const full = renderSubFeed(score, { sampleRate: sr, level: .25 });
  assert.equal(full.events[0].hz, 60);
  assert.ok(rms(full.feed.slice(sr / 2, sr)) > .01);
  assert.equal(rms(full.feed.slice(0, sr * .2)), 0);
  const mute = renderSubFeed(score, { sampleRate: sr, level: 0 });
  assert.equal(rms(mute.feed), 0);
  const cut = renderSubFeed(score, { sampleRate: sr, from: .5, to: 1 });
  assert.deepEqual(cut.feed.slice(0, sr / 2), full.feed.slice(sr / 2, sr));
  assert.ok(full.feed.every(v => Number.isFinite(v) && Math.abs(v) <= .89));
  const fractional = renderSubFeed(score, { sampleRate: sr, from: 1.000075, to: 1.5001125 });
  assert.equal(fractional.feed.length, Math.ceil((1.5001125 - 1.000075 + 1.5) * sr));
  assert.ok(fractional.feed.every(Number.isFinite));
});
