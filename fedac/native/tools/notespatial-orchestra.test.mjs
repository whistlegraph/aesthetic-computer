import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync, mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { renderGmBank } from './notespatial-gm-audio.mjs';
import { renderSeatEffects } from './notespatial-fx-audio.mjs';

test('suite uses all 128 audible GM programs without breaking the 32-voice budget', () => {
  const score = JSON.parse(readFileSync(new URL('../scores/notespatial-native-gm128-fx-kick.nsscore', import.meta.url)));
  const events = score.lanes.flatMap(l => l.events);
  const programs = new Set(events.filter(e => Number.isInteger(e.gm) && e.g > .01 && e.dur >= .03).map(e => e.gm));
  assert.equal(programs.size, 128);
  assert.ok([...programs].every(p => p >= 0 && p < 128));
  const edges = events.flatMap(e => [[e.t, 1], [e.t + e.dur, -1]]).sort((a, b) => a[0] - b[0] || a[1] - b[1]);
  let live = 0, peak = 0;
  for (const [, delta] of edges) { live += delta; peak = Math.max(peak, live); }
  assert.ok(peak <= 32, `peak ${peak}`);
  for (const cue of score.orchestra.cues) assert.ok(events.some(e => e.gm === cue.program && e.t === cue.t));
  for (const m of score.movements) {
    const hits = score.lanes.flatMap(l => l.events.filter(e => /tap|kick|snare|hats/.test(l.name) || e.wave === 'noise')).filter(e => e.t >= m.t0 && e.t < m.t1 - 3).sort((a, b) => a.t - b.t);
    assert.ok(hits.length > 0, m.name);
    for (let i = 1; i < hits.length; i++) assert.ok(hits[i].t - hits[i - 1].t < 1.2, `${m.name} loses pulse`);
  }
});

test('native core renders every GM program to finite, non-silent PCM including program zero', async () => {
  const work = mkdtempSync(join(tmpdir(), 'notespatial-gm-test-'));
  let bank;
  try {
    const events = Array.from({ length: 128 }, (_, gm) => ({ gm, t: gm, hz: 261.63, dur: .75, attack: .01, decay: .2 }));
    bank = await renderGmBank(events, { work, sampleRate: 16000 });
    for (const e of events) {
      const samples = bank.read(e);
      assert.equal(samples.length, 12000);
      let energy = 0;
      for (const v of samples) { assert.ok(Number.isFinite(v), `GM ${e.gm}`); energy += v * v; }
      assert.ok(energy > 1e-7, `GM ${e.gm} silent`);
    }
  } finally { bank?.close(); rmSync(work, { recursive: true, force: true }); }
});

test('seat effects make tails, obey overrides, and remain finite', () => {
  const sr = 44100, dry = new Float32Array(sr);
  for (let i = 0; i < sr / 2; i++) dry[i] = .2 * Math.sin(i * Math.PI * 2 * 220 / sr);
  const score = { dur: 1, fxRoom: [.5, .5], seatFx: { 1: { fxRoom: [0, 0] } } };
  const wet = renderSeatEffects(dry.slice(), score, 0, { sampleRate: sr });
  const bypass = renderSeatEffects(dry.slice(), score, 1, { sampleRate: sr });
  assert.deepEqual(bypass, dry);
  assert.ok(wet.slice(Math.ceil(sr * .62)).some(v => Math.abs(v) > 1e-4));
  for (const key of ['fxDrive', 'fxWobble', 'fxGlitch']) {
    const out = renderSeatEffects(dry.slice(), { dur: 1, [key]: [.7, .7] }, 0, { sampleRate: sr });
    assert.ok(out.every(Number.isFinite));
    assert.ok(out.some((v, i) => Math.abs(v - dry[i]) > 1e-4), key);
  }
  assert.deepEqual(renderSeatEffects(dry.slice(), { dur: 1 }, 0), dry);
});
