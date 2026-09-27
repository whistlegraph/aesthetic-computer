import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync, mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { renderGmBank } from './notespatial-gm-audio.mjs';
import { renderSeatEffects } from './notespatial-fx-audio.mjs';
import { noteLevel } from './notespatial-gm-balance.mjs';
import { makeSubScore } from './sub-receiver/core.mjs';

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
    const levels = JSON.parse(readFileSync(new URL('./notespatial-gm-levels.json', import.meta.url)));
    const raw = [], balanced = [];
    for (const e of events) {
      const samples = bank.read(e);
      assert.equal(samples.length, 12000);
      let energy = 0;
      for (const v of samples) { assert.ok(Number.isFinite(v), `GM ${e.gm}`); energy += v * v; }
      assert.ok(energy > 1e-7, `GM ${e.gm} silent`);
      const level = noteLevel(samples, 16000).rms;
      raw.push(level); balanced.push(level * levels.programs[e.gm].gain);
    }
    const spread = a => { const s = a.toSorted((a, b) => a - b); return 20 * Math.log10(s[115] / s[12]); };
    console.log(`held-out register/envelope: raw ${spread(raw).toFixed(1)} dB, balanced ${spread(balanced).toFixed(1)} dB (10th–90th percentile)`);
    assert.ok(spread(balanced) < spread(raw), 'trims must improve an independent audition');
  } finally { bank?.close(); rmSync(work, { recursive: true, force: true }); }
});

test('level measurement ignores release silence and follows amplitude', () => {
  const a = Float32Array.from({ length: 4000 }, (_, i) => Math.sin(2 * Math.PI * i * 220 / 8000));
  const padded = new Float32Array(8000); padded.set(a);
  assert.ok(Math.abs(noteLevel(a, 8000).rms - Math.SQRT1_2) < .005);
  assert.equal(noteLevel(a, 8000).rms, noteLevel(padded, 8000).rms);
  assert.ok(Math.abs(noteLevel(a.map(x => x * .5), 8000).rms / noteLevel(a, 8000).rms - .5) < 1e-6);
});

test('program trims leave the separately synthesized SUB unchanged', () => {
  const s = JSON.parse(readFileSync(new URL('../scores/notespatial-native-gm128-fx-kick.nsscore', import.meta.url)));
  assert.equal(s.gmGains.length, 128);
  assert.deepEqual(makeSubScore(s), makeSubScore({ ...s, gmGains: undefined }));
});

test('native playback applies program 0 and 127 trims to note-on and subsequent gain updates', async () => {
  const piece = await import('../pieces/spatial-rehearsal.mjs?gain-test');
  const e = { t: 0, dur: 1, hz: 440, g: .4, wave: 'sine' };
  const s = { dur: 2, gain: .5, center: 0, seats: 2, gmGains: Array(128).fill(1), lanes: [{ center: true, events: [{ ...e, gm: 0 }, { ...e, gm: 127 }, e] }] };
  s.gmGains[0] = .25; s.gmGains[127] = 1.5;
  let cmd = null; const played = [], updates = [];
  const sound = { time: 0, microphone: { close() {} }, synth(options) {
    played.push(options); return { update(o) { updates.push(o.volume); }, kill() {} };
  } };
  const system = { startSSH() {}, writeFile() {}, readFile(p) {
    return JSON.stringify(p.endsWith('config.json') ? { seat: 0, seats: 2, maxSeconds: 2 } : cmd);
  }, readFileBytes() { return new TextEncoder().encode(JSON.stringify(s)).buffer; } };
  const api = { sound, system, screen: { width: 455, height: 256 } };
  piece.boot(api);
  cmd = { id: 'prepare', action: 'prepare', startAt: 3, mode: 'score' }; sound.time = .1; piece.sim(api);
  cmd = { id: 'play', action: 'play' }; sound.time = .2; piece.sim(api);
  sound.time = 3.01; piece.sim(api);
  sound.time = 3.51; piece.sim(api);
  assert.deepEqual(played.map(p => +p.volume.toFixed(4)), [.05, .3, .2]);
  assert.deepEqual(updates.map(v => +v.toFixed(4)), [.05, .3, .2]);
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
