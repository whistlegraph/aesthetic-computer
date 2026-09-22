import test from 'node:test';
import assert from 'node:assert/strict';
import { rotationAt, voicePosition, seatGain, sourceGain, hasFocus, ringSeats } from '../lib/spatial-rehearsal.mjs';
import { createSpatialDataServer } from './spatial-data.mjs';
import { readFileSync } from 'node:fs';

const score = { dur: 10, rotation: [0, 1, 0], lanes: [{}, {}] };
test('rotation integrates ribbon area independently of frame history', () => {
  assert.ok(Math.abs(rotationAt(score, 10) - (1.5 + Math.PI * 5)) < 1e-9);
  const pinned = { ...score, lanes: [{ az: .3, el: .5 }, {}] };
  assert.equal(voicePosition(pinned, 0, 8).angle, .3);
  assert.deepEqual(voicePosition(score, 1, 7), voicePosition(score, 1, 7));
});
test('a center seat takes only center lanes and the ring keeps its power', () => {
  const s = { dur: 10, center: 5, lanes: [{ az: 0 }, { center: true }, { orbitSeconds: 8 }] };
  assert.equal(ringSeats(s, 6), 5);
  for (const t of [0, 1.3, 7.7]) {
    const pc = voicePosition(s, 1, t);
    assert.deepEqual([0,1,2,3,4,5].map(k => sourceGain(s, pc, k, 6)), [0, 0, 0, 0, 0, 1]);
    for (const lane of [0, 2]) {
      const gains = [0,1,2,3,4,5].map(k => sourceGain(s, voicePosition(s, lane, t), k, 6));
      assert.equal(gains[5], 0);
      assert.ok(Math.abs(gains.reduce((a, g) => a + g * g, 0) - 1) < 1e-10);
    }
  }
  assert.equal(sourceGain(s, voicePosition(s, 0, 0), 0, 6), 1); // az 0 is ring seat 1, still index 0
});
test('routing conserves summed power across 3–6 seats and negative angles', () => {
  for (const n of [3, 4, 5, 6]) for (let a = -7; a < 7; a += .031) {
    const gains = Array.from({ length: n }, (_, i) => seatGain(a, i, n));
    assert.ok(Math.abs(gains.reduce((s, g) => s + g * g, 0) - 1) < 1e-10);
    assert.ok(gains.filter(g => g > 0).length <= 2);
  }
});
test('runtime closes capture and never opens or records a microphone', async () => {
  const piece = await import('../pieces/spatial-rehearsal.mjs?test');
  const calls = []; let cmd = null; const statuses = [];
  const system = { startSSH() {}, readFile(path) {
    if (path.endsWith('config.json')) return JSON.stringify({ seat: 0, seats: 5, maxSeconds: 20 });
    if (path.endsWith('.nsscore')) return JSON.stringify({ ...score, lanes: [{ events: [] }, { events: [] }] });
    return JSON.stringify(cmd);
  }, writeFile(path, s) { if (path.endsWith('status.json')) statuses.push(JSON.parse(s)); } };
  const sound = { time: 0, microphone: { hot: false, recording: false,
    close() { calls.push('close'); }, open() { throw Error('must never open'); }, rec() { throw Error('must never record'); } },
    speaker: { amplitudes: { left: .2, right: .2 } }, synth() { calls.push('synth'); return { kill() {} }; } };
  const api = { sound, system, screen: { width: 455, height: 256 } };
  piece.boot(api);
  cmd = { id: 'prepare', action: 'prepare', startAt: 3, mode: 'beeps' }; sound.time = .1; piece.sim(api);
  cmd = { id: 'play', action: 'play' }; sound.time = .2; piece.sim(api);
  for (let t = 3; t < 7; t += .025) { sound.time = t; piece.sim(api); }
  assert.equal(calls.filter(c => c === 'close').length, 1);
  assert.equal(calls.filter(c => c === 'synth').length, 4);
  assert.equal(statuses.at(-1).microphone.recording, false);
  // Re-reading the same prepare command must not reset the epoch.
  assert.equal(statuses.at(-1).origin, 3);
});
test('bridge marks a frozen file stale even while HTTP requests succeed', async () => {
  let now = 1000, audioTime = 1;
  const bridge = createSpatialDataServer(['seat.local'], { interval: 100000, now: () => now,
    fetcher: async () => ({ ok: true, text: async () => JSON.stringify({ seat: 0, audioTime }) }) });
  await new Promise(r => setImmediate(r));
  assert.equal(bridge.snapshot().seats[0].stale, false);
  now += 2100; await bridge.pollAll();
  assert.equal(bridge.snapshot().seats[0].connected, true);
  assert.equal(bridge.snapshot().seats[0].stale, true);
  audioTime++; await bridge.pollAll();
  assert.equal(bridge.snapshot().seats[0].stale, false);
  bridge.server.emit('close');
});
test('bridge rejects writes and serves browser-readable status', async () => {
  const bridge = createSpatialDataServer([], { interval: 100000 });
  await new Promise(r => bridge.server.listen(0, '127.0.0.1', r));
  try {
    const url = `http://127.0.0.1:${bridge.server.address().port}`;
    const r = await fetch(url + '/api/seats');
    assert.equal(r.headers.get('access-control-allow-origin'), '*');
    assert.deepEqual((await r.json()).seats, []);
    assert.equal((await fetch(url + '/api/seats', { method: 'PUT', body: 'no' })).status, 405);
  } finally { await new Promise(r => bridge.server.close(r)); }
});

test('line reaches every assigned seat, conserves power and never wraps', () => {
  const line = JSON.parse(readFileSync(new URL('../scores/sine-line.nsscore', import.meta.url)));
  for (const [i,e] of line.lanes[0].events.entries()) {
    const seat = line.seatOrder[i % line.seatOrder.length];
    assert.ok(sourceGain(line, voicePosition(line, 0, e.t), seat, 6) > .999999);
    assert.equal(hasFocus(line, seat, 6, e.t), true);
  }
  for (let u = 0; u <= 1; u += .001) {
    const gains = line.seatOrder.map(seat => sourceGain(line, {line:u}, seat, 6));
    assert.ok(Math.abs(gains.reduce((sum,g)=>sum+g*g,0)-1) < 1e-10);
    assert.ok(gains.filter(g=>g>0).length <= 2);
  }
  assert.equal(sourceGain(line, {line:1}, line.seatOrder[0], 6), 0);
  const half = line.linePasses[0].step / 2;
  assert.equal(hasFocus(line, line.seatOrder[0], 6, half), false);
  assert.equal(hasFocus(line, line.seatOrder[1], 6, half), false);
});

test('polyrhythm has three melody attacks per two drum pulses and independent paths', () => {
  const s = JSON.parse(readFileSync(new URL('../scores/polyrhythm-bounce.nsscore', import.meta.url)));
  const melody = s.lanes[0].events, pulses = s.lanes[1].events.filter(e => e.hz === 1100);
  assert.equal(melody.length / pulses.length, 3 / 2);
  assert.equal(voicePosition(s, 0, 0).line, 1);
  assert.equal(voicePosition(s, 1, 0).line, 0);
  for (let lane = 0; lane < 2; lane++) {
    const events = lane === 0 ? melody : pulses;
    for (const [i, e] of events.entries()) {
      const seat = (i + (lane === 0 ? 1 : 0)) % 2;
      assert.ok(sourceGain(s, voicePosition(s, lane, e.t), seat, 2) > .999);
    }
  }
  // Neither idle routing position should light a laptop in a silent gap.
  assert.equal(hasFocus(s, 0, 2, .3), false);
  assert.equal(hasFocus(s, 1, 2, .3), false);
});
