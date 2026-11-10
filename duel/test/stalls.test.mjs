import test from 'node:test';
import assert from 'node:assert/strict';
import { Stalls, cause, stallRow } from '../stalls.mjs';

test('a stall is blamed on the largest thing seen inside it', () => {
  assert.equal(cause({ gap: 100, work: 80, render: 70 }), 'render');
  assert.equal(cause({ gap: 100, work: 80, render: 10 }), 'step');
  assert.equal(cause({ gap: 100, work: 5, message: 60, messageType: 'objects' }), 'socket:objects');
  assert.equal(cause({ gap: 100, work: 5, message: 60, messageType: '<b>' }), 'socket:other');
  assert.equal(cause({ gap: 100, work: 5, message: 3 }), 'starved');
});

test('only watched gaps count, and reports are rate limited', () => {
  const sent = [];
  let clock = 0;
  const s = new Stalls({ send: (m) => sent.push(m), now: () => clock });
  s.begin(0, true);
  s.end({ work: 4, render: 2 });
  s.begin(16, true);
  assert.equal(s.count, 0);
  s.handled('objects', 70);
  clock = 116;
  s.begin(116, true, { building: 1 });
  assert.equal(s.count, 1);
  assert.equal(sent[0].cause, 'socket:objects');
  assert.equal(sent[0].building, 1);
  // Coming back from a hidden tab is not a stall.
  s.begin(132, false);
  s.begin(5000, true);
  assert.equal(s.count, 1);
  // A second one inside the same quarter second is counted, not sent.
  s.end({ work: 90, render: 80 });
  clock = 5100;
  s.begin(5100, true);
  s.end({ work: 90, render: 80 });
  clock = 5200;
  s.begin(5200, true);
  assert.equal(s.count, 3);
  assert.equal(sent.length, 2);
  assert.match(s.note(), /^3 stalls 100ms render$/);
});

test('the server keeps numbers and known words only', () => {
  const row = stallRow({ gap: 120.4, work: 3, cause: 'socket:objects', messageType: 'objects', backend: 'webgpu',
    agent: 'safari', hosted: true, handle: '@someone', building: 2, objects: 1e12 }, { at: 1, session: 'ab', revision: 'r' });
  assert.deepEqual(row, { at: 1, session: 'ab', revision: 'r', cause: 'socket:objects', gap: 120, work: 3,
    building: 2, objects: 600000, messageType: 'objects', backend: 'webgpu', agent: 'safari', hosted: true });
  assert.equal(stallRow({ gap: 120, cause: 'because <script>' }), null);
  assert.equal(stallRow({ gap: 10, cause: 'starved' }), null);
});
