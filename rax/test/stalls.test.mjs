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
  // A second one inside the same quarter second is counted and waits its turn.
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

test('a stall during a dead socket is held and sent when it opens', () => {
  const sent = [];
  let clock = 1000, open = false;
  const s = new Stalls({ send: (m) => (open ? (sent.push(m), true) : false), now: () => clock });
  s.begin(1000, true);
  s.end({ work: 90, render: 80 });
  clock = 1100;
  s.begin(1100, true);
  assert.equal(s.count, 1);
  assert.equal(sent.length, 0);
  open = true;
  clock = 3100;
  s.begin(3100, false);
  assert.equal(sent.length, 1);
  assert.equal(sent[0].ago, 2000);
  assert.equal(stallRow(sent[0], { at: 5000 }).at, 3000);
});

test('a quiet server is a net stall, but only while watched', () => {
  const sent = [];
  let clock = 1000;
  const s = new Stalls({ send: (m) => (sent.push(m), true), now: () => clock });
  const frames = (until, watching = true) => {
    for (; clock < until; clock += 16) s.begin(clock, watching, { rtt: 40 });
  };
  frames(1032); s.heard();
  frames(1064); s.heard();
  frames(1664); s.heard(); // Frames kept coming; the server said nothing.
  assert.equal(s.count, 1);
  assert.match(s.note(), /^1 stalls \d+ms net$/);
  frames(2000);
  assert.equal(sent[0].cause, 'net');
  assert.equal(sent[0].rtt, 40);
  assert.equal(stallRow(sent[0]).cause, 'net');
  // Hidden, then back: the silence in between is not counted.
  frames(2100, false); clock = 9000; s.heard();
  s.begin(clock, true); clock += 16; s.heard();
  assert.equal(s.count, 1);
});

test('a slow run on the card is one gpu row, its worst frame', () => {
  const sent = [];
  let clock = 1000;
  const s = new Stalls({ send: (m) => (sent.push(m), true), now: () => clock });
  const frame = (ms, queued) => {
    s.begin(clock, true, { building: 1 });
    if (ms) s.gpu(ms, queued);
    clock += 16;
  };
  frame(9, 1); frame(12, 1);
  assert.equal(s.count, 0);
  frame(70, 3); frame(180, 9); frame(90, 4);
  assert.equal(s.count, 0); // Still behind: the row waits for the worst.
  frame(8, 1);
  assert.equal(s.count, 1);
  frame(0); frame(0);
  assert.deepEqual({ cause: sent[0].cause, gap: sent[0].gap, queued: sent[0].queued, building: sent[0].building },
    { cause: 'gpu', gap: 180, queued: 9, building: 1 });
  assert.equal(stallRow(sent[0]).queued, 9);
  // A card that never catches up still reports, once a second.
  for (let i = 0; i < 70; i++) frame(120, 6);
  assert.equal(s.count, 2);
  frame(8, 0); // And the rest of that run closes when it does.
  assert.equal(s.count, 3);
  // Nobody looking: nothing is opened.
  s.begin(clock, false); s.gpu(400, 20);
  s.begin(clock + 16, false); s.gpu(5, 0);
  assert.equal(s.count, 3);
});
