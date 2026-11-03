import test from 'node:test';
import assert from 'node:assert/strict';
import { homedir } from 'node:os';
import { Radial, THINGS, CATEGORIES, CHANGES, radialSlots } from '../radial.mjs';
import { devEntries, redact } from '../dev-log.mjs';
import { TouchPad } from '../touch.mjs';
import { hudLayout, joinCode } from '../hud.mjs';

test('the ring composes a new object from a thing and its traits', () => {
  const r = new Radial();
  r.begin();
  assert.deepEqual(r.items, CATEGORIES);
  r.pick(CATEGORIES.indexOf('play'));
  assert.equal(r.title, 'PLAY');
  r.back();
  assert.equal(r.category, null);
  r.pick(CATEGORIES.indexOf('play'));
  assert.deepEqual(r.items, THINGS);
  assert.equal(r.point(0, 1), 0); // straight up is the first slice
  assert.equal(r.point(1, 0), 2); // a quarter turn clockwise of eight
  assert.equal(r.point(0.1, 0.1), -1); // the middle selects nothing
  r.pick(THINGS.indexOf('a ramp'));
  assert.equal(r.text(), 'a ramp');
  r.pick(r.items.indexOf('red'));
  r.pick(r.items.indexOf('spinning'));
  assert.equal(r.text(), 'a red spinning ramp');
  assert.ok(!r.items.includes('red'));
  r.back();
  assert.equal(r.text(), 'a red ramp');
});

test('the ring edits an owned object with changes', () => {
  const r = new Radial();
  r.begin({ id: 'o', version: 2, geometry: { name: 'Car' } });
  assert.equal(r.title, 'Car V2');
  r.pick(CHANGES.indexOf('add wheels'));
  r.pick(r.items.indexOf('make it spin'));
  assert.equal(r.text(), 'add wheels, make it spin');
  assert.equal(radialSlots(8).length, 8);
});

test('dev log keeps notes and tool summaries, redacted', () => {
  const claude = JSON.stringify({
    type: 'assistant',
    message: {
      content: [
        { type: 'text', text: 'Shrinking the QR now. It was too big.' },
        { type: 'tool_use', name: 'Edit', input: { file_path: `${homedir()}/x/duel/hud.mjs` } },
        { type: 'tool_use', name: 'Bash', input: { command: 'ls', description: 'List files' } },
      ],
    },
  });
  assert.deepEqual(devEntries(claude), [
    { kind: 'say', text: 'Shrinking the QR now.' },
    { kind: 'do', text: 'EDIT hud.mjs' },
    { kind: 'do', text: 'RUN List files' },
  ]);
  const codex = JSON.stringify({
    type: 'response_item',
    payload: { type: 'custom_tool_call', input: '*** Begin Patch\n*** Update File: /Users/jas/a/duel/view.mjs\n' },
  });
  assert.deepEqual(devEntries(codex), [{ kind: 'do', text: 'EDIT view.mjs' }]);
  assert.deepEqual(devEntries('{"type":"user"}'), []);
  assert.deepEqual(devEntries('not json'), []);
  const secret = redact(`token=abc123 at /Users/jas/x sk-ant-REDACTEDREDACTED ${'a'.repeat(40)}`);
  assert.ok(!secret.includes('abc123') && !secret.includes('/Users/jas') && !secret.includes('aaaa'));
  assert.ok(!secret.includes('REDACTEDREDACTED'));
});

test('touch: left thumb moves, right taps jump, right hold opens the ring', () => {
  const t = new TouchPad();
  t.down({ pointerId: 1, clientX: 100, clientY: 500 }, 800, 600, 0);
  t.move({ pointerId: 1, clientX: 100, clientY: 300 }, 800, 600);
  const moved = t.sample(800, 600, 10);
  assert.ok(moved.fwd > 0.99 && Math.abs(moved.right) < 0.01);
  t.down({ pointerId: 2, clientX: 600, clientY: 300 }, 800, 600, 20);
  t.up({ pointerId: 2, clientX: 600, clientY: 300 }, 120);
  assert.deepEqual(t.sample(800, 600, 130).events.map((e) => e.type), ['tap']);
  t.down({ pointerId: 3, clientX: 600, clientY: 300 }, 800, 600, 200);
  assert.deepEqual(t.sample(800, 600, 300).events, []);
  assert.deepEqual(t.sample(800, 600, 700).events.map((e) => e.type), ['hold']);
  t.up({ pointerId: 3, clientX: 600, clientY: 300 }, 800);
  assert.deepEqual(t.sample(800, 600, 810).events, []); // a hold is not also a tap
  t.down({ pointerId: 4, clientX: 600, clientY: 300 }, 800, 600, 900);
  t.move({ pointerId: 4, clientX: 650, clientY: 310 }, 800, 600);
  const look = t.sample(800, 600, 910);
  assert.equal(look.dx, 50);
  assert.equal(look.dy, 10);
});

test('the join code is a corner badge, not a panel', () => {
  const cells = joinCode('https://lake-tea-journal-bureau.trycloudflare.com/?room=studio');
  for (const [w, h] of [[480, 270], [960, 540], [390, 844]]) {
    const { qrSize } = hudLayout(w, h, cells);
    assert.ok(qrSize <= Math.min(w, h) * 0.25, `${w}x${h}: ${qrSize}`);
    assert.ok(qrSize >= cells.length + 8);
  }
});
