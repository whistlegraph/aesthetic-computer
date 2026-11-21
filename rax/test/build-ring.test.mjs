import test from 'node:test';
import assert from 'node:assert/strict';
import { homedir } from 'node:os';
import { Radial, THINGS, CATEGORIES, CHANGES, radialSlots } from '../radial.mjs';
import { devEntries, redact } from '../dev-log.mjs';
import { TouchPad, touchButtons } from '../touch.mjs';
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
        { type: 'tool_use', name: 'Edit', input: { file_path: `${homedir()}/x/rax/hud.mjs` } },
        { type: 'tool_use', name: 'Bash', input: { command: 'ls', description: 'List files' } },
      ],
    },
  });
  assert.deepEqual(devEntries(claude), [
    { kind: 'say', text: 'Shrinking the QR now.', detail: [] },
    { kind: 'do', text: 'EDIT hud.mjs', detail: [] },
    { kind: 'do', text: 'RUN List files', detail: ['$ ls'] },
  ]);
  const codex = JSON.stringify({
    type: 'response_item',
    payload: { type: 'custom_tool_call', input: '*** Begin Patch\n*** Update File: /Users/jas/a/rax/view.mjs\n' },
  });
  assert.deepEqual(devEntries(codex), [{ kind: 'do', text: 'EDIT view.mjs', detail: [] }]);
  assert.deepEqual(devEntries('{"type":"user"}'), []);
  assert.deepEqual(devEntries('not json'), []);
  const secret = redact(`token=abc123 at /Users/jas/x sk-ant-REDACTEDREDACTED ${'a'.repeat(40)}`);
  assert.ok(!secret.includes('abc123') && !secret.includes('/Users/jas') && !secret.includes('aaaa'));
  assert.ok(!secret.includes('REDACTEDREDACTED'));
});

test('touch: left thumb moves, right thumb looks; a tap or a hold there does nothing', () => {
  const t = new TouchPad();
  t.down({ pointerId: 1, clientX: 100, clientY: 500 }, 800, 600, 0);
  t.move({ pointerId: 1, clientX: 100, clientY: 300 }, 800, 600);
  const moved = t.sample(800, 600, 10);
  assert.ok(moved.fwd > 0.99 && Math.abs(moved.right) < 0.01);
  t.down({ pointerId: 2, clientX: 600, clientY: 300 }, 800, 600, 20);
  t.up({ pointerId: 2, clientX: 600, clientY: 300 }, 120);
  assert.deepEqual(t.sample(800, 600, 130).events, []);
  t.down({ pointerId: 3, clientX: 600, clientY: 300 }, 800, 600, 200);
  assert.deepEqual(t.sample(800, 600, 700).events, []);
  t.up({ pointerId: 3, clientX: 600, clientY: 300 }, 800);
  t.down({ pointerId: 4, clientX: 600, clientY: 300 }, 800, 600, 900);
  t.move({ pointerId: 4, clientX: 650, clientY: 310 }, 800, 600);
  const look = t.sample(800, 600, 910);
  assert.equal(look.dx, 50);
  assert.equal(look.dy, 10);
});

test('the play surface takes gestures all the way to the bottom corners; editor controls take taps', () => {
  for (const [w, h] of [[390, 844], [844, 390], [1024, 768]]) {
    const t = new TouchPad(), buttons = touchButtons('play', w, h);
    assert.deepEqual(buttons, []);
    t.down({ pointerId: 1, clientX: w - 25, clientY: h - 25 }, w, h, 0, false, buttons);
    t.move({ pointerId: 1, clientX: w - 45, clientY: h - 25 }, w, h);
    assert.equal(t.sample(w, h, 10).dx, -20);
    const typing = touchButtons('typing', w, h * 0.55, { context: [{ name: 'ring' }] });
    assert.deepEqual(typing.map(b => b.name), ['send', 'cancel', 'ring']);
    assert.ok(typing.every(b => b.x >= 0 && b.y + b.h <= h * 0.55));
    const send = typing[0], finger = { pointerId: 2, clientX: send.x + 5, clientY: send.y + 5 };
    assert.equal(t.down(finger, w, h, 20, false, typing).name, 'send');
    assert.equal(t.up(finger, 40).name, 'send');
  }
  assert.deepEqual(touchButtons('chalk', 390, 844).map(b => b.name), ['build', 'cancel', 'draw']);
});

test('the join code is a corner badge that a phone can read', () => {
  const cells = joinCode('https://lake-tea-journal-bureau.trycloudflare.com/?room=studio');
  const span = cells.length + 8;
  // A 1x buffer (a narrow browser window, a phone): three screen pixels per
  // module and at least two per HUD unit, never past half the short side.
  for (const [w, h] of [[700, 757], [960, 540], [390, 844], [480, 270]]) {
    const { qrSize, module, unit } = hudLayout(w, h, cells, 1);
    assert.ok(qrSize <= Math.min(w, h) / 2, `${w}x${h}: ${qrSize}`);
    assert.ok(module >= Math.min(3, Math.floor(Math.min(w, h) / 2 / span)), `${w}x${h}: module ${module}`);
    assert.ok(unit >= 2, `${w}x${h}: unit ${unit}`);
  }
  // A 2x buffer behind a 1280x720 window: the same screen sizes, half the units.
  const two = hudLayout(640, 360, cells, 2);
  assert.ok(two.module * 2 >= 3 && two.qrSize * 2 >= 3 * span, `2x: ${two.qrSize}`);
  assert.ok(two.unit * 2 >= 2 && two.unit <= 2, `2x: unit ${two.unit}`);
  // The sizes stay a corner badge, not a panel, on a big 1x buffer.
  assert.ok(hudLayout(1024, 600, cells, 1).qrSize <= 600 * 0.3);
});
