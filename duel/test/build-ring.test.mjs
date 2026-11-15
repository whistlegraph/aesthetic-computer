import test from 'node:test';
import assert from 'node:assert/strict';
import { homedir } from 'node:os';
import { Radial, THINGS, CATEGORIES, CHANGES, radialSlots } from '../radial.mjs';
import { devEntries, redact } from '../dev-log.mjs';
import { TouchPad, touchButtons, touchZones, CONTEXT, ICONS } from '../touch.mjs';
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
    { kind: 'say', text: 'Shrinking the QR now.', detail: [] },
    { kind: 'do', text: 'EDIT hud.mjs', detail: [] },
    { kind: 'do', text: 'RUN List files', detail: ['$ ls'] },
  ]);
  const codex = JSON.stringify({
    type: 'response_item',
    payload: { type: 'custom_tool_call', input: '*** Begin Patch\n*** Update File: /Users/jas/a/duel/view.mjs\n' },
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

test('touch: the $ menu, jump, chat and build sit bottom right; what is in view adds caps above; a button takes its touch', () => {
  for (const [w, h] of [[390, 844], [844, 390], [1024, 768]]) {
    const row = touchButtons('play', w, h);
    assert.deepEqual(row.map((b) => b.name), ['build', 'chat', 'jump', 'menu']);
    assert.deepEqual(row.map((b) => b.icon), ['cube', 'bubble', 'jump', 'dollar']);
    for (const b of row) {
      assert.ok(b.x > w * 0.45 && b.x + b.w <= w && b.y + b.h <= h && b.w >= 44 && b.h >= 44, `${w}x${h} ${b.name}`);
      assert.ok(ICONS[b.icon].every((line) => line.length === 11), b.icon);
    }
    const caps = touchButtons('play', w, h, { context: [CONTEXT.use('throw'), CONTEXT.drop] }).slice(4);
    assert.deepEqual(caps.map((b) => b.name), ['throw', 'drop']);
    for (const c of caps) assert.ok(c.y + c.h <= row[0].y && c.x + c.w <= w && c.y > 0, `${w}x${h} ${c.name}`);
    // The zones stay on the lower half, clear of the icons and of each other.
    const [move, look] = touchZones(w, h);
    assert.ok(move.y >= h / 2 - 1 && move.y + move.h <= row[0].y && move.x + move.w <= look.x && look.x + look.w <= w);
    // With the keyboard up, send and cancel ride above it.
    for (const b of touchButtons('typing', w, h * 0.55)) assert.ok(b.y + b.h <= h * 0.55 && b.x >= 0);
  }
  assert.deepEqual(touchButtons(null, 390, 844, { context: [CONTEXT.paint] }), []);
  // Typing a build, the ring joins send and cancel; chalk has its own row.
  assert.deepEqual(touchButtons('typing', 390, 400, { context: [CONTEXT.ring] }).map((b) => b.name), ['send', 'cancel', 'ring']);
  assert.deepEqual(touchButtons('chalk', 390, 844).map((b) => b.name), ['build', 'cancel', 'draw']);
  for (const b of touchButtons('typing', 390, 400, { context: [CONTEXT.ring] })) assert.ok(b.x >= 0 && b.x + b.w <= 390);
  const t = new TouchPad(), buttons = touchButtons('play', 390, 844);
  const hit = t.down({ pointerId: 1, clientX: buttons[0].x + 5, clientY: buttons[0].y + 5 }, 390, 844, 0, false, buttons);
  assert.equal(hit.name, 'build');
  assert.equal(t.look, null);
  assert.equal(t.up({ pointerId: 1, clientX: 0, clientY: 0 }, 50).name, 'build');
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
