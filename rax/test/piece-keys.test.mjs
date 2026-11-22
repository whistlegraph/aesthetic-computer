// ⌨️ A panel's AC piece hears a keyboard as AC's own page would say it.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createPanel, keyEvent } from '../panel.mjs';

test('a key is named and matched as AC names and matches it', () => {
  const a = keyEvent('down', { key: 'A', code: 'KeyA', shiftKey: true });
  assert.equal(a.name, 'keyboard:down:a');
  assert.equal(a.key, 'A');
  assert.equal(a.device, 'keyboard');
  assert.equal(a.shift, true);
  assert.ok(a.is('keyboard:down') && a.is('keyboard:down:a') && a.is('keyboard'));
  assert.ok(!a.is('keyboard:up') && !a.is('keyboard:down:b') && !a.is('touch'));
  assert.equal(keyEvent('up', { key: ' ', code: 'Space' }).name, 'keyboard:up:space');
  assert.equal(keyEvent('down', { key: 'ArrowUp', code: 'ArrowUp' }).name, 'keyboard:down:arrowup');
});

test('only a piece that hears a keyboard is given keys, with the whole api', () => {
  const heard = [];
  const deaf = createPanel({ id: 'deaf', piece: { act: ($) => heard.push($.event.name) } });
  assert.equal(deaf.takesKeys, false);
  deaf.key('down', { key: 'c', code: 'KeyC' });
  assert.deepEqual(heard, []);
  const panel = createPanel({ id: 'keys', piece: { keys: true, act: ($) => heard.push([$.event.name, typeof $.ink, $.event.repeat]) } });
  assert.equal(panel.takesKeys, true);
  panel.key('down', { key: 'c', code: 'KeyC', repeat: true });
  panel.key('up', { key: 'c', code: 'KeyC' });
  assert.deepEqual(heard, [['keyboard:down:c', 'function', true], ['keyboard:up:c', 'function', false]]);
});
