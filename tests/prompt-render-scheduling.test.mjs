import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

const bios = readFileSync(new URL('../system/public/aesthetic.computer/bios.mjs', import.meta.url), 'utf8');
const begin = bios.indexOf('  let frameAlreadyRequested = false;');
const source = bios.slice(begin, bios.indexOf('  // 🛑 Frame pump stall watchdog', begin));

function harness(extra = {}) {
  const frames = [];
  const context = vm.createContext({
    currentPieceHasKeyboard: true, needsReframe: false,
    document: { hidden: false }, window: {}, screen: { pixels: [] },
    keyboard: {}, gamepad: {}, performance: { now: () => 42 },
    requestFrame(...args) {
      frames.push(args);
      vm.runInContext('promptRenderPending = false; frameAlreadyRequested = true;', context);
    }, ...extra,
  });
  vm.runInContext(source, context);
  return { frames, run: code => vm.runInContext(code, context) };
}

test('prompt edits render immediately without advancing simulation', () => {
  const h = harness();
  h.run('requestPromptRender()');
  assert.deepEqual(h.frames, [[true, 0, 42]]);
});

test('edits coalesce behind an in-flight frame and resume once it returns', () => {
  const h = harness();
  h.run('frameAlreadyRequested = true; requestPromptRender(); requestPromptRender();');
  assert.equal(h.frames.length, 0);
  h.run('frameAlreadyRequested = false; flushPromptRender(); flushPromptRender();');
  assert.deepEqual(h.frames, [[true, 0, 42]]);
});

test('background, paused, resizing, and non-text pieces do not force frames', () => {
  for (const extra of [{ document: { hidden: true } }, { window: { __acLoopPaused: true } },
    { window: { acInputLatencyProbe: { animationFrameOnly: true } } },
    { needsReframe: true }, { currentPieceHasKeyboard: false }]) {
    const h = harness(extra);
    h.run('requestPromptRender()');
    assert.equal(h.frames.length, 0);
  }
});
