import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {runInNewContext} from 'node:vm';

const swift = await readFile(new URL('../Sources/WhistlegraphAccount.swift', import.meta.url), 'utf8');
const script = swift.match(/static let script = """\n([\s\S]*?)\n    """/)[1];
function preview(size, stored = '4') {
  const messages = [], storage = new Map([['ac-density', stored]]);
  const window = {top: {}, __walkiewarePixelSize: size, postMessage: m => messages.push(m), addEventListener() {}};
  runInNewContext(script, {window, location: {origin: 'https://aesthetic.computer'},
    localStorage: {setItem: (key, value) => storage.set(key, value)}, setInterval() {}});
  return {window, storage, messages};
}

test('native default wins over stale preview density before boot', () => {
  const p = preview(undefined);
  assert.equal(p.storage.get('ac-density'), '2');
  assert.equal(p.window.acPACK_DENSITY, 2);
});

test('all pixel sizes resize the running preview without reloading its source', () => {
  const p = preview(3);
  p.window.acSEND = () => assert.fail('A density change must not reload the piece');
  for (const size of [1, 4, 2, 3]) {
    p.window.walkiewareSetPixelSize(size);
    assert.equal(p.storage.get('ac-density'), String(size));
    assert.equal(p.window.acAutoDensityOverride, true);
    assert.equal(p.messages.at(-1).type, 'ac-density-change');
    assert.equal(p.messages.at(-1).density, size);
  }
  const before = p.messages.length;
  for (const invalid of [0, 5, 1.5, '2', NaN, null]) p.window.walkiewareSetPixelSize(invalid);
  assert.equal(p.messages.length, before);
});
