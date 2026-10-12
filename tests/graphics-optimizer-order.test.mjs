import assert from 'node:assert/strict';
import test from 'node:test';
import { PixelBufferOptimizer } from '../system/public/aesthetic.computer/lib/graphics-optimizer.mjs';

test('same-size frames use their own pixels, never paint out of order, and close bitmaps', async () => {
  const original = globalThis.createImageBitmap;
  const pending = [], drawn = [], closed = [];
  globalThis.createImageBitmap = image => new Promise((resolve, reject) => pending.push({ image, resolve, reject }));
  try {
    const optimizer = new PixelBufferOptimizer();
    optimizer.asyncRenderingSupported = true;
    const ctx = { drawImage: bitmap => drawn.push(bitmap.id), putImageData: image => drawn.push(image.id) };
    const first = optimizer.renderImageDataAsync({ id: 'old', width: 20, height: 20 }, ctx);
    const second = optimizer.renderImageDataAsync({ id: 'new', width: 20, height: 20 }, ctx);
    assert.deepEqual(pending.map(p => p.image.id), ['old', 'new']);
    pending[1].resolve({ id: 'new', close: () => closed.push('new') });
    assert.equal(await second, true);
    pending[0].resolve({ id: 'old', close: () => closed.push('old') });
    assert.equal(await first, false);
    assert.deepEqual(drawn, ['new']);
    assert.deepEqual(closed, ['new', 'old']);

    const stale = optimizer.renderImageDataAsync({ id: 'stale' }, ctx);
    const latest = optimizer.renderImageDataAsync({ id: 'latest' }, ctx);
    pending[2].reject(new Error('stale decode'));
    assert.equal(await stale, false);
    assert.deepEqual(drawn, ['new']);
    pending[3].resolve({ id: 'latest', close() {} });
    await latest;
    assert.deepEqual(drawn, ['new', 'latest']);

    const otherCanvas = { drawImage: bitmap => drawn.push(bitmap.id) };
    const a = optimizer.renderImageDataAsync({ id: 'canvas-a' }, ctx);
    const b = optimizer.renderImageDataAsync({ id: 'canvas-b' }, otherCanvas);
    pending[4].resolve({ id: 'canvas-a', close() {} });
    pending[5].resolve({ id: 'canvas-b', close() {} });
    assert.deepEqual(await Promise.all([a, b]), [true, true]);
  } finally {
    if (original) globalThis.createImageBitmap = original;
    else delete globalThis.createImageBitmap;
  }
});
