import assert from 'node:assert/strict';
import test from 'node:test';
import { renderSurface, nextResolutionScale } from '../render-quality.mjs';

test('Retina uses display pixels rather than a 1080p ceiling', () => {
  assert.equal(renderSurface(1440, 900, 1728, 1080, 2).pixelHeight, 1800);
  assert.equal(renderSurface(1440, 900, 1728, 1080, 1).pixelHeight, 900);
});
test('rounding logical width cannot shrink the physical canvas by a pixel', () => {
  for (const [width, height, dpr] of [[1200, 700, 2], [1365, 911, 2], [390, 844, 3]]) {
    const logicalWidth = Math.round(1080 * width / height);
    const surface = renderSurface(width, height, logicalWidth, 1080, dpr);
    assert.equal(surface.pixelWidth, width * dpr);
    assert.equal(surface.pixelHeight, height * dpr);
    assert.ok(Math.abs(surface.scaleX * logicalWidth - surface.pixelWidth) < 1e-8);
  }
});
test('a refresh cap with cheap frames keeps full resolution', () => {
  assert.equal(nextResolutionScale(1, { fps: 30, renderCpuMs: 3 }), 1);
  assert.ok(nextResolutionScale(.62, { fps: 30, renderCpuMs: 3 }) > .62);
});
test('expensive frames can reduce resolution, within bounds', () => {
  assert.equal(nextResolutionScale(1, { fps: 40, renderCpuMs: 18 }), .88);
  assert.equal(nextResolutionScale(.62, { fps: 40, renderCpuMs: 18 }), .62);
  assert.equal(nextResolutionScale(1, { fps: 60, renderCpuMs: 3 }), 1);
});
