import assert from 'node:assert/strict';
import test from 'node:test';
import { renderDensity, nextResolutionScale } from '../render-quality.mjs';

test('Retina uses display pixels rather than a 1080p ceiling', () => {
  assert.equal(renderDensity(900, 1080, 2) * 1080, 1800);
  assert.equal(renderDensity(900, 1080, 1) * 1080, 900);
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
