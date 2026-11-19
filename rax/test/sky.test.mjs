import test from 'node:test';
import assert from 'node:assert/strict';
import { DAY_MS, STEPS, phase, stepOf, skyAt, skyColor, stars } from '../sky.mjs';

test('one clock for everyone: the phase comes from the time alone and wraps each day', () => {
  assert.equal(phase(0), 0);
  assert.equal(phase(DAY_MS / 2), 0.5);
  assert.equal(phase(DAY_MS * 7 + DAY_MS / 4), 0.25);
  assert.equal(stepOf(0.5), STEPS / 2);
  assert.equal(stepOf(0.99999), STEPS - 1);
});

test('the sun rises in the east, stands in the south at noon over the tower\'s door, and sets in the west', () => {
  const near = (a, b) => Math.abs(a - b) < 1e-9;
  const rise = skyAt(0.25).sun, noon = skyAt(0.5).sun, set = skyAt(0.75).sun, night = skyAt(0).sun;
  assert.ok(rise.x > 0.99 && near(rise.y, 0));
  assert.ok(noon.y > 0.8 && noon.z < 0 && near(noon.x, 0));
  assert.ok(set.x < -0.99 && Math.abs(set.y) < 1e-9);
  assert.ok(night.y < -0.8);
  for (let t = 0; t < 1; t += 0.01) {
    const { sun, moon } = skyAt(t);
    assert.ok(near(Math.hypot(sun.x, sun.y, sun.z), 1));
    assert.ok(near(sun.y + moon.y, 0), 'the moon is opposite the sun');
  }
});

test('day wears the day room and a blue sky; night the night room, dark, with stars', () => {
  const noon = skyAt(0.5), midnight = skyAt(0);
  assert.equal(noon.day, true);
  assert.equal(midnight.day, false);
  assert.equal(noon.stars, 0);
  assert.equal(midnight.stars, 1);
  assert.equal(noon.horizon, 0xcfe3f5, 'the day horizon is the day room\'s own sky colour');
  const light = (c) => ((c >> 16) & 255) + ((c >> 8) & 255) + (c & 255);
  assert.ok(light(noon.zenith) > 3 * light(midnight.zenith));
  assert.ok(light(skyColor(noon, 0)) > light(skyColor(noon, 1.4)), 'paler at the horizon than overhead');
});

test('the sky never jumps: neighbouring pictures differ by little', () => {
  const far = (a, b) => Math.max(...[16, 8, 0].map((s) => Math.abs(((a >> s) & 255) - ((b >> s) & 255))));
  for (let n = 0; n < STEPS; n++) {
    const a = skyAt(n / STEPS), b = skyAt(((n + 1) % STEPS) / STEPS);
    for (const k of ['zenith', 'mid', 'horizon']) assert.ok(far(a[k], b[k]) <= 12, `${k} at step ${n}`);
  }
});

test('the stars are the same for everyone and stay above the horizon', () => {
  assert.deepEqual(stars(), stars());
  for (const s of stars()) {
    assert.ok(s.y > 0.05 && Math.abs(Math.hypot(s.x, s.y, s.z) - 1) < 1e-9);
  }
});
