import test from 'node:test';
import assert from 'node:assert/strict';
import { BODIES, HOME, AU, YEAR_MS, periodOf, positionOf, skyOf, sunFrom } from '../orbits.mjs';
import { skyAt } from '../sky.mjs';

test('home goes round once a year; Kepler sets the rest', () => {
  const home = BODIES.find((b) => b.id === HOME);
  assert.equal(periodOf(home), YEAR_MS);
  const a = positionOf(HOME, 0), b = positionOf(HOME, YEAR_MS);
  for (let i = 0; i < 3; i++) assert.ok(Math.abs(a[i] - b[i]) < 1e-6);
  for (const body of BODIES) assert.ok(Math.abs(periodOf(body) / YEAR_MS - (body.distance / AU) ** 1.5) < 1e-9);
});

test('every planet keeps its circle', () => {
  for (const body of BODIES)
    for (const t of [0, 1e6, 1.79e12]) {
      const [x, y, z] = positionOf(body, t);
      assert.ok(Math.abs(Math.hypot(x, y, z) - body.distance) / body.distance < 0.01, body.id);
    }
});

test('the sun from home is the day\'s own sun', () => {
  for (const phase of [0.1, 0.25, 0.5, 0.8])
    for (const now of [0, 5e11, 1.79e12]) {
      const s = sunFrom(HOME, now, phase), day = skyAt(phase).sun;
      assert.ok(Math.hypot(s.x - day.x, s.y - day.y, s.z - day.z) < 1e-9);
    }
});

test('the sky over home shows the others, near first, at their true size', () => {
  const now = 1.79e12, seen = skyOf(HOME, now, 0.5);
  assert.deepEqual(seen.map((p) => p.id).sort(), BODIES.filter((b) => b.id !== HOME).map((b) => b.id).sort());
  for (let i = 1; i < seen.length; i++) assert.ok(seen[i - 1].distance <= seen[i].distance);
  for (const p of seen) {
    assert.ok(Math.abs(Math.hypot(p.dir.x, p.dir.y, p.dir.z) - 1) < 1e-9);
    assert.ok(Math.abs(Math.sin(p.size) * p.distance - p.radius) < 1e-6);
  }
});

test('the planets move across the sky in minutes, not years', () => {
  const a = skyOf(HOME, 0, 0.5)[0], b = skyOf(HOME, 5 * 60 * 1000, 0.5).find((p) => p.id === a.id);
  const turned = Math.acos(a.dir.x * b.dir.x + a.dir.y * b.dir.y + a.dir.z * b.dir.z);
  assert.ok(turned > 0.05, `turned ${turned}`);
});
