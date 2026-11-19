// 🌬️ The air: the wind reported, as it is felt at a place.
import test from 'node:test';
import assert from 'node:assert/strict';
import { airAt, heading, sun } from '../air.mjs';

const west = { wind: 6, gust: 10, from: 270, cloud: 0 }; // a westerly: it blows to the east
const mean = (w, x, y, z, opts) => { const m = { x: 0, y: 0, z: 0 }; for (let t = 0; t < 600; t++) { const a = airAt(w, x, y, z, t * 0.37, opts); m.x += a.x / 600; m.y += a.y / 600; m.z += a.z / 600; } return m; };

test('a westerly blows east, and still air is still', () => {
  assert.ok(Math.abs(heading(west).x - 1) < 1e-9 && Math.abs(heading(west).z) < 1e-9);
  const calm = airAt({ wind: 0, gust: 0, from: 0, cloud: 1 }, 40, 5, 40, 12);
  assert.ok(Math.hypot(calm.x, calm.z) < 0.6, 'only the small swirls are left');
  const far = mean(west, 80, 10, 80);
  assert.ok(far.x > 4 && Math.abs(far.z) < 1, `east at ten metres (${far.x.toFixed(1)}, ${far.z.toFixed(1)})`);
});

test('it is weaker at the ground than high up', () => {
  assert.ok(mean(west, 80, 0.5, 80).x < mean(west, 80, 3, 80).x && mean(west, 80, 3, 80).x < mean(west, 80, 20, 80).x);
});

test('the same place at the same moment is the same air, and it changes', () => {
  assert.deepEqual(airAt(west, 12, 4, -7, 100), airAt(west, 12, 4, -7, 100));
  assert.notDeepEqual(airAt(west, 12, 4, -7, 100), airAt(west, 12, 4, -7, 101));
});

test('the wind climbs the tower\'s windward face and is slack behind it', () => {
  const before = mean(west, -8, 6, 4), behind = mean(west, 10, 6, 4), open = mean(west, 0, 6, 60);
  assert.ok(before.y > 0.5, `rising ahead of it (${before.y.toFixed(2)})`);
  assert.ok(behind.x < open.x * 0.7, `slack in its lee (${behind.x.toFixed(1)} of ${open.x.toFixed(1)})`);
});

test('by day under a clear sky some air rises and some sinks; by night none does', () => {
  const still = { wind: 0, gust: 0, from: 0, cloud: 0 };
  let top = -9, low = 9;
  for (let x = 0; x < 200; x += 5) { const a = mean(still, x + 60, 15, 90, { day: 1 }).y; top = Math.max(top, a); low = Math.min(low, a); }
  assert.ok(top > 0.4 && low < -0.05, `${top.toFixed(2)} up, ${low.toFixed(2)} down`);
  assert.ok(Math.abs(mean(still, 100, 15, 90, { day: 0 }).y) < 0.2);
  assert.equal(sun(0), 0);
  assert.ok(Math.abs(sun(0.5) - 1) < 1e-9);
});

test('at the clock\'s real size the air still changes smoothly from one frame to the next', () => {
  const t = 1795120000, gusty = { wind: 3, gust: 9, from: 161, cloud: 0.5 };
  for (const [x, y, z] of [[2, 3, 6], [-30, 40, 12], [5, 800, 5]]) {
    const a = airAt(gusty, x, y, z, t, { day: 1 }), b = airAt(gusty, x + 0.05, y + 0.02, z, t + 1 / 60, { day: 1 });
    assert.ok(Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z) < 0.15, `a sixtieth of a second on, at ${x},${y},${z}: ${Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z).toFixed(2)} m/s different`);
  }
});
