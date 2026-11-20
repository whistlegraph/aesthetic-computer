// 🛫 The idle flyover: a path with no corners that comes round.
import test from 'node:test';
import assert from 'node:assert/strict';
import { flyover } from '../flyover.mjs';

test('the tour comes round, never jumps, never goes under the ground, and is never asked to fly absurdly fast', () => {
  const tour = flyover(33);
  assert.ok(tour.seconds > 120 && tour.seconds < 300, `a lap is ${tour.seconds.toFixed(0)} s`);
  let last = tour.at(0), fastest = 0, lowest = Infinity, highest = 0;
  for (let t = 1 / 30; t <= tour.seconds + 1; t += 1 / 30) {
    const p = tour.at(t), v = Math.hypot(p.x - last.x, p.y - last.y, p.z - last.z) * 30;
    fastest = Math.max(fastest, v); lowest = Math.min(lowest, p.y); highest = Math.max(highest, p.y);
    assert.ok([p.x, p.y, p.z, p.lx, p.ly, p.lz].every(Number.isFinite));
    last = p;
  }
  assert.ok(fastest < 45, `fastest ${fastest.toFixed(1)} m/s`);
  assert.ok(lowest >= 1.6 && highest > 100, `from ${lowest.toFixed(1)} m to ${highest.toFixed(0)} m up`);
  const a = tour.at(0), b = tour.at(tour.seconds);
  assert.ok(Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z) < 1e-6, 'the end is the beginning');
});

test('it looks at something that is not itself', () => {
  const tour = flyover(33);
  for (let t = 0; t < tour.seconds; t += 0.5) {
    const p = tour.at(t);
    assert.ok(Math.hypot(p.lx - p.x, p.ly - p.y, p.lz - p.z) > 1, `at ${t} s`);
  }
});
