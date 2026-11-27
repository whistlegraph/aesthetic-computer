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

// 🚀 Travel: out of one planet's air, across space, into another's.
import { body, launch, setDown, reached, spaceOf, aim, aimOf, LEAVE, ARRIVE, SPACE_HALF, planetOf, SPACE } from '../orbits.mjs';

test('a bike leaves over the top of its planet, still looking at what it looked at', () => {
  const now = 1_764_000_000_000, phase = 0.4;
  for (const id of ['home', 'pinch', 'brume']) {
    const b = body(id), c = spaceOf(b, now), s = launch(id, now, phase, LEAVE, 30, 20, [0, 7, 0]);
    assert.ok(Math.abs(Math.hypot(s.x - c[0], s.y - c[1], s.z - c[2]) - (b.radius + LEAVE)) < 1e-6, 'LEAVE metres over its ground');
    assert.equal(reached(s, now)?.body.id === id, false, 'not taken straight back in');
    // Climbing straight up is going straight away from the planet.
    const away = [s.x - c[0], s.y - c[1], s.z - c[2]].map((n) => n / (b.radius + LEAVE));
    assert.ok(Math.abs(away[0] * s.vx + away[1] * s.vy + away[2] * s.vz - 7) < 1e-6, 'its climb carries it outward');
    // Each other planet is, in space, where the sky showed it to a bike that high.
    for (const seen of skyOf(id, now, phase, LEAVE)) {
      const { yaw, pitch } = aimOf([seen.dir.x, seen.dir.y, seen.dir.z]);
      const out = launch(id, now, phase, LEAVE, yaw, pitch), look = aim(out.yaw, out.pitch);
      const t = spaceOf(seen.id, now), d = [t[0] - out.x, t[1] - out.y, t[2] - out.z], l = Math.hypot(...d);
      assert.ok(look[0] * d[0] / l + look[1] * d[1] / l + look[2] * d[2] / l > 0.9999, `${seen.id} from ${id} is where the sky had it`);
      assert.ok(Math.abs(l - seen.distance) < 1e-6, 'and as far off');
    }
  }
});

test('flown at a planet, a bike arrives over it looking down', () => {
  const now = 1_764_000_000_000, phase = 0.7, b = body('brume'), c = spaceOf(b, now);
  for (const from of [[1, 0, 0], [0, 0, -1], [0.6, 0.3, 0.74], [0, -1, 0]]) {
    const l = Math.hypot(...from), d = from.map((n) => n / l), r = b.radius + ARRIVE - 5;
    const at = { x: c[0] + d[0] * r, y: c[1] + d[1] * r, z: c[2] + d[2] * r, ...aimOf(d.map((n) => -n)), vx: -d[0] * 14, vy: -d[1] * 14, vz: -d[2] * 14 };
    assert.equal(reached(at, now).body.id, 'brume');
    const set = setDown('brume', at, now, phase);
    assert.ok(Math.abs(set.y - (ARRIVE - 5)) < 1e-6, 'as high over it as it was');
    assert.ok(set.pitch < -89, `looking down (${set.pitch.toFixed(1)})`);
    assert.ok(Math.abs(set.vy + 14) < 1e-6 && Math.hypot(set.vx, set.vz) < 1e-6, 'and going down');
  }
});

test('space holds every planet, and nowhere far from one is an arrival', () => {
  for (let k = 0; k < 40; k++) {
    const now = 1_764_000_000_000 + k * 47_000;
    for (const b of BODIES) assert.ok(Math.max(...spaceOf(b, now).map(Math.abs)) + b.radius + LEAVE < SPACE_HALF);
    assert.equal(reached({ x: 0, y: 300, z: 0 }, now), null);
  }
  assert.equal(planetOf(SPACE), null);
  assert.equal(planetOf('pinch').id, 'pinch');
  assert.equal(planetOf('lobby', true).id, 'home');
  assert.equal(planetOf('home'), null, 'a room that only shares home\'s name is nowhere');
});
