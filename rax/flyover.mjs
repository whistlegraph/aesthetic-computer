// 🛫 The idle flyover
// A drone nobody is flying takes itself on a tour and comes round again. The
// tour is not written down: it is worked out from what there is to see when
// the drone is let go, and worked out afresh each lap, so it finds what was
// built or who came in since. Four movements, each handing on to the next:
//   1. the climb     a rising turn round the tower, foot to above the roof,
//                    eye on the tower all the way
//   2. the pull-out  back and up until the whole small planet is in view,
//                    and a slow arc round it at that distance
//   3. the sights    down among whatever is worth a look (`sights`: a pad to
//                    play, a screen, a build, the ball, whoever is about),
//                    nearest first from wherever it is, a part-turn round
//                    each with the eye held on it; one that moves (a person,
//                    the ball) is followed as it goes
//   4. the return    back to the foot of the climb
// With nothing told to it, it looks over the yard itself: the door, the walk
// up to it, and a turn through the trees.
// It is a list of places to pass through, each with a thing to look at and a
// speed to arrive at, joined by a smooth curve (Catmull-Rom), so the path has
// no corners and can be read off for any moment; a leg that would cut
// through the tower goes round its corner instead. Metres, in the plaza's
// own (x east, z north, y up; the tower's door faces south).
const TOWER = { x: 0, z: 4 };
const TOWER_HALF = 11; // the tower's footprint and a drone's width of air round it
// What of the tower is solid to a drone: the glass hall on the ground (the north half of its footprint, up to the first floor), and the floors over it up to the roof. Under the first floor, south of the hall's front, is open air between the legs.
export const solidAt = (x, y, z, roof) =>
  (Math.abs(x) < 8.6 && z > 0.9 && z < 8.6 && y < 12.5) || (Math.abs(x) < 8.8 && Math.abs(z) < 8.8 && y > 11.5 && y < roof + 1);

// A sight: { x, y, z } its middle, r how big it is (metres), and if it moves, at() → { x, y, z } | null where it is now.
// roof: how high the tower stands. planet: the planet's radius. Back comes
// { seconds, keys, at(t) }, where at() gives { x, y, z } and what to look at
// { lx, ly, lz }, for t in seconds (any t: it goes round).
export function flyover(roof = 33, { sights = [], planet = 140 } = {}) {
  const keys = []; // [x, y, z, look ('ahead' | [x, y, z] | () => [x, y, z]), metres a second on the way here]
  const mid = roof * 0.55, towerLook = [TOWER.x, mid, TOWER.z];
  // A leg from the last place to (x, z) that would pass through the tower below its roof goes by the corner on its nearer side.
  const reach = (x, y, z, look, speed) => {
    const last = keys[keys.length - 1];
    if (last) {
      for (const u of [0.2, 0.35, 0.5, 0.65, 0.8]) {
        const mx = last[0] + (x - last[0]) * u, mz = last[2] + (z - last[2]) * u, my = last[1] + (y - last[1]) * u;
        if (solidAt(mx, my, mz, roof)) {
          // Out along the side of the leg its middle already lies on, well clear of the corner.
          const dx = x - last[0], dz = z - last[2], len = Math.hypot(dx, dz) || 1, side = (-dz * mx + dx * mz) >= 0 ? 1 : -1;
          keys.push([mx + ((-dz * side) / len) * (TOWER_HALF + 9), (last[1] + y) / 2, mz + ((dx * side) / len) * (TOWER_HALF + 9), look, speed]);
          break;
        }
      }
    }
    keys.push([x, y, z, look, speed]);
  };
  // 1. The climb: three quarters of a turn, 24 m out widening to 30.
  for (let k = 0; k <= 6; k++) {
    const s = k / 6, a = -Math.PI / 2 + s * Math.PI * 1.5, r = 24 + 6 * s;
    keys.push([TOWER.x + Math.cos(a) * r, 5 + (roof + 9 - 5) * s * s * (3 - 2 * s), TOWER.z + Math.sin(a) * r, [TOWER.x, mid * (0.5 + 0.5 * s), TOWER.z], 7]);
  }
  // 2. The pull-out: away and up until the whole ball is under the eye, then a third of the way round it.
  const far = Math.max(90, planet * 3.4), high = Math.max(108, planet * 2.2), ball = [0, -planet * 0.7, 0];
  const out = Math.atan2(keys[keys.length - 1][2], keys[keys.length - 1][0]);
  keys.push([Math.cos(out) * far * 0.45, roof + 30, Math.sin(out) * far * 0.45, towerLook, 16]);
  for (let k = 0; k <= 3; k++) {
    const a = out + (k / 3) * (Math.PI * 0.7);
    keys.push([Math.cos(a) * far, high - 10 * k, Math.sin(a) * far, ball, 22]);
  }
  // 3. The sights: nearest first from wherever the eye is, a part-turn round each.
  const todo = (sights.length ? sights : [
    { x: 0, y: 3, z: -5, r: 3 }, { x: 0, y: 2, z: -30, r: 6 }, { x: 34, y: 6, z: -14, r: 9 }, { x: -34, y: 6, z: -14, r: 9 }, { x: 0, y: 4, z: -52, r: 8 },
  ]).filter((s) => s && [s.x, s.y, s.z].every(Number.isFinite)).slice(0, 16);
  let from = keys[keys.length - 1];
  while (todo.length) {
    let best = 0;
    for (let i = 1; i < todo.length; i++)
      if (Math.hypot(todo[i].x - from[0], todo[i].z - from[2]) < Math.hypot(todo[best].x - from[0], todo[best].z - from[2])) best = i;
    const [s] = todo.splice(best, 1);
    const r = Math.max(3.5, Math.min(28, (s.r || 1) * 2.2 + 3)), y = Math.max(1.8, s.y + Math.max(0.6, (s.r || 1) * 0.5));
    const still = [s.x, s.y, s.z], look = s.at ? () => { const p = s.at(); return p ? [p.x, p.y, p.z] : still; } : still;
    // Come in on the side it is reached from, and turn about it the way that leads on from there.
    const a0 = Math.atan2(from[2] - s.z, from[0] - s.x), turn = todo.length % 2 ? 1 : -1;
    for (const [k, da] of [0, 0.9, 1.8].entries()) {
      const a = a0 + da * turn, x = s.x + Math.cos(a) * r, z = s.z + Math.sin(a) * r;
      // (Never inside the tower: a sight that stands in or against it is looked at only from its open side.)
      if (solidAt(x, y, z, roof)) continue;
      reach(x, y + 0.5 * k, z, look, k ? Math.max(2.5, r * 0.7) : 11);
    }
    // (One wholly inside, a screen on the hall's wall: from before the hall's front, through the glass.)
    if (keys[keys.length - 1] === from) reach(s.x * 0.8, Math.min(y, 4), -r - 1, look, 11);
    from = keys[keys.length - 1];
  }
  // 4. Back to the foot of the climb.
  reach(TOWER.x + 6, 6, TOWER.z - 30, [TOWER.x, mid * 0.5, TOWER.z], 8);
  // When each is reached: the way there, at the speed asked.
  const n = keys.length, when = [0];
  for (let i = 1; i <= n; i++) {
    const a = keys[i - 1], b = keys[i % n];
    when.push(when[i - 1] + Math.max(1.5, Math.hypot(b[0] - a[0], b[1] - a[1], b[2] - a[2]) / b[4]));
  }
  const seconds = when[n];
  const key = (i) => keys[((i % n) + n) % n];
  // The curve between two places, by the clock: each place has the velocity that carries its neighbours' places into one another in the time between them (so a short leg beside a long one is not flung through), and the leg between is the smooth one with those ends.
  const span = (i) => when[(((i % n) + n) % n) + 1] - when[((i % n) + n) % n];
  const velocity = (i, c) => (key(i + 1)[c] - key(i - 1)[c]) / (span(i - 1) + span(i));
  const place = (t) => {
    const s = ((t % seconds) + seconds) % seconds;
    let i = 0;
    while (i < n - 1 && when[i + 1] <= s) i++;
    const T = when[i + 1] - when[i], u = (s - when[i]) / T, u2 = u * u, u3 = u2 * u;
    const at = (c) => (2 * u3 - 3 * u2 + 1) * key(i)[c] + (u3 - 2 * u2 + u) * T * velocity(i, c) + (-2 * u3 + 3 * u2) * key(i + 1)[c] + (u3 - u2) * T * velocity(i + 1, c);
    return { i, u, x: at(0), y: Math.max(1.6, at(1)), z: at(2) };
  };
  const looked = (k, from, t) => {
    if (k[3] === 'ahead') { const on = place(t + 1.2); return [on.x, on.y - 0.6, on.z]; }
    return typeof k[3] === 'function' ? k[3]() : k[3];
  };
  return {
    seconds,
    keys: n,
    at(t) {
      const p = place(t), a = looked(key(p.i), p, t), b = looked(key(p.i + 1), p, t), e = p.u * p.u * (3 - 2 * p.u);
      return { x: p.x, y: p.y, z: p.z, lx: a[0] + (b[0] - a[0]) * e, ly: a[1] + (b[1] - a[1]) * e, lz: a[2] + (b[2] - a[2]) * e };
    },
  };
}
