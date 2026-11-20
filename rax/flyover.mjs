// 🛫 The idle flyover
// A drone nobody is flying takes itself on a tour of the plaza and comes
// round again, about three minutes a lap. Five movements, each handing on
// to the next where it ends:
//   1. the climb     a rising turn and a quarter round the tower, foot to
//                    above the roof, eye on the tower all the way
//   2. the pull-out  back and up until the whole small planet is in view
//   3. the far turn  a slow arc round the planet at that distance
//   4. the approach  down to head height at the far end of the promenade,
//                    then low over the pool and up the walk to the door
//   5. the skim      a figure of eight through the yard at treetop height,
//                    looking where it goes
// It is a list of places to pass through, each with a thing to look at and a
// speed to arrive at, joined by a smooth curve (Catmull-Rom), so the path has
// no corners and can be read off for any moment. Metres, in the plaza's own
// (x east, z north, y up; the tower's door faces south, the promenade runs
// south from it).
const TOWER = { x: 0, z: 4 };

// roof: how high the tower stands. Back comes { seconds, at(t) }, where at()
// gives { x, y, z } and what to look at { lx, ly, lz }, for t in seconds
// (any t: it goes round).
export function flyover(roof = 33) {
  const keys = []; // [x, y, z, look ('tower' | 'ahead' | [x, y, z]), metres a second on the way here]
  const mid = roof * 0.55;
  // 1. The climb: one and a quarter turns, 24 m out widening to 32.
  for (let k = 0; k <= 10; k++) {
    const s = k / 10, a = -Math.PI / 2 + s * Math.PI * 2.5, r = 24 + 8 * s;
    keys.push([TOWER.x + Math.cos(a) * r, 5 + (roof + 9 - 5) * s * s * (3 - 2 * s), TOWER.z + Math.sin(a) * r, [TOWER.x, mid * (0.5 + 0.5 * s), TOWER.z], 7]);
  }
  // 2. The pull-out: away to the south-west and up.
  keys.push([-70, roof + 40, -60, [TOWER.x, mid, TOWER.z], 16]);
  keys.push([-170, 150, -190, [0, -40, 0], 24]);
  // 3. The far turn: a third of the way round, the whole ball under the eye.
  for (let k = 1; k <= 4; k++) {
    const a = Math.atan2(-190, -170) + (k / 4) * (Math.PI * 0.7), r = 255;
    keys.push([Math.cos(a) * r, 150 - 12 * k, Math.sin(a) * r, [0, -40, 0], 22]);
  }
  // 4. The approach: down to the promenade's far end, then up it to the door.
  keys.push([40, 45, -110, [0, 2, -30], 20]);
  keys.push([4, 9, -66, [0, 2, -20], 13]);
  keys.push([0, 3.2, -46, [0, 5, 0], 8]);
  keys.push([0, 2.6, -30, [0, 6, 2], 6]);
  keys.push([0, 3.4, -15, [0, 8, 4], 5]);
  // 5. The skim: a figure of eight across the yard, east loop then west.
  for (let k = 1; k <= 12; k++) {
    const a = (k / 12) * Math.PI * 2;
    keys.push([Math.sin(a) * 46, 9 + 3 * Math.sin(a * 2), -8 + Math.sin(a * 2) * 26, 'ahead', 10]);
  }
  // Back to the foot of the climb.
  keys.push([TOWER.x + 6, 6, TOWER.z - 30, [TOWER.x, mid * 0.5, TOWER.z], 8]);
  // When each is reached: the way there, at the speed asked.
  const n = keys.length, when = [0];
  for (let i = 1; i <= n; i++) {
    const a = keys[i - 1], b = keys[i % n];
    when.push(when[i - 1] + Math.max(1.5, Math.hypot(b[0] - a[0], b[1] - a[1], b[2] - a[2]) / b[4]));
  }
  const seconds = when[n];
  const key = (i) => keys[((i % n) + n) % n];
  const curve = (p0, p1, p2, p3, u) => 0.5 * (2 * p1 + (p2 - p0) * u + (2 * p0 - 5 * p1 + 4 * p2 - p3) * u * u + (3 * p1 - p0 - 3 * p2 + p3) * u * u * u);
  const place = (t) => {
    const s = ((t % seconds) + seconds) % seconds;
    let i = 0;
    while (i < n - 1 && when[i + 1] <= s) i++;
    const u = (s - when[i]) / (when[i + 1] - when[i]);
    const a = key(i - 1), b = key(i), c = key(i + 1), d = key(i + 2);
    return { i, u, x: curve(a[0], b[0], c[0], d[0], u), y: Math.max(1.6, curve(a[1], b[1], c[1], d[1], u)), z: curve(a[2], b[2], c[2], d[2], u) };
  };
  const looked = (k, from, t) => {
    if (k[3] === 'ahead') { const on = place(t + 1.2); return [on.x, on.y - 0.6, on.z]; }
    return k[3];
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
