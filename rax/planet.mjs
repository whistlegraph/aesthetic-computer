// 🌍 The small planet's own sums: how the flat land is wrapped round a ball
// under the eye, what the ball hides, and how depth is shared out between
// what is near and what is far. Pure, so the eye that draws (project.mjs),
// the view and the drone agree, and so they can be tested.
// The planet's radius, metres: 880 m round, fifty-five blocks. (It was 900.
// At 140 every far shape had to be halved many times to bend so sharply, the
// far side cost as much as the near, and layers on the land tore; now what
// is over the horizon is not worked on, and it costs about what 900 did.)
export const PLANET = 140;

// A place in the flat world, measured from the eye (dx, dz across the
// ground; y its height over the ground), where the ball of radius R puts it:
// as far round the ball as it is off along the ground, upright on the ball.
// `eye` is the eye's own height over the ground (under 0: inside the ball).
// Back comes [x, y, z] from the eye, y up.
export function bend(dx, y, dz, R, eye) {
  const d = Math.hypot(dx, dz), turn = Math.min(Math.PI, d / R), out = R + y;
  const along = d > 1e-6 ? (out * Math.sin(turn)) / d : out / R;
  return [dx * along, out * Math.cos(turn) - R - eye, dz * along];
}

// How far off along the ground a thing `top` metres tall can be and still
// show over the ball to an eye `eye` metres up. (A little over the truth,
// never under: what it lets through is only drawn for nothing.) From inside
// the ball nothing is hidden by it.
export function sight(R, eye, top = 0) {
  if (!(eye > 0)) return Infinity;
  return R * Math.acos(R / (R + eye)) + (top > 0 ? Math.sqrt(2 * top * R) : 0);
}

// Is a ball of radius r, its middle d off along the ground and y over it,
// wholly out of sight: over the horizon, or past the far side?
export function hidden(R, eye, d, y, r) {
  return d - r > Math.min(Math.PI * R, sight(R, eye, y + r));
}

// 📏 Depth. The card keeps 24 bits of it and they go by 1 / distance, so
// with the near plane at 5 cm almost none are left a hundred metres off and
// two things a few millimetres apart there trade places in stripes. A
// drone's eye high over the land has nothing near it but its own drone and
// the air's motes. So when the nearest thing of any size (past SPLIT) is
// `clear` metres off, depth is cut in two: a thin share for what is within
// SPLIT of the eye, and all the rest from a quarter of `clear` outward. A
// shape between the two is drawn at the join, in front of all that is far.
// Each part still goes by 1 / distance, so it is right across a flat face.
export const NEAR = 0.05, FAR = 150000, SPLIT = 3, SHARE = 0.04;
export function depthRange(clear = 0) {
  const open = Math.min(clear / 4, FAR / 8);
  if (!(open > SPLIT)) return { split: Infinity, open: 0, a: FAR / (FAR - NEAR), b: (FAR * NEAR) / (FAR - NEAR), c: 0, d: 0 };
  const k = SHARE / (1 / NEAR - 1 / SPLIT), m = (1 - SHARE) / (1 / open - 1 / FAR);
  return { split: SPLIT, open, a: k / NEAR, b: k, c: SHARE + m / open, d: m };
}
export const depthAt = (range, z) => (z <= range.split ? range.a - range.b / z : z >= range.open ? range.c - range.d / z : SHARE);

// The widest angle from straight ahead that is still in the picture, under
// the lens: `reach` is how far out the picture's corner lies (in the flat
// picture's own units), `lens` how much of the lens is on.
export function widest(reach, lens = 0) {
  const out = (t) => t * (1 + (2 / (1 + Math.sqrt(1 + t * t)) - 1) * lens);
  if (out(1e4) <= reach) return Math.PI / 2;
  let lo = 0, hi = 1e4;
  for (let n = 0; n < 40; n++) { const mid = (lo + hi) / 2; if (out(mid) < reach) lo = mid; else hi = mid; }
  return Math.atan(hi);
}

// A model's own ball: the middle of its corners and how far the farthest is (and `low`, its lowest corner).
const balls = new WeakMap();
export function ballOf(vertices) {
  let ball = balls.get(vertices);
  if (ball && ball.n === vertices.length) return ball;
  let x0 = Infinity, y0 = Infinity, z0 = Infinity, x1 = -Infinity, y1 = -Infinity, z1 = -Infinity;
  for (let i = 0; i < vertices.length; i += 6) {
    const x = vertices[i], y = vertices[i + 1], z = vertices[i + 2];
    if (x < x0) x0 = x; if (x > x1) x1 = x;
    if (y < y0) y0 = y; if (y > y1) y1 = y;
    if (z < z0) z0 = z; if (z > z1) z1 = z;
  }
  ball = { n: vertices.length, x: (x0 + x1) / 2, y: (y0 + y1) / 2, z: (z0 + z1) / 2, r: Math.hypot(x1 - x0, y1 - y0, z1 - z0) / 2, low: y0 };
  balls.set(vertices, ball);
  return ball;
}

// The land beyond the blocks, as rings of squares about a hole `hole` metres
// across, each ring three times the last, out past `reach`: big squares far
// off, where they are small in the picture. `cut` cuts each square into
// that many a side (the dome's chequer). Each tile is its box (x0, z0, x1,
// z1), `points` round its edge in order, and `odd` for a chequer.
// (A tile's edge on a ring's inner side meets three short edges of the ring
// inside it. Its points there are those three edges' ends, so both sides are
// halved at the same places and no crack opens between rings.)
export function landTiles(hole, reach, cut = 1) {
  const tiles = [];
  for (let h = hole / 2, n = 0; h < reach; h *= 3, n++) {
    const s = (2 * h) / cut, at = (i) => -3 * h + i * s, inside = (i) => i >= cut && i < 2 * cut;
    for (let i = 0; i < 3 * cut; i++)
      for (let j = 0; j < 3 * cut; j++) {
        if (inside(i) && inside(j)) continue; // the hole: the ring inside this one, or the blocks
        const x0 = at(i), x1 = at(i + 1), z0 = at(j), z1 = at(j + 1), points = [];
        // One edge, from (ax, az) toward (bx, bz): cut in three where it faces the ring inside.
        const edge = (ax, az, bx, bz, faces) => {
          points.push([ax, az]);
          if (faces && n) points.push([ax + (bx - ax) / 3, az + (bz - az) / 3], [ax + ((bx - ax) * 2) / 3, az + ((bz - az) * 2) / 3]);
        };
        edge(x0, z0, x0, z1, inside(j) && i === 2 * cut);
        edge(x0, z1, x1, z1, inside(i) && j === cut - 1);
        edge(x1, z1, x1, z0, inside(j) && i === cut - 1);
        edge(x1, z0, x0, z0, inside(i) && j === 2 * cut);
        tiles.push({ x0, z0, x1, z1, points, odd: (i + j) & 1 });
      }
  }
  return tiles;
}
