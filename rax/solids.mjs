// 🧱 Solids that cross. Two boxes that share volume are a construction
// error: where their faces fall in one plane the renderer cannot say which
// is in front, and the surface flickers between the two colours. Boxes that
// only touch (a face against a face) are fine.
//
// A box here is { x, y, z, w, h, d } about its middle, as model()'s box()
// takes it, with an optional `color` and `name`.
const EPS = 1e-6;
const span = (b) => [
  [b.x - b.w / 2, b.x + b.w / 2],
  [b.y - b.h / 2, b.y + b.h / 2],
  [b.z - b.d / 2, b.z + b.d / 2],
];

// Every pair that shares volume: [{ a, b, overlap: [x, y, z], flickers }].
// `flickers` is true when the pair also has two faces in one plane, facing
// the same way, in different colours: the case that shows on screen.
export function crossings(boxes) {
  const spans = boxes.map(span), found = [];
  for (let i = 0; i < boxes.length; i++)
    for (let j = i + 1; j < boxes.length; j++) {
      const A = spans[i], B = spans[j];
      const overlap = [0, 1, 2].map((k) => Math.min(A[k][1], B[k][1]) - Math.max(A[k][0], B[k][0]));
      if (!overlap.every((v) => v > EPS)) continue;
      const flush = [0, 1, 2].some((k) => Math.abs(A[k][0] - B[k][0]) <= EPS || Math.abs(A[k][1] - B[k][1]) <= EPS);
      found.push({ a: boxes[i], b: boxes[j], overlap, flickers: flush && boxes[i].color !== boxes[j].color });
    }
  return found;
}

// One line a person can act on.
export function describeCrossing({ a, b, overlap, flickers }) {
  const say = (s) => `${s.name || 'box'}(${[s.x, s.y, s.z, s.w, s.h, s.d].map((n) => +n.toFixed(3)).join(', ')})`;
  return `${say(a)} crosses ${say(b)} by ${overlap.map((n) => +n.toFixed(3)).join(' × ')}${flickers ? ' and flickers' : ''}`;
}

// A box turned half round about the y axis and set down at (x, y, z), as
// the view draws the car's screen.
export const placed = (b, x, y, z, turned = false) => ({ ...b, x: (turned ? -b.x : b.x) + x, y: b.y + y, z: (turned ? -b.z : b.z) + z });
