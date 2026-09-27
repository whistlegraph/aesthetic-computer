// The web's interpreter for the oskiewar frame program.
//
// The game hands the shell one Float32Array per paint — a program of
// scene-level ops — and this runs it against the shell's own drawing
// functions. It is one reader of the format; the game's immediate path and
// the console's native interpreter are the others. Keep the op table equal
// to the one at the head of the frame-program section in oskiewar.js:
//
//   1 VIEW     clipX clipY clipW clipH        NaN clipX = the whole stage
//   2 FACE     x1 y1 z1 x2 y2 z2 x3 y3 z3 r g b
//   3 DISC     x y depth radius r g b
//   4 CAPSULE  x1 y1 x2 y2 depth width r g b
//   5 TEXT     font x y size r g b string     font 0 comic 1 ywft 2 system 3 block
//   6 BOX      x y w h r g b a
//   7 LINE     x1 y1 x2 y2 width r g b
//   8 WIPE     r g b
//
// Discs and capsules arrive whole and are fanned here; a face inside a second
// view (the P1 inset) is scissored to that view's rectangle here. The host's
// own coordinate limit is honoured the way the console's is: a face that
// asks for a coordinate past ±32200 is dropped, not drawn.

export const FRAME_VIEW = 1, FRAME_FACE = 2, FRAME_DISC = 3, FRAME_CAPSULE = 4,
  FRAME_TEXT = 5, FRAME_BOX = 6, FRAME_LINE = 7, FRAME_WIPE = 8;

// Ring tables by radius bucket, as the game fans them: a small disc is a
// hexagon, a large one thirty-two sides.
const discRings = [6, 8, 12, 16, 24, 32].map((sides) => {
  const ring = [];
  for (let side = 0; side < sides; side++) {
    const a = side * Math.PI * 2 / sides;
    ring.push(Math.cos(a), Math.sin(a));
  }
  return ring;
});
const discRingFor = (radius) => discRings[
  radius < 6 ? 0 : radius < 13 ? 1 : radius < 26 ? 2
    : radius < 52 ? 3 : radius < 110 ? 4 : 5];
const capsuleArcs = Object.fromEntries([3, 4, 6, 8, 12].map((n) => [n,
  Array.from({ length: n + 1 }, (_, i) =>
    [Math.cos(i * Math.PI / n), Math.sin(i * Math.PI / n)]).flat()]));

const safe = (value) => value > -32200 && value < 32200;

export function createFrameVm(host) {
  const { triangle3d, triangle, box, line, wipe, write, systemWrite,
    comicWrite, ywftWrite } = host;
  const fonts = [
    comicWrite || ywftWrite || systemWrite,
    ywftWrite || systemWrite,
    systemWrite,
    write || systemWrite,
  ];
  // A face with its own depth per vertex, or, on a host without a depth
  // buffer, the flat triangle the game drew before it had one.
  const face = triangle3d
    ? triangle3d
    : (x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b) =>
      triangle(x1, y1, x2, y2, x3, y3, r, g, b);
  let clip = null;
  const scissorPoly = new Float64Array(28), scissorNext = new Float64Array(28);

  function flatFace(x1, y1, x2, y2, x3, y3, depth, r, g, b) {
    if (!(safe(x1) && safe(y1) && safe(x2) && safe(y2) && safe(x3) && safe(y3))) return;
    if (!clip) { face(x1, y1, depth, x2, y2, depth, x3, y3, depth, r, g, b); return; }
    const left = clip.x, right = clip.x + clip.w, top = clip.y, bottom = clip.y + clip.h;
    const inside = (x, y) => x >= left && x <= right && y >= top && y <= bottom;
    if (inside(x1, y1) && inside(x2, y2) && inside(x3, y3)) {
      face(x1, y1, depth, x2, y2, depth, x3, y3, depth, r, g, b);
      return;
    }
    if ((x1 < left && x2 < left && x3 < left) || (x1 > right && x2 > right && x3 > right) ||
        (y1 < top && y2 < top && y3 < top) || (y1 > bottom && y2 > bottom && y3 > bottom)) return;
    let poly = scissorPoly, next = scissorNext, n = 3;
    poly[0] = x1; poly[1] = y1; poly[2] = x2; poly[3] = y2; poly[4] = x3; poly[5] = y3;
    for (let edge = 0; edge < 4; edge++) {
      const axis = edge < 2 ? 0 : 1;
      const bound = edge === 0 ? left : edge === 1 ? right : edge === 2 ? top : bottom;
      const sign = edge === 0 || edge === 2 ? 1 : -1;
      let m = 0;
      for (let i = 0; i < n; i++) {
        const j = (i + 1) % n;
        const ax = poly[i * 2], ay = poly[i * 2 + 1], bx = poly[j * 2], by = poly[j * 2 + 1];
        const da = ((axis ? ay : ax) - bound) * sign, db = ((axis ? by : bx) - bound) * sign;
        if (da >= 0) { next[m * 2] = ax; next[m * 2 + 1] = ay; m++; }
        if ((da >= 0) !== (db >= 0)) {
          const t = da / (da - db);
          next[m * 2] = ax + (bx - ax) * t; next[m * 2 + 1] = ay + (by - ay) * t; m++;
        }
      }
      const swap = poly; poly = next; next = swap; n = m;
      if (n < 3) return;
    }
    for (let i = 1; i + 1 < n; i++)
      face(poly[0], poly[1], depth, poly[i * 2], poly[i * 2 + 1], depth,
        poly[i * 2 + 2], poly[i * 2 + 3], depth, r, g, b);
  }

  // A FACE op carries a depth per vertex; scissoring flattens it to the first
  // vertex's depth, which is what the game's own scissor did for the inset.
  function fullFace(x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b) {
    if (!clip) {
      if (safe(x1) && safe(y1) && safe(z1) && safe(x2) && safe(y2) && safe(z2) &&
          safe(x3) && safe(y3) && safe(z3))
        face(x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b);
      return;
    }
    flatFace(x1, y1, x2, y2, x3, y3, z1, r, g, b);
  }

  function disc(x, y, depth, radius, r, g, b) {
    const ring = discRingFor(radius);
    const originX = x + ring[0] * radius, originY = y + ring[1] * radius;
    let lastX = x + ring[2] * radius, lastY = y + ring[3] * radius;
    for (let side = 4; side < ring.length; side += 2) {
      const nextX = x + ring[side] * radius;
      const nextY = y + ring[side + 1] * radius;
      flatFace(originX, originY, lastX, lastY, nextX, nextY, depth, r, g, b);
      lastX = nextX;
      lastY = nextY;
    }
  }

  function capsule(x1, y1, x2, y2, depth, width, r, g, b) {
    const dx = x2 - x1, dy = y2 - y1;
    const length = Math.hypot(dx, dy);
    const radius = width / 2;
    if (length < .001) { disc(x1, y1, depth, radius, r, g, b); return; }
    const nx = -dy / length * radius, ny = dx / length * radius;
    flatFace(x1 + nx, y1 + ny, x1 - nx, y1 - ny, x2 + nx, y2 + ny, depth, r, g, b);
    flatFace(x1 - nx, y1 - ny, x2 - nx, y2 - ny, x2 + nx, y2 + ny, depth, r, g, b);
    const steps = radius < 6 ? 3 : radius < 13 ? 4 : radius < 26 ? 6 : radius < 52 ? 8 : 12;
    const ring = capsuleArcs[steps], ux = dx / length, uy = dy / length;
    for (const end of [-1, 1]) {
      const cx = end < 0 ? x1 : x2, cy = end < 0 ? y1 : y2;
      let ax = cx + nx, ay = cy + ny;
      for (let i = 1; i <= steps; i++) {
        const side = ring[i * 2], along = ring[i * 2 + 1] * end * radius;
        const bx = cx + nx * side + ux * along, by = cy + ny * side + uy * along;
        flatFace(cx, cy, ax, ay, bx, by, depth, r, g, b);
        ax = bx; ay = by;
      }
    }
  }

  const clipRect = { x: 0, y: 0, w: 0, h: 0 };
  function run(p, length, strings) {
    clip = null;
    let at = 0;
    while (at < length) {
      const op = p[at];
      switch (op) {
        case FRAME_VIEW:
          if (Number.isNaN(p[at + 1])) clip = null;
          else {
            clipRect.x = p[at + 1]; clipRect.y = p[at + 2];
            clipRect.w = p[at + 3]; clipRect.h = p[at + 4];
            clip = clipRect;
          }
          at += 5; break;
        case FRAME_FACE:
          fullFace(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6],
            p[at + 7], p[at + 8], p[at + 9], p[at + 10], p[at + 11], p[at + 12]);
          at += 13; break;
        case FRAME_DISC:
          disc(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7]);
          at += 8; break;
        case FRAME_CAPSULE:
          capsule(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6],
            p[at + 7], p[at + 8], p[at + 9]);
          at += 10; break;
        case FRAME_TEXT:
          fonts[p[at + 1]](strings[p[at + 8]], p[at + 2], p[at + 3], p[at + 4],
            p[at + 5], p[at + 6], p[at + 7]);
          at += 9; break;
        case FRAME_BOX:
          if (p[at + 8] >= 255) box(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7]);
          else box(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7], p[at + 8]);
          at += 9; break;
        case FRAME_LINE:
          line(p[at + 1], p[at + 2], p[at + 3], p[at + 4], p[at + 5], p[at + 6], p[at + 7], p[at + 8]);
          at += 9; break;
        case FRAME_WIPE:
          wipe(p[at + 1], p[at + 2], p[at + 3]);
          at += 4; break;
        default:
          // An op this interpreter does not know ends the program rather than
          // walking into its arguments as if they were ops.
          throw new Error(`frame program: unknown op ${op} at ${at}`);
      }
    }
  }

  // Decode a program into plain objects, for tests and recorders.
  function decode(p, length, strings) {
    const ops = [];
    let at = 0;
    while (at < length) {
      const op = p[at];
      const size = op === FRAME_VIEW ? 5 : op === FRAME_FACE ? 13 : op === FRAME_DISC ? 8
        : op === FRAME_CAPSULE ? 10 : op === FRAME_TEXT ? 9 : op === FRAME_BOX ? 9
        : op === FRAME_LINE ? 9 : op === FRAME_WIPE ? 4 : 0;
      if (!size) throw new Error(`frame program: unknown op ${op} at ${at}`);
      const args = Array.from(p.subarray(at + 1, at + size));
      if (op === FRAME_TEXT) args[7] = strings[args[7]];
      ops.push({ op, args });
      at += size;
    }
    return ops;
  }

  return { run, decode };
}

export default createFrameVm;
