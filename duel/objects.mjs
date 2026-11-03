import {
  OBJECT_FORMAT,
  sourceGeometry,
  sourceFrame,
  parseSource,
} from './object-language.mjs';
// The shared compiler admits a bounded object dialect, never generated JavaScript.
export const MAX_PARTS = 32;
export const MAX_OBJECTS = 16;
export const MAX_VERSIONS = 32;
const finite = (v, lo, hi, fallback) =>
  Number.isFinite(v) ? Math.min(hi, Math.max(lo, v)) : fallback;

// Motion is clamped into range rather than refused.
function fitMotion(m) {
  if (!m || typeof m !== 'object') return null;
  const type = m.type === 'fly' ? 'fly' : m.type === 'walk' ? 'walk' : null;
  if (!type) throw Error('Invalid motion'); // a kind of motion, not a size
  const out = {
    type,
    axis: m.axis === 'z' ? 'z' : 'x',
    distance: finite(m.distance, 0, 2, 1),
    period: finite(m.period, 4, 20, 8),
  };
  if (type === 'fly') {
    out.height = finite(m.height, 0.5, 3, 1.5);
    out.flapHz = finite(m.flapHz, 1, 6, 3);
  }
  return out;
}

// Ground the shape, and when it is too big for its volume (motion margins
// included) recentre and scale it uniformly until it fits.
export function fitBoxes(boxes, motion = null) {
  const lo = (k, s) => Math.min(...boxes.map((b) => b[k] - Math.abs(b[s]) / 2));
  const hi = (k, s) => Math.max(...boxes.map((b) => b[k] + Math.abs(b[s]) / 2));
  const x0 = lo('x', 'w'), x1 = hi('x', 'w');
  const y0 = lo('y', 'h'), y1 = hi('y', 'h');
  const z0 = lo('z', 'd'), z1 = hi('z', 'd');
  const margin = { x: 0, z: 0 };
  if (motion) {
    margin[motion.axis] = motion.distance + 0.2;
    if (motion.type === 'fly')
      margin[motion.axis === 'x' ? 'z' : 'x'] = motion.distance / 2 + 0.2;
  }
  const inside =
    y0 >= -0.001 &&
    y1 <= 6 &&
    Math.max(-x0, x1) + margin.x <= 6 &&
    Math.max(-z0, z1) + margin.z <= 6 &&
    boxes.every((b) => [b.w, b.h, b.d].every((n) => Math.abs(n) >= 0.005));
  if (inside) return boxes;
  const cx = Math.max(-x0, x1) + margin.x > 6 ? (x0 + x1) / 2 : 0;
  const cz = Math.max(-z0, z1) + margin.z > 6 ? (z0 + z1) / 2 : 0;
  const scale = Math.min(
    1,
    (6 - margin.x) / Math.max(0.001, Math.max(x1 - cx, cx - x0)),
    (6 - margin.z) / Math.max(0.001, Math.max(z1 - cz, cz - z0)),
    5.999 / Math.max(0.001, y1 - Math.min(0, y0)),
  ) * 0.999;
  const ground = Math.min(0, y0);
  return boxes.map((b) => ({
    ...b,
    x: (b.x - cx) * scale,
    y: (b.y - ground) * scale,
    z: (b.z - cz) * scale,
    w: Math.max(0.005, Math.abs(b.w) * scale),
    h: Math.max(0.005, Math.abs(b.h) * scale),
    d: Math.max(0.005, Math.abs(b.d) * scale),
  }));
}

export function geometry(raw) {
  if (raw?.format === OBJECT_FORMAT) return sourceGeometry(raw);
  if (
    !raw ||
    typeof raw.name !== 'string' ||
    !Array.isArray(raw.boxes) ||
    raw.boxes.length < 1 ||
    raw.boxes.length > MAX_PARTS
  )
    throw Error('Expected a name and 1–32 boxes');
  const name = raw.name
    .replace(/[^a-zA-Z0-9 -]/g, '')
    .trim()
    .slice(0, 32);
  if (!name) throw Error('Object needs a name');
  for (const b of raw.boxes)
    if (
      !b ||
      !['x', 'y', 'z', 'w', 'h', 'd'].every((k) => Number.isFinite(b[k]))
    )
      throw Error('Invalid box');
  const motion = raw.motion != null ? fitMotion(raw.motion) : null;
  // Never refuse a shape for its size: scale it into the 12 × 6 × 12 volume.
  const boxes = fitBoxes(raw.boxes, motion).map((b) => {
    const out = Object.fromEntries(
      ['x', 'y', 'z', 'w', 'h', 'd'].map((k) => [
        k,
        Math.round(b[k] * 1000) / 1000,
      ]),
    );
    if (
      [
        'left-leg',
        'right-leg',
        'left-arm',
        'right-arm',
        'left-wing',
        'right-wing',
      ].includes(b.part)
    )
      out.part = b.part;
    out.color =
      Number.isInteger(b.color) && b.color >= 0 && b.color <= 0xffffff
        ? b.color
        : 0xc0c0c0;
    return out;
  });
  const result = { name, boxes };
  if (motion) {
    const m = motion;
    // Travel is shortened to the room the fitted shape leaves, never refused.
    const room = (axis, extra = 0) =>
      Math.min(
        ...boxes.map(
          (b) => 6 - 0.2 - extra - Math.abs(b[axis]) - b[axis === 'x' ? 'w' : 'd'] / 2,
        ),
      );
    const other = m.axis === 'x' ? 'z' : 'x';
    m.distance = Math.max(0, Math.min(m.distance, room(m.axis)));
    if (m.type === 'fly') m.distance = Math.max(0, Math.min(m.distance, 2 * room(other)));
    result.motion = {
      type: m.type,
      axis: m.axis,
      distance: Math.floor(m.distance * 1000) / 1000,
      period: m.period,
    };
    if (m.type === 'fly') {
      const wings = boxes.filter((b) => b.part?.endsWith('wing'));
      result.motion.height = Math.min(
        3,
        Math.max(m.height, ...wings.map((b) => b.w + 0.25 - b.y)),
      );
      result.motion.flapHz = m.flapHz;
    }
  }
  return result;
}
const collisionCache = new WeakMap();
export function poseObject(object, time = 0) {
  if (object.geometry.format === OBJECT_FORMAT)
    return {
      ...object,
      geometry: {
        ...object.geometry,
        boxes: sourceFrame(object.geometry, time, false).boxes,
      },
    };
  const motion = object.geometry.motion;
  if (!motion) return object;
  const pose = { ...object, geometry: { ...object.geometry } };
  pose[motion.axis] +=
    Math.sin((time * Math.PI * 2) / motion.period) * motion.distance;
  if (motion.type === 'fly') {
    const other = motion.axis === 'x' ? 'z' : 'x';
    pose[other] +=
      (Math.cos((time * Math.PI * 2) / motion.period) * motion.distance) / 2;
    pose.y =
      motion.height + Math.sin((time * Math.PI * 4) / motion.period) * 0.2;
  }
  pose.geometry.boxes = object.geometry.boxes.map((b) => {
    if (!b.part) return b;
    if (motion.type === 'fly') {
      if (!b.part.endsWith('wing')) return b;
      const side = b.part.startsWith('left') ? -1 : 1;
      const roll = side * Math.sin(time * Math.PI * 2 * motion.flapHz) * 0.9;
      const hinge = b.x - (side * b.w) / 2;
      return {
        ...b,
        x: hinge + ((side * b.w) / 2) * Math.cos(roll),
        y: b.y + ((side * b.w) / 2) * Math.sin(roll),
        roll,
      };
    }
    const swing = Math.sin(
      time * 7 + (b.part.startsWith('right') ? Math.PI : 0),
    );
    const arm = b.part.endsWith('arm');
    return {
      ...b,
      [motion.axis]: b[motion.axis] + swing * (arm ? -0.16 : 0.16),
      y: b.y + (arm ? 0 : Math.max(0, swing) * 0.14),
    };
  });
  return pose;
}
export function objectBoxes(objects = [], time = 0) {
  const sampleTime = objects.some(
    (o) => o.geometry.motion || o.geometry.animated,
  )
    ? time
    : 0;
  const cached = collisionCache.get(objects);
  if (cached?.time === sampleTime) return cached.boxes;
  const boxes = objects
    .map((o) => poseObject(o, time))
    .flatMap((o) =>
      o.geometry.boxes.map((b) => {
        const c = Math.abs(Math.cos(b.roll || 0)),
          s = Math.abs(Math.sin(b.roll || 0));
        const w = b.w * c + b.h * s,
          h = b.h * c + b.w * s;
        return {
          type: 'box',
          objectId: o.id,
          xMin: o.x + b.x - w / 2,
          xMax: o.x + b.x + w / 2,
          yMin: (o.y || 0) + b.y - h / 2,
          yMax: (o.y || 0) + b.y + h / 2,
          zMin: o.z + b.z - b.d / 2,
          zMax: o.z + b.z + b.d / 2,
        };
      }),
    );
  collisionCache.set(objects, { time: sampleTime, boxes });
  return boxes;
}
export function parseGeometry(text) {
  const clean = String(text)
    .trim()
    .replace(/^```(?:json)?\s*/, '')
    .replace(/\s*```$/, '');
  return clean.startsWith('{')
    ? geometry(JSON.parse(clean))
    : parseSource(clean);
}
export const OBJECT_INSTRUCTIONS = `You are Aesel, making objects for dool's minimal solid-color 3D world.
Return ONLY a short Oskiewar object KidLisp program. Start with ; Object name. No markdown or JSON.
Use explicit balanced parentheses. Units metres, x sideways, y up, z forward; ground y=0.
Shapes: (slab xMin yMin zMin xMax yMax zMax), (orb x y z radius [sides]),
(capsule x1 y1 z1 x2 y2 z2 width [sides]), (tri 9 coordinates), (quad 12 coordinates).
Slabs use opposite corners, NOT centers/dimensions. Thin wings may be .02 thick. Sides 3..8.
(ink white|black|gray|red|orange|yellow|green|cyan|blue|purple|pink|brown) or (ink r g b) with channels 0..255.
Transforms: (move x y z forms...), (rotate x|y|z radians forms...), (scale x y z forms...).
Bindings: (def name constant), (let name expression). (repeat N i forms...) repeats 1..16 times.
Math: + - * / sin cos abs min max, pi, tau. time is shared seconds. Use sin/cos for bounded movement.
Shape coordinates/dimensions and colors must be constant. Animate transforms around constant shapes.
Limits: 32 shapes, 512 triangles total, 8000 characters, nesting 12. Prefer 6 sides on orbs.
Keep ALL motion inside x/z -6..6 and y 0..10. Bound checking is conservative for rotations:
leave clearance at least the wing's reach below its hinge, and avoid rotating whole grounded bodies.
A flying butterfly MUST move and flap hinged wings with rotate, e.g.
(ink pink)
(move (sin time) 2 0
  (rotate z (* .6 (sin (* time 12))) (slab 0 0 -.3 1 .02 .3))
  (rotate z (* -.6 (sin (* time 12))) (slab -1 0 -.3 0 .02 .3))
  (ink black) (orb 0 0 0 .12 6))
A walking creature MUST move and have alternating animated feet. Use move with sin time.
For staircases, risers <=.3 and treads >=.5, ascending +z. Example:
(ink gray) (repeat 6 i (move 0 0 (* i .5) (scale 1 (+ 1 i) 1 (slab -1 0 0 1 .25 .5))))
Sound: (tone hz [sine|square|triangle|sawtooth] [volume 0..1]) puts an oscillator at that point (at most 4).
hz may follow time, e.g. (tone (+ 220 (* 30 (sin time))) triangle .3). Give singing, humming, buzzing or musical things a tone.
Bells, chimes, gongs, drums, horns, whistles, music boxes and speakers ALWAYS get a tone. Low or deep: 60..200 hz; high or bright: 600..2000 hz;
ringing or chiming: sine or triangle; buzzing: sawtooth or square; pulsing: hz follows (sin time).
Use a few meaningful shapes. No lights, textures, tools, JavaScript, ownership, versions or commentary.
When revising, preserve the object's purpose and unrequested details. Previous versions may use legacy boxes;
convert them to this dialect. Keep source concise.`;
