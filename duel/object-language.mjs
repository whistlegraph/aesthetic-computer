import { read, compile, objectLight } from './object-compiler.mjs';

export const OBJECT_FORMAT = 'oskiewar-object-v1';
const cache = new Map();
const identity = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1];
const colors = {
  white: [255, 255, 255],
  black: [0, 0, 0],
  gray: [128, 128, 128],
  red: [255, 0, 0],
  pink: [255, 105, 180],
  cyan: [0, 255, 255],
  yellow: [255, 255, 0],
  blue: [40, 90, 255],
  green: [40, 200, 80],
  orange: [255, 140, 0],
  purple: [150, 70, 220],
  brown: [130, 80, 40],
  grey: [128, 128, 128],
};
const fail = (message) => {
  throw Error(message);
};
const add = (a, b) => [a[0] + b[0], a[1] + b[1]];
const neg = (a) => [-a[1], -a[0]];
const mul = (a, b) => {
  if (a.every((x) => x === 0) || b.every((x) => x === 0)) return [0, 0];
  const v = [a[0] * b[0], a[0] * b[1], a[1] * b[0], a[1] * b[1]];
  return [Math.min(...v), Math.max(...v)];
};
// A limb swinging a few degrees does not sweep a whole circle. Include the
// interval endpoints and every extremum, so these remain conservative bounds.
const trig = (op, a) => {
  if (a[0] === a[1]) return [Math[op](a[0]), Math[op](a[0])];
  if (
    !a.every(Number.isFinite) ||
    a[1] - a[0] >= Math.PI * 2 ||
    a.some((v) => Math.abs(v) > 1e8)
  )
    return [-1, 1];
  const shift = op === 'cos' ? Math.PI / 2 : 0,
    lo = a[0] + shift,
    hi = a[1] + shift;
  let min = Math.min(Math.sin(lo), Math.sin(hi)),
    max = Math.max(Math.sin(lo), Math.sin(hi));
  const contains = (phase) =>
    Math.ceil((lo - phase) / (2 * Math.PI)) <=
    Math.floor((hi - phase) / (2 * Math.PI));
  if (contains(Math.PI / 2)) max = 1;
  if (contains((3 * Math.PI) / 2)) min = -1;
  return [min, max];
};
const exact = (r) => Number.isFinite(r[0]) && r[0] === r[1];
const finite = (r) => r.every(Number.isFinite);
// Pattern ops are for sound only: shapes keep to the shared dialect.
const SOUND_OPS = ['floor', 'frac', 'pulse', 'pick'];
function expression(form, env, depth = 0, sound = false) {
  if (depth > 12) fail('Expression nesting exceeds 12');
  if (typeof form === 'number') {
    if (!Number.isFinite(form) || Math.abs(form) > 10000)
      fail('Numbers must be finite and at most 10000');
    return { code: form, range: [form, form] };
  }
  if (typeof form === 'string') {
    if (!Object.hasOwn(env, form)) fail(`Unknown value: ${form}`);
    return env[form];
  }
  if (!Array.isArray(form) || form.length < 2 || form.length > 9)
    fail('Invalid expression');
  const [op, ...rest] = form,
    args = rest.map((f) => expression(f, env, depth + 1, sound)),
    r = args.map((a) => a.range);
  let range;
  if (op === '+') range = r.reduce(add, [0, 0]);
  else if (op === '-')
    range =
      r.length === 1
        ? neg(r[0])
        : r.slice(1).reduce((a, b) => add(a, neg(b)), r[0]);
  else if (op === '*') range = r.reduce(mul, [1, 1]);
  else if (op === '/' && r.length === 2 && exact(r[1]) && r[1][0] !== 0)
    range = mul(r[0], [1 / r[1][0], 1 / r[1][0]]);
  else if (['sin', 'cos'].includes(op) && r.length === 1)
    range = trig(op, r[0]);
  else if (op === 'abs' && r.length === 1)
    range = [
      r[0][0] <= 0 && r[0][1] >= 0 ? 0 : Math.min(...r[0].map(Math.abs)),
      Math.max(...r[0].map(Math.abs)),
    ];
  else if (['min', 'max'].includes(op))
    range = [Math[op](...r.map((x) => x[0])), Math[op](...r.map((x) => x[1]))];
  else if (sound && op === 'floor' && r.length === 1)
    range = [Math.floor(r[0][0]), Math.floor(r[0][1])];
  else if (sound && op === 'frac' && r.length === 1) range = [0, 1];
  else if (sound && op === 'pulse' && r.length === 1) range = [0, 1];
  else if (sound && op === 'pick' && r.length >= 2)
    range = [Math.min(...r.slice(1).map((x) => x[0])), Math.max(...r.slice(1).map((x) => x[1]))];
  else if (!sound && SOUND_OPS.includes(op))
    fail(`${op} is for tones only`);
  else fail(`Unsupported expression: ${op}; use + - * / sin cos abs min max`);
  if (range.some(Number.isNaN)) fail('Expression is not finite');
  if (exact(range)) return { code: range[0], range, cost: 1, depth: 0 };
  const cost = 1 + args.reduce((n, a) => n + (a.cost || 1), 0),
    nesting = 1 + Math.max(...args.map((a) => a.depth || 0));
  if (cost > 256 || nesting > 12)
    fail('Expression exceeds its instruction budget');
  return {
    code: [op, ...args.map((a) => a.code)],
    range,
    cost,
    depth: nesting,
  };
}
function transformed(bounds, transforms) {
  let b = bounds.map((a) => a.slice());
  for (const t of transforms.toReversed()) {
    if (t.kind === 'move') b = b.map((a, i) => add(a, t.args[i]));
    if (t.kind === 'scale') b = b.map((a, i) => mul(a, t.args[i]));
    if (t.kind === 'rotate') {
      const u = (t.axis + 1) % 3,
        v = (t.axis + 2) % 3,
        c = trig('cos', t.angle),
        s = trig('sin', t.angle);
      const a = add(mul(b[u], c), neg(mul(b[v], s))),
        d = add(mul(b[u], s), mul(b[v], c));
      b[u] = a;
      b[v] = d;
    }
  }
  return b;
}
const boxOf = (b) => ({
  x: (b[0][0] + b[0][1]) / 2,
  y: (b[1][0] + b[1][1]) / 2,
  z: (b[2][0] + b[2][1]) / 2,
  w: b[0][1] - b[0][0],
  h: b[1][1] - b[1][0],
  d: b[2][1] - b[2][0],
  color: 0xc0c0c0,
});

// A bounded admission pass, not a second evaluator. Accepted forms retain the
// shared dialect's semantics. Loops unroll here; only transforms can animate.
function admit(source, lenient = false) {
  if (typeof source !== 'string' || source.length > 8000)
    fail('Object source must be at most 8000 characters');
  let depth = 0;
  for (const c of source.replace(/;[^\n]*/g, '')) {
    // 12 for Aesel; two more levels are left for the fitting wrapper.
    if (c === '(' && ++depth > 14) fail('Object nesting exceeds 12');
    if (c === ')' && --depth < 0) fail('Unexpected closing parenthesis');
  }
  if (depth) fail('Finish the open parentheses');
  const program = read(source),
    boxes = [];
  let count = 0,
    work = 0,
    animated = false;
  // The syntax tree, kept for drawing it in space: each node has its source
  // text, its parent, and the primitives (box indices) beneath it.
  const nodes = [{ id: 0, parent: -1, text: '', boxes: [] }];
  const tones = [];
  const node = (parent, text) => {
    nodes.push({ id: nodes.length, parent, text, boxes: [] });
    return nodes.length - 1;
  };
  const extent = [
    [Infinity, -Infinity],
    [Infinity, -Infinity],
    [Infinity, -Infinity],
  ];
  const env = {
    time: { code: 'time', range: [-Infinity, Infinity] },
    // Beats at 120 BPM from the Unix epoch: the clock AC's clock.mjs keeps.
    beat: { code: 'beat', range: [-Infinity, Infinity] },
    pi: { code: Math.PI, range: [Math.PI, Math.PI] },
    tau: { code: 2 * Math.PI, range: [2 * Math.PI, 2 * Math.PI] },
  };
  function body(forms, scope, transforms = [], parent = 0) {
    const local = { ...scope },
      out = [];
    for (const f of forms) {
      if (++work > 512) fail('Object exceeds its instruction budget');
      if (!Array.isArray(f) || typeof f[0] !== 'string')
        fail('Expected an object form');
      const [op, ...args] = f;
      if (op === 'let' || op === 'def') {
        if (
          args.length !== 2 ||
          typeof args[0] !== 'string' ||
          !/^[a-z][a-z0-9-]{0,24}$/.test(args[0]) ||
          ['time', 'pi', 'tau', '__proto__', 'constructor'].includes(args[0])
        )
          fail('Invalid binding');
        const e = expression(args[1], local);
        if (op === 'def' && !exact(e.range))
          fail('def must be constant; use let for animation');
        local[args[0]] = e;
        continue;
      }
      if (op === 'repeat') {
        const e = expression(args[0], local);
        if (
          !exact(e.range) ||
          !Number.isInteger(e.range[0]) ||
          e.range[0] < 1 ||
          e.range[0] > 16
        )
          fail('repeat needs a constant count from 1 to 16');
        const named = typeof args[1] === 'string';
        if (
          named &&
          (Object.hasOwn(local, args[1]) ||
            !/^[a-z][a-z0-9-]{0,24}$/.test(args[1]))
        )
          fail('Use a new repeat index name');
        const loop = node(parent, head(f, named ? 3 : 2));
        for (let i = 0; i < e.range[0]; i++)
          out.push(
            ...body(
              args.slice(named ? 2 : 1),
              {
                ...local,
                ...(named ? { [args[1]]: { code: i, range: [i, i] } } : {}),
              },
              transforms,
              loop,
            ),
          );
        continue;
      }
      if (op === 'move' || op === 'scale' || op === 'rotate') {
        const axis = 'xyz'.indexOf(args[0]);
        const n =
          op === 'rotate'
            ? 2
            : op === 'scale' &&
                Array.isArray(args[1]) &&
                [
                  'slab',
                  'orb',
                  'capsule',
                  'tri',
                  'quad',
                  'move',
                  'rotate',
                  'scale',
                  'ink',
                  'let',
                  'def',
                  'repeat',
                ].includes(args[1][0])
              ? 1
              : 3;
        if (op === 'rotate' && (typeof args[0] !== 'string' || axis < 0))
          fail('rotate needs x, y or z');
        const es = args
          .slice(op === 'rotate' ? 1 : 0, n)
          .map((a) => expression(a, local));
        if (es.length !== (op === 'rotate' ? 1 : n)) fail(`Invalid ${op}`);
        const ranges = es.map((e) => e.range);
        if (op !== 'rotate' && ranges.some((r) => !finite(r)))
          fail('Movement must be bounded; use sin or cos of time');
        if (
          op === 'scale' &&
          ranges.some((r) => !exact(r) || r[0] <= 0 || r[0] > 10)
        )
          fail('scale needs positive constants at most 10');
        const t =
          op === 'rotate'
            ? { kind: op, axis, angle: ranges[0] }
            : {
                kind: op,
                args: n === 1 ? [ranges[0], ranges[0], ranges[0]] : ranges,
              };
        const branch = node(parent, head(f, n + 1));
        const nested = body(args.slice(n), local, [...transforms, t], branch);
        if (nested.length)
          out.push([
            op,
            ...(op === 'rotate' ? [args[0]] : []),
            ...es.map((e) => e.code),
            ...nested,
          ]);
        continue;
      }
      if (op === 'ink') {
        const values =
          args.length === 1 && colors[args[0]]
            ? colors[args[0]]
            : args.map((a) => {
                const e = expression(a, local);
                if (!exact(e.range)) fail('Ink must be constant');
                return e.range[0];
              });
        if (values.length !== 3 || values.some((v) => v < 0 || v > 255))
          fail('ink needs RGB values from 0 to 255');
        out.push(['ink', ...values]);
        continue;
      }
      // (tone frequency [wave] [volume]): an oscillator at this point in the
      // object. Frequency may follow time; it is evaluated by the listener.
      if (op === 'tone') {
        if (args.length < 1 || args.length > 3)
          fail('tone needs a frequency, then an optional wave and volume');
        const e = expression(args[0], local, 0, true);
        if (!finite(e.range) || e.range[0] < 20 || e.range[1] > 4000)
          fail('tone frequency must stay within 20..4000 Hz');
        const named = typeof args[1] === 'string';
        if (named && !WAVES.includes(args[1]))
          fail(`tone wave must be one of ${WAVES.join(' ')}`);
        const volumeForm = named ? args[2] : args[1];
        let volume = 0.3,
          level = 0.3;
        if (volumeForm !== undefined) {
          const v = expression(volumeForm, local, 0, true);
          if (!finite(v.range) || v.range[0] < 0 || v.range[1] > 1)
            fail('tone volume must stay within 0..1');
          volume = v.range[1];
          level = v.code;
        }
        if (tones.length >= 4) fail('Use at most 4 tones');
        const placed = transformed([[0, 0], [0, 0], [0, 0]], transforms);
        tones.push({
          freq: e.code,
          wave: named ? args[1] : 'sine',
          volume,
          level,
          at: placed.map(([lo, hi]) => (lo + hi) / 2),
        });
        node(parent, print(f));
        continue;
      }
      const arities = {
        slab: [6],
        orb: [4, 5],
        capsule: [7, 8],
        tri: [9],
        quad: [12],
      };
      if (!arities[op]?.includes(args.length))
        fail(`Unsupported shape or arguments: ${op}`);
      if (++count > 32) fail('Use at most 32 shapes');
      const values = args.map((a) => {
        const e = expression(a, local);
        if (!exact(e.range))
          fail('Keep shapes constant; animate with move or rotate');
        return e.range[0];
      });
      let bounds;
      if (op === 'slab') {
        bounds = [0, 1, 2].map((i) => [values[i], values[i + 3]]);
        if (bounds.some(([lo, hi]) => hi - lo < 0.005))
          fail('slab corners must be ordered with at least .005 thickness');
      } else if (op === 'orb') {
        if (values[3] < 0.005) fail('orb radius must be at least .005');
        values[3] = Math.min(6, values[3]);
        bounds = values.slice(0, 3).map((v) => [v - values[3], v + values[3]]);
      } else if (op === 'capsule') {
        if (values[6] < 0.005) fail('capsule width must be at least .005');
        values[6] = Math.min(6, values[6]);
        bounds = [0, 1, 2].map((i) => [
          Math.min(values[i], values[i + 3]) - values[6] / 2,
          Math.max(values[i], values[i + 3]) + values[6] / 2,
        ]);
      } else
        bounds = [0, 1, 2].map((i) => {
          const v = values.filter((_, j) => j % 3 === i);
          return [Math.min(...v), Math.max(...v)];
        });
      if (
        (op === 'orb' && values.length === 5) ||
        (op === 'capsule' && values.length === 8)
      ) {
        const sides = values.at(-1);
        if (!Number.isInteger(sides) || sides < 3 || sides > 8)
          fail('Use 3 to 8 sides');
      }
      const placed = transformed(bounds, transforms);
      if (placed.some((r) => !finite(r))) fail('Expression is not finite');
      placed.forEach(([lo, hi], i) => {
        extent[i][0] = Math.min(extent[i][0], lo);
        extent[i][1] = Math.max(extent[i][1], hi);
      });
      if (
        !lenient &&
        (placed[0][0] < -6.001 ||
          placed[0][1] > 6.001 ||
          placed[2][0] < -6.001 ||
          placed[2][1] > 6.001 ||
          placed[1][0] < -0.001 ||
          placed[1][1] > 10.001)
      )
        fail(
          'Keep the entire animation inside x/z -6..6 and y 0..10; move rotating wings higher',
        );
      const leaf = node(parent, print(f));
      for (let k = leaf; k > 0; k = nodes[k].parent) nodes[k].boxes.push(boxes.length);
      nodes[0].boxes.push(boxes.length);
      boxes.push(boxOf(placed));
      animated ||= transforms.some((t) =>
        (t.args || [t.angle]).some((r) => !exact(r)),
      );
      // An identity transform that reads time keeps each primitive as its own
      // retained MODEL/SKETCH, also giving collision the same part placements.
      out.push(['move', ['*', 'time', 0], 0, 0, [op, ...values]]);
    }
    return out;
  }
  const plan = body(program, env);
  if (!count) fail('Object needs at least one shape');
  return { plan, boxes, animated, extent, tree: nodes, tones };
}
function boundsOf(points) {
  return [0, 1, 2].map((i) => {
    let lo = Infinity,
      hi = -Infinity;
    for (let j = i; j < points.length; j += 3) {
      lo = Math.min(lo, points[j]);
      hi = Math.max(hi, points[j]);
    }
    return [lo, hi];
  });
}
function meshData(mesh) {
  const triangles = [];
  for (let i = 0; i < mesh.faces.length; i += 10) {
    const f = mesh.faces.slice(i, i + 10);
    for (const ids of [
      [f[0], f[1], f[2]],
      ...(f[3] === f[2] ? [] : [[f[0], f[2], f[3]]]),
    ])
      triangles.push({
        points: ids.flatMap((id) =>
          Array.from(mesh.vertices.slice(id * 3, id * 3 + 3)),
        ),
        color: Array.from(f.slice(4, 7)),
        normal: Array.from(f.slice(7, 10)),
      });
  }
  return { triangles, bounds: boundsOf(mesh.vertices) };
}
function sketchData(sketch) {
  const triangles = [],
    all = [];
  let i = 0;
  while (i < sketch.records.length) {
    const r = sketch.records;
    if (r[i] !== 4) fail('Only slab sketches are supported');
    const color = Array.from(r.slice(i + 6, i + 9)),
      normal = Array.from(r.slice(i + 9, i + 12)),
      n = r[i + 12];
    const p = Array.from(r.slice(i + 13, i + 13 + n * 3));
    all.push(...p);
    for (let k = 1; k < n - 1; k++)
      triangles.push({
        points: [
          ...p.slice(0, 3),
          ...p.slice(k * 3, k * 3 + 3),
          ...p.slice((k + 1) * 3, (k + 2) * 3),
        ],
        color,
        normal,
      });
    i += 13 + n * 3;
  }
  return { triangles, bounds: boundsOf(all) };
}
function compiled(source) {
  if (cache.has(source)) {
    const value = cache.get(source);
    cache.delete(source);
    cache.set(source, value);
    return value;
  }
  const accepted = admit(source),
    run = compile(accepted.plan, 'dool'),
    meshes = run.meshes.map(meshData),
    sketches = run.sketches.map(sketchData);
  let vertices = new Float64Array(512 * 18),
    length = 0,
    boxes = [],
    sampleTime,
    rendered = false,
    render = true;
  const point = (m, at, x, y, z) =>
    [0, 1, 2].map(
      (k) =>
        m[at + k] + x * m[at + 3 + k] + y * m[at + 6 + k] + z * m[at + 9 + k],
    );
  function emit(part, m, at) {
    const corners = [];
    for (let i = 0; i < 8; i++)
      corners.push(
        ...point(
          m,
          at,
          part.bounds[0][i & 1 ? 1 : 0],
          part.bounds[1][i & 2 ? 1 : 0],
          part.bounds[2][i & 4 ? 1 : 0],
        ),
      );
    boxes.push(boxOf(boundsOf(corners)));
    if (!render) return;
    for (const t of part.triangles) {
      if (length + 18 > vertices.length) fail('Object exceeds 512 triangles');
      const normal = [0, 1, 2].map(
          (k) =>
            t.normal[0] * m[at + 3 + k] +
            t.normal[1] * m[at + 6 + k] +
            t.normal[2] * m[at + 9 + k],
        ),
        norm = Math.hypot(...normal);
      const shade = norm
        ? 0.72 +
          0.28 *
            Math.max(
              0,
              normal.reduce((a, n, i) => a + n * objectLight[i], 0) / norm,
            )
        : 1;
      for (let i = 0; i < 9; i += 3) {
        vertices.set(point(m, at, ...t.points.slice(i, i + 3)), length);
        vertices.set(
          t.color.map((c) => (c * shade) / 255),
          length + 3,
        );
        length += 6;
      }
    }
  }
  const out = {
    model: (_radius, id, _mid, _far, m, at) => emit(meshes[id], m, at),
    sketch: (id, m, at) => emit(sketches[id], m, at),
    face: () => fail('Object must use retained parts'),
  };
  const frame = (time = 0, draw = true) => {
    time = accepted.animated ? time : 0;
    if (time !== sampleTime || (draw && !rendered)) {
      length = 0;
      boxes = [];
      render = draw;
      run({ time }, identity, out);
      sampleTime = time;
      rendered = draw;
    }
    return { vertices: vertices.subarray(0, length), boxes };
  };
  frame(0); // Reject excessive triangles before publication.
  const result = {
    ...accepted,
    frame,
    parts: run.parts,
    meshes: meshes.length,
    sketches: sketches.length,
  };
  if (cache.size >= 64) cache.delete(cache.keys().next().value);
  cache.set(source, result);
  return result;
}
const WAVES = ['sine', 'square', 'triangle', 'sawtooth'];

// Evaluate an admitted expression tree (constants folded, lets inlined).
export const BPM = 120;
export const beatAt = (epochMs) => (epochMs / 60000) * BPM;

export function evaluate(code, time, beat = 0) {
  if (typeof code === 'number') return code;
  if (code === 'time') return time;
  if (code === 'beat') return beat;
  if (typeof code === 'string') return 0;
  const [op, ...rest] = code;
  const a = rest.map((c) => evaluate(c, time, beat));
  switch (op) {
    case '+': return a.reduce((x, y) => x + y, 0);
    case '-': return a.length === 1 ? -a[0] : a.slice(1).reduce((x, y) => x - y, a[0]);
    case '*': return a.reduce((x, y) => x * y, 1);
    case '/': return a[0] / a[1];
    case 'sin': return Math.sin(a[0]);
    case 'cos': return Math.cos(a[0]);
    case 'abs': return Math.abs(a[0]);
    case 'min': return Math.min(...a);
    case 'max': return Math.max(...a);
    case 'floor': return Math.floor(a[0]);
    case 'frac': return a[0] - Math.floor(a[0]);
    // A strike that decays over each unit: a bell on the beat.
    case 'pulse': return Math.exp(-6 * (a[0] - Math.floor(a[0])));
    case 'pick': {
      const n = a.length - 1, i = ((Math.floor(a[0]) % n) + n) % n;
      return a[1 + i];
    }
  }
  return 0;
}

// The oscillators an object carries: [{ freq(time), wave, volume, at }].
export function sourceTones(shape) {
  if (shape?.format !== OBJECT_FORMAT) return [];
  return compiled(shape.source).tones.map((t) => ({
    ...t,
    frequency: (time, beat = 0) => evaluate(t.freq, time, beat),
    gain: (time, beat = 0) => Math.max(0, Math.min(1, evaluate(t.level, time, beat))),
  }));
}

// Source text back from the reader's arrays, for labels in the world.
export function print(form) {
  if (Array.isArray(form)) return `(${form.map(print).join(' ')})`;
  return typeof form === 'number' ? String(Math.round(form * 1000) / 1000) : String(form);
}
// A transform or loop shows its own arguments and elides its children.
function head(form, keep) {
  return `(${form.slice(0, keep).map(print).join(' ')} ...)`;
}

// The syntax tree of an admitted object, with each node's local bounds.
export function sourceTree(shape) {
  const p = compiled(shape.source);
  return p.tree.map((n) => ({
    ...n,
    bounds: n.boxes.length
      ? n.boxes.reduce(
          (b, i) => {
            const x = p.boxes[i];
            return {
              xMin: Math.min(b.xMin, x.x - x.w / 2),
              xMax: Math.max(b.xMax, x.x + x.w / 2),
              yMin: Math.min(b.yMin, x.y - x.h / 2),
              yMax: Math.max(b.yMax, x.y + x.h / 2),
              zMin: Math.min(b.zMin, x.z - x.d / 2),
              zMax: Math.max(b.zMax, x.z + x.d / 2),
            };
          },
          { xMin: Infinity, xMax: -Infinity, yMin: Infinity, yMax: -Infinity, zMin: Infinity, zMax: -Infinity },
        )
      : null,
  }));
}

const OUTSIDE = 'Keep the entire animation inside';
const round = (n) => Math.round(n * 10000) / 10000;

// Never refuse a program for its size: when its whole animation leaves the
// 12 × 10 × 12 volume, wrap it in (move (scale ...)) so it lands inside,
// centred and on the ground, and admit the wrapped source instead.
export function fitSource(source) {
  try {
    compiled(source);
    return source;
  } catch (error) {
    if (!String(error.message).startsWith(OUTSIDE)) throw error;
  }
  return wrapToFit(source, 11.9, 9.95);
}

// Handheld items fit a small volume, grip at the origin: scale the whole
// program to at most `size` metres on its longest side and stand it on y=0.
export function fitItemSource(source, size = 1.2) {
  const [[x0, x1], [y0, y1], [z0, z1]] = admit(source, true).extent;
  const longest = Math.max(x1 - x0, y1 - y0, z1 - z0);
  if (longest <= size && y0 >= -0.001 && Math.max(-x0, x1, -z0, z1) <= size) return source;
  const s = Math.min(1, size / Math.max(0.001, longest));
  const dx = -((x0 + x1) / 2) * s,
    dz = -((z0 + z1) / 2) * s,
    dy = -y0 * s;
  const head = /^\s*;[^\n]*\n?/.exec(source)?.[0] || '';
  return `${head}(move ${round(dx)} ${round(dy)} ${round(dz)} (scale ${round(s)} ${round(s)} ${round(s)}\n${source.slice(head.length)}\n))`;
}

function wrapToFit(source, across, tall) {
  const [[x0, x1], [y0, y1], [z0, z1]] = admit(source, true).extent;
  const s = Math.min(
    1,
    across / Math.max(0.001, x1 - x0),
    across / Math.max(0.001, z1 - z0),
    tall / Math.max(0.001, y1 - y0),
  );
  const cx = (x0 + x1) / 2,
    cz = (z0 + z1) / 2;
  // Recentre only on the axes that spill; lift onto the ground only if below it.
  const dx = x0 * s < -6 || x1 * s > 6 ? -cx * s : 0,
    dz = z0 * s < -6 || z1 * s > 6 ? -cz * s : 0,
    dy = y0 * s < 0 ? -y0 * s : y1 * s > 10 ? 10 - y1 * s : 0;
  const head = /^\s*;[^\n]*\n?/.exec(source)?.[0] || '';
  return `${head}(move ${round(dx)} ${round(dy)} ${round(dz)} (scale ${round(s)} ${round(s)} ${round(s)}\n${source.slice(head.length)}\n))`;
}

export function sourceGeometry(raw) {
  const name =
    String(raw.name || 'Object')
      .replace(/[^a-zA-Z0-9 -]/g, '')
      .trim()
      .slice(0, 32) || 'Object';
  const source = fitSource(raw.source);
  const p = compiled(source);
  return {
    format: OBJECT_FORMAT,
    name,
    source,
    boxes: p.boxes,
    animated: p.animated,
  };
}
export function sourceFrame(shape, time = 0, draw = true) {
  return compiled(shape.source).frame(time, draw);
}
export function parseSource(text) {
  const source = String(text)
    .trim()
    .replace(/^```(?:lisp|kidlisp|scheme)?\s*/, '')
    .replace(/\s*```$/, '');
  const name = /^\s*;\s*([^\n]+)/.exec(source)?.[1] || 'Object';
  return sourceGeometry({ name, source });
}
