// object-lisp.mjs — oskiewar objects written in KidLisp, compiled to closures.
//
// An object (a monowheel, a hat, a weapon) is a small .lisp program that says
// what the thing looks like this tick, in its own space: x forward, y up, z to
// the owner's right. `compile` turns it into `run(inputs, place, emit)` once;
// every tick after that is plain closures, never the tree-walker, because the
// tree-walker repaints a 61-line program every ~6 s on the oven. `emit` gets
// world faces with the game's own signature (emitWorldFace / the WORLD op), so
// an object is just more WORLD faces in the frame program. The dialect is
// spelled out in xbox/OBJECT-DIALECT.md.
//
// No imports and plain top-level exports: xbox/tools/embed-objects.mjs (next
// pass) seals this file into oskiewar.js the way embed-spine.mjs does, and
// every host (QuickJS, JavaScriptCore, the browser) runs the same compiler.

// What the game hands an object each tick. Seconds, world units, radians;
// `hit` and `land` count seconds since the event (large when it never was).
export const objectInputs = ["time", "distance", "speed", "lean", "heading",
  "pitch", "turbo", "hit", "land"];
const unset = [0, 0, 0, 0, 0, 0, 0, 1e9, 1e9];

// The game's sun (`globalLight` in oskiewar.js) and its flat-shading rule:
// .72 ambient plus .28 of the face turned toward the light, decided in world
// space from the face's own winding — so an object shades exactly like a
// worldQuad beside it. tests/object-lisp.test.mjs holds the two equal.
const sun = (() => {
  const x = -.42, y = 1, z = -.28, m = Math.hypot(x, y, z);
  return [x / m, y / m, z / m];
})();
export const objectLight = sun;

// ——— reading: KidLisp's own rules, so Aesel's KidLisp authoring applies ———
// A bare line that starts with a word is a call (`ring 12` → `(ring 12)`),
// commas separate calls on a line, `;` comments, missing `)` auto-close.
// tests hold this reader to KidLisp's `parse` on every object in objects/.

const tokenPattern = /\s*(;.*|[(),]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[^\s()";',]+)/g;

// Strings and comments masked with "_" (quotes and ";" kept), so line passes
// can count parens and find commas without cutting through text.
function mask(text) {
  let out = "", i = 0;
  while (i < text.length) {
    const ch = text[i];
    if (ch === ";") { out += ";" + "_".repeat(text.length - i - 1); break; }
    if (ch === '"' || ch === "'") {
      let j = i + 1;
      while (j < text.length && text[j] !== ch) j += text[j] === "\\" ? 2 : 1;
      if (j < text.length) { out += ch + "_".repeat(j - i - 1) + ch; i = j + 1; continue; }
    }
    out += ch; i++;
  }
  return out;
}
const startsWord = (text) => /^[a-zA-Z_]\w*/.test(text);
const count = (text, ch) => text.split(ch).length - 1;

export function read(source) {
  const lines = source.split("\n").map((line) => {
    const cut = mask(line).indexOf(";");
    return (cut < 0 ? line : line.slice(0, cut)).trim();
  }).filter(Boolean);
  const wrapped = lines.map((line, index) => {
    const masked = mask(line);
    if (masked.includes(",")) {
      const parts = [];
      let from = 0;
      for (let i = 0; i <= masked.length; i++)
        if (i === masked.length || masked[i] === ",") { parts.push(line.slice(from, i).trim()); from = i + 1; }
      return parts.filter(Boolean).map((part) =>
        part.startsWith("(") && part.endsWith(")") ? part : /^[a-zA-Z_$]\w*/.test(part) ? `(${part})` : part).join(" ");
    }
    // A word-led line inside an open call is a continuation, not a new call.
    const before = index > 0 ? mask(lines[index - 1]) : "";
    const continues = index > 0 && count(before, "(") > count(before, ")");
    return !line.startsWith("(") && startsWord(line) && !continues ? `(${line})` : line;
  }).join(" ");
  const tokens = [];
  for (const match of wrapped.matchAll(tokenPattern))
    if (!match[1].startsWith(";")) tokens.push(match[1]);
  let open = 0;
  for (const t of tokens) open += t === "(" ? 1 : t === ")" ? -1 : 0;
  while (open-- > 0) tokens.push(")");
  let at = 0;
  const form = () => {
    const t = tokens[at++];
    if (t === ")") throw new Error("unexpected )");
    // Numbers read as KidLisp's do; its timing words (`1s`, `2s...`) stay words.
    if (t !== "(") { const n = parseFloat(t); return Number.isNaN(n) || /^\d*\.?\d+s/.test(t) ? t : n; }
    const list = [];
    while (at < tokens.length && tokens[at] !== ")") {
      if (tokens[at] === ",") { at++; continue; }
      list.push(form());
    }
    at++;
    return list;
  };
  const forms = [];
  while (at < tokens.length) {
    if (tokens[at] === ",") { at++; continue; }
    forms.push(form());
  }
  return forms;
}

// ——— compiling ———

const show = (form) => Array.isArray(form) ? `(${form.map(show).join(" ")})` : String(form);

// Pure math the dialect knows. Everything is a number; comparisons are 1 or 0.
const math = {
  "+": (...v) => v.reduce((a, b) => a + b, 0),
  "-": (a, ...v) => v.length ? v.reduce((x, y) => x - y, a) : -a,
  "*": (...v) => v.reduce((a, b) => a * b, 1),
  "/": (a, b) => a / b,
  "%": (a, b) => ((a % b) + b) % b,
  min: Math.min, max: Math.max, abs: Math.abs, sign: Math.sign,
  sin: Math.sin, cos: Math.cos, tan: Math.tan, atan: Math.atan2, sqrt: Math.sqrt,
  pow: Math.pow, floor: Math.floor, round: Math.round,
  clamp: (x, lo, hi) => x < lo ? lo : x > hi ? hi : x,
  mix: (a, b, t) => a + (b - a) * t,
  "=": (a, b) => +(a === b), "<": (a, b) => +(a < b), ">": (a, b) => +(a > b),
  "<=": (a, b) => +(a <= b), ">=": (a, b) => +(a >= b),
  and: (...v) => +v.every(Boolean), or: (...v) => +v.some(Boolean), not: (a) => +!a,
};
const words = { pi: Math.PI, tau: Math.PI * 2 };
// A few names for `ink`; anything else is three numbers.
const inks = { white: [255, 255, 255], black: [0, 0, 0], gray: [128, 128, 128],
  red: [255, 0, 0], pink: [255, 105, 180], cyan: [0, 255, 255], yellow: [255, 255, 0] };

const maxDepth = 16;

export function compile(source, name = "object") {
  const forms = typeof source === "string" ? read(source) : source;
  const fail = (why, form) => { throw new Error(`${name}: ${why}${form === undefined ? "" : ` in ${show(form)}`}`); };
  let slots = objectInputs.length;

  // A scope maps a name to a slot (per tick) or a constant (a `def`).
  const lookup = (scope, word) => {
    for (let s = scope; s; s = s.up) if (word in s.names) return s.names[word];
    return word in words ? { value: words[word] } : null;
  };
  const top = { names: Object.fromEntries(objectInputs.map((n, i) => [n, { slot: i }])), up: null };

  // An expression is a closure (state) => number, or folds to a constant.
  function expr(form, scope) {
    if (typeof form === "number") return { value: form };
    if (typeof form === "string") {
      const found = lookup(scope, form);
      if (!found) fail(`unknown word \`${form}\``);
      if ("value" in found) return found;
      const i = found.slot;
      return { run: (s) => s.v[i] };
    }
    if (!Array.isArray(form) || !form.length) fail("empty expression", form);
    const [head, ...rest] = form;
    if (head === "owner") {
      // (owner part axis): a point of the owner's pose, handed over in object space.
      const part = rest[0], axis = "xyz".indexOf(rest[1]);
      if (typeof part !== "string" || axis < 0) fail("owner wants a part and x, y or z", form);
      return { run: (s) => s.owner?.[part]?.[axis] ?? 0 };
    }
    const fn = math[head];
    if (!fn) fail(`unknown function \`${head}\``, form);
    const args = rest.map((a) => expr(a, scope));
    if (args.every((a) => "value" in a)) return { value: fn(...args.map((a) => a.value)) };
    const run = args.map((a) => "value" in a ? () => a.value : a.run);
    if (run.length === 1) { const [a] = run; return { run: (s) => fn(a(s)) }; }
    if (run.length === 2) { const [a, b] = run; return { run: (s) => fn(a(s), b(s)) }; }
    if (run.length === 3) { const [a, b, c] = run; return { run: (s) => fn(a(s), b(s), c(s)) }; }
    return { run: (s) => fn(...run.map((r) => r(s))) };
  }
  const num = (form, scope) => {
    const e = expr(form, scope);
    return "value" in e ? () => e.value : e.run;
  };

  // A body runs its forms in order; a `let` binds for the forms after it.
  function body(list, scope, depth) {
    const inner = { names: {}, up: scope };
    const steps = list.map((form) => statement(form, inner, depth)).filter(Boolean);
    return (s) => { for (let i = 0; i < steps.length; i++) steps[i](s); };
  }

  // A transform pushes a new frame at a depth known here, so nesting is
  // checked once and the tick never counts.
  function framed(form, scope, depth, set) {
    if (depth + 1 >= maxDepth) fail(`nested deeper than ${maxDepth}`, form);
    const run = body(form.slice(set.arity + 1), scope, depth + 1);
    const args = form.slice(1, set.arity + 1).map((a) => num(a, scope));
    const from = depth * 13, to = from + 13;
    return (s) => {
      const m = s.m;
      for (let i = 0; i < 13; i++) m[to + i] = m[from + i];
      set.apply(m, to, args, s);
      run(s);
    };
  }

  const shapes = { tri: 9, quad: 12, disc: 1, hoop: 2, band: 2, capsule: 7, line: 6 };
  const statements = new Set(["def", "let", "if", "repeat", "ink", "glow", "move", "rotate", "scale"]);
  const isStatement = (f) => Array.isArray(f) && (statements.has(f[0]) || f[0] in shapes);

  function statement(form, scope, depth) {
    if (!Array.isArray(form)) fail(`\`${show(form)}\` on its own does nothing`);
    const [head, ...rest] = form;
    switch (head) {
      case "def": {
        // KidLisp's def binds once. Here that is at compile time, so a def
        // can't read a per-tick input — that is what `let` is for.
        let value;
        try { value = expr(rest[1], scope); } catch (e) { fail(e.message.replace(`${name}: `, ""), form); }
        if (!("value" in value)) fail("def binds once and can't read a per-tick input; use let", form);
        scope.names[rest[0]] = value;
        return null;
      }
      case "let": {
        const run = num(rest[1], scope), slot = slots++;
        scope.names[rest[0]] = { slot };
        return (s) => { s.v[slot] = run(s); };
      }
      case "if": {
        // No else, as in KidLisp: the body is every form after the test.
        const test = num(rest[0], scope), run = body(rest.slice(1), scope, depth);
        return (s) => { if (test(s)) run(s); };
      }
      case "repeat": {
        const times = num(rest[0], scope);
        const inner = { names: {}, up: scope };
        const named = typeof rest[1] === "string" && rest.length > 2 && !lookup(scope, rest[1]);
        const slot = named ? slots++ : -1;
        if (named) inner.names[rest[1]] = { slot };
        const run = body(rest.slice(named ? 2 : 1), inner, depth);
        return (s) => {
          const n = times(s);
          for (let i = 0; i < n; i++) { if (slot >= 0) s.v[slot] = i; run(s); }
        };
      }
      case "ink": {
        if (rest.length === 1 && inks[rest[0]]) {
          const [r, g, b] = inks[rest[0]];
          return (s) => { s.r = r; s.g = g; s.b = b; };
        }
        if (rest.length !== 3) fail("ink wants a name or r g b", form);
        const [r, g, b] = rest.map((a) => num(a, scope));
        return (s) => { s.r = r(s); s.g = g(s); s.b = b(s); };
      }
      case "glow": {
        // Unlit inside: lamps, turbo trim, anything that makes its own light.
        const run = body(rest, scope, depth);
        return (s) => { const was = s.glow; s.glow = true; run(s); s.glow = was; };
      }
      case "move": return framed(form, scope, depth, { arity: 3, apply: move });
      case "rotate": {
        const axis = "xyz".indexOf(rest[0]);
        if (axis < 0) fail("rotate wants x, y or z first", form);
        return framed(["rotate", ...rest.slice(1)], scope, depth,
          { arity: 1, apply: (m, at, [a], s) => rotate(m, at, axis, a(s)) });
      }
      case "scale": {
        // One number scales evenly; three scale each axis (a negative one
        // mirrors, and faces keep facing out).
        const three = rest.length >= 3 && !isStatement(rest[1]);
        return framed(three ? form : ["scale", rest[0], rest[0], rest[0], ...rest.slice(1)],
          scope, depth, { arity: 3, apply: scale });
      }
    }
    if (head in shapes) {
      const want = shapes[head];
      if (rest.length < want) fail(`${head} wants ${want} numbers`, form);
      const args = rest.map((a) => num(a, scope));
      const values = new Float64Array(args.length);
      const draw = primitives[head];
      return (s) => {
        for (let i = 0; i < args.length; i++) values[i] = args[i](s);
        draw(s, depth * 13, values);
      };
    }
    fail(`unknown form \`${head}\``, form);
  }

  const run = body(forms, top, 0);
  const state = { v: new Float64Array(slots), m: new Float64Array(maxDepth * 13),
    r: 255, g: 255, b: 255, glow: false, emit: null, owner: null };
  // place: origin, then where object x, y and z point, in world space —
  // twelve numbers, as the game's rig frames already know them.
  return function object(inputs, place, emit) {
    const v = state.v, m = state.m;
    for (let i = 0; i < objectInputs.length; i++) {
      const x = inputs[objectInputs[i]];
      v[i] = x === undefined ? unset[i] : +x;
    }
    for (let i = 0; i < 12; i++) m[i] = place[i];
    m[12] = handedness(m, 0);
    state.r = state.g = state.b = 255;
    state.glow = false;
    state.emit = emit;
    state.owner = inputs.owner || null;
    run(state);
  };
}

// ——— frames: 13 numbers each, origin · x axis · y axis · z axis · winding ———
// The winding flag is -1 under a mirror, so a face that faced out still does.

function handedness(m, at) {
  const ax = m[at + 3], ay = m[at + 4], az = m[at + 5];
  const bx = m[at + 6], by = m[at + 7], bz = m[at + 8];
  const cx = m[at + 9], cy = m[at + 10], cz = m[at + 11];
  return ax * (by * cz - bz * cy) - ay * (bx * cz - bz * cx) + az * (bx * cy - by * cx) < 0 ? -1 : 1;
}
function move(m, at, [x, y, z], s) {
  const dx = x(s), dy = y(s), dz = z(s);
  for (let k = 0; k < 3; k++) m[at + k] += dx * m[at + 3 + k] + dy * m[at + 6 + k] + dz * m[at + 9 + k];
}
function scale(m, at, [x, y, z], s) {
  const fx = x(s), fy = y(s), fz = z(s);
  for (let k = 0; k < 3; k++) { m[at + 3 + k] *= fx; m[at + 6 + k] *= fy; m[at + 9 + k] *= fz; }
  m[at + 12] = handedness(m, at);
}
// Right-handed turns: about x carries y toward z, about y carries z toward x,
// about z carries x toward y.
function rotate(m, at, axis, angle) {
  const c = Math.cos(angle), sn = Math.sin(angle);
  const p = at + 3 + ((axis + 1) % 3) * 3, q = at + 3 + ((axis + 2) % 3) * 3;
  for (let k = 0; k < 3; k++) {
    const u = m[p + k], w = m[q + k];
    m[p + k] = u * c + w * sn;
    m[q + k] = w * c - u * sn;
  }
}

// ——— faces ———

const world = new Float64Array(9);
// One face in object space: to world, shaded by the game's rule, emitted.
function face(s, at, ax, ay, az, bx, by, bz, cx, cy, cz, lightFrom) {
  const m = s.m, w = world;
  const put = (o, x, y, z) => {
    for (let k = 0; k < 3; k++) w[o + k] = m[at + k] + x * m[at + 3 + k] + y * m[at + 6 + k] + z * m[at + 9 + k];
  };
  if (m[at + 12] < 0) { put(0, ax, ay, az); put(3, cx, cy, cz); put(6, bx, by, bz); }
  else { put(0, ax, ay, az); put(3, bx, by, bz); put(6, cx, cy, cz); }
  let r = s.r, g = s.g, b = s.b;
  if (!s.glow) {
    const n = lightFrom || w;
    const ux = n[3] - n[0], uy = n[4] - n[1], uz = n[5] - n[2];
    const vx = n[6] - n[0], vy = n[7] - n[1], vz = n[8] - n[2];
    const nx = uy * vz - uz * vy, ny = uz * vx - ux * vz, nz = ux * vy - uy * vx;
    const toward = -(nx * sun[0] + ny * sun[1] + nz * sun[2]) / (Math.hypot(nx, ny, nz) || 1);
    const k = .72 + (toward > 0 ? toward : 0) * .28;
    r = Math.round(r * k); g = Math.round(g * k); b = Math.round(b * k);
  }
  s.emit(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8], r, g, b);
}
// A quad is two faces lit as one, off its first three corners, as worldQuad does.
const quadLight = new Float64Array(9);
function quad(s, at, ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz) {
  face(s, at, ax, ay, az, bx, by, bz, cx, cy, cz, null);
  quadLight.set(world);
  face(s, at, ax, ay, az, cx, cy, cz, dx, dy, dz, quadLight);
}
// Sides asked for (at least `least`), or picked by radius as the game's fans are.
const sidesFor = (r, v, i, least = 3) => v.length > i ? Math.min(64, Math.max(least, Math.floor(v[i])))
  : r < 6 ? 6 : r < 13 ? 8 : r < 26 ? 12 : 16;

const primitives = {
  tri: (s, at, v) => face(s, at, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], null),
  quad: (s, at, v) => quad(s, at, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11]),
  // (disc r [sides]) — flat, at the origin, facing +z.
  disc: (s, at, v) => {
    const r = v[0], n = sidesFor(r, v, 1);
    for (let i = 0; i < n; i++) {
      const a = i / n * Math.PI * 2, b = (i + 1) / n * Math.PI * 2;
      face(s, at, 0, 0, 0, Math.cos(a) * r, Math.sin(a) * r, 0, Math.cos(b) * r, Math.sin(b) * r, 0, null);
    }
  },
  // (hoop inner outer [sides]) — a flat ring facing +z.
  hoop: (s, at, v) => {
    const r1 = v[0], r2 = v[1], n = sidesFor(r2, v, 2);
    for (let i = 0; i < n; i++) {
      const a = i / n * Math.PI * 2, b = (i + 1) / n * Math.PI * 2;
      const ca = Math.cos(a), sa = Math.sin(a), cb = Math.cos(b), sb = Math.sin(b);
      quad(s, at, ca * r1, sa * r1, 0, ca * r2, sa * r2, 0, cb * r2, sb * r2, 0, cb * r1, sb * r1, 0);
    }
  },
  // (band radius width [sides] [turn]) — a tube's outside around z; `turn`
  // (0–1) draws only that much of it, from +x toward +y.
  band: (s, at, v) => {
    const r = v[0], h = v[1] / 2, n = sidesFor(r, v, 2, 1), turn = v.length > 3 ? v[3] : 1;
    for (let i = 0; i < n; i++) {
      const a = i / n * Math.PI * 2 * turn, b = (i + 1) / n * Math.PI * 2 * turn;
      const ca = Math.cos(a) * r, sa = Math.sin(a) * r, cb = Math.cos(b) * r, sb = Math.sin(b) * r;
      quad(s, at, ca, sa, -h, cb, sb, -h, cb, sb, h, ca, sa, h);
    }
  },
  // (capsule x1 y1 z1 x2 y2 z2 width [sides]) — a rod between two points.
  // Today a prism; a world CAPSULE op would let each host round it itself.
  capsule: (s, at, v) => rod(s, at, v, v[6], v.length > 7 ? v[7] : 6),
  // (line x1 y1 z1 x2 y2 z2 [width]) — a thin three-sided rod.
  line: (s, at, v) => rod(s, at, v, v.length > 6 ? v[6] : 1.5, 3),
};

function rod(s, at, v, width, sides) {
  let dx = v[3] - v[0], dy = v[4] - v[1], dz = v[5] - v[2];
  const length = Math.hypot(dx, dy, dz);
  if (length < 1e-6) return;
  dx /= length; dy /= length; dz /= length;
  // u: any unit vector across the rod; w = d × u completes the frame.
  let ux = -dy, uy = dx, uz = 0;
  if (Math.abs(dz) > .9) { ux = 0; uy = -dz; uz = dy; }
  const um = Math.hypot(ux, uy, uz); ux /= um; uy /= um; uz /= um;
  const wx = dy * uz - dz * uy, wy = dz * ux - dx * uz, wz = dx * uy - dy * ux;
  const r = width / 2, n = Math.max(3, Math.floor(sides));
  for (let i = 0; i < n; i++) {
    const a = i / n * Math.PI * 2, b = (i + 1) / n * Math.PI * 2;
    const ca = Math.cos(a) * r, sa = Math.sin(a) * r, cb = Math.cos(b) * r, sb = Math.sin(b) * r;
    const ax = ux * ca + wx * sa, ay = uy * ca + wy * sa, az = uz * ca + wz * sa;
    const bx = ux * cb + wx * sb, by = uy * cb + wy * sb, bz = uz * cb + wz * sb;
    quad(s, at, v[0] + ax, v[1] + ay, v[2] + az, v[0] + bx, v[1] + by, v[2] + bz,
      v[3] + bx, v[4] + by, v[5] + bz, v[3] + ax, v[4] + ay, v[5] + az);
  }
}
