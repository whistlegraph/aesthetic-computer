// object-lisp.mjs — oskiewar objects written in KidLisp, compiled once.
//
// An object (a monowheel, a hat, a weapon) is a small .lisp program that says
// what the thing looks like, in its own space: x forward, y up, z to the
// owner's right. `compile` does all the work when the object loads:
//
// - every part that doesn't change tick to tick is run once, at each of three
//   levels of detail, and baked into meshes (`object.meshes`), for the host to
//   keep as ASSETs;
// - what is left is closures, never the tree-walker, because the tree-walker
//   repaints a 61-line program about every 6 s on the oven.
//
// A tick, `object(inputs, place, out)`, then emits one MODEL op per baked part
// (`out.model`: a mesh by handle under a 12-number placement) and WORLD faces
// (`out.face`) only for what really moves. The high-level forms (revolve,
// radial, mirror) compile away here; hosts see faces and meshes and never
// expand anything. The dialect: xbox/OBJECT-DIALECT.md.
//
// No imports and plain top-level exports: xbox/tools/embed-objects.mjs (next
// pass) seals this file into oskiewar.js the way embed-spine.mjs does, and
// every host (QuickJS, JavaScriptCore, the browser) runs the same compiler.

// What the game hands an object each tick. Seconds, world units, radians;
// `hit` and `land` count seconds since the event (large when it never was).
export const objectInputs = ["time", "distance", "speed", "lean", "heading",
  "pitch", "turbo", "hit", "land"];
const unset = [0, 0, 0, 0, 0, 0, 0, 1e9, 1e9];
// Switches are 0 or 1, so a baked part that reads one is baked once per value.
const switches = new Set(["turbo"]);
// `detail` is the level a baked part is drawn at: 0 near, 2 far. The host
// picks it per MODEL op; outside baked parts it reads 0.
const detailSlot = objectInputs.length;
export const objectLevels = 3;

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
const owner = -1;   // a read of the owner's pose, which moves every tick
const shapes = { tri: 9, quad: 12, disc: 1, hoop: 2, band: 2, capsule: 7, line: 6 };
// Flat shapes: object-space anchors, projected here, drawn as 2D ops.
const flats = { ball: 4, limb: 7, ring: 2, drum: 3, stroke: 7, plate: 9, slab: 6 };
const forms = new Set(["def", "let", "if", "repeat", "ink", "glow", "move", "rotate",
  "scale", "radial", "mirror", "revolve", "outline", "nudge", "toward"]);
const isStatement = (f) => Array.isArray(f) && (forms.has(f[0]) || f[0] in shapes || f[0] in flats);
const union = (...sets) => { const out = new Set(); for (const s of sets) for (const x of s) out.add(x); return out; };

export function compile(source, name = "object") {
  const program = typeof source === "string" ? read(source) : source;
  const fail = (why, form) => { throw new Error(`${name}: ${why}${form === undefined ? "" : ` in ${show(form)}`}`); };
  let slots = detailSlot + 1;
  // Per slot: does it change from tick to tick? Switches and `detail` don't.
  const moving = objectInputs.map((n) => !switches.has(n));
  moving[detailSlot] = false;
  const isSwitch = (slot) => slot >= 0 && slot < objectInputs.length && switches.has(objectInputs[slot]);
  const ticks = (reads) => { for (const r of reads) if (r < 0 || moving[r]) return true; return false; };
  const parts = [];

  // A scope maps a name to a slot or a constant (a `def`).
  const lookup = (scope, word) => {
    for (let s = scope; s; s = s.up) if (word in s.names) return s.names[word];
    return word in words ? { value: words[word] } : null;
  };
  const top = { names: Object.fromEntries([...objectInputs, "detail"].map((n, i) => [n, { slot: i }])), up: null };

  // An expression folds to a constant, or is a closure with the slots it reads.
  function expr(form, scope) {
    if (typeof form === "number") return { value: form };
    if (typeof form === "string") {
      const found = lookup(scope, form);
      if (!found) fail(`unknown word \`${form}\``);
      if ("value" in found) return found;
      const i = found.slot;
      return { run: (s) => s.v[i], reads: new Set([i]) };
    }
    if (!Array.isArray(form) || !form.length) fail("empty expression", form);
    const [head, ...rest] = form;
    if (head === "owner") {
      // (owner part axis): a point of the owner's pose, handed over in object space.
      const part = rest[0], axis = "xyz".indexOf(rest[1]);
      if (typeof part !== "string" || axis < 0) fail("owner wants a part and x, y or z", form);
      return { run: (s) => s.owner?.[part]?.[axis] ?? 0, reads: new Set([owner]) };
    }
    const fn = math[head];
    if (!fn) fail(`unknown function \`${head}\``, form);
    const args = rest.map((a) => expr(a, scope));
    if (args.every((a) => "value" in a)) return { value: fn(...args.map((a) => a.value)) };
    const reads = union(...args.map((a) => a.reads || []));
    const run = args.map((a) => "value" in a ? () => a.value : a.run);
    if (run.length === 1) { const [a] = run; return { run: (s) => fn(a(s)), reads }; }
    if (run.length === 2) { const [a, b] = run; return { run: (s) => fn(a(s), b(s)), reads }; }
    if (run.length === 3) { const [a, b, c] = run; return { run: (s) => fn(a(s), b(s), c(s)), reads }; }
    return { run: (s) => fn(...run.map((r) => r(s))), reads };
  }
  // A number as a closure; what it reads is added to `reads`.
  const num = (form, scope, reads) => {
    const e = expr(form, scope);
    if ("value" in e) return () => e.value;
    for (const r of e.reads) reads.add(r);
    return e.run;
  };

  // Every statement compiles to a node: its closure, the slots it reads from
  // outside itself, whether it draws, and how it uses the sticky ink (reads
  // the ink it came in with; sets it 0 never, 1 maybe, 2 always). `ctx`
  // follows the ink and glow in effect as the compile walks in run order.
  const inkIn = (nodes) => { for (const n of nodes) { if (n.inkIn) return true; if (n.inkSets === 2) return false; } return false; };
  function body(list, scope, depth, ctx) {
    const inner = { names: {}, up: scope };
    const nodes = [], entries = [];
    for (const form of list) {
      const entry = { ink: ctx.ink, glow: ctx.glow, edge: ctx.edge, nudge: ctx.nudge }, first = parts.length;
      const node = statement(form, inner, depth, ctx);
      if (node) { node.parts = [first, parts.length]; nodes.push(node); entries.push(entry); }
    }
    const steps = bakeRuns(nodes, entries, depth);
    const bound = union(...nodes.map((n) => n.binds || []));
    const reads = union(...nodes.map((n) => n.reads));
    for (const b of bound) reads.delete(b);
    return { run: (s) => { for (let i = 0; i < steps.length; i++) steps[i](s); },
      reads, draws: nodes.some((n) => n.draws), inkIn: inkIn(nodes),
      inkSets: Math.max(0, ...nodes.map((n) => n.inkSets)) };
  }

  // Runs of statements that don't move from tick to tick become baked parts.
  function bakeRuns(nodes, entries, depth) {
    const steps = [];
    for (let i = 0; i < nodes.length;) {
      if (ticks(nodes[i].reads)) { steps.push(nodes[i].run); i++; continue; }
      let j = i;
      while (j < nodes.length && !ticks(nodes[j].reads)) j++;
      const group = nodes.slice(i, j), part = partOf(group, entries[i], depth);
      if (part) steps.push(part);
      else for (const n of group) steps.push(n.run);
      i = j;
    }
    return steps;
  }

  // A part bakes if everything it reads is a switch, `detail`, or bound
  // inside it, and it knows the ink it starts with.
  function partOf(group, entry, depth) {
    if (!group.some((n) => n.draws)) return null;
    const bound = union(...group.map((n) => n.binds || []));
    const free = union(...group.map((n) => n.reads));
    for (const b of bound) free.delete(b);
    for (const r of free) if (!isSwitch(r) && r !== detailSlot) return null;
    if (inkIn(group) && !entry.ink) return null;
    // An outline or nudge that moves per tick can't be baked into it either.
    if (!entry.edge || entry.nudge === null) return null;
    // Parts inside this one are baked into it, never emitted on their own.
    for (const n of group) for (let i = n.parts[0]; i < n.parts[1]; i++) parts[i].inner = true;
    const part = { depth, runs: group.map((n) => n.run), lets: group.filter((n) => n.binds).map((n) => n.run),
      switches: [...free].filter(isSwitch), detail: free.has(detailSlot),
      ink: entry.ink || [255, 255, 255], glow: entry.glow, edge: entry.edge, nudge: entry.nudge, variants: null };
    parts.push(part);
    const { runs, lets } = part, at = depth * 13;
    return (s) => {
      if (!part.variants || s.rec || (part.meshed && !s.model) || (part.sketched && !s.sketch)) {
        for (let k = 0; k < runs.length; k++) runs[k](s);
        return;
      }
      for (let k = 0; k < lets.length; k++) lets[k](s);   // what follows may read them
      let index = 0;
      for (let k = 0; k < part.switches.length; k++) if (s.v[part.switches[k]] >= .5) index |= 1 << k;
      const v = part.variants[index];
      if (v.levels[0] >= 0) s.model(v.radius, v.levels[0], v.levels[1], v.levels[2], s.m, at);
      if (v.shapes >= 0) s.sketch(v.shapes, s.m, at);
      s.r = v.ink[0]; s.g = v.ink[1]; s.b = v.ink[2];
    };
  }

  // A transform pushes a new frame at a depth known here, so nesting is
  // checked once and the tick never counts.
  function framed(form, scope, depth, ctx, arity, apply) {
    if (depth + 1 >= maxDepth) fail(`nested deeper than ${maxDepth}`, form);
    const reads = new Set();
    const args = form.slice(1, arity + 1).map((a) => num(a, scope, reads));
    const inside = body(form.slice(arity + 1), scope, depth + 1, ctx);
    const from = depth * 13, to = from + 13, run = inside.run;
    return { ...inside, reads: union(reads, inside.reads), run: (s) => {
      const m = s.m;
      for (let i = 0; i < 13; i++) m[to + i] = m[from + i];
      apply(m, to, args, s);
      run(s);
    } };
  }
  // An iterator slot: it moves only if the count it walks does.
  function iterator(scope, word, countReads, rest) {
    const named = typeof word === "string" && rest > 0 && !lookup(scope, word);
    if (!named) return -1;
    const slot = slots++;
    moving[slot] = ticks(countReads);
    scope.names[word] = { slot };
    return slot;
  }
  // A body that might run zero times or twice leaves the ink unknown after.
  const maybe = (ctx, run) => {
    const inside = run({ ...ctx });
    if (inside.inkSets) ctx.ink = null;
    return inside;
  };

  function statement(form, scope, depth, ctx) {
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
        const reads = new Set(), run = num(rest[1], scope, reads), slot = slots++;
        moving[slot] = ticks(reads);
        scope.names[rest[0]] = { slot };
        return { run: (s) => { s.v[slot] = run(s); }, reads, binds: [slot], inkSets: 0 };
      }
      case "if": {
        // No else, as in KidLisp: the body is every form after the test.
        const reads = new Set(), test = num(rest[0], scope, reads);
        const inside = maybe(ctx, (c) => body(rest.slice(1), scope, depth, c)), run = inside.run;
        return { ...inside, reads: union(reads, inside.reads), inkSets: inside.inkSets ? 1 : 0,
          run: (s) => { if (test(s)) run(s); } };
      }
      case "repeat": {
        const reads = new Set(), times = num(rest[0], scope, reads);
        const inner = { names: {}, up: scope };
        const slot = iterator(inner, rest[1], reads, rest.length - 2);
        const inside = maybe(ctx, (c) => body(rest.slice(slot >= 0 ? 2 : 1), inner, depth, c)), run = inside.run;
        const own = union(reads, inside.reads);
        own.delete(slot);
        return { ...inside, reads: own, inkSets: inside.inkSets ? 1 : 0, run: (s) => {
          const n = times(s);
          for (let i = 0; i < n; i++) { if (slot >= 0) s.v[slot] = i; run(s); }
        } };
      }
      case "ink": {
        if (rest.length === 1 && inks[rest[0]]) {
          const [r, g, b] = inks[rest[0]];
          ctx.ink = [r, g, b];
          return { run: (s) => { s.r = r; s.g = g; s.b = b; }, reads: new Set(), inkSets: 2 };
        }
        if (rest.length !== 3) fail("ink wants a name or r g b", form);
        const known = rest.map((a) => expr(a, scope));
        ctx.ink = known.every((e) => "value" in e) ? known.map((e) => e.value) : null;
        const reads = new Set(), [r, g, b] = rest.map((a) => num(a, scope, reads));
        return { run: (s) => { s.r = r(s); s.g = g(s); s.b = b(s); }, reads, inkSets: 2 };
      }
      case "glow": {
        // Unlit inside: lamps, turbo trim, anything that makes its own light.
        const was = ctx.glow;
        ctx.glow = true;
        const inside = body(rest, scope, depth, ctx), run = inside.run;
        ctx.glow = was;
        return { ...inside, run: (s) => { const before = s.glow; s.glow = true; run(s); s.glow = before; } };
      }
      case "move": return framed(form, scope, depth, ctx, 3, move);
      case "rotate": {
        const axis = "xyz".indexOf(rest[0]);
        if (axis < 0) fail("rotate wants x, y or z first", form);
        return framed(["rotate", ...rest.slice(1)], scope, depth, ctx, 1,
          (m, at, [a], s) => rotate(m, at, axis, a(s)));
      }
      case "scale": {
        // One number scales evenly; three scale each axis (a negative one
        // mirrors, and faces keep facing out).
        const three = rest.length >= 3 && !isStatement(rest[1]);
        return framed(three ? form : ["scale", rest[0], rest[0], rest[0], ...rest.slice(1)],
          scope, depth, ctx, 3, scale);
      }
      case "radial": {
        // (radial axis n [k] body…): the body n times, turned evenly about the axis.
        const axis = "xyz".indexOf(rest[0]);
        if (axis < 0) fail("radial wants x, y or z first", form);
        if (depth + 1 >= maxDepth) fail(`nested deeper than ${maxDepth}`, form);
        const reads = new Set(), times = num(rest[1], scope, reads);
        const inner = { names: {}, up: scope };
        const slot = iterator(inner, rest[2], reads, rest.length - 3);
        const inside = maybe(ctx, (c) => body(rest.slice(slot >= 0 ? 3 : 2), inner, depth + 1, c)), run = inside.run;
        const own = union(reads, inside.reads);
        own.delete(slot);
        const from = depth * 13, to = from + 13;
        return { ...inside, reads: own, inkSets: inside.inkSets ? 1 : 0, run: (s) => {
          const n = times(s), m = s.m;
          for (let i = 0; i < n; i++) {
            for (let k = 0; k < 13; k++) m[to + k] = m[from + k];
            rotate(m, to, axis, i / n * Math.PI * 2);
            if (slot >= 0) s.v[slot] = i;
            run(s);
          }
        } };
      }
      case "mirror": {
        // (mirror axis body…): the body, then its reflection across that axis.
        const axis = "xyz".indexOf(rest[0]);
        if (axis < 0) fail("mirror wants x, y or z first", form);
        if (depth + 1 >= maxDepth) fail(`nested deeper than ${maxDepth}`, form);
        const inside = body(rest.slice(1), scope, depth + 1, ctx), run = inside.run;
        const from = depth * 13, to = from + 13;
        return { ...inside, run: (s) => {
          const m = s.m;
          for (const side of [1, -1]) {
            for (let k = 0; k < 13; k++) m[to + k] = m[from + k];
            if (side < 0) { for (let k = 0; k < 3; k++) m[to + 3 + axis * 3 + k] *= -1; m[to + 12] *= -1; }
            run(s);
          }
        } };
      }
      case "outline": {
        // (outline w [r g b] body…): the body's flat shapes drawn with an ink
        // edge w world units wide, sized where the scope starts.
        const lead = rest.findIndex((a) => isStatement(a));
        const opening = rest.slice(0, lead < 0 ? rest.length : lead);
        if (opening.length !== 1 && opening.length !== 4) fail("outline wants w, or w r g b", form);
        const reads = new Set(), [w, r, g, b] = opening.map((a) => num(a, scope, reads));
        const known = opening.map((a) => expr(a, scope)), wasEdge = ctx.edge;
        ctx.edge = !wasEdge || !known.every((e) => "value" in e) ? null
          : [known[0].value, ...(known.length === 4 ? known.slice(1).map((e) => e.value) : wasEdge.slice(1))];
        const inside = body(rest.slice(opening.length), scope, depth, ctx), run = inside.run;
        ctx.edge = wasEdge;
        return { ...inside, reads: union(reads, inside.reads), run: (s) => {
          const was = s.outline.slice();
          // Baked, the width stays in world units and the host sizes it.
          s.outline[0] = s.sketching ? w(s) * frameSize(s.m, depth * 13) : w(s) * scaleAt(s, depth * 13);
          if (r) { s.outline[1] = r(s); s.outline[2] = g(s); s.outline[3] = b(s); }
          run(s);
          s.outline.splice(0, 4, ...was);
        } };
      }
      case "nudge": {
        // (nudge d body…): the body's flat shapes d world units further back,
        // to settle what covers what where two shapes share a depth.
        const reads = new Set(), d = num(rest[0], scope, reads);
        const known = expr(rest[0], scope), wasNudge = ctx.nudge;
        ctx.nudge = wasNudge === null || !("value" in known) ? null : wasNudge + known.value;
        const inside = body(rest.slice(1), scope, depth, ctx), run = inside.run;
        ctx.nudge = wasNudge;
        return { ...inside, reads: union(reads, inside.reads), run: (s) => {
          const was = s.nudge;
          s.nudge += d(s);
          run(s);
          s.nudge = was;
        } };
      }
      case "toward": {
        // (toward axis body…): the body on whichever side of that axis faces
        // the camera, so a wheel shows the face you can see.
        const axis = "xyz".indexOf(rest[0]);
        if (axis < 0) fail("toward wants x, y or z first", form);
        if (depth + 1 >= maxDepth) fail(`nested deeper than ${maxDepth}`, form);
        const inside = body(rest.slice(1), scope, depth + 1, ctx), run = inside.run;
        const from = depth * 13, to = from + 13;
        const flip = (m) => { for (let k = 0; k < 3; k++) m[to + 3 + axis * 3 + k] *= -1; m[to + 12] *= -1; };
        return { ...inside, run: (s) => {
          const m = s.m;
          if (s.sketching) {
            // Baked: both faces, each marked one-sided, and the host shows the
            // one turned its way.
            for (const side of [1, -1]) {
              for (let k = 0; k < 13; k++) m[to + k] = m[from + k];
              if (side < 0) flip(m);
              const was = s.facing;
              s.facing = [m[to + 3 + axis * 3], m[to + 4 + axis * 3], m[to + 5 + axis * 3]];
              run(s);
              s.facing = was;
            }
            return;
          }
          const V = s.view;
          for (let k = 0; k < 13; k++) m[to + k] = m[from + k];
          let away = 0;
          for (let k = 0; k < 3; k++) away += m[to + 3 + axis * 3 + k] * (m[to + k] - V[k]);
          if (away > 0) flip(m);
          run(s);
        } };
      }
      case "revolve": {
        // (revolve axis [turn] r h r h …): a closed profile turned about the axis.
        const axis = "xyz".indexOf(rest[0]);
        if (axis < 0) fail("revolve wants x, y or z first", form);
        if (rest.length < 7) fail("revolve wants at least three r h points", form);
        const reads = new Set(), args = rest.slice(1).map((a) => num(a, scope, reads));
        const values = new Float64Array(args.length);
        return { reads, draws: true, inkIn: true, inkSets: 0, run: (s) => {
          for (let i = 0; i < args.length; i++) values[i] = args[i](s);
          revolve(s, depth * 13, axis, values);
        } };
      }
    }
    if (head in flats) {
      if (rest.length < flats[head]) fail(`${head} wants ${flats[head]} arguments`, form);
      const axial = head === "ring" || head === "drum";
      const axis = axial ? "xyz".indexOf(rest[0]) : -1;
      if (axial && axis < 0) fail(`${head} wants x, y or z first`, form);
      const reads = new Set(), args = rest.slice(axial ? 1 : 0).map((a) => num(a, scope, reads));
      const values = new Float64Array(args.length), draw = flatShapes[head], record = recorders[head];
      return { reads, draws: true, inkIn: true, inkSets: 0, run: (s) => {
        for (let i = 0; i < args.length; i++) values[i] = args[i](s);
        if (s.sketching) record(s, depth * 13, values, axis);
        else { inkUp(s); draw(s, depth * 13, values, axis); }
      } };
    }
    if (head in shapes) {
      const want = shapes[head];
      if (rest.length < want) fail(`${head} wants ${want} numbers`, form);
      const reads = new Set(), args = rest.map((a) => num(a, scope, reads));
      const values = new Float64Array(args.length);
      const draw = primitives[head];
      return { reads, draws: true, inkIn: true, inkSets: 0, run: (s) => {
        for (let i = 0; i < args.length; i++) values[i] = args[i](s);
        draw(s, depth * 13, values);
      } };
    }
    fail(`unknown form \`${head}\``, form);
  }

  // What the compile knows is in effect as it walks: the ink, glow, and the
  // ink edge and nudge a part would bake with (null when they move per tick).
  const run = body(program, top, 0, { ink: [255, 255, 255], glow: false, edge: [0, 24, 20, 30], nudge: 0 }).run;
  const state = { v: new Float64Array(slots), m: new Float64Array(maxDepth * 13),
    r: 255, g: 255, b: 255, glow: false, face: null, model: null, owner: null, rec: null,
    out: null, view: null, nudge: 0, outline: [0, 24, 20, 30], sketch: null, sketching: null, facing: null,
    inked: [0, 0, 0, 0] };

  // Bake every part: per switch value, per level, run once into a mesh. Twin
  // meshes (a level that didn't change anything) share one handle.
  const meshes = [];
  const store = (mesh) => {
    const same = meshes.findIndex((o) => o.count === mesh.count &&
      o.vertices.every((x, i) => x === mesh.vertices[i]) && o.faces.every((x, i) => x === mesh.faces[i]));
    return same >= 0 ? same : meshes.push(mesh) - 1;
  };
  const sketches = [];
  const keep = (shapes) => {
    const same = sketches.findIndex((o) => o.count === shapes.count && o.records.length === shapes.records.length &&
      o.records.every((x, i) => x === shapes.records[i]));
    return same >= 0 ? same : sketches.push(shapes) - 1;
  };
  const outer = parts.filter((part) => !part.inner);
  for (const part of outer) {
    part.variants = [];
    for (let index = 0; index < 1 << part.switches.length; index++) {
      part.switches.forEach((slot, k) => { state.v[slot] = (index >> k) & 1; });
      const levels = [];
      let radius = 0, ink = part.ink, shapes = -1;
      for (let level = 0; level < objectLevels; level++) {
        if (level && !part.detail) { levels.push(levels[0]); continue; }
        state.v[detailSlot] = level;
        state.m.set(identity, part.depth * 13);
        [state.r, state.g, state.b] = part.ink;
        state.glow = part.glow;
        state.rec = builder();
        state.sketching = sketcher();
        state.outline.splice(0, 4, ...part.edge);
        state.nudge = part.nudge;
        state.facing = null;
        for (const step of part.runs) step(state);
        const mesh = state.rec.done(), sketch = state.sketching.done();
        state.rec = state.sketching = null;
        if (!level) {
          ink = [state.r, state.g, state.b];
          // Flat shapes don't change with level: the host picks their sides.
          shapes = sketch.count ? keep(sketch) : -1;
        }
        radius = Math.max(radius, mesh.radius);
        levels.push(mesh.count ? store(mesh) : -1);
      }
      part.meshed ||= levels[0] >= 0;
      part.sketched ||= shapes >= 0;
      part.variants.push({ levels, radius, ink, shapes });
    }
  }
  state.v.fill(0);

  // place: origin, then where object x, y and z point, in world space —
  // twelve numbers, as the game's rig frames already know them. out: a face
  // function (everything drawn as WORLD faces, level 0), or { face, model }.
  function object(inputs, place, out) {
    const v = state.v, m = state.m;
    for (let i = 0; i < objectInputs.length; i++) {
      const x = inputs[objectInputs[i]];
      v[i] = x === undefined ? unset[i] : +x;
    }
    v[detailSlot] = 0;
    for (let i = 0; i < 12; i++) m[i] = place[i];
    m[12] = handedness(m, 0);
    state.r = state.g = state.b = 255;
    state.glow = false;
    state.face = typeof out === "function" ? out : out.face;
    state.model = typeof out === "function" ? null : out.model || null;
    state.sketch = typeof out === "function" ? null : out.sketch || null;
    state.owner = inputs.owner || null;
    state.out = out;
    state.view = out.view || null;
    state.nudge = 0;
    state.outline[0] = 0;
    state.inked[0] = 0;
    run(state);
    if (state.inked[0]) out.outline(0, 0, 0, 0);   // no ink edge left on for what follows
  }
  object.meshes = meshes;
  object.sketches = sketches;
  object.parts = outer.length;
  return object;
}

const identity = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1];

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

// A baked mesh in the ASSET layout: vertices (x y z), and per face four ids
// (a triangle repeats its third), the unlit rgb, and a unit normal — zero for
// a glowing face, which a host draws unlit.
function builder() {
  const vertices = [], faces = [], ids = new Map();
  let radius = 0;
  const id = (w, o) => {
    const key = `${w[o]},${w[o + 1]},${w[o + 2]}`;
    let i = ids.get(key);
    if (i === undefined) {
      i = vertices.length / 3;
      ids.set(key, i);
      vertices.push(w[o], w[o + 1], w[o + 2]);
      radius = Math.max(radius, Math.hypot(w[o], w[o + 1], w[o + 2]));
    }
    return i;
  };
  return {
    add(w, n, glow, r, g, b, nx, ny, nz) {
      const a = id(w, 0), bb = id(w, 3), c = id(w, 6);
      faces.push(a, bb, c, n === 4 ? id(w, 9) : c, r, g, b, glow ? 0 : nx, glow ? 0 : ny, glow ? 0 : nz);
    },
    done: () => ({ vertices: Float64Array.from(vertices), faces: Float64Array.from(faces),
      count: faces.length / 10, radius }),
  };
}

const world = new Float64Array(12);
function put(m, at, o, x, y, z) {
  for (let k = 0; k < 3; k++) world[o + k] = m[at + k] + x * m[at + 3 + k] + y * m[at + 6 + k] + z * m[at + 9 + k];
}
// A triangle (n 3) or quad (n 4) already in `world`, wound to face out: shaded
// by the game's rule and emitted, or kept for a bake. No area, nothing drawn.
function finish(s, n) {
  const w = world;
  const ux = w[3] - w[0], uy = w[4] - w[1], uz = w[5] - w[2];
  const vx = w[6] - w[0], vy = w[7] - w[1], vz = w[8] - w[2];
  const nx = uy * vz - uz * vy, ny = uz * vx - ux * vz, nz = ux * vy - uy * vx;
  const length = Math.hypot(nx, ny, nz);
  if (!length) return;
  if (s.rec) { s.rec.add(w, n, s.glow, s.r, s.g, s.b, nx / length, ny / length, nz / length); return; }
  let r = s.r, g = s.g, b = s.b;
  if (!s.glow) {
    const toward = -(nx * sun[0] + ny * sun[1] + nz * sun[2]) / length;
    const k = .72 + (toward > 0 ? toward : 0) * .28;
    r = Math.round(r * k); g = Math.round(g * k); b = Math.round(b * k);
  }
  s.face(w[0], w[1], w[2], w[3], w[4], w[5], w[6], w[7], w[8], r, g, b);
  if (n === 4) s.face(w[0], w[1], w[2], w[6], w[7], w[8], w[9], w[10], w[11], r, g, b);
}
function tri(s, at, ax, ay, az, bx, by, bz, cx, cy, cz) {
  const m = s.m;
  put(m, at, 0, ax, ay, az);
  if (m[at + 12] < 0) { put(m, at, 3, cx, cy, cz); put(m, at, 6, bx, by, bz); }
  else { put(m, at, 3, bx, by, bz); put(m, at, 6, cx, cy, cz); }
  finish(s, 3);
}
// A quad is two faces lit as one, off its first three corners, as worldQuad
// does. A corner on an axis (a revolve's cap) makes it a triangle.
function quad(s, at, ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz) {
  const same = (x1, y1, z1, x2, y2, z2) => x1 === x2 && y1 === y2 && z1 === z2;
  if (same(ax, ay, az, bx, by, bz)) return tri(s, at, ax, ay, az, cx, cy, cz, dx, dy, dz);
  if (same(bx, by, bz, cx, cy, cz) || same(cx, cy, cz, dx, dy, dz)) return tri(s, at, ax, ay, az, bx, by, bz, dx, dy, dz);
  if (same(dx, dy, dz, ax, ay, az)) return tri(s, at, ax, ay, az, bx, by, bz, cx, cy, cz);
  const m = s.m;
  put(m, at, 0, ax, ay, az);
  if (m[at + 12] < 0) { put(m, at, 3, dx, dy, dz); put(m, at, 6, cx, cy, cz); put(m, at, 9, bx, by, bz); }
  else { put(m, at, 3, bx, by, bz); put(m, at, 6, cx, cy, cz); put(m, at, 9, dx, dy, dz); }
  finish(s, 4);
}
// Sides asked for (at least `least`), or picked by radius and level — the
// only place a level changes a shape.
const levelShare = [1, .67, .5];
const sidesFor = (s, r, v, i, least = 3) => v.length > i ? Math.min(64, Math.max(least, Math.floor(v[i])))
  : Math.max(3, Math.round((r < 6 ? 6 : r < 13 ? 8 : r < 26 ? 12 : 16) * levelShare[s.v[detailSlot]]));

const primitives = {
  tri: (s, at, v) => tri(s, at, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]),
  quad: (s, at, v) => quad(s, at, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11]),
  // (disc r [sides]) — flat, at the origin, facing +z.
  disc: (s, at, v) => {
    const r = v[0], n = sidesFor(s, r, v, 1);
    for (let i = 0; i < n; i++) {
      const a = i / n * Math.PI * 2, b = (i + 1) / n * Math.PI * 2;
      tri(s, at, 0, 0, 0, Math.cos(a) * r, Math.sin(a) * r, 0, Math.cos(b) * r, Math.sin(b) * r, 0);
    }
  },
  // (hoop inner outer [sides]) — a flat ring facing +z.
  hoop: (s, at, v) => {
    const r1 = v[0], r2 = v[1], n = sidesFor(s, r2, v, 2);
    for (let i = 0; i < n; i++) {
      const a = i / n * Math.PI * 2, b = (i + 1) / n * Math.PI * 2;
      const ca = Math.cos(a), sa = Math.sin(a), cb = Math.cos(b), sb = Math.sin(b);
      quad(s, at, ca * r1, sa * r1, 0, ca * r2, sa * r2, 0, cb * r2, sb * r2, 0, cb * r1, sb * r1, 0);
    }
  },
  // (band radius width [sides] [turn]) — a tube's outside around z; `turn`
  // (0–1) draws only that much of it, from +x toward +y.
  band: (s, at, v) => {
    const r = v[0], h = v[1] / 2, n = sidesFor(s, r, v, 2, 1), turn = v.length > 3 ? v[3] : 1;
    for (let i = 0; i < n; i++) {
      const a = i / n * Math.PI * 2 * turn, b = (i + 1) / n * Math.PI * 2 * turn;
      const ca = Math.cos(a) * r, sa = Math.sin(a) * r, cb = Math.cos(b) * r, sb = Math.sin(b) * r;
      quad(s, at, ca, sa, -h, cb, sb, -h, cb, sb, h, ca, sa, h);
    }
  },
  // (capsule x1 y1 z1 x2 y2 z2 width [sides]) — a rod between two points.
  capsule: (s, at, v) => rod(s, at, v, v[6], v.length > 7 ? v[7] : 6),
  // (line x1 y1 z1 x2 y2 z2 [width]) — a thin three-sided rod.
  line: (s, at, v) => rod(s, at, v, v.length > 6 ? v[6] : 1.5, 3),
};

// A closed (radius, height) profile turned about an axis. Walked either way:
// it is turned counter-clockwise (radius right, height up) so faces face out.
// An odd count leads with `turn`, the share of a full circle to sweep.
const corner = new Float64Array(12);
function revolve(s, at, axis, v) {
  const turn = v.length % 2 ? v[0] : 1, from = v.length % 2, n = (v.length - from) / 2;
  let area = 0, reach = 0;
  for (let i = 0; i < n; i++) {
    const r = v[from + i * 2], h = v[from + i * 2 + 1];
    const r2 = v[from + ((i + 1) % n) * 2], h2 = v[from + ((i + 1) % n) * 2 + 1];
    area += r * h2 - r2 * h;
    reach = Math.max(reach, r);
  }
  const sides = Math.max(1, Math.round(sidesFor(s, reach, [], 0) * turn));
  const u = (axis + 1) % 3, w = (axis + 2) % 3;
  const point = (o, angle, r, h) => {
    corner[o + u] = Math.cos(angle) * r; corner[o + w] = Math.sin(angle) * r; corner[o + axis] = h;
  };
  for (let e = 0; e < n; e++) {
    const p = area < 0 ? n - 1 - e : e, q = area < 0 ? (2 * n - 2 - e) % n : (e + 1) % n;
    const r1 = v[from + p * 2], h1 = v[from + p * 2 + 1], r2 = v[from + q * 2], h2 = v[from + q * 2 + 1];
    if (!r1 && !r2) continue;
    for (let i = 0; i < sides; i++) {
      const a = i / sides * Math.PI * 2 * turn, b = (i + 1) / sides * Math.PI * 2 * turn;
      point(0, a, r1, h1); point(3, b, r1, h1); point(6, b, r2, h2); point(9, a, r2, h2);
      const c = corner;
      quad(s, at, c[0], c[1], c[2], c[3], c[4], c[5], c[6], c[7], c[8], c[9], c[10], c[11]);
    }
  }
}

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

// ——— flat shapes ———
// Anchors go through the camera the frame program already carries (the
// CAMERA op's 24 numbers, `out.view`), by frame-vm's own projection, and come
// out as 2D ops with one flat depth each: out.disc, out.capsule,
// out.ellipse, out.plate, out.outline. No lighting; ink is the colour.

const seen = new Float64Array(4 * 16);
// Project a point of the current frame: x, y, depth, and px per world unit.
function see(s, at, x, y, z, o) {
  put(s.m, at, 0, x, y, z);
  const V = s.view;
  const dx = world[0] - V[0], dy = world[1] - V[1], dz = world[2] - V[2];
  const vz = dx * V[9] + dy * V[10] + dz * V[11];
  if (vz < V[19]) return false;
  const vx = dx * V[3] + dy * V[4] + dz * V[5], vy = dx * V[6] + dy * V[7] + dz * V[8];
  const k = V[14] + (V[15] / vz - V[14]) * V[16];
  seen[o] = V[12] + vx * k; seen[o + 1] = V[13] - vy * k; seen[o + 2] = vz; seen[o + 3] = k;
  return true;
}
const depthOf = (s, vz) => {
  const z = (vz + s.nudge) * s.view[17] + s.view[18];
  return z < -1.499 ? -1.499 : z > 1.4 ? 1.4 : z;
};
// How big a unit of this frame is in world units (its axes' mean length).
function frameSize(m, at) {
  const ax = m[at + 3], ay = m[at + 4], az = m[at + 5], bx = m[at + 6], by = m[at + 7], bz = m[at + 8];
  const cx = m[at + 9], cy = m[at + 10], cz = m[at + 11];
  return Math.cbrt(Math.abs(ax * (by * cz - bz * cy) - ay * (bx * cz - bz * cx) + az * (bx * cy - by * cx)));
}
// Px per unit of this frame at its origin.
function scaleAt(s, at) { return see(s, at, 0, 0, 0, 0) ? seen[3] * frameSize(s.m, at) : 0; }
// A circle about `axis`, `along` it from the frame's origin, as its centre
// (seen 0) and two conjugate half-axes (the points at seen 4 and seen 8).
function circle(s, at, axis, r, along = 0) {
  const u = (axis + 1) % 3, w = (axis + 2) % 3, c = [0, 0, 0];
  c[axis] = along;
  const p = c.slice(), q = c.slice();
  p[u] = r; q[w] = r;
  return see(s, at, c[0], c[1], c[2], 0) && see(s, at, p[0], p[1], p[2], 4) && see(s, at, q[0], q[1], q[2], 8);
}
// A sketch being baked: records in the SHAPES layout (frame-vm.mjs, op 18),
// anchors in the part's own space.
function sketcher() {
  const records = [];
  let count = 0;
  return {
    add(s, kind, ...geometry) {
      const f = s.facing || [0, 0, 0];
      records.push(kind, s.outline[0], s.outline[1], s.outline[2], s.outline[3], s.nudge,
        s.r, s.g, s.b, f[0], f[1], f[2], ...geometry);
      count++;
    },
    done: () => ({ count, records: Float64Array.from(records) }),
  };
}
const SHAPE = { ball: 1, limb: 2, ring: 3, plate: 4, drum: 5 };
// A point of the frame, in the part's space (baking) — into world[o…].
const at3 = (s, at, x, y, z) => { put(s.m, at, 0, x, y, z); return [world[0], world[1], world[2]]; };
// An axis of the frame times a length: a vector in the part's space.
const axis3 = (s, at, axis, length) => [0, 1, 2].map((k) => s.m[at + 3 + axis * 3 + k] * length);
const recorders = {
  ball: (s, at, v) => s.sketching.add(s, SHAPE.ball, ...at3(s, at, v[0], v[1], v[2]), v[3] * frameSize(s.m, at)),
  limb: (s, at, v) => s.sketching.add(s, SHAPE.limb, ...at3(s, at, v[0], v[1], v[2]), ...at3(s, at, v[3], v[4], v[5]),
    v[6] * frameSize(s.m, at)),
  ring: (s, at, v, axis) => s.sketching.add(s, SHAPE.ring, ...at3(s, at, 0, 0, 0),
    ...axis3(s, at, (axis + 1) % 3, v[0]), ...axis3(s, at, (axis + 2) % 3, v[0])),
  drum: (s, at, v, axis) => s.sketching.add(s, SHAPE.drum, ...at3(s, at, 0, 0, 0),
    ...axis3(s, at, (axis + 1) % 3, v[0]), ...axis3(s, at, (axis + 2) % 3, v[0]), ...axis3(s, at, axis, v[1] / 2)),
  stroke: (s, at, v) => {
    for (let i = 1; i + 5 < v.length; i += 3)
      s.sketching.add(s, SHAPE.limb, ...at3(s, at, v[i], v[i + 1], v[i + 2]), ...at3(s, at, v[i + 3], v[i + 4], v[i + 5]),
        v[0] / 2 * frameSize(s.m, at));
  },
  plate: (s, at, v) => {
    const n = Math.min(16, Math.floor(v.length / 3)), points = [];
    for (let i = 0; i < n; i++) points.push(...at3(s, at, v[i * 3], v[i * 3 + 1], v[i * 3 + 2]));
    s.sketching.add(s, SHAPE.plate, n, ...points);
  },
  // A box baked as its six faces, each one-sided: the host shows the three
  // turned its way, each outlined, which reads as a drawn box.
  slab: (s, at, v) => {
    const lo = [v[0], v[1], v[2]], hi = [v[3], v[4], v[5]], was = s.facing;
    for (let axis = 0; axis < 3; axis++) for (const end of [lo, hi]) {
      const u = (axis + 1) % 3, w = (axis + 2) % 3, sign = end === hi ? 1 : -1;
      const corner = (a, b) => { const c = [0, 0, 0]; c[axis] = end[axis]; c[u] = a ? hi[u] : lo[u]; c[w] = b ? hi[w] : lo[w]; return at3(s, at, ...c); };
      s.facing = axis3(s, at, axis, sign * Math.sign(hi[axis] - lo[axis] || 1));
      s.sketching.add(s, SHAPE.plate, 4, ...corner(0, 0), ...corner(1, 0), ...corner(1, 1), ...corner(0, 1));
    }
    s.facing = was;
  },
};
// The ink edge in effect, sent as an OUTLINE op only when a shape drawn this
// tick needs a different one than the host already has.
function inkUp(s) {
  const o = s.outline, sent = s.inked;
  if (o[0] === sent[0] && (!o[0] || (o[1] === sent[1] && o[2] === sent[2] && o[3] === sent[3]))) return;
  sent[0] = o[0]; sent[1] = o[1]; sent[2] = o[2]; sent[3] = o[3];
  s.out.outline(o[0], o[1], o[2], o[3]);
}
const flatShapes = {
  // (ball x y z r)
  ball: (s, at, v) => {
    if (!see(s, at, v[0], v[1], v[2], 0)) return;
    // As an ELLIPSE, the fan a baked ball gets, so both paths draw it alike.
    const rad = v[3] * seen[3] * frameSize(s.m, at);
    s.out.ellipse(seen[0], seen[1], depthOf(s, seen[2]), rad, 0, 0, rad, s.r, s.g, s.b);
  },
  // (limb x1 y1 z1 x2 y2 z2 r): a stadium between two ends
  limb: (s, at, v) => {
    if (!see(s, at, v[0], v[1], v[2], 0) || !see(s, at, v[3], v[4], v[5], 4)) return;
    s.out.capsule(seen[0], seen[1], seen[4], seen[5], depthOf(s, (seen[2] + seen[6]) / 2),
      v[6] * (seen[3] + seen[7]) * frameSize(s.m, at), s.r, s.g, s.b);
  },
  // (ring axis r): a circle about the axis, as its projected ellipse
  ring: (s, at, v, axis) => {
    if (!circle(s, at, axis, v[0])) return;
    s.out.ellipse(seen[0], seen[1], depthOf(s, seen[2]), seen[4] - seen[0], seen[5] - seen[1],
      seen[8] - seen[0], seen[9] - seen[1], s.r, s.g, s.b);
  },
  // (drum axis r width): a cylinder as its silhouette — the far end's
  // ellipse, the band between the two ends' tangent points, the near end's
  // ellipse — each at its own depth, so the near end covers the band.
  drum: (s, at, v, axis) => {
    const caps = [];
    for (const side of [-1, 1]) {
      if (!circle(s, at, axis, v[0], side * v[1] / 2)) return;
      caps.push({ x: seen[0], y: seen[1], vz: seen[2], ax: seen[4] - seen[0], ay: seen[5] - seen[1],
        bx: seen[8] - seen[0], by: seen[9] - seen[1] });
    }
    caps.sort((p, q) => q.vz - p.vz);
    const [far, near] = caps, cap = (c) => s.out.ellipse(c.x, c.y, depthOf(s, c.vz), c.ax, c.ay, c.bx, c.by, s.r, s.g, s.b);
    cap(far);
    // Where each end's ellipse runs parallel to the drum's length.
    const dx = near.x - far.x, dy = near.y - far.y;
    if (Math.hypot(dx, dy) > .5) {
      const tangent = (c) => {
        const t = Math.atan2(c.bx * dy - c.by * dx, c.ax * dy - c.ay * dx);
        return [c.ax * Math.cos(t) + c.bx * Math.sin(t), c.ay * Math.cos(t) + c.by * Math.sin(t)];
      };
      const [fx, fy] = tangent(far), [nx, ny] = tangent(near);
      s.out.plate(4, [far.x + fx, far.y + fy, near.x + nx, near.y + ny, near.x - nx, near.y - ny,
        far.x - fx, far.y - fy], depthOf(s, (far.vz + near.vz) / 2), s.r, s.g, s.b);
    }
    cap(near);
  },
  // (stroke w x y z x y z …): a thick polyline, w world units wide
  stroke: (s, at, v) => {
    for (let i = 1; i + 5 < v.length; i += 3) {
      if (!see(s, at, v[i], v[i + 1], v[i + 2], 0) || !see(s, at, v[i + 3], v[i + 4], v[i + 5], 4)) continue;
      s.out.capsule(seen[0], seen[1], seen[4], seen[5], depthOf(s, (seen[2] + seen[6]) / 2),
        v[0] * (seen[3] + seen[7]) / 2 * frameSize(s.m, at), s.r, s.g, s.b);
    }
  },
  // (plate x y z …): a flat polygon through projected points
  plate: (s, at, v) => {
    const n = Math.min(16, Math.floor(v.length / 3)), points = [];
    let vz = 0;
    for (let i = 0; i < n; i++) {
      if (!see(s, at, v[i * 3], v[i * 3 + 1], v[i * 3 + 2], 0)) return;
      points.push(seen[0], seen[1]);
      vz += seen[2];
    }
    s.out.plate(n, points, depthOf(s, vz / n), s.r, s.g, s.b);
  },
  // (slab x1 y1 z1 x2 y2 z2): a box as its silhouette — the hull of its
  // eight projected corners, one flat plate.
  slab: (s, at, v) => {
    const corners = [];
    let vz = 0;
    for (let i = 0; i < 8; i++) {
      if (!see(s, at, v[i & 1 ? 3 : 0], v[i & 2 ? 4 : 1], v[i & 4 ? 5 : 2], 0)) return;
      corners.push([seen[0], seen[1]]);
      vz += seen[2];
    }
    const hull = convexHull(corners);
    s.out.plate(hull.length, hull.flat(), depthOf(s, vz / 8), s.r, s.g, s.b);
  },
};
// Andrew's monotone chain.
function convexHull(points) {
  const p = points.slice().sort((a, b) => a[0] - b[0] || a[1] - b[1]);
  const cross = (o, a, b) => (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0]);
  const lower = [], upper = [];
  for (const q of p) { while (lower.length > 1 && cross(lower.at(-2), lower.at(-1), q) <= 0) lower.pop(); lower.push(q); }
  for (const q of p.reverse()) { while (upper.length > 1 && cross(upper.at(-2), upper.at(-1), q) <= 0) upper.pop(); upper.push(q); }
  return lower.slice(0, -1).concat(upper.slice(0, -1));
}
