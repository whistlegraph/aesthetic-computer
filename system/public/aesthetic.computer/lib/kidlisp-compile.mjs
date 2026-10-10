// A closure compiler for KidLisp pieces (kidlisp/PIECE-IL.md, measured in
// kidlisp/reports/whistlegraph-benchmark-2026-10-10.md).
//
// The reference evaluator walks the AST on every paint: each name is looked
// up by string, each function call copies an environment, each arithmetic
// node is dispatched through the global table. This compiles the program
// once, when the source changes, into a tree of JavaScript closures with
// names resolved to slots, and runs that tree every frame. No code is
// generated at runtime (SCORE.md §9: plans are data; nothing is evaluated
// as JavaScript); a closure is a prepared piece of the interpreter.
//
// Scope of this compiler, by the census: arithmetic, comparisons, if/else,
// def/now, later and calls, repeat, pools, the drawing and sound calls of
// the global table with evaluated arguments, width/height/frame and the
// hand. Forms that register behaviour or keep their own clock (tap, draw,
// lift, once, timers, hums) are handed to the interpreter at top level,
// unchanged. Anything else fails compilation, and the piece runs on the
// interpreter as before. Globals live in the interpreter's globalDef so a
// tap handler the interpreter runs sees the same values.

export class CompileError extends Error {}

const ARITH = {
  "+": (a) => a.reduce((s, v) => s + v, 0),
  "-": (a) => (a.length === 1 ? -a[0] : a.slice(1).reduce((s, v) => s - v, a[0])),
  "*": (a) => a.reduce((s, v) => s * v, 1),
  "/": (a) => a.slice(1).reduce((s, v) => (v !== 0 ? s / v : 0), a[0]),
  "%": (a) => (a[1] !== 0 ? a[0] % a[1] : 0),
  mod: (a) => (a[1] !== 0 ? a[0] % a[1] : 0),
  mul: (a) => a.reduce((s, v) => s * v, 1),
  max: (a) => Math.max(...a),
  min: (a) => Math.min(...a),
  sin: (a) => Math.sin(a[0]), cos: (a) => Math.cos(a[0]), tan: (a) => Math.tan(a[0]),
  abs: (a) => Math.abs(a[0]), sqrt: (a) => (a[0] >= 0 ? Math.sqrt(a[0]) : 0),
  floor: (a) => Math.floor(a[0]), ceil: (a) => Math.ceil(a[0]), round: (a) => Math.round(a[0]),
  exp: (a) => Math.exp(a[0]), pow: (a) => Math.pow(a[0], a[1] ?? 1), sign: (a) => Math.sign(a[0]),
  atan2: (a) => Math.atan2(a[0], a[1]), hypot: (a) => Math.hypot(...a),
  clamp: (a) => Math.max(a[1] ?? 0, Math.min(a[2] ?? 1, a[0])),
};
const COMPARE = {
  ">": (a, b) => a > b, "<": (a, b) => a < b, "=": (a, b) => a === b || (typeof a === "number" && typeof b === "number" && a === b),
};
// Zero-argument words the interpreter answers from the api.
const SCREEN = { width: (api) => api.screen?.width ?? 0, w: (api) => api.screen?.width ?? 0, height: (api) => api.screen?.height ?? 0, h: (api) => api.screen?.height ?? 0, frame: (api) => api.paintCount || 0, f: (api) => api.paintCount || 0 };
// Top-level forms that keep their own state or clock: the interpreter runs them as it always has.
const DELEGATE_TOP = new Set(["tap", "draw", "lift", "once", "melody", "clock", "later", "jump", "hop", "delay", "trans", "net", "source", "choose", "?", "bake", "embed", "fps", "resolution", "die", "mic", "speaker", "overtone", "amplitude"]);
// Forms that take their arguments raw and evaluate what they need themselves; usable inside functions.
const RAW_ANYWHERE = new Set(["hum", "tune", "hush", "pluck", "bell", "sub", "flute", "hat", "voice"]);
const isTimerHead = (head) => typeof head === "number" || (typeof head === "string" && /^\d*\.?\d+s(?:!|\.{2,3})?$/.test(head));
const unquote = (s) => (typeof s === "string" && /^".*"$/s.test(s) ? s.slice(1, -1) : s);
const NOTE = /^[a-g][#b]?[0-9]$/i;

export function compileProgram(ast, lisp) {
  const g = lisp.globalDef;                                  // shared with the interpreter
  const env = lisp.getGlobalEnv();                            // the drawing and sound table
  const rt = { killing: false, fns: new Map() };
  const topSlots = new Map();                                 // top-level locals: repeat iterators, each fields

  // ---- scopes -------------------------------------------------------------
  // A scope maps a name to a frame slot. Functions get a fresh frame; the top
  // level has one too, for iterators and pool fields.
  function scope(parent, isFunction) { return { names: new Map(), size: 0, isFunction, parent, loops: 0 }; }
  // At the top level a name is a frame slot only inside a repeat or each body
  // (the interpreter's localEnv exists only there); in a function every
  // declared name is a slot.
  function slotFor(sc, name) { const i = lookup(sc, name); return i >= 0 && (sc.isFunction || sc.loops > 0) ? i : -1; }
  function slotOf(sc, name) { if (sc.names.has(name)) return sc.names.get(name); const i = sc.size++; sc.names.set(name, i); return i; }
  function lookup(sc, name) { return sc.names.has(name) ? sc.names.get(name) : -1; }

  // Which names a body defines locally (def inside a function, repeat iterators, each fields).
  function declare(forms, sc) {
    for (const form of forms) {
      if (!Array.isArray(form)) continue;
      const [head] = form;
      if (head === "def" && sc.isFunction && typeof form[1] === "string") slotOf(sc, form[1]);
      if (head === "repeat" && form.length >= 4 && typeof form[2] === "string") { slotOf(sc, form[2]); declare(form.slice(3), sc); continue; }
      if (head === "each" && typeof form[1] === "string") { const pool = lisp.pools.get(form[1]) || poolDecl(form[1]); if (pool) for (const f of pool.fields) slotOf(sc, f); slotOf(sc, "slot"); declare(form.slice(2), sc); continue; }
      if (head === "if") { declare(form.slice(1), sc); continue; }
      if (head !== "later" && head !== "once" && !isTimerHead(head)) declare(form.slice(1).filter(Array.isArray), sc);
    }
  }
  // Pools declared anywhere in the program, so each can bind their fields before the pool exists at runtime.
  const poolDecls = new Map();
  (function findPools(forms) { for (const form of forms) { if (!Array.isArray(form)) continue; if (form[0] === "pool" && typeof form[1] === "string") poolDecls.set(form[1], { fields: form.slice(3).filter((f) => typeof f === "string") }); findPools(form.filter(Array.isArray)); } })(ast);
  function poolDecl(name) { return poolDecls.get(name); }
  // Names the program defines at its top level: globals.
  const globalNames = new Set();
  for (const form of ast) if (Array.isArray(form) && (form[0] === "def" || form[0] === "now") && typeof form[1] === "string") globalNames.add(form[1]);

  // ---- expressions ----------------------------------------------------------
  function compile(expr, sc) {
    if (typeof expr === "number") return () => expr;
    if (typeof expr === "string") {
      if (/^".*"$/s.test(expr)) { const v = expr.slice(1, -1); return () => v; }
      const i = slotFor(sc, expr);
      if (i >= 0) return (f) => f[i];
      if (SCREEN[expr]) { const fn = SCREEN[expr]; return (f, api) => fn(api); }
      // A global, or a word the interpreter would answer (a note, a color), or the word itself.
      return () => (expr in g ? g[expr] : typeof env[expr] === "function" && !(expr in ARITH) ? env[expr](lisp.api, []) : expr);
    }
    if (!Array.isArray(expr) || !expr.length) return () => undefined;
    const [head, ...args] = expr;
    if (typeof head !== "string") throw new CompileError("timer form inside a function: " + JSON.stringify(expr).slice(0, 60));
    if (ARITH[head]) { const op = ARITH[head], parts = args.map((a) => compile(a, sc)); return (f, api) => op(parts.map((p) => num(p(f, api)))); }
    if (COMPARE[head]) {
      if (args.length !== 2) throw new CompileError(head + " with a body");
      const op = COMPARE[head], a = compile(args[0], sc), b = compile(args[1], sc);
      return (f, api) => op(a(f, api), b(f, api));
    }
    if (head === "not") { const a = compile(args[0], sc); return (f, api) => !a(f, api); }
    if (head === "if") {
      const split = args.indexOf("else");
      const test = compile(args[0], sc);
      const then = (split < 0 ? args.slice(1) : args.slice(1, split)).map((a) => compile(a, sc));
      const other = (split < 0 ? [] : args.slice(split + 1)).map((a) => compile(a, sc));
      return (f, api) => { let out; if (test(f, api)) { out = then.length ? undefined : 1; for (const s of then) out = s(f, api); } else { out = other.length ? undefined : 0; for (const s of other) out = s(f, api); } return out; };
    }
    if (head === "def" || head === "now") {
      const name = args[0]; if (typeof name !== "string") throw new CompileError("def of a non-name");
      const value = compile(args[1], sc);
      const i = slotFor(sc, name);
      if (i >= 0) return (f, api) => (f[i] = value(f, api));
      if (head === "def") return (f, api) => { if (!Object.prototype.hasOwnProperty.call(g, name)) g[name] = value(f, api); return g[name]; };
      return (f, api) => (g[name] = value(f, api));
    }
    if (head === "repeat") {
      const count = compile(args[0], sc);
      const hasIter = args.length >= 3 && typeof args[1] === "string";
      const i = hasIter ? slotOf(sc, args[1]) : -1;
      sc.loops++;
      const body = (hasIter ? args.slice(2) : args.slice(1)).map((a) => compile(a, sc));
      sc.loops--;
      return (f, api) => { const n = Math.max(0, Math.min(10000, Math.floor(num(count(f, api))))); let out; for (let k = 0; k < n; k++) { if (hasIter) f[i] = k; for (const s of body) out = s(f, api); } return out; };
    }
    if (head === "pool") return (f, api) => env.pool(api, args);
    if (head === "spawn") {
      const name = args[0];
      const settings = args.slice(1).filter((s) => Array.isArray(s) && s.length >= 2).map((s) => [s[0], compile(s[1], sc)]);
      return (f, api) => {
        const pool = lisp.pools.get(name); if (!pool) return -1;
        let slot = -1;
        for (let k = 0; k < pool.cap; k++) if (!pool.alive[k]) { slot = k; break; }
        if (slot < 0) { slot = 0; for (let k = 1; k < pool.cap; k++) if (pool.born[k] < pool.born[slot]) slot = k; } else pool.count++;
        pool.alive[slot] = 1; pool.born[slot] = ++pool.serial;
        const n = pool.fields.length, base = slot * n;
        for (let k = 0; k < n; k++) pool.data[base + k] = 0;
        for (const [field, value] of settings) { const fi = pool.fields.indexOf(field); if (fi >= 0) pool.data[base + fi] = num(value(f, api)); }
        lisp.execution?.consume(["spawn"], 1);
        return slot;
      };
    }
    if (head === "each") {
      const name = args[0]; const decl = poolDecl(name); if (!decl) throw new CompileError("each over an undeclared pool " + name);
      const slots = decl.fields.map((field) => slotOf(sc, field)), slotIdx = slotOf(sc, "slot");
      sc.loops++;
      const body = args.slice(1).map((a) => compile(a, sc));
      sc.loops--;
      return (f, api) => {
        const pool = lisp.pools.get(name); if (!pool) return 0;
        const n = pool.fields.length, order = pool.order && pool.order.length ? pool.order : null, limit = order ? order.length : pool.cap;
        lisp.execution?.consume(["each"], pool.cap);
        let visited = 0;
        for (let step = 0; step < limit; step++) {
          const slot = order ? order[step] : step;
          if (!pool.alive[slot]) continue;
          visited++;
          const at = slot * n;
          for (let k = 0; k < n; k++) f[slots[k]] = pool.data[at + k];
          f[slotIdx] = slot;
          rt.killing = false;
          for (const s of body) { s(f, api); if (rt.killing) break; }
          if (rt.killing) { pool.alive[slot] = 0; pool.count = Math.max(0, pool.count - 1); rt.killing = false; }
          else for (let k = 0; k < n; k++) { const v = f[slots[k]]; if (typeof v === "number") pool.data[at + k] = v; }
        }
        return visited;
      };
    }
    if (head === "kill") return () => { rt.killing = true; return 0; };
    if (head === "alive") return () => lisp.pools.get(args[0])?.count ?? 0;
    if (head === "empty") return () => { const pool = lisp.pools.get(args[0]); if (pool) { pool.alive.fill(0); pool.count = 0; } return 0; };
    if (head === "rank") return (f, api) => env.rank(api, args);
    if (head === "key") return () => (lisp.keysDown.has(String(unquote(args[0] ?? "")).toLowerCase()) ? 1 : 0);
    if (head === "pad") return () => { const v = lisp.padState[String(unquote(args[0] ?? "")).toLowerCase()]; return typeof v === "number" ? v : 0; };
    if (head === "shape" && args.length === 1 && typeof args[0] === "string" && poolDecl(args[0])) return (f, api) => env.shape(api, args);
    if (head === "random") { const parts = args.map((a) => compile(a, sc)); return (f, api) => env.random(api, parts.map((p) => p(f, api))); }
    // A piece function.
    if (rt.fns.has(head) || definesFunction(head)) {
      const parts = args.map((a) => compile(a, sc));
      return (f, api) => { const fn = rt.fns.get(head); if (!fn) return undefined; const nf = new Array(fn.size); for (let k = 0; k < fn.arity; k++) nf[k] = parts[k] ? parts[k](f, api) : undefined; return fn.body(nf, api); };
    }
    if (RAW_ANYWHERE.has(head)) { const fn = env[head]; if (typeof fn !== "function") throw new CompileError("unknown form " + head); return (f, api) => fn(api, args, undefined); }
    if (DELEGATE_TOP.has(head) || isTimerHead(head)) throw new CompileError(head + " inside a function");
    // A line holding one word parses as a call of it; a known variable there is a read.
    if (!args.length && (globalNames.has(head) || slotFor(sc, head) >= 0)) return compile(head, sc);
    // The global table, with evaluated arguments: drawing, colour, text, sound.
    const fn = env[head];
    if (typeof fn !== "function") throw new CompileError("unknown form " + head);
    const parts = args.map((a) => compile(a, sc));
    return (f, api) => fn(api, parts.map((p) => p(f, api)), undefined);
  }
  const num = (v) => (typeof v === "number" ? v : typeof v === "boolean" ? (v ? 1 : 0) : 0);

  // ---- functions ----------------------------------------------------------
  const fnForms = new Map();
  for (const form of ast) if (Array.isArray(form) && form[0] === "later" && typeof form[1] === "string") fnForms.set(form[1], form);
  function definesFunction(name) { return fnForms.has(name); }
  for (const [name, form] of fnForms) {
    const params = []; let k = 2; while (k < form.length && !Array.isArray(form[k])) params.push(String(form[k++]));
    const bodyForms = form.slice(k);
    const sc = scope(null, true);
    for (const p of params) slotOf(sc, p);
    declare(bodyForms, sc);
    const entry = { arity: params.length, size: 0, body: null };
    rt.fns.set(name, entry);
    const body = bodyForms.map((b) => compile(b, sc));
    entry.size = sc.size;
    entry.body = (f, api) => { let out; for (const s of body) out = s(f, api); return out; };
  }

  // ---- the top level --------------------------------------------------------
  const top = scope(null, false);
  declare(ast.filter((form) => Array.isArray(form) && form[0] !== "later"), top);
  const steps = ast.map((form) => {
    if (!Array.isArray(form)) return compile(form, top);
    const [head] = form;
    if (head === "later" || DELEGATE_TOP.has(head) || isTimerHead(head)) return (f, api) => lisp.evaluate(form, api);
    return compile(form, top);
  });
  const frame = new Array(top.size);
  return {
    run(api) { lisp.api = api; let out; for (const s of steps) out = s(frame, api); return out; },
    functions: rt.fns.size, steps: steps.length,
  };
}
