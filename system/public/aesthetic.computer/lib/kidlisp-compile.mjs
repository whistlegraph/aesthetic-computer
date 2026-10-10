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

import { compileKernel, runKernel, instantiateKernelWasm, kernelGPUDevice, createKernelGPU } from "./kidlisp-kernel.mjs";

export class CompileError extends Error {}

// (run kernel from [to]): the kernel over every live slot of a pool. Inputs
// are the slot's fields, uniforms the program's globals (or width/height/
// frame), outputs the fields of `to` (default: the same pool). Wasm when the
// host has it, the JavaScript runner otherwise; the numbers are the same.
export function runKernelOverPool(plan, backend, lisp, api, fromName, toName) {
  const from = lisp.pools.get(fromName), to = lisp.pools.get(toName || fromName);
  if (!from || !to) return 0;
  const nIn = plan.inputs.length, nUni = plan.uniforms.length, nOut = plan.outputs.length, stride = nIn + nUni + nOut;
  const inIdx = plan.inputs.map((name) => from.fields.indexOf(name)), outIdx = plan.outputs.map((name) => to.fields.indexOf(name));
  if (inIdx.includes(-1) || outIdx.includes(-1)) return 0;
  const uniforms = plan.uniforms.map((name) => { const v = name === "width" ? api.screen?.width : name === "height" ? api.screen?.height : name === "frame" ? api.paintCount : lisp.globalDef[name]; return typeof v === "number" ? v : 0; });
  const slots = [];
  for (let s = 0; s < from.cap; s++) if (from.alive[s]) slots.push(s);
  const count = slots.length;
  if (!count) return 0;
  const rows = plan._rows && plan._rows.length >= count * stride ? plan._rows : (plan._rows = new Float64Array(Math.max(count, 64) * stride));
  const fn = from.fields.length, tn = to.fields.length;
  for (let r = 0; r < count; r++) {
    const base = r * stride, at = slots[r] * fn;
    for (let i = 0; i < nIn; i++) rows[base + i] = from.data[at + inIdx[i]];
    for (let i = 0; i < nUni; i++) rows[base + nIn + i] = uniforms[i];
  }
  if (backend) backend.run(rows, count, uniforms, lisp.execution); else runKernel(plan, rows, count, uniforms, lisp.execution);
  for (let r = 0; r < count; r++) { const base = r * stride, at = slots[r] * tn; for (let i = 0; i < nOut; i++) to.data[at + outIdx[i]] = rows[base + nIn + nUni + i]; }
  return count;
}
// (gpu kernel from [to]): the same map as `run`, dispatched to the GPU. The
// results of a dispatch land in the pool on the next frame that calls it,
// one frame late by design, which the source says by using `gpu` and not
// `run`. Without WebGPU, or while the device warms up, it is `run`.
export function runKernelOnGPU(plan, entry, lisp, api, fromName, toName) {
  const from = lisp.pools.get(fromName), to = lisp.pools.get(toName || fromName);
  if (!from || !to) return 0;
  if (!entry.gpu && !entry.gpuRequested) { entry.gpuRequested = true; kernelGPUDevice().then((device) => { if (device) try { entry.gpu = createKernelGPU(plan, device); } catch (error) { console.warn("Kernel " + plan.name + " stays on the CPU: " + (error?.message || error)); } }); }
  if (!entry.gpu) return runKernelOverPool(plan, entry.backend, lisp, api, fromName, toName);
  const nIn = plan.inputs.length, nUni = plan.uniforms.length, nOut = plan.outputs.length, stride = nIn + nUni + nOut;
  const inIdx = plan.inputs.map((name) => from.fields.indexOf(name)), outIdx = plan.outputs.map((name) => to.fields.indexOf(name));
  if (inIdx.includes(-1) || outIdx.includes(-1)) return 0;
  const tn = to.fields.length;
  // Land what the last dispatch produced, if the pool still has those slots.
  const landed = entry.landed;
  if (landed && landed.key === fromName + ">" + (toName || fromName)) {
    for (let r = 0; r < landed.count; r++) { const slot = landed.slots[r]; if (slot < to.cap && to.alive[slot]) { const base = r * stride, at = slot * tn; for (let i = 0; i < nOut; i++) to.data[at + outIdx[i]] = landed.out[base + nIn + nUni + i]; } }
    entry.landed = null;
  }
  if (entry.gpu.busy) return 0;
  const uniforms = plan.uniforms.map((name) => { const v = name === "width" ? api.screen?.width : name === "height" ? api.screen?.height : name === "frame" ? api.paintCount : lisp.globalDef[name]; return typeof v === "number" ? v : 0; });
  const slots = [];
  for (let s = 0; s < from.cap; s++) if (from.alive[s]) slots.push(s);
  const count = slots.length;
  if (!count) return 0;
  const rows = new Float64Array(count * stride), fn = from.fields.length;
  for (let r = 0; r < count; r++) { const base = r * stride, at = slots[r] * fn; for (let i = 0; i < nIn; i++) rows[base + i] = from.data[at + inIdx[i]]; for (let i = 0; i < nUni; i++) rows[base + nIn + i] = uniforms[i]; }
  lisp.execution?.consume(["kernel-gpu"], count);
  const key = fromName + ">" + (toName || fromName);
  entry.gpu.run(rows, count, uniforms).then((out) => { if (out) entry.landed = { key, count, slots, out }; }).catch((error) => console.warn("Kernel " + plan.name + " dispatch failed: " + (error?.message || error)));
  return count;
}
export function kernelBackend(plan) {
  if (typeof WebAssembly === "undefined") return null;
  try { return instantiateKernelWasm(plan); } catch (error) { console.warn("Kernel " + plan.name + " runs in JavaScript: " + (error?.message || error)); return null; }
}

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
// Drawing heads the GPU frame records (gpu-frame.mjs); ink runs on the CPU too, for its colour parsing.
const GPU_HEADS = new Set(["wipe", "ink", "line", "box", "circle", "oval", "tri", "shape", "write", "plot", "point"]);
const isTimerHead = (head) => typeof head === "number" || (typeof head === "string" && /^\d*\.?\d+s(?:!|\.{2,3})?$/.test(head));
const unquote = (s) => (typeof s === "string" && /^".*"$/s.test(s) ? s.slice(1, -1) : s);
const NOTE = /^[a-g][#b]?[0-9]$/i;

export function compileProgram(ast, lisp) {
  const g = lisp.globalDef;                                  // shared with the interpreter
  const env = lisp.getGlobalEnv();                            // the drawing and sound table
  const rt = { killing: false, fns: new Map(), kernels: lisp.kernels || (lisp.kernels = new Map()) };
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
    if (typeof expr === "number") { const c = () => expr; c.numeric = true; c.const = expr; return c; }
    if (typeof expr === "string") {
      if (/^".*"$/s.test(expr)) { const v = expr.slice(1, -1); return () => v; }
      const i = slotFor(sc, expr);
      if (i >= 0) { const r = (f) => f[i]; r.slot = i; return r; }
      if (SCREEN[expr]) { const fn = SCREEN[expr]; return (f, api) => fn(api); }
      // A global the program defines reads straight from the shared store.
      if (globalNames.has(expr)) return () => g[expr];
      // Otherwise a word the interpreter would answer (a note, a color), or the word itself.
      return () => (expr in g ? g[expr] : typeof env[expr] === "function" && !(expr in ARITH) ? env[expr](lisp.api, []) : expr);
    }
    if (!Array.isArray(expr) || !expr.length) return () => undefined;
    const [head, ...args] = expr;
    if (typeof head !== "string") throw new CompileError("timer form inside a function: " + JSON.stringify(expr).slice(0, 60));
    if (ARITH[head]) {
      const parts = args.map((a) => compile(a, sc));
      const fast = arith(head, parts);
      if (fast) { fast.numeric = true; return fast; }
      const op = ARITH[head];
      const slow = (f, api) => { const vals = new Array(parts.length); for (let k = 0; k < parts.length; k++) vals[k] = num(parts[k](f, api)); return op(vals); };
      slow.numeric = true; return slow;
    }
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
      if (head === "def") return (f, api) => { if (!(name in g)) g[name] = value(f, api); return g[name]; };
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
      let fieldIdx = null, forPool = null;
      return (f, api) => {
        const pool = lisp.pools.get(name); if (!pool) return -1;
        if (forPool !== pool) { forPool = pool; fieldIdx = settings.map(([field]) => pool.fields.indexOf(field)); }
        let slot = -1;
        for (let k = 0; k < pool.cap; k++) if (!pool.alive[k]) { slot = k; break; }
        if (slot < 0) { slot = 0; for (let k = 1; k < pool.cap; k++) if (pool.born[k] < pool.born[slot]) slot = k; } else pool.count++;
        pool.alive[slot] = 1; pool.born[slot] = ++pool.serial;
        const n = pool.fields.length, base = slot * n;
        for (let k = 0; k < n; k++) pool.data[base + k] = 0;
        for (let k = 0; k < settings.length; k++) { const fi = fieldIdx[k]; if (fi >= 0) pool.data[base + fi] = num(settings[k][1](f, api)); }
        lisp.execution?.consume(["spawn"], 1);
        return slot;
      };
    }
    if (head === "each") {
      const name = args[0]; const decl = poolDecl(name); if (!decl) throw new CompileError("each over an undeclared pool " + name);
      const slots = decl.fields.map((field) => slotOf(sc, field)), slotIdx = slotOf(sc, "slot");
      // Which fields the body mentions, and which it assigns: the rest are neither copied in nor written back.
      const mentioned = new Set(), assigned = new Set();
      (function scan(form) { if (typeof form === "string") mentioned.add(form); if (!Array.isArray(form)) return; if ((form[0] === "def" || form[0] === "now") && typeof form[1] === "string") assigned.add(form[1]); form.forEach(scan); })(args.slice(1));
      const reads = decl.fields.map((field, k) => (mentioned.has(field) ? k : -1)).filter((k) => k >= 0);
      const writes = decl.fields.map((field, k) => (assigned.has(field) ? k : -1)).filter((k) => k >= 0);
      const kills = mentioned.has("kill");
      sc.loops++;
      const body = args.slice(1).map((a) => compile(a, sc));
      sc.loops--;
      const bodyLength = body.length;
      return (f, api) => {
        const pool = lisp.pools.get(name); if (!pool) return 0;
        const n = pool.fields.length, order = pool.order && pool.order.length ? pool.order : null, limit = order ? order.length : pool.cap;
        const data = pool.data, alive = pool.alive;
        lisp.execution?.consume(["each"], pool.cap);
        let visited = 0;
        for (let step = 0; step < limit; step++) {
          const slot = order ? order[step] : step;
          if (!alive[slot]) continue;
          visited++;
          const at = slot * n;
          for (let r = 0; r < reads.length; r++) { const k = reads[r]; f[slots[k]] = data[at + k]; }
          f[slotIdx] = slot;
          if (kills) {
            rt.killing = false;
            for (let b = 0; b < bodyLength; b++) { body[b](f, api); if (rt.killing) break; }
            if (rt.killing) { alive[slot] = 0; pool.count = Math.max(0, pool.count - 1); rt.killing = false; continue; }
          } else for (let b = 0; b < bodyLength; b++) body[b](f, api);
          for (let w = 0; w < writes.length; w++) { const k = writes[w], v = f[slots[k]]; if (typeof v === "number") data[at + k] = v; }
        }
        return visited;
      };
    }
    if (head === "kill") return () => { rt.killing = true; return 0; };
    if (head === "alive") return () => lisp.pools.get(args[0])?.count ?? 0;
    if (head === "empty") return () => { const pool = lisp.pools.get(args[0]); if (pool) { pool.alive.fill(0); pool.count = 0; } return 0; };
    if (head === "rank") return (f, api) => env.rank(api, args);
    if (head === "kernel") { const plan = compileKernel(expr); rt.kernels.set(plan.name, { plan, backend: kernelBackend(plan) }); return () => 0; }
    if (head === "run") {
      const [kname, fromName, toName] = args;
      return (f, api) => { const k = rt.kernels.get(kname); return k ? runKernelOverPool(k.plan, k.backend, lisp, api, fromName, toName) : 0; };
    }
    if (head === "gpu") {
      const [kname, fromName, toName] = args;
      return (f, api) => { const k = rt.kernels.get(kname); return k ? runKernelOnGPU(k.plan, k, lisp, api, fromName, toName) : 0; };
    }
    if (head === "key") return () => (lisp.keysDown.has(String(unquote(args[0] ?? "")).toLowerCase()) ? 1 : 0);
    if (head === "pad") return () => { const v = lisp.padState[String(unquote(args[0] ?? "")).toLowerCase()]; return typeof v === "number" ? v : 0; };
    if (head === "shape" && args.length === 1 && typeof args[0] === "string" && poolDecl(args[0])) {
      const name = args[0];
      return (f, api) => {
        if (!lisp.gpuActive) return env.shape(api, args);
        const pool = lisp.pools.get(name); if (!pool) return;
        const n = pool.fields.length, order = pool.order && pool.order.length ? pool.order : null, limit = order ? order.length : pool.cap, points = [];
        for (let step = 0; step < limit; step++) { const slot = order ? order[step] : step; if (pool.alive[slot]) points.push(pool.data[slot * n], pool.data[slot * n + 1]); }
        if (points.length >= 6) lisp.gpuFrame.shape(points, lisp.fillMode !== false ? 1 : 0);
      };
    }
    if (head === "random") { const parts = args.map((a) => compile(a, sc)); return (f, api) => env.random(api, parts.map((p) => p(f, api))); }
    // The GPU path: when the piece asked for it and the host has a renderer,
    // the drawing heads record into the frame instead of rasterizing. ink
    // still runs on the CPU so names, fades and `?` resolve as they always
    // have; the resolved colour is read back. Text and anything else stays
    // on the CPU and is composited over the frame.
    if (lisp.gpuFrame && GPU_HEADS.has(head)) {
      const parts = args.map((a) => compile(a, sc));
      const n = parts.length;
      const cpu = env[head];
      if (typeof cpu !== "function") throw new CompileError("unknown form " + head);
      const vals = () => { const out = new Array(n); return out; };
      const num = (v) => (typeof v === "number" ? v : typeof v === "boolean" ? (v ? 1 : 0) : 0);
      return (f, api) => {
        const v = vals(); for (let k = 0; k < n; k++) v[k] = parts[k](f, api);
        if (!lisp.gpuActive) return cpu(api, v, undefined);
        const frame = lisp.gpuFrame, fill = lisp.fillMode !== false ? 1 : 0;
        switch (head) {
          case "wipe": { cpu(api, v, undefined); const c = api.inkrn?.() || [0, 0, 0, 255]; frame.clear(c[0], c[1], c[2], 255); api.wipe?.(0, 0, 0, 0); return; }
          case "ink": { const out = cpu(api, v, undefined); const c = api.inkrn?.(); if (c) frame.ink(c[0], c[1], c[2], c[3] ?? 255); return out; }
          case "line": if (n >= 4 && v.slice(0, 4).every((x) => typeof x === "number")) { frame.line(v[0], v[1], v[2], v[3], typeof v[4] === "number" ? v[4] : 1); return; } break;
          case "box": if (n >= 4 && v.slice(0, 4).every((x) => typeof x === "number")) { frame.box(v[0], v[1], v[2], v[3], v[4] === "outline" || /^outline/.test(String(v[4] ?? "")) ? 0 : fill); return; } break;
          case "circle": if (n >= 3 && v.slice(0, 3).every((x) => typeof x === "number")) { frame.circle(v[0], v[1], v[2], v[3] === "outline" || /^outline/.test(String(v[3] ?? "")) ? 0 : fill); return; } break;
          case "oval": if (n >= 4 && v.slice(0, 4).every((x) => typeof x === "number")) { frame.oval(v[0], v[1], v[2], v[3], v[4] === "outline" || /^outline/.test(String(v[4] ?? "")) ? 0 : fill); return; } break;
          case "tri": if (n >= 6 && v.slice(0, 6).every((x) => typeof x === "number")) { frame.tri(v[0], v[1], v[2], v[3], v[4], v[5], v[6] === "outline" || /^outline/.test(String(v[6] ?? "")) ? 0 : fill); return; } break;
          case "shape": if (n >= 6 && n % 2 === 0 && v.every((x) => typeof x === "number")) { frame.shape(v, fill); return; } break;
        }
        // Not a shape the frame takes (a named colour fill, text): the CPU draws it, over the frame.
        frame.overlay = true;
        return cpu(api, v, undefined);
      };
    }
    // A piece function.
    if (rt.fns.has(head) || definesFunction(head)) {
      const parts = args.map((a) => compile(a, sc));
      let fn = null;
      const arity = parts.length;
      return (f, api) => {
        if (fn === null) { fn = rt.fns.get(head); if (!fn) return undefined; }
        const free = fn.free;
        const nf = free.length ? free.pop() : new Array(fn.size);
        for (let k = 0; k < arity; k++) nf[k] = parts[k](f, api);
        const out = fn.body(nf, api);
        free.push(nf);
        return out;
      };
    }
    if (RAW_ANYWHERE.has(head)) { const fn = env[head]; if (typeof fn !== "function") throw new CompileError("unknown form " + head); return (f, api) => fn(api, args, undefined); }
    if (DELEGATE_TOP.has(head) || isTimerHead(head)) throw new CompileError(head + " inside a function");
    // A line holding one word parses as a call of it; a known variable there is a read.
    if (!args.length && (globalNames.has(head) || slotFor(sc, head) >= 0)) return compile(head, sc);
    // The global table, with evaluated arguments: drawing, colour, text, sound.
    const fn = env[head];
    if (typeof fn !== "function") throw new CompileError("unknown form " + head);
    const parts = args.map((a) => compile(a, sc));
    const n = parts.length;
    return (f, api) => { const vals = new Array(n); for (let k = 0; k < n; k++) vals[k] = parts[k](f, api); return fn(api, vals, undefined); };
  }
  const num = (v) => (typeof v === "number" ? v : typeof v === "boolean" ? (v ? 1 : 0) : 0);
  // The common shapes without an argument array: two-operand + - * / and
  // the one-argument functions, numbers guarded inline.
  // + - * with a slot or a constant on either side read the value in place:
  // no getter call for the operand, and the number guard folded in.
  const G = (v) => (typeof v === "number" ? v : typeof v === "boolean" ? (v ? 1 : 0) : 0);
  function binary(head, a, b) {
    const as = a.slot, bs = b.slot, ac = a.const, bc = b.const;
    const aSlot = as !== undefined, bSlot = bs !== undefined, aConst = ac !== undefined, bConst = bc !== undefined;
    if (head === "+") {
      if (aSlot && bSlot) return (f) => G(f[as]) + G(f[bs]);
      if (aSlot && bConst) return (f) => G(f[as]) + bc;
      if (aConst && bSlot) return (f) => ac + G(f[bs]);
      if (aSlot) return (f, api) => G(f[as]) + G(b(f, api));
      if (bSlot) return (f, api) => G(a(f, api)) + G(f[bs]);
      if (aConst) return (f, api) => ac + G(b(f, api));
      if (bConst) return (f, api) => G(a(f, api)) + bc;
      return (f, api) => G(a(f, api)) + G(b(f, api));
    }
    if (head === "-") {
      if (aSlot && bSlot) return (f) => G(f[as]) - G(f[bs]);
      if (aSlot && bConst) return (f) => G(f[as]) - bc;
      if (aConst && bSlot) return (f) => ac - G(f[bs]);
      if (aSlot) return (f, api) => G(f[as]) - G(b(f, api));
      if (bSlot) return (f, api) => G(a(f, api)) - G(f[bs]);
      if (aConst) return (f, api) => ac - G(b(f, api));
      if (bConst) return (f, api) => G(a(f, api)) - bc;
      return (f, api) => G(a(f, api)) - G(b(f, api));
    }
    if (head === "*") {
      if (aSlot && bSlot) return (f) => G(f[as]) * G(f[bs]);
      if (aSlot && bConst) return (f) => G(f[as]) * bc;
      if (aConst && bSlot) return (f) => ac * G(f[bs]);
      if (aSlot) return (f, api) => G(f[as]) * G(b(f, api));
      if (bSlot) return (f, api) => G(a(f, api)) * G(f[bs]);
      if (aConst) return (f, api) => ac * G(b(f, api));
      if (bConst) return (f, api) => G(a(f, api)) * bc;
      return (f, api) => G(a(f, api)) * G(b(f, api));
    }
    return null;
  }
  function arith(head, parts) {
    const [a, b, c] = parts;
    const n = (p) => (p.numeric ? p : (f, api) => { const v = p(f, api); return typeof v === "number" ? v : typeof v === "boolean" ? (v ? 1 : 0) : 0; });
    if (parts.length === 1) {
      const x = n(a);
      switch (head) {
        case "sin": return (f, api) => Math.sin(x(f, api));
        case "cos": return (f, api) => Math.cos(x(f, api));
        case "abs": return (f, api) => Math.abs(x(f, api));
        case "sqrt": return (f, api) => { const v = x(f, api); return v >= 0 ? Math.sqrt(v) : 0; };
        case "floor": return (f, api) => Math.floor(x(f, api));
        case "ceil": return (f, api) => Math.ceil(x(f, api));
        case "round": return (f, api) => Math.round(x(f, api));
        case "exp": return (f, api) => Math.exp(x(f, api));
        case "sign": return (f, api) => Math.sign(x(f, api));
        case "tan": return (f, api) => Math.tan(x(f, api));
        case "-": return (f, api) => -x(f, api);
        default: return null;
      }
    }
    if (parts.length === 2) {
      const inlined = binary(head, a, b);
      if (inlined) return inlined;
      const x = n(a), y = n(b);
      switch (head) {
        case "+": return (f, api) => x(f, api) + y(f, api);
        case "-": return (f, api) => x(f, api) - y(f, api);
        case "*": return (f, api) => x(f, api) * y(f, api);
        case "/": return (f, api) => { const d = y(f, api); return d !== 0 ? x(f, api) / d : 0; };
        case "%": case "mod": return (f, api) => { const d = y(f, api); return d !== 0 ? x(f, api) % d : 0; };
        case "max": return (f, api) => Math.max(x(f, api), y(f, api));
        case "min": return (f, api) => Math.min(x(f, api), y(f, api));
        case "pow": return (f, api) => Math.pow(x(f, api), y(f, api));
        case "atan2": return (f, api) => Math.atan2(x(f, api), y(f, api));
        case "hypot": return (f, api) => Math.hypot(x(f, api), y(f, api));
        default: return null;
      }
    }
    if (parts.length === 3) {
      const x = n(a), y = n(b), z = n(c);
      switch (head) {
        case "+": return (f, api) => x(f, api) + y(f, api) + z(f, api);
        case "-": return (f, api) => x(f, api) - y(f, api) - z(f, api);
        case "*": return (f, api) => x(f, api) * y(f, api) * z(f, api);
        case "clamp": return (f, api) => Math.max(y(f, api), Math.min(z(f, api), x(f, api)));
        default: return null;
      }
    }
    if (parts.length === 4) {
      const x = n(a), y = n(b), z = n(c), w = n(parts[3]);
      switch (head) {
        case "+": return (f, api) => x(f, api) + y(f, api) + z(f, api) + w(f, api);
        case "-": return (f, api) => x(f, api) - y(f, api) - z(f, api) - w(f, api);
        case "*": return (f, api) => x(f, api) * y(f, api) * z(f, api) * w(f, api);
        default: return null;
      }
    }
    return null;
  }

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
    const entry = { arity: params.length, size: 0, body: null, free: [] };
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
