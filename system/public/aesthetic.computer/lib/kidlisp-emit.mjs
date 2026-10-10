// A source emitter for KidLisp pieces: the closure compiler
// (kidlisp-compile.mjs) as text. Same scopes, same slots, same guards, same
// runtime objects (the interpreter's pools, globals, drawing table and
// frame), but the program comes out as one JavaScript function with locals
// instead of a tree of closures, so an engine without a JIT (QuickJS on the
// console) runs bytecode instead of a call per node.
//
// This is a trusted-host step, run where the piece is already being packed
// as a script the host built (kidlisp/tools/native-tv-script.mjs). The
// worker keeps plans as data and never evaluates generated source.
//
//   const text = emitProgram(ast, lisp);            // JavaScript, a factory
//   const make = new Function("H", text);            // on the host
//   lisp.compiled = make(programHelpers(lisp));      // { run(api) }
//
// Every form the closure compiler accepts is emitted; anything else throws
// CompileError, as there.

import { compileKernel } from "./kidlisp-kernel.mjs";
import { CompileError, kernelBackend, runKernelOverPool, runKernelOnGPU } from "./kidlisp-compile.mjs";

const ARITH_NAMES = new Set(["+", "-", "*", "/", "%", "mod", "mul", "max", "min", "sin", "cos", "tan", "abs", "sqrt", "floor", "ceil", "round", "exp", "pow", "sign", "atan2", "hypot", "clamp"]);
const COMPARE_NAMES = { ">": ">", "<": "<", "=": "===" };
const SCREEN = { width: "(api.screen?.width ?? 0)", w: "(api.screen?.width ?? 0)", height: "(api.screen?.height ?? 0)", h: "(api.screen?.height ?? 0)", frame: "(api.paintCount || 0)", f: "(api.paintCount || 0)" };
const DELEGATE_TOP = new Set(["mesh", "tap", "draw", "lift", "once", "melody", "clock", "later", "jump", "hop", "delay", "trans", "net", "source", "choose", "?", "bake", "embed", "fps", "resolution", "die", "mic", "speaker", "overtone", "amplitude"]);
const RAW_ANYWHERE = new Set(["hum", "tune", "hush", "pluck", "bell", "sub", "flute", "hat", "voice"]);
const GPU_HEADS = new Set(["wipe", "ink", "line", "box", "circle", "oval", "tri", "shape", "write", "plot", "point", "camera", "place"]);
const isTimerHead = (head) => typeof head === "number" || (typeof head === "string" && /^\d*\.?\d+s(?:!|\.{2,3})?$/.test(head));
const unquote = (s) => (typeof s === "string" && /^".*"$/s.test(s) ? s.slice(1, -1) : s);
const lit = (v) => JSON.stringify(v);
const prop = (name) => (/^[A-Za-z_$][\w$]*$/.test(name) ? "." + name : "[" + lit(name) + "]");

export function emitProgram(ast, lisp) {
  const literals = [];                                   // forms and argument lists the program needs as data
  const literal = (value) => { literals.push(value); return `L[${literals.length - 1}]`; };
  const out = { fns: [], top: [] };
  const fnNames = new Map();                             // piece function → JavaScript name
  let kernelCount = 0;

  // ---- scopes (as in the closure compiler) --------------------------------
  function scope(isFunction) { return { names: new Map(), size: 0, isFunction, loops: 0, declared: new Set() }; }
  function slotFor(sc, name) { const i = lookup(sc, name); return i >= 0 && (sc.isFunction || sc.loops > 0) ? i : -1; }
  function slotOf(sc, name) { if (sc.names.has(name)) return sc.names.get(name); const i = sc.size++; sc.names.set(name, i); return i; }
  function lookup(sc, name) { return sc.names.has(name) ? sc.names.get(name) : -1; }
  const local = (sc, i) => (sc.isFunction ? "s" : "t") + i;
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
  const poolDecls = new Map();
  (function findPools(forms) { for (const form of forms) { if (!Array.isArray(form)) continue; if (form[0] === "pool" && typeof form[1] === "string") poolDecls.set(form[1], { fields: form.slice(3).filter((f) => typeof f === "string") }); findPools(form.filter(Array.isArray)); } })(ast);
  function poolDecl(name) { return poolDecls.get(name); }
  const globalNames = new Set();
  for (const form of ast) if (Array.isArray(form) && (form[0] === "def" || form[0] === "now") && typeof form[1] === "string") globalNames.add(form[1]);
  const fnForms = new Map();
  for (const form of ast) if (Array.isArray(form) && form[0] === "later" && typeof form[1] === "string") fnForms.set(form[1], form);
  let fnIndex = 0;
  for (const name of fnForms.keys()) fnNames.set(name, "fn" + fnIndex++ + "_" + name.replace(/\W/g, "_"));

  // ---- expressions ---------------------------------------------------------
  // val(expr) → { code, numeric, const, slot }: a JavaScript expression.
  // stmt(expr) → JavaScript statements; the value of the last one lands in `r`
  // when `want` is set (so if/repeat/function bodies yield what the closure
  // compiler yields).
  const N = (p) => (p.numeric ? p.code : `G(${p.code})`);
  function val(expr, sc) {
    if (typeof expr === "number") return { code: Number.isFinite(expr) && expr < 0 ? `(${expr})` : String(expr), numeric: true, const: expr };
    if (typeof expr === "string") {
      if (/^".*"$/s.test(expr)) return { code: lit(expr.slice(1, -1)) };
      const i = slotFor(sc, expr);
      if (i >= 0) return { code: local(sc, i), slot: i };
      if (SCREEN[expr]) return { code: SCREEN[expr], numeric: true };
      if (globalNames.has(expr)) return { code: "g" + prop(expr) };
      return { code: `word(${lit(expr)})` };
    }
    if (!Array.isArray(expr) || !expr.length) return { code: "undefined" };
    const [head, ...args] = expr;
    if (typeof head !== "string") throw new CompileError("timer form inside a function: " + JSON.stringify(expr).slice(0, 60));
    if (ARITH_NAMES.has(head)) {
      const parts = args.map((a) => val(a, sc));
      const fast = arith(head, parts);
      if (fast) return { code: fast, numeric: true };
      return { code: `A${prop(head)}([${parts.map(N).join(", ")}])`, numeric: true };
    }
    if (COMPARE_NAMES[head]) {
      if (args.length !== 2) throw new CompileError(head + " with a body");
      return { code: `(${val(args[0], sc).code} ${COMPARE_NAMES[head]} ${val(args[1], sc).code})` };
    }
    if (head === "not") return { code: `(!${val(args[0], sc).code})` };
    if (head === "def" || head === "now") {
      const name = args[0]; if (typeof name !== "string") throw new CompileError("def of a non-name");
      const value = val(args[1], sc);
      const i = slotFor(sc, name);
      if (i >= 0) return { code: `(${local(sc, i)} = ${value.code})` };
      if (head === "def") return { code: `((${lit(name)} in g) ? g${prop(name)} : (g${prop(name)} = ${value.code}))` };
      return { code: `(g${prop(name)} = ${value.code})` };
    }
    if (head === "kill") return { code: "(rt.killing = true, 0)" };
    if (head === "alive") return { code: `(lisp.pools.get(${lit(args[0])})?.count ?? 0)`, numeric: true };
    if (head === "empty") return { code: `H.empty(${lit(args[0])})`, numeric: true };
    if (head === "rank") return { code: `env.rank(api, ${literal(args)})` };
    if (head === "run") return { code: `H.run(${lit(args[0])}, ${lit(args[1])}, ${lit(args[2] ?? null)}, api)`, numeric: true };
    if (head === "gpu") return { code: `H.gpu(${lit(args[0])}, ${lit(args[1])}, ${lit(args[2] ?? null)}, api)`, numeric: true };
    if (head === "key") return { code: `(lisp.keysDown.has(${lit(String(unquote(args[0] ?? "")).toLowerCase())}) ? 1 : 0)`, numeric: true };
    if (head === "pad") return { code: `H.pad(${lit(String(unquote(args[0] ?? "")).toLowerCase())})`, numeric: true };
    if (head === "pool") return { code: `env.pool(api, ${literal(args)})` };
    if (head === "random") { const parts = args.map((a) => val(a, sc)); return { code: `env.random(api, [${parts.map((p) => p.code).join(", ")}])` }; }
    if (head === "shape" && args.length === 1 && typeof args[0] === "string" && poolDecl(args[0])) return { code: `H.shapePool(${lit(args[0])}, api, ${literal(args)})` };
    if (head === "spawn") {
      const name = args[0];
      const settings = args.slice(1).filter((s) => Array.isArray(s) && s.length >= 2);
      return { code: `H.spawn(${lit(name)}, ${literal(settings.map((s) => s[0]))}, [${settings.map((s) => N(val(s[1], sc))).join(", ")}])`, numeric: true };
    }
    if (head === "if" || head === "repeat" || head === "each" || head === "kernel") {
      // statement forms in a value position: a block that leaves its value in r
      return { code: `(() => { let r; ${stmt(expr, sc, true)} return r; })()` };
    }
    if (lisp.gpuFrame && GPU_HEADS.has(head)) {
      const parts = args.map((a) => val(a, sc));
      if (typeof lisp.getGlobalEnv()[head] !== "function") throw new CompileError("unknown form " + head);
      return { code: `H.draw${prop(head)}(api, [${parts.map((p) => p.code).join(", ")}])` };
    }
    if (fnNames.has(head)) { const parts = args.map((a) => val(a, sc)); return { code: `${fnNames.get(head)}(api${parts.map((p) => ", " + p.code).join("")})` }; }
    if (RAW_ANYWHERE.has(head)) { if (typeof lisp.getGlobalEnv()[head] !== "function") throw new CompileError("unknown form " + head); return { code: `env${prop(head)}(api, ${literal(args)}, undefined)` }; }
    if (DELEGATE_TOP.has(head) || isTimerHead(head)) throw new CompileError(head + " inside a function");
    if (!args.length && (globalNames.has(head) || slotFor(sc, head) >= 0)) return val(head, sc);
    if (typeof lisp.getGlobalEnv()[head] !== "function") throw new CompileError("unknown form " + head);
    const parts = args.map((a) => val(a, sc));
    return { code: `env${prop(head)}(api, [${parts.map((p) => p.code).join(", ")}], undefined)` };
  }
  function arith(head, parts) {
    const [a, b, c] = parts;
    if (parts.length === 1) {
      const x = N(a);
      switch (head) {
        case "sin": case "cos": case "abs": case "floor": case "ceil": case "round": case "exp": case "sign": case "tan": return `Math.${head}(${x})`;
        case "sqrt": return `sqrt(${x})`;
        case "-": return `(-${x})`;
        default: return null;
      }
    }
    if (parts.length === 2) {
      const x = N(a), y = N(b);
      switch (head) {
        case "+": case "-": case "*": return `(${x} ${head} ${y})`;
        case "/": return `div(${x}, ${y})`;
        case "%": case "mod": return `rem(${x}, ${y})`;
        case "max": case "min": case "pow": case "atan2": case "hypot": return `Math.${head}(${x}, ${y})`;
        default: return null;
      }
    }
    if (parts.length === 3) {
      const x = N(a), y = N(b), z = N(c);
      switch (head) {
        case "+": case "-": case "*": return `(${x} ${head} ${y} ${head} ${z})`;
        case "clamp": return `Math.max(${y}, Math.min(${z}, ${x}))`;
        default: return null;
      }
    }
    if (parts.length === 4) {
      const x = N(a), y = N(b), z = N(c), w = N(parts[3]);
      switch (head) {
        case "+": case "-": case "*": return `(${x} ${head} ${y} ${head} ${z} ${head} ${w})`;
        default: return null;
      }
    }
    return null;
  }
  // Statements. `want`: leave the form's value in r.
  function stmt(expr, sc, want) {
    const set = (code) => (want ? `r = ${code};` : `${code};`);
    if (!Array.isArray(expr)) return set(val(expr, sc).code);
    const [head, ...args] = expr;
    if (head === "if") {
      const split = args.indexOf("else");
      const then = split < 0 ? args.slice(1) : args.slice(1, split), other = split < 0 ? [] : args.slice(split + 1);
      const body = (forms, none) => (forms.length ? forms.map((f) => stmt(f, sc, want)).join(" ") : want ? `r = ${none};` : "");
      return `if (${val(args[0], sc).code}) { ${body(then, "1")} } else { ${body(other, "0")} }`;
    }
    if (head === "repeat") {
      const count = N(val(args[0], sc));
      const hasIter = args.length >= 3 && typeof args[1] === "string";
      const i = hasIter ? slotOf(sc, args[1]) : -1;
      sc.loops++;
      const body = (hasIter ? args.slice(2) : args.slice(1)).map((f) => stmt(f, sc, want)).join(" ");
      sc.loops--;
      return `{ const n = Math.max(0, Math.min(10000, Math.floor(${count}))); for (let k = 0; k < n; k++) { ${hasIter ? `${local(sc, i)} = k; ` : ""}${body} } }`;
    }
    if (head === "each") {
      const name = args[0]; const decl = poolDecl(name); if (!decl) throw new CompileError("each over an undeclared pool " + name);
      const slots = decl.fields.map((field) => slotOf(sc, field)), slotIdx = slotOf(sc, "slot");
      const mentioned = new Set(), assigned = new Set();
      (function scan(form) { if (typeof form === "string") mentioned.add(form); if (!Array.isArray(form)) return; if ((form[0] === "def" || form[0] === "now") && typeof form[1] === "string") assigned.add(form[1]); form.forEach(scan); })(args.slice(1));
      const reads = decl.fields.map((field, k) => (mentioned.has(field) ? k : -1)).filter((k) => k >= 0);
      const writes = decl.fields.map((field, k) => (assigned.has(field) ? k : -1)).filter((k) => k >= 0);
      const kills = mentioned.has("kill");
      sc.loops++;
      const forms = args.slice(1).map((f) => stmt(f, sc, false));
      sc.loops--;
      const body = kills ? forms.map((f) => `${f} if (rt.killing) { alive[slot] = 0; pool.count = Math.max(0, pool.count - 1); rt.killing = false; continue; }`).join(" ") : forms.join(" ");
      return `{ const pool = lisp.pools.get(${lit(name)}); if (pool) { const n = pool.fields.length, order = pool.order && pool.order.length ? pool.order : null, limit = order ? order.length : pool.cap, data = pool.data, alive = pool.alive; lisp.execution?.consume(["each"], pool.cap); let visited = 0;` +
        ` for (let step = 0; step < limit; step++) { const slot = order ? order[step] : step; if (!alive[slot]) continue; visited++; const at = slot * n;` +
        ` ${reads.map((k) => `${local(sc, slots[k])} = data[at + ${k}];`).join(" ")} ${local(sc, slotIdx)} = slot; ${kills ? "rt.killing = false; " : ""}${body}` +
        ` ${writes.map((k) => `if (typeof ${local(sc, slots[k])} === "number") data[at + ${k}] = ${local(sc, slots[k])};`).join(" ")} }` +
        ` ${want ? "r = visited;" : ""} } ${want ? "else r = 0;" : ""} }`;
    }
    if (head === "kernel") {
      const plan = compileKernel(expr);
      const index = kernelCount++;
      out.kernels ||= [];
      out.kernels.push(`rt.kernels.set(${lit(plan.name)}, { plan: K[${index}], backend: H.kernelBackend(K[${index}]) });`);
      (out.plans ||= []).push(plan);
      return want ? "r = 0;" : "";
    }
    return set(val(expr, sc).code);
  }

  // ---- functions -----------------------------------------------------------
  for (const [name, form] of fnForms) {
    const params = []; let k = 2; while (k < form.length && !Array.isArray(form[k])) params.push(String(form[k++]));
    const bodyForms = form.slice(k);
    const sc = scope(true);
    for (const p of params) slotOf(sc, p);
    declare(bodyForms, sc);
    const body = bodyForms.map((b) => stmt(b, sc, true)).join("\n    ");
    const locals = []; for (let i = params.length; i < sc.size; i++) locals.push(local(sc, i));
    out.fns.push(`  function ${fnNames.get(name)}(api${params.map((_, i) => ", " + local(sc, i)).join("")}) {\n    let r;${locals.length ? ` let ${locals.join(", ")};` : ""}\n    ${body}\n    return r;\n  }`);
  }

  // ---- the top level -------------------------------------------------------
  const top = scope(false);
  declare(ast.filter((form) => Array.isArray(form) && form[0] !== "later"), top);
  const steps = ast.map((form) => {
    if (!Array.isArray(form)) return stmt(form, top, true);
    const [head] = form;
    if (head === "later" || DELEGATE_TOP.has(head) || isTimerHead(head)) return `r = lisp.evaluate(${literal(form)}, api);`;
    return stmt(form, top, true);
  });
  const topLocals = []; for (let i = 0; i < top.size; i++) topLocals.push(local(top, i));

  return `// emitted by kidlisp-emit.mjs from the piece's forms: one function, locals for slots
const { lisp, env, g, rt, G, A, word, div, rem, sqrt } = H;
const L = ${lit(literals)};
const K = ${lit(out.plans || [])};
${out.fns.join("\n")}
${topLocals.length ? `let ${topLocals.join(", ")};` : ""}
${(out.kernels || []).join("\n")}
return {
  run(api) {
    lisp.api = api; let r;
    ${steps.join("\n    ")}
    return r;
  },
  functions: ${fnForms.size}, steps: ${steps.length}, emitted: true,
};
`;
}

// The runtime the emitted program binds to: the same objects the closure
// compiler closes over, as one argument.
export function programHelpers(lisp) {
  const env = lisp.getGlobalEnv(), g = lisp.globalDef;
  const rt = { killing: false, kernels: lisp.kernels || (lisp.kernels = new Map()) };
  const G = (v) => (typeof v === "number" ? v : typeof v === "boolean" ? (v ? 1 : 0) : 0);
  const A = {
    "+": (a) => a.reduce((s, v) => s + v, 0), "-": (a) => (a.length === 1 ? -a[0] : a.slice(1).reduce((s, v) => s - v, a[0])), "*": (a) => a.reduce((s, v) => s * v, 1),
    "/": (a) => a.slice(1).reduce((s, v) => (v !== 0 ? s / v : 0), a[0]), "%": (a) => (a[1] !== 0 ? a[0] % a[1] : 0), mod: (a) => (a[1] !== 0 ? a[0] % a[1] : 0), mul: (a) => a.reduce((s, v) => s * v, 1),
    max: (a) => Math.max(...a), min: (a) => Math.min(...a), sin: (a) => Math.sin(a[0]), cos: (a) => Math.cos(a[0]), tan: (a) => Math.tan(a[0]), abs: (a) => Math.abs(a[0]), sqrt: (a) => (a[0] >= 0 ? Math.sqrt(a[0]) : 0),
    floor: (a) => Math.floor(a[0]), ceil: (a) => Math.ceil(a[0]), round: (a) => Math.round(a[0]), exp: (a) => Math.exp(a[0]), pow: (a) => Math.pow(a[0], a[1] ?? 1), sign: (a) => Math.sign(a[0]),
    atan2: (a) => Math.atan2(a[0], a[1]), hypot: (a) => Math.hypot(...a), clamp: (a) => Math.max(a[1] ?? 0, Math.min(a[2] ?? 1, a[0])),
  };
  const word = (name) => (name in g ? g[name] : typeof env[name] === "function" && !(name in A) ? env[name](lisp.api, []) : name);
  const div = (x, y) => (y !== 0 ? x / y : 0), rem = (x, y) => (y !== 0 ? x % y : 0), sqrt = (v) => (v >= 0 ? Math.sqrt(v) : 0);
  const spawnIdx = new Map();
  const outline = (v) => v === "outline" || /^outline/.test(String(v ?? ""));
  const numbers = (v, n) => { for (let k = 0; k < n; k++) if (typeof v[k] !== "number") return false; return true; };
  const draw = {};
  for (const head of GPU_HEADS) {
    const cpu = env[head];
    draw[head] = (api, v) => {
      if (!lisp.gpuActive) return cpu(api, v, undefined);
      const frame = lisp.gpuFrame, fill = lisp.fillMode !== false ? 1 : 0, n = v.length;
      switch (head) {
        case "wipe": { cpu(api, v, undefined); const c = api.inkrn?.() || [0, 0, 0, 255]; frame.clear(c[0], c[1], c[2], 255); api.wipe?.(0, 0, 0, 0); return; }
        case "ink": { const out = cpu(api, v, undefined); const c = api.inkrn?.(); if (c) frame.ink(c[0], c[1], c[2], c[3] ?? 255); return out; }
        case "line": if (n >= 4 && numbers(v, 4)) { frame.line(v[0], v[1], v[2], v[3], typeof v[4] === "number" ? v[4] : 1); return; } break;
        case "box": if (n >= 4 && numbers(v, 4)) { frame.box(v[0], v[1], v[2], v[3], outline(v[4]) ? 0 : fill); return; } break;
        case "circle": if (n >= 3 && numbers(v, 3)) { frame.circle(v[0], v[1], v[2], outline(v[3]) ? 0 : fill); return; } break;
        case "oval": if (n >= 4 && numbers(v, 4)) { frame.oval(v[0], v[1], v[2], v[3], outline(v[4]) ? 0 : fill); return; } break;
        case "tri": if (n >= 6 && numbers(v, 6)) { frame.tri(v[0], v[1], v[2], v[3], v[4], v[5], outline(v[6]) ? 0 : fill); return; } break;
        case "shape": if (n >= 6 && n % 2 === 0 && numbers(v, n)) { frame.shape(v, fill); return; } break;
        case "camera": if (n >= 3 && numbers(v, 3)) { frame.camera(v[0], v[1], v[2], G(v[3]), G(v[4]), typeof v[5] === "number" ? v[5] : 60, typeof v[6] === "number" ? v[6] : 1); return; } break;
        case "place": { const mesh = lisp.meshes?.get(String(v[0])); if (mesh && n >= 4 && typeof v[1] === "number" && typeof v[2] === "number" && typeof v[3] === "number") { if (!frame.meshes.has(mesh.id)) frame.defineMesh(mesh.id, mesh); frame.place(mesh.id, v[1], v[2], v[3], G(v[4]), G(v[5]), G(v[6]), typeof v[7] === "number" ? v[7] : 1); } return; }
      }
      frame.overlay = true;
      return cpu(api, v, undefined);
    };
  }
  return {
    lisp, env, g, rt, G, A, word, div, rem, sqrt, draw,
    kernelBackend,
    run: (kname, from, to, api) => { const k = rt.kernels.get(kname); return k ? runKernelOverPool(k.plan, k.backend, lisp, api, from, to || undefined) : 0; },
    gpu: (kname, from, to, api) => { const k = rt.kernels.get(kname); return k ? runKernelOnGPU(k.plan, k, lisp, api, from, to || undefined) : 0; },
    pad: (name) => { const v = lisp.padState[name]; return typeof v === "number" ? v : 0; },
    empty: (name) => { const pool = lisp.pools.get(name); if (pool) { pool.alive.fill(0); pool.count = 0; } return 0; },
    shapePool: (name, api, args) => {
      if (!lisp.gpuActive) return env.shape(api, args);
      const pool = lisp.pools.get(name); if (!pool) return;
      const n = pool.fields.length, order = pool.order && pool.order.length ? pool.order : null, limit = order ? order.length : pool.cap, points = [];
      for (let step = 0; step < limit; step++) { const slot = order ? order[step] : step; if (pool.alive[slot]) points.push(pool.data[slot * n], pool.data[slot * n + 1]); }
      if (points.length >= 6) lisp.gpuFrame.shape(points, lisp.fillMode !== false ? 1 : 0);
    },
    spawn: (name, fields, values) => {
      const pool = lisp.pools.get(name); if (!pool) return -1;
      let idx = spawnIdx.get(fields); if (!idx || idx.pool !== pool) { idx = { pool, at: fields.map((field) => pool.fields.indexOf(field)) }; spawnIdx.set(fields, idx); }
      let slot = -1;
      for (let k = 0; k < pool.cap; k++) if (!pool.alive[k]) { slot = k; break; }
      if (slot < 0) { slot = 0; for (let k = 1; k < pool.cap; k++) if (pool.born[k] < pool.born[slot]) slot = k; } else pool.count++;
      pool.alive[slot] = 1; pool.born[slot] = ++pool.serial;
      const n = pool.fields.length, base = slot * n;
      for (let k = 0; k < n; k++) pool.data[base + k] = 0;
      for (let k = 0; k < values.length; k++) { const fi = idx.at[k]; if (fi >= 0) pool.data[base + fi] = values[k]; }
      lisp.execution?.consume(["spawn"], 1);
      return slot;
    },
  };
}
