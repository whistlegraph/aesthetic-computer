// Kernels: the WebGPU-safe subset of KidLisp (kidlisp/PIECE-IL.md §8).
//
// A kernel is a pure function over numbers, declared in the source:
//
//   (kernel project (in hx hy) (uniform ssc zz cT sT cP sP foc hcx hcy) (out px py)
//     (def xr (+ (* hx cT) (* zz sT)))
//     (def z0 (- (* zz cT) (* hx sT)))
//     (def ff (/ foc (- foc (+ (* hy sP) (* z0 cP)))))
//     (set px (+ hcx (* xr ff)))
//     (set py (+ hcy (* (- (* hy cP) (* z0 sP)) ff))))
//   (run project hp sp)
//
// `run` applies it to every live slot of a pool: inputs are that slot's
// fields, uniforms are the program's globals, outputs are fields of the
// same pool or of a second one with the same slots. The body is a sequence
// of def (a local) and set (an output) over the kernel-v1 operations: the
// audited arithmetic, the transcendental functions a shader has, and
// select. No calls, no drawing, no pools, no clocks, no random: that is
// what makes it safe to lower, and the same plan runs three ways —
//   JavaScript  runKernel(plan, …)            the reference, no allocation per element
//   Wasm        instantiateKernelWasm(plan)   one function, a loop over rows in linear memory
//   WGSL        emitKernelWGSL(plan)          a compute shader, one invocation per row
// — so a piece that leans on a kernel is already shaped for the GPU, and a
// host that has no GPU runs the same numbers on the CPU.
import { KidLispExecutionError } from "./kidlisp-execution.mjs";

const encode = (n) => (Number.isNaN(n) ? "NaN" : n === Infinity ? "+Infinity" : n === -Infinity ? "-Infinity" : Object.is(n, -0) ? "-0" : n);
const decode = (n) => (n === "NaN" ? NaN : n === "+Infinity" ? Infinity : n === "-Infinity" ? -Infinity : n === "-0" ? -0 : n);
const unsupported = (message, path) => { throw new KidLispExecutionError("UNSUPPORTED_KERNEL", message, { path }); };
const NAME = /^[A-Za-z_][\w]*$/;

// kernel-v1 operations: id, arity (null = any), the reference apply, the
// Wasm lowering (an opcode, or an import name), the WGSL spelling.
const op = (id, name, arity, apply, wasm, wgsl) => Object.freeze({ id, name, arity, apply, wasm, wgsl });
export const KERNEL_OPERATIONS = Object.freeze([
  op(1, "+", null, (a) => a.reduce((x, y) => x + y, 0), { fold: 0xa0, unit: 0 }, { infix: "+" , unit: "0.0" }),
  op(2, "-", null, (a) => (!a.length ? 0 : a.length === 1 ? -a[0] : a.slice(1).reduce((x, y) => x - y, a[0])), { fold: 0xa1, unit: 0, negate: 0x9a }, { infix: "-", unit: "0.0" }),
  op(3, "*", null, (a) => a.reduce((x, y) => x * y, 1), { fold: 0xa2, unit: 1 }, { infix: "*", unit: "1.0" }),
  op(4, "/", 2, (a) => (a[1] === 0 ? 0 : a[0] / a[1]), { import: "div" }, { call: "safeDiv" }),
  op(5, "%", 2, (a) => (a[1] === 0 ? 0 : a[0] % a[1]), { import: "rem" }, { call: "safeRem" }),
  op(6, "floor", 1, (a) => Math.floor(a[0]), { unary: 0x9c }, { fn: "floor" }),
  op(7, "ceil", 1, (a) => Math.ceil(a[0]), { unary: 0x9b }, { fn: "ceil" }),
  op(8, "round", 1, (a) => Math.round(a[0]), { import: "round" }, { call: "jsRound" }),
  op(9, "sin", 1, (a) => Math.sin(a[0]), { import: "sin" }, { fn: "sin" }),
  op(10, "cos", 1, (a) => Math.cos(a[0]), { import: "cos" }, { fn: "cos" }),
  op(11, "tan", 1, (a) => Math.tan(a[0]), { import: "tan" }, { fn: "tan" }),
  op(12, "sqrt", 1, (a) => (a[0] >= 0 ? Math.sqrt(a[0]) : 0), { import: "sqrt" }, { call: "safeSqrt" }),
  op(13, "abs", 1, (a) => Math.abs(a[0]), { unary: 0x99 }, { fn: "abs" }),
  op(14, "min", 2, (a) => Math.min(a[0], a[1]), { binary: 0xa4 }, { fn: "min" }),
  op(15, "max", 2, (a) => Math.max(a[0], a[1]), { binary: 0xa5 }, { fn: "max" }),
  op(16, "exp", 1, (a) => Math.exp(a[0]), { import: "exp" }, { fn: "exp" }),
  op(17, "pow", 2, (a) => Math.pow(a[0], a[1]), { import: "pow" }, { fn: "pow" }),
  op(18, "sign", 1, (a) => Math.sign(a[0]), { import: "sign" }, { fn: "sign" }),
  op(19, "atan2", 2, (a) => Math.atan2(a[0], a[1]), { import: "atan2" }, { fn: "atan2" }),
  op(20, "hypot", 2, (a) => Math.hypot(a[0], a[1]), { import: "hypot" }, { call: "hypot2" }),
  op(21, "clamp", 3, (a) => Math.max(a[1], Math.min(a[2], a[0])), { import: "clamp" }, { fn: "clamp" }),
  op(22, ">", 2, (a) => (a[0] > a[1] ? 1 : 0), { compare: 0x64 }, { compare: ">" }),
  op(23, "<", 2, (a) => (a[0] < a[1] ? 1 : 0), { compare: 0x63 }, { compare: "<" }),
  op(24, "=", 2, (a) => (a[0] === a[1] ? 1 : 0), { compare: 0x61 }, { compare: "==" }),
  op(25, "select", 3, (a) => (a[0] !== 0 ? a[1] : a[2]), { select: true }, { select: true }),
]);
const byName = new Map(KERNEL_OPERATIONS.map((o) => [o.name, o]));
const byId = new Map(KERNEL_OPERATIONS.map((o) => [o.id, o]));
byName.set("mod", byName.get("%")); byName.set("mul", byName.get("*"));
export const kernelOperationNames = () => [...byName.keys()];

// ---- the plan ------------------------------------------------------------
// Slots: inputs, then uniforms, then outputs, then locals. Code is a stack
// program of const / load slot / call id count / store slot.
export function compileKernel(form, { maxNodes = 20000 } = {}) {
  if (!Array.isArray(form) || form[0] !== "kernel" || typeof form[1] !== "string" || !NAME.test(form[1])) unsupported("A kernel is (kernel name (in …) (uniform …) (out …) body…)", []);
  const name = form[1];
  const lists = { in: [], uniform: [], out: [] };
  let k = 2;
  while (k < form.length && Array.isArray(form[k]) && lists[form[k][0]]) {
    const [kind, ...names] = form[k++];
    for (const n of names) { if (typeof n !== "string" || !NAME.test(n)) unsupported(`Bad ${kind} name ${String(n)} in kernel ${name}`, [k]); lists[kind].push(n); }
  }
  const body = form.slice(k);
  const inputs = lists.in, uniforms = lists.uniform, outputs = lists.out, locals = [];
  const slots = new Map();
  const declare = (n) => { if (slots.has(n)) unsupported(`${n} declared twice in kernel ${name}`, []); slots.set(n, slots.size); };
  for (const n of [...inputs, ...uniforms, ...outputs]) declare(n);
  if (!outputs.length) unsupported(`Kernel ${name} has no outputs`, []);
  const code = [];
  let visited = 0;
  const lower = (expr, path) => {
    if (++visited > maxNodes) unsupported("Kernel too large", path);
    if (typeof expr === "number") { code.push({ op: "const", value: encode(expr) }); return; }
    if (typeof expr === "string") {
      if (!slots.has(expr)) unsupported(`${expr} is not an input, uniform, output or local of kernel ${name}`, path);
      code.push({ op: "load", slot: slots.get(expr) }); return;
    }
    if (!Array.isArray(expr) || !expr.length || typeof expr[0] !== "string") unsupported("Expected a number, a name, or a kernel operation", path);
    const [head, ...args] = expr;
    if (head === "if") {
      const split = args.indexOf("else");
      if (split < 0 || args.length !== 4 || split !== 2) unsupported("A kernel if is (if test value else value)", path);
      lower(args[0], [...path, 1]); lower(args[1], [...path, 2]); lower(args[3], [...path, 4]);
      code.push({ op: "call", id: byName.get("select").id, count: 3 }); return;
    }
    if (head === "not") { if (args.length !== 1) unsupported("not takes one value", path); lower(args[0], [...path, 1]); code.push({ op: "const", value: 0 }); code.push({ op: "call", id: byName.get("=").id, count: 2 }); return; }
    const o = byName.get(head);
    if (!o) unsupported(`${head} is outside kernel-v1 (no calls, drawing, pools, clocks or random in a kernel)`, path);
    if (o.arity !== null && args.length !== o.arity) unsupported(`${head} takes ${o.arity} value${o.arity === 1 ? "" : "s"}`, path);
    args.forEach((a, i) => lower(a, [...path, i + 1]));
    code.push({ op: "call", id: o.id, count: args.length });
  };
  body.forEach((statement, i) => {
    if (!Array.isArray(statement) || (statement[0] !== "def" && statement[0] !== "set") || typeof statement[1] !== "string" || statement.length !== 3) unsupported("A kernel body is (def local value) and (set output value) forms", [i]);
    const [kind, target, value] = statement;
    if (kind === "set" && !outputs.includes(target)) unsupported(`set ${target}: not an output of kernel ${name}`, [i]);
    if (kind === "def" && (inputs.includes(target) || uniforms.includes(target) || outputs.includes(target))) unsupported(`def ${target}: already an input, uniform or output`, [i]);
    if (kind === "def" && !slots.has(target)) { declare(target); locals.push(target); }
    lower(value, [i, 2]);
    code.push({ op: "store", slot: slots.get(target) });
  });
  if (!code.length) unsupported(`Kernel ${name} has an empty body`, []);
  return { version: 1, profile: "kernel-v1", name, inputs, uniforms, outputs, locals, slots: slots.size, code, sourceNodes: visited };
}

// ---- JavaScript: the reference runner -------------------------------------
// Runs the plan for every row of `rows` (a Float64Array, stride = inputs +
// uniforms + outputs; uniforms repeated per row by the caller or supplied
// once). Locals live in a scratch array; the stack is preallocated.
export function runKernel(plan, rows, count, uniforms = null, execution) {
  const stride = plan.inputs.length + plan.uniforms.length + plan.outputs.length;
  const values = new Float64Array(plan.slots), stack = new Float64Array(64);
  const code = plan.code, nIn = plan.inputs.length, nUni = plan.uniforms.length, nOut = plan.outputs.length;
  execution?.consume(["kernel"], count);
  for (let row = 0; row < count; row++) {
    const base = row * stride;
    for (let i = 0; i < nIn; i++) values[i] = rows[base + i];
    for (let i = 0; i < nUni; i++) values[nIn + i] = uniforms ? uniforms[i] : rows[base + nIn + i];
    for (let i = 0; i < nOut; i++) values[nIn + nUni + i] = 0;
    let sp = 0;
    for (let pc = 0; pc < code.length; pc++) {
      const ins = code[pc];
      if (ins.op === "const") stack[sp++] = decode(ins.value);
      else if (ins.op === "load") stack[sp++] = values[ins.slot];
      else if (ins.op === "store") values[ins.slot] = stack[--sp];
      else {
        const o = byId.get(ins.id), n = ins.count;
        const args = Array.from(stack.subarray(sp - n, sp)); sp -= n;
        stack[sp++] = o.apply(args);
      }
    }
    for (let i = 0; i < nOut; i++) rows[base + nIn + nUni + i] = values[nIn + nUni + i];
  }
}

// ---- Wasm: one function, a loop over rows in linear memory ---------------------
const uleb = (n) => { const a = []; do { const b = n & 127; n >>>= 7; a.push(b | (n ? 128 : 0)); } while (n); return a; };
const sleb = (n) => { const a = []; let more = true; while (more) { let b = n & 127; n >>= 7; if ((n === 0 && !(b & 64)) || (n === -1 && (b & 64))) more = false; else b |= 128; a.push(b); } return a; };
const vector = (items) => [...uleb(items.length), ...items.flat()];
const string = (s) => { const b = [...new TextEncoder().encode(s)]; return [...uleb(b.length), ...b]; };
const section = (id, b) => [id, ...uleb(b.length), ...b];
const f64const = (value) => { const b = new Uint8Array(8); new DataView(b.buffer).setFloat64(0, decode(value), true); return [0x44, ...b]; };
const IMPORTS = { div: (a, b) => (b === 0 ? 0 : a / b), rem: (a, b) => (b === 0 ? 0 : a % b), round: Math.round, sin: Math.sin, cos: Math.cos, tan: Math.tan, sqrt: (a) => (a >= 0 ? Math.sqrt(a) : 0), exp: Math.exp, pow: Math.pow, sign: Math.sign, atan2: Math.atan2, hypot: Math.hypot, clamp: (v, lo, hi) => Math.max(lo, Math.min(hi, v)) };
const IMPORT_ARITY = { div: 2, rem: 2, round: 1, sin: 1, cos: 1, tan: 1, sqrt: 1, exp: 1, pow: 2, sign: 1, atan2: 2, hypot: 2, clamp: 3 };

export function compileKernelWasm(plan) {
  const nIn = plan.inputs.length, nUni = plan.uniforms.length, nOut = plan.outputs.length, stride = nIn + nUni + nOut;
  const imports = [...new Set(plan.code.filter((i) => i.op === "call" && byId.get(i.id).wasm.import).map((i) => byId.get(i.id).wasm.import))];
  const importIndex = new Map(imports.map((name, i) => [name, i]));
  // Function type: (count i32, uniforms f64…) → nothing. Locals: row base i32, slot values f64, a stack of f64 temporaries.
  const typeRun = [0x60, ...vector([[0x7f], ...Array.from({ length: nUni }, () => [0x7c])]), 0];
  const importTypes = imports.map((name) => [0x60, ...vector(Array.from({ length: IMPORT_ARITY[name] }, () => [0x7c])), 1, 0x7c]);
  const types = [...importTypes, typeRun];
  const runTypeIndex = imports.length, runFuncIndex = imports.length;
  // Local layout: params 0 = count, 1..nUni = uniforms; locals: L_row (i32), L_base (i32), then slot values f64 for inputs/outputs/locals, then stack temps.
  const P_COUNT = 0, P_UNI = 1;
  const L_ROW = 1 + nUni, L_BASE = L_ROW + 1, L_SLOT = L_BASE + 1;
  const slotLocal = (slot) => (slot >= nIn && slot < nIn + nUni ? P_UNI + (slot - nIn) : slot < nIn ? L_SLOT + slot : L_SLOT + (slot - nUni));
  const nValueLocals = plan.slots - nUni;
  const T_STACK = L_SLOT + nValueLocals;
  const maxStack = 64;
  const get = (i) => [0x20, ...uleb(i)], set = (i) => [0x21, ...uleb(i)], tee = (i) => [0x22, ...uleb(i)];
  const body = [];
  // for (row = 0; row < count; row++)
  body.push(...[0x41, 0], ...set(L_ROW));
  body.push(0x02, 0x40, 0x03, 0x40);                        // block, loop
  body.push(...get(L_ROW), ...get(P_COUNT), 0x4e, 0x0d, 1); // if row >= count break
  body.push(...get(L_ROW), ...[0x41, ...sleb(stride * 8)], 0x6c, ...set(L_BASE)); // base = row*stride*8
  for (let i = 0; i < nIn; i++) body.push(...get(L_BASE), 0x2b, 3, ...uleb(i * 8), ...set(L_SLOT + i)); // load inputs
  for (let i = 0; i < nOut; i++) body.push(...f64const(0), ...set(slotLocal(nIn + nUni + i)));
  // the stack program on Wasm locals as a stack
  let sp = 0;
  for (const ins of plan.code) {
    if (ins.op === "const") { body.push(...f64const(ins.value), ...set(T_STACK + sp)); sp++; }
    else if (ins.op === "load") { body.push(...get(slotLocal(ins.slot)), ...set(T_STACK + sp)); sp++; }
    else if (ins.op === "store") { sp--; body.push(...get(T_STACK + sp), ...set(slotLocal(ins.slot))); }
    else {
      const o = byId.get(ins.id), n = ins.count, w = o.wasm, args = Array.from({ length: n }, (_, i) => T_STACK + sp - n + i);
      sp -= n;
      if (w.import !== undefined) { for (const a of args) body.push(...get(a)); body.push(0x10, ...uleb(importIndex.get(w.import))); }
      else if (w.fold !== undefined) {
        if (o.name === "-" && n === 1) body.push(...get(args[0]), w.negate);
        else if (!n) body.push(...f64const(w.unit));
        else { body.push(...get(args[0])); for (const a of args.slice(1)) body.push(...get(a), w.fold); }
      }
      else if (w.unary !== undefined) body.push(...get(args[0]), w.unary);
      else if (w.binary !== undefined) body.push(...get(args[0]), ...get(args[1]), w.binary);
      else if (w.compare !== undefined) body.push(...get(args[0]), ...get(args[1]), w.compare, 0xb8); // i32 → f64 (convert_i32_u)
      else if (w.select) body.push(...get(args[1]), ...get(args[2]), ...get(args[0]), ...f64const(0), 0x62, 0x1b); // select(a, b, cond != 0)
      body.push(...set(T_STACK + sp)); sp++;
      if (sp > maxStack) unsupported("Kernel expression too deep", []);
    }
  }
  for (let i = 0; i < nOut; i++) body.push(...get(L_BASE), ...get(slotLocal(nIn + nUni + i)), 0x39, 3, ...uleb((nIn + nUni + i) * 8)); // store outputs
  body.push(...get(L_ROW), ...[0x41, 1], 0x6a, ...set(L_ROW), 0x0c, 0, 0x0b, 0x0b); // row++, continue, end loop, end block
  body.push(0x0b);
  const locals = [2, 2, 0x7f, ...uleb(nValueLocals + maxStack), 0x7c];
  const code = [...locals, ...body];
  const bytes = Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    ...section(1, vector(types)),
    ...section(2, vector([...imports.map((name, i) => [...string("math"), ...string(name), 0, ...uleb(i)]), [...string("env"), ...string("memory"), 2, 0, 1]])),
    ...section(3, vector([[...uleb(runTypeIndex)]])),
    ...section(7, vector([[...string("run"), 0, ...uleb(runFuncIndex)]])),
    ...section(10, vector([[...uleb(code.length), ...code]])),
  ]);
  return { bytes, imports, stride };
}

export function instantiateKernelWasm(plan) {
  const { bytes, imports, stride } = compileKernelWasm(plan);
  const memory = new WebAssembly.Memory({ initial: 1 });
  const module = new WebAssembly.Module(bytes);
  const instance = new WebAssembly.Instance(module, { math: Object.fromEntries(imports.map((name) => [name, IMPORTS[name]])), env: { memory } });
  let view = new Float64Array(memory.buffer);
  return {
    bytes, module, stride,
    // rows: a Float64Array of count*stride values; outputs are written back into it.
    run(rows, count, uniforms = [], execution) {
      const needed = count * stride * 8;
      if (memory.buffer.byteLength < needed) { memory.grow(Math.ceil((needed - memory.buffer.byteLength) / 65536)); view = new Float64Array(memory.buffer); }
      view.set(rows.subarray(0, count * stride));
      execution?.consume(["kernel-wasm"], count);
      instance.exports.run(count, ...uniforms);
      rows.set(view.subarray(0, count * stride));
    },
  };
}

// ---- WGSL: one invocation per row -------------------------------------------
// f32 on the GPU where the CPU has f64; the conformance profile for kernels
// is tolerant on the GPU and exact on the CPU, as for the raytrace backends.
export function emitKernelWGSL(plan) {
  const nIn = plan.inputs.length, nUni = plan.uniforms.length, nOut = plan.outputs.length, stride = nIn + nUni + nOut;
  const slotName = (slot) => (slot < nIn ? plan.inputs[slot] : slot < nIn + nUni ? plan.uniforms[slot - nIn] : slot < nIn + nUni + nOut ? plan.outputs[slot - nIn - nUni] : plan.locals[slot - nIn - nUni - nOut]);
  const lines = [], stack = [];
  const expr = (v) => (typeof v === "number" ? (Number.isInteger(v) ? v.toFixed(1) : String(v)) : v);
  for (const ins of plan.code) {
    if (ins.op === "const") stack.push(expr(decode(ins.value)));
    else if (ins.op === "load") stack.push(slotName(ins.slot));
    else if (ins.op === "store") lines.push(`  ${slotName(ins.slot)} = ${stack.pop()};`);
    else {
      const o = byId.get(ins.id), args = stack.splice(stack.length - ins.count, ins.count), g = o.wgsl;
      if (g.infix) stack.push(args.length === 0 ? g.unit : args.length === 1 && o.name === "-" ? `(-${args[0]})` : `(${args.join(` ${g.infix} `)})`);
      else if (g.fn) stack.push(`${g.fn}(${args.join(", ")})`);
      else if (g.call) stack.push(`${g.call}(${args.join(", ")})`);
      else if (g.compare) stack.push(`select(0.0, 1.0, ${args[0]} ${g.compare} ${args[1]})`);
      else if (g.select) stack.push(`select(${args[2]}, ${args[1]}, ${args[0]} != 0.0)`);
    }
  }
  const decls = [...plan.inputs.map((n, i) => `  let ${n} = rows[base + ${i}u];`), ...plan.uniforms.map((n, i) => `  let ${n} = u.v[${i}u];`), ...plan.outputs.map((n) => `  var ${n}: f32 = 0.0;`), ...plan.locals.map((n) => `  var ${n}: f32 = 0.0;`)];
  const stores = plan.outputs.map((n, i) => `  rows[base + ${nIn + nUni + i}u] = ${n};`);
  return `// kernel ${plan.name}: kernel-v1, emitted from the same plan the CPU runs
struct Uniforms { v: array<f32, ${Math.max(1, nUni)}> };
@group(0) @binding(0) var<storage, read_write> rows: array<f32>;
@group(0) @binding(1) var<uniform> u: Uniforms;
fn safeDiv(a: f32, b: f32) -> f32 { return select(a / b, 0.0, b == 0.0); }
fn safeRem(a: f32, b: f32) -> f32 { return select(a - b * trunc(a / b), 0.0, b == 0.0); }
fn safeSqrt(a: f32) -> f32 { return select(0.0, sqrt(a), a >= 0.0); }
fn jsRound(a: f32) -> f32 { return floor(a + 0.5); }
fn hypot2(a: f32, b: f32) -> f32 { return sqrt(a * a + b * b); }
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) id: vec3u) {
  let row = id.x;
  if (row >= arrayLength(&rows) / ${stride}u) { return; }
  let base = row * ${stride}u;
${decls.join("\n")}
${lines.join("\n")}
${stores.join("\n")}
}
`;
}
