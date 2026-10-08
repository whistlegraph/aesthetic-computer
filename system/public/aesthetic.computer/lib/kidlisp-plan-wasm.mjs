// f64 backend for numeric-v1. Keeps JS reference edge semantics; no eval or
// generated JavaScript. Remainder is an explicit pure import (Wasm has no f64 %).
import { runNumeric } from "./kidlisp-plan.mjs";
import { numericOperationById } from "./kidlisp-ops.mjs";

const uleb = n => { const a = []; do { const b = n & 127; n >>>= 7; a.push(b | (n ? 128 : 0)); } while (n); return a; };
const vector = items => [...uleb(items.length), ...items.flat()];
const string = s => { const b = [...new TextEncoder().encode(s)]; return [...uleb(b.length), ...b]; };
const section = (id, b) => [id, ...uleb(b.length), ...b];
const constant = value => {
  const n = value === "NaN" ? NaN : value === "+Infinity" ? Infinity : value === "-Infinity" ? -Infinity : value === "-0" ? -0 : value;
  const b = new Uint8Array(8); new DataView(b.buffer).setFloat64(0, n, true);
  return [0x44, ...b];
};
const get = i => [0x20, ...uleb(i)];
const set = i => [0x21, ...uleb(i)];

export function compileNumericWasm(plan) {
  // Reuse the portable plan validator; dummy values cannot cause host effects.
  if (!Array.isArray(plan?.bindings) || plan.bindings.length > 128) throw new TypeError("Wasm numeric plans support at most 128 input slots");
  runNumeric(plan, plan.bindings.map(() => 0));
  const remainder = plan.code.some(i => i.op === "call" && numericOperationById(i.id)?.name === "%");
  const n = plan.bindings.length;
  const body = [];
  const stack = [];
  for (const [index, instruction] of plan.code.entries()) {
    const dest = n + index;
    if (instruction.op === "const") body.push(...constant(instruction.value));
    else if (instruction.op === "input") body.push(...get(instruction.slot));
    else {
      const args = stack.splice(stack.length - instruction.count, instruction.count);
      const name = numericOperationById(instruction.id).name;
      if (["+", "*"].includes(name)) {
        body.push(...constant(name === "+" ? 0 : 1));
        for (const arg of args) body.push(...get(arg), name === "+" ? 0xa0 : 0xa2);
      } else if (name === "-") {
        body.push(...(args.length ? get(args[0]) : constant(0)));
        if (args.length === 1) body.push(0x9a);
        else for (const arg of args.slice(1)) body.push(...get(arg), 0xa1);
      } else if (name === "/") {
        body.push(...(args.length ? get(args[0]) : constant(0)), ...set(dest));
        for (const arg of args.slice(1)) {
          body.push(...get(arg), ...constant(0), 0x62, 0x04, 0x40,
            ...get(dest), ...get(arg), 0xa3, ...set(dest), 0x0b);
        }
        body.push(...get(dest));
      } else if (name === "%") {
        if (args.length < 2) body.push(...constant(0));
        else body.push(...get(args[1]), ...constant(0), 0x61, 0x04, 0x7c, ...constant(0), 0x05,
          ...get(args[0]), ...get(args[1]), 0x10, 0, 0x0b);
      } else if (name === "floor" || name === "ceil") {
        body.push(...(args.length ? get(args[0]) : constant(0)), name === "floor" ? 0x9c : 0x9b);
      } else if (name === "round") {
        if (!args.length) body.push(...constant(0));
        else {
          const x = args[0];
          // Avoid x+0.5: that loses precision near large integers. Preserve -0.
          body.push(...get(x), ...constant(-0.5), 0x66, ...get(x), ...constant(0), 0x63, 0x71,
            0x04, 0x7c, ...constant(-0), 0x05,
            ...get(x), 0x9c, ...set(dest),
            ...get(x), ...get(dest), 0xa1, ...constant(0.5), 0x63,
            0x04, 0x7c, ...get(dest), 0x05, ...get(dest), ...constant(1), 0xa0, 0x0b, 0x0b);
        }
      } else throw new TypeError(`No numeric-v1 Wasm implementation for ${name}`);
    }
    body.push(...set(dest));
    stack.push(dest);
  }
  body.push(...get(stack[0]), 0x0b);
  const types = [[0x60, ...vector(Array.from({ length: n }, () => [0x7c])), 1, 0x7c]];
  if (remainder) types.push([0x60, 2, 0x7c, 0x7c, 1, 0x7c]);
  const locals = [1, ...uleb(plan.code.length), 0x7c];
  const code = [...locals, ...body];
  return Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0,
    ...section(1, vector(types)),
    ...(remainder ? section(2, vector([[...string("numeric"), ...string("remainder"), 0, 1]])) : []),
    ...section(3, [1, 0]),
    ...section(7, vector([[...string("run"), 0, remainder ? 1 : 0]])),
    ...section(10, vector([[...uleb(code.length), ...code]])),
  ]);
}

export function instantiateNumericWasm(plan) {
  const bytes = compileNumericWasm(plan);
  const module = new WebAssembly.Module(bytes);
  const instance = new WebAssembly.Instance(module, { numeric: { remainder: (a, b) => a % b } });
  const slots = plan.bindings.length, steps = plan.code.length;
  return {
    bytes, module,
    run(inputs = [], execution) {
      if (!Array.isArray(inputs) || inputs.length !== slots || inputs.some(value => typeof value !== "number")) throw new TypeError("Expected one numeric value per input slot");
      execution?.consume(["numeric-wasm"], steps);
      return instance.exports.run(...inputs);
    },
  };
}
