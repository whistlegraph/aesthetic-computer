// Lower the audited arithmetic plan into a shader/C function. Deliberately
// narrow: the ray kernel needs finite constants, inputs, +, -, and * only.
// WGSL is an explicit f32 profile, not a claim of numeric-v1 f64 equivalence.
import { runNumeric } from "../../system/public/aesthetic.computer/lib/kidlisp-plan.mjs";
import { numericOperationById } from "../../system/public/aesthetic.computer/lib/kidlisp-ops.mjs";

export function emitRayPlan(plan, target) {
  if (!["wgsl", "c"].includes(target)) throw new TypeError("Unknown ray plan target");
  if (plan?.bindings?.length !== 7) throw new TypeError("Expected seven ray input slots");
  if (!Array.isArray(plan.code) || plan.code.length > 512) throw new TypeError("Ray profile supports at most 512 instructions");
  runNumeric(plan, Array(7).fill(0));
  const stack = [], lines = [];
  const literal = n => {
    if (n === "-0") return "-0.0";
    if (typeof n !== "number" || !Number.isFinite(n) || (target === "wgsl" && !Number.isFinite(Math.fround(n)))) throw new TypeError("Ray profile requires finite constants");
    const text = String(n);
    return Object.is(n, -0) ? "-0.0" : /[.eE]/.test(text) ? text : `${text}.0`;
  };
  for (const [i, op] of plan.code.entries()) {
    let expression;
    if (op.op === "const") expression = literal(op.value);
    else if (op.op === "input") expression = `p${op.slot}`;
    else {
      const args = stack.splice(stack.length - op.count, op.count);
      const name = numericOperationById(op.id).name;
      if (name === "+" || name === "*") expression = args.reduce((a, b) => `(${a} ${name} ${b})`, name === "+" ? "0.0" : "1.0");
      else if (name === "-") expression = !args.length ? "0.0" : args.length === 1 ? `(-${args[0]})` : args.slice(1).reduce((a, b) => `(${a} - ${b})`, args[0]);
      else throw new TypeError(`Ray profile does not support ${name}`);
    }
    lines.push(target === "wgsl" ? `let v${i}: f32 = ${expression};` : `double v${i} = ${expression};`);
    stack.push(`v${i}`);
  }
  const parameters = Array.from({ length: 7 }, (_, i) => target === "wgsl" ? `p${i}: f32` : `double p${i}`).join(", ");
  return `${target === "wgsl" ? "fn rayKernel" : "static double rayKernel"}(${parameters})${target === "wgsl" ? " -> f32" : ""} {\n${lines.join("\n")}\nreturn ${stack[0]};\n}`;
}
