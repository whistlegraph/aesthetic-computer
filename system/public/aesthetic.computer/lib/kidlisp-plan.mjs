// Portable numeric instruction plans. No eval/new Function and no host calls.
import { numericOperation, numericOperationById } from "./kidlisp-ops.mjs";
import { KidLispExecutionError } from "./kidlisp-execution.mjs";

const encode = n => Number.isNaN(n) ? "NaN" : n === Infinity ? "+Infinity" : n === -Infinity ? "-Infinity" : Object.is(n, -0) ? "-0" : n;
const decode = n => n === "NaN" ? NaN : n === "+Infinity" ? Infinity : n === "-Infinity" ? -Infinity : n === "-0" ? -0 : typeof n === "number" ? n : (() => { throw new TypeError("Invalid numeric constant"); })();
const unsupported = (message, path) => { throw new KidLispExecutionError("UNSUPPORTED_PLAN", message, { path }); };

export function compileNumeric(expression, { bindings = [], fold = true, maxNodes = 10000, maxDepth = 64 } = {}) {
  if (!Number.isSafeInteger(maxNodes) || maxNodes < 1 || !Number.isSafeInteger(maxDepth) || maxDepth < 1 || maxDepth > 128) throw new TypeError("Invalid compilation budget");
  if (!Array.isArray(bindings) || bindings.some(name => typeof name !== "string" || !/^[A-Za-z_][\w-]*$/.test(name)) || new Set(bindings).size !== bindings.length) throw new TypeError("Bindings must be distinct explicit names");
  const slots = new Map(bindings.map((name, i) => [name, i]));
  const code = [];
  let visited = 0;
  const lower = (expr, path, depth) => {
    if (++visited > maxNodes || depth > maxDepth) unsupported("Numeric compilation budget exceeded", path);
    if (typeof expr === "number") {
      code.push({ op: "const", value: encode(expr), path });
      return { constant: true, value: expr };
    }
    if (typeof expr === "string" && slots.has(expr)) {
      code.push({ op: "input", slot: slots.get(expr), path });
      return { constant: false };
    }
    if (!Array.isArray(expr) || !expr.length) unsupported("Expected a number, input slot, or audited numeric operation", path);
    const op = numericOperation(expr[0]);
    if (!op) unsupported(`Operation ${String(expr[0])} is outside numeric-v1`, path);
    const start = code.length;
    const values = expr.slice(1).map((child, i) => lower(child, [...path, i + 1], depth + 1));
    if (fold && values.every(value => value.constant)) {
      const value = op.apply(values.map(v => v.value));
      code.length = start;
      code.push({ op: "const", value: encode(value), path });
      return { constant: true, value };
    }
    code.push({ op: "call", id: op.id, count: values.length, path });
    return { constant: false };
  };
  lower(expression, [], 0);
  return { version: 1, profile: "numeric-v1", bindings: [...bindings], sourceNodes: visited, code };
}

export function runNumeric(plan, inputs = [], execution) {
  if (plan?.version !== 1 || plan.profile !== "numeric-v1") throw new TypeError("Unsupported numeric plan version/profile");
  if (!Array.isArray(plan.bindings) || !Array.isArray(inputs) || inputs.length !== plan.bindings.length || inputs.some(value => typeof value !== "number")) throw new TypeError("Expected one numeric value per input slot");
  if (!Array.isArray(plan.code) || !plan.code.length || plan.code.length > 10000) throw new TypeError("Invalid numeric plan size");
  const stack = [];
  for (const instruction of plan.code) {
    execution?.consume(["numeric-plan"]);
    if (instruction.op === "const") stack.push(decode(instruction.value));
    else if (instruction.op === "input") {
      if (!Number.isInteger(instruction.slot) || instruction.slot < 0 || instruction.slot >= inputs.length) throw new TypeError("Invalid input slot");
      stack.push(inputs[instruction.slot]);
    } else if (instruction.op === "call") {
      const op = numericOperationById(instruction.id);
      if (!op || !Number.isInteger(instruction.count) || instruction.count < 0 || instruction.count > stack.length) throw new TypeError("Invalid numeric call");
      stack.push(op.apply(stack.splice(stack.length - instruction.count, instruction.count)));
    } else throw new TypeError("Invalid numeric instruction");
  }
  if (stack.length !== 1) throw new TypeError("Numeric plan must leave one result");
  return stack[0];
}
