import { test } from "node:test";
import assert from "node:assert/strict";
import { KidLisp } from "../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { KidLispExecution } from "../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import { compileNumeric, runNumeric } from "../system/public/aesthetic.computer/lib/kidlisp-plan.mjs";
import { NUMERIC_OPERATIONS, numericOperationNames, operationManifest } from "../system/public/aesthetic.computer/lib/kidlisp-ops.mjs";

const api = { screen: { width: 128, height: 128 } };
const roundtrip = value => JSON.parse(JSON.stringify(value));

test("every declared numeric operation and alias exists in the reference host", () => {
  const env = new KidLisp().getGlobalEnv();
  for (const name of numericOperationNames()) assert.equal(typeof env[name], "function", name);
  const manifest = roundtrip(operationManifest());
  assert.equal(new Set(manifest.operations.map(op => op.id)).size, NUMERIC_OPERATIONS.length);
  assert.ok(manifest.operations.every(op => op.effect === "pure" && !op.apply));
});

test("numeric plans preserve reference edge cases through JSON transport", () => {
  const lisp = new KidLisp();
  const values = [0, -0, 1, -1, 1.5, -0.5, NaN, Infinity, -Infinity];
  for (const name of numericOperationNames()) for (const args of [[], ...values.map(v => [v]), ...values.flatMap(a => values.map(b => [a, b])), [1, 0, 2, 3]]) {
    const expression = [name, ...args];
    const expected = lisp.evaluate(expression, api);
    for (const fold of [true, false]) {
      const plan = roundtrip(compileNumeric(expression, { fold }));
      assert.ok(Object.is(runNumeric(plan), expected), `${name} ${args}: folding=${fold}`);
    }
  }
});

test("nested pure plans and input slots match the interpreter across seeded expressions", () => {
  let seed = 427;
  const rand = n => { seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed % n; };
  const names = numericOperationNames();
  const generate = depth => !depth || rand(3) === 0 ? rand(2) ? rand(101) - 50 : "phase" : [names[rand(names.length)], ...Array.from({ length: rand(4) }, () => generate(depth - 1))];
  for (let i = 0; i < 500; i++) {
    const expression = generate(4);
    const phase = rand(101) - 50;
    const lisp = new KidLisp();
    lisp.localEnv.phase = phase;
    const expected = lisp.evaluate(expression, api);
    for (const fold of [true, false]) assert.ok(Object.is(runNumeric(roundtrip(compileNumeric(expression, { bindings: ["phase"], fold })), [phase]), expected), JSON.stringify(expression));
  }
});

test("folding retains source paths and slots read current values", () => {
  const plan = compileNumeric(["+", ["*", 2, 3], "phase"], { bindings: ["phase"] });
  assert.deepEqual(plan.code[0], { op: "const", value: 6, path: [1] });
  assert.equal(runNumeric(plan, [1]), 7);
  assert.equal(runNumeric(plan, [2]), 8);
});

test("effectful forms, undeclared bindings, oversized and malformed plans fail closed", () => {
  for (const expression of [["random", 2], ["clock"], ["wipe", 0], ["fetch", '"data.json"'], ["+", "unbound", 1]]) assert.throws(() => compileNumeric(expression), e => e.code === "UNSUPPORTED_PLAN");
  assert.throws(() => compileNumeric(["+", 1, 2], { maxNodes: 2 }), /budget/);
  const plan = compileNumeric(["+", "phase", 1], { bindings: ["phase"] });
  assert.throws(() => runNumeric(plan, ["1"]), /numeric value/);
  assert.throws(() => runNumeric({ ...plan, version: 2 }, [1]), /version/);
  assert.throws(() => runNumeric({ ...plan, code: [{ op: "call", id: 999, count: 0 }] }, [1]), /call/);
  const execution = new KidLispExecution({ maxSteps: 2 });
  assert.throws(() => runNumeric(plan, [1], execution), e => e.code === "STEP_BUDGET");
});
