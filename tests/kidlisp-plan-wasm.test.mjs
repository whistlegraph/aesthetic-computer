import { test } from "node:test";
import assert from "node:assert/strict";
import { KidLisp } from "../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { KidLispExecution } from "../system/public/aesthetic.computer/lib/kidlisp-execution.mjs";
import { compileNumeric, runNumeric } from "../system/public/aesthetic.computer/lib/kidlisp-plan.mjs";
import { instantiateNumericWasm } from "../system/public/aesthetic.computer/lib/kidlisp-plan-wasm.mjs";
import { numericOperationNames } from "../system/public/aesthetic.computer/lib/kidlisp-ops.mjs";

test("Wasm preserves reference numeric edge cases, rounding and aliases", () => {
  const lisp = new KidLisp();
  const api = { screen: { width: 1, height: 1 } };
  const values = [0, -0, 1, -1, 1.5, -0.5, -0.1, 0.49999999999999994, 4503599627370495.5, Number.MIN_VALUE, Number.MAX_VALUE, NaN, Infinity, -Infinity];
  for (const name of numericOperationNames()) {
    for (const count of [0, 1, 2, 4]) {
      const bindings = Array.from({ length: count }, (_, i) => `v${i}`);
      const plan = compileNumeric([name, ...bindings], { bindings, fold: false });
      const wasm = instantiateNumericWasm(plan);
      const inputs = count === 0 ? [[]] : count === 1 ? values.map(v => [v]) : count === 2 ? values.flatMap(a => values.map(b => [a, b])) : [[1, 0, 2, 3], [-0, -0, 0, Infinity]];
      for (const args of inputs) {
        const expected = lisp.evaluate([name, ...args], api);
        assert.ok(Object.is(wasm.run(args), expected), `${name}(${args.map(String)})`);
      }
    }
  }
});

test("folded and unfurled randomized plans match native Wasm", () => {
  let seed = 195;
  const rand = n => { seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed % n; };
  const names = numericOperationNames();
  const expr = depth => !depth || rand(3) === 0 ? rand(2) ? rand(101) - 50 : "phase" : [names[rand(names.length)], ...Array.from({ length: rand(4) }, () => expr(depth - 1))];
  for (let i = 0; i < 250; i++) {
    const ast = expr(4), inputs = [rand(101) - 50];
    for (const fold of [false, true]) {
      const plan = JSON.parse(JSON.stringify(compileNumeric(ast, { bindings: ["phase"], fold })));
      assert.ok(Object.is(instantiateNumericWasm(plan).run(inputs), runNumeric(plan, inputs)), JSON.stringify(ast));
    }
  }
});

test("Wasm declares only the necessary remainder import and preserves budgets", () => {
  const add = instantiateNumericWasm(compileNumeric(["+", "x", 3], { bindings: ["x"] }));
  assert.deepEqual(WebAssembly.Module.imports(add.module), []);
  const mod = instantiateNumericWasm(compileNumeric(["%", "x", 3], { bindings: ["x"] }));
  assert.deepEqual(WebAssembly.Module.imports(mod.module), [{ module: "numeric", name: "remainder", kind: "function" }]);
  const execution = new KidLispExecution({ maxSteps: 2 });
  assert.throws(() => add.run([4], execution), e => e.code === "STEP_BUDGET");
  assert.throws(() => add.run(["4"]), /numeric value/);
  assert.throws(() => instantiateNumericWasm({ version: 2, bindings: [] }), /version/);
});
