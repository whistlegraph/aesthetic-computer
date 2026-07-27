import assert from "node:assert/strict";
import test from "node:test";

import { outline, spotlight } from "../lib/effects.mjs";

const fakeCdp = () => {
  const expressions = [];
  return {
    expressions,
    async eval(expression) {
      expressions.push(expression);
      if (expression.includes(".spotlight(") || expression.includes(".outline(")) {
        return { x:12, y:24, width:80, height:40 };
      }
      return true;
    },
  };
};

test("spotlight waits for its animated label before reporting capture readiness", async () => {
  const cdp = fakeCdp();
  const result = await spotlight(cdp, "#target", { label:"Stable label" });

  assert.deepEqual(result, { x:12, y:24, width:80, height:40 });
  assert.match(cdp.expressions.at(-1), /\.captureReady\(\)/);
});

test("outline shares the same label readiness contract", async () => {
  const cdp = fakeCdp();
  await outline(cdp, "#target", { label:"Stable label" });

  assert.match(cdp.expressions.at(-1), /\.captureReady\(\)/);
});
