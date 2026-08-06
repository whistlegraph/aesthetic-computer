import assert from "node:assert/strict";
import test from "node:test";
import { validateManifest } from "../papers/aesthetic-eye.mjs";

const passing = {
  schema: 1,
  expectedDiagrams: 1,
  visualInference: true,
  pdfSha256: "abc",
  reviewedAt: "2026-07-20T23:00:00Z",
  reviewer: { kind: "visual-inference", agent: "test" },
  diagrams: [{
    id: "diagram-one",
    page: 1,
    crop: [0.1, 0.1, 0.8, 0.3],
    design: "pass",
    checks: {
      tangents: "pass",
      type: "pass",
      balance: "pass",
      spaceUse: "pass",
      hierarchy: "pass",
      edgeRouting: "pass",
    },
  }],
};

test("aesthetic-eye accepts a current all-pass visual review", () => {
  assert.deepEqual(validateManifest(passing, "abc"), {
    pass: true,
    errors: [],
    diagrams: passing.diagrams,
  });
});

test("aesthetic-eye rejects stale PDF evidence", () => {
  const result = validateManifest(passing, "changed");
  assert.equal(result.pass, false);
  assert.match(result.errors.join("\n"), /PDF hash changed/);
});

test("aesthetic-eye rejects a pass with a failed design check", () => {
  const manifest = structuredClone(passing);
  manifest.diagrams[0].checks.tangents = "fail";
  const result = validateManifest(manifest, "abc");
  assert.equal(result.pass, false);
  assert.match(result.errors.join("\n"), /design cannot pass/);
});

test("aesthetic-eye requires the full diagram inventory", () => {
  const manifest = { ...passing, expectedDiagrams: 2 };
  const result = validateManifest(manifest, "abc");
  assert.equal(result.pass, false);
  assert.match(result.errors.join("\n"), /expected 2 diagram/);
});
