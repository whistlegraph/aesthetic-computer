import assert from "node:assert/strict";
import test from "node:test";
import { PieceVmOperatorBandit } from "../src/piece-vm-operator-bandit.mjs";

function trial(policy, index, overrides = {}) {
  return { at: index, policy, parentId: `parent-${index}`, candidateId: `child-${index}`,
    mutation: policy, nativeValid: true, admitted: true, capabilityGain: policy === "machinery",
    staticDelta: 0, ...overrides };
}

test("operator UCB preserves variation, machinery, and exchange before its bonus", () => {
  const bandit = new PieceVmOperatorBandit();
  assert.deepEqual(bandit.schedule(4), ["variation", "machinery", "exchange", "variation"]);
  assert.equal(bandit.snapshot().dimension, "mutation-operator-family");
});

test("operator UCB spends its adaptive slot on measured machinery gains", () => {
  const bandit = new PieceVmOperatorBandit();
  for (const family of ["variation", "machinery", "exchange"]) for (let index = 0; index < 20; index += 1)
    bandit.record(trial(family, index, { admitted: family !== "exchange" }));
  assert.equal(bandit.bonusPolicy(), "machinery");
  assert.equal(bandit.schedule(4).at(-1), "machinery");
  assert.equal(bandit.snapshot().policies.find((value) => value.policy === "machinery").capabilityRate, 1);
});
