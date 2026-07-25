import assert from "node:assert/strict";
import test from "node:test";
import { PIECE_VM_POLICY_BANDIT, PieceVmPolicyBandit } from "../src/piece-vm-policy-bandit.mjs";

function trial(policy, index, overrides = {}) {
  return { at: index, policy, parentId: `parent-${index}`, candidateId: `child-${index}`,
    mutation: "color", nativeValid: true, admitted: true, capabilityGain: false,
    staticDelta: 0, ...overrides };
}

test("UCB1 schedule preserves three controls and gives the adaptive slot to unseen evidence", () => {
  const bandit = new PieceVmPolicyBandit();
  for (let index = 0; index < 4; index += 1) bandit.record(trial("phenotype-lead", index));
  assert.deepEqual(bandit.schedule(4),
    ["phenotype-lead", "champion-control", "branch-diversity", "champion-control"]);
  assert.equal(bandit.snapshot().algorithm, "ucb1");
  assert.equal(bandit.snapshot().policies.find((value) => value.policy === "champion-control").exploring, true);
});

test("bounded rewards prefer demonstrated capability without removing baseline exploration", () => {
  const bandit = new PieceVmPolicyBandit();
  for (const policy of PIECE_VM_POLICY_BANDIT.policies) for (let index = 0; index < 24; index += 1) {
    bandit.record(trial(policy, index + 100, { admitted: policy !== "champion-control",
      capabilityGain: policy === "branch-diversity" }));
  }
  assert.deepEqual(bandit.schedule(4),
    ["phenotype-lead", "champion-control", "branch-diversity", "branch-diversity"]);
  const diversity = bandit.snapshot().policies.find((value) => value.policy === "branch-diversity");
  assert.equal(diversity.meanReward, 1);
  assert.equal(diversity.capabilityRate, 1);
});

test("ready native phenotypes revise only matching admitted trials and serialization stays bounded", () => {
  const bandit = new PieceVmPolicyBandit();
  bandit.record(trial("phenotype-lead", 1, { candidateId: "abc123abc123", capabilityGain: true }));
  assert.equal(bandit.observePhenotypes([{ id: "abc123abc123", ready: true, score: .78 }]), 1);
  const phenotype = bandit.snapshot().policies.find((value) => value.policy === "phenotype-lead");
  assert.equal(phenotype.phenotypeReady, 1);
  assert.equal(phenotype.meanReward, (1 + 1 + .78) / 3);
  for (let index = 0; index < 240; index += 1) bandit.record(trial("champion-control", index));
  const restored = PieceVmPolicyBandit.fromJSON(bandit.toJSON());
  assert.equal(restored.toJSON().trials.length, PIECE_VM_POLICY_BANDIT.maxTrials);
  assert.ok(restored.toJSON().trials.every((value) => PIECE_VM_POLICY_BANDIT.policies.includes(value.policy)));
});
