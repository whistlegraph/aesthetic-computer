import assert from "node:assert/strict";
import test from "node:test";
import { PieceVmOutcomeModel } from "../src/piece-vm-outcome-model.mjs";

function trial(requestedMutation, mutation = requestedMutation, overrides = {}) {
  return { at: 1, requestedMutation, mutation, parentId: "parent", candidateId: "child",
    nativeValid: true, admitted: true, capabilityDelta: {}, ...overrides };
}

test("specific outcome model explores every known mutation within its requested family", () => {
  const model = new PieceVmOutcomeModel();
  assert.deepEqual(model.schedule(["variation", "machinery", "exchange"]),
    ["color", "function-graft", "lineage-crossover"]);
  model.record(trial("function-graft", "color"));
  assert.equal(model.snapshot().mutations.find((value) => value.mutation === "function-graft").availabilityRate, 0);
  assert.equal(model.bonusMutation("machinery"), "argument-function-graft");
});

test("compatibility misses demote an unavailable preference while crediting its feasible fallback", () => {
  const model = new PieceVmOutcomeModel();
  model.record(trial("data-layout", "data-layout", { preferredMutation: "function-graft",
    capabilityDelta: { layouts: 1, layoutBytes: 256 } }));
  const snapshot = model.snapshot();
  const missed = snapshot.mutations.find((value) => value.mutation === "function-graft");
  const selected = snapshot.mutations.find((value) => value.mutation === "data-layout");
  assert.deepEqual({ exposures: missed.exposures, preferences: missed.preferences,
    compatibilityMisses: missed.compatibilityMisses, compatibilityRate: missed.compatibilityRate },
  { exposures: 1, preferences: 1, compatibilityMisses: 1, compatibilityRate: 0 });
  assert.equal(selected.requests, 1);
  assert.equal(selected.honored, 1);
  assert.ok(selected.meanReward > 0);
  assert.equal(model.bonusMutation("machinery"), "argument-function-graft");
});

test("specific outcome model rewards honored causal capability breadth and matures phenotype evidence", () => {
  const model = new PieceVmOutcomeModel();
  model.record(trial("data-layout", "data-layout", { candidateId: "layout-child",
    capabilityDelta: { layouts: 1, layoutBytes: 256, memory: 2 } }));
  const before = model.snapshot().mutations.find((value) => value.mutation === "data-layout");
  assert.equal(before.capabilityGains, 1);
  assert.deepEqual({ layouts: before.deltas.layouts, layoutBytes: before.deltas.layoutBytes,
    memory: before.deltas.memory }, { layouts: 1, layoutBytes: 256, memory: 2 });
  assert.equal(model.observePhenotypes([{ id: "layout-child", ready: true, score: .9 }]), 1);
  const after = model.snapshot().mutations.find((value) => value.mutation === "data-layout");
  assert.equal(after.phenotypeReady, 1);
  assert.ok(after.meanReward > before.meanReward);
  assert.equal(PieceVmOutcomeModel.fromJSON(model.toJSON()).snapshot().trials, 1);
});
