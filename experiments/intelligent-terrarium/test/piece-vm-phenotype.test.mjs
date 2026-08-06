import assert from "node:assert/strict";
import test from "node:test";
import { PieceVmPhenotypeOracle, PIECE_VM_PHENOTYPE_LIMITS } from "../src/piece-vm-phenotype.mjs";

function sample(id, overrides = {}) {
  return { id, hp: 90, life: 0, resolution: 128, sonicVoices: 1, role: "diverse",
    traits: { actual: .06, variance: .12, spatial: .15, noise: .08, coherence: .8, muddiness: .1 },
    ...overrides };
}

test("PieceVM phenotype requires a sustained native window before biasing selection", () => {
  const oracle = new PieceVmPhenotypeOracle();
  for (let index = 0; index < PIECE_VM_PHENOTYPE_LIMITS.minReports - 1; index += 1)
    oracle.ingest([sample("aaaaaaaaaaaa")], index * 1000);
  assert.equal(oracle.summary("aaaaaaaaaaaa").ready, false);
  assert.equal(oracle.selectionBias("aaaaaaaaaaaa"), 0);
  oracle.ingest([sample("aaaaaaaaaaaa")], 11_000);
  const summary = oracle.summary("aaaaaaaaaaaa");
  assert.equal(summary.ready, true);
  assert.ok(summary.score > .5);
  assert.ok(summary.selectionBias > 0 && summary.selectionBias <= .06);
  assert.deepEqual(summary.resolutions, [128]);
  assert.equal(summary.sonicVoices, 1);
});

test("PieceVM phenotype penalizes sustained collapse without bypassing its bound", () => {
  const oracle = new PieceVmPhenotypeOracle();
  for (let index = 0; index < 80; index += 1) oracle.ingest([sample("bbbbbbbbbbbb", {
    hp: 4, life: 2, sonicVoices: 0,
    traits: { actual: 0, variance: 0, spatial: 0, noise: 1, coherence: 0, muddiness: 1 },
  })], index * 1000);
  const summary = oracle.summary("bbbbbbbbbbbb");
  assert.equal(summary.samples, PIECE_VM_PHENOTYPE_LIMITS.maxSamples);
  assert.equal(summary.ready, true);
  assert.ok(summary.selectionBias < 0 && summary.selectionBias >= -.06);
});

test("PieceVM phenotype windows survive bounded Git serialization", () => {
  const oracle = new PieceVmPhenotypeOracle();
  oracle.ingest([sample("cccccccccccc", { resolution: 64 })], 1000);
  oracle.ingest([sample("cccccccccccc", { resolution: 256, role: "probe-carrier" })], 2000);
  const restored = PieceVmPhenotypeOracle.fromJSON(oracle.toJSON());
  assert.deepEqual(restored.summary("cccccccccccc"), oracle.summary("cccccccccccc"));
  assert.equal(restored.summary("cccccccccccc").role, "probe-carrier");
  assert.deepEqual(restored.summary("cccccccccccc").resolutions, [64, 256]);
});
