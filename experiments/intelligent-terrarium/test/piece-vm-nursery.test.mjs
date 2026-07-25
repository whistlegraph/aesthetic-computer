import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import { evaluatePieceVmSource, PieceVmNursery, pieceVmMutationFamily,
  rankPieceVmCandidates } from "../src/piece-vm-nursery.mjs";

const foundingSource = await readFile(new URL("../examples/piece-vm-canary.lisp", import.meta.url), "utf8");

test("PieceVM nursery evaluates temporal, spatial, and structural behavior", () => {
  const candidate = evaluatePieceVmSource(foundingSource);
  assert.equal(candidate.proof.valid, true);
  assert.equal(candidate.frameHashes.length, 8);
  assert.notEqual(candidate.frameHashes[0], candidate.frameHashes[1]);
  assert.ok(candidate.traits.coverage > 0);
  assert.ok(candidate.traits.temporal > 0);
  assert.ok(candidate.structure.calls >= 3);
  assert.ok(candidate.structure.draws >= 12);
  assert.ok(candidate.score > 0 && candidate.score <= 1);
});

test("typed PieceVM mutations are deterministic and native evidence gates admission", () => {
  const left = new PieceVmNursery({ seed: "nursery-test", foundingSource });
  const right = new PieceVmNursery({ seed: "nursery-test", foundingSource });
  const seen = new Set();
  let accepted = null;
  for (let index = 0; index < 24; index += 1) {
    const a = left.propose(), b = right.propose();
    assert.deepEqual({ source: a.source, mutation: a.mutation, donor: a.donor, error: a.error },
      { source: b.source, mutation: b.mutation, donor: b.donor, error: b.error });
    if (a.program) {
      seen.add(a.mutation);
      assert.equal(a.proof.valid, true);
      if (!accepted && a.behaviorChanged) accepted = a;
    }
  }
  assert.ok(seen.size >= 4);
  assert.ok(accepted);
  assert.equal(left.admit(accepted, { valid: true, bytecodeHash: "wrong", frameHashes: accepted.frameHashes.slice(0, 2) }), null);
  accepted.selectionEvidence = { policy: "phenotype-lead", staticScore: .72,
    phenotypeReports: 18, phenotypeReady: true, phenotypeScore: .81,
    nativeBias: .037, combinedScore: 99, sonicVoices: 2, capturedAt: 1234 };
  const stored = left.admit(accepted, { valid: true, bytecodeHash: accepted.program.bytecodeHash,
    frameHashes: accepted.frameHashes.slice(0, 2), engine: "native-c11" });
  assert.equal(stored.id, accepted.id);
  assert.equal(left.championId, accepted.id);
  assert.ok(left.lineage.some((value) => value.id === accepted.parent));
  assert.ok(left.lineage.some((value) => value.id === accepted.id));
  assert.deepEqual(stored.selectionEvidence, { schema: 1, parentId: accepted.parent,
    policy: "phenotype-lead", staticScore: .72, phenotypeReports: 18,
    phenotypeReady: true, phenotypeScore: .81, nativeBias: .037,
    combinedScore: .757, sonicVoices: 2, capturedAt: 1234 });
  const restored = PieceVmNursery.fromJSON(left.toJSON(), { foundingSource });
  assert.equal(restored.champion.bytecodeHash, accepted.program.bytecodeHash);
  assert.equal(restored.champion.native.engine, "native-c11");
  assert.deepEqual(restored.champion.selectionEvidence, stored.selectionEvidence);
  assert.deepEqual(restored.lineage.map((value) => value.id), left.lineage.map((value) => value.id));
});

test("PieceVM lineage remains bounded when the founding record is reconstructed", () => {
  const nursery = new PieceVmNursery({ seed: "lineage-bound", foundingSource });
  const resident = nursery.champion;
  const stored = nursery.toJSON();
  stored.lineage = Array.from({ length: 128 }, (_, index) => ({ ...resident, id: index.toString(16).padStart(12, "0") }));
  const restored = PieceVmNursery.fromJSON(stored, { foundingSource });
  assert.equal(restored.lineage.length, 128);
});

test("PieceVM can grow a reusable procedure and an independent memory oscillator", () => {
  const nursery = new PieceVmNursery({ seed: "capability-probe", foundingSource });
  const candidates = Array.from({ length: 24 }, () => nursery.propose()).filter((value) => value.program);
  const graft = candidates.find((value) => value.mutation === "function-graft" && value.behaviorChanged);
  const oscillator = candidates.find((value) => value.mutation === "memory-oscillator" && value.behaviorChanged);
  assert.ok(graft);
  assert.ok(graft.structure.calls > nursery.champion.structure.calls);
  assert.ok(graft.source.includes("(label graft1)"));
  assert.ok(oscillator);
  assert.equal(oscillator.structure.memory, nursery.champion.structure.memory + 2);
  assert.ok(oscillator.program.registerCount > nursery.champion.registerCount);
});

test("requested operator families constrain mutation search without weakening verification", () => {
  assert.equal(pieceVmMutationFamily("color"), "variation");
  assert.equal(pieceVmMutationFamily("memory-oscillator"), "machinery");
  assert.equal(pieceVmMutationFamily("lineage-crossover"), "exchange");
  const nursery = new PieceVmNursery({ seed: "operator-family-probe", foundingSource });
  for (const family of ["variation", "machinery"]) {
    let candidate = null;
    for (let attempt = 0; attempt < 24 && !candidate; attempt += 1) {
      const value = nursery.propose(nursery.championId, [], { operatorFamily: family });
      if (value.program && value.behaviorChanged) candidate = value;
    }
    assert.ok(candidate);
    assert.equal(candidate.requestedOperatorFamily, family);
    assert.equal(candidate.operatorFamily, family);
    assert.equal(candidate.proof.valid, true);
  }
  const exactNursery = new PieceVmNursery({ seed: "exact-operator-probe", foundingSource });
  const exact = exactNursery.propose(exactNursery.championId, [],
    { operatorFamily: "machinery", mutation: "data-layout" });
  assert.equal(exact.requestedMutation, "data-layout");
  assert.equal(exact.mutation, "data-layout");
  assert.equal(exact.operatorFamily, "machinery");
  assert.equal(exact.proof.valid, true);
  const compatibilityNursery = new PieceVmNursery({ seed: "compatibility-probe", foundingSource: exact.source });
  const compatible = compatibilityNursery.propose(compatibilityNursery.championId, [],
    { operatorFamily: "machinery", mutations: ["data-layout", "function-graft"] });
  assert.equal(compatible.preferredMutation, "data-layout");
  assert.equal(compatible.requestedMutation, "function-graft");
  assert.equal(compatible.mutation, "function-graft");
  assert.equal(compatible.compatibilityFallback, true);
  assert.equal(compatible.proof.valid, true);
});

test("an unoccupied structural niche outranks a familiar high score", () => {
  const residents = [{ structure: { niche: "familiar" } }];
  const ranked = rankPieceVmCandidates([
    { id: "tweak", score: .99, iteration: 1, structure: { niche: "familiar" } },
    { id: "capability", score: .51, iteration: 2, structure: { niche: "new-machine-shape" } },
  ], residents);
  assert.deepEqual(ranked.map((value) => value.id), ["capability", "tweak"]);
});

test("a missing crossover capability receives verification priority without bypassing admission", () => {
  const residents = [{ mutation: "color", structure: { niche: "familiar" } }];
  const ranked = rankPieceVmCandidates([
    { id: "tweak", mutation: "color", score: .99, iteration: 1, structure: { niche: "new-shape" } },
    { id: "two-parent", mutation: "lineage-crossover", score: .51, iteration: 2,
      structure: { niche: "familiar" } },
  ], residents);
  assert.deepEqual(ranked.map((value) => value.id), ["two-parent", "tweak"]);
});

test("PieceVM can graft a bounded sensing channel into spatial behavior", () => {
  const nursery = new PieceVmNursery({ seed: "sensing-probe", foundingSource });
  let sensed = null;
  for (let index = 0; index < 96 && !sensed; index += 1) {
    const candidate = nursery.propose();
    if (candidate.mutation === "sense-graft" && candidate.behaviorChanged) sensed = candidate;
  }
  assert.ok(sensed);
  assert.equal(sensed.structure.senses, 1);
  assert.equal(sensed.program.registerCount, nursery.champion.registerCount + 1);
  assert.match(sensed.source, /\(sense8 sense1 (?:beat|bar|fringe)\)/);
});

test("PieceVM evolves argument-bearing functions and bounded named data layouts", () => {
  const nursery = new PieceVmNursery({ seed: "v4-growth", foundingSource });
  let argumentFunction = null, dataLayout = null;
  for (let index = 0; index < 64 && (!argumentFunction || !dataLayout); index += 1) {
    const candidate = nursery.propose();
    if (candidate.mutation === "argument-function-graft" && candidate.behaviorChanged) argumentFunction = candidate;
    if (candidate.mutation === "data-layout" && candidate.behaviorChanged) dataLayout = candidate;
  }
  assert.ok(argumentFunction);
  assert.equal(argumentFunction.structure.functions, 1);
  assert.equal(argumentFunction.structure.arguments, 1);
  assert.match(argumentFunction.source, /\(function arg1 \(angle\)/);
  assert.match(argumentFunction.source, /\(call arg1 [a-z0-9._-]+\)/);
  assert.equal(argumentFunction.proof.valid, true);
  assert.ok(dataLayout);
  assert.equal(dataLayout.structure.layouts, 1);
  assert.equal(dataLayout.structure.layoutBytes, 256);
  assert.match(dataLayout.source, /\(data region1 256\)/);
  assert.match(dataLayout.source, /\(read8 [a-z0-9._-]+ region1 [a-z0-9._-]+\)/);
  assert.match(dataLayout.source, /\(write8 region1 [a-z0-9._-]+ [a-z0-9._-]+\)/);
  assert.equal(dataLayout.proof.valid, true);
});

test("PieceVM maps bounded capabilities from a running neighbor into a provenance-bearing graft", () => {
  const nursery = new PieceVmNursery({ seed: "ecology-probe", foundingSource });
  const probe = { address: 64, track: "sequence", requestedBy: "loopboy", at: 1000 };
  const ecology = [{ id: "outer-cell", capabilities: ["copy"], probe, priority: true }];
  let graft = null;
  for (let index = 0; index < 64 && !graft; index += 1) {
    const candidate = nursery.propose(nursery.championId, ecology);
    if (candidate.mutation === "environment-graft" && candidate.behaviorChanged) graft = candidate;
  }
  assert.ok(graft);
  assert.equal(graft.environmentDonor, "outer-cell");
  assert.equal(graft.environmentCapability, "copy");
  assert.deepEqual(graft.environmentProbe, probe);
  assert.deepEqual(graft.capabilityLineage, [{ donor: "outer-cell", capability: "copy",
    probeAt: 1000, probeAddress: 64, probeTrack: "sequence", requestedBy: "loopboy" }]);
  assert.ok(graft.structure.functions === 1 || graft.structure.layouts === 1);
  assert.equal(graft.proof.valid, true);

  const admitted = nursery.admit(graft, { valid: true, engine: "native-c11",
    bytecodeHash: graft.program.bytecodeHash, frameHashes: graft.frameHashes.slice(0, 2) });
  assert.ok(admitted);
  let child = null;
  for (let index = 0; index < 64 && !child; index += 1) {
    const candidate = nursery.propose(admitted.id);
    if (candidate.program && candidate.behaviorChanged) child = candidate;
  }
  assert.ok(child);
  assert.deepEqual(child.capabilityLineage, admitted.capabilityLineage);
  const admittedChild = nursery.admit(child, { valid: true, engine: "native-c11",
    bytecodeHash: child.program.bytecodeHash, frameHashes: child.frameHashes.slice(0, 2) });
  assert.ok(admittedChild);
  const restored = PieceVmNursery.fromJSON(nursery.toJSON(), { foundingSource });
  assert.deepEqual(restored.lineage.find((value) => value.id === admittedChild.id).capabilityLineage,
    admitted.capabilityLineage);
});

test("PieceVM crossover imports a closed block from a verified ancestor", () => {
  const nursery = new PieceVmNursery({ seed: "capability-probe", foundingSource });
  const founder = nursery.champion;
  let graft = null;
  for (let index = 0; index < 96 && !graft; index += 1) {
    const candidate = nursery.propose();
    if (candidate.mutation === "function-graft" && candidate.behaviorChanged) graft = candidate;
  }
  assert.ok(graft);
  const admittedGraft = nursery.admit(graft, { valid: true, engine: "native-c11",
    bytecodeHash: graft.program.bytecodeHash, frameHashes: graft.frameHashes.slice(0, 2) });
  assert.ok(admittedGraft);
  let crossover = null;
  for (let index = 0; index < 160 && !crossover; index += 1) {
    const candidate = nursery.propose(founder.id);
    if (candidate.mutation === "lineage-crossover" && candidate.behaviorChanged) crossover = candidate;
  }
  assert.ok(crossover);
  assert.equal(crossover.parent, founder.id);
  assert.equal(crossover.donor, admittedGraft.id);
  assert.ok(crossover.structure.transforms > founder.structure.transforms);
  assert.equal(crossover.proof.valid, true);
});
