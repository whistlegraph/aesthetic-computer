import assert from "node:assert/strict";
import test from "node:test";
import { PieceVmCurriculum, pieceVmCurriculumEvidence, pieceVmDevelopment,
  pieceVmCurriculumParent, pieceVmCurriculumTarget, prioritizePieceVmCurriculum } from "../src/piece-vm-curriculum.mjs";

const parent = { id: "parent", structure: { layouts: 1, layoutBytes: 256, memory: 4,
  functions: 1, arguments: 1, senses: 0 }, capabilityLineage: [{ capability: "cellular" }] };
const sensingChild = { id: "child", structure: { ...parent.structure, senses: 1 },
  capabilityLineage: parent.capabilityLineage };

test("development fingerprint measures retained compound machinery rather than isolated mutation names", () => {
  assert.deepEqual(pieceVmDevelopment(parent), { schema: 1,
    flags: { "named-memory": true, "temporal-memory": true, abstraction: true, sensing: false, ecology: true },
    attained: ["named-memory", "temporal-memory", "abstraction", "ecology"], missing: ["sensing"],
    breadth: 4, signature: "11101" });
  const evidence = pieceVmCurriculumEvidence(parent, sensingChild);
  assert.deepEqual(evidence.gained, ["sensing"]);
  assert.deepEqual(evidence.lost, []);
  assert.equal(evidence.afterBreadth, 5);
  assert.equal(evidence.complete, true);
});

test("bounded curriculum lane periodically outranks one-step UCB while ordinary cycles preserve UCB priority", () => {
  const adaptive = { id: "adaptive", operatorFamily: "exchange",
    selectionEvidence: { policy: "branch-diversity" },
    curriculumEvidence: pieceVmCurriculumEvidence(parent, parent) };
  const developing = { id: "developing", operatorFamily: "machinery",
    selectionEvidence: { policy: "champion-control" },
    curriculumEvidence: pieceVmCurriculumEvidence(parent, sensingChild) };
  assert.equal(prioritizePieceVmCurriculum([adaptive, developing], "branch-diversity", "exchange", true)[0].id,
    "developing");
  assert.equal(prioritizePieceVmCurriculum([adaptive, developing], "branch-diversity", "exchange", false)[0].id,
    "adaptive");
  const chain = { id: "chain", operatorFamily: "machinery",
    selectionEvidence: { policy: "curriculum-chain" },
    curriculumEvidence: pieceVmCurriculumEvidence(parent, parent) };
  assert.equal(prioritizePieceVmCurriculum([developing, chain], "branch-diversity", "exchange", true)[0].id,
    "chain");
});

test("curriculum chains from the broadest resident toward its first missing capability", () => {
  const sensingBranch = { id: "sensing-branch", generation: 8,
    structure: { layouts: 1, layoutBytes: 256, memory: 4, functions: 0, arguments: 0, senses: 1 },
    capabilityLineage: [{ capability: "cellular" }] };
  assert.equal(pieceVmCurriculumParent([sensingBranch, parent]).id, "sensing-branch");
  const saturated = { ...parent, id: "saturated", generation: 100, registerCount: 32, instructionCount: 180 };
  const compatible = { ...parent, id: "compatible", generation: 1, registerCount: 31, instructionCount: 100 };
  assert.equal(pieceVmCurriculumParent([saturated, compatible]).id, "compatible");
  assert.deepEqual(pieceVmCurriculumTarget(parent),
    { capability: "sensing", family: "machinery", mutations: ["sense-graft"] });
  assert.deepEqual(pieceVmCurriculumTarget(sensingBranch),
    { capability: "abstraction", family: "machinery",
      mutations: ["argument-function-graft", "function-graft"] });
  assert.equal(pieceVmCurriculumTarget(sensingChild), null);
});

test("curriculum outcomes persist, mature phenotype evidence, and lead only one cycle in four", () => {
  const curriculum = new PieceVmCurriculum();
  assert.equal(curriculum.shouldLead(0, 4), false);
  assert.equal(curriculum.shouldLead(4, 4), false);
  assert.equal(curriculum.shouldLead(12, 4), true);
  assert.equal(curriculum.shouldLead(16, 4), false);
  curriculum.record({ at: 1, parentId: parent.id, candidateId: sensingChild.id, mutation: "sense-graft",
    lead: true, nativeValid: true, admitted: true,
    evidence: pieceVmCurriculumEvidence(parent, sensingChild) });
  assert.equal(curriculum.observePhenotypes([{ id: sensingChild.id, ready: true, score: .91 }]), 1);
  const snapshot = curriculum.snapshot(sensingChild, 4, 4);
  assert.deepEqual({ trials: snapshot.trials, admissions: snapshot.admissions,
    advancements: snapshot.advancements, compoundAdmissions: snapshot.compoundAdmissions,
    completeAdmissions: snapshot.completeAdmissions, maxBreadth: snapshot.maxBreadth,
    phenotypeReady: snapshot.phenotypeReady },
  { trials: 1, admissions: 1, advancements: 1, compoundAdmissions: 1,
    completeAdmissions: 1, maxBreadth: 5, phenotypeReady: 1 });
  assert.equal(PieceVmCurriculum.fromJSON(curriculum.toJSON()).snapshot().trials, 1);
});
