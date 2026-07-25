import assert from "node:assert/strict";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { createSortSoupServer, decodeMarginProbe, evaluateProposalWindow, nativeSnapshot, PerformanceOracle,
  pieceVmEcology, pieceVmEcologyWithProbe, prioritizePieceVmAdaptation, prioritizePieceVmPolicy,
  proposalEvaluationCost, scheduleProposalEvaluations } from "../src/soup-server.mjs";
import { GROOVE_LAYOUT } from "../src/pixel-groove.mjs";
import { NoveltyArchive } from "../src/sort-soup.mjs";
import { EvaluationPool } from "../src/evaluation-pool.mjs";

test("PieceVM ecology membrane extracts only bounded capabilities from visible residents", () => {
  assert.deepEqual(pieceVmEcology([
    { id: "neighbor-a", address: "B2", source: "(raster (triangle 1 2 3) (cellular 40 12) (internet nope))" },
    { id: "neighbor-b", address: "C2", source: "(raster (copy 0 0 4 4 1) (paste 1 2 3 xor))" },
  ]), [
    { id: "neighbor-a", address: "B2", capabilities: ["triangle", "cellular"] },
    { id: "neighbor-b", address: "C2", capabilities: ["copy", "paste"] },
  ]);
  assert.deepEqual(pieceVmEcologyWithProbe([
    { id: "neighbor-a", address: "B2", source: "(raster (triangle 1 2 3) (cellular 40 12))" },
  ], { id: "neighbor-a", address: 64, track: "sequence", capability: "cellular", requestedBy: "loopboy", at: 9 }), [{
    id: "neighbor-a", address: "B2", capabilities: ["cellular"], priority: true,
    probe: { address: 64, track: "sequence", requestedBy: "loopboy", at: 9 },
  }]);
});

test("the UCB-selected PieceVM arm receives the scarce native-verification position", () => {
  const candidates = [
    { id: "capability-ranked", selectionEvidence: { policy: "phenotype-lead" } },
    { id: "adaptive-a", selectionEvidence: { policy: "branch-diversity" } },
    { id: "control", selectionEvidence: { policy: "champion-control" } },
    { id: "adaptive-b", selectionEvidence: { policy: "branch-diversity" } },
  ];
  assert.deepEqual(prioritizePieceVmPolicy(candidates, "branch-diversity").map((value) => value.id),
    ["adaptive-a", "adaptive-b", "capability-ranked", "control"]);
});

test("paired parent-policy and operator bonuses receive joint verification priority", () => {
  const candidates = [
    { id: "operator-only", operatorFamily: "machinery", selectionEvidence: { policy: "phenotype-lead" } },
    { id: "policy-only", operatorFamily: "variation", selectionEvidence: { policy: "branch-diversity" } },
    { id: "paired", operatorFamily: "machinery", selectionEvidence: { policy: "branch-diversity" } },
    { id: "baseline", operatorFamily: "variation", selectionEvidence: { policy: "champion-control" } },
  ];
  assert.equal(prioritizePieceVmAdaptation(candidates, "branch-diversity", "machinery")[0].id, "paired");
});

test("every fourth PieceVM evolution reserves one chained proposal from the broadest developmental parent", async (context) => {
  const verifier = async (candidate) => ({ valid: true, engine: "test-native",
    bytecodeHash: candidate.program.bytecodeHash, frameHashes: candidate.frameHashes.slice(0, 2), checkedFrames: 2 });
  const app = await createSortSoupServer({ cycleMs: 0, pieceVmCycleMs: 0, pieceVmBatch: 4,
    pieceVmNativeVerifier: verifier });
  context.after(() => app.stop());
  for (let evolution = 0; evolution < 4; evolution += 1) await app.evolvePieceVm();
  const last = app.snapshot().runtime.pieceVm.lastEvolution;
  assert.equal(last.curriculumLead, true);
  assert.deepEqual(last.policySchedule.slice(0, 3),
    ["phenotype-lead", "champion-control", "branch-diversity"]);
  assert.deepEqual(last.operatorSchedule.slice(0, 3), ["variation", "machinery", "exchange"]);
  assert.equal(last.policySchedule.at(-1), "curriculum-chain");
  assert.match(last.curriculumParentId, /^[0-9a-f]{12}$/);
  const targetMutation = { "named-memory": "data-layout", "temporal-memory": "memory-oscillator",
    abstraction: "argument-function-graft", sensing: "sense-graft", ecology: "environment-graft" };
  assert.equal(last.mutationPreferenceSchedule.at(-1)[0], targetMutation[last.curriculumTarget]);
});

test("performance oracle rescues red once, then culls sustained red", () => {
  let clock = 0;
  const oracle = new PerformanceOracle({ now: () => clock, windowMs: 60_000, random: () => 0 });
  const rows = Array.from({ length: 12 }, (_, slot) => ({
    id: `resident-${slot}`, address: `${String.fromCharCode(65 + slot % 4)}${1 + Math.floor(slot / 4)}`,
    hp: slot === 0 ? 4 : 78, ageMs: 61_000,
  }));
  assert.equal(oracle.ingest(rows).action, null);
  clock = 30_001;
  const action = oracle.ingest(rows).action;
  assert.equal(action.type, "prod");
  assert.equal(action.id, "resident-0");
  assert.equal(action.strategy, "copy");
  assert.equal(action.donorId, "resident-1");
  clock = 90_002;
  assert.equal(oracle.ingest(rows).action.type, "cull");
});

test("performance oracle keeps and forks sustained yellow when rendering has headroom", () => {
  let clock = 0;
  const oracle = new PerformanceOracle({ now: () => clock, windowMs: 60_000, random: () => .9 });
  const rows = Array.from({ length: 12 }, (_, slot) => ({
    id: `yellow-${slot}`, address: `${String.fromCharCode(65 + slot % 4)}${1 + Math.floor(slot / 4)}`,
    hp: slot === 0 ? 52 : 82, ageMs: 61_000,
  }));
  assert.equal(oracle.ingest(rows, new Map(), 143.6).action, null);
  clock = 60_001;
  const action = oracle.ingest(rows, new Map(), 143.6).action;
  assert.equal(action.type, "fork");
  assert.equal(action.id, "yellow-0");
});

test("terminal residents die quickly only after repeated native reprobes", () => {
  let clock = 0;
  const oracle = new PerformanceOracle({ now: () => clock, windowMs: 60_000, terminalWindowMs: 5_000 });
  const rows = Array.from({ length: 12 }, (_, slot) => ({
    id: `terminal-${slot}`, address: `${String.fromCharCode(65 + slot % 4)}${1 + Math.floor(slot / 4)}`,
    hp: slot === 0 ? 3 : 78, ageMs: 15_000,
    life: slot === 0 ? 2 : 0, failedReprobes: slot === 0 ? 3 : 0,
  }));
  assert.equal(oracle.ingest(rows).action, null);
  clock = 5_001;
  const action = oracle.ingest(rows).action;
  assert.equal(action.type, "cull");
  assert.equal(action.id, "terminal-0");
  assert.equal(action.reason, "terminal-after-failed-reprobes");
});

test("performance oracle exposes sustained high-health and variability review signals", () => {
  let clock = 0;
  const oracle = new PerformanceOracle({ now: () => clock, windowMs: 120_000 });
  let result;
  for (let sample = 0; sample < 20; sample += 1) {
    const rows = Array.from({ length: 12 }, (_, slot) => ({
      id: `review-${slot}`, hp: slot === 0 ? (sample % 2 ? 94 : 54) : 90,
      ageMs: 20_000, failedReprobes: 0,
    }));
    result = oracle.ingest(rows);
    clock += 1_000;
  }
  const variable = result.metrics.find((metric) => metric.id === "review-0");
  const healthy = result.metrics.find((metric) => metric.id === "review-1");
  assert.equal(variable.samples, 20);
  assert.ok(variable.healthRange >= 40);
  assert.ok(variable.healthStdDev >= 20);
  assert.equal(healthy.healthMean, 90);
  assert.equal(healthy.healthyRatio, 1);
});

test("sort-soup serves distinct board and tiled population surfaces", async (context) => {
  const app = await createSortSoupServer({ port: 0, cycleMs: 0 });
  context.after(() => app.stop());
  const base = `http://127.0.0.1:${app.address.port}`;
  const [board, soup, soupJs] = await Promise.all([
    fetch(`${base}/board`).then((response) => response.text()),
    fetch(`${base}/soup`).then((response) => response.text()),
    fetch(`${base}/soup.mjs`).then((response) => response.text()),
  ]);
  assert.match(board, /PIECEFARM MISSION/);
  assert.match(board, /SEARCH PROGRAM-OUTPUT SPACE WITHOUT SURRENDERING VERIFICATION/);
  assert.match(board, /Live search measurements/);
  assert.match(soup, /Tiled live visualization of programs/);
  assert.match(soupJs, /columns: 4, rows: 3/);
  assert.match(soupJs, /fetch\(`\/api\/groove\/\$\{encodeURIComponent\(program.id\)\}`\)/);
  assert.match(soupJs, /groovePixels\[pixel\]/);
  assert.match(soupJs, /needlePixel/);
  assert.match(soupJs, /GROOVE_TRACKS/);
  assert.match(soupJs, /\["lifecycle", "LIFECYCLE", "#38d9e6"\]/);
  assert.match(soupJs, /density\[key\]\?\.pixelFill/);
  assert.match(soupJs, /healthGradient\.addColorStop\(\.34, "#ff7a1a"\)/);
  assert.ok(app.snapshot().programs.every((program) => ["resident", "dissolving", "rejected"].includes(program.status)));
  assert.equal(app.snapshot().checkpoint.nextIteration, 2048);
  assert.ok(app.snapshot().checkpoint.iterationsRemaining > 0);
});

test("sort-soup exposes a bounded native display membrane", async (context) => {
  const app = await createSortSoupServer({ cycleMs: 0 });
  context.after(() => app.stop());
  const base = `http://127.0.0.1:${app.address.port}`;
  const response = await fetch(`${base}/api/native`);
  const text = await response.text();
  assert.equal(response.headers.get("content-type"), "text/plain; charset=utf-8");
  assert.match(text, /^S\t\d+\t\d+\t\d+\t\d+\t24672\t/m);
  const summary = text.split("\n").find((line) => line.startsWith("S\t")).split("\t");
  assert.equal(summary.length, 102);
  assert.deepEqual(summary.slice(19, 23).map(Number), [0, 0, 0, 0]);
  assert.deepEqual([Number(summary[23]), summary[24], Number(summary[25])], [0, "", 0]);
  assert.deepEqual([
    Number(summary[26]), summary[27], Number(summary[28]), Number(summary[29]), Number(summary[30]),
  ], [0, "foundation", 1, 0, 1]);
  assert.match(summary[31], /^[0-9a-f]{12}$/);
  assert.deepEqual(summary.slice(32, 36).map(Number), [27, 3, 2, 0]);
  assert.ok(Number(summary[36]) > 0);
  assert.deepEqual(summary.slice(37, 40).map(Number), [0, 0, 0]);
  assert.equal(Number(summary[40]), 0);
  assert.deepEqual(summary.slice(41, 45).map(Number), [0, 0, 0, 0]);
  assert.deepEqual(summary.slice(45, 47), ["", ""]);
  assert.deepEqual(summary.slice(47, 52), ["", "-1", "", "", ""]);
  assert.deepEqual(summary.slice(52, 59), ["", "", "0", "", "0", "0", "0"]);
  assert.deepEqual(summary.slice(59, 64), ["0", "0", "0", "", "0"]);
  assert.match(summary[64], /^[0-9a-f]{12}$/);
  assert.deepEqual(summary.slice(65, 71), ["0", "0", "0.0000", "0.0000", "0", ""]);
  assert.deepEqual(summary.slice(71, 84), ["phenotype-lead",
    "0", "0.0000", "0.0000", "0.0000",
    "0", "0.0000", "0.0000", "0.0000",
    "0", "0.0000", "0.0000", "0.0000"]);
  assert.deepEqual(summary.slice(84, 94), ["variation",
    "0", "0.0000", "0.0000", "0", "0.0000", "0.0000",
    "0", "0.0000", "0.0000"]);
  assert.equal(summary[94], "color");
  assert.deepEqual(summary.slice(95, 102), ["0", "0", "0", "0", "0", "0", "00000"]);
  const ecologyState = app.snapshot();
  ecologyState.runtime.pieceVm.lastEnvironmentGraft = {
    id: "eco-child", environmentCapability: "cellular", environmentDonor: "neighbor123456",
  };
  const ecologySummary = nativeSnapshot(ecologyState).split("\n")[0].split("\t");
  assert.deepEqual(ecologySummary.slice(45, 47), ["cellular", "neighbor123456"]);
  assert.match(text, /^P\t[^\t]+\t(?:classic|grammar)\t/m);
  assert.ok(Buffer.byteLength(text) < 2 * 1024 * 1024);
  assert.doesNotMatch(text, /\[\s*\[\s*"[csw]"/);
  assert.match(text, /^P\t.*\traster\t[0-9a-f]{98304}\t128\t128\t[0-9a-f]+\t(?:[A-D][1-3])?\t[0-9a-f]{55296}\t(?:[0-9a-f]{1232})?\t(?:0|64)\t[01]\t(?:[0-9a-f]{12})?\t(?:canary)?$/m);
  const nativeRows = text.split("\n").filter((line) => line.startsWith("P\t")).map((line) => line.split("\t"));
  assert.ok(nativeRows.every((row) => row.length === 24));
  const canaryRows = nativeRows.filter((row) => row[19]);
  assert.equal(canaryRows.length, 1);
  assert.equal(canaryRows[0][19].length, 1232);
  assert.equal(Number(canaryRows[0][20]), 64);
  assert.ok(canaryRows[0][17] === "" || canaryRows[0][17] === "A1");
  const pieceVmResponse = await fetch(`${base}/api/piecevm`);
  const pieceVmText = await pieceVmResponse.text();
  const pieceVmState = JSON.parse(pieceVmText);
  assert.equal(pieceVmResponse.status, 200);
  assert.ok(Buffer.byteLength(pieceVmText) < 256 * 1024);
  assert.match(pieceVmState.pieceVm.selection.parentId, /^[0-9a-f]{12}$/);
  assert.deepEqual(pieceVmState.pieceVm.phenotypes, []);
  assert.deepEqual(app.snapshot().runtime.visualCuration.persisted, {
    specimens: 0, observations: 0, reviews: 0,
    recommendations: { retain: 0, watch: 0, reject: 0 },
  });
});

test("PieceVM nursery admits native-matched typed descendants into the A1 membrane", async (context) => {
  const app = await createSortSoupServer({ cycleMs: 0, pieceVmCycleMs: 0, pieceVmBatch: 8,
    pieceVmNativeVerifier: async (candidate) => ({ valid: true, engine: "test-native",
      bytecodeHash: candidate.program.bytecodeHash, frameHashes: candidate.frameHashes.slice(0, 2), checkedFrames: 2 }) });
  context.after(() => app.stop());
  const founding = app.pieceVmNursery.championId;
  const admitted = await app.evolvePieceVm();
  assert.ok(admitted);
  assert.notEqual(admitted.id, founding);
  assert.ok(["clone-child", "prune-child", "duplicate-draw", "delete-draw", "insert-rotation", "rotate-axis", "vector-component", "branch-depth", "color", "function-graft", "argument-function-graft", "data-layout", "environment-graft", "memory-oscillator", "sense-graft", "lineage-crossover"].includes(admitted.mutation));
  const state = app.snapshot();
  assert.equal(state.runtime.pieceVm.champion.id, admitted.id);
  assert.equal(state.runtime.pieceVm.champion.native.engine, "test-native");
  assert.deepEqual(Object.fromEntries(Object.entries(admitted.native.profiles)
    .map(([name, evidence]) => [name, [evidence.valid, evidence.resolution]])), {
    half: [true, 64], standard: [true, 128], double: [true, 256],
  });
  const displayPrograms = state.programs.filter((program) => program.domain === "raster").slice(-12)
    .map((program, index) => ({ ...program, address: ["A1", "B1", "C1"][index] || program.address }));
  const profileRows = nativeSnapshot({ ...state, displayPrograms }, admitted).split("\n")
    .filter((line) => line.startsWith("P\t")).map((line) => line.split("\t"));
  assert.deepEqual(profileRows.slice(0, 3).map((row) => [row[19] === admitted.bytecode, Number(row[20])]),
    [[true, 64], [true, 128], [true, 256]]);
  const response = await fetch(`http://127.0.0.1:${app.address.port}/api/native`);
  const row = (await response.text()).split("\n").find((line) => line.startsWith("P\t")).split("\t");
  assert.equal(row[19], admitted.bytecode);
});

test("sustained native PieceVM phenotype becomes bounded nursery selection evidence", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "piecevm-phenotype-"));
  let clock = 0;
  const app = await createSortSoupServer({ port: 0, cycleMs: 0, pieceVmCycleMs: 0,
    ledgerPath: join(root, "specimens.sqlite"), now: () => clock,
    pieceVmNativeVerifier: async (candidate) => ({ valid: true, engine: "test-native",
      bytecodeHash: candidate.program.bytecodeHash, frameHashes: candidate.frameHashes.slice(0, 2), checkedFrames: 2 }) });
  context.after(() => app.stop());
  assert.ok(await app.evolvePieceVm());
  const base = `http://127.0.0.1:${app.address.port}`;
  const state = app.snapshot();
  const lead = state.runtime.pieceVm.embodiment.find((value) => value.address === "A1");
  const raster = state.displayPrograms.find((value) => value.address === "A1");
  assert.ok(lead && raster);
  for (let sample = 0; sample < 12; sample += 1) {
    const response = await fetch(`${base}/api/health`, { method: "POST",
      headers: { "content-type": "application/json" }, body: JSON.stringify({ displayFps: 142, residents: [{
        id: raster.id, hp: 92, ageMs: 20_000, life: 0, failedReprobes: 0,
        resolution: lead.resolution, actual: .06, variance: .12, spatial: .15,
        noise: .08, coherence: .8, muddiness: .1,
        pieceVmId: lead.id, pieceVmRole: lead.role,
        pieceVmProbeCarrier: lead.probeCarrier, sonicVoices: 1,
      }] }) });
    assert.equal(response.status, 200);
    clock += 1_000;
  }
  const after = app.snapshot();
  const phenotype = after.runtime.pieceVm.phenotypes.find((value) => value.id === lead.id);
  assert.equal(phenotype.reports, 12);
  assert.equal(phenotype.ready, true);
  assert.ok(phenotype.selectionBias > 0 && phenotype.selectionBias <= .06);
  assert.equal(phenotype.sonicVoices, 1);
  assert.equal(after.runtime.pieceVm.selection.parentId, lead.id);
  assert.equal(after.runtime.pieceVm.selection.nativeBias, phenotype.selectionBias);
  const summary = nativeSnapshot(after).split("\n")[0].split("\t");
  assert.deepEqual(summary.slice(64, 71), [lead.id, "12", "1", phenotype.score.toFixed(4),
    phenotype.selectionBias.toFixed(4), "1", lead.role]);
  assert.ok(app.archive.pieceVm.phenotypes.windows.some(([id]) => id === lead.id));
  let descendant = null;
  for (let attempt = 0; attempt < 4 && !descendant; attempt += 1) descendant = await app.evolvePieceVm();
  assert.ok(descendant?.selectionEvidence);
  const evidence = descendant.selectionEvidence;
  assert.equal(evidence.parentId, descendant.parent);
  assert.ok(["phenotype-lead", "champion-control", "branch-diversity", "curriculum-chain"].includes(evidence.policy));
  assert.equal(evidence.combinedScore, evidence.staticScore + evidence.nativeBias);
  assert.ok(evidence.nativeBias >= -.06 && evidence.nativeBias <= .06);
  const parentPhenotype = app.snapshot().runtime.pieceVm.phenotypes
    .find((value) => value.id === descendant.parent);
  assert.equal(evidence.phenotypeReports, parentPhenotype?.reports || 0);
  assert.equal(evidence.nativeBias, parentPhenotype?.selectionBias || 0);
  assert.deepEqual(app.archive.pieceVm.lineage.find((value) => value.id === descendant.id).selectionEvidence,
    evidence);
  const policyState = app.snapshot().runtime.pieceVm;
  assert.equal(policyState.policyBandit.algorithm, "ucb1");
  assert.ok(policyState.policyBandit.trials >= 1);
  assert.deepEqual(policyState.lastEvolution.policySchedule.slice(0, 3),
    ["phenotype-lead", "champion-control", "branch-diversity"]);
  assert.deepEqual(policyState.lastEvolution.operatorSchedule.slice(0, 3),
    ["variation", "machinery", "exchange"]);
  assert.equal(policyState.operatorBandit.algorithm, "ucb1");
  assert.equal(policyState.operatorBandit.dimension, "mutation-operator-family");
  assert.ok(policyState.operatorBandit.trials >= 1);
  assert.equal(policyState.outcomeModel.dimension, "requested-mutation");
  assert.ok(policyState.outcomeModel.trials >= 1);
  assert.equal(policyState.curriculum.strategy, "retained-capability-breadth");
  assert.ok(policyState.curriculum.trials >= 1);
  assert.equal(policyState.lastEvolution.mutationSchedule.length, 4);
  assert.equal(policyState.lastEvolution.mutationPreferenceSchedule.length, 4);
  assert.ok(policyState.lastEvolution.mutationPreferenceSchedule.every((value) => value.length >= 2));
  assert.ok(app.archive.pieceVm.policyBandit.trials.some((value) =>
    value.candidateId === descendant.id && value.policy === evidence.policy && value.admitted));
  assert.ok(app.archive.pieceVm.operatorBandit.trials.some((value) =>
    value.candidateId === descendant.id && value.policy === descendant.operatorFamily && value.admitted));
  assert.ok(app.archive.pieceVm.outcomeModel.trials.some((value) =>
    value.candidateId === descendant.id && value.requestedMutation === descendant.requestedMutation && value.admitted));
  assert.ok(app.archive.pieceVm.curriculum.trials.some((value) =>
    value.candidateId === descendant.id && value.admitted));
});

test("recent native PieceVM lease survives embodiment churn without weakening identity checks", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "piecevm-lease-"));
  let clock = 10_000;
  const app = await createSortSoupServer({ port: 0, cycleMs: 0, pieceVmCycleMs: 0, pieceVmBatch: 8,
    ledgerPath: join(root, "specimens.sqlite"), now: () => clock,
    pieceVmNativeVerifier: async (candidate) => ({ valid: true, engine: "test-native",
      bytecodeHash: candidate.program.bytecodeHash, frameHashes: candidate.frameHashes.slice(0, 2), checkedFrames: 2 }) });
  context.after(() => app.stop());
  assert.ok(await app.evolvePieceVm());
  const base = `http://127.0.0.1:${app.address.port}`;
  assert.equal((await fetch(`${base}/api/native`)).status, 200);
  const before = app.snapshot();
  const leased = before.runtime.pieceVm.embodiment.find((value) => value.address === "B1") ||
    before.runtime.pieceVm.embodiment[0];
  const raster = before.displayPrograms.find((value) => value.address === leased.address);
  assert.ok(leased && raster);
  let current = leased;
  for (let attempt = 0; attempt < 32 && current.id === leased.id; attempt += 1) {
    await app.evolvePieceVm();
    current = app.snapshot().runtime.pieceVm.embodiment.find((value) => value.address === leased.address) || current;
  }
  assert.notEqual(current.id, leased.id);
  const resident = { id: raster.id, hp: 88, ageMs: 15_000, life: 0, failedReprobes: 0,
    resolution: leased.resolution, actual: .08, variance: .16, spatial: .2,
    noise: .09, coherence: .78, muddiness: .08,
    pieceVmId: leased.id, pieceVmRole: leased.role,
    pieceVmProbeCarrier: leased.probeCarrier, sonicVoices: 2 };
  const report = (value) => fetch(`${base}/api/health`, { method: "POST",
    headers: { "content-type": "application/json" }, body: JSON.stringify({ displayFps: 141, residents: [value] }) });
  assert.equal((await report(resident)).status, 200);
  assert.equal(app.snapshot().runtime.pieceVm.phenotypes.find((value) => value.id === leased.id)?.reports, 1);
  assert.equal((await report({ ...resident, pieceVmRole: "forged" })).status, 400);
  clock += 90_001;
  assert.equal((await report(resident)).status, 400);
});

test("PieceVM admissions survive through the farm's own Git edition", async () => {
  const root = await mkdtemp(join(tmpdir(), "piecevm-history-"));
  const verifier = async (candidate) => ({ valid: true, engine: "test-native",
    bytecodeHash: candidate.program.bytecodeHash, frameHashes: candidate.frameHashes.slice(0, 2), checkedFrames: 2 });
  const first = await createSortSoupServer({ cycleMs: 0, pieceVmCycleMs: 0, pieceVmBatch: 8,
    historyRoot: root, pieceVmNativeVerifier: verifier });
  const admitted = await first.evolvePieceVm();
  assert.ok(admitted);
  const edition = first.history.snapshot();
  assert.equal(edition.lastEdition.reason, "piecevm-admission");
  assert.equal(edition.lastEdition.pieceVm.championId, admitted.id);
  const policyTrials = first.snapshot().runtime.pieceVm.policyBandit.trials;
  const operatorTrials = first.snapshot().runtime.pieceVm.operatorBandit.trials;
  const outcomeTrials = first.snapshot().runtime.pieceVm.outcomeModel.trials;
  const curriculumTrials = first.snapshot().runtime.pieceVm.curriculum.trials;
  assert.ok(policyTrials >= 1);
  assert.ok(operatorTrials >= 1);
  assert.ok(outcomeTrials >= 1);
  assert.ok(curriculumTrials >= 1);
  assert.ok((await first.history.pieceVmLineage()).some((value) => value.id === admitted.id));
  await first.stop();
  const restored = await createSortSoupServer({ cycleMs: 0, pieceVmCycleMs: 0,
    historyRoot: root, pieceVmNativeVerifier: verifier });
  assert.equal(restored.restored, true);
  assert.equal(restored.pieceVmNursery.championId, admitted.id);
  assert.equal(restored.pieceVmNursery.champion.native.engine, "test-native");
  assert.equal(restored.snapshot().runtime.pieceVm.policyBandit.trials, policyTrials);
  assert.equal(restored.snapshot().runtime.pieceVm.operatorBandit.trials, operatorTrials);
  assert.equal(restored.snapshot().runtime.pieceVm.outcomeModel.trials, outcomeTrials);
  assert.equal(restored.snapshot().runtime.pieceVm.curriculum.trials, curriculumTrials);
  assert.ok(restored.pieceVmNursery.lineage.some((value) => value.id === admitted.parent));
  await restored.stop();
});

test("full PixelGrooves stay behind a bounded per-resident loopback endpoint", async (context) => {
  const app = await createSortSoupServer({ port: 0, cycleMs: 0 });
  context.after(() => app.stop());
  const base = `http://127.0.0.1:${app.address.port}`;
  const state = await fetch(`${base}/api/state`).then((response) => response.json());
  const resident = state.programs.find((program) => program.domain === "raster");
  assert.ok(resident);
  assert.equal(resident.sample.groove, undefined);
  assert.ok(resident.sample.grooveBytes > 0);
  const groove = await fetch(`${base}/api/groove/${resident.id}`).then((response) => response.json());
  assert.equal(groove.id, resident.id);
  assert.equal(groove.record.valid, true);
  assert.equal(groove.groove.length, resident.sample.grooveBytes * 2);
  const live = Buffer.from(groove.groove, "hex");
  live[GROOVE_LAYOUT.tracks.state.base * 3 + 4] ^= 1;
  const persisted = await fetch(`${base}/api/groove-state`, {
    method: "POST", headers: { "content-type": "application/json" },
    body: JSON.stringify({ id: resident.id, groove: live.toString("hex") }),
  });
  assert.equal(persisted.status, 200);
  const reread = await fetch(`${base}/api/groove/${resident.id}`).then((response) => response.json());
  assert.equal(reread.groove, live.toString("hex"));
  const ppm = await fetch(`${base}/api/groove/${resident.id}?format=ppm`);
  assert.equal(ppm.headers.get("content-type"), "image/x-portable-pixmap");
  assert.match(Buffer.from(await ppm.arrayBuffer()).subarray(0, 16).toString(), /^P6\n160 160\n255\n/);
});

test("visible margin probes are exact, persisted, and offered to the verified nursery", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "piecefarm-probes-"));
  const app = await createSortSoupServer({ port: 0, cycleMs: 0, pieceVmCycleMs: 0,
    ledgerPath: join(root, "specimens.sqlite"), now: () => 4242,
    pieceVmNativeVerifier: async (candidate) => ({ valid: true, engine: "test-native",
      bytecodeHash: candidate.program.bytecodeHash, frameHashes: candidate.frameHashes.slice(0, 2), checkedFrames: 2 }) });
  context.after(() => app.stop());
  const base = `http://127.0.0.1:${app.address.port}`;
  const visible = app.snapshot().displayPrograms;
  let target = null;
  for (const program of visible) {
    const groove = await fetch(`${base}/api/groove/${program.id}`).then((response) => response.json());
    const hydrated = { ...program, sample: { ...program.sample, groove: groove.groove } };
    for (let address = GROOVE_LAYOUT.tracks.sequence.base;
         address < GROOVE_LAYOUT.tracks.sequence.base + GROOVE_LAYOUT.tracks.sequence.pixels;
         address += 8) {
      const probe = decodeMarginProbe(hydrated, address, { at: 4242, requestedBy: "loopboy" });
      if (probe.capability) { target = probe; break; }
    }
    if (target) break;
  }
  assert.ok(target);
  assert.equal(target.protected, true);
  assert.equal(target.layer, "core");
  const response = await fetch(`${base}/api/margin-probe`, {
    method: "POST", headers: { "content-type": "application/json" },
    body: JSON.stringify({ id: target.id, address: target.address, requestedBy: "loopboy!!!" }),
  });
  assert.equal(response.status, 200);
  const result = await response.json();
  assert.equal(result.nurseryEligible, true);
  assert.equal(result.probe.requestedBy, "loopboy");
  assert.deepEqual(app.snapshot().runtime.marginProbe, result.probe);
  assert.deepEqual(app.ledger.latestMarginProbe(), result.probe);
  const summary = nativeSnapshot(app.snapshot()).split("\n")[0].split("\t");
  assert.deepEqual(summary.slice(47, 52), [target.id, String(target.address), "sequence", target.capability, "loopboy"]);
  const rejected = await fetch(`${base}/api/margin-probe`, {
    method: "POST", headers: { "content-type": "application/json" },
    body: JSON.stringify({ id: target.id, address: 556, requestedBy: "loopboy" }),
  });
  assert.equal(rejected.status, 400);
  while (app.snapshot().runtime.marginProbe.status === "pending") await app.evolvePieceVm();
  const outcome = app.snapshot().runtime.marginProbe;
  assert.ok(["admitted", "no-admission"].includes(outcome.status));
  assert.ok(outcome.attempts >= 1 && outcome.attempts <= 8);
  if (outcome.status === "admitted") {
    const descendant = app.pieceVmNursery.lineage.find((value) => value.id === outcome.descendantId);
    assert.equal(descendant.environmentDonor, target.id);
    assert.equal(descendant.environmentCapability, target.capability);
    assert.equal(descendant.environmentProbe.address, target.address);
    assert.equal(descendant.native.valid, true);
    assert.equal(outcome.descendant.id, outcome.descendantId);
    assert.ok(["resident", "lineage"].includes(outcome.descendant.state));
    assert.equal(outcome.descendant.nativeValid, true);
    assert.equal(outcome.descendant.profilesValid, true);
    assert.deepEqual(outcome.descendant.propagation, {
      descendants: 0, residents: 1, maxGeneration: descendant.generation,
      frontierId: outcome.descendantId, champion: true,
    });
    const outcomeSummary = nativeSnapshot(app.snapshot()).split("\n")[0].split("\t");
    assert.deepEqual(outcomeSummary.slice(52, 56), ["admitted", outcome.descendantId,
      String(outcome.attempts), outcome.descendant.state]);
    assert.deepEqual(outcomeSummary.slice(56, 59).map(Number), [outcome.descendant.descendants,
      outcome.descendant.generation, outcome.descendant.children]);
    assert.deepEqual(outcomeSummary.slice(59, 64), ["0", "1", String(descendant.generation),
      outcome.descendantId, "1"]);
    assert.ok(app.snapshot().runtime.pieceVm.embodiment.some((value) =>
      value.id === outcome.descendantId && value.probeCarrier));
    const carrierPrograms = nativeSnapshot(app.snapshot(), [{ address: "A1", resolution: 64,
      role: "probe-carrier", probeCarrier: true, program: descendant }]).split("\n")
      .filter((line) => line.startsWith("P\t") && line.split("\t")[19]);
    assert.equal(carrierPrograms.length, 1);
    assert.ok(carrierPrograms.every((line) => line.split("\t")[21] === "1"));
    assert.equal(carrierPrograms[0].split("\t")[22], outcome.descendantId);
    assert.equal(carrierPrograms[0].split("\t")[23], "probe-carrier");
  }
});

test("native mutable Groove state is validated and retained by the authority", async (context) => {
  const app = await createSortSoupServer({ port: 0, cycleMs: 0 });
  context.after(() => app.stop());
  const base = `http://127.0.0.1:${app.address.port}`;
  const state = await fetch(`${base}/api/state`).then((response) => response.json());
  const resident = state.programs.find((program) => program.domain === "raster");
  const initial = await fetch(`${base}/api/groove/${resident.id}`).then((response) => response.json());
  const bytes = Buffer.from(initial.groove, "hex");
  bytes.writeUInt32LE(73, 656 * 3 + 12); // mutable sequence-pass counter
  const update = await fetch(`${base}/api/groove-state`, {
    method: "POST", headers: { "content-type": "application/json" },
    body: JSON.stringify({ id: resident.id, groove: bytes.toString("hex") }),
  });
  assert.equal(update.status, 200);
  const retained = await fetch(`${base}/api/groove/${resident.id}`).then((response) => response.json());
  assert.equal(retained.record.valid, true);
  assert.equal(Buffer.from(retained.groove, "hex").readUInt32LE(656 * 3 + 12), 73);
  assert.equal(retained.record.protectedHash, initial.record.protectedHash);
});

test("outside LLM proposals require a capability and are provenance logged", async (context) => {
  const root = await mkdtemp(join(tmpdir(), "sort-soup-server-"));
  const log = join(root, "inbox", "proposals.ndjson");
  const app = await createSortSoupServer({ port: 0, cycleMs: 0, proposalToken: "bounded-cap", proposalLog: log });
  context.after(() => app.stop());
  const base = `http://127.0.0.1:${app.address.port}`;
  const denied = await fetch(`${base}/api/propose`, {
    method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify({ source: "(sort quick)" }),
  });
  assert.equal(denied.status, 401);
  const accepted = await fetch(`${base}/api/propose`, {
    method: "POST",
    headers: { authorization: "Bearer bounded-cap", "content-type": "application/json" },
    body: JSON.stringify({ source: "(hybrid 7 insertion heap)", origin: "prox:outside-model" }),
  });
  assert.equal(accepted.status, 202);
  const record = JSON.parse((await readFile(log, "utf8")).trim());
  assert.equal(record.origin, "prox:outside-model");
  assert.equal(record.source, "(hybrid 7 insertion heap)");
  assert.ok(!JSON.stringify(record).includes("bounded-cap"));
});

test("sort-soup refuses public listeners", async () => {
  await assert.rejects(() => createSortSoupServer({ host: "0.0.0.0", port: 0 }), /refuses non-loopback/);
});

test("bounded workers evaluate deterministic batches off the server thread", async (context) => {
  const app = await createSortSoupServer({
    port: 0, cycleMs: 20, workerCount: 2, evaluationWindow: 8, clockAuthorityUrl: null,
  });
  context.after(() => app.stop());
  const initial = app.snapshot().iteration;
  const deadline = Date.now() + 5_000;
  while (app.snapshot().iteration < initial + 8 && Date.now() < deadline) {
    await new Promise((resolve) => setTimeout(resolve, 20));
  }
  const state = app.snapshot();
  assert.ok(state.iteration >= initial + 8);
  assert.equal((state.iteration - initial) % 8, 0);
  assert.equal(state.runtime.workerCount, 2);
  assert.equal(state.runtime.evaluationWindow, 8);
  assert.ok(state.runtime.evaluationsPerSecond > 0);
  assert.equal(state.runtime.targetEvaluationsPerSecond, 100);
  assert.ok(state.runtime.evaluationWindowsCompleted > 0);
  assert.ok(state.runtime.lastEvaluationWindowMs > 0);
  assert.equal(state.runtime.evaluationPool.size, 2);
  assert.ok(state.runtime.evaluationPool.completed >= 8);
  assert.ok(state.runtime.evaluationPool.peakQueued >= 6);
  assert.ok(state.checkpoint.estimatedMs > 0);
});

test("look-ahead evaluation preserves archive order across worker counts", async () => {
  const origin = new NoveltyArchive({ seed: "ordered-lookahead" });
  origin.seedClassics();
  origin.seedFoundations();
  const serialArchive = NoveltyArchive.fromJSON(origin.toJSON());
  const parallelArchive = NoveltyArchive.fromJSON(origin.toJSON());
  const serialPool = new EvaluationPool({ size: 1 });
  const parallelPool = new EvaluationPool({ size: 4 });
  try {
    const serial = await evaluateProposalWindow({
      archive: serialArchive, evaluationPool: serialPool, count: 12, authorityUtcMs: 0,
    });
    const parallel = await evaluateProposalWindow({
      archive: parallelArchive, evaluationPool: parallelPool, count: 12, authorityUtcMs: 0,
    });
    const signature = ({ candidates }) => candidates.map((candidate) => ({
      id: candidate.id, iteration: candidate.iteration, retained: candidate.retained,
      status: candidate.status, niche: candidate.niche, quality: candidate.quality,
    }));
    assert.deepEqual(signature(parallel), signature(serial));
    assert.deepEqual(parallelArchive.toJSON(), serialArchive.toJSON());
    assert.ok(parallelPool.snapshot().peakQueued >= 8);
    assert.equal(parallel.scheduling.policy, "predicted-cost-descending/original-admission");
  } finally {
    await Promise.all([serialPool.close(), parallelPool.close()]);
  }
});

test("cost-aware dispatch starts expensive profiles first but admits original lineage order", async () => {
  const proposals = [
    { source: "(raster (add 1 2 3))", options: { profile: "quarter" } },
    { source: "(raster (blur) (edges))", options: { profile: "double" } },
    { source: "(raster (mix 1 0 64))", options: { profile: "half" } },
  ];
  const calls = [];
  const archive = {
    proposeMutation() { return proposals.shift(); },
    submitEvaluation(evaluation) { return evaluation.source; },
  };
  const evaluationPool = {
    async evaluate(proposal) {
      calls.push(proposal.source);
      return { source: proposal.source, options: proposal.options, candidate: {} };
    },
  };
  const result = await evaluateProposalWindow({ archive, evaluationPool, count: 3 });
  assert.deepEqual(calls, [
    "(raster (blur) (edges))",
    "(raster (mix 1 0 64))",
    "(raster (add 1 2 3))",
  ]);
  assert.deepEqual(result.candidates, [
    "(raster (add 1 2 3))",
    "(raster (blur) (edges))",
    "(raster (mix 1 0 64))",
  ]);
  assert.ok(result.scheduling.predictedMax > result.scheduling.predictedMin);
  assert.equal(scheduleProposalEvaluations([]).length, 0);
  assert.ok(proposalEvaluationCost({ source: "(raster (blur))", options: { profile: "double" } }) >
    proposalEvaluationCost({ source: "(raster (blur))", options: { profile: "quarter" } }));
});

test("evaluation look-ahead is bounded by the worker pool and authority cap", async () => {
  await assert.rejects(() => createSortSoupServer({
    port: 0, cycleMs: 20, workerCount: 4, evaluationWindow: 3,
  }), /evaluation window/);
  await assert.rejects(() => createSortSoupServer({
    port: 0, cycleMs: 20, workerCount: 4, evaluationWindow: 257,
  }), /evaluation window/);
});
