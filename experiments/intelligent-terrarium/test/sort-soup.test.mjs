import assert from "node:assert/strict";
import test from "node:test";
import { compileRasterLisp, compileSortLisp, evaluateRasterProgram, evaluateSortProgram, NoveltyArchive, RASTER, readLisp } from "../src/sort-soup.mjs";
import { inspectPixelGroove } from "../src/pixel-groove.mjs";

test("minimal Lisp reader and compiler accept only the sorting score", () => {
  assert.deepEqual(readLisp("(hybrid 8 insertion merge)"), ["hybrid", 8, "insertion", "merge"]);
  assert.equal(compileSortLisp("(sort heap)").algorithm, "heap");
  assert.throws(() => compileSortLisp("(shell rm everything)"), /program must be/);
  assert.throws(() => readLisp("(sort quick"), /unclosed/);
});

test("raster Lisp writes deterministic fixed memory and reports its real effects", () => {
  const source = "(raster (shift 1 0) (mix 0 1 96) (edges) (solarize 80))";
  assert.equal(compileRasterLisp(source).operations.length, 4);
  const first = evaluateRasterProgram(source);
  const second = evaluateRasterProgram(source);
  assert.deepEqual(first, second);
  assert.equal(first.domain, "raster");
  assert.equal(first.sample.rgb.length, 128 * 128 * 3 * 2);
  assert.equal(first.sample.bytecode.length, 4 * 24 * 2);
  assert.equal(first.metrics.allocationBytes, RASTER.bytes * 2 + RASTER.pixels * 6 + RASTER.permaBytes);
  assert.equal(first.sample.groove.length, RASTER.permaBytes * 2);
  assert.equal(first.sample.grooveBytes, RASTER.permaBytes);
  assert.ok(first.metrics.reads > 0 && first.metrics.writes > 0);
  assert.equal(first.metrics.energy.length, 4);
  assert.ok(first.metrics.energy.every((stage) => stage.actual >= 0 && stage.potential > 0));
  assert.ok(first.descriptor.every((value) => value >= 0 && value <= 1));
  assert.throws(() => compileRasterLisp("(raster (network internet))"), /invalid bounded raster/);
});

test("geometric and flood forms are bounded Lisps with measured energy", () => {
  for (const source of [
    "(raster (line 0 0 127 127 255 0 90))",
    "(raster (triangle 4 120 64 4 124 120 20 240 160))",
    "(raster (flood 64 64 48 255 255 0))",
    "(raster (and 240 192 128) (or 3 12 48))",
  ]) {
    const candidate = evaluateRasterProgram(source);
    assert.equal(candidate.sample.width, 128);
    assert.ok(["alive", "dormant", "collapsed"].includes(candidate.aliveness));
  }
  assert.throws(() => compileRasterLisp("(raster (line 0 0 128 2 0 0 0))"), /invalid bounded raster/);
});

test("mixed hardware profiles execute in distinct fixed memory spaces", () => {
  const expected = { quarter: 32, half: 64, standard: 128, double: 256 };
  for (const [profile, resolution] of Object.entries(expected)) {
    const candidate = evaluateRasterProgram("(raster (line 0 0 127 127 255 0 90) (mix 1 0 96))", { profile });
    const record = inspectPixelGroove(candidate.sample.groove);
    assert.equal(candidate.sample.width, resolution);
    assert.equal(candidate.sample.height, resolution);
    assert.equal(candidate.sample.rgb.length, resolution * resolution * 3 * 2);
    assert.equal(candidate.hardware.fieldBytes, resolution * resolution * 3);
    assert.equal(candidate.hardware.readerHz, 240);
    assert.equal(record.hardware.name, profile);
    assert.equal(record.hardware.fieldBytes, resolution * resolution * 3);
  }
});

test("boxed computation verifies nested permeable worlds in fixed memory", () => {
  const candidate = evaluateRasterProgram("(raster (box 8 8 112 112 32 0) (box 32 32 64 64 96 3))");
  assert.equal(candidate.metrics.boxes, 2);
  assert.ok(candidate.tags.includes("boxed-computation"));
  assert.ok(candidate.tags.includes("nested-worlds"));
  assert.equal(Buffer.from(candidate.sample.bytecode, "hex")[0], 16);
  assert.equal(candidate.sample.width, 128);
  assert.equal(candidate.sample.height, 128);
});

test("sprite copy and paste are verified margin instructions with visible consequences", () => {
  const candidate = evaluateRasterProgram("(raster (copy 8 12 16 20 2) (shift 3 1) (paste 2 80 72 xor))");
  const bytecode = Buffer.from(candidate.sample.bytecode, "hex");
  assert.equal(bytecode[0], 17);
  assert.equal(bytecode[24], 3);
  assert.equal(bytecode[48], 18);
  assert.equal(candidate.metrics.energy[0].op, "copy");
  assert.equal(candidate.metrics.energy[0].abstract, true);
  assert.ok(candidate.metrics.energy[2].actual > 0);
  assert.ok(candidate.tags.includes("sprite-memory"));
  assert.equal(candidate.metrics.allocationBytes, RASTER.bytes * 2 + RASTER.pixels * 6 + RASTER.permaBytes);
  assert.throws(() => compileRasterLisp("(raster (copy 0 0 33 8 0))"), /invalid bounded raster/);
});

test("cellular field rules are bounded bytecode and create measured spatial behavior", () => {
  const candidate = evaluateRasterProgram("(raster (cellular 8 12) (mix 1 0 72))", { profile: "quarter" });
  assert.equal(compileRasterLisp(candidate.source).operations[0].name, "cellular");
  assert.equal(Buffer.from(candidate.sample.bytecode, "hex")[0], 19);
  assert.equal(candidate.metrics.cellularOps, 1);
  assert.ok(candidate.sample.energy.some((stage) => stage.op === "cellular" && stage.actual > 0));
  assert.ok(candidate.tags.includes("cellular-field"));
  assert.throws(() => compileRasterLisp("(raster (cellular 512 12))"), /invalid bounded raster/);
});

test("structured review hints become verifier-gated capability lineage", () => {
  const archive = new NoveltyArchive({ seed: "review-inheritance" });
  archive.seedClassics();
  const parent = [...archive.cells.values()].find((candidate) => candidate.domain === "raster");
  assert.ok(parent);
  assert.equal(archive.recordVisualReview(parent.id, {
    capability: "none", mutationHints: ["add-cellular"], recommendation: "watch",
  }), true);
  let proposal;
  for (let attempt = 0; attempt < 24 && !proposal?.options.reviewParent; attempt += 1)
    proposal = archive.proposeMutation(parent.id);
  assert.equal(proposal.options.reviewParent, parent.id);
  assert.match(proposal.source, /\(cellular /);
  const child = archive.submit(proposal.source, proposal.options);
  assert.equal(child.status === "rejected", false);
  assert.equal(child.capabilityLineage.reviewParent, parent.id);
  assert.equal(child.capabilityLineage.capability, "cellular");
  assert.equal(child.capabilityLineage.verifier, "bounded-js");
});

test("review advice survives specimen churn as a bounded bake recipe", () => {
  const archive = new NoveltyArchive({ seed: "orphan-review-advice" });
  archive.seedClassics();
  assert.equal(archive.recordVisualReview("evicted-specimen", {
    trigger: "health-variability", criticism: "The temporal field needs a stable counter-current.",
    capability: "feedback", mutationHints: ["add-feedback"], recommendation: "watch",
  }), false);
  let proposal;
  for (let attempt = 0; attempt < 100 && proposal?.options.reviewParent !== "evicted-specimen"; attempt += 1)
    proposal = archive.proposeMutation();
  assert.equal(proposal.options.reviewParent, "evicted-specimen");
  assert.match(proposal.source, /^\(raster \(mix /);
  const child = archive.submit(proposal.source, proposal.options);
  assert.equal(child.status === "rejected", false);
  assert.equal(child.capabilityLineage.criticism, "The temporal field needs a stable counter-current.");
  const restored = NoveltyArchive.fromJSON(archive.toJSON());
  assert.equal(restored.snapshot().capabilityBakes.advice, 1);
});

test("new margin foundations are injected once into restored farms", () => {
  const archive = new NoveltyArchive({ seed: "foundation-migration" });
  archive.submit("(raster (shift 1 0))");
  assert.equal(archive.seedFoundations().length, 1);
  assert.equal(archive.seedFoundations().length, 0);
  assert.ok([...archive.cells.values(), ...archive.recent].some((candidate) => candidate.tags?.includes("sprite-memory")));
});

test("every founding sorting program verifies and leaves an actual execution trace", () => {
  for (const algorithm of ["bubble", "insertion", "selection", "merge", "quick", "heap"]) {
    const candidate = evaluateSortProgram(`(sort ${algorithm})`, { origin: "classic" });
    assert.equal(candidate.status, "verified");
    assert.equal(candidate.type, "Vector<Int,n> -> SortedPermutation<Int,n>");
    assert.ok(candidate.sample.trace.length > 0);
    assert.deepEqual(candidate.sample.output, [...candidate.sample.output].sort((a, b) => a - b));
    assert.equal(candidate.descriptor.length, 5);
  }
});

test("novelty archive is deterministic, bounded, and distinguishes rejected syntax", () => {
  const first = new NoveltyArchive({ seed: "same-soup", maxRecent: 12 });
  const second = new NoveltyArchive({ seed: "same-soup", maxRecent: 12 });
  first.seedClassics(); second.seedClassics();
  for (let i = 0; i < 40; i += 1) { first.mutate(); second.mutate(); }
  assert.deepEqual(first.snapshot(), second.snapshot());
  assert.deepEqual(NoveltyArchive.fromJSON(first.toJSON()).snapshot(), first.snapshot());
  for (const candidate of first.recent.filter((item) => item.domain === "raster" && item.source?.includes("(paste "))) {
    const initialized = new Set();
    for (const operation of compileRasterLisp(candidate.source).operations) {
      if (operation.name === "copy") initialized.add(operation.args[4]);
      if (operation.name === "paste") assert.ok(initialized.has(operation.args[0]), `paste reads initialized sprite slot in ${candidate.source}`);
    }
  }
  assert.ok(first.snapshot().programs.length <= first.snapshot().capacity + 12);
  const rejected = first.submit("(network unrestricted)", { origin: "llm" });
  assert.equal(rejected.status, "rejected");
  assert.match(rejected.error, /program must be/);
  assert.ok(first.snapshot().domains.raster.coverage > 0);
  assert.equal(first.snapshot().memory.workingBytes, first.snapshot().active.hardware.workingBytes);
  assert.deepEqual(new Set(first.snapshot().programs.map((program) => program.hardware?.name).filter(Boolean)),
    new Set(["quarter", "half", "standard", "double"]));
});

test("mutation proposals remain deterministic before ordered worker admission", () => {
  const first = new NoveltyArchive({ seed: "worker-batch" });
  const second = new NoveltyArchive({ seed: "worker-batch" });
  first.seedClassics(); second.seedClassics();
  const a = Array.from({ length: 4 }, () => first.proposeMutation());
  const b = Array.from({ length: 4 }, () => second.proposeMutation());
  assert.deepEqual(a, b);
  for (let index = 0; index < a.length; index += 1) {
    first.submitEvaluation({ ...a[index], candidate: evaluateRasterProgram(a[index].source, a[index].options) });
    second.submitEvaluation({ ...b[index], candidate: evaluateRasterProgram(b[index].source, b[index].options) });
  }
  assert.deepEqual(first.toJSON(), second.toJSON());
});

test("legacy resident sets upconvert in place of identity into mixed groove hardware", () => {
  const archive = new NoveltyArchive({ seed: "legacy-mixed" });
  for (const source of ["(raster (shift 1 0))", "(raster (mix 1 0 96))", "(raster (edges))", "(raster (solarize 128))"])
    archive.submit(source, { origin: "legacy" });
  const legacy = archive.toJSON();
  legacy.cells = legacy.recent.map((candidate, index) => [`legacy:${index}`, structuredClone(candidate)]);
  for (const [, candidate] of legacy.cells) { delete candidate.hardware; delete candidate.groove; }
  for (const candidate of legacy.recent) { delete candidate.hardware; delete candidate.groove; }
  const ids = new Set(legacy.cells.map(([, candidate]) => candidate.id));
  const restored = NoveltyArchive.fromJSON(legacy);
  assert.deepEqual(new Set([...restored.cells.values()].map((candidate) => candidate.id)), ids);
  assert.deepEqual(new Set([...restored.cells.values()].map((candidate) => candidate.hardware.name)),
    new Set(["quarter", "half", "standard", "double"]));
  assert.ok([...restored.cells.values()].every((candidate) => inspectPixelGroove(candidate.sample.groove).valid));
});

test("archive retirement removes a resident without confusing red with death", () => {
  const archive = new NoveltyArchive({ seed: "retirement-test" });
  const resident = archive.submit("(raster (shift 1 0))");
  assert.equal(resident.retained, true);
  assert.equal(archive.retire(resident.id, "statistical-low-performer")?.id, resident.id);
  assert.equal([...archive.cells.values()].some((candidate) => candidate.id === resident.id), false);
  assert.equal(resident.status, "retired");
});

test("forking a yellow resident preserves its parent", () => {
  const archive = new NoveltyArchive({ seed: "fork-test" });
  const parent = archive.submit("(raster (mix 1 0 96))");
  const child = archive.fork(parent.id);
  assert.ok(child);
  assert.equal(child.parent, parent.id);
  assert.ok([...archive.cells.values()].some((candidate) => candidate.id === parent.id));
});
