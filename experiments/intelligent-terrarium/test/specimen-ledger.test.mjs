import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { SpecimenLedger } from "../src/specimen-ledger.mjs";
import { rgbPng, VisualCurator } from "../src/visual-curator.mjs";

function candidate(id, quality = .2, iteration = 1, profile = null) {
  return {
    id, iteration, domain: "raster", source: "(raster (shift 1 0))", parent: null,
    generation: 0, status: "resident", retained: true, aliveness: "alive", quality,
    novelty: .4, tags: ["flow", "coherent", "high-difference", "low-noise",
      ...(profile ? [`hardware-${profile}`] : [])],
    sample: { width: 128, height: 128, rgb: Buffer.alloc(128 * 128 * 3, 96).toString("hex"),
      energy: [{ actual: .09, potential: .6, variance: .2, spatial: .15, noise: .08, coherence: .53 }] },
  };
}

test("specimen ledger persists observations and stable display addresses", async () => {
  const root = await mkdtemp(join(tmpdir(), "piecefarm-ledger-"));
  const path = join(root, "specimens.sqlite");
  const ledger = await SpecimenLedger.open(path);
  for (let index = 0; index < 12; index += 1) ledger.observe(candidate(`specimen-${index}`, .1 + index / 100, index + 1));
  assert.equal(ledger.addressed().length, 12);
  assert.deepEqual(ledger.addressed().map((row) => row.address), ["A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", "A3", "B3", "C3", "D3"]);
  ledger.observe(candidate("strong-newcomer", .9, 20));
  assert.ok(ledger.addressed().some((row) => row.id === "strong-newcomer"));
  const replacement = candidate("memory-graft-replacement", .05, 21);
  ledger.observe(replacement);
  const target = ledger.addressed()[0];
  ledger.recordHealth(target.id, { hp: 7, at: 1000, strikes: 83 });
  ledger.recordIntervention(target.id, { at: 1001, strategy: "copy", donorId: "strong-newcomer", beforeHp: 7 });
  assert.equal(ledger.get(target.id).interventions, 1);
  ledger.recordOutcome(target.id, "culled");
  assert.equal(ledger.recordMarginProbe("strong-newcomer", {
    at: 1002, address: 64, track: "sequence", capability: "shift", requestedBy: "loopboy",
  }), true);
  assert.deepEqual(ledger.latestMarginProbe(), {
    id: "strong-newcomer", at: 1002, address: 64, track: "sequence",
    capability: "shift", requestedBy: "loopboy",
  });
  ledger.cull(target.id, "sustained-statistical-low-performance");
  assert.deepEqual(ledger.fillVacancies([replacement]), [{ address: target.address, id: replacement.id }]);
  assert.equal(ledger.get(target.id).status, "culled");
  assert.equal(ledger.addressed().find((row) => row.id === replacement.id).address, target.address);
  ledger.close();
  assert.ok((await readFile(path)).length > 0);
});

test("stable display addresses retain every available hardware class", async () => {
  const root = await mkdtemp(join(tmpdir(), "piecefarm-ledger-profiles-"));
  const ledger = await SpecimenLedger.open(join(root, "specimens.sqlite"));
  for (let index = 0; index < 12; index += 1)
    ledger.observe(candidate(`quarter-${index}`, .5 + index / 100, index + 1, "quarter"));
  for (const [index, profile] of ["half", "standard", "double"].entries())
    ledger.observe(candidate(`${profile}-resident`, .01, 20 + index, profile));
  const profiles = new Set(ledger.addressed().flatMap((row) => row.tags
    .filter((tag) => tag.startsWith("hardware-")).map((tag) => tag.slice(9))));
  assert.deepEqual([...profiles].sort(), ["double", "half", "quarter", "standard"]);
  assert.equal(ledger.addressed().length, 12);
  ledger.close();
});

test("visual curator only admits verified high-change low-noise residents", () => {
  const curator = new VisualCurator({ apiKey: "test", now: () => 1000 });
  assert.equal(curator.eligible(candidate("alive")), true);
  const noisy = candidate("noisy"); noisy.sample.energy[0].noise = .8;
  assert.equal(curator.eligible(noisy), false);
  const png = rgbPng(2, 2, Buffer.alloc(12, 127));
  assert.equal(png.subarray(1, 4).toString(), "PNG");
});

test("visual reviews become bounded inferred categories without changing HP", async () => {
  const root = await mkdtemp(join(tmpdir(), "piecefarm-ledger-"));
  const ledger = await SpecimenLedger.open(join(root, "specimens.sqlite"));
  const specimen = candidate("visually-reviewed", .72, 44);
  ledger.observe(specimen);
  const review = {
    quality: .81, coherence: .77, distinctiveness: .84, artifact: "banding",
    tags: ["nested loops", "Blue / Green", "nested loops"],
    description: "Concentric loops hold a stable diagonal relation.", recommendation: "retain",
    model: "test-vision", at: "2026-07-24T18:00:00.000Z", specimenId: specimen.id,
  };
  assert.equal(ledger.recordVisualReview(specimen.id, review), true);
  const addressed = ledger.addressed().find((row) => row.id === specimen.id);
  assert.ok(addressed.tags.includes("vision:nested-loops"));
  assert.ok(addressed.tags.includes("vision:blue-green"));
  assert.ok(addressed.tags.includes("vision:artifact-banding"));
  assert.equal(addressed.runtime_health, null);
  assert.deepEqual(ledger.curationStats(), {
    specimens: 1, observations: 1, reviews: 1,
    recommendations: { retain: 1, watch: 0, reject: 0 },
  });
  ledger.close();
});

test("visual curator requests enough structured output and reports failures", async () => {
  let requestBody = null;
  const review = {
    quality: .7, coherence: .8, distinctiveness: .6, artifact: "none",
    tags: ["bounded-form"], description: "One persistent bounded form.",
    criticism: "The edge rhythm is underdeveloped.", capability: "feedback",
    mutationHints: ["add-feedback"], recommendation: "watch",
  };
  const curator = new VisualCurator({
    apiKey: "test", now: () => 1_000, cooldownMs: 0,
    request: async (_url, options) => {
      requestBody = JSON.parse(options.body);
      return { ok: true, json: async () => ({ output: [{ content: [{ type: "output_text", text: JSON.stringify(review) }] }] }) };
    },
  });
  const result = await curator.consider(candidate("curated"));
  assert.equal(requestBody.max_output_tokens, 600);
  assert.match(requestBody.input[0].content[1].image_url, /^data:image\/png;base64,/);
  assert.equal(result.recommendation, "watch");
  assert.deepEqual(curator.telemetry(), {
    status: "armed", inflight: false, reviewed: 1, failures: 0, seen: 1,
    lastReviewAt: 1_000, lastSpecimenId: "curated", lastTrigger: "visual-novelty",
    lastRecommendation: "watch", lastError: null,
  });

  const broken = new VisualCurator({
    apiKey: "test", now: () => 2_000, cooldownMs: 0,
    request: async () => ({ ok: true, json: async () => ({ output: [{ content: [{ type: "output_text", text: "{" }] }] }) }),
  });
  await assert.rejects(() => broken.consider(candidate("broken-review")), /JSON/);
  assert.equal(broken.telemetry().failures, 1);
  assert.match(broken.telemetry().lastError, /JSON/);
});
