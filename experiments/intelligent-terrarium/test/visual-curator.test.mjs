import assert from "node:assert/strict";
import test from "node:test";
import { VisualCurator } from "../src/visual-curator.mjs";

const review = Object.freeze({
  quality: .72, coherence: .68, distinctiveness: .61, artifact: "none",
  tags: ["layered", "temporal"], description: "Layered forms hold across the field.",
  criticism: "The center lacks a persistent counter-motion.", capability: "cellular",
  mutationHints: ["add-cellular", "add-feedback"], recommendation: "retain",
});

function candidate() {
  return {
    id: "reviewable-1", domain: "raster", retained: true, aliveness: "alive", quality: .2,
    sample: { width: 1, height: 1, rgb: "804020", energy: [{ actual: .1, noise: .05, muddiness: .1, coherence: .7 }] },
  };
}

test("visual curator distinguishes lifecycle triggers and requests strict bounded recipes", async () => {
  const requests = [], records = [];
  const curator = new VisualCurator({
    apiKey: "test", cooldownMs: 0, now: () => 1_000,
    request: async (_url, init) => {
      requests.push(JSON.parse(init.body));
      return { ok: true, json: async () => ({ output: [{ content: [{ type: "output_text", text: JSON.stringify(review) }] }] }) };
    },
    onReview: (_candidate, record) => records.push(record),
  });
  const lifecycle = { samples: 8, healthMean: 88, healthyRatio: .9, healthRange: 36, healthStdDev: 9 };
  await curator.consider(candidate(), { trigger: "high-health", lifecycle });
  await curator.consider(candidate(), { trigger: "health-variability", lifecycle });
  assert.deepEqual(records.map((record) => record.trigger), ["high-health", "health-variability"]);
  assert.ok(requests.every((body) => body.store === false && body.text.format.strict === true));
  assert.ok(requests.every((body) => body.text.format.schema.required.includes("mutationHints")));
  assert.equal(curator.telemetry().seen, 2);
});

test("visual curator refuses premature lifecycle reviews", () => {
  const curator = new VisualCurator({ apiKey: "test" });
  assert.equal(curator.eligible(candidate(), {
    trigger: "high-health", lifecycle: { samples: 7, healthMean: 99, healthyRatio: 1 },
  }), false);
  assert.equal(curator.eligible(candidate(), {
    trigger: "health-variability", lifecycle: { samples: 8, healthRange: 12, healthStdDev: 12 },
  }), false);
});
