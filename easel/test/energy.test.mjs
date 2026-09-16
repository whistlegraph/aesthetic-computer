import assert from "node:assert/strict";
import test from "node:test";
import {
  Energy,
  energyReport,
  everyday,
  formatJoules,
  joulesFor,
  phoneCharges,
  profileFor,
  readUsage,
  relativeModels,
} from "../src/energy.mjs";

test("published active-parameter counts are marked apart from guessed ones", () => {
  assert.equal(profileFor("z-ai/glm-4.6").known, true);
  assert.equal(profileFor("anthropic/claude-sonnet-4.6").known, false);
  // An unrecognized name lands at the frontier class rather than the cheap end.
  assert.ok(profileFor("some-unreleased-thing").active >= 200);
  assert.equal(profileFor("claude-haiku-4-5-20251001").active, 40);
});

test("usage arrives under several spellings and reads the same", () => {
  const anthropic = readUsage({
    input_tokens: 10,
    output_tokens: 20,
    cache_read_input_tokens: 30,
    cache_creation_input_tokens: 40,
  });
  const cli = readUsage({
    inputTokens: 10,
    outputTokens: 20,
    cacheReadInputTokens: 30,
    cacheCreationInputTokens: 40,
  });
  assert.deepEqual(anthropic, cli);
  assert.deepEqual(readUsage({ input_tokens: 5, cached_input_tokens: 7 }), {
    input: 5,
    output: 0,
    cacheRead: 7,
    cacheWrite: 0,
  });
  assert.deepEqual(readUsage({ input_tokens: -1, output_tokens: null }).input, 0);
});

test("writing a token costs more than reading one, and reading a cached one costs least", () => {
  const model = "z-ai/glm-4.6";
  const out = joulesFor(readUsage({ output_tokens: 1000 }), model);
  const read = joulesFor(readUsage({ input_tokens: 1000 }), model);
  const cached = joulesFor(readUsage({ cache_read_input_tokens: 1000 }), model);
  assert.ok(out > read && read > cached);
  assert.ok(cached > 0, "a cached token is cheap, not free");
});

test("a bigger model costs more for identical work", () => {
  const tokens = readUsage({ input_tokens: 6000, output_tokens: 800 });
  const small = joulesFor(tokens, "z-ai/glm-4.6");
  const large = joulesFor(tokens, "openai/gpt-5.4");
  assert.ok(large > small * 2);
});

// The anchor the whole formula hangs on: a few hundred output tokens from a
// frontier-class model should land near the published per-prompt figures
// (Google 0.24 Wh median, Epoch ~0.3 Wh), or the estimate has drifted into
// numbers nobody has evidence for.
test("a frontier-class answer lands inside the published per-prompt range", () => {
  const wh = joulesFor(readUsage({ input_tokens: 400, output_tokens: 300 }), "openai/gpt-5.4") / 3600;
  assert.ok(wh > 0.05 && wh < 0.6, `${wh} Wh is outside the published range`);
});

test("the ledger totals per session and per model", () => {
  const energy = new Energy();
  energy.add("z-ai/glm-4.6", { input_tokens: 100, output_tokens: 50 });
  energy.add("openai/gpt-5.4", { input_tokens: 100, output_tokens: 50 });
  assert.equal(energy.turns, 2);
  assert.equal(energy.tokens.output, 100);
  assert.equal(energy.byModel.size, 2);
  assert.ok(energy.byModel.get("openai/gpt-5.4").joules > energy.byModel.get("z-ai/glm-4.6").joules);
  // A turn the engine reported no counts for is not a turn the meter saw.
  assert.equal(energy.add("z-ai/glm-4.6", {}), 0);
  assert.equal(energy.turns, 2);
});

test("the relative table is cheapest-first and normalized to it", () => {
  const rows = relativeModels(readUsage({ output_tokens: 500 }), "z-ai/glm-4.6");
  assert.equal(rows[0].ratio, 1);
  assert.ok(rows.at(-1).ratio > 1);
  assert.deepEqual([...rows].sort((a, b) => a.joules - b.joules), rows);
  assert.equal(rows.find((row) => row.current).label, "glm");
});

test("numbers are shown in units a person owns", () => {
  assert.match(formatJoules(0), /^0 Wh$/);
  assert.match(formatJoules(200), /Wh$/);
  assert.match(formatJoules(36000), /^10\.0 Wh$/);
  assert.match(everyday(100), /LED bulb/);
  assert.match(everyday(500000), /kettle/);
  assert.ok(phoneCharges(54000) > 0.9 && phoneCharges(54000) < 1.1);
});

test("the report says it is an estimate, with or without metered turns", () => {
  assert.match(energyReport(new Energy()).join("\n"), /No metered turns yet/);
  const energy = new Energy();
  energy.add("z-ai/glm-4.6", { input_tokens: 6000, output_tokens: 900, cache_read_input_tokens: 24000 });
  const report = energyReport(energy, "z-ai/glm-4.6").join("\n");
  assert.match(report, /this session/);
  assert.match(report, /Same conversation, other models/);
  assert.match(report, /← running/);
  assert.match(report, /Estimated from active parameters/);
  assert.match(report, /size undisclosed/, "guessed rows say so");
});

// `/model glm` names a model by its short hosted name. A session that never
// heard the resolved id back should still price the right model.
test("hosted short names resolve to the model they select", () => {
  assert.equal(profileFor("glm").active, profileFor("z-ai/glm-4.6").active);
  assert.equal(profileFor("deepseek").known, true);
});
