// Stream segregation — Wessel's acceptance test for a timbre distance.
//
// The point of the check is that it is FALSIFIABLE: a computed timbral
// distance has to predict something a listener can hear, and the thing they
// hear is binary. One line, or two.

import test from "node:test";
import assert from "node:assert";
import {
  parseMelody,
  streamSplitRisk,
} from "../system/public/aesthetic.computer/lib/melody-parser.mjs";
import {
  WAVE_TIMBRE,
  waveDistance,
} from "../system/public/aesthetic.computer/lib/sound/wave-timbre.mjs";

// A repeating line with the timbre alternating note to note — the shape of
// Wessel's Figure 2. Rate is half the prediction, so every test states its
// tempo: `FAST` puts the inter-onset interval at 150 ms, inside the range
// where segregation actually happens.
const alternating = (a, b) =>
  `{${a}}c.{${b}}d.{${a}}e.{${b}}c.{${a}}d.{${b}}e.`;
const FAST = { baseTempo: 150 };

test("a wide-brightness alternation splits into two streams", () => {
  const [warning] = streamSplitRisk(parseMelody(alternating("sine", "harp")), FAST);
  assert.ok(warning, "expected a warning for sine/harp alternation");
  assert.deepEqual(warning.waves.slice().sort(), ["harp", "sine"]);
  assert.ok(warning.splits, "sine↔harp is the widest gap measured — must split");
  assert.ok(
    warning.brightnessGap > 3,
    `expected > 3 Bark, got ${warning.brightnessGap}`,
  );
});

test("a narrow-brightness alternation stays one line even when fast", () => {
  // sine and triangle sit 0.08 Bark apart — the same tone, effectively.
  assert.deepEqual(
    streamSplitRisk(parseMelody(alternating("sine", "triangle")), FAST),
    [],
  );
});

test("slowing the same alternation down re-coheres it", () => {
  // The notes, pitches and timbres are identical in all three cases; only
  // the clock changes. This is the van Noorden half of the prediction.
  const notes = parseMelody(alternating("sine", "harp"));
  assert.ok(streamSplitRisk(notes, FAST).length > 0, "fast should split");
  assert.deepEqual(
    streamSplitRisk(notes, { baseTempo: 4000 }),
    [],
    "the same notes, slow, should hold together",
  );
});

test("a run must actually alternate", () => {
  // Same two timbres, but in blocks rather than note-to-note.
  const notes = parseMelody("{sine}c.d.e.{harp}c.d.e.");
  assert.deepEqual(streamSplitRisk(notes, FAST), []);
});

test("a rest breaks the run", () => {
  const notes = parseMelody("{sine}c.{harp}d.{sine}e.-{harp}c.{sine}d.");
  assert.deepEqual(streamSplitRisk(notes, FAST), []);
});

test("runs shorter than four notes are not reported", () => {
  const notes = parseMelody("{sine}c.{harp}d.{sine}e.");
  assert.deepEqual(streamSplitRisk(notes, FAST), []);
});

test("threshold 0 reports every alternating run with its score", () => {
  const notes = parseMelody(alternating("sine", "triangle"));
  const all = streamSplitRisk(notes, { ...FAST, threshold: 0 });
  assert.equal(all.length, 1);
  assert.ok(all[0].risk < 0.5, "sine/triangle scores low but is still reported");
  assert.equal(all[0].splits, false);
});

test("modes without a measured timbre are skipped, not guessed at", () => {
  // `gm` is a 128-patch picker, not a single voice.
  const notes = parseMelody(alternating("sine", "gm"));
  assert.equal(waveDistance("sine", "gm"), null);
  assert.deepEqual(streamSplitRisk(notes, FAST), []);
});

test("risk rises monotonically with measured brightness gap", () => {
  const gapFor = (other) => {
    const [w] = streamSplitRisk(parseMelody(alternating("sine", other)), {
      ...FAST,
      threshold: 0,
    });
    return w ? w.risk : 0;
  };
  const ordered = ["triangle", "square", "sawtooth", "harp"];
  const risks = ordered.map(gapFor);
  const gaps = ordered.map((w) =>
    Math.abs(WAVE_TIMBRE[w].brightness - WAVE_TIMBRE.sine.brightness),
  );
  for (let i = 1; i < risks.length; i += 1) {
    assert.ok(
      gaps[i] >= gaps[i - 1],
      `brightness gaps out of order at ${ordered[i]}`,
    );
    assert.ok(
      risks[i] >= risks[i - 1],
      `risk fell from ${ordered[i - 1]} (${risks[i - 1]}) to ${ordered[i]} (${risks[i]})`,
    );
  }
});
