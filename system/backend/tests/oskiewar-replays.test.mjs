import assert from "node:assert/strict";
import test from "node:test";
import { validateDemo } from "../../netlify/functions/oskiewar-replays.mjs";

const demo = {
  format: "ac.oskiedemo", version: 1, game: "oskiewar",
  simulation: "oskiewar-physics-1", tickRate: 60,
  matchId: "ow-befaru-nitova-gopasu", matchName: "befaru-nitova-gopasu",
  startedAt: 1785870000000, durationTicks: 1200,
  fighters: ["@JEFFREY", "DUMMY"], winner: "@JEFFREY",
  finalRoundWins: [5, 0], commands: [[0, 0, 0], [5, 0, 18]],
  events: [[5, "kick", 0, 1, 0]], rounds: [[0, 1, 12, 0]],
  checkpoints: [Array(26).fill(0)],
};

test("accepts a bounded versioned demo stream", () => {
  assert.equal(validateDemo(demo), null);
});

test("rejects unknown formats and malformed command streams", () => {
  assert.equal(validateDemo({ ...demo, format: "quake-demo" }),
    "Unsupported demo format");
  assert.equal(validateDemo({ ...demo, commands: [[0, 0, 999]] }),
    "Invalid commands");
  assert.equal(validateDemo({ ...demo, matchName: "not-random",
    matchId: "ow-not-random" }), "Invalid match name");
});
