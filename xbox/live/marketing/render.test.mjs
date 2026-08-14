import assert from "node:assert/strict";
import test from "node:test";

import { demoOriginMs, offlineReplayAddress } from "./render.mjs";
import { replayOvenProfile } from "./replay-oven.mjs";

test("the Replay Oven keeps the match HUD on fixed-step burns", () => {
  assert.equal(replayOvenProfile.hud, true);
  assert.equal(
    offlineReplayAddress("http://oven.test", "tizze50"),
    "http://oven.test/tizze50?social-preview&replay-oven&offline-render&reel-hud",
  );
});

test("reviewers can still request a world-only burn", () => {
  assert.equal(
    offlineReplayAddress("http://oven.test", "tizze50", { hud: false }),
    "http://oven.test/tizze50?social-preview&replay-oven&offline-render",
  );
});

// The seam the reel of 2026-08-13 fell through: the sound was cut against the
// live screencast and the picture against the demo, 277ms apart, and nothing
// in the pipeline knew the two clocks were different.
test("the live recording finds the demo's tick zero under head-start and noise", () => {
  const events = [[0, "wind", -1], [1, "countdown", -1], [61, "countdown", -1],
    [121, "countdown", -1], [181, "fighters-lock", -1], [181, "move", 0],
    [194, "punch", 0], [194, "punch", 1], [196, "partdamage", 1],
    [240, "kick", 0], [301, "bodyhit", 1], [360, "dash", 0]];
  const origin = 1_700_000_000_000;
  // The recorder attached mid-round, so the first two events were missed, and
  // a stray from the previous round's result card leads the list.
  const live = [{ event: "result-card", player: -1, at: origin - 900 },
    ...events.slice(2).map(([tick, event, player]) =>
      ({ event, player, at: origin + tick * (1000 / 60) }))];
  assert.equal(Math.round(demoOriginMs(live, events)), origin);
});

test("an unalignable recording says so instead of guessing", () => {
  assert.equal(demoOriginMs([{ event: "ko", player: 0, at: 12 }],
    [[4, "ko", 0]]), null);
  assert.equal(demoOriginMs([], []), null);
});
