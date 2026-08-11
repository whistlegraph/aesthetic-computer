import assert from "node:assert/strict";
import test from "node:test";

import { offlineReplayAddress } from "./render.mjs";
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
