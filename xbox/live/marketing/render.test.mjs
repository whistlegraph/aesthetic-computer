import assert from "node:assert/strict";
import test from "node:test";

import { demoOriginMs, demoOutcome, doorForSeed, doors, offlineDemoAddress, offlineReplayAddress } from "./render.mjs";
import { replayOvenProfile } from "./replay-oven.mjs";

test("the Replay Oven keeps the match HUD on fixed-step burns", () => {
  assert.equal(replayOvenProfile.hud, true);
  assert.equal(
    offlineReplayAddress("http://oven.test", "tizze50"),
    "http://oven.test/tizze50?social-preview&replay-oven&offline-render&reel-hud",
  );
});

test("the Replay Oven owns offline simulation and audio", () => {
  assert.ok(replayOvenProfile.offlinePasses.includes("simulation"));
  assert.ok(replayOvenProfile.offlinePasses.includes("audio"));
});

test("reviewers can still request a world-only burn", () => {
  assert.equal(
    offlineReplayAddress("http://oven.test", "tizze50", { hud: false }),
    "http://oven.test/tizze50?social-preview&replay-oven&offline-render",
  );
});

test("offline replay addresses carry fractional time without changing output fps", () => {
  assert.equal(
    offlineReplayAddress("http://oven.test", "tizze50",
      { timeScale: .25 }),
    "http://oven.test/tizze50?social-preview&replay-oven&offline-render&reel-hud&time-scale=0.25",
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

test("the oven deals each slot's door from its seed, and a month is a mix", () => {
  const dealt = { survival: 0, fight: 0 };
  for (let day = 1; day <= 30; day++)
    for (let index = 0; index < 3; index++) {
      const seed = `2026-09-${String(day).padStart(2, "0")}#${index}`;
      const door = doorForSeed(seed);
      assert.ok(doors.includes(door));
      assert.equal(doorForSeed(seed), door, "a seed always deals the same door");
      dealt[door]++;
    }
  assert.ok(dealt.survival >= 20 && dealt.fight >= 20,
    `a month of slots is a real mix: ${JSON.stringify(dealt)}`);
});

test("the offline demo address names its door so the game never rolls under the oven", () => {
  assert.match(offlineDemoAddress("http://x", "survival"), /&opponent=survival&reel-hud/);
  assert.match(offlineDemoAddress("http://x", "fight", { hud: false, timeScale: .5 }),
    /&opponent=fight&time-scale=0\.5$/);
  assert.throws(() => offlineDemoAddress("http://x", "coin"), /unknown door/);
});

test("a demo's outcome reads the same shape off a climb envelope and a fight demo", () => {
  const climb = demoOutcome({ simulation: "oskiewar-survival-1", cause: "SUMMIT",
    height: 7593, winner: "@BOT", durationTicks: 1134, roundName: "climb" });
  assert.equal(climb.mode, "survival");
  assert.equal(climb.succeeded, true);
  assert.equal(climb.level, 32);
  const tie = demoOutcome({ simulation: "oskiewar-physics-1", winner: null,
    durationTicks: 1838, roundName: "vuggo908",
    events: [[10, "punch", 1, -1, 0], [1838, "tie", -1, 0, 0]] });
  assert.deepEqual([tie.mode, tie.cause, tie.succeeded, tie.level],
    ["fight", "TIE", false, null]);
  const ko = demoOutcome({ simulation: "oskiewar-physics-1", winner: "@JEFFREY",
    durationTicks: 900, roundName: "ruffi297",
    events: [[600, "killcam", 0, 1, 1], [900, "roundwin", 0, 1, 1]] });
  assert.deepEqual([ko.cause, ko.succeeded, ko.winner], ["KO", true, "@JEFFREY"]);
  const decision = demoOutcome({ simulation: "oskiewar-physics-1", winner: "@OSKIE",
    durationTicks: 1838, events: [[1838, "roundwin", 1, 1, 2]] });
  assert.equal(decision.cause, "TIME");
});
