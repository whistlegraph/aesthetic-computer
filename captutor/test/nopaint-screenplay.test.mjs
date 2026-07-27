import assert from "node:assert/strict";
import test from "node:test";

import screenplay from "../screenplays/nopaint-first-painting.mjs";

test("No Paint first-run screenplay is a fail-closed non-billable tutorial", () => {
  assert.equal(screenplay.signIn, false);
  assert.equal(screenplay.billable, false);
  assert.equal(screenplay.desktopFrame, true);
  assert.equal(screenplay.beats.length, 5);
  assert.ok(screenplay.openingCard);
  assert.ok(screenplay.closingCard);
  assert.deepEqual(screenplay.acceptance.requiredChecks, [
    "nopaint_3_booted",
    "no_preserved_painting",
    "paint_committed_proposal",
    "pause_round_trip",
    "save_requested",
  ]);
  for (const beat of screenplay.beats) {
    assert.equal(typeof beat.say, "string");
    assert.ok(beat.say.length > 30);
    assert.equal(typeof beat.do, "function");
  }
});
