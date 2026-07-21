import assert from "node:assert/strict";
import test from "node:test";

import screenplay, { PERFORMANCE_TABS } from "../screenplays/performance-test.mjs";

test("performance screenplay has five live tabs and complete accessible narration", () => {
  assert.equal(PERFORMANCE_TABS.length + 1, 5, "Fuser control tab plus four motion tabs");
  assert.equal(screenplay.preserveTabs, true);
  assert.equal(screenplay.billable, false);
  assert.ok(screenplay.beats.length >= 8);
  for (const beat of screenplay.beats) {
    for (const locale of ["en", "es", "zh-CN"]) {
      assert.equal(typeof beat.say[locale], "string");
      assert.ok(beat.say[locale].length > 20, `${locale} narration is substantive`);
    }
  }
});
