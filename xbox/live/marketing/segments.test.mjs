import assert from "node:assert/strict";
import test from "node:test";

import { dress, segments } from "./segments.mjs";

test("dressed segments keep their presentation fields and carry no caption or hashtags", () => {
  const copy = dress("fgc", 0, { under: "seeded 2026-08-26#0" });
  assert.equal(copy.hook, "hitbox on impact");
  assert.equal(copy.tail, "oskiewar.com");
  assert.equal(copy.lines.under, "seeded 2026-08-26#0");
  assert.equal(copy.caption, "");
  assert.deepEqual(copy.tags, []);
});

test("the market table holds only what is drawn", () => {
  assert.deepEqual(Object.keys(segments), ["fgc", "gamedev", "homebrew", "retro", "gen"]);
  for (const [key, segment] of Object.entries(segments)) {
    assert.ok(!("captions" in segment), `${key} carries no caption copy`);
    assert.ok(!("tags" in segment), `${key} carries no hashtags`);
    assert.ok(segment.hooks.length > 0);
  }
});
