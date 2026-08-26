import assert from "node:assert/strict";
import test from "node:test";

import { changelogCaption, dress, latestChangelogSubject } from "./segments.mjs";

test("changelog captions use the account voice and invite feedback", () => {
  assert.equal(
    changelogCaption("oskiewar v86: The Coconut Comes Down"),
    "oskiewar v86 — the coconut comes down.\n\ntell reelboy what to change.",
  );
});

test("changelog captions reject unrelated commit subjects", () => {
  assert.throws(() => changelogCaption("fix reel captions"),
    /no oskiewar changelog commit found/);
});

test("dressed segments keep their presentation fields and drop hashtags", () => {
  const copy = dress("fgc", 0, { under: "seeded 2026-08-26#0" });
  assert.equal(copy.hook, "hitbox on impact");
  assert.equal(copy.tail, "oskiewar.com");
  assert.equal(copy.lines.under, "seeded 2026-08-26#0");
  assert.deepEqual(copy.tags, []);
  assert.equal(copy.caption, changelogCaption(latestChangelogSubject()));
  assert.doesNotMatch(copy.caption, /#/);
  assert.ok(copy.caption.length <= 2200);
});
