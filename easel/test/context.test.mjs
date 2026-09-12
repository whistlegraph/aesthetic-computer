import assert from "node:assert/strict";
import { existsSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";
import { BUNDLE } from "../bin/sync-context.mjs";

const EASEL = join(dirname(fileURLToPath(import.meta.url)), "..");

// The bundle is the whole reason an installed Easel is worth having over the
// vendor CLI it drives. A missing file is not a cosmetic problem — it is the
// tool silently becoming general-purpose.
test("every bundled guide is present and carries its provenance", () => {
  for (const [from, to, subject] of BUNDLE) {
    const path = join(EASEL, "context", to);
    assert.ok(existsSync(path), `context/${to} is missing — run npm run context`);
    const body = readFileSync(path, "utf8");
    assert.ok(body.includes(from), `context/${to} should name where it came from`);
    assert.ok(body.includes(subject), `context/${to} should say what it is for`);
    assert.ok(body.length > 500, `context/${to} is suspiciously short`);
  }
});

test("the bundle is small enough to travel", () => {
  const total = BUNDLE.reduce(
    (sum, [, to]) => sum + readFileSync(join(EASEL, "context", to), "utf8").length,
    0,
  );
  // Not a style rule — a tripwire. If the bundle ever approaches the size of the
  // source it ships beside, someone has started shipping the repository.
  assert.ok(total < 120_000, `context bundle is ${(total / 1024).toFixed(0)} KB, which is too much to carry`);
});
