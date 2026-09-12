import assert from "node:assert/strict";
import test from "node:test";
import { checkForUpdate, currentVersion, installed, isNewer } from "../src/updates.mjs";

test("versions compare numerically, not alphabetically", () => {
  assert.equal(isNewer("0.10.0", "0.9.9"), true, "0.10 is newer than 0.9");
  assert.equal(isNewer("1.0.0", "0.99.99"), true);
  assert.equal(isNewer("0.4.0", "0.4.0"), false);
  assert.equal(isNewer("0.3.9", "0.4.0"), false);
});

// Every unparseable comparison must fail toward not updating. A tool that
// overwrites itself because it could not read a version number is worse than
// one that never updates at all.
test("anything unreadable means no update", () => {
  for (const [l, c] of [["", "0.4.0"], ["x.y.z", "0.4.0"], ["0.4.0", ""], [null, "0.4.0"], [undefined, undefined]]) {
    assert.equal(isNewer(l, c), false, `${JSON.stringify(l)} vs ${JSON.stringify(c)}`);
  }
});

// The rule that protects development: this repository is a checkout, so it must
// never see itself as updatable, whatever the server says.
test("a checkout is never an install, and never updates", async () => {
  assert.equal(installed(), false, "the repo copy must not carry an install stamp");
  const served = async () => ({
    ok: true,
    json: async () => ({ version: "99.0.0", sha256: "f".repeat(64), tarball: "/easel.tar.gz" }),
  });
  const update = await checkForUpdate({ fetch: served, force: true });
  assert.equal(update, null, "a checkout must refuse an update even when one exists");
});

test("a failed check is silent rather than an error", async () => {
  const broken = async () => { throw new Error("offline"); };
  assert.equal(await checkForUpdate({ fetch: broken, force: true }), null);
  const wrong = async () => ({ ok: true, json: async () => ({ nope: true }) });
  assert.equal(await checkForUpdate({ fetch: wrong, force: true }), null);
});

test("the current version is readable", () => {
  assert.match(currentVersion(), /^\d+\.\d+\.\d+$/);
});
