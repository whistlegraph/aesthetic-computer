import test from "node:test";
import assert from "node:assert/strict";
import { noPaintStartingPiece, noPaintHistoryTarget } from
  "../public/aesthetic.computer/lib/nopaint-navigation.mjs";

test("No Paint opens fresh tabs and resumes a reload without a visible seed", () => {
  const home = new URL("https://nopaint.art/");
  assert.equal(noPaintStartingPiece(home, "navigate", null), "nopaint~fresh");
  assert.equal(noPaintStartingPiece(home, "navigate", "123"), "nopaint~fresh");
  assert.equal(noPaintStartingPiece(home, "reload", "123"), "nopaint:123");
  assert.equal(noPaintStartingPiece(home, "reload", null), "nopaint~fresh");
  assert.equal(noPaintStartingPiece(new URL("https://nopaint.art/nopaint:123"), "navigate", null), "nopaint");
  assert.equal(noPaintStartingPiece(new URL("https://aesthetic.computer/"), "navigate", "123"), null);
});

test("only branded session URLs are reduced to the home address", () => {
  assert.deepEqual(noPaintHistoryTarget("/nopaint:123", "https://nopaint.art/"), { path: "/", seed: "123" });
  assert.deepEqual(noPaintHistoryTarget("/nopaint:123?workerbundle=1", "https://www.nopaint.art/"), {
    path: "/?workerbundle=1", seed: "123",
  });
  for (const path of ["/nopaint~archive~l4f0ipzy", "/gallery/", "/nopaint:new"]) {
    assert.deepEqual(noPaintHistoryTarget(path, "https://nopaint.art/"), { path, seed: null });
  }
  assert.deepEqual(noPaintHistoryTarget("/nopaint:123", "https://aesthetic.computer/"), {
    path: "/nopaint:123", seed: null,
  });
});
