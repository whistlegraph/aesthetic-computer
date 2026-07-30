import assert from "node:assert/strict";
import test from "node:test";

import { captionCacheKey, fullDesktopCrop } from "../lib/deliver.mjs";

test("caption cache identity changes with rendered copy", () => {
  const options = {
    width:1200, px:58, font:"Arial.ttf", color:null,
  };
  const first = captionCacheKey({
    ...options, words:[{ text:"The" }, { text:"circular" }, { text:"point" }],
  });
  assert.equal(first, captionCacheKey({
    ...options, words:[{ text:"The" }, { text:"circular" }, { text:"point" }],
  }));
  assert.notEqual(first, captionCacheKey({
    ...options, words:[{ text:"The" }, { text:"circular" }, { text:"connection" }, { text:"point" }],
  }));
});

test("full-desktop delivery preserves an exact-size Stage negative", () => {
  assert.equal(
    fullDesktopCrop({ source: { w: 2560, h: 1440 }, target: { w: 2560, h: 1440 } }),
    "",
  );
});

test("full-desktop delivery center-crops a taller HiDPI clamshell negative", () => {
  assert.equal(
    fullDesktopCrop({ source: { w: 2560, h: 1662 }, target: { w: 2560, h: 1440 } }),
    "crop=2560:1440:0:111,",
  );
});

test("full-desktop delivery refuses smaller or window-shaped negatives", () => {
  assert.throws(
    () => fullDesktopCrop({ source: { w: 1920, h: 1080 }, target: { w: 2560, h: 1440 } }),
    /must be 2560px wide/,
  );
  assert.throws(
    () => fullDesktopCrop({ source: { w: 2560, h: 1200 }, target: { w: 2560, h: 1440 } }),
    /at least 1440px tall/,
  );
});
