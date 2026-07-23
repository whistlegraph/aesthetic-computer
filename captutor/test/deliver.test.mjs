import assert from "node:assert/strict";
import test from "node:test";

import { fullDesktopCrop } from "../lib/deliver.mjs";

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
