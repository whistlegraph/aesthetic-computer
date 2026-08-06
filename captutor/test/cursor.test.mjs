import assert from "node:assert/strict";
import test from "node:test";

import { pagePointToScreen } from "../lib/cursor.mjs";

test("native cursor hotspot maps page pixels through browser chrome exactly", () => {
  const geometry = {
    screenX:40,
    screenY:24,
    outerWidth:1000,
    outerHeight:760,
    innerWidth:984,
    innerHeight:650,
  };
  // Eight points of symmetric side frame and 102 points of top chrome.
  assert.deepEqual(pagePointToScreen(geometry, { x:200, y:90 }), {
    x:248,
    y:216,
  });
});

test("native cursor mapping does not invent negative browser borders", () => {
  const geometry = {
    screenX:-1280,
    screenY:0,
    outerWidth:900,
    outerHeight:700,
    innerWidth:920,
    innerHeight:720,
  };
  assert.deepEqual(pagePointToScreen(geometry, { x:20, y:30 }), {
    x:-1260,
    y:30,
  });
});
