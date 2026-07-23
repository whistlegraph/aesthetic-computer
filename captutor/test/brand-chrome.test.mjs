import test from "node:test";
import assert from "node:assert/strict";

import { brandChromeFilter, layoutBrandChrome } from "../lib/brand-chrome.mjs";

const theme = {
  id:"client-x", asset:"/tmp/client.svg", periodSec:20,
  formats:{ vertical:{ edgeFraction:0.08, longSideFraction:0.18 } },
};

test("brand chrome scales from the delivery frame and honors format overrides", () => {
  const docs = layoutBrandChrome(theme, { width:2560, height:1440, format:"docs" });
  const vertical = layoutBrandChrome(theme, { width:1440, height:2560, format:"vertical" });
  assert.equal(docs.longSide, Math.round(1440 * 0.155));
  assert.equal(vertical.longSide, Math.round(1440 * 0.18));
  assert.equal(vertical.edgePx, Math.round(1440 * 0.08));
});

test("brand chrome faces two animated lockups inward", () => {
  const layout = layoutBrandChrome(theme, { width:1080, height:1920, format:"vertical" });
  const filter = brandChromeFilter(layout);
  assert.match(filter, /\[1:v\].*\[left\]/);
  assert.match(filter, /\[2:v\].*\[right\]/);
  assert.match(filter, /main_w-/);
  assert.match(filter, /sin\(2\*PI\*t\/20\.000\)/);
});

test("brand chrome rejects missing client assets in the contract", () => {
  assert.throws(
    () => layoutBrandChrome({ id:"empty" }, { width:1920, height:1080 }),
    /needs an asset/,
  );
});

test("client themes can remove flat badge colors without changing the renderer", () => {
  const layout = layoutBrandChrome({
    ...theme, transparentColors:[{ color:"#171717", fuzz:12 }],
  }, { width:1920, height:1080, format:"docs" });
  assert.deepEqual(layout.transparentColors, [{ color:"#171717", fuzz:12 }]);
});
