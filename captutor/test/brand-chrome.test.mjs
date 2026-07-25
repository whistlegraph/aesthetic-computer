import test from "node:test";
import assert from "node:assert/strict";

import { brandChromeFilter, layoutBrandChrome, separateBrandChromeFilter } from "../lib/brand-chrome.mjs";

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

test("separate brand chrome accepts a canonical wordmark asset instead of approximating type", () => {
  const crop = { x:0.42, y:0.43, width:0.32, height:0.13, aspect:1.25 };
  const layout = layoutBrandChrome({
    id:"client-wordmark",
    markAsset:"/tmp/mark.svg",
    labelAsset:"/tmp/canonical-wordmark.svg",
    labelAssetCrop:crop,
    formats:{ docs:{ markSideFraction:0.017, labelPxFraction:0.024 } },
  }, { width:2560, height:1440, format:"docs" });
  assert.equal(layout.labelAsset, "/tmp/canonical-wordmark.svg");
  assert.deepEqual(layout.labelAssetCrop, crop);
  assert.equal(layout.markSide, 24);
  assert.equal(layout.labelPx, 35);
});

test("canonical wordmarks can ripple per character without moving into the mark", () => {
  const cuts = [[0, 0.2], [0.2, 0.4], [0.4, 0.6], [0.6, 0.8], [0.8, 1]];
  const layout = layoutBrandChrome({
    id:"client-ripple",
    markAsset:"/tmp/mark.svg",
    labelAsset:"/tmp/canonical-wordmark.svg",
    labelCharacterCuts:cuts,
    periodSec:8,
    leftMarkCenterY:0.748,
    leftLabelCenterY:0.8,
    characterMotion:{ driftFraction:0.0014, periodSec:3.2, shimmerPeriodSec:2.4 },
  }, { width:2560, height:1440, format:"docs" });
  const filter = separateBrandChromeFilter(layout, cuts.length);
  assert.deepEqual(layout.labelCharacterCuts, cuts);
  assert.equal(layout.characterMotion.driftPx, 2);
  assert.match(filter, /alpha\(X,Y\).*sin\(2\*PI\*T\/2\.400/);
  assert.match(filter, /sin\(2\*PI\*t\/3\.200/);
  assert.match(filter, /main_h\*0\.80000.*sin\(2\*PI\*t\/8\.000\)/);
});

test("light-mode client chrome can use dark glyphs and a sharp colored hanging shadow", () => {
  const layout = layoutBrandChrome({
    id:"client-light",
    markAsset:"/tmp/mark.svg",
    label:"Client",
    font:"/tmp/client.ttf",
    markColor:"#17151a",
    labelColor:"#17151a",
    shadow:{ color:"#a58cbc", opacity:92, blur:0.55, x:1, y:2 },
  }, { width:2560, height:1440, format:"docs" });
  assert.equal(layout.markColor, "#17151a");
  assert.equal(layout.labelColor, "#17151a");
  assert.deepEqual(layout.shadow, { color:"#a58cbc", opacity:92, blur:0.55, x:1, y:2 });
});
