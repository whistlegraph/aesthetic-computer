import test from "node:test";
import assert from "node:assert/strict";
import { workspaceDaylight, workspacePalette, followWorkspaceTheme } from "../src/workspace-theme.mjs";

test("the shared clock follows LA daylight regardless of the visitor's timezone", () => {
  assert.equal(workspaceDaylight(Date.parse("2026-10-18T20:00:00Z")), 1);
  assert.equal(workspaceDaylight(Date.parse("2026-10-18T08:00:00Z")), 0);
  const dawn = workspaceDaylight(Date.parse("2026-10-18T14:00:00Z"));
  assert.ok(dawn > 0 && dawn < 1);
});
const luminance = hex => hex.match(/[0-9a-f]{2}/g).map(c => parseInt(c, 16) / 255)
  .map(c => c <= .04045 ? c / 12.92 : ((c + .055) / 1.055) ** 2.4)
  .reduce((sum, c, i) => sum + c * [.2126, .7152, .0722][i], 0);
const contrast = (a, b) => (Math.max(luminance(a), luminance(b)) + .05) / (Math.min(luminance(a), luminance(b)) + .05);
test("every status remains readable through sunrise and sunset, including the ink switch", () => {
  for (const state of ["blank", "working", "rendering", "complete", "awaiting", "interrupted", "stale"]) {
    for (let step = 0; step <= 100; step++) {
      const p = workspacePalette(state, step / 100);
      assert.ok(contrast(p.ink, p.background) >= 4.5, `${state} text at ${step}`);
      assert.ok(contrast(p.accent, p.background) >= 3, `${state} accent at ${step}`);
    }
  }
});
test("a returning tab refreshes daylight and browser chrome without losing its completion state", () => {
  let time = Date.parse("2026-10-18T20:00:00Z"), visibility, color;
  const vars = {}, root = { dataset: {}, style: { setProperty: (k, v) => vars[k] = v }, ownerDocument: {
    addEventListener: (_, listener) => visibility = listener,
    removeEventListener: (_, listener) => assert.equal(listener, visibility),
  } };
  const theme = followWorkspaceTheme({ root, meta: { setAttribute: (_, value) => color = value }, now: () => time });
  try {
    theme.set("complete");
    const light = vars["--background"];
    time = Date.parse("2026-10-19T08:00:00Z");
    visibility();
    assert.equal(root.dataset.phase, "complete");
    assert.equal(root.dataset.theme, "dark");
    assert.notEqual(light, vars["--background"]);
    assert.equal(color, vars["--background"]);
  } finally { theme.dispose(); }
});
