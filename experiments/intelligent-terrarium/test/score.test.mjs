import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

test("fullscreen score is only a bounded volumetric imagination buffer", async () => {
  const html = await readFile(new URL("../score.html", import.meta.url), "utf8");
  assert.match(html, /<canvas id="imagination"/);
  assert.match(html, /volumetric pixel lava lamp/);
  assert.match(html, /getContext\("webgl2"/);
  assert.match(html, /48 × 36 × 48 lattice/);
  assert.match(html, /gl\.drawArrays\(gl\.POINTS/);
  assert.match(html, /Sensory, spatial, drive, memory, action, and voice organs/);
  assert.match(html, /Miner guard auto-resumed/);
  assert.doesNotMatch(html, /<header|<aside|<footer|<img|CLIENT GATE · PREVIEW/);
  assert.doesNotMatch(html, /<script\s+src=|fetch\(|WebSocket|EventSource/);
});
