import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

test("fullscreen score presents organs, honest gated QR, and no network dependency", async () => {
  const html = await readFile(new URL("../score.html", import.meta.url), "utf8");
  assert.match(html, /<canvas id="score"/);
  assert.match(html, /Mediorgan score/);
  for (const organ of ["SENSORY", "SPATIAL", "DRIVE", "MEMORY", "ACTION", "VOICE"]) {
    assert.match(html, new RegExp(organ));
  }
  assert.match(html, /CLIENT GATE · PREVIEW/);
  assert.match(html, /remain closed pending Stage 5 approval/);
  assert.match(html, /score-assets\/join-preview\.svg/);
  assert.doesNotMatch(html, /<script\s+src=|fetch\(|WebSocket|EventSource/);
});
