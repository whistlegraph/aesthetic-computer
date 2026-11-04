// Real WebGL: daylight contrast, transparent curtain and reversible theme switching.
import assert from "node:assert/strict";
import { ACSession, CONFIG } from "./ac-harness.mjs";
const ac = await ACSession.open();
try {
  await ac.page.setRequestInterception(true);
  ac.page.on("request", request => request.isNavigationRequest()
    ? request.respond({status: 200, contentType: "text/html", body: "<!doctype html><body></body>"})
    : request.continue());
  await ac.page.goto(`${CONFIG.baseURL}/__glaze-day`);
  const result = await ac.page.evaluate(async () => {
    const Glaze = await import("/aesthetic.computer/lib/glaze.mjs");
    const input = document.createElement("canvas");
    input.width = 32; input.height = 24;
    const ctx = input.getContext("2d");
    const results = [];
    // Daylight keeps paper bright and ink dark, retains transparency, and
    // switching it off must restore the exact night rendering.
    await Glaze.on(32, 24, { width: 96, height: 72 }, 96, 72, document.body, "prompt", () => {});
    const actualGL = Glaze.getCan().getContext("webgl2");
    const sample = day => {
      Glaze.render(1000, { x: 0, y: 0 }, day);
      const bytes = new Uint8Array(96 * 72 * 4);
      actualGL.readPixels(0, 0, 96, 72, actualGL.RGBA, actualGL.UNSIGNED_BYTE, bytes);
      return bytes;
    };
    ctx.fillStyle = "rgb(252,247,197)"; ctx.fillRect(0, 0, 32, 24);
    ctx.fillStyle = "rgb(40,30,90)"; ctx.fillRect(12, 8, 8, 8);
    Glaze.update(input);
    const night = sample(false), daylight = sample(true), restored = sample(false);
    const luma = [];
    for (let i = 0; i < daylight.length; i += 4)
      luma.push((daylight[i] + daylight[i + 1] + daylight[i + 2]) / 3);
    luma.sort((a, b) => a - b);
    ctx.clearRect(0, 0, 32, 24); Glaze.update(input);
    const clear = sample(true);
    results.push({name: "daylight", median: luma[Math.floor(luma.length / 2)],
      darkest: luma[0], restored: restored.every((v, i) => v === night[i]),
      transparent: clear.every((v, i) => i % 4 !== 3 || v === 0), error: actualGL.getError()});
    return results[0];
  });
  console.log(JSON.stringify(result));
  assert.equal(result.error, 0);
  assert.ok(result.median > 180 && result.darkest < 100, "paper and ink must remain legible");
  assert.ok(result.restored && result.transparent, "theme switching and clear curtain must survive");
} finally { await ac.close(); }
