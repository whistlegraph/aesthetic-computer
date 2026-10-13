// Read-only interface benchmark. Optional AC_BASELINE_URL runs alternating A/B
// samples against two servers. No request interception: it can disturb workers.
import assert from "node:assert/strict";
import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { ACSession, CONFIG } from "./ac-harness.mjs";

const runs = Number(process.env.AC_PERF_RUNS || 3);
assert.ok(Number.isInteger(runs) && runs > 0 && runs <= 10);
const probe = readFileSync(new URL("../../apple/scripts/prompt-latency.js", import.meta.url), "utf8");
const ac = await ACSession.open();
const results = [];
try {
  await ac.page.setCacheEnabled(false);
  await ac.page.evaluateOnNewDocument(() => {
    window.addEventListener("ac:booted", () => { window.__interfaceReadyAt = performance.now(); }, { once: true });
  });
  const targets = process.env.AC_BASELINE_URL
    ? [["baseline", process.env.AC_BASELINE_URL], ["current", CONFIG.baseURL]]
    : [["current", CONFIG.baseURL]];
  for (let run = 0; run < runs; run++) for (const [name, url] of targets) {
    await ac.page.goto(`${url}/prompt:flair`, { waitUntil: "domcontentloaded", timeout: 30000 });
    await ac.page.waitForFunction(() => window.acBOOTED && window.preloaded, { timeout: 30000 });
    await ac.wait(2500);
    const readyMs = await ac.page.evaluate(() => window.__interfaceReadyAt);
    const idle = await ac.performanceProfile({ durationMs: 3000 });
    // Activate the input/audio before measuring steady typing; first-key startup
    // is intentionally excluded, and the probe records timings/lengths only.
    await ac.type("n");
    await ac.wait(700);
    await ac.page.evaluate(probe);
    await ac.type("otepat notepat notepat", { delay: 100 });
    await ac.wait(700);
    const latency = await ac.page.evaluate(() => window.acInputLatencyProbe.result());
    assert.ok(latency.keys > 0 && latency.keysVisible === latency.keys,
      "every measured key must reach a tagged canvas frame (worker mode required)");
    const active = await ac.performanceProfile({ durationMs: 3000 });
    results.push({ name, url, run, readyMs, idle, active, latency });
    console.log(JSON.stringify({ name, run, readyMs, idleFps: idle.fps, activeFps: active.fps,
      keyToCanvas: latency.keyToRendered, keyToNextRAF: latency.keyToNextRAF }));
  }
  writeFileSync(join(CONFIG.shotDir, "interface-performance.json"), JSON.stringify({
    browser: await ac.browser.version(), viewport: ac.page.viewport(), results,
    note: "Cache-disabled local/browser timings. Canvas submission and next RAF do not measure physical display latency.",
  }, null, 2));
} finally {
  await ac.close();
}
