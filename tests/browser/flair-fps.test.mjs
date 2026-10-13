// Prompt frame-rate gate. Run on a hardware-accelerated, visible Chrome target.
// Measures browser requestAnimationFrame cadence, not distinct canvas paints.
// Glaze keeps its normal resolution/settings in every mode.
// AC_TEST_URL=http://localhost:8889 node tests/browser/flair-fps.test.mjs

import { writeFileSync } from "node:fs";
import { join } from "node:path";
import { ACSession, scenario, report, CONFIG } from "./ac-harness.mjs";

const SAMPLE_MS = Number(process.env.FLAIR_FPS_SAMPLE_MS || 5000);
const TARGET = Number(process.env.FLAIR_FPS_FLOOR || 60);
if (!Number.isFinite(SAMPLE_MS) || SAMPLE_MS < 1000 || !Number.isFinite(TARGET) || TARGET < 60) {
  throw new Error("Use samples of at least 1000 ms and a target of at least 60 fps");
}

async function sampleFrames(page) {
  return page.evaluate((duration) => new Promise((resolve) => {
    let start, previous;
    const intervals = [];
    function tick(now) {
      if (start === undefined) start = now;
      else intervals.push(now - previous);
      previous = now;
      const elapsed = now - start;
      if (elapsed < duration) return requestAnimationFrame(tick);
      const sorted = intervals.slice().sort((a, b) => a - b);
      resolve({ frames: intervals.length, elapsedMs: elapsed,
        fps: intervals.length * 1000 / elapsed,
        p95Ms: sorted[Math.ceil(sorted.length * 0.95) - 1], maxMs: sorted.at(-1) });
    }
    requestAnimationFrame(tick);
  }), SAMPLE_MS);
}

const ac = await ACSession.open();
const results = [];
let gpuName;
const errors = [];
ac.page.on("pageerror", (error) => errors.push(error.message));
try {
  const diagnostics = await ac.browser.target().createCDPSession();
  const { gpu } = await diagnostics.send("SystemInfo.getInfo");
  gpuName = gpu.devices.map((device) => device.deviceString).join(", ");
  console.log(`  GPU: ${gpuName}`);
  await diagnostics.detach();
  for (const [name, piece, active] of [
    ["flair-on", "prompt:flair", false],
    ["nostars", "prompt:nostars", false],
    ["flair-off", "prompt:noflair", false],
    ["active-prompt", "prompt:flair", true],
  ]) {
    await scenario(`${name}: ${TARGET} fps`, async (expect) => {
      await ac.page.goto(`${CONFIG.baseURL}/${piece}`, { waitUntil: "domcontentloaded", timeout: 30000 });
      await ac.page.waitForFunction(() => window.acBOOTED && window.preloaded &&
        document.querySelector("#aesthetic-computer canvas"), { timeout: 20000 });
      if (active) {
        await ac.type("notepat");
        await ac.page.waitForFunction(() => document.querySelector("#software-keyboard-input")?.value === "notepat");
      }
      await ac.wait(3500);
      // Screenshot work must finish before the timed sample.
      await ac.shot(name);
      const sample = await sampleFrames(ac.page);
      // Count whole expected intervals: 60 Hz timestamps can resolve to 59.999
      // fps. This only discards the final fractional interval, never a % margin.
      const expectedFrames = Math.floor(sample.elapsedMs * TARGET / 1000);
      const passed = sample.frames >= expectedFrames;
      results.push({ name, ...sample, expectedFrames, passed });
      console.log(`  ${sample.fps.toFixed(3)} fps; p95 ${sample.p95Ms.toFixed(2)} ms; ${sample.frames}/${expectedFrames} intervals`);
      expect(passed, `meets the ${TARGET} fps frame budget`);
      if (active) {
        await ac.press("Enter");
        await ac.page.waitForFunction(() => location.pathname === "/notepat", { timeout: 15000 });
        expect(true, "typed command opens notepat");
      }
    });
  }
  await scenario("complete, healthy run", async (expect) => {
    expect(results.length === 4, "all four frame samples are present");
    expect(errors.length === 0, `no page errors (${errors.length})`);
  });
  writeFileSync(join(CONFIG.shotDir, "frame-performance.json"), JSON.stringify({
    url: CONFIG.baseURL, gpu: gpuName, viewport: ac.page.viewport(),
    browser: await ac.browser.version(), targetFps: TARGET, sampleMs: SAMPLE_MS,
    results, errors,
  }, null, 2));
} finally {
  await ac.close();
}
process.exit(report());
