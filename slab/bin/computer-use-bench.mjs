#!/usr/bin/env node
// Bounded, local-only measurements. No model calls, user tabs, or native input.
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { chromium } from "playwright-core";
import { SemanticBrowser } from "../lib/puppet-semantic.mjs";
import { captureFrame } from "./frame.mjs";

const summarize = values => {
  const sorted = [...values].sort((a, b) => a - b);
  return { samples: sorted.length, medianMs: sorted[Math.floor(sorted.length / 2)],
    p95Ms: sorted[Math.ceil(sorted.length * 0.95) - 1], maxMs: sorted.at(-1),
    under100ms: sorted.filter(ms => ms < 100).length };
};
const round = n => +n.toFixed(2);
const report = { measuredAt: new Date().toISOString(), browser: {}, native: [] };
const dir = await mkdtemp(join(tmpdir(), "computer-use-bench-"));
let context, service;
try {
  context = await chromium.launchPersistentContext(dir, {
    channel: "chrome", headless: true, args: ["--remote-debugging-port=0"],
  });
  const port = (await readFile(join(dir, "DevToolsActivePort"), "utf8")).split("\n")[0];
  service = new SemanticBrowser(() => `http://127.0.0.1:${port}`);
  const page = context.pages()[0];
  await page.setContent(`<label>Name<input></label><button onclick="document.querySelector('output').textContent='Saved '+(++window.saves)">Save</button><output>Pending</output><script>window.saves=0</script>`);
  const cdp = await context.newCDPSession(page);
  const { targetInfo } = await cdp.send("Target.getTargetInfo");
  await cdp.detach();
  const target = targetInfo.targetId;
  for (const [action, args] of [
    ["snapshot", {}],
    ["fill", { locator: { label: "Name" }, value: "sample" }],
    ["click", { locator: { role: "button", name: "Save" }, after: { locator: { text: "Saved" } } }],
  ]) {
    const times = [];
    for (let i = 0; i < 26; i++) {
      // Every click must cause a NEW transition; an old 'Saved' label is not
      // evidence that a subsequent click reached the application.
      const sampleArgs = action === "click"
        ? { ...args, after: { locator: { text: `Saved ${i + 1}`, exact: true } } }
        : args;
      const start = performance.now();
      const result = await service.run(action, { target, ...sampleArgs });
      if (result.verification?.ok === false) throw new Error(`fixture ${action} did not verify`);
      if (action === "click" && result.verification?.ok !== true) throw new Error('Click did not return successful verification');
      times.push(round(performance.now() - start));
    }
    report.browser[action] = { firstMs: times.shift(), ...summarize(times) };
  }
} finally {
  await service?.close();
  await context?.close();
  await rm(dir, { recursive: true, force: true });
}

// Only successful captures count. Locked displays and missing grants must not
// look like low-latency screenshots. This records no pixels or screen text.
if (process.argv.includes("--native")) {
  for (const [mode, options] of [["pixels-ax", { noOCR: true, noVisual: true }], ["no-ocr", { noOCR: true }], ["fast-ocr", { fast: true }], ["accurate-ocr", {}]]) {
    const times = [], stages = [];
    let visualCacheHits = 0;
    let skipped;
    for (let i = 0; i < 5; i++) {
      const start = performance.now();
      const { env, jpg } = await captureFrame("local", { ...options, memory: true, quietOverlay: true });
      if (env.capture !== "ok" || !jpg?.length) { skipped = env.capture; break; }
      if (options.noVisual && env.visual_suppressed !== "requested") { skipped = "native Frame needs update for noVisual"; break; }
      times.push(round(performance.now() - start));
      stages.push(env.timings_ms);
      if (env.visual_cache_hit) visualCacheHits++;
    }
    report.native.push({ mode, ...summarize(times), visualCacheHits, ...(skipped ? { skipped } : {}), stages });
  }
}
console.log(JSON.stringify(report, null, 2));
