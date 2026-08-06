#!/usr/bin/env node
// A bounded, invisible browser preflight for Captutor pathfinding.
//
// One CDP connection returns the current URL, visible controls, stable locator
// candidates, focus, viewport, and React Flow nodes/handles/edges. Optionally
// save a compositor screenshot without putting any Frame/Puppet UI on screen.

import { writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { withSession } from "../lib/cdp.mjs";

const args = process.argv.slice(2);
const value = (flag, fallback = null) => {
  const index = args.indexOf(flag);
  return index >= 0 ? args[index + 1] : fallback;
};
const match = value("--match", "fuser.studio");
const screenshotPath = value("--screenshot");
const compact = args.includes("--compact");

if (args.includes("--help") || args.includes("-h")) {
  console.log("usage: node bin/cdp-frame.mjs [--match text] [--screenshot path.png] [--compact]");
  process.exit(0);
}

const timeoutMs = Number(value("--timeout", "15000"));
const watchdog = setTimeout(() => {
  console.error(`CDP frame timed out after ${timeoutMs}ms`);
  process.exit(124);
}, timeoutMs);
try {
  const result = await withSession(match, async (cdp) => {
    const frame = await cdp.frame();
    if (screenshotPath) {
      const out = resolve(screenshotPath);
      writeFileSync(out, await cdp.screenshot());
      frame.screenshot = out;
    }
    return frame;
  });
  console.log(JSON.stringify(result, null, compact ? 0 : 2));
} catch (error) {
  console.error(JSON.stringify({
    ok:false,
    code:error.code || "CDP_FRAME_FAILED",
    message:error.message,
    details:error.details || null,
  }));
  process.exitCode = 1;
} finally {
  clearTimeout(watchdog);
}
