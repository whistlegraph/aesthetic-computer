#!/usr/bin/env node
// A compact semantic frame of Fuser: DOM/state facts, source-backed intent,
// and optional screenshot inference for autonomous pathfinding.

import { writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { FUSER_INTELLIGENCE, readFuserFrame } from "../app-intelligence/fuser.mjs";
import { inferAppFrame } from "../lib/app-frame-inference.mjs";
import { withSession } from "../lib/cdp.mjs";
import { translator } from "../lib/i18n.mjs";

const args = process.argv.slice(2);
const value = (flag, fallback = null) => {
  const index = args.indexOf(flag);
  return index < 0 ? fallback : args[index + 1];
};
if (args.includes("--help") || args.includes("-h")) {
  console.log("usage: node bin/fuser-frame.mjs [--locale en] [--match fuser.studio] [--screenshot frame.png] [--infer] [--model model] [--compact]");
  process.exit(0);
}
const locale = value("--locale", "en");
const match = value("--match", FUSER_INTELLIGENCE.hostMatch);
const screenshotPath = value("--screenshot");
const compact = args.includes("--compact");
const infer = args.includes("--infer");
const timeoutMs = Number(value("--timeout", infer ? "140000" : "15000"));
const watchdog = setTimeout(() => {
  console.error(`Fuser frame timed out after ${timeoutMs}ms`);
  process.exit(124);
}, timeoutMs);

try {
  const result = await withSession(match, async (cdp) => {
    const frame = await readFuserFrame(cdp, { locale, t:translator(locale) });
    let screenshot;
    if (screenshotPath || infer) screenshot = await cdp.screenshot();
    if (screenshotPath) {
      frame.screenshot = resolve(screenshotPath);
      writeFileSync(frame.screenshot, screenshot);
    }
    if (infer) frame.visualInference = await inferAppFrame({
      frame, screenshot, model:value("--model") || undefined,
    });
    return frame;
  });
  console.log(JSON.stringify(result, null, compact ? 0 : 2));
} catch (error) {
  console.error(JSON.stringify({ ok:false, code:error.code || "FUSER_FRAME_FAILED",
    message:error.message, details:error.details || null }));
  process.exitCode = 1;
} finally {
  clearTimeout(watchdog);
}
