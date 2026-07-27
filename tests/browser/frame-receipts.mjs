// Optional native Frame receipts for browser journeys. Puppeteer screenshots
// remain the portable CI artifact; Frame proves what the actual display showed
// during headed/staged runs, including browser chrome and compositor state.

import { execFileSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = dirname(dirname(dirname(fileURLToPath(import.meta.url))));
const FRAME = join(ROOT, "slab", "bin", "frame.mjs");

function localMachine() {
  return process.env.AC_FRAME_MACHINE || execFileSync(
    "scutil",
    ["--get", "LocalHostName"],
    { encoding: "utf8" },
  ).trim();
}

export function frameReceiptsEnabled() {
  return process.env.AC_FRAME_RECEIPTS === "1";
}

export function captureFrameReceipt(name, outputDir) {
  if (!frameReceiptsEnabled()) return null;
  mkdirSync(outputDir, { recursive: true });
  const output = join(outputDir, `${name}.frame.jpg`);
  try {
    execFileSync(process.execPath, [
      FRAME,
      localMachine(),
      "--screen",
      "--cursor",
      "--out",
      output,
      "--json",
    ], { stdio: ["ignore", "pipe", "inherit"], timeout: 20000 });
    console.log(`  🖼️  ${output}`);
    return output;
  } catch (error) {
    if (process.env.AC_FRAME_REQUIRED === "1") throw error;
    console.warn(`  ⚠️  Frame receipt unavailable: ${error.message}`);
    return null;
  }
}
