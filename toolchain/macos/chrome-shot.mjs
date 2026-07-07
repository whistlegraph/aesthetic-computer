#!/usr/bin/env node
// chrome-shot.mjs — take a headless Chrome screenshot without leaving
// orphans behind. Bounds Chrome with a hard timeout, uses an ephemeral
// user-data-dir that gets cleaned up afterwards, and reaps any stale
// headless Chrome processes before launching.
//
// Background: ad-hoc `Google Chrome --headless=new ... --user-data-dir=/tmp/foo --screenshot=...`
// calls have a habit of getting stuck and accumulating — each pinning a
// `/tmp/chrome-*` dir of state. Enough of them stacked up and a plain
// `open -a "Google Chrome"` stopped opening windows at all. This wrapper
// is the safe path: always exits, always cleans up.
//
// Usage:
//   node toolchain/macos/chrome-shot.mjs <url> <out.png> [--size WxH] [--budget MS] [--wait MS]
//
// Flags:
//   --size WxH        viewport (default 1440x900)
//   --budget MS       --virtual-time-budget (default 8000)
//   --wait MS         hard timeout on Chrome itself (default 30000)
//   --full-page       capture full page height (Chrome's built-in flag)
//   --keep-profile    keep the user-data-dir after exit (debugging)
//
// Exit codes: 0 = screenshot written, 1 = Chrome failed, 2 = timed out.

import { spawn, spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, existsSync, statSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const args = process.argv.slice(2);
const positionals = [];
const opts = { size: "1440x900", budget: 8000, wait: 30000, fullPage: false, keepProfile: false };

for (let i = 0; i < args.length; i++) {
  const a = args[i];
  if (a === "--size") opts.size = args[++i];
  else if (a === "--budget") opts.budget = Number(args[++i]);
  else if (a === "--wait") opts.wait = Number(args[++i]);
  else if (a === "--full-page") opts.fullPage = true;
  else if (a === "--keep-profile") opts.keepProfile = true;
  else if (a === "-h" || a === "--help") {
    console.log("usage: chrome-shot.mjs <url> <out.png> [--size WxH] [--budget MS] [--wait MS] [--full-page] [--keep-profile]");
    process.exit(0);
  } else positionals.push(a);
}

const [url, outPath] = positionals;
if (!url || !outPath) {
  console.error("usage: chrome-shot.mjs <url> <out.png> [--size WxH] [--budget MS] [--wait MS] [--full-page] [--keep-profile]");
  process.exit(64);
}

const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
if (!existsSync(CHROME)) {
  console.error(`✗ Google Chrome not at ${CHROME}`);
  process.exit(1);
}

// Reap stale headless Chromes that have been running > 2 minutes — these
// are almost always hung from a prior call and hold a /tmp/chrome-* dir.
reapStaleHeadless();

// Clear a pre-existing output file BEFORE launching: the success poller
// below watches for a stable non-zero file size, and a stale file from a
// prior run reads as instantly-stable — Chrome gets killed before it ever
// renders and the old image is silently returned as if fresh.
try { rmSync(outPath, { force: true }); } catch {}

const profile = mkdtempSync(join(tmpdir(), "chrome-shot-"));
const chromeArgs = [
  "--headless=new",
  "--hide-scrollbars",
  "--no-sandbox",
  `--window-size=${opts.size}`,
  `--virtual-time-budget=${opts.budget}`,
  `--user-data-dir=${profile}`,
  `--screenshot=${outPath}`,
];
if (opts.fullPage) chromeArgs.push("--full-page");
chromeArgs.push(url);

const child = spawn(CHROME, chromeArgs, { stdio: ["ignore", "ignore", "inherit"] });
let timedOut = false;
let captured = false;
let lastSize = -1;
let stableTicks = 0;

const killer = setTimeout(() => {
  timedOut = true;
  try { child.kill("SIGKILL"); } catch {}
}, opts.wait);

// `--headless=new` writes the screenshot then often refuses to exit.
// Poll the output: once the file appears and its size has been stable
// for ~600ms, take that as success and kill Chrome ourselves.
const poller = setInterval(() => {
  if (!existsSync(outPath)) return;
  let size = 0;
  try { size = statSync(outPath).size; } catch { return; }
  if (size === 0) return;
  if (size === lastSize) {
    if (++stableTicks >= 3) {
      captured = true;
      clearInterval(poller);
      try { child.kill("SIGKILL"); } catch {}
    }
  } else {
    lastSize = size;
    stableTicks = 0;
  }
}, 200);

child.on("exit", (code, signal) => {
  clearTimeout(killer);
  clearInterval(poller);
  if (!opts.keepProfile) {
    try { rmSync(profile, { recursive: true, force: true }); } catch {}
  }
  if (captured) {
    console.log(outPath);
    return;
  }
  if (timedOut) {
    console.error(`✗ chrome timed out after ${opts.wait}ms — killed.`);
    process.exit(2);
  }
  if (code !== 0 && signal !== "SIGKILL") {
    console.error(`✗ chrome exited ${code} (${signal || "no signal"})`);
    process.exit(1);
  }
  if (!existsSync(outPath) || statSync(outPath).size === 0) {
    console.error(`✗ chrome exited clean but no screenshot at ${outPath}`);
    process.exit(1);
  }
  console.log(outPath);
});

function reapStaleHeadless() {
  // pgrep returns "PID elapsed_seconds command" via ps -o etime; use a
  // two-step lookup so we don't kill a screenshot that's still legitimately
  // running in another shell.
  const pgrep = spawnSync("pgrep", ["-f", "Google Chrome.*--headless"], { encoding: "utf8" });
  const pids = (pgrep.stdout || "").trim().split(/\s+/).filter(Boolean);
  if (!pids.length) return;
  const ps = spawnSync("ps", ["-o", "pid=,etimes=", ...pids], { encoding: "utf8" });
  const stale = [];
  for (const line of (ps.stdout || "").trim().split("\n")) {
    const [pid, etimes] = line.trim().split(/\s+/);
    if (Number(etimes) > 120) stale.push(pid);
  }
  if (!stale.length) return;
  console.error(`! reaping ${stale.length} stale headless chrome(s): ${stale.join(", ")}`);
  spawnSync("kill", ["-9", ...stale]);
}
