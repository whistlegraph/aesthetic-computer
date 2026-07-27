#!/usr/bin/env node
// Run any Captutor command inside the reversible full-desk filming profile.

import { spawn, spawnSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { enterStageMode, exitStageMode, parseStageFlags } from "../lib/stage-mode.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REEL = resolve(HERE, "../vendor/reel.mjs");
const REEL_STATE = join(process.env.HOME, ".local", "share", "slab", "state", "reel.state");
const rawArgs = process.argv.slice(2);
let parsed;
try { parsed = parseStageFlags(rawArgs); }
catch (error) {
  console.error(error.message);
  process.exit(2);
}
const { vertical, brand, args } = parsed;
if (!args.length) {
  console.error("usage: node bin/stage.mjs [--vertical] [--brand fuser|classic] render <screenplay> [captutor options]");
  process.exit(2);
}

let child;
let interrupted = false;
const forward = (signal) => {
  interrupted = true;
  try { child?.kill(signal); } catch {}
};
process.on("SIGINT", () => forward("SIGINT"));
process.on("SIGTERM", () => forward("SIGTERM"));

function verifyFilmingPermissions() {
  const recorder = spawnSync(process.execPath, [REEL, "status"], {
    encoding: "utf8",
    env: process.env,
  });
  if (recorder.status !== 0) {
    throw new Error(
      `SlabMenubar recording bridge is unavailable: ${(recorder.stderr || recorder.stdout || "").trim()}`,
    );
  }
  let status;
  try { status = JSON.parse(recorder.stdout || "{}"); }
  catch { throw new Error("SlabMenubar recording bridge returned invalid status"); }
  if (typeof status.recording !== "boolean") {
    throw new Error("SlabMenubar recording bridge did not report recording permission state");
  }

  const accessibility = spawnSync("/usr/bin/osascript", [
    "-e", 'tell application "System Events" to get UI elements enabled',
  ], { encoding:"utf8", env:process.env });
  if (accessibility.status !== 0 || accessibility.stdout.trim() !== "true") {
    throw new Error(
      "System Events Accessibility is not enabled for Captutor; allow it in System Settings before filming",
    );
  }
  console.log("✓ filming permissions — SlabMenubar recorder + System Events Accessibility");
}

function stopOwnedReelIfNeeded() {
  if (!child || !existsSync(REEL_STATE)) return;
  let state;
  try { state = JSON.parse(readFileSync(REEL_STATE, "utf8")); }
  catch { return; }
  if (!state.recording) return;
  const interruptedClip = join(tmpdir(), `captutor-stage-interrupted-${Date.now()}.mp4`);
  spawnSync(process.execPath, [REEL, "stop", "--out", interruptedClip], {
    stdio: "inherit",
    env: {
      ...process.env,
      PATH: `/opt/homebrew/bin:${process.env.HOME}/.local/bin:/usr/bin:/bin:/usr/sbin:/sbin`,
    },
  });
}

let code = 1;
try {
  verifyFilmingPermissions();
  // Enter is inside the guarded region deliberately: if a preference change
  // fails halfway through, the state file still lets `finally` unwind it.
  await enterStageMode({ vertical, brand });
  if (vertical) {
    // Rotation can leave Chrome's process alive with no page window. Relaunch
    // the dedicated filming profile only when its Fuser target disappeared.
    const port = process.env.CDP_PORT || "9333";
    const pages = await fetch(`http://127.0.0.1:${port}/json/list`)
      .then((response) => response.json()).catch(() => []);
    if (!pages.some((page) => page.type === "page" && (page.url || "").includes("fuser.studio"))) {
      const launched = spawnSync(resolve(HERE, "film-chrome.sh"), [
        "https://app.fuser.studio/w/me", port,
      ], { stdio: "inherit", env: process.env });
      if (launched.status !== 0) throw new Error("could not relaunch Chrome after portrait rotation");
    }
  }
  child = spawn(process.execPath, [resolve(HERE, "../captutor.mjs"), ...args], {
    stdio: "inherit",
    env: {
      ...process.env,
      CAPTUTOR_STAGE_MODE: "1",
      CAPTUTOR_VERTICAL_MODE: vertical ? "1" : "0",
      CAPTUTOR_BRAND: brand,
      // The capture-visible Swift overlay is the normal filmed pointer. Keep
      // the system cursor opt-in for explicitly human-driven takes only.
      CAPTUTOR_REAL_CURSOR: process.env.CAPTUTOR_REAL_CURSOR ?? "0",
      CDP_PORT: process.env.CDP_PORT || "9333",
      PATH: `/opt/homebrew/bin:${process.env.HOME}/.local/bin:/usr/bin:/bin:/usr/sbin:/sbin`,
    },
  });
  code = await new Promise((done) => child.once("exit", (status) => done(status ?? 1)));
} finally {
  stopOwnedReelIfNeeded();
  await exitStageMode();
}
process.exit(interrupted ? 130 : code);
