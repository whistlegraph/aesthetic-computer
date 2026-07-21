#!/usr/bin/env node
// Run any Captutor command inside the reversible full-desk filming profile.

import { spawn, spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { enterStageMode, exitStageMode } from "../lib/stage-mode.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const rawArgs = process.argv.slice(2);
const vertical = rawArgs.includes("--vertical");
const args = rawArgs.filter((arg) => arg !== "--vertical");
if (!args.length) {
  console.error("usage: node bin/stage.mjs [--vertical] render <screenplay> [captutor options]");
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

let code = 1;
try {
  // Enter is inside the guarded region deliberately: if a preference change
  // fails halfway through, the state file still lets `finally` unwind it.
  await enterStageMode({ vertical });
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
      CAPTUTOR_REAL_CURSOR: "1",
      CDP_PORT: process.env.CDP_PORT || "9333",
      PATH: `/opt/homebrew/bin:${process.env.HOME}/.local/bin:/usr/bin:/bin:/usr/sbin:/sbin`,
    },
  });
  code = await new Promise((done) => child.once("exit", (status) => done(status ?? 1)));
} finally {
  await exitStageMode();
}
process.exit(interrupted ? 130 : code);
