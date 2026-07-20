#!/usr/bin/env node
// Run any Captutor command inside the reversible full-desk filming profile.

import { spawn } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { enterStageMode, exitStageMode } from "../lib/stage-mode.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
if (!args.length) {
  console.error("usage: node bin/stage.mjs render <screenplay> [captutor options]");
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
  await enterStageMode();
  child = spawn(process.execPath, [resolve(HERE, "../captutor.mjs"), ...args], {
    stdio: "inherit",
    env: {
      ...process.env,
      CAPTUTOR_STAGE_MODE: "1",
      CAPTUTOR_REAL_CURSOR: "1",
      PATH: `/opt/homebrew/bin:${process.env.HOME}/.local/bin:/usr/bin:/bin:/usr/sbin:/sbin`,
    },
  });
  code = await new Promise((done) => child.once("exit", (status) => done(status ?? 1)));
} finally {
  await exitStageMode();
}
process.exit(interrupted ? 130 : code);
