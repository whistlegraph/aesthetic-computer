#!/usr/bin/env node
// affirm-loop.mjs — cycle a star through a set of affirmations, forever.
//
// The star polls every ~45s, so give each message at least a poll's worth
// of air time (default: 120s per message).
//
//   node macpal/affirm-loop.mjs "it's good to be calm!" "i love you!"
//   node macpal/affirm-loop.mjs --every 300 "msg a" "msg b" "msg c"
//   node macpal/affirm-loop.mjs --to fia --every 120 "..." "..."
//
// Stop it:  pkill -f affirm-loop
//
// Each push shells out to affirm.mjs so auth/host logic lives in one place.

import { execFileSync } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);

function flag(name, fallback) {
  const i = args.indexOf(name);
  if (i === -1) return fallback;
  const v = args[i + 1];
  args.splice(i, 2);
  return v;
}

const to = flag("--to", "fia");
const every = Number(flag("--every", "120")) * 1000;
const messages = args;

if (messages.length < 2) {
  console.error('usage: node macpal/affirm-loop.mjs [--to fia] [--every 120] "msg a" "msg b" [...]');
  process.exit(1);
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

for (let i = 0; ; i++) {
  const text = messages[i % messages.length];
  try {
    execFileSync("node", [path.join(here, "affirm.mjs"), text, "--to", to], { stdio: "inherit" });
  } catch {
    console.error(`✗ push failed (token expired? re-run: node tezos/ac-login.mjs) — retrying next cycle`);
  }
  await sleep(every);
}
