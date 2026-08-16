#!/usr/bin/env node
// preview-server.mjs — the reel-watch shell as a standing service.
//
// Everything the preview needs runs in the visitor's browser; the server only
// hands over the page, the game, and the fonts. So a box with no Chrome and no
// GPU — jasellite — can host it, and any browser anywhere on the tailnet can
// open a live bot match in reel dress:
//
//   http://<host>:7899/?social-preview&replay-oven&reel-hud&self-play
//
// Drop `self-play` to drive fighter one with a keyboard; drop `reel-hud` to
// see the reel truly bare. Demos the page POSTs land in memory and go nowhere
// (`replays: "stub"`), so robot sparring never dilutes the real store.
//
//   PORT=7899 node xbox/live/marketing/preview-server.mjs

import { appendFileSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { serveShell } from "./shell.mjs";

const port = Number(process.env.PORT) || 7899;

// Every finished round any browser plays against this host lands here, one
// demo per line, one file per day — bot fights as a dataset. Tally with
// marketing/tally-demos.mjs to see how the bots actually perform: who wins,
// what causes, where the stalemates live, what needs mixing up.
const demoDir = process.env.DEMO_DIR ||
  resolve(dirname(fileURLToPath(import.meta.url)), "../../../..", "oskiewar-demos");
mkdirSync(demoDir, { recursive: true });
let saved = 0;
const sink = (demo) => {
  const day = new Date().toISOString().slice(0, 10);
  appendFileSync(`${demoDir}/${day}.jsonl`,
    JSON.stringify({ at: Date.now(), ...demo }) + "\n");
  saved++;
};

const shell = await serveShell({
  replays: "stub", port, host: "0.0.0.0", log: console.log, onDemo: sink });
console.log(`oskiewar preview on :${port}`);
console.log(`  reel mode: /?social-preview&replay-oven&reel-hud&self-play`);
console.log(`  demos → ${demoDir}`);
process.on("SIGTERM", async () => {
  console.log(`${saved} demos saved this run`);
  await shell.close();
  process.exit(0);
});
