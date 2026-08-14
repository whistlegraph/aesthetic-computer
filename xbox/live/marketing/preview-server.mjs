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

import { serveShell } from "./shell.mjs";

const port = Number(process.env.PORT) || 7899;
const shell = await serveShell({
  replays: "stub", port, host: "0.0.0.0", log: console.log });
console.log(`oskiewar preview on :${port}`);
console.log(`  reel mode: /?social-preview&replay-oven&reel-hud&self-play`);
process.on("SIGTERM", async () => { await shell.close(); process.exit(0); });
