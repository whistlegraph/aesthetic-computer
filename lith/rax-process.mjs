// 🏠 rax in a process of its own.
// rax keeps a clock: its world steps sixty times a second and every room is
// told each time. In lith's process that clock waited behind whatever else
// the site was doing (an image resized, a page rendered) and everyone in rax
// stood still for it (2026-11-19: the server's own record showed its clock
// 100 to 300 ms late several times a minute with one person in). Here it
// has a thread to itself. lith starts this (rax-child.mjs), Caddy sends
// /rax/* and /dool/* straight to it, and lith passes on anything that
// reaches it instead.
import { createRaxServer } from '../rax/server.mjs';
import { kidlispPublisher } from '../rax/kidlisp-code.mjs';
import { stateHome } from '../rax/env.mjs';
import { ROOM_HOSTS } from '../rax/world.mjs';

import { writeFileSync, mkdirSync, rmSync } from 'node:fs';
import { join } from 'node:path';
import { raxBuild } from './rax-build.mjs';

const port = Number(process.env.RAX_PORT || 8786);
// 🪪 Which rax this is (its own files' hash) and where it runs, written down:
// a lith that starts later and finds the same rax still up leaves it be, so
// a deploy that changed nothing of rax does not put anyone out of the room.
const build = raxBuild(), card = join(stateHome(), `process-${port}.json`);
const app = createRaxServer({
  prefix: '/rax',
  alias: '/dool', // its name until 2026-11-18: old links, pages and the builder still land
  publishKidlisp: kidlispPublisher(),
  stateDir: stateHome(),
  roomHosts: ROOM_HOSTS, // 🌐 rooms kept on other boxes (rax/satellite.mjs): walkers are handed over
});
await app.ready; // The store is read from disk before the first request.
app.server.listen(port, '127.0.0.1', () => {
  console.log(`[rax] its own process, on 127.0.0.1:${port} (${build.slice(0, 10)})`);
  try { mkdirSync(stateHome(), { recursive: true }); writeFileSync(card, JSON.stringify({ pid: process.pid, port, build })); } catch {}
});
let closing = false;
for (const signal of ['SIGTERM', 'SIGINT'])
  process.on(signal, () => {
    if (closing) return;
    closing = true;
    // What is in the air and where the ball lies are written down on the way out.
    try { rmSync(card, { force: true }); } catch {}
    app.close().catch(() => {}).finally(() => process.exit(0));
    setTimeout(() => process.exit(0), 8000).unref();
  });
