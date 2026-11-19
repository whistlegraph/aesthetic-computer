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

const port = Number(process.env.RAX_PORT || 8786);
const app = createRaxServer({
  prefix: '/rax',
  alias: '/dool', // its name until 2026-11-18: old links, pages and the builder still land
  publishKidlisp: kidlispPublisher(),
  stateDir: stateHome(),
});
await app.ready; // The store is read from disk before the first request.
app.server.listen(port, '127.0.0.1', () => console.log(`[rax] its own process, on 127.0.0.1:${port}`));
let closing = false;
for (const signal of ['SIGTERM', 'SIGINT'])
  process.on(signal, () => {
    if (closing) return;
    closing = true;
    // What is in the air and where the ball lies are written down on the way out.
    app.close().catch(() => {}).finally(() => process.exit(0));
    setTimeout(() => process.exit(0), 8000).unref();
  });
