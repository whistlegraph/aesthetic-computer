// 🏠 lith's side of rax's own process (rax-process.mjs): start it, start it
// again if it falls, and pass on whatever reaches lith for it. In production
// Caddy sends /rax/* straight to the child, so none of rax's traffic waits on
// lith; this passing-on is for what dials lith itself (the builder beside
// it, a developer's machine, a Caddy that has not yet learned the way).
import { fork } from 'node:child_process';
import http from 'node:http';
import net from 'node:net';
import { fileURLToPath } from 'node:url';
import { attachSocketRoute } from './socket-routing.mjs';

import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { raxBuild } from './rax-build.mjs';
import { stateHome } from '../rax/env.mjs';

export function startRax({ port = Number(process.env.RAX_PORT || 8786), entry = fileURLToPath(new URL('./rax-process.mjs', import.meta.url)), env = process.env } = {}) {
  let child = null, closed = false, starts = 0, adopted = 0;
  const build = raxBuild(), card = join(env.RAX_STATE_DIR || stateHome(), `process-${port}.json`);
  const alive = (pid) => { try { process.kill(pid, 0); return true; } catch { return false; } };
  const spawn = () => {
    if (closed) return;
    starts++;
    adopted = 0;
    // On its own feet (detached): it outlives this process, and the next lith
    // to start takes it up again if it is still the same rax.
    child = fork(entry, [], { env: { ...env, RAX_PORT: String(port) }, stdio: 'inherit', detached: true });
    child.unref();
    child.once('exit', (code, signal) => {
      child = null;
      if (closed) return;
      console.warn(`[lith] rax's process ended (${signal || code}); starting it again`);
      setTimeout(start, Math.min(5000, 250 * starts)).unref();
    });
  };
  // A rax left running by the lith before this one: the same build is kept
  // (nobody in the room notices the site's deploy); another is asked to stop
  // (it writes down what is in the air first) and the new one started.
  const start = () => {
    if (closed) return;
    let was = null;
    try { was = JSON.parse(readFileSync(card, 'utf8')); } catch {}
    if (was && Number.isInteger(was.pid) && was.pid !== process.pid && alive(was.pid)) {
      if (was.build === build && was.port === port) {
        adopted = was.pid;
        console.log(`[lith] rax is already up and is this build (${build.slice(0, 10)}, pid ${was.pid}): left running`);
        return;
      }
      console.log(`[lith] rax is up as another build (pid ${was.pid}): stopping it for ${build.slice(0, 10)}`);
      try { process.kill(was.pid, 'SIGTERM'); } catch {}
      const began = Date.now();
      const wait = setInterval(() => {
        if (alive(was.pid) && Date.now() - began < 15000) return; // (it tells its rooms and gives them five seconds to read it, then writes down what is in the air: rax-process.mjs)
        clearInterval(wait);
        if (alive(was.pid)) { try { process.kill(was.pid, 'SIGKILL'); } catch {} }
        spawn();
      }, 100);
      return;
    }
    spawn();
  };
  start();
  // One taken up has no exit to hear: look in on it, and start a new one if it has gone.
  const watch = setInterval(() => { if (!closed && adopted && !alive(adopted)) { console.warn('[lith] the rax taken up has gone; starting one'); adopted = 0; spawn(); } }, 3000);
  watch.unref();
  // Up when its health answers.
  const ask = () => new Promise((resolve) => {
    const request = http.get({ host: '127.0.0.1', port, path: '/rax/health', timeout: 1000 }, (r) => { r.resume(); resolve(r.statusCode === 200); });
    request.on('error', () => resolve(false));
    request.on('timeout', () => { request.destroy(); resolve(false); });
  });
  const ready = (async () => {
    for (let i = 0; i < 240 && !closed; i++) {
      if (await ask()) return true;
      await new Promise((r) => setTimeout(r, 250));
    }
    return false;
  })();
  // An ordinary request, passed on as it came (its Host with it: rax checks a socket's origin against it).
  const handle = (req, res) => {
    const up = http.request({ host: '127.0.0.1', port, method: req.method, path: req.originalUrl || req.url, headers: req.headers }, (answer) => {
      res.writeHead(answer.statusCode, answer.headers);
      answer.pipe(res);
    });
    up.on('error', () => {
      if (!res.headersSent) res.writeHead(503, { 'Retry-After': '2', 'Cache-Control': 'no-store' });
      res.end();
    });
    req.pipe(up);
  };
  // A socket, passed on byte for byte.
  const upgrade = (req, socket, head) => {
    const up = net.connect(port, '127.0.0.1', () => {
      const lines = [`${req.method} ${req.url} HTTP/1.1`];
      for (let i = 0; i < req.rawHeaders.length; i += 2) lines.push(`${req.rawHeaders[i]}: ${req.rawHeaders[i + 1]}`);
      up.write(lines.join('\r\n') + '\r\n\r\n');
      if (head?.length) up.write(head);
      socket.pipe(up);
      up.pipe(socket);
    });
    const end = () => { up.destroy(); socket.destroy(); };
    up.on('error', end); socket.on('error', end); up.on('close', end); socket.on('close', end);
  };
  const detach = [];
  return {
    port,
    ready,
    handle,
    attach: (server) => detach.push(attachSocketRoute(server, ['/rax', '/dool'].flatMap((p) => [`${p}/play`, `${p}/builder`]), upgrade, { allowQuery: true })),
    // lith going down leaves rax up (stop: true takes it down too: a test, or a machine being emptied).
    async close({ stop = env.RAX_STOP_WITH_LITH === '1' } = {}) {
      closed = true;
      clearInterval(watch);
      for (const off of detach.splice(0)) off();
      if (!stop) return;
      const pid = child?.pid || adopted;
      if (!pid) return;
      try { process.kill(pid, 'SIGTERM'); } catch {}
      for (let i = 0; i < 90 && alive(pid); i++) await new Promise((r) => setTimeout(r, 100));
      if (alive(pid)) { try { process.kill(pid, 'SIGKILL'); } catch {} }
    },
  };
}
