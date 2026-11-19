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

export function startRax({ port = Number(process.env.RAX_PORT || 8786), entry = fileURLToPath(new URL('./rax-process.mjs', import.meta.url)), env = process.env } = {}) {
  let child = null, closed = false, starts = 0;
  const start = () => {
    if (closed) return;
    starts++;
    child = fork(entry, [], { env: { ...env, RAX_PORT: String(port) }, stdio: 'inherit' });
    child.once('exit', (code, signal) => {
      child = null;
      if (closed) return;
      console.warn(`[lith] rax's process ended (${signal || code}); starting it again`);
      setTimeout(start, Math.min(5000, 250 * starts)).unref();
    });
  };
  start();
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
    async close() {
      closed = true;
      for (const off of detach.splice(0)) off();
      const running = child;
      if (!running) return;
      await new Promise((resolve) => {
        running.once('exit', resolve);
        running.kill('SIGTERM');
        setTimeout(() => { try { running.kill('SIGKILL'); } catch {} resolve(); }, 9000).unref();
      });
    },
  };
}
