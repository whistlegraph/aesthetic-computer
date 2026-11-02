import http from 'node:http';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';
import { WebSocketServer, WebSocket } from 'ws';
import { Match } from './match.mjs';
import { HZ, PROTOCOL, MODES } from './world.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const assets = new Map([
  ['/', ['index.html', 'text/html']],
  ...['client', 'view', 'net', 'world'].map(n => [`/${n}.mjs`, [`${n}.mjs`, 'text/javascript']]),
  ['/system/public/aesthetic.computer/lib/pmove.mjs',
    ['../system/public/aesthetic.computer/lib/pmove.mjs', 'text/javascript']],
  ...['three.module.js', 'three.core.js'].map(n => [`/vendor/${n}`,
    [`../system/public/aesthetic.computer/dep/three/${n}`, 'text/javascript']]),
]);

export function createDuelServer() {
  const rooms = new Map();
  const server = http.createServer(async (req, res) => {
    const url = new URL(req.url, 'http://localhost');
    const asset = assets.get(url.pathname);
    if (req.method !== 'GET' || !asset) { res.writeHead(404).end(); return; }
    try {
      const body = await readFile(resolve(here, asset[0]));
      res.writeHead(200, { 'Content-Type': asset[1], 'Cache-Control': 'no-store',
        'X-Content-Type-Options': 'nosniff' }).end(body);
    } catch { res.writeHead(500).end('Asset unavailable'); }
  });
  const wss = new WebSocketServer({ noServer: true, maxPayload: 8192, perMessageDeflate: false });
  server.on('upgrade', (req, socket, head) => {
    let url, origin;
    try {
      url = new URL(req.url, 'http://localhost');
      origin = req.headers.origin && new URL(req.headers.origin);
    } catch { socket.destroy(); return; }
    if (url.pathname !== '/play' || (origin && origin.host !== req.headers.host)) {
      socket.destroy(); return;
    }
    wss.handleUpgrade(req, socket, head, ws => wss.emit('connection', ws, url));
  });
  const send = (ws, data) => {
    if (ws.readyState !== WebSocket.OPEN) return;
    if (ws.bufferedAmount > 256 * 1024) { ws.close(1013, 'Connection too slow'); return; }
    ws.send(JSON.stringify(data));
  };
  wss.on('connection', (ws, url) => {
    const roomId = url.searchParams.get('room') || 'lobby';
    const mode = url.searchParams.get('mode') || 'instagib';
    if (!/^[a-z0-9-]{1,32}$/.test(roomId) || !Object.hasOwn(MODES, mode) ||
        url.searchParams.get('protocol') !== PROTOCOL) {
      ws.close(1008, 'Invalid room, mode or build'); return;
    }
    const key = `${mode}:${roomId}`;
    if (!rooms.has(key)) {
      if (rooms.size >= 32) { ws.close(1013, 'Server full'); return; }
      rooms.set(key, { match: new Match(mode), clients: new Map() });
    }
    const room = rooms.get(key), seat = room.match.join();
    if (seat < 0) { ws.close(1008, 'Room full: two players only'); return; }
    room.clients.set(ws, seat);
    send(ws, { type: 'welcome', seat, protocol: PROTOCOL, room: roomId });
    send(ws, room.match.snapshot());
    let windowAt = performance.now(), messages = 0, alive = true;
    ws.on('pong', () => { alive = true; });
    const heartbeat = setInterval(() => {
      if (!alive) return ws.terminate();
      alive = false; ws.ping();
    }, 10000);
    ws.on('error', () => ws.terminate());
    ws.on('message', raw => {
      const now = performance.now();
      if (now - windowAt >= 1000) { windowAt = now; messages = 0; }
      if (++messages > 180) { ws.close(1008, 'Input rate exceeded'); return; }
      let data;
      try { data = JSON.parse(raw.toString()); } catch { ws.close(1008, 'Invalid JSON'); return; }
      if (data?.type === 'input') room.match.input(seat, data.commands);
      if (data?.type === 'ping' && Number.isFinite(data.at)) send(ws, { type: 'pong', at: data.at });
    });
    ws.on('close', () => {
      clearInterval(heartbeat);
      room.clients.delete(ws);
      room.match.leave(seat);
      if (!room.clients.size) rooms.delete(key);
    });
  });

  let previous = performance.now(), debt = 0;
  const timer = setInterval(() => {
    const now = performance.now();
    debt += Math.min(100, now - previous); previous = now;
    while (debt >= 1000 / HZ) {
      debt -= 1000 / HZ;
      for (const room of rooms.values()) {
        room.match.step();
        if (room.match.tick % 2) continue;
        const snap = room.match.snapshot();
        for (const [ws, seat] of room.clients) {
          room.match.seats[seat].sentTick = snap.tick;
          send(ws, snap);
        }
      }
    }
  }, 4);

  return { server, rooms, async close() {
    clearInterval(timer);
    for (const ws of wss.clients) ws.terminate();
    await new Promise(resolve => wss.close(resolve));
    if (server.listening) await new Promise(resolve => server.close(resolve));
  } };
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const app = createDuelServer();
  const port = Number(process.env.PORT || 8787), host = process.env.HOST || '127.0.0.1';
  app.server.listen(port, host, () => console.log(`duel → http://${host}:${port}`));
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => app.close().then(() => process.exit(0)));
}
