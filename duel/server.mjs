import http from 'node:http';
import { tailDevLog } from './dev-log.mjs';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';
import { createRequire } from 'node:module';
import { attachSocketRoute } from '../lith/socket-routing.mjs';
import { Room } from './room.mjs';
import { handleColors } from './handle-style.mjs';
import { ObjectStore } from './object-store.mjs';
import { Builders } from './builders.mjs';
import { HZ, PROTOCOL } from './world.mjs';
import { verifyAccount } from '../aesel/src/account-access.mjs';

const here = dirname(fileURLToPath(import.meta.url));
// Lith installs this dependency in production; standalone development can resolve root's copy.
const { WebSocketServer, WebSocket } = createRequire(
  new URL('../lith/package.json', import.meta.url),
)('ws');
const assets = new Map([
  ['/', ['index.html', 'text/html']],
  ...[
    'client',
    'view',
    'paint',
    'project',
    'presentation',
    'net',
    'world',
    'objects',
    'object-language',
    'prompt',
    'connection',
    'account',
    'controller',
    'freecam',
    'hud',
    'radial',
    'touch',
    'sfx',
    'highlight',
    'chunky-font',
  ].map((n) => [`/${n}.mjs`, [`${n}.mjs`, 'text/javascript']]),
  ['/qr.mjs', ['../system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs', 'text/javascript']],
  ['/object-compiler.mjs', ['../xbox/live/object-lisp.mjs', 'text/javascript']],
  [
    '/handle-font.mjs',
    [
      '../system/public/aesthetic.computer/lib/pixel-font-data.mjs',
      'text/javascript',
    ],
  ],
  [
    '/auth0.js',
    [
      '../system/public/aesthetic.computer/dep/auth0-spa-js.production.js',
      'text/javascript',
    ],
  ],
  ...['scene3d', 'scene3d-webgpu', 'scene3d-webgl', 'block-font'].map((n) => [
    `/engine/${n}.mjs`,
    [`../xbox/live/${n}.mjs`, 'text/javascript'],
  ]),
  [
    '/system/public/aesthetic.computer/lib/pmove.mjs',
    ['../system/public/aesthetic.computer/lib/pmove.mjs', 'text/javascript'],
  ],
  [
    '/pmove.mjs',
    ['../system/public/aesthetic.computer/lib/pmove.mjs', 'text/javascript'],
  ],
]);

export function createDuelServer({
  prefix = '',
  stateDir,
  builderToken = process.env.DOOL_BUILDER_TOKEN,
  verifyIdentity = verifyAccount,
  resolveHandleColors = handleColors,
  publicURL = process.env.DOOL_PUBLIC_URL,
  devLog = process.env.DOOL_DEV_LOG,
  publishKidlisp = null,
  devLogRooms = (process.env.DOOL_DEV_LOG_ROOMS || 'studio').split(','),
} = {}) {
  if (prefix && !/^\/[a-z0-9-]+$/.test(prefix))
    throw new Error('Invalid mount path');
  const rooms = new Map();
  const store = new ObjectStore(stateDir),
    ready = store.load();
  const handle = async (req, res) => {
    let url;
    try {
      url = new URL(req.url, 'http://localhost');
    } catch {
      res.writeHead(400).end();
      return;
    }
    if (prefix && url.pathname === prefix) {
      res
        .writeHead(307, {
          Location: `${prefix}/${url.search}`,
          'Cache-Control': 'no-store',
        })
        .end();
      return;
    }
    if (prefix && !url.pathname.startsWith(`${prefix}/`)) {
      res.writeHead(404).end();
      return;
    }
    const path = url.pathname.slice(prefix.length);
    if (path === '/health' && req.method === 'GET') {
      const revision = await readFile(
        resolve(here, '../system/public/.commit-ref'),
        'utf8',
      ).catch(() => 'local');
      res
        .writeHead(200, {
          'Content-Type': 'application/json',
          'Cache-Control': 'no-store',
        })
        .end(
          JSON.stringify({
            name: 'dool',
            building: builders.jobs.size,
            protocol: PROTOCOL,
            revision: revision.trim(),
          }),
        );
      return;
    }
    const asset = assets.get(path);
    if (req.method !== 'GET' || !asset) {
      res.writeHead(404).end();
      return;
    }
    try {
      const body = await readFile(resolve(here, asset[0]));
      res
        .writeHead(200, {
          'Content-Type': asset[1],
          'Cache-Control': 'no-store',
          'X-Content-Type-Options': 'nosniff',
        })
        .end(body);
    } catch {
      res.writeHead(500).end('Asset unavailable');
    }
  };
  const server = http.createServer(handle);
  const wss = new WebSocketServer({
    noServer: true,
    maxPayload: 8192,
    perMessageDeflate: false,
  });
  const builderWss = new WebSocketServer({
    noServer: true,
    maxPayload: 32768,
    perMessageDeflate: false,
  });
  const upgrade = (req, socket, head) => {
    let url, origin;
    try {
      url = new URL(req.url, 'http://localhost');
      origin = req.headers.origin && new URL(req.headers.origin);
    } catch {
      socket.destroy();
      return;
    }
    if (url.pathname === `${prefix}/builder`) {
      if (!builders.authorized(req.headers.authorization)) {
        socket.write('HTTP/1.1 401 Unauthorized\r\nConnection: close\r\n\r\n');
        socket.destroy();
        return;
      }
      builderWss.handleUpgrade(req, socket, head, (ws) => builders.attach(ws));
      return;
    }
    if (
      url.pathname !== `${prefix}/play` ||
      (origin && origin.host !== req.headers.host)
    ) {
      socket.destroy();
      return;
    }
    wss.handleUpgrade(req, socket, head, (ws) =>
      wss.emit('connection', ws, url, req.headers.authorization),
    );
  };
  const detach = [];
  const attach = (host) =>
    detach.push(
      attachSocketRoute(
        host,
        [`${prefix}/play`, `${prefix}/builder`],
        upgrade,
        {
          allowQuery: true,
        },
      ),
    );
  attach(server);
  const send = (ws, data) => {
    if (ws.readyState !== WebSocket.OPEN) return;
    if (ws.bufferedAmount > 256 * 1024) {
      ws.close(1013, 'Connection too slow');
      return;
    }
    ws.send(JSON.stringify(data));
  };
  const builders = new Builders({ token: builderToken, store, rooms, send, publish: publishKidlisp });
  // Off unless a transcript is named, and only in the listed rooms.
  const devRecent = [];
  const devRoom = (key) => !!devLog && devLogRooms.includes(key);
  const stopDevLog = devLog
    ? tailDevLog(devLog, (entry) => {
        const item = { ...entry, at: Date.now() };
        devRecent.push(item);
        if (devRecent.length > 4) devRecent.shift();
        for (const [key, room] of rooms)
          if (devRoom(key))
            for (const ws of room.clients.keys())
              send(ws, { type: 'dev-log', entries: [item] });
      })
    : () => {};
  wss.on('connection', async (ws, url, authorization) => {
    try {
      await ready;
    } catch {
      ws.close(1011, 'Object storage unavailable');
      return;
    }
    if (ws.readyState !== WebSocket.OPEN) return;
    const roomId = url.searchParams.get('room') || 'lobby';
    if (
      !/^[a-z0-9-]{1,32}$/.test(roomId) ||
      url.searchParams.get('protocol') !== PROTOCOL
    ) {
      ws.close(1008, 'Invalid room or build');
      return;
    }
    const key = roomId;
    if (!rooms.has(key)) {
      if (rooms.size >= 32) {
        ws.close(1013, 'Server full');
        return;
      }
      rooms.set(key, new Room(send));
    }
    const room = rooms.get(key);
    if (room.clients.size >= 10) {
      ws.close(1013, 'Room full');
      return;
    }
    const nativeAccount = authorization?.startsWith('Bearer ');
    const member = room.add(ws, nativeAccount);
    room.match.setObjects(store.objects(key));
    let accountAttempted = false;
    const identifyAccount = async (token) => {
      if (
        accountAttempted ||
        typeof token !== 'string' ||
        !token ||
        token.length > 7500
      )
        return;
      accountAttempted = true;
      try {
        const account = await verifyIdentity(token);
        const handle = String(account.handle || '').replace(/^@/, '');
        if (!/^[a-z0-9]+([._][a-z0-9]+)*$/i.test(handle) || handle.length > 16)
          return;
        if (ws.readyState !== WebSocket.OPEN || !room.sessions.has(ws)) return;
        member.handle = handle;
        member.ready = true;
        room.assign();
        const colors = await resolveHandleColors(handle);
        if (ws.readyState === WebSocket.OPEN && room.sessions.has(ws)) {
          member.colors = colors;
          room.broadcast();
        }
      } catch {
        /* Unverified sessions remain guests. */
      } finally {
        if (room.sessions.has(ws) && !member.ready) {
          member.ready = true;
          room.assign();
        }
      }
    };
    send(ws, {
      type: 'welcome',
      seat: member.seat,
      role: member.seat < 0 ? 'spectator' : 'player',
      protocol: PROTOCOL,
      room: roomId,
      joinURL: publicURL ? (() => {
        const join = new URL(publicURL);
        join.search = new URLSearchParams({ room: roomId });
        join.hash = '';
        return join.href;
      })() : null,
    });
    member.welcomed = true;
    if (devRoom(key) && devRecent.length)
      send(ws, { type: 'dev-log', entries: devRecent });
    send(ws, room.match.snapshot());
    send(ws, builders.state(key));
    room.broadcast();
    if (nativeAccount) void identifyAccount(authorization.slice(7));
    let owner = null,
      identifying = false;
    let windowAt = performance.now(),
      messages = 0,
      alive = true;
    ws.on('pong', () => {
      alive = true;
    });
    const heartbeat = setInterval(() => {
      if (!alive) return ws.terminate();
      alive = false;
      ws.ping();
    }, 10000);
    ws.on('error', () => ws.terminate());
    ws.on('message', (raw) => {
      const now = performance.now();
      if (now - windowAt >= 1000) {
        windowAt = now;
        messages = 0;
      }
      if (++messages > 180) {
        ws.close(1008, 'Input rate exceeded');
        return;
      }
      let data;
      try {
        data = JSON.parse(raw.toString());
      } catch {
        ws.close(1008, 'Invalid JSON');
        return;
      }
      if (data?.type === 'identity' && !owner && !identifying) {
        identifying = true;
        store
          .identity(data.token)
          .then((identity) => {
            owner = identity.owner;
            if (ws.readyState === WebSocket.OPEN && room.sessions.has(ws)) {
              member.owner = owner;
              room.assign();
            }
            send(ws, { type: 'identity', ...identity });
          })
          .catch(() =>
            send(ws, { type: 'build-result', error: 'Identity unavailable' }),
          )
          .finally(() => {
            identifying = false;
          });
      }
      if (data?.type === 'cancel-build' && owner && member.seat >= 0)
        builders.cancel(owner, key);
      if (data?.type === 'account') void identifyAccount(data.accessToken);
      if (data?.type === 'prompt') {
        if (member.seat >= 0)
          builders.request(ws, key, owner, member.seat, data);
        else
          send(ws, { type: 'build-result', error: 'Spectators cannot build' });
      }
      if (data?.type === 'input' && member.seat >= 0)
        room.match.input(member.seat, data.commands);
      if (data?.type === 'ping' && Number.isFinite(data.at))
        send(ws, { type: 'pong', at: data.at });
    });
    ws.on('close', () => {
      clearInterval(heartbeat);
      room.remove(ws);
      if (!room.clients.size) rooms.delete(key);
    });
  });

  let previous = performance.now(),
    debt = 0;
  const timer = setInterval(() => {
    const now = performance.now();
    debt += Math.min(100, now - previous);
    previous = now;
    while (debt >= 1000 / HZ) {
      debt -= 1000 / HZ;
      for (const room of rooms.values()) {
        room.match.step();
        if (room.match.tick % 2) continue;
        const snap = room.match.snapshot();
        for (const ws of room.clients.keys()) {
          send(ws, snap);
        }
      }
    }
  }, 4);

  return {
    server,
    rooms,
    handle,
    attach,
    store,
    builders,
    ready,
    async close() {
      builders.close();
      stopDevLog();
      clearInterval(timer);
      for (const remove of detach) remove();
      for (const ws of wss.clients) ws.terminate();
      await new Promise((resolve) => wss.close(resolve));
      await new Promise((resolve) => builderWss.close(resolve));
      await store.queue;
      if (server.listening)
        await new Promise((resolve) => server.close(resolve));
    },
  };
}

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const app = createDuelServer({
    stateDir:
      process.env.DOOL_STATE_DIR ||
      resolve(process.env.HOME || '.', '.local/share/dool'),
  });
  await app.ready;
  const port = Number(process.env.PORT || 8787),
    host = process.env.HOST || '127.0.0.1';
  app.server.listen(port, host, () =>
    console.log(`dool → http://${host}:${port}`),
  );
  for (const signal of ['SIGINT', 'SIGTERM'])
    process.once(signal, () => app.close().then(() => process.exit(0)));
}
