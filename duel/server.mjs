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
import { Builders, GUEST_BUILDS } from './builders.mjs';
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
    'kidlisp-wall',
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
    // AC's own libraries, read-only, so a wall can run real KidLisp.
    if (req.method === 'GET' && /^\/ac\/(lib|dep)\/[\w@./-]+\.(mjs|js)$/.test(path) && !path.includes('..')) {
      const root = resolve(here, '../system/public/aesthetic.computer');
      const file = resolve(root, path.slice(4));
      if (file.startsWith(root + '/')) {
        try {
          const body = await readFile(file);
          res.writeHead(200, { 'Content-Type': 'text/javascript', 'Cache-Control': 'max-age=60', 'X-Content-Type-Options': 'nosniff' }).end(body);
        } catch {
          res.writeHead(404).end();
        }
        return;
      }
    }
    // A $code's source from AC's KidLisp store, fetched here to avoid CORS.
    if (req.method === 'GET' && path === '/kidlisp-source') {
      const code = url.searchParams.get('code') || '';
      if (!/^[A-Za-z0-9]{2,16}$/.test(code)) return void res.writeHead(400).end();
      try {
        const r = await fetch(`https://aesthetic.computer/api/store-kidlisp?code=${code}`, { signal: AbortSignal.timeout(6000) });
        const { source } = await r.json();
        if (typeof source !== 'string') throw Error('no source');
        res.writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'max-age=300' }).end(JSON.stringify({ code, source }));
      } catch {
        res.writeHead(404).end();
      }
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
  // Braincells: AC's allowance, read with the player's own sign-in and sent
  // to them alone. Refreshed every minute and after each build.
  async function refreshBraincells(ws, member) {
    if (ws.readyState !== WebSocket.OPEN) return;
    if (!member?.token) {
      // A guest: the day's allowance, once their identity is known.
      if (member?.owner) send(ws, { type: 'braincells', remaining: builders.guestRemaining(member.owner), limit: GUEST_BUILDS });
      return;
    }
    try {
      const r = await fetch('https://aesthetic.computer/api/easel-credits', {
        headers: { authorization: `Bearer ${member.token}` },
        signal: AbortSignal.timeout(6000),
      });
      if (!r.ok) return;
      const b = await r.json();
      if (Number.isFinite(b.remaining))
        send(ws, { type: 'braincells', remaining: b.remaining + (Number.isFinite(b.purchased) ? b.purchased : 0), limit: b.limit });
    } catch {}
  }
  // Every 4 s for anyone with a build going (so spending shows as it
  // happens), otherwise every minute.
  let braincellTick = 0;
  const braincellTimer = setInterval(() => {
    braincellTick++;
    for (const [key, room] of rooms)
      for (const [ws] of room.clients) {
        const member = room.sessions.get(ws);
        const busy = member?.owner && [...builders.jobs.values()].some((j) => j.room === key && j.owner === member.owner);
        if (busy || braincellTick % 15 === 0) void refreshBraincells(ws, member);
      }
  }, 4000);
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
        // Kept in memory only, to read this player's braincells.
        member.token = token;
        void refreshBraincells(ws, member);
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
    send(ws, { type: 'chalk', strokes: room.match.chalk });
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
              void refreshBraincells(ws, member);
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
        if (member.seat >= 0) {
          builders.request(ws, key, owner, member.seat, data, { guest: !member.token });
          void refreshBraincells(ws, member);
          for (const ms of [20000, 90000, 180000]) setTimeout(() => void refreshBraincells(ws, member), ms);
        }
        else send(ws, { type: 'build-result', error: 'Spectators cannot build' });
      }
      if (data?.type === 'chalk-clear' && member.seat >= 0) room.match.clearChalk(member.seat);
      if (data?.type === 'clear-room' && member.seat >= 0) void builders.clearRoom(key);
      // LB: take an object out of the room (it stays in the $ history).
      if (data?.type === 'remove' && member.seat >= 0 && typeof data.objectId === 'string')
        void store.removeObject(key, data.objectId).then((gone) => {
          if (!gone) return;
          room.match.emit('removed', { at: { x: gone.x, y: 1, z: gone.z } });
          builders.broadcast(key);
        });
      // Everything this handle has made that can stand in the space.
      if (data?.type === 'handle-history' && member.handle)
        void (async () => {
          try {
            const r = await fetch(`https://aesthetic.computer/api/store-kidlisp?recent=true&limit=300&handle=${encodeURIComponent(member.handle)}`, { signal: AbortSignal.timeout(8000) });
            const { recent = [] } = await r.json();
            const items = recent
              .filter((x) => typeof x.source === 'string' && /\((?:slab|orb|capsule)\s/.test(x.source.replace(/;[^\n]*/g, '')))
              .map((x) => ({ code: x.code, name: (/^\s*;\s*([^\n]+)/.exec(x.source)?.[1] || x.code).slice(0, 40), at: x.when }));
            send(ws, { type: 'handle-history', handle: member.handle, items });
          } catch {}
        })();
      // Sharper or chunkier: change a screen's pixel grid; no Aesel needed.
      if (data?.type === 'wall-res' && member.seat >= 0 && ['north', 'west'].includes(data.face) && [-1, 1].includes(data.dir))
        void store.stepWallRes(key, data.face, data.dir).then(() => builders.broadcast(key));
      else if (data?.type === 'kidlisp-mod' && member.seat >= 0) void builders.requestKidlisp(ws, key, owner, data);
      if (data?.type === 'paint' && member.seat >= 0 && Number.isInteger(data.i) && Number.isInteger(data.j) && data.i >= 0 && data.i < 18 && data.j >= 0 && data.j < 18)
        void store.paintTile(key, data.i, data.j).then(() => builders.broadcast(key));
      if (data?.type === 'restore' && member.seat >= 0 && typeof data.code === 'string' && /^[A-Za-z0-9]{2,16}$/.test(data.code))
        void builders.restore(ws, key, owner, member.seat, data.code);
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
        if (room.match.chalkDirty && room.match.tick % 3 === 0)
          for (const msg of room.match.takeChalk())
            for (const ws of room.clients.keys()) send(ws, msg);
        // Every tick (60 Hz): smaller gaps to interpolate across.
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
      clearInterval(braincellTimer);
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
