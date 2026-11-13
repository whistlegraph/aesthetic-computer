import http from 'node:http';
import { tailDevLog } from './dev-log.mjs';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';
import { createRequire } from 'node:module';
import { attachSocketRoute } from '../lith/socket-routing.mjs';
import { Room } from './room.mjs';
import { GeographyStore, BLOCK, LOTS, lot } from './geography.mjs';
import { handleColors } from './handle-style.mjs';
import { ObjectStore } from './object-store.mjs';
import { Builders, GUEST_BUILDS } from './builders.mjs';
import { createDiagnostics } from './diagnostics.mjs';
import { stallRow } from './stalls.mjs';
import { MAX_PLAYERS, HZ, PROTOCOL, WORLD, clamp } from './world.mjs';
import { randomUUID } from 'node:crypto';
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
    'diagnostics-client',
    'stalls',
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
  diagnosticsEnabled = process.env.DOOL_DIAGNOSTICS === '1',
  diagnosticWrite,
  devLogRooms = (process.env.DOOL_DEV_LOG_ROOMS || 'studio').split(','),
} = {}) {
  if (prefix && !/^\/[a-z0-9-]+$/.test(prefix))
    throw new Error('Invalid mount path');
  const rooms = new Map();
  const diagnostics = createDiagnostics({ enabled: diagnosticsEnabled, write: diagnosticWrite });
  // AC's profanity filter, loaded on first use (a static import of a module
  // with its own dependencies has taken lith down before); without it the
  // line passes as typed.
  let chatFilterLoad = null;
  const chatFilter = () => (chatFilterLoad ||= import('../shared/filter.mjs')
    .then((m) => (text) => { try { return String(m.filter(text)); } catch { return text; } })
    .catch(() => (text) => text));
  const stallLog = []; // 🧊 The newest stalls clients reported (GET /stalls).
  let stallRevision = 'local';
  const store = new ObjectStore(stateDir),
    geography = new GeographyStore(stateDir), // 🗺️ who lives where (GEOGRAPHY.md)
    ready = Promise.all([store.load(), geography.load(), readFile(resolve(here, '../system/public/.commit-ref'), 'utf8')
      .then((r) => { diagnostics.setRevision(r.trim()); stallRevision = r.trim().slice(0, 10); }).catch(() => {})]);
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
            persistent: !!stateDir, // false means the room dies with the process
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
    // The room as it stands, for a guest that only looks (the world behind
    // the prompt's curtain shows what is built without taking a seat).
    // 🧊 What clients said about stopped frames (stalls.mjs): numbers only.
    if (path === '/stalls' && req.method === 'GET') {
      res
        .writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' })
        .end(JSON.stringify({ building: builders.jobs.size, stalls: stallLog }));
      return;
    }
    // 🗺️ The map: every open block, who settled it, and the world's size.
    if (path === '/geography' && req.method === 'GET') {
      // Who is seated now, and where everyone else's avatar rests.
      const awake = [];
      for (const [room, r] of rooms)
        for (const m of r.sessions.values()) if (m.handle && m.seat >= 0) awake.push({ handle: m.handle.toLowerCase(), room });
      const up = new Set(awake.map((a) => a.handle));
      const resting = Object.entries(geography.geography.rests)
        .filter(([handle]) => !up.has(handle))
        .map(([handle, r]) => ({ handle, ...r }));
      res
        .writeHead(200, {
          'Content-Type': 'application/json',
          'Cache-Control': 'no-store',
          'Access-Control-Allow-Origin': '*', // the map piece reads it from any AC host
        })
        .end(
          JSON.stringify({
            ...geography.geography.census(),
            block: BLOCK,
            lotRects: Array.from({ length: LOTS }, (_, k) => lot(k)),
            awake,
            resting,
          }),
        );
      return;
    }
    if (path === '/room' && req.method === 'GET') {
      const id = url.searchParams.get('room') || 'lobby';
      if (!/^[a-z0-9-]{1,32}$/.test(id)) {
        res.writeHead(400).end();
        return;
      }
      res
        .writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' })
        .end(
          JSON.stringify({
            objects: store.objects(id),
            tiles: store.tiles(id),
            screens: store.screens(id),
            chalk: rooms.get(id)?.match.chalk || [],
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
    if (diagnostics.enabled && ws.diagnosticJobs &&
      ['objects', 'build-accepted', 'build-result'].includes(data.type)) {
      const now = Date.now();
      for (const [id, at] of ws.diagnosticJobs) if (now - at > 300000) ws.diagnosticJobs.delete(id);
      const ids = data.type === 'objects' ? data.jobs.map((j) => j.id)
        : ['build-accepted', 'build-result'].includes(data.type) && data.id ? [data.id] : [];
      for (const id of ids) ws.diagnosticJobs.set(id, now);
      while (ws.diagnosticJobs.size > 32) ws.diagnosticJobs.delete(ws.diagnosticJobs.keys().next().value);
    }
    ws.send(JSON.stringify(data));
  };
  // One frame for a whole room: serialize once, then hand the same text to
  // every socket (the relay's lesson — per-socket stringify scales with the room).
  // Players plus as many watchers again.
const ROOM_CONNECTIONS = MAX_PLAYERS * 2;
  const sendText = (ws, text) => {
    if (ws.readyState !== WebSocket.OPEN) return;
    if (ws.bufferedAmount > 256 * 1024) {
      ws.close(1013, 'Connection too slow');
      return;
    }
    ws.send(text);
  };
  // A handle's whole $ history: every KidLisp object it has published, from
  // any session, newest first. Read from the store for at most 30 s at a time.
  // The newest THUMBED rows carry a tiny picture (base64 RGB, TINY square).
  const THUMBED = 64;
  const handleHistories = new Map();
  async function handleHistory(handle) {
    const cached = handleHistories.get(handle);
    if (cached && Date.now() - cached.at < 30000) return cached.items;
    const r = await fetch(`https://aesthetic.computer/api/store-kidlisp?recent=true&limit=100000&handle=${encodeURIComponent(handle)}`, { signal: AbortSignal.timeout(10000) });
    const { recent = [] } = await r.json();
    // Loaded on first use, so the renderer can never stop lith from starting.
    const { tinyThumb, isObjectLisp } = await import('./tiny-thumbs.mjs');
    const items = recent
      .filter((x) => typeof x.code === 'string' && isObjectLisp(x.source))
      .map((x, i) => ({
        code: x.code,
        name: (/^\s*;\s*([^#\n][^\n]*)/.exec(x.source)?.[1] || x.code).trim().slice(0, 40),
        at: x.when,
        thumb: i < THUMBED ? tinyThumb(x.code, x.source) : null,
      }));
    handleHistories.set(handle, { at: Date.now(), items });
    if (handleHistories.size > 256) handleHistories.delete(handleHistories.keys().next().value);
    return items;
  }
  async function sendHandleHistory(ws, handle) {
    try {
      send(ws, { type: 'handle-history', handle, items: await handleHistory(handle) });
    } catch {
      send(ws, { type: 'handle-history', handle, items: [], error: true });
    }
  }
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
  const builders = new Builders({ token: builderToken, store, rooms, send, publish: publishKidlisp, diagnostics });
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
    if (room.clients.size >= ROOM_CONNECTIONS) {
      ws.close(1013, 'Room full');
      return;
    }
    const nativeAccount = authorization?.startsWith('Bearer ');
    ws.diagnosticSession = randomUUID();
    ws.diagnosticJobs = new Map();
    let diagnosticAt = -Infinity, stallAt = -Infinity, saidAt = -Infinity;
    const stallSession = randomUUID().slice(0, 6); // One visit, not a person.
    const creditRefreshes = new Set();
    // A page that says it is signed in waits for its handle before it is
    // seated or named, for a few seconds at most.
    const webAccount = !nativeAccount && url.searchParams.get('account') === '1';
    const member = room.add(ws, nativeAccount || webAccount);
    const seatGuest = () => {
      clearTimeout(accountHold);
      if (room.sessions.has(ws) && !member.ready) {
        member.ready = true;
        room.assign();
      }
    };
    const accountHold = webAccount ? setTimeout(seatGuest, 6000) : null;
    accountHold?.unref?.();
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
        // 🗺️ A handle has one home, settled the first time it arrives, and
        // its avatar wakes where it rested if that was this room. A reload
        // that takes over a body already here leaves the body where it is.
        const person = handle.toLowerCase();
        const takeover = [...room.sessions.values()].some(
          (o) => o !== member && o.seat >= 0 && o.handle?.toLowerCase() === person,
        );
        void geography.change((g) => g.settle(handle)).catch(() => {});
        member.handle = handle;
        member.ready = true;
        // The $ history is the handle's, ready before the menu opens.
        member.wantsHistory = false;
        void sendHandleHistory(ws, handle);
        // Kept in memory only, to read this player's braincells.
        member.token = token;
        void refreshBraincells(ws, member);
        room.assign();
        const rest = geography.geography.whereabouts(handle),
          body = room.match.seats[member.seat]?.player;
        if (!takeover && body && rest?.room === key) {
          const edge = BLOCK / 2 - 0.4; // inside the walls
          body.x = Math.max(-edge, Math.min(edge, rest.x));
          body.z = Math.max(-edge, Math.min(edge, rest.z));
          body.yaw = rest.yaw;
        }
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
      diagnostics: diagnostics.enabled,
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
    send(ws, builders.state(key, member));
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
      if (data?.type === 'stall') {
        if (now - stallAt < 200) return;
        stallAt = now;
        const row = stallRow(data, { session: stallSession, revision: stallRevision });
        if (row) {
          stallLog.push(row);
          if (stallLog.length > 500) stallLog.splice(0, stallLog.length - 500);
        }
        return;
      }
      if (data?.type === 'diagnostic') {
        if (!diagnostics.enabled || now - diagnosticAt < 5000) return;
        diagnosticAt = now;
        const fields = {};
        for (const k of ['durationMs', 'frames', 'frameGapMaxMs', 'frameWorkMaxMs', 'renderMaxMs',
          'snapshotGapMaxMs', 'longTasks', 'longTaskMaxMs', 'frameErrors', 'messageMaxMs',
          'reconcileMaxMs', 'pendingCommandsMax', 'snapshotsCoalesced',
          'rttMs', 'jitterMs', 'queuedBytes', 'objects', 'triangles', 'backend']) fields[k] = data[k];
        diagnostics.emit('client.sample', { ...fields, session: ws.diagnosticSession,
          buildIds: Array.isArray(data.buildIds) ? data.buildIds.slice(0, 12).filter((id) =>
            ws.diagnosticJobs.has(id) && Date.now() - ws.diagnosticJobs.get(id) <= 300000) : [] });
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
      if (data?.type === 'account') void identifyAccount(data.accessToken).finally(seatGuest);
      if (data?.type === 'account-none') seatGuest();
      // 💬 Room chat: a seated player's line goes to everyone in the room.
      // Printable ASCII (the room's font), short, filtered like AC's chat,
      // slowed to a line or so a second, and never stored.
      if (data?.type === 'say' && member.seat >= 0 && typeof data.text === 'string') {
        if (now - saidAt < 700) return;
        saidAt = now;
        const text = data.text.replace(/[^\x20-\x7e]/g, ' ').replace(/\s+/g, ' ').trim().slice(0, 140);
        if (!text) return;
        void chatFilter().then((clean) => {
          if (!room.sessions.has(ws)) return;
          const who = room.public(member);
          const message = { type: 'said', seat: who.seat, handle: who.handle, name: who.name, colors: who.colors, text: clean(text) };
          for (const client of room.clients.keys()) send(client, message);
        });
        return;
      }
      if (data?.type === 'prompt') {
        if (member.seat >= 0) {
          const accepted = builders.request(ws, key, owner, member.seat, data, { guest: !member.token });
          void refreshBraincells(ws, member);
          if (accepted) for (const ms of [20000, 90000, 180000]) {
            const timer = setTimeout(() => {
              creditRefreshes.delete(timer);
              void refreshBraincells(ws, member);
            }, ms);
            timer.unref();
            creditRefreshes.add(timer);
          }
        }
        else send(ws, { type: 'build-result', error: 'Spectators cannot build' });
      }
      if (data?.type === 'chalk-clear' && member.seat >= 0) room.match.clearChalk(member.seat);
      if (data?.type === 'chalk-dust' && member.seat >= 0) {
        // Dust the chalk around where the server has this player, not where it says it is.
        const p = room.match.seats[member.seat]?.player;
        if (p) room.match.dustChalk(p.x, p.z, 0.5);
      }
      if (data?.type === 'clear-room' && member.seat >= 0) void builders.clearRoom(key);
      // LB: take an object out of the room (it stays in the $ history).
      if (data?.type === 'remove' && member.seat >= 0 && typeof data.objectId === 'string')
        void store.removeObject(key, data.objectId).then((gone) => {
          if (!gone) return;
          room.match.emit('removed', { at: { x: gone.x, y: 1, z: gone.z } });
          builders.broadcast(key);
        });
      // Everything this handle has made that can stand in the space.
      if (data?.type === 'handle-history') {
        // Asked before the account check finished: answered once it does.
        if (member.handle) void sendHandleHistory(ws, member.handle);
        else {
          member.wantsHistory = true;
          // A guest never signs in: say so rather than leave the menu waiting.
          setTimeout(() => {
            if (member.handle || !member.wantsHistory) return;
            member.wantsHistory = false;
            send(ws, { type: 'handle-history', handle: null, items: [] });
          }, 6000).unref();
        }
      }
      // Sharper or chunkier: change a screen's pixel grid; no Aesel needed.
      if (data?.type === 'wall-res' && member.seat >= 0 && ['north', 'west'].includes(data.face) && [-1, 1].includes(data.dir))
        void store.stepWallRes(key, data.face, data.dir).then(() => builders.broadcast(key));
      else if (data?.type === 'kidlisp-mod' && member.seat >= 0) void builders.requestKidlisp(ws, key, owner, data);
      // A $code prompt stands that KidLisp piece on the field: no Aesel needed.
      if (data?.type === 'screen' && member.seat >= 0 && typeof data.code === 'string' && /^[A-Za-z0-9]{2,16}$/.test(data.code))
        void store
          .addScreen(key, {
            id: randomUUID(),
            owner,
            code: data.code,
            x: clamp(Number(data.x) || 0, -WORLD + 1, WORLD - 1),
            z: clamp(Number(data.z) || 0, -WORLD + 1, WORLD - 1),
            yaw: Number(data.yaw) || 0,
            at: Date.now(),
          })
          .then(() => builders.broadcast(key));
      if (data?.type === 'paint' && member.seat >= 0 && Number.isInteger(data.i) && Number.isInteger(data.j) && data.i >= 0 && data.i < 18 && data.j >= 0 && data.j < 18)
        void store.paintTile(key, data.i, data.j, data.c).then(() => builders.broadcast(key));
      if (data?.type === 'restore' && member.seat >= 0 && typeof data.code === 'string' && /^[A-Za-z0-9]{2,16}$/.test(data.code))
        void builders.restore(ws, key, owner, member.seat, data.code);
      if (data?.type === 'input' && member.seat >= 0)
        room.match.input(member.seat, data.commands);
      if (data?.type === 'ping' && Number.isFinite(data.at))
        send(ws, { type: 'pong', at: data.at });
    });
    ws.on('close', (code) => {
      diagnostics.emit('connection.closed', { session: ws.diagnosticSession, closeCode: code });
      clearTimeout(accountHold);
      for (const timer of creditRefreshes) clearTimeout(timer);
      clearInterval(heartbeat);
      // 🗺️ The avatar rests where it stood (rooms outside the lattice are refused).
      const body = member.handle && room.match.seats[member.seat]?.player;
      if (body)
        void geography
          .change((g) => g.rest(member.handle, { room: key, x: body.x, z: body.z, yaw: body.yaw }))
          .catch(() => {});
      room.remove(ws);
      if (!room.clients.size) rooms.delete(key);
    });
  });

  let previous = performance.now(),
    debt = 0;
  let diagnosticStart = previous, loopGapMaxMs = 0, tickMaxMs = 0;
  const timer = setInterval(() => {
    const now = performance.now();
    if (diagnostics.enabled) loopGapMaxMs = Math.max(loopGapMaxMs, now - previous);
    // After a stall the world catches up (to half a second): ticks lost here
    // would leave every player's commands waiting behind the world's clock.
    debt += Math.min(500, now - previous);
    previous = now;
    while (debt >= 1000 / HZ) {
      debt -= 1000 / HZ;
      for (const room of rooms.values()) {
        room.match.step();
        if (room.match.chalkDirty && room.match.tick % 3 === 0)
          for (const msg of room.match.takeChalk())
            for (const ws of room.clients.keys()) send(ws, msg);
        // Every tick (60 Hz): smaller gaps to interpolate across.
        const snap = JSON.stringify(room.match.snapshot());
        for (const ws of room.clients.keys()) sendText(ws, snap);
      }
    }
    if (diagnostics.enabled) {
      tickMaxMs = Math.max(tickMaxMs, performance.now() - now);
      if (now - diagnosticStart >= 5000) {
        if (rooms.size) diagnostics.emit('server.sample', { durationMs: now - diagnosticStart,
          loopGapMaxMs, tickMaxMs, clients: wss.clients.size, builds: builders.jobs.size,
          buildIds: [...builders.jobs.keys()] });
        diagnosticStart = now;
        loopGapMaxMs = tickMaxMs = 0;
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
