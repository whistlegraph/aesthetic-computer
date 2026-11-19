import http from 'node:http';
import { stateHome } from './env.mjs'; // first: it reads the old DOOL_* names
import { tailDevLog } from './dev-log.mjs';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';
import { createRequire } from 'node:module';
import { attachSocketRoute } from '../lith/socket-routing.mjs';
import { Room } from './room.mjs';
import { GeographyStore, BLOCK, LOTS, lot, blockAt, blockId, blockIndex, parseBlock, isGrounds } from './geography.mjs';
import { edgeAt, landing } from './park.mjs';
import { worldMap, peopleNow } from './map.mjs';
import { guestName } from './guest-name.mjs';
import { TowerStore, GROUND, floorRoom, isFloorRoom, carWalls, lobbyWalls, inCar, CAR_ARRIVAL, CAR, CAR_Z, LOBBY_FRONT, DOOR } from './tower.mjs';
import { isArena, LEDGES } from './bop.mjs';
import { handleColors } from './handle-style.mjs';
import { ObjectStore } from './object-store.mjs';
import { Builders, GUEST_BUILDS } from './builders.mjs';
import { createDiagnostics } from './diagnostics.mjs';
import { stallRow } from './stalls.mjs';
import { createWalks } from './walks.mjs';
import { MAX_PLAYERS, HZ, PROTOCOL, PROTOCOLS, WORLD, clamp } from './world.mjs';
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
    'park',
    'bop',
    'games',
    'elevator',
    'solids',
    'sky',
    'shadow',
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

export function createRaxServer({
  prefix = '',
  // The mount it answered at before a rename: pages are sent on to `prefix`
  // and sockets are taken as if they had asked there.
  alias = '',
  stateDir,
  builderToken = process.env.RAX_BUILDER_TOKEN,
  verifyIdentity = verifyAccount,
  resolveHandleColors = handleColors,
  // 🧹 Who keeps the ground nobody owns: the plaza and the blocks' commons.
  keeper = process.env.RAX_KEEPER || 'jeffrey',
  // 🏢 Every handle on the network, oldest first, for the tower's floors.
  listHandles = async () => {
    const r = await fetch('https://aesthetic.computer/api/handles?limit=100000&order=created', { signal: AbortSignal.timeout(20000) });
    const data = await r.json();
    if (data.order !== 'created' || !Array.isArray(data.handles)) throw Error('handles are not in founding order');
    return data.handles;
  },
  publicURL = process.env.RAX_PUBLIC_URL,
  devLog = process.env.RAX_DEV_LOG,
  publishKidlisp = null,
  diagnosticsEnabled = process.env.RAX_DIAGNOSTICS === '1',
  diagnosticWrite,
  devLogRooms = (process.env.RAX_DEV_LOG_ROOMS || 'studio').split(','),
  // 🪪 A body, a seat and a floor belong to a handle. Without one a visitor
  // flies over and looks (2026-11-17); RAX_GUESTS=1 seats anyone, for a
  // private or local server with no accounts.
  guests = process.env.RAX_GUESTS === '1',
} = {}) {
  if ((prefix && !/^\/[a-z0-9-]+$/.test(prefix)) || (alias && (!prefix || !/^\/[a-z0-9-]+$/.test(alias))))
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
  // 👣 Who came in and how they moved, kept beside the stores (walks.mjs).
  const walks = createWalks({ directory: stateDir && resolve(stateDir, 'walks') });
  const store = new ObjectStore(stateDir),
    geography = new GeographyStore(stateDir), // 🗺️ who lives where (GEOGRAPHY.md)
    tower = new TowerStore(stateDir), // 🏢 who holds which floor (tower.mjs)
    loaded = Promise.all([store.load(), geography.load(), tower.load(), readFile(resolve(here, '../system/public/.commit-ref'), 'utf8')
      .then((r) => { diagnostics.setRevision(r.trim()); stallRevision = r.trim().slice(0, 10); }).catch(() => {})]);
  // 🏢 The houses beside the tower come down (2026-11-17): a home is a floor
  // of the tower. What stood inside each goes up to its handle's floor; both
  // ledgers are kept as they stood (geography.before-houses.json,
  // objects.before-houses.json). Runs once: after it there are no houses.
  const ready = loaded.then(async () => {
    const pods = Object.values(geography.geography.pods);
    if (!pods.length) return;
    await geography.keep('geography.before-houses.json');
    const pairs = [];
    for (const p of pods) {
      const f = await tower.change((t) => {
        try { return t.arrive(p.handle, p.accountId || null); } catch { return t.forHandle(p.handle); }
      }).catch(() => null);
      if (f) pairs.push([p.id, floorRoom(f.id)]);
    }
    // (A house whose handle has no floor keeps its things in the store, under its old name.)
    const moved = await store.moveRooms(pairs);
    await geography.change((g) => g.retire());
    console.log(`rax: ${pods.length} houses retired, ${moved} builds moved to ${pairs.length} floors`);
  }).catch((e) => console.error('rax: houses not retired:', e?.message || e))
    // 🧹 The ground is cleared once (2026-11-17): the plaza and the blocks
    // start empty; what stood there is kept with the removals.
    .then(async () => {
      const swept = await store.clearGround((room) => room === GROUND || !!parseBlock(room), 'keeper');
      if (swept) console.log(`rax: ${swept} builds cleared from the ground`);
    }).catch((e) => console.error('rax: ground not cleared:', e?.message || e));
  const profiles = new Map();
  const colorsFor = (handle) => {
    let cached = profiles.get(handle);
    if (!cached || Date.now() - cached.at > 60000) {
      cached = { at: Date.now(), colors: null };
      cached.promise = Promise.resolve().then(() => resolveHandleColors(handle)).then(colors => {
        cached.colors = colors; return colors;
      }).catch(() => null);
      profiles.set(handle, cached);
      if (profiles.size > 256) profiles.delete(profiles.keys().next().value);
    }
    return cached;
  };
  const parkData = key => ({ neighbours: geography.geography.neighbours(key), plaza: key === 'lobby' });
  // What a socket hears of the place (its neighbours), and what the room
  // keeps of it: its solids and the sides that lead on.
  const sendPark = (ws, key) => {
    send(ws, { type: 'park', ...parkData(key) });
    rooms.get(key)?.match.setWalls(wallsFor(key)); // the elevator and the lobby are solid
    if (rooms.has(key)) rooms.get(key).match.open = Object.keys(geography.geography.neighbours(key) || {});
  };
  // 🏠 Home is the handle's floor of the tower.
  const sendHome = (ws, handle) => {
    const f = tower.tower.forHandle(handle);
    if (f) send(ws, { type: 'home', room: floorRoom(f.id), floor: f.number });
  };
  // 🏢 The tower as this socket sees it: how tall, whose floor this is, and
  // which floor is theirs.
  const hasCar = (key) => key === GROUND || isFloorRoom(key);
  // The elevator is solid on every floor; the lobby's glass and desk on the ground.
  const wallsFor = (key) => [...(hasCar(key) ? carWalls() : []), ...(key === GROUND ? lobbyWalls() : [])];
  const sendTower = (ws, key, member) => {
    const t = tower.tower, here = t.forRoom(key);
    // Every room hears how tall the tower is: it stands on the skyline.
    if (!hasCar(key)) return void send(ws, { type: 'tower', count: t.count, top: t.top, revision: t.revision, here: null, yours: null, owner: false });
    const mine = member?.handle ? t.forHandle(member.handle) : null;
    // The floors with someone on them now, lowest first: [number, whose, how many].
    const inside = [];
    for (const [id, r] of rooms) {
      const f = t.forRoom(id), people = f ? r.members.filter(Boolean).length : 0;
      if (people) inside.push([f.number, f.handle, people]);
    }
    inside.sort((a, b) => a[0] - b[0]);
    send(ws, { type: 'tower', count: t.count, top: t.top, revision: t.revision, inside: inside.slice(0, 8),
      here: here ? { ...t.public(here), colors: profiles.get(here.handle)?.colors || null } : { room: GROUND, number: 0 },
      yours: t.public(mine), owner: !!here && t.owns(here, member) });
    if (here && !profiles.get(here.handle)?.colors)
      void colorsFor(here.handle).promise.then((colors) => {
        if (colors && rooms.get(key)?.sessions.has(ws)) sendTower(ws, key, member);
      });
  };
  const towerChanged = () => {
    for (const [key, room] of rooms) for (const [ws, m] of room.sessions) {
      sendTower(ws, key, m);
      if (m.handle) sendHome(ws, m.handle);
    }
  };
  // New handles are given floors as the network grows.
  // (At start the handles API may not be answering yet — on lith it is this
  // same process — so a failed read is tried again soon, a few times.)
  const admitHandles = async (triesLeft = 0) => {
    try {
      const handles = await listHandles();
      if (await tower.change((t) => t.admit(handles))) towerChanged();
    } catch {
      // The tower keeps the floors it has.
      if (triesLeft > 0) setTimeout(() => admitHandles(triesLeft - 1), 20000).unref?.();
    }
  };
  void ready.then(() => admitHandles(6), () => {});
  const admitTimer = setInterval(() => admitHandles(), 10 * 60 * 1000);
  admitTimer.unref?.();
  // Short-lived, one-use travel receipts bind a server position to an owner.
  const passages = new Map();
  // What a passage carries besides the place: who walked through it. The
  // room they left knew them, so the room they reach need not ask again.
  const carry = (member) => ({ handle: member.handle || null, accountId: member.accountId || null,
    colors: member.colors || null, token: member.token || null });
  // 🔐 A token that verified a few minutes ago is not asked about again: a
  // reload or a new room would otherwise wait on two round trips every time.
  const verified = new Map();
  const verifyOnce = async (token) => {
    const known = verified.get(token);
    if (known && known.until > Date.now()) return known.account;
    const account = await verifyIdentity(token);
    for (const [t, v] of verified) if (v.until < Date.now()) verified.delete(t);
    if (verified.size < 2048) verified.set(token, { account, until: Date.now() + 300000 });
    return account;
  };
  const handle = async (req, res) => {
    let url;
    try {
      url = new URL(req.url, 'http://localhost');
    } catch {
      res.writeHead(400).end();
      return;
    }
    if (alias && (url.pathname === alias || url.pathname.startsWith(`${alias}/`))) {
      res
        .writeHead(307, {
          Location: `${prefix}${url.pathname.slice(alias.length) || '/'}${url.search}`,
          'Cache-Control': 'no-store',
        })
        .end();
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
            name: 'rax',
            building: builders.jobs.size,
            persistent: !!stateDir, // false means the room dies with the process
            protocol: PROTOCOL,
            revision: revision.trim(),
          }),
        );
      return;
    }
    // 🏢 The tower's directory: [number, handle, held] from the ground up,
    // or one floor by number or handle (?q=89, ?q=@jeffrey).
    if (path === '/tower' && req.method === 'GET') {
      await ready;
      const t = tower.tower, q = url.searchParams.get('q');
      const found = q === null ? null : t.public(t.resolve(q));
      res.writeHead(q !== null && !found ? 404 : 200, { 'Content-Type': 'application/json',
        'Cache-Control': 'no-store', 'Access-Control-Allow-Origin': '*' })
        .end(JSON.stringify(q !== null ? found || { error: 'No such floor' }
          : { revision: t.revision, count: t.count, top: t.top, floors: t.directory() }));
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
        for (const m of r.sessions.values()) {
          const p = m.handle && r.match.seats[m.seat]?.player;
          if (p) awake.push({ handle: m.handle.toLowerCase(), room, x: Math.round(p.x * 100) / 100, z: Math.round(p.z * 100) / 100 });
        }
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
    // 🗺️ The world as a drawing (map.mjs): every block, house, the tower
    // and what stands in the rooms as plain shapes in world metres, with
    // everyone who is in rax now. Readers outside rax draw from this.
    // ⚡ /people is the same people alone, for a reader that wants them moving.
    if ((path === '/map' || path === '/people') && req.method === 'GET') {
      await ready;
      const seated = [], watching = [];
      for (const [room, r] of rooms)
        for (const m of r.sessions.values()) {
          const p = m.handle && r.match.seats[m.seat]?.player;
          if (p) seated.push({ handle: m.handle, room, x: p.x, y: p.y, z: p.z, yaw: p.yaw });
          // Someone looking on, if their eye was heard from in the last ten seconds.
          else if (m.seat < 0 && m.look && Date.now() - m.look.at < 10000 && (m.handle || m.owner))
            watching.push({ name: m.handle ? `@${m.handle}` : guestName(m.owner), room, ...m.look });
        }
      if (path === '/people') {
        res
          .writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store', 'Access-Control-Allow-Origin': '*' })
          .end(JSON.stringify(peopleNow({ geography: geography.geography, tower: tower.tower, seated, watching })));
        return;
      }
      res
        .writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store', 'Access-Control-Allow-Origin': '*' })
        .end(JSON.stringify(worldMap({
          geography: geography.geography,
          tower: tower.tower,
          seated,
          watching,
          contents: (id) => ({ tiles: store.tiles(id), objects: store.objects(id), screens: store.screens(id) }),
        })));
      return;
    }
    // 🌍 The world around a block: itself and every open block beside it,
    // each with its lots and what stands and is painted there, so a client
    // draws the neighbourhood in place and keeps it between visits.
    if (path === '/world' && req.method === 'GET') {
      await ready;
      const asked = url.searchParams.get('room') || '';
      const at = asked === 'lobby' ? { i: 0, j: 0 } : parseBlock(asked);
      const blocks = [];
      if (at && blockIndex(at) <= geography.geography.opened)
        for (let dj = -1; dj <= 1; dj++)
          for (let di = -1; di <= 1; di++) {
            const b = { i: at.i + di, j: at.j + dj }, n = blockIndex(b);
            if (n > geography.geography.opened) continue;
            const id = blockId(b);
            blocks.push({
              id, di, dj, plaza: id === 'lobby',
              neighbours: geography.geography.neighbours(id),
              objects: store.objects(id).filter((o) => !o.item),
              tiles: store.tiles(id),
            });
          }
      res
        .writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' })
        .end(JSON.stringify({ room: at ? blockId(at) : null, revision: geography.geography.revision, blocks }));
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
            park: parkData(id),
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
    if (alias && url.pathname.startsWith(`${alias}/`)) url.pathname = prefix + url.pathname.slice(alias.length);
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
        [prefix, ...(alias ? [alias] : [])].flatMap((p) => [`${p}/play`, `${p}/builder`]),
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
  // 🚧 Where a build may not stand: in the room's own solids, in the car, in
  // front of its door, in the lobby's entrance or on the walk that leads to it.
  const clear = (xMin, xMax, zMin, zMax) => ({ type: 'box', xMin, xMax, yMin: 0, yMax: 12, zMin, zMax });
  const keepClear = (key) => [
    ...wallsFor(key),
    // The car, and an aisle from its door to the middle of the room.
    ...(hasCar(key) ? [clear(-CAR, CAR, -1, CAR_Z + CAR)] : []),
    ...(key === GROUND ? [clear(-DOOR, DOOR, LOBBY_FRONT - 2, LOBBY_FRONT + 2), clear(-1.2, 1.2, -WORLD, CAR_Z - CAR)] : []),
  ];
  const builders = new Builders({ token: builderToken, store, rooms, send, publish: publishKidlisp, diagnostics, solids: keepClear });
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
      !PROTOCOLS.includes(url.searchParams.get('protocol'))
    ) {
      ws.close(1008, 'Invalid room or build');
      return;
    }
    const key = roomId;
    if ((key.startsWith('pod-') && !geography.geography.pod(key)) ||
        (key.startsWith('floor-') && !tower.tower.forRoom(key)) ||
        (parseBlock(key) && blockIndex(parseBlock(key)) > geography.geography.opened)) {
      ws.close(1008, 'Place is not open'); return;
    }
    if (!rooms.has(key)) {
      if (rooms.size >= 32) {
        ws.close(1013, 'Server full');
        return;
      }
      rooms.set(key, new Room(send, { guests }));
      // 🛗 In a room with the car, bodies land in a row south of the middle,
      // facing the car: never inside it, never against a planter.
      if (hasCar(key)) rooms.get(key).match.spawnAt = (seat) => seat === 0 ? { x: 0, z: -5, yaw: 0 }
        : { x: (seat % 2 ? -1 : 1) * 1.5 * Math.ceil((seat % 8 || 8) / 2), z: seat < 9 ? -6.3 : -7.2, yaw: 0 };
      if (isArena(key)) rooms.get(key).match.setArena(LEDGES);
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
    // 🎬 watch=1 looks on without ever taking a seat (a film's camera).
    const watching = url.searchParams.get('watch') === '1';
    const member = room.add(ws, nativeAccount || webAccount || watching);
    // Nobody with a handle is here: say what the seat waits for, so the
    // page can ask for it (an account, or a handle on the account).
    const gate = () => {
      if (guests || watching || member.handle || !room.sessions.has(ws)) return;
      send(ws, { type: 'gate', needs: member.signedIn ? 'handle' : 'account' });
    };
    const seatGuest = () => {
      clearTimeout(accountHold);
      if (room.sessions.has(ws) && !member.ready && !watching) {
        member.ready = true;
        room.assign();
      }
      gate();
    };
    const accountHold = webAccount ? setTimeout(seatGuest, 6000) : null;
    accountHold?.unref?.();
    room.match.setObjects(store.objects(key));
    room.match.setWalls(wallsFor(key));
    // An old build in the way of the car or the entrance is stood aside.
    if (hasCar(key)) void builders.tidy(key).then((moved) => {
      if (!moved) return;
      room.match.setObjects(store.objects(key));
      builders.broadcast(key);
    }).catch(() => {});
    let arrived = false, leaving = null; // leaving: the passage this socket was last given
    // 👀 Told its place is another room, and not yet walked a step in this
    // one: a look in on the way, which leaves the avatar's rest where it was.
    let passing = false;
    // 🎟️ A passage is the visitor themself, known a moment ago in the room
    // they left: it seats them where it leads as soon as the page says whose
    // it is, with the handle they wore, and the account is checked after.
    const arrive = () => {
      if (arrived || !member.owner || watching) return;
      const receipt = url.searchParams.get('passage'), pass = passages.get(receipt);
      if (!pass || pass.owner !== member.owner || pass.room !== key || pass.until < Date.now()) return;
      passages.delete(receipt);
      for (const k of ['handle', 'accountId', 'colors', 'token']) if (pass[k] && !member[k]) member[k] = pass[k];
      if (!member.ready) { clearTimeout(accountHold); member.ready = true; room.assign(); }
      const body = room.match.seats[member.seat]?.player;
      if (!body) return;
      arrived = true;
      passing = false;
      Object.assign(body, pass.position);
      // The time on the way was lived: the commands given in it are owed a run.
      room.match.owe(member.seat, Math.round(((Date.now() - pass.at) * HZ) / 1000));
      send(ws, { type: 'arrival', yaw: pass.position.yaw, pitch: pass.position.pitch });
      send(ws, room.match.snapshot());
    };
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
        const account = await verifyOnce(token);
        member.signedIn = true; // An account, with or without a handle yet.
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
        member.handle = handle;
        member.accountId = account.sub;
        // 🏢 Its floor of the tower is its home, held for good from now on.
        // (Homes on the park's lots stay where they stand; none are added.)
        void tower.change((t) => t.arrive(handle, account.sub)).then(() => {
          sendHome(ws, handle);
          sendTower(ws, key, member);
          // 🌍 One persistent world: say where this handle last stood, so a
          // visit that has not gone anywhere yet picks up there. A first
          // visit starts on the plaza, at the foot of the tower.
          const last = geography.geography.whereabouts(handle)?.room;
          const open = last && (last === 'lobby' || tower.tower.forRoom(last) ||
            (parseBlock(last) && blockIndex(parseBlock(last)) <= geography.geography.opened));
          const place = open ? last : 'lobby';
          passing = place !== key && !arrived;
          send(ws, { type: 'whereabouts', room: place });
        }).catch(() => {});
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
        if (!arrived && !takeover && body && rest?.room === key) {
          const edge = BLOCK / 2 - 0.4; // inside the walls
          body.x = Math.max(-edge, Math.min(edge, rest.x));
          body.z = Math.max(-edge, Math.min(edge, rest.z));
          body.yaw = rest.yaw;
        }
        arrive();
        const colors = await colorsFor(handle).promise;
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
      guests,
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
    sendPark(ws, key);
    sendTower(ws, key, member);
    send(ws, room.match.snapshot());
    send(ws, builders.state(key, member));
    room.broadcast();
    if (nativeAccount) void identifyAccount(authorization.slice(7)).finally(gate);
    else if (!webAccount) gate();
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
      // (A link that held its messages back delivers seconds of them at once: that is not a flood.)
      if (++messages > 600) {
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
              arrive();
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
      if (data?.type === 'walk' && member.owner && member.seat >= 0) {
        const p = room.match.seats[member.seat]?.player;
        if (!p) return;
        let destination, position, walked;
        if (data.type === 'walk') {
          const edge = edgeAt(p);
          destination = geography.geography.neighbours(key)?.[edge];
          // Asked a moment early (the commands that carry the body over the
          // line are still queued): the world's clock asks again each tick.
          if (!destination || edge !== data.edge) {
            if (!data.again) member.walk = { until: Date.now() + 1500,
              ask: () => { messages--; ws.emit('message', JSON.stringify({ type: 'walk', edge: data.edge, again: true })); } };
            return;
          }
          member.walk = null;
          // The whole motion crosses, not only the place: speed and a jump carry over.
          // (A body that walked on past the line lands that much further in,
          // unless a house stands there: then in the yard, as from the line.)
          let spot = landing(p, edge);
          const inWall = (q) => wallsFor(destination).some((b) => q.x + 0.4 > b.xMin && q.x - 0.4 < b.xMax && q.z + 0.4 > b.zMin && q.z - 0.4 < b.zMax);
          if (inWall(spot)) spot = landing({ ...p, x: clamp(p.x, -WORLD + 0.4, WORLD - 0.4), z: clamp(p.z, -WORLD + 0.4, WORLD - 0.4) }, edge);
          position = { ...spot, y: p.y, vx: p.vx, vy: p.vy, vz: p.vz, onGround: p.onGround };
          walked = room.match.seats[member.seat].ack;
        }
        // Asked again before the answer got there (a slow link): the same
        // passage again, never a new one that would void the first.
        if (leaving && leaving.until > Date.now()) return void send(ws, leaving.message);
        for (const [id, pass] of passages) if (pass.until < Date.now() || pass.owner === member.owner) passages.delete(id);
        if (passages.size >= 1024) return;
        const receipt = randomUUID();
        passages.set(receipt, { owner: member.owner, ...carry(member), room: destination, position, at: Date.now(), until: Date.now() + 30000 });
        // `ack` is the last command this room ran: the walker's later ones go with them.
        leaving = { until: Date.now() + 10000, message: { type: 'travel', room: destination, passage: receipt, at: position, ...(walked === undefined ? {} : { ack: walked }) } };
        send(ws, leaving.message);
        return;
      }
      // 🛗 The elevator: from the car, to a floor by number or handle, to
      // the ground, or one floor up or down.
      if (data?.type === 'elevator' && member.owner && member.seat >= 0 && hasCar(key)) {
        const p = room.match.seats[member.seat]?.player, t = tower.tower;
        // Asked from outside the car: say so, rather than nothing.
        if (!inCar(p)) return void send(ws, { type: 'elevator-result', error: 'step into the car' });
        const asked = String(data.to ?? '').trim().toLowerCase().slice(0, 24), here = t.forRoom(key);
        const to = ['my floor', 'my', 'mine'].includes(asked) ? 'home' : asked;
        let destination = null, found = null;
        if (['0', 'g', 'ground', 'plaza', 'lobby'].includes(to)) destination = GROUND;
        else {
          found = to === 'up' ? t.step(here ? here.number : 0, 1)
            : to === 'down' ? (here ? t.step(here.number, -1) : null)
            : to === 'home' ? (member.handle ? t.forHandle(member.handle) : null) : t.resolve(to);
          if (to === 'down' && here && !found) destination = GROUND;
          else if (found) destination = floorRoom(found.id);
        }
        if (!destination) return void send(ws, { type: 'elevator-result',
          error: to === 'home' ? 'sign in for a floor' : to === 'up' ? 'this is the top' : `no floor ${to}` });
        if (destination === key) return void send(ws, { type: 'elevator-result', error: 'you are here' });
        // Asked again before the answer got there (a slow link): the same
        // passage again, never a new one that would void the first.
        if (leaving && leaving.until > Date.now()) return void send(ws, leaving.message);
        for (const [id, pass] of passages) if (pass.until < Date.now() || pass.owner === member.owner) passages.delete(id);
        if (passages.size >= 1024) return;
        const receipt = randomUUID();
        passages.set(receipt, { owner: member.owner, ...carry(member), room: destination, position: { ...CAR_ARRIVAL }, at: Date.now(), until: Date.now() + 30000 });
        leaving = { until: Date.now() + 10000, message: { type: 'travel', room: destination, passage: receipt, at: { ...CAR_ARRIVAL },
          ride: { from: here ? here.number : 0, to: found ? found.number : 0, handle: found?.handle || null } } };
        send(ws, leaving.message);
        return;
      }
      // Ask for a floor number, the way a name is registered.
      if (data?.type === 'take-floor') {
        if (!member.handle) return void send(ws, { type: 'elevator-result', error: 'sign in to take a floor' });
        void tower.change((t) => t.take(member.handle, Number(data.number)))
          .then((f) => { towerChanged(); send(ws, { type: 'elevator-result', note: `floor ${f.number} is yours` }); })
          .catch((e) => send(ws, { type: 'elevator-result', error: e.message.toLowerCase() }));
        return;
      }
      // A pod or a floor is public to visit, but only its verified owner arranges it.
      const floor = tower.tower.forRoom(key);
      const pod = geography.geography.pod(key) || floor;
      const ownPod = floor ? tower.tower.owns(floor, member)
        : pod && (pod.accountId ? pod.accountId === member.accountId : pod.handle === member.handle?.toLowerCase());
      // 🧹 The keeper may revise an object where it stands, in anyone's pod.
      const keeping = data?.type === 'prompt' && typeof data.objectId === 'string' && !!member.handle && member.handle.toLowerCase() === keeper;
      if (pod && !ownPod && !keeping && ['prompt', 'clear-room', 'remove', 'wall-res', 'kidlisp-mod', 'screen', 'paint', 'restore', 'chalk-clear', 'chalk-dust'].includes(data?.type)) {
        send(ws, { type: 'build-result', error: floor ? `Only @${floor.handle} can arrange this floor` : 'Only the pod owner can arrange this pod' });
        if (data.type === 'paint') builders.broadcast(key); // the page drew the square already: take it back
        return;
      }
      // 🧹 The plaza and the blocks belong to the world, not to whoever walks
      // in: only the keeper arranges them, or (in a block) someone who lives
      // there. A visitor builds on their own floor. Rooms outside the world
      // (a game, a room made by name) stay open to whoever is in them.
      const block = !pod && parseBlock(key);
      if (block && ['prompt', 'clear-room', 'remove', 'wall-res', 'kidlisp-mod', 'screen', 'paint', 'restore'].includes(data?.type)) {
        const handle = member.handle?.toLowerCase();
        const lives = key !== GROUND && handle && geography.geography.home(handle)?.block === blockIndex(block);
        if (handle !== keeper && !lives) {
          send(ws, { type: 'build-result', error: key === GROUND ? 'The plaza is kept as it is: build on your floor' : 'Only someone who lives on this block arranges it' });
          if (data.type === 'paint') builders.broadcast(key);
          return;
        }
      }
      if (pod && !ownPod && data?.type === 'input' && Array.isArray(data.commands))
        data.commands = data.commands.map(c => ({ ...c, chalk: null, use: false, drop: false, fire: false }));
      if (data?.type === 'prompt') {
        if (member.seat >= 0) {
          const accepted = builders.request(ws, key, owner, member.seat, data, { guest: !member.token, keeper: keeping });
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
        void store.removeObject(key, data.objectId, member.handle || owner).then((gone) => {
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
      // 👀 Where someone looking on has their eye, kept for the map alone.
      if (data?.type === 'look' && member.seat < 0 && [data.x, data.y, data.z, data.yaw].every(Number.isFinite))
        member.look = { x: clamp(data.x, -60, 60), y: clamp(data.y, 0, 40), z: clamp(data.z, -60, 60), yaw: data.yaw, at: Date.now() };
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
      if (data?.type === 'input' && member.seat >= 0) {
        if (passing && Array.isArray(data.commands) && data.commands.some((c) => c?.fwd || c?.right || c?.buttons)) passing = false;
        room.match.input(member.seat, data.commands);
      }
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
      if (body && !passing)
        void geography
          .change((g) => g.rest(member.handle, { room: key, x: body.x, z: body.z, yaw: body.yaw }))
          .catch(() => {});
      walks.leave(member);
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
      for (const [key, room] of rooms) {
        room.match.step();
        walks.tick(key, room);
        for (const m of room.sessions.values())
          if (m.walk) { if (m.walk.until < Date.now()) m.walk = null; else m.walk.ask(); }
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
    geography,
    tower,
    builders,
    walks,
    ready,
    async close() {
      builders.close();
      stopDevLog();
      clearInterval(braincellTimer);
      clearInterval(timer);
      clearInterval(admitTimer);
      for (const remove of detach) remove();
      for (const ws of wss.clients) ws.terminate();
      await new Promise((resolve) => wss.close(resolve));
      await new Promise((resolve) => builderWss.close(resolve));
      await store.queue;
      await geography.queue;
      await tower.queue;
      await walks.close();
      if (server.listening)
        await new Promise((resolve) => server.close(resolve));
    },
  };
}

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const app = createRaxServer({
    stateDir: stateHome(),
    // On its own (a laptop, a private server) there is no AC session to
    // sign in with: anyone is seated unless RAX_GUESTS=0.
    guests: process.env.RAX_GUESTS !== '0',
  });
  await app.ready;
  const port = Number(process.env.PORT || 8787),
    host = process.env.HOST || '127.0.0.1';
  app.server.listen(port, host, () =>
    console.log(`rax → http://${host}:${port}`),
  );
  for (const signal of ['SIGINT', 'SIGTERM'])
    process.once(signal, () => app.close().then(() => process.exit(0)));
}
