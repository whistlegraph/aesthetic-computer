import { BASEMENT, cellarFixtures, STAIR, stairHeight, stairPassage } from './basement.mjs';
import http from 'node:http';
import { stateHome } from './env.mjs'; // first: it reads the old DOOL_* names
import { tailDevLog } from './dev-log.mjs';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';
import { createRequire } from 'node:module';
import { attachSocketRoute } from '../lith/socket-routing.mjs';
import { Room } from './room.mjs';
import { GeographyStore, fromWorld, BLOCK, LOTS, lot, blockAt, blockId, blockIndex, parseBlock, isGrounds } from './geography.mjs';
import { edgeAt, landing, SPAN, wrap, onLap } from './park.mjs';
import { createHorde, ZOMBIE } from './horde.mjs';
import { createDispenser, DISPENSER_WALL, MAX_BALLS, carry as carryBall, liftOf } from './ball.mjs';
import { readFileSync as readBall } from 'node:fs';
import { mkdir as mkdirBall, writeFile as writeBall, rename as renameBall } from 'node:fs/promises';
import { MAX_FOCUS_ZOOM, OBJECT_PROXY_DISTANCE } from './screen-lod.mjs';
import { objectBoxes, distantBoxes } from './objects.mjs';
import { worldMap, peopleNow } from './map.mjs';
import { WEATHER_URL, CLEAR, read as readWeather } from './weather.mjs';
import { hungFor } from './orbits.mjs';
import { guestName } from './guest-name.mjs';
import { atriumRoof, aloft, storeyWindows, towerSides, SLAB, clearOfCar, altitude, floorsNear, rideSeconds, rideEase, shutTicks, ROOF, roofOf, roofNumber, roofWalls, storeyAt, storeyOf, PARAPET } from './elevator.mjs';
import { TowerStore, GROUND, floorRoom, isFloorRoom, carTo, carWalls, lobbyWalls, inCar, CAR_ARRIVAL, CAR, CAR_Z, LOBBY_FRONT, DOOR } from './tower.mjs';
import { kioskPlan } from './kiosks-plan.mjs';
import { isArena, LEDGES } from './bop.mjs';
import { handleColors } from './handle-style.mjs';
import { ObjectStore } from './object-store.mjs';
import { Builders, GUEST_BUILDS } from './builders.mjs';
import { createDiagnostics } from './diagnostics.mjs';
import { stallRow } from './stalls.mjs';
import { createWalks } from './walks.mjs';
import { MAX_PLAYERS, HZ, PROTOCOL, PROTOCOLS, halfOf, clamp, CHALK, CFG, ONE_ROOM, HOME_ROOM, CONJURE_MS, placeFor, worldPlace, slabOf, roofBoxes, STAGE, BELL_MS } from './world.mjs';
import { gameFor } from './games.mjs';
import { handsValid, headValid } from './immersive.mjs';
import { nativeScene } from './native-scene.mjs';
import { nativeLive } from './native-live.mjs';
import { randomUUID } from 'node:crypto';
import { verifyAccount } from '../aesel/src/account-access.mjs';
import { createPairing, ipOf, jsonBody } from './pairing.mjs';
import { createCommunalNotepat } from './communal-notepat.mjs';
import { createKiosks } from './kiosks.mjs';

const here = dirname(fileURLToPath(import.meta.url));
// Lith installs this dependency in production; standalone development can resolve root's copy.
const { WebSocketServer, WebSocket } = createRequire(
  new URL('../lith/package.json', import.meta.url),
)('ws');
const assets = new Map([
  ['/', ['index.html', 'text/html']],
  ['/pair', ['pair.html', 'text/html']], // 📱 the phone's side of a pairing (pairing.mjs)
  ...[
    'pair',
    'pair-number',
    'pair-desk',
    'pair-view',
    'client',
    'view',
    'pool-view',
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
    'agent',
    'lattice',
    'controller',
    'freecam',
    'hud',
    'build-line',
    'radial',
    'touch',
    'gyro',
    'immersive',
    'sfx',
    'highlight',
    'chunky-font',
    'kidlisp-wall',
    'screen-lod',
    'ball',
    'weather',
    'air',
    'planet',
    'lens-host', 'lens-host-gl', // 🔭 the lens on the card: WebGPU, and WebGL 2 where there is none
    'flyover', // 🛫 a drone's idle tour // 🔭 the lens on the card // 🌍 the small planet's sums (project.mjs, view.mjs, freecam.mjs)
    'diagnostics-client',
    'stalls',
    'rounds', // 🔫 rounds in flight, flown on and fired at once
    'gore', // 🩸 ragdolls, gibs, spurts and splats
    'park',
    'bop',
    'games',
    'elevator', 'basement',
    'notepat-screen',
    'kiosks-plan', // 🧩 which kiosks stand where
    'panel', // 🖼️ world panels, and the pieces on them
    'dock', // 🖥️ going in to a screen: its sums
    'pieces/keypad',
    'ac-piece', // 🧩 real AC disks on world panels
    'ac-window', // 🪟 whole AC pages on world panels
    'solids',
    'sky',
    'shadow',
    'orbits', // 🪐 the planets going round (view.mjs's sky)
    'bike', 'bike-decor', 'handle-style', // 🛵 the cabin bike, and its rider's paintings and colours
    'skin', 'textures', 'texture-loaders', 'texture-demo', // 🖼️ textures: models' skins, their sources, the cube
  ].map((n) => [`/${n}.mjs`, [`${n}.mjs`, 'text/javascript']]),
  ['/scream.mp3', ['scream.mp3', 'audio/mpeg']], // 😱 a real scream, for a fall off the roof (sfx.mjs fallScream)
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
  ...['scene3d', 'scene3d-webgpu', 'block-font'].map((n) => [
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
  // 🔒 A floor or pod is arranged by its owner, the roof, plaza and blocks by
  // the keeper: the first gate of papers/arxiv-gated-making (2026-11-25).
  // RAX_OPEN_BUILD=1 opens every room to everyone, as 2026-11-23 to 11-25.
  openBuild = process.env.RAX_OPEN_BUILD === '1',
  // 🏠 One room (world.mjs ONE_ROOM): everyone lands in the plaza and no way
  // leads out. RAX_ONE_ROOM=0 or 1 says so outright; the tests, which still
  // cover the whole world's ways, run with it off unless they ask.
  oneRoom = process.env.RAX_ONE_ROOM ? process.env.RAX_ONE_ROOM === '1' : process.env.NODE_TEST_CONTEXT ? false : ONE_ROOM,
  // ✨ How long a conjuring stands there (ms).
  conjureMs = CONJURE_MS,
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
  // ☠️ Where deaths are counted (deaths.mjs deathLedger): { count(handle, kind), read(handle) }. None given, none are.
  deaths = null,
  diagnosticsEnabled = process.env.RAX_DIAGNOSTICS === '1',
  diagnosticWrite,
  devLogRooms = (process.env.RAX_DEV_LOG_ROOMS || 'studio').split(','),
  // 🪪 A body, a seat and a floor belong to a handle. Without one a visitor
  // flies over and looks (2026-11-17); RAX_GUESTS=1 seats anyone, for a
  // private or local server with no accounts.
  guests = process.env.RAX_GUESTS === '1',
  pairLimits, // 📱 pairing.mjs PAIR_LIMITS, for a test
  // 🛰️ A satellite: a rax on another box (nearer some players) that keeps
  // only these rooms (satellite.mjs, ROOM_HOSTS in world.mjs). It takes
  // sockets from pages served by the hosts in `origins` as well as its own,
  // builds nothing (lith keeps the store) and sends nobody up to a roof it
  // does not have. Unset, this is the whole world: lith's rax.
  onlyRooms = process.env.RAX_ONLY_ROOMS ? process.env.RAX_ONLY_ROOMS.split(',') : null,
  origins = (process.env.RAX_ORIGINS || '').split(',').filter(Boolean),
  // 🌐 One world across hosts: which rooms another box keeps (lith: world.mjs
  // ROOM_HOSTS, passed by lith/rax-process.mjs; unset, every room is here),
  // the host this rax answers at, and home (lith) for a satellite's
  // neighbours. A walk into a room kept elsewhere hands the walker over with
  // a passage the other host fetches from this one (GET /passage).
  roomHosts = {},
  selfHost = process.env.RAX_SELF_HOST || 'aesthetic.computer',
  homeHost = process.env.RAX_HOME_HOST || 'aesthetic.computer',
  // 🔗 The link between hosts: a satellite takes it only from these addresses
  // (lith's own); lith dials each satellite at `linkURL`.
  linkFrom = (process.env.RAX_LINK_FROM || '').split(',').filter(Boolean),
  linkURL = (host) => `wss://${host}/rax/link`,
  fetchPassage = async (host, receipt) => {
    const r = await fetch(`https://${host}/rax/passage?receipt=${encodeURIComponent(receipt)}`, { signal: AbortSignal.timeout(4000) });
    return r.ok ? r.json() : null;
  },
} = {}) {
  const satellite = Array.isArray(onlyRooms) && onlyRooms.length > 0;
  // Where a room is kept, when not here (null: here).
  if (oneRoom && !satellite) roomHosts = {}; // (🏠 one room: nothing is kept on another box)
  const keptAt = (room) => (satellite ? (onlyRooms.includes(room) ? null : homeHost) : roomHosts[room] || null);
  const peers = new Set([homeHost, ...Object.values(roomHosts)].filter((h) => h && h !== selfHost));
  if ((prefix && !/^\/[a-z0-9-]+$/.test(prefix)) || (alias && (!prefix || !/^\/[a-z0-9-]+$/.test(alias))))
    throw new Error('Invalid mount path');
  const rooms = new Map();
  const connections = new Set();
  const bodyPose = ({ x, y, z, yaw, pitch }) => ({ x, y, z, yaw, pitch });
  const diagnostics = createDiagnostics({ enabled: diagnosticsEnabled, write: diagnosticWrite });
  // AC's profanity filter, loaded on first use (a static import of a module
  // with its own dependencies has taken lith down before); without it the
  // line passes as typed.
  let chatFilterLoad = null;
  const chatFilter = () => (chatFilterLoad ||= import('../shared/filter.mjs')
    .then((m) => (text) => { try { return String(m.filter(text)); } catch { return text; } })
    .catch(() => (text) => text));
  const loopStalls = []; // 🧊 when this process's own clock came late (over 100 ms), newest last
  const stallLog = []; // 🧊 The newest stalls clients reported (GET /stalls).
  let stallRevision = 'local';
  // 👣 Who came in and how they moved, kept beside the stores (walks.mjs).
  const walks = createWalks({ directory: stateDir && resolve(stateDir, 'walks') });
  const store = new ObjectStore(stateDir),
    geography = new GeographyStore(stateDir), // 🗺️ who lives where (GEOGRAPHY.md)
    tower = new TowerStore(stateDir), // 🏢 who holds which floor (tower.mjs)
    loaded = Promise.all([store.load(), geography.load(), tower.load(), readFile(resolve(here, '../system/public/.commit-ref'), 'utf8')
      .then((r) => { diagnostics.setRevision(r.trim()); stallRevision = r.trim().slice(0, 10); }).catch(() => {})]);
  // 👹 The stage is cleared, once (2026-11-27): what was built on home, its standing screens and its floor's paint are taken out, each kept as the store keeps what is cleared (the builds among what was removed, the paint and screens under `cleared`), so all of it can be put back. After this the room is built in as before.
  void loaded.then(() => {
    if (store.data.stageCleared || process.env.NODE_TEST_CONTEXT) return;
    return store.clearRoom(STAGE, 'stage').then(() => store.transact((data) => { data.stageCleared = Date.now(); return true; })).then(() => {
      rooms.get(STAGE)?.match.setObjects(store.objects(STAGE));
      builders.broadcast(STAGE);
    });
  }).catch((e) => console.warn('[rax] the stage was not cleared', e?.message));
  // 🏢 The houses beside the tower come down (2026-11-17): a home is a floor
  // of the tower. What stood inside each goes up to its handle's floor; both
  // ledgers are kept as they stood (geography.before-houses.json,
  // objects.before-houses.json). Runs once: after it there are no houses.
  // 📺 The lobby's own screens: three KidLisp pieces running on its walls,
  // facing in (east, west, and north beside the car), so there is always
  // something live to walk up to. Each is four metres wide. They are the
  // building's, not anyone's: told with the room's own screens and never
  // written down (object-store.mjs standing).
  store.standing = { [GROUND]: [['roz', 7.2, 4.6, 90], ['bop', -7.2, 4.6, 270], ['berz', 4.9, 7.7, 0]]
    .map(([code, x, z, yaw]) => ({ id: `lobby-${code}`, owner: null, code, x, z, yaw, at: 0 })) };
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
    }).catch((e) => console.error('rax: ground not cleared:', e?.message || e))
    // 📺 The three screens the lobby was once given as its own (2026-11-19) were written down; they come out. Nobody's: no owner, and the building's ids.
    .then(() => store.transact((data) => {
      const list = data.screens?.[GROUND];
      if (list) data.screens[GROUND] = list.filter((s) => !(s.owner == null && /^lobby-/.test(s.id || '')));
    })).catch((e) => console.error('rax: lobby screens not removed:', e?.message || e));
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
  // 🏠 One room: a place on the lattice has no side that leads on (an empty
  // list, not none: the plaza is still the plaza, drawn with all four walls).
  const neighboursOf = (key) => { const n = geography.geography.neighbours(key); return oneRoom && n ? {} : n; };
  const parkData = key => ({ neighbours: neighboursOf(key), plaza: key === 'lobby', ...(oneRoom ? { one: true } : {}) });
  // What a socket hears of the place (its neighbours), and what the room
  // keeps of it: its solids and the sides that lead on.
  const sendPark = (ws, key) => {
    send(ws, { type: 'park', ...parkData(key) });
    rooms.get(key)?.match.setWalls(solidsFor(key)); // the elevator and the lobby are solid, and a planet hung overhead
    if (rooms.has(key)) rooms.get(key).match.open = aloft(key) ? towerSides : Object.keys(neighboursOf(key) || {});
  };
  // 🏠 Home is the handle's floor of the tower.
  const sendHome = (ws, handle) => {
    const f = tower.tower.forHandle(handle);
    if (f) send(ws, { type: 'home', room: floorRoom(f.id), floor: f.number });
  };
  // 🏢 The tower as this socket sees it: how tall, whose floor this is, and
  // which floor is theirs.
  const hasCar = (key) => key === BASEMENT || key === GROUND || key === ROOF || isFloorRoom(key);
  // (🏠 one room: the car still stands on the plaza, and goes nowhere.)
  // The elevator is solid on every floor; the lobby's glass on the ground; the parapet on the roof.
  const wallsFor = (key) => [...(key === BASEMENT ? cellarFixtures : []), ...(hasCar(key) ? carWalls() : []), ...(key === GROUND ? [...lobbyWalls(), DISPENSER_WALL, atriumRoof()] : key === ROOF ? roofWalls() : isFloorRoom(key) ? storeyWindows() : [])];
  // 🪐 …and the planets hung over the plaza and the blocks (orbits.mjs): what a body there meets, besides the room's own boxes.
  // (🧱 A room that is no place in the world and no game stands on a platform: its block is its one solid, and its only ground.)
  const solidsFor = (key) => [...wallsFor(key), ...(key === GROUND || parseBlock(key) ? hungFor(key === GROUND ? { i: 0, j: 0 } : parseBlock(key)) : []), ...(!worldPlace(key) && !gameFor(key) ? roofBoxes(halfOf(key)) : [])];
  const sendTower = (ws, key, member) => {
    const t = tower.tower, here = t.forRoom(key);
    // Every room hears how tall the tower is: it stands on the skyline.
    // (`listed`: the floors with a storey of their own, so the page counts heights as the server does: elevator.mjs SHORT.)
    if (!hasCar(key)) return void send(ws, { type: 'tower', count: t.count, top: t.top, listed: t.listed(), revision: t.revision, here: null, yours: null, owner: false });
    const mine = member?.handle ? t.forHandle(member.handle) : null;
    // The floors with someone on them now, lowest first: [number, whose, how many].
    const inside = [];
    for (const [id, r] of rooms) {
      const f = t.forRoom(id), people = f ? r.members.filter(Boolean).length : 0;
      if (people) inside.push([f.number, f.handle, people]);
    }
    inside.sort((a, b) => a[0] - b[0]);
    send(ws, { type: 'tower', count: t.count, top: t.top, listed: t.listed(), revision: t.revision, inside: inside.slice(0, 8),
      here: key === BASEMENT ? { room: BASEMENT, number: -1 } : here ? { ...t.public(here), colors: profiles.get(here.handle)?.colors || null }
        : key === ROOF ? { room: ROOF, number: roofNumber(t.top), roof: true } // 🏙️ nobody's: its number is one over the highest floor
        : { room: GROUND, number: 0 },
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
  // 🌧️ The weather over 90026, asked every ten minutes (never under test).
  let weather = { ...CLEAR };
  const askWeather = async () => {
    try {
      const r = await fetch(WEATHER_URL, { signal: AbortSignal.timeout(10000) });
      if (r.ok) weather = readWeather(await r.json());
    } catch {}
  };
  const weatherTimer = process.env.NODE_TEST_CONTEXT ? null : setInterval(askWeather, 10 * 60 * 1000);
  // ✨ The conjurings' sweep (the one room): every second, what has stood its
  // time is taken out, with a puff where each build stood.
  const fadeTimer = oneRoom && !satellite ? setInterval(() => {
    void store.fade(HOME_ROOM).then(({ gone, changed }) => {
      if (!changed) return;
      const room = rooms.get(HOME_ROOM);
      for (const g of gone) room?.match.emit('removed', { at: { x: g.x, y: 1, z: g.z } });
      builders.broadcast(HOME_ROOM);
    }).catch(() => {});
  }, Math.max(50, Math.min(1000, conjureMs / 4))) : null;
  weatherTimer?.unref?.();
  if (weatherTimer) void askWeather();
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
  // The handle an account wears here, or null (none yet, or not one of ours).
  const handleOf = (account) => {
    const h = String(account?.handle || '').replace(/^@/, '');
    return /^[a-z0-9]+([._][a-z0-9]+)*$/i.test(h) && h.length <= 16 ? h : null;
  };
  const pairing = createPairing({ verify: verifyOnce, handleOf, limits: pairLimits });
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
    // AC's own libraries, read-only, so a wall can run real KidLisp; and its disks, so a world panel can run a real piece (ac-piece.mjs).
    if (req.method === 'GET' && /^\/ac\/(lib|dep|disks)\/[\w@./-]+\.(mjs|js)$/.test(path) && !path.includes('..')) {
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
    // 🥽 The room as boxes, for the native headset app (native-scene.mjs): asked for every few seconds, never kept.
    if (req.method === 'GET' && path === '/native-scene.txt') return void res.writeHead(200, { 'Content-Type': 'text/plain; charset=utf-8', 'Cache-Control': 'no-store' }).end(nativeScene());
    if (req.method === 'GET' && path === '/native-live.txt') return void res.writeHead(200, { 'Content-Type': 'text/plain; charset=utf-8', 'Cache-Control': 'no-store' }).end(nativeLive(rooms.get(HOME_ROOM)?.match)); // 🥽 …and who is in it this instant (native-live.mjs), ten times a second
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
    // 🌐 A passage this host gave for a room kept elsewhere, handed to that
    // host once (it asks with the receipt the walker brought). Only passages
    // marked away are ever given out, and only once, within their 30 s.
    if (path === '/passage' && req.method === 'GET') {
      const receipt = url.searchParams.get('receipt') || '', pass = passages.get(receipt);
      if (!pass?.away || pass.until < Date.now()) { res.writeHead(404, { 'Cache-Control': 'no-store' }).end(); return; }
      passages.delete(receipt);
      const { handle, accountId, colors, room: to, position, at, until, ride, heldObject } = pass;
      res.writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' })
        .end(JSON.stringify({ handle, accountId, colors, room: to, position, at, until, ride, heldObject }));
      return;
    }
    if (path === '/stalls' && req.method === 'GET') {
      res
        .writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' })
        .end(JSON.stringify({ building: builders.jobs.size, stalls: stallLog, loop: loopStalls }));
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
    // 🌧️ What is falling over Los Angeles now (weather.mjs).
    if (path === '/weather' && req.method === 'GET') {
      res.writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'public, max-age=60', 'Access-Control-Allow-Origin': '*' }).end(JSON.stringify(weather));
      return;
    }
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
          .end(JSON.stringify(peopleNow({ geography: geography.geography, tower: tower.tower, seated, watching, ball: ball.ball })));
        return;
      }
      res
        .writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store', 'Access-Control-Allow-Origin': '*' })
        .end(JSON.stringify(worldMap({
          geography: geography.geography,
          tower: tower.tower,
          seated,
          watching,
          ball: ball.ball,
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
      const contents = (id, distant = false) => {
        const items = rooms.get(id)?.match.items;
        const objects = store.objects(id).map(o => {
          const pose = o.item && items?.get(o.id);
          const placed = pose ? { ...o, x: pose.x, y: pose.y, z: pose.z } : o;
          if (!distant) return placed;
          return { id: o.id, x: placed.x, y: placed.y || 0, z: placed.z, version: o.version,
            geometry: { name: o.geometry.name, boxes: distantBoxes(o.geometry) }, coarse: true };
        });
        return { objects, screens: store.screens(id) };
      };
      if (onLap(at)) {
        const near = new Set();
        for (let dj = -1; dj <= 1; dj++)
          for (let di = -1; di <= 1; di++) {
            const id = blockId({ i: wrap(at.i + di), j: wrap(at.j + dj) });
            near.add(id);
            blocks.push({ id, di, dj, plaza: id === 'lobby', neighbours: neighboursOf(id),
              ...contents(id), tiles: store.tiles(id) });
          }
        // Beyond the ground tiles only occupied blocks need a record. Send
        // full models several blocks before their distance tier needs them.
        const zoom = url.searchParams.get('detail') === 'zoom' ? MAX_FOCUS_ZOOM : 1;
        const fullRadius = Math.ceil(OBJECT_PROXY_DISTANCE * zoom / SPAN) + 1;
        const occupied = new Set([...Object.keys(store.data.rooms), ...Object.keys(store.data.screens || {}), 'lobby']);
        for (const id of occupied) {
          const cell = parseBlock(id);
          if (near.has(id) || !onLap(cell)) continue;
          const di = wrap(cell.i - at.i), dj = wrap(cell.j - at.j);
          const scenery = contents(id, Math.max(Math.abs(di), Math.abs(dj)) > fullRadius);
          if (scenery.objects.length || scenery.screens.length)
            blocks.push({ id, di, dj, sceneryOnly: true, ...scenery });
        }
      }
      // 🏢 And the floors of the tower near a height asked for (an eye flying
      // up its side, or standing on one): what stands on each, to be drawn
      // where the floor is. Three storeys either way.
      const y = Number(url.searchParams.get('y')), floors = [];
      if (onLap(at) && url.searchParams.has('y') && Number.isFinite(y)) {
        for (const n of floorsNear(y, 3)) { // (a short tower: only the floors with a storey of their own)
          const f = tower.tower.at(n);
          if (!f) continue;
          const id = floorRoom(f.id);
          floors.push({ id, number: n, y: altitude(n), ...contents(id), tiles: store.tiles(id) });
        }
        // 🏙️ And the roof, when it is that near: what its keeper stood on it.
        const top = tower.tower.top;
        if (top && Math.abs(storeyOf(top) + 1 - storeyAt(Math.max(0, y))) <= 3)
          floors.push({ id: ROOF, number: roofNumber(top), y: roofOf(top), ...contents(ROOF), tiles: store.tiles(ROOF) });
      }
      res
        .writeHead(200, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' })
        .end(JSON.stringify({ room: at ? blockId(at) : null, revision: geography.geography.revision, blocks, floors }));
      return;
    }
    if (path === '/room' && req.method === 'GET') {
      await ready;
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
            tower: { top: tower.tower.top, listed: tower.tower.listed() },
            objects: store.objects(id),
            tiles: store.tiles(id),
            screens: store.screens(id),
            chalk: rooms.get(id)?.match.chalk || [],
          }),
        );
      return;
    }
    // 📱 A phone approves a pairing: the code it scanned and its own token
    // (pairing.mjs). Only from our own page; neither is ever logged.
    // (/pair/who: the same token alone, answered with its handle.)
    if ((path === '/pair' || path === '/pair/who') && req.method === 'POST') {
      let foreign = false;
      try { foreign = !!req.headers.origin && new URL(req.headers.origin).host !== req.headers.host; } catch { foreign = true; }
      const body = foreign ? null : await jsonBody(req);
      const { status, ...said } = foreign ? { status: 403, error: 'bad' }
        : path === '/pair' ? await pairing.approve(body?.code, body?.accessToken, ipOf(req))
        : await pairing.who(body?.accessToken, ipOf(req));
      res.writeHead(status, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' }).end(JSON.stringify(said));
      return;
    }
    // (`/~word` is the page itself, asked for a room by name: client.mjs reads the word.)
    const asset = assets.get(/^\/~[a-z0-9:,-]{1,40}$/.test(decodeURIComponent(path)) ? '/' : path); // (a room's word, or a cube's: ~studio, ~1,0,-2, ~2:0,0,0)
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
  // 🔗 The link: hosts that keep neighbouring rooms tell each other who
  // stands in them (tellBeyond), so a seam between hosts is looked across
  // like any other. lith dials; a satellite answers only lith's address,
  // read from Caddy's X-Forwarded-For (it sets that itself; a client's own
  // headers are not believed).
  const linkWss = new WebSocketServer({ noServer: true, maxPayload: 1 << 20, perMessageDeflate: false });
  const links = new Map(); // socket → the host at its other end
  const heard = new Map(); // host → { at, rooms: [{ key, people, eyes }] }
  // ⚽ The balls as home says (a satellite runs no dispenser of its own: one ball, one clock, lith's).
  let farBallsHeard = { at: 0, balls: [] };
  // 🧱 A build asked in a room another host keeps: run here for the asker's
  // own identity at home (the page's home token, sent on by that host), and
  // every word of it said back to their page over the link.
  const buildFor = async (host, m) => {
    const op = m.op || {}, who = m.who || {};
    if (!['prompt', 'restore', 'remove'].includes(op.type) || !Number.isInteger(m.seat)) return;
    const pose = m.pose && ['x', 'y', 'z', 'yaw'].every((k) => Number.isFinite(m.pose[k])) ? { ...m.pose, pitch: m.pose.pitch || 0 } : null;
    let owner;
    try { owner = (await store.identity(typeof who.homeToken === 'string' ? who.homeToken : '')).owner; } catch { return; }
    const page = { readyState: 1, bufferedAmount: 0, send: (text) => sendTo(host, { type: 'to', linkId: m.linkId, text }) };
    const handle = typeof who.handle === 'string' ? who.handle : null, token = typeof who.token === 'string' ? who.token : null;
    if (op.type === 'prompt') {
      const keeping = typeof op.objectId === 'string' && !!handle && handle.toLowerCase() === keeper;
      builders.request(page, m.room, owner, m.seat, op, { guest: !token, keeper: keeping, player: pose, member: { handle, token } });
    } else if (op.type === 'restore' && typeof op.code === 'string' && /^[A-Za-z0-9]{2,16}$/.test(op.code))
      void builders.restore(page, m.room, owner, m.seat, op.code, Builders.rid(op) || undefined, pose);
    else if (op.type === 'remove' && typeof op.objectId === 'string')
      void store.removeObject(m.room, op.objectId, handle || owner).then((gone) => { if (gone) builders.broadcast(m.room); });
  };
  // 🎯 Score a kill made over a seam, if that seat is still the one who fired.
  const creditHere = (key, seat, by) => {
    const there = rooms.get(key), m = there?.members?.[seat];
    if (!there || !Number.isInteger(seat) || !m) return;
    const name = m.handle ? `@${m.handle}` : m.name || null;
    if (by && name !== by) return;
    there.match.credit(seat);
  };
  // Who stands in a room kept elsewhere, as bodies a ball can meet (from the link's people).
  const farBodies = (key) => {
    for (const { at, rooms: far } of heard.values()) {
      if (Date.now() - at > 1500) continue;
      const f = far.find((x) => x.key === key);
      if (f) return f.people.map(([, , seat, x, y, z, yaw, pitch, vx, vz]) => ({ seat: -1 - seat, x, y, z, yaw, pitch, vx, vz, vy: 0, alive: true, onGround: true }));
    }
    return [];
  };
  let linking = true;
  const linkIp = (req) => {
    const peer = (req.socket?.remoteAddress || '').replace(/^::ffff:/, '');
    if (!/^(::1|127\.)/.test(peer)) return peer;
    return String(req.headers['x-forwarded-for'] || '').split(',').pop().trim() || peer;
  };
  const sendLink = (message) => {
    const text = JSON.stringify(message);
    for (const ws of links.keys()) if (ws.readyState === WebSocket.OPEN && ws.bufferedAmount < 1 << 20) ws.send(text);
  };
  const sendTo = (host, message) => {
    const text = JSON.stringify(message);
    for (const [ws, h] of links) if (h === host && ws.readyState === WebSocket.OPEN) ws.send(text);
  };
  const hearLink = (ws, host) => {
    links.set(ws, host);
    // 🧱 A satellite's rooms: what home keeps of them, said as the link opens.
    if (!satellite) for (const [room, h] of Object.entries(roomHosts)) if (h === host) sendTo(host, { type: 'objects', room, objects: store.stored(room) });
    ws.on('message', (raw) => {
      let m;
      try { m = JSON.parse(raw); } catch { return; }
      // Only what that host keeps is believed of it.
      // 🎒 A thing of ours, carried to that host, left lying there: home again here.
      if (m?.type === 'home' && typeof m.id === 'string') {
        if (store.carried.get(m.id)?.room === `~${host}`) { const was = store.uncarry(m.id); builders.broadcast(was.origin); }
        return;
      }
      if (m?.type === 'balls' && Array.isArray(m.balls) && satellite) {
        farBallsHeard = { at: Date.now(), balls: m.balls.slice(0, 16).filter((b) => typeof b?.room === 'string' && ['x', 'y', 'z', 'vx', 'vy', 'vz'].every((k) => Number.isFinite(b[k]))) };
        return;
      }
      if (m?.type === 'credit' && typeof m.room === 'string' && !keptAt(m.room)) { creditHere(m.room, m.seat, m.by); return; }
      // 🧱 Building in a room another host keeps, done here (home keeps the store).
      if (m?.type === 'build' && !satellite && typeof m.room === 'string' && keptAt(m.room) === host) { void buildFor(host, m); return; }
      if (satellite && m?.type === 'objects' && typeof m.room === 'string' && !keptAt(m.room)) { store.setMirror(m.room, m.objects); builders.broadcast(m.room); return; }
      if (satellite && m?.type === 'jobs' && typeof m.room === 'string' && rooms.has(m.room)) {
        const text = JSON.stringify({ type: 'jobs', jobs: Array.isArray(m.jobs) ? m.jobs : [] });
        for (const ws of rooms.get(m.room).clients.keys()) sendText(ws, text);
        return;
      }
      if (satellite && m?.type === 'to' && typeof m.linkId === 'string' && typeof m.text === 'string') {
        for (const room of rooms.values()) for (const [ws, member] of room.sessions) if (member.linkId === m.linkId && ws.readyState === WebSocket.OPEN) { ws.send(m.text); return; }
        return;
      }
      // 🌐 A round in over a seam to a room this host keeps.
      if (m?.type === 'round' && typeof m.room === 'string' && !keptAt(m.room)) { rooms.get(m.room)?.match.takeRound(m); return; }
      if (m?.type === 'stood' && Array.isArray(m.rooms))
        heard.set(host, { at: Date.now(), rooms: m.rooms.filter((r) => typeof r?.key === 'string' && keptAt(r.key) === host &&
          Array.isArray(r.people) && Array.isArray(r.eyes)).slice(0, 64).map((r) => ({ key: r.key, people: r.people.slice(0, 64), eyes: r.eyes.slice(0, 64),
            shots: Array.isArray(r.shots) ? r.shots.slice(0, 32).filter((b) => Array.isArray(b) && b.length === 7 && b.every(Number.isFinite)) : [] })) });
    });
    ws.on('close', () => { if (links.get(ws) === host) links.delete(ws); heard.delete(host); });
    ws.on('error', () => {});
  };
  const dial = (host) => {
    if (!linking) return;
    let ws;
    try { ws = new WebSocket(linkURL(host)); } catch { setTimeout(() => dial(host), 3000).unref?.(); return; }
    ws.on('open', () => hearLink(ws, host));
    ws.on('close', () => { if (linking) setTimeout(() => dial(host), 3000).unref?.(); });
    ws.on('error', () => {});
  };
  if (!satellite) for (const host of new Set(Object.values(roomHosts))) if (host && host !== selfHost) dial(host);
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
    if (url.pathname === `${prefix}/link`) {
      if (!satellite || !linkFrom.includes(linkIp(req))) {
        socket.write('HTTP/1.1 403 Forbidden\r\nConnection: close\r\n\r\n');
        socket.destroy();
        return;
      }
      linkWss.handleUpgrade(req, socket, head, (ws) => hearLink(ws, homeHost));
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
      (origin && origin.host !== req.headers.host && !origins.includes(origin.host))
    ) {
      socket.destroy();
      return;
    }
    wss.handleUpgrade(req, socket, head, (ws) =>
      wss.emit('connection', Object.assign(ws, { ip: ipOf(req) }), url, req.headers.authorization),
    );
  };
  const detach = [];
  const attach = (host) =>
    detach.push(
      attachSocketRoute(
        host,
        [prefix, ...(alias ? [alias] : [])].flatMap((p) => [`${p}/play`, `${p}/builder`, `${p}/link`]),
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
    if (data.type === 'role' && data.seat < 0) notepat.release(ws);
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
    ...(key === GROUND ? [clear(-DOOR, DOOR, LOBBY_FRONT - 2, LOBBY_FRONT + 2), clear(-1.2, 1.2, -halfOf(key), CAR_Z - CAR)] : []),
  ];
  const builders = new Builders({ token: builderToken, store, rooms, send, sendText, publish: publishKidlisp, diagnostics, solids: keepClear });
  // 🌐 A room kept on another host is told there of what is built in it (objects) and the line of builds (jobs).
  builders.onRoom = (room, kind) => {
    const host = keptAt(room);
    if (satellite || !host) return;
    if (kind === 'state') sendTo(host, { type: 'objects', room, objects: store.stored(room) });
    else sendTo(host, { type: 'jobs', room, jobs: builders.jobRows(room) });
  };
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
    // (🏠 one room: whatever place in the world was asked for, it is the one room.)
    const roomId = placeFor(url.searchParams.get('room') || (oneRoom ? HOME_ROOM : 'lobby'), oneRoom);
    if (
      !/^[a-z0-9-]{1,32}$/.test(roomId) ||
      !PROTOCOLS.includes(url.searchParams.get('protocol'))
    ) {
      ws.close(1008, 'Invalid room or build');
      return;
    }
    if (satellite && !onlyRooms.includes(roomId)) {
      ws.close(1008, 'Not kept here');
      return;
    }
    // 🧵 The place this socket stands in. It changes when the body walks over a
    // line or rides the car on a socket that stays (`stay=1`, relocate below).
    let key = roomId;
    if ((key.startsWith('pod-') && !geography.geography.pod(key)) ||
        (key.startsWith('floor-') && !tower.tower.forRoom(key)) ||
        (key === ROOF && !tower.tower.top) || // (the roof lies on the highest floor: no floors, no roof)
        (parseBlock(key) && !onLap(parseBlock(key)))) {
      ws.close(1008, 'Place is not open'); return;
    }
    // The room of a place, made if nobody is in it yet; null when the server is full.
    const openRoom = (key) => {
      if (rooms.has(key)) return rooms.get(key);
      if (rooms.size >= 96) return null;
      rooms.set(key, new Room(send, { guests, connections }));
      rooms.get(key).key = key;
      { const made = rooms.get(key); made.match.nameOf = (seat) => { const m = made.members?.[seat]; return m ? (m.handle ? `@${m.handle}` : m.name || null) : null; }; }
      rooms.get(key).match.half = halfOf(key);
      rooms.get(key).match.waves = key === STAGE; // 👹 heads fly in at the stage (match.mjs stepHeads)
      rooms.get(key).match.bellMs = key === STAGE ? BELL_MS : 0; // 🕰️ …each wave as the clock tower strikes
      if (flights.has(key)) {
        rooms.get(key).match.setObjects(store.objects(key));
        rooms.get(key).match.resume(flights.get(key));
        flights.delete(key);
      }
      // 🛗 In a room with the car, bodies land in a row south of the middle,
      // facing the car: never inside it, never against a planter.
      if (key === BASEMENT) rooms.get(key).match.spawnAt = seat => ({ x: seat % 4 * 1.2, z: -5 - Math.floor(seat / 4)*0.65, yaw: 0 });
      else if (hasCar(key)) rooms.get(key).match.spawnAt = (seat) => seat === 0 ? { x: 0, z: -5, yaw: 0 }
        : { x: (seat % 2 ? -1 : 1) * 1.5 * Math.ceil((seat % 8 || 8) / 2), z: seat < 9 ? -6.3 : -7.2, yaw: 0 };
      if (isArena(key)) rooms.get(key).match.setArena(LEDGES);
      return rooms.get(key);
    };
    let room = openRoom(key);
    if (!room) {
      ws.close(1013, 'Server full');
      return;
    }
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
    // 🚪 wait=1 stands at the door: a walker near a seam opens the next
    // room ahead of the step and says who they are, and is seated only when
    // they hand in the passage (an `arrive` message). So the step over the
    // line costs a word each way, not a new connection.
    let waiting = url.searchParams.get('wait') === '1';
    // 🧵 stay=1: this page keeps its one socket from room to room (relocate).
    const stays = url.searchParams.get('stay') === '1';
    let passageAsked = url.searchParams.get('passage');
    const member = room.add(ws, nativeAccount || webAccount || watching || waiting);
    // 🚁 drone=1: this screen looks on by choice (it left as a drone and comes back as one). It is given no body until it asks (`take-body`), even when it is the handle's only screen.
    member.drone = url.searchParams.get('drone') === '1';
    // Nobody with a handle is here: say what the seat waits for, so the
    // page can ask for it (an account, or a handle on the account).
    const gate = () => {
      if (guests || watching || waiting || member.handle || !room.sessions.has(ws)) return;
      send(ws, { type: 'gate', needs: member.signedIn ? 'handle' : 'account' });
    };
    const seatGuest = () => {
      clearTimeout(accountHold);
      if (room.sessions.has(ws) && !member.ready && !watching && !waiting) {
        member.ready = true;
        room.assign(member.handle);
      }
      gate();
    };
    const accountHold = webAccount ? setTimeout(seatGuest, 6000) : null;
    accountHold?.unref?.();
    // What stands in the room is solid for whoever comes into it.
    const furnish = () => {
      const at = key, here = room;
      here.match.setObjects(store.objects(at));
      here.match.setWalls(solidsFor(at));
      // An old build in the way of the car or the entrance is stood aside.
      if (hasCar(at)) void builders.tidy(at).then((moved) => {
        if (!moved) return;
        here.match.setObjects(store.objects(at));
        builders.broadcast(at);
      }).catch(() => {});
    };
    furnish();
    let arrived = false, leaving = null; // leaving: the passage this socket was last given
    // 👀 Told its place is another room, and not yet walked a step in this
    // one: a look in on the way, which leaves the avatar's rest where it was.
    let passing = false;
    // 🎟️ A passage is the visitor themself, known a moment ago in the room
    // they left: it seats them where it leads as soon as the page says whose
    // it is, with the handle they wore, and the account is checked after.
    const arrive = () => {
      if (arrived || watching || waiting) return;
      const receipt = passageAsked, pass = passages.get(receipt);
      // 🌐 A passage from another host: fetched from it once (its name is in the receipt), then this runs again.
      if (!pass && receipt && receipt.includes('|') && !member.passageFetch) {
        const host = receipt.split('|')[0];
        if (peers.has(host)) member.passageFetch = fetchPassage(host, receipt).then((got) => {
          if (!got || got.room !== key || !(got.until > Date.now())) return;
          passages.set(receipt, { ...got, owner: null, away: false, fetched: true, from: host });
          if (ws.readyState === WebSocket.OPEN && room.sessions.has(ws)) arrive();
        }).catch(() => {});
        return;
      }
      // Whose it is: the page's owner, or (the account answered first) the
      // account the walker wore. Else the body stood two ticks at the
      // room's spawn before it was set down (seen 2026-11-19).
      // (From another host the page's owner is not known there: only the verified account can claim it.)
      const mine = pass && (pass.fetched ? !!member.accountId && pass.accountId === member.accountId
        : member.owner ? pass.owner === member.owner : !!member.accountId && pass.accountId === member.accountId);
      if (!mine || pass.room !== key || pass.until < Date.now()) return;
      passages.delete(receipt);
      for (const k of ['handle', 'accountId', 'colors', 'token']) if (pass[k] && !member[k]) member[k] = pass[k];
      member.owner ||= pass.owner; // the page's own word may still be on its way
      // (Looking on until now, with no account of the page's own: a screen a
      // phone signed in. The handle it brought seats it.)
      if (!member.ready || member.seat < 0) { clearTimeout(accountHold); member.ready = true; room.assign(member.handle); }
      const body = room.match.seats[member.seat]?.player;
      if (!body) return;
      arrived = true;
      passing = false;
      Object.assign(body, pass.position);
      // 🛗 Come by the car: its door is shut for what is left of the ride (world.mjs `shut`).
      if (pass.ride) {
        member.ride = pass.ride;
        body.shut = Math.max(0, shutTicks(pass.ride.seconds, HZ) - Math.round(((Date.now() - pass.at) * HZ) / 1000));
      }
      // The time on the way was lived: the commands given in it are owed a run.
      room.match.owe(member.seat, Math.round(((Date.now() - pass.at) * HZ) / 1000));
      // 🎒 What was in hand on the way over is in hand here (object-store.mjs carry; carryIn from another host).
      const moved = pass.heldObject && pass.from ? store.carryIn(pass.heldObject, key, { x: body.x, z: body.z }, `~${pass.from}`) && { from: null }
        : pass.held && store.carry(pass.held, key, { x: body.x, z: body.z });
      if (pass.heldObject && moved) pass.held = pass.heldObject.id;
      if (moved) {
        if (moved.from) builders.broadcast(moved.from);
        builders.broadcast(key);
        const it = room.match.items.get(pass.held);
        if (it) Object.assign(it, { holder: member.seat, thrownBy: -1, lyingSince: 0 });
      }
      sendTower(ws, key, member); // whose floor this is, known with the passage
      send(ws, { type: 'arrival', yaw: pass.position.yaw, pitch: pass.position.pitch });
      send(ws, room.match.snapshot());
      bringWatchers();
    };
    // 🪞 The same handle looking on from another screen, and following (the
    // page's `follow`): where the body goes, that drone is brought.
    const bringWatchers = () => {
      const body = room.match.seats[member.seat]?.player, person = member.handle?.toLowerCase();
      if (!body || !person) return;
      for (const r of [...connections])
        if (r !== member && r.following && r.ready && r.seat < 0 && r.room !== room && r.handle?.toLowerCase() === person)
          r.followBody?.(key, bodyPose(body));
    };
    // 👥 Leaving with a hand still on a kiosk: the room is told it was let go.
    const pressing = new Map();
    const letGo = () => {
      notepat.leave(ws);
      for (const [finger, id] of pressing) {
        const word = JSON.stringify({ type: 'panel', id, seat: member.seat, finger, name: 'lift', x: -1, y: -1 });
        kiosks.record(key, id, 'lift', -1, -1, member.seat, finger);
        for (const client of room.clients.keys()) if (client !== ws) sendText(client, word);
      }
      pressing.clear();
    };
    // 🧵 The body goes to another room and the socket stays. Nothing is
    // closed or opened and nobody is asked again who they are: the same
    // member leaves this room's match and takes a seat in the next, set down
    // where the line or the car left them, and the next room's words follow
    // the `travel` on the same wire, in order. → whether it was done (a page
    // that does not stay, or a room with no place left, takes a passage).
    const relocate = (destination, position, { ride = null, told = {} } = {}) => {
      if (!stays || destination === key || keptAt(destination)) return false; // (a room kept elsewhere: a passage, not a move)
      const next = openRoom(destination);
      if (!next || next.clients.size >= ROOM_CONNECTIONS) return false;
      const from = room, fromKey = key;
      // 🎒 What is in hand goes over with the hand (object-store.mjs carry).
      const heldId = member.seat >= 0 ? from.match.held(member.seat)?.id : null;
      send(ws, { type: 'travel', room: destination, here: true, at: position, ...told });
      letGo();
      from.remove(ws, true);
      if (!from.clients.size) {
        const kept = from.match.flight();
        if (kept) flights.set(fromKey, kept);
        rooms.delete(fromKey);
      }
      key = destination;
      room = next;
      room.adopt(member);
      furnish();
      arrived = true;
      passing = false;
      leaving = null;
      member.walk = null;
      const body = room.match.seats[member.seat]?.player;
      if (body) {
        Object.assign(body, position);
        // 🛗 Come by the car: its door is shut for the ride (world.mjs `shut`).
        if (ride) { member.ride = ride; body.shut = shutTicks(ride.seconds, HZ); }
        // The commands given while the word was on its way are owed a run.
        room.match.owe(member.seat, HZ / 2);
        if (heldId && store.carry(heldId, destination, { x: body.x, z: body.z })) {
          builders.broadcast(fromKey);
          builders.broadcast(destination);
          const it = room.match.items.get(heldId);
          if (it) Object.assign(it, { holder: member.seat, thrownBy: -1, lyingSince: 0 });
        }
      }
      send(ws, { type: 'role', seat: member.seat, role: member.seat < 0 ? 'spectator' : 'player' });
      if (devRoom(key) && devRecent.length) send(ws, { type: 'dev-log', entries: devRecent });
      send(ws, { type: 'chalk', strokes: room.match.chalk });
    send(ws, { type: 'panels', kept: kiosks.of(key) }); // 🧩 where the room's pieces stand
      sendPark(ws, key);
      sendTower(ws, key, member);
      send(ws, { type: 'arrival', yaw: position.yaw, pitch: position.pitch });
      send(ws, room.match.snapshot());
      send(ws, builders.state(key, member, ws));
      if (member.seat >= 0) bringWatchers();
      return true;
    };
    member.followBody = (destination, position) => relocate(destination, position);
    // 🏙️ Instagibbed: come back on the tower's roof, at its south edge, looking out over the city.
    member.toRoof = () => relocate(ROOF, { x: 0, y: CFG.eyeHeight, z: -(PARAPET.at - 1.4), yaw: 180, pitch: 0,
      vx: 0, vy: 0, vz: 0, onGround: true, alive: true });
    member.crossWindow = () => {
      const seat = room.match.seats[member.seat], p = seat?.player;
      if (!stays || !p?.alive || p.shut > 0 || Math.max(Math.abs(p.x), Math.abs(p.z)) <= SLAB + 0.4) return;
      const {block, local} = fromWorld(p);
      const height = cellOf(key).h;
      relocate(blockId(block), { ...local, y: height + p.y, yaw: p.yaw, pitch: p.pitch,
        vx: p.vx, vy: p.vy, vz: p.vz, onGround: false, bike: !!p.bike }, { told: {ack: seat.ack} });
    };
    member.crossStairs = () => {
      if (!stays || !member.owner || member.seat < 0 || (key !== GROUND && key !== BASEMENT)) return;
      const seat = room.match.seats[member.seat], p = seat?.player;
      const pass = stairPassage(key, p);
      if (pass) relocate(pass.room, { x: p.x, y: pass.y, z: p.z, yaw: p.yaw, pitch: p.pitch,
        vx: p.vx, vy: p.vy, vz: p.vz, onGround: p.onGround }, { told: { ack: seat.ack } });
    };
    // 🪪 This socket is now this verified account, whichever way it was
    // verified: by the page's own token (`account`), or by a phone's
    // (pairing.mjs). → whether the socket took a handle.
    // Paired, no token is kept (the phone's stays the phone's: the screen
    // wears the handle and builds on a guest's allowance), and the body
    // stands up here: going to where the handle last stood would open a new
    // socket, and the pairing is this socket's alone.
    const signIn = (account, token, paired = false) => {
      member.signedIn = true; // An account, with or without a handle yet.
      const handle = handleOf(account);
      if (!handle) return false;
      if (ws.readyState !== WebSocket.OPEN || !room.sessions.has(ws)) return false;
      // 🗺️ A handle has one home, settled the first time it arrives, and
      // its avatar wakes where it rested if that was this room. A reload
      // that takes over a body already here leaves the body where it is.
      const person = handle.toLowerCase();
      const incumbent = room.counterpart(person, member, true);
      const takeover = !!incumbent;
      member.handle = handle;
      member.accountId = account.sub;
      // 🏢 Its floor of the tower is its home, held for good from now on.
      // (Homes on the park's lots stay where they stand; none are added.)
      void tower.change((t) => t.arrive(handle, account.sub)).then(() => {
        sendHome(ws, handle);
        sendTower(ws, key, member);
        if (paired || takeover || arrived || waiting) return;
        // 🌍 One persistent world: say where this handle last stood, so a
        // visit that has not gone anywhere yet picks up there. A first
        // visit starts on the plaza, at the foot of the tower.
        const last = geography.geography.whereabouts(handle)?.room;
        const open = last && (last === BASEMENT || last === 'lobby' || tower.tower.forRoom(last) || (last === ROOF && !!tower.tower.top) ||
          onLap(parseBlock(last)));
        const place = oneRoom ? HOME_ROOM : open ? last : 'lobby';
        passing = place !== key && !arrived;
        send(ws, { type: 'whereabouts', room: place });
      }).catch(() => {});
      if (!waiting && !watching) member.ready = true;
      // The $ history is the handle's, ready before the menu opens.
      member.wantsHistory = false;
      void sendHandleHistory(ws, handle);
      // Kept in memory only, to read this player's braincells.
      member.token = paired ? null : token;
      void refreshBraincells(ws, member);
      void deaths?.read(handle).then((total) => { if (total !== null) send(ws, { type: 'deaths', deaths: total }); });
      // (🪞 A newer screen comes to where the handle's body stands. Not out of a
      // room asked for by name or a game's arena, though: nobody arrives there
      // except by asking, so the body comes to it instead — assign() below
      // seats this newest screen, and the older one looks on.)
      if (incumbent && room !== incumbent.room && !passageAsked && worldPlace(key) && room.counterpart(person) === member) {
        const body = incumbent.room.match.seats[incumbent.seat]?.player;
        if (body) relocate(incumbent.room.key, bodyPose(body));
      }
      // 🧍 A screen that comes as a drone, with no body of its handle anywhere, has one stood here idle: the body is always in the world while its person is.
      if (member.drone && !room.counterpart(person, member, true) && ![...rooms.values()].some((r) => r.parked.has(person))) room.stand(member);
      room.assign(member.handle);
      const rest = geography.geography.whereabouts(handle),
        body = room.match.seats[member.seat]?.player;
      if (!arrived && !takeover && body && rest?.room === key) {
        const edge = BLOCK / 2 - 0.4; // inside the walls
        body.x = Math.max(-edge, Math.min(edge, rest.x));
        body.z = Math.max(-edge, Math.min(edge, rest.z));
        body.yaw = rest.yaw;
        if (key === BASEMENT && body.x >= STAIR.left && body.x <= STAIR.right && body.z >= STAIR.landing && body.z <= STAIR.end)
          body.y += stairHeight(body.z);
      }
      arrive();
      void colorsFor(handle).promise.then((colors) => {
        if (ws.readyState !== WebSocket.OPEN || !room.sessions.has(ws)) return;
        member.colors = colors;
        room.broadcast();
      });
      return true;
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
        signIn(await verifyOnce(token), token);
      } catch {
        /* Unverified sessions remain guests. */
      } finally {
        if (room.sessions.has(ws) && !member.ready && !waiting && !watching) {
          member.ready = true;
          room.assign(member.handle);
        }
      }
    };
    send(ws, {
      type: 'welcome',
      eye: (member.eye ||= randomUUID().slice(0, 8)), // 🚁 its drone's name among the rest, to leave its own undrawn
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
      send(ws, { type: 'panels', kept: kiosks.of(key) }); // 🧩 where the room's pieces stand
    sendPark(ws, key);
    sendTower(ws, key, member);
    send(ws, room.match.snapshot());
    send(ws, builders.state(key, member, ws));
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
      // 🧵 Said in a room this socket has since left (the word crossed the move on the wire): not this room's to hear.
      if (typeof data?.in === 'string' && data.in !== key) return;
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
          'rttMs', 'jitterMs', 'queuedBytes', 'objects', 'triangles', 'backend', 'gyroState', 'gyroSamples', 'gyroValid', 'gyroTurns']) fields[k] = data[k];
        diagnostics.emit('client.sample', { ...fields, session: ws.diagnosticSession,
          buildIds: Array.isArray(data.buildIds) ? data.buildIds.slice(0, 12).filter((id) =>
            ws.diagnosticJobs.has(id) && Date.now() - ws.diagnosticJobs.get(id) <= 300000) : [] });
        return;
      }
      // 🪞 A drone that follows its own body (or has been taken back by hand).
      if (data?.type === 'follow') {
        member.following = data.on === true;
        if (!member.following || !member.ready || member.seat >= 0 || !member.handle) return;
        const other = room.counterpart(member.handle, member, true);
        const body = other && other.room !== room && other.room.match.seats[other.seat]?.player;
        if (body) relocate(other.room.key, bodyPose(body));
        return;
      }
      // 🚁 A body's own screen steps back to look on: the body goes to the handle's next screen, and if there is none it stays where it stands, idle (room.mjs `park`). The screen stays a drone until it asks for the body.
      if (data?.type === 'take-drone') {
        if (!member.ready || !member.handle || member.seat < 0) return;
        member.drone = true;
        const heir = room.counterpart(member.handle, member);
        if (!heir || heir.drone) room.park(member);
        room.assign(member.handle);
        return;
      }
      if (data?.type === 'take-body') {
        if (!member.ready || watching || waiting || member.seat >= 0 || !member.handle) return;
        const chose = member.drone;
        member.drone = false;
        const other = room.counterpart(member.handle, member, true);
        if (!other) {
          // (No screen has the body: one that had chosen to look on takes up the body standing idle here, or is seated afresh. A body left idle in another room is let go.)
          if (chose) for (const [k, r] of rooms) if (r !== room && r.unpark(member.handle.toLowerCase())) r.assign();
          if (chose) { connections.delete(member); connections.add(member); room.assign(member.handle); send(ws, room.match.snapshot()); }
          return;
        }
        const position = bodyPose(other.room.match.seats[other.seat].player);
        // Move first as a spectator. A full destination leaves the incumbent in control.
        if (other.room !== room && !relocate(other.room.key, position)) return;
        connections.delete(member);
        connections.add(member);
        room.assign(member.handle);
        send(ws, room.match.snapshot());
        return;
      }
      if (data?.type === 'identity' && !owner && !identifying) {
        identifying = true;
        if (typeof data.token === 'string') member.homeToken = data.token; // (🌐 the page's identity at home, for building from a satellite)
        member.linkId ||= randomUUID();
        store
          .identity(data.token)
          .then((identity) => {
            owner = identity.owner;
            if (ws.readyState === WebSocket.OPEN && room.sessions.has(ws)) {
              member.owner = owner;
              room.assign(member.handle);
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
      // 🧩 The page keeps shapes it was sent (builders.shapes); one it lost asks for the room whole again.
      if (data?.type === 'shapes-kept' || data?.type === 'shapes-lost') {
        ws.keptShapes = new Set();
        if (data.type === 'shapes-lost') send(ws, builders.state(key, member, ws));
      }
      if (data?.type === 'cancel-build' && owner && member.seat >= 0)
        builders.cancel(owner, key);
      if (data?.type === 'account') void identifyAccount(data.accessToken).finally(seatGuest);
      if (data?.type === 'account-none') seatGuest();
      // 📱 Looking on with nobody signed in: a code a phone can sign this
      // socket in with (pairing.mjs). A reload is a new socket: scan again.
      if (data?.type === 'pair') {
        if (guests || watching || waiting || member.handle) return;
        const approve = (account) => {
          try { return !member.handle && signIn(account, null, true); } catch { return false; }
        };
        void pairing.create(ws, ws.ip, approve).then((made) => send(ws, { type: 'pair', ...made }));
        return;
      }
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
      // 🚪 The one at the door steps in with their passage. Not yet known
      // (their own word still on its way)? Then when it is; and if the
      // passage is no good, they are seated as anyone arriving would be.
      if (data?.type === 'arrive' && waiting) {
        waiting = false;
        passageAsked = String(data.passage || '');
        arrive();
        if (!arrived) setTimeout(() => {
          if (arrived || member.ready || !room.sessions.has(ws)) return;
          member.ready = true;
          room.assign(member.handle);
          gate();
        }, 1500).unref?.();
        return;
      }
      // 👥 A press on a world panel (a kiosk: panel.mjs) is told to everyone else in the room, with whose it is, so the piece on it is worked by all of them together.
      // ✋ Hands in a headset, told to everyone else in the room as they are (immersive.mjs handsPacked): as often as the headset draws, a hundred and twenty times a second at the most from one screen.
      if (data?.type === 'hands') {
        const at = Date.now();
        if (at - (member.handsAt || 0) < 8 || !handsValid(data.hands) || !headValid(data.head)) return;
        member.handsAt = at;
        const word = JSON.stringify({ type: 'hands', id: (member.eye ||= randomUUID().slice(0, 8)), seat: member.seat, hands: data.hands, ...(data.head ? { head: data.head } : {}),
          // 🧍 (The seat it rides in: its own, or its handle's body on another screen. Nobody rides in a stranger.)
          ...(Number.isInteger(data.ride) && data.ride >= 0 && data.ride < MAX_PLAYERS && (data.ride === member.seat || (member.handle && room.members[data.ride]?.handle?.toLowerCase() === member.handle.toLowerCase())) ? { ride: data.ride } : {}) });
        for (const other of room.clients.keys()) if (other !== ws) sendText(other, word);
        return;
      }
      if (data?.type === 'notepat-watch' || data?.type === 'notepat-key') {
        if (key !== 'lobby') return;
        if (data.type === 'notepat-watch') notepat.watch(ws);
        else if (member.seat >= 0) notepat.key(ws, data);
        return;
      }
      if (data?.type === 'panel') {
        // (Only a kiosk that stands in this room: kiosks-plan.mjs. Any other id is dropped.)
        if (member.seat < 0 || typeof data.id !== 'string' || !kioskPlan(key).some((k) => k.id === data.id) || !['touch', 'draw', 'lift', 'reset'].includes(data.name) || !Number.isFinite(data.x) || !Number.isFinite(data.y)) return;
        // ↻ The kiosk's name was pressed: its piece begins again, for everyone here and whoever comes later.
        if (data.name === 'reset') {
          if (data.id === 'yard-notepat') {
            if (!notepat.reset()) return;
          } else kiosks.reset(key, data.id);
          const again = JSON.stringify({ type: 'panel', id: data.id, seat: member.seat, name: 'reset', x: -1, y: -1 });
          for (const client of room.clients.keys()) if (client !== ws) sendText(client, again);
          return;
        }
        const finger = data.finger ?? 1;
        if (!Number.isInteger(finger) || finger < 1 || finger > 16) return;
        if (data.name === 'touch') {
          if (pressing.has(finger)) return;
          pressing.set(finger, data.id);
        } else if (pressing.get(finger) !== data.id) return;
        if (data.name === 'lift') pressing.delete(finger);
        const px = clamp(Math.round(data.x), -1, 1024), py = clamp(Math.round(data.y), -1, 1024);
        if (data.id === 'yard-notepat') notepat.pointer(ws, data.name, finger, px, py);
        kiosks.record(key, data.id, data.name, px, py, member.seat, finger); // 🧩 kept, so the piece is one piece for whoever comes later (kiosks.mjs)
        const word = JSON.stringify({ type: 'panel', id: data.id, seat: member.seat, finger, name: data.name, x: px, y: py });
        for (const client of room.clients.keys()) if (client !== ws) sendText(client, word);
        return;
      }
      // 🫧 The page's own count of a walk over a seam: from the old room's
      // word to its first place in this one.
      if (data?.type === 'seam' && member.seat >= 0) {
        const ms = Number(data.ms);
        if (Number.isFinite(ms) && ms >= 0 && ms < 60000 && /^[a-z0-9-]{1,32}$/.test(String(data.from || '')))
          walks.seam(member, { from: data.from, ms: Math.round(ms) });
        return;
      }
      if (data?.type === 'walk' && member.owner && member.seat >= 0) {
        const p = room.match.seats[member.seat]?.player;
        if (!p) return;
        let destination, position, walked;
        if (data.type === 'walk') {
          const edge = edgeAt(p);
          destination = neighboursOf(key)?.[edge];
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
          if (inWall(spot)) spot = landing({ ...p, x: clamp(p.x, -halfOf(key) + 0.4, halfOf(key) - 0.4), z: clamp(p.z, -halfOf(key) + 0.4, halfOf(key) - 0.4) }, edge);
          position = { ...spot, y: p.y, vx: p.vx, vy: p.vy, vz: p.vz, onGround: p.onGround, bike: !!p.bike };
          walked = room.match.seats[member.seat].ack;
        }
        // 🧵 On a socket that stays, the body is in the next room now. `ack` is the last command this room ran.
        if (relocate(destination, position, { told: walked === undefined ? {} : { ack: walked } })) return;
        // Asked again before the answer got there (a slow link): the same
        // passage again, never a new one that would void the first.
        if (leaving && leaving.until > Date.now()) return void send(ws, leaving.message);
        for (const [id, pass] of passages) if (pass.until < Date.now() || pass.owner === member.owner) passages.delete(id);
        if (passages.size >= 1024) return;
        // 🌐 Into a room kept elsewhere: the receipt names this host, and the
        // other host fetches the passage from here (GET /passage), once.
        const away = keptAt(destination);
        const receipt = away ? `${selfHost}|${randomUUID()}` : randomUUID();
        // 🎒 What is in hand goes with the passage to the other host; here it is put away meanwhile.
        const inHand = room.match.held(member.seat), heldObject = away && inHand ? store.objects(key).find((o) => o.id === inHand.id) || null : null;
        if (heldObject && store.carry(heldObject.id, `~${away}`, { x: p.x, z: p.z })) builders.broadcast(key);
        passages.set(receipt, { owner: member.owner, ...carry(member), held: away ? null : inHand?.id || null, heldObject, room: destination, position, at: Date.now(), until: Date.now() + 30000, away: !!away });
        // `ack` is the last command this room ran: the walker's later ones go with them.
        leaving = { until: Date.now() + 10000, message: { type: 'travel', room: destination, passage: receipt, at: position, ...(walked === undefined ? {} : { ack: walked }) } };
        send(ws, leaving.message);
        return;
      }
      // 🛗 The elevator: from the car, to a floor by number or handle, to
      // the ground, or one floor up or down.
      if (data?.type === 'elevator' && !oneRoom && member.owner && member.seat >= 0 && hasCar(key)) {
        const p = room.match.seats[member.seat]?.player, t = tower.tower;
        // Asked from outside the car: say so, rather than nothing.
        if (!inCar(p)) return void send(ws, { type: 'elevator-result', error: 'step into the car' });
        // 🛗 Where to, for what was asked (tower.mjs carTo): a floor, the ground, or the roof.
        const went = carTo(t, key, data.to, member.handle);
        if (went.error) return void send(ws, { type: 'elevator-result', error: went.error });
        const destination = went.room;
        // 🛗 A rider stays where they stand in the car, looking where they look: the car moves, they do not. (No facing is given, so the page keeps its own.)
        const stood = { x: p.x, z: p.z };
        // 🚪 How long the car runs, as the page counts it (asked while it ran, it starts from where it is): the door is shut for that long, and the rider walks about inside.
        const was = member.ride, since = was ? (Date.now() - was.at) / 1000 : Infinity;
        const running = was && since < was.seconds ? was.span * (1 - rideEase(since / was.seconds)) : 0;
        const span = altitude(went.from) - altitude(went.number) + running; // (the roof's number is its height too: elevator.mjs roofNumber)
        const run = { at: Date.now(), span, seconds: rideSeconds(span) };
        const riding = { from: went.from, to: went.number, handle: went.handle };
        // 🧵 On a socket that stays, the rider is on the next floor now (and a ball in the car with them).
        const carried = dispenser.balls.map(s => s.ball).filter(b => b.room === key && inCar(b));
        const moveCargo = () => { for (const b of carried) { b.room = destination; carryBall(b, span, run.seconds); } if (carried.length) keepBall(); };
        if (relocate(destination, stood, { ride: run, told: { ride: riding } })) {
          moveCargo();
          return;
        }
        // Asked again before the answer got there (a slow link): the same
        // passage again, never a new one that would void the first.
        if (leaving && leaving.until > Date.now()) return void send(ws, leaving.message);
        for (const [id, pass] of passages) if (pass.until < Date.now() || pass.owner === member.owner) passages.delete(id);
        if (passages.size >= 1024) return;
        const receipt = randomUUID();
        passages.set(receipt, { owner: member.owner, ...carry(member), held: room.match.held(member.seat)?.id || null, room: destination, position: stood, ride: run, at: Date.now(), until: Date.now() + 30000 });
        // ⚽ A ball in the car rides with it.
        moveCargo();
        leaving = { until: Date.now() + 10000, message: { type: 'travel', room: destination, passage: receipt, at: { ...stood },
          ride: riding } };
        send(ws, leaving.message);
        return;
      }
      // Ask for a floor number, the way a name is registered.
      if (data?.type === 'take-floor' && !oneRoom) {
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
      if (!openBuild && pod && !ownPod && !keeping && ['prompt', 'clear-room', 'remove', 'wall-res', 'kidlisp-mod', 'screen', 'paint', 'restore', 'chalk-clear', 'chalk-dust'].includes(data?.type)) {
        send(ws, { type: 'build-result', rid: Builders.rid(data) || undefined, error: floor ? `Only @${floor.handle} can arrange this floor` : 'Only the pod owner can arrange this pod' }); // (`rid`: the page's own name for what it asked, said back so it knows which asking was refused)
        if (data.type === 'paint') builders.broadcast(key); // the page drew the square already: take it back
        return;
      }
      // 🛰️ A satellite keeps no store: a build, a $code called in or a thing
      // taken out goes home over the link, for the page's home identity
      // (server.mjs buildFor); the rest of building waits for later.
      if (satellite && ['prompt', 'remove', 'restore'].includes(data?.type) && member.seat >= 0 && links.size) {
        const p = room.match.seats[member.seat]?.player;
        sendLink({ type: 'build', room: key, linkId: member.linkId, seat: member.seat, op: data,
          who: { homeToken: member.homeToken || '', handle: member.handle || null, token: member.token || null },
          pose: p ? { x: p.x, y: p.y, z: p.z, yaw: p.yaw, pitch: p.pitch || 0 } : null });
        return;
      }
      if (satellite && ['prompt', 'clear-room', 'remove', 'wall-res', 'kidlisp-mod', 'screen', 'paint', 'restore', 'chalk-clear', 'chalk-dust'].includes(data?.type)) {
        send(ws, { type: 'build-result', rid: Builders.rid(data) || undefined, error: 'Nothing is built in this room' });
        return;
      }
      // 🏙️ The roof is nobody's floor and is kept as it is, as the plaza is: only the keeper arranges it.
      if (!openBuild && key === ROOF && member.handle?.toLowerCase() !== keeper && ['prompt', 'clear-room', 'remove', 'wall-res', 'kidlisp-mod', 'screen', 'paint', 'restore', 'chalk-clear', 'chalk-dust'].includes(data?.type)) {
        send(ws, { type: 'build-result', error: 'The roof is kept as it is: build on your floor' });
        if (data.type === 'paint') builders.broadcast(key); // the page drew the square already: take it back
        return;
      }
      // 🧹 The plaza and the blocks belong to the world, not to whoever walks
      // in: only the keeper arranges them, or (in a block) someone who lives
      // there. A visitor builds on their own floor. Rooms outside the world
      // (a game, a room made by name) stay open to whoever is in them.
      // ✨ In the one room everyone builds, and what they build is a
      // conjuring: it stands for `conjureMs` and is gone (the sweep below).
      // The keeper's stays, and only the keeper takes things out, clears the
      // room or changes its walls: the room is theirs to furnish.
      const conjuring = oneRoom && key === HOME_ROOM && member.handle?.toLowerCase() !== keeper;
      const fades = conjuring ? conjureMs : 0;
      // (🧭 Home is `origin`, which is no block of the old park: the same word is said here. A visitor conjures; only the keeper takes things out, clears the room or changes its walls.)
      if (conjuring && !parseBlock(key) && ['clear-room', 'remove', 'wall-res', 'kidlisp-mod'].includes(data?.type)) {
        send(ws, { type: 'build-result', rid: Builders.rid(data) || undefined, error: 'Only the keeper takes things out of the room: what you make fades by itself' });
        return;
      }
      const block = !openBuild && !pod && parseBlock(key);
      if (block && ['prompt', 'clear-room', 'remove', 'wall-res', 'kidlisp-mod', 'screen', 'paint', 'restore'].includes(data?.type)
        && !(conjuring && ['prompt', 'screen', 'paint', 'restore'].includes(data.type))) {
        const handle = member.handle?.toLowerCase();
        const lives = key !== GROUND && handle && geography.geography.home(handle)?.block === blockIndex(block);
        if (handle !== keeper && !lives) {
          send(ws, { type: 'build-result', rid: Builders.rid(data) || undefined, error: key === GROUND ? (oneRoom ? 'Only the keeper takes things out of the room: what you make fades by itself' : 'The plaza is kept as it is: build on your floor') : 'Only someone who lives on this block arranges it' });
          if (data.type === 'paint') builders.broadcast(key);
          return;
        }
      }
      if (!openBuild && pod && !ownPod && data?.type === 'input' && Array.isArray(data.commands))
        data.commands = data.commands.map(c => ({ ...c, chalk: null, use: false, drop: false, fire: false }));
      // ✏️ Chalk is retired (world.mjs CHALK): a stroke sent by a page that still has it is not taken.
      else if (!CHALK && data?.type === 'input' && Array.isArray(data.commands))
        for (const c of data.commands) if (c && c.chalk) c.chalk = null;
      if (data?.type === 'prompt') {
        if (member.seat >= 0) {
          const accepted = builders.request(ws, key, owner, member.seat, data, { guest: !member.token, keeper: keeping, fades: keeping ? 0 : fades });
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
        else send(ws, { type: 'build-result', rid: Builders.rid(data) || undefined, error: 'Spectators cannot build' });
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
        member.look = { x: clamp(data.x, -4000, 4000), y: clamp(data.y, 0, 50000), z: clamp(data.z, -4000, 4000), yaw: data.yaw, at: Date.now() };
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
            // Never on the car, in its shaft or across its door.
            ...((x, z) => (hasCar(key) ? clearOfCar(x, z) : { x, z }))(clamp(Number(data.x) || 0, -halfOf(key) + 1, halfOf(key) - 1), clamp(Number(data.z) || 0, -halfOf(key) + 1, halfOf(key) - 1)),
            yaw: Number(data.yaw) || 0,
            at: Date.now(),
            ...(fades ? { fades: Date.now() + fades } : {}),
          })
          .then(() => builders.broadcast(key));
      if (data?.type === 'paint' && member.seat >= 0 && Number.isInteger(data.i) && Number.isInteger(data.j) && data.i >= 0 && data.i < 18 && data.j >= 0 && data.j < 18)
        void store.paintTile(key, data.i, data.j, data.c, fades).then(() => builders.broadcast(key));
      if (data?.type === 'restore' && member.seat >= 0 && typeof data.code === 'string' && /^[A-Za-z0-9]{2,16}$/.test(data.code))
        void builders.restore(ws, key, owner, member.seat, data.code, Builders.rid(data) || undefined, null, fades);
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
      pairing.drop(ws);
      letGo();
      if (body) {
        const next = room.counterpart(member.handle, member);
        member.ready = false;
        if (next?.room !== room) next?.followBody?.(key, bodyPose(body));
        room.assign(member.handle);
      }
      room.remove(ws);
      // 🧍 The last screen of a handle is gone: its idle body goes too.
      if (member.handle && ![...connections].some((c) => c !== member && c.ready && c.handle?.toLowerCase() === member.handle.toLowerCase()))
        for (const r of rooms.values()) if (r.unpark(member.handle.toLowerCase())) r.assign();
      if (!room.clients.size) {
        const kept = room.match.flight();
        if (kept) flights.set(key, kept);
        rooms.delete(key);
      }
    });
  });

  // The world clock runs the dispenser and its balls across room boundaries.
  // A place's cell of the lattice and how high it is: a floor of the tower
  // stands over the plaza's cell.
  const cellOf = (key) => {
    if (key === BASEMENT) return { i: 0, j: 0, h: altitude(-1), floor: -1 };
    if (key === GROUND) return { i: 0, j: 0, h: 0 };
    const floor = tower.tower.forRoom(key);
    if (floor) return { i: 0, j: 0, h: altitude(floor.number), floor: floor.number };
    if (key === ROOF) return tower.tower.top ? { i: 0, j: 0, h: roofOf(tower.tower.top), floor: roofNumber(tower.tower.top) } : null;
    const block = parseBlock(key);
    return block ? { ...block, h: 0 } : null;
  };
  // 🏬 Two floors on one storey of the short tower (elevator.mjs SHORT) are
  // different rooms in the same place: neither sees into the other.
  const overlaid = (a, b) => !!a.floor && !!b.floor && a.floor !== b.floor && a.h === b.h;
  const dispenser = createDispenser({
    neighbours: (key) => neighboursOf(key),
    boxes: (key) => [...wallsFor(key), ...objectBoxes(store.objects(key).filter((o) => !o.item), Date.now() / 1000)],
    bodies: (key) => keptAt(key) ? farBodies(key) : (rooms.get(key)?.match.seats || []).filter((s) => s?.player?.alive).map((s) => s.player), // (🌐 a block kept elsewhere: who stands there, as the link says)
    below: (key) => { const floor = tower.tower.forRoom(key); return floor ? { room: GROUND, dy: altitude(floor.number) } : key === ROOF && tower.tower.top ? { room: GROUND, dy: roofOf(tower.tower.top) } : null; },
  });
  const ball = dispenser.balls[0]; // first ball for readers of the original map format
  // It lies where it was left: kept in the state directory when it comes to
  // rest, every few seconds while it rolls, and when the server closes.
  const kiosks = createKiosks({ directory: stateDir }); // 🧩 what has been done to the pieces on the panels
  const notepat = createCommunalNotepat({ kiosks, send, sendText });
  const ballFile = stateDir && resolve(stateDir, 'ball.json');
  let ballSaved = Promise.resolve(), ballRolling = false, ballKeptAt = 0, ballLinkTick = 0;
  const keepBall = () => {
    if (!ballFile || satellite) return ballSaved; // (a satellite's ball is home's)
    const [first, ...others] = dispenser.balls.map(sim => sim.ball);
    const text = JSON.stringify({ ...first, others });
    ballKeptAt = Date.now();
    return (ballSaved = ballSaved.then(async () => {
      await mkdirBall(stateDir, { recursive: true });
      await writeBall(`${ballFile}.tmp`, text);
      await renameBall(`${ballFile}.tmp`, ballFile);
    }).catch(() => {}));
  };
  try {
    const was = ballFile && JSON.parse(readBall(ballFile, 'utf8'));
    const saved = [was, ...(Array.isArray(was?.others) ? was.others : [])].filter(b => b && cellOf(String(b.room)) &&
      ['x', 'y', 'z', 'vx', 'vy', 'vz'].every(k => Number.isFinite(b[k]))).slice(0, MAX_BALLS);
    for (const [i, b] of saved.entries()) {
      const sim = i ? dispenser.make() : ball;
      Object.assign(sim.ball, { room: b.room, x: clamp(b.x, -SPAN / 2, SPAN / 2), y: Math.max(0, b.y),
        z: clamp(b.z, -SPAN / 2, SPAN / 2), vx: b.vx, vy: b.vy, vz: b.vz });
      if (i) dispenser.balls.push(sim);
    }
  } catch {} // none kept yet: it starts in the courtyard
  let ballTick = 0;
  // ✈️ What was in the air in a room that emptied, or when the server last
  // stopped (match.mjs flight): taken up when the room is next made.
  const flightFile = stateDir && resolve(stateDir, 'flight.json');
  const flights = new Map();
  try { for (const [key, kept] of Object.entries(JSON.parse(readBall(flightFile, 'utf8')))) flights.set(key, kept); } catch {}
  let flightSaved = Promise.resolve(), flightText = '', flightAt = 0;
  const keepFlights = () => {
    flightAt = Date.now();
    if (!flightFile) return flightSaved;
    const all = Object.fromEntries(flights);
    for (const [key, room] of rooms) { const f = room.match.flight(); if (f) all[key] = f; else delete all[key]; }
    const text = JSON.stringify(all);
    if (text === flightText) return flightSaved;
    flightText = text;
    return (flightSaved = flightSaved.then(async () => {
      await mkdirBall(stateDir, { recursive: true });
      await writeBall(`${flightFile}.tmp`, text);
      await renameBall(`${flightFile}.tmp`, flightFile);
    }).catch(() => {}));
  };
  const seeingBalls = new WeakSet();
  const tellBall = (moved) => {
    if (ballRolling && !moved) keepBall(); // come to rest
    else if (moved && Date.now() - ballKeptAt > 5000) keepBall();
    ballRolling = moved;
    if (!moved && ++ballTick % 30) return; // at rest: twice a second, for whoever just came
    const positions = satellite
      ? (Date.now() - farBallsHeard.at < 1500 ? farBallsHeard.balls : []).map((b, id) => ({ b, id: b.id ?? id, at: cellOf(b.room) }))
      : dispenser.balls.map(({ ball: b }, id) => ({ b, id, at: cellOf(b.room) }));
    const r = n => Math.round(n * 1000) / 1000;
    for (const [key, room] of rooms) {
      const here = cellOf(key);
      if (!here || !room.clients.size) continue;
      const visible = [];
      for (const { b, id, at } of positions) {
        if (!at || overlaid(at, here)) continue;
        const bi = wrap(at.i - here.i), bj = wrap(at.j - here.j);
        if (Math.abs(bi) > 2 || Math.abs(bj) > 2) continue;
        // `g`: the ground it lies on, here (another floor's, or the car's while it runs: `lift` is the car's share).
        const lift = liftOf(b), g = at.h - here.h + lift;
        visible.push({ id, x: r(b.x + bi * SPAN), y: r(b.y + g), z: r(b.z + bj * SPAN),
          vx: r(b.vx), vy: r(b.vy), vz: r(b.vz), ...(g ? { g: r(g) } : {}), ...(b.ride ? { lift: r(lift) } : {}) });
      }
      if (!visible.length) {
        if (seeingBalls.delete(room)) for (const ws of room.clients.keys()) send(ws, { type: 'balls-away' });
        continue;
      }
      seeingBalls.add(room);
      // Older clients draw the first ball. New clients draw all visible slots.
      const [first, ...others] = visible;
      const word = JSON.stringify({ type: 'ball', ...first, ...(others.length ? { others } : {}) });
      for (const ws of room.clients.keys()) sendText(ws, word);
    }
  };
  // 🧟 The horde walks the whole lap on the world's clock (horde.mjs); a block kept on another host is a wall to it.
  const horde = satellite ? null : createHorde({
    neighbours: (key) => {
      const n = neighboursOf(key);
      if (!n) return n;
      return Object.fromEntries(Object.entries(n).filter(([, room]) => !keptAt(room)));
    },
    boxes: (key) => [...wallsFor(key), ...objectBoxes(store.objects(key).filter((o) => !o.item), Date.now() / 1000)],
    start: ['lobby', ...Object.values(neighboursOf('lobby') || {})].filter(key => !keptAt(key)),
    ...(oneRoom ? { count: 2 } : {}), // (🏠 one room: two to shoot at, not eight shut in with you)
  });
  // 👀 The people beyond: each room is told who walks the rooms around it
  // (two blocks out, and the floors over the plaza), in its own metres, 15
  // times a second. A room simulates only its own; what it hears of the rest
  // is for the eye, so the park is one place to look across however many
  // rooms it is made of, and a room's cost does not grow with the world's.
  const BEYOND = 2;
  let beyondTick = 0;
  const tellBeyond = () => {
    if (++beyondTick % 4 || (!rooms.size && !links.size)) return;
    const r = (n) => Math.round(n * 100) / 100, stood = [], fresh = Date.now() - 4000;
    for (const [key, room] of rooms) {
      const at = cellOf(key);
      if (!at) continue;
      const people = [], eyes = [];
      room.match.seats.forEach((s, seat) => {
        const p = s?.player, m = room.members[seat];
        if (p?.alive && m) people.push([`${key}:${seat}`, m.handle ? `@${m.handle}` : m.name || '', seat, p.x, p.y, p.z, p.yaw, p.pitch || 0, p.vx || 0, p.vz || 0, !!p.boat, !!p.bike]);
      });
      // 🚁 Whoever looks on is a small drone where their eye is (heard of in the last four seconds).
      for (const m of room.sessions.values())
        if (m.seat < 0 && m.eye && m.look && m.look.at > fresh)
          eyes.push([m.eye, m.handle ? `@${m.handle}` : m.owner ? guestName(m.owner) : '', m.look.x, m.look.y, m.look.z, m.look.yaw]);
      // 🔫 Its rounds in flight, so the rooms around see a shot fly on.
      const shots = room.match.bullets.map((b) => [b.id, r(b.x), r(b.y), r(b.z), r(b.vx), r(b.vy), r(b.vz)]);
      stood.push({ key, room, at, people, eyes, shots });
    }
    // 🔗 Over the link: ours to the other hosts, theirs (heard in the last 1.5 s) beside ours.
    if (links.size) sendLink({ type: 'stood', rooms: stood.map(({ key, people, eyes, shots }) => ({ key, people, eyes, shots })) });
    for (const { at, rooms: far } of heard.values()) {
      if (Date.now() - at > 1500) continue;
      for (const f of far) { const cell = cellOf(f.key); if (cell && !rooms.has(f.key)) stood.push({ key: f.key, room: null, at: cell, people: f.people, eyes: f.eyes, shots: f.shots || [] }); }
    }
    for (const here of stood) {
      if (!here.room?.clients.size) continue;
      const seen = [], drones = [], shots = [];
      for (const there of stood) {
        const di = wrap(there.at.i - here.at.i), dj = wrap(there.at.j - here.at.j);
        if (Math.abs(di) > BEYOND || Math.abs(dj) > BEYOND || overlaid(there.at, here.at)) continue;
        for (const [id, name, x, y, z, yaw] of there.eyes)
          drones.push([id, name, r(x + di * SPAN), r(y + there.at.h - here.at.h), r(z + dj * SPAN), r(yaw)]);
        if (there === here) continue; // (its own people are in its snapshot)
        for (const [id, x, y, z, vx, vy, vz] of there.shots || [])
          shots.push([`${there.key}:${id}`, r(x + di * SPAN), r(y + there.at.h - here.at.h), r(z + dj * SPAN), vx, vy, vz]);
        for (const [id, name, seat, x, y, z, yaw, pitch, vx, vz, boat, bike] of there.people)
          seen.push([id, name, seat, r(x + di * SPAN), r(y + there.at.h - here.at.h), r(z + dj * SPAN), r(yaw), r(pitch), r(vx), r(vz), boat, bike]);
      }
      for (const z of horde?.zombies || []) { // 🧟 (its own block's too: they are not in any snapshot)
        const at = cellOf(z.room);
        if (!at) continue;
        const di = wrap(at.i - here.at.i), dj = wrap(at.j - here.at.j);
        if (Math.abs(di) > BEYOND || Math.abs(dj) > BEYOND || overlaid(at, here.at)) continue;
        seen.push([`z${z.id}`, '', 3, r(z.x + di * SPAN), r(ZOMBIE.eye + at.h - here.at.h), r(z.z + dj * SPAN), r(z.yaw), 0, r(z.vx), r(z.vz), false, false]);
      }
      if (!seen.length && !drones.length && !shots.length && !here.room.beyondSeen) continue; // (nobody, and it was said)
      here.room.beyondSeen = seen.length + drones.length + shots.length;
      const word = JSON.stringify({ type: 'beyond', people: seen, drones, ...(shots.length ? { shots } : {}) });
      for (const ws of here.room.clients.keys()) sendText(ws, word);
    }
  };
  let previous = performance.now(),
    debt = 0;
  let diagnosticStart = previous, loopGapMaxMs = 0, tickMaxMs = 0;
  const timer = setInterval(() => {
    const now = performance.now();
    if (diagnostics.enabled) loopGapMaxMs = Math.max(loopGapMaxMs, now - previous);
    // 🧊 The server's own stops: the world's clock is asked for every 16 ms,
    // and when it comes this late something else held the one thread (this
    // process serves the whole site). Kept beside what the pages reported
    // (GET /stalls), so a page's quiet can be told from the wire's.
    if (now - previous > 100 && rooms.size) {
      loopStalls.push({ at: Date.now(), gap: Math.round(now - previous), rooms: rooms.size, clients: wss.clients.size });
      if (loopStalls.length > 300) loopStalls.shift();
    }
    // After a stall the world catches up (to half a second): ticks lost here
    // would leave every player's commands waiting behind the world's clock.
    debt += Math.min(500, now - previous);
    previous = now;
    while (debt >= 1000 / HZ) {
      debt -= 1000 / HZ;
      if (satellite) {
        // ⚽ Home's balls, told to whoever is near them here.
        if (rooms.size) tellBall(farBallsHeard.balls.some((b) => Math.hypot(b.vx, b.vy, b.vz) > 0.01));
      } else {
        if (rooms.size) tellBall(dispenser.step());
        // ⚽ …and to the other hosts, fifteen times a second.
        if (links.size && !(++ballLinkTick % 4))
          sendLink({ type: 'balls', balls: dispenser.balls.map(({ ball: b }, id) => ({ id, room: b.room, x: b.x, y: b.y, z: b.z, vx: b.vx, vy: b.vy, vz: b.vz, g: b.g || 0 })) });
      }
      horde?.step();
      tellBeyond();
      if (Date.now() - flightAt > 2000) keepFlights();
      for (const [key, room] of rooms) {
        // 🚁 Who is looking on here, for rounds to meet; 🏙️ whether a gibbed body has a roof to go to.
        room.match.watchers = [...room.sessions.values()].filter((m) => m.seat < 0 && m.eye && m.look && Date.now() - m.look.at < 3000)
          .map((m) => ({ eye: m.eye, x: m.look.x, y: m.look.y, z: m.look.z }));
        room.match.roofBound = !oneRoom && !satellite && key !== ROOF && !!tower.tower.top && !room.match.arena;
        if (horde) room.match.zombies = horde.inRoom(key); // 🧟 for rounds to meet
        room.match.step();
        if (horde && room.match.zombieKills.length) for (const id of room.match.zombieKills.splice(0)) horde.kill(id);
        walks.tick(key, room);
        if (!oneRoom && (key === GROUND || key === BASEMENT)) for (const m of room.sessions.values()) m.crossStairs?.();
        if (aloft(key)) for (const m of room.sessions.values()) m.crossWindow?.();
        for (const m of room.sessions.values())
          if (m.walk) { if (m.walk.until < Date.now()) m.walk = null; else m.walk.ask(); }
        if (room.match.chalkDirty && room.match.tick % 3 === 0)
          for (const msg of room.match.takeChalk())
            for (const ws of room.clients.keys()) send(ws, msg);
        // Every tick (60 Hz): smaller gaps to interpolate across.
        const snap = JSON.stringify(room.match.snapshot());
        for (const ws of room.clients.keys()) sendText(ws, snap);
      }
      // 🏙️ Gibbed bodies queued for the roof go up now, outside the loop over rooms (relocate changes it).
      for (const room of [...rooms.values()])
        for (const seat of room.match.toRoof.splice(0))
          [...room.sessions.values()].find((m) => m.seat === seat)?.toRoof?.();
      // 🌐 Rounds that flew out over an open side go on in the room beside: here, or over the link.
      for (const [key, room] of [...rooms])
        for (const r of room.match.outbound.splice(0)) {
          const to = neighboursOf(key)?.[r.side];
          if (!to) continue;
          const shift = { east: [-SPAN, 0], west: [SPAN, 0], north: [0, -SPAN], south: [0, SPAN] }[r.side];
          const round = { ...r, x: r.x + shift[0], z: r.z + shift[1], from: r.from && !r.from.room ? { ...r.from, room: key } : r.from };
          if (keptAt(to)) sendLink({ type: 'round', room: to, ...round });
          else rooms.get(to)?.match.takeRound(round);
        }
      // 🎯 Kills owed to shooters in other rooms: credited there (here, or over the link), if the seat is still theirs.
      for (const room of [...rooms.values()])
        for (const c of room.match.credits.splice(0)) {
          if (keptAt(c.from.room)) sendLink({ type: 'credit', room: c.from.room, seat: c.from.seat, by: c.by });
          else creditHere(c.from.room, c.from.seat, c.by);
        }
      // ☠️ Each body that died is one more for its handle, in the database; every screen of that handle is told the new count.
      if (deaths) for (const room of rooms.values())
        for (const { seat, attacker, fell } of room.match.deaths.splice(0)) {
          const handle = room.members?.[seat]?.handle;
          if (!handle) continue;
          void deaths.count(handle, fell ? 'fell' : attacker === seat ? 'self' : attacker >= 0 ? 'shot' : 'other').then((total) => {
            if (total === null) return;
            for (const m of connections) if (m.handle?.toLowerCase() === handle.toLowerCase()) send(m.ws, { type: 'deaths', deaths: total });
          });
        }
      else for (const room of rooms.values()) room.match.deaths.length = 0;
      // 🔫 A gun that fired its last round and broke on the ground is taken out of its room (kept among what was removed, to put back).
      for (const [key, room] of [...rooms])
        for (const id of room.match.broken.splice(0))
          void store.removeObject(key, id, 'spent').then((gone) => { if (gone) builders.broadcast(key); });
      // 🎒 Carried-in things left lying go home: listed where they came from again.
      for (const room of [...rooms.values()])
        for (const id of room.match.goingHome.splice(0)) {
          const was = store.uncarry(id);
          if (!was) continue;
          builders.broadcast(was.room);
          // (Brought from another host: that host lists it at home again.)
          if (was.origin.startsWith('~')) sendLink({ type: 'home', id });
          else builders.broadcast(was.origin);
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
    horde,
    ball,
    flights,
    handle,
    attach,
    store,
    geography,
    tower,
    builders,
    walks,
    pairing,
    ready,
    async close() {
      linking = false;
      for (const ws of links.keys()) ws.terminate();
      builders.close();
      stopDevLog();
      clearInterval(braincellTimer);
      clearInterval(timer);
      clearInterval(admitTimer);
      clearInterval(weatherTimer);
      clearInterval(fadeTimer);
      for (const remove of detach) remove();
      for (const ws of wss.clients) ws.terminate();
      await new Promise((resolve) => wss.close(resolve));
      await new Promise((resolve) => builderWss.close(resolve));
      await store.queue;
      await geography.queue;
      await tower.queue;
      await walks.close();
      await keepBall();
      await notepat.close();
      await kiosks.keep();
      await keepFlights();
      if (server.listening)
        await new Promise((resolve) => server.close(resolve));
    },
  };
}

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url) &&
  /server\.mjs$/.test(process.argv[1]) // (bundled into satellite.mjs, this file is not the one being run)
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
