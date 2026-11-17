#!/usr/bin/env node
// 🎬 Shoot a film in dool.
//
//   node duel/film/shoot.mjs tower            a take of films/tower.mjs
//   node duel/film/shoot.mjs tower --open     …and open the camera page in Chrome
//
// It starts a private dool of its own (fresh state, a cast that signs in by
// name, no production data touched), waits for one camera page, runs the
// screenplay — actors here, camera and titles in the page — and writes
// out/<film>-<time>.mp4 beside the recording the page sends back.
import http from 'node:http';
import { once } from 'node:events';
import { mkdtemp, mkdir, readFile, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { execFile, spawn } from 'node:child_process';
import { WebSocketServer } from 'ws';
import { createDuelServer } from '../server.mjs';
import { attachSocketRoute } from '../../lith/socket-routing.mjs';
import { Actor } from './actor.mjs';

const here = dirname(fileURLToPath(import.meta.url));
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const PAGE_MODULES = ['stage.mjs', 'camera.mjs'];

// A film's own dool, with the routes the camera page needs.
export async function studio(film, { port = 0, out = resolve(here, 'out') } = {}) {
  const stateDir = await mkdtemp(join(tmpdir(), `dool-film-${film.name}-`));
  const app = createDuelServer({
    stateDir,
    listHandles: async () => film.handles || [],
    // The cast signs in by name: a token is the handle it plays.
    verifyIdentity: async (token) => ({ handle: token, sub: `film-${token}` }),
    ...(film.offline ? { resolveHandleColors: async () => null } : {}),
  });
  await app.ready;
  await app.tower.change((t) => t.admit(film.handles || []));
  const takes = new Map(); // name → resolve(path)
  const server = http.createServer(async (req, res) => {
    const url = new URL(req.url, 'http://localhost');
    const module = /^\/film\/([a-z-]+\.mjs)$/.exec(url.pathname)?.[1];
    if (req.method === 'GET' && PAGE_MODULES.includes(module)) {
      res.writeHead(200, { 'Content-Type': 'text/javascript', 'Cache-Control': 'no-store' })
        .end(await readFile(join(here, module)));
      return;
    }
    if (req.method === 'POST' && url.pathname === '/film/take') {
      const name = (url.searchParams.get('name') || 'take').replace(/[^a-z0-9-]/gi, '');
      const parts = [];
      for await (const chunk of req) parts.push(chunk);
      await mkdir(out, { recursive: true });
      const path = join(out, `${name}.webm`);
      await writeFile(path, Buffer.concat(parts));
      res.writeHead(200).end('ok');
      takes.get(name)?.(path);
      return;
    }
    return app.handle(req, res);
  });
  app.attach(server);
  // The control line to the camera page: one page at a time.
  const wss = new WebSocketServer({ noServer: true });
  let page = null, state = { room: null, here: [], connected: false }, drewAt = 0, stalled = 0;
  const listeners = new Set();
  attachSocketRoute(server, ['/film/control'], (req, socket, head) =>
    wss.handleUpgrade(req, socket, head, (ws) => {
      page?.close();
      page = ws;
      ws.on('message', (raw) => {
        const d = JSON.parse(raw);
        if (d.op === 'state') {
          // A browser stops drawing a tab it thinks nobody can see; a take
          // with a hole in it is no take. Remember the longest stall.
          const now = Date.now();
          if (d.frames !== state.frames) drewAt = now;
          else if (drewAt) stalled = Math.max(stalled, now - drewAt);
          state = d;
        }
        for (const l of [...listeners]) if (l.test(d)) { listeners.delete(l); l.resolve(d); }
      });
      ws.on('close', () => { if (page === ws) page = null; });
    }));
  const heard = (test, ms, what) => new Promise((ok, fail) => {
    const l = { test, resolve: ok };
    listeners.add(l);
    setTimeout(() => listeners.delete(l) && fail(Error(`the camera page did not answer: ${what}`)), ms).unref?.();
  });
  const tell = (message) => {
    if (!page) throw Error('no camera page is connected');
    page.send(JSON.stringify(message));
  };
  server.listen(port, '127.0.0.1');
  await once(server, 'listening');
  const base = `http://127.0.0.1:${server.address().port}`;
  return {
    app, server, base, out, takes, tell, heard,
    state: () => state,
    stalled: () => stalled,
    watchStalls() { stalled = 0; drewAt = Date.now(); },
    pageURL: (room = 'lobby') => `${base}/?film=${film.name}&room=${room}`,
    connected: () => !!page,
    async close() { page?.close(); wss.close(); await app.close(); server.close(); },
  };
}

// What a screenplay's `direct` is handed: the cast, the camera, the titles.
export function director(film, set) {
  const actors = [];
  const until = async (test, ms, what) => {
    const end = Date.now() + ms;
    while (!test()) {
      if (Date.now() > end) throw Error(`timed out waiting for ${what} (the camera page says ${JSON.stringify(set.state())})`);
      await sleep(50);
    }
  };
  const camera = {
    // Call a shot (camera.mjs lists them). The camera keeps it until the next.
    shot: (shot, { cut = true } = {}) => set.tell({ op: 'shot', shot: { within: 7.6, ...shot }, cut }),
    // Go to black, and (given a room) move the camera page there and wait for it.
    async cut(room, { seconds = 0.25 } = {}) {
      set.tell({ op: 'fade', to: 1, seconds });
      await sleep(seconds * 1000 + 60);
      if (room && set.state().room !== room) {
        set.tell({ op: 'room', room });
        await until(() => set.state().room === room && set.state().connected, 8000, `the camera in ${room}`);
      }
    },
    open: (seconds = 0.35) => set.tell({ op: 'fade', to: 0, seconds }),
    // Wait until the camera's room shows this handle.
    sees: (handle) => until(() => set.state().here.includes(handle.toLowerCase()), 8000, `@${handle} in view`),
  };
  return {
    camera,
    // Bring a handle on: in `room`, standing at [x, z], facing a bearing.
    async cast(handle, { room = 'lobby', at = null, facing = null } = {}) {
      const actor = new Actor({ base: set.base, handle });
      actors.push(actor);
      await actor.enter(room);
      if (at || facing !== null) {
        // The director sets a mark by hand: the stage is ours.
        const body = set.app.rooms.get(room).match.seats[actor.seat].player;
        if (at) Object.assign(body, { x: at[0], z: at[1] });
        if (facing !== null) body.yaw = actor.control.yaw = facing;
        await sleep(120);
      }
      return actor;
    },
    // 🛗 An actor rides and the camera rides with them: the page travels the
    // same distance in the same car. Resolves when the car has stopped.
    async ride(actor, to) {
      const trip = await actor.ride(to);
      set.tell({ op: 'room', room: actor.room, ride: trip });
      await until(() => set.state().room === actor.room && set.state().connected, 8000, 'the camera in the car');
      await sleep(300);
      await until(() => !set.state().riding, 15000, 'the car to stop');
      return trip;
    },
    wait: (seconds) => sleep(seconds * 1000),
    fade: async (to, seconds = 0.4) => { set.tell({ op: 'fade', to, seconds }); await sleep(seconds * 1000); },
    async card(title, sub = '', seconds = 2.5) { set.tell({ op: 'card', title, sub, seconds }); await sleep(seconds * 1000); },
    caption: (text, seconds = 0) => set.tell({ op: 'caption', text, seconds }),
    counter: (from, to, { seconds = 1.6, label = '', hold = 1.5 } = {}) => set.tell({ op: 'counter', from, to, seconds, label, hold }),
    async roll() {
      const rolling = set.heard((d) => d.op === 'rolling', 5000, 'roll');
      set.tell({ op: 'roll', size: film.size || [1280, 720], fps: film.fps || 60, bitrate: film.bitrate, take: set.take });
      await rolling;
    },
    actors,
  };
}

const transcode = (webm, mp4, fps) => new Promise((ok, fail) => {
  const ffmpeg = existsSync('/opt/homebrew/bin/ffmpeg') ? '/opt/homebrew/bin/ffmpeg' : 'ffmpeg';
  execFile(ffmpeg, ['-y', '-loglevel', 'error', '-i', webm, '-r', String(fps), '-c:v', 'libx264', '-preset', 'slow',
    '-crf', '15', '-pix_fmt', 'yuv420p', '-movflags', '+faststart', '-an', mp4], (error, _o, stderr) =>
    error ? fail(Error(stderr || error.message)) : ok(mp4));
});

export async function shoot(name, { open = false, port = 8798, log = console.log } = {}) {
  const film = (await import(`./films/${name}.mjs`)).default;
  const set = await studio(film, { port });
  set.take = `${film.name}-${new Date().toISOString().replace(/[-:]/g, '').slice(0, 15)}`;
  const d = director(film, set);
  try {
    await film.set?.({ app: set.app, tower: set.app.tower, store: set.app.store });
    const url = set.pageURL(film.room || 'lobby');
    log(`camera page → ${url}`);
    // A window of its own, so the page is in front and drawing.
    if (open) spawn('open', ['-na', 'Google Chrome', '--args', '--new-window', url], { stdio: 'ignore', detached: true }).unref();
    const end = Date.now() + 120000;
    // Up means connected, visible and drawing: a hidden tab films nothing.
    let drawn = -1, told = false;
    for (;;) {
      const st = set.state();
      if (set.connected() && st.connected && !st.hidden && st.frames > drawn && drawn >= 0) break;
      if (set.connected() && st.hidden && !told) { told = true; log('the camera page is hidden: bring its Chrome tab to the front'); }
      if (set.connected() && st.connected) drawn = st.frames ?? drawn;
      if (Date.now() > end) throw Error('no visible camera page arrived (open the URL above in a Chrome tab and keep it in front)');
      await sleep(300);
    }
    log('camera is up; action');
    const uploaded = new Promise((ok) => set.takes.set(set.take, ok));
    set.watchStalls();
    await film.direct(d);
    const stall = set.stalled();
    set.tell({ op: 'wrap' });
    const answer = await set.heard((m) => m.op === 'uploaded', 60000, 'the take');
    if (!answer.ok) throw Error(`the take did not upload: ${answer.error || 'refused'}`);
    const webm = await uploaded;
    const mp4 = await transcode(webm, webm.replace(/\.webm$/, '.mp4'), film.fps || 60);
    if (stall > 600) throw Error(`the camera page stopped drawing for ${(stall / 1000).toFixed(1)} s (keep its Chrome window visible and in front); the broken take is ${mp4}`);
    log(`take → ${mp4}`);
    return mp4;
  } finally {
    for (const a of d.actors) a.leave();
    await set.close();
  }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const [name, ...flags] = process.argv.slice(2);
  if (!name) { console.error('usage: shoot.mjs <film> [--open] [--port n]'); process.exit(2); }
  const port = flags.includes('--port') ? Number(flags[flags.indexOf('--port') + 1]) : 8798;
  shoot(name, { open: flags.includes('--open'), port }).then(() => process.exit(0), (error) => {
    console.error(`cut: ${error.message}`);
    process.exit(1);
  });
}
