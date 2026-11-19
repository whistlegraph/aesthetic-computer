// 🪄 Spell: "take 1111", typed on the elevator car's line.
//
// A private rax, the real socket, and a cast of @jeffrey plus invented
// neighbours. @jeffrey is the 11th handle founded, so the tower pencils him in
// on floor 11; an invented neighbour who never comes is pencilled in on 1111.
// Each test says what a visitor ought to get.
import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import WebSocket from 'ws';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';
import { CAR_Z, altitude } from '../elevator.mjs';
import { Prompt } from '../prompt.mjs';

// 1,200 handles in founding order: @jeffrey 11th, invented neighbours around him.
const HANDLES = [];
for (let i = 1; i <= 1200; i++) HANDLES.push(i === 11 ? 'jeffrey' : `neighbor${i.toString(36)}`);
const ABSENT = HANDLES[1110]; // pencilled in on 1111, never comes

const CLIENT = await readFile(new URL('../client.mjs', import.meta.url), 'utf8');

// The page's own car-screen code (client.mjs: readDirectory, floorAsked,
// carScreen), lifted out as written and run here: the page itself needs a
// browser. Only its surroundings are stood in for.
function carScreenOf(base) {
  const from = CLIENT.indexOf('// 📟 Who lives on which floor'), to = CLIENT.indexOf('// Where the plaza (and the tower on it)');
  assert.ok(from > 0 && to > from, 'the car screen code is where it was in client.mjs');
  const source = CLIENT.slice(from, to).replaceAll('import.meta.url', 'BASE');
  return new Function('fetch', 'altitude', 'BASE', `
    let tower = null, home = null, liftLine = false;
    const chat = { open: false, text: '' };
    ${source}
    return {
      // What client.mjs does on a 'tower' and a 'home' message.
      async hear(d) {
        if (d.type === 'tower') { tower = d; await readDirectory(d.revision ?? d.count); }
        if (d.type === 'home') home = { room: d.room, floor: d.floor };
      },
      // The screen at rest, and the screen while a line is typed on it.
      atRest() { liftLine = false; chat.open = false; return carScreen(null); },
      typing(text) { liftLine = true; chat.open = true; chat.text = text; const s = carScreen(null); liftLine = false; chat.open = false; return s; },
    };
  `)(fetch, altitude, base);
}

async function stage() {
  const dir = await mkdtemp(join(tmpdir(), 'rax-spell-take-1111-'));
  const app = createRaxServer({ stateDir: dir, listHandles: async () => HANDLES,
    verifyIdentity: async (token) => ({ handle: token, sub: `acct-${token}` }),
    resolveHandleColors: async () => null });
  await app.ready;
  await app.tower.change((t) => t.admit(HANDLES));
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const base = `http://127.0.0.1:${app.server.address().port}`;
  // A signed-in visitor over the real socket; `owner` carries a body between rooms.
  const join_ = async (handle, room = 'lobby', { owner, passage } = {}) => {
    const ws = new WebSocket(`${base.replace('http', 'ws')}/play?room=${room}&protocol=${PROTOCOL}&account=1${passage ? `&passage=${passage}` : ''}`);
    ws.got = [];
    ws.on('message', (raw) => { const d = JSON.parse(raw); if (d.type !== 'snapshot') ws.got.push(d); });
    await once(ws, 'open');
    ws.send(JSON.stringify({ type: 'identity', token: owner }));
    ws.send(JSON.stringify({ type: 'account', accessToken: handle }));
    ws.until = async (f, what = 'a reply') => {
      for (let i = 0; i < 400; i++) {
        const hit = ws.got.find(f);
        if (hit) return hit;
        await delay(10);
      }
      throw Error(`waited for ${what}; heard ${JSON.stringify(ws.got.map((d) => d.type))}`);
    };
    ws.tell = (d) => ws.send(JSON.stringify(d));
    await ws.until((d) => d.type === 'tower' && d.yours, 'a floor of my own');
    return ws;
  };
  // Stand a visitor in the car (the director sets the mark by hand, as tower.test does).
  const intoCar = (room) => {
    for (const m of app.rooms.get(room).sessions.values())
      if (m.seat >= 0) Object.assign(app.rooms.get(room).match.seats[m.seat].player, { x: 0, z: CAR_Z });
  };
  const floor = async (q) => (await fetch(`${base}/tower?q=${q}`)).json();
  return { app, base, join: join_, intoCar, floor, close: () => app.close() };
}

test('floor 11 is @jeffrey\'s by founding order, and it is arriving (not a take) that marks it held', async () => {
  const s = await stage();
  try {
    assert.deepEqual(await s.floor('jeffrey').then((f) => [f.number, f.held]), [11, false]); // pencilled in
    const ws = await s.join('jeffrey');
    assert.equal((await ws.until((d) => d.type === 'home')).floor, 11);
    assert.deepEqual(await s.floor('jeffrey').then((f) => [f.number, f.held]), [11, true]); // no take was sent
    assert.deepEqual(await s.floor('1111').then((f) => [f.handle, f.held]), [ABSENT, false]);
    ws.close();
  } finally { await s.close(); }
});

test('typing "take 1111" on the car\'s line sends the whole number, 1111, not 11', () => {
  // The line is a Prompt (client.mjs: `const chat = new Prompt()`), one key at a time.
  const chat = new Prompt();
  chat.begin();
  for (const key of 'take 1111') chat.key({ key });
  assert.equal(chat.text, 'take 1111');
  assert.equal(chat.text.slice(0, 22), 'take 1111'); // what the screen shows of the line
  // The pattern Enter tests the line with, as written in client.mjs.
  const written = /const taking = liftLine && (\/.+?\/i)\.exec\(text\)/.exec(CLIENT);
  assert.ok(written, 'the take pattern is where it was in client.mjs');
  const taking = new Function(`return ${written[1]}`)().exec(chat.text.trim());
  assert.equal(Number(taking?.[1]), 1111);
});

test('"take 1111" gives the visitor 1111 when its holder has never come: the car says so, the neighbour gets 11, and home is 1111', async () => {
  const s = await stage();
  try {
    const ws = await s.join('jeffrey');
    const before = await ws.until((d) => d.type === 'home');
    ws.tell({ type: 'take-floor', number: 1111 });
    const said = await ws.until((d) => d.type === 'elevator-result', 'the elevator');
    assert.equal(said.error, undefined);
    assert.equal(said.note, 'floor 1111 is yours');
    const home = await ws.until((d) => d.type === 'home' && d.floor === 1111, 'my new number');
    assert.equal(home.room, before.room); // the room and its things come along
    assert.deepEqual(await s.floor('1111').then((f) => [f.handle, f.held]), ['jeffrey', true]);
    assert.deepEqual(await s.floor(`@${ABSENT}`).then((f) => [f.number, f.held]), [11, false]); // traded
    // From the car on the plaza, "home" now rides to 1111.
    s.intoCar('lobby');
    ws.tell({ type: 'elevator', to: 'home' });
    assert.deepEqual((await ws.until((d) => d.type === 'travel', 'the ride')).ride, { from: 0, to: 1111, handle: 'jeffrey' });
    ws.close();
  } finally { await s.close(); }
});

test('"take 1111" is refused, in words that name the holder, once that neighbour has arrived', async () => {
  const s = await stage();
  try {
    const neighbour = await s.join(ABSENT); // now they have come: 1111 is theirs for good
    neighbour.close();
    const ws = await s.join('jeffrey');
    ws.tell({ type: 'take-floor', number: 1111 });
    const said = await ws.until((d) => d.type === 'elevator-result', 'the elevator');
    assert.equal(said.error, `floor 1111 is @${ABSENT}'s`);
    assert.equal((await s.floor('jeffrey')).number, 11);
    ws.close();
  } finally { await s.close(); }
});

test('after "take 1111" the car\'s screen on my own floor names me, not the neighbour who was pencilled in', async () => {
  const s = await stage();
  try {
    const lobby = await s.join('jeffrey');
    const home = await lobby.until((d) => d.type === 'home');
    const owner = (await lobby.until((d) => d.type === 'identity')).token;
    lobby.close();
    // Up on floor 11, in the car, with the page's screen listening to the socket.
    const ws = await s.join('jeffrey', home.room, { owner });
    const screen = carScreenOf(`${s.base}/`);
    let heard = 0;
    const listen = async () => { for (; heard < ws.got.length; heard++) await screen.hear(ws.got[heard]); };
    await listen();
    assert.deepEqual([screen.atRest().number, screen.atRest().handle], ['11', '@jeffrey']);
    ws.tell({ type: 'take-floor', number: 1111 });
    assert.equal((await ws.until((d) => d.type === 'elevator-result', 'the elevator')).note, 'floor 1111 is yours');
    await ws.until((d) => d.type === 'tower' && d.here?.number === 1111, 'the floor to be renumbered');
    await listen();
    const now = screen.atRest();
    assert.equal(now.number, '1111');
    assert.equal(now.handle, '@jeffrey', `the screen on @jeffrey's floor 1111 reads "${now.number} ${now.handle}"`);
    assert.equal(screen.typing('1111').note, '@jeffrey', 'typing 1111 previews its new holder');
    ws.close();
  } finally { await s.close(); }
});

test('the car\'s screen tells a number that is only pencilled in (free to take) from one that is held', async () => {
  const s = await stage();
  try {
    const held = HANDLES[1111]; // floor 1112: this neighbour arrives, so it is theirs for good
    (await s.join(held)).close();
    const ws = await s.join('jeffrey');
    const screen = carScreenOf(`${s.base}/`);
    for (const d of ws.got) await screen.hear(d);
    assert.deepEqual([(await s.floor('1111')).held, (await s.floor('1112')).held], [false, true]);
    const pencilled = screen.typing('1111').note.replace(ABSENT, 'X'), taken = screen.typing('1112').note.replace(held, 'X');
    assert.notEqual(pencilled, taken, `a takeable floor reads "${screen.typing('1111').note}" and a held one "${screen.typing('1112').note}": the same`);
    ws.close();
  } finally { await s.close(); }
});
