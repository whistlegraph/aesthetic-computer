// 📱 Signing a screen in from a phone (pairing.mjs): a code works once, runs
// out, belongs to the socket that asked, and only a verified token with a
// handle approves it. Then the same through a server: the screen's socket is
// seated under the phone's handle and never hears the phone's token.
import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { WebSocket } from 'ws';
import { createPairing, ipOf } from '../pairing.mjs';
import { pairNumber, CODE } from '../pair-number.mjs';
import { createRaxServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';

const accounts = { 'phone-token': { sub: 'acct-jo', handle: 'jo' }, 'new-token': { sub: 'acct-new', handle: '' } };
const verify = async (token) => {
  if (!accounts[token]) throw Error('not authenticated');
  return accounts[token];
};
// A pairing with a clock the test turns, and a screen that notes who it became.
const desk = (limits) => {
  let at = 1e6;
  const pairing = createPairing({ verify, now: () => at, limits });
  const screen = () => {
    const s = { as: [] };
    s.approve = (account) => (s.as.push(account.handle), true);
    return s;
  };
  return { pairing, screen, wait: (ms) => (at += ms) };
};

test('a code is long, URL-safe, and carries a four-digit number both sides can work out', async () => {
  const { pairing, screen } = desk();
  const s = screen(), made = await pairing.create(s, '1.1.1.1', s.approve);
  assert.match(made.code, CODE);
  assert.ok(Buffer.from(made.code, 'base64url').length >= 16, 'at least 128 bits');
  assert.match(made.number, /^\d{4}$/);
  assert.equal(made.number, await pairNumber(made.code), 'the phone derives the same number from the code');
  assert.notEqual((await pairing.create(screen(), '1.1.1.1', () => true)).code, made.code);
});

test('a code approves exactly once, for the socket that asked', async () => {
  const { pairing, screen } = desk();
  const a = screen(), b = screen();
  const forA = await pairing.create(a, '1.1.1.1', a.approve), forB = await pairing.create(b, '1.1.1.2', b.approve);
  assert.deepEqual(await pairing.approve(forA.code, 'phone-token', '9.9.9.9'), { status: 200, ok: true, handle: 'jo' });
  assert.deepEqual(a.as, ['jo']);
  assert.deepEqual(b.as, [], 'the other screen is not signed in');
  assert.equal((await pairing.approve(forA.code, 'phone-token', '9.9.9.9')).error, 'gone', 'used');
  assert.deepEqual(a.as, ['jo']);
  assert.equal((await pairing.approve(forB.code, 'phone-token', '9.9.9.9')).ok, true);
  assert.deepEqual(b.as, ['jo']);
  assert.equal(pairing.size, 0);
});

test('two phones at once: one of them signs the screen in', async () => {
  let release;
  const slow = new Promise((r) => (release = r));
  const pairing = createPairing({ verify: async (t) => (await slow, verify(t)) });
  const as = [], made = await pairing.create({}, '1.1.1.1', (a) => (as.push(a.handle), true));
  const both = [pairing.approve(made.code, 'phone-token', '9.9.9.1'), pairing.approve(made.code, 'phone-token', '9.9.9.2')];
  release();
  assert.deepEqual((await Promise.all(both)).map((r) => r.status).sort(), [200, 410]);
  assert.deepEqual(as, ['jo']);
});

test('a code runs out, and a closed socket takes its codes with it', async () => {
  const { pairing, screen, wait } = desk();
  const a = screen(), b = screen();
  const old = await pairing.create(a, '1.1.1.1', a.approve);
  wait(119000);
  const kept = await pairing.create(b, '1.1.1.2', b.approve);
  wait(2000);
  assert.equal((await pairing.approve(old.code, 'phone-token', '9.9.9.9')).status, 410, 'two minutes and it is gone');
  pairing.drop(b);
  assert.equal((await pairing.approve(kept.code, 'phone-token', '9.9.9.9')).status, 410);
  assert.deepEqual([a.as, b.as], [[], []]);
  // A screen that would not take it (closed, or signed in meanwhile) uses the code up all the same.
  const c = await pairing.create(a, '1.1.1.1', () => false);
  assert.equal((await pairing.approve(c.code, 'phone-token', '9.9.9.9')).status, 410);
  assert.equal(pairing.size, 0);
});

test('a socket holds the code it shows and the one before, no more', async () => {
  const { pairing, screen } = desk();
  const s = screen(), codes = [];
  for (let i = 0; i < 3; i++) codes.push((await pairing.create(s, '1.1.1.1', s.approve)).code);
  assert.equal(pairing.size, 2);
  assert.equal((await pairing.approve(codes[0], 'phone-token', '9.9.9.9')).status, 410);
  assert.equal((await pairing.approve(codes[1], 'phone-token', '9.9.9.9')).ok, true, 'scanned just before the screen changed');
  assert.equal((await pairing.approve(codes[2], 'phone-token', '9.9.9.9')).status, 410, 'one approval ends them all');
});

test('a bad token, an account with no handle, and a malformed code are refused', async () => {
  const { pairing, screen } = desk();
  const s = screen(), made = await pairing.create(s, '1.1.1.1', s.approve);
  assert.deepEqual(await pairing.approve(made.code, 'stolen', '9.9.9.9'), { status: 401, error: 'account' });
  assert.deepEqual(await pairing.approve(made.code, 'new-token', '9.9.9.9'), { status: 403, error: 'handle' });
  for (const [code, token] of [['short', 'phone-token'], [made.code + '!', 'phone-token'], [null, 'phone-token'], [made.code, ''], [made.code, 7], [made.code, 'x'.repeat(8000)]])
    assert.equal((await pairing.approve(code, token, '9.9.9.9')).status, 400);
  assert.equal((await pairing.approve('A'.repeat(22), 'phone-token', '9.9.9.9')).status, 410, 'a guess');
  assert.deepEqual(s.as, []);
  assert.equal((await pairing.approve(made.code, 'phone-token', '9.9.9.9')).ok, true, 'the code itself was not spent by them');
  assert.deepEqual(await pairing.who('phone-token', '9.9.9.8'), { status: 200, ok: true, handle: 'jo' });
  assert.equal((await pairing.who('stolen', '9.9.9.8')).status, 401);
});

test('refused five times a code is thrown away', async () => {
  const { pairing, screen } = desk();
  const s = screen(), made = await pairing.create(s, '1.1.1.1', s.approve);
  for (let i = 0; i < 5; i++) assert.equal((await pairing.approve(made.code, 'stolen', `9.9.9.${i}`)).status, 401);
  assert.equal((await pairing.approve(made.code, 'phone-token', '9.9.8.1')).status, 410);
  assert.deepEqual(s.as, []);
});

test('rate limits hold: codes per socket and per address, approvals per address', async () => {
  const { pairing, screen, wait } = desk({ createPerSocket: [3, 60000], createPerIp: [5, 60000], approvePerIp: [4, 60000] });
  const s = screen();
  for (let i = 0; i < 3; i++) assert.ok((await pairing.create(s, '1.1.1.1', s.approve)).code);
  assert.deepEqual(await pairing.create(s, '1.1.1.1', s.approve), { error: 'slow' }, 'a fourth from one socket');
  for (let i = 0; i < 2; i++) assert.ok((await pairing.create(screen(), '1.1.1.1', () => true)).code);
  assert.deepEqual(await pairing.create(screen(), '1.1.1.1', () => true), { error: 'slow' }, 'a sixth from one address');
  const other = screen(), made = await pairing.create(other, '2.2.2.2', other.approve);
  assert.ok(made.code, 'another address is not held up');
  for (let i = 0; i < 4; i++) assert.equal((await pairing.approve('B'.repeat(22), 'phone-token', '7.7.7.7')).status, 410);
  assert.equal((await pairing.approve(made.code, 'phone-token', '7.7.7.7')).status, 429, 'a guesser is stopped, right code or not');
  assert.deepEqual(other.as, []);
  wait(61000);
  assert.ok((await pairing.create(s, '1.1.1.1', s.approve)).code, 'and let go when the window passes');
});

test('an address is taken from the proxy only when the proxy is this machine', () => {
  const req = (peer, headers = {}) => ({ socket: { remoteAddress: peer }, headers });
  assert.equal(ipOf(req('203.0.113.9', { 'cf-connecting-ip': '1.2.3.4' })), '203.0.113.9');
  assert.equal(ipOf(req('127.0.0.1', { 'cf-connecting-ip': '1.2.3.4' })), '1.2.3.4');
  assert.equal(ipOf(req('::1', { 'x-forwarded-for': '5.5.5.5, 6.6.6.6' })), '6.6.6.6');
  assert.equal(ipOf(req('127.0.0.1', { 'x-forwarded-for': 'not an address' })), '127.0.0.1');
});

test('a phone signs a looking-on screen in: seated under its handle, for that socket only', async () => {
  const app = createRaxServer({
    verifyIdentity: verify,
    resolveHandleColors: async () => null,
    listHandles: async () => [],
  });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const port = app.server.address().port;
  const wait = async (f) => {
    for (let i = 0; i < 200; i++) {
      if (f()) return;
      await delay(10);
    }
    throw Error('timeout');
  };
  const open = async () => {
    const ws = new WebSocket(`ws://127.0.0.1:${port}/play?room=pairing&protocol=${PROTOCOL}`), messages = [], raw = [];
    ws.on('message', (data) => { raw.push(String(data)); messages.push(JSON.parse(data)); });
    await once(ws, 'open');
    await wait(() => messages.some((m) => m.type === 'gate'));
    ws.send(JSON.stringify({ type: 'identity' }));
    return { ws, messages, raw };
  };
  const post = async (path, body, headers = {}) => {
    const r = await fetch(`http://127.0.0.1:${port}${path}`, { method: 'POST', headers: { 'Content-Type': 'application/json', ...headers }, body: JSON.stringify(body) });
    return { status: r.status, ...(await r.json()) };
  };
  const tv = await open(), other = await open();
  try {
    assert.equal(tv.messages.find((m) => m.type === 'gate').needs, 'account');
    assert.equal(tv.messages.find((m) => m.type === 'welcome').seat, -1, 'looking on');
    tv.ws.send(JSON.stringify({ type: 'pair' }));
    other.ws.send(JSON.stringify({ type: 'pair' }));
    await wait(() => tv.messages.some((m) => m.type === 'pair') && other.messages.some((m) => m.type === 'pair'));
    const { code, number, ttl } = tv.messages.find((m) => m.type === 'pair');
    assert.match(code, CODE);
    assert.equal(number, await pairNumber(code));
    assert.equal(ttl, 120000);

    // The phone's page is served, and names the handle before anything is approved.
    const page = await fetch(`http://127.0.0.1:${port}/pair`);
    assert.equal(page.status, 200);
    assert.match(await page.text(), /pair\.mjs/);
    assert.deepEqual(await post('/pair/who', { accessToken: 'phone-token' }), { status: 200, ok: true, handle: 'jo' });

    assert.equal((await post('/pair', { code, accessToken: 'stolen' })).status, 401);
    assert.equal((await post('/pair', { code, accessToken: 'phone-token' }, { Origin: 'https://elsewhere.example' })).status, 403, 'only our own page');
    assert.equal(tv.messages.some((m) => m.type === 'role'), false);

    assert.deepEqual(await post('/pair', { code, accessToken: 'phone-token' }), { status: 200, ok: true, handle: 'jo' });
    await wait(() => tv.messages.some((m) => m.type === 'role' && m.seat >= 0));
    await wait(() => tv.messages.some((m) => m.type === 'presence' && m.self?.handle === 'jo'));
    const people = [...app.rooms.get('pairing').sessions.values()], member = people.find((m) => m.handle);
    assert.equal(member.handle, 'jo');
    assert.equal(member.accountId, 'acct-jo');
    assert.ok(member.seat >= 0, 'seated');
    assert.equal(member.token, null, 'the phone’s token is not kept for the screen');
    assert.equal(tv.raw.some((line) => line.includes('phone-token')), false, 'and never sent to it');
    assert.equal(tv.messages.some((m) => m.type === 'whereabouts'), false, 'it stands up where it is');

    assert.equal((await post('/pair', { code, accessToken: 'phone-token' })).status, 410, 'used');
    const stillOut = people.find((m) => m !== member);
    assert.equal(people.length, 2);
    assert.equal(stillOut.handle, null, 'the screen beside it is who it was');
    assert.equal(stillOut.seat, -1);

    // Signed in, it is given no more codes; and a closed socket's code is dead.
    const pairs = tv.messages.filter((m) => m.type === 'pair').length;
    tv.ws.send(JSON.stringify({ type: 'pair' }));
    const mine = other.messages.find((m) => m.type === 'pair').code;
    other.ws.close();
    await wait(() => app.pairing.size === 0);
    assert.equal((await post('/pair', { code: mine, accessToken: 'phone-token' })).status, 410);
    assert.equal(tv.messages.filter((m) => m.type === 'pair').length, pairs);
  } finally {
    tv.ws.terminate();
    other.ws.terminate();
    await app.close();
  }
});

test('a paired screen keeps its handle through the elevator: the passage carries it', async () => {
  const app = createRaxServer({ verifyIdentity: verify, resolveHandleColors: async () => null, listHandles: async () => ['jo'] });
  await app.ready;
  await app.tower.change((t) => t.admit(['jo']));
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const port = app.server.address().port;
  const wait = async (f) => {
    for (let i = 0; i < 300; i++) {
      if (f()) return;
      await delay(10);
    }
    throw Error('timeout');
  };
  const open = (query) => {
    const ws = new WebSocket(`ws://127.0.0.1:${port}/play?protocol=${PROTOCOL}&${query}`), messages = [];
    ws.on('message', (data) => messages.push(JSON.parse(data)));
    return { ws, messages, heard: (type) => messages.find((m) => m.type === type) };
  };
  const tv = open('room=lobby');
  let next = null;
  try {
    await wait(() => tv.heard('gate'));
    tv.ws.send(JSON.stringify({ type: 'identity' }));
    tv.ws.send(JSON.stringify({ type: 'pair' }));
    await wait(() => tv.heard('pair') && tv.heard('identity'));
    const r = await fetch(`http://127.0.0.1:${port}/pair`, { method: 'POST', body: JSON.stringify({ code: tv.heard('pair').code, accessToken: 'phone-token' }) });
    assert.equal(r.status, 200);
    await wait(() => tv.messages.some((m) => m.type === 'role' && m.seat >= 0));
    // Stood in the car (the walk there is the ride tests' business), it asks for its floor.
    const member = [...app.rooms.get('lobby').sessions.values()][0];
    Object.assign(app.rooms.get('lobby').match.seats[member.seat].player, { x: 0, z: 6.1 });
    tv.ws.send(JSON.stringify({ type: 'elevator', to: 'home' }));
    await wait(() => tv.heard('travel'));
    const { room, passage } = tv.heard('travel');
    // The page has no account of its own to offer the next room: only the passage.
    next = open(`room=${room}&passage=${passage}`);
    await wait(() => next.heard('welcome'));
    next.ws.send(JSON.stringify({ type: 'identity', token: tv.heard('identity').token }));
    await wait(() => next.messages.some((m) => m.type === 'role' && m.seat >= 0));
    const there = [...app.rooms.get(room).sessions.values()][0];
    assert.equal(there.handle, 'jo');
    assert.ok(!there.token);
  } finally {
    tv.ws.terminate();
    next?.ws.terminate();
    await app.close();
  }
});
