import { test } from 'node:test';
import assert from 'node:assert/strict';
import { compileSeat, preflightSeatHosts } from './deskflow-seat-model.mjs';

const config = `section: screens\n a:\n b:\n c:\nend\nsection: links\n a:\n  down = b\n b:\n c:\nend\nsection: options\n clipboardSharing = true\nend\n`;
const seat = { version: 1, screens: [
  { number: 1, screenName: 'a', address: 'a:1', x: 0, y: 0, width: 600, height: 300 },
  { number: 2, screenName: 'b', address: 'b:1', x: 0, y: 300, width: 300, height: 200 },
  { number: 3, screenName: 'c', address: 'c:1', x: 300, y: 300, width: 300, height: 200 },
] };
test('split boundaries generate reciprocal source and destination ranges', () => {
  const plan = compileSeat(seat, config);
  assert.match(plan.config, /down\(0,50\) = b/);
  assert.match(plan.config, /down\(50,100\) = c/);
  assert.match(plan.config, /up = a\(0,50\)/);
  assert.match(plan.config, /up = a\(50,100\)/);
  assert.match(plan.config, /clipboardSharing = true/);
  assert.equal(compileSeat(seat, plan.config).config, plan.config);
  assert.deepEqual(plan.routeKeys, { a: 105, b: 107, c: 113 });
});
test('unknown, duplicate and missing machine tiles cannot remove existing routes', () => {
  for (const screens of [seat.screens.slice(0, 2), [...seat.screens, seat.screens[0]], seat.screens.map((s, i) => i ? s : { ...s, screenName: 'intruder' })]) {
    assert.throws(() => compileSeat({ version: 1, screens }, config));
  }
});
test('gaps, overlap and sub-percent crossings are rejected', () => {
  for (const x of [1000, 299, 599]) {
    const draft = structuredClone(seat); draft.screens[2].x = x;
    assert.throws(() => compileSeat(draft, config));
  }
});

test('swapping upper monitors reverses the laptop split and keeps reciprocal routes', () => {
  const layout = { version: 1, screens: [
    { number: 1, screenName: 'a', x: 600, y: 0, width: 600, height: 338 },
    { number: 2, screenName: 'b', x: 0, y: 0, width: 600, height: 338 },
    { number: 3, screenName: 'c', x: 400, y: 338, width: 400, height: 250 },
  ] };
  const plan = compileSeat(layout, config);
  assert.match(plan.config, /a:\n\t\tleft = b\n\t\tdown\(0,33\) = c\(50,100\)/);
  assert.match(plan.config, /b:\n\t\tright = a\n\t\tdown\(67,100\) = c\(0,50\)/);
  assert.match(plan.config, /c:\n\t\tup\(50,100\) = a\(0,33\)\n\t\tup\(0,50\) = b\(67,100\)/);
});

const expectedHosts = [
  { machine: 'controller', ok: true, hash: 'old' },
  { machine: 'client', ok: true, hash: 'old' },
  { machine: 'offline', ok: false },
];
const readHost = async machine => {
  if (machine === 'offline') throw new Error('connection timed out');
  return { hash: 'old', role: machine === 'controller' ? 'server' : 'client' };
};
test('known offline peers are deferred while connected clients and controller are checked', async () => {
  const result = await preflightSeatHosts(expectedHosts, readHost);
  assert.deepEqual(result.pendingHosts, ['offline']);
  assert.deepEqual(result.hosts.map(h => h.machine), ['controller', 'client']);
  assert.equal(result.active.machine, 'controller');
});
test('changed, newly offline, and returning hosts require refresh before any writes', async () => {
  for (const [target, replacement] of [
    ['client', async () => ({ hash: 'new', role: 'client' })],
    ['client', async () => { throw new Error('disconnected'); }],
    ['offline', async () => ({ hash: 'old', role: 'client' })],
  ]) {
    await assert.rejects(preflightSeatHosts(expectedHosts, machine => machine === target ? replacement() : readHost(machine)), /refresh before applying/);
  }
});
test('zero or multiple reachable controllers cannot apply', async () => {
  for (const role of ['server', 'client']) {
    await assert.rejects(preflightSeatHosts(expectedHosts, async machine => ({ ...await readHost(machine), role })), /Exactly one active controller/);
  }
});
