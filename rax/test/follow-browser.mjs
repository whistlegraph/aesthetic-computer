// 🪞 Two windows signed in as one handle: the older looks on as a drone and
// flies after the body the newer one walks, circles it when it stands, and is
// carried over a block's line in the world's metres. node rax/test/follow-browser.mjs
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { existsSync } from 'node:fs';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';

const dir = await mkdtemp(join(tmpdir(), 'rax-follow-browser-'));
const app = createRaxServer({ guests: true, stateDir: dir, listHandles: async () => ['jeffrey'],
  verifyIdentity: async (token) => ({ handle: token, sub: `account:${token}` }) });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const browser = await chromium.launch({ ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}), headless: true, args: ['--enable-unsafe-webgpu'] });
const wait = async (test, ms = 12000, what = 'waited too long') => {
  for (const end = Date.now() + ms; Date.now() < end; await delay(25)) if (await test()) return;
  assert.fail(what);
};
try {
  const errors = [];
  const open = async () => {
    const page = await (await browser.newContext({ viewport: { width: 640, height: 400 } })).newPage();
    page.on('pageerror', (e) => errors.push(e.message));
    await page.addInitScript(() => { window.acTOKEN = 'jeffrey'; });
    await page.goto(`http://127.0.0.1:${app.server.address().port}/?debug&room=lobby`);
    return page;
  };
  const members = () => [...app.rooms.entries()].flatMap(([key, r]) => [...r.sessions.values()].map((m) => ({ key, m })));
  const drone = await open();
  await wait(() => members().some(({ m }) => m.handle === 'jeffrey' && m.seat >= 0), 15000, 'the first window was never seated');
  const droneMember = members().find(({ m }) => m.handle === 'jeffrey').m;
  const walker = await open();
  await wait(() => droneMember.seat < 0 && members().some(({ m }) => m !== droneMember && m.handle === 'jeffrey' && m.seat >= 0), 15000, 'the second window never took the body');
  const walkerMember = members().find(({ m }) => m !== droneMember && m.handle === 'jeffrey').m;
  const roomOf = (member) => members().find(({ m }) => m === member)?.key;
  const body = () => app.rooms.get(roomOf(walkerMember)).match.seats[walkerMember.seat].player;
  const eye = async () => (await drone.evaluate(() => raxDebug())).eye;
  await wait(async () => !!(await eye()), 15000, 'the older window is not looking on');
  await wait(() => droneMember.following === true, 8000, 'the drone never said it follows');

  // The body is set down a long way off: the drone flies there, it is not set down beside it.
  Object.assign(body(), { x: -6, z: -6, y: 1.7, yaw: 0, pitch: 0, vx: 0, vz: 0 });
  await delay(1500);
  Object.assign(body(), { x: 7, z: 7 });
  let last = await eye(), most = 0, lastAt = Date.now();
  const far0 = Math.hypot(last.x - 7, last.z - 7);
  for (let i = 0; i < 80; i++) {
    await delay(50);
    const e = await eye(), now = Date.now();
    most = Math.max(most, Math.hypot(e.x - last.x, e.y - last.y, e.z - last.z) / Math.max(0.03, (now - lastAt) / 1000));
    last = e; lastAt = now;
  }
  const b = body(), gap = Math.hypot(last.x - b.x, last.z - b.z);
  console.log(`flew ${far0.toFixed(1)} m to ${gap.toFixed(2)} m from the body, at ${most.toFixed(1)} m/s at the most`);
  assert.ok(far0 > 8, 'it began a long way off');
  assert.ok(gap < 5.5, `it came up to the body: ${gap.toFixed(2)} m`);
  assert.ok(most < 80, `it flew, it did not jump: ${most.toFixed(1)} m/s`);

  // The body stands: after a moment the drone goes round it.
  const angleOf = (e) => Math.atan2(e.x - body().x, e.z - body().z);
  await delay(4500);
  const a0 = angleOf(await eye());
  await delay(4000);
  const e1 = await eye(), swept = Math.abs(((angleOf(e1) - a0 + 3 * Math.PI) % (2 * Math.PI)) - Math.PI);
  console.log(`circled ${(swept * 180 / Math.PI).toFixed(0)}° in four seconds, ${Math.hypot(e1.x - body().x, e1.z - body().z).toFixed(2)} m out`);
  assert.ok(swept > 0.5, 'it circles a body that stands');

  // Over the east line: the drone is brought on its socket and is where it was in the world.
  // (The walker's own page asks to cross as soon as its body is past the line.)
  await delay(1500);
  const before = await eye(), from = roomOf(walkerMember);
  Object.assign(body(), { x: 8.3, z: 0.2, y: 1.7, yaw: 90, vx: 6 });
  await delay(300);
  if (roomOf(walkerMember) === from) walkerMember.ws.emit('message', JSON.stringify({ type: 'walk', edge: 'east', in: from }));
  await wait(() => roomOf(walkerMember) !== from && roomOf(droneMember) === roomOf(walkerMember), 8000, `the drone was not brought to the next room: body in ${roomOf(walkerMember)}, drone in ${roomOf(droneMember)}, following ${droneMember.following}`);
  await wait(async () => (await drone.evaluate(() => raxDebug())).room === roomOf(walkerMember) && !!(await eye()), 8000, 'the drone page never arrived');
  await delay(400);
  const after = await eye(), nb = body();
  console.log(`over the line: eye ${before.x.toFixed(1)} → ${after.x.toFixed(1)}, body at ${nb.x.toFixed(1)}`);
  assert.ok(Math.abs(after.x - (before.x - 16)) < 9, `carried over in the world's metres, not set down afresh: ${after.x.toFixed(1)}`);
  // (It flies on after the body from there: by it again within a few seconds.)
  await wait(async () => { const e = await eye(), b = body(); return Math.hypot(e.x - b.x, e.z - b.z) < 6; }, 6000, 'it did not fly on to the body in the next room').catch(async (e) => { const d = await drone.evaluate(() => raxDebug()); throw Error(`${e.message}: eye ${JSON.stringify(d.eye)}, body ${JSON.stringify({ x: body().x, y: body().y, z: body().z })}, following ${droneMember.following}, drone room ${d.room}, seat ${d.seat}`); });
  assert.deepEqual(errors, []);
  console.log('follow browser ok');
} finally {
  await browser.close();
  await app.close();
  await rm(dir, { recursive: true, force: true });
}
