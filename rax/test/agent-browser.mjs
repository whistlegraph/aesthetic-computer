// 🦾 rax played with no eyes and no mouse: the page's words (#rax-mirror) and
// its calls (window.rax, agent.mjs) are enough to know where you are, walk,
// turn and speak. Driven the way any agent or screen reader would, through
// the DOM alone. node rax/test/agent-browser.mjs
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { existsSync } from 'node:fs';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';

const dir = await mkdtemp(join(tmpdir(), 'rax-agent-browser-'));
const app = createRaxServer({ guests: true, stateDir: dir, listHandles: async () => ['jeffrey'],
  verifyIdentity: async (token) => ({ handle: token, sub: `account:${token}` }) });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const browser = await chromium.launch({ ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}), headless: true, args: ['--enable-unsafe-webgpu'] });
const wait = async (test, ms = 15000, what = 'waited too long') => {
  for (const end = Date.now() + ms; Date.now() < end; await delay(50)) if (await test()) return;
  assert.fail(what);
};
try {
  const errors = [];
  const page = await (await browser.newContext({ viewport: { width: 640, height: 400 } })).newPage();
  page.on('pageerror', (e) => errors.push(e.message));
  await page.addInitScript(() => { window.acTOKEN = 'jeffrey'; });
  await page.goto(`http://127.0.0.1:${app.server.address().port}/`);
  const state = () => page.evaluate(() => window.rax?.state());
  await wait(async () => { const s = await state(); return s?.connected && s.you.seated && s.you.x !== null; }, 20000, 'never seated');

  // 1. State: who and where, in numbers.
  const first = await state();
  assert.equal(first.room, 'origin'); // (🧭 home, world.mjs HOME_ROOM; the old plaza is ~plaza)
  assert.equal(first.you.handle, 'jeffrey');
  // (🏠 world.mjs ONE_ROOM: while the world is one room the plaza has no way out; what is asked of ways below holds when it opens again)
  const open = first.nearby.some((t) => t.kind === 'way');
  for (const t of first.nearby) assert.ok(t.name && Number.isFinite(t.distance) && t.where && t.compass, `a near thing is named and placed: ${JSON.stringify(t)}`);

  // 2. The mirror says the same in words, in a live region, with real buttons.
  await wait(async () => (await page.locator('#rax-words').textContent())?.includes('origin'), 5000, 'the mirror never described the origin');
  const words = await page.locator('#rax-words').textContent();
  assert.match(words, /You are @jeffrey/);
  if (open) assert.match(words, /Ways out:/);
  assert.equal(await page.locator('#rax-live').getAttribute('aria-live'), 'polite');
  const buttons = await page.locator('#rax-mirror button').allTextContents();
  for (const label of ['Use', 'Jump', 'Turn left', 'Step forward', 'Say']) assert.ok(buttons.includes(label), `a ${label} button`);
  if (open) assert.ok(buttons.some((b) => b.startsWith('Go through the')), `a button for a way out: ${buttons.join(' | ')}`);
  assert.equal(await page.locator('#rax-mirror button', { hasText: 'Sign in' }).isHidden(), true, 'signed in: no sign-in button');

  // (north is +z and east is +x, as geography.mjs has it)
  for (const t of first.nearby.filter((n) => n.kind === 'way')) {
    if (t.side === 'north') assert.ok(t.z > first.you.z, 'the north side lies to +z');
    if (t.side === 'east') assert.ok(t.x > first.you.x, 'the east side lies to +x');
  }
  await page.evaluate(() => window.rax.look({ yaw: 0 }));
  await wait(async () => (await state()).you.facing === 'north', 3000, 'did not face north at yaw 0');
  // 3. Turning: look({ yaw }) is what state() then reports.
  await page.evaluate(() => window.rax.look({ yaw: 90 }));
  await wait(async () => (await state()).you.facing === 'east', 3000, 'did not face east at yaw 90');

  // 4. Walking: move() holds the stick, and the body goes the way it faces.
  const before = (await state()).you;
  await page.evaluate(() => window.rax.move({ fwd: 1, seconds: 1.2 }));
  const after = (await state()).you;
  assert.ok(after.x - before.x > 1.5, `walked east: x ${before.x} → ${after.x}`);
  assert.ok(Math.abs(after.z - before.z) < 1, `and only east: z ${before.z} → ${after.z}`);

  // 5. walkTo a point: turns to it and stops there.
  const to = { x: after.x - 6, z: after.z + 6 };
  const walked = await page.evaluate((at) => window.rax.walkTo(at, { within: 1.5, seconds: 15 }), to);
  assert.equal(walked.ok, true, `walkTo a point: ${JSON.stringify(walked)}`);
  const there = (await state()).you;
  assert.ok(Math.hypot(there.x - to.x, there.z - to.z) < 2.5, `stands by the point: ${JSON.stringify(there)}`);

  // 6. An unknown name is refused, with what is here.
  const lost = await page.evaluate(() => window.rax.walkTo('no such thing'));
  assert.equal(lost.ok, false);
  assert.ok(Array.isArray(lost.nearby));

  // 7. Saying: through the form a keyboard or screen reader would use.
  await page.locator('#rax-say').fill('hello from the mirror');
  await page.locator('#rax-mirror button', { hasText: /^Say$/ }).click();
  await wait(async () => (await state()).said.some((c) => c.text === 'hello from the mirror'), 5000, 'the line was never heard back');
  assert.equal(await page.locator('#rax-say').inputValue(), '');

  // 8. A way out, by its button: the room changes under the walker.
  const way = (await state()).nearby.find((t) => t.kind === 'way' && t.to);
  if (way) {
    await page.locator('#rax-reach button', { hasText: `Go through the ${way.name}` }).first().click();
    await wait(async () => (await state()).room === way.to, 40000, `never walked through the ${way.name}`);
    await wait(async () => (await page.locator('#rax-words').textContent())?.includes(`Block ${way.to}`), 5000, 'the mirror never named the new block');
  }

  // 9. Signed out: whoever looks on (a drone) is flown by the same calls.
  const guest = await (await browser.newContext({ viewport: { width: 640, height: 400 } })).newPage();
  guest.on('pageerror', (e) => errors.push(e.message));
  await guest.goto(`http://127.0.0.1:${app.server.address().port}/`);
  const seen = () => guest.evaluate(() => window.rax?.state());
  await wait(async () => { const s = await seen(); return s?.connected && s.you.x !== null; }, 20000, 'the guest never arrived');
  const looking = await seen();
  assert.equal(looking.you.signedIn, false);
  assert.equal(await guest.locator('#rax-mirror button', { hasText: 'Sign in' }).isHidden(), false, 'signed out: a sign-in button');
  if (!looking.you.seated) {
    assert.match(await guest.evaluate(() => window.rax.describe()), /looking on/);
    await guest.evaluate(() => window.rax.look({ yaw: 90, pitch: 0 }));
    const flew = await guest.evaluate(() => window.rax.move({ fwd: 1, seconds: 1.2 }));
    assert.ok(flew.you.x - looking.you.x > 1, `the drone flew east: x ${looking.you.x} → ${flew.you.x}`);
    const spot = { x: flew.you.x - 5, z: flew.you.z };
    const flown = await guest.evaluate((at) => window.rax.walkTo(at, { within: 1.5, seconds: 15 }), spot);
    assert.equal(flown.ok, true, `the drone was flown to a point: ${JSON.stringify(flown)}`);
  }

  assert.deepEqual(errors, []);
  console.log(`✔ rax by words and calls: state, mirror, turn, walk, walkTo, say${way ? `, and through the ${way.name}` : ''}`);
} finally {
  await browser.close();
  app.server.close();
  await rm(dir, { recursive: true, force: true });
}
