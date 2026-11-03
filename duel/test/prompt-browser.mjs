import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdir } from 'node:fs/promises';
import { chromium } from 'playwright';
import { createDuelServer } from '../server.mjs';
import { connectBuilder, generateObject } from '../aesel.mjs';
const token = 'b'.repeat(64),
  app = createDuelServer({ builderToken: token });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}`;
const live = process.argv.includes('--live');
const worker = connectBuilder({
  url: base.replace('http', 'ws') + '/builder',
  token,
  generate: live
    ? generateObject
    : async (job) => ({
        name: 'Staircase',
        boxes: Array.from({ length: 8 }, (_, i) => ({
          x: 0,
          y: (i + 1) * 0.15,
          z: i * 0.6,
          w: job.previous ? 3 : 2,
          h: (i + 1) * 0.3,
          d: 0.6,
          color: 0x69bfff,
        })),
      }),
});
const browser = await chromium.launch({
  headless: true,
  channel: 'chrome',
  args: ['--enable-unsafe-webgpu'],
});
const errors = [];
const contexts = [];
const pages = [];
await mkdir('tmp/duel', { recursive: true });
try {
  for (let i = 0; i < 2; i++) {
    const c = await browser.newContext({ viewport: { width: 900, height: 650 } });
    contexts.push(c);
    const p = await c.newPage();
    pages.push(p);
    p.on('pageerror', (e) => errors.push(e.message));
    await p.goto(base + '/?room=prompt-test');
    await p.waitForFunction(
      () =>
        document.querySelector('canvas').doolStats?.builder &&
        document.querySelector('canvas').doolStats?.owner,
    );
  }
  const a = pages[0],
    b = pages[1];
  await a.locator('canvas').focus();
  await a.keyboard.press('Enter');
  await a.keyboard.type('add a small eight-step staircase');
  await a.waitForFunction(
    () =>
      document.querySelector('canvas').doolStats.promptText === 'add a small eight-step staircase',
  );
  assert.equal(await a.locator('input,textarea,button,form,div,span').count(), 0);
  await a.screenshot({ path: 'tmp/duel/prompt-entry.png' });
  await a.keyboard.press('Enter');
  await a.waitForFunction(
    () => document.querySelector('canvas').doolStats.objects.length === 1,
    {},
    { timeout: 170000 },
  );
  await b.waitForFunction(() => document.querySelector('canvas').doolStats.objects.length === 1);
  const first = app.store.objects('prompt-test')[0];
  assert.equal(first.version, 0);
  assert.equal(
    first.owner,
    await a.evaluate(() => document.querySelector('canvas').doolStats.owner),
  );
  // Aim the real player at the new object before reopening the in-engine prompt.
  const p = app.rooms.get('prompt-test').match.seats[0].player;
  p.x = first.x;
  p.z = first.z - 6;
  p.y = 1.7;
  p.yaw = 0;
  p.pitch = 0;
  p.life++;
  await a.waitForTimeout(250);
  await a.screenshot({ path: 'tmp/duel/prompt-v0.png' });
  await a.keyboard.press('Enter');
  await a.keyboard.type('make the staircase wider');
  await a.keyboard.press('Enter');
  await a.waitForFunction(
    () => document.querySelector('canvas').doolStats.objects.some((o) => o.version === 1),
    {},
    { timeout: 170000 },
  );
  await b.waitForFunction(() =>
    document.querySelector('canvas').doolStats.objects.some((o) => o.version === 1),
  );
  assert.equal(app.store.data.rooms['prompt-test'][0].versions.length, 2);
  await b.locator('canvas').focus();
  await b.keyboard.press('Enter');
  await b.keyboard.type('a small staircase');
  await b.keyboard.press('Enter');
  await b.waitForFunction(
    () => document.querySelector('canvas').doolStats.objects.length === 2,
    {},
    { timeout: 170000 },
  );
  assert.equal(new Set(app.store.objects('prompt-test').map((o) => o.owner)).size, 2);
  await a.screenshot({ path: 'tmp/duel/prompt-v1.png' });
  await a.setViewportSize({ width: 233, height: 390 });
  await a.keyboard.press('Enter');
  await a.keyboard.type('add a handrail');
  await a.screenshot({ path: 'tmp/duel/prompt-tile.png' });
  await a.keyboard.press('Escape');
  await a.waitForFunction(() => !document.querySelector('canvas').doolStats.promptOpen);
  assert.deepEqual(errors, []);
  console.log(
    `Prompt browser passed (${live ? 'live remote Aesel' : 'test builder'}): Enter typing, GPU prompt, two owners, v0/v1 shared, compact layout, Escape.`,
  );
} finally {
  await browser.close();
  worker.close();
  await app.close();
}
