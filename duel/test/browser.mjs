import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import { createDuelServer } from '../server.mjs';

const app = createDuelServer();
app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}`;
const browser = await chromium.launch({ headless: true,
  ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}),
  args: ['--disable-background-timer-throttling', '--disable-renderer-backgrounding'] });
const errors = [];
const wait = async (condition, label, timeout = 10000) => {
  const end = Date.now() + timeout;
  while (!condition()) {
    if (Date.now() > end) throw new Error(`Timed out: ${label}`);
    await delay(40);
  }
};
const shots = new URL('../../tmp/duel/', import.meta.url);
await mkdir(shots, { recursive: true });

try {
  const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
  async function tab(mode) {
    const page = await context.newPage();
    const state = { page, snap: null };
    page.on('pageerror', e => errors.push(e.message));
    page.on('websocket', socket => socket.on('framereceived', ({ payload }) => {
      const data = JSON.parse(payload.toString());
      if (data.type === 'snapshot') state.snap = data;
    }));
    await page.goto(`${base}/?room=browser-test&mode=${mode}`);
    await page.locator('canvas').waitFor({ state: 'visible' });
    return state;
  }
  const a = await tab('instagib');
  await a.page.goto(base);
  assert.equal(await a.page.locator('#room').inputValue(), 'lobby');
  await a.page.goto(`${base}/?room=browser-test&mode=instagib`);
  await a.page.screenshot({ path: new URL('menu.png', shots).pathname });
  await a.page.getByRole('button', { name: 'Enter arena', exact: true }).click();
  await wait(() => a.snap?.phase === 'waiting', 'first player warmup');
  const b = await tab('instagib');
  await b.page.getByRole('button', { name: 'Enter arena', exact: true }).click();
  await wait(() => a.snap?.phase === 'playing' && b.snap?.phase === 'playing', 'two player countdown');
  await a.page.bringToFront();
  if (await a.page.locator('#menu').isVisible()) await a.page.getByRole('button', { name: 'Resume', exact: true }).click();
  assert.equal(await a.page.evaluate(() => !!document.pointerLockElement), true, 'pointer lock');
  await a.page.screenshot({ path: new URL('first-person.png', shots).pathname });
  await a.page.mouse.down();
  await wait(() => a.snap?.players[0].score === 1, 'instagib frag');
  await a.page.mouse.up();
  await wait(() => a.snap?.players[1].alive, 'respawn');
  const startX = a.snap.players[0].x;
  await a.page.keyboard.down('KeyD'); await delay(250); await a.page.keyboard.up('KeyD');
  await wait(() => a.snap?.players[0].x < startX - 1, 'camera-relative right strafe');
  await a.page.keyboard.press('Space');
  await wait(() => a.snap?.players[0].y > 1.8, 'jump');
  await a.page.keyboard.press('KeyV');
  await a.page.locator('#stats').filter({ hasText: 'third person' }).waitFor();
  await a.page.screenshot({ path: new URL('third-person.png', shots).pathname });
  await b.page.close();
  await wait(() => a.snap?.phase === 'waiting', 'opponent disconnect');
  await a.page.close();
  const r = await tab('rocket'), s = await tab('rocket');
  await r.page.getByRole('button', { name: 'Enter arena', exact: true }).click();
  await s.page.getByRole('button', { name: 'Enter arena', exact: true }).click();
  await wait(() => r.snap?.phase === 'playing', 'rocket countdown');
  await r.page.bringToFront();
  if (await r.page.locator('#menu').isVisible()) await r.page.getByRole('button', { name: 'Resume', exact: true }).click();
  await r.page.mouse.down();
  await wait(() => r.snap?.rockets.length > 0, 'rocket launch');
  await r.page.mouse.up();
  await r.page.screenshot({ path: new URL('rocket.png', shots).pathname });
  await wait(() => r.snap?.players[0].score === 1, 'rocket frag');
  assert.deepEqual(errors, []);
  console.log('Browser passed: two players, countdown, pointer lock, strafe, jump, both cameras, instagib, rockets, respawn, disconnect.');
  console.log(`Screenshots: ${shots.pathname}`);
} finally {
  await browser.close(); await app.close();
}
