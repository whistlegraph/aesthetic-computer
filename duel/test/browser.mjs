import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdir } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import { createDuelServer } from '../server.mjs';

const app = createDuelServer({ prefix: '/dool' });
app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}/dool/`;
// Real pointer capture needs a visible Chrome window on macOS.
const browser = await chromium.launch({ headless: process.env.DUEL_HEADLESS === '1',
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
async function focus(page) {
  await page.bringToFront();
  if (process.platform === 'darwin' && process.env.DUEL_HEADLESS !== '1') {
    execFileSync('osascript', ['-e', 'tell application "Google Chrome" to activate']);
  }
}

try {
  const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
  // Keep the user's desktop pointer from steering the visible test window.
  // Relative aim below still goes through the real client input listener.
  await context.addInitScript(() => document.addEventListener('mousemove', e => {
    if (e.isTrusted) e.stopImmediatePropagation();
  }, true));
  async function tab() {
    const page = await context.newPage();
    const state = { page, snap: null };
    page.on('pageerror', e => errors.push(e.message));
    page.on('websocket', socket => socket.on('framereceived', ({ payload }) => {
      const data = JSON.parse(payload.toString());
      if (data.type === 'snapshot') state.snap = data;
    }));
    await page.goto(`${base}?room=browser-test`);
    await page.locator('canvas').waitFor({ state: 'visible' });
    return state;
  }
  const a = await tab();
  await wait(() => a.snap?.phase === 'waiting', 'automatic room entry');
  assert.equal(await a.page.locator('form,button,input,select,div,span').count(), 0, 'no DOM interface');
  await a.page.screenshot({ path: new URL('entry.png', shots).pathname });
  await focus(a.page);
  await a.page.locator('canvas').click();
  await a.page.waitForFunction(() => !!document.pointerLockElement);
  assert.equal(await a.page.evaluate(() => !!document.pointerLockElement), true, 'pointer lock');
  await a.page.dispatchEvent('canvas', 'mousedown', { button: 0, bubbles: true });
  await wait(() => a.snap?.rockets.length > 0, 'warmup firing');
  await a.page.dispatchEvent('canvas', 'mouseup', { button: 0, bubbles: true });
  const b = await tab();
  await wait(() => a.snap?.phase === 'playing' && b.snap?.phase === 'playing', 'two player countdown');
  await focus(a.page);
  if (!await a.page.evaluate(() => !!document.pointerLockElement)) await a.page.locator('canvas').click();
  await a.page.waitForFunction(() => !!document.pointerLockElement);
  // Pointer locking can warp the native cursor on macOS. Send explicit relative
  // aim through the game's input listener, keeping the combat test repeatable.
  const aim = a.snap.players[0];
  const yawDelta = ((aim.yaw + 180) % 360) - 180;
  await a.page.dispatchEvent('canvas', 'mousemove', {
    movementX: yawDelta / 0.12, movementY: aim.pitch / 0.12, bubbles: true,
  });
  await wait(() => Math.abs(a.snap.players[0].pitch) < 0.3 &&
    Math.min(a.snap.players[0].yaw, 360 - a.snap.players[0].yaw) < 0.3, 'aim settles');
  await a.page.screenshot({ path: new URL('first-person.png', shots).pathname });
  await a.page.dispatchEvent('canvas', 'mousedown', { button: 0, bubbles: true });
  await wait(() => a.snap?.rockets.length > 0, 'rocket launch');
  await a.page.dispatchEvent('canvas', 'mouseup', { button: 0, bubbles: true });
  await a.page.screenshot({ path: new URL('rocket.png', shots).pathname });
  await wait(() => a.snap?.players[0].score === 1, 'rocket frag');
  await wait(() => a.snap?.players[1].alive, 'respawn');
  await focus(a.page);
  if (!await a.page.evaluate(() => !!document.pointerLockElement)) await a.page.locator('canvas').click();
  await a.page.waitForFunction(() => !!document.pointerLockElement);
  const start = { ...a.snap.players[0] };
  await a.page.keyboard.down('KeyD');
  try {
    await wait(() => Math.hypot(a.snap.players[0].x - start.x, a.snap.players[0].z - start.z) > 1, 'right strafe');
  } finally { await a.page.keyboard.up('KeyD'); }
  await a.page.keyboard.press('Space');
  await wait(() => a.snap?.players[0].y > 1.8, 'jump');
  await a.page.keyboard.press('KeyV');
  await a.page.screenshot({ path: new URL('third-person.png', shots).pathname });
  await b.page.close();
  await wait(() => a.snap?.phase === 'waiting', 'opponent disconnect');
  await a.page.keyboard.press('Escape');
  await a.page.reload();
  await wait(() => a.snap?.phase === 'waiting', 'reload');
  assert.equal(await a.page.locator('form,button,input,select,div,span').count(), 0);
  for (const [width, height] of [[390, 844], [1600, 700], [1280, 800]]) {
    await a.page.setViewportSize({ width, height });
    await a.page.waitForFunction(({ width, height }) => {
      const canvas = document.getElementById('game');
      const scale = Math.max(1, Math.floor(Math.min(width / 480, height / 270)));
      return canvas.clientWidth === width && canvas.clientHeight === height &&
        canvas.width === Math.ceil(width / scale) && canvas.height === Math.ceil(height / scale) &&
        getComputedStyle(canvas).imageRendering === 'pixelated';
    }, { width, height });
    await a.page.screenshot({ path: new URL(`resize-${width}x${height}.png`, shots).pathname });
  }
  assert.deepEqual(errors, []);
  console.log('Browser passed: two players, countdown, pointer lock, strafe, jump, both cameras, rockets, respawn, disconnect, canvas-only interface.');
  console.log(`Screenshots: ${shots.pathname}`);
} catch (error) {
  console.error(error.message);
  throw error;
} finally {
  await browser.close(); await app.close();
}
