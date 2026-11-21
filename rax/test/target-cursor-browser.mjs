import assert from 'node:assert/strict';
import { once } from 'node:events';
import { chromium } from 'playwright';
import { existsSync } from 'node:fs';
import { createRaxServer } from '../server.mjs';
import { CAR_PAD, CAR_Z } from '../elevator.mjs';
import { kioskPlan } from '../kiosks-plan.mjs';

const app = createRaxServer({ guests: true, listHandles: async () => [] });
await app.ready;
await app.store.commit({ room: 'lobby', objectId: 'cube', owner: 'fixture', baseVersion: -1, x: 0, z: -6 },
  { name: 'cube', boxes: [{ x: 0, y: 1, z: 0, w: 1, h: 2, d: 1, color: 0x2299ff }] });
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const browser = await chromium.launch({ ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}), headless: true, args: ['--enable-unsafe-webgpu'] });
try {
  const page = await browser.newPage();
  page.setDefaultTimeout(9000);
  const errors = [], panelPresses = [];
  page.on('websocket', ws => ws.on('framesent', ({ payload }) => {
    const event = JSON.parse(payload);
    if (event.type === 'panel' && event.name === 'touch') panelPresses.push(event);
  }));
  page.on('pageerror', e => errors.push(e.message));
  await page.addInitScript(() => {
    window.testPad = { connected: true, id: 'test', index: 0, axes: [0, 0, 0, 0],
      buttons: Array.from({ length: 17 }, () => ({ pressed: false, value: 0 })) };
    Object.defineProperty(navigator, 'getGamepads', { value: () => [testPad] });
  });
  await page.goto(`http://127.0.0.1:${app.server.address().port}/?debug&room=lobby`);
  await page.waitForFunction(() => window.raxDebug?.().player && raxDebug().panels.includes('yard-0'));
  const sample = () => page.evaluate(() => raxDebug());
  async function button(index) {
    await page.evaluate(async i => {
      testPad.buttons[i].pressed = true;
      const frame = () => new Promise(requestAnimationFrame);
      await frame(); await frame();
      testPad.buttons[i].pressed = false;
      await frame(); await frame();
    }, index);
  }
  async function aim(target, x, z, yaw) {
    const body = app.rooms.get('lobby').match.seats.find(Boolean).player;
    Object.assign(body, { x, z, yaw, y: 1.7, pitch: 0 });
    body.life++;
    await page.waitForFunction(({ x, z }) => {
      const p = raxDebug().player;
      return p && Math.hypot(p.x - x, p.z - z) < 0.2;
    }, { x, z });
    await page.evaluate(yaw => raxLook({ yaw, pitch: 0 }), yaw);
    await page.waitForFunction(id => raxDebug().focus === id, target);
  }
  async function exerciseCursor(screen, edge = null) {
    const cursor = () => page.evaluate(s => s ? raxDebug().dock.cursor : raxDebug().lockCursor, screen);
    const start = await cursor();
    const camera = screen ? await page.evaluate(() => document.querySelector('canvas').raxStats.camera) : null;
    await page.evaluate(() => { testPad.axes[2] = 0.8; testPad.axes[3] = -0.7; });
    await page.waitForFunction(({ screen, start }) => {
      const p = screen ? raxDebug().dock.cursor : raxDebug().lockCursor;
      return p.x > start.x * 1.3 && p.y < start.y * 0.8;
    }, { screen, start });
    await page.evaluate(() => { testPad.axes[2] = testPad.axes[3] = 0; });
    const parked = await cursor();
    if (camera) assert.deepEqual(await page.evaluate(() => document.querySelector('canvas').raxStats.camera), camera, 'the stick only moves the reticle');
    await page.waitForTimeout(400);
    assert.deepEqual(await cursor(), parked, 'releasing the stick keeps the reticle in place');
    if (edge) {
      await page.evaluate(() => { testPad.axes[2] = 1; testPad.axes[3] = 0.8; });
      await page.waitForFunction(({ screen, edge }) => {
        const p = screen ? raxDebug().dock.cursor : raxDebug().lockCursor;
        return p.x === edge.x && p.y === edge.y;
      }, { screen, edge });
      await page.evaluate(() => { testPad.axes[2] = testPad.axes[3] = 0; });
      assert.deepEqual(await cursor(), edge);
    }
  }

  const kiosk = kioskPlan('lobby')[0];
  await aim('panel:' + kiosk.id, kiosk.x, kiosk.z - 1.8, 0);
  await button(4);
  await page.waitForFunction(id => raxDebug().dock?.id === id && raxDebug().dock.t === 1, 'panel:' + kiosk.id);
  assert.equal((await sample()).lock, null);
  await exerciseCursor(true);
  const { cursor, hot } = (await sample()).dock;
  assert.equal(hot.id, kiosk.id);
  assert.equal(hot.x, cursor.x);
  assert.equal(hot.y, cursor.y);
  await button(0);
  const press = panelPresses.at(-1);
  assert.deepEqual([press?.id, press?.x, press?.y], [kiosk.id, Math.round(cursor.x), Math.round(cursor.y)]);
  await button(1);
  await page.waitForFunction(() => !raxDebug().dock && !raxDebug().lock);

  const cube = app.store.get('lobby', 'cube');
  await aim('cube', cube.x, cube.z + 2.5, 180);
  await button(4);
  await page.waitForFunction(() => raxDebug().lock === 'cube');
  await exerciseCursor(false, { x: 1, y: 1 });
  await button(15);
  await page.waitForFunction(() => !!raxDebug().dock && !raxDebug().lock);
  await button(4);
  await page.waitForFunction(() => !raxDebug().dock && !raxDebug().lock);

  await aim('screen:lobby-bop', -5, 4.6, 270);
  await button(4);
  await page.waitForFunction(() => raxDebug().dock?.id === 'screen:lobby-bop' && raxDebug().dock.t === 1);
  await exerciseCursor(true, { x: 159.99, y: 89.99 });
  await button(1);
  await page.waitForFunction(() => !raxDebug().dock);
  await aim('panel:car-pad', CAR_PAD.x, CAR_Z, CAR_PAD.yaw);
  await button(4);
  await page.waitForFunction(() => raxDebug().dock?.id === 'panel:car-pad' && raxDebug().dock.t === 1);
  assert.equal((await sample()).lock, null);
  // Move off the up arrow, park on down, and take the elevator to B1.
  await page.evaluate(() => { testPad.axes[3] = 0.6; });
  await page.waitForFunction(() => raxDebug().dock.hot?.y >= 70);
  await page.evaluate(() => { testPad.axes[3] = 0; });
  const arrow = (await sample()).dock.hot;
  assert.equal(arrow.id, 'car-pad');
  assert.ok(arrow.y < 93);
  await page.waitForTimeout(250);
  assert.deepEqual((await sample()).dock.hot, arrow);
  await button(0);
  await page.waitForFunction(() => raxDebug().room === 'basement' && !raxDebug().dock);
  assert.deepEqual(errors, []);
  console.log('Single-press screen entry, cycling, exit, and persistent right-stick reticles passed on kiosk, screen, object, and elevator keypad. Rode to B1.');
} finally {
  await browser.close();
  await app.close();
}
