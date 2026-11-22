import assert from 'node:assert/strict';
import { once } from 'node:events';
import { chromium } from 'playwright';
import { existsSync } from 'node:fs';
import { connectBuilder } from '../aesel.mjs';
import { createRaxServer } from '../server.mjs';
import { CAR_PAD, CAR_Z } from '../elevator.mjs';
import { kioskPlan } from '../kiosks-plan.mjs';

const token = 'b'.repeat(64);
const app = createRaxServer({ guests: true, builderToken: token, listHandles: async () => [] });
await app.ready;
await app.store.commit({ room: 'lobby', objectId: 'poker', item: 'poke', owner: 'fixture', baseVersion: -1, x: 6, z: -2 },
  { name: 'poker', boxes: [{ x: 0, y: 0.2, z: 0, w: 0.2, h: 0.4, d: 0.2, color: 0xff44dd }] });
await app.store.commit({ room: 'lobby', objectId: 'cube', owner: 'fixture', baseVersion: -1, x: 0, z: -6 },
  { name: 'cube', boxes: [{ x: 0, y: 1, z: 0, w: 1, h: 2, d: 1, color: 0x2299ff }] });
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const worker = connectBuilder({
  url: 'ws://127.0.0.1:' + app.server.address().port + '/builder', token,
  generate: async () => ({name:'wheel fixture', boxes:[{x:0,y:0.5,z:0,w:1,h:1,d:1,color:0x112233}]}),
});
const browser = await chromium.launch({ ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}), headless: true, args: ['--enable-unsafe-webgpu'] });
try {
  const page = await browser.newPage({ hasTouch: true, viewport: { width: 390, height: 844 } });
  const cdp = await page.context().newCDPSession(page);
  const touch = (type, touchPoints) => cdp.send('Input.dispatchTouchEvent', { type, touchPoints });
  const left = { x: 166, y: 410, id: 1 }, right = { x: 222, y: 430, id: 2 };
  page.setDefaultTimeout(9000);
  const errors = [], panelPresses = [], buildRequests = [];
  page.on('websocket', ws => ws.on('framesent', ({ payload }) => {
    const event = JSON.parse(payload);
    if (event.type === 'prompt') buildRequests.push(event);
    if (event.type === 'panel') panelPresses.push(event);
  }));
  page.on('websocket', ws => ws.on('framereceived', ({ payload }) => {
    if (JSON.parse(payload).events?.some(e => e.type === 'poke')) void page.evaluate(() => { window.testPoked = true; }).catch(() => {});
  }));
  page.on('pageerror', e => errors.push(e.message));
  await page.addInitScript(() => {
    window.testPad = { connected: true, id: 'test', index: 0, axes: [0, 0, 0, 0],
      buttons: Array.from({ length: 18 }, () => ({ pressed: false, value: 0 })) };
    Object.defineProperty(navigator, 'getGamepads', { value: () => [testPad] });
  });
  await page.route('**/kidlisp-source?*', route => route.fulfill({ json: { source: '(wipe 0 220 255) (if down (wipe 255 0 0))' } }));
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
  await button(5);
  await page.waitForFunction(id => raxDebug().dock?.id === id && raxDebug().dock.t === 1, 'panel:' + kiosk.id);
  assert.equal((await sample()).lock, null);
  await exerciseCursor(true);
  await touch('touchStart', [left]);
  await touch('touchStart', [left, right]);
  await touch('touchMove', [{ ...left, x: 146 }, { ...right, x: 247 }]);
  await touch('touchMove', [right]);
  await touch('touchMove', [{ ...right, y: 445 }]);
  await touch('touchEnd', []);
  await page.waitForTimeout(100);
  for (const finger of [1, 2]) {
    const events = panelPresses.filter(e => e.finger === finger && e.id === kiosk.id);
    assert.equal(events[0]?.name, 'touch');
    assert.ok(events.some(e => e.name === 'draw' && e.x !== events[0].x));
    assert.equal(events.at(-1)?.name, 'lift');
  }
  await button(11);
  await page.waitForFunction(() => {
    const { screenFrame:f, hudRegions:r } = document.querySelector('canvas').raxStats;
    return f && r?.some(b => b.id === 'avatar') && r.some(b => b.id === 'debug-chart') &&
      r.every(b => f.x + f.w <= b.x || b.x + b.w <= f.x || f.y + f.h <= b.y || b.y + b.h <= f.y);
  });
  if (process.env.RAX_SCREEN_CAPTURE) await page.screenshot({path:process.env.RAX_SCREEN_CAPTURE});
  await button(11);
  const before = await sample();
  const home = await page.evaluate(() => document.querySelector('canvas').raxStats.camera);
  await button(10);
  await page.waitForFunction(z => Math.abs(raxDebug().fov / z - 2) < 0.01, before.fov);
  await page.evaluate(() => { testPad.buttons[6].value = 1; });
  await page.waitForFunction(z => Math.abs(raxDebug().fov / z - 5) < 0.01, before.fov);
  await page.evaluate(() => { testPad.axes[0] = 0.8; testPad.axes[1] = -0.5; });
  await page.waitForFunction(home => {
    const camera = document.querySelector('canvas').raxStats.camera;
    return camera.yaw < home.yaw - 1 && camera.pitch > home.pitch + 1;
  }, home);
  await page.evaluate(() => { testPad.axes[0] = testPad.axes[1] = 0; });
  await page.waitForFunction(home => {
    const camera = document.querySelector('canvas').raxStats.camera;
    return Math.abs(camera.yaw - home.yaw) < 0.01 && Math.abs(camera.pitch - home.pitch) < 0.01;
  }, home);
  const recentered = await sample();
  assert.deepEqual(recentered.dock.cursor, before.dock.cursor);
  assert.ok(Math.hypot(recentered.player.x - before.player.x, recentered.player.z - before.player.z) < 0.05, 'looking stays in place');
  const { cursor, hot } = (await sample()).dock;
  assert.equal(hot.id, kiosk.id);
  assert.equal(hot.x, cursor.x);
  assert.equal(hot.y, cursor.y);
  await button(7);
  const press = panelPresses.findLast(e => e.name === 'touch');
  assert.deepEqual([press?.id, press?.x, press?.y], [kiosk.id, Math.round(cursor.x), Math.round(cursor.y)]);
  await button(10);
  await page.waitForFunction(z => Math.abs(raxDebug().fov / z - 2.5) < 0.01, before.fov);
  await page.evaluate(() => { testPad.buttons[6].value = 0; });
  await button(10);
  await button(1);
  await page.waitForFunction(() => !raxDebug().dock && !raxDebug().lock && raxDebug().fov === 1);

  const cube = app.store.get('lobby', 'cube');
  await aim('cube', cube.x, cube.z + 2.5, 180);
  await button(5);
  await page.waitForFunction(() => raxDebug().lock === 'cube');
  await exerciseCursor(false, { x: 1, y: 1 });
  for (let n = 0; n < 12 && !(await sample()).dock; n++) await button(15);
  await page.waitForFunction(() => !!raxDebug().dock && !raxDebug().lock);
  await button(5);
  await page.waitForFunction(() => !raxDebug().dock && !raxDebug().lock);

  await aim('screen:lobby-bop', -5, 4.6, 270);
  await button(5);
  await page.waitForFunction(() => raxDebug().dock?.id === 'screen:lobby-bop' && raxDebug().dock.t === 1);
  await exerciseCursor(true, { x: 159.99, y: 89.99 });
  await page.waitForFunction(() => raxDebug().dock.pixel?.[1] === 220);
  await touch('touchStart', [left, right]);
  await page.waitForFunction(() => raxDebug().dock.pixel?.[0] === 255);
  await touch('touchMove', [right]);
  await page.waitForTimeout(150);
  assert.equal((await sample()).dock.pixel[0], 255, 'one finger is still holding KidLisp down');
  await touch('touchEnd', []);
  await page.waitForFunction(() => raxDebug().dock.pixel?.[1] === 220);
  await page.evaluate(() => { testPad.buttons[7].pressed = true; });
  await page.waitForFunction(() => raxDebug().dock.pixel?.[0] === 255 && raxDebug().dock.pixel?.[1] === 0);
  await page.evaluate(() => { testPad.buttons[7].pressed = false; });
  await page.waitForFunction(() => raxDebug().dock.pixel?.[1] === 220);
  await button(1);
  await page.waitForFunction(() => !raxDebug().dock);
  const match = app.rooms.get('lobby').match;
  match.items.get('poker').holder = match.seats.find(Boolean).player.seat;
  await page.waitForFunction(() => raxDebug().holding === 'poker');
  await button(7);
  await page.waitForFunction(() => window.testPoked);
  await button(2);
  await button(4);
  await button(17);
  assert.ok(await page.evaluate(() => {
    const s = document.querySelector('canvas').raxStats;
    return !s.radial && !s.promptOpen;
  }), 'bumper and rear button leave the build menu closed');
  async function draft() {
    await page.evaluate(() => { testPad.buttons[4].pressed = true; });
    await page.waitForFunction(() => document.querySelector('canvas').raxStats.radial?.bumper);
    await page.evaluate(() => { testPad.axes[3] = -1; });
    await button(0);
    await button(0);
    await page.waitForFunction(() => document.querySelector('canvas').raxStats.radial?.ready);
  }
  await draft();
  await button(2);
  assert.equal(buildRequests.length, 0);
  await page.evaluate(() => { testPad.buttons[4].pressed = false; });
  await page.waitForFunction(() => !document.querySelector('canvas').raxStats.radial);
  assert.equal(buildRequests.length, 0, 'release over a slice discards the draft');
  await draft();
  await page.evaluate(() => { testPad.axes[3] = 0; });
  await page.waitForFunction(() => document.querySelector('canvas').raxStats.radial?.centered);
  await page.evaluate(() => { testPad.buttons[4].pressed = false; });
  await page.waitForFunction(() => !document.querySelector('canvas').raxStats.radial);
  assert.equal(buildRequests.length, 1, 'release over Build sends one request');
  await aim('panel:car-pad', CAR_PAD.x, CAR_Z, CAR_PAD.yaw);
  await button(5);
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
  await button(7);
  await page.waitForFunction(() => raxDebug().room === 'basement' && !raxDebug().dock);
  assert.deepEqual(errors, []);
  console.log('Two-finger kiosk drags, KidLisp holds, focused cursor, zoom, RT use and elevator ride passed.');
} finally {
  worker.close();
  await browser.close();
  await app.close();
}
