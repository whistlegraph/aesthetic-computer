import assert from 'node:assert/strict';
import { once } from 'node:events';
import { existsSync } from 'node:fs';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';
import { kioskPlan } from '../kiosks-plan.mjs';
import { CAR_PAD } from '../elevator.mjs';
import { distantBoxes } from '../objects.mjs';
import { SPAN } from '../park.mjs';

const app = createRaxServer({ guests: true, listHandles: async () => [] });
await app.ready;
for (const [id, room] of [['west-build', 'lobby'], ['east-build', 'e1n0'], ['far-build', 'e7n0'], ['loose-item', 'e1n0']]) {
  await app.store.commit({ room, objectId: id, item: id === 'loose-item' ? 'poke' : null, owner: 'fixture', baseVersion: -1, x: 2, z: -2 },
    { name: id, boxes: [{ x: 0, y: 1, z: 0, w: 1, h: 2, d: 1, color: 0x22ffcc }] });
}
await app.store.addScreen('e1n0', { id: 'east-screen', code: 'eee', x: 3, z: -3, yaw: 0 });
await app.store.addScreen('e4n0', { id: 'zoom-screen', code: 'eee', x: -2.4, z: 3, yaw: 270 });
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const browser = await chromium.launch({ headless: true, args: ['--enable-unsafe-webgpu'],
  ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}) });
let unblock;
try {
  const world = async room => (await fetch(`http://127.0.0.1:${app.server.address().port}/world?room=${room}`)).json();
  const farObject = scene => scene.blocks.find(b => b.id === 'e7n0').objects[0];
  const distant = farObject(await world('lobby')), nearer = farObject(await world('e2n0'));
  assert.equal(distant.coarse, true);
  assert.ok(!nearer.coarse);
  assert.deepEqual(distant.geometry.boxes, distantBoxes(nearer.geometry));
  const page = await browser.newPage();
  page.setDefaultTimeout(15000);
  await page.addInitScript(() => {
    const pad = { id: 'zoom test', index: 0, connected: true, axes: [0, 0, 0, 0],
      buttons: Array.from({ length: 17 }, () => ({ value: 0, pressed: false })) };
    Object.defineProperty(navigator, 'getGamepads', { value: () => [pad] });
    window.squeezeLT = pressure => { pad.buttons[6].value = pressure; };
  });
  await page.route('**/kidlisp-source?*', route => route.fulfill({ json: { source: '(wipe cyan)' } }));
  const blocked = new Promise(r => { unblock = r; });
  let pending = false;
  await page.route('**/world?*', async route => {
    if (new URL(route.request().url()).searchParams.get('room') === 'e1n0') {
      pending = true;
      await blocked;
    }
    await route.continue().catch(() => {});
  });
  await page.goto(`http://127.0.0.1:${app.server.address().port}/?debug&room=lobby`);
  await page.waitForFunction(() => {
    const s = document.querySelector('canvas')?.raxStats?.scenery;
    return window.raxDebug?.().player && s?.panels.some(p => p.id === 'yard-notepat') &&
      s.screens.some(p => p.id === 'e1n0/east-screen') && s.objects.some(o => o.id === 'far-build');
  });
  const body = app.rooms.get('lobby').match.seats.find(Boolean).player;
  Object.assign(body, { x: 7, z: 3, y: 1.7, yaw: 90, pitch: 0 });
  body.life++;
  await page.waitForFunction(() => raxDebug().player?.x > 6.8);
  await page.evaluate(() => raxLook({ yaw: 90, pitch: 0 }));
  async function checkZoom(pressure, cols, model, panelCols) {
    await page.evaluate(p => squeezeLT(p), pressure);
    await page.waitForFunction(({ cols, model, panelCols }) => {
      const s = document.querySelector('canvas').raxStats.scenery;
      const proxy = s.objects.find(o => o.id === 'far-build')?.proxyScale;
      return s.screens.find(o => o.id === 'e4n0/zoom-screen')?.cols === cols &&
        s.panels.find(p => p.id === 'yard-notepat')?.cols === panelCols &&
        (model === 'mixed' ? proxy > 0 && proxy < 1 : proxy === model);
    }, { cols, model, panelCols });
  }
  await checkZoom(0, 16, 1, 64);
  await checkZoom(0.5, 32, 'mixed', 128);
  await checkZoom(1, 64, 0, 128);
  await checkZoom(0, 16, 1, 64);
  console.log('LT at rest, half and full gave the 55 m screen 16, 32 and 64 columns and resolved the distant build and kiosk. Release restored the original detail.');
  const walker = app.rooms.get('lobby').match.seats.find(Boolean).player;
  Object.assign(walker, { z: -2, yaw: 270 });
  walker.life++;
  await page.waitForFunction(() => Math.abs(raxDebug().player.z + 2) < 0.1);
  await page.evaluate(() => raxLook({ yaw: 270, pitch: 0 }));
  const panel = kioskPlan('lobby').find(p => p.id === 'yard-notepat');
  const expected = [
    ['panels', panel.id, panel.x], ['panels', 'car-pad', CAR_PAD.x],
    ['screens', 'lobby/lobby-bop', -7.2], ['screens', 'e1n0/east-screen', 3 + SPAN],
    ...[['west-build', 'lobby', 0], ['east-build', 'e1n0', 1], ['far-build', 'e7n0', 7], ['loose-item', 'e1n0', 1]]
      .map(([id, room, i]) => ['objects', id, app.store.get(room, id).x + SPAN * i]),
  ];
  await page.evaluate(({ expected, span }) => {
    window.crossing = { rooms: [], errors: [] };
    function check() {
      const room = raxDebug().room, s = document.querySelector('canvas').raxStats.scenery;
      if (crossing.rooms.at(-1) !== room) crossing.rooms.push(room);
      for (const [kind, id, x] of expected) {
        const matches = s[kind].filter(row => row.id === id);
        if (matches.length !== 1 || Math.abs(matches[0].x + (room === 'lobby' ? 0 : span) - x) > 0.001)
          crossing.errors.push({ room, id, positions: matches.map(o => o.x) });
      }
      crossing.frame = requestAnimationFrame(check);
    }
    check();
  }, { expected, span: SPAN });
  await page.keyboard.down('KeyS');
  await page.waitForFunction(() => raxDebug().room === 'e1n0' && raxDebug().player?.x > -6);
  await page.keyboard.up('KeyS');
  assert.ok(pending, 'next world request is still outstanding');
  await page.keyboard.down('KeyW');
  await page.waitForFunction(() => raxDebug().room === 'lobby' && raxDebug().player?.x < 7.5);
  await page.keyboard.up('KeyW');
  const result = await page.evaluate(() => { cancelAnimationFrame(crossing.frame); return crossing; });
  assert.deepEqual(result.rooms, ['lobby', 'e1n0', 'lobby']);
  assert.deepEqual(result.errors.slice(0, 3), []);
  console.log('Kiosks, standing screens, nearby builds and distant silhouettes stayed in place on every sampled frame across both crossings.');
} finally {
  unblock();
  await browser.close();
  await app.close();
}
