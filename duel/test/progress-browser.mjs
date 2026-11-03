import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';
import { createDuelServer } from '../server.mjs';
import { connectBuilder, generateObject } from '../aesel.mjs';
import { partialGeometry } from '../build-progress.mjs';
const live = process.argv.includes('--live'),
  butterfly = process.argv.includes('--butterfly');
const lisp = process.argv.includes('--lisp');
const artifact = process.argv.includes('--artifact'),
  renderOnly = process.argv.includes('--render-only');
const token = 'c'.repeat(64),
  app = createDuelServer({ builderToken: token });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}`;
let snowman = {
  name: 'Walking snowman',
  boxes: [
    { x: 0, y: 1, z: 0, w: 1.3, h: 1.3, d: 1.3, color: 0xffffff },
    { x: 0, y: 2, z: 0, w: 0.8, h: 0.8, d: 0.8, color: 0xffffff },
    {
      x: -0.35,
      y: 0.2,
      z: 0,
      w: 0.3,
      h: 0.4,
      d: 0.5,
      color: 0x181818,
      part: 'left-leg',
    },
    {
      x: 0.35,
      y: 0.2,
      z: 0,
      w: 0.3,
      h: 0.4,
      d: 0.5,
      color: 0x181818,
      part: 'right-leg',
    },
    { x: -0.2, y: 2.1, z: -0.41, w: 0.1, h: 0.1, d: 0.1, color: 0x181818 },
    { x: 0.2, y: 2.1, z: -0.41, w: 0.1, h: 0.1, d: 0.1, color: 0x181818 },
    { x: 0, y: 1.95, z: -0.55, w: 0.15, h: 0.15, d: 0.3, color: 0xff9933 },
  ],
  motion: { type: 'walk', axis: 'x', distance: 1, period: 8 },
};
if (lisp)
  snowman = JSON.parse(await readFile('tmp/duel/butterfly-lisp.json', 'utf8'));
if (artifact)
  snowman = JSON.parse(await readFile('tmp/duel/butterfly.json', 'utf8'));
const worker = connectBuilder({
  url: base.replace('http', 'ws') + '/builder',
  token,
  generate: live
    ? generateObject
    : async (job, signal, { onProgress }) => {
        if (snowman.source) {
          for (let i = 40; i < snowman.source.length + 40; i += 40) {
            signal.throwIfAborted();
            const code = snowman.source.slice(0, i);
            onProgress({
              phase: 'generating',
              geometry: partialGeometry(code),
              code,
            });
            await delay(200, null, { signal });
          }
          return snowman;
        }
        for (let i = 1; i <= snowman.boxes.length; i++) {
          signal.throwIfAborted();
          const shape = { ...snowman, boxes: snowman.boxes.slice(0, i) };
          onProgress({
            phase: 'generating',
            geometry: shape,
            code: JSON.stringify(shape),
          });
          await delay(550, null, { signal });
        }
        return snowman;
      },
});
const browser = await chromium.launch({
  headless: process.env.DUEL_HEADLESS === '1',
  channel: 'chrome',
  args: ['--enable-unsafe-webgpu'],
});
const errors = [];
try {
  const pages = [];
  for (let i = 0; i < 2; i++) {
    const context = await browser.newContext({
      viewport: { width: 900, height: 650 },
    });
    const p = await context.newPage();
    pages.push(p);
    p.on('console', (m) => {
      if (m.text().startsWith('Pointer capture:')) console.log(m.text());
    });
    p.on('pageerror', (e) => {
      errors.push(e.message);
      console.log('Page error:', e.message);
    });
    await p.goto(base + '/?room=progress');
    await p.waitForFunction(
      () =>
        document.querySelector('canvas').doolStats?.owner &&
        document.querySelector('canvas').doolStats.builder,
    );
  }
  const [a, b] = pages;
  await a.waitForTimeout(3100);
  await a.bringToFront();
  if (process.env.DUEL_HEADLESS !== '1' && process.platform === 'darwin')
    execFileSync('osascript', [
      '-e',
      'tell application "Google Chrome" to activate',
    ]);
  await a.locator('canvas').focus();
  await a.keyboard.press('Enter');
  await a.keyboard.type(
    butterfly
      ? 'a colorful butterfly, flying gently with flapping wings'
      : 'a walking snowman with a carrot nose, dark feet and a gentle short walk',
    { delay: 20 },
  );
  await a.keyboard.press('Enter');
  if (!renderOnly)
    try {
      await a.waitForFunction(
        () => document.pointerLockElement,
        {},
        { timeout: 5000 },
      );
    } catch (e) {
      console.log(
        'Capture state',
        await a.evaluate(() => ({
          focus: document.hasFocus(),
          active: document.activeElement?.tagName,
          activation: navigator.userActivation.isActive,
          stats: document.querySelector('canvas').doolStats,
        })),
        errors,
      );
      throw e;
    }
  await a.waitForFunction(
    () => document.querySelector('canvas').doolStats.jobs.length === 1,
  );
  await a.screenshot({ path: 'tmp/duel/build-footprint.png' });
  if (!renderOnly) {
    const old = app.rooms.get('progress').match.seats[0].player.x;
    await a.keyboard.down('KeyD');
    await a.waitForTimeout(250);
    await a.keyboard.up('KeyD');
    assert.ok(
      Math.abs(app.rooms.get('progress').match.seats[0].player.x - old) > 0.2,
      'movement continues while building',
    );
  }
  await a.waitForFunction(
    () =>
      document
        .querySelector('canvas')
        .doolStats.jobs.some((j) => j.parts > 0 && j.code),
    {},
    { timeout: live ? 170000 : 15000 },
  );
  await b.waitForFunction(() =>
    document.querySelector('canvas').doolStats.jobs.some((j) => j.parts > 0),
  );
  assert.equal(
    app.store.objects('progress').length,
    0,
    'preview is not committed geometry',
  );
  await a.screenshot({ path: 'tmp/duel/build-progress.png' });
  await a.setViewportSize({ width: 300, height: 420 });
  await a.screenshot({ path: 'tmp/duel/build-progress-tile.png' });
  await a.setViewportSize({ width: 900, height: 650 });
  await a.waitForFunction(
    () => document.querySelector('canvas').doolStats.objects.length === 1,
    {},
    { timeout: live ? 170000 : 15000 },
  );
  const object = app.store.objects('progress')[0];
  if (object.geometry.source) assert.equal(object.geometry.animated, true);
  else assert.equal(object.geometry.motion.type, butterfly ? 'fly' : 'walk');
  assert.equal(
    await a.evaluate(() => document.querySelector('canvas').doolStats.tool),
    'hands',
  );
  const player = app.rooms.get('progress').match.seats[0].player;
  player.x = object.x;
  player.z = object.z - 5;
  player.y = 1.7;
  player.yaw = 0;
  player.pitch = 0;
  player.life++;
  await a.waitForTimeout(200);
  const before = await a.evaluate(
    () => document.querySelector('canvas').doolStats.animated[0],
  );
  await a.screenshot({
    path: lisp
      ? 'tmp/duel/lisp-butterfly.png'
      : butterfly
        ? 'tmp/duel/flying-butterfly.png'
        : 'tmp/duel/walking-snowman.png',
  });
  await a.waitForTimeout(250);
  const after = await a.evaluate(
    () => document.querySelector('canvas').doolStats.animated[0],
  );
  assert.notDeepEqual(before, after);
  assert.equal(
    await b.evaluate(
      () => document.querySelector('canvas').doolStats.animated.length,
    ),
    1,
  );
  if (!renderOnly) {
    await a.bringToFront();
    if (!(await a.evaluate(() => !!document.pointerLockElement)))
      await a.locator('canvas').click();
    await a.waitForFunction(() => document.pointerLockElement);
    await a.keyboard.press('KeyV');
    await a.screenshot({ path: 'tmp/duel/hands-third.png' });
    await a.keyboard.press('KeyV');
    await a.mouse.click(450, 325);
    await a.waitForFunction(() => document.querySelector('canvas').doolStats.promptOpen);
    assert.equal(await a.evaluate(() => !!document.pointerLockElement), false);
    assert.equal(app.rooms.get('progress').match.rockets.length, 0, 'hands open prompt without firing');
    await a.keyboard.press('Escape');
  }
  // Only the local controlled builder is used for the cancellation check.
  if (!live) {
    await a.keyboard.press('Enter');
    await a.keyboard.type('add a hat');
    await a.keyboard.press('Enter');
    await a.waitForFunction(
      () => document.querySelector('canvas').doolStats.jobs.length === 1,
    );
    await a.keyboard.press('Escape');
    await a.waitForFunction(
      () => document.querySelector('canvas').doolStats.jobs.length === 0,
    );
    assert.equal(app.store.objects('progress').length, 1);
    assert.equal(app.store.objects('progress')[0].version, 0);
  }
  assert.deepEqual(errors, []);
  console.log(
    `Build progress passed (${live ? 'remote Aesel' : 'controlled builder'}): footprint, streamed source/geometry, ${renderOnly ? 'rendering' : 'movement'}, shared animation${live ? '' : ', cancel'}.`,
  );
} finally {
  await browser.close();
  worker.close();
  await app.close();
}
