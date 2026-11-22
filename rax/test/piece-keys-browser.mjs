// ⌨️ Stepped in to the yard's notepat, the keyboard plays it; Escape comes out
// and the keys are rax's again. node rax/test/piece-keys-browser.mjs
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { existsSync } from 'node:fs';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';
import { kioskPlan } from '../kiosks-plan.mjs';

const dir = await mkdtemp(join(tmpdir(), 'rax-keys-browser-'));
const app = createRaxServer({ guests: true, stateDir: dir, listHandles: async () => [] });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const browser = await chromium.launch({ ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}), headless: true, args: ['--enable-unsafe-webgpu'] });
try {
  const page = await browser.newPage({ viewport: { width: 800, height: 500 } });
  page.setDefaultTimeout(15000);
  const errors = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.goto(`http://127.0.0.1:${app.server.address().port}/?debug&room=lobby`);
  await page.waitForFunction(() => window.raxDebug?.().player && raxDebug().panels.includes('yard-notepat'));
  const sample = () => page.evaluate(() => raxDebug());
  const until = (what, f, arg) => page.waitForFunction(f, arg).catch(async (e) => { const d = await sample(); throw Error(`${what}: ${e.message.split('\n')[0]} · ${JSON.stringify({ focus: d.focus, lock: d.lock, dock: d.dock && { id: d.dock.id, t: d.dock.t, way: d.dock.way }, inside: d.inside, player: d.player })}`); });
  const board = kioskPlan('lobby').find((k) => k.id === 'yard-notepat');
  const body = app.rooms.get('lobby').match.seats.find(Boolean).player;
  // Stood before the board, looking at it (whichever side it faces).
  let stood = null;
  for (const side of [1]) { // (the board stands against the south side and faces the tower)
    const z = board.z + side * 3;
    Object.assign(body, { x: board.x, z, yaw: 0, y: 1.7, pitch: 0 });
    body.life++;
    await page.waitForTimeout(1500);
    console.log('stood', JSON.stringify((await sample()).player), 'asked', board.x, z, 'server', body.x.toFixed(2), body.z.toFixed(2));
    for (const yaw of [0, 180]) {
      await page.evaluate((yaw) => raxLook({ yaw, pitch: 0 }), yaw);
      if (await page.waitForFunction(() => raxDebug().focus === 'panel:yard-notepat', null, { timeout: 2000 }).then(() => true, () => false)) { stood = { side, yaw }; break; }
    }
    if (stood) break;
  }
  assert.equal((await sample()).focus, 'panel:yard-notepat', `aimed at the board: ${JSON.stringify(await sample().then((d) => ({ focus: d.focus, player: d.player, yaw: d.yaw })))}`);
  // The lock control twice: locked on, then stepped in.
  for (let n = 0; n < 4 && !(await sample()).dock; n++) { await page.keyboard.press('KeyZ'); await page.waitForTimeout(350); }
  await until('stepped in', () => raxDebug().dock?.id === 'panel:yard-notepat' && raxDebug().dock.t === 1);
  await until('piece has the keyboard', () => raxDebug().inside?.id === 'yard-notepat');
  const at = (await sample()).player, quiet = (await sample()).inside.ink;
  // A note held: the piece has the key (its picture shows it), and the body has not taken a step for it.
  await page.keyboard.down('KeyA');
  await until('note held and shown', (quiet) => raxDebug().inside?.keys.includes('KeyA') && raxDebug().inside.ink !== quiet, quiet);
  await page.keyboard.down('KeyZ'); // (notepat's own key here, not the way out)
  await page.waitForTimeout(400);
  const held = await sample();
  assert.ok(held.dock && held.inside.keys.includes('KeyZ'), 'z is the piece\'s while inside');
  assert.ok(Math.hypot(held.player.x - at.x, held.player.z - at.z) < 0.05, 'the body stood still');
  await page.keyboard.up('KeyZ');
  await page.keyboard.up('KeyA');
  await until('keys let go', () => raxDebug().inside?.keys.length === 0);
  // Escape is rax's: out, and a held note is let go on the way.
  await page.keyboard.down('KeyC');
  await until('c held', () => raxDebug().inside?.keys.includes('KeyC'));
  await page.keyboard.press('Escape');
  await until('out again', () => !raxDebug().dock && !raxDebug().inside);
  await page.keyboard.up('KeyC');
  // Out here A is a step to the side again.
  await page.keyboard.down('KeyA');
  await until('a step to the side', (at) => { const p = raxDebug().player; return Math.hypot(p.x - at.x, p.z - at.z) > 0.3; }, at);
  await page.keyboard.up('KeyA');
  assert.deepEqual(errors, []);
  console.log('piece keys browser ok');
} finally {
  await browser.close();
  await app.close();
  await rm(dir, { recursive: true, force: true });
}
