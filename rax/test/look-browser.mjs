// 👀 A look at the stage, in pictures: a local rax, one signed-in body, and a
// screenshot each second while it walks to the gun, the heads come, and it is
// bitten. For whoever changes the HUD or the heads and cannot see a screen.
//   node rax/test/look-browser.mjs <out-dir> [seconds]
import { once } from 'node:events';
import { existsSync } from 'node:fs';
import { mkdtemp, mkdir, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';

const out = process.argv[2], seconds = Number(process.argv[3] || 26);
if (!out) { console.error('look-browser.mjs <out-dir> [seconds]'); process.exit(1); }
await mkdir(out, { recursive: true });
const dir = await mkdtemp(join(tmpdir(), 'rax-look-'));
const app = createRaxServer({ guests: true, stateDir: dir, listHandles: async () => ['jeffrey'],
  verifyIdentity: async (token) => ({ handle: token, sub: `account:${token}` }) });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const browser = await chromium.launch({ ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}), headless: true, args: ['--enable-unsafe-webgpu'] });
try {
  const errors = [];
  const page = await (await browser.newContext({ viewport: { width: 960, height: 600 } })).newPage();
  page.on('pageerror', (e) => errors.push(e.message));
  page.on('console', (m) => { if (m.type() === 'error' || /KidLisp/i.test(m.text())) errors.push(m.type() + ': ' + m.text().slice(0, 900)); });
  await page.addInitScript(() => { window.acTOKEN = 'jeffrey'; });
  await page.goto(`http://127.0.0.1:${app.server.address().port}/?debug`);
  const state = () => page.evaluate(() => window.rax?.state());
  for (const end = Date.now() + 20000; Date.now() < end; await delay(100)) { const s = await state(); if (s?.connected && s.you.seated && s.you.x !== null) break; }
  await page.mouse.click(480, 300); // (the first click: sound and the mouse)
  const shot = async (name) => { await page.screenshot({ path: join(out, `${name}.png`) }); };
  await shot('00-arrived');
  // (LOOK=office: off the south edge on to the fire escape, in at the door, and a look round the office.)
  if (process.env.LOOK === 'office') {
    await page.evaluate(() => window.rax.walkTo({ x: 0, z: -7 }, { within: 0.3, seconds: 8 })).catch(() => {});
    await page.evaluate(() => { window.rax.look({ yaw: 180 }); return window.rax.move({ fwd: 0.3, seconds: 3.5 }); });
    await delay(800); await shot('01-landing');
    await page.evaluate(() => { window.rax.look({ yaw: 0 }); return window.rax.move({ fwd: 0.6, seconds: 2.5 }); });
    await shot('02-inside');
    await page.evaluate(() => window.rax.look({ yaw: 60, pitch: -5 })); await delay(600); await shot('03-inside-east');
    console.log(JSON.stringify({ errors: [...new Set(errors)].slice(0, 8), you: (await state())?.you }, null, 1));
    await browser.close(); await app.close(); process.exit(0);
  }
  // (LOOK_YAW=270 LOOK_PITCH=10: stand and look that way instead of walking to the gun.)
  if (process.env.LOOK_YAW) { await page.evaluate(([y, p]) => window.rax.look({ yaw: y, pitch: p }), [Number(process.env.LOOK_YAW), Number(process.env.LOOK_PITCH || 0)]); for (let s = 1; s <= seconds; s++) { await delay(1000); await shot(`${String(s).padStart(2, '0')}-look`); } console.log(JSON.stringify({ errors: [...new Set(errors)].slice(0, 8), walls: await page.evaluate(() => window.raxDebug?.()?.walls ?? null) }, null, 1)); await browser.close(); await app.close(); process.exit(0); }
  void page.evaluate(() => window.rax.walkTo({ x: 0, z: 0 }, { within: 0.4, seconds: 12 }));
  for (let s = 1; s <= seconds; s++) {
    await delay(1000);
    // (Then off the roof: the fall, the street, and standing again.)
    if (s === Number(process.argv[4] || 9)) void page.evaluate((slow) => { window.rax.look({ yaw: 90 }); return window.rax.move({ fwd: slow ? 0.3 : 1, seconds: slow ? 9 : 6 }); }, process.argv[5] === 'slow').catch(() => {});
    // (`slow`: stepped off, not run off: on to the fire escape; then a look back at the tower, and from behind.)
    if (process.argv[5] === 'slow' && s === Number(process.argv[4] || 9) + 10) await page.evaluate(() => window.rax.look({ yaw: 270, pitch: 20 }));
    if (process.argv[5] === 'slow' && s === Number(process.argv[4] || 9) + 12) await page.keyboard.press('KeyV');
    const st = await state();
    await shot(`${String(s).padStart(2, '0')}-${st?.you?.alive === false ? 'down' : 'up'}`);
  }
  console.log(JSON.stringify({ errors: [...new Set(errors)].slice(0, 8), last: (await state())?.you }, null, 1));
} finally {
  await browser.close();
  await app.close();
  await rm(dir, { recursive: true, force: true }).catch(() => {});
}
