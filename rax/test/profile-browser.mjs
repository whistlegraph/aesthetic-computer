// ⏱️ Where a frame of rax goes: a local rax, one body on the roof, and the
// page's own JavaScript profiled for a few seconds. Prints frames a second
// and the functions that took the most time themselves.
//   node rax/test/profile-browser.mjs [seconds] [yaw]
import { once } from 'node:events';
import { existsSync } from 'node:fs';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import { createRaxServer } from '../server.mjs';

const seconds = Number(process.argv[2] || 5), yaw = Number(process.argv[3] || 0);
const dir = await mkdtemp(join(tmpdir(), 'rax-profile-'));
const app = createRaxServer({ guests: true, stateDir: dir, listHandles: async () => ['jeffrey'], verifyIdentity: async (token) => ({ handle: token, sub: `account:${token}` }) });
await app.ready; app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
const browser = await chromium.launch({ ...(existsSync('/Applications/Google Chrome.app') ? { channel: 'chrome' } : {}), headless: true, args: ['--enable-unsafe-webgpu'] });
try {
  const page = await (await browser.newContext({ viewport: { width: 1280, height: 720 } })).newPage();
  await page.addInitScript(() => { window.acTOKEN = 'jeffrey'; });
  await page.goto(`http://127.0.0.1:${app.server.address().port}/?debug`);
  for (const end = Date.now() + 20000; Date.now() < end; await delay(100)) { const s = await page.evaluate(() => window.rax?.state()); if (s?.connected && s.you.seated && s.you.x !== null) break; }
  await page.mouse.click(640, 360);
  // (`play`: take the gun and wait for the bell first, so heads, their trails, the billboard and blood are all in the frame that is measured.)
  if (process.argv[4] === 'play') { await page.evaluate(() => window.rax.walkTo({ x: 0, z: 0 }, { within: 0.4, seconds: 8 })).catch(() => {}); await delay(Number(process.argv[5] || 17) * 1000); }
  await page.evaluate((y) => window.rax.look({ yaw: y, pitch: 0 }), yaw);
  await delay(2500);
  const cdp = await page.context().newCDPSession(page);
  await cdp.send('Profiler.enable');
  await cdp.send('Profiler.setSamplingInterval', { interval: 500 });
  await cdp.send('Profiler.start');
  const fps = await page.evaluate((ms) => new Promise((done) => { let n = 0, worst = 0, last = performance.now(); const t0 = last; const tick = (t) => { n++; worst = Math.max(worst, t - last); last = t; if (t - t0 < ms) requestAnimationFrame(tick); else done({ fps: Math.round((n * 1000) / (t - t0) * 10) / 10, worstMs: Math.round(worst) }); }; requestAnimationFrame(tick); }), seconds * 1000);
  const { profile } = await cdp.send('Profiler.stop');
  const self = new Map(), dt = profile.timeDeltas, byId = new Map(profile.nodes.map((n) => [n.id, n]));
  profile.samples.forEach((id, i) => { const n = byId.get(id), f = n.callFrame, key = `${f.functionName || '(anon)'} ${f.url.split('/').pop().split('?')[0]}:${f.lineNumber + 1}`; self.set(key, (self.get(key) || 0) + (dt[i] || 0)); });
  const total = [...self.values()].reduce((a, b) => a + b, 0);
  console.log(JSON.stringify(fps), `yaw ${yaw}`);
  for (const [key, us] of [...self].sort((a, b) => b[1] - a[1]).slice(0, 22)) console.log(`${((us / total) * 100).toFixed(1).padStart(5)}%  ${(us / 1000 / seconds).toFixed(1).padStart(6)} ms/s  ${key}`);
} finally { await browser.close(); await app.close(); await rm(dir, { recursive: true, force: true }).catch(() => {}); }
