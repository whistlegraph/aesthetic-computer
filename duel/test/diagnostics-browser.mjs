import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdir } from 'node:fs/promises';
import { chromium } from 'playwright';
import { createDuelServer } from '../server.mjs';
import { connectBuilder } from '../aesel.mjs';
const rows = [], token = 't'.repeat(64);
const app = createDuelServer({ builderToken: token, diagnosticsEnabled: true, diagnosticWrite: (r) => rows.push(r) });
await app.ready;
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}`;
const shape = { name: 'box', boxes: [{ x: 0, y: 1, z: 0, w: 1, h: 2, d: 1, color: 0xff0088 }] };
const worker = connectBuilder({ url: base.replace('http', 'ws') + '/builder', token,
  generate: async (_job, signal, { onProgress }) => {
    for (let i = 0; i < 24; i++) {
      signal.throwIfAborted();
      onProgress({ phase: 'generating', geometry: shape, code: 'test only' });
      await new Promise((r) => setTimeout(r, 300));
    }
    return shape;
  } });
const browser = await chromium.launch({ channel: 'chrome', headless: true });
const errors = [];
try {
  const pages = [];
  for (let i = 0; i < 2; i++) {
    const context = await browser.newContext({ viewport: { width: 1000, height: 700 } });
    const page = await context.newPage();
    page.on('pageerror', (e) => errors.push(e.message));
    await page.goto(base + '/?room=diagnostics');
    await page.waitForFunction(() => document.querySelector('canvas').doolStats?.owner && document.querySelector('canvas').doolStats?.builder);
    pages.push(page);
  }
  const [a, b] = pages;
  await a.waitForFunction(() => document.querySelector('canvas').doolStats.members.length === 2);
  await a.locator('#game').focus();
  await a.keyboard.press('Enter');
  await a.keyboard.type('a box');
  await a.keyboard.press('Enter');
  await b.waitForFunction(() => document.querySelector('canvas').doolStats.jobs.length === 1);
  // Controlled main-thread stall: a recovered freeze must appear in this
  // client's sample, with the same build ID seen by the other participant.
  await a.evaluate(() => { const until = performance.now() + 450; while (performance.now() < until) {} });
  await a.waitForFunction(() => document.querySelector('canvas').doolStats.objects.length === 1, {}, { timeout: 15000 });
  await b.waitForFunction(() => document.querySelector('canvas').doolStats.objects.length === 1);
  const accepted = rows.find((r) => r.event === 'build.accepted');
  assert.ok(accepted);
  const samples = rows.filter((r) => r.event === 'client.sample' && r.buildIds.includes(accepted.id));
  assert.equal(new Set(samples.map((r) => r.session)).size, 2);
  assert.ok(samples.some((r) => r.frameGapMaxMs >= 400 || r.longTaskMaxMs >= 400));
  assert.ok(rows.some((r) => r.event === 'build.finished' && r.outcome === 'committed'));
  assert.equal(app.store.objects('diagnostics').length, 1);
  await mkdir('tmp/duel', { recursive: true });
  await a.screenshot({ path: 'tmp/duel/diagnostics-two-players.png' });
  // Render the opponent in front of this player so the head label is visible.
  const match = app.rooms.get('diagnostics').match;
  Object.assign(match.seats[0].player, { x: 5, z: -5, yaw: 0, pitch: 0 });
  Object.assign(match.seats[1].player, { x: 5, z: 0 });
  match.seats[0].player.life++;
  await a.waitForFunction(() => document.querySelector('canvas').doolStats.objects.length === 1);
  // A real snapshot and frame after the teleport, not a fixed browser sleep.
  await a.evaluate(() => new Promise((r) => {
    let frames = 0;
    const step = () => ++frames >= 30 ? r() : requestAnimationFrame(step);
    requestAnimationFrame(step);
  }));
  await a.screenshot({ path: 'tmp/duel/diagnostics-names.png' });
  assert.deepEqual(errors, []);
  console.log(JSON.stringify({ samples, results: rows.filter((r) => r.event.startsWith('build.')) }, null, 2));
} finally {
  await browser.close();
  worker.close();
  await app.close();
}
