import assert from 'node:assert/strict';
import { once } from 'node:events';
import { readFile } from 'node:fs/promises';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import { createDuelServer } from '../server.mjs';
import { populatedRoom } from './load-fixture.mjs';

const rows = [], errors = [];
const app = createDuelServer({ prefix: '/dool', diagnosticsEnabled: true, diagnosticWrite: r => rows.push(r) });
await app.ready;
app.store.data.rooms.lobby = process.env.DOOL_ROOM_FIXTURE
  ? JSON.parse(await readFile(process.env.DOOL_ROOM_FIXTURE, 'utf8')).rooms.lobby : populatedRoom();
app.server.listen(0, '127.0.0.1');
await once(app.server, 'listening');
const base = `http://127.0.0.1:${app.server.address().port}`;
const browser = await chromium.launch({ channel: 'chrome', headless: true });
const watchdog = setTimeout(() => { console.error('Load browser exceeded 90 seconds'); process.exit(1); }, 90000);
try {
  const page = await browser.newPage({ viewport: { width: 465, height: 390 } });
  page.on('pageerror', e => errors.push(e.message));
  // Batch snapshots, as happens when the browser/network has fallen behind.
  // Preserve wire order and every non-snapshot message, including pongs.
  await page.addInitScript(() => {
    const Native = WebSocket;
    window.WebSocket = class extends Native {
      constructor(...args) {
        super(...args);
        const queue = [];
        const timer = setInterval(() => {
          for (const event of queue.splice(0)) if (this.readyState === Native.OPEN) this.onmessage?.(event);
        }, 150);
        super.addEventListener('message', event => { event.stopImmediatePropagation(); queue.push(event); });
        super.addEventListener('close', () => clearInterval(timer));
      }
    };
  });
  await page.goto(base + '/dool/');
  await page.waitForFunction(() => document.querySelector('canvas')?.doolStats?.objects.length === 11);
  const cdp = await page.context().newCDPSession(page);
  await cdp.send('Emulation.setCPUThrottlingRate', { rate: 4 });
  await page.locator('#game').focus();
  const seat = app.rooms.get('lobby').match.seats[0];
  const start = { ...seat.player };
  await page.keyboard.down('KeyD');
  await delay(1800);
  await page.keyboard.up('KeyD');
  assert.ok(Math.hypot(seat.player.x - start.x, seat.player.z - start.z) > 1, 'movement reaches authority under load');
  // Alternate direction over two diagnostic windows; keep exercising prediction.
  for (let i = 0; i < 10; i++) {
    const key = i % 2 ? 'KeyA' : 'KeyD';
    await page.keyboard.down(key); await delay(1000); await page.keyboard.up(key);
  }
  const state = await page.evaluate(() => {
    const s = document.querySelector('canvas').doolStats;
    return { connection: s.connection, objects: s.objects.length, backend: s.backend };
  });
  const samples = rows.filter(r => r.event === 'client.sample');
  assert.ok(samples.length >= 2, 'periodic diagnostics keep flowing');
  assert.ok(samples.some(r => r.snapshotsCoalesced > 0), 'burst snapshots coalesce');
  assert.ok(samples.every(r => r.frameErrors === 0), 'no frame failures');
  assert.ok(state.connection.samples >= 5, 'pongs continue while moving');
  assert.equal(state.connection.stalled, false);
  assert.equal(state.objects, 11);
  assert.equal(rows.filter(r => r.event === 'connection.closed').length, 0, 'no spontaneous disconnect');
  // A real relay disconnect must reload, reconnect and recover movement.
  const oldSession = samples[0].session;
  for (const socket of app.rooms.get('lobby').clients.keys()) socket.terminate();
  await page.waitForFunction(() => document.querySelector('canvas')?.doolStats?.connection?.samples >= 2 &&
    document.querySelector('canvas')?.doolStats?.objects.length === 11, {}, { timeout: 15000 });
  for (let i = 0; i < 100 && !rows.some(r => r.event === 'client.sample' && r.session !== oldSession); i++) await delay(100);
  assert.ok(rows.some(r => r.event === 'client.sample' && r.session !== oldSession), 'new session recovers after relay loss');
  assert.deepEqual(errors, []);
  console.log(JSON.stringify({ state, samples: samples.map(({frames,durationMs,frameGapMaxMs,reconcileMaxMs,pendingCommandsMax,snapshotsCoalesced,rttMs}) =>
    ({fps: Math.round(frames * 1000 / durationMs),frameGapMaxMs,reconcileMaxMs,pendingCommandsMax,snapshotsCoalesced,rttMs})), reconnect: true }));
} finally {
  clearTimeout(watchdog);
  await browser.close(); await app.close();
}
