// class-load.mjs — a classroom in one rax room, against a live server.
//
// Scripted guests join a fresh named room (anyone there may build), move the
// whole time, ping twice a second, and all ask Aesel for a build at the same
// moment; one real Chrome window joins too and counts its own frames. The
// report is what a class would feel: frame rate, round trips, how far the
// server's acknowledgements trail the inputs, snapshot cadence, and what
// happened to every build.
//
//   node rax/test/class-load.mjs [--base https://aesthetic.computer/rax]
//     [--room ucla-load-…] [--bots 7] [--seconds 120] [--build-at 20] [--no-browser]
//   node rax/test/class-load.mjs --local …   the same against rax's own server
//     here, with guests seated and one builder worker on this machine's Aesel
//     sign-in (production seats only signed-in handles).
import { randomUUID } from 'node:crypto';
import { setTimeout as delay } from 'node:timers/promises';
import { createRequire } from 'node:module';
import { PROTOCOL } from '../world.mjs';

const { WebSocket } = createRequire(new URL('../../lith/package.json', import.meta.url))('ws');
const arg = (name, fallback) => { const i = process.argv.indexOf(`--${name}`); return i > 0 ? process.argv[i + 1] : fallback; };
const local = process.argv.includes('--local');
let base = arg('base', 'https://aesthetic.computer/rax').replace(/\/$/, '');
let app, builder, serverRows = [];
const CLASS_TOKEN = randomUUID() + randomUUID(); // the builder's shared secret with this server, 72 characters
if (local) {
  const { once } = await import('node:events');
  const { mkdtempSync } = await import('node:fs');
  const { tmpdir } = await import('node:os');
  const { join } = await import('node:path');
  const { spawn } = await import('node:child_process');
  const { createRaxServer } = await import('../server.mjs');
  app = createRaxServer({ guests: true, prefix: '/rax', builderToken: CLASS_TOKEN, stateDir: mkdtempSync(join(tmpdir(), 'rax-class-')),
    diagnosticsEnabled: true, diagnosticWrite: (r) => serverRows.push(r) });
  await app.ready;
  app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
  base = `http://127.0.0.1:${app.server.address().port}/rax`;
  builder = spawn(process.execPath, [new URL('../aesel.mjs', import.meta.url).pathname], {
    env: { ...process.env, RAX_SERVER_URL: `${base.replace(/^http/, 'ws')}/builder`, RAX_BUILDER_TOKEN: CLASS_TOKEN, RAX_BUILDER_NAME: 'class-load' },
    stdio: ['ignore', 'ignore', 'pipe'] });
  builder.stderr.on('data', (d) => process.env.CLASS_LOAD_VERBOSE && process.stderr.write(`builder: ${d}`));
  await delay(3000);
}
const room = arg('room', `ucla-load-${Date.now().toString(36)}`);
const bots = Number(arg('bots', 7)), seconds = Number(arg('seconds', 120)), buildAt = Number(arg('build-at', 20));
const browserToo = !process.argv.includes('--no-browser');
const PROMPTS = ['a red lighthouse', 'a stack of blue books', 'a small green tree', 'a yellow taxi',
  'a pink teapot', 'a wooden bench', 'a purple mushroom', 'a stone arch', 'an orange traffic cone', 'a snowman'];

const pct = (values, p) => { const v = [...values].sort((a, b) => a - b); return v.length ? v[Math.min(v.length - 1, Math.floor(p * v.length))] : null; };
const round = (n) => (n == null ? null : Math.round(n));
const most = (values) => values.reduce((m, v) => (v > m ? v : m), -Infinity);
const play = `${base.replace(/^http/, 'ws')}/play?${new URLSearchParams({ room, protocol: PROTOCOL, stay: '1' })}`;

function bot(index) {
  const c = { index, seat: -1, seq: 0, ack: 0, lag: [], rtt: [], snapshots: 0, bytes: 0, owner: null, builds: [], errors: [], ws: new WebSocket(play) };
  c.ws.on('message', (raw) => {
    c.bytes += raw.length;
    let d; try { d = JSON.parse(raw); } catch { return; }
    if (d.type === 'welcome' || d.type === 'role') { c.seat = d.seat ?? c.seat; if (d.type === 'welcome') {
      // As the page does for a guest: an identity, then no account, and the room seats it.
      c.ws.send(JSON.stringify({ type: 'identity', token: null }));
      c.ws.send(JSON.stringify({ type: 'account-none' }));
    } }
    if (d.type === 'identity') c.owner = d.owner;
    if (d.type === 'snapshot') { c.snapshots++; if (c.seat >= 0 && d.acks) { c.ack = d.acks[c.seat] || c.ack; c.lag.push(c.seq - c.ack); } }
    if (d.type === 'pong') c.rtt.push(performance.now() - d.at);
    if (d.type === 'build-accepted') { const b = c.builds.find((x) => x.rid === d.rid) || c.builds.at(-1); if (b && !b.acceptedMs) b.acceptedMs = Date.now() - b.at; }
    if (d.type === 'build-result') {
      const b = c.builds.find((x) => x.rid === d.rid) || c.builds.find((x) => !x.doneMs);
      if (b && !b.doneMs) { b.doneMs = Date.now() - b.at; b.error = d.error || null; b.ok = !d.error; }
    }
  });
  c.ws.on('error', (e) => c.errors.push(e.message));
  c.ws.on('close', (code) => c.errors.push(`closed ${code}`));
  return c;
}

const clients = Array.from({ length: bots }, (_, i) => bot(i));
await Promise.all(clients.map((c) => new Promise((resolve) => { c.ws.once('open', resolve); c.ws.once('error', resolve); })));
console.error(`room ${room}: ${clients.filter((c) => c.ws.readyState === 1).length}/${bots} bots connected`);

// The one real window: a page on this machine, counting its own frames.
let browser, page; const frames = [];
if (browserToo) {
  const { chromium } = await import('playwright');
  browser = await chromium.launch({ channel: 'chrome', headless: false, args: ['--window-size=960,640'] });
  page = await browser.newPage({ viewport: { width: 960, height: 600 } });
  page.on('pageerror', (e) => console.error('page error:', e.message));
  await page.goto(`${base}/?room=${encodeURIComponent(room)}`);
  await page.waitForFunction(() => document.querySelector('canvas')?.raxStats, null, { timeout: 60000 }).catch(() => console.error('page: no raxStats after 60 s'));
}

const timers = [];
timers.push(setInterval(() => {
  for (const c of clients) if (c.seat >= 0 && c.ws.readyState === 1) {
    const t = ++c.seq / 60 + c.index;
    c.ws.send(JSON.stringify({ type: 'input', commands: [{ seq: c.seq, fwd: Math.sin(t / 3), right: Math.cos(t / 4) * 0.5, yaw: (c.seq / 2 + c.index * 40) % 360, pitch: 0, buttons: 0 }] }));
  }
}, 1000 / 60));
timers.push(setInterval(() => { for (const c of clients) if (c.ws.readyState === 1) c.ws.send(JSON.stringify({ type: 'ping', at: performance.now() })); }, 500));

const began = Date.now();
let built = 0; const waves = Number(arg('waves', 1)), waveGap = Number(arg('wave-gap', 45));
while (Date.now() - began < seconds * 1000) {
  if (built < waves && Date.now() - began >= (buildAt + built * waveGap) * 1000) {
    built++;
    for (const c of clients) {
      const rid = randomUUID(), prompt = PROMPTS[(c.index + built * 3) % PROMPTS.length];
      c.builds.push({ rid, prompt, at: Date.now() });
      c.ws.send(JSON.stringify({ type: 'prompt', prompt, rid }));
    }
    console.error(`${bots} builds asked at once (wave ${built})`);
  }
  if (page) {
    const sample = await page.evaluate(() => new Promise((resolve) => {
      const start = performance.now(); let n = 0, worst = 0, last = start;
      const tick = (now) => { n++; worst = Math.max(worst, now - last); last = now; if (now - start < 1000) requestAnimationFrame(tick); else {
        const s = document.querySelector('canvas')?.raxStats || {};
        resolve({ fps: n / ((now - start) / 1000), worstFrameMs: worst, cpuMs: s.cpuMs, triangles: s.triangles, objects: s.objects?.length, members: s.members?.length, role: s.role, backend: s.backend });
      } };
      requestAnimationFrame(tick);
    })).catch(() => null);
    if (sample) frames.push({ at: Math.round((Date.now() - began) / 1000), ...sample });
  }
  await delay(4000);
}

for (const t of timers) clearInterval(t);
const elapsed = (Date.now() - began) / 1000;
const rtt = clients.flatMap((c) => c.rtt), lag = clients.flatMap((c) => c.lag);
const report = {
  room, bots, seconds: Math.round(elapsed), browser: browserToo,
  seated: clients.filter((c) => c.seat >= 0).length,
  rttMs: { p50: round(pct(rtt, 0.5)), p95: round(pct(rtt, 0.95)), max: round(most(rtt)) },
  snapshotsPerSecond: { min: round(Math.min(...clients.map((c) => c.snapshots / elapsed))), avg: round(clients.reduce((n, c) => n + c.snapshots, 0) / clients.length / elapsed) },
  inputAckLagFrames: { p50: pct(lag, 0.5), p95: pct(lag, 0.95), max: lag.length ? most(lag) : null },
  downKBps: round(clients.reduce((n, c) => n + c.bytes, 0) / clients.length / elapsed / 1024),
  builds: clients.flatMap((c) => c.builds.map((b) => ({ prompt: b.prompt, ok: b.ok ?? null, error: b.error ?? (b.doneMs ? null : 'no answer'), acceptedS: b.acceptedMs ? +(b.acceptedMs / 1000).toFixed(1) : null, doneS: b.doneMs ? +(b.doneMs / 1000).toFixed(1) : null }))),
  socketErrors: clients.flatMap((c) => c.errors),
  browserFrames: frames.length ? {
    fps: { min: round(Math.min(...frames.map((f) => f.fps))), p50: round(pct(frames.map((f) => f.fps), 0.5)) },
    worstFrameMs: round(Math.max(...frames.map((f) => f.worstFrameMs))),
    cpuMsP50: pct(frames.map((f) => f.cpuMs).filter(Number.isFinite), 0.5),
    finalObjects: frames.at(-1)?.objects, members: frames.at(-1)?.members, role: frames.at(-1)?.role, backend: frames.at(-1)?.backend,
    timeline: frames.map((f) => `${f.at}s ${Math.round(f.fps)}fps worst ${Math.round(f.worstFrameMs)}ms obj ${f.objects ?? '?'}`),
  } : null,
};
console.log(JSON.stringify(report, null, 2));
if (local) {
  const samples = serverRows.filter((r) => r.event === 'server.sample');
  report.server = { tickMaxMs: samples.length ? Math.max(...samples.map((r) => r.tickMaxMs || 0)) : null,
    tickP95Ms: pct(samples.map((r) => r.tickMaxMs || 0), 0.95), samples: samples.length };
  console.log(JSON.stringify({ server: report.server }));
}
for (const c of clients) c.ws.terminate();
await browser?.close();
builder?.kill(); await app?.close();
process.exit(0);
