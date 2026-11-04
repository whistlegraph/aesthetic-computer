import assert from 'node:assert/strict';
import { once } from 'node:events';
import { monitorEventLoopDelay } from 'node:perf_hooks';
import { setTimeout as delay } from 'node:timers/promises';
import { WebSocket } from 'ws';
import { createDuelServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';
import { populatedRoom } from './load-fixture.mjs';

// Local only: four populated rooms, two players and two spectators per room.
const rows = [], clients = [], timers = [];
const app = createDuelServer({ diagnosticsEnabled: true, diagnosticWrite: r => rows.push(r) });
await app.ready;
for (let i = 0; i < 4; i++) app.store.data.rooms[`load-${i}`] = populatedRoom();
app.server.listen(0, '127.0.0.1'); await once(app.server, 'listening');
const loop = monitorEventLoopDelay({ resolution: 10 }); loop.enable();
try {
  for (let room = 0; room < 4; room++) for (let i = 0; i < 4; i++) {
    const ws = new WebSocket(`ws://127.0.0.1:${app.server.address().port}/play?protocol=${PROTOCOL}&room=load-${room}`);
    const c = { ws, snapshots: 0, pongs: 0, ack: 0, seq: 0, seat: -1, rtt: [] };
    clients.push(c);
    ws.on('message', raw => {
      const d = JSON.parse(raw);
      if (d.type === 'welcome' || d.type === 'role') c.seat = d.seat;
      if (d.type === 'snapshot') { c.snapshots++; c.ack = d.acks[c.seat] || 0; }
      if (d.type === 'pong') { c.pongs++; c.rtt.push(performance.now() - d.at); }
    });
    await once(ws, 'open');
  }
  const began = performance.now();
  timers.push(setInterval(() => {
    for (const c of clients) if (c.seat >= 0) c.ws.send(JSON.stringify({ type: 'input', commands: [{
      seq: ++c.seq, fwd: Math.sin(c.seq / 60), right: Math.cos(c.seq / 60), yaw: c.seq % 360, pitch: 0, buttons: 0,
    }] }));
  }, 1000 / 60));
  timers.push(setInterval(() => { for (const c of clients) c.ws.send(JSON.stringify({ type: 'ping', at: performance.now() })); }, 500));
  await delay(10000);
  const seconds = (performance.now() - began) / 1000;
  for (const c of clients) {
    assert.equal(c.ws.readyState, WebSocket.OPEN);
    assert.ok(c.snapshots / seconds > 40, 'snapshot cadence under load');
    assert.ok(c.pongs >= 15, 'echoes keep flowing');
    if (c.seat >= 0) assert.ok(c.ack > c.seq - 15, 'input acknowledgements keep up');
  }
  assert.ok(loop.max / 1e6 < 500, 'no event-loop stall over half a second');
  console.log(JSON.stringify({ rooms: 4, clients: clients.length, animatedObjects: 44,
    minimumSnapshotsPerSecond: Math.round(Math.min(...clients.map(c => c.snapshots / seconds))),
    maximumRttMs: Math.round(Math.max(...clients.flatMap(c => c.rtt))),
    eventLoopMaxMs: Math.round(loop.max / 1e6),
    tickMaxMs: Math.max(...rows.filter(r => r.event === 'server.sample').map(r => r.tickMaxMs)) }));
} finally { for (const timer of timers) clearInterval(timer); loop.disable(); for (const c of clients) c.ws.terminate(); await app.close(); }
