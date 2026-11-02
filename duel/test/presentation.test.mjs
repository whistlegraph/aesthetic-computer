import assert from 'node:assert/strict';
import test from 'node:test';
import { LocalPresentation, SnapshotClock } from '../presentation.mjs';
import { DT, HZ, move, spawn } from '../world.mjs';
import { Match } from '../match.mjs';
import { reconcile } from '../net.mjs';

const pose = (x, extra = {}) => ({ ...spawn(0), x, ...extra });
const command = (seq) => ({ seq, fwd: 1, right: 0, yaw: 0, pitch: 0, buttons: 0, fire: false });

test('120 Hz presentation moves on every frame between 60 Hz physics steps', () => {
  const view = new LocalPresentation();
  view.reset(pose(0));
  const samples = [];
  for (let tick = 1; tick <= 10; tick++) {
    view.step(pose(tick / 6));
    samples.push(view.sample(0, 1 / 120).x, view.sample(0.5, 1 / 120).x);
  }
  for (let i = 1; i < samples.length; i++)
    assert.ok(Math.abs(samples[i] - samples[i - 1] - 1 / 12) < 1e-12);
});

test('reconciliation keeps the displayed pose continuous without changing authority', () => {
  const view = new LocalPresentation();
  view.reset(pose(0));
  view.step(pose(0.2));
  const before = view.sample(0.6, 0),
    authoritative = pose(0.3);
  view.correct(authoritative);
  assert.ok(Math.abs(view.sample(0.6, 0).x - before.x) < 1e-12);
  assert.equal(authoritative.x, 0.3);
  view.step(authoritative);
  for (let i = 0; i < 120; i++) view.sample(1, 1 / 120);
  assert.ok(Math.abs(view.sample(1, 0).x - 0.3) < 1e-6);
});

test('a stopped prediction tick holds still as the render fraction wraps', () => {
  const view = new LocalPresentation();
  view.reset(pose(0));
  const current = pose(.2);
  view.step(current);
  view.step(current);
  for (const alpha of [.9, 0, .5, .9, 0]) assert.equal(view.sample(alpha, 1/120).x, .2);
});

test('correction decay is independent of refresh rate and resets across lives and teleports', () => {
  function decay(rate) {
    const v = new LocalPresentation();
    v.reset(pose(0));
    v.correct(pose(0.4));
    for (let i = 0; i < rate / 2; i++) v.sample(1, 1 / rate);
    return v.sample(1, 0).x;
  }
  assert.ok(Math.abs(decay(60) - decay(144)) < 1e-12);
  const v = new LocalPresentation();
  v.reset(pose(0));
  v.correct(pose(0.4));
  v.correct(pose(-12, { life: 2 }));
  assert.equal(v.sample(0.2, 0).x, -12);
  v.correct(pose(10));
  assert.equal(v.sample(0.2, 0).x, 10);
  v.correct(pose(9.8, { alive: false }));
  assert.equal(v.sample(0.2, 0).x, 9.8);
});

test('packet jitter cannot rewind remote playback, and starvation freezes at the latest snapshot', () => {
  const clock = new SnapshotClock();
  clock.receive(20, 0);
  let previous = clock.sample(0);
  for (let frame = 1; frame <= 120; frame++) {
    const now = (frame * 1000) / 120;
    if (frame % 4 === 0) clock.receive(20 + frame / 2, now + (frame % 12 === 0 ? -10 : 0));
    const tick = clock.sample(now);
    assert.ok(tick >= previous);
    assert.ok(tick <= clock.latest);
    assert.ok(tick - previous <= 0.56);
    previous = tick;
  }
  clock.receive(1, 1001);
  assert.ok(clock.latest > 1, 'old snapshots ignored');
  clock.sample(2000);
  assert.equal(clock.sample(2100), clock.latest);
  clock.reset();
  clock.receive(2, 2200);
  assert.equal(clock.sample(2200), -4);
});

test('80 ms RTT with jitter: presentation reduces abrupt motion while simulation still reconciles', () => {
  const match = new Match();
  match.join();
  let player = spawn(0),
    pending = [],
    seq = 0,
    nextClient = 0,
    nextServer = 7,
    nextFrame = 0;
  const view = new LocalPresentation();
  view.reset(player);
  const packets = [],
    raw = [],
    smooth = [];
  let seed = 7;
  const latency = () => {
    seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0;
    return 28 + (seed / 4294967296) * 24;
  };
  for (let ms = 0; ms < 2400; ms++) {
    for (let i = packets.length - 1; i >= 0; i--) {
      const packet = packets[i];
      if (packet.at > ms) continue;
      packets.splice(i, 1);
      if (packet.commands) match.input(0, packet.commands);
      else {
        const result = reconcile(packet.player, pending, packet.ack);
        // Match the real client's rejection of out-of-order snapshots.
        if (packet.tick <= (view.serverTick || 0)) continue;
        view.serverTick = packet.tick;
        player = result.player;
        pending = result.pending;
        view.correct(player);
      }
    }
    if (ms >= nextClient) {
      const c = command(++seq);
      pending.push(c);
      player = move(player, c);
      view.step(player);
      packets.push({ at: ms + latency(), commands: pending.slice(-3) });
      nextClient += 1000 / HZ;
    }
    if (ms >= nextServer) {
      match.step();
      nextServer += 1000 / HZ;
      if (match.tick % 2 === 0)
        packets.push({
          at: ms + latency(),
          player: { ...match.seats[0].player },
          ack: match.seats[0].ack,
          tick: match.tick,
        });
    }
    if (ms >= nextFrame) {
      const p = view.sample((ms - (nextClient - 1000 / HZ)) / 1000 / DT, 1 / 120);
      if (ms > 400) {
        raw.push(player.z);
        smooth.push(p.z);
      }
      nextFrame += 1000 / 120;
    }
  }
  const roughness = (values) => {
    const deltas = values.slice(1).map((p, i) => p - values[i]);
    return deltas.slice(1).reduce((sum, d, i) => sum + Math.abs(d - deltas[i]), 0) / deltas.length;
  };
  assert.ok(roughness(smooth) < roughness(raw) * 0.35, `${roughness(raw)} -> ${roughness(smooth)}`);
  assert.ok(pending.length < 20);
  assert.ok(Math.abs(smooth.at(-1) - player.z) < 0.5);
});
