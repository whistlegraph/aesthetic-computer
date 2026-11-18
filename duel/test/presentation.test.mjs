import assert from 'node:assert/strict';
import test from 'node:test';
import { LocalPresentation, SnapshotClock } from '../presentation.mjs';
import { DT, HZ, HORIZON, INTERP_TICKS, OVERRUN, WORLD, move, spawn } from '../world.mjs';
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

// 🏃 The rubric (README, "Local first"): the body answers its own keys at once
// and the server ends where the client predicted, whatever the link did.
test('commands held back by a three second stall are all run, and the server ends where the client predicted', () => {
  const m = new Match(), seat = m.join();
  let predicted = { ...m.seats[seat].player };
  const cmds = Array.from({ length: 181 }, (_, i) => ({ ...command(i + 1), right: i % 40 < 20 ? 1 : 0, buttons: i % 50 === 0 ? 1 : 0 }));
  for (const [i, c] of cmds.entries()) predicted = move(predicted, c, [], 0, [], false);
  m.input(seat, [cmds[0]]);
  m.step();
  for (let i = 0; i < 180; i++) m.step(); // the link is quiet: the body waits
  assert.ok(180 < HORIZON);
  for (let i = 1; i < cmds.length; i += 12) m.input(seat, cmds.slice(i, i + 12));
  m.step();
  const p = m.seats[seat].player;
  assert.equal(m.seats[seat].ack, 181);
  for (const k of ['x', 'y', 'z']) assert.ok(Math.abs(p[k] - predicted[k]) < 1e-9, `${k} ${p[k]} ${predicted[k]}`);
});

test('snapshots that arrive in clumps play further back, so others glide; a steady link plays close', () => {
  const steady = new SnapshotClock();
  for (let i = 0; i <= 120; i++) steady.receive(i, (i * 1000) / HZ);
  assert.ok(steady.delay - INTERP_TICKS < 0.5);
  const clumped = new SnapshotClock();
  // Twelve ticks at a time, every fifth of a second.
  for (let i = 0; i <= 120; i++) clumped.receive(i, Math.floor(i / 12) * 200);
  assert.ok(clumped.delay >= INTERP_TICKS + 10 && clumped.delay <= INTERP_TICKS + 30);
  let previous = clumped.sample(2000), stopped = 0;
  for (let frame = 1; frame <= 120; frame++) {
    const now = 2000 + (frame * 1000) / 60;
    if (frame % 12 === 0) for (let i = 0; i < 12; i++) clumped.receive(120 + frame - 11 + i, now);
    const tick = clumped.sample(now);
    assert.ok(tick >= previous && tick <= clumped.latest);
    if (frame > 30 && tick === previous) stopped++;
    previous = tick;
  }
  assert.equal(stopped, 0, 'playback never waits for a clump');
});

test('a side with a neighbour has no wall: the body walks on past the line; a closed side stops it', () => {
  const run = (open) => {
    let p = { ...spawn(0), x: 6, z: 0 };
    for (let i = 1; i <= 240; i++) p = move(p, { ...command(i), yaw: 90 }, [], 0, [], false, open);
    return p;
  };
  assert.ok(Math.abs(run(null).x - (WORLD - 0.4)) < 1e-9);
  assert.ok(Math.abs(run(['west']).x - (WORLD - 0.4)) < 1e-9);
  const through = run(['east']);
  assert.ok(Math.abs(through.x - (WORLD - 0.4 + OVERRUN)) < 1e-9);
  // Crossing the line costs no speed.
  let p = { ...spawn(0), x: 6, z: 0 }, slowest = Infinity;
  for (let i = 1; i <= 30; i++) { p = move(p, { ...command(i), yaw: 90 }, [], 0, [], false, ['east']); if (i > 20) slowest = Math.min(slowest, p.vx); }
  assert.ok(p.x > WORLD && slowest > 9);
});
