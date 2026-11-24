import test from 'node:test';
import assert from 'node:assert/strict';
import { move, spawn, CFG } from '../world.mjs';
import { command } from '../net.mjs';
import { BIKE, rideable } from '../bike.mjs';
import { POOL } from '../basement.mjs';

const cmd = (over = {}) => ({ seq: 1, fwd: 0, right: 0, yaw: 0, pitch: 0, buttons: 0, ...over });
const run = (p, c, n, ...rest) => { for (let i = 0; i < n; i++) p = move(p, c, ...rest); return p; };
const settled = () => run(spawn(0), cmd(), 30);

test('the wire carries B, and nothing else turns it on', () => {
  assert.equal(command(cmd({ bike: true })).bike, true);
  assert.equal(command(cmd({ bike: 'yes' })).bike, false);
});

test('B gets on; the bike hovers, then goes where the eye looks', () => {
  let p = move(settled(), cmd({ bike: true }));
  assert.equal(p.bike, true);
  p = run(p, cmd(), 60);
  const hover = BIKE.hover + BIKE.seat + CFG.eyeHeight;
  assert.ok(Math.abs(p.y - hover) < 0.05, `hovers at ${p.y}`);
  const start = p;
  p = run(p, cmd({ fwd: 1, pitch: 30 }), 120); // two seconds, looking up and ahead
  assert.ok(p.y > start.y + 5, `climbed to ${p.y}`);
  assert.equal(p.bike, true);
  assert.equal(p.onGround, false);
});

test('space lifts, shift lets it sink, never under its hover', () => {
  let p = move(settled(), cmd({ bike: true }));
  p = run(p, cmd({ buttons: 1 }), 120);
  const high = p.y;
  assert.ok(high > 8, `lifted to ${high}`);
  p = run(p, cmd({ duck: true }), 600);
  assert.ok(Math.abs(p.y - (BIKE.hover + BIKE.seat + CFG.eyeHeight)) < 0.05);
});

test('B again in the air: off the bike, the body falls to the ground', () => {
  let p = move(settled(), cmd({ bike: true }));
  p = run(p, cmd({ buttons: 1 }), 90);
  p = move(p, cmd({ bike: true }));
  assert.equal(p.bike, false);
  p = run(p, cmd(), 240);
  assert.equal(p.onGround, true);
  assert.ok(Math.abs(p.y - CFG.eyeHeight) < 0.01);
});

test('no bikes in a side-on arena, the inner sea, or under a ceiling', () => {
  assert.equal(rideable([], true), false);
  assert.equal(rideable([POOL]), false);
  assert.equal(rideable([{ ceiling: true }]), false);
  assert.equal(rideable([]), true);
  assert.equal(move(settled(), cmd({ bike: true }), [], 0, [], true).bike || false, false);
});
