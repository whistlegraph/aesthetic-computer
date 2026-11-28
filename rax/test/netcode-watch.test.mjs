import test from 'node:test';
import assert from 'node:assert/strict';
import { watch, LINKS } from './netcode-watch.mjs';
import { interpolate } from '../net.mjs';
import { SeatClocks } from '../presentation.mjs';

test('a body watched over a jittery link walks and aims evenly: shown in its own steps it never stops, on any link', () => {
  for (const [name, up, down] of LINKS) {
    const own = watch({ up, down, own: true }), ticks = watch({ up, down, own: false });
    assert.equal(own.stops, 0, `${name}: no frame stands still`);
    assert.ok(own.walk < 0.15 && own.aim < 0.12, `${name}: walk ${own.walk} aim ${own.aim}`);
    assert.ok(own.walk <= ticks.walk + 1e-9 && own.aim <= ticks.aim + 1e-9, `${name}: never rougher than the server's ticks`);
  }
  // The case it is for: commands that reach the server in clumps.
  const [, up, down] = LINKS[2], ticks = watch({ up, down, own: false });
  assert.ok(ticks.stops > 0.1 && ticks.walk > 0.5, 'on the server\'s ticks that body stops and leaps');
});

test('a new body in a seat counts its steps from nothing, and a server that does not count them leaves the tick clock in charge', () => {
  const bodies = new SeatClocks(), body = { x: 0, y: 2, z: 0, yaw: 0, pitch: 0, life: 1, alive: true };
  for (let i = 0; i < 30; i++) bodies.receive([100 + i], [body], i * 16);
  assert.ok(bodies.sample(0, 500).tick > 100);
  bodies.receive([1], [{ ...body, life: 1 }], 520);
  assert.ok(bodies.sample(0, 540).tick <= 1, 'the old count is forgotten');
  bodies.receive(undefined, [body], 560);
  assert.equal(bodies.sample(0, 580), null);
  // Its steps start again inside the history: only the run that leads to now is read.
  const history = [{ tick: 1, moves: [500], players: [{ ...body, x: 9 }] }, { tick: 2, moves: [1], players: [{ ...body, x: 1 }] }, { tick: 3, moves: [3], players: [{ ...body, x: 3 }] }];
  assert.equal(interpolate(history, 2, 0, 0, true).x, 2);
});
