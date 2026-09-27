import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';

const source = await readFile(new URL('../oskiewar.js', import.meta.url), 'utf8');

// Step the real movement solver directly so title/countdown timing cannot
// mask the trajectory. Record simulation seconds and real time at game speed.
function arc(mode = 'fight', releaseFrame = Infinity) {
  const noop = () => {};
  const api = new Function('runtime', 'capabilities', 'telemetry', 'gameSignal',
    'drum', 'wipe', 'box', 'line', 'triangle', 'write', 'systemWrite', `${source}\nreturn {
      players, updatePlayer, terrainFloorAt, gameSpeed,
      mode(value) { gameMode = value === 'survival' ? 'survival' : 'fight';
        fightOpponent = value === 'freeskate' ? 'freeskate' : 'dummy'; }
    };`)(() => ({ monotonicUs: 0 }), () => ({ platform: 'web' }),
    noop, noop, noop, noop, noop, noop, noop, noop, noop);
  api.mode(mode);
  const player = api.players[0];
  player.x = 1800;
  player.y = api.terrainFloorAt(player.x);
  player.grounded = true;
  const floor = player.y;
  let peak = 0, lift = null;
  for (let frame = 0; frame < 240; frame++) {
    api.updatePlayer(player, { down: frame < releaseFrame ? ['ArrowUp'] : [], leftX: 0, leftY: 0 },
      1 / 60, (frame + 1) * 1e6 / 60);
    if (!player.grounded && lift === null) lift = frame;
    peak = Math.max(peak, floor - player.y);
    if (lift !== null && player.grounded) return {
      peak, airtime: (frame - lift) / 60, wallTime: (frame - lift) / 60 / api.gameSpeed,
      latency: (lift + 1) / 60,
    };
  }
  assert.fail('Jump never landed');
}

test('fight jumps clear adjacent decks and return in about one real second', () => {
  const jump = arc();
  assert.ok(jump.peak > 360 && jump.peak < 420, JSON.stringify(jump));
  assert.ok(jump.airtime > .7 && jump.airtime < .9, JSON.stringify(jump));
  assert.ok(jump.wallTime < 1.1, JSON.stringify(jump));
  assert.ok(jump.latency < .09, JSON.stringify(jump));
});

test('releasing jump produces a lower, shorter hop', () => {
  const held = arc(), tap = arc('fight', 2);
  assert.ok(tap.peak > 60 && tap.peak < held.peak * .5, JSON.stringify(tap));
  assert.ok(tap.airtime < held.airtime * .7, JSON.stringify(tap));
});

test('freeskate and survival retain their existing jump heights', () => {
  for (const mode of ['freeskate', 'survival']) {
    const jump = arc(mode);
    if (mode === 'freeskate') assert.ok(jump.wallTime < 1, JSON.stringify(jump));
    assert.ok(jump.peak > 280 && jump.peak < 340, `${mode}: ${JSON.stringify(jump)}`);
  }
});
