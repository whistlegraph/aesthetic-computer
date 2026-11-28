import test from 'node:test';
import assert from 'node:assert/strict';
import { deathLedger } from '../deaths.mjs';
import { Match } from '../match.mjs';
import { FALL_Y, WORLD, slabOf } from '../world.mjs';

test('a death is counted for its handle with the server\'s secret, and never without one', async () => {
  const calls = [];
  const fetch = async (url, init) => { calls.push({ url, init }); return { ok: true, json: async () => ({ deaths: 7 }) }; };
  const ledger = deathLedger({ url: 'https://x/api/rax-deaths', token: 'secret', fetch });
  assert.equal(await ledger.count('jeffrey', 'self'), 7);
  assert.equal(calls[0].init.method, 'POST');
  assert.equal(calls[0].init.headers.authorization, 'Bearer secret');
  assert.deepEqual(JSON.parse(calls[0].init.body), { handle: 'jeffrey', kind: 'self' });
  assert.equal(await ledger.read('jeffrey'), 7);
  assert.ok(calls[1].url.endsWith('?handle=jeffrey') && !calls[1].init.method);
  assert.equal(await ledger.count(null), null, 'a guest has no count');
  const bare = deathLedger({ token: '', fetch });
  assert.equal(await bare.count('jeffrey'), null);
  assert.equal(calls.length, 2, 'nothing was sent without the secret');
  const down = deathLedger({ token: 'secret', fetch: async () => { throw Error('down'); } });
  assert.equal(await down.count('jeffrey'), null, 'a database that is away costs the room nothing');
});

test('the match lists each body that dies, for the server to count', () => {
  const m = new Match(), seat = m.join();
  m.damage(seat, 100, seat);
  assert.deepEqual(m.deaths, [{ seat, attacker: seat }]);
  // A fall off a platform is one too.
  const p = new Match(), who = p.join();
  p.setWalls([slabOf(WORLD)]);
  Object.assign(p.seats[who].player, { x: WORLD + 3, y: FALL_Y - 5, onGround: false });
  p.step();
  assert.deepEqual(p.deaths, [{ seat: who, attacker: -1, fell: true }]);
});
