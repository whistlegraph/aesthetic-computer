import test from 'node:test';
import assert from 'node:assert/strict';
import { Connection } from '../connection.mjs';

test('a good line is not shoddy; a late, uneven or slow one is', () => {
  const c = new Connection();
  assert.equal(c.shoddy(0), 0, 'nothing heard yet: not shoddy');
  for (let i = 0; i < 60; i++) c.snapshot(i, i * 1000 / 60);
  c.rtt = 40;
  assert.equal(c.shoddy(1000), 0, 'steady 60 Hz, quick echo');
  assert.ok(c.shoddy(1000 + 900) > 0.4, 'nearly a second late');
  assert.equal(c.shoddy(1000 + 5000), 1, 'long late saturates');
  const u = new Connection();
  for (let i = 0, at = 0; i < 120; i++) u.snapshot(i, (at += i % 2 ? 2 : 60));
  assert.ok(u.shoddy(u.lastSnapshot) > 0.1, 'uneven arrivals');
  const s = new Connection();
  s.snapshot(0, 0);
  s.rtt = 600;
  assert.ok(s.shoddy(0) >= 0.4, 'a slow echo');
});
