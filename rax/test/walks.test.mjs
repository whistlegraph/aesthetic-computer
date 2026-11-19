import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile, readdir, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createWalks, readMotion, readWalks } from '../walks.mjs';
import { HZ } from '../world.mjs';

test('a visit is indexed and its motion kept tick for tick', async () => {
  const directory = await mkdtemp(join(tmpdir(), 'rax-walks-'));
  let t = Date.UTC(2026, 10, 18, 12);
  await writeFile(join(directory, '2026-01-01.jsonl'), '{}\n'); // past keeping
  await writeFile(join(directory, '2026-01-01.motion'), '');
  const walks = createWalks({ directory, now: () => t, flushMs: 1e9 });
  const player = { x: 0, y: 1.7, z: -5, yaw: 0, pitch: 0 };
  const member = { ready: true, handle: null, owner: null, seat: -1 };
  const watcher = { ready: true, handle: null, owner: 'abcdef0123456789', seat: -1 };
  const room = { sessions: new Map([[1, member], [2, watcher]]), match: { seats: [{ player }, null] } };
  const step = (fn) => { fn?.(); walks.tick('lobby', room); t += 1000 / HZ; };
  step(); // nobody yet
  Object.assign(member, { handle: 'jeffrey', seat: 0 });
  const truth = [];
  for (let i = 0; i < 300; i++) step(() => {
    if (i < 120) { player.x += 0.02; player.yaw = (player.yaw + 1.25) % 360; player.pitch = -Math.sin(i / 9) * 30; }
    else if (i >= 280) player.z += 0.011; // one whole frame stands still between
    truth.push({ ...player });
  });
  walks.leave(member);
  await walks.close();
  const names = (await readdir(directory)).sort();
  assert.deepEqual(names, ['2026-11-18.jsonl', '2026-11-18.motion'], 'old days are pruned');
  const motion = readMotion(await readFile(join(directory, names[1])));
  const lines = (await readFile(join(directory, names[0]), 'utf8')).split('\n');
  const [a, b] = readWalks(lines, motion).sort((x, y) => (x.who > y.who ? 1 : -1));
  assert.equal(a.who, '@jeffrey');
  assert.equal(a.room, 'lobby');
  const path = motion.get(a.id);
  assert.equal(path.length, 180, 'the still frame is a gap, not samples');
  const kept = [...truth.slice(0, 120), ...truth.slice(240)];
  path.forEach((s, i) => {
    for (const k of ['x', 'y', 'z']) assert.ok(Math.abs(s[k] - kept[i][k]) <= 0.0005, `${k} at ${i}`);
    for (const k of ['yaw', 'pitch']) assert.ok(Math.abs(s[k] - kept[i][k]) <= 0.005, `${k} at ${i}`);
  });
  assert.ok(Math.abs(path[1].t - path[0].t - 1000 / HZ) <= 1, 'a tick apart');
  assert.ok(Math.abs(a.distance - (119 * 0.02 + 20 * 0.011)) < 0.01);
  assert.ok(a.left && a.seated);
  assert.match(b.who, /^[a-z]+-[a-z]+ \(abcdef01\)$/);
  assert.ok(b.guest && !b.seated && b.left && !motion.has(b.id), 'close ends visits still open');
  const size = (await readFile(join(directory, names[1]))).length;
  assert.ok(size < 180 * 6, `motion is small on disk (${size} bytes for 180 ticks)`);
  await rm(directory, { recursive: true });
});

test('without a state directory nothing is kept', async () => {
  const walks = createWalks({});
  walks.tick('lobby', { sessions: new Map([[1, { ready: true, handle: 'a', seat: -1 }]]), match: { seats: [] } });
  assert.equal(walks.enabled, false);
  await walks.close();
});

test("a walk in over a seam keeps the page's own count, once", async () => {
  const directory = await mkdtemp(join(tmpdir(), 'rax-walks-'));
  const t = Date.UTC(2026, 10, 19, 8);
  const walks = createWalks({ directory, now: () => t, flushMs: 1e9 });
  const member = { ready: true, handle: 'jeffrey', owner: null, seat: 0 };
  const room = { sessions: new Map([[1, member]]), match: { seats: [{ player: { x: 0, y: 1.7, z: 7, yaw: 0, pitch: 0 } }] } };
  walks.seam(member, { from: 'e0s1', ms: 1 }); // before its first tick: no visit yet
  walks.tick('e0s2', room);
  walks.seam(member, { from: 'e0s1', ms: 412 });
  walks.seam(member, { from: 'e0s1', ms: 9 });
  await walks.close();
  const lines = (await readFile(join(directory, '2026-11-19.jsonl'), 'utf8')).split('\n');
  const [v] = readWalks(lines);
  assert.deepEqual(v.seam, { from: 'e0s1', ms: 412 });
  await rm(directory, { recursive: true });
});
