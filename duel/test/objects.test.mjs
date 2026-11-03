import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { WebSocket } from 'ws';
import { geometry, objectBoxes, poseObject } from '../objects.mjs';
import { ObjectStore } from '../object-store.mjs';
import { createDuelServer } from '../server.mjs';
import { connectBuilder } from '../aesel.mjs';
import { move, spawn, PROTOCOL, wallDistance } from '../world.mjs';
import { reconcile } from '../net.mjs';
import { partialGeometry } from '../build-progress.mjs';
import { Prompt } from '../prompt.mjs';
const stairs = {
  name: 'Stairs',
  boxes: Array.from({ length: 8 }, (_, i) => ({
    x: 0,
    y: (i + 1) * 0.15,
    z: i * 0.6,
    w: 2,
    h: (i + 1) * 0.3,
    d: 0.6,
    color: 0x69bfff,
  })),
};
const wait = async (fn) => {
  for (let i = 0; i < 200; i++) {
    if (fn()) return;
    await delay(15);
  }
  throw Error('Timed out');
};
test('bounded geometry rejects invalid shapes and fits oversized ones', () => {
  assert.equal(geometry(stairs).boxes.length, 8);
  // Below the ground or too big is never a refusal: the shape is grounded and scaled.
  assert.equal(geometry({ name: 'sunk', boxes: [{ x: 0, y: 0, z: 0, w: 1, h: 1, d: 1 }] }).boxes[0].y, 0.5);
  const huge = geometry({ name: 'huge', boxes: [{ x: 20, y: 30, z: 0, w: 40, h: 60, d: 2 }] }).boxes[0];
  assert.ok(huge.w <= 12 && huge.y + huge.h / 2 <= 6 && Math.abs(huge.x) + huge.w / 2 <= 6);
  assert.throws(() =>
    geometry({ name: 'bad', boxes: [{ x: Infinity, y: 1, z: 0, w: 1, h: 1, d: 1 }] }),
  );
  assert.throws(() => geometry({ name: 'bad', boxes: Array(33).fill(stairs.boxes[0]) }));
});
test('stair traversal, falling off, rocket collisions and prediction use the same geometry', () => {
  const objects = [{ id: 'stairs', x: 0, z: -5, geometry: stairs }];
  let p = { ...spawn(0), z: -6 };
  const commands = [];
  let highest = 0;
  for (let seq = 1; seq <= 95; seq++) {
    const c = { seq, fwd: 1, yaw: 0, pitch: 0, buttons: 0 };
    commands.push(c);
    p = move(p, c, objects);
    highest = Math.max(highest, p.y);
  }
  assert.ok(highest > 3.9, `climbed: ${highest}`);
  assert.ok(p.y < 2, 'falls back to the ground after leaving the stairs');
  assert.deepEqual(reconcile({ ...spawn(0), z: -6 }, commands, 0, objects).player, p);
  assert.ok(wallDistance({ x: 0, y: 0.15, z: -6 }, { x: 0, y: 0, z: 1 }, 10, objects) < 1);
  assert.equal(objectBoxes(objects).length, 8);
});
test('prompt editing never leaks WASD into movement', () => {
  const p = new Prompt();
  p.begin();
  for (const key of 'stairs') p.key({ key });
  p.key({ key: 'ArrowLeft' });
  p.key({ key: 'Backspace' });
  assert.equal(p.text, 'stais');
  p.key({ key: 'Escape' });
  assert.equal(p.open, false);
});
test('ownership and immutable versions survive restart; stale and foreign edits fail', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'dool-objects-'));
  try {
    const s = new ObjectStore(dir);
    await s.load();
    const a = await s.identity();
    const job = {
      room: 'r',
      owner: a.owner,
      objectId: 'object',
      baseVersion: -1,
      x: 3,
      z: 3,
      prompt: 'stairs',
    };
    await s.commit(job, stairs);
    await s.commit(
      { ...job, baseVersion: 0, prompt: 'wider' },
      { ...stairs, boxes: stairs.boxes.map((b) => ({ ...b, w: 3 })) },
    );
    await assert.rejects(s.commit({ ...job, baseVersion: 0 }, stairs));
    await assert.rejects(s.commit({ ...job, baseVersion: 1, owner: 'other' }, stairs));
    const reloaded = new ObjectStore(dir);
    await reloaded.load();
    assert.equal((await reloaded.identity(a.token)).owner, a.owner);
    assert.equal(reloaded.get('r', 'object').version, 1);
    assert.equal(reloaded.data.rooms.r[0].versions[0].geometry.boxes[0].w, 2);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});
test('builder authentication, concurrent users, shared versions and unauthorized edits', async () => {
  const token = 'a'.repeat(64),
    app = createDuelServer({ builderToken: token });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const base = `ws://127.0.0.1:${app.server.address().port}`;
  const clients = [];
  let worker;
  try {
    const denied = new WebSocket(base + '/builder');
    const [response] = await new Promise((resolve) => {
      denied.on('unexpected-response', (_, r) => {
        r.resume();
        resolve([r]);
      });
      denied.on('error', () => {});
    });
    assert.equal(response.statusCode, 401);
    denied.terminate();
    worker = connectBuilder({
      url: base + '/builder',
      token,
      generate: async () => {
        await delay(40);
        return stairs;
      },
    });
    await wait(() => app.builders.workers.size === 1);
    for (let i = 0; i < 2; i++) {
      const ws = new WebSocket(base + `/play?room=r&protocol=${PROTOCOL}`),
        messages = [];
      ws.on('message', (b) => messages.push(JSON.parse(b)));
      await once(ws, 'open');
      ws.send(JSON.stringify({ type: 'identity' }));
      await wait(() => messages.some((m) => m.type === 'identity'));
      clients.push({ ws, messages, owner: messages.find((m) => m.type === 'identity').owner });
    }
    for (const c of clients) c.ws.send(JSON.stringify({ type: 'prompt', prompt: 'stairs' }));
    await wait(() => app.store.objects('r').length === 2);
    assert.notEqual(clients[0].owner, clients[1].owner);
    for (const c of clients)
      await wait(() => c.messages.some((m) => m.type === 'objects' && m.objects.length === 2));
    const own = app.store.objects('r').find((o) => o.owner === clients[0].owner);
    clients[1].ws.send(
      JSON.stringify({ type: 'prompt', prompt: 'steal', objectId: own.id, baseVersion: 0 }),
    );
    await wait(() => clients[1].messages.some((m) => m.error?.includes('owner')));
    clients[0].ws.send(
      JSON.stringify({ type: 'prompt', prompt: 'wider', objectId: own.id, baseVersion: 0 }),
    );
    await wait(() => app.store.get('r', own.id).version === 1);
    assert.equal(app.rooms.get('r').match.objects.length, 2);
  } finally {
    worker?.close();
    for (const c of clients) c.ws.terminate();
    await app.close();
  }
});

test('partial source previews only complete valid boxes', () => {
  const prefix = '{"name":"Stairs","boxes":[';
  assert.equal(partialGeometry(prefix + '{"x":0'), null);
  const b = JSON.stringify(stairs.boxes[0]);
  assert.equal(partialGeometry(prefix + b + ',{"x":').boxes.length, 1);
  assert.ok(partialGeometry(prefix + b.replace('"w":2', '"w":200') + ']').boxes[0].w <= 12);
});
test('walking protocol has bounded paths and deterministic shared collision poses', () => {
  const shape = geometry({
    name: 'Walker',
    boxes: [
      { x: 0, y: 1.5, z: 0, w: 1, h: 1, d: 1, color: 0xffffff },
      { x: -0.3, y: 0.25, z: 0, w: 0.3, h: 0.5, d: 0.4, part: 'left-leg' },
      { x: 0.3, y: 0.25, z: 0, w: 0.3, h: 0.5, d: 0.4, part: 'right-leg' },
    ],
    motion: { type: 'walk', axis: 'z', distance: 1, period: 8 },
  });
  const object = { id: 'walker', x: 0, z: 0, geometry: shape };
  assert.equal(poseObject(object, 2).z, 1);
  assert.equal(poseObject(object, 6).z, -1);
  assert.deepEqual(poseObject(object, 1), poseObject(structuredClone(object), 1));
  const posed = poseObject(object, 1),
    bounds = objectBoxes([object], 1);
  assert.equal(bounds[0].zMin, posed.z - 0.5);
  assert.notEqual(posed.geometry.boxes[1].y, posed.geometry.boxes[2].y);
  assert.ok(geometry({ ...shape, motion: { ...shape.motion, distance: 100 } }).motion.distance <= 2);
  assert.throws(() => geometry({ ...shape, motion: { ...shape.motion, type: 'javascript' } }));
});

test('flight paths and hinged wings have synchronized conservative collision bounds', () => {
  const shape = geometry({
    name: 'Butterfly',
    boxes: [
      { x: 0, y: 0.4, z: 0, w: 0.2, h: 0.2, d: 0.8 },
      { x: -0.6, y: 0.4, z: 0, w: 1, h: 0.1, d: 1, part: 'left-wing' },
      { x: 0.6, y: 0.4, z: 0, w: 1, h: 0.1, d: 1, part: 'right-wing' },
    ],
    motion: { type: 'fly', axis: 'x', distance: 1, period: 8, height: 2, flapHz: 3 },
  });
  const object = { id: 'fly', x: 0, z: 0, geometry: shape };
  const pose = poseObject(object, 1 / 12),
    boxes = objectBoxes([object], 1 / 12);
  assert.ok(pose.y > 2);
  assert.ok(pose.geometry.boxes[1].roll < 0);
  assert.ok(pose.geometry.boxes[2].roll > 0);
  assert.ok(pose.geometry.boxes[1].y > 0.4);
  assert.ok(pose.geometry.boxes[2].y > 0.4);
  assert.ok(boxes.every((b) => b.yMin > 0));
  assert.deepEqual(poseObject(structuredClone(object), 1 / 12), pose);
  assert.equal(geometry({ ...shape, motion: { ...shape.motion, flapHz: 99 } }).motion.flapHz, 6);
});
