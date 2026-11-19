import test from 'node:test';
import assert from 'node:assert/strict';
import { Builders } from '../builders.mjs';

// One object stands in the room, made by `maker`. A worker is connected.
function rig() {
  const sent = [];
  const object = { id: 'obj', owner: 'maker', x: 1, z: 1, item: null, effect: null, version: 0,
    geometry: { name: 'Sign', boxes: [{ x: 0, y: 0.5, z: 0, w: 1, h: 1, d: 0.1 }] } };
  const store = { get: (_room, id) => (id === 'obj' ? object : undefined), objects: () => [object] };
  const player = { x: 0, y: 1.7, z: 0, yaw: 0, pitch: 0 };
  const rooms = new Map([['pod', { match: { seats: [{ player }], chalk: [[]] }, sessions: new Map() }]]);
  const builders = new Builders({ token: 't'.repeat(32), store, rooms, send: (to, m) => sent.push([to, m]) });
  builders.place = () => {};
  builders.broadcast = () => {};
  const worker = {};
  builders.workers.set(worker, new Set());
  return { builders, sent, worker };
}
const ask = { prompt: 'letter the board', objectId: 'obj', baseVersion: 0 };

test('a visitor cannot revise an object they did not make', () => {
  const { builders, sent } = rig();
  assert.equal(builders.request({}, 'pod', 'visitor', 0, ask), null);
  assert.match(sent.at(-1)[1].error, /Only the owner/);
});

test('the keeper revises it in place: it stays its maker\'s, asked for by the keeper', () => {
  const { builders, sent, worker } = rig();
  const id = builders.request({}, 'pod', 'keeper', 0, ask, { keeper: true });
  assert.ok(id);
  const job = builders.jobs.get(id);
  clearTimeout(job.timer);
  assert.equal(job.owner, 'maker'); // the commit's ownership check still holds
  assert.equal(job.asker, 'keeper'); // the result, signature and $code go to the keeper
  const build = sent.find(([to, m]) => to === worker && m.type === 'build')[1];
  assert.equal(build.previous.name, 'Sign');
  assert.equal(build.meta.asker, 'keeper');
});
