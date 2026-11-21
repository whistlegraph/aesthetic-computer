import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { WebSocket } from 'ws';
import { createRaxServer } from '../server.mjs';
import { connectBuilder } from '../aesel.mjs';
import { PROTOCOL } from '../world.mjs';
import { parseSource } from '../object-language.mjs';
import { createBuilds, ASK_MS, SLOW_MS, UNSENT_MS, LOST_MS, KEPT_MS } from '../build-line.mjs';

// A page's line with a clock the test turns and a wire the test can cut.
function line() {
  const sent = [];
  let t = 0, up = true, n = 0;
  const builds = createBuilds({ send: (m) => up && !!sent.push(m), now: () => t, name: () => `r${++n}` });
  return { builds, sent, pass: (ms) => ((t += ms), builds.tick()), wire: (on) => (up = on), states: () => builds.list.map((b) => b.state) };
}

test('the line: one state from the press to the end, and the request named', () => {
  const { builds, sent, states } = line();
  const b = builds.ask({ type: 'prompt', prompt: 'a ramp' }, 'a ramp', 'yard');
  assert.deepEqual(sent, [{ type: 'prompt', prompt: 'a ramp', rid: 'r1', in: 'yard' }]);
  assert.equal(b.state, 'asked');
  builds.hear({ type: 'build-accepted', id: 'j1', objectId: 'o1', rid: 'r1' });
  assert.equal(b.state, 'accepted');
  // The room's word moves it on, and says which rows stepped (for the ear).
  const job = { id: 'j1', owner: 'me', objectId: 'o1', version: 0, phase: 'connecting', x: 3, z: 4, startedAt: 1 };
  assert.deepEqual(builds.room('yard', [job], []), []);
  assert.deepEqual(builds.room('yard', [{ ...job, phase: 'generating', preview: { boxes: [1, 2] } }], []), [b]);
  assert.deepEqual([b.state, b.phase, b.parts], ['building', 'generating', 2]);
  builds.hear({ type: 'build-result', id: 'j1', rid: 'r1', objectId: 'o1', version: 0 });
  assert.deepEqual(states(), ['done']);
  // The room no longer speaking of it changes nothing once it has ended.
  builds.room('yard', [], [{ id: 'o1', version: 0 }]);
  assert.deepEqual(builds.rows().map((r) => [r.state, r.text, r.version]), [['done', 'a ramp', 0]]);
});

test('the line: a refusal is a row with the server\'s reason, and so is one that never left the page', () => {
  const { builds, states } = line();
  builds.ask({ type: 'prompt', prompt: 'a cat' }, 'a cat', 'w5s16');
  builds.hear({ type: 'build-result', rid: 'r1', error: 'Only someone who lives on this block arranges it' });
  assert.deepEqual(builds.rows().map((r) => [r.state, r.note]), [['failed', 'Only someone who lives on this block arranges it']]);
  builds.refuse('a dog', 'aesel is offline');
  builds.refuse('a dog', 'aesel is offline'); // pressed again: the same row
  // A refusal that names no asking of ours (a square painted where we may not) still shows.
  builds.hear({ type: 'build-result', error: 'The plaza is kept as it is: build on your floor' });
  assert.deepEqual(states(), ['failed', 'refused', 'refused']);
  // An answer said twice (to an asking sent again) is not a second row.
  builds.hear({ type: 'build-result', rid: 'gone', error: 'nope', again: true });
  assert.equal(builds.list.length, 3);
});

test('the line: no answer is asked once more under the same name, then said to be no answer', () => {
  const { builds, sent, pass, states } = line();
  builds.ask({ type: 'prompt', prompt: 'a kite' }, 'a kite', 'yard');
  assert.deepEqual(pass(ASK_MS - 1), []);
  assert.deepEqual(pass(2).map((e) => e.type), ['again']);
  assert.deepEqual(sent.map((m) => m.rid), ['r1', 'r1']);
  assert.deepEqual(pass(ASK_MS + 1).map((e) => [e.type, e.build.note]), [['lost', 'no answer from the server']]);
  assert.deepEqual(states(), ['failed']);
  assert.equal(sent.length, 2, 'given up: not said a third time');
  // The answer to the second saying, come late, is the one job's.
  const late = line();
  late.builds.ask({ type: 'prompt', prompt: 'a kite' }, 'a kite', 'yard');
  late.pass(ASK_MS + 1);
  late.builds.hear({ type: 'build-accepted', id: 'j', objectId: 'o', rid: 'r1', again: true });
  assert.deepEqual(late.pass(ASK_MS * 3), []);
  assert.deepEqual(late.states(), ['accepted']);
  // What cannot be said twice (a $code stood again) waits longer and is said once.
  const once = line();
  once.builds.ask({ type: 'restore', code: 'abc' }, '$abc', 'yard');
  assert.deepEqual(once.pass(SLOW_MS - 1), []);
  assert.deepEqual(once.pass(2).map((e) => e.type), ['lost']);
  assert.equal(once.sent.length, 1);
});

test('the line: an asking that could not be sent goes when the wire is back, and an unanswered one is said again on a new socket', () => {
  const { builds, sent, pass, wire, states } = line();
  wire(false);
  builds.ask({ type: 'prompt', prompt: 'a bell' }, 'a bell', 'yard');
  assert.equal(sent.length, 0);
  pass(1000);
  wire(true);
  pass(16);
  assert.deepEqual(sent.map((m) => m.rid), ['r1']);
  // The socket drops before the answer; the next one knows us: said again, same name.
  builds.rejoined();
  assert.deepEqual(sent.map((m) => m.rid), ['r1', 'r1']);
  builds.hear({ type: 'build-accepted', id: 'j1', objectId: 'o1', rid: 'r1' });
  builds.rejoined();
  assert.equal(sent.length, 2, 'an accepted build is not asked for again');
  assert.deepEqual(states(), ['accepted']);
  // Never any wire at all: said so.
  const dead = line();
  dead.wire(false);
  dead.builds.ask({ type: 'prompt', prompt: 'a gong' }, 'a gong', 'yard');
  assert.deepEqual(dead.pass(UNSENT_MS + 1).map((e) => e.build.note), ['not sent: no line to the server']);
});

test('the line: a build is picked back up from the room\'s word after the socket dropped', () => {
  const mine = (j) => (j.asker || j.owner) === 'me';
  const { builds, pass, states } = line();
  builds.ask({ type: 'prompt', prompt: 'a tower' }, 'a tower', 'yard');
  builds.hear({ type: 'build-accepted', id: 'j1', objectId: 'o1', rid: 'r1' });
  // Still running when the page is back: the same row, moved on.
  builds.room('yard', [{ id: 'j1', owner: 'me', objectId: 'o1', version: 0, phase: 'validating' }], [], mine);
  assert.deepEqual(builds.rows().map((r) => [r.state, r.phase]), [['building', 'validating']]);
  // Another room's word says nothing of it, and is not taken to mean it ended.
  builds.room('lobby', [], [], mine);
  assert.deepEqual(states(), ['building']);
  // Ended while the line was down: what stands in its room says how.
  builds.room('yard', [], [{ id: 'o1', version: 0 }], mine);
  assert.deepEqual(states(), ['done']);
  const gone = line();
  gone.builds.ask({ type: 'prompt', prompt: 'a moat' }, 'a moat', 'yard');
  gone.builds.hear({ type: 'build-accepted', id: 'j2', objectId: 'o2', rid: 'r1' });
  gone.builds.room('yard', [], [], mine);
  assert.deepEqual(gone.builds.rows().map((r) => r.state), ['failed']);
  // A page loaded again mid-build knows nothing: its own job joins the line, a stranger's does not.
  const fresh = line();
  fresh.builds.room('yard', [{ id: 'j3', owner: 'me', objectId: 'o3', prompt: 'a pond', phase: 'generating' }, { id: 'j4', owner: 'them', objectId: 'o4', prompt: 'a wall', phase: 'generating' }], [], mine);
  assert.deepEqual(fresh.builds.rows().map((r) => [r.state, r.text]), [['building', 'a pond']]);
  // The room spoke first on the new socket, then the asking said again is accepted: one row.
  const twice = line();
  twice.builds.ask({ type: 'prompt', prompt: 'a pond' }, 'a pond', 'yard');
  twice.builds.room('yard', [{ id: 'j5', owner: 'me', objectId: 'o5', prompt: 'a pond', phase: 'generating' }], [], mine);
  twice.builds.hear({ type: 'build-accepted', id: 'j5', objectId: 'o5', rid: 'r1', again: true });
  assert.equal(twice.builds.list.length, 1);
  // And a build nobody ever speaks of again does not spin for ever.
  const quiet = line();
  quiet.builds.ask({ type: 'prompt', prompt: 'a hill' }, 'a hill', 'yard');
  quiet.builds.hear({ type: 'build-accepted', id: 'j6', objectId: 'o6', rid: 'r1' });
  assert.deepEqual(quiet.pass(LOST_MS + 1).map((e) => e.build.state), ['failed']);
  assert.deepEqual(pass(0), []);
});

test('the line: an asking not yet answered outlives the page loading itself again', () => {
  let text = null, wall = 1000;
  const kept = { read: () => text, write: (t) => (text = t) };
  const a = createBuilds({ send: () => true, kept, now: () => 0, clock: () => wall, name: () => 'r1' });
  a.ask({ type: 'prompt', prompt: 'a drum' }, 'a drum', 'yard');
  a.ask({ type: 'restore', code: 'abc' }, '$abc', 'yard'); // (cannot be said twice: not kept)
  assert.deepEqual(JSON.parse(text).map((k) => k.message.rid), ['r1']);
  // The next page, three seconds on: the row is there, and waits to be known before it speaks.
  wall += 3000;
  const sent = [];
  let known = false, room = 'lobby';
  const b = createBuilds({ send: (m) => !!sent.push(m), ready: (askedRoom) => known && askedRoom === room, kept, now: () => 0, clock: () => wall });
  assert.deepEqual(b.rows().map((r) => [r.state, r.text]), [['asked', 'a drum']]);
  b.tick();
  assert.equal(sent.length, 0);
  known = true;
  b.rejoined();
  b.tick();
  assert.deepEqual(sent, [], 'a saved request cannot land on another floor');
  room = 'yard';
  b.rejoined();
  assert.deepEqual(sent.map((m) => [m.prompt, m.rid]), [['a drum', 'r1']]);
  b.hear({ type: 'build-accepted', id: 'j', objectId: 'o', rid: 'r1', again: true });
  b.tick();
  assert.equal(text, '[]', 'answered: nothing left to say again');
  // Written down too long ago, it is let go.
  text = JSON.stringify([{ at: wall - KEPT_MS - 1, text: 'a drum', room: 'yard', message: { type: 'prompt', prompt: 'a drum', rid: 'r1' } }]);
  assert.equal(createBuilds({ send: () => true, kept, now: () => 0, clock: () => wall }).list.length, 0);
});

// The server's half: a name said twice is one job.
const token = 'b'.repeat(40);
function player(port, ownerToken = null, room = 'line') {
  const ws = new WebSocket(`ws://127.0.0.1:${port}/play?room=${room}&protocol=${PROTOCOL}`);
  const seen = [];
  ws.on('message', (raw) => {
    const data = JSON.parse(raw);
    if (data.type === 'snapshot') return;
    seen.push(data);
    if (data.type === 'welcome') ws.send(JSON.stringify({ type: 'identity', token: ownerToken }));
  });
  const wait = async (pred, ms = 8000) => {
    for (const end = Date.now() + ms; Date.now() < end; await new Promise((r) => setTimeout(r, 20))) {
      const hit = seen.find(pred);
      if (hit) return hit;
    }
    throw Error('timed out waiting; saw ' + JSON.stringify(seen.map((d) => d.type + (d.error ? ':' + d.error : '')).slice(-12)));
  };
  return { ws, seen, wait, say: (m) => ws.send(JSON.stringify(m)) };
}

test('the server: a request said twice under one name is one build, answered again; without a name it is as it was', async () => {
  const app = createRaxServer({ guests: true, builderToken: token, verifyIdentity: async () => { throw Error('guest'); }, listHandles: async () => [] });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const port = app.server.address().port;
  let release, started = 0;
  const gate = new Promise((r) => (release = r));
  const builder = connectBuilder({
    url: `ws://127.0.0.1:${port}/builder`, token,
    generate: async (job, signal, { onProgress }) => {
      started++;
      onProgress({ phase: 'generating', code: '(ink red)', geometry: null });
      await gate;
      return parseSource('; Slow cube\n(ink red) (slab -1 0 -1 1 2 1)');
    },
  });
  try {
    const a = player(port);
    const identity = await a.wait((d) => d.type === 'identity');
    await a.wait((d) => d.type === 'objects' && d.builder);
    const left = () => app.builders?.guestRemaining?.(identity.owner);
    a.say({ type: 'prompt', prompt: 'a slow cube', rid: 'one' });
    const first = await a.wait((d) => d.type === 'build-accepted');
    assert.equal(first.rid, 'one', 'the name is said back');
    // Said again on the same socket (its answer was slow): the same job.
    a.say({ type: 'prompt', prompt: 'a slow cube', rid: 'one' });
    const again = await a.wait((d) => d.type === 'build-accepted' && d.again);
    assert.equal(again.id, first.id);
    const state = await a.wait((d) => d.type === 'objects' && d.jobs.length);
    assert.equal(state.jobs.length, 1);
    assert.equal(state.jobs[0].asker, identity.owner, 'the room says whose build it is');
    // The socket drops; the same owner on a new one says it again and is told it runs, and the result comes there.
    a.ws.close();
    const b = player(port, identity.token);
    await b.wait((d) => d.type === 'identity');
    b.say({ type: 'prompt', prompt: 'a slow cube', rid: 'one' });
    assert.equal((await b.wait((d) => d.type === 'build-accepted')).id, first.id);
    assert.equal(started, 1, 'one build was started');
    release();
    const result = await b.wait((d) => d.type === 'build-result');
    assert.deepEqual([result.rid, result.id, !!result.objectId, result.error], ['one', first.id, true, undefined]);
    // After it has ended, the name said once more is told how it ended, and nothing is built.
    b.say({ type: 'prompt', prompt: 'a slow cube', rid: 'one' });
    const told = await b.wait((d) => d.type === 'build-result' && d.again);
    assert.equal(told.objectId, result.objectId);
    assert.equal(app.store.objects('line').length, 1);
    assert.equal(started, 1);
    if (left() !== undefined) assert.equal(left(), 4, 'a guest spends one braincell for the one build');
    // A refusal says the name back too.
    b.say({ type: 'prompt', prompt: '', rid: 'two' });
    assert.equal((await b.wait((d) => d.type === 'build-result' && d.rid === 'two')).error, 'Use 1–280 characters');
    // An older page names nothing: each asking is a build, answered with no name.
    b.say({ type: 'prompt', prompt: 'another' });
    const plain = await b.wait((d) => d.type === 'build-accepted' && d.id !== first.id);
    assert.equal('rid' in plain, false);
    const done = await b.wait((d) => d.type === 'build-result' && d.id === plain.id);
    assert.equal('rid' in done, false);
    b.ws.close();
  } finally {
    builder.close();
    await app.close();
  }
});
