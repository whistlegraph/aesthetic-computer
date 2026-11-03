import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { WebSocket } from 'ws';
import { Connection } from '../connection.mjs';
import { createDuelServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';

test('ping accepts only outstanding echoes, including a real zero ms sample', () => {
  const c = new Connection();
  assert.equal(c.state(0, true).label, '...');
  assert.equal(c.reply(12, 20), false);
  assert.deepEqual(c.probe(0), { type: 'ping', at: 0 });
  assert.equal(c.reply(0, 0), true);
  assert.equal(c.state(0, true).label, '0 MS');
  assert.equal(c.state(0, true).bars, 4);
  assert.equal(c.reply(0, 10), false);
  assert.equal(c.probe(500), null);
  c.probe(1000);
  c.reply(1000, 1100);
  assert.equal(c.rtt, 15);
  assert.equal(c.jitter, 15);
  c.snapshot(60, 1100);
  assert.equal(c.state(2200, true).label, 'STALLED');
  assert.equal(c.state(2200, false).label, 'OFFLINE');
  c.reset();
  assert.equal(c.samples, 0);
  assert.equal(c.rtt, null);
});
test('probes stay bounded under loss and queue congestion', () => {
  const c = new Connection();
  for (let n = 0; n < 60000; n += 100) c.probe(n);
  assert.ok(c.pending.size <= 6);
  assert.equal(c.probe(60000, false), null);
  assert.equal(c.state(60000, true, 8192).label, 'STALLED');
  c.reset();
  c.snapshot(0, 0);
  c.snapshot(2, 33.333, 60);
  assert.ok(c.arrivalJitter < 0.01);
  c.snapshot(4, 100, 60);
  assert.ok(c.arrivalJitter > 3);
});
test('presence uses verified AC handles, survives match reset and clears departed seats', async () => {
  const app = createDuelServer({
    resolveHandleColors: async () => null,
    verifyIdentity: async (token) => {
      if (token === 'valid-test-token')
        return { handle: 'test_handle', sub: 'private-subject' };
      throw Error('not authenticated');
    },
  });
  await app.ready;
  app.server.listen(0, '127.0.0.1');
  await once(app.server, 'listening');
  const url = `ws://127.0.0.1:${app.server.address().port}/play?room=names&protocol=${PROTOCOL}`;
  const clients = [];
  const wait = async (f) => {
    for (let i = 0; i < 100; i++) {
      if (f()) return;
      await delay(10);
    }
    throw Error('presence timeout');
  };
  try {
    const a = new WebSocket(url, {
      headers: { authorization: 'Bearer valid-test-token' },
    });
    clients.push(a);
    const messages = [];
    a.on('message', (raw) => messages.push(JSON.parse(raw)));
    await once(a, 'open');
    await wait(() =>
      messages.some(
        (m) => m.type === 'presence' && m.members[0]?.handle === 'test_handle',
      ),
    );
    assert.equal(messages[0].type, 'welcome');
    const b = new WebSocket(url);
    clients.push(b);
    await once(b, 'open');
    b.send(
      JSON.stringify({
        type: 'account',
        accessToken: 'invalid',
        handle: 'jeffrey',
      }),
    );
    await wait(
      () => app.rooms.get('names').members.filter(Boolean).length === 2,
    );
    await delay(30);
    assert.equal(app.rooms.get('names').members[1].handle, null);
    app.rooms.get('names').match.reset();
    assert.equal(app.rooms.get('names').members[0].handle, 'test_handle');
    a.send(JSON.stringify({ type: 'ping', at: 123.45 }));
    await wait(() =>
      messages.some((m) => m.type === 'pong' && m.at === 123.45),
    );
    assert.ok(!JSON.stringify(messages).includes('private-subject'));
    assert.ok(!JSON.stringify(messages).includes('valid-test-token'));
    b.close();
    await wait(
      () =>
        (messages.at(-1)?.type === 'presence' &&
          messages.at(-1).members.length === 1) ||
        app.rooms.get('names').members[1] === null,
    );
  } finally {
    for (const ws of clients) ws.terminate();
    await app.close();
  }
});

test('duplicate handle spectator cannot send player input, build, or occupy the second player slot', async () => {
  const app=createDuelServer({verifyIdentity:async token=>({handle:token==='another'?'another':'jeffrey',sub:'unused'}),resolveHandleColors:async()=>null});
  await app.ready;app.server.listen(0,'127.0.0.1');await once(app.server,'listening');
  const url=`ws://127.0.0.1:${app.server.address().port}/play?room=watch&protocol=${PROTOCOL}`;
  const sockets=[];const wait=async f=>{for(let i=0;i<200;i++){if(f())return;await delay(10);}throw Error('role timeout');};
  const connect=async token=>{const ws=new WebSocket(url,{headers:{authorization:'Bearer '+token}});ws.messages=[];ws.on('message',raw=>ws.messages.push(JSON.parse(raw)));sockets.push(ws);await once(ws,'open');return ws;};
  try {
    const a=await connect('same');await wait(()=>app.rooms.get('watch').members[0]?.handle==='jeffrey');
    const b=await connect('same');await wait(()=>b.messages.some(m=>m.type==='presence'&&m.spectators===1));
    assert.equal(app.rooms.get('watch').match.seats.filter(Boolean).length,1);
    b.send(JSON.stringify({type:'input',commands:[{seq:1,fwd:1,right:1,yaw:90,pitch:0,buttons:1}]}));
    b.send(JSON.stringify({type:'prompt',prompt:'build something'}));
    await wait(()=>b.messages.some(m=>m.error==='Spectators cannot build'));
    assert.equal(app.rooms.get('watch').match.seats[0].ack,0);
    const c=await connect('another');await wait(()=>app.rooms.get('watch').members[1]?.handle==='another');
    assert.equal(app.rooms.get('watch').match.seats.filter(Boolean).length,2);
    b.close();await once(b,'close');assert.equal(app.rooms.get('watch').match.seats.filter(Boolean).length,2);
  } finally {for(const s of sockets)s.terminate();await app.close();}
});
