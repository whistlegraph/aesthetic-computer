import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp, writeFile, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { WebSocket } from 'ws';
import { Geography, GeographyStore } from '../geography.mjs';
import { createDuelServer } from '../server.mjs';
import { PROTOCOL } from '../world.mjs';
import { edgeAt, landing } from '../park.mjs';

const legacy = () => ({ opened: 1, homes: { jeffrey: { block: 1, lot: 0, at: 10 }, oskie: { block: 1, lot: 1, at: 20 } },
  rests: { jeffrey: { room: 'lobby', x: 1, z: 2, yaw: 90, at: 30 } } });
const wait = async (f) => {
  for (let i = 0; i < 300; i++) { if (await f()) return; await delay(10); }
  throw Error('timeout');
};

test('v1 migration keeps neighbours and rests, writes a backup, and produces permanent pod addresses', async () => {
  const dir = await mkdtemp(join(tmpdir(), 'dool-pods-'));
  try {
    await writeFile(join(dir, 'geography.json'), JSON.stringify(legacy()));
    const a = new GeographyStore(dir); await a.load();
    const j = a.geography.forHandle('jeffrey').id, o = a.geography.forHandle('oskie').id;
    assert.notEqual(j, o);
    assert.deepEqual(a.geography.placements, { [j]: { block: 1, lot: 0 }, [o]: { block: 1, lot: 1 } });
    assert.deepEqual(a.geography.rests, legacy().rests);
    assert.deepEqual(JSON.parse(await readFile(join(dir,'geography.v1.json'))), legacy());
    await a.change(g => g.curate([{ pod: j, block: 1, lot: 6 }, { pod: o, block: 1, lot: 7 }]));
    const b = new GeographyStore(dir); await b.load();
    assert.equal(b.geography.forHandle('jeffrey').id, j);
    assert.equal(b.geography.forHandle('oskie').id, o);
    assert.equal(b.geography.publicPod(o).placement.lot, 7);
    assert.deepEqual(JSON.parse(await readFile(join(dir,'geography.v1.json'))), legacy());
  } finally { await rm(dir, { recursive: true, force: true }); }
});

test('curation permits swaps, rejects occupied destinations atomically, and detects stale edits', () => {
  const g = new Geography(legacy()), j = g.forHandle('jeffrey').id, o = g.forHandle('oskie').id;
  const before = JSON.stringify(g);
  assert.throws(() => g.curate([{ pod:j, block:1, lot:1 }]), /taken/);
  assert.equal(JSON.stringify(g), before);
  assert.throws(() => g.curate([{ pod:j, block:1, lot:2 }, { pod:o, block:2, lot:0 }]), /not open/);
  assert.equal(JSON.stringify(g), before);
  g.curate([{ pod:j, block:1, lot:1 }, { pod:o, block:1, lot:0 }], 0);
  assert.equal(g.home('jeffrey').lot, 1);
  assert.throws(() => g.curate([{ pod:j, block:1, lot:2 }], 0), /refresh/);
  assert.equal(g.publicPod(j).interior, j);
  assert.equal(g.publicPod(j).at, 10);
});

test('verified handle changes retain pod identity; private account IDs never enter the public map', () => {
  const g = new Geography(legacy());
  const j = g.identify('jeffrey', 'account:secret').pod;
  g.identify('newname', 'account:secret');
  assert.equal(g.home('newname').pod, j);
  assert.equal(g.home('jeffrey'), null);
  assert.equal(g.publicPod(j).profile.handle, 'newname');
  assert.equal(g.whereabouts('newname').room, 'lobby');
  assert.throws(() => g.identify('newname', 'account:other'), /another account/);
  assert.ok(!JSON.stringify(g.census()).includes('account:secret'));
});

test('park paths only connect open adjacent blocks and land inside the opposite edge', () => {
  const g = new Geography();
  assert.deepEqual(g.neighbours('lobby'), { east: 'e1n0' });
  assert.deepEqual(g.neighbours('e1n0'), { west: 'lobby' });
  assert.equal(g.neighbours('studio'), null);
  assert.equal(edgeAt({x:7.4,y:1.7,z:0}), 'east');
  assert.equal(edgeAt({x:7.4,y:1.7,z:2}), null);
  assert.equal(edgeAt({x:7.4,y:3,z:0}), null);
  assert.deepEqual(landing({x:7.4,z:0.2,yaw:90,pitch:12},'east'),{x:-6.9,z:0.2,yaw:90,pitch:12});
});

async function fixture(t) {
  const dir = await mkdtemp(join(tmpdir(), 'dool-pod-server-'));
  const app = createDuelServer({stateDir:dir, resolveHandleColors:async h => Array.from({length:h.length+1},()=>({r:20,g:200,b:240})),
    verifyIdentity:async token=>({handle:token,sub:`account:${token}`}), verifyCurator:async auth=>auth==='Bearer curator'});
  await app.ready;
  await app.geography.change(g=>{g.identify('jeffrey','account:jeffrey');g.identify('oskie','account:oskie');});
  app.server.listen(0,'127.0.0.1'); await once(app.server,'listening');
  const base=`http://127.0.0.1:${app.server.address().port}`;
  t.after(async()=>{await app.close();await rm(dir,{recursive:true,force:true});});
  const connect=async(room,handle=null,token=null,passage=null)=>{
    const params=new URLSearchParams({room,protocol:PROTOCOL,...(handle?{account:'1'}:{}),...(passage?{passage}:{})});
    const ws=new WebSocket(base.replace('http','ws')+'/play?'+params), messages=[];
    ws.on('message',raw=>messages.push(JSON.parse(raw)));
    await once(ws,'open');
    ws.send(JSON.stringify({type:'identity',token}));
    if(handle)ws.send(JSON.stringify({type:'account',accessToken:handle}));
    await wait(()=>messages.some(m=>m.type==='identity') && [...app.rooms.get(room).sessions.values()].some(m=>m.ws!==undefined&&m.owner));
    return {ws,messages,token:messages.find(m=>m.type==='identity').token};
  };
  return {app,base,connect};
}

test('live curation is admin-only, keeps pod contents, refreshes tags, and serializes competing edits', async t => {
  const {app,base,connect}=await fixture(t);
  const j=app.geography.geography.forHandle('jeffrey').id;
  await app.store.paintTile(j,8,8,3);
  await app.store.addScreen(j,{id:'screen',owner:'owner',code:'abc',x:0,z:0});
  const c=await connect('e1n0');
  await wait(()=>c.messages.some(m=>m.type==='lots'&&m.homes[0]?.colors));
  assert.equal(c.messages.find(m=>m.type==='lots').homes[0].pod,j);
  const before=structuredClone(app.store.data);
  const edit={revision:app.geography.geography.revision,moves:[{pod:j,block:1,lot:6}]};
  const post=auth=>fetch(base+'/park/layout',{method:'POST',headers:{authorization:auth},body:JSON.stringify(edit)});
  assert.equal((await post('')).status,403);
  const results=await Promise.all([post('Bearer curator'),post('Bearer curator')]);
  assert.deepEqual(results.map(r=>r.status).sort(),[200,409]);
  assert.deepEqual(app.store.data,before);
  await wait(()=>c.messages.some(m=>m.type==='lots'&&m.homes.some(h=>h.pod===j&&h.lot===6)));
  assert.equal((await (await fetch(base+'/pod?id='+j)).json()).placement.lot,6);
  assert.equal((await fetch(base+'/pod?id=pod-missing')).status,404);
});

test('guests can enter a pod but cannot paint, clear, build, remove, or restore; its owner can arrange it', async t => {
  const {app,connect}=await fixture(t), j=app.geography.geography.forHandle('jeffrey').id;
  await app.store.paintTile(j,8,8,2);
  const visitor=await connect(j);
  for(const type of ['paint','clear-room','prompt','remove','restore','screen','wall-res','kidlisp-mod'])
    visitor.ws.send(JSON.stringify({type,i:8,j:8,c:4,prompt:'a chair',objectId:'x',code:'abc',face:'north',dir:1}));
  await wait(()=>visitor.messages.filter(m=>m.type==='build-result'&&m.error?.includes('pod owner')).length===8);
  assert.equal(app.store.tiles(j)['8,8'],2);
  const owner=await connect(j,'jeffrey');
  await wait(()=>[...app.rooms.get(j).sessions.values()].some(m=>m.handle==='jeffrey'));
  owner.ws.send(JSON.stringify({type:'paint',i:8,j:8,c:4}));
  await wait(()=>app.store.tiles(j)['8,8']===4);
});

test('walking uses server position and a one-use owner-bound passage, preserving facing', async t => {
  const {app,connect}=await fixture(t);
  const a=await connect('lobby');
  a.ws.send(JSON.stringify({type:'walk',edge:'east',x:7.4,z:0}));
  await delay(40);
  assert.ok(!a.messages.some(m=>m.type==='travel'));
  const member=[...app.rooms.get('lobby').sessions.values()][0];
  Object.assign(app.rooms.get('lobby').match.seats[member.seat].player,{x:7.4,z:0.2,y:1.7,yaw:90,pitch:12});
  a.ws.send(JSON.stringify({type:'walk',edge:'east'}));
  await wait(()=>a.messages.some(m=>m.type==='travel'));
  const travel=a.messages.find(m=>m.type==='travel');
  const stranger=await connect('e1n0',null,null,travel.passage);
  await delay(40);
  assert.ok(!stranger.messages.some(m=>m.type==='arrival'));
  const b=await connect('e1n0',null,a.token,travel.passage);
  await wait(()=>b.messages.some(m=>m.type==='arrival'));
  assert.equal(b.messages.find(m=>m.type==='arrival').yaw,90);
  assert.equal(b.messages.find(m=>m.type==='arrival').pitch,12);
  const replay=await connect('e1n0',null,a.token,travel.passage);
  await delay(40);
  assert.ok(!replay.messages.some(m=>m.type==='arrival'));
});

test('a pod doorway admits nearby visitors and exits at its current park placement after curation', async t => {
  const {app,connect}=await fixture(t), id=app.geography.geography.forHandle('oskie').id;
  const a=await connect('e1n0');
  a.ws.send(JSON.stringify({type:'enter-pod',pod:id}));
  await delay(40);
  assert.ok(!a.messages.some(m=>m.type==='travel'));
  const member=[...app.rooms.get('e1n0').sessions.values()][0];
  Object.assign(app.rooms.get('e1n0').match.seats[member.seat].player,{x:6.25,z:1.25,y:1.7});
  a.ws.send(JSON.stringify({type:'enter-pod',pod:id}));
  await wait(()=>a.messages.some(m=>m.type==='travel'));
  const enter=a.messages.find(m=>m.type==='travel');
  assert.equal(enter.room,id);
  const inside=await connect(id,null,a.token,enter.passage);
  await wait(()=>inside.messages.some(m=>m.type==='arrival'));
  await app.curate({revision:app.geography.geography.revision,moves:[{pod:id,block:1,lot:6}]});
  inside.ws.send(JSON.stringify({type:'exit-pod'}));
  await wait(()=>inside.messages.some(m=>m.type==='travel'));
  const exit=inside.messages.find(m=>m.type==='travel');
  const outside=await connect(exit.room,null,a.token,exit.passage);
  await wait(()=>outside.messages.some(m=>m.type==='arrival'));
  const pose=outside.messages.find(m=>m.type==='snapshot'&&m.players.some(p=>p?.x===-6.25));
  assert.ok(pose,'exit follows the relocated pod');
});
