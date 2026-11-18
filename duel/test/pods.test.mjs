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
import { move } from '../world.mjs';
import { PROTOCOL } from '../world.mjs';
import { edgeAt, landing } from '../park.mjs';
import { lot } from '../geography.mjs';

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
    await a.change(g => g.curate([{ pod: j, block: 1, lot: 2 }, { pod: o, block: 1, lot: 3 }]));
    const b = new GeographyStore(dir); await b.load();
    assert.equal(b.geography.forHandle('jeffrey').id, j);
    assert.equal(b.geography.forHandle('oskie').id, o);
    assert.equal(b.geography.publicPod(o).placement.lot, 3);
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
  // Housing to the east; the tower's grounds to the west and south (no way through its back).
  assert.deepEqual(g.neighbours('lobby'), { east: 'e1n0', west: 'w1n0', south: 'e0s1' });
  assert.deepEqual(g.neighbours('e1n0'), { west: 'lobby', south: 'e1s1' });
  assert.equal(g.neighbours('studio'), null);
  assert.equal(edgeAt({x:8.25,y:1.7,z:0}), 'east');
  assert.equal(edgeAt({x:8.25,y:1.7,z:2}), 'east'); // the whole side is open, not only the lane
  assert.equal(edgeAt({x:8.1,y:1.7,z:2}), null);
  assert.equal(edgeAt({x:8.25,y:3,z:0}), null);
  // The very same spot seen from one block over: nothing moves, and it is not handed straight back.
  const landed = landing({x:8.3,z:0.2,yaw:90,pitch:12},'east');
  assert.ok(Math.abs(landed.x+7.7)<1e-9);
  assert.deepEqual({...landed,x:0},{x:0,z:0.2,yaw:90,pitch:12});
  assert.equal(edgeAt({...landed,y:1.7}), null);
});

async function fixture(t) {
  const dir = await mkdtemp(join(tmpdir(), 'dool-pod-server-'));
  const app = createDuelServer({stateDir:dir, resolveHandleColors:async h => Array.from({length:h.length+1},()=>({r:20,g:200,b:240})),
    verifyIdentity:async token=>({handle:token,sub:`account:${token}`})});
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

test('walking uses server position and a one-use owner-bound passage, preserving facing', async t => {
  const {app,connect}=await fixture(t);
  const a=await connect('lobby');
  a.ws.send(JSON.stringify({type:'walk',edge:'east',x:8.3,z:0}));
  await delay(40);
  assert.ok(!a.messages.some(m=>m.type==='travel'));
  const member=[...app.rooms.get('lobby').sessions.values()][0];
  Object.assign(app.rooms.get('lobby').match.seats[member.seat].player,{x:8.3,z:0.2,y:1.7,yaw:90,pitch:12,vx:6});
  a.ws.send(JSON.stringify({type:'walk',edge:'east'}));
  await wait(()=>a.messages.some(m=>m.type==='travel'));
  const travel=a.messages.find(m=>m.type==='travel');
  // Asked again before the answer got there (a slow link): the same passage, never one that voids the first.
  a.ws.send(JSON.stringify({type:'walk',edge:'east'}));
  await wait(()=>a.messages.filter(m=>m.type==='travel').length===2);
  assert.equal(a.messages.filter(m=>m.type==='travel')[1].passage,travel.passage);
  // The motion crosses with the body, and the walker is told which commands the plaza ran.
  // (A tick or two of friction may pass before the plaza answers.)
  assert.ok(travel.at.vx>4&&travel.at.vx<=6);
  assert.equal(travel.ack,0);
  const stranger=await connect('e1n0',null,null,travel.passage);
  await delay(40);
  assert.ok(!stranger.messages.some(m=>m.type==='arrival'));
  const b=await connect('e1n0',null,a.token,travel.passage);
  await wait(()=>b.messages.some(m=>m.type==='arrival'));
  assert.equal(b.messages.find(m=>m.type==='arrival').yaw,90);
  assert.equal(b.messages.find(m=>m.type==='arrival').pitch,12);
  // 🏃 The walker kept walking on the way: the commands given between the two
  // rooms are run when they arrive, and the body ends where they lead.
  const next=app.rooms.get('e1n0'), seat=[...next.sessions.values()].filter(s=>s.seat>=0).at(-1).seat;
  const landed={...next.match.seats[seat].player};
  assert.equal(landed.vx,travel.at.vx);
  await delay(450);
  assert.equal(next.match.seats[seat].player.x,landed.x); // waiting for them, not drifting
  const cmds=Array.from({length:24},(_,i)=>({seq:i+1,fwd:1,right:0,yaw:90,pitch:12,buttons:0}));
  const led=cmds.reduce((p,c)=>move(p,c,[],0,next.match.walls,false),landed);
  b.ws.send(JSON.stringify({type:'input',commands:cmds.slice(0,12)}));
  b.ws.send(JSON.stringify({type:'input',commands:cmds.slice(12)}));
  await wait(()=>next.match.seats[seat].ack===24);
  assert.ok(Math.abs(next.match.seats[seat].player.x-led.x)<1e-9);
  assert.ok(led.x-landed.x>1);
  const replay=await connect('e1n0',null,a.token,travel.passage);
  await delay(40);
  assert.ok(!replay.messages.some(m=>m.type==='arrival'));
});

test('a visitor walks out of the plaza and down the promenade, two blocks south, and no further', async t => {
  const {app,connect}=await fixture(t);
  let at=await connect('lobby'), here='lobby';
  for(const next of ['e0s1','e0s2']){
    const member=[...app.rooms.get(here).sessions.values()].find(m=>m.owner);
    Object.assign(app.rooms.get(here).match.seats[member.seat].player,{x:0.3,z:-8.3,y:1.7,yaw:180,pitch:0});
    at.ws.send(JSON.stringify({type:'walk',edge:'south'}));
    await wait(()=>at.messages.some(m=>m.type==='travel'));
    const travel=at.messages.find(m=>m.type==='travel');
    assert.equal(travel.room,next);
    const token=at.token;
    at=await connect(next,null,token,travel.passage);
    await wait(()=>at.messages.some(m=>m.type==='arrival'));
    const landed=app.rooms.get(next).match.seats[[...app.rooms.get(next).sessions.values()].find(m=>m.owner).seat].player;
    assert.ok(Math.abs(landed.z-7.7)<0.5,'set down at the same spot, seen from the next block');
    assert.equal(at.messages.find(m=>m.type==='lots'),undefined,'the grounds have no lots');
    here=next;
  }
  const member=[...app.rooms.get(here).sessions.values()].find(m=>m.owner);
  Object.assign(app.rooms.get(here).match.seats[member.seat].player,{x:0,z:-8.3,y:1.7});
  at.ws.send(JSON.stringify({type:'walk',edge:'south'}));
  await delay(60);
  assert.ok(!at.messages.some(m=>m.type==='travel'),'the terrace is the end of the grounds');
});

test('an eight-lot ledger is set down again on the four house lots, in founding order', () => {
  const pods = {}, placements = {};
  [['jeffrey', 10, 0], ['oskie', 20, 1], ['prutti', 30, 5], ['max', 40, 7], ['fia', 50, 2]].forEach(([handle, at, old]) => {
    pods['pod-' + handle] = { id: 'pod-' + handle, handle, at };
    placements['pod-' + handle] = { block: 1, lot: old };
  });
  const g = new Geography({ version: 2, pods, placements, opened: 1, revision: 4 });
  assert.deepEqual(['jeffrey', 'oskie', 'prutti', 'max', 'fia'].map((h) => g.home(h)).map(({ block, lot }) => [block, lot]),
    [[1, 0], [1, 1], [1, 2], [1, 3], [2, 0]]);
  assert.equal(g.opened, 2);
  assert.equal(g.forHandle('jeffrey').id, 'pod-jeffrey', 'identity is kept');
  // Read back, a version 3 ledger is left exactly where it is.
  const again = new Geography(JSON.parse(JSON.stringify(g)));
  assert.deepEqual(again.placements, g.placements);
});

test('a passage seats a signed-in walker with their handle before the account is checked again', async t => {
  const {app,base,connect}=await fixture(t);
  const a=await connect('lobby','jeffrey');
  await wait(()=>[...app.rooms.get('lobby').sessions.values()].some(m=>m.handle==='jeffrey'&&m.seat>=0));
  const member=[...app.rooms.get('lobby').sessions.values()][0];
  Object.assign(app.rooms.get('lobby').match.seats[member.seat].player,{x:8.3,z:0.2,y:1.7,yaw:90,pitch:12,vx:6});
  a.ws.send(JSON.stringify({type:'walk',edge:'east'}));
  await wait(()=>a.messages.some(m=>m.type==='travel'));
  const travel=a.messages.find(m=>m.type==='travel');
  // The motion crosses with the body, and the walker is told which commands the plaza ran.
  // (A tick or two of friction may pass before the plaza answers.)
  assert.ok(travel.at.vx>4&&travel.at.vx<=6);
  assert.equal(travel.ack,0);
  assert.equal(typeof travel.at.x,'number');
  // The page says it is signed in (account=1) and never sends the token:
  // the passage alone must seat it, as the handle that walked.
  const params=new URLSearchParams({room:'e1n0',protocol:PROTOCOL,account:'1',passage:travel.passage});
  const ws=new WebSocket(base.replace('http','ws')+'/play?'+params), messages=[];
  t.after(()=>ws.close());
  ws.on('message',raw=>messages.push(JSON.parse(raw)));
  await once(ws,'open');
  ws.send(JSON.stringify({type:'identity',token:a.token}));
  await wait(()=>messages.some(m=>m.type==='arrival'));
  const landed=[...app.rooms.get('e1n0').sessions.values()].find(m=>m.handle==='jeffrey');
  assert.ok(landed&&landed.seat>=0);
  assert.equal(app.rooms.get('e1n0').match.seats[landed.seat].player.x,travel.at.x);
});

test('the houses come down when the server starts: what stood inside goes up to the handle\'s floor, both ledgers are kept, and nobody is left resting in a house', async t => {
  const dir = await mkdtemp(join(tmpdir(), 'dool-retire-'));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const seed = new GeographyStore(dir);
  await seed.change(g => { g.settle('jeffrey'); g.settle('oskie'); });
  const j = seed.geography.forHandle('jeffrey').id, o = seed.geography.forHandle('oskie').id;
  await seed.change(g => { g.rest('jeffrey', { room: j, x: 1, z: 1 }); g.rest('oskie', { room: 'lobby', x: 2, z: 2 }); });
  const thing = (id, x) => ({ id, owner: 'owner', x, z: 1, versions: [{ version: 1, code: null, geometry: [] }] });
  await writeFile(join(dir, 'objects.json'), JSON.stringify({ owners: {},
    rooms: { [j]: [thing('chair', 1), thing('lamp', 2)], [o]: [], lobby: [thing('fountain', 0)] },
    tiles: { [j]: { '1,1': 2 } }, history: { [j]: [{ code: 'abc' }] } }));
  const before = await readFile(join(dir, 'objects.json'), 'utf8'), ledger = await readFile(join(dir, 'geography.json'), 'utf8');
  const app = createDuelServer({ stateDir: dir });
  t.after(() => app.close?.());
  await app.ready;
  const floor = 'floor-' + app.tower.tower.forHandle('jeffrey').id;
  assert.deepEqual(app.store.data.rooms[floor].map(x => x.id), ['chair', 'lamp']);
  assert.deepEqual(app.store.data.tiles[floor], { '1,1': 2 });
  assert.deepEqual(app.store.data.history[floor], [{ code: 'abc' }]);
  assert.ok(!(j in app.store.data.rooms) && !(o in app.store.data.rooms));
  assert.deepEqual(app.store.data.rooms.lobby.map(x => x.id), ['fountain']);
  assert.deepEqual(app.geography.geography.homes, {});
  assert.equal(app.geography.geography.whereabouts('jeffrey'), null);
  assert.equal(app.geography.geography.whereabouts('oskie').room, 'lobby');
  assert.equal(await readFile(join(dir, 'objects.before-houses.json'), 'utf8'), before);
  assert.equal(await readFile(join(dir, 'geography.before-houses.json'), 'utf8'), ledger);
  // A second start finds nothing to do and keeps the copies as they were.
  const again = createDuelServer({ stateDir: dir });
  t.after(() => again.close?.());
  await again.ready;
  assert.deepEqual(again.store.data.rooms[floor].map(x => x.id), ['chair', 'lamp']);
  assert.equal(await readFile(join(dir, 'objects.before-houses.json'), 'utf8'), before);
});
