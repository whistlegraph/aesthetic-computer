import test from 'node:test';import assert from 'node:assert/strict';
import {mkdtemp,mkdir,writeFile,rm} from 'node:fs/promises';import {tmpdir} from 'node:os';import {join} from 'node:path';
import NetClock from '../desktop/net-clock.js';
import {localPreview} from '../desktop/local-preview.cjs';
const {sample,estimate,createClock,beat,loop,nextBeat,beatMs,BLEND}=NetClock;
// A machine: its own Date.now, and a fake /api/clock that answers in true time
// after `rtt` ms. `skew` is how far its local clock sits from true time.
const machine=({skew,rtt,now=1_700_000_000_000})=>{let local=now+skew;const m={skew,now:()=>local,tick:ms=>{local+=ms;},true:()=>local-m.skew};
 m.fetch=async()=>{m.tick(rtt/2);const iso=new Date(m.true()).toISOString();m.tick(rtt/2);return {ok:true,text:async()=>iso};};
 m.clock=createClock({sample:()=>sample({fetch:m.fetch,now:m.now,site:'https://ac.test'}),now:m.now});return m;};
test('offset is server time minus the request midpoint, taken whole first and eased a quarter after',async()=>{
 assert.equal(estimate({t0:1000,t1:1200,serverTime:5100}),4000);
 const m=machine({skew:-4000,rtt:200});
 assert.equal(await m.clock.resync(),4000);assert.equal(m.clock.rtt,200);assert.equal(m.clock.synced,1);
 assert.equal(m.clock.time(),m.true());
 m.skew=-3000; // the local clock jumps a second ahead: the next estimate says 3000
 assert.equal(await m.clock.resync(),4000+(3000-4000)*BLEND);assert.equal(BLEND,.25);
});
test('a second sample eases in: one slow round trip cannot yank the beat',async()=>{
 const m=machine({skew:-4000,rtt:200});await m.clock.resync();
 // a burst of lag answers late: the estimate lands 400ms off
 const slow=async()=>{m.tick(100);const iso=new Date(m.true()-400).toISOString();m.tick(100);return {ok:true,text:async()=>iso};};
 const c=createClock({sample:()=>sample({fetch:slow,now:m.now}),now:m.now});c.adopt(4000);
 assert.equal(await c.resync(),4000-400*BLEND);
 // a concurrent resync shares the in-flight sample
 const a=c.resync(),b=c.resync();assert.equal(a,b);await a;
});
test('sample rejects bad answers without touching the offset',async()=>{
 const m=machine({skew:0,rtt:10});
 const bad=createClock({sample:()=>sample({fetch:async()=>({ok:false,status:503}),now:m.now}),now:m.now});
 await assert.rejects(bad.resync(),/503/);assert.equal(bad.offset,0);assert.equal(bad.synced,0);
 const junk=createClock({sample:()=>sample({fetch:async()=>({ok:true,text:async()=>'soon'}),now:m.now}),now:m.now});
 await assert.rejects(junk.resync(),/unreadable/);
});
test('beat index, phase and next beat come from the epoch at the bpm, as clock.mjs counts them',()=>{
 assert.equal(beatMs(120),500);
 assert.deepEqual(beat(1500,120),{index:3,period:500,start:1500,phase:0,next:2000});
 assert.deepEqual(beat(1625,120),{index:3,period:500,start:1500,phase:.25,next:2000});
 assert.equal(nextBeat(1625,120),2000);assert.equal(nextBeat(1500,120),2000);
 const t=1_700_000_000_123;const b=beat(t,100);assert.equal(b.index,Math.floor(t/600));assert.ok(b.phase>=0&&b.phase<1);assert.equal(b.next,b.start+600);
});
test('a loop of N beats wraps on beats whose index is a multiple of N',()=>{
 const grid={bpm:120,beats:8}; // 4000ms loop
 assert.deepEqual(loop(9000,grid),{index:2,period:4000,start:8000,phase:.25,next:12000,beat:2});
 assert.equal(loop(8000,grid).phase,0);assert.equal(loop(11999,grid).beat,7);assert.equal(loop(12000,grid).beat,0);
 assert.equal(loop(9000,grid).start%beatMs(120),0);
});
test('two machines with different local clocks land on the same beat and loop phase once synced',async()=>{
 const a=machine({skew:-2345,rtt:120}),b=machine({skew:+987,rtt:40});
 await a.clock.resync();await b.clock.resync();b.tick(a.true()-b.true()); // meet at the same true instant
 const grid={bpm:100,beats:16};
 for(const step of [0,1,733,4000,17_500]){a.tick(step);b.tick(step);
  assert.equal(beat(a.clock.time(),100).index,beat(b.clock.time(),100).index);
  assert.ok(Math.abs(loop(a.clock.time(),grid).phase-loop(b.clock.time(),grid).phase)<1e-9);}
 // unsynced they disagree by the skew
 assert.notEqual(beat(a.now(),100).index,beat(b.now(),100).index);
});
test('the sound preview carries its score grid so the deck can lock the loop',async t=>{
 const workspace=await mkdtemp(join(tmpdir(),'easel-clock-'));t.after(()=>rm(workspace,{recursive:true,force:true}));
 const root=join(workspace,'.easel-media','art');await mkdir(root,{recursive:true});
 await writeFile(join(root,'sound.wav'),Buffer.alloc(44));await writeFile(join(root,'score.json'),JSON.stringify({bpm:90,beats:8,loop:true,notes:[]}));
 const preview=await localPreview(workspace,{mime:'audio/wav',path:join(root,'sound.wav')});
 assert.deepEqual(preview.grid,{bpm:90,beats:8,loop:true});
 await writeFile(join(root,'score.json'),'{');
 assert.equal((await localPreview(workspace,{mime:'audio/wav',path:join(root,'sound.wav')})).grid,undefined);
});
