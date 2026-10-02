import test from 'node:test';
import assert from 'node:assert/strict';
import {TurnRecovery} from '../src/turn-recovery.mjs';
import {connectionFailure} from '../src/connection-status.mjs';

function fixture(options={}) {
  const timers=new Map(),notices=[],failures=[];let id=0;
  const recovery=new TurnRecovery({run:async()=>{},changed:s=>notices.push(s),exhausted:e=>failures.push(e),
    setTimer:(fn,ms)=>{timers.set(++id,{fn,ms});return id;},clearTimer:id=>timers.delete(id),...options});
  return {recovery,timers,notices,failures,tick:async()=>{const [id,timer]=timers.entries().next().value;timers.delete(id);await timer.fn();return timer.ms;}};
}
const lost=()=>new TypeError('fetch failed');

test('classifies transport failures without retrying authentication, quota or source errors',()=>{
  for(const error of [lost(),{name:'TimeoutError'},new Error('stream disconnected before completion'),{message:'failed',codexErrorInfo:{responseStreamDisconnected:{}}},{cause:{code:'UND_ERR_SOCKET'}},{status:503}])assert.equal(connectionFailure(error),true,JSON.stringify(error));
  for(const error of [new Error('SyntaxError: bad code'),{status:401,message:'network error'},{message:'insufficient_quota'},{message:'usage limit reached'},{message:'Invalid API key'}])assert.equal(connectionFailure(error),false);
});

test('waits for provider retry, clears on real progress, and bounds a stuck retry',async()=>{
  let stalls=0,runs=0;const f=fixture({run:async()=>runs++,stalled:()=>stalls++});
  f.recovery.providerRetry();f.recovery.providerRetry();assert.equal(f.timers.size,1);
  f.recovery.progress();assert.equal(f.timers.size,0);assert.equal(runs,0);
  f.recovery.providerRetry();await f.tick();assert.equal(stalls,1);assert.equal(runs,0);
});

test('backoff is bounded across repeated failed turns, without concurrent recovery',async()=>{
  let runs=0;const f=fixture({run:async()=>{runs++;throw lost();}});
  f.recovery.schedule(lost());f.recovery.schedule(lost());assert.equal(f.timers.size,1);
  for(const delay of [1000,3000,10000])assert.equal(await f.tick(),delay);
  assert.equal(runs,3);assert.equal(f.timers.size,0);assert.equal(f.failures.length,1);assert.equal(f.recovery.active,false);
});

test('a synchronous terminal failure during recovery is not swallowed',async()=>{
  let runs=0;const f=fixture({run:async()=>{runs++;if(runs===1)f.recovery.schedule(lost());}});
  f.recovery.schedule(lost());await f.tick();assert.equal(f.timers.size,1);
  await f.tick();assert.equal(runs,2);assert.equal(f.recovery.active,false);
});

test('duplicate failure signals before the retry do not schedule a second continuation',async()=>{
  let runs=0;const f=fixture({run:async()=>runs++});
  f.recovery.schedule(lost());f.recovery.schedule(lost());await f.tick();
  assert.equal(runs,1);assert.equal(f.timers.size,0);
});

test('a retry notification during turn startup retains its provider watchdog',async()=>{
  let stalled=0;const f=fixture({run:async()=>f.recovery.providerRetry(),stalled:()=>stalled++});
  f.recovery.schedule(lost());await f.tick();assert.equal(f.recovery.providerWaiting,true);
  assert.match(f.notices.at(-1),/Reconnecting/);await f.tick();assert.equal(stalled,1);
});

test('cancel clears a delayed recovery and invalidates an in-flight reconnect',async()=>{
  let release,isCurrent;const f=fixture({run:async ctx=>{isCurrent=ctx.isCurrent;await new Promise(r=>release=r);}});
  f.recovery.schedule(lost());f.recovery.reset();assert.equal(f.timers.size,0);
  f.recovery.schedule(lost());const pending=f.tick();assert.equal(isCurrent(),true);
  f.recovery.reset();release();await pending;assert.equal(isCurrent(),false);assert.equal(f.timers.size,0);assert.equal(f.recovery.active,false);
});

test('successful completion resets the budget; permanent reconnect errors stop immediately',async()=>{
  const f=fixture({run:async()=>{throw Object.assign(new Error('Unauthorized'),{status:401});}});
  f.recovery.schedule(lost());await f.tick();assert.equal(f.failures.length,1);assert.equal(f.timers.size,0);
  f.recovery.reset();assert.equal(f.recovery.attempt,0);
});
