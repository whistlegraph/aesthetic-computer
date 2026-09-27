import test from 'node:test';
import assert from 'node:assert/strict';
import {createPlaybackClock} from '../../system/public/aesthetic.computer/lib/playback-clock.mjs';

test('clock tempo stays continuous and controls every consumer of the shared clock',()=>{
  let now=1700000000000;const clock=createPlaybackClock(()=>now);
  assert.equal(clock.time(),now);
  now+=200;const before=clock.time();clock.setRate(2);assert.equal(clock.time(),before);
  now+=500;assert.equal(clock.time(),before+1000);
  const step=()=>Math.floor(clock.time()/500),phase=()=>clock.time()%500;
  const beat=step();now+=250;assert.equal(step(),beat+1);assert.equal(phase(),before%500);
  const second=clock.time();clock.setRate(0.5);now+=400;assert.equal(clock.time(),second+200);
  clock.setRate(1,true);assert.equal(clock.time(),now);
  now+=12;assert.equal(clock.time(),now); // Includes later network-offset corrections.
});

test('invalid rates leave the clock intact and repeated settings never restart it',()=>{
  let now=1000;const clock=createPlaybackClock(()=>now);clock.setRate(1.5);now+=100;
  for(const bad of [NaN,Infinity,-1,0,0.2,2.1,'2',null])assert.equal(clock.setRate(bad),false);
  assert.equal(clock.rate,1.5);assert.equal(clock.time(),1150);
  clock.setRate(1.5);now+=100;assert.equal(clock.time(),1300);
});
