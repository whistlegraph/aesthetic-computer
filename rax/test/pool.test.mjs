import assert from 'node:assert/strict';
import test from 'node:test';
import { cellarFixtures, berths, inPool } from '../basement.mjs';
import { spawn, move } from '../world.mjs';

const step = (p, yaw, fwd = 0, buttons = 0) =>
  move(p,{yaw,pitch:0,fwd,right:0,buttons},[],0,cellarFixtures);
function walk(p,ticks,yaw,fwd=1,buttons=0) {
  for(let i=0;i<ticks;i++) p=step(p,yaw,fwd,buttons);
  return p;
}
test('an avatar climbs the scaffold, jumps through its open window and swims to shore',()=>{
  let p={...spawn(0),x:-1.05,z:3,y:1.7,onGround:true};
  p=walk(p,31,180);
  assert.ok(p.y>2.54 && p.y<2.62);
  p=walk(p,12,-90);
  assert.ok(inPool(p)<0,'sill holds a walking body');
  p=step(p,-90,1,1);
  p=walk(p,47,-90);
  p=walk(p,110,0,0);
  assert.ok(p.swimming && p.y<0.65 && p.y>0.36,JSON.stringify(p));
  p=walk(p,50,0,0,2);
  assert.ok(p.y<0,'crouch dives below the surface');
  p=walk(p,220,0);
  assert.ok(!p.swimming && p.onGround && Math.abs(p.y-1.7)<0.01);
});
test('each dock launches a steerable boat and a fresh jump gets the rider back into the water',()=>{
  for(const berth of berths) {
    let p={...spawn(0), ...berth, y:0.5,onGround:false};
    p=step(p,-90,0,1);
    assert.ok(p.boat && !p.swimming);
    p=walk(p,12,-90,1,1);
    assert.ok(p.boat,'a held jump only boards once');
    p=walk(p,18,-90);
    assert.ok(p.boat && p.x<berth.x-0.4 && p.y>1.2);
    p=step(p,-90,0,1);
    p=walk(p,60,0,0);
    assert.ok(p.swimming && !p.boat && p.y<0.7);
  }
});
