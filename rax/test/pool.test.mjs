import assert from 'node:assert/strict';
import test from 'node:test';
import { cellarFixtures, berths, deck, POOL, SEA_HALF } from '../basement.mjs';
import { spawn, move } from '../world.mjs';
import { inCar, CAR_Z } from '../elevator.mjs';

const step = (p, yaw, fwd = 0, buttons = 0) =>
  move(p,{yaw,pitch:0,fwd,right:0,buttons},[],0,cellarFixtures,false,null,SEA_HALF);
function walk(p,ticks,yaw,fwd=1,buttons=0) {
  for(let i=0;i<ticks;i++) p=step(p,yaw,fwd,buttons);
  return p;
}
test('an avatar climbs the scaffold, jumps through its open window and swims to shore',()=>{
  let p={...spawn(0),x:-1.05,z:3,y:1.7,onGround:true};
  p=walk(p,31,180);
  assert.ok(p.y>2.54 && p.y<2.62);
  p=walk(p,12,-90);
  assert.ok(p.x > deck.xMin,'sill holds a walking body');
  p=step(p,-90,1,1);
  p=walk(p,47,-90);
  p=walk(p,110,0,0);
  assert.ok(p.swimming && Math.abs(p.y-POOL.level-0.48)<0.1,JSON.stringify(p));
  p=walk(p,50,0,0,2);
  assert.ok(p.y<POOL.level,'crouch dives below the surface');
  p=walk(p,280,-90);
  assert.ok(p.swimming && p.x < -9,'the sea continues past the old cellar wall');
  for (let i=0;i<600 && Math.hypot(-17.1-p.x,4.4-p.z)>0.1;i++)
    p=step(p,Math.atan2(-17.1-p.x,4.4-p.z)*180/Math.PI,1);
  assert.ok(Math.hypot(-17.1-p.x,4.4-p.z)<0.1,'swam back to the foot of the sea stairs');
  p=walk(p,300,90);
  assert.ok(!p.swimming && p.onGround && Math.abs(p.y-1.7)<0.01,'the sea stairs return to B1 '+JSON.stringify(p));
});
test('water below the elevator stays outside the car', () => {
  assert.equal(inCar({x:0, y:POOL.level+0.48, z:CAR_Z}), false);
});

test('each dock launches a steerable boat and a fresh jump gets the rider back into the water',()=>{
  for(const berth of berths) {
    let p={...spawn(0), ...berth, y:POOL.level+0.5,onGround:false};
    p=step(p,-90,0,1);
    assert.ok(p.boat && !p.swimming);
    p=walk(p,12,-90,1,1);
    assert.ok(p.boat,'a held jump only boards once');
    p=walk(p,18,-90);
    assert.ok(p.boat && p.x<berth.x-0.4 && p.y>POOL.level+1.2);
    p=step(p,-90,0,1);
    p=walk(p,60,0,0);
    assert.ok(p.swimming && !p.boat && p.y<POOL.level+0.7);
  }
});
