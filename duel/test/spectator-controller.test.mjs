import test from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {Room} from '../room.mjs';
import {Controller,stick} from '../controller.mjs';
import {moveFreecam} from '../freecam.mjs';
import {parseSource,sourceFrame} from '../object-language.mjs';

test('one handle claims one player; late identity cannot steal the earlier connection',()=>{
 const messages=[];const room=new Room((ws,data)=>messages.push({ws,...data}));
 const a={},b={},c={};const first=room.add(a,true),second=room.add(b,true);
 first.welcomed=second.welcomed=true;
 second.handle='Jeffrey';second.ready=true;room.assign();
 assert.equal(second.seat,0);
 first.handle='jeffrey';first.ready=true;room.assign();
 assert.equal(first.seat,0);assert.equal(second.seat,-1);
 assert.equal(room.match.seats.filter(Boolean).length,1);assert.equal(room.match.phase,'waiting');
 const other=room.add(c);other.handle='another';room.assign();assert.equal(other.seat,1);
 assert.equal(messages.filter(m=>m.type==='presence').at(-1).members.length,2);
 room.remove(a);assert.equal(second.seat,0);assert.equal(other.seat,1);
});
test('spectator movement has no collision, normalization prevents diagonal boost',()=>{
 const camera={x:0,y:2,z:0,yaw:0,pitch:0};
 const p=moveFreecam(camera,{fwd:1,right:1,up:0},.1);
 assert.ok(Math.abs(Math.hypot(p.x,p.z)-.6)<1e-9);
 const high=moveFreecam({...camera,y:39.9},{fwd:0,right:0,up:1,boost:true},1);
 assert.equal(high.y,40);assert.deepEqual(camera,{x:0,y:2,z:0,yaw:0,pitch:0});
});
test('controller deadzone, edge actions, disconnect and pane focus',()=>{
 assert.deepEqual(stick(.1,.05),[0,0]);assert.deepEqual(stick(1,0),[1,0]);
 const c=new Controller();const p={id:'8BitDo Pro test',index:0,connected:true,mapping:'standard',axes:[0,0,0,0],buttons:Array.from({length:17},()=>({pressed:false,value:0}))};
 c.sample([p]);p.axes=[1,0,.6,-.5];p.buttons[6].pressed=true;
 const active=c.sample([p]);assert.equal(active.right,1);assert.equal(active.jump,true);assert.ok(active.lookX>0&&active.lookY<0);
 assert.equal(c.sample([p]).jump,false);assert.equal(c.sample([p],false).right,0);
 assert.equal(c.sample([p],true).jump,false,'held button cannot fire when pane gains focus');
 assert.equal(c.sample([]).right,0);assert.equal(c.id,null);
});
test('the rejected Big friend walks within its actual swept bounds',async()=>{
 const source=await readFile(new URL('./fixtures/big-friend.lisp',import.meta.url),'utf8');const g=parseSource(source);
 assert.equal(g.animated,true);assert.equal(g.boxes.length,12);
 for(let t=0;t<15;t+=.053){const v=sourceFrame(g,t).vertices;for(let i=0;i<v.length;i+=6){
  assert.ok(v[i+1]>=-.001,'foot stays above ground');
  assert.ok(g.boxes.some(b=>Math.abs(v[i]-b.x)<=b.w/2+1e-6&&Math.abs(v[i+1]-b.y)<=b.h/2+1e-6&&Math.abs(v[i+2]-b.z)<=b.d/2+1e-6));
 }}
});
