import test from 'node:test';
import assert from 'node:assert/strict';
import { objectPiece } from '../public/aesthetic.computer/lib/object/viewer.mjs';
const source='; Chime\n(ink 255 90 150) (slab -.2 0 -.1 .2 3 .1) (tone 220 sine .1)';
const api=()=>{const a={screen:{width:100,height:100,pixels:new Uint8ClampedArray(40000)},ink(){return a},write(t){assert.ok(!t.includes('tap for sound'));return a}};return a};
const event=(name,x,y)=>({x,y,is:n=>n===name});
test('an object sounds whenever it runs, with no speaker control, and preparing one is silent',()=>{
 let starts=0,kills=0,updates=0;
 const sound={synth(){starts++;return{kill(){kills++},update(){updates++}}}};
 const first=objectPiece(source), next=objectPiece(source);const a=api();
 first.paint(a);assert.equal(starts,0,'painting alone is silent');
 assert.ok(a.screen.pixels.subarray(100*4*11+78*4,100*4*11+100*4).every((v,i,row)=>v===row[i%4]),'no speaker drawn top right');
 first.sim({sound});assert.equal(starts,1);assert.ok(updates>0);
 first.act({event:event('touch',85,15),sound});first.act({event:event('lift',85,15),sound});
 first.sim({sound});assert.equal(starts,1,'a touch top right toggles nothing');assert.equal(kills,0);
 first.leave();assert.equal(kills,1);
 next.paint(a);assert.equal(starts,1,'preparing a model is silent');
 next.boot({});next.sim({sound});assert.equal(starts,2,'the next model sounds when it runs');
});
