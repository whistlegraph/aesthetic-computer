import test from 'node:test';import assert from 'node:assert/strict';import {readFileSync} from 'node:fs';
import library from '../desktop/donkey-actions.js';
const {validateActions,sampleAction,chooseAction,createActionPlayer,cropPose,loadActions}=library;
const idle=JSON.parse(readFileSync(new URL('../desktop/donkey-actions/idle.json',import.meta.url),'utf8'));
test('48 personality actions have distinct choreography, bounded poses, and no easel in any crop',()=>{
 validateActions(idle,{expectedCount:48});
 const choreography=new Set(idle.map(a=>JSON.stringify(a.frames.map(({duration,...frame})=>frame))));assert.equal(choreography.size,48,'Actions must differ by more than labels and hold times');
 for(const action of idle)for(const frame of action.frames){const crop=cropPose(frame.pose);assert.equal(crop.width,46);assert.equal(crop.height,64);assert.equal(crop.x%64,0);assert.ok(crop.x%64+crop.width<=46);}
});
test('sample advances exact frame boundaries, loops, ends and reduces motion without timers',()=>{
 const action=idle.find(a=>a.id==='idle-sneeze'),total=action.frames.reduce((n,f)=>n+f.duration,0);
 assert.equal(sampleAction(action,449).frameIndex,0);assert.equal(sampleAction(action,450).frameIndex,1);assert.equal(sampleAction(action,total).done,true);assert.equal(sampleAction(action,total).nextDelay,null);
 const loop=idle.find(a=>a.loop);assert.equal(sampleAction(loop,4000).frameIndex,0);assert.equal(sampleAction(loop,4000).done,false);
 const still=sampleAction(action,800,{reducedMotion:true});assert.equal(still.frameIndex,0);assert.equal(still.x,0);assert.equal(still.rotate,0);assert.equal(still.effect,undefined);assert.equal(still.nextDelay,null);
});
test('deterministic selection honors status and avoids immediate repeats; player advances only on request',()=>{
 const selected=chooseAction(idle,{state:'sleeping',seed:'same'});assert.equal(chooseAction(idle,{state:'sleeping',seed:'same'}).id,selected.id);assert.ok(selected.states.includes('sleeping'));assert.notEqual(chooseAction(idle,{state:'sleeping',seed:'same',previous:selected.id}).id,selected.id);
 const player=createActionPlayer(idle,{state:'idle',seed:12,now:100});const first=player.action;player.sample(100000);assert.equal(player.action,first);player.next(100000);assert.notEqual(player.action,first);player.setState('sleeping',100000);assert.ok(idle.find(a=>a.id===player.action).states.includes('sleeping'));player.setReducedMotion(true);assert.equal(player.sample(100000).nextDelay,null);
});
test('loader validates merged groups and rejects duplicate identities or malformed transforms',async()=>{
 const fake=async()=>({ok:true,json:async()=>idle});assert.equal((await loadActions({urls:['idle'],fetch:fake,expectedCount:48})).length,48);await assert.rejects(loadActions({urls:['idle','duplicate'],fetch:fake,expectedCount:96}),/duplicate/);
 const corrupt=structuredClone(idle);corrupt[0].frames[0].duration=0;assert.throws(()=>validateActions(corrupt),/timing/);corrupt[0].frames[0].duration=100;corrupt[0].frames[0].scaleX=Infinity;assert.throws(()=>validateActions(corrupt),/scaleX/);
});
