import assert from 'node:assert/strict';
import {checksFor,moves} from '../Resources/Web/sequence-benchmark.mjs';
assert.equal(moves.length,32);assert.equal(new Set(moves).size,32);
const good={finished:true,changed:true,painted:true,errors:[],source:'export function paint(){}'};
assert.ok(Object.values(checksFor(good)).every(Boolean));
for(const field of ['finished','changed','painted'])assert.ok(Object.values(checksFor({...good,[field]:false})).some(v=>!v));
assert.equal(checksFor({...good,errors:['runtime error']}).noRuntimeErrors,false);
assert.equal(checksFor({...good,source:'x'.repeat(100001)}).sourceBounded,false);
console.log('PASS: 32 distinct edits; unfinished, unchanged, unpainted, errored, and oversized revisions fail.');
const {testFeatures}=await import('./sequence-features.mjs');
const fixture=`let x=30,y=30,vx=2,vy=1.5,hits=0;const trail=[];
export function sim({screen}){x+=vx;y+=vy;if(x<10||x>screen.width-10){vx=-vx;x=Math.max(10,Math.min(screen.width-10,x));hits++;}if(y<10||y>screen.height-10){vy=-vy;y=Math.max(10,Math.min(screen.height-10,y));hits++;}trail.unshift({x,y});if(trail.length>12)trail.pop();}
export function paint({wipe,ink,circle,write}){wipe('black');trail.forEach((p,i)=>{ink(255,105,180,150-i*10);circle(p.x,p.y,10,true);});ink(255,105,180);circle(x,y,10,true);ink(255,255,255);circle(x-3,y-3,2,true);write('Pink: '+hits,2,2);}`;
assert.equal(testFeatures(fixture,4).passed,true);
assert.equal(testFeatures(fixture.replace('150-i*10','0.5'),4).passed,false);
assert.equal(testFeatures(fixture.replace('x+=vx;y+=vy','x+=0;y+=0'),4).passed,false);
assert.equal(testFeatures(fixture.replace("'Pink: '+hits","'Pink: '+0"),4).passed,false);
assert.equal(testFeatures(fixture.replace('circle(x-3,y-3,2,true)','circle(-30,-30,2,true)'),4).passed,false);
console.log('PASS: real generated-code probes detect invisible trails, frozen motion, stuck counters, and missing highlights.');
