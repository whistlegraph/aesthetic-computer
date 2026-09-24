import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {visualPerformance} from './visual-feed.mjs';
const score=JSON.parse(readFileSync(new URL('score.json',import.meta.url)));
test('stop, expired heartbeat, invalid clock and score end release the stage',()=>{
 for(const [transport,now] of [[{playing:false,elapsed:1},1000],[{playing:true,elapsed:1},2251],[{playing:true,elapsed:1},999],[{playing:true,elapsed:score.duration},1000]])assert.equal(visualPerformance(score,transport,1000,now),null);
});
test('score clock seeks directly into sections with bounded actual-note windows',()=>{
 for(const section of score.sections){
  const p=visualPerformance(score,{playing:true,elapsed:section.startSec},1000,1100);
  assert.equal(p.elapsed,section.startSec+.1);assert.equal(p.section,section.name);
  assert.equal(p.dance,'femrag-round-v1');assert(p.hits.length<=48);
  assert(p.hits.every(e=>e[0]>=-60&&e[0]<=100));
 }
});
test('dense arrangement sections stay below a 1 KB stage packet budget',()=>{
 for(let elapsed=0;elapsed<score.duration;elapsed+=.1){
  const performance=visualPerformance(score,{playing:true,elapsed},1000,1000);
  const packet={t:'stage',id:'test-123456789',curtain:false,stopFrame:123456,
    performance:{...performance,curtainStyle:'auto',active:true}};
  assert(Buffer.byteLength(JSON.stringify(packet))<1024);
 }
});
