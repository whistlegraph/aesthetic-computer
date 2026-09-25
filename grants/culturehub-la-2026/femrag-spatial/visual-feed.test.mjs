import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {visualPerformance, trioPerformance} from './visual-feed.mjs';
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
test('trio transports carry the sung lyric and release when stopped, stale or not trio',()=>{
 const syllables=[{t:5.2,dur:.8,text:'good',note:62},{t:6,dur:.8,text:'mor',note:64},{t:6.8,dur:.8,text:'ning',note:65},{t:7.6,dur:.8,text:'So',note:67},{t:8.4,dur:.8,text:'phi',note:69},{t:9.2,dur:1.2,text:'a',note:67}];
 const lyric={text:'good morning Sophia',member:'neo',rgb:[143,209,63],t:5.2,dur:5.2,role:'lead',answer:false,syllables,syllable:5};
 const next={text:'Sophia',member:'frisbee',rgb:[242,167,185],in:8.3};
 const faces={neo:{text:'good morning Sophia',rgb:[143,209,63],t:5.2,dur:5.2,role:'lead'},blueberry:null,frisbee:{text:'hmm hmm',rgb:[242,167,185],t:10,dur:4,role:'hum'}};
 const transport={playing:true,elapsed:12.4,title:'The MacNeoPolitan Trio — Good morning, Sophia',dance:'trio-round-v1',bpm:69,duration:52.17,lyric,next,faces,sentAt:1790293779.47};
 const p=trioPerformance(transport,1000,1100);
 assert.equal(p.dance,'trio-round-v1');assert(Math.abs(p.elapsed-12.5)<1e-9);assert.equal(p.duration,52.17);
 assert.equal(p.section,'neo');assert.equal(p.source,'MacNeoPolitan Trio · neo');assert.equal(p.playing,true);
 assert.equal(p.sentAt,1790293779.47);assert.deepEqual(p.faces,faces);
 assert.deepEqual(p.lyric,{text:'good morning Sophia',member:'neo',rgb:[143,209,63],t:5.2,dur:5.2,role:'lead',answer:false,syllable:5,
  syl:[[5.2,'good'],[6,'mor'],[6.8,'ning'],[7.6,'So'],[8.4,'phi'],[9.2,'a']]});
 assert.deepEqual(p.next,next);assert.deepEqual(p.hits,[]);assert.deepEqual(p.notes,[]);
 const quiet=trioPerformance({...transport,lyric:null,faces:undefined},1000,1100);
 assert.equal(quiet.lyric,null);assert.equal(quiet.section,null);assert.equal(quiet.source,'MacNeoPolitan Trio · ');
 assert.deepEqual(quiet.faces,{neo:null,blueberry:null,frisbee:null});
 // A long line with every face singing stays inside the stage packet budget.
 const long={...transport,lyric:{...lyric,text:'and now we know them all together',syllables:Array.from({length:15},(_,i)=>({t:5+i*.4,dur:.4,text:'syl'+i,note:60+i}))},
  faces:{neo:faces.neo,blueberry:{text:'we sang them in order',rgb:[90,87,211],t:5,dur:5,role:'hum'},frisbee:faces.frisbee}};
 const big=trioPerformance(long,1000,1100);assert.equal(big.lyric.syl.length,15);
 const bigPacket={t:'stage',id:'test-123456789',curtain:false,stopFrame:123456,performance:{...big,relayedAt:1790293779470,curtainStyle:'auto',active:true}};
 assert(Buffer.byteLength(JSON.stringify(bigPacket))<1500,'trio packet '+Buffer.byteLength(JSON.stringify(bigPacket)));
 for(const [t,now] of [[{...transport,playing:false},1100],[transport,2251],[transport,999],[{...transport,dance:undefined},1100],[{...transport,elapsed:54},1100]])assert.equal(trioPerformance(t,1000,now),null);
 // Femrag transports never become Trio output, and a stopped Trio never becomes Femrag output.
 assert.equal(trioPerformance({playing:true,elapsed:1},1000,1100),null);
 assert.equal(visualPerformance(score,{...transport,playing:false},1000,1100),null);
 const packet={t:'stage',id:'test-123456789',curtain:false,stopFrame:123456,performance:{...p,curtainStyle:'auto',active:true}};
 assert(Buffer.byteLength(JSON.stringify(packet))<1024);
});
