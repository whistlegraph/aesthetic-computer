import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {gunzipSync} from 'node:zlib';
import {makeSubScore} from './fixtures/sub-score-core.mjs';
const original=JSON.parse(gunzipSync(readFileSync(new URL('./fixtures/baseline-score.nsscore.gz',import.meta.url))));
const fx=JSON.parse(readFileSync(new URL('./notespatial-echo-flange.nsscore',import.meta.url)));
function rig(score,seat=0){let status={},synths=[],lines=0,fills=0,labels=[];let data=Buffer.from(JSON.stringify(score));
 const api={colon:[String(seat+1),'6'],params:[],system:{readFileBytes:()=>data.buffer.slice(data.byteOffset,data.byteOffset+data.byteLength),readFile:()=>'{"seats":[]}',writeFile:(p,v)=>{if(p.includes('status'))status=JSON.parse(v);return true;},battery:{percent:50}},sound:{time:1,microphone:{close(){},hot:false,recording:false},speaker:{amplitudes:{left:.1,right:0}},synth:p=>{synths.push(p);return{update(){},kill(){}};}},screen:{width:683,height:384},wifi:{},wipe(){},ink(){},box:(x,y,w,h,mode)=>{if(mode==='fill')fills++;},line:()=>lines++,write:()=>{},overlayWrite:(s,p)=>labels.push([s,p])};
 return{api,get status(){return status},synths,get lines(){return lines},get fills(){return fills},labels,reset(){lines=0;fills=0;labels.length=0;}};
}
test('echo score preserves all original events, SUB events and global32voice ceiling',()=>{
 for(let i=0;i<original.lanes.length;i++)assert.deepEqual(fx.lanes[i].events.filter(e=>!e.echoTap),original.lanes[i].events);
 assert.deepEqual(makeSubScore(fx).events,makeSubScore(original).events);
 const edges=fx.lanes.flatMap(l=>l.events.flatMap(e=>[[e.t,1],[e.t+e.dur,-1]])).sort((a,b)=>a[0]-b[0]||a[1]-b[1]);let n=0,peak=0;for(const [,d]of edges){n+=d;peak=Math.max(peak,n);}assert.ok(peak<=32,`peak${peak}`);
 console.log({events:fx.lanes.reduce((n,l)=>n+l.events.length,0),echoTaps:fx.effectRevision.echoTaps,peakVoices:peak});
 for(const mine of Object.values(fx.seatFx))assert.ok(mine.fxWobble.every(v=>v>=0&&v<=.35));
});
test('optimized original-score synthesis matches original while raster work stays bounded',async()=>{
 for(let seat=0;seat<6;seat++){
  const old=await import(`./fixtures/baseline-performance.mjs?test=${seat}`),next=await import(`./notespatial-performance-optimized.mjs?test=${seat}`),a=rig(original,seat),b=rig(original,seat);
  old.boot(a.api);next.boot(b.api);old.sim(a.api);next.sim(b.api);
  for(const t of [0,22,60.45,121,220,320,442,485,515,556,650,740]){
   a.api.sound.time=b.api.sound.time=t+4;old.sim(a.api);next.sim(b.api);a.reset();b.reset();old.paint(a.api);next.paint(b.api);
   assert.deepEqual(b.synths,a.synths);assert.ok(b.lines<=48);assert.ok(b.fills<=1);assert.ok(next.getPerformanceVisualState().frames<=24);
  }
 }
});
test('note mode bypasses concert text suppression and reports locally audible note',async()=>{
 const score={...original,dur:10,lanes:[{name:'held',center:true,events:[{t:0,dur:1,note:'C#5',hz:554,g:.2,wave:'sine'}]}]};
 const next=await import('./notespatial-performance-optimized.mjs?notes'),r=rig(score,5);next.boot(r.api);next.sim(r.api);r.api.sound.time=4;next.sim(r.api);next.setVisualMode('notes');next.paint(r.api);
 assert.equal(next.getPerformanceVisualState().note,'C#5');assert.equal(r.labels[0][0],'C#5');assert.ok(r.labels[0][1].size>=20);assert.equal(r.lines,0);
 r.api.sound.time=5.5;next.sim(r.api);next.paint(r.api);assert.equal(next.getPerformanceVisualState().note,'');
});
