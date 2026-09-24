import {candlelightRgb} from './candlelight.mjs';
import {writeFile} from 'node:fs/promises';
const base='http://127.0.0.1:8790';
const get=async url=>{const r=await fetch(url,{signal:AbortSignal.timeout(1500)});if(!r.ok)throw Error(`HTTP ${r.status}`);return r.json();};
const post=async(path,data)=>{const r=await fetch(base+path,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(data),signal:AbortSignal.timeout(1500)});if(!r.ok)throw Error(`HTTP ${r.status}`);return r.text();};
let run,commands=0,skipped=0,lastState=0;
let deadline=Date.now()+120000;
try {
 const s=await get(base+'/state');if(Date.now()/1000-s.bridgeSeen>3||s.queueDepth)throw Error('DMX bridge not ready');
 console.log('Candlelight ready; waiting for a native score cue');
 while(Date.now()<deadline){
  const s=await get('http://192.168.1.237/pieces/spatial-rehearsal-status.json');
  if(s.phase==='playing'&&Number.isFinite(s.scoreDuration)&&s.scoreDuration>0&&s.scoreDuration<=1800){
   if(run&&run!==s.runId)break;if(!run)deadline=Date.now()+(s.scoreDuration-s.scoreTime+10)*1000;run=s.runId;
   const t=s.scoreTime;const fade=Math.min(1,t,Math.max(0,s.scoreDuration-t));
   // One bounded update for all room fixtures; never enqueue a backlog.
   const bridge=await get(base+'/state');
   if(bridge.queueDepth===0){
    await post('/command',{address:'all',color:'rgb',rgb:candlelightRgb(t,0).map(v=>Math.round(v*fade)),level:48,duration:.75});commands++;
   }else skipped++;
  }else if(run)break;
  await new Promise(r=>setTimeout(r,125));
 }
} finally {
 await post('/cancel',{});await new Promise(r=>setTimeout(r,600));
 const finalState=await get(base+'/state');const receipt={run,commands,skipped,finalState};
 await writeFile(new URL('./candle-room-receipt.json',import.meta.url),JSON.stringify(receipt,null,2));
 console.log(JSON.stringify({run,commands,skipped,color:finalState.color,lastResult:finalState.lastResult}));
}
