import {readFile,writeFile} from 'node:fs/promises';
import {calmRgb} from './calm-light.mjs';
const timeline=JSON.parse(await readFile(new URL('./notespatial-look.nstimeline',import.meta.url)));
const base='http://127.0.0.1:8790',source='http://192.168.1.234:8791/api/state';
const sleep=ms=>new Promise(r=>setTimeout(r,ms));
async function req(url,data){const r=await fetch(url,{signal:AbortSignal.timeout(1200),...(data?{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(data)}:{})});if(!r.ok)throw Error('HTTP '+r.status);const text=await r.text();try{return JSON.parse(text)}catch{return text;}}
let run=null,index=0,commands=0,skipped=0,deadline=Date.now()+180000,stopping=false;
process.on('SIGTERM',()=>{stopping=true});process.on('SIGINT',()=>{stopping=true});
try{
 while(!stopping&&Date.now()<deadline){
  const s=await req(source);
  if(s.live&&s.phase==='playing'){
   if(s.scoreHash!==timeline.sourceHash)throw Error('Score/timeline mismatch');
   if(run&&run!==s.runId)break;
   if(!run){run=s.runId;deadline=Date.now()+(timeline.duration-s.scoreTime+5)*1000;console.log('Following',run);}
   const state=await req(base+'/state');
   if(Date.now()/1000-state.bridgeSeen>3)throw Error('Room bridge stale');
   if(state.queueDepth===0){const seat=index++%4,f={address:[1,11,31,21][seat],rgb:calmRgb(timeline,s.scoreTime,seat)};
    await req(base+'/command',{...f,color:'rgb',level:28,duration:3,envelope:{attack:.8,decay:1}});commands++;
   }else skipped++;
  }else if(run)break;
  await sleep(140);
 }
}finally{
 await req(base+'/cancel',{});await sleep(700);
 const finalState=await req(base+'/state');
 await writeFile(new URL('./room-receipt.json',import.meta.url),JSON.stringify({run,commands,skipped,finalState},null,2));
 console.log(JSON.stringify({run,commands,skipped,blackout:finalState.lastResult}));
}
