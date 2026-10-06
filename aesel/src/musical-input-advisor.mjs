import {summarizeMusicalInput,MUSICAL_CHOICES} from './musical-decisions.mjs';
// Incremental observations are replaceable snapshots, never a queue of stale work.
export class MusicalInputAdvisor {
 constructor({fetchImpl=(...args)=>globalThis.fetch(...args),token,endpoint='https://aesthetic.computer/api/easel-musical-jev',onEvent=()=>{},timeoutMs=900}={}){Object.assign(this,{fetchImpl,token,endpoint,onEvent,timeoutMs});this.reset();}
 reset(){this.controller?.abort();this.sessionId=globalThis.crypto.randomUUID();this.sequence=0;this.calls=0;this.cache=null;this.pending=null;this.lastSent=0;}
 observe(input){
  const features=summarizeMusicalInput(input),key=JSON.stringify(features);
  if(this.cache?.key===key||this.pending||this.calls>=3||Date.now()-this.lastSent<500)return;
  void this.request(features,key);
 }
 async request(features,key){
  const bearer=this.token?.();if(!bearer)return null;
  const sessionId=this.sessionId,sequence=++this.sequence;this.calls++;this.lastSent=Date.now();
  const controller=new AbortController();this.controller=controller;
  const started=performance.now();
  const work=(async()=>{
   let timer;
   try{
    await Promise.resolve();
    const deadline=new Promise((_,reject)=>{timer=setTimeout(()=>{controller.abort();reject(Error('deadline'));},this.timeoutMs);});
    const response=await Promise.race([deadline,this.fetchImpl(this.endpoint,{method:'POST',signal:controller.signal,headers:{'Content-Type':'application/json',Authorization:`Bearer ${bearer}`},body:JSON.stringify({schema:'whistlegraph-input/v1',sessionId,sequence,features})}).then(async r=>{if(!r.ok)throw Error(`http_${r.status}`);return r.json();})]);
    if(this.sessionId!==sessionId||controller.signal.aborted||response.schema!=='whistlegraph-decision/v1'||response.sessionId!==sessionId||response.sequence!==sequence||!Object.hasOwn(MUSICAL_CHOICES,response.choice)||!Number.isFinite(response.confidence)||response.confidence<.8||response.confidence>1)return null;
    const value={key,choice:response.choice,cue:MUSICAL_CHOICES[response.choice],elapsedMs:Math.round(performance.now()-started),transport:response.transport||'http',transportMs:response.transportMs,serverMs:response.serverMs,providerMs:response.elapsedMs};
    this.cache=value;this.onEvent('jevDecision',{choice:value.choice,elapsedMs:value.elapsedMs,sequence,transport:response.transport||'http',transportMs:response.transportMs,serverMs:response.serverMs,providerMs:response.elapsedMs});return value;
   }catch(error){if(this.sessionId===sessionId)this.onEvent('jevFallback',{reason:/^http_\d+$/.test(error.message)?error.message:controller.signal.aborted?'timeout_or_cancel':error.name||'unavailable'});return null;}
   finally{clearTimeout(timer);if(this.sessionId===sessionId)this.pending=null;}
  })();
  this.pending={key,work};return work;
 }
 async finish(input){
  const features=summarizeMusicalInput(input),key=JSON.stringify(features);
  if(this.cache?.key===key){this.onEvent('jevCacheHit',{});return this.cache;}
  if(this.pending?.key===key)return this.pending.work;
  this.controller?.abort();this.pending=null;
  // An older observation must never overwrite this final result.
  this.sessionId=globalThis.crypto.randomUUID();this.cache=null;
  if(this.calls>=4)return null;
  return this.request(features,key);
 }
 cancel(){this.reset();}
}
