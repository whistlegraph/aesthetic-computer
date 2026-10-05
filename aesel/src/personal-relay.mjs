// Browser-safe private relay transport. Provider credentials never enter the app.
const URL='https://help.aesthetic.computer/api/aesel/sessions';
export function personalUsage(u={}) {
 return {input_tokens:u.inputTokens??u.input_tokens,output_tokens:u.outputTokens??u.output_tokens,
  cache_read_input_tokens:u.cacheReadInputTokens??u.cache_read_input_tokens,
  cache_creation_input_tokens:u.cacheCreationInputTokens??u.cache_creation_input_tokens,
  cost:null,cost_estimate_usd:u.costBasis==='list'?u.costUSD:null};
}
export async function runPersonalTurn({token,model,instructions='',content,tools=[],onTool,onEvent=()=>{},onHeaders=()=>{},signal,storage,key='personal-relay',fetch=globalThis.fetch,pollMs=350}) {
 const parts=Array.isArray(content)?content:[{type:'text',text:String(content)}];
 const text=parts.filter(p=>p.type==='text').map(p=>p.text).join('\n');
 const images=parts.filter(p=>p.type==='image').map(p=>({mimeType:p.source.media_type,data:p.source.data}));
 const signature=Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256',new TextEncoder().encode(JSON.stringify({model,instructions,text,images})))),b=>b.toString(16).padStart(2,'0')).join('');
 let state;try{state=JSON.parse(storage?.getItem(key)||'null');}catch{}
 if(!state || (state.finished && state.signature!==signature))state={signature,requestId:crypto.randomUUID(),sessionId:null,results:{}};
 const save=()=>storage?.setItem(key,JSON.stringify(state));save();
 const call=async(path,data,{detached=false}={})=>{
  const bearer=typeof token==='function'?await token():token;if(!bearer)throw Error('Sign in to use your personal relay');
  const response=await fetch(URL+path,{method:data===undefined?'GET':'POST',headers:{authorization:'Bearer '+bearer,'content-type':'application/json'},
   ...(data===undefined?{}:{body:JSON.stringify(data)}),signal:detached?AbortSignal.timeout(5000):signal});
  const result=await response.json();if(!response.ok)throw Object.assign(Error(result.error||`Personal relay returned ${response.status}`),{status:response.status});
  if(path.endsWith('/turn'))onHeaders(response);return result;
 };
 let cursor=0,answer='',finished=false,disconnectedAt=0;
 const abort=()=>{if(state.sessionId&&!finished)void call('/'+state.sessionId+'/interrupt',{}, {detached:true}).catch(()=>{});};
 signal?.addEventListener('abort',abort,{once:true});
 try {
  signal?.throwIfAborted();
  if(!state.sessionId){const created=await call('',{provider:'claude',model:model.replace(/^anthropic\//,''),instructions,clientTools:tools});state.sessionId=created.thread.id;save();}
  const input={requestId:state.requestId,text,images};
  try{await call('/'+state.sessionId+'/turn',input);}catch(error){if(error.status || signal?.aborted)throw error;await call('/'+state.sessionId+'/turn',input);}
  for(;;){
   signal?.throwIfAborted();
   let data;
   try{data=await call('/'+state.sessionId+'?after='+cursor);disconnectedAt=0;}
   catch(error){if(error.status || signal?.aborted)throw error;disconnectedAt||=Date.now();if(Date.now()-disconnectedAt>30000)throw error;await new Promise(r=>setTimeout(r,1000));continue;}
   for(const entry of data.events){
    cursor=entry.seq;
    if(entry.type==='fatal')throw Error(entry.value.message);
    if(entry.type!=='notification')continue;
    const {method,params}=entry.value;
    if(method==='item/agentMessage/delta')answer+=params.delta||'';
    if(method==='item/completed'&&params.item?.type==='agentMessage'&&!answer)answer=params.item.text||'';
    if(method==='turn/usage')onEvent({method,params:{...params,usage:personalUsage(params.usage)}});
    else onEvent(entry.value);
    if(method==='turn/completed'){
      finished=true;state.finished=true;save();
      if(params.turn.status!=='completed')throw Error(params.turn.error?.message||'Personal relay stopped');
      return {text:answer,sessionId:state.sessionId,requestId:state.requestId};
    }
   }
   for(const pending of data.pending){
    if(pending.method!=='phone/tool')throw Error('Unexpected relay approval');
    let result=state.results[pending.id];
    if(!result){
     if(!onTool)throw Error('Review requested an unavailable tool');
     const output=await onTool({id:pending.id,...pending.params});
     result={isError:!!output.is_error,content:[{type:'text',text:typeof output.content==='string'?output.content:JSON.stringify(output.content)}]};
     state.results[pending.id]=result;save(); // Persist before acknowledgement: edits execute once.
    }
    try{await call('/'+state.sessionId+'/respond',{id:pending.id,result});}catch(error){if(error.status!==409)throw error;}
   }
   await new Promise(resolve=>setTimeout(resolve,pollMs));
  }
 }finally{signal?.removeEventListener('abort',abort);}
}
