import {createHash} from 'node:crypto';
import {wavInput,createTranscriber} from '../../backend/whistlegraph-whisper.mjs';
import {speechBilling,speechRequestKey} from '../../backend/whistlegraph-speech-billing.mjs';
const reply=(statusCode,value)=>({statusCode,headers:{'Content-Type':'application/json','Cache-Control':'private, no-store','Access-Control-Allow-Origin':'*','Access-Control-Allow-Methods':'POST, OPTIONS','Access-Control-Allow-Headers':'Authorization,Content-Type'},body:JSON.stringify(value)});
export function createHandler({authorize,getHandleOrEmail,billing,transcribe,enabled=true,now=Date.now}) {
  // Retry results live only in memory for one minute. Durable receipts contain
  // cost and an audio digest, never recordings, transcripts or bearer tokens.
  const retry=new Map();let active=0;
  return async event=>{
    if(event.httpMethod==='OPTIONS')return reply(204,null);
    if(event.httpMethod!=='POST')return reply(405,{error:'POST only'});
    if(!event.headers?.authorization)return reply(401,{error:'Sign in for OpenAI speech'});
    try {
      const user=await authorize(event.headers);
      if(!user?.sub)return reply(401,{error:'Sign in again for OpenAI speech'});
      if(user.email_verified!==true)return reply(403,{error:'Verify your email for OpenAI speech'});
      if(!enabled)return reply(503,{error:'OpenAI speech is unavailable'});
      const handle=await getHandleOrEmail(user.sub);
      if(typeof handle!=='string'||!handle.startsWith('@'))return reply(403,{error:'Choose an AC handle first'});
      if(typeof event.body!=='string'||event.body.length>2_001_000)return reply(413,{error:'Recording too large'});
      let input;try {input=JSON.parse(event.body);}catch{return reply(400,{error:'Invalid recording'});}
      if(!input||typeof input!=='object'||Array.isArray(input)||typeof input.requestId!=='string'||!/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(input.requestId))return reply(400,{error:'Recording ID must be a UUID'});
      const {audio,durationMs}=wavInput(input),id=speechRequestKey(user.sub,input.requestId);
      const hash=createHash('sha256').update(audio).digest('hex');
      for(const [key,item] of retry)if(item.until<=now())retry.delete(key);
      const cached=retry.get(id);if(cached){if(cached.hash!==hash)return reply(409,{error:'Recording ID already used'});return reply(200,cached.value);}
      if(active>=2)return reply(429,{error:'OpenAI speech is busy; using device speech'});
      active++;
      try {
        const receipt=await billing.begin({user:user.sub,handle:handle.slice(1),requestId:input.requestId,audio,durationMs});
        let result;
        try {result=await transcribe(input);}
        catch(error){await billing.finish(receipt.id,false);throw error;}
        // If accounting cannot complete, recovery refunds the hold. Do not
        // retry the provider or pretend the user received a successful result.
        if(!await billing.finish(receipt.id,true))throw Object.assign(Error('Speech expired; using device speech'),{status:503});
        const value={...result,billing:{braincells:receipt.braincells,free:receipt.free,paid:receipt.paid,durationMs}};
        if(retry.size>=100)retry.delete(retry.keys().next().value);
        retry.set(id,{hash,value,until:now()+60000});return reply(200,value);
      } finally {active--;}
    } catch(error){return reply(error.status||503,{error:error.status?error.message:'OpenAI speech unavailable; using device speech'});}
  };
}
let live;
export async function handler(event) {
  if(!live)live=(async()=>{
    const [{authorize,getHandleOrEmail},{connect}]=await Promise.all([import('../../backend/authorization.mjs'),import('../../backend/database.mjs')]);
    const {db}=await connect(),key=process.env.WHISTLEGRAPH_TRANSCRIPTION_KEY;
    return createHandler({authorize,getHandleOrEmail,billing:speechBilling(db),transcribe:createTranscriber({apiKey:key}),enabled:!!key});
  })().catch(error=>{live=null;throw error;});
  try{return await(await live)(event);}catch{return reply(503,{error:'OpenAI speech unavailable'});}
}
