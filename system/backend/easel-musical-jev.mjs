import {createHash} from 'node:crypto';
import {musicalDecisionRequest,MUSICAL_CHOICES} from '../../aesel/src/musical-decisions.mjs';
import {evaluateChoices} from '../../aesel/src/jev-decisions.mjs';
export function musicalBudget(collection) {
 async function reserve(id,limit,expiresAt){
  try{await collection.updateOne({_id:id},{$setOnInsert:{count:0,expiresAt}},{upsert:true});}catch(e){if(e.code!==11000)throw e;}
  const r=await collection.findOneAndUpdate({_id:id,count:{$lt:limit}},{$inc:{count:1}},{returnDocument:'after'});
  return Boolean((r?.value??r)?.count);
 }
 return {async consume(subject,now){
  const day=new Date(now).toISOString().slice(0,10), user=createHash('sha256').update(subject).digest('hex');
  const expires=new Date(now+2*86400000);
  return await reserve(`minute:${Math.floor(now/60000)}:${user}`,30,expires)&&await reserve(`user:${day}:${user}`,500,expires)&&await reserve(`global:${day}`,20000,expires);
 }};
}
export function createMusicalHandler({authenticate,budget,evaluate=evaluateChoices,now=Date.now}={}) {
 const busy=new Set();
 const reply=(statusCode,value)=>({statusCode,headers:{'Content-Type':'application/json','Cache-Control':'no-store','Access-Control-Allow-Origin':'*','Access-Control-Allow-Headers':'Content-Type, Authorization','Access-Control-Allow-Methods':'POST, OPTIONS'},body:JSON.stringify(value)});
 return async (event,{subject:trustedSubject,signal}={})=>{
  if(event.httpMethod==='OPTIONS')return reply(200,{});
  if(event.httpMethod!=='POST')return reply(405,{error:'POST only'});
  if(!event.headers?.authorization)return reply(401,{error:'Sign in first'});
  if(typeof event.body!=='string'||event.body.length>2048)return reply(400,{error:'Invalid observation'});
  let body,request;
  try{body=JSON.parse(event.body);if(!['whistlegraph-input/v1','walkieware-input/v1'].includes(body.schema)||! /^[a-f0-9-]{36}$/i.test(body.sessionId)||!Number.isInteger(body.sequence)||body.sequence<1||body.sequence>1000||Object.keys(body).some(k=>!['schema','sessionId','sequence','features'].includes(k)))throw Error();request=musicalDecisionRequest(body.features);}catch{return reply(400,{error:'Invalid observation'});}
  let subject;try{subject=trustedSubject??await authenticate(event.headers);}catch{return reply(503,{error:'Account check unavailable'});}
  if(!subject)return reply(401,{error:'A valid account with a handle is required'});
  if(busy.has(subject)||busy.size>=8)return reply(429,{error:'Decision already running'});
  busy.add(subject);
  try{
   if(!await budget.consume(subject,now()))return reply(429,{error:'Decision allowance reached'});
   const started=performance.now();
   const deadline=AbortSignal.timeout(1000);
   const result=await evaluate(request,{signal:signal?AbortSignal.any([signal,deadline]):deadline});
   const answer=result.answers?.mapping, confidence=answer?.probabilities?.[answer.choice];
   if(!Object.hasOwn(MUSICAL_CHOICES,answer?.choice)||!Number.isFinite(confidence)||confidence<0||confidence>1)throw Error('Invalid answer');
   return reply(200,{schema:body.schema.replace('-input/','-decision/'),sessionId:body.sessionId,sequence:body.sequence,choice:answer.choice,confidence,elapsedMs:Math.round(performance.now()-started)});
  }catch{return reply(503,{error:'Musical advice unavailable; continue without it'});}
  finally{busy.delete(subject);}
 };
}
