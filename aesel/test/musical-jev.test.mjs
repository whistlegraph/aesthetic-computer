import test from 'node:test';import assert from 'node:assert/strict';
import {summarizeMusicalInput} from '../src/musical-decisions.mjs';
import {MusicalInputAdvisor} from '../src/musical-input-advisor.mjs';
import {createMusicalHandler,musicalBudget} from '../../system/backend/easel-musical-jev.mjs';
const input={transcript:'PRIVATE WORDS',words:[{atMs:0,durationMs:200}],sound:{frames:[{atMs:400,rms:.2,pitchHz:440},{atMs:600,rms:.2,pitchHz:660},{atMs:800,rms:.2,pitchHz:880}],onsetsMs:[400]}};
const features=summarizeMusicalInput(input);
const event={httpMethod:'POST',headers:{authorization:'test'},body:JSON.stringify({schema:'walkieware-input/v1',sessionId:crypto.randomUUID(),sequence:1,features})};
const answer={answers:{mapping:{choice:'follow_speech',probabilities:{follow_speech:.94}}}};
test('server minimizes evidence and echoes observation identity',async()=>{
 const handler=createMusicalHandler({authenticate:async()=> 'subject',budget:{consume:async()=>true},evaluate:async request=>{assert.doesNotMatch(JSON.stringify(request),/PRIVATE|subject/);return answer;}});
 const r=await handler(event);assert.equal(r.statusCode,200);assert.equal(JSON.parse(r.body).sessionId,JSON.parse(event.body).sessionId);
 assert.equal((await handler({...event,headers:{}})).statusCode,401);
 assert.equal((await handler({...event,body:JSON.stringify({...JSON.parse(event.body),prompt:'anything'})})).statusCode,400);
 assert.equal((await handler({...event,body:JSON.stringify({...JSON.parse(event.body),features:{...features,transcript:'bad'}})})).statusCode,400);
});
test('budget failure, provider failure and invalid account never fabricate advice',async()=>{
 let calls=0;const base={authenticate:async()=> 'subject',budget:{consume:async()=>false},evaluate:async()=>{calls++;return answer;}};
 assert.equal((await createMusicalHandler(base)(event)).statusCode,429);assert.equal(calls,0);
 assert.equal((await createMusicalHandler({...base,authenticate:async()=>null})(event)).statusCode,401);
 assert.equal((await createMusicalHandler({...base,budget:{consume:async()=>true},evaluate:async()=>{throw Error('secret');}})(event)).statusCode,503);
});
test('streaming advice is cached for matching musical state without sending words',async()=>{
 let calls=0;const advisor=new MusicalInputAdvisor({token:()=> 'test',fetchImpl:async(_,options)=>{calls++;assert.doesNotMatch(options.body,/PRIVATE|transcript|pitchHz/);const b=JSON.parse(options.body);return Response.json({...b,schema:'walkieware-decision/v1',choice:'follow_speech',confidence:.94});}});
 advisor.observe(input);await advisor.pending.work;const r=await advisor.finish(input);assert.equal(r.choice,'follow_speech');assert.equal(calls,1);
});
test('late responses, low confidence and timeouts cannot steer',async()=>{
 let resolve;const a=new MusicalInputAdvisor({token:()=> 'test',timeoutMs:15,fetchImpl:()=>new Promise(r=>resolve=r)});
 const p=a.finish(input);await Promise.resolve();a.cancel();resolve(Response.json({choice:'sustain',confidence:1}));assert.equal(await p,null);assert.equal(a.cache,null);
 const stalled=new MusicalInputAdvisor({token:()=> 'test',timeoutMs:15,fetchImpl:()=>new Promise(()=>{})});assert.equal(await stalled.finish(input),null);
 const uncertain=new MusicalInputAdvisor({token:()=> 'test',fetchImpl:async(_,o)=>Response.json({...JSON.parse(o.body),schema:'walkieware-decision/v1',choice:'sustain',confidence:.2})});assert.equal(await uncertain.finish(input),null);
});
test('feature summaries separate speech interval from later sound',()=>{assert.equal(features.hasSpeech,true);assert.equal(features.soundAfterSpeech,true);assert.equal(features.contour,'rising');assert.equal(features.attacks,1);});
test('atomic quota reservation refuses a full counter',async()=>{
 let increments=0;const budget=musicalBudget({updateOne:async()=>{},findOneAndUpdate:async()=>{increments++;return null;}});
 assert.equal(await budget.consume('subject',Date.now()),false);assert.equal(increments,1);
});
test('default fetch keeps its global receiver for Safari',async t=>{
 t.mock.method(globalThis,'fetch',async function(_,o){assert.equal(this,globalThis);const b=JSON.parse(o.body);return Response.json({...b,schema:'walkieware-decision/v1',choice:'follow_speech',confidence:.95});});
 const advisor=new MusicalInputAdvisor({token:()=> 'test'});assert.equal((await advisor.finish(input)).choice,'follow_speech');
});
