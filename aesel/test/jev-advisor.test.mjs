import test from 'node:test';
import assert from 'node:assert/strict';
import { JevAdvisor, configuredJev } from '../src/jev-advisor.mjs';
const input = { feedback: { logs:[{level:'error',text:'TypeError: PRIVATE_SECRET is not a function'}] }, blocks:[], results:[] };
const response = { model:'jev', usage:{input_tokens:12}, answers:{next:{choice:'inspect_api',probabilities:{inspect_api:.95}}} };
test('triage minimizes evidence, bounds calls and preserves blank artwork', async()=>{
  let calls=0;
  const jev=new JevAdvisor({evaluate:async request=>{calls++;assert.doesNotMatch(JSON.stringify(request),/PRIVATE_SECRET/);return response;}});
  assert.equal(await jev.advise({...input,feedback:{logs:[],frame:{blank:true}}}),null);
  assert.equal((await jev.advise(input)).choice,'inspect_api');
  await jev.advise(input);await jev.advise(input);assert.equal(calls,2);
  jev.beginTurn();assert.equal((await jev.advise({...input,feedback:{logs:[{level:'error',text:'SyntaxError'}]}})).local,true);
  assert.equal(calls,2);
});
test('timeout falls back, interrupt propagates, low confidence does not steer',async()=>{
  const stalled=new JevAdvisor({timeoutMs:15,evaluate:()=>new Promise(()=>{})});
  assert.equal(await stalled.advise(input),null);
  const c=new AbortController();c.abort();
  await assert.rejects(stalled.advise({...input,signal:c.signal}),{name:'AbortError'});
  const uncertain=new JevAdvisor({evaluate:async()=>({answers:{next:{choice:'repair',probabilities:{repair:.3}}}})});
  assert.equal((await uncertain.advise(input)).cue,'');
});
test('configuration requires explicit opt-in and a key',()=>{
  assert.equal(configuredJev({env:{EASEL_JEV:'0',OPENROUTER_API_KEY:'x'},home:'/nonexistent'}),null);
  assert.equal(configuredJev({env:{EASEL_JEV:'1'},home:'/nonexistent'}),null);
  assert.ok(configuredJev({env:{EASEL_JEV:'1',OPENROUTER_API_KEY:'x'},home:'/nonexistent'}));
});
