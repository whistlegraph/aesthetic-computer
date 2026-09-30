// Hosted inference billed in braincells: OpenRouter's reported cost at 2×,
// the rate fallback, the daily wallet cap and settlement that cannot repeat.
import test from 'node:test';
import assert from 'node:assert/strict';
import {authorizePaidRequest,braincellRate,braincellsFromCost,BRAINCELLS_PER_USD,CREDIT_PACK,DAILY_PAID_BRAINCELL_CAP,INFERENCE_MARKUP,OUT_OF_BRAINCELLS,reserve,settle,settleDurably,reconcileWallet,HOLD_LIFETIME_MS,usageBraincells} from '../backend/easel-paid-credits.mjs';
import {DEFAULT_EASEL_MODEL,EASEL_MODELS,HOSTED_MAX_TOKENS,inferenceRequest} from '../backend/easel-policy.mjs';
import {relayInference} from '../backend/easel-stream.mjs';

// Just enough of a Mongo collection for the wallet's conditional updates.
const get=(doc,path)=>path.split('.').reduce((v,k)=>v?.[k],doc);
function put(doc,path,value){const keys=path.split('.');const last=keys.pop();let at=doc;for(const k of keys)at=at[k]??={};if(value===undefined)delete at[last];else at[last]=value;}
function expr(value,doc,vars={}) {
 if(typeof value==='string'&&value.startsWith('$$'))return get(vars,value.slice(2));
 if(typeof value==='string'&&value.startsWith('$'))return get(doc,value.slice(1));
 if(Array.isArray(value))return value.map(v=>expr(v,doc,vars));
 if(!value||typeof value!=='object'||!Object.keys(value).some(k=>k.startsWith('$')))return value;
 const [[op,args]]=Object.entries(value);
 if(op==='$map')return expr(args.input,doc,vars).map(v=>expr(args.in,doc,{...vars,[args.as]:v}));
 const a=expr(args,doc,vars);
 if(op==='$ifNull')return a[0]??a[1];
 if(op==='$objectToArray')return Object.entries(a).map(([k,v])=>({k,v}));
 if(op==='$sum'||op==='$add')return a.reduce((n,v)=>n+v,0);
 if(op==='$lte')return a[0]<=a[1];
 throw Error('Unsupported mock operator '+op);
}
function matches(doc,filter){
 return Object.entries(filter).every(([path,want])=>{
  if(path==='$expr')return expr(want,doc);
  const have=get(doc,path);
  if(want&&typeof want==='object'){
   if('$gte' in want)return have>=want.$gte;
   if('$not' in want)return !(have>=want.$not.$gte);
   if('$exists' in want)return (have!==undefined)===want.$exists;
  }
  return have===want;
 });
}
function wallets(docs){
 return {
  docs,
  async findOne(filter){return docs.find(d=>matches(d,filter))||null;},
  async updateOne(filter,update){
   const doc=docs.find(d=>matches(d,filter));if(!doc)return {modifiedCount:0};
   for(const [path,n] of Object.entries(update.$inc||{}))put(doc,path,(get(doc,path)||0)+n);
   for(const [path,v] of Object.entries(update.$set||{}))put(doc,path,v);
   for(const path of Object.keys(update.$unset||{}))put(doc,path,undefined);
   return {modifiedCount:1};
  },
 };
}
const messages=[{role:'user',content:'hello'}];

test('the six open models are allowed, flash is the default, and workspace turns may ask for 32,000',()=>{
 for(const model of ['deepseek/deepseek-v4.1-flash','deepseek/deepseek-v4-pro','moonshotai/kimi-k3','qwen/qwen3.7-plus','minimax/minimax-m3','z-ai/glm-5.3-flash']){
  assert.ok(Object.hasOwn(EASEL_MODELS,model),model);
  assert.ok(braincellRate(model)>0,model);
 }
 assert.equal(DEFAULT_EASEL_MODEL,'deepseek/deepseek-v4.1-flash');
 assert.equal(inferenceRequest({messages}).model,DEFAULT_EASEL_MODEL);
 assert.equal(inferenceRequest({messages,max_tokens:32000}).maxTokens,32000);
 assert.equal(inferenceRequest({messages,max_tokens:64000}).maxTokens,HOSTED_MAX_TOKENS);
 assert.equal(inferenceRequest({messages,model:'openai/gpt-5.6-luna'}).model,'openai/gpt-5.6-luna','installed clients keep working');
});

test('a reported cost is billed at twice the price, at pack value',()=>{
 assert.equal(INFERENCE_MARKUP,2);
 assert.equal(BRAINCELLS_PER_USD,CREDIT_PACK.credits/(CREDIT_PACK.amount/100));
 assert.equal(BRAINCELLS_PER_USD,200_000);
 assert.equal(braincellsFromCost(0.001),400,'no float noise tips a whole braincell');
 assert.equal(braincellsFromCost(0.0123456),4939);
 assert.equal(braincellsFromCost(0),0);
 assert.equal(usageBraincells({model:'moonshotai/kimi-k3',tokens:1_000_000,cost:0.01}),4000,'cost wins over tokens');
});

test('without a reported cost, the model rate over weighted tokens applies',()=>{
 assert.equal(usageBraincells({model:'openai/gpt-5.6-luna',tokens:1000}),1000);
 assert.equal(usageBraincells({model:'anthropic/claude-opus-5',tokens:4}),100);
 assert.equal(usageBraincells({model:'deepseek/deepseek-v4.1-flash',tokens:1000}),480);
 assert.equal(usageBraincells({model:'openai/gpt-5.6-luna',tokens:1000,cost:'0.1'}),1000,'a non-number cost is not trusted');
 // Reservation rates are worst case: every token at the output price, marked up.
 assert.equal(braincellRate('moonshotai/kimi-k3'),15*INFERENCE_MARKUP*BRAINCELLS_PER_USD/1e6);
});

test('the relay hands the provider cost to the meter once',async()=>{
 const events=[{type:'message_start',message:{usage:{input_tokens:100,output_tokens:1}}},{type:'message_delta',usage:{output_tokens:50,cost:0.00042}}];
 const body=new Response(events.map(e=>`data: ${JSON.stringify(e)}\n\n`).join('')).body;
 const calls=[];await new Response(relayInference(body,{onUsage:(tokens,usage)=>calls.push({tokens,usage})})).text();
 await new Promise(r=>setImmediate(r));
 assert.equal(calls.length,1);
 assert.equal(calls[0].tokens,150);
 assert.equal(calls[0].usage.cost,0.00042);
 assert.equal(usageBraincells({model:DEFAULT_EASEL_MODEL,tokens:calls[0].tokens,cost:calls[0].usage.cost}),168);
});

test('settlement is keyed by the hold, so a repeated settle cannot charge twice',async()=>{
 const w=wallets([{_id:'u',balance:10_000}]);
 const now=new Date('2026-09-28T12:00:00Z');
 const hold=await reserve('u',1000,w,{now});
 assert.equal(w.docs[0].balance,9000);
 assert.equal(await settle(hold,250,w,{now}),true);
 assert.equal(await settle(hold,250,w,{now}),false);
 await Promise.all([settle(hold,900,w,{now}),settle(hold,900,w,{now})]);
 assert.equal(w.docs[0].balance,9750);
 assert.equal(w.docs[0].daily['2026-09-28'],250);
 assert.equal(w.docs[0].holds[hold.id],undefined);
 const capped=await reserve('u',100,w,{now});await settle(capped,5000,w,{now});
 assert.equal(w.docs[0].balance,9650,'never more than the hold');
});

test('a day of bought braincells is capped, and the refusal says when it resets',async()=>{
 const now=new Date('2026-09-28T21:30:00Z');
 const w=wallets([{_id:'rich',balance:10_000_000,daily:{'2026-09-28':DAILY_PAID_BRAINCELL_CAP}},{_id:'poor',balance:10}]);
 const using=fn=>fn(w);
 const body={messages};
 await assert.rejects(authorizePaidRequest({user:'rich',model:DEFAULT_EASEL_MODEL,body,maxTokens:1000,now,withWallets:using}),
  e=>e.statusCode===429&&/2,000,000/.test(e.message)&&/midnight UTC, in 2h 30m/.test(e.message));
 await assert.rejects(authorizePaidRequest({user:'poor',model:DEFAULT_EASEL_MODEL,body,maxTokens:1000,now,withWallets:using}),
  e=>e.statusCode===402&&e.message===OUT_OF_BRAINCELLS&&/\/provider/.test(e.message));
 // Yesterday's spend does not count against today.
 const tomorrow=new Date('2026-09-29T00:00:01Z');
 const hold=await authorizePaidRequest({user:'rich',model:DEFAULT_EASEL_MODEL,body,maxTokens:1000,now:tomorrow,withWallets:using});
 assert.ok(hold.amount>0&&hold.day==='2026-09-29');
});

test('the hold grows with max_tokens and with the model rate',async()=>{
 const w=wallets([{_id:'u',balance:100_000_000}]);const using=fn=>fn(w);const body={messages};
 const small=await authorizePaidRequest({user:'u',model:DEFAULT_EASEL_MODEL,body,maxTokens:1000,withWallets:using});
 const large=await authorizePaidRequest({user:'u',model:DEFAULT_EASEL_MODEL,body,maxTokens:32000,withWallets:using});
 const kimi=await authorizePaidRequest({user:'u',model:'moonshotai/kimi-k3',body,maxTokens:32000,withWallets:using});
 assert.ok(small.amount<large.amount&&large.amount<kimi.amount);
 assert.ok(large.amount<(32000+4096+100)*1,'flash holds under one braincell a token');
});


test('pending holds and requested amount cannot overshoot the daily cap',async()=>{
 const now=new Date('2026-09-30T12:00:00Z');
 const w=wallets([{_id:'u',balance:10000000,daily:{'2026-09-30':DAILY_PAID_BRAINCELL_CAP-100}}]);
 const holds=await Promise.all([reserve('u',100,w,{now}),reserve('u',100,w,{now})]);
 assert.equal(holds.filter(Boolean).length,1);
 assert.equal(await reserve('u',1,w,{now}),null);
 await settle(holds.find(Boolean),75,w,{now});
 assert.ok(await reserve('u',25,w,{now}));
 assert.equal(await reserve('u',1,w,{now}),null);
});

test('recovery settles a persisted charge once after a database failure',async()=>{
 const now=new Date('2026-09-30T12:00:00Z'),w=wallets([{_id:'u',balance:1000}]);
 const hold=await reserve('u',100,w,{now});
 const update=w.updateOne;
 w.updateOne=async(filter,change)=>{if(change.$unset)throw Error('database offline');return update(filter,change);};
 await assert.rejects(settleDurably(hold,30,w,{now}),/database offline/);
 assert.equal(w.docs[0].holds[hold.id].settlement,30);
 w.updateOne=update;
 const snapshot=structuredClone(w.docs[0]);
 assert.equal(await reconcileWallet(snapshot,w,{now}),1);
 assert.equal(await reconcileWallet(snapshot,w,{now}),0);
 assert.equal(w.docs[0].balance,970);
});

test('expired unknown holds refund once and fence late charges; active work stays held',async()=>{
 const now=new Date('2026-09-30T23:59:00Z'),w=wallets([{_id:'u',balance:1000}]);
 const hold=await reserve('u',100,w,{now});
 assert.equal(await reconcileWallet(w.docs[0],w,{now}),0);
 assert.equal(await reconcileWallet(w.docs[0],w,{now:new Date(+now+HOLD_LIFETIME_MS)}),1);
 assert.equal(await settleDurably(hold,80,w,{now}),false);
 assert.equal(w.docs[0].balance,1000);
 assert.equal(w.docs[0].daily['2026-09-30'],0);
});

test('stream completion awaits settlement and reports its failure exactly once',async()=>{
 let attempts=0,errors=0,finished=0;
 const text=await new Response(relayInference(new Response('data: {"usage":{"output_tokens":1}}\n\n').body,{
   onUsage:async()=>{attempts++;await new Promise(r=>setImmediate(r));throw Error('database down');},
   onSettlementError:()=>errors++,onFinish:()=>finished++,
 })).text();
 assert.ok(text.includes('output_tokens'));
 assert.deepEqual([attempts,errors,finished],[1,1,1]);
});


test('stream cancellation aborts upstream and waits for one settlement',async()=>{
 let aborted=0,settled=0,cancelled=0;
 const source=new ReadableStream({start(c){c.enqueue(new TextEncoder().encode('data: {"usage":{"output_tokens":7}}\n\n'));},cancel(){cancelled++;}});
 const reader=relayInference(source,{abort:()=>aborted++,onUsage:async tokens=>{
   assert.equal(tokens,7);await new Promise(r=>setImmediate(r));settled++;
 }}).getReader();
 await reader.read();await reader.cancel('user stopped');
 assert.deepEqual([aborted,settled,cancelled],[1,1,1]);
});
