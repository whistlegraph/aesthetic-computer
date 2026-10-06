import test from 'node:test';
import assert from 'node:assert/strict';
import {randomUUID} from 'node:crypto';
import {speechCost,speechBilling} from '../backend/whistlegraph-speech-billing.mjs';
import {wavInput,createTranscriber} from '../backend/whistlegraph-whisper.mjs';
import {createHandler} from '../netlify/functions/whistlegraph-transcribe.mjs';
function wav(ms=1000) {
 const b=Buffer.alloc(44+32*ms);b.write('RIFF');b.writeUInt32LE(b.length-8,4);b.write('WAVEfmt ',8);b.writeUInt32LE(16,16);b.writeUInt16LE(1,20);b.writeUInt16LE(1,22);b.writeUInt32LE(16000,24);b.writeUInt32LE(32000,28);b.writeUInt16LE(2,32);b.writeUInt16LE(16,34);b.write('data',36);b.writeUInt32LE(b.length-44,40);return b;
}
const event=(id=randomUUID())=>({httpMethod:'POST',headers:{authorization:'Bearer test'},body:JSON.stringify({requestId:id,audio:wav().toString('base64')})});
test('Whisper uses actual bounded PCM duration and the published cost conversion',()=>{
 assert.equal(speechCost(1000),40);assert.equal(speechCost(8000),320);assert.equal(speechCost(45000),1800);
 assert.equal(wavInput({audio:wav(1100).toString('base64')}).durationMs,1100);
 assert.throws(()=>wavInput({audio:wav(47000).toString('base64')}),{status:400});
 const bad=wav();bad.writeUInt32LE(48000,24);assert.throws(()=>wavInput({audio:bad.toString('base64')}),{status:400});
});
test('authenticated replay returns the same result without another reservation or provider call',async()=>{
 let starts=0,calls=0,finishes=0;
 const handler=createHandler({authorize:async()=>({sub:'u',email_verified:true}),getHandleOrEmail:async()=> '@fifi',billing:{begin:async()=>{starts++;return {id:'r',braincells:40,free:40,paid:0}},finish:async(_,ok)=>{assert.equal(ok,true);finishes++;return true;}},transcribe:async()=>{calls++;return {transcript:'hello',words:[]}}});
 const input=event();assert.equal((await handler(input)).statusCode,200);assert.equal((await handler(input)).statusCode,200);
 assert.deepEqual([starts,calls,finishes],[1,1,1]);
 const changed={...input,body:JSON.stringify({...JSON.parse(input.body),audio:wav(2000).toString('base64')})};assert.equal((await handler(changed)).statusCode,409);
 assert.equal((await handler({...input,headers:{}})).statusCode,401);
});
test('provider failure refunds and unverified users never reserve or upload audio',async()=>{
 let verified=true,refunded=0,calls=0;
 const handler=createHandler({authorize:async()=>({sub:'u',email_verified:verified}),getHandleOrEmail:async()=> '@fifi',billing:{begin:async()=>({id:'r'}),finish:async(_,ok)=>{assert.equal(ok,false);refunded++;return true}},transcribe:async()=>{calls++;throw Object.assign(Error('Provider failed'),{status:502});}});
 assert.equal((await handler(event())).statusCode,502);assert.equal(refunded,1);
 verified=false;assert.equal((await handler(event())).statusCode,403);assert.equal(calls,1);
});
test('Whisper request preserves measured word timing',async()=>{
 const transcribe=createTranscriber({apiKey:'test',fetch:async(url,options)=>{
  assert.equal(options.body.get('model'),'whisper-1');assert.equal(options.body.get('timestamp_granularities[]'),'word');
  return {ok:true,json:async()=>({text:'Hello',words:[{word:'Hello',start:0.1,end:0.5}]})};
 }});
 assert.deepEqual((await transcribe({audio:wav().toString('base64')})).words,[{text:'Hello',atMs:100,durationMs:400}]);
});
// Explicit integration mode, using only fresh, randomly named collections.
// Never read or mutate a real account, wallet or usage document.
test('Mongo transactions fence concurrent spending, replay, failure and crash recovery',{skip:process.env.WHISPER_BILLING_INTEGRATION!=='1'},async()=>{
 const {connect,closePool}=await import('../backend/database.mjs');const {db}=await connect();
 const prefix='test-whisper-'+randomUUID()+'-',names=new Set();
 const isolated={client:db.client,collection(name){names.add(prefix+name);return db.collection(prefix+name);}};
 let clock=new Date('2026-10-05T12:00:00Z');const billing=speechBilling(isolated,{now:()=>clock,limit:60});
 const request=(user='test-user')=>({user,handle:user,requestId:randomUUID(),audio:wav(),durationMs:1000});
 try {
  // Materialize the three isolated collections before transactional DDL.
  for(const name of ['ai-usage','ac-credit-wallets','whistlegraph-speech-requests'])await isolated.collection(name).insertOne({_id:'fixture'});
  const wallets=isolated.collection('ac-credit-wallets'),usage=isolated.collection('ai-usage'),receipts=isolated.collection('whistlegraph-speech-requests');
  await wallets.insertOne({_id:'test-user',balance:100});
  const a=request(),first=await billing.begin(a);assert.equal(first.free,40);assert.equal(first.paid,0);
  await assert.rejects(billing.begin(a),{status:409});await billing.finish(first.id,true);assert.equal(await billing.finish(first.id,false),false);
  const second=await billing.begin(request());assert.equal(second.free,20);assert.equal(second.paid,20);
  assert.equal((await wallets.findOne({_id:'test-user'})).balance,80);await billing.finish(second.id,false);
  assert.equal((await wallets.findOne({_id:'test-user'})).balance,100);assert.equal((await usage.findOne({_id:'test-user:2026-10-05'})).tokens,40);
  const third=await billing.begin(request());await billing.finish(third.id,true);
  assert.equal((await wallets.findOne({_id:'test-user'})).balance,80);
  const stale=await billing.begin(request());clock=new Date(+clock+121000);assert.equal(await billing.reconcile(),1);
  assert.equal((await wallets.findOne({_id:'test-user'})).balance,80);assert.equal(await billing.finish(stale.id,true),false);
  const concurrent=await Promise.allSettled([billing.begin(request('race')),billing.begin(request('race'))]);
  assert.equal(concurrent.filter(v=>v.status==='fulfilled').length,1);assert.equal(concurrent.find(v=>v.status==='rejected').reason.status,402);
  const doc=await receipts.findOne({_id:first.id});assert.equal(doc.charged,40);assert.ok(!('audio' in doc));assert.ok(!('transcript' in doc));
  const overspend=request('no-money');await billing.begin(overspend);await assert.rejects(billing.begin(request('no-money')),{status:402});
 } finally {for(const name of names)await db.collection(name).drop();await closePool();}
});
