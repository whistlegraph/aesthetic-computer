import test from 'node:test';
import assert from 'node:assert/strict';
import {paidCheckout,CREDIT_PACK,reservationSize,fulfillCheckout,reserve,settle,refundCheckout,braincellRate} from '../backend/easel-paid-credits.mjs';
import {createHandler} from '../netlify/functions/easel-checkout.mjs';
const paid={id:'cs_test_123',mode:'payment',payment_status:'paid',status:'complete',livemode:false,amount_total:500,currency:'usd',client_reference_id:'user1',metadata:{type:'ac-credits',pack:CREDIT_PACK.id,userSub:'user1'}};
test('only a matching, confirmed server offer can grant credit',()=>{
 assert.equal(paidCheckout(paid,{live:false}).credits,1_000_000);
 assert.equal(paidCheckout({...paid,payment_status:'unpaid'}),null);
 for(const change of [{amount_total:1},{currency:'eur'},{client_reference_id:'other'},{metadata:{...paid.metadata,pack:'invented'}},{livemode:true}])assert.throws(()=>paidCheckout({...paid,...change},{live:false}));
});
test('reservation covers byte-heavy input and rejects unmetered media',()=>{
 assert.ok(reservationSize({messages:[{content:'é'.repeat(100)}]},100)>reservationSize({messages:[{content:'a'.repeat(100)}]},100));
 assert.throws(()=>reservationSize({messages:[{content:[{type:'image',source:{}}]}]},100));
});
const event=body=>({httpMethod:'POST',headers:{authorization:'Bearer test'},body:JSON.stringify(body)});
function handler(options={}){return createHandler({stripe:{checkout:{sessions:{create:async()=>({id:'cs_test_new',url:'https://checkout.stripe.com/test'}),retrieve:async()=>paid}},webhooks:{constructEvent(){throw Error('invalid')}}},verifyUser:async()=>({sub:'user1'}),handleFor:async()=>'@tester',secret:'test',fulfill:async()=>true,...options});}
test('checkout requires sign in, known pack, and request ID',async()=>{
 assert.equal((await handler({verifyUser:async()=>null})(event({}))).statusCode,401);
 assert.equal((await handler()(event({pack:'made-up'}))).statusCode,400);
 assert.equal((await handler()(event({pack:CREDIT_PACK.id,requestId:'12345678-1234-1234-1234-123456789abc'}))).statusCode,200);
 assert.equal((await handler({enabled:false})(event({}))).statusCode,503);
});
test('invalid webhook signatures never fulfill; unpaid sessions do not grant',async()=>{
 let calls=0;const h=handler({fulfill:async()=>calls++});
 assert.equal((await h({...event({}),headers:{'stripe-signature':'bad'}})).statusCode,400);assert.equal(calls,0);
 assert.equal((await h({httpMethod:'GET'})).statusCode,200);assert.equal(calls,0);
});
test('another user cannot reconcile a checkout',async()=>{
 let calls=0;const h=handler({verifyUser:async()=>({sub:'other'}),fulfill:async()=>calls++});
 assert.equal((await h(event({sessionId:'cs_test_123'}))).statusCode,404);assert.equal(calls,0);
});
test('signed webhook fulfillment failure asks Stripe to retry',async()=>{
 const h=handler({stripe:{webhooks:{constructEvent:()=>({type:'checkout.session.completed',data:{object:paid}})}},fulfill:async()=>{throw Error('database unavailable');}});
 assert.equal((await h({...event({}),headers:{'stripe-signature':'valid'}})).statusCode,500);
});
// This same suite is run against a separate test collection in production Mongo
// to exercise the real conditional-update semantics and concurrent deliveries.
if(process.env.AC_CREDITS_TEST_MONGO==='true'){
 test('Mongo: concurrent grants, holds, settlement, and retries are atomic',async()=>{
  const {connect}=await import('../backend/database.mjs');const c=await connect();const w=c.db.collection('ac-credit-wallets-test');const user='test-'+crypto.randomUUID();const s={...paid,client_reference_id:user,metadata:{...paid.metadata,userSub:user}};
  try{
   await Promise.all(Array.from({length:10},()=>fulfillCheckout(s,w,{live:false})));
   assert.equal((await w.findOne({_id:user})).balance,1_000_000);
   const holds=await Promise.all(Array.from({length:10},()=>reserve(user,600_000,w)));
   assert.equal(holds.filter(Boolean).length,1);
   const hold=holds.find(Boolean);await Promise.all([settle(hold,100,w),settle(hold,100,w)]);
   assert.equal((await w.findOne({_id:user})).balance,999900);
   const weighted=await reserve(user,1000,w);await settle({...weighted,rate:25},4,w);
   assert.equal((await w.findOne({_id:user})).balance,999800);
   const canceled=await reserve(user,1000,w);await settle(canceled,0,w);assert.equal((await w.findOne({_id:user})).balance,999800);
   await Promise.all([refundCheckout(s,250,w,{live:false}),refundCheckout(s,250,w,{live:false})]);
   assert.equal((await w.findOne({_id:user})).balance,499800);
   await refundCheckout(s,500,w,{live:false});await refundCheckout(s,250,w,{live:false});
   assert.equal((await w.findOne({_id:user})).balance,-200);
  }finally{await w.deleteOne({_id:user});await c.disconnect();}
 });
}

test('braincells are one currency with model and long-context consumption rates',()=>{
 assert.equal(braincellRate('openai/gpt-5.6-luna'),1);
 assert.equal(braincellRate('anthropic/claude-opus-5'),25);
 assert.equal(braincellRate('openai/gpt-5.6-luna',300000),2);
 assert.throws(()=>braincellRate('invented'));
 assert.equal(paidCheckout({...paid,metadata:{...paid.metadata,pack:'luna-1m-v1'}}).credits,1_000_000);
});
